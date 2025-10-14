################################################################################
#Owner           => E.F.P.                                                     #
#Programa PENC103=> GENERACION DE LA OPERACION 78 DE RETIROS POR PENSION       #
#                   MINIMA GARANTIZADA                                         #
#Sistema         => PEN                                                        #
#Version         => CPL (se queda ya como version propia por biometricos)      #
#Fecha creacion  => 6 DE ABRIL DE 2010                                         #
#By              => JAVIER GONZALEZ JERONIMO                                   #
#Fecha actualiz. => 14 DE FEBRERO DE 2011                                      #
#Actualizacion   => JAVIER GONZALEZ JERONIMO                                   #
#                => Se modifica para permitir la generacion de la operacion    #
#                   78 para los registros a los que se les paga a partir de    #
#                   la segunda mensualidad                                     #
#Actualizacion   => ISAI JIMENEZ ROJAS  21-NOV-2013   v1.1                     #
#                =  Ajuste a sobregiros y remanentes                           #
#                =  REF. MLM-1936, CPL-1414                                    #
#Actualizacion   => ISAI JIMENEZ ROJAS  08-ENE-2013                            #
#Actualizacion   => ISAI JIMENEZ ROJAS  31-AGO-2015   CPL-2067                 #
#                => Correccion a marca de ultimo pago para no dejar remanente  #
#Actualizacion   => v1.3 EMMANUEL REYES CPL-2114                               #
#                => Cambio del arreglo de acciones por siefore 90              #
#Actualizacion   => CPL-2619 DMR 14/11/2017 Se modifico de acuerdo al layout   #
#                => de la Op78 y 79  de PMG campos de sello y biometricos      #
#Actualizacion   => CPL-2798 IJR 13/16/2018 ajustes a tratamiento de infor.    #
#                => biometrica al generar operacion 78                         #
#Actualizacion   => CPL-2912 IJR 12-oct-18 ajustes para corregir la marca de   #
#                => ultimo pago que aplicaba de forma erronea                  #
#Actualizacion   => CPL-3000 ajustes por cambio en fondos generacionales       #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gar_precio_acc ARRAY [90] OF RECORD #Arreglo para los precios_accion --CPL-2114
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        capturado             LIKE pen_estado_pmg.estado_solicitud    ,
        confirmado            LIKE pen_estado_pmg.estado_solicitud    ,
        enviado               LIKE pen_estado_pmg.estado_solicitud    ,
        en_proceso_pago       LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado             LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gr_rango_fechas    RECORD
        inicio                   DATE,
        fin                      DATE
                              END RECORD

    DEFINE #glo #date
        gdt_fec_pago_pmg      ,
        gdt_fec_oper          ,
        HOY                   DATE

    DEFINE
        gc_nom_archivo        CHAR(012) ,
        G_LISTA_DET           CHAR(100) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        ENTER                 CHAR(001) ,
        gc_modulo             CHAR(003) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_tipo_op            ,
        gs_procesa            ,
        gs_cod_xxi            ,
        gs_codigo_afore       SMALLINT

    DEFINE
        gi_num_regs           ,
        gs_ult_folio          INTEGER
        
    DEFINE g_mensaje          CHAR(80)
    DEFINE VERSION            CHAR(07)
    DEFINE TECLA              CHAR(1)

    DEFINE gs_sin_sello        SMALLINT       --CPL-2798
    
END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I,
        MESSAGE LINE LAST -1

    CALL init()
    
    LET VERSION = "v1.2798"    --cpl-2798
    LET VERSION = "v1.2912"    --cpl-2912
    LET VERSION = "v2.3000"    --CPL-3000
    
    CALL STARTLOG(gc_usuario CLIPPED||".PENC103.log")
    
    CALL ERRORLOG("\nSE EJECUTA PROGRAMA VERSION "||VERSION)

    CALL f_captura_fechas() RETURNING gs_procesa, gr_rango_fechas.*

    IF gs_procesa THEN
      
       CALL f_obten_fechas() RETURNING gdt_fec_pago_pmg, gdt_fec_oper

       LET  gc_nom_archivo = gdt_fec_oper USING "YYYYMMDD",".78P"

       #-Genera encabezado transaccional
       CALL primer_paso()

        #--Genera el detalle del reporte
       CALL segundo_paso(gr_rango_fechas.*) RETURNING gi_num_regs
       
       IF gi_num_regs > 0 THEN 

          CALL tercer_paso(gi_num_regs)        #--Genera el sumario del reporte

          CALL cuarto_paso()                   #--Concatena los archivos

          CALL quinto_paso(gi_num_regs)        #--Actualiza tablas
       ELSE
          PROMPT "NO SE GENERO INFORMACION PRESIONE <ENTER>: " FOR CHAR ENTER
       END IF
        
       IF gs_sin_sello = TRUE THEN                                                             --CPL-2798
          --SE ENCONTRARON NSSs SIN SELLO (DEBEN TENERLO)                                      --CPL-2798
          PROMPT "SE ENCONTRARON NSS SIN SELLO, DESEA VER LA LISTA (S/N): " FOR CHAR TECLA    --CPL-2798
          IF TECLA MATCHES "[sS]" THEN                                                         --CPL-2798
             CALL f_muestra_lista_sin_sellos()                                                 --CPL-2798
          END IF                                                                               --CPL-2798
       END IF                                                                                  --CPL-2798
    ELSE
        CALL f_error_msg("PROCESO CANCELADO")
        CLOSE WINDOW penc1031
    END IF
    
END MAIN


#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()
    DEFINE
        lc_prepare      CHAR(300)

    LET HOY             = TODAY
    LET gs_tipo_op      = 78

    ----- CODIGOS AFORES -----    
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pmg"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.en_proceso_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"


    ----- HABIL SIGUIENTE -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_siguiente(?,?) "
    PREPARE eje_habil_sig FROM lc_prepare

    LET lc_prepare = " "

    ----- CALCULA PROPORCIONAL PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_calcula_prop_pmg(?,?,?,?) "
    PREPARE eje_prop_pmg FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el rango de fechas en el que se buscara para   #
#                    generar la operacion 78                                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_fechas          RECORD
                                 inicio        DATE,
                                 fin           DATE
                              END RECORD

    DEFINE li_tot_registros   INTEGER
    DEFINE li_tot_por_pagar   INTEGER
    DEFINE li_tot_prov        INTEGER

    DEFINE ls_estado          SMALLINT
    DEFINE ls_flag            SMALLINT

    DEFINE lc_mensaje         CHAR(100)
        
    DEFINE ld_fecha_ini       DATE       --CPL-2798
    DEFINE ld_fecha_fin       DATE       --CPL-2798

    ----------------------------------------------------------------------

    CALL f_abre_ventana()
    
    
    IF f_valida_historicos_pmg() = FALSE THEN    --CPL-2812
       --la notificacion esta dentro de la funcion
       EXIT PROGRAM
    END IF 
    
    LET ls_flag = 1

    CALL f_obtiene_precios_accion(HOY)
    
    LET li_tot_registros    = 0 
    LET li_tot_por_pagar    = 0 
    LET li_tot_prov         = 0
    
    LET ld_fecha_ini = MDY(MONTH(HOY),1,YEAR(HOY)) 
    LET ld_fecha_fin = ld_fecha_ini + 1 units MONTH 
    LET ld_fecha_fin = MDY(MONTH(ld_fecha_fin),1,YEAR(ld_fecha_fin))-1
    
    LET lr_fechas.inicio    = ld_fecha_ini      
    LET lr_fechas.fin       = ld_fecha_fin

    SELECT NVL(COUNT(*),0)
    INTO   li_tot_prov
    FROM   pen_solicitud_pmg
    WHERE  estado_solicitud = gr_edo.confirmado    

    DISPLAY BY NAME lr_fechas.inicio
    DISPLAY BY NAME lr_fechas.fin
    DISPLAY "RANGO DE FECHAS PARA MENSUALIDADES MAYORES A 1*: " AT 7,9
    DISPLAY "SOLICITUDES CON MENSUALIDAD 1 POR PAGAR    : ", li_tot_prov AT 15,9

    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD inicio
            CALL f_valida_fechas(lr_fechas.inicio)
            RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

        AFTER FIELD fin
            CALL f_valida_fechas(lr_fechas.fin)
            RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
               ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
               SLEEP 2
               ERROR ""
               NEXT FIELD fin
            ELSE
               IF lr_fechas.inicio > lr_fechas.fin THEN
                  ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                  SLEEP 2
                  ERROR ""
                  NEXT FIELD inicio
               END IF
            END IF
            
            --VALIDA QUE EL PERIODO SEA DEL MISMO MES Y ANIO --CPL-2798
            IF ( MONTH(lr_fechas.fin) > MONTH(HOY) ) THEN
               ERROR "NO ES POSIBLE PROCESAR PAGOS POSTERIORES AL MES ACTUAL "
               NEXT FIELD inicio
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            LET ls_flag = 0
            EXIT INPUT

        #--
        ON KEY (ESC)
            CALL f_valida_fechas(lr_fechas.inicio)
                 RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

            CALL f_valida_fechas(lr_fechas.fin)
                 RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
               ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
               SLEEP 2
               ERROR ""
               NEXT FIELD fin
            ELSE
               IF lr_fechas.inicio > lr_fechas.fin THEN
                  ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                  SLEEP 2
                  ERROR ""
                  NEXT FIELD inicio
               END IF
            END IF
            
            --VALIDA QUE EL PERIODO SEA DEL MISMO MES Y ANIO   --CPL-2798
            IF ( MONTH(lr_fechas.fin) > MONTH(HOY) ) THEN
               ERROR "NO ES POSIBLE PROCESAR PAGOS POSTERIORES AL MES ACTUAL "
               NEXT FIELD inicio
            END IF

            LET ls_flag = 1

            EXIT INPUT

    END INPUT

    IF ls_flag = 1 THEN

        SELECT NVL(COUNT(*),0)
        INTO   li_tot_por_pagar
        FROM   pen_ctr_pago_det
        WHERE  fecha_pago_estimada BETWEEN lr_fechas.inicio AND lr_fechas.fin 
        AND    num_mensualidad > 1
        AND    estado          = gr_edo.capturado
        
        LET li_tot_registros = li_tot_por_pagar + li_tot_prov

        IF li_tot_registros = 0 THEN
            CALL f_error_msg("NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO")
            EXIT PROGRAM
        END IF
        
        WHILE TRUE
            PROMPT " DESEA GENERAR EL ARCHIVO DE LA OP.78 ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[nN]" THEN
                    LET ls_flag = 0
                END IF
        
                EXIT WHILE
            END IF
        END WHILE
    END IF
    
    RETURN ls_flag, lr_fechas.*
END FUNCTION


#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado transaccional del archivo de la op 78  #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    MESSAGE "GENERANDO ENCABEZADO..." SLEEP 1   --CPL-2912

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_78P"
    ERROR "Generando archivo de encabezado "
    START REPORT cza_tran TO G_LISTA_CZA
       OUTPUT TO REPORT cza_tran()
    FINISH REPORT cza_tran
    
    MESSAGE " "    --CPL-2912

END FUNCTION


#---------------------------------------------------------------------------#
# segundo_paso : Genera el detalle del archivo de la op 78                  #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_fechas)
    DEFINE pr_fechas           RECORD
                                  inicio        DATE,
                                  fin           DATE
                               END RECORD

    DEFINE lr_info_biometrica  RECORD 
                                  id_pago                 INTEGER,
                                  idsolicitante           SMALLINT, --CHAR(2),
                                  curpsolicitante         CHAR(18),
                                  sellotrabajador         CHAR(14),
                                  curpagenteservicio      CHAR(18)
                               END RECORD
                               
    DEFINE lr_insuficiencia     RECORD          --CPL-3378
           ind_notificacion     CHAR(1),
           saldo_estimado       CHAR(12),
           f_ultimo_pago        CHAR(8),
           f_ultima_mens        CHAR(8)
    END RECORD

    DEFINE
        lr_sol_pmg             RECORD LIKE pen_solicitud_pmg.* ,
        lr_detalle_sol         RECORD LIKE pen_detalle_sol.*

    DEFINE
        ruta_det_nss           ,
        ruta_det_sie           CHAR(100)
    
    DEFINE ls_id_pri_pago      SMALLINT
    DEFINE ls_diag             SMALLINT
    DEFINE cont_sello          SMALLINT

    DEFINE li_num_regs         INTEGER
    DEFINE ld_mto_pago_pesos   LIKE pen_ctr_pago_det.mto_pago_pesos
    DEFINE ld_fecha_estimada   LIKE pen_ctr_pago_det.fecha_pago_estimada
    DEFINE ld_saldo_pmg        DECIMAL(18,6)
    
    DEFINE ls_num_mensualidad  SMALLINT       --CPL-2798
    DEFINE ls_cont_regs        SMALLINT       --CPL-2912
    DEFINE ls_num_reg          SMALLINT       --CPL-2912
    DEFINE ld_monto_vigente    DECIMAL(16,2)
    
    DEFINE v_fecha_pago_estimada   CHAR(10)
    DEFINE v_fecha_ult_mensualidad CHAR(10) 
    DEFINE v_provisiona            SMALLINT
    DEFINE v_cta_clabe             CHAR(18)
    DEFINE ld_marca_ult_pago       CHAR(01)

    -- ----------------------------------------------------------------------

    MESSAGE "GENERANDO DETALLE..." SLEEP 1   --CPL-2912
   
    CALL f_tablas_tmp()

    LET li_num_regs = 0
    LET ls_diag     = 0
    LET ls_num_reg  = 0 
    LET v_provisiona     = 0
    
    LET gs_sin_sello = FALSE  --bandera para sabes si hay regs sin sellos

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               "DET-NSS-78-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               "DET-SIE-78-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    #-- Detalle general
    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".DET_78P"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    ----------------------------------------------------------------------
    --SELECCIONA CADA UNA DE LAS SOLICITUDES A CONSIDERAR EN OPERACION 78
    ----------------------------------------------------------------------
    SELECT COUNT(*)                                                                        --CPL-2912
    INTO   ls_cont_regs                                                                    --CPL-2912
    FROM   pen_solicitud_pmg                                                               --CPL-2912
    WHERE  estado_solicitud = gr_edo.confirmado                                            --CPL-2912
    OR    (estado_solicitud = gr_edo.en_proceso_pago AND                                   --CPL-2912
           nss IN ( SELECT UNIQUE(nss)                                                     --CPL-2912
                    FROM   pen_ctr_pago_det                                                --CPL-2912
                    WHERE  fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin  --CPL-2912
                    AND    num_mensualidad  > 1                                            --CPL-2912
                    AND    estado           = gr_edo.capturado                             --CPL-2912
                  )                                                                        --CPL-2912
          )                                                                                --CPL-2912

    DECLARE cur_det CURSOR FOR
    SELECT *    ,
           1
    FROM   pen_solicitud_pmg
    WHERE  estado_solicitud = gr_edo.confirmado
    AND    nss IN ( SELECT UNIQUE(nss)                                                     --CPL-3000
                    FROM   pen_ctr_pago_det                                                --CPL-3000
                    WHERE  fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin  --CPL-3000
                  )                                                      
    UNION
    SELECT *    ,
           0
    FROM   pen_solicitud_pmg
    WHERE  estado_solicitud = gr_edo.en_proceso_pago
    AND    nss IN ( SELECT UNIQUE(nss)                                   
                    FROM   pen_ctr_pago_det                                  
                    WHERE  fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
                    AND    num_mensualidad  > 1                              
                    AND    estado           = gr_edo.capturado                
                  )                                                          
    ORDER BY nss

    -------------------------------------------------------------------
    --PROCESA CADA UNA DE LAS SOLICITUDES A CONSIDERAR EN OPERACION 78
    -------------------------------------------------------------------
    FOREACH cur_det INTO lr_sol_pmg.*, ls_id_pri_pago

        INITIALIZE lr_detalle_sol.* TO NULL
        
        WHENEVER ERROR CONTINUE
        
        LET ls_num_reg = ls_num_reg + 1                                      --CPL-2912
        LET g_mensaje = "PROCESANDO(",ls_num_reg   USING "<<<<", " de ",    --CPL-2912
                                      ls_cont_regs USING "<<<<",")",        --CPL-2912
                                      " NSS: ",lr_sol_pmg.nss               --CPL-2912
        
        MESSAGE g_mensaje CLIPPED                           --CPL-2912
        
           --RECUPERA EL MONTO PAGO EN PESOS y EL NUMERO DE MENSUALIDAD
           SELECT NVL(mto_pago_pesos,0),num_mensualidad,fecha_pago_estimada,marca_ult_pago
           INTO   ld_mto_pago_pesos, ls_num_mensualidad,ld_fecha_estimada,ld_marca_ult_pago        --CPL-2798
           FROM   pen_ctr_pago_det
           WHERE  nss                 = lr_sol_pmg.nss
           AND    consecutivo         = lr_sol_pmg.consecutivo
           AND    fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
           AND  ( estado              = gr_edo.capturado OR
                  estado              = gr_edo.confirmado )

           IF SQLCA.SQLCODE < 0 THEN
              LET g_mensaje = "ERROR NSS:",lr_sol_pmg.nss,"CONSEC:",lr_sol_pmg.consecutivo," CAPTURADO"
              ERROR g_mensaje
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
           END IF
           
           --SE AGREGA VALIDACION PENDIENTE    --CPL-2798
           IF SQLCA.sqlcode = NOTFOUND THEN 
              LET g_mensaje = "NO SE LOCALIZO MENSUALIDAD PARA NSS:",lr_sol_pmg.nss," CONSECUTIVO:",lr_sol_pmg.consecutivo USING "<<<<<<<<"
              ERROR g_mensaje CLIPPED
              CALL ERRORLOG(g_mensaje)

              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF

        WHENEVER ERROR STOP 

        --OBTIENE EL SALDO DE LA CUENTA
        LET ld_saldo_pmg = f_saldo_pmg(lr_sol_pmg.nss)
        LET ld_saldo_pmg = fg_saldo_dia_pmg(lr_sol_pmg.nss,lr_sol_pmg.estado_sub_viv,today)
        

        LET ld_monto_vigente = f_importe_mensual_pmg(lr_sol_pmg.nss, lr_sol_pmg.consecutivo, lr_sol_pmg.fecha_ini_pen) --CPL-3279
        IF ld_monto_vigente <> lr_sol_pmg.importe_mensual  AND
           ld_monto_vigente <> 0 AND ld_monto_vigente IS NOT NULL THEN 
            LET lr_sol_pmg.importe_mensual = ld_monto_vigente
            UPDATE pen_solicitud_pmg
            SET    importe_mensual = ld_monto_vigente
            WHERE  nss = lr_sol_pmg.nss
            AND    consecutivo = lr_sol_pmg.consecutivo

        END IF 

        IF ld_monto_vigente = 0 OR ld_monto_vigente IS NULL THEN      
           --como se venia realizando                                 
           LET ld_monto_vigente = lr_sol_pmg.importe_mensual          
        END IF 

        IF (ld_marca_ult_pago = "*" AND ld_mto_pago_pesos > lr_sol_pmg.importe_mensual 
            AND ld_mto_pago_pesos > ld_monto_vigente) THEN
            UPDATE pen_ctr_pago_det
            SET    mto_pago_pesos = ld_monto_vigente
            WHERE  nss = lr_sol_pmg.nss
            AND    consecutivo = lr_sol_pmg.consecutivo
            AND    num_mensualidad = ls_num_mensualidad

            LET ld_mto_pago_pesos = ld_monto_vigente
        END IF
        
        IF ld_monto_vigente > ld_mto_pago_pesos THEN
            -- Actualiza la tabla con el monto actualizado
            UPDATE pen_ctr_pago_det
            SET    mto_pago_pesos = ld_monto_vigente
            WHERE  nss = lr_sol_pmg.nss
            AND    consecutivo = lr_sol_pmg.consecutivo
            AND    num_mensualidad = ls_num_mensualidad

            LET ld_mto_pago_pesos = ld_monto_vigente
        END IF

        IF ld_mto_pago_pesos > ld_saldo_pmg THEN
        
            UPDATE pen_ctr_pago_det
            SET    mto_pago_pesos = ld_saldo_pmg
            WHERE  nss = lr_sol_pmg.nss
            AND    consecutivo = lr_sol_pmg.consecutivo
            AND    num_mensualidad = ls_num_mensualidad

            LET ld_mto_pago_pesos = ld_saldo_pmg
        END IF
        
        
        
        CALL f_inserta_prop_pago(lr_sol_pmg.*, pr_fechas.*, gdt_fec_oper, ls_id_pri_pago)
        #-- Iniciamos los reportes
        START REPORT det_pension_min TO ruta_det_nss
        START REPORT det_subcuenta   TO ruta_det_sie

           --CONSULTA Y PREPARA INFORMACION BIOMETRICA
           --(SE MODIFICA BLOQUE DE ACUERDO A REQUERIMIENTO CPL-2798)
           IF ls_num_mensualidad = 1 THEN 
              --(PRIMER MENSUALIDAD) SE VERIFICA SI CUENTA CON SELLO
              SELECT count(*)
              INTO   cont_sello
              FROM   ret_sellosbiometricos_suv
              WHERE  nss         = lr_sol_pmg.nss
              AND    consecutivo = lr_sol_pmg.consecutivo      #CPL-2619
              
              IF cont_sello = 0 THEN
                 --NO SE ENCONTRO INFORMACION BIOMETRICA
                 LET gs_sin_sello = TRUE    --Bandera global indica que se encontraron nss que no tienen sello
                 INSERT INTO tmp_sin_sellos VALUES(lr_sol_pmg.nss,
                                                   lr_sol_pmg.consecutivo,
                                                   ls_num_mensualidad    ,
                                                   lr_sol_pmg.fecha_solicitud
                                                  )
                 DELETE FROM tmp_detalle_sol                      --CPL-3000
                 WHERE  nss = lr_sol_pmg.nss                      --CPL-3000
                 AND    consecutivo = lr_sol_pmg.consecutivo      --CPL-3000
                                                                  --CPL-3000
                 DELETE FROM tmp_ctr_pago_det                     --CPL-3000
                 WHERE  nss = lr_sol_pmg.nss                      --CPL-3000
                 AND    consecutivo = lr_sol_pmg.consecutivo      --CPL-3000
                 
                 CONTINUE FOREACH --siguiente registro
              ELSE
                 --RECUPERA INFORMACION BIOMETRICA
                 SELECT identificadorpago,                    #CPL-2619
                        idsolicitante    ,
                        curpsolicitante  ,
                        sellotrabajador  ,
                        curpagenteservicio
                 INTO lr_info_biometrica.id_pago           ,
                      lr_info_biometrica.idsolicitante     ,
                      lr_info_biometrica.curpsolicitante   ,
                      lr_info_biometrica.sellotrabajador   ,
                      lr_info_biometrica.curpagenteservicio 
                 FROM  ret_sellosbiometricos_suv
                 WHERE nss         = lr_sol_pmg.nss
                 AND   consecutivo = lr_sol_pmg.consecutivo   #CPL-2619

                 IF lr_info_biometrica.id_pago != 1 THEN
                    LET g_mensaje = "NSS:",lr_sol_pmg.nss," CONSEC:",lr_sol_pmg.consecutivo USING "<<<<<<<<<&"," CON IDPAGO INCONSISTENTE, <ENTER>:"
                    CALL ERRORLOG(g_mensaje CLIPPED)
                    PROMPT g_mensaje CLIPPED FOR CHAR ENTER
                 ELSE
                    LET lr_info_biometrica.id_pago = 1
                 END IF
              END IF
           ELSE
              --(MENSUALIDADES SUBSECUENTES) SE VALIDA SI EL CONTRATO ES PREVIO A BIOMETRIA
              IF lr_sol_pmg.fecha_solicitud > MDY(1,15,2018) THEN
                 LET lr_info_biometrica.id_pago            = 2                   --subsecuentes
                 LET lr_info_biometrica.idsolicitante      = "  "                
                 LET lr_info_biometrica.curpsolicitante    = "000000000000000000"
                 LET lr_info_biometrica.sellotrabajador    = "00000000000000"    
                 LET lr_info_biometrica.curpagenteservicio = "000000000000000000"
              ELSE
                 --NO ENCONTRO INFORMACIÓN BIOMETRICA
                 LET lr_info_biometrica.id_pago            = 3   --PAGO PREVIO A BIOMETRICOS
                 LET lr_info_biometrica.idsolicitante      = "  "
                 LET lr_info_biometrica.curpsolicitante    = "000000000000000000"
                 LET lr_info_biometrica.sellotrabajador    = "00000000000000"
                 LET lr_info_biometrica.curpagenteservicio = "000000000000000000"
              END IF 
           END IF

        --INICIO CPL-3378
        LET lr_insuficiencia.ind_notificacion = " "
        LET lr_insuficiencia.saldo_estimado  = "            "
        LET lr_insuficiencia.f_ultimo_pago   = "00010101"
        LET lr_insuficiencia.f_ultima_mens   = "00010101"
        LET v_cta_clabe = "0"

        --DISPLAY " ld_saldo_pmg: ", ld_saldo_pmg
        --DISPLAY " ld_mto_pago_pesos: ", ld_mto_pago_pesos
        --DISPLAY " ld_monto_vigente: ", ld_monto_vigente
        --DISPLAY " lr_sol_pmg.importe_mensual : ", lr_sol_pmg.importe_mensual 
        
        IF ls_num_mensualidad > 1 THEN
            IF ld_saldo_pmg < ld_mto_pago_pesos * 12 THEN
                IF ld_saldo_pmg >= (ld_mto_pago_pesos * 4) AND 
                    ld_saldo_pmg < (ld_mto_pago_pesos * 5) THEN
                    LET lr_insuficiencia.ind_notificacion = "4"
                    LET lr_insuficiencia.saldo_estimado  = "            "
                    LET lr_insuficiencia.f_ultimo_pago   = "00010101"
                    LET lr_insuficiencia.f_ultima_mens   = "00010101"
                END IF

                IF ld_saldo_pmg <= ld_mto_pago_pesos + ld_mto_pago_pesos THEN

                    LET v_fecha_pago_estimada = gdt_fec_oper
                    LET v_fecha_ult_mensualidad = gdt_fec_oper

                    SELECT num_cuenta
                    INTO   v_cta_clabe
                    FROM   ret_beneficiario
                    WHERE nss = lr_sol_pmg.nss
                    AND consecutivo = lr_sol_pmg.consecutivo
                    AND consec_benef = 1

                    LET lr_insuficiencia.ind_notificacion = "5"
                    LET lr_insuficiencia.saldo_estimado  = (ld_saldo_pmg - ld_mto_pago_pesos) * 100 USING "&&&&&&&&&&&&"
                    LET g_mensaje = "NSS: ",lr_sol_pmg.nss, " Saldo: ",ld_saldo_pmg
                    CALL ERRORLOG(g_mensaje)
                    LET g_mensaje = "Monto pago pesos: ", ld_mto_pago_pesos, " Estimado: ",lr_insuficiencia.saldo_estimado/100
                    CALL ERRORLOG(g_mensaje)
                    LET lr_insuficiencia.f_ultima_mens   = v_fecha_pago_estimada[7,10],v_fecha_pago_estimada[1,2],v_fecha_pago_estimada[4,5]
                    LET lr_insuficiencia.f_ultimo_pago   = v_fecha_ult_mensualidad[7,10],v_fecha_ult_mensualidad[1,2],v_fecha_ult_mensualidad[4,5]

                END IF
            END IF

            --[CPL-3378] Fallecimiento del trabajador
            IF lr_sol_pmg.ind_fallecimiento IS NOT NULL AND
               lr_sol_pmg.ind_fallecimiento = 1  THEN
                 LET lr_insuficiencia.ind_notificacion = "6"    
                 LET lr_insuficiencia.saldo_estimado  = "            "
                 LET lr_insuficiencia.f_ultimo_pago   = "00010101"
                 LET lr_insuficiencia.f_ultima_mens   = "00010101"
                 LET v_cta_clabe = "0"
            END IF
        --FIN CPL-3378
        ELSE 
            IF ld_saldo_pmg <= ld_mto_pago_pesos + ld_monto_vigente THEN

                LET v_fecha_pago_estimada = gdt_fec_oper
                LET v_fecha_ult_mensualidad = gdt_fec_oper

                SELECT num_cuenta
                INTO   v_cta_clabe
                FROM   ret_beneficiario
                WHERE nss = lr_sol_pmg.nss
                AND consecutivo = lr_sol_pmg.consecutivo
                AND consec_benef = 1

                LET lr_insuficiencia.ind_notificacion = "5"
                LET lr_insuficiencia.saldo_estimado  = (ld_saldo_pmg - ld_mto_pago_pesos) * 100 USING "&&&&&&&&&&&&"
                LET g_mensaje = "NSS: ",lr_sol_pmg.nss, " Saldo: ",ld_saldo_pmg
                CALL ERRORLOG(g_mensaje)
                LET g_mensaje = "Monto pago pesos: ", ld_mto_pago_pesos, " Estimado: ",lr_insuficiencia.saldo_estimado/100
                CALL ERRORLOG(g_mensaje)
                LET lr_insuficiencia.f_ultima_mens   = v_fecha_pago_estimada[7,10],v_fecha_pago_estimada[1,2],v_fecha_pago_estimada[4,5]
                LET lr_insuficiencia.f_ultimo_pago   = v_fecha_ult_mensualidad[7,10],v_fecha_ult_mensualidad[1,2],v_fecha_ult_mensualidad[4,5]
            ELSE 
                IF ld_saldo_pmg >= (ld_monto_vigente * 3) + ld_mto_pago_pesos AND 
                    ld_saldo_pmg < (ld_monto_vigente * 4) + ld_mto_pago_pesos THEN
                    LET lr_insuficiencia.ind_notificacion = "4"
                    LET lr_insuficiencia.saldo_estimado  = "            "
                    LET lr_insuficiencia.f_ultimo_pago   = "00010101"
                    LET lr_insuficiencia.f_ultima_mens   = "00010101"
                END IF
            END IF
        END IF
           
           LET li_num_regs = li_num_regs + 1
           
           --GENERA LA INFORMACION A SER NOTIFICADA
           OUTPUT TO REPORT det_pension_min(lr_sol_pmg.*,lr_info_biometrica.*,lr_insuficiencia.*,v_cta_clabe)

           DECLARE cur_det_sol CURSOR FOR
--           SELECT B.*
--           FROM   tmp_detalle_sol  B
--           WHERE  B.nss          = lr_sol_pmg.nss
--           AND    B.consecutivo  = lr_sol_pmg.consecutivo
--           ORDER BY B.siefore, B.subcuenta

           SELECT B.nss,B.consecutivo,B.folio_lote,B.tipo_retiro,
                  B.siefore,C.agrupamiento,B.fecha_valuacion,
                  SUM(B.monto_en_acciones) AS monto_en_acciones,
                  SUM(B.monto_en_pesos) AS monto_en_pesos      ,
                  B.estado_sub_viv,B.num_mensualidad
           FROM   tmp_detalle_sol  B,
                  tmp_tab_subcuenta C
           WHERE  B.nss          = lr_sol_pmg.nss
           AND    B.consecutivo  = lr_sol_pmg.consecutivo
           AND    B.subcuenta    = C.subcuenta
           GROUP BY B.nss,B.consecutivo,B.folio_lote,B.tipo_retiro,
                    B.siefore,C.agrupamiento,B.fecha_valuacion,
                    B.estado_sub_viv,B.num_mensualidad
           ORDER BY B.siefore, C.agrupamiento

           IF lr_sol_pmg.ind_fallecimiento IS NULL OR lr_sol_pmg.ind_fallecimiento != 1 THEN
               FOREACH cur_det_sol INTO lr_detalle_sol.*
                  OUTPUT TO REPORT det_subcuenta(lr_sol_pmg.*, lr_detalle_sol.*)
               END FOREACH
           END IF

        FINISH REPORT det_pension_min
        FINISH REPORT det_subcuenta

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL f_concatena_reportes(ruta_det_nss   ,
                                  ruta_det_sie   ,
                                  li_num_regs    )

    END FOREACH

    MESSAGE " " --CPL-2912

    RETURN li_num_regs

END FUNCTION


#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de la op 43                   #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_num_regs)
    DEFINE
        pi_num_regs     INTEGER

   MESSAGE "GENERANDO ARCHIVOS..." SLEEP 1   --CPL-2912

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".SUM_78P"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran(pi_num_regs)
    FINISH REPORT sum_tran

   MESSAGE " "  --CPL-2912
   
END FUNCTION


#---------------------------------------------------------------------------#
# cuarto_paso : Concatena los archivos generados para formar el lote        #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()
    DEFINE
        comando     ,
        cat         CHAR(500)

    MESSAGE "PREPARANDO ARCHIVO..." SLEEP 1   --CPL-2912
    
    LET cat = "cat ", G_LISTA_CZA ,
                      G_LISTA_DET ,
                      G_LISTA_SUM ,
                     "> ",g_seg_modulo.ruta_envio CLIPPED,"/",gc_nom_archivo

    RUN cat

    WHENEVER ERROR CONTINUE

    LET comando = "chmod 777 ", G_LISTA_CZA
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_DET
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_SUM
    RUN comando

    LET comando = "chmod 777 ", g_seg_modulo.ruta_envio CLIPPED,"/",gc_nom_archivo
    RUN comando

    WHENEVER ERROR STOP
    
    MESSAGE " " --CPL-2912
    
END FUNCTION


#===========================================================================#
# quinto_paso : Actualiza las tablas de envio y muestra resultados en       #
#               la pantalla                                                 #
# Mofifico    : ISAI JIMENEZ ROJAS                                          #
#===========================================================================#
FUNCTION quinto_paso(pi_tot_regs)
    DEFINE pi_tot_regs           INTEGER
    DEFINE li_folio              INTEGER

    DEFINE lr_act                RECORD
                                    nss              LIKE pen_solicitud_pmg.nss          , 
                                    consecutivo      LIKE pen_solicitud_pmg.consecutivo  
                                 END RECORD

    DEFINE lr_ctr_det            RECORD
                                    nss              LIKE pen_ctr_pago_det.nss               ,
                                    consecutivo      LIKE pen_ctr_pago_det.consecutivo       ,
                                    num_mensualidad  LIKE pen_ctr_pago_det.num_mensualidad   ,
                                    mto_pago_pesos   LIKE pen_ctr_pago_det.mto_pago_pesos 
                                 END RECORD
                                 
    DEFINE ls_contador           SMALLINT 
    DEFINE ld_mto_saldo_pesos    DECIMAL(18,2)
    DEFINE ld_pago_mensual_pesos LIKE pen_ctr_pago.pago_mensual_pesos          
    DEFINE ls_cont_sin_sellos    SMALLINT   --CPL-2798
    DEFINE ld_fecha_ini_pen      LIKE pen_solicitud_pmg.fecha_ini_pen
    -- ----------------------------------------------------------------------

    MESSAGE "ACTUALIZANDO TABLAS..." SLEEP 1   --CPL-2912
    
    --OBTIENE NUEVO FOLIO PARA EL ENVIO
    LET li_folio = f_obtiene_folio()

    --INSERTA LA INFORMACION DE CONTROL DE ENVIO
    INSERT INTO pen_envio
    VALUES ( li_folio               ,
             78                     ,
             HOY                    ,
             CURRENT HOUR TO SECOND ,
             gc_nom_archivo         ,
             pi_tot_regs            ,
             gc_usuario             ,
             gr_edo.enviado             
            )

    -- Se actualizan los registros a los que se pagara la primer mensualidad
    --ACTUALIZA EL ESTADO A ENVIADO DE TODAS LAS SOLICITUDES CONFIRMADAS
    --PRIMER MENSUALIDAD
    UPDATE pen_solicitud_pmg
    SET    folio_lote       = li_folio       ,
           estado_solicitud = gr_edo.enviado
    WHERE  estado_solicitud = gr_edo.confirmado

    --SELECCIONA CADA UNA DE LAS SOLICITUDES EN ESTADO ENVIADO
    DECLARE cur_act CURSOR FOR
    SELECT nss          ,
           consecutivo
    FROM   pen_solicitud_pmg
    WHERE  folio_lote       = li_folio
    AND    estado_solicitud = gr_edo.enviado

    FOREACH cur_act INTO lr_act.*
        --ACTUALIZA ESTADO DE CONTROL DE PAGO
        UPDATE pen_ctr_pago
        SET    folio_lote   = li_folio      ,
               estado       = gr_edo.enviado
        WHERE  nss          = lr_act.nss        
        AND    consecutivo  = lr_act.consecutivo
        
    END FOREACH

    UPDATE tmp_detalle_sol            
    SET    folio_lote       = li_folio
    
    --------------------------------------
    --INSERTA TODOS LOS MONTOS CALCULADOS
    --------------------------------------
    INSERT INTO pen_detalle_sol
    SELECT *
    FROM   tmp_detalle_sol

    ---------------------------------------------------------
    -- SE ACTUALIZA EL FOLIO Y EL ESTADO EN PEN_CTR_PAGO_DET
    ---------------------------------------------------------
    DECLARE cur_ctr_det CURSOR FOR
    SELECT nss              , 
           consecutivo      ,
           num_mensualidad  ,
           sum(monto_en_pesos)
    FROM   pen_detalle_sol
    WHERE  folio_lote   = li_folio  
    GROUP  BY 1,2,3  
    
    FOREACH cur_ctr_det INTO lr_ctr_det.*
      
      --OBTIENE EL PAGO MENSUAL
      --SELECT pago_mensual_pesos                     --CPL-2912
      --INTO   ld_pago_mensual_pesos                  --CPL-2912
      --FROM   pen_ctr_pago                           --CPL-2912
      --WHERE  nss         = lr_ctr_det.nss           --CPL-2912
      --AND    consecutivo = lr_ctr_det.consecutivo   --CPL-2912

      --RECUPERA FECHA DE INICIO DE PENSION --CPL-3279
      SELECT fecha_ini_pen
       INTO   ld_fecha_ini_pen
      FROM   pen_solicitud_pmg
      WHERE  nss          = lr_ctr_det.nss
      AND    consecutivo  = lr_ctr_det.consecutivo

      LET ld_pago_mensual_pesos = f_importe_mensual_pmg(lr_ctr_det.nss, lr_ctr_det.consecutivo, ld_fecha_ini_pen) --CPL-3279

        
      --ONTIENE EL SALDO DE LA CUENTA PARA ACTUALIZAR REGISTRO DE CTR PAGO
      LET ld_mto_saldo_pesos = f_saldo_pmg(lr_ctr_det.nss)
        
    --DISPLAY "lr_ctr_det.nss            =>",lr_ctr_det.nss
    --DISPLAY "lr_ctr_det.mto_pago_pesos =>",lr_ctr_det.mto_pago_pesos 
    --DISPLAY "ld_mto_saldo_pesos        =>",ld_mto_saldo_pesos  
    --DISPLAY "ld_pago_mensual_pesos     =>",ld_pago_mensual_pesos     
    --PROMPT "Enter:" FOR CHAR enter

        IF (lr_ctr_det.mto_pago_pesos >= ld_mto_saldo_pesos) OR
           (ld_mto_saldo_pesos <= ld_pago_mensual_pesos  * 2) THEN
           --------------------------------------------------
           --MARCA LA SOLICITUD COMO ULTIMO PAGO --MLM-1936
           --------------------------------------------------
           IF (lr_ctr_det.mto_pago_pesos >= ld_mto_saldo_pesos) THEN
               UPDATE pen_ctr_pago_det
               SET    folio_op78       = li_folio      ,
                      estado           = gr_edo.enviado,
                      marca_ult_pago   = "*"           ,
                      mto_pago_pesos   = ld_mto_saldo_pesos
               WHERE  nss              = lr_ctr_det.nss
               AND    consecutivo      = lr_ctr_det.consecutivo    
               AND    num_mensualidad  = lr_ctr_det.num_mensualidad
           ELSE
               UPDATE pen_ctr_pago_det
               SET    folio_op78       = li_folio      ,
                      estado           = gr_edo.enviado,
                      marca_ult_pago   = "*"           
               WHERE  nss              = lr_ctr_det.nss
               AND    consecutivo      = lr_ctr_det.consecutivo    
               AND    num_mensualidad  = lr_ctr_det.num_mensualidad
           END IF
        ELSE
           --UNICAMENTE ACTUALIZA ESTADO
           UPDATE pen_ctr_pago_det
           SET    folio_op78       = li_folio      ,
                  estado           = gr_edo.enviado --,     --
                  --marca_ult_pago   = NULL
           WHERE  nss              = lr_ctr_det.nss
           AND    consecutivo      = lr_ctr_det.consecutivo    
           AND    num_mensualidad  = lr_ctr_det.num_mensualidad
        END IF

    END FOREACH
    
    MESSAGE " "   --CPL-2912
    
    SELECT COUNT(*)             --CPL-2798
    INTO   ls_cont_sin_sellos   --CPL-2798
    FROM   tmp_sin_sellos       --CPL-2798

    DISPLAY "                                                                     " AT 7,9
    DISPLAY "                                                                     " AT 11,9
    DISPLAY "                                                                     " AT 15,9
    DISPLAY "                           " AT 18,5
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 9,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 11,20
    DISPLAY "                                                " AT 13,11
    DISPLAY "NOMBRE DEL ARCHIVO     : ", gc_nom_archivo AT 13,20
    DISPLAY "TOTAL DE REGISTROS     : ", pi_tot_regs        AT 14,20
    DISPLAY "REGISTROS NO INCLUIDOS : ", ls_cont_sin_sellos AT 15,20         --CPL-2798
    DISPLAY "FOLIO DE GENERACION    : ", li_folio           AT 16,20

    CALL f_error_msg("PROCESO TERMINADO CORRECTAMENTE")
    
    CLOSE WINDOW penc1031
    
END FUNCTION


#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se captura los datos de la fecha   #
#                  inicial y final para generar la operacion 78             #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW penc1031 AT 2,2 WITH FORM "PENC1031" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - ACEPTAR                                             <CTRL-C> - Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC103            GENERACION DE LA OPERACION 78 - PMG                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY VERSION AT 2,71

END FUNCTION


#---------------------------------------------------------------------------#
# f_obten_fechas : Obtiene las fechas de operacion y de pago de la PMG para #
#                  la generacion de la op. 78 de transferencias             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_fechas()
    DEFINE lr_fechas   RECORD
                          pago_pmg        DATE,
                          operacion       DATE
                       END RECORD

    DEFINE
        ls_dias_hab    SMALLINT

    #-- ---------------------------------------------------------------------

    LET lr_fechas.operacion = HOY
    LET ls_dias_hab         = 3   --MLM-1936

    EXECUTE eje_habil_sig USING HOY,
                                ls_dias_hab
                          INTO  lr_fechas.pago_pmg

    RETURN lr_fechas.*
END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_nombres : Regresa los nombres del nss dado                      #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_nombres(pc_nss)
    DEFINE pc_nss     LIKE pen_solicitud_pmg.nss

    DEFINE lr_afi     RECORD
                         ap_paterno      LIKE afi_mae_afiliado.paterno   ,
                         ap_materno      LIKE afi_mae_afiliado.materno   ,
                         nombres         LIKE afi_mae_afiliado.nombres
                      END RECORD

    --  ---------------------------------------------------------------------

    SELECT paterno  ,
           materno  ,
           nombres
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    RETURN lr_afi.*
END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_folio : Obtiene el ultimo folio para asignarse a la operacion   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_folio()
    DEFINE
       li_ult_folio        INTEGER


    SELECT NVL(MAX(folio),0) + 1
    INTO   li_ult_folio
    FROM   glo_folio
    
    INSERT INTO glo_folio
    VALUES (li_ult_folio)
    
    RETURN li_ult_folio
END FUNCTION


#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)
    DEFINE
        pc_mensaje          CHAR(77)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION


#---------------------------------------------------------------------------#
# f_concatena_reportes : Dadas las rutas de los dos archivos de detalle     #
#                        temporales de los reportes, los va concatenando    #
#                        en uno solo que sera el archivo de detalle final   #
#---------------------------------------------------------------------------#
FUNCTION f_concatena_reportes(lc_det_3, lc_det_4, p_regs)
    DEFINE
        lc_det_3        ,
        lc_det_4        CHAR(100)

    DEFINE
        ps_diag         ,
        p_regs          SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = g_seg_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_seg_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ",
                        lc_det_4 CLIPPED
    LET com_rm = com_rm CLIPPED

    LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                          lc_det_4 CLIPPED, " > ", ruta_tmp
    LET com_cat = com_cat CLIPPED

    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN com_cat
    RUN com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET com_cat = "cat ", G_LISTA_DET, " ", ruta_tmp, " > ", ruta_det
        RUN com_cat

        LET com_cat = " mv ", ruta_det, " ", G_LISTA_DET
        RUN com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET com_cat = " cp ", ruta_tmp, " ", G_LISTA_DET
        RUN com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET com_rm = "rm ", ruta_tmp
    RUN com_rm
END FUNCTION


#---------------------------------------------------------------------------#
# f_inserta_prop_pago : Genera el proporcional por subcuenta para el primer #
#                       pago de pmg del nss dado y lo inserta en la tabla   #
#                       temporal                                            #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_prop_pago(pr_solicitud, pr_fechas, ps_id_pago)
    DEFINE
       pr_solicitud   RECORD LIKE pen_solicitud_pmg.*

    DEFINE
       pr_fechas      RECORD
                         inicio              DATE,
                         fin                 DATE,
                         opera               DATE
                      END RECORD
    
    DEFINE
       ps_id_pago     SMALLINT

    DEFINE
       lr_pago_prop   RECORD
                         subcuenta       SMALLINT        ,
                         siefore         SMALLINT        ,
                         monto_acciones  DECIMAL(16,6)   ,
                         acciones_pagar  DECIMAL(16,6)   ,
                         pesos_pagar     DECIMAL(16,6)
                      END RECORD

    DEFINE
       ld_primer_pago LIKE pen_ctr_pago_det.mto_pago_pesos

    DEFINE
       ls_edo_viv     LIKE pen_solicitud_pmg.estado_sub_viv   ,
       ld_acciones    LIKE pen_detalle_sol.monto_en_acciones  ,
       ldt_fecha_val  LIKE pen_detalle_sol.fecha_valuacion

    DEFINE
       ls_mensualidad SMALLINT

    -- -----------------------------------------------------------------------

    -- Se obtiene la mensualidad a pagar
    IF ps_id_pago = 0 THEN
        SELECT MIN(num_mensualidad)
        INTO   ls_mensualidad
        FROM   pen_ctr_pago_det
        WHERE  nss              = pr_solicitud.nss
        AND    consecutivo      = pr_solicitud.consecutivo
        AND    sec_contrato     = pr_solicitud.sec_contrato
        AND    estado           = gr_edo.capturado
        AND    fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
    ELSE
        LET ls_mensualidad = 1
    END IF

    SELECT mto_pago_pesos
    INTO   ld_primer_pago
    FROM   pen_ctr_pago_det
    WHERE  nss              = pr_solicitud.nss
    AND    consecutivo      = pr_solicitud.consecutivo
    AND    sec_contrato     = pr_solicitud.sec_contrato
    AND    num_mensualidad  = ls_mensualidad

    DECLARE cur_prop CURSOR FOR eje_prop_pmg
    FOREACH cur_prop USING pr_solicitud.nss         ,
                           pr_solicitud.consecutivo ,
                           ld_primer_pago           ,
                           pr_fechas.opera
                     INTO  lr_pago_prop.*

        INITIALIZE ls_edo_viv TO NULL
        LET ld_acciones     = 0

        IF lr_pago_prop.subcuenta <> 4 THEN
            LET ls_edo_viv      = NULL
            LET ld_acciones     = 0
            LET ldt_fecha_val   = NULL
        ELSE
            LET ls_edo_viv      = pr_solicitud.estado_sub_viv
            LET ld_acciones     = lr_pago_prop.acciones_pagar
            LET ldt_fecha_val   = gar_precio_acc[11].fecha
        END IF

        INSERT INTO tmp_detalle_sol
        VALUES (pr_solicitud.nss         ,
                pr_solicitud.consecutivo ,
                0                        , -- folio_lote
                "S"                      , -- tipo_ret
                lr_pago_prop.siefore     ,
                lr_pago_prop.subcuenta   ,
                ldt_fecha_val            , -- fecha_valuacion
                ld_acciones              , -- Monto acciones
                lr_pago_prop.pesos_pagar ,
                ls_edo_viv               ,
                ls_mensualidad             -- Num mensualidad
               )
    END FOREACH -- Pago prop
END FUNCTION


#===========================================================================#
#                                                                           #
#===========================================================================#
FUNCTION f_inserta_pago_saldo(pr_solicitud, pr_fechas, ps_id_pago)
   DEFINE pr_solicitud      RECORD LIKE pen_solicitud_pmg.*

   DEFINE pr_fechas         RECORD
                               inicio              DATE,
                               fin                 DATE,
                               opera               DATE
                            END RECORD
    
   DEFINE ps_id_pago        SMALLINT

   DEFINE ls_edo_viv        LIKE pen_solicitud_pmg.estado_sub_viv   ,
          ldt_fecha_val     LIKE pen_detalle_sol.fecha_valuacion

   DEFINE ls_mensualidad    SMALLINT

   DEFINE v_mto_acciones    DECIMAL(18,2)
   DEFINE v_mto_pesos       DECIMAL(18,6)
   DEFINE v_mto_pesos_tot   DECIMAL(18,6)
   DEFINE v_precio_del_dia  DECIMAL(18,6)
   DEFINE v_subcuenta       SMALLINT
   DEFINE v_siefore         SMALLINT
   DEFINE ld_mto_pago_pesos DECIMAL(18,6)

   LET v_mto_acciones    = 0
   LET v_mto_pesos       = 0
   LET v_mto_pesos_tot   = 0
   LET ld_mto_pago_pesos = 0

   -- SE OBTIENE LA MENSUALIDAD A PAGAR
   IF ps_id_pago = 0 THEN
      SELECT MAX(num_mensualidad)  --TENIA MIN
      INTO   ls_mensualidad
      FROM   pen_ctr_pago_det
      WHERE  nss              = pr_solicitud.nss
      AND    consecutivo      = pr_solicitud.consecutivo
      AND    sec_contrato     = pr_solicitud.sec_contrato
      AND    estado           = gr_edo.capturado
      AND    fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
   ELSE
      LET ls_mensualidad = 1
   END IF 

   -----------------------------------------------------------------
   -- SELECCIONA CADA UNO DE LOS SALDOS DE LA CUENTA DEL TRABAJADOR
   -----------------------------------------------------------------
   DECLARE cur_total CURSOR FOR
   SELECT subcuenta, siefore, ROUND(SUM(monto_en_acciones),2)
   FROM   dis_cuenta
   WHERE  nss = pr_solicitud.nss
   AND    subcuenta IN (1,2,5,6,9,4)
   GROUP  BY 1,2
   HAVING ROUND(SUM(monto_en_acciones),2) > 0.00

   FOREACH cur_total INTO v_subcuenta, v_siefore, v_mto_acciones

      INITIALIZE ls_edo_viv TO NULL
      
      IF v_subcuenta <> 4 THEN
          LET ls_edo_viv      = NULL
          LET ldt_fecha_val   = TODAY  --TENIA NULL
      ELSE
          LET ls_edo_viv      = pr_solicitud.estado_sub_viv
          LET ldt_fecha_val   = gar_precio_acc[11].fecha
      END IF

      --OBTIENE EL VALOR DE ACCION DE LA SIEFORE CORRESPONDIENTE
      SELECT precio_del_dia
      INTO   v_precio_del_dia
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = ldt_fecha_val  --TENIA TODAY
      AND    codigo_siefore  = v_siefore

      LET v_mto_pesos = v_mto_acciones * v_precio_del_dia
      LET ld_mto_pago_pesos = ld_mto_pago_pesos + v_mto_pesos

      IF v_subcuenta <> 4 THEN
          LET v_mto_acciones     = 0
      END IF

      --INSERTA EL DETALLE A PAGAR DE LA SOLICITUD CON BASE AL SALDO
      INSERT INTO tmp_detalle_sol 
      VALUES (pr_solicitud.nss         ,
              pr_solicitud.consecutivo ,
              0                        , -- folio_lote
              "S"                      , -- tipo_ret
              v_siefore                ,
              v_subcuenta   ,
              ldt_fecha_val            , -- fecha_valuacion
              v_mto_acciones           , -- Monto acciones
              v_mto_pesos              ,
              ls_edo_viv               ,
              ls_mensualidad             -- Num mensualidad
             )

       --AL CALCULAR CON BASE AL SALDO MARCA LA SOLICIUD COMO ULTIMO PAGO
       --INSERT INTO tmp_ctr_pago_det
       --VALUES ( pr_solicitud.nss,pr_solicitud.consecutivo,ls_mensualidad)

   END FOREACH

   INSERT INTO tmp_ctr_pago_det
   VALUES ( pr_solicitud.nss,pr_solicitud.consecutivo,ls_mensualidad,ld_mto_pago_pesos )
END FUNCTION


#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_detalle_sol
        DROP TABLE tmp_ctr_pago_det     --MLM-1936
        DROP TABLE tmp_sin_sellos       --CPL-2798
        DROP TABLE tmp_tab_subcuenta
    WHENEVER ERROR STOP

    SELECT *
    FROM   pen_detalle_sol
    WHERE  1 = 0
    INTO TEMP tmp_detalle_sol
    
    --MLM-1936
    SELECT nss, consecutivo, num_mensualidad,mto_pago_pesos
    FROM   pen_ctr_pago_det
    WHERE  1 = 0   
    INTO TEMP tmp_ctr_pago_det
    
    --CPL-2798
    SELECT a.nss, a.consecutivo, a.num_mensualidad, b.fecha_solicitud
    FROM   pen_ctr_pago_det  a,
           pen_solicitud_pmg b
    WHERE  1 = 0 
    INTO TEMP tmp_sin_sellos

    --- CPL-3363
    SELECT subct_cod AS subcuenta, subct_cod AS agrupamiento
    FROM   tab_subcuenta
    WHERE  subct_cod IN (1,2,4,5,6,9)
    INTO   TEMP tmp_tab_subcuenta
    
    UPDATE tmp_tab_subcuenta
    SET    agrupamiento = 3
    WHERE  subcuenta = 5

    UPDATE tmp_tab_subcuenta
    SET    agrupamiento = 2
    WHERE  subcuenta = 9

END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por el usuario                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)
    DEFINE
       pdt_fec_precios    DATE

    DEFINE
       lr_precio_acc      RECORD
                                estado                SMALLINT     ,
                                fecha                 DATE         ,
                                siefore               SMALLINT     ,
                                precio_dia            DECIMAL(16,6)
                          END RECORD

    DEFINE
       lc_exe_precios     CHAR(100) ,
       lc_mensaje         CHAR(100) ,
       lc_siefore         CHAR(002)

    DEFINE
       li_cont            SMALLINT

    LET li_cont = 0

    LET lc_exe_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_exe_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        --IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
        --    LET lc_siefore = lr_precio_acc.siefore
        --    LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", pdt_fec_precios,
        --                     ", SIEFORE: ", lc_siefore
        --
        --    LET lc_mensaje = lc_mensaje CLIPPED
        --    CALL f_error_msg(lc_mensaje)
        --    EXIT PROGRAM
        --ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        --END IF

    END FOREACH
END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(pdt_fecha)
    DEFINE
        pdt_fecha           DATE

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado = 0

    IF pdt_fecha IS NULL THEN
        LET lc_mensaje = " LA FECHA NO DEBE SER NULA"
        LET ls_estado = 1
    ELSE
        IF pdt_fecha < "01/01/1900" THEN
            LET lc_mensaje = " FECHA INVALIDA"
            LET ls_estado = 1
        ELSE
{
            IF pdt_fecha > HOY THEN
                LET lc_mensaje = " LA FECHA NO DEBE SER MAYOR A LA FECHA DEL DIA"
                LET ls_estado = 1
            END IF
}
        END IF
    END IF

    RETURN ls_estado, lc_mensaje
    
END FUNCTION

#==============================================================================#
# CPL-2912                                                                     #
#==============================================================================#
FUNCTION f_importe_mensual_pmg(pc_nss, pc_consecutivo, pc_fecha_ini_pen) 

   DEFINE pc_nss                   CHAR(11)
   DEFINE ld_importe_mensual       DECIMAL(10,2)
   DEFINE ld_monto_pago            DECIMAL(10,2)
   DEFINE lc_tipo                  CHAR(1)
   DEFINE lc_mensaje               CHAR(200)
   DEFINE pc_consecutivo           DECIMAL(11,0)
   DEFINE pc_fecha_ini_pen         DATE
   DEFINE v_saldo_promedio        LIKE ret_datamart_comp.saldo_promedio
   DEFINE v_edad                  LIKE ret_datamart_comp.edad
   DEFINE v_semanas_cotizadas     LIKE ret_det_datamart.semanas_cotizadas
   DEFINE ls_year_actual          SMALLINT
   DEFINE ls_columna              SMALLINT
   DEFINE ls_edad_consulta        SMALLINT
   DEFINE ld_valor_uma            DECIMAL(10,2)
   DEFINE ld_sal_umas             DECIMAL(10,6)
   
   
   WHENEVER ERROR CONTINUE 
   
      --RECUPERA EL MONTO DE PAGO DE LA SOLICITUD
      SELECT importe_mensual
      INTO   ld_importe_mensual
      FROM   pen_solicitud_pmg
      WHERE  nss           = pc_nss
      AND    sec_contrato = (SELECT MAX(sec_contrato)
                             FROM   pen_solicitud_pmg
                             WHERE  nss         = pc_nss
                            )
      AND    estado_solicitud NOT IN(80)
      
      IF SQLCA.sqlcode < 0 THEN
         LET lc_mensaje = "Error al consultar importe mensual NSS: ",pc_nss,"\n",
                          ERR_GET(SQLCA.SQLCODE),
                          "\nSe asigna tipo 2 por omision"
         CALL ERRORLOG(lc_mensaje CLIPPED)
         DISPLAY "(log)" AT 18,70   --debug
         LET lc_tipo = "2"   --por default
      ELSE 
         --BUSCA EL MONTO EN CATALOGO
         SELECT "1"
         INTO   lc_tipo
         FROM   tab_pmg_historica
         WHERE  importe_mensual = ld_importe_mensual
         
         IF lc_tipo IS NULL OR lc_tipo = "" THEN 
            --BUSCA EL MONTO EN CATALOGO
            SELECT "2"
            INTO   lc_tipo
            FROM   tab_pmg_historica
            WHERE  importe_mensual_11p = ld_importe_mensual
            
            IF lc_tipo IS NULL OR lc_tipo = "" THEN 
               LET lc_tipo = "2"   --tipo por default
            END IF
         END IF
      END IF 

    --DETERMINA EL MONTO SEGUN LA FECHA DE INICIO DE LA PENSION CPL-3279
    IF pc_fecha_ini_pen < '01/01/2021' THEN
        --DEPENDIENDO DEL TIPO RECUPERA EL MONTO AUTORIZADO EN CATALOGO
        IF lc_tipo = "1" THEN
            SELECT MAX(importe_mensual)
            INTO   ld_monto_pago 
            FROM   tab_pmg_historica
            WHERE  fecha_hasta IS NULL
        ELSE
            SELECT MAX(importe_mensual_11p)
            INTO   ld_monto_pago 
            FROM   tab_pmg_historica
            WHERE  fecha_hasta IS NULL
        END IF

        WHENEVER ERROR STOP

        IF ld_importe_mensual != ld_monto_pago THEN 
            LET lc_mensaje = "MONTO HISTORICO - NSS: ",pc_nss,
                           " IMPORTE MENSUAL: ",ld_importe_mensual USING "##,##&.&&",
                           " TIPO: ",lc_tipo," A PAGAR: ",ld_monto_pago USING "##,##&.&&"
            CALL ERRORLOG(lc_mensaje CLIPPED )
        END IF   

    ELSE 
        SELECT DISTINCT a.saldo_promedio, a.edad, b.semanas_cotizadas
        INTO   v_saldo_promedio, v_edad, v_semanas_cotizadas
        FROM   ret_datamart_comp a,
               ret_det_datamart b
        WHERE  a.nss = pc_nss
        AND    a.nss = b.nss
        AND    a.folio_t_procesar = b.folio_t_procesar
        AND    a.saldo_promedio > 0  
        AND    a.edad > 0
        AND    a.saldo_promedio = (SELECT MAX(c.saldo_promedio)
                                   FROM   ret_datamart_comp c,
                                          ret_det_datamart d
                                   WHERE  c.nss = pc_nss
                                   AND    c.nss = d.nss
                                   AND    c.folio_t_procesar = d.folio_t_procesar
                                   AND    c.saldo_promedio > 0  
                                   AND    c.edad > 0)

        LET ls_year_actual      = YEAR(pc_fecha_ini_pen)

        SELECT monto_uma
        INTO   ld_valor_uma
        FROM   tab_valor_uma
        WHERE  (fecha_desde_uma <= pc_fecha_ini_pen AND
                fecha_hasta_uma >= pc_fecha_ini_pen)
        OR     (fecha_desde_uma <= pc_fecha_ini_pen AND
                fecha_hasta_uma IS NULL)
                
        SELECT columna
        INTO   ls_columna
        FROM   pen_matriz_anio_semanas
        WHERE  anio = ls_year_actual
        AND    semanas_min <= v_semanas_cotizadas
        AND    semanas_max >= v_semanas_cotizadas
            
        IF v_edad > 65 THEN 
            LET ls_edad_consulta = 65
        ELSE 
            LET ls_edad_consulta = v_edad
        END IF

        LET ld_sal_umas = v_saldo_promedio / ld_valor_uma
        IF ld_sal_umas > 5 THEN 
            LET ld_sal_umas = 5
        END IF 
        IF ld_sal_umas < 1 AND ld_sal_umas > 0 THEN 
            LET ld_sal_umas = 1
        END IF 

        --- Otenemos el monto actualizado (Por los notificados en enero 2021)
        SELECT pesos_pension
        INTO   ld_monto_pago
        FROM   pen_matriz_sem_cotiza
        WHERE  columna = ls_columna
        AND    edad = ls_edad_consulta
        AND    (salario_min_inf <= ld_sal_umas AND salario_min_sup >= ld_sal_umas)
        AND    fecha_fin_vigencia IS NULL
        
    END IF 

   RETURN ld_monto_pago
   
END FUNCTION 


#==============================================================================#
# --CPL-2912                                                                             #
#==============================================================================#
FUNCTION f_valida_historicos_pmg()

   DEFINE lr_tab_pmg_historica    RECORD LIKE tab_pmg_historica.*
   DEFINE ls_contador             SMALLINT 
   
   --VALIDA QUE SOLO HAYA UN MONTO VIGENTE
   SELECT COUNT(*)
   INTO   ls_contador
   FROM   tab_pmg_historica
   WHERE  fecha_hasta IS NULL
   
   IF ls_contador != 1 THEN 
      ERROR "HAY INCONSISTENCIAS EN CATALOGO DE MONTOS IMSS, INFORME A SISTEMAS"
      CALL ERRORLOG("NO HAY UN SOLO REGISTRO EN CATALOGO DE MONTOS AUTORIZADOS IMSS")
      PROMPT "PROCESO CANCELADO, PRESIONE <ENTER>: " FOR CHAR ENTER
      RETURN FALSE 
   END IF
   
   --VALIDA QUE LOS MONTOS ESTEN REGISTRADOS 
   SELECT *
   INTO   lr_tab_pmg_historica.*
   FROM   tab_pmg_historica
   WHERE fecha_hasta IS NULL
   
   IF lr_tab_pmg_historica.importe_mensual     IS NULL OR 
      lr_tab_pmg_historica.importe_mensual     = 0     OR 
      lr_tab_pmg_historica.importe_mensual_11p IS NULL OR
      lr_tab_pmg_historica.importe_mensual_11p = 0     THEN 
      ERROR "HAY INCONSISTENCIAS EN CATALOGO DE MONTOS IMSS, INFORME A SISTEMAS"
      CALL ERRORLOG("NO MONTOS VIGENTES NULOS O EN CERO EN CATALOGO DE MONTOS AUTORIZADOS IMSS")
      RETURN FALSE
   END IF 
   
   RETURN TRUE
        
END FUNCTION 
#==============================================================================#
# cza_tran : Reporte que genera el encabezado del archivo de la op. 78         # 
#==============================================================================#
REPORT cza_tran()
DEFINE v_paso CHAR(50)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
--            CALL ERRORLOG("\n ENCABEZADO ")
--            LET v_paso = gs_codigo_afore USING "&&&"
--            CALL ERRORLOG("\n gs_codigo_afore "||v_paso)
--            LET v_paso = gdt_fec_oper   USING "YYYYMMDD"
--            CALL ERRORLOG("\n gdt_fec_oper "||v_paso)
--            LET v_paso = gdt_fec_pago_pmg USING "YYYYMMDD"
--            CALL ERRORLOG("\n gdt_fec_pago_pmg "||v_paso)
            PRINT
                COLUMN 001, "01"                              ,-- Tipo registro
                COLUMN 003, "04"                              ,-- Id de servicio
                COLUMN 005, "01"                              ,-- Entidad origen
                COLUMN 007, gs_codigo_afore USING "&&&"       ,-- Cve entidad origen
                COLUMN 010, "03"                              ,-- Entidad destino
                COLUMN 012, "001"                             ,-- Cve entidad destino
                COLUMN 015, gdt_fec_oper   USING "YYYYMMDD"   ,-- Fecha operacion
                COLUMN 023, gdt_fec_pago_pmg USING "YYYYMMDD" ,-- Fecha pago de la PMG
                COLUMN 031, 2 SPACES                          ,-- Resultado de la operacion
                COLUMN 033, 3 SPACES                          ,-- Motivo Rechazo 1
                COLUMN 036, 3 SPACES                          ,-- Motivo Rechazo 2
                COLUMN 039, 3 SPACES                          ,-- Motivo Rechazo 3
                COLUMN 042, 309 SPACES                         -- Filler


                
END REPORT


#---------------------------------------------------------------------------#
# det_pension_min : Reporte que genera el detalle del archivo de la op. 43  #
#---------------------------------------------------------------------------#
REPORT det_pension_min(pr_sol_pmg,lr_info_biometrica,pr_insuficiencia,p_cta_clabe)

    DEFINE pr_sol_pmg            RECORD LIKE pen_solicitud_pmg.*

    DEFINE lr_nombre             RECORD
                                    ap_paterno      LIKE afi_mae_afiliado.paterno   ,
                                    ap_materno      LIKE afi_mae_afiliado.materno   ,
                                    nombres         LIKE afi_mae_afiliado.nombres
                                 END RECORD

    DEFINE
       lr_info_biometrica        RECORD                             #CPL-2619
                                    id_pago                 INTEGER,
                                    idsolicitante           SMALLINT,
                                    curpsolicitante         CHAR(18),
                                    sellotrabajador         CHAR(14),
                                    curpagenteservicio      CHAR(18)
                                 END RECORD
    
    DEFINE pr_insuficiencia     RECORD          --CPL-3378
           ind_notificacion     CHAR(1),
           saldo_estimado       CHAR(12),
           f_ultimo_pago        CHAR(8),
           f_ultima_mens        CHAR(8)
    END RECORD
    DEFINE p_cta_clabe          CHAR(18)
    DEFINE v_paso               CHAR(50)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            CALL f_obtiene_nombres(pr_sol_pmg.nss) RETURNING lr_nombre.*
--            CALL ERRORLOG("\n DETALLE 03 ")
--            LET v_paso = gs_codigo_afore USING "&&&"
--            CALL ERRORLOG("\n gs_codigo_afore "|| v_paso)
--            CALL ERRORLOG("\n gs_tipo_op "||gs_tipo_op)
--            LET v_paso = gs_tipo_op USING "&&"
--            CALL ERRORLOG("\n gs_tipo_op "||v_paso)                           -- ID de operacion
--            CALL ERRORLOG("\n pr_sol_pmg.nss "||pr_sol_pmg.nss)                                  -- NSS
--            CALL ERRORLOG("\n pr_sol_pmg.curp "||pr_sol_pmg.curp)                                 -- CURP
--            CALL ERRORLOG("\n lr_nombre.nombres "||lr_nombre.nombres)                               -- Nombre
--            CALL ERRORLOG("\n lr_nombre.ap_paterno "||lr_nombre.ap_paterno)                            -- Paterno
--            CALL ERRORLOG("\n lr_nombre.ap_materno "||lr_nombre.ap_materno)                            -- Materno
--            LET v_paso = pr_sol_pmg.sec_pension USING "&&"
--            CALL ERRORLOG("\n pr_sol_pmg.sec_pension "||v_paso)               -- Sec de Pension
--            CALL ERRORLOG("\n pr_sol_pmg.tipo_retiro "||pr_sol_pmg.tipo_retiro)                          -- Tipo de Retiro
--            CALL ERRORLOG("\n pr_sol_pmg.regimen "||pr_sol_pmg.regimen)                              -- Regimen
--            CALL ERRORLOG("\n pr_sol_pmg.tipo_seguro "||pr_sol_pmg.tipo_seguro)                          -- Tipo de Seguro
--            CALL ERRORLOG("\n pr_sol_pmg.tipo_pension "||pr_sol_pmg.tipo_pension)                         -- Tipo de Pension
--            LET v_paso = pr_sol_pmg.tipo_prestacion USING "&&"
--            CALL ERRORLOG("\n pr_sol_pmg.tipo_prestacion "||v_paso)           -- Tipo de Prestacion
--            LET v_paso = pr_sol_pmg.fecha_ini_pen USING "YYYYMMDD"
--            CALL ERRORLOG("\n pr_sol_pmg.fecha_ini_pen"||v_paso)       -- Fecha Inicio pension
--            LET v_paso = pr_sol_pmg.fecha_resolucion USING "YYYYMMDD"
--            CALL ERRORLOG("\n pr_sol_pmg.fecha_resolucion "||v_paso)    -- Fecha Resolucion
--            LET v_paso = gdt_fec_pago_pmg USING "YYYYMMDD"
--            CALL ERRORLOG("\n gdt_fec_pago_pmg "||v_paso)               -- Fecha pago de la PMG
--            LET v_paso = pr_sol_pmg.fecha_solicitud USING "YYYYMMDD"
--            CALL ERRORLOG("\n pr_sol_pmg.fecha_solicitud "||v_paso)     -- Fecha Solicitud
--            LET v_paso = lr_info_biometrica.id_pago       USING "&"
--            CALL ERRORLOG("\n lr_info_biometrica.id_pago "||v_paso)      -- id_pago          CPL-2619
--            LET v_paso = lr_info_biometrica.idsolicitante USING "&&"
--            CALL ERRORLOG("\n lr_info_biometrica.idsolicitante "||v_paso)     -- id_solicitante   CPL-2619  --CPL-2798
--            CALL ERRORLOG("\n lr_info_biometrica.curpsolicitante "||lr_info_biometrica.curpsolicitante)              -- curp solicitante CPL-2619
--            CALL ERRORLOG("\n lr_info_biometrica.sellotrabajador "||lr_info_biometrica.sellotrabajador)              -- sellotrabajador  CPL-2619
--            CALL ERRORLOG("\n lr_info_biometrica.curpagenteservicio "||lr_info_biometrica.curpagenteservicio)           -- curpagenteservicio CPL-2619
--            CALL ERRORLOG("\n pr_insuficiencia.ind_notificacion "||pr_insuficiencia.ind_notificacion)               -- Indicador de notificacion            --CPL-3378
--            CALL ERRORLOG("\n pr_insuficiencia.saldo_estimado "||pr_insuficiencia.saldo_estimado)                 -- Saldo estimado a transferir          --CPL-3378
--            CALL ERRORLOG("\n pr_insuficiencia.f_ultimo_pago "||pr_insuficiencia.f_ultimo_pago)                  -- Fecha ultimo pago                    --CPL-3378
--            CALL ERRORLOG("\n pr_insuficiencia.f_ultima_mens "||pr_insuficiencia.f_ultima_mens)                  -- Fecha de ultima mensualidad pagada   --CPL-3378
--            LET v_paso = p_cta_clabe USING "&&&&&&&&&&&&&&&&&&"
--            CALL ERRORLOG("\n p_cta_clabe "||v_paso)          -- Cuenta Clabe                         --CPL-3845

        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, gs_tipo_op USING "&&"                           , -- ID de operacion
            COLUMN 007, pr_sol_pmg.nss                                  , -- NSS
            COLUMN 018, pr_sol_pmg.curp                                 , -- CURP
            COLUMN 036, "0"                                             , -- ID instituto (0 = IMSS)
            COLUMN 037, "0"                                             , -- ID portabilidad
            COLUMN 038, lr_nombre.nombres                               , -- Nombre
            COLUMN 078, lr_nombre.ap_paterno                            , -- Paterno
            COLUMN 118, lr_nombre.ap_materno                            , -- Materno
            COLUMN 158, pr_sol_pmg.sec_pension USING "&&"               , -- Sec de Pension
            COLUMN 160, pr_sol_pmg.tipo_retiro                          , -- Tipo de Retiro
            COLUMN 161, pr_sol_pmg.regimen                              , -- Regimen
            COLUMN 163, pr_sol_pmg.tipo_seguro                          , -- Tipo de Seguro
            COLUMN 165, "   "                                           , -- Clave de Pension
            COLUMN 168, pr_sol_pmg.tipo_pension                         , -- Tipo de Pension
            COLUMN 170, pr_sol_pmg.tipo_prestacion USING "&&"           , -- Tipo de Prestacion
            COLUMN 172, pr_sol_pmg.fecha_ini_pen USING "YYYYMMDD"       , -- Fecha Inicio pension
            COLUMN 180, pr_sol_pmg.fecha_resolucion USING "YYYYMMDD"    , -- Fecha Resolucion
            COLUMN 188, gdt_fec_pago_pmg USING "YYYYMMDD"               , -- Fecha pago de la PMG
            COLUMN 196, pr_sol_pmg.fecha_solicitud USING "YYYYMMDD"     , -- Fecha Solicitud
            COLUMN 204, lr_info_biometrica.id_pago       USING "&"      , -- id_pago          CPL-2619
            COLUMN 205, lr_info_biometrica.idsolicitante USING "&&"     , -- id_solicitante   CPL-2619  --CPL-2798
            COLUMN 207, lr_info_biometrica.curpsolicitante              , -- curp solicitante CPL-2619
            COLUMN 225, lr_info_biometrica.sellotrabajador              , -- sellotrabajador  CPL-2619
            COLUMN 239, lr_info_biometrica.curpagenteservicio           , -- curpagenteservicio CPL-2619
            COLUMN 257, pr_insuficiencia.ind_notificacion               , -- Indicador de notificacion            --CPL-3378
            COLUMN 258, pr_insuficiencia.saldo_estimado                 , -- Saldo estimado a transferir          --CPL-3378
            COLUMN 270, pr_insuficiencia.f_ultimo_pago                  , -- Fecha ultimo pago                    --CPL-3378
            COLUMN 278, pr_insuficiencia.f_ultima_mens                  , -- Fecha de ultima mensualidad pagada   --CPL-3378
            COLUMN 286, p_cta_clabe USING "&&&&&&&&&&&&&&&&&&"          , -- Cuenta Clabe                         --CPL-3845
            COLUMN 304, 32 SPACES                                       , -- filler
            COLUMN 337, 3  SPACES                                       , -- Diag de registro
            COLUMN 340, 2  SPACES                                       , -- Resultado de la op
            COLUMN 342, 3  SPACES                                       , -- Mot Rechazo 1
            COLUMN 345, 3  SPACES                                       , -- Mot Rechazo 2
            COLUMN 348, 3  SPACES                                         -- Mot Rechazo 3


END REPORT


#---------------------------------------------------------------------------#
# det_subcuenta : Reporte que genera el det por subcuentas del archivo de   #
#                 la operacion 78                                           #
#---------------------------------------------------------------------------#
REPORT det_subcuenta(pr_sol_pmg, pr_montos)

    DEFINE pr_sol_pmg      RECORD LIKE pen_solicitud_pmg.*
    DEFINE pr_montos       RECORD LIKE pen_detalle_sol.*

    DEFINE
        lc_cve_subcta      CHAR(02)

    DEFINE
        lc_mto_pesos_12    CHAR(12)    ,
        lc_mto_aivs_12     CHAR(12)    ,
        lc_val_aivs_07     CHAR(07)    ,
        lc_mto_pesos_13    CHAR(13)    ,
        lc_mto_aivs_13     CHAR(13)    ,
        lc_val_aivs_08     CHAR(08)    ,
        lc_fecha_aivs      CHAR(08) 
    DEFINE v_paso          CHAR(50)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            -- Obtenemos la clave de la subcuenta de acuerdo al layout
            -- Se agruparon las subcuentas por la forma en la que se reporta en el archivo
            -- La subcuenta 1 = 1, 2 = 2, 4 = 4, 5 = 3, 6 = 6 y 9 = 2
            CASE pr_montos.subcuenta
                WHEN 1
                    LET lc_cve_subcta = "01"
                WHEN 2
                    LET lc_cve_subcta = "02"
                WHEN 3
                    LET lc_cve_subcta = "03"
                WHEN 4
                    LET lc_cve_subcta = "04"
                WHEN 6
                    LET lc_cve_subcta = "06"
            END CASE

            #-- Obtenemos el valor del monto en pesos a pagar
            IF pr_montos.monto_en_pesos IS NULL THEN
                LET pr_montos.monto_en_pesos = 0
            END IF

            LET lc_mto_pesos_13 = pr_montos.monto_en_pesos USING "&&&&&&&&&&.&&"
            LET lc_mto_pesos_12 = lc_mto_pesos_13[01,10],
                                  lc_mto_pesos_13[12,13]

            #-- Obtenemos el monto de las AIVS
            IF pr_montos.monto_en_acciones IS NULL THEN
                LET pr_montos.monto_en_acciones = 0
            END IF

            LET lc_mto_aivs_13 = pr_montos.monto_en_acciones USING "&&&&&&&&&&.&&"
            LET lc_mto_aivs_12 = lc_mto_aivs_13[01,10],
                                 lc_mto_aivs_13[12,13]

            IF pr_montos.subcuenta = 4 THEN 
                #-- Obtenemos el valor de las AIVS
                LET lc_val_aivs_08 = gar_precio_acc[11].precio_dia USING "&&.&&&&&"
                LET lc_val_aivs_07 = lc_val_aivs_08[01,02],
                                     lc_val_aivs_08[04,08]
            
                #-- Formateamos la fecha valuacion
                LET lc_fecha_aivs = pr_montos.fecha_valuacion USING "YYYYMMDD"
            ELSE
                LET lc_val_aivs_07  = "0000000"
                LET lc_fecha_aivs   = "00010101"
            END IF
--            CALL ERRORLOG("\n Detalle 04 ")
--            LET v_paso = gs_tipo_op USING "&&"
--            CALL ERRORLOG("\n gs_tipo_op "||v_paso)
--            CALL ERRORLOG("\n pr_sol_pmg.nss  "||pr_sol_pmg.nss )
--            CALL ERRORLOG("\n pr_sol_pmg.curp "||pr_sol_pmg.curp)
--            CALL ERRORLOG("\n lc_cve_subcta "||lc_cve_subcta)
--            CALL ERRORLOG("\n lc_mto_pesos_12 "||lc_mto_pesos_12)
--            CALL ERRORLOG("\n lc_mto_aivs_12 "||lc_mto_aivs_12)
--            CALL ERRORLOG("\n lc_val_aivs_07 "||lc_val_aivs_07)
--            CALL ERRORLOG("\n lc_fecha_aivs "||lc_fecha_aivs)
        PRINT
            COLUMN 001, "04"                                    , -- Tipo de registro
            COLUMN 003, "04"                                    , -- ID de servicio
            COLUMN 005, gs_tipo_op USING "&&"                   , -- ID de operacion
            COLUMN 007, pr_sol_pmg.nss                          , -- NSS
            COLUMN 018, pr_sol_pmg.curp                         , -- CURP
            COLUMN 036, lc_cve_subcta                           , -- Clave de subcuenta
            COLUMN 038, lc_mto_pesos_12                         , -- Monto en pesos por subcta
            COLUMN 050, lc_mto_aivs_12                          , -- Monto en AIVS
            COLUMN 062, lc_val_aivs_07                          , -- Valor AIV
            COLUMN 069, lc_fecha_aivs                           , -- Fecha Valor viv
            COLUMN 077, 274 SPACES                                -- Filler

END REPORT


#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del archivo de la op. 78         #
#---------------------------------------------------------------------------#
REPORT sum_tran(pi_num_regs)
    DEFINE
        pi_num_regs     INTEGER
    DEFINE v_paso       CHAR(50)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
--            CALL ERRORLOG("\n SUMARIO ")
--            LET v_paso = gs_codigo_afore  USING "&&&"
--            CALL ERRORLOG("\n gs_codigo_afore "||v_paso)
--            LET v_paso = gs_codigo_afore  USING "YYYYMMDD"
--            CALL ERRORLOG("\n gdt_fec_oper "||v_paso)
--            LET v_paso = gs_codigo_afore  USING "&&&&&&"
--            CALL ERRORLOG("\n pi_num_regs "||v_paso)
            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, gdt_fec_oper USING "YYYYMMDD"   ,-- Fecha operacion
                COLUMN 023, pi_num_regs USING "&&&&&&"      ,-- Total registros
                COLUMN 029, 322 SPACES                       -- Filler





END REPORT


#===========================================================================#
# Objetivo: devolver el saldo en pesos de las subcuentas involucradas en PMG#
#         : valuadas las acciones al día de ejecucion                       #
#===========================================================================#
FUNCTION f_saldo_pmg(p_nss)
   DEFINE p_nss               CHAR(11)
   DEFINE v_mto_acciones      DECIMAL(18,6)
   DEFINE v_mto_pesos         DECIMAL(18,6)
   DEFINE v_mto_pesos_tot     DECIMAL(18,6)
   DEFINE v_precio_del_dia    DECIMAL(18,6)
   DEFINE v_monto_en_pesos    DECIMAL(18,6)
   DEFINE v_monto_en_acciones DECIMAL(18,6)
   DEFINE v_subcuenta         SMALLINT
   DEFINE v_siefore           SMALLINT
   DEFINE lc_comando          CHAR(100)
   DEFINE ls_grupo            SMALLINT
   DEFINE v_fecha_inicio      DATE
   
   DEFINE v_estado_sub_viv    SMALLINT
   

   LET v_mto_acciones   = 0
   LET v_mto_pesos      = 0
   LET v_mto_pesos_tot  = 0
   LET ls_grupo         = 0
   LET v_estado_sub_viv = 1
   LET v_fecha_inicio = MDY(MONTH(HOY),1,YEAR(HOY))

   SELECT estado_sub_viv
   INTO   v_estado_sub_viv
   FROM   pen_solicitud_pmg
   WHERE  nss = p_nss
   AND    estado_solicitud IN (gr_edo.confirmado, gr_edo.en_proceso_pago) 
   
   DECLARE cur_sdo_pmg CURSOR FOR
   SELECT subcuenta, siefore, sum(monto_en_acciones)
   FROM   dis_cuenta
   WHERE  nss = p_nss
   AND    subcuenta IN (1,2,5,6,9,4)
   GROUP  BY 1,2

   FOREACH cur_sdo_pmg INTO v_subcuenta, v_siefore, v_monto_en_acciones      

      IF (v_subcuenta == 4) THEN
          --OBTIENE EL PRECIO DE LA SIEFORE SELECCIONADA
          SELECT precio_del_dia
          INTO   v_precio_del_dia
          FROM   glo_valor_accion
          WHERE  fecha_valuacion = v_fecha_inicio
          AND    codigo_siefore  = v_siefore
      ELSE
          SELECT precio_del_dia
          INTO   v_precio_del_dia
          FROM   glo_valor_accion
          WHERE  fecha_valuacion = TODAY
          AND    codigo_siefore  = v_siefore
      END IF

      IF v_estado_sub_viv == 2 AND v_subcuenta == 4 THEN
        LET v_monto_en_pesos = 0
      ELSE
        LET v_monto_en_pesos = v_monto_en_acciones * v_precio_del_dia
      END IF
      
      LET v_mto_pesos_tot = v_mto_pesos_tot + v_monto_en_pesos

   END FOREACH

   RETURN v_mto_pesos_tot

END FUNCTION

#==============================================================================#
# Implementada en CPL-2798                                                     #
#==============================================================================#
FUNCTION f_muestra_lista_sin_sellos()

   DEFINE la_sin_sellos       ARRAY[100] OF RECORD
          nss                 LIKE pen_ctr_pago_det.nss,
          consecutivo         LIKE pen_ctr_pago_det.consecutivo,
          num_mensualidad     LIKE pen_ctr_pago_det.num_mensualidad,
          fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud
          END RECORD 

   DEFINE ls_pos              SMALLINT 
   DEFINE lc_mensaje          CHAR(200)


   OPTIONS FORM LINE 1,
           PROMPT LINE LAST -1 


   LET ls_pos = 1
   
   --SELECCIONA NSS IDENTIFICADOS SIN BIOMETRICOS
   DECLARE cursin CURSOR FOR
   SELECT *
   FROM   tmp_sin_sellos
   
   FOREACH cursin INTO la_sin_sellos[ls_pos].*
      --NOTIFICA EN LOG
      LET lc_mensaje = "SIN SELLO(BIOMETRICOS):\n",
                       "NSS        : ",la_sin_sellos[ls_pos].nss,"\n",
                       "CONSECUTIVO: ",la_sin_sellos[ls_pos].consecutivo USING "<<<<<<<<<","\n",
                       "MENSUALIDAD: ",la_sin_sellos[ls_pos].num_mensualidad,"\n",
                       "F.SOLICITUD: ",la_sin_sellos[ls_pos].fecha_solicitud
      CALL ERRORLOG(lc_mensaje CLIPPED)
      
      LET ls_pos = ls_pos + 1
      
      IF ls_pos > 100 THEN
         ERROR "SOLO SE MOSTRARAN 100 REGISTROS"
         EXIT FOREACH
      END IF

   END FOREACH
   
   LET ls_pos = ls_pos -1
   
   CALL SET_COUNT(ls_pos)
   
   OPEN WINDOW w_sin at 2,2 WITH FORM "PENC1032" ATTRIBUTE(BORDER)

      DISPLAY ARRAY la_sin_sellos TO sa_sin_sellos.*
      
   CLOSE WINDOW w_sin


END FUNCTION 

