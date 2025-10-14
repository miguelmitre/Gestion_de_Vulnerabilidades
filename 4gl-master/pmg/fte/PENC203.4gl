################################################################################
#Owner             => E.F.P.                                                   #
#Programa PENC203  => GENERACION DE LA OPERACION 72 DE PMG ISSSTE              #
#Sistema           => PEN                                                      #
#Version           => UNICA INVERCAP y COPPEL                                  #
#Fecha creacion    => 19-sep-2019 con base a ultime version de INV             #
#Por               => ISAI JIMENEZ ROJAS                                       #
#Actualizacion     => INV-4731 implementacion de campos biometricos en layout  #
#==============================================================================#
#ACTUALIZACION     => VERSION UNICA CPL e INV                                  #
#Actualizacion     =>                                                          #
#Actualizacion     => CPL-3030 Se corrige inclusion de ID tramite en detalle03 #
################################################################################ 
DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo          RECORD LIKE seg_modulo.*

    DEFINE gar_precio_acc        ARRAY [90] OF RECORD
           estado                SMALLINT     ,
           fecha                 DATE         ,
           siefore               SMALLINT     ,
           precio_dia            DECIMAL(16,6)
           END RECORD

    DEFINE gr_edo                RECORD
           capturado             LIKE pen_estado_pmg.estado_solicitud    ,
           confirmado            LIKE pen_estado_pmg.estado_solicitud    ,
           enviado               LIKE pen_estado_pmg.estado_solicitud    ,
           en_proceso_pago       LIKE pen_estado_pmg.estado_solicitud    ,
           liquidado             LIKE pen_estado_pmg.estado_solicitud
           END RECORD

    DEFINE gr_rango_fechas       RECORD
           inicio                DATE,
           fin                   DATE
           END RECORD

    DEFINE gdt_fec_pago_pmg      DATE 
    DEFINE gdt_fec_oper          DATE 
    DEFINE HOY                   DATE

    DEFINE gc_nom_archivo        CHAR(012)
    DEFINE G_LISTA_DET           CHAR(100)
    DEFINE G_LISTA_CZA           CHAR(100)
    DEFINE G_LISTA_SUM           CHAR(100)
    DEFINE enter                 CHAR(001)
    DEFINE gc_usuario            CHAR(015)

    DEFINE gs_tipo_op            SMALLINT 
    DEFINE gs_procesa            SMALLINT 
    DEFINE gs_codigo_afore       SMALLINT

    DEFINE gi_num_regs           INTEGER 
    DEFINE g_mensaje             CHAR(80)

    DEFINE VERSION               CHAR(7)
    
    DEFINE mc_mensaje            CHAR(200)
END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    DEFER INTERRUPT
    
    OPTIONS INPUT WRAP         ,
            PROMPT LINE LAST   ,
            MESSAGE LINE LAST -1

    CALL init()
    
    LET VERSION = "v1.4731"        --INV-4731
    LET VERSION = "v1.0001"        --INV-5387
    LET VERSION = "v1.0002"        --CPL-3030
        
    CALL STARTLOG(gc_usuario CLIPPED||".PENC203.log")

    IF gs_codigo_afore != 562 AND gs_codigo_afore != 568  THEN
       CALL ERRORLOG("EL PROGRAMA HA SIDO EJECUTADO EN UNA AFORE NO VALIDA...")
       PROMPT "AFORE NO VALIDA: " FOR CHAR ENTER
       EXIT PROGRAM 
    END IF 
    
    CALL f_captura_fechas() RETURNING gs_procesa, gr_rango_fechas.*

    IF gs_procesa THEN
        
       CALL f_obten_fechas() RETURNING gdt_fec_pago_pmg, gdt_fec_oper

       LET  gc_nom_archivo = gdt_fec_oper USING "YYYYMMDD",".72P"

       CALL f_genera_encabezado()              --Genera encabezado primer_paso()

       CALL f_genera_detalle(gr_rango_fechas.*)  --Genera detalle segundo_paso()
           RETURNING gi_num_regs

       IF gi_num_regs > 0 THEN
        
          CALL f_genera_sumario(gi_num_regs)        --Genera el sumario tercer_paso
          
          CALL f_concatena_archivo()                --Concatena los archivos
          
          CALL f_actualiza_tablas(gi_num_regs)     --Actualiza tablas
       ELSE
          ERROR " NO HAY REGISTROS A PROCESAR "
          PROMPT "PRESIONE ENTER:" FOR CHAR ENTER
       END IF
    ELSE
       CALL f_error_msg("PROCESO CANCELADO")
       CLOSE WINDOW penc2031
    END IF

END MAIN

#==============================================================================#
# init : Inicializa las variables globales que se usaran en el programa        #
#==============================================================================#
FUNCTION init()

    DEFINE lc_prepare      CHAR(300)

    LET HOY             = TODAY
    
    LET gs_tipo_op      = 72

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

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#==============================================================================#
# f_captura_fechas : Captura el rango de fechas en el que se buscara para      #
#                    generar la operacion 72                                   #
#==============================================================================#
FUNCTION f_captura_fechas()

    DEFINE lr_fechas          RECORD
           inicio             DATE,
           fin                DATE
           END RECORD

    DEFINE li_tot_registros   INTEGER
    DEFINE li_tot_por_pagar   INTEGER
    DEFINE li_tot_prov        INTEGER

    DEFINE ls_estado          SMALLINT 
    DEFINE ls_flag            SMALLINT

    DEFINE lc_mensaje         CHAR(100)

    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()
    
    LET ls_flag = 1

    CALL f_obtiene_precios_accion(HOY)
    LET li_tot_registros    = 0 
    LET li_tot_por_pagar    = 0 
    LET li_tot_prov         = 0 

    --SUGIERE FECHA INICIO
    SELECT NVL(MIN(fecha_pago_estimada),TODAY), 
           NVL(MAX(fecha_pago_estimada),TODAY)
    INTO   lr_fechas.inicio,
           lr_fechas.fin
    FROM   pen_ctr_pago_det_iss
    WHERE  estado IN(10,20)   --10=capturaro, 20=confirmado

    LET lr_fechas.inicio    = MDY(MONTH(TODAY),"01",YEAR(TODAY))  --INV-4730
    LET lr_fechas.fin       = f_ultimo_dia_mes(TODAY)             --INV-4730
  

    --CUENTA MENSUALIDADES NUEVAS
    SELECT NVL(COUNT(*),0)
    INTO   li_tot_prov
    FROM   pen_solicitud_iss
    WHERE  estado_solicitud = gr_edo.confirmado
    AND    nss IN (SELECT nss 
                   FROM   pen_ctr_pago_det_iss
                   WHERE  fecha_pago_estimada BETWEEN lr_fechas.inicio AND lr_fechas.fin)


    DISPLAY "RANGO DE FECHAS PARA MENSUALIDADES MAYORES A 1 : " AT 7,9
    DISPLAY "SOLICITUDES CON MENSUALIDAD 1 POR PAGAR    : ", li_tot_prov AT 15,9

    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD inicio
            CALL f_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                NEXT FIELD inicio
            END IF

        AFTER FIELD fin
            CALL f_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    NEXT FIELD inicio
                END IF
            END IF

            --RECUENTA MENSUALIDADES NUEVAS DEL PERIODO CAPTURADO
            SELECT NVL(COUNT(*),0)
            INTO   li_tot_prov
            FROM   pen_solicitud_iss
            WHERE  estado_solicitud = gr_edo.confirmado
            AND    nss IN (SELECT nss 
                           FROM   pen_ctr_pago_det_iss
                           WHERE  fecha_pago_estimada BETWEEN lr_fechas.inicio AND lr_fechas.fin)

            DISPLAY "SOLICITUDES CON MENSUALIDAD 1 POR PAGAR    : ", li_tot_prov AT 15,9


        ON KEY (CONTROL-C,INTERRUPT)
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (ESC)
            CALL f_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                NEXT FIELD inicio
            END IF

            CALL f_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    NEXT FIELD inicio
                END IF
            END IF

            LET ls_flag = 1
            EXIT INPUT

    END INPUT

    IF ls_flag = 1 THEN

        --CUENTA MENSUALIDADAES SUBSECUENTES
        SELECT NVL(COUNT(*),0)
        INTO   li_tot_por_pagar
        FROM   pen_ctr_pago_det_iss
        WHERE  fecha_pago_estimada BETWEEN lr_fechas.inicio AND lr_fechas.fin    
          AND  ( (num_mensualidad = 1 AND estado = 10 ) OR 
                 (num_mensualidad > 1 AND estado IN (10,20) ) )
      
      #--  AND    estado          IN (10,20) -- gr_edo.capturado
        
        LET li_tot_registros = li_tot_por_pagar + li_tot_prov

        IF li_tot_registros = 0 THEN
            CALL f_error_msg("NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO")
            EXIT PROGRAM
        ELSE
            --MUSTRA LISTA DE SOLICITUDES A ENVIAR
            CALL f_muestra_lista(lr_fechas.*)
        END IF
        
        WHILE TRUE
            PROMPT "¿ DESEA GENERAR EL ARCHIVO DE LA OP.72 ? (S/N) : " FOR CHAR enter
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

#==============================================================================#
# primer_paso : Genera el encabezado transaccional del archivo de la op 72     #
#==============================================================================#
FUNCTION f_genera_encabezado()

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_72P"

    START REPORT rep_encabezado TO G_LISTA_CZA
        OUTPUT TO REPORT rep_encabezado()
    FINISH REPORT rep_encabezado

END FUNCTION

#==============================================================================#
# segundo_paso : Genera el detalle del archivo de la op 72                     #
#==============================================================================#
FUNCTION f_genera_detalle(pr_fechas)

    DEFINE pr_fechas              RECORD
           inicio                 DATE,
           fin                    DATE
           END RECORD

    DEFINE lr_sol_pmg             RECORD LIKE pen_solicitud_iss.*
    DEFINE lr_detalle_sol         RECORD LIKE pen_detalle_sol.*

    DEFINE lc_ruta_det_nss        CHAR(100)
    DEFINE lc_ruta_det_sie        CHAR(100)
    
    DEFINE ls_id_pri_pago         SMALLINT

    DEFINE li_num_regs            INTEGER
    DEFINE ld_mto_pago_pesos      LIKE pen_ctr_pago_det_iss.mto_pago_pesos
    DEFINE ld_saldo_pmg           DECIMAL(18,6)

    DEFINE ls_existe_33           SMALLINT       -- MLM-4110
    DEFINE ls_existe_34           SMALLINT       -- MLM-4110
    DEFINE ls_has_suma            SMALLINT       -- MLM-4110
    DEFINE ls_mto_pesos           DECIMAL(16,6)  -- MLM-4110    
    DEFINE lc_sql                 CHAR(500)
    
  --DEFINE lr_biometricos         RECORD LIKE tbl_biometricos_sellos_retiro_total.*      --INV-4731 -- SE QUITA LA "S" POR INDICACION DEL AFORE INV-4947
    DEFINE lr_biometricos         RECORD
           tipo_tramite           SMALLINT,
           idsolicitante          INTEGER ,
           curp_solicitante       CHAR(18),
           curp_agente_servicio   VARCHAR(18),
           sello_unico_trabajador CHAR(14),
           curp_trabajador        CHAR(18)
           END RECORD
    DEFINE ls_num_mensualidad     SMALLINT                           --INV-4731
    DEFINE lv_curp_agente         VARCHAR(18)                        --INV-4947
    DEFINE ls_longitud            SMALLINT                           --INV-4947    
    DEFINE pd_pago_mensual        DECIMAL(10,2)                                 --CPL-3827
    DEFINE pd_pago_retroactivo    LIKE pen_solicitud_iss.pago_retroactivo       --CPL-3827
    DEFINE pd_pago_primer_mes     LIKE pen_solicitud_iss.pago_primer_mes        --CPL-3827
    DEFINE pd_bandera_montos      SMALLINT                                      --CPL-3827
    
    ----------------------------------------------------------------------------

    MESSAGE "GENERANDO DETALLES..." ATTRIBUTE(REVERSE) SLEEP 1

    CALL f_tablas_tmp()

    LET li_num_regs = 0

    #-- Determinamos la ubicacion de los reportes
    LET lc_ruta_det_nss = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               "DET-NSS-72-tmp"
    LET lc_ruta_det_nss = lc_ruta_det_nss CLIPPED

    LET lc_ruta_det_sie = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               "DET-SIE-72-tmp"
    LET lc_ruta_det_sie = lc_ruta_det_sie CLIPPED

    #-- Detalle general
    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".DET_72P"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED


    WHENEVER ERROR CONTINUE 

    ----------------------------------------------------------------------
    --SELECCIONA CADA UNA DE LAS SOLICITUDES A CONSIDERAR EN OPERACION 72
    ----------------------------------------------------------------------
    DECLARE cur_det CURSOR FOR 
    SELECT *    ,
           1
    FROM   pen_solicitud_iss
    WHERE  estado_solicitud = gr_edo.confirmado
    UNION  -----------------------------------------
    SELECT *    ,
           0
    FROM   pen_solicitud_iss
    WHERE  estado_solicitud = gr_edo.en_proceso_pago
    AND    nss IN ( SELECT UNIQUE(nss)
                    FROM   pen_ctr_pago_det_iss
                    WHERE  fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
                    --AND    num_mensualidad  > 1
                    AND    estado           IN (10,20)  --gr_edo.capturado
                  )
    ORDER BY nss

    -------------------------------------------------------------------
    --PROCESA CADA UNA DE LAS SOLICITUDES A CONSIDERAR EN OPERACION 72
    -------------------------------------------------------------------
    FOREACH cur_det INTO lr_sol_pmg.*, ls_id_pri_pago

        --OBTIENE EL SALDO DE LA CUENTA
        LET ld_saldo_pmg = f_saldo_pmg_iss(lr_sol_pmg.nss,HOY)
        
        --SE VALIDA SALDO PARA EXCLUIR REGISTROS SIN SALDO
        IF ld_saldo_pmg <= 0 THEN 
           LET mc_mensaje = "EL NSS ",lr_sol_pmg.nss," NO CUENTA CON SALDOS...",ld_saldo_pmg USING "<<<,<<<,<<&.&&"
           CALL ERRORLOG(mc_mensaje CLIPPED)
           ERROR mc_mensaje CLIPPED
           PROMPT "PRESIONE <ENTER> PARA CONTNUAR: " FOR CHAR ENTER
           CONTINUE FOREACH
        END IF

        INITIALIZE lr_detalle_sol.* TO NULL

        --CPL-3827 Se realiza el calculo para comparar montos con el Front de Coppel
        IF ls_id_pri_pago = 1 AND (lr_sol_pmg.tipo_retiro = 'M' 
                                    OR lr_sol_pmg.tipo_retiro = 'N') THEN
            LET pd_pago_mensual = 0
            LET pd_pago_retroactivo = 0
            LET pd_pago_primer_mes = 0
            LET pd_bandera_montos = 0
            
            LET pd_pago_mensual = f_obten_pago_mensual()
            CALL f_obten_montos_pago(lr_sol_pmg.nss, lr_sol_pmg.fecha_ini_pen, 
                    pd_pago_mensual) RETURNING  pd_pago_retroactivo,
                                                    pd_pago_primer_mes
            IF lr_sol_pmg.pago_mensual <> pd_pago_mensual OR lr_sol_pmg.pago_retroactivo <> pd_pago_retroactivo
                    OR lr_sol_pmg.pago_primer_mes <> pd_pago_primer_mes THEN
                CALL fn_actualiza_montos(lr_sol_pmg.*,pd_pago_mensual,pd_pago_retroactivo,pd_pago_primer_mes)
            END IF
        END IF
        
        --RECUPERA EL MONTO PAGO EN PESOS
        SELECT NVL(mto_pago_pesos,0),
               num_mensualidad
        INTO   ld_mto_pago_pesos,
               ls_num_mensualidad
        FROM   pen_ctr_pago_det_iss
        WHERE  nss         = lr_sol_pmg.nss
        AND    consecutivo = lr_sol_pmg.consecutivo
        AND    fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
        AND    estado      IN( gr_edo.capturado, gr_edo.confirmado)
        
        IF SQLCA.SQLCODE < 0 THEN
           LET g_mensaje = "ERROR NSS:",lr_sol_pmg.nss,"CONSEC:",lr_sol_pmg.consecutivo," CAPTURADO"
           CALL ERRORLOG(g_mensaje)   --MLM-4088
           ERROR g_mensaje
           PROMPT "PRESIONE ENTER: " FOR CHAR enter
        END IF
        
        ------------------------------------------------------
        --RECUPERA INFORMACION BIOMETRICA SEGUN No MENSUALIDAD   --INV-4731
        ------------------------------------------------------

        INITIALIZE lr_biometricos.* TO NULL
        
        IF (ls_num_mensualidad = 1) OR ((ls_num_mensualidad-1) mod 12) = 0 THEN   --no es erronea la formula --CPL-3030
           
           -- se envia informacion biometrica en la mensualidad inicial de cada 
           --contrato 1, 13, 25, 37, 49... etc
           
           --PREPARA SELECCION DE INFORMACION BIOMETRICA
           CASE 
              WHEN gs_codigo_afore = 562     --INVERCAP
                   LET lc_sql = '\n SELECT tipo_tramite          ,              ',
                                '\n        curp_trabajador       ,              ',
                                '\n        sello_unico_trabajador,              ',
                                '\n        curp_agente_servicio  ,              ',
                                '\n        idsolicitante         ,              ',
                                '\n        curp_solicitante                     ',
                                '\n FROM   tbl_biometricos_sellos_retiro_total  ',       -- SE QUITA LA "S" POR INDICACION DEL AFORE INV-4947
                                '\n WHERE  nss         = ?                      ',       --lr_sol_pmg.nss      
                                '\n AND    consecutivo = ?                      '       --lr_sol_pmg.consecutivo

              WHEN gs_codigo_afore = 568     --COPPEL
                   LET lc_sql = '\n SELECT identificadorpago ,                  ',
                                '\n        curptrabajador    ,                  ',
                                '\n        sellotrabajador   ,                  ',
                                '\n        curpagenteservicio,                  ',
                                '\n        idsolicitante     ,                  ',
                                '\n        curpsolicitante                      ',
                                '\n FROM  ret_sellosbiometricos_suv             ',
                                '\n WHERE nss         = ?                       ',
                                '\n AND   consecutivo = ?                       '
              OTHERWISE
                   PROMPT "AFORE NO VALIDA, PRESIONE <ENTER>: " FOR CHAR ENTER
                   EXIT PROGRAM
           END CASE
              
           
           PREPARE exebio FROM lc_sql

           EXECUTE exebio USING lr_sol_pmg.nss,
                                lr_sol_pmg.consecutivo
                           INTO lr_biometricos.tipo_tramite          ,   --no sirve para el negocio
                                lr_biometricos.curp_trabajador       ,
                                lr_biometricos.sello_unico_trabajador,
                                lr_biometricos.curp_agente_servicio  ,
                                lr_biometricos.idsolicitante         ,
                                lr_biometricos.curp_solicitante

           IF SQLCA.SQLCODE < 0 THEN
              LET g_mensaje = "ERROR AL RECUPERAR INFORMACION BIOMETRICA NSS:",lr_sol_pmg.nss
              CALL ERRORLOG(g_mensaje CLIPPED)
              CALL ERRORLOG(ERR_GET(SQLCA.sqlcode))
              PROMPT "ERROR AL RECUPERAR INFORMACION BIOMETRICA, PRESIONE <ENTER>: " FOR CHAR enter
              CONTINUE FOREACH 
           END IF
           
           IF SQLCA.SQLCODE = NOTFOUND THEN
              LET g_mensaje = "NO SE RECUPERO INFORMACION BIOMETRICA NSS:",lr_sol_pmg.nss
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF
           
           LET lr_biometricos.tipo_tramite = 1   --PRIMER PAGO         --CPL-3030
           
        ELSE
           --EVALUA SI EL CONTRATO ES PREVIO AL INICIO DE BIOMETRICOS
           --TOMANDO COMO BASE LA FECHA DE CAPTURA DE LA SOLICITUD
           IF lr_sol_pmg.fecha_captura < "01/15/2018" THEN 
              --PREVIO A BIOMETRICOS
              LET lr_biometricos.tipo_tramite           = 3
              LET lr_biometricos.sello_unico_trabajador = "00000000000000"
           ELSE
              --SUBSECUENTE
              LET lr_biometricos.tipo_tramite           = 2
              LET lr_biometricos.sello_unico_trabajador = "00000000000000"
           END IF
        
        END IF 

        -------------------------------------------------
        --INSERTA LOS MONTOS DE DETALLES DE LA SOLICITUD
        -------------------------------------------------
        IF ld_saldo_pmg <= (ld_mto_pago_pesos) THEN -- + lr_sol_pmg.importe_mensual) THEN
           --AGOTAR RECURSOS
           CALL f_inserta_pago_saldo(lr_sol_pmg.*, pr_fechas.*,      
                                     gdt_fec_oper, ls_id_pri_pago)   
        ELSE
           --CALCULA PROPORCION
           CALL f_inserta_prop_pago(lr_sol_pmg.*, pr_fechas.*, gdt_fec_oper, ls_id_pri_pago)
        END IF
        
        ------------------------------------------------
        --INV-4731   VALIDACIONES DE DATOS BIOMETRICOS
        ------------------------------------------------
        
        --DEBE CONTAR CON EL ID PAGO
        IF lr_biometricos.tipo_tramite = 1 THEN 
           --DEBE CONTAR CON CURP DEL TRABAJADOR
           IF lr_biometricos.curp_trabajador IS NULL OR
              lr_biometricos.curp_trabajador = ""    OR
              LENGTH(lr_biometricos.curp_trabajador) = 0 THEN
           
              LET g_mensaje = "NSS ",lr_sol_pmg.nss," SIN CURP DE TRABAJADOR "
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF 
        END IF
        
        --VALIDA CURP DEL TRABAJADOR (no se valida cuando es anterior a biometricos)
        IF lr_biometricos.tipo_tramite = 1  THEN  --1er pago
          
           --DEBE TENER UN ID SOLICITANTE VALIDO
           IF lr_biometricos.idsolicitante != "1" AND
            --lr_biometricos.idsolicitante != "2" AND
              lr_biometricos.idsolicitante != "3" AND
              lr_biometricos.idsolicitante != "4" AND
              lr_biometricos.idsolicitante != "5" THEN 
              
              LET g_mensaje = "NSS ",lr_sol_pmg.nss," SIN ID SOLICITANTE PARA PRIMER PAGO"
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF
           
           --DEBE CONTAR CON CURP DE SOLICITANTE
           IF lr_biometricos.curp_solicitante IS NULL OR 
              lr_biometricos.curp_solicitante = ""    OR
              LENGTH(lr_biometricos.curp_solicitante) = 0 THEN
              
              LET g_mensaje = "NSS ",lr_sol_pmg.nss," SIN CURP DE SOLICITANTE EN INICIO DE CONTRATO"
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF
           
           --DEBE CONTAR CON SELLO DE VERIFICACION
           IF lr_biometricos.sello_unico_trabajador IS NULL OR 
              lr_biometricos.sello_unico_trabajador = ""    OR
              LENGTH(lr_biometricos.sello_unico_trabajador) = 0 THEN
              
              LET g_mensaje = "NSS ",lr_sol_pmg.nss," SIN SELLO DE TRABAJADOR EN INCIO DE CONTRATO"
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF

           --DEBE CONTAR CON CURP AGENTE DE SERVICIO
           IF lr_biometricos.curp_agente_servicio IS NULL OR 
              lr_biometricos.curp_agente_servicio = ""    OR
              LENGTH(lr_biometricos.curp_agente_servicio) = 0 THEN
              
              LET g_mensaje = "NSS ",lr_sol_pmg.nss," SIN CURP AGENTE SERVICIO EN INCIO DE CONTRATO"
              ERROR g_mensaje
              CALL ERRORLOG(g_mensaje)
              PROMPT "PRESIONE ENTER: " FOR CHAR enter
              CONTINUE FOREACH
           END IF

        END IF
              
        --INV-4731 FIN DE VALIDACIONES

        --SE ENVIA LA INFORMACION DE DETALLE AL REPORTE
        START REPORT rep_detalle03 TO lc_ruta_det_nss
        START REPORT rep_detalle04 TO lc_ruta_det_sie

        LET li_num_regs = li_num_regs + 1

        -- INV-4947
        
        LET lv_curp_agente = lr_biometricos.curp_agente_servicio CLIPPED 
        LET ls_longitud    = LENGTH(lv_curp_agente)
                
        IF ls_longitud = 18 THEN
        	 -- LA CURP DEL AGENTE DE SERVICIO ESTA BIEN 
        ELSE 
        	 -- PONER ESPACIOS PARA CUMPLIR CON LA ESTRUCTURA DEL LAYOUT 
        	 --                                         123456789012345678
        	 LET lr_biometricos.curp_agente_servicio = "                  "
        END IF 	     
        

        ----------------------------------
        --ENVIA INFORMACION AL LISTADO
        ----------------------------------
        OUTPUT TO REPORT rep_detalle03(lr_sol_pmg.*,lr_biometricos.*)

        -- MLM-4110 INI 
        
        LET ls_existe_33 = 0
        LET ls_existe_34 = 0
        LET ls_has_suma  = 0 
        LET ls_mto_pesos = 0 
        
        -- VER SI EXISTE LA 33
        SELECT COUNT(*) INTO ls_existe_33
          FROM tmp_detalle_sol  B
         WHERE B.nss          = lr_sol_pmg.nss
           AND B.consecutivo  = lr_sol_pmg.consecutivo
           AND B.subcuenta    = 33 
        
        DISPLAY " ls_existe_33 ",ls_existe_33
        
        -- VER SI EXISTE LA 34
        SELECT COUNT(*) INTO ls_existe_34
          FROM tmp_detalle_sol  B
         WHERE B.nss          = lr_sol_pmg.nss
           AND B.consecutivo  = lr_sol_pmg.consecutivo
           AND B.subcuenta    = 34
           
        DISPLAY " ls_existe_34 ",ls_existe_34        
        
        IF ls_existe_33 = 1 AND ls_existe_34 = 1 THEN 
           LET ls_has_suma = 1 
        END IF 
        
        -- MLM-41140 FIN 


        DECLARE cur_det_sol CURSOR FOR
        SELECT B.*
        FROM   tmp_detalle_sol  B
        WHERE  B.nss          = lr_sol_pmg.nss
        AND    B.consecutivo  = lr_sol_pmg.consecutivo
        ORDER  BY B.siefore, B.subcuenta

        FOREACH cur_det_sol INTO lr_detalle_sol.*

           DISPLAY "lr_sol_pmg"
           DISPLAY lr_sol_pmg.*

           DISPLAY "----"

           DISPLAY "lr_detalle_sol"
           DISPLAY lr_detalle_sol.*

           DISPLAY "*******************"
           
           DISPLAY "has_ suma ",ls_has_suma 
           
           IF ls_has_suma = 1 THEN 
              IF lr_detalle_sol.subcuenta = 33 THEN 
                 LET ls_mto_pesos = lr_detalle_sol.monto_en_pesos
                 display "guarde monto ",ls_mto_pesos, "< "
              END IF   
             
              IF lr_detalle_sol.subcuenta = 34 THEN 
                 display "monto 35 ",lr_detalle_sol.monto_en_pesos
                 LET lr_detalle_sol.monto_en_pesos = lr_detalle_sol.monto_en_pesos + ls_mto_pesos 
           	     display "suma     ",lr_detalle_sol.monto_en_pesos
              END IF
           END IF 	
           
           IF ls_has_suma = 1 AND lr_detalle_sol.subcuenta = 33 THEN 
           	  -- NO MANDAR EL DETALLE
           ELSE
           	 	  
              -- GENERA INFORMACION PARA DETALLE 04
              
              -- INV-4947
              -- NO ENVIAR SUBCUENTA DE VIVIENDA EN CEROS AL ARCHIVO
              IF lr_detalle_sol.subcuenta = 35 THEN 
                 IF lr_detalle_sol.monto_en_pesos = 0 THEN 
                 	  -- NO MANDAR AL REPORTE 
                 ELSE 
                 	  OUTPUT TO REPORT rep_detalle04(lr_sol_pmg.*, lr_detalle_sol.*) 
                 END IF 		  
              ELSE
                 OUTPUT TO REPORT rep_detalle04(lr_sol_pmg.*, lr_detalle_sol.*)
              END IF    
              -- INV-4947
              
           END IF    


        END FOREACH

        FINISH REPORT rep_detalle03
        FINISH REPORT rep_detalle04

        --CONCATENA LA SALIDA
        CALL f_concatena_reportes(lc_ruta_det_nss   ,
                                  lc_ruta_det_sie   ,
                                  li_num_regs    )

    END FOREACH

    WHENEVER ERROR STOP

    MESSAGE " "
    
    RETURN li_num_regs

END FUNCTION

#==============================================================================#
# tercer_paso : Genera el sumario del archivo de la op 43                      #
#==============================================================================#
FUNCTION f_genera_sumario(pi_num_regs)

    DEFINE pi_num_regs     INTEGER

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".SUM_72P"

    START REPORT rep_sumario TO G_LISTA_SUM
        OUTPUT TO REPORT rep_sumario(pi_num_regs)
    FINISH REPORT rep_sumario

END FUNCTION

#==============================================================================#
# cuarto_paso : Concatena los archivos generados para formar el lote           #
#==============================================================================#
FUNCTION f_concatena_archivo()

    DEFINE comando     CHAR(500)
    DEFINE cat         CHAR(500)
    
    ----------------------------------------------------------------------------

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

END FUNCTION

#==============================================================================#
# quinto_paso : Actualiza las tablas de envio y muestra resultados en          #
#               la pantalla                                                    #
# Mofifico    : ISAI JIMENEZ ROJAS                                             #
#==============================================================================#
FUNCTION f_actualiza_tablas(pi_tot_regs)

    DEFINE pi_tot_regs             INTEGER
    DEFINE li_folio                INTEGER

    DEFINE lr_act                  RECORD
           nss                     LIKE pen_solicitud_iss.nss          , 
           consecutivo             LIKE pen_solicitud_iss.consecutivo  
           END RECORD

    DEFINE lr_ctr_det              RECORD
           nss                     LIKE pen_ctr_pago_det_iss.nss               ,
           consecutivo             LIKE pen_ctr_pago_det_iss.consecutivo       ,
           num_mensualidad         LIKE pen_ctr_pago_det_iss.num_mensualidad   ,
           mto_pago_pesos          LIKE pen_ctr_pago_det_iss.mto_pago_pesos 
           END RECORD
           
    DEFINE ls_estado_sub_viv       SMALLINT 
    
    DEFINE ld_mto_saldo_pesos      DECIMAL(18,6)
    DEFINE ld_pago_mensual_pesos   LIKE pen_ctr_pago.pago_mensual_pesos
    
    ----------------------------------------------------------------------------

    --OBTIENE NUEVO FOLIO PARA EL ENVIO
    LET li_folio = f_obtiene_folio()

    DISPLAY "REGISTRANDO EN PEN_ENVIO..."          --DEBUG
    MESSAGE "REGISTRANDO EN PEN_ENVIO..." SLEEP 1 --DEBUG
    --INSERTA LA INFORMACION DE CONTROL DE ENVIO
    INSERT INTO pen_envio
    VALUES ( li_folio               ,
             72                     ,
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
    DISPLAY "ACTUALIZANDO EN PEN_ENVIO..."         --DEBUG
    MESSAGE "ACTUALIZANDO EN PEN_ENVIO..." SLEEP 1 --DEBUG
    
    UPDATE pen_solicitud_iss
    SET    folio_lote       = li_folio       ,
           estado_solicitud = gr_edo.enviado
    WHERE  estado_solicitud = gr_edo.confirmado

    --SELECCIONA CADA UNA DE LAS SOLICITUDES EN ESTADO ENVIADO
    --DECLARE cur_act CURSOR FOR
    --SELECT nss          ,
    --       consecutivo
    --FROM   pen_solicitud_iss
    --WHERE  folio_lote       = li_folio
    --AND    estado_solicitud = gr_edo.enviado
    --
    --FOREACH cur_act INTO lr_act.*
    --    --ACTUALIZA ESTADO DE CONTROL DE PAGO
    --    UPDATE pen_ctr_pago
    --    SET    folio_lote   = li_folio      ,
    --           estado       = gr_edo.enviado
    --    WHERE  nss          = lr_act.nss        
    --    AND    consecutivo  = lr_act.consecutivo
    --    
    --END FOREACH

    UPDATE tmp_detalle_sol            
    SET    folio_lote       = li_folio
    --------------------------------------
    --INSERTA TODOS LOS MONTOS CALCULADOS
    --------------------------------------
    
    DISPLAY "INSERTANDO EN DETALLE SOL..."         --DEBUG
    MESSAGE "INSERTANDO EN DETALLE SOL..." SLEEP 1 --DEBUG
    
    INSERT INTO pen_detalle_sol
    SELECT *
    FROM   tmp_detalle_sol

    ---------------------------------------------------------
    -- SE ACTUALIZA EL FOLIO Y EL ESTADO EN pen_ctr_pago_det_iss
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
      
      --OBTIENE EL SALDO DE LA CUENTA PARA ACTUALIZAR REGISTRO DE CTR PAGO
      LET ld_mto_saldo_pesos = f_saldo_pmg_iss(lr_ctr_det.nss,HOY) --, ls_estado_sub_viv)
      
      MESSAGE "ACTUALIZANDO MENSUALIDAD..." SLEEP 1 --DEBUG        
      
      IF FGL_GETENV("DEBUG")="1" THEN
           --UNICAMENTE ACTUALIZA ESTADO
           DISPLAY "ACTUALIZANDO MENSUALIDAD..."         --DEBUG

           
           DISPLAY "folio           ",li_folio
           DISPLAY "estado          ",gr_edo.enviado
           DISPLAY "nss             ",lr_ctr_det.nss
           DISPLAY "consecutivo     ",lr_ctr_det.consecutivo  
           DISPLAY "num_mensualidad ",lr_ctr_det.num_mensualidad
      END IF 

      --ACTUALIZA LA MENSUALIDAD
      UPDATE pen_ctr_pago_det_iss
         SET folio_op72        = li_folio      ,
             estado            = gr_edo.enviado,
             marca_ult_pago    = NULL
       WHERE nss               = lr_ctr_det.nss
         AND consecutivo       = lr_ctr_det.consecutivo    
         AND num_mensualidad   = lr_ctr_det.num_mensualidad
         AND estado            IN (10,20)

      IF FGL_GETENV("DEBUG")="1" THEN           
         IF SQLCA.SQLERRD[3] > 0 THEN 
            DISPLAY "REGISTRO ACTUALIZADO "
         ELSE 
            DISPLAY "NO ACTUALIZO " 
         END IF 	
         PROMPT "PRESiONE ENTER" CLIPPED ATTRIBUTE(REVERSE) FOR CHAR enter
      END IF   

    END FOREACH

    DISPLAY "                                                                     " AT 7,9
    DISPLAY "                                                                     " AT 11,9
    DISPLAY "                                                                     " AT 15,9
    DISPLAY "                           " AT 18,5
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 9,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 11,20
    DISPLAY "                                                " AT 13,11
    DISPLAY "NOMBRE DEL ARCHIVO  : ", gc_nom_archivo AT 13,20
    DISPLAY "TOTAL DE REGISTROS  : ", pi_tot_regs AT 14,20
    DISPLAY "FOLIO DE GENERACION : ", li_folio AT 15,20

    CALL f_error_msg("PROCESO TERMINADO CORRECTAMENTE")
    CLOSE WINDOW penc2031

END FUNCTION

#==============================================================================#
# f_abre_ventana : Abre la ventana donde se captura los datos de la fecha      #
#                  inicial y final para generar la operacion 72                #
#==============================================================================#
FUNCTION f_abre_ventana()

    OPEN WINDOW penc2031 AT 2,2 WITH FORM "PENC2031" ATTRIBUTE(BORDER)
    
    DISPLAY "                                                             <CTRL-C> - Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC203            GENERACION DE LA OPERACION 72 - PMG                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY VERSION AT 2,71

END FUNCTION

#==============================================================================#
# f_obten_fechas : Obtiene las fechas de operacion y de pago de la PMG para    #
#                  la generacion de la op. 72 de transferencias                #
#==============================================================================#
FUNCTION f_obten_fechas()

    DEFINE lr_fechas          RECORD
           pago_pmg           DATE,
           operacion          DATE
           END RECORD

    DEFINE ls_dias_hab        SMALLINT

    ----------------------------------------------------------------------------

    LET lr_fechas.operacion = HOY
    LET ls_dias_hab         = 2   

    EXECUTE eje_habil_sig USING HOY,
                                ls_dias_hab
                          INTO  lr_fechas.pago_pmg

    RETURN lr_fechas.*

END FUNCTION

#==============================================================================#
# f_obtiene_nombres : Regresa los nombres del nss dado                         #
#==============================================================================#
FUNCTION f_obtiene_nombres(pc_nss)

    DEFINE pc_nss          LIKE pen_solicitud_iss.nss

    DEFINE lr_afi          RECORD
           ap_paterno      LIKE afi_mae_afiliado.paterno   ,
           ap_materno      LIKE afi_mae_afiliado.materno   ,
           nombres         LIKE afi_mae_afiliado.nombres
           END RECORD

    ----------------------------------------------------------------------------

    SELECT paterno  ,
           materno  ,
           nombres
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    RETURN lr_afi.*

END FUNCTION

#==============================================================================#
# f_obtiene_folio : Obtiene el ultimo folio para asignarse a la operacion      #
#==============================================================================#
FUNCTION f_obtiene_folio()

    DEFINE li_ult_folio        INTEGER

    --RECUPERA UN NUEVO FOLIO DE LOTE
    SELECT NVL(MAX(folio),0) + 1
    INTO   li_ult_folio
    FROM   glo_folio
    
    INSERT INTO glo_folio
    VALUES (li_ult_folio)
    
    RETURN li_ult_folio

END FUNCTION

#==============================================================================#
# f_concatena_reportes : Dadas las rutas de los dos archivos de detalle        #
#                        temporales de los reportes, los va concatenando       #
#                        en uno solo que sera el archivo de detalle final      #
#==============================================================================#
FUNCTION f_concatena_reportes(pc_det_3, pc_det_4, p_regs)

    DEFINE pc_det_3        CHAR(100)
    DEFINE pc_det_4        CHAR(100)
    DEFINE p_regs          SMALLINT

    DEFINE lc_ruta_tmp     CHAR(100)
    DEFINE lc_ruta_det     CHAR(100)

    DEFINE lc_com_cat         CHAR(500)
    DEFINE lc_com_rm          CHAR(500)

    ----------------------------------------------------------------------------

    LET lc_ruta_tmp = g_seg_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    LET lc_ruta_det = g_seg_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET lc_ruta_det = lc_ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET lc_com_rm = "rm ", pc_det_3 CLIPPED, " ",
                        pc_det_4 CLIPPED
    LET lc_com_rm = lc_com_rm CLIPPED

    LET lc_com_cat = "cat ", pc_det_3 CLIPPED, " ",
                          pc_det_4 CLIPPED, " > ", lc_ruta_tmp
    LET lc_com_cat = lc_com_cat CLIPPED


    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN lc_com_cat
    RUN lc_com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET lc_com_cat = "cat ", G_LISTA_DET, " ", lc_ruta_tmp, " > ", lc_ruta_det
        RUN lc_com_cat

        LET lc_com_cat = " mv ", lc_ruta_det, " ", G_LISTA_DET
        RUN lc_com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET lc_com_cat = " cp ", lc_ruta_tmp, " ", G_LISTA_DET
        RUN lc_com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET lc_com_rm = "rm ", lc_ruta_tmp
    RUN lc_com_rm

END FUNCTION

#==============================================================================#
# f_inserta_prop_pago : Genera el proporcional por subcuenta para el primer    #
#                       pago de pmg del nss dado y lo inserta en la tabla      #
#                       temporal                                               #
#==============================================================================#
FUNCTION f_inserta_prop_pago(pr_solicitud, pr_fechas, ps_id_pago)

    DEFINE pr_solicitud        RECORD LIKE pen_solicitud_iss.*

    DEFINE pr_fechas           RECORD
           inicio              DATE    ,
           fin                 DATE    ,
           opera               DATE     
           END RECORD
    
    DEFINE ps_id_pago          SMALLINT

    DEFINE lr_pago_prop       RECORD
           subcuenta          SMALLINT        ,
           siefore            SMALLINT        ,
           monto_acciones     DECIMAL(16,6)   ,
           acciones_pagar     DECIMAL(16,6)   ,
           pesos_pagar        DECIMAL(16,6)
           END RECORD

    DEFINE ld_primer_pago     LIKE pen_ctr_pago_det_iss.mto_pago_pesos

    DEFINE ls_edo_viv         SMALLINT --LIKE pen_solicitud_iss.estado_sub_viv
    DEFINE ld_acciones        LIKE pen_detalle_sol.monto_en_acciones 
    DEFINE ldt_fecha_val      LIKE pen_detalle_sol.fecha_valuacion

    DEFINE ls_mensualidad     SMALLINT

    ----------------------------------------------------------------------------

    -- Se obtiene la mensualidad a pagar
    --IF ps_id_pago = 0 THEN
        SELECT MAX(num_mensualidad)
        INTO   ls_mensualidad
        FROM   pen_ctr_pago_det_iss
        WHERE  nss              = pr_solicitud.nss
        AND    consecutivo      = pr_solicitud.consecutivo
        AND    sec_contrato     = pr_solicitud.sec_contrato
        AND    estado           IN( gr_edo.capturado, gr_edo.confirmado)
        AND    fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin
    --ELSE
    --    LET ls_mensualidad = 1
    --END IF
    
    --LET ls_mensualidad = pr_solicitud.num_mensualidades 

    --RECUPERA EL MONTO A PAGAR DE LA MENSUALIDAD CORRESPONDIENTE
    SELECT mto_pago_pesos
    INTO   ld_primer_pago
    FROM   pen_ctr_pago_det_iss
    WHERE  nss              = pr_solicitud.nss
    AND    consecutivo      = pr_solicitud.consecutivo
    AND    sec_contrato     = pr_solicitud.sec_contrato
    AND    num_mensualidad  = ls_mensualidad

  --DECLARE cur_prop CURSOR FOR eje_prop_pmg
    PREPARE eje_prop_iss FROM "EXECUTE FUNCTION fn_calcula_prop_iss(?,?,?)"
    
    DECLARE cur_prop CURSOR FOR eje_prop_iss

    FOREACH cur_prop USING pr_solicitud.nss         ,
                           ld_primer_pago           ,
                           pr_fechas.opera
                     INTO  lr_pago_prop.*

        INITIALIZE ls_edo_viv TO NULL
        
        LET ld_acciones     = 0

        IF lr_pago_prop.subcuenta <> 35 THEN
            LET ld_acciones     = 0
            LET ldt_fecha_val   = NULL
        ELSE
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
                ls_edo_viv               ,  --null para isssste
                ls_mensualidad             -- Num mensualidad
               )
    END FOREACH -- Pago prop

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_inserta_pago_saldo(pr_solicitud, pr_fechas, ps_id_pago)

   DEFINE pr_solicitud        RECORD LIKE pen_solicitud_iss.*

   DEFINE pr_fechas           RECORD
          inicio              DATE    ,
          fin                 DATE    ,
          opera               DATE     
          END RECORD
    
   DEFINE ps_id_pago          SMALLINT

   DEFINE ls_edo_viv          SMALLINT
   DEFINE ldt_fecha_val       LIKE pen_detalle_sol.fecha_valuacion

   DEFINE ls_mensualidad      SMALLINT

   DEFINE ld_mto_acciones     DECIMAL(18,2)
   DEFINE ld_mto_pesos        DECIMAL(18,2)
   DEFINE ld_mto_pesos_tot    DECIMAL(18,2)
   DEFINE ld_precio_del_dia   DECIMAL(18,6)
   
   DEFINE ld_subcuenta        SMALLINT
   DEFINE ls_siefore          SMALLINT
   DEFINE ld_mto_pago_pesos   DECIMAL(18,2)
   
   -----------------------------------------------------------------------------

   LET ld_mto_acciones   = 0
   LET ld_mto_pesos      = 0
   LET ld_mto_pesos_tot  = 0
   LET ld_mto_pago_pesos = 0

   -- SE OBTIENE LA MENSUALIDAD A PAGAR
   IF ps_id_pago = 0 THEN
      SELECT MAX(num_mensualidad)  --TENIA MIN
      INTO   ls_mensualidad
      FROM   pen_ctr_pago_det_iss
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
   AND    subcuenta IN (30,31,32,33,34,35)
   GROUP  BY 1,2
   HAVING ROUND(SUM(monto_en_acciones),2) > 0.00

   FOREACH cur_total INTO ld_subcuenta, ls_siefore, ld_mto_acciones

      INITIALIZE ls_edo_viv TO NULL
      
      IF ld_subcuenta <> 35 THEN
          LET ls_edo_viv      = NULL
          LET ldt_fecha_val   = TODAY --TENUA NULL
      ELSE
          LET ls_edo_viv      = NULL --pr_solicitud.estado_sub_viv
          LET ldt_fecha_val   = gar_precio_acc[11].fecha
      END IF

      --OBTIENE EL VALOR DE ACCION DE LA SIEFORE CORRESPONDIENTE
      SELECT precio_del_dia
      INTO   ld_precio_del_dia
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = ldt_fecha_val  --TENIA TODAY
      AND    codigo_siefore  = ls_siefore

      LET ld_mto_pesos = ld_mto_acciones * ld_precio_del_dia
      LET ld_mto_pago_pesos = ld_mto_pago_pesos + ld_mto_pesos


     IF ld_subcuenta <> 35 THEN
         LET ld_mto_acciones     = 0
     END IF

     IF ld_subcuenta <> 35 THEN --OR ( ld_subcuenta = 0 AND pr_solicitud.estado_sub_viv = 1)  THEN 
        --no se registra monto para vivienda
        --INSERTA EL DETALLE A PAGAR DE LA SOLICITUD CON BASE AL SALDO
        INSERT INTO tmp_detalle_sol 
               VALUES (pr_solicitud.nss         ,
                       pr_solicitud.consecutivo ,
                       0                        , -- folio_lote
                       "S"                      , -- tipo_ret
                       ls_siefore                ,
                       ld_subcuenta   ,
                       ldt_fecha_val            , -- fecha_valuacion
                       ld_mto_acciones           , -- Monto acciones
                       ld_mto_pesos              ,
                       ls_edo_viv               , --null para issste
                       ls_mensualidad             -- Num mensualidad
                      )
     END IF
       --AL CALCULAR CON BASE AL SALDO MARCA LA SOLICIUD COMO ULTIMO PAGO
       --INSERT INTO tmp_ctr_pago_det
       --VALUES ( pr_solicitud.nss,pr_solicitud.consecutivo,ls_mensualidad)
       
   END FOREACH
       
       INSERT INTO tmp_ctr_pago_det
       VALUES ( pr_solicitud.nss,pr_solicitud.consecutivo,ls_mensualidad,ld_mto_pago_pesos )

END FUNCTION

#==============================================================================#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los           #
#                calculos y cambios                                            #
#==============================================================================#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_detalle_sol
        DROP TABLE tmp_ctr_pago_det   
    WHENEVER ERROR STOP

    SELECT *
    FROM   pen_detalle_sol
    WHERE  1 = 0
    INTO TEMP tmp_detalle_sol
    
    
    SELECT nss, consecutivo, num_mensualidad,mto_pago_pesos
    FROM   pen_ctr_pago_det_iss
    WHERE  1 = 0   
    INTO TEMP tmp_ctr_pago_det


END FUNCTION

#==============================================================================#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha       #
#                            dada por el usuario                               #
#==============================================================================#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE pdt_fec_precios       DATE

    DEFINE lr_precio_acc         RECORD
           estado                SMALLINT     ,
           fecha                 DATE         ,
           siefore               SMALLINT     ,
           precio_dia            DECIMAL(16,6)
           END RECORD

    DEFINE lc_exe_precios        CHAR(100) ,
           lc_mensaje            CHAR(100) ,
           lc_siefore            CHAR(002)

    DEFINE li_cont               SMALLINT

    ----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
   
    LET li_cont = 0

    LET lc_exe_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_exe_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*
        IF SQLCA.sqlcode < 0 THEN 
           PROMPT "ERROR AL RECUPERAR PRECIOS DE ACCION, NOTIFIQUE A SISTEMAS, PRESIONE <ENTER>:" FOR CHAR ENTER
           CALL ERRORLOG("Error al ejecutar fn_verifica_precio_accion()")
           CALL ERRORLOG(ERR_GET(SQLCA.sqlcode))
           EXIT PROGRAM 
        END IF
        
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
WHENEVER ERROR STOP 
END FUNCTION

#==============================================================================#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla     #
#                   las condiciones necesarias para ser aceptada               #
#==============================================================================#
FUNCTION f_valida_fechas(pdt_fecha)

    DEFINE pdt_fecha           DATE
    DEFINE ls_estado           SMALLINT
    DEFINE lc_mensaje          CHAR(100)

    ----------------------------------------------------------------------------

    LET ls_estado = 0

    IF pdt_fecha IS NULL THEN
       LET lc_mensaje = " LA FECHA NO DEBE SER NULA"
       LET ls_estado  = 1
    ELSE
       IF pdt_fecha < "01/01/1900" THEN
          LET lc_mensaje = " FECHA INVALIDA"
          LET ls_estado = 1
       END IF
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION

#==============================================================================#
# Objetivo: devolver el saldo en pesos de las subcuentas involucradas en PMG   #
#         : valuadas las acciones al día de ejecucion                          #
#==============================================================================#
FUNCTION f_saldo_pmg_iss(p_nss,p_fecha)

    DEFINE p_nss                CHAR(11)
    DEFINE p_fecha              DATE         --fecha a la que se realiza calculo
    
    DEFINE ld_mto_pesos_tot     DECIMAL(18,6)
    DEFINE ld_precio_del_dia    DECIMAL(18,6)
    DEFINE v_monto_en_pesos     DECIMAL(18,6)
    DEFINE v_monto_en_acciones  DECIMAL(18,6)
    DEFINE ld_subcuenta         SMALLINT
    DEFINE ls_siefore           SMALLINT

    LET ld_mto_pesos_tot  = 0
   
    WHENEVER ERROR CONTINUE 

    --RECUPERA LOS SALDOS DE LAS SUBCUENTAS DE ISSSTE
    DECLARE cur_sdo_pmg CURSOR FOR
    SELECT subcuenta, siefore, sum(monto_en_acciones)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        IN (30,31,32,33,34,35)
    AND    fecha_conversion <= p_fecha
    GROUP  BY 1,2

    FOREACH cur_sdo_pmg INTO ld_subcuenta, ls_siefore, v_monto_en_acciones
       --IF ld_subcuenta == 35 THEN
       --   --OBTIENE PRECIO DE ACCION PARA VIVIENDA
       --   SELECT precio_del_dia
       --   INTO   ld_precio_del_dia
       --   FROM   glo_valor_accion
       --   WHERE  fecha_valuacion = MDY(MONTH(TODAY),1,YEAR(TODAY))
       --   AND    codigo_siefore  = ls_siefore
       --ELSE
          --OBTIENE EL PRECIO DE LA SIEFORE SELECCIONADA
          SELECT precio_del_dia
          INTO   ld_precio_del_dia
          FROM   glo_valor_accion
          WHERE  fecha_valuacion = p_fecha
          AND    codigo_siefore  = ls_siefore
       --END IF
       
       LET v_monto_en_pesos = v_monto_en_acciones * ld_precio_del_dia
       LET ld_mto_pesos_tot = ld_mto_pesos_tot + v_monto_en_pesos
    END FOREACH

    WHENEVER ERROR STOP

    RETURN ld_mto_pesos_tot

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_muestra_lista(pr_fechas)

    DEFINE pr_fechas          RECORD
           inicio             DATE,
           fin                DATE
           END RECORD
    DEFINE la_lista              ARRAY[100] OF RECORD 
           nss                   LIKE pen_ctr_pago_det_iss.nss,                 
           consecutivo           LIKE pen_ctr_pago_det_iss.consecutivo,        
           sec_contrato          LIKE pen_ctr_pago_det_iss.sec_contrato,       
           fecha_pago_estimada   LIKE pen_ctr_pago_det_iss.fecha_pago_estimada,
           num_mensualidad       LIKE pen_ctr_pago_det_iss.num_mensualidad,    
           mto_pago_pesos        LIKE pen_ctr_pago_det_iss.mto_pago_pesos 
           END RECORD      
    DEFINE ls_pos             SMALLINT 
    
    LET ls_pos = 1
    
    OPEN WINDOW wmuestra AT 6,6 WITH FORM "PENC2032" ATTRIBUTE(BORDER, FORM LINE 1)

        DISPLAY "           LISTA DE SOLICITUDES A INFORMAR EN OPERACION 72            " AT 3,1 ATTRIBUTE(REVERSE)
        --CUENTA MENSUALIDADAES SUBSECUENTES
        DECLARE curmuestra CURSOR FOR 
        SELECT  nss,
                consecutivo,
                sec_contrato,
                fecha_pago_estimada,
                num_mensualidad,
                mto_pago_pesos
        FROM    pen_ctr_pago_det_iss

        WHERE  fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin 
        AND    estado IN (10,20)

        FOREACH curmuestra INTO la_lista[ls_pos].*
           LET ls_pos = ls_pos + 1
        END FOREACH
        
        LET ls_pos = ls_pos -1
        
        IF ls_pos = 0 THEN
           PROMPT "NO HAY INFORMACION A CONSULTAR: " FOR CHAR ENTER
            RETURN
        END IF
             
        --MUSTRA LISTA DE SOLICITUDES A ENNVIAR
        CALL SET_COUNT(ls_pos)
        
        DISPLAY ARRAY la_lista TO sa_lista.* 
                ATTRIBUTES(CURRENT ROW DISPLAY = "REVERSE")
                
        IF INT_FLAG THEN
           LET INT_FLAG = FALSE
        END IF 
        
    CLOSE WINDOW wmuestra

END FUNCTION

#==============================================================================#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla      #
#==============================================================================#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE pc_mensaje    CHAR(77)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje CLIPPED ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#==============================================================================#
# rep_encabezado : Reporte que genera el encabezado del archivo de la op. 72   #
#==============================================================================#
REPORT rep_encabezado()

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "01"                               ,-- Tipo registro
                COLUMN 003, "04"                               ,-- Id de servicio
                COLUMN 005, "01"                               ,-- Entidad origen
                COLUMN 007, gs_codigo_afore USING "&&&"        ,-- Cve entidad origen
                COLUMN 010, "03"                               ,-- Entidad destino
                COLUMN 012, "001"                              ,-- Cve entidad destino
                COLUMN 015, gdt_fec_oper     USING "YYYYMMDD"  ,-- Fecha operacion
                COLUMN 023, gdt_fec_pago_pmg USING "YYYYMMDD"  ,-- Fecha pago de la PMG
                COLUMN 031, 2   SPACES                         ,-- Resultado de la operacion
                COLUMN 033, 3   SPACES                         ,-- Motivo Rechazo 1
                COLUMN 036, 3   SPACES                         ,-- Motivo Rechazo 2
                COLUMN 039, 3   SPACES                         ,-- Motivo Rechazo 3
                COLUMN 042, 232 SPACES                          -- Filler              --INV-4731
END REPORT

#==============================================================================#
# rep_detalle03 : Reporte que genera el detalle del archivo de la op. 43       #
#==============================================================================#
REPORT rep_detalle03(pr_sol_pmg,pr_biometricos)

    DEFINE pr_sol_pmg             RECORD LIKE pen_solicitud_iss.*
--  DEFINE pr_biometricos RECORD LIKE tbl_biometricos_sellos_retiros_total.*    -- SE QUITA LA "S" POR INDICACION DEL AFORE INV-4947
--  DEFINE pr_biometricos RECORD LIKE tbl_biometricos_sellos_retiro_total.*     -- SE QUITA LA "S" POR INDICACION DEL AFORE INV-4947
    DEFINE pr_biometricos         RECORD
           tipo_tramite           SMALLINT   ,
           idsolicitante          INTEGER    ,
           curp_solicitante       CHAR(18)   ,
           curp_agente_servicio   VARCHAR(18),
           sello_unico_trabajador CHAR(14)   ,
           curp_trabajador        CHAR(18)
           END RECORD 
    
    DEFINE lr_nombre      RECORD
           ap_paterno     LIKE afi_mae_afiliado.paterno   ,
           ap_materno     LIKE afi_mae_afiliado.materno   ,
           nombres        LIKE afi_mae_afiliado.nombres
           END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            CALL f_obtiene_nombres(pr_sol_pmg.nss) RETURNING lr_nombre.*
            
            --MLM-4088
            IF pr_sol_pmg.nss MATCHES "I*" THEN
               LET pr_sol_pmg.nss = NULL
            END IF --MLM-4088
            
            PRINT
                COLUMN 001, "03"                                            , --01 Tipo de registro
                COLUMN 003, "04"                                            , --02 ID de servicio
                COLUMN 005, gs_tipo_op USING "&&"                           , --03 ID de operacion
                COLUMN 007, pr_sol_pmg.nss                                  , --04 NSS
                COLUMN 018, pr_sol_pmg.curp                                 , --05 CURP
                COLUMN 036, "0"                                             , --06 ID portabilidad
                COLUMN 037, lr_nombre.nombres                               , --07 Nombre
                COLUMN 077, lr_nombre.ap_paterno                            , --08 Paterno
                COLUMN 117, lr_nombre.ap_materno                            , --09 Materno
                COLUMN 157, pr_sol_pmg.sec_pension USING "&&"               , --10 Sec de Pension
              --COLUMN 159, pr_sol_pmg.tipo_retiro                          , --11 Tipo de Retiro
                COLUMN 159, "G"                                             , --11 Tipo de Retiro  --MLM-4088
                COLUMN 160, pr_sol_pmg.regimen                              , --12 Regimen
                COLUMN 162, pr_sol_pmg.tipo_seguro                          , --13 Tipo de Seguro
                COLUMN 164, pr_sol_pmg.tipo_pension                         , --14 Tipo de Pension
                COLUMN 166, pr_sol_pmg.cve_pension                          , --15 Clave de Pension
                COLUMN 169, pr_sol_pmg.tipo_prestacion  USING "&&"          , --16 Tipo de Prestacion
                COLUMN 171, pr_sol_pmg.fecha_ini_pen    USING "YYYYMMDD"    , --17 Fecha Inicio pension
                COLUMN 179, pr_sol_pmg.fecha_resolucion USING "YYYYMMDD"    , --18 Fecha Resolucion
                COLUMN 187, gdt_fec_pago_pmg            USING "YYYYMMDD"    , --19 Fecha pago de la PMG
              --COLUMN 195, pr_sol_pmg.fecha_captura    USING "YYYYMMDD"    , --20 Fecha Solicitud
                COLUMN 195, HOY                         USING "YYYYMMDD"    , --20 Fecha Solicitud --MLM-4088
                COLUMN 203, 3  SPACES                                       , --21 Diag de registro
                COLUMN 206, 4  SPACES                                       , --22 filler
                COLUMN 210, 2  SPACES                                       , --23 Resultado de la op
                COLUMN 212, 3  SPACES                                       , --24 Mot Rechazo 1
                COLUMN 215, 3  SPACES                                       , --25 Mot Rechazo 2
                COLUMN 218, 3  SPACES                                       , --26 Mot Rechazo 3
              --COLUMN 221, pr_biometricos.tipo_tramite USING "&"           , --27 identificador de pago     --INV-4731
                COLUMN 221, "2"                                             , --27 identificador de pago     --INV-4947
                COLUMN 222, pr_biometricos.idsolicitante     USING "&&"     , --28 idsolicitante             --INV-4731
                COLUMN 224, pr_biometricos.curp_solicitante                 , --29 curp_solicitante          --INV-4731
                COLUMN 242, pr_biometricos.sello_unico_trabajador           , --30 sello_unico_trabajador    --INV-4731
                COLUMN 256, pr_biometricos.curp_agente_servicio               --31 curp_agente_servicio      --INV-4731


END REPORT

#==============================================================================#
# rep_detalle04 : Reporte que genera el det por subcuentas del archivo de      #
#                 la operacion 72                                              #
#==============================================================================#
REPORT rep_detalle04(pr_sol_pmg, pr_montos)

    DEFINE pr_sol_pmg          RECORD LIKE pen_solicitud_iss.*
    DEFINE pr_montos           RECORD LIKE pen_detalle_sol.*

    DEFINE lc_cve_subcta       CHAR(02)

    DEFINE lc_mto_pesos_12     CHAR(12)
    DEFINE lc_mto_pesos_13     CHAR(13)
    DEFINE lc_fecha_aivs       CHAR(08) 

    
    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            --MLM-4088
            IF pr_sol_pmg.nss MATCHES "I*" THEN
               LET pr_sol_pmg.nss = NULL
            END IF --MLM-4088
            
            -- Obtenemos la clave de la subcuenta de acuerdo al layout
            CASE pr_montos.subcuenta
                WHEN 30 LET lc_cve_subcta = "25"
                WHEN 31 LET lc_cve_subcta = "27"
                WHEN 32 LET lc_cve_subcta = "29"
                WHEN 33 LET lc_cve_subcta = "28"
                WHEN 34 LET lc_cve_subcta = "28"
                WHEN 35 LET lc_cve_subcta = "24"
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

            --IF pr_montos.subcuenta = 35 THEN 
            --    LET lc_fecha_aivs = pr_montos.fecha_valuacion USING "YYYYMMDD"
            --ELSE
            --    LET lc_fecha_aivs   = "00010101"
            --END IF


        PRINT
            COLUMN 001, "04"                              , --01 Tipo de registro
            COLUMN 003, "04"                              , --02 ID de servicio
            COLUMN 005, "72"                              , --03 ID de operacion
            COLUMN 007, pr_sol_pmg.nss                    , --04 NSS
            COLUMN 018, pr_sol_pmg.curp                   , --05 CURP
            COLUMN 036, lc_cve_subcta                     , --06 Clave de subcuenta
            COLUMN 038, lc_mto_pesos_12                   , --07 Monto en pesos por subcta
          --COLUMN 050, lc_fecha_aivs                     , --08 Fecha de liquidacion
            COLUMN 050, gdt_fec_pago_pmg USING "YYYYMMDD" , --08 Fecha de liquidacion
            COLUMN 058, 216 SPACES                          --09 Filler         --INV-4731

END REPORT

#==============================================================================#
# rep_sumario : Reporte que genera el sumario del archivo de la op. 72         #
#==============================================================================#
REPORT rep_sumario(pi_num_regs)

    DEFINE pi_num_regs     INTEGER

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, gdt_fec_oper USING "YYYYMMDD"   ,-- Fecha operacion
                COLUMN 023, pi_num_regs  USING "&&&&&&"     ,-- Total registros
                COLUMN 029, 245 SPACES                       -- Filler      --INV-4731
END REPORT

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_ultimo_dia_mes(pd_fecha)

   DEFINE pd_fecha         DATE
   DEFINE ld_fecha_aux     DATE
   
   LET ld_fecha_aux = pd_fecha + 1 units MONTH
   LET ld_fecha_aux = MDY(MONTH(ld_fecha_aux),"01",YEAR(ld_fecha_aux))
   LET ld_fecha_aux = ld_fecha_aux - 1
   
   RETURN ld_fecha_aux
   
END FUNCTION

#==============================================================================#
# Valida suficiencia de fondos basado en retroactivo y proyeccion de 12 meses  #
# Regresa monto retroactivo y monto de primer mensualidad                      #
#==============================================================================#
FUNCTION f_obten_montos_pago(pc_nss, pd_fecha_ini_pen, pd_pago_mensual)
   
   DEFINE pc_nss              CHAR(11)
   DEFINE pd_fecha_ini_pen    DATE
   DEFINE pd_pago_mensual  DECIMAL(10,2)
   
   DEFINE ld_retroactivo      DECIMAL(10,2)
   DEFINE ld_proyeccion       DECIMAL(10,2)
   DEFINE ld_importe_total    DECIMAL(10,2)
   DEFINE ld_saldo_pesos      DECIMAL(10,2)
   DEFINE ld_primer_mens      DECIMAL(10,2)
   
   DEFINE ld_fecha_desde      DATE
   DEFINE ld_fecha_hasta      DATE
   
   LET ld_fecha_desde = pd_fecha_ini_pen
   LET ld_fecha_hasta = HOY - 1 UNITS MONTH
   
   --REALIZA LOS CALCULOS
   LET ld_retroactivo   = f_calcula_retroactivo_iss(ld_fecha_desde,ld_fecha_hasta)
   LET ld_proyeccion    = pd_pago_mensual * 12
   LET ld_importe_total = ld_retroactivo + pd_pago_mensual + ld_proyeccion
   LET ld_saldo_pesos   = f_obten_saldo_iss(pc_nss)
   LET ld_primer_mens   = ld_retroactivo + pd_pago_mensual

   RETURN ld_retroactivo, ld_primer_mens
   
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_calcula_retroactivo_iss(pd_fecha_desde, pd_fecha_hasta)

   DEFINE pd_fecha_desde    DATE
   DEFINE pd_fecha_hasta    DATE

   DEFINE ld_retroactivo    DECIMAL(10,2)
   
   --Variables utilizadas en la funcion
   DEFINE ld_aux_fec_desde  DATE           ; --Fecha inicio busqueda (incremental)
   DEFINE ld_aux_fec_hasta  DATE           ; --Fecha hasta
   DEFINE ls_mes            SMALLINT       ; --Obtiene mes para fecha aux
   DEFINE ls_anio           SMALLINT       ; --Obtiene aqo para fecha aux
   DEFINE ld_importe        DECIMAL (10,2) ; --Recibe el importe de pago

   --GENERANDO LA FECHA DESDE
   LET ls_mes            = MONTH(pd_fecha_desde);
   LET ls_anio           =  YEAR(pd_fecha_desde);
   LET ld_aux_fec_desde  = MDY(ls_mes,1,ls_anio);

   --Generando fecha hasta
   LET ls_mes            = MONTH(pd_fecha_hasta);
   LET ls_anio           =  YEAR(pd_fecha_hasta);
   LET ld_aux_fec_hasta  = MDY(ls_mes,1,ls_anio);

   -----------------------------------------------------------------------------
   -- Cuando el tipo de pago es pago_mensual
   -----------------------------------------------------------------------------

   LET ld_retroactivo  = 0; --Limpia el acumulador

   WHILE ld_aux_fec_desde <= ld_aux_fec_hasta

      LET ld_importe = 0; --Limpia la variable

      SELECT MAX(importe_mensual)
        INTO ld_importe
        FROM tab_pmg_historica_iss
       WHERE fecha_desde <= ld_aux_fec_desde;

      --acumula montos
      LET ld_retroactivo = ld_retroactivo + ld_importe;
      
      --Incrementa fecha aux
      LET ld_aux_fec_desde = ld_aux_fec_desde + 1 units MONTH;
      
   END WHILE;

   RETURN ld_retroactivo;

END FUNCTION

#==============================================================================#
# f_obten_saldo_iss : Verifica si el pago que se debe realizar no            #
#                        sobregira la cuenta individual del nss                #
#==============================================================================#
FUNCTION f_obten_saldo_iss(pc_nss)

    DEFINE pc_nss       LIKE pen_solicitud_iss.nss
    
    DEFINE lr_saldo RECORD
           subcuenta           SMALLINT        ,
           siefore             SMALLINT        ,
           monto_acc           DECIMAL(16,6)   ,
           monto_pesos         DECIMAL(16,6)
           END RECORD

    DEFINE ls_subcta               ,
           ls_gr_saldo             ,
           ls_grupo                SMALLINT

    DEFINE ld_saldo_dia_pesos      DECIMAL(22,6)

    -----------------------------------------------------------------------
    
    LET ls_gr_saldo         = 0
    LET ld_saldo_dia_pesos  = 0
    
    
    LET ls_subcta = 0 
    LET ls_grupo  = 0
    
    --OBTENEMOS EL SALDO DE LA SUBCUENTA SELECCIONADA
    DECLARE cur_saldo_iss CURSOR FOR eje_saldo_dia
    
    FOREACH cur_saldo_iss USING pc_nss       ,
                                ls_subcta    ,
                                ls_gr_saldo  ,
                                HOY
                          INTO  lr_saldo.*

       IF lr_saldo.subcuenta = 30 OR
          lr_saldo.subcuenta = 31 OR
          lr_saldo.subcuenta = 32 OR
          lr_saldo.subcuenta = 33 OR
          lr_saldo.subcuenta = 34 OR
          lr_saldo.subcuenta = 35 THEN

          IF lr_saldo.monto_pesos <= 0 THEN
             CONTINUE FOREACH 
          ELSE 
             LET ld_saldo_dia_pesos  = lr_saldo.monto_pesos + ld_saldo_dia_pesos
          END IF
       
       END IF

    END FOREACH

    RETURN ld_saldo_dia_pesos

END FUNCTION

#==============================================================================#
# Calcula monto a pagar mensual vigente                                        #
#==============================================================================#
FUNCTION f_obten_pago_mensual()

    DEFINE ld_pago_mensual    DECIMAL (10,2)
    DEFINE ls_contador        SMALLINT
    
    LET ld_pago_mensual = NULL 
    
    WHENEVER ERROR CONTINUE
    
       --VALIDA QUE SOLO EXISTA UN MONTO DE PAGO HISTORICO ACTIVO
       SELECT COUNT(*)
       INTO   ls_contador
       FROM   tab_pmg_historica_iss
       WHERE  fecha_hasta IS NULL
       
       IF SQLCA.SQLCODE < 0 THEN
          CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
          call f_error_msg("ERROR AL CONSULTAR MONTO DE PAGO")
          RETURN ld_pago_mensual
       END IF
     
       IF ls_contador = 0  THEN 
          call f_error_msg("NO SE ENCONTRO MONTO PAGO VIGENTE")
          RETURN ld_pago_mensual
       ELSE
          IF ls_contador > 1 THEN 
             call f_error_msg("SE IDENTIFICO MAS DE UN MONTO PAGO ACTIVO")
             RETURN ld_pago_mensual
          END IF
       END IF
       
       --VALIDA QUE SOLO EXISTA UN MONTO DE PAGO HISTORICO ACTIVO
       SELECT importe_mensual
       INTO   ld_pago_mensual
       FROM   tab_pmg_historica_iss
       WHERE  fecha_hasta IS NULL
       
    WHENEVER ERROR STOP

    RETURN ld_pago_mensual
    
END FUNCTION

#==============================================================================#
# fn_actualiza_montos : Actualiza montos comparados entre el Front y SAFRE     #
#==============================================================================#
FUNCTION fn_actualiza_montos(lr_sol_pmg,pd_pago_mensual,pd_pago_retroactivo,pd_pago_primer_mes)
    DEFINE lr_sol_pmg               RECORD LIKE pen_solicitud_iss.*
    DEFINE pd_pago_mensual        DECIMAL(10,2)                                 
    DEFINE pd_pago_retroactivo    LIKE pen_solicitud_iss.pago_retroactivo       
    DEFINE pd_pago_primer_mes     LIKE pen_solicitud_iss.pago_primer_mes


   OPEN WINDOW wvalsu AT 5,12 WITH 17 ROWS, 60 COLUMNS ATTRIBUTE(BORDER)

       DISPLAY "      DETALLE DE MONTOS PARA EL NSS: ",lr_sol_pmg.nss          AT 1,2
      
       DISPLAY " DETALLE DE MONTOS PMG ISSSTE SOLICITUD"                              AT 3,2
       DISPLAY "       PAGO MENSUAL : ",lr_sol_pmg.pago_mensual   USING "$$,$$$,$$&.&&" AT 5,10
       DISPLAY "   PAGO RETROACTIVO : ",lr_sol_pmg.pago_retroactivo    USING "$$,$$$,$$&.&&" AT 6,10
       DISPLAY "PRIMERA MENSUALIDAD : ",lr_sol_pmg.pago_primer_mes   USING "$$,$$$,$$&.&&" AT 7,10

       DISPLAY " DETALLE DE MONTOS PMG ISSSTE CALCULO SAFRE"                              AT 10,2
       DISPLAY "       PAGO MENSUAL : ",pd_pago_mensual   USING "$$,$$$,$$&.&&" AT 12,10
       DISPLAY "   PAGO RETROACTIVO : ",pd_pago_retroactivo    USING "$$,$$$,$$&.&&" AT 13,10
       DISPLAY "PRIMERA MENSUALIDAD : ",pd_pago_primer_mes   USING "$$,$$$,$$&.&&" AT 14,10       
       
        
       WHILE TRUE
           PROMPT "? DESEA ACTUALIZAR LOS MONTOS ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
           IF enter MATCHES "[sSnN]" THEN
               IF enter MATCHES "[sS]" THEN
               
                   #Se actualiza tabla de la solicitud
                    UPDATE pen_solicitud_iss
                    SET    pago_mensual = pd_pago_mensual,
                           pago_retroactivo = pd_pago_retroactivo,
                           pago_primer_mes = pd_pago_primer_mes
                    WHERE nss = lr_sol_pmg.nss
                    AND consecutivo = lr_sol_pmg.consecutivo
                    
                    IF SQLCA.SQLCODE < 0 THEN
                       CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
                       CALL f_error_msg("ERROR AL ACUTUALIZAR SOLICITUD")
                       RETURN 
                    END IF 
                    
                    #Se actualiza tabla de pago
                    UPDATE pen_ctr_pago_det_iss
                    SET    mto_pago_pesos = pd_pago_primer_mes
                    WHERE nss = lr_sol_pmg.nss
                    AND consecutivo = lr_sol_pmg.consecutivo
                    AND num_mensualidad = 1
                    
                    IF SQLCA.SQLCODE < 0 THEN
                       CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
                       CALL f_error_msg("ERROR AL ACUTUALIZAR PAGO")
                       RETURN 
                    END IF
                END IF
                EXIT WHILE
           END IF
       END WHILE
   
   CLOSE WINDOW wvalsu 
    

END FUNCTION

