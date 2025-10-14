################################################################################
#Owner            => E.F.P.                                                    #
#Programa PENC106 => LIQUIDACION DE SOLICITUDES DE RETIROS POR PENSION MINIMA  #
#                    GARANTIZADA                                               #
#Fecha creacion   => 13 DE ABRIL DE 2010                                       #
#By               => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion    => ISAI JIMENEZ ROJAS 23-OCT-13 v1.2                         #
#                 => Se corrige la generacion del nuevo pago con monto vigente #
#Sistema          => PEN                                                       #
#Actualizacion    => cpl-1858 EMMANUEL REYES 07-abr-15 v1.3                    #
#                 => Se modifica algoritmo para NO generar la mensualidad sig  #
#                 => cuando ésta sea multiplo de 12+1                          #
#Actualizacion    => CPL-2167 EMMANUEL REYES                                   #
#                 => Se agregan las condiciones para generar EDC a las cuentas #
#                 => a las que se liquide su ultima mensualidad                #
#Actualizacion    => CPL-2527 ISAI JIMENEZ se modifica mecanica para desmarcar #
#Actualizacion    => CPL-2891 ISAI JIMENEZ se modifica mecanismo para generar  #
#                 => nuevas mensualidades a partir de nutina y no de tabla     #
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        capturado   LIKE pen_estado_pmg.estado_solicitud ,
        pre_liq     LIKE pen_estado_pmg.estado_solicitud ,
        liquidado   LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DEFINE gr_noti RECORD
        agotamiento     LIKE tab_tipo_notifica_pmg.clave        ,
        conclusion      LIKE tab_tipo_notifica_pmg.clave        
    END RECORD

    DEFINE HOY                     DATE
    DEFINE VERSION                 CHAR(7)       --cpl-2527
    DEFINE PROGRAMA                CHAR(20)      --CPL-2891
    DEFINE USUARIO                 CHAR(20)      --CPL-2891
    DEFINE enter                   CHAR(001)
    DEFINE gc_usuario              CHAR(015)
    DEFINE gs_peiss                SMALLINT
    DEFINE gs_flag                 SMALLINT
    DEFINE gs_codigo_afore         SMALLINT
    DEFINE gi_folio_pre            INTEGER
    DEFINE gc_mensaje              CHAR(200)
    
END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    DEFER INTERRUPT
    
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    --cambiar version en cada liberacion
    LET VERSION = "v1.2527"   --CPL-2527
    LET VERSION = "v1.2891"   --CPL-2891   
    
    LET PROGRAMA = ARG_VAL(0)
    LET USUARIO  = FGL_GETENV("USER")
    
    CALL STARTLOG(USUARIO CLIPPED||".PENC106.log")

    LET gc_mensaje = "======================================================\n",
                     "SE EJECUTA LIQUIDACION POR "||FGL_GETENV("USER")||" VERSION: "||VERSION,"\n",
                     "======================================================"
    CALL ERRORLOG(gc_mensaje CLIPPED)

    CALL init() #i

    CALL f_despliega_info() RETURNING gs_flag, gi_folio_pre
    
    IF gs_flag THEN

        CALL f_abre_ventana()
        
        CALL primer_paso(gi_folio_pre)       #-- Realiza la Liquidacion de los montos
        CALL segundo_paso(gi_folio_pre, HOY) #-- Determina la siguiente mensualidad o si se concluye el pago
        

        PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
        CLOSE WINDOW PENC1062

    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY = TODAY
    
    IF FGL_GETENV("DEBUG") = "2" THEN
       PROMPT "INGRESA LA FECHA DE LIQUIDACION mm/dd/yyyy:" FOR HOY
    END IF 

    ----- CODIGOS AFORES -----
    SELECT codigo_afore     ,
           USER
    INTO   gs_codigo_afore  ,
           gc_usuario
    FROM   tab_afore_local

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.pre_liq
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- TIPO DE NOTIFICACION -----
    SELECT clave
    INTO   gr_noti.agotamiento
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*AGOTAMIENTO*'
    
    SELECT clave
    INTO   gr_noti.conclusion
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*CONCLUSION*'

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- AGREGA UN MES A LA FECHA DADA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_agrega_mes(?,?) "
    PREPARE eje_agrega_mes FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    liquidacion de PMG                                     #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE
        ls_flag             ,
        ls_estado           ,
        ls_procesa          SMALLINT

    DEFINE lc_mensaje     CHAR(100)

    DEFINE lr_dat RECORD
        folio_preliquida    LIKE pen_preliquida_pmg.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    OPEN WINDOW penc1061 AT 2,2 WITH FORM "PENC1061" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                            < ESC > Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 2,1  
    DISPLAY " PENC106      LIQUIDACION DE SOLICITUDES DE RETIROS PMG                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY VERSION AT 2,71   --cpl-2527


    LET ls_procesa  = 0

    SELECT MAX(folio)
    INTO   lr_dat.folio_preliquida
    FROM   pen_preliquida_pmg
    WHERE  estado_pmg = gr_edo.pre_liq

    INPUT BY NAME lr_dat.* WITHOUT DEFAULTS

        AFTER FIELD folio_preliquida
            
            CALL f_valida_folio(lr_dat.folio_preliquida)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_preliquida
            END IF

        ON KEY (ESC)
            -- Validaciones de folio
            CALL f_valida_folio(lr_dat.folio_preliquida)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_preliquida
            END IF

            WHILE TRUE
                PROMPT "¿ DESEA EJECUTAR LA LIQUIDACION DE RETIROS PMG ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa  = 1
                    ELSE
                        LET ls_procesa  = 0
                    END IF
                END IF

                EXIT INPUT

            END WHILE
        
        ON KEY (CONTROL-C, INTERRUPT)
            PROMPT " PROCESO CANCELADO, PRESIONE <ENTER>: " FOR CHAR ENTER
            EXIT INPUT

        ON KEY (CONTROL-W)
            CALL f_acerca_de()
            
    END INPUT

    RETURN ls_procesa, lr_dat.folio_preliquida

END FUNCTION


#---------------------------------------------------------------------------#
# primer_paso : Ejecuta el proceso de liquidacion de PMG del folio dado     #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE 
        pi_folio        INTEGER 

    DEFINE lr_preliquida RECORD LIKE dis_cuenta.*

    -- -----------------------------------------------------------------------------

    DISPLAY "LIQUIDANDO RETIROS ..." AT 16,5

    DECLARE cur_preliq CURSOR FOR
    SELECT *
    FROM   pen_preliquida_pmg
    WHERE  folio        = pi_folio
    AND    estado_pmg   = gr_edo.pre_liq

    FOREACH cur_preliq INTO lr_preliquida.*
        IF NOT(lr_preliquida.monto_en_pesos = 0 AND lr_preliquida.monto_en_acciones = 0) THEN

            INSERT INTO dis_cuenta
            VALUES(lr_preliquida.*)
        
            UPDATE pen_preliquida_pmg
            SET    estado_pmg       = gr_edo.liquidado
            WHERE  nss              = lr_preliquida.nss
            AND    consecutivo_lote = lr_preliquida.consecutivo_lote
            AND    subcuenta        = lr_preliquida.subcuenta
            AND    folio            = pi_folio
            AND    estado_pmg       = gr_edo.pre_liq

        END IF
    END FOREACH
    
END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Verifica todos los nss del folio liquidado para determinar #
#                si se inserta una nueva mensualidad o si se termino de     #
#                pagar para realizar la desmarca                            #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio, pdt_fecha_liquida)

    DEFINE 
        pi_folio            INTEGER 

    DEFINE
        pdt_fecha_liquida   DATE
        
    DEFINE lr_ctr_pago_det RECORD LIKE pen_ctr_pago_det.*

    DEFINE lr_pagar RECORD
        nss             LIKE pen_preliquida_pmg.nss                 , 
        consecutivo     LIKE pen_preliquida_pmg.consecutivo_lote    ,
        mensualidad     LIKE pen_preliquida_pmg.mensualidad         ,
        sec_pension     LIKE pen_solicitud_pmg.sec_pension          ,
        edo_sub_viv     LIKE pen_solicitud_pmg.estado_sub_viv
    END RECORD
    
    DEFINE
        ld_pago_mensual         DECIMAL(16,2)   ,       --MLM-1936 antes 10,2
        ld_monto_pago           DECIMAL(16,6)   ,
        ld_saldo_dia            DECIMAL(16,6)
    DEFINE li_cod_tramite_pmg   INTEGER       --CPL-2017
    DEFINE ls_bandera_edocta    SMALLINT      --CPL-2017
    DEFINE lc_regimen           CHAR(2)       --CPL-2017
    DEFINE ls_status_edocta     SMALLINT      --CPL-2017
    DEFINE ls_edc_liq           SMALLINT      --CPL-2167
    DEFINE ld_fecha_ini_pen     LIKE pen_solicitud_pmg.fecha_ini_pen
    -- -----------------------------------------------------------------------------

    --CPL-2017
    LET li_cod_tramite_pmg = 15   --PMG --CPL-2017
    LET ls_bandera_edocta  = 0    --PMG --CPL-2017
    LET lc_regimen         = NULL --PMG --CPL-2017
    LET ls_edc_liq         = 0    --PMG --CPL-2167

    WHENEVER ERROR CONTINUE                                                                            --CPL-2017
    PREPARE exe_inserta_edocta FROM "EXECUTE FUNCTION fn_inserta_edo_cta_pen(?,?,?,?,?,?)"                              --CPL-2017
    IF SQLCA.SQLCODE < 0 THEN                                                                          --CPL-2017
       PROMPT "ERROR AL PREPARAR INSERCION A ESTADO DE CUENTA, NOTIFIQUE A SISTEMAS:" FOR CHAR ENTER   --CPL-2017
       CALL ERRORLOG("ERROR AL PREPARAR INSERCION A ESTADO DE CUENTA")                                 --CPL-2017        
       CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
       LET ls_bandera_edocta = 1                                                                       --CPL-2017
    END IF                                                                                             --CPL-2017
    WHENEVER ERROR STOP                                                                                --CPL-2017


    LET ld_monto_pago   = 0

    DECLARE cur_pagos CURSOR FOR
    SELECT UNIQUE(A.nss)        ,
           A.consecutivo_lote   ,
           A.mensualidad        ,
           B.sec_pension        ,
           B.estado_sub_viv
    FROM   pen_preliquida_pmg A ,
           pen_solicitud_pmg  B
    WHERE  A.folio              = pi_folio
    AND    A.estado_pmg         = gr_edo.liquidado
    AND    A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    ORDER BY 1

    FOREACH cur_pagos INTO lr_pagar.*

        -- obtenemos el saldo del trabajador despues de la liquidacion
        LET ld_saldo_dia = f_saldo_dia(lr_pagar.nss, lr_pagar.edo_sub_viv, pdt_fecha_liquida)
        
        -- Si el saldo es cero se desmarca la cuenta, se actualiza a liquidado y se envia a op70
        IF ld_saldo_dia = 0 THEN

            CALL f_desmarca_cuenta(lr_pagar.nss         ,
                                   lr_pagar.consecutivo ,
                                   "S"
                                  )

            CALL f_agrega_det_op70(lr_pagar.nss         ,
                                   lr_pagar.consecutivo ,
                                   gr_noti.conclusion       
                                  )
            
            UPDATE pen_solicitud_pmg
            SET    estado_solicitud = gr_edo.liquidado
            WHERE  nss              = lr_pagar.nss
            AND    consecutivo      = lr_pagar.consecutivo

            UPDATE pen_ctr_pago
            SET    estado           = gr_edo.liquidado
            WHERE  nss              = lr_pagar.nss
            AND    consecutivo      = lr_pagar.consecutivo

            --Se solicita EDC 
            LET ls_edc_liq = 1

        ELSE
            ---SELECT pago_mensual_pesos                      --CPL-2891
            ---INTO   ld_pago_mensual                         --CPL-2891
            ---FROM   pen_ctr_pago                            --CPL-2891
            ---WHERE  nss          = lr_pagar.nss             --CPL-2891
            ---AND    consecutivo  = lr_pagar.consecutivo     --CPL-2891

            --RECUPERA FECHA DE INICIO DE PENSION --CPL-3279
            SELECT fecha_ini_pen
            INTO   ld_fecha_ini_pen
            FROM   pen_solicitud_pmg
            WHERE  nss          = lr_pagar.nss
            AND    consecutivo  = lr_pagar.consecutivo

            --LET ld_pago_mensual =  f_importe_mensual_pmg(lr_pagar.nss) --CPL-2891
            LET ld_pago_mensual = f_importe_mensual_pmg(lr_pagar.nss, lr_pagar.consecutivo, ld_fecha_ini_pen) --CPL-3279

            --RECUPERA LA SOLICITUD ACTUAL
            SELECT *
            INTO   lr_ctr_pago_det.*
            FROM   pen_ctr_pago_det
            WHERE  nss              = lr_pagar.nss
            AND    consecutivo      = lr_pagar.consecutivo
            AND    folio_liquida    = pi_folio
            AND    num_mensualidad  = lr_pagar.mensualidad


            -- si el saldo es dif de cero se valida si alcanza a pagar una mensualidad completa
            IF ld_saldo_dia < ld_pago_mensual THEN
                LET ld_monto_pago = ld_saldo_dia
            ELSE
                -- si alcanza a pagar una mensualidad se valida si el restante alcanza para otra                
                IF ld_saldo_dia < (ld_pago_mensual * 2) THEN
                    -- si no alcanza, se inserta como pago la mensualidad mas el remanente
                    LET ld_monto_pago = ld_saldo_dia
                ELSE
                    -- en otro caso, se inserta como pago la mensualidad normal
                    --LET ld_monto_pago = f_obten_pago_mensual_actual(ld_pago_mensual)
                    LET ld_monto_pago = ld_pago_mensual
                END IF
            END IF -- saldo menor a pago mensual

            
            CALL f_inserta_pago_sig(lr_ctr_pago_det.*, ld_monto_pago)

        END IF  -- saldo cero
        
        ----------------------------------------------------
        -- REGISTRO DE SOLICITUD DE ESTADO DE CUENTA 
        -- CPL-2017
        ----------------------------------------------------
        IF lr_pagar.mensualidad = 1 OR ( (lr_pagar.mensualidad MOD 12)= 0 ) 
                                                          OR ls_edc_liq = 1 THEN--CPL-2167

           WHENEVER ERROR CONTINUE
           EXECUTE exe_inserta_edocta USING lr_pagar.nss        ,     --pc_nss           
                                            lc_regimen          ,     --pc_regimen       
                                            li_cod_tramite_pmg  ,     --pi_cod_tramite   
                                            HOY                 ,     --pdt_fecha_liquida
                                            pi_folio            ,     --pi_folio_liquida 
                                            lr_pagar.consecutivo      --pd_consec_liquida
                                      INTO  ls_status_edocta
           IF SQLCA.SQLCODE < 0 THEN
              LET ls_bandera_edocta = 1
              CALL ERRORLOG("ERROR AL INSERTAR SOLICITUD DE EDO CTA NSS "||lr_pagar.nss)
              CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
           END IF
           WHENEVER ERROR STOP
           
           LET ls_edc_liq = 0 --CPL-2167 Se limpia la variable

        END IF 
        
        
    END FOREACH
    
    --CPL-2017
    IF ls_bandera_edocta = 1 THEN 
       PROMPT "ERROR AL INSERTAR SOLICITUD DE ESTADO DE CUENTA, NOTIFIQUE A SISTEMAS:" FOR CHAR ENTER
    END IF 

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
        AND    (salario_min_inf <= TRUNC(ld_sal_umas,2) AND salario_min_sup >= TRUNC(ld_sal_umas,2))
        AND    fecha_fin_vigencia IS NULL

   END IF 

   RETURN ld_monto_pago
   
END FUNCTION 


#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  liquidacion de PMG                                       #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW PENC1062 AT 4,4 WITH FORM "PENC1062" ATTRIBUTE(BORDER)
    DISPLAY "                                                 PENSION MINIMA GARANTIZADA " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC106         LIQUIDACION DE SOLICITUDES DE RETIROS PMG                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca        RECORD
           nss                LIKE pen_solicitud_pmg.nss          ,
           consec             LIKE pen_solicitud_pmg.consecutivo  ,
           tipo_retiro        LIKE pen_solicitud_pmg.tipo_retiro
           END RECORD
    DEFINE ls_primer_consec   LIKE pen_solicitud_pmg.consecutivo
    DEFINE lr_dat RECORD
           edo_marca          SMALLINT ,
           marca_causa        SMALLINT
           END RECORD

    DEFINE ls_movim           SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    SELECT movimiento
    INTO   ls_movim
    FROM   tab_retiro
    WHERE  tipo_retiro = pr_desmarca.tipo_retiro
    
    --CPL-2527 busca el correlativo de la marca original 
    SELECT sol.consecutivo
    INTO   ls_primer_consec
    FROM   pen_solicitud_pmg sol,
           cta_act_marca     mar
    WHERE  sol.nss         = mar.nss
    AND    sol.consecutivo = mar.correlativo
    AND    sol.nss         = pr_desmarca.nss
    AND    mar.marca_cod   = 841;

    --SELECT MIN(consecutivo)
    --INTO   ls_primer_consec
    --FROM   pen_solicitud_pmg
    --WHERE  nss = pr_desmarca.nss    

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               ls_movim             ,--marca entrante
                             --pr_desmarca.consec   ,--consecutivo    --CPL-2527
                               ls_primer_consec     ,--consecutivo    --CPL-2527
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_agrega_det_op70 : Inserta un registro en la tabla de detalle de la op70 #
#                     de acuerdo al tipo de notificacion                    #
#---------------------------------------------------------------------------#
FUNCTION f_agrega_det_op70(pr_datos)

    DEFINE pr_datos RECORD
        nss          LIKE pen_solicitud_pmg.nss         ,
        consecutivo  LIKE pen_solicitud_pmg.consecutivo ,
        id_notifica  LIKE tab_tipo_notifica_pmg.clave  
    END RECORD 

    DEFINE lr_preliq RECORD
        subcuenta           SMALLINT        ,
        monto_pesos         DECIMAL(22,6)
    END RECORD
    
    DEFINE lr_dat_saldo RECORD
        subcuenta           SMALLINT    ,
        grupo               SMALLINT
    END RECORD 

    DEFINE lr_saldo_dia RECORD
        subcuenta           SMALLINT        ,
        siefore             SMALLINT        ,
        monto_acc           DECIMAL(16,6)   ,
        monto_pesos         DECIMAL(16,6)
    END RECORD
    
    DEFINE lr_op70 RECORD LIKE pen_detalle_op70.*
        
    -- --------------------------------------------------------------------
    LET lr_dat_saldo.subcuenta      = 0
    LET lr_dat_saldo.grupo          = 0

    LET lr_op70.folio_envio         = 0
    LET lr_op70.folio_datamart      = 0
    LET lr_op70.id_tipo_notifica    = pr_datos.id_notifica
    LET lr_op70.nss                 = pr_datos.nss
    LET lr_op70.cve_pension         = NULL
    LET lr_op70.diag_operacion      = NULL
    LET lr_op70.codigo_rechazo      = 0
    LET lr_op70.fecha_fallecimiento = NULL
 
    LET lr_op70.origen_informacion  = 0
    LET lr_op70.fecha_carga         = HOY
    LET lr_op70.hora_carga          = CURRENT HOUR TO SECOND
    LET lr_op70.usuario             = gc_usuario
    LET lr_op70.estado              = gr_edo.capturado
    LET lr_op70.num_men_calculadas  = 0 


    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
        LET lr_op70.fecha_agotamiento = HOY 
    ELSE
        LET lr_op70.fecha_agotamiento = NULL
    END IF

    SELECT curp             ,
           sec_pension      ,
           tipo_retiro      ,
           regimen          ,
           tipo_seguro      ,
           tipo_pension     ,
           "   "            ,
           tipo_prestacion  ,
           fecha_ini_pen
    INTO   lr_op70.curp             ,
           lr_op70.sec_pension      ,
           lr_op70.tipo_retiro      ,
           lr_op70.regimen          ,
           lr_op70.tipo_seguro      ,
           lr_op70.tipo_pension     ,
           lr_op70.cve_pension      ,
           lr_op70.tipo_prestacion  ,
           lr_op70.fecha_ini_pen
    FROM   pen_solicitud_pmg
    WHERE  nss         = pr_datos.nss         
    AND    consecutivo = pr_datos.consecutivo 
    
    SELECT fecha_liquida
    INTO   lr_op70.fecha_primer_pago     
    FROM   pen_ctr_pago_det
    WHERE  nss              = pr_datos.nss         
    AND    consecutivo      = pr_datos.consecutivo 
    AND    num_mensualidad  = 1

    SELECT MAX(num_mensualidad)
    INTO   lr_op70.num_men_pagadas
    FROM   pen_ctr_pago_det
    WHERE  nss          = pr_datos.nss         
    AND    consecutivo  = pr_datos.consecutivo 
    AND    estado       = gr_edo.liquidado
    
    SELECT fecha_liquida
    INTO   lr_op70.fecha_ultimo_pago  
    FROM   pen_ctr_pago_det
    WHERE  nss              = pr_datos.nss         
    AND    consecutivo      = pr_datos.consecutivo 
    AND    estado           = gr_edo.liquidado
    AND    num_mensualidad  = lr_op70.num_men_pagadas
    
    SELECT SUM(mto_pago_pesos)
    INTO   lr_op70.mto_total_pmg
    FROM   pen_ctr_pago_det
    WHERE  nss          = pr_datos.nss         
    AND    consecutivo  = pr_datos.consecutivo 
    AND    estado       = gr_edo.liquidado

    -- Montos de la ultima mensualidad pagada
    LET lr_op70.mto_retiro97    = 0
    LET lr_op70.mto_cv          = 0
    LET lr_op70.mto_cs          = 0
    LET lr_op70.mto_viv97       = 0

    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
        DECLARE cur_op70pre CURSOR FOR
        SELECT subcuenta            ,
               monto_en_pesos * -1
        FROM   pen_preliquida_pmg
        WHERE  nss              = pr_datos.nss         
        AND    consecutivo_lote = pr_datos.consecutivo 
        AND    mensualidad      = lr_op70.num_men_pagadas
        
        FOREACH cur_op70pre INTO lr_preliq.*
        
            CASE lr_preliq.subcuenta
                WHEN 1
                    LET lr_op70.mto_retiro97 = lr_op70.mto_retiro97 + lr_preliq.monto_pesos
                WHEN 2                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 6                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 9                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 5                       
                    LET lr_op70.mto_cs       = lr_op70.mto_cs + lr_preliq.monto_pesos
                WHEN 4                       
                    LET lr_op70.mto_viv97    = lr_op70.mto_viv97 + lr_preliq.monto_pesos
            END CASE
        
        END FOREACH
    END IF

    -- Saldo de la subcuenta
    LET lr_op70.saldo_retiro97  = 0
    LET lr_op70.saldo_cv        = 0
    LET lr_op70.saldo_cs        = 0
    LET lr_op70.saldo_viv97     = 0
    
    DECLARE cur_saldo_op70 CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo_op70 USING pr_datos.nss            ,
                                 lr_dat_saldo.subcuenta  ,
                                 lr_dat_saldo.grupo      ,
                                 HOY                      
                           INTO  lr_saldo_dia.*

        CASE lr_saldo_dia.subcuenta
            WHEN 1
                LET lr_op70.saldo_retiro97  = lr_op70.saldo_retiro97 + lr_preliq.monto_pesos
            WHEN 2                       
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 6                          
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 9                          
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 5                          
                LET lr_op70.saldo_cs        = lr_op70.saldo_cs + lr_preliq.monto_pesos
            WHEN 4                          
                LET lr_op70.saldo_viv97     = lr_op70.saldo_viv97 + lr_preliq.monto_pesos
        END CASE
       
    END FOREACH


    INSERT INTO pen_detalle_op70
    VALUES (lr_op70.*)

END FUNCTION 

#---------------------------------------------------------------------------#
# f_inserta_pago_sig : Inserta el registro de la siguiente mensualidad en   #
#                       pen_ctr_pago_det en caso de no existir              #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_pago_sig(pr_ctr_pago_det, pd_pago_mensual)

    DEFINE pr_ctr_pago_det RECORD   LIKE pen_ctr_pago_det.*

    DEFINE pd_pago_mensual          DECIMAL(16,6)
    DEFINE lr_pago_det_nuevo RECORD LIKE pen_ctr_pago_det.*

    DEFINE ls_tot_mens              SMALLINT 
    DEFINE ls_mes                   SMALLINT
    DEFINE ls_mod                   SMALLINT   --cpl-1858
    DEFINE lc_mensaje               CHAR(500)
    DEFINE ld_fecha_aux             DATE       --CPL-2891
    DEFINE ls_mes_proceso           SMALLINT 

    -- -----------------------------------------------------------------------------
    
    LET ls_mes      = 1
    LET ls_tot_mens = 0
    LET ls_mes_proceso = MONTH(TODAY)
    
    INITIALIZE lr_pago_det_nuevo.* TO NULL
    -- cpl-1858 inicio
    
    --LAS SIGUIENTES LINEAS SON ALTERNATIVAS A LOS PROBLEMAS QUE SE PUEDAN
    --PRESENTAR CON EL CIERRE DE MENSUALIDADES PARA RENOVACION DE CONTRATO
    --LET ls_mod = pr_ctr_pago_det.sec_contrato * 12
    --IF pr_ctr_pago_det.num_mensualidad >= ls_mod  THEN
    
    LET ls_mod = pr_ctr_pago_det.num_mensualidad MOD 12

    -- A partir del 1 julio de 2021 se vuelven todos los contratos multianuales, ya no se deben renovar cada año
    -- CPL-3357

--    IF ls_mes_proceso = 1 THEN  -- CPL-3125 Se ajusta para que en enero se supenda la mensualidad de febrero

    --IF ls_mod = 0 THEN --CPL-3125

       --No generar nueva mensualidad
--       WHENEVER ERROR CONTINUE
--          UPDATE pen_solicitud_pmg
--             SET estado_solicitud = 70
--           WHERE nss          = pr_ctr_pago_det.nss
--             AND consecutivo  = pr_ctr_pago_det.consecutivo
--             AND sec_contrato = pr_ctr_pago_det.sec_contrato
             
--             IF SQLCA.sqlcode < 0 THEN
--                LET lc_mensaje = "ERROR AL ACTUALIZAR ESTADO DE SOLICITUD PARA NSS: ",
--                                 pr_ctr_pago_det.nss," CONSECUTIVO: ",
--                                 pr_ctr_pago_det.consecutivo," Y SEC_CONTRATO: ",
--                                 pr_ctr_pago_det.sec_contrato

--                LET lc_mensaje = lc_mensaje CLIPPED
                
--                CALL ERRORLOG(lc_mensaje)
--             END IF
             
--       WHENEVER ERROR STOP
--    ELSE
       SELECT "OK"
       FROM   pen_ctr_pago_det
       WHERE  nss              = pr_ctr_pago_det.nss
       AND    consecutivo      = pr_ctr_pago_det.consecutivo    
       AND    num_mensualidad  = pr_ctr_pago_det.num_mensualidad + ls_mes
       GROUP BY 1
       
       IF STATUS = NOTFOUND THEN 
       
           LET lr_pago_det_nuevo.nss               = pr_ctr_pago_det.nss            
           LET lr_pago_det_nuevo.consecutivo       = pr_ctr_pago_det.consecutivo    
           LET lr_pago_det_nuevo.sec_contrato      = pr_ctr_pago_det.sec_contrato
           LET lr_pago_det_nuevo.num_mensualidad   = pr_ctr_pago_det.num_mensualidad + ls_mes
           LET lr_pago_det_nuevo.mto_pago_pesos    = pd_pago_mensual
           LET lr_pago_det_nuevo.estado            = gr_edo.capturado        
       
         --EXECUTE eje_agrega_mes USING pr_ctr_pago_det.fecha_pago_estimada ,   --CPL-2891
           
           LET ld_fecha_aux = MDY(MONTH(HOY),1,YEAR(HOY))   --CPL-3591
           
           EXECUTE eje_agrega_mes USING ld_fecha_aux,                            --CPL-2891
                                        ls_mes 
                                  INTO  lr_pago_det_nuevo.fecha_pago_estimada
           
           INSERT INTO pen_ctr_pago_det
           VALUES (lr_pago_det_nuevo.*)
       ELSE
           -- si existe el registro, se actualiza el monto a pagar por el obtenido
           UPDATE pen_ctr_pago_det
           SET    mto_pago_pesos   = pd_pago_mensual
           WHERE  nss              = pr_ctr_pago_det.nss
           AND    consecutivo      = pr_ctr_pago_det.consecutivo    
           AND    num_mensualidad  = pr_ctr_pago_det.num_mensualidad + ls_mes        
       END IF
--    END IF 
    --cpl-1858

END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_folio : Valida que el folio capturada cumpla las condiciones     #
#                  necesarias para ser aceptado                             #
#---------------------------------------------------------------------------#
FUNCTION f_valida_folio(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE
        ls_edo_pmg          ,
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado   = 0
    LET ls_edo_pmg  = 0

    IF pi_folio IS NULL THEN
        LET lc_mensaje = " EL FOLIO NO DEBE SER NULO"
        LET ls_estado = 1
    ELSE
        SELECT MAX(estado_pmg)
        INTO   ls_edo_pmg
        FROM   pen_preliquida_pmg
        WHERE  folio = pi_folio
                
        IF STATUS = NOTFOUND THEN
            LET lc_mensaje = " EL FOLIO INGRESADO NO CORRESPONDE A UNA PRELIQUIDACION"
            LET ls_estado = 1
        ELSE
            IF ls_edo_pmg = gr_edo.liquidado THEN
                LET lc_mensaje = " EL FOLIO INGRESADO YA FUE LIQUIDADO"
                LET ls_estado = 1                
            END IF 
        END IF
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_saldo_dia : obtiene el saldo al dia de las subcuentas relacionadas      #
#---------------------------------------------------------------------------#
FUNCTION f_saldo_dia(pr_datos_trab)

    DEFINE pr_datos_trab RECORD
        nss             LIKE pen_preliquida_pmg.nss             , 
        edo_sub_viv     LIKE pen_solicitud_pmg.estado_sub_viv   ,
        fec_liquida     DATE
    END RECORD

    DEFINE lr_saldo RECORD
        subcta      SMALLINT        ,
        sie         SMALLINT        ,
        monto_acc   DECIMAL(16,6)   ,
        monto_pes   DECIMAL(16,6)
    END RECORD
    
    DEFINE
        ls_subcta           ,
        ls_grupo            SMALLINT

    DEFINE
        ld_saldo_dia        DECIMAL(16,6)

    -- -----------------------------------------------------------------------------

    LET ls_subcta       = 0
    LET ls_grupo        = 0
    LET ld_saldo_dia    = 0

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo USING pr_datos_trab.nss           ,
                            ls_subcta                   ,
                            ls_grupo                    ,
                            pr_datos_trab.fec_liquida
                      INTO  lr_saldo.*

        IF lr_saldo.monto_pes <= 0 THEN
            LET lr_saldo.monto_pes = 0
        END IF
        
        IF (lr_saldo.subcta = 1) OR (lr_saldo.subcta = 2) OR (lr_saldo.subcta = 5) OR 
           (lr_saldo.subcta = 6) OR (lr_saldo.subcta = 9) THEN 
            LET ld_saldo_dia = ld_saldo_dia + lr_saldo.monto_pes
        ELSE
            IF (lr_saldo.subcta = 4 AND pr_datos_trab.edo_sub_viv = 1) THEN
                LET ld_saldo_dia = ld_saldo_dia + lr_saldo.monto_pes
            END IF
        END IF
    
    END FOREACH 

    RETURN ld_saldo_dia

END FUNCTION

#==============================================================================#
# CALCULA EL PAGO ACTUAL QUE CORRESPONDE CON EL PAGO REGISTRADO                #
#==============================================================================#
FUNCTION f_obten_pago_mensual_actual(pd_mto_capturado)
 
    DEFINE pd_mto_capturado        DECIMAL(10,6)
    DEFINE ls_tipo_pago            SMALLINT
    DEFINE ld_imp_mensual          DECIMAL(16,6)

    ---------------------------------------------------

    --OBTIENE EL TIPO DE MONTO AL QUE CORRESPONDE EL MONTO CAPTURADO
    SELECT 1
    INTO   ls_tipo_pago
    FROM   tab_pmg_historica
    WHERE  pd_mto_capturado BETWEEN (importe_mensual-1) AND (importe_mensual+1)
    UNION
    SELECT 2
    FROM   tab_pmg_historica
    WHERE  pd_mto_capturado BETWEEN (importe_mensual_11p -1) AND
            (importe_mensual_11p +1)
    
    IF ls_tipo_pago = 1 THEN
       SELECT importe_mensual
       INTO   ld_imp_mensual
       FROM   tab_pmg_historica
       WHERE  fecha_hasta IS NULL
    ELSE
       SELECT importe_mensual_11p
       INTO   ld_imp_mensual
       FROM   tab_pmg_historica
       WHERE  fecha_hasta IS NULL
    END IF
    
    RETURN ld_imp_mensual
    
END FUNCTION

#===========================================================================#
#                                                                           #
#===========================================================================#
FUNCTION f_acerca_de()

    DEFINE lc_fecha_hora CHAR(25)
   
    LET lc_fecha_hora = "25/10/2018 04:56:31 p.m."   --obtenido con F7 en ultraedit
    
    OPEN WINDOW w_acerca AT 8,10 WITH 10 rows, 60 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST -1)
    
       DISPLAY "PROGRAMA    : ", PROGRAMA           AT 2,2
       DISPLAY "VERSION     : ", VERSION            AT 3,2
       DISPLAY "Edicion     : ", lc_fecha_hora      AT 4,2
       DISPLAY "Ult cambio  : ", "Correcta validacion de suficiencia para pago" 
                                                    AT 5,2
       DISPLAY "            : ", "" 
                                                    AT 6,2
       
       PROMPT "PRESIONA <ENTER> PARA CONTINUAR: " FOR CHAR enter

    CLOSE WINDOW w_acerca

END FUNCTION 



