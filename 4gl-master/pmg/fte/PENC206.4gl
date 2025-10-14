################################################################################
#Owner            => E.F.P.                                                    #
#Programa PENC106 => LIQUIDACION DE SOLICITUDES DE RETIROS POR PENSION MINIMA  #
#                    GARANTIZADA                                               #
#Fecha creacion   => 13 DE ABRIL DE 2010                                       #
#By               => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion    => ISAI JIMENEZ ROJAS 23-OCT-13 v1.2                         #
#                 => Se corrige la generacion del nuevo pago con monto vigente #
#Actualizacion    => ISAI JIMENEZ ROJAS 14-ENE-2014    v1.2                    #
#                 => Se corrige la generacion del nuevo pago con monto vigente #
#Actualizacion    => ISAI JIMENEZ ROJAS 07-AGO-2018    v1.3 INV-5003           #
#                 => se corrige para asegurar que no se generen nuevas mens    #
#                 => al finalizar contrato (mensualidad 12 o multiplo de 12)   #
#Sistema          => PEN                                                       #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo2 RECORD
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

    DEFINE HOY2                    DATE
    DEFINE enter2                  CHAR(001)
    DEFINE gc_usuario2             CHAR(015)
    DEFINE gs_peiss                SMALLINT
    DEFINE gs_flag                 SMALLINT
    DEFINE gs_codigo_afore         SMALLINT
    DEFINE g_mensaje               CHAR(80)
    DEFINE ENTER                   CHAR(1)
END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_liquida_pmg_iss(pi_folio_pre,pi_folio_op72)

   DEFINE pi_folio_pre       INTEGER 
   DEFINE pi_folio_op72      INTEGER    
   
    CALL init2()

    --REALIZA LA LIQUIDACION
    CALL f_inserta_liquidacion(pi_folio_pre)

    --ACTUALIZA MENSUALIDADES
    CALL f_actualiza_liquidacion(pi_folio_pre, HOY2) 
        
    # PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter


END FUNCTION    
#==============================================================================#
# init : Inicializa las variables globales que se usaran en el programa        #
#==============================================================================#
FUNCTION init2()

    DEFINE
        lc_prepare      CHAR(300)

    ----------------------------------------------------------------------------

    LET HOY2 = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore     ,
           USER
    INTO   gs_codigo_afore  ,
           gc_usuario2
    FROM   tab_afore_local

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo2.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo2.pre_liq
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo2.liquidado
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

    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_edo_cta_pen(?,?,?,?,?,?)"
    PREPARE exe_inserta_edocta FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#==============================================================================#
# f_inserta_liquidacion : Ejecuta el proceso de liquidacion de PMG del folio   #
#==============================================================================#
FUNCTION f_inserta_liquidacion(pi_folio)

    DEFINE pi_folio        INTEGER 
    DEFINE lr_preliquida   RECORD LIKE dis_cuenta.*

    ----------------------------------------------------------------------------

    MESSAGE "LIQUIDANDO RETIROS ..." SLEEP 1

    --SELECCIONA LOS MOVIMIENTOS DE PRELIQUIDACION 
    DECLARE cur_preliq CURSOR FOR
    SELECT *
    FROM   pen_preliquida_pmg
    WHERE  folio        = pi_folio
    AND    estado_pmg   = 60 --gr_edo2.pre_liq

    FOREACH cur_preliq INTO lr_preliquida.*
      
        --APLICA MOVIMIENTO DE LIQUIDACION
        INSERT INTO dis_cuenta
        VALUES(lr_preliquida.*)
        
        --CAMBIA ESTADO A LA PRELIQUIDACION 
        UPDATE pen_preliquida_pmg
        SET    estado_pmg       = gr_edo2.liquidado
        WHERE  nss              = lr_preliquida.nss
        AND    consecutivo_lote = lr_preliquida.consecutivo_lote
        AND    subcuenta        = lr_preliquida.subcuenta
        AND    folio            = pi_folio
        AND    estado_pmg       = gr_edo2.pre_liq


    END FOREACH
    
    MESSAGE " "
    
END FUNCTION

#==============================================================================#
# f_actualiza_liquidacion :                                                    #
#                Verifica todos los nss del folio liquidado para determinar    #
#                si se inserta una nueva mensualidad o si se termino de        #
#                pagar para realizar la desmarca                               #
#==============================================================================#
FUNCTION f_actualiza_liquidacion(pi_folio, pdt_fecha_liquida)

    DEFINE pi_folio             INTEGER 
    DEFINE pdt_fecha_liquida    DATE
    DEFINE lr_ctr_pago_det_iss  RECORD LIKE pen_ctr_pago_det_iss.*

    DEFINE lr_pagar             RECORD
                                nss             LIKE pen_preliquida_pmg.nss             ,
                                consecutivo     LIKE pen_preliquida_pmg.consecutivo_lote,
                                mensualidad     LIKE pen_preliquida_pmg.mensualidad     ,
                                sec_pension     LIKE pen_solicitud_iss.sec_pension
                                END RECORD

    DEFINE ld_monto_pago        DECIMAL(16,6)
    DEFINE ld_saldo_dia         DECIMAL(16,6)
    DEFINE ls_edc_liq           SMALLINT      
    DEFINE lc_regimen           CHAR(2)
    DEFINE li_cod_tramite_pmg   INTEGER
    DEFINE ls_status_edocta     SMALLINT
    DEFINE ls_bandera_edocta    SMALLINT
    
    ----------------------------------------------------------------------------
    
    LET ld_monto_pago      = 0 	
    LET ls_edc_liq         = 0
    LET lc_regimen         = NULL
    LET li_cod_tramite_pmg = 5
    LET ls_bandera_edocta  = 0

    --SELECCIONA MOVIMIENTOS PRELIQUIDADOS DEL FOLIO
    DECLARE cur_pagos CURSOR FOR
    SELECT UNIQUE(A.nss)        ,
           A.consecutivo_lote   ,
           A.mensualidad        ,
           B.sec_pension        
    FROM   pen_preliquida_pmg A ,
           pen_solicitud_iss  B
    WHERE  A.folio              = pi_folio
    AND    A.estado_pmg         = 70 --gr_edo2.liquidado
    AND    A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    ORDER BY 1
	
    FOREACH cur_pagos INTO lr_pagar.*
        
        -- obtenemos el saldo del trabajador despues de la liquidacion
        LET ld_saldo_dia = f_saldo_dia(lr_pagar.nss, pdt_fecha_liquida)
       
        -- Si el saldo es cero se desmarca la cuenta, se actualiza a liquidado y se envia a op70
        IF ld_saldo_dia = 0 THEN

            CALL f_desmarca_cuenta(lr_pagar.nss         ,
                                   lr_pagar.consecutivo ,
                                   "S"
                                  )

            --CALL f_agrega_det_op70_2(lr_pagar.nss         ,
            --                       lr_pagar.consecutivo ,
            --                       gr_noti.conclusion       
            --                      )
            
            --ACTUALIZA LA SOLICITUD A LIQUIDADA
            UPDATE pen_solicitud_iss
            SET    estado_solicitud = gr_edo2.liquidado
            WHERE  nss              = lr_pagar.nss
            AND    consecutivo      = lr_pagar.consecutivo

            LET ls_edc_liq = 1

        ELSE
            --OBTIENE LA MENSUALIDAD
            SELECT *
            INTO   lr_ctr_pago_det_iss.*
            FROM   pen_ctr_pago_det_iss
            WHERE  nss              = lr_pagar.nss
            AND    consecutivo      = lr_pagar.consecutivo
            AND    folio_liquida    = pi_folio
            AND    num_mensualidad  = lr_pagar.mensualidad

            
            --SE TOMA EL MONTO AUTORIZADO VIGENTE
            LET ld_monto_pago = f_obten_pago_mensual_actual()

            -- si el saldo es dif de cero se valida si alcanza a pagar una mensualidad completa
            IF ld_saldo_dia < lr_ctr_pago_det_iss.mto_pago_pesos  THEN
                LET ld_monto_pago = ld_saldo_dia
            ELSE
                -- si alcanza a pagar una mensualidad se valida si el restante alcanza para otra                
                IF ld_saldo_dia < (ld_monto_pago * 2) THEN
                    -- si no alcanza, se inserta como pago la mensualidad mas el remanente
                    LET ld_monto_pago = ld_saldo_dia
                END IF
            END IF -- saldo menor a pago mensual

            --INSERTA NUEVA MENSUALIDAD            
            CALL f_inserta_pago_sig(lr_ctr_pago_det_iss.*, ld_monto_pago)

        END IF  -- saldo cero
        IF lr_pagar.mensualidad = 1 OR 
           ((lr_pagar.mensualidad MOD 12) = 0 ) OR 
           ls_edc_liq = 1 THEN

           WHENEVER ERROR CONTINUE
           EXECUTE exe_inserta_edocta USING lr_pagar.nss         ,     --pc_nss           
                                            lc_regimen           ,     --pc_regimen       
                                            li_cod_tramite_pmg   ,     --pi_cod_tramite   
                                            HOY2                 ,     --pdt_fecha_liquida
                                            pi_folio             ,     --pi_folio_liquida 
                                            lr_pagar.consecutivo       --pd_consec_liquida
                                      INTO  ls_status_edocta
           IF SQLCA.SQLCODE < 0 THEN
              LET ls_bandera_edocta = 1
              CALL ERRORLOG("ERROR AL INSERTAR SOLICITUD DE EDO CTA NSS "||lr_pagar.nss)
              CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
           END IF
           WHENEVER ERROR STOP
           
           LET ls_edc_liq = 0 

        END IF         
        --AACTUALIZA FOLIO DE PAGO DEL NSS EN PROCESAMIENTO
        CALL f_actualiza_folio_pago(lr_pagar.nss,lr_pagar.consecutivo,pi_folio)
        
    END FOREACH --Siguiente solicitud preliquidada

END FUNCTION

#==============================================================================#
# tercer paso : Actualiza los nuevos folios de pago para las solicitudes       #
#               generadas en el folio                                          #
#==============================================================================#
FUNCTION f_actualiza_folio_pago(p_nss, p_consecutivo, p_folio_liquida)

    DEFINE p_nss                     CHAR(11)
    DEFINE p_consecutivo             INTEGER
    DEFINE p_folio_liquida           INTEGER   --folio con el que se liquido
   
    DEFINE lr_pen_ctr_pago_det_iss   RECORD LIKE pen_ctr_pago_det_iss.*
    DEFINE ls_cont_benef             SMALLINT

    --WHENEVER ERROR CONTINUE 
   
    ------------------------------------------------
    --SELECIONA MENSUALIDADES LIQUIDADAS CON EL FOLIO
    ------------------------------------------------
    SELECT  * 
    INTO    lr_pen_ctr_pago_det_iss.*
    FROM    pen_ctr_pago_det_iss
    WHERE   nss           = p_nss
    AND     consecutivo   = p_consecutivo
    AND     folio_liquida = p_folio_liquida

    IF SQLCA.SQLCODE < 0 THEN
       LET g_mensaje="ERROR AL RECUPERAR PAGO DET DEL NSS ",p_nss,
                     " CON CONSECUTIVO ",p_consecutivo USING "<<<<<<<<<&"
       CALL ERRORLOG(g_mensaje)
       PROMPT g_mensaje CLIPPED FOR CHAR enter2
       RETURN
    END IF

    -----------------------------------------------------------------
    --SE GENERA NUEVO FOLIO DE PAGO PARA MENSUALIDADES SUBSECUENTES
    -----------------------------------------------------------------
    --VERIFICA QUE TENGA UN BENEFICIARIO REGISTRADO
    SELECT COUNT(*)
    INTO   ls_cont_benef
    FROM   ret_beneficiario
    WHERE  nss           = p_nss
    AND    consecutivo   = p_consecutivo
    AND    consec_benef  = 1  
   
    IF ls_cont_benef = 0 OR  SQLCA.SQLCODE = NOTFOUND THEN
       --NO ENCONTRO BENEFICIARIO
       LET g_mensaje="EL NSS ",p_nss," CON CONSECUTIVO ",
                     p_consecutivo USING "<<<<<<<<<&", " NO TIENE BENEFICIARIOS<ENTER>:"
       CALL ERRORLOG(g_mensaje)
       --PROMPT g_mensaje CLIPPED FOR CHAR enter2
       --RETURN
    END IF 
   
    --ACTUALIZA ESTADO A LA MENSUALIDAD
   
    UPDATE pen_ctr_pago_det_iss
    SET    estado = 70 --LIQUIDADO
    WHERE  nss           = p_nss
    AND    consecutivo   = p_consecutivo
    AND    folio_liquida = p_folio_liquida 

END FUNCTION 

#==============================================================================#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la        #
#                     cuenta cuando esta se elimine                            #
#==============================================================================#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca     RECORD
           nss             LIKE pen_solicitud_iss.nss          ,
           consec          LIKE pen_solicitud_iss.consecutivo  ,
           tipo_retiro     LIKE pen_solicitud_iss.tipo_retiro
           END RECORD

    DEFINE lr_dat RECORD
           edo_marca       SMALLINT ,
           marca_causa     SMALLINT
           END RECORD

    DEFINE ls_movim        SMALLINT

    ----------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    SELECT movimiento
    INTO   ls_movim
    FROM   tab_retiro
    WHERE  tipo_retiro = pr_desmarca.tipo_retiro

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               ls_movim             ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario2            --usuario

END FUNCTION

#==============================================================================#
# f_agrega_det_op70_2 : Inserta un registro en la tabla de detalle de la op70  #
#                     de acuerdo al tipo de notificacion                       #
#==============================================================================#
--POR EL MOMENTO NO SE USA (NO ELIMINAR)
--FUNCTION f_agrega_det_op70_2(pr_datos)
--
--    DEFINE pr_datos RECORD
--        nss          LIKE pen_solicitud_iss.nss         ,
--        consecutivo  LIKE pen_solicitud_iss.consecutivo ,
--        id_notifica  LIKE tab_tipo_notifica_pmg.clave  
--    END RECORD 
--
--    DEFINE lr_preliq RECORD
--        subcuenta           SMALLINT        ,
--        monto_pesos         DECIMAL(22,6)
--    END RECORD
--    
--    DEFINE lr_dat_saldo RECORD
--        subcuenta           SMALLINT    ,
--        grupo               SMALLINT
--    END RECORD 
--
--    DEFINE lr_saldo_dia RECORD
--        subcuenta           SMALLINT        ,
--        siefore             SMALLINT        ,
--        monto_acc           DECIMAL(16,6)   ,
--        monto_pesos         DECIMAL(16,6)
--    END RECORD
--    
--    DEFINE lr_op70 RECORD LIKE pen_detalle_op70.*
--        
--    -- --------------------------------------------------------------------
--    LET lr_dat_saldo.subcuenta      = 0
--    LET lr_dat_saldo.grupo          = 0
--
--    LET lr_op70.folio_envio         = 0
--    LET lr_op70.folio_datamart      = 0
--    LET lr_op70.id_tipo_notifica    = pr_datos.id_notifica
--    LET lr_op70.nss                 = pr_datos.nss
--    LET lr_op70.cve_pension         = NULL
--    LET lr_op70.diag_operacion      = NULL
--    LET lr_op70.codigo_rechazo      = 0
--    LET lr_op70.fecha_fallecimiento = NULL
-- 
--    LET lr_op70.origen_informacion  = 0
--    LET lr_op70.fecha_carga         = HOY2
--    LET lr_op70.hora_carga          = CURRENT HOUR TO SECOND
--    LET lr_op70.usuario             = gc_usuario2
--    LET lr_op70.estado              = gr_edo2.capturado
--    LET lr_op70.num_men_calculadas  = 0 
--
--
--    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
--        LET lr_op70.fecha_agotamiento = HOY2 
--    ELSE
--        LET lr_op70.fecha_agotamiento = NULL
--    END IF
--
--    SELECT curp             ,
--           sec_pension      ,
--           tipo_retiro      ,
--           regimen          ,
--           tipo_seguro      ,
--           tipo_pension     ,
--           "   "            ,
--           tipo_prestacion  ,
--           fecha_ini_pen
--    INTO   lr_op70.curp             ,
--           lr_op70.sec_pension      ,
--           lr_op70.tipo_retiro      ,
--           lr_op70.regimen          ,
--           lr_op70.tipo_seguro      ,
--           lr_op70.tipo_pension     ,
--           lr_op70.cve_pension      ,
--           lr_op70.tipo_prestacion  ,
--           lr_op70.fecha_ini_pen
--    FROM   pen_solicitud_iss
--    WHERE  nss         = pr_datos.nss         
--    AND    consecutivo = pr_datos.consecutivo 
--    
--    SELECT fecha_liquida
--    INTO   lr_op70.fecha_primer_pago     
--    FROM   pen_ctr_pago_det_iss
--    WHERE  nss              = pr_datos.nss         
--    AND    consecutivo      = pr_datos.consecutivo 
--    AND    num_mensualidad  = 1
--
--    SELECT MAX(num_mensualidad)
--    INTO   lr_op70.num_men_pagadas
--    FROM   pen_ctr_pago_det_iss
--    WHERE  nss          = pr_datos.nss         
--    AND    consecutivo  = pr_datos.consecutivo 
--    AND    estado       = gr_edo2.liquidado
--    
--    SELECT fecha_liquida
--    INTO   lr_op70.fecha_ultimo_pago  
--    FROM   pen_ctr_pago_det_iss
--    WHERE  nss              = pr_datos.nss         
--    AND    consecutivo      = pr_datos.consecutivo 
--    AND    estado           = gr_edo2.liquidado
--    AND    num_mensualidad  = lr_op70.num_men_pagadas
--    
--    SELECT SUM(mto_pago_pesos)
--    INTO   lr_op70.mto_total_pmg
--    FROM   pen_ctr_pago_det_iss
--    WHERE  nss          = pr_datos.nss         
--    AND    consecutivo  = pr_datos.consecutivo 
--    AND    estado       = gr_edo2.liquidado
--
--    -- Montos de la ultima mensualidad pagada
--    LET lr_op70.mto_retiro97    = 0
--    LET lr_op70.mto_cv          = 0
--    LET lr_op70.mto_cs          = 0
--    LET lr_op70.mto_viv97       = 0
--
--    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
--        DECLARE cur_op70pre CURSOR FOR
--        SELECT subcuenta            ,
--               monto_en_pesos * -1
--        FROM   pen_preliquida_pmg
--        WHERE  nss              = pr_datos.nss         
--        AND    consecutivo_lote = pr_datos.consecutivo 
--        AND    mensualidad      = lr_op70.num_men_pagadas
--        
--        FOREACH cur_op70pre INTO lr_preliq.*
--        
--            CASE lr_preliq.subcuenta
--                WHEN 1
--                    LET lr_op70.mto_retiro97 = lr_op70.mto_retiro97 + lr_preliq.monto_pesos
--                WHEN 2                       
--                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
--                WHEN 6                       
--                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
--                WHEN 9                       
--                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
--                WHEN 5                       
--                    LET lr_op70.mto_cs       = lr_op70.mto_cs + lr_preliq.monto_pesos
--                WHEN 4                       
--                    LET lr_op70.mto_viv97    = lr_op70.mto_viv97 + lr_preliq.monto_pesos
--            END CASE
--        
--        END FOREACH
--    END IF
--
--    -- Saldo de la subcuenta
--    LET lr_op70.saldo_retiro97  = 0
--    LET lr_op70.saldo_cv        = 0
--    LET lr_op70.saldo_cs        = 0
--    LET lr_op70.saldo_viv97     = 0
--    
--    DECLARE cur_saldo_op70 CURSOR FOR eje_saldo_dia
--
--    FOREACH cur_saldo_op70 USING pr_datos.nss            ,
--                                 lr_dat_saldo.subcuenta  ,
--                                 lr_dat_saldo.grupo      ,
--                                 HOY2                     
--                           INTO  lr_saldo_dia.*
--
--        CASE lr_saldo_dia.subcuenta
--            WHEN 1
--                LET lr_op70.saldo_retiro97  = lr_op70.saldo_retiro97 + lr_preliq.monto_pesos
--            WHEN 2                       
--                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
--            WHEN 6                          
--                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
--            WHEN 9                          
--                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
--            WHEN 5                          
--                LET lr_op70.saldo_cs        = lr_op70.saldo_cs + lr_preliq.monto_pesos
--            WHEN 4                          
--                LET lr_op70.saldo_viv97     = lr_op70.saldo_viv97 + lr_preliq.monto_pesos
--        END CASE
--       
--    END FOREACH
--
--
--    INSERT INTO pen_detalle_op70
--    VALUES (lr_op70.*)
--
--END FUNCTION 

#==============================================================================#
# f_inserta_pago_sig : Inserta el registro de la siguiente mensualidad en      #
#                       pen_ctr_pago_det_iss en caso de no existir             #
#==============================================================================#
FUNCTION f_inserta_pago_sig(pr_ctr_pago_det_iss, pd_pago_mensual)

    DEFINE pr_ctr_pago_det_iss     RECORD LIKE pen_ctr_pago_det_iss.*
    DEFINE pd_pago_mensual         DECIMAL(16,6)
    DEFINE lr_pago_det_nuevo       RECORD LIKE pen_ctr_pago_det_iss.*

    DEFINE ls_tot_mens             SMALLINT 
    DEFINE ls_mes                  SMALLINT
    DEFINE ls_mod                  SMALLINT
    DEFINE lc_mensaje              CHAR(200)
    ----------------------------------------------------------------------------
    
    LET ls_mes      = 1
    LET ls_tot_mens = 0
    INITIALIZE lr_pago_det_nuevo.* TO NULL
    

    LET ls_mod = pr_ctr_pago_det_iss.num_mensualidad MOD 12

    --VERIFICA SI ES MENSUALIDAD 12 O MULTIPLO DE 12
    IF ls_mod = 0 THEN
       --SE TRATA DE UNA MENSUALIDAD 12 O MULTIPLO
       --SE INFORMA EN BITACORA
       LET lc_mensaje = "CIERRE DE CONTRATO PARA NSS: ",pr_ctr_pago_det_iss.nss,
                        " MENSUALIDAD ",pr_ctr_pago_det_iss.num_mensualidad USING "<<<"
       CALL ERRORLOG(lc_mensaje CLIPPED)

       --NO GENERAR NUEVA MENSUALIDAD, SOLO SE ACTUALIZA SOLICITUD
       WHENEVER ERROR CONTINUE
          UPDATE pen_solicitud_iss
             SET estado_solicitud = 70    --SE LIQUIDA EL CONTRATO
           WHERE nss          = pr_ctr_pago_det_iss.nss
             AND consecutivo  = pr_ctr_pago_det_iss.consecutivo
             AND sec_contrato = pr_ctr_pago_det_iss.sec_contrato
             
          IF SQLCA.sqlcode < 0 THEN
             LET lc_mensaje = "ERROR AL ACTUALIZAR ESTADO DE SOLICITUD PARA NSS: ",
                              pr_ctr_pago_det_iss.nss," CONSECUTIVO: ",
                              pr_ctr_pago_det_iss.consecutivo," SEC_CONTRATO: ",
                              pr_ctr_pago_det_iss.sec_contrato
             CALL ERRORLOG(lc_mensaje CLIPPED)
             LET lc_mensaje = ERR_GET(SQLCA.SQLCODE)
             CALL ERRORLOG(lc_mensaje CLIPPED)
          END IF 
       WHENEVER ERROR STOP
    ELSE
       --NO ES MENSUALIDAD FIN DE CONTRATO
       --BUSCA SI LA MENSUALIDAD NUEVA YA EXISTE
       SELECT "OK"
       FROM   pen_ctr_pago_det_iss
       WHERE  nss              = pr_ctr_pago_det_iss.nss
       AND    consecutivo      = pr_ctr_pago_det_iss.consecutivo    
       AND    num_mensualidad  = pr_ctr_pago_det_iss.num_mensualidad + ls_mes
       GROUP BY 1
       
       IF SQLCA.SQLCODE = NOTFOUND THEN 
       
           LET lr_pago_det_nuevo.nss               = pr_ctr_pago_det_iss.nss            
           LET lr_pago_det_nuevo.consecutivo       = pr_ctr_pago_det_iss.consecutivo    
           LET lr_pago_det_nuevo.sec_contrato      = pr_ctr_pago_det_iss.sec_contrato
           LET lr_pago_det_nuevo.num_mensualidad   = pr_ctr_pago_det_iss.num_mensualidad + ls_mes
           LET lr_pago_det_nuevo.mto_pago_pesos    = pd_pago_mensual
           LET lr_pago_det_nuevo.estado            = gr_edo2.capturado        
       
       
           EXECUTE eje_agrega_mes USING pr_ctr_pago_det_iss.fecha_pago_estimada , 
                                        ls_mes 
                                  INTO  lr_pago_det_nuevo.fecha_pago_estimada
       
           --INSERTA NUEVA MENSUALIDAD
           INSERT INTO pen_ctr_pago_det_iss
           VALUES (lr_pago_det_nuevo.*)
       ELSE
           -- si existe el registro, se actualiza el monto a pagar por el obtenido
           UPDATE pen_ctr_pago_det_iss
           SET    mto_pago_pesos   = pd_pago_mensual
           WHERE  nss              = pr_ctr_pago_det_iss.nss
           AND    consecutivo      = pr_ctr_pago_det_iss.consecutivo    
           AND    num_mensualidad  = pr_ctr_pago_det_iss.num_mensualidad + ls_mes        
       END IF

    END IF

END FUNCTION


#==============================================================================#
# f_valida_folio : Valida que el folio capturada cumpla las condiciones        #
#                  necesarias para ser aceptado                                #
#==============================================================================#
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
            IF ls_edo_pmg = gr_edo2.liquidado THEN
                LET lc_mensaje = " EL FOLIO INGRESADO YA FUE LIQUIDADO"
                LET ls_estado = 1                
            END IF 
        END IF
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION

#==============================================================================#
# f_saldo_dia : obtiene el saldo al dia de las subcuentas relacionadas         #
#==============================================================================#
FUNCTION f_saldo_dia(pr_datos_trab)

    DEFINE pr_datos_trab      RECORD
           nss                LIKE pen_preliquida_pmg.nss, 
           fec_liquida        DATE
           END RECORD

    DEFINE lr_saldo           RECORD
           subcta             SMALLINT     ,
           sie                SMALLINT     ,
           monto_acc          DECIMAL(16,6),
           monto_pes          DECIMAL(16,6)
           END RECORD
    
    DEFINE ls_subcta          SMALLINT
    DEFINE ls_grupo           SMALLINT

    DEFINE ld_saldo_dia       DECIMAL(16,6)

    ----------------------------------------------------------------------------

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
           CONTINUE FOREACH 
        END IF
        
        IF (lr_saldo.subcta = 30) OR (lr_saldo.subcta = 31) OR (lr_saldo.subcta = 32) OR 
           (lr_saldo.subcta = 33) OR (lr_saldo.subcta = 34) OR (lr_saldo.subcta = 35) THEN 
           	
            LET ld_saldo_dia = ld_saldo_dia + lr_saldo.monto_pes
        END IF
    
    END FOREACH 

    RETURN ld_saldo_dia

END FUNCTION

#==============================================================================#
# CALCULA EL PAGO ACTUAL QUE CORRESPONDE CON EL PAGO REGISTRADO                #
#==============================================================================#
FUNCTION f_obten_pago_mensual_actual()
 
    DEFINE ls_tipo_pago            SMALLINT
    DEFINE ld_imp_mensual          DECIMAL(16,6)

    ---------------------------------------------------
    
    SELECT importe_mensual
    INTO   ld_imp_mensual
    FROM   tab_pmg_historica_iss
    WHERE  fecha_hasta IS NULL
    
    RETURN ld_imp_mensual
    
END FUNCTION




