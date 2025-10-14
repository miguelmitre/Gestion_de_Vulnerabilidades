#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC002  => PRELIQUIDACION DE SOLICITUDES DE RETIROS IMSS             #
#Fecha creacion    => 7 DE MARZO DE 2014                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################
#Modificado por    => Jonathan Joddy Zavala Zavala         23-Sep-2015          #
#Requerimiento     => MLM-3407 Retiro Total Imss // Siefore 0/90                #
#                  => Adaptaciones relacionadas con la siefore basica 0         #
#Req CPL-2362 CMR 08/02/2016 se valida la siefore del ret de matrimonio         #
#################################################################################
DATABASE safre_af

    DEFINE gr_estado RECORD
        enviado         LIKE ret_estado.estado_solicitud    ,
        recibido        LIKE ret_estado.estado_solicitud    ,
        pre_liq         LIKE ret_estado.estado_solicitud    ,
        rechazado       LIKE ret_estado.estado_solicitud    ,
        liquidado       LIKE ret_estado.estado_solicitud    ,
        proceso_pago    LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [99] OF RECORD #Arreglo para los precios_accion    #MLM-3407
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_mov RECORD
        desempleo,
        matrimonio,
        cargo_desinv,
        abono_desinv          SMALLINT
    END RECORD

    DEFINE gr_prestacion RECORD
        desempleo       SMALLINT,
        matrimonio      SMALLINT
    END RECORD


    DEFINE
        HOY                   DATE

    DEFINE
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE
        gs_mov_parcial        ,
        gs_peiss              ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_primer_pago        ,
        gs_cod_afore           SMALLINT,
        m_siefore_90           SMALLINT,  #MLM-3409
        ms_99_siefores         SMALLINT   #MLM-3407

   DEFINE 
        m_folio       LIKE ret_parcial.folio

#######################################################################################

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC002")
    CALL init()
    CALL f_abre_ventanas()

    MENU "PRELIQUIDACION"
        COMMAND "Preliquida" "Ejecuta la preliquidacion de Retiros IMSS"
            CALL f_genera_preliquida()

       -- COMMAND "Detalle X Siefore" "Muestra el detalle de montos preliquidados por siefore"
         --   CALL f_genera_detalle(HOY)

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Preliquidacion"
            CALL f_bitacora_err(0)

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU

    CALL f_cierra_ventanas()

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare              CHAR(300)

    DEFINE
        ls_dias_hab             SMALLINT

    -- -----------------------------------------------------------------------------

    LET HOY                         = TODAY
    LET gs_sieviv                   = 11
    LET ls_dias_hab                 = 2
    LET gs_primer_pago              = 1
    LET gs_num_siefores             = f_lib_obtiene_num_siefores()
    LET gc_usuario                  = f_lib_obten_user()
    LET gr_prestacion.desempleo     = 6           LET gr_prestacion.matrimonio    = 7
    LET gr_mov.desempleo            = 875         LET gr_mov.matrimonio           = 870
    LET gr_mov.cargo_desinv         = 927         LET gr_mov.abono_desinv         = 928
    LET m_siefore_90                = 90
    LET ms_99_siefores              = 99

    ----- CODIGOS AFORES -----
    SELECT codigo_afore INTO gs_cod_afore
    FROM   tab_afore_local

    SELECT afore_cod INTO gs_peiss
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*PENSIONISSSTE*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud INTO gr_estado.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud INTO gr_estado.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud INTO gr_estado.pre_liq
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud INTO gr_estado.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud INTO gr_estado.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- PRELIQUIDACION -----
    LET lc_prepare = "EXECUTE FUNCTION fn_preliquida_ret_ms( ?,?,?,? )"
    PREPARE eje_preliquida FROM lc_prepare

    LET lc_prepare = " "

    ----- RETENCION DE ISR -----
    LET lc_prepare = "EXECUTE FUNCTION fn_ret_isr_issste (?,?,?,?) "
    PREPARE eje_ret_isr FROM lc_prepare

    LET lc_prepare = " "

    ----- PAGO DE DIAS DE SALARIO BASE COT  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_calcula_pago_sbc_parciales (?,?,?)"
    PREPARE eje_parcial_sbc FROM lc_prepare

    ----- LIQUIDACION PARA PARCIAL MATRIOMONIO  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_liq_parcial_mat(?,?,?,?)"
    PREPARE eje_parcial_mat FROM lc_prepare

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "
    
    SELECT COUNT(*)
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_estado.recibido
    AND    tipo_prestacion = 6
    AND    folio = 0
    
    IF SQLCA.SQLCODE = 0 THEN  --existe registros de desempleo por app sin folio 
        
        LET m_folio = f_lib_obtiene_ult_folio()
        
        UPDATE ret_monto_siefore
        SET folio = m_folio
        WHERE consecutivo IN (SELECT consecutivo FROM ret_parcial
                               WHERE estado_solicitud = gr_estado.recibido
                                 AND tipo_prestacion = 6
                                 AND folio = 0)

        UPDATE ret_ctr_pago
        SET folio_op12 = m_folio
        WHERE consecutivo IN (SELECT consecutivo FROM ret_parcial
                               WHERE estado_solicitud = gr_estado.recibido
                                 AND tipo_prestacion = 6
                                 AND folio = 0)
                                 
        UPDATE ret_parcial_tx
        SET folio = m_folio
        WHERE consecutivo IN (SELECT consecutivo FROM ret_parcial
                               WHERE estado_solicitud = gr_estado.recibido
                                 AND tipo_prestacion = 6
                                 AND folio = 0)


        UPDATE ret_parcial
        SET folio = m_folio
        WHERE  estado_solicitud = gr_estado.recibido
        AND tipo_prestacion = 6
        AND    folio = 0
    END IF
    
    SELECT COUNT(*)
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_estado.recibido
    AND    tipo_prestacion = 7
    AND    folio = 0
    
    IF SQLCA.SQLCODE = 0 THEN  --existe registros de matrimonio por app sin folio 
        
        LET m_folio = f_lib_obtiene_ult_folio()
        
        UPDATE ret_monto_siefore
        SET folio = m_folio
        WHERE consecutivo IN (SELECT consecutivo FROM ret_parcial
                               WHERE estado_solicitud = gr_estado.recibido
                                 AND tipo_prestacion = 7
                                 AND folio = 0)

        UPDATE ret_parcial_tx
        SET folio = m_folio
        WHERE consecutivo IN (SELECT consecutivo FROM ret_parcial
                               WHERE estado_solicitud = gr_estado.recibido
                                 AND tipo_prestacion = 7
                                 AND folio = 0)

        UPDATE ret_parcial
        SET folio = m_folio
        WHERE  estado_solicitud = gr_estado.recibido
        AND tipo_prestacion = 7
        AND    folio = 0
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_preliquida : Ejecuta los pasos necesarios para realizar la       #
#                       preliquidacion de los procesos de retiros IMSS      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_preliquida()

    DEFINE lr_folios RECORD
        fec_prel            DATE                            ,
        total               LIKE ret_solicitud_tx.folio     ,
        parcial             LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE
        ls_flag_err             ,
        ls_procesa              SMALLINT

    DEFINE
        li_proceso              INTEGER

    -- -----------------------------------------------------------------------------

    CALL f_despliega_info() RETURNING ls_procesa, lr_folios.*

    IF (ls_procesa = TRUE) THEN
        CALL f_obtiene_precios_accion(lr_folios.fec_prel)

        CALL primer_paso(lr_folios.*)   #-- Realiza la preliquidacion de los montos

        CALL segundo_paso(lr_folios.*)  #-- Valida la informacion de los montos
            RETURNING ls_flag_err, li_proceso

        IF (ls_flag_err = FALSE) THEN
            CALL tercer_paso(lr_folios.*)   #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO")
            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

    CURRENT WINDOW IS win_menu_principal

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    preliquidacion de los procesos de retiros IMSS         #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lr_info RECORD
        fecha_preliq        DATE    ,
        sel_tot             CHAR    ,
        sel_par             CHAR    ,
        folio_tot           INTEGER ,
        folio_par           INTEGER
    END RECORD

    DEFINE lr_folios RECORD
        disp                INTEGER ,
        par                 INTEGER
    END RECORD

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_despliega

    LET ls_flag                 = 1
    LET lr_info.fecha_preliq    = HOY
    INITIALIZE lr_folios.* TO NULL

    -- DISPOSICIONES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.disp
    FROM   ret_solicitud_tx
    WHERE  estado_solicitud = gr_estado.recibido

    IF (lr_folios.disp IS NULL) THEN
        LET lr_folios.disp = 0
    END IF

    -- PARCIALES
    SELECT COUNT(UNIQUE folio) INTO   lr_folios.par
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_estado.recibido
    AND    folio <> 0

    IF (lr_folios.par IS NULL) THEN
        LET lr_folios.par = 0
    END IF
    
    INPUT BY NAME lr_info.* WITHOUT DEFAULTS

        BEFORE INPUT
            IF ( (lr_folios.disp = 0) AND (lr_folios.par = 0) ) THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PRELIQUIDAR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD fecha_preliq
            IF (lr_info.fecha_preliq IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_preliq
            END IF

        AFTER FIELD sel_tot
            IF lr_info.sel_tot = "x" THEN
                CALL f_muestra_folios("dis", lr_folios.disp) RETURNING lr_info.folio_tot
                DISPLAY BY NAME lr_info.folio_tot

                IF lr_info.folio_tot = 0 THEN
                    LET lr_info.sel_tot = " "
                    DISPLAY BY NAME lr_info.sel_tot
                END IF
            END IF

        AFTER FIELD sel_par
            IF (lr_info.sel_par = "x") THEN
                CALL f_muestra_folios("par", lr_folios.par) RETURNING lr_info.folio_par
                DISPLAY BY NAME lr_info.folio_par

                IF (lr_info.folio_par = 0) THEN
                    LET lr_info.sel_par = " "
                    DISPLAY BY NAME lr_info.sel_par
                END IF
            END IF

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (ESC)
            IF lr_info.sel_tot IS NULL THEN
                LET lr_info.sel_tot = " "
            END IF

            IF lr_info.sel_par IS NULL THEN
                LET lr_info.sel_par = " "
            END IF

            IF lr_info.sel_tot <> "x" THEN
                LET lr_info.folio_tot = 0
            END IF

            IF lr_info.sel_par <> "x" THEN
                LET lr_info.folio_par = 0
            END IF

            IF ( (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") ) THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PRELIQUIDAR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

            IF (lr_info.fecha_preliq IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_preliq
            END IF

            IF f_lib_pregunta("¿DESEA GENERAR LA PRELIQUIDACION? (S/N) : ") = TRUE THEN
                LET ls_flag = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_flag = FALSE
            END IF

            EXIT INPUT
    END INPUT

    RETURN ls_flag              ,
           lr_info.fecha_preliq ,
           lr_info.folio_tot    ,
           lr_info.folio_par

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Llama a las funciones que realizan los procesos de          #
#               preliquidacion                                              #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel            DATE                        ,
        total               LIKE ret_solicitud_tx.folio ,
        parcial             LIKE ret_solicitud_tx.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_resultados

    CALL f_tablas_tmp()

    IF (pr_folios.total <> 0) THEN
        CALL f_preliquida_tot(pr_folios.total, pr_folios.fec_prel)      -- Preliquidacion de retiros totales
    END IF

    IF (pr_folios.parcial <> 0) THEN
        CALL f_preliquida_par(pr_folios.parcial, pr_folios.fec_prel)    -- Preliquidacion de retiros parciales
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida los montos preliquidados                            #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel        DATE                            ,
        total           LIKE ret_solicitud_tx.folio     ,
        parcial         LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE li_idproc LIKE ret_bitacora_error.id_proceso

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_idproc = f_ultimo_id_err()

    ----------  Valida sobregiro de los retiros Totales     --------------
    CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.total, pr_folios.fec_prel, "tot")
        RETURNING ls_flag


    ----------  Valida sobregiro de los retiros parciales   --------------
    CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.parcial, pr_folios.fec_prel, "par")
        RETURNING ls_flag

    RETURN ls_flag, li_idproc


END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Vacia la informacion de las tablas temporales a las fisicas #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                        ,
        total       LIKE ret_solicitud_tx.folio ,
        parcial     LIKE ret_solicitud_tx.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1

    --- Preliquidacion de totales
    IF (pr_folios.total <> 0) THEN
        INSERT INTO ret_preliquida
        SELECT *
        FROM   tmp_preliquida_disp
        WHERE  folio = pr_folios.total

        CALL f_act_estado_sol(pr_folios.total, "tot")
    END IF

    --- Preliquidacion de parciales
    IF (pr_folios.parcial <> 0) THEN

        INSERT INTO ret_preliquida
        SELECT *
        FROM   tmp_preliquida_disp
        WHERE  folio = pr_folios.parcial

        INSERT INTO ret_monto_siefore
        SELECT *
        FROM   tmp_monto_siefore
        WHERE  folio = pr_folios.parcial

        CALL f_act_estado_sol(pr_folios.parcial, "par")
    END IF


    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_folios : Despliega la pantalla con los folios que existen para  #
#                    ser preliquidados. Busca de acuerdo a la modalidad de  #
#                    retiro que se desee utilizar                           #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_folios(pc_modalidad, pi_numfol)

    DEFINE
        pc_modalidad        CHAR(03)

    DEFINE lr_reg ARRAY[10] OF RECORD
        folio           INTEGER ,
        total           INTEGER ,
        aceptadas       INTEGER ,
        rechazadas      INTEGER ,
        sin_resp        INTEGER
    END RECORD

    DEFINE
        lc_proceso      CHAR(015),
        lc_tabla        CHAR(100),
        lc_and          CHAR(1000),
        lc_prepare      CHAR(500)

    DEFINE
        ls_estado           ,
        ls_pos              SMALLINT

    DEFINE
        pi_numfol           ,
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_reg[1].* TO NULL

    FOR ls_pos = 2 TO 10
        LET lr_reg[ls_pos].*    = lr_reg[1].*
    END FOR

    LET li_folio = 0

    CASE pc_modalidad
        WHEN "dis"
            LET lc_tabla    = " FROM ret_solicitud_tx "
            LET ls_estado   = gr_estado.recibido
            LET lc_proceso  = "DISPOSICIONES"
            LET lc_and      = ""

        WHEN "par"
            LET lc_tabla    = " FROM ret_parcial "
            LET ls_estado   = gr_estado.recibido
            LET lc_proceso  = "PARCIALES"
            LET lc_and      = ""
    END CASE

    LET lc_prepare = " SELECT UNIQUE(folio)  "      ,
                     lc_tabla                       ,
                     " WHERE  estado_solicitud = ? ",
                     lc_and CLIPPED                 ,
                     " ORDER BY 1 "

    PREPARE prp_dat FROM lc_prepare

    IF (pi_numfol > 0) THEN

        OPEN WINDOW win_folios AT 12,6 WITH FORM "RETC0024" ATTRIBUTE(BORDER)
        DISPLAY "                  FOLIOS DE RETIROS TOTALES A PRELIQUIDAR                 " AT 2,1
            ATTRIBUTE(REVERSE)

        DISPLAY " PRESIONE <ENTER> PARA SELECCIONAR FOLIO " AT 11,1

        WHILE TRUE
            DECLARE cur_dat CURSOR FOR prp_dat

            LET ls_pos = 1

            FOREACH cur_dat USING ls_estado
                            INTO  lr_reg[ls_pos].folio

                IF (pc_modalidad = "dis") THEN
                    -- Obtenemos el detalle de solicitudes de totales
                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].rechazadas
                    FROM   ret_solicitud_tx
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.rechazado
                    AND    diag_registro IS NOT NULL

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].aceptadas
                    FROM   ret_solicitud_tx
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.recibido
                    AND    diag_registro IS NOT NULL

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].sin_resp
                    FROM   ret_solicitud_tx
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.enviado

                    LET lr_reg[ls_pos].total = lr_reg[ls_pos].rechazadas + lr_reg[ls_pos].aceptadas + lr_reg[ls_pos].sin_resp
                ELSE
                	  -- Obtenemos el detalle de solicitudes de parciales
                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].rechazadas
                    FROM   ret_parcial
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.rechazado
                    AND    diag_cuenta_ind IS NOT NULL

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].aceptadas
                    FROM   ret_parcial
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.recibido
                    AND    diag_cuenta_ind IS NOT NULL

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].sin_resp
                    FROM   ret_parcial
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.enviado

                    LET lr_reg[ls_pos].total = lr_reg[ls_pos].rechazadas + lr_reg[ls_pos].aceptadas + lr_reg[ls_pos].sin_resp
                END IF

                LET ls_pos = ls_pos + 1
                IF (ls_pos >= 10) THEN
                    CALL f_lib_error_msg("SE LLEGO AL LIMITE DEL ARREGLO")
                    EXIT FOREACH
                END IF

            END FOREACH

            CALL SET_COUNT(ls_pos-1)
            DISPLAY ARRAY lr_reg TO scr_2.*
                ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                ON KEY (CONTROL-C, INTERRUPT)
                    LET ls_pos = 0
                    EXIT DISPLAY

                ON KEY ( CONTROL-M )
                    LET ls_pos      = ARR_CURR()
                    LET li_folio    = lr_reg[ls_pos].folio
                                        
                    EXIT DISPLAY

            END DISPLAY

            IF (ls_pos <> 0) THEN
                EXIT WHILE
            END IF

        END WHILE

        CLOSE WINDOW win_folios
    ELSE
        LET lc_tabla = "NO HAY FOLIOS PARA ", lc_proceso CLIPPED
        CALL f_lib_error_msg(lc_tabla)
    END IF

    RETURN li_folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_tot : Valida y realiza la preliquidacion para disposiciones  #
#                    de Retiros IMSS                                        #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_tot(pi_folio, pdt_fecha_pre)

    DEFINE pi_folio LIKE ret_solicitud_tx.folio

    DEFINE
        pdt_fecha_pre               DATE

    DEFINE lr_result RECORD
        codigo_res              SMALLINT      ,
        mto_total               DECIMAL(16,6)
    END RECORD

    DEFINE lr_datos_pre RECORD
        nss                 LIKE ret_solicitud_tx.nss           ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo   ,
        edo_sub_viv         LIKE ret_monto_viv.estado_sub_viv   ,
        diag_registro       LIKE ret_solicitud_tx.diag_registro
    END RECORD

    DEFINE
        lc_mensaje              CHAR(70)

    DEFINE
        ls_liquida              ,
        ls_num_regs             ,
        ls_num_vent             ,
        ls_subcta               SMALLINT

    DEFINE
        lc_curp                 CHAR(18)    ,
        lc_tipo_vent            CHAR(04)

    -- ---------------------------------------------------------------------------------

    DISPLAY "PRELIQUIDANDO RETIROS TOTALES ..." AT 4,5

    LET ls_liquida = TRUE

    DECLARE cur_nss CURSOR FOR
        SELECT A.nss            ,
               A.consecutivo    ,
               B.estado_sub_viv ,
               A.diag_registro
        FROM   ret_solicitud_tx A       ,
               ret_monto_viv B          ,
               tab_diag_procesar_disp C
        WHERE  A.folio              = pi_folio
        AND    A.folio              = B.folio
        AND    A.nss                = B.nss
        AND    A.consecutivo        = B.consecutivo
        AND    A.diag_registro      = C.diag_procesar
        AND    A.estado_solicitud   = gr_estado.recibido
        AND    C.id_aceptado        = 1
        ORDER BY A.tipo_retiro, A.nss


    FOREACH cur_nss INTO lr_datos_pre.*

        LET ls_num_regs = ls_num_regs + 1

        DECLARE cur_prov CURSOR FOR
            SELECT UNIQUE(subcuenta)
            FROM   dis_provision
            WHERE  folio            = pi_folio
            AND    nss              = lr_datos_pre.nss
            AND    consecutivo_lote = lr_datos_pre.consecutivo
            ORDER BY 1

        FOREACH cur_prov INTO ls_subcta

            IF ( (ls_subcta <> 4) AND (ls_subcta <> 8) ) THEN
                LET ls_liquida = TRUE
            ELSE
                LET ls_liquida = f_valida_pago_vivienda(lr_datos_pre.diag_registro,
                                                        lr_datos_pre.edo_sub_viv  ,
                                                        ls_subcta
                                                       )
            END IF

            IF (ls_liquida = TRUE) THEN
                EXECUTE eje_preliquida USING pi_folio          ,
                                             lr_datos_pre.nss  ,
                                             ls_subcta         ,
                                             pdt_fecha_pre
                                       INTO lr_result.*

                IF (lr_result.codigo_res < 0) THEN
                    LET lc_mensaje = "ERROR: NSS ", lr_datos_pre.nss,
                                     " COD ", lr_result.codigo_res USING "###&"

                    CALL f_lib_error_msg(lc_mensaje CLIPPED)
                    EXIT PROGRAM
                END IF

                -- Se actualiza la tabla de preliquidacion para que recalcule correctamente
                -- el valor de vivienda con el precio de accion
                IF ( (ls_subcta = 4) OR (ls_subcta = 8) ) THEN
                    UPDATE tmp_preliquida_disp
                    SET    monto_en_pesos   = ROUND(monto_en_acciones * precio_accion,2)
                    WHERE  nss              = lr_datos_pre.nss
                    AND    folio            = pi_folio
                    AND    subcuenta        = ls_subcta
                END IF

                SELECT n_unico
                INTO   lc_curp
                FROM   afi_mae_afiliado
                WHERE  n_seguro = lr_datos_pre.nss

                UPDATE tmp_preliquida_disp
                SET    curp             = lc_curp   ,
                       sucursal         = 0
                WHERE  nss              = lr_datos_pre.nss
                AND    folio            = pi_folio
                AND    subcuenta        = ls_subcta

            END IF

            LET ls_num_vent = 0

        END FOREACH -- Subcuentas

        INITIALIZE lr_datos_pre.* TO NULL
        LET ls_subcta = 0

    END FOREACH -- Siguiente NSS



END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_par : Valida y realiza la preliquidacion de retiros parciales#
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_par(pr_preliquida_par)

    DEFINE pr_preliquida_par RECORD
        folio                   LIKE ret_parcial.folio  ,
        fecha_preliquida        DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "PRELIQUIDANDO RETIROS PARCIALES  ..." AT 11,5

    CALL f_preliquida_desempleo(pr_preliquida_par.*)
    CALL f_preliquida_desempleo_anterior(pr_preliquida_par.*)
    CALL f_preliquida_matrimonio(pr_preliquida_par.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_desempleo : Realiza la preliquidacion para los retiros       #
#                          parciales por desempleo, modalidad A, B y        #
#                          complementarios                                  #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_desempleo(pr_preliquida_par)

    DEFINE pr_preliquida_par RECORD
        folio                   LIKE ret_parcial.folio  ,
        fecha_preliquida        DATE
    END RECORD

    DEFINE lr_liquida RECORD
        nss                 LIKE ret_ctr_pago.nss                   ,
        consecutivo         LIKE ret_ctr_pago.consecutivo           ,
        mensualidades       LIKE ret_ctr_pago.mensualidades         ,
        folio_op12          LIKE ret_ctr_pago.folio_op12            ,
        num_mensualidad     LIKE ret_ctr_pago_det.num_mensualidad   ,
        mto_mensualidad     LIKE ret_ctr_pago_det.mto_mensualidad   ,
        num_resolucion      LIKE ret_parcial.num_resolucion         ,
        curp                LIKE ret_parcial.curp
    END RECORD

    DEFINE lr_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_acc       DECIMAL(16,6) ,
        acc_pagar       DECIMAL(16,6) ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    DEFINE lr_montos RECORD
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv_tot   DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6)
    END RECORD

    DEFINE lr_marca RECORD #loc #lr_marca
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD

    DEFINE
        lc_msg              CHAR(60)

    DEFINE
        ls_movimiento       ,
        ls_ind_parcialidad  ,
        ls_siefore_tmp      SMALLINT

    DEFINE
        ld_mto_pagar        DECIMAL(16,6)

    DEFINE
        li_cont_regs        INTEGER,
        ls_dif              DECIMAL(16,6),      #Monto diferencia para desinversion MLM-3409
        ls_sie_reg          SMALLINT            #siefore del regimen

    -- -----------------------------------------------------------------------------

    LET lr_marca.estado_marca   = 0       LET lr_marca.marca_causa    = 0
    LET li_cont_regs            = 0
    LET ld_mto_pagar            = NULL    LET ls_ind_parcialidad      = NULL

    DECLARE cur_reg CURSOR FOR
        SELECT A.nss,
               A.consecutivo,
               A.mensualidades,
               A.folio_op12,
               B.num_mensualidad,
               B.mto_mensualidad,
               D.num_resolucion,
               D.curp
        FROM   ret_ctr_pago      A ,
               ret_ctr_pago_det  B ,
               ret_parcial_tx    C ,
               ret_parcial       D
        WHERE  A.nss                = B.nss
        AND    A.consecutivo        = B.consecutivo
        AND    A.nss                = C.nss
        AND    A.consecutivo        = C.consecutivo
        AND    A.nss                = D.nss
        AND    A.consecutivo        = D.consecutivo
        AND    D.folio              = pr_preliquida_par.folio
        AND    D.diag_cuenta_ind    = 400
        AND    D.tipo_prestacion    = gr_prestacion.desempleo
        AND    D.estado_solicitud   = gr_estado.recibido
        ORDER BY 1

    -- Ciclo sobre cada NSS a liquidar
    FOREACH cur_reg INTO lr_liquida.*

        LET lr_montos.acciones_ret97    = 0         LET lr_montos.acciones_cs       = 0
        LET lr_montos.acciones_cv_tot   = 0

        SELECT A.tipo_movimiento INTO ls_movimiento
        FROM   tab_tipo_desempleo A, ret_parcial B
        WHERE  B.nss            = lr_liquida.nss
        AND    B.consecutivo    = lr_liquida.consecutivo
        AND    A.tipo_desempleo = B.tipo_desempleo

        SELECT "OK" FROM ret_parcialidad_des
        WHERE consecutivo = lr_liquida.consecutivo
        AND consec_pago = gs_primer_pago
        AND estado = gr_estado.recibido
        GROUP BY 1   #MLM-3409

        LET ls_ind_parcialidad = SQLCA.SQLCODE

        IF ls_ind_parcialidad = 0 THEN

            LET ld_mto_pagar = lr_liquida.mto_mensualidad     #Se asigna el monto total del retiro parcial

            SELECT monto_en_pesos INTO lr_liquida.mto_mensualidad
            FROM ret_parcialidad_des
            WHERE nss = lr_liquida.nss
            AND consecutivo = lr_liquida.consecutivo
            AND consec_pago = gs_primer_pago
            AND estado = gr_estado.recibido

            LET ls_sie_reg = f_lib_obtiene_siefore_act(lr_liquida.nss)  #MLM-3409

        END IF

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_sbc CURSOR FOR eje_parcial_sbc
        FOREACH cur_sbc USING lr_liquida.nss             ,
                              lr_liquida.mto_mensualidad ,
                              pr_preliquida_par.fecha_preliquida
                        INTO  lr_pago.*

            IF (lr_pago.subcta > 0) THEN
                LET ls_siefore_tmp           = lr_pago.siefore
                CALL f_inserta_preliquida(pr_preliquida_par.folio               ,
                                          lr_liquida.nss                         ,
                                          lr_liquida.curp                        ,
                                          lr_liquida.consecutivo                 ,
                                          lr_pago.subcta                         ,
                                          lr_pago.siefore                        ,
                                          ls_movimiento                          ,
                                          pr_preliquida_par.fecha_preliquida    ,
                                          gar_precio_acc[ls_siefore_tmp].precio_dia   ,
                                          -lr_pago.acc_pagar                     ,
                                          -lr_pago.pesos_pagar
                                         )

                CASE lr_pago.subcta
                    WHEN 1
                        LET lr_montos.acciones_ret97  = lr_montos.acciones_ret97  + lr_pago.acc_pagar
                    WHEN 5
                        LET lr_montos.acciones_cs     = lr_montos.acciones_cs     + lr_pago.acc_pagar
                    WHEN 2
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                    WHEN 6
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                    WHEN 9
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                END CASE
            ELSE
                LET lc_msg  = "ERROR EN LA LIQUIDACION ", lr_liquida.nss, " CVE ERR: ", lr_pago.subcta
                CALL f_lib_error_msg(lc_msg)
                EXIT PROGRAM
            END IF
        END FOREACH -- Siguiente subcuenta

        IF ls_ind_parcialidad = 0 THEN

             LET lr_montos.acciones_ret97    = 0             LET lr_montos.acciones_cs       = 0
             LET lr_montos.acciones_cv_tot   = 0

             -- Obtenemos los montos por subcuenta para reportar el pago total en la OP16
             DECLARE cur_sbc_parc CURSOR FOR eje_parcial_sbc
             FOREACH cur_sbc_parc USING lr_liquida.nss ,
                                        ld_mto_pagar ,
                                        pr_preliquida_par.fecha_preliquida
                                  INTO  lr_pago.*

                 IF (lr_pago.subcta > 0) THEN

                     CASE lr_pago.subcta
                         WHEN 1
                             LET lr_montos.acciones_ret97  = lr_montos.acciones_ret97  + lr_pago.acc_pagar
                         WHEN 5
                             LET lr_montos.acciones_cs     = lr_montos.acciones_cs     + lr_pago.acc_pagar
                         WHEN 2
                             LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                         WHEN 6
                             LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                         WHEN 9
                             LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                     END CASE
                 END IF
             END FOREACH -- Siguiente subcuenta
        END IF

        INSERT INTO tmp_monto_siefore
        VALUES (lr_liquida.nss              ,-- nss
                lr_liquida.consecutivo      ,-- consecutivo
                pr_preliquida_par.folio     ,-- folio
                "I"                         ,-- tipo_retiro
                16                          ,-- tipo_operacion
                lr_pago.siefore             ,-- siefore
                lr_montos.acciones_ret97    ,-- acciones_ret97
                lr_montos.acciones_cv_tot   ,-- acciones_cv
                lr_montos.acciones_cs       ,-- acciones_cs
                0                            -- acciones_ret92
               )

    END FOREACH -- Siguiente NSS

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_desempleo_anterior : Realiza la preliquidacion para los      #
#                          retiros parciales por desempleo, modalidad       #
#                          anterior                                         #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_desempleo_anterior(pr_preliquida_par)

    DEFINE pr_preliquida_par RECORD
        folio                   LIKE ret_parcial.folio  ,
        fecha_preliquida        DATE
    END RECORD

    DEFINE lr_par_ant RECORD
        nss                     LIKE ret_parcial_tx.nss             ,
        consecutivo             LIKE ret_parcial.consecutivo        ,
        pago_desempleo          LIKE ret_parcial.pago_desempleo     ,
        salario_base_cot        LIKE ret_parcial.salario_base_cot   ,
        num_resolucion          LIKE ret_parcial.num_resolucion     ,
        curp                    LIKE ret_parcial.curp
    END RECORD

    DEFINE lar_pago ARRAY [99] OF RECORD   #MLM-3407
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE lr_det_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_sie_acc   DECIMAL(16,6) ,
        acciones_pago   DECIMAL(16,6) ,
        pesos_pago      DECIMAL(16,6)
    END RECORD

    DEFINE
        ld_monto_acc_cv       DECIMAL(16,6)

    DEFINE
        ls_cont_des             INTEGER

    DEFINE
        ls_siefore              ,
        ls_siefore_tmp               SMALLINT

    -- -----------------------------------------------------------------------------

    DECLARE cur_des_ant CURSOR FOR
        SELECT  nss               ,
                consecutivo       ,
                pago_desempleo    ,
                salario_base_cot  ,
                num_resolucion
        FROM    ret_parcial
        WHERE   folio             = pr_preliquida_par.folio
        AND     tipo_prestacion   = gr_prestacion.desempleo
        AND     estado_solicitud  = gr_estado.recibido
        AND     diag_cuenta_ind   = 400 --Cuenta Aceptada
        AND     tipo_desempleo    = "D"

    FOREACH cur_des_ant INTO lr_par_ant.*

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO ms_99_siefores
            LET lar_pago[ls_siefore].activo             = FALSE
            LET lar_pago[ls_siefore].acciones_ret97     = 0
            LET lar_pago[ls_siefore].acciones_cv        = 0
            LET lar_pago[ls_siefore].acciones_cs        = 0
            LET lar_pago[ls_siefore].acciones_est       = 0
            LET lar_pago[ls_siefore].acciones_esp       = 0
        END FOR

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_sbc_ant CURSOR FOR eje_parcial_sbc
        FOREACH cur_sbc_ant USING lr_par_ant.nss                        ,
                                  lr_par_ant.pago_desempleo             ,
                                  pr_preliquida_par.fecha_preliquida
                            INTO  lr_det_pago.*

            IF lr_det_pago.acciones_pago > 0 THEN
                LET ls_siefore_tmp                  = lr_det_pago.siefore
                LET lar_pago[ls_siefore_tmp].activo = TRUE

                CALL f_inserta_preliquida(pr_preliquida_par.folio               ,
                                          lr_par_ant.nss                        ,
                                          lr_par_ant.curp                       ,
                                          lr_par_ant.consecutivo                ,
                                          lr_det_pago.subcta                    ,
                                          lr_det_pago.siefore                   ,
                                          gr_mov.desempleo                      ,
                                          pr_preliquida_par.fecha_preliquida    ,
                                          gar_precio_acc[ls_siefore_tmp].precio_dia  ,
                                          -lr_det_pago.acciones_pago            ,
                                          -lr_det_pago.pesos_pago
                                          )
            ELSE
                LET lr_det_pago.acciones_pago = 0
            END IF

            CASE lr_det_pago.subcta
                WHEN 1
                    LET lar_pago[ls_siefore_tmp].acciones_ret97 = lr_det_pago.acciones_pago
                WHEN 2
                    LET lar_pago[ls_siefore_tmp].acciones_cv    = lr_det_pago.acciones_pago
                WHEN 5
                    LET lar_pago[ls_siefore_tmp].acciones_cs    = lr_det_pago.acciones_pago
                WHEN 6
                    LET lar_pago[ls_siefore_tmp].acciones_est   = lr_det_pago.acciones_pago
                WHEN 9
                    LET lar_pago[ls_siefore_tmp].acciones_esp   = lr_det_pago.acciones_pago
            END CASE
        END FOREACH

        FOR ls_siefore = 1 TO ms_99_siefores

            LET ld_monto_acc_cv = 0

            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_pago[ls_siefore].activo = TRUE THEN

                LET ld_monto_acc_cv  = lar_pago[ls_siefore].acciones_cv  +
                                       lar_pago[ls_siefore].acciones_est +
                                       lar_pago[ls_siefore].acciones_esp

                INSERT INTO tmp_monto_siefore
                VALUES (lr_par_ant.nss                          ,-- nss
                        lr_par_ant.consecutivo                  ,-- consecutivo
                        pr_preliquida_par.folio                      ,-- folio
                        "I"                                     ,-- tipo_retiro
                        16                                      ,-- tipo_operacion
                        ls_siefore                              ,-- siefore
                        lar_pago[ls_siefore].acciones_ret97     ,-- acciones_ret97
                        ld_monto_acc_cv                         ,-- acciones_cv
                        lar_pago[ls_siefore].acciones_cs        ,-- acciones_cs
                        0                                        -- acciones_ret92
                       )
            END IF
        END FOR

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_matrimonio : Realiza la preliquidacion para los retiros      #
#                          parciales por matrimonio                         #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_matrimonio(pr_preliquida_mat)

    DEFINE pr_preliquida_mat RECORD
        folio                   LIKE ret_parcial.folio  ,
        fecha_preliquida        DATE
    END RECORD

    DEFINE lr_reg_mat RECORD
        nss                 LIKE ret_parcial.nss                ,
        curp                LIKE ret_parcial.curp               ,
        consecutivo         LIKE ret_parcial.consecutivo        ,
        impt_autorizado     LIKE ret_parcial.impt_autorizado    ,
        num_resolucion      LIKE ret_parcial.num_resolucion
    END RECORD

    DEFINE lr_montos RECORD
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv_tot   DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6)
    END RECORD

    DEFINE lr_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_acc       DECIMAL(16,6) ,
        acc_pagar       DECIMAL(16,6) ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    DEFINE ls_id_aporte_cs LIKE ret_parcial_resol.id_aportacion_cs

    DEFINE
        lc_msg                      CHAR(60)

    DEFINE
        ls_siefore_tmp              ,
        ls_resp_liq                 SMALLINT

    DEFINE
        ld_rowid                    DECIMAL(20,0)

    -- -----------------------------------------------------------------------------

    DECLARE cur_mat CURSOR FOR
        SELECT  nss             ,
                curp            ,
                consecutivo     ,
                impt_autorizado ,
                num_resolucion
        FROM    ret_parcial
        WHERE   folio               = pr_preliquida_mat.folio
        AND     tipo_prestacion     = gr_prestacion.matrimonio
        AND     estado_solicitud    = gr_estado.recibido
        AND     diag_cuenta_ind     = 400 --Cuenta Aceptada


    FOREACH cur_mat INTO lr_reg_mat.*

        LET lr_montos.acciones_ret97    = 0
        LET lr_montos.acciones_cs       = 0
        LET lr_montos.acciones_cv_tot   = 0

        SELECT MAX(ROWID)
        INTO   ld_rowid
        FROM   ret_parcial_resol
        WHERE  nss              = lr_reg_mat.nss
        AND    tipo_prestacion  = gr_prestacion.matrimonio
        AND    num_resolucion   = lr_reg_mat.num_resolucion

        SELECT id_aportacion_cs
        INTO   ls_id_aporte_cs
        FROM   ret_parcial_resol
        WHERE  nss              = lr_reg_mat.nss
        AND    tipo_prestacion  = gr_prestacion.matrimonio
        AND    num_resolucion   = lr_reg_mat.num_resolucion
        AND    ROWID            = ld_rowid
        GROUP BY 1

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_liq_mat CURSOR FOR eje_parcial_mat
        FOREACH cur_liq_mat USING lr_reg_mat.nss                    ,
                              lr_reg_mat.impt_autorizado        ,
                              ls_id_aporte_cs                   ,
                              pr_preliquida_mat.fecha_preliquida
                        INTO  lr_pago.*

            IF (lr_pago.subcta > 0) THEN
                LET ls_siefore_tmp   = lr_pago.siefore
                CALL f_inserta_preliquida(pr_preliquida_mat.folio               ,
                                          lr_reg_mat.nss                        ,
                                          lr_reg_mat.curp                       ,
                                          lr_reg_mat.consecutivo                ,
                                          lr_pago.subcta                        ,
                                          lr_pago.siefore                       ,
                                          gr_mov.matrimonio                     ,
                                          pr_preliquida_mat.fecha_preliquida    ,
                                          gar_precio_acc[ls_siefore_tmp].precio_dia  ,
                                          -lr_pago.acc_pagar                    ,
                                          -lr_pago.pesos_pagar
                                         )

                CASE lr_pago.subcta
                    WHEN 1
                        LET lr_montos.acciones_ret97  = lr_montos.acciones_ret97  + lr_pago.acc_pagar
                    WHEN 5
                        LET lr_montos.acciones_cs     = lr_montos.acciones_cs     + lr_pago.acc_pagar
                    WHEN 2
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                    WHEN 6
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                    WHEN 9
                        LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                END CASE
            ELSE
                LET lc_msg  = "ERROR EN LA LIQUIDACION ", lr_reg_mat.nss, " CVE ERR: ", lr_pago.subcta
                CALL f_lib_error_msg(lc_msg)
                EXIT PROGRAM
            END IF
        END FOREACH

        INSERT INTO tmp_monto_siefore
        VALUES (lr_reg_mat.nss              ,-- nss
                lr_reg_mat.consecutivo      ,-- consecutivo
                pr_preliquida_mat.folio     ,-- folio
                "I"                         ,-- tipo_retiro
                16                          ,-- tipo_operacion
                lr_pago.siefore             ,-- siefore
                lr_montos.acciones_ret97    ,-- acciones_ret97
                lr_montos.acciones_cv_tot   ,-- acciones_cv
                lr_montos.acciones_cs       ,-- acciones_cs
                0                            -- acciones_ret92
               )

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventanas : Abre y prepara las ventanas que se usaran en            #
#                   el programa                                             #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventanas()

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_menu_principal AT 4,4 WITH 20 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC002        PRELIQUIDACION DE SOLICITUDES DE RETIROS IMSS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_despliega AT 4,4 WITH FORM "RETC0021" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC002        PRELIQUIDACION DE SOLICITUDES DE RETIROS IMSS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_resultados AT 4,4 WITH FORM "RETC0021" ATTRIBUTE (BORDER)
    DISPLAY "                                                         DISPOSICIONES IMSS   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC002        PRELIQUIDACION DE SOLICITUDES DE RETIROS IMSS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                            " AT 5,1
    DISPLAY "                                                                            " AT 6,1
    DISPLAY "                                                                            " AT 7,1
    DISPLAY "                                                                            " AT 8,1
    DISPLAY "                                                                            " AT 9,1
    DISPLAY "                                                                            " AT 10,1
    DISPLAY "                                                                            " AT 11,1
    DISPLAY "                                                                            " AT 12,1
    DISPLAY "                                                                            " AT 13,1
    DISPLAY "                                                                            " AT 14,1
    DISPLAY "                                                                            " AT 15,1
    DISPLAY "                                                                            " AT 16,1

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_bitacora AT 4,4 WITH FORM "RETC0022" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC002       BITACORA DE ERRORES DE PRELIQUIDACION IMSS                     " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                              " AT 3,1
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_menu_principal


END FUNCTION


#---------------------------------------------------------------------------#
# f_cierra_ventanas : Cierra las ventanas usadas por el programa            #
#---------------------------------------------------------------------------#
FUNCTION f_cierra_ventanas()

    CLOSE WINDOW win_bitacora
    CLOSE WINDOW win_resultados
    CLOSE WINDOW win_despliega
    CLOSE WINDOW win_menu_principal

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usan en el proceso       #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_preliquida_disp
        DROP TABLE tmp_monto_siefore
    WHENEVER ERROR STOP

    -- ---------------------------------------

    SELECT *
    FROM   dis_cuenta
    WHERE  0 = 1
    INTO TEMP tmp_preliquida_disp

    -- ---------------------------------------

    SELECT *
    FROM   ret_monto_siefore
    WHERE  1 = 0
    INTO TEMP tmp_monto_siefore

    -- ---------------------------------------

END FUNCTION


#---------------------------------------------------------------------------#
# f_verifica_sobregiro : Valida que no exista sobregiro en las subcuentas   #
#                        que se provisionaron                               #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_sobregiro(pr_valida)

    DEFINE pr_valida RECORD
        bandera             SMALLINT                    ,
        id_proc             SMALLINT                    ,
        folio               LIKE ret_solicitud_tx.folio,
        fec_preliq          DATE                        ,
        tipo                CHAR(3)
    END RECORD

    DEFINE lr_error RECORD
        id_proceso          LIKE ret_bitacora_error.id_proceso     ,
        nss                 LIKE ret_bitacora_error.nss            ,
        curp                LIKE ret_bitacora_error.curp           ,
        folio               LIKE ret_bitacora_error.folio          ,
        tipo_campo          LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo           LIKE ret_bitacora_error.nom_campo      ,
        valor_campo         LIKE ret_bitacora_error.valor_campo    ,
        id_error            LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        lr_saldo_prov        RECORD
            nss             LIKE ret_solicitud_tx.nss            ,
            subcta          SMALLINT                             ,
            tipo_mov        SMALLINT                             , #CPL-2362
            monto_acc       LIKE dis_provision.monto_en_acciones ,
            monto_pes       LIKE dis_provision.monto_en_acciones
        END RECORD
        , lr_saldo_dia       RECORD  #CPL-2362
            subcta          SMALLINT                             ,
            siefore         SMALLINT                             ,
            monto_acc       LIKE dis_provision.monto_en_acciones ,
            monto_pes       LIKE dis_provision.monto_en_acciones
        END RECORD
    
    DEFINE ld_curp  LIKE dis_provision.curp

    DEFINE
        ls_error                ,
        ls_grupo                ,
        ls_siefore_reg          SMALLINT

    -- -----------------------------------------------------------------------------

    CASE pr_valida.tipo
        WHEN "tot"
            LET lr_error.tipo_campo   = "RETIRO TOTAL IMSS"

        WHEN "par"
            LET lr_error.tipo_campo   = "RETIRO PARCIAL IMSS"
    END CASE

    -- Inicializamos las variables de la bitacora de errores
    LET ls_error                = pr_valida.bandera
    LET lr_error.id_proceso     = pr_valida.id_proc
    LET lr_error.folio          = pr_valida.folio

    LET lr_error.id_error       = 10
    LET ls_grupo                = 0
    LET lr_saldo_dia.monto_acc  = 0

    DECLARE cur_tmp CURSOR FOR
    SELECT curp              ,
           nss               ,
           subcuenta         ,
           tipo_movimiento   ,
           monto_en_acciones ,
           monto_en_pesos
    FROM   tmp_preliquida_disp
    WHERE  folio    = pr_valida.folio
    ORDER BY 1,3

    FOREACH cur_tmp INTO ld_curp, lr_saldo_prov.*


        LET lr_error.nss          = lr_saldo_prov.nss
        LET lr_error.curp         = ld_curp
        
        LET ls_siefore_reg = f_lib_obtiene_siefore_act(lr_saldo_prov.nss) #CPL-2362
        
        DECLARE cur_val_sobre CURSOR FOR eje_saldo_dia

        OPEN cur_val_sobre USING lr_saldo_prov.nss      ,
                                 lr_saldo_prov.subcta   ,
                                 ls_grupo               ,
                                 pr_valida.fec_preliq

        FETCH cur_val_sobre INTO lr_saldo_dia.*

        CLOSE cur_val_sobre

         IF lr_saldo_prov.tipo_mov = 870 THEN  #CPL-2362
            IF ls_siefore_reg <> 90 THEN --INv-3588
                  IF lr_saldo_dia.siefore = 90 OR lr_saldo_dia.siefore = 10 THEN  --- CPL-2595 se agrega la siefore 10
                     CONTINUE FOREACH
                  END IF
            END IF
         END IF
        -- Si el monto de provisionado es mayor al saldo al dia entonces registra
        -- un sobregiro

        LET lr_saldo_prov.monto_acc = lr_saldo_prov.monto_acc * -1

        IF ( (lr_saldo_prov.subcta = 19) AND
             (f_lib_redondea_val(lr_saldo_prov.monto_pes,2) > f_lib_redondea_val(lr_saldo_dia.monto_pes,2)) )
           OR
           ( (lr_saldo_prov.subcta <> 19) AND
             (f_lib_redondea_val(lr_saldo_prov.monto_acc,2) > f_lib_redondea_val(lr_saldo_dia.monto_acc,2)) ) THEN

            LET lr_error.nom_campo    = "Sobregiro Subcta ", lr_saldo_prov.subcta


            IF (lr_saldo_prov.subcta <> 19) THEN
                LET lr_error.valor_campo  = "Prov = ", lr_saldo_prov.monto_acc USING "<<<<<<<<<.<<<<<<",
                                            " Saldo = ",  lr_saldo_dia.monto_acc USING "<<<<<<<<<.<<<<<<"
            ELSE
                LET lr_error.valor_campo  = "Prov = ", lr_saldo_prov.monto_pes USING "<<<<<<<<<.<<<<<<",
                                            " Saldo = ",  lr_saldo_dia.monto_pes USING "<<<<<<<<<.<<<<<<"
            END IF

            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

    END FOREACH

    RETURN ls_error

END FUNCTION


#---------------------------------------------------------------------------#
# f_inserta_preliquida : Inserta los movimientos de liquidacion en la       #
#                        cuenta individual                                  #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_preliquida(pr_liquida)

    DEFINE pr_liquida RECORD
        folio                   LIKE dis_cuenta.folio               ,
        nss                     LIKE dis_cuenta.nss                 ,
        curp                    LIKE dis_cuenta.curp                ,
        consecutivo             LIKE dis_cuenta.consecutivo_lote    ,
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        tipo_movimiento         LIKE dis_cuenta.tipo_movimiento     ,
        fecha_liquida           LIKE dis_cuenta.fecha_pago          ,
        precio_accion           LIKE dis_cuenta.precio_accion       ,
        monto_en_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_en_pesos          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_dis_cuenta RECORD LIKE dis_cuenta.*

    -- -----------------------------------------------------------------------------

    LET lr_dis_cuenta.tipo_movimiento       = pr_liquida.tipo_movimiento
    LET lr_dis_cuenta.subcuenta             = pr_liquida.subcuenta
    LET lr_dis_cuenta.siefore               = pr_liquida.siefore
    LET lr_dis_cuenta.folio                 = pr_liquida.folio
    LET lr_dis_cuenta.consecutivo_lote      = pr_liquida.consecutivo
    LET lr_dis_cuenta.nss                   = pr_liquida.nss
    LET lr_dis_cuenta.curp                  = pr_liquida.curp
    LET lr_dis_cuenta.folio_sua             = NULL
    LET lr_dis_cuenta.fecha_pago            = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.fecha_valor           = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.fecha_conversion      = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.monto_en_pesos        = pr_liquida.monto_en_pesos
    LET lr_dis_cuenta.monto_en_acciones     = pr_liquida.monto_en_acciones
    LET lr_dis_cuenta.precio_accion         = pr_liquida.precio_accion
    LET lr_dis_cuenta.dias_cotizados        = 0
    LET lr_dis_cuenta.sucursal              = NULL
    LET lr_dis_cuenta.id_aportante          = "RETIRO"
    LET lr_dis_cuenta.estado                = gr_estado.liquidado
    LET lr_dis_cuenta.fecha_proceso         = HOY
    LET lr_dis_cuenta.usuario               = gc_usuario
    LET lr_dis_cuenta.fecha_archivo         = HOY
    LET lr_dis_cuenta.etiqueta              = 0

    INSERT INTO tmp_preliquida_disp
    VALUES(lr_dis_cuenta.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_pago_vivienda : Valida si el estado de la subcuenta de vivienda  #
#                          es aceptado o no para pagarse                    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_pago_vivienda(pr_viv)

    DEFINE pr_viv RECORD
        diag_procesar   LIKE ret_solicitud_tx.diag_registro ,
        edo_viv         LIKE ret_monto_viv.estado_sub_viv   ,
        subcuenta       LIKE dis_provision.subcuenta
    END RECORD

    DEFINE
        ls_id_aceptado          SMALLINT

    -- -----------------------------------------------------------------------------

    IF (pr_viv.diag_procesar <> 400) THEN
        LET ls_id_aceptado = FALSE
    ELSE
        LET ls_id_aceptado = TRUE
    END IF

    IF (ls_id_aceptado = TRUE) THEN
        SELECT id_aceptado
        INTO   ls_id_aceptado
        FROM   tab_estado_sub_viv
        WHERE  estado_sub_viv = pr_viv.edo_viv

        IF (ls_id_aceptado IS NULL) THEN
            LET ls_id_aceptado = FALSE
        ELSE
            IF (pr_viv.edo_viv = 3 AND pr_viv.subcuenta <> 8) THEN
                LET ls_id_aceptado = FALSE
            END IF
        END IF

    END IF

    RETURN ls_id_aceptado

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_estado_sol : Actualiza los estados de solicitud de acuerdo al tipo  #
#                   de retiro IMSS                                          #
#---------------------------------------------------------------------------#
FUNCTION f_act_estado_sol(pr_folio, pc_id_oper)

    DEFINE pr_folio LIKE ret_solicitud_tx.folio

    DEFINE
        pc_id_oper      CHAR(3)

    DEFINE la_rets ARRAY[8] OF INTEGER

    DEFINE lr_datos RECORD
        nss         LIKE dis_provision.nss                  ,
        consec      LIKE dis_provision.consecutivo_lote     ,
        tipo_mov    LIKE dis_provision.tipo_movimiento
    END RECORD

    DEFINE
        lc_query                CHAR(500)

    DEFINE
        li_rechazo              INTEGER

    DEFINE
        ls_cont                 SMALLINT,
        ls_app                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_rechazo  = 0

    FOR ls_cont = 1 TO 8
        LET la_rets[ls_cont] = 0
    END FOR

    DECLARE cur_pre CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo_lote ,
               tipo_movimiento
        FROM   ret_preliquida
        WHERE  folio = pr_folio
        ORDER BY 1

    -- Actualiza el estado de la solicitud de los registros preliquidados
    CASE pc_id_oper

        ---- RETIROS TOTALES ----
        WHEN "tot"
            DISPLAY "FOLIO    : ", pr_folio AT 5,12

            DISPLAY "RETIRO D : ", la_rets[1] AT  6,05
            DISPLAY "RETIRO E : ", la_rets[2] AT  7,05
            DISPLAY "RETIRO F : ", la_rets[3] AT  8,05
            DISPLAY "RETIRO G : ", la_rets[4] AT  9,05
            DISPLAY "RETIRO H : ", la_rets[5] AT  6,33
            DISPLAY "RETIRO J : ", la_rets[6] AT  7,33
            DISPLAY "RETIRO M : ", la_rets[7] AT  8,33
            DISPLAY "RETIRO P : ", la_rets[8] AT  9,33
            DISPLAY "RECHAZOS : ", li_rechazo AT  6,57

            FOREACH cur_pre INTO lr_datos.*
                UPDATE ret_solicitud_tx
                SET    estado_solicitud = gr_estado.pre_liq
                WHERE  folio            = pr_folio
                AND    nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido

                UPDATE ret_bus_control_sol
                SET    estado_solicitud = gr_estado.pre_liq
                WHERE  folio            = pr_folio
                AND    nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido

                CASE lr_datos.tipo_mov
                    WHEN 820 -- D
                        LET la_rets[1] = la_rets[1] + 1
                        DISPLAY "RETIRO D : ", la_rets[1] AT  6,05

                    WHEN 830 -- E
                        LET la_rets[2] = la_rets[2] + 1
                        DISPLAY "RETIRO E : ", la_rets[2] AT  7,05

                    WHEN 840 -- F
                        LET la_rets[3] = la_rets[3] + 1
                        DISPLAY "RETIRO F : ", la_rets[3] AT  8,05

                    WHEN 850 -- G
                        LET la_rets[4] = la_rets[4] + 1
                        DISPLAY "RETIRO G : ", la_rets[4] AT  9,05

                    WHEN 860 -- H
                        LET la_rets[5] = la_rets[5] + 1
                        DISPLAY "RETIRO H : ", la_rets[5] AT  6,33

                    WHEN 880 -- J
                        LET la_rets[6] = la_rets[6] + 1
                        DISPLAY "RETIRO J : ", la_rets[6] AT  7,33

                    WHEN 825 -- M
                        LET la_rets[7] = la_rets[7] + 1
                        DISPLAY "RETIRO M : ", la_rets[7] AT  8,33

                    WHEN 835 -- P
                        LET la_rets[8] = la_rets[8] + 1
                        DISPLAY "RETIRO P : ", la_rets[8] AT  9,33

                END CASE


            END FOREACH

            INITIALIZE lr_datos.* TO NULL

            -- Se rechazan todas las solicitudes que no tengan preliquidacion
            DECLARE cur_rechazo CURSOR FOR
                SELECT A.nss            ,
                       A.consecutivo    ,
                       B.movimiento
                FROM   ret_solicitud_tx A   ,
                       tab_retiro B
                WHERE  A.folio              = pr_folio
                AND    A.estado_solicitud   = gr_estado.recibido
                AND    A.tipo_retiro        = B.tipo_retiro

            FOREACH cur_rechazo INTO lr_datos.*
                CALL f_desmarca_cuenta(lr_datos.*)

                UPDATE ret_solicitud_tx
                SET    estado_solicitud = gr_estado.rechazado
                WHERE  folio            = pr_folio
                AND    nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido

                UPDATE ret_bus_control_sol
                SET    estado_solicitud = gr_estado.rechazado
                WHERE  nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido

                LET li_rechazo = li_rechazo + 1
                DISPLAY "RECHAZOS : ", li_rechazo AT  6,57

                INITIALIZE lr_datos.* TO NULL

            END FOREACH

            DISPLAY "(TERMINADO)" AT 4,43

        ---- RETIROS PARCIALES ----
        WHEN "par"
            DISPLAY "FOLIO    : ", pr_folio AT 12,12

            DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 13,05
            DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 14,05
            DISPLAY "TIPO A               : ", la_rets[2] AT 15,05
            DISPLAY "TIPO B               : ", la_rets[3] AT 16,05
            DISPLAY "COMPLEMENTARIOS      : ", la_rets[4] AT 17,05
            DISPLAY "ANTERIORES           : ", la_rets[5] AT 18,05

            DISPLAY "MATRIMONIO PRESENCIAL: ", la_rets[6] AT 13,45
            DISPLAY "MATRIMONIO POR APP   : ", la_rets[8] AT 14,45
            DISPLAY "RECHAZOS             : ", li_rechazo AT 16,45

            FOREACH cur_pre INTO lr_datos.*
                UPDATE ret_parcial
                SET    estado_solicitud = gr_estado.pre_liq
                WHERE  nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido

                SELECT "OK"
                FROM ret_parcialidad_des
                WHERE consecutivo = lr_datos.consec
                AND consec_pago = gs_primer_pago
                AND estado = gr_estado.recibido

                IF SQLCA.SQLCODE = 0 THEN

                    UPDATE ret_parcialidad_des
                    SET estado = gr_estado.pre_liq,
                        folio = pr_folio
                    WHERE consecutivo = lr_datos.consec
                    AND consec_pago = gs_primer_pago
                    AND estado = gr_estado.recibido

                    UPDATE ret_parcial_sub
                    SET estado = gr_estado.pre_liq
                    WHERE consecutivo = lr_datos.consec
                    AND estado = gr_estado.recibido

                END IF

                LET ls_app = 0
                SELECT COUNT(*)
                INTO   ls_app
                FROM   ret_ws_notifica_app
                WHERE  nss = lr_datos.nss 
                AND    consecutivo = lr_datos.consec

                CASE lr_datos.tipo_mov
                    WHEN 876    -- Tipo A
                    	  IF ls_app = 0 THEN
                    	        LET la_rets[1] = la_rets[1] + 1
                    	  ELSE
                    	  	    LET la_rets[7] = la_rets[7] + 1
                    	  END IF
                        LET la_rets[2] = la_rets[2] + 1
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 13,05
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 14,05
                        DISPLAY "TIPO A               : ", la_rets[2] AT 15,05

                    WHEN 877    -- Tipo B
                        IF ls_app = 0 THEN
                    	        LET la_rets[1] = la_rets[1] + 1
                    	  ELSE
                    	  	    LET la_rets[7] = la_rets[7] + 1
                    	  END IF
                        LET la_rets[3] = la_rets[3] + 1
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 13,05
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 14,05
                        DISPLAY "TIPO B               : ", la_rets[3] AT 16,05

                    WHEN 878    -- Complementarios
                        IF ls_app = 0 THEN
                    	        LET la_rets[1] = la_rets[1] + 1
                    	  ELSE
                    	  	    LET la_rets[7] = la_rets[7] + 1
                    	  END IF
                        LET la_rets[4] = la_rets[4] + 1
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 13,05
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 14,05
                        DISPLAY "COMPLEMENTARIOS     : ", la_rets[4] AT 17,05

                    WHEN 875    -- Anteriores
                        IF ls_app = 0 THEN
                    	        LET la_rets[1] = la_rets[1] + 1
                    	  ELSE
                    	  	    LET la_rets[7] = la_rets[7] + 1
                    	  END IF
                        LET la_rets[5] = la_rets[5] + 1
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 13,05
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 14,05
                        DISPLAY "ANTERIORES          : ", la_rets[5] AT 18,05

                    WHEN 870    -- Matrimonio
                        IF ls_app = 0 THEN
                    	        LET la_rets[6] = la_rets[6] + 1
                    	  ELSE
                    	  	    LET la_rets[8] = la_rets[8] + 1
                    	  END IF
                        DISPLAY "MATRIMONIO PRESENCIAL: ", la_rets[6] AT 13,45
                        DISPLAY "MATRIMONIO POR APP   : ", la_rets[8] AT 14,45
                END CASE

            END FOREACH

            INITIALIZE lr_datos.* TO NULL

            -- Se rechazan todas las solicitudes que no tengan preliquidacion
            LET lc_query = " SELECT nss            ,        \n" ,
                           "        consecutivo    ,        \n" ,
                           "        CASE tipo_prestacion    \n" ,
                           "            WHEN 6 THEN 875     \n" ,
                           "            WHEN 7 THEN 870     \n" ,
                           "        END                     \n" ,
                           " FROM   ret_parcial             \n" ,
                           " WHERE  folio              = ?  \n" ,
                           " AND    estado_solicitud   = ?  \n" ,
                           " AND    consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub \n" ,
                           "                    WHERE estado <> ",gr_estado.liquidado,") \n"

            PREPARE prp_rechazo_par FROM lc_query
            DECLARE cur_rechazo_par CURSOR FOR prp_rechazo_par

            FOREACH cur_rechazo_par USING pr_folio          ,
                                          gr_estado.recibido
                                    INTO lr_datos.*

                CALL f_desmarca_cuenta(lr_datos.*)

                UPDATE ret_parcial
                SET    estado_solicitud = gr_estado.rechazado
                WHERE  folio            = pr_folio
                AND    nss              = lr_datos.nss
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_estado.recibido
                AND    consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub
                                            WHERE estado <> gr_estado.liquidado)

                LET li_rechazo = li_rechazo + SQLCA.SQLERRD[3]
                DISPLAY "RECHAZOS            : ", li_rechazo AT 16,45

                INITIALIZE lr_datos.* TO NULL

            END FOREACH


            DISPLAY "(TERMINADO)" AT 11,43

    END CASE

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por ldt_precios                           #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(ldt_precios)

    DEFINE
        ldt_precios             DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100) ,
        lc_mensaje              CHAR(100) ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING ldt_precios
                      INTO lr_precio_acc.*

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss             LIKE ret_solicitud_tx.nss          ,
        consec          LIKE ret_solicitud_tx.consecutivo  ,
        movimiento      LIKE tab_retiro.movimiento
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_movim            SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss          ,--nss
                               pr_desmarca.movimiento   ,--marca entrante
                               pr_desmarca.consec       ,--consecutivo
                               lr_dat.edo_marca         ,--estado_marco
                               lr_dat.marca_causa       ,--marca_causa
                               gc_usuario                --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso para insertar#
#                   los registros en la bitacora de errores                 #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_id_err()

    DEFINE
        li_iderr                INTEGER

    -- -----------------------------------------------------------------------------

    SELECT MAX(id_proceso) + 1
    INTO   li_iderr
    FROM   ret_bitacora_error

    IF (li_iderr IS NULL) THEN
        LET li_iderr = 1
    END IF

    RETURN li_iderr

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_bitacora : Inserta el registro en la bitacora de errores        #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_bitacora(pr_error)

    DEFINE pr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        folio        LIKE ret_bitacora_error.folio          ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_bitacora RECORD LIKE ret_bitacora_error.*

    DEFINE
        lc_hora         CHAR(05)

    -- -----------------------------------------------------------------------------

    LET lc_hora = TIME

    -- Campos generales
    LET lr_bitacora.programa    = "RETC002"
    LET lr_bitacora.nom_archivo = " "
    LET lr_bitacora.fecha_error = HOY
    LET lr_bitacora.hora_error  = lc_hora
    LET lr_bitacora.usuario     = gc_usuario

    -- Campos por parametro
    LET lr_bitacora.id_proceso  = pr_error.id_proceso
    LET lr_bitacora.nss         = pr_error.nss
    LET lr_bitacora.curp        = pr_error.curp
    LET lr_bitacora.folio       = pr_error.folio
    LET lr_bitacora.tipo_campo  = pr_error.tipo_campo
    LET lr_bitacora.nom_campo   = pr_error.nom_campo
    LET lr_bitacora.valor_campo = pr_error.valor_campo
    LET lr_bitacora.id_error    = pr_error.id_error

    INSERT INTO ret_bitacora_error
    VALUES (lr_bitacora.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_bitacora_err : Consulta la informacion de la bitacora de errores, ya    #
#                  sea por medio del menu principal o de forma automatica   #
#                  al presentarse un error                                  #
#---------------------------------------------------------------------------#
FUNCTION f_bitacora_err(pi_proceso)

    DEFINE pi_proceso LIKE ret_bitacora_error.id_proceso

    DEFINE lar_bitacora ARRAY[5000] OF RECORD
        programa        LIKE ret_bitacora_error.programa     ,
        folio           LIKE ret_bitacora_error.folio        ,
        id_proceso      LIKE ret_bitacora_error.id_proceso   ,
        fecha_error     LIKE ret_bitacora_error.fecha_error  ,
        hora_error      LIKE ret_bitacora_error.hora_error   ,
        usuario         LIKE ret_bitacora_error.usuario      ,
        nss             LIKE ret_bitacora_error.nss          ,
        curp            LIKE ret_bitacora_error.curp         ,
        tipo_campo      LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo       LIKE ret_bitacora_error.nom_campo    ,
        valor_campo     LIKE ret_bitacora_error.valor_campo  ,
        id_error        LIKE ret_bitacora_error.id_error     ,
        desc_error      LIKE tab_ret_cod_error.descripcion
    END RECORD

    DEFINE
        li_pos          INTEGER

    DEFINE
        lc_where        CHAR(200),
        lc_query        CHAR(500)

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_bitacora

    -- El proceso viene de la carga de archivo, por lo que se hace directa
    -- la carga del arreglo mediante el id de proceso
    IF (pi_proceso <> 0) THEN
        LET lc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         "FROM   ret_bitacora_error ",
                         "WHERE  id_proceso = " , pi_proceso
    ELSE
        -- Se inicia el construct para obtener los datos de consulta
        -- para la bitacora
        LET int_flag = FALSE

        CONSTRUCT BY NAME lc_where ON  folio             ,
                                       fecha_error       ,
                                       usuario
            ON KEY (CONTROL-C)
                LET INT_FLAG = TRUE
                EXIT CONSTRUCT

            ON KEY ( ESC )
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT


        END CONSTRUCT

        IF (INT_FLAG = TRUE) THEN
            LET INT_FLAG = FALSE
            CALL f_lib_error_msg("BUSQUEDA CANCELADA")
            CLEAR SCREEN
            CURRENT WINDOW IS win_menu_principal
            RETURN
        END IF

        LET lc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         " FROM   ret_bitacora_error ",
                         " WHERE ", lc_where CLIPPED ,
                         " AND    programa = 'RETC002' ",
                         " ORDER BY id_proceso DESC "
    END IF

    PREPARE prp_err FROM lc_query
    DECLARE cur_err CURSOR FOR prp_err

    LET li_pos = 1

    FOREACH cur_err INTO lar_bitacora[li_pos].*

        SELECT descripcion
        INTO   lar_bitacora[li_pos].desc_error
        FROM   tab_ret_cod_error
        WHERE  id_error = lar_bitacora[li_pos].id_error

        LET li_pos = li_pos + 1

    END FOREACH

    INITIALIZE lar_bitacora[li_pos].* TO NULL

        IF ( (li_pos - 1) >= 1 ) THEN
            CALL SET_COUNT(li_pos - 1)

            DISPLAY ARRAY lar_bitacora TO scr_err.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLEAR SCREEN
            CURRENT WINDOW IS win_menu_principal
        ELSE
            CALL f_lib_error_msg("NO EXISTEN REGISTROS")
            CLEAR SCREEN
            CURRENT WINDOW IS win_menu_principal
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_detalle : Genera la pantalla de detalle de montos preliquidados  #
#                    de la fecha en curso                                   #
#---------------------------------------------------------------------------#
FUNCTION f_genera_detalle(pdt_fec_detalle)

    DEFINE
        pdt_fec_detalle         DATE

    DEFINE lar_tot, lar_totales ARRAY[99] OF RECORD     #MLM-3409
        siefore              LIKE dis_cuenta.siefore           ,
        mto_acciones_tot     LIKE dis_cuenta.monto_en_acciones ,
        mto_acciones_par     LIKE dis_cuenta.monto_en_acciones ,
        mto_acciones_isr     LIKE dis_cuenta.monto_en_pesos    ,
        subtotal_acciones    LIKE dis_cuenta.monto_en_acciones ,
        mto_pesos_tot        LIKE dis_cuenta.monto_en_pesos    ,
        mto_pesos_par        LIKE dis_cuenta.monto_en_pesos    ,
        mto_pesos_isr        LIKE dis_cuenta.monto_en_pesos    ,
        subtotal_pesos       LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_montos RECORD
        tipo_mov        LIKE dis_cuenta.tipo_movimiento   ,
        acciones        LIKE dis_cuenta.monto_en_acciones ,
        pesos           LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        ls_sie              ,
        ls_cont              ,
        ls_contador          SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_detalle AT 4,4 WITH FORM "RETC0023" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC002 DETALLE POR MODALIDAD DE RETIRO Y SIEFORE DE SOLICITUDES             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    
    DECLARE cur_sie CURSOR FOR
    SELECT codigo_siefore 
    FROM   tab_siefore_local
    WHERE  ind_activo =  1
    AND    codigo_siefore NOT IN (0,12,13)
    ORDER  by 1

   FOREACH cur_sie INTO ls_sie
    
    --FOR ls_sie = 1 TO 99    #MLM-3409
      DISPLAY "siefore ", ls_sie
      LET ls_cont = ls_cont +1
        CASE ls_sie
            WHEN 6
                LET lar_tot[ls_sie].siefore = gs_sieviv

            WHEN 7
                LET lar_tot[ls_sie].siefore = 10

            WHEN 8
                LET lar_tot[ls_sie].siefore = 0

            OTHERWISE
                LET lar_tot[ls_sie].siefore = ls_sie

        END CASE

        LET lar_tot[ls_sie].mto_acciones_tot   = 0
        LET lar_tot[ls_sie].mto_acciones_par   = 0
        LET lar_tot[ls_sie].subtotal_acciones  = 0
        LET lar_tot[ls_sie].mto_pesos_tot      = 0
        LET lar_tot[ls_sie].mto_pesos_par      = 0
        LET lar_tot[ls_sie].subtotal_pesos     = 0
        LET lar_tot[ls_sie].mto_acciones_isr   = 0
        LET lar_tot[ls_sie].mto_pesos_isr      = 0
    --END FOR
   END FOREACH
   FREE cur_sie


    LET lc_query = " SELECT siefore               ,     \n",
                   "        tipo_movimiento       ,     \n",
                   "        monto_en_acciones * -1,     \n",
                   "        monto_en_pesos * -1         \n",
                   " FROM   ret_preliquida              \n",
                   " WHERE  fecha_conversion = ?        \n",
                   " AND    tipo_movimiento IN (10, 817, 820, 825, 830,  \n",
                   "                            835, 840, 850, 860, 880, \n",
                   "                            870, 875, 876, 877, 878, 927,928) \n"   #MLM-3409

    PREPARE prp_mto FROM lc_query
    DECLARE cur_mto CURSOR FOR prp_mto

    FOREACH cur_mto USING pdt_fec_detalle
                    INTO  ls_sie, lr_montos.*

         
        CASE ls_sie
            WHEN gs_sieviv
                LET ls_sie = 6

            WHEN 10
                LET ls_sie = 7

            WHEN 0
                LET ls_sie = 8

        END CASE

        LET lar_tot[ls_sie].subtotal_acciones = lar_tot[ls_sie].subtotal_acciones +
                                                    lr_montos.acciones

        LET lar_tot[ls_sie].subtotal_pesos    = lar_tot[ls_sie].subtotal_pesos +
                                                    lr_montos.pesos

        CASE
            WHEN (lr_montos.tipo_mov = 817) OR
                 (lr_montos.tipo_mov = 820) OR
                 (lr_montos.tipo_mov = 825) OR
                 (lr_montos.tipo_mov = 830) OR
                 (lr_montos.tipo_mov = 835) OR
                 (lr_montos.tipo_mov = 840) OR
                 (lr_montos.tipo_mov = 850) OR
                 (lr_montos.tipo_mov = 860) OR
                 (lr_montos.tipo_mov = 880)

                -- Retiros Totales
                LET lar_tot[ls_sie].mto_acciones_tot = lar_tot[ls_sie].mto_acciones_tot +
                                                           lr_montos.acciones

                LET lar_tot[ls_sie].mto_pesos_tot    = lar_tot[ls_sie].mto_pesos_tot +
                                                           lr_montos.pesos

            WHEN (lr_montos.tipo_mov = 870) OR
                 (lr_montos.tipo_mov >= 875 AND lr_montos.tipo_mov <= 878) OR
                 (lr_montos.tipo_mov >= 927 AND lr_montos.tipo_mov <= 928)     #MLM-3409


                -- Retiros Parciales
                LET lar_tot[ls_sie].mto_acciones_par = lar_tot[ls_sie].mto_acciones_par +
                                                           lr_montos.acciones

                LET lar_tot[ls_sie].mto_pesos_par    = lar_tot[ls_sie].mto_pesos_par +
                                                           lr_montos.pesos

            WHEN lr_montos.tipo_mov = 10

                -- Retencion ISR

                LET lar_tot[ls_sie].mto_acciones_isr = lar_tot[ls_sie].mto_acciones_isr  +
                                                           lr_montos.acciones

                LET lar_tot[ls_sie].mto_pesos_isr    = lar_tot[ls_sie].mto_pesos_isr     +
                                                           lr_montos.pesos
        END CASE
      
    END FOREACH
    DECLARE cur_sie2 CURSOR FOR                       
    SELECT codigo_siefore                            
    FROM   tab_siefore_local                         
    WHERE  ind_activo =  1                           
    AND    codigo_siefore NOT IN (0,12,13)          
    ORDER  by 1                                      
                                                     
   LET ls_cont = 0
   FOREACH cur_sie2 INTO ls_sie
    
    --FOR ls_sie = 1 TO 99    #MLM-3409
      LET ls_cont = ls_cont +1
      DISPLAY "siefore ", ls_sie   , " contador ", ls_cont
         
        CASE ls_sie
            WHEN 6
                LET lar_tot[ls_cont].siefore = gs_sieviv

            WHEN 7
                LET lar_tot[ls_cont].siefore = 10

            WHEN 8
                LET lar_tot[ls_cont].siefore = 0

            OTHERWISE
                LET lar_totales[ls_cont].siefore = lar_tot[ls_sie].siefore

        END CASE
        
        LET lar_totales[ls_cont].mto_acciones_tot   = lar_tot[ls_sie].mto_acciones_tot  
        LET lar_totales[ls_cont].mto_acciones_par   = lar_tot[ls_sie].mto_acciones_par  
        LET lar_totales[ls_cont].subtotal_acciones  = lar_tot[ls_sie].subtotal_acciones 
        LET lar_totales[ls_cont].mto_pesos_tot      = lar_tot[ls_sie].mto_pesos_tot     
        LET lar_totales[ls_cont].mto_pesos_par      = lar_tot[ls_sie].mto_pesos_par     
        LET lar_totales[ls_cont].subtotal_pesos     = lar_tot[ls_sie].subtotal_pesos    
        LET lar_totales[ls_cont].mto_acciones_isr   = lar_tot[ls_sie].mto_acciones_isr  
        LET lar_totales[ls_cont].mto_pesos_isr      = lar_tot[ls_sie].mto_pesos_isr     
    --END FOR
   END FOREACH
   FREE cur_sie2

        CALL SET_COUNT(ls_cont)

    DISPLAY ARRAY lar_totales TO scr_det1.*

    CLOSE WINDOW win_detalle
    CLEAR SCREEN

END FUNCTION
