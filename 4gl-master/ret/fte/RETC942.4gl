#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC942  => PRELIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE           #
#                             ( VERSION GENERAL - AFORES )                      #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 10 DE AGOSTO DE 2010                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Se modifica la forma de elegir los folios, mostrando una  #
#                     pantalla con los folios que se encuentren listos a ser    #
#                     preliquidados.                                            #
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos de              #
#                     preliquidacion de retiros totales ISSSTE                  #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        enviado     LIKE ret_estado_issste.estado_solicitud ,
        recibido    LIKE ret_estado_issste.estado_solicitud ,
        pre_liq     LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_mov_parcial        ,
        gs_peiss              ,
        gs_xxi                ,
        gs_flag               ,
        gs_flag_err           ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_cod_afore          SMALLINT

    DEFINE #glo #integer
        gi_proceso            ,
        gs_ult_folio          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC942")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC942    PRELIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "PRELIQUIDACION"
        COMMAND "Preliquida" "Ejecuta la preliquidacion de Retiros ISSSTE"
            CALL f_genera_preliquida()

        COMMAND "Detalle X Siefore" "Muestra el detalle de montos preliquidados por siefore"
            CALL f_genera_detalle(HOY)

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Preliquidacion"
            CALL f_bitacora_err(0)

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    DEFINE
        ls_dias_hab     SMALLINT

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gs_sieviv           = 12
    LET ls_dias_hab         = 2
    LET gs_num_siefores     = f_lib_obtiene_num_siefores()
    LET gc_usuario          = f_lib_obten_user()

    ----- TIPOS DE MOVIMIENTO -----
    SELECT movimiento
    INTO   gs_mov_parcial
    FROM   tab_ret_issste
    WHERE  tipo_retiro  = "F"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_xxi
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*AFORE XXI*"

    SELECT afore_cod
    INTO   gs_peiss
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*PENSIONISSSTE*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.pre_liq
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PRELIQUIDADO"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia_isss (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- RETENCION DE ISR -----
    LET lc_prepare = "EXECUTE FUNCTION fn_ret_isr_issste (?,?,?,?) "
    PREPARE eje_ret_isr FROM lc_prepare

    LET lc_prepare = " "

    ----- PROPORCIONAL DE PARCIALES -----
    LET lc_prepare = "EXECUTE FUNCTION fn_calcula_prop_parcial(?,?,?,?) "
    PREPARE eje_prop_par FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_preliquida : Ejecuta los pasos necesarios para realizar la       #
#                       preliquidacion de los procesos de retiros issste    #
#---------------------------------------------------------------------------#
FUNCTION f_genera_preliquida()

    -- -----------------------------------------------------------------------------
    
    CALL f_despliega_info() RETURNING gs_flag, gr_folios.*

    IF gs_flag THEN
        CALL f_obtiene_precios_accion(gr_folios.fec_prel)
        CALL f_abre_ventana()

        CALL primer_paso(gr_folios.*)   #-- Realiza la preliquidacion de los montos

        CALL segundo_paso(gr_folios.*)  #-- Valida la informacion de los montos
            RETURNING gs_flag_err, gi_proceso

        IF gs_flag_err = 0 THEN
            CALL tercer_paso(gr_folios.*)   #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO")
            CLOSE WINDOW RETC9422
            CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    preliquidacion de los procesos de retiros issste       #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lr_info RECORD
        fecha_preliq        DATE    ,
        sel_tot             CHAR    ,
        sel_par             CHAR    ,
        sel_tran            CHAR    ,
        folio_tot           INTEGER ,
        folio_par           INTEGER ,
        folio_tran          INTEGER
    END RECORD

    DEFINE lr_folios RECORD
        disp                INTEGER ,
        trans               INTEGER ,
        par                 INTEGER
    END RECORD

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc9421 AT 4,4 WITH FORM "RETC9421" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC942       PRELIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    LET ls_flag                 = 1
    LET lr_info.fecha_preliq    = HOY
    INITIALIZE lr_folios.* TO NULL

    -- DISPOSICIONES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.disp
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.recibido

    IF lr_folios.disp IS NULL THEN
        LET lr_folios.disp = 0
    END IF

    -- TRANSFERENCIAS
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.trans
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.enviado

    IF lr_folios.trans IS NULL THEN
        LET lr_folios.trans = 0
    END IF

    -- PARCIALES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.par
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.recibido

    IF lr_folios.par IS NULL THEN
        LET lr_folios.par = 0
    END IF

    INPUT BY NAME lr_info.* WITHOUT DEFAULTS

        BEFORE INPUT
            IF (lr_folios.disp = 0) AND (lr_folios.trans = 0) AND (lr_folios.par = 0) THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA LIQUIDAR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD fecha_preliq
            IF lr_info.fecha_preliq IS NULL THEN
                ERROR " LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA ..."
                SLEEP 2
                ERROR " "
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
            IF lr_info.sel_par = "x" THEN
                CALL f_muestra_folios("par", lr_folios.par) RETURNING lr_info.folio_par
                DISPLAY BY NAME lr_info.folio_par

                IF lr_info.folio_par = 0 THEN
                    LET lr_info.sel_par = " "
                    DISPLAY BY NAME lr_info.sel_par
                END IF
            END IF

        AFTER FIELD sel_tran
            IF lr_info.sel_tran = "x" THEN
                CALL f_muestra_folios("tra", lr_folios.trans) RETURNING lr_info.folio_tran
                DISPLAY BY NAME lr_info.folio_tran

                IF lr_info.folio_tran = 0 THEN
                    LET lr_info.sel_tran = " "
                    DISPLAY BY NAME lr_info.sel_tran
                END IF
            END IF

        ON KEY (CONTROL-C)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
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

            IF lr_info.sel_tran IS NULL THEN
                LET lr_info.sel_tran = " "
            END IF

            IF lr_info.sel_tot <> "x" THEN
                LET lr_info.folio_tot = 0
            END IF

            IF lr_info.sel_par <> "x" THEN
                LET lr_info.folio_par = 0
            END IF

            IF lr_info.sel_tran <> "x" THEN
                LET lr_info.folio_tran = 0
            END IF

            IF (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") AND (lr_info.sel_tran <> "x") THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA LIQUIDAR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

            IF lr_info.fecha_preliq IS NULL THEN
                CALL f_lib_error_msg("LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_preliq
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET ls_flag = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

    END INPUT

    CLOSE WINDOW retc9421

    RETURN ls_flag              ,
           lr_info.fecha_preliq ,
           lr_info.folio_tot    ,
           lr_info.folio_par    ,
           lr_info.folio_tran

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Llama a las funciones que realizan los procesos de          #
#               preliquidacion                                              #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    CALL f_tablas_tmp()

    IF pr_folios.total <> 0 THEN
        CALL f_preliquida_tot(pr_folios.total, pr_folios.fec_prel)      #-- Preliquidacion de retiros totales
    END IF

    IF pr_folios.transfer <> 0 THEN
        CALL f_preliquida_trans(pr_folios.transfer, pr_folios.fec_prel)   #-- Preliquidacion de transferencias
    END IF

    IF pr_folios.parcial <> 0 THEN
        CALL f_preliquida_par(pr_folios.parcial, pr_folios.fec_prel)   #-- Preliquidacion de retiros parciales
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida los montos preliquidados                            #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DEFINE li_idproc LIKE ret_bitacora_error.id_proceso

    DEFINE
        ls_flag         SMALLINT

    LET ls_flag = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_idproc = f_ultimo_id_err()

    ----------  Valida sobregiro de los retiros Totales     --------------
    CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.total, pr_folios.fec_prel, "tot")
        RETURNING ls_flag

    ----------  Valida sobregiro de las transferencias      --------------
    CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.transfer, pr_folios.fec_prel, "tra")
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
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DISPLAY "                                             " AT 18,1

    --- Preliquidacion de totales
    IF pr_folios.total <> 0 THEN
        INSERT INTO ret_preliquida
        SELECT *
        FROM   tmp_preliquida
        WHERE  folio = pr_folios.total

        CALL f_act_estado_sol(pr_folios.total, "tot")
    END IF

    --- Preliquidacion de Transferencias
    IF pr_folios.transfer <> 0 THEN
        INSERT INTO ret_preliquida
        SELECT *
        FROM   tmp_preliquida
        WHERE  folio = pr_folios.transfer

        CALL f_act_estado_sol(pr_folios.transfer, "tra")
    END IF

    --- Preliquidacion de parciales
    IF pr_folios.parcial <> 0 THEN

        INSERT INTO ret_monto_issste
        SELECT *
        FROM   tmp_monto_issste
        WHERE  folio = pr_folios.parcial

        INSERT INTO ret_preliquida
        SELECT *
        FROM   tmp_preliquida
        WHERE  folio = pr_folios.parcial

        CALL f_act_estado_sol(pr_folios.parcial, "par")
    END IF


    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")
    CLOSE WINDOW RETC9422
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
        folio       INTEGER
    END RECORD

    DEFINE
        lc_proceso      CHAR(015),
        lc_tabla        CHAR(100),
        lc_prepare      CHAR(500)

    DEFINE
        ls_estado           ,
        ls_pos              SMALLINT

    DEFINE
        pi_numfol           ,
        li_folio            INTEGER

    LET li_folio = 0

    CASE pc_modalidad
        WHEN "dis"
            LET lc_tabla    = " FROM ret_sol_issste_tx "
            LET ls_estado   = gr_edo.recibido
            LET lc_proceso  = "DISPOSICIONES"

        WHEN "tra"
            LET lc_tabla    = " FROM ret_trans_issste "
            LET ls_estado   = gr_edo.enviado
            LET lc_proceso  = "TRANSFERENCIAS"

        WHEN "par"
            LET lc_tabla    = " FROM ret_parcial_issste "
            LET ls_estado   = gr_edo.recibido
            LET lc_proceso  = "PARCIALES"
    END CASE

    LET lc_prepare = " SELECT UNIQUE(folio)  "      ,
                     lc_tabla                       ,
                     " WHERE  estado_solicitud = ? ",
                     " ORDER BY 1 "

    PREPARE prp_dat FROM lc_prepare

    IF pi_numfol > 0 THEN
        OPEN WINDOW retc9425 AT 14,62 WITH FORM "RETC9425" ATTRIBUTE(BORDER)

        WHILE TRUE
            DECLARE cur_dat CURSOR FOR prp_dat

            LET ls_pos = 1

            FOREACH cur_dat USING ls_estado
                            INTO lr_reg[ls_pos].*

                LET ls_pos = ls_pos + 1
                IF ls_pos >= 10 THEN
                    ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                    ATTRIBUTE(NORMAL)
                    EXIT FOREACH
                END IF
            END FOREACH

            CALL SET_COUNT(ls_pos-1)
            DISPLAY ARRAY lr_reg TO scr_2.*

                ON KEY (CONTROL-C, INTERRUPT)
                    LET ls_pos = 0
                    EXIT DISPLAY

                ON KEY ( CONTROL-M )
                    LET ls_pos      = ARR_CURR()
                    LET li_folio    = lr_reg[ls_pos].folio
                    EXIT DISPLAY

            END DISPLAY

            IF ls_pos <> 0 THEN
                EXIT WHILE
            END IF

        END WHILE

        CLOSE WINDOW retc9425
    ELSE
        LET lc_tabla = "NO HAY FOLIOS PARA ", lc_proceso CLIPPED
        CALL f_lib_error_msg(lc_tabla)
    END IF

    RETURN li_folio
END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_tot : Valida y realiza la preliquidacion para disposiciones  #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_tot(pi_folio, pdt_fecha_pre)

    DEFINE pi_folio LIKE ret_sol_issste_tx.folio

    DEFINE lr_provi,
           lr_isr   RECORD LIKE dis_cuenta.*

    DEFINE lr_reten RECORD
        mto_neto    LIKE dis_provision.monto_en_pesos,
        mto_isr     LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE ls_diag_procesar LIKE ret_sol_issste_tx.diag_procesar
    DEFINE ls_regimen       LIKE ret_sol_issste_tx.regimen
    DEFINE ls_diag_viv      LIKE ret_monto_viv_issste.estado_sub_viv

    DEFINE #loc #date
        pdt_fecha_pre           ,
        ld_fecha_saldo          DATE

    DEFINE #loc #smallint
        ls_flag                 ,
        ls_tipo_mov             ,
        ls_siefore              , #-- contador para los ciclos for
        ls_subcta               SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "PRELIQUIDANDO RETIROS TOTALES ..." AT 4,5

    DECLARE cur_prov CURSOR FOR
    SELECT B.*             ,
           A.diag_procesar ,
           A.regimen       ,
           C.estado_sub_viv
    FROM   ret_sol_issste_tx A  ,
           dis_provision B      ,
           ret_monto_viv_issste C
    WHERE A.folio            = pi_folio
    AND   A.folio            = B.folio
    AND   A.curp             = B.curp
    AND   A.consecutivo      = B.consecutivo_lote
    AND   A.estado_solicitud = gr_edo.recibido
    AND   B.folio            = C.folio
    AND   B.curp             = C.curp
    AND   B.consecutivo_lote = C.consecutivo
    ORDER BY B.curp, B.subcuenta

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_provi.*, ls_diag_procesar, ls_regimen, ls_diag_viv

        LET ls_flag                     = 0
        LET lr_provi.fecha_pago         = pdt_fecha_pre
        LET lr_provi.fecha_conversion   = pdt_fecha_pre
        LET lr_provi.fecha_archivo      = pdt_fecha_pre
        LET lr_provi.fecha_proceso      = pdt_fecha_pre
        LET lr_provi.usuario            = gc_usuario

        -- Preliquidamos subcuentas de vivienda
        IF (lr_provi.subcuenta = 14) OR (lr_provi.subcuenta = 35) THEN

            IF ls_diag_procesar = 400 AND ls_diag_viv = 1 THEN

                LET ls_flag = 1

                IF (gs_cod_afore = gs_peiss) AND (ls_regimen = "DT") THEN
                    LET lr_provi.fecha_pago         = lr_provi.fecha_valor
                    LET lr_provi.fecha_conversion   = lr_provi.fecha_valor
                END IF

                IF (gs_cod_afore <> gs_peiss) OR ((gs_cod_afore = gs_peiss) AND (ls_regimen = "RO")) THEN
                    IF lr_provi.fecha_valor <> pdt_fecha_pre THEN
                        LET ls_siefore              = lr_provi.siefore
                        LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
                        LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
                        LET lr_provi.fecha_valor    = pdt_fecha_pre
                    END IF
                END IF

                IF gs_cod_afore = gs_xxi THEN
                    LET lr_provi.monto_en_pesos = f_lib_redondea_val(lr_provi.monto_en_pesos, 2)
                END IF
            ELSE
                -- Verificamos si el diagnostico recibido se acepta o no
                SELECT id_aceptado
                INTO   ls_flag
                FROM   tab_diag_procesar_disp
                WHERE  diag_procesar    = ls_diag_procesar

                LET lr_provi.monto_en_pesos     = 0
                LET lr_provi.monto_en_acciones  = 0
            END IF
        ELSE
            -- Preliquidamos el resto de las subcuentas
            LET ls_flag                 = 1
            LET ls_siefore              = lr_provi.siefore

            IF ls_siefore <> 0 THEN
                LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
                LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
                LET lr_provi.fecha_valor    = pdt_fecha_pre
            ELSE
                LET lr_provi.precio_accion  = 0

                IF (gs_cod_afore = gs_peiss) AND (ls_regimen = "DT") THEN
                    LET lr_provi.fecha_pago         = lr_provi.fecha_valor
                    LET lr_provi.fecha_conversion   = lr_provi.fecha_valor
                END IF
            END IF

            IF gs_cod_afore <> gs_peiss THEN
                -- Se calcula la retencion de ISR
                IF lr_provi.subcuenta = 13 THEN

                    LET lr_isr.* = lr_provi.*

                    LET lr_isr.monto_en_pesos = lr_isr.monto_en_pesos * -1

                    EXECUTE eje_ret_isr USING lr_provi.nss              ,
                                              lr_provi.consecutivo_lote ,
                                              lr_isr.monto_en_pesos     ,
                                              lr_isr.monto_en_pesos
                                        INTO  lr_reten.*

                    IF lr_reten.mto_isr > 0 THEN

                        -- si encontro una retencion, introducimos el monto neto en la temporal
                        -- de preliquidacion y generamos un registro para la retencion
                        LET lr_isr.monto_en_pesos    = lr_reten.mto_isr * -1

                        LET lr_isr.monto_en_acciones = lr_isr.monto_en_pesos / lr_isr.precio_accion
                        LET lr_isr.tipo_movimiento   = 10

                        INSERT INTO tmp_preliquida
                        VALUES (lr_isr.*)

                        LET lr_provi.monto_en_pesos    = lr_reten.mto_neto * -1
                        LET lr_provi.monto_en_acciones = lr_provi.monto_en_pesos / lr_provi.precio_accion

                    END IF -- mto isr > 0
                END IF -- sub 13
            END IF -- Afore diferente a peisss
        END IF

        IF ls_flag THEN
            INSERT INTO tmp_preliquida
            VALUES (lr_provi.*)
        END IF

    END FOREACH -- Siguiente registro

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_trans : Valida y realiza la preliquidacion de transferencias #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_trans(pi_folio, pdt_fecha_pre)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE lr_provi,
           lr_isr   RECORD LIKE dis_cuenta.*

    DEFINE lr_reten RECORD
        mto_neto    LIKE dis_provision.monto_en_pesos,
        mto_isr     LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE ls_diag_procesar     LIKE ret_trans_issste.diag_procesar
    DEFINE ls_diag_viv          LIKE ret_monto_viv_issste.estado_sub_viv
    DEFINE ld_monto_recibido    LIKE ret_trans_issste.mto_solic_issste
    DEFINE ld_monto_calculado   LIKE ret_trans_issste.mto_const_calculado


    DEFINE #loc #date
        pdt_fecha_pre           ,
        ld_fecha_saldo          DATE

    DEFINE #loc #smallint
        ls_flag                 ,
        ls_tipo_mov             ,
        ls_siefore              , #-- contador para los ciclos for
        ls_subcta               SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "PRELIQUIDANDO TRANSFERENCIAS  ..." AT 10,5

    DECLARE cur_trans CURSOR FOR
    SELECT B.*                   ,
           A.diag_procesar       ,
           A.mto_solic_issste    ,
           A.mto_const_calculado ,
           C.estado_sub_viv
    FROM   ret_trans_issste A   ,
           dis_provision B      ,
           ret_monto_viv_issste C
    WHERE A.folio            = pi_folio
    AND   A.folio            = B.folio
    AND   A.curp             = B.curp
    AND   A.consecutivo      = B.consecutivo_lote
    AND   A.estado_solicitud = gr_edo.enviado
    AND   B.folio            = C.folio
    AND   B.curp             = C.curp
    AND   B.consecutivo_lote = C.consecutivo
    ORDER BY B.curp, B.subcuenta

    -- Iniciamos ciclo para cada nss
    FOREACH cur_trans INTO lr_provi.*           ,
                           ls_diag_procesar     ,
                           ld_monto_recibido    ,
                           ld_monto_calculado   ,
                           ls_diag_viv

        LET ls_flag                     = 0
        LET lr_provi.usuario            = gc_usuario
        LET lr_provi.fecha_archivo      = pdt_fecha_pre
        LET lr_provi.fecha_proceso      = pdt_fecha_pre

        -- En el caso de las afores se recalcula los montos al dia de preliquidacion
        IF gs_cod_afore <> gs_peiss THEN

            LET lr_provi.fecha_pago         = pdt_fecha_pre
            LET lr_provi.fecha_conversion   = pdt_fecha_pre

            -- Preliquidamos subcuentas de vivienda
            IF (lr_provi.subcuenta = 14) OR (lr_provi.subcuenta = 35) THEN
                LET ls_flag = 1

                IF lr_provi.fecha_valor <> pdt_fecha_pre THEN
                    LET ls_siefore              = lr_provi.siefore
                    LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
                    LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
                    LET lr_provi.fecha_valor    = pdt_fecha_pre
                END IF

                IF gs_cod_afore = gs_xxi THEN
                    LET lr_provi.monto_en_pesos = f_lib_redondea_val(lr_provi.monto_en_pesos, 2)
                END IF
            ELSE
                -- Preliquidamos el resto de las subcuentas
                LET ls_flag                 = 1
                LET ls_siefore              = lr_provi.siefore
                LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
                LET lr_provi.fecha_valor    = pdt_fecha_pre

                IF ld_monto_recibido <= ld_monto_calculado THEN
                    -- Si pagamos el monto constitutivo nos aseguramos de no rebasar dicho monto en pesos
                    LET lr_provi.monto_en_acciones = lr_provi.monto_en_pesos/lr_provi.precio_accion
                ELSE
                    -- Si pagamos el saldo de la cuenta nos aseguramos de pagar lo provisionado en acciones
                    LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
                END IF
            END IF
        ELSE
            -- En el caso de Pension ISSSTE se guarda la provision sin cambios
            LET ls_flag = 1
        END IF

        IF ls_flag THEN
            INSERT INTO tmp_preliquida
            VALUES (lr_provi.*)
        END IF

    END FOREACH -- Siguiente registro

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_par : Valida y realiza la preliquidacion de retiros parciales#
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_par(pi_folio, pdt_fecha_pre)

    DEFINE pi_folio LIKE ret_parcial_issste.folio

    DEFINE
        pdt_fecha_pre           DATE

    DEFINE lr_datos_par RECORD
        curp                LIKE ret_parcial_issste.curp           ,
        nss                 LIKE ret_parcial_issste.nss            ,
        consec              LIKE ret_parcial_issste.consecutivo    ,
        grupo               LIKE ret_parcial_issste.grupo          ,
        mto_pago            LIKE ret_monto_par_issste.mto_a_pagar
    END RECORD

    DEFINE lr_pago RECORD
        subcta              SMALLINT      ,
        siefore             SMALLINT      ,
        saldo_acc           DECIMAL(16,6) ,
        acc_pagar           DECIMAL(16,6) ,
        pesos_pagar         DECIMAL(16,6)
    END RECORD

    DEFINE lr_montos RECORD
        acciones_ret08      DECIMAL(16,6) ,
        acciones_cv_tot     DECIMAL(16,6) ,
        acciones_sar92      DECIMAL(16,6) ,
        pesos_banxico       DECIMAL(16,6)
    END RECORD

    DEFINE
        ls_siefore              SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "PRELIQUIDANDO RETIROS PARCIALES  ..." AT 16,5

    DECLARE cur_par CURSOR FOR
    SELECT A.curp,
           A.nss,
           A.consecutivo,
           A.grupo,
           B.mto_a_pagar
    FROM   ret_parcial_issste A ,
           ret_monto_par_issste B
    WHERE  A.folio              = pi_folio
    AND    A.folio              = B.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.estado_solicitud   = gr_edo.recibido
    ORDER BY B.curp

    -- Iniciamos ciclo para cada nss
    FOREACH cur_par INTO lr_datos_par.*

        LET lr_pago.saldo_acc           = 0
        LET lr_pago.acc_pagar           = 0
        LET lr_pago.pesos_pagar         = 0
        LET lr_montos.acciones_ret08    = 0
        LET lr_montos.acciones_cv_tot   = 0
        LET lr_montos.acciones_sar92    = 0
        LET lr_montos.pesos_banxico     = 0

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_liq CURSOR FOR eje_prop_par
        FOREACH cur_liq USING lr_datos_par.nss             ,
                              lr_datos_par.grupo           ,
                              lr_datos_par.mto_pago        ,
                              pdt_fecha_pre
                        INTO  lr_pago.*

            -- Actualizamos los valores para insertar a preliquida

            IF lr_pago.siefore <> 0 THEN
                LET ls_siefore = lr_pago.siefore
            END IF

            CALL f_act_preliquida_par(pi_folio              ,
                                      lr_datos_par.curp     ,
                                      lr_datos_par.nss      ,
                                      lr_pago.subcta        ,
                                      lr_datos_par.consec   ,
                                      lr_pago.acc_pagar     ,
                                      lr_pago.pesos_pagar   ,
                                      pdt_fecha_pre         ,
                                      gs_mov_parcial        ,
                                      lr_pago.siefore
                                      )

            -- Guardamos los valores para tmp_monto_issste
            CASE lr_pago.subcta
                -- Para DT
                WHEN 13
                    LET lr_montos.acciones_sar92 = lr_montos.acciones_sar92 + lr_pago.acc_pagar
                WHEN 19
                    LET lr_montos.pesos_banxico = lr_montos.pesos_banxico + lr_pago.pesos_pagar

                -- Para RO
                WHEN 30
                    LET lr_montos.acciones_ret08  = lr_montos.acciones_ret08  + lr_pago.acc_pagar
                WHEN 31
                    LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                WHEN 32
                    LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
            END CASE

        END FOREACH -- Siguiente subcuenta

        INSERT INTO tmp_monto_issste
        VALUES (lr_datos_par.curp           , -- curp
                lr_datos_par.consec         , -- consecutivo
                pi_folio                    , -- folio
                "F"                         , -- tipo_retiro
                56                          , -- cve_operacion
                ls_siefore                  , -- siefore
                lr_montos.acciones_ret08    , -- acc_ret08
                lr_montos.acciones_cv_tot   , -- acc_cv
                0                           , -- acc_ahorro_sol
                lr_montos.acciones_sar92    , -- acc_ret92
                0                           , -- acc_comp_ret
                lr_montos.pesos_banxico       -- pes_banxico
                )

    END FOREACH -- Siguiente nss

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  preliquidacion de retiros issste                         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9422 AT 4,4 WITH FORM "RETC9422" ATTRIBUTE(BORDER)
    DISPLAY "                                                             RETIROS ISSSTE " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC942      PRELIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usan en el proceso       #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_preliquida
        DROP TABLE tmp_monto_issste
    WHENEVER ERROR STOP

    SELECT *
    FROM   dis_cuenta
    WHERE  0 = 1
    INTO TEMP tmp_preliquida

    SELECT *
    FROM   ret_monto_issste
    WHERE  0 = 1
    INTO TEMP tmp_monto_issste

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_sobregiro : Valida que no exista sobregiro en las subcuentas   #
#                        que se provisionaron                               #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_sobregiro(pr_valida)

    DEFINE pr_valida RECORD
        bandera             SMALLINT                    ,
        id_proc             SMALLINT                    ,
        folio               LIKE ret_sol_issste_tx.folio,
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
        lr_saldo_prov, lr_saldo_dia RECORD
            nss             LIKE ret_sol_issste_tx.nss           ,
            subcta          SMALLINT                             ,
            monto_acc       LIKE dis_provision.monto_en_acciones ,
            monto_pes       LIKE dis_provision.monto_en_acciones
        END RECORD

    DEFINE ld_curp  LIKE dis_provision.curp

    DEFINE
        ls_error                ,
        ls_grupo                SMALLINT

    -- -----------------------------------------------------------------------------

    CASE pr_valida.tipo
        WHEN "tot"
            LET lr_error.tipo_campo   = "RETIRO TOTAL ISSSTE"

        WHEN "par"
            LET lr_error.tipo_campo   = "RETIRO PARCIAL ISSSTE"

        WHEN "tra"
            LET lr_error.tipo_campo   = "TRANSFERENCIAS ISSSTE"
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
           monto_en_acciones ,
           monto_en_pesos
    FROM   tmp_preliquida
    WHERE  folio    = pr_valida.folio
    ORDER BY 1,3

    FOREACH cur_tmp INTO ld_curp, lr_saldo_prov.*


        LET lr_error.nss          = lr_saldo_prov.nss
        LET lr_error.curp         = ld_curp

        DECLARE cur_val_sobre CURSOR FOR eje_saldo_dia

        OPEN cur_val_sobre USING lr_saldo_prov.nss      ,
                                 lr_saldo_prov.subcta   ,
                                 ls_grupo               ,
                                 pr_valida.fec_preliq

        FETCH cur_val_sobre INTO lr_saldo_dia.*

        CLOSE cur_val_sobre

        -- Si el monto de provisionado es mayor al saldo al dia entonces registra
        -- un sobregiro

        LET lr_saldo_prov.monto_acc = lr_saldo_prov.monto_acc * -1

        IF ( (lr_saldo_prov.subcta = 19) AND
             (f_lib_redondea_val(lr_saldo_prov.monto_pes,2) > f_lib_redondea_val(lr_saldo_dia.monto_pes,2)) )
           OR
           ( (lr_saldo_prov.subcta <> 19) AND
             (f_lib_redondea_val(lr_saldo_prov.monto_acc,2) > f_lib_redondea_val(lr_saldo_dia.monto_acc,2)) ) THEN

            LET lr_error.nom_campo    = "Sobregiro Subcta ", lr_saldo_prov.subcta


            IF lr_saldo_prov.subcta <> 19 THEN
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
# f_act_preliquida_par : Inserta los valores en la tabla tmp_preliquida     #
#                        para los retiros parciales                         #
#---------------------------------------------------------------------------#
FUNCTION f_act_preliquida_par(pr_preliq, ps_sie)

    DEFINE pr_preliq RECORD
        folio       LIKE ret_parcial_issste.curp         ,
        curp        LIKE ret_parcial_issste.curp         ,
        nss         LIKE ret_parcial_issste.nss          ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_parcial_issste.consecutivo  ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE                                ,
        tipo_mov    SMALLINT
    END RECORD

    DEFINE
        ps_sie              SMALLINT

    DEFINE lr_preliquida RECORD LIKE ret_preliquida.*

    DEFINE ld_precio_acc LIKE dis_provision.precio_accion

    -- -----------------------------------------------------------------------------

    IF ps_sie > 0 THEN
        LET ld_precio_acc = gar_precio_acc[ps_sie].precio_dia
    ELSE
        LET ld_precio_acc       = 0
        LET pr_preliq.acciones  = 0
    END IF

    LET lr_preliquida.tipo_movimiento       =  pr_preliq.tipo_mov  
    LET lr_preliquida.subcuenta             =  pr_preliq.subcta    
    LET lr_preliquida.siefore               =  ps_sie              
    LET lr_preliquida.folio                 =  pr_preliq.folio     
    LET lr_preliquida.consecutivo_lote      =  pr_preliq.consec     
    LET lr_preliquida.nss                   =  pr_preliq.nss       
    LET lr_preliquida.curp                  =  pr_preliq.curp      
    LET lr_preliquida.folio_sua             =  NULL                
    LET lr_preliquida.fecha_pago            =  HOY                 
    LET lr_preliquida.fecha_valor           =  pr_preliq.fecha_proc        
    LET lr_preliquida.fecha_conversion      =  HOY                  
    LET lr_preliquida.monto_en_pesos        =  pr_preliq.pesos * -1
    LET lr_preliquida.monto_en_acciones     =  pr_preliq.acciones * -1                
    LET lr_preliquida.precio_accion         =  ld_precio_acc       
    LET lr_preliquida.dias_cotizados        =  0                   
    LET lr_preliquida.sucursal              =  ""                  
    LET lr_preliquida.id_aportante          =  "RETIRO"           
    LET lr_preliquida.estado                =  6                   
    LET lr_preliquida.fecha_proceso         =  HOY                 
    LET lr_preliquida.usuario               =  gc_usuario          
    LET lr_preliquida.fecha_archivo         =  HOY                 
    LET lr_preliquida.etiqueta              =  1                   
 
    INSERT INTO safre_tmp:tmp_preliquida
    VALUES (lr_preliquida.*)

END FUNCTION


#---------------------------------------------------------------------------#
# f_act_estado_sol : Actualiza los estados de solicitud de acuerdo al tipo  #
#                   de retiro issste                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_estado_sol(pr_folio, pc_id_oper)

    DEFINE pr_folio LIKE ret_sol_issste_tx.folio

    DEFINE
        pc_id_oper      CHAR(3)

    DEFINE la_rets ARRAY[7] OF INTEGER

    DEFINE lr_datos RECORD
        curp        LIKE dis_provision.curp             ,
        tipo_mov    LIKE dis_provision.tipo_movimiento  ,
        consec      LIKE dis_provision.consecutivo_lote
    END RECORD

    DEFINE
        ls_cont     SMALLINT

    -- -----------------------------------------------------------------------------

    FOR ls_cont = 1 TO 7
        LET la_rets[ls_cont] = 0
    END FOR

    DECLARE cur_pre CURSOR FOR
    SELECT UNIQUE(curp)    ,
           tipo_movimiento ,
           consecutivo_lote
    FROM   ret_preliquida
    WHERE  folio = pr_folio
    ORDER BY 1

    -- Actualiza el estado de la solicitud de los registros preliquidados
    CASE pc_id_oper

        ---- RETIROS TOTALES ----
        WHEN "tot"
            DISPLAY "FOLIO    : ", pr_folio AT 5,12

            FOREACH cur_pre INTO lr_datos.*
                UPDATE ret_sol_issste_tx
                SET    estado_solicitud = gr_edo.pre_liq
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.recibido

                CASE lr_datos.tipo_mov
                    WHEN 851
                        LET la_rets[1] = la_rets[1] + 1
                    WHEN 852
                        SELECT "OK"
                        FROM   ret_sol_issste_tx
                        WHERE  tipo_retiro      = "I"
                        AND    estado_solicitud = gr_edo.pre_liq
                        AND    curp             = lr_datos.curp
                        AND    consecutivo      = lr_datos.consec
                        GROUP BY 1
                        
                        IF STATUS = NOTFOUND THEN
                            LET la_rets[2] = la_rets[2] + 1
                        ELSE
                            LET la_rets[6] = la_rets[6] + 1
                        END IF
                        
                    WHEN 853
                        LET la_rets[3] = la_rets[3] + 1
                    WHEN 854
                        LET la_rets[4] = la_rets[4] + 1
                    WHEN 855
                        LET la_rets[5] = la_rets[5] + 1
                    WHEN 862 -- Tipo I para Transferencias (Invercap)
                        LET la_rets[6] = la_rets[6] + 1
                    WHEN 864 -- Tipo I para Disposiciones (Metlife)
                        LET la_rets[6] = la_rets[6] + 1
                    WHEN 858
                        LET la_rets[7] = la_rets[7] + 1

                END CASE

                DISPLAY "RETIRO A : ", la_rets[1] AT 6,12
                DISPLAY "RETIRO B : ", la_rets[2] AT 7,12
                DISPLAY "RETIRO C : ", la_rets[3] AT 8,12
                DISPLAY "RETIRO D : ", la_rets[4] AT 6,45
                DISPLAY "RETIRO E : ", la_rets[5] AT 7,45
                DISPLAY "RETIRO I : ", la_rets[6] AT 8,45
                DISPLAY "RETIRO K : ", la_rets[7] AT 9,45

            END FOREACH

            DISPLAY "(TERMINADO)" AT 4,41

        ---- TRANSFERENCIAS ----
        WHEN "tra"
            DISPLAY "FOLIO    : ", pr_folio AT 11,12

            FOREACH cur_pre INTO lr_datos.*
                UPDATE ret_trans_issste
                SET    estado_solicitud = gr_edo.pre_liq
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.enviado

                CASE lr_datos.tipo_mov
                    WHEN 861
                        LET la_rets[1] = la_rets[1] + 1
                    WHEN 862
                        LET la_rets[2] = la_rets[2] + 1
                    WHEN 863
                        LET la_rets[3] = la_rets[3] + 1
                    WHEN 865
                        LET la_rets[4] = la_rets[4] + 1
                    WHEN 866
                        LET la_rets[5] = la_rets[5] + 1
                END CASE

                DISPLAY "RETIRO H : ", la_rets[1] AT 12,12
                DISPLAY "RETIRO I : ", la_rets[2] AT 13,12
                DISPLAY "RETIRO J : ", la_rets[3] AT 12,45
                DISPLAY "RETIRO L : ", la_rets[4] AT 13,45
                DISPLAY "RETIRO N : ", la_rets[5] AT 14,45

            END FOREACH

            DISPLAY "(TERMINADO)" AT 10,41

        ---- RETIROS PARCIALES ----
        WHEN "par"
            DISPLAY "FOLIO    : ", pr_folio AT 17,12

            FOREACH cur_pre INTO lr_datos.*
                UPDATE ret_parcial_issste
                SET    estado_solicitud = gr_edo.pre_liq
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.recibido

                CASE lr_datos.tipo_mov
                    WHEN gs_mov_parcial
                        LET la_rets[1] = la_rets[1] + 1
                END CASE

                DISPLAY "RETIRO F : ", la_rets[1] AT 17,45

            END FOREACH

            DISPLAY "(TERMINADO)" AT 16,42

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

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
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
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso para insertar#
#                   los registros en la bitacora de errores                 #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_id_err()

    DEFINE
        li_iderr        INTEGER

    SELECT MAX(id_proceso) + 1
    INTO   li_iderr
    FROM   ret_bitacora_error

    IF li_iderr IS NULL THEN
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

    LET lc_hora = TIME

    -- Campos generales
    LET lr_bitacora.programa    = "RETC942"
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

    OPEN WINDOW retc9423 AT 4,4 WITH FORM "RETC9423" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC942      BITACORA DE ERRORES DE PRELIQUIDACION ISSSTE                     " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    -- El proceso viene de la carga de archivo, por lo que se hace directa
    -- la carga del arreglo mediante el id de proceso
    IF pi_proceso <> 0 THEN
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

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc9423
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
                         " AND    programa = 'RETC942' ",
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

        IF (li_pos - 1) >= 1 THEN
            CALL SET_COUNT(li_pos - 1)

            DISPLAY ARRAY lar_bitacora TO scr_err.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLEAR SCREEN
            CLOSE WINDOW retc9423

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc9423
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_detalle : Genera la pantalla de detalle de montos preliquidados  #
#                    de la fecha en curso                                   #
#---------------------------------------------------------------------------#
FUNCTION f_genera_detalle(pdt_fec_detalle)

    DEFINE
        pdt_fec_detalle         DATE

    DEFINE lar_totales ARRAY[8] OF RECORD
        siefore              LIKE dis_cuenta.siefore           ,
        mto_acciones_tot     LIKE dis_cuenta.monto_en_acciones ,
        mto_acciones_par     LIKE dis_cuenta.monto_en_acciones ,
        mto_acciones_tran    LIKE dis_cuenta.monto_en_acciones ,
        mto_acciones_isr     LIKE dis_cuenta.monto_en_pesos    ,
        subtotal_acciones    LIKE dis_cuenta.monto_en_acciones ,
        mto_pesos_tot        LIKE dis_cuenta.monto_en_pesos    ,
        mto_pesos_par        LIKE dis_cuenta.monto_en_pesos    ,
        mto_pesos_tran       LIKE dis_cuenta.monto_en_pesos    ,
        mto_pesos_isr        LIKE dis_cuenta.monto_en_pesos    ,
        subtotal_pesos       LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_montos RECORD
        tipo_mov        LIKE dis_cuenta.tipo_movimiento   ,
        acciones        LIKE dis_cuenta.monto_en_acciones ,
        pesos           LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        ls_sie              SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    OPEN WINDOW retc9424 AT 4,4 WITH FORM "RETC9424" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC942 DETALLE POR MODALIDAD DE RETIRO Y SIEFORE DE SOLICITUDES             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    FOR ls_sie = 1 TO 8

        CASE ls_sie
            WHEN 6
                LET lar_totales[ls_sie].siefore = gs_sieviv

            WHEN 7
                LET lar_totales[ls_sie].siefore = 10

            WHEN 8
                LET lar_totales[ls_sie].siefore = 0

            OTHERWISE
                LET lar_totales[ls_sie].siefore = ls_sie

        END CASE

        LET lar_totales[ls_sie].mto_acciones_tot   = 0
        LET lar_totales[ls_sie].mto_acciones_par   = 0
        LET lar_totales[ls_sie].mto_acciones_tran  = 0
        LET lar_totales[ls_sie].subtotal_acciones  = 0
        LET lar_totales[ls_sie].mto_pesos_tot      = 0
        LET lar_totales[ls_sie].mto_pesos_par      = 0
        LET lar_totales[ls_sie].mto_pesos_tran     = 0
        LET lar_totales[ls_sie].subtotal_pesos     = 0
        LET lar_totales[ls_sie].mto_acciones_isr   = 0
        LET lar_totales[ls_sie].mto_pesos_isr      = 0
    END FOR


    LET lc_query = " SELECT siefore               , ",
                   "        tipo_movimiento       , ",
                   "        monto_en_acciones * -1, ",
                   "        monto_en_pesos * -1     ",
                   " FROM   ret_preliquida          ",
                   " WHERE  fecha_conversion = ?    ",
                   " AND    (   (tipo_movimiento BETWEEN 851 AND 859) " ,
                   "          OR (tipo_movimiento BETWEEN 861 AND 869) ) "

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

        LET lar_totales[ls_sie].subtotal_acciones = lar_totales[ls_sie].subtotal_acciones +
                                                    lr_montos.acciones

        LET lar_totales[ls_sie].subtotal_pesos    = lar_totales[ls_sie].subtotal_pesos +
                                                    lr_montos.pesos

        CASE
            WHEN (lr_montos.tipo_mov >= 851 AND lr_montos.tipo_mov <= 855) OR
                 (lr_montos.tipo_mov = 858)

                -- Retiros Totales
                LET lar_totales[ls_sie].mto_acciones_tot = lar_totales[ls_sie].mto_acciones_tot +
                                                           lr_montos.acciones

                LET lar_totales[ls_sie].mto_pesos_tot    = lar_totales[ls_sie].mto_pesos_tot +
                                                           lr_montos.pesos

            WHEN (lr_montos.tipo_mov >= 861 AND lr_montos.tipo_mov <= 865)

                -- Transferencias
                LET lar_totales[ls_sie].mto_acciones_tran = lar_totales[ls_sie].mto_acciones_tran +
                                                            lr_montos.acciones

                LET lar_totales[ls_sie].mto_pesos_tran    = lar_totales[ls_sie].mto_pesos_tran +
                                                            lr_montos.pesos

            WHEN lr_montos.tipo_mov = gs_mov_parcial

                -- Retiros Parciales
                LET lar_totales[ls_sie].mto_acciones_par = lar_totales[ls_sie].mto_acciones_par +
                                                           lr_montos.acciones

                LET lar_totales[ls_sie].mto_pesos_par    = lar_totales[ls_sie].mto_pesos_par +
                                                           lr_montos.pesos

            WHEN lr_montos.tipo_mov = 10

                -- Retencion ISR

                LET lar_totales[ls_sie].mto_acciones_isr = lar_totales[ls_sie].mto_acciones_isr  +
                                                           lr_montos.acciones

                LET lar_totales[ls_sie].mto_pesos_isr    = lar_totales[ls_sie].mto_pesos_isr     +
                                                           lr_montos.pesos
        END CASE
    END FOREACH

    IF lar_totales[8].subtotal_pesos > 0 THEN
        CALL SET_COUNT(8)
    ELSE
        CALL SET_COUNT(7)
    END IF

    DISPLAY ARRAY lar_totales TO scr_det1.*

    CLOSE WINDOW retc9424
    CLEAR SCREEN

END FUNCTION
