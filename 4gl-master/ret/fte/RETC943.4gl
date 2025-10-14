#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC943  => LIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE              #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega la ejecucion del SPL que inserta en las tablas  #
#                     de las estadisticas de CONSAR (Req. EFPS-152)             #
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos de liquidacion  #
#                     de retiros totales ISSSTE                                 #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        pre_liq     LIKE ret_estado_issste.estado_solicitud ,
        liquidado   LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE
        gs_peiss                ,
        gs_flag                 ,
        gs_codigo_afore         SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC943")

    CALL init() #i

    CALL f_despliega_info() RETURNING gs_flag, gr_folios.*

    IF gs_flag THEN

        CALL f_abre_ventana()
        CALL primer_paso(gr_folios.*)    #-- Realiza la Liquidacion de los montos
        CALL segundo_paso(gr_folios.*)   #-- Desmarca la cuenta y actualiza el estado de la solicitud

    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY             = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore     ,
           USER
    INTO   gs_codigo_afore  ,
           gc_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_peiss
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*PENSIONISSSTE*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.pre_liq
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_consar(?,?,?,?)"
    PREPARE eje_CONSAR FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    liquidacion de los procesos de retiros issste          #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lr_info RECORD
        fecha_liq           DATE    ,
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
        ls_cont             ,
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------
    
    OPEN WINDOW retc9431 AT 4,4 WITH FORM "RETC9431" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC943        LIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    LET ls_flag           = 1
    LET lr_info.fecha_liq = HOY
    INITIALIZE lr_folios.* TO NULL

    -- DISPOSICIONES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.disp
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.pre_liq

    IF lr_folios.disp IS NULL THEN
        LET lr_folios.disp = 0
    END IF

    -- TRANSFERENCIAS
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.trans
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.pre_liq

    IF lr_folios.trans IS NULL THEN
        LET lr_folios.trans = 0
    END IF

    -- PARCIALES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.par
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.pre_liq

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

        AFTER FIELD fecha_liq
            IF lr_info.fecha_liq IS NULL THEN
                CALL f_lib_error_msg("LA FECHA DE LIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_liq
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

            IF lr_info.fecha_liq IS NULL THEN
                CALL f_lib_error_msg("LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_liq
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

    CLOSE WINDOW retc9431

    RETURN ls_flag              ,
           lr_info.fecha_liq    ,
           lr_info.folio_tot    ,
           lr_info.folio_par    ,
           lr_info.folio_tran

END FUNCTION


#---------------------------------------------------------------------------#
# primer_paso : Llama a las funciones que realizan los procesos de          #
#               liquidacion de cuentas                                      #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    IF pr_folios.total <> 0 THEN
        CALL f_liquida_folio(pr_folios.total, "tot")    #-- Liquidacion de retiros totales
    END IF

    IF pr_folios.transfer <> 0 THEN
        CALL f_liquida_folio(pr_folios.transfer, "tra") #-- Liquidacion de transferencias
    END IF

    IF pr_folios.parcial <> 0 THEN
        CALL f_liquida_folio(pr_folios.parcial, "par")  #-- Liquidacion de retiros parciales
    END IF


END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Desmarca las cuentas del folio y actualiza el estado       #
#                de solicitud                                               #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DISPLAY "                                             " AT 18,1

    IF pr_folios.total <> 0 THEN
        CALL f_act_estado_sol(pr_folios.total, "tot")
        CALL f_desmarca_cta(pr_folios.total, "tot")
    END IF

    IF pr_folios.transfer <> 0 THEN
        CALL f_act_estado_sol(pr_folios.transfer, "tra")
        CALL f_desmarca_cta(pr_folios.transfer, "tra")

        -- Si es PENSION ISSSTE, se inserta en datamart los registros liquidados
        IF gs_codigo_afore = gs_peiss THEN
            CALL f_inserta_dtm(pr_folios.transfer)
        END IF

    END IF

    IF pr_folios.parcial <> 0 THEN
        CALL f_act_estado_sol(pr_folios.parcial, "par")
        CALL f_desmarca_cta(pr_folios.parcial, "par")
    END IF

    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")
    CLOSE WINDOW retc9432

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
        ls_pos              SMALLINT

    DEFINE
        pi_numfol           ,
        li_folio            INTEGER

    LET li_folio = 0

    CASE pc_modalidad
        WHEN "dis"
            LET lc_tabla    = " FROM ret_sol_issste_tx "
            LET lc_proceso  = "DISPOSICIONES"

        WHEN "tra"
            LET lc_tabla    = " FROM ret_trans_issste "
            LET lc_proceso  = "TRANSFERENCIAS"

        WHEN "par"
            LET lc_tabla    = " FROM ret_parcial_issste "
            LET lc_proceso  = "PARCIALES"
    END CASE

    LET lc_prepare = " SELECT UNIQUE(folio)  "      ,
                     lc_tabla                       ,
                     " WHERE  estado_solicitud = ? ",
                     " ORDER BY 1 "

    PREPARE prp_dat FROM lc_prepare

    IF pi_numfol > 0 THEN
        OPEN WINDOW retc9433 AT 14,62 WITH FORM "RETC9433" ATTRIBUTE(BORDER)

        WHILE TRUE
            DECLARE cur_dat CURSOR FOR prp_dat

            LET ls_pos = 1

            FOREACH cur_dat USING gr_edo.pre_liq
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

        CLOSE WINDOW retc9433
    ELSE
        PROMPT " NO HAY FOLIOS PARA  ", lc_proceso, "... < ENTER > PARA SALIR " FOR CHAR enter
    END IF

    RETURN li_folio
END FUNCTION


#---------------------------------------------------------------------------#
# f_liquida_folio : Realiza la liquidacion de acuerdo a la modalidad        #
#                   de retiro e inserta en las tablas de estadistica consar #
#---------------------------------------------------------------------------#
FUNCTION f_liquida_folio(pi_folio, pc_modo)

    DEFINE pi_folio LIKE ret_sol_issste_tx.folio

    DEFINE lr_preliquida RECORD LIKE dis_cuenta.*

    DEFINE lr_liquida RECORD
        nss                 LIKE dis_cuenta.nss             ,
        curp                LIKE dis_cuenta.curp            ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio           ,
        movimiento          LIKE dis_cuenta.tipo_movimiento
    END RECORD

    DEFINE
        pc_modo                 CHAR(3)

    DEFINE
        lc_tipo_retiro          CHAR(1)

    DEFINE
        ls_resp                 ,
        ls_inserta              ,
        ls_cod_tramite          SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_preliquida.* TO NULL

    CASE pc_modo
        WHEN "tot"
            SELECT cod_tramite
            INTO   ls_cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "DISPOSICION ISSSTE"

            DISPLAY "LIQUIDANDO RETIROS TOTALES ..." AT 4,5

        WHEN "tra"
            SELECT cod_tramite
            INTO   ls_cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "TRANSFERENCIA ISSSTE"

            DISPLAY "LIQUIDANDO TRANSFERENCIAS  ..." AT 10,5

        WHEN "par"
            SELECT cod_tramite
            INTO   ls_cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "RETIRO PARCIAL ISSSTE"

            DISPLAY "LIQUIDANDO RETIROS PARCIALES ..." AT 16,5

    END CASE

    DECLARE cur_preliq CURSOR FOR
    SELECT *
    FROM   ret_preliquida
    WHERE  folio = pi_folio

    FOREACH cur_preliq INTO lr_preliquida.*

        IF NOT(lr_preliquida.monto_en_pesos = 0 AND lr_preliquida.monto_en_acciones = 0) THEN
            INSERT INTO dis_cuenta
            VALUES(lr_preliquida.*)
        END IF

        INITIALIZE lr_preliquida.* TO NULL

    END FOREACH

    -- Inserta montos a estadisticas CONSAR
    DECLARE cur_consar CURSOR FOR
    SELECT UNIQUE(nss)      ,
           curp             ,
           consecutivo_lote ,
           folio            ,
           tipo_movimiento
    FROM   dis_cuenta
    WHERE  folio            = pi_folio
    AND    tipo_movimiento <> 10

    FOREACH cur_consar INTO lr_liquida.*

        EXECUTE eje_CONSAR USING lr_liquida.nss                 ,
                                 lr_liquida.consecutivo_lote    ,
                                 lr_liquida.folio               ,
                                 ls_cod_tramite
                           INTO  ls_inserta

        IF (pc_modo = "par") OR
           ( (pc_modo = "tot") AND
            (lr_liquida.movimiento = 853 OR
             lr_liquida.movimiento = 854 OR
             lr_liquida.movimiento = 855 OR
             lr_liquida.movimiento = 862 OR
             lr_liquida.movimiento = 864
            )
           ) THEN

            LET ls_resp = 0

            CASE lr_liquida.movimiento
                WHEN 853
                    LET lc_tipo_retiro = "C"

                WHEN 854
                    LET lc_tipo_retiro = "D"

                WHEN 855
                    LET lc_tipo_retiro = "E"

                WHEN 856
                    LET lc_tipo_retiro = "F"

                WHEN 862
                    LET lc_tipo_retiro = "I"

                WHEN 864
                    LET lc_tipo_retiro = "I"

            END CASE

            EXECUTE eje_marca_pen USING lr_liquida.nss              ,
                                        lr_liquida.curp             ,
                                        lr_liquida.folio            ,
                                        lr_liquida.consecutivo_lote ,
                                        lc_tipo_retiro              ,
                                        ls_cod_tramite              ,
                                        HOY
                                  INTO  ls_resp
        END IF

        INITIALIZE lr_liquida.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  liquidacion de retiros issste                            #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9432 AT 4,4 WITH FORM "RETC9432" ATTRIBUTE(BORDER)
    DISPLAY "                                                             RETIROS ISSSTE " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC943       LIQUIDACION DE SOLICITUDES DE RETIROS ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cta : Realiza la desmarca de las cuentas liquidadas            #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cta(pi_folio, pc_id_oper)

    DEFINE
        pi_folio        ,
        li_desm         INTEGER

    DEFINE
        pc_id_oper      CHAR(3)

    DEFINE lr_desmarca RECORD
        nss         LIKE cta_his_marca.nss          ,
        movimiento  LIKE cta_his_marca.marca_cod    ,
        consec      LIKE cta_his_marca.correlativo  ,
        edo_causa   LIKE cta_his_marca.estado_marca ,
        marca_causa LIKE cta_his_marca.marca_causa
    END RECORD


    LET li_desm = 0

    DECLARE cur_desmarca CURSOR FOR
    SELECT UNIQUE(nss)      ,
           tipo_movimiento  ,
           consecutivo_lote ,
           0                ,
           0
    FROM   ret_preliquida
    WHERE  folio            = pi_folio
    AND    tipo_movimiento <> 10
    ORDER BY 1

    FOREACH cur_desmarca INTO lr_desmarca.*
        EXECUTE eje_desmarca USING lr_desmarca.*, gc_usuario

        LET li_desm = li_desm + 1

        CASE pc_id_oper
            WHEN  "tot"
                DISPLAY "DESMARCAS : ", li_desm AT 5,44

            WHEN  "tra"
                DISPLAY "DESMARCAS : ", li_desm AT 11,44

            WHEN  "par"
                DISPLAY "DESMARCAS : ", li_desm AT 17,44
        END CASE
    END FOREACH

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

    FOR ls_cont = 1 TO 7
        LET la_rets[ls_cont] = 0
    END FOR

    DECLARE cur_liq CURSOR FOR
    SELECT UNIQUE(curp)    ,
           tipo_movimiento ,
           consecutivo_lote
    FROM   ret_preliquida
    WHERE  folio = pr_folio
    ORDER BY 1

    -- Actualiza el estado de la solicitud de los registros liquidados
    CASE pc_id_oper

        WHEN "tot"
            DISPLAY "FOLIO    : ", pr_folio AT 5,12

            FOREACH cur_liq INTO lr_datos.*

                UPDATE ret_sol_issste_tx
                SET    estado_solicitud = gr_edo.liquidado
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.pre_liq

                CASE lr_datos.tipo_mov
                    WHEN 851
                        LET la_rets[1] = la_rets[1] + 1
                    WHEN 852
                        SELECT "OK"
                        FROM   ret_sol_issste_tx
                        WHERE  tipo_retiro      = "I"
                        AND    estado_solicitud = gr_edo.liquidado
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
                DISPLAY "RETIRO D  : ", la_rets[4] AT 6,44
                DISPLAY "RETIRO E  : ", la_rets[5] AT 7,44
                DISPLAY "RETIRO I  : ", la_rets[6] AT 8,44
                DISPLAY "RETIRO K  : ", la_rets[7] AT 9,44

            END FOREACH

            -- Actualiza la tabla en caso de que haya quedado algun registro en
            -- estado preliquidado
            UPDATE ret_sol_issste_tx
            SET    estado_solicitud = gr_edo.liquidado
            WHERE  estado_solicitud = gr_edo.pre_liq

            DISPLAY "(TERMINADO)" AT 4,39

        WHEN "tra"
            DISPLAY "FOLIO    : ", pr_folio AT 11,12

            FOREACH cur_liq INTO lr_datos.*

                UPDATE ret_trans_issste
                SET    estado_solicitud = gr_edo.liquidado
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.pre_liq

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
                DISPLAY "RETIRO J : ", la_rets[3] AT 14,12
                DISPLAY "RETIRO L : ", la_rets[4] AT 12,45
                DISPLAY "RETIRO N : ", la_rets[5] AT 13,45

            END FOREACH

            -- Actualiza la tabla en caso de que haya quedado algun registro en
            -- estado preliquidado
            UPDATE ret_trans_issste
            SET    estado_solicitud = gr_edo.liquidado
            WHERE  estado_solicitud = gr_edo.pre_liq

            DISPLAY "(TERMINADO)" AT 10,39

        WHEN "par"
            DISPLAY "FOLIO    : ", pr_folio AT 17,12

            FOREACH cur_liq INTO lr_datos.*

                UPDATE ret_parcial_issste
                SET    estado_solicitud = gr_edo.liquidado
                WHERE  curp             = lr_datos.curp
                AND    consecutivo      = lr_datos.consec
                AND    estado_solicitud = gr_edo.pre_liq

                CASE lr_datos.tipo_mov
                    WHEN 856
                        LET la_rets[1] = la_rets[1] + 1
                END CASE

                DISPLAY "RETIRO F : ", la_rets[1] AT 18,12

            END FOREACH

            INSERT INTO ret_ctr_envio
            VALUES ( pr_folio                              ,
                     "AV56"                                ,
                     "CZAAV56 DETAV56 SUMAV56"             ,
                     0                                     ,
                     1                                     ,
                     NULL
                    )

            -- Actualiza la tabla en caso de que haya quedado algun registro en
            -- estado preliquidado
            UPDATE ret_parcial_issste
            SET    estado_solicitud = gr_edo.liquidado
            WHERE  estado_solicitud = gr_edo.pre_liq

            DISPLAY "(TERMINADO)" AT 16,39

    END CASE

END FUNCTION

#-------------------------------------------------------------------------------#
# f_inserta_dtm : Inserta en la tabla de datamart las solicitudes de            #
#                 transferencias que hayan sido liquidadas (Solo PensionISSSTE) #
#-------------------------------------------------------------------------------#
FUNCTION f_inserta_dtm(pi_folio)

    DEFINE
        pi_folio        INTEGER

    DEFINE lr_datamart RECORD LIKE ret_datamart_issste.*

    DECLARE cur_dtm CURSOR FOR
        SELECT folio                ,
               nss                  ,
               nss_issste           ,
               curp                 ,
               sec_pension          ,
               nombre_datamart      ,
               paterno_datamart     ,
               materno_datamart     ,
               nombre_afore         ,
               paterno_afore        ,
               materno_afore        ,
               num_concesion        ,
               delegacion           ,
               tipo_movimiento      ,
               tipo_retiro          ,
               regimen              ,
               tipo_seguro          ,
               tipo_pension         ,
               cve_pension          ,
               tipo_prestacion      ,
               fecha_ini_pen        ,
               fecha_resolucion     ,
               semanas_cotizadas    ,
               101
        FROM   ret_trans_issste
        WHERE  folio            = pi_folio
        AND    estado_solicitud = gr_edo.liquidado

    FOREACH cur_dtm INTO lr_datamart.*

        SELECT "OK"
        FROM   ret_datamart_issste
        WHERE  curp        = lr_datamart.curp
        AND    sec_pension = lr_datamart.sec_pension
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            INSERT INTO ret_datamart_issste
            VALUES (lr_datamart.*)
        END IF

    END FOREACH

END FUNCTION
