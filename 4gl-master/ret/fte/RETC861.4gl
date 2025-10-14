#################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC861  => PROGRAMA GENERAL DE LIQUIDACION DE DISPOSICION DE         #
#                     RECURSOS                                                  #
#Fecha creacion    => 18 DE MAYO DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega la ejecucion del SPL que inserta en las tablas  #
#                     de las estadisticas de CONSAR (Req. EFPS-152)             #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 11 DE MAYO DE 2011                                        #
#                  => Se realizan las modificaciones al codigo para incluir     #
#                     el llamado de las funciones para realizar el marcaje de   #
#                     trabajador pensionado (REQ. EFPS-157)                     #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        enviado         LIKE ret_estado.estado_solicitud ,
        liquidado       LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        gdt_cambio_infonavit    ,
        gdt_liquida             DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_programa             CHAR(007) ,
        gc_tipo_retiro          CHAR(001) ,
        gc_usuario              CHAR(015) ,
        gc_titulo               CHAR(072)

    DEFINE
        gs_cod_tramite          ,
        gs_movimiento           ,
        gs_cod_afore            SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC861")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY gc_titulo AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY gdt_liquida USING "DD-MM-YYYY", " " AT 3,65 ATTRIBUTE(REVERSE)

    MENU "LIQUIDACION"
        COMMAND "Liquida" "Ejecuta el proceso de liquidacion"
            CALL f_genera_liquidacion()

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Liquidacion"
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

    -- -----------------------------------------------------------------------------

    ----- PARAMETROS DE ENTRADA -----
    LET gc_programa             = ARG_VAL(1)
    LET gc_tipo_retiro          = ARG_VAL(2)
    LET gdt_liquida             = ARG_VAL(3)
    LET gc_titulo               = " ", gc_programa CLIPPED,
                                  "   INCORPORA PAGO DISPOSICION DE RECURSOS - RETIRO ",
                                  gc_tipo_retiro,
                                  "          "

    LET gdt_cambio_infonavit    = MDY(01,12,2012)
    LET gc_usuario              = f_lib_obten_user()

    ----- CLAVE DE MOVIMIENTO -----
    SELECT movimiento
    INTO   gs_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = gc_tipo_retiro

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"


    ----- DESMARCA DE CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE desmarca_cuenta( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- PRELIQUIDACION -----
    LET lc_prepare = "EXECUTE FUNCTION fn_preliquida_ret_ms( ?,?,?,? )"
    PREPARE eje_preliquida FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_consar(?,?,?,?)"
    PREPARE eje_CONSAR FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EDO CUENTA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_edo_cta_pen(?,?,?,?,?,?) "
    PREPARE eje_edo_cta_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_liquidacion : Ejecuta los pasos necesarios para realizar la      #
#                        liquidacion                                        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_liquidacion()

    DEFINE lr_retiro RECORD
        fecha_liq       DATE                        ,
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE
        ls_flag_err         ,
        ls_flag             SMALLINT

    DEFINE
        li_proceso          INTEGER

    -- -----------------------------------------------------------------------------

    CALL f_captura_datos() RETURNING ls_flag, lr_retiro.*

    IF ls_flag THEN
        CALL f_obtiene_precios_accion(lr_retiro.fecha_liq)
        CALL f_abre_ventana()

        CALL primer_paso(lr_retiro.*)   #-- Realiza la preliquidacion de los montos

        CALL segundo_paso(lr_retiro.*)  #-- Valida la informacion de los montos
            RETURNING ls_flag_err, li_proceso

        IF ls_flag_err = 0 THEN
            CALL tercer_paso(lr_retiro.*)   #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            PROMPT " SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO ... <ENTER> PARA MOSTRAR" FOR CHAR enter
            CLOSE WINDOW retc8612
            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Despliega la pantalla de captura del folio y la fecha   #
#                   que se usara para generar la liquidacion del retiro imss#
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_info RECORD
        fecha_liq           DATE    ,
        folio               INTEGER
    END RECORD

    DEFINE
        ls_procesa          ,
        ls_flag             SMALLINT

    DEFINE
        lc_cad_msg          CHAR(100)

    -- ---------------------------------------------------------------------------------

    LET ls_flag             = TRUE
    LET lr_info.fecha_liq   = gdt_liquida

    -- Ultimo folio por liquidar
    SELECT MAX(folio)
    INTO   lr_info.folio
    FROM   ret_solicitud_tx
    WHERE  tipo_retiro      = gc_tipo_retiro
    AND    estado_solicitud = gr_edo.enviado

    OPEN WINDOW retc8611 AT 4,4 WITH FORM "RETC8611" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY gc_titulo AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY gdt_liquida USING "DD-MM-YYYY", " " AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_info.folio WITHOUT DEFAULTS

        AFTER FIELD folio
            CALL f_valida_folio(lr_info.folio,gc_tipo_retiro)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD folio
            END IF

        ON KEY (ESC)
            CALL f_valida_folio(lr_info.folio,gc_tipo_retiro)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD folio
            END IF

            LET lc_cad_msg  = " ¿EJECUTAR LA LIQUIDACION DEL TIPO RETIRO ", gc_tipo_retiro, "? (S/N) : "

            IF f_lib_pregunta(lc_cad_msg) = TRUE THEN
                LET ls_flag = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_flag = FALSE
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_error_msg("PROCESO CANCELADO")
            LET ls_flag = FALSE
            EXIT INPUT

    END INPUT

    CLOSE WINDOW retc8611

    RETURN ls_flag, lr_info.*

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Llama a las funciones que realizan los procesos de          #
#               preliquidacion                                              #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pdt_fecha_pre,pi_folio)

    DEFINE pi_folio LIKE ret_solicitud_tx.folio

    DEFINE
        pdt_fecha_pre           DATE

    DEFINE lr_result RECORD
        codigo_res              SMALLINT      ,
        mto_total               DECIMAL(16,6)
    END RECORD

    DEFINE lr_datos_pre RECORD
        nss                     LIKE ret_solicitud_tx.nss               ,
        consecutivo             LIKE ret_solicitud_tx.consecutivo       ,
        regimen                 LIKE ret_solicitud_tx.regimen           ,
        fecha_resolucion        LIKE ret_solicitud_tx.fecha_resolucion  ,
        edo_sub_viv             LIKE ret_monto_viv.estado_sub_viv       ,
        diag_registro           LIKE ret_solicitud_tx.diag_registro
    END RECORD

    DEFINE
        ls_liquida              ,
        ls_num_regs             ,
        ls_num_vent             ,
        ls_subcta               SMALLINT

    DEFINE
        lc_curp                 CHAR(18)    ,
        lc_tipo_vent            CHAR(04)

    -- ---------------------------------------------------------------------------------

    INITIALIZE lr_datos_pre.* TO NULL

    CALL f_tablas_tmp()
    LET ls_liquida      = TRUE
    LET ls_num_regs     = 0
    LET ls_num_vent     = 0

    DISPLAY "PRELIQUIDANDO RETIROS TIPO ", gc_tipo_retiro, " ..." AT 6,5
    DISPLAY "FOLIO DE LIQUIDACION    : ", pi_folio AT 11,13
    DISPLAY "REGISTROS PROCESADOS    : ", ls_num_regs AT 13,13

    DECLARE cur_nss CURSOR FOR
    SELECT A.nss                ,
           A.consecutivo        ,
           A.regimen            ,
           A.fecha_resolucion   ,
           B.estado_sub_viv     ,
           A.diag_registro
    FROM   ret_solicitud_tx A       ,
           ret_monto_viv B          ,
           tab_diag_procesar_disp C
    WHERE  A.folio          = pi_folio
    AND    A.folio          = B.folio
    AND    A.nss            = B.nss
    AND    A.consecutivo    = B.consecutivo
    AND    A.tipo_retiro    = gc_tipo_retiro
    AND    A.diag_registro  = C.diag_procesar
    AND    C.id_aceptado    = 1

    FOREACH cur_nss INTO lr_datos_pre.*

        LET ls_num_regs = ls_num_regs + 1
        DISPLAY "REGISTROS PROCESADOS    : ", ls_num_regs AT 13,13

        DECLARE cur_prov CURSOR FOR
            SELECT UNIQUE(subcuenta)
            FROM   dis_provision
            WHERE  folio            = pi_folio
            AND    nss              = lr_datos_pre.nss
            AND    consecutivo_lote = lr_datos_pre.consecutivo

        FOREACH cur_prov INTO ls_subcta

            IF ( (ls_subcta <> 4) AND (ls_subcta <> 8) ) THEN
                LET ls_liquida = TRUE
            ELSE
                -- CPL-1820
                -- Se liquida vivienda dependiendo de sus diagnosticos en estos casos:
                -- Para cualquier tipo de retiro diferente a E
                -- Si es retiro E, regimen 97
                -- Si es retiro E, regimen 73 y con fecha resolucion menor al 12/01/2012
                IF ( (gc_tipo_retiro <> "E") OR
                     ( (gc_tipo_retiro = "E") AND 
                       (lr_datos_pre.regimen = 73) AND
                       (lr_datos_pre.fecha_resolucion <= gdt_cambio_infonavit)
                     ) OR
                     ( (gc_tipo_retiro = "E") AND (lr_datos_pre.regimen = 97) )
                   )
                THEN
                    LET ls_liquida = f_valida_pago_vivienda(lr_datos_pre.diag_registro,
                                                            lr_datos_pre.edo_sub_viv  ,
                                                            ls_subcta
                                                           )
                ELSE
                    -- Para el retiro E, regimen 73 con fecha resolucion mayor al 12-01-2012
                    -- se verifica si viene de ventanilla
                    SELECT NVL(COUNT(*), 0)
                    INTO   ls_num_vent
                    FROM   ret_notifica_vivienda
                    WHERE  nss          = lr_datos_pre.nss

                    IF (ls_num_vent > 0) THEN

                        SELECT grupo_trabajador
                        INTO   lc_tipo_vent
                        FROM   ret_notifica_vivienda
                        WHERE  nss          = lr_datos_pre.nss
                        AND    consecutivo  = lr_datos_pre.consecutivo
                        GROUP BY 1

                        -- Si el registro viene por Ventanilla INFONAVIT, se paga la vivienda
                        IF (lc_tipo_vent = "0101") THEN
                            LET ls_liquida = TRUE
                        ELSE
                            -- Si el registro viene por Ventanilla AFORE, no se paga la vivienda
                            LET ls_liquida = FALSE
                        END IF
                    ELSE
                        LET ls_liquida = FALSE
                    END IF -- Tipo Ventanilla
                END IF  --Retiro diferente de E
            END IF -- Subcuentas

            IF (ls_liquida = TRUE) THEN
                EXECUTE eje_preliquida USING pi_folio          ,
                                             lr_datos_pre.nss  ,
                                             ls_subcta         ,
                                             gdt_liquida
                                       INTO lr_result.*

                -- Se actualiza la tabla de preliquidacion para que recalcule correctamente
                -- el valor de vivienda con el precio de accion
                IF (ls_subcta = 4 OR ls_subcta = 8) THEN
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
                SET    curp             = lc_curp       ,
                       folio_sua        = "         "   ,
                       sucursal         = 0
                WHERE  nss              = lr_datos_pre.nss
                AND    folio            = pi_folio
                AND    subcuenta        = ls_subcta

            END IF

            LET ls_num_vent = 0

        END FOREACH -- Subcuentas

        INITIALIZE lr_datos_pre.* TO NULL

    END FOREACH -- Siguiente NSS

    DISPLAY "OK" AT 6,38

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida los montos preliquidados en el primer paso          #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel        DATE                         ,
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE li_idproc LIKE ret_bitacora_error.id_proceso

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY "VALIDANDO INFORMACION        ..." AT 7,5
    SLEEP 1

    LET li_idproc = f_ultimo_id_err()

    ----------  Valida sobregiro del retiros --------------
    CALL f_valida_sobregiro(ls_flag, li_idproc, pr_folios.*)
        RETURNING ls_flag

    IF ls_flag = 0 THEN
        DISPLAY "OK" AT 7,38
    ELSE
        DISPLAY "ERROR" AT 7,38
    END IF

    RETURN ls_flag, li_idproc

END FUNCTION


#---------------------------------------------------------------------------#
# tercer_paso : Vacia la informacion de las tablas temporales a las fisicas #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fec_prel    DATE                        ,
        total       LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE lr_liquida RECORD
        nss                 LIKE dis_cuenta.nss             ,
        curp                LIKE dis_cuenta.curp            ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio
    END RECORD

    DEFINE lc_regimen LIKE ret_solicitud_tx.regimen

    DEFINE
        ls_resp                 ,
        ls_inserta              SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1
    DISPLAY "LIQUIDANDO REGISTROS         ... " AT 8,5

    INSERT INTO dis_cuenta
        SELECT *
        FROM   tmp_preliquida_disp
        WHERE  folio = pr_folios.total

    DECLARE cur_liquida CURSOR FOR
    SELECT UNIQUE(nss)      ,
           curp             ,
           consecutivo_lote ,
           folio
    FROM   tmp_preliquida_disp
    WHERE  folio = pr_folios.total

    FOREACH cur_liquida INTO lr_liquida.*

        -- Inserta los datos de la liquidacion
        EXECUTE eje_CONSAR USING lr_liquida.nss                 ,
                                 lr_liquida.consecutivo_lote    ,
                                 lr_liquida.folio               ,
                                 gs_cod_tramite
                           INTO  ls_inserta

        -- Inserta en la tabla para el estado de cuenta de trabajador pensionado
        SELECT regimen
        INTO   lc_regimen
        FROM   ret_solicitud_tx
        WHERE  nss          = lr_liquida.nss
        AND    consecutivo  = lr_liquida.consecutivo_lote
        AND    folio        = lr_liquida.folio

        EXECUTE eje_edo_cta_pen USING lr_liquida.nss                ,
                                      lc_regimen                    ,
                                      gs_cod_tramite                ,
                                      gdt_liquida                   ,
                                      lr_liquida.folio              ,
                                      lr_liquida.consecutivo_lote
                                INTO  ls_resp

        IF (gc_tipo_retiro = "E") THEN
            SELECT "OK"
            FROM   ret_marca_pensionado
            WHERE  nss          = lr_liquida.nss
            AND    tipo_retiro  = "E"
            GROUP BY 1

            IF (STATUS = NOTFOUND) THEN
                EXECUTE eje_marca_pen USING lr_liquida.nss              ,
                                            lr_liquida.curp             ,
                                            lr_liquida.folio            ,
                                            lr_liquida.consecutivo_lote ,
                                            gc_tipo_retiro              ,
                                            gs_cod_tramite              ,
                                            gdt_liquida
                                      INTO  ls_resp
            END IF
        ELSE
            -- MLM-2633
            -- Inserta la marca de trabajador pensionado para todos los tipos de retiro
            EXECUTE eje_marca_pen USING lr_liquida.nss              ,
                                        lr_liquida.curp             ,
                                        lr_liquida.folio            ,
                                        lr_liquida.consecutivo_lote ,
                                        gc_tipo_retiro              ,
                                        gs_cod_tramite              ,
                                        gdt_liquida
                                  INTO  ls_resp
        END IF

        INITIALIZE lr_liquida.* TO NULL

    END FOREACH

    UPDATE ret_solicitud_tx
    SET    estado_solicitud   = gr_edo.liquidado
    WHERE  folio              = pr_folios.total
    AND    estado_solicitud   = gr_edo.enviado
    AND    diag_registro IN ( SELECT diag_procesar
                              FROM   tab_diag_procesar_disp
                              WHERE  id_aceptado = 1
                            )

    DISPLAY "OK" AT 8,38

    CALL f_desmarca_cta(pr_folios.total)

    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc8612

END FUNCTION


#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  preliquidacion de retiros issste                         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc8612 AT 4,4 WITH FORM "RETC8612" ATTRIBUTE(BORDER)
    DISPLAY "                                                                            " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY gc_titulo AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY gdt_liquida USING "DD-MM-YYYY", " " AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usan en el proceso       #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_preliquida_disp
    WHENEVER ERROR STOP

    SELECT *
    FROM   dis_cuenta
    WHERE  0 = 1
    INTO TEMP tmp_preliquida_disp

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_sobregiro : Valida que no exista sobregiro en las subcuentas     #
#                      que se provisionaron                                 #
#---------------------------------------------------------------------------#
FUNCTION f_valida_sobregiro(pr_valida)

    DEFINE pr_valida RECORD
        bandera         SMALLINT                    ,
        id_proc         SMALLINT                    ,
        fec_preliq      DATE                        ,
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        folio        LIKE ret_bitacora_error.folio          ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_saldo_prov, lr_saldo_dia RECORD
            nss             LIKE ret_solicitud_tx.nss            ,
            subcta          SMALLINT                             ,
            monto_acc       LIKE dis_provision.monto_en_acciones ,
            monto_pes       LIKE dis_provision.monto_en_acciones
        END RECORD

    DEFINE ld_curp  LIKE dis_provision.curp

    DEFINE
        ls_error            ,
        ls_grupo            SMALLINT

    -- ---------------------------------------------------------------------------------

    -- Inicializamos las variables de la bitacora de errores
    LET ls_error                = pr_valida.bandera
    LET lr_error.id_proceso     = pr_valida.id_proc
    LET lr_error.folio          = pr_valida.folio
    LET lr_error.tipo_campo     = "RETIRO IMSS - TIPO ", gc_tipo_retiro
    LET lr_error.id_error       = 10
    LET ls_grupo                = 0
    LET lr_saldo_dia.monto_acc  = 0

    DECLARE cur_tmp CURSOR FOR
    SELECT curp              ,
           nss               ,
           subcuenta         ,
           monto_en_acciones ,
           monto_en_pesos
    FROM   tmp_preliquida_disp
    WHERE  folio    = pr_valida.folio
    ORDER BY 1,3

    FOREACH cur_tmp INTO ld_curp, lr_saldo_prov.*
        LET lr_error.nss          = lr_saldo_prov.nss
        LET lr_error.curp         = ld_curp

        INITIALIZE lr_saldo_dia.* TO NULL

        DECLARE cur_val_sobre CURSOR FOR eje_saldo_dia

        OPEN cur_val_sobre USING lr_saldo_prov.nss      ,
                                 lr_saldo_prov.subcta   ,
                                 ls_grupo               ,
                                 pr_valida.fec_preliq

        FETCH cur_val_sobre INTO lr_saldo_dia.*

        CLOSE cur_val_sobre

        IF lr_saldo_dia.monto_acc IS NULL THEN
            LET lr_saldo_dia.monto_acc = 0
        END IF

        -- Si el monto de provisionado es mayor al saldo al dia entonces registra
        -- un sobregiro
        LET lr_saldo_prov.monto_acc = lr_saldo_prov.monto_acc * -1

        IF (f_redondea_val(lr_saldo_prov.monto_acc,2) > f_redondea_val(lr_saldo_dia.monto_acc,2)) THEN

            LET lr_error.nom_campo    = "Sobregiro Subcta ", lr_saldo_prov.subcta
            LET lr_error.valor_campo  = "Prov = ", lr_saldo_prov.monto_acc USING "<<<<<<<<<.<<<<<<",
                                        " Saldo = ",  lr_saldo_dia.monto_acc USING "<<<<<<<<<.<<<<<<"

            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

    END FOREACH

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_folio : Valida que el folio capturado sea correcto para ejecutar #
#                  la liquidacion                                           #
#---------------------------------------------------------------------------#
FUNCTION f_valida_folio(pr_valida)

    DEFINE pr_valida RECORD
        folio           LIKE ret_solicitud_tx.folio         ,
        tipo_ret        LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado = 0

    IF pr_valida.folio IS NULL OR pr_valida.folio <= 0 THEN
        LET ls_estado   = 1
        LET lc_mensaje  = "EL FOLIO NO PUEDE SER NULO"
    ELSE
        SELECT "OK"
        FROM   ret_solicitud_tx A
        WHERE  A.folio          = pr_valida.folio
        AND    A.tipo_retiro    = pr_valida.tipo_ret
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            LET ls_estado   = 1
            LET lc_mensaje  = "EL FOLIO INGRESADO NO EXISTE PARA EL TIPO RETIRO ", pr_valida.tipo_ret CLIPPED
        ELSE
            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio             = pr_valida.folio
            AND    tipo_movimiento   = gs_movimiento
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET ls_estado   = 1
                LET lc_mensaje  = "EL FOLIO INGRESADO YA FUE LIQUIDADO"
            ELSE
                SELECT "OK"
                FROM   ret_solicitud_tx A       ,
                       ret_monto_viv B          ,
                       tab_diag_procesar_disp C
                WHERE  A.folio          = pr_valida.folio
                AND    A.folio          = B.folio
                AND    A.nss            = B.nss
                AND    A.consecutivo    = B.consecutivo
                AND    A.tipo_retiro    = pr_valida.tipo_ret
                AND    A.diag_registro  = C.diag_procesar
                AND    C.id_aceptado    = 1
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    LET ls_estado   = 1
                    LET lc_mensaje  = "NO EXISTEN REGISTROS A LIQUIDAR EN ESTE FOLIO"
                END IF -- No existen registros
            END IF -- Folio ya liquidado
        END IF -- Folio no existe
    END IF -- Folio nulo

    RETURN ls_estado, lc_mensaje

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

    IF pr_viv.diag_procesar <> 400 THEN
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
        END IF

        IF (pr_viv.edo_viv = 3 AND pr_viv.subcuenta <> 8) THEN
            LET ls_id_aceptado = FALSE
        END IF

    END IF

    RETURN ls_id_aceptado

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cta : Realiza la desmarca de las cuentas liquidadas            #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cta(pi_folio)

    DEFINE
        pi_folio        ,
        li_desm         INTEGER

    DEFINE lr_desmarca RECORD
        nss                 LIKE cta_his_marca.nss          ,
        movimiento          LIKE cta_his_marca.marca_cod    ,
        consecutivo         LIKE cta_his_marca.correlativo  ,
        edo_causa           LIKE cta_his_marca.estado_marca ,
        marca_causa         LIKE cta_his_marca.marca_causa
    END RECORD

    DEFINE ldt_fecha_resol  LIKE ret_solicitud_tx.fecha_resolucion
    DEFINE ls_regimen       LIKE ret_solicitud_tx.regimen

    DEFINE
        ls_num_vent             ,
        ls_id_desmarca          SMALLINT

    DEFINE
        lc_tipo_vent            CHAR(04)

    -- ---------------------------------------------------------------------------------

    LET ls_num_vent     = 0
    LET li_desm         = 0
    LET ls_id_desmarca  = TRUE

    DISPLAY "REGISTROS DESMARCADOS   : ", li_desm AT 14,13

    DECLARE cur_desmarca CURSOR FOR
        SELECT A.nss                ,
               A.marca_cod          ,
               A.correlativo        ,
               0                    ,
               0                    ,
               B.regimen            ,
               B.fecha_resolucion
        FROM   cta_his_marca A      ,
               ret_solicitud_tx B
        WHERE  A.correlativo        = B.consecutivo
        AND    A.nss                = B.nss
        AND    B.folio              = pi_folio
        AND    A.marca_cod          = gs_movimiento
        AND    B.tipo_retiro        = gc_tipo_retiro
        AND    A.fecha_fin IS NULL
        ORDER BY 1

    FOREACH cur_desmarca INTO lr_desmarca.*     ,
                              ls_regimen        ,
                              ldt_fecha_resol

        -- CPL-1820
        -- Se desmarca la cuenta en estos casos:
        -- Para cualquier tipo de retiro diferente a E
        -- Si es retiro E, regimen 97
        -- Si es retiro E, regimen 73 y con fecha resolucion menor al 12/01/2012
        IF ( (gc_tipo_retiro <> "E") OR
             ( (gc_tipo_retiro  = "E") AND 
               (ls_regimen      = 73 ) AND
               (ldt_fecha_resol <= gdt_cambio_infonavit)
             ) OR
             ( (gc_tipo_retiro = "E") AND (ls_regimen = 97) )
           )
        THEN
            LET ls_id_desmarca = TRUE
        ELSE
            -- Para el retiro E, regimen 73 con fecha resolucion mayor al 12-01-2012
            -- se verifica si viene de ventanilla
            SELECT NVL(COUNT(*), 0)
            INTO   ls_num_vent
            FROM   ret_notifica_vivienda
            WHERE  nss          = lr_desmarca.nss

            IF (ls_num_vent > 0) THEN

                SELECT grupo_trabajador
                INTO   lc_tipo_vent
                FROM   ret_notifica_vivienda
                WHERE  nss          = lr_desmarca.nss
                AND    consecutivo  = lr_desmarca.consecutivo
                GROUP BY 1

                -- Si el registro viene por Ventanilla INFONAVIT, se desmarca la cuenta
                IF (lc_tipo_vent = "0101") THEN
                    LET ls_id_desmarca  = TRUE
                ELSE
                    -- Si el registro viene por Ventanilla AFORE, no se desmarca la cuenta
                    LET ls_id_desmarca  = FALSE
                END IF
            ELSE
                LET ls_id_desmarca  = TRUE
            END IF -- Tipo Ventanilla

        END IF -- Retiro diferente a E o E-97

        IF (ls_id_desmarca = TRUE) THEN

            EXECUTE eje_desmarca USING lr_desmarca.*, gc_usuario

            SELECT "OK"
            FROM   cta_act_marca
            WHERE  nss       = lr_desmarca.nss
            AND    marca_cod = 140
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_solicita_edo_cuenta(lr_desmarca.nss, gdt_liquida)
            END IF

            LET li_desm = li_desm + 1
            DISPLAY "REGISTROS DESMARCADOS   : ", li_desm AT 14,13
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por pdt_fecha_precio                      #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fecha_precio)

    DEFINE
        pdt_fecha_precio        DATE

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

    -- ---------------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fecha_precio
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
# f_solicita_edo_cuenta : Actualiza la tabla cta_ctr_cuenta para hacer la   #
#                         solicitud de estado de cuenta                     #
#---------------------------------------------------------------------------#
FUNCTION f_solicita_edo_cuenta(pc_nss, pdt_fecha)

   DEFINE
        pc_nss          CHAR(11)

    DEFINE
        pdt_fecha       DATE

    -- ---------------------------------------------------------------------------------

    UPDATE cta_ctr_cuenta
    SET    tipo_informe  = 5        ,
           fecha_informe = pdt_fecha
    WHERE  nss           = pc_nss

END FUNCTION

#---------------------------------------------------------------------------#
# f_redondea_val : Redondea la cantidad p_monto_redondear a tantos digitos  #
#                  como indique la variable p_redondea                      #
#---------------------------------------------------------------------------#
FUNCTION f_redondea_val(p_monto_redondear, p_redondea)

    DEFINE
        p_monto_redondear DECIMAL(16,6)

    DEFINE
        p_redondea       SMALLINT


    DEFINE
        ls_monto_return   DECIMAL(16,2)

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING p_monto_redondear, p_redondea
    FETCH round_cur INTO ls_monto_return

    CLOSE round_cur

    RETURN ls_monto_return

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
    LET lr_bitacora.programa    = gc_programa
    LET lr_bitacora.nom_archivo = " "
    LET lr_bitacora.fecha_error = gdt_liquida
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
        li_pos              INTEGER

    DEFINE
        lc_titulo           CHAR(077),
        lc_where            CHAR(200),
        lc_query            CHAR(500)

    -- -----------------------------------------------------------------------------

    LET lc_titulo = " ", gc_programa CLIPPED,
                    "             BITACORA DE ERRORES DE LIQUIDACION                            "

    OPEN WINDOW retc8613 AT 4,4 WITH FORM "RETC8613" ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl-C > Salir                                        < ESC > Consulta     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY lc_titulo AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY gdt_liquida USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

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
                         "WHERE  id_proceso = " , pi_proceso ,
                         "AND    programa = ? "
    ELSE
        -- Se inicia el construct para obtener los datos de consulta
        -- para la bitacora
        LET int_flag = FALSE

        DISPLAY gc_programa TO programa

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
            CLOSE WINDOW retc8613
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
                         " AND    programa = ? ",
                         " ORDER BY id_proceso DESC "
    END IF

    PREPARE prp_err FROM lc_query
    DECLARE cur_err CURSOR FOR prp_err

    LET li_pos = 1

    FOREACH cur_err USING gc_programa
                    INTO lar_bitacora[li_pos].*

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
            CLOSE WINDOW retc8613

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc8613
        END IF

END FUNCTION
