#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETL833  => CONSULTA Y GENERACION DEL ANEXO CONSAR 1112               #
#Fecha creacion    => 1 DE JUNIO DE 2012                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 15 DE AGOSTO DE 2012                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => - Se incluyen los movimientos ISSSTE en el anexo 1112     #
#                  => - Se separa la parte de las entradas en el modulo         #
#                       RETL833A.4gl                                            #
#Fecha actualiz.   => 3 DE FEBRERO DE 2013                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => - Unificacion de versiones entre todas las afores         #
#                  => - Se Cambian los nombres de los programas para incluir    #
#                       un 4gl para funciones y variables globales              #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

GLOBALS "RETL833A.4gl"

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETL833")

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETL833         GENERACION Y CONSULTA DE ANEXO CONSAR 1112            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "ANEXO CONSAR 1112"
        COMMAND "Genera Anexo 1112" "Genera el archivo plano del Anexo 1112"
            CALL f_genera_anexo_1112()

        COMMAND "Consulta" "Consulta los registros del Anexo 1112 generados anteriormente"
            CALL f_consulta_anexo_1112()

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

    LET HOY = TODAY

    SELECT USER         ,
           *
    INTO   gc_usuario   ,
           gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gr_afore.coppel
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*COPPEL*"

    SELECT afore_cod
    INTO   gr_afore.issste
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
    INTO   gr_edo.rechazado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECHAZADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_anexo_1112 : Ejecuta las instrucciones para generar el layout    #
#                       del anexo CONSAR 1112                               #
#---------------------------------------------------------------------------#
FUNCTION f_genera_anexo_1112()

    DEFINE lr_fechas_anexo  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE li_folio LIKE dis_cuenta.folio

    DEFINE
        ls_salida           SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_salida   = 0
    LET li_folio    = 1

    CALL f_captura_fechas("REP") RETURNING lr_fechas_anexo.*, ls_salida

    IF ls_salida = 0 THEN

        CALL f_abre_ventana()

        DISPLAY "REPORTE CONSAR 1112" AT 5,6
        DISPLAY "DEL     " AT 6,14
        DISPLAY lr_fechas_anexo.inicial USING "DD/MM/YYYY" AT 6,22
        DISPLAY "AL     " AT 6,45
        DISPLAY lr_fechas_anexo.final USING "DD/MM/YYYY" AT 6,52

        CALL f_tablas_tmp_genera()
        CALL f_genera_extracto_temp(lr_fechas_anexo.*)
        CALL f_genera_detalle_anexo(li_folio, lr_fechas_anexo.*)
        CALL f_genera_plano_1112()
        CALL f_consolida_tablas()

        CLOSE WINDOW retl8332

    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el intervalo de fechas para realizar la        #
#                    de registros a liquidar                                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas(pc_input)

    DEFINE
        pc_input                CHAR(03)

    DEFINE lr_fechas RECORD
        inicio        DATE,
        fin           DATE
    END RECORD

    DEFINE
        ldt_fecha_ini_mes           DATE

    DEFINE
        li_folio_previo             INTEGER

    DEFINE
        ls_exit                     ,
        ls_estado                   SMALLINT

    DEFINE
        lc_pregunta                 CHAR(65)    ,
        lc_mensaje                  CHAR(100)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETL8331 AT 4,4 WITH FORM "RETL8331" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)

    IF pc_input = "CON" THEN
        DISPLAY " RETL833    CONSULTA DE REGISTROS GENERADOS ANEXO CONSAR 1112                " AT 3,1 ATTRIBUTE(REVERSE)
    ELSE
        DISPLAY " RETL833               GENERACION DEL ANEXO CONSAR 1112                      " AT 3,1 ATTRIBUTE(REVERSE)
    END IF

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET ls_exit           = 0

    LET ldt_fecha_ini_mes   = MDY(MONTH(HOY),1,YEAR(HOY))

    LET lr_fechas.inicio  = f_lib_suma_mes(ldt_fecha_ini_mes, -1)
    LET lr_fechas.fin     = ldt_fecha_ini_mes - 1 UNITS DAY

    DISPLAY BY NAME lr_fechas.inicio
    DISPLAY BY NAME lr_fechas.fin


    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD inicio
            CALL f_lib_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                CALL f_lib_error_msg(lc_mensaje)
                NEXT FIELD inicio
            END IF

        AFTER FIELD fin
            CALL f_lib_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                CALL f_lib_error_msg(lc_mensaje)
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                    NEXT FIELD inicio
                END IF
            END IF

        ON KEY(ESC)
            CALL f_lib_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                CALL f_lib_error_msg(lc_mensaje)
                NEXT FIELD inicio
            END IF

            CALL f_lib_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                CALL f_lib_error_msg(lc_mensaje)
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                    NEXT FIELD inicio
                END IF
            END IF

            WHILE TRUE
                IF pc_input = "CON" THEN
                    LET lc_pregunta  = "¿ DESEA EJECUTAR LA CONSULTA DEL ANEXO ? (S/N) : "
                ELSE
                    LET li_folio_previo = f_valida_genera_previa(lr_fechas.*)

                    IF li_folio_previo <> 0 THEN
                        LET lc_pregunta  = "YA EXISTE UN ARCHIVO PARA ESE MES ¿GENERAR DE NUEVO? (S/N) : "
                    ELSE
                        LET lc_pregunta  = "¿ DESEA GENERAR EL ARCHIVO DEL ANEXO ? (S/N) :  "
                    END IF
                END IF

                PROMPT lc_pregunta CLIPPED FOR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN

                        IF li_folio_previo <> 0 THEN
                            CALL f_cancela_folio_ant(li_folio_previo)
                        END IF

                        LET ls_exit = 0
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET ls_exit = 1
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT, CONTROL-C)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_exit = 1
            EXIT INPUT

    END INPUT

    CLOSE WINDOW RETL8331

    RETURN lr_fechas.*, ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_extracto_temp : Genera el extracto de dis_cuenta que contiene    #
#                          unicamente los movimientos de retiros y de       #
#                          solicitud de saldo del periodo seleccionado      #
#                          Genera ademas el extracto de las resoluciones de #
#                          datamart para el periodo capturado               #
#---------------------------------------------------------------------------#
FUNCTION f_genera_extracto_temp(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    DEFINE
        ls_year                     SMALLINT 

    DEFINE
        lc_dis_tmp                  CHAR(0020)  ,
        lc_dis_cuenta               CHAR(0020)

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_mae_afiliado
        DROP TABLE tmp_dis_cuenta_1112
    WHENEVER ERROR STOP

    -- Creamos la tabla temporal de dis_cuenta
    SELECT *
    FROM   dis_cuenta
    WHERE  0 = 1
    INTO TEMP tmp_dis_cuenta_1112

    CREATE INDEX idx_tmp_cta1112_01
    ON tmp_dis_cuenta_1112 (tipo_movimiento, consecutivo_lote, nss)

    DISPLAY " OBTENIENDO DATOS DE LA CUENTA INDIVIDUAL ...              " AT 19,1 ATTRIBUTE(REVERSE)

    -- Verificamos si el periodo corresponde a un unico año
    IF ( YEAR(pr_fechas.fecha_ini) = YEAR(pr_fechas.fecha_fin) ) THEN
        LET lc_dis_cuenta = f_obtiene_tabla_cta(YEAR(pr_fechas.fecha_ini))
        CALL f_inserta_dis_cuenta(lc_dis_cuenta, pr_fechas.*)
    ELSE
        -- Hacemos la carga inicial
        LET ls_year         = YEAR(pr_fechas.fecha_fin)
        LET lc_dis_cuenta   = f_obtiene_tabla_cta(ls_year)
        LET lc_dis_tmp      = lc_dis_cuenta
        
        CALL f_inserta_dis_cuenta(lc_dis_cuenta, pr_fechas.*)
        
        WHILE ls_year >= YEAR(pr_fechas.fecha_ini)

            -- Si ya se proceso la tabla entonces no se insertan datos en dis_cuenta
            IF lc_dis_cuenta <> lc_dis_tmp THEN
                CALL f_inserta_dis_cuenta(lc_dis_cuenta, pr_fechas.*)
                LET lc_dis_tmp = lc_dis_cuenta
            END IF 
            
            LET ls_year         = ls_year - 1
            LET lc_dis_cuenta   = f_obtiene_tabla_cta(ls_year)
        END WHILE 

    END IF

    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta_1112

    -- Creamos la tabla temporal de afiliados
    SELECT *
    FROM   afi_mae_afiliado
    WHERE  0 = 1
    INTO TEMP tmp_mae_afiliado
    
    INSERT INTO tmp_mae_afiliado
    SELECT *
    FROM   afi_mae_afiliado
    WHERE  n_seguro IN (SELECT UNIQUE(nss)
                        FROM   tmp_dis_cuenta_1112
                        GROUP BY 1
                        )

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_detalle_anexo : Obtiene el detalle de cada registro del layout   #
#                          segun lo indicado para el anexo 1112             #
#---------------------------------------------------------------------------#
FUNCTION f_genera_detalle_anexo(pr_fechas_anexo)

    DEFINE pr_fechas_anexo  RECORD
        folio               INTEGER ,
        inicial             DATE    ,
        final               DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY " GENERANDO ENTRADAS Y SALIDAS  ...                         " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Genera Detalle de entradas
    CALL f_genera_entradas(pr_fechas_anexo.*)
    CALL f_genera_entradas_ISSSTE(pr_fechas_anexo.*)
    CALL f_actualiza_tot_entradas(pr_fechas_anexo.folio)

    -- Genera Detalle de salidas
    CALL f_genera_salidas(pr_fechas_anexo.*)
    CALL f_genera_salidas_ISSSTE(pr_fechas_anexo.*)
    CALL f_actualiza_tot_salidas(pr_fechas_anexo.*)

END FUNCTION



#---------------------------------------------------------------------------#
# f_genera_plano_1112 : Genera el archivo plano con el layout del anexo 1112#
#---------------------------------------------------------------------------#
FUNCTION f_genera_plano_1112()

    DEFINE lr_anexo         RECORD LIKE ret_det_anexo1112.*
    DEFINE lr_movs_anexo    RECORD LIKE tab_movimientos_anexo1112.*
    DEFINE lr_envio         RECORD LIKE ret_envio.*

    DEFINE
        lc_nom_plano                CHAR(025)   ,
        lc_ruta_reporte             CHAR(300)

    DEFINE
        ls_num_entradas             SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY " GENERANDO ARCHIVO DE REPORTE ...                          " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    INITIALIZE lr_anexo.* TO NULL
    INITIALIZE lr_movs_anexo.* TO NULL
    INITIALIZE lr_envio.* TO NULL

    LET lc_nom_plano    = f_arma_nom_archivo()
    LET lc_ruta_reporte = gs_modulo.ruta_listados CLIPPED, "/", lc_nom_plano

    SELECT COUNT(*) + 1
    INTO   ls_num_entradas
    FROM   tab_movimientos_anexo1112

    START REPORT rpt_1112 TO lc_ruta_reporte

    -- Se insertan las Entradas IMSS e ISSSTE
    DECLARE cur_entradas CURSOR FOR
        SELECT A.*
        FROM   safre_tmp:tmp_det_anexo1112 A    ,
               tab_movimientos_anexo1112 B
        WHERE  A.id_movimiento  = B.id_movimiento
        AND    B.tipo_cuenta    = "01"
        ORDER BY B.tipo_cuenta      ,
                 B.cuenta           ,
                 B.subcuenta        ,
                 B.sub_subcuenta

    FOREACH cur_entradas INTO lr_anexo.*

        SELECT *
        INTO   lr_movs_anexo.*
        FROM   tab_movimientos_anexo1112
        WHERE  id_movimiento    = lr_anexo.id_movimiento

        OUTPUT TO REPORT rpt_1112(lr_anexo.*, lr_movs_anexo.*, ls_num_entradas)

    END FOREACH

    INITIALIZE lr_anexo.* TO NULL

    -- Se insertan las Salidas por Transferencia IMSS e ISSSTE
    DECLARE cur_trans CURSOR FOR
        SELECT A.*
        FROM   safre_tmp:tmp_det_anexo1112 A    ,
               tab_movimientos_anexo1112 B
        WHERE  A.id_movimiento  = B.id_movimiento
        AND    B.tipo_cuenta    = "02"
        AND    B.cuenta         MATCHES "*1" -- 01 y 51
        ORDER BY B.tipo_cuenta      ,
                 B.cuenta           ,
                 B.subcuenta        ,
                 B.sub_subcuenta

    FOREACH cur_trans INTO lr_anexo.*

        SELECT *
        INTO   lr_movs_anexo.*
        FROM   tab_movimientos_anexo1112
        WHERE  id_movimiento    = lr_anexo.id_movimiento

        OUTPUT TO REPORT rpt_1112(lr_anexo.*, lr_movs_anexo.*, ls_num_entradas)

    END FOREACH

    INITIALIZE lr_anexo.* TO NULL

    -- Se insertan las Salidas por Disposiciones IMSS e ISSSTE
    DECLARE cur_disp CURSOR FOR
        SELECT A.*
        FROM   safre_tmp:tmp_det_anexo1112 A    ,
               tab_movimientos_anexo1112 B
        WHERE  A.id_movimiento  = B.id_movimiento
        AND    B.tipo_cuenta    = "02"
        AND    B.cuenta         MATCHES "*2" -- 02 y 52
        ORDER BY B.tipo_cuenta      ,
                 B.cuenta           ,
                 B.subcuenta        ,
                 B.sub_subcuenta


    FOREACH cur_disp INTO lr_anexo.*

        SELECT *
        INTO   lr_movs_anexo.*
        FROM   tab_movimientos_anexo1112
        WHERE  id_movimiento    = lr_anexo.id_movimiento

        OUTPUT TO REPORT rpt_1112(lr_anexo.*, lr_movs_anexo.*, ls_num_entradas)

    END FOREACH

    INITIALIZE lr_anexo.* TO NULL

    -- Se insertan las Salidas por Reinversiones IMSS e ISSSTE
    DECLARE cur_reinv CURSOR FOR
        SELECT A.*
        FROM   safre_tmp:tmp_det_anexo1112 A    ,
               tab_movimientos_anexo1112 B
        WHERE  A.id_movimiento  = B.id_movimiento
        AND    B.tipo_cuenta    = "02"
        AND    B.cuenta         MATCHES "*3" -- 03 y 53
        ORDER BY B.tipo_cuenta      ,
                 B.cuenta           ,
                 B.subcuenta        ,
                 B.sub_subcuenta


    FOREACH cur_reinv INTO lr_anexo.*

        SELECT *
        INTO   lr_movs_anexo.*
        FROM   tab_movimientos_anexo1112
        WHERE  id_movimiento    = lr_anexo.id_movimiento

        OUTPUT TO REPORT rpt_1112(lr_anexo.*, lr_movs_anexo.*, ls_num_entradas)

    END FOREACH

    INITIALIZE lr_anexo.* TO NULL

    FINISH REPORT rpt_1112

    -- Se actualiza el registro de envio del archivo
    LET lr_envio.folio              = 1
    LET lr_envio.tipo_operacion     = 1112
    LET lr_envio.fecha_envio        = HOY
    LET lr_envio.hora_envio         = CURRENT HOUR TO SECOND
    LET lr_envio.nom_archivo        = lc_nom_plano
    LET lr_envio.tot_registros      = ls_num_entradas
    LET lr_envio.usuario            = gc_usuario
    LET lr_envio.estado             = gr_edo.enviado

    INSERT INTO safre_tmp:tmp_envio
    VALUES (lr_envio.*)

    CALL f_lib_borra_lineas(gs_modulo.ruta_listados, lc_nom_plano)

    DISPLAY "EL ARCHIVO FUE GENERADO EN LA RUTA : ", gs_modulo.ruta_listados AT 13,6
    DISPLAY "CON EL NOMBRE : ", lc_nom_plano AT 14,27


END FUNCTION

#---------------------------------------------------------------------------#
# f_consolida_tablas : Inserta los valores de las tablas temporales a las   #
#                      tablas fisicas finales                               #
#---------------------------------------------------------------------------#
FUNCTION f_consolida_tablas()

    DEFINE li_ult_folio LIKE ret_envio.folio

    -- -----------------------------------------------------------------------------

    DISPLAY " CONSOLIDANDO TABLAS FISICAS ...                           " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_ult_folio = f_lib_obtiene_ult_folio()

    DISPLAY "FOLIO DE ENVIO             : ", li_ult_folio AT 16,6

    -- Copiamos el envio de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_envio
    SET    folio    = li_ult_folio
    WHERE  folio    = 1

    INSERT INTO ret_envio
    SELECT *
    FROM   safre_tmp:tmp_envio
    WHERE  folio    = li_ult_folio

    -- Copiamos el detalle de reinversiones de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_detalle_reinversion_1112
    SET    folio_anexo  = li_ult_folio
    WHERE  folio_anexo  = 1

    INSERT INTO ret_detalle_reinversion_1112
    SELECT *
    FROM   safre_tmp:tmp_detalle_reinversion_1112
    WHERE  folio_anexo  = li_ult_folio

    -- Copiamos el detalle de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_det_anexo1112
    SET    folio    = li_ult_folio
    WHERE  folio    = 1

    INSERT INTO ret_det_anexo1112
    SELECT *
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  folio    = li_ult_folio

    DISPLAY "                                                           " AT 19,1
    CALL f_lib_error_msg("PROCESO TERMINADO SATISFACTORIAMENTE")

END FUNCTION

-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_consulta_anexo_1112 : Realiza la consulta de los registros generados    #
#                         previamente del anexo CONSAR 1112                 #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_anexo_1112()

    DEFINE lr_fechas_anexo  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE
        ls_salida           SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_salida   = 0

    CALL f_captura_fechas("CON") RETURNING lr_fechas_anexo.*, ls_salida

    IF ls_salida = 0 THEN

        CALL f_tablas_tmp_consulta()

        -- Obtiene los datos del reporte
        CALL f_genera_datos_consulta(lr_fechas_anexo.*)

        -- Muestra la salida
        CALL f_despliega_datos()
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_datos_consulta : Obtiene los datos de acuerdo a los criterios de #
#                           busqueda insertados por el usuario y los        #
#                           almacena en la tabla temporal desde donde se    #
#                           recuperan para desplegarse                      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_datos_consulta(pr_fechas)

    DEFINE pr_fechas RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE lr_datos_tmp RECORD
        desc_tipo_cuenta        CHAR(08)       ,
        cuenta                  CHAR(02)       ,
        subcuenta               CHAR(02)       ,
        sub_subcuenta           CHAR(02)       ,
        descripcion             CHAR(80)       ,
        fecha_inicio            DATE           ,
        num_solicitudes         INTEGER        ,
        mto_mes_actual          DECIMAL(12,2)  ,
        folio                   INTEGER        ,
        fecha_fin               DATE           ,
        mto_mes_anterior        DECIMAL(12,2)  ,
        mto_acumulado_anual     DECIMAL(12,2)  ,
        nom_archivo             CHAR(25)
    END RECORD

    DEFINE
        lc_tipo_cta             CHAR(02)        ,
        lc_query                CHAR(2000)

    DEFINE
        li_folio                INTEGER

    -- -----------------------------------------------------------------------------

    DECLARE cur_folios CURSOR FOR
        SELECT UNIQUE(folio)
        FROM   ret_det_anexo1112
        WHERE  fecha_inicio BETWEEN pr_fechas.inicial AND pr_fechas.final
        AND    fecha_fin    BETWEEN pr_fechas.inicial AND pr_fechas.final

    LET lc_query    = " SELECT C.tipo_cuenta            ,         ",
                      "        ' '                      ,         ",
                      "        C.cuenta                 ,         ",
                      "        C.subcuenta              ,         ",
                      "        C.sub_subcuenta          ,         ",
                      "        C.descripcion            ,         ",
                      "        B.fecha_inicio           ,         ",
                      "        B.num_solicitudes        ,         ",
                      "        B.mto_mes_actual         ,         ",
                      "        B.folio                  ,         ",
                      "        B.fecha_fin              ,         ",
                      "        B.mto_mes_anterior       ,         ",
                      "        B.mto_acumulado_anual    ,         ",
                      "        A.nom_archivo                      ",
                      " FROM   ret_envio                   A    , ",
                      "        ret_det_anexo1112           B    , ",
                      "        tab_movimientos_anexo1112   C      ",
                      " WHERE  B.folio          = ?               ",
                      " AND    A.folio          = B.folio         ",
                      " AND    A.estado         = ?               ",
                      " AND    B.id_movimiento  = C.id_movimiento ",
                      " ORDER BY C.tipo_cuenta, B.id_movimiento   "

    LET lc_query = lc_query CLIPPED

    PREPARE prp_datos FROM lc_query
    DECLARE cur_datos CURSOR FOR prp_datos

    FOREACH cur_folios INTO li_folio

        FOREACH cur_datos USING li_folio, gr_edo.enviado
                          INTO  lc_tipo_cta, lr_datos_tmp.*

            IF lc_tipo_cta = "01" THEN
                LET lr_datos_tmp.desc_tipo_cuenta = "ENTRADA"
            ELSE
                LET lr_datos_tmp.desc_tipo_cuenta = "SALIDA"
            END IF

            INSERT INTO safre_tmp:tmp_datos_reporte
            VALUES (lr_datos_tmp.*)

        END FOREACH

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_datos : Muestra en pantalla los datos de la busqueda y        #
#                     ejecuta las funciones de busqueda detallada           #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_datos()

    DEFINE lar_datos_anexo ARRAY[5000] OF RECORD
        desc_tipo_cuenta        CHAR(08)        ,
        cuenta                  CHAR(02)        ,
        subcuenta               CHAR(02)        ,
        sub_subcuenta           CHAR(02)        ,
        descripcion             CHAR(80)        ,
        fecha_inicio            DATE            ,
        num_solicitudes         INTEGER         ,
        mto_mes_actual          DECIMAL(12,2)   ,
        folio                   INTEGER         ,
        fecha_fin               DATE            ,
        mto_mes_anterior        DECIMAL(12,2)   ,
        mto_acumulado_anual     DECIMAL(12,2)   ,
        nom_archivo             CHAR(25)
    END RECORD

    DEFINE lr_soli RECORD
        desc_tipo_cuenta        CHAR(08)        ,
        cuenta                  CHAR(02)        ,
        subcuenta               CHAR(02)        ,
        sub_subcuenta           CHAR(02)        ,
        descripcion             CHAR(80)        ,
        fecha_inicio            DATE            ,
        num_solicitudes         INTEGER         ,
        mto_mes_actual          DECIMAL(12,2)   ,
        folio                   INTEGER         ,
        fecha_fin               DATE            ,
        mto_mes_anterior        DECIMAL(12,2)   ,
        mto_acumulado_anual     DECIMAL(12,2)   ,
        nom_archivo             CHAR(25)
    END RECORD

    DEFINE
        li_tot_elem         ,
        li_arr_elem         ,
        li_scr_elem         ,
        li_cont             INTEGER

    DEFINE
        ls_flag             SMALLINT

    DEFINE
        lc_display          CHAR(80)    ,
        lc_ruta_arch        CHAR(80)    ,
        lc_marca            CHAR(1)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retl8333 AT 4,4 WITH FORM "RETL8333" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL833    CONSULTA DE REGISTROS GENERADOS ANEXO CONSAR 1112                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET li_cont = 1
    LET ls_flag = 0

    DECLARE cur_screen CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_datos_reporte
    ORDER BY folio, desc_tipo_cuenta, cuenta

    FOREACH cur_screen INTO lr_soli.*

        LET lar_datos_anexo[li_cont].desc_tipo_cuenta       = lr_soli.desc_tipo_cuenta
        LET lar_datos_anexo[li_cont].cuenta                 = lr_soli.cuenta
        LET lar_datos_anexo[li_cont].subcuenta              = lr_soli.subcuenta
        LET lar_datos_anexo[li_cont].sub_subcuenta          = lr_soli.sub_subcuenta
        LET lar_datos_anexo[li_cont].descripcion            = lr_soli.descripcion
        LET lar_datos_anexo[li_cont].fecha_inicio           = lr_soli.fecha_inicio
        LET lar_datos_anexo[li_cont].num_solicitudes        = lr_soli.num_solicitudes
        LET lar_datos_anexo[li_cont].mto_mes_actual         = lr_soli.mto_mes_actual
        LET lar_datos_anexo[li_cont].folio                  = lr_soli.folio
        LET lar_datos_anexo[li_cont].fecha_fin              = lr_soli.fecha_fin
        LET lar_datos_anexo[li_cont].mto_mes_anterior       = lr_soli.mto_mes_anterior
        LET lar_datos_anexo[li_cont].mto_acumulado_anual    = lr_soli.mto_acumulado_anual
        LET lar_datos_anexo[li_cont].nom_archivo            = lr_soli.nom_archivo

        LET li_cont = li_cont + 1

        IF li_cont > 5000 THEN
            LET li_cont = li_cont - 1
            PROMPT "CAPACIDAD DE ARREGLO REBASADA, SE MUESTRAN ", li_cont, " REGISTROS." FOR CHAR enter
            LET li_cont = li_cont + 1
            EXIT FOREACH
        END IF

    END FOREACH

    LET li_cont = li_cont - 1

    IF li_cont = 0 THEN
        CLOSE WINDOW retl8333
        CALL f_lib_error_msg("NO EXISTEN REGISTROS CON EL CRITERIO CAPTURADO")
        RETURN
    END IF

    CALL SET_COUNT(li_cont)
    DISPLAY ARRAY lar_datos_anexo TO scr_1112.*

    CLOSE WINDOW retl8333

END FUNCTION


-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------#
# f_obtiene_montos_actual : Obtiene el numero de solicitudes y el monto     #
#                           total del rubro indicado en la condicion        #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_montos_actual(pc_condicion)

    DEFINE
        pc_condicion            CHAR(300)

    DEFINE
        lc_query                CHAR(600)

    DEFINE lr_resultado RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes  ,
        mto_mes_actual          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    -- -----------------------------------------------------------------------------

    -- Obtenemos el total de solicitudes

    LET lc_query = " SELECT COUNT(UNIQUE nss)    ",
                   " FROM   tmp_dis_cuenta_1112  ",
                   pc_condicion CLIPPED

    PREPARE prp_soli FROM lc_query
    EXECUTE prp_soli INTO lr_resultado.num_solicitudes

    LET lc_query = " "

    -- Obtenemos el monto total

    LET lc_query = " SELECT NVL(SUM(monto_en_pesos),0) ",
                   " FROM   tmp_dis_cuenta_1112  ",
                   pc_condicion CLIPPED

    PREPARE prp_mto FROM lc_query
    EXECUTE prp_mto INTO lr_resultado.mto_mes_actual

    IF lr_resultado.mto_mes_actual < 0 THEN
        LET lr_resultado.mto_mes_actual = f_lib_redondea_val(lr_resultado.mto_mes_actual, 2) * -1
    END IF

    RETURN lr_resultado.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_montos_historia : Obtiene el monto del periodo anterior         #
#                             generado y del acumulado anual                #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_montos_historia(pr_historico)

    DEFINE pr_historico  RECORD
        id_movimiento           SMALLINT    ,
        folio                   INTEGER     ,
        inicial                 DATE        ,
        final                   DATE
    END RECORD

    DEFINE lr_mto_hist RECORD
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    DEFINE
        ls_mes_anterior         ,
        ls_anio_anterior        SMALLINT

    DEFINE
        li_folio_ant            INTEGER

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_mto_hist.* TO NULL
    LET li_folio_ant = 0

    -- Determina la fecha del mes anterior al periodo actual
    LET ls_mes_anterior = MONTH(pr_historico.inicial)

    IF ls_mes_anterior = 1 THEN
        LET ls_mes_anterior = 12
    ELSE
        LET ls_mes_anterior = ls_mes_anterior - 1
    END IF

    LET ls_anio_anterior = YEAR(pr_historico.inicial)

    IF ls_mes_anterior = 12 THEN
        LET ls_anio_anterior =  ls_anio_anterior - 1
    END IF

    -- Obtiene el folio del periodo anterior
    SELECT NVL(A.folio, 0)
    INTO   li_folio_ant
    FROM   ret_det_anexo1112    A   ,
           ret_envio            B
    WHERE  A.folio                  = B.folio
    AND    ( MONTH(A.fecha_inicio) <= ls_mes_anterior AND MONTH(A.fecha_fin) >= ls_mes_anterior)
    AND    ( YEAR(A.fecha_inicio) <= ls_anio_anterior AND YEAR(A.fecha_fin) >= ls_anio_anterior)
    AND    A.id_movimiento          = pr_historico.id_movimiento
    AND    B.estado                 = gr_edo.enviado
    GROUP BY 1

    IF li_folio_ant = 0 THEN
        LET lr_mto_hist.mto_mes_anterior    = 0
        LET lr_mto_hist.mto_acumulado_anual = 0
    ELSE
        SELECT NVL(mto_mes_actual, 0)       ,
               NVL(mto_acumulado_anual, 0)
        INTO   lr_mto_hist.*
        FROM   ret_det_anexo1112
        WHERE  folio                = li_folio_ant
        AND    id_movimiento        = pr_historico.id_movimiento

        IF lr_mto_hist.mto_mes_anterior IS NULL THEN
            LET lr_mto_hist.mto_mes_anterior = 0
        END IF

        IF lr_mto_hist.mto_acumulado_anual IS NULL THEN
            LET lr_mto_hist.mto_acumulado_anual = 0
        END IF

        -- El acumulado anual para Febrero debe ser el corte de Enero
        IF MONTH(pr_historico.inicial) = 2 THEN
            LET lr_mto_hist.mto_acumulado_anual = 0
        END IF

        -- Actualizamos el acumulado anual con el monto del mes anterior
        LET lr_mto_hist.mto_acumulado_anual = lr_mto_hist.mto_acumulado_anual + lr_mto_hist.mto_mes_anterior

    END IF -- Folio = 0

    RETURN lr_mto_hist.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_arma_nom_archivo : Arma el nombre del archivo del anexo de acuerdo a lo #
#                      solicitado por CONSAR                                #
#---------------------------------------------------------------------------#
FUNCTION f_arma_nom_archivo()

    DEFINE
        lc_nombre   CHAR(25)

    -- -----------------------------------------------------------------------------

    LET lc_nombre[01,08]    = HOY USING "YYYYMMDD"
    LET lc_nombre[09,12]    = "_AF_"
    LET lc_nombre[13,15]    = gs_codigo_afore USING "&&&"
    LET lc_nombre[16,25]    = "_000.1112"

    RETURN lc_nombre

END FUNCTION

#---------------------------------------------------------------------------#
# f_cancela_folio_ant : Pone en estado rechazado el folio indicado          #
#---------------------------------------------------------------------------#
FUNCTION f_cancela_folio_ant(li_folio)

    DEFINE
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------

    UPDATE ret_envio
    SET    estado   = gr_edo.rechazado
    WHERE  folio    = li_folio
    AND    estado   = gr_edo.enviado

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_genera_previa : Verifica si exisiten generaciones previas en el  #
#                          rango ingresado                                  #
#---------------------------------------------------------------------------#
FUNCTION f_valida_genera_previa(pr_fechas)

    DEFINE pr_fechas  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------

    SELECT MAX(A.folio)
    INTO   li_folio
    FROM   ret_det_anexo1112 A  ,
           ret_envio B
    WHERE  A.folio          = B.folio
    AND    B.estado         = gr_edo.enviado
    AND    A.fecha_inicio   = pr_fechas.inicial
    AND    A.fecha_fin      = pr_fechas.final

    IF li_folio IS NULL THEN
        LET li_folio    = 0
    END IF

    RETURN li_folio

END FUNCTION


#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  liquidacion de retiros issste                            #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retl8332 AT 4,4 WITH FORM "RETL8332" ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL833           GENERA ARCHIVO DE ANEXO CONSAR 1112                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_tabla_cta : Dada una fecha determina si la tabla de dis_cuenta  #
#                       es la actual o si corresponde a una tabla historica.#
#                       Verifica tambien que exista dicha tabla historica   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_tabla_cta(ps_anio)

    DEFINE
        ps_anio                 SMALLINT

    DEFINE
        lc_anio_tmp             CHAR(04)    ,
        ld_tabla                CHAR(20)

    -- -----------------------------------------------------------------------------

    -- Si el periodo esta dentro del año actual se toma dis_cuenta
    IF (YEAR(HOY) = ps_anio) THEN
        LET ld_tabla = "dis_cuenta"
    ELSE
        -- Se verifica que exista la tabla historica, en caso contrario se toma dis_cuenta
        LET lc_anio_tmp     = ps_anio USING "&&&&"
        LET ld_tabla        = "dis_cuenta", lc_anio_tmp[3,4]
    
        SELECT "OK"
        FROM   SYSTABLES
        WHERE  tabname MATCHES ld_tabla
        GROUP BY 1
    
        IF STATUS = NOTFOUND THEN
            LET ld_tabla = "dis_cuenta"
        END IF
    
    END IF
    
    RETURN ld_tabla

END FUNCTION 

#---------------------------------------------------------------------------#
# f_inserta_dis_cuenta : Inserta en la tabla temporal los registros de la   #
#                        cuenta individual correspondientes a la tabla y el #
#                        periodo indicado                                   #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_dis_cuenta(pr_cuenta)

    DEFINE pr_cuenta RECORD
        tabla           CHAR(20)    ,
        fec_ini         DATE        ,
        fec_fin         DATE         
    END RECORD 

    DEFINE
        lc_dis_cta              CHAR(5000)
    -- -----------------------------------------------------------------------------
    
    LET lc_dis_cta = " INSERT INTO tmp_dis_cuenta_1112 \n"    ,
                     " SELECT *                        \n"    ,
                     " FROM  ", pr_cuenta.tabla , "\n" ,
                     " WHERE  fecha_conversion BETWEEN ? AND ? \n" ,
                     " AND   (   tipo_movimiento = 83                 \n" , -- Devoluciones por Desempleo
                     "        OR tipo_movimiento = 490                \n" , -- Voluntarias
                     "        OR tipo_movimiento BETWEEN 800 AND 899  \n" , -- Retiros IMSS e ISSSTE
                     "        OR tipo_movimiento BETWEEN 921 AND 924) \n"   -- Cargos y Abonos por Desinversion/Reinversion

    PREPARE prp_cta_normal FROM  lc_dis_cta
    EXECUTE prp_cta_normal USING pr_cuenta.fec_ini  ,
                                 pr_cuenta.fec_fin

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp_genera : Genera las tablas temporales donde se almacenan los #
#                       calculos y cambios de la generacion del anexo 1112  #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp_genera()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_envio
        DROP TABLE tmp_det_anexo1112
        DROP TABLE tmp_detalle_reinversion_1112
    WHENEVER ERROR STOP

    --------------------------------

    CREATE TABLE tmp_envio (
        folio                   INTEGER                 ,
        tipo_operacion          SMALLINT                ,
        fecha_envio             DATE                    ,
        hora_envio              DATETIME HOUR TO SECOND ,
        nom_archivo             CHAR(25)                ,
        tot_registros           INTEGER                 ,
        usuario                 CHAR(15)                ,
        estado                  SMALLINT
    )

    GRANT ALL ON tmp_envio TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_det_anexo1112(
        folio                   INTEGER             ,
        id_movimiento           SMALLINT NOT NULL   ,
        fecha_inicio            DATE                ,
        fecha_fin               DATE                ,
        num_solicitudes         INTEGER             ,
        mto_mes_actual          DECIMAL(12,2)       ,
        mto_mes_anterior        DECIMAL(12,2)       ,
        mto_acumulado_anual     DECIMAL(12,2)
      )

    CREATE INDEX tmp_dt1112_01
    ON tmp_det_anexo1112(folio)

    CREATE INDEX tmp_dt1112_02
    ON tmp_det_anexo1112(id_movimiento, fecha_inicio, fecha_fin)

    GRANT ALL ON tmp_det_anexo1112 TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_detalle_reinversion_1112(
        nss             CHAR(11)        ,
        folio_anexo     INTEGER         ,
        mto_pago        DECIMAL(12,2)   ,
        tipo_origen     SMALLINT
      )

    GRANT ALL ON tmp_detalle_reinversion_1112 TO PUBLIC

    --------------------------------

    DATABASE safre_af

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp_consulta : Genera las tablas temporales donde se almacenan los #
#                       calculos y cambios de la generacion del anexo 1112  #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp_consulta()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_datos_reporte
    WHENEVER ERROR STOP

    --------------------------------

    CREATE TABLE tmp_datos_reporte (
        desc_tipo_cuenta        CHAR(08)                ,
        cuenta                  CHAR(02)                ,
        subcuenta               CHAR(02)                ,
        sub_subcuenta           CHAR(02)                ,
        descripcion             CHAR(80)                ,
        fecha_inicio            DATE                    ,
        num_solicitudes         INTEGER                 ,
        mto_mes_actual          DECIMAL(12,2)           ,
        folio                   INTEGER                 ,
        fecha_fin               DATE                    ,
        mto_mes_anterior        DECIMAL(12,2)           ,
        mto_acumulado_anual     DECIMAL(12,2)           ,
        nom_archivo             CHAR(25)
    )

    GRANT ALL ON tmp_datos_reporte TO PUBLIC

    --------------------------------

    DATABASE safre_af

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_1112 : Reporte que genera el archivo plano de anexo 1112              #
#---------------------------------------------------------------------------#
REPORT rpt_1112(pr_dat_anexo, pr_movs, ps_num_entradas)

    DEFINE pr_dat_anexo RECORD LIKE ret_det_anexo1112.*
    DEFINE pr_movs      RECORD LIKE tab_movimientos_anexo1112.*

    DEFINE
        lc_importe_13               CHAR(13),
        lc_mto_mes_actual           CHAR(12),
        lc_mto_mes_anterior         CHAR(12),
        lc_mto_acumulado_anual      CHAR(12)

    DEFINE
        ps_num_entradas             SMALLINT

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   100
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        -- ENCABEZADO
        FIRST PAGE HEADER

            PRINT
                COLUMN 001, "000"                                   , -- Tipo de registro
                COLUMN 004, "1112"                                  , -- Tipo de archivo
                COLUMN 008, "001"                                   , -- Tipo entidad origen
                COLUMN 011, gs_codigo_afore USING "&&&"             , -- Clave entidad origen
                COLUMN 014, HOY USING "YYYYMMDD"                    , -- Fecha de envio
                COLUMN 022, "073"                                   , -- Tamanio del registro
                COLUMN 025, ps_num_entradas USING "&&&&&"           , -- Total de registros enviados
                COLUMN 030, 44 SPACES

        -- DETALLE
        ON EVERY ROW

            LET lc_importe_13           = pr_dat_anexo.mto_mes_actual USING "&&&&&&&&&&.&&"
            LET lc_mto_mes_actual       = lc_importe_13[01,10],
                                          lc_importe_13[12,13]

            LET lc_importe_13           = pr_dat_anexo.mto_mes_anterior USING "&&&&&&&&&&.&&"
            LET lc_mto_mes_anterior     = lc_importe_13[01,10],
                                          lc_importe_13[12,13]

            LET lc_importe_13           = pr_dat_anexo.mto_acumulado_anual USING "&&&&&&&&&&.&&"
            LET lc_mto_acumulado_anual  = lc_importe_13[01,10],
                                          lc_importe_13[12,13]

            PRINT
                COLUMN 001, "301"                                           , -- Tipo de registro
                COLUMN 004, pr_dat_anexo.fecha_inicio USING "YYYYMMDD"      , -- Fecha inicio
                COLUMN 012, pr_dat_anexo.fecha_fin USING "YYYYMMDD"         , -- Fecha fin
                COLUMN 020, pr_movs.tipo_cuenta                             , -- Tipo cuenta
                COLUMN 022, pr_movs.cuenta                                  , -- Cuenta
                COLUMN 024, pr_movs.subcuenta                               , -- SubCuenta
                COLUMN 026, pr_movs.sub_subcuenta                           , -- Sub SubCuenta
                COLUMN 028, pr_dat_anexo.num_solicitudes USING "&&&&&&&&&&" , -- Numero de solicitudes
                COLUMN 038, lc_mto_mes_actual                               , -- Monto mes actual
                COLUMN 050, lc_mto_mes_anterior                             , -- Monto mes anterior
                COLUMN 062, lc_mto_acumulado_anual                            -- Monto acumulado anual

END REPORT
