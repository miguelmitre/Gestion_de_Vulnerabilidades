#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC810  => RECEPCIONA ARCHIVOS DE RETIROS ENVIADOS POR PROCESAR      #
#Fecha creacion    => 20 DE ENERO DE 2004                                       #
#By                => FRANCO ULLOA VIDELA                                       #
#Fecha actualiz.   => 16-01-2008                                                #
#Fecha actualiz.   => 03-03-2008                                                #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 3 DE OCTUBRE DE 2008                                      #
#                     Modificaciones aceptar el campo monto constitutivo en el  #
#                     layout de transferencias, tipo retiro A                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 20 DE ABRIL DE 2010                                       #
#                  => Se modifica la insercion en la tabla ret_cza_datamart     #
#                     debido a los cambios hechos para el modulo de PMG         #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 21 DE JUNIO DE 2010                                       #
#                  => Se modifica para realizar el manejo de la carga con       #
#                     deteccion de errores y bitacora. Se modifica para         #
#                     permitir la carga del campo de clave de aseguradora       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 11 DE MAYO DE 2011                                        #
#                  => Se realizan las modificaciones al codigo para incluir     #
#                     el llamado de las funciones para realizar el marcaje de   #
#                     trabajador pensionado (REQ. EFPS-157 )                    #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 10 DE DICIEMBRE DE 2013                                   #
#                  => Se modifica para que obtenga desde los catalogos si el    #
#                     tipo de retiro corresponde a disposicion o transferencia  #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gs_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_dat RECORD
        nom_archivo           CHAR(20)
    END RECORD

    DEFINE gr_tramite RECORD
        disp            CHAR(2) ,
        transf          CHAR(2) ,
        hist            CHAR(2)
    END RECORD

    DEFINE gr_edo RECORD
        enviado         LIKE ret_estado.estado_solicitud ,
        recibido        LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        gc_usuario              CHAR(012),
        gc_ruta_archivo         CHAR(200),
        gc_tipo_operacion       CHAR(002),
        enter                   CHAR(001)

    DEFINE
        gs_tipo_disposicion     ,
        gs_tipo_transferencia   ,
        gs_flag_err             ,
        gs_flag_proceso         ,
        gs_codigo_afore         ,
        gs_cod_inv              SMALLINT

    DEFINE
        gi_proceso              INTEGER

    DEFINE
        HOY                     DATE

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC810")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC810             RECEPCION DE ARCHIVOS DE RETIROS IMSS              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "ARCHIVOS"
        COMMAND "Carga archivo" "Carga el Archivo de Retiros IMSS"
            CALL f_genera_carga()

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Carga"
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

    LET HOY  = TODAY
    LET gr_tramite.transf   = "02"
    LET gr_tramite.disp     = "06"
    LET gr_tramite.hist     = "19"

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore    ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_cod_inv
    FROM   tab_afore
    WHERE  afore_desc MATCHES '*INVERCAP*'

    ----- TIPOS DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_tipo_disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    SELECT cod_tramite
    INTO   gs_tipo_transferencia
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- MARCAJE -----
    LET lc_prepare = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM lc_prepare

    LET lc_prepare = " "

    ----- ULTIMO CONSECUTIVO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_ret_consecutivo() "
    PREPARE eje_consecutivo FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de retiros IMSS #
#                  que el usuario seleccione                                #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    DEFINE
        lc_tramite          CHAR(02)

    -- -----------------------------------------------------------------------------

    CALL f_carga_archivo() RETURNING lc_tramite       ,
                                     gs_flag_proceso  ,
                                     gi_proceso

    IF gs_flag_proceso = 1 THEN #-- La carga se realizo sin errores

        CALL primer_paso(lc_tramite)        #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso(lc_tramite)       #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err, gi_proceso

        IF gs_flag_err = 0 THEN
            CALL tercer_paso(lc_tramite)    #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            PROMPT " SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO ... <ENTER> PARA MOSTRAR" FOR CHAR enter

            CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
        END IF


    ELSE
        IF gi_proceso <> 0 THEN
            DISPLAY "                                             " AT 18,1
            PROMPT " ARCHIVO CON INCONSISTENCIAS DE ESTRUCTURA ... <ENTER> PARA MOSTRAR" FOR CHAR enter
            CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

    CLEAR SCREEN
    CLOSE WINDOW retc8101

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de     #
#                   rescate si existe. En caso de existir, lo carga sin     #
#                   formato en la tabla temporal                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    DEFINE
        ls_flag                 ,
        ls_procesa              SMALLINT

    DEFINE
        li_num_reg              INTEGER

    DEFINE
        lc_tipo_tramite         CHAR(02)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc8101 AT 4,4 WITH FORM "RETC8101" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC810            CARGA DE ARCHIVO DE RETIROS IMSS                           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET li_num_reg  = 0
    LET ls_procesa  = 0

    INPUT BY NAME gr_dat.nom_archivo WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo
            LET gr_dat.nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
            IF gr_dat.nom_archivo IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO  "
                NEXT FIELD nom_archivo
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = gr_dat.nom_archivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " ARCHIVO YA PROCESADO CON ANTERIORIDAD  "
                NEXT FIELD nom_archivo
            END IF

            LET gc_ruta_archivo = gs_modulo.ruta_rescate CLIPPED,"/",
                                  gr_dat.nom_archivo CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM gc_ruta_archivo
            INSERT INTO tmp_arch_carga

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_num_reg
            FROM   tmp_arch_carga

            IF li_num_reg = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo
            ELSE
                WHILE TRUE
                    PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN

                            CALL f_valida_archivo() RETURNING lc_tipo_tramite   ,
                                                              ls_flag           ,
                                                              li_id_proc

                            IF ls_flag = 1 THEN -- hubo errores en la carga
                                LET ls_procesa  = 0
                            ELSE
                                LET ls_procesa  = 1
                            END IF

                            EXIT INPUT
                        ELSE
                            PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                            LET ls_procesa  = 0
                            EXIT INPUT
                        END IF
                    END IF
                END WHILE
            END IF

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
    END INPUT

    RETURN lc_tipo_tramite, ls_procesa, li_id_proc

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de transferencias  #
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pc_tramite)

    DEFINE
        pc_tramite          CHAR(002),
        carga_reg           CHAR(800)

    DEFINE li_folio LIKE ret_solicitud_tx.folio

    -- -----------------------------------------------------------------------------

    LET li_folio = 0

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_carga

    FOREACH cur_ar INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "01"
                CALL f_carga_encabezado(carga_reg            ,
                                        li_folio             ,
                                        gr_dat.nom_archivo
                                       )

            WHEN "03"
                CALL f_carga_det(pc_tramite, carga_reg, li_folio)

            WHEN "09"
                CALL f_carga_sumario(carga_reg, li_folio)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de retiros imss          #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pc_tramite)

    DEFINE
        pc_tramite          CHAR(02)

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    CASE pc_tramite

        WHEN gr_tramite.transf
            CALL f_valida_det_transf(lr_error.*) RETURNING ls_flag

        WHEN gr_tramite.disp
            CALL f_valida_det_disposicion(lr_error.*) RETURNING ls_flag

        WHEN gr_tramite.hist
            CALL f_valida_det_historico(lr_error.*) RETURNING ls_flag

    END CASE

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pc_tramite)

    DEFINE
        pc_tramite          CHAR(02)

    DEFINE
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_folio = f_ultimo_folio()

    CASE pc_tramite

        WHEN gr_tramite.transf
            CALL f_inserta_datamart()
            CALL f_inserta_transferencia(li_folio)

        WHEN gr_tramite.disp
            CALL f_inserta_disposicion(li_folio)

        WHEN gr_tramite.hist
            CALL f_inserta_historico(li_folio)

    END CASE

    DISPLAY "                                             " AT 18,1
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_bitacora_err : Consulta la informacion de la bitacora de errores, ya    #
#                  sea por medio del menu principal o de forma automatica   #
#                  al presentarse un error en la carga                      #
#---------------------------------------------------------------------------#
FUNCTION f_bitacora_err(pi_proceso)

    DEFINE pi_proceso LIKE ret_bitacora_error.id_proceso

    DEFINE lar_bitacora ARRAY[5000] OF RECORD
        programa        LIKE ret_bitacora_error.programa     ,
        nom_archivo     LIKE ret_bitacora_error.nom_archivo  ,
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

    OPEN WINDOW retc8102 AT 4,4 WITH FORM "RETC8102" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC810      BITACORA DE ERRORES DE CARGA DE RETIROS IMSS                     " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    -- El proceso viene de la carga de archivo, por lo que se hace directa
    -- la carga del arreglo mediante el id de proceso
    IF pi_proceso <> 0 THEN
        LET lc_query =   "SELECT programa    ,",
                         "       nom_archivo ,",
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

        CONSTRUCT BY NAME lc_where ON  nom_archivo       ,
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
            CLOSE WINDOW retc8102
            RETURN
        END IF

        LET lc_query =   "SELECT programa    ,",
                         "       nom_archivo ,",
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
                         " AND    programa = 'RETC810' ",
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
            CLOSE WINDOW retc8102

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc8102
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del           #
#                      encabezado del archivo de retiros                    #
#---------------------------------------------------------------------------#
FUNCTION f_carga_encabezado(pr_encab)

    DEFINE pr_encab RECORD
        registro        CHAR(350)                       ,
        folio           LIKE ret_cza_lote.folio         ,
        nom_archivo     LIKE ret_cza_lote.nom_archivo
    END RECORD

    DEFINE lr_fechas RECORD
        operacion       LIKE ret_cza_lote.fecha_operacion,
        transfer        LIKE ret_cza_lote.fecha_valor_trans
    END RECORD

    DEFINE
        c10_fecha   CHAR(10)

    -- -----------------------------------------------------------------------------

    LET c10_fecha = pr_encab.registro[19,20],"/",
                    pr_encab.registro[21,22],"/",
                    pr_encab.registro[15,18]

    LET lr_fechas.operacion = c10_fecha

    LET c10_fecha = pr_encab.registro[27,28],"/",
                    pr_encab.registro[29,30],"/",
                    pr_encab.registro[23,26]

    LET lr_fechas.transfer  = c10_fecha

    INSERT INTO tmp_cza_lote
    VALUES(pr_encab.folio       ,
           lr_fechas.operacion  ,
           lr_fechas.transfer   ,
           pr_encab.nom_archivo ,
           HOY                  , -- fecha_carga
           0                    , -- total registros
           gr_edo.recibido
          )

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de transferencias                                   #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pr_carga)

    DEFINE pr_carga RECORD
        tipo_tramite        CHAR(02)                 ,
        registro            CHAR(800)                ,
        folio               LIKE dis_provision.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    CASE pr_carga.tipo_tramite

        WHEN gr_tramite.transf
            CALL f_carga_transferencia(pr_carga.registro,
                                       pr_carga.folio
                                      )

        WHEN gr_tramite.disp
            CALL f_carga_disposicion(pr_carga.registro,
                                     pr_carga.folio
                                    )

        WHEN gr_tramite.hist
            CALL f_carga_historico(pr_carga.registro,
                                   pr_carga.folio
                                  )
    END CASE


END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_transferencia : Carga el detalle del archivo de una operacion 02  #
#                         de transferencias imss                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_transferencia(pc_registro,p_folio)

    DEFINE
        pc_registro         CHAR(800)

    DEFINE p_folio LIKE ret_transf_rx.folio

    DEFINE lr_transfer_tmp  RECORD LIKE ret_transf_rx.*

    DEFINE
        lc_det_viv          CHAR(800),
        lc_mto_viv          CHAR(015),
        lc_mto_const        CHAR(016),
        lc_fechas           CHAR(010),
        lc_prcje            CHAR(006)

    -- -----------------------------------------------------------------------------

    LET lr_transfer_tmp.nss                 = pc_registro[007,017]
    LET lr_transfer_tmp.consecutivo         = f_obtiene_ult_consec()
    LET lr_transfer_tmp.folio               = p_folio

    LET lr_transfer_tmp.curp                = pc_registro[018,035]
    LET lr_transfer_tmp.nombre_datamart     = pc_registro[036,085]
    LET lr_transfer_tmp.nombre_afore        = pc_registro[086,125]
    LET lr_transfer_tmp.paterno_afore       = pc_registro[126,165]
    LET lr_transfer_tmp.materno_afore       = pc_registro[166,205]
    LET lr_transfer_tmp.sec_pension         = pc_registro[206,207]
    LET lr_transfer_tmp.tipo_mov_procesar   = pc_registro[208,210]
    LET lr_transfer_tmp.regimen             = pc_registro[211,212]
    LET lr_transfer_tmp.tipo_retiro         = pc_registro[213,213]
    LET lr_transfer_tmp.tipo_seguro         = pc_registro[214,215]
    LET lr_transfer_tmp.tipo_pension        = pc_registro[216,217]
    LET lr_transfer_tmp.tipo_prestacion     = pc_registro[218,219]

    LET lr_transfer_tmp.cve_aseguradora     = pc_registro[297,299]
    
    CASE lr_transfer_tmp.tipo_retiro

        WHEN "A"
            IF lr_transfer_tmp.cve_aseguradora = "999" THEN
                LET lr_transfer_tmp.cve_destino = "I"
            ELSE
                LET lr_transfer_tmp.cve_destino = "A"
            END IF

            LET lc_mto_const                       = pc_registro[282,294], ".",
                                                     pc_registro[295,296]
            LET lr_transfer_tmp.mto_constitutivo   = lc_mto_const

        WHEN "B"
            LET lr_transfer_tmp.cve_destino = "G"

        WHEN "C"
            LET lr_transfer_tmp.cve_destino = "G"

    END CASE

    LET lc_fechas                           = pc_registro[224,225],"/",
                                              pc_registro[226,227],"/",
                                              pc_registro[220,223]
    LET lr_transfer_tmp.fecha_ini_pen       = lc_fechas

    LET lc_fechas                           = pc_registro[232,233],"/",
                                              pc_registro[234,235],"/",
                                              pc_registro[228,231]
    LET lr_transfer_tmp.fecha_resolucion    = lc_fechas

    LET lc_prcje                            = pc_registro[236,238],".",
                                              pc_registro[239,240]
    LET lr_transfer_tmp.porcentaje_val      = lc_prcje

    LET lr_transfer_tmp.semanas_cotizadas   = pc_registro[241,244]

    LET lc_fechas                           = pc_registro[249,250],"/",
                                              pc_registro[251,252],"/",
                                              pc_registro[245,248]
    LET lr_transfer_tmp.fecha_carga_datama  = lc_fechas

    LET lc_fechas                           = pc_registro[309,310],"/",
                                              pc_registro[311,312],"/",
                                              pc_registro[305,308]
    LET lr_transfer_tmp.fecha_valor_viv     = lc_fechas

    LET lr_transfer_tmp.diag_registro       = pc_registro[253,255]

    LET lc_prcje                            = pc_registro[262,264],".",
                                              pc_registro[265,266]
    LET lr_transfer_tmp.porcentaje_ret97    = lc_prcje

    LET lc_prcje                            = pc_registro[267,269],".",
                                              pc_registro[270,271]
    LET lr_transfer_tmp.porcentaje_cv       = lc_prcje

    LET lc_prcje                            = pc_registro[272,274],".",
                                              pc_registro[275,276]
    LET lr_transfer_tmp.porcentaje_cs       = lc_prcje

    LET lc_prcje                            = pc_registro[277,279],".",
                                              pc_registro[280,281]
    LET lr_transfer_tmp.porcentaje_viv      = lc_prcje

    LET lr_transfer_tmp.estado_solicitud    = gr_edo.recibido
    LET lr_transfer_tmp.usuario             = gc_usuario

    -- -----------------------------------------------------------------------------

    -- Obtenemos los datos de vivienda
    SELECT *
    INTO   lc_det_viv
    FROM   tmp_arch_carga
    WHERE  n_registros[03,13] = lr_transfer_tmp.nss
    AND    n_registros[01,02] = "05"

    LET lr_transfer_tmp.estado_sub_viv  = lc_det_viv[032,032]

    LET lc_mto_viv                      = lc_det_viv[041,048], ".",
                                          lc_det_viv[049,054]
    LET lr_transfer_tmp.saldo_viv97     = lc_mto_viv

    IF lr_transfer_tmp.saldo_viv97 IS NULL OR lr_transfer_tmp.saldo_viv97 = "" THEN
        LET lr_transfer_tmp.saldo_viv97 = 0
    END IF

    -- -----------------------------------------------------------------------------

    INSERT INTO tmp_trans_imss_recep VALUES(lr_transfer_tmp.*)  --cpl2291

END FUNCTION


#---------------------------------------------------------------------------#
# f_carga_historico : Carga el detalle del archivo de una operacion 06 de   #
#                     disposiciones. Se carga el detalle solo para hacer    #
#                     validaciones y el detalle 04 y 05 para tener listo    #
#                     si se hace la carga                                   #
#---------------------------------------------------------------------------#
FUNCTION f_carga_disposicion(pc_registro,p_folio)

    DEFINE
        pc_registro         CHAR(800)

    DEFINE p_folio LIKE ret_solicitud_tx.folio

    DEFINE lr_disp_temporal  RECORD LIKE ret_solicitud_tx.*
    DEFINE lr_mto_siefore    RECORD LIKE ret_monto_siefore.*
    DEFINE lr_mto_viv        RECORD LIKE ret_monto_viv.*

    DEFINE
        lc_det_sie          CHAR(800),
        lc_det_viv          CHAR(800),
        lc_montos           CHAR(015),
        lc_mto_const        CHAR(016),
        lc_fechas           CHAR(010),
        lc_prcje            CHAR(006)

    DEFINE lc_notif_cuenta_clabe   CHAR(003)    #CPL-2104
    -- -----------------------------------------------------------------------------

    LET lr_disp_temporal.nss                = pc_registro[007,017]
    LET lr_disp_temporal.consecutivo        = pc_registro[233,243]
    LET lr_disp_temporal.tipo_retiro        = pc_registro[158,158]

    LET lr_disp_temporal.curp               = pc_registro[018,035]
    LET lr_disp_temporal.sec_pension        = pc_registro[156,157]
    LET lr_disp_temporal.regimen            = pc_registro[159,160]
    LET lr_disp_temporal.tipo_seguro        = pc_registro[161,162]
    LET lr_disp_temporal.tipo_pension       = pc_registro[163,164]
    LET lr_disp_temporal.tipo_prestacion    = pc_registro[165,166]

    LET lc_fechas                           = pc_registro[171,172],"/",
                                              pc_registro[173,174],"/",
                                              pc_registro[167,170]
    LET lr_disp_temporal.fecha_ini_pen      = lc_fechas

    LET lc_fechas                           = pc_registro[179,180],"/",
                                              pc_registro[181,182],"/",
                                              pc_registro[175,178]
    LET lr_disp_temporal.fecha_resolucion   = lc_fechas

    LET lc_prcje                            = pc_registro[183,185],".",
                                              pc_registro[186,187]
    LET lr_disp_temporal.porcentaje_val     = lc_prcje

    LET lc_fechas                           = pc_registro[196,197],"/",
                                              pc_registro[198,199],"/",
                                              pc_registro[192,195]
    LET lr_disp_temporal.fecha_solicitud    = lc_fechas

    LET lr_disp_temporal.cve_doc_probatorio = pc_registro[200,200]

    LET lc_fechas                           = pc_registro[205,206],"/",
                                              pc_registro[207,208],"/",
                                              pc_registro[201,204]
    LET lr_disp_temporal.fecha_nacimiento   = lc_fechas

    LET lr_disp_temporal.aseguradora        = pc_registro[209,211]
    LET lr_disp_temporal.actuario           = pc_registro[212,218]
    LET lr_disp_temporal.num_plan_pension   = pc_registro[219,226]
    LET lr_disp_temporal.periodo_pago       = pc_registro[227,232]
    LET lr_disp_temporal.diag_registro      = pc_registro[337,339]

#CPL-2104 INI
    IF (lr_disp_temporal.tipo_retiro = "E") THEN   --Trae notif clabe

        LET lc_notif_cuenta_clabe = pc_registro[541,543]

        SELECT "OK"
        FROM ret_op06_cuenta_clabe
        WHERE nss = lr_disp_temporal.nss
        AND consecutivo = lr_disp_temporal.consecutivo
    
        IF (SQLCA.SQLERRD[3] = 0) THEN
            INSERT INTO ret_op06_cuenta_clabe VALUES (lr_disp_temporal.nss,
                                                      lr_disp_temporal.consecutivo,
                                                      lc_notif_cuenta_clabe,
                                                      gc_usuario,
                                                      CURRENT)
        ELSE
            IF (SQLCA.SQLERRD[3] > 0) THEN
                UPDATE ret_op06_cuenta_clabe
                SET diag_procesar = lc_notif_cuenta_clabe,
                    usuario_catura = gc_usuario,
                    fecha_captura = CURRENT
                WHERE nss = lr_disp_temporal.nss
                AND consecutivo = lr_disp_temporal.consecutivo
            END IF
        END IF

    END IF
#CPL-2104 FIN

    INSERT INTO tmp_solicitud VALUES (lr_disp_temporal.*)

    -- -----------------------------------------------------------------------------

    -- Obtenemos los datos de detalle de siefore
    SELECT *
    INTO   lc_det_sie
    FROM   tmp_arch_carga
    WHERE  n_registros[03,13] = lr_disp_temporal.nss
    AND    n_registros[01,02] = "04"

    LET lr_mto_siefore.nss              = lc_det_sie[003,013]
    LET lr_mto_siefore.siefore          = lc_det_sie[032,033]

    LET lc_montos                       = lc_det_sie[034,041],".",
                                          lc_det_sie[042,047]
    LET lr_mto_siefore.acciones_ret97   = lc_montos

    LET lc_montos                       = lc_det_sie[048,055],".",
                                          lc_det_sie[056,061]
    LET lr_mto_siefore.acciones_cv      = lc_montos

    LET lc_montos                       = lc_det_sie[062,069],".",
                                          lc_det_sie[070,075]
    LET lr_mto_siefore.acciones_cs      = lc_montos

    LET lc_montos                       = lc_det_sie[076,083],".",
                                          lc_det_sie[084,089]
    LET lr_mto_siefore.acciones_ret92   = lc_montos

    LET lr_mto_siefore.folio            = 0
    LET lr_mto_siefore.tipo_operacion   = 6
    LET lr_mto_siefore.consecutivo      = lr_disp_temporal.consecutivo
    LET lr_mto_siefore.tipo_retiro      = lr_disp_temporal.tipo_retiro

    INSERT INTO tmp_monto_siefore VALUES(lr_mto_siefore.*)

    -- -----------------------------------------------------------------------------

    -- Obtenemos los datos de detalle de vivienda
    SELECT *
    INTO   lc_det_viv
    FROM   tmp_arch_carga
    WHERE  n_registros[03,13] = lr_disp_temporal.nss
    AND    n_registros[01,02] = "05"

#CPL-2104 INI
    IF (SQLCA.SQLERRD[3] = 0) THEN   --No trae detalle 05
        LET lr_mto_viv.nss = lc_det_sie[003,013]
        LET lr_mto_viv.estado_sub_viv = 0
        LET lr_mto_viv.pesos_viv72 = 0
        LET lr_mto_viv.acc_viv97_bdsviv = 0
        LET lr_mto_viv.acc_viv92_bdsviv = 0
#CPL-2104 FIN
    ELSE
        LET lr_mto_viv.nss              = lc_det_viv[003,013]
        
        LET lr_mto_viv.estado_sub_viv   = lc_det_viv[082,082]
        
        LET lc_montos                   = lc_det_viv[068,079],".",
                                          lc_det_viv[080,081]
        LET lr_mto_viv.pesos_viv72      = lc_montos
        
        LET lc_montos                   = lc_det_viv[083,090],".",
                                          lc_det_viv[091,096]
        LET lr_mto_viv.acc_viv97_bdsviv = lc_montos
        
        LET lc_montos                   = lc_det_viv[097,104],".",
                                          lc_det_viv[105,110]
        LET lr_mto_viv.acc_viv92_bdsviv = lc_montos
    END IF
    LET lr_mto_viv.consecutivo      = lr_disp_temporal.consecutivo
    LET lr_mto_viv.tipo_retiro      = lr_disp_temporal.tipo_retiro
    LET lr_mto_viv.folio            = 0

    INSERT INTO tmp_monto_viv VALUES(lr_mto_viv.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_historico : Carga el detalle del archivo de una operacion 19 de   #
#                     transferencias saldo historico                        #
#---------------------------------------------------------------------------#
FUNCTION f_carga_historico(pc_registro,p_folio)

    DEFINE
        pc_registro         CHAR(800)

    DEFINE p_folio LIKE ret_sal_his_rx.folio

    DEFINE lr_historico_tmp  RECORD LIKE ret_sal_his_rx.*

    DEFINE
        lc_det_viv          CHAR(800),
        lc_mto_const        CHAR(016),
        lc_fechas           CHAR(010),
        lc_prcje            CHAR(006)

    -- -----------------------------------------------------------------------------

    LET lr_historico_tmp.nss                    = pc_registro[007,017]
    LET lr_historico_tmp.consecutivo            = f_obtiene_ult_consec()
    LET lr_historico_tmp.folio                  = p_folio

    LET lr_historico_tmp.curp                   = pc_registro[018,035]
    LET lr_historico_tmp.nombre_datamart        = pc_registro[036,085]
    LET lr_historico_tmp.nombre_afore           = pc_registro[086,125]
    LET lr_historico_tmp.paterno_afore          = pc_registro[126,165]
    LET lr_historico_tmp.materno_afore          = pc_registro[166,205]
    LET lr_historico_tmp.sec_pension            = pc_registro[206,207]
    LET lr_historico_tmp.tipo_mov_procesar      = pc_registro[208,210]
    LET lr_historico_tmp.regimen                = pc_registro[211,212]
    LET lr_historico_tmp.tipo_retiro            = pc_registro[213,213]
    LET lr_historico_tmp.tipo_seguro            = pc_registro[214,215]
    LET lr_historico_tmp.tipo_pension           = pc_registro[216,217]
    LET lr_historico_tmp.tipo_prestacion        = pc_registro[218,219]

    LET lc_fechas                               = pc_registro[224,225],"/",
                                                  pc_registro[226,227],"/",
                                                  pc_registro[220,223]
    LET lr_historico_tmp.fecha_ini_pen          = lc_fechas

    LET lc_fechas                               = pc_registro[232,233],"/",
                                                  pc_registro[234,235],"/",
                                                  pc_registro[228,231]
    LET lr_historico_tmp.fecha_resolucion       = lc_fechas

    LET lc_prcje                                = pc_registro[236,238],".",
                                                  pc_registro[239,240]
    LET lr_historico_tmp.porcentaje_val         = lc_prcje

    LET lr_historico_tmp.semanas_cotizadas      = pc_registro[241,244]

    LET lc_fechas                               = pc_registro[249,250],"/",
                                                  pc_registro[251,252],"/",
                                                  pc_registro[245,248]
    LET lr_historico_tmp.fecha_carga_datama     = lc_fechas

    LET lr_historico_tmp.diag_registro          = pc_registro[253,255]
    LET lr_historico_tmp.estado_sub_viv         = pc_registro[256,256]

    LET lr_historico_tmp.estado_solicitud       = gr_edo.recibido

    INSERT INTO tmp_historico VALUES(lr_historico_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de transferencias                               #
#---------------------------------------------------------------------------#
FUNCTION f_carga_sumario(pr_sumario)

    DEFINE pr_sumario RECORD
        registro        CHAR(350)                   ,
        folio           LIKE ret_cza_lote.folio
    END RECORD

    DEFINE li_tot_regs LIKE ret_cza_lote.tot_registros

    -- -----------------------------------------------------------------------------

    LET li_tot_regs = pr_sumario.registro[23,28]

    UPDATE tmp_cza_lote
    SET    tot_registros = li_tot_regs
    WHERE  folio         = pr_sumario.folio

END FUNCTION

#----------------------------------------------------------------------------#
# f_valida_archivo : Ejecuta las validaciones sobre la estructura del archivo#
#----------------------------------------------------------------------------#
FUNCTION f_valida_archivo()

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        lc_tipo_tramite     CHAR(02),
        lc_tram             CHAR(02),
        lc_nss              CHAR(11),
        lc_fec8             CHAR(10),
        lc_fecha            CHAR(10)

    DEFINE
        li_num_tram         ,
        li_cont             INTEGER

    DEFINE
        ls_flag             SMALLINT

    -- ---------------- -------------------------------------------------------------

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    ----------  Validaciones de estructura del archivo   --------------
    LET lr_error.tipo_campo   = "ARCHIVO"
    LET lr_error.nom_campo    = " "

    -- Valida que el archivo sea una operacion permitida de retiros imss
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] NOT IN (gr_tramite.disp     ,
                                    gr_tramite.transf   ,
                                    gr_tramite.hist
                                   )

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
    END IF

    -- Valida que el archivo contenga solo registros de detalle con una unica operacion
    LET li_num_tram = 0

    DECLARE cur_opera CURSOR FOR
    SELECT n_registros[5,6],
           COUNT(*)
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] IN (gr_tramite.disp     ,
                                gr_tramite.transf   ,
                                gr_tramite.hist
                               )
    GROUP BY 1

    FOREACH cur_opera INTO lc_tipo_tramite,
                           li_cont

        LET li_num_tram = li_num_tram + 1

        -- Si se encontro mas de un tipo de tramite se marca error
        IF li_num_tram > 1 THEN
            LET ls_flag           = 1
            LET lr_error.id_error = 1
            CALL f_inserta_bitacora(lr_error.*)
            RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
        END IF
    END FOREACH

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "01"

    IF li_cont <> 1 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 2
        CALL f_inserta_bitacora(lr_error.*)
        RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el sumario
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "09"

    IF li_cont <> 1 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 3
        CALL f_inserta_bitacora(lr_error.*)
        RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
    END IF

    -- Archivo sin registros de detalle
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[07,17],
           n_registros[05,06]
    FROM   tmp_arch_carga
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_nss, lc_tram

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_carga
        WHERE  n_registros[1,2]   = "03"
        AND    n_registros[07,17] = lc_nss

        IF li_cont > 1 THEN
            LET lr_error.nom_campo   = "det 03 duplicado"
            LET ls_flag              = 1
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_nss

            CALL f_inserta_bitacora(lr_error.*)
            RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
        END IF

        IF lc_tram <> gr_tramite.hist THEN

            --- Valida detalle 05 ---
            SELECT COUNT(*)
            INTO   li_cont
            FROM   tmp_arch_carga
            WHERE  n_registros[1,2]   = "05"
            AND    n_registros[03,13] = lc_nss

            IF li_cont > 1 THEN
                --- Registro 05 duplicado ---
                LET lr_error.nom_campo   = "det 05 duplicado"
                LET ls_flag              = 1
                LET lr_error.id_error    = 11
                LET lr_error.valor_campo = lc_nss

                CALL f_inserta_bitacora(lr_error.*)
                RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso
            ELSE
                IF li_cont = 0 OR li_cont IS NULL THEN
                    --- Falta detalle 05 del registro 03 ---
                    LET lr_error.nom_campo   = "reg. 03 sin detalle 05"
                    #LET ls_flag              = 1     #CPL-2104   No es obligatorio que traiga detalle 05
                    LET lr_error.id_error    = 11
                    LET lr_error.valor_campo = lc_nss

                    CALL f_inserta_bitacora(lr_error.*)
                    #RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso   #CPL-2104   Datdo que no es obligatorio no se manda error
                END IF
            END IF
        END IF

    END FOREACH

    RETURN lc_tipo_tramite, ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_transf : Valida el detalle cargado de la transferencia       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_transf(pr_error)

    DEFINE pr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_transfer RECORD LIKE ret_transf_rx.*

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DECLARE cur_tmp CURSOR FOR
    SELECT *
    FROM   tmp_trans_imss_recep     --CPL-2291

    FOREACH cur_tmp INTO lr_transfer.*

        LET pr_error.nss          = lr_transfer.nss
        LET pr_error.curp         = lr_transfer.curp
        LET pr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre transferencias
        CALL f_valida_transferencia(ls_flag            ,
                                    pr_error.id_proceso,
                                    lr_transfer.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_det_trans_nulo(ls_flag            ,
                                     pr_error.id_proceso,
                                     lr_transfer.*      )
            RETURNING ls_flag

        --- Valida que los campos existan contra el catalogo ---
        CALL f_valida_det_catalogo_transf(ls_flag            ,
                                          pr_error.id_proceso,
                                          lr_transfer.*      )
            RETURNING ls_flag

    END FOREACH

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_disposicion : Valida el detalle cargado de disposicion de    #
#                            recursos                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_disposicion(pr_error)

    DEFINE pr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_disposicion RECORD LIKE ret_solicitud_tx.*

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DECLARE cur_dis CURSOR FOR
    SELECT *
    FROM   tmp_solicitud

    FOREACH cur_dis INTO lr_disposicion.*

        LET pr_error.nss          = lr_disposicion.nss
        LET pr_error.curp         = lr_disposicion.curp
        LET pr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre saldo historico
        CALL f_valida_disposicion(ls_flag            ,
                                  pr_error.id_proceso,
                                  lr_disposicion.*      )
            RETURNING ls_flag

    END FOREACH

    RETURN ls_flag


END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_historico : Valida el detalle cargado de la transferencia    #
#                          saldo historico                                  #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_historico(pr_error)

    DEFINE pr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_historico RECORD LIKE ret_sal_his_rx.*

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    DECLARE cur_his CURSOR FOR
    SELECT *
    FROM   tmp_historico

    FOREACH cur_his INTO lr_historico.*

        LET pr_error.nss          = lr_historico.nss
        LET pr_error.curp         = lr_historico.curp
        LET pr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre saldo historico
        CALL f_valida_historico(ls_flag            ,
                                pr_error.id_proceso,
                                lr_historico.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_det_hist_nulo(ls_flag            ,
                                    pr_error.id_proceso,
                                    lr_historico.*      )
            RETURNING ls_flag

        --- Valida que los campos existan contra el catalogo ---
        CALL f_valida_det_catalogo_hist(ls_flag            ,
                                        pr_error.id_proceso,
                                        lr_historico.*      )
            RETURNING ls_flag

    END FOREACH

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_transferencia : Validaciones para el proceso de transferencias   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_transferencia(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_transf_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que el NSS no sea nulo o que venga a 11 posiciones ---
    IF (pr_trans_temp.nss IS NULL) OR (LENGTH(pr_trans_temp.nss) <> 11) THEN
        LET lr_error.nom_campo    = "nss"
        LET lr_error.valor_campo  = pr_trans_temp.nss
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que la Secuencia de pension no sea nula ---
    IF (pr_trans_temp.sec_pension IS NULL) OR (pr_trans_temp.sec_pension = "  ") THEN
        LET lr_error.nom_campo    = "sec_pension"
        LET lr_error.valor_campo  = pr_trans_temp.sec_pension
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el tipo de retiro corresponda a transferencias ---
    SELECT "OK"
    FROM   tab_retiro
    WHERE  tipo_retiro  = pr_disp_temp.tipo_retiro
    AND    tipo_retiro IN (SELECT tipo_retiro
                           FROM   tab_tramite_retiro
                           WHERE  cod_tramite = gs_tipo_transferencia
                          )
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "Tipo Retiro no valido para Transf :", pr_trans_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que los porcentajes sean correctos ---
    IF pr_trans_temp.diag_registro = 302 THEN
        IF pr_trans_temp.porcentaje_ret97 > 100 OR pr_trans_temp.porcentaje_ret97 < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_ret97"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_ret97
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        IF pr_trans_temp.porcentaje_cv > 100 OR pr_trans_temp.porcentaje_cv < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_cv"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_cv
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        IF pr_trans_temp.porcentaje_cs > 100 OR pr_trans_temp.porcentaje_cs < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_cs"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_cs
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        IF pr_trans_temp.porcentaje_viv > 100 OR pr_trans_temp.porcentaje_viv < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_viv"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_viv
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_disposicion : Validaciones para el proceso de disposiciones      #
#---------------------------------------------------------------------------#
FUNCTION f_valida_disposicion(ps_error, pi_id_error, pr_disp_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_disp_temp RECORD LIKE ret_solicitud_tx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_disp_temp.nss
    LET lr_error.curp         = pr_disp_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que el NSS no sea nulo o que venga a 11 posiciones ---
    IF (pr_disp_temp.nss IS NULL) OR (LENGTH(pr_disp_temp.nss) <> 11) THEN
        LET lr_error.nom_campo    = "nss"
        LET lr_error.valor_campo  = pr_disp_temp.nss
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el tipo de retiro corresponda a disposiciones  ---
    SELECT "OK"
    FROM   tab_retiro
    WHERE  tipo_retiro  = pr_disp_temp.tipo_retiro
    AND    tipo_retiro IN (SELECT tipo_retiro
                           FROM   tab_tramite_retiro
                           WHERE  cod_tramite = gs_tipo_disposicion
                          )
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "Tipo Retiro no valido para Disp :", pr_disp_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que la solicitud con el nss y el tipo de retiro ya exista previamente ---
    SELECT "OK"
    FROM   ret_solicitud_tx
    WHERE  nss            = pr_disp_temp.nss
    AND    tipo_retiro    = pr_disp_temp.tipo_retiro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "nss/tipo_ret"
        LET lr_error.valor_campo  = "NSS no existe con tipo retiro ", pr_disp_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el consecutivo del tramite exista ---
    SELECT "OK"
    FROM   ret_solicitud_tx A
    WHERE  nss              = pr_disp_temp.nss
    AND    tipo_retiro      = pr_disp_temp.tipo_retiro
    AND    consecutivo      = pr_disp_temp.consecutivo
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "consecutivo"
        LET lr_error.valor_campo  = pr_disp_temp.consecutivo
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_historico : Validaciones para el proceso de transferencias saldo #
#                      historico                                            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_historico(ps_error, pi_id_error, pr_hist_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_hist_temp RECORD LIKE ret_sal_his_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_hist_temp.nss
    LET lr_error.curp         = pr_hist_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que el NSS no sea nulo o que venga a 11 posiciones ---
    IF (pr_hist_temp.nss IS NULL) OR (LENGTH(pr_hist_temp.nss) <> 11) THEN
        LET lr_error.nom_campo    = "nss"
        LET lr_error.valor_campo  = pr_hist_temp.nss
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que la Secuencia de pension no sea nula ---
    IF (pr_hist_temp.sec_pension IS NULL) OR (pr_hist_temp.sec_pension = "  ") THEN
        LET lr_error.nom_campo    = "sec_pension"
        LET lr_error.valor_campo  = pr_hist_temp.sec_pension
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el tipo de retiro sea valido ---
    SELECT "OK"
    FROM   tab_retiro
    WHERE  tipo_retiro  = pr_hist_temp.tipo_retiro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "Tipo Retiro no valido :", pr_hist_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el porcentaje valuacion sea correcto ---
    IF pr_hist_temp.porcentaje_val > 100 OR pr_hist_temp.porcentaje_val < 0 THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "porcentaje_val"
        LET lr_error.valor_campo  = pr_hist_temp.porcentaje_val
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_trans_nulo : Valida que los campos obligatorios para         #
#                           transferencia contengan informacion             #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_trans_nulo(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_transf_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_trans_temp.nombre_datamart IS NULL THEN
        LET lr_error.nom_campo    = "nombre_datamart"
        LET lr_error.valor_campo  = pr_trans_temp.nombre_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.regimen IS NULL OR pr_trans_temp.regimen = "  " THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_trans_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_retiro IS NULL OR pr_trans_temp.tipo_retiro = "  " THEN
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_seguro IS NULL OR pr_trans_temp.tipo_seguro = "  " THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_pension IS NULL OR pr_trans_temp.tipo_pension = "  " THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_prestacion IS NULL THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.fecha_ini_pen IS NULL THEN
        LET lr_error.nom_campo    = "fecha_ini_pen"
        LET lr_error.valor_campo  = pr_trans_temp.fecha_ini_pen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.fecha_resolucion IS NULL THEN
        LET lr_error.nom_campo    = "fecha_resolucion"
        LET lr_error.valor_campo  = pr_trans_temp.fecha_resolucion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.fecha_carga_datama IS NULL THEN
        LET lr_error.nom_campo    = "fecha_carga_datama"
        LET lr_error.valor_campo  = pr_trans_temp.fecha_carga_datama
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_mov_procesar IS NULL OR pr_trans_temp.regimen = "   " THEN
        LET lr_error.nom_campo    = "tipo_mov_procesar"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_mov_procesar
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.diag_registro IS NULL OR pr_trans_temp.diag_registro = 0 THEN
        LET lr_error.nom_campo    = "diag_registro"
        LET lr_error.valor_campo  = pr_trans_temp.diag_registro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.estado_sub_viv IS NULL OR pr_trans_temp.estado_sub_viv = 0 THEN
        LET lr_error.nom_campo    = "estado_sub_viv"
        LET lr_error.valor_campo  = pr_trans_temp.estado_sub_viv
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_hist_nulo : Valida que los campos obligatorios para          #
#                           transferencia contengan informacion             #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_hist_nulo(ps_error, pi_id_error, pr_hist_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_hist_temp RECORD LIKE ret_sal_his_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_hist_temp.nss
    LET lr_error.curp         = pr_hist_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_hist_temp.nombre_datamart IS NULL THEN
        LET lr_error.nom_campo    = "nombre_datamart"
        LET lr_error.valor_campo  = pr_hist_temp.nombre_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.tipo_mov_procesar IS NULL OR pr_hist_temp.tipo_mov_procesar = "   " THEN
        LET lr_error.nom_campo    = "tipo_mov_procesar"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_mov_procesar
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.regimen IS NULL OR pr_hist_temp.regimen = "  " THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_hist_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.tipo_retiro IS NULL OR pr_hist_temp.tipo_retiro = "  " THEN
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.tipo_seguro IS NULL OR pr_hist_temp.tipo_seguro = "  " THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.tipo_pension IS NULL OR pr_hist_temp.tipo_pension = "  " THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.tipo_prestacion IS NULL THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.fecha_ini_pen IS NULL THEN
        LET lr_error.nom_campo    = "fecha_ini_pen"
        LET lr_error.valor_campo  = pr_hist_temp.fecha_ini_pen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.fecha_resolucion IS NULL THEN
        LET lr_error.nom_campo    = "fecha_resolucion"
        LET lr_error.valor_campo  = pr_hist_temp.fecha_resolucion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.fecha_carga_datama IS NULL THEN
        LET lr_error.nom_campo    = "fecha_resolucion"
        LET lr_error.valor_campo  = pr_hist_temp.fecha_carga_datama
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.diag_registro IS NULL OR pr_hist_temp.diag_registro = 0 THEN
        LET lr_error.nom_campo    = "diag_registro"
        LET lr_error.valor_campo  = pr_hist_temp.diag_registro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_hist_temp.estado_sub_viv IS NULL OR pr_hist_temp.estado_sub_viv = 0 THEN
        LET lr_error.nom_campo    = "estado_sub_viv"
        LET lr_error.valor_campo  = pr_hist_temp.estado_sub_viv
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_catalogo_transf : Valida que existan los valores de los      #
#                                campos que tengan su propio catalogo       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_catalogo_transf(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_transf_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lc_tmp_retiro LIKE ret_transf_rx.tipo_retiro

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 9

    -- Valida contra catalogo de Regimen
    SELECT "OK"
    FROM   tab_regimen
    WHERE  regimen = pr_trans_temp.regimen
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_trans_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Seguro
    SELECT "OK"
    FROM   tab_seguro
    WHERE  clave = pr_trans_temp.tipo_seguro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Pension
    SELECT "OK"
    FROM   tab_pension
    WHERE  tipo_pension = pr_trans_temp.tipo_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Prestacion
    SELECT "OK"
    FROM   tab_prestacion
    WHERE  tipo_prestacion = pr_trans_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida que la combinacion exista en la matriz de derechos.
    SELECT UNIQUE(tipo_retiro)
    INTO   lc_tmp_retiro
    FROM   ret_matriz_derecho
    WHERE  regimen          = pr_trans_temp.regimen
    AND    tipo_seguro      = pr_trans_temp.tipo_seguro
    AND    tipo_pension     = pr_trans_temp.tipo_pension
    AND    tipo_prestacion  = pr_trans_temp.tipo_prestacion
    AND    cve_destino      = pr_trans_temp.cve_destino

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "No existe en matriz derecho"
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    ELSE
        IF lc_tmp_retiro <> pr_trans_temp.tipo_retiro THEN
            LET lr_error.nom_campo    = "tipo_retiro"
            LET lr_error.valor_campo  = "No concuerda con la matriz de derechos"
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_catalogo_hist : Valida que existan los valores de los        #
#                                campos que tengan su propio catalogo       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_catalogo_hist(ps_error, pi_id_error, pr_hist_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_hist_temp RECORD LIKE ret_sal_his_rx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE
        ls_error        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_hist_temp.nss
    LET lr_error.curp         = pr_hist_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 9

    -- Valida contra catalogo de Regimen
    SELECT "OK"
    FROM   tab_regimen
    WHERE  regimen = pr_hist_temp.regimen
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_hist_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Seguro
    SELECT "OK"
    FROM   tab_seguro
    WHERE  clave = pr_hist_temp.tipo_seguro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Pension
    SELECT "OK"
    FROM   tab_pension
    WHERE  tipo_pension = pr_hist_temp.tipo_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Prestacion
    SELECT "OK"
    FROM   tab_prestacion
    WHERE  tipo_prestacion = pr_hist_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_hist_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida que la combinacion exista en la matriz de derechos.
    SELECT "OK"
    FROM   ret_matriz_derecho
    WHERE  regimen          = pr_hist_temp.regimen
    AND    tipo_seguro      = pr_hist_temp.tipo_seguro
    AND    tipo_pension     = pr_hist_temp.tipo_pension
    AND    tipo_prestacion  = pr_hist_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "No existe en matriz derecho"
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_datamart : Guarda la informacion ya validada en la tabla de     #
#                      datamart temporal                                    #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_datamart()

    DEFINE lr_tmp_datamart RECORD LIKE ret_det_datamart.*
    DEFINE lr_tmp_transfer RECORD LIKE ret_transf_rx.*

    DEFINE
        li_folio        INTEGER

    -- -----------------------------------------------------------------------------

    LET li_folio = 0

    DECLARE cur_dtm CURSOR FOR
        SELECT *
        FROM   tmp_trans_imss_recep   --cpl2291
        WHERE  folio = li_folio

    FOREACH cur_dtm INTO lr_tmp_transfer.*

        SELECT "OK"
        FROM   ret_det_datamart
        WHERE  nss         = lr_tmp_transfer.nss
        AND    sec_pension = lr_tmp_transfer.sec_pension
        AND    diag_datamart IN (101,300,301)
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            #modificacion Jairo Palafox Reocmpilacion nueva plataforma
            INSERT INTO tmp_datamart(nss,sec_pension,folio,curp,nombre_datamart,nombre_afore,paterno_afore,materno_afore,tipo_movimiento,regimen,tipo_seguro,tipo_pension,tipo_prestacion,art_negativa,frac_negativa,num_considerando,fecha_ini_pen,fecha_resolucion,porcentaje_val,semanas_cotizadas,diag_datamart,estado)
            VALUES (lr_tmp_transfer.nss                 ,
                    lr_tmp_transfer.sec_pension         ,
                    li_folio                            ,
                    lr_tmp_transfer.curp                ,
                    lr_tmp_transfer.nombre_datamart     ,
                    lr_tmp_transfer.nombre_afore        ,
                    lr_tmp_transfer.paterno_afore       ,
                    lr_tmp_transfer.materno_afore       ,
                    lr_tmp_transfer.tipo_mov_procesar   ,
                    lr_tmp_transfer.regimen             ,
                    lr_tmp_transfer.tipo_seguro         ,
                    lr_tmp_transfer.tipo_pension        ,
                    lr_tmp_transfer.tipo_prestacion     ,
                    NULL                                , -- art_negativa
                    NULL                                , -- frac_negativa
                    NULL                                , -- num_considerando
                    lr_tmp_transfer.fecha_ini_pen       ,
                    lr_tmp_transfer.fecha_resolucion    ,
                    lr_tmp_transfer.porcentaje_val      ,
                    lr_tmp_transfer.semanas_cotizadas   ,
                    101                                 , -- diag_datamart
                    NULL                                  -- edo_sub_viv
                   )
        END IF

    END FOREACH

END FUNCTION

#----------------------------------------------------------------------------#
# f_inserta_bitacora : Inserta el registro en la bitacora de errores de carga#
#----------------------------------------------------------------------------#
FUNCTION f_inserta_bitacora(pr_error)

    DEFINE pr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
        nss          LIKE ret_bitacora_error.nss            ,
        curp         LIKE ret_bitacora_error.curp           ,
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
    LET lr_bitacora.programa    = "RETC810"
    LET lr_bitacora.nom_archivo = gr_dat.nom_archivo
    LET lr_bitacora.folio       = 0
    LET lr_bitacora.fecha_error = HOY
    LET lr_bitacora.hora_error  = lc_hora
    LET lr_bitacora.usuario     = gc_usuario


    -- Campos por parametro
    LET lr_bitacora.id_proceso  = pr_error.id_proceso
    LET lr_bitacora.nss         = pr_error.nss
    LET lr_bitacora.curp        = pr_error.curp
    LET lr_bitacora.tipo_campo  = pr_error.tipo_campo
    LET lr_bitacora.nom_campo   = pr_error.nom_campo
    LET lr_bitacora.valor_campo = pr_error.valor_campo
    LET lr_bitacora.id_error    = pr_error.id_error

    INSERT INTO ret_bitacora_error
    VALUES (lr_bitacora.*)


END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_transferencia : Guarda la informacion de transferencias en las  #
#                           tablas finales una vez que ya fue validada      #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_transferencia(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE lr_tmp_transfer RECORD LIKE ret_transf_rx.*
    DEFINE lr_tmp_cza RECORD LIKE ret_cza_lote.*

    DEFINE
        ls_resp                 ,
        ls_cod_tramite          SMALLINT

    DEFINE
        li_tot_a                ,
        li_tot_b                ,
        li_tot_c                ,
        li_tot_detalle          INTEGER

    -- -----------------------------------------------------------------------------

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    LET li_tot_detalle  = 0
    LET li_tot_a        = 0
    LET li_tot_b        = 0
    LET li_tot_c        = 0

    DISPLAY " RETC810          CARGA DE ARCHIVO DE TRANSFERENCIAS IMSS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "TOTAL DE REGISTROS DE DETALLE    : ", li_tot_detalle AT 10,13
    DISPLAY "RETIRO A    : ", li_tot_a AT 11,34
    DISPLAY "RETIRO B    : ", li_tot_b AT 12,34
    DISPLAY "RETIRO C    : ", li_tot_c AT 13,34
    DISPLAY "FOLIO DE CARGA    : ", pi_folio AT 15,28

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_cza_lote
    UPDATE tmp_cza_lote
    SET    folio = pi_folio

    INSERT INTO ret_cza_lote
    SELECT *
    FROM   tmp_cza_lote

    -- Insertamos en ret_det_datamart
    UPDATE tmp_datamart
    SET    folio = pi_folio

    INSERT INTO ret_det_datamart
    SELECT *
    FROM   tmp_datamart

    -- Insertamos en ret_transf_rx
    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_trans_imss_recep  --cpl2291

    FOREACH cur_ok INTO lr_tmp_transfer.*

        LET lr_tmp_transfer.folio = pi_folio

        INSERT INTO ret_transf_rx
        VALUES (lr_tmp_transfer.*)

        EXECUTE eje_marca_pen USING lr_tmp_transfer.nss            ,
                                    lr_tmp_transfer.curp           ,
                                    lr_tmp_transfer.folio          ,
                                    lr_tmp_transfer.consecutivo    ,
                                    lr_tmp_transfer.tipo_retiro    ,
                                    ls_cod_tramite                 ,
                                    HOY
                              INTO  ls_resp

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE    : ", li_tot_detalle AT 10,13

        CASE lr_tmp_transfer.tipo_retiro
            WHEN "A"
                LET li_tot_a = li_tot_a + 1
                DISPLAY "RETIRO A    : ", li_tot_a AT 11,34

            WHEN "B"
                LET li_tot_b = li_tot_b + 1
                DISPLAY "RETIRO B    : ", li_tot_b AT 12,34

            WHEN "C"
                LET li_tot_c = li_tot_c + 1
                DISPLAY "RETIRO C    : ", li_tot_c AT 13,34
        END CASE

        -- Se marca la cuenta
        CALL f_marca_cuenta(lr_tmp_transfer.nss         ,
                            lr_tmp_transfer.consecutivo ,
                            lr_tmp_transfer.tipo_retiro ,
                            gc_usuario)

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_disposicion : Guarda la informacion de disposiciones en las     #
#                         tablas finales una vez que ya fue validada        #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_disposicion(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE lr_tmp_cza   RECORD LIKE ret_cza_lote.*
    DEFINE lr_tmp_disp  RECORD LIKE ret_solicitud_tx.*
    DEFINE lr_mto_sie   RECORD LIKE ret_monto_siefore.*
    DEFINE lr_mto_viv   RECORD LIKE ret_monto_viv.*

    DEFINE lr_etapa_2 RECORD
        nss                 CHAR(11) ,
        folio_serv          INTEGER  ,
        tipo_serv           CHAR(01) ,
        usuario             CHAR(15)  
    END RECORD

    DEFINE
        li_cont_D           ,
        li_cont_E           ,
        li_cont_F           ,
        li_cont_G           ,
        li_cont_H           ,
        li_cont_J           ,
        li_cont_M           ,
        li_cont_P           ,
        li_rechazo          ,
        li_tot_detalle      ,
        li_consecutivo_solic  INTEGER

    -- -----------------------------------------------------------------------------
    LET li_consecutivo_solic = 0
    LET li_tot_detalle  = 0
    LET li_rechazo      = 0
    LET li_cont_D       = 0
    LET li_cont_E       = 0
    LET li_cont_F       = 0
    LET li_cont_G       = 0
    LET li_cont_H       = 0
    LET li_cont_J       = 0
    LET li_cont_M       = 0
    LET li_cont_P       = 0

    LET lr_etapa_2.tipo_serv    = "S"
    LET lr_etapa_2.usuario      = gc_usuario


    DISPLAY " RETC810           CARGA DE ARCHIVO DE DISPOSICIONES IMSS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "TOTAL DE REGISTROS DE DETALLE    : ", li_tot_detalle AT 10,18
    DISPLAY "RETIRO D : ", li_cont_D AT 12,10
    DISPLAY "RETIRO E : ", li_cont_E AT 13,10
    DISPLAY "RETIRO F : ", li_cont_F AT 14,10
    DISPLAY "RETIRO G : ", li_cont_G AT 12,32
    DISPLAY "RETIRO H : ", li_cont_H AT 13,32
    DISPLAY "RETIRO J : ", li_cont_J AT 14,32
    DISPLAY "RETIRO M : ", li_cont_M AT 12,54
    DISPLAY "RETIRO P : ", li_cont_P AT 13,54
    DISPLAY "RECHAZOS : ", li_rechazo AT 14,54
    DISPLAY "FOLIO DE CARGA : ", pi_folio AT 16,10

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_cza_lote
    UPDATE tmp_cza_lote
    SET    folio = pi_folio

    INSERT INTO ret_cza_lote
    SELECT *
    FROM   tmp_cza_lote

    -- Insertamos en ret_monto_siefore
    UPDATE tmp_monto_siefore
    SET    folio = pi_folio

    INSERT INTO ret_monto_siefore
    SELECT *
    FROM   tmp_monto_siefore

    -- Actualizamos en ret_monto_viv y ret_solicitud_tx
    DECLARE cur_dispo CURSOR FOR
    SELECT *
    FROM   tmp_solicitud

    FOREACH cur_dispo INTO lr_tmp_disp.*

        -- Actualizamos los valores de ret_monto_viv
        SELECT *
        INTO   lr_mto_viv.*
        FROM   tmp_monto_viv
        WHERE  nss          = lr_tmp_disp.nss
        AND    consecutivo  = lr_tmp_disp.consecutivo

        UPDATE ret_monto_viv
        SET    pesos_viv72      = lr_mto_viv.pesos_viv72      ,
               estado_sub_viv   = lr_mto_viv.estado_sub_viv   ,
               acc_viv97_bdsviv = lr_mto_viv.acc_viv97_bdsviv ,
               acc_viv92_bdsviv = lr_mto_viv.acc_viv92_bdsviv
        WHERE  nss              = lr_mto_viv.nss
        AND    consecutivo      = lr_mto_viv.consecutivo

        -- Actualizamos los valores de ret_solicitud_tx
        LET lr_tmp_disp.folio = pi_folio

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE    : ", li_tot_detalle AT 10,18

        CASE lr_tmp_disp.tipo_retiro
            WHEN "D"
                LET li_cont_D = li_cont_D + 1
                DISPLAY "RETIRO D : ", li_cont_D AT 12,10

            WHEN "E"
                LET li_cont_E = li_cont_E + 1
                DISPLAY "RETIRO E : ", li_cont_E AT 13,10

            WHEN "F"
                LET li_cont_F = li_cont_F + 1
                DISPLAY "RETIRO F : ", li_cont_F AT 14,10

            WHEN "G"
                LET li_cont_G = li_cont_G + 1
                DISPLAY "RETIRO G : ", li_cont_G AT 12,32

            WHEN "H"
                LET li_cont_H = li_cont_H + 1
                DISPLAY "RETIRO H : ", li_cont_H AT 13,32

            WHEN "J"
                LET li_cont_J = li_cont_J + 1
                DISPLAY "RETIRO J : ", li_cont_J AT 14,32

            WHEN "M"
                LET li_cont_M = li_cont_M + 1
                DISPLAY "RETIRO M : ", li_cont_M AT 12,54

            WHEN "P"
                LET li_cont_P = li_cont_P + 1
                DISPLAY "RETIRO P : ", li_cont_P AT 13,54

        END CASE

        --Actualiza consecutivo padre
        UPDATE ret_solicitud_tx
        SET    diag_registro = lr_tmp_disp.diag_registro
        WHERE  nss           = lr_tmp_disp.nss
        AND    consecutivo   = lr_tmp_disp.consecutivo

        --Actualiza consecutivos --CPL-3342
        DECLARE cur_benef CURSOR FOR
        SELECT  consecutivo_solic
        FROM ret_ctr_benef 
        WHERE nss = lr_tmp_disp.nss
        AND consecutivo_padre = lr_tmp_disp.consecutivo

        FOREACH cur_benef INTO li_consecutivo_solic
            UPDATE ret_solicitud_tx
            SET    diag_registro = lr_tmp_disp.diag_registro
            WHERE  nss           = lr_tmp_disp.nss
            AND    consecutivo   = li_consecutivo_solic
        END FOREACH
        
        -- Si es Invercap, se ejecuta el cierre automatico de la etapa 2
        IF gs_codigo_afore = gs_cod_inv THEN
            SELECT nss              ,
                   folio_solicitud
            INTO   lr_etapa_2.nss       ,
                   lr_etapa_2.folio_serv
            FROM  ret_solicitud_tx
            WHERE  nss           = lr_tmp_disp.nss
            AND    consecutivo   = lr_tmp_disp.consecutivo

            CALL sol_etapa_2(lr_etapa_2.*)
        END IF

        -- Una vez cargado el diagnostico de registro, se verifica este para ver
        -- si se realiza la desmarca
        SELECT "OK"
        FROM   tab_diag_procesar_disp
        WHERE  diag_procesar    = lr_tmp_disp.diag_registro
        AND    id_aceptado      = 1
        GROUP BY 1
        
        IF STATUS = NOTFOUND THEN
            CALL f_desmarca_cuenta(lr_tmp_disp.nss          ,
                                   lr_tmp_disp.consecutivo  ,
                                   lr_tmp_disp.tipo_retiro
                                  )
        
            LET li_rechazo = li_rechazo + 1
            DISPLAY "RECHAZOS : ", li_rechazo AT 14,54

             --Desmarca consecutivos --CPL-3342
            DECLARE cur_benef_des_marca CURSOR FOR
            SELECT  consecutivo_solic
            FROM ret_ctr_benef 
            WHERE nss = lr_tmp_disp.nss 

            FOREACH cur_benef_des_marca INTO li_consecutivo_solic
                CALL f_desmarca_cuenta(lr_tmp_disp.nss          ,
                                       li_consecutivo_solic     ,
                                       lr_tmp_disp.tipo_retiro
                                      )
            END FOREACH
        END IF

    END FOREACH -- Siguiente NSS

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_historico : Guarda la informacion de transferencias saldo       #
#                       historico en las tablas finales una vez que ya fue  #
#                       validada                                            #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_historico(pi_folio)

    DEFINE
        pi_folio            INTEGER

    DEFINE lr_tmp_hist RECORD LIKE ret_sal_his_rx.*
    DEFINE lr_tmp_cza RECORD LIKE ret_cza_lote.*

    DEFINE
        li_tot_detalle          INTEGER

    -- -----------------------------------------------------------------------------

    LET li_tot_detalle  = 0

    DISPLAY " RETC810         CARGA DE ARCHIVO DE SALDO HISTORICO IMSS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY "TOTAL DE REGISTROS SALDO HISTORICO    : ", li_tot_detalle AT 10,13
    DISPLAY "FOLIO DE CARGA    : ", pi_folio AT 12,33

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_cza_lote
    UPDATE tmp_cza_lote
    SET    folio = pi_folio

    INSERT INTO ret_cza_lote
    SELECT *
    FROM   tmp_cza_lote

    -- Insertamos en ret_sal_his_rx
    DECLARE cur_ok_his CURSOR FOR
    SELECT *
    FROM   tmp_historico

    FOREACH cur_ok_his INTO lr_tmp_hist.*

        LET lr_tmp_hist.folio = pi_folio

        INSERT INTO ret_sal_his_rx
        VALUES (lr_tmp_hist.*)

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS SALDO HISTORICO    : ", li_tot_detalle AT 10,13

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio a procesar                       #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------
    
    SELECT MAX(folio) + 1
    INTO   li_folio
    FROM   glo_folio

    IF li_folio IS NULL THEN
        LET li_folio = 1
    END IF

    INSERT INTO glo_folio VALUES (li_folio)

    RETURN li_folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso para la      #
#                   bitacora de errores de carga                            #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_id_err()

    DEFINE
        li_iderr        INTEGER
        
    -- -----------------------------------------------------------------------------

    SELECT MAX(id_proceso) + 1
    INTO   li_iderr
    FROM   ret_bitacora_error

    IF li_iderr IS NULL THEN
        LET li_iderr = 1
    END IF

    RETURN li_iderr

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_ult_consec : Obtiene el ultimo consecutivo a insertarse         #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_ult_consec()

    DEFINE li_consecutivo LIKE dis_cuenta.consecutivo_lote

    EXECUTE eje_consecutivo INTO li_consecutivo
    RETURN li_consecutivo

END FUNCTION

#---------------------------------------------------------------------------#
# f_marca_cuenta : Ejecuta el SPL que realiza la marca del nss recibido     #
#---------------------------------------------------------------------------#
FUNCTION f_marca_cuenta(pr_marca)

    DEFINE pr_marca RECORD
        nss         LIKE ret_trans_issste.nss           ,
        consec      LIKE ret_trans_issste.consecutivo   ,
        tipo_ret    LIKE ret_trans_issste.tipo_retiro   ,
        usuario     LIKE ret_trans_issste.usuario
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        cod_rechazo     SMALLINT ,
        marca_causa     SMALLINT ,
        fec_causa       DATE
    END RECORD

    DEFINE
        ls_marca_res    ,
        ls_cod_rech     ,
        ls_movim        SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET lr_dat.edo_marca    = 0
    LET lr_dat.cod_rechazo  = 0
    LET lr_dat.marca_causa  = 0
    INITIALIZE lr_dat.fec_causa TO NULL

    SELECT "OK"
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pr_marca.nss
    GROUP  BY 1

    IF STATUS <> NOTFOUND THEN
        SELECT movimiento
        INTO   ls_movim
        FROM   tab_retiro
        WHERE  tipo_retiro = pr_marca.tipo_ret

        --MARCAJE--
        EXECUTE eje_marca USING pr_marca.nss            ,--nss
                                ls_movim                ,--marca entrante
                                pr_marca.consec         ,--consecutivo
                                lr_dat.edo_marca        ,--estado_marco
                                lr_dat.cod_rechazo      ,--codigo de rechazo
                                lr_dat.marca_causa      ,--marca_causa
                                lr_dat.fec_causa        ,--fecha_causa
                                pr_marca.usuario         --usuario
                          INTO  ls_marca_res   ,
                                ls_cod_rech
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE ret_solicitud_tx.nss          ,
        consec      LIKE ret_solicitud_tx.consecutivo  ,
        tipo_retiro LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_movim            SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 40
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
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usaran en el proceso     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_arch_carga
        DROP TABLE tmp_cza_lote
        DROP TABLE tmp_trans_imss_recep  --cpl2291
        DROP TABLE tmp_datamart
        DROP TABLE tmp_historico
        DROP TABLE tmp_solicitud
        DROP TABLE tmp_monto_siefore
        DROP TABLE tmp_monto_viv
    WHENEVER ERROR STOP

    #---
    CREATE TEMP TABLE tmp_arch_carga
    (
    n_registros          CHAR(800)
    )

    #---
    SELECT *
    FROM   ret_cza_lote
    WHERE  1 = 0
    INTO TEMP tmp_cza_lote

    #---
    SELECT *
    FROM   ret_transf_rx
    WHERE  1 = 0
    INTO TEMP tmp_trans_imss_recep   --cpl2291

    #---
    SELECT *
    FROM   ret_det_datamart
    WHERE  1 = 0
    INTO TEMP tmp_datamart


    #---
    SELECT *
    FROM   ret_sal_his_rx
    WHERE  1 = 0
    INTO TEMP tmp_historico

    #---
    SELECT *
    FROM   ret_solicitud_tx
    WHERE  1 = 0
    INTO TEMP tmp_solicitud

    #---
    SELECT *
    FROM   ret_monto_siefore
    WHERE  1 = 0
    INTO TEMP tmp_monto_siefore

    #---
    SELECT *
    FROM   ret_monto_viv
    WHERE  1 = 0
    INTO TEMP tmp_monto_viv

END FUNCTION

#---------------------------------------------------------------------------#
# sol_etapa_2 : Ejecuta el cierre automatico de la etapa 2 para el modulo   #
#               de servicios                                                #
#---------------------------------------------------------------------------#
FUNCTION sol_etapa_2( l_nss_afi         ,
                      l_fol_serv        ,
                      l_tipo_serv       ,
                      l_usuario )

#---          Definición de Variables que cacha la Funcion     ---
DEFINE  l_nss_afi               CHAR(11)
DEFINE  l_fol_serv              INTEGER
DEFINE  l_tipo_serv             CHAR(01)
DEFINE  l_usuario               CHAR(10)             
#---  Fin de las Definiciones de Variables que cacha la Funcion ---                                                        
                                                    
DEFINE  l_verif_txt_eta_sol            ,        
        l_procede                      ,
        l_encontro                     ,
        l_consec_sol                   ,
        l_etapa_cod                    ,
        l_resol_linea           SMALLINT
        
DEFINE  l_leyenda_eta2          CHAR(30)

DEFINE  reg_serv     RECORD
        folio_rec    LIKE safre_af:rec_solucion.folio_rec         ,    
        tipo_id      LIKE safre_af:rec_solucion.tipo_id           ,    
        tipo_cod     LIKE safre_af:rec_solucion.tipo_cod          ,    
        motivo_cod   LIKE safre_af:rec_solucion.motivo_cod        ,    
        etapa_cod    LIKE safre_af:rec_solucion.etapa_cod         ,    
        ffin_est     LIKE safre_af:rec_solucion.ffin_est          ,    
        ffin_real    LIKE safre_af:rec_solucion.ffin_real         ,    
        consec_sol   LIKE safre_af:rec_solucion.consec_sol        ,    
        termino_cod  LIKE safre_af:rec_solicitud.termino_cod      ,    
        n_seguro     LIKE safre_af:afi_mae_afiliado.n_seguro           
                     END RECORD        
        
        
    
   #Valores Fijos
   
   LET      l_consec_sol           =         2
   LET      l_etapa_cod            =         2
   LET      l_resol_linea          =         1
   LET      l_leyenda_eta2         =         "SOLICITUD DE RETIRO LIQUIDADO"

   LET      l_encontro             =         0
   LET      l_procede              =         0
                           
                           
   SELECT  COUNT(*)
   INTO    l_encontro
   FROM    rec_solicitud A
   WHERE   A.folio_rec            =        l_fol_serv
   AND     A.tipo_id              =        l_tipo_serv
   AND     A.n_seguro             =        l_nss_afi
   AND     A.termino_cod          =        50          #EN PROCESO DE SOLUCION
   
   
   CASE l_encontro  
      
      WHEN 0  #Registro  -- NO -- ENCONTRADO PARA CERRARLE ETAPA
              
         LET l_procede  = 0 #NO PROCEDE
         RETURN
         
      WHEN 1  #CASO NORMAL
        
         LET l_procede  = 1 #SE PRENDE BANDERA 
       
      OTHERWISE #Registro Duplicado NO  PUEDE CERRARSE LAS ETAPAS
      
         LET l_procede  = 0 #NO PROCEDE  
         RETURN
              
   END CASE
   
   IF  ( l_procede   =   1  )  THEN #REGISTRO SI PROCEDE
                            
                            
      #-- INICIALIZA VARIABLES --#   
            
          INITIALIZE  reg_serv.*  TO   NULL   
                        
      #-- FIN DE INICIALIZACION DE VARIABLES--#
                             
      DECLARE d CURSOR FOR 
        
         SELECT  solucion.folio_rec       ,                       
                 solucion.tipo_id         ,                       
                 solucion.tipo_cod        ,
                 solucion.motivo_cod      ,
                 solucion.etapa_cod       ,
                 solucion.ffin_est        ,
                 solucion.ffin_real       ,
                 solucion.consec_sol      ,                 
                 recsolic.termino_cod     ,
                 recsolic.n_seguro
         FROM    rec_solucion solucion    , rec_solicitud recsolic
         WHERE   solucion.folio_rec  > 0
         AND     recsolic.folio_rec       =       l_fol_serv
         AND     recsolic.tipo_id         =       l_tipo_serv
         AND     recsolic.folio_rec       =       solucion.folio_rec
         AND     recsolic.tipo_id         =       solucion.tipo_id
         AND     solucion.ffin_real  IS NULL
         AND     recsolic.termino_cod     =   50 #EN PROCESO DE SOLUCION      
         AND     solucion.consec_sol      =   2  #Esto es para cerrarle la 2da
                                                 #etapa y asegurarnos que tenga
                                                 #2da Etapa.                                  
      FOREACH d INTO reg_serv.*   
                                                                                                
         UPDATE  safre_af:rec_solicitud                                                           
         SET     termino_cod       =   90 #SOLUCIONADO
         WHERE   folio_rec         =   reg_serv.folio_rec 
         AND     tipo_id           =   reg_serv.tipo_id
         AND     n_seguro          =   reg_serv.n_seguro
         
         UPDATE  safre_af:rec_solucion                                        
         SET     ffin_real         =   TODAY                                  
         WHERE   folio_rec         =   reg_serv.folio_rec                                                                   
         AND     tipo_id           =   reg_serv.tipo_id                       
         AND     etapa_cod         =   l_etapa_cod   #2                                      
         AND     consec_sol        =   l_consec_sol  #2
         
         
         
         #Verifica si existe el registro antes de INSERTARSE 
         
         LET l_verif_txt_eta_sol   =    0
         
         SELECT  COUNT(*)
         INTO    l_verif_txt_eta_sol
         FROM    rec_txt_etapa_sol B
         WHERE   B.folio_rec         =   reg_serv.folio_rec         
         AND     B.tipo_id           =   reg_serv.tipo_id 
         AND     B.consec_sol        =   l_consec_sol    #2   
         AND     B.etapa_cod         =   l_etapa_cod     #2
         
         IF ( l_verif_txt_eta_sol = 0 ) THEN #-- NO EXISTE REG -- SE PUEDE INSERTAR               
                                                                                                
            INSERT INTO safre_af:rec_txt_etapa_sol VALUES ( reg_serv.folio_rec  ,#folio_rec       
                                                            reg_serv.tipo_id    ,#tipo_id         
                                                            l_consec_sol        ,#consec_sol      
                                                            l_etapa_cod         ,#etapa_cod       
                                                            l_resol_linea       ,#resol_linea     
                                                            l_leyenda_eta2      ,#resol_eta       
                                                            l_usuario           ,#usuario         
                                                            TODAY               #factualiza       
                                                                                                  
                                                            )                                     
                                                                                                
         ELSE #Ya existe el registro --NO SE INSERTA                                            
                                                                                                
                                                                                                
                                                                                                
         END IF                                                                                 
                                                                                                
                     
                     
      END FOREACH       
      
    
   END IF #FIN DE  VALIDACION DE REGISTRO PROCEDIDO ó NO 
   
   #---INICIALIZA VARIABLES DE SALIDA  sol_etapa_2 ---#
   
   
   LET      l_consec_sol           =         0
   LET      l_etapa_cod            =         0
   LET      l_resol_linea          =         0
   LET      l_leyenda_eta2         =         NULL
   LET      l_encontro             =         0
   LET      l_procede              =         0
   LET      l_verif_txt_eta_sol    =         0
   
   #---FIN DE INICIALIZACION DE VARIABLES ---#
   
    
                               
    
END FUNCTION




