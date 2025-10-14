#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENC102  => RECEPCION DEL ARCHIVO DE RESPUESTA DE LA OPERACION 70 DE  #
#                     PENSION MINIMA GARANTIZADA                                #
#Fecha creacion    => 5 DE ABRIL DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#                                                                               #
#Sistema           => PEN                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_dat RECORD
        folio_car_op70          INTEGER ,
        nom_archivo_car_op70    CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        enviado               LIKE pen_estado_pmg.estado_solicitud ,
        recibido              LIKE pen_estado_pmg.estado_solicitud ,
        rechazado             LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                     DATE

    DEFINE #glo #char
        ruta_arch_res_op70      CHAR(200) ,
        carga_reg               CHAR(360) ,
        enter                   CHAR(001) ,
        gc_modulo               CHAR(003) ,
        gc_usuario              CHAR(020)

    DEFINE #glo INTEGER
       gi_folio                 ,
       gi_proceso               ,
       cuantos                  INTEGER

    DEFINE
        gs_id_operacion         ,
        gs_flag_proceso         ,
        gs_flag_err             ,
        gs_codigo_afore         ,
        gs_cod_xxi              SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PENC102.log")

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " PENC102    RECEPCION ARCHIVOS OP.70 PENSION MINIMA GARANTIZADA        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "OPERACION_70"
        COMMAND "Carga archivo" "Carga Archivo de Respuesta de la Operacion 70"
            CALL f_genera_carga()

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Carga"
            CALL f_bitacora_err(0)
            CLEAR SCREEN

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

    LET HOY             = TODAY
    LET gs_id_operacion = 70

    ----- CODIGOS AFORES -----    
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_cod_xxi
    FROM   tab_afore
    WHERE  afore_desc MATCHES '*AFORE XXI*'

    IF gs_codigo_afore = gs_cod_xxi THEN
        LET gc_modulo = "ret"
    ELSE
        LET gc_modulo = "pmg"
    END IF

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = gc_modulo

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la respuesta #
#                  de la op. 70 de pension minima garantizada               #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    CALL carga_archivo() RETURNING gs_flag_proceso  ,
                                   gi_proceso       ,
                                   gi_folio

    IF gs_flag_proceso = 1 THEN #-- La carga se realizo sin errores

        CALL primer_paso(gi_folio)  #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso() #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err, gi_proceso

        IF gs_flag_err = 0 THEN
            CALL tercer_paso(gi_folio) #-- Vacia la informacion hacia las tablas fisicas
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
    CLOSE WINDOW penc1021

END FUNCTION

#---------------------------------------------------------------------------#
# carga_archivo : Captura el nombre del archivo y busca en la ruta de       #
#                 rescate si existe. En caso de existir, lo carga sin       #
#                 formato en la tabla temporal                              #
#---------------------------------------------------------------------------#
FUNCTION carga_archivo()

    DEFINE
        ls_flag             ,
        ls_procesa          SMALLINT

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    OPEN WINDOW penc1021 AT 4,4 WITH FORM "PENC1021" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Ejecutar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC102      CARGA ARCHIVO DE OPERACION 70 RETIROS PMG                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET cuantos     = 0
    LET ls_procesa  = 0

    SELECT MAX(folio_envio)
    INTO   gr_dat.folio_car_op70
    FROM   pen_detalle_op70
    WHERE  estado = gr_edo.enviado

    INITIALIZE gr_dat.nom_archivo_car_op70 TO NULL

    INPUT BY NAME gr_dat.* WITHOUT DEFAULTS

        AFTER FIELD folio_car_op70
            IF gr_dat.folio_car_op70 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF

            SELECT "OK"
            FROM   pen_detalle_op70
            WHERE  folio_envio = gr_dat.folio_car_op70
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA PMG "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF

            SELECT "OK"
            FROM   pen_recepcion
            WHERE  folio_lote       = gr_dat.folio_car_op70
            AND    tipo_operacion   = gs_id_operacion
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE RECIBIDO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF


        AFTER FIELD nom_archivo_car_op70
            IF gr_dat.nom_archivo_car_op70 IS NULL THEN
                ERROR " EL NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_car_op70
            END IF

            SELECT "OK"
            FROM   pen_recepcion
            WHERE  nom_archivo      = gr_dat.nom_archivo_car_op70
            AND    tipo_operacion   = gs_id_operacion
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA FUE CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_car_op70
            END IF

        ON KEY (ESC)
            -- Validaciones de folio
            IF gr_dat.folio_car_op70 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF

            SELECT "OK"
            FROM   pen_detalle_op70
            WHERE  folio_envio = gr_dat.folio_car_op70
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA PMG "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF

            SELECT "OK"
            FROM   pen_recepcion
            WHERE  folio_lote       = gr_dat.folio_car_op70
            AND    tipo_operacion   = gs_id_operacion
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE RECIBIDO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_car_op70
            END IF

            -- Validaciones de nom archivo
            IF gr_dat.nom_archivo_car_op70 IS NULL THEN
                ERROR " EL NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_car_op70
            END IF

            SELECT "OK"
            FROM   pen_recepcion
            WHERE  nom_archivo      = gr_dat.nom_archivo_car_op70
            AND    tipo_operacion   = gs_id_operacion
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA FUE CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_car_op70
            END IF

            -- Se realiza la carga del archivo
            LET ruta_arch_res_op70 = gs_modulo.ruta_rescate CLIPPED,"/",
                                     gr_dat.nom_archivo_car_op70 CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM ruta_arch_res_op70
            INSERT INTO tmp_arch_op70

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   cuantos
            FROM   tmp_arch_op70

            IF cuantos = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo_car_op70
            ELSE
                WHILE TRUE
                    PROMPT "¿ DESEA CARGAR EL ARCHIVO DE RESPUESTA DE LA OP.70 ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN

                            CALL f_valida_archivo() RETURNING ls_flag ,
                                                              li_id_proc

                            IF ls_flag = 1 THEN -- hubo errores en la carga
                                LET ls_procesa  = 0
                            ELSE
                                LET ls_procesa  = 1
                            END IF
                        ELSE
                            PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                        END IF
                        EXIT INPUT
                    END IF
                END WHILE
            END IF

        ON KEY (CONTROL-C, INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
    END INPUT

    RETURN ls_procesa, li_id_proc, gr_dat.folio_car_op70

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de pmg a las       #
#               tablas temporales donde se realizara su validacion          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE
        carga_reg           CHAR(600)

    DEFINE pi_folio LIKE pen_detalle_op70.folio_envio

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_op70

    FOREACH cur_ar INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "01"
                CALL f_carga_encabezado(carga_reg            ,
                                        pi_folio             ,
                                        gr_dat.nom_archivo_car_op70
                                       )
            WHEN "03"
                CALL f_carga_det(carga_reg, pi_folio)

            WHEN "09"
                CALL f_carga_sumario(carga_reg, pi_folio)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de operacion 79          #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_error RECORD
        id_proceso   LIKE ret_bitacora_error.id_proceso   ,
        nss          LIKE ret_bitacora_error.nss          ,
        curp         LIKE ret_bitacora_error.curp         ,
        tipo_campo   LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo    LIKE ret_bitacora_error.nom_campo    ,
        valor_campo  LIKE ret_bitacora_error.valor_campo  ,
        id_error     LIKE ret_bitacora_error.id_error
    END RECORD

    DEFINE lr_soli_pmg RECORD LIKE pen_detalle_op70.*

    DEFINE
        lc_max_sec      CHAR(002)

    DEFINE
        li_cont         INTEGER

    DEFINE
        ls_flag         SMALLINT


    LET ls_flag = 0
    LET li_cont = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    #-- Validaciones del detalle
    DECLARE cur_tmp CURSOR FOR
    SELECT *
    FROM   tmp_det_op70

    FOREACH cur_tmp INTO lr_soli_pmg.*

        LET lr_error.nss          = lr_soli_pmg.nss
        LET lr_error.curp         = lr_soli_pmg.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre pension minima garantizada
        CALL f_valida_pension(ls_flag            ,
                              lr_error.id_proceso,
                              lr_soli_pmg.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_detalle_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_soli_pmg.*      )
            RETURNING ls_flag

    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_folio)

    DEFINE lr_tmp_pmg RECORD LIKE pen_detalle_op70.*

    DEFINE
        pi_folio                ,
        lt_tot_rechazo          ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER

    DISPLAY "                                                                           " AT 9,1
    DISPLAY "                                                                           " AT 11,1
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros = 0
    LET li_tot_detalle   = 0
    LET lt_tot_rechazo   = 0

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_op70

    DISPLAY "FOLIO DE CARGA       : ", pi_folio AT 8,24
    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
    DISPLAY "REGISTROS RECHAZADOS       : ", lt_tot_rechazo AT 12,18

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en pen_recepcion
    INSERT INTO pen_recepcion
    SELECT *
    FROM   tmp_recepcion

    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_det_op70
    WHERE  folio_envio = pi_folio

    FOREACH cur_ok INTO lr_tmp_pmg.*

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9

        IF lr_tmp_pmg.estado = gr_edo.rechazado THEN
            LET lt_tot_rechazo = lt_tot_rechazo + 1
            DISPLAY "REGISTROS RECHAZADOS       : ", lt_tot_rechazo AT 12,18
        END IF

        UPDATE pen_detalle_op70
        SET    diag_operacion   = lr_tmp_pmg.diag_operacion ,
               estado           = lr_tmp_pmg.estado         ,
               codigo_rechazo   = lr_tmp_pmg.codigo_rechazo
        WHERE  nss              = lr_tmp_pmg.nss
        AND    folio_envio      = pi_folio
        AND    estado           = gr_edo.enviado

    END FOREACH

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

    OPEN WINDOW penc1022 AT 4,4 WITH FORM "PENC1022" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC102      BITACORA DE ERRORES DE CARGA OP.79 RETIROS PMG                   " AT 2,1 ATTRIBUTE(REVERSE)
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
            CLOSE WINDOW penc1022
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
                         " AND    programa = 'PENC102' ",
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
            CLOSE WINDOW penc1022

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW penc1022
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del encabezado#
#                      del archivo de op 70                                 #
#---------------------------------------------------------------------------#
FUNCTION f_carga_encabezado(pr_encab)

    DEFINE pr_encab RECORD
        registro        CHAR(350)                       ,
        folio           LIKE pen_recepcion.folio_lote   ,
        nom_archivo     LIKE pen_recepcion.nom_archivo
    END RECORD

    INSERT INTO tmp_recepcion
    VALUES(pr_encab.folio           ,   -- folio_lote
           gs_id_operacion          ,
           HOY                      ,
           CURRENT HOUR TO SECOND   ,
           pr_encab.nom_archivo     , -- nombre_archivo
           0                        , -- total registros
           gc_usuario               ,
           gr_edo.recibido
          )

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de la operacion 79 de pmg                           #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE pen_detalle_op70.folio_envio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE lr_soli_pmg_tmp RECORD LIKE pen_detalle_op70.*

    DEFINE
        ls_edo_recep        SMALLINT

    LET lr_soli_pmg_tmp.folio_envio         = p_folio
    LET lr_soli_pmg_tmp.nss                 = pc_registro[010,020]
    LET lr_soli_pmg_tmp.curp                = pc_registro[021,038]
    LET lr_soli_pmg_tmp.sec_pension         = pc_registro[159,160]
    LET lr_soli_pmg_tmp.tipo_retiro         = pc_registro[161,161]
    LET lr_soli_pmg_tmp.regimen             = pc_registro[162,163]
    LET lr_soli_pmg_tmp.tipo_seguro         = pc_registro[164,165]
    LET lr_soli_pmg_tmp.tipo_pension        = pc_registro[166,167]
    LET lr_soli_pmg_tmp.tipo_prestacion     = pc_registro[171,172]

    LET lr_soli_pmg_tmp.diag_operacion      = pc_registro[310,311]

    -- Verificamos si el diagnostico recibido se acepta o no
    IF lr_soli_pmg_tmp.diag_operacion <> "01" THEN
        LET ls_edo_recep  = gr_edo.rechazado
    ELSE
        LET ls_edo_recep  = gr_edo.recibido
    END IF

    LET lr_soli_pmg_tmp.codigo_rechazo      = pc_registro[312,314]
    LET lr_soli_pmg_tmp.estado              = ls_edo_recep

    INSERT INTO tmp_det_op70 VALUES(lr_soli_pmg_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de la operacion 46 de disposiciones             #
#---------------------------------------------------------------------------#
FUNCTION f_carga_sumario(pr_sumario)

    DEFINE pr_sumario RECORD
        registro        CHAR(350)                       ,
        folio           LIKE pen_recepcion.folio_lote
    END RECORD

    DEFINE li_tot_regs LIKE pen_recepcion.tot_registros

    LET li_tot_regs = pr_sumario.registro[23,28]

    UPDATE tmp_recepcion
    SET    tot_registros = li_tot_regs
    WHERE  folio_lote    = pr_sumario.folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_archivo : Realiza las validaciones del lote de la operacion 70   #
#                    de pension minima garantizada                          #
#---------------------------------------------------------------------------#
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
        lc_nss          CHAR(11),
        lc_fec8         CHAR(10),
        lc_fecha        CHAR(10)

    DEFINE
        li_cont         INTEGER

    DEFINE
        ls_flag         SMALLINT


    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    ----------  Validaciones de estructura del archivo   --------------
    LET lr_error.tipo_campo   = "ARCHIVO"
    LET lr_error.nom_campo    = " "

    -- Valida que la operacion sea 70 - Detalle de PMG
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op70
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> gs_id_operacion

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op70
    WHERE  n_registros[1,2] = "01"

    IF li_cont <> 1 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 2
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el sumario
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op70
    WHERE  n_registros[1,2] = "09"

    IF li_cont <> 1 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 3
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Archivo sin registros de detalle
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op70
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[10,20]
    FROM   tmp_arch_op70
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_nss

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op70
        WHERE  n_registros[1,2]   = "03"
        AND    n_registros[10,20] = lc_nss

        IF li_cont > 1 THEN
            LET lr_error.nom_campo   = "det 03 duplicado"
            LET ls_flag              = 1
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_nss

            CALL f_inserta_bitacora(lr_error.*)
            RETURN ls_flag, lr_error.id_proceso
        END IF

    END FOREACH

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_pension : Realiza las validaciones para el proceso de pension    #
#                    minima garantizada                                     #
#---------------------------------------------------------------------------#
FUNCTION f_valida_pension(ps_error, pi_id_error, pr_pmg_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_pmg_temp RECORD LIKE pen_detalle_op70.*

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

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_pmg_temp.nss
    LET lr_error.curp         = pr_pmg_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que el NSS no sea nulo o que venga a 11 posiciones ---
    IF (pr_pmg_temp.nss IS NULL) OR (LENGTH(pr_pmg_temp.nss) <> 11) THEN
        LET lr_error.nom_campo    = "nss"
        LET lr_error.valor_campo  = pr_pmg_temp.nss
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el tipo de retiro corresponda a pmg ---
    IF pr_pmg_temp.tipo_retiro <> "S" THEN
        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "Tipo Retiro no valido para PMG :", pr_pmg_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_pmg_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_pmg_temp RECORD LIKE pen_detalle_op70.*

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

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_pmg_temp.nss
    LET lr_error.curp         = pr_pmg_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_pmg_temp.diag_operacion IS NULL OR pr_pmg_temp.diag_operacion = "  " THEN
        LET lr_error.nom_campo    = "diag_operacion"
        LET lr_error.valor_campo  = pr_pmg_temp.diag_operacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso para la      #
#                   bitacora de errores de carga                            #
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
# f_inserta_bitacora : Inserta el registro en la bitacora de errores de     #
#                      carga                                                #
#---------------------------------------------------------------------------#
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

    LET lc_hora = TIME

    -- Campos generales
    LET lr_bitacora.programa    = "PENC102"
    LET lr_bitacora.nom_archivo = gr_dat.nom_archivo_car_op70
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
# f_tablas_tmp : Crea las tablas temporales que se usaran en el proceso     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE

    DROP TABLE tmp_arch_op70

    CREATE TEMP TABLE tmp_arch_op70
    (
    n_registros          CHAR(450)
    )

    DROP TABLE tmp_det_op70

    SELECT *
    FROM   pen_detalle_op70
    WHERE  1 = 0
    INTO TEMP tmp_det_op70

    DROP TABLE tmp_recepcion

    SELECT *
    FROM   pen_recepcion
    WHERE  1 = 0
    INTO TEMP tmp_recepcion

    WHENEVER ERROR STOP

END FUNCTION
