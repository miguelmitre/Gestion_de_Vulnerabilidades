#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC931  => RECEPCION Y CONSULTA DEL ARCHIVO DE RESOLUCIONES DE       # 
#                     RETIROS PARCIALES ISSSTE                                  #
#Fecha creacion    => 1 DE NOVIEMBRE DE 2009                                    #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 07 DE DICIEMBRE DE 2009                                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se elimina la validacion sobre la Curp + Sec Pension, en  #
#                     lugar de esto se realiza la carga del registro en el      #
#                     historico de datamart y se inserta el nuevo valor         #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 11 DE MAYO DE 2011                                        #
#                  => Se realizan las modificaciones al codigo para incluir     #
#                     el llamado de las funciones para realizar el marcaje de   #
#                     trabajador pensionado (REQ. EFPS-157)                     #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 17 DE JUNIO DE 2013                                       #
#                  => Se inserta el campo "Salario Promedio" en la carga del    #
#                     archivo de Datamart parcial ISSSTE (REQ. EFPS-244)        #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_datos RECORD
        nom_archivo_par       CHAR(20)
    END RECORD

    DEFINE
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE
        HOY                   DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_param_serv           CHAR(003) ,
        gc_tipo_op              CHAR(002) ,
        gc_usuario              CHAR(020)

    DEFINE
        gs_cod_tramite          ,
        gs_peiss                ,
        gs_cod_afore            ,
        gs_flag_err             ,
        gs_recibido             SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC931")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC931   RECEPCION DE RESOLUCIONES RETIROS PARCIALES ISSSTE           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    IF gc_param_serv = "CON" THEN
        CALL f_consulta_resol()
    ELSE
        MENU "DATAMART"
            COMMAND "Carga archivo" "Carga Archivo de Resoluciones"
                CALL f_genera_carga()
        
            COMMAND "Consulta" "Consulta Registro de Resoluciones"
                CALL f_consulta_resol()
        
            COMMAND "Bitacora" "Consulta la Bitacora de Errores de Carga"
                CALL f_bitacora_err(0)
        
            COMMAND "Salir" "Salir del Programa "
                EXIT MENU
        END MENU
    END IF

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gc_tipo_op      = "47"
    LET gc_usuario      = f_lib_obten_user()

    ----- PARAMETROS DE ENTRADA -----
    -- Solo aplica en caso de que la afore use el lanzador via servicios
    LET gc_param_serv   = ARG_VAL(1)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_peiss
    FROM   tab_afore
    WHERE  afore_desc = "PENSIONISSSTE"

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion  = "RETIRO PARCIAL ISSSTE"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gs_recibido
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECIBIDO"

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la op. 47    #
#                  de Datamart parcial ISSSTE                               #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    DEFINE
        ls_flag_proceso         SMALLINT
        
    DEFINE
       li_proceso               INTEGER
               
    -- -----------------------------------------------------------------------------

    CALL f_carga_archivo() RETURNING ls_flag_proceso  ,
                                     li_proceso

    IF ls_flag_proceso = 1 THEN #-- La carga se realizo sin errores
        
        CALL primer_paso()  #-- Vacia la informacion del archivo a las tablas de validacion
    
        CALL segundo_paso() #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err, li_proceso
    
        IF gs_flag_err = 0 THEN
            CALL tercer_paso() #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO")
            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    ELSE
        IF li_proceso <> 0 THEN
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("ARCHIVO CON INCONSISTENCIAS DE ESTRUCTURA")
            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF
    
    CLEAR SCREEN
    CLOSE WINDOW RETC9311

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de     #
#                   rescate si existe. En caso de existir, lo carga sin     #
#                   formato en la tabla temporal                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    DEFINE
        lc_ruta_arch_dtm    CHAR(200)

    DEFINE
        ls_flag             ,
        ls_procesa          SMALLINT

    DEFINE
        li_num_regs         INTEGER
        
    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()
    CALL f_tablas_tmp()

    LET li_num_regs = 0
    LET ls_procesa  = 0

    INPUT BY NAME gr_datos.nom_archivo_par WITHOUT DEFAULTS

        AFTER FIELD nom_archivo_par
            IF gr_datos.nom_archivo_par IS NULL THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo_par
            END IF

            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  nom_archivo = gr_datos.nom_archivo_par
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA CARGADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo_par
            END IF

        ON KEY (ESC)
            IF gr_datos.nom_archivo_par IS NULL THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo_par
            END IF

            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  nom_archivo = gr_datos.nom_archivo_par
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA CARGADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo_par
            END IF

            -- Se realiza la carga del archivo
            LET lc_ruta_arch_dtm =  gs_modulo.ruta_rescate CLIPPED,
                                    "/" ,
                                    gr_datos.nom_archivo_par CLIPPED

            WHENEVER ERROR CONTINUE
                LOAD FROM lc_ruta_arch_dtm
                INSERT INTO tmp_arch_resol
            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_num_regs
            FROM   tmp_arch_resol

            IF li_num_regs = 0 THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO")
                NEXT FIELD nom_archivo_par
            ELSE
                WHILE TRUE
                    PROMPT "¿DESEA REALIZAR LA CARGA DEL ARCHIVO? (S/N) : " FOR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            CALL f_valida_archivo() RETURNING ls_flag       ,
                                                              li_id_proc

                            IF ls_flag = 1 THEN -- hubo errores en la carga
                                LET ls_procesa  = 0
                            ELSE
                                LET ls_procesa  = 1
                            END IF
                        ELSE
                            CALL f_lib_error_msg("CARGA CANCELADA")
                        END IF
                        EXIT INPUT
                    END IF
                END WHILE
            END IF

        ON KEY (INTERRUPT)
            CALL f_lib_error_msg("CARGA CANCELADA")
            EXIT INPUT
    END INPUT

    RETURN ls_procesa, li_id_proc

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de datamart a las  #
#               tablas temporales donde se realizara su validacion          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_carga RECORD
        registro    CHAR(350)                           ,
        folio       LIKE ret_datamart_par_issste.folio
    END RECORD 

    -- -----------------------------------------------------------------------------

    LET lr_carga.folio  = 0

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_resol

    FOREACH cur_ar INTO lr_carga.registro
        CASE lr_carga.registro[1,2]
            WHEN "01"
                CALL f_carga_encabezado(lr_carga.*              ,
                                        gr_datos.nom_archivo_par
                                       )
            WHEN "03"
                CALL f_carga_det(lr_carga.*)

            WHEN "09"
                CALL f_carga_sumario(lr_carga.*)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de resoluciones          #
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

    DEFINE 
        lr_datamart         ,
        lr_hist_datamart    RECORD LIKE ret_datamart_par_issste.*

    DEFINE
        lc_max_sec      CHAR(002),
        lc_error        CHAR(100)

    DEFINE
        li_cont         INTEGER

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0
    LET li_cont = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    ----------  Validaciones de estructura del archivo   --------------
    LET lr_error.tipo_campo   = "ARCHIVO"
    LET lr_error.nom_campo    = " "

    -- Valida que la operacion sea 47 - Notificacion de resoluciones parciales
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_resol
    WHERE  n_registros[5,6] <> gc_tipo_op

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_resol
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
    FROM   tmp_arch_resol
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
    FROM   tmp_arch_resol
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    #-- Validaciones del detalle
    DECLARE cur_tmp CURSOR FOR
    SELECT *
    FROM   tmp_datamart_par

    FOREACH cur_tmp INTO lr_datamart.*

        LET lr_error.nss          = lr_datamart.nss
        LET lr_error.curp         = lr_datamart.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Valida que CURP no sea nulo o que venga a 18 posiciones ---
        IF (lr_datamart.curp IS NULL) OR (LENGTH(lr_datamart.curp) <> 18) THEN
            LET lr_error.nom_campo    = "curp"
            LET lr_error.valor_campo  = lr_datamart.curp
            LET lr_error.id_error     = 6
            LET ls_flag               = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        --- Valida que la Secuencia de pension no sea nula ---
        IF (lr_datamart.sec_pension IS NULL) OR (lr_datamart.sec_pension = "  ") THEN
            LET lr_error.nom_campo    = "sec_pension"
            LET lr_error.valor_campo  = lr_datamart.sec_pension
            LET lr_error.id_error     = 6
            LET ls_flag               = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        --- Valida que la Secuencia de pension recibida sea mayor a la maxima actual ---
        SELECT MAX(sec_pension)
        INTO   lc_max_sec
        FROM   ret_datamart_par_issste
        WHERE  curp = lr_datamart.curp

        IF lc_max_sec IS NOT NULL THEN
            IF lr_datamart.sec_pension < lc_max_sec THEN
                LET lr_error.nom_campo    = "sec_pension"
                LET lr_error.valor_campo  = lr_datamart.sec_pension
                LET lr_error.id_error     = 8
                LET ls_flag               = 1
                CALL f_inserta_bitacora(lr_error.*)
            END IF
        END IF

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_detalle_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_datamart.*      )
            RETURNING ls_flag

        --- Valida que los campos existan contra el catalogo ---
        CALL f_valida_detalle_catalogo(ls_flag            ,
                                       lr_error.id_proceso,
                                       lr_datamart.*      )
            RETURNING ls_flag

        --- Valida que la llave CURP - Secuencia de pension sea unica ---
        SELECT *
        INTO   lr_hist_datamart.*
        FROM   ret_datamart_par_issste
        WHERE  curp         = lr_datamart.curp
        AND    sec_pension  = lr_datamart.sec_pension

        IF STATUS <> NOTFOUND THEN

            INSERT INTO tmp_historico_dtm_par
            VALUES (lr_hist_datamart.*)
{
            LET lr_error.nom_campo    = "curp/sec_pension"
            LET lr_error.valor_campo  = lr_datamart.curp, " | ", lr_datamart.sec_pension
            LET lr_error.id_error     = 7
            LET ls_flag               = 1
            CALL f_inserta_bitacora(lr_error.*)
}
        END IF


    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE lr_tmp_cza RECORD LIKE ret_cza_datamart.*

    DEFINE 
        lr_tmp_datamart         ,
        lr_hist_datamart        RECORD LIKE ret_datamart_par_issste.*

    DEFINE
        ls_resp                 SMALLINT

    DEFINE
        li_folio                ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros    = 0
    LET li_tot_detalle      = 0
    LET li_folio            = f_lib_obtiene_ult_folio()

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_resol

    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
    DISPLAY "FOLIO DE CARGA       : ", li_folio AT 13,24

    -- Insertamos todos los registros una vez que ya han sido validados
    UPDATE tmp_cza_datamart
    SET    folio = li_folio

    INSERT INTO ret_cza_datamart
    SELECT *
    FROM   tmp_cza_datamart

    #----
    DECLARE cur_hist CURSOR FOR
    SELECT *
    FROM   tmp_historico_dtm_par
    
    FOREACH cur_hist INTO lr_hist_datamart.*
        
        INSERT INTO ret_hist_par_dtm_issste
        VALUES (li_folio          ,
                HOY               ,
                lr_hist_datamart.*
               )

        DELETE
        FROM   ret_datamart_par_issste
        WHERE  folio        = lr_hist_datamart.folio
        AND    curp         = lr_hist_datamart.curp
        AND    sec_pension  = lr_hist_datamart.sec_pension

    END FOREACH
    
    #----

    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_datamart_par

    FOREACH cur_ok INTO lr_tmp_datamart.*

        LET lr_tmp_datamart.folio = li_folio

        INSERT INTO ret_datamart_par_issste
        VALUES (lr_tmp_datamart.*)

        IF lr_tmp_datamart.diag_datamart = 101 THEN

            LET ls_resp = 0
            
            EXECUTE eje_marca_pen USING lr_tmp_datamart.nss         ,
                                        lr_tmp_datamart.curp        ,
                                        lr_tmp_datamart.folio       ,
                                        ls_resp                     , -- consecutivo
                                        "F"                         ,
                                        gs_cod_tramite              ,
                                        HOY
                                  INTO  ls_resp
        END IF

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9

    END FOREACH

    DISPLAY "                                             " AT 18,1
    CALL f_lib_error_msg("CARGA FINALIZADA CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran la carga del archivo   #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9311 AT 4,4 WITH FORM "RETC9311" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC931  RECEPCION Y CONSULTA - RESOLUCIONES RETIROS PARCIALES                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

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

    OPEN WINDOW retc9313 AT 4,4 WITH FORM "RETC9313" ATTRIBUTE(BORDER)

    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC931 BITACORA DE ERRORES DE CARGA RESOLUCIONES PARCIALES ISSSTE            " AT 2,1 ATTRIBUTE(REVERSE)
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
            CALL f_lib_error_msg("BUSQUEDA CANCELADA")
            CLEAR SCREEN
            CLOSE WINDOW RETC9313
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
                         " AND    programa = 'RETC931' ",
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
            CLOSE WINDOW RETC9313

        ELSE
            CALL f_lib_error_msg("NO EXISTEN REGISTROS")
            CLEAR SCREEN
            CLOSE WINDOW RETC9313
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_resol : Consulta la informacion que se cargo en las tablas de  #
#                    resoluciones parciales                                 #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_resol()

    DEFINE lar_dtmart ARRAY[5000] OF RECORD
        curp                 LIKE ret_datamart_par_issste.curp              ,
        sec_pension          LIKE ret_datamart_par_issste.sec_pension       ,
        folio                LIKE ret_datamart_par_issste.folio             ,
        nss                  LIKE ret_datamart_par_issste.nss               ,
        nss_issste           LIKE ret_datamart_par_issste.nss_issste        ,
        nombre_datamart      LIKE ret_datamart_par_issste.nombre_datamart   ,
        paterno_datamart     LIKE ret_datamart_par_issste.paterno_datamart  ,
        materno_datamart     LIKE ret_datamart_par_issste.materno_datamart  ,
        nombre_afore         LIKE ret_datamart_par_issste.nombre_afore      ,
        paterno_afore        LIKE ret_datamart_par_issste.paterno_afore     ,
        materno_afore        LIKE ret_datamart_par_issste.materno_afore     ,
        num_concesion        LIKE ret_datamart_par_issste.num_concesion     ,
        delegacion           LIKE ret_datamart_par_issste.delegacion        ,
        tipo_movimiento      LIKE ret_datamart_par_issste.tipo_movimiento   ,
        regimen              LIKE ret_datamart_par_issste.regimen           ,
        tipo_seguro          LIKE ret_datamart_par_issste.tipo_seguro       ,
        tipo_pension         LIKE ret_datamart_par_issste.tipo_pension      ,
        cve_pension          LIKE ret_datamart_par_issste.cve_pension       ,
        tipo_prestacion      LIKE ret_datamart_par_issste.tipo_prestacion   ,
        fecha_ini_pen        LIKE ret_datamart_par_issste.fecha_ini_pen     ,
        fecha_resolucion     LIKE ret_datamart_par_issste.fecha_resolucion  ,
        semanas_cotizadas    LIKE ret_datamart_par_issste.semanas_cotizadas ,
        diag_datamart        LIKE ret_datamart_par_issste.diag_datamart     ,
        salario_promedio     LIKE ret_datamart_par_issste.salario_promedio  ,
        estado_lote          LIKE ret_cza_datamart.estado_lote              ,
        desc_estado          LIKE ret_estado_issste.descripcion             ,
        fecha_datamart       LIKE ret_cza_datamart.fecha_datamart           ,
        fecha_carga          LIKE ret_cza_datamart.fecha_carga              ,
        fecha_proceso        LIKE ret_cza_datamart.fecha_proceso            ,
        nom_archivo          LIKE ret_cza_datamart.nom_archivo
    END RECORD

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_flag                 SMALLINT

    DEFINE
        lc_query                CHAR(1800) ,
        lc_where                CHAR(1200)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETC9312 AT 4,4 WITH FORM "RETC9312" ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "RETC931        CONSULTA DE RESOLUCIONES PARCIALES ISSSTE                       " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    LET ls_flag = 0

    CONSTRUCT BY NAME lc_where ON A.curp        ,
                                  A.folio       ,
                                  A.nss         ,
                                  B.nom_archivo

        ON KEY (CONTROL-C)
            IF ls_flag = 0 THEN
                LET ls_flag = 1
                EXIT CONSTRUCT
            END IF

        ON KEY (INTERRUPT)
            IF ls_flag = 0 THEN
                LET ls_flag = 1
                EXIT CONSTRUCT
            END IF

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF ls_flag = 1 THEN
        CALL f_lib_error_msg("BUSQUEDA CANCELADA")
        CLEAR SCREEN
        CLOSE WINDOW RETC9312
        RETURN
    END IF

    LET lc_query = "SELECT A.curp               ,",
                         " A.sec_pension        ,",
                         " A.folio              ,",
                         " A.nss                ,",
                         " A.nss_issste         ,",
                         " A.nombre_datamart    ,",
                         " A.paterno_datamart   ,",
                         " A.materno_datamart   ,",
                         " A.nombre_afore       ,",
                         " A.paterno_afore      ,",
                         " A.materno_afore      ,",
                         " A.num_concesion      ,",
                         " A.delegacion         ,",
                         " A.tipo_movimiento    ,",
                         " A.regimen            ,",
                         " A.tipo_seguro        ,",
                         " A.tipo_pension       ,",
                         " A.cve_pension        ,",
                         " A.tipo_prestacion    ,",
                         " A.fecha_ini_pen      ,",
                         " A.fecha_resolucion   ,",
                         " A.semanas_cotizadas  ,",
                         " A.diag_datamart      ,",
                         " A.salario_promedio   ,",
                         " B.estado_lote        ,",
                         " C.descripcion        ,",
                         " B.fecha_datamart     ,",
                         " B.fecha_carga        ,",
                         " B.fecha_proceso      ,",
                         " B.nom_archivo         ",
                   " FROM   ret_datamart_par_issste A,ret_cza_datamart B, ret_estado_issste C ",
                   " WHERE  ", lc_where CLIPPED,
                   " AND A.folio = B.folio ",
                   " AND B.estado_lote = C.estado_solicitud"

    PREPARE prp_busca FROM lc_query
    DECLARE cur_busca CURSOR FOR prp_busca

    LET li_cont = 1

    FOREACH cur_busca INTO lar_dtmart[li_cont].*
        LET li_cont = li_cont + 1
	    
	    IF li_cont >= 5000 THEN
	        CALL f_lib_error_msg("SE SOBREPASO LA CAPACIDAD DEL ARREGLO")
	        EXIT FOREACH
	    END IF
    END FOREACH

    IF li_cont = 1 THEN
        CLEAR FORM
        CALL f_lib_error_msg("NO EXISTEN REGISTROS")
        CLEAR SCREEN
        CLOSE WINDOW RETC9312
        RETURN
    END IF

    CALL SET_COUNT(li_cont-1)
    
    DISPLAY ARRAY lar_dtmart TO scr1.*

        ON KEY ( CONTROL-C )
	        EXIT DISPLAY

        ON KEY ( INTERRUPT )
	        EXIT DISPLAY

    END DISPLAY

    CLEAR SCREEN
    CLOSE WINDOW RETC9312

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del encabezado#
#                      del archivo de resoluciones parciales                #
#---------------------------------------------------------------------------#
FUNCTION f_carga_encabezado(pr_encab)

    DEFINE pr_encab RECORD
        registro        CHAR(350)                           ,
        folio           LIKE ret_cza_datamart.folio         ,
        nom_archivo     LIKE ret_cza_datamart.nom_archivo
    END RECORD

    DEFINE lr_cza_datamart RECORD LIKE ret_cza_datamart.*

    DEFINE
        lc_fecha_10         CHAR(10)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_cza_datamart.* TO NULL
    
    LET lc_fecha_10         = pr_encab.registro[21,22],"/",
                              pr_encab.registro[23,24],"/",
                              pr_encab.registro[17,20]

    LET lr_cza_datamart.fecha_datamart  = lc_fecha_10

    LET lc_fecha_10         = pr_encab.registro[29,30],"/",
                              pr_encab.registro[31,32],"/",
                              pr_encab.registro[25,28]

    LET lr_cza_datamart.fecha_proceso   = lc_fecha_10

    LET lr_cza_datamart.folio           = pr_encab.folio       
    LET lr_cza_datamart.fecha_carga     = HOY                  
    LET lr_cza_datamart.hora_carga      = CURRENT HOUR TO SECOND                 
    LET lr_cza_datamart.nom_archivo     = pr_encab.nom_archivo       
    LET lr_cza_datamart.tot_registros   = 0                      
    LET lr_cza_datamart.estado_lote     = gs_recibido          

    INSERT INTO tmp_cza_datamart
    VALUES(lr_cza_datamart.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de resoluciones parciales                           #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE ret_datamart_par_issste.folio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE lr_datamart_tmp  RECORD LIKE ret_datamart_par_issste.*

    DEFINE
        ls_sec_pen          SMALLINT

    DEFINE
        lc_monto_09         CHAR(09)    ,
        lc_fechas           CHAR(10)

    -- -----------------------------------------------------------------------------

    LET lr_datamart_tmp.folio               = p_folio
    LET lr_datamart_tmp.curp                = pc_registro[015,032]
    LET lr_datamart_tmp.nss                 = f_obten_nss(lr_datamart_tmp.curp)
    
    LET lr_datamart_tmp.num_issste          = pc_registro[007,014]
    LET lr_datamart_tmp.nombre_datamart     = pc_registro[033,072]
    LET lr_datamart_tmp.paterno_datamart    = pc_registro[073,112]
    LET lr_datamart_tmp.materno_datamart    = pc_registro[113,152]
    LET lr_datamart_tmp.nombre_afore        = pc_registro[153,192]
    LET lr_datamart_tmp.paterno_afore       = pc_registro[193,232]
    LET lr_datamart_tmp.materno_afore       = pc_registro[233,272]

    LET lr_datamart_tmp.nss_issste          = pc_registro[273,283]
    LET lr_datamart_tmp.num_concesion       = pc_registro[284,292]
    LET lr_datamart_tmp.delegacion          = pc_registro[293,295]

    -- Formateamos el valor de la secuencia de pension
    LET lr_datamart_tmp.sec_pension         = pc_registro[296,297]

    IF lr_datamart_tmp.sec_pension IS NOT NULL THEN
        LET ls_sec_pen                  = lr_datamart_tmp.sec_pension
        LET lr_datamart_tmp.sec_pension = ls_sec_pen USING "&&"
    END IF

    LET lr_datamart_tmp.tipo_movimiento     = pc_registro[298,300]
    LET lr_datamart_tmp.regimen             = pc_registro[301,302]
    LET lr_datamart_tmp.tipo_seguro         = pc_registro[303,304]
    LET lr_datamart_tmp.tipo_pension        = pc_registro[305,306]
    LET lr_datamart_tmp.cve_pension         = pc_registro[307,309]

    IF lr_datamart_tmp.cve_pension = " NA" OR lr_datamart_tmp.cve_pension = "   " THEN
        LET lr_datamart_tmp.cve_pension = "NA"
    END IF

    IF lr_datamart_tmp.cve_pension = " CD" THEN
        LET lr_datamart_tmp.cve_pension = "CD"
    END IF

    LET lr_datamart_tmp.tipo_prestacion     = pc_registro[310,311]

    LET lc_fechas                           = pc_registro[316,317],"/",
                                              pc_registro[318,319],"/",
                                              pc_registro[312,315]
    LET lr_datamart_tmp.fecha_ini_pen       = lc_fechas

    LET lc_fechas                           = pc_registro[324,325],"/",
                                              pc_registro[326,327],"/",
                                              pc_registro[320,323]
    LET lr_datamart_tmp.fecha_resolucion    = lc_fechas

    LET lr_datamart_tmp.semanas_cotizadas   = pc_registro[328,331]
    LET lr_datamart_tmp.diag_datamart       = pc_registro[332,334]

    -- Se agrega el campo "Salario Promedio" a la carga de datamart
    -- Req. EFPS-244
    LET lc_monto_09                         = pc_registro[335,340], ".",
                                              pc_registro[341,342]

    LET lr_datamart_tmp.salario_promedio    = lc_monto_09

    INSERT INTO tmp_datamart_par 
    VALUES(lr_datamart_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de resoluciones parciales                       #
#---------------------------------------------------------------------------#
FUNCTION f_carga_sumario(pr_sumario)

    DEFINE pr_sumario RECORD
        registro        CHAR(350)                   ,
        folio           LIKE ret_cza_datamart.folio
    END RECORD

    DEFINE li_tot_regs LIKE ret_cza_datamart.tot_registros

    -- -----------------------------------------------------------------------------

    LET li_tot_regs = pr_sumario.registro[33,38]

    UPDATE tmp_cza_datamart
    SET    tot_registros    = li_tot_regs
    WHERE  folio            = pr_sumario.folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_archivo : Realiza las validaciones del lote de la operacion 47   #
#                    de resoluciones de Datamart Parcial ISSSTE             #
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
        lc_curp         CHAR(18),
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

    -- Valida que la operacion sea 47 - Resoluciones Parciales ISSSTE
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_resol
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> gc_tipo_op

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_resol
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
    FROM   tmp_arch_resol
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
    FROM   tmp_arch_resol
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---
    DECLARE cur_det03 CURSOR FOR
    SELECT n_registros[15,32]
    FROM   tmp_arch_resol
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_det03 INTO lc_curp

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_resol
        WHERE  n_registros[1,2]   = "03"
        AND    n_registros[15,32] = lc_curp

        IF li_cont > 1 THEN
            LET lr_error.nom_campo   = "det 03 duplicado"
            LET ls_flag              = 1
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_curp

            CALL f_inserta_bitacora(lr_error.*)
            RETURN ls_flag, lr_error.id_proceso
        END IF

    END FOREACH

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_dtm_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_dtm_temp RECORD LIKE ret_datamart_par_issste.*

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
    LET lr_error.nss          = pr_dtm_temp.nss
    LET lr_error.curp         = pr_dtm_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_dtm_temp.nombre_datamart IS NULL OR
       pr_dtm_temp.nombre_datamart = "                                        " THEN
        LET lr_error.nom_campo    = "nombre_datamart"
        LET lr_error.valor_campo  = pr_dtm_temp.nombre_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.paterno_datamart IS NULL OR
       pr_dtm_temp.paterno_datamart = "                                        " THEN
        LET lr_error.nom_campo    = "paterno_datamart"
        LET lr_error.valor_campo  = pr_dtm_temp.paterno_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.nombre_afore IS NULL OR
       pr_dtm_temp.nombre_afore = "                                        " THEN
        LET lr_error.nom_campo    = "nombre_afore"
        LET lr_error.valor_campo  = pr_dtm_temp.nombre_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.paterno_afore IS NULL OR
       pr_dtm_temp.paterno_afore = "                                        " THEN
        LET lr_error.nom_campo    = "paterno_afore"
        LET lr_error.valor_campo  = pr_dtm_temp.paterno_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.num_concesion IS NULL OR
       pr_dtm_temp.num_concesion = "         " THEN
        LET lr_error.nom_campo    = "num_concesion"
        LET lr_error.valor_campo  = pr_dtm_temp.num_concesion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.tipo_movimiento IS NULL OR pr_dtm_temp.tipo_movimiento = 0 THEN
        LET lr_error.nom_campo    = "tipo_movimiento"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_movimiento
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.regimen IS NULL OR pr_dtm_temp.regimen = "  " THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_dtm_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.tipo_seguro IS NULL OR pr_dtm_temp.tipo_seguro = "  " THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.tipo_pension IS NULL OR pr_dtm_temp.tipo_pension = "  " THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.cve_pension IS NULL OR pr_dtm_temp.cve_pension = "   " THEN
        LET lr_error.nom_campo    = "cve_pension"
        LET lr_error.valor_campo  = pr_dtm_temp.cve_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.tipo_prestacion IS NULL THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.fecha_resolucion IS NULL THEN
        LET lr_error.nom_campo    = "fecha_resolucion"
        LET lr_error.valor_campo  = pr_dtm_temp.fecha_resolucion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.diag_datamart IS NULL OR pr_dtm_temp.diag_datamart = 0 THEN
        LET lr_error.nom_campo    = "diag_datamart"
        LET lr_error.valor_campo  = pr_dtm_temp.diag_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_catalogo : Valida que existan los valores de los campos  #
#                             que tengan su propio catalogo                 #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_catalogo(ps_error, pi_id_error, pr_dtm_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_dtm_temp RECORD LIKE ret_datamart_par_issste.*

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
    LET lr_error.nss          = pr_dtm_temp.nss
    LET lr_error.curp         = pr_dtm_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 9

    -- Valida contra catalogo de Regimen
    SELECT "OK"
    FROM   tab_regimen_issste
    WHERE  regimen = pr_dtm_temp.regimen
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "regimen"
        LET lr_error.valor_campo  = pr_dtm_temp.regimen
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Seguro
    SELECT "OK"
    FROM   tab_seguro_issste
    WHERE  clave = pr_dtm_temp.tipo_seguro
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_seguro"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_seguro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Pension
    SELECT "OK"
    FROM   tab_pension_issste
    WHERE  tipo_pension = pr_dtm_temp.tipo_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Clave de Pension
    SELECT "OK"
    FROM   tab_cve_pen_issste
    WHERE  cve_pension = pr_dtm_temp.cve_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "cve_pension"
        LET lr_error.valor_campo  = pr_dtm_temp.cve_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Prestacion
    SELECT "OK"
    FROM   tab_prestacion_issste
    WHERE  tipo_prestacion = pr_dtm_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_nss : Obtiene el nss del trabajador de la tabla afi_mae_afiliado  #
#---------------------------------------------------------------------------#
FUNCTION f_obten_nss(pc_curp)

    DEFINE pc_curp LIKE ret_datamart_par_issste.curp
    
    DEFINE lc_nss       LIKE ret_datamart_par_issste.nss
    DEFINE ld_saldo_acc LIKE dis_cuenta.monto_en_acciones

    -- -----------------------------------------------------------------------------

    LET lc_nss = NULL

    IF (gs_cod_afore <> gs_peiss) THEN 
        -- CPL-1660
        -- Se valida que el NSS tenga saldo en la cuenta individual
        -- para asegurar que se tome el registro activo
        DECLARE cur_afi_nss CURSOR FOR
            SELECT n_seguro
            INTO   lc_nss
            FROM   afi_mae_afiliado
            WHERE  n_unico = pc_curp
            
        FOREACH cur_afi_nss INTO lc_nss
            SELECT NVL(SUM(monto_en_acciones), 0)
            INTO   ld_saldo_acc
            FROM   dis_cuenta
            WHERE  nss  = lc_nss
            
            IF (ld_saldo_acc <= 0) THEN
                LET lc_nss = NULL
                CONTINUE FOREACH
            ELSE
                EXIT FOREACH
            END IF             
        END FOREACH
    ELSE
        SELECT n_seguro
        INTO   lc_nss
        FROM   afi_mae_afiliado
        WHERE  n_unico        = pc_curp
        AND    tipo_solicitud = 8

        IF STATUS = NOTFOUND THEN
            LET lc_nss = NULL
        END IF

    END IF
  
    RETURN lc_nss
    
END FUNCTION 

#---------------------------------------------------------------------------#
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso              #
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
    LET lr_bitacora.programa    = "RETC931"
    LET lr_bitacora.nom_archivo = gr_datos.nom_archivo_par
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

    #----
    DROP TABLE tmp_arch_resol
    CREATE TEMP TABLE tmp_arch_resol
    (
    n_registros          CHAR(400)
    )

    #----
    DROP TABLE tmp_datamart_par

    SELECT *
    FROM   ret_datamart_par_issste
    WHERE  1 = 0
    INTO TEMP tmp_datamart_par

    #----
    DROP TABLE tmp_historico_dtm_par

    SELECT *
    FROM   ret_datamart_par_issste
    WHERE  1 = 0
    INTO TEMP tmp_historico_dtm_par
    
    #----
    DROP TABLE tmp_cza_datamart

    SELECT *
    FROM   ret_cza_datamart
    WHERE  1 = 0
    INTO TEMP tmp_cza_datamart

    WHENEVER ERROR STOP

END FUNCTION
