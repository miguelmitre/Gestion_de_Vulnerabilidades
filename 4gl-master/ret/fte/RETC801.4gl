#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC801  => RECEPCION Y CONSULTA DEL ARCHIVO DE DATAMART IMSS         #
#Fecha creacion    => 22 DE ABRIL DE 2003                                       #
#By                => DMR                                                       #
#Fecha actualiz.   => 10 DE MARZO DE 2010                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se modifico el programa para que tenga la misma           #
#                     funcionalidad que tiene la carga de datamart de retiros   #
#                     ISSSTE                                                    #
#Fecha actualiz.   => 19 DE MAYO DE 2010                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se elimina validacion para evitar cargar registros con    #
#                     secuencia de pension menor a la maxima cargada. Solo se   #
#                     almacenan los registros a reemplazar en la tabla historica#
#Fecha actualiz.   => 28 DE MARZO DE 2011                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Modificaciones al proceso de validacion de suficiencia de #
#                     saldo para PMG. (Requerimiento - EFPS 155)                #
#Fecha actualiz.   => 23 DE ABRIL DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Modificaciones a la consulta de resoluciones para cargar  #
#                     las resoluciones que llegan por Ventanilla 2.5            #
#                     (Requerimiento - EFPS-176)                                #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_nom RECORD #glo #gr_nom
        nom_archivo_dtm       CHAR(20)
    END RECORD

    DEFINE #glo #record
        gr_modulo RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                     DATE

    DEFINE #glo #char
        enter                   CHAR(001) ,
        gc_usuario              CHAR(020) ,
        gc_param_serv           CHAR(003)

    DEFINE #glo INTEGER
       gi_proceso               INTEGER

    DEFINE #glo #smallint
        gs_cod_afore            ,
        gs_flag_err             ,
        gs_capturado_pmg        ,
        gs_recibido_ret         SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC801")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC801         RECEPCION Y CONSULTA - ARCHIVOS DATAMART                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    IF gc_param_serv = "CON" THEN
        CALL f_consulta_dtm()
    ELSE
        MENU "DATAMART"
            COMMAND "Carga archivo" "Carga Archivo de Datamart"
                CALL f_genera_carga()

            COMMAND "Consulta" "Consulta registros cargados de Datamart"
                CALL f_consulta_dtm()

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

    LET HOY         = TODAY
    LET gc_usuario  = f_lib_obten_user()

    CALL f_tablas_tmp()

    ----- PARAMETROS DE ENTRADA -----
    -- Solo aplica en caso de que la afore use el lanzador via servicios
    LET gc_param_serv   = ARG_VAL(1)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT *
    INTO   gr_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.estado_solicitud
    INTO   gs_capturado_pmg
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gs_recibido_ret
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    ----- VALIDA SALDOS PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_valida_suficiencia_pmg(?,?,?,?) "
    PREPARE eje_valida_pmg FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare
    
    LET lc_prepare = " "

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de datamart     #
#                  IMSS                                                     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    DEFINE
        li_ult_folio        INTEGER

    -- -----------------------------------------------------------------------------

    IF f_carga_archivo() THEN
        CALL primer_paso()  #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso() #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err, gi_proceso

        IF gs_flag_err = 0 THEN
            CALL tercer_paso() #-- Vacia la informacion hacia las tablas fisicas
                RETURNING li_ult_folio

            CALL cuarto_paso(li_ult_folio) #-- Valida la suficiencia para los tipos de retiro S
        ELSE
            DISPLAY "                                             " AT 18,1
            PROMPT " SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO ... <ENTER> PARA MOSTRAR" FOR CHAR enter

            CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

    CLEAR SCREEN
    CLOSE WINDOW RETC8011

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de       #
#                 rescate si existe. En caso de existir, lo carga sin       #
#                 formato en la tabla temporal                              #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE
        li_num_totreg       INTEGER

    DEFINE
        ls_procesa          SMALLINT

    DEFINE
        lc_ruta_archivo     CHAR(200)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETC8011 AT 4,4 WITH FORM "RETC8011" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC801          CARGA ARCHIVO DE RESOLUCIONES DATAMART                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET li_num_totreg   = 0
    LET ls_procesa      = 0

    INPUT BY NAME gr_nom.nom_archivo_dtm WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo_dtm
            LET gr_nom.nom_archivo_dtm = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo_dtm
            IF gr_nom.nom_archivo_dtm IS NULL THEN
                CALL f_lib_error_msg("CAMPO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo_dtm
            END IF

            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  nom_archivo = gr_nom.nom_archivo_dtm
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA PROCESADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo_dtm
            END IF

            LET lc_ruta_archivo = gr_modulo.ruta_rescate CLIPPED,"/",
                                  gr_nom.nom_archivo_dtm CLIPPED

            WHENEVER ERROR CONTINUE
                LOAD FROM lc_ruta_archivo
                INSERT INTO tmp_arch_datamart
            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_num_totreg
            FROM   tmp_arch_datamart

            IF li_num_totreg = 0 THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO")
                NEXT FIELD nom_archivo_dtm
            ELSE
                WHILE TRUE
                    PROMPT "¿DESEA CARGAR ARCHIVO DATAMART (S/N)? " FOR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_procesa  = 1
                            EXIT INPUT
                        ELSE
                            CALL f_lib_error_msg("CARGA CANCELADA")
                            EXIT WHILE
                        END IF
                    END IF
                END WHILE
            END IF

        ON KEY (INTERRUPT)
            CALL f_lib_error_msg("CARGA CANCELADA")
            EXIT INPUT
    END INPUT

    RETURN ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de datamart a las  #
#               tablas temporales donde se realizara su validacion          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE li_folio LIKE ret_det_datamart.folio

    DEFINE
        lc_registro           CHAR(350)

    -- -----------------------------------------------------------------------------

    LET li_folio = 0

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_datamart

    FOREACH cur_ar INTO lc_registro
        CASE lc_registro[1,2]
            WHEN "01"
                CALL f_carga_encabezado(lc_registro             ,
                                        li_folio                ,
                                        gr_nom.nom_archivo_dtm
                                       )
            WHEN "03"
                CALL f_carga_det(lc_registro, li_folio)

            WHEN "09"
                CALL f_carga_sumario(lc_registro, li_folio)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de datamart              #
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
        lr_hist_datamart    RECORD LIKE ret_det_datamart.*

    DEFINE
        lc_max_sec      CHAR(002),
        lc_error        CHAR(100)

    DEFINE
        li_cont         INTEGER

    DEFINE
        ls_tipo_oper    ,
        ls_max_sec      ,
        ls_sec_pension  ,
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0
    LET li_cont = 0

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    ----------  Validaciones de estructura del archivo   --------------
    LET lr_error.tipo_campo   = "ARCHIVO"
    LET lr_error.nom_campo    = " "

    -- Valida que la operacion sea 01 - Notificacion de resoluciones
    SELECT NVL(COUNT(*), 0)
    INTO   li_cont
    FROM   tmp_arch_datamart
    WHERE  n_registros[5,6] <> "01"

    IF li_cont <> 0 THEN
        -- En caso de no ser 01, valida que la operacion sea 
        -- 50 - Resoluciones emitidas en una afore diferente a la actual
        -- Req. CPL-1267
        SELECT NVL(COUNT(*), 0)
        INTO   li_cont
        FROM   tmp_arch_datamart
        WHERE  n_registros[5,6] <> "50"
        
        IF li_cont <> 0 THEN
            -- En caso de no ser 01, valida que la operacion sea 
            -- 71 - Notificacion de resoluciones PMG
            SELECT NVL(COUNT(*), 0)
            INTO   li_cont
            FROM   tmp_arch_datamart
            WHERE  n_registros[5,6] <> "71"
            
            IF li_cont <> 0 THEN
                LET ls_flag           = 1
                LET lr_error.id_error = 1
                CALL f_inserta_bitacora(lr_error.*)
                RETURN ls_flag, lr_error.id_proceso
            ELSE
                -- En caso de ser una operacion 71, se validan los precios de accion
                CALL f_valida_precio_acc(HOY)
            END IF -- Op. 71
        END IF -- Op. 50
    END IF -- Op. 01

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_datamart
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
    FROM   tmp_arch_datamart
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
    FROM   tmp_arch_datamart
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
    FROM   tmp_det_datamart

    FOREACH cur_tmp INTO lr_datamart.*

        LET lr_error.nss          = lr_datamart.nss
        LET lr_error.curp         = lr_datamart.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Valida que NSS no sea nulo o que venga a 11 posiciones ---
        IF (lr_datamart.nss IS NULL) OR (LENGTH(lr_datamart.nss) <> 11) THEN
            LET lr_error.nom_campo    = "nss"
            LET lr_error.valor_campo  = lr_datamart.nss
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

        --- Valida que el nss este afiliado en la afore ---
        SELECT "OK"
        FROM   afi_mae_afiliado
        WHERE  n_seguro = lr_datamart.nss
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            UPDATE tmp_det_datamart
            SET    diag_datamart = "503"
            WHERE  folio         = lr_datamart.folio
            AND    nss           = lr_datamart.nss
            AND    sec_pension   = lr_datamart.sec_pension
        END IF

        -- Verifica que la llave NSS - Secuencia de pension sea unica,
        -- En caso de encontrarse ya en la base de datos, guarda dichos registros
        -- que se ingresara en el historico.
        SELECT "OK"
        FROM   ret_det_datamart
        WHERE  nss          = lr_datamart.nss
        AND    sec_pension  = lr_datamart.sec_pension
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            DECLARE cur_dupli CURSOR FOR
            SELECT *
            FROM   ret_det_datamart
            WHERE  nss          = lr_datamart.nss
            AND    sec_pension  = lr_datamart.sec_pension

            FOREACH cur_dupli INTO lr_hist_datamart.*
                INSERT INTO tmp_historico_dtm
                VALUES (lr_hist_datamart.*)
            END FOREACH
        END IF -- Duplicidad en nss + sec_pension

    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE lr_datos RECORD
        diag_datamart       LIKE ret_det_datamart.diag_datamart ,
        num_regs            INTEGER
    END RECORD

    DEFINE lr_tmp_cza       RECORD LIKE ret_cza_datamart.*

    DEFINE
        lr_tmp_datamart     ,
        lr_hist_datamart    RECORD LIKE ret_det_datamart.*

    DEFINE
        li_folio                ,
        li_tot_registros        ,
        li_tot_duplicados       ,
        li_tot_detalle          INTEGER

    DEFINE
        ls_resp                 ,
        ls_cod_tramite          ,
        ls_screen               SMALLINT

    DEFINE
        lc_tipo_retiro          CHAR(1)

    DEFINE
        ldt_hora_act            DATETIME HOUR TO SECOND
    
    -- -------------------------------------------------------------------------

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"
    
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros    = 0
    LET li_tot_duplicados   = 0
    LET li_tot_detalle      = 0
    LET li_folio            = f_lib_obtiene_ult_folio()

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_datamart
    WHERE  n_registros[1,2] = "03"


    DISPLAY "TOTAL REGISTROS PROCESADOS       : ", li_tot_registros  AT 9,2
    DISPLAY "TOTAL RESPALDADOS EN HISTORICO   : ", li_tot_duplicados AT 10,2
    DISPLAY "TOTAL REGISTROS CARGADOS         : ", li_tot_detalle    AT 11,2
    DISPLAY "FOLIO DE CARGA   :  ", li_folio AT 16,2

    DISPLAY "DIAGNOSTICO       TOTAL" AT 7,48
    DISPLAY "-----------------------------" AT 8,45

    -- Insertamos todos los registros una vez que ya han sido validados

    ---------------------------------------------------------------------------

    UPDATE tmp_cza_datamart
    SET    folio = li_folio

    INSERT INTO ret_cza_datamart
    SELECT *
    FROM   tmp_cza_datamart

    ---------------------------------------------------------------------------

    DECLARE cur_hist CURSOR FOR
    SELECT *
    FROM   tmp_historico_dtm

    FOREACH cur_hist INTO lr_hist_datamart.*

        LET ldt_hora_act = CURRENT HOUR TO SECOND
        
        INSERT INTO ret_dtm_historico
        VALUES (li_folio            ,
                HOY                 ,
                ldt_hora_act        ,
                lr_hist_datamart.*
               )

        DELETE
        FROM   ret_det_datamart
        WHERE  folio        = lr_hist_datamart.folio
        AND    nss          = lr_hist_datamart.nss
        AND    sec_pension  = lr_hist_datamart.sec_pension

    END FOREACH

    SELECT COUNT(UNIQUE nss)
    INTO   li_tot_duplicados
    FROM   ret_dtm_historico
    WHERE  folio_reemplazo = li_folio

    DISPLAY "TOTAL RESPALDADOS EN HISTORICO   : ", li_tot_duplicados AT 10,2

    ---------------------------------------------------------------------------

    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_det_datamart

    FOREACH cur_ok INTO lr_tmp_datamart.*

        LET lr_tmp_datamart.folio = li_folio

        INSERT INTO ret_det_datamart
        VALUES (lr_tmp_datamart.*)
        
        LET ls_resp     = 0

        IF lr_tmp_datamart.estado_sub_viv IS NOT NULL THEN 
            LET lc_tipo_retiro = "S"            
        ELSE 
            SELECT tipo_retiro
            INTO   lc_tipo_retiro
            FROM   ret_matriz_derecho
            WHERE  regimen          = lr_tmp_datamart.regimen        
            AND    tipo_seguro      = lr_tmp_datamart.tipo_seguro    
            AND    tipo_pension     = lr_tmp_datamart.tipo_pension   
            AND    tipo_prestacion  = lr_tmp_datamart.tipo_prestacion
            AND    tipo_retiro IN ("D", "E", "J")
        END IF

        EXECUTE eje_marca_pen USING lr_tmp_datamart.nss     ,
                                    lr_tmp_datamart.curp    ,
                                    lr_tmp_datamart.folio   ,
                                    ls_resp                 ,
                                    lc_tipo_retiro          ,
                                    ls_cod_tramite          ,
                                    HOY
                              INTO  ls_resp

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL REGISTROS CARGADOS         : ", li_tot_detalle AT 11,2

    END FOREACH

    --- Se muestra el detalle de diagnosticos procesados en el archivo ----

    -- Determina la fila inicial a partir de donde se muestran los registros en pantalla
    LET ls_screen = 9

    DECLARE cur_diags CURSOR FOR
    SELECT diag_datamart    ,
           COUNT(*)
    FROM   ret_det_datamart
    WHERE  folio = li_folio
    GROUP BY 1
    ORDER BY 1

    FOREACH cur_diags INTO lr_datos.*

        DISPLAY lr_datos.diag_datamart AT ls_screen, 52
        DISPLAY lr_datos.num_regs      AT ls_screen, 67

        LET ls_screen = ls_screen + 1

    END FOREACH

    RETURN li_folio

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Valida la suficiencia de saldos para los tipos de retiro S  #
#               (PMG) y almacena la informacion de rechazo en caso de no    #
#               pasar la validacion                                         #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso(pi_folio)

    DEFINE
        pi_folio        INTEGER

    DEFINE lr_datamart RECORD
        nss             LIKE ret_det_datamart.nss           ,
        curp            LIKE ret_det_datamart.curp          ,
        sec_pension     LIKE ret_det_datamart.sec_pension
    END RECORD

    DEFINE lr_datos_val RECORD
        retroactivo             DECIMAL(16,6)   ,
        devengado               DECIMAL(16,6)   ,
        saldo_dia               DECIMAL(16,6)   ,
        id_suficiencia          SMALLINT
    END RECORD

    DEFINE
        ls_id_importe               ,
        ls_meses_cubiertos          SMALLINT

    DEFINE
        li_tot_ret_s                ,
        li_tot_no_valido            INTEGER

    -- -----------------------------------------------------------

    DISPLAY "                                             " AT 18,1
    DISPLAY " VALIDANDO INSUFICIENCIA PMG ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET ls_id_importe       = 1 -- Por default se valida la suficiencia sobre el importe mas el 11%
    LET ls_meses_cubiertos  = 0
    LET li_tot_ret_s        = 0
    LET li_tot_no_valido    = 0

    DECLARE cur_dtm CURSOR FOR
    SELECT nss          ,
           curp         ,
           sec_pension
    FROM   ret_det_datamart
    WHERE  folio            = pi_folio
    AND    regimen          = 97
    AND    tipo_seguro      = "CV"
    AND    tipo_pension     = "RP"
    AND    tipo_prestacion  = 2

    FOREACH cur_dtm INTO lr_datamart.*

        LET li_tot_ret_s = li_tot_ret_s + 1

        INITIALIZE lr_datos_val.* TO NULL
        
        EXECUTE eje_valida_pmg USING lr_datamart.nss            ,
                                     lr_datamart.sec_pension    ,
                                     HOY                        ,
                                     ls_id_importe
                               INTO lr_datos_val.*

        IF lr_datos_val.id_suficiencia = 0 THEN
            LET li_tot_no_valido = li_tot_no_valido + 1
        END IF 

        DISPLAY "TOTAL REGISTROS PMG              : ", li_tot_ret_s AT 13,2
        DISPLAY "REGISTROS PMG SALDO INSUFICIENTE : ", li_tot_no_valido AT 14,2

    END FOREACH


    DISPLAY "                                             " AT 18,1
    CALL f_lib_error_msg("PROCESO FINALIZADO")

END FUNCTION


#---------------------------------------------------------------------------#
# f_bitacora_err : Consulta la informacion de la bitacora de errores, ya sea#
#                  por medio del menu principal o de forma automatica al    #
#                  presentarse un error en la carga                         #
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

    OPEN WINDOW RETC8013 AT 4,4 WITH FORM "RETC8013" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC801          BITACORA DE ERRORES DE CARGA DATAMART                        " AT 2,1 ATTRIBUTE(REVERSE)
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
            CLOSE WINDOW RETC8013
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
                         " AND    programa = 'RETC801' ",
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
            CLOSE WINDOW RETC8013

        ELSE
            CALL f_lib_error_msg("NO EXISTEN REGISTROS")
            CLEAR SCREEN
            CLOSE WINDOW RETC8013
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_DTM_consulta : Genera la tabla temporal con los datos a mostrar  #
#                         de acuerdo a la busqueda hecha por el usuario     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_DTM_consulta(pc_busca)

    DEFINE
        pc_busca                CHAR(500)

    DEFINE lr_dtm_consulta RECORD
        nss                 LIKE ret_det_datamart.nss               ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        folio               LIKE ret_det_datamart.folio             ,
        curp                LIKE ret_det_datamart.curp              ,
        nombre_datamart     LIKE ret_det_datamart.nombre_datamart   ,
        nombre_afore        LIKE ret_det_datamart.nombre_afore      ,
        paterno_afore       LIKE ret_det_datamart.paterno_afore     ,
        materno_afore       LIKE ret_det_datamart.materno_afore     ,
        tipo_movimiento     LIKE ret_det_datamart.tipo_movimiento   ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        art_negativa        LIKE ret_det_datamart.art_negativa      ,
        frac_negativa       LIKE ret_det_datamart.frac_negativa     ,
        num_considerando    LIKE ret_det_datamart.num_considerando  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        porcentaje_val      LIKE ret_det_datamart.porcentaje_val    ,
        semanas_cotizadas   LIKE ret_det_datamart.semanas_cotizadas ,
        diag_datamart       LIKE ret_det_datamart.diag_datamart     ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        tipo_retiro         LIKE ret_det_datamart.tipo_retiro       ,
        monto_sol_imss      LIKE ret_det_datamart.monto_sol_imss    ,
        estado_lote         LIKE ret_cza_datamart.estado_lote       ,
        desc_estado         LIKE ret_estado.descripcion             ,
        desc_cargado        CHAR(15)                                ,
        fecha_datamart      LIKE ret_cza_datamart.fecha_datamart    ,
        fecha_carga         LIKE ret_cza_datamart.fecha_carga       ,
        fecha_proceso       LIKE ret_cza_datamart.fecha_proceso     ,
        nom_archivo         LIKE ret_cza_datamart.nom_archivo         
    END RECORD 

    DEFINE
        lc_query                CHAR(10000)
        
    DEFINE
        li_cont                 SMALLINT

    -- -------------------------------------------------------------------
    
    INITIALIZE lr_dtm_consulta.* TO NULL
    
    DELETE 
    FROM   tmp_dtm_consulta
    
    LET li_cont = 0
    
    LET lc_query = 
                "SELECT A.nss                 ,", -- Consulta los cargados desde datamart
                      " A.sec_pension         ,",
                      " A.folio               ,",
                      " A.curp                ,",
                      " A.nombre_datamart     ,",
                      " A.nombre_afore        ,",
                      " A.paterno_afore       ,",
                      " A.materno_afore       ,",
                      " A.tipo_movimiento     ,",
                      " A.regimen             ,",
                      " A.tipo_seguro         ,",
                      " A.tipo_pension        ,",
                      " A.tipo_prestacion     ,",
                      " A.art_negativa        ,",
                      " A.frac_negativa       ,",
                      " A.num_considerando    ,",
                      " A.fecha_ini_pen       ,",
                      " A.fecha_resolucion    ,",
                      " A.porcentaje_val      ,",
                      " A.semanas_cotizadas   ,",
                      " A.diag_datamart       ,",
                      " A.estado_sub_viv      ,",
                      " ' '                   ,",
                      " 0                     ,",
                      " B.estado_lote        ,",
                      " C.descripcion        ,",
                      " 'RESOLUCION DTM'     ,",
                      " B.fecha_datamart     ,",
                      " B.fecha_carga        ,",
                      " B.fecha_proceso      ,",
                      " B.nom_archivo         ",
               " FROM   ret_det_datamart A, ret_cza_datamart B, ret_estado C ",
               " WHERE  ",pc_busca CLIPPED,
               " AND A.folio = B.folio ",
               " AND B.estado_lote = C.estado_solicitud",
               " UNION ",
                "SELECT A.nss                 ,", -- Consulta los registros del historico de datamart
                      " A.sec_pension         ,",
                      " A.folio               ,",
                      " A.curp                ,",
                      " A.nombre_datamart     ,",
                      " A.nombre_afore        ,",
                      " A.paterno_afore       ,",
                      " A.materno_afore       ,",
                      " A.tipo_movimiento     ,",
                      " A.regimen             ,",
                      " A.tipo_seguro         ,",
                      " A.tipo_pension        ,",
                      " A.tipo_prestacion     ,",
                      " A.art_negativa        ,",
                      " A.frac_negativa       ,",
                      " A.num_considerando    ,",
                      " A.fecha_ini_pen       ,",
                      " A.fecha_resolucion    ,",
                      " A.porcentaje_val      ,",
                      " A.semanas_cotizadas   ,",
                      " A.diag_datamart       ,",
                      " A.estado_sub_viv      ,",
                      " ' '                   ,",
                      " 0                     ,",
                      " B.estado_lote         ,",
                      " C.descripcion         ,",
                      " 'HISTORICO DTM'       ,",
                      " B.fecha_datamart      ,",
                      " B.fecha_carga         ,",
                      " B.fecha_proceso       ,",
                      " B.nom_archivo          ",
               " FROM   ret_dtm_historico A, ret_cza_datamart B, ret_estado C ",
               " WHERE  ",pc_busca CLIPPED,
               " AND A.folio = B.folio ",
               " AND B.estado_lote = C.estado_solicitud",
               " UNION ",
               "SELECT A.nss                 ,", -- Consulta los cargados desde transferencias
                     " A.sec_pension         ,",
                     " A.folio               ,",
                     " A.curp                ,",
                     " A.nombre_datamart     ,",
                     " A.nombre_afore        ,",
                     " A.paterno_afore       ,",
                     " A.materno_afore       ,",
                     " A.tipo_movimiento     ,",
                     " A.regimen             ,",
                     " A.tipo_seguro         ,",
                     " A.tipo_pension        ,",
                     " A.tipo_prestacion     ,",
                     " A.art_negativa        ,",
                     " A.frac_negativa       ,",
                     " A.num_considerando    ,",
                     " A.fecha_ini_pen       ,",
                     " A.fecha_resolucion    ,",
                     " A.porcentaje_val      ,",
                     " A.semanas_cotizadas   ,",
                     " A.diag_datamart       ,",
                     " A.estado_sub_viv      ,",
                     " ' '                   ,",
                     " 0                     ,",
                     " B.estado_lote         ,",
                     " C.descripcion         ,",
                     " 'TRANSFERENCIAS'      ,",
                     " B.fecha_operacion     ,",
                     " B.fecha_carga         ,",
                     " B.fecha_valor_trans   ,",
                     " B.nom_archivo          ",
               " FROM   ret_det_datamart A, ret_cza_lote B, ret_estado C ",
               " WHERE  ",pc_busca CLIPPED,
               " AND A.folio = B.folio ",
               " AND B.estado_lote = C.estado_solicitud",
               " UNION " ,
                "SELECT A.nss                 , ", -- Consulta los cargados via WEBSERVICES
                      " A.sec_pension         , ",                         
                      " A.folio               , ",                         
                      " A.curp                , ",                         
                      " A.nombre_datamart     , ",                         
                      " A.nombre_afore        , ",                         
                      " A.paterno_afore       , ",                         
                      " A.materno_afore       , ",                         
                      " A.tipo_movimiento     , ",                         
                      " A.regimen             , ",                         
                      " A.tipo_seguro         , ",                         
                      " A.tipo_pension        , ",                         
                      " A.tipo_prestacion     , ",                         
                      " A.art_negativa        , ",                         
                      " A.frac_negativa       , ",                         
                      " A.num_considerando    , ",                         
                      " A.fecha_ini_pen       , ",                         
                      " A.fecha_resolucion    , ",                         
                      " A.porcentaje_val      , ",                         
                      " A.semanas_cotizadas   , ",                         
                      " A.diag_datamart       , ",                         
                      " A.estado_sub_viv      , ",                         
                      " A.tipo_retiro         , ",                         
                      " A.monto_sol_imss      , ",                         
                      " A.estado              , ",                         
                      " C.descripcion         , ",                         
                      " 'VENTANILLA 2.5'      , ",                         
                      " CAST(A.fec_carga_datamart AS DATE), ",             
                      " CAST(A.fecha_carga_afore AS DATE) , ",             
                      " CAST(A.fecha_carga_afore AS DATE) , ",             
                      " ' '                     ",
               " FROM   ret_det_datamart A, ret_estado C    ",             
               " WHERE  ",pc_busca CLIPPED,                                 
               " AND A.estado = C.estado_solicitud          ",             
               " AND A.id_registro IS NOT NULL              "              

    PREPARE prp_busqueda FROM lc_query
    DECLARE cur_busqueda CURSOR FOR prp_busqueda    

    FOREACH cur_busqueda INTO lr_dtm_consulta.*
        LET li_cont = li_cont + 1
	    IF li_cont >= 5000 THEN
	        ERROR "  Fue Sobrepasada la capacidad maxima del arreglo  "
	        EXIT FOREACH
	    ELSE
	        INSERT INTO tmp_dtm_consulta
	        VALUES (lr_dtm_consulta.*)
	    END IF
    END FOREACH

END FUNCTION 

#---------------------------------------------------------------------------#
# f_consulta_dtm : Consulta la informacion que se cargo en las tablas del   #
#                  datamart                                                 #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_dtm()

    DEFINE lar_dtmart ARRAY[5000] OF RECORD #glo #lar_dtmart
        nss                 LIKE ret_det_datamart.nss               ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        folio               LIKE ret_det_datamart.folio             ,
        curp                LIKE ret_det_datamart.curp              ,
        nombre_datamart     LIKE ret_det_datamart.nombre_datamart   ,
        nombre_afore        LIKE ret_det_datamart.nombre_afore      ,
        paterno_afore       LIKE ret_det_datamart.paterno_afore     ,
        materno_afore       LIKE ret_det_datamart.materno_afore     ,
        tipo_movimiento     LIKE ret_det_datamart.tipo_movimiento   ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        art_negativa        LIKE ret_det_datamart.art_negativa      ,
        frac_negativa       LIKE ret_det_datamart.frac_negativa     ,
        num_considerando    LIKE ret_det_datamart.num_considerando  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        porcentaje_val      LIKE ret_det_datamart.porcentaje_val    ,
        semanas_cotizadas   LIKE ret_det_datamart.semanas_cotizadas ,
        diag_datamart       LIKE ret_det_datamart.diag_datamart     ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        tipo_retiro         LIKE ret_det_datamart.tipo_retiro       ,
        monto_sol_imss      LIKE ret_det_datamart.monto_sol_imss    ,
        estado_lote         LIKE ret_cza_datamart.estado_lote       ,
        desc_estado         LIKE ret_estado.descripcion             ,
        desc_cargado        CHAR(15)                                ,
        fecha_datamart      LIKE ret_cza_datamart.fecha_datamart    ,
        fecha_carga         LIKE ret_cza_datamart.fecha_carga       ,
        fecha_proceso       LIKE ret_cza_datamart.fecha_proceso     ,
        nom_archivo         LIKE ret_cza_datamart.nom_archivo         
    END RECORD

    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    DEFINE
        flag                  SMALLINT

    DEFINE
        txt_1                 CHAR(5000) ,
        x_busca               CHAR(500)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETC8014 AT 4,4 WITH FORM "RETC8014" ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC801            CONSULTA DE RESOLUCIONES DATAMART                          " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    IF gc_param_serv = "CON" THEN
        DISPLAY "RETC802" AT 2,2 ATTRIBUTE(REVERSE)
    END IF

    LET flag = 0

    CONSTRUCT BY NAME x_busca ON A.nss               ,
                                 A.folio             
                                 
        ON KEY (CONTROL-C)
            IF flag = 0 THEN
                LET flag = 1
                EXIT CONSTRUCT
            END IF

        ON KEY (INTERRUPT)
            IF flag = 0 THEN
                LET flag = 1
                EXIT CONSTRUCT
            END IF

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF flag = 1 THEN
        CALL f_lib_error_msg("BUSQUEDA CANCELADA")
        CLEAR SCREEN
        CLOSE WINDOW RETC8014
        RETURN
    END IF

    CALL f_genera_DTM_consulta(x_busca)

    DECLARE cur_6 CURSOR FOR 
        SELECT *
        FROM   tmp_dtm_consulta
        ORDER BY nss

    LET i = 1

    FOREACH cur_6 INTO lar_dtmart[i].*
        LET i = i + 1
	    IF i >= 5000 THEN
	        ERROR "  Fue Sobrepasada la capacidad maxima del arreglo  "
	        EXIT FOREACH
	    END IF
    END FOREACH

    IF i = 1 THEN
        CLEAR FORM
        CALL f_lib_error_msg("NO EXISTEN REGISTROS")
        CLEAR SCREEN
        CLOSE WINDOW RETC8014
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY lar_dtmart TO scr1.*

        ON KEY ( CONTROL-C )
	        EXIT DISPLAY

        ON KEY ( INTERRUPT )
	        EXIT DISPLAY

    END DISPLAY

    CLEAR SCREEN
    CLOSE WINDOW RETC8014

END FUNCTION

#---------------------------------------------------------------------------#
# f_cast_fecha : Realiza el cast para el formato de fecha en la tabla de    #
#                resoluciones de datamart                                   #
#---------------------------------------------------------------------------#
FUNCTION f_cast_fecha(pc_where)

    DEFINE 
        pc_where            CHAR(500)

    DEFINE
        lc_where            CHAR(500)   ,
        lc_nss              CHAR(25)    ,
        lc_fec_dtm          CHAR(40)    ,
        lc_fecha            CHAR(12)

    -- -----------------------------------------------------------------------------

    INITIALIZE lc_fecha TO NULL

    LET lc_fec_dtm = " CAST(fec_carga_datamart AS DATE) = "
    
    IF pc_where[3] = "f" THEN 
        LET lc_fecha = pc_where[21,32]
        LET lc_where = lc_fec_dtm, lc_fecha CLIPPED
    ELSE
        IF pc_where[27] = "f" THEN 
            LET lc_nss      = pc_where[1,24]
            LET lc_fecha    = pc_where[45,57]
            LET lc_where    = lc_nss CLIPPED, lc_fec_dtm, lc_fecha CLIPPED
        END IF 
    END IF
    
    IF lc_fecha IS NULL THEN
        LET lc_where = pc_where
    END IF 

    RETURN lc_where

END FUNCTION


#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del encabezado#
#                      del archivo de datamart                              #
#---------------------------------------------------------------------------#
FUNCTION f_carga_encabezado(pr_encab)

    DEFINE pr_encab RECORD
        registro        CHAR(350)                           ,
        folio           LIKE ret_cza_datamart.folio         ,
        nom_archivo     LIKE ret_cza_datamart.nom_archivo
    END RECORD

    DEFINE lr_fechas RECORD
        datamart    LIKE ret_cza_datamart.fecha_datamart,
        proceso     LIKE ret_cza_datamart.fecha_proceso
    END RECORD

    DEFINE
        c10_fecha   CHAR(10)

    DEFINE
        ldt_hora_act            DATETIME HOUR TO SECOND
        
    -- -------------------------------------------------------------

    LET c10_fecha = pr_encab.registro[21,22],"/",
                    pr_encab.registro[23,24],"/",
                    pr_encab.registro[17,20]

    LET lr_fechas.datamart = c10_fecha

    LET c10_fecha = pr_encab.registro[29,30],"/",
                    pr_encab.registro[31,32],"/",
                    pr_encab.registro[25,28]

    LET lr_fechas.proceso  = c10_fecha

    LET ldt_hora_act       = CURRENT HOUR TO SECOND

    INSERT INTO tmp_cza_datamart
    VALUES(pr_encab.folio       ,
           lr_fechas.datamart   ,
           lr_fechas.proceso    ,
           HOY                  , -- fecha_carga
           ldt_hora_act         , -- hora_carga
           pr_encab.nom_archivo ,
           0                    , -- total registros
           gs_recibido_ret
          )

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de datamart                                         #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE ret_det_datamart.folio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE lr_datamart_tmp  RECORD LIKE ret_det_datamart.*

    DEFINE
        ls_sec_pen          SMALLINT

    DEFINE
        lc_pval             CHAR(007) ,
        lc_fechas           CHAR(010)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_datamart_tmp.* TO NULL

    LET lr_datamart_tmp.folio               = p_folio
    LET lr_datamart_tmp.nss                 = pc_registro[007,017]

    LET lr_datamart_tmp.curp                = pc_registro[018,035]
    LET lr_datamart_tmp.nombre_datamart     = pc_registro[036,085]
    LET lr_datamart_tmp.nombre_afore        = pc_registro[086,125]
    LET lr_datamart_tmp.paterno_afore       = pc_registro[126,165]
    LET lr_datamart_tmp.materno_afore       = pc_registro[166,205]
    LET lr_datamart_tmp.tipo_movimiento     = pc_registro[208,210]
    LET lr_datamart_tmp.regimen             = pc_registro[211,212]
    LET lr_datamart_tmp.tipo_seguro         = pc_registro[213,214]
    LET lr_datamart_tmp.tipo_pension        = pc_registro[215,216]
    LET lr_datamart_tmp.tipo_prestacion     = pc_registro[217,218]
    LET lr_datamart_tmp.art_negativa        = pc_registro[219,221]
    LET lr_datamart_tmp.frac_negativa       = pc_registro[222,223]
    LET lr_datamart_tmp.num_considerando    = pc_registro[224,225]

    -- Formateamos el valor de la secuencia de pension
    LET lr_datamart_tmp.sec_pension         = pc_registro[206,207]

    IF lr_datamart_tmp.sec_pension IS NOT NULL THEN
        LET ls_sec_pen                  = lr_datamart_tmp.sec_pension
        LET lr_datamart_tmp.sec_pension = ls_sec_pen USING "&&"
    END IF

    LET lc_fechas                           = pc_registro[230,231],"/",
                                              pc_registro[232,233],"/",
                                              pc_registro[226,229]
    LET lr_datamart_tmp.fecha_ini_pen       = lc_fechas

    LET lc_fechas                           = pc_registro[238,239],"/",
                                              pc_registro[240,241],"/",
                                              pc_registro[234,237]
    LET lr_datamart_tmp.fecha_resolucion    = lc_fechas

    LET lc_pval                             = pc_registro[242,244],".",
                                              pc_registro[245,246]
    LET lr_datamart_tmp.porcentaje_val      = lc_pval

    LET lr_datamart_tmp.semanas_cotizadas   = pc_registro[247,250]
    LET lr_datamart_tmp.diag_datamart       = pc_registro[251,253]
    LET lr_datamart_tmp.estado_sub_viv      = pc_registro[254,254]

    INSERT INTO tmp_det_datamart VALUES(lr_datamart_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de datamart                                     #
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
    SET    tot_registros = li_tot_regs
    WHERE  folio         = pr_sumario.folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_dtm_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_dtm_temp RECORD LIKE ret_det_datamart.*

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

    IF pr_dtm_temp.tipo_movimiento IS NULL THEN
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

    IF pr_dtm_temp.tipo_prestacion IS NULL THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dtm_temp.fecha_ini_pen IS NULL THEN
        LET lr_error.nom_campo    = "fecha_ini_pen"
        LET lr_error.valor_campo  = pr_dtm_temp.fecha_ini_pen
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

    DEFINE pr_dtm_temp RECORD LIKE ret_det_datamart.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso
--    DEFINE lc_tipo_retiro LIKE ret_matriz_derecho.tipo_retiro

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
    FROM   tab_regimen
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
    FROM   tab_seguro
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
    FROM   tab_pension
    WHERE  tipo_pension = pr_dtm_temp.tipo_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Prestacion
    SELECT "OK"
    FROM   tab_prestacion
    WHERE  tipo_prestacion = pr_dtm_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_dtm_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida que la combinacion exista en la matriz de derechos y que
    -- corresponda a una disposicion
    SELECT "OK"
    FROM   ret_matriz_derecho
    WHERE  regimen          = pr_dtm_temp.regimen
    AND    tipo_seguro      = pr_dtm_temp.tipo_seguro
    AND    tipo_pension     = pr_dtm_temp.tipo_pension
    AND    tipo_prestacion  = pr_dtm_temp.tipo_prestacion
    AND    tipo_retiro NOT IN ("A", "B", "C", "K")
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
# f_valida_precio_acc : Valida que existan los precios de accion para el    #
#                       dia en curso                                        #
#---------------------------------------------------------------------------#
FUNCTION f_valida_precio_acc(pdt_fecha_precios)

    DEFINE
        pdt_fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE
        ls_valida               ,
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie      = 1
    LET ls_valida   = 1


    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTA PRECIO: ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " SIE ", lc_siefore CLIPPED

            CALL f_lib_error_msg(lc_mensaje)
            LET ls_valida = 0
            EXIT PROGRAM 
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF

    END FOREACH

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
    LET lr_bitacora.programa    = "RETC801"
    LET lr_bitacora.nom_archivo = gr_nom.nom_archivo_dtm
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
    DROP TABLE tmp_arch_datamart
    CREATE TEMP TABLE tmp_arch_datamart
    (
    n_registros          CHAR(350)
    )

    #----
    DROP TABLE tmp_det_datamart

    SELECT *
    FROM   ret_det_datamart
    WHERE  1 = 0
    INTO TEMP tmp_det_datamart

    #----
    DROP TABLE tmp_historico_dtm

    SELECT *
    FROM   ret_det_datamart
    WHERE  1 = 0
    INTO TEMP tmp_historico_dtm

    #----
    DROP TABLE tmp_cza_datamart

    SELECT *
    FROM   ret_cza_datamart
    WHERE  1 = 0
    INTO TEMP tmp_cza_datamart

    #----
    
    DROP TABLE tmp_dtm_consulta
    
    CREATE TEMP TABLE tmp_dtm_consulta
    (
        nss                 CHAR(11)        ,
        sec_pension         CHAR(02)        ,
        folio               INTEGER         ,
        curp                CHAR(18)        ,
        nombre_datamart     CHAR(50)        ,
        nombre_afore        CHAR(40)        ,
        paterno_afore       CHAR(40)        ,
        materno_afore       CHAR(40)        ,
        tipo_movimiento     SMALLINT        ,
        regimen             CHAR(02)        ,
        tipo_seguro         CHAR(02)        ,
        tipo_pension        CHAR(02)        ,
        tipo_prestacion     SMALLINT        ,
        art_negativa        CHAR(03)        ,
        frac_negativa       CHAR(02)        ,
        num_considerando    CHAR(02)        ,
        fecha_ini_pen       DATE            ,
        fecha_resolucion    DATE            ,
        porcentaje_val      DECIMAL(5,2)    ,
        semanas_cotizadas   SMALLINT        ,
        diag_datamart       SMALLINT        ,
        estado_sub_viv      SMALLINT        ,
        tipo_retiro         CHAR(1)         ,
        monto_sol_imss      DECIMAL(13,2)   ,
        estado_lote         SMALLINT        ,
        desc_estado         CHAR(20)        ,
        desc_cargado        CHAR(15)        ,                        
        fecha_datamart      DATE            ,
        fecha_carga         DATE            ,
        fecha_proceso       DATE            ,
        nom_archivo         CHAR(20)
    )
    #----
    
    WHENEVER ERROR STOP

END FUNCTION
