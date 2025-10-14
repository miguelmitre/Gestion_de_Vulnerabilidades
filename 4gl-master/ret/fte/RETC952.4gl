#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC952  => RECEPCION DEL ARCHIVO DE OP. 46 DISPOSICIONES ISSSTE      #
#Fecha creacion    => 21 DE SEPTIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 22 DE DICIEMBRE DE 2009                                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => En caso de venir con diagnostico de rechazo, ya no se     #
#                     debe validar el diagnostico de vivienda                   #
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos validos de      #
#                     retiros totales ISSSTE                                    #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper_45          INTEGER ,
        nom_archivo_op46       CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        enviado               LIKE ret_estado_issste.estado_solicitud ,
        recibido              LIKE ret_estado_issste.estado_solicitud ,
        rechazado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        ruta_arch_op46        CHAR(200) ,
        carga_reg             CHAR(360) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(020)

    DEFINE #glo INTEGER
       gi_folio              ,
       gi_proceso            ,
       cuantos               INTEGER

    DEFINE #glo #smallint
        gs_codigo_afore      ,
        gs_cod_inv           ,
        gs_flag_proceso      ,
        gs_flag_err          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC952") #CPL-2431

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC952       RECEPCION ARCHIVOS OP.46 DISPOSICIONES ISSSTE           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "DISPOSICIONES"
        COMMAND "Carga archivo" "Carga Archivo de Operacion 46"
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

    LET HOY = TODAY

    SELECT USER,
           *
    INTO   gc_usuario,
           gs_modulo.*
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

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la op. 46    #
#                  de disposiciones issste                                  #
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
    CLOSE WINDOW retc9521

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

    OPEN WINDOW retc9521 AT 4,4 WITH FORM "RETC9521" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Ejecutar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC952  CARGA ARCHIVO DE OPERACION 46 DISPOSICION ISSSTE                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET cuantos     = 0
    LET ls_procesa  = 0

    SELECT MAX(folio)
    INTO   reg_1.folio_oper_45
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.enviado

    INITIALIZE reg_1.nom_archivo_op46 TO NULL

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS

        AFTER FIELD folio_oper_45
            IF reg_1.folio_oper_45 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF

            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio = reg_1.folio_oper_45
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA DISPOSICION "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  folio =  reg_1.folio_oper_45
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF


        AFTER FIELD nom_archivo_op46
            IF reg_1.nom_archivo_op46 IS NULL THEN
                ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op46
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_1.nom_archivo_op46
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op46
            END IF

        ON KEY (ESC)
            -- Validaciones de folio
            IF reg_1.folio_oper_45 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF

            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio = reg_1.folio_oper_45
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA DISPOSICION "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  folio =  reg_1.folio_oper_45
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_45
            END IF

            -- Validaciones de nom archivo
            IF reg_1.nom_archivo_op46 IS NULL THEN
                ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op46
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_1.nom_archivo_op46
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op46
            END IF

            -- Se realiza la carga del archivo
            LET ruta_arch_op46 = gs_modulo.ruta_rescate CLIPPED,"/",
                                reg_1.nom_archivo_op46 CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM ruta_arch_op46
            INSERT INTO tmp_arch_op46

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   cuantos
            FROM   tmp_arch_op46

            IF cuantos = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo_op46
            ELSE
                WHILE TRUE
                    PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
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

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
    END INPUT

    RETURN ls_procesa, li_id_proc, reg_1.folio_oper_45

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de transferencias  #
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE
        carga_reg           CHAR(600)

    DEFINE pi_folio LIKE ret_sol_issste_tx.folio

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_op46

    FOREACH cur_ar INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "01"
                CALL f_carga_encabezado(carga_reg            ,
                                        pi_folio             ,
                                        reg_1.nom_archivo_op46
                                       )
            WHEN "03"
                CALL f_carga_det(carga_reg, pi_folio)

            WHEN "09"
                CALL f_carga_sumario(carga_reg, pi_folio)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de operacion 46          #
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

    DEFINE lr_dispos RECORD LIKE ret_sol_issste_tx.*

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
    FROM   tmp_disp_issste

    FOREACH cur_tmp INTO lr_dispos.*

        LET lr_error.nss          = lr_dispos.nss
        LET lr_error.curp         = lr_dispos.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre disposiciones
        CALL f_valida_disposicion(ls_flag            ,
                                    lr_error.id_proceso,
                                    lr_dispos.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_detalle_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_dispos.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle de vivienda no sean nulos ---
        CALL f_valida_det_viv_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_dispos.*      )
            RETURNING ls_flag

    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_folio)

    DEFINE lr_tmp_disp RECORD LIKE ret_sol_issste_tx.*
    DEFINE lr_tmp_viv  RECORD LIKE ret_monto_viv_issste.*

    DEFINE lr_etapa_2 RECORD
        nss                 CHAR(11) ,
        folio_serv          INTEGER  ,
        tipo_serv           CHAR(01) ,
        usuario             CHAR(15)  
    END RECORD

    DEFINE
        pi_folio                ,
        lt_tot_rechazo          ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY "                                                                           " AT 9,1
    DISPLAY "                                                                           " AT 11,1
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros = 0
    LET li_tot_detalle   = 0
    LET lt_tot_rechazo   = 0
    
    LET lr_etapa_2.tipo_serv    = "S"
    LET lr_etapa_2.usuario      = gc_usuario


    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_op46

    DISPLAY "FOLIO DE CARGA       : ", pi_folio AT 8,24
    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
    DISPLAY "REGISTROS RECHAZADOS       : ", lt_tot_rechazo AT 12,18

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_cza_lote
    INSERT INTO ret_cza_lote
    SELECT *
    FROM   tmp_cza_lote

    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_disp_issste
    WHERE  folio = pi_folio

    FOREACH cur_ok INTO lr_tmp_disp.*

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9

        IF lr_tmp_disp.estado_solicitud = gr_edo.rechazado THEN

            CALL f_desmarca_cuenta(lr_tmp_disp.nss          ,
                                   lr_tmp_disp.consecutivo  ,
                                   lr_tmp_disp.tipo_retiro  ,
                                   gc_usuario               )

            LET lt_tot_rechazo = lt_tot_rechazo + 1
            DISPLAY "REGISTROS RECHAZADOS       : ", lt_tot_rechazo AT 12,18
        END IF

        SELECT *
        INTO   lr_tmp_viv.*
        FROM   tmp_monto_viv_issste
        WHERE  curp = lr_tmp_disp.curp

        UPDATE ret_monto_viv_issste
        SET    cve_operacion    = lr_tmp_viv.cve_operacion      ,
               estado_sub_viv   = lr_tmp_viv.estado_sub_viv     ,
               acc_viv08_bdsviv = lr_tmp_viv.acc_viv08_bdsviv   ,
               acc_viv92_bdsviv = lr_tmp_viv.acc_viv92_bdsviv
        WHERE  curp             = lr_tmp_disp.curp
        AND    folio            = pi_folio

        -- Si es Invercap, se ejecuta el cierre automatico de la etapa 2
        IF gs_codigo_afore = gs_cod_inv THEN
            SELECT nss                  ,
                   folio_solicitud
            INTO   lr_etapa_2.nss       ,
                   lr_etapa_2.folio_serv
            FROM   ret_sol_issste_tx
            WHERE  curp             = lr_tmp_disp.curp
            AND    folio            = pi_folio
            AND    estado_solicitud = gr_edo.enviado

            CALL sol_etapa_2(lr_etapa_2.*, lr_tmp_disp.diag_procesar)
        END IF


        UPDATE ret_sol_issste_tx
        SET    diag_procesar    = lr_tmp_disp.diag_procesar     ,
               estado_solicitud = lr_tmp_disp.estado_solicitud  ,
               codigo_rechazo   = lr_tmp_disp.codigo_rechazo
        WHERE  curp             = lr_tmp_disp.curp
        AND    folio            = pi_folio
        AND    estado_solicitud = gr_edo.enviado

        SELECT 'OK'
        FROM ret_sol_issste_tx
        WHERE  folio  = pi_folio
        AND    curp   = lr_tmp_disp.curp
        AND    estado_solicitud = lr_tmp_disp.codigo_rechazo
        GROUP BY 1
        
        IF SQLCA.SQLCODE = NOTFOUND THEN 
             CALL ERRORLOG("No se actualizo a rechazado el estado del curp: "||lr_tmp_disp.curp||" para el folio: "||pi_folio)
        END IF  

    END FOREACH

    DISPLAY "                                             " AT 18,1
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el SPL que realiza la desmarca de la cuenta   #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE ret_sol_issste_tx.nss           ,
        consec      LIKE ret_sol_issste_tx.consecutivo   ,
        tipo_ret    LIKE ret_sol_issste_tx.tipo_retiro   ,
        usuario     LIKE cta_act_marca.usuario
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_marca_res    ,
        ls_cod_rech     ,
        ls_movim        SMALLINT

    LET lr_dat.edo_marca    = 40
    LET lr_dat.marca_causa  = 0

    SELECT DISTINCT a.movimiento
    INTO   ls_movim
    FROM   tab_ret_issste a,
           cta_act_marca b
    WHERE  tipo_retiro = pr_desmarca.tipo_ret
    AND    a.movimiento = b.marca_cod
    AND    b.nss = pr_desmarca.nss
    
    

    EXECUTE eje_desmarca USING pr_desmarca.nss         ,--nss
                               ls_movim                ,--marca entrante
                               pr_desmarca.consec      ,--consecutivo
                               lr_dat.edo_marca        ,--estado_marco
                               lr_dat.marca_causa      ,--marca_causa
                               pr_desmarca.usuario      --usuario
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

    OPEN WINDOW retc9522 AT 4,4 WITH FORM "RETC9522" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC952  BITACORA DE ERRORES DE CARGA OP.46 DISPOSICION ISSSTE                " AT 2,1 ATTRIBUTE(REVERSE)
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
            CLOSE WINDOW retc9522
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
                         " AND    programa = 'RETC952' ",
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
            CLOSE WINDOW retc9522

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc9522
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del encabezado#
#                      del archivo de op 42                                 #
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
#               archivo de la operacion 46 de disposiciones                 #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE ret_sol_issste_tx.folio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE lr_disp_tmp  RECORD LIKE ret_sol_issste_tx.*
    DEFINE lr_mto_viv   RECORD LIKE ret_monto_viv_issste.*

    DEFINE
        ls_edo_recep        ,
        ls_aceptado         SMALLINT

    DEFINE
        lc_det_viv          CHAR(450),
        lc_mto_viv          CHAR(15),
        lc_mto_solic        CHAR(16),
        lc_prcje            CHAR(06),
        lc_fechas           CHAR(10)

    LET lr_disp_tmp.folio               = p_folio
    LET lr_disp_tmp.consecutivo         = pc_registro[236,246]
    LET lr_disp_tmp.curp                = pc_registro[018,035]
    LET lr_disp_tmp.nss                 = pc_registro[007,017]

    -- En caso de no viajar en el archivo, obtenemos el nss de la tabla
    -- de disposiciones. Esto es necesario ya que el nss se usara para
    -- realizar la desmarca en caso de algun rechazo
    IF lr_disp_tmp.nss IS NULL OR lr_disp_tmp.nss = "           " THEN
        SELECT nss 
        INTO   lr_disp_tmp.nss
        FROM   ret_sol_issste_tx
        WHERE  curp     = lr_disp_tmp.curp
        AND    folio    = p_folio
    END IF

    LET lr_disp_tmp.nombre_afore        = pc_registro[036,075]
    LET lr_disp_tmp.paterno_afore       = pc_registro[076,115]
    LET lr_disp_tmp.materno_afore       = pc_registro[116,155]
    LET lr_disp_tmp.sec_pension         = pc_registro[156,157]
    LET lr_disp_tmp.tipo_retiro         = pc_registro[158,158]

    LET lr_disp_tmp.regimen             = pc_registro[159,160]
    LET lr_disp_tmp.tipo_seguro         = pc_registro[161,162]
    LET lr_disp_tmp.tipo_pension        = pc_registro[163,164]
    LET lr_disp_tmp.cve_pension         = pc_registro[165,167]

    IF lr_disp_tmp.cve_pension = " NA" OR lr_disp_tmp.cve_pension = "   " THEN
        LET lr_disp_tmp.cve_pension = "NA"
    END IF

    LET lr_disp_tmp.tipo_prestacion     = pc_registro[168,169]

    LET lc_fechas                       = pc_registro[174,175],"/",
                                          pc_registro[176,177],"/",
                                          pc_registro[170,173]
    LET lr_disp_tmp.fecha_ini_pen       = lc_fechas

    LET lc_fechas                       = pc_registro[182,183],"/",
                                          pc_registro[184,185],"/",
                                          pc_registro[178,181]
    LET lr_disp_tmp.fecha_resolucion    = lc_fechas

    LET lr_disp_tmp.semanas_cotizadas   = pc_registro[191,194]

    LET lc_fechas                       = pc_registro[199,200],"/",
                                          pc_registro[201,202],"/",
                                          pc_registro[195,198]
    LET lr_disp_tmp.fecha_solicitud     = lc_fechas

    LET lr_disp_tmp.cve_doc_probatorio  = pc_registro[203,203]

    LET lc_fechas                       = pc_registro[208,209],"/",
                                          pc_registro[210,211],"/",
                                          pc_registro[204,207]
    LET lr_disp_tmp.fecha_nacimiento    = lc_fechas

    LET lr_disp_tmp.aseguradora         = pc_registro[212,214]
    LET lr_disp_tmp.actuario            = pc_registro[215,221]
    LET lr_disp_tmp.num_plan_privado    = pc_registro[222,229]
    LET lr_disp_tmp.periodo_pago        = pc_registro[230,235]
    LET lr_disp_tmp.diag_procesar       = pc_registro[337,339]

    -- Verificamos si el diagnostico recibido se acepta o no
    SELECT id_aceptado
    INTO   ls_aceptado
    FROM   tab_diag_procesar_disp
    WHERE  diag_procesar    = lr_disp_tmp.diag_procesar

    IF (STATUS = NOTFOUND) OR (ls_aceptado = 0) THEN
        LET ls_edo_recep  = gr_edo.rechazado
    ELSE
        LET ls_edo_recep  = gr_edo.recibido
    END IF

    LET lr_disp_tmp.codigo_rechazo      = pc_registro[342,344]
    LET lr_disp_tmp.grupo               = 0
    LET lr_disp_tmp.estado_solicitud    = ls_edo_recep

    -- Carga detalle para vivienda
    SELECT  *
    INTO    lc_det_viv
    FROM    tmp_arch_op46
    WHERE   n_registros[014,031] = lr_disp_tmp.curp
    AND     n_registros[001,002] = "05"

    LET lr_mto_viv.curp             = lr_disp_tmp.curp
    LET lr_mto_viv.consecutivo      = lr_disp_tmp.consecutivo
    LET lr_mto_viv.folio            = p_folio
    LET lr_mto_viv.tipo_retiro      = lr_disp_tmp.tipo_retiro
    LET lr_mto_viv.cve_operacion    = 46
    LET lr_mto_viv.fecha_valor_viv  = NULL
    LET lr_mto_viv.acciones_viv08   = 0
    LET lr_mto_viv.acciones_viv92   = 0

    LET lr_mto_viv.estado_sub_viv   = lc_det_viv[082,082]

    IF lc_det_viv[082,082] = " " THEN
        LET lr_mto_viv.estado_sub_viv = 0
    END IF

    LET lc_mto_viv                  = lc_det_viv[083,090],".",
                                      lc_det_viv[091,096]
    LET lr_mto_viv.acc_viv08_bdsviv = lc_mto_viv


    LET lc_mto_viv                  = lc_det_viv[097,104],".",
                                      lc_det_viv[105,110]
    LET lr_mto_viv.acc_viv92_bdsviv = lc_mto_viv


    INSERT INTO tmp_disp_issste VALUES(lr_disp_tmp.*)
    INSERT INTO tmp_monto_viv_issste VALUES(lr_mto_viv.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de la operacion 46 de disposiciones             #
#---------------------------------------------------------------------------#
FUNCTION f_carga_sumario(pr_sumario)

    DEFINE pr_sumario RECORD
        registro        CHAR(350)                   ,
        folio           LIKE ret_cza_lote.folio
    END RECORD

    DEFINE li_tot_regs LIKE ret_cza_lote.tot_registros

    LET li_tot_regs = pr_sumario.registro[23,28]

    UPDATE tmp_cza_lote
    SET    tot_registros = li_tot_regs
    WHERE  folio         = pr_sumario.folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_archivo : Realiza las validaciones del lote de la operacion 46   #
#                    de disposiciones                                       #
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

    -- Valida que la operacion sea 46 - Detalle de Disposiciones
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op46
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> "46"

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op46
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
    FROM   tmp_arch_op46
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
    FROM   tmp_arch_op46
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[18,35]
    FROM   tmp_arch_op46
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_curp

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op46
        WHERE  n_registros[1,2]   = "03"
        AND    n_registros[18,35] = lc_curp

        IF li_cont > 1 THEN
            LET lr_error.nom_campo   = "det 03 duplicado"
            LET ls_flag              = 1
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_curp

            CALL f_inserta_bitacora(lr_error.*)
            RETURN ls_flag, lr_error.id_proceso
        END IF

        --- Valida detalle 05 ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op46
        WHERE  n_registros[1,2]   = "05"
        AND    n_registros[14,31] = lc_curp

        IF li_cont > 1 THEN
            --- Registro 05 duplicado ---
            LET lr_error.nom_campo   = "det 05 duplicado"
            LET ls_flag              = 1
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_curp

            CALL f_inserta_bitacora(lr_error.*)
            RETURN ls_flag, lr_error.id_proceso
        ELSE
            IF li_cont = 0 OR li_cont IS NULL THEN
                --- Falta detalle 05 del registro 03 ---
                LET lr_error.nom_campo   = "reg. 03 sin detalle 05"
                LET ls_flag              = 1
                LET lr_error.id_error    = 11
                LET lr_error.valor_campo = lc_curp

                CALL f_inserta_bitacora(lr_error.*)
                RETURN ls_flag, lr_error.id_proceso
            END IF
        END IF

    END FOREACH

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_disposicion : Realiza las validaciones para el proceso de        #
#                        disposiciones                                      #
#---------------------------------------------------------------------------#
FUNCTION f_valida_disposicion(ps_error, pi_id_error, pr_dispo_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_dispo_temp RECORD LIKE ret_sol_issste_tx.*

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

    DEFINE
        lc_max_sec      CHAR(002)

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_dispo_temp.nss
    LET lr_error.curp         = pr_dispo_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que CURP no sea nulo o que venga a 18 posiciones ---
    IF (pr_dispo_temp.curp IS NULL) OR (LENGTH(pr_dispo_temp.curp) <> 18) THEN
        LET lr_error.nom_campo    = "curp"
        LET lr_error.valor_campo  = pr_dispo_temp.curp
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    --- Valida que el tipo de retiro corresponda a transferencias ---
    IF pr_dispo_temp.tipo_retiro <> "A" AND
       pr_dispo_temp.tipo_retiro <> "B" AND
       pr_dispo_temp.tipo_retiro <> "C" AND
       pr_dispo_temp.tipo_retiro <> "D" AND
       pr_dispo_temp.tipo_retiro <> "E" AND
       pr_dispo_temp.tipo_retiro <> "G" AND
       pr_dispo_temp.tipo_retiro <> "I" AND
       pr_dispo_temp.tipo_retiro <> "K" AND
       pr_dispo_temp.tipo_retiro <> "M" THEN 

        LET lr_error.id_error     = 11
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = "Tipo Retiro no valido para Disp :", pr_dispo_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)  
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_dispo_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_dispo_temp RECORD LIKE ret_sol_issste_tx.*

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
    LET lr_error.nss          = pr_dispo_temp.nss
    LET lr_error.curp         = pr_dispo_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_dispo_temp.nombre_afore IS NULL OR
       pr_dispo_temp.nombre_afore = "                                        " THEN
        LET lr_error.nom_campo    = "nombre_afore"
        LET lr_error.valor_campo  = pr_dispo_temp.nombre_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dispo_temp.paterno_afore IS NULL OR
       pr_dispo_temp.paterno_afore = "                                        " THEN
        LET lr_error.nom_campo    = "paterno_afore"
        LET lr_error.valor_campo  = pr_dispo_temp.paterno_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dispo_temp.tipo_retiro IS NULL OR pr_dispo_temp.tipo_retiro = "  " THEN
        LET lr_error.nom_campo    = "tipo_retiro"
        LET lr_error.valor_campo  = pr_dispo_temp.tipo_retiro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_dispo_temp.diag_procesar IS NULL OR pr_dispo_temp.diag_procesar = 0 THEN
        LET lr_error.nom_campo    = "diag_procesar"
        LET lr_error.valor_campo  = pr_dispo_temp.diag_procesar
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_viv_nulo : Valida que los campos del detalle de vivienda 05  #
#                         contengan informacion                             #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_viv_nulo(ps_error, pi_id_error, pr_dispo_temp)

    DEFINE
        ps_error        ,
        ls_aceptado     SMALLINT

    DEFINE pr_dispo_temp RECORD LIKE ret_sol_issste_tx.*

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE lr_mto_viv RECORD LIKE ret_monto_viv_issste.*

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

    SELECT *
    INTO   lr_mto_viv.*
    FROM   tmp_monto_viv_issste
    WHERE  curp         = pr_dispo_temp.curp
    AND    consecutivo  = pr_dispo_temp.consecutivo

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_dispo_temp.nss
    LET lr_error.curp         = pr_dispo_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    -- Verificamos si el diagnostico recibido no es un rechazo
    SELECT id_aceptado
    INTO   ls_aceptado
    FROM   tab_diag_procesar_disp
    WHERE  diag_procesar    = pr_dispo_temp.diag_procesar

    IF ls_aceptado THEN
        IF lr_mto_viv.estado_sub_viv IS NULL OR lr_mto_viv.estado_sub_viv = 0 THEN
            LET lr_error.nom_campo    = "estado_sub_viv"
            LET lr_error.valor_campo  = lr_mto_viv.estado_sub_viv
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
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
    LET lr_bitacora.programa    = "RETC952"
    LET lr_bitacora.nom_archivo = reg_1.nom_archivo_op46
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

    DROP TABLE tmp_arch_op46

    CREATE TEMP TABLE tmp_arch_op46
    (
    n_registros          CHAR(450)
    )

    DROP TABLE tmp_disp_issste

    SELECT *
    FROM   ret_sol_issste_tx
    WHERE  1 = 0
    INTO TEMP tmp_disp_issste

    DROP TABLE tmp_cza_lote

    SELECT *
    FROM   ret_cza_lote
    WHERE  1 = 0
    INTO TEMP tmp_cza_lote

    DROP TABLE tmp_monto_viv_issste

    SELECT *
    FROM   ret_monto_viv_issste
    WHERE  1 = 0
    INTO TEMP tmp_monto_viv_issste

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# sol_etapa_2 : Ejecuta el cierre automatico de la etapa 2 para el modulo   #
#               de servicios                                                #
#---------------------------------------------------------------------------#
FUNCTION sol_etapa_2( l_nss_afi         ,
                      l_fol_serv        ,
                      l_tipo_serv       ,
                      l_usuario         ,
                      pc_diag_registro  )

#---          Definición de Variables que cacha la Funcion     ---
DEFINE  l_nss_afi               CHAR(11)
DEFINE  l_fol_serv              INTEGER
DEFINE  l_tipo_serv             CHAR(01)
DEFINE  l_usuario               CHAR(10)             

DEFINE  pc_diag_registro LIKE ret_solicitud_tx.diag_registro

#---  Fin de las Definiciones de Variables que cacha la Funcion ---                                                        
                                                    
DEFINE  l_verif_txt_eta_sol            ,        
        l_procede                      ,
        l_encontro                     ,
        l_consec_sol                   ,
        l_etapa_cod                    ,
        l_resol_linea           SMALLINT
        
DEFINE  l_leyenda_eta2 LIKE rec_txt_etapa_sol.resol_eta       

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

DEFINE lr_diagnostico RECORD LIKE tab_diag_procesar_disp.*
        
        
    
   #Valores Fijos
   
   LET      l_consec_sol           =         2
   LET      l_etapa_cod            =         2
   LET      l_resol_linea          =         1
   LET      l_encontro             =         0
   LET      l_procede              =         0
                           
   -- Se verifica el diagnostico para ver si es aceptado o no
   SELECT *
   INTO   lr_diagnostico.*
   FROM   tab_diag_procesar_disp
   WHERE  diag_procesar    = pc_diag_registro

    IF lr_diagnostico.id_aceptado = 1 THEN
        LET l_leyenda_eta2 = "SOLICITUD DE RETIRO LIQUIDADO" 
    ELSE
        LET l_leyenda_eta2 = "SOL RECHAZADA DIAG ", 
                             lr_diagnostico.diag_procesar USING "&&&" ,
                             " - " ,
                             lr_diagnostico.descripcion CLIPPED
    END IF
 
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
