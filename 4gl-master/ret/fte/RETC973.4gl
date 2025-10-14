################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC973  => RECEPCION DEL ARCHIVO DE RESPUESTA DE LA OPERACION 56    #
#                     RETIROS PARCIALES ISSSTE                                 #
#Fecha creacion    => 12 DE NOVIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper_56          INTEGER ,
        nom_archivo_op56       CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        recibido              LIKE ret_estado_issste.estado_solicitud ,
        liquidado             LIKE ret_estado_issste.estado_solicitud ,
        rechazado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        ruta_arch_op56        CHAR(200) ,
        carga_reg             CHAR(360) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(020)

    DEFINE #glo INTEGER
       gi_folio              ,
       gi_proceso            ,
       gi_rechazos           ,
       cuantos               INTEGER

    DEFINE #glo #smallint
        gs_flag_proceso      ,
        gs_flag_err          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC973.log")

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC973 RECEPCION ARCHIVOS RESPUESTA OP.56 RET PARCIAL ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CARGA ARCHIVO"
        COMMAND "Inicia Carga" "Carga Archivo de Operacion 56"
            CALL f_genera_carga()
            
        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Carga"
            CALL f_bitacora_err(0)
            CLEAR SCREEN

        COMMAND "Regresar" "Regresa al menu principal "
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

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

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
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la op. 56    #
#                  de retiros parciales                                     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    CALL f_carga_archivo() RETURNING gs_flag_proceso  ,
                                     gi_proceso       ,
                                     gi_folio
    
    IF gs_flag_proceso = 1 THEN #-- La carga se realizo sin errores
    
        CALL primer_paso(gi_folio)  #-- Vacia la informacion del archivo a las tablas de validacion
            RETURNING gi_rechazos
        
        CALL segundo_paso(gi_folio, gi_rechazos)  #-- Despliega informacion en pantalla
    ELSE
        IF gi_proceso <> 0 THEN
            DISPLAY "                                             " AT 18,1
            PROMPT " ARCHIVO CON INCONSISTENCIAS DE ESTRUCTURA ... <ENTER> PARA MOSTRAR" FOR CHAR enter
            CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
        END IF
    
    END IF
    
    CLEAR SCREEN
    CLOSE WINDOW retc9731

END FUNCTION


#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de     #
#                   rescate si existe. En caso de existir, lo carga sin     #
#                   formato en la tabla temporal                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE
        ls_flag             ,
        ls_procesa          SMALLINT

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    OPEN WINDOW retc9731 AT 4,4 WITH FORM "RETC9731" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Ejecutar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC973 CARGA ARCHIVO DE RESPUESTA OP 56 RET PARCIAL ISSSTE                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET cuantos     = 0
    LET ls_procesa  = 0

    SELECT MAX(folio)
    INTO   reg_1.folio_oper_56
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.liquidado

    INITIALIZE reg_1.nom_archivo_op56 TO NULL

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS

        AFTER FIELD folio_oper_56
            IF reg_1.folio_oper_56 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio = reg_1.folio_oper_56
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UN RETIRO PARCIAL "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

        AFTER FIELD nom_archivo_op56
            IF reg_1.nom_archivo_op56 IS NULL THEN
                ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op56
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_1.nom_archivo_op56
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op56
            END IF

        ON KEY (ESC)
            -- Validaciones de folio
            IF reg_1.folio_oper_56 IS NULL THEN
                ERROR " FOLIO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio = reg_1.folio_oper_56
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UN RETIRO PARCIAL "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

            -- Validaciones de nom archivo
            IF reg_1.nom_archivo_op56 IS NULL THEN
                ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op56
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_1.nom_archivo_op56
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " EL ARCHIVO YA CARGADO CON ANTERIORIDAD  "
                SLEEP 2
                ERROR ""
                NEXT FIELD nom_archivo_op56
            END IF

            -- Se realiza la carga del archivo
            LET ruta_arch_op56 = gs_modulo.ruta_rescate CLIPPED,"/",
                                reg_1.nom_archivo_op56 CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM ruta_arch_op56
            INSERT INTO tmp_arch_op56

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   cuantos
            FROM   tmp_arch_op56

            IF cuantos = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo_op56
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

    RETURN ls_procesa, li_id_proc, reg_1.folio_oper_56

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de transferencias a#
#               las tablas temporales donde se realizara su validacion      #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE
        carga_reg           CHAR(600)

    DEFINE 
        li_num_rechazo      INTEGER

    DEFINE pi_folio LIKE ret_parcial_issste.folio

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_op56

    FOREACH cur_ar INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "01"
                CALL f_carga_encabezado(carga_reg            ,
                                        pi_folio             ,
                                        reg_1.nom_archivo_op56
                                       )
            WHEN "03"
                CALL f_carga_det(carga_reg, pi_folio)
                    RETURNING li_num_rechazo

            WHEN "09"
                CALL f_carga_sumario(carga_reg, pi_folio)
        END CASE
    END FOREACH

    RETURN li_num_rechazo

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Despliega en pantalla la informacion de la carga de archivo#
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio, pi_tot_rechazo)

    DEFINE lr_tmp_par RECORD LIKE ret_parcial_issste.*

    DEFINE
        pi_folio                ,
        pi_tot_rechazo          ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER

    DISPLAY "                                                                           " AT 9,1
    DISPLAY "                                                                           " AT 11,1
    DISPLAY "                                             " AT 18,1

    LET li_tot_registros = 0
    LET li_tot_detalle   = 0

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_op56
    
    LET li_tot_detalle = ((li_tot_registros - 2)/2)

    DISPLAY "FOLIO DE CARGA       : ", pi_folio AT 8,24
    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
    DISPLAY "REGISTROS RECHAZADOS       : ", pi_tot_rechazo AT 12,18

    DISPLAY "                                             " AT 18,1
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

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

    OPEN WINDOW retc9732 AT 4,4 WITH FORM "RETC9732" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC973 BITACORA DE ERRORES DE CARGA OP.56 RET PARCIAL ISSSTE                 " AT 2,1 ATTRIBUTE(REVERSE)
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
            CLOSE WINDOW retc9732
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
                         " AND    programa = 'RETC973' ",
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
            CLOSE WINDOW retc9732

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc9732
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_encabezado : Carga en la tabla temporal los valores del           #
#                      encabezado del archivo de op 56                      #
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

    INSERT INTO tmp_cza_lote
    VALUES(pr_encab.folio       ,
           lr_fechas.operacion  ,
           HOY                  ,
           pr_encab.nom_archivo ,
           HOY                  , -- fecha_carga
           0                    , -- total registros
           gr_edo.recibido
          )

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de la operacion 56                                  #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE ret_parcial_issste.folio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE
        lc_resultado        CHAR(002)

    DEFINE lr_parcial_tmp  RECORD LIKE ret_parcial_issste.*

    DEFINE
        ls_edo_recep        ,
        ls_aceptado         SMALLINT

    DEFINE
        li_num_rechazo      INTEGER

    DEFINE
        lc_det_viv          CHAR(450),
        lc_mto_viv          CHAR(15),
        lc_mto_solic        CHAR(16),
        lc_prcje            CHAR(06),
        lc_fechas           CHAR(10)

    LET li_num_rechazo              = 0
    LET lr_parcial_tmp.folio        = p_folio
    LET lr_parcial_tmp.curp         = pc_registro[026,043]

    SELECT consecutivo
    INTO   lr_parcial_tmp.consecutivo
    FROM   ret_parcial_issste
    WHERE  folio    = p_folio
    AND    curp     = lr_parcial_tmp.curp

    LET lr_parcial_tmp.num_issste          = pc_registro[007,014]
    LET lr_parcial_tmp.nss                 = pc_registro[015,025]

    LET lr_parcial_tmp.diag_op56           = pc_registro[211,213]
    LET lc_resultado                       = pc_registro[590,591]


    IF lc_resultado = "02" THEN
        LET lr_parcial_tmp.codigo_rechazo = pc_registro[592,594]
        LET li_num_rechazo = li_num_rechazo + 1
    ELSE
        LET lr_parcial_tmp.codigo_rechazo = NULL
    END IF

    UPDATE ret_parcial_issste
    SET    diag_op56        = lr_parcial_tmp.diag_op56,
           codigo_rechazo   = lr_parcial_tmp.codigo_rechazo 
    WHERE  folio            = lr_parcial_tmp.folio
    AND    curp             = lr_parcial_tmp.curp
    AND    consecutivo      = lr_parcial_tmp.consecutivo
    
    RETURN li_num_rechazo

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_sumario : Carga en la tabla temporal los valores del sumario del  #
#                   archivo de la operacion 53                              #
#---------------------------------------------------------------------------#
FUNCTION f_carga_sumario(pr_sumario)

    DEFINE pr_sumario RECORD
        registro        CHAR(350)                   ,
        folio           LIKE ret_cza_lote.folio
    END RECORD

    DEFINE li_tot_regs LIKE ret_cza_lote.tot_registros

    LET li_tot_regs = pr_sumario.registro[28,33]

    UPDATE tmp_cza_lote
    SET    tot_registros = li_tot_regs
    WHERE  folio         = pr_sumario.folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_archivo : Realiza las validaciones del formato del archivo       #
#                    de la op. 56 cargado                                   #
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

    -- Valida que la operacion sea 56 - Detalle de Transacciones Parciales
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op56
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> "56"

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op56
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
    FROM   tmp_arch_op56
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
    FROM   tmp_arch_op56
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[26,43]
    FROM   tmp_arch_op56
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_curp

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op56
        WHERE  n_registros[1,2]   = "03"
        AND    n_registros[26,43] = lc_curp

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
# f_ultimo_id_err : Obtiene el ultimo identificador de proceso para la      #
#                   bitacora de errores                                     #
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
# f_inserta_bitacora : Inserta registro en la bitacora de errores de carga  #
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
    LET lr_bitacora.programa    = "RETC973"
    LET lr_bitacora.nom_archivo = reg_1.nom_archivo_op56
    LET lr_bitacora.folio       = gi_folio
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
# f_tablas_tmp :  Crea las tablas temporales que se usaran en el proceso    #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE

    DROP TABLE tmp_arch_op56

    CREATE TEMP TABLE tmp_arch_op56
    (
    n_registros          CHAR(700)
    )

    DROP TABLE tmp_par_issste

    SELECT *
    FROM   ret_parcial_issste
    WHERE  1 = 0
    INTO TEMP tmp_par_issste

    DROP TABLE tmp_cza_lote

    SELECT *
    FROM   ret_cza_lote
    WHERE  1 = 0
    INTO TEMP tmp_cza_lote

    WHENEVER ERROR STOP

END FUNCTION


