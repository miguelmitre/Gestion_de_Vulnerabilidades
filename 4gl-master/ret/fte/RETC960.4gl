#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC960  => RECEPCION Y CONSULTA DEL ARCHIVO DE TRANSFERENCIAS        #
#                     ISSSTE (OP. 42)                                           #
#Fecha creacion    => 21 DE SEPTIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 11 DE FEBRERO DE 2010                                     #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Se agrego validacion para dejar pasar una combinacion     #
#                     repetida de CURP + Secuencia de pension en caso de que    #
#                     la transferencia haya sido rechazada previamente          #
#Fecha actualiz.   => 17 DE MAYO DE 2010                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Se incluye el tipo de retiro N en la lista de retiros     #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 21 DE JUNIO DE 2010                                       #
#                  => Se modifica para permitir la carga del campo de clave de  #
#                     aseguradora                                               #
# Fecha actualiz.   => 11 DE MAYO DE 2011                                       #
#                   => Se realizan las modificaciones al codigo para incluir    #
#                      el llamado de las funciones para realizar el marcaje de  #
#                      trabajador pensionado (REQ. EFPS-157 )                   #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE reg_1 RECORD #glo #reg_1
        nom_archivo_op42       CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        capturado LIKE ret_estado_issste.estado_solicitud,
        recibido  LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        ruta_arch_op42        CHAR(200) ,
        carga_reg             CHAR(360) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(020)

    DEFINE #glo #integer
       gi_proceso            INTEGER

    DEFINE #glo #smallint
        gs_peiss             ,
        gs_cod_afore         ,
        gs_flag_proceso      ,
        gs_flag_err          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC960.log")

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC960   RECEPCION Y CONSULTA - ARCHIVOS OP.42 TRANSFERENCIAS ISSSTE  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "TRANSFERENCIAS"
        COMMAND "Carga archivo" "Carga Archivo de Operacion 42"
            CALL f_genera_carga()

        COMMAND "Consulta" "Consulta de Registros de Transferencia"
            CALL f_consulta_trans()

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

    LET HOY = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECIBIDO"

    ----- MARCAJE -----
    LET lc_prepare = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM lc_prepare

    LET lc_prepare = " "

   ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE exe_desmarca FROM lc_prepare

    LET lc_prepare = " "
    
    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "#CPL-2105
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la op. 42    #
#                  de transferencias ISSSTE                                 #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    CALL carga_archivo() RETURNING gs_flag_proceso, gi_proceso

    IF gs_flag_proceso = 1 THEN #-- La carga se realizo sin errores

        CALL primer_paso()  #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso() #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err, gi_proceso

        IF gs_flag_err = 0 THEN

            IF gs_cod_afore <> gs_peiss THEN
                CALL tercer_paso() #-- Copia la informacion de transferencias al Datamart
            END IF

            CALL cuarto_paso() #-- Vacia la informacion hacia las tablas fisicas

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
    CLOSE WINDOW retc9601

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

    DEFINE
        li_num_reg          INTEGER

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    OPEN WINDOW retc9601 AT 4,4 WITH FORM "RETC9601" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC960  CARGA ARCHIVO DE OPERACION 42 TRANSFERENCIAS ISSSTE                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET li_num_reg  = 0
    LET ls_procesa  = 0

    INPUT BY NAME reg_1.nom_archivo_op42 WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo_op42
            LET reg_1.nom_archivo_op42 = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo_op42
            IF reg_1.nom_archivo_op42 IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO  "
                NEXT FIELD nom_archivo_op42
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_1.nom_archivo_op42
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " ARCHIVO YA PROCESADO CON ANTERIORIDAD  "
                NEXT FIELD nom_archivo_op42
            END IF

            LET ruta_arch_op42 = gs_modulo.ruta_rescate CLIPPED,"/",
                                 reg_1.nom_archivo_op42 CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM ruta_arch_op42
            INSERT INTO tmp_arch_op42

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_num_reg
            FROM   tmp_arch_op42

            IF li_num_reg = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo_op42
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

                            EXIT INPUT
                        ELSE
                            PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                            EXIT WHILE
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

    RETURN ls_procesa, li_id_proc

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de transferencias  #
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE
        carga_reg           CHAR(600)

    DEFINE li_folio LIKE ret_trans_issste.folio


    LET li_folio = 0

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_op42

    FOREACH cur_ar INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "01"
                CALL f_carga_encabezado(carga_reg            ,
                                        li_folio             ,
                                        reg_1.nom_archivo_op42
                                       )
            WHEN "03"
                CALL f_carga_det(carga_reg, li_folio)

            WHEN "09"
                CALL f_carga_sumario(carga_reg, li_folio)
        END CASE
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de transferencias        #
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

    DEFINE lr_transfer RECORD LIKE ret_trans_issste.*

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

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    #-- Validaciones del detalle
    DECLARE cur_tmp CURSOR FOR
    SELECT *
    FROM   tmp_trans_issste

    FOREACH cur_tmp INTO lr_transfer.*

        LET lr_error.nss          = lr_transfer.nss
        LET lr_error.curp         = lr_transfer.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre transferencias
        CALL f_valida_transferencia(ls_flag            ,
                                    lr_error.id_proceso,
                                    lr_transfer.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_detalle_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_transfer.*      )
            RETURNING ls_flag

        --- Valida que los campos del detalle de vivienda no sean nulos ---
        CALL f_valida_det_viv_nulo(ls_flag            ,
                                   lr_error.id_proceso,
                                   lr_transfer.*      )
            RETURNING ls_flag


        --- Valida que los campos existan contra el catalogo ---
        CALL f_valida_detalle_catalogo(ls_flag            ,
                                       lr_error.id_proceso,
                                       lr_transfer.*      )
            RETURNING ls_flag

    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion ya validada en la tabla de datamart   #
#               temporal (Solo Afores)                                      #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE lr_tmp_datamart RECORD LIKE ret_datamart_issste.*
    DEFINE lr_tmp_transfer RECORD LIKE ret_trans_issste.*

    DEFINE
        ls_resp                 ,
        ls_cod_tramite          SMALLINT

    DEFINE
        li_folio                INTEGER

    -- -----------------------------------------------------------------------------

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA ISSSTE"

    LET li_folio    = 0

    DECLARE cur_dtm CURSOR FOR
        SELECT folio                ,
               nss                  ,
               num_issste           ,
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
               101                  ,
               50
        FROM   tmp_trans_issste
        WHERE  folio = li_folio

    FOREACH cur_dtm INTO lr_tmp_datamart.*

        SELECT "OK"
        FROM   ret_datamart_issste
        WHERE  curp        = lr_tmp_datamart.curp
        AND    sec_pension = lr_tmp_datamart.sec_pension
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            LET ls_resp     = 0

            INSERT INTO tmp_datamart
            VALUES (lr_tmp_datamart.*)

            EXECUTE eje_marca_pen USING lr_tmp_datamart.nss         ,
                                        lr_tmp_datamart.curp        ,  #CPL-2105
                                        lr_tmp_datamart.folio       ,
                                        ls_resp                     ,
                                        lr_tmp_datamart.tipo_retiro ,
                                        ls_cod_tramite              ,
                                        HOY
                                  INTO  ls_resp

        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE lr_tmp_transfer RECORD LIKE ret_trans_issste.*
    DEFINE lr_tmp_cza RECORD LIKE ret_cza_lote.*

    DEFINE
        li_folio                ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER,
        ls_marca                INTEGER,
        ls_consec_previo         INTEGER,
        ls_cero                  SMALLINT
        
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros = 0
    LET li_tot_detalle   = 0
    LET ls_marca         = 931  
    LET ls_cero          = 0     
    LET li_folio         = f_ultimo_folio()

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_op42

    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
    DISPLAY "FOLIO DE CARGA       : ", li_folio AT 13,24

    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_cza_lote
    UPDATE tmp_cza_lote
    SET    folio = li_folio

    INSERT INTO ret_cza_lote
    SELECT *
    FROM   tmp_cza_lote

    -- Insertamos en ret_monto_viv_issste
    UPDATE tmp_monto_viv_issste
    SET    folio = li_folio

    INSERT INTO ret_monto_viv_issste
    SELECT *
    FROM   tmp_monto_viv_issste

    -- Insertamos en ret_datamart_issste
    IF gs_cod_afore <> gs_peiss THEN
        UPDATE tmp_datamart
        SET    folio = li_folio

        INSERT INTO ret_datamart_issste
        SELECT *
        FROM   tmp_datamart
    END IF

    -- Insertamos en ret_trans_issste
    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_trans_issste

    FOREACH cur_ok INTO lr_tmp_transfer.*

        LET lr_tmp_transfer.folio = li_folio

        INSERT INTO ret_trans_issste
        VALUES (lr_tmp_transfer.*)

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",li_tot_detalle AT 11,9
      
         SELECT 'ok'
         FROM    cta_act_marca
         WHERE   nss = lr_tmp_transfer.nss
         AND     marca_cod = ls_marca
         
         IF SQLCA.SQLCODE = 0 THEN 
             SELECT correlativo                       
             INTO ls_consec_previo                      
             FROM   cta_act_marca                     
             WHERE  nss = lr_tmp_transfer.nss                      
             AND    marca_cod = ls_marca;             
                                                      
             EXECUTE exe_desmarca USING  lr_tmp_transfer.nss,     -- nss                
                                         ls_marca,                -- marca_cod          
                                         ls_consec_previo,        --            
                                         ls_cero,                 -- pestado_marca = 0  
                                         ls_marca,                -- marca causa        
                                         gc_usuario             -- usuario que marca 
         END IF 
         
        -- Se marca la cuenta
        CALL f_marca_cuenta(lr_tmp_transfer.nss         ,
                            lr_tmp_transfer.consecutivo ,
                            lr_tmp_transfer.tipo_retiro ,
                            gc_usuario)

    END FOREACH

    DISPLAY "                                             " AT 18,1
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_consec : Obtiene el ultimo consecutivo de retiros                #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_consec()

    DEFINE ld_ult_consec LIKE ret_consecutivo.consecutivo

    SELECT MAX(consecutivo) + 1
    INTO   ld_ult_consec
    FROM   ret_consecutivo

    INSERT INTO ret_consecutivo
    VALUES (ld_ult_consec)

    RETURN ld_ult_consec


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
        FROM   tab_ret_issste
        WHERE  tipo_retiro  = pr_marca.tipo_ret
        AND    cod_tramite  = 10

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

    OPEN WINDOW retc9603 AT 4,4 WITH FORM "RETC9603" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC960   BITACORA DE ERRORES DE CARGA TRANSFERENCIAS ISSSTE                  " AT 2,1 ATTRIBUTE(REVERSE)
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
            CLOSE WINDOW retc9603
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
                         " AND    programa = 'RETC960' ",
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
            CLOSE WINDOW retc9603

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retc9603
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
#               archivo de transferencias                                   #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro,p_folio)

    DEFINE p_folio LIKE ret_trans_issste.folio

    DEFINE
        pc_registro         CHAR(600)

    DEFINE lr_transfer_tmp  RECORD LIKE ret_trans_issste.*

    DEFINE lr_mto_viv       RECORD LIKE ret_monto_viv_issste.*

    DEFINE
        lc_det_viv          CHAR(450),
        lc_mto_viv          CHAR(15),
        lc_mto_solic        CHAR(16),
        lc_prcje            CHAR(06),
        lc_fechas           CHAR(10)

    LET lr_transfer_tmp.folio               = p_folio
    LET lr_transfer_tmp.curp                = pc_registro[015,032]
    LET lr_transfer_tmp.nss                 = f_obten_nss(lr_transfer_tmp.curp)
    LET lr_transfer_tmp.consecutivo         = f_ultimo_consec()

    LET lr_transfer_tmp.num_issste          = pc_registro[007,014]
    LET lr_transfer_tmp.nombre_datamart     = pc_registro[033,072]
    LET lr_transfer_tmp.paterno_datamart    = pc_registro[073,112]
    LET lr_transfer_tmp.materno_datamart    = pc_registro[113,152]
    LET lr_transfer_tmp.nombre_afore        = pc_registro[153,192]
    LET lr_transfer_tmp.paterno_afore       = pc_registro[193,232]
    LET lr_transfer_tmp.materno_afore       = pc_registro[233,272]
    LET lr_transfer_tmp.nss_issste          = pc_registro[273,283]
    LET lr_transfer_tmp.num_concesion       = pc_registro[284,292]
    LET lr_transfer_tmp.delegacion          = pc_registro[293,295]
    LET lr_transfer_tmp.sec_pension         = pc_registro[296,297]
    LET lr_transfer_tmp.tipo_movimiento     = pc_registro[298,300]
    LET lr_transfer_tmp.regimen             = pc_registro[301,302]
    LET lr_transfer_tmp.tipo_retiro         = pc_registro[303,303]
    LET lr_transfer_tmp.tipo_seguro         = pc_registro[304,305]
    LET lr_transfer_tmp.tipo_pension        = pc_registro[306,307]
    LET lr_transfer_tmp.cve_pension         = pc_registro[308,310]

    IF lr_transfer_tmp.cve_pension = " NA" THEN
        LET lr_transfer_tmp.cve_pension = "NA"
    END IF

    LET lr_transfer_tmp.tipo_prestacion     = pc_registro[311,312]

    LET lc_fechas                           = pc_registro[317,318],"/",
                                              pc_registro[319,320],"/",
                                              pc_registro[313,316]
    LET lr_transfer_tmp.fecha_ini_pen       = lc_fechas

    LET lc_fechas                           = pc_registro[325,326],"/",
                                              pc_registro[327,328],"/",
                                              pc_registro[321,324]
    LET lr_transfer_tmp.fecha_resolucion    = lc_fechas

    LET lr_transfer_tmp.semanas_cotizadas   = pc_registro[329,332]

    LET lc_fechas                           = pc_registro[337,338],"/",
                                              pc_registro[339,340],"/",
                                              pc_registro[333,336]
    LET lr_transfer_tmp.fecha_carga_datamart = lc_fechas

    LET lr_transfer_tmp.diag_registro       = pc_registro[341,343]
    LET lr_transfer_tmp.diag_procesar       = 0
    LET lr_transfer_tmp.periodo_pago        = pc_registro[344,349]

    LET lc_prcje                            = pc_registro[350,352],".",
                                              pc_registro[353,354]
    LET lr_transfer_tmp.porcentaje_ret08    = lc_prcje

    LET lc_prcje                            = pc_registro[355,357],".",
                                              pc_registro[358,359]
    LET lr_transfer_tmp.porcentaje_cv       = lc_prcje

    LET lc_prcje                            = pc_registro[360,362],".",
                                              pc_registro[363,364]
    LET lr_transfer_tmp.porcentaje_ahorro_sol = lc_prcje

    LET lc_prcje                            = pc_registro[365,367],".",
                                              pc_registro[368,369]
    LET lr_transfer_tmp.porcentaje_viv08    = lc_prcje

    LET lc_mto_solic                        = pc_registro[370,382],".",
                                              pc_registro[383,384]
    LET lr_transfer_tmp.mto_solic_issste    = lc_mto_solic

    --LET lr_transfer_tmp.cve_aseguradora     = pc_registro[385,387]
    
    LET lr_transfer_tmp.id_ap_comp = pc_registro[385,386]

    LET lr_transfer_tmp.mto_const_calculado = 0
    LET lr_transfer_tmp.grupo               = 0
    LET lr_transfer_tmp.estado_solicitud    = gr_edo.capturado
    LET lr_transfer_tmp.usuario             = gc_usuario

    -- Carga detalle para vivienda
    SELECT  *
    INTO    lc_det_viv
    FROM    tmp_arch_op42
    WHERE   n_registros[014,031] = lr_transfer_tmp.curp
    AND     n_registros[001,002] = "05"

    LET lr_mto_viv.curp             = lr_transfer_tmp.curp
    LET lr_mto_viv.consecutivo      = lr_transfer_tmp.consecutivo
    LET lr_mto_viv.folio            = p_folio
    LET lr_mto_viv.tipo_retiro      = lr_transfer_tmp.tipo_retiro
    LET lr_mto_viv.cve_operacion    = 42
    LET lr_mto_viv.fecha_valor_viv  = NULL
    LET lr_mto_viv.acciones_viv08   = 0
    LET lr_mto_viv.acciones_viv92   = 0
    LET lr_mto_viv.acc_viv92_bdsviv = 0

    LET lr_mto_viv.estado_sub_viv   = lc_det_viv[032,032]

    LET lc_mto_viv                  = lc_det_viv[041,048],".",
                                      lc_det_viv[049,054]
    LET lr_mto_viv.acc_viv08_bdsviv = lc_mto_viv


    INSERT INTO tmp_trans_issste VALUES(lr_transfer_tmp.*)
    INSERT INTO tmp_monto_viv_issste VALUES(lr_mto_viv.*)

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

    -- Valida que la operacion sea 42 - Detalle de Transferencias
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op42
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> "42"

    IF li_cont <> 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 1
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op42
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
    FROM   tmp_arch_op42
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
    FROM   tmp_arch_op42
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = 1
        LET lr_error.id_error = 4
        CALL f_inserta_bitacora(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[15,32]
    FROM   tmp_arch_op42
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_curp

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op42
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

        --- Valida detalle 05 ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op42
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
# f_valida_transferencia : Validaciones para el proceso de transferencias   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_transferencia(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_trans_issste.*

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
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que CURP no sea nulo o que venga a 18 posiciones ---
    IF (pr_trans_temp.curp IS NULL) OR (LENGTH(pr_trans_temp.curp) <> 18) THEN
        LET lr_error.nom_campo    = "curp"
        LET lr_error.valor_campo  = pr_trans_temp.curp
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

    --- Valida que la llave CURP - Secuencia de pension sea unica ---
    SELECT "OK"
    FROM   ret_trans_issste
    WHERE  curp         = pr_trans_temp.curp
    AND    sec_pension  = pr_trans_temp.sec_pension

    IF STATUS <> NOTFOUND THEN

        -- Si encuentra un registro con la misma combinacion,
        -- revisa si fue diagnosticado como rechazo
        SELECT "OK"
        FROM   ret_trans_issste A       ,
               tab_diag_transferencia B
        WHERE  A.curp           = pr_trans_temp.curp
        AND    A.sec_pension    = pr_trans_temp.sec_pension
        AND    A.diag_procesar  = B.diag_procesar
        AND    B.id_trans_rcv   = 0     -- Indica que no se paga rcv
        AND    B.id_trans_viv   = 0     -- Indica que no se paga vivienda
        GROUP BY 1

        -- Si el registro no viene de un rechazo entonces se envia error
        IF STATUS = NOTFOUND THEN
            LET lr_error.nom_campo    = "curp/sec_pension"
            LET lr_error.valor_campo  = pr_trans_temp.curp, " | ", pr_trans_temp.sec_pension
            LET lr_error.id_error     = 7
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
    END IF

    --- Valida que la Secuencia de pension recibida sea mayor a la maxima actual ---
    SELECT MAX(sec_pension)
    INTO   lc_max_sec
    FROM   ret_trans_issste
    WHERE  curp = pr_trans_temp.curp

    IF lc_max_sec IS NOT NULL THEN
        IF pr_trans_temp.sec_pension < lc_max_sec THEN
            LET lr_error.nom_campo    = "sec_pension"
            LET lr_error.valor_campo  = pr_trans_temp.sec_pension
            LET lr_error.id_error     = 8
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
    END IF

    --- Valida que el tipo de retiro corresponda a transferencias ---
    SELECT "OK"
    FROM   tab_ret_issste
    WHERE  tipo_retiro  = pr_trans_temp.tipo_retiro
    AND    movimiento BETWEEN 861 AND 869 -- Rango de movimientos de transferencias ISSSTE
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
        IF pr_trans_temp.porcentaje_ret08 > 100 OR pr_trans_temp.porcentaje_ret08 < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_ret08"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_ret08
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

        IF pr_trans_temp.porcentaje_ahorro_sol > 100 OR pr_trans_temp.porcentaje_ahorro_sol < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_ahorro_sol"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_ahorro_sol
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF

        IF pr_trans_temp.porcentaje_viv08 > 100 OR pr_trans_temp.porcentaje_viv08 < 0 THEN
            LET lr_error.id_error     = 11
            LET lr_error.nom_campo    = "porcentaje_viv08"
            LET lr_error.valor_campo  = pr_trans_temp.porcentaje_viv08
            LET ls_error              = 1
            CALL f_inserta_bitacora(lr_error.*)
        END IF
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_trans_issste.*

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
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_trans_temp.nombre_datamart IS NULL OR
       pr_trans_temp.nombre_datamart = "                                        " THEN
        LET lr_error.nom_campo    = "nombre_datamart"
        LET lr_error.valor_campo  = pr_trans_temp.nombre_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.paterno_datamart IS NULL OR
       pr_trans_temp.paterno_datamart = "                                        " THEN
        LET lr_error.nom_campo    = "paterno_datamart"
        LET lr_error.valor_campo  = pr_trans_temp.paterno_datamart
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.nombre_afore IS NULL OR
       pr_trans_temp.nombre_afore = "                                        " THEN
        LET lr_error.nom_campo    = "nombre_afore"
        LET lr_error.valor_campo  = pr_trans_temp.nombre_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.paterno_afore IS NULL OR
       pr_trans_temp.paterno_afore = "                                        " THEN
        LET lr_error.nom_campo    = "paterno_afore"
        LET lr_error.valor_campo  = pr_trans_temp.paterno_afore
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.delegacion IS NULL OR pr_trans_temp.delegacion = 0 THEN
        LET lr_error.nom_campo    = "delegacion"
        LET lr_error.valor_campo  = pr_trans_temp.delegacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF pr_trans_temp.tipo_movimiento IS NULL OR pr_trans_temp.tipo_movimiento = 0 THEN
        LET lr_error.nom_campo    = "tipo_movimiento"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_movimiento
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

    IF pr_trans_temp.cve_pension IS NULL OR pr_trans_temp.cve_pension = "   " THEN
        LET lr_error.nom_campo    = "cve_pension"
        LET lr_error.valor_campo  = pr_trans_temp.cve_pension
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
{
    IF pr_trans_temp.semanas_cotizadas IS NULL OR pr_trans_temp.semanas_cotizadas = 0 THEN
        LET lr_error.nom_campo    = "semanas_cotizadas"
        LET lr_error.valor_campo  = pr_trans_temp.semanas_cotizadas
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF
}
    IF pr_trans_temp.diag_registro IS NULL OR pr_trans_temp.diag_registro = 0 THEN
        LET lr_error.nom_campo    = "diag_registro"
        LET lr_error.valor_campo  = pr_trans_temp.diag_registro
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF
{
    IF pr_trans_temp.mto_solic_issste IS NULL OR pr_trans_temp.mto_solic_issste = 0 THEN
        LET lr_error.nom_campo    = "mto_solic_issste"
        LET lr_error.valor_campo  = pr_trans_temp.mto_solic_issste
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF
}
    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_det_viv_nulo : Valida que los campos del detalle de vivienda 05  #
#                         contengan informacion                             #
#---------------------------------------------------------------------------#
FUNCTION f_valida_det_viv_nulo(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_trans_issste.*

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
    WHERE  curp         = pr_trans_temp.curp
    AND    consecutivo  = pr_trans_temp.consecutivo

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF lr_mto_viv.estado_sub_viv IS NULL OR lr_mto_viv.estado_sub_viv = 0 THEN
        LET lr_error.nom_campo    = "estado_sub_viv"
        LET lr_error.valor_campo  = lr_mto_viv.estado_sub_viv
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    IF lr_mto_viv.acc_viv08_bdsviv IS NULL THEN
        LET lr_error.nom_campo    = "intereses viv 08"
        LET lr_error.valor_campo  = lr_mto_viv.acc_viv08_bdsviv
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_catalogo : Valida que existan los valores de los campos  #
#                             que tengan su propio catalogo                 #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_catalogo(ps_error, pi_id_error, pr_trans_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_trans_temp RECORD LIKE ret_trans_issste.*

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

    DEFINE lc_tmp_retiro LIKE ret_trans_issste.tipo_retiro

    DEFINE
        ls_error        SMALLINT

    LET ls_error              = ps_error
    LET lr_error.id_proceso   = pi_id_error
    LET lr_error.nss          = pr_trans_temp.nss
    LET lr_error.curp         = pr_trans_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 9

    -- Valida contra catalogo de Regimen
    SELECT "OK"
    FROM   tab_regimen_issste
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
    FROM   tab_seguro_issste
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
    FROM   tab_pension_issste
    WHERE  tipo_pension = pr_trans_temp.tipo_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_pension"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Clave de Pension
    SELECT "OK"
    FROM   tab_cve_pen_issste
    WHERE  cve_pension = pr_trans_temp.cve_pension
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "cve_pension"
        LET lr_error.valor_campo  = pr_trans_temp.cve_pension
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida contra catalogo de Tipo de Prestacion
    SELECT "OK"
    FROM   tab_prestacion_issste
    WHERE  tipo_prestacion = pr_trans_temp.tipo_prestacion
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lr_error.nom_campo    = "tipo_prestacion"
        LET lr_error.valor_campo  = pr_trans_temp.tipo_prestacion
        LET ls_error              = 1
        CALL f_inserta_bitacora(lr_error.*)
    END IF

    -- Valida que la combinacion exista en la matriz de derechos.
    SELECT UNIQUE(tipo_retiro)  ,
           grupo
    INTO   lc_tmp_retiro        ,
           pr_trans_temp.grupo
    FROM   ret_matriz_derecho_issste
    WHERE  regimen          = pr_trans_temp.regimen
    AND    tipo_seguro      = pr_trans_temp.tipo_seguro
    AND    tipo_pension     = pr_trans_temp.tipo_pension
    AND    cve_pension      = pr_trans_temp.cve_pension
    AND    tipo_prestacion  = pr_trans_temp.tipo_prestacion

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
        ELSE
            IF pr_trans_temp.tipo_retiro = 'I' AND pr_trans_temp.tipo_prestacion = 1 THEN
            	  LET pr_trans_temp.grupo = 116
            END IF
            -- Si la combinacion existe y es correcta, se da de alta el grupo
            UPDATE tmp_trans_issste
            SET    grupo       = pr_trans_temp.grupo
            WHERE  curp        = pr_trans_temp.curp
            AND    consecutivo = pr_trans_temp.consecutivo
        END IF
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio a procesar                       #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_folio       INTEGER

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
# f_obten_nss : Obtiene el nss del trabajador de la tabla afi_mae_afiliado  #
#---------------------------------------------------------------------------#
FUNCTION f_obten_nss(pc_curp)

    DEFINE pc_curp LIKE ret_trans_issste.curp
    DEFINE lc_nss  LIKE ret_trans_issste.nss

    IF gs_cod_afore <> gs_peiss THEN
        SELECT n_seguro
        INTO   lc_nss
        FROM   afi_mae_afiliado
        WHERE  n_unico = pc_curp
    ELSE
        SELECT n_seguro
        INTO   lc_nss
        FROM   afi_mae_afiliado
        WHERE  n_unico        = pc_curp
        AND    tipo_solicitud = 8
    END IF

    IF STATUS = NOTFOUND THEN
        LET lc_nss = NULL
    END IF

    RETURN lc_nss

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

    LET lc_hora = TIME

    -- Campos generales
    LET lr_bitacora.programa    = "RETC960"
    LET lr_bitacora.nom_archivo = reg_1.nom_archivo_op42
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

    DROP TABLE tmp_arch_op42

    CREATE TEMP TABLE tmp_arch_op42
    (
    n_registros          CHAR(450)
    )

    DROP TABLE tmp_trans_issste

    SELECT *
    FROM   ret_trans_issste
    WHERE  1 = 0
    INTO TEMP tmp_trans_issste


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

    DROP TABLE tmp_datamart

    SELECT *
    FROM   ret_datamart_issste
    WHERE  1 = 0
    INTO TEMP tmp_datamart

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_trans : Consulta la informacion que se cargo en las tablas de  #
#                    transferencias                                         #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_trans()

    DEFINE lar_transf ARRAY[5000] OF RECORD #glo #lar_transf
        curp                 LIKE ret_trans_issste.curp                 ,
        sec_pension          LIKE ret_trans_issste.sec_pension          ,
        folio                LIKE ret_trans_issste.folio                ,
        nss                  LIKE ret_trans_issste.nss                  ,
        num_issste           LIKE ret_trans_issste.num_issste           ,
        nombre_datamart      LIKE ret_trans_issste.nombre_datamart      ,
        paterno_datamart     LIKE ret_trans_issste.paterno_datamart     ,
        materno_datamart     LIKE ret_trans_issste.materno_datamart     ,
        nombre_afore         LIKE ret_trans_issste.nombre_afore         ,
        paterno_afore        LIKE ret_trans_issste.paterno_afore        ,
        materno_afore        LIKE ret_trans_issste.materno_afore        ,
        num_concesion        LIKE ret_trans_issste.num_concesion        ,
        delegacion           LIKE ret_trans_issste.delegacion           ,
        tipo_movimiento      LIKE ret_trans_issste.tipo_movimiento      ,
        tipo_retiro          LIKE ret_trans_issste.tipo_retiro          ,
        regimen              LIKE ret_trans_issste.regimen              ,
        tipo_seguro          LIKE ret_trans_issste.tipo_seguro          ,
        tipo_pension         LIKE ret_trans_issste.tipo_pension         ,
        cve_pension          LIKE ret_trans_issste.cve_pension          ,
        tipo_prestacion      LIKE ret_trans_issste.tipo_prestacion      ,
        fecha_ini_pen        LIKE ret_trans_issste.fecha_ini_pen        ,
        fecha_resolucion     LIKE ret_trans_issste.fecha_resolucion     ,
        semanas_cotizadas    LIKE ret_trans_issste.semanas_cotizadas    ,
        diag_registro        LIKE ret_trans_issste.diag_registro        ,
        mto_solic_issste     LIKE ret_trans_issste.mto_solic_issste     ,
        estado_sub_viv       LIKE ret_monto_viv_issste.estado_sub_viv   ,
        acc_viv08_bdsviv     LIKE ret_monto_viv_issste.acc_viv08_bdsviv ,
        estado_lote          LIKE ret_trans_issste.estado_solicitud     ,
        desc_estado          LIKE ret_estado_issste.descripcion         ,
        fecha_valor_trans    LIKE ret_cza_lote.fecha_valor_trans        ,
        fecha_carga          LIKE ret_cza_lote.fecha_carga              ,
        fecha_operacion      LIKE ret_cza_lote.fecha_operacion          ,
        nom_archivo          LIKE ret_cza_lote.nom_archivo
    END RECORD
     DEFINE lar_transf_comp ARRAY[5000] OF RECORD #glo #lar_transf
       id_ap_comp           CHAR(02)
    END RECORD
    DEFINE #loc #integer
        li_elemento        ,
        li_screen        ,
        arr_c                 ,
        i                     INTEGER

    DEFINE
        flag                  SMALLINT

    DEFINE
        txt_1                 CHAR(2000) ,
        x_busca               CHAR(1200)

    OPEN WINDOW retc9602 AT 4,4 WITH FORM "RETC9602" ATTRIBUTE(BORDER)
    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "RETC960        CONSULTA DE TRANSFERENCIAS CARGADAS ISSSTE                      " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    LET flag = 0

    CONSTRUCT BY NAME x_busca ON A.curp         ,
                                 A.folio        ,
                                 A.nss          ,
                                 B.nom_archivo

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
        ERROR "  BUSQUEDA CANCELADA...  "
        SLEEP 1
        ERROR ""
        CLEAR SCREEN
        CLOSE WINDOW retc9602
        RETURN
    END IF

    LET txt_1 = "SELECT A.curp               ,",
                      " A.sec_pension        ,",
                      " A.folio              ,",
                      " A.nss                ,",
                      " A.num_issste         ,",
                      " A.nombre_datamart    ,",
                      " A.paterno_datamart   ,",
                      " A.materno_datamart   ,",
                      " A.nombre_afore       ,",
                      " A.paterno_afore      ,",
                      " A.materno_afore      ,",
                      " A.num_concesion      ,",
                      " A.delegacion         ,",
                      " A.tipo_movimiento    ,",
                      " A.tipo_retiro        ,",
                      " A.regimen            ,",
                      " A.tipo_seguro        ,",
                      " A.tipo_pension       ,",
                      " A.cve_pension        ,",
                      " A.tipo_prestacion    ,",
                      " A.fecha_ini_pen      ,",
                      " A.fecha_resolucion   ,",
                      " A.semanas_cotizadas  ,",
                      " A.diag_registro      ,",
                      " A.mto_solic_issste   ,",
                      " C.estado_sub_viv     ,",
                      " C.acc_viv08_bdsviv   ,",
                      " A.estado_solicitud   ,",
                      " D.descripcion        ,",
                      " B.fecha_valor_trans  ,",
                      " B.fecha_carga        ,",
                      " B.fecha_operacion    ,",
                      " B.nom_archivo        ,",  
                      " A.id_ap_comp         ",  
               " FROM   ret_trans_issste A, ret_cza_lote B, ",
               "        ret_monto_viv_issste C, ret_estado_issste D ",
               " WHERE  ",x_busca CLIPPED,
               " AND A.folio = B.folio ",
               " AND A.estado_solicitud = D.estado_solicitud ",
               " AND A.folio = C.folio ",
               " AND A.consecutivo = C.consecutivo "



    PREPARE pre_6 FROM txt_1
    DECLARE cur_6 CURSOR FOR pre_6

    LET i = 1

    FOREACH cur_6 INTO lar_transf[i].*,lar_transf_comp[i].id_ap_comp
        LET i = i + 1
	    IF i >= 5000 THEN
	        ERROR "  Fue Sobrepasada la capacidad maxima del arreglo  "
	        EXIT FOREACH
	    END IF
    END FOREACH

    IF i = 1 THEN
        CLEAR FORM
        PROMPT " NO EXISTEN REGISTROS... <ENTER> PARA CONTINUAR " FOR CHAR enter
        CLEAR SCREEN
        CLOSE WINDOW retc9602
        RETURN
    ELSE
        DISPLAY "< Ctrl-P > Porcentajes  " AT 1,52 ATTRIBUTE(REVERSE)
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY lar_transf TO scr1.*

        ON KEY (CONTROL-P)
            LET li_elemento = ARR_CURR()
            LET li_screen   = SCR_LINE()

            IF lar_transf[li_elemento].diag_registro = 302 THEN
                CALL f_despliega_porc(lar_transf[li_elemento].curp       ,
                                      lar_transf[li_elemento].folio      ,
                                      lar_transf[li_elemento].sec_pension )
            ELSE
                ERROR "SOLO APLICA PARA TRANSFERENCIAS CON DIAGNOSTICO 302..." ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR "     " ATTRIBUTE(NORMAL)
            END IF

        ON KEY ( CONTROL-C )
	        EXIT DISPLAY

        ON KEY ( INTERRUPT )
	        EXIT DISPLAY

    END DISPLAY

    CLEAR SCREEN
    CLOSE WINDOW retc9602

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_porc : Despliega una ventana alterna donde se muestran los    #
#                    porcentajes a transferir en caso de tener el registro  #
#                    un diagnostico 302                                     #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_porc(pr_datos)

    DEFINE pr_datos RECORD
        curp        LIKE ret_trans_issste.curp      ,
        folio       LIKE ret_trans_issste.folio     ,
        sec_pension LIKE ret_trans_issste.sec_pension
    END RECORD

    DEFINE lar_porcent ARRAY[1] OF RECORD
        ret08       LIKE ret_trans_issste.porcentaje_ret08      ,
        cv          LIKE ret_trans_issste.porcentaje_cv         ,
        ahorro_sol  LIKE ret_trans_issste.porcentaje_ahorro_sol ,
        viv08       LIKE ret_trans_issste.porcentaje_viv08
    END RECORD

    SELECT porcentaje_ret08      ,
           porcentaje_cv         ,
           porcentaje_ahorro_sol ,
           porcentaje_viv08
    INTO   lar_porcent[1].*
    FROM   ret_trans_issste
    WHERE  curp         = pr_datos.curp
    AND    folio        = pr_datos.folio
    AND    sec_pension  = pr_datos.sec_pension

    OPEN WINDOW retc9604 AT 15,5 WITH FORM "RETC9604" ATTRIBUTE(BORDER)
    DISPLAY "            PORCENTAJE A TRANSFERIR             " AT 2,1  ATTRIBUTE(REVERSE)
    DISPLAY "                             <CTRL-C> - SALIR   " AT 8,1  ATTRIBUTE(REVERSE)

    CALL SET_COUNT(1)
    DISPLAY ARRAY lar_porcent TO prc.*

    CLOSE WINDOW retc9604

END FUNCTION

