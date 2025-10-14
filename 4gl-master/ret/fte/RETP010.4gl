#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETP010  => RECEPCION DEL ARCHIVO DE OP.60 Y GENERACION DE LA OP.61   #
#                     NOTIFICACION DE CLABE Y PAGO DE PROCESAR A ADMNISTRADORA  #
#Fecha creacion    => 3 DE JUNIO DE 2013                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_dat RECORD
        nom_archivo_det         CHAR(20)
    END RECORD

    DEFINE gr_edo RECORD
        capturado               LIKE ret_estado.estado_solicitud    ,
        confirmado              LIKE ret_estado.estado_solicitud    ,
        enviado                 LIKE ret_estado.estado_solicitud    ,
        recibido                LIKE ret_estado.estado_solicitud    ,
        liquidado               LIKE ret_estado.estado_solicitud    ,
        rechazado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_afores RECORD
        coppel                  SMALLINT    ,
        invercap                SMALLINT    ,
        issste                  SMALLINT    ,
        metlife                 SMALLINT
    END RECORD

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[6] OF RECORD
        accion      LIKE ret_tablas_rev.accion          ,
        tabla       LIKE ret_tablas_rev.tabla           ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_movimiento RECORD
        cancela_int                 SMALLINT    ,
        retiro                      SMALLINT
    END RECORD

    DEFINE gr_modulo RECORD LIKE seg_modulo.*

    DEFINE
        gs_codigo_afore         ,
        gs_marca_dev            ,
        gs_tipo_op              SMALLINT

    DEFINE
        gdt_fecha_viv           ,
        HOY                     DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_usuario              CHAR(020)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETP010        CARGA Y NOTIFICACION PAGO VIVIENDA                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "NOTIFICACION PAGO"
        COMMAND "Carga OP 60" "Carga Archivo de Notificacion (OP 60)"
            CALL f_menu_carga_archivo()

        COMMAND "Genera OP 61" "Genera Archivo de pago aplicado (OP 61)"
            CALL f_menu_envio_archivo()

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gdt_fecha_viv   = MDY(MONTH(HOY),01,YEAR(HOY))
    LET gc_usuario      = f_lib_obten_user()
    LET gs_tipo_op      = 60

    SELECT *
    INTO   gr_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gr_afores.coppel
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*COPPEL*"

    SELECT afore_cod
    INTO   gr_afores.invercap
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*INVERCAP*"

    SELECT afore_cod
    INTO   gr_afores.issste
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*PENSIONISSSTE*"

    SELECT afore_cod
    INTO   gr_afores.metlife
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*METLIFE*"

    ----- TIPOS DE MOVIMIENTO -----
    SELECT codigo
    INTO   gr_movimiento.cancela_int
    FROM   tab_movimiento
    WHERE  descripcion  = "CANCELACION DE INTERESES DE VIVIENDA"

    SELECT codigo
    INTO   gr_movimiento.retiro
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    SELECT marca_cod
    INTO   gs_marca_dev
    FROM   tab_marca
    WHERE  marca_desc   = "PROCESO DE DEVOLUCION DE VIVIENDA"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_menu_carga_archivo : Lanza el menu de las opciones para la carga del    #
#                        archivo de la operacion 60                         #
#---------------------------------------------------------------------------#
FUNCTION f_menu_carga_archivo()

    OPEN WINDOW carga_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETP010   RECEPCION ARCHIVOS NOTIFICACION CLABE Y PAGO - OP 60        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "RECEPCION OP60"
        COMMAND "Carga archivo" "Carga Archivo de Notificacion"
            CALL f_genera_carga()

        COMMAND "Reverso" "Realiza el reverso de la carga del archivo"
            CALL f_reverso_carga()

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Carga"
            CALL f_bitacora_err(0)
            CLEAR SCREEN

        COMMAND "Salir" "Regresa al Menu Principal"
            EXIT MENU
    END MENU

    CLOSE WINDOW carga_win

END FUNCTION

#---------------------------------------------------------------------------#
# f_menu_envio_archivo : Lanza el menu de las opciones para la generacion   #
#                        del archivo de la operacion 61                     #
#---------------------------------------------------------------------------#
FUNCTION f_menu_envio_archivo()

    OPEN WINDOW envia_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETP010   RECEPCION ARCHIVOS NOTIFICACION CLABE Y PAGO - OP 61        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "GENERACION OP61"
        COMMAND "Genera Archivo" "Genera el Archivo de la operacion 61"
            CALL f_genera_op61()
            CLEAR SCREEN

        COMMAND "Reverso" "Realiza el reverso de la carga del archivo"
            CALL f_reverso_envio()

        COMMAND "Salir" "Regresa al Menu Principal"
            EXIT MENU
    END MENU

    CLOSE WINDOW envia_win

END FUNCTION


#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de la op 60     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    DEFINE
        li_folio                ,
        li_proceso              INTEGER

    DEFINE
        ls_flag_err             ,
        ls_flag_proceso         SMALLINT

    -- -----------------------------------------------------------------------------

    CALL f_carga_archivo() RETURNING ls_flag_proceso  ,
                                     li_proceso

    IF (ls_flag_proceso = TRUE) THEN #-- La carga se realizo sin errores

        CALL primer_paso()      #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso()     #-- Realiza las validaciones de la informacion
            RETURNING ls_flag_err, li_proceso

        IF ls_flag_err = 0 THEN
            CALL tercer_paso()  #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("SE ENCONTRARON INCONSISTENCIAS")

            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    ELSE
        IF (li_proceso <> 0) THEN
            DISPLAY "                                             " AT 18,1
            CALL f_lib_error_msg("ARCHIVO CON INCONSISTENCIAS DE ESTRUCTURA")
            CALL f_bitacora_err(li_proceso) #-- Muestra la pantalla de errores
        END IF
    END IF

    CLEAR SCREEN
    CLOSE WINDOW RETP0101

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de     #
#                   rescate si existe. En caso de existir, lo carga sin     #
#                   formato en la tabla temporal                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE
        ls_flag                 ,
        ls_procesa              SMALLINT

    DEFINE
        li_tot_regs             INTEGER

    DEFINE
        lc_ruta_arch            CHAR(200)

    DEFINE li_id_proc LIKE ret_bitacora_error.id_proceso

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETP0101 AT 4,4 WITH FORM "RETP0101" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Ejecutar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETP010   RECEPCION ARCHIVOS NOTIFICACION CLABE Y PAGO - OP 60                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_id_proc  = 0
    LET li_tot_regs = 0
    LET ls_procesa  = 0

    INITIALIZE gr_dat.nom_archivo_det TO NULL

    INPUT BY NAME gr_dat.* WITHOUT DEFAULTS

        AFTER FIELD nom_archivo_det
            IF gr_dat.nom_archivo_det IS NULL THEN
                CALL f_lib_error_msg("EL NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo_det
            END IF

            SELECT "OK"
            FROM   ret_recepcion
            WHERE  nom_archivo = gr_dat.nom_archivo_det
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA CARGADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo_det
            END IF

        ON KEY (ESC)
            -- Validaciones de nom archivo
            IF gr_dat.nom_archivo_det IS NULL THEN
                CALL f_lib_error_msg("EL NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo_det
            END IF

            SELECT "OK"
            FROM   ret_recepcion
            WHERE  nom_archivo = gr_dat.nom_archivo_det
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA CARGADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo_det
            END IF

            -- Se realiza la carga del archivo
            LET lc_ruta_arch = gr_modulo.ruta_rescate CLIPPED,"/",
                               gr_dat.nom_archivo_det CLIPPED

            WHENEVER ERROR CONTINUE
                LOAD FROM lc_ruta_arch
                INSERT INTO tmp_arch_op60
            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_tot_regs
            FROM   tmp_arch_op60

            IF li_tot_regs = 0 THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO")
                NEXT FIELD nom_archivo_det
            ELSE
                IF f_lib_pregunta("¿DESEA CARGAR EL ARCHIVO DE LA OPERACION 60? (S/N) ") = TRUE THEN
                    CALL f_valida_archivo() RETURNING ls_flag   ,
                                                      li_id_proc

                    IF (ls_flag = TRUE) THEN -- hubo errores en la carga
                        LET ls_procesa  = FALSE
                    ELSE
                        LET ls_procesa  = TRUE
                    END IF
                ELSE
                    LET ls_procesa  = FALSE
                    CALL f_lib_error_msg("CARGA CANCELADA")
                END IF

                EXIT INPUT
            END IF

        ON KEY (INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            EXIT INPUT
    END INPUT

    RETURN ls_procesa, li_id_proc

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de la operacion 60 #
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE
        lc_reg_carga            CHAR(900)

    DEFINE
        li_cont                 INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_cont = 0

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_op60
    WHERE   n_registros[1,2] = "03"

    CALL f_ins_recepcion(gr_dat.nom_archivo_det)

    FOREACH cur_ar INTO lc_reg_carga

        LET li_cont     = li_cont + 1
        CALL f_carga_det(lc_reg_carga, li_cont)

    END FOREACH

    CALL f_act_tot_registros(li_cont)

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de la operacion 60       #
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

    DEFINE lr_notifica      RECORD LIKE ret_notifica_vivienda.*
    DEFINE lr_ajuste_viv    RECORD LIKE ret_ajuste_vivienda_nss.*

    DEFINE
        li_cont         INTEGER

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_notifica.* TO NULL
    INITIALIZE lr_ajuste_viv.* TO NULL

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
    FROM   tmp_notifica_vivienda

    FOREACH cur_tmp INTO lr_notifica.*

        SELECT *
        INTO   lr_ajuste_viv.*
        FROM   tmp_ajuste_vivienda_nss
        WHERE  nss              = lr_notifica.nss
        AND    consecutivo      = lr_notifica.consecutivo
        AND    folio            = lr_notifica.folio_carga


        LET lr_error.nss          = lr_notifica.nss
        LET lr_error.curp         = lr_notifica.curp
        LET lr_error.tipo_campo   = "REGISTRO"

        --- Validaciones sobre campos del detalle
        CALL f_valida_detalle(ls_flag            ,
                              lr_error.id_proceso,
                              lr_notifica.*      )
        RETURNING ls_flag

        --- Valida que los campos del detalle no sean nulos ---
        CALL f_valida_detalle_nulo(ls_flag              ,
                                   lr_error.id_proceso  ,
                                   lr_notifica.*        ,
                                   lr_ajuste_viv.*
                                  )
        RETURNING ls_flag

    END FOREACH  -- Validacion de Detalle

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas fisicas una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE lr_tmp_notifica      RECORD LIKE ret_notifica_vivienda.*
    DEFINE lr_tmp_ajuste_viv    RECORD LIKE ret_ajuste_vivienda_nss.*

    DEFINE lr_resultado_marca RECORD
        cod_rech            SMALLINT    ,
        desc_rechazo        CHAR(050)
    END RECORD

    DEFINE
        li_folio                ,
        li_tot_registros        ,
        li_tot_detalle          INTEGER

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_tmp_notifica.*   TO NULL
    INITIALIZE lr_tmp_ajuste_viv.* TO NULL

    DISPLAY "                                                                           " AT 9,1
    DISPLAY "                                                                           " AT 10,1
    DISPLAY "                                                                           " AT 11,1
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros    = 0
    LET li_tot_detalle      = 0
    LET li_folio            = f_lib_obtiene_ult_folio()

    SELECT COUNT(*)
    INTO   li_tot_registros
    FROM   tmp_arch_op60

    DISPLAY "FOLIO DE CARGA       : ", li_folio AT 8,24
    DISPLAY "TOTAL DE REGISTROS PROCESADOS            : ", li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE            : ", li_tot_detalle AT 11,9
    DISPLAY "TOTAL DE SOLICITUDES MARCADAS            : ", li_tot_detalle AT 12,9
    DISPLAY "TOTAL DE SOLICITUDES DE RETIRO GENERADAS : ", li_tot_detalle AT 13,9


    -- Insertamos todos los registros una vez que ya han sido validados

    -- Insertamos en ret_recepcion
    UPDATE tmp_recepcion
    SET    folio    = li_folio

    INSERT INTO ret_recepcion
    SELECT *
    FROM   tmp_recepcion

    -- Actualizamos en ret_notifica_vivienda
    DECLARE cur_ok CURSOR FOR
    SELECT *
    FROM   tmp_notifica_vivienda
    WHERE  folio_carga = 1

    FOREACH cur_ok INTO lr_tmp_notifica.*

        LET li_tot_detalle = li_tot_detalle + 1
        DISPLAY "TOTAL DE REGISTROS DE DETALLE            : ",li_tot_detalle AT 11,9

        SELECT *
        INTO   lr_tmp_ajuste_viv.*
        FROM   tmp_ajuste_vivienda_nss
        WHERE  nss          = lr_tmp_notifica.nss
        AND    consecutivo  = lr_tmp_notifica.consecutivo
        AND    folio        = lr_tmp_notifica.folio_carga

        LET lr_tmp_notifica.folio_carga = li_folio
        LET lr_tmp_notifica.consecutivo = f_lib_obtiene_ult_consec()

        INSERT INTO ret_notifica_vivienda
        VALUES (lr_tmp_notifica.*)

        LET lr_tmp_ajuste_viv.folio         = li_folio
        LET lr_tmp_ajuste_viv.consecutivo   = lr_tmp_notifica.consecutivo

        INSERT INTO ret_ajuste_vivienda_nss
        VALUES (lr_tmp_ajuste_viv.*)
{
        -- Marca la cuenta en el proceso de devolucion de vivienda
        CALL f_marca_cuenta(lr_tmp_notifica.nss         ,
                            lr_tmp_notifica.consecutivo ,
                            gs_marca_dev
                           )
            RETURNING lr_resultado_marca.*
}
        
        INITIALIZE lr_tmp_notifica.*   TO NULL
        INITIALIZE lr_tmp_ajuste_viv.* TO NULL

    END FOREACH

    CALL f_inserta_captura(li_folio)

    DISPLAY "                                             " AT 18,1
    CALL f_lib_error_msg("PROCESO FINALIZADO")

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_archivo : Realiza las validaciones del lote de la operacion 60   #
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

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_error.* TO NULL
    LET lr_error.id_proceso   = f_ultimo_id_err()

    ----------  Validaciones de estructura del archivo   --------------
    LET lr_error.tipo_campo   = "ARCHIVO"
    LET lr_error.nom_campo    = " "

    -- Valida que la operacion sea 60 - Notificacion de CLABE y Pago a administradora
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op60
    WHERE  n_registros[1,2] = "03"
    AND    n_registros[5,6] <> "60"

    IF li_cont <> 0 THEN
        LET ls_flag           = TRUE
        LET lr_error.id_error = 1
        CALL f_ins_bitacora_err(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el encabezado
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op60
    WHERE  n_registros[1,2] = "01"

    IF li_cont <> 1 THEN
        LET ls_flag           = TRUE
        LET lr_error.id_error = 2
        CALL f_ins_bitacora_err(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Inconsistencias en el sumario
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op60
    WHERE  n_registros[1,2] = "09"

    IF li_cont <> 1 THEN
        LET ls_flag           = TRUE
        LET lr_error.id_error = 3
        CALL f_ins_bitacora_err(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    -- Archivo sin registros de detalle
    SELECT COUNT(*)
    INTO   li_cont
    FROM   tmp_arch_op60
    WHERE  n_registros[1,2] = "03"

    IF li_cont = 0 THEN
        LET ls_flag           = TRUE
        LET lr_error.id_error = 4
        CALL f_ins_bitacora_err(lr_error.*)
        RETURN ls_flag, lr_error.id_proceso
    END IF

    --- Valida registro 03 ---

    DECLARE cur_03 CURSOR FOR
    SELECT n_registros[046,056]
    FROM   tmp_arch_op60
    WHERE  n_registros[1,2] = "03"

    FOREACH cur_03 INTO lc_nss

        --- Valida registro 03 duplicado ---
        SELECT COUNT(*)
        INTO   li_cont
        FROM   tmp_arch_op60
        WHERE  n_registros[001,002]     = "03"
        AND    n_registros[046,056]     = lc_nss

        IF li_cont > 1 THEN
            LET lr_error.nom_campo   = "det 03 duplicado"
            LET ls_flag              = TRUE
            LET lr_error.id_error    = 11
            LET lr_error.valor_campo = lc_nss

            CALL f_ins_bitacora_err(lr_error.*)
            RETURN ls_flag, lr_error.id_proceso
        END IF

    END FOREACH

    RETURN ls_flag, lr_error.id_proceso

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_captura : Inserta los registros en ret_solicitud_tx para que    #
#                     sean incluidos en la operacion 05 de disposiciones    #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_captura(pi_folio)

    DEFINE
        pi_folio        INTEGER

    DEFINE lr_ajuste        RECORD LIKE ret_ajuste_vivienda_nss.*
    DEFINE lr_notifica      RECORD LIKE ret_notifica_vivienda.*
    DEFINE lr_beneficiario  RECORD LIKE ret_beneficiario.*
--    DEFINE lr_ctr_pago      RECORD LIKE ret_ctr_folio_pago.*
--    DEFINE lr_folio_pago    RECORD LIKE ret_folio_pago.*

    DEFINE
        lr_solicitud        ,
        lr_sol_actual       RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_resultado_marca RECORD
        cod_rech            SMALLINT    ,
        desc_rechazo        CHAR(050)
    END RECORD

    DEFINE lr_pago05 RECORD
        aivs_97             DECIMAL(16,6)   ,
        aivs_92             DECIMAL(16,6)
    END RECORD

    DEFINE
        li_max_consec            INTEGER,
        li_num_marca             INTEGER

    DEFINE
        ls_inserta              ,
        ls_movimiento           ,
        ls_error                ,
        ls_num_sol              SMALLINT

    DEFINE
        lc_cad_subcuenta        CHAR(150)
        
    DEFINE lc_error         CHAR(1000)
    
    DEFINE ls_correlativo       INTEGER #correlativo de la marca  #CPL-1903

    -- -----------------------------------------------------------------------------

    INITIALIZE
        lr_solicitud.*      ,
        lr_sol_actual.*     ,
        lr_ajuste.*         ,
        lr_beneficiario.*   ,
        lr_notifica.*       TO NULL

    LET li_max_consec   = 0
    LET ls_num_sol      = 0
    LET li_num_marca    = 0
    LET lc_error        = ""
    LET ls_error        = 0

    -- Se generan registros para los que ingresen por ventanilla INFONAVIT (0101)
    DECLARE cur_inserta CURSOR FOR
        SELECT A.*  ,
               B.*
        FROM   ret_ajuste_vivienda_nss A    ,
               ret_notifica_vivienda B
        WHERE  A.estado             = gr_edo.capturado
        AND    A.folio              = pi_folio
        AND    A.folio              = B.folio_carga
        AND    A.nss                = B.nss
        AND    A.consecutivo        = B.consecutivo
        AND    B.grupo_trabajador   = "0101"
        AND    B.indicador          = "A"
        ORDER BY A.consecutivo

    FOREACH cur_inserta INTO lr_ajuste.*, lr_notifica.*

#CPL-2312 INI Valida monto de cuenta segun matriz de derecho
        CALL f_valida_mto_subcuentas(lr_notifica.nss                ,
                                     lr_notifica.tipo_retiro        ,
                                     lr_notifica.regimen            ,
                                     lr_notifica.tipo_seguro        ,
                                     lr_notifica.tipo_pension       ,
                                     lr_notifica.tipo_prestacion
                                    )
            RETURNING lr_solicitud.grupo    ,
                      ls_inserta

        -- la cuenta tiene saldo en las subcuentas de rcv
        IF (ls_inserta = TRUE) THEN
#CPL-2312 FIN
            SELECT NVL(SUM(viv_97_aivs), 0)
            INTO   lr_pago05.aivs_97
            FROM   ret_ajuste_vivienda_nss
            WHERE  nss              = lr_ajuste.nss
            AND    folio            = pi_folio

            SELECT NVL(SUM(viv_92_aivs), 0)
            INTO   lr_pago05.aivs_92
            FROM   ret_ajuste_vivienda_nss
            WHERE  nss              = lr_ajuste.nss
            AND    folio            = pi_folio

            SELECT NVL(MAX(id_registro), 0)
            INTO   li_max_consec
            FROM   ret_det_datamart
            WHERE  nss = lr_ajuste.nss

            IF li_max_consec = 0 THEN
                SELECT NVL(MAX(id_registro), 0)
                INTO   li_max_consec
                FROM   ret_rech_datamart
                WHERE  nss = lr_ajuste.nss

                SELECT NVL(fecha_ini_pen,"")        ,
                       fecha_resolucion
                INTO   lr_sol_actual.fecha_ini_pen      ,
                       lr_sol_actual.fecha_resolucion
                FROM   ret_rech_datamart
                WHERE  nss         = lr_ajuste.nss
                AND    id_registro = li_max_consec
            ELSE
                SELECT NVL(fecha_ini_pen,"")        ,
                       fecha_resolucion
                INTO   lr_sol_actual.fecha_ini_pen      ,
                       lr_sol_actual.fecha_resolucion
                FROM   ret_det_datamart
                WHERE  nss         = lr_ajuste.nss
                AND    id_registro = li_max_consec
            END IF

#CPL-2316 INI Si no trae FIP por no tener datamart, no se inserta solicitud.
            IF (lr_sol_actual.fecha_ini_pen IS NULL OR lr_sol_actual.fecha_ini_pen = "") THEN

                INITIALIZE
                    lr_solicitud.*      ,
                    lr_sol_actual.*     ,
                    lr_ajuste.*         ,
                    lr_beneficiario.*   ,
                    lr_notifica.*       TO NULL

                CONTINUE FOREACH

            END IF
#CPL-2316 FIN

            -- -----------------------------------------------------------------------------
            -- Datos de la Solicitud
            -- -----------------------------------------------------------------------------
            LET lr_solicitud.nss                    = lr_notifica.nss
            LET lr_solicitud.consecutivo            = lr_notifica.consecutivo
            LET lr_solicitud.folio                  = lr_notifica.folio_carga
            LET lr_solicitud.tipo_id                = "S"
            LET lr_solicitud.curp                   = lr_notifica.curp
            LET lr_solicitud.sec_pension            = lr_notifica.sec_pension
            LET lr_solicitud.tipo_documento         = 4
            LET lr_solicitud.tipo_retiro            = lr_notifica.tipo_retiro
            LET lr_solicitud.regimen                = lr_notifica.regimen
            LET lr_solicitud.tipo_seguro            = lr_notifica.tipo_seguro
            LET lr_solicitud.tipo_pension           = lr_notifica.tipo_pension
            LET lr_solicitud.tipo_prestacion        = lr_notifica.tipo_prestacion
            LET lr_solicitud.fecha_ini_pen          = lr_sol_actual.fecha_ini_pen
            LET lr_solicitud.fecha_resolucion       = lr_sol_actual.fecha_resolucion
            LET lr_solicitud.fecha_solicitud        = HOY
            LET lr_solicitud.aseguradora            = 0
            LET lr_solicitud.fecha_valor_viv        = gdt_fecha_viv
            LET lr_solicitud.saldo_viv97            = lr_pago05.aivs_97
            LET lr_solicitud.saldo_viv92            = lr_pago05.aivs_92
            LET lr_solicitud.saldo_viv72            = 0
            LET lr_solicitud.semanas_cotizadas      = lr_notifica.semanas_cotizadas
            LET lr_solicitud.estado_solicitud       = gr_edo.confirmado
            LET lr_solicitud.rechazo_cod            = 0
            LET lr_solicitud.fecha_captura          = HOY
            LET lr_solicitud.fecha_confirma         = HOY
            LET lr_solicitud.usuario_captura        = "safre"
            LET lr_solicitud.usuario_confirma       = "safre"
            LET lr_solicitud.cve_destino            = "T"
            LET lr_solicitud.porcentaje_val         = 100
            LET lr_solicitud.num_resolucion         = 0
            LET lr_solicitud.paterno_sol            = lr_notifica.paterno
            LET lr_solicitud.materno_sol            = lr_notifica.materno
            LET lr_solicitud.nombre_sol             = lr_notifica.nombre
{
            SELECT grupo
            INTO   lr_solicitud.grupo
            FROM   ret_matriz_derecho
            WHERE  tipo_retiro      = lr_notifica.tipo_retiro
            AND    regimen          = lr_notifica.regimen
            AND    tipo_seguro      = lr_notifica.tipo_seguro
            AND    tipo_pension     = lr_notifica.tipo_pension
            AND    tipo_prestacion  = lr_notifica.tipo_prestacion

            -- Recuperamos el grupo que corresponde a la cadena de derechos, pero 
            -- excluyendo las subcuentas de vivienda
            LET lc_cad_subcuenta = f_lib_genera_cadena_subcuentas(lr_solicitud.grupo, FALSE)

            SELECT grupo
            INTO   lr_solicitud.grupo
            FROM   tab_agrupa_subcta_cad
            WHERE  cad_subcta = lc_cad_subcuenta
            GROUP BY 1
}
            IF lr_solicitud.sec_pension IS NULL THEN
                LET lr_solicitud.sec_pension = "01"
            END IF

            IF lr_solicitud.fecha_resolucion IS NULL THEN
                LET lr_solicitud.fecha_resolucion = HOY
            END IF

#CPL-1903  INI  #Si el nss se encuentra marcado cn la 819, lo desmarca
            IF f_val_marca (lr_notifica.nss, gs_marca_dev) = 1 THEN

                INITIALIZE ls_correlativo TO NULL
                SELECT correlativo INTO ls_correlativo
                FROM safre_af:cta_act_marca
                WHERE nss = lr_notifica.nss
                AND marca_cod = gs_marca_dev
                GROUP BY 1

                CALL f_desmarca_cuenta(lr_notifica.nss, ls_correlativo, gs_marca_dev)

            END IF
#CPL-1903  FIN

            -- Marca la cuenta en el proceso de disposicion total de acuerdo al tipo de retiro
            SELECT movimiento
            INTO   ls_movimiento
            FROM   tab_retiro
            WHERE  tipo_retiro  = lr_solicitud.tipo_retiro

            CALL f_marca_cuenta(lr_solicitud.nss            ,
                                lr_solicitud.consecutivo    ,
                                ls_movimiento
                               )
                RETURNING lr_resultado_marca.*

            IF lr_resultado_marca.cod_rech = 0 THEN

                LET li_num_marca    = li_num_marca + 1
                DISPLAY "TOTAL DE SOLICITUDES MARCADAS            : ", li_num_marca AT 12,9

                -- Genera el folio de solicitud de acuerdo a la afore
                LET lr_solicitud.folio_solicitud    = f_genera_folio_sol(lr_notifica.nss        ,
                                                                         lr_notifica.consecutivo
                                                                        )

                -- Obtenemos el registro a insertar del beneficiario
                CALL f_genera_beneficiario(lr_notifica.*)
                    RETURNING lr_beneficiario.*

                -- Insertamos la solicitud de retiro
                INSERT INTO ret_solicitud_tx
                VALUES (lr_solicitud.*)

                LET ls_num_sol = ls_num_sol + 1

                -- Insertamos el beneficiario y los folios de control de pago
                INSERT INTO ret_beneficiario
                VALUES (lr_beneficiario.*)
{               
                SELECT MAX(folio_pago) + 1
                INTO   lr_ctr_pago.folio_pago
                FROM   ret_ctr_folio_pago
                
                LET lr_ctr_pago.fecha_registro  = HOY
                
                INSERT INTO ret_ctr_folio_pago
                VALUES (lr_ctr_pago.*)
                
                LET lr_folio_pago.nss           = lr_beneficiario.nss
                LET lr_folio_pago.consecutivo   = lr_beneficiario.consecutivo
                LET lr_folio_pago.consec_benef  = lr_beneficiario.consec_benef
                LET lr_folio_pago.folio_pago    = lr_ctr_pago.folio_pago
                LET lr_folio_pago.folio_rec     = lr_solicitud.folio_solicitud
                
                INSERT INTO ret_folio_pago
                VALUES (lr_folio_pago.*)
}               
                DISPLAY "TOTAL DE SOLICITUDES DE RETIRO GENERADAS : ", ls_num_sol AT 13,9

            ELSE

                LET ls_error = 1
                LET lc_error = "NO SE PUDO MARCAR EL NSS: ",lr_solicitud.nss CLIPPED, ". SE INSERTA SU SOLICITUD DE DISPOSICION RECHAZADA." CLIPPED
                CALL ERRORLOG (lc_error)

                LET lr_solicitud.estado_solicitud       = gr_edo.rechazado

                -- Genera el folio de solicitud de acuerdo a la afore
                LET lr_solicitud.folio_solicitud    = f_genera_folio_sol(lr_notifica.nss        ,
                                                                         lr_notifica.consecutivo
                                                                        )

                LET ls_num_sol = ls_num_sol + 1

                -- Insertamos la solicitud de retiro rechazada
                INSERT INTO ret_solicitud_tx
                VALUES (lr_solicitud.*)

                DISPLAY "TOTAL DE SOLICITUDES DE RETIRO GENERADAS : ", ls_num_sol AT 13,9

                {WHENEVER ERROR CONTINUE
                SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
                LOCK TABLE ret_ajuste_vivienda_nss IN EXCLUSIVE MODE;
            
                    DELETE FROM   ret_ajuste_vivienda_nss
                    WHERE  folio       = lr_notifica.folio_carga
                    AND    nss         = lr_notifica.nss
                    AND    consecutivo = lr_notifica.consecutivo 
                    AND    estado      = gr_edo.capturado
                
                IF SQLCA.SQLERRD[3] = 0 THEN
                   LET lc_error = "NO SE ELIMINO EL REGISTRO DE ret_ajuste_vivienda_nss NSS : ", lr_notifica.nss CLIPPED
                   CALL ERRORLOG (lc_error)               
                   SET LOCK MODE TO NOT WAIT;
                   UNLOCK TABLE ret_ajuste_vivienda_nss;
                   WHENEVER ERROR STOP
                   CONTINUE FOREACH;
                END IF
                SET LOCK MODE TO NOT WAIT;
                UNLOCK TABLE ret_ajuste_vivienda_nss;
                WHENEVER ERROR STOP
                
                LET lc_error = "REGISTRO ELIMINADO DE ret_ajuste_vivienda_nss. NSS: ",lr_notifica.nss CLIPPED
                CALL ERRORLOG (lc_error)
                
                WHENEVER ERROR CONTINUE
                SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
                LOCK TABLE ret_notifica_vivienda IN EXCLUSIVE MODE;
                
                   DELETE FROM   ret_notifica_vivienda
                   WHERE  folio_carga  = lr_notifica.folio_carga
                   AND    nss         = lr_notifica.nss
                   AND    consecutivo = lr_notifica.consecutivo 
                   AND    estado       = gr_edo.capturado
                
                IF SQLCA.SQLERRD[3] = 0 THEN
                   LET lc_error = "NO SE ELIMINO EL REGISTRO DE ret_notifica_vivienda NSS : ", lr_notifica.nss CLIPPED
                   CALL ERRORLOG (lc_error)
                   SET LOCK MODE TO NOT WAIT;
                   UNLOCK TABLE ret_notifica_vivienda;
                   WHENEVER ERROR STOP
                   CONTINUE FOREACH;
                END IF
                SET LOCK MODE TO NOT WAIT;
                UNLOCK TABLE ret_notifica_vivienda;
                WHENEVER ERROR STOP
                
                LET lc_error = "REGISTRO ELIMINADO DE ret_notifica_vivienda. NSS: ",lr_notifica.nss CLIPPED
                CALL ERRORLOG (lc_error)}

            END IF   -- marca de la cuenta

        END IF   -- saldo de las subcuentas

        INITIALIZE
            lr_solicitud.*      ,
            lr_sol_actual.*     ,
            lr_ajuste.*         ,
            lr_beneficiario.*   ,
            lr_notifica.*       TO NULL

    END FOREACH
    
    IF ls_error = 1 THEN
    	 DISPLAY "                                             " AT 18,1
       CALL f_lib_error_msg("REVISAR LOG DEL PROGRAMA")
    END IF
END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_beneficiario : Genera el registro de beneficiarios que se        #
#                         insertara en la solicitud, dependiendo de los     #
#                         datos cargados en ventanilla y de la afore        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_beneficiario(pr_notifica)

    DEFINE pr_notifica RECORD LIKE ret_notifica_vivienda.*

    DEFINE lr_benef_vent RECORD LIKE ret_beneficiario.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_benef_vent.* TO NULL 

    LET lr_benef_vent.nss               = pr_notifica.nss
    LET lr_benef_vent.consecutivo       = pr_notifica.consecutivo
    LET lr_benef_vent.consec_benef      = 1
    LET lr_benef_vent.paterno           = pr_notifica.paterno_benef
    LET lr_benef_vent.materno           = pr_notifica.materno_benef
    LET lr_benef_vent.nombres           = pr_notifica.nombre_benef
--    LET lr_benef_vent.rfc_benef         = pr_notifica.rfc_benef
    LET lr_benef_vent.banco             = pr_notifica.clabe[1,3]
    LET lr_benef_vent.paren_cod         = 12
    LET lr_benef_vent.porcentaje        = 100
    LET lr_benef_vent.fecha_captura     = HOY
    LET lr_benef_vent.usuario_captura   = gc_usuario

    CALL f_obten_domicilio(lr_benef_vent.nss)
        RETURNING lr_benef_vent.ciudad_cod        ,
                  lr_benef_vent.estad_cod         ,
                  lr_benef_vent.dom_calle         ,
                  lr_benef_vent.dom_numero_ext    ,
                  lr_benef_vent.dom_numero_int    ,
                  lr_benef_vent.dom_codpos        ,
                  lr_benef_vent.dom_colonia       ,
                  lr_benef_vent.dom_delega        ,
                  lr_benef_vent.dom_ciudad_cod    ,
                  lr_benef_vent.dom_estado_cod

    CASE gs_codigo_afore
        WHEN gr_afores.issste
            LET lr_benef_vent.num_cuenta    = pr_notifica.clabe
            LET lr_benef_vent.tipo_pago     = 3

        WHEN gr_afores.invercap
            LET lr_benef_vent.num_cuenta    = pr_notifica.clabe
            LET lr_benef_vent.tipo_pago     = 1

        WHEN gr_afores.metlife
            LET lr_benef_vent.clabe         = pr_notifica.clabe
            LET lr_benef_vent.tipo_pago     = 4

        WHEN gr_afores.coppel
            IF lr_benef_vent.banco = 137 THEN      #cpl2012
               LET lr_benef_vent.tipo_pago     = 3
            ELSE
               LET lr_benef_vent.tipo_pago     = 2
            END IF 
            LET lr_benef_vent.num_cuenta    = pr_notifica.clabe
        
        OTHERWISE
            LET lr_benef_vent.num_cuenta    = pr_notifica.clabe
            LET lr_benef_vent.tipo_pago     = 2

    END CASE

    RETURN lr_benef_vent.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_domicilio : Obtiene los datos registrados de domicilio para el    #
#                     nss actual                                            #
#---------------------------------------------------------------------------#
FUNCTION f_obten_domicilio(pc_nss)

    DEFINE pc_nss LIKE ret_beneficiario.nss

    DEFINE lr_afiliado RECORD
        num_folio       LIKE afi_mae_afiliado.n_folio           ,
        tipo_sol        LIKE afi_mae_afiliado.tipo_solicitud
    END RECORD

    DEFINE lr_domicilio RECORD
        ciudad_cod          LIKE afi_domicilio.ciudad   ,
        estad_cod           LIKE afi_domicilio.estado   ,
        dom_calle           LIKE afi_domicilio.calle    ,
        dom_numero_ext      LIKE afi_domicilio.numero   ,
        dom_numero_int      LIKE afi_domicilio.depto    ,
        dom_codpos          LIKE afi_domicilio.codpos   ,
        dom_colonia         LIKE afi_domicilio.colonia  ,
        dom_delega          LIKE afi_domicilio.delega   ,
        dom_ciudad_cod      LIKE afi_domicilio.ciudad   ,
        dom_estado_cod      LIKE afi_domicilio.estado
    END RECORD

    -- -----------------------------------------------------------------------------

    -- Obtiene los datos del afiliado
    SELECT n_folio          ,
           tipo_solicitud
    INTO   lr_afiliado.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    -- Obtiene los datos del domicilio del afiliado
    SELECT a.ciudad         ,
           a.estado         ,
           a.calle          ,
           a.numero         ,
           a.depto          ,
           a.codpos         ,
           a.colonia        ,
           a.delega         ,
           a.ciudad         ,
           a.estado
    INTO   lr_domicilio.*
    FROM   afi_domicilio a
    WHERE  a.nss            = pc_nss
    AND    a.n_folio        = lr_afiliado.num_folio
    AND    a.tipo_solicitud = lr_afiliado.tipo_sol
    AND    a.marca_envio    = "X"
    GROUP BY 1,2,3,4,5,6,7,8,9,10

    RETURN lr_domicilio.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_folio_sol : Genera el folio de solicitud de acuerdo a la afore   #
#                      y dependiendo si se debe generar un servicio         #
#---------------------------------------------------------------------------#
FUNCTION f_genera_folio_sol(pr_folio)

    DEFINE pr_folio RECORD
        nss                 LIKE ret_solicitud_tx.nss           ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lr_datos_serv RECORD 
        nss                 CHAR(11)    ,
        medio               SMALLINT    ,
        tipo                SMALLINT    ,
        motivo              SMALLINT    , 
        fecha               DATE        ,
        descripcion         CHAR(500)   ,
        usuario             VARCHAR(10)
    END RECORD 

    DEFINE lr_servicio RECORD
        folio_sol           LIKE ret_solicitud_tx.folio_solicitud   ,
        descripcion         CHAR(250)
    END RECORD 

    DEFINE
        lc_prepare          CHAR(300)

    -- -----------------------------------------------------------------------------

    INITIALIZE 
        lr_datos_serv.*     ,
        lr_servicio.*       TO NULL 

    IF (gs_codigo_afore <> gr_afores.metlife) THEN
        LET lr_servicio.folio_sol = 0
    ELSE
        LET lc_prepare = " EXECUTE FUNCTION fn_inserta_servicios(?,?,?,?,?,?,?) "
        
        LET lr_datos_serv.nss           = pr_folio.nss
        LET lr_datos_serv.medio         = "7"
        LET lr_datos_serv.tipo          = "3"
        LET lr_datos_serv.motivo        = "4"
        LET lr_datos_serv.fecha         = HOY
        LET lr_datos_serv.descripcion   = "SOLICITUD GENERADA POR VENTANILLA INFONAVIT"
        LET lr_datos_serv.usuario       = gc_usuario

        PREPARE eje_servicio FROM lc_prepare
        EXECUTE eje_servicio USING lr_datos_serv.*
                             INTO  lr_servicio.*
    
    END IF

    RETURN lr_servicio.folio_sol

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

    OPEN WINDOW RETP0102 AT 4,4 WITH FORM "RETP0102" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETP010   BITACORA DE ERRORES DE CARGA DE NOTIFICACION DE PAGO                " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    DISPLAY "RETP010" TO programa

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
            CLOSE WINDOW RETP0102
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
                         " AND    programa = 'RETP010' ",
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
            CLOSE WINDOW RETP0102

        ELSE
            CALL f_lib_error_msg("NO EXISTEN REGISTROS")
            CLEAR SCREEN
            CLOSE WINDOW RETP0102
        END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_carga : Ejecuta el reverso de la carga de archivo de la op. 60  #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_carga()

    DEFINE li_folio_carga LIKE ret_notifica_vivienda.folio_carga

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    LET gr_bitacora.usuario         = gc_usuario
    LET gr_bitacora.programa        = "RETP010"
    LET gr_bitacora.desc_tramite    = "REC. ARCHIVO PAGO VIV"
    LET gr_bitacora.fecha_ini       = HOY
    LET gr_bitacora.hora_ini        = CURRENT HOUR TO SECOND

    CALL f_captura_folio_reverso(60)
        RETURNING ls_procesa, li_folio_carga

    IF ls_procesa THEN
        CALL f_ejecuta_reverso_carga(li_folio_carga)
        CALL f_act_bitacora(li_folio_carga)
        CALL f_lib_error_msg("REVERSO TERMINADO")
    END IF

    CLOSE WINDOW win_reverso_vent

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio_reverso : Captura el folio de carga a reversar            #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio_reverso(ps_operacion)

    DEFINE
        ps_operacion        SMALLINT

    DEFINE li_folio_carga LIKE ret_notifica_vivienda.folio_carga

    DEFINE
        ls_estado_Rev       ,
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_procesa  = TRUE

    OPEN WINDOW win_reverso_vent AT 4,4 WITH FORM "RETP0103" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    IF (ps_operacion = 60) THEN
        LET ls_estado_Rev   = gr_edo.capturado
        DISPLAY " RETP010  REVERSO DE CARGA DE ARCHIVO DE NOTIFICACION DE PAGO              " AT 3,1 ATTRIBUTE(REVERSE)
    ELSE
        LET ls_estado_Rev   = gr_edo.liquidado
        DISPLAY " RETP010           REVERSO DE GENERACION DE LA OP. 61                      " AT 3,1 ATTRIBUTE(REVERSE)
    END IF

    SELECT MAX(folio_carga)
    INTO   li_folio_carga
    FROM   ret_notifica_vivienda
    WHERE  estado   = ls_estado_Rev

    -- Captura Del Folio A Reversar
    INPUT BY NAME li_folio_carga WITHOUT DEFAULTS

        AFTER FIELD li_folio_carga
            IF (li_folio_carga IS NULL) THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que el folio exista y este capturado
            SELECT "OK"
            FROM   ret_notifica_vivienda
            WHERE  folio_carga      = li_folio_carga
            AND    estado           = ls_estado_Rev
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA CARGADO")
                NEXT FIELD li_folio_carga
            END IF

        ON KEY (ESC)
            IF (li_folio_carga IS NULL) THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que el folio exista y este cargado
            SELECT "OK"
            FROM   ret_notifica_vivienda
            WHERE  folio_carga      = li_folio_carga
            AND    estado           = ls_estado_Rev
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA CARGADO")
                NEXT FIELD li_folio_carga
            END IF

            IF f_lib_pregunta("¿DESEA GENERAR EL REVERSO? (S/N) : ") = TRUE THEN
                LET ls_procesa = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_procesa = FALSE
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = FALSE
            EXIT INPUT

    END INPUT

    RETURN ls_procesa, li_folio_carga

END FUNCTION

#-------------------------------------------------------------------------------#
# f_ejecuta_reverso_carga : Ejecuta el reverso de la carga de la op. 60         #
#-------------------------------------------------------------------------------#
FUNCTION f_ejecuta_reverso_carga(pi_folio)

    DEFINE
        pi_folio           INTEGER

    DEFINE lr_desmarca RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo   ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE
        li_num_afect        INTEGER

    DEFINE
        ls_movimiento       SMALLINT

    DEFINE
        lc_prepare          CHAR(100)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_desmarca.* TO NULL
    LET li_num_afect =  0

    DECLARE cur_desmarca CURSOR FOR
        SELECT nss          ,
               consecutivo  ,
               tipo_retiro
        FROM   ret_notifica_vivienda
        WHERE  folio_carga  = pi_folio
        ORDER BY nss

    FOREACH cur_desmarca INTO lr_desmarca.*

        SELECT movimiento
        INTO   ls_movimiento
        FROM   tab_retiro
        WHERE  tipo_retiro  = lr_desmarca.tipo_retiro

        CALL f_desmarca_cuenta(lr_desmarca.nss          ,
                               lr_desmarca.consecutivo  ,
                               ls_movimiento
                              )
        LET li_num_afect = li_num_afect + 1

    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION              : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_beneficiario
    WHERE  consecutivo IN (SELECT consecutivo
                           FROM   ret_solicitud_tx
                           WHERE  folio    = pi_folio
                          )

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_beneficiario          : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[2].tabla     = "ret_beneficiario"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_solicitud_tx
    WHERE  folio    = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_solicitud_tx          : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[3].tabla     = "ret_solicitud_tx"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_recepcion
    WHERE  folio    = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_recepcion             : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[4].tabla     = "ret_recepcion"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_ajuste_vivienda_nss
    WHERE  folio    = pi_folio
    AND    estado   = gr_edo.capturado

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_ajuste_vivienda_nss   : ", li_num_afect
            USING "<<<,<<&" AT 14,14

    LET gar_tablas_rev[5].tabla     = "ret_ajuste_vivienda_nss"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_notifica_vivienda
    WHERE  folio_carga  = pi_folio
    AND    estado       = gr_edo.capturado


    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_notifica_vivienda     : ", li_num_afect
            USING "<<<,<<&" AT 15,14

    LET gar_tablas_rev[6].tabla     = "ret_notifica_vivienda"
    LET gar_tablas_rev[6].accion    = "BORRADA"
    LET gar_tablas_rev[6].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

END FUNCTION

#------------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del reverso   #
#------------------------------------------------------------------------------#
FUNCTION f_act_bitacora(pi_folio)

    DEFINE pi_folio     LIKE ret_bitacora_rev.folio

    DEFINE ls_ind_rev   LIKE ret_bitacora_rev.id_rev

    DEFINE
        li_cont       SMALLINT

    -- -----------------------------------------------------------------------------

    LET gr_bitacora.folio       = pi_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   ls_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio    = gr_bitacora.folio

    FOR li_cont = 1 TO 6
        IF gar_tablas_rev[li_cont].num_regs > 0 THEN
            INSERT INTO ret_tablas_rev
            VALUES (ls_ind_rev                  ,
                    gr_bitacora.folio           ,
                    gar_tablas_rev[li_cont].*
                   )
        END IF
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_ins_recepcion : Carga en la tabla temporal los valores de la recepcion  #
#                   del archivo de notificacion                             #
#---------------------------------------------------------------------------#
FUNCTION f_ins_recepcion(pr_encab)

    DEFINE pr_encab RECORD
        nom_archivo     LIKE ret_recepcion.nom_archivo
    END RECORD

    DEFINE lr_recepcion RECORD LIKE ret_recepcion.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_recepcion.* TO NULL

    LET lr_recepcion.folio              = 1
    LET lr_recepcion.tipo_operacion     = gs_tipo_op
    LET lr_recepcion.fecha_recepcion    = HOY
    LET lr_recepcion.hora_recepcion     = CURRENT HOUR TO SECOND
    LET lr_recepcion.nom_archivo        = pr_encab.nom_archivo
    LET lr_recepcion.tot_registros      = 0
    LET lr_recepcion.usuario            = gc_usuario
    LET lr_recepcion.estado             = gr_edo.recibido
    LET lr_recepcion.mot_rechazo_01     = 0
    LET lr_recepcion.mot_rechazo_02     = 0
    LET lr_recepcion.mot_rechazo_03     = 0

    INSERT INTO tmp_recepcion
    VALUES(lr_recepcion.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de la operacion 60                                  #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro, pi_consec_tmp)

    DEFINE
        pc_registro         CHAR(900)

    DEFINE
        pi_consec_tmp       INTEGER

    DEFINE lr_vivienda_tmp      RECORD LIKE ret_notifica_vivienda.*
    DEFINE lr_montos_viv_tmp    RECORD LIKE ret_ajuste_vivienda_nss.*

    DEFINE
        ls_edo_recep        ,
        ls_aceptado         SMALLINT

    DEFINE
        lc_fechas           CHAR(10) ,
        lc_mto_16           CHAR(16)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_vivienda_tmp.* TO NULL
    INITIALIZE lr_montos_viv_tmp.* TO NULL

    -- Detalle de datos del trabajador

    LET lr_vivienda_tmp.folio_carga             = 1 -- Folio temporal
    LET lr_vivienda_tmp.nss                     = pc_registro[046,056]
    LET lr_vivienda_tmp.consecutivo             = pi_consec_tmp

    LET lr_vivienda_tmp.folio_notifica          = pc_registro[007,035]
    LET lr_vivienda_tmp.folio_operacion         = pc_registro[036,045]
    LET lr_vivienda_tmp.clabe                   = pc_registro[057,074]
    LET lr_vivienda_tmp.grupo_trabajador        = pc_registro[075,078]
    LET lr_vivienda_tmp.sec_pension             = pc_registro[079,080]

    LET lr_vivienda_tmp.regimen                 = pc_registro[081,082]
    LET lr_vivienda_tmp.tipo_retiro             = pc_registro[083,083]
    LET lr_vivienda_tmp.tipo_seguro             = pc_registro[084,085]
    LET lr_vivienda_tmp.tipo_pension            = pc_registro[086,087]
    LET lr_vivienda_tmp.tipo_prestacion         = pc_registro[088,089]
    LET lr_vivienda_tmp.semanas_cotizadas       = pc_registro[090,093]

    LET lr_vivienda_tmp.nombre                  = pc_registro[094,133]
    LET lr_vivienda_tmp.paterno                 = pc_registro[134,173]
    LET lr_vivienda_tmp.materno                 = pc_registro[174,213]
    LET lr_vivienda_tmp.rfc                     = pc_registro[214,226]
    LET lr_vivienda_tmp.curp                    = pc_registro[227,244]

    LET lr_vivienda_tmp.entidad                 = pc_registro[245,246]
    LET lr_vivienda_tmp.id_benef                = pc_registro[247,247]

    LET lr_vivienda_tmp.nombre_benef            = pc_registro[248,287]
    LET lr_vivienda_tmp.paterno_benef           = pc_registro[288,327]
    LET lr_vivienda_tmp.materno_benef           = pc_registro[328,367]
    LET lr_vivienda_tmp.rfc_benef               = pc_registro[368,380]
    LET lr_vivienda_tmp.curp_benef              = pc_registro[381,398]

    LET lr_vivienda_tmp.comentarios             = pc_registro[497,623]
    LET lr_vivienda_tmp.referencia_pago         = pc_registro[632,651]
    LET lr_vivienda_tmp.observaciones           = pc_registro[652,778]

    -- El formato para la fecha de captura es "DDMMAAAA"
    LET lc_fechas                               = pc_registro[781,782],"/",
                                                  pc_registro[779,780],"/",
                                                  pc_registro[783,786]
    LET lr_vivienda_tmp.fecha_captura           = lc_fechas

    LET lr_vivienda_tmp.indicador               = pc_registro[787,787]
    LET lr_vivienda_tmp.diag_infonavit          = pc_registro[788,790]

    LET lr_vivienda_tmp.usuario_carga           = gc_usuario
    LET lr_vivienda_tmp.fecha_carga             = TODAY
    LET lr_vivienda_tmp.hora_carga              = CURRENT HOUR TO SECOND
    LET lr_vivienda_tmp.estado                  = gr_edo.capturado

    INSERT INTO tmp_notifica_vivienda
    VALUES(lr_vivienda_tmp.*)

    -- Detalle de montos de vivienda

    LET lr_montos_viv_tmp.nss                   = lr_vivienda_tmp.nss
    LET lr_montos_viv_tmp.consecutivo           = lr_vivienda_tmp.consecutivo
    LET lr_montos_viv_tmp.folio                 = 1 -- Folio temporal

    LET lr_montos_viv_tmp.viv_92_aivs               = 0
    LET lr_montos_viv_tmp.viv_92_aivs_cta           = 0
    LET lr_montos_viv_tmp.viv_92_aivs_dif           = 0
    LET lr_montos_viv_tmp.viv_97_aivs               = 0
    LET lr_montos_viv_tmp.viv_97_aivs_cta           = 0
    LET lr_montos_viv_tmp.viv_97_aivs_dif           = 0
    LET lr_montos_viv_tmp.viv_92_pesos              = 0
    LET lr_montos_viv_tmp.viv_92_pesos_cta          = 0
    LET lr_montos_viv_tmp.viv_92_pesos_dif          = 0
    LET lr_montos_viv_tmp.viv_97_pesos              = 0
    LET lr_montos_viv_tmp.viv_97_pesos_cta          = 0
    LET lr_montos_viv_tmp.viv_97_pesos_dif          = 0
    LET lr_montos_viv_tmp.otros_importes            = 0
    LET lr_montos_viv_tmp.neto_depositado           = 0

    LET lc_mto_16                               = pc_registro[399,411], ".",
                                                  pc_registro[412,413]
    LET lr_montos_viv_tmp.viv_92_aivs           = lc_mto_16
    LET lr_montos_viv_tmp.viv_92_aivs_inf       = lr_montos_viv_tmp.viv_92_aivs

    LET lc_mto_16                               = pc_registro[414,426], ".",
                                                  pc_registro[427,428]
    LET lr_montos_viv_tmp.viv_97_aivs           = lc_mto_16
    LET lr_montos_viv_tmp.viv_97_aivs_inf       = lr_montos_viv_tmp.viv_97_aivs

    LET lc_mto_16                               = pc_registro[437,449], ".",
                                                  pc_registro[450,451]
    LET lr_montos_viv_tmp.viv_92_pesos          = lc_mto_16
    LET lr_montos_viv_tmp.viv_92_pesos_inf      = lr_montos_viv_tmp.viv_92_pesos

    LET lc_mto_16                               = pc_registro[452,464], ".",
                                                  pc_registro[465,466]
    LET lr_montos_viv_tmp.viv_97_pesos          = lc_mto_16
    LET lr_montos_viv_tmp.viv_97_pesos_inf      = lr_montos_viv_tmp.viv_97_pesos

    LET lc_mto_16                               = pc_registro[467,479], ".",
                                                  pc_registro[480,481]
    LET lr_montos_viv_tmp.otros_importes        = lc_mto_16

    LET lc_mto_16                               = pc_registro[482,494], ".",
                                                  pc_registro[495,496]
    LET lr_montos_viv_tmp.neto_depositado       = lc_mto_16

    -- El formato para la fecha de pago es "DDMMAAAA"
    LET lc_fechas                               = pc_registro[626,627],"/",
                                                  pc_registro[624,625],"/",
                                                  pc_registro[628,631]
    LET lr_montos_viv_tmp.fecha_pago            = lc_fechas

    -- El formato para la fecha valor vivienda es "DDMMAAAA"
    LET lc_fechas                               = pc_registro[431,432],"/",
                                                  pc_registro[429,430],"/",
                                                  pc_registro[433,436]
    LET lr_montos_viv_tmp.fecha_id_aivs         = lc_fechas

    LET lr_montos_viv_tmp.estado                = gr_edo.capturado

    LET lr_montos_viv_tmp.precio_accion         = f_lib_obten_precio_accion(lr_montos_viv_tmp.fecha_id_aivs, 11)

    INSERT INTO tmp_ajuste_vivienda_nss
    VALUES(lr_montos_viv_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_tot_registros : Carga en la tabla temporal el numero de registros   #
#                       procesados                                          #
#---------------------------------------------------------------------------#
FUNCTION f_act_tot_registros(pi_num_regs)

    DEFINE pi_num_regs LIKE ret_recepcion.tot_registros

    -- -----------------------------------------------------------------------------

    UPDATE tmp_recepcion
    SET    tot_registros = pi_num_regs
    WHERE  folio         = 1

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle : Realiza las validaciones para el proceso notificacion  #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle(ps_error, pi_id_error, pr_tramite_temp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pr_tramite_temp RECORD LIKE ret_notifica_vivienda.*

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
    LET lr_error.nss          = pr_tramite_temp.nss
    LET lr_error.curp         = pr_tramite_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"

    --- Valida que el NSS no sea nulo o que venga a 11 posiciones ---
    IF (pr_tramite_temp.nss IS NULL) OR (LENGTH(pr_tramite_temp.nss) <> 11) THEN
        LET lr_error.nom_campo    = "nss"
        LET lr_error.valor_campo  = pr_tramite_temp.nss
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_ins_bitacora_err(lr_error.*)
    END IF

    --- Valida que el nombre no sea nulo ---
    IF (pr_tramite_temp.nombre IS NULL) THEN
        LET lr_error.nom_campo    = "nombre"
        LET lr_error.valor_campo  = pr_tramite_temp.nombre
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_ins_bitacora_err(lr_error.*)
    END IF

    --- Valida que el paterno no sea nulo ---
    IF (pr_tramite_temp.paterno IS NULL) THEN
        LET lr_error.nom_campo    = "paterno"
        LET lr_error.valor_campo  = pr_tramite_temp.paterno
        LET lr_error.id_error     = 6
        LET ls_error              = 1
        CALL f_ins_bitacora_err(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_detalle_nulo : Valida que los campos obligatorios contengan      #
#                         informacion                                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_detalle_nulo(ps_error, pi_id_error, pr_tramite_temp, pr_ajuste_tmp)

    DEFINE
        ps_error        SMALLINT

    DEFINE pi_id_error LIKE ret_bitacora_error.id_proceso

    DEFINE pr_tramite_temp  RECORD LIKE ret_notifica_vivienda.*
    DEFINE pr_ajuste_tmp    RECORD LIKE ret_ajuste_vivienda_nss.*

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
    LET lr_error.nss          = pr_tramite_temp.nss
    LET lr_error.curp         = pr_tramite_temp.curp
    LET lr_error.tipo_campo   = "REGISTRO"
    LET lr_error.id_error     = 6

    IF pr_ajuste_tmp.fecha_pago IS NULL THEN
        LET lr_error.nom_campo    = "fecha_pago"
        LET lr_error.valor_campo  = pr_ajuste_tmp.fecha_pago
        LET ls_error              = 1
        CALL f_ins_bitacora_err(lr_error.*)
    END IF

    IF pr_ajuste_tmp.neto_depositado IS NULL THEN
        LET lr_error.nom_campo    = "neto_depositado"
        LET lr_error.valor_campo  = pr_ajuste_tmp.neto_depositado
        LET ls_error              = 1
        CALL f_ins_bitacora_err(lr_error.*)
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_op61 : Genera el archivo plano de la operacion 61                #
#---------------------------------------------------------------------------#
FUNCTION f_genera_op61()

    DEFINE li_folio_carga LIKE ret_notifica_vivienda.folio_carga

    DEFINE lr_notifica_viv RECORD LIKE ret_notifica_vivienda.*
    DEFINE lr_ajuste_viv_nss RECORD LIKE ret_ajuste_vivienda_nss.*
    DEFINE lr_ret_envio RECORD LIKE ret_envio.*

    DEFINE lr_mto_pagado RECORD
        retiro_aivs_viv92       LIKE dis_cuenta.monto_en_acciones   ,
        retiro_pesos_viv92      LIKE dis_cuenta.monto_en_pesos      ,
        interes_aivs_viv92      LIKE dis_cuenta.monto_en_acciones   ,
        interes_pesos_viv92     LIKE dis_cuenta.monto_en_pesos      ,
        retiro_aivs_viv97       LIKE dis_cuenta.monto_en_acciones   ,
        retiro_pesos_viv97      LIKE dis_cuenta.monto_en_pesos      ,
        interes_aivs_viv97      LIKE dis_cuenta.monto_en_acciones   ,
        interes_pesos_viv97     LIKE dis_cuenta.monto_en_pesos      ,
        indicador               SMALLINT
    END RECORD

    DEFINE ls_consecutivo_disp LIKE ret_solicitud_tx.consecutivo

    DEFINE
        ls_movimiento           ,
        ls_envio                ,
        ls_desmarcas            ,
        ls_diag_registro        ,
        ls_procesa              SMALLINT

    DEFINE
        lc_nombre_op61          CHAR(200)   ,
        lc_nombre_ruta          CHAR(200)

    -- -----------------------------------------------------------------------------

    LET ls_envio            = TRUE

    LET lc_nombre_op61      = gc_usuario CLIPPED             ,
                              "_OP61_"                       ,
                              HOY USING "YYYYMMDD"

    LET lc_nombre_ruta      = gr_modulo.ruta_envio CLIPPED   ,
                              "/"                            ,
                              lc_nombre_op61 CLIPPED

    START REPORT rpt_op61   TO lc_nombre_ruta

    INITIALIZE lr_notifica_viv.*   TO NULL
    INITIALIZE lr_ajuste_viv_nss.* TO NULL

    LET lr_mto_pagado.retiro_aivs_viv92     = 0
    LET lr_mto_pagado.retiro_pesos_viv92    = 0
    LET lr_mto_pagado.interes_aivs_viv92    = 0
    LET lr_mto_pagado.interes_pesos_viv92   = 0
    LET lr_mto_pagado.retiro_aivs_viv97     = 0
    LET lr_mto_pagado.retiro_pesos_viv97    = 0
    LET lr_mto_pagado.interes_aivs_viv97    = 0
    LET lr_mto_pagado.interes_pesos_viv97   = 0

    CALL f_captura_folio_op61()
        RETURNING ls_procesa, li_folio_carga

    IF (ls_procesa = TRUE) THEN

        LET lr_ret_envio.tot_registros  = 0
        LET ls_desmarcas                = 0

        DISPLAY "REGISTROS DESMARCADOS    : ", ls_desmarcas AT 10, 17

        DECLARE cur_op61 CURSOR FOR
            SELECT *
            FROM   ret_notifica_vivienda
            WHERE  folio_carga  = li_folio_carga
            ORDER BY grupo_trabajador, nss

        FOREACH cur_op61 INTO lr_notifica_viv.*

            SELECT *
            INTO   lr_ajuste_viv_nss.*
            FROM   ret_ajuste_vivienda_nss
            WHERE  folio        = lr_notifica_viv.folio_carga
            AND    nss          = lr_notifica_viv.nss
            AND    consecutivo  = lr_notifica_viv.consecutivo

            -- Obtenemos los montos pagados para Vivienda 92
            -- RETIRO VIV 92
            SELECT NVL(SUM(monto_en_acciones) * -1, 0)  ,
                   NVL(SUM(monto_en_pesos) * -1, 0)
            INTO   lr_mto_pagado.retiro_aivs_viv92      ,
                   lr_mto_pagado.retiro_pesos_viv92
            FROM   dis_cuenta
            WHERE  folio            = lr_notifica_viv.folio_carga
            AND    nss              = lr_notifica_viv.nss
            AND    consecutivo_lote = lr_notifica_viv.consecutivo
            AND    subcuenta        = 8
            AND    tipo_movimiento  = gr_movimiento.retiro

            -- INTERESES VIV 92
            SELECT NVL(SUM(monto_en_acciones) * -1, 0)   ,
                   NVL(SUM(monto_en_pesos) * -1, 0)
            INTO   lr_mto_pagado.interes_aivs_viv92     ,
                   lr_mto_pagado.interes_pesos_viv92
            FROM   dis_cuenta
            WHERE  folio            = lr_notifica_viv.folio_carga
            AND    nss              = lr_notifica_viv.nss
            AND    consecutivo_lote = lr_notifica_viv.consecutivo
            AND    subcuenta        = 8
            AND    tipo_movimiento  = gr_movimiento.cancela_int

            -- Obtenemos los montos pagados para Vivienda 97
            -- RETIRO VIV 97
            SELECT NVL(SUM(monto_en_acciones) * -1, 0)  ,
                   NVL(SUM(monto_en_pesos) * -1, 0)
            INTO   lr_mto_pagado.retiro_aivs_viv97      ,
                   lr_mto_pagado.retiro_pesos_viv97
            FROM   dis_cuenta
            WHERE  folio            = lr_notifica_viv.folio_carga
            AND    nss              = lr_notifica_viv.nss
            AND    consecutivo_lote = lr_notifica_viv.consecutivo
            AND    subcuenta        = 4
            AND    tipo_movimiento  = gr_movimiento.retiro

            -- INTERESES VIV 97
            SELECT NVL(SUM(monto_en_acciones) * -1, 0)   ,
                   NVL(SUM(monto_en_pesos) * -1, 0)
            INTO   lr_mto_pagado.interes_aivs_viv97     ,
                   lr_mto_pagado.interes_pesos_viv97
            FROM   dis_cuenta
            WHERE  folio            = lr_notifica_viv.folio_carga
            AND    nss              = lr_notifica_viv.nss
            AND    consecutivo_lote = lr_notifica_viv.consecutivo
            AND    subcuenta        = 4
            AND    tipo_movimiento  = gr_movimiento.cancela_int

#CPL-2352 INI
            IF (lr_notifica_viv.indicador = "A") THEN
                #LET lr_mto_pagado.indicador = 1
                CASE
                    -- Se registraron pagos de vivienda 97 y vivienda 92
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 ) <> 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 ) <> 0 )
                        LET lr_mto_pagado.indicador = 1

                    -- Se registro pago de vivienda 92 solamente
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 )  = 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 ) <> 0 )

                        IF (lr_ajuste_viv_nss.viv_97_aivs = 0) THEN
                            LET lr_mto_pagado.indicador = 1
                        ELSE
                            LET lr_mto_pagado.indicador = 2
                        END IF

                    -- Se registro pago de vivienda 97 solamente
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 ) <> 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 )  = 0 )

                        IF (lr_ajuste_viv_nss.viv_92_aivs = 0) THEN
                            LET lr_mto_pagado.indicador = 1
                        ELSE
                            LET lr_mto_pagado.indicador = 3
                        END IF

                    -- No hay ningun pago de vivienda registrado
                    OTHERWISE
                        LET lr_mto_pagado.indicador = 4

                END CASE
#CPL-2352 FIN
            ELSE
                CASE
                    -- Se registraron pagos de vivienda 97 y vivienda 92
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 ) <> 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 ) <> 0 )
                        LET lr_mto_pagado.indicador = 1

                    -- Se registro pago de vivienda 92 solamente
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 )  = 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 ) <> 0 )
                        LET lr_mto_pagado.indicador = 2

                    -- Se registro pago de vivienda 97 solamente
                    WHEN ( (lr_mto_pagado.retiro_aivs_viv97 + lr_mto_pagado.interes_aivs_viv97 ) <> 0 ) AND
                         ( (lr_mto_pagado.retiro_aivs_viv92 + lr_mto_pagado.interes_aivs_viv92 )  = 0 )
                        LET lr_mto_pagado.indicador = 3

                    -- No hay ningun pago de vivienda registrado
                    OTHERWISE
                        LET lr_mto_pagado.indicador = 4

                END CASE
            END IF

            -- Si el indicador es B o C, se rechaza la solicitud y se coloca el indicador en 1
            -- para que se realice la desmarca de la cuenta
            IF lr_notifica_viv.indicador <> "A" THEN

                UPDATE ret_notifica_vivienda
                SET    estado       = gr_edo.rechazado
                WHERE  nss          = lr_notifica_viv.nss
                AND    consecutivo  = lr_notifica_viv.consecutivo
                AND    folio_carga  = li_folio_carga
                AND    estado       = gr_edo.liquidado

               LET lr_mto_pagado.indicador = 1
            END IF

            -- Se envian solo los registros aceptados
            IF (lr_notifica_viv.indicador = "A") THEN
                LET ls_envio    = TRUE
            ELSE
                LET ls_envio    = FALSE
            END IF 

            IF (ls_envio = TRUE) THEN

                OUTPUT TO REPORT rpt_op61(lr_notifica_viv.*                 ,
                                          lr_ajuste_viv_nss.*               ,
                                          lr_mto_pagado.retiro_aivs_viv92   ,
                                          lr_mto_pagado.retiro_pesos_viv92  ,
                                          lr_mto_pagado.retiro_aivs_viv97   ,
                                          lr_mto_pagado.retiro_pesos_viv97  ,
                                          lr_mto_pagado.indicador
                                         )
            END IF

            LET lr_ret_envio.tot_registros  = lr_ret_envio.tot_registros + 1

            SELECT movimiento
            INTO   ls_movimiento
            FROM   tab_retiro
            WHERE  tipo_retiro  = lr_notifica_viv.tipo_retiro

            -- Para los registros que provienen de ventanilla INFONAVIT (0101) se debe
            -- tener diagnostico 400 para realizar la desmarca
            IF (lr_notifica_viv.grupo_trabajador = 0101) THEN

                -- La condicion para desmarcar las cuentas es que se tenga indicador 1
                IF (lr_mto_pagado.indicador = 1) THEN

                    SELECT diag_registro
                    INTO   ls_diag_registro
                    FROM   ret_solicitud_tx
                    WHERE  folio    = li_folio_carga
                    AND    nss      = lr_notifica_viv.nss

                    IF ls_diag_registro = 400 THEN

                        CALL f_desmarca_cuenta(lr_notifica_viv.nss          ,
                                               lr_notifica_viv.consecutivo  ,
                                               ls_movimiento
                                              )
                        LET ls_desmarcas = ls_desmarcas + 1
                    END IF
                END IF -- Indicador = 1
            ELSE
                -- CPL-1834
                -- Se desmarcan los registros que provienen de ventanilla AFORE (0201)
                -- utilizando el consecutivo de la solicitud de retiros IMSS
                SELECT consecutivo
                INTO   ls_consecutivo_disp
                FROM   ret_solicitud_tx
                WHERE  nss          = lr_notifica_viv.nss
                AND    tipo_retiro  = lr_notifica_viv.tipo_retiro
                AND    consecutivo IN (SELECT correlativo   
                                       FROM   cta_act_marca 
                                       WHERE  nss       = lr_notifica_viv.nss
                                       AND    marca_cod = ls_movimiento
                                      )

                CALL f_desmarca_cuenta(lr_notifica_viv.nss  ,
                                       ls_consecutivo_disp  ,
                                       ls_movimiento
                                      )
                
                LET ls_desmarcas = ls_desmarcas + 1
            END IF

            DISPLAY "REGISTROS DESMARCADOS    : ", ls_desmarcas AT 10, 17

            INITIALIZE lr_notifica_viv.*   TO NULL
            INITIALIZE lr_ajuste_viv_nss.* TO NULL

            LET lr_mto_pagado.retiro_aivs_viv92     = 0
            LET lr_mto_pagado.retiro_pesos_viv92    = 0
            LET lr_mto_pagado.interes_aivs_viv92    = 0
            LET lr_mto_pagado.interes_pesos_viv92   = 0
            LET lr_mto_pagado.retiro_aivs_viv97     = 0
            LET lr_mto_pagado.retiro_pesos_viv97    = 0
            LET lr_mto_pagado.interes_aivs_viv97    = 0
            LET lr_mto_pagado.interes_pesos_viv97   = 0

        END FOREACH

        LET lr_ret_envio.folio          = li_folio_carga
        LET lr_ret_envio.tipo_operacion = 61
        LET lr_ret_envio.fecha_envio    = HOY
        LET lr_ret_envio.hora_envio     = CURRENT HOUR TO SECOND
        LET lr_ret_envio.nom_archivo    = lc_nombre_op61
        LET lr_ret_envio.usuario        = gc_usuario
        LET lr_ret_envio.estado         = gr_edo.enviado

        INSERT INTO ret_envio
        VALUES (lr_ret_envio.*)

        FINISH REPORT rpt_op61

        CALL f_lib_borra_lineas(gr_modulo.ruta_envio, lc_nombre_op61)

        DISPLAY "ARCHIVO GENERADO CON EL NOMBRE " AT 14, 5
        DISPLAY lc_nombre_ruta AT 15, 5

        CALL f_lib_error_msg("ARCHIVO TERMINADO")
    END IF

    DISPLAY "                                               " AT 10, 17
    CLOSE WINDOW win_op61

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio_op61 : Captura el folio de la op.60 liquidada, con el que #
#                        se generara la operacion 61                        #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio_op61()

    DEFINE li_folio_carga LIKE ret_notifica_vivienda.folio_carga

    DEFINE
        ls_procesa      INTEGER

    -- -----------------------------------------------------------------------------

    LET ls_procesa = TRUE

    SELECT MAX(folio_carga)
    INTO   li_folio_carga
    FROM   ret_notifica_vivienda
    WHERE  estado   = gr_edo.liquidado

    OPEN WINDOW win_op61 AT 4,4 WITH FORM "RETP0103" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETP010               GENERACION DE LA OPERACION 61                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    -- Captura Del Folio
    INPUT BY NAME li_folio_carga WITHOUT DEFAULTS

        AFTER FIELD li_folio_carga
            IF li_folio_carga IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_notifica_vivienda
            WHERE  folio_carga      = li_folio_carga
            AND    estado           = gr_edo.liquidado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA LIQUIDADO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que no se haya generado una operacion previa
            SELECT "OK"
            FROM   ret_envio
            WHERE  folio        = li_folio_carga
            AND    estado       = gr_edo.enviado
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND THEN
                CALL f_lib_error_msg("YA SE GENERO UN ARCHIVO PARA ESTE FOLIO")
                NEXT FIELD li_folio_carga
            END IF

        ON KEY (ESC)
            IF (li_folio_carga IS NULL) THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que el folio exista y este cargado
            SELECT "OK"
            FROM   ret_notifica_vivienda
            WHERE  folio_carga      = li_folio_carga
            AND    estado           = gr_edo.liquidado
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA LIQUIDADO")
                NEXT FIELD li_folio_carga
            END IF

            -- Verifica que no se haya generado una operacion previa
            SELECT "OK"
            FROM   ret_envio
            WHERE  folio    = li_folio_carga
            AND    estado   = gr_edo.enviado
            GROUP BY 1

            IF (SQLCA.SQLCODE <> NOTFOUND) THEN
                CALL f_lib_error_msg("YA SE GENERO UN ARCHIVO PARA ESTE FOLIO")
                NEXT FIELD li_folio_carga
            END IF

            IF f_lib_pregunta("¿DESEA GENERAR LA OPERACION 61? (S/N): " ) = TRUE THEN
                LET ls_procesa = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_procesa = FALSE
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = FALSE
            EXIT INPUT

    END INPUT

    RETURN ls_procesa, li_folio_carga

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_envio : Genera el reverso de la generacion de la operacion 61   #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_envio()

    DEFINE li_folio_carga LIKE ret_notifica_vivienda.folio_carga

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    LET gr_bitacora.usuario         = gc_usuario
    LET gr_bitacora.programa        = "RETP010"
    LET gr_bitacora.desc_tramite    = "ENVIO OP. 61"
    LET gr_bitacora.fecha_ini       = HOY
    LET gr_bitacora.hora_ini        = CURRENT HOUR TO SECOND

    CALL f_captura_folio_reverso(61)
        RETURNING ls_procesa, li_folio_carga

    IF ls_procesa THEN
        CALL f_ejecuta_reverso_envio(li_folio_carga)
        CALL f_act_bitacora(li_folio_carga)
        CALL f_lib_error_msg("REVERSO TERMINADO")
    END IF

    CLOSE WINDOW win_reverso_vent

END FUNCTION

#-------------------------------------------------------------------------------#
# f_ejecuta_reverso_envio : Ejecuta el reverso de la carga de la op. 61         #
#-------------------------------------------------------------------------------#
FUNCTION f_ejecuta_reverso_envio(pi_folio)

    DEFINE
        pi_folio           INTEGER

    DEFINE lr_desmarca RECORD
        nss         LIKE ret_solicitud_tx.nss           ,
        marca_cod   LIKE cta_his_marca.marca_cod        ,
        consec      LIKE ret_solicitud_tx.consecutivo   ,
        fecha_ini   LIKE cta_his_marca.fecha_ini
    END RECORD

    DEFINE
        li_num_afect        INTEGER

    DEFINE
        lc_prepare          CHAR(100)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_desmarca.* TO NULL
    LET li_num_afect =  0

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare

    DECLARE cur_rev_desmarca CURSOR FOR
        SELECT A.nss            ,
               B.marca_cod      ,
               A.consecutivo    ,
               B.fecha_ini
        FROM   ret_notifica_vivienda A ,
               cta_his_marca B
        WHERE  A.folio_carga        = pi_folio
        AND    A.nss                = B.nss
        AND    A.consecutivo        = B.correlativo
        AND    A.estado             = gr_edo.liquidado
        AND    B.fecha_fin IS NOT NULL

    FOREACH cur_rev_desmarca INTO lr_desmarca.*

        EXECUTE eje_rev_desmarca USING lr_desmarca.*

        IF SQLCA.SQLCODE = 0 THEN
            LET li_num_afect = li_num_afect + 1
        END IF

    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION              : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_envio
    WHERE  folio    = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_envio                 : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[2].tabla     = "ret_envio"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

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
# f_ins_bitacora_err : Inserta el registro en la bitacora de errores de     #
#                      carga                                                #
#---------------------------------------------------------------------------#
FUNCTION f_ins_bitacora_err(pr_error)

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
    LET lr_bitacora.programa    = "RETP010"
    LET lr_bitacora.nom_archivo = gr_dat.nom_archivo_det
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
# f_marca_cuenta : Ejecuta el script para realizar la marca de la cuenta    #
#---------------------------------------------------------------------------#
FUNCTION f_marca_cuenta(pr_marca)

    DEFINE pr_marca RECORD
        nss         LIKE ret_solicitud_tx.nss           ,
        consec      LIKE ret_solicitud_tx.consecutivo   ,
        movimiento  LIKE cta_act_marca.marca_cod
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        cod_rechazo     SMALLINT ,
        marca_causa     SMALLINT ,
        fec_causa       DATE
    END RECORD

    DEFINE lr_resultado RECORD
        cod_rech            SMALLINT    ,
        desc_rechazo        CHAR(050)
    END RECORD

    DEFINE
        ls_marca_res        SMALLINT

    DEFINE
        lc_prepare          CHAR(100)

    -- ---------------------------------------------------------------------------------

    LET lr_resultado.desc_rechazo   = " "
    LET lr_dat.edo_marca            = 0
    LET lr_dat.cod_rechazo          = 0
    LET lr_dat.marca_causa          = 0
    INITIALIZE lr_dat.fec_causa TO NULL

    ----- MARCAJE -----
    LET lc_prepare = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM lc_prepare

    EXECUTE eje_marca USING pr_marca.nss            , -- nss
                            pr_marca.movimiento     , -- marca entrante
                            pr_marca.consec         , -- consecutivo
                            lr_dat.edo_marca        , -- estado_marco
                            lr_dat.cod_rechazo      , -- codigo de rechazo
                            lr_dat.marca_causa      , -- marca_causa
                            lr_dat.fec_causa        , -- fecha_causa
                            gc_usuario                -- usuario
                      INTO  ls_marca_res            ,
                            lr_resultado.cod_rech

    -- Si existe un error en la marca, regresamos el codigo y el mensaje de error
    IF (lr_resultado.cod_rech <> 0) THEN
        SELECT rechazo_desc
        INTO   lr_resultado.desc_rechazo
        FROM   tab_rch_marca
        WHERE  rechazo_cod = lr_resultado.cod_rech
    END IF

    RETURN lr_resultado.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la cta #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo   ,
        movimiento      LIKE cta_act_marca.marca_cod
    END RECORD

   DEFINE lr_edo_des RECORD
        edo_marca       SMALLINT    ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        lc_prepare          CHAR(100)

    -- -----------------------------------------------------------------------------

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lr_edo_des.edo_marca    = 0
    LET lr_edo_des.marca_causa  = 0

    EXECUTE eje_desmarca USING pr_desmarca.nss          ,   -- nss
                               pr_desmarca.movimiento   ,   -- marca entrante
                               pr_desmarca.consecutivo  ,   -- consecutivo
                               lr_edo_des.edo_marca     ,   -- estado_marco
                               lr_edo_des.marca_causa   ,   -- marca_causa
                               gc_usuario                   -- usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_val_marca : valida si el nss tiene una marca especifica                 #
#---------------------------------------------------------------------------#
FUNCTION f_val_marca(pc_nss, ps_marca)
DEFINE    pc_nss       CHAR(11),
          ps_marca     SMALLINT

   SELECT "OK" FROM safre_af:cta_act_marca
   WHERE nss = pc_nss
   AND marca_cod = ps_marca
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN   #--Existe la marca
      RETURN 1
   END IF

 RETURN 0

END FUNCTION 


#---------------------------------------------------------------------------#
# f_valida_mto_subcuentas : Determina el grupo de subcuentas a pagar y      #
#                           valida que el NSS tenga saldo CPL-2312          #
#---------------------------------------------------------------------------#
FUNCTION f_valida_mto_subcuentas(pr_matriz_derecho)

    DEFINE pr_matriz_derecho RECORD
        nss                 LIKE ret_notifica_vivienda.nss          ,
        tipo_retiro         LIKE ret_matriz_derecho.tipo_retiro     ,
        regimen             LIKE ret_matriz_derecho.regimen         ,
        tipo_seguro         LIKE ret_matriz_derecho.tipo_seguro     ,
        tipo_pension        LIKE ret_matriz_derecho.tipo_pension    ,
        tipo_prestacion     LIKE ret_matriz_derecho.tipo_prestacion
    END RECORD

    DEFINE lr_mtos_valida RECORD
        grupo           SMALLINT    ,
        pago            SMALLINT
    END RECORD

    DEFINE
        lc_cad_subcuenta        CHAR(150)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_mtos_valida.* TO NULL

    SELECT grupo
    INTO   lr_mtos_valida.grupo
    FROM   ret_matriz_derecho
    WHERE  tipo_retiro      = pr_matriz_derecho.tipo_retiro
    AND    regimen          = pr_matriz_derecho.regimen
    AND    tipo_seguro      = pr_matriz_derecho.tipo_seguro
    AND    tipo_pension     = pr_matriz_derecho.tipo_pension
    AND    tipo_prestacion  = pr_matriz_derecho.tipo_prestacion

    -- Recuperamos el grupo que corresponde a la cadena de derechos, pero
    -- excluyendo las subcuentas de vivienda
    LET lc_cad_subcuenta = f_lib_genera_cadena_subcuentas(lr_mtos_valida.grupo, FALSE)

    SELECT grupo
    INTO   lr_mtos_valida.grupo
    FROM   tab_agrupa_subcta_cad
    WHERE  cad_subcta = lc_cad_subcuenta
    GROUP BY 1

    LET lr_mtos_valida.pago = f_valida_saldo_grupo(pr_matriz_derecho.nss    ,
                                                   lr_mtos_valida.grupo
                                                  )

    RETURN lr_mtos_valida.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_saldo_grupo : Antes de insertar la captura, verifica que el      #
#                        trabajador tenga saldo en las subcuentas del grupo #
#                        que se pagara. En caso que no tenga saldo en       #
#                        todas sus subcuentas, se enviara una bandera       #
#                        para cancelar la captura   CPL-2312                #
#---------------------------------------------------------------------------#
FUNCTION f_valida_saldo_grupo(pr_valida)

    DEFINE pr_valida RECORD
        nss             LIKE ret_notifica_vivienda.nss  ,
        grupo           SMALLINT
    END RECORD

    DEFINE lr_saldos RECORD
        subcuenta           SMALLINT        ,
        acciones            DECIMAL(22,6)   ,
        pesos               DECIMAL(22,6)
    END RECORD

    DEFINE lr_obten_saldo RECORD
        nss             LIKE dis_cuenta.nss         ,
        subcuenta       LIKE dis_cuenta.subcuenta   ,
        grupo           SMALLINT                    ,
        fecha_saldo     DATE
    END RECORD

    DEFINE
        ld_monto_tot            DECIMAL(22,6)

    DEFINE
        ls_con_saldo            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE
        lr_saldos.*         ,
        lr_obten_saldo.*    TO NULL

    LET ls_con_saldo                = TRUE
    LET ld_monto_tot                = 0

    LET lr_obten_saldo.nss          = pr_valida.nss
    LET lr_obten_saldo.grupo        = 0
    LET lr_obten_saldo.fecha_saldo  = HOY

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_saldo_subcta
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_saldo_subcta(
        subcuenta       SMALLINT        ,
        acciones        DECIMAL(22,6)   ,
        pesos           DECIMAL(22,6)
    )

    DECLARE cur_saldos CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = pr_valida.grupo
        AND    subcuenta > 0
        ORDER BY 1

    -- Obtenemos el saldo al dia de cada subcuenta dentro del grupo
    FOREACH cur_saldos INTO lr_obten_saldo.subcuenta

        LET lr_saldos.subcuenta = lr_obten_saldo.subcuenta

        CALL f_obten_saldo_dia(lr_obten_saldo.*)
            RETURNING lr_saldos.acciones    ,
                      lr_saldos.pesos

        IF (lr_saldos.acciones > 0) THEN
            INSERT INTO tmp_saldo_subcta
            VALUES(lr_saldos.*)
        END IF

    END FOREACH

    -- Si la suma total del monto a pagar es cero, se manda cancelacion
    SELECT NVL(SUM(acciones), 0)
    INTO   ld_monto_tot
    FROM   tmp_saldo_subcta

    IF ld_monto_tot <= 0 THEN
        LET ls_con_saldo = FALSE
    END IF

    RETURN ls_con_saldo

END FUNCTION


#---------------------------------------------------------------------------#
# f_obten_saldo_dia : Obtiene el saldo al dia para un nss a una fecha dada  #
#                                                                           #
#                     Si la funcion se manda llamar con subcuenta = 0 y     #
#                     grupo = 0, la funcion regresa el saldo al dia de toda #
#                     la cuenta individual         CPL-2312                 #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_dia(pr_saldo)

    DEFINE pr_saldo RECORD
        nss                 LIKE dis_cuenta.nss         ,
        subcuenta           LIKE dis_cuenta.subcuenta   ,
        grupo               SMALLINT                    ,
        fecha_saldo         DATE
    END RECORD

    DEFINE lr_ejecuta_saldo RECORD
        subcuenta   LIKE dis_provision.subcuenta            ,
        siefore     LIKE dis_provision.siefore              ,
        monto_acc   LIKE dis_provision.monto_en_acciones    ,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE lr_saldo_dia RECORD
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        lc_prepare          CHAR(400)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_ejecuta_saldo.* TO NULL

    LET lc_prepare                  = " "
    LET lr_saldo_dia.monto_acc      = 0
    LET lr_saldo_dia.monto_pes      = 0

    ----- SALDO AL DIA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE prp_saldo_dia FROM lc_prepare
    DECLARE cur_saldo_dia CURSOR FOR prp_saldo_dia

    FOREACH cur_saldo_dia USING pr_saldo.*
                          INTO  lr_ejecuta_saldo.*

        IF (lr_ejecuta_saldo.monto_acc IS NULL) THEN
            LET lr_ejecuta_saldo.monto_acc = 0
        END IF

        IF (lr_ejecuta_saldo.monto_pes IS NULL) THEN
            LET lr_ejecuta_saldo.monto_pes = 0
        END IF

        -- Si tiene saldos desinvertidos, se consideran como cero
        IF (lr_ejecuta_saldo.siefore = 10) THEN
            LET lr_ejecuta_saldo.monto_acc  = 0
            LET lr_ejecuta_saldo.monto_pes  = 0
        END IF

        LET lr_saldo_dia.monto_acc  = lr_saldo_dia.monto_acc + lr_ejecuta_saldo.monto_acc
        LET lr_saldo_dia.monto_pes  = lr_saldo_dia.monto_pes + lr_ejecuta_saldo.monto_pes

        INITIALIZE lr_ejecuta_saldo.* TO NULL

    END FOREACH

    RETURN lr_saldo_dia.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usaran en el proceso     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_arch_op60
        DROP TABLE tmp_notifica_vivienda
        DROP TABLE tmp_ajuste_vivienda_nss
        DROP TABLE tmp_recepcion
        DROP TABLE tmp_solicitud_tx
        DROP TABLE tmp_beneficiario
    WHENEVER ERROR STOP

    -- -----------------------------------------------------

    CREATE TEMP TABLE tmp_arch_op60
    (
    n_registros          CHAR(900)
    )

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_notifica_vivienda
    WHERE  1 = 0
    INTO TEMP tmp_notifica_vivienda

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_ajuste_vivienda_nss
    WHERE  1 = 0
    INTO TEMP tmp_ajuste_vivienda_nss

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_recepcion
    WHERE  1 = 0
    INTO TEMP tmp_recepcion

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_solicitud_tx
    WHERE  1 = 0
    INTO TEMP tmp_solicitud_tx

    -- -----------------------------------------------------

    SELECT *
    FROM   ret_beneficiario
    WHERE  1 = 0
    INTO TEMP tmp_beneficiario

    -- -----------------------------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_op61 : Genera el archivo plano de la operacion 61                     #
#---------------------------------------------------------------------------#
REPORT rpt_op61(pr_notifica_viv, pr_ajuste_viv_nss, pr_mto_pagado)

    DEFINE pr_notifica_viv RECORD LIKE ret_notifica_vivienda.*
    DEFINE pr_ajuste_viv_nss RECORD LIKE ret_ajuste_vivienda_nss.*

    DEFINE pr_mto_pagado RECORD
        aivs_viv92          LIKE dis_cuenta.monto_en_acciones   ,
        pesos_viv92         LIKE dis_cuenta.monto_en_pesos      ,
        aivs_viv97          LIKE dis_cuenta.monto_en_acciones   ,
        pesos_viv97         LIKE dis_cuenta.monto_en_pesos      ,
        indicador           SMALLINT
    END RECORD

    DEFINE lr_cadena RECORD
        viv_92_AIVS                 CHAR(15)    ,
        viv_97_AIVS                 CHAR(15)    ,
        viv_92_pesos                CHAR(15)    ,
        viv_97_pesos                CHAR(15)    ,
        otros_importes              CHAR(15)    ,
        impt_neto                   CHAR(15)    ,
        viv_92_AIVS_aplicado        CHAR(15)    ,
        viv_97_AIVS_aplicado        CHAR(15)    ,
        viv_92_pesos_aplicado       CHAR(15)    ,
        viv_97_pesos_aplicado       CHAR(15)
    END RECORD

    DEFINE
        lc_cad_16               CHAR(16)

    DEFINE
        ls_cont                 SMALLINT

    DEFINE
        lc_indicador_desmarca   CHAR(1)    #CPL-2104
    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   10000
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        FIRST PAGE HEADER
            LET ls_cont = 0

            PRINT
                COLUMN 001, "01"                                ,   -- Tipo registro
                COLUMN 003, "04"                                ,   -- Id de servicio
                COLUMN 005, "01"                                ,   -- Id operacion
                COLUMN 007, gs_codigo_afore USING "&&&"         ,   --  Entidad origen
                COLUMN 010, "03"                                ,   -- Cve entidad origen
                COLUMN 012, "001"                               ,   -- Entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"                ,   -- Fecha valor
                COLUMN 023, 841 SPACES                              -- Filler

        ON EVERY ROW
            LET ls_cont = ls_cont + 1
#CPL-2398 INI
            IF (pr_notifica_viv.grupo_trabajador = 0201) THEN  --Ventanilla afore
                LET lc_indicador_desmarca = "1"
            ELSE    --Grupo 0101  ventanilla infonavit
                SELECT "OK"
                FROM ret_solicitud_tx
                WHERE nss = pr_notifica_viv.nss
                AND consecutivo = pr_notifica_viv.consecutivo
                GROUP BY 1
                
                IF (SQLCA.SQLCODE = 0) THEN
                    LET lc_indicador_desmarca = "0"
                ELSE
                    LET lc_indicador_desmarca = "1"
                END IF
            END IF
#CPL-2398 FIN
            -- Si el indicador es diferente de A, se reportan los saldos en cero
            IF pr_notifica_viv.indicador <> "A" THEN
                LET pr_ajuste_viv_nss.viv_92_aivs       = 0
                LET pr_ajuste_viv_nss.viv_92_aivs_cta   = 0
                LET pr_ajuste_viv_nss.viv_92_aivs_dif   = 0
                LET pr_ajuste_viv_nss.viv_92_aivs_inf   = 0
                LET pr_ajuste_viv_nss.viv_97_aivs       = 0
                LET pr_ajuste_viv_nss.viv_97_aivs_cta   = 0
                LET pr_ajuste_viv_nss.viv_97_aivs_dif   = 0
                LET pr_ajuste_viv_nss.viv_97_aivs_inf   = 0
                LET pr_ajuste_viv_nss.viv_92_pesos      = 0
                LET pr_ajuste_viv_nss.viv_92_pesos_cta  = 0
                LET pr_ajuste_viv_nss.viv_92_pesos_dif  = 0
                LET pr_ajuste_viv_nss.viv_92_pesos_inf  = 0
                LET pr_ajuste_viv_nss.viv_97_pesos      = 0
                LET pr_ajuste_viv_nss.viv_97_pesos_cta  = 0
                LET pr_ajuste_viv_nss.viv_97_pesos_dif  = 0
                LET pr_ajuste_viv_nss.viv_97_pesos_inf  = 0
                LET pr_ajuste_viv_nss.otros_importes    = 0
                LET pr_ajuste_viv_nss.neto_depositado   = 0
            END IF

            -- Formateamos los montos enviados por INFONAVIT a cadenas
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.viv_92_aivs_inf IS NULL THEN
                LET pr_ajuste_viv_nss.viv_92_aivs_inf   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.viv_92_aivs_inf USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_92_AIVS   = lc_cad_16[01,13]  ,
                                          lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.viv_97_aivs_inf IS NULL THEN
                LET pr_ajuste_viv_nss.viv_97_aivs_inf   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.viv_97_aivs_inf USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_97_AIVS   = lc_cad_16[01,13]  ,
                                          lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.viv_92_pesos_inf IS NULL THEN
                LET pr_ajuste_viv_nss.viv_92_pesos_inf   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.viv_92_pesos_inf USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_92_pesos  = lc_cad_16[01,13]  ,
                                          lc_cad_16[15,16]

            ----
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.viv_97_pesos_inf IS NULL THEN
                LET pr_ajuste_viv_nss.viv_97_pesos_inf   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.viv_97_pesos_inf USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_97_pesos  = lc_cad_16[01,13]  ,
                                          lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.otros_importes IS NULL THEN
                LET pr_ajuste_viv_nss.otros_importes   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.otros_importes USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.otros_importes  = lc_cad_16[01,13]  ,
                                            lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_ajuste_viv_nss.neto_depositado IS NULL THEN
                LET pr_ajuste_viv_nss.neto_depositado   = 0
            END IF

            LET lc_cad_16   = pr_ajuste_viv_nss.neto_depositado USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.impt_neto       = lc_cad_16[01,13]  ,
                                            lc_cad_16[15,16]


            -- Formateamos los montos pagados a cadenas
            ----
            LET lc_cad_16   = " "

            IF pr_mto_pagado.aivs_viv92 IS NULL THEN
                LET pr_mto_pagado.aivs_viv92   = 0
            END IF

            LET lc_cad_16   = pr_mto_pagado.aivs_viv92 USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_92_AIVS_aplicado  = lc_cad_16[01,13]  ,
                                                  lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_mto_pagado.aivs_viv97 IS NULL THEN
                LET pr_mto_pagado.aivs_viv97   = 0
            END IF

            LET lc_cad_16   = pr_mto_pagado.aivs_viv97 USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_97_AIVS_aplicado  = lc_cad_16[01,13]  ,
                                                  lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_mto_pagado.pesos_viv92 IS NULL THEN
                LET pr_mto_pagado.pesos_viv92   = 0
            END IF

            LET lc_cad_16   = pr_mto_pagado.pesos_viv92 USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_92_pesos_aplicado  = lc_cad_16[01,13]  ,
                                                   lc_cad_16[15,16]
            ----
            LET lc_cad_16   = " "

            IF pr_mto_pagado.pesos_viv97 IS NULL THEN
                LET pr_mto_pagado.pesos_viv97   = 0
            END IF

            LET lc_cad_16   = pr_mto_pagado.pesos_viv97 USING "&&&&&&&&&&&&&.&&"
            LET lr_cadena.viv_97_pesos_aplicado  = lc_cad_16[01,13]  ,
                                                   lc_cad_16[15,16]

            PRINT
                COLUMN 001, "03"                                                ,
                COLUMN 003, "04"                                                ,
                COLUMN 005, "61"                                                ,
                COLUMN 007, pr_notifica_viv.folio_notifica                      ,
                COLUMN 036, pr_notifica_viv.folio_operacion  CLIPPED            ,
                COLUMN 046, pr_notifica_viv.nss                                 ,
                COLUMN 057, pr_notifica_viv.clabe                               ,
                COLUMN 075, pr_notifica_viv.grupo_trabajador                    ,
                COLUMN 079, pr_notifica_viv.sec_pension                         ,
                COLUMN 081, pr_notifica_viv.regimen                             ,
                COLUMN 083, pr_notifica_viv.tipo_retiro                         ,
                COLUMN 084, pr_notifica_viv.tipo_seguro                         ,
                COLUMN 086, pr_notifica_viv.tipo_pension                        ,
                COLUMN 088, pr_notifica_viv.tipo_prestacion                     ,
                COLUMN 090, pr_notifica_viv.semanas_cotizadas USING "&&&&"      ,
                COLUMN 094, pr_notifica_viv.nombre                              ,
                COLUMN 134, pr_notifica_viv.paterno                             ,
                COLUMN 174, pr_notifica_viv.materno                             ,
                COLUMN 214, pr_notifica_viv.rfc                                 ,
                COLUMN 227, pr_notifica_viv.curp                                ,
                COLUMN 245, pr_notifica_viv.entidad USING "&&"                  ,
                COLUMN 247, pr_notifica_viv.id_benef USING "&"                  ,
                COLUMN 248, pr_notifica_viv.nombre_benef                        ,
                COLUMN 288, pr_notifica_viv.paterno_benef                       ,
                COLUMN 328, pr_notifica_viv.materno_benef                       ,
                COLUMN 368, pr_notifica_viv.rfc_benef                           ,
                COLUMN 381, pr_notifica_viv.curp_benef                          ,
                COLUMN 399, lr_cadena.viv_92_AIVS                               ,
                COLUMN 414, lr_cadena.viv_97_AIVS                               ,
                COLUMN 429, pr_ajuste_viv_nss.fecha_id_aivs USING "DDMMYYYY"    ,
                COLUMN 437, lr_cadena.viv_92_pesos                              ,
                COLUMN 452, lr_cadena.viv_97_pesos                              ,
                COLUMN 467, lr_cadena.otros_importes                            ,
                COLUMN 482, lr_cadena.impt_neto                                 ,
                COLUMN 497, pr_notifica_viv.comentarios                         ,
                COLUMN 624, pr_ajuste_viv_nss.fecha_pago USING "DDMMYYYY"       ,
                COLUMN 632, pr_notifica_viv.referencia_pago                     ,
                COLUMN 652, pr_notifica_viv.observaciones                       ,
                COLUMN 779, pr_notifica_viv.fecha_captura USING "DDMMYYYY"      ,
                COLUMN 787, pr_mto_pagado.indicador USING "&"                   ,
                COLUMN 788, lr_cadena.viv_92_AIVS_aplicado                      ,
                COLUMN 803, lr_cadena.viv_97_AIVS_aplicado                      ,
                COLUMN 818, lr_cadena.viv_92_pesos_aplicado                     ,
                COLUMN 833, lr_cadena.viv_97_pesos_aplicado                     ,
                #COLUMN 848, 16 SPACES    #CPL-2104
                COLUMN 848, lc_indicador_desmarca                               ,
                COLUMN 849, 15 SPACES

        -- SUMARIO
        ON LAST ROW

            PRINT
                COLUMN 001, "09"                                , -- tipo de registro
                COLUMN 003, "04"                                , -- identificador de servicio
                COLUMN 005, "01"                                , -- tipo entidad origen
                COLUMN 007, gs_codigo_afore USING "&&&"         , -- clave entidad origen
                COLUMN 010, "03"                                , -- tipo entidad destino
                COLUMN 012, "001"                               , -- clave entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"                , -- fecha de operacion
                COLUMN 023, ls_cont USING "&&&&&&"              , -- numero de registros
                COLUMN 029, 835 SPACES

END REPORT
