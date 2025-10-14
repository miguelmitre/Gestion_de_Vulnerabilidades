################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETC8171 => REVERSO DE GENERACION DEL LOTE DE DISPOSICIONES          #
#Fecha creacion    => 10 DE ENERO DEL 2004                                     #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#Fecha             => 24 DE FEBRERO DE 2010                                    #
#                     Modificaciones para registrar el reverso en las tablas   #
#                     de bitacora                                              #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[2] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_estado RECORD
        procesado       LIKE ret_estado.estado_solicitud    ,
        enviado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        enter                   CHAR(001)

    DEFINE
        gs_procesa              SMALLINT

    DEFINE
        gdt_fecha_proc          ,
        HOY                     DATE

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC8171")
    CALL init()

    CALL f_captura_datos() RETURNING gdt_fecha_proc, gs_procesa

    IF gs_procesa THEN
        CALL f_reverso_lote(gdt_fecha_proc)
        CALL f_act_bitacora()
        CALL f_lib_error_msg("REVERSO TERMINADO CORRECTAMENTE")
    END IF
    
    CLOSE WINDOW retc8171

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion  = "DISPOSICION"

    LET gr_bitacora.usuario         = f_lib_obten_user()
    LET gr_bitacora.programa        = "RETC8171"
    LET gr_bitacora.desc_tramite    = "GENERA LOTE DISPOSICIONES"
    LET gr_bitacora.fecha_ini       = HOY
    LET gr_bitacora.hora_ini        = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT estado_solicitud
    INTO   gr_estado.procesado
    FROM   ret_estado
    WHERE  descripcion = "PROCESADO"

    SELECT estado_solicitud
    INTO   gr_estado.enviado
    FROM   ret_estado
    WHERE  descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura la fecha de generacion del lote con el que se   #
#                   hara el reverso                                         #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE
        ldt_fecha_gen           DATE

    DEFINE
        ls_procesa              ,
        ls_valida_rev           SMALLINT

    DEFINE
        li_registros            INTEGER

    DEFINE
        lc_nom_archivo          CHAR(015),
        lc_mensaje_err          CHAR(100)

    -- -----------------------------------------------------------------------------

    LET ls_procesa = 1

    OPEN WINDOW retc8171 AT 4,4 WITH FORM "RETC81711" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                         <CTRL-C> - Salir     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8171   REVERSO GENERACION DEL LOTE DE DISPOSICIONES IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET ldt_fecha_gen = HOY

    INPUT BY NAME ldt_fecha_gen WITHOUT DEFAULTS

        AFTER FIELD ldt_fecha_gen
            IF ldt_fecha_gen IS NULL THEN
                CALL f_lib_error_msg("FECHA NO PUEDE SER NULA")
	            NEXT FIELD ldt_fecha_gen
	        ELSE
                CALL f_valida_revlote(ldt_fecha_gen) RETURNING ls_valida_rev    ,
	                                                           lc_mensaje_err   ,
	                                                           li_registros
	            IF NOT ls_valida_rev THEN
	                CALL f_lib_error_msg(lc_mensaje_err)
	                NEXT FIELD ldt_fecha_gen
	            ELSE
	                LET lc_nom_archivo  = ldt_fecha_gen USING "YYYYMMDD",".04D"
	                LET ls_procesa      = 1
	                DISPLAY "Nombre del archivo generado  : ", lc_nom_archivo AT 6,13
	                DISPLAY "Total de registros del lote : ", li_registros AT 7,14
	            END IF
            END IF

        ON KEY (ESC)
            IF ldt_fecha_gen IS NULL THEN
                CALL f_lib_error_msg("FECHA NO PUEDE SER NULA")
	            NEXT FIELD ldt_fecha_gen
            ELSE
	            CALL f_valida_revlote(ldt_fecha_gen) RETURNING ls_valida_rev    ,
	                                                           lc_mensaje_err   ,
	                                                           li_registros
	            IF NOT ls_valida_rev THEN
	                CALL f_lib_error_msg(lc_mensaje_err)
	                NEXT FIELD ldt_fecha_gen
	            ELSE
	                LET lc_nom_archivo  = ldt_fecha_gen USING "YYYYMMDD",".04D"
	                LET ls_procesa      = 1
	                DISPLAY "Nombre del archivo generado  : ", lc_nom_archivo AT 6,13
	                DISPLAY "Total de registros del lote : ", li_registros AT 7,14
	            END IF

                WHILE TRUE
                    PROMPT "¿EJECUTAR REVERSO DE LOTE DE DISPOSICION? (S/N) : " FOR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_procesa = 1
                        ELSE
                            CALL f_lib_error_msg("REVERSO CANCELADO")
                            LET ls_procesa = 0
                        END IF
                        
                        EXIT INPUT
                    END IF
                END WHILE
            END IF

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("REVERSO CANCELADO")
            LET ls_procesa = 0
            EXIT INPUT

    END INPUT

    RETURN ldt_fecha_gen, ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_revlote : Valida que sea posible realizar el reverso del lote    #
#                    verificando que el estado de la solicitud sea correcto #
#                    y que no se haya cargado la operacion 06               #
#---------------------------------------------------------------------------#
FUNCTION f_valida_revlote(pdt_fecha)

    DEFINE lr_envio_lote RECORD LIKE ret_ctr_envio_lote.*

    DEFINE ls_diag_reg  LIKE ret_solicitud_tx.diag_registro

    DEFINE
        pdt_fecha               DATE

    DEFINE
        ls_tot_folio            ,
        ls_valida               SMALLINT

    DEFINE
        li_tot_regs             INTEGER

    DEFINE
        lc_mensaje              CHAR(100)

    -- -----------------------------------------------------------------------------

    LET li_tot_regs     = 0
    LET ls_diag_reg     = 0
    LET ls_tot_folio    = 0
    LET ls_valida       = 1
    LET lc_mensaje      = " "

    DECLARE cur_folio CURSOR FOR
        SELECT *
        FROM   ret_ctr_envio_lote
        WHERE  tipo_retiro IN (SELECT tipo_retiro
                               FROM   tab_tramite_retiro
                               WHERE  cod_tramite = gr_bitacora.cod_tramite
                              )
        AND    estado      = gr_estado.enviado
        AND    fecha_envio = pdt_fecha

    FOREACH cur_folio INTO lr_envio_lote.*

        LET ls_tot_folio    = ls_tot_folio + 1
        LET li_tot_regs     = li_tot_regs + lr_envio_lote.total_registros

        SELECT MAX(diag_registro)
        INTO   ls_diag_reg
        FROM   ret_solicitud_tx
        WHERE  folio = lr_envio_lote.folio

        IF (ls_diag_reg <> "   ") AND (ls_diag_reg <> 0) AND (ls_diag_reg IS NOT NULL) THEN
            LET ls_valida   = 0
            LET lc_mensaje  = "DEBE REVERSAR LA RECEPCION DE LA OP06"
            EXIT FOREACH
        END IF

    END FOREACH

    IF ls_valida AND ls_tot_folio = 0 THEN
        LET ls_valida   = 0
        LET lc_mensaje  = "NO HA GENERADO UN LOTE EN LA FECHA DADA"
    END IF

    RETURN ls_valida, lc_mensaje, li_tot_regs

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_lote : Ejecuta el proceso de reverso de la generacion de lote   #
#                  de disposiciones                                         #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_lote(pdt_fecha_proc)

    DEFINE
        pdt_fecha_proc          DATE

    DEFINE
        li_cont                 INTEGER

    -- -----------------------------------------------------------------------------
    
    UPDATE ret_solicitud_tx
    SET    estado_solicitud = gr_estado.procesado
    WHERE  folio IN (SELECT folio
                     FROM   ret_ctr_envio_lote
                     WHERE  estado          = gr_estado.enviado
                     AND    fecha_envio     = pdt_fecha_proc
                     AND    tipo_retiro IN (SELECT tipo_retiro
                                            FROM   tab_tramite_retiro
                                            WHERE  cod_tramite = gr_bitacora.cod_tramite
                                           )
                    )

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_solicitud_tx    : ", li_cont
             USING "<<<,<<&" AT 10,09

    LET gar_tablas_rev[1].tabla     = "ret_solicitud_tx"
    LET gar_tablas_rev[1].accion    = "ACTUALIZA"
    LET gar_tablas_rev[1].num_regs  = li_cont


    -----------------------------------------------------------------------------
    
    UPDATE ret_ctr_envio_lote
    SET    estado           = gr_estado.procesado   ,
           fecha_envio      = NULL                  ,
           fecha_reverso    = HOY                   ,
           hora_envio       = NULL                  ,
           usuario_envio    = NULL                  ,
           usuario_reverso  = gr_bitacora.usuario
    WHERE  tipo_retiro IN (SELECT tipo_retiro
                           FROM   tab_tramite_retiro
                           WHERE  cod_tramite = gr_bitacora.cod_tramite
                          )
    AND    estado          = gr_estado.enviado
    AND    fecha_envio     = pdt_fecha_proc

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_envio_lote  : ", li_cont
             USING "<<<,<<&" AT 11,09

    LET gar_tablas_rev[2].tabla     = "ret_ctr_envio_lote"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora()

    DEFINE li_ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE
        li_cont             SMALLINT

    -- -----------------------------------------------------------------------------

    LET gr_bitacora.folio       = 0
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   li_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR li_cont = 1 TO 2
        IF gar_tablas_rev[li_cont].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev                  ,
                    gr_bitacora.folio           ,
                    gar_tablas_rev[li_cont].*
                   )
        END IF
    END FOR

END FUNCTION
