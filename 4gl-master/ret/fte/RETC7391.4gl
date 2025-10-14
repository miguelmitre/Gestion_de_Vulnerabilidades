#################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC7391 => REVERSO DE PRELIQUIDACION DE SOLICITUD DE SALDO           #
#                     VENTANILLA 2.5                                            #
#Fecha creacion    => 3 DE MAYO DE 2012                                         #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion     =>                                                           #
#Fecha             =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[3] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE ret_solicitud_saldo.folio_cargo
    END RECORD

    DEFINE
        gs_procesa      SMALLINT

    DEFINE gr_edo RECORD
        provisionado        LIKE ret_estado.estado_solicitud    ,
        preliquidado        LIKE ret_estado.estado_solicitud      
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE
        enter                 CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL STARTLOG("RETC7391.log")
    CALL init()#i

    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*

    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF 

    CLOSE WINDOW RETC73911
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY

    ----- DATOS PARA LA BITACORA DE REVERSOS ----- 
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "VENTANILLA 2.5"

    LET gr_bitacora.programa      = "RETC7391"
    LET gr_bitacora.desc_tramite  = "PRELIQUIDACION DERECHO OTORGADO VENT 2.5"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO DERECHO"

    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DERECHO"

END FUNCTION 

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio del que se hara el reverso             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD 
        folio           LIKE ret_solicitud_saldo.folio_cargo
    END RECORD
    
    DEFINE lr_preliquida RECORD
        nss             SMALLINT    ,
        fecha_conv      DATE
    END RECORD
    
    
    DEFINE ls_estado LIKE ret_solicitud_saldo.estado_solicitud

    DEFINE
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------
    
    SELECT NVL(MAX(folio), -1)
    INTO   lr_captura.folio
    FROM   ret_trans_imss
    WHERE  estado_solicitud IN (gr_edo.preliquidado)

    
    OPEN WINDOW RETC73911 AT 4,4 WITH FORM "RETC73911" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> Ejecutar                                           < CTRL-C > SALIR  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7391   REVERSO PRELIQUIDACION DERECHO OTORGADO - VENT 2.5              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    IF lr_captura.folio < 0 THEN
        CALL f_error_msg("NO EXISTEN REGISTROS A REVERSAR")
        LET ls_procesa = 0
    ELSE
        INPUT BY NAME lr_captura.* WITHOUT DEFAULTS
            AFTER FIELD folio
                IF lr_captura.folio IS NULL THEN
                    CALL f_error_msg("EL FOLIO NO PUEDE SER NULO")
                    NEXT FIELD folio
                ELSE
                    CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                    IF ls_estado <> 0 THEN
                        CALL f_error_msg(lc_cad_msg)
                        NEXT FIELD folio
                    ELSE
                        SELECT COUNT(UNIQUE nss),
                               fecha_conversion
                        INTO   lr_preliquida.*
                        FROM   ret_preliquida
                        WHERE  folio    = lr_captura.folio
                        GROUP BY 2

                        DISPLAY "Total de registros del folio       : ", lr_preliquida.nss AT 07,12
                        DISPLAY "Fecha de preliquidacion del folio  : ", lr_preliquida.fecha_conv USING "DD-MM-YYYY" AT 08,12
                    END IF
                END IF
        
            ON KEY (ESC)
                IF lr_captura.folio IS NULL THEN
                    CALL f_error_msg("EL FOLIO NO PUEDE SER NULO")
                    NEXT FIELD folio
                ELSE
                    CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                    IF ls_estado <> 0 THEN
                        CALL f_error_msg(lc_cad_msg)
                        NEXT FIELD folio
                    ELSE
                        SELECT COUNT(UNIQUE nss),
                               fecha_conversion
                        INTO   lr_preliquida.*
                        FROM   ret_preliquida
                        WHERE  folio    = lr_captura.folio
                        GROUP BY 2

                        DISPLAY "Total de registros del folio       : ", lr_preliquida.nss AT 07,12
                        DISPLAY "Fecha de preliquidacion del folio  : ", lr_preliquida.fecha_conv USING "DD-MM-YYYY" AT 08,12
                    END IF
                END IF
        
                WHILE TRUE
                    PROMPT "¿EJECUTAR REVERSO DE PRELIQUIDACION DE DERECHO OTORGADO? (S/N) : " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_procesa = 1
                            EXIT INPUT
                        ELSE
                            CALL f_error_msg("PROCESO CANCELADO")
                            LET ls_procesa = 0
                            EXIT INPUT
                        END IF
                    END IF
                END WHILE
        
            ON KEY (INTERRUPT, CONTROL-C)
                CALL f_error_msg("PROCESO CANCELADO")
                LET ls_procesa = 0
                EXIT INPUT
        END INPUT
        
    END IF -- Registros a procesar
 
    RETURN ls_procesa, lr_captura.*
    
END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE ret_solicitud_saldo.folio_cargo
    END RECORD

    DEFINE 
        li_cont         INTEGER

    -- -----------------------------------------------------------------------------
    
    LET li_cont  = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    
    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_preliquida
    WHERE  folio          = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_preliquida         : ", li_cont
            USING "<<<,<<&" AT 11,12

    LET gar_tablas_rev[1].tabla     = "ret_preliquida"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    UPDATE ret_trans_imss
    SET    estado_solicitud         = gr_edo.provisionado
    WHERE  folio                    = pr_datos.folio
    AND    estado_solicitud         = gr_edo.preliquidado

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ACTUALIZADOS EN ret_trans_imss       : ", li_cont
            USING "<<<,<<&" AT 12,12

    LET gar_tablas_rev[2].tabla     = "ret_trans_imss"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------    

    UPDATE ret_det_datamart
    SET    estado                   = gr_edo.provisionado
    WHERE  folio                    = pr_datos.folio
    AND    estado                   = gr_edo.preliquidado

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ACTUALIZADOS EN ret_det_datamart     : ", li_cont
            USING "<<<,<<&" AT 13,12

    LET gar_tablas_rev[3].tabla     = "ret_det_datamart"
    LET gar_tablas_rev[3].accion    = "ACTUALIZA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------    
    

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(76)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR  "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(p_folio)

    DEFINE p_folio LIKE ret_bitacora_rev.folio
    DEFINE ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE 
        i       SMALLINT

    LET gr_bitacora.folio       = p_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR i = 1 TO 3
        IF gar_tablas_rev[i].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (ind_rev,
                    gr_bitacora.folio,
                    gar_tablas_rev[i].*
                   )
        END IF
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_estado : Valida que los datos capturados sean correctos        #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_estado(lr_datos)

    DEFINE lr_datos RECORD 
        folio           LIKE ret_solicitud_saldo.folio_cargo
    END RECORD

    DEFINE
        ls_estado       LIKE ret_solicitud_saldo.estado_solicitud

    DEFINE
        lc_error            CHAR(100)

    DEFINE 
        ls_edo_folio        ,
        ls_max_diag         ,
        ls_id               SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_id = 0

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_trans_imss
    WHERE  folio        = lr_datos.folio
    
    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "    FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        CASE ls_estado
            WHEN gr_edo.preliquidado  
                LET ls_id = 0

            OTHERWISE
                LET lc_error = " ERROR...EL LOTE DEBE ESTAR EN ESTADO ", gr_edo.preliquidado, " - PRELIQUIDADO"
                LET ls_id = 1
        END CASE
    END IF

    RETURN ls_id, lc_error

END FUNCTION 

