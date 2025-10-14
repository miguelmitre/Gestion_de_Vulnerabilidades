#################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC7201 => REVERSO DE LIQUIDACION DE SOLICITUD DE SALDO              #
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

    DEFINE gar_tablas_rev ARRAY[4] OF RECORD
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
        preliquidado        LIKE ret_estado.estado_solicitud    ,
        liquidado           LIKE ret_estado.estado_solicitud      
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

    CALL STARTLOG("RETC7201.log")
    CALL init()#i

    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*

    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF 

    CLOSE WINDOW RETC72011
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY = TODAY

    ----- DATOS PARA LA BITACORA DE REVERSOS ----- 
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "VENTANILLA 2.5"

    LET gr_bitacora.programa      = "RETC7201"
    LET gr_bitacora.desc_tramite  = "LIQUIDACION DERECHO OTORGADO VENT 2.5"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DERECHO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO DERECHO"

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

END FUNCTION 

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio del que se hara el reverso             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD 
        folio           LIKE ret_solicitud_saldo.folio_cargo
    END RECORD
    
    DEFINE lr_liquida RECORD
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
    WHERE  estado_solicitud IN (gr_edo.liquidado)

    
    OPEN WINDOW RETC72011 AT 4,4 WITH FORM "RETC72011" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> Ejecutar                                           < CTRL-C > SALIR  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7201      REVERSO LIQUIDACION DERECHO OTORGADO - VENT 2.5              " AT 3,1 ATTRIBUTE(REVERSE)
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
                        INTO   lr_liquida.*
                        FROM   dis_cuenta
                        WHERE  folio    = lr_captura.folio
                        GROUP BY 2

                        DISPLAY "Total de registros del folio       : ", lr_liquida.nss AT 07,12
                        DISPLAY "Fecha de liquidacion del folio     : ", lr_liquida.fecha_conv USING "DD-MM-YYYY" AT 08,12
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
                        INTO   lr_liquida.*
                        FROM   dis_cuenta
                        WHERE  folio    = lr_captura.folio
                        GROUP BY 2

                        DISPLAY "Total de registros del folio       : ", lr_liquida.nss AT 07,12
                        DISPLAY "Fecha de liquidacion del folio     : ", lr_liquida.fecha_conv USING "DD-MM-YYYY" AT 08,12
                    END IF
                END IF
        
                WHILE TRUE
                    PROMPT "¿EJECUTAR REVERSO DE LIQUIDACION DE DERECHO OTORGADO? (S/N) : " FOR CHAR enter
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

    DEFINE lr_reg RECORD
        nss         LIKE ret_parcial.nss            ,
        marca_cod   LIKE cta_act_marca.marca_cod    ,
        consec      LIKE ret_parcial.consecutivo    ,
        fecha_ini   LIKE cta_act_marca.fecha_ini
    END RECORD

    DEFINE 
        li_cont         INTEGER

    -- -----------------------------------------------------------------------------
    
    LET li_cont  = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    -----------------------------------------------------------------------------
    
    DECLARE cur_ptx CURSOR FOR
    SELECT A.nss              , 
           B.marca_cod        ,
           A.consecutivo      ,
           B.fecha_ini
    FROM   ret_trans_imss   A   ,
           cta_his_marca    B
    WHERE  A.nss = B.nss
    AND    A.consecutivo        = B.correlativo 
    AND    A.folio              = pr_datos.folio
    AND    A.estado_solicitud   = gr_edo.liquidado
    AND    B.marca_cod          IN (800, 810, 815)
    AND    B.fecha_fin IS NOT NULL

     FOREACH cur_ptx INTO lr_reg.*

        EXECUTE eje_rev_desmarca USING lr_reg.*

        IF SQLCA.SQLCODE = 0 THEN
            LET li_cont = li_cont + 1
        END IF
        
        UPDATE ret_trans_imss
        SET    estado_solicitud = gr_edo.preliquidado
        WHERE  nss              = lr_reg.nss
        AND    consecutivo      = lr_reg.consec
        AND    estado_solicitud = gr_edo.liquidado

    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", li_cont
            USING "<<<,<<&" AT 11,12

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DISPLAY "R. ACTUALIZADOS EN ret_trans_imss       : ", li_cont
             USING "<<<,<<&" AT 12,12
    
    LET gar_tablas_rev[2].tabla     = "ret_trans_imss"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont
    
    -----------------------------------------------------------------------------

    DELETE
    FROM   dis_cuenta
    WHERE  folio          = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN dis_cuenta             : ", li_cont
            USING "<<<,<<&" AT 13,12

    LET gar_tablas_rev[3].tabla     = "dis_cuenta"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    UPDATE ret_det_datamart
    SET    estado                   = gr_edo.preliquidado
    WHERE  folio                    = pr_datos.folio
    AND    estado                   = gr_edo.liquidado

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ACTUALIZADOS EN ret_det_datamart     : ", li_cont
            USING "<<<,<<&" AT 14,12

    LET gar_tablas_rev[4].tabla     = "ret_det_datamart"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA"
    LET gar_tablas_rev[4].num_regs  = li_cont

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

    FOR i = 1 TO 4
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
            WHEN gr_edo.liquidado  
                LET ls_id = 0

            OTHERWISE
                LET lc_error = " ERROR...EL LOTE DEBE ESTAR EN ESTADO ", gr_edo.liquidado, " - LIQUIDADO"
                LET ls_id = 1
        END CASE
    END IF

    RETURN ls_id, lc_error

END FUNCTION 

