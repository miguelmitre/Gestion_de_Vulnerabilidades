#################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC8251 => REVERSO VERIFICACION DE RESOLUCION IMSS (Oper.12)         #
#Fecha creacion    => 15 DE FEBRERO DEL 2004                                    #
#By                => FRANCO ESTEBAN ULLOA VIDELA                               #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha             => 9 DE MARZO DE 2010                                        #
#                     Modificaciones para registrar el reverso en las tablas    #
#                     de bitacora                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[6] OF RECORD #modificacion del arreglo por agregado de delete
        accion          LIKE ret_tablas_rev.accion       ,
        tabla           LIKE ret_tablas_rev.tabla        ,
        num_regs        LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio
    END RECORD


    DEFINE gr_edo RECORD
        capturado       LIKE ret_estado.estado_solicitud ,
        confirmado      LIKE ret_estado.estado_solicitud ,
        provisionado    LIKE ret_estado.estado_solicitud ,
        procesado       LIKE ret_estado.estado_solicitud ,
        liquidado       LIKE ret_estado.estado_solicitud ,
        enviado         LIKE ret_estado.estado_solicitud  #INV-2860
    END RECORD

    DEFINE
        HOY                 DATE

    DEFINE
        enter               CHAR(1)

    DEFINE
        gs_cod_afore        ,
        gs_procesa          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC8251")
    CALL init()

    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*

    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        CALL f_lib_error_msg("REVERSO TERMINADO CORRECTAMENTE")
    END IF 

    CLOSE WINDOW retc82511
END MAIN

FUNCTION init()

    -- -----------------------------------------------------------------------------
    
    LET HOY     = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion  = "RETIRO PARCIAL"

    LET gr_bitacora.usuario         = f_lib_obten_user()
    LET gr_bitacora.programa        = "RETC8251"
    LET gr_bitacora.desc_tramite    = "GENERA OP.12 PARCIALES"
    LET gr_bitacora.fecha_ini       = HOY
    LET gr_bitacora.hora_ini        = CURRENT HOUR TO SECOND

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
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"
    
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud       #4 INV-2860
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

END FUNCTION 

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y el tipo de retiro del que se         #
#                   hara el reverso de la generacion de la operacion 12     #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD 
        folio           LIKE ret_parcial.folio
    END RECORD
    
    DEFINE ls_estado LIKE ret_parcial.estado_solicitud

    DEFINE
        lc_nom_arch         CHAR(012),
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_procesa          SMALLINT

    DEFINE
        li_tot_reg          INTEGER

    DEFINE
        ldt_fec_genera      DATE

    -- -----------------------------------------------------------------------------

    LET li_tot_reg = 0

    SELECT MAX(folio)
    INTO   lr_captura.folio
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_edo.enviado #INV-2860

    OPEN WINDOW retc82511 AT 4,4 WITH FORM "RETC82511" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8251 REVERSO SOLICITUD VERIFICACION DE RESOL IMSS (OP 12)             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS
        AFTER FIELD folio
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    CALL f_lib_error_msg(lc_cad_msg)
                    NEXT FIELD folio
                ELSE
                    SELECT fecha_envio
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio
                    WHERE  folio        = lr_captura.folio
                    
                    LET lc_nom_arch = ldt_fec_genera USING "YYYYMMDD",".12P"
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,12
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,12
                    #DISPLAY "Nombre del archivo generado    : ", lc_nom_arch AT 09,12
                END IF
            END IF

        ON KEY (ESC)
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    CALL f_lib_error_msg(lc_cad_msg)
                    NEXT FIELD folio
                ELSE
                    SELECT fecha_envio
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio
                    WHERE  folio        = lr_captura.folio

                    LET lc_nom_arch = ldt_fec_genera USING "YYYYMMDD",".12P"
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,12
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,12
                    #DISPLAY "Nombre del archivo generado    : ", lc_nom_arch AT 09,12
                 END IF
            END IF

            WHILE TRUE
                PROMPT "¿EJECUTAR REVERSO DE NOTIFICACION (OP. 12)? (S/N) : " FOR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa = 1
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET ls_procesa = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT, CONTROL-C)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = 0
            EXIT INPUT
    END INPUT
 
    RETURN ls_procesa, lr_captura.*
    
END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE 
        li_num_afect        INTEGER

    -- -----------------------------------------------------------------------------

    DELETE
    FROM   ret_ctr_envio
    WHERE  folio            = pr_datos.folio
    AND    tipo_operacion   = "AV12"

    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_ctr_envio      : ", li_num_afect
            USING "<<<,<<&" AT 11,15

    LET gar_tablas_rev[1].tabla     = "ret_ctr_envio"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    #se agrega delete para el borrado del registro en ret_bus_diag12
    DELETE
    FROM   ret_bus_diag12
    WHERE  folio = pr_datos.folio

    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_bus_diag12     : ", li_num_afect
            USING "<<<,<<&" AT 12,15

    LET gar_tablas_rev[2].tabla     = "ret_bus_diag12"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect
    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_monto_siefore
    WHERE  folio = pr_datos.folio

    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_monto_siefore  : ", li_num_afect
            USING "<<<,<<&" AT 13,15

    LET gar_tablas_rev[3].tabla     = "ret_monto_siefore"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE 
    FROM   ret_ctr_pago_det
    WHERE  estado   = gr_edo.provisionado
    AND    consecutivo IN (SELECT consecutivo
                           FROM   ret_ctr_pago
                           WHERE  folio_op12 = pr_datos.folio
                           AND    estado     = gr_edo.provisionado)
    AND    nss         IN (SELECT nss
                           FROM   ret_ctr_pago
                           WHERE  folio_op12 = pr_datos.folio
                           AND    estado     = gr_edo.provisionado)
    
    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_ctr_pago_det   : ", li_num_afect
            USING "<<<,<<&" AT 14,15
    
    LET gar_tablas_rev[4].tabla     = "ret_ctr_pago_det"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    UPDATE ret_ctr_pago
    SET    folio_op12  = NULL            ,
           estado      = gr_edo.capturado 
    WHERE  folio_op12  = pr_datos.folio
    AND    estado      = gr_edo.provisionado

    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ACTUALIZADOS EN ret_ctr_pago     : ", li_num_afect
            USING "<<<,<<&" AT 15,15

    LET gar_tablas_rev[5].tabla     = "ret_ctr_pago"
    LET gar_tablas_rev[5].accion    = "ACTUALIZA"
    LET gar_tablas_rev[5].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    UPDATE ret_parcial
    SET    folio                = "" ,
           fecha_envio          = "" ,
           estado_solicitud     = gr_edo.confirmado
    WHERE  folio                = pr_datos.folio

    LET li_num_afect = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ACTUALIZADOS EN ret_parcial      : ", li_num_afect
            USING "<<<,<<&" AT 16,15

    LET gar_tablas_rev[6].tabla     = "ret_parcial"
    LET gar_tablas_rev[6].accion    = "ACTUALIZA"
    LET gar_tablas_rev[6].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(pi_folio)

    DEFINE pi_folio     LIKE ret_bitacora_rev.folio
    
    DEFINE li_ind_rev   LIKE ret_bitacora_rev.id_rev

    DEFINE 
        li_cont         SMALLINT

    -- -----------------------------------------------------------------------------

    LET gr_bitacora.folio       = pi_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   li_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR li_cont = 1 TO 5
        IF gar_tablas_rev[li_cont].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev                  ,
                    gr_bitacora.folio           ,
                    gar_tablas_rev[li_cont].*
                   )
        END IF 
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_estado : Valida que los datos capturados sean correctos        #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_estado(lr_datos)

    DEFINE lr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE ls_estado LIKE ret_parcial.estado_solicitud

    DEFINE
        lc_error            CHAR(100)

    DEFINE
        li_num_regs         INTEGER

    DEFINE 
        ls_max_diag         ,
        ls_id               SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_id = 0

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_parcial
    WHERE  folio            = lr_datos.folio
    
    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        CASE ls_estado
            WHEN gr_edo.enviado #INV-2860
                SELECT NVL(MAX(diag_cuenta_ind),0)
                INTO   ls_max_diag
                FROM   ret_parcial
                WHERE  folio = lr_datos.folio
                
                IF ls_max_diag > 0 THEN 
                    LET lc_error = "DEBE REVERSAR LA RECEPCION DE LA OP.13"
                    LET ls_id    = 1
                ELSE
                    LET ls_id = 0
                END IF
                
            WHEN gr_edo.liquidado
                LET lc_error    = "FOLIO YA LIQUIDADO"
                LET ls_id       = 1 

            OTHERWISE
                LET lc_error    = "EL LOTE DEBE ESTAR EN EDO. 4 - ENVIADO" #INV-2860
                LET ls_id       = 1
        END CASE
    END IF

    IF ls_id = 0 THEN 
        SELECT NVL(COUNT(*),0)
        INTO   li_num_regs
        FROM   ret_parcial    
        WHERE  folio            = lr_datos.folio
    END IF 
   
    RETURN ls_id, lc_error, li_num_regs

END FUNCTION 
