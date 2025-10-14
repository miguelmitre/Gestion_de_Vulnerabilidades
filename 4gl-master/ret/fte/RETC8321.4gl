#################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                                #
#Owner             => E.F.P.                                                    #
#Programa RETC8321 => REVERSO DE GENERACION DE LA OPERACION 20 - TRANSFERENCIAS #
#                     HISTORICAS                                                #        
#Fecha creacion    => 27 DE ABRIL DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gr_edo RECORD #gr_edo
        enviado               LIKE ret_estado.estado_solicitud ,
        recibido              LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_tablas_rev ARRAY[5] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE
        gs_procesa      SMALLINT

    DEFINE gr_datos RECORD
        folio           LIKE ret_sal_his_rx.folio
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE
        enter                 CHAR(1)

END GLOBALS

-- -----------------------------------------------------------------------------

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL STARTLOG("RETC8321.log")
    CALL init()#i

    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*

    IF gs_procesa THEN
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF
    
    CLOSE WINDOW retc_83211
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
    WHERE  descripcion = "TRANSFERENCIA"

    LET gr_bitacora.programa      = "RETC8321"
    LET gr_bitacora.desc_tramite  = "GENERA OP. 20"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado 
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y el tipo de retiro del que se         #
#                   hara el reverso de la liquidacion                       #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD
        folio_oper19    LIKE ret_sal_his_rx.folio
    END RECORD

    DEFINE ls_estado LIKE ret_sal_his_rx.estado_solicitud

    DEFINE
        ls_procesa              SMALLINT

    DEFINE
        lc_nom_arch             CHAR(012),
        lc_cad_msg              CHAR(100)

    DEFINE
        li_tot_reg          INTEGER

    DEFINE
        ldt_fec_genera      DATE
        
    -- -----------------------------------------------------------------------------        

    SELECT MAX(folio)
    INTO   lr_captura.folio_oper19
    FROM   ret_sal_his_rx
    WHERE  estado_solicitud = gr_edo.enviado

    OPEN WINDOW retc_83211 AT 4,4 WITH FORM "RETC83211" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8321   REVERSO GENERACION DE OPERACION 20 TRANSFERENCIAS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS
        AFTER FIELD folio_oper19
            IF lr_captura.folio_oper19 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper19
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper19
                ELSE
                    SELECT fecha_envio
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio
                    WHERE  folio        = lr_captura.folio_oper19
                    
                    LET lc_nom_arch = ldt_fec_genera USING "YYYYMMDD",".020"
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,12
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,12
                    DISPLAY "Nombre del archivo generado    : ", lc_nom_arch AT 09,12
                END IF
            END IF

        ON KEY (ESC)
            IF lr_captura.folio_oper19 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper19
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper19
                ELSE
                    SELECT fecha_envio
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio
                    WHERE  folio        = lr_captura.folio_oper19
                    
                    LET lc_nom_arch = ldt_fec_genera USING "YYYYMMDD",".020"
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,12
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,12
                    DISPLAY "Nombre del archivo generado    : ", lc_nom_arch AT 09,12
                END IF
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE GENERACION DE LA OP. 20 ? (S/N) : " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa = 1
                        EXIT INPUT
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_procesa = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT, CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
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
        folio           LIKE ret_sal_his_rx.folio
    END RECORD

    DEFINE
        li_cont        INTEGER

    LET li_cont = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_sal_his_tx2
    WHERE  folio = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_sal_his_tx2    : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[1].tabla     = "ret_sal_his_tx2"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------

    UPDATE ret_sal_his_rx
    SET    diag_envio       = 0                   ,
           estado_solicitud = gr_edo.recibido
    WHERE  folio = pr_datos.folio

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_sal_his_rx   : ", li_cont
             USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[2].tabla     = "ret_sal_his_rx"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_ctr_envio
    WHERE  folio          = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_ctr_envio      : ", li_cont
            USING "<<<,<<&" AT 14,10

    LET gar_tablas_rev[3].tabla     = "ret_ctr_envio"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------

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

    FOR i = 1 TO 5
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
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE
        ls_estado   LIKE ret_parcial.estado_solicitud

    DEFINE
        lc_error            CHAR(100)

    DEFINE
        li_num_regs         INTEGER

    DEFINE 
        ls_max_diag         ,
        ls_id               SMALLINT

    LET ls_id = 0

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_sal_his_rx
    WHERE  folio    = lr_datos.folio
    
    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "    FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        CASE ls_estado
            WHEN gr_edo.enviado
                LET lc_error = " "
                LET ls_id    = 0
            
            WHEN gr_edo.recibido
                LET lc_error =    "    ERROR...DEBE REVERSAR ANTES LA RECEPCION DE LA OP. 19"
                LET ls_id    = 1 

            OTHERWISE
                LET lc_error = "    ERROR...EL LOTE DEBE ESTAR EN ESTADO 4 - ENVIADO"
                LET ls_id = 1
        END CASE
    END IF

    IF ls_id = 0 THEN 
        SELECT NVL(COUNT(*),0)
        INTO   li_num_regs
        FROM   ret_sal_his_rx    
        WHERE  folio = lr_datos.folio
    END IF 
   
    RETURN ls_id, lc_error, li_num_regs

END FUNCTION 