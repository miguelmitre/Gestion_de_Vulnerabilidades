#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC9701  => REVERZO DE LA GENERACION DE LA OP. 52 RETIROS PARCIALES #
#                       ISSSTE                                                  #
# Fecha creacion     => 4 DE NOVIEMBRE DE 2009                                  #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Actualizacion      =>                                                         #
# Fecha              =>                                                         #
# Sistema            => RET                                                     #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[3] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        enviado               LIKE ret_estado_issste.estado_solicitud ,
        confirmado            LIKE ret_estado_issste.estado_solicitud
    END RECORD
 
    DEFINE 
        enter           CHAR(1)

    DEFINE 
        HOY             DATE

    DEFINE
        gs_procesa      SMALLINT

    DEFINE 
        gi_folio        INTEGER

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC9701.log")

    CALL init() #i
    
    CALL f_captura_folio()
        RETURNING gs_procesa, gi_folio

    IF gs_procesa THEN 
        CALL f_reverso(gi_folio)
        CALL f_act_bitacora(gi_folio)

        PROMPT "PROCESO TERMINADO PRESIONE <ENTER> PARA SALIR ..." FOR CHAR enter ATTRIBUTE(NORMAL)
    END IF

    CLOSE WINDOW main_rev
END MAIN

#------------------------------------------------------------------------------#
# Inicializa variables y funciones a utilizar en el programa                   #
#------------------------------------------------------------------------------#
FUNCTION init()
    
    LET HOY = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL ISSSTE"

    LET gr_bitacora.programa      = "RETC9701"
    LET gr_bitacora.desc_tramite  = "GENERA OP52 RET PAR ISSSTE"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"
 
    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CONFIRMADO"

END FUNCTION

#------------------------------------------------------------------------------#
# Captura el folio que se usara para el reverso de la liquidacion              #
#------------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE folio_op52 LIKE ret_parcial_issste.folio

    DEFINE
        ls_procesa      INTEGER

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_op52
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.enviado

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC97011" ATTRIBUTE(BORDER) 
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC9701  REVERSO DE GENERACION DE OP.52 RET PARCIALES ISSSTE             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_op52 WITHOUT DEFAULTS
        
        AFTER FIELD folio_op52
            IF folio_op52 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op52
            END IF
             
            -- Verifica que el folio exista y este enviado
            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio            = folio_op52
            AND    estado_solicitud = gr_edo.enviado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA ENVIADO ... "
                NEXT FIELD folio_op52
            END IF

        ON KEY (ESC)
            IF folio_op52 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op52
            END IF
             
            -- Verifica que el folio exista y este enviado
            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio            = folio_op52
            AND    estado_solicitud = gr_edo.enviado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA ENVIADO ... "
                NEXT FIELD folio_op52
            END IF

            WHILE TRUE
                PROMPT "ESTA SEGURO? (S/N) : " FOR CHAR enter
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

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT
    
    END INPUT

    RETURN ls_procesa, folio_op52

END FUNCTION

#------------------------------------------------------------------------------#
# Ejecuta el reverso de la operacion solicitada                                #
#------------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE 
        pi_folio           INTEGER 

    DEFINE 
        li_num_afect        INTEGER

    -----------------------------------------------------------------------------
    DELETE 
    FROM   ret_ctr_envio
    WHERE  folio            = pi_folio
    AND    tipo_operacion   = "AV52"
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_ctr_envio          : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "ret_ctr_envio"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
   
    UPDATE ret_monto_par_issste
    SET    folio            = 0  ,
           cve_operacion    = 47
    WHERE  folio            = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_monto_par_issste : ", li_num_afect
             USING "<<<,<<&" AT 11,14
    
    LET gar_tablas_rev[2].tabla     = "ret_monto_par_issste"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect
    
    -----------------------------------------------------------------------------
   
    UPDATE ret_parcial_issste
    SET    estado_solicitud = gr_edo.confirmado ,
           folio            = NULL
    WHERE  folio            = pi_folio
    AND    estado_solicitud = gr_edo.enviado

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_parcial_issste   : ", li_num_afect
             USING "<<<,<<&" AT 12,14
    
    LET gar_tablas_rev[3].tabla     = "ret_parcial_issste"
    LET gar_tablas_rev[3].accion    = "ACTUALIZA"
    LET gar_tablas_rev[3].num_regs  = li_num_afect
    
    -----------------------------------------------------------------------------

END FUNCTION

#------------------------------------------------------------------------------#
# Inserta en las tablas de bitacora y de tablas del reverso realizado          #
#------------------------------------------------------------------------------#
FUNCTION f_act_bitacora(pi_folio)

    DEFINE pi_folio LIKE ret_bitacora_rev.folio
    DEFINE ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE 
        i       SMALLINT

    LET gr_bitacora.folio       = pi_folio
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

