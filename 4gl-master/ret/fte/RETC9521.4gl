#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC9521  => REVERZO DE LA RECEPCION DE LA OP. 46                    #
#                       DISPOSICIONES ISSSTE                                    #
# Fecha creacion     => 24 DE OCTUBRE DE 2009                                   #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Actualizacion      =>                                                         #
# Fecha              =>                                                         #
# Sistema            => RET                                                     #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[4] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        enviado         LIKE ret_estado_issste.estado_solicitud ,
        rechazado       LIKE ret_estado_issste.estado_solicitud ,
        recibido        LIKE ret_estado_issste.estado_solicitud
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

    CALL STARTLOG("RETC9521.log")

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
    
    DEFINE
        lc_prepare      CHAR(300)

    LET HOY = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION ISSSTE"

    LET gr_bitacora.programa      = "RETC9521"
    LET gr_bitacora.desc_tramite  = "REC. OP 46 DISP ISSSTE"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE S0LICITUD -----
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
 
    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

END FUNCTION

#------------------------------------------------------------------------------#
# Captura el folio que se usara para el reverso de la liquidacion              #
#------------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE folio_op46 LIKE ret_sol_issste_tx.folio

    DEFINE
        ls_procesa      INTEGER

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_op46
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.recibido

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC95211" ATTRIBUTE(BORDER) 
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC9521   REVERSO DE RECEPCION DE OP.46 DISPOSICION ISSSTE               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_op46 WITHOUT DEFAULTS
        
        AFTER FIELD folio_op46
            IF folio_op46 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op46
            END IF
             
            -- Verifica que el folio exista y este recibido
            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio            = folio_op46
            AND    estado_solicitud IN (gr_edo.recibido ,
                                        gr_edo.rechazado)
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA RECIBIDO ... "
                NEXT FIELD folio_op46
            END IF

        ON KEY (ESC)
            IF folio_op46 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op46
            END IF
             
            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio            = folio_op46
            AND    estado_solicitud IN (gr_edo.recibido ,
                                        gr_edo.rechazado)
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA RECIBIDO ... "
                NEXT FIELD folio_op46
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

    RETURN ls_procesa, folio_op46

END FUNCTION

#------------------------------------------------------------------------------#
# Ejecuta el reverso de la operacion solicitada                                #
#------------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE 
        pi_folio           INTEGER 

    DEFINE 
        li_num_afect        INTEGER

    DEFINE lr_desmarca RECORD
        nss         LIKE ret_sol_issste_tx.nss        ,
        marca_cod   LIKE cta_his_marca.marca_cod     ,    
        consec      LIKE ret_sol_issste_tx.consecutivo,
        fecha_ini   LIKE cta_his_marca.fecha_ini   
    END RECORD 

    -----------------------------------------------------------------------------
    
    DELETE 
    FROM   ret_cza_lote
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_cza_lote           : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    
    UPDATE ret_monto_viv_issste
    SET    cve_operacion    = 45    ,
           estado_sub_viv   = 0     ,
           acc_viv08_bdsviv = 0     ,
           acc_viv92_bdsviv = 0
    WHERE  folio            = pi_folio
   
    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_monto_viv_issste : ", li_num_afect
             USING "<<<,<<&" AT 11,14
    
    LET gar_tablas_rev[2].tabla     = "ret_monto_viv_issste"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect   

    -----------------------------------------------------------------------------
    -- Reversa desmarca de registros rechazados

    LET li_num_afect  = 0
   
    DECLARE cur_rev_des CURSOR FOR
        SELECT A.nss        ,
               B.marca_cod  ,
               A.consecutivo,
               B.fecha_ini
        FROM   ret_sol_issste_tx A ,
               cta_his_marca B
        WHERE  A.folio            = pi_folio
        AND    A.nss              = B.nss
        AND    A.consecutivo      = B.correlativo
        AND    A.estado_solicitud = gr_edo.rechazado
      
    FOREACH cur_rev_des INTO lr_desmarca.*

        EXECUTE eje_rev_desmarca USING lr_desmarca.*

        IF SQLCA.SQLCODE = 0 then
            LET li_num_afect = li_num_afect + 1
        END IF
    
    END FOREACH
    
    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[3].tabla     = "cta_act_marca"
    LET gar_tablas_rev[3].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[3].num_regs  = li_num_afect
   
    -----------------------------------------------------------------------------   
    UPDATE ret_sol_issste_tx
    SET    diag_procesar        = 0                ,
           estado_solicitud     = gr_edo.enviado
    WHERE  folio                = pi_folio
    AND    estado_solicitud IN (gr_edo.rechazado   ,
                                gr_edo.recibido)

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_sol_issste_tx    : ", li_num_afect
             USING "<<<,<<&" AT 13,14
    
    LET gar_tablas_rev[4].tabla     = "ret_sol_issste_tx"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

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
