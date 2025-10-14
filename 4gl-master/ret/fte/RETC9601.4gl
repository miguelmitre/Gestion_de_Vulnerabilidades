#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC9601  => REVERZO DE LA CARGA DE LA OP. 42                        #
#                       TRANSFERENCIA ISSSTE                                    #
# Fecha creacion     => 24 DE OCTUBRE DE 2009                                   #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Fecha actualiz.   => 11 DE MAYO DE 2011                                       #
#                   => Se realizan las modificaciones al codigo para incluir    #
#                      el llamado de las funciones para realizar el marcaje de  #
#                      trabajador pensionado (REQ. EFPS-157 )                   #
# Sistema            => RET                                                     #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[6] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado_issste.estado_solicitud
    END RECORD
 
    DEFINE 
        enter           CHAR(1)

    DEFINE 
        HOY             DATE

    DEFINE
        gs_peiss             ,
        gs_cod_afore         ,
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

    CALL STARTLOG("RETC9601.log")

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
    WHERE  descripcion = "TRANSFERENCIA ISSSTE"

    LET gr_bitacora.programa      = "RETC9601"
    LET gr_bitacora.desc_tramite  = "CARGA OP.42 TRANSF ISSSTE"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

END FUNCTION

#------------------------------------------------------------------------------#
# Captura el folio que se usara para el reverso de la liquidacion              #
#------------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE folio_op42 LIKE ret_trans_issste.folio

    DEFINE
        ls_procesa      INTEGER

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_op42
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.capturado

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC96011" ATTRIBUTE(BORDER) 
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC9601   REVERSO DE CARGA DE LA OP.42 TRANSFERENCIAS ISSSTE             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_op42 WITHOUT DEFAULTS
        
        AFTER FIELD folio_op42
            IF folio_op42 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op42
            END IF
             
            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio            = folio_op42
            AND    estado_solicitud = gr_edo.capturado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_op42
            END IF

        ON KEY (ESC)
            IF folio_op42 IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_op42
            END IF
             
            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio            = folio_op42
            AND    estado_solicitud = gr_edo.capturado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_op42
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

    RETURN ls_procesa, folio_op42

END FUNCTION

#------------------------------------------------------------------------------#
# Ejecuta el reverso de la operacion solicitada                                #
#------------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE 
        pi_folio           INTEGER 

    DEFINE 
        li_num_afect        INTEGER

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE lr_desmarca RECORD
        nss         LIKE ret_trans_issste.nss        ,
        marca_cod   LIKE cta_his_marca.marca_cod     ,    
        consec      LIKE ret_trans_issste.consecutivo
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
    
    DELETE 
    FROM   ret_monto_viv_issste
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_monto_viv_issste   : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[2].tabla     = "ret_monto_viv_issste"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    -- Desmarca de registros cargados

    LET li_num_afect        = 0
    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0
   
    DECLARE cur_rev_des CURSOR FOR
        SELECT A.nss        ,
               B.marca_cod  ,
               A.consecutivo
        FROM   ret_trans_issste A ,
               cta_his_marca B
        WHERE  A.folio            = pi_folio
        AND    A.nss              = B.nss
        AND    A.consecutivo      = B.correlativo
        AND    A.estado_solicitud = gr_edo.capturado
        AND    B.marca_cod BETWEEN 860 AND 869      -- Marcas de transferencias
      
    FOREACH cur_rev_des INTO lr_desmarca.*

        EXECUTE eje_desmarca USING lr_desmarca.*        ,
                                   lr_dat.edo_marca     ,
                                   lr_dat.marca_causa   ,
                                   gr_bitacora.usuario   
        
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
    DELETE 
    FROM   ret_trans_issste
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_trans_issste       : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[4].tabla     = "ret_trans_issste"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

    -----------------------------------------------------------------------------   
    DELETE 
    FROM   ret_marca_pensionado
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado   : ", li_num_afect
            USING "<<<,<<&" AT 14,14

    LET gar_tablas_rev[5].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_num_afect

    -----------------------------------------------------------------------------   
    
    IF gs_cod_afore <> gs_peiss THEN         
        DELETE
        FROM   ret_datamart_issste
        WHERE  folio = pi_folio
        
        LET li_num_afect = SQLCA.sqlerrd[3]
        
        DISPLAY "R. ELIMINADOS EN ret_datamart_issste    : ", li_num_afect
                USING "<<<,<<&" AT 15,14
        
        LET gar_tablas_rev[6].tabla     = "ret_datamart_issste"
        LET gar_tablas_rev[6].accion    = "BORRADA"
        LET gar_tablas_rev[6].num_regs  = li_num_afect
    ELSE
        LET gar_tablas_rev[6].num_regs  = 0
    END IF

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

    FOR i = 1 TO 6
        IF gar_tablas_rev[i].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (ind_rev,
                    gr_bitacora.folio,
                    gar_tablas_rev[i].*
                   )
        END IF 
    END FOR

END FUNCTION
