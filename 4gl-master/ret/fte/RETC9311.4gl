#################################################################################
# Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
# Owner             => E.F.P.                                                   #
# Programa RETC9311 => REVERZO DE LA CARGA DE RESOLUCIONES PARCIALES ISSSTE     #
# Fecha creacion    => 11 DE NOVIEMBRE DE 2009                                  #
# By                => JAVIER GONZALEZ JERONIMO                                 #
#Actualizacion      => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.    => 11 DE MAYO DE 2011                                       #
#                   => Se realizan las modificaciones al codigo para incluir    #
#                      el llamado de las funciones para realizar el marcaje de  #
#                      trabajador pensionado (REQ. EFPS-157)                    #
# Sistema           => RET                                                      #
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
        recibido             LIKE ret_estado_issste.estado_solicitud
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

    CALL f_lib_abre_log("RETC9311")
    CALL init()
    
    CALL f_captura_folio() RETURNING gs_procesa , 
                                     gi_folio

    IF gs_procesa THEN 
        CALL f_reverso(gi_folio)
        CALL f_act_bitacora(gi_folio)

        CALL f_lib_error_msg("REVERSO TERMINADO")
    END IF

    CLOSE WINDOW main_rev

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
    WHERE  descripcion  = "RETIRO PARCIAL ISSSTE"

    LET gr_bitacora.usuario       = f_lib_obten_user()
    LET gr_bitacora.programa      = "RETC9311"
    LET gr_bitacora.desc_tramite  = "CARGA RESOL PAR ISSSTE"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado_issste A
    WHERE  A.descripcion    = "RECIBIDO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara para el reverso de        #
#                   la carga del archivo de Datamart Parcial ISSSTE         #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE folio_resol LIKE ret_datamart_par_issste.folio

    DEFINE
        ls_procesa      INTEGER

    -- -----------------------------------------------------------------------------

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_resol
    FROM   ret_datamart_par_issste

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC93111" ATTRIBUTE(BORDER) 
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC9311      REVERSO DE CARGA DE DATAMART PARCIAL ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_resol WITHOUT DEFAULTS
        
        AFTER FIELD folio_resol
            IF folio_resol IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_resol
            END IF
             
            -- Verifica que el folio exista y ya haya sido recibido
            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  folio       = folio_resol
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA CARGADO")
                NEXT FIELD folio_resol
            END IF

        ON KEY (ESC)
            IF folio_resol IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_resol
            END IF
             
            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  folio       = folio_resol
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA CARGADO")
                NEXT FIELD folio_resol
            END IF

            WHILE TRUE
                PROMPT "¿DESEA EJECUTAR EL REVERSO DE LA CARGA? (S/N) : " FOR enter
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

        ON KEY (CONTROL-C)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = 0
            EXIT INPUT
    
    END INPUT

    RETURN ls_procesa, folio_resol

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE 
        pi_folio           INTEGER 

    DEFINE lr_hist_datamart RECORD LIKE ret_datamart_par_issste.*

    DEFINE 
        li_num_afect        INTEGER

    -- -----------------------------------------------------------------------------

    DELETE 
    FROM   ret_marca_pensionado
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado      : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[1].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE 
    FROM   ret_datamart_par_issste
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_datamart_par_issste   : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[2].tabla     = "ret_datamart_par_issste"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    
    LET li_num_afect = 0

    DECLARE cur_hist CURSOR FOR
    SELECT  folio               ,
            nss                 ,
            nss_issste          ,
            curp                ,
            sec_pension         ,
            num_issste          ,
            nombre_datamart     ,
            paterno_datamart    , 
            materno_datamart    , 
            nombre_afore        ,
            paterno_afore       ,
            materno_afore       ,
            num_concesion       ,
            delegacion          ,
            tipo_movimiento     ,
            regimen             ,
            tipo_seguro         ,
            tipo_pension        ,
            cve_pension         ,
            tipo_prestacion     ,
            fecha_ini_pen       ,
            fecha_resolucion    , 
            semanas_cotizadas   ,  
            diag_datamart    
    FROM   ret_hist_par_dtm_issste
    WHERE  folio_reemplazo = pi_folio
    
    FOREACH cur_hist INTO lr_hist_datamart.*
        
        INSERT INTO ret_datamart_par_issste
        VALUES (lr_hist_datamart.*)

        DELETE
        FROM   ret_hist_par_dtm_issste
        WHERE  folio_reemplazo  = pi_folio
        AND    curp             = lr_hist_datamart.curp
        AND    sec_pension      = lr_hist_datamart.sec_pension
        
        LET li_num_afect = li_num_afect + 1

    END FOREACH

    DISPLAY "R. ELIMINADOS EN ret_hist_par_dtm_issste   : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[3].tabla     = "ret_hist_par_dtm_issste"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    
    DELETE 
    FROM   ret_cza_datamart
    WHERE  folio = pi_folio
    
    LET li_num_afect = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN ret_cza_datamart          : ", li_num_afect
            USING "<<<,<<&" AT 14,14

    LET gar_tablas_rev[4].tabla     = "ret_cza_datamart"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

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

    FOR li_cont = 1 TO 4
        IF gar_tablas_rev[li_cont].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev,
                    gr_bitacora.folio,
                    gar_tablas_rev[li_cont].*
                   )
        END IF 
    END FOR

END FUNCTION
