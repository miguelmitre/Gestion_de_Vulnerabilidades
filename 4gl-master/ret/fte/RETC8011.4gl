#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC8011  => REVERZO DE LA CARGA DE LA DATAMART                      #
# Fecha creacion     => 11 DE MARZO DE 2010                                     #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
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
    DEFINE gar_tablas_rev ARRAY[5] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        recibido             LIKE ret_estado.estado_solicitud
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

    CALL STARTLOG("RETC8011.log")
    CALL init() #i

    CALL f_captura_folio() RETURNING gs_procesa, gi_folio

    IF gs_procesa THEN
        CALL f_reverso(gi_folio)
        CALL f_act_bitacora(gi_folio)
        PROMPT "PROCESO TERMINADO PRESIONE <ENTER> PARA SALIR ..." FOR CHAR enter ATTRIBUTE(NORMAL)
    END IF

    CLOSE WINDOW main_rev
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    LET gr_bitacora.programa      = "RETC8011"
    LET gr_bitacora.desc_tramite  = "CARGA DATAMART"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio de carga de archivo con el que se      #
#                   hara el reverso de la carga                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_lote RECORD LIKE ret_cza_datamart.*
        
    DEFINE folio_dtm LIKE ret_det_datamart.folio

    DEFINE
        ls_procesa      INTEGER

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_dtm
    FROM   ret_cza_datamart
    WHERE  estado_lote = gr_edo.recibido

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC80111" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8011        REVERSO DE CARGA DE ARCHIVO DATAMART                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_dtm WITHOUT DEFAULTS

        AFTER FIELD folio_dtm
            IF folio_dtm IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_dtm
            END IF

            -- Verifica que el folio exista
            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  folio       = folio_dtm
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_dtm
            ELSE
	            SELECT *
	            INTO   lr_lote.*
	            FROM   ret_cza_datamart
	            WHERE  folio = folio_dtm

	            DISPLAY "Nombre del archivo   : ", lr_lote.nom_archivo AT 7,16
	            DISPLAY "Fecha de carga   : ", lr_lote.fecha_carga AT 8,20
            END IF

        ON KEY (ESC)
            IF folio_dtm IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_dtm
            END IF

            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_cza_datamart
            WHERE  folio       = folio_dtm
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_dtm
            ELSE
	            SELECT *
	            INTO   lr_lote.*
	            FROM   ret_cza_datamart
	            WHERE  folio = folio_dtm

	            DISPLAY "Nombre del archivo   : ", lr_lote.nom_archivo AT 7,16
	            DISPLAY "Fecha de carga   : ", lr_lote.fecha_carga AT 8,20
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE CARGA DE DATAMART ? (S/N) : " FOR CHAR enter
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

    RETURN ls_procesa, folio_dtm

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE
        pi_folio           INTEGER

    DEFINE
        li_num_afect        INTEGER

    DEFINE lr_hist_datamart RECORD LIKE ret_det_datamart.*

    -----------------------------------------------------------------------------
    DELETE
    FROM   pen_detalle_op70
    WHERE  folio_datamart = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN pen_detalle_op70     : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "pen_detalle_op70"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect
    
    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_det_datamart
    WHERE  folio = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_det_datamart     : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[2].tabla     = "ret_det_datamart"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    LET li_num_afect = 0

{
    DECLARE cur_hist CURSOR FOR
    SELECT  0                   ,
            nss                 ,
            sec_pension         ,
            folio               ,
            curp                ,
            nombre_datamart     ,
            nombre_afore        ,
            paterno_afore       ,
            materno_afore       ,
            tipo_movimiento     ,
            regimen             ,
            tipo_seguro         ,
            tipo_pension        ,
            tipo_prestacion     ,
            art_negativa        ,
            frac_negativa       ,
            num_considerando    ,
            fecha_ini_pen       ,
            " "                 , -- fec_ini_pago
            fecha_resolucion    ,
            " "                 , -- Clave aseguradora
            porcentaje_val      ,
            semanas_cotizadas   ,
            diag_datamart       ,
            estado_sub_viv
    FROM   ret_dtm_historico
    WHERE  folio_reemplazo  = pi_folio

    FOREACH cur_hist INTO lr_hist_datamart.*

        INSERT INTO ret_det_datamart
        VALUES (lr_hist_datamart.*)

        DELETE
        FROM   ret_dtm_historico
        WHERE  folio_reemplazo  = pi_folio
        AND    nss              = lr_hist_datamart.nss
        AND    sec_pension      = lr_hist_datamart.sec_pension

        LET li_num_afect = li_num_afect + 1

    END FOREACH

    DISPLAY "R. ELIMINADOS EN ret_dtm_historico    : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[3].tabla     = "ret_dtm_historico"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_num_afect
}
    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_marca_pensionado
    WHERE  folio = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[4].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_cza_datamart
    WHERE  folio = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_cza_datamart     : ", li_num_afect
            USING "<<<,<<&" AT 14,14

    LET gar_tablas_rev[5].tabla     = "ret_cza_datamart"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_num_afect


    -----------------------------------------------------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
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
