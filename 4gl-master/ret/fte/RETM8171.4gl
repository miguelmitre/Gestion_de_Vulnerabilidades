#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETM8171  => REVERZO DE LA CARGA RESOLUCIONES DE RETIROS PARCIALES   #
#                       OPERACION 07                                            #
# Fecha creacion     => 22 DE ABRIL DE 2010                                     #
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

    CALL STARTLOG("RETM8171.log")
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
    WHERE  descripcion = "RETIRO PARCIAL"

    LET gr_bitacora.programa      = "RETM8171"
    LET gr_bitacora.desc_tramite  = "CARGA OPERACION 07"
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

    DEFINE lr_resol RECORD LIKE ret_cza_resol.*
        
    DEFINE folio_resol LIKE ret_parcial_resol.folio

    DEFINE
        ls_procesa      INTEGER

    LET ls_procesa = 1

    SELECT MAX(folio)
    INTO   folio_resol
    FROM   ret_cza_resol
    WHERE  estado_lote = gr_edo.recibido

    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETM81711" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM8171      REVERSO DE CARGA DE ARCHIVO OPERACION 07                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    --CAPTURA DEL FOLIO A REVERSAR
    INPUT BY NAME folio_resol WITHOUT DEFAULTS

        AFTER FIELD folio_resol
            IF folio_resol IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_resol
            END IF

            -- Verifica que el folio exista
            SELECT "OK"
            FROM   ret_cza_resol
            WHERE  folio       = folio_resol
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_resol
            ELSE
	            SELECT *
	            INTO   lr_resol.*
	            FROM   ret_cza_resol
	            WHERE  folio = folio_resol

	            DISPLAY "Nombre del archivo   : ", lr_resol.nom_archivo AT 7,16
	            DISPLAY "Fecha de carga   : ", lr_resol.fecha_carga AT 8,20
            END IF

        ON KEY (ESC)
            IF folio_resol IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio_resol
            END IF

            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_cza_resol
            WHERE  folio       = folio_resol
            AND    estado_lote = gr_edo.recibido
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO INGRESADO NO ESTA CARGADO ... "
                NEXT FIELD folio_resol
            ELSE
	            SELECT *
	            INTO   lr_resol.*
	            FROM   ret_cza_resol
	            WHERE  folio = folio_resol

	            DISPLAY "Nombre del archivo   : ", lr_resol.nom_archivo AT 7,16
	            DISPLAY "Fecha de carga   : ", lr_resol.fecha_carga AT 8,20
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE CARGA DE LA OPERACION 07 ? (S/N) : " FOR CHAR enter
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

    RETURN ls_procesa, folio_resol

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio)

    DEFINE
        pi_folio           INTEGER

    DEFINE
        li_num_afect        INTEGER

    DEFINE lr_hist_datamart RECORD LIKE ret_parcial_resol.*

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_parcial_resol
    WHERE  folio = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_parcial_resol : ", li_num_afect
            USING "<<<,<<&" AT 10,14

    LET gar_tablas_rev[1].tabla     = "ret_cza_resol"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_cza_resol
    WHERE  folio = pi_folio

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_cza_resol     : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[2].tabla     = "ret_parcial_resol"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_num_afect

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

    FOR i = 1 TO 2
        IF gar_tablas_rev[i].num_regs > 0 THEN
            INSERT INTO ret_tablas_rev
            VALUES (ind_rev,
                    gr_bitacora.folio,
                    gar_tablas_rev[i].*
                   )
        END IF
    END FOR

END FUNCTION
