#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa PENC1021 => REVERSO DE LA RECEPCION DEL ARCHIVO DE RESPUESTA DE LA    #
#                     OPERACION 79 DE PENSION MINIMA GARANTIZADA                #
#Fecha creacion    => 6 DE ABRIL DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion     =>                                                           #
#Fecha             =>                                                           #
#                                                                               #
#Sistema           => PEN                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[2] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        recibido        LIKE pen_estado_pmg.estado_solicitud ,
        enviado         LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE
        enter                   CHAR(001)

    DEFINE
        gs_procesa              SMALLINT

    DEFINE
        gi_folio                INTEGER

    DEFINE
        HOY                     DATE

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I
        CALL STARTLOG("PENC1021.log")

    CALL init() #i

    CALL f_captura_datos() RETURNING gi_folio, gs_procesa

    IF gs_procesa THEN
        CALL f_reverso_lote(gi_folio) #pp
        CALL f_act_bitacora()
        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR enter
    END IF
    
    CLOSE WINDOW penc1021

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300) 

    LET HOY  = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "PENSION MINIMA GARANTIZADA"

    LET gr_bitacora.programa      = "PENC1021"
    LET gr_bitacora.desc_tramite  = "CARGA OPERACION 79"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ---------------------------------------------------------

    SELECT estado_solicitud
    INTO   gr_edo.recibido
    FROM   pen_estado_pmg
    WHERE  descripcion = "RECIBIDO"

    SELECT estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg
    WHERE  descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio de generacion de la operacion 70 con   #
#                   el que se hara el reverso                               #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_lote RECORD LIKE pen_recepcion.*

    DEFINE
        ls_procesa              ,
        ls_valida_rev           SMALLINT

    DEFINE
        li_folio                ,
        li_registros            INTEGER

    DEFINE
        lc_nom_archivo          CHAR(015),
        lc_mensaje_err          CHAR(100),
        lc_clave                CHAR(003)

    LET ls_procesa = 1

    OPEN WINDOW penc1021 AT 4,4 WITH FORM "PENC10211" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC1021      REVERSO RECEPCION DE OPERACION 70 DE PMG                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    SELECT MAX(folio_lote)
    INTO   li_folio
    FROM   pen_recepcion
    WHERE  tipo_operacion   = 70
    AND    estado           = gr_edo.recibido

    INPUT BY NAME li_folio WITHOUT DEFAULTS

        AFTER FIELD li_folio
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
	        ELSE
	            SELECT *
	            INTO   lr_lote.*
	            FROM   pen_recepcion
	            WHERE  folio_lote       = li_folio
	            AND    tipo_operacion   = 70

	            DISPLAY "Nombre del archivo     : ", lr_lote.nom_archivo AT 6,19
	            DISPLAY "Fecha de carga     : ", lr_lote.fecha_recepcion AT 7,23
            END IF

        ON KEY (ESC)
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
            ELSE
                CALL f_determina_tipo(li_folio) RETURNING lc_clave

                IF lc_clave = "xxx" THEN
                    ERROR "   EL FOLIO NO CORRESPONDE A LA CARGA DE ARCHIVO" ATTRIBUTE(NORMAL)
                    LET li_folio = 0
                    DISPLAY BY NAME li_folio
	                NEXT FIELD li_folio
                END IF
                
                EXIT INPUT
                
            END IF

        ON KEY (CONTROL-C, INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT

    END INPUT

    IF ls_procesa THEN
        WHILE TRUE
            PROMPT "¿ EJECUTAR REVERSO DE RECEPCION DE LA OPERACION 70 ? (S/N) : " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    LET ls_procesa = 1
                ELSE
                    PROMPT "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                    LET ls_procesa = 0
                END IF
                EXIT WHILE
            END IF
        END WHILE
    END IF

    RETURN li_folio, ls_procesa
END FUNCTION

#---------------------------------------------------------------------------#
# f_determina_tipo : Valida que el folio ingresado corresponda a alguno de  #
#                    los tipos de proceso que se cargan                     #
#---------------------------------------------------------------------------#
FUNCTION f_determina_tipo(li_folio)

    DEFINE
        li_folio        INTEGER

    DEFINE
        lc_clave        CHAR(003)

    LET lc_clave = "   "

    -- Verifica si el folio es de pmg
    SELECT "OK"
    FROM   pen_envio
    WHERE  folio_lote       = li_folio
    AND    tipo_operacion   = 70
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lc_clave = "xxx"
    END IF

    RETURN lc_clave

END FUNCTION


#---------------------------------------------------------------------------#
# f_reverso_lote : Ejecuta el proceso de reverso de la recepcion del        #
#                  archivo de respuesta de la operacion 70                  #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_lote(pi_folio)

    DEFINE lr_reg RECORD
        nss         LIKE pen_solicitud_pmg.nss            ,
        consec      LIKE pen_solicitud_pmg.consecutivo    ,
        diag_cta    LIKE pen_solicitud_pmg.diag_registro  ,
        marca_cod   LIKE cta_his_marca.marca_cod          ,
        fecha_ini   LIKE cta_his_marca.fecha_ini
    END RECORD
    
    DEFINE
        pi_folio                ,
        li_cont_desm            ,
        li_cont                 INTEGER

    DEFINE 
        ls_aceptado             SMALLINT

    LET li_cont_desm    = 0
    LET li_cont         = 0

    -----------------------------------------------------------------------------

    UPDATE pen_detalle_op70
    SET    diag_operacion   = NULL          ,
           codigo_rechazo   = NULL          ,
           estado           = gr_edo.enviado
    WHERE  folio_envio      = pi_folio

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN pen_detalle_op70  : ", li_cont
             USING "<<<,<<&" AT 11,10
    
    LET gar_tablas_rev[1].tabla     = "pen_detalle_op70"
    LET gar_tablas_rev[1].accion    = "ACTUALIZA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------  

    DELETE 
    FROM   pen_recepcion
    WHERE  folio_lote       = pi_folio
    AND    tipo_operacion   = 70
    AND    estado           = gr_edo.recibido

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN pen_recepcion       : ", li_cont
             USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[2].tabla     = "pen_recepcion"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora()

    DEFINE ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE
        i       SMALLINT

    LET gr_bitacora.folio       = gi_folio
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
