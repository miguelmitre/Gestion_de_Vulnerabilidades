#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa PENC1011 => REVERSO DE GENERACION DEL ARCHIVO DE LA OPERACION 70 DE   #
#                     PENSION MINIMA GARANTIZADA                                #
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
        capturado       LIKE pen_estado_pmg.estado_solicitud ,
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
        CALL STARTLOG("PENC1011.log")

    CALL init() #i

    CALL f_captura_datos() RETURNING gi_folio, gs_procesa

    IF gs_procesa THEN
        CALL f_reverso_lote(gi_folio) #pp
        CALL f_act_bitacora()
        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR enter
    END IF
    
    CLOSE WINDOW penc1011

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY  = TODAY

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "PENSION MINIMA GARANTIZADA"

    LET gr_bitacora.programa      = "PENC1011"
    LET gr_bitacora.desc_tramite  = "GENERA OPERACION 70"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ---------------------------------------------------------

    SELECT estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg
    WHERE  descripcion = "CAPTURADO"

    SELECT estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg
    WHERE  descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio de generacion de la operacion 78 con   #
#                   el que se hara el reverso                               #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE
        ldt_fecha_gen           DATE

    DEFINE
        ls_procesa              ,
        ls_valida_rev           SMALLINT

    DEFINE
        li_folio                ,
        li_registros            INTEGER

    DEFINE
        lc_nom_archivo          CHAR(015),
        lc_mensaje_err          CHAR(100)

    LET ls_procesa = 1

    OPEN WINDOW penc1011 AT 4,4 WITH FORM "PENC10111" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC1011      REVERSO GENERACION DE OPERACION 70 DE PMG                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET ldt_fecha_gen = HOY

    SELECT MAX(folio_lote)
    INTO   li_folio
    FROM   pen_envio
    WHERE  tipo_operacion   = 70
    AND    estado           = gr_edo.enviado

    INPUT BY NAME li_folio WITHOUT DEFAULTS

        AFTER FIELD li_folio
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
	        ELSE
	            CALL f_valida_revlote(li_folio) RETURNING ls_valida_rev     ,
	                                                      lc_mensaje_err    ,
	                                                      li_registros      ,
	                                                      lc_nom_archivo
	            IF NOT ls_valida_rev THEN
	                ERROR lc_mensaje_err ATTRIBUTE(NORMAL)
	                NEXT FIELD li_folio
	            ELSE
	                LET ls_procesa      = 1
	                DISPLAY "Nombre del archivo generado  : ", lc_nom_archivo AT 6,13
	                DISPLAY "Total de registros del lote : ", li_registros AT 7,14
	            END IF
            END IF

        ON KEY (ESC)
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
            ELSE
	            CALL f_valida_revlote(li_folio) RETURNING ls_valida_rev     ,
	                                                      lc_mensaje_err    ,
	                                                      li_registros      ,
	                                                      lc_nom_archivo
	            
	            IF NOT ls_valida_rev THEN
	                ERROR lc_mensaje_err ATTRIBUTE(NORMAL)
	                NEXT FIELD li_folio
	            ELSE
	                LET ls_procesa      = 1
	                DISPLAY "Nombre del archivo generado  : ", lc_nom_archivo AT 6,13
	                DISPLAY "Total de registros del lote : ", li_registros AT 7,14
	            END IF

	            EXIT INPUT
            END IF

        ON KEY (INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT

    END INPUT

    IF ls_procesa THEN
        WHILE TRUE
            PROMPT "¿ EJECUTAR REVERSO DE GENERACION DE LA OPERACION 70 ? (S/N) : " FOR CHAR enter
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
# f_valida_revlote : Valida que sea posible realizar el reverso del lote    #
#                    verificando que el estado de la solicitud sea correcto #
#                    y que no se haya cargado la respuesta de la op 70      #
#---------------------------------------------------------------------------#
FUNCTION f_valida_revlote(pi_folio)

    DEFINE lr_envio RECORD LIKE pen_envio.*

    DEFINE
        pdt_fecha               DATE

    DEFINE
        ls_valida               SMALLINT

    DEFINE
        pi_folio                INTEGER

    DEFINE
        lc_mensaje              CHAR(100)

    LET ls_valida       = 1
    LET lc_mensaje      = " "

    SELECT *
    INTO   lr_envio.*
    FROM   pen_envio
    WHERE  folio_lote       = pi_folio
    AND    tipo_operacion   = 70

    IF STATUS = NOTFOUND THEN
        LET ls_valida   = 0
        LET lc_mensaje  = "NO SE HA GENERADO UNA OP. 70 CON ESE FOLIO"
    ELSE
        #-- Valida que no se haya recibido la respuesta de la op 70
    
        SELECT "OK"
        FROM   pen_recepcion
        WHERE  folio_lote       = pi_folio
        AND    tipo_operacion   = 70
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            LET ls_valida   = 0
            LET lc_mensaje  = "DEBE REVERSARSE ANTES LA RECEPCION DE LA RESPUESTA OP. 70"
        END IF
    END IF

    RETURN ls_valida                , 
           lc_mensaje               , 
           lr_envio.tot_registros   ,
           lr_envio.nom_archivo

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_lote : Ejecuta el proceso de reverso de la generacion de lote   #
#                  de disposiciones                                         #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_lote(pi_folio)

    DEFINE
        pi_folio                ,
        li_cont                 INTEGER

    -----------------------------------------------------------------------------
    UPDATE pen_detalle_op70
    SET    folio_envio      = 0               , 
           estado           = gr_edo.capturado
    WHERE  folio_envio      = pi_folio
    AND    estado           = gr_edo.enviado

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN pen_detalle_op70    : ", li_cont
             USING "<<<,<<&" AT 10,09

    LET gar_tablas_rev[1].tabla     = "pen_solicitud_pmg"
    LET gar_tablas_rev[1].accion    = "ACTUALIZA"
    LET gar_tablas_rev[1].num_regs  = li_cont


    -----------------------------------------------------------------------------
    DELETE 
    FROM   pen_envio
    WHERE  folio_lote       = pi_folio
    AND    tipo_operacion   = 70
    AND    estado           = gr_edo.enviado

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN pen_envio             : ", li_cont
             USING "<<<,<<&" AT 11,09

    LET gar_tablas_rev[2].tabla     = "pen_envio"
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
