################################################################################
#PROYECTO          => SISTEMA DE AFORES.( MEXICO )                             #
#SISTEMA           => PRO.                                                     #
#PROGRAMA PROC0051 => REVERSO GENERA ENCABEZADO Y SUMARIO DE TRANSACCIONES     #
#FECHA             => 04 DE ENERO DEL 2000                                     #
#POR               => FRANCO ESTEBAN ULLOA VIDELA                              #
#FECHA MODIFICACION=> 29 DE MARZO DEL 2004                                     #
#MODIFICADO POR    => LAURA EUGENIA CORTES GUZMAN                              #
#POR               => Isabel Fonseca Frias                                     #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT vencioa     #
#                  => 3.0   (v1)                                             
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
             fecha_genera          LIKE pro_ctr_envio.fecha_genera   ,
             fenvio                LIKE pro_ctr_envio.fenvio         ,
             tipo_operacion        LIKE pro_ctr_envio.tipo_operacion ,
             nro_lote              LIKE pro_ctr_envio.nro_lote
           END RECORD,

           reg_2 RECORD
             procesado             LIKE pro_estado.estado
           END RECORD,

           HOY                   DATE,

           enter                 CHAR(1)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP            ,
        PROMPT LINE LAST      ,
        ACCEPT KEY CONTROL -I

    CALL STARTLOG("PROC0051.log")
    CALL init() #i
    OPEN WINDOW proc00511 AT 4,4 WITH FORM "PROC00511" ATTRIBUTE(BORDER)

    DISPLAY "                           <Ctrl-C> Sal",
            "ir                                      " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " PROC005        REVERSO GENERA LOTE PARA ENVIAR A ",
            "CONSAR                       " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg_1.fenvio = HOY
    INPUT BY NAME reg_1.fenvio WITHOUT DEFAULTS
        AFTER FIELD fenvio
            IF reg_1.fenvio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fenvio
            END IF

        ON KEY (ESC)
            IF reg_1.fenvio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fenvio
            END IF                                   
            EXIT INPUT
        
        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    CALL primer_paso() #pp

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW proc00511
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT estado
    INTO   reg_2.procesado
    FROM   pro_estado
    WHERE  descripcion = "PROCESADO"
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_1 CURSOR FOR
        SELECT tipo_operacion ,
               fecha_genera   ,
               nro_lote
        FROM   pro_ctr_envio
        WHERE  fenvio = reg_1.fenvio

    FOREACH cur_1 INTO reg_1.tipo_operacion ,
                       reg_1.fecha_genera   ,
                       reg_1.nro_lote

        CASE reg_1.tipo_operacion
            WHEN "ALT"
                  UPDATE pro_solicitud
                  SET    pro_solicitud.num_lote = 0,
		                   pro_solicitud.status_interno = 1
                  WHERE  pro_solicitud.fenvio = reg_1.fecha_genera
            WHEN "BAJ"
                  UPDATE pro_mae_promotor
		            SET    pro_mae_promotor.status_interno = 1
                  WHERE  pro_mae_promotor.status_interno = 3
            WHEN "REA"
                  UPDATE pro_solicitud
                  SET    pro_solicitud.num_lote = 0
                  WHERE  pro_solicitud.fenvio = reg_1.fecha_genera
						
            WHEN "REE"                                               --(v1)
                  UPDATE pro_solicitud
                  SET    pro_solicitud.num_lote = 0
                  WHERE  pro_solicitud.fenvio = reg_1.fecha_genera

            WHEN "REC"
                  UPDATE pro_solicitud
                  SET    pro_solicitud.num_lote = 0
                  WHERE  pro_solicitud.fenvio = reg_1.fecha_genera
        END CASE
    END FOREACH

    UPDATE pro_ctr_envio
    SET    estado   = reg_2.procesado ,
           fenvio   = NULL            ,
           nro_lote = NULL
    WHERE  fenvio = reg_1.fenvio

    DELETE
    FROM   pro_ctr_lote
    WHERE  pro_ctr_lote.fecha_envio = reg_1.fenvio

    DELETE
    FROM   pro_lote_envio
    WHERE  fenvio = reg_1.fenvio
END FUNCTION
