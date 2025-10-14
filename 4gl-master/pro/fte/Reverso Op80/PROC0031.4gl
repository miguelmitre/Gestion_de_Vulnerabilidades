################################################################################
#Proyecto          => SISTEMA SAFRE ( MEXICO )                                 #
#Sistema           => PRO                                                      #
#Programa PROC0031 => REVERSO GENERACION DE ARCHIVO PARA LA CONSAR PROMOTORES  #
#                     DADOS DE BAJA                                            #
#Fecha             => 24 DE ENERO DEL 2001                                     #
#ELABORADO POR     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Actualizado => 29 DE MARZO DEL 2004                                     #
#POR               => LAURA EUGENIA CORTES GUZMAN                              #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD
              fecha_genera   LIKE pro_envio_scb.fecha_genera
           END RECORD,

           HOY               DATE,
           enter             CHAR(01)

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS 
        ACCEPT KEY CONTROL-I , 
        INPUT WRAP           ,
        PROMPT LINE LAST

   CALL STARTLOG("PROC0031.log")
   CALL init() #i
   OPEN WINDOW proc00311 AT 4,4 WITH FORM "PROC00311" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Sa",
           "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY " PROC0031      REVERSO ARCHIVO PROMOTORES DADOS ",
           "DE BAJA                    " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.fecha_genera WITHOUT DEFAULTS
       AFTER FIELD fecha_genera
           IF reg_1.fecha_genera IS NULL THEN 
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD fecha_genera
           ELSE
              SELECT UNIQUE "OK"
              FROM   pro_ctr_envio A
              WHERE  A.fecha_genera   = reg_1.fecha_genera
              AND    A.estado         = 4
              AND    A.tipo_operacion = "BAJ"

              IF  STATUS <> NOTFOUND THEN
                  PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                         "LOTE YA GENERADO...<ENTER> PARA SALIR "
                  FOR CHAR enter

                  EXIT PROGRAM
              END IF
           END IF                                                             

       ON KEY (ESC)
          IF reg_1.fecha_genera IS NOT NULL THEN  
             SELECT UNIQUE "OK"                                                
             FROM   pro_ctr_envio A                                            
             WHERE  A.fecha_genera   = reg_1.fecha_genera
             AND    A.estado         = 4 
             AND    A.tipo_operacion = "BAJ"

             IF  STATUS <> NOTFOUND THEN                                       
                 PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                        "LOTE YA GENERADO...<ENTER> PARA SALIR " 
                 FOR CHAR enter

                 EXIT PROGRAM 
             END IF                                                            
          END IF
          EXIT INPUT

       ON KEY (INTERRUPT)
           PROMPT " PROCESO CANCELADO... <ENER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT

   DISPLAY " PROCESANDO INFORMACION... " AT 19,2 ATTRIBUTE(REVERSE)

   CALL primer_paso() #pp

   PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW proc00311
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    LET reg_1.fecha_genera = HOY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #reg_2
        reg_2                 RECORD LIKE pro_envio_scb.*

    DEFINE #loc #smallint
        tot_registros         SMALLINT

    DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   pro_envio_scb
        WHERE  fecha_genera = reg_1.fecha_genera

        LET tot_registros = 0
    FOREACH cur_1 INTO reg_2.*
        LET tot_registros = tot_registros + 1

        DISPLAY " NUMERO DE REGISTROS REVERSADOS : ",tot_registros AT 12,20

        UPDATE pro_mae_promotor
        SET    pro_mae_promotor.status_interno = 0
        WHERE  pro_mae_promotor.cod_promotor   = reg_2.cod_promotor
        AND    pro_mae_promotor.status_interno = 1
    END FOREACH
              
    DELETE 
    FROM   pro_ctr_envio
    WHERE  pro_ctr_envio.fecha_genera   = reg_1.fecha_genera
    AND    pro_ctr_envio.tipo_operacion = "BAJ"
    AND    pro_ctr_envio.estado         = 2

    DELETE
    FROM   pro_envio_scb
    WHERE  pro_envio_scb.fecha_genera = reg_1.fecha_genera
END FUNCTION
