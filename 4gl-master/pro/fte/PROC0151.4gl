################################################################################
#PROYECTO          => SISTEMA DE AFORES.( MEXICO )                             #
#OWNER             => E.F.P.                                                   #
#PROGRAMA PROC0151 => REVERSO ENVIO DE PREPROMOTORES DES LAS SOLICITUDES DE    ##                     REACTIVACIOMES                                           #
#FECHA             => 26 de Febrero del 2008                                   #
#POR               => Isabel Fonseca Frias                                     #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT vencioa     #
#                  => 3.0   (v1)                                               #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        fecha_genera          DATE
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP            ,
        PROMPT LINE LAST      ,
        ACCEPT KEY CONTROL -I

    CALL STARTLOG("¨PROC0151.log")
    CALL init() #i
    OPEN WINDOW PROC0151 AT 4,4 WITH FORM "PROC01511" ATTRIBUTE(BORDER)
    DISPLAY "                           <Ctrl-C> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC0151       REVERSO ENVIO DE REACTIVAVIONES PREPROMOTORES A CONSAR         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET reg_1.fecha_genera = HOY
    INPUT BY NAME reg_1.fecha_genera WITHOUT DEFAULTS
        AFTER FIELD fecha_genera
            IF reg_1.fecha_genera IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_genera
            ELSE
               SELECT UNIQUE "OK"                                 
               FROM   pro_ctr_envio A                      
               WHERE  A.fecha_genera = reg_1.fecha_genera        
               AND    A.estado = 4

               IF  STATUS <> NOTFOUND THEN
                   PROMPT "NO SE PUEDE REALIZAR OPERACION,LOTE YA ",
                          "GENERADO...<ENTER> PARA SALIR "  
                   FOR CHAR enter                                               
                                                                                
                   EXIT PROGRAM                                                     
              END IF                                                           
            END IF

        ON KEY (ESC)
            IF reg_1.fecha_genera IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_genera
            ELSE                                                                
               SELECT UNIQUE "OK"
               FROM   pro_ctr_envio A
	       WHERE  A.fecha_genera = reg_1.fecha_genera
	       AND    A.estado = 4      

               IF  STATUS <> NOTFOUND THEN                                      
                   PROMPT "NO SE PUEDE REALIZAR OPERACION,LOTE YA ",
                          "GENERADO...<ENTER> PARA SALIR " 
                   FOR CHAR enter                                               
                   EXIT PROGRAM                   
               END IF                              

            END IF
            EXIT INPUT
        
        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW PROC0151
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 7    ,
	        pro_solicitud.status         = 0    ,
	        pro_solicitud.fenvio         = NULL ,
           pro_solicitud.num_lote       = 0 
    WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
    AND    pro_solicitud.status_interno = 8 
    AND    cod_promotor in (SELECT  B.cod_promotor                   --(v1)
                                FROM    pro_mae_promotor B               --(v1) 
                                WHERE   B.diag_proceso in   ("7E", "7T"))--(v1)
  

    DELETE 
    FROM   pro_ctr_envio
    WHERE  fecha_genera   = reg_1.fecha_genera
    AND    tipo_operacion IN("REE")                                       --(v1)

    DELETE
    FROM   pro_envio_alta
    WHERE  fenvio = reg_1.fecha_genera
      AND  cod_promotor in (SELECT B.cod_promotor                         --(v1)
                                  FROM    pro_mae_promotor B              --(v1)
                                  WHERE   B.diag_proceso in ("7E", "7T")) --(v1)

END FUNCTION
