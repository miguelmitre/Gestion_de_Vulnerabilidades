################################################################################
#PROYECTO          => SISTEMA DE AFORES.( MEXICO )                             #
#OWNER             => E.F.P.                                                   #
#PROGRAMA PROC0011 => REVERSO ENVIO DE PREPROMOTORES A CONSAR, PARA SER        #
#                     CERTIFICADOS                                             #
#FECHA             => 03 DE ENERO DEL 2000                                     #
#POR               => FRANCO ESTEBAN ULLOA VIDELA                              #
#FECHA MODIFICACION=> 16 DE ENERO DEL 2001                                     #
#MODIFICADO POR    => FRANCO ESTEBAN ULLOA VIDELA                              #
#SISTEMA           => PRO.                                                     #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT vencioa     #
#                  => 3.0   (v1)                                               #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 17-09-2009                                               #
#Observacion       => Se actualiza de acuerdo a layout del 27/07/09            #
#                  => (v10)                                                    #
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

    CALL STARTLOG("¨PROC0011.log")
    CALL init() #i
    OPEN WINDOW proc00111 AT 4,4 WITH FORM "PROC00111" ATTRIBUTE(BORDER)
    DISPLAY "                           <Ctrl-C> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC0011       REVERSO ENVIO DE PREPROMOTORES A CONSAR                        " AT 3,1 ATTRIBUTE(REVERSE)
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

    CLOSE WINDOW proc00111
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 0    ,
	        pro_solicitud.status         = 0    ,
	        pro_solicitud.fenvio         = NULL ,
           pro_solicitud.num_lote       = 0 
    WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
    AND    pro_solicitud.status_interno = 1

    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 7    ,
	        pro_solicitud.status         = 0    ,
	        pro_solicitud.fenvio         = NULL ,
           pro_solicitud.num_lote       = 0 
    WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
    AND    pro_solicitud.status_interno = 8 
    AND    cod_promotor not in (SELECT  B.cod_promotor                   --(v1)
                                FROM    pro_mae_promotor B               --(v1) 
--                              WHERE   B.diag_proceso in ("7E", "7T"))  --(v1)
                                WHERE   B.diag_proceso in ("7X"))  --(v10)
  

    UPDATE pro_solicitud 
    SET    pro_solicitud.status_interno = 20   ,
	        pro_solicitud.status         = 0    ,
	        pro_solicitud.fenvio         = NULL ,
           pro_solicitud.num_lote       = 0 
    WHERE  pro_solicitud.fenvio         = reg_1.fecha_genera
    AND    pro_solicitud.status_interno = 21

    DELETE 
    FROM   pro_ctr_envio
    WHERE  fecha_genera   = reg_1.fecha_genera
    AND    tipo_operacion IN("ALT","REA","REC")

    DELETE
    FROM   pro_envio_alta
    WHERE  fenvio = reg_1.fecha_genera
      AND  (cod_promotor not in (SELECT B.cod_promotor                    --(v1)
                                  FROM   pro_mae_promotor B              --(v1)
--                                WHERE  B.diag_proceso in ("7E", "7T")) --(v1)
                                  WHERE  B.diag_proceso in ("7X"))       --(v10)
            OR cod_promotor IS NULL)                                      --(v1)


END FUNCTION
