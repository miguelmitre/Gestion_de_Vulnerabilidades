################################################################################
#Proyecto          => SISTEMA DE safre_af.( MEXICO )                           #
#Sistema           => PRO.                                                     #
#Programa PROC006  => REVERSO ENVIO DE SOLICITUDES PARA LA REVALIDACION DE     #
#                     PROMOTORES                                               #
#Fecha             => 14 DE FEBRERO DEL 2000                                   #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modificacion=> 29 DE MARZO DEL 2004                                     #
#Modificado Por    => LAURA EUGENIA CORTES GUZMAN                              #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        fecha_genera          LIKE pro_capacitacion.fecha_genera
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(001)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROC0061.log")
    CALL init() #i
    OPEN WINDOW proc00611 AT 4,4 WITH FORM "PROC00611" ATTRIBUTE(BORDER)
    DISPLAY "                             <Ctrl-C> Sal",
            "ir                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC0061    REVERSO ENVIO DE PROMOTORES PARA ",
            "REVALIDACION                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

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
               AND    A.tipo_operacion = "REV"

               IF  STATUS <> NOTFOUND THEN
                   PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                          "LOTE YA GENERADO...<ENTER> PARA SALIR " 
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
               AND    A.tipo_operacion = "REV"                                  
                                                                                
               IF  STATUS <> NOTFOUND THEN                                      
                   PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                          "LOTE YA GENERADO...<ENTER> PARA SALIR " 
                   FOR CHAR enter                                               
                                                                                
                   EXIT PROGRAM                                                 
                                                                                
               END IF                                                           
            END IF                                                              

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N " FOR CHAR enter
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                        EXIT INPUT
                    ELSE
                        PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR "
                        FOR CHAR enter 
                        EXIT PROGRAM
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    EXIT PROGRAM

    CLOSE WINDOW proc00611
END MAIN

FUNCTION init()
#i-------------
    LET HOY                = TODAY
    LET reg_1.fecha_genera = HOY
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    UPDATE pro_capacitacion
    SET    estado       = 1 ,
           fecha_genera = ""
    WHERE  fecha_genera = reg_1.fecha_genera

    DELETE
    FROM   pro_ctr_envio
    WHERE  fecha_genera   = reg_1.fecha_genera
    AND    tipo_operacion = "REV"

END FUNCTION

