################################################################################
#Owner             => E.F.P.
#Programa ESTB100  => FORMATO B ( Flujo de Tranferencias Voluntorias ente 
#                     SIEFORE).
#Fecha de Creacion => 07 Mayo 2010
#By                => Marco Antonio Gonzalez Rojas
#Sistema           => 
###############################################################################

DATABASE safre_af 

GLOBALS

DEFINE  reg_1      RECORD
        estado     CHAR(010),
        fecha_ini  DATE,
        fecha_fin  DATE,
        total_reg  INTEGER
                   END RECORD

DEFINE sw SMALLINT

DEFINE reg_2 RECORD 
       fecha_trasp DATE
             END RECORD 

DEFINE HOY DATE

END GLOBALS

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    LET HOY = TODAY

    OPEN WINDOW estb1001 AT 4,4 WITH FORM "ESTB1001" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" ESTB100     GENERACION DE INFORMACION A CONSAR ANEXO B                       " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET sw = 1 

    INPUT BY NAME reg_1.fecha_ini,
		  reg_1.fecha_fin WITHOUT DEFAULTS

    BEFORE FIELD fecha_ini

    IF sw = 1 THEN

          SELECT A.*
          INTO   reg_1.*
          FROM   safre_af:taa_ctr_anexob A
   
        IF STATUS = NOTFOUND THEN
           LET reg_1.fecha_ini = " "
           LET reg_1.fecha_fin = " "
        END IF

             DISPLAY BY NAME   reg_1.estado
             DISPLAY BY NAME   reg_1.fecha_ini
             DISPLAY BY NAME   reg_1.fecha_fin

              LET sw = 0 
    END IF

     AFTER FIELD fecha_ini 
	 IF reg_1.fecha_ini IS NULL THEN
	    ERROR"CAMPO NO PUEDE SER NULO..."
	    NEXT FIELD fecha_ini
         END IF

	 NEXT FIELD fecha_fin

     AFTER FIELD fecha_fin

       IF reg_1.estado = "ACTIVO" THEN 

         DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.fecha_ini
         DISPLAY BY NAME   reg_1.fecha_fin

         NEXT FIELD fecha_ini

       ELSE

          IF reg_1.fecha_fin < reg_1.fecha_ini THEN

             DISPLAY "FECHA FINAL NO PUEDE SER MENOR QUE FECHA INICIAL ..."
             AT 19,2 ATTRIBUTE(REVERSE)
             SLEEP 5
             NEXT FIELD fecha_ini

          END IF

          IF MONTH(reg_1.fecha_ini) <> MONTH(reg_1.fecha_fin) THEN
             DISPLAY "EL MES DE FECHA INI Y FECHA FIN DEBEN SER IGUALES..." 
             AT 19,2 ATTRIBUTE(REVERSE)
             SLEEP 4

             LET reg_1.fecha_ini = " "
             LET reg_1.fecha_fin = " "

             DISPLAY BY NAME   reg_1.fecha_ini
             DISPLAY BY NAME   reg_1.fecha_fin

             NEXT FIELD fecha_ini

          END IF

        END IF

      ON KEY(ESC)
         IF reg_1.fecha_ini IS NULL THEN
	    ERROR"FECHA INICIAL NO PUEDE SER NULA..."
            NEXT FIELD fecha_ini
	 END IF

         IF reg_1.fecha_fin IS NULL THEN
	    ERROR"FECHA FINAL NO PUEDE SER NULA..."
            NEXT FIELD fecha_ini
	 END IF

         IF reg_1.estado = "ACTIVO" THEN 
            DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 5

            DISPLAY BY NAME   reg_1.estado
            DISPLAY BY NAME   reg_1.fecha_ini
            DISPLAY BY NAME   reg_1.fecha_fin

       ELSE

         IF reg_1.fecha_fin < reg_1.fecha_ini THEN
            DISPLAY "FECHA FINAL NO PUEDE SER MENOR QUE FECHA INICIAL ..." 
            AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 5
            NEXT FIELD fecha_ini

         IF MONTH(reg_1.fecha_ini) <> MONTH(reg_1.fecha_fin) THEN
             DISPLAY "EL MES DE FECHA INI Y FECHA FIN DEBEN SER IGUALES..." 
             AT 19,2 ATTRIBUTE(REVERSE)
             SLEEP 3

             LET reg_1.fecha_ini = " "
             LET reg_1.fecha_fin = " "

             DISPLAY BY NAME   reg_1.fecha_ini
             DISPLAY BY NAME   reg_1.fecha_fin

             NEXT FIELD fecha_ini

         END IF

         ELSE 

            EXIT INPUT

         END IF
      END IF 

      ON KEY(INTERRUPT)
          EXIT PROGRAM
    END INPUT
      

CALL lanza_proceso()

CLOSE WINDOW estb1001
 
END MAIN 

FUNCTION lanza_proceso()
#lp---------------------
DEFINE lanza_proc CHAR(200)


DISPLAY "INICIALIZANDO TABLAS DE TRABAJO    ......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4

DELETE FROM safre_af:taa_ctr_anexob

INSERT INTO safre_af:taa_ctr_anexob
VALUES("ACTIVO",reg_1.fecha_ini,reg_1.fecha_fin,0)

DISPLAY "LANZANDO PROCESO EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
LET lanza_proc = "nohup  fglgo ESTB101 ",reg_1.fecha_ini CLIPPED," ",
    #reg_1.fecha_fin CLIPPED," &" #" 1>/dev/null&" Para que mande por nohup.out
     reg_1.fecha_fin CLIPPED," 1>/dev/null&"
RUN lanza_proc

DISPLAY "PROCESO EJECUTANDOSE EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
END FUNCTION
