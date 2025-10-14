DATABASE safre_af 

GLOBALS
DEFINE  reg_1  RECORD
       estado CHAR(010),
       fecha_ini DATE,
       fecha_fin DATE,
       total_reg  INTEGER
END RECORD
DEFINE sw SMALLINT
DEFINE reg_2 RECORD 
    fecha_trasp DATE
END RECORD 
DEFINE total_proc INTEGER

DEFINE HOY DATE
END GLOBALS

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    LET HOY = TODAY

    OPEN WINDOW hisp0011 AT 4,4 WITH FORM "HISP0011" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" HISP001           HISTORICO DE TRASPASOS AFORE - AFORE                       " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET sw = 1 
    INPUT BY NAME reg_1.fecha_ini,
		  reg_1.fecha_fin WITHOUT DEFAULTS

    BEFORE FIELD fecha_ini

IF sw = 1 THEN
      SELECT A.*
      INTO   reg_1.*
      FROM   safre_tmp:tra_ctr_historico A
   
 IF STATUS = NOTFOUND THEN
    LET reg_1.fecha_ini = " "
    LET reg_1.fecha_fin = " "
 END IF

         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.fecha_ini
         DISPLAY BY NAME   reg_1.fecha_fin
         DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc
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
         DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc
         NEXT FIELD fecha_ini

       ELSE

         SELECT "OK"
         FROM   taa_cd_det_cedido
         WHERE   fecha_trasp BETWEEN reg_1.fecha_ini AND reg_1.fecha_fin
         AND estado in (103,99,12)
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "PERIODO INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD fecha_ini
          END IF
        END IF

      ON KEY(ESC)
         IF reg_1.fecha_ini IS NULL THEN
	    ERROR"FECHA INICIAL NO PUEDE SER NULA..."
	 END IF

         IF reg_1.fecha_fin IS NULL THEN
	    ERROR"FECHA FINAL NO PUEDE SER NULA..."
	 END IF

         IF reg_1.estado = "ACTIVO" THEN 
            DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.fecha_ini
         DISPLAY BY NAME   reg_1.fecha_fin
         DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc

        ELSE
         SELECT "OK"
         FROM   taa_cd_det_cedido
         WHERE   fecha_trasp BETWEEN reg_1.fecha_ini AND reg_1.fecha_fin
         AND  estado in (103,99,12)
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "PERIODO INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD fecha_ini
          ELSE
           EXIT INPUT
          END IF
       END IF
 
      ON KEY(INTERRUPT)
          EXIT PROGRAM
    END INPUT
      

CALL lanza_proceso()
CLOSE WINDOW hisp0011
 
END MAIN 

FUNCTION lanza_proceso()
#lp---------------------
DEFINE lanza_proc CHAR(200)




DISPLAY "INICIALIZANDO TABLAS DE TRABAJO    ......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
DELETE from safre_tmp:cza_hist_trab
DELETE from safre_tmp:det_mov_trab

SELECT COUNT(*) INTO 
reg_1.total_reg
FROM taa_cd_det_cedido
WHERE fecha_trasp  BETWEEN reg_1.fecha_ini AND reg_1.fecha_fin
AND   estado in (103,99,12)

DELETE FROM safre_tmp:tra_ctr_historico

INSERT INTO safre_tmp:tra_ctr_historico 
VALUES("ACTIVO",reg_1.fecha_ini,reg_1.fecha_fin,reg_1.total_reg)

DISPLAY "LANZANDO PROCESO EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
LET lanza_proc = "nohup fglgo HISB001 ",reg_1.fecha_ini CLIPPED," ",
    reg_1.fecha_fin CLIPPED," &"
RUN lanza_proc

DISPLAY "PROCESO EJECUTANDOSE EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
END FUNCTION



