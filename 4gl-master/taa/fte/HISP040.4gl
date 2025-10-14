DATABASE safre_af 

GLOBALS
DEFINE cfecha_presentacion CHAR(008)
DEFINE  reg_1  RECORD
       estado CHAR(010),
       fecha_presentacion DATE,
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

    OPEN WINDOW hisp0011 AT 4,4 WITH FORM "HISP0301" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" HISP001    HISTORICO DE TRASPASOS AFORE - AFORE RECHAZADOS                      " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET sw = 1 
    INPUT BY NAME reg_1.fecha_presentacion

     BEFORE FIELD fecha_presentacion

     IF sw = 1 THEN
       SELECT A.*
       INTO   reg_1.*
       FROM   safre_af:tra_ctr_historico_r A
   
       IF STATUS = NOTFOUND THEN
          LET reg_1.fecha_presentacion = " "
       END IF

         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.fecha_presentacion
         DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc
         LET sw = 0 
     END IF

      AFTER FIELD fecha_presentacion

      LET cfecha_presentacion = reg_1.fecha_presentacion USING"YYYYMMDD"

      IF reg_1.estado = "ACTIVO" THEN
          DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
	  SLEEP 5                                         

          DISPLAY BY NAME   reg_1.estado
          DISPLAY BY NAME   reg_1.fecha_presentacion
          DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc
         NEXT FIELD fecha_presentacion

       ELSE

         SELECT "OK"
         FROM   taa_his_rech_01 b
         WHERE  b.fecha_presentacion = cfecha_presentacion
	 AND    b.ident_operacion = '07'
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "FECHA INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD fecha_presentacion
          END IF
        END IF

      ON KEY(ESC)
         IF reg_1.fecha_presentacion IS NULL THEN
	    ERROR"FECHA NO PUEDE SER NULA..."
	 END IF

         IF reg_1.estado = "ACTIVO" THEN 
            DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.fecha_presentacion
         DISPLAY BY NAME   reg_1.total_reg

         SELECT max(cont_serv) 
         INTO  total_proc
         FROM   safre_tmp:det_mov_trab
         DISPLAY BY NAME  total_proc

        ELSE

         SELECT "OK"
         FROM   taa_his_rech_01 b
         WHERE  b.fecha_presentacion = cfecha_presentacion
	 AND    b.ident_operacion = '07'
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "FECHA INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD fecha_presentacion
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

SELECT COUNT(b.nss) 
INTO reg_1.total_reg
FROM taa_his_rech_01 a,
     taa_his_rech_02 b
WHERE a.fecha_presentacion = cfecha_presentacion
AND   a.ident_operacion = '07'
AND   a.folio = b.folio

DELETE FROM safre_af:tra_ctr_historico_r

INSERT INTO safre_af:tra_ctr_historico_r
VALUES("ACTIVO",reg_1.fecha_presentacion,reg_1.total_reg)

DISPLAY "LANZANDO PROCESO EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
LET lanza_proc = "nohup fglgo HISB041 ",reg_1.fecha_presentacion CLIPPED," ",
    reg_1.fecha_presentacion CLIPPED," & "
RUN lanza_proc

DISPLAY "PROCESO EJECUTANDOSE EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
END FUNCTION
