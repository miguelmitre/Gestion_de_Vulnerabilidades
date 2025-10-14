##############################################################################
#Owner             => E.F.P.
#Programa TRAC051  => PANTALLA DE CONTROL CONFRONTA AUTOMATICA CRUCE SAR 92
#Fecha creacion    => 22 DE NOVIEMBRE DE 2001
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 09 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af 

GLOBALS

DEFINE enter char(001)

DEFINE reg_1         RECORD
       estado        CHAR(010) ,
       folio_interno INTEGER   ,
       total_reg     INTEGER
END RECORD

DEFINE sw SMALLINT

DEFINE reg_2 RECORD 
       folio_interno INTEGER
END RECORD 

DEFINE total_proc           ,
       total_acep           ,
       total_acep_dup       ,
       total_rech           ,
       total_rech_dup INTEGER

DEFINE HOY DATE

END GLOBALS

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    LET HOY = TODAY

    OPEN WINDOW trac0511 AT 4,4 WITH FORM "TRAC0511" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" TRAC051   CONFRONTA AUTOMATICA CRUCE SAR 92 ICEFA-AFORE IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET sw = 1 

    INPUT BY NAME reg_1.folio_interno WITHOUT DEFAULTS

    BEFORE FIELD folio_interno

    IF sw = 1 THEN
      SELECT A.*
      INTO   reg_1.*
      FROM   safre_tmp:tra_ctr_automatico A
     
      DISPLAY BY NAME   reg_1.estado
      DISPLAY BY NAME   reg_1.folio_interno
      DISPLAY BY NAME   reg_1.total_reg

      SELECT count(*)
      INTO  total_proc
      FROM  safre_tmp:tra_det_automatico
      WHERE folio_interno = reg_1.folio_interno
      AND   estado <> 0

      SELECT count(*)
      INTO  total_acep
      FROM  safre_tmp:tra_det_automatico
      WHERE folio_interno = reg_1.folio_interno
      AND estado = 10

      SELECT count(*)
      INTO  total_acep_dup
      FROM  safre_tmp:tra_det_automatico
      WHERE folio_interno = reg_1.folio_interno
      AND estado = 11

      SELECT count(*)
      INTO  total_rech
      FROM  safre_tmp:tra_det_automatico
      WHERE folio_interno = reg_1.folio_interno
      AND estado = 12

      SELECT count(*)
      INTO  total_rech_dup
      FROM  safre_tmp:tra_det_automatico
      WHERE folio_interno = reg_1.folio_interno
      AND estado = 13

      DISPLAY BY NAME  total_proc
      DISPLAY BY NAME  total_acep
      DISPLAY BY NAME  total_acep_dup
      DISPLAY BY NAME  total_rech
      DISPLAY BY NAME  total_rech_dup

      LET sw = 0 
END IF

     AFTER FIELD folio_interno

         IF reg_1.estado = "ACTIVO" THEN 
         DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.folio_interno
         DISPLAY BY NAME   reg_1.total_reg

         SELECT count(*)
         INTO  total_proc
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND   estado <> 0

         SELECT count(*)
         INTO  total_acep
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 10

         SELECT count(*)
         INTO  total_acep_dup
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 11

         SELECT count(*)
         INTO  total_rech
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 12

         SELECT count(*)
         INTO  total_rech_dup
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 13

         DISPLAY BY NAME  total_proc
         DISPLAY BY NAME  total_acep
         DISPLAY BY NAME  total_acep_dup
         DISPLAY BY NAME  total_rech
         DISPLAY BY NAME  total_rech_dup

         NEXT FIELD folio_interno
        ELSE
         SELECT "OK"
         FROM   safre_tmp:tra_det_automatico
         WHERE   folio_interno = reg_1.folio_interno
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "FOLIO INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD folio_interno
          END IF
        END IF

      ON KEY(ESC)
         IF reg_1.estado = "ACTIVO" THEN 
            DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.folio_interno
         DISPLAY BY NAME   reg_1.total_reg

         SELECT count(*)
         INTO  total_proc
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND   estado <> 0

         SELECT count(*)
         INTO  total_acep
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 10

         SELECT count(*)
         INTO  total_acep_dup
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 11

         SELECT count(*)
         INTO  total_rech
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 12

         SELECT count(*)
         INTO  total_rech_dup
         FROM  safre_tmp:tra_det_automatico
         WHERE folio_interno = reg_1.folio_interno
         AND estado = 13


         DISPLAY BY NAME  total_proc
         DISPLAY BY NAME  total_acep
         DISPLAY BY NAME  total_acep_dup
         DISPLAY BY NAME  total_rech
         DISPLAY BY NAME  total_rech_dup
        ELSE
         SELECT "OK"
         FROM   safre_tmp:tra_det_automatico
         WHERE folio_interno= reg_1.folio_interno
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "FOLIO INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
SLEEP 5
           NEXT FIELD folio_interno
          ELSE
           EXIT INPUT
          END IF
       END IF
 
      ON KEY(INTERRUPT)
          EXIT PROGRAM
    END INPUT
      

CALL lanza_proceso()
CLOSE WINDOW trac0511
 
END MAIN 

FUNCTION lanza_proceso()
#lp---------------------
DEFINE lanza_proc CHAR(200)


SELECT COUNT(*) 
INTO reg_1.total_reg
FROM  safre_tmp:tra_det_automatico
WHERE folio_interno = reg_1.folio_interno

DELETE FROM safre_tmp:tra_ctr_automatico

INSERT INTO safre_tmp:tra_ctr_automatico
VALUES("ACTIVO",reg_1.folio_interno,reg_1.total_reg)

DISPLAY "LANZANDO PROCESO EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
LET lanza_proc = "nohup fglgo TRAC053 ",reg_1.folio_interno CLIPPED," &"
RUN lanza_proc

DISPLAY "PROCESO EJECUTANDOSE EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
END FUNCTION
