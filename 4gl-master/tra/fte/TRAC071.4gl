##############################################################################
#Owner             => E.F.P.
#Programa TRAC071  => PANTALLA DE CONTROL CONFRONTA AUTOMATICA CRUCE SAR 92
#Fecha creacion    => 10 DE NOVIEMBRE DEL 2005
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Ultima Mod        => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af 

GLOBALS

DEFINE enter char(001)

DEFINE paso  CHAR(1000)
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

    OPEN WINDOW trac0711 AT 4,4 WITH FORM "TRAC0711" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" TRAC071  CONFRONTA AUTOMATICA CRUCE SAR 92 ICEFA-AFORE ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET sw = 1 

    INPUT BY NAME reg_1.folio_interno WITHOUT DEFAULTS

    BEFORE FIELD folio_interno

    IF sw = 1 THEN
      SELECT A.*
      INTO   reg_1.*
      FROM   tra_ctr_automatico A
     
      DISPLAY BY NAME   reg_1.estado
      DISPLAY BY NAME   reg_1.folio_interno
      DISPLAY BY NAME   reg_1.total_reg

      SELECT count(*)      ---TOTAL DE REGISTROS PROCESADOS---
      INTO  total_proc
      FROM  tra_det_atm_issste
      WHERE folio_interno = reg_1.folio_interno
    --AND   tipo_criterio in (1,2)
      AND   estado not in (0,400)

      SELECT count(*)
      INTO  total_acep     ---TOTAL DE REGISTROS ACEPTADOS---
      FROM  tra_det_atm_issste
      WHERE folio_interno = reg_1.folio_interno
     -- AND   tipo_criterio in (1,2)
      AND   estado = 110

      SELECT count(*)      ---TOTAL DE REGISTROS ACEPTADOS DUPLICADOS---
      INTO  total_acep_dup
      FROM  tra_det_atm_issste
      WHERE folio_interno = reg_1.folio_interno
      --AND   tipo_criterio in (1,2)
      AND estado = 111

      SELECT count(*)      ---TOTAL DE REGISTROS RECHAZADOS---
      INTO  total_rech
      FROM  tra_det_atm_issste
      WHERE folio_interno = reg_1.folio_interno
      --AND   tipo_criterio in (1,2)
      AND estado = 112

      SELECT count(*)      ---TOTAL RECHAZADOS DUPICADOS---
      INTO  total_rech_dup
      FROM  tra_det_atm_issste
      WHERE folio_interno = reg_1.folio_interno
     -- AND   tipo_criterio in (1,2)
      AND estado = 113

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
         INTO  total_proc      ---TOTAL DE REGISTROS PROCESADOS---
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
      -- AND   tipo_criterio in (1,2)
      AND   estado not in (0,400)

         SELECT count(*)
         INTO  total_acep     ---TOTAL DE REGISTROS ACEPTADOS---
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	 --AND   tipo_criterio in (1,2)
         AND estado = 110

         SELECT count(*)
         INTO  total_acep_dup  ---TOTAL DE REGISTROS ACEPTADOS DUPLICADOS---
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	 --AND   tipo_criterio in (1,2)
         AND estado = 111

         SELECT count(*)
         INTO  total_rech      ---TOTAL DE REGISTROS RECHAZADOS---
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	-- AND   tipo_criterio in (1,2)
         AND estado = 112

         SELECT count(*)
         INTO  total_rech_dup  ---TOTAL DE REGISTROS RECHAZADOS DUPLICADOS---
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	-- AND   tipo_criterio in (1,2)
         AND estado = 113

         DISPLAY BY NAME  total_proc
         DISPLAY BY NAME  total_acep
         DISPLAY BY NAME  total_acep_dup
         DISPLAY BY NAME  total_rech
         DISPLAY BY NAME  total_rech_dup

         NEXT FIELD folio_interno
        ELSE
         SELECT "OK"
         FROM   tra_det_atm_issste
         WHERE   folio_interno = reg_1.folio_interno
         GROUP BY  1
          IF STATUS = NOTFOUND THEN
           DISPLAY "FOLIO INEXISTENTE...." AT 19,2 ATTRIBUTE(REVERSE)
           SLEEP 5
           NEXT FIELD folio_interno 
         END IF
        END IF

      ON KEY(ESC)

          WHILE TRUE
             PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
             IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                   EXIT WHILE
                ELSE
                   DISPLAY "                                                                                  " at 19,1 
                   ERROR"PROCESO CANCELADO"
                   SLEEP 2
                   ERROR""
                   NEXT FIELD folio_interno
                END IF
             END IF
         END WHILE


         IF reg_1.estado = "ACTIVO" THEN 
            DISPLAY"PROCESO AUN ACTIVO...." AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 5
         DISPLAY BY NAME   reg_1.estado
         DISPLAY BY NAME   reg_1.folio_interno
         DISPLAY BY NAME   reg_1.total_reg

         SELECT count(*)
         INTO  total_proc
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	 --and   tipo_criterio in (1,2)
         AND   estado not in (0,400)

         SELECT count(*)
         INTO  total_acep
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	-- AND   tipo_criterio in (1,2)
         AND estado = 110

         SELECT count(*)
         INTO  total_acep_dup
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	 --AND   tipo_criterio in (1,2)
         AND estado = 111

         SELECT count(*)
         INTO  total_rech
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
	 --AND   tipo_criterio in (1,2)
         AND estado = 112

         SELECT count(*)
         INTO  total_rech_dup
         FROM  tra_det_atm_issste
         WHERE folio_interno = reg_1.folio_interno
--	 AND   tipo_criterio in (1,2)
         AND estado = 113


         DISPLAY BY NAME  total_proc
         DISPLAY BY NAME  total_acep
         DISPLAY BY NAME  total_acep_dup
         DISPLAY BY NAME  total_rech
         DISPLAY BY NAME  total_rech_dup
        ELSE
         SELECT "OK"
         FROM   tra_det_atm_issste
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
CLOSE WINDOW trac0711
 
END MAIN 

FUNCTION lanza_proceso()
#lp---------------------
DEFINE lanza_proc   CHAR(200)
DEFINE l_listados   CHAR(040)

SELECT ruta_listados
INSERT INTO l_listados
   FROM seg_modulo
WHERE modulo_cod = "tra"


SELECT COUNT(*) 
INTO reg_1.total_reg
FROM  tra_det_atm_issste
WHERE folio_interno = reg_1.folio_interno
--AND   tipo_criterio IN (1,2)
AND   estado = 0

DELETE FROM tra_ctr_automatico

INSERT INTO tra_ctr_automatico
VALUES("ACTIVO",reg_1.folio_interno,reg_1.total_reg)

DISPLAY "LANZANDO PROCESO EN MODO NOHUP......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4

LET lanza_proc = "nohup fglgo TRAC073 ",reg_1.folio_interno CLIPPED

LET paso       = l_listados CLIPPED ,"/",
                 "nohup_atm_",reg_1.folio_interno USING"&&&&&&&&" 

LET lanza_proc = lanza_proc CLIPPED, " 1>",paso CLIPPED," 2>&1 &"

RUN lanza_proc

DISPLAY "PROCESO EJECUTANDOSE EN MODO BACKGROUND......."  AT 19,2  
ATTRIBUTE(REVERSE)
SLEEP 4
END FUNCTION
