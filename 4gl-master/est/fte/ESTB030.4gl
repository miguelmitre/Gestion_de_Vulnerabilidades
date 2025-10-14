DATABASE safre_af 
MAIN 

DEFINE enter         CHAR(001)
DEFINE bat SMALLINT
DEFINE tipo CHAR(003)
DEFINE max_hora CHAR(005)
DEFINE total_activas_s1 INTEGER
DEFINE total_activas_s2 INTEGER
DEFINE vigentes INTEGER
DEFINE mf DATE,
      mh DATETIME HOUR TO MINUTE 

DEFINE hora CHAR(005)

DEFINE qr            CHAR(040),
       ejecuta       CHAR(020),
       x             CHAR(001)
DEFINE HOY           DATE,
       vfecha_corte  DATE
DEFINE sw            SMALLINT

OPTIONS
   PROMPT LINE LAST     ,
   ACCEPT KEY CONTROL-I ,
   INPUT WRAP


LET HOY = TODAY

LET total_activas_s1 = 0
LET total_activas_s2 = 0
--LET ejecuta = ". var"
--RUN ejecuta

LET qr = 'set pdqpriority 100;'
PREPARE qr1 FROM qr
EXECUTE qr1

    OPEN WINDOW ESTB0301 AT 4,4 WITH FORM "ESTB0301" ATTRIBUTE(BORDER)
    DISPLAY " ESTB030        GENERA CIFRA DE APOYO DE CUENTAS ACTIVAS                       " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY "                    <ESC>Iniciar <CTRL-C>Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)             
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,62 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha_corte WITHOUT DEFAULTS

    BEFORE FIELD vfecha_corte
	   IF sw = 0 THEN
	      LET vfecha_corte = TODAY
	      DISPLAY BY NAME vfecha_corte
	      LET sw = 1
	    END IF

    AFTER FIELD vfecha_corte 
    ON KEY (ESC) 
        
       SELECT "OK" 
       FROM   safre_tmp:est_activas_apoyo a
       WHERE  a.fecha    = vfecha_corte
       GROUP BY 1
   
       IF STATUS = NOTFOUND THEN
	  DISPLAY "Procesando..." AT 19,2 ATTRIBUTE(REVERSE)
          EXIT INPUT
       ELSE
	  PROMPT "Cifra ya existe para esa fecha, desea continuar [S/N]..." 
	  ATTRIBUTE(REVERSE) FOR CHAR enter 
      WHILE TRUE
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
	       EXIT INPUT
            ELSE
		EXIT WHILE
            END IF
        END IF
      END WHILE

          SELECT MAX(a.hora)
          INTO max_hora
          FROM   safre_tmp:est_activas_apoyo a
          WHERE  a.fecha = vfecha_corte

	  SELECT a.nro_ctas_s1
          INTO   total_activas_s1
	  FROM   safre_tmp:est_activas_apoyo a
	  WHERE  a.fecha = vfecha_corte
          AND    a.hora  = max_hora

	  SELECT a.nro_ctas_s2
          INTO   total_activas_s2
	  FROM   safre_tmp:est_activas_apoyo a
	  WHERE  a.fecha = vfecha_corte
          AND    a.hora  = max_hora

          DISPLAY BY NAME total_activas_s1
          DISPLAY BY NAME total_activas_s2

          PROMPT "Enter para salir     ..." ATTRIBUTE(REVERSE) FOR CHAR x
	  EXIT PROGRAM

       END IF

       ON KEY(INTERRUPT)
	  EXIT PROGRAM


    END INPUT

    DISPLAY "Procesando..." AT 19,2 ATTRIBUTE(REVERSE)

WHENEVER ERROR CONTINUE
  DATABASE safre_tmp
  DROP TABLE tmp_nss
  CREATE TABLE tmp_nss(nss char(011),monto_en_acciones decimal(16,6))
  DATABASE safre_af
WHENEVER ERROR STOP

INSERT INTO safre_tmp:tmp_nss
SELECT A.nss,SUM(A.monto_en_acciones)
FROM   safre_af:dis_cuenta A
WHERE  A.fecha_conversion <= vfecha_corte
AND    A.subcuenta NOT in (4,8)
AND    A.nss not in (select b.nss
                     from safre_af:cta_act_marca b
                     WHERE b.marca_cod in (120,130,140)
                     AND   b.fecha_ini <= vfecha_corte)
GROUP BY A.nss HAVING SUM(A.monto_en_acciones) >= 1

SELECT COUNT(*)
INTO total_activas_s1
FROM safre_tmp:tmp_nss a,
     safre_af:cta_nss_regimen b
WHERE a.nss = b.nss
AND   b.siefore_rcv = 1

SELECT COUNT(*) - total_activas_s1
INTO total_activas_s2
FROM safre_tmp:tmp_nss 

DISPLAY BY NAME total_activas_s1--,vigentes
DISPLAY BY NAME total_activas_s2--,vigentes

LET hora = TIME

INSERT INTO safre_tmp:est_activas_apoyo
VALUES(vfecha_corte,total_activas_s1,total_activas_s2,HOY,hora,vigentes)

PROMPT "Enter para salir       ..." ATTRIBUTE (REVERSE) FOR CHAR x


--LET ejecuta = ". var1"
--RUN ejecuta

END MAIN

