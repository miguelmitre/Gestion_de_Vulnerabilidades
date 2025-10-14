DATABASE safre_af
GLOBALS
DEFINE enter char(001)
DEFINE normal_comp    SMALLINT
DEFINE  i                  INTEGER
DEFINE  vident             char(002)
DEFINE  tipo_archivo             char(20)
DEFINE vfolio   INTEGER
DEFINE HOY DATE
DEFINE dfecha_presentacion DATE
DEFINE cfecha_presentacion CHAR(008)
DEFINE vmotivo_paso        CHAR(003)
DEFINE areg_motivo   ARRAY[100] OF RECORD
       indice        INTEGER,
       motivo        CHAR(003),
       des_motivo    CHAR(030)
END RECORD
DEFINE reg_detalle RECORD 
       nss char(011),
       cont_serv integer,
       detalle char(002),
       consecutivo integer
END RECORD

DEFINE areg_detalle ARRAY[1000] OF RECORD 
       indice_det  integer  ,
       nss         char(011),
       cont_serv   integer  ,
       detalle     char(002),
       consecutivo integer,
       motivo      char(003)
END RECORD

END GLOBALS

MAIN
  OPTIONS PROMPT LINE LAST    ,
          INPUT WRAP          ,
          ACCEPT KEY CONTROL-I
          DEFER INTERRUPT

CALL init()
    OPEN WINDOW hism0011 AT 2,2 WITH FORM "HISM0011" ATTRIBUTE( BORDER)
    DISPLAY " HISM001           DATOS DEL HISTORICO DE TRASPASOS RECHAZADO                     " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
    DISPLAY "<Ctrl-C>Salir                                                                  " AT 1,1 ATTRIBUTE(REVERSE) 


INPUT BY NAME dfecha_presentacion,normal_comp
  AFTER FIELD dfecha_presentacion
        LET   cfecha_presentacion = dfecha_presentacion USING"YYYYMMDD" 

        SELECT "OK" 
	FROM   taa_his_rech_01 a
	WHERE  a.fecha_presentacion = cfecha_presentacion
	GROUP BY 1
	   
        IF STATUS = NOTFOUND THEN
	   ERROR"FECHA NO DISPONIBLE..."
	   NEXT FIELD dfecha_presentacion
        END IF

        IF dfecha_presentacion IS NULL THEN
	   ERROR"FECHA NO PUEDE SER NULA..." 
	   NEXT FIELD dfecha_presentacion
        END IF      

  AFTER FIELD normal_comp
        IF normal_comp         IS NULL THEN
	   ERROR"TIPO NO PUEDE SER NULO..." 
	   NEXT FIELD normal_comp
        END IF      

        IF (normal_comp <> 5 AND 
	   normal_comp <> 7 ) THEN
	   ERROR"TIPO INVALIDO..." 
	   NEXT FIELD normal_comp
        END IF      


 ON KEY(ESC)
        IF dfecha_presentacion IS NULL THEN
	   ERROR"FECHA NO PUEDE SER NULA..." 
	   NEXT FIELD dfecha_presentacion
        END IF      
        IF normal_comp         IS NULL THEN
	   ERROR"TIPO NO PUEDE SER NULO..." 
	   NEXT FIELD normal_comp
        END IF      

        IF (normal_comp <> 5 AND 
	   normal_comp <> 7 ) THEN
	   ERROR"TIPO INVALIDO..." 
	   NEXT FIELD normal_comp
        END IF      
        SELECT "OK" 
	FROM   taa_his_rech_01 a
	WHERE  a.fecha_presentacion = cfecha_presentacion
	AND    a.ident_operacion    = normal_comp

        IF STATUS = NOTFOUND THEN
	   ERROR"FECHA NO DISPONIBLE..."
	   NEXT FIELD dfecha_presentacion
        ELSE  
	   SELECT a.folio,a.ident_operacion
	   INTO vfolio,vident
	   FROM  taa_his_rech_01 a
	   WHERE a.fecha_presentacion = cfecha_presentacion
	   AND   a.ident_operacion = normal_comp

	   IF vident = "05" THEN
	      LET tipo_archivo = "NORMAL"
           END IF
	   IF vident = "07" THEN
	      LET tipo_archivo = "COMPLEMENTARIO"
           END IF
	END IF
	
        EXIT INPUT
  ON KEY(INTERRUPT)
	EXIT PROGRAM
  END INPUT


    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo1 
    FROM   taa_his_rech_02
    WHERE  folio = vfolio
    INSERT INTO motivo_paso 
    SELECT unique motivo_rechazo1 
    FROM   taa_his_rech_03
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo1 
    FROM   taa_his_rech_04
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo2 
    FROM   taa_his_rech_02
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo2 
    FROM   taa_his_rech_03
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo2 
    FROM   taa_his_rech_04
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo3 
    FROM   taa_his_rech_02
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo3
    FROM   taa_his_rech_03
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo3 
    FROM   taa_his_rech_04
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo4 
    FROM   taa_his_rech_02
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo4 
    FROM   taa_his_rech_03
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo4 
    FROM   taa_his_rech_04
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo5 
    FROM   taa_his_rech_02
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo5 
    FROM   taa_his_rech_03
    WHERE  folio = vfolio
    INSERT INTO motivo_paso
    SELECT unique motivo_rechazo5 
    FROM   taa_his_rech_04
    WHERE  folio = vfolio

    LET i = 0

    DECLARE cur_1 CURSOR FOR 
      SELECT a.*
      FROM   motivo_paso a
      WHERE  a.motivo <> " "
      GROUP BY 1
      ORDER BY 1

    FOREACH cur_1 INTO vmotivo_paso
      LET i = i + 1

      LET areg_motivo[i].indice = i
      LET areg_motivo[i].motivo = vmotivo_paso

      SELECT a.des_error
      INTO   areg_motivo[i].des_motivo
      FROM   tab_rch_icefa  a
      WHERE  a.cod_error = vmotivo_paso

    END FOREACH
    CALL muestra_motivos(i+1)

END MAIN

FUNCTION muestra_motivos(j) 
#mm----------------------

DEFINE j INTEGER
DEFINE pos INTEGER


    OPEN WINDOW hism0012 AT 2,2 WITH FORM "HISM0012" ATTRIBUTE( BORDER)
    DISPLAY " HISM001             DATOS DEL ARCHIVO RECHAZADO                               " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                         MOTIVOS DE RECHAZO                                       " AT 6,1 ATTRIBUTE(REVERSE) 
    DISPLAY "                     CONSECUTIVOS CON TIPO DE RECHAZO                          " AT 12,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
    DISPLAY "<Ctrl-C>Salir <F1>Detalle                                                      " AT 1,1 ATTRIBUTE(REVERSE) 

DISPLAY BY NAME dfecha_presentacion
DISPLAY BY NAME tipo_archivo
    CALL SET_COUNT(j-1)

    DISPLAY ARRAY areg_motivo TO scr_1.*

    ON KEY ( INTERRUPT )
	EXIT DISPLAY

    ON KEY (F1)
       LET pos = ARR_CURR()
       CALL muestra_detalle(areg_motivo[pos].motivo)

    END DISPLAY 
END FUNCTION 

FUNCTION muestra_detalle(fmotivo) 
#md------------------------------ 
DEFINE fmotivo CHAR(003) 
DEFINE k       INTEGER 
  INSERT INTO detalle_paso
  SELECT a.nss          , 
         a.cont_serv, 
	 "02" detalle          , 
         a.consecutivo 
  FROM   taa_his_rech_02 a 
  WHERE  a.folio = vfolio 
  AND    a.codigo_resultado = "02" 
  AND    (a.motivo_rechazo1 = fmotivo OR 
  	  a.motivo_rechazo2 = fmotivo OR 
	  a.motivo_rechazo3 = fmotivo OR 
	  a.motivo_rechazo4 = fmotivo OR 
	  a.motivo_rechazo5 = fmotivo ) 
  INSERT INTO detalle_paso
  SELECT a.nss          , 
         a.cont_serv, 
	 "03"   detalle        , 
         a.consecutivo
  FROM   taa_his_rech_03 a
  WHERE  a.folio = vfolio
  AND    a.codigo_resultado = "02"
  AND    (a.motivo_rechazo1 = fmotivo OR
	  a.motivo_rechazo2 = fmotivo OR 
	  a.motivo_rechazo3 = fmotivo OR
	  a.motivo_rechazo4 = fmotivo OR
	  a.motivo_rechazo5 = fmotivo )
  INSERT INTO detalle_paso
  SELECT a.nss          ,
	 a.cont_serv,
  	 "04"   detalle        ,
	 a.consecutivo
  FROM   taa_his_rech_04 a
  WHERE  a.folio = vfolio
  AND    a.codigo_resultado = "02"
  AND    (a.motivo_rechazo1 = fmotivo OR
	  a.motivo_rechazo2 = fmotivo OR 
	  a.motivo_rechazo3 = fmotivo OR
	  a.motivo_rechazo4 = fmotivo OR
	  a.motivo_rechazo5 = fmotivo )

   LET k = 0

   DECLARE cur_3 CURSOR FOR 
     SELECT a.*
     FROM   detalle_paso a
     GROUP BY 1,2,3,4
     ORDER BY 2,3,4

   FOREACH cur_3 INTO reg_detalle.*
  
     LET k = k + 1
     LET areg_detalle[k].indice_det     = k
     LET areg_detalle[k].nss            = reg_detalle.nss
     LET areg_detalle[k].cont_serv      = reg_detalle.cont_serv
     LET areg_detalle[k].detalle        = reg_detalle.detalle 
     LET areg_detalle[k].consecutivo    = reg_detalle.consecutivo
     LET areg_detalle[k].motivo         = fmotivo
  
   END FOREACH
    LET k = k + 1
    CALL SET_COUNT(k-1)

    DISPLAY ARRAY areg_detalle TO scr_2.*

    ON KEY ( INTERRUPT )
	EXIT DISPLAY

    END DISPLAY 
END FUNCTION

FUNCTION init()
#i-------------
LET HOY = TODAY


whenever error continue
DROP TABLE motivo_paso
DROP TABLE detalle_paso

    CREATE  TEMP TABLE motivo_paso(motivo char(003))
    CREATE  TEMP TABLE detalle_paso(    nss         char(011), 
                                   cont_serv   integer  ,
				   detalle     char(02) ,
				   consecutivo integer)
whenever error stop
END FUNCTION
