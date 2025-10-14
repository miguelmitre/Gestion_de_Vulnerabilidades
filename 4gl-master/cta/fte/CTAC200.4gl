#############################################################################
# Proyecto          => Sistema de Afores. (MEXICO)                          #
# Propietario       => E.F.P                                                #
# Programa RETM810  => Definicion de cuentas activas e inactivas y          #
#                      calculo de saldos                                    #
# Sistema           => CTA.                                                 #
# Autor             => ALEJANDRO CHAGOYA SALAZAR                            #
# Fecha             => 30 DE JUNIO DE 2011                                  #
# Modifico          =>                                                      #
#############################################################################
DATABASE safre_tmp

DEFINE g_msj      CHAR(300),
       g_fecha ,
       g_hoy      DATE,
       g_enter    CHAR(1),
       g_cuenta   INTEGER,
       g_user     CHAR(8),
       g_log      CHAR(20),
       g_param    RECORD LIKE safre_af:seg_modulo.*,
       g_error    SMALLINT,
       g_prepare  CHAR(300),
       g_run      CHAR(300),
       g_paso     SMALLINT,
       g_hora     DATETIME HOUR TO SECOND,
       g_impre    CHAR(150)

MAIN

 DEFER INTERRUPT
 OPTIONS
     INPUT WRAP           ,
     PROMPT LINE LAST     ,
     ACCEPT KEY CONTROL-I

  CALL f_inicio()

  LET g_log = g_user CLIPPED, ".CTAC200.log"
  
	CALL STARTLOG	(g_log CLIPPED)
  
  
  CALL f_principal()

END MAIN

###############################################################################
FUNCTION f_inicio()

LET g_cuenta = 0
LET g_msj    = " "
LET g_hoy    = TODAY
LET g_enter  = " "
LET g_error = 0
LET g_run = " "
LET g_paso = 0
LET g_impre = " "

SELECT s.*, USER 
INTO g_param.*,g_user
 FROM   safre_af:seg_modulo s
 WHERE  s.modulo_cod = "cta"
-----------------
 --DATABASE safre_tmp
 LET g_prepare = " "
 LET g_prepare = "EXECUTE PROCEDURE amafore(?)"
 PREPARE eje_amafore FROM g_prepare
 --DATABASE safre_af
----------------- 

LET g_prepare = " "
LET g_prepare = "EXECUTE PROCEDURE sp_saldo_asignadas(?)"
PREPARE eje_saldo FROM g_prepare


 CREATE TEMP TABLE tmp_periodo(
  nss   CHAR(11),
  periodo_pago CHAR(6)
  );
  
  CREATE INDEX ix1_tmp_periodo ON tmp_periodo (nss);
  
  
CREATE TEMP TABLE tmp_aporte(
   n_seguro             char(11),
   periodo_pago         char(6) );
  
END FUNCTION

###############################################################################
FUNCTION f_principal()

CALL f_ventana()
      MENU "Asignadas"
         COMMAND "Cuota" "Identifica cuentas a procesar"
         LET g_paso = 1
            CALL f_captura() RETURNING g_error
            IF g_error = 0 THEN
            	  CALL f_valida() RETURNING g_error
            	  IF g_error = 0 THEN
                WHENEVER ERROR CONTINUE
             	    EXECUTE eje_amafore USING g_fecha
                WHENEVER ERROR STOP
                CALL f_paso()
                --CALL f_load()
                CALL f_insert()
                CALL f_upd_paso()
                ERROR ""
                CLEAR FORM 
               ELSE
                	ERROR g_msj
               END IF	 
           ELSE
          	  CLEAR FORM
           END IF
            
         COMMAND "Asigna" "Define estatus de cuentas a procesar"
         LET g_paso = 2
              	  CALL f_captura() RETURNING g_error
                      IF g_error = 0 THEN
                  	      CALL f_valida() RETURNING g_error
                    	    IF g_error = 0 THEN
                     	    	--ERROR "PROCESANDO INFORMACION"
                     	    	
                     	    	 LET g_run = "nohup time fglgo CTAC201.4gi ",
                                          g_fecha ,
                                          " 1> ", g_user CLIPPED, ".cta_asig.salida ",
                                          "2> ", g_user CLIPPED, ".cta_asig.error &"

   	  	                       ERROR "EJECUTANDO PROCESO POR NOHUP."
                               SLEEP 2
                               ERROR ""
                               RUN  g_run
                               PROMPT " <ENTER> PARA SALIR" ATTRIBUTE(REVERSE) FOR CHAR g_enter
                            --CALL f_asigna_2()
                            --CALL f_asigna_3()
                            --CALL f_upd_paso()
                          ELSE
                        	  ERROR g_msj
                      END IF
                     ELSE
          	            CLEAR FORM
                     END IF 	

         COMMAND "Saldos" "Genera reporte de saldos"
               LET g_paso = 3
               CALL f_captura() RETURNING g_error
            IF g_error = 0 THEN
                 CALL f_valida() RETURNING g_error
                  IF g_error = 0 THEN
                   CALL f_reporte()
                   CALL f_upd_paso()
                  ELSE 
                  	ERROR g_msj
                  	CLEAR FORM
                END IF
           ELSE
          	  CLEAR FORM
           END IF 
                
         
         COMMAND "Salir" "Salir del programa"
            EXIT PROGRAM
      END MENU
CLOSE WINDOW cta2001

END FUNCTION

###############################################################################
FUNCTION f_captura()
DEFINE l_fecha DATE

LET g_error = 0
INITIALIZE l_fecha, g_fecha TO NULL

CALL f_display()
CLEAR FORM

   INPUT BY NAME l_fecha
      BEFORE INPUT   	
   	  	LET l_fecha = g_hoy
   	    DISPLAY BY NAME l_fecha
   	    
   	    
   	  AFTER FIELD l_fecha
       IF l_fecha IS NULL THEN
          ERROR " LA FECHA NO PUEDE SER NULO"
          NEXT FIELD l_fecha
	    ELSE
          CALL f_fecha(l_fecha) RETURNING g_error, g_fecha
          
          IF g_error = 1 THEN
          	ERROR "FECHA INVALIDA, VERIFIQUE"
          	NEXT FIELD l_fecha
          ELSE	
            DISPLAY g_fecha TO l_fecha
          END IF  
	    END IF
	    
   	  
   	  ON KEY (ESC)
   	  IF l_fecha IS NULL THEN
          ERROR " LA FECHA NO PUEDE SER NULO"
	        NEXT FIELD l_fecha
	    END IF    
   	  	
          WHILE TRUE
              PROMPT "¿ EJECUTAR PROCESO ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR g_enter
              IF g_enter MATCHES "[sSnN]" THEN
                  IF g_enter MATCHES "[sS]" THEN
                  	  ERROR "PROCESANDO INFORMACION"
                  	  LET g_error = 0
                      EXIT INPUT
                  ELSE
                      ERROR " PROCESO CANCELADO" SLEEP 2
                      LET g_error = 1
                      CLEAR FORM
                      EXIT INPUT
                  END IF
              END IF
          END WHILE   	

      ON KEY (INTERRUPT, CONTROL-C)
          ERROR " PROCESO CANCELADO" SLEEP 2
          LET g_error = 1
          CLEAR FORM
          EXIT INPUT
  
   END INPUT 

RETURN g_error
END FUNCTION

###############################################################################
FUNCTION f_insert()
DEFINE l_nss     CHAR(11),
       l_status  SMALLINT

-------------------------------------------------------
--Los que NO existen en tabla de historicos, son inactivas en un principio
  DECLARE cur_cuota2 CURSOR FOR
   SELECT UNIQUE(a.n_seguro)
    FROM safre_tmp:cuota a 
    WHERE a.tipo_solicitud = 5 
    AND a.n_seguro NOT IN (SELECT b.nss FROM safre_tmp:cta_his_asig b )
  	
  LET g_cuenta = 0	
  LET l_nss = " "
  
  	FOREACH cur_cuota2 INTO l_nss
       INSERT INTO safre_tmp:cta_his_asig VALUES(l_nss, g_fecha, TODAY, 0)
       	
        IF SQLCA.SQLERRD[3] = 0 THEN   --Si NO actualiza
        		LET g_msj ="ERROR AL INSERTAR_0 cta_his_asig, NSS: ", l_nss
        		--DISPLAY g_msj AT 17,2
        		CALL ERRORLOG(g_msj CLIPPED)
        		CONTINUE FOREACH
        END IF
        LET g_cuenta = g_cuenta + 1
      		  	
     END FOREACH	
  DISPLAY g_cuenta, " REGISTROS INSERTADOS EN cta_his_asig SOL_5" AT 13,2
  
-------------------------------------------------------
--Los que NO existen en tabla de cuota, son trspasadas

  DECLARE cur_cuota1 CURSOR FOR 
  	SELECT a.nss FROM safre_tmp:cta_his_asig a
  	WHERE a.nss NOT IN (SELECT n_seguro FROM safre_tmp:cuota)
  	AND a.estado != 2
 	
  LET g_cuenta = 0	
  LET l_nss = " "
  	FOREACH cur_cuota1 INTO l_nss
      	UPDATE safre_tmp:cta_his_asig SET estado = 2
      	WHERE nss = l_nss
      	AND estado != 2
       	
        IF SQLCA.SQLERRD[3] = 0 THEN   --Si NO actualiza
        		LET g_msj ="ERROR AL ACTUALIZAR_2 cta_his_asig, NSS: ", l_nss
        		--DISPLAY g_msj AT 17,2
        		CALL ERRORLOG(g_msj CLIPPED)
        		CONTINUE FOREACH
        END IF
        LET g_cuenta = g_cuenta + 1
      		  	
     END FOREACH	
  DISPLAY g_cuenta, " REGISTROS ACTUALIZADOS EN cta_his_asig TRASP" AT 14,2


-------------------------------------------------------
--Los que existen en las dos tablas pero el tipo de solicitus es !=5 y
--el estado es diferente de 3(registrada) se actualiza a 3
LET l_nss    = " "
LET g_cuenta = 0
  
  DECLARE cur_cuota CURSOR FOR
   SELECT UNIQUE(a.n_seguro)
    FROM safre_tmp:cuota a , safre_tmp:cta_his_asig b
    WHERE a.n_seguro = b.nss
    AND a.tipo_solicitud != 5
    AND b.estado != 3
    
     FOREACH cur_cuota INTO l_nss
       	      	UPDATE safre_tmp:cta_his_asig SET estado = 3
      	        WHERE nss = l_nss
       	
        IF SQLCA.SQLERRD[3] != 1 THEN   --Si NO inserta
        		LET g_msj ="ERROR AL ACTUALIZAR_3 EN cta_his_asig, NSS: ", l_nss
        		--DISPLAY g_msj AT 17,2
        		CALL ERRORLOG(g_msj CLIPPED)
        		CONTINUE FOREACH
        END IF
        LET g_cuenta = g_cuenta + 1
      		  	
      END FOREACH	
  DISPLAY g_cuenta, " REGISTROS ACTUALIZADOS EN cta_his_asig REG" AT 15,2
  
-------------------------------------------------------  

END FUNCTION  


###############################################################################
FUNCTION f_ventana()

    OPEN WINDOW cta2001 AT 3,3 WITH FORM "CTAC2001" ATTRIBUTE (BORDER)

    DISPLAY " CTAC200         	CUENTAS ASIGNADAS/DESASIGNADAS                       " AT 6,1 ATTRIBUTE(REVERSE)
    DISPLAY g_hoy USING "DD-MM-YYYY"  AT 6,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir           [ESC ] Aceptar " AT 5,1

END FUNCTION



###############################################################################
FUNCTION f_fecha(p_fecha)
DEFINE p_fecha, l_fecha   DATE,
       dia, mes, anio     SMALLINT

INITIALIZE l_fecha TO NULL
LET dia  = 0
LET mes  = 0
LET anio = 0

 LET dia  = DAY(p_fecha)
 LET mes  = MONTH(p_fecha)
 LET anio = YEAR(p_fecha)
 
 IF dia = 0 OR mes = 0 OR anio = 0 THEN
 	  LET g_error = 1
 END IF	
 
 LET l_fecha = MDY(mes, dia, anio)

RETURN g_error, l_fecha 

END FUNCTION 


###############################################################################
FUNCTION f_asigna_1()
DEFINE l_nss CHAR(11)

LET g_cuenta = 0
LET l_nss    = " "

DECLARE cur_asig CURSOR FOR
  SELECT h.nss
   FROM safre_tmp:cta_his_asig h, safre_af:cta_ctr_cuenta c
   WHERE h.nss = c.nss
   AND h.fecha_corte = g_fecha
   AND c.fecha_ult_general > (g_fecha - 1 UNITS YEAR)
   AND h.estado = 0
  	
  	FOREACH cur_asig INTO l_nss
  	  UPDATE safre_tmp:cta_his_asig SET ESTADO = 1
  	         WHERE nss = l_nss
  	
  	  LET g_cuenta = g_cuenta + SQLCA.SQLERRD[3]
  	
   END FOREACH	
 
DISPLAY g_cuenta," REGISTROS ACTUALIZADOS EN PRIMERA ASIGNACION" AT 16,2
        		        		
END FUNCTION

###############################################################################
FUNCTION f_asigna_2()
DEFINE l_periodo_pago CHAR(6),
       l_nss     CHAR(11)
       
LET l_nss = " "       
LET g_cuenta  = 0

DECLARE cur_asig2 CURSOR FOR
	
  SELECT h.nss
   FROM safre_tmp:cta_his_asig h
   WHERE h.estado = 0
   
 	FOREACH cur_asig2 INTO l_nss
 		DECLARE cur_pago CURSOR FOR
 			SELECT d.periodo_pago
 			FROM safre_af:dis_det_aporte d
 			WHERE d.n_seguro = l_nss
      
       LET l_periodo_pago = " "
 		  FOREACH cur_pago INTO l_periodo_pago

         INSERT INTO tmp_aporte VALUES(l_nss, l_periodo_pago)

 		  END FOREACH 	
 		  LET g_cuenta = g_cuenta + 1
  END FOREACH 		 	 

CREATE INDEX ix2_tmp_aporte ON tmp_aporte(n_seguro)
UPDATE STATISTICS FOR TABLE tmp_aporte;

--DISPLAY g_cuenta, " REGISTROS PARA PERIODO DE PAGO" AT 16,2

	{UNLOAD TO "acs.acs"
	SELECT * FROM tmp_aporte}

END FUNCTION



###############################################################################
FUNCTION f_asigna_3()
DEFINE l_cuenta, i, x  SMALLINT,
       l_nss           CHAR(11),
       l_anio, l_bim   SMALLINT,
       a_pago ARRAY[200] OF RECORD
         anio, bim SMALLINT
       END RECORD,
       l_asig                  ,
       l_next_anio             , 
       l_next_bim      SMALLINT

LET l_nss = " "
LET g_cuenta = 0
LET l_cuenta = 0

DECLARE cur_asig3 CURSOR FOR 
   SELECT a.n_seguro, COUNT(*) FROM tmp_aporte a
   GROUP BY 1 
   HAVING COUNT(*) > 5

   FOREACH cur_asig3 INTO l_nss, l_cuenta
  	
     FOR i = 1 TO 200
	     LET a_pago[i].bim  = 0
	     LET a_pago[i].anio = 0
     END FOR 	
     
        DECLARE cur_asig4 CURSOR FOR 
         SELECT UNIQUE b.periodo_pago[1,4], b.periodo_pago[5,6]
         FROM tmp_aporte b
         WHERE b.n_seguro = l_nss
         ORDER BY 1,2
         
      	   LET x = 1
      	   FOREACH cur_asig4 INTO a_pago[x].anio, a_pago[x].bim
            	IF X > 199 THEN --excede capacidad del arreglo
            		   EXIT  FOREACH	
      	   	  END IF
      	      LET x = x + 1
      	   END FOREACH
      	   
       LET l_asig = 1   	   
   	   FOR i = 1 TO x
         LET l_next_anio = 0 
         LET l_next_bim = 0
         CALL f_next(a_pago[i].anio, a_pago[i].bim)
           RETURNING l_next_anio, l_next_bim
           
          IF a_pago[i+1].anio = l_next_anio AND a_pago[i+1].bim = l_next_bim THEN
          	 LET l_asig = l_asig + 1
          ELSE
          	 LET l_asig = 1
          	 CONTINUE FOR 	 
          END IF
          
          IF l_asig = 6 THEN
          	
          	  UPDATE safre_tmp:cta_his_asig SET estado = 1
          	  WHERE nss = l_nss
          	  
          	    LET g_cuenta = g_cuenta + SQLCA.SQLERRD[3]
          	    LET l_asig = 1
              EXIT FOR	
          END IF
        
       END FOR
   END FOREACH
   
   DISPLAY g_cuenta, " REGISTROS ACTUALIZADOS EN SEGUNDA ASIGNACION" AT 16,2
   
END FUNCTION
###############################################################################
FUNCTION f_next(p_anio, p_bim)
DEFINE p_bim, p_anio,
       l_bim, l_anio SMALLINT
       

LET l_bim  = 0
LET l_anio = 0

LET l_anio = p_anio

    CASE p_bim 
    	WHEN 2
    		LET l_bim = 4
    	WHEN 4
    		LET l_bim = 6		
    	WHEN 6
    		LET l_bim = 8
    	WHEN 8
    		LET l_bim = 10
    	WHEN 10
    		LET l_bim = 12
    	WHEN 12
    		LET l_bim = 2
    		LET l_anio = p_anio + 1
    END CASE

RETURN l_anio, l_bim

END FUNCTION

###############################################################################
FUNCTION f_load()
DEFINE l_nss CHAR(11),
       l_ruta CHAR(100)
       
LET l_ruta = " "
LET l_nss = " "
LET g_cuenta = 0
WHENEVER ERROR CONTINUE
CREATE TEMP TABLE tmp_inac
( nss CHAR(11) );

LET l_ruta = g_param.ruta_rescate CLIPPED, "INACTIVAS.txt"

LET g_run = "ls ", l_ruta CLIPPED

RUN g_run RETURNING g_error

IF g_error != 0 THEN
   DISPLAY "NO EXISTE ARCHIVO DE CARGA ", l_ruta CLIPPED AT 12,2
ELSE    

    LOAD FROM l_ruta
    INSERT INTO tmp_inac
    
    DECLARE cur_inac CURSOR FOR 
    	SELECT UNIQUE c.nss FROM tmp_inac c
    	       WHERE c.nss NOT IN (SELECT a.nss FROM safre_tmp:cta_his_asig a)
    	
    	FOREACH cur_inac INTO l_nss
    	   INSERT INTO safre_tmp:cta_his_asig VALUES(l_nss, g_fecha, TODAY, 0)
    	   
    	   LET g_cuenta = g_cuenta + SQLCA.SQLERRD[3]
    	END FOREACH 
    
    DISPLAY g_cuenta, " REGISTROS CARGADOS DE ARCHIVO " AT 12,2
    
    UPDATE STATISTICS FOR TABLE safre_tmp:cta_his_asig

END IF	
	
	WHENEVER ERROR STOP

END FUNCTION 


###############################################################################
FUNCTION f_upd_paso()
DEFINE l_hora    CHAR(8),
       l_result  CHAR(100),
       l_etapa   SMALLINT
       
LET l_etapa = 0       
LET g_hora = TIME
LET g_hoy = TODAY
LET l_hora = g_hora       
       
CASE g_paso 
	WHEN 1
		LET l_etapa  = 11
		LET l_result = "TERMINO INSERT DE CUENTAS"
	WHEN 2
		LET l_etapa  = 12
		LET l_result = "TERMINO VALIDACION DE ASIGNADAS"
	WHEN 3
		LET l_etapa  = 13
		LET l_result = "TERMINO CALCULO SALDOS"
END CASE

LET l_result = l_result CLIPPED
       
UPDATE safre_af:dis_ctrl_proceso SET hora_final = l_hora,
                                     parametro2 = g_hoy,
                                     resultado = l_result,
                                     etapa_cod= l_etapa
WHERE proceso_cod = "CTAC200"
AND   parametro3 = g_fecha
 

END FUNCTION

###############################################################################
FUNCTION f_paso ()
DEFINE l_hora    CHAR(8),
       l_result  CHAR(100)
       
       
LET g_hora = TIME
LET g_hoy = TODAY
LET l_hora = g_hora

CASE g_paso 
	WHEN 1
		LET l_result = "INSERTA CUENTAS"
	WHEN 2
		LET l_result = "VALIDA ASIGNADAS"
	WHEN 3
		LET l_result = "CALCULA SALDOS"
	
END CASE

INSERT INTO safre_af:dis_ctrl_proceso VALUES(
            TODAY       ,--fecha_proceso
            "CTAC200"   ,--proceso_cod  
            g_paso      ,--etapa_cod    
            l_hora      ,--hora_inicial 
            NULL        ,--hora_final   
            g_hoy       ,--parametro1   --fecha_ini
            NULL        ,--parametro2   --fecha_fin
            g_fecha     ,--parametro3   --fecha_corte   
            NULL        ,--parametro4   
            NULL        ,--parametro5   
            0           ,--folio        
            l_result    ,--resultado    
           g_user       ,--usuario      
                       0)--consecutivo  

END FUNCTION  


###############################################################################
FUNCTION f_valida()
DEFINE l_etapa SMALLINT

LET l_etapa = 0
LET g_error = 0         

SELECT a.etapa_cod INTO l_etapa
FROM safre_af:dis_ctrl_proceso a
WHERE  a.proceso_cod = "CTAC200"
AND    a.parametro3 = g_fecha

IF STATUS = NOTFOUND THEN
  CASE g_paso
   WHEN 2
   	LET g_error = 1
   	LET g_msj = "AUN NO TERMINA LA DEFINICION"
   WHEN 3
   	LET g_error = 1
   	LET g_msj = "AUN NO TERMINA LA ASIGNACION"
  END CASE
  
ELSE 
	
	 CASE g_paso
	 	WHEN 1
	 		 LET g_error = 1
   	   LET g_msj = "YA SE EJECUTO PARA ESTA FECHA"
   WHEN 2
   	  IF l_etapa = g_paso + 10 THEN
    	   LET g_error = 1
    	   LET g_msj = "YA SE EJECUTO PARA ESTA FECHA"
      ELSE 
      	IF l_etapa != g_paso + 9 THEN
      	   LET g_error = 1
    	     LET g_msj = "AUN NO TERMINA LA DEFINICION"
      END IF
     END IF  
    
   WHEN 3
   		IF l_etapa = g_paso + 10 THEN
   	   LET g_error = 1
   	   LET g_msj = "YA SE EJECUTO PARA ESTA FECHA"
   	  ELSE
    	 	IF l_etapa != g_paso + 9 THEN
   	     LET g_error = 1
   	     LET g_msj = "AUN NO TERMINA LA ASIGNACION"
      END IF    
   	END IF   
   END CASE

END IF


RETURN g_error

END FUNCTION 

###############################################################################
FUNCTION f_reporte()
DEFINE l_datos       CHAR(150),
       l_encabezado  CHAR(150)
       
       
       LET l_datos = " "
       LET l_encabezado =" "
 
WHENEVER ERROR CONTINUE
   EXECUTE eje_saldo USING g_fecha
WHENEVER ERROR STOP

LET g_impre = g_param.ruta_listados CLIPPED, "/", g_user CLIPPED,
              ".RPT_SALDOS_CUENTAS.",g_hoy USING "ddmmyy"

--LET g_impre = g_user CLIPPED,
  --            ".RPT_SALDOS_CUENTAS.",g_hoy USING "ddmmyy"              
              

LET l_datos = g_param.ruta_listados CLIPPED, "/","DATA_asig.",g_hoy USING "ddmmyy"
LET l_encabezado =g_param.ruta_listados CLIPPED, "/","ENC_asig.",g_hoy USING "ddmmyy"

{DISPLAY "datos ",l_datos
DISPLAY "l enca ", l_encabezado
DISPLAY "impre ", g_impre}

UNLOAD TO l_datos
	SELECT a.estado, a.subcuenta,
	       SUM(a.pesos), SUM(a.acciones), COUNT(*)
	FROM safre_tmp:tmp_det_asigna a
	WHERE a.acciones > 0
	GROUP BY 1,2
	ORDER BY 1,2
 
START REPORT report_saldo TO l_encabezado

   OUTPUT TO REPORT	report_saldo()
	

FINISH REPORT report_saldo

LET g_run ="cat ", l_encabezado CLIPPED, " ", l_datos CLIPPED , " > ", g_impre

--DISPLAY "run ",g_run SLEEP 3
RUN g_run RETURNING g_error

IF g_error != 0 THEN
	 LET g_msj ="	ERROR AL CREAR REPORTE", g_run
	 DISPLAY g_msj
	 CALL ERRORLOG(g_msj CLIPPED)
ELSE
		
		DISPLAY "VALIDAR ARCHIVO: ", g_impre CLIPPED AT 15,2
	  LET g_run = "rm ", l_datos CLIPPED, " ", l_encabezado CLIPPED
	  RUN g_run
END IF

END FUNCTION

###############################################################################
REPORT report_saldo ()


   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, 'ESTADO'                , '|',
                       'SUBCUENTA'             , '|',
                       'SALDO_PESOS'           , '|',
                       'SALDO_ACCIONES'        , '|',
                       'REGISTROS'             , '|'
                       
END REPORT



###############################################################################
FUNCTION f_display()

DISPLAY "                                                               " AT 12,2
DISPLAY "                                                               " AT 13,2
DISPLAY "                                                               " AT 14,2
DISPLAY "                                                               " AT 15,2
DISPLAY "                                                               " AT 16,2


END FUNCTION