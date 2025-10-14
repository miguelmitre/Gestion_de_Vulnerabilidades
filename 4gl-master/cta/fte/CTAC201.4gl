#############################################################################
# Proyecto          => Sistema de Afores. (MEXICO)                          #
# Propietario       => E.F.P                                                #
# Programa RETM810  => Definicion de cuentas activas e inactivas y          #
#                      calculo de saldos                                    #
# Sistema           => CTA.                                                 #
# Autor             => ALEJANDRO CHAGOYA SALAZAR                            #
# Fecha             => 07 DE JULIO DE 2011                                  #
# Modifico          =>                                                      #
#############################################################################
DATABASE safre_tmp

DEFINE g_msj      CHAR(300),
       g_fecha ,
       g_hoy      DATE,
       g_hora     CHAR(8),
       g_enter    CHAR(1),
       g_cuenta   INTEGER,
       g_user     CHAR(8),
       g_log      CHAR(20),
       g_param    RECORD LIKE safre_af:seg_modulo.*,
       g_error    SMALLINT,
       g_prepare  CHAR(300)

MAIN

 DEFER INTERRUPT

CALL f_inicio()

  DISPLAY "INICIA PROCESO ", g_hora
  
  LET g_log = g_user CLIPPED, ".CTA201.log"
    
 	CALL STARTLOG	(g_log CLIPPED)
 	
 IF g_fecha IS NULL THEN
	LET g_msj ="ERROR EN PARAMETRO 1, FECHA ES NULA"
  DISPLAY g_msj   
  CALL ERRORLOG(g_msj CLIPPED)
  EXIT PROGRAM 1   
END IF 
  
  CALL f_asigna_2()
  CALL f_asigna_3()
  CALL f_upd_paso()
  LET g_hora = TIME
  DISPLAY "PROCESO FINALIZADO ", g_hora 

END MAIN

###############################################################################
FUNCTION f_inicio()

INITIALIZE g_fecha TO NULL
LET g_fecha = ARG_VAL(1)

LET g_cuenta = 0
LET g_msj    = " "
LET g_hora = TIME
LET g_hoy    = TODAY
LET g_enter  = " "
LET g_error = 0

SELECT s.*, USER 
INTO g_param.*,g_user
 FROM   safre_af:seg_modulo s
 WHERE  s.modulo_cod = "cta"

CREATE TEMP TABLE tmp_aporte(
   n_seguro             char(11),
   periodo_pago         char(6) );
  
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

--DISPLAY g_cuenta, " REGISTROS PARA PERIODO DE PAGO"

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
   
   DISPLAY g_cuenta, " REGISTROS ACTUALIZADOS EN SEGUNDA ASIGNACION"
   
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
       
		LET l_etapa  = 12
		LET l_result = "TERMINO CALCULO SALDOS"


LET l_result = l_result CLIPPED
       
UPDATE safre_af:dis_ctrl_proceso SET hora_final = l_hora,
                                     parametro2 = g_hoy,
                                     resultado = l_result,
                                     etapa_cod= l_etapa
WHERE proceso_cod = "CTAC200"
AND   parametro3 = g_fecha
 

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