DATABASE safre_af
   DEFINE
      reg_1 RECORD 
               fecha_conversion DATE,
               tipo  char(01)
            END RECORD,

      x_fecha_ini_prov DATE

MAIN
  DECLARE cur1 CURSOR FOR
  SELECT fecha_valuacion, "M" from glo_valor_accion
  WHERE fecha_valuacion between "03/01/2012" AND "04/01/2012"
    and codigo_siefore = 5 

  FOREACH cur1 INTO reg_1.*
   IF reg_1.tipo = "M" THEN
      LET x_fecha_ini_prov = f_calculo(reg_1.fecha_conversion) +1
   ELSE
      LET x_fecha_ini_prov = "04/01/2012"
   END IF

   DISPLAY reg_1.fecha_conversion," ",x_fecha_ini_prov
  END FOREACH
END MAIN


FUNCTION f_calculo (x_fecha)
   DEFINE diaSemana	  SMALLINT,
          feriado	  SMALLINT,
          finSemana	  SMALLINT,
          x_fecha	  DATE,
          x_fecha_calculo DATE

   LET diaSemana = NULL
   LET feriado   = NULL
   LET finSemana = NULL

   LET diaSemana = WEEKDAY(x_fecha + 1)  

   IF diaSemana = 0 OR diaSemana = 6 THEN
      LET finSemana = 1
   END IF

   SELECT *
   FROM   tab_feriado 
   WHERE  feria_fecha = (x_fecha + 1)  

   IF SQLCA.SQLCODE <> NOTFOUND THEN
      LET feriado = 1
   END IF 

   IF feriado = 1 OR finSemana = 1 THEN
      CALL habil_siguiente(x_fecha + 1) 
      RETURNING x_fecha_calculo
      LET x_fecha_calculo = x_fecha_calculo - 1
   ELSE
      LET x_fecha_calculo = x_fecha
   END IF

   RETURN x_fecha_calculo
END FUNCTION


FUNCTION habil_siguiente(diaActual)
   DEFINE
      diaTmp       DATE,
      contador     SMALLINT,
      diaActual    DATE
   
   DEFINE
      diaHabilSig  DATE,
      diaSemana    SMALLINT,
      feriado      SMALLINT,
      finSemana    SMALLINT
	  
   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
     	 LET finSemana = 1
      END IF
  	     
      SELECT *
      FROM   tab_feriado 
      WHERE  feria_fecha = diaHabilSig
    	
      IF SQLCA.SQLCODE <> NOTFOUND THEN
         LET feriado = 1
      END IF 
		
      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION
