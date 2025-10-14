-------------------------------------------------------------------------------
-- Modulo            => DIS
-- Programa          => DISL020
-- Descripcion       => Programa que genera saldos diarios
-- Autor             => Luis Avila
-- Fecha ult modi.   => 07 julio 2004
-- Autor             => Gerardo Alfonso Vega Paredes
-- Descripcion modi. => Adecuacion para participaciones (codigo_siefore)
-- Fecha ult modi.   => 26 noviembre 2004
-- Autor             => Gerardo Alfonso Vega Paredes
-- Descripcion modi. => Adecuacion para que tome nuevas subcuentas
-------------------------------------------------------------------------------
-- Fecha ult modi.   => 07 Diciembre 2005
-- Autor             => Alejandro Ramirez
-- Descripcion modi. => v2 Para la subcuenta 10 obtengo el precio de la accion
--                   => de la siefore 3
-------------------------------------------------------------------------------
-- Fecha ult modi.   => 03 Mayo 2006
-- Autor             => Alejandro Ramirez
-- Descripcion modi. => v3 Ingresar la subcuenta 19 en una vari. igual a la 14
-------------------------------------------------------------------------------
-- Fecha ult modi.   =>    Diciembre 2011
-- Autor             => JGHM              
-- Descripcion modi. => Adecuar a condiciones de req. punto 2.5                   
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE saldo RECORD
      siefore                   SMALLINT,
      subcuenta 		SMALLINT,
      ord                       SMALLINT, 
      pesos                     DECIMAL(27,6),
      acciones			DECIMAL(27,6),
      pesos_viv 		DECIMAL(27,6),
      descripcion               CHAR(40)
   END RECORD

--   DEFINE v_precio_accion     DECIMAL(11,6)      ----jerry
   DEFINE v_precio_accion10     DECIMAL(19,14)   --v2
   DEFINE v_precio_accion11     DECIMAL(19,14)   ----jerry
   DEFINE v_prioridad_on        CHAR(25)
   DEFINE v_prioridad_off       CHAR(25)
   DEFINE query                 CHAR(1200)
   DEFINE ruta_reporte	        CHAR(40)
   DEFINE opc                   CHAR(1)
   DEFINE enter                 CHAR(1)
   DEFINE i                     SMALLINT
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST

   FOR i=1 TO 13
      IF i=1 OR i=2 OR i=3 OR i=4 OR i=5 OR i=6 OR i=11 OR i=12 OR i=13 THEN
         SELECT "OK"
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = TODAY
         AND    codigo_siefore  = i

         IF STATUS = NOTFOUND THEN
            ERROR "Falta cargar precio del dia de la siefore ",i USING "&&"
         END IF
      END IF
      SLEEP 1
   END FOR

   WHILE TRUE
      PROMPT " DESEA LANZAR EL PROCESO S/N : ? " FOR CHAR enter
      IF enter MATCHES '[SsNn]' THEN
         EXIT WHILE
      END IF
   END WHILE

   IF enter = "S" OR enter = "s" THEN
      CALL obten_saldo()
   ELSE
      ERROR " PROCESO CANCELADO !!! "
      SLEEP 3
   END IF
END MAIN


FUNCTION obten_saldo()

   SELECT ruta_listados
   INTO   ruta_reporte
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"
 
   LET v_prioridad_on  = "SET PDQPRIORITY HIGH"
   LET v_prioridad_off = "SET PDQPRIORITY OFF"
 
   PREPARE eje_pri_on  FROM v_prioridad_on
   PREPARE eje_pri_off FROM v_prioridad_off
 
   EXECUTE eje_pri_on

   ERROR "Generando listado de saldos diario, por favor espere ..."

   LET query ="SELECT siefore,subcuenta,siefore ord,",
              "      SUM(monto_en_acciones) monto_en_acciones,",
              "      SUM(monto_en_pesos) monto_en_pesos_viv ",
              "FROM  dis_cuenta ",
              "WHERE siefore = 10 ",
              "GROUP BY 1,2,3 ",  
              "ORDER BY 3,1,2 ",
              "INTO TEMP tmp_saldo "

   PREPARE query_1 FROM query
   EXECUTE query_1

   EXECUTE eje_pri_off

   LET ruta_reporte = ruta_reporte CLIPPED,"/SALDOS_DIARIOS.",
                      TODAY USING "DDMMYYYY"

   START REPORT rep_saldo TO ruta_reporte

   DECLARE c_saldo CURSOR FOR
   SELECT s.siefore,s.subcuenta,s.ord,s.monto_en_acciones,
          s.monto_en_pesos_viv,
          t.subct_desc
   FROM   tmp_saldo s, tab_subcuenta t
   WHERE  t.subct_cod = s.subcuenta
   ORDER  BY s.ord,s.subcuenta

   FOREACH c_saldo INTO saldo.siefore,saldo.subcuenta,saldo.ord,
                        saldo.acciones,saldo.pesos_viv,
                        saldo.descripcion

      LET v_precio_accion10 = 0                     --v2

      SELECT precio_del_dia                         --v2
      INTO v_precio_accion10                        --v2
      FROM glo_valor_accion                         --v2
      WHERE fecha_valuacion = TODAY                 --v2
      AND   codigo_siefore  = saldo.siefore         --v2

      IF v_precio_accion10 IS NULL THEN             --v2
         LET v_precio_accion10 = 0                  --v2
      END IF                                        --v2
      LET saldo.pesos = saldo.acciones * v_precio_accion10   --v2

      ---   Dic 2011, Para siefore 10-Desinversion Solo debe presentar las subcuentas 
      ---   1, 2,5, 6, 7, 9  menos elegante pero efectivo

      IF    saldo.subcuenta    =   1   
        OR  saldo.subcuenta    =   2  
        OR  saldo.subcuenta    =   5  
        OR  saldo.subcuenta    =   6  
        OR  saldo.subcuenta    =   7
        OR  saldo.subcuenta    =   9    THEN 
            OUTPUT TO REPORT rep_saldo ( saldo.* )
      END IF
   END FOREACH
 
   CLOSE c_saldo
 
   FINISH REPORT rep_saldo
 
   ERROR "Listado generado: ",ruta_reporte
   SLEEP 5
END FUNCTION


REPORT rep_saldo ( saldo )
   DEFINE
      saldo RECORD
               siefore          SMALLINT,
               subcuenta 	SMALLINT,
               ord              SMALLINT,
               pesos    	DECIMAL(27,6),
               acciones		DECIMAL(27,6),
               pesos_viv        DECIMAL(27,6),
               descripcion      CHAR(40)
            END RECORD
 
   FORMAT
 
   FIRST PAGE HEADER
 
   SKIP  1 LINE
   PRINT COLUMN  5, "Fecha : " CLIPPED, TODAY
   SKIP  2 LINE
   PRINT COLUMN 20, " SALDO DIARIO POR SUBCUENTA"
   PRINT COLUMN  5, "============================================================"

   ON EVERY ROW
      PRINT COLUMN 5, "Siefore   = " CLIPPED,saldo.siefore
      PRINT COLUMN 5, "Subcuenta = " CLIPPED,saldo.subcuenta,
                   2 SPACES, saldo.descripcion

      IF saldo.subcuenta = 19 THEN  --v3
         PRINT COLUMN 5, "Pesos     = ", 2 SPACES,
                         saldo.pesos_viv USING "$#####,######,######.######"
      ELSE
         PRINT COLUMN 5, "Pesos     = ", 2 SPACES,
                   --    saldo.pesos USING "$###,###,###.######"  ALEX
                         saldo.pesos USING "$#####,######,######.######"
      END IF

      ---   Dic 2011, Para siefore 10-Desinversion no hay acciones 
      IF    saldo.siefore        <>  10 THEN 
            PRINT COLUMN 5, "Acciones  = ", 2 SPACES,
                        --  saldo.acciones USING "$###,###,###.######"  ALEX
                            saldo.acciones USING "$#####,######,######.######"
      END IF 
      SKIP 1 LINE

END REPORT

