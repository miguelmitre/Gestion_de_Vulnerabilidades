DATABASE safre_af

DEFINE saldo RECORD
       subcuenta 		SMALLINT,
       pesos    		DECIMAL(16,6),
       acciones			DECIMAL(16,6),
       pesos_viv 		DECIMAL(16,6),
       descripcion              CHAR(40)
END RECORD

DEFINE	v_precio_accion		DECIMAL(11,6)
DEFINE	v_prioridad_on		CHAR(25)
DEFINE	v_prioridad_off		CHAR(25)

DEFINE  ruta_reporte		CHAR(40)

MAIN
    CALL obten_saldo()
END MAIN


FUNCTION obten_saldo()

SELECT precio_del_dia 
  INTO v_precio_accion
  FROM glo_valor_accion
 WHERE fecha_valuacion = TODAY

IF v_precio_accion IS NULL THEN
   ERROR "Falta cargar precio de accion para el día Hoy.!!"
   SLEEP 3
   EXIT PROGRAM
END IF

SELECT ruta_listados 
  INTO ruta_reporte
  FROM seg_modulo
 WHERE modulo_cod = "dis"

LET v_prioridad_on = "SET PDQPRIORITY HIGH"
LET v_prioridad_off = "SET PDQPRIORITY OFF"

PREPARE eje_pri_on  FROM v_prioridad_on
PREPARE eje_pri_off FROM v_prioridad_off

EXECUTE eje_pri_on

ERROR "Generando listado de saldos diario, por favor espere ..."

SELECT subcuenta,
       SUM(monto_en_acciones) * v_precio_accion monto_en_pesos,
       SUM(monto_en_acciones) monto_en_acciones,
       SUM(monto_en_pesos) monto_en_pesos_viv
  FROM dis_cuenta
 GROUP BY 1
 ORDER BY 1
  INTO TEMP tmp_saldo

EXECUTE eje_pri_off

LET ruta_reporte = ruta_reporte CLIPPED,"/SALDOS_DIARIOS.",TODAY USING "DDMMYYYY"

START REPORT rep_saldo TO ruta_reporte

DECLARE c_saldo CURSOR FOR
       SELECT s.*,
              t.subct_desc
         FROM tmp_saldo s, tab_subcuenta t
        WHERE t.subct_cod = s.subcuenta
       ORDER BY s.subcuenta

FOREACH c_saldo INTO  saldo.* 
   OUTPUT TO REPORT rep_saldo ( saldo.* )
END FOREACH

CLOSE c_saldo

FINISH REPORT rep_saldo

ERROR "Listado generado: ",ruta_reporte
SLEEP 5

END FUNCTION


REPORT rep_saldo ( saldo )


DEFINE saldo RECORD
       subcuenta 		SMALLINT,
       pesos    		DECIMAL(16,6),
       acciones			DECIMAL(16,6),
       pesos_viv 		DECIMAL(16,6),
       descripcion              CHAR(40)
END RECORD

FORMAT  

FIRST PAGE HEADER

SKIP  2 LINE
PRINT COLUMN  5, "Fecha : " CLIPPED, TODAY
SKIP  2 LINE
PRINT COLUMN 20, " SALDO DIARIO POR SUBCUENTA"
PRINT COLUMN  5, "============================================================"

ON EVERY ROW 
   PRINT COLUMN 5, "Subcuenta = " CLIPPED,saldo.subcuenta, 2 SPACES, saldo.descripcion
   IF saldo.subcuenta = 4 OR
      saldo.subcuenta = 8 THEN
   PRINT COLUMN 5, "Pesos     = ", 2 SPACES,saldo.pesos_viv USING "$###,###,###.######" 
   ELSE
   PRINT COLUMN 5, "Pesos     = ", 2 SPACES,saldo.pesos USING "$###,###,###.######" 
   END IF
   PRINT COLUMN 5, "Acciones  = ", 2 SPACES,saldo.acciones USING "$###,###,###.######" 
   SKIP 1 LINE

END REPORT
