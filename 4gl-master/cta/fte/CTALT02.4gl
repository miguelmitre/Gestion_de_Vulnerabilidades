--------------------------------------------------------------------------
-- Programa    : CTALT02
-- Descripcion : Ejecuta spl sp_saldo_edad que se encuentra en safre_tmp 
-- Fecha       : 1 sep 2010
-- Autor       : Jesus Yanez Moreno
--------------------------------------------------------------------------
DATABASE safre_tmp
GLOBALS 

  DEFINE g_qry_1 CHAR(100)
  DEFINE g_qry_2 CHAR(100)
  DEFINE g_qry_3 CHAR(100)
  DEFINE fecha_valuacion DATE
  DEFINE lanzador  CHAR(100)
END GLOBALS

MAIN 

LET fecha_valuacion = ARG_VAL(1)

DISPLAY "CTALT02.",fecha_valuacion using"ddmmyyyy",
        ": INICIANDO PROCESO DE PREVIO "

DISPLAY "CTALT02.",fecha_valuacion using"ddmmyyyy",
        ": CREANDO SALDOS PREVIOS "

WHENEVER ERROR CONTINUE
 LET g_qry_2 = "DROP TABLE tmp_saldo_edad"
 PREPARE qry_2 FROM g_qry_2
 EXECUTE qry_2
WHENEVER ERROR STOP

create table tmp_saldo_edad
  (
    nss char(11),
    subcuenta smallint,
    siefore smallint,
    fecha_conversion date,
    monto_en_acciones decimal(22,6),
    monto_en_pesos decimal(22,6)
  )

WHENEVER ERROR CONTINUE
 LET g_qry_3 = "DROP TABLE tmp_saldo_grupo"
 PREPARE qry_3 FROM g_qry_3
 EXECUTE qry_3
WHENEVER ERROR STOP

create table tmp_saldo_grupo
  (
    nss char(11),
    grupo_regimen smallint,
    siefore smallint,
    fecha_conversion date,
    monto_en_acciones decimal(22,6),
    monto_en_pesos decimal(22,6)
  )

LET g_qry_1 = "EXECUTE PROCEDURE sp_saldo_edad(?)" 

PREPARE qry_1 FROM g_qry_1

EXECUTE qry_1 USING fecha_valuacion

DISPLAY "CTALT02.",fecha_valuacion using"ddmmyyyy",
        ": TERMINA CREACION SALDOS PREVIOS "

UPDATE safre_af:cta_ctrl_tes_ied
SET    estado1 = "OK"
WHERE  fecha = fecha_valuacion 
AND    tipo_proceso = 1

---
--- lanza reporte de saldos previos globales
--- el parametro 1 es para indicar que es de toda la tabla de saldos
---

LET lanzador = "fglgo CTALT04 ",fecha_valuacion
RUN lanzador

END MAIN




