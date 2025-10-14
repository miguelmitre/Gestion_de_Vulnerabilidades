-------------------------------------------------------------------------------
-- Proyecto     => safre_af                                                  --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB2007B                                                 --
-- Descripcion  => LIQUIDACION POR nohup DE RCV y VIV, EST                   --
-- Fecha        => 21 junio 2000.                                            --
-- By           => GERARDO ALFONSO VEGA PAREDES                              --
-- Sistema      => DIS                                                       --
-------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   DEFINE
      hoy,
      fecha1,
      fecha2,
      z_fecha DATE,
      aux_pausa,
      uno,
      dos,
      tres,
      cuatro,
      enter         CHAR(1),
      salir,
      cont,
      sigue         SMALLINT,
      tipo_desc	    CHAR(30),
      tipo_liquida,
      tipo_comision CHAR(2),
      opc           CHAR(01),
      vfolio        CHAR(50),
      g_bat RECORD  LIKE dis_ctrl_proceso.*,
      vhora_max     CHAR(08),
      vhora_final   CHAR(08),
      vresultado    CHAR(23),
      hora_inicial  CHAR(08),
      hora_final    CHAR(08),
      ejecuta       CHAR(200),
      usuario       CHAR(08),
      xfolio        INTEGER

   DEFINE reg_ident RECORD
          pid                          ,
          proceso_cod                  ,
          opera_cod           INTEGER  ,
          nom_archivo         CHAR(025),
          folio               INTEGER  ,
          fecha_proceso       DATE     ,
          fecha_acreditacion  DATE     ,
          tipo_liq            CHAR(004) 
   END RECORD
   DEFINE tipo               SMALLINT
   DEFINE v_tipo        CHAR(010),
          v_usuario     CHAR(010)

   DEFINE opcion                    ,
          i               SMALLINT
   DEFINE valor_accion DECIMAL(16,6)
END GLOBALS

################################################################################
MAIN
DEFINE vv char(7)
   OPTIONS 
      INPUT WRAP,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT 


		DISPLAY " "
		DISPLAY ".1"
   CALL startlog("DISB2007B.log")
   LET  reg_ident.pid                = ARG_VAL(1)
   LET  reg_ident.proceso_cod        = ARG_VAL(2)
   LET  reg_ident.opera_cod          = ARG_VAL(3)
   LET  reg_ident.nom_archivo        = ARG_VAL(4)
   LET  reg_ident.folio              = ARG_VAL(5)
   LET  reg_ident.fecha_proceso      = ARG_VAL(6)
   LET  reg_ident.fecha_acreditacion = ARG_VAL(7)
   LET  reg_ident.tipo_liq           = ARG_VAL(8)


DISPLAY "INCIO PROCESO  :"
DISPLAY "ID. PROCESO    :",reg_ident.pid
DISPLAY "PROCESO COD    :",reg_ident.proceso_cod
DISPLAY "OPERA_COD      :",reg_ident.opera_cod 
DISPLAY "NOMBRE ARCHIVO :",reg_ident.nom_archivo
DISPLAY "FOLIO          :",reg_ident.folio
DISPLAY "FECHA_PROCESO  :",reg_ident.fecha_proceso
DISPLAY "ACREDITACION   :",reg_ident.fecha_acreditacion

SELECT * FROM yy  

   LET opcion = LENGTH(reg_ident.tipo_liq)
 
   FOR i = 1 TO opcion

   LET tipo = reg_ident.tipo_liq[i] 
 
   LET hoy = TODAY
   LET vhora_max = TIME
   
   LET fecha1 = reg_ident.fecha_proceso
   LET fecha2 = reg_ident.fecha_acreditacion

   SELECT precio_del_dia
   INTO   valor_accion
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha2
   IF STATUS = NOTFOUND THEN
      ERROR "NO EXISTE VALOR ACCION PARA ESTA FECNA"
		EXIT PROGRAM
   END IF

   CALL Proceso_principal(reg_ident.folio) RETURNING xfolio
   CALL Actualiza_estados(reg_ident.folio)

   LET vhora_final = TIME

   SELECT user
   INTO   v_usuario
   FROM   dis_ctrl_proceso
   GROUP BY 1

   CASE tipo
   WHEN 1 
        LET v_tipo = "RCV y VIV"
        EXIT CASE
   WHEN 2
        LET v_tipo = "EST"
        EXIT CASE
   WHEN 3
        LET v_tipo = "INT RCV"
        EXIT CASE
   WHEN 4
        LET v_tipo = "INT EST"
        EXIT CASE
   OTHERWISE
        EXIT CASE
   END CASE
DISPLAY "TIPO LIQUIDACION :",v_tipo
sleep 30
SELECT * FROM yy  
{
   INSERT INTO dis_ctrl_proceso
   VALUES
( reg_ident.fecha_proceso,
  "DIS"                  ,
  8                      ,
  vhora_max              ,
  vhora_final            ,
  reg_ident.folio        ,
  v_tipo                 ,
  reg_ident.fecha_proceso,
  reg_ident.fecha_acreditacion,
  NULL                   ,
  NULL                   , 
  NULL                   ,
  v_usuario )
}
 END FOR
  CALL actualiza_operacion()
  CALL actualiza_proceso()

END MAIN

FUNCTION Proceso_principal(vfolio)
   DEFINE
      x_reg RECORD LIKE dis_provision.*

   DEFINE
      text   CHAR(500),
      vfolio CHAR(50)
      -----vfolio INTEGER

   LET text = NULL

   CASE tipo
      WHEN 1              # APORTE RCV y VIV
         LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio CLIPPED,
		              " AND subcuenta IN (1,2,3,4) ",
                    " AND tipo_movimiento IN (1,2,100) ",
                    " AND estado = 5 "
 
      WHEN 2                   # APORTE EST
         LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio CLIPPED,
                    " AND subcuenta IN (5,6,9) ",
                    " AND tipo_movimiento IN (1,2,100) ",
                    " AND estado = 5 "

      WHEN 3                # INTRESES RCV    
         LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio CLIPPED,
		              " AND subcuenta IN (1,2,3) ",
                    " AND tipo_movimiento IN (3,4) ",
                    " AND estado = 5 "

      WHEN 4                # INTRESES EST    
         LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio CLIPPED,
		              " AND subcuenta IN (5,6) ",
                    " AND tipo_movimiento IN (3,4) ",
                    " AND estado = 5 "
   END CASE

   PREPARE cur1 FROM text
   DECLARE cursor_1 CURSOR FOR cur1

      FOREACH cursor_1 INTO x_reg.*
	      CALL salida(x_reg.*)
      END FOREACH

   RETURN x_reg.folio
END FUNCTION

#############################################################################
FUNCTION Actualiza_estados(vfolio)
   ----------DEFINE vfolio INTEGER
   DEFINE vfolio CHAR(50),
          cla_up CHAR(200)

   IF tipo = 1 THEN
      LET cla_up = "UPDATE dis_dep_aporte ",
                  " SET estado = 3, ",              # LIQUIDADO
                  " fech_liquidacion = ","'",fecha2,"'",   # FECHA VALOR ACCION 
                  " WHERE folio = ",vfolio CLIPPED,
                  " AND ident_pago[14,15] in ('41','11') ",
                  " AND estado = 2 "

      PREPARE up01 FROM cla_up
      EXECUTE up01

      LET cla_up = "UPDATE dis_dep_aporte ",
                     " SET estado = 3, ",              # LIQUIDADO
              " fech_liquidacion = ","'",fecha2,"'",   # FECHA VALOR ACCION 
                   " WHERE folio = ",vfolio CLIPPED,
                     " AND ident_pago[14,15] = '43' ",
                     " AND estado = 2 "
      PREPARE up02 FROM cla_up
      EXECUTE up02
   END IF
   
   IF tipo = 2 THEN
      LET cla_up = "UPDATE dis_dep_aporte ",
                     " SET estado = 3, ",              # LIQUIDADO
              " fech_liquidacion = ","'",fecha2,"'",   # FECHA VALOR ACCION 
                   " WHERE folio = ",vfolio CLIPPED,
                     " AND ident_pago[14,15] in ('42','12') ",
                     " AND estado = 2 "

      PREPARE up03 FROM cla_up
      EXECUTE up03
   END IF

   IF tipo = 3 THEN
      LET cla_up = "UPDATE dis_dep_aporte ",
                     " SET estado = 3, ",              # LIQUIDADO
              " fech_liquidacion = ","'",fecha2,"'",   # FECHA VALOR ACCION 
                   " WHERE folio = ",vfolio CLIPPED,
                     " AND ident_pago[14,15] in ('21') ",
                     " AND estado = 2 "

      PREPARE up04 FROM cla_up
      EXECUTE up04

   END IF

   IF tipo = 4 THEN
      LET cla_up = "UPDATE dis_dep_aporte ",
                     " SET estado = 3, ",              # LIQUIDADO
              " fech_liquidacion = ","'",fecha2,"'",   # FECHA VALOR ACCION 
                   " WHERE folio = ",vfolio CLIPPED,
                     " AND ident_pago[14,15] in ('22') ",
                     " AND estado = 2 "

      PREPARE up05 FROM cla_up
      EXECUTE up05

   END IF

-----   CALL Control_folio(vfolio,TODAY)
END FUNCTION

#############################################################################
FUNCTION salida(x_reg)
   DEFINE x_reg	          RECORD LIKE dis_provision.*
   DEFINE x_reg_1         RECORD LIKE dis_provision.*
   DEFINE fecha_anterior  DATE
   DEFINE fecha_actual    DATE
   DEFINE val_peso        DECIMAL(16,6)
   DEFINE val_accion      DECIMAL(16,6)
   DEFINE x_accion	  RECORD LIKE glo_valor_accion.*
   DEFINE x_fecha	  DATE
   DEFINE valor_en_accion DECIMAL(16,6)

      LET x_fecha = fecha2

      IF x_reg.subcuenta <> 4 AND
         x_reg.subcuenta <> 8 THEN
         LET x_reg.precio_accion = valor_accion
         LET x_reg.monto_en_acciones=(x_reg.monto_en_pesos/x_reg.precio_accion)
      ELSE
         LET x_reg.precio_accion = 0
         LET x_reg.monto_en_acciones = 0
      END IF 

      LET x_reg.fecha_conversion  = fecha2

      IF x_reg.fecha_valor IS NULL OR x_reg.fecha_valor="01/01/0001" THEN
         LET x_reg.fecha_valor = fecha2
      END IF 

      INSERT INTO dis_cuenta VALUES ( x_reg.* )

END FUNCTION

FUNCTION Control_folio(vfolio,vfecha_recepcion)
   DEFINE
      vfolio INTEGER,
      vfecha_recepcion DATE,
      vfecha_char    CHAR(08),
      vfecha_archivo DATE,
      vdepositos     SMALLINT,
      varchivo_cod   SMALLINT,
      vestado_cod    SMALLINT,
      vusuario       CHAR(08)

   ----------------  C O N T R O L    F O L I O ------------------------
   SELECT fech_creac_lote INTO vfecha_char
     FROM dis_cza_aporte
    WHERE folio = vfolio

   LET vfecha_archivo = mdy(vfecha_char[5,6],
                            vfecha_char[7,8],
                            vfecha_char[1,4])

   SELECT COUNT(*),user 
     INTO vdepositos,vusuario
     FROM pagos
       IF vdepositos <= 5 THEN
          LET varchivo_cod = 11
       ELSE
          LET varchivo_cod = 12
       END IF 
       LET vestado_cod = 40 --- Estado Liquidado --- 

   PREPARE proc_exe 
      FROM "EXECUTE PROCEDURE safreps_tab:proc_ctrl(?,?,?,?,?,?,?) "
   EXECUTE proc_exe USING vfolio,
                          vfecha_recepcion, 
                          varchivo_cod,
                          vfecha_archivo,
                          vestado_cod,
                          vfecha_recepcion,
                          vusuario
    ---------------------------------------------------------------------
END FUNCTION

FUNCTION actualiza_operacion() 

      UPDATE bat_ctr_operacion
      SET    bat_ctr_operacion.estado_operacion   = 4            ,
             bat_ctr_operacion.fecha_fin          = CURRENT      ,
             bat_ctr_operacion.folio              = reg_ident.folio
      WHERE  bat_ctr_operacion.pid                = reg_ident.pid
      AND    bat_ctr_operacion.proceso_cod        = reg_ident.proceso_cod
      AND    bat_ctr_operacion.opera_cod          = reg_ident.opera_cod 

END FUNCTION

FUNCTION actualiza_proceso()

        UPDATE bat_ctr_proceso
        SET    bat_ctr_proceso.estado_proceso     = 4            ,
               bat_ctr_proceso.fecha_fin          = CURRENT     
        WHERE  bat_ctr_proceso.pid                = reg_ident.pid
        AND    bat_ctr_proceso.proceso_cod        = reg_ident.proceso_cod
END FUNCTION
