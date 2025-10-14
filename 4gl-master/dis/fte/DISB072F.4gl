-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB072F                                                  --
-- Descripcion  => PROCESO BATCH CALCULO INTERESES RETIRO BANXICO            -- 
-- Sistema      => DIS                                                       --
-- Por          => DMR                                                       --
-- Fecha        => 19 Septiembre 2007                                        --
-- Modificado   => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 30 octubre 2001.                                          --
-- Modificado   => DMR                                                       --
-- Fecha        => 8 Septiembre 2009.                                        --
-- Fecha        => DMR 29/oct/2014 Utilizar tasa BNX no RCV   CPL-1776       --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE
      g_reg   RECORD
                 fecha_aplicacion   DATE,
                 tasa_interes       DECIMAL(10,6)
              END RECORD

   DEFINE
      g_param RECORD LIKE dis_parametro.*

   DEFINE 
      hoy                 DATE,
      usuario             CHAR(08),
      opc                 CHAR(01),
      ejecuta             CHAR(200),
      comando             CHAR(200),
      hora_inicial        CHAR(08),
      hora_final          CHAR(08),
      aux_subct_desc      CHAR(03),
      hist_fecha_aplica   DATE,
      vsubcuenta          CHAR(03),
      vtipo               CHAR(01),
      vfecha_recepcion    DATE,
      vrecauda            CHAR(01),
      enter               CHAR(01)

   DEFINE 
      vrecaudax           SMALLINT

   DEFINE
      tipo_liquida,
      tipo_comision       CHAR(2)

   DEFINE
      importe_liquida,
      importe_comision,
      total_accion,
      totala,
      totalc,
      importe_total       DECIMAL(16,6)

   DEFINE
      i                   SMALLINT

   DEFINE
      g_sal ARRAY[500] OF RECORD
                             folio          INTEGER,
                             fecha_archivo  DATE,
                             aportacion     DECIMAL(16,2),
                             comision       DECIMAL(16,2)
                          END RECORD

   DEFINE 
      arr_c               SMALLINT,
      arr_l               SMALLINT

   DEFINE
      cla_sel             CHAR(300)
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   g_param.*, 
          usuario
   FROM   dis_parametro

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB072F1" ATTRIBUTE(BORDER)

   DISPLAY " DISB072F          CALCULO INTERESES SUBCUENTA RETIRO BANXICO                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "BATCH"
      COMMAND "Calculo"                  "Calculo intereses"
         CALL Intereses()
      COMMAND "Agrupacion"               "Agrupa  intereses"
         CALL Agrupacion()
      COMMAND "Liquidacion"              "Liquida intereses"
         CALL Liquidacion()
      COMMAND "Respaldo 888"             "Respaldo 888"
         CALL Respaldo888()
      COMMAND "Salida"                   "Regresa menu anterior"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN


FUNCTION Intereses()
   LET INT_FLAG = FALSE
   INPUT BY NAME g_reg.*

      AFTER FIELD fecha_aplicacion
         IF g_reg.fecha_aplicacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_aplicacion
         END IF

         IF DAY(g_reg.fecha_aplicacion) <> 1 THEN
            ERROR "Los intereses deben aplicarse al primer dia del mes"
            NEXT FIELD fecha_aplicacion
         END IF

         LET aux_subct_desc = "BNX"     #CPL-1776

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  proceso_cod = "DISB073F"
         AND    parametro2  = g_reg.fecha_aplicacion

         IF STATUS <> NOTFOUND THEN
            ERROR "Calculo de Intereses del ",g_reg.fecha_aplicacion USING
                  "dd/mm/yyyy", " ya realizado "
            NEXT FIELD fecha_aplicacion
         END IF

         DECLARE cur_viv CURSOR FOR
         SELECT "X"
         FROM   dis_cuenta
         WHERE  fecha_conversion = g_reg.fecha_aplicacion
         AND    subcuenta        IN (13,19,30,31)
         AND    tipo_movimiento  = 3
         AND    fecha_valor      = g_reg.fecha_aplicacion
         
         OPEN cur_viv 
         FETCH cur_viv
         IF STATUS <> NOTFOUND THEN
            ERROR "Intereses ya aplicados a esta fecha: ",
                   g_reg.fecha_aplicacion USING "dd-mm-yyyy"
            CLOSE cur_viv
            NEXT FIELD fecha_aplicacion
         END IF
         CLOSE cur_viv

         SELECT tasa_valor
         INTO   g_reg.tasa_interes
         FROM   tab_tasa_ordinaria                       --v2
         WHERE  tasa_fecha = g_reg.fecha_aplicacion
         AND    tasa_origen = "BNX"
  
         IF STATUS = NOTFOUND THEN
            ERROR "NO existe tasa BNX para la fecha ",g_reg.fecha_aplicacion USING "DD/MM/YYYY"
            NEXT FIELD fecha_aplicacion
         END IF
     
	 DISPLAY BY NAME g_reg.tasa_interes
      
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_intereses()
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_intereses()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB073F",              -- proceso_cod
       1,                       -- etapa_cod    -- CALCULO RENDIMIENTOS
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "19",                    -- parametro1
       g_reg.fecha_aplicacion,  -- parametro2
       g_reg.tasa_interes,      -- parametro3 
       NULL,                    -- parametro4
       NULL,    -------g_reg.monto_interes,     -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario 
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Intereses ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo de Interes por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB073F.4gi 0 0 0 ",
                                                g_reg.tasa_interes," ",
                                                g_reg.fecha_aplicacion,"  & "
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Agrupacion()
   DEFINE conta_int     INTEGER
   DEFINE conta_agru    INTEGER

   SELECT count(*)
   INTO  conta_int
   FROM  safre_tmp:cta_interes_retbnx
   
   IF conta_int = 0 THEN
      ERROR "No existen datos para ser Agrupados"
      SLEEP 3
      ERROR ""
      RETURN
   ELSE
      SELECT count(*)
      INTO   conta_agru
      FROM   safre_tmp:cta_interes_retbnx
      WHERE  estado = 6
   
      IF conta_agru > 0 THEN
         ERROR " Los Datos ya han sido Agrupados"
         SLEEP 3
         ERROR ""
         RETURN
      END IF
   END IF
   
   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_Agrupacion()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_Agrupacion()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB074F",              -- proceso_cod
       2,                       -- etapa_cod  -- Agrupacion intereses
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "S",                     -- parametro1
       "",                      -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Agrupacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Agrupacion intereses por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB074F.4gi 0 0 0 & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""

   UPDATE safre_tmp:cta_interes_retbnx
   SET estado = 6
END FUNCTION


FUNCTION Liquidacion()
   DECLARE cur_integra CURSOR FOR
   SELECT *
   FROM   safre_tmp:dis_cuenta_retbnx
   
   OPEN cur_integra
   FETCH cur_integra

   IF STATUS = NOTFOUND THEN
      ERROR "No existen datos en dis_cuenta_retbnx para integrar en dis_cuenta"
      SLEEP 3
      CLOSE cur_integra
      RETURN
   ELSE
      DECLARE cur_integra2 CURSOR FOR
      SELECT *
      FROM   safre_tmp:dis_cuenta_retbnx
      WHERE estado = 8

      OPEN cur_integra2
      FETCH cur_integra2

      IF STATUS <> NOTFOUND THEN
         ERROR "Los Datos ya fueron Liquidados en dis_cuenta"
         SLEEP 3
         CLOSE cur_integra2
         RETURN
      END IF
      CLOSE cur_integra2
   END IF
   CLOSE cur_integra

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_Liquidacion()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_Liquidacion()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB075F",              -- proceso_cod
       3,                       -- etapa_cod  -- Integra intereses en dis_cuenta
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "S",                     -- parametro1
       "",                      -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Integracion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Integracion Intereses por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB075F.4gi 0 0 0 & "   
   RUN ejecuta
   ERROR "El Proceso se ejecuto Satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""

   UPDATE safre_tmp:dis_cuenta_retbnx
   SET estado = 8
END FUNCTION


FUNCTION Respaldo888()     
   DEFINE xfol      INTEGER
   DEFINE conta_8   INTEGER

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*

      AFTER FIELD fecha_aplicacion
         IF g_reg.fecha_aplicacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_aplicacion
         END IF
         
         IF DAY(g_reg.fecha_aplicacion) <> 1 THEN
            ERROR "La fecha deben aplicarse al primer dia del mes"
            NEXT FIELD fecha_aplicacion
         END IF

         DECLARE cur_fol CURSOR FOR
         SELECT folio
         FROM safre_tmp:dis_cuenta_retbnx
         WHERE fecha_conversion = g_reg.fecha_aplicacion

         FOREACH cur_fol INTO xfol
            EXIT FOREACH
         END FOREACH

         DECLARE cur_viv2 CURSOR FOR
         SELECT "X"
         FROM   dis_cuenta
         WHERE  folio     = xfol
         AND    subcuenta IN (13,19,30,31)
         AND    tipo_movimiento  = 3  

         OPEN cur_viv2
         FETCH cur_viv2
         IF STATUS = NOTFOUND THEN
            ERROR "No existen movimientos para respaldar 888 ",
                   g_reg.fecha_aplicacion USING "dd/mm/yyyy"
         -- CLOSE cur_viv2
         -- NEXT FIELD fecha_aplicacion
         ELSE
            SELECT count(*)
            INTO   conta_8
            FROM   dis_cuenta
            WHERE  folio     = xfol
            AND    subcuenta IN (13,19,30,31)
            AND    tipo_movimiento  = 3
            AND    estado = 8

            IF conta_8 > 0 THEN
               ERROR "Respaldo 888 ya realizado con fecha de Aplicacion ",
                     g_reg.fecha_aplicacion USING "dd/mm/yyyy"
               NEXT FIELD fecha_aplicacion
            END IF
         END IF

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_respaldo888()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_respaldo888()
   DEFINE mfolio    INTEGER

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB076F",              -- proceso_cod
       4,                       -- etapa_cod  -- Respaldo 888
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_aplicacion,  -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Respaldo 888 ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Respaldo 888 por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB076F.4gi 0 0 0 ",g_reg.fecha_aplicacion,"  & " 
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""

   SELECT UNIQUE folio
   INTO mfolio
   FROM safre_tmp:dis_cuenta_retbnx

   UPDATE dis_cuenta
   SET estado = 8
   WHERE folio = mfolio
END FUNCTION

