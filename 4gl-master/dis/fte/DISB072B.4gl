-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB072B                                                  --
-- Descripcion  => PROCESO BATCH CALCULO INTERESES FONDOS VIVIENDA           -- 
-- Sistema      => DIS                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 7 julio 2001.                                             --
-- Modificado   => GEARADO ALFONSO VEGA PAREDES                              --
-- Fecha        => 30 octubre 2001.                                          --
-- Modificado   => GEARADO ALFONSO VEGA PAREDES                              --
-- Fecha        => 2 agosto 2004.                                            --
-------------------------------------------------------------------------------

DATABASE  safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_aplicacion   DATE,
      tasa_interes       DECIMAL(10,6)
   END RECORD

   DEFINE g_param RECORD LIKE dis_parametro.*

   DEFINE 
      hoy               DATE,
      usuario           CHAR(08),
      opc               CHAR(01),
      ejecuta           CHAR(200),
      comando           CHAR(200),
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      aux_subct_desc    CHAR(03),
      hist_fecha_aplica DATE,
      vsubcuenta        CHAR(03),
      vtipo             CHAR(01),
      vfecha_recepcion  DATE,
      vrecauda          CHAR(01)
   DEFINE vrecaudax SMALLINT

   DEFINE tipo_liquida,
          tipo_comision CHAR(2)

   DEFINE importe_liquida,
          importe_comision,
          total_accion,
          totala,
          totalc,
          importe_total DECIMAL(16,6)
   DEFINE i SMALLINT

   DEFINE g_sal ARRAY[500] OF RECORD
      folio          INTEGER,
      fecha_archivo  DATE,
      aportacion     DECIMAL(16,2),
      comision       DECIMAL(16,2)
   END RECORD

   DEFINE arr_c SMALLINT,
          arr_l SMALLINT

   DEFINE cla_sel CHAR(300)

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

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB0721" ATTRIBUTE(BORDER)

   DISPLAY " DISB072B          CALCULO INTERESES FONDO DE VIVIENDA                        " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "BATCH"
      COMMAND "Calculo"                  "Calculo intereses"
         CALL Intereses()
      COMMAND "Agrupacion"               "Agrupa intereses"
         CALL Agrupacion()
      COMMAND "Integracion"              "Integra intereses"
         CALL Integracion()
      COMMAND "Respaldo 888"             "Respaldo 888"
         CALL Respaldo888()
      COMMAND "Calculo Rema"             "Calculo viv rema"   --v2
         CALL Intereses_rem()                                 --v2
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

         LET aux_subct_desc = "FOV"

         DECLARE cur_viv CURSOR FOR
         SELECT "X"
         FROM   dis_cuenta
         WHERE  subcuenta = 14
         AND    tipo_movimiento  = 3
         AND    fecha_valor      = g_reg.fecha_aplicacion
         AND    fecha_conversion = g_reg.fecha_aplicacion
         
         OPEN cur_viv 
         FETCH cur_viv
         IF STATUS <> NOTFOUND THEN
            ERROR "Intereses ya aplicados a esta fecha: ",
                   g_reg.fecha_aplicacion USING "dd-mm-yyyy"
            CLOSE cur_viv
            NEXT FIELD fecha_aplicacion
         ELSE
            SELECT MAX(tasa_fecha)
            INTO   hist_fecha_aplica
            FROM   tab_tasa_ordinaria                    --v2
            WHERE  tasa_origen = aux_subct_desc

            --v2
            IF g_reg.fecha_aplicacion-1 UNITS MONTH >=  hist_fecha_aplica THEN
               ERROR "Se requiere aplicar intereses de un periodo anterior"
               NEXT FIELD fecha_aplicacion
            END IF
         END IF
         CLOSE cur_viv

         SELECT tasa_valor
         INTO   g_reg.tasa_interes
         FROM   tab_tasa_ordinaria                       --v2
         WHERE  tasa_fecha = g_reg.fecha_aplicacion
	      AND    tasa_origen = "FOV"

         LET g_reg.tasa_interes = g_reg.fecha_aplicacion

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
       "DISB073B",              -- proceso_cod
       1,                       -- etapa_cod  -- CALCULO RENDIMIENTOS
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "14",                    -- parametro1
       g_reg.fecha_aplicacion,  -- parametro2
       g_reg.tasa_interes,      -- parametro3 
       NULL,                    -- parametro4
       NULL,          -------g_reg.monto_interes,     -- parametro5
       NULL,                    -- folio
       "PROCESANDO",             -- resultado
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Intereses ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo de Interes por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB073B.4gi 0 0 0 ",
                                                g_reg.tasa_interes," ",
                                                g_reg.fecha_aplicacion,"  & "
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Agrupacion()

   DECLARE cur_agrupa CURSOR FOR
   SELECT "M"
   FROM   safre_tmp:cta_interes_fov
   
   OPEN cur_agrupa
   FETCH cur_agrupa
   IF STATUS = NOTFOUND THEN
      ERROR "No exiten datos para ser integrados"
      SLEEP 3
      ERROR ""
      CLOSE cur_agrupa
      RETURN
   END IF
   CLOSE cur_agrupa
   
   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_Agrupacion()
   END IF
END FUNCTION

FUNCTION Ejecuta_Agrupacion()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB074B",              -- proceso_cod
       2,                       -- etapa_cod  -- Agrupacion intereses
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "S",                     -- parametro1
       "",                      -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",             -- resultado
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Agrupacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Agrupacion intereses por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB074B.4gi 0 0 0 & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Integracion()
   PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_Integracion()
   END IF

END FUNCTION

FUNCTION Ejecuta_Integracion()

   DECLARE cur_integra CURSOR FOR
   SELECT *
   FROM   safre_tmp:dis_cuenta_fov

   OPEN cur_integra
   FETCH cur_integra

   IF STATUS = NOTFOUND THEN
      ERROR "No existen datos en dis_cuenta_fov para integrar en dis_cuenta"
      SLEEP 3
      CLOSE cur_integra
      RETURN
   END IF

   CLOSE cur_integra

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB075B",              -- proceso_cod
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
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Integracion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Integracion intereses por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB075B.4gi 0 0 0 & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Respaldo888()     
   DEFINE 
      aux_subct_desc CHAR(03),
      vfecha_aux     DATE

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

         SELECT tasa_fecha
           FROM tab_tasa_ordinaria                      --v2
          WHERE tasa_fecha = g_reg.fecha_aplicacion
          AND   tasa_origen = "FOV"

         IF STATUS = NOTFOUND THEN
            ERROR "No exite esta fecha para respaldar 888 ",
                   g_reg.fecha_aplicacion USING "dd/mm/yyyy"
            NEXT FIELD fecha_aplicacion
         END IF

---         SELECT MAX(tasa_fecha)
---         INTO   vfecha_aux
---         FROM   tab_tasa_remanente
---         WHERE  tasa_origen = "FOV"

---          IF g_reg.fecha_aplicacion < vfecha_aux THEN
---             ERROR "Debes aplicar una fecha posterior a ",
---                    g_reg.fecha_aplicacion
---             NEXT FIELD fecha_aplicacion 
---          END IF

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

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB076B",              -- proceso_cod
       4,                       -- etapa_cod  -- Respaldo 888
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_aplicacion,  -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",             -- resultado
       usuario,
       0)                 -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Respaldo 888 ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Respaldo 888 por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB076B.4gi 0 0 0 ",g_reg.fecha_aplicacion,"  & " 
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Intereses_rem()   --v2

   CLEAR FORM

   LET ejecuta = "fglgo DISB072C.4gi" 
   RUN ejecuta

END FUNCTION               --v2
