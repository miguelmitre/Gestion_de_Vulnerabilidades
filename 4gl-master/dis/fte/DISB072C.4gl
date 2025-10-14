-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB072C                                                  --
-- Descripcion  => PROCESO BATCH CALCULO INTERESES FONDOS VIVIENDA REMANEN   -- 
-- Sistema      => DIS                                                       --
-- Por          => ALEJANDRO RAMIREZ                                         --
-- Fecha        => 3 Abril 2006.                                             --
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

   DISPLAY " DISB072C          CALCULO INTERESES FONDO DE VIVIENDA REMANENTE              " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "BATCH"
      COMMAND "Calculo"                  "Calculo intereses"
         CALL Intereses()
      COMMAND "Agrupacion"               "Agrupa intereses"
         CALL Agrupacion()
      COMMAND "Integracion"              "Integra intereses"
         CALL Integracion()
      COMMAND "Salida"                   "Regresa menu anterior"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN

FUNCTION Intereses()
   DEFINE vmes_valido CHAR(2)
   DEFINE tasa_ord    DECIMAL(10,6)

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
         SELECT 'x'
         FROM dis_ctrl_proceso
         WHERE proceso_cod='DISB073C'
         AND   parametro2=g_reg.fecha_aplicacion
         AND   resultado='INTERESES CALCULADOS'

         OPEN cur_viv
         FETCH cur_viv
         IF STATUS <> NOTFOUND THEN
            ERROR "Intereses de viv remanente ya aplicados a esta fecha: ",
                   g_reg.fecha_aplicacion USING "dd-mm-yyyy"
            CLOSE cur_viv
            NEXT FIELD fecha_aplicacion
         ELSE
         
            DECLARE cur_viv2 CURSOR FOR
            SELECT 'x'
            FROM dis_ctrl_proceso
            WHERE proceso_cod='DISB073B'
            AND   parametro2=g_reg.fecha_aplicacion
            AND   resultado='INTERESES CALCULADOS'

            OPEN cur_viv2 
            FETCH cur_viv2
            IF STATUS = NOTFOUND THEN
               ERROR "Intereses de FOVISSSTE aun no se han aplicado: ",
                      g_reg.fecha_aplicacion USING "dd-mm-yyyy"
               CLOSE cur_viv2
               NEXT FIELD fecha_aplicacion
            ELSE

               SELECT MAX(tasa_fecha)
               INTO   hist_fecha_aplica
               FROM   tab_tasa_remanente
               WHERE  tasa_origen = aux_subct_desc

               --v2
               IF g_reg.fecha_aplicacion-1 UNITS MONTH >= hist_fecha_aplica THEN
                  ERROR "Se requiere aplicar intereses de un periodo anterior"
                  NEXT FIELD fecha_aplicacion
               ELSE
                  LET vmes_valido= g_reg.fecha_aplicacion USING "mm"
                  IF g_reg.fecha_aplicacion <> '04' THEN
                    ERROR "La fecha a aplicarse para remanentes de fovissste debe ser de abril "
                    NEXT FIELD fecha_aplicacion
                  END IF
               END IF
            END IF
            CLOSE cur_viv2

         END IF
         CLOSE cur_viv

         SELECT tasa_valor
         INTO   g_reg.tasa_interes
         FROM   tab_tasa_remanente
         WHERE  tasa_fecha = g_reg.fecha_aplicacion
         AND    tasa_origen = "FOV"

         LET tasa_ord = 0
         SELECT tasa_valor
         INTO   tasa_ord
         FROM   tab_tasa_ordinaria
         WHERE  tasa_fecha = g_reg.fecha_aplicacion
         AND    tasa_origen = "FOV"

         LET g_reg.tasa_interes = g_reg.tasa_interes - tasa_ord
         

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
       "DISB073C",              -- proceso_cod
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
   LET ejecuta = "nohup time fglgo DISB073C.4gi 0 0 0 ",
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
   FROM   safre_tmp:cta_interes_fov2
   
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
       "DISB074C",              -- proceso_cod
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
   LET ejecuta = "nohup time fglgo DISB074C.4gi 0 0 0 & "   
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
   FROM   safre_tmp:dis_cuenta_fov2

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
       "DISB075C",              -- proceso_cod
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
   LET ejecuta = "nohup time fglgo DISB075C.4gi 0 0 0 ",
                                                g_reg.fecha_aplicacion,"  & "
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""


END FUNCTION
