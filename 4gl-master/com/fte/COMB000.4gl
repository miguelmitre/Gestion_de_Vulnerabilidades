-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB000                                                   --
-- Descripcion  => PROCESO BATCH PROVISION DE COMISION                       --
-- Sistema      => COM                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 12 enero 2004.                                            --
-- Modificado   => ISABEL FONSECA FRIAS                                      --
-- Fecha        => 02-Juni-2005                                              --
-- Descripcion  => (v1) se agrego la generacion de archivos                  --
--              => con sumatoria para gerentes,subdirectores y ejecutivos    --
-------------------------------------------------------------------------------

DATABASE  safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_corte DATE,
      periodo_limite CHAR(06)
   END RECORD

   DEFINE g_param RECORD LIKE com_parametro.*

   DEFINE hoy          DATE,
          usuario      CHAR(08),
          opc          CHAR(01),
          ejecuta      CHAR(200),
          comando      CHAR(200),
          hora_inicial CHAR(08),
          hora_final   CHAR(08)

   DEFINE cla_sel CHAR(200)

   DEFINE vfecha date

   DEFINE vfecha_recep_arch DATE,
          vmes_recep        SMALLINT,
          vano_recep        SMALLINT

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
   FROM   com_parametro

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "COMB0001" ATTRIBUTE(BORDER)

   DISPLAY " COMB000             CALCULO COMISIONES A PROMOTORES                            " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "Calculo Comisiones"
      COMMAND "(Provision Anticipo)"  "Provisiona Anticipo Produccion"
         CALL  Anticipo()
      COMMAND "(Actualiza No. Interno)" "Actualiza No. Interno en provision"
         CALL Actualiza_interno()
      COMMAND "(Calculo Anticipo)"  "Calcula Comision Anticipo"
         CALL  Calcula_anticipo()
      COMMAND "(Genera archivos)"  "Genera archivos con sumatoria" -- (v1)
         CALL  Genera_archivos()                                   -- (v1) 
      COMMAND "(Reliquidacion)"      "Calcula Comision Reliquidacion"
         CALL  Reliquidacion()
      COMMAND "(Cierre periodo)"   "Calcula cierre del periodo"
         CALL  Calcula_cierre()
      COMMAND "Salida"        "Salida"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN

FUNCTION Anticipo()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF
         
         LET cla_sel="SELECT FIRST 1 fecha_corte ",
                     "FROM   com_comis_detalle ",
                     "WHERE  fecha_corte = ","'",g_reg.fecha_corte,"'" CLIPPED

         PREPARE claexe FROM cla_sel
         DECLARE cur_fecha CURSOR FOR claexe
         OPEN cur_fecha
         FETCH cur_fecha INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha

      AFTER FIELD periodo_limite
         IF g_reg.periodo_limite IS NULL THEN
            ERROR "El periodo no pude ser nulo"
            NEXT FIELD fecha_corte
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
      CALL Ejecuta_Anticipo()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_Anticipo()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB001",               -- proceso_cod
       1,                       -- etapa_cod  -- PROVISION COMISION
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_corte,       -- parametro1  -- fecha_corte
       NULL,                    -- parametro2
       NULL,                    -- parametro3 
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Calculo Anticipo ",
      STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo Anticipo por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB001.4gi ",g_reg.fecha_corte," ",
                                                 g_reg.periodo_limite,"  & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Actualiza_interno()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
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

      ERROR "PROCESANDO INFORMACION..."
      LET ejecuta = "fglgo COMB046.4gi ",g_reg.fecha_corte," &"
      RUN ejecuta
      ERROR "PROCESO TERMINADO..."
      SLEEP 1 
      ERROR " "

   END IF

   CLEAR FORM
   CLEAR SCREEN

   PROMPT  "PROCESO TERMINADO... INTRO " FOR opc 
END FUNCTION

FUNCTION Genera_archivos()


      ERROR "PROCESANDO INFORMACION..."
      LET ejecuta = "fglgo COMC025.4gi "
      RUN ejecuta
      ERROR "PROCESO TERMINADO..."
      SLEEP 1
      ERROR " "


   CLEAR FORM
   CLEAR SCREEN

END FUNCTION

FUNCTION Calcula_anticipo()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF
         
         LET cla_sel="SELECT FIRST 1 fecha_corte ",
                     "FROM   com_comis_detalle ",
                     "WHERE  fecha_corte = ","'",g_reg.fecha_corte,"'" CLIPPED,
                     "AND    estado_comision in (10,20,30) "

         PREPARE claexe2 FROM cla_sel
         DECLARE cur_fecha2 CURSOR FOR claexe2
         OPEN cur_fecha2
         FETCH cur_fecha2 INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha2
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha2

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
      CALL Ejecuta_Calculo_Anticipo()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_Calculo_Anticipo()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB002",               -- proceso_cod
       1,                       -- etapa_cod  -- PROVISION COMISION
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_corte,       -- parametro1  -- fecha_corte
       NULL,                    -- parametro2
       NULL,                    -- parametro3 
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Calculo Anticipo ",
      STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo Anticipo por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB002.4gi ",g_reg.fecha_corte," &"
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Reliquidacion()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF

         LET cla_sel ="SELECT FIRST 1 fecha_corte ",
                      "FROM   com_comis_detalle ",
                      "WHERE  fecha_corte = ","'",g_reg.fecha_corte,"'" CLIPPED,
                     " AND    estado_comision>=40 "

         PREPARE claexe1 FROM cla_sel
         DECLARE cur_fecha1 CURSOR FOR claexe1
         OPEN cur_fecha1
         FETCH cur_fecha1 INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha1
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha1

         SELECT MONTH(fecha_recep_arch),
                YEAR(fecha_recep_arch)
         INTO   vmes_recep,
                vano_recep
         FROM   com_calendario
         WHERE  MONTH(fecha_recep_arch) = MONTH(g_reg.fecha_corte)
         AND    YEAR(fecha_recep_arch) = YEAR(g_reg.fecha_corte)

         IF SQLCA.SQLCODE = NOTFOUND THEN
            ERROR "ESTE MES NO ES DE RECAUDACION"
            NEXT FIELD fecha_corte
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
      CALL Ejecuta_Reliquidacion()
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_Reliquidacion()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB003",               -- proceso_cod
       2,                       -- etapa_cod  -- CALCULO COMISION CON SALARIO
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_corte,       -- parametro1  -- fecha_corte
       NULL,                    -- parametro2
       NULL,                    -- parametro3 
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Calculo Reliquidacion",
      STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo Reliquidacion por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB003.4gi ",g_reg.fecha_corte,"  & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Calcula_cierre()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF
         
         LET cla_sel = "SELECT FIRST 1 fecha_corte ",
                       "FROM   com_comis_resumen ",
	               "WHERE  fecha_corte = ","'",g_reg.fecha_corte,"'" CLIPPED

         PREPARE claexe22 FROM cla_sel
         DECLARE cur_fecha22 CURSOR FOR claexe22
         OPEN cur_fecha22
         FETCH cur_fecha22 INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha22
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha22

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
      CALL Ejecuta_calculo_cierre()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION

FUNCTION Ejecuta_calculo_cierre()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB029",               -- proceso_cod
       7,                       -- etapa_cod  -- PROVISION COMISION
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_corte,       -- parametro1  -- fecha_corte
       NULL,                    -- parametro2
       NULL,                    -- parametro3 
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Cierre periodo ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Cierre del periodo por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB029.4gi ",g_reg.fecha_corte,"  & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION
