-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => COMB031B                                                  --
-- Descripcion  => PROGRAMA QUE EJECUTA PROGRAMAS DEL CALCULO COMISION       --
--              => NUEVA VERSION DE JULIO GUERRERO                           --
-- Sistema      => COM                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 12 octubre 2004.                                          --
-- Modificado   =>                                                           --
-- Fecha        =>                                                           --
-------------------------------------------------------------------------------

DATABASE  safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_corte DATE
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

   MENU "Comision"
      COMMAND "(Masa/registro)" "Calcula Comision masa salarial sobre registro"
         CALL  Masa_slarial_regi()

      COMMAND "(Masa/recaudacion)" "Calcula Comision masa salarial sobre recaudacion"
         CALL  Masa_slarial_reca()

      COMMAND "(Concurso)" "Calcula premio a promotores"
         CALL  Calcula_concurso()

      COMMAND "(Cierre)" "Calcula cierre del periodo"
         CALL  Calcula_cierre()

      COMMAND "Salida"        "Salida"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN

FUNCTION Masa_slarial_regi()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF
         
         LET cla_sel = "SELECT FIRST 1 fecha_corte ",
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
      CALL Ejecuta_Masa_slarial_regi()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION

FUNCTION Ejecuta_Masa_slarial_regi()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB032",               -- proceso_cod
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
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Masa_slarial_regi ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Comision Masa salarial regi por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB032.4gi ",g_reg.fecha_corte,"  & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Masa_slarial_reca()

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
                     " AND estado_comision>=250 " 

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
      CALL Ejecuta_Masa_slarial_reca()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION

FUNCTION Ejecuta_Masa_slarial_reca()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB034",               -- proceso_cod
       2,                       -- etapa_cod  -- PROVISION COMISION
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
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Masa_slarial_regi ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Comision Masa salarial reca por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB034.4gi ",g_reg.fecha_corte,"  & "   
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION

FUNCTION Calcula_concurso()

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR "La fecha no pude ser nula"
            NEXT FIELD fecha_corte
         END IF
         
         LET cla_sel = "SELECT FIRST 1 fecha_corte ",
                       "FROM   com_comis_detalle ",
	               "WHERE  fecha_corte = ","'",g_reg.fecha_corte,"'" CLIPPED,
                  "AND  nss = 'VARIOS' "

         PREPARE claexe3 FROM cla_sel
         DECLARE cur_fecha3 CURSOR FOR claexe3
         OPEN cur_fecha3
         FETCH cur_fecha3 INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha3
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha3

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
      CALL Ejecuta_calculo_concurso()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION

FUNCTION Ejecuta_calculo_concurso()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO com_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "COMB030",               -- proceso_cod
       9,                       -- etapa_cod  -- PROVISION COMISION
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
      ERROR "ERROR AL INSERTA EN TABLA com_ctrl_proceso Calculo concurso",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
        
   ERROR "Ejecutando Calculo concurso  por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo COMB030.4gi ",g_reg.fecha_corte,"  & "   
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

         PREPARE claexe4 FROM cla_sel
         DECLARE cur_fecha4 CURSOR FOR claexe4
         OPEN cur_fecha4
         FETCH cur_fecha4 INTO vfecha
         IF SQLCA.SQLCODE <> NOTFOUND THEN
            ERROR "ESTA FECHA YA FUE PROCESADA"
            CLOSE cur_fecha4
            NEXT FIELD fecha_corte
         END IF
         CLOSE cur_fecha4

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
       10,                      -- etapa_cod  -- PROVISION COMISION
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
