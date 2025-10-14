################################################################################
#Owner             => E.F.P.
#Programa RETC028  => RECIBE RESPUESTA DE IMSS POSIBLE UNIFICACION / Op 73
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.
#fecha actualiza   => 25 de septiembre 2003
#Sistema           => UNI
################################################################################
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          nombre_archivo     CHAR(15)
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          cla_where          CHAR(200)

END GLOBALS
#*********************************************************************
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("UNIC028.log")

   SELECT *,
          USER
   INTO   gparam_dev.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "UNIC0281" ATTRIBUTE(BORDER)

   DISPLAY " UNIC028                                                                       " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "                        LECTURA  DE  ARCHIVO  Y  PROCESO                       " AT 5,1 ATTRIBUTE(REVERSE) 

   MENU "CARGA ARCHIVO" 
      COMMAND "Lectura" "Lectura archvio de respuesta de posibles unificados."
         CALL Lectura()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
#*********************************************************************
FUNCTION Lectura()

   DEFINE x_mes         INTEGER,
          x_ano         INTEGER,
          hoy2          DATE

   LET hoy = TODAY

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD nombre_archivo
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  parametro1 = g_reg.nombre_archivo
         AND    proceso_cod = "UNIC028"
         AND    etapa_cod = 1

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "NOMBRE DE ARCHIVO YA CARGADO,VERIFICAR POR FAVOR" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD nombre_archivo
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
      ERROR "PROCESANDO INFORMACION ..."

      LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                    "; ls > archivos" CLIPPED
      RUN ejecuta

      WHENEVER ERROR CONTINUE
         DROP TABLE archivos_uni
      WHENEVER ERROR STOP

      CREATE TEMP TABLE archivos_uni
         (campo  CHAR(100))

      LET ejecuta = gparam_dev.ruta_rescate CLIPPED,"/archivos" CLIPPED

      LOAD FROM ejecuta INSERT INTO archivos_uni

      SELECT "X"
      FROM   archivos_uni
      WHERE  campo = g_reg.nombre_archivo

      IF SQLCA.SQLCODE <> 0 THEN
         PROMPT "NOMBRE DE ARCHIVO INCORRECTO" FOR opc

         CLEAR FORM
         CLEAR SCREEN

         LET ejecuta = "cd ",gparam_dev.ruta_rescate CLIPPED,
                       "; rm archivos "
         RUN ejecuta

         RETURN
      END IF

      CALL Ejecuta_lectura()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION Ejecuta_lectura()

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "UNIC028",               -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.nombre_archivo,    -- parametro1
           NULL,                    -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           NULL,                    -- folio 
           NULL,                    -- resultado
           usuario,                 -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta="nohup fglgo UNIC036.4gi & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
#*********************************************************************

