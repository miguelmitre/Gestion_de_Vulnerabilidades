#********************************************************************#
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => EXCB001                                             #
#Descripcion  => DEVOLUCION DE PAGOS EN EXCESO PROCESO BATCH         #
#Fecha        => 28 de marzo de 2001.                                #
#Por          => MIGUEL ANGEL HERNANDEZ MARTINEZ                     #
#Actualizado  => 15 de noviembre de 2001                             #
#Actualizado  => 27 de junio de 2002.                                #
#Actualizado  => 07 de enero de 2005.                                #
#Sistema      => EXC.                                                #
#********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          nombre_archivo     CHAR(18)
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          vsubcuenta         CHAR(10),
          vtipo_liquidacion  CHAR(10),
          vexceso            CHAR(01),
          cla_where          CHAR(200),
          fecha_1            DATE,
          fecha_2            DATE,
          vtasa_valor        DECIMAL(16,6)

END GLOBALS
#*********************************************************************
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("EXCB001.log")

   SELECT *,
          USER
   INTO   gparam_dev.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "EXCB0011" ATTRIBUTE(BORDER)

   DISPLAY " EXCB001  DEV. DE PAG. EN EXCESO PROCESO BATCH LIB. DE TERMINAL                " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "                        LECTURA  DE  ARCHIVO  Y  PROCESO                       " AT 5,1 ATTRIBUTE(REVERSE) 

   MENU "PROCESOS BATCH" 
      COMMAND "Lectura IMSS" "Lectura archivo de devolucion de pagos IMSS"
         CALL Lectura(1)
      COMMAND "Lectura ISSSTE" "Lectura archivo de devolucion de pagos ISSSTE"
         CALL Lectura(2)
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
#*********************************************************************
FUNCTION Lectura(valor)

   DEFINE x_mes         INTEGER,
          x_ano         INTEGER,
          valor         SMALLINT,
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
         AND    proceso_cod = "EXC"
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
         DROP TABLE archivos_dis
      WHENEVER ERROR STOP

      CREATE TEMP TABLE archivos_dis
         (campo  CHAR(100))

      LET ejecuta = gparam_dev.ruta_rescate CLIPPED,"/archivos" CLIPPED

      LOAD FROM ejecuta INSERT INTO archivos_dis

      SELECT "X"
      FROM   archivos_dis
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

      CALL Ejecuta_lectura(valor)
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION Ejecuta_lectura(valor1)
   DEFINE 
       valor1    SMALLINT,
       voper     CHAR (13)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   IF valor1 = 1 THEN
     LET voper = "SIDECO IMSS"
   ELSE
     LET voper = "SIDECO ISSSTE"
   END IF
   
   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "EXC",                   -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           g_reg.nombre_archivo,    -- parametro1
           vexceso,                 -- parametro2
           vtasa_valor,             -- parametro3
           fecha_2,                 -- parametro4
           voper,                    -- parametro5
           NULL,                    -- folio 
           NULL,                    -- resultado
           usuario,                 -- usuario
           0                        -- correlativo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta = "nohup time fglgo EXCB008.4gi ",valor1

   LET ejecuta= ejecuta CLIPPED, " 1>"," EXCB008.out" CLIPPED," 2>EXCB008.err & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
#*********************************************************************
