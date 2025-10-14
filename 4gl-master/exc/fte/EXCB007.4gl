#********************************************************************#
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => EXCB007                                             #
#Descripcion  => PARAMETRO PARA EJECUTAR ERSPUESTA A PROCESAR        #
#Fecha        => 17 de septiembre de 2001                            #
#Por          => MIGUEL ANGEL HERNANDEZ MARTINEZ                     #
#Actualizado  => 12 de noviembre de 2001.                            #
#Actualizado  => 21 de febrero de 2002.                              #
#Actualizado  => 06 de junio de 2002.                                #
#Sistema      => EXC.                                                #
#********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          folio              CHAR(15)
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08)

END GLOBALS
#*********************************************************************
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   SELECT *,
          USER
   INTO   gparam_dev.*, 
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "EXCB0071" ATTRIBUTE(BORDER)

   DISPLAY " EXCB007       DEV. DE PAG. EN EXCESO PROCESO LIB. DE TERMINAL                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   DISPLAY "                  ARCHIVO DE RESPUESTA A PROCESAR                              " AT 5,1 ATTRIBUTE(REVERSE) 

   MENU "PROCESOS BATCH" 
      COMMAND "Archivo IMSS" "Archivo de respuesta devolucion de pagos IMSS."
         CALL Lectura(1)
      COMMAND "Archivo ISSSTE" "Archivo de respuesta devolucion de pagos ISSSTE."
         CALL Lectura(2)         
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
#*********************************************************************
FUNCTION Lectura(vtipo_ejec)

   DEFINE
      vtipo_ejec   SMALLINT

   LET hoy = TODAY

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.folio

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   dis_ctrl_proceso
         WHERE  folio = g_reg.folio
         AND    etapa_cod = 1
         AND    proceso_cod = "EXC"
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            PROMPT "FOLIO DE PAGOS EN EXCESO NO EXISTE" ATTRIBUTE (REVERSE)
                   FOR opc ATTRIBUTE (REVERSE)
            NEXT FIELD folio
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

      CALL Ejecuta_lectura(vtipo_ejec)
   ELSE
      ERROR "Generacion de archivo CANCELADO"
      SLEEP 2
      ERROR ""
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION Ejecuta_lectura(vtipo_ejec)

   DEFINE
      vtipo_ejec   SMALLINT
      
   LET hora_inicial = TIME
   LET hora_final   = NULL

   SELECT "X"
   FROM   dis_ctrl_proceso
   WHERE  folio = g_reg.folio
   AND    proceso_cod = "EXC"
   AND    etapa_cod = 6
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN
      UPDATE dis_ctrl_proceso
      SET    hora_inicial = hora_inicial,
             hora_final = hora_final,
             parametro2 = TODAY,
             usuario = usuario
      WHERE  folio = g_reg.folio
      AND    proceso_cod = "EXC"
      AND    etapa_cod = 6
   ELSE
      INSERT INTO dis_ctrl_proceso
      VALUES (TODAY,                   -- fecha_proceso
              "EXC",                   -- proceso_cod
              6,                       -- etapa_cod   -- ARCHIVO DE SALIDA
              hora_inicial,            -- hora_inicial
              hora_final,              -- hora_final
              g_reg.folio,             -- parametro1
              "",                      -- parametro2
              "",                      -- parametro3
              "",                      -- parametro4
              vtipo_ejec,              -- parametro5
              g_reg.folio,             -- folio 
              NULL,                    -- resultado
              usuario,                 -- usuario
              0                        -- correlativo
             )

      IF SQLCA.SQLCODE <> 0 THEN
         ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
         SLEEP 4
         EXIT PROGRAM
      END IF
   END IF

   ERROR "Ejecutando Lectura y Proceso de Archivo por nohup ..." 
   SLEEP 3
   ERROR ""

   LET ejecuta = "nohup fglgo EXCB010.4gi ",vtipo_ejec," ",g_reg.folio," & "
   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."
   SLEEP 2
   ERROR ""
END FUNCTION
