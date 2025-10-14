###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB070                                                 #
#Descripcion       => Genera nss c/saldo cero, sin cuenta, icefas devuleas,   #
#                  => pagos en exceso y voluntarias.                          #
#Fecha Inicio      => 04-julio-2004.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE hoy           DATE
   DEFINE fecha_proceso DATE
   DEFINE fecha_corte   DATE
   DEFINE opc          CHAR(01)
   DEFINE cla_ins      CHAR(500)
   DEFINE acelera_on   CHAR(30)
   DEFINE usuario      CHAR(12)
   DEFINE hora_inicial CHAR(08),
          hora_final   CHAR(08),
          ejecuta      CHAR(200)

END GLOBALS

MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   CALL Captura()


END MAIN

FUNCTION Captura()
   LET hoy = TODAY
   LET INT_FLAG = FALSE

   OPEN WINDOW v1 AT 2,2 WITH FORM "DISB0701" ATTRIBUTE(BORDER)
   DISPLAY " DISB070             IDENTIFICA CTAS CON SALDO CERO                            " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 1,67 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 3,62 ATTRIBUTE(REVERSE)

   INPUT BY NAME fecha_proceso   
      AFTER FIELD fecha_proceso
      IF fecha_proceso IS NULL THEN
         ERROR "La fecha no puede ser nula"
         NEXT FIELD fecha_proceso
      END IF 

      --Obtiene fecha inicio (7 bim atras)
      LET fecha_corte = fecha_proceso - 14 UNITS MONTH

      EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "OPERACION CANCELADA..."
      SLEEP 3
      EXIT PROGRAM
   END IF

   PROMPT "Desea ejecutar el proceso [S/N]... " FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_saldo_cero()
   END IF

END FUNCTION

FUNCTION Ejecuta_saldo_cero()

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB070",               -- proceso_cod
       9,                       -- etapa_cod     -- SALDO CERO
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       fecha_proceso,           -- parametro1
       "",                      -- parametro2
       "",                      -- parametro3
       "",                      -- parametro4
       "",                      -- parametro5
       "",                      -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando Proceso Saldo cero por nohup ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB070B.4gi ",fecha_proceso CLIPPED," ",fecha_corte CLIPPED,"  &"
   RUN ejecuta
   ERROR "El proceso se ejecuto satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION


