DATABASE safre_af
DEFINE
   ejecuta               CHAR(150),
   usuario               CHAR(8),
   periodo               CHAR(8),
   band                  CHAR(1),
   opc                   CHAR(1),
   hoy                   DATE,
   per_pago              INTEGER

DEFINE
   g_reg RECORD
            fecha_corte  DATE
         END RECORD


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   CALL STARTLOG("DISB01_SML.log")

   SELECT USER
   INTO  usuario
   FROM  seg_modulo
   WHERE modulo_cod = "dis"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB01SML1" ATTRIBUTE(BORDER)
   DISPLAY " DISB01_SML        PROGRAMA DE CARGA DE INFORMACION SALARIOS                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    BARRIDO DE AFILIADOS Y APORTES POR PERIODO DE PAGO PARA OBTENER SALARIO        " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*

      AFTER FIELD fecha_corte
         IF g_reg.fecha_corte IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD fecha_corte
         END IF
      
         LET periodo = YEAR (g_reg.fecha_corte) USING "&&&&",
                       MONTH(g_reg.fecha_corte) USING "&&"
         LET per_pago = periodo 
    
         EXIT INPUT
  
       ON KEY(INTERRUPT)
          LET INT_FLAG = TRUE
          EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      EXIT PROGRAM
   END IF

   LET band = 0

   IF band = 0 THEN
      PROMPT " Desea ejecutar el Proceso [S/N] ... " FOR opc

      IF opc MATCHES '[Ss]' THEN
         ERROR " Lanzando Proceso por NOHUP ..."
         SLEEP 3

         ERROR " Procesando Informacion ..."

         LET ejecuta ="nohup time fglgo DISB001_SM.4gi ",per_pago USING "&&&&&&"
                      CLIPPED, " &"
         DISPLAY ejecuta
         SLEEP 3
         RUN ejecuta
      END IF
   END IF
END MAIN

