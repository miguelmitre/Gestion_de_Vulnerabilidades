###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Owner             => E.F.P                                               #
#Programa DISB029  => Posocion siefore                                    #
#Fecha             => 12 de octubre 2001.                                 #
#Por               => GERARDO ALFONSO VEGA PAREDES.                       #
#Sistema           => DIS.                                                #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE afore CHAR(25)

   DEFINE hoy DATE

   DEFINE g_usuario   CHAR (08)
   DEFINE g_param_dis RECORD LIKE dis_parametro.*
   DEFINE G_LISTA     CHAR(300)
   DEFINE G_IMPRE     CHAR(300)
   DEFINE impresion   CHAR(300)
   DEFINE hora        CHAR (08)

   DEFINE v RECORD
      folio INTEGER
   END RECORD

   DEFINE l_record      RECORD
      nss               CHAR(11),
      monto_en_acciones DECIMAL(16,6)
   END RECORD
    
   DEFINE g_reg RECORD
      contrcv INTEGER,
      contvol INTEGER,
      contsar INTEGER,
      totrcv  DECIMAL(16,6),
      totvol  DECIMAL(16,6),
      totsar  DECIMAL(16,6),
      tot_nss INTEGER
   END RECORD

   DEFINE opc CHAR(01)

   DEFINE tot_nss INTEGER

END GLOBALS

MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY control-o

   DEFER INTERRUPT

   SELECT  razon_social,USER
   INTO    afore,g_usuario
   FROM    tab_afore_local

   SELECT  ruta_spool
   INTO    g_param_dis.ruta_spool
   FROM    dis_parametro

   LET hoy = TODAY

   OPEN WINDOW ventana_2 AT 3,4 WITH FORM "DISB0291" ATTRIBUTE(BORDER)
   DISPLAY " DISB029                                                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING " dd-mm-yyyy  " AT 3,61 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-c] Salir                                       [Ctrl-p] Imprimir " AT 4,1

   MENU "Posicion siefore"
      COMMAND "Consulta" 
        CALL Consulta() 
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU 
   END MENU 
                                          
   CLOSE WINDOW ventana_2 
END MAIN

FUNCTION Consulta()

   DEFINE pos       INTEGER,
          prioridad CHAR(25)

   LET prioridad = "set pdqpriority high"
   PREPARE clapri FROM prioridad         
   EXECUTE clapri                        

   ERROR "PROCESANDO INFORMACION..."

   LET g_reg.contrcv = 0
   LET g_reg.totrcv  = 0
   LET g_reg.contvol = 0
   LET g_reg.totvol  = 0
   LET g_reg.contsar = 0
   LET g_reg.totsar  = 0
   LET g_reg.tot_nss = 0
  
   DECLARE cursor_rcv CURSOR FOR
   SELECT nss,
          sum(monto_en_acciones)
   FROM   cta_saldo_cuenta
   WHERE  subcuenta in (1,2,5,6,9)
     AND  monto_en_acciones > 0.00
   GROUP  BY 1
   ORDER  BY 1

   FOREACH cursor_rcv INTO l_record.*
      LET g_reg.contrcv = g_reg.contrcv + 1
      LET g_reg.totrcv  = g_reg.totrcv  + l_record.monto_en_acciones
   END FOREACH

   -- Despliega la informacion de RCV
      DISPLAY BY NAME g_reg.contrcv, g_reg.totrcv

   DECLARE cursor_vol CURSOR FOR
   SELECT nss,
          sum(monto_en_acciones)
   FROM   cta_saldo_cuenta
   WHERE  subcuenta = 3
     AND  monto_en_acciones > 0.00
   GROUP  BY 1
   ORDER  BY 1

   FOREACH cursor_vol INTO l_record.*
      LET g_reg.contvol = g_reg.contvol + 1
      LET g_reg.totvol  = g_reg.totvol  + l_record.monto_en_acciones
   END FOREACH

   -- Despliega la informacion de VOL
      DISPLAY BY NAME g_reg.contvol, g_reg.totvol

   DECLARE cursor_sar CURSOR FOR
   SELECT nss,
          sum(monto_en_acciones)
   FROM   cta_saldo_cuenta
   WHERE  subcuenta = 7
     AND  monto_en_acciones > 0.00
   GROUP  BY 1
   ORDER  BY 1

   FOREACH cursor_sar INTO l_record.*
      LET g_reg.contsar = g_reg.contsar + 1
      LET g_reg.totsar  = g_reg.totsar  + l_record.monto_en_acciones
   END FOREACH

   -- Despliega la informacion de SAR
      DISPLAY BY NAME g_reg.contsar, g_reg.totsar
   
   SELECT count(unique nss)
   INTO   g_reg.tot_nss
   FROM   cta_saldo_cuenta
   WHERE  subcuenta in (1,2,3,5,6,7,9)
     AND  monto_en_acciones > 0.00

   ERROR ""

   DISPLAY BY NAME g_reg.tot_nss

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.* WITHOUT DEFAULTS
      ON KEY(control-p)
         CALL Imprime(g_reg.*)
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT
   
   CLEAR FORM
   CLEAR SCREEN

END FUNCTION

FUNCTION Imprime(g_reg)
   DEFINE g_reg RECORD
      contrcv INTEGER,
      contvol INTEGER,
      contsar INTEGER,
      totrcv  DECIMAL(16,6),
      totvol  DECIMAL(16,6),
      totsar  DECIMAL(16,6),
      tot_nss INTEGER
   END RECORD

   LET hora = TIME

   LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                             ".DISB029.",HOY USING "DD-MM-YYYY",
                             ".",hora CLIPPED

   START REPORT rpt_cuenta_imp TO G_IMPRE

   CALL cuentas_impri(g_reg.*)

   LET impresion = "lp ",G_IMPRE
   RUN impresion

END FUNCTION

FUNCTION cuentas_impri(g_reg)
   DEFINE g_reg RECORD
      contrcv INTEGER,
      contvol INTEGER,
      contsar INTEGER,
      totrcv  DECIMAL(16,6),
      totvol  DECIMAL(16,6),
      totsar  DECIMAL(16,6),
      tot_nss INTEGER
   END RECORD

   OUTPUT TO REPORT rpt_cuenta_imp(g_reg.*)

   FINISH REPORT rpt_cuenta_imp

   ERROR "LISTADO GENERADO...."
   SLEEP 2
   ERROR ""

END FUNCTION

REPORT rpt_cuenta_imp(g_reg)
   DEFINE g_reg RECORD
      contrcv INTEGER,
      contvol INTEGER,
      contsar INTEGER,
      totrcv  DECIMAL(16,6),
      totvol  DECIMAL(16,6),
      totsar  DECIMAL(16,6),
      tot_nss INTEGER
   END RECORD

   OUTPUT
     TOP MARGIN 1
     BOTTOM MARGIN 0
     LEFT MARGIN   0
     RIGHT MARGIN  0
     PAGE LENGTH   60

   FORMAT
      PAGE HEADER
         PRINT '\033e\033(s1p11v0s0b4168T\033(s8H\033(s7B'      
         PRINT COLUMN 01, afore CLIPPED
         PRINT
         PRINT COLUMN 01,  "DISB029",
               COLUMN 35,  "Reporte de trabajadores con posicion en Siefore",
               COLUMN 100, TODAY USING "mm-dd-yyyy"
         SKIP 2 LINE

         PRINT '\033e\033(s1p8v0s0b4168T\033(s8H\033(s7B'      
         PRINT COLUMN 20,  "No.Traba.",
               COLUMN 50,  "Sdo.RCV.",
               COLUMN 90,  "Sdo.VOL.",
               COLUMN 135, "Sdo.SAR."
         SKIP 1 LINE

      ON EVERY ROW
         PRINT '\033e\033(s1p8v0s0b4168T\033(s11H\033(s7B'      
         PRINT COLUMN 19, g_reg.contrcv  USING "-------&",
               COLUMN 45, g_reg.totrcv   USING "--,---,---,--&.&&"

         SKIP 1 LINE

         PRINT COLUMN 19, g_reg.contvol  USING "-------&",
               COLUMN 95, g_reg.totvol   USING "--,---,---,--&.&&"

         SKIP 1 LINE

         PRINT COLUMN 19,  g_reg.contsar  USING "-------&",
               COLUMN 140, g_reg.totsar   USING "--,---,---,--&.&&"

         SKIP 1 LINE
      
         PRINT COLUMN 19,  "Total de Trabajadores con posicion: ",g_reg.tot_nss USING '------------'
END REPORT
