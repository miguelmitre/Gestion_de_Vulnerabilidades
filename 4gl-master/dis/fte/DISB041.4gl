######################################################################
#Proyecto            => Sistema de Afores. (MEXICO)                  #
#Propietario         => E.F.P.                                       #
#Sistema             => DIS                                          #
#Programa            => DISB041                                      #
#Descripcion         => Reverso de Provision y Liquidacion Int Trans.#
#                    => EST.                                         #
#Fecha               => 17 de JULIO de 2002                          #
#Por                 => GERARDO ALFONSO VEGA PAREDES.                #
----------------------------------------------------------------------
#Modifi.             => 24-05-2005 ALEJANDRO RAMIREZ                 #
#Descr.              => Estaba borrando rcv y no est de liquidacion  #
######################################################################
DATABASE safre_af
GLOBALS
   DEFINE hoy        DATE,
	  opc        CHAR(01)

   DEFINE vfolio     INTEGER,
	  vpasswd    CHAR(01),
	  vnip       INTEGER

   DEFINE g_reg4  RECORD
      super_cod      SMALLINT,
      super_desc     CHAR(30),
      nip            INTEGER
   END RECORD
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   LET vpasswd = "N"

   CALL Aplica_passwd() RETURNING vpasswd

   IF vpasswd = "S" THEN
      ERROR "ACCESO ACEPTADO"
      SLEEP 2
      ERROR ""
      CALL inicio()
   END IF
END MAIN


FUNCTION Aplica_passwd()
   OPEN WINDOW ventana_4 AT 08,12 WITH FORM "DISB0413" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Procesar      [DISB041]      [Ctrl-c] Cancelar " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_reg4.*
      AFTER FIELD super_cod
	 IF g_reg4.super_cod IS NULL THEN
	    ERROR "Id no puede ser nulo"
	    NEXT FIELD super_cod
         END IF

         SELECT super_desc,
		nip
         INTO   g_reg4.super_desc,
		vnip
         FROM   tab_supervisor
	 WHERE  super_cod = g_reg4.super_cod

	 IF STATUS = NOTFOUND THEN
	    ERROR "No existe este Id"
	    NEXT FIELD super_cod
         END IF

	 DISPLAY BY NAME g_reg4.super_desc
	 NEXT FIELD nip

      AFTER FIELD nip
         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

      ON KEY (ESC)
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID no puede ser nulo"
            NEXT FIELD super_cod
         END IF

         SELECT "x"
           FROM tab_supervisor 
          WHERE super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "No existe este ID "
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "Clave de acceso no existe"
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip <> vnip THEN
            ERROR "Clave de acceso no existe"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         LET vpasswd = "S"
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR "Acceso denegado"
         SLEEP 2
         LET vpasswd = "N"
         EXIT INPUT
          
   END INPUT

   CLOSE WINDOW ventana_4
   RETURN vpasswd
END FUNCTION


FUNCTION inicio()
  LET hoy = TODAY

  OPEN WINDOW ventana AT 3,4 WITH 3 ROWS, 72 COLUMNS ATTRIBUTE(BORDER)
  
  DISPLAY " DISB040 " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY hoy USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

   MENU "Reverso Int Transito EST" 
      COMMAND "Reverso 1" "Provision Intereses Transito EST."
         CALL Reverso_1()
      COMMAND "Reverso 2" "Liquidacion Intereses Transito EST."
         CALL Reverso_2()
      COMMAND "Salir" "Salir del menu principal."
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana
END FUNCTION


FUNCTION Reverso_1()
   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0411" ATTRIBUTE(BORDER)

   DISPLAY " (Control-c) Salir                               (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
	 LET INT_FLAG = FALSE
	 EXIT INPUT
      ON KEY (control-c)
	 LET INT_FLAG = TRUE
	 EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT UNIQUE folio
   FROM   cta_interes_rcv
   WHERE  folio = vfolio
   AND    subcuenta in (5,6,9)
   AND    tipo_movimiento = 3

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE
      PROMPT "Deseas hacer el reverso [S/N]..." FOR opc
      IF opc MATCHES "[Ss]" THEN
	 ERROR "PROCESANDO INFORMACION..."

	 DELETE 
	 FROM   cta_interes_rcv
	 WHERE  folio = vfolio
         AND    subcuenta in (5,6,9)
	 AND    tipo_movimiento = 3
       
         ERROR ""

	 PROMPT "Proceso finalizado oprima [Enter] para salir..." for opc
      ELSE
	 ERROR "PROCESO CANCELADO..."
	 SLEEP 2
      END IF
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF
END FUNCTION


FUNCTION Reverso_2()
   DEFINE    fech_proces     DATE

   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0411" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                               (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
	 LET INT_FLAG = FALSE
	 EXIT INPUT
      ON KEY (control-c)
	 LET INT_FLAG = TRUE
	 EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   SELECT UNIQUE folio
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (5,6,9)
   AND    tipo_movimiento = 3
   AND    estado = 7

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE
      PROMPT "Deseas hacer el reverso [S/N]..." FOR opc
      IF opc MATCHES "[Ss]" THEN
	 ERROR "PROCESANDO INFORMACION..."

	 DELETE 
	 FROM   dis_cuenta
	 WHERE  folio = vfolio
         AND    subcuenta in (5,6,9)
	 AND    tipo_movimiento = 3
         AND    estado = 7

         SELECT fecha_proceso
           INTO fech_proces
           FROM dis_ctrl_proceso
          WHERE folio = vfolio
            AND proceso_cod = 'DISB027'

         DELETE
           FROM dis_ctrl_proceso
          WHERE fecha_proceso = fech_proces
            AND proceso_cod IN ('DISB027','DISB078B')

         ERROR ""

	 PROMPT "Proceso finalizado oprima [Enter] para salir..." for opc
      ELSE
	 ERROR "PROCESO CANCELADO..."
	 SLEEP 2
      END IF
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF

END FUNCTION

