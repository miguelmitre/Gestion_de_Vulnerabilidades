###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa  UNIC040 => CONSULTA DE CRUCE DE OP21 con OP73                      #
#Sistema           => UNI                                                     #
#Autor             => Omar Sandoval Badillo                                   #
#Fecha             => 27 de enero de 2004.                                    #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param         RECORD LIKE seg_modulo.*

   DEFINE aux_pausa      CHAR(1),
	  pos            SMALLINT,
	  total           SMALLINT,
	  sw_1           SMALLINT,
	  sw             SMALLINT,
	  hora           CHAR(8),
	  hoy            DATE,
	  usuario        CHAR(8),
	  sel_where      CHAR(300),
	  cla_where      CHAR(300),
	  opc            CHAR(1),
	  g_impre        CHAR(300),
	  g_lista        CHAR(300)

   DEFINE gr_cuentas    RECORD
	  nss_uni          CHAR(11),
	  nss_cta1         CHAR(11),
	  nombre           CHAR(40),
	  fecha_solicitud  CHAR(10)
   END RECORD

   DEFINE l_record   ARRAY[3000] OF RECORD
          nss_cta1         CHAR(11),
	  nss_uni          CHAR(11),
	  nombre           CHAR(40),
	  fecha_solicitud  CHAR(10)
   END RECORD
END GLOBALS
######################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O,
      HELP FILE "UNIHLP.ex",
      HELP KEY CONTROL-R

   DEFER INTERRUPT

   LET hoy = TODAY

   SELECT USER,
	  ruta_listados,
	  ruta_envio
   INTO   usuario,
	  g_param.ruta_listados,
	  g_param.ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   CALL STARTLOG("UNIC040.log")
   CALL principal()

END MAIN
######################################################################
FUNCTION principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0401" ATTRIBUTE(BORDER)
   DISPLAY " UNIC040             CONSULTA DE CRUCE DE OP21 CON OP73                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD/MM/YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   MENU "CUENTAS"
      COMMAND "Consulta" "Consulta los NSS unificados"
	 CALL consulta()
      COMMAND "Salir" "Sale del programa"
	 EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
######################################################################
FUNCTION consulta()

   OPEN WINDOW ventana_2 AT 2,2 WITH FORM "UNIC0401" ATTRIBUTE(BORDER)
   DISPLAY " UNIC040            CONSULTA DE CRUCE DE OP21 CON OP73               CONSULTA  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " (Ctrl-p) Imprimir         (Control-B) Archivo                 (Ctrl-C) Salir " AT 2,1
   
   LET total = 0
   LET int_flag = FALSE

   CONSTRUCT cla_where ON a.folio,
			  a.nss_cta1,
			  a.nss_uni
                     FROM x_folio,
                          x_nss_cta1,
                          x_nss_uni

      ON KEY (CONTROL-M)
         ERROR "PROCESANDO INFORMACION..."
	 LET int_flag = FALSE
	 EXIT CONSTRUCT
      ON KEY (CONTROL-C)
	 LET int_flag = TRUE
	 EXIT CONSTRUCT

   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "PROCESO DE CONSULTA CANCELADO"
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana_2
      RETURN
   END IF

   LET sel_where = "SELECT b.nss_cta1,",
			  "b.nss_uni,",
			  "NVL(TRIM(b.nombre),' ')||' '",
			  "||NVL(TRIM(b.paterno),' ')||' '",
			  "||NVL(TRIM(b.materno),' ') nombre,",
			  "b.fecha_solicitud ",
                   " FROM uni_unificado a,uni_solicitud b ",
		   " WHERE ", cla_where CLIPPED , 
		   " AND a.nss_uni  = b.nss_uni ",
		   " AND a.nss_cta1 = b.nss_cta1 "
--  		   " AND a.estado = 10 "
--		   " AND b.estado = 30"

   PREPARE query FROM sel_where
   DECLARE cursor_1 CURSOR FOR query

   LET pos = 1

   FOREACH cursor_1 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   LET total = pos - 1

   DISPLAY BY NAME total
   DISPLAY "  Unificado    Unificador                Nombre               Fecha Solicitud  " AT 7,1 ATTRIBUTE(REVERSE)
   DISPLAY " Total de Registros " AT 20,1

   INITIALIZE l_record[pos].* TO NULL

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      ERROR ""

      DISPLAY ARRAY l_record TO scr_1.*

         ON KEY (CONTROL-P)
	    CALL impresion(pos)
         ON KEY (CONTROL-B)
	    CALL archivo(pos)
         ON KEY (INTERRUPT)
	    ERROR "CONSULTA CANCELADA..."
	    SLEEP 2
	    ERROR ""
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "ARCHIVO DE CUENTAS ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana_2
END FUNCTION
######################################################################
FUNCTION impresion(pos)
   DEFINE i,pos   SMALLINT

   LET g_impre = g_param.ruta_listados CLIPPED,"/",
		 usuario CLIPPED," .IMPUNINSS",
                 hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_cuentas TO g_impre

   FOR i = 1 TO (pos+1)

      LET gr_cuentas.nss_uni  = l_record[i].nss_uni   
      LET gr_cuentas.nss_cta1 = l_record[i].nss_cta1      
      LET gr_cuentas.nombre   = l_record[i].nombre
      LET gr_cuentas.fecha_solicitud = l_record[i].fecha_solicitud

      IF gr_cuentas.nss_uni IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuentas(gr_cuentas.*)
   END FOR

   FINISH REPORT rpt_cuentas

   PROMPT  "LISTADO GENERADO...<ENTER> PARA CONTINUAR " FOR opc

   LET g_lista = "lp ",g_impre
   RUN g_lista


END FUNCTION
##############################################################################
REPORT rpt_cuentas(gr_cuentas)

   DEFINE gr_cuentas RECORD
	  nss_uni          CHAR(11),
	  nss_cta1         CHAR(11),
	  nombre           CHAR(30),
	  fecha_solicitud  DATE
   END RECORD

    OUTPUT
	TOP MARGIN    1
        BOTTOM MARGIN 0
	LEFT MARGIN   0
        RIGHT MARGIN  0
	PAGE LENGTH   60

   FORMAT
      PAGE HEADER
	 PRINT COLUMN 12," REPORTE DE LAS CUENTAS QUE FUERON DISPERSADAS Y",
			 " CORRESPONDEN A LA OP.73 "

               SKIP 1 LINE
	 PRINT COLUMN 02," UNIC040 ",
	       COLUMN 67, TODAY USING "dd-mm-yyyy"
	       SKIP 1 LINE

         PRINT COLUMN 03," UNIFICADOS ",
	       COLUMN 16," UNIFICADOR ",
	       COLUMN 31," NOMBRE ",
	       COLUMN 64," FECHA SOLICITUD "
	       SKIP 1 LINE

         ON EVERY ROW
            PRINT COLUMN 03,gr_cuentas.nss_cta1 ,
	          COLUMN 16,gr_cuentas.nss_uni  ,
	          COLUMN 32,gr_cuentas.nombre   ,
	          COLUMN 67,gr_cuentas.fecha_solicitud

         ON LAST ROW
	    SKIP 4 LINE
            PRINT COLUMN 01,"Total de Registros : ",COUNT(*) USING "<<<<"

         PAGE TRAILER
	    SKIP 2 LINE
            PRINT COLUMN 60," Pagina : ",PAGENO USING"<<<<<"

END REPORT
############################################################################
FUNCTION archivo(pos)

 DEFINE i,pos   SMALLINT

  LET g_impre = g_param.ruta_envio CLIPPED,"/",
	   	hoy USING "ddmmyyyy" CLIPPED, ".21_73"

  START REPORT rpt_arcuenta TO g_impre

  FOR i = 1 TO (pos+1)

     LET gr_cuentas.nss_uni  = l_record[i].nss_uni
     LET gr_cuentas.nss_cta1 = l_record[i].nss_cta1
     LET gr_cuentas.nombre   = l_record[i].nombre
     LET gr_cuentas.fecha_solicitud = l_record[i].fecha_solicitud

     IF gr_cuentas.nss_uni IS NULL THEN
	EXIT FOR
     END IF

     OUTPUT TO REPORT rpt_arcuenta(gr_cuentas.*)
  END FOR

  FINISH REPORT rpt_arcuenta

  PROMPT "ARCHIVO GENERADO...<ENTER> PARA CONTINUAR" FOR opc


END FUNCTION
###########################################################################
REPORT rpt_arcuenta(gr_cuentas)

   DEFINE gr_cuentas RECORD
	  nss_uni          CHAR(11),
          nss_cta1         CHAR(11),
	  nombre           CHAR(30),
	  fecha_solicitud  DATE
   END RECORD

   OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0

   FORMAT

   ON EVERY ROW
      PRINT COLUMN 01,gr_cuentas.nss_cta1 ,"|",
                      gr_cuentas.nss_uni  ,"|",
	              gr_cuentas.nombre   ,"|",
	              gr_cuentas.fecha_solicitud, "|"

END REPORT
