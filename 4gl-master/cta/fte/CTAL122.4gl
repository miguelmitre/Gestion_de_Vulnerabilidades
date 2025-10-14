######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAL122  => CONSULTA CIFRAS GLOBALES TIPO DE MOVIMIENTO    #
#                     QUE INHABILITO LA CUENTA POR SALDO CERO        #
#Fecha             => 05 de Noviembre de 2009.                       #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg        RECORD
    tipo_solicitud    SMALLINT,
    desc_tipo_sol     CHAR(30),
    tipo_trabajador   CHAR(02),
    desc_tipo_trab    CHAR(20),
    id_cuenta         CHAR(02),
    desc_id_cuenta    CHAR(40),
    tipo_movimiento   SMALLINT,
    desc_tipo_mov     CHAR(80),
    total             INTEGER
  END RECORD

  DEFINE l_record     ARRAY[30000] OF RECORD 
    tipo_solicitud    SMALLINT,
    desc_tipo_sol     CHAR(30),
    tipo_trabajador   CHAR(02),
    desc_tipo_trab    CHAR(20),
    id_cuenta         CHAR(02),
    desc_id_cuenta    CHAR(40),
    tipo_movimiento   SMALLINT,
    desc_tipo_mov     CHAR(80),
    total             INTEGER
  END RECORD

  DEFINE g_reg1       RECORD
    nss               CHAR(11),
    tipo_solicitud    SMALLINT,
    desc_tipo_sol     CHAR(30),
    tipo_trabajador   CHAR(02),
    desc_tipo_trab    CHAR(20),
    id_cuenta         CHAR(02),
    desc_id_cuenta    CHAR(40),
    tipo_movimiento   SMALLINT,
    desc_tipo_mov     CHAR(80)
  END RECORD

  DEFINE g_param_dis  RECORD LIKE seg_modulo.*

  DEFINE sw_1         SMALLINT,
    vfecha_corte      CHAR(10),
    dia1              CHAR(02),
    mes1              CHAR(02),
    anio1             CHAR(04),
    vfecha_corte1     CHAR(10),
    vfecha_corte2     DATE,
    aux_pausa         CHAR(01),
    HOY               DATE,
    aux_estad_desc    CHAR(40),
    seg_usuario       CHAR(08),
    pos               SMALLINT,
    cla_where         CHAR(900),
    sel_where         CHAR(900),
    pos2              SMALLINT,
    cla_where2        CHAR(900),
    sel_where2        CHAR(900),
    g_lista           CHAR(300),
    g_impre           CHAR(300),
    g_lista2          CHAR(300),
    g_impre2          CHAR(300),
    g_impre3          CHAR(300),
    vfecha_ini        DATE,
    gtotal            INTEGER,
    l                 SMALLINT

  DEFINE
    total_arh         INTEGER,
    total_reg         INTEGER

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-M

  DEFER INTERRUPT

  CALL STARTLOG("CTAL122.log")
  CALL inicio()
  CALL proceso()

END MAIN
##########################################################################
FUNCTION inicio()

  SELECT USER,*
  INTO   seg_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  SELECT ruta_listados, modulo_desc
  INTO   g_param_dis.ruta_listados, g_param_dis.modulo_desc
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

END FUNCTION
##########################################################################
FUNCTION proceso()

  LET HOY = TODAY

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAL1221" ATTRIBUTE( BORDER)
    DISPLAY " CTAL122           CONSULTA CIFRAS CTAS SDO CERO (POR TIP MOV)                     " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CONSULTA"
      COMMAND "Cifras Cuentas Saldo Cero"
        CALL Inicializa()
        CALL Consulta()
        CALL Inicializa()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

END FUNCTION
#############################################################################
FUNCTION Inicializa()
  DEFINE j SMALLINT

  LET j               = 0
  LET sw_1            = 0
  LET total_arh       = 0

  INITIALIZE g_reg.*  TO NULL
  INITIALIZE l_record TO NULL

  FOR j = 1 TO 10      
      INITIALIZE l_record[j].* TO NULL
      DISPLAY l_record[j].*
  END FOR

  DISPLAY "                                                                               " AT 16,1

END FUNCTION
################################################################################
FUNCTION Consulta()

  LET pos = 2

  DISPLAY BY NAME g_reg.*   
  DISPLAY BY NAME total_arh 

  IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)

     LET int_flag = FALSE

     CONSTRUCT cla_where ON vfecha_corte
                       FROM vfecha_corte

       BEFORE CONSTRUCT
	 BEFORE FIELD vfecha_corte
           LET vfecha_corte = NULL
	   MESSAGE "Capture Fecha de Corte"

       AFTER CONSTRUCT
	 AFTER FIELD vfecha_corte
	   MESSAGE ""
	   IF FIELD_TOUCHED(vfecha_corte) THEN
              LET vfecha_corte = GET_FLDBUF(vfecha_corte)

	      IF vfecha_corte IS NULL THEN
                 ERROR "La fecha de corte no debe ser nula"
		 NEXT FIELD vfecha_corte
              ELSE 
	         EXIT CONSTRUCT
	      END IF
           ELSE
	      IF vfecha_corte IS NULL THEN
                 ERROR "La fecha de corte no debe ser nula"
		 NEXT FIELD vfecha_corte
              END IF
	   END IF

       ON KEY (CONTROL-M)
          LET int_flag = FALSE
          EXIT CONSTRUCT

       ON KEY (CONTROL-C)
          LET int_flag = TRUE
          EXIT CONSTRUCT
     END CONSTRUCT

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA CANCELADA..."
        SLEEP 2
        ERROR ""
        CLEAR SCREEN
        RETURN
     END IF

     LET dia1          = cla_where[18,19]
     LET mes1          = cla_where[15,16]
     LET anio1         = cla_where[21,24]
     LET vfecha_corte1 = mes1, "/", dia1, "/", anio1
     LET vfecha_corte2 = vfecha_corte1
     LET vfecha_corte  = mes1, "/", dia1, "/", anio1
     LET cla_where     = " fecha_corte = '", vfecha_corte, "'"
     LET cla_where     = cla_where CLIPPED

     LET sel_where = "SELECT a.tipo_solicitud, b.desc_solicitud,",
		     " a.tipo_trabajador, d.desc_tipo_trabajador,",
		     {" CASE WHEN a.tipo_trabajador = '01' THEN 'AFILIADO'",
		     "      WHEN a.tipo_trabajador = '02' THEN 'NO AFILIADO'",
		     "      WHEN a.tipo_trabajador = '03' THEN 'ASIGNADO'",
		     " END CASE,",}
		     " a.id_cuenta, e.desc_id_registro,",
		     {" CASE WHEN a.id_cuenta = '01' THEN 'SIN APORTACION'",
		     "      WHEN a.id_cuenta = '02' THEN 'CON APORTACION'",
		     " END CASE,",}
		     " a.movto_operacion, c.descripcion, COUNT(*)",
                     " FROM cta_saldo_cero a, tab_tipo_solic b,",
		     " OUTER tab_movimiento c,",
		     " tab_tipo_trabajador d,",
		     " tab_id_reg_sdo_cero e",
                     " WHERE ", cla_where CLIPPED,
		     " AND a.tipo_solicitud = b.tipo_solicitud",
		     " AND a.movto_operacion = c.codigo",
		     " AND a.tipo_trabajador = d.tipo_trabajador",
		     " AND d.operacion = '99'",
		     " AND a.id_cuenta = e.id_registro",
		     " GROUP BY 1,2,3,4,5,6,7,8",
                     " ORDER BY 5,3,7,1"

     PREPARE query_p FROM sel_where
     DECLARE cursor_p CURSOR FOR query_p

     LET pos       = 1
     LET total_arh = 0
     FOREACH cursor_p INTO l_record[pos].*

       LET total_arh = l_record[pos].total + total_arh
       LET pos       = pos + 1

     END FOREACH

     DISPLAY BY NAME total_arh

     IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_record TO scr_1.*
        ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

          ON KEY (CONTROL-P)
             ERROR "PROCESANDO IMPRESION..."
             CALL impresion(pos, 1)

          ON KEY (CONTROL-B)
             ERROR "PROCESANDO IMPRESION..."
             CALL impresion(pos, 2)

          ON KEY (INTERRUPT)
             INITIALIZE l_record TO NULL
             CLEAR FORM
             EXIT DISPLAY
        END DISPLAY

        RETURN
        CLOSE WINDOW ventana_1
     ELSE
        ERROR "CONSULTA CIFRAS CONTROL VACIA"
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF

     DISPLAY "" AT 1,1
     DISPLAY "" AT 2,1
     DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
     DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

     DISPLAY BY NAME  g_reg.*
  END IF

  CLEAR SCREEN
END FUNCTION
################################################################################

################################################################################
FUNCTION impresion(pos, tip_rep)
   DEFINE 
     i,
     tip_rep,
     pos       SMALLINT 

   CASE tip_rep
     WHEN 1
       LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                     ".SDO_CERO_CIFRAS_CTRL_",hoy USING "ddmmyyyy" CLIPPED

       DISPLAY "Nombre reporte: ", g_impre AT 20,1

       START REPORT rpt_tabrdeta TO g_impre
    
       FOR i = 1 TO (pos + 1)
          LET g_reg.tipo_solicitud  = l_record[i].tipo_solicitud
          LET g_reg.desc_tipo_sol   = l_record[i].desc_tipo_sol
          LET g_reg.tipo_trabajador = l_record[i].tipo_trabajador
          LET g_reg.desc_tipo_trab  = l_record[i].desc_tipo_trab
          LET g_reg.id_cuenta       = l_record[i].id_cuenta
          LET g_reg.desc_id_cuenta  = l_record[i].desc_id_cuenta
          LET g_reg.tipo_movimiento = l_record[i].tipo_movimiento
          LET g_reg.desc_tipo_mov   = l_record[i].desc_tipo_mov
          LET g_reg.total           = l_record[i].total
    
          IF g_reg.tipo_solicitud IS NULL THEN
             EXIT FOR
          END IF
    
          OUTPUT TO REPORT rpt_tabrdeta(g_reg.*)
       END FOR
    
       FINISH REPORT rpt_tabrdeta

     WHEN 2
       LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                     ".SDO_CERO_DETALLE_",hoy USING "ddmmyyyy" CLIPPED

       DISPLAY "Nombre reporte: ", g_impre AT 20,1

       START REPORT rpt_tabrdeta2 TO g_impre

       LET sel_where2= "SELECT a.nss, a.tipo_solicitud, b.desc_solicitud,",
                       " a.tipo_trabajador, d.desc_tipo_trabajador,",
                       {" CASE WHEN a.tipo_trabajador = '01' THEN 'AFILIADO'",
                       "      WHEN a.tipo_trabajador = '02' THEN 'NO AFILIADO'",
                       "      WHEN a.tipo_trabajador = '03' THEN 'ASIGNADO'",
                       " END CASE,",}
                       " a.id_cuenta, e.desc_id_registro,",
                       {" CASE WHEN a.id_cuenta = '01' THEN 'SIN APORTACION'",
                       "      WHEN a.id_cuenta = '02' THEN 'CON APORTACION'",
                       " END CASE,",}
                       " a.movto_operacion, c.descripcion",
                       " FROM cta_saldo_cero a, tab_tipo_solic b,",
                       " OUTER tab_movimiento c,",
		       " tab_tipo_trabajador d,",
		       " tab_id_reg_sdo_cero e",
                       " WHERE a.fecha_corte = '", vfecha_corte2, "'",
                       " AND a.tipo_solicitud = b.tipo_solicitud",
                       " AND a.movto_operacion = c.codigo",
		       " AND a.tipo_trabajador = d.tipo_trabajador",
		       " AND d.operacion = '99'",
		       " AND a.id_cuenta = e.id_registro",
                       " ORDER BY 6,4,8,2,1"
       
       PREPARE query_d FROM sel_where2
       DECLARE cursor_d CURSOR FOR query_d
       FOREACH cursor_d INTO g_reg1.*
          OUTPUT TO REPORT rpt_tabrdeta2(g_reg1.*)
       END FOREACH
    
       FINISH REPORT rpt_tabrdeta2
   END CASE

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   --LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   --RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabrdeta2(g_reg1)
   DEFINE g_reg1       RECORD
     nss               CHAR(11),
     tipo_solicitud    SMALLINT,
     desc_tipo_sol     CHAR(30),
     tipo_trabajador   CHAR(02),
     desc_tipo_trab    CHAR(20),
     id_cuenta         CHAR(02),
     desc_id_cuenta    CHAR(40),
     tipo_movimiento   SMALLINT,
     desc_tipo_mov     CHAR(80)
   END RECORD

   DEFINE 
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      FIRST PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 

         SELECT a.codigo_afore, a.razon_social
           INTO codigo_afore, razon_social
           FROM tab_afore_local a

         PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                          razon_social CLIPPED
         PRINT
         PRINT COLUMN 001,"MODULO: ", g_param_dis.modulo_desc CLIPPED
         PRINT
         PRINT COLUMN 001,"PROGRAMA: CTAL122 ",
               COLUMN 065,"LISTADO DE CONSULTA DETALLE CUENTAS SALDO CERO",
			  " POR TIPO MOVIMIENTO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

	 PRINT COLUMN 001,"FECHA CORTE: ", vfecha_corte2 USING "DD/MM/YYYY"
         SKIP 1 LINE

	 PRINT COLUMN 001,"NSS|TIPO SOLICITUD|TIPO TRABAJADOR|ID REGISTRO|",
			  "TIPO MOVIMIENTO|"
         SKIP 1 LINE

      ON EVERY ROW
	 PRINT COLUMN 001, g_reg1.nss CLIPPED, "|",
			   g_reg1.tipo_solicitud USING "##", " ",
			   g_reg1.desc_tipo_sol CLIPPED, "|",
			   g_reg1.tipo_trabajador CLIPPED, " ",
			   g_reg1.desc_tipo_trab CLIPPED, "|", 
			   g_reg1.id_cuenta CLIPPED, " ",
			   g_reg1.desc_id_cuenta CLIPPED, "|",
			   g_reg1.tipo_movimiento USING "##&", " ",
			   g_reg1.desc_tipo_mov CLIPPED, "|"
      ON LAST ROW
         PRINT
	 PRINT COLUMN 001, "Total Sum Id Cuenta y Tipo Movimiento: ",
         total_arh USING "###,###,##&"

END REPORT

#####################################################################
REPORT rpt_tabrdeta(g_reg)

   DEFINE g_reg        RECORD
     tipo_solicitud    SMALLINT,
     desc_tipo_sol     CHAR(30),
     tipo_trabajador   CHAR(02),
     desc_tipo_trab    CHAR(20),
     id_cuenta         CHAR(02),
     desc_id_cuenta    CHAR(40),
     tipo_movimiento   SMALLINT,
     desc_tipo_mov     CHAR(80),
     total             INTEGER
   END RECORD

   DEFINE
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

         SELECT a.codigo_afore, a.razon_social
           INTO codigo_afore, razon_social
           FROM tab_afore_local a

         PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                          razon_social CLIPPED
         PRINT
         PRINT COLUMN 001,"MODULO: ", g_param_dis.modulo_desc CLIPPED
         PRINT
         PRINT COLUMN 001,"PROGRAMA: CTAL122 ",
               COLUMN 065,"LISTADO DE CONSULTA CIFRAS CUENTAS SALDO CERO",
			  " POR TIPO MOVIMIENTO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 001,"FECHA CORTE: ", vfecha_corte2 USING "DD/MM/YYYY"
	 PRINT

         PRINT COLUMN 001,"TIPO SOLICITUD",
               COLUMN 036,"TIPO TRABAJADOR",
               COLUMN 052,"ID REGISTRO",
	       COLUMN 089,"TIPO MOVIMIENTO"

      ON EVERY ROW

         PRINT COLUMN 001, g_reg.tipo_solicitud USING "##",
	       COLUMN 004, g_reg.desc_tipo_sol CLIPPED,
	       COLUMN 036, g_reg.tipo_trabajador CLIPPED,
	       COLUMN 039, g_reg.desc_tipo_trab CLIPPED,
	       COLUMN 052, g_reg.id_cuenta CLIPPED,
	       COLUMN 055, g_reg.desc_id_cuenta CLIPPED,
	       COLUMN 089, g_reg.tipo_movimiento USING "##&",
	       COLUMN 093, g_reg.desc_tipo_mov CLIPPED
         PRINT COLUMN 001, "Total por Id Cuenta y Tipo Movimiento: ", 
	 g_reg.total USING "###,###,##&"
         PRINT

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 060," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         PRINT
         PRINT COLUMN 001, "Total Sum Id Cuenta y Tipo Movimiento: ",
         total_arh USING "###,###,##&"

         SKIP 2 LINE
         PRINT COLUMN 001,"Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT

#####################################################################
