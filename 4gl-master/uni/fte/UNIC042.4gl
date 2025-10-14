###############################################################################
#Proyecto          => Sistema de Afores.(MEXICO)                              #
#Propietario       => E.F.P.                                                  #
#Programa  UNIC042 => CONSULTA DE CUENTAS APERTURADAS                         #
#Sistema           => UNI                                                     #
#Autor             => Omar Sandoval Badillo                                   #
#Fecha             => 08 de abril de 2005.                                    #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param         RECORD LIKE seg_modulo.*

   DEFINE aux_pausa      CHAR(1),
          pos            SMALLINT,
          total          SMALLINT,
          sw_1           SMALLINT,
          sw             SMALLINT,
          hora           CHAR(8),
          hoy            DATE,
          usuario        CHAR(8),
          sel_where      CHAR(300),
          sel_where1     CHAR(800),
          cla_where      CHAR(300),
          opc            CHAR(1),
          g_impre        CHAR(300),
          g_lista        CHAR(300)

   DEFINE   gr_cuentas    RECORD
      folio            INTEGER,
      tipo_nss         CHAR(15),
      tipo_operacion   SMALLINT,
      n_seguro         CHAR(11),
      n_folio          INTEGER,
      nombres          CHAR(35),
      tipo_solicitud   SMALLINT
   END RECORD

   DEFINE l_record1  ARRAY[8000] OF RECORD
      folio            INTEGER,
      tipo_nss         CHAR(15),
      tipo_operacion   SMALLINT,
      n_seguro         CHAR(11),
      n_folio          INTEGER,
      tipo_solicitud   SMALLINT
   END RECORD

   DEFINE l_record2  ARRAY[8000] OF RECORD
      folio            INTEGER,
      tipo_nss         CHAR(15),
      tipo_operacion   SMALLINT,
      n_seguro         CHAR(11),
      n_folio          INTEGER,
      nombres          CHAR(35),
      tipo_solicitud   SMALLINT
   END RECORD

   DEFINE nombre_completo     CHAR(40),
          vrazon_social       CHAR(50),
          vcodigo_afore       CHAR(03)
   
END GLOBALS
######################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT
   CALL STARTLOG("UNIC042.log")

   LET hoy = TODAY

   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   SELECT codigo_afore,
          razon_social
   INTO   vcodigo_afore,
          vrazon_social
   FROM   tab_afore_local

   LET sel_where1 = " SELECT NVL(TRIM(a.paterno),' ')||' '",
                          "||NVL(TRIM(a.materno),' ')||' '",
                          "||NVL(TRIM(a.nombres),' ') nombre",
                    " FROM   afi_mae_afiliado a,uni_apertura_cuenta b",
                    " WHERE  a.n_seguro = ? ",
                    " AND    a.n_folio  = b.n_folio ",
                    " AND    a.tipo_solicitud = b.tipo_solicitud ",
                    " UNION ",
                    " SELECT NVL(TRIM(a.paterno),' ')||' '",
                          "||NVL(TRIM(a.materno),' ')||' '",
                          "||NVL(TRIM(a.nombres),' ') nombre",
                    " FROM   afi_solicitud a,uni_apertura_cuenta b",
                    " WHERE  a.n_seguro = ? ",
                    " AND    a.n_folio  = b.n_folio ",
                    " AND    a.tipo_solicitud = b.tipo_solicitud "


   PREPARE query_nom FROM sel_where1

   CALL principal()

END MAIN
######################################################################
FUNCTION principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0421" ATTRIBUTE(BORDER)
   DISPLAY " UNIC042                CONSULTA DE CUENTAS APERTURADAS                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD/MM/YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   MENU "CUENTAS"
      COMMAND "Consulta" "Consulta los NSS de las cuentas aperturadas"
         CALL consulta()
      COMMAND "Salir" "Sale del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
######################################################################
FUNCTION consulta()

   OPEN WINDOW ventana_2 AT 2,2 WITH FORM "UNIC0421" ATTRIBUTE(BORDER)
   DISPLAY " UNIC042              CONSULTA DE CUENTAS APERTURADAS                CONSULTA  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " (Ctrl-p) Imprimir                                             (Ctrl-C) Salir " AT 2,1

   LET total = 0
   LET int_flag = FALSE

   CONSTRUCT cla_where ON a.folio,
                          a.n_seguro,
                          a.tipo_procesos
                     FROM x_folio,
                          x_nss,
                          x_tipo_proceso

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

   LET sel_where = " SELECT a.* ",
                   " FROM   uni_apertura_cuenta a ",
                   " WHERE  ", cla_where CLIPPED,
                   " GROUP BY 1,2,3,4,5,6 "

   PREPARE query FROM sel_where
   DECLARE cursor_1 CURSOR FOR query

   LET pos = 1

   FOREACH cursor_1 INTO l_record1[pos].*

      LET l_record2[pos].folio          = l_record1[pos].folio
      LET l_record2[pos].tipo_nss       = l_record1[pos].tipo_nss
      LET l_record2[pos].tipo_operacion = l_record1[pos].tipo_operacion
      LET l_record2[pos].n_seguro       = l_record1[pos].n_seguro
      LET l_record2[pos].n_folio        = l_record1[pos].n_folio
      LET l_record2[pos].tipo_solicitud = l_record1[pos].tipo_solicitud

      DECLARE cur_nom CURSOR FOR query_nom

      OPEN  cur_nom USING l_record1[pos].n_seguro,
                          l_record1[pos].n_seguro
      FETCH cur_nom INTO nombre_completo
      CLOSE cur_nom

      LET l_record2[pos].nombres = nombre_completo CLIPPED

      LET pos = pos + 1
   END FOREACH

   LET total = pos - 1

   DISPLAY BY NAME total
   DISPLAY " Folio   Tipo    Tipo    N.S.S.    Folio              Nombre             Tipo  " AT 7,1 ATTRIBUTE(REVERSE)
   DISPLAY "        N.S.S.   Proc.             Afil.                                 Sol.   " AT 8,1 ATTRIBUTE(REVERSE)

   DISPLAY " Total de Registros " AT 20,1

   INITIALIZE l_record2[pos].* TO NULL

   LET pos = pos - 1

   IF (pos) >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record2 TO scr_1.*

         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            CALL busca_datos(l_record2[pos].n_seguro,
                             l_record2[pos].n_folio,
                             l_record2[pos].tipo_solicitud)
         ON KEY (CONTROL-P)
            CALL impresion(pos)
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
                 usuario CLIPPED,".IMPUNIAPER",
                 hoy USING "DD-MM-YYYY" CLIPPED

   START REPORT rpt_cuentas TO g_impre

   FOR i = 1 TO (pos+1)

      LET gr_cuentas.folio           = l_record2[i].folio
      LET gr_cuentas.tipo_nss        = l_record2[i].tipo_nss
      LET gr_cuentas.tipo_operacion  = l_record2[i].tipo_operacion
      LET gr_cuentas.n_seguro        = l_record2[i].n_seguro
      LET gr_cuentas.n_folio         = l_record2[i].n_folio
      LET gr_cuentas.nombres         = l_record2[i].nombres
      LET gr_cuentas.tipo_solicitud  = l_record2[i].tipo_solicitud

      IF gr_cuentas.n_seguro IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_cuentas(gr_cuentas.*)
   END FOR

   FINISH REPORT rpt_cuentas

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista

END FUNCTION
##############################################################################
REPORT rpt_cuentas(gr_cuentas)

   DEFINE gr_cuentas RECORD
      folio            INTEGER,
      tipo_nss         CHAR(15),
      tipo_operacion   SMALLINT,
      n_seguro         CHAR(11),
      n_folio          INTEGER,
      nombres          CHAR(35),
      tipo_solicitud   SMALLINT
   END RECORD

   DEFINE L1               CHAR(01),
          L2               CHAR(02),
          L3               CHAR(03),
          L4               CHAR(04),
          L5               CHAR(05),
          L6               CHAR(06),
          L7               CHAR(07),
          L8               CHAR(08),
          L9               CHAR(09),
          L10              CHAR(10)

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   80

   FORMAT
      PAGE HEADER

      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 01,'\033e\033(s218T\033(s8H\033(s3B'
      PRINT COLUMN 29,"REPORTE DE CUENTAS APERTURADAS"
            SKIP 3 LINE
      PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B'
      PRINT COLUMN 03,vrazon_social
            SKIP 1 LINE
      PRINT COLUMN 02," UNIC042 ",
            COLUMN 104, TODAY USING "DD-MM-YYYY"
            SKIP 4 LINE

      PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B','\033015'
      PRINT COLUMN 02,L2,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'
      #SKIP 1 LINE

      PRINT COLUMN 05,"FOLIO",
            COLUMN 13,"TIPO NSS",
            COLUMN 27,"TIPO OPERACION",
            COLUMN 44,"N.S.S.",
            COLUMN 56,"FOLIO AFIL.",
            COLUMN 78,"NOMBRE",
            COLUMN 96,"TIPO SOLICITUD"

      #SKIP 1 LINE
      PRINT COLUMN 02,L2,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

   ON EVERY ROW
      SKIP 2 LINE
      PRINT COLUMN 01,'\033e\033(s218T\033(s12H\033(s7B'
      PRINT COLUMN 01,gr_cuentas.folio,
            COLUMN 14,gr_cuentas.tipo_nss,
            COLUMN 32,gr_cuentas.tipo_operacion,
            COLUMN 46,gr_cuentas.n_seguro,
            COLUMN 56,gr_cuentas.n_folio,
            COLUMN 74,gr_cuentas.nombres CLIPPED,
            COLUMN 104,gr_cuentas.tipo_solicitud

   ON LAST ROW
      SKIP 2 LINE
      PRINT COLUMN 02,L1,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      SKIP 5 LINE
      PRINT COLUMN 03,"Total de Registros : ",COUNT(*) USING "<<<<"

   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 105," Pagina : ",PAGENO USING"<<<<<"

END REPORT

