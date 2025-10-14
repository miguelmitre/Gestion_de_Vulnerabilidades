#############################################################################
#Proyecto          => AFORE(MEXICO)                                         #
#Propietario       => E.F.P                                                 #
#Programa UNIC045  => CONSULTA DE CUENTAS COMPLEMENTARIAS DE UNIFICACION    #
#Sistema           => UNI                                                   #
#Autor             => OMAR SANDOVAL BADILLO                                 #
#Fecha             => 26 DE DICIEMBRE 2005                                  #
#############################################################################
DATABASE safre_af
GLOBALS

   DEFINE seg_modulo  RECORD LIKE seg_modulo.*

   DEFINE opc                   CHAR(01),
          g_usuario             CHAR(08),
          HOY                   DATE,
          g_afore               RECORD LIKE tab_afore_local.*,
          g_impre               CHAR(300),
          g_lista               CHAR(100)

   DEFINE greg_01 ARRAY[5000] OF RECORD
          nss_uni            CHAR(11),
          total_traspasado   DECIMAL(16,6)
   END RECORD

   DEFINE greg_02 ARRAY[5000] OF RECORD
          nss_cta1           CHAR(11),
          total_traspasar    DECIMAL(16,6)
   END RECORD

   DEFINE greg_03 ARRAY[5000] OF RECORD
          subcuenta          SMALLINT,
          siefore            SMALLINT,
          tipo_movimiento    SMALLINT,
          monto_acciones     DECIMAL(18,6),
          monto_pesos        DECIMAL(18,6),
          fecha_liquida      DATE
   END RECORD

   DEFINE vfolio_liquida   INTEGER,
          vtipo            SMALLINT,
          vnss             CHAR(11)

   DEFINE sql_txt1           ,
          sql_txt2           ,
          sql_txt3           ,
          sql_txt4           ,
          sql_txt5           CHAR(500)

   DEFINE l_reg_sum ARRAY[20] OF RECORD
         monto_acc    DECIMAL(16,6),
         monto_pes    DECIMAL(16,6)
   END RECORD

END GLOBALS
#############################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      COMMENT LINE LAST,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   CALL STARTLOG("UNIC045.log")

   CALL inicio()
   CALL proceso()

END MAIN
#############################################################################
FUNCTION inicio()

   LET HOY = TODAY

   SELECT *,
          USER
   INTO   g_afore.*,
          g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

END FUNCTION
#############################################################################
FUNCTION proceso()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0451" ATTRIBUTE(BORDER)
   DISPLAY " UNIC045  LIQUIDACIONES NORMALES Y COMPLEMENTARIAS POR UNIFICACION             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " <Esc> Consulta " AT 4,1

   MENU "CONSULTA"
      COMMAND "Consulta de Liquidaciones"
              "Consulta de Liquidaciones Normales y Complementarias"
        CALL complementarias()
      COMMAND "Salir" "Salir de Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1

END FUNCTION
#############################################################################
FUNCTION complementarias()

   DEFINE x_busca   CHAR(200)

   DEFINE sql_com   CHAR(700)
   DEFINE i         SMALLINT

   DEFINE sql_stat     INTEGER,
          item_row_cnt SMALLINT,
          row_cnt      SMALLINT,
          cont_inp     SMALLINT,
          cur_row      INTEGER,
          scr_row      SMALLINT

   DEFINE sql_stat02     INTEGER,
          item_row_cnt02 SMALLINT

   INPUT BY NAME vfolio_liquida,
                 vtipo,
                 vnss

      AFTER FIELD vfolio_liquida
         IF vfolio_liquida IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfolio_liquida
         END IF

      AFTER FIELD vtipo
         IF vtipo IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo
         END IF

         IF vfolio_liquida IS NULL AND vtipo IS NULL THEN
            ERROR "DEBE INGRESAR DATOS EN EL CAMPO FOLIO LIQUIDA Y TIPO "
            NEXT FIELD vfolio_liquida
         END IF

      ON KEY(ESC)
         LET INT_FLAG = FALSE

         IF vfolio_liquida IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vfolio_liquida
         END IF

         IF vtipo IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD vtipo
         END IF

         IF vfolio_liquida IS NULL AND vtipo IS NULL THEN
            ERROR "DEBE INGRESAR DATOS EN EL CAMPO FOLIO LIQUIDA Y TIPO "
            NEXT FIELD vfolio_liquida
         END IF
         EXIT INPUT

      ON KEY(CONTROL-C)
         LET INT_FLAG = TRUE
         CLEAR FORM
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO "
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ..."

   IF vtipo = 1 THEN
      IF vnss IS NULL OR vnss = " " THEN
         LET sql_com = " SELECT a.nss,SUM(a.monto_en_pesos)",
                       " FROM   dis_cuenta a,uni_unificado b ",
                       " WHERE  a.folio = ",vfolio_liquida,
                       " AND    a.tipo_movimiento IN(1,4)",
                       " AND    a.folio = b.folio_liquida",
                       #" AND    a.nss   = b.nss_uni",
                       " AND    a.nss NOT IN(SELECT nss_uni ",
                                " FROM   uni_complementario",
                                " WHERE  folio_liquida = ",vfolio_liquida,")",
                       " GROUP BY 1 ",
                       " ORDER BY 1 "
      ELSE
         LET sql_com = " SELECT a.nss,SUM(a.monto_en_pesos)",
                       " FROM   dis_cuenta a,uni_unificado b ",
                       " WHERE  a.folio = ",vfolio_liquida,
                       " AND    a.tipo_movimiento IN(1,4)",
                       " AND    a.folio = b.folio_liquida",
                       #" AND    a.nss   = b.nss_uni",
                       " AND    a.nss   = ",vnss,
                       " AND    a.nss NOT IN(SELECT nss_uni ",
                                    " FROM   uni_complementario",
                                    " WHERE  folio_liquida = ",vfolio_liquida,
                                    " AND    nss_uni = ",vnss,")",
                       " GROUP BY 1 ",
                       " ORDER BY 1 "
      END IF
   ELSE
      IF vnss IS NULL OR vnss = " " THEN
         LET sql_com = " SELECT a.nss,SUM(a.monto_en_pesos)",
                       " FROM   dis_cuenta a,uni_unificado b ",
                       " WHERE  a.folio = ",vfolio_liquida,
                       " AND    a.tipo_movimiento IN(1,4)",
                       " AND    a.folio = b.folio_liquida",
                       #" AND    a.nss   = b.nss_uni",
                       " AND    a.nss IN(SELECT nss_uni ",
                               " FROM   uni_complementario",
                               " WHERE  folio_liquida = ",vfolio_liquida,")",
                       " GROUP BY 1 ",
                       " ORDER BY 1 "
      ELSE
         LET sql_com = " SELECT a.nss,SUM(a.monto_en_pesos)",
                       " FROM   dis_cuenta a,uni_unificado b ",
                       " WHERE  a.folio = ",vfolio_liquida,
                       " AND    a.tipo_movimiento IN(1,4)",
                       " AND    a.folio = b.folio_liquida",
                       #" AND    a.nss   = b.nss_uni",
                       " AND    a.nss   = ",vnss,
                       " AND    a.nss IN(SELECT nss_uni ",
                                 " FROM   uni_complementario",
                                 " WHERE  folio_liquida = ",vfolio_liquida,
                                 " AND    nss_uni = ",vnss,")",
                       " GROUP BY 1 ",
                       " ORDER BY 1 "
      END IF
   END IF

   PREPARE query01 FROM sql_com
   DECLARE cur01 CURSOR FOR query01

   LET i = 1

   FOREACH cur01 INTO greg_01[i].*
      LET i = i + 1
   END FOREACH
   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      LET cont_inp = TRUE

      DISPLAY "    UNIFICADOR        TOTAL ABONO           UNIFICADO        TOTAL CARGO       " AT 7,1 ATTRIBUTE(REVERSE)
      DISPLAY "    SUB   SIE   TIPO MOV.   MONTO ACCIONES      MONTO PESOS   FECHA LIQUIDA    " AT 14,1 ATTRIBUTE(REVERSE)
      DISPLAY " <Ctrl-V> Cambia ventana             <Ctrl-C> Salir " AT 4,25
      #DISPLAY " <Ctrl-G>Detalle  <Ctrl-T>Imp.Total  <Ctrl-P>Imp. Pantalla  <Ctrl-F>Archivo" AT 13,1
      DISPLAY " <Ctrl-G>Detalle              <Ctrl-T>Imp.Total             <Ctrl-F>Archivo" AT 13,1

      WHILE (cont_inp = TRUE)
         INPUT ARRAY greg_01 WITHOUT DEFAULTS FROM scr_1.*
         ATTRIBUTES(MAXCOUNT=i,COUNT=i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY greg_01[cur_row].* TO scr_1[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY greg_01[cur_row].* TO scr_1[scr_row].*
                  ATTRIBUTE(REVERSE)

                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt

                  CALL proc_items02(cur_row,0)
                       RETURNING sql_stat02,
                                 item_row_cnt02

            END IF
{
            ON KEY(CONTROL-P)
               ERROR "PROCESANDO IMPRESION ..."
               #CALL genera_impresion(i,0)
               CALL genera_impresion(cur_row,0)
}
            ON KEY(CONTROL-T)
               ERROR "PROCESANDO IMPRESION ..."
               CALL genera_impresion(i,2)

            ON KEY(CONTROL-F)
               ERROR "PROCESANDO ARCHIVO ..."
               CALL genera_archivo(i,2)

            ON KEY(CONTROL-V)
               CALL disp_array_items(item_row_cnt)

            ON KEY(CONTROL-G)
               LET cur_row = ARR_CURR()
               #CALL disp_array_items02(cur_row)
               CALL disp_array_items02(item_row_cnt02)

            ON KEY(INTERRUPT)
               DISPLAY "                         " AT 4,50
               DISPLAY "                                                    " AT 4,25
               DISPLAY "                                                                               " AT 13,1
               DISPLAY "                                                                               " AT 7,1
               DISPLAY "                                                                               " AT 14,1
               CALL inicializa()
               CLEAR FORM
               EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      CLEAR FORM
      ERROR "NO EXISTEN REGISTROS ..."
   END IF

END FUNCTION
#####################################################
FUNCTION proc_items(p_cur_row)

   DEFINE sql_status   INTEGER,
          p_cur_row    INTEGER,
          item_row_cnt SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_status,
                 item_row_cnt

   CALL disp_four_items()

   RETURN sql_status,
         item_row_cnt

END FUNCTION
#####################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_status  INTEGER,
          p_cur_row   INTEGER,
          row_cnt     SMALLINT,
          sql_txt     CHAR(250)

   DEFINE vsubcuenta SMALLINT,
          vgrupo     SMALLINT

   LET vsubcuenta = 0
   LET vgrupo     = 0

   CALL null_items()

   LET sql_txt1 = " SELECT nss_cta1",
                  " FROM   uni_unificado",
                  " WHERE  nss_uni = ? ",
                  " AND    folio_liquida = ? ",
                  " AND    estado IN(95,96,100) ",
                  " GROUP BY 1"

   PREPARE query02 FROM sql_txt1
   DECLARE cur02 CURSOR FOR query02

   WHENEVER ERROR CONTINUE
      OPEN cur02 USING greg_01[p_cur_row].nss_uni,
                       vfolio_liquida
   WHENEVER ERROR STOP

   LET sql_status = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_status) AND (row_cnt <= 10))
      WHENEVER ERROR CONTINUE
         FETCH cur02 INTO greg_02[row_cnt].nss_cta1
            SELECT SUM(a.monto_en_pesos)
            INTO   greg_02[row_cnt].total_traspasar
            FROM   dis_cuenta a,uni_unificado b
            WHERE  a.nss   = greg_02[row_cnt].nss_cta1
            AND    a.folio = vfolio_liquida
            --AND    a.nss   = b.nss_cta1
            AND    tipo_movimiento IN(241,242,244,245)

            IF greg_02[row_cnt].total_traspasar IS NULL OR
               greg_02[row_cnt].total_traspasar = " " THEN
                  LET greg_02[row_cnt].nss_cta1 = NULL
                  FETCH NEXT cur02
            END IF

      WHENEVER ERROR STOP
         LET sql_status = SQLCA.SQLCODE
         IF (NOT sql_status) THEN
            LET row_cnt = row_cnt + 1
         END IF
   END WHILE

   IF (sql_status = 100) THEN
      LET sql_status = 0
   END IF

   RETURN sql_status,
          row_cnt - 1
END FUNCTION
#####################################################
FUNCTION disp_four_items()
   DEFINE i SMALLINT

   FOR i = 1 TO 5
      DISPLAY greg_02[i].* TO scr_2[i].*
   END FOR

END FUNCTION
#####################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE greg_02[1].* TO NULL

   FOR i = 1 TO 100
      LET greg_02[i].* = greg_02[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   DEFINE sql_status INTEGER,
          item_row_cnt SMALLINT,
          row_cnt SMALLINT,
          cont_inp SMALLINT,
          cur_row SMALLINT,
          scr_row SMALLINT,
          sw      SMALLINT

   LET sw = 0
   CALL SET_COUNT(p_item_row_cnt)
   LET cont_inp = TRUE

   WHILE (cont_inp = TRUE)
      INPUT ARRAY greg_02 WITHOUT DEFAULTS FROM scr_2.*
      ATTRIBUTES(MAXCOUNT=p_item_row_cnt,COUNT=p_item_row_cnt)

         AFTER ROW
            LET cur_row = ARR_CURR()
            LET scr_row = SCR_LINE()

            DISPLAY greg_02[cur_row].* TO scr_2[scr_row].*

         BEFORE ROW
            LET cur_row = ARR_CURR()
            LET scr_row = SCR_LINE()

            IF (cur_row = p_item_row_cnt + 1) THEN
                LET cont_inp = TRUE
                EXIT INPUT
            ELSE
               LET scr_row = SCR_LINE()

               DISPLAY greg_02[cur_row].* TO scr_2[scr_row].*
               ATTRIBUTE(REVERSE)

               LET cont_inp = FALSE

                  CALL proc_items02(cur_row,1)
                       RETURNING sql_status,
                                 item_row_cnt

            END IF
{
         ON KEY(CONTROL-P)
            ERROR "PROCESANDO IMPRESION ..."
            CALL genera_impresion(p_item_row_cnt,1)
}

         ON KEY(CONTROL-G)
            LET cur_row = ARR_CURR()
            #CALL disp_array_items02(cur_row)
            CALL disp_array_items02(item_row_cnt)

         ON KEY(INTERRUPT)
            CALL proc_items02(cur_row,0)
                 RETURNING sql_status,
                           item_row_cnt
            EXIT INPUT
      END INPUT
   END WHILE
END FUNCTION
###########################################################################
FUNCTION proc_items02(p_cur_row,bandera)

   DEFINE sql_status   INTEGER,
          p_cur_row    SMALLINT,
          item_row_cnt SMALLINT

   DEFINE bandera      SMALLINT

   CALL sel_ord_item02(p_cur_row,bandera)
        RETURNING sql_status,
                 item_row_cnt

   CALL disp_four_items02()

   RETURN sql_status,
         item_row_cnt

END FUNCTION
##################################################################
FUNCTION sel_ord_item02(p_cur_row,bandera)

   DEFINE sql_status  INTEGER,
          p_cur_row   SMALLINT,
          row_cnt     SMALLINT,
          sql_txt     CHAR(250),
          sql_tipo    CHAR(250)

   DEFINE bandera      SMALLINT

   CALL null_items02()

   IF bandera = 0 THEN
      LET sql_tipo = " AND tipo_movimiento IN(1,4) "
   ELSE
      IF bandera = 1 THEN
         LET sql_tipo = " AND tipo_movimiento IN(241,242,244,245) "
      END IF
   END IF

   LET sql_txt =  " SELECT subcuenta,",
                          "siefore,",
                          "tipo_movimiento,",
                          "monto_en_acciones,",
                          "monto_en_pesos,",
                          "fecha_proceso",
                  " FROM   dis_cuenta ",
                  " WHERE  folio = ? ",
                  " AND    nss   = ? ",
                    sql_tipo,
                  " GROUP BY 1,2,3,4,5,6 ",
                  " ORDER BY 1,2,3 "

   PREPARE sel_item_stmt FROM sql_txt
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   IF bandera = 0 then
      WHENEVER ERROR CONTINUE
         OPEN sel_item_curs USING vfolio_liquida,
                                  greg_01[p_cur_row].nss_uni
      WHENEVER ERROR STOP
   ELSE
      IF bandera = 1 then
      WHENEVER ERROR CONTINUE
            OPEN sel_item_curs USING vfolio_liquida,
                                     greg_02[p_cur_row].nss_cta1
      WHENEVER ERROR STOP
      END IF
   END IF

   LET sql_status = SQLCA.SQLCODE
   LET row_cnt = 1

   WHILE ((NOT sql_status) AND (row_cnt<= 10))
      WHENEVER ERROR CONTINUE
         FETCH sel_item_curs INTO greg_03[row_cnt].*
      WHENEVER ERROR STOP
         LET sql_status = SQLCA.SQLCODE
         IF (NOT sql_status) THEN
            LET row_cnt = row_cnt + 1
         END IF
   END WHILE

   IF (sql_status = 100) THEN
      LET sql_status = 0
   END IF

   RETURN sql_status,
          row_cnt - 1
END FUNCTION
#####################################################
FUNCTION disp_four_items02()
   DEFINE i SMALLINT

   FOR i = 1 TO 5
      DISPLAY greg_03[i].* TO scr_3[i].*
   END FOR
END FUNCTION
#####################################################
FUNCTION null_items02()
   DEFINE i SMALLINT

   INITIALIZE greg_03[1].* TO NULL

   FOR i = 2 TO 100
      LET greg_03[i].* = greg_03[1].*
   END FOR
END FUNCTION
#####################################################
FUNCTION disp_array_items02(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY greg_03 TO scr_3.*
      ON KEY(INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
#############################################################
FUNCTION inicializa()

   DEFINE i     SMALLINT

   FOR i = 1 TO 5
      INITIALIZE greg_01[i].* TO NULL
      INITIALIZE greg_02[i].* TO NULL
      INITIALIZE greg_03[i].* TO NULL
      DISPLAY greg_01[i].* TO scr_1[i].*
      DISPLAY greg_02[i].* TO scr_2[i].*
      DISPLAY greg_03[i].* TO scr_3[i].*
   END FOR

END FUNCTION
###########################################################################
FUNCTION genera_impresion(cuantos,bandera1)

   DEFINE cuantos,i   INTEGER

   DEFINE sql_rpt     CHAR(500)

   DEFINE l_reg01   ARRAY[9000] OF RECORD
          nss                  CHAR(11),
          subcuenta            SMALLINT,
          siefore              SMALLINT,
          tipo_movimiento      SMALLINT,
          monto_en_acciones    DECIMAL(16,6),
          monto_en_pesos       DECIMAL(16,6),
          fecha_liquidacion    DATE
   END RECORD

   DEFINE l_reg   ARRAY[9000] OF RECORD
          nss   CHAR(11)
   END RECORD

   DEFINE l_reg2   ARRAY[9000] OF RECORD
          nss1   CHAR(11),
          nss2   CHAR(11)
   END RECORD

   DEFINE tipo_mov       CHAR(100),
          bandera1       CHAR(01)

   IF bandera1 = 0 THEN
      LET tipo_mov = " AND tipo_movimiento IN(1,4) "

      LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                 vfolio_liquida USING "<<<<<<" CLIPPED,"ABONO.LIQ22"
   ELSE
      IF bandera1 = 1 THEN
         LET tipo_mov = " AND tipo_movimiento IN(241,242,244,245) "

         LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                 vfolio_liquida USING "<<<<<<" CLIPPED,"CARGO.LIQ22"
      ELSE
         IF bandera1 = 2 THEN
            LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                    vfolio_liquida USING "<<<<<<" CLIPPED,".LIQ22"
         END IF
      END IF
   END IF

   IF bandera1 = 0 OR bandera1 = 1 THEN
      LET sql_rpt = " SELECT  nss,",
                            " subcuenta,",
                            " siefore,",
                            " tipo_movimiento,",
                            " monto_en_acciones,",
                            " monto_en_pesos,",
                            " fecha_proceso",
                    " FROM    dis_cuenta",
                    " WHERE   nss   = ? ",
                    " AND     folio = ? ",
                    " AND     subcuenta <> 0",tipo_mov CLIPPED,
                    " GROUP BY 1,2,3,4,5,6,7",
                    " ORDER BY 1,2"
   ELSE
      IF bandera1 = 2 THEN
         LET sql_rpt = " SELECT  nss,",
                               " subcuenta,",
                               " siefore,",
                               " tipo_movimiento,",
                               " monto_en_acciones,",
                               " monto_en_pesos,",
                               " fecha_proceso",
                       " FROM    dis_cuenta",
                       " WHERE   nss   = ? ",
                       " AND     folio = ? ",
                       " AND     subcuenta <> 0",
                       " AND     tipo_movimiento IN(1,4)",
                       " GROUP BY 1,2,3,4,5,6,7",
                       " UNION ",
                       " SELECT  nss,",
                               " subcuenta,",
                               " siefore,",
                               " tipo_movimiento,",
                               " monto_en_acciones,",
                               " monto_en_pesos,",
                               " fecha_proceso",
                       " FROM    dis_cuenta",
                       " WHERE   nss   = ? ",
                       " AND     folio = ? ",
                       " AND     subcuenta <> 0",
                       " AND     tipo_movimiento IN(241,242,244,245)",
                       " GROUP BY 1,2,3,4,5,6,7",
                       " ORDER BY 1,2"
      END IF
   END IF

   PREPARE query_rpt FROM sql_rpt
   DECLARE cur_rpt CURSOR FOR query_rpt

   FOR i = 1 TO 20
       LET l_reg_sum[i].monto_acc = 0
       LET l_reg_sum[i].monto_pes = 0
   END FOR

   START REPORT rpt_complementarias_imp TO g_impre
      FOR i = 1 TO cuantos
         IF bandera1 = 0 THEN
            LET l_reg[i].nss = greg_01[i].nss_uni
         ELSE
            IF bandera1 = 1 THEN
               LET l_reg[i].nss = greg_02[i].nss_cta1
            ELSE
               IF bandera1 = 2 THEN
                  LET l_reg2[i].nss1 = greg_01[i].nss_uni

                  DECLARE cur_ban CURSOR FOR
                  SELECT nss_cta1
                  FROM   uni_unificado
                  WHERE  nss_uni = l_reg2[i].nss1

                  FOREACH cur_ban INTO l_reg2[i].nss2
                  END FOREACH
               END IF
            END IF
         END IF

         IF bandera1 = 0 OR bandera1 = 1 THEN
            FOREACH cur_rpt USING l_reg[i].nss,
                                  vfolio_liquida
                             INTO l_reg01[i].*

               IF l_reg01[i].nss IS NULL THEN
                  EXIT FOREACH
               END IF

               OUTPUT TO REPORT rpt_complementarias_imp(l_reg01[i].*)
            END FOREACH
         ELSE
            FOREACH cur_rpt USING l_reg2[i].nss1,
                                  vfolio_liquida,
                                  l_reg2[i].nss2,
                                  vfolio_liquida
                             INTO l_reg01[i].*

               IF l_reg01[i].nss IS NULL THEN
                  EXIT FOREACH
               END IF

               OUTPUT TO REPORT rpt_complementarias_imp(l_reg01[i].*)
            END FOREACH
         END IF
      END FOR

   FINISH REPORT rpt_complementarias_imp

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista

END FUNCTION
###########################################################################
REPORT rpt_complementarias_imp(l_reg01)

   DEFINE l_reg01 RECORD
          nss                  CHAR(11),
          subcuenta            SMALLINT,
          siefore              SMALLINT,
          tipo_movimiento      SMALLINT,
          monto_en_acciones    DECIMAL(16,6),
          monto_en_pesos       DECIMAL(16,6),
          fecha_liquidacion    DATE
   END RECORD

   DEFINE i,vsub      SMALLINT

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
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90

   FORMAT
      PAGE HEADER

      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 01,'\033e\033(s218T\033(s10H\033(s7B'

      PRINT COLUMN 02," UNIC045 ",
            COLUMN 23,"                 REPORTE DE LIQUIDACIONES             ",
            COLUMN 96,TODAY USING "DD-MM-YYYY"
      SKIP 3 LINE

      #PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'  --letra pequeñ     PRINT COLUMN 01,'\033e\033(s218T\033(s11H\033(s7B','\033015'
      PRINT COLUMN 01,L3,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      PRINT COLUMN 05,"NSS",
            COLUMN 13,"SUBCUENTA",
            COLUMN 24,"SIEFORE",
            COLUMN 34,"TIPO MOVIMIENTO",
            COLUMN 52,"MONTO EN ACCIONES",
            COLUMN 78,"MONTO EN PESOS",
            COLUMN 96,"FECHA LIQUIDACION"

      PRINT COLUMN 01,L3,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'

   BEFORE GROUP OF l_reg01.nss
--      SKIP 1 LINE
      PRINT COLUMN 02,l_reg01.nss

   ON EVERY ROW
      LET vsub = l_reg01.subcuenta

      PRINT COLUMN 17,l_reg01.subcuenta,
            COLUMN 29,l_reg01.siefore,
            COLUMN 46,l_reg01.tipo_movimiento,
            COLUMN 66,l_reg01.monto_en_acciones,
            COLUMN 95,l_reg01.monto_en_pesos,
            COLUMN 129,l_reg01.fecha_liquidacion

      IF l_reg01.monto_en_acciones > 0 AND
         l_reg01.monto_en_pesos    > 0 THEN
            LET l_reg_sum[vsub].monto_acc = l_reg_sum[vsub].monto_acc +
                                            l_reg01.monto_en_acciones
            LET l_reg_sum[vsub].monto_pes = l_reg_sum[vsub].monto_pes +
                                            l_reg01.monto_en_pesos
      END IF

   AFTER GROUP OF l_reg01.nss
      IF l_reg01.tipo_movimiento = 241 OR
         l_reg01.tipo_movimiento = 242 OR
         l_reg01.tipo_movimiento = 244 OR
         l_reg01.tipo_movimiento = 245 THEN
      PRINT COLUMN 01,L10,L10,L10,L5,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10
,'\033015'
      END IF

   ON LAST ROW
      SKIP 2 LINE
      PRINT COLUMN 2,"TOTALES"

      FOR i = 1 TO 20
         IF l_reg_sum[i].monto_acc > 0 AND
            l_reg_sum[i].monto_pes > 0 THEN
               PRINT COLUMN 17,i,
                     COLUMN 66,l_reg_sum[i].monto_acc,
                     COLUMN 95,l_reg_sum[i].monto_pes
         END IF
      END FOR

   PAGE TRAILER
      SKIP 3 LINE
      PRINT COLUMN 01," Pagina : ",PAGENO USING"<<<<<"
      SKIP 2 LINE

END REPORT
###########################################################################
FUNCTION genera_archivo(cuantos,bandera1)

   DEFINE cuantos,i   INTEGER

   DEFINE sql_rpt     CHAR(500)

   DEFINE l_reg01   ARRAY[9000] OF RECORD
          nss                  CHAR(11),
          subcuenta            SMALLINT,
          siefore              SMALLINT,
          tipo_movimiento      SMALLINT,
          monto_en_acciones    DECIMAL(16,6),
          monto_en_pesos       DECIMAL(16,6),
          fecha_liquidacion    DATE
   END RECORD

   DEFINE l_reg   ARRAY[9000] OF RECORD
          nss   CHAR(11)
   END RECORD

   DEFINE l_reg2   ARRAY[9000] OF RECORD
          nss1   CHAR(11),
          nss2   CHAR(11)
   END RECORD

   DEFINE tipo_mov       CHAR(100),
          bandera1       CHAR(01)

   IF bandera1 = 0 THEN
      LET tipo_mov = " AND tipo_movimiento IN(1,4) "

      LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                 vfolio_liquida USING "<<<<<<" CLIPPED,"ABONO.LIQ22"
   ELSE
      IF bandera1 = 1 THEN
         LET tipo_mov = " AND tipo_movimiento IN(241,242,244,245) "

         LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                 vfolio_liquida USING "<<<<<<" CLIPPED,"CARGO.LIQ22"
      ELSE
         IF bandera1 = 2 THEN
            LET g_impre = seg_modulo.ruta_listados CLIPPED,"/",
                    vfolio_liquida USING "<<<<<<" CLIPPED,".LIQ22"
         END IF
      END IF
   END IF

   IF bandera1 = 0 OR bandera1 = 1 THEN
      LET sql_rpt = " SELECT  nss,",
                            " subcuenta,",
                            " siefore,",
                            " tipo_movimiento,",
                            " monto_en_acciones,",
                            " monto_en_pesos,",
                            " fecha_proceso",
                    " FROM    dis_cuenta",
                    " WHERE   nss   = ? ",
                    " AND     folio = ? ",
                    " AND     subcuenta <> 0",tipo_mov CLIPPED,
                    " GROUP BY 1,2,3,4,5,6,7",
                    " ORDER BY 1,2"
   ELSE
      IF bandera1 = 2 THEN
         LET sql_rpt = " SELECT  nss,",
                               " subcuenta,",
                               " siefore,",
                               " tipo_movimiento,",
                               " monto_en_acciones,",
                               " monto_en_pesos,",
                               " fecha_proceso",
                       " FROM    dis_cuenta",
                       " WHERE   nss   = ? ",
                       " AND     folio = ? ",
                       " AND     subcuenta <> 0",
                       " AND     tipo_movimiento IN(1,4)",
                       " GROUP BY 1,2,3,4,5,6,7",
                       " UNION ",
                       " SELECT  nss,",
                               " subcuenta,",
                               " siefore,",
                               " tipo_movimiento,",
                               " monto_en_acciones,",
                               " monto_en_pesos,",
                               " fecha_proceso",
                       " FROM    dis_cuenta",
                       " WHERE   nss   = ? ",
                       " AND     folio = ? ",
                       " AND     subcuenta <> 0",
                       " AND     tipo_movimiento IN(241,242,244,245)",
                       " GROUP BY 1,2,3,4,5,6,7",
                       " ORDER BY 1,2"
      END IF
   END IF

   PREPARE query_arc FROM sql_rpt
   DECLARE cur_arc CURSOR FOR query_arc

   FOR i = 1 TO 20
       LET l_reg_sum[i].monto_acc = 0
       LET l_reg_sum[i].monto_pes = 0
   END FOR

   START REPORT rpt_complementarias_arc TO g_impre
      FOR i = 1 TO cuantos
         IF bandera1 = 0 THEN
            LET l_reg[i].nss = greg_01[i].nss_uni
         ELSE
            IF bandera1 = 1 THEN
               LET l_reg[i].nss = greg_02[i].nss_cta1
            ELSE
               IF bandera1 = 2 THEN
                  LET l_reg2[i].nss1 = greg_01[i].nss_uni

                  DECLARE cur_ban1 CURSOR FOR
                  SELECT nss_cta1
                  FROM   uni_unificado
                  WHERE  nss_uni = l_reg2[i].nss1

                  FOREACH cur_ban1 INTO l_reg2[i].nss2
                  END FOREACH
               END IF
            END IF
         END IF

         IF bandera1 = 0 OR bandera1 = 1 THEN
            FOREACH cur_arc USING l_reg[i].nss,
                                  vfolio_liquida
                             INTO l_reg01[i].*

               IF l_reg01[i].nss IS NULL THEN
                  EXIT FOREACH
               END IF

               OUTPUT TO REPORT rpt_complementarias_arc(l_reg01[i].*)
            END FOREACH
         ELSE
            FOREACH cur_arc USING l_reg2[i].nss1,
                                  vfolio_liquida,
                                  l_reg2[i].nss2,
                                  vfolio_liquida
                             INTO l_reg01[i].*

               IF l_reg01[i].nss IS NULL THEN
                  EXIT FOREACH
               END IF

               OUTPUT TO REPORT rpt_complementarias_arc(l_reg01[i].*)
            END FOREACH
         END IF
      END FOR

   FINISH REPORT rpt_complementarias_arc

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   RUN g_lista

END FUNCTION
###########################################################################
REPORT rpt_complementarias_arc(l_reg01)

   DEFINE l_reg01 RECORD
          nss                  CHAR(11),
          subcuenta            SMALLINT,
          siefore              SMALLINT,
          tipo_movimiento      SMALLINT,
          monto_en_acciones    DECIMAL(16,6),
          monto_en_pesos       DECIMAL(16,6),
          fecha_liquidacion    DATE
   END RECORD

   DEFINE i,vsub      SMALLINT
   DEFINE indicador   CHAR(02)

   OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         LET vsub = l_reg01.subcuenta

         IF l_reg01.tipo_movimiento = 1 OR
            l_reg01.tipo_movimiento = 4 THEN
               LET indicador = "02"
         ELSE
            LET indicador = "03"
         END IF

         PRINT indicador,
               l_reg01.nss,
               l_reg01.subcuenta         USING "&&",
               l_reg01.siefore           USING "&&",
               l_reg01.tipo_movimiento   USING "&&&",
               l_reg01.monto_en_acciones USING "&&&&&&&&&&&&&&.&&",
               l_reg01.monto_en_pesos    USING "&&&&&&&&&&&&&&.&&",
               l_reg01.fecha_liquidacion USING "DD/MM/YYYY"

         IF l_reg01.monto_en_acciones > 0 AND
            l_reg01.monto_en_pesos    > 0 THEN
               LET l_reg_sum[vsub].monto_acc = l_reg_sum[vsub].monto_acc +
                                               l_reg01.monto_en_acciones
               LET l_reg_sum[vsub].monto_pes = l_reg_sum[vsub].monto_pes +
                                               l_reg01.monto_en_pesos
         END IF

   ON LAST ROW
      FOR i = 1 TO 20
         IF l_reg_sum[i].monto_acc > 0 AND
            l_reg_sum[i].monto_pes > 0 THEN
               PRINT "91",i                 USING "&&",
                     l_reg_sum[i].monto_acc USING "&&&&&&&&&&&&&&.&&",
                     l_reg_sum[i].monto_pes USING "&&&&&&&&&&&&&&.&&"
         END IF
      END FOR

       PRINT "99",vfolio_liquida

END REPORT
###########################################################################
#eof

