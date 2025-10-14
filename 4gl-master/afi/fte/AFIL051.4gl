###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P  					              #
#Programa AFIL051  => STATUS INTERNO DE PRODUCCION VS CERTIFICACION           #
#Sistema           => AFI  					              #
#Autor             => OMAR SANDOVAL BADILLO                                   #
#Fecha             => 17 DE ENERO DE 2007                                     #
###############################################################################
DATABASE safre_af
GLOBALS

   DEFINE enter                 CHAR(1),
          opc                   CHAR(1),
          g_usuario             CHAR(8),
          g_impre               CHAR(100),
          aux_pausa             CHAR(01),
          HOY                   DATE,
          g_afore               RECORD LIKE tab_afore_local.*,
          g_parametro           RECORD LIKE seg_modulo.*
        
   DEFINE arr_01  ARRAY[32000] OF RECORD
     p_tipo_solicitud   INTEGER,
     p_desc_solicitud   CHAR(30),
     p_total            INTEGER,
     c_tipo_solicitud   SMALLINT,
     c_desc_solicitud   CHAR(30),
     c_total            INTEGER,
     diferencia         INTEGER
   END RECORD

   DEFINE arr_02  ARRAY[32000] OF RECORD
     p_tipo_solicitud   LIKE solicitudafi.idtposolicitud,
     p_desc_solicitud   CHAR(30),
     p_status           VARCHAR(3,0),
     p_status_desc      CHAR(30),
     p_total            INTEGER,
     c_tipo_solicitud   SMALLINT,
     c_desc_solicitud   CHAR(30),
     c_status           SMALLINT,
     c_status_desc      CHAR(30),
     c_total            INTEGER,
     diferencia         INTEGER
   END RECORD

   DEFINE arr_03 RECORD
     p_tipo_solicitud        INTEGER,
     p_desc_solicitud        CHAR(30),
     p_status                SMALLINT,
     p_status_desc           CHAR(30),
     p_total                 INTEGER
   END RECORD

   DEFINE arr_04 RECORD
      p_tipo_solicitud        INTEGER,
      p_desc_solicitud        CHAR(30),
      p_total                 INTEGER
   END RECORD


   DEFINE sql_txt_01   ,
          sql_txt_02   ,
          sql_txt_03   ,
          sql_txt_04   CHAR(500)

END GLOBALS
###############################################################################
MAIN

    OPTIONS
       PROMPT LINE LAST,
       INPUT WRAP,
       COMMENT LINE LAST

    DEFER INTERRUPT

    SELECT *,USER
    INTO   g_parametro.*,
           g_usuario
    FROM   seg_modulo
    WHERE modulo_cod = "afi"

    CALL STARTLOG("AFIL051.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN
###############################################################################
FUNCTION proceso_principal()

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL05111" ATTRIBUTE(BORDER)
    DISPLAY " AFIL051      STATUS INTERNO CONTROL PRODUCCION VS CERTIFICACION               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                                <Ctrl-C> Cancelar           <Ctrl-T> Detalle   " AT 4,1 

    MENU "CONTROL "
        COMMAND "Consulta" "Inicia consulta de status interno"
           CALL control_interno()
           CLEAR FORM
        COMMAND "Salir" "Salir de Programa"
           EXIT MENU
    END MENU

END FUNCTION
###############################################################################
FUNCTION inicio()

    LET HOY = TODAY

    SELECT *,
           USER 
    INTO   g_afore.*,
           g_usuario 
    FROM   tab_afore_local

---pantalla 01

    LET sql_txt_01 = " SELECT a.idtposolicitud p_tipo_solicitud,",
                             "b.descorta       p_desc_solicitud,",
                             "COUNT(*)         p_total",
                     " FROM   solicitudafi a, OUTER catalogosistema b",
                     " WHERE b.cvenatural in(1,2,3,4,5,6,7,8,9,10,11,12)",
                     " AND   b.idpadre IN( SELECT c.idcatalogo",
                                         " FROM   catalogosistema c",
                                         " WHERE  c.cvenatural= 'TPOSOLICITUD'",
                                         " AND    c.idpadre IS NULL)",
                     " AND   a.idtposolicitud = b.idcatalogo",
                     " GROUP BY 1,2",
                     " ORDER BY 1"
                     #" INTO TEMP tmp_produccion " 
    LET sql_txt_01 = sql_txt_01 CLIPPED

    LET sql_txt_02 = " SELECT a.tipo_solicitud  c_tipo_solicitud,",
                     "NVL(b.desc_solicitud,'----------') c_desc_solicitud,",
                             "COUNT(*)          c_total",
                     " FROM   afi_solicitud a,",
                             "tab_tipo_solic b,",
                             "tab_status_afi c",
                     " WHERE  b.tipo_solicitud IN(1,2,3,4,5,6,7,8,9,10,11,12)",
                     " AND    a.tipo_solicitud = b.tipo_solicitud",
                     " AND    a.status_interno = c.estado_cod",
                     " GROUP BY 1,2",
                     " ORDER BY 1,2,3",
                     " INTO TEMP tmp_certificacion " 
    LET sql_txt_02 = sql_txt_02 CLIPPED

---pantalla 02

    LET sql_txt_03 = " SELECT a.idtposolicitud    p_tipo_solicitud,",
                             "b.descorta          p_desc_solicitud,",
             "NVL(a.stamcertificafi,'          ') p_status,",
                             "COUNT(*)            p_total",
                     " FROM   solicitudafi a, OUTER catalogosistema b",
                     " WHERE b.cvenatural in(1,2,3,4,5,6,7,8,9,10,11,12)",
                     " AND   b.idpadre IN( SELECT c.idcatalogo",
                                         " FROM   catalogosistema c",
                                         " WHERE  c.cvenatural= 'TPOSOLICITUD'",
                                         " AND    c.idpadre IS NULL)",
                     " AND   a.idtposolicitud = b.idcatalogo",
                     " GROUP BY 1,2,3",
                     " ORDER BY 1,3,4"
                     #" INTO TEMP tmp_pro_detalle "
    LET sql_txt_03 = sql_txt_03 CLIPPED

    LET sql_txt_04 = " SELECT a.tipo_solicitud           c_tipo_solicitud,",
                     "NVL(c.desc_solicitud,'          ') c_desc_solicitud,",
                             "a.status_interno           c_status,",
                             "b.estado_desc              c_status_desc,",
                             "COUNT(*)                   c_total",
                     " FROM  afi_solicitud a,",
                            "tab_status_afi b, OUTER ",
                            "tab_tipo_solic c",
                     " WHERE  c.tipo_solicitud IN(1,2,3,4,5,6,7,8,9,10,11,12)",
                     " AND    a.tipo_solicitud = c.tipo_solicitud",
                     " AND    a.status_interno = b.estado_cod",
                     " GROUP BY 1,2,3,4",
                     " ORDER BY 1,3,5",
                     " INTO TEMP tmp_cer_detalle "
    LET sql_txt_04 = sql_txt_04 CLIPPED

    PREPARE query01 FROM sql_txt_01 
    PREPARE query02 FROM sql_txt_02 
    PREPARE query03 FROM sql_txt_03 
    PREPARE query04 FROM sql_txt_04 

END FUNCTION
################################################################################
FUNCTION control_interno()

   DEFINE i             INTEGER

   DEFINE p_totales      INTEGER,
          c_totales      INTEGER,
          d_totales      INTEGER

   DEFINE p_tipo_solicitud        LIKE solicitudafi.idtposolicitud,
          p_desc_solicitud        CHAR(30),
          p_total                 INTEGER


   DISPLAY "     CONTROL DE PRODUCCION                CONTROL DE CERTIFICACION             " AT 5,1 ATTRIBUTES(REVERSE)
   DISPLAY "Tipo Solicitud                 Total Tipo Solicitud              Total  Difer. " AT 6,1
   DISPLAY " Totales :                                                                     " AT 20,1 ATTRIBUTE(REVERSE)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_produccion
      DROP TABLE tmp_certificacion
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_produccion
   (p_tipo_solicitud        INTEGER,
    p_desc_solicitud        CHAR(30),
    p_total                 INTEGER
   )

   DECLARE cur_001 CURSOR FOR query01
   FOREACH cur_001 INTO p_tipo_solicitud,
                        p_desc_solicitud,
                        p_total

      LET arr_04.p_tipo_solicitud = p_tipo_solicitud
      LET arr_04.p_desc_solicitud = p_desc_solicitud
      LET arr_04.p_total          = p_total

      INSERT INTO tmp_produccion 
      VALUES(arr_04.*)

   END FOREACH

   #EXECUTE query01
   EXECUTE query02

   LET i = 1

   LET p_totales = 0
   LET c_totales = 0
   LET d_totales = 0

   DECLARE cur_01 CURSOR FOR 
   SELECT a.*,
          b.*,
          (NVL(a.p_total,0) - NVL(b.c_total,0)) diferencia
   FROM  tmp_produccion a, OUTER tmp_certificacion b
   WHERE a.p_desc_solicitud = b.c_desc_solicitud

   FOREACH cur_01 INTO arr_01[i].*

      LET p_totales = p_totales + arr_01[i].p_total
      LET c_totales = c_totales + arr_01[i].c_total
      LET d_totales = d_totales + arr_01[i].diferencia

      LET i = i + 1
   END FOREACH

   DISPLAY BY NAME p_totales ATTRIBUTE(REVERSE)
   DISPLAY BY NAME c_totales ATTRIBUTE(REVERSE)
   DISPLAY BY NAME d_totales ATTRIBUTE(REVERSE)

   LET i = i - 1

   IF i >= 1 THEN
      CALL SET_COUNT(i)
      DISPLAY ARRAY arr_01 TO scr_01.*

         ON KEY (CONTROL-C)
            DISPLAY "                                                                               " AT 5,1 
            DISPLAY "                                                                               " AT 6,1
            DISPLAY "           " AT 19,3
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            DISPLAY "                                                                               " AT 5,1 
            DISPLAY "                                                                               " AT 6,1
            DISPLAY "           " AT 19,3
            EXIT DISPLAY
         ON KEY (CONTROL-T)
            CALL control_detalle()

      END DISPLAY
      DISPLAY "                                                                               " AT 20,1 
   ELSE
      CLEAR FORM
      ERROR "  NO EXISTEN REGISTROS " ATTRIBUTE(NORMAL)
      RETURN
   END IF

END FUNCTION
################################################################################
FUNCTION control_detalle()

   DEFINE ii           SMALLINT
   DEFINE j,longitud   SMALLINT
   DEFINE xstatus      VARCHAR(3,0),
          xdescripcion VARCHAR(30,0)

   DEFINE cd_p_totales      INTEGER,
          cd_c_totales      INTEGER,
          cd_d_totales      INTEGER

   DEFINE l_arr_02  ARRAY[32000] OF RECORD
     p_tipo_solicitud   INTEGER,
     p_status_desc      CHAR(30),
     p_total            INTEGER,
     c_tipo_solicitud   SMALLINT,
     c_status           SMALLINT,
     c_status_desc      CHAR(30),
     c_total            INTEGER,
     diferencia         INTEGER
   END RECORD

   DEFINE p_tipo_solicitud        LIKE solicitudafi.idtposolicitud, 
          p_desc_solicitud        CHAR(30),
          p_status_desc           CHAR(30),
          p_total                 INTEGER

   OPEN WINDOW ventana_2 AT 4,2 WITH FORM "AFIL05112"
   DISPLAY " AFIL051      STATUS INTERNO CONTROL PRODUCCION VS CERTIFICACION               " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " <Ctrl-I> Imprime reporte       <Ctrl-C> Cancelar           <Ctrl-T> Detalle   " AT 2,1 
   DISPLAY " Totales :                                                                     " AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY "     CONTROL DE PRODUCCION                CONTROL DE CERTIFICACION             " AT 3,1 ATTRIBUTES(REVERSE)
   DISPLAY "Tip.Sol.      Estado      Total    Tip.Sol.     Estado       Total  Diferencia " AT 4,1

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_pro_detalle
      DROP TABLE tmp_cer_detalle
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_pro_detalle
   (p_tipo_solicitud        INTEGER,
    p_desc_solicitud        CHAR(30),
    p_status                VARCHAR(3,0),
    p_status_desc           CHAR(30),
    p_total                 INTEGER
   )

   EXECUTE query04

   LET cd_p_totales = 0 
   LET cd_c_totales = 0 
   LET cd_d_totales = 0 

   DECLARE cur_02 CURSOR FOR query03
   FOREACH cur_02 INTO  p_tipo_solicitud,
                        p_desc_solicitud,
                        p_status_desc,
                        p_total      

      LET p_status_desc = p_status_desc CLIPPED
      LET longitud = LENGTH(p_status_desc)

      FOR j = 1 TO longitud
         IF p_status_desc[j] = "-" THEN
            LET xstatus = p_status_desc[1,j-1]
            LET xdescripcion = p_status_desc[j+1,longitud]
            EXIT FOR
         END IF  
      END FOR

      LET arr_03.p_tipo_solicitud = p_tipo_solicitud
      LET arr_03.p_desc_solicitud = p_desc_solicitud
      LET arr_03.p_status         = xstatus
      --LET arr_03.p_status_desc    = xdescripcion
      LET arr_03.p_status_desc    = p_status_desc
      LET arr_03.p_total          = p_total

      INSERT INTO tmp_pro_detalle
      VALUES(arr_03.*)

      LET xstatus = " "
   END FOREACH

   DECLARE cur_03 CURSOR FOR
   SELECT a.*,
          b.*,
          (NVL(a.p_total,0) - NVL(b.c_total,0)) diferencia
   FROM   tmp_pro_detalle a, OUTER tmp_cer_detalle b
   WHERE  a.p_status = b.c_status
   AND    a.p_desc_solicitud = b.c_desc_solicitud
   #GROUP BY 1,2,3,4,5,6,7,8,9,10,11
   ORDER BY 1,4,5

   LET ii = 1

   FOREACH cur_03 INTO arr_02[ii].*

      LET l_arr_02[ii].p_tipo_solicitud = arr_02[ii].p_tipo_solicitud
      LET l_arr_02[ii].p_status_desc    = arr_02[ii].p_status_desc
      LET l_arr_02[ii].p_total          = arr_02[ii].p_total
      LET l_arr_02[ii].c_tipo_solicitud = arr_02[ii].c_tipo_solicitud
      LET l_arr_02[ii].c_status         = arr_02[ii].c_status
      LET l_arr_02[ii].c_status_desc    = arr_02[ii].c_status_desc
      LET l_arr_02[ii].c_total          = arr_02[ii].c_total
      LET l_arr_02[ii].diferencia       = arr_02[ii].diferencia

      IF l_arr_02[ii].p_total IS NULL OR
       l_arr_02[ii].p_total = " "   THEN
         LET l_arr_02[ii].p_total = 0
      END IF
 
      IF l_arr_02[ii].c_total IS NULL OR 
         l_arr_02[ii].c_total = " "   THEN
         LET l_arr_02[ii].c_total = 0
      END IF

      IF l_arr_02[ii].diferencia IS NULL OR
         l_arr_02[ii].diferencia = " "   THEN
         LET l_arr_02[ii].diferencia = 0
      END IF

      LET cd_p_totales = cd_p_totales + l_arr_02[ii].p_total
      LET cd_c_totales = cd_c_totales + l_arr_02[ii].c_total
      LET cd_d_totales = cd_d_totales + l_arr_02[ii].diferencia

      LET ii = ii + 1
   END FOREACH

   DISPLAY BY NAME cd_p_totales ATTRIBUTE(REVERSE)
   DISPLAY BY NAME cd_c_totales ATTRIBUTE(REVERSE)
   DISPLAY BY NAME cd_d_totales ATTRIBUTE(REVERSE)

   LET ii = ii - 1

   IF ii >= 1 THEN
      CALL SET_COUNT(ii)
      DISPLAY ARRAY l_arr_02 TO scr_02.*

         ON KEY (CONTROL-C)
            DISPLAY "                                                                               " AT 5,1
            DISPLAY "                                                                               " AT 6,1
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            DISPLAY "                                                                               " AT 5,1
            DISPLAY "                                                                               " AT 6,1
            EXIT DISPLAY

         ON KEY (CONTROL-I)
            CALL impresion()

      END DISPLAY
   ELSE
      CLEAR FORM
      ERROR "  NO EXISTEN REGISTROS " ATTRIBUTE(NORMAL)
      RETURN
   END IF

   CLOSE WINDOW ventana_2

END FUNCTION
################################################################################
FUNCTION impresion()

   DEFINE l_reg_imp  RECORD
      p_tipo_solicitud   INTEGER,
      p_desc_solicitud   VARCHAR(30,0),
      p_status           VARCHAR(3,0),
      p_status_desc      VARCHAR(30,0),
      p_totales          INTEGER,
      c_tipo_solicitud   INTEGER,
      c_desc_solicitud   VARChAR(30,0),
      c_status           VARCHAR(3,0),
      c_status_desc      VARCHAR(30,0),
      c_totales          INTEGER,
      diferencia         INTEGER
   END RECORD

   LET g_impre = g_parametro.ruta_listados CLIPPED, "/",
                 g_usuario CLIPPED,".STATUS",
                 ".",hoy USING "dd-mm-yyyy"

   START REPORT rep_status TO g_impre
      DECLARE cur_imp CURSOR FOR
      SELECT a.*,
             b.*,
             (NVL(a.p_total,0) - NVL(b.c_total,0)) diferencia
      FROM   tmp_pro_detalle a, OUTER tmp_cer_detalle b
      WHERE  a.p_status = b.c_status
      AND    a.p_desc_solicitud = b.c_desc_solicitud
      ORDER BY 1,4,5

      FOREACH cur_imp INTO l_reg_imp.*

         IF l_reg_imp.p_totales IS NULL OR
            l_reg_imp.p_totales = " "   THEN
            LET l_reg_imp.p_totales = 0
         END IF

         IF l_reg_imp.c_totales IS NULL OR
            l_reg_imp.c_totales = " "   THEN
            LET l_reg_imp.c_totales = 0
         END IF

         IF l_reg_imp.diferencia IS NULL OR
            l_reg_imp.diferencia = " "   THEN
            LET l_reg_imp.diferencia = 0
         END IF

         OUTPUT TO REPORT rep_status(l_reg_imp.*) 

      END FOREACH
   FINISH REPORT rep_status 

   WHILE TRUE
      PROMPT " Esta seguro de generar el reporte? S/N" FOR CHAR opc
         IF opc  MATCHES "[SsNn]" THEN
            IF opc MATCHES "[Ss]" THEN
               LET g_impre = "lp ",g_impre
               #LET g_impre = "vi ",g_impre
               RUN g_impre

               ERROR " REPORTE GENERADO"
               SLEEP 1
               EXIT WHILE
            ELSE
               ERROR " PROCESO CANCELADO..."
               SLEEP 2
               EXIT WHILE
            END IF
         END IF
   END WHILE

END FUNCTION
########################################################################
REPORT rep_status(l_reg_report)

   DEFINE l_reg_report  RECORD
      p_tipo_solicitud   INTEGER,
      p_desc_solicitud   VARCHAR(30,0),
      p_status           VARCHAR(3,0),
      p_status_desc      VARCHAR(30,0),
      p_totales          INTEGER,
      c_tipo_solicitud   INTEGER,
      c_desc_solicitud   VARCHAR(30,0),
      c_status           VARCHAR(3,0),
      c_status_desc      VARCHAR(30,0),
      c_totales          INTEGER,
      diferencia         INTEGER
   END RECORD

   DEFINE gran_total_p    ,
          gran_total_c    ,
          gran_total_d    INTEGER

   DEFINE xp_total    ,
          xc_total    ,
          xd_total    INTEGER

   DEFINE identificador    INTEGER

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
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   90

   FORMAT

   PAGE HEADER
      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      LET gran_total_p = 0
      LET gran_total_c = 0
      LET gran_total_d = 0

      LET xp_total = 0
      LET xc_total = 0
      LET xd_total = 0

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      PRINT COLUMN 001,g_afore.razon_social  CLIPPED,
            COLUMN 052," STATUS INTERNO PRODUCCION vs CERTIFICACION ",
            COLUMN 106,"FECHA: ",TODAY USING "DD/MM/YYYY"
      SKIP 1 LINES

      PRINT COLUMN 001,"AFIL051"
      SKIP 1 LINES

      PRINT COLUMN 033,"SAFRE V2.5",
            COLUMN 122,"SAFRE V2.0"
      SKIP 4 LINES

      PRINT COLUMN 1,'\033e\033(s218T\033(s16H\033(s7B'
--osb
      PRINT COLUMN 1,"\332",L10,L3,
                     "\302",L10,L10,L1,
                     "\302",L10,L10,L10,L10,L10,
                     "\302",L5,L5,L1,
                     "\302",L5,L5,L10,L5,
                     "\302",L5,L5,L3,
                     "\302",L5,L5,L1,
                     "\302",L10,L5,L3,L2,L1,
                     "\277"

      PRINT COLUMN 1,"| Tipo Solicitud |      Descripcion      |   Status certificacion    |   Total   | Tipo Solicitud |     Descripcion     |       Status interno       |   Total  | Diferencia |"

      PRINT COLUMN 1,"\300",L10,L3,
                     "\301",L10,L10,L1,
                     "\301",L10,L10,L10,L10,L10,
                     "\301",L5,L5,L1,
                     "\301",L5,L5,L10,L5,
                     "\301",L5,L5,L3,
                     "\301",L5,L5,L1,
                     "\301",L10,L5,L3,L2,L1,
                     "\331"

      LET xp_total = xp_total + l_reg_report.p_totales
      LET xc_total = xc_total + l_reg_report.c_totales
      LET xd_total = xd_total + l_reg_report.diferencia

      LET identificador = l_reg_report.p_tipo_solicitud

   ON EVERY ROW
      PRINT COLUMN 008,l_reg_report.p_tipo_solicitud USING "<<<<",
            COLUMN 024,l_reg_report.p_desc_solicitud CLIPPED,
            #COLUMN 030,l_reg_report.p_status         USING "<<<<",
            COLUMN 044,l_reg_report.p_status_desc    CLIPPED,
            COLUMN 075,l_reg_report.p_totales        USING "<<<<<",
            COLUMN 090,l_reg_report.c_tipo_solicitud USING "<<<<",
            COLUMN 103,l_reg_report.c_desc_solicitud CLIPPED,
            COLUMN 125,l_reg_report.c_status         USING "<<<<",
            COLUMN 129,l_reg_report.c_status_desc    CLIPPED,
            COLUMN 154,l_reg_report.c_totales        USING "<<<<<",
            COLUMN 165,l_reg_report.diferencia       USING "<<<<<"

      LET gran_total_p = gran_total_p + l_reg_report.p_totales
      LET gran_total_c = gran_total_c + l_reg_report.c_totales
      LET gran_total_d = gran_total_d + l_reg_report.diferencia

   ON LAST ROW
      SKIP 4 LINE

      PRINT COLUMN 1,"\332",L10,L3,
                     L1,L10,L1,L10,L10,L10,L10,L10,L10,L10,L10,L10,L3,L1,
                     "\302",L5,L5,L1,L3,L1,
                     "\302",L5,L5,L3,
                     "\302",L10,L5,L3,L4,
                     "\277"

      PRINT COLUMN 001,"|    Total Global",
            COLUMN 072,"|",
            COLUMN 074,gran_total_p USING "<<<<<<",
            COLUMN 083,"|",
            COLUMN 150,"|",
            COLUMN 154,gran_total_c USING "<<<<<",
            COLUMN 161,"|",
            COLUMN 163,gran_total_d USING "<<<<<",
            COLUMN 174,"|"

      PRINT COLUMN 1,"\300",L10,L3,
                     L1,L10,L1,L10,L10,L10,L10,L10,L10,L10,L10,L10,L3,L1,
                     "\301",L5,L5,L1,L3,L1,
                     "\301",L5,L5,L3,
                     "\301",L10,L5,L3,L4,
                     "\331"

END REPORT
########################################################################
