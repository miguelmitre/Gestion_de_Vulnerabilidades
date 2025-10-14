###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Programa     EXCC002   => Reporte consulta de  pagos en exceso           #
#Fecha                  => 20 de Marzo de 2002                            #
#Modificado             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha modifica         => 12 de junio de 2002.                           #
#Fecha modifica         => 11 de septiembre de 2002.                      #
#Modulo                 => EXC.                                           #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE x_folio            INTEGER,
          hoy                DATE,
          hoy_01                DATE,
          usuario            CHAR(8),
          vruta_listado      CHAR(40),
          nss                CHAR(8),
          g_lista            CHAR(100),
          g_impre            CHAR(100),
          vtipo_rechazo      SMALLINT,
          vtipo_rechazo2     SMALLINT,
          opc                CHAR(1)

   DEFINE l_reg2  ARRAY[25000] OF RECORD 
          nss_uni           CHAR(11),
          ident_movimiento  CHAR(2),
          desc_ident        CHAR(5),
          nombre            CHAR(60),
          fecha_liquidacion DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
          folio             INTEGER
   END RECORD

   DEFINE x_periodo             DATE,
          x_tipo                CHAR(2),
          x_total_uni           SMALLINT,
          x_total_cta1          SMALLINT,
          x_fecha_liquidacion   DATE

   DEFINE pos                INTEGER,
          i                  SMALLINT,
          cuantos            SMALLINT,
          cla_where          CHAR(600),
          sel_where          CHAR(600)

END GLOBALS
#####################################################################
{MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   CALL STARTLOG("UNIM0412.log")

   LET hoy = TODAY

   SELECT ruta_listados
   INTO   vruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   OPEN WINDOW v2 AT 3,3 WITH FORM "UNIM00412" ATTRIBUTE (BORDER)
   DISPLAY "UNIM004          CUENTAS A UNIFICAR EN EL SIGUIENTE PERIODO                    " AT 3,1 ATTRIBUTE (REVERSE) 
   DISPLAY "      [ Enter ] para iniciar                      [ Control-C ] para salir       " AT 2,1
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)

   CALL proceso_2()

--   CLOSE WINDOW v2

END MAIN}
#####################################################################
FUNCTION proceso_2()

   DEFINE vtipo_rechazo      SMALLINT,
          x_resul_operacion  CHAR(2)
          
   OPEN WINDOW v2 AT 6,2 WITH FORM "UNIM00412"
   DISPLAY "[Enter] para iniciar       [Ctrl-U] Detalle unificador            [Ctrl-C] Salir  " AT 1,1
   DISPLAY "UNIM004          CUENTAS A UNIFICAR EN EL SIGUIENTE PERIODO                    " AT 2,1 ATTRIBUTE (REVERSE) 
   DISPLAY hoy USING "DD-MM-YYYY" AT 2,67 ATTRIBUTE (REVERSE)

   DISPLAY " NSS                                           Fecha   No.  Estado             " AT 4,1 ATTRIBUTE (REVERSE) 
   DISPLAY " Unificador           Nombre                   Liquida Uni  Actual     Afore   " AT 5,1 ATTRIBUTE (REVERSE) 

   LET pos = 1
   LET cuantos = 0
   LET x_total_uni = 0
   LET x_total_cta1 = 0

   LET int_flag = FALSE

   IF hoy_01 = MDY(MONTH(hoy),1,YEAR(hoy)) THEN
   ELSE
      LET hoy_01 = hoy + 1 UNITS MONTH
      LET hoy_01 = MDY(MONTH(hoy_01),1,YEAR(hoy_01))
   END IF

   LET x_fecha_liquidacion = habil_siguiente(hoy_01)

   CONSTRUCT cla_where ON ident_movimiento
                     FROM x_tipo

      ON KEY (control-m)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (control-c)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW v2
      RETURN
   END IF

   LET sel_where = " SELECT b.nss_uni,",
                           "b.ident_movimiento,",
                           "'',",
                           "TRIM (b.paterno_uni)||' '||TRIM(b.materno_uni)||' '||TRIM(b.nombre_uni) nombre,",
                           "' ',",
                           "b.num_ctas_asoc,",
                           "b.estado,",
                           "c.descripcion,",
                           "b.cve_ent_nss, ",
                           "b.folio ",
                   " FROM   uni_unificador b ,outer uni_status c ",
                   " WHERE  b.estado in (40,90) ",
                   " AND b.estado = c.estado ",
                   " AND ",cla_where CLIPPED,
                   " AND b.num_ctas_asoc = ( SELECT COUNT(*)",
                                           " FROM  uni_unificado ",
                                           " WHERE nss_uni = b.nss_uni ",
                                           " AND   estado IN (40,90)) "

   PREPARE exe_sel1 FROM sel_where

   DECLARE cursor_2 CURSOR FOR exe_sel1

   LET cuantos = 0

   FOREACH cursor_2 INTO l_reg2[pos].*

      IF l_reg2[pos].ident_movimiento = "01" THEN
         LET l_reg2[pos].desc_ident = "INTRA"
      ELSE
         LET l_reg2[pos].desc_ident = "EXTRA"
      END IF

      LET l_reg2[pos].fecha_liquidacion = x_fecha_liquidacion

      IF l_reg2[pos].nombre = " " THEN
         SELECT UNIQUE nombre_imss_uni
         INTO   l_reg2[pos].nombre
         FROM   uni_unificador
         WHERE  nss_uni = l_reg2[pos].nss_uni
      END IF

      LET cuantos = cuantos + l_reg2[pos].num_ctas_asoc
      LET pos  = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      LET x_total_uni = pos - 1
      LET x_total_cta1 = cuantos

      DISPLAY BY NAME x_total_uni
      DISPLAY BY NAME x_total_cta1

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg2 TO scr_1.*
         ON KEY (control-p)
            ERROR "PROCESANDO IMPRESION..."

            CALL listado_2(pos)
         ON KEY (control-m)
            LET pos = ARR_CURR()

            CALL detalle_cta1(l_reg2[pos].folio,
                              l_reg2[pos].nss_uni,
                              l_reg2[pos].num_ctas_asoc)
         ON KEY (control-u)
            LET pos = ARR_CURR()
            CALL consulta(l_reg2[pos].nss_uni,
                          l_reg2[pos].folio,
                          l_reg2[pos].estado)
         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY

      CLOSE WINDOW v2
   ELSE
      ERROR "Registro inexistente ..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW v2
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION listado_2(pos)

   DEFINE vtipo_rechazo2     SMALLINT,
          pos                SMALLINT,
          i                  SMALLINT

   DEFINE g_reg2  RECORD 
          nss_uni           CHAR(11),
          ident_movimiento  CHAR(2),
          desc_ident        CHAR(5),
          nombre            CHAR(60),
          fecha_liquidacion DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
         folio             INTEGER
   END RECORD

   ERROR "PROCESANDO INFORMACION ..."

   LET g_impre = vruta_listado CLIPPED,"/",
                 "REPORTE_2"

   START REPORT impresion_2 TO g_impre
      FOR i = 1 TO (pos+1)

         LET g_reg2.nss_uni           = l_reg2[i].nss_uni
         LET g_reg2.ident_movimiento  = l_reg2[i].ident_movimiento
         LET g_reg2.desc_ident        = l_reg2[i].desc_ident
         LET g_reg2.nombre            = l_reg2[i].nombre
         LET g_reg2.fecha_liquidacion = l_reg2[i].fecha_liquidacion
         LET g_reg2.num_ctas_asoc     = l_reg2[i].num_ctas_asoc
         LET g_reg2.estado            = l_reg2[i].estado
         LET g_reg2.descripcion       = l_reg2[i].descripcion
         LET g_reg2.cve_ent_nss       = l_reg2[i].cve_ent_nss
         LET g_reg2.folio             = l_reg2[i].folio

         IF g_reg2.nss_uni IS NULL THEN
            EXIT FOR
         END IF

         OUTPUT TO REPORT impresion_2(g_reg2.*)
      END FOR
   FINISH REPORT impresion_2

   ERROR ""
   ERROR "LISTADO GENERADO ..."
   SLEEP 2
   ERROR ""
{
   LET g_lista = "vi ",g_impre
   RUN g_lista
}
END FUNCTION
###############################################################################
REPORT impresion_2(g_reg2)

   DEFINE g_reg2  RECORD 
          nss_uni           CHAR(11),
          ident_movimiento  CHAR(2),
          desc_ident        CHAR(5),
          nombre            CHAR(60),
          fecha_liquidacion DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
          folio             INTEGER
   END RECORD

   DEFINE L1                 CHAR(01),
          L2                 CHAR(02),
          L3                 CHAR(03),
          L4                 CHAR(04),
          L5                 CHAR(05),
          L6                 CHAR(06),
          L7                 CHAR(07),
          L8                 CHAR(08),
          L9                 CHAR(09),
          L10                CHAR(10)

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  2

   FORMAT
      PAGE HEADER
         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"

      ON EVERY ROW
         PRINT COLUMN 01,g_reg2.nss_uni,"|",
                         g_reg2.ident_movimiento,"|",
                         g_reg2.nombre CLIPPED,"|", 
                         g_reg2.fecha_liquidacion USING "DD/MM/YYYY","|",
                         g_reg2.num_ctas_asoc USING "###","|",
                         g_reg2.estado USING "###","|",
                         g_reg2.descripcion CLIPPED,"|",
                         g_reg2.cve_ent_nss ,"|"

END REPORT
################################################################################
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado 
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig

END FUNCTION
