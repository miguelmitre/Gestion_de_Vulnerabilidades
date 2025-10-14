###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Fecha                  => 20 de Marzo de 2002                            #
#Modificado             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha modificacion     => 29 de marzo de 2005                            #
#Modulo                 => UNI.                                           #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE hoy                DATE,
          usuario            CHAR(8),
          vruta_listado      CHAR(40),
          nss                CHAR(8),
          g_lista            CHAR(100),
          g_impre            CHAR(100),
          vcodigo_afore      SMALLINT,
          vtipo_rechazo      SMALLINT,
          vtipo_rechazo2     SMALLINT,
          opc                CHAR(1)

   DEFINE l_reg3  ARRAY[30000] OF RECORD 
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_op_21       DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
          fecha_certifi     DATE,
          folio             INTEGER
   END RECORD

   DEFINE x_periodo             DATE,
          x_tipo                CHAR(2),
          xtipo                 CHAR(2),
          x_total_uni           SMALLINT,
          x_total_cta1          SMALLINT,
          x_fecha_liquidacion   DATE,
          x_folio               INTEGER,
          x_nombre_imss         CHAR(60)

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

   CALL STARTLOG("UNIM00413.log")

   LET hoy = TODAY

   SELECT ruta_listados
   INTO   vruta_listado
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   SELECT codigo_afore
   INTO   vcodigo_afore
   FROM   tab_afore_local

   OPEN WINDOW v3 AT 3,3 WITH FORM "UNIM00413" ATTRIBUTE (BORDER)
   DISPLAY "UNIM004         CUENTAS EN PROCESO DE UNIFICACION                              " AT 3,1 ATTRIBUTE (REVERSE) 
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)
   #DISPLAY "      [ Enter ] para iniciar                      [ Control-C ] para salir       " AT 2,1 ATTRIBUTE (REVERSE)
   DISPLAY "      [ Enter ] para iniciar                      [ Control-C ] para salir       " AT 2,1

   CALL proceso_3()

--   CLOSE WINDOW v3

END MAIN}
#####################################################################
FUNCTION proceso_3()

   DEFINE vtipo_rechazo      SMALLINT,
          x_resul_operacion  CHAR(2)

   OPEN WINDOW v3 AT 3,3 WITH FORM "UNIM00413" ATTRIBUTE (BORDER)
   DISPLAY "UNIM004         CUENTAS EN PROCESO DE UNIFICACION                              " AT 3,1 ATTRIBUTE (REVERSE) 
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)
   DISPLAY "      [ Enter ] para iniciar                      [ Control-C ] para salir       " AT 2,1


   LET pos = 1
   LET cuantos = 0
   LET x_total_uni = 0
   LET x_total_cta1 = 0

   LET int_flag = FALSE

   CONSTRUCT cla_where ON fecha_recepcion,
                          ident_movimiento
                     FROM x_periodo,
                          x_tipo

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
      CLOSE WINDOW v3
      RETURN
   END IF

   LET sel_where = " SELECT b.nss_uni,",
                           "TRIM (b.paterno_uni)||' '||TRIM(b.materno_uni)||' '||TRIM(b.nombre_uni) nombre,",
                           "' ',",
                           "b.num_ctas_asoc,",
                           "b.estado,",
                           "c.descripcion,",
                           "b.cve_ent_nss,",
                           "' ',",
                           "b.folio,",
                           "b.nombre_imss_uni, ",
                           "b.tipo_ent_nss ",
                   " FROM   uni_ctr_archivo a,uni_unificador b ,outer uni_status c ",
                   " WHERE  a.folio = b.folio ",
                   --" AND    a.nombre MATCHES '*uni2' ",
                   " AND    b.estado = c.estado ",
                   " AND ",cla_where CLIPPED,
                   " GROUP BY 1,2,3,4,5,6,7,8,9,10,11 ",
                   " ORDER BY 1,5 "

   PREPARE exe_sel3 FROM sel_where

   DECLARE cursor_3 CURSOR FOR exe_sel3

   ERROR "PROCESANDO INFORMACION"

   LET cuantos = 0

   FOREACH cursor_3 INTO l_reg3[pos].*,
                         x_nombre_imss,
                         xtipo

      IF l_reg3[pos].estado >= 20 THEN
         SELECT max (fecha_recepcion)
         INTO   l_reg3[pos].fecha_op_21
         FROM   uni_ctr_archivo
         WHERE  folio = x_folio
         --AND    nombre MATCHES "*uni9" 

         CASE xtipo
            WHEN "00"
               SELECT a.fecha_certifica
               INTO   l_reg3[pos].fecha_certifi
               FROM   uni_det_certifica a
               WHERE  a.nss = l_reg3[pos].nss_uni
            WHEN "01"
               IF l_reg3[pos].cve_ent_nss = vcodigo_afore THEN
                  SELECT fentcons
                  INTO l_reg3[pos].fecha_certifi
                  FROM  afi_mae_afiliado
                  WHERE n_seguro = l_reg3[pos].nss_uni
               ELSE
                  SELECT a.fecha_certifica
                  INTO   l_reg3[pos].fecha_certifi
                  FROM   uni_det_traspaso a
                  WHERE  a.nss = l_reg3[pos].nss_uni
               END IF
            WHEN "59"
               SELECT a.fecha_certifica
               INTO   l_reg3[pos].fecha_certifi
               FROM   uni_det_asignado a
               WHERE  a.nss = l_reg3[pos].nss_uni
         END CASE
      END IF

      IF l_reg3[pos].nombre = "  " THEN
         LET l_reg3[pos].nombre = x_nombre_imss
      END IF

      LET cuantos = cuantos + l_reg3[pos].num_ctas_asoc
      LET pos  = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      LET x_total_uni = pos - 1
      LET x_total_cta1 = cuantos

      DISPLAY BY NAME x_total_uni
      DISPLAY BY NAME x_total_cta1

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg3 TO scr_1.*
         ON KEY (control-p)
            ERROR "PROCESANDO IMPRESION..."
            CALL listado_3(pos)
         ON KEY (control-m)
            LET pos = ARR_CURR()

            CALL detalle_cta1(l_reg3[pos].folio,
                              l_reg3[pos].nss_uni,
                              l_reg3[pos].num_ctas_asoc)

         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY

      CLOSE WINDOW v3
   ELSE
      ERROR "Registro inexistente ..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW v3
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION listado_3(pos)

   DEFINE vtipo_rechazo2     SMALLINT,
          pos                SMALLINT,
          i                  SMALLINT

   DEFINE g_reg3  RECORD 
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_op_21       DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
          fecha_certifi     DATE,
	  folio             INTEGER
   END RECORD

   ERROR "PROCESANDO INFORMACION ..."

   LET g_impre = vruta_listado CLIPPED,"/",
                 "REPORTE_3"

   START REPORT impresion_3 TO g_impre

      FOR i = 1 TO (pos+1)

         LET g_reg3.nss_uni         = l_reg3[i].nss_uni
         LET g_reg3.nombre          = l_reg3[i].nombre
         LET g_reg3.fecha_op_21     = l_reg3[i].fecha_op_21
         LET g_reg3.num_ctas_asoc   = l_reg3[i].num_ctas_asoc
         LET g_reg3.estado          = l_reg3[i].estado
         LET g_reg3.descripcion     = l_reg3[i].descripcion
         LET g_reg3.cve_ent_nss     = l_reg3[i].cve_ent_nss
         LET g_reg3.fecha_certifi   = l_reg3[i].fecha_certifi
         LET g_reg3.folio           = l_reg3[i].folio

         IF g_reg3.nss_uni IS NULL THEN
            EXIT FOR
         END IF

         OUTPUT TO REPORT impresion_3(g_reg3.*)
      END FOR

   FINISH REPORT impresion_3

   ERROR "LISTADO GENERADO ..."
   SLEEP 2
   ERROR ""
{
   LET g_lista = "vi ",g_impre
   RUN g_lista
}
END FUNCTION
###############################################################################
REPORT impresion_3(g_reg3)

   DEFINE g_reg3  RECORD 
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_op_21       DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          cve_ent_nss       CHAR(3),
          fecha_certifi     DATE,
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
      ON EVERY ROW

         PRINT COLUMN 01,g_reg3.nss_uni,"|",
                         g_reg3.nombre CLIPPED ,"|",
                         g_reg3.fecha_op_21 USING "DD/MM/YYYY","|",
                         g_reg3.num_ctas_asoc USING "###","|",
                         g_reg3.estado USING "###","|",
                         g_reg3.descripcion CLIPPED,"|",
                         g_reg3.cve_ent_nss ,"|",
                         g_reg3.fecha_certifi USING "DD/MM/YYYY","|"
END REPORT
#########################################################################
