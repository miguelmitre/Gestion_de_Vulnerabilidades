###########################################################################
#Proyecto               => Sistema de Afores. (MEXICO)                    #
#Propietario            => E.F.P                                          #
#Fecha                  =>                                                #
#Modificado             => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Fecha modificacion     => 29 de marzo de 2005                            #
#Modulo                 => UNI.                                           #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE x_folio            INTEGER,
          hoy                DATE,
          usuario            CHAR(8),
          vruta_listado      CHAR(40),
          nss                CHAR(8),
          g_lista            CHAR(100),
          g_impre            CHAR(100),
          vtipo_rechazo      SMALLINT,
          vtipo_rechazo2     SMALLINT,
          opc                CHAR(1)

   DEFINE l_reg1  ARRAY[25000] OF RECORD 
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_recepcion   DATE,
          fnotifica         DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          folio             INTEGER
   END RECORD

   DEFINE x_periodo       DATE,
          x_tipo          CHAR(2),
          x_total_uni     SMALLINT,
          x_total_cta1    SMALLINT

   DEFINE pos                INTEGER,
          i                  SMALLINT,
          cuantos            SMALLINT,
          sed1               CHAR(60),
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

   CALL STARTLOG("UNIM00411.log")

   LET hoy = TODAY

   OPEN WINDOW v1 AT 3,3 WITH FORM "UNIM00411" ATTRIBUTE (BORDER)
   DISPLAY "              CONSULTA DE RECHAZOS DE OPERACION 22                                " AT 3,1 ATTRIBUTE (REVERSE) 
   DISPLAY "      [ Enter ] para iniciar                      [ Control-C ] para salir       " AT 2,1
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE (REVERSE)

   CALL proceso()

--   CLOSE WINDOW v1

END MAIN}
#####################################################################
FUNCTION proceso()

   DEFINE vtipo_rechazo      SMALLINT,
          x_resul_operacion  CHAR(2)

   OPEN WINDOW v11 AT 6,2 WITH FORM "UNIM00411" --ATTRIBUTE (BORDER)
   DISPLAY "              CONSULTA DE RECHAZOS DE OPERACION 22                                " AT 1,1 ATTRIBUTE (REVERSE) 
   DISPLAY hoy USING "DD-MM-YYYY" AT 1,67 ATTRIBUTE (REVERSE)
   DISPLAY "[Enter] para iniciar       [Ctrl-U] Detalle unificador            [Ctrl-C] Salir  " AT 2,1

   DISPLAY " NSS                                   Fecha    Fecha       No.       Estado      " AT 5,1 ATTRIBUTE (REVERSE) 
   DISPLAY " Unificador  Folio Nombre              Rechazo  Reenvio  Unificados   Actual      " AT 6,1 ATTRIBUTE (REVERSE) 

   LET pos = 1
   LET cuantos = 0
   LET x_total_uni = 0
   LET x_total_cta1 = 0

   LET int_flag = FALSE

   CONSTRUCT cla_where ON fnotifica,
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
      CLOSE WINDOW v11
      RETURN
   END IF

   LET sel_where = " SELECT b.nss_uni,",
                           "TRIM (b.paterno_uni)||' '||TRIM(b.materno_uni)||' '||TRIM(b.nombre_uni) nombre,",
                           "b.fliquida,",
                           "b.fnotifica,",
                           "b.num_ctas_asoc,",
                           "b.estado,",
                           "c.descripcion, ",
                           "b.folio ",
                      " FROM   uni_unificador b ,OUTER uni_status c",
                      " WHERE ",cla_where CLIPPED,
                      " AND b.fnotifica > b.fliquida ",
                      " AND b.estado = 95 ",
                      " AND b.estado = c.estado "

   PREPARE exe_sel FROM sel_where

   DECLARE cursor_1 CURSOR FOR exe_sel

   LET cuantos = 0

   FOREACH cursor_1 INTO l_reg1[pos].*

      IF l_reg1[pos].nombre = " " THEN
         SELECT UNIQUE nombre_imss_uni
         INTO   l_reg1[pos].nombre
         FROM uni_unificador
         WHERE  nss_uni = l_reg1[pos].nss_uni
      END IF

      LET cuantos = cuantos + l_reg1[pos].num_ctas_asoc
      LET pos  = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      LET x_total_uni = pos - 1
      LET x_total_cta1 = cuantos

      DISPLAY BY NAME x_total_uni
      DISPLAY BY NAME x_total_cta1

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg1 TO scr_1.*
         ON KEY (control-p)
            ERROR "PROCESANDO IMPRESION..."

            CALL listado_pendientes(pos)
         ON KEY (control-m)
            LET pos = ARR_CURR()

            CALL detalle_cta1(l_reg1[pos].folio,
                              l_reg1[pos].nss_uni,
                              l_reg1[pos].num_ctas_asoc)
         ON KEY (control-u)
            LET pos = ARR_CURR()
            CALL consulta(l_reg1[pos].nss_uni,
                          l_reg1[pos].folio,
                          l_reg1[pos].estado)

         ON KEY (control-c)
            EXIT DISPLAY
      END DISPLAY

      CLOSE WINDOW v11
   ELSE
      ERROR "Registro inexistente ..."
      SLEEP 2
      ERROR ""
      CLOSE WINDOW v11
   END IF

   CLEAR SCREEN
END FUNCTION
###############################################################################
FUNCTION listado_pendientes(pos)

   DEFINE pos                SMALLINT,
          i                  SMALLINT

   DEFINE g_reg RECORD
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_recepcion   DATE,
          fnotifica         DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          folio             INTEGER
   END RECORD

   DEFINE g_reg1 RECORD
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_recepcion   DATE,
          fnotifica         DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          folio             INTEGER
   END RECORD

   ERROR "PROCESANDO INFORMACION ..."

   SELECT ruta_listados,USER
   INTO   vruta_listado,usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "uni"

   LET g_impre = vruta_listado CLIPPED,"/",
                 "REPORTE_1"

   START REPORT impresion_1 TO g_impre
      FOR i = 1 TO (pos+1)

         LET g_reg.nss_uni           = l_reg1[i].nss_uni
         LET g_reg.nombre            = l_reg1[i].nombre
         LET g_reg.fecha_recepcion   = l_reg1[i].fecha_recepcion
         LET g_reg.fnotifica         = l_reg1[i].fnotifica
         LET g_reg.estado            = l_reg1[i].estado
         LET g_reg.descripcion       = l_reg1[i].descripcion
         LET g_reg.folio             = l_reg1[i].folio

         IF g_reg.nss_uni IS NULL THEN
            EXIT FOR
         END IF

         OUTPUT TO REPORT impresion_1(g_reg.*)
      END FOR
   FINISH REPORT impresion_1

   ERROR "LISTADO GENERADO ..."
   SLEEP 2
   ERROR ""
   #LET g_lista = "lp ",g_impre
   #LET g_lista = "vi ",g_impre
   #RUN g_lista

END FUNCTION
###############################################################################
REPORT impresion_1(l_reg3)

   DEFINE l_reg3 RECORD
          nss_uni           CHAR(11),
          nombre            CHAR(60),
          fecha_recepcion   DATE,
          fnotifica         DATE,
          num_ctas_asoc     SMALLINT,
          estado            SMALLINT,
          descripcion       CHAR(20),
          folio             INTEGER
   END RECORD

   OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   2

   FORMAT
      ON EVERY ROW
            PRINT COLUMN 001,l_reg3.nss_uni,
                  COLUMN 012,"|",
                  COLUMN 013,l_reg3.nombre,
                  COLUMN 073,"|",
                  COLUMN 074,l_reg3.fecha_recepcion,
                  COLUMN 084,"|",
                  COLUMN 085,l_reg3.fnotifica,
                  COLUMN 095,"|",
                  COLUMN 096,l_reg3.num_ctas_asoc USING "&&&",
                  COLUMN 099,"|",
                  COLUMN 100,l_reg3.estado USING "&&&",
                  COLUMN 103,"|",
                  COLUMN 104,l_reg3.descripcion

END REPORT
