################################################################################
#Owner             => E.F.P.                                                   #
#Programa TABM090  => CATALOGO DE GRUPO         			       #
#Fecha creacion    => 04 FEBRERO 2004                                          #
#By                => JUAN CARLOS MENDOZA MORENO                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param_dis   RECORD LIKE seg_modulo.*

   DEFINE aux_pausa     CHAR(1),
          sw_1          SMALLINT,
          hoy           DATE,
          pos           INTEGER,
          sel_where     CHAR(30000),
          cursor1       CHAR(30000),
          cursor2       CHAR(30000),
          cursor3       CHAR(30000),
	  select_cur    CHAR(30000),
          cla_where     CHAR(30000),
          g_impre       CHAR(300),
          g_lista       CHAR(300)

   DEFINE g_reg RECORD
          grupo         SMALLINT, 
          descripcion   CHAR(40)
   END RECORD

   DEFINE l_record ARRAY[30000] OF RECORD
          grupo         SMALLINT,
          descripcion   CHAR(40)
   END RECORD

END GLOBALS

MAIN

   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O
   DEFER INTERRUPT
   CALL proceso()

END MAIN

###############################################################################
FUNCTION proceso()

   LET HOY = TODAY
   OPEN WINDOW ven_1 AT 3,3 WITH FORM "TABM0901" ATTRIBUTE( BORDER)
   DISPLAY " TABM090                    CATALOGO  DE  GRUPO                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "CATALOGO GRUPO"
      COMMAND "Agrega" "Agrega grupo"
         CALL Agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta grupo"
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica grupo"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina grupo"
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Salir" "Sale del programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ven_1

END FUNCTION

###############################################################################
FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*

END FUNCTION

###############################################################################
FUNCTION Agrega()

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( ESC ) AGREGA                 (CTRL-C) SALIR                                 " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   LET g_reg.descripcion = NULL
   LET sw_1 = 0
   INPUT BY NAME  g_reg.*

   AFTER FIELD grupo
      SELECT "X"
      FROM   tab_grupo
      WHERE  grupo = g_reg.grupo
      IF STATUS <> NOTFOUND THEN
         ERROR "  GRUPO YA INGRESADO  "
         NEXT FIELD grupo
      END IF

      ON KEY ( ESC )

         IF g_reg.grupo = 0 OR g_reg.grupo IS NULL THEN
            ERROR "  CODIGO DE GRUPO NO PUEDE SER NULO  "
            NEXT FIELD  grupo
            IF g_reg.descripcion = 0 OR g_reg.descripcion IS NULL THEN
               ERROR "  DESCRIPCION DE GRUPO NO PUEDE SER NULO  "
               NEXT FIELD descripcion
            END IF
         END IF

         IF g_reg.descripcion = 0 OR g_reg.descripcion IS NULL THEN
            ERROR "  DESCRIPCION DE GRUPO NO PUEDE SER NULO  "
            NEXT FIELD descripcion
            IF g_reg.grupo = 0 OR g_reg.grupo IS NULL THEN
               ERROR "  CODIGO DE GRUPO NO PUEDE SER NULO  "
               NEXT FIELD  grupo
	    END IF  
         END IF

         SELECT "X"
         FROM   tab_grupo
         WHERE  grupo = g_reg.grupo

         IF STATUS <> NOTFOUND THEN
            ERROR "  GRUPO YA INGRESADO  "
            NEXT FIELD grupo
         END IF

         INSERT INTO tab_grupo VALUES ( g_reg.* )
         ERROR "  REGISTRO INGRESADO  " 
         SLEEP 1
         ERROR ""
         CALL Inicializa()
         NEXT FIELD grupo

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT

   END INPUT

END FUNCTION

###############################################################################
FUNCTION Consulta()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ven_2 AT 6,3 WITH FORM "TABM0902" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) CONSULTA             (CTRL-P) IMPRIMIR              (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY " TABM090                     CATALOGO  DE  GRUPO                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON grupo,descripcion
                          FROM grupo,descripcion
         ON KEY (control-m)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 1
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ven_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_grupo WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 1"

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL
      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-p)
               ERROR "  PROCESANDO INFORMACION...  "
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ven_2
      ELSE
         ERROR "  ARCHIVO DE GRUPO ... VACIO  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ven_2
      END IF
   END IF
   CLEAR SCREEN

END FUNCTION

###############################################################################
FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ven_2 AT 6,3 WITH FORM "TABM0902" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) MODIFICA                                            (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                     Escoja con <ENTER> el GRUPO a modificar                   " AT 2,1
      DISPLAY " TABM090                       CATALOGO  DE  GRUPO                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON grupo,descripcion
                          FROM grupo,descripcion
         ON KEY (CONTROL-M)
            ERROR "  PROCESANDO INFORMACION...  "
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ven_2
         RETURN
      END IF
      LET sel_where = "SELECT * FROM tab_grupo WHERE ",
                       cla_where CLIPPED,
                       "ORDER BY 1 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_reg.grupo       = l_record[pos].grupo
               LET g_reg.descripcion = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "  DEBE ELEGIR UN REGISTRO  "
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ven_2
      ELSE
         ERROR "  ARCHIVO DE  GRUPO ... VACIO  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ven_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (CTRL-C) SALIR " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME g_reg.* WITHOUT DEFAULTS

         BEFORE FIELD grupo
         NEXT FIELD descripcion

         AFTER FIELD descripcion
         IF g_reg.descripcion IS NULL THEN
            ERROR "  DESCRIPCION DE GRUPO NO PUEDE SER NULO  "
            NEXT FIELD descripcion
         END IF

         CALL Pregunta()
         IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_grupo 
            SET    descripcion = g_reg.descripcion
            WHERE  grupo       = g_reg.grupo
            ERROR "  REGISTRO MODIFICADO  "
            SLEEP 2
            ERROR ""
            CALL Inicializa()
         ELSE
            ERROR "  PROCESO DE MODIFICAR CANCELADO  "
            SLEEP 2
            ERROR ""
            CALL Inicializa()
         END IF

         EXIT INPUT
         ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT

      END INPUT
   ELSE
      ERROR "  ARCHIVO DE GRUPO ... VACIO  "
   END IF
   CLEAR SCREEN

END FUNCTION

###############################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ven_2 AT 6,3 WITH FORM "TABM0902" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) ELIMINA                                             (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                     Escoja con <ENTER> el GRUPO a eliminar                    " AT 2,1
      DISPLAY " TABM090                       CATALOGO  DE  GRUPO                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON grupo,descripcion
                          FROM grupo,descripcion
         ON KEY (control-m)
            ERROR "  PROCESANDO INFORMACION...  "
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ven_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_grupo WHERE ",
                       cla_where CLIPPED,
                      "ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET g_reg.grupo       = l_record[pos].grupo
               LET g_reg.descripcion = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "  DEBE ELEGIR UN REGISTRO  "
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ven_2
      ELSE
         ERROR "  ARCHIVO DE GRUPO ... VACIO  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ven_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (CTRL-C) SALIR " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY BY NAME g_reg.*
      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_grupo
         WHERE  grupo = g_reg.grupo
         ERROR "  REGISTRO ELIMINADO  "
         SLEEP 1
         ERROR ""
      ELSE
         ERROR "  PROCESO CANCELADO  "
         SLEEP 1
         ERROR ""
      END IF

      CALL Inicializa()
   ELSE
      ERROR "  ARCHIVO DE GRUPO ... VACIO  "
   END IF
   CLEAR SCREEN

END FUNCTION

###############################################################################

FUNCTION Pregunta()
   
   PROMPT "  ESTA SEGURO (S/N) ?  " FOR CHAR aux_pausa

END FUNCTION

###############################################################################
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT,
          usuario    CHAR(8)

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
                 usuario CLIPPED,".IMPTRECHA",
                 hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_grupo To PRINTER


   FOR i = 1 TO (pos+1)
      LET g_reg.grupo       = l_record[i].grupo
      LET g_reg.descripcion = l_record[i].descripcion
      IF g_reg.grupo   IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_grupo(g_reg.*)
   END FOR

   FINISH REPORT rpt_grupo
   ERROR "  LISTADO GENERADO...  "
   SLEEP 1
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista

END FUNCTION

###############################################################################
REPORT rpt_grupo(g_reg)

   DEFINE g_reg  RECORD
          grupo        SMALLINT,
          descripcion  CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM090 ",
               COLUMN 24," REPORTE DE GRUPO ",
               COLUMN 67,TODAY USING "dd/mm/yyyy"
         SKIP 2 LINE

         PRINT COLUMN 5,"GRUPO",
               COLUMN 30,"DESCRIPCION"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 7,g_reg.grupo,
               COLUMN 15,g_reg.descripcion
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," PAGINA : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," TOTAL DE REGISTROS : ",COUNT(*) USING "<<<<"
END REPORT
