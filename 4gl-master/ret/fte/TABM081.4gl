################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Propietario       => E.F.P.                                                   #
#Programa TABM081  => CATALOGO DE RETIROS                                      #
#Fecha             => 14 DE NOVIEMBRE 2003                                     #
#Autor             => JOSE MANUEL VIZCAINO CULEBRA                             #
#Fecha modifica    => 17 FECBRERO DE 2004                                      #
#Modificado        => JUAN CARLOS MENDOZA MORENO                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_dis   RECORD LIKE seg_modulo.*

   DEFINE aux_pausa     CHAR(1),
          sw_1          SMALLINT,
          hoy           DATE,
          usuario       CHAR(8),
          pos           INTEGER,
          sel_where     CHAR(30000),
          cla_where     CHAR(30000),
          g_impre       CHAR(300),
          g_lista       CHAR(300)

   DEFINE g_reg             RECORD
          tipo_retiro       CHAR(1),
	  movimiento        SMALLINT,
          descripcion       CHAR(60),
          fecha_actualiza   DATE,
	  nom_archivo       CHAR(5),	
          usuario           CHAR(12)
   END RECORD

   DEFINE l_record       ARRAY[30000] OF RECORD
	  tipo_retiro     CHAR(1),
	  movimiento      SMALLINT,
	  descripcion     CHAR(60), 
	  fecha_actualiza DATE,
	  nom_archivo     CHAR(5),
	  usuario         CHAR(12)
   END RECORD

END GLOBALS
------------------------------------------------------------------
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL inicio()
   CALL proceso()
END MAIN
-------------------------------------------------------------------
FUNCTION inicio()
   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"
END FUNCTION
---------------------------------------------------------------------
FUNCTION proceso()
      LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0811" ATTRIBUTE( BORDER)
   DISPLAY " TABM081                  CATALOGO  TIPO  DE  RETIRO                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "CATALOGO DE RETIROS"
      COMMAND "Agrega" "Agrega un tipo de retiro"
         CALL Agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta un tipo de retiro"
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica un tipo de retiro"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina un tipo de retiro"
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Salir" "Sale del programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Inicializa()
   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
--   DISPLAY BY NAME g_reg.tipo_retiro,
--		   g_reg.movimento,
--                   g_reg.descripcion
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( ESC ) AGREGA                 (CTRL-C) SALIR                                 " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
--   LET g_reg.descripcion = NULL
   CALL Inicializa()
   INITIALIZE g_reg.* TO NULL
   LET sw_1 = 0

   INPUT BY NAME  g_reg.tipo_retiro,
		  g_reg.movimiento,
		  g_reg.descripcion,
		  g_reg.nom_archivo

      AFTER FIELD tipo_retiro
         SELECT "X"
         FROM   tab_retiro
         WHERE  tipo_retiro = g_reg.tipo_retiro

         IF STATUS <> NOTFOUND THEN
         ERROR "  RETIRO YA INGRESADO  "
         NEXT FIELD tipo_retiro
         END IF

      ON KEY ( ESC )
         IF g_reg.tipo_retiro IS NULL THEN
            ERROR " CODIGO DEL RETIRO NO PUEDE SER NULO  "
            NEXT FIELD tipo_retiro
         END IF

         IF g_reg.movimiento IS NULL THEN
            ERROR "  MOVIMIENTO DE RETIRO NO PUEDE SER NULO  "
            NEXT FIELD movimiento
         END IF

         IF g_reg.descripcion IS NULL THEN
            ERROR "  DESCRIPCION DE RETIRO NO PUEDE SER NULO  "
            NEXT FIELD descripcion
         END IF
 
         LET g_reg.fecha_actualiza = TODAY

         IF g_reg.nom_archivo IS NULL THEN
            ERROR "  NOMBRE DEL ARCHIVO NO PUEDE SER NULO  "
            NEXT FIELD nom_archivo
         END IF

         LET g_reg.usuario = usuario

         INSERT INTO tab_retiro VALUES ( g_reg.* )
            ERROR "  REGISTRO INGRESADO  " 
            SLEEP 2
            ERROR ""
            CALL Inicializa()
            NEXT FIELD tipo_retiro

      ON KEY (INTERRUPT)
         CLEAR FORM
         CALL Inicializa()
         EXIT INPUT
   END INPUT
END FUNCTION        
----------------------------------------------------------------------
FUNCTION Consulta()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0812" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) CONSULTA             (CTRL-P) IMPRIMIR              (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY " TABM081                  CATALOGO  TIPO  DE  RETIRO                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT BY NAME cla_where ON tab_retiro.*
         ON KEY (control-m)
            ERROR "  PROCESANDO INFORMACION...  "
            SLEEP 1
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag     = TRUE THEN
         LET int_flag = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_retiro WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 1 "

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
--               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
      CLOSE WINDOW ventana_2
      ELSE
         ERROR "  ARCHIVO DE RETIROS... VACIO  " 
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
----------------------------------------------------------------------
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0812" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) MODIFICA                                            (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el RETIRO a MODIFICAR                        " AT 2,1
     DISPLAY " TABM081                CATALOGO  TIPO  DE  RETIRO                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
     DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT BY NAME cla_where ON tab_retiro.* 
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF
      LET sel_where = "SELECT * FROM tab_retiro WHERE ",
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
               LET g_reg.tipo_retiro     = l_record[pos].tipo_retiro
               LET g_reg.movimiento      = l_record[pos].movimiento
               LET g_reg.descripcion     = l_record[pos].descripcion
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_reg.nom_archivo     = l_record[pos].nom_archivo
               LET g_reg.usuario         = l_record[pos].usuario    
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "  DEBE ELEGIR UN REGISTRO  "
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "  ARCHIVO DE RETIROS... VACIO  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME g_reg.tipo_retiro,
                    g_reg.movimiento,
                    g_reg.descripcion, 
                    g_reg.nom_archivo WITHOUT DEFAULTS

         BEFORE FIELD tipo_retiro
            NEXT FIELD movimiento

         AFTER FIELD movimiento
         IF g_reg.movimiento IS NULL THEN
            ERROR "  MOVIMIENTO DE RETIRO NO PUEDE SER NULO  "
            NEXT FIELD movimiento
         END IF

         AFTER FIELD descripcion
            IF g_reg.descripcion IS NULL THEN
               ERROR "  DESCRIPCION DEL RETIRO NO DEBE SER NULO   "
               NEXT FIELD descripcion
            END IF

         AFTER FIELD nom_archivo 
            IF g_reg.nom_archivo IS NULL THEN
               ERROR "  NOMBRE DEL ARCHIVO NO DEBE SER NULO   "
               NEXT FIELD nom_archivo
            END IF

         CALL Pregunta()
         IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_retiro SET
                   movimiento  = g_reg.movimiento,  
                   descripcion = g_reg.descripcion, 
                   nom_archivo = g_reg.nom_archivo 
            WHERE tipo_retiro = g_reg.tipo_retiro
            ERROR "  REGISTRO MODIFICADO  "
            SLEEP 1
            ERROR ""
            CALL Inicializa()
	    CLEAR FORM
         ELSE
            ERROR "  PROCESO DE MODIFICAR CANCELADO  "
            SLEEP 1
            ERROR ""
            INITIALIZE g_reg.* TO NULL
            CLEAR FORM
         END IF
         EXIT INPUT
      ON KEY (INTERRUPT)
         CALL Inicializa()
	 CLEAR FORM
         EXIT INPUT
      END INPUT
   ELSE
      ERROR "  ARCHIVO DE RETIROS... VACIO  "
   END IF
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0812" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) ELIMINA                                            (CTRL-C) SALIR     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el RETIRO a ELIMINAR                         " AT 2,1
      DISPLAY " TABM081              CATAGOLO  TIPO  DE  RETIRO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT BY NAME cla_where ON tab_retiro.*
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_retiro WHERE ",
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
               LET g_reg.tipo_retiro        = l_record[pos].tipo_retiro
               LET g_reg.movimiento      = l_record[pos].movimiento
               LET g_reg.descripcion     = l_record[pos].descripcion
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_reg.nom_archivo     = l_record[pos].nom_archivo
               LET g_reg.usuario         = l_record[pos].usuario    
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "  DEBE ELEGIR UN REGISTRO  "
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "  ARCHIVO DE RETIROS... VACIO  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME g_reg.tipo_retiro,
		      g_reg.movimiento,
   		      g_reg.descripcion,
		      g_reg.nom_archivo
      CALL Pregunta()
      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_retiro
         WHERE tipo_retiro = g_reg.tipo_retiro
         ERROR "  REGISTRO ELIMINADO  "
         SLEEP 1
         ERROR ""
	 CALL Inicializa()
         CLEAR FORM 
      ELSE
         ERROR "  PROCESO CANCELADO  "
         SLEEP 1
         ERROR ""
	 CALL Inicializa()
         CLEAR FORM 
      END IF

      CALL Inicializa()
   ELSE
      ERROR "  ARCHIVO DE RETIROS... VACIO  "
   END IF
   CLEAR SCREEN
END FUNCTION


-------------------------------------------------------------------------
FUNCTION Pregunta()
   PROMPT "ESTA SEGURO (S/N) ? " FOR CHAR aux_pausa
END FUNCTION
-------------------------------------------------------------------------
{

FUNCTION impresion(pos)
   DEFINE i, pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
                 usuario CLIPPED,".IMPTRECHA",
                 hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabretiro To PRINTER


   FOR i = 1 TO (pos+1)
       LET g_reg.tipo_retiro       = l_record[i].codigo
       LET g_reg.descripcion   = l_record[i].des_tipo_retiro

       IF g_reg.tipo_retiro   IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabretiro(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabretiro

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
----------------------------------------------------------------------
REPORT rpt_tabretiro(g_reg)
   DEFINE g_reg              RECORD
          tipo_retiro           CHAR(1),
          descripcion       CHAR(60),
          fecha_actualiza    DATE,
          usuario            CHAR(8)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," xxxxxx ",
               COLUMN 24," LISTADO DE CATALOGO DE RETIROS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 5,"CODIGO",
               COLUMN 30,"DESCRIPCION RETIRO"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 7,g_reg.tipo_retiro,
               COLUMN 15,g_reg.descripcion
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT

}
