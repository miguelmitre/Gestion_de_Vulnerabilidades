######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		     #
#Propietario       => E.F.P.                                         #
#Programa TAAM001  => CATALOGO DE SIEFORES                           #
#Fecha             => 11 Noviembre 1999.  			     #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Sistema           => TAA. 					     #
######################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis         RECORD LIKE seg_modulo.*

   DEFINE g_reg			RECORD 
          afore_cod		SMALLINT,
	  siefore_cod	  	SMALLINT,
	  siefore_desc	 	CHAR(08),
          fecha_actualiza       DATE	
   END RECORD

   DEFINE l_record   ARRAY[300] OF RECORD
          afore_cod		SMALLINT,
	  siefore_cod	  	SMALLINT,
	  siefore_desc	 	CHAR(08),
          fecha_actualiza       DATE
   END RECORD

   DEFINE vafore_desc    char(40)

   DEFINE l_record1   ARRAY[500] OF RECORD
          afore_cod        CHAR(10),
          afore_desc       CHAR(40)
   END RECORD

   DEFINE
          hoy                   DATE,
          aux_pausa             CHAR(01),
          sw_1                  SMALLINT,
          g_usuario               CHAR(08),
          siono                 CHAR(01),
          pos                   SMALLINT,
          cla_where             CHAR(200),
          sel_where             CHAR(200),
          g_lista               CHAR(300),
          g_impre               CHAR(300),
          hora                  CHAR(08)

END GLOBALS

MAIN

   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   CALL inicio()
   CALL proceso()

END MAIN

FUNCTION inicio()

   SELECT ruta_listados, user
   INTO   g_param_dis.ruta_listados, g_usuario
   FROM   seg_modulo
   WHERE modulo_cod = 'tab'

   LET HOY = DATE

END FUNCTION

FUNCTION proceso()

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TAAM0011" ATTRIBUTE( BORDER)
   DISPLAY " TAAM001                      CATALOGO DE SIEFORES                             " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "SIEFORES "
      COMMAND "Agrega" "Agrega Siefores"
         CALL Agrega()
      COMMAND "Consulta" "Consulta Siefores"
         CALL Consulta()
      COMMAND "Modifica" "Modifica Siefores"
         CALL Modifica()
      COMMAND "Elimina" "Elimina Siefores"
         CALL Elimina()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()

   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
   INITIALIZE vafore_desc TO NULL
   --DISPLAY BY NAME vafore_desc
   LET sw_1 = 0

END FUNCTION

FUNCTION Agrega()

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                     AGREGA          " AT 1,1 ATTRIBUTE(GREEN)

   INPUT BY NAME  g_reg.*
      AFTER FIELD afore_cod
         IF g_reg.afore_cod IS NULL THEN

            DECLARE afore CURSOR FOR

            SELECT afore_cod, afore_desc 
            FROM tab_afore
     	    ORDER BY 1

	    LET pos = 1
            FOREACH afore INTO l_record1[pos].*
	       LET pos = pos + 1
            END FOREACH

	    IF (pos-1) >= 1 THEN
	       CALL  SET_COUNT(pos-1)
	       OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0013" ATTRIBUTE( BORDER)
	       DISPLAY " AGREGA " AT 1,69 
	       DISPLAY " (Ctrl-C) Salir " AT 1,1 
	       DISPLAY "                                 A F O R E S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	       DISPLAY ARRAY l_record1 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
	             LET g_reg.afore_cod  = l_record1[pos].afore_cod
                     LET vafore_desc       = l_record1[pos].afore_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
	             ERROR "Usted debe escoger un registro"
                     LET pos = ARR_CURR()
                     EXIT DISPLAY
	          END DISPLAY
	          CLOSE WINDOW ventana_2
                  DISPLAY BY NAME g_reg.afore_cod,vafore_desc
            ELSE
	       ERROR "ARCHIVO DE AFORE VACIO"
	    END IF
         ELSE
       	    SELECT afore_desc 
            INTO vafore_desc
            FROM tab_afore
            WHERE afore_cod = g_reg.afore_cod
	    ORDER BY 1
            --DISPLAY BY NAME vafore_desc
         END IF

         SELECT "X"
         FROM   tab_afore
         WHERE  afore_cod = g_reg.afore_cod

         IF STATUS = NOTFOUND THEN
            ERROR "Codigo Afore NO existe ..."
            NEXT FIELD afore_cod
         END IF

      AFTER FIELD siefore_cod
         IF g_reg.siefore_cod IS NULL THEN
            ERROR "Codigo Siefore NO puede ser nulo"
            NEXT FIELD  siefore_cod
         END IF

         SELECT "X"
         FROM   tab_siefore
         WHERE  afore_cod = g_reg.afore_cod
         AND    siefore_cod = g_reg.siefore_cod

         IF STATUS <> NOTFOUND THEN
            ERROR "Siefore YA ingresada ..."
            NEXT FIELD siefore_cod
         END IF

      AFTER FIELD siefore_desc
	 IF g_reg.siefore_desc IS NULL THEN
	    ERROR "Descripcion de la Siefore NO puede ser nula"
	    NEXT FIELD  siefore_desc
	 END IF

         SELECT "X"
         FROM   tab_siefore
         WHERE  afore_cod = g_reg.afore_cod
         AND    siefore_desc = g_reg.siefore_desc

         IF STATUS <> NOTFOUND THEN
            ERROR "Descripcion Siefore YA ingresada ..."
            NEXT FIELD siefore_desc
         END IF

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
      ON KEY (ESC)
         IF g_reg.afore_cod IS NULL THEN
            ERROR "Codigo Afore NO puede ser nulo"
            NEXT FIELD afore_cod
         END IF

         IF g_reg.siefore_cod IS NULL THEN
            ERROR "Codigo Siefore NO puede ser nulo"
            NEXT FIELD  siefore_cod
         END IF

         IF g_reg.siefore_desc IS NULL THEN
            ERROR "Descripcion de la Siefore NO puede ser nula"
            NEXT FIELD  siefore_desc
         END IF

         LET g_reg.fecha_actualiza = TODAY

         INSERT INTO tab_siefore VALUES (g_reg.*,g_usuario) 

         ERROR "REGISTRO INGRESADO"
         SLEEP 2
         ERROR ""

         CALL Inicializa()
         EXIT INPUT
           
   END INPUT
   CLEAR SCREEN

END FUNCTION

FUNCTION Consulta()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0012" ATTRIBUTE( BORDER)
      DISPLAY " (ESC) Todos     (ENTER) Consulta     (Ctrl-C) Salir     (Ctrl-P) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                                S I E F O R E S                                " AT 3,1 ATTRIBUTE(REVERSE,GREEN) 

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON afore_cod FROM afore_cod
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_siefore WHERE ",cla_where CLIPPED,
                      "ORDER BY 1 " 
   
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)	
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SIEFORE ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN

END FUNCTION

FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0012" ATTRIBUTE(BORDER)
      DISPLAY " (ESC) Todos                 (ENTER) Consulta                (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "               Escoja con < ENTER > la Siefore a modificar                     " AT 2,1 
      DISPLAY "                                   S I E F O R E                               " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON afore_cod FROM afore_cod
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_siefore WHERE ",cla_where CLIPPED,
                      "ORDER BY 1 " 
   
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.afore_cod       = l_record[pos].afore_cod
               LET g_reg.siefore_cod     = l_record[pos].siefore_cod
               LET g_reg.siefore_desc    = l_record[pos].siefore_desc
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escoger un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SIEFORE ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,GREEN)

      INPUT BY NAME  g_reg.afore_cod,g_reg.siefore_cod,
                     g_reg.siefore_desc WITHOUT DEFAULTS 
         BEFORE FIELD afore_cod
            NEXT FIELD siefore_desc 

         AFTER FIELD siefore_desc
	    IF g_reg.siefore_desc IS NULL THEN 
	       ERROR "Descripcion de la Siefore NO puede ser nulo"
	       NEXT FIELD  siefore_desc
	    END IF

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN     
               UPDATE tab_siefore 
               SET    siefore_desc    = g_reg.siefore_desc,
                      fecha_actualiza = TODAY,
                      usuario         = g_usuario  
               WHERE  afore_cod       = g_reg.afore_cod
               AND    siefore_cod     = g_reg.siefore_cod

	       ERROR "REGISTRO MODIFICADO"
               SLEEP 2
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR, CANCELADO"
               SLEEP 2
            END IF
            ERROR "" 
	    EXIT INPUT
	 ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
  	 END INPUT
   ELSE
      ERROR "PROCESO DE MODIFICAR, CANCELADO"
   END IF
   CLEAR SCREEN

END FUNCTION

FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0012" ATTRIBUTE( BORDER)
      DISPLAY " (ESC) Todos                 (ENTER) Consulta                (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                Escoja con < ENTER > la Siefore a borrar                      " AT 2,1 
      DISPLAY "                               S I E F O R E S                                 " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON afore_cod FROM afore_cod
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_siefore WHERE ",cla_where CLIPPED,
                      "ORDER BY 1 " 
   
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.afore_cod       = l_record[pos].afore_cod
               LET g_reg.siefore_cod     = l_record[pos].siefore_cod
               LET g_reg.siefore_desc    = l_record[pos].siefore_desc
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escoger un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SIEFORE ... NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,GREEN)

      DISPLAY BY NAME  g_reg.afore_cod,
                       g_reg.siefore_cod,
                       g_reg.siefore_desc

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_siefore
            WHERE afore_cod = g_reg.afore_cod
            AND siefore_cod = g_reg.siefore_cod

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE SIEFORES .... VACIO"
   END IF
   CLEAR SCREEN

END FUNCTION

FUNCTION Pregunta()

   PROMPT "Esta seguro S/N ? " FOR aux_pausa

END FUNCTION

FUNCTION impresion(pos)

   DEFINE i, pos     SMALLINT

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
               ".IMPTABSIEFORE",HOY USING "dd-mm-yyyy","_",hora CLIPPED
  
   START REPORT rpt_tabsiefore TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.afore_cod           = l_record[i].afore_cod
       LET g_reg.siefore_cod         = l_record[i].siefore_cod
       LET g_reg.siefore_desc        = l_record[i].siefore_desc
       LET g_reg.fecha_actualiza     = l_record[i].fecha_actualiza

       IF g_reg.afore_cod IS NULL  THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabsiefore(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabsiefore
  
   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp -d afoafi_1 ",g_impre
   RUN g_lista

END FUNCTION

REPORT rpt_tabsiefore(g_reg)
  
   DEFINE g_reg			RECORD 
          afore_cod		SMALLINT,
	  siefore_cod	  	SMALLINT,
	  siefore_desc	 	CHAR(08),
          fecha_actualiza       DATE	
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
   PAGE HEADER
      PRINT COLUMN 02," TAAM001 ",
            COLUMN 24," LISTADO DE CATALOGO DE SIEFORES ",
            COLUMN 67,TODAY USING "dd-mm-yyyy"
      SKIP 2 LINE

      PRINT COLUMN 05,"COD. AFORE",
            COLUMN 19,"COD. SIEFORE",
            COLUMN 36,"DESC. SIEFORE",
            COLUMN 57,"FECHA ALTA"
      SKIP 1 LINE
   ON EVERY ROW
      PRINT COLUMN 09,g_reg.afore_cod USING "&&&",
            COLUMN 23,g_reg.siefore_cod USING "&&",
            COLUMN 38,g_reg.siefore_desc,
            COLUMN 59,g_reg.fecha_actualiza USING "dd-mm-yyyy"
   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
   ON LAST ROW
      SKIP 4 LINE
      PRINT COLUMN 01," Total de registros : " ,COUNT(*) USING "<<<<<"
END REPORT
