################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa TABM017  => CATALOGO ARCHIVO com_nivel (CARGOS)  		       #
#Fecha             => DICIEMBRE    1996. 				       #
#Por               => ADOLFO DE LA MAZA            			       #
#Fecha modifica    => 13 DE DICIEMBRE DE 1999                                  #
#Por               => ALEJANDRO CAMPOS SUAREZ                                  #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
         DEFINE g_param_dis	RECORD LIKE seg_modulo.*

	 DEFINE aux_pausa	CHAR(1),
		sw_1		SMALLINT,
		usuario		CHAR(8),
		hoy		DATE,
		pos		INTEGER,
		sel_where	CHAR(30000),
		cla_where	CHAR(30000),
		g_impre		CHAR(300),
		g_lista		CHAR(300)

         DEFINE g_reg		RECORD 
		nivel_cod	SMALLINT,
		nivel_desc	CHAR(40)
	 END RECORD
 
         DEFINE l_record	ARRAY[30000] OF RECORD
		codigo 		SMALLINT,
		descripcion	CHAR(40)
	 END RECORD

END GLOBALS
################################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()
	CALL proceso()
END MAIN
################################################################################
FUNCTION inicio()
	SELECT USER,*
	INTO   usuario
	FROM   glo_parametro

	SELECT ruta_listados
	INTO   g_param_dis.ruta_listados
	FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
################################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0171" ATTRIBUTE( BORDER)
	DISPLAY " TABM017             CATALOGO DE NIVELES   (CARGOS)                           " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO DE com_nivel "
			   COMMAND "Agrega" "Agrega Niveles"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Niveles"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Niveles"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Niveles"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                  " AT 1,1 ATTRIBUTE(BOLD)
  	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.nivel_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD nivel_cod
                 IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(nivel_cod) 
                     INTO g_reg.nivel_cod 
                     FROM tab_nivel
                     IF g_reg.nivel_cod = 0 OR g_reg.nivel_cod IS NULL THEN
	                LET g_reg.nivel_cod = 1
	             ELSE
			LET g_reg.nivel_cod = g_reg.nivel_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                 END IF
	      AFTER FIELD nivel_cod
		    IF g_reg.nivel_cod IS NULL THEN
		       ERROR "Codigo de com_nivel NO puede ser nulo"
		       NEXT FIELD  nivel_cod
		    END IF

                    SELECT "X" 
		    FROM tab_nivel
                    WHERE nivel_cod = g_reg.nivel_cod
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD nivel_cod
                    END IF 
	      BEFORE FIELD nivel_desc
		     IF g_reg.nivel_cod IS NULL OR  g_reg.nivel_cod = 0 THEN
		        ERROR "Codigo de com_nivel NO puede ser nulo"
			NEXT FIELD  nivel_cod
		     END IF 
              AFTER FIELD nivel_desc
		     IF g_reg.nivel_desc IS NULL THEN
		        ERROR "Descripcion de com_nivel NO puede ser nula"
		        NEXT FIELD  nivel_desc
		     END IF 
                     SELECT "X" FROM tab_nivel
                     WHERE nivel_desc = g_reg.nivel_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "NIVELES Ya Ingresado"
	                NEXT FIELD nivel_cod
                     END IF 
	      ON KEY ( ESC )
		     IF g_reg.nivel_cod IS NULL THEN
		        ERROR "Codigo de Niveles NO puede ser NULO"
		        NEXT FIELD nivel_cod
		     END IF
		     IF g_reg.nivel_desc IS NULL THEN
		        ERROR "Descripcion de Niveles  NO puede ser NULO"
                        NEXT FIELD nivel_desc
		     END IF
                     SELECT "X" FROM tab_nivel
                     WHERE nivel_desc = g_reg.nivel_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "Nivel Ya Ingresado"
	                NEXT FIELD nivel_cod
                     END IF 
                     INSERT INTO tab_nivel VALUES ( g_reg.* ) 
		     ERROR "REGISTRO INGRESADO" SLEEP 2
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD nivel_cod
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()   
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0172" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta            (Ctrl-p) Imprimir               (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                               N I V E L E S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON nivel_cod,nivel_desc FROM nivel_cod,nivel_desc
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
		SLEEP 2
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

	   LET sel_where = "SELECT * FROM tab_nivel WHERE ",cla_where CLIPPED,
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
                       ERROR "PROCESANDO INFORMACION..."
		       CALL impresion(pos)
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE com_nivel... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
	   END IF
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0172" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                    Escoja con <ENTER> el nivel a modificar                 " AT 2,1
           DISPLAY "                                N I V E L E S                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON nivel_cod,nivel_desc FROM nivel_cod,nivel_desc
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
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
	   LET sel_where = "SELECT * FROM tab_nivel WHERE ",cla_where CLIPPED,
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
		       LET g_reg.nivel_cod = l_record[pos].codigo
                       LET g_reg.nivel_desc = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE com_nivel... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   INPUT BY NAME g_reg.* WITHOUT DEFAULTS
	      BEFORE FIELD nivel_cod
		NEXT FIELD nivel_desc

	      AFTER FIELD nivel_desc
		IF g_reg.nivel_desc IS NULL THEN
		   ERROR "Descipcion de nivel no debe ser nulo."
		   NEXT FIELD nivel_desc
		END IF

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
		   UPDATE tab_nivel SET
			nivel_desc = g_reg.nivel_desc
		   WHERE nivel_cod = g_reg.nivel_cod
	
		   ERROR "REGISTRO MODIFICADO"
		   SLEEP 2
		   ERROR ""

		   CALL Inicializa()
		ELSE
		   ERROR "PROCESO DE MODIFICAR CANCELADO."
		   SLEEP 2
		   ERROR ""
		END IF
		EXIT INPUT
	   ON KEY (INTERRUPT)
		CALL Inicializa()
		EXIT INPUT
	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE com_nivel... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0172" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                    Escoja con <ENTER> el nivel a eliminar                  " AT 2,1
           DISPLAY "                                N I V E L E S                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON nivel_cod,nivel_desc FROM nivel_cod,nivel_desc
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
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

	   LET sel_where = "SELECT * FROM tab_nivel WHERE ",cla_where CLIPPED,
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
		    ON KEY (CONTROL-M)
		       LET pos = ARR_CURR()
		       LET g_reg.nivel_cod = l_record[pos].codigo
                       LET g_reg.nivel_desc = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE com_nivel... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   DISPLAY BY NAME g_reg.*
	      CALL Pregunta()

	   IF aux_pausa MATCHES "[Ss]" THEN
	      DELETE FROM tab_nivel
	      WHERE nivel_cod = g_reg.nivel_cod

	      ERROR "REGISTRO ELIMINADO."
	      SLEEP 2
	      ERROR ""
	   ELSE
	      ERROR "PROCESO CANCELADO."
	      SLEEP 2
	      ERROR ""
	   END IF

	   CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE com_nivel... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
	DEFINE i, pos SMALLINT

	LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",usuario CLIPPED,".IMPTNIVELES",hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_tabnivel To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.nivel_cod = l_record[i].codigo
	   LET g_reg.nivel_desc = l_record[i].descripcion

	   IF g_reg.nivel_cod IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_tabnivel(g_reg.*)
	END FOR

	FINISH REPORT rpt_tabnivel

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tabnivel(g_reg)
         DEFINE g_reg		RECORD 
		nivel_cod	SMALLINT,
		nivel_desc  	CHAR(40)
	 END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM017 ",
               COLUMN 24," LISTADO DE CATALOGO DE com_nivel ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION NIVEL"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.nivel_cod ,
               COLUMN 25,g_reg.nivel_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
