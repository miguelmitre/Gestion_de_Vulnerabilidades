################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa TABM040  => MANTENEDOR ARCHIVO DE RED.
#Sistema           => COMM. 					               #
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ. 			       #
#Fecha             => 22 MARZO 1999 . 	          			       #
#By                => GERARDO ALFONSO VEGA PAREDES.    			       #
#Fecha             => 16 enero 2001.  	          			       #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
   DEFINE g_reg RECORD 
		red_cod	  SMALLINT,
		red_desc	  CHAR(50),
      factualiza DATE,
      usuario    CHAR(8)
	END RECORD
	DEFINE HOY			DATE
      
        DEFINE
            sw_1                        SMALLINT
   DEFINE g_param RECORD LIKE com_parametro.*
	DEFINE vusuario CHAR(08)
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0161" ATTRIBUTE( BORDER)
	DISPLAY " COMM016                MANTENEDOR  DE  RED                                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "MANTENEDOR DE RED"
		COMMAND "Agrega" "Agrega red"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta red"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica red"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina red"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	INITIALIZE g_reg.red_cod,g_reg.red_desc  TO NULL
	DISPLAY BY NAME g_reg.red_cod,g_reg.red_desc
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(green)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,green)
        LET g_reg.red_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.red_cod,g_reg.red_desc
	      BEFORE FIELD red_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(red_cod) INTO g_reg.red_cod FROM tab_red
                     IF g_reg.red_cod = 0 OR g_reg.red_cod IS NULL THEN
	                LET g_reg.red_cod = 1
	             ELSE
			LET g_reg.red_cod = g_reg.red_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.red_cod,g_reg.red_desc
                  END IF
	      AFTER FIELD red_cod
		    IF g_reg.red_cod IS NULL THEN
		       ERROR "Codigo de red NO puede ser nulo"
		       NEXT FIELD red_cod
		    END IF
                    SELECT "X" FROM tab_red
                    WHERE red_cod = g_reg.red_cod
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD red_cod
                    END IF 
	      BEFORE FIELD red_desc
		     IF g_reg.red_cod IS NULL OR  g_reg.red_cod = 0 THEN
		        ERROR "Codigo de red NO puede ser nulo"
			NEXT FIELD  red_cod
		     END IF 
              AFTER FIELD red_desc
		     IF g_reg.red_desc IS NULL THEN
		        ERROR "Descripcion de red NO puede ser nula"
		        NEXT FIELD  red_desc
		     END IF 
		     SELECT "X" FROM tab_red
		     WHERE red_desc = g_reg.red_desc
		     IF STATUS <> NOTFOUND THEN
			ERROR "red ya Ingresado"
			NEXT FIELD red_cod
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.red_cod IS NULL THEN
		        ERROR "Codigo de red NO puede ser NULO"
		        NEXT FIELD red_cod
		     END IF
		     IF g_reg.red_desc IS NULL THEN
		        ERROR "Descripcion de red NO puede ser NULO"
                        NEXT FIELD red_desc
		     END IF
		     SELECT "X" FROM tab_red
		     WHERE red_desc = g_reg.red_desc
		     IF STATUS <> NOTFOUND THEN
			ERROR "Red ya Ingresado"
			NEXT FIELD red_cod
		     END IF

			SELECT user
			INTO   vusuario
			FROM   com_parametro

			LET g_reg.usuario = vusuario
			LET g_reg.factualiza = TODAY

                     INSERT INTO tab_red VALUES (g_reg.*)
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD red_cod
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion  CHAR(50)
         END RECORD
         DEFINE pos                SMALLINT   
         DECLARE cursor_1 CURSOR FOR
	 SELECT * FROM tab_red
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0162" ATTRIBUTE( BORDER)
	    DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,green)
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,green)
	    DISPLAY "                        R E D                                                     " AT 3,1 ATTRIBUTE(REVERSE,green) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	    END DISPLAY
	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE RED... VACIO"
	 END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()
        DEFINE l_record   ARRAY[300] OF RECORD
               codigo         SMALLINT,
               descripcion  CHAR(50)
        END RECORD
        DEFINE pos                SMALLINT   

        DECLARE cursor_2 CURSOR FOR
	SELECT * FROM tab_red
	ORDER BY 1
	LET pos = 1
	FOREACH cursor_2 INTO l_record[pos].*
	        LET pos = pos + 1
        END FOREACH
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0162" ATTRIBUTE( BORDER)
	   DISPLAY " ESCOJA CON < ENTER > LA RED A MODIFICAR" AT 1,1 ATTRIBUTE(REVERSE,green)
           DISPLAY " MODIFICA " AT 1,54 ATTRIBUTE(REVERSE,green)
	   DISPLAY "                              R E D                                        " AT 3,1 ATTRIBUTE(REVERSE,green)
	   DISPLAY ARRAY l_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
		      LET g_reg.red_cod = l_record[pos].codigo
                      LET g_reg.red_desc = l_record[pos].descripcion
                      EXIT DISPLAY
                   ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(green)
           DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,green)
	   INPUT BY NAME  g_reg.red_cod,g_reg.red_desc WITHOUT DEFAULTS 
                 BEFORE FIELD red_cod
		       NEXT FIELD red_desc
                 AFTER FIELD red_desc
		       IF g_reg.red_desc IS NULL THEN
                          ERROR "Descripcion de red NO puede ser nula"
                          NEXT FIELD  red_desc
                       END IF 
	         ON KEY ( ESC )
                    IF g_reg.red_desc IS NULL THEN
		       ERROR "Descripcion de red NO puede ser NULO"
                       NEXT FIELD red_desc
		    END IF
                    UPDATE tab_red SET
                           red_desc = g_reg.red_desc
                    WHERE red_cod = g_reg.red_cod
		    ERROR "REGISTRO MODIFICADO" SLEEP 1
		    ERROR ""
                    CALL Inicializa()
		    EXIT INPUT
	         ON KEY ( INTERRUPT )
                        CALL Inicializa()
                        EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE RED.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion  CHAR(50)
         END RECORD
         DEFINE pos                SMALLINT   

         DECLARE cursor_3 CURSOR FOR
	 SELECT * FROM tab_red
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	                  LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0162" ATTRIBUTE( BORDER)
            DISPLAY " ELIMINA " AT 1,55 ATTRIBUTE(REVERSE,green)
	    DISPLAY " ESCOJA CON < ENTER > LA RED A ELIMINAR" AT 1,1 ATTRIBUTE(REVERSE,green)
	   DISPLAY "                              R E D                                  " AT 3,1 ATTRIBUTE(REVERSE,green)
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_reg.red_cod = l_record[pos].codigo
                       LET g_reg.red_desc = l_record[pos].descripcion
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Elimina        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(green)
           DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,green)
	   DISPLAY BY NAME  g_reg.red_cod,g_reg.red_desc
           CALL Pregunta()
           IF aux_pausa MATCHES "[Ss]" THEN
              DELETE FROM tab_red
              WHERE red_cod = g_reg.red_cod
              ERROR "REGISTRO ELIMINADO" SLEEP 1
           ELSE
              ERROR "ELIMINAR CANCELADO" SLEEP 1
           END IF
           ERROR ""
           CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE RED.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
