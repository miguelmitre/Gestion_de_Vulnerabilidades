################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa COMM006  => CATALOGO DE DIRECCION COMERCIAL.
#Sistema           => COM. 					               #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Fecha             => 27 Noviembre 1996. 				       #
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha             => 16 enero 2001.     				       #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
              DEFINE g_reg			RECORD 
		  coduni_n5		CHAR(10),
		  nombre_uni_n5	  	CHAR(40),
		  uni_superior_n5	CHAR(10)
	END RECORD
	DEFINE HOY			DATE

        DEFINE
            sw_1                        SMALLINT
    DEFINE
       vusuario CHAR(8)
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

	LET HOY = DATE
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0041" ATTRIBUTE( BORDER)
	DISPLAY " COMM004          MANTENEDOR DE DIRECCION COMERCIAL                             " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	
	MENU "DIRECCION COMERCIAL"
		COMMAND "Agrega" "Agrega Direccion Comercial"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Direccion Comercial"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Direccion Comercial"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Direccion Comercial"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION Inicializa()
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
        LET sw_1 = 0
        SELECT USER
          INTO vusuario
          FROM com_parametro
END FUNCTION
################################################################################
FUNCTION Agrega()
   DEFINE cuantos CHAR(50)

	CALL Inicializa()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 2,69 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
        LET g_reg.nombre_uni_n5 = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD coduni_n5
               SELECT COUNT(*) into cuantos FROM com_nivel5
                 IF cuantos >= 1 THEN
                 ERROR "No se pueden adicionar registros, primero borre o actualize"
               EXIT INPUT
                 ELSE
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(coduni_n5) INTO g_reg.coduni_n5 FROM com_nivel5
                     IF g_reg.coduni_n5 = 0 OR g_reg.coduni_n5 IS NULL THEN
	                LET g_reg.coduni_n5 = 1
	             ELSE
			LET g_reg.coduni_n5 = g_reg.coduni_n5 + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
            END IF
	      AFTER FIELD coduni_n5
		    IF g_reg.coduni_n5 IS NULL THEN
		       ERROR "Codigo de Direccion NO puede ser nulo"
		       NEXT FIELD  coduni_n5
		    END IF
                    SELECT "X" FROM com_nivel5
                    WHERE coduni_n5 = g_reg.coduni_n5
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD coduni_n5
                    END IF 
	      BEFORE FIELD nombre_uni_n5
		     IF g_reg.coduni_n5 IS NULL OR  g_reg.coduni_n5 = 0 THEN
		        ERROR "Codigo de Direccion NO puede ser nulo"
			NEXT FIELD  coduni_n5
		     END IF 
              AFTER FIELD nombre_uni_n5
		     IF g_reg.nombre_uni_n5 IS NULL THEN
		        ERROR "Descripcion de Direccion NO puede ser nula"
		        NEXT FIELD  nombre_uni_n5
		     END IF 
		     SELECT "X" FROM com_nivel5
		     WHERE nombre_uni_n5 = g_reg.nombre_uni_n5
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Direccion ya Ingresada"
			NEXT FIELD coduni_n5
		     END IF
	      BEFORE FIELD uni_superior_n5
		     IF g_reg.nombre_uni_n5 IS NULL THEN
		        ERROR "Descripcion de Direccion NO puede ser nula"
		        NEXT FIELD  nombre_uni_n5
		     END IF 
              AFTER FIELD uni_superior_n5
		     IF g_reg.uni_superior_n5 IS NULL THEN
		        ERROR "Direccion Abreviada  NO puede ser nula"
		        NEXT FIELD  uni_superior_n5
		     END IF 
		     SELECT "X" FROM com_nivel5
		     WHERE uni_superior_n5 = g_reg.uni_superior_n5
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Direccion Abreviada ya Ingresada"
			NEXT FIELD uni_superior_n5
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.coduni_n5 IS NULL THEN
		        ERROR "Codigo de Direccion NO puede ser NULO"
		        NEXT FIELD coduni_n5
		     END IF
		     IF g_reg.nombre_uni_n5 IS NULL THEN
		        ERROR "Descripcion de Direccion NO puede ser NULO"
                        NEXT FIELD nombre_uni_n5
		     END IF
		     IF g_reg.uni_superior_n5 IS NULL THEN
		        ERROR "Direccion Abreviada  NO puede ser nula"
		        NEXT FIELD  uni_superior_n5
		     END IF 
		     SELECT "X" FROM com_nivel5
		     WHERE nombre_uni_n5 = g_reg.nombre_uni_n5
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Direccion ya Ingresada"
			NEXT FIELD coduni_n5
		     END IF
				
            INSERT INTO com_nivel5 VALUES(g_reg.*,TODAY,vusuario,1) 
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                    # CALL Inicializa()
		    # NEXT FIELD coduni_n5
                    #ON KEY (INTERRUPT)
                    #   CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         CHAR(10),
                descripcion  CHAR(40),
                abrevia      CHAR(10)
         END RECORD
         DEFINE pos                SMALLINT   
         DECLARE cursor_1 CURSOR FOR
	 SELECT * FROM com_nivel5
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,2 WITH FORM "COMM0042" ATTRIBUTE( BORDER)
	    DISPLAY " CONSULTA " AT 1,64 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                        DIRECCION COMERCIAL                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	    END DISPLAY
	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE DIRECCION COMMERCIAL.... VACIO"
	 END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()
        DEFINE l_record   ARRAY[300] OF RECORD
               codigo         CHAR(10),
               descripcion  CHAR(40),
               abrevia       CHAR(10)
        END RECORD
        DEFINE cuantos  CHAR(70)
        DEFINE pos                SMALLINT   

        DECLARE cursor_2 CURSOR FOR
	SELECT * FROM com_nivel5
	ORDER BY 1
	LET pos = 1
	FOREACH cursor_2 INTO l_record[pos].*
	        LET pos = pos + 1
        END FOREACH
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 7,2 WITH FORM "COMM0042" ATTRIBUTE( BORDER)
	   DISPLAY " ESCOJA CON < ENTER > LA DIRECCION COMERCIAL" AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY " MODIFICA " AT 1,64 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                          DIRECCION COMERCIAL                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY ARRAY l_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
		      LET g_reg.coduni_n5 = l_record[pos].codigo
                      LET g_reg.nombre_uni_n5 = l_record[pos].descripcion
                      LET g_reg.uni_superior_n5 = l_record[pos].abrevia
                      EXIT DISPLAY
                   ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
	   INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
                 BEFORE FIELD coduni_n5
                 NEXT FIELD nombre_uni_n5 	
                 AFTER FIELD nombre_uni_n5
		       IF g_reg.nombre_uni_n5 IS NULL THEN
                          ERROR "Descripcion de Direccion NO puede ser nula"
                          NEXT FIELD  nombre_uni_n5
                       END IF 
                 AFTER FIELD uni_superior_n5
		       IF g_reg.uni_superior_n5 IS NULL THEN
                          ERROR "Descripcion de Direccion NO puede ser nula"
                          NEXT FIELD  uni_superior_n5
                       END IF 
	         ON KEY ( ESC )
                    IF g_reg.nombre_uni_n5 IS NULL THEN
		       ERROR "Descripcion de Direccion NO puede ser NULO"
                       NEXT FIELD nombre_uni_n5
		    END IF
                    UPDATE com_nivel5 SET
                           nombre_uni_n5 = g_reg.nombre_uni_n5,
                           uni_superior_n5 = g_reg.uni_superior_n5
                    WHERE coduni_n5 = g_reg.coduni_n5
		    ERROR "REGISTRO MODIFICADO" SLEEP 1
		    ERROR ""
                    CALL Inicializa()
		    EXIT INPUT
	         ON KEY ( INTERRUPT )
                        CALL Inicializa()
                        EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE DIRECCION COMERCIAL .... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         CHAR(10),
                descripcion  CHAR(40),
                abrevia      CHAR(10)
         END RECORD
         DEFINE pos                SMALLINT   

         DECLARE cursor_3 CURSOR FOR
	 SELECT * FROM com_nivel5
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	                  LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,2 WITH FORM "COMM0042" ATTRIBUTE( BORDER)
            DISPLAY " " AT 1,64 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " ELIMINA  " AT 1,64 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " ESCOJA CON < ENTER > LA DIRECCION COMERCIAL  A ELIMINAR" AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                         DIRECCION COMERCIAL                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_reg.coduni_n5 = l_record[pos].codigo
                       LET g_reg.nombre_uni_n5 = l_record[pos].descripcion
                       LET g_reg.uni_superior_n5 = l_record[pos].abrevia
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Elimina        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY BY NAME  g_reg.*
           CALL Pregunta()
           IF aux_pausa MATCHES "[Ss]" THEN
              DELETE FROM com_nivel5
              WHERE coduni_n5 = g_reg.coduni_n5
              ERROR "REGISTRO ELIMINADO" SLEEP 1
           ELSE
              ERROR "ELIMINAR CANCELADO" SLEEP 1
           END IF
           ERROR ""
           CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE DIRECCION COMERCIAL  ... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ?" 
        attribute (reverse)
        FOR CHAR aux_pausa
        attribute (reverse)
END FUNCTION
################################################################################
