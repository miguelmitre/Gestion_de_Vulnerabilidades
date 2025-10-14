################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P. 
#Programa COMM011  => MANTENEDOR ARCHIVO DE PERCEPCIONES 
#Sistema           => COM. 					               #
#By                => GERARDO ALFONOS VEGA PAREDES.			       #
#Fecha             =>  1 diciembre 1997.				       #
#By                => GERARDO ALFONOS VEGA PAREDES.			       #
#Fecha             =>  16 enero 2001.     		       #
################################################################################
DATABASE safre_af
GLOBALS
 
	DEFINE aux_pausa			CHAR(1)
              DEFINE g_reg   RECORD 
		  percep_cod		SMALLINT,
		  percep_desc  	CHAR(50)
	END RECORD
	DEFINE HOY			DATE

        DEFINE
            sw_1                        SMALLINT,
            vaccion smallint,
            opc CHAR(01)
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

	LET HOY = DATE
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0111" ATTRIBUTE(BORDER)
	DISPLAY " COMM011               MANTENEDOR DE PERCEPCIONES                                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "PERCEPCIONES"
		COMMAND "Agrega" "Agrega Percepcion"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Percepcion"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Percepcion"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Percepcion"
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
        LET vaccion = 0
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 2,69 ATTRIBUTE(REVERSE)
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                      " AT 1,1 ATTRIBUTE(blue)
        LET g_reg.percep_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD percep_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(percep_cod) 
                       INTO g_reg.percep_cod 
                       FROM tab_percepcion
                     IF g_reg.percep_cod IS NULL THEN
	                LET g_reg.percep_cod = 1
	             ELSE
			LET g_reg.percep_cod = g_reg.percep_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD percep_cod
		    IF g_reg.percep_cod IS NULL THEN
		       ERROR "Codigo de Percepcion NO puede ser nulo"
		       NEXT FIELD  percep_cod
		    END IF
                    SELECT "X" FROM tab_percepcion
                    WHERE percep_cod = g_reg.percep_cod
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD percep_cod
                    END IF 
	      BEFORE FIELD percep_desc
		     IF g_reg.percep_cod IS NULL THEN
		        ERROR "Codigo de Percepcion NO puede ser nulo"
			NEXT FIELD  percep_cod
		     END IF 
              AFTER FIELD percep_desc
		     IF g_reg.percep_desc IS NULL THEN
		        ERROR "Descripcion de Percepcion NO puede ser nula"
		        NEXT FIELD  percep_desc
		     END IF 
		     SELECT "X" FROM tab_percepcion
		     WHERE percep_desc = g_reg.percep_desc
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Percepcion ya Ingresado"
			NEXT FIELD percep_cod
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.percep_cod IS NULL THEN
		        ERROR "Codigo de Percepcion NO puede ser NULO"
		        NEXT FIELD percep_cod
		     END IF
		     IF g_reg.percep_desc IS NULL THEN
		        ERROR "Descripcion de Percepcion NO puede ser NULO"
                        NEXT FIELD percep_desc
		     END IF
		     SELECT "X" FROM tab_percepcion
		     WHERE percep_desc = g_reg.percep_desc
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Percepcion ya Ingresado"
			NEXT FIELD percep_cod
		     END IF

                     INSERT INTO tab_percepcion VALUES ( g_reg.* ) 
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD percep_cod
                  ON KEY (INTERRUPT)
                     CALL Inicializa()
                     EXIT INPUT
	END INPUT
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo       SMALLINT, 
                descripcion  CHAR(50)
         END RECORD
         DEFINE pos                SMALLINT   
         DECLARE cursor_1 CURSOR FOR
	 SELECT * FROM tab_percepcion
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0112" ATTRIBUTE( BORDER)
	    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                        P E R C E P C I O N E S                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	    END DISPLAY
	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE PERCEPCIONES.... VACIO"
	 END IF
         CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   DEFINE l_record   ARRAY[300] OF RECORD
      codigo       SMALLINT, 
      descripcion  CHAR(50)
   END RECORD
   DEFINE pos                SMALLINT   

   DECLARE cursor_2 CURSOR FOR
   SELECT * FROM tab_percepcion
   ORDER BY 1
   LET pos = 1
   FOREACH cursor_2 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0112" ATTRIBUTE(BORDER)
      DISPLAY "              ESCOJA CON < ENTER > LA PERCEPCION  A MODIFICAR                     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                        P E R C E P C I O N E S                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
      DISPLAY ARRAY l_record TO scr_1.* 
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET g_reg.percep_cod = l_record[pos].codigo
            LET g_reg.percep_desc = l_record[pos].descripcion
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

         BEFORE FIELD percep_cod
            ERROR "Verificando codigo percepcion contra afiliaciones"
            DECLARE curper1 CURSOR FOR
            SELECT "1"
              FROM afi_mae_afiliado a,
                   pro_mae_promotor b,
                   com_percepcion c,
                   tab_percepcion d
             WHERE a.codven = b.codven
               AND b.nivel = c.cod_tipo_prom 
               AND c.percep_cod = d.percep_cod
               AND d.percep_cod = g_reg.percep_cod
            OPEN curper1
            FETCH curper1
            IF STATUS <> NOTFOUND THEN
               ERROR "No puedes modificar porque existen afiliaciones con este codigo"
               SLEEP 2
               CLOSE curper1
               EXIT INPUT
            END IF
            CLOSE curper1
            ERROR "" 
            NEXT FIELD percep_desc

         AFTER FIELD percep_desc
	    IF g_reg.percep_desc IS NULL THEN
               ERROR "Campo NO puede ser nula"
               NEXT FIELD  percep_desc
            END IF 
         ON KEY ( ESC )
	    IF g_reg.percep_cod IS NULL THEN
	       ERROR "Codigo de Percepcion NO puede ser NULO"
	       NEXT FIELD percep_cod
	    END IF
	    IF g_reg.percep_desc IS NULL THEN
	       ERROR "Descripcion de Percepcion NO puede ser NULO"
               NEXT FIELD percep_desc
	    END IF

            UPDATE tab_percepcion 
               SET percep_desc = g_reg.percep_desc
             WHERE percep_cod = g_reg.percep_cod
	    ERROR "REGISTRO MODIFICADO" SLEEP 1
	    ERROR ""
            CALL Inicializa()
	    EXIT INPUT
         ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE PERCEPCIONES.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina() 
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo       SMALLINT, 
                descripcion  CHAR(50)
         END RECORD
         DEFINE pos                SMALLINT   

         DECLARE cursor_3 CURSOR FOR
	 SELECT * FROM tab_percepcion
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	                  LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0112" ATTRIBUTE( BORDER)
	    DISPLAY "             ESCOJA CON < ENTER > LA PERCEPCION A ELIMINAR                     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                        P E R C E P C I O N E S                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_reg.percep_cod = l_record[pos].codigo
                       LET g_reg.percep_desc = l_record[pos].descripcion
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
           CLOSE WINDOW ventana_2

                 ERROR "Verificando codigo percepcion contra afiliaciones"
                 DECLARE curper2 CURSOR FOR
                 SELECT "1"
                   FROM afi_mae_afiliado a,
                        pro_mae_promotor b,
                        com_percepcion c,
                        tab_percepcion d
                  WHERE a.codven = b.codven
                    AND b.nivel = c.cod_tipo_prom 
                    AND c.percep_cod = d.percep_cod
                    AND d.percep_cod = g_reg.percep_cod
                 OPEN curper2
                 FETCH curper2
                 IF STATUS <> NOTFOUND THEN
                    ERROR "No puedes eliminar porque existen afiliaciones con este codigo"
                    SLEEP 2
                    LET opc="N"
                 ELSE
                    LET opc="S"
                 END IF
                 ERROR ""
                 CLOSE curper2

   IF opc="S" THEN
	      DISPLAY "" AT 1,1
	      DISPLAY "" AT 2,1
	      DISPLAY " ( Esc ) Elimina        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
              DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	      DISPLAY BY NAME  g_reg.*

              CALL Pregunta()
              IF aux_pausa MATCHES "[Ss]" THEN

                 DELETE FROM tab_percepcion
                 WHERE percep_cod = g_reg.percep_cod
                 ERROR "REGISTRO ELIMINADO" SLEEP 1
              ELSE
                ERROR "ELIMINAR CANCELADO" SLEEP 1
              END IF
              ERROR ""
              CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE PERCEPCIONES .... VACIO"
	END IF
  END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
