################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa COMM017  => MANTENEDOR ARCHIVO DE CANAL.
#Sistema           => COMM. 					               #
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ. 			       #
#Fecha             => 22 MARZO 1999. 				       #
#By                => GERARDO ALFONSO VEGA PAREDES.    			       #
#Fecha             => 16 enero 2001. 				       #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa CHAR(1)

   DEFINE g_reg  RECORD 
		canal_cod  SMALLINT,
		canal_desc CHAR(50),
      fctualiza  DATE,
      usuario    CHAR(8)
	END RECORD

	DEFINE hoy DATE
      
   DEFINE sw_1 SMALLINT
END GLOBALS

MAIN
	OPTIONS 
      PROMPT LINE LAST,
	   INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

	LET hoy = TODAY

   SELECT user
   INTO   g_reg.usuario
   FROM   com_parametro

	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0171" ATTRIBUTE( BORDER)

	DISPLAY "  COMM017               MANTENEDOR  DE  CANAL                                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY hoy USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "MANTENEDOR CANAL"
		COMMAND "Agrega" "Agrega canal"
         CALL Agrega()
      COMMAND "Consulta" "Consulta canal"
         CALL Consulta()
      COMMAND "Modifica" "Modifica canal"
         CALL Modifica()
      COMMAND "Elimina" "Elimina canal"
         CALL Elimina()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   LET sw_1 = 0
	INITIALIZE g_reg.canal_cod,g_reg.canal_desc  TO NULL
	DISPLAY BY NAME g_reg.canal_cod,g_reg.canal_desc
END FUNCTION

FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(yellow)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,green)

   LET g_reg.canal_desc = NULL
   LET sw_1 = 0

   INPUT BY NAME  g_reg.canal_cod,g_reg.canal_desc
      BEFORE FIELD canal_cod
         IF sw_1 = 0 THEN

            LET sw_1 = 1

	         SELECT MAX(canal_cod) 
            INTO   g_reg.canal_cod 
            FROM   tab_canal

            IF g_reg.canal_cod = 0 OR g_reg.canal_cod IS NULL THEN
	            LET g_reg.canal_cod = 1
	         ELSE
			      LET g_reg.canal_cod = g_reg.canal_cod + 1
		      END IF
            DISPLAY BY NAME g_reg.canal_cod,g_reg.canal_desc
         END IF

      AFTER FIELD canal_cod
	      IF g_reg.canal_cod IS NULL THEN
		      ERROR "Codigo de canal NO puede ser nulo"
		      NEXT FIELD canal_cod
		   END IF

         SELECT "X" 
         FROM   tab_canal
         WHERE  canal_cod = g_reg.canal_cod
         IF STATUS <> NOTFOUND THEN
	         ERROR "Codigo Ya Ingresado"
	         NEXT FIELD canal_cod
         END IF 

      BEFORE FIELD canal_desc
	      IF g_reg.canal_cod IS NULL OR  g_reg.canal_cod = 0 THEN
		      ERROR "Codigo de canal NO puede ser nulo"
			   NEXT FIELD  canal_cod
		   END IF 

      AFTER FIELD canal_desc
		   IF g_reg.canal_desc IS NULL THEN
		      ERROR "Descripcion de canal NO puede ser nula"
		      NEXT FIELD  canal_desc
		   END IF 

		   SELECT "X" 
         FROM   tab_canal
		   WHERE  canal_desc = g_reg.canal_desc
		   IF STATUS <> NOTFOUND THEN
			   ERROR "Canal ya Ingresado"
			   NEXT FIELD canal_cod
		   END IF
      ON KEY ( ESC )
	      IF g_reg.canal_cod IS NULL THEN
		      ERROR "Codigo de canal NO puede ser NULO"
		      NEXT FIELD canal_cod
		   END IF
		   IF g_reg.canal_desc IS NULL THEN
		      ERROR "Descripcion de canal NO puede ser NULO"
            NEXT FIELD canal_desc
		   END IF

		   SELECT "X" 
         FROM   tab_canal
		   WHERE  canal_desc = g_reg.canal_desc
	      IF STATUS <> NOTFOUND THEN
       	   ERROR "Canal ya Ingresado"
			   NEXT FIELD canal_cod
		   END IF

         INSERT INTO tab_canal VALUES (g_reg.*) 

	      ERROR "REGISTRO INGRESADO" SLEEP 1

		   ERROR ""

         CALL Inicializa()

	      NEXT FIELD canal_cod
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
	 SELECT * FROM tab_canal
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0172" ATTRIBUTE( BORDER)
	    DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,yellow)
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,YELLOW)
	    DISPLAY "                       C A N A L                                   " AT 3,1 ATTRIBUTE(REVERSE,green) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	    END DISPLAY
	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE CANAL.... VACIO"
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
	SELECT * FROM tab_canal
	ORDER BY 1
	LET pos = 1
	FOREACH cursor_2 INTO l_record[pos].*
	        LET pos = pos + 1
        END FOREACH
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0172" ATTRIBUTE( BORDER)
	   DISPLAY " ESCOJA CON < ENTER > EL CANAL A MODIFICAR" AT 1,1 ATTRIBUTE(REVERSE,yellow)
           DISPLAY " MODIFICA " AT 1,54 ATTRIBUTE(REVERSE,yellow)
	   DISPLAY "                       C A N A L                                   " AT 3,1 ATTRIBUTE(REVERSE,green)
	   DISPLAY ARRAY l_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
		      LET g_reg.canal_cod = l_record[pos].codigo
                      LET g_reg.canal_desc = l_record[pos].descripcion
                      EXIT DISPLAY
                   ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(yellow)
           DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,yellow)
	   INPUT BY NAME  g_reg.canal_cod,g_reg.canal_desc WITHOUT DEFAULTS 
                 BEFORE FIELD canal_cod
		       NEXT FIELD canal_desc
                 AFTER FIELD canal_desc
		       IF g_reg.canal_desc IS NULL THEN
                          ERROR "Descripcion de canal NO puede ser nula"
                          NEXT FIELD  canal_desc
                       END IF 
	         ON KEY ( ESC )
                    IF g_reg.canal_desc IS NULL THEN
		       ERROR "Descripcion de canal NO puede ser NULO"
                       NEXT FIELD canal_desc
		    END IF
                    UPDATE tab_canal SET
                           canal_desc = g_reg.canal_desc
                    WHERE canal_cod = g_reg.canal_cod
		    ERROR "REGISTRO MODIFICADO" SLEEP 1
		    ERROR ""
                    CALL Inicializa()
		    EXIT INPUT
	         ON KEY ( INTERRUPT )
                        CALL Inicializa()
                        EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE CANAL.... VACIO"
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
	 SELECT * FROM tab_canal
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	                  LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMM0172" ATTRIBUTE( BORDER)
            DISPLAY " ELIMINA " AT 1,55 ATTRIBUTE(REVERSE,yellow)
	    DISPLAY " ESCOJA CON < ENTER > EL CANAL A ELIMINAR" AT 1,1 ATTRIBUTE(REVERSE,yellow)
	   DISPLAY "                        C A N A L                                 " AT 3,1 ATTRIBUTE(REVERSE,green)
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_reg.canal_cod = l_record[pos].codigo
                       LET g_reg.canal_desc = l_record[pos].descripcion
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	   END DISPLAY
	   CLOSE WINDOW ventana_2
	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( Esc ) Elimina        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(yellow)
           DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,yellow)
	   DISPLAY BY NAME  g_reg.canal_cod,g_reg.canal_desc
           CALL Pregunta()
           IF aux_pausa MATCHES "[Ss]" THEN
              DELETE FROM tab_canal
              WHERE canal_cod = g_reg.canal_cod
              ERROR "REGISTRO ELIMINADO" SLEEP 1
           ELSE
              ERROR "ELIMINAR CANCELADO" SLEEP 1
           END IF
           ERROR ""
           CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE CANAL.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
