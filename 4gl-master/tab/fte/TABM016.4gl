################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.P.P.
#Programa TABM016  => MANTENEDOR ARCHIVO SUPERVISORES
#Fecha             => ABRIL    1997.    				       #
#By                => JUAN COLIN M.
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
              DEFINE g_reg			RECORD 
		  super_cod		SMALLINT,
		  super_desc	  	CHAR(40),
                  area_cod              SMALLINT,
                  desc 		        CHAR(60),
                  nip                   INTEGER
	END RECORD
	DEFINE HOY			DATE
        
        DEFINE 
            sw_1                        SMALLINT
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM0161" ATTRIBUTE( BORDER)
	DISPLAY " TABM016              MANTENIMIENTO  DE  SUPERVISORES                               " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "MANTENEDOR DIAGNOSTICOS "
		COMMAND "Agrega" "Agrega SUPERVISORES"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta SUPERVISORES"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica SUPERVISORES"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina SUPERVISORES"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END MAIN
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
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.super_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD super_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(super_cod) INTO g_reg.super_cod FROM tab_supervisor
                     IF g_reg.super_cod = 0 OR g_reg.super_cod IS NULL THEN
	                LET g_reg.super_cod = 1
	             ELSE
			LET g_reg.super_cod = g_reg.super_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD super_cod
		    IF g_reg.super_cod IS NULL THEN
		       ERROR "Codigo de SUPERVISORES NO puede ser nulo"
		       NEXT FIELD  super_cod
		    END IF
                    SELECT "X" FROM tab_supervisor
                    WHERE super_cod = g_reg.super_cod
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD super_cod
                    END IF 
	      BEFORE FIELD super_desc
		     IF g_reg.super_cod IS NULL OR  g_reg.super_cod = 0 THEN
		        ERROR "Codigo de SUPERVISORES NO puede ser nulo"
			NEXT FIELD  super_cod
		     END IF 
              AFTER FIELD super_desc
		     IF g_reg.super_desc IS NULL THEN
		        ERROR "Descripcion de SUPERVISORES NO puede ser nula"
		        NEXT FIELD  super_desc
		     END IF 
                     SELECT "X" FROM tab_supervisor
                     WHERE super_desc = g_reg.super_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "SUPERVISORES Ya Ingresado"
	                NEXT FIELD super_cod
                     END IF 
              AFTER FIELD area_cod
                     IF g_reg.area_cod IS NULL THEN
                       CALL Despliega_area() RETURNING g_reg.area_cod,
                                                        g_reg.desc
                       IF g_reg.area_cod = 0 THEN NEXT FIELD area_cod END IF
                     ELSE
                        SELECT area_desc INTO g_reg.desc FROM tab_area
                        WHERE area_code = g_reg.area_cod
                        IF status = NOTFOUND THEN
                           ERROR "Codigo de Area Inexistente"
                           NEXT FIELD area_cod
                        END IF
                     END IF
		     DISPLAY BY NAME g_reg.area_cod,g_reg.desc
	
              AFTER FIELD nip
                     IF g_reg.nip IS NULL THEN
                       ERROR "password no puede ser nulo"
                       NEXT FIELD nip
                     END IF
                     SELECT "X" FROM tab_supervisor
                     WHERE nip = g_reg.nip
                     IF status <> NOTFOUND THEN
                        ERROR "El codigo ya fue password"
                        NEXT FIELD nip
                     END IF
	      ON KEY ( ESC )
		     IF g_reg.super_cod IS NULL THEN
		        ERROR "Codigo de SUPERVISORES NO puede ser NULO"
		        NEXT FIELD super_cod
		     END IF
		     IF g_reg.super_desc IS NULL THEN
		        ERROR "Descripcion de SUPERVISORES  NO puede ser NULO"
                        NEXT FIELD super_desc
		     END IF
		     IF g_reg.area_cod IS NULL THEN
		        ERROR "CODIGO de AREA  NO puede ser NULO"
                        NEXT FIELD area_cod
                     END IF
		     IF g_reg.nip IS NULL THEN
		        ERROR "PASSWORD  NO puede ser NULO"
                        NEXT FIELD nip
                     END IF
                     SELECT "X" FROM tab_supervisor
                     WHERE super_desc = g_reg.super_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "SUPERVISORES Ya Ingresado"
	                NEXT FIELD super_cod
                     END IF 
                     INSERT INTO tab_supervisor VALUES ( g_reg.super_cod,
		  				   g_reg.super_desc,
                  				   g_reg.area_cod,
                  				   g_reg.nip)
		     ERROR "REGISTRO INGRESADO" SLEEP 1
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD super_cod
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion  CHAR(50),
                area_cod     SMALLINT ,
                pas          INTEGER
         END RECORD
         DEFINE pos                SMALLINT   
         DECLARE cursor_1 CURSOR FOR
	 SELECT * FROM tab_supervisor
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,10 WITH FORM "TABM0162" ATTRIBUTE( BORDER)
	    DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                         SUPERVISORES                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	    END DISPLAY
	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE SUPERVISORES... VACIO"
	 END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()
        DEFINE l_record   ARRAY[300] OF RECORD
               codigo         SMALLINT,
               descripcion  CHAR(50) ,
               area         SMALLINT ,
               pas          INTEGER
        END RECORD
        DEFINE pos                SMALLINT   

        DECLARE cursor_2 CURSOR FOR
	SELECT * FROM tab_supervisor
	ORDER BY 1
	LET pos = 1
	FOREACH cursor_2 INTO l_record[pos].*
	        LET pos = pos + 1
        END FOREACH
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 5,10 WITH FORM "TABM0162" ATTRIBUTE( BORDER)
	   DISPLAY " ESCOJA CON < ENTER > EL CODIGO  A MODIFICAR" AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                          SUPERVISORES                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
           DISPLAY " MODIFICA " AT 2,54 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY ARRAY l_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
		      LET g_reg.super_cod = l_record[pos].codigo
                      LET g_reg.super_desc = l_record[pos].descripcion
                      LET g_reg.area_cod = l_record[pos].area
                      LET g_reg.nip = l_record[pos].pas
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
                        SELECT area_desc INTO g_reg.desc FROM tab_area
                        WHERE area_code = g_reg.area_cod
	   INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
                 BEFORE FIELD super_cod
		       NEXT FIELD super_desc
                 AFTER FIELD super_desc
		       IF g_reg.super_desc IS NULL THEN
                          ERROR "Descripcion de SUPERVISORES NO puede ser nula"
                          NEXT FIELD  super_desc
                       END IF 
                 AFTER FIELD area_cod
                     IF g_reg.area_cod IS NULL THEN
                       CALL Despliega_area() RETURNING g_reg.area_cod,
                                                        g_reg.desc
                       IF g_reg.area_cod = 0 THEN NEXT FIELD area_cod END IF
                     ELSE
                        SELECT area_desc INTO g_reg.desc FROM tab_area
                        WHERE area_code = g_reg.area_cod
                        IF status = NOTFOUND THEN
                           ERROR "Codigo de Area Inexistente"
                           NEXT FIELD area_cod
                        END IF
                     END IF
		     DISPLAY BY NAME g_reg.area_cod,g_reg.desc
                 AFTER FIELD nip
                       IF g_reg.nip IS NULL THEN
                         ERROR "Password no puede ser nula"
                         NEXT FIELD nip
                       END IF
	         ON KEY ( ESC )
                    IF g_reg.super_desc IS NULL THEN
		       ERROR "Descripcion de SUPERVISORES NO puede ser NULO"
                       NEXT FIELD super_desc
		    END IF
                    IF g_reg.area_cod IS NULL THEN
		       ERROR "Area de Codigo NO puede ser NULO"
                       NEXT FIELD area_cod
		    END IF
                    IF g_reg.nip IS NULL THEN
		       ERROR "Password  NO puede ser NULO"
                       NEXT FIELD nip
		    END IF
                    UPDATE tab_supervisor SET
                           super_desc = g_reg.super_desc,
                           area_cod = g_reg.area_cod,
                           nip = g_reg.nip
                    WHERE super_cod = g_reg.super_cod
		    ERROR "REGISTRO MODIFICADO" SLEEP 1
		    ERROR ""
                    CALL Inicializa()
		    EXIT INPUT
	         ON KEY ( INTERRUPT )
                        CALL Inicializa()
                        EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE SUPERVISORES VACIO..."
	END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
         DEFINE l_record   ARRAY[300] OF RECORD
                codigo         SMALLINT,
                descripcion  CHAR(50) ,
                area         SMALLINT ,
                pas          INTEGER
         END RECORD
         DEFINE pos                SMALLINT   

         DECLARE cursor_3 CURSOR FOR
	 SELECT * FROM tab_supervisor
	 ORDER BY 1
	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	                  LET pos = pos + 1
         END FOREACH
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 7,10 WITH FORM "TABM0162" ATTRIBUTE( BORDER)
	    DISPLAY " ESCOJA CON < ENTER > EL TIPO DE SUPERVISORES A ELIMINAR" AT 1,1 ATTRIBUTE(REVERSE,BOLD)
            DISPLAY " ELIMINA " AT 1,55 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                         SUPERVISORES                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_reg.super_cod = l_record[pos].codigo
                       LET g_reg.super_desc = l_record[pos].descripcion
                       LET g_reg.area_cod = l_record[pos].area
                       LET g_reg.nip = l_record[pos].pas
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
              DELETE FROM tab_supervisor
              WHERE super_cod = g_reg.super_cod
              ERROR "REGISTRO ELIMINADO" SLEEP 1
           ELSE
              ERROR "ELIMINAR CANCELADO" SLEEP 1
           END IF
           ERROR ""
           CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE SUPERVISORES  .... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
