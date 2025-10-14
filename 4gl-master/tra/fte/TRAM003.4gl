##############################################################################
#Owner             => E.F.P.
#Programa TRAM003  => MANTENIMIENTO TABLA DE VALOR DE UDI   
#Fecha creacion    => 24 DE MARZO DE 1998
#By                => MAURO MUNIZ CABALLERO     
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 04 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS

    DEFINE
        aux_pausa             CHAR(1)

    DEFINE g_doctos           RECORD
        fecha_valor_udi       LIKE tra_cotiza_udi.fecha_valor_udi,
        valor_udi             LIKE tra_cotiza_udi.valor_udi
    END RECORD

    DEFINE
        HOY                   DATE
        
    DEFINE 
        bandera               ,
        sw_1                  SMALLINT

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o
 
    DEFER INTERRUPT

    LET HOY = TODAY
    LET bandera = FALSE

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TRAM0031" ATTRIBUTE( BORDER)
    DISPLAY " TRAM003      MANTENIMIENTO TABLA VALOR UDI TRA-ICE-AFO IMSS                   " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "MANTENIMIENTO VALOR UDI"

		COMMAND "Agrega"   "Agrega   COTIZACION VALOR UDI"
		         CALL Agrega() #a

                COMMAND "Consulta" "Consulta COTIZACION VALOR UDI"
                         CALL Consulta() #c

                COMMAND "Modifica" "Modifica COTIZACION VALOR UDI"
                         CALL Modifica() #m

                COMMAND "Elimina"  "Elimina  COTIZACION VALOR UDI"
                         CALL Elimina() #e

                COMMAND KEY(S) "Salir" "Salir del Programa"
	                 EXIT MENU

	END MENU

    CLOSE WINDOW ventana_1

END MAIN

FUNCTION Inicializa()
#i-------------------

        LET sw_1 = 0
	INITIALIZE g_doctos.* TO NULL
	DISPLAY BY NAME g_doctos.*

END FUNCTION

FUNCTION Agrega()
#a---------------

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( ESC ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY "    AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

        LET g_doctos.fecha_valor_udi  = NULL
        LET g_doctos.valor_udi        = NULL
        LET sw_1 = 0

        INPUT BY NAME g_doctos.* WITHOUT DEFAULTS
	      AFTER FIELD fecha_valor_udi

		    IF g_doctos.fecha_valor_udi IS NULL THEN
		        ERROR "Fecha de cotizacion valor UDI NO puede ser nula"
		        NEXT FIELD fecha_valor_udi
		    END IF

                    SELECT "X"
                    FROM   tra_cotiza_udi A
                    WHERE  A.fecha_valor_udi = g_doctos.fecha_valor_udi

                    IF STATUS <> NOTFOUND THEN
		        ERROR "Fecha ya ingresada"
	                NEXT FIELD fecha_valor_udi
                    END IF 

	      AFTER FIELD valor_udi
		    IF g_doctos.valor_udi IS NULL
                    OR g_doctos.valor_udi = 0 THEN
		         ERROR "Valor de la UDI NO puede ser nulo"
			 NEXT FIELD valor_udi
	            END IF 

	      ON KEY ( ESC )
		     IF g_doctos.fecha_valor_udi IS NULL THEN
		         ERROR "Fecha de cotizacion valor UDI NO puede ser nula"
		         NEXT FIELD fecha_valor_udi
		     END IF

		     IF g_doctos.valor_udi IS NULL THEN
	 	         ERROR "Valor de la UDI NO puede ser nulo"
                         NEXT FIELD valor_udi
		     END IF

                     INSERT INTO tra_cotiza_udi VALUES (g_doctos.fecha_valor_udi,
		  				   g_doctos.valor_udi,
                                                   HOY,
                                                   USER)

		     ERROR "VALOR INGRESADO" SLEEP 1
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD fecha_valor_udi

              ON KEY (INTERRUPT)
                     CALL Inicializa()
                     EXIT INPUT
	END INPUT

END FUNCTION

FUNCTION Consulta()
#c-----------------

    DEFINE l_record ARRAY[900] OF RECORD
        fecha_udi              DATE,
        descripcion            DECIMAL(10,6)
    END RECORD

    DEFINE
        pos                    SMALLINT   

    DECLARE cursor_1 CURSOR FOR
        SELECT * FROM tra_cotiza_udi
        ORDER BY 1

        LET pos = 1
        FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
        END FOREACH

        IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 8,8 WITH FORM "TRAM0032" ATTRIBUTE(BORDER)
	    DISPLAY "     CONSULTA " AT 1,50 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " (Ctrl-C) Salir                                     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "         CONSULTA DEL VALOR DE LA UDI TRA-ICE-AFO IMSS                         " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	    DISPLAY ARRAY l_record TO scr_1.*
                ON KEY (INTERRUPT)
                EXIT DISPLAY
	    END DISPLAY

	    CLOSE WINDOW ventana_2
	 ELSE
	    ERROR "ARCHIVO DE COTIZACIONES.... VACIO"
	END IF

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE l_record ARRAY[100] OF RECORD
        fecha_udi                DATE,
        descripcion              CHAR(20)
    END RECORD

    DEFINE
        pos                   SMALLINT   

        DECLARE cursor_2 CURSOR FOR
	SELECT * FROM tra_cotiza_udi
	ORDER BY 1

	LET pos = 1
	FOREACH cursor_2 INTO l_record[pos].*
	    LET pos = pos + 1
        END FOREACH

	IF (pos-1) >= 1 THEN

	   CALL  SET_COUNT(pos-1)

	   OPEN WINDOW ventana_2 AT 8,8 WITH FORM "TRAM0032" ATTRIBUTE(BORDER)
	   DISPLAY " ESCOJA CON < ENTER > EL VALOR A MODIFICAR           " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY " MODIFICA " AT 1,54 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "          MODIFICA EL VALOR DE LA UDI TRA-ICE-AFO IMSS                         " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
	   DISPLAY ARRAY l_record TO scr_1.* 
                   ON KEY (CONTROL-M)
                      LET pos = ARR_CURR()
		      LET g_doctos.fecha_valor_udi  = l_record[pos].fecha_udi
                      LET g_doctos.valor_udi = l_record[pos].descripcion
                      EXIT DISPLAY
                   ON KEY (INTERRUPT)
                      LET bandera = TRUE 
                      EXIT DISPLAY 
	   END DISPLAY

	   CLOSE WINDOW ventana_2

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " ( ESC ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   INPUT BY NAME  g_doctos.* WITHOUT DEFAULTS 
               BEFORE FIELD fecha_valor_udi
                   IF bandera THEN
                       LET bandera = FALSE
                       EXIT INPUT
                       CALL Inicializa()
                   END IF
	           NEXT FIELD valor_udi
               AFTER FIELD valor_udi

                   IF g_doctos.valor_udi IS NULL THEN
                       ERROR "El valor de la UDI NO puede ser nulo"
                       NEXT FIELD valor_udi
                   END IF 

	       ON KEY ( ESC )
                   IF g_doctos.valor_udi IS NULL THEN
		       ERROR "El valor de la UDI NO puede ser NULO"
                       NEXT FIELD valor_udi
	           END IF

                   UPDATE tra_cotiza_udi SET
                       valor_udi = g_doctos.valor_udi
                   WHERE fecha_valor_udi = g_doctos.fecha_valor_udi
	           ERROR "REGISTRO MODIFICADO" SLEEP 1
		   ERROR ""

                   CALL Inicializa()
		   EXIT INPUT
	       ON KEY ( INTERRUPT )
                        CALL Inicializa()
                        EXIT INPUT
  	   END INPUT
        ELSE
	   ERROR "ARCHIVO DE COTIZACIONES.... VACIO"
	END IF

END FUNCTION

FUNCTION Elimina()
#e----------------

    DEFINE l_record ARRAY[100] OF RECORD
        fecha_udi             DATE,
        descripcion           CHAR(20)
    END RECORD

    DEFINE
        pos                   SMALLINT   

         DECLARE cursor_3 CURSOR FOR
	 SELECT * FROM tra_cotiza_udi
	 ORDER BY 1

	 LET pos = 1
	 FOREACH cursor_3 INTO l_record[pos].*
	     LET pos = pos + 1
         END FOREACH

	 IF (pos-1) >= 1 THEN

	    CALL SET_COUNT(pos-1)

	    OPEN WINDOW ventana_2 AT 8,8 WITH FORM "TRAM0032" ATTRIBUTE(BORDER)
            DISPLAY " ELIMINA " AT 1,55 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY " ESCOJA CON < ENTER > EL VALOR A ELIMINAR             " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "         ELIMINA EL VALOR DE LA UDI TRA-ICE-AFO IMSS                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
	    DISPLAY ARRAY l_record TO scr_1.* 
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
		       LET g_doctos.fecha_valor_udi  = l_record[pos].fecha_udi
                       LET g_doctos.valor_udi = l_record[pos].descripcion
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       LET bandera = TRUE
                       EXIT DISPLAY 
	   END DISPLAY

	   CLOSE WINDOW ventana_2

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY "                        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY BY NAME  g_doctos.*

           CALL Pregunta()

           IF aux_pausa MATCHES "[Ss]" THEN
              DELETE FROM tra_cotiza_udi
              WHERE fecha_valor_udi = g_doctos.fecha_valor_udi
              ERROR "REGISTRO ELIMINADO" SLEEP 1
           ELSE
              ERROR "ELIMINAR CANCELADO" SLEEP 1
           END IF
           ERROR ""
           CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE COTIZACIONES.... VACIO"
	END IF

END FUNCTION

FUNCTION Pregunta()
#p-----------------

        IF bandera THEN
            LET bandera = FALSE
            LET aux_pausa = "N"
        ELSE
	    PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
        END IF

END FUNCTION

