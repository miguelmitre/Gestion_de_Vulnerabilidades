################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Programa TABM107  => CATALOGO DE SALARIO MINIMO                               #
#Fecha             => 12 octubre 2004                                        #
#Autor             => ISABEL FONSECA FRIAS                                     #
#Fecha modifica    =>                                                          #
#Actualizado       =>                                                          #
#Sistema           => TAB.                                                     #
################################################################################
DATABASE safre_af
GLOBALS

	 DEFINE aux_pausa	CHAR(1),
		sw_1		SMALLINT,
		vusuario	CHAR(8),
		hoy		DATE,
		vfecha_desde_sm	DATE,
		vfecha_desde	DATE,
		vfecha_hasta_sm	DATE,
		fecha_alta	DATE,
		pos		INTEGER,
		sel_where	CHAR(30000),
		cla_where	CHAR(30000)
      
         DEFINE g_reg		RECORD 
		fecha_desde_sm	DATE,
		monto_sm        DECIMAL(12,2)
	 END RECORD
 
         DEFINE l_record	ARRAY[30000] OF RECORD
		fecha_desde     DATE,
		monto_sm      	DECIMAL(12,2),
                fecha_hasta     DATE
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
	INTO   vusuario
	FROM   seg_modulo
        WHERE  modulo_cod = 'tab'

END FUNCTION
################################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1011" ATTRIBUTE( BORDER)
	DISPLAY " TABM101                CATALOGO SALARIO MINIMO                                " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO SALARIO MINIMO"
			   COMMAND "Agrega" "Agrega Salario Minimo"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Salario Minimo"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Salario Minimo"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Salario Minimo"
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
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      AFTER FIELD fecha_desde_sm
		    IF g_reg.fecha_desde_sm IS NULL THEN
		       ERROR "Fecha de Alta NO puede ser nula"
		       NEXT FIELD fecha_desde_sm
		    END IF

              #  Valida que la fecha_desde capturada no sea
              #  Menor a la ultima de la tab_salario_minimo

                     SELECT  MAX (fecha_desde_sm)
                     INTO vfecha_desde
                     FROM tab_salario_minimo
		     WHERE fecha_desde_sm > g_reg.fecha_desde_sm	 
                       IF vfecha_desde > g_reg.fecha_desde_sm THEN
		         ERROR "Fecha de Alta  menor a las ya capturadas"
		         NEXT FIELD fecha_desde_sm
		       END IF

		    SELECT "X"
		    FROM tab_salario_minimo
		    WHERE fecha_desde_sm= g_reg.fecha_desde_sm
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Fecha de Alta YA ingresado"
		       NEXT FIELD fecha_desde_sm
		    END IF
		     IF g_reg.monto_sm IS NULL THEN
		        ERROR "Monto  NO puede ser nulo"
		        NEXT FIELD  monto_sm
		     END IF
                    


              AFTER FIELD monto_sm
		     IF g_reg.monto_sm IS NULL THEN
		        ERROR "Monto  NO puede ser nulo"
		        NEXT FIELD  monto_sm
		     END IF
                    
	      ON KEY ( ESC )
		    IF g_reg.fecha_desde_sm IS NULL THEN
		       ERROR "Fecha de Alta NO puede ser nula"
		       NEXT FIELD fecha_desde_sm
		    END IF


              #  Valida que la fecha_desde capturada no sea
              #  Menor a la ultima de la tab_salario_minimo

                     SELECT  MAX (fecha_desde_sm)
                     INTO vfecha_desde
                     FROM tab_salario_minimo
		     WHERE fecha_desde_sm > g_reg.fecha_desde_sm	 
                       IF vfecha_desde > g_reg.fecha_desde_sm THEN
		         ERROR "Fecha de Alta  menor a las ya capturadas"
		         NEXT FIELD fecha_desde_sm
		       END IF

		    SELECT "X"
		    FROM tab_salario_minimo
		    WHERE fecha_desde_sm= g_reg.fecha_desde_sm
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Fecha de Alta YA ingresado"
		       NEXT FIELD fecha_desde_sm
		    END IF
		     IF g_reg.monto_sm IS NULL THEN
		        ERROR "Monto  NO puede ser nulo"
		        NEXT FIELD  monto_sm
		     END IF
                    
         # obtiene la fehca-hasta_sm
                   #  INITIALIZE vfecha_hasta_sm TO NULL 
                     SELECT fecha_desde_sm,fecha_hasta_sm
                     INTO vfecha_desde_sm,vfecha_hasta_sm
                     FROM tab_salario_minimo
		     WHERE fecha_hasta_sm IS NULL 
                       IF vfecha_hasta_sm IS NULL THEN
                        LET fecha_alta = g_reg.fecha_desde_sm - 1 UNITS DAY
		           UPDATE tab_salario_minimo SET
			        fecha_hasta_sm = fecha_alta
                           WHERE fecha_hasta_sm IS NULL
                       END IF
                     INSERT INTO tab_salario_minimo VALUES(g_reg.fecha_desde_sm,
                                      g_reg.monto_sm, "", hoy, vusuario) 
		     ERROR "REGISTRO INGRESADO"
                     SLEEP 2
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD fecha_desde_sm
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1012" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                             SALARIO MINIMO                                    " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON fecha_desde_sm, monto_sm, fecha_hasta_sm FROM                   fecha_desde_sm, monto_sm, fecha_hasta_sm  
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
		SLEEP 2
		ERROR ""
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


	LET sel_where = "SELECT  fecha_desde_sm, monto_sm, fecha_hasta_sm " ,
                         "FROM tab_salario_minimo WHERE " 
        	      	 ,cla_where CLIPPED,
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
	      DISPLAY ARRAY l_record TO scr_1.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE SALARIO MINIMO... VACIO"
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1012" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Modifica                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Salario Minimo a  modificar                 " AT 2,1
	   DISPLAY "                          SALARIO MINIMO                                        " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON fecha_desde_sm, monto_sm, fecha_hasta_sm FROM                   fecha_desde_sm, monto_sm, fecha_hasta_sm  
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
		SLEEP 2
		ERROR ""
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

	LET sel_where = "SELECT  fecha_desde_sm, monto_sm, fecha_hasta_sm " ,
                         "FROM tab_salario_minimo WHERE " 
        	      	 ,cla_where CLIPPED,
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
		       LET g_reg.fecha_desde_sm = l_record[pos].fecha_desde
                       LET g_reg.monto_sm = l_record[pos].monto_sm
		       EXIT DISPLAY
                      ON KEY (INTERRUPT) 
	               LET int_flag = TRUE
		       EXIT DISPLAY
	      END DISPLAY
	   IF int_flag = TRUE THEN
	      LET int_flag = FALSE
	      ERROR "BUSQUEDA CANCELADA..."
	      SLEEP 2
	      ERROR ""
	      CLEAR SCREEN
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE SALARIO MINIMO... VACIO"
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
	      BEFORE FIELD fecha_desde_sm
		NEXT FIELD monto_sm

	      AFTER FIELD monto_sm
		IF g_reg.monto_sm IS NULL THEN
		   ERROR "Monto NO debe ser nulo."
		   NEXT FIELD monto_sm
		END IF

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
		   UPDATE tab_salario_minimo SET
			monto_sm = g_reg.monto_sm,
                        fecha_actualiza=hoy,
                        usuario  = vusuario
		   WHERE fecha_desde_sm = g_reg.fecha_desde_sm
		   ERROR "REGISTRO MODIFICADO"
		   SLEEP 2
		   ERROR ""

		   CALL Inicializa()
		ELSE
		   ERROR "PROCESO DE MODIFICAR CANCELADO."
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
	   ERROR "ARCHIVO DE SALARIO MINIMO... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1012" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Elimina                                             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Salario Minimo  a eliminar                  " AT 2,1
	   DISPLAY "                          SALARIO MINIMO                                      " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON fecha_desde_sm, monto_sm, fecha_hasta_sm FROM                   fecha_desde_sm, monto_sm, fecha_hasta_sm  

	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
		SLEEP 2
		ERROR ""
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
	LET sel_where = "SELECT  fecha_desde_sm, monto_sm, fecha_hasta_sm " ,
                         "FROM tab_salario_minimo WHERE " 
        	      	 ,cla_where CLIPPED,
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
		       LET g_reg.fecha_desde_sm = l_record[pos].fecha_desde
                       LET g_reg.monto_sm = l_record[pos].monto_sm
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
	               LET int_flag = TRUE
		       EXIT DISPLAY
	      END DISPLAY
	   IF int_flag = TRUE THEN
	      LET int_flag = FALSE
	      ERROR "BUSQUEDA CANCELADA..."
	      SLEEP 2
	      ERROR ""
	      CLEAR SCREEN
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE SALARIO MINIMO... VACIO"
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
              DELETE FROM tab_salario_minimo 
              WHERE fecha_desde_sm = g_reg.fecha_desde_sm

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
	   ERROR "ARCHIVO DE SALARIO MINIMO... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
