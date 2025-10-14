################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa TABM065  => CATALOGO ARCHIVO DE SUCURSALES	                       #
#Fecha             => 16 DE MAYO 2002    				       #
#By                => STEFANIE DANIELA VERA PIÑA   			       #
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
		suc_cod	        SMALLINT,
		suc_desc  	CHAR(40)
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
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0651" ATTRIBUTE( BORDER)
	DISPLAY " TABM065              CATALOGO DE SUCURSALES DE AFORES                             " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO SUCURSALES "
	               	   COMMAND "Agrega" "Agrega Sucursales "
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Sucursales"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Sucursales"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Sucursales"
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
	DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.suc_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*

	      BEFORE FIELD suc_cod
                 IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(suc_cod) INTO g_reg.suc_cod FROM tab_sucursal
                     IF g_reg.suc_cod = 0 OR g_reg.suc_cod IS NULL THEN
	                LET g_reg.suc_cod = 1
	             ELSE
			LET g_reg.suc_cod = g_reg.suc_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                 END IF

	      AFTER FIELD suc_cod
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                       NEXT FIELD suc_desc  
                    END IF

                    IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                       NEXT FIELD suc_desc 
                    END IF
                                                                               
		    IF g_reg.suc_cod IS NULL THEN
		       ERROR "Codigo de sucursal NO puede ser nulo"
		       NEXT FIELD  suc_cod
		    END IF

                    SELECT "X"
 		    FROM tab_sucursal
                    WHERE suc_cod = g_reg.suc_cod
                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD suc_cod
                    END IF 

	      BEFORE FIELD suc_desc
		     IF g_reg.suc_cod IS NULL OR  g_reg.suc_cod = 0 THEN
		        ERROR "Codigo de sucursal NO puede ser nulo"
			NEXT FIELD  suc_cod
		     END IF 

              AFTER FIELD suc_desc
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                       NEXT FIELD suc_cod  
                    END IF

                    IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                       NEXT FIELD suc_cod 
                    END IF                                                     
		     IF g_reg.suc_desc IS NULL THEN
		        ERROR "Descripcion de sucursal NO puede ser nula"
		        NEXT FIELD  suc_desc
		     END IF 
                     SELECT "X" FROM tab_sucursal
                     WHERE suc_desc = g_reg.suc_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "Sucursal Ya Ingresada"
	                NEXT FIELD suc_cod
                     END IF 

	      ON KEY ( ESC )
		     IF g_reg.suc_cod IS NULL THEN
		        ERROR "Codigo de sucursal NO puede ser NULO"
		        NEXT FIELD suc_cod
		     END IF
		     IF g_reg.suc_desc IS NULL THEN
		        ERROR "Descripcion de sucursal NO puede ser NULA"
                        NEXT FIELD suc_desc
		     END IF
                     SELECT "X" FROM tab_sucursal
                     WHERE suc_desc = g_reg.suc_desc
                     IF STATUS <> NOTFOUND THEN
		        ERROR "Sucursal Ya Ingresada"
	                NEXT FIELD suc_cod
                     END IF 
                     INSERT INTO tab_sucursal VALUES ( g_reg.* ) 
		     ERROR "REGISTRO INGRESADO" SLEEP 2
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD suc_cod
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0652" ATTRIBUTE(BORDER)
	   DISPLAY " (Enter) Consulta             (Ctrl-p) Imprimir              (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                         SUCURSALES DE AFORES                                   " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON suc_cod,suc_desc FROM suc_cod,suc_desc
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

	   LET sel_where = "SELECT * FROM tab_sucursal WHERE ",cla_where CLIPPED,
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
	      ERROR "ARCHIVO DE SUCURSALES... VACIO"
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0652" ATTRIBUTE(BORDER)
	   DISPLAY " (Enter) Modifica                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                 Escoja con <ENTER> la sucursal a modificar                     " AT 2,1
	   DISPLAY "                         SUCURSALES DE AFORES                                   " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON suc_cod,suc_desc FROM suc_cod,suc_desc
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
	   LET sel_where = "SELECT * FROM tab_sucursal WHERE ",cla_where CLIPPED,
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
		       LET g_reg.suc_cod = l_record[pos].codigo
                       LET g_reg.suc_desc = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE SUCURSALES... VACIO"
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
	      BEFORE FIELD suc_cod
		NEXT FIELD suc_desc

	      AFTER FIELD suc_desc
		IF g_reg.suc_desc IS NULL THEN
		   ERROR "Descipcion de sucursal no debe ser nulo."
		   NEXT FIELD suc_desc
		END IF

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
		   UPDATE tab_sucursal SET
			suc_desc = g_reg.suc_desc
		   WHERE suc_cod = g_reg.suc_cod
	
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
	   ERROR "ARCHIVO DE SUCURSALES... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0652" ATTRIBUTE(BORDER)
	   DISPLAY " (Enter) Elimina                                             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                  Escoja con <ENTER> la sucursal a eliminar                     " AT 2,1
	   DISPLAY "                         SUCURSALES DE AFORES                                   " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON suc_cod,suc_desc FROM suc_cod,suc_desc
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

	   LET sel_where = "SELECT * FROM tab_sucursal WHERE ",cla_where CLIPPED,
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
		       LET g_reg.suc_cod  = l_record[pos].codigo
                       LET g_reg.suc_desc = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE SUCURSALES... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

	   DISPLAY BY NAME g_reg.*
	      CALL Pregunta()

	   IF aux_pausa MATCHES "[Ss]" THEN
	      DELETE FROM tab_sucursal
	      WHERE suc_cod = g_reg.suc_cod

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
	   ERROR "ARCHIVO DE SUCURSALES... VACIO."
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

	LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
                      usuario CLIPPED,".IMPTABAGENC",
                      hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_tabsucursal To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.suc_cod  = l_record[i].codigo
	   LET g_reg.suc_desc = l_record[i].descripcion

	   IF g_reg.suc_cod IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_tabsucursal(g_reg.*)
	END FOR

	FINISH REPORT rpt_tabsucursal

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tabsucursal(g_reg)
         DEFINE g_reg		RECORD 
		suc_cod	SMALLINT,
		suc_desc  	CHAR(40)
	 END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM065 ",
               COLUMN 24," LISTADO DE CATALOGO DE SUCURSALES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION SUCURSAL"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.suc_cod ,
               COLUMN 25,g_reg.suc_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
