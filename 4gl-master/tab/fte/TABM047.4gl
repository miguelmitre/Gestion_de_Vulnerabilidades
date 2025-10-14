################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.                                                   #
#Programa TABM019  => MANTENEDOR ARCHIVO STATUS DE PROMOTORES.(DIS)            #
#Fecha             => 05 Enero 1996.                                           #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Fecha modifica    => 21 DE JULIO DEL 2000                                     #
#Por               => ALEJANDRO CAMPOS SUAREZ                                  #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
         DEFINE g_param_dis	RECORD LIKE dis_parametro.*

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
		status_interno	  SMALLINT,
		desc_status_corta CHAR(25)
	 END RECORD
 
         DEFINE l_record	ARRAY[30000] OF RECORD
		codigo 		SMALLINT,
		descripcion	CHAR(25)
	 END RECORD
END GLOBALS
#####################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
   SELECT USER,*
   INTO   usuario
   FROM   glo_parametro

   SELECT ruta_spool
   INTO   g_param_dis.ruta_spool
   FROM   dis_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0471" ATTRIBUTE( BORDER)
	DISPLAY " TABM047             CATALOGO DE STATUS DE PROMOTORES                        " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO DE STATUS DE PROMOTORES "
			   COMMAND "Agrega" "Agrega Status de Promotor"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Status de Promotor"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Status de Promotor"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Status de Promotor"
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
        LET g_reg.desc_status_corta = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD status_interno	
                 IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(status_interno) INTO g_reg.status_interno	 FROM pro_status_interno
                     IF g_reg.status_interno	 = 0 OR g_reg.status_interno IS NULL THEN
	                LET g_reg.status_interno	 = 1
	             ELSE
			LET g_reg.status_interno	 = g_reg.status_interno + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                 END IF
	      AFTER FIELD status_interno	
		    IF g_reg.status_interno IS NULL THEN
		       ERROR "Codigo de Status NO puede ser nulo"
		       NEXT FIELD  status_interno	
		    END IF
		    SELECT "X" 
                    FROM pro_status_interno
		    WHERE status_interno	 = g_reg.status_interno	
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Status ya Ingresado"
		       NEXT FIELD status_interno	
		    END IF
	      BEFORE FIELD desc_status_corta
		     IF g_reg.status_interno IS NULL OR  g_reg.status_interno = 0 THEN
		        ERROR "Codigo de Status NO puede ser nulo"
			NEXT FIELD  status_interno	
		     END IF 
              AFTER FIELD desc_status_corta
		     IF g_reg.desc_status_corta IS NULL THEN
		        ERROR "Descripcion de Status NO puede ser nulo"
		        NEXT FIELD  desc_status_corta
		     END IF 
		     SELECT "X" FROM pro_status_interno
		     WHERE desc_status_corta = g_reg.desc_status_corta
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Status ya Ingresado"
		        NEXT FIELD status_interno	
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.status_interno IS NULL THEN
		        ERROR "Codigo de Status NO puede ser NULO"
		        NEXT FIELD status_interno	
		     END IF
		     IF g_reg.desc_status_corta IS NULL THEN
		        ERROR "Descripcion de Status NO puede ser NULO"
                        NEXT FIELD desc_status_corta
		     END IF
		     SELECT "X" 
                     FROM   pro_status_interno
		     WHERE desc_status_corta = g_reg.desc_status_corta
		     IF STATUS <> NOTFOUND THEN
		        ERROR "Status ya Ingresado"
		        NEXT FIELD status_interno	
		     END IF
                     INSERT INTO pro_status_interno VALUES (g_reg.*," ") 
		     ERROR "REGISTRO INGRESADO" SLEEP 2
		     ERROR ""
                     CALL Inicializa()
		     NEXT FIELD status_interno	
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0472" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta           (Ctrl-p) Imprimir            (Ctrl-C) Salir        " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                             S  T  A  T  U  S                                  " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON status_interno,desc_status_corta FROM status_interno,desc_status_corta
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

	   LET sel_where = "SELECT * FROM pro_status_interno WHERE ",cla_where CLIPPED,
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
	      ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO"
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0472" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "          Escoja con <ENTER> el Status de Promotor a modificar                " AT 2,1
	   DISPLAY "                             S  T  A  T  U  S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON status_interno,desc_status_corta FROM status_interno,desc_status_corta
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
	   LET sel_where = "SELECT * FROM pro_status_interno WHERE ",cla_where CLIPPED,
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
		       LET g_reg.status_interno = l_record[pos].codigo
                       LET g_reg.desc_status_corta = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO"
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
	      BEFORE FIELD status_interno	
		NEXT FIELD desc_status_corta

	      AFTER FIELD desc_status_corta
		IF g_reg.desc_status_corta IS NULL THEN
		   ERROR "Descipcion de Status no debe ser nulo."
		   NEXT FIELD desc_status_corta
		END IF

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
		   UPDATE pro_status_interno SET
			desc_status_corta = g_reg.desc_status_corta
		   WHERE status_interno = g_reg.status_interno	
	
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
	   ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0472" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "         Escoja con <ENTER> la Status de Promotor a eliminar                  " AT 2,1
	   DISPLAY "                        E  N  T  I  D  A  D  E  S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON status_interno,desc_status_corta FROM status_interno,desc_status_corta
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

	   LET sel_where = "SELECT * FROM pro_status_interno WHERE ",cla_where CLIPPED,
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
		       LET g_reg.status_interno = l_record[pos].codigo
                       LET g_reg.desc_status_corta = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO"
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
	      DELETE FROM pro_status_interno
	      WHERE status_interno	 = g_reg.status_interno	

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
	   ERROR "ARCHIVO DE STATUS DE PROMOTORES... VACIO."
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

	LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,".IMPTSTAT",hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_pro_status_interno To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.status_interno	 = l_record[i].codigo
	   LET g_reg.desc_status_corta = l_record[i].descripcion

	   IF g_reg.status_interno	 IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_pro_status_interno(g_reg.*)
	END FOR

	FINISH REPORT rpt_pro_status_interno

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_pro_status_interno(g_reg)
         DEFINE g_reg		RECORD 
		status_interno	SMALLINT,
		desc_status_corta  	CHAR(40)
	 END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM019 ",
               COLUMN 19," LISTADO DE CATALOGO DE STATUS DE PROMOTORES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION STATUS"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.status_interno ,
               COLUMN 25,g_reg.desc_status_corta
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"

END REPORT
