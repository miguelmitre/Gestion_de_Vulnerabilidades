################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Propietario       => E.F.P.                                                   #
#Programa TABM124  => CATALOGO DIAGNOSTICOS DE PENDIENTES DE CERTIF TRASPASOS  #
#Fecha             => 7 DE ABRIL DE 2006                                       #
#Por               => MAURO MUÑIZ CABALLERO                                    #
#Sistema           => TAB. 					               #
################################################################################

DATABASE safre_af

GLOBALS

         DEFINE g_param_taa	RECORD LIKE seg_modulo.*

	 DEFINE aux_pausa	CHAR(1),
		sw_1		SMALLINT,
		usuario		CHAR(8),
		hoy		DATE,
		pos		INTEGER,
		sel_where	CHAR(300),
		cla_where	CHAR(300),
		g_impre		CHAR(300),
		g_lista		CHAR(300)

         DEFINE g_reg		RECORD 
		pend_cod	CHAR(3),
		pend_desc	CHAR(35)
	 END RECORD
 
         DEFINE l_record	ARRAY[100] OF RECORD
		codigo 		CHAR(3),
		descripcion	CHAR(35)
	 END RECORD

END GLOBALS

MAIN

	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()  #i
	CALL proceso() #p

END MAIN

FUNCTION inicio()
#i---------------

	LET HOY = TODAY

	SELECT *, USER
	INTO   g_param_taa.*, usuario
	FROM   seg_modulo
        WHERE  modulo_cod = 'taa'

END FUNCTION

FUNCTION proceso()
#p----------------

	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1241" ATTRIBUTE( BORDER)
	DISPLAY " TABM124         CATALOGO DIAGNOSTICOS PENDIENTES CERTIF TRASP                 " AT 4,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 4,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO DIAG PEND TAA"
             		   COMMAND "Agrega" "Agrega Diagnostico"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Diagnostico"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Diagnostico"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Diagnostico"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Inicializa()
#iz------------------

        LET sw_1 = 0
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.pend_cod,
                        g_reg.pend_desc

END FUNCTION

FUNCTION Agrega()
#a---------------

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega               (Ctrl-c) Salir                                   " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.pend_desc = NULL
        LET sw_1 = 0

	INPUT BY NAME  g_reg.pend_cod,
                       g_reg.pend_desc WITHOUT DEFAULTS

	      AFTER FIELD pend_cod
		    IF g_reg.pend_cod IS NULL THEN
		       ERROR "Diagnostico NO puede ser nulo"
		       NEXT FIELD  pend_cod
		    END IF

		    SELECT "X" 
                    FROM   tab_pendiente_taa
		    WHERE  pend_cod = g_reg.pend_cod

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Diagnostico ya Ingresado"
		       NEXT FIELD pend_cod
		    END IF

              AFTER FIELD pend_desc
		     IF g_reg.pend_desc IS NULL THEN
		        ERROR "Descripcion de Diagnostico NO puede ser nulo"
		        NEXT FIELD  pend_desc
		     END IF 

	      ON KEY ( ESC )
		     IF g_reg.pend_cod IS NULL THEN
		        ERROR "Diagnostico NO puede ser NULO"
		        NEXT FIELD pend_cod
		     END IF

		     SELECT "X" 
                     FROM   tab_pendiente_taa
		     WHERE  pend_cod = g_reg.pend_cod

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Diagnostico ya Ingresado"
		        NEXT FIELD pend_cod
		     END IF

		     IF g_reg.pend_desc IS NULL THEN
		        ERROR "Descripcion de Diagnostico NO puede ser NULO"
                        NEXT FIELD pend_desc
		     END IF

                     INSERT INTO tab_pendiente_taa 
                     VALUES ( g_reg.pend_cod,
                              g_reg.pend_desc,
                              usuario,
                              HOY ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()

		     NEXT FIELD pend_cod

              ON KEY (INTERRUPT)
                     CALL Inicializa()
                     EXIT INPUT

        END INPUT

END FUNCTION

FUNCTION Consulta()
#c-----------------

	LET pos = 2

	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1242" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta             (Ctrl-p) Imprimir              (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                      D I A G N O S T I C O S                                  " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON   pend_cod
                               FROM pend_cod

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

	   LET sel_where =" SELECT pend_cod,pend_desc ",
                          " FROM tab_pendiente_taa WHERE ",cla_where CLIPPED,
                          " ORDER BY 1 "

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
	      ERROR "ARCHIVO DE ENTIDAD... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
	   END IF
	END IF
	CLEAR SCREEN
END FUNCTION

FUNCTION  Modifica()
#m------------------

	LET pos = 2

	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1242" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                    Escoja con <ENTER> diagnostico a modificar                 " AT 2,1
	   DISPLAY "                       D I A G N O S T I C O S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON pend_cod
                             FROM pend_cod
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
	   LET sel_where =" SELECT pend_cod,pend_desc ",
                          " FROM tab_pendiente_taa WHERE ",cla_where CLIPPED,
                          " ORDER BY 1 "

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
		       LET g_reg.pend_cod   = l_record[pos].codigo
                       LET g_reg.pend_desc  = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE ENTIDAD... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   INPUT BY NAME g_reg.pend_cod,
                         g_reg.pend_desc WITHOUT DEFAULTS

	      BEFORE FIELD pend_cod
		NEXT FIELD pend_desc

	      AFTER FIELD pend_desc
		IF g_reg.pend_desc IS NULL THEN
		   ERROR "Descipcion de Entidad no debe ser nulo."
		   NEXT FIELD pend_desc
		END IF

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
		   UPDATE tab_pendiente_taa 
                   SET    pend_desc  = g_reg.pend_desc,
                          usuario    = usuario,
                          factualiza = TODAY
		   WHERE  pend_cod   = g_reg.pend_cod
	
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
	   ERROR "ARCHIVO DE ENTIDAD... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION

FUNCTION Elimina()
#e----------------

	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1242" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                     Escoja con <ENTER> diagnostico a eliminar                  " AT 2,1
	   DISPLAY "                       D I A G N O S T I C O S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON pend_cod
                             FROM pend_cod
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

	   LET sel_where =" SELECT pend_cod,pend_desc ",
                          " FROM tab_pendiente_taa WHERE ",cla_where CLIPPED,
                          " ORDER BY 1 "

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
		       LET g_reg.pend_cod   = l_record[pos].codigo
                       LET g_reg.pend_desc  = l_record[pos].descripcion
		       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Debe elegir un registro."
		       LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE ENTIDAD... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	   DISPLAY BY NAME g_reg.pend_cod,
                           g_reg.pend_desc

	   CALL Pregunta()

	   IF aux_pausa MATCHES "[Ss]" THEN
	      DELETE FROM tab_pendiente_taa
	      WHERE pend_cod = g_reg.pend_cod

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
	   ERROR "ARCHIVO DE ENTIDAD... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION

FUNCTION Pregunta()

	PROMPT "Esta seguro S/N ? " FOR aux_pausa

END FUNCTION

FUNCTION impresion(pos)
#i---------------------

	DEFINE i, pos SMALLINT

	LET g_impre = g_param_taa.ruta_listados CLIPPED,"/",
                      usuario CLIPPED,".IMPTENTIDAD",
                      hoy USING "dd-mm-yyyy" CLIPPED

	START REPORT rpt_tabpend To g_impre

	FOR i = 1 TO (pos+1)
	   LET g_reg.pend_cod  = l_record[i].codigo
	   LET g_reg.pend_desc = l_record[i].descripcion

	   IF g_reg.pend_cod IS NULL THEN
	      EXIT FOR
	   END IF

	   OUTPUT TO REPORT rpt_tabpend(g_reg.*)
	END FOR

	FINISH REPORT rpt_tabpend

	ERROR "LISTADO GENERADO..."
	SLEEP 2
	ERROR ""

	LET g_lista = "lp ",g_impre
	RUN g_lista
END FUNCTION

REPORT rpt_tabpend(g_reg)
#rtp---------------------

   DEFINE g_reg		RECORD 
	  pend_cod	CHAR(3),
	  pend_desc  	CHAR(35)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM124 ",
               COLUMN 24," LISTADO DE CATALOGO DE DIAGNOSTICOS DE CERTIFICACION TRASPASOS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 03,"DIAGNOSTICO",
               COLUMN 20,"DESCRIPCION"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 3,g_reg.pend_cod ,
               COLUMN 10,g_reg.pend_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
	 PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
