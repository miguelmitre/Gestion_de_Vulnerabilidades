######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		     #
#Owner             => E.F.P.                                         #
#Programa TABM008  => CATALOGO ARCHIVO DE ENTIDADES FEDERATIVAS      #
#Fecha             =>  7 Mayo 1997.     			     #
#By                => GERARDO ALFONOS VEGA PAREDES.		     #
#Fecha modifica    => 26 Octubre 1999.                               #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Sistema           => TAB. 					     #
######################################################################
DATABASE safre_af
GLOBALS
 
        DEFINE g_param_dis      RECORD LIKE seg_modulo.*

	DEFINE aux_pausa		CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               seg_usuario                  CHAR(08),
               pos                      SMALLINT,
               sel_where                CHAR(300),
               cla_where                CHAR(300),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300),
               vaccion                  SMALLINT

        DEFINE g_reg		RECORD 
	       estad_cod		INTEGER,
	       estad_desc	  	CHAR(40),
	       estad_abre	  	CHAR(40),
               cpos_desde               CHAR(05),	
               cpos_hasta               CHAR(05)
	END RECORD

        DEFINE l_record         ARRAY[300] OF RECORD
               codigo                   INTEGER,
               descripcion              CHAR(50),
               abrevia                  CHAR(50),
               desde                    CHAR(05),
               hasta                    CHAR(05)
        END RECORD
END GLOBALS

MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

        CALL inicio()
        CALL proceso()
END MAIN
 
FUNCTION inicio()
   SELECT USER,*
   INTO   seg_usuario
   FROM   glo_parametro

   SELECT ruta_listados
   INTO   g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE modulo_cod = 'tab'
END FUNCTION

FUNCTION proceso()
	LET HOY = DATE
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0081" ATTRIBUTE( BORDER)
	DISPLAY " TABM008             CATALOGO DE ENTIDADES FEDERATIVAS                         " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "ENTIDADES FEDERATIVAS"
		COMMAND "Agrega" "Agrega Estado"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Estado"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Estado"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Estado"
		                     CALL Elimina()
                           COMMAND "Salir" "Salir del Programa"
		                     EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END FUNCTION
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
	DISPLAY " AGREGA " AT 2,69 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " ( Esc ) Agrega               (Ctrl-c) Salir                                   " AT 1,1 ATTRIBUTE(BOLD)
        LET g_reg.estad_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD estad_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(estad_cod) 
                     INTO g_reg.estad_cod 
                     FROM tab_estado

                     IF g_reg.estad_cod = 0 OR g_reg.estad_cod IS NULL THEN
	                LET g_reg.estad_cod = 1
	             ELSE
			LET g_reg.estad_cod = g_reg.estad_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD estad_cod
		    IF g_reg.estad_cod IS NULL THEN
		       ERROR "Codigo de Estado NO puede ser nulo"
		       NEXT FIELD  estad_cod
		    END IF

                    SELECT "X" 
                    FROM tab_estado
                    WHERE estad_cod = g_reg.estad_cod

                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD estad_cod
                    END IF 
	      BEFORE FIELD estad_desc
		     IF g_reg.estad_cod IS NULL OR  g_reg.estad_cod = 0 THEN
		        ERROR "Codigo de Estado NO puede ser nulo"
			NEXT FIELD  estad_cod
		     END IF 
              AFTER FIELD estad_desc
		     IF g_reg.estad_desc IS NULL THEN
		        ERROR "Descripcion de Estado NO puede ser nula"
		        NEXT FIELD  estad_desc
		     END IF 

		     SELECT "X" 
                     FROM tab_estado
		     WHERE estad_desc = g_reg.estad_desc

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Estado ya Ingresada"
			NEXT FIELD estad_cod
		     END IF
	      BEFORE FIELD estad_abre
		     IF g_reg.estad_desc IS NULL THEN
		        ERROR "Descripcion de Estado NO puede ser nula"
		        NEXT FIELD  estad_desc
		     END IF 
              AFTER FIELD estad_abre
		     IF g_reg.estad_abre IS NULL THEN
		        ERROR "Estado Abreviada  NO puede ser nulo"
		        NEXT FIELD  estad_abre
		     END IF 
  
		     SELECT "X" 
                     FROM tab_estado
		     WHERE estad_abre = g_reg.estad_abre

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Estado Abreviada ya Ingresada"
			NEXT FIELD estad_abre
		     END IF
	      BEFORE FIELD cpos_desde
		     IF g_reg.estad_abre IS NULL THEN
		        ERROR "Estado Abreviada  NO puede ser nulo"
		        NEXT FIELD  estad_abre
		     END IF 
              AFTER FIELD cpos_desde
		     IF g_reg.cpos_desde IS NULL THEN
		        ERROR "Codigo Postal NO puede ser nulo"
		        NEXT FIELD  cpos_desde
		     END IF 

                     IF LENGTH (g_reg.cpos_desde) <> 5 then
                        ERROR "Debe Ingresar Codigo Postal completo"
			NEXT FIELD cpos_desde
		     END IF

		     SELECT "X" 
                     FROM tab_estado
		     WHERE cpos_desde <= g_reg.cpos_desde 
                     AND   cpos_hasta >= g_reg.cpos_desde

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Codigo Postal ya Ingresado"
			NEXT FIELD cpos_desde
		     END IF
	      BEFORE FIELD cpos_hasta
		     IF g_reg.cpos_desde IS NULL THEN
		        ERROR "Codigo Postal NO puede ser nulo"
		        NEXT FIELD  cpos_desde
		     END IF 
              AFTER FIELD cpos_hasta
		     IF g_reg.cpos_hasta IS NULL THEN
		        ERROR "Codigo Postal NO puede ser nulo"
		        NEXT FIELD  cpos_hasta
		     END IF 

                     IF LENGTH (g_reg.cpos_hasta) <> 5 then
                        ERROR "Debe Ingresar Codigo Postal completo"
			NEXT FIELD cpos_hasta
		     END IF

		     SELECT "X" 
                     FROM tab_estado
		     WHERE cpos_desde <= g_reg.cpos_hasta 
                     AND   cpos_hasta >= g_reg.cpos_hasta

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Codigo Postal ya Ingresado"
			NEXT FIELD cpos_hasta
		     END IF

                     IF g_reg.cpos_hasta < g_reg.cpos_desde THEN
                        ERROR "Este Codigo no puede ser menor al Codigo Desde"
                        NEXT FIELD cpos_hasta
                     END IF
	      ON KEY ( ESC )
		     IF g_reg.estad_cod IS NULL THEN
		        ERROR "Codigo de Estado NO puede ser NULO"
		        NEXT FIELD estad_cod
		     END IF

		     IF g_reg.estad_desc IS NULL THEN
		        ERROR "Descripcion de Estado NO puede ser NULO"
                        NEXT FIELD estad_desc
		     END IF

		     IF g_reg.estad_abre IS NULL THEN
		        ERROR "Estado Abreviada  NO puede ser nula"
		        NEXT FIELD  estad_abre
		     END IF 

		     IF g_reg.cpos_desde IS NULL THEN
		        ERROR "Codigo Postal NO puede ser nula"
		        NEXT FIELD  cpos_desde
		     END IF 

                     IF LENGTH (g_reg.cpos_desde) <> 5 then
                        ERROR "Debe Ingresar Codigo Postal completo"
			NEXT FIELD cpos_desde
		     END IF

		     SELECT "X" 
                     FROM tab_estado
		     WHERE cpos_desde <= g_reg.cpos_desde 
                     AND   cpos_hasta >= g_reg.cpos_desde

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Codigo Postal ya Ingresado"
			NEXT FIELD cpos_desde
		     END IF

		     IF g_reg.cpos_hasta IS NULL THEN
		        ERROR "Codigo Postal NO puede ser nula"
		        NEXT FIELD  cpos_hasta
		     END IF 

                     IF LENGTH (g_reg.cpos_hasta) <> 5 then
                        ERROR "Debe Ingresar Codigo Postal completo"
			NEXT FIELD cpos_hasta
		     END IF

		     SELECT "X" 
                     FROM tab_estado
		     WHERE cpos_desde <= g_reg.cpos_hasta 
                     AND   cpos_hasta >= g_reg.cpos_hasta

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Codigo Postal ya Ingresado"
			NEXT FIELD cpos_hasta
		     END IF

                     IF g_reg.cpos_hasta < g_reg.cpos_desde THEN
                        ERROR "Este Codigo no puede ser menor al Codigo Desde"
                        NEXT FIELD cpos_hasta
                     END IF

		     SELECT "X" 
                     FROM tab_estado
		     WHERE estad_desc = g_reg.estad_desc

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Estado ya Ingresada"
			NEXT FIELD estad_cod
		     END IF

                     INSERT INTO tab_estado VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO"
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD estad_cod
                  ON KEY (INTERRUPT)
                     CALL Inicializa()
                     EXIT INPUT
	END INPUT
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()
   	
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0082" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                              ENTIDADES FEDERATIVAS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod FROM estad_cod
         ON KEY (control-m)
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

      LET sel_where = "SELECT * FROM tab_estado WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
	 DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE ENTIDADES FEDERATIVAS.... VACIO"
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
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0082" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > la entidad a modificar                   " AT 2,1
      DISPLAY "                            ENTIDADES FEDERATIVAS                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod FROM estad_cod
         ON KEY (control-m)
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

      LET sel_where = "SELECT * FROM tab_estado WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.estad_cod = l_record[pos].codigo
               LET g_reg.estad_desc = l_record[pos].descripcion
               LET g_reg.estad_abre = l_record[pos].abrevia
               LET g_reg.cpos_desde = l_record[pos].desde
               LET g_reg.cpos_hasta = l_record[pos].hasta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	   END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE ENTIDADES .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD estad_cod
            NEXT FIELD estad_desc
            AFTER FIELD estad_desc
            IF g_reg.estad_desc IS NULL THEN
               ERROR "Descripcion de Ciudad NO puede ser nula"
               NEXT FIELD  estad_desc
            END IF 
         AFTER FIELD estad_abre
            IF g_reg.estad_abre IS NULL THEN
               ERROR "Descripcion de Estado NO puede ser nula"
               NEXT FIELD  estad_abre
            END IF 
         BEFORE FIELD cpos_desde
            DECLARE xcurcol cursor for
            SELECT "X" 
            FROM tab_colonia
            WHERE cpos_cod = g_reg.cpos_desde

            OPEN xcurcol
            FETCH xcurcol

            IF STATUS <> 100 THEN
               ERROR "No puedes modificar Codigo Postal porque existen Colonias con este Codigo"
               #SLEEP 2
               CLOSE xcurcol
               #CALL Inicializa()
               #EXIT INPUT
               NEXT FIELD estad_cod
            END IF

         AFTER FIELD cpos_desde
	    IF g_reg.cpos_desde IS NULL THEN
	       ERROR "Codigo Postal NO puede ser nulo"
	       NEXT FIELD  cpos_desde
	    END IF 

            IF LENGTH (g_reg.cpos_desde) <> 5 then
               ERROR "Debe Ingresar Codigo Postal completo"
	       NEXT FIELD cpos_desde
	    END IF
         AFTER FIELD cpos_hasta
	    IF g_reg.cpos_hasta IS NULL THEN
	       ERROR "Codigo Postal NO puede ser nulo"
	       NEXT FIELD  cpos_hasta
	    END IF 

            IF LENGTH (g_reg.cpos_hasta) <> 5 then
               ERROR "Debe Ingresar Codigo Postal completo"
	       NEXT FIELD cpos_hasta
	    END IF

            IF g_reg.cpos_hasta < g_reg.cpos_desde THEN
               ERROR "Este Codigo no puede ser menor al Codigo Desde"
               NEXT FIELD cpos_hasta
            END IF

            CALL Pregunta()
          
            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_estado SET
                      estad_desc = g_reg.estad_desc,
                      estad_abre = g_reg.estad_abre,
                      cpos_desde = g_reg.cpos_desde,
                      cpos_hasta = g_reg.cpos_hasta
               WHERE estad_cod = g_reg.estad_cod

	       ERROR "REGISTRO MODIFICADO" 
               SLEEP 2
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR,CANCELADO"
               SLEEP 2
            END IF

	    ERROR ""
	    EXIT INPUT
	 ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE ENTIDADES FEDERATIVAS.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0082" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con < ENTER > la entidad a eliminar                   " AT 2,1
      DISPLAY "                             ENTIDADES FEDERATIVAS                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON estad_cod FROM estad_cod
         ON KEY (control-m)
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

      LET sel_where = "SELECT * FROM tab_estado WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.estad_cod = l_record[pos].codigo
               LET g_reg.estad_desc = l_record[pos].descripcion
               LET g_reg.estad_abre = l_record[pos].abrevia
               LET g_reg.cpos_desde = l_record[pos].desde
               LET g_reg.cpos_hasta = l_record[pos].hasta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE ENTIDAD .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DECLARE xcurcol2 cursor for
      SELECT "X" 
      FROM tab_colonia
      WHERE cpos_cod = g_reg.cpos_desde 

      OPEN xcurcol2
      FETCH xcurcol2

      IF STATUS <> 100 THEN
         ERROR "No puedes Eliminar Entidad porque existen Colonias con este Codigo"
         SLEEP 2
         CLOSE xcurcol2
         ERROR "" 
         CALL Inicializa()
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_estado
            WHERE estad_cod = g_reg.estad_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE ENTIDADES  FEDERATIVAS.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPTABESTAD",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabestad TO g_impre

   FOR i=1 TO (pos+1)
   
       LET g_reg.estad_cod = l_record[i].codigo
       LET g_reg.estad_desc = l_record[i].descripcion
       LET g_reg.estad_abre = l_record[i].abrevia
       LET g_reg.cpos_desde = l_record[i].desde
       LET g_reg.cpos_hasta = l_record[i].hasta

       IF g_reg.estad_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabestad(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabestad

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabestad(g_reg)
   DEFINE g_reg		RECORD 
          estad_cod		INTEGER,
          estad_desc	  	CHAR(30),
          estad_abre	  	CHAR(10),
          cpos_desde            CHAR(05),	
          cpos_hasta            CHAR(05)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM008 ",
               COLUMN 14," LISTADO DE CATALOGO DE ENTIDADES FEDERATIVAS",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 10,"DESCRIPCION",
               COLUMN 50,"ABREVIA",
               COLUMN 61,"DESDE",
               COLUMN 70,"HASTA"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 02,g_reg.estad_cod USING "###&",
               COLUMN 10,g_reg.estad_desc,
               COLUMN 50,g_reg.estad_abre,
               COLUMN 61,g_reg.cpos_desde USING "#####",
               COLUMN 70,g_reg.cpos_hasta USING "#####"
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
