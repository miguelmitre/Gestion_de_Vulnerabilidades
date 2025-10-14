######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )	             #
#Owner             => Carlos Welsh. 			             #
#Programa TABM006  => CATALOGO ARCHIVO LOCALIDADES.                  #
#Fecha             => 27 Noviembre 1996. 		             #
#By                => JUAN DAVID HERNANDEZ OYARCE. 	             #
#Fecha modifica    => 26 Octubre 1999.                               #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                #
#Sistema           => TAB. 				             #
######################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis     RECORD LIKE dis_parametro.*
 
	DEFINE aux_pausa		CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               pos                      SMALLINT,
               cla_where                CHAR(300),
               seg_usuario                  CHAR(08),
               sel_where                CHAR(300),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300)

        DEFINE g_reg		RECORD 
	       local_cod		SMALLINT,
	       local_desc	  	CHAR(40)
	END RECORD

        DEFINE l_record         ARRAY[3000] OF RECORD
               codigo                   SMALLINT,
               descripcion              CHAR(50)
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
   INTO seg_usuario
   FROM glo_parametro

   SELECT ruta_spool
   INTO g_param_dis.ruta_spool
   FROM dis_parametro
END FUNCTION

FUNCTION proceso()

	LET HOY  = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0061" ATTRIBUTE( BORDER)
	DISPLAY " TABM006                  CATALOGO  DE  LOCALIDADES                            " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO LOCALIDADES "
		COMMAND "Agrega" "Agrega Localidades"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Localidades"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Localidades"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Localidades"
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
	DISPLAY " ( Esc ) Agrega                (Ctrl-c) Salir                                  " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.local_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD local_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(local_cod) 
                     INTO g_reg.local_cod 
                     FROM tab_localidad

                     IF g_reg.local_cod = 0 OR g_reg.local_cod IS NULL THEN
	                LET g_reg.local_cod = 1
	             ELSE
			LET g_reg.local_cod = g_reg.local_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD local_cod
		    IF g_reg.local_cod IS NULL THEN
		       ERROR "Codigo de Ciudad NO puede ser nulo"
		       NEXT FIELD  local_cod
		    END IF

                    SELECT "X" 
                    FROM tab_localidad
                    WHERE local_cod = g_reg.local_cod

                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD local_cod
                    END IF 
	      BEFORE FIELD local_desc
		     IF g_reg.local_cod IS NULL OR  g_reg.local_cod = 0 THEN
		        ERROR "Codigo de Ciudad NO puede ser nulo"
			NEXT FIELD  local_cod
		     END IF 
              AFTER FIELD local_desc
		     IF g_reg.local_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  local_desc
		     END IF 

                     SELECT "X" 
                     FROM tab_localidad
                     WHERE local_desc = g_reg.local_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Local Ya Ingresado"
	                NEXT FIELD local_cod
                     END IF 
	      ON KEY ( ESC )
		     IF g_reg.local_cod IS NULL THEN
		        ERROR "Codigo de Ciudad NO puede ser NULO"
		        NEXT FIELD local_cod
		     END IF

		     IF g_reg.local_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser NULO"
                        NEXT FIELD local_desc
		     END IF

                     SELECT "X" 
                     FROM tab_localidad
                     WHERE local_desc = g_reg.local_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Local Ya Ingresado"
	                NEXT FIELD local_cod
                     END IF 

                     INSERT INTO tab_localidad VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD local_cod
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
   	
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0062" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                            L O C A L I D A D E S                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON local_cod FROM local_cod
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

      LET sel_where = "SELECT * FROM tab_localidad WHERE ",cla_where CLIPPED,
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
         ERROR "ARCHIVO DE LOCALIDADES... VACIO"
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0062" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > la localidad a modificar                 " AT 2,1
      DISPLAY "                             L O C A L I D A D E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON local_cod FROM local_cod
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

      LET sel_where = "SELECT * FROM tab_localidad WHERE ",cla_where CLIPPED,
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
	        LET g_reg.local_cod = l_record[pos].codigo
                LET g_reg.local_desc = l_record[pos].descripcion
                EXIT DISPLAY
             ON KEY (INTERRUPT)
	        ERROR "Usted debe escojer un registro"
                LET pos = ARR_CURR()
	  END DISPLAY
	  CLOSE WINDOW ventana_2
      ELSE
          ERROR "REGISTRO DE LOCALIDADES .... NO EXISTE"
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
         BEFORE FIELD local_cod
            NEXT FIELD local_desc
            AFTER FIELD local_desc

            IF g_reg.local_desc IS NULL THEN
               ERROR "Descripcion de Ciudad NO puede ser nula"
               NEXT FIELD  local_desc
            END IF 
    
            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_localidad SET
                      local_desc = g_reg.local_desc
               WHERE local_cod = g_reg.local_cod

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
      ERROR "ARCHIVO DE LOCALIDADES... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0062" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > la localidad a eliminar                  " AT 2,1
      DISPLAY "                             L O C A L I D A D E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON local_cod FROM local_cod
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

      LET sel_where = "SELECT * FROM tab_localidad WHERE ",cla_where CLIPPED,
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
	       LET g_reg.local_cod = l_record[pos].codigo
               LET g_reg.local_desc = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE LOCALIDADES .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_localidad
            WHERE local_cod = g_reg.local_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE LOCALIDADES  .... VACIO"
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

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPTABLOCAL",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tablocal TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.local_cod = l_record[i].codigo
       LET g_reg.local_desc = l_record[i].descripcion
   
       IF g_reg.local_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tablocal(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tablocal

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tablocal(g_reg)
   DEFINE g_reg		RECORD 
          local_cod		SMALLINT,
          local_desc	  	CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM006 ",
               COLUMN 24," LISTADO DE CATALOGO DE LOCALIDADES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO ",
               COLUMN 20,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.local_cod USING "####&",
               COLUMN 20,g_reg.local_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
