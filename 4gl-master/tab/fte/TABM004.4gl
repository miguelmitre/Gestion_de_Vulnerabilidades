######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		     #
#Owner             => Carlos Welsh. 				     #
#Programa TABM004  => CATALOGO ARCHIVO DE SEXOS.                     #
#Fecha             => 27 Noviembre 1996. 			     #
#By                => JUAN DAVID HERNANDEZ OYARCE. 		     #
#Fecha Modifica    => 25 Octubre 1999.                               #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                #
#Sistema           => TAB. 					     #
######################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis      RECORD LIKE seg_modulo.*

	DEFINE aux_pausa		CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               sel_where                CHAR(300),
               cla_where                CHAR(300),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300),
               seg_usuario                  CHAR(08),
               pos                      SMALLINT
      
        DEFINE g_reg		RECORD 
       	       sexo_cod	                SMALLINT,
	       sexo_desc	  	CHAR(40)
	END RECORD

        DEFINE l_record         ARRAY[300] OF RECORD
               codigo                   SMALLINT,
               descripcion              CHAR(50)
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
   INTO seg_usuario
   FROM glo_parametro

   SELECT ruta_listados
   INTO   g_param_dis.ruta_listados
   FROM seg_modulo
   WHERE modulo_cod = 'tab'
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0041" ATTRIBUTE( BORDER)
	DISPLAY " TABM004                   CATALOGO  DE  SEXOS                                 " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO SEXOS   "
		COMMAND "Agrega" "Agrega Sexos"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Sexos"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Sexos"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Sexos"
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
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.sexo_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD sexo_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(sexo_cod) 
                     INTO g_reg.sexo_cod 
                     FROM tab_sexo

                     IF g_reg.sexo_cod = 0 OR g_reg.sexo_cod IS NULL THEN
	                LET g_reg.sexo_cod = 1
	             ELSE
			LET g_reg.sexo_cod = g_reg.sexo_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD sexo_cod
		    IF g_reg.sexo_cod IS NULL THEN
		       ERROR "Codigo de Ciudad NO puede ser nulo"
		       NEXT FIELD  sexo_cod
		    END IF

                    SELECT "X" 
                    FROM tab_sexo
                    WHERE sexo_cod = g_reg.sexo_cod

                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD sexo_cod
                    END IF 
	      BEFORE FIELD sexo_desc
		     IF g_reg.sexo_cod IS NULL OR  g_reg.sexo_cod = 0 THEN
		        ERROR "Codigo de Ciudad NO puede ser nulo"
			NEXT FIELD  sexo_cod
		     END IF 
              AFTER FIELD sexo_desc
		     IF g_reg.sexo_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  sexo_desc
		     END IF 

                     SELECT "X" 
                     FROM tab_sexo
                     WHERE sexo_desc = g_reg.sexo_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Sexo Ya Ingresado"
	                NEXT FIELD sexo_cod
                     END IF 
	      ON KEY ( ESC )
		     IF g_reg.sexo_cod IS NULL THEN
		        ERROR "Codigo de Ciudad NO puede ser NULO"
		        NEXT FIELD sexo_cod
		     END IF

		     IF g_reg.sexo_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser NULO"
                        NEXT FIELD sexo_desc
		     END IF

                     SELECT "X" 
                     FROM tab_sexo
                     WHERE sexo_desc = g_reg.sexo_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Sexo Ya Ingresado"
	                NEXT FIELD sexo_cod
                     END IF 

                     INSERT INTO tab_sexo VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD sexo_cod
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                             S   E   X   O   S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON sexo_cod FROM sexo_cod
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

      LET sel_where = "SELECT * FROM tab_sexo WHERE ",cla_where CLIPPED,
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
               ERROR "PROCESANDO IMPRESION ..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE SEXOS   .... VACIO"
         SLEEP 2
         ERROR ""
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                   Escoja con < ENTER > el sexo a modificar                    " AT 2,1
      DISPLAY "                              S   E   X   O   S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON sexo_cod FROM sexo_cod
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

      LET sel_where = "SELECT * FROM tab_sexo WHERE ",cla_where CLIPPED,
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
             ON KEY (control-m)
                LET pos = ARR_CURR()
		LET g_reg.sexo_cod = l_record[pos].codigo
                LET g_reg.sexo_desc = l_record[pos].descripcion
                EXIT DISPLAY
             ON KEY (INTERRUPT)
	        ERROR "Usted debe escojer un registro"
                LET pos = ARR_CURR()
	  END DISPLAY
	  CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE SEXO .... NO EXISTE"
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
          BEFORE FIELD sexo_cod
             NEXT FIELD sexo_desc
             AFTER FIELD sexo_desc
 
  	     IF g_reg.sexo_desc IS NULL THEN
                ERROR "Descripcion de Ciudad NO puede ser nula"
                NEXT FIELD  sexo_desc
             END IF 
  
             CALL Pregunta()
         
             IF aux_pausa MATCHES "[Ss]" THEN
                UPDATE tab_sexo SET
                       sexo_desc = g_reg.sexo_desc
                WHERE sexo_cod = g_reg.sexo_cod

   	       ERROR "REGISTRO MODIFICADO" 
                SLEEP 2
   	       ERROR ""

                CALL Inicializa()
             ELSE
                ERROR "PROCESO DE MODIFICAR,CANCELADO"
                SLEEP 2
                ERROR ""
             END IF
 	    EXIT INPUT
          ON KEY ( INTERRUPT )
             CALL Inicializa()
             EXIT INPUT
       END INPUT
    ELSE
       ERROR "ARCHIVO DE SEXOS   .... VACIO"
    END IF
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                    Escoja con < ENTER > el sexo a eliminar                    " AT 2,1
      DISPLAY "                              S   E   X   O   S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON sexo_cod FROM sexo_cod
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

      LET sel_where = "SELECT * FROM tab_sexo WHERE ",cla_where CLIPPED,
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
               LET g_reg.sexo_cod = l_record[pos].codigo
               LET g_reg.sexo_desc = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SEXO .... NO EXISTE"
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
            DELETE FROM tab_sexo
            WHERE sexo_cod = g_reg.sexo_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE SEXOS   .... VACIO"
   END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPTABSEXO",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabsexo TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.sexo_cod  = l_record[i].codigo
       LET g_reg.sexo_desc = l_record[i].descripcion
   
       IF g_reg.sexo_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabsexo(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabsexo

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabsexo(g_reg)
   DEFINE g_reg		RECORD 
          sexo_cod	        SMALLINT,
          sexo_desc	  	CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM004 ",
               COLUMN 24," LISTADO DE CATALOGO DE SEXO ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO SEXO",
               COLUMN 20,"DESCRIPCION SEXO"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.sexo_cod USING "#####&",
               COLUMN 20,g_reg.sexo_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
