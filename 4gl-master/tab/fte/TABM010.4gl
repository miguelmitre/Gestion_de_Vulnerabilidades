######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )	             #
#Owner             => Carlos Welsh. 			             #
#Programa TABM010  => CATALOGO ARCHIVO DE PATRONES                   #
#Fecha             => 27 Noviembre 1996. 		             #
#By                => JUAN DAVID HERNANDEZ OYARCE. 	             #
#Fecha modifica    => 27 Octubre 1999                                #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Sistema           => TAB. 				             #
######################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis      RECORD LIKE dis_parametro.*

	DEFINE aux_pausa		CHAR(1),
               sw_1                     SMALLINT,
               seg_usuario                  CHAR(08),
               hoy                      DATE,
               pos                      INTEGER,
               sel_where                CHAR(30000),
               cla_where                CHAR(30000),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300) 
            
        DEFINE g_reg		RECORD 
	       reg_fed_contrib		CHAR(11),
	       razon_social	  	CHAR(50)
	END RECORD
      
        DEFINE l_record         ARRAY[30000] OF RECORD
               codigo                   CHAR(11),
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
   INTO   seg_usuario
   FROM   glo_parametro

   SELECT ruta_spool
   INTO   g_param_dis.ruta_spool
   FROM   dis_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0101" ATTRIBUTE( BORDER)
	DISPLAY " TABM010                      CATALOGO DE PATRONES                             " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO PATRONES"
		COMMAND "Agrega" "Agrega Patrones"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Patrones"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Patrones"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Patrones"
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
	DISPLAY " ( Esc ) Agrega                       (Ctrl-c) Salir                           " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.razon_social = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      AFTER FIELD reg_fed_contrib
		    IF g_reg.reg_fed_contrib IS NULL THEN
		       ERROR "Registro patron  NO puede ser nulo"
		       NEXT FIELD  reg_fed_contrib
		    END IF

                    SELECT "X" 
                    FROM tab_patron
                    WHERE reg_fed_contrib = g_reg.reg_fed_contrib

                    IF STATUS <> NOTFOUND THEN
		       ERROR "Registro patron Ya Ingresado"
	               NEXT FIELD reg_fed_contrib
                    END IF 
	      BEFORE FIELD razon_social
		     IF g_reg.reg_fed_contrib IS NULL OR  g_reg.reg_fed_contrib = 0 THEN
		        ERROR "Razon social NO puede ser nulo"
			NEXT FIELD  reg_fed_contrib
		     END IF 
              AFTER FIELD razon_social
		     IF g_reg.razon_social IS NULL THEN
		        ERROR "Razon social NO puede ser nula"
		        NEXT FIELD  razon_social
		     END IF 

	      ON KEY ( ESC )
		     IF g_reg.reg_fed_contrib IS NULL THEN
		        ERROR "Registro patron NO puede ser NULO"
		        NEXT FIELD reg_fed_contrib
		     END IF

		     IF g_reg.razon_social IS NULL THEN
		        ERROR "Razon social NO puede ser NULO"
                        NEXT FIELD razon_social
		     END IF


                     INSERT INTO tab_patron VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD reg_fed_contrib
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0102" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                P A T R O N E S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON reg_fed_contrib,razon_social FROM reg_fed_contrib,razon_social
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

      LET sel_where = "SELECT * FROM tab_patron WHERE ",cla_where CLIPPED,
                      "ORDER BY 2 "
  
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          ERROR ""
	  DISPLAY ARRAY l_record TO scr_1.*
             ON KEY (control-p)
                ERROR "PROCESANDO IMPRESION..."
                CALL impresion(pos)
             ON KEY (INTERRUPT)
                EXIT DISPLAY
	  END DISPLAY
          CLOSE WINDOW ventana_2
      ELSE
	  ERROR "ARCHIVO DE PATRONES.... VACIO"
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0102" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                    Escoja con < ENTER > el patron a modificar                 " AT 2,1
      DISPLAY "                               P A T R O N E S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON reg_fed_contrib,razon_social FROM reg_fed_contrib,razon_social
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

      LET sel_where = "SELECT * FROM tab_patron WHERE ",cla_where CLIPPED,
                      "ORDER BY 2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (control-m)
               LET pos = ARR_CURR()
	       LET g_reg.reg_fed_contrib = l_record[pos].codigo
               LET g_reg.razon_social = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE  
         ERROR "REGISTRO DE PATRONES .... NO EXISTE "
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
         BEFORE FIELD reg_fed_contrib
            NEXT FIELD razon_social
            AFTER FIELD razon_social

            IF g_reg.razon_social IS NULL THEN
               ERROR "Descripcion de Ciudad NO puede ser nula"
               NEXT FIELD  razon_social
            END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_patron SET
                      razon_social = g_reg.razon_social
               WHERE reg_fed_contrib = g_reg.reg_fed_contrib

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
      ERROR "ARCHIVO DE PATRONES.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0102" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                   Escoja con < ENTER > el patron a eliminar                   " AT 2,1
      DISPLAY "                                   P A T R O N E S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON reg_fed_contrib,razon_social FROM reg_fed_contrib,razon_social
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

      LET sel_where = "SELECT * FROM tab_patron WHERE ",cla_where CLIPPED,
                      "ORDER BY 2 "
  
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.reg_fed_contrib = l_record[pos].codigo
               LET g_reg.razon_social = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE PATRONES .... NO EXISTE"
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
            DELETE FROM tab_patron
            WHERE reg_fed_contrib = g_reg.reg_fed_contrib

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE PATRONES.... VACIO"
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
                 ".IMPMAEPATRO",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_maepatro TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.reg_fed_contrib = l_record[i].codigo
       LET g_reg.razon_social = l_record[i].descripcion
   
       IF g_reg.reg_fed_contrib IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_maepatro(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_maepatro

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_maepatro(g_reg)
   DEFINE g_reg		RECORD 
          reg_fed_contrib		CHAR(11),
          razon_social	  	CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM010 ",
               COLUMN 24," LISTADO DE CATALOGO DE PATRONES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"REGISTRO",
               COLUMN 20,"DESCRIPCION PATRON"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.reg_fed_contrib ,
               COLUMN 25,g_reg.razon_social
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
