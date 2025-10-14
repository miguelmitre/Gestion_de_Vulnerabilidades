################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Programa TABM014  => MANTENCION ARCHIVO DIAGNOSTICOS PROMOTORES
#Fecha             => DICIEMBRE    1996. 				       #
#By                => ADOLFO DE LA MAZA            			       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Fecha modifica    => 22 Diciembre 1999.                                       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af

GLOBALS
        DEFINE g_param_dis              RECORD LIKE seg_modulo.*

        DEFINE g_reg			RECORD 
	       diagn_cod		CHAR(2),
	       diagn_desc	  	CHAR(80)
	END RECORD

        DEFINE l_record   ARRAY[300] OF RECORD
               diagn_cod                CHAR(2),
               diagn_desc               CHAR(80)
        END RECORD

	DEFINE HOY			DATE,
               aux_pausa                CHAR(01),
               pos                      SMALLINT,
               seg_usuario                  CHAR(08),
               sel_where                CHAR(100),
               cla_where                CHAR(100),
               g_lista                  CHAR(300),
               g_impre                  CHAR(300)
END GLOBALS
###############################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT

        CALL inicio()
        CALL proceso()
END MAIN
###############################################################################
FUNCTION inicio()
        SELECT USER
        INTO   seg_usuario
        FROM   glo_parametro

        SELECT ruta_listados
        INTO   g_param_dis.ruta_listados
        FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
##############################################################################
FUNCTION proceso()
	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0141" ATTRIBUTE( BORDER)
	DISPLAY " TABM014         CATALOGO  DE  DIAGNOSTICOS DE PROMOTORES                      " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO DIAGNOSTICOS "
           COMMAND "Agrega" "Agrega Diagnosticos"
              CALL Agrega()
           COMMAND "Consulta" "Consulta Diagnosticos"
              CALL Consulta()
           COMMAND "Modifica" "Modifica Diagnosticos"
              CALL Modifica()
           COMMAND "Elimina" "Elimina Diagnosticos"
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
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.diagn_desc = NULL
	INPUT BY NAME  g_reg.*
	      AFTER FIELD diagn_cod
		 IF g_reg.diagn_cod IS NULL THEN
		    ERROR "Codigo de diagnostico NO puede ser nulo"
		    NEXT FIELD  diagn_cod
		 END IF

                 SELECT "X" 
                 FROM   tab_diagnos_pro
                 WHERE  diagn_cod = g_reg.diagn_cod

                 IF STATUS <> NOTFOUND THEN
		    ERROR "Codigo Ya Ingresado"
	            NEXT FIELD diagn_cod
                 END IF 
	      BEFORE FIELD diagn_desc
		 IF g_reg.diagn_cod IS NULL OR  g_reg.diagn_cod = 0 THEN
		    ERROR "Codigo de diagnostico NO puede ser nulo"
		    NEXT FIELD  diagn_cod
		 END IF 
              AFTER FIELD diagn_desc
		 IF g_reg.diagn_desc IS NULL THEN
		    ERROR "Descripcion de diagnostico NO puede ser nula"
		    NEXT FIELD  diagn_desc
		 END IF 

                 SELECT "X" 
                 FROM   tab_diagnos_pro
                 WHERE  diagn_desc = g_reg.diagn_desc

                 IF STATUS <> NOTFOUND THEN
		    ERROR "Diagnostico Ya Ingresado"
	            NEXT FIELD diagn_cod
                 END IF 

	      ON KEY ( ESC )
		 IF g_reg.diagn_cod IS NULL THEN
		    ERROR "Codigo de Diagnostico NO puede ser NULO"
		    NEXT FIELD diagn_cod
		 END IF

		 IF g_reg.diagn_desc IS NULL THEN
		    ERROR "Descripcion de Diagnostico  NO puede ser NULO"
                    NEXT FIELD diagn_desc
		 END IF

                 SELECT "X" 
                 FROM   tab_diagnos_pro
                 WHERE  diagn_desc = g_reg.diagn_desc

                 IF STATUS <> NOTFOUND THEN
		    ERROR "Diagnostico Ya Ingresado"
	            NEXT FIELD diagn_cod
                 END IF 

                 INSERT INTO tab_diagnos_pro VALUES ( g_reg.* ) 

		 ERROR "REGISTRO INGRESADO" 
                 SLEEP 2
		 ERROR ""

                 CALL Inicializa()
                 EXIT INPUT
	         --NEXT FIELD diagn_cod
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
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0142" ATTRIBUTE( BORDER)
	    DISPLAY " (ENTER) Consulta               (Ctrl-p) Impresion            (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                             DIAGNOSTICOS DE PROMOTORES                        " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

            LET int_flag = FALSE

            CONSTRUCT cla_where ON diagn_cod FROM diagn_cod
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

            LET sel_where = "SELECT * FROM tab_diagnos_pro WHERE ",
                            cla_where CLIPPED,
                            " ORDER BY 1 "
  
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
	       ERROR "ARCHIVO DE DIAGNOSTICOS... VACIO"
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
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0142" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Consulta                                             (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                   Escoja con < ENTER > el codigo  a modificar                 " AT 2,1 
	   DISPLAY "                             DIAGNOSTICOS DE PROMOTORES                        " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 


           LET int_flag = FALSE

           CONSTRUCT cla_where ON diagn_cod FROM diagn_cod
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

           LET sel_where = "SELECT * FROM tab_diagnos_pro WHERE ",cla_where CLIPPED,
                           " ORDER BY 1 "
  
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
		      LET g_reg.diagn_cod = l_record[pos].diagn_cod
                      LET g_reg.diagn_desc = l_record[pos].diagn_desc
                      EXIT DISPLAY
                   ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2

              DISPLAY "" AT 1,1
              DISPLAY "" AT 2,1
              DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
              DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

	      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
                 BEFORE FIELD diagn_cod
		       NEXT FIELD diagn_desc
                 AFTER FIELD diagn_desc
		       IF g_reg.diagn_desc IS NULL THEN
                          ERROR "Descripcion de diagnostico NO puede ser nula"
                          NEXT FIELD  diagn_desc
                       END IF 
                 CALL Pregunta()

                 IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE tab_diagnos_pro SET
                           diagn_desc = g_reg.diagn_desc
                    WHERE  diagn_cod = g_reg.diagn_cod

		    ERROR "REGISTRO MODIFICADO" 
                    SLEEP 2
                 ELSE
                    ERROR "PROCESO DE MODIFICACION,CANCELADO"
                    SLEEP 2
                 END IF
                 CALL Inicializa()

                 ERROR ""
		 EXIT INPUT
	      ON KEY ( INTERRUPT )
                 CALL Inicializa()
                 EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE DIAGNOSTICOS DE PROMOTORES  VACIO"
	END IF
     END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
	 LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0142" ATTRIBUTE( BORDER)
            DISPLAY " (Enter) Consulta                                             (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "              Escoja con < ENTER > el tipo de tabajador a eliminar             " AT 2,1 
	    DISPLAY "                           DIAGNOSTICOS DE PROMOTORES                          " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

            LET int_flag = FALSE

            CONSTRUCT cla_where ON diagn_cod FROM diagn_cod
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

            LET sel_where = "SELECT * FROM tab_diagnos_pro WHERE ",cla_where CLIPPED,
                            " ORDER BY 1 "
  
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
	             LET g_reg.diagn_cod = l_record[pos].diagn_cod
                     LET g_reg.diagn_desc = l_record[pos].diagn_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
	             ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
	       END DISPLAY
	       CLOSE WINDOW ventana_2

	       DISPLAY "" AT 1,1
	       DISPLAY "" AT 2,1
	       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
               DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

               DISPLAY BY NAME  g_reg.*
                  CALL Pregunta()
                  IF aux_pausa MATCHES "[Ss]" THEN
                     DELETE FROM tab_diagnos_pro
                     WHERE diagn_cod = g_reg.diagn_cod

                     ERROR "REGISTRO ELIMINADO" 
                     SLEEP 2
                  ELSE
                     ERROR "ELIMINAR CANCELADO" 
                     SLEEP 2
                  END IF
                  ERROR ""
                  CALL Inicializa()
	    ELSE
	       ERROR "ARCHIVO DE DIAGNOSTICOS  .... VACIO"
            END IF
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
                 ".IMPTABDIPRO",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabdipro TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.diagn_cod  = l_record[i].diagn_cod
       LET g_reg.diagn_desc = l_record[i].diagn_desc
   
       IF g_reg.diagn_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabdipro(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabdipro

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabdipro(g_reg)

   DEFINE g_reg			RECORD 
          diagn_cod		CHAR(2),
          diagn_desc	  	CHAR(80)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(11U\033&l1O\033&k2S\033&l12d\033(s13H\033(s7B'   
         PRINT COLUMN 01,"TABM014 ",
               COLUMN 45," LISTADO DE CATALOGO DE DIAGNOSTICO DE PROMOTORES ",
               COLUMN 125,TODAY USING "dd-mm-yyyy"
         SKIP 3 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 45,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.diagn_cod ,
               COLUMN 15,g_reg.diagn_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
