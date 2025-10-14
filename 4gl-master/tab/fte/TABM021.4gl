################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa TABM021  => CATALOGO ARCHIVO DE LOTES
#Fecha             => 27 Noviembre 1996. 				       #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Fecha modifica    => 15 Diciembre 1999.                                       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis              RECORD LIKE seg_modulo.*

        DEFINE g_reg			RECORD 
		  lotes_fecha		DATE,
		  lotes_cod	  	SMALLINT,
		  lotes_desc	  	CHAR(40),
		  lotes_correlativo  	SMALLINT,
                  lotes_num             SMALLINT
	END RECORD

        DEFINE l_record   ARRAY[2600] OF RECORD
	       lotes_fecha		DATE,
	       lotes_cod	  	SMALLINT,
	       lotes_desc	  	CHAR(40),
	       lotes_correlativo  	SMALLINT
        END RECORD

        DEFINE pos              SMALLINT,
	       HOY		DATE,
	       aux_pausa	CHAR(1),
               sw_1             SMALLINT,
               seg_usuario          CHAR(08),
               siono            CHAR(01),
               cla_where        CHAR(300),
               sel_where        CHAR(300),
               g_lista          CHAR(300),
               g_impre          CHAR(300),
               hora             CHAR(08)
END GLOBALS
###########################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
      
        CALL inicio()
        CALL proceso()
END MAIN
###########################################################################
FUNCTION inicio()

        SELECT USER
        INTO   seg_usuario
        FROM   glo_parametro

        SELECT ruta_listados
        INTO   g_param_dis.ruta_listados
        FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
############################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0211" ATTRIBUTE( BORDER)
	DISPLAY " TABM021                   CATALOGO DE LOTES                                   " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "LOTES"
	      COMMAND "Agrega" "Agrega Lotes"
	          CALL Agrega()
                  CLEAR SCREEN
              COMMAND "Consulta" "Consulta Lotes"
	          CALL Consulta()
                  CLEAR SCREEN
              COMMAND "Modifica" "Modifica Lotes"
	          CALL Modifica()
                  CLEAR SCREEN
              COMMAND "Elimina" "Elimina Lotes"
	          CALL Elimina()
                  CLEAR SCREEN
              COMMAND "Salir" "Salir del Programa"
	          EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
	INITIALIZE g_reg.* TO NULL
	DISPLAY BY NAME g_reg.*
	LET g_reg.lotes_fecha = HOY
	DISPLAY BY NAME g_reg.lotes_fecha
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.lotes_desc = NULL
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD lotes_fecha
	             LET g_reg.lotes_fecha = HOY
	             DISPLAY BY NAME g_reg.lotes_fecha
		     NEXT FIELD lotes_cod
	      AFTER FIELD lotes_cod
		    IF g_reg.lotes_cod IS NULL THEN
		       ERROR "Codigo de Lote NO puede ser nulo"
		       NEXT FIELD  lotes_cod
		    END IF

		    SELECT "X" 
                    FROM tab_lote
		    WHERE lotes_cod = g_reg.lotes_cod

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Lote ya Ingresado"
		       NEXT FIELD lotes_cod
		    END IF
              AFTER FIELD lotes_desc
		     IF g_reg.lotes_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  lotes_desc
		     END IF 

		     SELECT "X" 
                     FROM tab_lote
		     WHERE lotes_desc = g_reg.lotes_desc

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Lote ya Ingresado"
		        NEXT FIELD lotes_cod
		     END IF
              AFTER FIELD lotes_correlativo
		     IF g_reg.lotes_correlativo IS NULL THEN
		        ERROR "Correlativo de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_correlativo
		     END IF 
              AFTER FIELD lotes_num
		     IF g_reg.lotes_num IS NULL THEN
		        ERROR "Numero de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_num
		     END IF 
	      ON KEY ( ESC )
		     IF g_reg.lotes_fecha IS NULL THEN
		        ERROR "Fecha de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_fecha
		     END IF

		     IF g_reg.lotes_cod IS NULL THEN
		        ERROR "Codigo de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_cod
		     END IF

		     IF g_reg.lotes_desc IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  lotes_desc
		     END IF 

		     IF g_reg.lotes_correlativo IS NULL THEN
		        ERROR "Correlativo de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_correlativo
		     END IF 

		     IF g_reg.lotes_num IS NULL THEN
		        ERROR "Numero de Lote NO puede ser nulo"
		        NEXT FIELD  lotes_num
		     END IF 

                     INSERT INTO tab_lote VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD lotes_cod
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
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0212" ATTRIBUTE(BORDER)
	    DISPLAY " (ENTER) Consulta               (Ctrl-p) Impresion           (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                             L   O   T   E   S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
            LET int_flag = FALSE

            CONSTRUCT cla_where ON   lotes_fecha,
                                     lotes_cod 
                                FROM lotes_fecha,
                                     lotes_cod
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

            LET sel_where = "SELECT * FROM tab_lote WHERE ",cla_where CLIPPED,
                            "ORDER BY 1 "
  
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
	       ERROR "ARCHIVO DE LOTES.... VACIO"
	   END IF
       END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()


        LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0212" ATTRIBUTE( BORDER)
           DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                 Escoja con < ENTER > el lote a modificar                      " AT 2,2 
	   DISPLAY "                           L     O     T    E    S                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
           LET int_flag = FALSE

           CONSTRUCT cla_where ON   lotes_fecha,
                                    lotes_cod 
                               FROM lotes_fecha,
                                    lotes_cod
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

           LET sel_where = "SELECT * FROM tab_lote WHERE ",cla_where CLIPPED,
                            "ORDER BY 1 "
  
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
                   LET g_reg.lotes_fecha = l_record[pos].lotes_fecha
                   LET g_reg.lotes_cod      = l_record[pos].lotes_cod     
                   LET g_reg.lotes_desc = l_record[pos].lotes_desc
                   LET g_reg.lotes_correlativo = l_record[pos].lotes_correlativo
                   EXIT DISPLAY
                ON KEY (INTERRUPT)
	           ERROR "Usted debe escojer un registro"
                   LET pos = ARR_CURR()
	      END DISPLAY

	      CLOSE WINDOW ventana_2

	      DISPLAY "" AT 1,1
	      DISPLAY "" AT 2,1
	      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
              DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

	      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
                 BEFORE FIELD lotes_fecha
                    NEXT FIELD lotes_correlativo
                 AFTER FIELD lotes_correlativo
		    IF g_reg.lotes_correlativo IS NULL THEN
                       ERROR "Correlativo de Lotes NO puede ser nulo"
                       NEXT FIELD  lotes_correlativo
                    END IF 

                 CALL Pregunta()

                 IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE tab_lote SET
                           lotes_correlativo = g_reg.lotes_correlativo
                    WHERE  lotes_fecha = g_reg.lotes_fecha
                    AND    lotes_cod   = g_reg.lotes_cod  

		    ERROR "REGISTRO MODIFICADO" 
                    SLEEP 2

                    CALL Inicializa()
                 ELSE
                    ERROR "PROCESO DE MODIFICACION,CANCELADO"
                    SLEEP 2
                 END IF
                 ERROR ""

    	         EXIT INPUT
	      ON KEY ( INTERRUPT )
                 CALL Inicializa()
                 EXIT INPUT
  	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE LOTES.... VACIO"
	END IF
     END IF
END FUNCTION
################################################################################
FUNCTION Elimina()

         LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0212" ATTRIBUTE( BORDER)
            DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                   Escoja con < ENTER > el lote a eliminar                     " AT 2,2 
            DISPLAY "                            L    O     T     E     S                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

   
            LET int_flag = FALSE

            CONSTRUCT cla_where ON   lotes_fecha,
                                     lotes_cod 
                                FROM lotes_fecha,
                                     lotes_cod
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

            LET sel_where = "SELECT * FROM tab_lote WHERE ",cla_where CLIPPED,
                            "ORDER BY 1 "
  
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
	           LET g_reg.lotes_fecha = l_record[pos].lotes_fecha
                   LET g_reg.lotes_cod      = l_record[pos].lotes_cod     
                   LET g_reg.lotes_desc = l_record[pos].lotes_desc
                   LET g_reg.lotes_correlativo = l_record[pos].lotes_correlativo
                   EXIT DISPLAY
                ON KEY (INTERRUPT)
	           ERROR "Usted debe escojer un registro"
                   LET pos = ARR_CURR()
               END DISPLAY

	       CLOSE WINDOW ventana_2

	       DISPLAY "" AT 1,1
               DISPLAY "" AT 2,1
               DISPLAY " ( Esc ) Elimina                         (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
               DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
	       DISPLAY BY NAME  g_reg.*

               CALL Pregunta()

               IF aux_pausa MATCHES "[Ss]" THEN
                  DELETE FROM tab_lote
                  WHERE lotes_fecha = g_reg.lotes_fecha
                  AND   lotes_cod = g_reg.lotes_cod

                  ERROR "REGISTRO ELIMINADO" 
                  SLEEP 2
               ELSE
                  ERROR "ELIMINAR CANCELADO" 
                  SLEEP 2
               END IF

               ERROR ""
               CALL Inicializa()
            ELSE
               ERROR "ARCHIVO DE LOTES.... VACIO"
            END IF
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
                 ".IMPTABLOTE",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tablotes TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.lotes_fecha       = l_record[i].lotes_fecha
       LET g_reg.lotes_cod         = l_record[i].lotes_cod     
       LET g_reg.lotes_desc        = l_record[i].lotes_desc
       LET g_reg.lotes_correlativo = l_record[i].lotes_correlativo
   
       IF g_reg.lotes_fecha IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tablotes(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tablotes

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tablotes(g_reg)
   DEFINE g_reg			RECORD 
	  lotes_fecha		DATE,
	  lotes_cod	  	SMALLINT,
	  lotes_desc	  	CHAR(40),
	  lotes_correlativo  	SMALLINT,
          lotes_num             SMALLINT
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM021 ",
               COLUMN 24," LISTADO DE CATALOGO DE LOTES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"FECHA ",
               COLUMN 14,"CODIGO",
               COLUMN 27,"DESCRIPCION",
               COLUMN 60,"CORR"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 1,g_reg.lotes_fecha USING "dd-mm-yyyy",
               COLUMN 15,g_reg.lotes_cod USING "###",
               COLUMN 20,g_reg.lotes_desc,
               COLUMN 60,g_reg.lotes_correlativo USING "####"
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
