################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa TABM019  => CATALOGO ARCHIVO ENTIDADES RECAUDADORAS.(DIS)            #
#Fecha             => 05 Enero 1996.                                           #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Fecha modifica    => 23 Diciembre 1999.                                       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af

GLOBALS
        DEFINE g_param_dis              RECORD LIKE seg_modulo.*

        DEFINE g_reg			RECORD 
               banco		SMALLINT,
               descripcion	  	CHAR(40)
	END RECORD

        DEFINE l_record   ARRAY[300] OF RECORD
               codigo                   SMALLINT,
               descripcion              CHAR(50)
        END RECORD

	DEFINE HOY		        DATE,
               aux_pausa                CHAR(01),
               SW_1                     SMALLINT,
               pos                      SMALLINT,
               seg_usuario                  CHAR(08),
               cla_where                CHAR(100),
               sel_where                CHAR(100),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300)


END GLOBALS
############################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
############################################################################
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
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0191" ATTRIBUTE( BORDER)
	DISPLAY " TABM019            CATALOGO DE ENTIDADES RECAUDADORAS                         " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "ENT.RECAUDADORAS "
           COMMAND "Agrega" "Agrega Entidades Recaudadoras"
              CALL Agrega()
              CLEAR SCREEN
           COMMAND "Consulta" "Consulta Entidades Recaudadoras"
	      CALL Consulta()
              CLEAR SCREEN
           COMMAND "Modifica" "Modifica Entidades Recaudadoras"
	      CALL Modifica()
              CLEAR SCREEN
           COMMAND "Elimina" "Elimina Entidades Recaudadoras"
	      CALL Elimina()
              CLEAR SCREEN
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
	DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                                " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.descripcion = NULL
        LET sw_1 = 0

	INPUT BY NAME  g_reg.*
	      BEFORE FIELD banco
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(banco) 
                     INTO   g_reg.banco 
                     FROM   tab_banco

                     IF g_reg.banco = 0 OR g_reg.banco IS NULL THEN
	                LET g_reg.banco = 1
	             ELSE
			LET g_reg.banco = g_reg.banco + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD banco
		    IF g_reg.banco IS NULL THEN
		       ERROR "Codigo de Ciudad NO puede ser nulo"
		       NEXT FIELD  banco
		    END IF

		    SELECT "X" 
                    FROM   tab_banco
		    WHERE  banco = g_reg.banco

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Ciudad ya Ingresada"
		       NEXT FIELD banco
		    END IF
	      BEFORE FIELD descripcion
		     IF g_reg.banco IS NULL OR  g_reg.banco = 0 THEN
		        ERROR "Codigo de Ciudad NO puede ser nulo"
			NEXT FIELD  banco
		     END IF 
              AFTER FIELD descripcion
		     IF g_reg.descripcion IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser nula"
		        NEXT FIELD  descripcion
		     END IF 

		     SELECT "X" 
                     FROM   tab_banco
		     WHERE  descripcion = g_reg.descripcion

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Ciudad ya Ingresada"
		        NEXT FIELD banco
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.banco IS NULL THEN
		        ERROR "Codigo de Ciudad NO puede ser NULO"
		        NEXT FIELD banco
		     END IF

		     IF g_reg.descripcion IS NULL THEN
		        ERROR "Descripcion de Ciudad NO puede ser NULO"
                        NEXT FIELD descripcion
		     END IF

		     SELECT "X" 
                     FROM   tab_banco
		     WHERE  descripcion = g_reg.descripcion

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Ciudad ya Ingresada"
		        NEXT FIELD banco
		     END IF

                     INSERT INTO tab_banco VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD banco
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
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0192" ATTRIBUTE( BORDER)
	    DISPLAY " (ENTER) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                            ENTIDADES  RECAUDADORAS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
            LET int_flag = FALSE

            CONSTRUCT cla_where ON banco FROM banco
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

            LET sel_where = "SELECT * FROM tab_banco WHERE ",cla_where CLIPPED,
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
                   ON KEY(control-p)
                      ERROR "PROCESANDO IMPRESION..."
                      CALL impresion(pos)
                   ON KEY (INTERRUPT)
                      EXIT DISPLAY
	       END DISPLAY
	       CLOSE WINDOW ventana_2
	    ELSE
	       ERROR "ARCHIVO DE ENTIDADES RECAUDADORAS.... VACIO"
	    END IF
        END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0192" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                 Escoja con < ENTER > la entidad a modificar                   " AT 2,1 
	   DISPLAY "                             ENTIDADES  RECAUDADORAS                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
           LET int_flag = FALSE

           CONSTRUCT cla_where ON banco FROM banco
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

           LET sel_where = "SELECT * FROM tab_banco WHERE ",cla_where CLIPPED,
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
		      LET g_reg.banco = l_record[pos].codigo
                      LET g_reg.descripcion = l_record[pos].descripcion
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
                 BEFORE FIELD banco
		       NEXT FIELD descripcion
                 AFTER FIELD descripcion
		       IF g_reg.descripcion IS NULL THEN
                          ERROR "Descripcion de Ciudad NO puede ser nula"
                          NEXT FIELD  descripcion
                       END IF 

                 CALL Pregunta()   
        
                 IF aux_pausa MATCHES "[Ss]" THEN
                    UPDATE tab_banco SET
                           descripcion = g_reg.descripcion
                    WHERE banco = g_reg.banco

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
	   ERROR "ARCHIVO DE ENTIDADES RECAUDADORAS.... VACIO"
	END IF
     END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
	 LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0192" ATTRIBUTE( BORDER)
            DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                 Escoja con < ENTER > la entidad a eliminar                    " AT 2,1 
	    DISPLAY "                            ENTIDADES  RECAUDADORAS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
            LET int_flag = FALSE

            CONSTRUCT cla_where ON banco FROM banco
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

            LET sel_where = "SELECT * FROM tab_banco WHERE ",cla_where CLIPPED,
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
		       LET g_reg.banco = l_record[pos].codigo
                       LET g_reg.descripcion = l_record[pos].descripcion
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
		      ERROR "Usted debe escojer un registro"
                      LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2

	      DISPLAY "" AT 1,1
	      DISPLAY "" AT 2,1
	      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
              DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

	      DISPLAY BY NAME  g_reg.*
              CALL Pregunta()

              IF aux_pausa MATCHES "[Ss]" THEN
                 DELETE FROM tab_banco
                 WHERE banco = g_reg.banco

                 ERROR "REGISTRO ELIMINADO" 
                 SLEEP 2
              ELSE
                 ERROR "ELIMINAR CANCELADO" 
                 SLEEP 2
              END IF
              ERROR ""
              CALL Inicializa()
           ELSE
	      ERROR "ARCHIVO DE ENTIDADES RECAUDADORAS.... VACIO"
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
                 ".IMPTABERECA",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabereca TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.banco  = l_record[i].codigo
       LET g_reg.descripcion = l_record[i].descripcion
   
       IF g_reg.banco IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabereca(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabereca

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabereca(g_reg)

   DEFINE g_reg			RECORD 
          banco		SMALLINT,
          descripcion	  	CHAR(40)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,"TABM019 ",
               COLUMN 15," LISTADO DE CATALOGO DE ENTIDADES RECAUDADORAS ",
               COLUMN 68,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.banco USING "###&",
               COLUMN 20,g_reg.descripcion
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
