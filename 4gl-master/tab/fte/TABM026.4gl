#########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                      #
#Owner             => E.F.P                                             #
#Programa TABM026  => CATALOGO ARCHIVO A TIPO DE DOCUMENTOS             #
#Fecha             => 08 Febrero 1997.                                  #
#By                => ADRIAN PEREZ MORALES                              #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ.                  #
#Fecha modifoca    => 17 Diciembre 1999.                                #
#Sistema           => TAB. 					        #
#########################################################################
DATABASE safre_af

GLOBALS
        DEFINE g_param_dis              RECORD LIKE dis_parametro.*

        DEFINE g_reg			RECORD LIKE tab_tipo_documento.*

        DEFINE l_record   ARRAY[300] OF RECORD 
               codigo                   SMALLINT,
               descripcion              CHAR(40),
               obligatorio              CHAR(1)  
        END RECORD

        DEFINE HOY		        DATE,
               aux_pausa                CHAR(1),
               seg_usuario                  CHAR(08),
               pos                      SMALLINT,
               cla_where                CHAR(100),
               sel_where                CHAR(100),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300),
               hora                     CHAR(08)
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
###############################################################################
FUNCTION inicio()
        SELECT USER
        INTO   seg_usuario
        FROM   glo_parametro
     
        SELECT ruta_spool
        INTO   g_param_dis.ruta_spool
        FROM   dis_parametro
END FUNCTION
###############################################################################
FUNCTION proceso()

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0261" ATTRIBUTE( BORDER)
	DISPLAY " TABM026              CATALOGO DE TIPOS DE DOCUMENTOS                          " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO TIPO DE DOCUMENTOS"
           COMMAND "Agrega" "Agrega Tipo de Documentos"
              CALL Agrega()
              CLEAR SCREEN
           COMMAND "Consulta" "Consulta Tipo de Documentos"
	      CALL Consulta()
              CLEAR SCREEN
           COMMAND "Modifica" "Modifica Tipo de Documentos"
	      CALL Modifica()
              CLEAR SCREEN
           COMMAND "Elimina" "Elimina Tipo de Documentos"
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
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.tipdoc_desc = NULL
        LET g_reg.tipdoc_obli = "S"

	INPUT BY NAME  g_reg.*
	      BEFORE FIELD tipdoc_cod
	         SELECT MAX(tipdoc_cod) 
                 INTO   g_reg.tipdoc_cod 
                 FROM   tab_tipo_documento

                 IF g_reg.tipdoc_cod = 0 OR g_reg.tipdoc_cod IS NULL THEN
	            LET g_reg.tipdoc_cod = 1
	         ELSE
		    LET g_reg.tipdoc_cod = g_reg.tipdoc_cod + 1
		 END IF

                 DISPLAY BY NAME g_reg.*
	      AFTER FIELD tipdoc_cod
		 IF g_reg.tipdoc_cod IS NULL THEN
		    ERROR "Codigo de Tipo de Documentos NO puede ser nulo"
		    NEXT FIELD  tipdoc_cod
		 END IF

		 SELECT "X" 
                 FROM   tab_tipo_documento
		 WHERE  tipdoc_cod = g_reg.tipdoc_cod

		 IF STATUS <> NOTFOUND THEN
		    ERROR "Tipo de Documentos ya Ingresada"
		    NEXT FIELD tipdoc_cod
		 END IF
	      BEFORE FIELD tipdoc_desc
		 IF g_reg.tipdoc_cod IS NULL OR  g_reg.tipdoc_cod = 0 THEN
		    ERROR "Codigo de Tipo de Documentos NO puede ser nulo"
	            NEXT FIELD  tipdoc_cod
	         END IF 
              AFTER FIELD tipdoc_desc
	         IF g_reg.tipdoc_desc IS NULL THEN
	            ERROR "Descripcion de Tipo de Documentos NO puede ser nula"
	            NEXT FIELD  tipdoc_desc
	         END IF 

		 SELECT "X" 
                 FROM   tab_tipo_documento
		 WHERE  tipdoc_desc = g_reg.tipdoc_desc

		 IF STATUS <> NOTFOUND THEN
		    ERROR "Tipo de Documentos ya Ingresada"
		    NEXT FIELD tipdoc_cod
		 END IF
              AFTER FIELD tipdoc_obli
                 IF NOT g_reg.tipdoc_obli MATCHES "[SN]"  
		    OR g_reg.tipdoc_obli IS NULL THEN
		    ERROR " Documento Obligatorio (S/N) "
		    NEXT FIELD  tipdoc_obli
                 END IF 

	      ON KEY ( ESC )
		 IF g_reg.tipdoc_cod IS NULL THEN
		    ERROR "Codigo de Tipo de Documentos NO puede ser NULO"
		    NEXT FIELD tipdoc_cod
		 END IF

		 IF g_reg.tipdoc_desc IS NULL THEN
		    ERROR "Descripcion de Tipo de Documentos NO puede ser NULO"
                    NEXT FIELD tipdoc_desc
		 END IF

		 IF g_reg.tipdoc_obli IS NULL THEN
		    ERROR "Codigo de Tipo de Documentos NO puede ser NULO"
		    NEXT FIELD tipdoc_obli
		 END IF

		 SELECT "X" 
                 FROM   tab_tipo_documento
		 WHERE  tipdoc_desc = g_reg.tipdoc_desc

		 IF STATUS <> NOTFOUND THEN
		    ERROR "Tipo de Documentos ya Ingresada"
		    NEXT FIELD tipdoc_cod
		 END IF

                 INSERT INTO tab_tipo_documento VALUES ( g_reg.* ) 

		 ERROR "REGISTRO INGRESADO" 
                 SLEEP 2
		 ERROR ""

                 CALL Inicializa()
		 NEXT FIELD tipdoc_cod
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
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0262" ATTRIBUTE( BORDER)
	    DISPLAY " (ENTER) Consulta             (Ctrl-p) Impresion            (Ctrl-C) Salir     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                             TIPO DE  DOCUMENTOS                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

            LET int_flag = FALSE

            CONSTRUCT cla_where ON tipdoc_cod FROM tipdoc_cod
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

            LET sel_where = "SELECT * FROM tab_tipo_documento WHERE ",cla_where CLIPPED,
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
	       ERROR "ARCHIVO DE TIPO DE DOCUMENTOS.... VACIO"
               SLEEP 2
               ERROR ""
               CLOSE WINDOW ventana_2
               RETURN
            END IF
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
        LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0262" ATTRIBUTE( BORDER)
           DISPLAY " (ENTER) Consulta                                           (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "            Escoja con < ENTER > el tipo de documento a modificar              " AT 2,1 
	   DISPLAY "                            TIPO DE DOCUMENTOS                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON tipdoc_cod FROM tipdoc_cod
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

           LET sel_where = "SELECT * FROM tab_tipo_documento WHERE ",cla_where CLIPPED,
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
	            LET g_reg.tipdoc_cod = l_record[pos].codigo
                    LET g_reg.tipdoc_desc = l_record[pos].descripcion
                    LET g_reg.tipdoc_obli = l_record[pos].obligatorio
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
	            ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
           ELSE
	      ERROR "ARCHIVO DE TIPO DE DOCUMENTOS.... VACIO"
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
                    BEFORE FIELD tipdoc_cod
	   	       NEXT FIELD tipdoc_desc
                    AFTER FIELD tipdoc_desc
		       IF g_reg.tipdoc_desc IS NULL THEN
                          ERROR "Descripcion de Tipo de Documentos NO puede ser nula"
                          NEXT FIELD  tipdoc_desc
                       END IF 
                    AFTER FIELD tipdoc_obli
                       IF NOT g_reg.tipdoc_obli MATCHES "[SN]"  
		          OR g_reg.tipdoc_obli IS NULL THEN
		          ERROR " Documento Obligatorio (S/N) "
		          NEXT FIELD  tipdoc_obli
                        END IF 

                    CALL Pregunta()

                    IF aux_pausa MATCHES "[Ss]" THEN
                       UPDATE tab_tipo_documento SET
                           (tipdoc_desc,tipdoc_obli) = 
			   (g_reg.tipdoc_desc,g_reg.tipdoc_obli)
                       WHERE tipdoc_cod = g_reg.tipdoc_cod

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
	   ERROR "ARCHIVO DE TIPO DE DOCUMENTOS.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Elimina()
         LET pos = 2
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0262" ATTRIBUTE( BORDER)
            DISPLAY " (ENTER) Consulta                                           (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "             Escoja con < ENTER > el tipo de documento a eliminar              " AT 2,1 
	    DISPLAY "                              TIPO DE  DOCUMENTOS                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON tipdoc_cod FROM tipdoc_cod
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

           LET sel_where = "SELECT * FROM tab_tipo_documento WHERE ",cla_where CLIPPED,
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
	            LET g_reg.tipdoc_cod = l_record[pos].codigo
                    LET g_reg.tipdoc_desc = l_record[pos].descripcion
                    LET g_reg.tipdoc_obli = l_record[pos].obligatorio
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
	            ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
           ELSE
	      ERROR "ARCHIVO DE TIPO DE DOCUMENTOS.... VACIO"
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
                 DELETE FROM tab_tipo_documento
                 WHERE tipdoc_cod = g_reg.tipdoc_cod

                 ERROR "REGISTRO ELIMINADO" 
                 SLEEP 2
              ELSE
                 ERROR "ELIMINAR CANCELADO" 
                 SLEEP 2
              END IF

              ERROR ""
              CALL Inicializa()
    	ELSE
    	   ERROR "ARCHIVO DE TIPO DE DOCUMENTOS.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".IMPTABTIPDOC",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabtipdoc TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.tipdoc_cod  = l_record[i].codigo
       LET g_reg.tipdoc_desc = l_record[i].descripcion
       LET g_reg.tipdoc_obli = l_record[i].obligatorio
   
       IF g_reg.tipdoc_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabtipdoc(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabtipdoc

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabtipdoc(g_reg)

   DEFINE g_reg			RECORD LIKE tab_tipo_documento.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM026 ",
               COLUMN 20," LISTADO DE CATALOGO DE TIPO DOCUMENTO ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 25,"DESCRIPCION",
               COLUMN 60,"OBLIGATORIO"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 2,g_reg.tipdoc_cod USING "##&",
               COLUMN 10,g_reg.tipdoc_desc,
               COLUMN 63,g_reg.tipdoc_obli
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
