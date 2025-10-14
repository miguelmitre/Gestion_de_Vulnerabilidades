################################################################################
#Proyecto          => Sistema de Afores.( EXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa TABM024  => CATALOGO ARCHIVO DE TIPOS DE MOVIMIENTO                  #
#Fecha             => 27 Noviembre 1996. 				       #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Fecha modifica    => 17 Diciembre 1999.                                       #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af

GLOBALS
        DEFINE g_param_dis            RECORD LIKE seg_modulo.*

        DEFINE g_reg                  RECORD LIKE tab_movimiento.*

        DEFINE l_record ARRAY[300] OF RECORD LIKE tab_movimiento.*

	DEFINE HOY		DATE,
               aux_pausa        CHAR(01),
               seg_usuario          CHAR(08),
               pos              SMALLINT,
               cla_where        CHAR(100),
               sel_where        CHAR(100),
               g_impre          CHAR(300),
               g_lista          CHAR(300),
               hora             CHAR(08)
END GLOBALS
################################################################################
MAIN
	OPTIONS 
                PROMPT LINE LAST,
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
###############################################################################
FUNCTION proceso()

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0241" ATTRIBUTE( BORDER)
	DISPLAY " TABM024               CATALOGO DE TIPOS DE MOVIMIENTOS                        " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "TIPOS MOVIMIENTOS"
	   COMMAND "Agrega" "Agrega Tipos"
	      CALL Agrega()
              CLEAR SCREEN
           COMMAND "Consulta" "Consulta Tipos"
	      CALL Consulta()
              CLEAR SCREEN
           COMMAND "Modifica" "Modifica Tipos"
	      CALL Modifica()
              CLEAR SCREEN
           COMMAND "Elimina" "Elimina Tipos"
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
	DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                                " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

	INPUT BY NAME  g_reg.*
	      AFTER FIELD codigo
		    IF g_reg.codigo IS NULL THEN
		       ERROR "Tipo de Comision no puede ser nulo"
		       NEXT FIELD  codigo
		    END IF
              AFTER FIELD descripcion
		     IF g_reg.descripcion IS NULL THEN
		        ERROR "Codigo de Subcuenta NO puede ser nulo"
		        NEXT FIELD  descripcion
		     END IF 
	      AFTER FIELD tipo
		    IF g_reg.tipo IS NULL THEN
		       ERROR "Valor de Porcentaje no puede ser nulo"
		       NEXT FIELD  tipo
		    END IF
              ON KEY ( ESC )
		    IF g_reg.codigo IS NULL THEN
		       ERROR "Tipo de Comision no puede ser nulo"
		       NEXT FIELD  codigo
		    END IF

		    IF g_reg.descripcion IS NULL THEN
		       ERROR "Codigo de Subcuenta NO puede ser nulo"
		       NEXT FIELD  descripcion
		    END IF 

		    IF g_reg.tipo IS NULL THEN
		       ERROR "Valor de Porcentaje no puede ser nulo"
		       NEXT FIELD  tipo
		    END IF

		    SELECT "X" 
                    FROM   tab_movimiento
		    WHERE  codigo      = g_reg.codigo

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Tipo de ovimiento ya Ingresado"
		       NEXT FIELD codigo
		    END IF

                    INSERT INTO tab_movimiento VALUES ( g_reg.* ) 

		    ERROR "REGISTRO INGRESADO" 
                    SLEEP 2
		    ERROR ""

                    CALL Inicializa()
		    NEXT FIELD codigo
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
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0242" ATTRIBUTE( BORDER)
	    DISPLAY " (ENTER) Consulta               (Ctrl-p) Impresion           (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                            TIPOS DE MOVIMIENTOS                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

            LET int_flag = FALSE

            CONSTRUCT cla_where ON codigo FROM codigo
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

            LET sel_where = "SELECT * FROM tab_movimiento WHERE ",
                             cla_where CLIPPED,
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
	       ERROR "ARCHIVO DE dis_cuenta.... VACIO"
	    END IF
	 END IF
END FUNCTION
################################################################################
FUNCTION  Modifica()

        LET pos = 2 
	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0242" ATTRIBUTE( BORDER)
           DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	   DISPLAY "                 Escoja con < ENTER > el movimiento a modificar                " AT 2,1 
	   DISPLAY "                               TIPOS DE MOVIMIENTOS                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 


           LET int_flag = FALSE

           CONSTRUCT cla_where ON codigo FROM codigo
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

           LET sel_where = "SELECT * FROM tab_movimiento WHERE ",
                            cla_where CLIPPED,
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
	            LET g_reg.codigo       =  l_record[pos].codigo
                    LET g_reg.descripcion  =  l_record[pos].descripcion
                    LET g_reg.tipo         =  l_record[pos].tipo
                    EXIT DISPLAY
                 ON KEY (INTERRUPT)
		    ERROR "Usted debe escojer un registro"
                    LET pos = ARR_CURR()
	      END DISPLAY
	      CLOSE WINDOW ventana_2
           ELSE
              ERROR "REGISTRO DE TIPOS MOVIMIENTO....NO EXISTE"
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
              BEFORE FIELD codigo
	          NEXT FIELD descripcion
              AFTER FIELD descripcion
	          IF g_reg.descripcion IS NULL THEN
                     ERROR "La descripcion NO puede ser nula"
	             NEXT FIELD descripcion
                  END IF 
              AFTER FIELD tipo
	          IF g_reg.tipo IS NULL THEN
                     ERROR "Tipo de ovimiento NO puede ser nulo"
                     NEXT FIELD tipo
                  END IF 

                  CALL Pregunta()

                  IF aux_pausa MATCHES "[Ss]" THEN
                     UPDATE tab_movimiento SET
                            descripcion = g_reg.descripcion,
                            tipo = g_reg.tipo
	             WHERE  codigo   = g_reg.codigo

   	             ERROR "REGISTRO MODIFICADO" 
                     SLEEP 2

                     CALL Inicializa()
                  ELSE
                     ERROR "PORCESO DE MODIFICACION,CANCELADO"
                     SLEEP 2
                  END IF
                  ERROR ""
   	          EXIT INPUT
   	       ON KEY ( INTERRUPT )
                  CALL Inicializa()
                  EXIT INPUT
     	   END INPUT
   	ELSE
   	   ERROR "ARCHIVO DE TIPOS DE MOVIMIENTOS.... VACIO"
	END IF
END FUNCTION
################################################################################
FUNCTION Elimina()

         LET pos = 2 
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0242" ATTRIBUTE( BORDER)
            DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                 Escoja con < ENTER > el movimiento a eliminar                 " AT 2,1 ATTRIBUTE(REVERSE,BOLD)
	    DISPLAY "                       TIPOS DE MOVIMIENTOS                                    " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

            LET int_flag = FALSE

            CONSTRUCT cla_where ON codigo FROM codigo
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

            LET sel_where = "SELECT * FROM tab_movimiento WHERE ",
                             cla_where CLIPPED,
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
	             LET g_reg.codigo      =  l_record[pos].codigo
                     LET g_reg.descripcion =  l_record[pos].descripcion
                     LET g_reg.tipo        =  l_record[pos].tipo
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
	             ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
	       END DISPLAY
	       CLOSE WINDOW ventana_2
            ELSE
               ERROR "REGISTRO DE TIPOS MOVIMIENTO....NO EXISTE"
               SLEEP 2
               ERROR ""
               CLOSE WINDOW ventana_2
               RETURN
            END IF

	       DISPLAY "" AT 1,1
	       DISPLAY "" AT 2,1
	       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
               DISPLAY " ELIINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

	       DISPLAY BY NAME  g_reg.*

               CALL Pregunta()

               IF aux_pausa MATCHES "[Ss]" THEN
                  DELETE FROM tab_movimiento
                  WHERE codigo  = g_reg.codigo
                  AND   descripcion      = g_reg.descripcion
                  AND   tipo             = g_reg.tipo

                  ERROR "REGISTRO ELIINADO" 
                  SLEEP 2
               ELSE
                  ERROR "ELIINAR CANCELADO" 
                  SLEEP 2
               END IF

               ERROR ""
               CALL Inicializa()
            ELSE
               ERROR "ARCHIVO DE TIPOS DE MOVIMIENTO.... VACIO"
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
                 ".IMPTIPOSMVT",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tiposmvtos TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.codigo      =  l_record[i].codigo
       LET g_reg.descripcion =  l_record[i].descripcion
       LET g_reg.tipo        =  l_record[i].tipo
   
       IF g_reg.codigo IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tiposmvtos(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tiposmvtos

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tiposmvtos(g_reg)

   DEFINE g_reg                  RECORD LIKE tab_movimiento.*

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM024 ",
               COLUMN 20," LISTADO DE CATALOGO DE TIPOS DE MOTIVOS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO ",
               COLUMN 30,"DESCRIPCION ",
               COLUMN 64,"TIPO"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.codigo USING "##&",
               COLUMN 10,g_reg.descripcion,
               COLUMN 66,g_reg.tipo USING "##&"
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
