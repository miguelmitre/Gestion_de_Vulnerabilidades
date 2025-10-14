######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => Carlos Welsh. 			             #
#Programa TABM012  => CATALOGO ARCHIVO SUBCUENTAS                    #
#Fecha             => 09 Diciembre 1996. 		             #
#By                => JUAN DAVID HERNANDEZ OYARCE	             #
#Fecha modifica    => 29 Octubre 1999.                               #
#Modificado por    => MIGUEL ANGEL HERNANDEZ MARTINEZ                #
#Sistema           => TAB. 				             #
######################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis         RECORD LIKE seg_modulo.*

        DEFINE aux_pausa           CHAR(01),
               hoy                 DATE,
               pos                 SMALLINT,
               seg_usuario             CHAR(08),
               sel_where           CHAR(300),
               cla_where           CHAR(300),
               g_impre             CHAR(300),
               g_lista             CHAR(300),
               sw_1                SMALLINT

        DEFINE g_reg	           RECORD 
	       subct_cod	   SMALLINT,
	       subct_desc	   CHAR(40),
               subct_corta         CHAR(05)
	END RECORD

        DEFINE l_record            ARRAY[300] OF RECORD
               codigo              SMALLINT,
               descripcion         CHAR(40),
               corta               CHAR(05)
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
   FROM glo_parametro

   SELECT ruta_listados
   INTO   g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE modulo_cod = 'tab'
END FUNCTION

FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0121" ATTRIBUTE( BORDER)
	DISPLAY " TABM012                 CATALOGO DE SUB-CUENTAS                             " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO SUB-CUENTAS"
		COMMAND "Agrega" "Agrega Sub-cuentas"
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Sub-cuentas"
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Sub-cuentas"
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Sub-cuentas"
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
	DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                                " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET g_reg.subct_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD subct_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(subct_cod) 
                     INTO g_reg.subct_cod 
                     FROM tab_subcuenta

                     IF g_reg.subct_cod = 0 OR g_reg.subct_cod IS NULL THEN
	                LET g_reg.subct_cod = 1
	             ELSE
			LET g_reg.subct_cod = g_reg.subct_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF    
	      AFTER FIELD subct_cod
		    IF g_reg.subct_cod IS NULL THEN
		       ERROR "Codigo de Sub-cuentas NO puede ser nulo"
		       NEXT FIELD  subct_cod
		    END IF

		    SELECT "X" 
                    FROM tab_subcuenta
		    WHERE subct_cod = g_reg.subct_cod

		    IF STATUS <> NOTFOUND THEN
		       ERROR "Sub-cuentas ya Ingresada"
		       NEXT FIELD subct_cod
		    END IF
	      BEFORE FIELD subct_desc
		     IF g_reg.subct_cod IS NULL OR  g_reg.subct_cod = 0 THEN
		        ERROR "Codigo de Sub-cuentas NO puede ser nulo"
			NEXT FIELD  subct_cod
		     END IF 
              AFTER FIELD subct_desc
		     IF g_reg.subct_desc IS NULL THEN
		        ERROR "Descripcion de Sub-cuentas NO puede ser nula"
		        NEXT FIELD  subct_desc
		     END IF 

		     SELECT "X" 
                     FROM tab_subcuenta
		     WHERE subct_desc = g_reg.subct_desc

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Sub-cuentas ya Ingresada"
		        NEXT FIELD subct_cod
		     END IF
	      BEFORE FIELD subct_corta
		     IF g_reg.subct_desc IS NULL THEN
		        ERROR "Descripcion de Sub-cuentas NO puede ser nulo"
			NEXT FIELD  subct_desc
		     END IF 
              AFTER FIELD subct_corta
		     IF g_reg.subct_corta IS NULL THEN
		        ERROR "Descripcion corta de Sub-cuentas NO puede ser nula"
		        NEXT FIELD  subct_corta
		     END IF 

		     SELECT "X" 
                     FROM tab_subcuenta
		     WHERE subct_corta = g_reg.subct_corta

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Sub-cuentas ya Ingresada"
		        NEXT FIELD subct_cod
		     END IF
	      ON KEY ( ESC )
		     IF g_reg.subct_cod IS NULL THEN
		        ERROR "Codigo de Sub-cuentas NO puede ser NULO"
		        NEXT FIELD subct_cod
		     END IF

		     IF g_reg.subct_desc IS NULL THEN
		        ERROR "Descripcion de Sub-cuentas NO puede ser NULO"
                        NEXT FIELD subct_desc
		     END IF

		     SELECT "X" 
                     FROM tab_subcuenta
		     WHERE subct_desc = g_reg.subct_desc

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Sub-cuentas ya Ingresada"
		        NEXT FIELD subct_cod
		     END IF

		     IF g_reg.subct_corta IS NULL THEN
		        ERROR "Descripcion corta de Sub-cuentas NO puede ser nula"
		        NEXT FIELD  subct_corta
		     END IF 

		     SELECT "X" 
                     FROM tab_subcuenta
		     WHERE subct_corta = g_reg.subct_corta

		     IF STATUS <> NOTFOUND THEN
		        ERROR "Sub-cuentas ya Ingresada"
		        NEXT FIELD subct_cod
		     END IF

                     INSERT INTO tab_subcuenta VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD subct_cod
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0122" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                           S U B C U E N T A S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON subct_cod FROM subct_cod
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

      LET sel_where = "SELECT * FROM tab_subcuenta WHERE ",cla_where CLIPPED,
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
	 ERROR "ARCHIVO DE SUB-CUENTAS.... VACIO"
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0122" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > la subcuenta a modificar                 " AT 2,1
      DISPLAY "                            S U B C U E N T A S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON subct_cod FROM subct_cod
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

      LET sel_where = "SELECT * FROM tab_subcuenta WHERE ",cla_where CLIPPED,
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
	       LET g_reg.subct_cod = l_record[pos].codigo
               LET g_reg.subct_desc = l_record[pos].descripcion
               LET g_reg.subct_corta = l_record[pos].corta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SUBCUENTA ... NO EXISTE"
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
         BEFORE FIELD subct_cod
	    NEXT FIELD subct_desc
            AFTER FIELD subct_corta

	    IF g_reg.subct_desc IS NULL THEN
               ERROR "Descripcion de Sub-cuentas NO puede ser nula"
               NEXT FIELD  subct_desc
            END IF 
   
	    IF g_reg.subct_corta IS NULL THEN
               ERROR "Descripcion corta de Sub-cuentas NO puede ser nula"
               NEXT FIELD  subct_corta
            END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_subcuenta SET
                      subct_desc = g_reg.subct_desc,
                      subct_corta = g_reg.subct_corta
               WHERE subct_cod = g_reg.subct_cod

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
      ERROR "ARCHIVO DE SUB-CUENTAS.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0122" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > la subcuenta a eliminar                  " AT 2,1
      DISPLAY "                            S U B C U E N T A S                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON subct_cod FROM subct_cod
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

      LET sel_where = "SELECT * FROM tab_subcuenta WHERE ",cla_where CLIPPED,
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
	       LET g_reg.subct_cod = l_record[pos].codigo
               LET g_reg.subct_desc = l_record[pos].descripcion
               LET g_reg.subct_corta = l_record[pos].corta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE SUBCUENTA .... NO EXISTE"
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
            DELETE FROM tab_subcuenta
            WHERE subct_cod = g_reg.subct_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
      ELSE
         ERROR "ARCHIVO DE SUB-CUENTAS.... VACIO"
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
                 ".IMPTABSUBCT",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabsubct TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.subct_cod = l_record[i].codigo
       LET g_reg.subct_desc = l_record[i].descripcion
       LET g_reg.subct_corta = l_record[i].corta
   
       IF g_reg.subct_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabsubct(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabsubct

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabsubct(g_reg)
   DEFINE g_reg		RECORD 
          subct_cod		SMALLINT,
          subct_desc	  	CHAR(40),
          subct_corta           CHAR(05)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM012 ",
               COLUMN 24," LISTADO DE CATALOGO DE SUBCUENTAS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO ",
               COLUMN 20,"DESCRIPCION ",
               COLUMN 60,"DECS.CORTA"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.subct_cod USING "#####&",
               COLUMN 20,g_reg.subct_desc,
               COLUMN 63,g_reg.subct_corta
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
