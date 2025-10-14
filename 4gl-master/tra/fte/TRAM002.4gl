##############################################################################
#Owner             => E.F.P.
#Programa TRAM002  => CATALOGO ARCHIVO DE TIPOS DE COMPROBANTES
#Fecha creacion    => 07 Julio de 2000  
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 04 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis     RECORD LIKE dis_parametro.*
 
	DEFINE aux_pausa		CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               pos                      SMALLINT,
               cla_where                CHAR(300),
               usuario                  CHAR(08),
               sel_where                CHAR(300),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300)

        DEFINE g_reg		RECORD 
	       tipcom_cod		SMALLINT,
	       tipcom_desc	  	CHAR(20)
	END RECORD

        DEFINE l_record         ARRAY[100] OF RECORD
               codigo                   SMALLINT,
               descripcion              CHAR(20)
        END RECORD
   
        DEFINE RUTA                     CHAR(100) 
         
        DEFINE  g_glob            RECORD
                codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
                razon_social      LIKE safre_af:tab_afore_local.razon_social
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
   INTO usuario
   FROM glo_parametro

   SELECT A.ruta_listados
   INTO   RUTA
   FROM   seg_modulo A
   WHERE  A.modulo_cod = "tra"

   SELECT codigo_afore,
          razon_social
   INTO g_glob.*
   FROM safre_af:tab_afore_local


END FUNCTION

FUNCTION proceso()

	LET HOY  = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TRAM0021" ATTRIBUTE( BORDER)
	DISPLAY " TRAM002         CATALOGO DE COMPROBANTES ICEFA-AFORE IMSS                     " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO COMPROBANTES"
		COMMAND "Agrega" "Agrega Comprobantes."
		                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Comprobantes."
		                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Comprobantes."
		                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Comprobantes."
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
        LET g_reg.tipcom_desc = NULL
        LET sw_1 = 0
	INPUT BY NAME  g_reg.*
	      BEFORE FIELD tipcom_cod
                  IF sw_1 = 0 THEN
                     LET sw_1 = 1
	             SELECT MAX(tipcom_cod) 
                     INTO g_reg.tipcom_cod 
                     FROM tab_tipo_com

                     IF g_reg.tipcom_cod = 0 OR g_reg.tipcom_cod IS NULL THEN
	                LET g_reg.tipcom_cod = 1
	             ELSE
			LET g_reg.tipcom_cod = g_reg.tipcom_cod + 1
		     END IF
                     DISPLAY BY NAME g_reg.*
                  END IF
	      AFTER FIELD tipcom_cod
		    IF g_reg.tipcom_cod IS NULL THEN
		       ERROR "Codigo de comprobante NO puede ser nulo"
		       NEXT FIELD  tipcom_cod
		    END IF

                    SELECT "X" 
                    FROM tab_tipo_com
                    WHERE tipcom_cod = g_reg.tipcom_cod

                    IF STATUS <> NOTFOUND THEN
		       ERROR "Codigo Ya Ingresado"
	               NEXT FIELD tipcom_cod
                    END IF 
	      BEFORE FIELD tipcom_desc
		     IF g_reg.tipcom_cod IS NULL OR  g_reg.tipcom_cod = 0 THEN
		        ERROR "Codigo de comprobante NO puede ser nulo"
			NEXT FIELD  tipcom_cod
		     END IF 
              AFTER FIELD tipcom_desc
		     IF g_reg.tipcom_desc IS NULL THEN
		        ERROR "Descripcion de comprobante NO puede ser nula"
		        NEXT FIELD  tipcom_desc
		     END IF 

                     SELECT "X" 
                     FROM tab_tipo_com
                     WHERE tipcom_desc = g_reg.tipcom_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Descripcion Ya Ingresada"
	                NEXT FIELD tipcom_cod
                     END IF 
	      ON KEY ( ESC )
		     IF g_reg.tipcom_cod IS NULL THEN
		        ERROR "Codigo de comprobante NO puede ser NULO"
		        NEXT FIELD tipcom_cod
		     END IF

		     IF g_reg.tipcom_desc IS NULL THEN
		        ERROR "Descripcion de comprobante NO puede ser NULO"
                        NEXT FIELD tipcom_desc
		     END IF

                     SELECT "X" 
                     FROM tab_tipo_com
                     WHERE tipcom_desc = g_reg.tipcom_desc

                     IF STATUS <> NOTFOUND THEN
		        ERROR "Descripcion Ya Ingresada"
	                NEXT FIELD tipcom_cod
                     END IF 

                     INSERT INTO tab_tipo_com VALUES ( g_reg.* ) 

		     ERROR "REGISTRO INGRESADO" 
                     SLEEP 1
		     ERROR ""

                     CALL Inicializa()
		     NEXT FIELD tipcom_cod
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TRAM0022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                            C O M P R O B A N T E S                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON tipcom_cod FROM tipcom_cod
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
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_tipo_com WHERE ",cla_where CLIPPED,
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
         ERROR "ARCHIVO DE COMPROBANTES... VACIO"
         SLEEP 1
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TRAM0022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > el comprobante modificar                 " AT 2,1
      DISPLAY "                             C O M P R O B A N T E S                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON tipcom_cod FROM tipcom_cod
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
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_tipo_com WHERE ",cla_where CLIPPED,
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
	        LET g_reg.tipcom_cod = l_record[pos].codigo
                LET g_reg.tipcom_desc = l_record[pos].descripcion
                EXIT DISPLAY
             ON KEY (INTERRUPT)
	        ERROR "Usted debe escojer un registro"
                LET pos = ARR_CURR()
	  END DISPLAY
	  CLOSE WINDOW ventana_2
      ELSE
          ERROR "REGISTRO DE COMPROBANTES .... NO EXISTE"
          SLEEP 1
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD tipcom_cod
            NEXT FIELD tipcom_desc
            AFTER FIELD tipcom_desc

            IF g_reg.tipcom_desc IS NULL THEN
               ERROR "Descripcion de comprobante NO puede ser nula"
               NEXT FIELD  tipcom_desc
            END IF 
    
            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_tipo_com SET
                      tipcom_desc = g_reg.tipcom_desc
               WHERE tipcom_cod = g_reg.tipcom_cod

       	       ERROR "REGISTRO MODIFICADO" 
               SLEEP 1
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR,CANCELADO"
               SLEEP 1
            END IF

	    ERROR ""
            EXIT INPUT
	 ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE COMPROBANTES... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TRAM0022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > el comprobante eliminar                  " AT 2,1
      DISPLAY "                             C O M P R O B A N T E S                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON tipcom_cod FROM tipcom_cod
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
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT * FROM tab_tipo_com WHERE ",cla_where CLIPPED,
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
	       LET g_reg.tipcom_cod = l_record[pos].codigo
               LET g_reg.tipcom_desc = l_record[pos].descripcion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE COMPROBANTES .... NO EXISTE"
         SLEEP 1
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
            DELETE FROM tab_tipo_com
            WHERE tipcom_cod = g_reg.tipcom_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 1
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 1
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE COMPROBANTES  .... VACIO"
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

   LET g_impre = RUTA CLIPPED,"/",usuario CLIPPED,
                 ".IMPTABTIPCOM",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabtipcom TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.tipcom_cod = l_record[i].codigo
       LET g_reg.tipcom_desc = l_record[i].descripcion
   
       IF g_reg.tipcom_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabtipcom(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabtipcom

   ERROR "LISTADO GENERADO..."
   SLEEP 1
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabtipcom(g_reg)
   DEFINE g_reg		RECORD 
          tipcom_cod		SMALLINT,
          tipcom_desc	  	CHAR(20)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TRAM002 ",

               COLUMN 35,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED,
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         PRINT COLUMN 15,"LISTADO DE CATALOGO DE COMPROBANTES TRA-AFO-IMSS"
         SKIP 2 LINE

         PRINT COLUMN 05,"CODIGO ",
               COLUMN 20,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 03,g_reg.tipcom_cod USING "####&",
               COLUMN 20,g_reg.tipcom_desc
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
