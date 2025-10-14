############################################################
#Proyecto          => Sistema de Afores. ( MEXICO )	   #
#Owner             => E.F.P.                               #
#Programa TABM044  => CATALOGO DE DIAS FERIADOS            #   
#Fecha             => 18 de AGOSTO de 1999.                # 
#By                => MIGUEL ANGEL HERNANDEZ MARTINEZ.     #
#Sistema           => TAB. 			           #
############################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_dis    RECORD LIKE seg_modulo.*

   DEFINE g_reg			RECORD 
          feria_fecha           DATE,
          feria_desc            CHAR(50)
   END RECORD

   DEFINE l_record   ARRAY[300] OF RECORD
          feria_fecha           DATE,
          feria_desc            CHAR(50)
   END RECORD

   DEFINE
         aux_pausa                   CHAR(01),
         HOY                         DATE,
         pos                         SMALLINT,
         seg_usuario                     CHAR(08),
         siono                       CHAR(01),
         cli_where                   CHAR(500),
         sel_where                   CHAR(500),
         g_impre                     CHAR(300),
         g_lista                     CHAR(300),
         hora                        CHAR(08)

END GLOBALS
###########################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   
   CALL inicio()
   CALL proceso()
END MAIN
###########################################################
FUNCTION inicio()
   SELECT user, * 
   INTO   seg_usuario
   FROM   glo_parametro

   SELECT ruta_listados
   INTO   g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE modulo_cod = 'tab'
END FUNCTION
###########################################################
FUNCTION proceso()
   LET HOY = DATE
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0441" ATTRIBUTE( BORDER)
   DISPLAY " TABM044                  CATALOGO DE DIAS FERIADOS                            " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
      MENU "DIAS FERIADOS"
         COMMAND "Agrega" "Agrega Dias Feriados"
	    CALL Agrega()
         COMMAND "Consulta" "Consulta Dias Feriados"
            CALL Consulta()
         COMMAND "Modifica" "Modifica Dias Feriados"
            CALL Modifica()
         COMMAND "Elimina" "Elimina Dias Feriados"
            CALL Elimina()
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
      END MENU
   CLOSE WINDOW ventana_1
END FUNCTION
############################################################
FUNCTION Inicializa()
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
END FUNCTION
############################################################
FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega           (Ctrl-c) Salir               AGREGA                  " AT 1,1 ATTRIBUTE(GREEN)

   INPUT BY NAME  g_reg.*
      AFTER FIELD feria_fecha
         IF g_reg.feria_fecha IS NULL THEN
	    ERROR "DEBE INDICAR LA FECHA"
            NEXT FIELD feria_fecha
         END IF

         SELECT "X"
         FROM tab_feriado
         WHERE feria_fecha = g_reg.feria_fecha

         IF STATUS <> NOTFOUND THEN
            ERROR "Fecha YA Ingresado"
	    NEXT FIELD feria_fecha
         END IF 

      AFTER FIELD feria_desc
         IF g_reg.feria_desc IS NULL THEN
            ERROR "La descripcion NO puede ser nula"
            NEXT FIELD  feria_desc
         END IF

      ON KEY (ESC)
         IF g_reg.feria_fecha IS NULL THEN
	    ERROR "DEBE INDICAR LA FECHA"
            NEXT FIELD feria_fecha
         END IF

         SELECT "X"
         FROM tab_feriado
         WHERE feria_fecha = g_reg.feria_fecha

         IF STATUS <> NOTFOUND THEN
            ERROR "Fecha YA Ingresado"
	    NEXT FIELD feria_fecha
         END IF 

         IF g_reg.feria_desc IS NULL THEN
            ERROR "La descripcion NO puede ser nula"
            NEXT FIELD  feria_desc
         END IF

         INSERT INTO tab_feriado VALUES ( g_reg.* ) 

         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         ERROR ""

         CALL Inicializa()
   ON KEY (INTERRUPT)
      CALL Inicializa()
      EXIT INPUT
   END INPUT

   CLEAR SCREEN
END FUNCTION
############################################################
FUNCTION Consulta()
   LET pos = 2  
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0442" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta             (Ctrl-C) Salir             (Ctrl-P) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                               DIAS FERIADOS                                   " AT 3,1 ATTRIBUTE(REVERSE,GREEN) 

      LET int_flag = FALSE

      CONSTRUCT cli_where ON feria_fecha FROM feria_fecha
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

      LET sel_where = "SELECT * FROM tab_feriado WHERE ",cli_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query FROM sel_where
   	
      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
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
         ERROR "REGISTROS DE DIAS FERIADOS....NO EXISTE "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF

   CLEAR SCREEN 
END FUNCTION
############################################################
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0442" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                   Escoja con < ENTER > el dia a modificar                     " AT 2,1 
      DISPLAY "                                DIAS FERIADOS                                  " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE
      CONSTRUCT cli_where ON feria_fecha FROM feria_fecha
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

      LET sel_where = "SELECT * FROM tab_feriado WHERE ",cli_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query1 FROM sel_where
   
      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH
      
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	    DISPLAY ARRAY l_record TO scr_1.* 
               ON KEY (CONTROL-M)
                  LET pos = ARR_CURR()
                  LET g_reg.feria_fecha = l_record[pos].feria_fecha
                  LET g_reg.feria_desc = l_record[pos].feria_desc
                  EXIT DISPLAY
               ON KEY (INTERRUPT)
		  ERROR "Usted debe escojer un registro"
                  LET pos = ARR_CURR()
	     END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE DIAS FERIADOS....NO EXISTE "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,GREEN)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD feria_fecha
            NEXT FIELD feria_desc

         AFTER FIELD feria_desc
            IF g_reg.feria_desc IS NULL THEN
               ERROR "La descripcion NO puede ser nula"
               NEXT FIELD  feria_desc
            END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN     
 
               UPDATE tab_feriado SET
                     feria_fecha =g_reg.feria_fecha,
                     feria_desc =g_reg.feria_desc
               WHERE  feria_fecha = g_reg.feria_fecha

	       ERROR "REGISTRO MODIFICADO"
               SLEEP 1
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR, CANCELADO"
               SLEEP 1
               CALL Inicializa()
            END IF

            ERROR "" 
            EXIT INPUT
         ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "PROCESO DE MODIFICAR, CANCELADO"
   END IF
   CLEAR SCREEN
END FUNCTION
############################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0442" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                     Escoja con < ENTER > el dia a borrar                      " AT 2,1 
      DISPLAY "                                DIA FERIADO                                    " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE
      CONSTRUCT cli_where ON feria_fecha FROM feria_fecha
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

      LET sel_where = "SELECT * FROM tab_feriado WHERE ",cli_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query2 FROM sel_where
   
      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH
      
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.feria_fecha = l_record[pos].feria_fecha
               LET g_reg.feria_desc = l_record[pos].feria_desc
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE DIAS FERIADOS....NO EXISTE "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir  " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,GREEN)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_feriado
            WHERE feria_fecha = g_reg.feria_fecha

            ERROR "REGISTRO ELIMINADO"
            SLEEP 1
         ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 1
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE DIAS FERIADOS.... VACIO"
      SLEEP 1
   END IF
   CLEAR SCREEN
END FUNCTION
############################################################
FUNCTION Pregunta()
	PROMPT " Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
############################################################
FUNCTION impresion(pos)
   DEFINE i,
          pos     INTEGER
   
   LET hora = TIME

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                               ".IMPTABFERIA",HOY USING "DD-MM-YYYY",
                                 "_",hora CLIPPED

   START REPORT rpt_tabferia TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.feria_fecha  = l_record[i].feria_fecha
       LET g_reg.feria_desc   = l_record[i].feria_desc

       IF g_reg.feria_fecha IS NULL OR g_reg.feria_fecha="12/31/1899" THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabferia(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabferia

   ERROR "LISTADO GENERADO"
   SLEEP 1
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
###########################################################
REPORT rpt_tabferia(g_reg)
   DEFINE g_reg			RECORD 
          feria_fecha           DATE,
          feria_desc            CHAR(50)
   END RECORD
  
   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
   PAGE HEADER
      PRINT COLUMN 02," TABM044 ",
            COLUMN 20," LISTADO DE CATALOGO DE DIAS FERIADOS ",
            COLUMN 65,TODAY USING "mm-dd-yyyy"
      SKIP 2 LINE
     
      PRINT COLUMN 07," FECHA ",
            COLUMN 35," DESCRIPCION DE DIA FERIADO "
      SKIP 1 LINE

   ON EVERY ROW
      PRINT COLUMN 05,g_reg.feria_fecha USING "dd-mm-yyyy",
            COLUMN 25,g_reg.feria_desc

   PAGE TRAILER
      SKIP 2 LINES
      PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      
   ON LAST ROW
      SKIP 4 LINE
      PRINT COLUMN 2,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT   
