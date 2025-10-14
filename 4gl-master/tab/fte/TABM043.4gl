######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		     #
#Owner             => E.F.P.                                         #
#Programa TABM043  => CATALOGO ARCHIVO DE TASAS VIVIENDA/RCV       #
#Fecha             =>  28 Enero 1999.     			     #
#By                => SELENA VALENCIA.    			     #
#Fecha Actualiza   => 19 de AGOSTO de 1999.                          #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.               #
#Sistema           => TAB. 					     #
######################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis         RECORD LIKE dis_parametro.*

   DEFINE g_reg			RECORD 
          tasa_origen		CHAR(3),
	  tasa_fecha	  	DATE,
	  tasa_valor	 	DECIMAL(16,6),
          fecha_actualiza       DATE,	
          usuario               CHAR(8)
   END RECORD

   DEFINE l_record   ARRAY[1000] OF RECORD
          tasa_origen         CHAR(3),
          tasa_fecha          DATE,
          tasa_valor          DECIMAL(16,6),
          fecha_actualiza     DATE,
          usuario             CHAR(08)
   END RECORD

   DEFINE
          HOY                   DATE,
          aux_pausa             CHAR(01),
          sw_1                  SMALLINT,
          usuario               CHAR(08),
          siono                 CHAR(01),
          pos                   SMALLINT,
          cla_where             CHAR(200),
          sel_where             CHAR(200),
          g_lista               CHAR(300),
          g_impre               CHAR(300),
          hora                  CHAR(08),
          fecha_1               DATE,
          fecha_2               DATE

END GLOBALS
#####################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   CALL inicio()
   CALL proceso()
END MAIN

FUNCTION inicio()
   SELECT user, * 
   INTO   usuario
   FROM   glo_parametro

   SELECT ruta_spool
   INTO   g_param_dis.ruta_spool
   FROM   dis_parametro
END FUNCTION

FUNCTION proceso()
   LET HOY = DATE
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0431" ATTRIBUTE( BORDER)
   DISPLAY " TABM043             CATALOGO DE TASAS VIVIENDA/RCV                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "TASAS VIVIENDA/RCV"
      COMMAND "Agrega" "Agrega Tasas"
         CALL Agrega()
      COMMAND "Consulta" "Consulta Tasas"
         CALL Consulta()
      COMMAND "Modifica" "Modifica Tasas"
         CALL Modifica()
      COMMAND "Elimina" "Elimina Tasas"
         CALL Elimina()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
######################################################################
FUNCTION Inicializa()
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
   LET sw_1 = 0
END FUNCTION
######################################################################
FUNCTION Agrega()
   LET fecha_1 = DATE

   LET fecha_2 = DATE

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                     AGREGA          " AT 1,1 ATTRIBUTE(GREEN)

   INPUT BY NAME  g_reg.*
      AFTER FIELD tasa_origen
         IF g_reg.tasa_origen IS NULL THEN
            NEXT FIELD tasa_origen
         END IF
         IF g_reg.tasa_origen <> "RCV" AND
            g_reg.tasa_origen <> "VIV" THEN
            ERROR "Tasa no valida"
            NEXT FIELD tasa_origen
         END IF
      AFTER FIELD tasa_fecha
         IF g_reg.tasa_fecha IS NULL THEN
            ERROR "Fecha de la tasa NO puede ser nulo"
            NEXT FIELD  tasa_fecha
         END IF

         SELECT "X"
         FROM tab_tasa_ordinaria
         WHERE tasa_fecha = g_reg.tasa_fecha 
         AND   tasa_origen = g_reg.tasa_origen

         IF STATUS <> NOTFOUND THEN
	    ERROR "TASA YA Ingresado"
	    NEXT FIELD tasa_fecha
         END IF 

         SELECT MAX(tasa_fecha)
         INTO fecha_1
         FROM tab_tasa_ordinaria
         WHERE tasa_origen = g_reg.tasa_origen

         LET fecha_2 = fecha_1 + 1 UNITS MONTH

         IF fecha_2 <> g_reg.tasa_fecha THEN
            ERROR "FECHA TIENE QUE SER LA DEL SIGUIENTE MES"
            NEXT FIELD tasa_fecha
         END IF
            
	 AFTER FIELD tasa_valor
	    IF g_reg.tasa_valor IS NULL OR  g_reg.tasa_valor = 0 THEN
	       ERROR "Valor de la Tasa NO puede ser nulo"
	       NEXT FIELD  tasa_valor
	    END IF

         ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT
         ON KEY (ESC)
            IF g_reg.tasa_origen IS NULL THEN
               NEXT FIELD tasa_origen
            END IF
            IF g_reg.tasa_origen <> "RCV" AND
               g_reg.tasa_origen <> "VIV" THEN
               ERROR "Tasa no valida"
               NEXT FIELD tasa_origen
            END IF

            IF g_reg.tasa_fecha IS NULL THEN
               ERROR "Fecha de la tasa NO puede ser nulo"
               NEXT FIELD  tasa_fecha
            END IF

            SELECT "X"
            FROM tab_tasa_ordinaria
            WHERE tasa_fecha = g_reg.tasa_fecha
            AND   tasa_origen = g_reg.tasa_origen

            IF STATUS <> NOTFOUND THEN
	       ERROR "TASA YA Ingresado"
	       NEXT FIELD tasa_fecha
            END IF 

	    IF g_reg.tasa_valor IS NULL OR  g_reg.tasa_valor = 0 THEN
	       ERROR "Valor de la Tasa NO puede ser nulo"
	       NEXT FIELD  tasa_valor
	    END IF

            LET g_reg.fecha_actualiza = TODAY
            LET g_reg.usuario = usuario 

            INSERT INTO tab_tasa_ordinaria VALUES ( g_reg.* ) 

	    ERROR "REGISTRO INGRESADO"
            SLEEP 2
	    ERROR ""

            CALL Inicializa()
            EXIT INPUT
           
   END INPUT
   CLEAR SCREEN
END FUNCTION
######################################################################
FUNCTION Consulta()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0432" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta             (Ctrl-C) Salir             (Ctrl-P) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                     TASAS APLICABLES A RCV Y VIVIENDA                         " AT 3,1 ATTRIBUTE(REVERSE,GREEN) 

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON tasa_origen,tasa_fecha FROM tasa_origen,tasa_fecha
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

      LET sel_where = "SELECT * FROM tab_tasa_ordinaria WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 " 
   
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
         ERROR "REGISTROS DE TASA ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
######################################################################
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0432" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                 Escoja con < ENTER > la tasa a modificar                      " AT 2,1 
      DISPLAY "                     TASAS APLICABLES A RCV Y VIVIENDA                         " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON tasa_origen,tasa_fecha FROM tasa_origen,tasa_fecha
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

      LET sel_where = "SELECT * FROM tab_tasa_ordinaria WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 " 
   
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.tasa_origen     = l_record[pos].tasa_origen
               LET g_reg.tasa_fecha      = l_record[pos].tasa_fecha
               LET g_reg.tasa_valor      = l_record[pos].tasa_valor
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_reg.usuario         = l_record[pos].usuario
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE TASA ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,GREEN)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD tasa_origen
            NEXT FIELD tasa_valor 

         AFTER FIELD tasa_valor
	    IF g_reg.tasa_valor IS NULL OR  g_reg.tasa_valor = 0 THEN
	       ERROR "Valor de la Tasa NO puede ser nulo"
	       NEXT FIELD  tasa_valor
	    END IF

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN     
               UPDATE tab_tasa_ordinaria SET
                      tasa_valor      = g_reg.tasa_valor,
                      fecha_actualiza = TODAY,
                      usuario         = usuario  #g_reg.usuario 
               WHERE  tasa_origen     = g_reg.tasa_origen
               AND    tasa_fecha      = g_reg.tasa_fecha

	       ERROR "REGISTRO MODIFICADO"
               SLEEP 2
               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICAR, CANCELADO"
               SLEEP 2
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
######################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0432" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                  Escoja con < ENETER > la tasa a borrar                       " AT 2,1 
      DISPLAY "                        TASAS APLICABLES A VIV Y RCV                           " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON tasa_origen,tasa_fecha FROM tasa_origen,tasa_fecha
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

      LET sel_where = "SELECT * FROM tab_tasa_ordinaria WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 " 
   
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.* 
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
	       LET g_reg.tasa_origen     = l_record[pos].tasa_origen
               LET g_reg.tasa_fecha      = l_record[pos].tasa_fecha
               LET g_reg.tasa_valor      = l_record[pos].tasa_valor
               LET g_reg.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_reg.usuario         = l_record[pos].usuario
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTROS DE TASAS....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,GREEN)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_tasa_ordinaria
            WHERE tasa_fecha = g_reg.tasa_fecha
              AND tasa_origen = g_reg.tasa_origen

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE TASAS .... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION
######################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
######################################################################
FUNCTION impresion(pos)
   DEFINE i,
          pos     SMALLINT

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
               ".IMPTABTASA",HOY USING "dd-mm-yyyy","_",hora CLIPPED
  
   START REPORT rpt_tabtasa TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.tasa_origen         = l_record[i].tasa_origen
       LET g_reg.tasa_fecha          = l_record[i].tasa_fecha
       LET g_reg.tasa_valor          = l_record[i].tasa_valor
       LET g_reg.fecha_actualiza     = l_record[i].fecha_actualiza
       LET g_reg.usuario             = l_record[i].usuario

       IF g_reg.tasa_fecha IS NULL OR g_reg.tasa_fecha = "12/31/1899" THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabtasa(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabtasa
  
   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabtasa(g_reg)
  
   DEFINE g_reg			RECORD 
          tasa_origen		CHAR(3),
	  tasa_fecha	  	DATE,
	  tasa_valor	 	DECIMAL(16,6),
          fecha_actualiza       DATE,	
          usuario               CHAR(8)
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
   PAGE HEADER
      PRINT COLUMN 02," TABM043 ",
            COLUMN 24," LISTADO DE CATALOGO DE TASAS ",
            COLUMN 67,TODAY USING "dd-mm-yyyy"
      SKIP 2 LINE

      PRINT COLUMN 01,"ORIGEN",
            COLUMN 08,"FECHA TASA",
            COLUMN 25,"TASA VALOR",
            COLUMN 40,"FECHA ACTUALIZA",
            COLUMN 60,"USUARIO"
      SKIP 1 LINE
  
   ON EVERY ROW
      PRINT COLUMN 02,g_reg.tasa_origen,
            COLUMN 08,g_reg.tasa_fecha USING "dd-mm-yyyy",
            COLUMN 20,g_reg.tasa_valor,
            COLUMN 43,g_reg.fecha_actualiza USING "dd-mm-yyyy",
            COLUMN 60,g_reg.usuario
   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
   ON LAST ROW
      SKIP 4 LINE
      PRINT COLUMN 01," Total de registros : " ,COUNT(*) USING "<<<<<"
END REPORT
