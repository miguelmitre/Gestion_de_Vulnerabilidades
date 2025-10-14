################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P. 
#Programa TABM036  => CATALOGO DE ARCHIVO DE CRITERIOS  
#Fecha             =>  7 Mayo 1997.     				       #
#By                => GERARDO ALFONOS VEGA PAREDES.			       #
#Fecha Actualiza   => 13 de ENERO 2000.                                        #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af

GLOBALS
 
        DEFINE g_param_dis             RECORD LIKE seg_modulo.*

        DEFINE g_reg   RECORD 
               criterio_cod		SMALLINT,
               criterio_desc  	        CHAR(50),
               abreviatura	  	        CHAR(05),
               calculo_cod           CHAR(01)	
	END RECORD

        DEFINE l_record   ARRAY[300] OF RECORD
               codigo       SMALLINT, 
               descripcion  CHAR(50),
               abrevia      CHAR(05),
               desde        CHAR(01)
        END RECORD

        DEFINE sw_1                     SMALLINT,
               vaccion                  SMALLINT,
               aux_pausa                CHAR(01),
               HOY                      DATE,
               pos                      SMALLINT,
               seg_usuario                  CHAR(08),
               cla_where                CHAR(300),
               sel_where                CHAR(300),
               g_lista                  CHAR(300),
               g_impre                  CHAR(300),
               hora                     CHAR(08)
END GLOBALS
#############################################################################
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
        FROM   tab_afore_local

        SELECT ruta_listados
        INTO   g_param_dis.ruta_listados
        FROM   seg_modulo
        WHERE modulo_cod = 'tab'
END FUNCTION
#############################################################################
FUNCTION proceso()
	LET HOY = DATE
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0361" ATTRIBUTE( BORDER)
	DISPLAY " TABM036                    CATALOGO DE CRITERIOS                              " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
	
	MENU "CRITERIOS FEDERATIVAS"
	   COMMAND "Agrega" "Agrega Criterio"
              CALL Agrega()
           COMMAND "Consulta" "Consulta Criterio"
	      CALL Consulta()
           COMMAND "Modifica" "Modifica Criterio"
	      CALL Modifica()
           COMMAND "Elimina" "Elimina Criterio"
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
        LET sw_1 = 0
        LET vaccion = 0
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 2,69 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
        LET g_reg.criterio_desc = NULL
        LET sw_1 = 0

	INPUT BY NAME  g_reg.*
	   BEFORE FIELD criterio_cod
              IF sw_1 = 0 THEN
                 LET sw_1 = 1
	         SELECT MAX(criterio_cod) 
                 INTO   g_reg.criterio_cod 
                 FROM   tab_criterio

                 IF g_reg.criterio_cod = 0 OR g_reg.criterio_cod IS NULL THEN
	            LET g_reg.criterio_cod = 1
	         ELSE
		    LET g_reg.criterio_cod = g_reg.criterio_cod + 1
		 END IF
                 DISPLAY BY NAME g_reg.*
              END IF
	   AFTER FIELD criterio_cod
	      IF g_reg.criterio_cod IS NULL THEN
	         ERROR "Codigo de Criterio NO puede ser nulo"
	         NEXT FIELD  criterio_cod
	      END IF

              SELECT "X" 
              FROM   tab_criterio
              WHERE  criterio_cod = g_reg.criterio_cod

              IF STATUS <> NOTFOUND THEN
	         ERROR "Codigo Ya Ingresado"
	         NEXT FIELD criterio_cod
              END IF 
	   BEFORE FIELD criterio_desc
	      IF g_reg.criterio_cod IS NULL OR  g_reg.criterio_cod = 0 THEN
	         ERROR "Codigo de Criterio NO puede ser nulo"
	         NEXT FIELD  criterio_cod
	      END IF 
           AFTER FIELD criterio_desc
	      IF g_reg.criterio_desc IS NULL THEN
	         ERROR "Descripcion de Criterio NO puede ser nula"
	         NEXT FIELD  criterio_desc
	      END IF 

	      SELECT "X" 
              FROM   tab_criterio
	      WHERE  criterio_desc = g_reg.criterio_desc

	      IF STATUS <> NOTFOUND THEN
	         ERROR "Criterio ya Ingresado"
	         NEXT FIELD criterio_cod
	      END IF
	   BEFORE FIELD abreviatura
	      IF g_reg.criterio_desc IS NULL THEN
	         ERROR "Descripcion de Criterio NO puede ser nula"
	         NEXT FIELD  criterio_desc
	      END IF 
           AFTER FIELD abreviatura
	      IF g_reg.abreviatura IS NULL THEN
	         ERROR "Criterio Abreviado  NO puede ser nulo"
	         NEXT FIELD  abreviatura
	      END IF 

	      SELECT "X" 
              FROM   tab_criterio
	      WHERE  abreviatura = g_reg.abreviatura

	      IF STATUS <> NOTFOUND THEN
	         ERROR "Criterio Abreviado ya Ingresado"
	 	 NEXT FIELD abreviatura
	      END IF
           BEFORE FIELD calculo_cod
	      IF g_reg.abreviatura IS NULL THEN
	         ERROR "Criterio Abreviada  NO puede ser nulo"
	         NEXT FIELD  abreviatura
	      END IF 
           AFTER FIELD calculo_cod
	      IF g_reg.calculo_cod IS NULL THEN
	         ERROR "Codigo Postal NO puede ser nulo"
	         NEXT FIELD  calculo_cod
	      END IF 

	      ---SELECT "X" 
              ---FROM   tab_criterio
	      ---WHERE  calculo_cod = g_reg.calculo_cod 

	      ---IF STATUS <> NOTFOUND THEN
	         ---ERROR "Codigo ya Ingresado"
	 	 ---NEXT FIELD calculo_cod
	      ---END IF
	   ON KEY ( ESC )
	      IF g_reg.criterio_cod IS NULL THEN
	         ERROR "Codigo de Criterio NO puede ser NULO"
	         NEXT FIELD criterio_cod
	      END IF

	      IF g_reg.criterio_desc IS NULL THEN
	         ERROR "Descripcion de Criterio NO puede ser NULO"
                 NEXT FIELD criterio_desc
	      END IF

	      IF g_reg.abreviatura IS NULL THEN
	         ERROR "Criterio Abreviada  NO puede ser nulo"
	         NEXT FIELD  abreviatura
	      END IF 

	      IF g_reg.calculo_cod IS NULL THEN
	         ERROR "Codigo NO puede ser nulo"
	         NEXT FIELD  calculo_cod
	      END IF 

	      ---SELECT "X" 
              ---FROM   tab_criterio
	      ---WHERE  calculo_cod = g_reg.calculo_cod 

	      ---IF STATUS <> NOTFOUND THEN
	         ---ERROR "Codigo ya Ingresado"
	 	---NEXT FIELD calculo_cod
	      ---END IF

	      ---SELECT "X" 
              ---FROM   tab_criterio
	      ---WHERE  criterio_desc = g_reg.criterio_desc

	      ---IF STATUS <> NOTFOUND THEN
	         ---ERROR "Criterio ya Ingresado"
	 	 ---NEXT FIELD criterio_cod
	      ---END IF

              INSERT INTO tab_criterio VALUES ( g_reg.* ) 

	      ERROR "REGISTRO INGRESADO" 
              SLEEP 2
	      ERROR ""

              CALL Inicializa()
	      NEXT FIELD criterio_cod
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0362" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta             (Ctrl-C) Salir             (Ctrl-p) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                             C R I T E R I O S                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   
      LET int_flag = FALSE

      CONSTRUCT cla_where ON criterio_cod FROM criterio_cod
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

      LET sel_where = "SELECT * FROM tab_criterio WHERE ",
                      cla_where CLIPPED,
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
               ERROR "PROCESANDO IMPRESION ..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE CRITERIOS.... VACIO"
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0362" ATTRIBUTE(BORDER)
      DISPLAY " (ENTER) Consulta                                        (Ctrl-p) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                Escoja con < ENTER > el criterio a modificar                   " AT 2,1 
      DISPLAY "                             CRITERIOS FEDERATIVAS                             " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON criterio_cod FROM criterio_cod
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

      LET sel_where = "SELECT * FROM tab_criterio WHERE ",
                      cla_where CLIPPED,
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
	       LET g_reg.criterio_cod   = l_record[pos].codigo
               LET g_reg.criterio_desc  = l_record[pos].descripcion
               LET g_reg.abreviatura       = l_record[pos].abrevia
               LET g_reg.calculo_cod = l_record[pos].desde
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
            BEFORE FIELD criterio_cod
            NEXT FIELD criterio_desc
             --  DECLARE xcurcol CURSOR FOR
             --  SELECT "X" 
             --  FROM   com_esq_comis
             --  WHERE  criterio_cod = g_reg.criterio_cod

             --  OPEN xcurcol
             --  FETCH xcurcol

             --  IF STATUS <> 100 THEN
             --     ERROR "No puedes modificar porque existen Calculos con este Criterio"
             --     CLOSE xcurcol
             --     NEXT FIELD criterio_cod
             --  END IF
            AFTER FIELD criterio_desc
	       IF g_reg.criterio_desc IS NULL THEN
                  ERROR "Campo NO puede ser nula"
                  NEXT FIELD  criterio_desc
               END IF 
            AFTER FIELD abreviatura
	       IF g_reg.abreviatura IS NULL THEN
                  ERROR "Campo NO puede ser nula"
                  NEXT FIELD  abreviatura
               END IF 
            AFTER FIELD calculo_cod
	       IF g_reg.calculo_cod IS NULL THEN
	          ERROR "Codigo NO puede ser nulo"
	          NEXT FIELD  calculo_cod
	       END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_criterio SET
                      criterio_desc  = g_reg.criterio_desc,
                      abreviatura       = g_reg.abreviatura,
                      calculo_cod = g_reg.calculo_cod
               WHERE  criterio_cod   = g_reg.criterio_cod

	       ERROR "REGISTRO MODIFICADO" 
               SLEEP 2

               CALL Inicializa()
            ELSE
               ERROR "PROCESO DE MODIFICACION,CANCELADO"
               SLEEP 2
            END IF
            ERROR ""
            CALL Inicializa()
	    EXIT INPUT
	 ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT
         END INPUT
      ELSE
         ERROR "ARCHIVO DE CRITERIOS.... VACIO"
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina() 
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0362" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                        (Ctrl-p) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con < ENTER > el criterio a eliminar                     " AT 2,1 
      DISPLAY "                            C R I T E R I O S                                  " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON criterio_cod FROM criterio_cod
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

      LET sel_where = "SELECT * FROM tab_criterio WHERE ",
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
	       LET g_reg.criterio_cod   = l_record[pos].codigo
               LET g_reg.criterio_desc  = l_record[pos].descripcion
               LET g_reg.abreviatura       = l_record[pos].abrevia
               LET g_reg.calculo_cod = l_record[pos].desde
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2

         --DECLARE xcurcol2 CURSOR FOR
         --SELECT "X" 
         --FROM   com_esq_comis
         --WHERE  criterio_cod = g_reg.criterio_cod 

         --OPEN xcurcol2
         --FETCH xcurcol2

         --IF STATUS <> 100 THEN
          --  ERROR "No puedes Eliminar Criterio porque existen Calculos con este Codigo"
           -- SLEEP 3
           -- CLOSE xcurcol2
           -- ERROR "" 
           -- CALL Inicializa()
           -- RETURN
         --END IF

         DISPLAY "" AT 1,1
         DISPLAY "" AT 2,1
         DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

	 DISPLAY BY NAME  g_reg.*

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_criterio
            WHERE criterio_cod = g_reg.criterio_cod

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO" 
            SLEEP 2
         END IF
         ERROR ""
         CALL Inicializa()
      ELSE
         ERROR "ARCHIVO DE CRITERIOS  FEDERATIVAS.... VACIO"
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
                 ".IMPTABCRITE",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabcriterio TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.criterio_cod   = l_record[i].codigo
       LET g_reg.criterio_desc  = l_record[i].descripcion
       LET g_reg.abreviatura       = l_record[i].abrevia
       LET g_reg.calculo_cod = l_record[i].desde
   
       IF g_reg.criterio_cod IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabcriterio(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabcriterio

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabcriterio(g_reg)

   DEFINE g_reg   RECORD 
          criterio_cod		SMALLINT,
          criterio_desc  	CHAR(50),
          abreviatura	  	CHAR(05),
          calculo_cod        CHAR(01)	
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,"TABM036 ",
               COLUMN 17,"LISTADO DE CATALOGO DE CRITERIOS FEDERATIVOS ",
               COLUMN 69,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 10,"DESCRIPCION CRITERIO",
               COLUMN 60,"ETIQ.",
               COLUMN 67,"ENTERO/DECIMAL"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 02,g_reg.criterio_cod USING "##&",
               COLUMN 10,g_reg.criterio_desc,
               COLUMN 60,g_reg.abreviatura,
               COLUMN 74,g_reg.calculo_cod
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
