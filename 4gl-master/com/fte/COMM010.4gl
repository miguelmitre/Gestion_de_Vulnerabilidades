################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P. 
#Programa COMM010  => CATALOGO ARCHIVO DE CRITERIOS 
#Sistema           => TAB. 					               #
#By                => GERARDO ALFONOS VEGA PAREDES.			       #
#Fecha             =>  7 Mayo 1997.     				       #
#Fecha             =>  16 enero 2001.   				       #
################################################################################
DATABASE safre_af

GLOBALS
 
   DEFINE g_reg   RECORD 
      criterio_cod   SMALLINT,
      criterio_desc  CHAR(50),
      abreviatura	     CHAR(05),
      calculo_cod CHAR(01)	
   END RECORD

   DEFINE 
      hoy     DATE,
      opc     CHAR(01),
      sw1     SMALLINT,
      vaccion SMALLINT

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET hoy = DATE

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0101" ATTRIBUTE( BORDER)
   DISPLAY " COMM010                 CATALOGO DE CRITERIOS                                 " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	
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
END MAIN

FUNCTION Inicializa()
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*
   LET sw1 = 0
   LET vaccion = 0
END FUNCTION

FUNCTION Agrega()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGA " AT 2,69 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)

   LET g_reg.criterio_desc = NULL

   LET sw1 = 0

   INPUT BY NAME  g_reg.*
      BEFORE FIELD criterio_cod
         IF sw1 = 0 THEN
            LET sw1 = 1

	    SELECT MAX(criterio_cod) 
              INTO g_reg.criterio_cod 
              FROM tab_criterio
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
           FROM tab_criterio
          WHERE criterio_cod = g_reg.criterio_cod
         IF STATUS <> NOTFOUND THEN
	    ERROR "Codigo Ya Ingresado"
	    NEXT FIELD criterio_cod
         END IF 

      BEFORE FIELD criterio_desc
	 IF g_reg.criterio_cod IS NULL OR  g_reg.criterio_cod = 0 THEN
	    ERROR "Codigo de Criterio NO puede ser nulo"
	    NEXT FIELD criterio_cod
	 END IF 

      AFTER FIELD criterio_desc
	 IF g_reg.criterio_desc IS NULL THEN
	    ERROR "Descripcion de Criterio NO puede ser nula"
	    NEXT FIELD  criterio_desc
	 END IF 
	 SELECT "X" 
           FROM tab_criterio
	  WHERE criterio_desc = g_reg.criterio_desc
	 IF STATUS <> NOTFOUND THEN
	    ERROR "Criterio ya Ingresado"
	    NEXT FIELD criterio_cod
	 END IF

      BEFORE FIELD abreviatura
	 IF g_reg.criterio_desc IS NULL THEN
	    ERROR "Descripcion de Criterio NO puede ser nula"
	    NEXT FIELD criterio_desc
	 END IF 

      AFTER FIELD abreviatura
	 IF g_reg.abreviatura IS NULL THEN
	    ERROR "Criterio Abreviado  NO puede ser nulo"
	    NEXT FIELD abreviatura
	 END IF 
	 SELECT "X" 
           FROM tab_criterio
	  WHERE abreviatura = g_reg.abreviatura
	 IF STATUS <> NOTFOUND THEN
	    ERROR "Criterio Abreviado ya Ingresado"
	    NEXT FIELD abreviatura
         END IF

      BEFORE FIELD calculo_cod
	 IF g_reg.abreviatura IS NULL THEN
	    ERROR "Criterio Abreviada  NO puede ser nulo"
	    NEXT FIELD abreviatura
	 END IF 

      AFTER FIELD calculo_cod
	 IF g_reg.calculo_cod IS NULL THEN
	    ERROR "Codigo Calculo NO puede ser nulo"
	    NEXT FIELD calculo_cod
	 END IF 
	
      ON KEY (ESC)
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
	    NEXT FIELD abreviatura
	 END IF 
	 IF g_reg.calculo_cod IS NULL THEN
	    ERROR "Codigo NO puede ser nulo"
	    NEXT FIELD calculo_cod
	 END IF 

	 SELECT "X" 
           FROM tab_criterio
	  WHERE criterio_desc = g_reg.criterio_desc
	 IF STATUS <> NOTFOUND THEN
	    ERROR "Criterio ya Ingresado"
	    NEXT FIELD criterio_cod
         END IF

         INSERT INTO tab_criterio VALUES (g_reg.*) 

	 ERROR "REGISTRO INGRESADO" SLEEP 1
	 ERROR ""

         CALL Inicializa()

	 NEXT FIELD criterio_cod

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT

   END INPUT

   CLEAR SCREEN
END FUNCTION

FUNCTION Consulta()
   DEFINE 
      l_record ARRAY[300] OF RECORD
         codigo       SMALLINT, 
         descripcion  CHAR(50),
         abrevia      CHAR(05),
         calculo      CHAR(01)
      END RECORD

   DEFINE pos SMALLINT   

   DECLARE cursor_1 CURSOR FOR
   SELECT * 
     FROM tab_criterio
    ORDER BY 1

   LET pos = 1

   FOREACH cursor_1 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0102" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                          C R I T E R I O S                                  " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY

      CLOSE WINDOW ventana_2
   ELSE
      ERROR "ARCHIVO DE CRITERIOS.... VACIO"
   END IF

   CLEAR SCREEN
END FUNCTION

FUNCTION  Modifica()
   DEFINE 
      l_record ARRAY[300] OF RECORD
         codigo      SMALLINT, 
         descripcion CHAR(50),
         abrevia     CHAR(05),
         calculo     CHAR(01)
      END RECORD

   DEFINE pos SMALLINT   

   DECLARE cursor_2 CURSOR FOR
   SELECT * 
     FROM tab_criterio
    ORDER BY 1

   LET pos = 1

   FOREACH cursor_2 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0102" ATTRIBUTE(BORDER)
      DISPLAY "           ESCOJA CON < ENTER > EL CRITERIO A MODIFICAR                      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                            CRITERIOS FEDERATIVAS                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY ARRAY l_record TO scr_1.* 
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
	    LET g_reg.criterio_cod = l_record[pos].codigo
            LET g_reg.criterio_desc = l_record[pos].descripcion
            LET g_reg.abreviatura = l_record[pos].abrevia
            LET g_reg.calculo_cod = l_record[pos].calculo
            EXIT DISPLAY
         ON KEY (INTERRUPT)
	    ERROR "Usted debe escojer un registro"
            LET pos = ARR_CURR()
      END DISPLAY

      CLOSE WINDOW ventana_2

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " ( Esc ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS 
         BEFORE FIELD criterio_desc
            DECLARE xcurcol cursor for
            SELECT "X" 
              FROM com_esq_comis
             WHERE criterio_cod = g_reg.criterio_cod
            OPEN xcurcol
            FETCH xcurcol
               IF STATUS <> 100 THEN
                  ERROR "No puedes modificar porque existen Calculos con este Criterio"
                  CLOSE xcurcol
		  NEXT FIELD criterio_cod
               END IF
            CLOSE xcurcol

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

	 ON KEY ( ESC )
            DECLARE ycurcol cursor for
            SELECT "X" 
              FROM com_esq_comis
             WHERE criterio_cod = g_reg.criterio_cod
            OPEN ycurcol
            FETCH ycurcol
               IF STATUS <> 100 THEN
                  ERROR "No puedes modificar porque existen Calculos con este Criterio"
                  CLOSE ycurcol
		  NEXT FIELD criterio_cod
               END IF
            CLOSE ycurcol

            IF g_reg.criterio_cod IS NULL THEN
               ERROR "Codigo de Criterio NO puede ser NULO"
	       NEXT FIELD criterio_cod
	    END IF
	    IF g_reg.criterio_desc IS NULL THEN
	       ERROR "Descripcion de Criterio NO puede ser NULO"
               NEXT FIELD criterio_desc
	    END IF
	    IF g_reg.abreviatura IS NULL THEN
	       ERROR "Criterio Abreviada  NO puede ser nula"
	       NEXT FIELD  abreviatura
	    END IF 
	    IF g_reg.calculo_cod IS NULL THEN
	       ERROR "Codigo NO puede ser nula"
	       NEXT FIELD  calculo_cod
	    END IF 

            UPDATE tab_criterio 
               SET criterio_desc = g_reg.criterio_desc,
                   abreviatura = g_reg.abreviatura,
                   calculo_cod = g_reg.calculo_cod
             WHERE criterio_cod = g_reg.criterio_cod

 	    ERROR "REGISTRO MODIFICADO" SLEEP 1
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

   CLEAR SCREEN
END FUNCTION

FUNCTION Elimina() 
   DEFINE 
      l_record ARRAY[300] OF RECORD
         codigo      SMALLINT, 
         descripcion CHAR(50),
         abrevia     CHAR(05),
         calculo     CHAR(01)
      END RECORD

   DEFINE pos SMALLINT   

   DECLARE cursor_3 CURSOR FOR
   SELECT * 
     FROM tab_criterio
    ORDER BY 1

   LET pos = 1

   FOREACH cursor_3 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 7,3 WITH FORM "COMM0102" ATTRIBUTE( BORDER)
      DISPLAY "          ESCOJA CON < ENTER > EL CRITERIO A ELIMINAR                       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                            C R I T E R I O S                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY ARRAY l_record TO scr_1.* 
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
	    LET g_reg.criterio_cod = l_record[pos].codigo
            LET g_reg.criterio_desc = l_record[pos].descripcion
            LET g_reg.abreviatura = l_record[pos].abrevia
            LET g_reg.calculo_cod = l_record[pos].calculo
            EXIT DISPLAY
         ON KEY (INTERRUPT)
	    ERROR "Usted debe escojer un registro"
            LET pos = ARR_CURR()
      END DISPLAY

      CLOSE WINDOW ventana_2

      DECLARE xcurcol2 cursor for
      SELECT "X" 
        FROM com_esq_comis
       WHERE criterio_cod = g_reg.criterio_cod 
      OPEN xcurcol2
      FETCH xcurcol2
      IF STATUS <> 100 THEN
         ERROR "No puedes Eliminar Criterio porque existen Calculos con este Codigo"
         SLEEP 3
         CLOSE xcurcol2
         ERROR "" 
         CALL Inicializa()
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " ( Esc ) Elimina        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()
         IF opc MATCHES "[Ss]" THEN
            DELETE 
              FROM tab_criterio
             WHERE criterio_cod = g_reg.criterio_cod

            ERROR "REGISTRO ELIMINADO" SLEEP 1
         ELSE
            ERROR "ELIMINAR CANCELADO" SLEEP 1
         END IF
         ERROR ""
         CALL Inicializa()
   ELSE
	 ERROR "ARCHIVO DE CRITERIOS  FEDERATIVAS.... VACIO"
   END IF
   CLEAR SCREEN
END FUNCTION

FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR opc
END FUNCTION

