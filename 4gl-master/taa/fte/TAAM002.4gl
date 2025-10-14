######################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		     #
#Propietario       => E.F.P.                                         #
#Programa TAAM002  => PRECIO SIEFORES CEDENTES                       #
#Fecha             => 6 DE DICIEMBRE DE 1999.                        #
#Por               => MAURO MUNIZ CABALLERO                          #
#Sistema           => TAA. 					     #
######################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE g_reg RECORD 
        codigo_afore	SMALLINT,
	codigo_siefore	SMALLINT,
	siefore_desc	CHAR(08),
        precio_del_dia  DECIMAL(10,6),
        fecha_valuacion DATE	
    END RECORD

    DEFINE l_record ARRAY[3000] OF RECORD
        codigo_afore	SMALLINT,
	codigo_siefore	SMALLINT,
	siefore_desc	CHAR(08),
        precio_del_dia  DECIMAL(10,6),
        fecha_valuacion DATE	
    END RECORD

    DEFINE l_record1 ARRAY[500] OF RECORD
        cod_afore  SMALLINT,
        desc_afore CHAR(40)
    END RECORD

    DEFINE
        HOY         DATE,
        HOY1        DATE,
        HOY2        DATE,
        sw_1        SMALLINT,
        pos         SMALLINT,
        enter       CHAR(1),
        aux_pausa   CHAR(1),
        siono       CHAR(1),
        g_usuario   CHAR(8),
        hora        CHAR(8),
        vdesc_afore CHAR(30),
        cla_where   CHAR(300),
        sel_where   CHAR(300),
        g_lista     CHAR(300),
        g_impre     CHAR(300)

END GLOBALS

MAIN

   DEFER INTERRUPT
   OPTIONS PROMPT LINE LAST,
           INPUT WRAP

   CALL STARTLOG('TAAM002.log')
   CALL inicio()
   --CALL fechas(HOY1) RETURNING HOY2
   CALL proceso()

END MAIN

FUNCTION inicio()
#i---------------

   SELECT *, user
   INTO   g_param_taa.*, g_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'taa'

   LET HOY  = TODAY
   LET HOY1 = MDY(MONTH(HOY),1,YEAR(HOY))
   LET HOY2 = TODAY

   INITIALIZE g_reg.* TO NULL

END FUNCTION

FUNCTION proceso()
#p----------------

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TAAM0021" ATTRIBUTE( BORDER)
    DISPLAY " TAAM002          MANTENIMIENTO A PRECIO DE SIEFORES CEDENTES                  " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "PRECIO SIEFORES "
        COMMAND "Agrega" "Agrega Precio"
            CALL Agrega()    #A
        COMMAND "Consulta" "Consulta Precio"
            CALL Consulta()  #C
        COMMAND "Modifica" "Modifica Precio"
            CALL Modifica()  #M
        COMMAND "Elimina" "Elimina Precio"
            CALL Elimina()   #E
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()
#iz------------------

    INITIALIZE g_reg.* TO NULL
    DISPLAY BY NAME g_reg.*
    INITIALIZE vdesc_afore TO NULL
    DISPLAY BY NAME vdesc_afore

    LET sw_1 = 0

END FUNCTION

FUNCTION Agrega()
#A---------------

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                        AGREGA          " AT 1,1 ATTRIBUTE(GREEN)

    INPUT BY NAME  g_reg.* 

    AFTER FIELD codigo_afore
        IF g_reg.codigo_afore IS NULL THEN
            DECLARE cur_afo CURSOR FOR
            SELECT @afore_cod, @afore_desc
            FROM   tab_afore
     	    ORDER BY 1

	    LET pos = 1

            FOREACH cur_afo INTO l_record1[pos].*
	       LET pos = pos + 1
            END FOREACH

	    IF (pos-1) >= 1 THEN
	        CALL  SET_COUNT(pos-1)
	        OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0023" ATTRIBUTE( BORDER)
	        DISPLAY " AGREGA " AT 1,69 
	        DISPLAY " (Ctrl-C) Salir " AT 1,1 
	        DISPLAY "          AGREGA PRECIO DE SIEFORES CEDENTES                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
   	
	        DISPLAY ARRAY l_record1 TO scr_1.*

                ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
	            LET g_reg.codigo_afore  = l_record1[pos].cod_afore
                    LET vdesc_afore = l_record1[pos].desc_afore
                    EXIT DISPLAY

                ON KEY (INTERRUPT)
	            ERROR "Usted debe escoger un registro"
                    LET pos = ARR_CURR()
                    EXIT DISPLAY

	        END DISPLAY

	        CLOSE WINDOW ventana_2

                DISPLAY BY NAME g_reg.codigo_afore, vdesc_afore
            ELSE
	        ERROR "ARCHIVO DE AFORE VACIO"
	    END IF
        ELSE
       	    SELECT @afore_desc
            INTO   vdesc_afore
            FROM   tab_afore
            WHERE  @afore_cod = g_reg.codigo_afore
	    ORDER BY 1

            DISPLAY BY NAME vdesc_afore
        END IF

        SELECT "X"
        FROM   tab_afore
        WHERE  @afore_cod = g_reg.codigo_afore

        IF STATUS = NOTFOUND THEN
            ERROR "Codigo Afore NO existe ..."
            NEXT FIELD codigo_afore
        END IF

        AFTER FIELD codigo_siefore
         IF g_reg.codigo_siefore IS NULL THEN
            ERROR "Codigo Siefore NO puede ser nulo"
            NEXT FIELD  codigo_siefore
         END IF

       	    SELECT @siefore_desc 
            INTO   g_reg.siefore_desc
            FROM   tab_siefore
            WHERE  @afore_cod = g_reg.codigo_afore
            AND    @siefore_cod = g_Reg.codigo_siefore

            IF STATUS = NOTFOUND THEN
                ERROR "Codigo de Siefore NO existe "
                NEXT FIELD codigo_siefore
            ELSE
                DISPLAY BY NAME g_reg.siefore_desc
            END IF

         AFTER FIELD precio_del_dia
             IF g_reg.precio_del_dia IS NULL OR
                g_reg.precio_del_dia = 0 THEN
                 ERROR "Precio de ACCION NO puede ser nulo"
                 NEXT FIELD precio_del_dia
             END IF

         BEFORE FIELD fecha_valuacion
             LET g_reg.fecha_valuacion = HOY2
             DISPLAY BY NAME g_reg.fecha_valuacion

         AFTER FIELD fecha_valuacion
             IF g_reg.fecha_valuacion IS NULL OR
                g_reg.fecha_valuacion = 0 THEN
                 ERROR "Fecha de Valuacion de ACCION NO puede ser nula"
                 NEXT FIELD fecha_valuacion
             END IF            

         SELECT "X"
         FROM   taa_accion_siefore
         WHERE  codigo_afore = g_reg.codigo_afore
         AND    codigo_siefore = g_reg.codigo_siefore
         AND    fecha_valuacion = g_reg.fecha_valuacion

         IF STATUS <> NOTFOUND THEN
            ERROR "Siefore YA ingresada ..."
            SLEEP 3
            CALL Inicializa()
            NEXT FIELD codigo_afore
         END IF

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT

      ON KEY (ACCEPT)
         IF g_reg.codigo_afore IS NULL THEN
            ERROR "Codigo Afore NO puede ser nulo"
            NEXT FIELD codigo_afore
         END IF

         IF g_reg.codigo_siefore IS NULL THEN
            ERROR "Codigo Siefore NO puede ser nulo"
            NEXT FIELD  codigo_siefore
         END IF

         IF g_reg.precio_del_dia IS NULL THEN
            ERROR "Precio de la Siefore NO puede ser nulo"
            NEXT FIELD  siefore_desc
         END IF

         IF g_reg.fecha_valuacion IS NULL THEN
            ERROR "Fecha valuacion de la Siefore NO puede ser nula"
            NEXT FIELD fecha_aluacion
         END IF

         SELECT 'X'
         FROM   taa_accion_siefore
         WHERE  codigo_afore    = g_reg.codigo_afore
         AND    codigo_siefore  = g_reg.codigo_siefore
         AND    fecha_valuacion = g_reg.fecha_valuacion

	 IF SQLCA.SQLCODE = 0 THEN
             ERROR "REGISTRO YA FUE INGRESADO"
             SLEEP 3
             CALL Inicializa()
             NEXT FIELD codigo_afore
         END IF

         INSERT INTO taa_accion_siefore VALUES (g_reg.*,g_usuario) 

         ERROR "REGISTRO INGRESADO"
         SLEEP 2
         ERROR ""

         CALL Inicializa()
         EXIT INPUT
           
   END INPUT
   CLEAR SCREEN

END FUNCTION

FUNCTION Consulta()
#C-----------------

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0022" ATTRIBUTE( BORDER)
      DISPLAY " (ESC) Todos     (ENTER) Consulta     (Ctrl-C) Salir     (Ctrl-P) Impresion    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "               CONSULTA PRECIO DE SIEFORES CEDENTES                            " AT 3,1 ATTRIBUTE(REVERSE,GREEN) 

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON codigo_afore, desc_siefore, fecha_valuacion 
                        FROM codigo_afore, desc_siefore, fecha_valuacion

         ON KEY (ACCEPT)
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

      LET sel_where = "SELECT codigo_afore, codigo_siefore, desc_siefore, " ,
                      "       precio_del_dia, fecha_valuacion " ,
                      "       FROM taa_accion_siefore WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 5 DESC,1 " 
   
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
         ERROR "REGISTRO DE SIEFORE ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN

END FUNCTION

FUNCTION  Modifica()
#M------------------

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0022" ATTRIBUTE(BORDER)
      DISPLAY " (ESC) Todos                 (ENTER) Consulta                (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "               Escoja con < ENTER > la Siefore a modificar                     " AT 2,1 
      DISPLAY "             MODIFICA PRECIO DE SIEFORES CEDENTES                              " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON fecha_valuacion FROM fecha_valuacion
         ON KEY (ACCEPT)
            LET int_flag = FALSE
            EXIT CONSTRUCT
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

      LET sel_where = "SELECT codigo_afore, codigo_siefore, desc_siefore, " ,
                      "       precio_del_dia, fecha_valuacion " ,
                      "       FROM taa_accion_siefore WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 5 DESC, 1 " 
   
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
	       LET g_reg.codigo_afore    = l_record[pos].codigo_afore
               LET g_reg.codigo_siefore  = l_record[pos].codigo_siefore
               LET g_reg.siefore_desc    = l_record[pos].siefore_desc
               LET g_reg.fecha_valuacion = l_record[pos].fecha_valuacion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escoger un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SIEFORE ....NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,GREEN)

      INPUT BY NAME  g_reg.codigo_afore,g_reg.codigo_siefore,
                     g_reg.precio_del_dia, g_reg.fecha_valuacion
                     WITHOUT DEFAULTS 

      BEFORE FIELD codigo_afore
         NEXT FIELD precio_del_dia

         AFTER FIELD precio_del_dia
	    IF g_reg.precio_del_dia IS NULL THEN 
	       ERROR "Descripcion de la Siefore NO puede ser nulo"
	       NEXT FIELD precio_del_dia
	    END IF

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN     
               UPDATE taa_accion_siefore 
               SET    precio_del_dia = g_reg.precio_del_dia ,
                      usuario         = g_usuario  
               WHERE  codigo_afore    = g_reg.codigo_afore
               AND    codigo_siefore  = g_reg.codigo_siefore
               AND    fecha_valuacion = g_reg.fecha_valuacion

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

FUNCTION Elimina()
#E----------------

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TAAM0022" ATTRIBUTE( BORDER)
      DISPLAY " (ESC) Todos                 (ENTER) Consulta                (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,GREEN)
      DISPLAY "                Escoja con < ENTER > la Siefore a borrar                      " AT 2,1 
      DISPLAY "                ELIMINA PRECIO DE SIEFORES CEDENTES                            " AT 3,1 ATTRIBUTE(REVERSE,GREEN)

      LET int_flag = FALSE 

      CONSTRUCT cla_where ON fecha_valuacion FROM fecha_valuacion
         ON KEY (ACCEPT)
            LET int_flag = FALSE
            EXIT CONSTRUCT
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

      LET sel_where = "SELECT codigo_afore, codigo_siefore, desc_siefore, " ,
                      "       precio_del_dia, fecha_valuacion " ,
                      "       FROM taa_accion_siefore WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 5 DESC, 1 " 
   
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
	       LET g_reg.codigo_afore    = l_record[pos].codigo_afore
               LET g_reg.codigo_siefore  = l_record[pos].codigo_siefore
               LET g_reg.siefore_desc    = l_record[pos].siefore_desc
               LET g_reg.precio_del_dia  = l_record[pos].precio_del_dia
               LET g_reg.fecha_valuacion = l_record[pos].fecha_valuacion
               EXIT DISPLAY
            ON KEY (INTERRUPT)
	       ERROR "Usted debe escoger un registro"
               LET pos = ARR_CURR()
	 END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE SIEFORE ... NO EXISTE "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(GREEN)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,GREEN)

      DISPLAY BY NAME  g_reg.codigo_afore,
                       g_reg.codigo_siefore,
                       g_reg.siefore_desc,
                       g_reg.precio_del_dia,
                       g_reg.fecha_valuacion

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM taa_accion_siefore
            WHERE  codigo_afore    = g_reg.codigo_afore
            AND    codigo_siefore  = g_reg.codigo_siefore
            AND    fecha_valuacion = g_reg.fecha_valuacion 

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
         ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE SIEFORES .... VACIO"
   END IF
   CLEAR SCREEN

END FUNCTION

FUNCTION Pregunta()

   PROMPT "Esta seguro S/N ? " FOR aux_pausa

END FUNCTION

FUNCTION impresion(pos)

   DEFINE i, pos     SMALLINT
   DEFINE g_cambio CHAR(300)

   LET hora = TIME

   LET g_impre = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
               ".PRECIO_ACC.",HOY USING "dd-mm-yyyy","_",hora CLIPPED
  
   START REPORT rpt_tabsiefore TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.codigo_afore    = l_record[i].codigo_afore
       LET g_reg.codigo_siefore  = l_record[i].codigo_siefore
       LET g_reg.siefore_desc    = l_record[i].siefore_desc
       LET g_reg.precio_del_dia  = l_record[i].precio_del_dia
       LET g_reg.fecha_valuacion = l_record[i].fecha_valuacion

       IF g_reg.codigo_afore IS NULL  THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabsiefore(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabsiefore
  
   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_impre = g_impre CLIPPED

   LET g_cambio = "chmod 777 ", g_impre
   RUN g_cambio

   LET g_lista = "lp ",g_impre
   RUN g_lista

END FUNCTION

REPORT rpt_tabsiefore(g_reg)
  
   DEFINE g_reg			RECORD 
          codigo_afore		SMALLINT,
	  codigo_siefore	  	SMALLINT,
	  siefore_desc	 	CHAR(08),
          precio_del_dia        DECIMAL(10,6),
          fecha_valuacion       DATE	
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
   PAGE HEADER
      PRINT COLUMN 02," TAAM002 ",
            COLUMN 12," LISTADO PRECIO ACCIONES DE SIEFORES CEDENTES",
            COLUMN 60,TODAY USING "dd-mm-yyyy"
      SKIP 2 LINE

      PRINT COLUMN 05,"COD. AFORE",
            COLUMN 20,"SIEFORE",
            COLUMN 28,"DESCRIPCION  ",
            COLUMN 45,"PRECIO ",
            COLUMN 58,"FECHA VALUACION"
      SKIP 1 LINE
   ON EVERY ROW
      PRINT COLUMN 09,g_reg.codigo_afore USING "&&&",
            COLUMN 24,g_reg.codigo_siefore USING "&&",
            COLUMN 30,g_reg.siefore_desc,
            COLUMN 40,g_reg.precio_del_dia USING "##,###.&&&&&&",
            COLUMN 59,g_reg.fecha_valuacion USING "dd-mm-yyyy"
   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
   ON LAST ROW
      SKIP 4 LINE
      PRINT COLUMN 01," Total de registros : " ,COUNT(*) USING "<<<<<"
END REPORT

FUNCTION fechas(diaActual)
#sdh----------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE,
        numDias   SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO 6
        IF contador = 1 THEN
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        ELSE
            LET diaTmp = diaTmp + 1 UNITS DAY
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        END IF
    END FOR

    RETURN diaTmp

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado 
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
        ELSE
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

