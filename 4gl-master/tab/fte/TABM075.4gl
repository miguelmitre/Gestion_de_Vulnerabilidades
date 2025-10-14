######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => E.F.P.                                         #
#Programa TABM0073 => CATALOGO AFORES.                               #
#Fecha             => 26 de marzo del 2003.                          #
#Fecha modifica    => 26 de marzo del 2003.                          #
#Modificado por    => LAURA EUGENIA CORTES GUZMAN                    #
#Fecha modifica    => 19 de julio del 2004.                          #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => TAB.                                           #
######################################################################
DATABASE safre_af

GLOBALS
    DEFINE g_reg  RECORD
           afore_cod    SMALLINT,
           afore_desc   CHAR(120),   
           marca        SMALLINT,
	   afore_fusion SMALLINT
    END RECORD

    DEFINE g_param_dis RECORD LIKE glo_parametro.*

    DEFINE l_record ARRAY[3000] OF RECORD 
           afore_cod    SMALLINT,
           afore_desc   CHAR(120),
           marca        SMALLINT,
	   afore_fusion SMALLINT
    END RECORD

    DEFINE sw_1           SMALLINT,
           aux_pausa      CHAR(01),
           HOY            DATE,
           fecha          DATE,
           aux_estad_desc CHAR(40),
           usuario        CHAR(08),
           pos            SMALLINT,
           cla_where      CHAR(300),
           sel_where      CHAR(300),
           g_lista        CHAR(300),
           g_impre        CHAR(300)

END GLOBALS
##########################################################################
MAIN
    OPTIONS 
       PROMPT LINE LAST,
       INPUT WRAP,
       ACCEPT KEY control-o

    DEFER INTERRUPT

    CALL STARTLOG("TABM075.log")
    CALL inicio()
    CALL proceso()

END MAIN
##########################################################################
FUNCTION inicio()

    SELECT USER,*
    INTO   usuario
    FROM   glo_parametro

    SELECT ruta_spool
    INTO   g_param_dis.ruta_spool
    FROM   glo_parametro

    INITIALIZE g_reg.*, fecha TO NULL

END FUNCTION
##########################################################################
FUNCTION proceso()

    LET HOY = TODAY

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0751" ATTRIBUTE( BORDER)
    DISPLAY " TABM075             CATALOGO DE AFORES                                        " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CATALOGO AFORES"
       COMMAND "Agrega" "Agrega Afores"
          CALL Agrega()
       COMMAND "Consulta" "Consulta Afores"
          CALL Consulta()
       COMMAND "Modifica" "Modifica Afores"
          CALL Modifica()
       COMMAND "Elimina" "Elimina Afores"
          CALL Elimina()
       COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU

    CLOSE WINDOW ventana_1

END FUNCTION
#############################################################################
FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
   DISPLAY BY NAME g_reg.*

END FUNCTION
################################################################################
FUNCTION Agrega()

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

   INPUT BY NAME g_reg.*

      AFTER FIELD afore_cod
         IF g_reg.afore_cod IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD afore_cod
         ELSE
            SELECT "a.X"
            FROM   tab_afore a
	    WHERE  a.afore_cod = g_reg.afore_cod

            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe el Codigo de Afore "
               NEXT FIELD afore_cod
            END IF
         END IF

      AFTER FIELD afore_desc
         IF g_reg.afore_desc IS NULL    OR
            g_reg.afore_desc[1,1] = " " THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD afore_desc
         END IF

      BEFORE FIELD marca
	 LET g_reg.marca = 0
	 DISPLAY BY NAME g_reg.marca

      AFTER FIELD marca
         IF g_reg.marca IS NULL OR
            g_reg.marca = " "   THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD marca
         END IF

         IF g_reg.marca <> 0 AND
            g_reg.marca <> 1 THEN
            ERROR "Tipo marca solo puede ser <0> Otras afores o <1> Afore local"
            LET g_reg.marca = NULL
            NEXT FIELD  marca
         END IF 

         IF g_reg.marca = 1 THEN
            SELECT "a.X"
            FROM   tab_afore a
	    WHERE  a.marca = 1

            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe marca de Afore local "
	       LET g_reg.marca = 0
	       DISPLAY BY NAME g_reg.marca
               NEXT FIELD afore_cod
            END IF
        END IF

      ON KEY ( ESC )
         IF g_reg.afore_cod IS NULL THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD afore_cod
         ELSE
            SELECT "a.X"
            FROM   tab_afore a
	    WHERE  a.afore_cod = g_reg.afore_cod

            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe el Codigo de Afore "
               NEXT FIELD afore_cod
            END IF
         END IF

         IF g_reg.afore_desc IS NULL    OR
            g_reg.afore_desc[1,1] = " " THEN
            ERROR "El campo no puede ser nulo"
            NEXT FIELD afore_desc
         END IF

         IF g_reg.marca IS NULL OR
            g_reg.marca = " "   THEN
            ERROR "El campo no puede ser nulo o blanco"
            NEXT FIELD marca
         END IF

         INSERT INTO tab_afore
         VALUES ( g_reg.afore_cod, g_reg.afore_desc, g_reg.marca, 
		  g_reg.afore_fusion )

         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         ERROR ""

         CALL Inicializa()

         NEXT FIELD afore_cod

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
   END INPUT

   CALL Inicializa()

   CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION Consulta()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0752" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta              (Ctrl-p) Impresion             (Ctrl-C) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "          Seleccione con < ENTER > la Afore para ver completo                 " AT 2,1 
      DISPLAY "                             CATALOGO DE AFORES                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             marca
                        FROM afore_cod,
                             marca
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT afore_cod, afore_desc, marca, ",
		      " afore_fusion ",
		      " FROM tab_afore WHERE ",
                       cla_where CLIPPED,
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
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.afore_cod       = l_record[pos].afore_cod
               LET g_reg.afore_desc      = l_record[pos].afore_desc
               LET g_reg.marca           = l_record[pos].marca
	       LET g_reg.afore_fusion    = l_record[pos].afore_fusion
               EXIT DISPLAY

            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE AFORES.... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
   END IF
   CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION  Modifica()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0752" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Seleccione con < ENTER > la Afore a modificar              " AT 2,1 
      DISPLAY "                              CATALOGO DE AFORES                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod, marca
                        FROM afore_cod,
                             marca
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT afore_cod, afore_desc, marca, ",
		      " afore_fusion",
		      " FROM tab_afore WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 2 "

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
               LET g_reg.afore_cod      = l_record[pos].afore_cod
               LET g_reg.afore_desc     = l_record[pos].afore_desc
               LET g_reg.marca          = l_record[pos].marca
	       LET g_reg.afore_fusion   = l_record[pos].afore_fusion
               EXIT DISPLAY

            ON KEY (INTERRUPT)
               ERROR "Deberá seleccionar un registro a modificar"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE AFORES VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      dISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.* WITHOUT DEFAULTS
         BEFORE FIELD afore_cod
            NEXT FIELD afore_desc

         AFTER FIELD afore_desc
            IF g_reg.afore_desc IS NULL    OR
               g_reg.afore_desc[1,1] = " " THEN
               ERROR "LA descripcion de Afore NO puede ser nula"
               NEXT FIELD  afore_desc
            END IF 

         AFTER FIELD marca
            IF g_reg.marca IS NULL OR
               g_reg.marca  =  " " THEN
               ERROR "Marca de afore local No puede ser nulo o blanco"
               NEXT FIELD  marca
            END IF 

         IF g_reg.marca <> 0 AND
            g_reg.marca <> 1 THEN
            ERROR "Tipo marca solo puede ser <0> Otras afores o <1> Afore local"
            LET g_reg.marca = NULL
            NEXT FIELD  marca
         END IF 

         IF g_reg.marca = 1 THEN
            SELECT "a.X"
            FROM   tab_afore a
	    WHERE  a.marca = 1

            IF STATUS <> NOTFOUND THEN
               ERROR "Ya existe marca de Afore local "
	       LET g_reg.marca = 0
	       DISPLAY BY NAME g_reg.marca
               NEXT FIELD afore_cod
            END IF
        END IF

        AFTER FIELD afore_fusion
	   
            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE tab_afore
               SET    afore_desc   = g_reg.afore_desc,
                      marca        = g_reg.marca,
		      afore_fusion = g_reg.afore_fusion
               WHERE  afore_cod    = g_reg.afore_cod

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
      ERROR "ARCHIVO DE AFORES SE ENCUENTRA.... VACIO"
   END IF

   CALL Inicializa()
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0752" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) Consulta                                             (Ctrl-c) Salir   " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                Seleccione con < ENTER > la Afore a eliminar              " AT 2,1 
      DISPLAY "                              CATALOGO DE AFORES                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             marca
                        FROM afore_cod,
                             marca
         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
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

      LET sel_where = "SELECT afore_cod, afore_desc, marca, ",
		      " afore_fusion",
		      " FROM tab_afore WHERE ",
                      cla_where CLIPPED,
                      " ORDER BY 1 "

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
               LET g_reg.afore_cod     = l_record[pos].afore_cod
               LET g_reg.afore_desc    = l_record[pos].afore_desc
               LET g_reg.marca         = l_record[pos].marca
	       LET g_reg.afore_fusion  = l_record[pos].afore_fusion
               EXIT DISPLAY

            ON KEY (INTERRUPT)
               ERROR "Debera seleccionar un registro"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE AFORES VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_afore
         WHERE afore_cod = g_reg.afore_cod

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 2
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 2
      END IF

      ERROR ""

      CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE AFORES SE ENCUENTRA.... VACIO"
   END IF

   CALL Inicializa()

   CLEAR SCREEN
END FUNCTION

FUNCTION Pregunta()
   PROMPT "Esta seguro ¿ S/N ? " FOR aux_pausa
END FUNCTION

FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                 ".CAT_AFO",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabdevol TO g_impre

   FOR i=1 TO (pos+1)
      LET g_reg.afore_cod     = l_record[i].afore_cod
      LET g_reg.afore_desc    = l_record[i].afore_desc
      LET g_reg.marca         = l_record[i].marca
      LET g_reg.afore_fusion  = l_record[i].afore_fusion

      IF g_reg.afore_cod IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_tabdevol(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabdevol

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabdevol(g_reg)

   DEFINE g_reg  RECORD 
          afore_cod    SMALLINT,
          afore_desc   CHAR(120),
          marca        CHAR(1),
	  afore_fusion SMALLINT
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 
         PRINT COLUMN 01,"TABM075 ",
               COLUMN 70,"LISTADO DE CATALOGO DE AFORES           ",
               COLUMN 165,TODAY USING "dd-mm-yyyy"
         SKIP 3 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 10,"DESCRIPCION ",
               COLUMN 134,"TIPO",
	       COLUMN 140,"AFORE FUSION"
         SKIP 1 LINE

      ON EVERY ROW
            PRINT COLUMN 1,g_reg.afore_cod USING "###",
                  COLUMN 10,g_reg.afore_desc,
                  COLUMN 135,g_reg.marca,
		  COLUMN 140,g_reg.afore_fusion
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
