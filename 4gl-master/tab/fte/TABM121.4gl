###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM121  => CATALOGO: ESTADOS DE COMISION                       #
#Fecha             => 07 DE JULIO DE 2005                                 #
#Por               => EDUARDO JOAQUIN RESENDIZ MEDINA                     #
#Sistema           => TAB                                                 #
###########################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_tab RECORD LIKE safre_af:seg_modulo.*

   DEFINE aux_pausa   CHAR(1),
          sw_1        SMALLINT,
          hoy         DATE,
          usuario     CHAR(8),
          pos         INTEGER,
          sel_where   CHAR(300),
          cla_where   CHAR(300),
          g_impre     CHAR(300),
          g_lista     CHAR(300)

   DEFINE g_edo         RECORD
          codigo        SMALLINT,
          descripcion   CHAR(70),
          desc_corta    CHAR(15),
          factualiza    DATE,
          usuario       CHAR(8)
   END RECORD

   DEFINE l_record ARRAY[200] OF RECORD
          codigo       SMALLINT,
          descripcion  CHAR(70),
          desc_corta   CHAR(15)
   END RECORD

END GLOBALS
-------------------------------------------------------------------------
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT  WRAP,
      ACCEPT KEY CONTROL-O

      DEFER INTERRUPT

      CALL STARTLOG("TABM121.log")
      CALL inicio()
      CALL proceso()

END MAIN
-------------------------------------------------------------------------
FUNCTION inicio()

   SELECT USER 
     INTO usuario
     FROM SYSTABLES
    WHERE tabid=1;

   SELECT *
     INTO g_param_tab.*
     FROM seg_modulo
    WHERE modulo_cod = 'tab'

END FUNCTION
-------------------------------------------------------------------------
FUNCTION proceso()

   LET HOY = TODAY

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1211" ATTRIBUTE (BORDER)

   DISPLAY " TABM121           CATALOGO ESTADOS DE COMISION                  " AT 3,1    ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CATALOGO ESTADO COMISION"
      COMMAND " Agrega" "Agrega Estado"
         CALL Agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta Estado"
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica Estado"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina  Estado"
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Inicializa()

   LET sw_1 = 0

   INITIALIZE g_edo.* TO NULL

   DISPLAY BY NAME g_edo.codigo,
                   g_edo.descripcion,
                   g_edo.desc_corta
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Agrega()

   DISPLAY "" AT 1,1

   DISPLAY "" AT 2,1

   DISPLAY " (ESC ) Agrega                  (CTRL-C) Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE)

   LET g_edo.codigo = NULL
   LET sw_1 = 0

   INPUT BY NAME g_edo.codigo,
                 g_edo.descripcion,
                 g_edo.desc_corta

   BEFORE FIELD codigo
      IF sw_1 = 0 THEN
         LET sw_1 = 1

         DISPLAY BY NAME g_edo.codigo
      END IF

   AFTER FIELD codigo
      IF g_edo.codigo IS NULL OR
         g_edo.codigo = "" THEN
         ERROR "EL CODIGO NO PUEDE SER NULO"
         NEXT FIELD codigo
      END IF

      SELECT "X"
      FROM tab_edo_comision
      WHERE codigo = g_edo.codigo

      IF STATUS <> NOTFOUND THEN
         ERROR  "CODIGO YA INGRESADO... VERIFIQUE"
         SLEEP  2
         ERROR  " "
         NEXT FIELD codigo
      END IF

   AFTER FIELD descripcion
      IF g_edo.descripcion IS NULL OR
         g_edo.descripcion = "" THEN
         ERROR "LA DESCRIPCION NO PUEDE SER NULA"
         NEXT FIELD descripcion
      END IF

      SELECT "X"
      FROM   tab_edo_comision
      WHERE  descripcion = g_edo.descripcion

      IF STATUS <> NOTFOUND THEN
         ERROR "DESCRIPCION YA INGRESADA... VERIFIQUE"
         NEXT FIELD descripcion
      END IF

   AFTER FIELD desc_corta
      IF g_edo.desc_corta IS NULL OR
         g_edo.desc_corta = "" THEN
         ERROR "LA DESCRIPCION CORTA NO PUEDE SER NULA"
         NEXT FIELD desc_corta
      END IF

      SELECT "X"
      FROM   tab_edo_comision
      WHERE  desc_corta = g_edo.desc_corta

      IF STATUS <> NOTFOUND THEN
         ERROR "DESCRIPCION CORTA YA INGRESADA... VERIFIQUE"
         NEXT FIELD desc_corta
      END IF

      ON KEY ( ESC )
         IF g_edo.codigo IS NULL OR
            g_edo.codigo = "" THEN
            ERROR "EL CODIGO NO PUEDE SER NULO"
            NEXT FIELD codigo
         END IF

         SELECT "X"
         FROM   tab_edo_comision
         WHERE  codigo = g_edo.codigo

         IF STATUS <> NOTFOUND THEN
            ERROR "CODIGO YA INGRESADO"
            NEXT FIELD codigo
         END IF

         IF g_edo.descripcion IS NULL OR
            g_edo.descripcion = "" THEN
            ERROR "DESCRIPCION NO PUEDE SER NULA"
            NEXT FIELD descripcion
         END IF

         SELECT "X"
         FROM   tab_edo_comision
         WHERE  descripcion = g_edo.descripcion

         IF STATUS <> NOTFOUND THEN
            ERROR "DESCRIPCION YA INGRESADA"
            SLEEP 1
            ERROR " "
            NEXT FIELD descripcion
         END IF

         IF g_edo.desc_corta IS NULL OR
            g_edo.desc_corta = "" THEN
            ERROR "DESCRIPCION CORTA NO PUEDE SER NULA"
            NEXT FIELD desc_corta
         END IF

         SELECT "X"
         FROM   tab_edo_comision
         WHERE  desc_corta = g_edo.desc_corta

         IF STATUS <> NOTFOUND THEN
            ERROR "DESCRIPCION CORTA YA INGRESADA"
            SLEEP 1
            ERROR " "
            NEXT FIELD desc_corta
         END IF

         LET g_edo.factualiza = TODAY
         LET g_edo.usuario    = usuario

         INSERT INTO tab_edo_comision VALUES (g_edo.* )
            ERROR "REGISTRO INGRESADO"
            SLEEP 1
            ERROR ""
            CLEAR FORM
            CALL inicializa()
            NEXT FIELD codigo
            CLEAR FORM

      ON KEY (INTERRUPT)
         CLEAR FORM
         CALL inicializa()
         EXIT INPUT
   END INPUT
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Consulta()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1212" ATTRIBUTE( BORDER)

      DISPLAY " (ENTER) Consulta               (CTRL-P) Imprimir            (CTRL-C)Salir          " AT 1,1 ATTRIBUTE(REVERSE)

      DISPLAY "          Escoja Con <ENTER> El Registro a Consultar                           " AT 2,1

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo
                        FROM codigo

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 2
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM tab_edo_comision ",
                      " WHERE ",cla_where CLIPPED,
                      " ORDER BY 1"

      PREPARE query1 FROM sel_where

      DECLARE cursor_1 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET l_record[pos].codigo      = l_record[pos].codigo
         LET l_record[pos].descripcion = l_record[pos].descripcion
         LET l_record[pos].desc_corta  = l_record[pos].desc_corta
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_edo.codigo      = l_record[pos].codigo
               LET g_edo.descripcion = l_record[pos].descripcion
               LET g_edo.desc_corta  = l_record[pos].desc_corta
               EXIT DISPLAY
            ON KEY (CONTROL-P)
               ERROR "PROCESANDO INFORMACION ..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               CALL Inicializa()
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO ... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF

   DISPLAY BY NAME g_edo.codigo,
                   g_edo.descripcion,
                   g_edo.desc_corta

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "CONSULTA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR FORM
         CLEAR SCREEN
         RETURN
      ELSE
         PROMPT "<ENTER> Para Salir de la Consulta" for aux_pausa
      END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Modifica()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1212" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Modifica                                            (CTRL-C)Salir          " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "          Escoja Con <ENTER> El Registro a Modificar                           " AT 2,1

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo
                        FROM codigo

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM tab_edo_comision  WHERE ",
                        cla_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query2

      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].*
         LET l_record[pos].codigo      = l_record[pos].codigo
         LET l_record[pos].descripcion = l_record[pos].descripcion
         LET l_record[pos].desc_corta  = l_record[pos].desc_corta
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_edo.codigo      = l_record[pos].codigo
               LET g_edo.descripcion = l_record[pos].descripcion
               LET g_edo.desc_corta  = l_record[pos].desc_corta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "DEBE ELEGIR UN REGISTRO."
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY BY NAME g_edo.codigo,
                      g_edo.descripcion,
                      g_edo.desc_corta

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY "  MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)

      INPUT BY NAME g_edo.codigo,
                    g_edo.descripcion,
                    g_edo.desc_corta
      WITHOUT DEFAULTS

      BEFORE FIELD codigo
      NEXT FIELD descripcion

      AFTER FIELD descripcion
         IF g_edo.descripcion IS NULL THEN
            ERROR "NO DEBE SER NULO."
            NEXT FIELD descripcion
         END IF

      AFTER FIELD desc_corta
         IF g_edo.desc_corta IS NULL THEN
            ERROR "NO DEBE SER NULO."
            NEXT FIELD desc_corta
         END IF

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_edo_comision SET
                   descripcion  = g_edo.descripcion,
                   desc_corta   = g_edo.desc_corta,
                   factualiza   = TODAY,
                   usuario      = usuario
            WHERE codigo   = g_edo.codigo
            ERROR "REGISTRO MODIFICADO"
            SLEEP 2
            ERROR ""
            CALL Inicializa()
         ELSE
            ERROR "PROCESO 'MODIFICAR' CANCELADO."
            SLEEP 2
            ERROR ""
         END IF
            EXIT INPUT

         ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO... VACIO."
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Elimina()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1212" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Elimina                                             (CTRL-C)Salir     " AT 1,1 ATTRIBUTE (REVERSE)
      DISPLAY "          Escoga con <ENTER> el Registro a Eliminar                            " AT 2,1

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo
                        FROM codigo

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
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

      LET sel_where = " SELECT codigo,descripcion,desc_corta FROM tab_edo_comision WHERE ",
                        cla_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query3 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query3

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*
         LET g_edo.codigo      = l_record[pos].codigo
         LET g_edo.descripcion = l_record[pos].descripcion
         LET g_edo.desc_corta  = l_record[pos].desc_corta
         LET pos = pos + 1 
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_edo.codigo      = l_record[pos].codigo
               LET g_edo.descripcion = l_record[pos].descripcion
               LET g_edo.desc_corta  = l_record[pos].desc_corta
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "DEBE ELEGIR UN REGISTRO"
               LET pos = ARR_CURR()
         END DISPLAY

            CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE INFORMES... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY "  ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME g_edo.codigo,
                      g_edo.descripcion,
                      g_edo.desc_corta

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_edo_comision
         WHERE codigo = g_edo.codigo

         ERROR "REGISTRO ELIMINADO."
         SLEEP 2
         ERROR ""
      ELSE
         ERROR "PROCESO 'ELIMINAR' CANCELADO."
         SLEEP 2
         ERROR ""
      END IF

         CALL Inicializa()
      ELSE
         ERROR "ARCHIVO DE INFORMES... VACIO."
      END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------------
FUNCTION Pregunta()

   PROMPT "Estas seguro S/N ? " FOR CHAR aux_pausa

END FUNCTION
-------------------------------------------------------------------------
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

   LET usuario = g_edo.usuario
   LET g_impre = g_param_tab.ruta_listados CLIPPED,"/",
                 usuario CLIPPED, ".IND_TRANSFE",
                 hoy USING "dd-mm-yyyy" CLIPPED

    START REPORT rpt_tabedocom to g_impre

    FOR i = 1 to (pos+1)
       LET g_edo.codigo      = l_record[i].codigo
       LET g_edo.descripcion = l_record[i].descripcion
       LET g_edo.desc_corta  = l_record[i].desc_corta

       IF g_edo.codigo IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabedocom(g_edo.*)
    END FOR

    FINISH REPORT rpt_tabedocom
       ERROR "LISTADO GENERADO..."
       SLEEP 2
       ERROR ""

       LET g_lista = "lp ", g_impre
       RUN g_lista

END FUNCTION
-------------------------------------------------------------------------
REPORT rpt_tabedocom(g_edo)

   DEFINE g_edo          RECORD
          codigo         SMALLINT,
          descripcion    CHAR(70),
          desc_corta     CHAR(15),
          factualiza     DATE,
          usuario        CHAR(8)
   END RECORD

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   88

   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM121 ",
               COLUMN 70,TODAY USING "dd-mm-yyyy"

         PRINT COLUMN 20," CATALOGO DE ESTADO DE COMISION "
            SKIP 2 LINE

         PRINT COLUMN 01,"COD.",
               COLUMN 07,"DESCRIPCION",
               COLUMN 50,"DESCRIPCION CORTA"
            SKIP 1 LINE 

         ON EVERY ROW
            SKIP 1 LINE
         PRINT COLUMN 01,g_edo.codigo USING "<<<<",
               COLUMN 07,g_edo.descripcion CLIPPED,
               COLUMN 50,g_edo.desc_corta  CLIPPED
             SKIP 2 LINE

         ON LAST ROW
         PRINT COLUMN 01," TOTAL DE REGISTROS : ",COUNT(*) USING "<<<"
END REPORT
-------------------------------------------------------------------------
