#############################################################################
# Proyecto     => Sistema de Afores.( MEXICO )                              #
# Propietario  => E.F.P.                                                    #
# Programa     => CATALOGO DE UDIS                                          #
# Fecha        => 1 DE JULIO DEL 2004                                       #
# Autor        => ERIKA PAOLA VERA PIÑA                                     #
############################################################################# 
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis  RECORD LIKE seg_modulo.*

   DEFINE aux_pausa       CHAR(1),
          sw_1            SMALLINT,
          hoy             DATE,
          usuario         CHAR(8),
          pos             INTEGER,
          sel_where       CHAR(30000),
          cla_where       CHAR(30000),
          g_impre         CHAR(300),
          g_lista         CHAR(300)

   DEFINE g_reg           RECORD
          fecha_udi       DATE,
          valor_udi       DECIMAL(16,6)
   END RECORD

   DEFINE l_record        ARRAY[30000] OF RECORD
          fecha_udi       DATE,
          valor_udi       DECIMAL(16,6)
   END RECORD
END GLOBALS
-----------------------------------------------------------------------------
MAIN 
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

      DEFER INTERRUPT
      CALL inicio()
      CALL proceso()
END MAIN
------------------------------------------------------------------------------
FUNCTION inicio()

   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"

END FUNCTION
------------------------------------------------------------------------------
FUNCTION proceso()

   LET HOY = TODAY

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0981" ATTRIBUTE( BORDER)

   DISPLAY " TABM098                 CATALOGO   DE   UDIS                                  " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "CATALOGO UDIS "   
       COMMAND "Agrega" "Agrega Registro"
          CALL Agrega()
          CLEAR SCREEN
       COMMAND "Consulta" "Consulta Registro"
          CALL Consulta()
          CLEAR SCREEN
       COMMAND "Modifica" "Modifica Registro"
          CALL Modifica()
          CLEAR SCREEN
       COMMAND "Elimina" "Elimina Registro"
          CALL Elimina()
          CLEAR SCREEN
       COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU
    CLOSE WINDOW ventana_1
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Inicializa()

   LET sw_1 = 0

   INITIALIZE g_reg.* TO NULL

   DISPLAY BY NAME g_reg.fecha_udi,
                   g_reg.valor_udi

END FUNCTION
-----------------------------------------------------------------------------
FUNCTION Agrega()

   DISPLAY "" AT 1,1

   DISPLAY "" AT 2,1

   DISPLAY " ( ESC ) Agrega                 (CTRL-C) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)

   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_reg.fecha_udi = NULL
   LET g_reg.valor_udi = NULL

   LET sw_1 = 0

   INPUT BY NAME  g_reg.fecha_udi,
                  g_reg.valor_udi

    BEFORE FIELD fecha_udi
      IF sw_1 = 0 THEN
         LET sw_1 = 1
      END IF

   AFTER FIELD fecha_udi
      IF g_reg.fecha_udi IS NULL THEN
         ERROR " ---> LA FECHA NO PUEDE SER NULA"
         NEXT FIELD fecha_udi
      END IF

      SELECT "X"
      FROM   tab_udi
      WHERE  fecha_udi = g_reg.fecha_udi

      IF STATUS <> NOTFOUND THEN
         ERROR " --->FECHA YA INGRESADA"
         NEXT FIELD fecha_udi
      END IF

   AFTER FIELD valor_udi
      IF g_reg.valor_udi IS NULL THEN
         ERROR " ---> ESTE CAMPO NO PUEDE SER NULO"
         NEXT FIELD valor_udi
      END IF

   ON KEY ( ESC )

      IF g_reg.fecha_udi IS NULL THEN
         ERROR " ---> LA FECHA NO PUEDE SER NULA"
         NEXT FIELD fecha_udi
      END IF

      SELECT "X"
      FROM   tab_udi
      WHERE  fecha_udi = g_reg.fecha_udi

      IF STATUS <> NOTFOUND THEN
         ERROR " ---> FECHA YA INGRESADA"
         NEXT FIELD fecha_udi
      END IF

      IF g_reg.valor_udi IS NULL THEN
         ERROR " ---> EL CAMPO NO PUEDE SER NULO"
         NEXT FIELD valor_udi
      END IF

      INSERT INTO tab_udi
      VALUES (g_reg.*)

      ERROR " --->REGISTRO INGRESADO"
      SLEEP 2
      ERROR ""
      CALL Inicializa()
      NEXT FIELD fecha_udi

   ON KEY(INTERRUPT)
      CALL Inicializa()
      EXIT INPUT
  END INPUT
END FUNCTION
-----------------------------------------------------------------------------
FUNCTION Consulta()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0982" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Consulta             (CTRL-P) Imprimir              (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY "                        C O N S U L T A     U D I                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON fecha_udi,
                             valor_udi
                        FROM fecha_udi,
                             valor_udi
 
         ON KEY (CONTROL-M)
            ERROR " --->PROCESANDO INFORMACION..."
            SLEEP 2
            ERROR ""
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            CALL Inicializa()
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
            ERROR " --->BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM tab_udi ",
                      " WHERE ", cla_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.fecha_udi = l_record[pos].fecha_udi
               LET g_reg.valor_udi = l_record[pos].valor_udi
               EXIT DISPLAY
            ON KEY (CONTROL-P)
               ERROR " --->PROCESANDO INFORMACION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
               CALL Inicializa()
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR " --->ARCHIVO... VACIO"
         SLEEP 2
         ERROR ""
         CALL Inicializa()
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF

   DISPLAY BY NAME g_reg.fecha_udi,
                   g_reg.valor_udi

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR " --->CONSULTA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR FORM
         CLEAR SCREEN
         RETURN
      ELSE
         PROMPT " Con <ENTER> Sale de la Consulta " for aux_pausa
      END IF

  CLEAR FORM
  CLEAR SCREEN
END FUNCTION
------------------------------------------------------------------------------
FUNCTION  Modifica()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0982" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Modifica                                            (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY "               Escoja con <ENTER> el Registro a Modificar                      " AT 2,1

      DISPLAY "                        M O D I F I C A    U D I                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON fecha_udi,
                             valor_udi
                        FROM fecha_udi,
                             valor_udi

         ON KEY (CONTROL-M)
            ERROR " --->PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            CALL Inicializa()
            EXIT CONSTRUCT
         END CONSTRUCT

         IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR " --->BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
         END IF

         LET sel_where = " SELECT * ",
                         " FROM tab_udi ",
                         " WHERE ", cla_where CLIPPED,
                         " ORDER BY 1 "

         PREPARE query1 FROM sel_where

         DECLARE cursor_2 CURSOR FOR query1

         LET pos = 1

         FOREACH cursor_2 INTO l_record[pos].*
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record[pos].* TO NULL

         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""

            DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (CONTROL-M)
                  LET pos = ARR_CURR()
                  LET g_reg.fecha_udi = l_record[pos].fecha_udi
                  LET g_reg.valor_udi = l_record[pos].valor_udi
                  EXIT DISPLAY
               ON KEY (INTERRUPT)
                  ERROR " --->DEBE ELEGIR UN REGISTRO."
                  LET pos = ARR_CURR()
            END DISPLAY

            CLOSE WINDOW ventana_2
         ELSE
            ERROR " --->ARCHIVO... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
         END IF

         DISPLAY BY NAME g_reg.fecha_udi,
                         g_reg.valor_udi

         DISPLAY "" AT 1,1
         DISPLAY "" AT 2,1
         DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

         INPUT BY NAME g_reg.fecha_udi,
                       g_reg.valor_udi WITHOUT DEFAULTS

            BEFORE FIELD fecha_udi
               NEXT FIELD valor_udi

            AFTER FIELD valor_udi
               IF g_reg.valor_udi IS NULL THEN
                  ERROR " ---> ESTE CAMPO NO DEBE SER NULO."
                  NEXT FIELD valor_udi
               END IF

               CALL Pregunta()

               IF aux_pausa MATCHES "[Ss]" THEN
                  UPDATE tab_udi SET
                         valor_udi = g_reg.valor_udi
                  WHERE  fecha_udi = g_reg.fecha_udi

                  ERROR " --->REGISTRO MODIFICADO"
                  SLEEP 2
                  ERROR ""

                  CALL Inicializa()
               ELSE
                  ERROR " --->PROCESO DE MODIFICAR CANCELADO."
                  CALL Inicializa()
                  SLEEP 2
                  ERROR ""
               END IF
               EXIT INPUT

            ON KEY (INTERRUPT)
               CALL Inicializa()
               EXIT INPUT
         END INPUT
   ELSE
      ERROR " --->ARCHIVO... VACIO."
   END IF
   CLEAR SCREEN
END FUNCTION
------------------------------------------------------------------------------
FUNCTION Elimina()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 3,3 WITH FORM "TABM0982" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Elimina                                             (CTRL-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY "               Escoja con <ENTER> el Registro a Eliminar                       " AT 2,1

      DISPLAY "                        E L I M I N A    U D I                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON fecha_udi,
                             valor_udi
                        FROM fecha_udi,
                             valor_udi

         ON KEY (CONTROL-M)
            ERROR " --->PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            CALL Inicializa()
            EXIT CONSTRUCT
         END CONSTRUCT

         IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR " --->BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
         END IF


         LET sel_where = " SELECT * ",
                         " FROM tab_udi ",
                         " WHERE ", cla_where CLIPPED,
                         " ORDER BY 1 "

         PREPARE query2 FROM sel_where

         DECLARE cursor_3 CURSOR FOR query2

         LET pos = 1

         FOREACH cursor_3 INTO l_record[pos].*
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record[pos].* TO NULL

         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""
            DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (CONTROL-M)
                  LET pos = ARR_CURR()
                  LET g_reg.fecha_udi = l_record[pos].fecha_udi
                  LET g_reg.valor_udi = l_record[pos].valor_udi
                  EXIT DISPLAY
               ON KEY (INTERRUPT)
                  ERROR " --->DEBE ELEGIR UN REGISTRO."
                  LET pos = ARR_CURR()
            END DISPLAY

            CLOSE WINDOW ventana_2
         ELSE
            ERROR " --->ARCHIVO... VACIO"
            SLEEP 2
            ERROR ""
            CLOSE WINDOW ventana_2
            RETURN
         END IF

         DISPLAY "" AT 1,1
         DISPLAY "" AT 2,1
         DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

         DISPLAY BY NAME g_reg.fecha_udi,
                         g_reg.valor_udi

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_udi
            WHERE  fecha_udi  = g_reg.fecha_udi

            ERROR " --->REGISTRO ELIMINADO."
            SLEEP 2
            ERROR ""
         ELSE
            ERROR " --->PROCESO CANCELADO."
            SLEEP 2
            ERROR ""
         END IF
      CALL Inicializa()
   ELSE
      ERROR " --->ARCHIVO... VACIO."
   END IF
   CLEAR SCREEN
END FUNCTION
-------------------------------------------------------------------------------
FUNCTION Pregunta()

   PROMPT "ESTA SEGURO S/N ? " FOR CHAR aux_pausa

END FUNCTION
-------------------------------------------------------------------------------
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
                 usuario CLIPPED,".IMPTRECHA",
                 hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabtipinfo TO g_impre


   FOR i = 1 TO (pos+1)
       LET g_reg.fecha_udi = l_record[i].fecha_udi
       LET g_reg.valor_udi = l_record[i].valor_udi

       IF g_reg.fecha_udi IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabtipinfo(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabtipinfo

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
-----------------------------------------------------------------------------
REPORT rpt_tabtipinfo(g_reg)

   DEFINE g_reg             RECORD
          fecha_udi         DATE,
          valor_udi         DECIMAL(16,6)
   END RECORD

   DEFINE x_total_caracteres SMALLINT

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

  FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM098 ",
               COLUMN 70,TODAY USING "dd-mm-yyyy"

         PRINT COLUMN 25," CATALOGO DE TAB_UDI "
            SKIP 2 LINE

         PRINT COLUMN 15,"FECHA",
               COLUMN 40,"VALOR"
            SKIP 1 LINE

         ON EVERY ROW
            SKIP 1 LINE
         PRINT COLUMN 15,g_reg.fecha_udi,
               COLUMN 38,g_reg.valor_udi
             SKIP 2 LINE

          ON LAST ROW
             PRINT COLUMN 01," TOTAL DE REGISTROS : ",COUNT(*) USING "<<<"
END REPORT
-----------------------------------------------------------------------------
