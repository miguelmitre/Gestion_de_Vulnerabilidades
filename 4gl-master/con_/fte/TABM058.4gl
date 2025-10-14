####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa TABM058  => Catalogo de Contabilidad de Transacciones    #
#Fecha             => 12/10/2001  .                                #
#Por               => Laura Eugenia Cortes Guzman                  #
#Sistema           => TAB.                                         #
####################################################################
DATABASE safre_af
GLOBALS
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*

    DEFINE enter          CHAR(1)

    DEFINE aux_pausa      CHAR(1),
           sw_1           SMALLINT,
           hoy            DATE,
           sel_where      CHAR(500),
           cla_where      CHAR(300),
           g_impre        CHAR(300),
           g_lista        CHAR(300),
           seg_usuario    CHAR(08),
           pos            SMALLINT

    DEFINE g_reg           RECORD
           proceso_cod     CHAR(05),
           proceso_desc    CHAR(50),
           subcuenta       CHAR(05),
           desc_sub        CHAR(05),
           subcuenta1      CHAR(05),
           desc_sub1       CHAR(05),
           subcuenta2      CHAR(05),
           desc_sub2       CHAR(05),
           transaccion_cod INTEGER,
           descripcion_1   CHAR(60),
           descripcion_2   CHAR(60),
           descripcion_3   CHAR(60),
           descripcion_4   CHAR(60),
           descripcion_5   CHAR(60),
           descripcion_6   CHAR(60),
           descripcion_7   CHAR(60),
           descripcion_8   CHAR(60)
    END RECORD

    DEFINE l_record_2      ARRAY[1] OF RECORD
           proceso_cod     CHAR(05),
           proceso_desc    CHAR(50),
           subcuenta       CHAR(05),
           desc_sub        CHAR(05),
           subcuenta1       CHAR(05),
           desc_sub1        CHAR(05),
           subcuenta2       CHAR(05),
           desc_sub2        CHAR(05),
           transaccion_cod INTEGER,
           descripcion_1   CHAR(60),
           descripcion_2   CHAR(60),
           descripcion_3   CHAR(60),
           descripcion_4   CHAR(60),
           descripcion_5   CHAR(60),
           descripcion_6   CHAR(60),
           descripcion_7   CHAR(60),
           descripcion_8   CHAR(60)
    END RECORD

    DEFINE l_record  ARRAY[1000] OF RECORD
           codigo             INTEGER,
           descripcion_1      CHAR(80)
    END RECORD

    DEFINE l_record1  ARRAY[1000] OF RECORD
           proceso_cod     CHAR(05),
           proceso_desc    CHAR(50),
           subcuenta       CHAR(05),
           desc_sub        CHAR(05),
           subcuenta1       CHAR(05),
           desc_sub1        CHAR(05),
           subcuenta2       CHAR(05),
           desc_sub2        CHAR(05),
           transaccion_cod INTEGER,
           descripcion_1   CHAR(60),
           descripcion_2   CHAR(60),
           descripcion_3   CHAR(60),
           descripcion_4   CHAR(60),
           descripcion_5   CHAR(60),
           descripcion_6   CHAR(60),
           descripcion_7   CHAR(60),
           descripcion_8   CHAR(60)
    END RECORD
END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
           INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
   INITIALIZE g_reg.* TO NULL

   SELECT ruta_listados,USER
   INTO   g_param_dis.ruta_listados, seg_usuario
   FROM   seg_modulo
   where modulo_cod = "con"

END FUNCTION
#####################################################################
FUNCTION proceso()
   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0581" ATTRIBUTE( BORDER)

   DISPLAY " TABM058        CATALOGO DE CONTABILIDAD DE TRANSACCIONES                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CATALOGO TRANSACCIONES "
      COMMAND "Agrega" "Agrega Transaccion"
                       CALL Agrega()
      COMMAND "Consulta" "Consulta Transaccion"
                       CALL Consulta()
      COMMAND "Modifica" "Modifica Transaccion"
                       CALL Modifica()
      COMMAND "Elimina" "Elimina Transaccion"
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
   DISPLAY " ( Esc ) Agrega                   (Ctrl-C) Salir                               " AT 1,1 ATTRIBUTE(BOLD)

   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   INITIALIZE g_reg.* TO NULL
   LET sw_1 = 0


   INPUT BY NAME  g_reg.*

         AFTER FIELD proceso_cod
               IF g_reg.proceso_cod IS NULL THEN
                  ERROR "Proceso NO puede ser nulo"
                  NEXT FIELD  proceso_cod
               END IF

                  SELECT descripcion
                  INTO g_reg.proceso_desc
                  FROM tab_proceso
                  WHERE proceso_cod = g_reg.proceso_cod

               IF STATUS = NOTFOUND THEN
                  ERROR " Proceso No Existe"
                  NEXT FIELD proceso_cod
                  INITIALIZE g_reg.proceso_desc TO NULL
               ELSE
                  DISPLAY BY NAME g_reg.proceso_desc
               END IF

         AFTER FIELD subcuenta

               IF g_reg.subcuenta IS NULL THEN
                  ERROR "Subcuenta NO puede ser nula"
                  NEXT FIELD  subcuenta
               END IF

                  SELECT subct_corta
                  INTO g_reg.desc_sub
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta

               IF STATUS = NOTFOUND THEN
                  ERROR " Subcuenta No Existe"
                  NEXT FIELD subcuenta
                  INITIALIZE g_reg.desc_sub TO NULL
               ELSE
                  DISPLAY BY NAME g_reg.desc_sub
               END IF

         AFTER FIELD subcuenta1

               IF g_reg.subcuenta1 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub1
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta1

                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta1
                     INITIALIZE g_reg.desc_sub1 TO NULL
                  ELSE
                     DISPLAY BY NAME g_reg.desc_sub1
                  END IF
               END IF

                  IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD subcuenta
                  END IF


         AFTER FIELD subcuenta2

               IF g_reg.subcuenta2 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub2
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta2

                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta2
                     INITIALIZE g_reg.desc_sub2 TO NULL
                  ELSE
                     DISPLAY BY NAME g_reg.desc_sub2
                  END IF
               END IF

                  IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD subcuenta1
                  END IF

         AFTER FIELD transaccion_cod
               IF g_reg.transaccion_cod IS NULL THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  transaccion_cod
               END IF

               SELECT "X"
                    FROM tab_transaccion
                    WHERE transaccion_cod = g_reg.transaccion_cod
                      AND proceso_cod = g_reg.proceso_cod

               IF STATUS <> NOTFOUND THEN
                  ERROR "Transaccion  ya Ingresado"
                  NEXT FIELD transaccion_cod
               END IF

         AFTER FIELD descripcion_1
               IF g_reg.descripcion_1 IS NULL THEN
                  ERROR "Descripcion NO puede ser nula"
                  NEXT FIELD  descripcion_1
               END IF

               SELECT "X"
                     FROM tab_transaccion
                     WHERE descripcion_1 = g_reg.descripcion_1

               IF STATUS <> NOTFOUND THEN
                   ERROR "El Codigo ya fue Ingresado"
                   NEXT FIELD transaccion_cod
               END IF

         AFTER FIELD descripcion_2

         AFTER FIELD descripcion_3

         AFTER FIELD descripcion_4

         AFTER FIELD descripcion_5

         AFTER FIELD descripcion_6

         AFTER FIELD descripcion_7

         AFTER FIELD descripcion_8


         ON KEY ( ESC )

               IF g_reg.proceso_cod IS NULL THEN
                  ERROR "Proceso NO puede ser nulo"
                  NEXT FIELD  proceso_cod
               END IF

                  SELECT descripcion
                  INTO g_reg.proceso_desc
                  FROM tab_proceso
                  WHERE proceso_cod = g_reg.proceso_cod

               IF STATUS = NOTFOUND THEN
                  ERROR " Proceso No Existe"
                  NEXT FIELD proceso_cod
               END IF

               IF g_reg.subcuenta IS NULL THEN
                  ERROR "Subcuenta NO puede ser nula"
                  NEXT FIELD  subcuenta
               END IF

                  SELECT subct_corta
                  INTO g_reg.desc_sub
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta

               IF STATUS = NOTFOUND THEN
                  ERROR " Subcuenta No Existe"
                  NEXT FIELD subcuenta
               END IF


               IF g_reg.subcuenta1 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub1
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta1
                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta1
                  END IF
               END IF


               IF g_reg.subcuenta2 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub2
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta2

                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta2
                  END IF
               END IF
               IF g_reg.transaccion_cod IS NULL THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  transaccion_cod
               END IF

               SELECT "X"
                    FROM tab_transaccion
                    WHERE transaccion_cod = g_reg.transaccion_cod
                      AND proceso_cod = g_reg.proceso_cod

               IF STATUS <> NOTFOUND THEN
                  ERROR "Codigo ya Ingresado"
                  NEXT FIELD transaccion_cod
               END IF


               IF g_reg.descripcion_1 IS NULL THEN
                  ERROR "Descripcion NO puede ser nula"
                  NEXT FIELD  descripcion_1
               END IF

               SELECT "X"
                     FROM tab_transaccion
                     WHERE descripcion_1 = g_reg.descripcion_1

               IF STATUS <> NOTFOUND THEN
                   ERROR "El Codigo ya fue Ingresado"
                   NEXT FIELD transaccion_cod
               END IF



               INSERT INTO tab_transaccion VALUES ( g_reg.transaccion_cod,
                                                    g_reg.descripcion_1,
                                                    g_reg.descripcion_2,
                                                    g_reg.descripcion_3,
                                                    g_reg.descripcion_4,
                                                    g_reg.descripcion_5,
                                                    g_reg.descripcion_6,
                                                    g_reg.descripcion_7,
                                                    g_reg.descripcion_8,
                                                    g_reg.proceso_cod,
                                                    g_reg.subcuenta,
                                                    g_reg.subcuenta1,
                                                    g_reg.subcuenta2 )

               ERROR "REGISTRO INGRESADO"
                     SLEEP 2
               ERROR ""

               CALL Inicializa()
               NEXT FIELD proceso_cod

         ON KEY (CONTROL-C)
               CALL Inicializa()
               EXIT INPUT

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0582" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod, transaccion_cod
                          FROM proceso_cod, transaccion_cod

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
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT proceso_cod, '', ",
                             " subcuenta, '',  ",
                             " subcuenta1, '', ",
                             " subcuenta2, '', ",
                             " transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8 ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,5 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record1[pos].*

         SELECT descripcion
         INTO   l_record1[pos].proceso_desc
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub1
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta1

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub2
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta2

         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record1 TO scr_1.*
            ON KEY (CONTROL-P)
               ERROR "PROCESANDO INFORMACION ..."
               CALL impresion(pos)

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE TRANSACCIONES .... VACIO"
         SLEEP 2
         ERROR ""
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0582" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta               (Esc) Selec.Todo             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > el registro a modificar                  " AT 2,1
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod, transaccion_cod
                          FROM proceso_cod, transaccion_cod

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 2
            ERROR ""
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

      LET sel_where = "SELECT proceso_cod, '', ",
                             " subcuenta, '',  ",
                             " subcuenta1, '', ",
                             " subcuenta2, '', ",
                             " transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8 ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,5 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1
      LET pos = 1

      FOREACH cursor_2 INTO l_record1[pos].*

         SELECT descripcion
         INTO   l_record1[pos].proceso_desc
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod


         SELECT subct_corta
         INTO   l_record1[pos].desc_sub
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta


         SELECT subct_corta
         INTO   l_record1[pos].desc_sub1
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta1


         SELECT subct_corta
         INTO   l_record1[pos].desc_sub2
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta2

         LET pos = pos +1
      END FOREACH


      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1)
          ERROR ""
          DISPLAY ARRAY l_record1 TO scr_1.*
             ON KEY (CONTROL-M)

                LET pos = ARR_CURR()
                LET g_reg.proceso_cod     = l_record1[pos].proceso_cod
                LET g_reg.transaccion_cod = l_record1[pos].transaccion_cod
                LET g_reg.subcuenta       = l_record1[pos].subcuenta
                LET g_reg.subcuenta1      = l_record1[pos].subcuenta1
                LET g_reg.subcuenta2      = l_record1[pos].subcuenta2
                LET g_reg.descripcion_1   = l_record1[pos].descripcion_1
                LET g_reg.descripcion_2   = l_record1[pos].descripcion_2
                LET g_reg.descripcion_3   = l_record1[pos].descripcion_3
                LET g_reg.descripcion_4   = l_record1[pos].descripcion_4
                LET g_reg.descripcion_5   = l_record1[pos].descripcion_5
                LET g_reg.descripcion_6   = l_record1[pos].descripcion_6
                LET g_reg.descripcion_7   = l_record1[pos].descripcion_7
                LET g_reg.descripcion_8   = l_record1[pos].descripcion_8

                EXIT DISPLAY

             ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT DISPLAY

             ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT DISPLAY

          END DISPLAY
           IF int_flag = TRUE THEN
              LET int_flag = FALSE
              ERROR "BUSQUEDA CANCELADA..."
              SLEEP 2
              ERROR ""
              CLEAR SCREEN
              CLOSE WINDOW ventana_2
              RETURN
           END IF
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE TRANSACCIONES .... INEXISTE"
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

          BEFORE FIELD proceso_cod
                SELECT descripcion
                INTO   l_record1[pos].proceso_desc
                FROM   tab_proceso
                WHERE  proceso_cod = l_record1[pos].proceso_cod
                SELECT subct_corta
                INTO   l_record1[pos].desc_sub
                FROM   tab_subcuenta
                WHERE  subct_cod = l_record1[pos].subcuenta

                SELECT subct_corta
                INTO   l_record1[pos].desc_sub1
                FROM   tab_subcuenta
                WHERE  subct_cod = l_record1[pos].subcuenta1

                SELECT subct_corta
                INTO   l_record1[pos].desc_sub2
                FROM   tab_subcuenta
                WHERE  subct_cod = l_record1[pos].subcuenta2

                DISPLAY BY NAME l_record1[pos].desc_sub,
                                l_record1[pos].desc_sub1,
                                l_record1[pos].desc_sub2,
                                l_record1[pos].proceso_desc

                 NEXT FIELD subcuenta

         AFTER FIELD subcuenta

               IF g_reg.subcuenta IS NULL THEN
                  ERROR "Subcuenta NO puede ser nula"
                  NEXT FIELD  subcuenta
               END IF

                  SELECT subct_corta
                  INTO g_reg.desc_sub
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta

               IF STATUS = NOTFOUND THEN
                  ERROR " Subcuenta No Existe"
                  NEXT FIELD subcuenta
                  INITIALIZE g_reg.desc_sub TO NULL
               ELSE
                  DISPLAY BY NAME g_reg.desc_sub
               END IF

                  NEXT FIELD  subcuenta1


         AFTER FIELD subcuenta1

               IF g_reg.subcuenta1 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub1
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta1

                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta1
                     INITIALIZE g_reg.desc_sub1 TO NULL
                  ELSE
                     DISPLAY BY NAME g_reg.desc_sub1
                  END IF
               END IF

                  IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD subcuenta
                  END IF

                  NEXT FIELD  subcuenta2

         AFTER FIELD subcuenta2

               IF g_reg.subcuenta2 IS NOT NULL THEN

                  SELECT subct_corta
                  INTO g_reg.desc_sub2
                  FROM tab_subcuenta
                  WHERE subct_cod = g_reg.subcuenta2

                  IF STATUS = NOTFOUND THEN
                     ERROR " Subcuenta No Existe"
                     NEXT FIELD subcuenta2
                     INITIALIZE g_reg.desc_sub2 TO NULL
                  ELSE
                     DISPLAY BY NAME g_reg.desc_sub2
                  END IF
               END IF

                  IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD subcuenta1
                  END IF

                  NEXT FIELD descripcion_1

          AFTER FIELD descripcion_1

             IF g_reg.descripcion_1 IS NULL THEN
                ERROR "La descripcion no puede ser Nula "
                NEXT FIELD  descripcion_1
             END IF

          AFTER FIELD descripcion_2
          AFTER FIELD descripcion_3
          AFTER FIELD descripcion_4
          AFTER FIELD descripcion_5
          AFTER FIELD descripcion_6
          AFTER FIELD descripcion_7
          AFTER FIELD descripcion_8

             CALL Pregunta()

             IF aux_pausa MATCHES "[Ss]" THEN
                UPDATE tab_transaccion
                SET    subcuenta     = g_reg.subcuenta,
                       subcuenta1    = g_reg.subcuenta1,
                       subcuenta2    = g_reg.subcuenta2,
                       descripcion_1 = g_reg.descripcion_1,
                       descripcion_2 = g_reg.descripcion_2,
                       descripcion_3 = g_reg.descripcion_3,
                       descripcion_4 = g_reg.descripcion_4,
                       descripcion_5 = g_reg.descripcion_5,
                       descripcion_6 = g_reg.descripcion_6,
                       descripcion_7 = g_reg.descripcion_7,
                       descripcion_8 = g_reg.descripcion_8
                WHERE  transaccion_cod   = g_reg.transaccion_cod

                ERROR "REGISTRO MODIFICADO"
                SLEEP 2
                ERROR ""

                CALL Inicializa()
             ELSE
                ERROR "PROCESO DE MODIFICAR, CANCELADO"
                SLEEP 2
                ERROR ""
             END IF
             EXIT INPUT

          ON KEY ( INTERRUPT )
             CALL Inicializa()
             EXIT INPUT

          ON KEY ( CONTROL-C )
             CALL Inicializa()
             EXIT INPUT
       END INPUT
    ELSE
       ERROR "ARCHIVO DE TRANSACCIONES.... VACIO"
    END IF
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0582" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con < ENTER > el registro a eliminar                  " AT 2,1
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod, transaccion_cod
                          FROM proceso_cod, transaccion_cod

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
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT proceso_cod, '', ",
                             " subcuenta, '',  ",
                             " subcuenta1, '', ",
                             " subcuenta2, '', ",
                             " transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8 ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,5 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record1[pos].*
         SELECT descripcion
         INTO   l_record1[pos].proceso_desc
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub1
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta1

         SELECT subct_corta
         INTO   l_record1[pos].desc_sub2
         FROM   tab_subcuenta
         WHERE  subct_cod = l_record1[pos].subcuenta2
         LET pos = pos +1
      END FOREACH


      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""
         DISPLAY ARRAY l_record1 TO scr_1.*

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

                LET g_reg.proceso_cod     = l_record1[pos].proceso_cod
                LET g_reg.transaccion_cod = l_record1[pos].transaccion_cod

                LET g_reg.subcuenta       = l_record1[pos].subcuenta
                LET g_reg.subcuenta1      = l_record1[pos].subcuenta1
                LET g_reg.subcuenta2      = l_record1[pos].subcuenta2
                LET g_reg.descripcion_1   = l_record1[pos].descripcion_1
                LET g_reg.descripcion_2   = l_record1[pos].descripcion_2
                LET g_reg.descripcion_3   = l_record1[pos].descripcion_3
                LET g_reg.descripcion_4   = l_record1[pos].descripcion_4
                LET g_reg.descripcion_5   = l_record1[pos].descripcion_5
                LET g_reg.descripcion_6   = l_record1[pos].descripcion_6
                LET g_reg.descripcion_7   = l_record1[pos].descripcion_7
                LET g_reg.descripcion_8   = l_record1[pos].descripcion_8

               EXIT DISPLAY

             ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT DISPLAY

            ON KEY (INTERRUPT)
               LET int_flag = TRUE
               EXIT DISPLAY
    END DISPLAY
           IF int_flag = TRUE THEN
              LET int_flag = FALSE
              ERROR "BUSQUEDA CANCELADA..."
              SLEEP 2
              ERROR ""
              CLEAR SCREEN
              CLOSE WINDOW ventana_2
              RETURN
           END IF

    CLOSE WINDOW ventana_2
      ELSE
         ERROR "INEXISTENTE REGISTRO DEL ORIGEN "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

         SELECT descripcion
         INTO   g_reg.proceso_desc
         FROM   tab_proceso
         WHERE  proceso_cod = g_reg.proceso_cod

         SELECT subct_corta
         INTO   g_reg.desc_sub
         FROM   tab_subcuenta
         WHERE  subct_cod = g_reg.subcuenta

         SELECT subct_corta
         INTO   g_reg.desc_sub1
         FROM   tab_subcuenta
         WHERE  subct_cod = g_reg.subcuenta1

         SELECT subct_corta
         INTO   g_reg.desc_sub2
         FROM   tab_subcuenta
         WHERE  subct_cod = g_reg.subcuenta2

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()
         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_transaccion
                   WHERE transaccion_cod = g_reg.transaccion_cod
                     AND proceso_cod = g_reg.proceso_cod

            ERROR "REGISTRO ELIMINADO"
            SLEEP 2
         ELSE
            ERROR "PROCESO CANCELADA"
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "VACIO ARCHIVO DEL ORIGEN "
   END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                 ".CON_TRAN",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabTransaccion TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.proceso_cod        = l_record1[i].proceso_cod
       LET g_reg.proceso_desc       = l_record1[i].proceso_desc
       LET g_reg.transaccion_cod    = l_record1[i].transaccion_cod
       LET g_reg.subcuenta          = l_record1[i].subcuenta
       LET g_reg.desc_sub           = l_record1[i].desc_sub
       LET g_reg.subcuenta1         = l_record1[i].subcuenta1
       LET g_reg.desc_sub1          = l_record1[i].desc_sub1
       LET g_reg.subcuenta2         = l_record1[i].subcuenta2
       LET g_reg.desc_sub2          = l_record1[i].desc_sub2
       LET g_reg.descripcion_1      = l_record1[i].descripcion_1
       LET g_reg.descripcion_2      = l_record1[i].descripcion_2
       LET g_reg.descripcion_3      = l_record1[i].descripcion_3
       LET g_reg.descripcion_4      = l_record1[i].descripcion_4
       LET g_reg.descripcion_5      = l_record1[i].descripcion_5
       LET g_reg.descripcion_6      = l_record1[i].descripcion_6
       LET g_reg.descripcion_7      = l_record1[i].descripcion_7
       LET g_reg.descripcion_8      = l_record1[i].descripcion_8

       IF g_reg.transaccion_cod IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabTransaccion(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabTransaccion

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabTransaccion(g_reg)
    DEFINE g_reg     RECORD
       proceso_cod     CHAR(05),
       proceso_desc    CHAR(30),
       subcuenta       CHAR(05),
       desc_sub        CHAR(05),
       subcuenta1      CHAR(05),
       desc_sub1       CHAR(05),
       subcuenta2      CHAR(05),
       desc_sub2       CHAR(05),
       transaccion_cod INTEGER,
       descripcion_1   CHAR(40),
       descripcion_2   CHAR(40),
       descripcion_3   CHAR(40),
       descripcion_4   CHAR(40),
       descripcion_5   CHAR(40),
       descripcion_6   CHAR(40),
       descripcion_7   CHAR(40),
       descripcion_8   CHAR(40)
    END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  90
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01," TABM058 ",
               COLUMN 29," LISTADO DE CONTABILIDAD DE TRANSACCIONES ",
               COLUMN 110,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

         PRINT COLUMN 01,"PROCESO ",
               COLUMN 10,"TRANSACCION ",
               COLUMN 30,"DESCRIPCION ",
               COLUMN 75,"SUBC-1",
               COLUMN 92,"SUBC-2",
               COLUMN 109,"SUBC-3"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,g_reg.proceso_cod,
               COLUMN 10,g_reg.transaccion_cod,
               COLUMN 30,g_reg.descripcion_1,
               COLUMN 75,g_reg.subcuenta,
               COLUMN 82,g_reg.desc_sub,
               COLUMN 92,g_reg.subcuenta1,
               COLUMN 99,g_reg.desc_sub1,
               COLUMN 109,g_reg.subcuenta2,
               COLUMN 116,g_reg.desc_sub2

         IF g_reg.descripcion_2 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_2
         END IF

         IF g_reg.descripcion_3 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_3
         END IF

         IF g_reg.descripcion_4 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_4
         END IF
         IF g_reg.descripcion_5 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_5
         END IF

         IF g_reg.descripcion_6 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_6
         END IF

         IF g_reg.descripcion_7 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_7
         END IF

         IF g_reg.descripcion_8 IS NOT NULL THEN
         PRINT COLUMN 30,g_reg.descripcion_8
         END IF

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 30," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
