################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Programa INTM0902 => MANTENIMIENTO A PRECIO DE UDIS                           #
#Fecha             => 21 Noviembre 2008                                        #
#Autor             => ISABEL FONSECA FRIAS                                     #
#Fecha modifica    =>                                                          #
#Actualizado       =>                                                          #
#Sistema           => INT                                                      #
################################################################################
DATABASE safre_af
GLOBALS

        DEFINE aux_pausa                CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               sel_where                CHAR(300),
               cla_where                CHAR(300),
               seg_usuario              CHAR(08),
               v_salir                  CHAR(01),
               v_codigo_afore           SMALLINT,
               v_codigo_siefore         LIKE tab_siefore_local.codigo_siefore,
               pos                      SMALLINT,
               v_time                   DATETIME YEAR TO SECOND

        DEFINE g_reg            RECORD
               codigo_siefore           LIKE glo_valor_accion.codigo_siefore,
               razon_social             LIKE tab_siefore_local.razon_social,
               precio_del_dia           LIKE glo_valor_accion.precio_del_dia,
               fecha_valuacion          LIKE glo_valor_accion.fecha_valuacion
        END RECORD

        DEFINE l_record         ARRAY[1500] OF RECORD
               codigo_siefore           LIKE glo_valor_accion.codigo_siefore,
               razon_social             LIKE tab_siefore_local.razon_social,
               precio_del_dia           LIKE glo_valor_accion.precio_del_dia,
               fecha_valuacion          LIKE glo_valor_accion.fecha_valuacion
        END RECORD
END GLOBALS
#####################################################################
MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP,
                ACCEPT KEY control-o

        SELECT codigo_afore
          INTO v_codigo_afore
          FROM tab_afore_local

        LET v_time  = CURRENT

        DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()

   SELECT USER,*
   INTO seg_usuario
   FROM glo_parametro

END FUNCTION
#####################################################################
FUNCTION proceso()
        LET HOY = TODAY
        OPEN WINDOW ventana_1 AT 3,3 WITH FORM "INTM09021" ATTRIBUTE( BORDER)
        DISPLAY " INTM0902                    PRECIOS DE LAS UDIS                               " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

        MENU "PRECIOS DE LAS UDIS "
                           COMMAND "Agrega" "Agrega Precio de las UDIS"
                                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Precio de las UDIS"
                                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Precio de las UDIS"
                                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Precio de las UDIS"
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
        DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.precio_del_dia   = NULL

        LET sw_1 = 0

        INPUT BY NAME  g_reg.*

              AFTER FIELD codigo_siefore
                LET g_reg.fecha_valuacion  = HOY
                    IF g_reg.codigo_siefore <> 13 THEN
                       ERROR "Codigo de Siefore NO puede ser diferente a 13"
                       NEXT FIELD codigo_siefore
                    ELSE

                       LET v_codigo_siefore = NULL

                       SELECT codigo_siefore,
                              razon_social
                         INTO v_codigo_siefore,
                              g_reg.razon_social
                         FROM tab_siefore_local
                        WHERE codigo_afore = v_codigo_afore
                          AND codigo_siefore = g_reg.codigo_siefore


                     DISPLAY BY NAME  g_reg.razon_social

                   END IF

              BEFORE FIELD precio_del_dia
                    IF g_reg.codigo_siefore IS NULL THEN
                       ERROR "Codigo de Siefore NO puede ser nulo"
                       NEXT FIELD codigo_siefore

                    END IF

              AFTER FIELD precio_del_dia

                   IF g_reg.precio_del_dia < 0 THEN
                      ERROR "Precio de la UDI  NO puede ser NEGATIVO"
                      NEXT FIELD precio_del_dia
                   END IF

                   IF g_reg.codigo_siefore <> 0 THEN
                       IF g_reg.precio_del_dia         IS NULL OR
                          g_reg.precio_del_dia = 0 THEN
                          ERROR "Precio de la UDI  NO puede ser nulo o cero"
                          NEXT FIELD precio_del_dia
                       END IF
                    ELSE
                      LET g_reg.precio_del_dia = 0
                    END IF


              AFTER FIELD fecha_valuacion
                     IF g_reg.fecha_valuacion IS NULL THEN
                        ERROR "Fecha de Valuacion NO puede ser nula"
                        NEXT FIELD fecha_valuacion
                     ELSE

                           LET v_codigo_siefore = Null

                           SELECT codigo_siefore
                             INTO v_codigo_siefore
                             FROM glo_valor_accion
                            WHERE codigo_siefore  = g_reg.codigo_siefore
                              AND fecha_valuacion = g_reg.fecha_valuacion

                            IF v_codigo_siefore IS NOT NULL THEN
                               ERROR "Precio de la UDI ya fue capturada con misma fecha"
                               NEXT FIELD codigo_siefore
                            END IF

                     END IF


              ON KEY ( ESC )

                    IF g_reg.codigo_siefore <> 13  THEN
                       ERROR "Codigo de Siefore NO puede ser diferente a 13"
                       NEXT FIELD codigo_siefore
                    END IF

                     IF g_reg.precio_del_dia < 0 THEN
                        ERROR "Precio de la UDI  NO puede ser NEGATIVO"
                        NEXT FIELD precio_del_dia
                     END IF

                       IF g_reg.precio_del_dia         IS NULL OR
                          g_reg.precio_del_dia = 0 THEN
                          ERROR "Precio de la UDI no puede ser nulo o cero"
                          NEXT FIELD precio_del_dia
                       END IF

                     IF g_reg.fecha_valuacion IS NULL THEN
                        ERROR "Fecha de Valuacion NO puede ser nula"
                        NEXT FIELD fecha_valuacion
                     ELSE

                           LET v_codigo_siefore = Null

                           SELECT codigo_siefore
                             INTO v_codigo_siefore
                             FROM glo_valor_accion
                            WHERE codigo_siefore  = g_reg.codigo_siefore
                              AND fecha_valuacion = g_reg.fecha_valuacion

                            IF v_codigo_siefore IS NOT NULL THEN
                               ERROR "Precio de las UDIS  ya fue capturada con misma fecha"
                               NEXT FIELD codigo_siefore
                            END IF
                     END IF

                     INSERT INTO glo_valor_accion VALUES (
                                                  g_reg.codigo_siefore,
                                                  g_reg.precio_del_dia,
                                                  "",
                                                  g_reg.fecha_valuacion,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0)
                      -- Inserta registro en historico de precios de las UDIS


                      INSERT INTO glo_valor_acc_his  VALUES (
                                                  g_reg.codigo_siefore,
                                                  g_reg.precio_del_dia,
                                                  "",
                                                  g_reg.fecha_valuacion,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  "AGREGA",
                                                  seg_usuario,
                                                  v_time)


                     ERROR "REGISTRO INGRESADO"
                     SLEEP 2
                     ERROR ""

                     CALL Inicializa()
                     NEXT FIELD codigo_siefore
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "INTM09022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                    PRECIOS   DE   LA   ACCION                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo_siefore,
                             fecha_valuacion
                             FROM codigo_siefore,
                             fecha_valuacion
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

      LET sel_where = "SELECT codigo_siefore,",
                      "' ',",
                      "precio_del_dia,",
                      "fecha_valuacion ",
                      "FROM glo_valor_accion WHERE ",cla_where CLIPPED,
                      "  AND codigo_siefore = ", 13,
                      "ORDER BY 1,4 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*

           LET  l_record[pos].razon_social = NULL

         SELECT razon_social
           INTO l_record[pos].razon_social
           FROM tab_siefore_local
          WHERE codigo_afore   = v_codigo_afore
            AND codigo_siefore = l_record[pos].codigo_siefore

         IF pos = 1500 THEN
            ERROR "EL ARREGLO HA SIDO SOBREPASADO...SOLO CARGO 1500 REGISTROS" ATTRIBUTE(NORMAL)
            EXIT FOREACH
         END IF

         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE PRECIOS DE LA ACCION   .... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN                           -- IF 1
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "INTM09022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                   Escoja con < ENTER > el precio de las UDIS            " AT 2,1
      DISPLAY "                         PRECIO  DE  LA  ACCCION                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo_siefore,
                             fecha_valuacion
                             FROM codigo_siefore,
                             fecha_valuacion
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT


      IF int_flag = TRUE THEN                       --IF2
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF                                         --END IF 2

      LET sel_where = "SELECT codigo_siefore,",
                      "' ',",
                      "precio_del_dia,",
                      "fecha_valuacion ",
                      "FROM glo_valor_accion WHERE ",cla_where CLIPPED,
                      "  AND codigo_siefore = ", 13,
                      "ORDER BY 1,4 "


      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos     = 1
      LET v_salir = "0"

      FOREACH cursor_2 INTO l_record[pos].*       -- FOREACH 1

         SELECT razon_social
           INTO l_record[pos].razon_social
           FROM tab_siefore_local
          WHERE codigo_afore   = v_codigo_afore
            AND codigo_siefore = l_record[pos].codigo_siefore

        LET pos = pos +1

        IF pos = 1500 THEN                        --IF 3
           ERROR " EL ARREGLO HA SIDO SOBREPASADO....solo se cargaron 1500 registros" ATTRIBUTE(NORMAL)
           EXIT  FOREACH
        END IF                                    -- END 3

      END FOREACH                                -- END FOREACHA

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN                        -- IF 4
          CALL SET_COUNT(pos-1)
          DISPLAY ARRAY l_record TO scr_1.*
             ON KEY (control-m)
                LET pos = ARR_CURR()

                SELECT razon_social
                  INTO g_reg.razon_social
                  FROM tab_siefore_local
                 WHERE codigo_afore = v_codigo_afore
                   AND codigo_siefore = l_record[pos].codigo_siefore

                LET g_reg.codigo_siefore  = l_record[pos].codigo_siefore
                LET g_reg.precio_del_dia  = l_record[pos].precio_del_dia
                LET g_reg.fecha_valuacion = l_record[pos].fecha_valuacion
                EXIT DISPLAY
             ON KEY (INTERRUPT)
                LET v_salir = "1"
                EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE                                         -- ELSE 4
          ERROR "REGISTROS DE PRECIO DE LA ACCION .... NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF                                       -- END IF 4

 IF v_salir = "0" THEN
       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1
       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

       INPUT BY NAME  g_reg.* WITHOUT DEFAULTS
          BEFORE FIELD codigo_siefore
             NEXT FIELD precio_del_dia

          AFTER FIELD precio_del_dia

             IF g_reg.precio_del_dia < 0 THEN
                ERROR "Precio de las UDIS  NO puede ser NEGATIVO"
                NEXT FIELD precio_del_dia
             END IF

             IF g_reg.codigo_siefore <> 0 THEN
                IF g_reg.precio_del_dia         IS NULL OR
                   g_reg.precio_del_dia = 0 THEN
                   ERROR "Precio de las UDIS NO puede ser nulo o cero"
                   NEXT FIELD precio_del_dia
                END IF
             END IF

          AFTER FIELD fecha_valuacion
             IF g_reg.fecha_valuacion IS NULL THEN
                ERROR "Fecha de Valuacion NO puede ser nula"
                NEXT FIELD fecha_valuacion
             ELSE
                IF g_reg.fecha_valuacion < HOY THEN
                   ERROR "Fecha no se puede modificar"
                   NEXT FIELD fecha_valuacion
                ELSE
                   LET v_codigo_siefore = Null

                   SELECT codigo_siefore
                     INTO v_codigo_siefore
                     FROM glo_valor_accion
                    WHERE codigo_siefore  = g_reg.codigo_siefore
                      AND fecha_valuacion = g_reg.fecha_valuacion
                      AND precio_del_dia  = g_reg.precio_del_dia

                   IF v_codigo_siefore IS NOT NULL THEN
                      ERROR "Precio de las UDIS  ya fue capturada con misma fecha"
                      NEXT FIELD codigo_siefore
                   END IF

                END IF
              END IF


             CALL Pregunta()

             IF aux_pausa MATCHES "[Ss]" THEN                   -- IF 7
                UPDATE glo_valor_accion SET
                       precio_del_dia = g_reg.precio_del_dia
                WHERE  codigo_siefore = g_reg.codigo_siefore
                  AND  fecha_valuacion = g_reg.fecha_valuacion

               -- Inserta registro en historico de precios de las UDIS

                      INSERT INTO glo_valor_acc_his  VALUES (
                                                  g_reg.codigo_siefore,
                                                  g_reg.precio_del_dia,
                                                  "",
                                                  g_reg.fecha_valuacion,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  "MODIFICA",
                                                  seg_usuario,
                                                  v_time)


               ERROR "REGISTRO MODIFICADO"
                SLEEP 2
               ERROR ""

                CALL Inicializa()
             ELSE                                               --ELSE 7
                ERROR "PROCESO DE MODIFICAR,CANCELADO"
                SLEEP 2
                ERROR ""
             END IF                                             -- END IF 7
            EXIT INPUT
          ON KEY ( INTERRUPT )
             CALL Inicializa()
             ERROR "PROCESO DE MODIFICAR,CANCELADO"
             SLEEP 2
             ERROR ""
             EXIT INPUT
       END INPUT
END IF
    ELSE                                                       -- ELSE 1
       ERROR "ARCHIVO DE PRECIOS DE LA ACCION   .... VACIO"
    END IF                                                     -- END IF 1
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   LET v_salir = "0"

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "INTM09022" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                    Escoja con < ENTER > el precio de las UDIS                 " AT 2,1
      DISPLAY "                           PRECIOS   DE  LA  ACCION                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo_siefore,
                             fecha_valuacion
                             FROM codigo_siefore,
                             fecha_valuacion
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


      LET sel_where = "SELECT codigo_siefore,",
                      "' ',",
                      "precio_del_dia,",
                      "fecha_valuacion ",
                      "FROM glo_valor_accion WHERE ",cla_where CLIPPED,
                      "  AND codigo_siefore = ", 13,
                      "ORDER BY 1,4 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*

         SELECT razon_social
           INTO l_record[pos].razon_social
           FROM tab_siefore_local
          WHERE codigo_afore   = v_codigo_afore
            AND codigo_siefore = l_record[pos].codigo_siefore

        LET pos = pos +1
        IF pos = 1500 THEN
           ERROR " EL ARREGLO HA SIDO SOBREPASADO...solo se cargaron 1500 registros" ATTRIBUTE(NORMAL)
           EXIT FOREACH
        END IF
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)


                SELECT razon_social
                  INTO g_reg.razon_social
                  FROM tab_siefore_local
                 WHERE codigo_afore = v_codigo_afore
                   AND codigo_siefore = l_record[pos].codigo_siefore

               LET pos = ARR_CURR()
               LET g_reg.codigo_siefore  = l_record[pos].codigo_siefore
               LET g_reg.precio_del_dia  = l_record[pos].precio_del_dia
               LET g_reg.fecha_valuacion = l_record[pos].fecha_valuacion
               EXIT DISPLAY


            ON KEY (INTERRUPT)
                LET v_salir = "1"
                EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "REGISTRO DE PRECIO DE LA ACCION .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

   IF v_salir = "0" THEN

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN

            IF g_reg.fecha_valuacion < HOY THEN
               ERROR "Fecha no se puede Eliminar"
               SLEEP 3
            ELSE

               DELETE FROM glo_valor_accion
               WHERE codigo_siefore   = g_reg.codigo_siefore
                 AND fecha_valuacion  = g_reg.fecha_valuacion

                  -- Inserta registro en historico de precios de las UDIS

                         INSERT INTO glo_valor_acc_his  VALUES (
                                                     g_reg.codigo_siefore,
                                                     g_reg.precio_del_dia,
                                                     " ",
                                                     g_reg.fecha_valuacion,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     "ELIMINA",
                                                     seg_usuario,
                                                     v_time)

               ERROR "REGISTRO ELIMINADO"
               SLEEP 2
            END IF
         ELSE
            ERROR "ELIMINAR CANCELADO"
            SLEEP 2
         END IF
END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE PRECIOS DE LA ACCION  .... VACIO"
   END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
        PROMPT "Este proceso requiere autorizacion del supervisor .....Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
