################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Programa TABM112  => CATALOGO DE CTAS CONTABLES                               #
#Fecha             => 14 Enero 2004                                            #
#Autor             => ISABEL FONSECA FRIAS                                     #
#Fecha modifica    =>                                                          #
#Actualizado       =>                                                          #
#Sistema           => TAB.                                                     #
################################################################################
DATABASE safre_af
GLOBALS

         DEFINE aux_pausa         CHAR(1),
                sw_1              SMALLINT,
                sino              SMALLINT,
                usuario           CHAR(8),
                HOY               DATE,
                pos               INTEGER,
                sel_where         CHAR(3000),
                cla_where         CHAR(3000)

         DEFINE g_reg             RECORD
                proceso_cod       char(5),
                transaccion_cod   integer,
                siefore           smallint, 
                desc_siefore      char(8),
                cuenta            char(9),                                
                descripcion       char(25), 
                tipo              char(1), 
                identificador     smallint,
                analisis_cod      char(6)

         END RECORD

         DEFINE l_record        ARRAY[30000] OF RECORD
                proceso_cod       char(5),
                transaccion_cod   integer,
                siefore           smallint, 
                desc_siefore      char(8),
                cuenta            char(9),                                
                descripcion       char(25), 
                tipo              char(1), 
                identificador     smallint,
                analisis_cod      char(6)
         END RECORD

END GLOBALS
################################################################################
MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP,
                ACCEPT KEY control-o

        DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#################################################D MAIN
################################################################################
FUNCTION inicio()
        SELECT USER,*
        INTO   usuario
        FROM   glo_parametro

END FUNCTION
################################################################################
FUNCTION proceso()
        LET HOY = TODAY
        OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1121" ATTRIBUTE( BORDER)
        DISPLAY " TABM112                CATALOGO CUENTAS CONTABLES                             " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

        MENU "CATALOGO CTAS CONT"
                           COMMAND "Agrega" "Agrega Cta. Contable"
                                     CALL Agrega()
                           COMMAND "Consulta" "Consulta Cta. Contable"
                                     CALL Consulta()
                           COMMAND "Modifica" "Modifica Cta. Contable"
                                     CALL Modifica()
                           COMMAND "Elimina" "Elimina Cta. Contable"
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
        INITIALIZE sino TO NULL
        DISPLAY BY NAME g_reg.*
END FUNCTION
################################################################################
FUNCTION Agrega()
        DISPLAY "" AT 1,1
        DISPLAY "" AT 2,1
        DISPLAY " ( Esc ) Agrega        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET sw_1 = 0
        INPUT BY NAME  g_reg.*
              AFTER FIELD proceso_cod 
                    IF g_reg.proceso_cod IS NULL THEN
                       ERROR "Codigo de Proceso NO puede ser nulo"
                       NEXT FIELD proceso_cod
                    END IF
   
                     SELECT "X"
                     FROM tab_proceso 
                     WHERE proceso_cod = g_reg.proceso_cod
                     IF STATUS = NOTFOUND THEN
                        ERROR "No existe Codigo de Proceso"
                        NEXT FIELD proceso_cod
                     ELSE
                        IF g_reg.transaccion_cod IS  NULL OR 
                           g_reg.transaccion_cod = " "    OR 
                           sino = 1                       THEN
                           NEXT FIELD transaccion_cod
                        ELSE
                           NEXT FIELD siefore 
                        END IF
                     END IF

              AFTER FIELD transaccion_cod  
                     IF g_reg.transaccion_cod IS NULL THEN
                        ERROR "Codigo de Transaccion NO puede ser nulo"
                        NEXT FIELD  transaccion_cod
                     END IF

                     SELECT "X"
                     FROM tab_transaccion
                     WHERE proceso_cod = g_reg.proceso_cod
                       AND transaccion_cod = g_reg.transaccion_cod
                     IF STATUS = NOTFOUND THEN
                        LET sino = 1
                        ERROR "No existe Transaccion"
                        NEXT FIELD proceso_cod 
                     ELSE 
                        INITIALIZE sino TO NULL
                     END IF

              AFTER FIELD siefore
                    IF g_reg.siefore IS NULL THEN
                        ERROR "Siefore NO puede ser nula"
                        NEXT FIELD  siefore
                     END IF

                     SELECT razon_social
                     INTO g_reg.desc_siefore  
                     FROM tab_siefore_local
                     WHERE codigo_siefore = g_reg.siefore
                     IF STATUS = NOTFOUND THEN
                       INITIALIZE g_reg.desc_siefore TO NULL
                     END IF

                     DISPLAY BY NAME g_reg.desc_siefore 

              AFTER FIELD cuenta

                    IF g_reg.cuenta IS NULL THEN
                        ERROR "Cuenta NO puede ser nula"
                        NEXT FIELD cuenta
                     END IF

              AFTER FIELD descripcion 

                    IF g_reg.descripcion IS NULL THEN
                        ERROR "Descripcion NO puede ser nula"
                        NEXT FIELD descripcion
                     END IF

              AFTER FIELD tipo 

                    IF g_reg.tipo IS NULL THEN
                        ERROR "Tipo NO puede ser nulo"
                        NEXT FIELD tipo
                     END IF

              AFTER FIELD identificador 

                    IF g_reg.identificador IS NULL THEN
                        ERROR "Identificador NO puede ser nulo"
                        NEXT FIELD identificador
                     END IF

                     SELECT "X"
                     FROM con_traductor 
                     WHERE proceso_cod     = g_reg.proceso_cod
                       AND transaccion_cod = g_reg.transaccion_cod
                       AND siefore         = g_reg.siefore
                       AND cuenta          = g_reg.cuenta
                       AND tipo            = g_reg.tipo
                       AND identificador   = g_reg.identificador 
                     IF STATUS <> NOTFOUND THEN
                        ERROR "Cuenta ya existe"
                        NEXT FIELD proceso_cod
                     END IF

                  AFTER FIELD analisis_cod


              ON KEY ( ESC )
                    IF g_reg.proceso_cod IS NULL THEN
                       ERROR "Codigo de Proceso NO puede ser nulo"
                       NEXT FIELD proceso_cod
                    END IF
   
                     SELECT "X"
                     FROM tab_proceso 
                     WHERE proceso_cod = g_reg.proceso_cod
                     IF STATUS = NOTFOUND THEN
                        ERROR "No existe Codigo de Proceso"
                        NEXT FIELD proceso_cod
                     END IF

                     IF g_reg.transaccion_cod IS NULL THEN
                        ERROR "Codigo de Transaccion NO puede ser nulo"
                        NEXT FIELD  transaccion_cod
                     END IF

                     SELECT "X"
                     FROM tab_transaccion
                     WHERE proceso_cod = g_reg.proceso_cod
                       AND transaccion_cod = g_reg.transaccion_cod
                     IF STATUS = NOTFOUND THEN
                        ERROR "No existe Transaccion"
                        NEXT FIELD proceso_cod 
                     END IF

                    IF g_reg.siefore IS NULL THEN
                        ERROR "Siefore NO puede ser nula"
                        NEXT FIELD  siefore
                     END IF

                    IF g_reg.cuenta IS NULL THEN
                        ERROR "Cuenta NO puede ser nula"
                        NEXT FIELD cuenta
                     END IF

                    IF g_reg.descripcion IS NULL THEN
                        ERROR "Descripcion NO puede ser nula"
                        NEXT FIELD descripcion
                     END IF

                    IF g_reg.tipo IS NULL THEN
                        ERROR "Tipo NO puede ser nulo"
                        NEXT FIELD tipo
                     END IF

                    IF g_reg.identificador IS NULL THEN
                        ERROR "Identificador NO puede ser nulo"
                        NEXT FIELD identificador
                     END IF

                     SELECT "X"
                     FROM con_traductor 
                     WHERE proceso_cod     = g_reg.proceso_cod
                       AND transaccion_cod = g_reg.transaccion_cod
                       AND siefore         = g_reg.siefore
                       AND cuenta          = g_reg.cuenta
                       AND tipo            = g_reg.tipo
                       AND identificador   = g_reg.identificador 
                     IF STATUS <> NOTFOUND THEN
                        ERROR "Cuenta ya existe"
                        NEXT FIELD proceso_cod
                     END IF

                     INSERT INTO con_traductor  VALUES(g_reg.proceso_cod,
                                                       g_reg.transaccion_cod, 
                                                       g_reg.siefore, 
                                                       g_reg.cuenta, 
                                                       g_reg.descripcion, 
                                                       g_reg.tipo, 
                                                       g_reg.identificador, 
                                                       g_reg.analisis_cod,
                                                       HOY,
                                                       usuario)
                     ERROR "REGISTRO INGRESADO"
                     SLEEP 2
                     ERROR ""
                     CALL Inicializa()
                     NEXT FIELD proceso_cod 
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
        END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
        LET pos = 2
        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "TABM1122" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                         CUENTAS CONTABLES                                     " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON proceso_cod, transaccion_cod, siefore, 
                     cuenta, descripcion, tipo, identificador, analisis_cod
                     FROM proceso_cod, transaccion_cod, siefore, cuenta,
                     descripcion, tipo,identificador, analisis_cod


              ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
                LET int_flag = FALSE
                EXIT CONSTRUCT
              ON KEY (control-c)
                LET int_flag = TRUE
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


        LET sel_where = "SELECT  proceso_cod, transaccion_cod, siefore, ' ', ", 
                         " cuenta, descripcion, tipo, identificador, " ,
                         " analisis_cod ",
                         "FROM con_traductor WHERE "
                         ,cla_where CLIPPED,
                         "ORDER BY 1,2,3,4,6,7 " CLIPPED
        LET sel_where = sel_where CLIPPED

           PREPARE query FROM sel_where

           DECLARE cursor_1 CURSOR FOR query

           LET pos = 1

           FOREACH cursor_1 INTO l_record[pos].*
        
              INITIALIZE l_record[pos].desc_siefore TO NULL 

              SELECT razon_social
              INTO l_record[pos].desc_siefore
              FROM tab_siefore_local
              where codigo_siefore = l_record[pos].siefore
               
              LET pos = pos + 1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL  SET_COUNT(pos-1)
              DISPLAY ARRAY l_record TO scr_1.*
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
                   ON KEY (INTERRUPT)
                       EXIT DISPLAY
              END DISPLAY
              CLOSE WINDOW ventana_2
           ELSE
              ERROR "ARCHIVO DE CUENTAS CONTABLES... VACIO"
              SLEEP 2
              ERROR ""
              CLOSE WINDOW ventana_2
           END IF
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
        LET pos = 2
        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "TABM1122" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Modifica                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Cuenta Contable a  modificar                 " AT 2,1
           DISPLAY "                        CUENTAS CONTABLES                                       " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON proceso_cod, transaccion_cod, siefore, cuenta,
                     descripcion, tipo,identificador, analisis_cod
                FROM proceso_cod,
                     transaccion_cod, siefore, cuenta,descripcion,
                     tipo,identificador, analisis_cod

              ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
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

        LET sel_where = "SELECT  proceso_cod, transaccion_cod, siefore, '', ",  
                         " cuenta, descripcion, tipo, identificador, " ,
                         " analisis_cod ",
                         "FROM con_traductor WHERE "
                         ,cla_where CLIPPED,
                         "ORDER BY 1,2,3,4,6,7 "

           PREPARE query1 FROM sel_where

           DECLARE cursor_2 CURSOR FOR query1

           LET pos = 1

           FOREACH cursor_2 INTO l_record[pos].*

              INITIALIZE l_record[pos].desc_siefore TO NULL 

              SELECT razon_social
              INTO l_record[pos].desc_siefore
              FROM tab_siefore_local
              where codigo_siefore = l_record[pos].siefore

              LET pos = pos + 1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL
           IF (pos-1) >= 1 THEN
              CALL  SET_COUNT(pos-1)
              ERROR ""
              DISPLAY ARRAY l_record TO scr_1.*
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                  ON KEY (control-m)
                     LET pos = ARR_CURR()
                     LET g_reg.proceso_cod     = l_record[pos].proceso_cod
                     LET g_reg.transaccion_cod = l_record[pos].transaccion_cod
                     LET g_reg.siefore         = l_record[pos].siefore
                     LET g_reg.desc_siefore    = l_record[pos].desc_siefore 
                     LET g_reg.cuenta          = l_record[pos].cuenta 
                     LET g_reg.descripcion     = l_record[pos].descripcion
                     LET g_reg.tipo            = l_record[pos].tipo
                     LET g_reg.identificador   = l_record[pos].identificador
                     LET g_reg.analisis_cod    = l_record[pos].analisis_cod
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
              ERROR "ARCHIVO DE CATALOGO DE CUENTAS ... VACIO"
              SLEEP 2
              ERROR ""
              CLOSE WINDOW ventana_2
              RETURN
           END IF

           DISPLAY "" AT 1,1
           DISPLAY "" AT 2,1
           DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

           INPUT BY NAME g_reg.* WITHOUT DEFAULTS

           BEFORE FIELD proceso_cod

           INITIALIZE l_record[pos].desc_siefore TO NULL 

           SELECT razon_social
           INTO l_record[pos].desc_siefore
           FROM tab_siefore_local
           WHERE codigo_siefore = l_record[pos].siefore
           NEXT FIELD descripcion 

           BEFORE FIELD transaccion_cod
           NEXT FIELD nescripcion

           BEFORE FIELD siefore 
           NEXT FIELD descripcion 

           BEFORE FIELD cuenta 
           NEXT FIELD descripcion 

           AFTER FIELD descripcion 

                 IF g_reg.descripcion IS NULL THEN
                     ERROR "Descripcion NO puede ser nula"
                     NEXT FIELD descripcion
                 END IF


                CALL Pregunta()

                IF aux_pausa MATCHES "[Ss]" THEN
                        UPDATE con_traductor SET
                               descripcion     = g_reg.descripcion,
                               factualiza      = HOY,
                               usuario         = usuario
                        WHERE  proceso_cod     = g_reg.proceso_cod
                          AND  transaccion_cod = g_reg.transaccion_cod
                          AND  siefore         = g_reg.siefore
                          AND  cuenta          = g_reg.cuenta    
                          AND  tipo            = g_reg.tipo
                          AND  identificador   = g_reg.identificador
                          AND  analisis_cod    = g_reg.analisis_cod
                        ERROR "DESCRIPCION MODIFICADA"
                        SLEEP 2
                        ERROR ""

                   CALL Inicializa()

                ELSE
                   ERROR "PROCESO DE MODIFICAR CANCELADO."
                   SLEEP 2
                   ERROR ""
                   CALL Inicializa()
                END IF
                EXIT INPUT
           ON KEY (INTERRUPT)
                CALL Inicializa()
                EXIT INPUT
           END INPUT
        ELSE
           ERROR "ARCHIVO DE CUENTAS CONTABLES... VACIO."
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
        LET pos = 2
        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "TABM1122" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Elimina                                             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Cta. Contable  a eliminar                   " AT 2,1
           DISPLAY "                         CUENTA CONTABLE                                      " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

           LET int_flag = FALSE

           CONSTRUCT cla_where ON proceso_cod,transaccion_cod,siefore,cuenta,                                     descripcion,tipo,identificador, analisis_cod
                            FROM  proceso_cod,transaccion_cod,siefore,cuenta,
                                  descripcion,tipo,identificador, analisis_cod


              ON KEY (control-m)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
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

        LET sel_where = "SELECT  proceso_cod, transaccion_cod, siefore, '', ",  
                         " cuenta, descripcion, tipo, identificador, " ,
                         " analisis_cod ",
                         "FROM con_traductor WHERE "
                         ,cla_where CLIPPED,
                         "ORDER BY 1,2,3,4,6,7 "


           PREPARE query2 FROM sel_where

           DECLARE cursor_3 CURSOR FOR query2

           LET pos = 1

           FOREACH cursor_3 INTO l_record[pos].*

              INITIALIZE l_record[pos].desc_siefore TO NULL 

              SELECT razon_social
              INTO l_record[pos].desc_siefore
              FROM tab_siefore_local
              where codigo_siefore = l_record[pos].siefore

              LET pos = pos + 1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL  SET_COUNT(pos-1)
              ERROR ""
              DISPLAY ARRAY l_record TO scr_1.*
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                  ON KEY (control-m)

                     LET pos = ARR_CURR()
                     SELECT razon_social
                     INTO l_record[pos].desc_siefore
                     FROM tab_siefore_local
                     WHERE codigo_siefore = l_record[pos].siefore

                     LET g_reg.proceso_cod     = l_record[pos].proceso_cod
                     LET g_reg.transaccion_cod = l_record[pos].transaccion_cod
                     LET g_reg.siefore         = l_record[pos].siefore
                     LET g_reg.desc_siefore    = l_record[pos].desc_siefore
                     LET g_reg.cuenta          = l_record[pos].cuenta 
                     LET g_reg.descripcion     = l_record[pos].descripcion
                     LET g_reg.tipo            = l_record[pos].tipo
                     LET g_reg.identificador   = l_record[pos].identificador
                     LET g_reg.analisis_cod    = l_record[pos].analisis_cod
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
              ERROR "ARCHIVO DE CUENTAS CONTABLES... VACIO"
              SLEEP 2
              ERROR ""
              CLOSE WINDOW ventana_2
              RETURN
           END IF

           DISPLAY "" AT 1,1
           DISPLAY "" AT 2,1
           DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
           DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

           DISPLAY BY NAME g_reg.*
              CALL Pregunta()

           IF aux_pausa MATCHES "[Ss]" THEN

           IF  g_reg.analisis_cod IS NULL THEN
              DELETE FROM con_traductor
                   WHERE  proceso_cod     = g_reg.proceso_cod
                     AND  transaccion_cod = g_reg.transaccion_cod
                     AND  siefore         = g_reg.siefore
                     AND  cuenta          = g_reg.cuenta    
                     AND  tipo            = g_reg.tipo
                     AND  identificador   = g_reg.identificador
                     AND  analisis_cod    IS NULL
           ELSE
              DELETE FROM con_traductor
                   WHERE  proceso_cod     = g_reg.proceso_cod
                     AND  transaccion_cod = g_reg.transaccion_cod
                     AND  siefore         = g_reg.siefore
                     AND  cuenta          = g_reg.cuenta    
                     AND  tipo            = g_reg.tipo
                     AND  identificador   = g_reg.identificador
                     AND  analisis_cod    = g_reg.analisis_cod
           END IF

              ERROR "REGISTRO ELIMINADO."
              SLEEP 2
              ERROR ""
           ELSE
              ERROR "PROCESO CANCELADO."
              SLEEP 2
              ERROR ""
           END IF

           CALL Inicializa()
        ELSE
           ERROR "ARCHIVO DE CUENTAS CONTABLES... VACIO."
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
        PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
