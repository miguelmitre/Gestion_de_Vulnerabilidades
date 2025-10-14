################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Programa PROB005  => CATALOGO DE SEDES PARA EXAMEN                            #
#Fecha Elaboracion => 07 de Noviembre del 2006                                 #
#Elaborado por     => Laura Eugenia Cortes Guzman                              #
#Fecha Ult.Modific.=> 07 de Noviembre del 2006                                 #
#Modificado por    => Laura Eugenia Cortes Guzman                              #
#Sistema           => PRO.                                                     #
################################################################################
DATABASE safre_af
GLOBALS

         DEFINE aux_pausa         CHAR(1),
                sw_1              SMALLINT,
                sino              SMALLINT,
                usuario           CHAR(8),
                HOY               DATE,
                pos               INTEGER,
                sel_where         CHAR(300),
                cla_where         CHAR(300)

         DEFINE g_reg             RECORD
                cod_sede          SMALLINT,
                desc_sede         CHAR(60),
                edo_sede          SMALLINT, 
                desc_edo          CHAR(40)
         END RECORD

         DEFINE l_record        ARRAY[30] OF RECORD
                cod_sede          SMALLINT,
                desc_sede         CHAR(60),
                edo_sede          SMALLINT, 
                desc_edo          CHAR(40)
         END RECORD,

         desc_edo                 CHAR(40)
END GLOBALS
################################################################################
MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP,
                ACCEPT KEY control-o

        DEFER INTERRUPT
        CALL STARTLOG("PROB005.log")
        CALL inicio()
        CALL proceso()
END MAIN
################################################################################
FUNCTION inicio()
        SELECT USER,*
        INTO   usuario
        FROM   glo_parametro

END FUNCTION
################################################################################
FUNCTION proceso()
        LET HOY = TODAY
        OPEN WINDOW ventana_1 AT 3,3 WITH FORM "PROB0051" ATTRIBUTE( BORDER)
        DISPLAY " PROB005                 CATALOGO SEDES DE ",
                "EXAMEN                              " 
                AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

        MENU "CATALOGO SEDES"
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
              AFTER FIELD cod_sede 
                    IF g_reg.cod_sede IS NULL THEN
                       ERROR "Codigo de Proceso NO puede ser nulo"
                       NEXT FIELD cod_sede
                    END IF
   
                     SELECT "X"
                     FROM   pro_sede_examen 
                     WHERE cod_sede = g_reg.cod_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        ERROR "Ya se encuentra este codigo"
                        NEXT FIELD cod_sede
                     END IF

              AFTER FIELD desc_sede  
                     IF g_reg.desc_sede IS NULL OR
                        g_reg.desc_sede[1,3] = "   " THEN 
                        ERROR "Digite Correctamente la Descripcion del Codigo"
                        NEXT FIELD  desc_sede
                     END IF

                     SELECT "X"
                     FROM   pro_sede_examen 
                     WHERE desc_sede = g_reg.desc_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        ERROR "Ya se encuentra esta Descripcion, ",
                              "intente nuevamente"
                        NEXT FIELD desc_sede
                     END IF

              AFTER FIELD edo_sede
                 IF g_reg.edo_sede IS NULL THEN
                     CALL selec_edo() RETURNING g_reg.edo_sede, g_reg.desc_edo
                     IF g_reg.edo_sede IS NULL OR 
                        g_reg.edo_sede = 0 THEN
                        ERROR "El Estado NO puede ser nula"
                        NEXT FIELD edo_sede
                     ELSE
                        DISPLAY g_reg.edo_sede TO edo_sede
                        DISPLAY g_reg.desc_edo TO desc_edo
                     END IF
                 ELSE
                     SELECT estad_desc INTO g_reg.desc_edo FROM tab_estado
                     WHERE  estad_cod = g_reg.edo_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        DISPLAY g_reg.desc_edo TO desc_edo
                     ELSE
                        ERROR "El Estado seleccionado no Existe "
                        NEXT FIELD edo_sede
                     END IF
                 END IF

              ON KEY ( ESC )
                    IF g_reg.cod_sede IS NULL THEN
                       ERROR "Codigo de Proceso NO puede ser nulo"
                       NEXT FIELD cod_sede
                    END IF
   
                     SELECT "X"
                     FROM   pro_sede_examen 
                     WHERE cod_sede = g_reg.cod_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        ERROR "Ya se encuentra este codigo"
                        NEXT FIELD cod_sede
                     END IF

                     IF g_reg.desc_sede IS NULL OR
                        g_reg.desc_sede[1,3] = "   " THEN 
                        ERROR "Digite Correctamente la Descripcion del Codigo"
                        NEXT FIELD  desc_sede
                     END IF

                     SELECT "X"
                     FROM   pro_sede_examen 
                     WHERE desc_sede = g_reg.desc_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        ERROR "Ya se encuentra esta Descripcion, ",
                              "intente nuevamente"
                        NEXT FIELD desc_sede
                     END IF

                 IF g_reg.edo_sede IS NULL THEN
                     CALL selec_edo() RETURNING g_reg.edo_sede, desc_edo
                     IF g_reg.edo_sede IS NULL OR 
                        g_reg.edo_sede = 0 THEN
                        ERROR "El Estado NO puede ser nula"
                        NEXT FIELD edo_sede
                     ELSE
                        DISPLAY g_reg.edo_sede TO edo_sede
                        DISPLAY g_reg.desc_edo TO desc_edo
                     END IF
                 ELSE
                     SELECT estad_desc INTO g_reg.desc_edo FROM tab_estado
                     WHERE  estad_cod = g_reg.edo_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        DISPLAY g_reg.desc_edo TO desc_edo
                     ELSE
                        ERROR "El Estado seleccionado no Existe "
                        NEXT FIELD edo_sede
                     END IF
                 END IF

                 INSERT INTO pro_sede_examen  
                 VALUES(g_reg.cod_sede,
                        g_reg.desc_sede, 
                        g_reg.edo_sede, 
                        HOY,
                        usuario)

                 ERROR "REGISTRO INGRESADO"
                 SLEEP 2
                 ERROR ""
                 INITIALIZE g_reg.desc_edo TO NULL
                 CALL Inicializa()
                 NEXT FIELD cod_sede 

              ON KEY ( CONTROL-C, INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
        END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
        LET pos = 2
        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "PROB0052" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Consulta              ",
                   "                              (Ctrl-C) Salir    " 
                   AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "                          SEDES DE EXAMEN",
                   "                                      " 
                   AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON cod_sede, desc_sede, edo_sede 
                     FROM cod_sede, desc_sede, edo_sede

              ON KEY (CONTROL-M)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
                LET int_flag = FALSE
                EXIT CONSTRUCT
              ON KEY (INTERRUPT,CONTROL-C)
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


        LET sel_where = "SELECT  a.cod_sede, a.desc_sede, a.edo_sede,  ", 
                         " b.estad_desc " ,
                         "FROM pro_sede_examen a, tab_estado b WHERE "
                         ,cla_where CLIPPED,
                         "AND a.edo_sede = b.estad_cod ",
                         "ORDER BY 1 " CLIPPED
        LET sel_where = sel_where CLIPPED

           PREPARE query FROM sel_where

           DECLARE cursor_1 CURSOR FOR query

           LET pos = 1

           FOREACH cursor_1 INTO l_record[pos].*
        
              LET pos = pos + 1
           END FOREACH

           INITIALIZE l_record[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL  SET_COUNT(pos-1)
              DISPLAY ARRAY l_record TO scr_1.*
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
                   ON KEY (CONTROL-C,INTERRUPT)
                       EXIT DISPLAY
              END DISPLAY
              CLOSE WINDOW ventana_2
           ELSE
              ERROR "ARCHIVO DE SEDE... VACIO"
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
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "PROB0052" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Modifica              ",
                   "                              (Ctrl-C) Salir    " 
                   AT 1,1 ATTRIBUTE(REVERSE,BOLD)

           DISPLAY "               Seleccione con <ENTER> la ",
                   "Sede                                  " AT 2,1

           DISPLAY "                      SEDE DEL EXAMEN",
                   "                                          " 
                   AT 3,1 ATTRIBUTE(REVERSE,BOLD)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON cod_sede, desc_sede, edo_sede
                FROM cod_sede, desc_sede, edo_sede

              ON KEY (CONTROL-M)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
                LET int_flag = FALSE
                EXIT CONSTRUCT
              ON KEY (CONTROL-C, INTERRUPT)
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

        LET sel_where = "SELECT  a.cod_sede, a.desc_sede, a.edo_sede, ",  
                         " b.estad_desc " ,
                         "FROM pro_sede_examen a, tab_estado b WHERE "
                         ,cla_where CLIPPED,
                         "AND a.edo_sede = b.estad_cod ",
                         "ORDER BY 1 "

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
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_reg.cod_sede      = l_record[pos].cod_sede
                     LET g_reg.desc_sede     = l_record[pos].desc_sede
                     LET g_reg.edo_sede      = l_record[pos].edo_sede
                     LET g_reg.desc_edo      = l_record[pos].desc_edo 
                     EXIT DISPLAY
                  ON KEY (CONTROL-C, INTERRUPT)
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
              ERROR "ARCHIVO DE SEDES DE EXAMEN ... VACIO"
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

           BEFORE FIELD cod_sede 
               NEXT FIELD desc_sede

##           INITIALIZE l_record[pos].desc_edo TO NULL 

           AFTER FIELD desc_sede 

                 IF g_reg.desc_sede IS NULL THEN
                     ERROR "Descripcion NO puede ser nula"
                     NEXT FIELD desc_sede
                 END IF

           AFTER FIELD edo_sede 

                 IF g_reg.edo_sede IS NULL THEN
                     CALL selec_edo() RETURNING g_reg.edo_sede, g_reg.desc_edo
                     IF g_reg.edo_sede IS NULL OR 
                        g_reg.edo_sede = 0 THEN
                        ERROR "El Estado NO puede ser nula"
                        NEXT FIELD edo_sede
                     ELSE
                        DISPLAY g_reg.edo_sede TO edo_sede
                        DISPLAY g_reg.desc_edo TO desc_edo
                     END IF
                 ELSE
                     SELECT estad_desc INTO g_reg.desc_edo FROM tab_estado
                     WHERE  estad_cod = g_reg.edo_sede
                     IF SQLCA.SQLCODE = 0 THEN
                        DISPLAY g_reg.desc_edo TO desc_edo
                     ELSE
                        ERROR "El Estado seleccionado no Existe "
                        NEXT FIELD edo_sede
                     END IF
                 END IF

                CALL Pregunta()

                IF aux_pausa MATCHES "[Ss]" THEN
                        UPDATE pro_sede_examen 
                        SET    desc_sede       = g_reg.desc_sede,
                               edo_sede        = g_reg.edo_sede,
                               factualiza      = HOY,
                               usuario         = usuario
                        WHERE  cod_sede     = g_reg.cod_sede

                        ERROR "MODIFICACION SATISFACTORIA"
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
           ON KEY (CONTROL-C, INTERRUPT)
                CALL Inicializa()
                EXIT INPUT
           END INPUT
        ELSE
           ERROR "ARCHIVO DE SEDES DE EXAMEN... VACIO."
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
        LET pos = 2
        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW ventana_2 AT 6,2 WITH FORM "PROB0052" ATTRIBUTE( BORDER)
           DISPLAY " (Enter) Elimina              ",
                   "                               (Ctrl-C) Salir    " 
                   AT 1,1 ATTRIBUTE(REVERSE,BOLD)

           DISPLAY "               Seleccione <ENTER> la Sede",
                   "                                     " AT 2,1

           DISPLAY "                         SEDES DE EXAMEN",
                   "                                       " 
                   AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

           LET int_flag = FALSE

           CONSTRUCT cla_where ON cod_sede,desc_sede,edo_sede
                            FROM  cod_sede,desc_sede,edo_sede


              ON KEY (CONTROL-M)
                ERROR "PROCESANDO INFORMACION..."
                SLEEP 2
                ERROR ""
                LET int_flag = FALSE
                EXIT CONSTRUCT

              ON KEY (CONTROL-C, INTERRUPT)
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

        LET sel_where = "SELECT  a.cod_sede, a.desc_sede, a.edo_sede, ",  
                         " b.estad_desc " ,
                         "FROM pro_sede_examen a, tab_estado b WHERE "
                         ,cla_where CLIPPED,
                         "AND a.edo_sede = b.estad_cod ",
                         "ORDER BY 1 "

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
              ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                  ON KEY (CONTROL-M)

                     LET pos = ARR_CURR()
                     LET g_reg.cod_sede        = l_record[pos].cod_sede
                     LET g_reg.desc_sede       = l_record[pos].desc_sede
                     LET g_reg.edo_sede        = l_record[pos].edo_sede
                     LET g_reg.desc_edo        = l_record[pos].desc_edo
                     EXIT DISPLAY
                  ON KEY (CONTROL-C, INTERRUPT)
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
              ERROR "ARCHIVO DE SEDES DE EXAMEN... VACIO"
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

              DELETE FROM pro_sede_examen
                   WHERE  cod_sede     = g_reg.cod_sede
                     AND  desc_sede    = g_reg.desc_sede
                     AND  edo_sede     = g_reg.edo_sede

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
           ERROR "ARCHIVO DE SEDES DE EXAMEN... VACIO."
        END IF
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
        PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION selec_edo()
   DEFINE aux_val          SMALLINT,
          x_x              CHAR(100),
          x_buscar         CHAR(30),

          l_reg ARRAY[1000] OF RECORD
              codigo       INTEGER,
              descripcion  CHAR(50)
          END RECORD,

          pos              SMALLINT

   OPEN WINDOW vent_3 AT 05,12 WITH FORM "PROB0053" ATTRIBUTE(BORDER)
   DISPLAY "                ENTIDADES   FEDERATIVAS               "
           AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
             IF x_buscar IS NULL THEN
                ERROR "Descripcion a Buscar NO puede ser nulo"
                NEXT FIELD x_buscar
             ELSE
                EXIT INPUT
             END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT estad_cod,estad_desc FROM tab_estado ",
                   " WHERE estad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED

         PREPARE curg2 FROM x_x
         DECLARE cur_g2 CURSOR FOR curg2
            LET pos = 1
         FOREACH cur_g2 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO ESTADOS..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( CONTROL-C, INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_3
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
