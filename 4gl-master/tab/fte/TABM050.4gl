####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa TABM050  => Catalogo de LAY-OUT                          #
#Fecha             =>                                              #
#Por               => Laura Eugenia Cortes Guzman                  #
#Sistema           => TAB.                                         #
####################################################################
DATABASE safre_af
GLOBALS
         DEFINE g_param_dis   RECORD LIKE dis_parametro.*

         DEFINE aux_pausa            CHAR(1),
                sw_1                 SMALLINT,
                usuario              CHAR(8),
                hoy                  DATE,
                pos                  INTEGER,
                sel_where            CHAR(300),
                cla_where            CHAR(300),
                g_impre              CHAR(300),
                g_lista              CHAR(300)

         DEFINE g_reg      RECORD 
                layout_cod           smallint,
                layout_des           char(60),
                desde                char(20),
                hasta                char(20),
                periodo              integer ,
                programa             char(30),
                modulo_cod           char(03),
                tab_cza              char(20),
                tab_det              char(20),
                tab_sum              char(20),
                arch_cza             char(20),
                arch_det             char(20),
                arch_sum             char(20),
                nom_arch             char(20),
                origen_cod           smallint
         END RECORD
 
         DEFINE l_record2  ARRAY[1000] OF RECORD
                layout_cod           smallint,
                modulo_cod           char(03),
                programa             char(30),
                layout_des           char(60)
         END RECORD

         DEFINE l_record   ARRAY[1000] OF RECORD
                layout_cod           smallint,
                layout_des           char(60),
                desde                char(20),
                hasta                char(20),
                periodo              integer ,
                programa             char(30),
                tab_cza              char(20),
                tab_det              char(20),
                tab_sum              char(20),
                arch_cza             char(20),
                arch_det             char(20),
                arch_sum             char(20),
                nom_arch             char(20),
                origen_cod           smallint,
                modulo_cod           char(03)
         END RECORD

         DEFINE l_reg      ARRAY[1000] OF RECORD
                campo_cod       SMALLINT, 
                pos_ini         SMALLINT, 
                pos_fin         SMALLINT
         END RECORD
END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
           INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
        CALL inicio()
        CALL proceso()
END MAIN
#####################################################################
FUNCTION inicio()
   SELECT USER,*
   INTO   usuario
   FROM   dis_parametro

   SELECT ruta_spool
   INTO   g_param_dis.ruta_spool
   FROM   dis_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TABM0501" ATTRIBUTE(BORDER)
   DISPLAY " TABM0501                                                                     " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
   
   MENU "LAY-OUT  "
      COMMAND "Agrega" "Agrega "
          CALL Agrega()
      COMMAND "Consulta" "Consulta "
          CALL Consulta()
      COMMAND "Modifica" "Modifica "
          CALL Modifica()
      COMMAND "Elimina" "Elimina "
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
 
   DEFINE vpos_fin       SMALLINT,
          vcampo_cod     SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega        (Ctrl-C) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

        LET g_reg.layout_cod = NULL

   INPUT BY NAME  g_reg.*

         AFTER FIELD layout_cod   
              IF g_reg.layout_cod IS NULL OR
                 g_reg.layout_cod = 0     THEN
                 ERROR "El codigo del layout NO puede ser nulo O Cero"
                 NEXT FIELD  layout_cod   
              END IF
              SELECT "K" FROM tab_layout
                     WHERE layout_cod = g_reg.layout_cod
              IF STATUS != NOTFOUND THEN
                 ERROR "Este codigo ya existe, digite otro"
                 NEXT FIELD layout_cod
              END IF

         AFTER FIELD layout_des
              IF g_reg.layout_des IS NULL OR
                 g_reg.layout_des =  " "  THEN
                 ERROR "La descripcion no puede ser nula"
                 NEXT FIELD  layout_des
              END IF 

         AFTER FIELD desde

         AFTER FIELD hasta

         AFTER FIELD periodo
              IF g_reg.periodo IS NULL THEN
                 ERROR "El periodo NO puede ser nulo "
                 NEXT FIELD  periodo
              END IF 

         AFTER FIELD programa
              IF g_reg.programa IS NULL OR
                 g_reg.programa  = " "  THEN
                 ERROR "El programa NO puede ser nulo "
                 NEXT FIELD  programa
              END IF 

         AFTER FIELD modulo_cod
              IF g_reg.modulo_cod = " " THEN
                 ERROR "No puede dejar en blanco o nulo el Modulo al que pertenece"
                 NEXT FIELD modulo_cod
              END IF

         AFTER FIELD tab_cza
              IF g_reg.tab_cza = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD tab_cza
              END IF

         AFTER FIELD tab_det
              IF g_reg.tab_det = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD tab_det
              END IF

         AFTER FIELD tab_sum
              IF g_reg.tab_sum = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD tab_sum
              END IF

         AFTER FIELD arch_cza
              IF g_reg.arch_cza = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD arch_cza
              END IF

         AFTER FIELD arch_det
              IF g_reg.arch_det = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD arch_det
              END IF

         AFTER FIELD arch_sum
              IF g_reg.arch_sum = " " THEN
                 ERROR "No deje la primera posicion en blanco"
                 NEXT FIELD arch_sum
              END IF

         AFTER FIELD nom_arch
              IF g_reg.nom_arch IS NULL OR
                 g_reg.nom_arch  = " "  THEN
                 ERROR "El programa NO puede ser nulo "
                 NEXT FIELD  nom_arch
              END IF 


         AFTER FIELD origen_cod


         ON KEY ( ESC )
               INSERT INTO tab_layout 
                    VALUES (g_reg.layout_cod,
                            g_reg.layout_des,
                            g_reg.desde,
                            g_reg.hasta,
                            g_reg.periodo,
                            g_reg.programa,
                            g_reg.tab_cza,
                            g_reg.tab_det,
                            g_reg.tab_sum,
                            g_reg.arch_cza,
                            g_reg.arch_det,
                            g_reg.arch_sum,
                            g_reg.nom_arch,
                            g_reg.origen_cod,
                            g_reg.modulo_cod)

               ERROR "REGISTRO INGRESADO"
                         SLEEP 2
               ERROR ""
               CALL Inicializa()
               NEXT FIELD layout_cod     

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0502" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta           (Ctrl-p) Imprimir            (Ctrl-C) Salir        " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON   layout_cod,
                               modulo_cod,
                               programa,
                               layout_des 
                               FROM layout_cod,
                                    modulo_cod,
                                    programa,
                                    layout_des 
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

      LET sel_where = "SELECT layout_cod, modulo_cod, programa, layout_des ",
                      " FROM tab_layout WHERE ",
                            cla_where CLIPPED,
                      "ORDER BY 1 "

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

         LET pos = 1

      FOREACH cursor_1 INTO l_record2[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""   
         DISPLAY ARRAY l_record2 TO scr_1.*
                ON KEY (CONTROL-P)
                     ERROR "PROCESANDO INFORMACION..."
--           CALL impresion(pos)

                ON KEY (CONTROL-C)
                     EXIT DISPLAY

                ON KEY (INTERRUPT)
                     EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE ... VACIO"
              SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
        DEFINE vpos_fin      SMALLINT,
               vlong         SMALLINT,
               vvlong        SMALLINT,
               vvpos_ini     SMALLINT,
               vvpos_fin     SMALLINT

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0502" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "          Escoja con <ENTER> el                    a modificar                " AT 2,1
      DISPLAY "                                                                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON layout_cod,
                             modulo_cod,
                             programa,
                             layout_des 
                             FROM layout_cod,
                                  modulo_cod,
                                  programa,
                                  layout_des 
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

      LET sel_where = "SELECT layout_cod, modulo_cod, programa, layout_des ",
                      " FROM tab_layout WHERE ",
                            cla_where CLIPPED,
                      "ORDER BY 1 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

         LET pos = 1

      FOREACH cursor_2 INTO l_record2[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""   
         DISPLAY ARRAY l_record2 TO scr_1.*
                 ON KEY (CONTROL-M)
                    LET pos = ARR_CURR()
                    SELECT a.layout_cod,  a.layout_des,  a.desde,
                           a.hasta     ,  a.periodo   ,  a.programa,
                           a.modulo_cod,  a.tab_cza   ,  a.tab_det,
                           a.tab_sum   ,  a.arch_cza  ,  a.arch_det,
                           a.arch_sum  ,  a.nom_arch  ,  a.origen_cod
                           INTO g_reg.*
                           FROM tab_layout a
                                WHERE a.layout_cod = l_record2[pos].layout_cod 

                    EXIT DISPLAY

                 ON KEY (CONTROL-C)
                    ERROR "Debe elegir un registro."
                    LET pos = ARR_CURR()

                 ON KEY (INTERRUPT)
                    ERROR "Debe elegir un registro."
                    LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE ... VACIO"
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
              BEFORE FIELD layout_cod
                    NEXT FIELD layout_des

              AFTER FIELD layout_des
                    IF g_reg.layout_des IS NULL THEN
                       ERROR "Descripcion del Lay-Out NO puede ser nulo"
                       NEXT FIELD  layout_des
                    END IF 

              AFTER FIELD desde

              AFTER FIELD hasta

              AFTER FIELD periodo
                   IF g_reg.periodo IS NULL THEN
                      ERROR "El periodo NO puede ser nulo "
                      NEXT FIELD  periodo
                   END IF 

              AFTER FIELD programa
                   IF g_reg.programa IS NULL OR
                      g_reg.programa  = " "  THEN
                      ERROR "El programa NO puede ser nulo "
                      NEXT FIELD  programa
                   END IF 

              AFTER FIELD modulo_cod
                   IF g_reg.modulo_cod = " " THEN
                      ERROR "No puede dejar en blanco o nulo el Modulo al que pertenece"
                      NEXT FIELD modulo_cod
                   END IF

              AFTER FIELD tab_cza
                   IF g_reg.tab_cza = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD tab_cza
                   END IF

              AFTER FIELD tab_det
                   IF g_reg.tab_det = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD tab_det
                   END IF

              AFTER FIELD tab_sum
                   IF g_reg.tab_sum = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD tab_sum
                   END IF

              AFTER FIELD arch_cza
                   IF g_reg.arch_cza = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD arch_cza
                   END IF

              AFTER FIELD arch_det
                   IF g_reg.arch_det = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD arch_det
                   END IF

              AFTER FIELD arch_sum
                   IF g_reg.arch_sum = " " THEN
                      ERROR "No deje la primera posicion en blanco"
                      NEXT FIELD arch_sum
                   END IF

              AFTER FIELD nom_arch
                   IF g_reg.nom_arch IS NULL OR
                      g_reg.nom_arch  = " "  THEN
                      ERROR "El programa NO puede ser nulo "
                      NEXT FIELD  nom_arch
                   END IF 


              AFTER FIELD origen_cod

              CALL Pregunta()

              IF aux_pausa MATCHES "[Ss]" THEN
                 UPDATE tab_layout SET
                             layout_des   =  g_reg.layout_des,
                             desde        =  g_reg.desde,
                             hasta        =  g_reg.hasta,
                             periodo      =  g_reg.periodo,
                             programa     =  g_reg.programa,
                             tab_cza      =  g_reg.tab_cza,
                             tab_det      =  g_reg.tab_det,
                             tab_sum      =  g_reg.tab_sum,
                             arch_cza     =  g_reg.arch_cza,
                             arch_det     =  g_reg.arch_det,
                             arch_sum     =  g_reg.arch_sum,
                             nom_arch     =  g_reg.nom_arch,
                             origen_cod   =  g_reg.origen_cod,
                             modulo_cod   =  g_reg.modulo_cod
                        WHERE  layout_cod =  g_reg.layout_cod   
        
                 ERROR "REGISTRO MODIFICADO"
                 SLEEP 2
                 ERROR ""

                 CALL Inicializa()
              ELSE
                  ERROR "PROCESO DE MODIFICAR CANCELADO."
                  SLEEP 2
                  ERROR ""
              END IF

              EXIT INPUT

      ON KEY (CONTROL-C)
         CALL Inicializa()
         EXIT INPUT

      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT INPUT
      END INPUT
   ELSE
      ERROR "ARCHIVO DE ... VACIO."
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0502" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                        (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "         Escoja con <ENTER>                       a eliminar                  " AT 2,1
      DISPLAY "                                                                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON layout_cod,
                             modulo_cod,
                             programa,
                             layout_des 
                             FROM layout_cod,
                                  modulo_cod,
                                  programa,
                                  layout_des 
         ON KEY (CONTROL-M)
      ERROR "PROCESANDO INFORMACION..."
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

      LET sel_where = "SELECT layout_cod, modulo_cod, programa, layout_des ",
                      " FROM tab_layout WHERE ",
                            cla_where CLIPPED,
                      "ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

         LET pos = 1

      FOREACH cursor_3 INTO l_record2[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
            ERROR ""   
            DISPLAY ARRAY l_record2 TO scr_1.*
   
                ON KEY (CONTROL-M)
                   LET pos = ARR_CURR()
                    SELECT a.layout_cod,  a.layout_des,  a.desde,
                           a.hasta     ,  a.periodo   ,  a.programa,
                           a.modulo_cod,  a.tab_cza   ,  a.tab_det,
                           a.tab_sum   ,  a.arch_cza  ,  a.arch_det,
                           a.arch_sum  ,  a.nom_arch  ,  a.origen_cod
                           INTO g_reg.*
                           FROM tab_layout a
                                WHERE a.layout_cod = l_record2[pos].layout_cod 

                   EXIT DISPLAY
   
                ON KEY (CONTROL-C)
                   ERROR "Debe elegir un registro."
                   LET pos = ARR_CURR()

                ON KEY (INTERRUPT)
                   ERROR "Debe elegir un registro."
                   LET pos = ARR_CURR()

         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE ... VACIO"
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

         DELETE FROM tab_layout
         WHERE layout_cod    = g_reg.layout_cod   

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
      ERROR "ARCHIVO DE STATUS DE LAY-OUT... VACIO."
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
{
FUNCTION impresion(pos)
   DEFINE i, pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,".IMPTSTAT",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_pro_status_interno To g_impre

   FOR i = 1 TO (pos+1)
      LET g_reg.status_interno    = l_record[i].codigo
      LET g_reg.desc_status_corta = l_record[i].descripcion

      IF g_reg.status_interno    IS NULL THEN
         EXIT FOR
      END IF

      OUTPUT TO REPORT rpt_pro_status_interno(g_reg.*)
   END FOR

   FINISH REPORT rpt_pro_status_interno

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_pro_status_interno(g_reg)
         DEFINE g_reg      RECORD 
      status_interno   SMALLINT,
      desc_status_corta     CHAR(40)
    END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM019 ",
               COLUMN 19," LISTADO DE CATALOGO DE STATUS DE PROMOTORES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"CODIGO",
               COLUMN 20,"DESCRIPCION STATUS"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 10,g_reg.status_interno ,
               COLUMN 25,g_reg.desc_status_corta
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
    PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
}
