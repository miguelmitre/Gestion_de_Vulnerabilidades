####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa TABM056  => Catalogo de DIAGNOSTICOS DE RETIRO DE SOLICIT#
#Fecha             => 24/08/2001  .                                #
#Por               => Laura Eugenia Cortes Guzman                  #
#Sistema           => TAB.                                         #
####################################################################
DATABASE safre_af
GLOBALS
        DEFINE g_param_dis      RECORD LIKE glo_parametro.*

   DEFINE aux_pausa      CHAR(1),
               sw_1                     SMALLINT,
               hoy                      DATE,
               sel_where                CHAR(300),
               cla_where                CHAR(300),
               g_impre                  CHAR(300),
               g_lista                  CHAR(300),
               seg_usuario              CHAR(08),
               pos                      SMALLINT

        DEFINE g_reg      RECORD 
                       codigo_diag      CHAR(02),
                       desc_diag        CHAR(70)
               END RECORD

        DEFINE l_record  ARRAY[1000] OF RECORD
               codigo                   CHAR(02),
               descripcion              CHAR(70)
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
   SELECT USER,*
          INTO seg_usuario
          FROM glo_parametro

   SELECT ruta_spool
          INTO   g_param_dis.ruta_spool
          FROM   glo_parametro
END FUNCTION
#####################################################################
FUNCTION proceso()
   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0561" ATTRIBUTE( BORDER)
   DISPLAY " TABM056           CATALOGO DE DIAGNOSTICOS DE RETIRO                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
   
   MENU "CATALOGO DIAGNOSTICO "
      COMMAND "Agrega" "Agrega Diagnostico"
                       CALL Agrega()
      COMMAND "Consulta" "Consulta Diagnostico"
                       CALL Consulta()
      COMMAND "Modifica" "Modifica Diagnostico"
                       CALL Modifica()
      COMMAND "Elimina" "Elimina Diagnostico"
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
   LET g_reg.desc_diag = NULL
   LET sw_1 = 0

   INPUT BY NAME  g_reg.*

         AFTER FIELD codigo_diag
               IF g_reg.codigo_diag IS NULL THEN
                  ERROR "Codigo de Ciudad NO puede ser nulo"
                  NEXT FIELD  codigo_diag
               END IF

               SELECT "X" 
                    FROM tab_ret_diag_sol
                    WHERE codigo_diag = g_reg.codigo_diag

               IF STATUS <> NOTFOUND THEN
                  ERROR "Codigo ya Ingresado"
                  NEXT FIELD codigo_diag
               END IF 

         BEFORE FIELD desc_diag
               IF g_reg.codigo_diag IS NULL OR  g_reg.codigo_diag = 0 THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  codigo_diag
               END IF 

         AFTER FIELD desc_diag
               IF g_reg.desc_diag IS NULL THEN
                  ERROR "Descripcion NO puede ser nula"
                  NEXT FIELD  desc_diag
               END IF 

               SELECT "X" 
                     FROM tab_ret_diag_sol
                     WHERE desc_diag = g_reg.desc_diag

               IF STATUS <> NOTFOUND THEN
                   ERROR "El Diagnostico ya fue Ingresado"
                   NEXT FIELD codigo_diag
               END IF 

         ON KEY ( ESC )
               IF g_reg.codigo_diag IS NULL THEN
                  ERROR "Codigo de Ciudad NO puede ser NULO"
                  NEXT FIELD codigo_diag
               END IF

               IF g_reg.desc_diag IS NULL THEN
                  ERROR "Descripcion de Ciudad NO puede ser NULO"
                  NEXT FIELD desc_diag
               END IF

               SELECT "X" 
                     FROM tab_ret_diag_sol
                     WHERE desc_diag = g_reg.desc_diag

               IF STATUS <> NOTFOUND THEN
                   ERROR "Sexo Ya Ingresado"
                   NEXT FIELD codigo_diag
               END IF 

               INSERT INTO tab_ret_diag_sol VALUES ( g_reg.* ) 

               ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
               ERROR ""

               CALL Inicializa()
               NEXT FIELD codigo_diag

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0562" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON codigo_diag 
                          FROM codigo_diag

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

      LET sel_where = "SELECT * FROM tab_ret_diag_sol WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-P)
               ERROR "PROCESANDO INFORMACION ..."
               CALL impresion(pos)

            ON KEY (CONTROL-C)
               EXIT DISPLAY

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE DIAGNOSTICOS .... VACIO"
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0562" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > el registro a modificar                  " AT 2,1
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON codigo_diag 
                          FROM codigo_diag

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

      LET sel_where = "SELECT * FROM tab_ret_diag_sol WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
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
                LET g_reg.codigo_diag = l_record[pos].codigo
                LET g_reg.desc_diag = l_record[pos].descripcion
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE DIAGNOSTICOS .... INEXISTE"
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

          BEFORE FIELD codigo_diag
                 NEXT FIELD desc_diag

          AFTER FIELD codigo_diag
              IF g_reg.codigo_diag IS NULL THEN
                 ERROR "Digite correctamente el Codigo "
                 NEXT FIELD  codigo_diag
              END IF

          AFTER FIELD desc_diag
 
             IF g_reg.desc_diag IS NULL THEN
                ERROR "La descripcion no puede ser Nula "
                NEXT FIELD  desc_diag
             END IF 
  
             CALL Pregunta()
         
             IF aux_pausa MATCHES "[Ss]" THEN
                UPDATE tab_ret_diag_sol SET desc_diag   = g_reg.desc_diag
                WHERE codigo_diag  = g_reg.codigo_diag

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
       ERROR "ARCHIVO DE DIAGNOSTICO.... VACIO"
    END IF
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0562" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con < ENTER > el registro a eliminar                  " AT 2,1
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON codigo_diag 
                          FROM codigo_diag

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

      LET sel_where = "SELECT * FROM tab_ret_diag_sol WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
         DISPLAY ARRAY l_record TO scr_1.* 

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.codigo_diag = l_record[pos].codigo
               LET g_reg.desc_diag   = l_record[pos].descripcion
               EXIT DISPLAY

            ON KEY (CONTROL-C)
               ERROR "Debe seleccionar un registro"
               LET pos = ARR_CURR()

            ON KEY (INTERRUPT)
               ERROR "Debe seleccionar un registro"
               LET pos = ARR_CURR()
    END DISPLAY
    CLOSE WINDOW ventana_2
      ELSE
         ERROR "INEXISTENTE REGISTRO DE DIAGNOSTICO "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_ret_diag_sol
                   WHERE codigo_diag = g_reg.codigo_diag

            ERROR "REGISTRO ELIMINADO" 
            SLEEP 2
         ELSE
            ERROR "PROCESO CANCELADA" 
            SLEEP 2
         END IF

         ERROR ""
         CALL Inicializa()
   ELSE
      ERROR "VACIO ARCHIVO DE DIAGNOSTICO "
   END IF
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".DIAGRETSOL",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabdiag TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.codigo_diag  = l_record[i].codigo
       LET g_reg.desc_diag = l_record[i].descripcion
   
       IF g_reg.codigo_diag IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_tabdiag(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_tabdiag

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_tabdiag(g_reg)
   DEFINE g_reg      RECORD 
               codigo_diag           CHAR(02),
               desc_diag             CHAR(70)
          END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM056 ",
               COLUMN 24," LISTADO DE CATALOGO DE DIAGNOSTICOS ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 10,"CODIGO ",
               COLUMN 30,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW

         PRINT COLUMN 12,g_reg.codigo_diag USING "#&&",
               COLUMN 30,g_reg.desc_diag
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
