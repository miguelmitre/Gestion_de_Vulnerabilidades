####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa PROB004  => Catalogo de Resultado                        #
#Fecha Elaboracion => 22 de Agosto del 2008                        #
#Elaborado por     => Isabel Fonseca Frias                         #
#Sistema           => PRO.                                         #
####################################################################
DATABASE safre_af
GLOBALS
    DEFINE g_param_dis      RECORD LIKE glo_parametro.*

    DEFINE 
        aux_pausa      CHAR(1),
        sw_1           SMALLINT,
        hoy            DATE,
        sel_where      CHAR(300),
        cla_where      CHAR(300),
        g_impre        CHAR(300),
        g_lista        CHAR(300),
        seg_usuario    CHAR(08),
        pos            SMALLINT

    DEFINE g_reg      RECORD 
        cod_result    CHAR(01),
        desc_result   CHAR(60)
    END RECORD

    DEFINE l_record  ARRAY[10] OF RECORD
        codigo       CHAR(05),
        desc_result    CHAR(80)
    END RECORD
END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
           INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT
        CALL STARTLOG("PROB004.log")
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
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "PROB0041" ATTRIBUTE( BORDER)
   DISPLAY " PROB004               CATALOGO DE RESULTADOS DE ",
           "EXAMEN                             " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
   
   MENU "CATALOGO DE EXAMEN "
      COMMAND "Agrega" "Agrega Resultado"
                       CALL Agrega()
      COMMAND "Consulta" "Consulta Resultado"
                       CALL Consulta()
      COMMAND "Modifica" "Modifica Resultado"
                       CALL Modifica()
      COMMAND "Elimina" "Elimina Resultado"
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
   LET g_reg.desc_result = NULL
   LET sw_1 = 0

   INPUT BY NAME  g_reg.*

         AFTER FIELD cod_result
               IF g_reg.cod_result IS  NULL  THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD cod_result 
               END IF

               SELECT "X" 
               FROM   pro_result_examen
               WHERE  cod_result = g_reg.cod_result

               IF STATUS <> NOTFOUND THEN
                  ERROR "Codigo ya Ingresado"
                  NEXT FIELD cod_result
               END IF 

         BEFORE FIELD desc_result
               IF g_reg.cod_result IS NULL THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  cod_result
               END IF 

         AFTER FIELD desc_result
               IF g_reg.desc_result IS NULL THEN
                  ERROR "Descripcion NO puede ser nula"
                  NEXT FIELD  desc_result
               END IF 

               SELECT "X" 
               FROM   pro_result_examen
               WHERE  desc_result = g_reg.desc_result

               IF STATUS <> NOTFOUND THEN
                   ERROR "El Codigo ya fue Ingresado"
                   NEXT FIELD cod_result
               END IF 

         ON KEY ( ESC )
               IF g_reg.cod_result IS NULL THEN
                  ERROR "Codigo NO puede ser NULO"
                  NEXT FIELD cod_result
               END IF

               IF g_reg.desc_result IS NULL THEN
                  ERROR "Descripcion NO puede ser NULO"
                  NEXT FIELD desc_result
               END IF

               SELECT "X" 
               FROM   pro_result_examen
               WHERE  desc_result = g_reg.desc_result

               IF STATUS <> NOTFOUND THEN
                   ERROR "Codigo Ya Ingresado"
                   NEXT FIELD cod_result
               END IF 

               INSERT INTO pro_result_examen VALUES 
                           ( g_reg.*,TODAY,seg_usuario ) 

               ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
               ERROR ""

               CALL Inicializa()
               NEXT FIELD cod_result

         ON KEY (INTERRUPT,CONTROL-C)
               CALL Inicializa()
               EXIT INPUT
   END INPUT
END FUNCTION
################################################################################
FUNCTION Consulta()
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "PROB0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            ",
              "(Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                              ",
              "                                 " 
              AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON cod_result 
                          FROM cod_result

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

     LET sel_where = "SELECT * FROM pro_result_examen WHERE ",cla_where CLIPPED,
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
         ERROR "TABLA DE ORIGEN .... VACIO"
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "PROB0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            ",
              "(Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                 Escoja con < ENTER > el registro a ",
              "modificar                  " AT 2,1
      DISPLAY "                                        ",
              "                                       " 
              AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON cod_result 
                          FROM cod_result

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (INTERRUPT,CONTROL-C)
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

     LET sel_where = "SELECT * FROM pro_result_examen WHERE ",cla_where CLIPPED,
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
                LET g_reg.cod_result = l_record[pos].codigo
                LET g_reg.desc_result = l_record[pos].desc_result
                EXIT DISPLAY

             ON KEY (INTERRUPT,CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTROS DE ORIGEN .... INEXISTE"
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

          BEFORE FIELD cod_result
                 NEXT FIELD desc_result

          AFTER FIELD cod_result
              IF g_reg.cod_result IS NULL THEN
                 ERROR "Digite correctamente el Codigo "
                 NEXT FIELD  cod_result
              END IF

          AFTER FIELD desc_result
 
             IF g_reg.desc_result IS NULL THEN
                ERROR "La desc_tipo no puede ser Nula "
                NEXT FIELD  desc_tipo
             END IF 
  
             CALL Pregunta()
         
             IF aux_pausa MATCHES "[Ss]" THEN
                UPDATE pro_result_examen 
                SET    desc_result  = g_reg.desc_result
                WHERE  cod_result  = g_reg.cod_result

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

          ON KEY ( INTERRUPT,CONTROL-C )
             CALL Inicializa()
             EXIT INPUT
       END INPUT
    ELSE
       ERROR "ARCHIVO DE ORIGEN.... VACIO"
    END IF
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "PROB0042" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            ",
              "(Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                  Escoja con < ENTER > el registro a ",
              "eliminar                  " AT 2,1
      DISPLAY "                                    ",
              "                                           " 
              AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON cod_result
                          FROM cod_result

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 2
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (INTERRUPT,CONTROL-C)
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

     LET sel_where = "SELECT * FROM pro_result_examen WHERE ",cla_where CLIPPED,
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
               LET g_reg.cod_result = l_record[pos].codigo
               LET g_reg.desc_result   = l_record[pos].desc_result
               EXIT DISPLAY

            ON KEY (INTERRUPT,CONTROL-C)
               ERROR "Debe seleccionar un registro"
               LET pos = ARR_CURR()

    END DISPLAY
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

      DISPLAY BY NAME  g_reg.*
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE
            FROM   pro_result_examen
            WHERE  cod_result = g_reg.cod_result

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
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".RESULT",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_taborigen TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.cod_result  = l_record[i].codigo
       LET g_reg.desc_result = l_record[i].desc_result
   
       IF g_reg.cod_result IS NULL THEN
          EXIT FOR
       END IF
  
       OUTPUT TO REPORT rpt_taborigen(g_reg.*)
   END FOR
  
   FINISH REPORT rpt_taborigen

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   --LET g_lista = "vi ",g_impre
   RUN g_lista
END FUNCTION
#####################################################################
REPORT rpt_taborigen(g_reg)
   DEFINE g_reg      RECORD 
               cod_result           CHAR(03),
               desc_result          CHAR(70)
          END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," PROB004 ",
               COLUMN 27," LISTADO DE RESULTADO DE EXAMEN ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 10,"CODIGO ",
               COLUMN 30,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW

         PRINT COLUMN 12,g_reg.cod_result USING "#&&&&&",
               COLUMN 30,g_reg.desc_result
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
