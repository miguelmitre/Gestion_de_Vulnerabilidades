####################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                #
#Owner             => E.F.P.                                       #
#Programa TABM058  => Catalogo de Contabilidad de Transacciones    #
#Fecha             => 12/10/2001  .                                #
#Por               => Laura Eugenia Cortes Guzman                  #
#Sistema           => TAB.                                         #
#Actualizacion     => Eduardo Resendiz Medina  27 Mzo 2006         #
####################################################################
DATABASE safre_af
GLOBALS
    DEFINE g_param_dis    RECORD LIKE glo_parametro.*

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

    DEFINE g_reg, g_reg_1      RECORD 
           transaccion_cod INTEGER,
           descripcion_1   CHAR(80),
           descripcion_2   CHAR(80),
           descripcion_3   CHAR(80),
           descripcion_4   CHAR(80),
           descripcion_5   CHAR(80),
           descripcion_6   CHAR(80),
           descripcion_7   CHAR(80),
           descripcion_8   CHAR(80),
           proceso_cod     CHAR(05),
           subcuenta       CHAR(05)
    END RECORD
--->erm 27 Mzo 2006
    DEFINE g_agre         RECORD 
           transaccion_cod INTEGER,
           descripcion_1   CHAR(80),
           descripcion_2   CHAR(80),
           descripcion_3   CHAR(80),
           descripcion_4   CHAR(80),
           descripcion_5   CHAR(80),
           descripcion_6   CHAR(80),
           descripcion_7   CHAR(80),
           descripcion_8   CHAR(80),
           proceso_cod     CHAR(05),
           subcuenta       CHAR(05),
           subcuenta2      CHAR(05),
           subcuenta3      CHAR(05)
    END RECORD
---<erm 27 Mzo 2006
    DEFINE l_record_2      ARRAY[1] OF RECORD 
           transaccion_cod INTEGER,
           descripcion_1   CHAR(80),
           descripcion_2   CHAR(80),
           descripcion_3   CHAR(80),
           descripcion_4   CHAR(80),
           descripcion_5   CHAR(80),
           descripcion_6   CHAR(80),
           descripcion_7   CHAR(80),
           descripcion_8   CHAR(80),
           proceso_cod     CHAR(05),
           subcuenta       CHAR(05)
    END RECORD

    DEFINE l_record  ARRAY[1000] OF RECORD
           codigo             INTEGER,
           descripcion_1      CHAR(80)
    END RECORD

    DEFINE l_record1  ARRAY[1000] OF RECORD
           codigo             INTEGER,
           descripcion_1      CHAR(80),
           descripcion_2      CHAR(80),
           descripcion_3      CHAR(80),
           descripcion_4      CHAR(80),
           descripcion_5      CHAR(80),
           descripcion_6      CHAR(80),
           descripcion_7      CHAR(80),
           descripcion_8      CHAR(80),
           proceso_cod        CHAR(05),
           desc_proceso       CHAR(60),
           subcuenta          CHAR(05)
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
   INITIALIZE g_reg.*, g_reg_1.* TO NULL

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

         AFTER FIELD transaccion_cod
               IF g_reg.transaccion_cod IS NULL THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  transaccion_cod
               END IF

               SELECT "X" 
                    FROM tab_transaccion
                    WHERE transaccion_cod = g_reg.transaccion_cod

               IF STATUS <> NOTFOUND THEN
                  ERROR "Codigo ya Ingresado"
                  NEXT FIELD transaccion_cod
               END IF 

         BEFORE FIELD descripcion_1
               IF g_reg.transaccion_cod IS NULL OR
                  g_reg.transaccion_cod = 0 THEN
                  ERROR "Codigo NO puede ser nulo"
                  NEXT FIELD  transaccion_cod
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

         AFTER FIELD proceso_cod

         AFTER FIELD subcuenta

--->erm 27 Mzo 2006
         IF g_reg.subcuenta <> "" OR 
            g_reg.subcuenta IS NOT NULL THEN
               LET g_agre.subcuenta  = g_reg.subcuenta[1]
               LET g_agre.subcuenta2 = g_reg.subcuenta[3]
               LET g_agre.subcuenta3 = g_reg.subcuenta[5]
         END IF 
---<erm 27 Mzo 2006
         ON KEY ( ESC )
               IF g_reg.transaccion_cod IS NULL THEN
                  ERROR "Codigo NO puede ser NULO"
                  NEXT FIELD transaccion_cod
               END IF

               IF g_reg.descripcion_1 IS NULL THEN
                  ERROR "Descripcion_1 NO puede ser NULO"
                  NEXT FIELD descripcion_1
               END IF

               SELECT "X" 
                     FROM tab_transaccion
                     WHERE descripcion_1 = g_reg.descripcion_1

               IF STATUS <> NOTFOUND THEN
                   ERROR "Codigo Ya Ingresado"
                   NEXT FIELD transaccion_cod
               END IF 

               LET g_agre.transaccion_cod = g_reg.transaccion_cod
               LET g_agre.descripcion_1   = g_reg.descripcion_1
               let g_agre.descripcion_2   = g_reg.descripcion_2
               let g_agre.descripcion_3   = g_reg.descripcion_3
               let g_agre.descripcion_4   = g_reg.descripcion_4
               let g_agre.descripcion_5   = g_reg.descripcion_5
               let g_agre.descripcion_6   = g_reg.descripcion_6
               let g_agre.descripcion_7   = g_reg.descripcion_7
               let g_agre.descripcion_8   = g_reg.descripcion_8
               let g_agre.proceso_cod     = g_reg.proceso_cod

               INSERT INTO tab_transaccion VALUES ( g_agre.* ) 

               ERROR "REGISTRO INGRESADO" 
                     SLEEP 2
               ERROR ""

               CALL Inicializa()
               NEXT FIELD transaccion_cod

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
   DEFINE desc_proceso CHAR(60)
   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0582" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrl-p) Impresion            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                                                                               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

      LET int_flag = FALSE

      CONSTRUCT cla_where   ON transaccion_cod 
                          FROM transaccion_cod

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

      LET sel_where = "SELECT  transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8, ",
                             " proceso_cod, ",
                             " subcuenta,   ",
                             " '' ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,2 "
  
      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1
      FOREACH cursor_1 INTO l_record1[pos].*

      LET l_record1[pos].subcuenta = l_record1[pos].desc_proceso   ---erm 27 Mzo 2006

         SELECT descripcion
         INTO   l_record1[pos].desc_proceso
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod

         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
         DISPLAY ARRAY l_record1 TO scr_1.*
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
         ERROR "ARCHIVO DE TRANSACCIONES .... VACIO"
         SLEEP 2
         ERROR ""
         LET int_flag = TRUE      ---erm 27 Mzo 2006
         CLOSE WINDOW ventana_2   ---erm 27 Mzo 2006
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

      CONSTRUCT cla_where   ON transaccion_cod 
                          FROM transaccion_cod

         ON KEY (ESC)
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
--->erm 27 Mzo 2006
{      LET sel_where = "SELECT *,'' ",
                      "FROM tab_transaccion WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
}
      LET sel_where = "SELECT  transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8, ",
                             " proceso_cod, ",
                             " subcuenta,   ",
                             " '' ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,2 "
---<erm 27 Mzo 2006
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record1[pos].*

      LET l_record1[pos].subcuenta = l_record1[pos].desc_proceso   ---erm 27 Mzo 2006

         SELECT descripcion
         INTO   l_record1[pos].desc_proceso
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod

         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record1 TO scr_1.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET g_reg.transaccion_cod = l_record1[pos].codigo
                LET g_reg.descripcion_1   = l_record1[pos].descripcion_1
                LET g_reg.proceso_cod     = l_record1[pos].proceso_cod   ---27 Mzo 2006
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
          ERROR "REGISTROS DE TRANSACCIONES .... INEXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       INITIALIZE g_reg_1.* TO NULL
       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1
       DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

       SELECT * 
       INTO   g_reg_1.*
       FROM   tab_transaccion
       WHERE  transaccion_cod = g_reg.transaccion_cod
       AND    proceso_cod     = g_reg.proceso_cod    ---erm Mzo 2006
##la 

display g_reg_1.*

       DISPLAY g_reg_1.transaccion_cod TO transaccion_cod
       DISPLAY g_reg_1.descripcion_1   TO descripcion_1  
       DISPLAY g_reg_1.descripcion_2   TO descripcion_2  
       DISPLAY g_reg_1.descripcion_3   TO descripcion_3  
       DISPLAY g_reg_1.descripcion_4   TO descripcion_4  
       DISPLAY g_reg_1.descripcion_5   TO descripcion_5  
       DISPLAY g_reg_1.descripcion_6   TO descripcion_6  
       DISPLAY g_reg_1.descripcion_7   TO descripcion_7  
       DISPLAY g_reg_1.descripcion_8   TO descripcion_8  
       DISPLAY g_reg_1.proceso_cod     TO proceso_cod
       DISPLAY g_reg_1.subcuenta       TO subcuenta

prompt "enter" for enter
       INPUT BY NAME  g_reg_1.*

          BEFORE FIELD transaccion_cod
--                 NEXT FIELD descripcion     ---erm 27 Mzo 2006

          AFTER FIELD transaccion_cod
              IF g_reg_1.transaccion_cod IS NULL THEN
                 ERROR "Digite correctamente el Codigo "
                 NEXT FIELD  transaccion_cod
              END IF

          AFTER FIELD descripcion_1
 
             IF g_reg_1.descripcion_1 IS NULL THEN
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
                SET    descripcion_1 = g_reg_1.descripcion_1,
                       descripcion_2 = g_reg_1.descripcion_2,
                       descripcion_3 = g_reg_1.descripcion_3,
                       descripcion_4 = g_reg_1.descripcion_4,
                       descripcion_5 = g_reg_1.descripcion_5,
                       descripcion_6 = g_reg_1.descripcion_6,
                       descripcion_7 = g_reg_1.descripcion_7,
                       descripcion_8 = g_reg_1.descripcion_8
                WHERE  transaccion_cod   = g_reg_1.transaccion_cod

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

      CONSTRUCT cla_where   ON transaccion_cod
                          FROM transaccion_cod

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
--->erm 27 Mzo 2006
{      LET sel_where = "SELECT * ",
                      "FROM tab_transaccion WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
}
      LET sel_where = "SELECT  transaccion_cod, ",
                             " descripcion_1, ",
                             " descripcion_2, ",
                             " descripcion_3, ",
                             " descripcion_4, ",
                             " descripcion_5, ",
                             " descripcion_6, ",
                             " descripcion_7, ",
                             " descripcion_8, ",
                             " proceso_cod, ",
                             " subcuenta,   ",
                             " '' ",
                      " FROM tab_transaccion  WHERE ",cla_where CLIPPED,
                      " ORDER BY 1,2 "
---<erm 27 Mzo 2006
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
--      FOREACH cursor_3 INTO l_record[pos].*
--->erm 27 Mzo 2006
      FOREACH cursor_3 INTO l_record1[pos].*

      LET l_record1[pos].subcuenta = l_record1[pos].desc_proceso

        SELECT descripcion
         INTO   l_record1[pos].desc_proceso
         FROM   tab_proceso
         WHERE  proceso_cod = l_record1[pos].proceso_cod
---<erm 27 Mzo 2006

         LET pos = pos +1
      END FOREACH

--      INITIALIZE l_record[pos].* TO NULL
     INITIALIZE l_record1[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1) 
---         DISPLAY ARRAY l_record TO scr_1.* 
         DISPLAY ARRAY l_record1 TO scr_1.*         ---27 Mzo 2006

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.transaccion_cod   = l_record1[pos].codigo
               LET g_reg.descripcion_1 = l_record1[pos].descripcion_1
               LET g_reg.proceso_cod     = l_record1[pos].proceso_cod   ---erm 27 Mzo 2006
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

--      DISPLAY BY NAME  g_reg.*

--->erm 27 Mzo 2006
       SELECT * 
       INTO   g_reg_1.*
       FROM   tab_transaccion
       WHERE  transaccion_cod = g_reg.transaccion_cod
       AND    proceso_cod     = g_reg.proceso_cod    ---erm Mzo 2006
##la 

display g_reg_1.*

       DISPLAY g_reg_1.transaccion_cod TO transaccion_cod
       DISPLAY g_reg_1.descripcion_1   TO descripcion_1  
       DISPLAY g_reg_1.descripcion_2   TO descripcion_2  
       DISPLAY g_reg_1.descripcion_3   TO descripcion_3  
       DISPLAY g_reg_1.descripcion_4   TO descripcion_4  
       DISPLAY g_reg_1.descripcion_5   TO descripcion_5  
       DISPLAY g_reg_1.descripcion_6   TO descripcion_6  
       DISPLAY g_reg_1.descripcion_7   TO descripcion_7  
       DISPLAY g_reg_1.descripcion_8   TO descripcion_8  
       DISPLAY g_reg_1.proceso_cod     TO proceso_cod
       DISPLAY g_reg_1.subcuenta       TO subcuenta
---<erm 27 Mzo 2006

         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            DELETE FROM tab_transaccion
                   WHERE transaccion_cod = g_reg.transaccion_cod
                   AND   proceso_cod     = g_reg.proceso_cod    ---erm 27 Mzo 2006

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

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",seg_usuario CLIPPED,
                 ".CON_TRAN",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabTransaccion TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.transaccion_cod   = l_record[i].codigo
       LET g_reg.descripcion_1 = l_record[i].descripcion_1
   
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
REPORT rpt_tabTransaccion(g_reg1)
    DEFINE g_reg1     RECORD 
        transaccion_cod       INTEGER,
        descripcion_1         CHAR(80),
        descripcion_2         CHAR(80),
        descripcion_3         CHAR(80),
        descripcion_4         CHAR(80),
        descripcion_5         CHAR(80),
        descripcion_6         CHAR(80),
        descripcion_7         CHAR(80),
        descripcion_8         CHAR(80),
        proceso_cod           CHAR(05),
        subcuenta             CHAR(05)
    END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM058 ",
               COLUMN 19," LISTADO DE CONTABILIDAD DE TRANSACCIONES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 10,"CODIGO ",
               COLUMN 30,"DESCRIPCION "
         SKIP 1 LINE
      ON EVERY ROW

         PRINT COLUMN 12,g_reg1.transaccion_cod USING "#&&",
               COLUMN 30,g_reg1.descripcion_1
         IF g_reg1.descripcion_2 IS NULL THEN
            PRINT
         ELSE
             PRINT COLUMN 30,g_reg1.descripcion_2
             PRINT COLUMN 30,g_reg1.descripcion_3
             PRINT COLUMN 30,g_reg1.descripcion_4
             PRINT COLUMN 30,g_reg1.descripcion_5
             PRINT COLUMN 30,g_reg1.descripcion_6
             PRINT COLUMN 30,g_reg1.descripcion_7
             PRINT COLUMN 30,g_reg1.descripcion_8
         END IF
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
