###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM067  => CATALOGO DE TIPO DE OPERACION DE LAVADO DE DINERO   #   
#Fecha             => 11 DE JUNIO DE 2002                                 #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB                                                 #
###########################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE aux_pausa            CHAR(1)

   DEFINE g_reg  RECORD
          lav_operacion        SMALLINT,
          reporte_desc         CHAR(20)
   END RECORD

   DEFINE l_record ARRAY[1000] OF RECORD
          lav_operacion        SMALLINT,
          reporte_desc         CHAR(20)
   END RECORD

   DEFINE vlav_operacion       SMALLINT,
          vreporte_desc        CHAR(20)

   DEFINE pos   SMALLINT

   DEFINE
        bandera               ,
        sw_1                  SMALLINT

   DEFINE usuario              CHAR(08),
          HOY                  DATE,
          cla_where            CHAR(200),
          sel_where            CHAR(200),
          g_lista              CHAR(300),
          g_impre              CHAR(300),
          hora                 CHAR(08)
END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL inic()

   LET HOY = TODAY
   LET bandera = FALSE
                                                                   
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0671" ATTRIBUTE( BORDER)
   DISPLAY " TABM067           TIPO DE OPERACION DE LAVADO DE DINERO                                     " AT 3,1 ATTRIBUTE(REVERSE)    
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "TIPO DE OPERACION "
      COMMAND "Agrega" "Agrega TIPO DE OPERACION"
         CALL agrega() #a
      COMMAND "Consulta" "Consulta TIPO DE OPERACION"
         CALL consulta() #c
      COMMAND "Modifica" "Modifica TIPO DE OPERACION"
         CALL modifica() #m
      COMMAND "Elimina" "Elimina TIPO DE OPERACION"
         CALL elimina() #e
      COMMAND KEY(S) "Salir" "Salir del Programa"

      EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION inic()
#--------------
   SELECT USER,*
      INTO   usuario
      FROM   glo_parametro

   SELECT ruta_spool
      INTO   g_param_dis.ruta_spool
      FROM   glo_parametro
END FUNCTION
################################################################################
FUNCTION Inicializa()
#i-------------------
   LET sw_1 = 0
   INITIALIZE g_reg.* TO NULL
END FUNCTION
################################################################################
FUNCTION agrega()
#a---------------
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                               " AT 1,1 ATTRIBUTE(BOLD)        
                                                                             
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_reg.lav_operacion  = NULL
   LET g_reg.reporte_desc   = NULL

   LET sw_1 = 0

   INPUT BY NAME g_reg.lav_operacion,
                 g_reg.reporte_desc WITHOUT DEFAULTS

      AFTER FIELD lav_operacion
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD reporte_desc
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD reporte_desc
         END IF                                                                

         IF g_reg.lav_operacion IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD lav_operacion
         ELSE 
            SELECT "X"
            FROM    tab_tipo_reporte
            WHERE   lav_operacion = g_reg.lav_operacion

                IF STATUS <> NOTFOUND THEN
               ERROR "   EL CODIGO YA EXISTE   "
               NEXT FIELD lav_operacion                                                    END IF                                
         END IF

      AFTER FIELD reporte_desc
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD lav_operacion
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD lav_operacion
         END IF                                                                

         IF g_reg.reporte_desc  IS NULL THEN
            ERROR "   DESCRIPCION NO PUEDE SER NULA   "
            NEXT FIELD reporte_desc
         END IF

      ON KEY ( ESC )

          IF g_reg.lav_operacion IS NULL THEN
            ERROR "   CODGIO NO PUEDE SER NULO   "
            NEXT FIELD lav_operacion
         END IF

         IF g_reg.reporte_desc  IS NULL THEN
            ERROR "   DESCRIPCION NO PIEDE SER NULA   "
            NEXT FIELD reporte_desc
         END IF


         INSERT INTO tab_tipo_reporte  VALUES (g_reg.lav_operacion    ,
                                               g_reg.reporte_desc     ,
                                               usuario                ,
                                               TODAY                  )

         ERROR "   REGISTRO INGRESADO   " SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()

         NEXT FIELD lav_operacion
         ON KEY (INTERRUPT)
            CLEAR FORM
            CALL Inicializa()
            EXIT INPUT
         ON KEY (control-c)
            CLEAR FORM
            CALL Inicializa()
            EXIT INPUT
   END INPUT
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION consulta()
#c-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0672" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion           (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                         
      DISPLAY "                  TIPO DE OPERACION DE LAVADO DE DINERO                            " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_operacion FROM lav_operacion 
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "   BUSQUEDA CANCELADA...   "
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF
                                                                         
      LET sel_where = "SELECT lav_operacion,reporte_desc FROM tab_tipo_reporte  WHERE ",

                       cla_where CLIPPED ,"ORDER BY 1 "

      PREPARE query1 FROM sel_where

      DECLARE cursor_1 CURSOR FOR query1

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-p)
               ERROR "   PROCESANDO IMPRESION...   "
               CALL Impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            ON KEY (control-c)

               EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "   REGISTRO DE TIPO DE OPERACION .... NO EXISTE   "
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
       END IF
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION modifica()
#m-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)                             
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0672" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "           Escoja con < ENTER > el tipo de operacion a modificar                     " AT 2,1     
      DISPLAY "                  TIPO DE OPERACION DE LAVADO DE DINERO                        " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_operacion FROM lav_operacion
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE

            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "   BUSQUEDA CANCELADA...   "
                                                                       
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT lav_operacion,reporte_desc FROM tab_tipo_reporte WHERE ", 
                       cla_where CLIPPED , "ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
                                                                    
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.lav_operacion   = l_record[pos].lav_operacion
               LET vlav_operacion        = l_record[pos].lav_operacion 
               LET g_reg.reporte_desc    = l_record[pos].reporte_desc
               LET vreporte_desc         = l_record[pos].reporte_desc
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "   USTED DEBE ESCIJER UN REGISTRO   "
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
               LET pos = ARR_CURR()
          END DISPLAY
          CLOSE WINDOW ventana_2
      ELSE
          ERROR "   REGISTRO DE TIPO DE OPERACION .... NO EXISTE   "
                                                                          
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN

      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_reg.lav_operacion,
                      g_reg.reporte_desc

      INPUT BY NAME  g_reg.reporte_desc WITHOUT DEFAULTS

         AFTER FIELD reporte_desc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD reporte_desc   
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD reporte_desc   
            END IF                                

            IF g_reg.reporte_desc  IS NULL THEN
               ERROR "   DESCRIPCION NO PUEDE SER NULA   "
               NEXT FIELD reporte_desc
            END IF
                                                                      
            WHILE TRUE

             PROMPT "Esta seguro S/N ? "
             FOR CHAR aux_pausa
                                                                     
             IF aux_pausa MATCHES "[sS]" THEN
                UPDATE tab_tipo_reporte
                SET reporte_desc    = g_reg.reporte_desc,
                    usuario         = usuario,
                    factualiza      = TODAY
                WHERE lav_operacion = vlav_operacion
                AND   reporte_desc  = vreporte_desc

                IF SQLCA.SQLCODE != 0 then
                   ERROR "   ERROR EN LA ACTUALIZACION DE TIPO DE CAMBIO   "
                   ATTRIBUTE (reverse)
                   SLEEP 2
                   ERROR " "
                ELSE
                   ERROR "   REGISTRO MODIFICADO   "
                   SLEEP 1
                   ERROR ""
                   CLEAR FORM
                                                                         
                   EXIT WHILE
                END IF
             ELSE
                ERROR "   PROCESO DE MODIFICAR,CANCELADO   "
                SLEEP 2
                ERROR " "
                INITIALIZE g_reg.* TO NULL
                CLEAR FORM
                EXIT WHILE
             END IF

             CALL Inicializa()
             END WHILE
             EXIT INPUT
             ON KEY ( INTERRUPT )
                CALL Inicializa()
                CLEAR FORM
                EXIT INPUT
             ON KEY (control-c )
                CALL Inicializa()
                CLEAR FORM
                                                                       
                EXIT INPUT
      END INPUT
   ELSE
      ERROR "   ARCHIVO DE TIPO DE OPERACION VACIO   "
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0672" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "           Escoja con < ENTER > el tipo de operacion a eliminar                   " AT 2,1  
      DISPLAY "                   TIPO DE OPERACION DE LAVADO DE DINERO                      " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_operacion FROM lav_operacion
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "   BUSQUEDA CANCELADA...   "
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT lav_operacion,reporte_desc FROM tab_tipo_reporte WHERE ",  
                                                               
                       cla_where CLIPPED , "ORDER BY 1 "

      PREPARE query3 FROM sel_where
      DECLARE cursor_3 CURSOR FOR query3

      LET pos = 1
      FOREACH cursor_3 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.lav_operacion  = l_record[pos].lav_operacion
               LET g_reg.reporte_desc   = l_record[pos].reporte_desc
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "   REGISTRO DE TIPO DE OPERACION .... NO EXISTE   "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF                                            
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)


      DISPLAY BY NAME g_reg.lav_operacion,
                      g_reg.reporte_desc

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM tab_tipo_reporte
         WHERE lav_operacion = g_reg.lav_operacion
         AND   reporte_desc  = g_reg.reporte_desc

         IF SQLCA.SQLCODE != 0 THEN
                                                                     
            ERROR "   ERROR EN LA ELIMINACION DE TIPO DE CAMBIO   "
            ATTRIBUTE (reverse)
         ELSE
            ERROR "   REGISTRO ELIMINADO   "
            SLEEP 2
            CLEAR FORM
         END IF
      ELSE
         ERROR "   ELIMINAR CANCELADO   "
         SLEEP 2
         CLEAR FORM
      END IF

      ERROR ""
      CALL Inicializa()
   ELSE
      ERROR "   REGISTRO DE TIPO DE OPERACION .... NO EXISTE   "
   END IF
   ERROR ""
END FUNCTION
################################################################################
                                                                               
FUNCTION Pregunta()
#p-----------------
   PROMPT "Esta seguro S/N ? "
      FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Impresion(pos)
#----------------------
   DEFINE i,pos INTEGER

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                  ".IMPTIPOPER",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tab_reporte_desc TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.lav_operacion    = l_record[i].lav_operacion
       LET g_reg.reporte_desc     = l_record[i].reporte_desc

       IF g_reg.lav_operacion IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tab_reporte_desc(g_reg.*)
   END FOR


   FINISH REPORT rpt_tab_reporte_desc

   ERROR "   LISTADO GENERADO...   "
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_reporte_desc (g_reg)

   DEFINE g_reg  RECORD
          lav_operacion        SMALLINT,
          reporte_desc         CHAR(20)
   END RECORD               

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM067 ",
               COLUMN 22,"TIPO DE OPERACION DE LAVADO DE DINERO",
               COLUMN 67, TODAY  USING "dd/mm/yyyy"
         SKIP 2 LINE

         PRINT COLUMN 18,"Codigo",
               COLUMN 37,"Descripcion"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 17,g_reg.lav_operacion CLIPPED,
               COLUMN 37,g_reg.reporte_desc 

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
                                                                         
      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT                     
