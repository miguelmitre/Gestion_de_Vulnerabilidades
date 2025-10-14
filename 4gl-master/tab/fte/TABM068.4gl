###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM068  => CATALOGO DE TIPO DE MONEDA                          #
#Fecha             => 11 DE JUNIO DE 2002                                 #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB                                                 #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE aux_pausa            CHAR(1)

   DEFINE g_reg  RECORD
          moneda_cod           SMALLINT,
          moneda_desc          CHAR(20),
          moneda_abrevia       CHAR(3)                               
   END RECORD

   DEFINE l_record ARRAY[1000] OF RECORD
          moneda_cod           SMALLINT,
          moneda_desc          CHAR(20),
          moneda_abrevia       CHAR(3)
   END RECORD

   DEFINE vmoneda_cod          SMALLINT,
          vmoneda_desc         CHAR(20)

   DEFINE pos                  SMALLINT,
          usuario              CHAR(8)

   DEFINE
        bandera               ,
        sw_1                  SMALLINT

   DEFINE 
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
                                                                   
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0681" ATTRIBUTE( BORDER)
   DISPLAY " TABM068                CATALOGO DE TIPO DE MONEDA                                         " AT 3,1 ATTRIBUTE(REVERSE)    
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "TIPO DE MONEDA "
      COMMAND "Agrega" "Agrega TIPO DE MONEDA"
         CALL agrega() #a
      COMMAND "Consulta" "Consulta TIPO DE MONEDA"
         CALL consulta() #c
      COMMAND "Modifica" "Modifica TIPO DE MONEDA"
         CALL modifica() #m
      COMMAND "Elimina" "Elimina TIPO DE MONEDA"
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

   LET g_reg.moneda_cod     = NULL
   LET g_reg.moneda_desc    = NULL
   LET g_reg.moneda_abrevia = NULL

   LET sw_1 = 0

   INPUT BY NAME g_reg.moneda_cod,
                 g_reg.moneda_desc,
                 g_reg.moneda_abrevia WITHOUT DEFAULTS

      AFTER FIELD moneda_cod
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD moneda_abrevia
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD moneda_desc
         END IF                                                                

         IF g_reg.moneda_cod IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD moneda_cod
         ELSE
            SELECT "X"
            FROM    tab_moneda      
            WHERE   moneda_cod = g_reg.moneda_cod    

            IF STATUS <> NOTFOUND THEN
               ERROR "   EL CODIGO YA EXISTE   "
               NEXT FIELD moneda_cod    
            END IF                                 
         END IF

      AFTER FIELD moneda_desc
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD moneda_cod
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD moneda_abrevia
         END IF                                                                

         IF g_reg.moneda_desc  IS NULL THEN
            ERROR "   DESCRIPCION NO PUEDE SER NULA   "
            NEXT FIELD moneda_desc
         END IF

      AFTER FIELD moneda_abrevia
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD moneda_desc
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD moneda_cod
         END IF                                                                

         IF g_reg.moneda_abrevia IS NULL THEN
            ERROR "   ABREVIATURA NO PUEDE SER NULA   "
            NEXT FIELD moneda_abrevia
         END IF  

      ON KEY ( ESC )

          IF g_reg.moneda_cod IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD moneda_cod
         END IF

         IF g_reg.moneda_desc  IS NULL THEN
            ERROR "   DESCRIPCION NO PUEDE SER NULA   "
            NEXT FIELD moneda_desc
         END IF

         IF g_reg.moneda_abrevia IS NULL THEN
            ERROR "   ABREVIATURA NO PUEDE SER NULA   "
            NEXT FIELD moneda_abrevia
         END IF                           

         INSERT INTO tab_moneda  VALUES (g_reg.moneda_cod      ,
                                               g_reg.moneda_desc     ,
                                               g_reg.moneda_abrevia  )

         ERROR "   REGISTRO INGRESADO   " SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()

         NEXT FIELD moneda_cod
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0682" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta            (Ctrol-p) Impresion             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                         
      DISPLAY "                              TIPO DE MONEDA                                    " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod FROM moneda_cod      
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
                                                                         
      LET sel_where = "SELECT moneda_cod,moneda_desc,moneda_abrevia FROM tab_moneda  WHERE ",

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
          ERROR "   REGISTRO DE TIPO DE MONEDA .... NO EXISTE   "
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0682" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "             Escoja con < ENTER > el tipo de moneda a modificar                     " AT 2,1     
      DISPLAY "                             TIPO DE MONEDA                                    " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod FROM moneda_cod     
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

      LET sel_where = "SELECT moneda_cod,moneda_desc,moneda_abrevia FROM tab_moneda WHERE ", 
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
               LET g_reg.moneda_cod     = l_record[pos].moneda_cod     
               LET vmoneda_cod          = l_record[pos].moneda_cod      
               LET g_reg.moneda_desc    = l_record[pos].moneda_desc
               LET vmoneda_desc         = l_record[pos].moneda_desc
               LET g_reg.moneda_abrevia = l_record[pos].moneda_abrevia
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
          ERROR "   REGISTRO DE TIPO DE MONEDA .... NO EXISTE   "
                                                                          
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN

      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_reg.moneda_cod,
                      g_reg.moneda_desc,
                      g_reg.moneda_abrevia


      INPUT BY NAME g_reg.moneda_desc,
                    g_reg.moneda_abrevia WITHOUT DEFAULTS

         AFTER FIELD moneda_desc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD moneda_abrevia
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD moneda_abrevia
            END IF

            IF g_reg.moneda_desc  IS NULL THEN
               ERROR "   DESCRIPCION NO PUEDE SER NULA   "
               NEXT FIELD moneda_desc
            END IF

         AFTER FIELD moneda_abrevia                                         
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD moneda_desc
            END IF
   
            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD moneda_desc
            END IF

            IF g_reg.moneda_abrevia IS NULL THEN
               ERROR "   ABREVIATURA NO PUEDE SER NULA   "
               NEXT FIELD moneda_abrevia
            END IF
                                                           
            WHILE TRUE

             PROMPT "Esta seguro S/N ? "
             FOR CHAR aux_pausa
                                                                     
             IF aux_pausa MATCHES "[sS]" THEN
                UPDATE tab_moneda
                SET moneda_desc    = g_reg.moneda_desc,
                    moneda_abrevia = g_reg.moneda_abrevia
                WHERE moneda_cod   = vmoneda_cod     
                AND   moneda_desc  = vmoneda_desc 

                IF SQLCA.SQLCODE != 0 then
                   ERROR "   ERROR EN LA ACTUALIZACION DE TIPO DE MONEDA   "
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
      ERROR "   ARCHIVO DE TIPO DE MONEDA VACIO   "
   END IF
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0682" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "             Escoja con < ENTER > el tipo de moneda a eliminar                   " AT 2,1  
      DISPLAY "                             TIPO DE MONEDA                                  " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod FROM moneda_cod     
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

      LET sel_where = "SELECT moneda_cod,moneda_desc,moneda_abrevia FROM tab_moneda WHERE ",  
                                                               
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
               LET g_reg.moneda_cod     = l_record[pos].moneda_cod     
               LET g_reg.moneda_desc    = l_record[pos].moneda_desc 
               LET g_reg.moneda_abrevia = l_record[pos].moneda_abrevia
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
         ERROR "   REGISTRO DE TIPO DE MONEDA .... NO EXISTE   "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF                                            
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)


      DISPLAY BY NAME g_reg.moneda_cod,
                      g_reg.moneda_desc,
                      g_reg.moneda_abrevia

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM tab_moneda
         WHERE moneda_cod   = g_reg.moneda_cod     
         AND   moneda_desc  = g_reg.moneda_desc 

         IF SQLCA.SQLCODE != 0 THEN
                                                                     
            ERROR "   ERROR EN AL ELIMINACION DE TIPO DE MONEDA   "
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
      ERROR "   REGISTRO DE TIPO DE MONEDA .... NO EXISTE   "
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
                  ".IMPTIPMONE",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tab_moneda TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.moneda_cod     = l_record[i].moneda_cod     
       LET g_reg.moneda_desc    = l_record[i].moneda_desc 
       LET g_reg.moneda_abrevia = l_record[i].moneda_abrevia

       IF g_reg.moneda_cod IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tab_moneda(g_reg.*)
   END FOR


   FINISH REPORT rpt_tab_moneda

   ERROR "   LISTADO GENERADO...   "
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_moneda (g_reg)

   DEFINE g_reg  RECORD
          moneda_cod           SMALLINT,
          moneda_desc          CHAR(20),
          moneda_abrevia       CHAR(3)            
   END RECORD               

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 02," TABM068 ",
               COLUMN 25,"CATALOGO DE TIPO DE MONEDA    ",
               COLUMN 67, TODAY  USING "dd/mm/yyyy"
         SKIP 2 LINE

         PRINT COLUMN 13,"Codigo",
               COLUMN 31,"Descripcion",
               COLUMN 59,"Abreviatura"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 11,g_reg.moneda_cod CLIPPED,
               COLUMN 29,g_reg.moneda_desc, 
               COLUMN 63,g_reg.moneda_abrevia

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
                                                                         
      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT                     
