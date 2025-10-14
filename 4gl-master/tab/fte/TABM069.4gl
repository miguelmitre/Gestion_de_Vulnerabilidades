###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM069  => CATALOGO DE CONDICIONES DE LAVADO DE DINERO         #
#Fecha             => 13 DE JUNIO DE 2002                                 #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB                                                 #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE aux_pausa            CHAR(1)

   DEFINE g_reg  RECORD 
          lav_reporte          SMALLINT,
          condicion_cod        SMALLINT,
          condicion_desc       CHAR(20),
          moneda_cod           SMALLINT,
          monto_desde          DECIMAL(16,6),
          monto_hasta          DECIMAL(16,6)    
   END RECORD                                     

   DEFINE l_record ARRAY[1000] OF RECORD
          lav_reporte          SMALLINT,
          condicion_cod        SMALLINT,
          condicion_desc       CHAR(20),
          moneda_cod           SMALLINT,
          monto_desde          DECIMAL(16,6),
          monto_hasta          DECIMAL(16,6)
   END RECORD

   DEFINE l_record2 ARRAY[1000] OF RECORD     
          moneda_cod           SMALLINT,
          moneda_desc          CHAR(20),
          moneda_abrevia       CHAR(3)        
   END RECORD

   DEFINE l_record3 ARRAY[1000] OF RECORD     
          lav_operacion        SMALLINT,
          reporte_desc         CHAR(20)
   END RECORD   

   DEFINE vlav_reporte         SMALLINT,
          vreporte_desc        CHAR(20), 
          vmoneda_cod          SMALLINT,
          vmoneda_desc         CHAR(20),
          vmoneda_abrevia      CHAR(3) ,
          vcondicion_cod       DATE,
          vcondicion_desc      DECIMAL(16,6)
 
   DEFINE pos                  SMALLINT,
          vcodigo              SMALLINT,
          vcodigo2             SMALLINT

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

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0691" ATTRIBUTE( BORDER)
   DISPLAY " TABM069        CATALOGO DE CONDICIONES DE LAVADO DE DINERO                                        " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CONDICION DE LAVADO "
      COMMAND "Agrega" "Agrega CONDICION DE LAVADO"
         CALL agrega() #a
      COMMAND "Consulta" "Consulta CONDICION DE LAVADO"
         CALL consulta() #c
      COMMAND "Modifica" "Modifica CONDICION DE LAVADO"
         CALL modifica() #m
      COMMAND "Elimina" "Elimina CONDICION DE LAVADO"
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

   LET g_reg.lav_reporte    = NULL
   LET g_reg.condicion_cod  = NULL
   LET g_reg.condicion_desc = NULL
   LET g_reg.moneda_cod     = NULL
   LET g_reg.monto_desde    = NULL
   LET g_reg.monto_hasta    = NULL 

   LET sw_1 = 0

   INPUT BY NAME g_reg.lav_reporte,
                 g_reg.condicion_cod,
                 g_reg.condicion_desc,
                 g_reg.moneda_cod,
                 g_reg.monto_desde,
                 g_reg.monto_hasta WITHOUT DEFAULTS   

      AFTER FIELD lav_reporte
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD monto_hasta
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD condicion_cod
         END IF                                                                

         IF g_reg.lav_reporte IS NULL THEN
            OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0693" ATTRIBUTE (BORDER)
            DISPLAY " (ENTER) Consulta  " AT 1,1
            DISPLAY "                    TIPO DE OPERACION DE LAVADO DE DINERO                             " AT 2,1 ATTRIBUTE(REVERSE)   

            LET int_flag = FALSE

            CONSTRUCT cla_where ON lav_operacion FROM lav_operacion
               ON KEY (control-m)
                  LET int_flag = FALSE
                  EXIT CONSTRUCT
               ON KEY (control-c)
                  IF int_flag = TRUE THEN
                     EXIT CONSTRUCT
                  END IF
            END CONSTRUCT

            IF int_flag = TRUE THEN                                   
               LET int_flag = FALSE
               ERROR "   BUSQUEDA CANCELADA...   "
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               NEXT FIELD lav_reporte 
            END IF

            LET sel_where = "SELECT           ",
                            " lav_operacion  ,",
                            " reporte_desc    ",
                            " FROM tab_tipo_reporte WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query1 FROM sel_where

            DECLARE cursor_1 CURSOR FOR query1                    

            LET pos = 1
            FOREACH cursor_1 INTO l_record3[pos].lav_operacion,
                                  l_record3[pos].reporte_desc

               LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record3 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_reg.lav_reporte = l_record3[pos].lav_operacion
                     LET vreporte_desc     = l_record3[pos].reporte_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                     LET pos = ARR_CURR()
--                     EXIT DISPLAY
               END DISPLAY                                                    
               CLOSE WINDOW ventana_2

               DISPLAY BY NAME g_reg.lav_reporte,
                               vreporte_desc

               LET vcodigo = g_reg.lav_reporte

            ELSE
               ERROR "   ARCHIVO VACIO   "
            END IF
            NEXT FIELD condicion_cod

         ELSE
            SELECT "X"
            FROM    tab_tipo_reporte
            WHERE   lav_operacion = g_reg.lav_reporte

            LET vcodigo = g_reg.lav_reporte

            IF STATUS = NOTFOUND THEN
               ERROR "   CODIGO INEXISTENTE   "
               SLEEP 2
               ERROR " "
               NEXT FIELD lav_reporte 
            ELSE
               SELECT reporte_desc
               INTO   vreporte_desc     
               FROM   tab_tipo_reporte
               WHERE  lav_operacion = g_reg.lav_reporte 

               DISPLAY BY NAME vreporte_desc    
               NEXT FIELD condicion_cod
            END IF
         END IF                                     

      AFTER FIELD condicion_cod
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD lav_reporte
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD condicion_desc
         END IF                                                                

         IF g_reg.condicion_cod IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD condicion_cod
         ELSE   
            LET vcodigo2 = vcodigo + 100

            IF g_reg.condicion_cod  <= vcodigo  OR   g_reg.condicion_cod >= vcodigo2 THEN
              ERROR "   EL CODIGO DEBE DE SER MAYOR A : ",vcodigo," Y MENOR A : ",vcodigo2,"   "
               NEXT FIELD condicion_cod
                   
            ELSE   
               SELECT "X"
               FROM    lav_condicion
               WHERE   condicion_cod = g_reg.condicion_cod

               IF STATUS <> NOTFOUND THEN
                  ERROR "   EL CODIGO YA EXISTE   "
                  SLEEP 2
                  ERROR " "
                  NEXT FIELD condicion_cod
               END IF         
            END IF
         END IF 

      AFTER FIELD condicion_desc 
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD condicion_cod
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD moneda_cod
         END IF                                                                

         IF g_reg.condicion_desc  IS NULL THEN
            ERROR "   DESCRIPCION NO PUEDE SER NULA   "
            NEXT FIELD condicion_desc 
         END IF     


      AFTER FIELD moneda_cod
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD condicion_desc
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD monto_desde
         END IF                                                                

         IF g_reg.moneda_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0663" ATTRIBUTE (BORDER
)
            DISPLAY " (ENTER) Consulta " AT 1,1
            DISPLAY "                             TIPO DE MONEDA                                                " AT 2,1 ATTRIBUTE(REVERSE)  

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON moneda_cod
               ON KEY (control-m)
                  LET int_flag = FALSE
                  EXIT CONSTRUCT
               ON KEY (control-c)
                  IF int_flag = TRUE THEN
                     EXIT CONSTRUCT
                  END IF
            END CONSTRUCT                 

               IF int_flag = TRUE THEN
               LET int_flag = FALSE
               ERROR "   BUSQUEDA CANCELADA...   "
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               NEXT FIELD moneda_cod
            END IF

            LET sel_where = "SELECT          ",
                            " moneda_cod    ,",
                            " moneda_desc   ,",
                            " moneda_abrevia ",
                            " FROM tab_moneda WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query2 FROM sel_where

            DECLARE cursor_2 CURSOR FOR query2

                                                                    
             LET pos = 1
            FOREACH cursor_2 INTO l_record2[pos].moneda_cod,
                                  l_record2[pos].moneda_desc,
                                  l_record2[pos].moneda_abrevia

               LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_reg.moneda_cod = l_record2[pos].moneda_cod
                     LET vmoneda_desc     = l_record2[pos].moneda_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                     LET pos = ARR_CURR()
--                     EXIT DISPLAY
                                                                      
               END DISPLAY
               CLOSE WINDOW ventana_2

               DISPLAY BY NAME g_reg.moneda_cod,
                               vmoneda_desc

            ELSE
               ERROR "   ARCHIVO DE MARCAS VACIO   "
            END IF
            NEXT FIELD monto_desde  

         ELSE
            SELECT "X"
            FROM    tab_moneda
            WHERE   moneda_cod = g_reg.moneda_cod

            IF STATUS = NOTFOUND THEN
               ERROR "   CODIGO INEXISTENTE   "
               SLEEP 2
               ERROR " "
               NEXT FIELD moneda_cod
                                                 
            ELSE
               SELECT moneda_desc
               INTO   vmoneda_desc
               FROM   tab_moneda
               WHERE  moneda_cod = g_reg.moneda_cod

               DISPLAY BY NAME vmoneda_desc
               NEXT FIELD monto_desde  
            END IF
         END IF                                                                                                
      AFTER FIELD monto_desde   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD moneda_cod   
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD monto_hasta
         END IF                                                                

         IF g_reg.monto_desde IS NULL THEN
            ERROR "   EL MONTO NO PUEDE SER NULO   "
            NEXT FIELD monto_desde   
         END IF                     

      AFTER FIELD monto_hasta   
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD monto_desde
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD lav_reporte
         END IF                                                                

         IF g_reg.monto_hasta  IS NULL THEN
            ERROR "   EL MONTO NO PUEDE SER NULO   "
            NEXT FIELD monto_hasta
         END IF                   
  
      ON KEY ( ESC )

          IF g_reg.lav_reporte IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD lav_reporte
         END IF                                                                                  
         IF g_reg.condicion_cod IS NULL THEN
            ERROR "   CODGIO NO PUEDER DER NULO   "
            NEXT FIELD condicion_cod
         END IF       
   
         IF g_reg.condicion_desc  IS NULL THEN
            ERROR "   LA DESCRIPCION NO PUEDE SER NULA   "
            NEXT FIELD condicion_desc
         END IF       

         IF g_reg.moneda_cod IS NULL THEN
            ERROR "   CODIGO NO PUEDE SER NULO   "
            NEXT FIELD moneda_cod 
         END IF

         IF g_reg.monto_desde IS NULL THEN
            ERROR "   EL MONTO NO PUEDE SER NULO   "
            NEXT FIELD monto_desde
         END IF

         IF g_reg.monto_hasta  IS NULL THEN
            ERROR "   EL MONTO NO PUEDE SER NULO   "
            NEXT FIELD monto_hasta
         END IF                               

         INSERT INTO lav_condicion VALUES (g_reg.lav_reporte       ,
                                           g_reg.condicion_cod     ,
                                           g_reg.condicion_desc    ,
                                           g_reg.moneda_cod        ,
                                           g_reg.monto_desde       ,
                                           g_reg.monto_hasta       , 
                                           usuario                 ,
                                           TODAY                   )

         ERROR "   REGISTRO INGRESADO   " SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()

         NEXT FIELD lav_reporte                                            
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0692" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta            (Ctrol-p) Impresion             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "                       CONDICIONES DE LAVADO DE DINERO                                        " AT 3,1 ATTRIBUTE(REVERSE)  
                                                                        
      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_reporte FROM lav_reporte 
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

      LET sel_where = "SELECT lav_reporte,condicion_cod,condicion_desc,moneda_cod,monto_desde,monto_hasta FROM lav_condicion  WHERE ",
                                                                     
                       cla_where CLIPPED ,"ORDER BY 1 "

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
          ERROR "   REGISTRO .... INEXISTENTE   "
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0692" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "               Escoja con < ENTER > la condicion a modificar                       " AT 2,1
      DISPLAY "                     CONDICIONES DE LAVADO DE DINERO                                       " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_reporte FROM lav_reporte
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

      LET sel_where = "SELECT lav_reporte,condicion_cod,condicion_desc,moneda_cod,monto_desde,monto_hasta FROM lav_condicion  WHERE ",

                       cla_where CLIPPED ,"ORDER BY 1 "                        

      PREPARE query4 FROM sel_where
                                                                               
      DECLARE cursor_4 CURSOR FOR query4

      LET pos = 1
      FOREACH cursor_4 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.lav_reporte      = l_record[pos].lav_reporte
               LET vlav_reporte           = l_record[pos].lav_reporte
               LET g_reg.condicion_cod    = l_record[pos].condicion_cod
               LET vcondicion_cod         = l_record[pos].condicion_cod
               LET g_reg.condicion_desc   = l_record[pos].condicion_desc
               LET g_reg.moneda_cod       = l_record[pos].moneda_cod    
               LET vmoneda_cod            = l_record[pos].moneda_cod    
               LET g_reg.monto_desde      = l_record[pos].monto_desde   
               LET g_reg.monto_hasta      = l_record[pos].monto_hasta   
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
          ERROR "   REGISTRO .... INEXISTENTE   "
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
                                                                     
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      SELECT reporte_desc
      INTO   vreporte_desc
      FROM   tab_tipo_reporte
      WHERE  lav_operacion = vlav_reporte       

      SELECT moneda_desc
      INTO vmoneda_desc
      FROM tab_moneda
      WHERE moneda_cod = vmoneda_cod

      DISPLAY BY NAME g_reg.lav_reporte, 
                      vreporte_desc,
                      g_reg.condicion_cod,
                      g_reg.condicion_desc,
                      g_reg.moneda_cod,
                      vmoneda_desc,
                      g_reg.monto_desde,
                      g_reg.monto_hasta


      INPUT BY NAME g_reg.moneda_cod,
                    g_reg.monto_desde,
                    g_reg.monto_hasta WITHOUT DEFAULTS

 
         AFTER FIELD moneda_cod
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD monto_hasta   
            END IF
              
            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD monto_desde
            END IF                                    

            IF g_reg.moneda_cod IS NULL THEN
               OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0663" ATTRIBUTE (BORDER)
               DISPLAY " (ENTER) Consulta " AT 1,1
              DISPLAY "                             TIPO DE MONEDA                                               " AT 2,1 ATTRIBUTE(REVERSE)   

               LET int_flag = FALSE
 
               CONSTRUCT BY NAME cla_where ON moneda_cod
                  ON KEY (control-m)
                     LET int_flag = FALSE
                        EXIT CONSTRUCT
                  ON KEY (control-c)
                     IF int_flag = TRUE THEN
                        EXIT CONSTRUCT
                     END IF
               END CONSTRUCT   
              
               IF int_flag = TRUE THEN
                  LET int_flag = FALSE
                  ERROR "   BUSQUEDA CANCELADA...   "
                  SLEEP 2
                  ERROR ""
                  CLEAR SCREEN
                  CLOSE WINDOW ventana_2
                  NEXT FIELD moneda_cod
               END IF

               LET sel_where = "SELECT          ",
                               " moneda_cod    ,",
                               " moneda_desc   ,",
                               " moneda_abrevia ",
                               " FROM tab_moneda WHERE ",      
                                cla_where CLIPPED ,
                               " ORDER BY 1,2 "
                                                       

               LET sel_where = sel_where CLIPPED

               PREPARE query6 FROM sel_where

               DECLARE cursor_6 CURSOR FOR query6


                LET pos = 1
                FOREACH cursor_6 INTO l_record2[pos].moneda_cod,
                                      l_record2[pos].moneda_desc,
                                      l_record2[pos].moneda_abrevia

                   LET pos = pos+1
                END FOREACH

                IF (pos-1) >= 1 THEN
                   CALL SET_COUNT(pos-1)
                   DISPLAY ARRAY l_record2 TO scr_1.*
                      ON KEY (CONTROL-M)
                         LET pos = ARR_CURR()                         
                         LET g_reg.moneda_cod = l_record2[pos].moneda_cod
                         LET vmoneda_desc     = l_record2[pos].moneda_desc
                         EXIT DISPLAY
                      ON KEY (INTERRUPT)
                         ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                         LET pos = ARR_CURR()
                      ON KEY (control-c)
                         ERROR "   USTED DEBE ESCOJER UN REGISTRO   "
                         LET pos = ARR_CURR()
                   END DISPLAY
                   CLOSE WINDOW ventana_2

                   DISPLAY BY NAME g_reg.moneda_cod,
                                   vmoneda_desc

                ELSE
                   ERROR "   ARCHIVO DE MARCAS VACIO   "
                END IF
                NEXT FIELD monto_desde

            ELSE                                           
               SELECT "X"
               FROM    tab_moneda
               WHERE   moneda_cod = g_reg.moneda_cod

               IF STATUS = NOTFOUND THEN
                  ERROR "   CODIGO INEXISTENTE   "
                  SLEEP 2
                  ERROR " "
                  NEXT FIELD moneda_cod

               ELSE
                  SELECT moneda_desc
                  INTO   vmoneda_desc
                  FROM   tab_moneda
                  WHERE  moneda_cod = g_reg.moneda_cod

                  DISPLAY BY NAME vmoneda_desc
                  NEXT FIELD monto_desde
               END IF
            END IF

         AFTER FIELD monto_desde
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD moneda_cod
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD monto_hasta
            END IF                                                       

            IF g_reg.monto_desde IS NULL THEN                              
               ERROR "   EL MONTO NO PUEDE SER NULO   "
               NEXT FIELD monto_desde
            END IF

         AFTER FIELD monto_hasta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD monto_desde
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD moneda_cod
            END IF              

            IF g_reg.monto_hasta  IS NULL THEN
               ERROR "   EL MONTO NO PUEDE SER NULO   "
               NEXT FIELD monto_hasta
            END IF
                 
            WHILE TRUE 
                
            PROMPT "Esta seguro S/N ? "
            FOR CHAR aux_pausa

            IF aux_pausa MATCHES "[sS]" THEN
               UPDATE lav_condicion
               SET moneda_cod        = g_reg.moneda_cod,    
                   monto_desde       = g_reg.monto_desde,   
                   monto_hasta       = g_reg.monto_hasta,   
                   usuario           = usuario,
                   factualiza        = TODAY
                WHERE lav_reporte    = vlav_reporte   
                AND   condicion_cod  = vcondicion_cod 
                AND   moneda_cod     = vmoneda_cod  

                IF SQLCA.SQLCODE != 0 then
                   ERROR "   ERROR EN LA ACTUALIZACION   "
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
      ERROR "   ARCHIVO DE VACIO   "
   END IF
   CLEAR SCREEN
END FUNCTION                        
################################################################################
FUNCTION elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0692" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "                 Escoja con < ENTER > la condicion a eliminar                   " AT 2,1
      DISPLAY "                        CONDICIONES DE LAVADO DE DINERO                                    " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON lav_reporte FROM lav_reporte
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

      LET sel_where = "SELECT lav_reporte,condicion_cod,condicion_desc,moneda_cod,monto_desde,monto_hasta FROM lav_condicion  WHERE ",

                       cla_where CLIPPED ,"ORDER BY 1 "       

      PREPARE query7 FROM sel_where                     

      DECLARE cursor_7 CURSOR FOR query7

      LET pos = 1
      FOREACH cursor_7 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.lav_reporte      = l_record[pos].lav_reporte
               LET vlav_reporte           = l_record[pos].lav_reporte
               LET g_reg.condicion_cod    = l_record[pos].condicion_cod
               LET vcondicion_cod         = l_record[pos].condicion_cod
               LET g_reg.condicion_desc   = l_record[pos].condicion_desc
               LET g_reg.moneda_cod       = l_record[pos].moneda_cod
               LET vmoneda_cod            = l_record[pos].moneda_cod
               LET g_reg.monto_desde      = l_record[pos].monto_desde
               LET g_reg.monto_hasta      = l_record[pos].monto_hasta
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
         ERROR "   REGISTRO .... INEXISTENTE   "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)

      SELECT reporte_desc
      INTO   vreporte_desc
      FROM   tab_tipo_reporte
      WHERE  lav_operacion = vlav_reporte

      SELECT moneda_desc
      INTO   vmoneda_desc
      FROM   tab_moneda
      WHERE  moneda_cod  =  vmoneda_cod

      DISPLAY BY NAME g_reg.lav_reporte,
                      vreporte_desc,
                      g_reg.condicion_cod,
                      g_reg.condicion_desc,
                      g_reg.moneda_cod,
                      vmoneda_desc,
                      g_reg.monto_desde,
                      g_reg.monto_hasta

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM lav_condicion     
         WHERE lav_reporte    = vlav_reporte
         AND   condicion_cod  = vcondicion_cod
         AND   moneda_cod     = vmoneda_cod      

         IF SQLCA.SQLCODE != 0 THEN
            ERROR "   ERROR EN LA ELIMINACION   "
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
      ERROR "   REGISTRO .... INEXISTENTE   "
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
                  ".IMPCONDLAV",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_lav_condicion TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.lav_reporte      = l_record[i].lav_reporte
       LET g_reg.condicion_cod    = l_record[i].condicion_cod
       LET g_reg.condicion_desc   = l_record[i].condicion_desc
       LET g_reg.moneda_cod       = l_record[i].moneda_cod    
       LET g_reg.monto_desde      = l_record[i].monto_desde   
       LET g_reg.monto_hasta      = l_record[i].monto_hasta   

       IF g_reg.lav_reporte IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_lav_condicion(g_reg.*)
   END FOR
                                         

   FINISH REPORT rpt_lav_condicion

   ERROR "   LISTADO GENERADO...   "
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_lav_condicion (g_reg)

   DEFINE g_reg  RECORD
          lav_reporte          SMALLINT,
          condicion_cod        SMALLINT,
          condicion_desc       CHAR(20),
          moneda_cod           SMALLINT,
          monto_desde          DECIMAL(16,6),
          monto_hasta          DECIMAL(16,6)     
   END RECORD                             

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60
   FORMAT
      PAGE HEADER                                                     
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033(s13H\033(s7B'
         PRINT COLUMN 02," TABM069 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'

         PRINT COLUMN 29,"CATALOGO DE CONDICIONES DE LAVADO DE DINERO"          

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         SKIP 2 LINE

         PRINT COLUMN 05,"Codigo",      
               COLUMN 22,"Codigo",        
               COLUMN 38,"Descripcion",     
               COLUMN 72,"Codigo",
               COLUMN 100,"Monto",
               COLUMN 120,"Monto"

         PRINT COLUMN 05,"Reporte",
               COLUMN 22,"Condicion",
               COLUMN 38,"Condicion",
               COLUMN 72,"Moneda",
               COLUMN 100,"Desde",
               COLUMN 120,"Hasta"       
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         PRINT COLUMN 04,g_reg.lav_reporte CLIPPED,        
               COLUMN 22,g_reg.condicion_cod CLIPPED,
               COLUMN 38,g_reg.condicion_desc CLIPPED,
               COLUMN 70,g_reg.moneda_cod CLIPPED,
               COLUMN 90,g_reg.monto_desde ,
               COLUMN 112,g_reg.monto_hasta 

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT                                                   
