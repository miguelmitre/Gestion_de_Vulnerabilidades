###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM066  => CATALOGO DE TIPO DE CAMBIO                          #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Fecha             => 27 DE MAYO DE 2002                                  #
#Modificado por    => OMAR SANDOVAL BADILLO                               #
#Fecha modificacion=> 03 SEPTIEMBRE 2004                                  #
#Sistema           => TAB                                                 #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.*

   DEFINE aux_pausa            CHAR(1)

   DEFINE g_reg  RECORD 
          moneda_cod           SMALLINT,
          fecha_aplica         DATE,
          tipo_cambio          DECIMAL(16,6)
   END RECORD                                     

   DEFINE l_record ARRAY[1000] OF RECORD
          moneda_cod           SMALLINT,
          fecha_aplica         DATE,
          tipo_cambio          DECIMAL(16,6) 
   END RECORD

   DEFINE l_record2 ARRAY[1000] OF RECORD     
          moneda_cod           SMALLINT,
          moneda_desc          CHAR(20),
          moneda_abrevia       CHAR(3)        
   END RECORD

   DEFINE vmoneda_cod          SMALLINT,
          vmoneda_desc         CHAR(20),
          vmoneda_abrevia      CHAR(3) ,
          vfecha_aplica        DATE,
          vfecha_aplica2       DATE,
          vfecha_aplica3       DATE,  
          vtipo_cambio         DECIMAL(16,6)
 
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

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0661" ATTRIBUTE( BORDER)
   DISPLAY " TABM066                CATALOGO DE TIPO DE CAMBIO                                        " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "TIPO DE CAMBIO "
      COMMAND "Agrega" "Agrega TIPO DE CAMBIO"
         CALL agrega() #a
      COMMAND "Consulta" "Consulta TIPO DE CAMBIO"
         CALL consulta() #c
      COMMAND "Modifica" "Modifica TIPO DE CAMBIO"
         CALL modifica() #m
      COMMAND "Elimina" "Elimina TIPO DE CAMBIO"
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

   LET g_reg.moneda_cod   = NULL
   LET g_reg.fecha_aplica = NULL
   LET g_reg.tipo_cambio  = NULL

   LET sw_1 = 0

   INPUT BY NAME g_reg.moneda_cod,
                 g_reg.fecha_aplica,
                 g_reg.tipo_cambio WITHOUT DEFAULTS

      AFTER FIELD moneda_cod

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD tipo_cambio
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD fecha_aplica
         END IF                                                                

         IF g_reg.moneda_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0663" ATTRIBUTE (BORDER)
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

            PREPARE query1 FROM sel_where

            DECLARE cursor_1 CURSOR FOR query1                    

            LET pos = 1
            FOREACH cursor_1 INTO l_record2[pos].moneda_cod,
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
            NEXT FIELD fecha_aplica

         ELSE
            SELECT "X"
            FROM    tab_moneda
            WHERE   moneda_cod = g_reg.moneda_cod

            IF STATUS = NOTFOUND THEN
               ERROR "   CODIGO INEXISTENTE   "
               NEXT FIELD moneda_cod
            ELSE
               SELECT moneda_desc
               INTO   vmoneda_desc      
               FROM   tab_moneda
               WHERE  moneda_cod = g_reg.moneda_cod  

               DISPLAY BY NAME vmoneda_desc     
               NEXT FIELD fecha_aplica
            END IF
         END IF                                     

      AFTER FIELD fecha_aplica
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD moneda_cod
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD tipo_cambio
         END IF                                                                

         IF g_reg.fecha_aplica IS NULL THEN
            ERROR "   LA FECHA NO PUEDE SER NULA   "
            NEXT FIELD fecha_aplica
         ELSE
            LET vfecha_aplica2 = g_reg.fecha_aplica

            IF vfecha_aplica2 > HOY THEN
               ERROR " LA FECHA NO PUEDE SER MAYOR AL DIA DE HOY "
               SLEEP 2
               NEXT FIELD fecha_aplica
            END IF
{
            LET vfecha_aplica2 = MDY(MONTH(vfecha_aplica2),1,YEAR(vfecha_aplica2))

            CALL habil_siguiente(vfecha_aplica2)
               RETURNING vfecha_aplica3  

            IF vfecha_aplica3 <> g_reg.fecha_aplica THEN
               ERROR "   CAMPO INCORRECTO LA FECHA DEBE SER:",vfecha_aplica3 USING "dd/mm/yyyy","   " 
               NEXT FIELD fecha_aplica
            END IF 
}
            SELECT "X"
            FROM    tab_tipo_cambio
            WHERE   moneda_cod   = g_reg.moneda_cod
            AND     fecha_aplica = g_reg.fecha_aplica

            IF STATUS <> NOTFOUND THEN
               ERROR "   LA FECHA YA EXISTE   "
               SLEEP 2
               ERROR " "
               NEXT FIELD fecha_aplica                                    
            END IF
         END IF

      AFTER FIELD tipo_cambio 
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD fecha_aplica
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD moneda_cod
         END IF                                                                

         IF g_reg.tipo_cambio  IS NULL THEN
            ERROR "   TIPO DE CAMBIO NO PUEDE SER NULO   "
            NEXT FIELD tipo_cambio 
         END IF     

      ON KEY ( ESC )

          IF g_reg.moneda_cod IS NULL THEN
            ERROR "   CODIGO NO PUEDE SE NULO   "
            NEXT FIELD moneda_cod
         END IF                                                                                  
         IF g_reg.fecha_aplica IS NULL THEN
            ERROR "   LA FECHA NO PUEDE SER NULA   "
            NEXT FIELD fecha_aplica
         END IF       
   
         IF g_reg.tipo_cambio  IS NULL THEN
            ERROR "   TIPO DE CAMBIO NO PUEDE SER NULO   "   
            NEXT FIELD tipo_cambio
         END IF       

         INSERT INTO tab_tipo_cambio  VALUES (g_reg.moneda_cod       ,
                                              g_reg.fecha_aplica     ,
                                              g_reg.tipo_cambio      ,
                                              usuario                ,
                                              TODAY                  )

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
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF
                                                         
      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig

END FUNCTION                           
###############################################################################
FUNCTION consulta()
#c-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0662" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion           (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "                              TIPO DE CAMBIO                                       " AT 3,1 ATTRIBUTE(REVERSE)  
                                                                        
      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod,fecha_aplica 
                        FROM moneda_cod,fecha_aplica
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

      LET sel_where = "SELECT moneda_cod,fecha_aplica,tipo_cambio FROM tab_tipo_cambio  WHERE ",
                                                                     
                       cla_where CLIPPED ,"ORDER BY 1,2 "

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
          ERROR "   REGISTRO DE TIPO DE CAMBIO .... NO EXISTE   "
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0662" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "             Escoja con < ENTER > el tipo de cambio a modificar                       " AT 2,1
      DISPLAY "                             TIPO DE CAMBIO                                       " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod,fecha_aplica 
                        FROM moneda_cod,fecha_aplica
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

      LET sel_where = "SELECT moneda_cod,fecha_aplica,tipo_cambio FROM tab_tipo_cambio WHERE ",
                       cla_where CLIPPED , "ORDER BY 1,2 "

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
               LET g_reg.moneda_cod      = l_record[pos].moneda_cod
               LET vmoneda_cod           = l_record[pos].moneda_cod
               LET g_reg.fecha_aplica    = l_record[pos].fecha_aplica
               LET vfecha_aplica         = l_record[pos].fecha_aplica
               LET g_reg.tipo_cambio     = l_record[pos].tipo_cambio 
               LET vtipo_cambio          = l_record[pos].tipo_cambio 
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
          ERROR "   REGISTRO DE TIPO DE CAMBIO .... NO EXISTE   "
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
                                                                     
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
      
      SELECT moneda_desc
      INTO vmoneda_desc
      FROM tab_moneda
      WHERE moneda_cod  =  vmoneda_cod

      DISPLAY BY NAME g_reg.moneda_cod,
                      vmoneda_desc,
                      g_reg.fecha_aplica,
                      g_reg.tipo_cambio

      INPUT BY NAME  g_reg.fecha_aplica,
                     g_reg.tipo_cambio WITHOUT DEFAULTS

      AFTER FIELD fecha_aplica
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
             NEXT FIELD fecha_aplica
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD tipo_cambio
         END IF                                                                

         IF g_reg.fecha_aplica IS NULL THEN
            ERROR "   LA FECHA NO PUEDE SER NULA   "
            NEXT FIELD fecha_aplica
         ELSE
            LET vfecha_aplica2 = g_reg.fecha_aplica

            IF vfecha_aplica2 > HOY THEN
               ERROR " LA FECHA NO PUEDE SER MAYOR AL DIA DE HOY "
               SLEEP 2
               NEXT FIELD fecha_aplica
            END IF

            SELECT "X"
            FROM    tab_tipo_cambio
            WHERE   moneda_cod   = g_reg.moneda_cod
            AND     fecha_aplica = g_reg.fecha_aplica

            IF STATUS <> NOTFOUND THEN
               ERROR "   LA FECHA YA EXISTE   "
               SLEEP 2
               ERROR " "
               NEXT FIELD fecha_aplica                                    
            END IF
         END IF

         AFTER FIELD tipo_cambio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_aplica 
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
               NEXT FIELD tipo_cambio 
            END IF                                                       

            IF g_reg.tipo_cambio  IS NULL THEN
               ERROR "   TIPO DE CAMBIO NO PUEDE SER NULO   "
               NEXT FIELD tipo_cambio
            END IF    
                            
            WHILE TRUE

             PROMPT "Esta seguro S/N ? "
             FOR CHAR aux_pausa

             IF aux_pausa MATCHES "[sS]" THEN
                UPDATE tab_tipo_cambio
                SET fecha_aplica    = g_reg.fecha_aplica,
                    tipo_cambio     = g_reg.tipo_cambio,
                    usuario         = usuario,
                    factualiza      = TODAY 
                WHERE moneda_cod    = vmoneda_cod    
                AND   fecha_aplica  = vfecha_aplica
                AND   tipo_cambio   = vtipo_cambio 

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
      ERROR "   ARCHIVO DE TIPO DE CAMBIO VACIO   "
   END IF
   CLEAR SCREEN
END FUNCTION                        
################################################################################
FUNCTION elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0662" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE) 
      DISPLAY "              Escoja con < ENTER > el tipo de cambio a eliminar                   " AT 2,1
      DISPLAY "                             TIPO DE CAMBIO                                       " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON moneda_cod,fecha_aplica 
                        FROM moneda_cod,fecha_aplica
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

      LET sel_where = "SELECT moneda_cod,fecha_aplica,tipo_cambio FROM tab_tipo_cambio WHERE ", 

                       cla_where CLIPPED , "ORDER BY 1,2 "

      PREPARE query5 FROM sel_where                     

      DECLARE cursor_5 CURSOR FOR query5

      LET pos = 1
      FOREACH cursor_5 INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.moneda_cod      = l_record[pos].moneda_cod
               LET g_reg.fecha_aplica    = l_record[pos].fecha_aplica 
               LET g_reg.tipo_cambio     = l_record[pos].tipo_cambio 
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
         ERROR "   REGISTRO DE TIPO DE CAMBIO .... NO EXISTE   "
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)

      SELECT moneda_desc
      INTO vmoneda_desc
      FROM tab_moneda                     
      WHERE moneda_cod = g_reg.moneda_cod                                                                         
      DISPLAY BY NAME g_reg.moneda_cod,
                      vmoneda_desc, 
                      g_reg.fecha_aplica,
                      g_reg.tipo_cambio

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM tab_tipo_cambio
         WHERE moneda_cod   = g_reg.moneda_cod
         AND   fecha_aplica = g_reg.fecha_aplica 
         AND   tipo_cambio  = g_reg.tipo_cambio 

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
      ERROR "   REGISTRO DE TIPO DE CAMBIO .... NO EXISTE   "
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
                  ".IMPTIPOCAMB",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tab_tipo_cambio TO g_impre

   FOR i=1 TO (pos+1)
       LET g_reg.moneda_cod      = l_record[i].moneda_cod 
       LET g_reg.fecha_aplica    = l_record[i].fecha_aplica
       LET g_reg.tipo_cambio     = l_record[i].tipo_cambio    

       IF g_reg.moneda_cod IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_tab_tipo_cambio(g_reg.*)
   END FOR
                                         

   FINISH REPORT rpt_tab_tipo_cambio

   ERROR "   LISTADO GENERADO...   "
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_tipo_cambio (g_reg)

   DEFINE g_reg  RECORD
          moneda_cod           SMALLINT,
          fecha_aplica         DATE,
          tipo_cambio          DECIMAL(16,6)
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
         PRINT COLUMN 02," TABM066 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'

         PRINT COLUMN 31,"CATALOGO DE TIPO DE CAMBIO"          

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         SKIP 2 LINE

         PRINT COLUMN 26,"Codigo Moneda",
               COLUMN 57,"Fecha Aplica",
               COLUMN 98,"Tipo Cambio"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'
         PRINT COLUMN 28,g_reg.moneda_cod CLIPPED,        
               COLUMN 58,g_reg.fecha_aplica USING "dd/mm/yyyy" CLIPPED,
               COLUMN 90,g_reg.tipo_cambio 
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT                                                   
