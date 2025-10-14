###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM061  => CATALOGO DE RECHAZOS DE CONVIVENCIA                 # 
#Fecha             => 11 DE MARZO DE 2002  	                          #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Modifico          => JUAN CARLOS MENDOZA MORENO                          #
#Sistema           => TAB                                                 #
###########################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE glo_parametro.* 

   DEFINE        aux_pausa             CHAR(1)

   DEFINE g_marca  RECORD LIKE tab_rch_marca.*

   DEFINE l_record ARRAY[1000] OF RECORD
          rechazo_cod            SMALLINT,
          rechazo_desc           CHAR(50),
          fecha_actualiza        DATE,
          usuario                CHAR(8)
   END RECORD


   DEFINE pos   SMALLINT

   DEFINE 
        bandera               ,
        sw_1                  SMALLINT

   DEFINE usuario              CHAR(08),
          HOY                  DATE,
          USR                  CHAR(8),   
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

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0611" ATTRIBUTE( BORDER)
   DISPLAY " TABM061             CATALOGO DE RECHAZOS DE CONVIVENCIA                               " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "RECHAZOS CONVIVENCIA"       
      COMMAND "Agrega" "Agrega RECHAZO DE CONVIVENCIA"
         CALL Agrega() #a
      COMMAND "Consulta" "Consulta RECHAZO DE CONVIVENCIA"
         CALL Consulta() #c
      COMMAND "Modifica" "Modifica RECHAZO DE CONVIVENCIA"
         CALL Modifica() #m
      COMMAND "Elimina" "Elimina RECHAZO DE CONVIVENCIA"
         CALL Elimina() #e
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
   INITIALIZE g_marca.* TO NULL
END FUNCTION   
################################################################################
FUNCTION Agrega()
#a---------------
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( ESC ) AGREGA                   (CTRL-C) SALIR                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_marca.rechazo_cod     = NULL
   LET g_marca.rechazo_desc    = NULL
   LET g_marca.fecha_actualiza = TODAY
   LET g_marca.usuario         = usuario

   LET sw_1 = 0

   INPUT BY NAME g_marca.rechazo_cod,
                 g_marca.rechazo_desc WITHOUT DEFAULTS  

      AFTER FIELD rechazo_cod
         IF g_marca.rechazo_cod IS NULL THEN 
            ERROR "  CODIGO DE RECHAZO NO PUEDE SER NULO  "
            NEXT FIELD rechazo_cod
         END IF
 
         SELECT "X" 
         FROM    tab_rch_marca
         WHERE   rechazo_cod = g_marca.rechazo_cod

         IF STATUS <> NOTFOUND THEN
            ERROR "  CODIGO YA INGRESADO  "
            SLEEP 1
            ERROR " "  
            NEXT FIELD rechazo_cod
         END IF 

      AFTER FIELD rechazo_desc
         IF g_marca.rechazo_desc IS NULL THEN
            ERROR "  DESCRIPCION DE RECHAZO NO PUEDE SER NULO  "
            NEXT FIELD rechazo_desc
	 END IF 

      ON KEY ( ESC )
         IF g_marca.rechazo_cod IS NULL THEN
            ERROR "  CODIGO DE RECHAZO NO PUEDE SER NULO  "
            NEXT FIELD rechazo_cod
         END IF

         IF g_marca.rechazo_desc IS NULL THEN
            ERROR "  DESCRIPCION DE RECHAZO NO PUEDE SER NULO  "
            NEXT FIELD rechazo_desc
         END IF

         LET g_marca.fecha_actualiza = TODAY   
--         LET g_marca.usuario = usuario

         INSERT INTO tab_rch_marca    VALUES (g_marca.rechazo_cod      ,
         			    	      g_marca.rechazo_desc     ,
  				              g_marca.fecha_actualiza  , 
				      	      USER              )

         ERROR "  REGISTRO INGRESADO  " SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()
         NEXT FIELD rechazo_cod

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
FUNCTION Consulta()
#c-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0612" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) CONSULTA              (CTROL-P) IMPRESION           (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                          RECHAZOS DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE   

      CONSTRUCT cla_where ON rechazo_cod FROM rechazo_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""      
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT rechazo_cod,rechazo_desc,fecha_actualiza,usuario FROM tab_rch_marca  WHERE ",
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
               ERROR "  PROCESANDO IMPRESION...  "
               CALL Impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            ON KEY (control-c)
               EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "  REGISTRO DE RECHAZO DE CONVIVENCIA .... NO EXISTE  "
          SLEEP 1
          ERROR ""
          CLOSE WINDOW ventana_2
       END IF                  
   END IF	 
   CLEAR SCREEN 
END FUNCTION
################################################################################
FUNCTION Modifica()
#m-----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0612" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) MODIFICA                                            (CTRL-C) SALIR     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "      Escoja con < ENTER > el tipo de RECHAZO de convivencia a modificar      " AT 2,1
      DISPLAY "                          RECHAZOS DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON rechazo_cod FROM rechazo_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN                    
      END IF
 
      LET sel_where = "SELECT rechazo_cod,rechazo_desc,fecha_actualiza,usuario FROM tab_rch_marca WHERE ",
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
               LET g_marca.rechazo_cod     = l_record[pos].rechazo_cod
               LET g_marca.rechazo_desc    = l_record[pos].rechazo_desc     
               LET g_marca.fecha_actualiza = l_record[pos].fecha_actualiza                 LET g_marca.usuario         = l_record[pos].usuario 
               EXIT DISPLAY                    
            ON KEY (INTERRUPT)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()     
            ON KEY (control-c)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()                    
          END DISPLAY
          CLOSE WINDOW ventana_2
      ELSE
          ERROR "  REGISTRO DE RECHAZO DE CONVIVENCIA .... NO EXISTE  "
          SLEEP 1
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (CTRL-C) SALIR " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_marca.rechazo_cod,
                      g_marca.rechazo_desc

      INPUT BY NAME  g_marca.rechazo_desc WITHOUT DEFAULTS 
                  --   g_marca.fecha_actualiza WITHOUT DEFAULTS

         AFTER FIELD rechazo_desc
             IF g_marca.rechazo_desc IS NULL THEN
                ERROR "  DESCRIPCION DE RECHAZO NO PUEDE SER NULO  "
                NEXT FIELD rechazo_desc
             END IF

             PROMPT "  ESTA SEGURO S/N ?  "
             FOR CHAR aux_pausa

             IF aux_pausa MATCHES "[sS]" THEN
                UPDATE tab_rch_marca
                SET rechazo_cod     = g_marca.rechazo_cod,
                    rechazo_desc    = g_marca.rechazo_desc,
                    fecha_actualiza = g_marca.fecha_actualiza,
                    usuario         = g_marca.usuario 
                WHERE rechazo_cod   = g_marca.rechazo_cod

                IF SQLCA.SQLCODE != 0 then
                   ERROR "  ERROR EN LA ACTUALIZACION DE RECHAZOS DE CONVIVENCIA  "
                   ATTRIBUTE (reverse)
                   SLEEP 1
                   ERROR " "
                ELSE
                   ERROR "  REGISTRO MODIFICADO  "
                   SLEEP 1
                   ERROR ""
                   CLEAR FORM
                END IF
             ELSE
                ERROR "  PROCESO DE MODIFICAR, CANCELADO  "
                SLEEP 1
                ERROR " "
                INITIALIZE g_marca.* TO NULL
                CLEAR FORM
             END IF                          

             CALL Inicializa()
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
      ERROR "  ARCHIVO DE RECHAZOS DE CONVIVENCIA VACIO  "
   END IF
   CLEAR SCREEN      
END FUNCTION
################################################################################
FUNCTION Elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0612" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) ELIMINA                                             (CTRL-C) SALIR     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "           Escoja con < ENTER > el RECHAZO de convivencia a eliminar         " AT 2,1 
      DISPLAY "                          RECHAZOS DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE
      CONSTRUCT cla_where ON rechazo_cod FROM rechazo_cod
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT        

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN                         
      END IF

      LET sel_where = "SELECT rechazo_cod,rechazo_desc,fecha_actualiza,usuario FROM tab_rch_marca WHERE ",
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
               LET g_marca.rechazo_cod     = l_record[pos].rechazo_cod
               LET g_marca.rechazo_desc    = l_record[pos].rechazo_desc   
               LET g_marca.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_marca.usuario         = l_record[pos].usuario
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE                                  
         ERROR "  REGISTRO DE RECHAZO DE CONVIVENCIA .... NO EXISTE  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1      
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_marca.rechazo_cod,
                      g_marca.rechazo_desc
--                      g_marca.fecha_actualiza,
--                      g_marca.usuario

      CALL Pregunta()
 
      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM tab_rch_marca
         WHERE rechazo_cod = g_marca.rechazo_cod

         IF SQLCA.SQLCODE != 0 THEN
            ERROR "  ERROR EN LA ELIMINACION DE RECHAZOS DE CONVIVENCIA  "
            ATTRIBUTE (reverse)
         ELSE
            ERROR "  REGISTRO ELIMINADO  "       
            SLEEP 1
            CLEAR FORM
         END IF
      ELSE
         ERROR "  ELIMINAR CANCELADO  "
         SLEEP 1
         CLEAR FORM
      END IF

      ERROR ""
      CALL Inicializa()
   ELSE
      ERROR "  REGISTRO DE RECHAZOS DE CONVIVENCIA .... NO EXISTE  "
   END IF
   ERROR ""             
END FUNCTION
################################################################################
FUNCTION Pregunta()
#p-----------------
   PROMPT "  ESTA SEGURO S/N ?  " 
      FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Impresion(pos)
#----------------------
   DEFINE i,pos INTEGER

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                  ".IMPRECHAZOS",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tab_rch_marca TO PRINTER

   FOR i=1 TO (pos+1)
       LET g_marca.rechazo_cod     = l_record[i].rechazo_cod
       LET g_marca.rechazo_desc    = l_record[i].rechazo_desc
       LET g_marca.fecha_actualiza = l_record[i].fecha_actualiza
       LET g_marca.usuario         = l_record[i].usuario

       IF g_marca.rechazo_cod IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_tab_rch_marca(g_marca.*)
   END FOR

   FINISH REPORT rpt_tab_rch_marca

   ERROR "  LISTADO GENERADO...  "
   SLEEP 1
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_rch_marca (g_marca)

   DEFINE g_marca  RECORD LIKE tab_rch_marca.*

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
         PRINT COLUMN 02," TABM061 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'    

         PRINT COLUMN 28,"CATALOGO DE RECHAZOS DE CONVIVENCIA"

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         SKIP 2 LINE         

         PRINT COLUMN 10,"Codigo Rechazo",
               COLUMN 32,"Descripcion",
               COLUMN 91,"Fecha actualiza",
               COLUMN 110,"Usuario"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         PRINT COLUMN 12,g_marca.rechazo_cod CLIPPED,
               COLUMN 32,g_marca.rechazo_desc CLIPPED,
               COLUMN 93,g_marca.fecha_actualiza,
               COLUMN 111,g_marca.usuario
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"TOTAL DE REGISTROS : ",COUNT(*) USING "<<<<<"
END REPORT          
