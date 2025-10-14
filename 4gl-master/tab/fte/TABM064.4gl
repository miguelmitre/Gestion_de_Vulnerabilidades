###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM064  => CATALOGO DE ESTADOS DE MARCA                        # 
#Fecha             => 11 DE MARZO DE 2002  	                          #
#Por               => STEFANIE DANIELA VERA PIÑA                          #
#Sistema           => TAB                                                 #
#Modificado por    => OMAR SANDOVAL BADILLO                               #
#Fecha Modificado  => 26 DE NOVIEMBRE DE 2003                             #
###########################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis          RECORD LIKE seg_modulo.* 

   DEFINE g_marca  RECORD LIKE tab_estado_marca.*

   DEFINE l_record ARRAY[1000] OF RECORD
          marca_estado           SMALLINT,
          marca_desc             CHAR(50),
          fecha_actualiza        DATE,
          usuario                CHAR(8)
   END RECORD

   DEFINE usuario              CHAR(08),
          aux_pausa            CHAR(1),
          sw_1                 SMALLINT,
          bandera              ,
          pos                  SMALLINT,
          vmarca_estado        SMALLINT,
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

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0641" ATTRIBUTE( BORDER)
   DISPLAY " TABM064              CATALOGO DE ESTADOS DE MARCA                                " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "ESTADOS DE MARCA"       
      COMMAND "Agrega" "Agrega ESTADO DE MARCA"
         CALL Agrega() #a
      COMMAND "Consulta" "Consulta ESTADO DE MARCA"
         CALL Consulta() #c
      COMMAND "Modifica" "Modifica ESTADO DE MARCA"
         CALL Modifica() #m
      COMMAND "Elimina" "Elimina ESTADO DE MARCA"
         CALL Elimina() #e
      COMMAND KEY(S) "Salir" "Salir del Programa"
      EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION inic()
#--------------
   SELECT USER,
          ruta_listados
      INTO   usuario,
             g_param_dis.ruta_listados
      FROM   seg_modulo
      WHERE  modulo_cod = "tab"
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
   DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_marca.marca_estado    = NULL
   LET g_marca.marca_desc      = NULL
   LET g_marca.fecha_actualiza = TODAY
   LET g_marca.usuario         = usuario 


   LET sw_1 = 0

   INPUT BY NAME g_marca.marca_estado,
                 g_marca.marca_desc WITHOUT DEFAULTS  

      AFTER FIELD marca_estado
         IF g_marca.marca_estado IS NULL THEN 
            ERROR "Estado de Marca NO puede ser nulo"
            NEXT FIELD marca_estado
         END IF
 
         SELECT "X" 
            FROM    tab_estado_marca
            WHERE   marca_estado = g_marca.marca_estado

         IF STATUS <> NOTFOUND THEN
            ERROR "Codigo Ya Ingresado"
            SLEEP 2
            ERROR " "  
            NEXT FIELD marca_estado
         END IF 

      AFTER FIELD marca_desc
         IF g_marca.marca_desc IS NULL THEN
            ERROR "Descripcion de Marca NO puede ser nulo"
            NEXT FIELD marca_desc
	 END IF

         SELECT "X"
            FROM tab_estado_marca
            WHERE marca_desc = g_marca.marca_desc

         IF STATUS <> NOTFOUND THEN
            ERROR "Codigo ya Ingresado"
            SLEEP 2
            ERROR " "
            NEXT FIELD marca_desc 
         END IF

      ON KEY ( ESC )
         IF g_marca.marca_estado IS NULL THEN
            ERROR "Codigo de Marca NO puede ser nulo"
            NEXT FIELD marca_estado
         END IF

         SELECT "X" 
            FROM    tab_estado_marca
            WHERE   marca_estado = g_marca.marca_estado

         IF STATUS <> NOTFOUND THEN
            ERROR "Codigo Ya Ingresado"
            SLEEP 2
            ERROR " "  
            NEXT FIELD marca_estado
         END IF 

         IF g_marca.marca_desc IS NULL THEN
            ERROR "Descripcion de Marca NO puede ser nulo"
            NEXT FIELD marca_desc
         END IF

         LET g_marca.fecha_actualiza = TODAY

         INSERT INTO tab_estado_marca VALUES (g_marca.marca_estado  ,
     			    	           g_marca.marca_desc       ,
  				           g_marca.fecha_actualiza  , 
				      	   USER              )

         ERROR "REGISTRO INGRESADO" SLEEP 1
         ERROR ""
         CLEAR FORM
         CALL Inicializa()
         NEXT FIELD marca_estado

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0642" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta             (Ctrol-p) Impresion            (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                         ESTADOS DE MARCAS                                        " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE   

      CONSTRUCT cla_where ON marca_estado FROM marca_estado
         ON KEY (control-m)
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

      LET sel_where = "SELECT marca_estado,marca_desc,fecha_actualiza,usuario FROM tab_estado_marca  WHERE ",
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
               ERROR "PROCESANDO IMPRESION..."
               CALL Impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            ON KEY (control-c)
               EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTRO DE ESTADO DE MARCA .... NO EXISTE"
          SLEEP 2
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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0642" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "            Escoja con < ENTER > el estado de marca a modificar                      " AT 2,1
      DISPLAY "                          ESTADOS DE MARCAS                                    " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON marca_estado FROM marca_estado
         ON KEY (control-m)
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
 
      LET sel_where = "SELECT marca_estado,marca_desc,fecha_actualiza,usuario FROM tab_estado_marca WHERE ",
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
               LET g_marca.marca_estado    = l_record[pos].marca_estado
               LET vmarca_estado           = l_record[pos].marca_estado
               LET g_marca.marca_desc      = l_record[pos].marca_desc
               LET g_marca.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_marca.usuario         = l_record[pos].usuario
               EXIT DISPLAY                    
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()     
            ON KEY (control-c)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()                    
          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "REGISTRO DE ESTADO DE MARCA .... NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

       DISPLAY "" AT 1,1
       DISPLAY "" AT 2,1    
       DISPLAY " (Ctrl-C) Salir " AT 1,1
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
 
       DISPLAY BY NAME g_marca.marca_estado,
                       g_marca.marca_desc

       INPUT BY NAME g_marca.marca_desc WITHOUT DEFAULTS
           
          AFTER FIELD marca_desc
              IF g_marca.marca_desc IS NULL THEN
                 ERROR "Descripcion de Marca NO puede ser nulo"
                 NEXT FIELD marca_desc
              END IF

          PROMPT "Esta seguro S/N ? "
             FOR CHAR aux_pausa
          IF aux_pausa MATCHES "[sS]" THEN
             UPDATE tab_estado_marca
                SET marca_estado    = g_marca.marca_estado,
                    marca_desc      = g_marca.marca_desc,
                    fecha_actualiza = g_marca.fecha_actualiza
                WHERE marca_estado  = vmarca_estado

             IF SQLCA.SQLCODE != 0 then
                ERROR "Error en la Actualizacion de Estados de Marcas"
                ATTRIBUTE (reverse)
                SLEEP 2
                ERROR " "
             ELSE
                ERROR "REGISTRO MODIFICADO"
                SLEEP 1
                ERROR ""
                CLEAR FORM
             END IF
          ELSE
             ERROR "PROCESO DE MODIFICAR,CANCELADO"
             SLEEP 2
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
       ERROR "ARCHIVO DE ESTADOS DE MARCAS VACIO"
   END IF
   CLEAR SCREEN      
END FUNCTION
################################################################################
FUNCTION Elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0642" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "             Escoja con < ENTER > el estado de marca a eliminar         " AT 2,1 
      DISPLAY "                           ESTADOS DE MARCA                                    " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE
      CONSTRUCT cla_where ON marca_estado FROM marca_estado
         ON KEY (control-m)
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

      LET sel_where = "SELECT marca_estado,marca_desc,fecha_actualiza,usuario FROM tab_estado_marca WHERE ",
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
               LET g_marca.marca_estado    = l_record[pos].marca_estado
               LET g_marca.marca_desc      = l_record[pos].marca_desc   
               LET g_marca.fecha_actualiza = l_record[pos].fecha_actualiza
               LET g_marca.usuario         = l_record[pos].usuario
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE                                  
         ERROR "REGISTRO DE ESTADO DE MARCA .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1      
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_marca.marca_estado,
                      g_marca.marca_desc

      CALL Pregunta()
 
      IF aux_pausa MATCHES "[Ss]" THEN
         SELECT "X"
         FROM cta_convivencia
         WHERE convive_cod = g_marca.marca_estado
      
         IF SQLCA.SQLCODE = 0  THEN
            PROMPT  "Codigo actualmente en USO" FOR aux_pausa
            CALL inicializa()
            CLEAR FORM
         ELSE
            DELETE
            FROM tab_estado_marca
            WHERE marca_estado = g_marca.marca_estado
         
            ERROR "REGISTRO ELIMINADO"       
            SLEEP 2
            CLEAR FORM
         END IF
      ELSE
         ERROR "ELIMINAR CANCELADO"
         SLEEP 2
         CLEAR FORM
      END IF

      ERROR ""
      CALL Inicializa()
   ELSE
      ERROR "REGISTRO DE ESTADO DE MARCA .... NO EXISTE"
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

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",usuario CLIPPED,
                  ".IMESTADOMAR",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_tab_estado_marca TO g_impre

   FOR i=1 TO (pos+1)
       LET g_marca.marca_estado    = l_record[i].marca_estado
       LET g_marca.marca_desc      = l_record[i].marca_desc
       LET g_marca.fecha_actualiza = l_record[i].fecha_actualiza
       LET g_marca.usuario         = l_record[i].usuario

       IF g_marca.marca_estado IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_tab_estado_marca(g_marca.*)
   END FOR

   FINISH REPORT rpt_tab_estado_marca

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = " lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_estado_marca (g_marca)

   DEFINE g_marca  RECORD LIKE tab_estado_marca.*

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
         PRINT COLUMN 02," TABM064 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'    

         PRINT COLUMN 28,"CATALOGO DE ESTADO DE MARCAS"

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         SKIP 2 LINE         

         PRINT COLUMN 05,"Estado Marca",
               COLUMN 26,"Descripcion",
               COLUMN 56,"Fecha actualiza",
               COLUMN 80,"Usuario"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         PRINT COLUMN 12,g_marca.marca_estado ,
               COLUMN 27,g_marca.marca_desc CLIPPED,
               COLUMN 57,g_marca.fecha_actualiza,
               COLUMN 81,g_marca.usuario
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT          
