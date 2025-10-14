##############################################################################
#Proyecto              => Sistema de Afores. ( MEXICO )                      #
#Propietario           => E.F.P.                                             #
#Programa TABM062      => CATALOGO DE CONVIVENCIA                            # 
#Fecha                 => 11 DE MARZO DE 2002  	                             #
#Por                   => STEFANIE DANIELA VERA PIÑA                         #
#Sistema               => TAB                                                #
#Modificado por        => OMAR SANDOVAL BADILLO                              #
#FECHA DE MODIFICACION => 28/11/2003                                         #
##############################################################################
DATABASE safre_af
GLOBALS

   DEFINE g_param_dis      RECORD LIKE seg_modulo.* 

   DEFINE g_conviv         RECORD LIKE cta_convivencia.*

   DEFINE l_record3 ARRAY[30000] OF RECORD
          marca_entra            SMALLINT,
          marca_entra_desc       CHAR(50),
          marca_activa           SMALLINT,
          marca_activa_desc      CHAR(50),
          convive_cod            SMALLINT,
          convive_desc           CHAR(50),
          rechazo_cod            SMALLINT,
          rechazo_desc           CHAR(50),
          fecha_actualiza        DATE,
          usuario                CHAR(8)
   END RECORD     
                                                             
   DEFINE l_record ARRAY[30000] OF RECORD
          marca_entra            SMALLINT,
          marca_activa           SMALLINT,
          convive_cod            SMALLINT,
          rechazo_cod            SMALLINT,
          fecha_actualiza        DATE,
          usuario                CHAR(8)
   END RECORD

   DEFINE l_record2  ARRAY[30000] OF RECORD
          marca_cod  SMALLINT,
          marca_desc CHAR(40)
   END RECORD                                                                  

   DEFINE vconviv                 SMALLINT,
          vmarca_entra            SMALLINT,
          vmarca_activa           SMALLINT,
          vconvive_cod            SMALLINT,
          vrechazo_cod            SMALLINT,
          vmarca_entra_desc       CHAR(50),
          vmarca_activa_desc      CHAR(50),
          vconvive_desc           CHAR(50),
          vrechazo_desc           CHAR(50)

   DEFINE usuario              CHAR(08),
          bandera              , 
          aux_pausa            CHAR(1),
          pos                  SMALLINT,
          sw_1                 SMALLINT,
          HOY                  DATE,
          USR                  CHAR(8),   
          cla_where            CHAR(200),
          sel_where            CHAR(600),
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

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0621" ATTRIBUTE( BORDER)
   DISPLAY " TABM062                CATALOGO DE CONVIVENCIA                                   " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "CONVIVENCIA"       
      COMMAND "Agrega" "Agrega CONVIVENCIA"
         CALL Agrega() #a
      COMMAND "Consulta" "Consulta CONVIVENCIA"
         CALL Consulta() #c
      COMMAND "Modifica" "Modifica CONVIVENCIA"
         CALL Modifica() #m
      COMMAND "Elimina" "Elimina CONVIVENCIA"
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
   INITIALIZE g_conviv.* TO NULL
END FUNCTION   
################################################################################
FUNCTION Agrega()
#a---------------
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_conviv.marca_entra     = NULL
   LET g_conviv.marca_activa    = NULL
   LET g_conviv.convive_cod     = NULL
   LET g_conviv.rechazo_cod     = NULL
   LET g_conviv.fecha_actualiza = TODAY
   LET g_conviv.usuario         = usuario


   LET sw_1 = 0

   INPUT BY NAME g_conviv.marca_entra,
                 g_conviv.marca_activa,
                 g_conviv.convive_cod,
                 g_conviv.rechazo_cod WITHOUT DEFAULTS 

      AFTER FIELD marca_entra
         IF g_conviv.marca_entra IS NULL THEN 
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT 1,1
            DISPLAY "                 CATALOGO DE MARCAS                                              " AT 2,1 ATTRIBUTE(REVERSE) 
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON marca_cod
               ON KEY (control-m)
                  LET int_flag = FALSE
                  EXIT CONSTRUCT
               ON KEY (control-c)
                  IF int_flag = TRUE THEN
                     EXIT CONSTRUCT
                  END IF
            END CONSTRUCT

            IF int_flag = TRUE THEN
               ERROR "BUSQUEDA CANCELADA..."                              
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               NEXT FIELD marca_entra
            END IF

            LET sel_where = "SELECT      ",
                            " marca_cod ,",
                            " marca_desc ",
                            " FROM tab_marca WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query1 FROM sel_where

            DECLARE cursor_1 CURSOR FOR query1
                                                                 
            LET pos = 1
            FOREACH cursor_1 INTO l_record2[pos].marca_cod,
                                  l_record2[pos].marca_desc
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_conviv.marca_entra =l_record2[pos].marca_cod
                     LET vmarca_entra_desc    =l_record2[pos].marca_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ventana_2

               DISPLAY BY NAME g_conviv.marca_entra,
                               vmarca_entra_desc
                            
            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
               CLOSE WINDOW ventana_2
            END IF
            NEXT FIELD marca_entra 

         ELSE 
            SELECT "X"
            FROM   tab_marca
            WHERE  marca_cod = g_conviv.marca_entra

            IF STATUS = NOTFOUND THEN
               ERROR " MARCA INEXISTENTE "
               NEXT FIELD marca_entra
            ELSE
                 SELECT marca_desc
                 INTO   vmarca_entra_desc
                 FROM   tab_marca      
                 WHERE  marca_cod = g_conviv.marca_entra

                DISPLAY BY NAME vmarca_entra_desc 
                NEXT FIELD marca_activa
            END IF   
         END IF

      AFTER FIELD marca_activa
         IF g_conviv.marca_activa IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)

            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT 1,1
            DISPLAY "                 CATALOGO DE MARCAS                                              " AT 2,1 ATTRIBUTE(REVERSE)
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON marca_cod
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
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               NEXT FIELD marca_activa
            END IF

            LET sel_where = "SELECT      ",
                            " marca_cod ,",
                            " marca_desc ",
                            " FROM tab_marca WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query2 FROM sel_where

            DECLARE cursor_2 CURSOR FOR query2

            LET pos = 1
            FOREACH cursor_2 INTO l_record2[pos].marca_cod,
                                  l_record2[pos].marca_desc
               LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_conviv.marca_activa =l_record2[pos].marca_cod
                     LET vmarca_activa_desc    =l_record2[pos].marca_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
               END DISPLAY
            CLOSE WINDOW ventana_2

            SELECT "X"
            FROM cta_convivencia
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa = g_conviv.marca_activa
            
            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF
            
            DISPLAY BY NAME g_conviv.marca_activa,
                            vmarca_activa_desc

            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
               CLOSE WINDOW ventana_2
            END IF
            NEXT FIELD marca_activa 

         ELSE    
            SELECT "X"
            FROM cta_convivencia 
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa = g_conviv.marca_activa

            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF

            SELECT "X"
            FROM    tab_marca
            WHERE   marca_cod = g_conviv.marca_activa

            IF STATUS = NOTFOUND THEN
               ERROR "   MARCA INEXISTENTE " 
               NEXT FIELD marca_activa
            ELSE
               SELECT marca_desc
               INTO   vmarca_activa_desc
               FROM   tab_marca
               WHERE  marca_cod = g_conviv.marca_activa

               DISPLAY BY NAME vmarca_activa_desc
               NEXT FIELD convive_cod  
            END IF
         END IF 

      AFTER FIELD convive_cod
         IF g_conviv.convive_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)

            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
            DISPLAY "              CATALOGO ESTADO DE MARCA                                        " AT 2,1 ATTRIBUTE(REVERSE)        
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

            LET int_flag = FALSE
            
            CONSTRUCT BY NAME cla_where ON marca_cod
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
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW ventana_2
               NEXT FIELD convive_cod
            END IF

            LET sel_where = "SELECT        ",
                            " marca_estado,",
                            " marca_desc   ",
                            " FROM tab_estado_marca WHERE ",
                            cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query3 FROM sel_where

            DECLARE cursor_3 CURSOR FOR query3

            LET pos = 1
            FOREACH cursor_3 INTO l_record2[pos].marca_cod,
                                  l_record2[pos].marca_desc
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_conviv.convive_cod =l_record2[pos].marca_cod
                     LET vconvive_desc        =l_record2[pos].marca_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR " Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
               END DISPLAY
               
               CLOSE WINDOW ventana_2

               IF g_conviv.convive_cod = 0 THEN
                  LET g_conviv.rechazo_cod = 0

                  SELECT rechazo_desc
                  INTO vrechazo_desc
                  FROM tab_rch_marca
                  WHERE rechazo_cod = g_conviv.rechazo_cod

                  DISPLAY BY NAME g_conviv.convive_cod,
                                  vconvive_desc,
                                  g_conviv.rechazo_cod,
                                  vrechazo_desc
                  NEXT FIELD marca_entra
               ELSE
                  DISPLAY BY NAME g_conviv.convive_cod,
                                  vconvive_desc
               END IF

            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
               CLOSE WINDOW ventana_2
               NEXT FIELD convive_cod
            END IF
            NEXT FIELD convive_cod 
         ELSE

            SELECT "X"
            FROM tab_estado_marca
            WHERE marca_estado = g_conviv.convive_cod

            IF g_conviv.convive_cod = 0 THEN
               LET g_conviv.rechazo_cod = 0
                  
               SELECT rechazo_desc
               INTO vrechazo_desc
               FROM tab_rch_marca
               WHERE rechazo_cod = g_conviv.rechazo_cod
                 
               SELECT marca_desc
               INTO vconvive_desc
               FROM tab_estado_marca
               WHERE marca_estado = g_conviv.convive_cod
 
               DISPLAY BY NAME g_conviv.convive_cod,
                               vconvive_desc,
                               g_conviv.rechazo_cod,
                               vrechazo_desc
               NEXT FIELD marca_entra
            ELSE
               IF g_conviv.rechazo_cod < 0 THEN
                  SELECT marca_desc
                  INTO vconvive_desc
                  FROM tab_estado_marca
                  WHERE marca_estado = g_conviv.convive_cod

                  DISPLAY BY NAME vconvive_desc

                  NEXT FIELD rechazo_cod
               END IF
            END IF
         END IF

            IF STATUS = NOTFOUND THEN
               ERROR "   CODIGO INEXISTENTE "  
               NEXT FIELD convive_cod
            ELSE
               SELECT marca_desc
               INTO   vconvive_desc
               FROM   tab_estado_marca
               WHERE  marca_estado = g_conviv.convive_cod
               
               DISPLAY BY NAME vconvive_desc
               NEXT FIELD      rechazo_cod
            END IF
        
      AFTER FIELD rechazo_cod
         IF g_conviv.rechazo_cod  = 0  AND
            g_conviv.convive_cod  > 0 THEN
            ERROR "CODIGO DE RECHAZO NO VALIDO "
            NEXT FIELD rechazo_cod
         END IF 

         IF g_conviv.rechazo_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)

            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
            DISPLAY "                 CATALOGO DE RECHAZOS                                            " AT 2,1 ATTRIBUTE(REVERSE)   
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON marca_cod
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
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
            CLOSE WINDOW ventana_2
            NEXT FIELD convive_cod
            END IF                           
                                  
            LET sel_where = "SELECT        ",
                            " rechazo_cod ,",
                            " rechazo_desc ",
                            " FROM tab_rch_marca WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query4 FROM sel_where

            DECLARE cursor_4 CURSOR FOR query4

            LET pos = 1
            FOREACH cursor_4 INTO l_record2[pos].marca_cod,
                                  l_record2[pos].marca_desc
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)                                           
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_conviv.rechazo_cod =l_record2[pos].marca_cod
                     LET vrechazo_desc        =l_record2[pos].marca_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro" 
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ventana_2

               IF g_conviv.rechazo_cod  = 0  AND
                  g_conviv.convive_cod > 0 THEN
                  ERROR "CODIGO DE RECHAZO NO VALIDO "
                  NEXT FIELD rechazo_cod
               END IF 

            DISPLAY BY NAME g_conviv.rechazo_cod,
                                  vrechazo_desc
            NEXT FIELD marca_entra
                 
            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
               CLOSE WINDOW ventana_2
               NEXT FIELD rechazo_cod 
            END IF                                                             
            NEXT FIELD marca_entra 

         ELSE                                                                  
            SELECT "X"
            FROM   tab_rch_marca
            WHERE  rechazo_cod = g_conviv.rechazo_cod

            IF STATUS = NOTFOUND THEN
               ERROR "  CODIGO DE RECHAZO INEXISTENTE " 
               NEXT FIELD rechazo_cod
            ELSE

               IF g_conviv.convive_cod > 0 AND
                  g_conviv.rechazo_cod = 0 THEN
                     ERROR "CODIGO DE RECHAZO NO VALIDO"
                     NEXT FIELD rechazo_cod
               ELSE   
                  SELECT rechazo_desc
                  INTO   vrechazo_desc
                  FROM   tab_rch_marca
                  WHERE  rechazo_cod=g_conviv.rechazo_cod

                  DISPLAY BY NAME vrechazo_desc
                  NEXT FIELD marca_entra
               END IF
            END IF
         END IF  
                                                              
      ON KEY ( ESC )
         IF g_conviv.marca_entra IS NULL THEN
            ERROR "Marca Entrante NO puede ser nula" 
            NEXT FIELD marca_entra
         ELSE 
            SELECT "X"
            FROM cta_convivencia
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa  = g_conviv.marca_activa
            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF 
         END IF

         IF g_conviv.marca_activa IS NULL THEN
            ERROR "Marca Activa NO puede ser nula" 
            NEXT FIELD marca_activa
         ELSE 
            SELECT "X"
            FROM cta_convivencia
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa  = g_conviv.marca_activa
            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF
         END IF

         IF g_conviv.convive_cod IS NULL THEN
            ERROR "Codigo de Convivencia NO puede ser nulo" 
            NEXT FIELD convive_cod
         ELSE
            SELECT "X"
            FROM cta_convivencia
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa  = g_conviv.marca_activa
            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF
         END IF

         IF g_conviv.rechazo_cod IS NULL THEN
            ERROR "Codigo de Rechazo NO puede ser nulo"  
            NEXT FIELD rechazo_cod
         ELSE
---
               IF g_conviv.convive_cod > 0 AND
                  g_conviv.rechazo_cod = 0 THEN
                     ERROR "CODIGO DE RECHAZO NO VALIDO"
                     NEXT FIELD rechazo_cod
               END IF
---
            SELECT "X"
            FROM cta_convivencia
            WHERE marca_entra = g_conviv.marca_entra
            AND marca_activa  = g_conviv.marca_activa
            IF STATUS <> NOTFOUND THEN
               ERROR "REGISTRO YA INGRESADO"
               NEXT FIELD marca_activa
            END IF
         END IF

         LET g_conviv.fecha_actualiza = TODAY

         INSERT INTO cta_convivencia  VALUES (g_conviv.marca_entra     ,
       			   	             g_conviv.marca_activa     ,
     			    	             g_conviv.convive_cod      ,
      			    	             g_conviv.rechazo_cod      ,
  				             g_conviv.fecha_actualiza  , 
      				      	     USER )
         ERROR " REGISTRO INGRESADO " SLEEP 2
         ERROR ""

        CLEAR  marca_entra,
               marca_activa,
               convive_cod,
               rechazo_cod,
               vmarca_entra_desc,
               vmarca_activa_desc,
               vconvive_desc,
               vrechazo_desc
              
        CALL Inicializa()

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
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0622" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion           (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                          CATALOGO DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE   

      CONSTRUCT cla_where ON   marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod
                          FROM marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod 
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
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      LET sel_where = "SELECT marca_entra,marca_activa,convive_cod,rechazo_cod ,fecha_actualiza,usuario FROM cta_convivencia WHERE ",
                       cla_where CLIPPED ,"ORDER BY 1 "

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
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL Impresion(pos)
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            ON KEY (control-c)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_3
      ELSE
         ERROR "REGISTRO DE CONVIVENCIA .... NO EXISTE" 
         SLEEP 2
         ERROR "" 
         CLOSE WINDOW ventana_3
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
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0622" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "               Escoja con < ENTER > la convivencia a modificar                          " AT 2,1
      DISPLAY "                          CATALOGO DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON   marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod 
			  FROM marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod
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
         CLOSE WINDOW ventana_3
         RETURN                    
      END IF

      LET sel_where = "SELECT marca_entra,marca_activa,convive_cod,rechazo_cod ,fecha_actualiza,usuario FROM cta_convivencia WHERE ",
                        cla_where CLIPPED,"ORDER BY 1 "                        

      LET sel_where = sel_where CLIPPED   

      PREPARE query6 FROM sel_where

      DECLARE cursor_6 CURSOR FOR query6

      LET pos = 1

      FOREACH cursor_6 
         INTO l_record[pos].*
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL
 
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*  
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_conviv.marca_entra     =l_record[pos].marca_entra
               LET vmarca_entra             =l_record[pos].marca_entra 
               LET g_conviv.marca_activa    =l_record[pos].marca_activa
               LET vmarca_activa            =l_record[pos].marca_activa
               LET g_conviv.convive_cod     =l_record[pos].convive_cod
               LET vconvive_cod             =l_record[pos].convive_cod 
               LET g_conviv.rechazo_cod     =l_record[pos].rechazo_cod
               LET vrechazo_cod             =l_record[pos].rechazo_cod 
               LET g_conviv.fecha_actualiza =l_record[pos].fecha_actualiza
               LET g_conviv.usuario         =l_record[pos].usuario 
               EXIT DISPLAY                    
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro" 
               LET pos = ARR_CURR()     
            ON KEY (control-c)
               ERROR "Usted debe escojer un registro" 
               LET pos = ARR_CURR()                    
          END DISPLAY
          CLOSE WINDOW ventana_3
      ELSE
         ERROR "REGISTRO DE CONVIVENCIA .... NO EXISTE" 
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      SELECT B.marca_desc,
             C.marca_desc,
             D.marca_desc,
             E.rechazo_desc
      INTO   vmarca_entra_desc ,
             vmarca_activa_desc,
             vconvive_desc     ,
             vrechazo_desc
      FROM   cta_convivencia A,
	     tab_marca B,
	     tab_marca C,
	     tab_estado_marca D,
	     tab_rch_marca E
      WHERE  A.marca_entra  = vmarca_entra
      AND    A.marca_activa = vmarca_activa
      AND    A.convive_cod  = vconvive_cod
      AND    A.rechazo_cod  = vrechazo_cod
      AND    A.marca_entra  = B.marca_cod 
      AND    A.marca_activa = C.marca_cod 
      AND    A.convive_cod  = D.marca_estado 
      AND    A.rechazo_cod  = E.rechazo_cod 

      DISPLAY BY NAME g_conviv.marca_entra,
                      vmarca_entra_desc,
                      g_conviv.marca_activa,
                      vmarca_activa_desc,
                      g_conviv.convive_cod,
                      vconvive_desc,
                      g_conviv.rechazo_cod,
                      vrechazo_desc

      INPUT BY NAME g_conviv.marca_entra,
                    g_conviv.marca_activa,
                    g_conviv.convive_cod,
                    g_conviv.rechazo_cod WITHOUT DEFAULTS

         AFTER FIELD marca_entra
            IF g_conviv.marca_entra IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
            DISPLAY "                 CATALOGO DE MARCAS                                             " AT 2,1 ATTRIBUTE(REVERSE)
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

               LET int_flag = FALSE

               CONSTRUCT BY NAME cla_where ON marca_cod
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
                  ERROR "BUSQUEDA CANCELADA..."
                  SLEEP 2
                  ERROR ""
                  CLEAR SCREEN
                  CLOSE WINDOW ventana_2
                  NEXT FIELD marca_entra
               END IF

               LET sel_where = "SELECT      ",
                               " marca_cod ,",
                               " marca_desc ",
                               " FROM tab_marca WHERE ",
                                cla_where CLIPPED ,
                               " ORDER BY 1,2 "

               LET sel_where = sel_where CLIPPED

               PREPARE query8 FROM sel_where

               DECLARE cursor_8 CURSOR FOR query8
                                                                               
               LET pos = 1
               FOREACH cursor_8 INTO l_record2[pos].marca_cod,
                                     l_record2[pos].marca_desc
                   LET pos = pos+1
               END FOREACH

               IF (pos-1) >= 1 THEN
                  CALL SET_COUNT(pos-1)
                  DISPLAY ARRAY l_record2 TO scr_1.*
                     ON KEY (CONTROL-M)
                        LET pos = ARR_CURR()
                        LET g_conviv.marca_entra  =l_record2[pos].marca_cod
                        LET vmarca_entra_desc     =l_record2[pos].marca_desc
                        EXIT DISPLAY
                     ON KEY (INTERRUPT)
                        ERROR "Usted debe escojer un registro" 
                        LET pos = ARR_CURR()                             
                     ON KEY (control-c)
                        ERROR "Usted debe escojer un registro" 
                        LET pos = ARR_CURR()
                  END DISPLAY

                  CLOSE WINDOW ventana_2

                  DISPLAY BY NAME g_conviv.marca_entra,
                                  vmarca_entra_desc

               ELSE
                  ERROR "ARCHIVO DE MARCAS VACIO"
               END IF
               NEXT FIELD marca_activa
            ELSE
               SELECT "X"
               FROM    tab_marca
               WHERE   marca_cod = g_conviv.marca_entra

               IF STATUS = NOTFOUND THEN
                  ERROR "   MARCA INEXISTENTE " 
                  NEXT FIELD marca_entra
               ELSE
                  SELECT marca_desc
                  INTO   vmarca_entra_desc
                  FROM   tab_marca
                  WHERE  marca_cod=g_conviv.marca_entra

                  DISPLAY BY NAME vmarca_entra_desc
                  NEXT FIELD marca_activa
               END IF
            END IF
                                                                               
         AFTER FIELD marca_activa
            IF g_conviv.marca_activa IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
            DISPLAY "                 CATALOGO DE MARCAS                                            " AT 2,1 ATTRIBUTE(REVERSE)
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

               LET int_flag = FALSE

               CONSTRUCT BY NAME cla_where ON marca_cod
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
                  ERROR "BUSQUEDA CANCELADA..."             
                  SLEEP 2
                  ERROR ""
                  CLEAR SCREEN
                  CLOSE WINDOW ventana_2
                  NEXT FIELD marca_activa
               END IF

               LET sel_where = "SELECT      ",
                               " marca_cod ,",
                               " marca_desc ",
                               " FROM tab_marca WHERE ",
                                cla_where CLIPPED ,
                               " ORDER BY 1,2 "

               LET sel_where = sel_where CLIPPED
   
               PREPARE query9 FROM sel_where

               DECLARE cursor_9 CURSOR FOR query9

               LET pos = 1
               FOREACH cursor_9 INTO l_record2[pos].marca_cod,              
                                     l_record2[pos].marca_desc
                  LET pos = pos+1
               END FOREACH

               IF (pos-1) >= 1 THEN
                  CALL SET_COUNT(pos-1)
                  DISPLAY ARRAY l_record2 TO scr_1.*
                     ON KEY (CONTROL-M)
                        LET pos = ARR_CURR()
                        LET g_conviv.marca_activa =l_record2[pos].marca_cod
                        LET vmarca_activa_desc    =l_record2[pos].marca_desc
                        EXIT DISPLAY
                     ON KEY (INTERRUPT)
                        ERROR "Usted debe escojer un registro" 
                        LET pos = ARR_CURR()
                     ON KEY (control-c)
                        ERROR "Usted debe escojer un registro" 
                        LET pos = ARR_CURR()
                  END DISPLAY
                  CLOSE WINDOW ventana_2

                  DISPLAY BY NAME g_conviv.marca_activa,
                                  vmarca_activa_desc

               ELSE
                  ERROR "ARCHIVO DE MARCAS VACIO"
               END IF
               NEXT FIELD convive_cod
   
            ELSE
               SELECT "X"
               FROM    tab_marca
               WHERE   marca_cod = g_conviv.marca_activa

               IF STATUS = NOTFOUND THEN
                  ERROR "   MARCA INEXISTENTE " 
                  NEXT FIELD marca_activa
               ELSE
                  SELECT marca_desc
                  INTO   vmarca_activa_desc
                  FROM   tab_marca
                  WHERE  marca_cod=g_conviv.marca_activa                          
                  DISPLAY BY NAME vmarca_activa_desc
                  NEXT FIELD convive_cod
              END IF
            END IF                                                                
         AFTER FIELD convive_cod
            IF g_conviv.convive_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
            DISPLAY "              CATALOGO ESTADO DE MARCA                                         " AT 2,1 ATTRIBUTE(REVERSE) 
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

               LET int_flag = FALSE

               CONSTRUCT BY NAME cla_where ON marca_cod
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
                  ERROR "BUSQUEDA CANCELADA..."
                  SLEEP 2
                  ERROR ""
                  CLEAR SCREEN
                  CLOSE WINDOW ventana_2
                  NEXT FIELD convive_cod
               END IF

               LET sel_where = "SELECT        ",
                               " marca_estado,",
                               " marca_desc    ",
                               " FROM tab_estado_marca WHERE ",
                                cla_where CLIPPED ,
                               " ORDER BY 1,2 "
   
               LET sel_where = sel_where CLIPPED

               PREPARE query10 FROM sel_where

               DECLARE cursor_10 CURSOR FOR query10
                                                                               
               LET pos = 1
               FOREACH cursor_10 INTO l_record2[pos].marca_cod,
                                      l_record2[pos].marca_desc
                  LET pos = pos+1
               END FOREACH

              IF (pos-1) >= 1 THEN
                 CALL SET_COUNT(pos-1)
                 DISPLAY ARRAY l_record2 TO scr_1.*
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
                       LET g_conviv.convive_cod =l_record2[pos].marca_cod
                       LET vconvive_desc        =l_record2[pos].marca_desc
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                      ERROR "Usted debe escojer un registro" 
                      LET pos = ARR_CURR()
                    ON KEY (control-c)
                       ERROR "Usted debe escojer un registro" 
                       LET pos = ARR_CURR()
                 END DISPLAY                                                     
                 CLOSE WINDOW ventana_2

                 DISPLAY BY NAME g_conviv.convive_cod,
                                 vconvive_desc

              ELSE
                 ERROR "ARCHIVO DE MARCAS VACIO"
              END IF
              NEXT FIELD rechazo_cod

            ELSE
               SELECT "X"
               FROM    tab_estado_marca
               WHERE   marca_estado = g_conviv.convive_cod
  
              IF STATUS = NOTFOUND THEN
                 ERROR "   CODIGO INEXISTENTE "
                 NEXT FIELD convive_cod
              ELSE
                 SELECT marca_desc
                 INTO   vconvive_desc
                 FROM   tab_estado_marca                                            
                 WHERE  marca_estado=g_conviv.convive_cod

                 DISPLAY BY NAME vconvive_desc
                 NEXT FIELD rechazo_cod
              END IF
            END IF                                                                
         AFTER FIELD rechazo_cod
            IF g_conviv.rechazo_cod IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0623" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT 1,1
            DISPLAY "                 CATALOGO DE RECHAZOS                                            " AT 2,1 ATTRIBUTE(REVERSE)   
            DISPLAY "      <F3> Av. Pagina           <F4> Re. Pagina" AT 13,1

               LET int_flag = FALSE

               CONSTRUCT BY NAME cla_where ON marca_cod
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
                  ERROR "BUSQUEDA CANCELADA..."                         
                  SLEEP 2
                  ERROR ""
                  CLEAR SCREEN
                  CLOSE WINDOW ventana_2
                  NEXT FIELD convive_cod
               END IF

               LET sel_where = "SELECT        ",
                               " rechazo_cod ,",
                               " rechazo_desc ",
                               " FROM tab_rch_marca WHERE ",
                                cla_where CLIPPED ,
                               " ORDER BY 1,2 "

              LET sel_where = sel_where CLIPPED

              PREPARE query11 FROM sel_where

              DECLARE cursor_11 CURSOR FOR query11

              LET pos = 1                                                        
              FOREACH cursor_11 INTO l_record2[pos].marca_cod,
                                     l_record2[pos].marca_desc
                  LET pos = pos+1
              END FOREACH

              IF (pos-1) >= 1 THEN
                 CALL SET_COUNT(pos-1)
                 DISPLAY ARRAY l_record2 TO scr_1.*
                    ON KEY (CONTROL-M)
                       LET pos = ARR_CURR()
                       LET g_conviv.rechazo_cod =l_record2[pos].marca_cod
                       LET vrechazo_desc        =l_record2[pos].marca_desc
                       EXIT DISPLAY
                    ON KEY (INTERRUPT)
                       ERROR "Usted debe escojer un registro" 
                       LET pos = ARR_CURR()
                    ON KEY (control-c)
                       ERROR "Usted debe escojer un registro" 
                       LET pos = ARR_CURR()
                 END DISPLAY
                 CLOSE WINDOW ventana_2

                 DISPLAY BY NAME g_conviv.rechazo_cod,
                                 vrechazo_desc

              ELSE
                 ERROR "ARCHIVO DE MARCAS VACIO"
              END IF
--              NEXT FIELD marca_entra

            ELSE
               SELECT "X"
               FROM    tab_rch_marca
               WHERE  rechazo_cod = g_conviv.rechazo_cod

               IF STATUS = NOTFOUND THEN
                   ERROR "  CODIGO DE RECHAZO INEXISTENTE " 
                  NEXT FIELD rechazo_cod
               ELSE
                  SELECT rechazo_desc
                  INTO   vrechazo_desc
                  FROM   tab_rch_marca
                  WHERE  rechazo_cod=g_conviv.rechazo_cod                         

                  DISPLAY BY NAME vrechazo_desc
--                  NEXT FIELD marca_entra
               END IF
            END IF                                                                
         PROMPT "Esta seguro S/N ? "
            FOR CHAR aux_pausa

         IF aux_pausa MATCHES "[sS]" THEN
            UPDATE cta_convivencia
               SET marca_entra     = g_conviv.marca_entra,
                   marca_activa    = g_conviv.marca_activa,
                   convive_cod     = g_conviv.convive_cod,
                   rechazo_cod     = g_conviv.rechazo_cod,
                   fecha_actualiza = g_conviv.fecha_actualiza
               WHERE marca_entra   = vmarca_entra
               AND   marca_activa  = vmarca_activa
               AND   convive_cod   = vconvive_cod
               AND   rechazo_cod   = vrechazo_cod  

            IF SQLCA.SQLCODE != 0 then
               ERROR "Error en la Actualizacion de Convivencia"
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
            INITIALIZE g_conviv.* TO NULL
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
      ERROR "ARCHIVO DE CONVIVENCIA VACIO"
   END IF
   CLEAR SCREEN      
END FUNCTION
################################################################################
FUNCTION Elimina()
#e----------------
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0622" ATTRIBUTE(BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "           Escoja con < ENTER > el rechazo de convivencia a eliminar         " AT 2,1 
      DISPLAY "                          CATALOGO DE CONVIVENCIA                              " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON   marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod
			  FROM marca_entra,
                               marca_activa,
                               convive_cod,
                               rechazo_cod

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
         CLOSE WINDOW ventana_3
         RETURN
      END IF         

      LET sel_where = "SELECT marca_entra,marca_activa,convive_cod,rechazo_cod,fecha_actualiza,usuario FROM cta_convivencia WHERE ",
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
               LET g_conviv.marca_entra     =l_record[pos].marca_entra
               LET vmarca_entra             =l_record[pos].marca_entra
               LET g_conviv.marca_activa    =l_record[pos].marca_activa
               LET vmarca_activa            =l_record[pos].marca_activa 
               LET g_conviv.convive_cod     =l_record[pos].convive_cod
               LET vconvive_cod             =l_record[pos].convive_cod 
               LET g_conviv.rechazo_cod     =l_record[pos].rechazo_cod
               LET vrechazo_cod             =l_record[pos].rechazo_cod       
               LET g_conviv.fecha_actualiza =l_record[pos].fecha_actualiza
               LET g_conviv.usuario         =l_record[pos].usuario 
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
            ON KEY (control-c)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
          END DISPLAY
          CLOSE WINDOW ventana_3      
      ELSE
         ERROR "REGISTRO DE RECHAZO DE CONVIVENCIA .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF    

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1      
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE)

      SELECT B.marca_desc,
             C.marca_desc,

             D.marca_desc,
             E.rechazo_desc
      INTO   vmarca_entra_desc ,
             vmarca_activa_desc,
             vconvive_desc     ,
             vrechazo_desc
      FROM   cta_convivencia A,
	     tab_marca B,
	     tab_marca C,
	     tab_estado_marca D,
	     tab_rch_marca E
      WHERE  A.marca_entra  = vmarca_entra
      AND    A.marca_activa = vmarca_activa
      AND    A.convive_cod  = vconvive_cod
      AND    A.rechazo_cod  = vrechazo_cod
      AND    A.marca_entra  = B.marca_cod
      AND    A.marca_activa = C.marca_cod
      AND    A.convive_cod  = D.marca_estado                                   
      AND    A.rechazo_cod  = E.rechazo_cod 

      DISPLAY BY NAME g_conviv.marca_entra,
                      vmarca_entra_desc,
                      g_conviv.marca_activa,
                      vmarca_activa_desc,
                      g_conviv.convive_cod,
                      vconvive_desc,
                      g_conviv.rechazo_cod,
                      vrechazo_desc

      CALL Pregunta() 
 
      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE
         FROM cta_convivencia
         WHERE marca_entra   = vmarca_entra
         AND   marca_activa  = vmarca_activa
         AND   convive_cod   = vconvive_cod
         AND   rechazo_cod   = vrechazo_cod                              

         IF SQLCA.SQLCODE != 0 THEN
            ERROR "Error en la eliminacion de Convivencia"
            ATTRIBUTE (reverse)
         ELSE
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
      ERROR "REGISTRO DE CONVIVENCIA .... NO EXISTE"
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
                  ".IMPCONVIVEN",hoy USING "dd-mm-yyyy","_",hora CLIPPED

   START REPORT rpt_cta_convivencia TO g_impre

   FOR i=1 TO (pos+1)
       LET g_conviv.marca_entra     =l_record[i].marca_entra
       LET g_conviv.marca_activa    =l_record[i].marca_activa
       LET g_conviv.convive_cod     =l_record[i].convive_cod
       LET g_conviv.rechazo_cod     =l_record[i].rechazo_cod
       LET g_conviv.fecha_actualiza =l_record[i].fecha_actualiza
       LET g_conviv.usuario         =l_record[i].usuario

       IF g_conviv.marca_entra IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_cta_convivencia (g_conviv.*)
   END FOR

   FINISH REPORT rpt_cta_convivencia

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_cta_convivencia(g_conviv)

   DEFINE g_conviv  RECORD LIKE cta_convivencia.*

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
         PRINT COLUMN 02," TABM062 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'    

         PRINT COLUMN 35,"CATALOGO DE CONVIVENCIA"

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         SKIP 2 LINE         

         PRINT COLUMN 05,"Marca Entrante",
               COLUMN 26,"Marca Activa",
               COLUMN 45,"Codigo Convivencia",
               COLUMN 71,"Codigo Rechazo",
               COLUMN 90,"Fecha Actualiza",
               COLUMN 110,"Usuario"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         PRINT COLUMN 07,g_conviv.marca_entra     ,
               COLUMN 26,g_conviv.marca_activa    ,
               COLUMN 47,g_conviv.convive_cod     ,
               COLUMN 73,g_conviv.rechazo_cod     ,
               COLUMN 92,g_conviv.fecha_actualiza ,
               COLUMN 111,g_conviv.usuario         
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT          
