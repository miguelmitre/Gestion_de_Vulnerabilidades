###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa TABM063  => CATALOGO DE MARCAS                                  # 
#Fecha             => 11 DE MARZO DE 2002  	                          #
#Modificado        => STEFANIE VERA PIÑA                                  #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                          #
#Sistema           => TAB                                                 #
###########################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_param_dis          RECORD LIKE seg_modulo.* 

   DEFINE        aux_pausa             CHAR(1)

   DEFINE g_marca  RECORD LIKE tab_marca.*

   DEFINE vmarca_desc           CHAR(50),
          vind_habilita_desc    CHAR(40),
          vmarca_resulta_desc   CHAR(40),
          vind_saldo_desc       CHAR(40)

   DEFINE g_marca1  RECORD
          marca_cod            SMALLINT,
          marca_desc           CHAR(40),
          ind_habilita         SMALLINT,
          ind_habilita_desc    CHAR(40),
          marca_resulta        SMALLINT,
          marca_resulta_desc   CHAR(40),
          ind_saldo            SMALLINT,
          ind_saldo_desc       CHAR(40)
   END RECORD

   DEFINE l_record ARRAY[1000] OF RECORD
          marca_cod            SMALLINT,
          marca_desc           CHAR(50),
          ind_habilita         SMALLINT,
          marca_resulta        SMALLINT,
          ind_saldo            SMALLINT,
          fecha_actualiza      DATE,
          usuario              CHAR(8)
   END RECORD

   DEFINE l_record8 ARRAY[1000] OF RECORD
          marca_cod            SMALLINT,
          ind_habilita         SMALLINT,
          marca_resulta        SMALLINT,
          ind_saldo            SMALLINT
   END RECORD

   DEFINE pos                  SMALLINT                      

   DEFINE bandera              ,
          sw_1                 SMALLINT

   DEFINE usuario              CHAR(08),
          HOY                  DATE,
          USR                  CHAR(8),   
          cla_where            CHAR(200),
          sel_where            CHAR(200),
          g_lista              CHAR(300),
          g_impre              CHAR(300),
          hora                 CHAR(08) 

   DEFINE l_record2  ARRAY[3000] OF RECORD
          ind_habilita         SMALLINT,
          descripcion          CHAR(40)
   END RECORD

   DEFINE l_record3 ARRAY[3000] OF RECORD
          ind_saldo            SMALLINT,
          descripcion          CHAR(40)
   END RECORD
   
   DEFINE l_record4 ARRAY[3000] OF RECORD
          marca_cod            SMALLINT,
          marca_desc           CHAR(40)
   END RECORD
  
   DEFINE l_record5 ARRAY[3000] OF RECORD
          marca_cod            SMALLINT,
          marca_desc           CHAR(40)
   END RECORD

   DEFINE errvar               CHAR(80),
          error                CHAR(10)


END GLOBALS

################################################################################
MAIN
   CALL startlog("/home/jmendoza/TABM063.log")   
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY control-o
   DEFER INTERRUPT

   CALL inic() 

   LET HOY = TODAY
   LET bandera = FALSE

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0631" ATTRIBUTE( BORDER)
   DISPLAY " TABM063                     CATALOGO DE MARCAS                                        " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
	
   MENU "MARCAS"       
      COMMAND "Agrega" "Agrega MARCA"
         CALL Agrega() #a
      COMMAND "Consulta" "Consulta MARCA"
         CALL Consulta() #c
      COMMAND "Modifica" "Modifica MARCA"
         CALL Modifica() #m
      COMMAND "Elimina" "Elimina MARCA"
         CALL Elimina() #e
      COMMAND KEY(S) "Salir" "Salir del Programa"
      EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION inic()
   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"
END FUNCTION    
################################################################################
FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE g_marca1.* TO NULL

   INITIALIZE vind_habilita_desc,
              vmarca_resulta_desc,
              vind_saldo_desc TO NULL

   DISPLAY BY NAME g_marca1.marca_cod,
                   g_marca1.marca_desc,
                   g_marca1.ind_habilita,
                   vind_habilita_desc,
                   g_marca1.marca_resulta,
                   vmarca_resulta_desc,
                   g_marca1.ind_saldo,
                   vind_saldo_desc

END FUNCTION
################################################################################
FUNCTION Agrega()
  
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                   (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET g_marca1.marca_cod       = NULL
   LET g_marca1.marca_desc      = NULL
   LET g_marca1.marca_resulta   = NULL
   LET g_marca1.ind_habilita    = NULL
   LET g_marca1.ind_saldo       = NULL   

   LET sw_1 = 0

   INPUT BY NAME g_marca1.marca_cod,
                 g_marca1.marca_desc,
                 g_marca1.ind_habilita,
                 g_marca1.marca_resulta,
                 g_marca1.ind_saldo WITHOUT DEFAULTS

      AFTER FIELD marca_cod
         IF g_marca1.marca_cod IS NULL THEN 
            ERROR "CODIGO DE MARCA NO PUEDE SER NULO"
            NEXT FIELD marca_cod
         END IF

         SELECT "X" 
            FROM    tab_marca
            WHERE   marca_cod = g_marca1.marca_cod

         IF STATUS <> NOTFOUND THEN
            ERROR "Codigo Ya Ingresado"
            SLEEP 1 
            ERROR "     "  
            NEXT FIELD marca_cod
         END IF 

      AFTER FIELD marca_desc
         IF g_marca1.marca_desc IS NULL THEN
            ERROR "Descripcion de MARCA NO puede ser nulo"
            NEXT FIELD marca_desc
	 END IF 

      AFTER FIELD ind_habilita
         IF g_marca1.ind_habilita IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0633" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT 1,1
            DISPLAY "                 CATALOGO DE MARCAS                                              " AT 2,1 ATTRIBUTE(REVERSE)
      

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON ind_habilita
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
               NEXT FIELD ind_habilita
            END IF

            LET sel_where = "SELECT      ",
                            " ind_habilita ,",
                            " descripcion ",
                            " FROM tab_ind_habilita WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query1 FROM sel_where

            DECLARE cursor_1 CURSOR FOR query1

            LET pos = 1
            FOREACH cursor_1 INTO l_record2[pos].ind_habilita,
                                  l_record2[pos].descripcion
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_marca1.ind_habilita =l_record2[pos].ind_habilita
                     LET vind_habilita_desc    =l_record2[pos].descripcion
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY

               IF g_marca1.ind_habilita = 0 THEN
                  LET g_marca1.marca_resulta = 0
                  SELECT descripcion
                  INTO   vind_habilita_desc
                  FROM   tab_ind_habilita
                  WHERE  ind_habilita = g_marca1.ind_habilita

                  SELECT marca_desc
                  INTO   vmarca_resulta_desc
                  FROM   tab_marca
                  WHERE  marca_cod = g_marca1.marca_resulta

                  CLOSE WINDOW ventana_2

                  DISPLAY BY NAME g_marca1.ind_habilita,
                                  vind_habilita_desc,
                                  g_marca1.marca_resulta,
                                  vmarca_resulta_desc
                  NEXT FIELD ind_saldo
               END IF

               CLOSE WINDOW ventana_2
               DISPLAY BY NAME g_marca1.ind_habilita,
                               vind_habilita_desc
            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
            END IF
            NEXT FIELD marca_resulta
         ELSE   
            IF g_marca1.ind_habilita = 0 THEN
               LET g_marca1.marca_resulta = 0
                 SELECT descripcion
                 INTO   vind_habilita_desc
                 FROM   tab_ind_habilita
                 WHERE  ind_habilita = g_marca1.ind_habilita

                 SELECT marca_desc
                 INTO   vmarca_resulta_desc
                 FROM   tab_marca
                 WHERE  marca_cod = g_marca1.marca_resulta
   
               DISPLAY BY NAME g_marca1.ind_habilita,
                               vind_habilita_desc,
                               g_marca1.marca_resulta,
                               vmarca_resulta_desc 
               NEXT FIELD ind_saldo
            END IF         

            SELECT "X"
            FROM   tab_ind_habilita
            WHERE  ind_habilita = g_marca1.ind_habilita

            IF STATUS = NOTFOUND THEN
               ERROR "   MARCA INEXISTENTE "
               NEXT FIELD ind_habilita
            ELSE
                 SELECT descripcion
                 INTO   vind_habilita_desc
                 FROM   tab_ind_habilita
                 WHERE  ind_habilita = g_marca1.ind_habilita

                DISPLAY BY NAME g_marca1.ind_habilita,
                                vind_habilita_desc
                NEXT FIELD marca_resulta
            END IF
         END IF

      AFTER FIELD marca_resulta
         IF g_marca1.ind_habilita > 0 AND
            g_marca1.marca_resulta = 0 THEN
            ERROR "MARCA RESULTA NO PUEDE SER CERO"
            NEXT FIELD marca_resulta
         END IF

         IF g_marca1.marca_resulta IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0635" ATTRIBUTE (BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir "AT 1,1
            DISPLAY "                 CODIGO DE MARCAS                                                " AT 2,1 ATTRIBUTE(REVERSE)

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
               NEXT FIELD marca_cod
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
            FOREACH cursor_8 INTO l_record4[pos].marca_cod,
                                  l_record4[pos].marca_desc
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record4 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_marca1.marca_resulta =l_record4[pos].marca_cod
                     LET vmarca_resulta_desc    =l_record4[pos].marca_desc
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY
                  IF g_marca1.marca_resulta = 0 THEN
                  ERROR "MARCA RESULTANTE NO PUEDE SER CERO"

                  CLOSE WINDOW ventana_2

                  NEXT FIELD marca_resulta
               END IF
               CLOSE WINDOW ventana_2
               DISPLAY BY NAME g_marca1.marca_resulta,
                               vmarca_resulta_desc

            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
            END IF
            NEXT FIELD ind_saldo
         ELSE
            SELECT "X"
            FROM   tab_marca
            WHERE  marca_cod = g_marca1.marca_resulta

            IF STATUS = NOTFOUND THEN
               ERROR "CODIGO DE MARCA INEXISTENTE "
               NEXT FIELD marca_resulta
            ELSE
                 SELECT marca_desc
                 INTO   vmarca_resulta_desc
                 FROM   tab_marca
                 WHERE  marca_cod = g_marca1.marca_resulta

                DISPLAY BY NAME g_marca1.marca_resulta,
                                vmarca_resulta_desc
                NEXT FIELD ind_saldo
            END IF
         END IF

         AFTER FIELD ind_saldo
         IF g_marca1.ind_saldo IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0634" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir "AT 1,1
            DISPLAY "                 INDICADOR DE SALDO                                              " AT 2,1 ATTRIBUTE(REVERSE)

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON ind_saldo
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
               NEXT FIELD ind_saldo
            END IF
            LET sel_where = "SELECT      ",
                            " ind_saldo ,",
                            " descripcion ",
                            " FROM tab_ind_saldo WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query2 FROM sel_where

            DECLARE cursor_2 CURSOR FOR query2

            LET pos = 1
            FOREACH cursor_2 INTO l_record3[pos].ind_saldo,
                                  l_record3[pos].descripcion
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_marca1.ind_saldo =l_record3[pos].ind_saldo
                     LET vind_saldo_desc    =l_record3[pos].descripcion
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ventana_2

               SELECT descripcion
               INTO   vind_saldo_desc
               FROM   tab_ind_saldo
               WHERE  ind_saldo = g_marca1.ind_saldo

               DISPLAY BY NAME g_marca1.ind_saldo,
                               vind_saldo_desc

            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
            END IF
            NEXT FIELD ind_saldo   --marca_cod
         ELSE
            SELECT "X"
            FROM   tab_ind_saldo
            WHERE  ind_saldo = g_marca1.ind_saldo

            IF STATUS = NOTFOUND THEN
               ERROR "INDICADOR DE SALDO INEXISTENTE "
               NEXT FIELD ind_saldo
            ELSE
                 SELECT descripcion
                 INTO   vind_saldo_desc
                 FROM   tab_ind_saldo
                 WHERE  ind_saldo = g_marca1.ind_saldo

                DISPLAY BY NAME g_marca1.ind_saldo,
                                vind_saldo_desc
                NEXT FIELD marca_cod
            END IF
         END IF

      ON KEY ( ESC )
         IF g_marca1.marca_cod IS NULL THEN
            ERROR "CODIGO DE MARCA NO PUEDE SER NULO."
            NEXT FIELD marca_cod
         END IF

         IF g_marca1.marca_desc IS NULL THEN
            ERROR "DESCRIPCION DE MARCA NO PUEDE SER NULO."
            NEXT FIELD marca_desc
         END IF

         IF g_marca1.marca_resulta IS NULL THEN
            ERROR "MARCA RESULTANTE NO PUEDE SER NULO."
            NEXT FIELD marca_resulta
         END IF

         IF g_marca1.ind_habilita IS NULL THEN
            ERROR "INDICADOR HABILITA NO PUEDE SER NULO."
            NEXT FIELD ind_habilita
         END IF

         IF g_marca1.ind_saldo IS NULL THEN
            ERROR "INDICADOR SALDO NO PUEDE SER NULO."
            NEXT FIELD ind_saldo
         END IF

         LET g_marca.fecha_actualiza = TODAY
         INSERT INTO tab_marca VALUES (g_marca1.marca_cod       ,
                                       g_marca1.marca_desc      ,
                                       g_marca1.marca_resulta   ,
                                       g_marca1.ind_habilita    ,
                                       g_marca1.ind_saldo,       
                                       g_marca.fecha_actualiza ,
                                       USER)
         IF SQLCA.SQLCODE != 0 THEN
            CALL err_print(SQLCA.SQLCODE)

            LET error = SQLCA.SQLCODE
            LET errvar = error CLIPPED, err_get(SQLCA.SQLCODE)
            CALL errorlog(errvar CLIPPED)
            ERROR errvar CLIPPED
            PROMPT "" FOR aux_pausa
         ELSE
            WHENEVER ERROR STOP
            ERROR "REGISTRO INGRESADO."
            SLEEP 2
            CLEAR FORM
         END IF

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
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0632" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta              (Ctrol-p) Impresion           (Ctrl-c) Salir    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                          CATALOGO DE MARCAS                                   " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE   

      CONSTRUCT cla_where ON marca_cod,
                             marca_resulta,
                             ind_habilita,
                             ind_saldo
                        FROM marca_cod,
                             marca_resulta,
                             ind_habilita,
                             ind_saldo
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

      LET sel_where = "SELECT marca_cod,",
                             "ind_habilita,",
                             "marca_resulta,",
                             "ind_saldo",
                      " FROM tab_marca  ",
                      " WHERE ",cla_where CLIPPED ,
                      "ORDER BY 1 " 
      PREPARE query3 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query3

      LET pos = 1

      FOREACH cursor_3 INTO l_record8[pos].*

         SELECT marca_desc
         INTO   l_record4[pos].marca_desc 
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_cod

         SELECT descripcion
         INTO   l_record2[pos].descripcion 
         FROM   tab_ind_habilita
         WHERE  ind_habilita = l_record8[pos].ind_habilita

         SELECT marca_desc
         INTO   l_record5[pos].marca_desc 
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_resulta

         SELECT descripcion
         INTO   l_record3[pos].descripcion 
         FROM   tab_ind_saldo
         WHERE  ind_saldo = l_record8[pos].ind_saldo

         LET pos = pos + 1    
      END FOREACH

      INITIALIZE l_record8[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)   
         DISPLAY ARRAY l_record8 TO scr_1.*          
            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL Impresion(pos)

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_marca1.marca_cod          = l_record8[pos].marca_cod
               LET g_marca1.marca_desc         = l_record4[pos].marca_desc
               LET g_marca1.ind_habilita       = l_record8[pos].ind_habilita
               LET vind_habilita_desc          = l_record2[pos].descripcion
               LET g_marca1.marca_resulta      = l_record8[pos].marca_resulta
               LET vmarca_resulta_desc         = l_record5[pos].marca_desc
               LET g_marca1.ind_saldo          = l_record8[pos].ind_saldo
               LET vind_saldo_desc             = l_record3[pos].descripcion
               EXIT DISPLAY

            ON KEY (control-c)
               ERROR "Usted debe escojer un registro"
               LET pos = ARR_CURR()
         END DISPLAY

         CLOSE WINDOW ventana_3

      ELSE
         ERROR "REGISTRO DE .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF                  

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         RETURN
      END IF

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " (Ctrl-C) Salir " AT 1,1
   DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)

   DISPLAY BY NAME g_marca1.marca_cod,
                   g_marca1.marca_desc,
                   g_marca1.ind_habilita,
                   vind_habilita_desc,
                   g_marca1.marca_resulta,
                   vmarca_resulta_desc,
                   g_marca1.ind_saldo,
                   vind_saldo_desc

   PROMPT " Oprima ...<ENTER>... Para salir " ATTRIBUTE (REVERSE)
     FOR CHAR aux_pausa ATTRIBUTE (REVERSE)
     CALL Inicializa()              
   ELSE
     ERROR "ARCHIVO VACIO"
   END IF
   ERROR ""
END FUNCTION
################################################################################
FUNCTION Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0632" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Modifica                                            (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                Escoja con < ENTER > la marca a modificar                                " AT 2,1
      DISPLAY "                          CATALOGO DE MARCAS                                   " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON marca_cod,
                             ind_habilita,
                             marca_resulta,
                             ind_saldo
                        FROM marca_cod,
                             ind_habilita,
                             marca_resulta,
                             ind_saldo
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_3
         RETURN                    
      END IF
 
      LET sel_where = "SELECT marca_cod,",
                             "ind_habilita,",
                             "marca_resulta,",
                             "ind_saldo ",
                      " FROM tab_marca",
                      " WHERE ",cla_where CLIPPED ,
                      " ORDER BY 1 "

      PREPARE query4 FROM sel_where

      DECLARE cursor_4 CURSOR FOR query4

      LET pos = 1

      FOREACH cursor_4 INTO l_record8[pos].*

         SELECT marca_desc
         INTO   l_record4[pos].marca_desc 
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_cod

         SELECT a.descripcion
         INTO   l_record2[pos].descripcion
         FROM   tab_ind_habilita a
         WHERE  a.ind_habilita = l_record8[pos].ind_habilita

         SELECT marca_desc
         INTO   l_record5[pos].marca_desc
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_resulta

         SELECT descripcion
         INTO   l_record3[pos].descripcion
         FROM   tab_ind_saldo
         WHERE  ind_saldo = l_record8[pos].ind_saldo  

         LET pos = pos + 1
      END FOREACH  

      INITIALIZE l_record8[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record8 TO scr_1.*  
            ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET g_marca1.marca_cod          = l_record8[pos].marca_cod
            LET g_marca1.marca_desc         = l_record4[pos].marca_desc
            LET g_marca1.ind_habilita       = l_record8[pos].ind_habilita
            LET vind_habilita_desc          = l_record2[pos].descripcion
            LET g_marca1.marca_resulta      = l_record8[pos].marca_resulta
            LET vmarca_resulta_desc         = l_record5[pos].marca_desc
            LET g_marca1.ind_saldo          = l_record8[pos].ind_saldo
            LET vind_saldo_desc             = l_record3[pos].descripcion
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
         ERROR "REGISTRO DE .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_marca1.marca_cod,
                      g_marca1.marca_desc,
                      g_marca1.ind_habilita,
                      vind_habilita_desc,
                      g_marca1.marca_resulta,
                      vmarca_resulta_desc,
                      g_marca1.ind_saldo,
                      vind_saldo_desc

      INPUT BY NAME g_marca1.marca_desc,
                    g_marca1.ind_habilita,
                    g_marca1.marca_resulta,
                    g_marca1.ind_saldo WITHOUT DEFAULTS


         AFTER FIELD marca_desc
            IF g_marca1.marca_desc IS NULL THEN
               ERROR "Descripcion de MARCA NO puede ser nulo"
               NEXT FIELD marca_desc
            END IF

         AFTER FIELD ind_habilita
            IF g_marca1.ind_habilita IS NULL THEN
               OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0633" ATTRIBUTE(BORDER)            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir " AT
1,1
               DISPLAY "                 CATALOGO DE MARCAS                                              " AT 2,1 ATTRIBUTE(REVERSE)

               LET int_flag = FALSE

               CONSTRUCT BY NAME cla_where ON ind_habilita
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
                  NEXT FIELD ind_habilita
               END IF

               LET sel_where = "SELECT      ",
                            " ind_habilita ,",
                            " descripcion ",
                            " FROM tab_ind_habilita WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

               LET sel_where = sel_where CLIPPED

               PREPARE query5 FROM sel_where

               DECLARE cursor_5 CURSOR FOR query5

               LET pos = 1
               FOREACH cursor_5 INTO l_record2[pos].ind_habilita,
                                     l_record2[pos].descripcion
                  LET pos = pos+1
               END FOREACH

               IF (pos-1) >= 1 THEN
                  CALL SET_COUNT(pos-1)
                  DISPLAY ARRAY l_record2 TO scr_1.*
                     ON KEY (CONTROL-M)
                        LET pos = ARR_CURR()
                        LET g_marca1.ind_habilita =l_record2[pos].ind_habilita
                        LET vind_habilita_desc    =l_record2[pos].descripcion
                        EXIT DISPLAY
                     ON KEY (INTERRUPT)
                        ERROR "Usted debe escojer un registro"
                        LET pos = ARR_CURR()
                     ON KEY (control-c)
                        ERROR "Usted debe escojer un registro"
                        LET pos = ARR_CURR()
                  END DISPLAY

                  IF g_marca1.ind_habilita = 0 THEN
                     LET g_marca1.marca_resulta = 0
                     SELECT descripcion
                     INTO   vind_habilita_desc
                     FROM   tab_ind_habilita
                     WHERE  ind_habilita = g_marca1.ind_habilita

                     SELECT marca_desc
                     INTO   vmarca_resulta_desc
                     FROM   tab_marca
                     WHERE  marca_cod = g_marca1.marca_resulta
                     CLOSE WINDOW ventana_2

                     DISPLAY BY NAME g_marca1.ind_habilita,
                                     vind_habilita_desc,
                                     g_marca1.marca_resulta,
                                     vmarca_resulta_desc
                     NEXT FIELD ind_saldo
                  END IF

                  CLOSE WINDOW ventana_2
                  DISPLAY BY NAME g_marca1.ind_habilita,
                                  vind_habilita_desc
               ELSE
                  ERROR "ARCHIVO DE MARCAS VACIO"
               END IF   
               NEXT FIELD marca_resulta
            ELSE
               IF g_marca1.ind_habilita = 0 THEN
                  LET g_marca1.marca_resulta = 0
                  SELECT descripcion
                  INTO   vind_habilita_desc
                  FROM   tab_ind_habilita
                  WHERE  ind_habilita = g_marca1.ind_habilita

                  SELECT marca_desc
                  INTO   vmarca_resulta_desc
                  FROM   tab_marca
                  WHERE  marca_cod = g_marca1.marca_resulta

                  DISPLAY BY NAME g_marca1.ind_habilita,
                                  vind_habilita_desc,
                                  g_marca1.marca_resulta,
                                  vmarca_resulta_desc
                  NEXT FIELD ind_saldo
               END IF

               SELECT "X"
               FROM   tab_ind_habilita
               WHERE  ind_habilita = g_marca1.ind_habilita

               IF STATUS = NOTFOUND THEN
                  ERROR "   MARCA INEXISTENTE "
                  NEXT FIELD ind_habilita
               ELSE
                  SELECT descripcion
                  INTO   vind_habilita_desc
                  FROM   tab_ind_habilita
                  WHERE  ind_habilita = g_marca1.ind_habilita

                  DISPLAY BY NAME g_marca1.ind_habilita,
                                 vind_habilita_desc
                  NEXT FIELD marca_resulta
               END IF
            END IF   --de ind_habilita

         AFTER FIELD marca_resulta
            IF g_marca1.ind_habilita > 0 AND
               g_marca1.marca_resulta = 0 THEN
               ERROR "MARCA RESULTA NO PUEDE SER CERO"
               NEXT FIELD marca_resulta
            END IF

            IF g_marca1.marca_resulta IS NULL THEN
               OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0635" ATTRIBUTE (BORDER
)
               DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir "AT 1
,1
               DISPLAY "                 CODIGO DE MARCAS                                               " AT 2,1 ATTRIBUTE(REVERSE)

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
                  NEXT FIELD marca_cod
               END IF
               LET sel_where = "SELECT      ",
                            " marca_cod ,",
                            " marca_desc ",
                            " FROM tab_marca WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

               LET sel_where = sel_where CLIPPED

               PREPARE query6 FROM sel_where

               DECLARE cursor_6 CURSOR FOR query6

               LET pos = 1
               FOREACH cursor_6 INTO l_record4[pos].marca_cod,
                                     l_record4[pos].marca_desc
                  LET pos = pos+1
               END FOREACH

               IF (pos-1) >= 1 THEN
                  CALL SET_COUNT(pos-1)
                  DISPLAY ARRAY l_record4 TO scr_1.*
                     ON KEY (CONTROL-M)
                        LET pos = ARR_CURR()
                        LET g_marca1.marca_resulta =l_record4[pos].marca_cod
                        LET vmarca_resulta_desc    =l_record4[pos].marca_desc
                        EXIT DISPLAY
                     ON KEY (INTERRUPT)
                        ERROR "Usted debe escojer un registro"
                        LET pos = ARR_CURR()
                     ON KEY (control-c)
                        ERROR "Usted debe escojer un registro"
                        LET pos = ARR_CURR()
                  END DISPLAY
                  IF g_marca1.marca_resulta = 0 THEN
                     ERROR "MARCA RESULTANTE NO PUEDE SER CERO"
                     CLOSE WINDOW ventana_2
                     NEXT FIELD marca_resulta
                  END IF
                  CLOSE WINDOW ventana_2
                  DISPLAY BY NAME g_marca1.marca_resulta,
                                  vmarca_resulta_desc
               ELSE
                  ERROR "ARCHIVO DE MARCAS VACIO"
               END IF
               NEXT FIELD ind_saldo
            ELSE
               SELECT "X"
               FROM   tab_marca
               WHERE  marca_cod = g_marca1.marca_resulta

               IF STATUS = NOTFOUND THEN
                  ERROR "CODIGO DE MARCA INEXISTENTE "
                  NEXT FIELD marca_resulta
               ELSE
                  SELECT marca_desc
                  INTO   vmarca_resulta_desc
                  FROM   tab_marca
                  WHERE  marca_cod = g_marca1.marca_resulta

                  DISPLAY BY NAME g_marca1.marca_resulta,
                                  vmarca_resulta_desc
                  NEXT FIELD ind_saldo
               END IF
            END IF

         AFTER FIELD ind_saldo
         IF g_marca1.ind_saldo IS NULL THEN
            OPEN WINDOW ventana_2 AT 7,15 WITH FORM "TABM0634" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) Consulta                      (Ctrl-C) Salir "AT 1
,1
            DISPLAY "                 INDICADOR DE SALDO                                            " AT 2,1 ATTRIBUTE(REVERSE)

            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON ind_saldo
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
               NEXT FIELD ind_saldo
            END IF
            LET sel_where = "SELECT      ",
                            " ind_saldo ,",
                            " descripcion ",
                            " FROM tab_ind_saldo WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1,2 "

            LET sel_where = sel_where CLIPPED

            PREPARE query7 FROM sel_where

            DECLARE cursor_7 CURSOR FOR query7

            LET pos = 1
            FOREACH cursor_7 INTO l_record3[pos].ind_saldo,
                                  l_record3[pos].descripcion
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_marca1.ind_saldo =l_record3[pos].ind_saldo
                     LET vind_saldo_desc    =l_record3[pos].descripcion
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ventana_2

               SELECT descripcion
               INTO   vind_saldo_desc
               FROM   tab_ind_saldo
               WHERE  ind_saldo = g_marca1.ind_saldo

               DISPLAY BY NAME g_marca1.ind_saldo,
                               vind_saldo_desc

            ELSE
               ERROR "ARCHIVO DE MARCAS VACIO"
            END IF
            NEXT FIELD ind_saldo   --marca_cod
         ELSE
            SELECT "X"
            FROM   tab_ind_saldo
            WHERE  ind_saldo = g_marca1.ind_saldo

            IF STATUS = NOTFOUND THEN
               ERROR "INDICADOR DE SALDO INEXISTENTE "
               NEXT FIELD ind_saldo
            ELSE
                 SELECT descripcion
                 INTO   vind_saldo_desc
                 FROM   tab_ind_saldo
                 WHERE  ind_saldo = g_marca1.ind_saldo

                DISPLAY BY NAME g_marca1.ind_saldo,
                                vind_saldo_desc
            END IF
         END IF

         PROMPT "Esta seguro S/N ? "
         FOR CHAR aux_pausa
 
         IF aux_pausa MATCHES "[sS]" THEN
            WHENEVER ERROR CONTINUE
            UPDATE tab_marca
            SET marca_desc         = g_marca1.marca_desc,
                ind_habilita       = g_marca1.ind_habilita,
                marca_resulta      = g_marca1.marca_resulta,
                ind_saldo          = g_marca1.ind_saldo,
                fecha_actualiza    = HOY,
                usuario            = usuario 
            WHERE marca_cod     = g_marca1.marca_cod

            IF SQLCA.SQLCODE != 0 THEN
               CALL err_print(SQLCA.SQLCODE)

               LET error = SQLCA.SQLCODE
               LET errvar = error CLIPPED, err_get(SQLCA.SQLCODE)
               CALL errorlog(errvar CLIPPED)
               ERROR errvar CLIPPED
               PROMPT "" FOR aux_pausa
            ELSE
               WHENEVER ERROR STOP
               ERROR "REGISTRO MODIFICADO."
               SLEEP 2
               CLEAR FORM
            END IF

         ELSE
            ERROR "PROCESO DE MODIFICAR,CANCELADO"
            SLEEP 2
            ERROR " "
            INITIALIZE g_marca1.* TO NULL
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
      ERROR "ARCHIVO DE MARCAS VACIO"
   END IF
   CLEAR SCREEN      
END FUNCTION
################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM0632" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Elimina                                             (Ctrl-c) Salir     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                  Escoja con < ENTER > la marca a eliminar                            " AT 2,1 
      DISPLAY "                          CATALOGO DE MARCAS                                   " AT 3,1 ATTRIBUTE(REVERSE) 

      LET int_flag = FALSE

      CONSTRUCT cla_where ON marca_cod,
                             ind_habilita,
                             marca_resulta,
                             ind_saldo
                        FROM marca_cod,
                             ind_habilita,
                             marca_resulta,
                             ind_saldo
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_3
         RETURN                    
      END IF
 
      LET sel_where = "SELECT marca_cod,",
                             "ind_habilita,",
                             "marca_resulta,",
                             "ind_saldo ",
                      " FROM tab_marca",
                      " WHERE ",cla_where CLIPPED ,
                      " ORDER BY 1 "

      PREPARE query10 FROM sel_where

      DECLARE cursor_10 CURSOR FOR query10

      LET pos = 1

      FOREACH cursor_10 INTO l_record8[pos].*

         SELECT marca_desc
         INTO   l_record4[pos].marca_desc 
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_cod

         SELECT a.descripcion
         INTO   l_record2[pos].descripcion
         FROM   tab_ind_habilita a
         WHERE  a.ind_habilita = l_record8[pos].ind_habilita

         SELECT marca_desc
         INTO   l_record5[pos].marca_desc
         FROM   tab_marca
         WHERE  marca_cod = l_record8[pos].marca_resulta

         SELECT descripcion
         INTO   l_record3[pos].descripcion
         FROM   tab_ind_saldo
         WHERE  ind_saldo = l_record8[pos].ind_saldo  

         LET pos = pos + 1
      END FOREACH  

      INITIALIZE l_record8[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)

         DISPLAY ARRAY l_record8 TO scr_1.*  
            ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET g_marca1.marca_cod          = l_record8[pos].marca_cod
            LET g_marca1.marca_desc         = l_record4[pos].marca_desc
            LET g_marca1.ind_habilita       = l_record8[pos].ind_habilita
            LET vind_habilita_desc          = l_record2[pos].descripcion
            LET g_marca1.marca_resulta      = l_record8[pos].marca_resulta
            LET vmarca_resulta_desc         = l_record5[pos].marca_desc
            LET g_marca1.ind_saldo          = l_record8[pos].ind_saldo
            LET vind_saldo_desc             = l_record3[pos].descripcion
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
         ERROR "REGISTRO DE .... NO EXISTE"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      DISPLAY BY NAME g_marca1.marca_cod,
                      g_marca1.marca_desc,
                      g_marca1.ind_habilita,
                      vind_habilita_desc,
                      g_marca1.marca_resulta,
                      vmarca_resulta_desc,
                      g_marca1.ind_saldo,
                      vind_saldo_desc
      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         WHENEVER ERROR CONTINUE
         DELETE
         FROM tab_marca
         WHERE marca_cod = g_marca1.marca_cod

         IF SQLCA.SQLCODE != 0 THEN
            CALL err_print(SQLCA.SQLCODE)

	    LET error = SQLCA.SQLCODE
            LET errvar = error CLIPPED, err_get(SQLCA.SQLCODE)
	    CALL errorlog(errvar CLIPPED)
	    ERROR errvar CLIPPED
            PROMPT "" FOR aux_pausa
         ELSE
            WHENEVER ERROR STOP
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
      ERROR "REGISTRO DE MARCAS .... NO EXISTE"
   END IF
   ERROR ""             
END FUNCTION
################################################################################
FUNCTION Pregunta()
   PROMPT "Esta seguro S/N ? " 
      FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Impresion(pos)
   DEFINE i,
          pos INTEGER

   DEFINE g_marca2 RECORD
          marca_cod          SMALLINT,
          marca_desc         CHAR(25),
          ind_habilita       SMALLINT,
          ind_habilita_desc  CHAR(25),
          marca_resulta      SMALLINT,
          marca_resulta_desc CHAR(20),
          ind_saldo          SMALLINT,
          ind_saldo_desc     CHAR(20)
   END RECORD

   LET hora = TIME

   LET g_impre = g_param_dis.ruta_listados CLIPPED,
                 "/",usuario CLIPPED,
                 ".IMPMARCAS",
                 hoy USING "dd-mm-yyyy",
                 "_",hora CLIPPED

   START REPORT rpt_tab_marca TO g_impre

   FOR i=1 TO (pos+1)
          LET g_marca2.marca_cod          = l_record8[i].marca_cod
          LET g_marca2.marca_desc         = l_record4[i].marca_desc
          LET g_marca2.ind_habilita       = l_record8[i].ind_habilita
          LET g_marca2.ind_habilita_desc  = l_record2[i].descripcion
          LET g_marca2.marca_resulta      = l_record8[i].marca_resulta
          LET g_marca2.marca_resulta_desc = l_record5[i].marca_desc
          LET g_marca2.ind_saldo          = l_record8[i].ind_saldo
          LET g_marca2.ind_saldo_desc     = l_record3[i].descripcion

       IF g_marca2.marca_cod IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_tab_marca(g_marca2.*)
   END FOR

   FINISH REPORT rpt_tab_marca

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_tab_marca (g_marca2)

   DEFINE g_marca2 RECORD
          marca_cod          SMALLINT,
          marca_desc         CHAR(25),
          ind_habilita       SMALLINT,
          ind_habilita_desc  CHAR(25),
          marca_resulta      SMALLINT,
          marca_resulta_desc CHAR(20),
          ind_saldo          SMALLINT,
          ind_saldo_desc     CHAR(20)
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
         PRINT COLUMN 02," TABM063 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'    

         PRINT COLUMN 35,"CATALOGO DE MARCAS"

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         SKIP 2 LINE         

         PRINT COLUMN 05,"COD. MARCA",
               COLUMN 17,"DESCR. MARCA" ,
               COLUMN 41,"IND. HABILITA.",
               COLUMN 56,"DESCR. IND. HABILITA.",
               COLUMN 80,"MARCA RESULTA",
               COLUMN 94,"DESCR. MARCA RESUL.",
               COLUMN 115,"IND. SALDO",
               COLUMN 126,"DESCR. IND. SALDO"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   

         PRINT COLUMN 04, g_marca2.marca_cod, 
               COLUMN 17, g_marca2.marca_desc,       
               COLUMN 30, g_marca2.ind_habilita,    
               COLUMN 56, g_marca2.ind_habilita_desc, 
               COLUMN 60, g_marca2.marca_resulta,     
               COLUMN 94, g_marca2.marca_resulta_desc,
               COLUMN 102, g_marca2.ind_saldo,         
               COLUMN 127, g_marca2.ind_saldo_desc   
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"Total de registros : ",COUNT(*) USING "<<<<<"
END REPORT          
