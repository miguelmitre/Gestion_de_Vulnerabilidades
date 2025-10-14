################################################################################
#Owner                      : E.F.P.                                           #
#Programa RETM050           : CATALOGO DE MATRIZ DE DERECHO                    #
#Fecha                      : 18 FEBRERO 2004 				       #
#Por                        : JUAN CARLOS MENDOZA MORENO 		       #
#Sistema                    : RET					       #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_reg RECORD
          tipo_retiro      CHAR(1),
          desc_retiro      CHAR(40),
          tipo_seguro      CHAR(2),
          desc_seguro      CHAR(40),
          tipo_pension     CHAR(2),
          desc_pension     CHAR(40),
          regimen          CHAR(2),
          desc_regimen     CHAR(40),
          tipo_prestacion  SMALLINT,
          desc_prestacion  CHAR(40),
          grupo            SMALLINT,
          desc_grupo       CHAR(40),
	  cve_destino      CHAR(1),
          desc_destino     CHAR(40)
   END RECORD

   DEFINE l_record ARRAY[30000] OF RECORD
          tipo_retiro      CHAR(1),
          tipo_seguro      CHAR(2),
          tipo_pension     CHAR(2),
          regimen          CHAR(2),
          tipo_prestacion  SMALLINT,
          grupo            SMALLINT,
	  cve_destino      CHAR(1)
   END RECORD
  
   DEFINE l_record2 ARRAY[30000] OF RECORD
          grupo            SMALLINT,
	  descripcion      CHAR(40)
   END RECORD
 
   DEFINE aux_pausa        CHAR(1),
          pos              SMALLINT,                      
          HOY              DATE,
          cla_where        CHAR(200),
          sel_where        CHAR(200),
          errvar           CHAR(10000),
          error            CHAR(10)

END GLOBALS

################################################################################
MAIN
   CALL startlog("/home/jmendoza/RETM050.err")   
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O
   DEFER INTERRUPT

   CALL proceso()

END MAIN

################################################################################
FUNCTION Pregunta()
   PROMPT "  ESTA SEGURO (S/N) ?  " 
   FOR CHAR aux_pausa

END FUNCTION
################################################################################
FUNCTION select()

      SELECT descripcion
      INTO g_reg.desc_retiro
      FROM tab_retiro
      WHERE tipo_retiro = g_reg.tipo_retiro 
      DISPLAY BY NAME g_reg.desc_retiro

      SELECT descripcion
      INTO g_reg.desc_seguro
      FROM tab_seguro
      WHERE clave = g_reg.tipo_seguro 
      DISPLAY BY NAME g_reg.desc_seguro

      SELECT descripcion
      INTO g_reg.desc_pension
      FROM tab_pension
      WHERE tipo_pension= g_reg.tipo_pension
      DISPLAY BY NAME g_reg.desc_pension

      SELECT descripcion
      INTO g_reg.desc_regimen
      FROM tab_regimen
      WHERE regimen = g_reg.regimen     
      DISPLAY BY NAME g_reg.desc_regimen

      SELECT descripcion
      INTO g_reg.desc_prestacion
      FROM tab_prestacion
      WHERE tipo_prestacion = g_reg.tipo_prestacion
      DISPLAY BY NAME g_reg.desc_prestacion

      SELECT descripcion
      INTO g_reg.desc_grupo   
      FROM tab_grupo 
      WHERE grupo = g_reg.grupo       
      DISPLAY BY NAME g_reg.desc_grupo 

      SELECT descripcion
      INTO g_reg.desc_destino
      FROM tab_destino_ret
      WHERE cve_destino = g_reg.cve_destino 
      DISPLAY BY NAME g_reg.desc_destino

   RETURN g_reg.desc_retiro,
	  g_reg.desc_seguro,
	  g_reg.desc_pension,
	  g_reg.desc_regimen,  
	  g_reg.desc_prestacion,
	  g_reg.desc_grupo,
	  g_reg.desc_destino
END FUNCTION
##############################################################################
FUNCTION proceso()

   LET HOY = TODAY
   OPEN WINDOW ven_1 AT 3,3 WITH FORM "RETM0501" ATTRIBUTE( BORDER)
   DISPLAY        " RETM050              CATALOGO  MATRIZ  DE  DERECHO                              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "MATRIZ DE DERECHO"
      COMMAND "Agrega" "Agrega "
         CALL agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta "
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina "
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Salir" "Salir del Programa "
         EXIT MENU
   END MENU
   CLOSE WINDOW ven_1


END FUNCTION

################################################################################
FUNCTION Inicializa()

{   CALL select()

   RETURNING g_reg.desc_retiro,
             g_reg.desc_seguro,
	     g_reg.desc_pension,
	     g_reg.desc_regimen,
	     g_reg.desc_prestacion,
	     g_reg.desc_grupo,
	     g_reg.desc_destino
}
   INITIALIZE g_reg.* TO NULL

END FUNCTION
################################################################################
FUNCTION Agrega()
  
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " (ESC) AGREGA                   (CTRL-C) SALIR                                          " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   CALL Inicializa()

   INPUT BY NAME g_reg.* WITHOUT DEFAULTS

      AFTER FIELD tipo_retiro
         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR g_reg.tipo_retiro IS NULL THEN
            INITIALIZE g_reg.tipo_retiro TO NULL
            INITIALIZE g_reg.desc_retiro TO NULL
            NEXT FIELD tipo_seguro    
         END IF

         SELECT "X" 
         FROM    tab_retiro
         WHERE   tipo_retiro = g_reg.tipo_retiro
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE RETIRO NO EXISTE  "
            NEXT FIELD tipo_retiro
         ELSE
            SELECT descripcion
            INTO g_reg.desc_retiro
            FROM tab_retiro
            WHERE tipo_retiro = g_reg.tipo_retiro 
            DISPLAY BY NAME g_reg.desc_retiro
            NEXT FIELD tipo_seguro
         END IF	

      AFTER FIELD tipo_seguro
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            INITIALIZE g_reg.tipo_seguro TO NULL
            INITIALIZE g_reg.desc_seguro TO NULL
            NEXT FIELD tipo_retiro    
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
	      INITIALIZE g_reg.tipo_seguro TO NULL
              INITIALIZE g_reg.desc_seguro TO NULL
              NEXT FIELD tipo_pension
	   END IF 
         END IF
   
         IF g_reg.tipo_seguro IS NULL THEN
            NEXT FIELD tipo_pension
         END IF

         SELECT "X" 
         FROM    tab_seguro
         WHERE   clave = g_reg.tipo_seguro
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE SEGURO NO EXISTE  "
            NEXT FIELD tipo_seguro
         ELSE
            SELECT descripcion
            INTO g_reg.desc_seguro
            FROM tab_seguro
            WHERE clave = g_reg.tipo_seguro 
            DISPLAY BY NAME g_reg.desc_seguro
            NEXT FIELD tipo_pension
         END IF	

      AFTER FIELD tipo_pension
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	    INITIALIZE g_reg.tipo_pension TO NULL
            INITIALIZE g_reg.desc_pension TO NULL
            NEXT FIELD tipo_seguro    
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
	      INITIALIZE g_reg.tipo_pension TO NULL	
              INITIALIZE g_reg.desc_pension TO NULL
              NEXT FIELD regimen
	   END IF 
         END IF

         IF g_reg.tipo_pension IS NULL THEN
            NEXT FIELD regimen     
         END IF

         SELECT "X" 
         FROM    tab_pension
         WHERE   tipo_pension = g_reg.tipo_pension
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE PENSION NO EXISTE  "
            NEXT FIELD tipo_pension
         ELSE
            SELECT descripcion
            INTO g_reg.desc_pension
            FROM tab_pension
            WHERE tipo_pension= g_reg.tipo_pension
            DISPLAY BY NAME g_reg.desc_pension
            NEXT FIELD regimen
         END IF	

      AFTER FIELD regimen
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	    INITIALIZE g_reg.regimen TO NULL
            INITIALIZE g_reg.desc_regimen TO NULL
            NEXT FIELD tipo_pension   
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
	      INITIALIZE g_reg.regimen TO NULL
              INITIALIZE g_reg.desc_regimen TO NULL
              NEXT FIELD tipo_prestacion
	   END IF 
         END IF

         SELECT "X" 
         FROM    tab_regimen
         WHERE   regimen = g_reg.regimen
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE SEGURO NO EXISTE  "
            NEXT FIELD regimen
         ELSE
            SELECT descripcion
            INTO g_reg.desc_regimen
            FROM tab_regimen
            WHERE regimen = g_reg.regimen     
            DISPLAY BY NAME g_reg.desc_regimen
            NEXT FIELD tipo_prestacion
         END IF	

      AFTER FIELD tipo_prestacion
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
	    INITIALIZE g_reg.tipo_prestacion TO NULL 	
            INITIALIZE g_reg.desc_prestacion TO NULL
            NEXT FIELD regimen       
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
	      INITIALIZE g_reg.tipo_prestacion TO NULL 	
              INITIALIZE g_reg.desc_prestacion TO NULL
              NEXT FIELD grupo
	   END IF 
         END IF

         SELECT "X" 
         FROM    tab_prestacion
         WHERE   tipo_prestacion = g_reg.tipo_prestacion
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE SEGURO NO EXISTE  "
            NEXT FIELD tipo_prestacion
         ELSE
            SELECT descripcion
            INTO g_reg.desc_prestacion
            FROM tab_prestacion
            WHERE tipo_prestacion = g_reg.tipo_prestacion
            DISPLAY BY NAME g_reg.desc_prestacion
            NEXT FIELD grupo      
         END IF	

      AFTER FIELD grupo
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            INITIALIZE g_reg.grupo TO NULL
            INITIALIZE g_reg.desc_grupo TO NULL
            NEXT FIELD tipo_prestacion
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
              INITIALIZE g_reg.grupo TO NULL
              INITIALIZE g_reg.desc_grupo TO NULL
              NEXT FIELD cve_destino
	   END IF 
         END IF

         IF g_reg.grupo  IS NULL THEN
            OPEN WINDOW ven_3 AT 7,15 WITH FORM "RETM0503" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) CONSULTA                     (CTRL-C) SALIR   " AT 1,1
            DISPLAY "                  CATALOGO DE GRUPO                      " AT 3,1 ATTRIBUTE(REVERSE)
            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON tab_grupo.*
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
               CLOSE WINDOW ven_3
               NEXT FIELD grupo       
            END IF
            LET sel_where = "SELECT      ",
                            " grupo ,",
                            " descripcion ",
                            " FROM tab_grupo WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1 "

            LET sel_where = sel_where CLIPPED

            PREPARE query5 FROM sel_where

            DECLARE cursor_5 CURSOR FOR query5

            LET pos = 1
            FOREACH cursor_5 INTO l_record2[pos].grupo,
                                  l_record2[pos].descripcion
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_reg.grupo =l_record2[pos].grupo
                     LET g_reg.desc_grupo  =l_record2[pos].descripcion
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ven_3

               SELECT descripcion
               INTO g_reg.desc_grupo   
               FROM tab_grupo 
               WHERE grupo = g_reg.grupo       

               DISPLAY BY NAME g_reg.grupo,
			       g_reg.desc_grupo
	    ELSE 
	       ERROR "  ARCHIVO DE GRUPO... VACIO  "
	    END IF
	    NEXT FIELD cve_destino
         END IF

         SELECT "X" 
         FROM    tab_grupo
         WHERE   grupo = g_reg.grupo
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE GRUPO NO EXISTE  "
            NEXT FIELD grupo
         ELSE
            SELECT descripcion
            INTO g_reg.desc_grupo   
            FROM tab_grupo 
            WHERE grupo = g_reg.grupo       
            DISPLAY BY NAME g_reg.desc_grupo 
            NEXT FIELD cve_destino
         END IF	

      AFTER FIELD cve_destino
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            INITIALIZE g_reg.cve_destino TO NULL
            INITIALIZE g_reg.desc_destino TO NULL
            NEXT FIELD grupo         
	 ELSE 
           IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
              INITIALIZE g_reg.cve_destino TO NULL
              INITIALIZE g_reg.desc_destino TO NULL
              NEXT FIELD tipo_retiro
	   END IF 
         END IF

         SELECT "X" 
         FROM    tab_destino_ret
         WHERE   cve_destino = g_reg.cve_destino

         IF STATUS = NOTFOUND THEN
            ERROR "  CLAVE DE DESTINO NO EXISTE  "
            NEXT FIELD cve_destino
         ELSE
            SELECT descripcion
            INTO g_reg.desc_destino
            FROM tab_destino_ret
            WHERE cve_destino = g_reg.cve_destino 
            DISPLAY BY NAME g_reg.desc_destino
         END IF	

      SELECT "OK"
      FROM ret_matriz_derecho
      WHERE regimen         = g_reg.regimen
      AND   tipo_seguro     = g_reg.tipo_seguro
      AND   tipo_pension    = g_reg.tipo_pension
      AND   tipo_prestacion = g_reg.tipo_prestacion
      AND   cve_destino     = g_reg.cve_destino
      IF STATUS <>  NOTFOUND THEN
         ERROR "  REGISTRO YA EXISTE  "
	 SLEEP 1
         CLEAR FORM
         CALL Inicializa()
	 NEXT FIELD tipo_retiro
      END IF
    
      ON KEY ( ESC )
         IF g_reg.tipo_retiro = 0 OR g_reg.tipo_retiro IS NULL THEN
            ERROR "  TIPO DE RETIRO NO PUEDE SER NULO  "
            NEXT FIELD tipo_retiro
         END IF

         IF g_reg.regimen = 0 OR g_reg.regimen IS NULL THEN
            ERROR "  REGIMEN NO PUEDE SER NULO  "
            NEXT FIELD regimen
         END IF

         IF g_reg.tipo_prestacion IS NULL THEN
            ERROR "  TIPO DE PRESTACION NO PUEDE SER NULO  "
            NEXT FIELD tipo_prestacion
         END IF

         IF g_reg.grupo  IS NULL THEN
            ERROR "  GRUPO NO PUEDE SER NULO  "
            NEXT FIELD grupo
         END IF

         IF g_reg.cve_destino = 0 OR g_reg.cve_destino IS NULL THEN
            ERROR "  CLAVE DE DESTINO NO PUEDE SER NULO  "
            NEXT FIELD cve_destino
         END IF

         WHENEVER ERROR CONTINUE
         INSERT INTO ret_matriz_derecho VALUES (g_reg.regimen        ,
                                                g_reg.tipo_seguro    ,
                                                g_reg.tipo_pension   ,
                                                g_reg.tipo_prestacion,
                                                g_reg.cve_destino    ,       
                                                g_reg.tipo_retiro    ,
                                                g_reg.grupo)
         IF SQLCA.SQLCODE != 0 THEN
            CALL err_print(SQLCA.SQLCODE)
            LET error = SQLCA.SQLCODE
            LET errvar = error CLIPPED, " ", err_get(SQLCA.SQLCODE)           
            CALL errorlog(errvar CLIPPED)
            ERROR errvar CLIPPED
            PROMPT "" FOR aux_pausa
         ELSE
            WHENEVER ERROR STOP
            ERROR "  REGISTRO INGRESADO  "
            SLEEP 1
            CLEAR FORM
         END IF

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
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "RETM0502" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) CONSULTA              (CTROL-P) IMPRESION           (CTRL-C) SALIR    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY " RETM050                CATALOGO  DE  MATRIZ  DE  DERECHO                        " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE   

      CONSTRUCT cla_where ON tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino
                        FROM tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino

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

      LET sel_where = "SELECT tipo_retiro ,",
                             "tipo_seguro ,",
                             "tipo_pension ,",
                             "regimen ,",
		             "tipo_prestacion ,",
		             "grupo ,",
	                     "cve_destino ",
                             "FROM ret_matriz_derecho ",
                             "WHERE ",cla_where CLIPPED ,
                             "ORDER BY 1,2,3" 
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
--            ON KEY (control-p)
--               ERROR "PROCESANDO IMPRESION..."
--               CALL Impresion(pos)

            ON KEY (INTERRUPT)
	       EXIT DISPLAY	
         END DISPLAY

         CLOSE WINDOW ventana_2

      ELSE
         ERROR "  REGISTRO .... NO EXISTE  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
      END IF                  
   END IF
   CLEAR SCREEN
END FUNCTION


################################################################################
FUNCTION Modifica()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "RETM0502" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) MODIFICA                                            (CTRL-C) SALIR     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                Escoja con <ENTER> el REGISTRO a  MODIFICAR                  " AT 2,1
      DISPLAY " RETM050             CATALOGO  DE  MATRIZ  DE  DERECHO                           " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino
                        FROM tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         ERROR "  BUSQUEDA CANCELADA...  "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN                    
      END IF
 
      LET sel_where = "SELECT tipo_retiro ,",
                             "tipo_seguro ,",
                             "tipo_pension ,",
                             "regimen ,",
			     "tipo_prestacion ,",
			     "grupo ,",
			     "cve_destino ",
                      " FROM ret_matriz_derecho  ",
                      " WHERE ",cla_where CLIPPED ,
                      "ORDER BY 1,2,3 " 

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
            LET g_reg.tipo_retiro     = l_record[pos].tipo_retiro
            LET g_reg.tipo_seguro     = l_record[pos].tipo_seguro
            LET g_reg.tipo_pension    = l_record[pos].tipo_pension
            LET g_reg.regimen         = l_record[pos].regimen
            LET g_reg.tipo_prestacion = l_record[pos].tipo_prestacion
            LET g_reg.grupo           = l_record[pos].grupo
            LET g_reg.cve_destino     = l_record[pos].cve_destino
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
         ERROR "  REGISTRO .... NO EXISTE  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

      CALL select()
      RETURNING g_reg.desc_retiro,
	        g_reg.desc_seguro,
	        g_reg.desc_pension,
	        g_reg.desc_regimen,
	        g_reg.desc_prestacion,
	        g_reg.desc_grupo,
	        g_reg.desc_destino

      DISPLAY g_reg.desc_retiro,
	      g_reg.desc_seguro,
	      g_reg.desc_pension,
	      g_reg.desc_regimen,  
	      g_reg.desc_prestacion,
	      g_reg.desc_grupo,
	      g_reg.desc_destino

      INPUT BY NAME g_reg.*  WITHOUT DEFAULTS 

      AFTER FIELD tipo_retiro
         IF g_reg.tipo_retiro = 0 OR g_reg.tipo_retiro IS NULL THEN
            ERROR "  CODIGO DE TIPO DE RETIRO NO PUEDE SER NULO  "
            NEXT FIELD tipo_retiro
         END IF

         SELECT "X" 
         FROM    tab_retiro
         WHERE   tipo_retiro = g_reg.tipo_retiro
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE TIPO DE RETIRO NO EXISTE  "
            NEXT FIELD tipo_retiro
         ELSE
            SELECT descripcion
            INTO g_reg.desc_retiro
            FROM tab_retiro
            WHERE tipo_retiro = g_reg.tipo_retiro 
            DISPLAY BY NAME g_reg.desc_retiro
            NEXT FIELD grupo      
         END IF 

	 NEXT FIELD grupo

      AFTER FIELD grupo 
         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            CALL Inicializa()
            NEXT FIELD tipo_retiro    
         END IF
         IF g_reg.grupo  IS NULL THEN
            OPEN WINDOW ven_6 AT 7,15 WITH FORM "RETM0503" ATTRIBUTE(BORDER)
            DISPLAY " (ENTER) CONSULTA                     (CTRL-C) SALIR   " AT 1,1
            DISPLAY "                  CATALOGO DE GRUPO                      " AT 3,1 ATTRIBUTE(REVERSE)
            LET int_flag = FALSE

            CONSTRUCT BY NAME cla_where ON tab_grupo.*
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
               CLOSE WINDOW ven_3
               NEXT FIELD grupo       
            END IF
            LET sel_where = "SELECT      ",
                            " grupo ,",
                            " descripcion ",
                            " FROM tab_grupo WHERE ",
                             cla_where CLIPPED ,
                            " ORDER BY 1 "

            LET sel_where = sel_where CLIPPED

            PREPARE query6 FROM sel_where

            DECLARE cursor_6 CURSOR FOR query6

            LET pos = 1
            FOREACH cursor_6 INTO l_record2[pos].grupo,
                                  l_record2[pos].descripcion
                  LET pos = pos+1
            END FOREACH

            IF (pos-1) >= 1 THEN
               CALL SET_COUNT(pos-1)
               DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()
                     LET g_reg.grupo =l_record2[pos].grupo
                     LET g_reg.desc_grupo  =l_record2[pos].descripcion
                     EXIT DISPLAY
                  ON KEY (INTERRUPT)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
                  ON KEY (control-c)
                     ERROR "Usted debe escojer un registro"
                     LET pos = ARR_CURR()
               END DISPLAY
               CLOSE WINDOW ven_6

               SELECT descripcion
               INTO g_reg.desc_grupo   
               FROM tab_grupo 
               WHERE grupo = g_reg.grupo       

               DISPLAY BY NAME g_reg.grupo,
			       g_reg.desc_grupo
	    ELSE 
	       ERROR "  ARCHIVO DE GRUPO... VACIO  "
	    END IF
         END IF
	 
         SELECT "X" 
         FROM    tab_grupo
         WHERE   grupo = g_reg.grupo
         IF STATUS = NOTFOUND THEN
            ERROR "  CODIGO DE GRUPO NO EXISTE  "
            NEXT FIELD grupo
         ELSE
            SELECT descripcion
            INTO g_reg.desc_grupo   
            FROM tab_grupo 
            WHERE grupo = g_reg.grupo       
            DISPLAY BY NAME g_reg.desc_grupo 
         END IF	

      CALL Pregunta()
      IF aux_pausa MATCHES "[sS]" THEN
         WHENEVER ERROR CONTINUE
         UPDATE ret_matriz_derecho
         SET tipo_retiro       = g_reg.tipo_retiro  ,
             grupo             = g_reg.grupo
         WHERE regimen         = g_reg.regimen
     	 AND   tipo_seguro     = g_reg.tipo_seguro
    	 AND   tipo_pension    = g_reg.tipo_pension
    	 AND   tipo_prestacion = g_reg.tipo_prestacion
    	 AND   cve_destino     = g_reg.cve_destino

      IF SQLCA.SQLCODE != 0 THEN
         CALL err_print(SQLCA.SQLCODE)
         LET error = SQLCA.SQLCODE
         LET errvar = error CLIPPED, err_get(SQLCA.SQLCODE)
         CALL errorlog(errvar CLIPPED)
         ERROR errvar CLIPPED
         PROMPT "" FOR aux_pausa
      ELSE
         WHENEVER ERROR STOP
         ERROR "  REGISTRO MODIFICADO  "
          CLEAR FORM
      END IF

   ELSE
      ERROR "  PROCESO DE MODIFICAR, CANCELADO  "
      SLEEP 1
      ERROR " "
      INITIALIZE g_reg.* TO NULL
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
      ERROR "  ARCHIVO  VACIO  "
   END IF
   CLEAR SCREEN      
END FUNCTION

################################################################################
FUNCTION Elimina()
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_3 AT 6,3 WITH FORM "RETM0502" ATTRIBUTE( BORDER)
      DISPLAY " (ENTER) ELIMINA                                             (CTRL-C) SALIR     " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                  Escoja con < ENTER > el REGISTRO a ELIMINAR                            " AT 2,1 
      DISPLAY " RETM050               CATALOGO DE MATRIZ DE DERECHO                                   " AT 3,1 ATTRIBUTE(REVERSE) 
      DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino
                        FROM tipo_retiro    ,
                             tipo_seguro    ,
                             tipo_pension   ,
                             regimen        ,
                             tipo_prestacion,
                             grupo          ,
                             cve_destino
         ON KEY (control-m)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         ERROR "   BUSQUEDA CANCELADA...   "
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_3
         RETURN                    
      END IF
 
      LET sel_where = "SELECT tipo_retiro ,",
                             "tipo_seguro ,",
                             "tipo_pension ,",
                             "regimen ,",
			     "tipo_prestacion ,",
			     "grupo ,",
			     "cve_destino ",
                      " FROM ret_matriz_derecho  ",
                      " WHERE ",cla_where CLIPPED ,
                      "ORDER BY 1,2,3 " 

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
            LET g_reg.tipo_retiro     = l_record[pos].tipo_retiro
            LET g_reg.tipo_seguro     = l_record[pos].tipo_seguro
            LET g_reg.tipo_pension    = l_record[pos].tipo_pension
            LET g_reg.regimen         = l_record[pos].regimen
            LET g_reg.tipo_prestacion = l_record[pos].tipo_prestacion
            LET g_reg.grupo           = l_record[pos].grupo
            LET g_reg.cve_destino     = l_record[pos].cve_destino
            EXIT DISPLAY                    
            ON KEY (INTERRUPT)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()     
            ON KEY (control-c)
               ERROR "  USTED DEBE ESCOJER UN REGISTRO  "
               LET pos = ARR_CURR()                    
         END DISPLAY
         CLOSE WINDOW ventana_3
      ELSE
         ERROR "  REGISTRO .... NO EXISTE  "
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_3
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1    
      DISPLAY " (Ctrl-C) Salir " AT 1,1
      DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE)

      CALL select()
      RETURNING g_reg.desc_retiro,
	        g_reg.desc_seguro,
	        g_reg.desc_pension,
	        g_reg.desc_regimen,
	        g_reg.desc_prestacion,
	        g_reg.desc_grupo,
	        g_reg.desc_destino

      DISPLAY g_reg.desc_retiro,
	      g_reg.desc_seguro,
	      g_reg.desc_pension,
	      g_reg.desc_regimen,  
	      g_reg.desc_prestacion,
	      g_reg.desc_grupo,
	      g_reg.desc_destino

      DISPLAY BY NAME g_reg.*
      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         WHENEVER ERROR CONTINUE
         DELETE
         FROM ret_matriz_derecho
         WHERE regimen         = g_reg.regimen
     	 AND   tipo_seguro     = g_reg.tipo_seguro
    	 AND   tipo_pension    = g_reg.tipo_pension
    	 AND   tipo_prestacion = g_reg.tipo_prestacion
         AND   cve_destino     = g_reg.cve_destino

         IF SQLCA.SQLCODE != 0 THEN
            CALL err_print(SQLCA.SQLCODE)

	    LET error = SQLCA.SQLCODE
            LET errvar = error CLIPPED, err_get(SQLCA.SQLCODE)
	    CALL errorlog(errvar CLIPPED)
	    ERROR errvar CLIPPED
            PROMPT "" FOR aux_pausa
         ELSE
            WHENEVER ERROR STOP
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
      ERROR "  REGISTRO .... NO EXISTE  "
   END IF
   ERROR ""             
END FUNCTION

{
################################################################################

FUNCTION Impresion(pos)
   DEFINE i,
          pos INTEGER

       DEFINE g_reg2 RECORD
            tipo_retiro      CHAR(1),
          tipo_seguro      CHAR(2),
          tipo_pension     CHAR(2),
          regimen          CHAR(2),
          tipo_prestacion  SMALLINT,
          grupo            SMALLINT,
	  cve_destino      CHAR(1)
   END RECORD

--   LET hora = TIME

   LET g_impre = g_param_dis.ruta_listados CLIPPED,
                 "/",usuario CLIPPED,
                 ".IMPMARCAS",
                 hoy USING "dd-mm-yyyy",
                 "_",hora CLIPPED

   START REPORT rpt_matriz_der TO g_impre

   FOR i=1 TO (pos+1)
          LET g_reg2.tipo_retiro          = l_record[i].tipo_retiro
          LET g_reg2.tipo_seguro= l_record[i].tipo_seguro
          LET g_reg2.tipo_pension= l_record[i].tipo_pension
          LET g_reg2.regimen= l_record[i].regimen
          LET g_reg2.tipo_prestacion= l_record[i].tipo_prestacion
          LET g_reg2.grupo= l_record[i].grupo
          LET g_reg2.cve_destino= l_record[i].cve_destino

       IF g_reg2.tipo_retiro IS NULL THEN
          EXIT FOR
       END IF
       OUTPUT TO REPORT rpt_matriz_der(g_reg2.*)
   END FOR

   FINISH REPORT rpt_matriz_der

    ERROR "  LISTADO GENERADO...  "
   SLEEP 1
   ERROR ""

   LET g_lista = "lp ",g_impre
   RUN g_lista
END FUNCTION
################################################################################
REPORT rpt_matriz_der (g_reg2)

   DEFINE g_reg2 RECORD
          tipo_retiro      CHAR(1),
          tipo_seguro      CHAR(2),
          tipo_pension     CHAR(2),
          regimen          CHAR(2),
          tipo_prestacion  SMALLINT,
          grupo            SMALLINT,
	  cve_destino      CHAR(1)
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
         PRINT COLUMN 02," RETM050 ",
               COLUMN 128, TODAY  USING "dd/mm/yyyy"

         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'    

         PRINT COLUMN 35,"CATALOGO DE MATRIZ DE DERECHO"

         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   
         SKIP 2 LINE         

         PRINT COLUMN 05,"TIPO RETIRO",
               COLUMN 17,"TIPO SEGURO" ,
               COLUMN 41,"TIPO PENSION",
               COLUMN 56,"REGIMEN",
               COLUMN 80,"TIPO PRESTACION",
               COLUMN 94,"GRUPO",
               COLUMN 115,"CLAVE DESTINO"
         SKIP 1 LINE

      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s14H\033(s7B'   

         PRINT COLUMN 04, g_reg2.tipo_retiro, 
               COLUMN 17, g_reg2.tipo_seguro,       
               COLUMN 30, g_reg2.tipo_pension,
               COLUMN 56, g_reg2.regimen,
               COLUMN 60, g_reg2.tipo_prestacion,
               COLUMN 94, g_reg2.grupo,
               COLUMN 102, g_reg2.cve_destino
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 02,"TOTAL DE REGISTROS : ",COUNT(*) USING "<<<<<"
END REPORT          
}


