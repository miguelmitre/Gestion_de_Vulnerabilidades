################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa TABM031  => CATALOGO ARCHIVO COLONIAS    
#Fecha             => 09 Mayo 1997.     				       #
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha Actializa   => 12 de ENERO 2000.                                        #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ.                         #
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
       DEFINE g_reg     ARRAY[1000] OF RECORD 
               cpos_cod	       CHAR(05),
  	       colon_cod       INTEGER,
	       colon_desc      CHAR(40)
       END RECORD 

       DEFINE g_reg2     RECORD 	 
              estad_cod	       INTEGER,
              estad_desc       CHAR(40),
	      cpos_desde       CHAR(05),
              cpos_hasta       CHAR(05),
              deleg_cod	       INTEGER,
              deleg_desc       CHAR(40),
              ciudad_cod         INTEGER,
              ciudad_desc	       CHAR(40)
      END RECORD

      DEFINE l_record1 ARRAY[40] OF RECORD
             codigo           INTEGER,
             descripcion      CHAR(50)
      END RECORD

      DEFINE l_record2 ARRAY[3000] OF RECORD
             codigo           INTEGER,
             descripcion      CHAR(50)
      END RECORD
        
      DEFINE l_record3 ARRAY[3000] OF RECORD
             codigo           INTEGER,
             descripcion      CHAR(50)
      END RECORD

      #DEFINE g_param_dis       RECORD LIKE dis_parametro.*
      DEFINE g_param_dis       RECORD LIKE seg_modulo.*

      DEFINE vmenu             CHAR(01),
             aux_pausa	       CHAR(1),
             HOY	       DATE,
             SW_1              INTEGER,
             vaccion           SMALLINT,
             pos               INTEGER,
             seg_usuario           CHAR(08),
             arr_c,
             arr_ci,
             scr_l,
             i                 INTEGER,
             cla_where         CHAR(100),
             sel_where         CHAR(100)
END GLOBALS
##########################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL inicio()
        CALL proceso()
END MAIN
###########################################################################
FUNCTION inicio()
        SELECT USER
        INTO   seg_usuario
        FROM   glo_parametro

        {SELECT ruta_spool
        INTO   g_param_dis.ruta_spool
        FROM   dis_parametro}

        SELECT ruta_listados
          INTO g_param_dis.ruta_listados
          FROM seg_modulo
         WHERE modulo_cod = 'tab'

END FUNCTION
###########################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0311" ATTRIBUTE( BORDER)
	DISPLAY " TABM031                   CATALOGO DE COLONIAS                                " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,64 ATTRIBUTE(REVERSE)
	
	MENU "CATALOGO COLONIAS"
		COMMAND "Agrega" "Agrega Colonias"
	           CALL Agrega()
                COMMAND "Consulta" "Consulta Colonias"
	           CALL Consulta()
                COMMAND "Modifica" "Modifica Colonias"
	           CALL Modifica()
                COMMAND "Elimina" "Elimina Colonias"
	           CALL Elimina()
                COMMAND "Salir" "Salir del Programa"
	           EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
    LET sw_1 = 0
    INITIALIZE g_reg TO NULL 
    FOR i=1 TO 9
       DISPLAY g_reg[i].* TO scr_1[i].*
    END FOR

    INITIALIZE g_reg2.* TO NULL
    DISPLAY BY NAME g_reg2.*
    CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Agrega()

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ( Esc ) Agrega                  (Ctrl-c) Salir                                " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
        LET sw_1 = 0
       
	INPUT BY NAME  g_reg2.*
             AFTER  FIELD estad_cod
                IF g_reg2.estad_cod IS NULL THEN
                   CALL Despliega_estado() 
                   RETURNING g_reg2.estad_cod,g_reg2.estad_desc

                   IF g_reg2.estad_desc IS NULL THEN
                      ERROR "No existe esta entidad"
                      LET g_reg2.estad_cod=NULL
                      NEXT FIELD estad_cod
                   END IF 

                   SELECT cpos_desde,
                          cpos_hasta 
                   INTO   g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado
                   WHERE  estad_cod = g_reg2.estad_cod
                ELSE
                   SELECT estad_cod,
                          estad_desc,
                          cpos_desde,
                          cpos_hasta 
                   INTO   g_reg2.estad_cod,
                          g_reg2.estad_desc, 
                          g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado 
                   WHERE estad_cod = g_reg2.estad_cod

                   IF STATUS = NOTFOUND THEN
                       ERROR "No existe esta entidad"
                       NEXT FIELD estad_cod
		   END IF

                   DISPLAY BY NAME g_reg2.*
	        END IF

	      AFTER FIELD deleg_cod
                    LET vmenu='A'
		    IF g_reg2.deleg_cod IS NULL THEN
                       CALL Despliega_delegacion()
	               RETURNING g_reg2.deleg_cod,g_reg2.deleg_desc

                       IF g_reg2.deleg_desc IS NULL THEN
                          ERROR "No existe esta delegacion"
                          LET g_reg2.deleg_cod=NULL
                          NEXT FIELD deleg_cod
                       END IF 
                    ELSE
                       SELECT deleg_cod,
                              deleg_desc 
                       INTO   g_reg2.deleg_cod,
                              g_reg2.deleg_desc
                       FROM   tab_delegacion
	               WHERE  deleg_cod = g_reg2.deleg_cod 
                       AND    estad_cod = g_reg2.estad_cod 

                       IF STATUS=100 THEN
                          ERROR "No existe esta delegacion"
			  NEXT FIELD deleg_cod
                       END IF
               	    END IF

                    DISPLAY BY NAME g_reg2.*

                AFTER FIELD ciudad_cod
		    IF g_reg2.ciudad_cod IS NULL THEN
                       CALL Despliega_city()
		       RETURNING g_reg2.ciudad_cod,g_reg2.ciudad_desc

                       IF g_reg2.ciudad_desc IS NULL THEN
                          ERROR "No existe esta Localidad"
                          LET g_reg2.ciudad_cod=NULL
                          NEXT FIELD ciudad_cod
                       END IF 
		    ELSE
                       SELECT ciudad_cod,
                              ciudad_desc
	               INTO   g_reg2.ciudad_cod,
                              g_reg2.ciudad_desc
                       FROM   tab_ciudad
		       WHERE  ciudad_cod = g_reg2.ciudad_cod 
                       AND    estad_cod= g_reg2.estad_cod

		       IF STATUS=100 THEN
                          ERROR "No existe esta loc/ciu"
			  NEXT FIELD ciudad_cod
         	       END IF
		    END IF			

      		    DISPLAY BY NAME g_reg2.*

                 ON KEY (INTERRUPT)
                    LET vaccion=1
                    EXIT INPUT
                 ON KEY ( ESC )
                    IF g_reg2.estad_cod IS NULL THEN
                       ERROR "El Codigo del Estado NO puede ser NULO"
                       NEXT FIELD estad_cod
                    END IF

		    IF g_reg2.deleg_cod IS NULL THEN
                       ERROR "El Codigo de la Delegacion/Municipio NO puede ser NULO"
                       NEXT FIELD deleg_cod
                    END IF

		    IF g_reg2.ciudad_cod IS NULL THEN
                       ERROR "El Codigo de la Localidad/Ciudad NO puede ser NULO"
                       NEXT FIELD ciudad_cod
                    END IF

                    LET vaccion=2
                    EXIT INPUT
	END INPUT

        IF vaccion=1 THEN
           CALL Inicializa()
           RETURN
        END IF

	INPUT ARRAY g_reg FROM scr_1.*
            BEFORE FIELD cpos_cod
	       LET arr_c = ARR_CURR()
               LET scr_l = SCR_LINE()
	    AFTER FIELD cpos_cod	
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                   IF ARR_C > 1 THEN
                      LET arr_c = ARR_CURR()-1
		      LET scr_l = SCR_LINE()-1
		      DISPLAY g_reg[arr_c].cpos_cod TO scr_1[scr_l].cpos_cod
		   END IF

                   IF g_reg[arr_c].cpos_cod IS NULL THEN
                      ERROR "El Codigo Postal NO puede ser NULO"
		      NEXT field cpos_cod
                   END IF
	       END IF
               
         {      SELECT cpos_cod 
               FROM   tab_codpos
               WHERE  cpos_cod = g_reg[arr_c].cpos_cod 

               IF STATUS <> NOTFOUND THEN
                  ERROR "Codigo Postal ya existente verifique .."
                  ATTRIBUTE (REVERSE)
                  SLEEP 2
                  ERROR " "
                  NEXT FIELD cpos_cod
               END IF
          }
               IF g_reg[arr_c].cpos_cod IS NULL THEN
                  ERROR "El Codigo Postal NO puede ser NULO"
		  NEXT field cpos_cod
               END IF

               IF LENGTH (g_reg[arr_c].cpos_cod) <> 5 then
		  ERROR "Debe Ingresar Codigo Postal Completo"
	          NEXT FIELD cpos_cod
               END IF

               IF NOT (g_reg[arr_c].cpos_cod >= g_reg2.cpos_desde AND
                  g_reg[arr_c].cpos_cod <= g_reg2.cpos_hasta) THEN
                  ERROR "Codigo Postal debe estar dentro del rango"
	          NEXT FIELD cpos_cod
	       END IF
 
               DECLARE ccol CURSOR FOR
               SELECT * 
               FROM   tab_colonia a,tab_codpos b
               WHERE  a.cpos_cod = g_reg[arr_c].cpos_cod 
               AND    a.cpos_cod = b.cpos_cod 
               AND    b.deleg_cod = g_reg2.deleg_cod
               --AND    b.deleg_cod <> g_reg2.deleg_cod

               OPEN ccol

               FETCH ccol

               IF STATUS <> 100 THEN
                  ERROR "Este Codigo Postal ya pertenece a una Localidad"
                  CLOSE ccol
                  NEXT FIELD cpos_cod
               END IF 

               CLOSE ccol

            BEFORE FIELD colon_cod
	       LET arr_c = ARR_CURR()
               LET scr_l = SCR_LINE()

               IF g_reg[arr_c].colon_cod IS NULL THEN
                  SELECT MAX(colon_cod) 
                  INTO   g_reg[arr_c].colon_cod
                  FROM   tab_colonia

                  IF g_reg[arr_c].colon_cod = 0 OR 
                     g_reg[arr_c].colon_cod IS NULL THEN
                     LET g_reg[arr_c].colon_cod = 1 
		  ELSE
		     LET g_reg[arr_c].colon_cod = g_reg[arr_c].colon_cod + 1
		  END IF
               END IF

	       DISPLAY g_reg[arr_c].colon_cod TO scr_1[scr_l].colon_cod
               NEXT FIELD colon_desc

            BEFORE FIELD colon_desc
	       LET arr_c = ARR_CURR()
               LET scr_l = SCR_LINE()
            AFTER FIELD colon_desc
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD cpos_cod
	       END IF

               IF g_reg[arr_c].colon_desc IS NULL THEN
                  ERROR "La Colonia NO puede ser NULA"
		  NEXT FIELD colon_desc
               END IF

               SELECT "X" 
               FROM   tab_colonia 
               WHERE  colon_cod = g_reg[arr_c].colon_cod

               IF STATUS = 100 THEN
                  INSERT INTO tab_colonia    
                  VALUES (g_reg[arr_c].cpos_cod,
                          g_reg[arr_c].colon_cod,
                          g_reg[arr_c].colon_desc)
               END IF

               SELECT cpos_cod 
               FROM   tab_codpos
               WHERE  cpos_cod = g_reg[arr_c].cpos_cod 
               AND    estad_cod= g_reg2.estad_cod 
               AND    deleg_cod= g_reg2.deleg_cod 
               AND    ciudad_cod = g_reg2.ciudad_cod

               IF STATUS = 100 THEN
                  INSERT INTO tab_codpos
                  VALUES (g_reg[arr_c].cpos_cod,
                          g_reg2.estad_cod,
                          g_reg2.deleg_cod,
                          g_reg2.ciudad_cod)
               END IF

            ON KEY (INTERRUPT)
               FOR i=1 TO arr_count()
                  DELETE FROM tab_colonia 
                  WHERE colon_cod = g_reg[i].colon_cod 

                  DELETE FROM tab_codpos
                  WHERE cpos_cod = g_reg[i].cpos_cod and
                        estad_cod= g_reg2.estad_cod and
                        deleg_cod= g_reg2.deleg_cod and
                        ciudad_cod = g_reg2.ciudad_cod
               END FOR

               CALL Inicializa()
               EXIT INPUT
            ON KEY ( ESC )

               DECLARE ccol1 CURSOR FOR
               SELECT * 
               FROM   tab_colonia a,tab_codpos b
               WHERE  a.cpos_cod = g_reg[arr_c].cpos_cod 
               AND    a.cpos_cod = b.cpos_cod 
               AND    b.deleg_cod = g_reg2.deleg_cod
               --AND    b.deleg_cod <> g_reg2.deleg_cod

               OPEN ccol1

               FETCH ccol1

               IF STATUS <> 100 THEN
                  ERROR "Este Codigo Postal ya pertenece a una Localidad"
                  CLOSE ccol1
                  NEXT FIELD cpos_cod
               END IF 

               CLOSE ccol1

               ERROR "REGISTROS AGREGADOS" 
               SLEEP 2
               ERROR ""

               CALL Inicializa()
               EXIT INPUT     
        END INPUT  
        
        CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Consulta()

        DEFINE l_record   ARRAY[2000] OF RECORD
           cpos_cod	CHAR(05),
  	   colon_cod	INTEGER,
	   colon_desc  	CHAR(40)
        END RECORD

        DEFINE pos                INTEGER   

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE (REVERSE)
	DISPLAY " [ Ctrl-C ] Salir de Consulta" AT 1,1

	INPUT BY NAME  g_reg2.*
             AFTER  FIELD estad_cod
                IF g_reg2.estad_cod IS NULL THEN
                   CALL Despliega_estado() 
                   RETURNING g_reg2.estad_cod,g_reg2.estad_desc

                   IF g_reg2.estad_desc IS NULL THEN
                      ERROR "No existe esta entidad"
                      LET g_reg2.estad_cod=NULL
                      NEXT FIELD estad_cod
                   END IF 

                   SELECT cpos_desde,
                          cpos_hasta 
                   INTO   g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado
                   WHERE  estad_cod = g_reg2.estad_cod
                ELSE
                   SELECT estad_cod,
                          estad_desc,
                          cpos_desde,
                          cpos_hasta 
                   INTO   g_reg2.estad_cod,
                          g_reg2.estad_desc, 
                          g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado 
                   WHERE  estad_cod = g_reg2.estad_cod

                   IF STATUS = NOTFOUND THEN
                       ERROR "No existe esta entidad"
                       NEXT FIELD estad_cod
		   END IF

                   DISPLAY BY NAME g_reg2.*
	        END IF
	      AFTER FIELD deleg_cod
                    LET vmenu='A'
		    IF g_reg2.deleg_cod IS NULL THEN
                       CALL Despliega_delegacion()
	               RETURNING g_reg2.deleg_cod,g_reg2.deleg_desc

                       IF g_reg2.deleg_desc IS NULL THEN
                          ERROR "No existe esta delegacion"
                          LET g_reg2.deleg_cod=NULL
                          NEXT FIELD deleg_cod
                       END IF 
                    ELSE
                       SELECT deleg_cod,
                              deleg_desc 
                       INTO   g_reg2.deleg_cod,
                              g_reg2.deleg_desc
                       FROM   tab_delegacion
	               WHERE  deleg_cod = g_reg2.deleg_cod 
                       AND    estad_cod = g_reg2.estad_cod 

                       IF STATUS=100 THEN
                          ERROR "No existe esta delegacion"
			  NEXT FIELD deleg_cod
                       END IF
               	    END IF
                    DISPLAY BY NAME g_reg2.*
                AFTER FIELD ciudad_cod
		    IF g_reg2.ciudad_cod IS NULL THEN
                       CALL Despliega_city()
		       RETURNING g_reg2.ciudad_cod,g_reg2.ciudad_desc

                       IF g_reg2.ciudad_desc IS NULL THEN
                          ERROR "No existe esta Localidad"
                          LET g_reg2.ciudad_cod=NULL
                          NEXT FIELD ciudad_cod
                       END IF 
		    ELSE
                       SELECT ciudad_cod,
                              ciudad_desc
	               INTO   g_reg2.ciudad_cod,
                              g_reg2.ciudad_desc
                       FROM   tab_ciudad
		       WHERE  ciudad_cod = g_reg2.ciudad_cod 
                       AND    estad_cod= g_reg2.estad_cod

		       IF STATUS=100 THEN
                          ERROR "No existe esta loc/ciu"
			  NEXT FIELD ciudad_cod
         	       END IF
		    END IF			
      		    DISPLAY BY NAME g_reg2.*
                    EXIT INPUT
	END INPUT

        DECLARE cursor_0 CURSOR FOR
        SELECT * 
        FROM   tab_codpos 
        WHERE  estad_cod=g_reg2.estad_cod 
        AND    deleg_cod=g_reg2.deleg_cod 
        AND    ciudad_cod =g_reg2.ciudad_cod

        OPEN cursor_0
        FETCH cursor_0

        IF STATUS=100 THEN
	   ERROR "ARCHIVO DE COLONIAS VACIO"
           SLEEP 2
           ERROR ""
           CALL Inicializa()
           CLEAR SCREEN
           RETURN
         END IF
	
	 ERROR "Buscando Datos..."

         DECLARE cursor_1 CURSOR FOR

	 SELECT b.cpos_cod,
                a.colon_cod,
                a.colon_desc 
         FROM   tab_colonia a,tab_codpos b 
         WHERE  a.cpos_cod=b.cpos_cod 
         AND    b.estad_cod=g_reg2.estad_cod 
         AND    b.deleg_cod=g_reg2.deleg_cod 
         AND    b.ciudad_cod =g_reg2.ciudad_cod
	 ORDER BY 1,2

	 LET pos = 1
	 FOREACH cursor_1 INTO l_record[pos].*
	         LET pos = pos + 1
         END FOREACH

	 ERROR ""
	 IF (pos-1) >= 1 THEN
	    CALL  SET_COUNT(pos-1)
	    DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
            DISPLAY "" AT 4,1
	 ELSE
	    ERROR "ARCHIVO DE COLONIAS VACIO"
            SLEEP 2
            ERROR ""
	 END IF

         CALL Inicializa()
         CLEAR SCREEN
END FUNCTION
#########################################################################
FUNCTION Modifica()
        DEFINE l_record   ARRAY[1000] OF RECORD
               cpos_cod	        CHAR(05),
 	       colon_cod	INTEGER,
               colon_desc  	CHAR(40)
        END RECORD

        DEFINE arr_c,
               scr_l,
               i    INTEGER
        DEFINE pos                INTEGER   

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE (REVERSE)
	DISPLAY " [ Esc ] Modifica            [ Ctrl-C ] Salir sin Modificar                    " AT 1,1

	INPUT BY NAME  g_reg2.*
             AFTER  FIELD estad_cod
                IF g_reg2.estad_cod IS NULL THEN
                   CALL Despliega_estado() 
                   RETURNING g_reg2.estad_cod,g_reg2.estad_desc

                   IF g_reg2.estad_desc IS NULL THEN
                      ERROR "No existe esta entidad"
                      LET g_reg2.estad_cod=NULL
                      NEXT FIELD estad_cod
                   END IF 

                   SELECT cpos_desde,
                          cpos_hasta 
                   INTO   g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado
                   WHERE  estad_cod = g_reg2.estad_cod
                ELSE
                   SELECT estad_cod,
                          estad_desc,
                          cpos_desde,
                          cpos_hasta 
                    INTO  g_reg2.estad_cod,
                          g_reg2.estad_desc, 
                          g_reg2.cpos_desde,
                          g_reg2.cpos_hasta
                   FROM   tab_estado 
                   WHERE  estad_cod = g_reg2.estad_cod

                   IF STATUS = NOTFOUND THEN
                       ERROR "No existe esta entidad"
                       NEXT FIELD estad_cod
		   END IF
                   DISPLAY BY NAME g_reg2.*
	        END IF
	     AFTER FIELD deleg_cod
                LET vmenu='A'
		IF g_reg2.deleg_cod IS NULL THEN
                   CALL Despliega_delegacion()
	           RETURNING g_reg2.deleg_cod,g_reg2.deleg_desc

                   IF g_reg2.deleg_desc IS NULL THEN
                      ERROR "No existe esta delegacion"
                      LET g_reg2.deleg_cod=NULL
                      NEXT FIELD deleg_cod
                   END IF 
                ELSE
                   SELECT deleg_cod,
                          deleg_desc 
                   INTO   g_reg2.deleg_cod,
                          g_reg2.deleg_desc
                   FROM   tab_delegacion
	           WHERE  deleg_cod = g_reg2.deleg_cod
                   AND    estad_cod = g_reg2.estad_cod 

                   IF STATUS=100 THEN
                      ERROR "No existe esta delegacion"
		      NEXT FIELD deleg_cod
                   END IF
               	END IF
                DISPLAY BY NAME g_reg2.*
             AFTER FIELD ciudad_cod
	        IF g_reg2.ciudad_cod IS NULL THEN
                   CALL Despliega_city()
	           RETURNING g_reg2.ciudad_cod,g_reg2.ciudad_desc

                   IF g_reg2.ciudad_desc IS NULL THEN
                      ERROR "No existe esta Localidad"
                      LET g_reg2.ciudad_cod=NULL
                      NEXT FIELD ciudad_cod
                   END IF 
                ELSE
                   SELECT ciudad_cod,
                          ciudad_desc
	           INTO   g_reg2.ciudad_cod,
                          g_reg2.ciudad_desc
                   FROM   tab_ciudad
		   WHERE  ciudad_cod = g_reg2.ciudad_cod 
                   AND    estad_cod= g_reg2.estad_cod

		   IF STATUS=100 THEN
                      ERROR "No existe esta loc/ciu"
		      NEXT FIELD ciudad_cod
         	   END IF
		END IF			
      		DISPLAY BY NAME g_reg2.*
                EXIT INPUT
	END INPUT

        DECLARE cursor_x CURSOR FOR
        SELECT * 
        FROM   tab_codpos 
        WHERE  estad_cod=g_reg2.estad_cod 
        AND    deleg_cod=g_reg2.deleg_cod 
        AND    ciudad_cod =g_reg2.ciudad_cod

        OPEN cursor_x
        FETCH cursor_x

        IF STATUS=100 THEN
           ERROR "ARCHIVO DE COLONIAS VACIO"
           SLEEP 2
           ERROR ""
           CALL Inicializa()
           CLEAR SCREEN
           RETURN
        END IF

        ERROR "Buscando Datos..."

        DECLARE cursor_z CURSOR FOR
        SELECT a.cpos_cod,
               colon_cod,
               colon_desc 
        FROM   tab_colonia a,tab_codpos b
        WHERE  a.cpos_cod = b.cpos_cod 
        AND    b.estad_cod=g_reg2.estad_cod 
        AND    b.deleg_cod=g_reg2.deleg_cod 
        AND    b.ciudad_cod =g_reg2.ciudad_cod
	ORDER BY 1,2

        LET pos = 1
        FOREACH cursor_z INTO g_reg[pos].*
           LET pos = pos + 1
        END FOREACH

	IF (pos-1) >= 1 THEN
	   CALL  SET_COUNT(pos-1)
           INPUT ARRAY g_reg without defaults FROM scr_1.*
               BEFORE FIELD cpos_cod
	          LET arr_c = ARR_CURR()
                  LET scr_l = SCR_LINE()
                  NEXT FIELD colon_desc
               BEFORE FIELD colon_desc
	          LET arr_c = ARR_CURR()
                  LET scr_l = SCR_LINE()
             ON KEY (INTERRUPT)
                EXIT INPUT
             ON KEY ( ESC )
                FOR i=1 TO ARR_COUNT()
                    UPDATE tab_colonia 
                    SET   colon_desc = g_reg[i].colon_desc 
                    WHERE cpos_cod = g_reg[i].cpos_cod 
                    AND   colon_cod =g_reg[i].colon_cod
                END FOR

                ERROR "REGISTROS MODIFICADOS" 
                SLEEP 2
                ERROR ""
                EXIT INPUT     
            END INPUT  
	 ELSE
	    ERROR "ARCHIVO DE COLONIAS VACIO"
            SLEEP 2
            ERROR ""
	 END IF
         CALL Inicializa()
         CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Elimina()
         DEFINE l_record   ARRAY[1000] OF RECORD
           cpos_cod	CHAR(05),
  	   colon_cod	INTEGER,
	   colon_desc  	CHAR(40)
         END RECORD

    DEFINE 
       arr_c,
       scr_l,
       i    INTEGER
         DEFINE pos                INTEGER   

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE (REVERSE)
	DISPLAY " [ Esc ] Elimina   [ Ctrl-C ] Salir sin Eliminar " AT 1,1
	INPUT BY NAME  g_reg2.*
           AFTER  FIELD estad_cod
              IF g_reg2.estad_cod IS NULL THEN
                 CALL Despliega_estado() 
                 RETURNING g_reg2.estad_cod,g_reg2.estad_desc

                 IF g_reg2.estad_desc IS NULL THEN
                    ERROR "No existe esta entidad"
                    LET g_reg2.estad_cod=NULL
                    NEXT FIELD estad_cod
                 END IF 

                 SELECT cpos_desde,
                        cpos_hasta 
                 INTO   g_reg2.cpos_desde,
                        g_reg2.cpos_hasta
                 FROM   tab_estado
                 WHERE  estad_cod = g_reg2.estad_cod
              ELSE
                 SELECT estad_cod,
                        estad_desc,
                        cpos_desde,
                        cpos_hasta 
                  INTO  g_reg2.estad_cod,
                        g_reg2.estad_desc, 
                        g_reg2.cpos_desde,
                        g_reg2.cpos_hasta
                 FROM   tab_estado 
                 WHERE  estad_cod = g_reg2.estad_cod

                 IF STATUS = NOTFOUND THEN
                    ERROR "No existe esta entidad"
                    NEXT FIELD estad_cod
	         END IF
                 DISPLAY BY NAME g_reg2.*
	      END IF
	   AFTER FIELD deleg_cod
              LET vmenu='A'
	      IF g_reg2.deleg_cod IS NULL THEN
                 CALL Despliega_delegacion()
	         RETURNING g_reg2.deleg_cod,g_reg2.deleg_desc

                 IF g_reg2.deleg_desc IS NULL THEN
                    ERROR "No existe esta delegacion"
                    LET g_reg2.deleg_cod=NULL
                    NEXT FIELD deleg_cod
                 END IF 
              ELSE
                 SELECT deleg_cod,
                        deleg_desc 
                 INTO   g_reg2.deleg_cod,
                        g_reg2.deleg_desc
                 FROM   tab_delegacion
	         WHERE  deleg_cod = g_reg2.deleg_cod 
                 AND    estad_cod = g_reg2.estad_cod 

                 IF STATUS=100 THEN
                    ERROR "No existe esta delegacion"
	            NEXT FIELD deleg_cod
                 END IF
              END IF
              DISPLAY BY NAME g_reg2.*
           AFTER FIELD ciudad_cod
	      IF g_reg2.ciudad_cod IS NULL THEN
                 CALL Despliega_city()
	         RETURNING g_reg2.ciudad_cod,g_reg2.ciudad_desc

                 IF g_reg2.ciudad_desc IS NULL THEN
                    ERROR "No existe esta Localidad"
                    LET g_reg2.ciudad_cod=NULL
                    NEXT FIELD ciudad_cod
                 END IF 
	      ELSE
                 SELECT ciudad_cod,
                        ciudad_desc
	         INTO   g_reg2.ciudad_cod,
                        g_reg2.ciudad_desc
                 FROM   tab_ciudad
	         WHERE  ciudad_cod = g_reg2.ciudad_cod 
                 AND    estad_cod= g_reg2.estad_cod

	         IF STATUS=100 THEN
                    ERROR "No existe esta loc/ciu"
	            NEXT FIELD ciudad_cod
         	 END IF
	      END IF			
      	      DISPLAY BY NAME g_reg2.*
              EXIT INPUT
	   END INPUT

           DECLARE cursor_e CURSOR FOR
           SELECT * 
           FROM   tab_codpos 
           WHERE  estad_cod=g_reg2.estad_cod 
           AND    deleg_cod=g_reg2.deleg_cod 
           AND    ciudad_cod =g_reg2.ciudad_cod

           OPEN cursor_e
           FETCH cursor_e

           IF STATUS=100 THEN
    	    ERROR "ARCHIVO DE COLONIAS VACIO"
              SLEEP 2
              ERROR ""
              CALL Inicializa()
              CLEAR SCREEN
              RETURN
           END IF

	   ERROR "Buscando Datos..."

           DECLARE cursor_f CURSOR FOR
           SELECT a.cpos_cod,
                  colon_cod,
                  colon_desc 
           FROM   tab_colonia a,tab_codpos b
           WHERE  a.cpos_cod = b.cpos_cod 
           AND    b.estad_cod=g_reg2.estad_cod 
           AND    b.deleg_cod=g_reg2.deleg_cod
           AND    b.ciudad_cod =g_reg2.ciudad_cod
    	   ORDER BY 1,2

  	   LET pos = 1
  	   FOREACH cursor_f INTO g_reg[pos].*
    	         LET pos = pos + 1
           END FOREACH
  
	   IF (pos-1) >= 1 THEN
	      CALL  SET_COUNT(pos-1)
              DISPLAY ARRAY g_reg TO scr_1.*
                 ON KEY (INTERRUPT)
                    EXIT DISPLAY
                 ON KEY ( ESC )
	            LET i = ARR_CURR()

                 DELETE FROM tab_colonia 
                 WHERE cpos_cod = g_reg[i].cpos_cod and
                       colon_cod =g_reg[i].colon_cod

                 ERROR "REGISTROS ELIMINADOS" sleep 2
                 ERROR ""
                 EXIT DISPLAY     
              END DISPLAY  
	   ELSE
	      ERROR "ARCHIVO DE COLONIAS VACIO"
              SLEEP 2
              ERROR ""
	   END IF
           CALL Inicializa()
           CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Despliega_estado()
        LET pos = 2
        IF (pos -1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0312" ATTRIBUTE(BORDER)
           DISPLAY "                              E N T I D A D E S                                " AT 2,1 ATTRIBUTE(REVERSE)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON estad_cod FROM estad_cod
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
              CLOSE WINDOW vent_1
              RETURN
           END IF

           LET sel_where = "SELECT estad_cod,estad_desc FROM tab_estado WHERE ",
                            cla_where CLIPPED,
                           " ORDER BY 1 "
  
           PREPARE query FROM sel_where

           DECLARE cursor_m1 CURSOR FOR query

           LET pos = 1
           FOREACH cursor_m1 INTO l_record1[pos].*
              LET pos = pos +1
           END FOREACH

           INITIALIZE l_record1[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
              DISPLAY ARRAY l_record1 TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
              END DISPLAY
           END IF
        END IF

        CLOSE WINDOW vent_1
        RETURN l_record1[pos].codigo,l_record1[pos].descripcion
END FUNCTION
##########################################################################
FUNCTION Despliega_delegacion()

        LET pos = 2
        IF (pos -1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0313" ATTRIBUTE(BORDER)
           DISPLAY "                        DELEGACIONES / MUNICIPIOS                              " AT 2,1 ATTRIBUTE(REVERSE)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON deleg_cod FROM deleg_cod
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

           LET sel_where = "SELECT deleg_cod,deleg_desc FROM tab_delegacion WHERE ",
                             cla_where CLIPPED,
                           " ORDER BY 1 "
  
           PREPARE query1 FROM sel_where

           DECLARE cursor_m2 CURSOR FOR query1

           LET pos = 1
           FOREACH cursor_m2 INTO l_record2[pos].*
              LET pos = pos +1
           END FOREACH

           INITIALIZE l_record2[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
              DISPLAY ARRAY l_record2 TO scr_1.*
                  ON KEY ( INTERRUPT )
                     LET pos = 1
                     EXIT DISPLAY
                  ON KEY ( CONTROL-M )
                     LET pos = ARR_CURR()
                     EXIT DISPLAY
              END DISPLAY
           END IF
        END IF
        CLOSE WINDOW vent_1
        RETURN l_record2[pos].codigo,l_record2[pos].descripcion
END FUNCTION
##########################################################################
FUNCTION Despliega_city()

        LET pos = 2
        IF (pos -1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           OPEN WINDOW vent_1 AT 6,3 WITH FORM "TABM0314" ATTRIBUTE(BORDER)
           DISPLAY "                           LOCALIDADES / CIUDADES                              " AT 2,1 ATTRIBUTE(REVERSE)

           LET int_flag = FALSE

           CONSTRUCT cla_where ON ciudad_cod FROM ciudad_cod
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

           LET sel_where = "SELECT ciudad_cod,ciudad_desc FROM tab_ciudad WHERE ",
                            cla_where CLIPPED,
                           " ORDER BY 1 "
  
           PREPARE query2 FROM sel_where

           DECLARE cursor_m3 CURSOR FOR query2

           LET pos = 1
           FOREACH cursor_m3 INTO l_record3[pos].*
              LET pos = pos +1
           END FOREACH

           INITIALIZE l_record3[pos].* TO NULL

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1) 
              DISPLAY ARRAY l_record3 TO scr_1.*
                  ON KEY ( INTERRUPT )
                     LET pos = 0
                     EXIT DISPLAY
                  ON KEY ( CONTROL-M )
                     LET pos = ARR_CURR()
                     EXIT DISPLAY
              END DISPLAY
           END IF
        END IF

        CLOSE WINDOW vent_1
        RETURN l_record3[pos].codigo,l_record3[pos].descripcion
END FUNCTION
