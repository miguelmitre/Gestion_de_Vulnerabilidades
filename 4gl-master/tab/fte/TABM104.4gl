################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Programa TABM104  => CATALOGO DE CODIGOS DE SIEFORE LOCAL                     ##Fecha             => 25 OCTUBRE 2004                                          #
#Autor             => ISABEL FONSECA FRIAS                                     #
#Fecha modifica    =>                                                          #
#Actualizado       =>                                                          #
#Sistema           => TAB.                                                     #
################################################################################
DATABASE safre_af
GLOBALS

	 DEFINE aux_pausa	CHAR(1),
                hoy             DATE,
                g_fecha_desde   DATE,
                usuario         CHAR(12),
		sw_1		SMALLINT,
		pos		INTEGER,
		sel_where	CHAR(30000),
		cla_where	CHAR(30000),
		gind_rdeta	SMALLINT

         DEFINE g_reg		RECORD 
		codigo_afore    SMALLINT,
                codigo_siefore  SMALLINT,
                razon_social    CHAR(50),
                representante   CHAR(50),
                calle           CHAR(50),
                numero          INTEGER,
                depto           CHAR(10),
                cod_postal      CHAR(10),
                colonia         CHAR(60),
		estado          SMALLINT,
		delegacion      SMALLINT,
	        ciudad          SMALLINT,
                telefono        CHAR(10)
              
	 END RECORD
 
         DEFINE l_record	ARRAY[3000] OF RECORD
		codigo_afore    SMALLINT,
                codigo_siefore  SMALLINT,
                razon_social    CHAR(50)
	 END RECORD

         # para ventana de estados
         DEFINE ga_est  	ARRAY[200] OF RECORD
                estad_cod       SMALLINT,
                estad_desc      CHAR(30)
	 END RECORD
         # para ventana de colonia
         DEFINE ga_col  	ARRAY[200] OF RECORD
                colon_cod       SMALLINT,
                colon_desc      CHAR(40)
	 END RECORD
         # para ventana de delegaciones
         DEFINE ga_del  	ARRAY[200] OF RECORD
                deleg_cod       SMALLINT,
                deleg_desc      CHAR(40)
	 END RECORD
         # para ventana de ciudades
         DEFINE ga_ciu  	ARRAY[200] OF RECORD
                ciudad_cod      SMALLINT,
                ciudad_desc     CHAR(40)
	 END RECORD
         
         DEFINE g_man_max         INTEGER,
                g_man_loaded      INTEGER,  
                g_man_loaded1     INTEGER,  
                g_man_loaded2     INTEGER,  
                g_man_loaded3     INTEGER,  
		vdesc_estado      CHAR(40),
		vdesc_delegacion  CHAR(40),
		vdesc_ciudad      CHAR(40),
                vcol_cod          INTEGER 

END GLOBALS
################################################################################
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

	DEFER INTERRUPT
        CALL inicio()
	CALL proceso()
END MAIN
################################################################################
FUNCTION inicio()

        SELECT USER,*
        INTO   usuario
        FROM   glo_parametro

        LET g_man_max = 200

END FUNCTION
################################################################################
FUNCTION proceso()
	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1041" ATTRIBUTE( BORDER)
	DISPLAY " TABM104                CATALOGO SIEFORE LOCAL                                 " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

        CALL w_estado()

	MENU "CATALOGO SIEFORE LOCAL"
		COMMAND "Agrega" "Agrega Siefore Local"
		        CALL Agrega()
                COMMAND "Consulta" "Consulta Siefore Local"
		        CALL Consulta()
                COMMAND "Modifica" "Modifica Siefore Local"
		        CALL Modifica()
                COMMAND "Elimina" "Elimina Siefore Local"
		        CALL Elimina()
                COMMAND "Salir" "Salir del Programa"
		        EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION Inicializa()
        LET sw_1 = 0
	INITIALIZE g_reg.* TO NULL
        INITIALIZE vdesc_estado TO NULL
        INITIALIZE vdesc_delegacion TO NULL
        INITIALIZE vdesc_ciudad TO NULL
	DISPLAY BY NAME g_reg.*
        DISPLAY BY NAME vdesc_estado
        DISPLAY BY NAME vdesc_delegacion
        DISPLAY BY NAME vdesc_ciudad
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY "(Ctrl-c) Salir                                                                 " AT 1,1 ATTRIBUTE(BOLD)
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
        LET sw_1 = 0

	INPUT BY NAME  g_reg.*

	      AFTER FIELD codigo_afore 
		    IF g_reg.codigo_afore IS NULL THEN
		       ERROR "Codigo Afore NO puede ser nulo"
		       NEXT FIELD codigo_afore
		    END IF


               AFTER FIELD codigo_siefore
		     IF g_reg.codigo_siefore IS NULL THEN
		        ERROR "Codigo Siefore NO puede ser nulo"
		        NEXT FIELD  codigo_siefore
                     ELSE
     		        SELECT "X"
		        FROM tab_siefore_local
		        WHERE codigo_afore = g_reg.codigo_afore AND
                              codigo_siefore = g_reg.codigo_siefore
		        IF STATUS <> NOTFOUND THEN
		           ERROR "Codigo Afore YA ingresado con misma Siefore"
		           NEXT FIELD codigo_afore
		        END IF
		     END IF

          
               AFTER FIELD razon_social
		     IF g_reg.razon_social IS NULL THEN
		        ERROR "Razon Social NO puede ser nula"
		        NEXT FIELD  razon_social
		     END IF

               AFTER FIELD calle
                     IF g_reg.calle IS NULL THEN
                        ERROR "Calle NO puede ser nula"
                        NEXT FIELD  calle
                     END IF

               AFTER FIELD numero
                     IF g_reg.numero IS NULL THEN
                        ERROR "Numero NO puede ser nulo"
                        NEXT FIELD  numero
                     END IF
               AFTER FIELD cod_postal
                     IF g_reg.cod_postal IS  NULL THEN
                        ERROR "Codigo NO puede ser nulo"
                        NEXT FIELD cod_postal

                     {   INITIALIZE g_reg.colonia TO NULL
                        INITIALIZE g_reg.estado TO NULL
                        INITIALIZE g_reg.delegacion TO NULL
                        INITIALIZE g_reg.ciudad TO NULL
                        INITIALIZE vdesc_estado TO NULL
                        INITIALIZE vdesc_delegacion TO NULL
                        INITIALIZE vdesc_ciudad TO NULL
                        DISPLAY BY NAME g_reg.colonia
                        DISPLAY BY NAME g_reg.estado
                        DISPLAY BY NAME g_reg.delegacion
                        DISPLAY BY NAME g_reg.ciudad
                        DISPLAY BY NAME vdesc_estado
                        DISPLAY BY NAME vdesc_delegacion
                        DISPLAY BY NAME vdesc_ciudad}
                       
                     END IF 
            # Colonia
    
               BEFORE FIELD colonia
               CALL w_colonia()
                    MESSAGE "CONTROL-B para lista de Colonia"
               ON KEY(CONTROL-B)
                  IF INFIELD(colonia) THEN
                     CALL col_wind() RETURNING vcol_cod,g_reg.colonia
                        IF int_flag = TRUE THEN
                           LET int_flag = FALSE
                           LET g_reg.colonia= NULL
                        ELSE
                           DISPLAY BY NAME g_reg.colonia
                        END IF
                  END IF

               AFTER FIELD colonia
                  IF g_reg.colonia IS NOT NULL THEN
     		     SELECT "X"
		     FROM tab_colonia
		     WHERE colon_desc = g_reg.colonia and 
                             cpos_cod = g_reg.cod_postal 
		     IF STATUS = NOTFOUND THEN
		        ERROR "No existe Colonia"
		        NEXT FIELD colonia
		     END IF
                        DISPLAY BY NAME g_reg.colonia
                  ELSE
                     INITIALIZE g_reg.colonia TO NULL
                     INITIALIZE g_reg.estado TO NULL
                     INITIALIZE g_reg.delegacion TO NULL
                     INITIALIZE g_reg.ciudad TO NULL
                     INITIALIZE vdesc_estado TO NULL
                     INITIALIZE vdesc_delegacion TO NULL
                     INITIALIZE vdesc_ciudad TO NULL
                     DISPLAY BY NAME g_reg.colonia
                     DISPLAY BY NAME g_reg.estado
                     DISPLAY BY NAME g_reg.delegacion
                     DISPLAY BY NAME g_reg.ciudad
                     DISPLAY BY NAME vdesc_estado
                     DISPLAY BY NAME vdesc_delegacion
                     DISPLAY BY NAME vdesc_ciudad
                  END IF
            # Estado

               BEFORE FIELD estado
                    MESSAGE "CONTROL-E para lista de Estado"
                ON KEY(CONTROL-E)
                  IF INFIELD(estado) THEN
                     CALL est_wind() RETURNING g_reg.estado,
                                    vdesc_estado 
                     IF int_flag = TRUE THEN
                        LET int_flag = FALSE
                        LET g_reg.estado= NULL
                     ELSE
                        DISPLAY BY NAME g_reg.estado
                        DISPLAY BY NAME vdesc_estado
                     END IF
                  END IF

               AFTER FIELD estado
                  IF g_reg.estado IS NOT NULL THEN
     		      SELECT "X"
		      FROM tab_estado
		      WHERE estad_cod = g_reg.estado 
		         IF STATUS = NOTFOUND THEN
		            ERROR "No existe Estado"
                            INITIALIZE vdesc_estado TO NULL
                            DISPLAY BY NAME vdesc_estado
		            NEXT FIELD estado
		         END IF

                         SELECT estad_desc 
                         INTO vdesc_estado
                         FROM tab_estado  
                         WHERE  g_reg.cod_postal BETWEEN cpos_desde 
                                AND cpos_hasta
                         AND g_reg.estado = estad_cod 
		        IF STATUS = NOTFOUND THEN
		           ERROR "Estado no entra en el rango de codigo postal"
                           INITIALIZE vdesc_estado TO NULL
                           DISPLAY BY NAME vdesc_estado
                        END IF
                           SELECT estad_desc 
                           INTO vdesc_estado
                           FROM tab_estado  
                           WHERE  g_reg.estado=estad_cod 
                           DISPLAY BY NAME vdesc_estado
                  ELSE
                    INITIALIZE vdesc_estado TO NULL
                    INITIALIZE vdesc_delegacion TO NULL
                    INITIALIZE vdesc_ciudad TO NULL
                    INITIALIZE g_reg.delegacion TO NULL
                    INITIALIZE g_reg.ciudad TO NULL
                    DISPLAY BY NAME vdesc_estado
                    DISPLAY BY NAME vdesc_delegacion
                    DISPLAY BY NAME vdesc_ciudad
                    DISPLAY BY NAME g_reg.delegacion 
                    DISPLAY BY NAME g_reg.ciudad
                  END IF
                
            # Delegacion

               BEFORE FIELD delegacion
               CALL w_delegacion()
                    MESSAGE "CONTROL-F para lista de Delegaciones"
                ON KEY(CONTROL-F)
                  IF INFIELD(delegacion) THEN
                    CALL del_wind() RETURNING g_reg.delegacion,
                                    vdesc_delegacion 
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.delegacion= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.delegacion
                      DISPLAY BY NAME vdesc_delegacion
                    END IF
                  END IF

               AFTER FIELD delegacion
                  IF g_reg.delegacion IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_delegacion
		        WHERE deleg_cod = g_reg.delegacion 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Delegacion"
                           INITIALIZE vdesc_delegacion TO NULL
                           DISPLAY BY NAME vdesc_delegacion
		           NEXT FIELD delegacion
		        END IF
                         SELECT deleg_desc 
                         INTO vdesc_delegacion
                         FROM tab_delegacion  
		        WHERE estad_cod = g_reg.estado AND
                              deleg_cod = g_reg.delegacion
		        IF STATUS = NOTFOUND THEN
		           ERROR "Delegacion no corresponde al Estado seleccionado"
                        END IF
                           DISPLAY BY NAME vdesc_delegacion
                  ELSE
                    INITIALIZE vdesc_delegacion TO NULL
                    INITIALIZE vdesc_ciudad TO NULL
                    INITIALIZE g_reg.delegacion TO NULL
                    INITIALIZE g_reg.ciudad TO NULL
                    DISPLAY BY NAME vdesc_delegacion
                    DISPLAY BY NAME vdesc_ciudad
                    DISPLAY BY NAME g_reg.delegacion 
                    DISPLAY BY NAME g_reg.ciudad
                  END IF

            # Ciudad

               BEFORE FIELD ciudad
               CALL w_ciudad()
                    MESSAGE "CONTROL-I para lista de Ciudades"
                ON KEY(CONTROL-I)
                  IF INFIELD(ciudad) THEN
                    CALL ciu_wind() RETURNING g_reg.ciudad,
                                    vdesc_ciudad
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.ciudad= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.ciudad
                      DISPLAY BY NAME vdesc_ciudad
                    END IF
                  END IF

               AFTER FIELD ciudad
                  IF g_reg.ciudad IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_ciudad
		        WHERE ciudad_cod = g_reg.ciudad 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Ciudad"
                           INITIALIZE vdesc_ciudad TO NULL
                           DISPLAY BY NAME vdesc_ciudad
		           NEXT FIELD ciudad
		        END IF
                         SELECT ciudad_desc 
                         INTO vdesc_ciudad
                         FROM tab_ciudad  
		        WHERE estad_cod = g_reg.estado AND
                              ciudad_cod = g_reg.ciudad
		        IF STATUS = NOTFOUND THEN
		           ERROR "Ciudad no corresponde al Estado seleccionado"
                        END IF
                           DISPLAY BY NAME vdesc_ciudad
                  ELSE
                        INITIALIZE vdesc_ciudad TO NULL
                        DISPLAY BY NAME vdesc_ciudad
                  END IF

               AFTER FIELD telefono

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN

                     INSERT INTO tab_siefore_local 
                                 VALUES(g_reg.codigo_afore,
                                        g_reg.codigo_siefore,                                                           g_reg.razon_social,
                                        g_reg.representante,                                                            g_reg.calle,
                                        g_reg.numero,
                                        g_reg.depto,
                                        g_reg.colonia,
                                        g_reg.delegacion,
                                        g_reg.ciudad,
                                        g_reg.estado,
                                        g_reg.cod_postal,
                                        g_reg.telefono)

                     INSERT INTO tab_his_siefore_local
                                 VALUES(g_reg.codigo_afore,
                                        g_reg.codigo_siefore,
                                        g_reg.razon_social,
                                        g_reg.representante,
                                        g_reg.calle,
                                        g_reg.numero,
                                        g_reg.depto,
                                        g_reg.colonia,
                                        g_reg.delegacion,
                                        g_reg.ciudad, 
                                        g_reg.estado,
                                        g_reg.cod_postal,
                                        g_reg.telefono,
                                        hoy,
                                        hoy,
                                        usuario)


		     ERROR "REGISTRO INGRESADO"

                     SLEEP 2
		     ERROR ""
                     CALL Inicializa()
		       NEXT FIELD codigo_afore
                 END IF
              ON KEY (INTERRUPT)
                       CALL Inicializa()
                       EXIT INPUT
	END INPUT
END FUNCTION

################################################################################
FUNCTION Consulta()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1042" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Consulta                                             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Siefore a Consultar                          " AT 2,1
	   DISPLAY "                  CATALOGO SIEFORE LOCAL                                       " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON codigo_afore,codigo_siefore,razon_social
                FROM codigo_afore,codigo_siefore,razon_social
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
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

	LET sel_where = "SELECT  codigo_afore,codigo_siefore,razon_social " ,
                         "FROM tab_siefore_local WHERE " 
        	      	 ,cla_where CLIPPED,
                         "ORDER BY 1,2 "
	   PREPARE query FROM sel_where

	   DECLARE cursor_1 CURSOR FOR query

   	   LET pos = 1

	   FOREACH cursor_1 INTO l_record[pos].*
	      LET pos = pos + 1
	   END FOREACH

	   INITIALIZE l_record[pos].* TO NULL
	   IF (pos-1) >= 1 THEN
	      CALL  SET_COUNT(pos-1)
              ERROR ""	
	      DISPLAY ARRAY l_record TO scr_1.*
		    ON KEY (control-m)
		       LET pos = ARR_CURR()
		       LET g_reg.codigo_afore = l_record[pos].codigo_afore
		       LET g_reg.codigo_siefore = l_record[pos].codigo_siefore
                       LET g_reg.razon_social = l_record[pos].razon_social
		       EXIT DISPLAY
                      ON KEY (INTERRUPT) 
		         LET int_flag = TRUE
		         EXIT DISPLAY 
	      END DISPLAY
              IF int_flag = TRUE THEN
                 LET int_flag = FALSE
                 ERROR "BUSQUEDA CANCELADA..."
                 SLEEP 2
                 ERROR ""
                 CLEAR SCREEN
                 CLOSE WINDOW ventana_2
                 RETURN
              END IF

              CLOSE WINDOW ventana_2
           ELSE
              ERROR "ARCHIVO DE SIEFORE LOCAL.....VACIO"
              SLEEP 2
              ERROR ""
              CLOSE WINDOW ventana_2
              RETURN
           END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " CONSULTA  " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

              SELECT codigo_afore,codigo_siefore,razon_social,representante,
                     calle,numero,depto,colonia,delegacion,ciudad,estado,
                     cod_postal,telefono
              INTO   g_reg.codigo_afore,g_reg.codigo_siefore,g_reg.razon_social,                     g_reg.representante,g_reg.calle,g_reg.numero,g_reg.depto,
                     g_reg.colonia,g_reg.delegacion,g_reg.ciudad,g_reg.estado,
                     g_reg.cod_postal,g_reg.telefono
              FROM tab_siefore_local
              WHERE codigo_afore = g_reg.codigo_afore AND
                   codigo_siefore = g_reg.codigo_siefore

              # Descripción de Estado
              SELECT estad_desc
              INTO vdesc_estado
              FROM tab_estado
              WHERE estad_cod = g_reg.estado
              DISPLAY BY NAME vdesc_estado

              # Descripcion de delegacion
              SELECT deleg_desc
              INTO vdesc_delegacion
              FROM tab_delegacion
              WHERE deleg_cod = g_reg.delegacion
              DISPLAY BY NAME vdesc_delegacion
  
              # Descripcion de ciudad
              SELECT ciudad_desc
              INTO vdesc_ciudad
              FROM tab_ciudad
              WHERE ciudad_cod = g_reg.ciudad
              DISPLAY BY NAME vdesc_ciudad

	   DISPLAY BY NAME g_reg.* 
	   PROMPT "" FOR CHAR aux_pausa
 
	ELSE
	   ERROR "ARCHIVO DE CODIGO DE SIEFORE LOCAL... VACIO."
	END IF
        CALL Inicializa()
	CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION  Modifica()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1042" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Modifica                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Siefore a Modificar                          " AT 2,1
	   DISPLAY "                  CATALOGO SIEFORE LOCAL                                       " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON codigo_afore,codigo_siefore,razon_social
                FROM codigo_afore,codigo_siefore,razon_social
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
		SLEEP 2
		ERROR ""
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

	LET sel_where = "SELECT  codigo_afore,codigo_siefore,razon_social " ,
                         "FROM tab_siefore_local WHERE " 
        	      	 ,cla_where CLIPPED,
                         "ORDER BY 1,2 "
	   PREPARE query1 FROM sel_where

	   DECLARE cursor_2 CURSOR FOR query1

   	   LET pos = 1

	   FOREACH cursor_2 INTO l_record[pos].*
	      LET pos = pos + 1
	   END FOREACH

	   INITIALIZE l_record[pos].* TO NULL
	   IF (pos-1) >= 1 THEN
	      CALL  SET_COUNT(pos-1)
              ERROR ""	
	      DISPLAY ARRAY l_record TO scr_1.*
		    ON KEY (control-m)
		       LET pos = ARR_CURR()
		       LET g_reg.codigo_afore = l_record[pos].codigo_afore
		       LET g_reg.codigo_siefore = l_record[pos].codigo_siefore
                       LET g_reg.razon_social = l_record[pos].razon_social
		       EXIT DISPLAY
                      ON KEY (INTERRUPT) 
	               LET int_flag = TRUE
		       EXIT DISPLAY
	      END DISPLAY
	   IF int_flag = TRUE THEN
	      LET int_flag = FALSE
	      ERROR "BUSQUEDA CANCELADA..."
	      SLEEP 2
	      ERROR ""
	      CLEAR SCREEN
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF
	      CLOSE WINDOW ventana_2
	   ELSE
	      ERROR "ARCHIVO DE CODIGOS DE SIEFORE LOCAL... VACIO"
              SLEEP 2
	      ERROR ""
	      CLOSE WINDOW ventana_2
              RETURN
	   END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

              SELECT codigo_afore,codigo_siefore,razon_social,representante,
                     calle,numero,depto,colonia,delegacion,ciudad,estado,
                     cod_postal,telefono
              INTO   g_reg.codigo_afore,g_reg.codigo_siefore,g_reg.razon_social,                     g_reg.representante,g_reg.calle,g_reg.numero,g_reg.depto,
                     g_reg.colonia,g_reg.delegacion,g_reg.ciudad,g_reg.estado,
                     g_reg.cod_postal,g_reg.telefono
              FROM tab_siefore_local
              WHERE codigo_afore = g_reg.codigo_afore AND
                   codigo_siefore = g_reg.codigo_siefore

              # Descripción de Estado
              SELECT estad_desc
              INTO vdesc_estado
              FROM tab_estado
              WHERE estad_cod = g_reg.estado
              DISPLAY BY NAME vdesc_estado

              # Descripcion de delegacion
              SELECT deleg_desc
              INTO vdesc_delegacion
              FROM tab_delegacion
              WHERE deleg_cod = g_reg.delegacion
              DISPLAY BY NAME vdesc_delegacion
  
              # Descripcion de ciudad
              SELECT ciudad_desc
              INTO vdesc_ciudad
              FROM tab_ciudad
              WHERE ciudad_cod = g_reg.ciudad
              DISPLAY BY NAME vdesc_ciudad

              INPUT BY NAME g_reg.* WITHOUT DEFAULTS
              BEFORE FIELD codigo_afore
              NEXT FIELD razon_social
              BEFORE FIELD codigo_siefore
              NEXT FIELD razon_social

	      AFTER FIELD razon_social
		IF g_reg.razon_social IS NULL THEN
		   ERROR "razon_social NO debe ser nula."
		   NEXT FIELD razon_social
		END IF

               AFTER FIELD calle
                     IF g_reg.calle IS NULL THEN
                        ERROR "Calle NO puede ser nula"
                        NEXT FIELD  calle
                     END IF

               AFTER FIELD numero
                     IF g_reg.numero IS NULL THEN
                        ERROR "Numero NO puede ser nulo"
                        NEXT FIELD  numero
                     END IF


               AFTER FIELD cod_postal 
                  IF g_reg.cod_postal IS  NULL THEN
                        ERROR "Codigo Postal NO puede ser nulo"
                        NEXT FIELD cod_postal
                      {  INITIALIZE g_reg.colonia TO NULL
                        INITIALIZE g_reg.estado TO NULL
                        INITIALIZE g_reg.delegacion TO NULL
                        INITIALIZE g_reg.ciudad TO NULL
                        INITIALIZE vdesc_estado TO NULL
                        INITIALIZE vdesc_delegacion TO NULL
                        INITIALIZE vdesc_ciudad TO NULL
                        DISPLAY BY NAME g_reg.colonia
                        DISPLAY BY NAME g_reg.estado
                        DISPLAY BY NAME g_reg.delegacion
                        DISPLAY BY NAME g_reg.ciudad
                        DISPLAY BY NAME vdesc_estado
                        DISPLAY BY NAME vdesc_delegacion
                        DISPLAY BY NAME vdesc_ciudad}
                  END IF 

            # Colonia
    
               BEFORE FIELD colonia
               CALL w_colonia()
                    MESSAGE "CONTROL-B para lista de Colonia"
                ON KEY(CONTROL-B)
                  IF INFIELD(colonia) THEN
                    CALL col_wind() RETURNING vcol_cod,g_reg.colonia
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.colonia= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.colonia
                    END IF
                  END IF

               AFTER FIELD colonia
                  IF g_reg.colonia IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_colonia
		        WHERE colon_desc = g_reg.colonia and 
                              cpos_cod = g_reg.cod_postal 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Colonia"
		           NEXT FIELD colonia
		        END IF
                        DISPLAY BY NAME g_reg.colonia
                  ELSE
                        INITIALIZE g_reg.colonia TO NULL
                        DISPLAY BY NAME g_reg.colonia
                  END IF
            # Estado

               BEFORE FIELD estado
                    MESSAGE "CONTROL-E para lista de Estado"
                ON KEY(CONTROL-E)
                  IF INFIELD(estado) THEN
                    CALL est_wind() RETURNING g_reg.estado,
                                    vdesc_estado 
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.estado= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.estado
                      DISPLAY BY NAME vdesc_estado
                    END IF
                  END IF

               AFTER FIELD estado
                  IF g_reg.estado IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_estado
		        WHERE estad_cod = g_reg.estado 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Estado"
                           INITIALIZE vdesc_estado TO NULL
                           DISPLAY BY NAME vdesc_estado
		           NEXT FIELD estado
		        END IF

                         SELECT estad_desc 
                         INTO vdesc_estado
                         FROM tab_estado  
                         WHERE  g_reg.cod_postal BETWEEN cpos_desde 
                                AND cpos_hasta
                         AND g_reg.estado = estad_cod 
		        IF STATUS = NOTFOUND THEN
		           ERROR "Estado no entra en el rango de codigo postal"
                           INITIALIZE vdesc_estado TO NULL
                           DISPLAY BY NAME vdesc_estado
                        END IF
                           SELECT estad_desc 
                           INTO vdesc_estado
                           FROM tab_estado  
                           WHERE  g_reg.estado=estad_cod 
                           DISPLAY BY NAME vdesc_estado
                  ELSE
                    INITIALIZE vdesc_estado TO NULL
                    INITIALIZE vdesc_delegacion TO NULL
                    INITIALIZE vdesc_ciudad TO NULL
                    INITIALIZE g_reg.delegacion TO NULL
                    INITIALIZE g_reg.ciudad TO NULL
                    DISPLAY BY NAME vdesc_estado
                    DISPLAY BY NAME vdesc_delegacion
                    DISPLAY BY NAME vdesc_ciudad
                    DISPLAY BY NAME g_reg.delegacion 
                    DISPLAY BY NAME g_reg.ciudad
                  END IF
                
            # Delegacion

               BEFORE FIELD delegacion
               CALL w_delegacion()
                    MESSAGE "CONTROL-F para lista de Delegaciones"
                ON KEY(CONTROL-F)
                  IF INFIELD(delegacion) THEN
                    CALL del_wind() RETURNING g_reg.delegacion,
                                    vdesc_delegacion 
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.delegacion= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.delegacion
                      DISPLAY BY NAME vdesc_delegacion
                    END IF
                  END IF

               AFTER FIELD delegacion
                  IF g_reg.delegacion IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_delegacion
		        WHERE deleg_cod = g_reg.delegacion 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Delegacion"
                           INITIALIZE vdesc_delegacion TO NULL
                           DISPLAY BY NAME vdesc_delegacion
		           NEXT FIELD delegacion
		        END IF
                         SELECT deleg_desc 
                         INTO vdesc_delegacion
                         FROM tab_delegacion  
		        WHERE estad_cod = g_reg.estado AND
                              deleg_cod = g_reg.delegacion
		        IF STATUS = NOTFOUND THEN
		           ERROR "Delegacion no corresponde al Estado seleccionado"
                        END IF
                           DISPLAY BY NAME vdesc_delegacion
                  ELSE
                    INITIALIZE vdesc_delegacion TO NULL
                    INITIALIZE vdesc_ciudad TO NULL
                    INITIALIZE g_reg.delegacion TO NULL
                    INITIALIZE g_reg.ciudad TO NULL
                    DISPLAY BY NAME vdesc_delegacion
                    DISPLAY BY NAME vdesc_ciudad
                    DISPLAY BY NAME g_reg.delegacion 
                    DISPLAY BY NAME g_reg.ciudad
                  END IF

            # Ciudad

               BEFORE FIELD ciudad
               CALL w_ciudad()
                    MESSAGE "CONTROL-I para lista de Ciudades"
                ON KEY(CONTROL-I)
                  IF INFIELD(ciudad) THEN
                    CALL ciu_wind() RETURNING g_reg.ciudad,
                                    vdesc_ciudad
                    IF int_flag = TRUE THEN
                      LET int_flag = FALSE
                      LET g_reg.ciudad= NULL
                    ELSE
                      DISPLAY BY NAME g_reg.ciudad
                      DISPLAY BY NAME vdesc_ciudad
                    END IF
                  END IF

               AFTER FIELD ciudad
                  IF g_reg.ciudad IS NOT NULL THEN
     		        SELECT "X"
		        FROM tab_ciudad
		        WHERE ciudad_cod = g_reg.ciudad 
		        IF STATUS = NOTFOUND THEN
		           ERROR "No existe Ciudad"
                           INITIALIZE vdesc_ciudad TO NULL
                           DISPLAY BY NAME vdesc_ciudad
		           NEXT FIELD ciudad
		        END IF
                         SELECT ciudad_desc 
                         INTO vdesc_ciudad
                         FROM tab_ciudad  
		        WHERE estad_cod = g_reg.estado AND
                              ciudad_cod = g_reg.ciudad
		        IF STATUS = NOTFOUND THEN
		           ERROR "Ciudad no corresponde al Estado seleccionado"
                        END IF
                           DISPLAY BY NAME vdesc_ciudad
                  ELSE
                        INITIALIZE vdesc_ciudad TO NULL
                        DISPLAY BY NAME vdesc_ciudad
                  END IF

               AFTER FIELD telefono

		CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN

                   SELECT MIN(fecha_desde)
                   INTO   g_fecha_desde 
                   FROM tab_his_siefore_local
                   WHERE codigo_afore   = g_reg.codigo_afore
                   AND codigo_siefore = g_reg.codigo_siefore

		   UPDATE tab_siefore_local SET
			razon_social = g_reg.razon_social,
			representante = g_reg.representante,
			calle = g_reg.calle,
			numero = g_reg.numero,
			depto = g_reg.depto,
			colonia = g_reg.colonia,
			delegacion = g_reg.delegacion,
			ciudad = g_reg.ciudad,
			estado = g_reg.estado,
			cod_postal = g_reg.cod_postal,
			telefono = g_reg.telefono 
		   WHERE codigo_afore = g_reg.codigo_afore and
                         codigo_siefore = g_reg.codigo_siefore

                     INSERT INTO tab_his_siefore_local
                                 VALUES(g_reg.codigo_afore,
                                        g_reg.codigo_siefore,
                                        g_reg.razon_social,
                                        g_reg.representante,
                                        g_reg.calle,
                                        g_reg.numero,
                                        g_reg.depto,
                                        g_reg.colonia,
                                        g_reg.delegacion,
                                        g_reg.ciudad, 
                                        g_reg.estado,
                                        g_reg.cod_postal,
                                        g_reg.telefono,
                                        g_fecha_desde,
                                        hoy,
                                        usuario)
		   ERROR "REGISTRO MODIFICADO"
		   SLEEP 2
		   ERROR ""

		   CALL Inicializa()
		ELSE
		   ERROR "PROCESO DE MODIFICAR CANCELADO."
		   SLEEP 2
		   ERROR ""
		   CALL Inicializa()
		END IF
		EXIT INPUT
	   ON KEY (INTERRUPT)
		CALL Inicializa()
		EXIT INPUT
	   END INPUT
	ELSE
	   ERROR "ARCHIVO DE CODIGO DE SIEFORE LOCAL... VACIO."
	END IF
	CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION Pregunta()
	PROMPT "Esta seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION est_wind()
	 DEFINE pa_elem 	SMALLINT
         OPEN WINDOW w_est AT 3,40
 
            WITH FORM "TABM1043" ATTRIBUTE(BORDER)

	 DISPLAY "(Enter) Selecciona        (Ctrl-C)Salir" AT 1,1
	 DISPLAY "          CATALOGO DE ESTADOS          " AT 2,1 ATTRIBUTE(REVERSE)

         CALL set_count(g_man_loaded)
         LET int_flag = FALSE
         DISPLAY ARRAY ga_est TO scr_2.*
		ON KEY (control-m)
                EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW w_est
         LET pa_elem = arr_curr()
         RETURN ga_est[pa_elem].estad_cod,ga_est[pa_elem].estad_desc

END FUNCTION
################################################################################
FUNCTION col_wind()
	 DEFINE pa_elem1 	SMALLINT
         OPEN WINDOW w_col AT 3,40
 
            WITH FORM "TABM1044" ATTRIBUTE(BORDER)

	 DISPLAY "(Enter) Selecciona        (Ctrl-C)Salir" AT 1,1
	 DISPLAY "          CATALOGO DE COLONIA          " AT 2,1 ATTRIBUTE(REVERSE)

         LET g_man_loaded1 = g_man_loaded1 - 1
         CALL set_count(g_man_loaded1)
         LET int_flag = FALSE
         DISPLAY ARRAY ga_col TO scr_3.*
		ON KEY (control-m)
                EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW w_col
         LET pa_elem1 = arr_curr()
         RETURN ga_col[pa_elem1].colon_cod,ga_col[pa_elem1].colon_desc

END FUNCTION
################################################################################
FUNCTION  Elimina()
	LET pos = 2
	IF (pos-1) >= 1 THEN
	   CALL SET_COUNT(pos-1)
	   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1042" ATTRIBUTE( BORDER)
	   DISPLAY " (Enter) Elimina                                             (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "               Escoja con <ENTER> Siefore a Eliminar                           " AT 2,1
	   DISPLAY "                CATALOGO DE SIEFORE LOCAL                                       " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 

	   LET int_flag = FALSE

	   CONSTRUCT cla_where ON codigo_afore,codigo_siefore,razon_social
                FROM codigo_afore,codigo_siefore,razon_social
	      ON KEY (control-m)
		ERROR "PROCESANDO INFORMACION..."
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

	LET sel_where = "SELECT  codigo_afore,codigo_siefore,razon_social " ,
                         "FROM tab_siefore_local WHERE " 
        	      	 ,cla_where CLIPPED,
                         "ORDER BY 1,2 "
	   PREPARE query2 FROM sel_where

	   DECLARE cursor_3 CURSOR FOR query2

   	   LET pos = 1

	   FOREACH cursor_3 INTO l_record[pos].*
	      LET pos = pos + 1
	   END FOREACH

	   INITIALIZE l_record[pos].* TO NULL
	   IF (pos-1) >= 1 THEN
	      CALL  SET_COUNT(pos-1)
              ERROR ""	
	      DISPLAY ARRAY l_record TO scr_1.*
		    ON KEY (control-m)
		       LET pos = ARR_CURR()
		       LET g_reg.codigo_afore = l_record[pos].codigo_afore
		       LET g_reg.codigo_siefore = l_record[pos].codigo_siefore
                       LET g_reg.razon_social = l_record[pos].razon_social
		       EXIT DISPLAY
                      ON KEY (INTERRUPT) 
		         LET int_flag = TRUE
		         EXIT DISPLAY 
	      END DISPLAY
              IF int_flag = TRUE THEN
                 LET int_flag = FALSE
                 ERROR "BUSQUEDA CANCELADA..."
                 SLEEP 2
                 ERROR ""
                 CLEAR SCREEN
                 CLOSE WINDOW ventana_2
                 RETURN
              END IF
              CLOSE WINDOW ventana_2
           ELSE
              ERROR "ARCHIVO DE SIEFORE LOCAL.....VACIO"
              SLEEP 2
              ERROR ""
              CLOSE WINDOW ventana_2
              RETURN
           END IF

	   DISPLAY "" AT 1,1
	   DISPLAY "" AT 2,1
	   DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
	   DISPLAY " ELIMINA  " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

              SELECT codigo_afore,codigo_siefore,razon_social,representante,
                     calle,numero,depto,colonia,delegacion,ciudad,estado,
                     cod_postal,telefono
              INTO   g_reg.codigo_afore,g_reg.codigo_siefore,g_reg.razon_social,                     g_reg.representante,g_reg.calle,g_reg.numero,g_reg.depto,
                     g_reg.colonia,g_reg.delegacion,g_reg.ciudad,g_reg.estado,
                     g_reg.cod_postal,g_reg.telefono
              FROM tab_siefore_local
              WHERE codigo_afore = g_reg.codigo_afore AND
                   codigo_siefore = g_reg.codigo_siefore

              # Descripción de Estado
              SELECT estad_desc
              INTO vdesc_estado
              FROM tab_estado
              WHERE estad_cod = g_reg.estado
              DISPLAY BY NAME vdesc_estado

              # Descripcion de delegacion
              SELECT deleg_desc
              INTO vdesc_delegacion
              FROM tab_delegacion
              WHERE deleg_cod = g_reg.delegacion
              DISPLAY BY NAME vdesc_delegacion
  
              # Descripcion de ciudad
              SELECT ciudad_desc
              INTO vdesc_ciudad
              FROM tab_ciudad
              WHERE ciudad_cod = g_reg.ciudad
              DISPLAY BY NAME vdesc_ciudad
	   DISPLAY BY NAME g_reg.* 

                CALL Pregunta()

		IF aux_pausa MATCHES "[Ss]" THEN
 
		   DELETE FROM tab_siefore_local
		   WHERE codigo_afore = g_reg.codigo_afore and
                         codigo_siefore = g_reg.codigo_siefore
		   ERROR "REGISTRO ELIMINADO"
		   SLEEP 2
		   ERROR ""
          
		ELSE
		   ERROR "PROCESO CANCELADO."
		   SLEEP 2
		   ERROR ""
		END IF
                CALL Inicializa()
	ELSE
	   ERROR "ARCHIVO DE CODIGO DE SIEFORE LOCAL... VACIO."
	END IF
	CLEAR SCREEN

END FUNCTION
#############################################################################
FUNCTION  w_estado()
        DECLARE manu_ptr CURSOR FOR
          SELECT estad_cod,estad_desc
          FROM tab_estado
          ORDER BY estad_cod

        LET g_man_loaded = 1
        FOREACH manu_ptr INTO ga_est[g_man_loaded].*
           LET g_man_loaded = g_man_loaded + 1
           IF g_man_loaded > g_man_max THEN
              EXIT FOREACH
           END IF
        END FOREACH
        FREE manu_ptr
END FUNCTION
#############################################################################
FUNCTION  w_colonia()
        DECLARE manu_ptr1 CURSOR FOR
        
          SELECT colon_cod,colon_desc
          FROM tab_colonia 
          WHERE cpos_cod = g_reg.cod_postal

        LET g_man_loaded1 = 1
        INITIALIZE ga_col[g_man_loaded1].* TO NULL

        FOREACH manu_ptr1 INTO ga_col[g_man_loaded1].*
           LET g_man_loaded1 = g_man_loaded1 + 1
          IF g_man_loaded1 > g_man_max  THEN
              EXIT FOREACH
           END IF
        END FOREACH
        FREE manu_ptr1

END FUNCTION
#############################################################################
FUNCTION  w_delegacion()
        DECLARE manu_ptr2 CURSOR FOR
        
          SELECT deleg_cod,deleg_desc
          FROM tab_delegacion 
          WHERE estad_cod = g_reg.estado

        LET g_man_loaded2 = 1
        INITIALIZE ga_del[g_man_loaded2].* TO NULL

        FOREACH manu_ptr2 INTO ga_del[g_man_loaded2].*
          IF g_man_loaded2 > g_man_max  THEN
              EXIT FOREACH
           END IF
           LET g_man_loaded2 = g_man_loaded2 + 1
        END FOREACH
        FREE manu_ptr2

END FUNCTION
#############################################################################
FUNCTION  w_ciudad()
        DECLARE manu_ptr3 CURSOR FOR
        
          SELECT ciudad_cod,ciudad_desc
          FROM tab_ciudad 
          WHERE estad_cod = g_reg.estado

        LET g_man_loaded3 = 1
        INITIALIZE ga_ciu[g_man_loaded3].* TO NULL

        FOREACH manu_ptr3 INTO ga_ciu[g_man_loaded3].*
          IF g_man_loaded3 > g_man_max  THEN
              EXIT FOREACH
           END IF
           LET g_man_loaded3 = g_man_loaded3 + 1
        END FOREACH
        FREE manu_ptr3

END FUNCTION
################################################################################
FUNCTION del_wind()
	 DEFINE pa_elem2 	SMALLINT
         OPEN WINDOW w_del AT 3,40
 
            WITH FORM "TABM1045" ATTRIBUTE(BORDER)

	 DISPLAY "(Enter) Selecciona        (Ctrl-C)Salir" AT 1,1
	 DISPLAY "        CATALOGO DE DELEGACION         " AT 2,1 ATTRIBUTE(REVERSE)

         LET g_man_loaded2 = g_man_loaded2 - 1
         CALL set_count(g_man_loaded2)
         LET int_flag = FALSE
         DISPLAY ARRAY ga_del TO scr_4.*
		ON KEY (control-m)
                EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW w_del
         LET pa_elem2 = arr_curr()
         RETURN ga_del[pa_elem2].deleg_cod,ga_del[pa_elem2].deleg_desc

END FUNCTION
################################################################################
FUNCTION ciu_wind()
	 DEFINE pa_elem3 	SMALLINT
         OPEN WINDOW w_ciu AT 3,40
 
            WITH FORM "TABM1046" ATTRIBUTE(BORDER)

	 DISPLAY "(Enter) Selecciona        (Ctrl-C)Salir" AT 1,1
	 DISPLAY "         CATALOGO DE CIUDADES          " AT 2,1 ATTRIBUTE(REVERSE)

         LET g_man_loaded3 = g_man_loaded3 - 1
         CALL set_count(g_man_loaded3)
         LET int_flag = FALSE
         DISPLAY ARRAY ga_ciu TO scr_5.*
		ON KEY (control-m)
                EXIT DISPLAY
	 END DISPLAY
         CLOSE WINDOW w_ciu
         LET pa_elem3 = arr_curr()
         RETURN ga_ciu[pa_elem3].ciudad_cod,ga_ciu[pa_elem3].ciudad_desc

END FUNCTION
###########################################################################
FUNCTION val1_colonia()
END FUNCTION
