DATABASE safre_af
################################################################################
FUNCTION Despliega_localidades()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                   L O C A L I D A D E S                 " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT local_cod,local_desc FROM tab_localidad ",
	                " WHERE local_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg1 FROM x_x
	      DECLARE cur_g1 CURSOR FOR curg1
	      LET pos = 1
	      FOREACH cur_g1 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO LOCALIDADES... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_estados()
	DEFINE aux_val		SMALLINT
	DEFINE x_x CHAR(100)
	DEFINE x_buscar CHAR(30)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                ENTIDADES   FEDERATIVAS               " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	      
	WHILE TRUE
	      LET x_x = " SELECT estad_cod,estad_desc FROM tab_estado ",
	                " WHERE estad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg2 FROM x_x
	      DECLARE cur_g2 CURSOR FOR curg2
	      LET pos = 1
	      FOREACH cur_g2 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO ESTADOS..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_sexos()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                      S  E  X  O  S                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT sexo_cod,sexo_desc FROM tab_sexo ",
	                " WHERE sexo_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg3 FROM x_x
	      DECLARE cur_g3 CURSOR FOR curg3
	      LET pos = 1
	      FOREACH cur_g3 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO SEXO..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_tipos_trabajador()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                 TIPOS   DE    TRABAJADOR                " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT tiptr_cod,tiptr_desc FROM tab_tipo_trabaj ",
	                " WHERE tiptr_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg4 FROM x_x
	      DECLARE cur_g4 CURSOR FOR curg4
	      LET pos = 1
	      FOREACH cur_g4 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO TIPO DE TRABAJADOR..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
############################################################################
FUNCTION Despliega_delegaciones()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                 D E L E G A C I O N E S                  " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT deleg_cod,deleg_desc FROM tab_delegacion ",
	                " WHERE deleg_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg5 FROM x_x
	      DECLARE cur_g5 CURSOR FOR curg5
	      LET pos = 1
	      FOREACH cur_g5 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO DELEGACIONES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_ciudades()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                     C I U D A D E S                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT ciudad_cod,ciudad_desc FROM tab_ciudad ",
	                " WHERE ciudad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg6 FROM x_x
	      DECLARE cur_g6 CURSOR FOR curg6
	      LET pos = 1
	      FOREACH cur_g6 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO CUIDADES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_parentescos()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                 P A R E N T E S C O S                   " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT paren_cod,paren_desc FROM tab_parentesco ",
	                " WHERE paren_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg7 FROM x_x
	      DECLARE cur_g7 CURSOR FOR curg7
	      LET pos = 1
	      FOREACH cur_g7 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PARENTESCO..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_promotores()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		DECIMAL(10,0)
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(100)

	OPEN WINDOW vent_1 AT 07,12 WITH FORM "PANTALLAP2" ATTRIBUTE(BORDER)
	DISPLAY "                 P R O M O T O R E S                     " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_texto = " SELECT codven,paterno,materno,nombres ",
			    " FROM pro_mae_promotor WHERE paterno MATCHES ",'"',x_buscar CLIPPED,'"'," ORDER BY 2 " CLIPPED
	      PREPARE curg8 FROM x_texto
	      DECLARE cur_g8 CURSOR FOR curg8
	      LET pos = 1
	      FOREACH cur_g8 INTO cod,pat,mat,nom
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = pat CLIPPED," ",
		                               mat CLIPPED," ",
		                               nom CLIPPED
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PROMOTORES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
		      ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
	         EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_safre_af()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                   A F O R E S                           " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT codigo_afore,razon_social FROM tab_afore_local ",
	                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg9 FROM x_x
	      DECLARE cur_g9 CURSOR FOR curg9
	      LET pos = 1
	      FOREACH cur_g9 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO AFORE..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_siefores()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                  S  I  E  A F  O  R  E  S              " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT codigo_siefore,razon_social FROM tab_siafo ",
	                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg10 FROM x_x
	      DECLARE cur_g10 CURSOR FOR curg10
	      LET pos = 1
	      FOREACH cur_g10 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO SIEAFORE..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_subcuentas()
	DEFINE aux_val		SMALLINT
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		INTEGER
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                    S U B C U E N T A S                    " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT subct_cod,subct_desc FROM tab_subcuenta ",
	                " WHERE subct_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg11 FROM x_x
	      DECLARE cur_g11 CURSOR FOR curg11
	      LET pos = 1
	      FOREACH cur_g11 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO SUBCUENTAS..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_agencias()
	DEFINE aux_val		SMALLINT
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		INTEGER
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		char(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                      A G E N C I A S                           " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT coduni_n1,nombre_uni_n1 FROM com_nivel1 ",
	                " WHERE nombre_uni_n1 MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 1,2 " CLIPPED
	      PREPARE curg12 FROM x_x
	      DECLARE cur_g12 CURSOR FOR curg12
	      LET pos = 1
	      FOREACH cur_g12 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO AGENCIAS..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_supervisores()
	DEFINE aux_val		SMALLINT
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		INTEGER
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_x		char(100),
	       x_buscar		char(30)

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
           DISPLAY "                   S U P E R V I S O R E S                " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT super_cod,super_desc FROM tab_supervisor ",
	                " WHERE super_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg13 FROM x_x
	      DECLARE cur_g13 CURSOR FOR curg13
	      LET pos = 1
	      FOREACH cur_g13 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO SUPERVISOR..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
###############################################################
FUNCTION Despliega_niveles()
	DEFINE aux_val		SMALLINT
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		INTEGER
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_x		char(100),
	       x_buscar		char(30)

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "             N I V E L E S  (CARGOS)                 " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT nivel_cod,nivel_desc FROM tab_nivel ",
	                " WHERE nivel_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg14 FROM x_x
	      DECLARE cur_g14 CURSOR FOR curg14
	      LET pos = 1
	      FOREACH cur_g14 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO NIVELES(CARGOS)..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_patrones()
	DEFINE aux_pausa		SMALLINT
	DEFINE aux_val		SMALLINT
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		char(11),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 09,2 WITH FORM "PANTALLAP" ATTRIBUTE(BORDER)
	DISPLAY "                    P A T R O N E S                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT reg_patronal,razon_social FROM tab_patron ",
	                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg15 FROM x_x
	      DECLARE cur_g15 CURSOR FOR curg15
	      LET pos = 1
	      FOREACH cur_g15 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PATRONES... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_estados_civiles()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "               E S T A D O S   C I V I L E S             " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM tab_edo_civil ",
	                " WHERE ecivi_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg16 FROM x_x
	      DECLARE cur_g16 CURSOR FOR curg16
	      LET pos = 1
	      FOREACH cur_g16 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO ESTADOS CIVILES ... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION digito_verif(valor,longitud )
  DEFINE cadena CHAR(20),
                   valor CHAR(10),
                   longitud SMALLINT,
                   suma SMALLINT,
                   sumachar CHAR(2),
                   digito SMALLINT,
                   i,j SMALLINT,
                   temp CHAR(2)
DEFINE ultima	   SMALLINT
DEFINE t SMALLINT 
define x array[10] of char(1)

       LET x[1] =valor[1]
       LET x[2] =valor[2]
       LET x[3] =valor[3]
       LET x[4] =valor[4]
       LET x[5] =valor[5]
       LET x[6] =valor[6]
       LET x[7] =valor[7]
       LET x[8] =valor[8]
       LET x[9] =valor[9]
       LET x[10] =valor[10]

FOR t = 1 TO 10
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
	LET digito = 32000
       RETURN  digito
    END IF
END FOR
  LET j = 0
  FOR i = 1 TO longitud
     LET j = j + 1
     IF i MOD 2 = 0 THEN
         LET temp = valor[i] * 2
         LET cadena[j] = temp[1]
         IF LENGTH(temp) > 1 THEN
             LET j = j + 1
             LET cadena[j] = temp[2]
         END IF
    ELSE
         LET cadena[j] = valor[i]
    END IF
  END FOR

  LET suma = 0
  FOR i = 1 TO j
     LET suma = suma + cadena[i]
  END FOR

  LET sumachar = suma
let ultima = LENGTH(sumachar)  

  LET digito = 10 - sumachar[ultima]  

  IF digito = 10 THEN
      LET digito = 0
  END IF

  RETURN digito  

END FUNCTION
################################################################################
FUNCTION valida_fecha_rfc(a)

define a char(6)
define x array[6] of char(1)
define t smallint

       LET x[1] =a[1]
       LET x[2] =a[2]
       LET x[3] =a[3]
       LET x[4] =a[4]
       LET x[5] =a[5]
       LET x[6] =a[6]

FOR t = 1 TO 6
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
       RETURN  FALSE
    END IF
END FOR
return true
end function
################################################################################
FUNCTION Despliega_pais()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		char(03),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                    P A I S E S                          " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM tab_pais ",
	                " WHERE pais_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg17 FROM x_x
	      DECLARE cur_g17 CURSOR FOR curg17
	      LET pos = 1
	      FOREACH cur_g17 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PAISES ... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_documento_probatorio()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "            TIPOS DE DOCUMENTOS PROBATORIOS              " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM tab_doc_prob ",
	                " WHERE docprob_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg18 FROM x_x
	      DECLARE cur_g18 CURSOR FOR curg18
	      LET pos = 1
	      FOREACH cur_g18 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO DOCUMENTO PROBATORIO... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_tipo_promotor()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                 TIPOS DE PROMOTOR                       " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM com_tipo_promotor ",
	                " WHERE desc_tipo MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg119 FROM x_x
	      DECLARE cur_g119 CURSOR FOR curg119
	      LET pos = 1
	      FOREACH cur_g119 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "EN ARCHIVO TIPOS DE PROMOTOR NO EXISTE EL PATRON BUSQUEDA"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_icefas()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "            I C E F A S                                  " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM tab_icefa ",
	                " WHERE icefa_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg19 FROM x_x
	      DECLARE cur_g19 CURSOR FOR curg19
	      LET pos = 1
	      FOREACH cur_g19 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO ICEFAS... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_cancelacion()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "            CODIGOS DE CANCELACION                       " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT * FROM tab_cancela ",
	                " WHERE cancel_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg20 FROM x_x
	      DECLARE cur_g20 CURSOR FOR curg20
	      LET pos = 1
	      FOREACH cur_g20 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO ICEFAS... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo#,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Verifica_documento_probatorio(x_num,x_param)
	DEFINE x_num			SMALLINT
	DEFINE x_param			CHAR(20)
	DEFINE op1_uno			CHAR(2)
	DEFINE op1_dos			CHAR(3)
	DEFINE op1_tres			CHAR(2)
	DEFINE op1_cuatro		CHAR(4)
	DEFINE op1_cinco		CHAR(5)
	#
	DEFINE numero			INTEGER
	#
	DEFINE op2_uno			CHAR(1)
	#
	DEFINE op3_uno			CHAR(9)
	DEFINE op3_dos			CHAR(7)
	#
	DEFINE op4_uno			CHAR(9)
	DEFINE op4_dos			CHAR(2)
	DEFINE op4_tres			CHAR(5)

	IF x_num <> 6 THEN
	   IF LENGTH(x_param) <> 16 THEN
	      ERROR "El Largo de Documento Probatorio debe ser 16"
	      RETURN FALSE
	   END IF
	END IF
	CASE x_num
		    ######################
	     WHEN 1 # ACTA DE NACIMIENTO #
		    ######################
		    LET op1_uno   = x_param[1,2]
		    LET op1_dos   = x_param[3,5]
		    LET op1_tres  = x_param[6,7]
		    LET op1_cuatro = x_param[8,11]
		    LET op1_cinco = x_param[12,16]
		    LET numero = op1_uno
		    SELECT "X" FROM tab_estado
		    WHERE estad_cod = numero
		    IF STATUS = NOTFOUND THEN
		       ERROR "Entidad Federativa Inexistente ( POSICION 1/2 ) ",
			op1_uno
		       RETURN FALSE
		    END IF
		    LET numero = op1_dos
		    IF numero IS NULL THEN 
			ERROR "Municipio Erroneo NO es un Numero ( POCISION 3/4/5 ) ",op1_dos RETURN FALSE
		    END IF
		    LET numero = op1_tres
		    IF numero IS NULL THEN 
			ERROR "An~o Erroneo NO es un Numero ( POCISION 6/7 ) ",op1_tres RETURN FALSE
		    END IF
		    LET numero = op1_cuatro
		    IF numero IS NULL THEN 
			ERROR "Libro Erroneo NO es un Numero ( POCISION 8/9/10/11 ) ",op1_cuatro RETURN FALSE
		    END IF
		    LET numero = op1_cinco
		    IF numero IS NULL THEN 
			ERROR "Acta Erronea NO es un Numero ( POSICION 12/13/14/15/16 ) ",op1_cinco RETURN FALSE
		    END IF
		    ######################
	     WHEN 2 # ACTA DE NACIMIENTO #
		    ######################
		    LET op2_uno = x_param[1] 
		    IF op2_uno <> " " THEN
		       ERROR "La Primera Pocision del Documento debe ser un Blanco ",op2_uno
		       RETURN FALSE
		    END IF
		    ########################
	     WHEN 3 # DOCUMENTO MIGRATORIO #
		    ########################
		    LET op3_uno = x_param[1,9] 
		    LET op3_dos = x_param[10,16] 
		    IF op3_uno <> "         " THEN
		       ERROR "La Nueve Primeras Pocisiones del Documento deben ser Blancos ",op3_uno
		       RETURN FALSE
		    END IF
		    LET numero = op3_dos
		    IF numero IS NULL THEN 
			ERROR "Reg. Nacional Extrangeros NO es Numero ( POSICION 10/11/12/13/14/15/16 ) ",op3_dos RETURN FALSE
		    END IF
		    #########################################
	     WHEN 4 # DOCUMENTO DE NATURALIZACION DE LA SRE #
		    #########################################
		    LET op4_uno = x_param[1,9] 
		    LET op4_dos = x_param[10,11] 
		    LET op4_tres = x_param[12,16] 
		    IF op4_uno <> "         " THEN
		       ERROR "La Nueve Primeras Pocisiones del Documento deben ser Blancos ",op4_uno
		       RETURN FALSE
		    END IF
		    LET numero = op4_dos
		    IF numero IS NULL THEN 
			ERROR "An~o de Registro ( POSICION 10/11 ) ",op4_dos RETURN FALSE
		    END IF
		    LET numero = op4_tres
		    IF numero IS NULL THEN 
			ERROR "N. Folio Carta ( POSICION 12/13/14/15/16 ) ",op4_tres RETURN FALSE
		    END IF
		    ##################
	     WHEN 5 # DOCUMENTO CURP #
		    ##################
		    #SE DEBE VERIFICAR ALGORITMO DE VALIDACION H.F.
	END CASE
	RETURN TRUE
END FUNCTION

FUNCTION Despliega_colonias(xcpos_cod)
   DEFINE 
      xcpos_cod         CHAR(05), 
      aux_val		SMALLINT,
      x_x 		CHAR(300),
      x_buscar 		CHAR(30),
      pos		SMALLINT,
      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint, 
         ciudad  smallint,
         estado  smallint
      end record,
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      l_reg ARRAY[1000] OF RECORD
         cod            CHAR(05),
         codigo		INTEGER,
         descripcion	CHAR(40)
      END RECORD
ERROR "BUSCANDO INFORMAICON ... "
      DECLARE cur_cp CURSOR FOR
      SELECT cpos_cod,colon_cod,colon_desc 
      FROM tab_colonia
      WHERE cpos_cod = xcpos_cod
#      ORDER BY 2 
      
      LET pos = 1
      FOREACH cur_cp INTO l_reg[pos].*
         LET pos = pos + 1
      END FOREACH
ERROR ""    
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
	 OPEN WINDOW ventana_cp AT 6,08 WITH FORM "PANTALLAP3" ATTRIBUTE (BORDER)
         DISPLAY " (ENTER) Elegir                                   (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY "                      C  O  L  O  N  I  A  S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)
     
         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

      	       SELECT deleg_cod,ciudad_cod,estad_cod 
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM tab_codpos  
               WHERE cpos_cod = l_reg[pos].cod

               SELECT deleg_desc 
               INTO desdeleg 
               FROM tab_delegacion 
               WHERE deleg_cod = reg.deleg

               SELECT ciudad_desc INTO desciuda
               FROM tab_ciudad WHERE
               ciudad_cod = reg.ciudad

               SELECT estad_desc INTO desestad
               FROM tab_estado WHERE
               estad_cod = reg.estado

               EXIT DISPLAY
            ON KEY(INTERRUPT)
               LET pos = ARR_CURR()
               LET l_reg[pos].descripcion = NULL
               LET reg.deleg = NULL
               LET desdeleg  = NULL
               LET reg.ciudad= NULL
               LET desciuda  = NULL
               LET reg.estado= NULL
               LET desestad  = NULL
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_cp
      ELSE
         ERROR "ARCHIVO DE COLONIAS ..... VACIO"
      END IF

      RETURN   
         l_reg[pos].descripcion,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad 

#      CLEAR SCREEN
END FUNCTION
FUNCTION Despliega_codigo_postal()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
               cod              char(05),
	       descrip       	char(25),
	       descripcion	CHAR(25)
	END RECORD,
      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint, 
         ciudad  smallint,
         estado  smallint
      end record,
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      vestad smallint
	DEFINE x_x		char(300),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,07 WITH FORM "PANTALLAP4" ATTRIBUTE(BORDER)
	DISPLAY "                        CODIGOS POSTALES                          " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
WHENEVER ERROR CONTINUE
PROMPT "DIGITA CODIGO ESTADO DE ESTA COLONIA ..." for vestad attribute(reverse)
WHENEVER ERROR STOP

if vestad is null then
   ERROR "SOLO PUEDE SER CODIGO" SLEEP 1
end if
if vestad is null then
   let vestad = 0
end if
ERROR "BUSCANDO INFORMACION ..."

	WHILE TRUE
           LET x_x = " SELECT c.cpos_cod,a.colon_desc,b.deleg_desc FROM ",
                     " tab_codpos c,tab_colonia a,tab_delegacion b ",
" WHERE c.cpos_cod=a.cpos_cod and c.deleg_cod=b.deleg_cod and a.colon_desc MATCHES ",'"',x_buscar CLIPPED,'"',
" and c.estad_cod=",vestad CLIPPED,
                     "   ORDER BY 2 " CLIPPED
	      PREPARE curg21 FROM x_x
	      DECLARE cur_g21 CURSOR FOR curg21
	      LET pos = 1
	      FOREACH cur_g21 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
ERROR ""
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO CODIGOS POSTALES... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
                         LET pos = ARR_CURR()
                         LET l_reg[pos].descripcion = NULL
                         LET reg.deleg = NULL
                         LET desdeleg  = NULL
                         LET reg.ciudad= NULL
                         LET desciuda  = NULL
                         LET reg.estado= NULL
                         LET desestad  = NULL
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
      	                 SELECT deleg_cod,ciudad_cod,estad_cod 
                         INTO   reg.deleg,reg.ciudad,reg.estado
                         FROM tab_codpos  
                         WHERE cpos_cod = l_reg[pos].cod
          
                         SELECT deleg_desc 
                         INTO desdeleg 
                         FROM tab_delegacion 
                         WHERE deleg_cod = reg.deleg

                         SELECT ciudad_desc INTO desciuda
                         FROM tab_ciudad WHERE
                         ciudad_cod = reg.ciudad
          
                         SELECT estad_desc INTO desestad
                         FROM tab_estado WHERE
                         estad_cod = reg.estado

	                 EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
      RETURN   
         l_reg[pos].cod,
         l_reg[pos].descrip,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad 
END FUNCTION
################################################################################
FUNCTION Despliega_area()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
	DISPLAY "                   A R E A S                             " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT

	WHILE TRUE
	      LET x_x = " SELECT area_cod,area_desc FROM tab_area ",
	                " WHERE area_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE cur28 FROM x_x
	      DECLARE cur_28 CURSOR FOR cur28
	      LET pos = 1
	      FOREACH cur_28 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO AREAS... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
