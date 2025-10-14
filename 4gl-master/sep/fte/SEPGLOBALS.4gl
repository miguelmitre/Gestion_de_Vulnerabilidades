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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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

FUNCTION Despliega_estados()
	DEFINE aux_val		SMALLINT
	DEFINE x_x CHAR(100)
	DEFINE x_buscar CHAR(30)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	DEFINE cod      		CHAR(10)
	DEFINE pat,mat,nom	CHAR(60)
	DEFINE l_reg ARRAY[2000] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(200)
define enter char(1)
	OPEN WINDOW vent_1 AT 07,12 WITH FORM "AFIM00G2" ATTRIBUTE(BORDER)
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
	      LET x_texto = " SELECT cod_promotor,paterno,materno,nombres ",
			              " FROM pro_mae_promotor WHERE paterno MATCHES ",
                       '"',x_buscar CLIPPED,'"'," ORDER BY 2 " CLIPPED

WHENEVER ERROR CONTINUE
              ERROR "Buscando Informacion"
	      PREPARE curg8 FROM x_texto
	      DECLARE cur_8 CURSOR FOR curg8
	      LET pos = 1
	      FOREACH cur_8 INTO cod,pat,mat,nom
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = pat CLIPPED," ",
		                               mat CLIPPED," ",
		                               nom CLIPPED
		      LET pos = pos + 1
		      IF pos > 2000 THEN
			 ERROR "Fue Sobrepasada la capacidad del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
              free curg8
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
WHENEVER ERROR STOP
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_referente()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		CHAR(10)
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[2000] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(100)

	OPEN WINDOW vent_1 AT 07,12 WITH FORM "AFIM00G2" ATTRIBUTE(BORDER)
	DISPLAY "                 R E F E R E N T E S                     " AT 2,1 ATTRIBUTE(REVERSE)
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
	      LET x_texto = " SELECT nomina_cod,paterno,materno,nombres ",
			    " FROM com_nomina WHERE paterno MATCHES ",
                            '"',x_buscar CLIPPED,'"'," ORDER BY 2 " CLIPPED
WHENEVER ERROR CONTINUE
              ERROR "Buscando Informacion"
	      PREPARE currf FROM x_texto
	      DECLARE cur_rf8 CURSOR FOR currf
	      LET pos = 1
	      FOREACH cur_rf8 INTO cod,pat,mat,nom
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = pat CLIPPED," ",
		                               mat CLIPPED," ",
		                               nom CLIPPED
		      LET pos = pos + 1
		      IF pos > 2000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
              free currf
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO REFERENTES..... VACIO"
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
WHENEVER ERROR STOP
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_afores()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(300),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
define enter char(1)
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	      LET x_x = " SELECT @afore_cod, @afore_desc FROM tab_afore ",
	                " WHERE @afore_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                        " AND marca <> 1 ",
	                " ORDER BY 2 " CLIPPED
display x_x
prompt "" for enter
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
	DISPLAY "                  S  I  E  F  O  R  E  S              " AT 2,1 ATTRIBUTE(REVERSE)
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
	      LET x_x = " SELECT siefore_cod,siefore_desc FROM tab_siefore ",
	                " WHERE siefore_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                        " AND marca = 1 ",
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 09,2 WITH FORM "AFIM00G0" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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

FUNCTION Despliega_documento_probatorio()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
	DISPLAY "              TIPOS DE DOCUMENTO PROBATORIO              " AT 2,1 ATTRIBUTE(REVERSE)
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
	                " ORDER BY 1 " CLIPPED
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	                " ORDER BY 1 " CLIPPED
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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

FUNCTION Verifica_documento_probatorio(x_num,x_param)

    DEFINE x_num      SMALLINT
    DEFINE x_param    CHAR(16)

    DEFINE op1_uno    CHAR(2)
    DEFINE op1_dos    CHAR(3)
    DEFINE op1_tres   CHAR(2)
    DEFINE op1_cuatro CHAR(4)
    DEFINE op1_cinco  CHAR(5)

    DEFINE numero     INTEGER
    DEFINE enter      CHAR(1)

    DEFINE op2_uno    CHAR(1)
    DEFINE op2_dos    CHAR(2)
    DEFINE op2_tres   CHAR(3)

    DEFINE op3_uno    CHAR(9)
    DEFINE op3_dos    CHAR(7)

    DEFINE op4_uno    CHAR(9)
    DEFINE op4_dos    CHAR(4)
    DEFINE op4_tres   CHAR(5)

    DEFINE act_edo     SMALLINT
    DEFINE act_mun     INTEGER
    DEFINE act_fen     SMALLINT
    DEFINE act_lib     CHAR(4)
    DEFINE act_act     CHAR(5)
    DEFINE act_edo_mun CHAR(5)

    DEFINE crip_reg    CHAR(14)
    DEFINE crip_num    DECIMAL(15,0)
    DEFINE crip_edo    SMALLINT
    DEFINE crip_dig    CHAR(1)
    DEFINE crip_anyo   CHAR(2)
    DEFINE crip_v_a    CHAR(1)
    DEFINE crip_f_a    CHAR(4)
    DEFINE crip_v_n    SMALLINT
    DEFINE crip_n_a    SMALLINT

    DEFINE dcto_anyo  CHAR(4)
    DEFINE dcto_1900  SMALLINT

    LET dcto_anyo = YEAR(TODAY)

    IF x_num <> 6 AND 
       x_num <> 5 THEN
        IF LENGTH(x_param) <> 16 THEN
            ERROR "El Largo de Documento Probatorio debe ser 16"
            RETURN FALSE
        END IF
    END IF

    LET numero = 0

    CASE x_num
        WHEN 1 # ACTA DE NACIMIENTO #
            LET op1_uno    = x_param[1,2]
            LET op1_dos    = x_param[3,5]
            LET op1_tres   = x_param[6,7]
            LET op1_cuatro = x_param[8,11]
            LET op1_cinco  = x_param[12,16]
            LET numero     = op1_uno

            ---- Valida entidad federativa

            LET act_edo = op1_uno

            IF act_edo < 1 OR
               act_edo > 32 THEN
                IF act_edo <> 35 AND
                   act_edo <> 39 THEN
                    ERROR "Entidad Federativa Inexistente (posicion 1-2) ", 
                          op1_uno
                    RETURN FALSE
                END IF
            END IF

            SELECT "X"
            FROM   tab_estado
            WHERE  estad_cod = numero

            IF STATUS = NOTFOUND THEN
                ERROR "Entidad Federativa Inexistente (posicion 1-2) ", op1_uno
                RETURN FALSE
            END IF

            ---- Valida municipio

            LET numero = op1_dos

            IF numero IS NULL OR numero < 0 THEN
                ERROR "Municipio Erroneo (posicion 3-5) ", op1_dos
                RETURN FALSE
            END IF

            LET act_edo_mun = op1_uno, op1_dos
            LET act_mun     = act_edo_mun

          {
            SELECT "X"
            FROM   tab_delegacion
            WHERE  deleg_cod = act_mun

            IF STATUS = NOTFOUND THEN
              ERROR "Municipio/Delegacion Inexistente (posicion 3-5) ", op1_dos
                RETURN FALSE
            END IF
	    }

            ---- Valida fecha_de nacimiento

            LET numero = op1_tres

            IF numero IS NULL or numero < 0 THEN
                ERROR "An~o de registro erroneo, debe ser un numero (posicion 6-7) ", op1_tres
                RETURN FALSE
            END IF

            ---- Valida numero de libro

            LET numero = op1_cuatro

            IF numero IS NULL OR numero < 0 THEN
                ERROR "Libro erroneo, debe ser un numero (posicion 8-11) ", op1_cuatro
                RETURN FALSE
            END IF

	    {
            IF numero = 9999 THEN
                ERROR "Numero de libro erroneo (posicion 8-11) ", op1_cuatro
                RETURN FALSE
            END IF
	    }

            ---- Valida numero de acta

            LET numero = op1_cinco

            IF numero IS NULL OR numero < 0 THEN
                ERROR "Acta erronea, debe ser un numero (posicion 12-16) ", op1_cinco
                RETURN FALSE
            END IF

	    {
            IF numero = 99999 THEN
                ERROR "Numero de acta erroneo (posicion 12-16) ", op1_cinco
                RETURN FALSE
            END IF
	    }

        WHEN 2 # CRIP #
            LET op2_uno  = x_param[1]
            LET op2_dos  = x_param[2,3]
            LET op2_tres = x_param[4,6]

            LET crip_reg = x_param[2,15]
            LET crip_num = crip_reg
            LET crip_dig = x_param[16]
            LET crip_anyo = x_param[9,10]

            ---- Valida posiones en blanco

            IF op2_uno <> " " THEN
                ERROR "La primera posicion debe ser un blanco "
                RETURN FALSE
            END IF

            ---- Valida entidad federativa

            LET crip_edo = op2_dos

            IF crip_edo < 1 OR
               crip_edo > 32 THEN
                IF crip_edo <> 35 AND
                   crip_edo <> 39 THEN
                    ERROR "Entidad Federativa Inexistente (posicion 2-2) ", 
                          op2_dos
                    RETURN FALSE
                END IF
            END IF

            SELECT "X"
            FROM   tab_estado
            WHERE  estad_cod = crip_edo

            IF STATUS = NOTFOUND THEN
                ERROR "Entidad Federativa Inexistente (posicion 2-3) ", op2_dos
                RETURN FALSE
            END IF

            ---- Valida municipio

            LET numero = op2_tres

            IF numero IS NULL OR numero < 0 THEN
                ERROR "Municipio Erroneo (posicion 4-6) ", op2_tres
                RETURN FALSE
            END IF

            LET act_edo_mun = op2_dos, op2_tres
            LET act_mun     = act_edo_mun

	    {
            SELECT "X"
            FROM   tab_delegacion
            WHERE  deleg_cod = act_mun

            IF STATUS = NOTFOUND THEN
            ERROR "Municipio/Delegacion Inexistente (posicion 4-6) ", op2_tres
                RETURN FALSE
            END IF
	    }

            ---- Valida crip

            IF crip_num IS NULL OR crip_num <= 0 THEN
                ERROR "Crip erronea, (posiciones 2-15) ", op2_uno
                RETURN FALSE
            END IF

            ---- Valida digito ultimo

            LET numero = crip_dig 

            IF crip_anyo > 81 AND
               crip_anyo <= 99 THEN
               ---crip_anyo = 00 THEN
                IF numero IS NULL THEN
                    ERROR "Ultima posicion erronea, debe ser numero ", op2_tres
                    RETURN FALSE
                END IF
            ELSE
              IF crip_dig = "[" OR crip_dig = '"'  OR
                 crip_dig = "]" OR crip_dig = "#"  OR
                 crip_dig = "$" OR crip_dig = "%"  OR
                 crip_dig = "&" OR crip_dig = "="  OR
                 crip_dig = "/" OR crip_dig = "?"  OR
                 crip_dig = "-" OR crip_dig = "'"  OR
                 crip_dig = "(" OR crip_dig = ")"  OR
                 crip_dig = "^" OR crip_dig = "!"  OR
                 crip_dig = "~" OR crip_dig = "_"  OR
                 crip_dig = "." OR crip_dig = ":"  OR
                 crip_dig = "," OR crip_dig = ";"  OR
                 crip_dig = "<" OR crip_dig = ">"  OR
                 crip_dig = "@" OR crip_dig = "|"  OR
                 crip_dig = "{" OR crip_dig = "}"  OR
                 crip_dig = "+" OR crip_dig = "*"  OR
                 crip_dig = "`" OR crip_dig = "¿"  OR
                 crip_dig = "¡" OR crip_dig = "Ä"  OR
                 crip_dig = "É" OR crip_dig = "Í"  OR
                 crip_dig = "Ó" OR crip_dig = "Ú"  OR
                 crip_dig = "¨" OR crip_dig = "Ä"  OR
                 crip_dig = "Ë" OR crip_dig = "Ï"  OR
                 crip_dig = "Ö" OR crip_dig = "Ö"  OR
                 crip_dig = "Ü" OR crip_dig = "á"  OR
                 crip_dig = "é" OR crip_dig = "í"  OR
                 crip_dig = "ó" OR crip_dig = "ú"  OR
                 crip_dig = "ä" OR crip_dig = "ë"  OR
                 crip_dig = "ï" OR crip_dig = "ö"  OR
                 crip_dig = "ü" OR crip_dig = "´"  OR
                 crip_dig = "Á" THEN
                   ERROR "Ultima posicion erronea, tiene caracteres invalidos ", op2_tres
                    RETURN FALSE
                END IF
            END IF

        WHEN 3 # DOCUMENTO MIGRATORIO #
            LET op3_uno = x_param[1,9]
            LET op3_dos = x_param[10,16]

            ---- Valida posiones en blanco

            IF op3_uno <> "         " THEN
        ERROR "Las nueve primeras posiciones del documento deben ser blancos "
                RETURN FALSE
            END IF

            ---- Valida documento

           LET numero = op3_dos

           IF numero IS NULL THEN
               ERROR "Documento migratorio de ser mumerico (posicion 10-16 ) ",op3_dos
               RETURN FALSE
           END IF

	   {
           IF numero < 0 OR numero = 9999999 THEN
               ERROR "Documento migratorio erroneo ",op3_dos
               RETURN FALSE
           END IF
	   }

       WHEN 4 # DOCUMENTO DE NATURALIZACION DE LA SRE #
           LET op4_uno   = x_param[1,7]
           LET op4_dos   = x_param[8,11]
           LET op4_tres  = x_param[12,16]
           LET dcto_1900 = op4_dos

            ---- Valida posiones en blanco

           IF op4_uno <> "       " THEN
               ERROR "Las siete primeras posiciones del documento deben ser blancos ",op4_uno
               RETURN FALSE
           END IF

            ---- Valida anyo del documento

           LET numero = op4_dos

           IF numero IS NULL OR numero <= 0 THEN
               ERROR "An~o de registro del documento erroneo (posicion 8-11 ) ",op4_dos
               RETURN FALSE
           END IF

           IF dcto_anyo < op4_dos THEN
               ERROR "An~o de registro del documento erroneo (posicion 8/11 ) ",op4_dos
               RETURN FALSE
           END IF

         {
           IF dcto_1900 < 1900 THEN
               ERROR "An~o de registro del documento erroneo (posicion 8/11 ) ",op4_dos
               RETURN FALSE
           END IF
         }

            ---- Valida folio del documento

           LET numero = op4_tres

           IF numero IS NULL OR numero < 0 THEN
               ERROR "Folio del documento debe ser numero (posicion 12-16 ) ",op4_tres
               RETURN FALSE
           END IF

	   {
           IF numero = 99999 THEN
               ERROR "Folio del documento erroneo (posicion 12-16 ) ",op4_tres
               RETURN FALSE
           END IF
	   }


       WHEN 5 # DOCUMENTO CURP #
           #SE DEBE VERIFICAR ALGORITMO DE VALIDACION H.F.

       WHEN 7 # CERTIFICADO DE NACIONALIDAD MEXICANA #
           LET op4_uno   = x_param[1,7]
           LET op4_dos   = x_param[8,11]
           LET op4_tres  = x_param[12,16]
           LET dcto_1900 = op4_dos

            ---- Valida posiones en blanco

           IF op4_uno <> "       " THEN
               ERROR "Las siete primeras posiciones del certificado deben ser blancos ",op4_uno
               RETURN FALSE
           END IF

            ---- Valida anyo del certificado

           LET numero = op4_dos

           IF numero IS NULL OR numero <= 0 THEN
               ERROR "An~o de registro del certificado erroneo (posicion 8-11 ) ",op4_dos
               RETURN FALSE
           END IF

           IF dcto_anyo < op4_dos THEN
               ERROR "An~o de registro del certificado erroneo (posicion 8/11 ) ",op4_dos
               RETURN FALSE
           END IF

         {
           IF dcto_1900 < 1900 THEN
               ERROR "An~o de registro del certificado erroneo (posicion 8/11 ) ",op4_dos
               RETURN FALSE
           END IF
         }

            ---- Valida folio del certificado

           LET numero = op4_tres

           IF numero IS NULL OR numero < 0 THEN
               ERROR "Folio del certificado debe ser numero (posicion 12-16 ) ",op4_tres
               RETURN FALSE
           END IF

	   {
           IF numero = 99999 THEN
               ERROR "Folio del certificado erroneo (posicion 12-16 ) ",op4_tres
               RETURN FALSE
           END IF
	   }
    END CASE

    RETURN TRUE

END FUNCTION

FUNCTION Despliega_colonias(xcpos_cod)
   DEFINE 
      enter             CHAR(01), 
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

    ERROR "BUSCANDO INFORMACION ..." 

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
	 OPEN WINDOW ventana_cp AT 6,08 WITH FORM "AFIM00G3" ATTRIBUTE (BORDER)
         DISPLAY " (ENTER) Elegir                                   (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY "                      C  O  L  O  N  I  A  S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)
     
         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

      	       SELECT deleg_cod,ciudad_cod,estad_cod 
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM   tab_codpos  
               WHERE  cpos_cod = l_reg[pos].cod

               SELECT deleg_desc 
               INTO   desdeleg 
               FROM   tab_delegacion 
               WHERE  deleg_cod = reg.deleg

               SELECT ciudad_desc 
               INTO   desciuda
               FROM   tab_ciudad 
               WHERE  ciudad_cod = reg.ciudad

               SELECT estad_desc 
               INTO   desestad
               FROM   tab_estado
               WHERE  estad_cod = reg.estado

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
        #CALL Despliega_estados()

	OPEN WINDOW vent_1 AT 05,07 WITH FORM "AFIM00G4" ATTRIBUTE(BORDER)
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
        PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' for vestad attribute(reverse)
        WHENEVER ERROR STOP

        IF vestad IS NULL THEN
           ERROR "SOLO PUEDE SER CODIGO" SLEEP 1 
        END IF
        IF vestad IS NULL THEN
           LET vestad=0
        END IF

ERROR "BUSCANDO INFORMACION ..."

	WHILE TRUE


           LET x_x = " SELECT c.cpos_cod,a.colon_desc,b.deleg_desc FROM ",
                     " tab_codpos c,tab_colonia a,tab_delegacion b ",
" WHERE c.cpos_cod=a.cpos_cod and c.deleg_cod=b.deleg_cod ",
        " and a.colon_desc MATCHES ",'"',x_buscar CLIPPED,'"',
        " and c.estad_cod=",vestad CLIPPED,
        "     ORDER BY 2 " CLIPPED

	      PREPARE curg21 FROM x_x
	      DECLARE cur_g21 CURSOR FOR curg21
	      LET pos = 1
	      FOREACH cur_g21 INTO l_reg[pos].*
                 IF STATUS=100 THEN
                    EXIT FOREACH
                 END IF
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
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	      LET x_x = " SELECT area_code,area_desc FROM tab_area ",
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
FUNCTION Despliega_nom_afore()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(70)
	END RECORD
	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
	      LET x_x = " SELECT afore_cod, afore_desc FROM tab_afore ",
	                " WHERE desc_afore MATCHES ",'"',x_buscar CLIPPED,'"',
                        " AND marca = 1 ",
	                " ORDER BY 2 " CLIPPED
	      PREPARE cur29 FROM x_x
	      DECLARE cur_29 CURSOR FOR cur29
	      LET pos = 1
	      FOREACH cur_29 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO AFORES... VACIO"
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
# Funcion que verif.que el nombre completo no tenga XXX o mas espacio o digitos.
################################################################################

FUNCTION arma_clave(paterno, materno, nombres, fena, estadon, sexo)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT,
          enter                         CHAR(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE
   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET pa_papa = pa_t1 CLIPPED

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ñ" OR pa_t[j,j] = "ñ" THEN
             LET pa_t[j,j] = "X"
          END IF
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
{
          ELSE
             LET ch_ll[j,j] = pa_t1[i,i]
             IF ch_ll = "CH" OR ch_ll[j,j] = "LL" THEN
                LET j = 2 
             ELSE
                 LET pa_t[j,j] = pa_t1[i,i]
                 IF j = 2 THEN
                    EXIT FOR
                 END IF
             END IF
}
          END IF
       END IF
   END FOR 

   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ñ" OR pa_pa[j,j] = "ñ" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF 
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF 
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF 
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF 
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF 
                END CASE 
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR 
      
         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF
      
         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ñ" OR ma_t[i,i] = "ñ" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR 

         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ñ" OR ma_ma[j,j] = "ñ" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
   END IF
   
## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   FOR i = 1 TO long
     IF i = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR 
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "¥" OR no_no[j,j] = "¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR
   
##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"
##sexo
   CASE sexo
     WHEN 1 LET sexo1 = "H"
     WHEN 2 LET sexo1 = "M"
   END CASE

##ent. federativa
   SELECT a.estad_ren INTO ent_fed1 FROM tab_edo_norma a
          WHERE a.estad_cod = estadon
   IF STATUS = NOTFOUND THEN
      LET ent_fed1 = "  "
   END IF


## consonantes
 LET consonante = pa_pa CLIPPED, ma_ma CLIPPED, no_no CLIPPED

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED, sexo1 CLIPPED, 
                 ent_fed1  CLIPPED, consonante CLIPPED

   RETURN cve_cur
 
END FUNCTION

################################################################################
# Funcion que verif.que el nombre completo del RFC
################################################################################

FUNCTION arma_clave_rfc(paterno, materno, nombres, fena)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT

   DEFINE enter char(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    IF i = 2 THEN
                       LET pater[2,2] = "X"
                       EXIT FOR
                    END IF
                    
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE

   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ñ" OR pa_t[j,j] = "ñ" THEN
             LET pa_t[j,j] = "X"
          END IF
{
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
}
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
          END IF
       END IF
   END FOR 

{
   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ñ" OR pa_pa[j,j] = "ñ" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR
}

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF 
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF 
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF 
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF 
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF 
                END CASE 
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR 
      
         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF
      
         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ñ" OR ma_t[i,i] = "ñ" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR 

{
         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ñ" OR ma_ma[j,j] = "ñ" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
}
   END IF
   
## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF 
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF 
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF 
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF 
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF 
          END CASE 
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR 

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF


   FOR i = 1 TO long
     IF 1 = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR 
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

{
   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "¥" OR no_no[j,j] = "¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR
} 
##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED

   RETURN cve_cur
 
END FUNCTION

FUNCTION verifica_nombre(no_bre)
#ve---------------------------------
  DEFINE no_bre          CHAR(40),
         bl1, bl2, bl3   SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(40)

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0  LET bl3 = 0  
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(no_bre CLIPPED)
  IF no_bre[1,3] MATCHES "XXX" THEN
     LET bl3 = 1
  END IF

  FOR i = 1 TO long
      IF i < 40 THEN
         IF no_bre[i,i+1] = "  " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR 

  INITIALIZE espe TO NULL
  LET i = 0
  FOR i = 1 TO long
      IF no_bre[i,i] = "[" OR no_bre[i,i] = '"'  OR
         no_bre[i,i] = "]" OR no_bre[i,i] = "#"  OR
         no_bre[i,i] = "$" OR no_bre[i,i] = "%"  OR
         no_bre[i,i] = "&" OR no_bre[i,i] = "="  OR
         no_bre[i,i] = "/" OR no_bre[i,i] = "?"  OR
         no_bre[i,i] = "-" OR no_bre[i,i] = "'"  OR
         no_bre[i,i] = "(" OR no_bre[i,i] = ")"  OR
         no_bre[i,i] = "^" OR no_bre[i,i] = "!"  OR
         no_bre[i,i] = "~" OR no_bre[i,i] = "_"  OR
         no_bre[i,i] = "." OR no_bre[i,i] = ":"  OR
         no_bre[i,i] = "," OR no_bre[i,i] = ";"  OR
         no_bre[i,i] = "<" OR no_bre[i,i] = ">"  OR
         no_bre[i,i] = "@" OR no_bre[i,i] = "|"  OR
         no_bre[i,i] = "{" OR no_bre[i,i] = "}"  OR
         no_bre[i,i] = "+" OR no_bre[i,i] = "*"  OR
         no_bre[i,i] = "`" OR no_bre[i,i] = "1"  OR
         no_bre[i,i] = "2" OR no_bre[i,i] = "3"  OR
         no_bre[i,i] = "4" OR no_bre[i,i] = "5"  OR
         no_bre[i,i] = "6" OR no_bre[i,i] = "7"  OR
         no_bre[i,i] = "8" OR no_bre[i,i] = "9"  OR
         no_bre[i,i] = "0" OR no_bre[i,i] = "¿"  OR
         no_bre[i,i] = "¡" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "É" OR no_bre[i,i] = "Í"  OR
         no_bre[i,i] = "Ó" OR no_bre[i,i] = "Ú"  OR
         no_bre[i,i] = "¨" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "Ë" OR no_bre[i,i] = "Ï"  OR
         no_bre[i,i] = "Ö" OR no_bre[i,i] = "Ö"  OR
         no_bre[i,i] = "Ü" OR no_bre[i,i] = "á"  OR
         no_bre[i,i] = "é" OR no_bre[i,i] = "í"  OR
         no_bre[i,i] = "ó" OR no_bre[i,i] = "ú"  OR
         no_bre[i,i] = "ä" OR no_bre[i,i] = "ë"  OR
         no_bre[i,i] = "ï" OR no_bre[i,i] = "ö"  OR
         no_bre[i,i] = "ü" OR no_bre[i,i] = "´"  OR
         no_bre[i,i] = "Á" THEN

         LET espe[i,i] = no_bre[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR

  IF bl1 = 1 THEN
     LET vval = "tiene mas de 1 espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales "
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales "
         LET var = 1
     END IF
  END IF

  IF bl3 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
        LET vval = vval  CLIPPED,", comienza con XXX "
        LET var = 1
     ELSE
        LET vval = vval  CLIPPED," no debe comenzar con XXX "
        LET var = 1
     END IF
  END IF

  IF bl1 = 0  AND bl2 = 0 AND bl3 = 0 THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION
################################################################################
FUNCTION verifica_rfc(r_f_c)
#ve---------------------------------
  DEFINE r_f_c           CHAR(4),
         bl1, bl2        SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(4)

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0 
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(r_f_c CLIPPED)

  FOR i = 1 TO long
      IF i < 4 THEN
         IF r_f_c[i,i+1] = " " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR 

  INITIALIZE espe TO NULL
  LET i = 0
  FOR i = 1 TO long
      IF r_f_c[i,i] = "[" OR r_f_c[i,i] = '"'  OR
         r_f_c[i,i] = "]" OR r_f_c[i,i] = "#"  OR
         r_f_c[i,i] = "$" OR r_f_c[i,i] = "%"  OR
         r_f_c[i,i] = "&" OR r_f_c[i,i] = "="  OR
         r_f_c[i,i] = "/" OR r_f_c[i,i] = "?"  OR
         r_f_c[i,i] = "-" OR r_f_c[i,i] = "'"  OR
         r_f_c[i,i] = "(" OR r_f_c[i,i] = ")"  OR
         r_f_c[i,i] = "^" OR r_f_c[i,i] = "!"  OR
         r_f_c[i,i] = "~" OR r_f_c[i,i] = "_"  OR
         r_f_c[i,i] = "." OR r_f_c[i,i] = ":"  OR
         r_f_c[i,i] = "," OR r_f_c[i,i] = ";"  OR
         r_f_c[i,i] = "<" OR r_f_c[i,i] = ">"  OR
         r_f_c[i,i] = "@" OR r_f_c[i,i] = "|"  OR
         r_f_c[i,i] = "{" OR r_f_c[i,i] = "}"  OR
         r_f_c[i,i] = "+" OR r_f_c[i,i] = "*"  OR
         r_f_c[i,i] = "`" OR r_f_c[i,i] = "1"  OR
         r_f_c[i,i] = "2" OR r_f_c[i,i] = "3"  OR
         r_f_c[i,i] = "4" OR r_f_c[i,i] = "5"  OR
         r_f_c[i,i] = "6" OR r_f_c[i,i] = "7"  OR
         r_f_c[i,i] = "8" OR r_f_c[i,i] = "9"  OR
         r_f_c[i,i] = "0" OR r_f_c[i,i] = "¿"  OR
         r_f_c[i,i] = "¡" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "É" OR r_f_c[i,i] = "Í"  OR
         r_f_c[i,i] = "Ó" OR r_f_c[i,i] = "Ú"  OR
         r_f_c[i,i] = "¨" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "Ë" OR r_f_c[i,i] = "Ï"  OR
         r_f_c[i,i] = "Ö" OR r_f_c[i,i] = "Ö"  OR
         r_f_c[i,i] = "Ü" OR r_f_c[i,i] = "á"  OR
         r_f_c[i,i] = "é" OR r_f_c[i,i] = "í"  OR
         r_f_c[i,i] = "ó" OR r_f_c[i,i] = "ú"  OR
         r_f_c[i,i] = "ä" OR r_f_c[i,i] = "ë"  OR
         r_f_c[i,i] = "ï" OR r_f_c[i,i] = "ö"  OR
         r_f_c[i,i] = "ü" OR r_f_c[i,i] = "´"  OR
         r_f_c[i,i] = "Á" OR r_f_c[i,i] = " "  THEN

         LET espe[i,i] = r_f_c[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR

  IF bl1 = 1 THEN
     LET vval = "tiene espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales  o espacio"
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales  o espacio"
         LET var = 1
     END IF
  END IF
 
  IF bl1 = 0  AND bl2 = 0  THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION

###########################################################################
FUNCTION var_dig_curp(curp)
   DEFINE
     dv_curp		CHAR(1),
     curp		CHAR(18),
     arr		ARRAY[18] OF RECORD
                        curp_pos	CHAR(1)
                        END RECORD,
     i			SMALLINT,
     arr1		ARRAY[36] OF RECORD
                        char		CHAR(1),
			val		SMALLINT
                        END RECORD,
     j			SMALLINT,
     arr2		ARRAY[17] OF RECORD
                        cons		SMALLINT
                        END RECORD,
     k			SMALLINT,
     resultado		INTEGER,
     dism		SMALLINT,
     f			SMALLINT,
     n			SMALLINT,
     a			SMALLINT,
     arr3		ARRAY[17] OF RECORD
                        mult		INTEGER
                        END RECORD, 
     res_mult		INTEGER,
     acu_mult		INTEGER,
     residuo		SMALLINT,
     dig_ver_curp	SMALLINT,
     pasa		CHAR(1)

   LET pasa = 0

   ### SEPARA CURP POR POSICIONES
   LET arr[1].curp_pos  = curp[1]  LET arr[2].curp_pos  = curp[2]
   LET arr[3].curp_pos  = curp[3]  LET arr[4].curp_pos  = curp[4]
   LET arr[5].curp_pos  = curp[5]  LET arr[6].curp_pos  = curp[6]
   LET arr[7].curp_pos  = curp[7]  LET arr[8].curp_pos  = curp[8]
   LET arr[9].curp_pos  = curp[9]  LET arr[10].curp_pos = curp[10]
   LET arr[11].curp_pos = curp[11] LET arr[12].curp_pos = curp[12]
   LET arr[13].curp_pos = curp[13] LET arr[14].curp_pos = curp[14]
   LET arr[15].curp_pos = curp[15] LET arr[16].curp_pos = curp[16]
   LET arr[17].curp_pos = curp[17] LET arr[18].curp_pos = curp[18]

   ### PREPARA CARACTER PARA VALORES
   LET j = 0
   FOR j = 1 TO 36
      LET arr1[j].char = j
      LET arr1[j].val  = j
   END FOR

   LET arr1[10].char = 'A' LET arr1[11].char = 'B' LET arr1[12].char = 'C'
   LET arr1[13].char = 'D' LET arr1[14].char = 'E' LET arr1[15].char = 'F'
   LET arr1[16].char = 'G' LET arr1[17].char = 'H' LET arr1[18].char = 'I'
   LET arr1[19].char = 'J' LET arr1[20].char = 'K' LET arr1[21].char = 'L'
   LET arr1[22].char = 'M' LET arr1[23].char = 'N' LET arr1[24].char = 'Ñ'
   LET arr1[25].char = 'O' LET arr1[26].char = 'P' LET arr1[27].char = 'Q'
   LET arr1[28].char = 'R' LET arr1[29].char = 'S' LET arr1[30].char = 'T'
   LET arr1[31].char = 'U' LET arr1[32].char = 'V' LET arr1[33].char = 'W' 
   LET arr1[34].char = 'X' LET arr1[35].char = 'Y' LET arr1[36].char = 'Z'
    
   ### PREPARA CONSTANTES
   LET k    = 0
   LET dism = 18
   FOR k = 1 TO 17
      LET arr2[k].cons = dism
      LET dism = dism - 1
   END FOR

   ### OBTIENE DIGITO
   LET f = 0
   LET n = 0
   LET a = 0
   LET res_mult     = 0
   LET residuo      = 0
   LET dig_ver_curp = 0
   FOR f = 1 TO 17
      FOR n = 1 TO 36
        IF arr[f].curp_pos  = arr1[n].char THEN
           LET arr3[f].mult = arr1[n].val * arr2[f].cons
           LET res_mult     = arr3[f].mult
           LET acu_mult     = acu_mult + res_mult
        END IF
      END FOR  
   END FOR

   ### OBTIENE RESIDUO Y SE RESTA CON CONSTANTE
   LET residuo = acu_mult MOD 10 
   IF residuo = 0 THEN
      LET dig_ver_curp = 0
   ELSE
      LET dig_ver_curp = 10 - residuo
   END IF

   ### VALIDA RESULTADO DE D.V. VS POS. 18
   IF arr[18].curp_pos = dig_ver_curp THEN
      LET pasa = 1 
   ELSE
      LET pasa = 0
   END IF 

   RETURN pasa, dig_ver_curp

END FUNCTION
###########################################################################
FUNCTION valida_est_curp(curp)
  DEFINE 
     curp 					CHAR(18),
     arr_curp					ARRAY[18] OF RECORD
						curp_pos	CHAR(1)
						END RECORD,
     i						SMALLINT,
     arr_letr					ARRAY[27] OF RECORD
						car		CHAR(1)
						END RECORD,
     j						SMALLINT,
     arr_nume					ARRAY[10] OF RECORD
						num		CHAR(1)	
						END RECORD,
     k						SMALLINT,
     pasa					CHAR(1),
     contador1					SMALLINT,
     contador2					SMALLINT,
     contador3					SMALLINT,
     contador4					SMALLINT,
     contador5					SMALLINT,
     desc_err					CHAR(60),
     desp_err					SMALLINT

   LET pasa = 0

   ### SEPARA CURP POR POSICIONES
   LET arr_curp[01].curp_pos = curp[01]  LET arr_curp[02].curp_pos = curp[02]
   LET arr_curp[03].curp_pos = curp[03]  LET arr_curp[04].curp_pos = curp[04]
   LET arr_curp[05].curp_pos = curp[05]  LET arr_curp[06].curp_pos = curp[06]
   LET arr_curp[07].curp_pos = curp[07]  LET arr_curp[08].curp_pos = curp[08]
   LET arr_curp[09].curp_pos = curp[09]  LET arr_curp[10].curp_pos = curp[10]
   LET arr_curp[11].curp_pos = curp[11]  LET arr_curp[12].curp_pos = curp[12]
   LET arr_curp[13].curp_pos = curp[13]  LET arr_curp[14].curp_pos = curp[14]
   LET arr_curp[15].curp_pos = curp[15]  LET arr_curp[16].curp_pos = curp[16]
   LET arr_curp[17].curp_pos = curp[17]  LET arr_curp[18].curp_pos = curp[18]

   ### INICIALIZA ARREGLO CON VALORES ALFABETICOS
   LET arr_letr[01].car = 'A'  LET arr_letr[02].car = 'B'
   LET arr_letr[03].car = 'C'  LET arr_letr[04].car = 'D'
   LET arr_letr[05].car = 'E'  LET arr_letr[06].car = 'F'
   LET arr_letr[07].car = 'G'  LET arr_letr[08].car = 'H'
   LET arr_letr[09].car = 'I'  LET arr_letr[10].car = 'J'
   LET arr_letr[11].car = 'K'  LET arr_letr[12].car = 'L' 
   LET arr_letr[13].car = 'M'  LET arr_letr[14].car = 'N'  
   LET arr_letr[15].car = 'Ñ'  LET arr_letr[16].car = 'O'  
   LET arr_letr[17].car = 'P'  LET arr_letr[18].car = 'Q'  
   LET arr_letr[19].car = 'R'  LET arr_letr[20].car = 'S'  
   LET arr_letr[21].car = 'T'  LET arr_letr[22].car = 'U' 
   LET arr_letr[23].car = 'V'  LET arr_letr[24].car = 'W'  
   LET arr_letr[25].car = 'X'  LET arr_letr[26].car = 'Y'  
   LET arr_letr[27].car = 'Z'

   ### INICIALIZA ARREGLO CON VALORES NUMERICOS
   LET k = 0
   FOR k = 1 TO 9
     LET arr_nume[k].num = k
   END FOR 
   LET arr_nume[10].num = 0

   ### Valida curp
   LET i         = 0
   LET j         = 0
   LET k         = 0
   LET contador1 = 0
   LET contador2 = 0
   LET contador3 = 0
   LET contador4 = 0
   LET contador5 = 0
   LET desp_err  = 0
 
   FOR i = 1 TO 18

     ### Valida letras (Pos 1 a 4)
     IF i >= 1 AND i <= 4 THEN
        FOR j = 1 TO 27  
           IF arr_curp[i].curp_pos = arr_letr[j].car THEN
              LET contador1 = contador1 + 1
           END IF
        END FOR
     END IF

     ### Valida numeros (Pos 5 a 10)
     IF i >= 5 AND i <= 10 THEN
        FOR k = 1 TO 10 
           IF arr_curp[i].curp_pos = arr_nume[k].num THEN
              LET contador2 = contador2 + 1
           END IF
        END FOR 
     END IF

     ### Valida sexo (Pos 11)
     IF i = 11 THEN
        IF arr_curp[i].curp_pos NOT MATCHES "[HM]" THEN
           LET contador3 = 1
        END IF
     END IF

     ### Valida letras (Pos 12 a 16)
     IF i >= 12 AND i <= 16 THEN
        FOR j = 1 TO 27  
           IF arr_curp[i].curp_pos = arr_letr[j].car THEN
              LET contador4 = contador4 + 1
           END IF
        END FOR
     END IF

     ### Valida numeros (Pos 17 a 18)
     IF i >= 17 AND i <= 18 THEN
        FOR k = 1 TO 10 
           IF arr_curp[i].curp_pos = arr_nume[k].num THEN
              LET contador5 = contador5 + 1
           END IF
        END FOR 
     END IF

   END FOR 

   IF contador1 < 04 THEN
      LET pasa = 1
      LET desc_err = "Error en las primeras 4 posiciones de la CURP"
      LET desp_err = 1 
   END IF

   IF desp_err = 0 THEN
      IF contador2 < 06 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 5 a 10 de la CURP"
         LET desp_err = 1 
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador3 = 01 THEN
         LET pasa = 1
         LET desc_err = "Error en la posicion 11 de la CURP"
         LET desp_err = 1 
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador4 < 05 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 12 a 16 de la CURP"
         LET desp_err = 1 
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador5 < 02 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 17 a 18 de la CURP"
         LET desp_err = 1 
      END IF
   END IF

   RETURN pasa, desc_err

END FUNCTION
#############################################################################
FUNCTION Despliega_ind_info()
   DEFINE aux_val		SMALLINT
   DEFINE lreg			ARRAY[1000] OF RECORD
      	  codigo		INTEGER,
          descripcion		CHAR(50)
          END RECORD
   DEFINE x_x			CHAR(100),
          x_buscar		CHAR(30)
   DEFINE pos			SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
   DISPLAY "          I N D I C A D O R    I N F O N A V I T         " AT 2,1
   ATTRIBUTE(REVERSE)
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
      LET x_x = "SELECT codigo, descripcion FROM tab_ind_cred ",
                " WHERE codigo MATCHES ",'"', x_buscar CLIPPED, '"',
                " ORDER BY 1 " CLIPPED
      PREPARE ind_c1 FROM x_x
      DECLARE ind_c11 CURSOR FOR ind_c1
      LET pos = 1
      FOREACH ind_c11 INTO lreg[pos].*
        LET pos = pos + 1
        IF pos >= 1000 THEN
           ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
           EXIT FOREACH
        END IF
      END FOREACH
      IF (pos-1) < 1 THEN
        ERROR "ARCHIVO INDICADOR INFONAVIT..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY lreg TO scr_1.*
         ON KEY (INTERRUPT)
            LET pos = 0
            EXIT DISPLAY
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY
      IF pos <> 0 THEN
         EXIT WHILE
      END IF
      END WHILE
      CLOSE WINDOW vent_1
      RETURN lreg[pos].codigo, lreg[pos].descripcion

END FUNCTION
#############################################################################
FUNCTION Despliega_rfc_patron()
        DEFINE aux_pausa                SMALLINT
        DEFINE aux_val          SMALLINT
        DEFINE x_x              char(100),
        x_buscar                char(30)
        DEFINE l_reg ARRAY[1000] OF RECORD
               reg_fed_contrib  char(13),
               descripcion      CHAR(50)
        END RECORD
        DEFINE pos              SMALLINT
        OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
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
           LET x_x = " SELECT reg_fed_contrib,razon_social FROM tab_patron ",
                     " WHERE reg_fed_contrib MATCHES ",'"',x_buscar CLIPPED,'"',
                     " ORDER BY 2 " CLIPPED
              PREPARE curr15 FROM x_x
              DECLARE cur_r15 CURSOR FOR curr15
              LET pos = 1
              FOREACH cur_r15 INTO l_reg[pos].*
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
        RETURN l_reg[pos].reg_fed_contrib,l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_profesion()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		SMALLINT,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
	DISPLAY "               P R O F E S I O N E S                     " AT 2,1 ATTRIBUTE(REVERSE)
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
	      LET x_x = " SELECT * FROM tab_profesion ",
	                " WHERE profesion_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 1 " CLIPPED
	      PREPARE curg_prf FROM x_x
	      DECLARE cur_prf CURSOR FOR curg_prf
	      LET pos = 1
	      FOREACH cur_prf INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PROFESIONES ... VACIO"
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

FUNCTION Despliega_actividad()
	DEFINE aux_val		SMALLINT
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		SMALLINT,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE x_x		char(100),
	x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM00G1" ATTRIBUTE(BORDER)
	DISPLAY "               A C T I V I D A D E S                     " AT 2,1 ATTRIBUTE(REVERSE)
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
	      LET x_x = " SELECT * FROM tab_actividad ",
	                " WHERE actividad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 1 " CLIPPED
	      PREPARE curg_act FROM x_x
	      DECLARE cur_act CURSOR FOR curg_act
	      LET pos = 1
	      FOREACH cur_act INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PROFESIONES ... VACIO"
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

