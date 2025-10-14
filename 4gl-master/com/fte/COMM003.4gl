#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Owner             => E.F.P.        					    #
#Programa COMM003  => MANTENIMIENTO DE ESTRUCTURA COMERCIAL.                #
#Sistema           => COM. 					            #
#Fecha             => 01 Mayo 1996.                                         #
#By                => JUAN DAVID HERNANDEZ OYARCE.			    #
#Fecha             => 2 Marzo 1999.                                         #
#By                => GERARDO ALFONSO VEGA PAREDES.      		    #
#Fecha             => 16 enero 2001.                                        #
#By                => GERARDO ALFONSO VEGA PAREDES.      		    #
#############################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa	 	CHAR(1)
	DEFINE HOY			DATE
	DEFINE g_com_parametro		RECORD LIKE com_parametro.*
        DEFINE ciudad smallint,
               desciuda  char(40)
	###############
	#RECORD NIVEL 4
	###############
	DEFINE g_reg ARRAY[200] OF RECORD
               coduni_n4       LIKE com_nivel5.coduni_n5,
               nombre_uni_n4   LIKE com_nivel5.nombre_uni_n5,
               uni_superior_n4 LIKE com_nivel5.uni_superior_n5
	END RECORD
	DEFINE g_reg_1 ARRAY[200] OF RECORD
               coduni_n4       LIKE com_nivel5.coduni_n5,
               nombre_uni_n4   LIKE com_nivel5.nombre_uni_n5,
               uni_superior_n4 LIKE com_nivel5.uni_superior_n5
	END RECORD
	DEFINE x_reg1 ARRAY[200] OF RECORD
               coduni_n4       LIKE com_nivel4.coduni_n4,
               nombre_uni_n4   LIKE com_nivel4.nombre_uni_n4,
               uni_superior_n4 LIKE com_nivel4.uni_superior_n4,
               cont3           INTEGER,
               cont2           INTEGER,
               cont1           INTEGER
	END RECORD
	###############
	#RECORD NIVEL 3
	###############
	DEFINE g_reg2 ARRAY[200] OF RECORD
               coduni_n3       LIKE com_nivel3.coduni_n3,
               nombre_uni_n3   LIKE com_nivel3.nombre_uni_n3,
               uni_superior_n3 LIKE com_nivel3.uni_superior_n3
	END RECORD
	DEFINE g_reg_2 ARRAY[200] OF RECORD
               coduni_n3       LIKE com_nivel3.coduni_n3,
               nombre_uni_n3   LIKE com_nivel3.nombre_uni_n3,
               uni_superior_n3 LIKE com_nivel3.uni_superior_n3
	END RECORD
	DEFINE x_reg2 ARRAY[200] OF RECORD
               coduni_n3       LIKE com_nivel3.coduni_n3,
               nombre_uni_n3   LIKE com_nivel3.nombre_uni_n3,
               uni_superior_n3 LIKE com_nivel3.uni_superior_n3,
               cont2           INTEGER,
               cont1           INTEGER
	END RECORD
	###############
	#RECORD NIVEL 2
	###############
	DEFINE g_reg3 ARRAY[200] OF RECORD
               coduni_n2       LIKE com_nivel2.coduni_n2,
               nombre_uni_n2   LIKE com_nivel2.nombre_uni_n2,
               uni_superior_n2 LIKE com_nivel2.uni_superior_n2
	END RECORD
	DEFINE g_reg_3 ARRAY[200] OF RECORD
               coduni_n2       LIKE com_nivel2.coduni_n2,
               nombre_uni_n2   LIKE com_nivel2.nombre_uni_n2,
               uni_superior_n2 LIKE com_nivel2.uni_superior_n2
	END RECORD
	DEFINE x_reg3 ARRAY[200] OF RECORD
               coduni_n2       LIKE com_nivel2.coduni_n2,
               nombre_uni_n2   LIKE com_nivel2.nombre_uni_n2,
               uni_superior_n2 LIKE com_nivel2.uni_superior_n2,
               cont1           INTEGER
	END RECORD
	###############
	#RECORD NIVEL 2
	###############
	DEFINE g_reg5 ARRAY[200] OF RECORD
               coduni_n1       LIKE com_nivel1.coduni_n1,
               nombre_uni_n1   LIKE com_nivel1.nombre_uni_n1,
               uni_superior_n1 LIKE com_nivel1.uni_superior_n1
	END RECORD
	DEFINE g_reg_5 ARRAY[200] OF RECORD
               coduni_n1       LIKE com_nivel1.coduni_n1,
               nombre_uni_n1   LIKE com_nivel1.nombre_uni_n1,
               uni_superior_n1 LIKE com_nivel1.uni_superior_n1
	END RECORD
	DEFINE x_reg11 ARRAY[1500] OF RECORD
               coduni_n1       LIKE com_nivel1.coduni_n1,
               nombre_uni_n1   LIKE com_nivel1.nombre_uni_n1,
               uni_superior_n1 LIKE com_nivel1.uni_superior_n1,
               cont1           INTEGER
	END RECORD
	###############
	#RECORD NIVEL 1
	###############
	DEFINE x_reg4 ARRAY[5500] OF RECORD
               codven          LIKE pro_mae_promotor.codven,
               g_nombre        CHAR(60),
               fingre          DATE,
               cod_tipo_prom   LIKE pro_mae_promotor.nivel,
               estado          LIKE pro_mae_promotor.motivo_suspende
	END RECORD
	DEFINE G_RETORNO  CHAR(10)
	###################
	#DATOS DE LA UNIDAD
	###################
	DEFINE reg_datos_uni RECORD LIKE com_dat_uni_com.*

	DEFINE reg_desc RECORD 
            g_nombre        CHAR(50),
            g_deleg         CHAR(50),
            g_estad         CHAR(50),
            vgdesc_puesto   CHAR(50),
            vcodven         CHAR(10)
        END RECORD

        DEFINE
           vpaterno  CHAR(20),
           vmaterno  CHAR(20),
           vnombres  CHAR(20)

        DEFINE
            x_descripcion   CHAR(50),
            ACCION          CHAR(1),
            usuario         CHAR(8)
     
        DEFINE 
            vcod_uni     CHAR(10),
            vnom_uni     CHAR(40),
            vuni_sup     CHAR(10),
            tipo_cam     SMALLINT

   DEFINE opc CHAR(01)          
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
	        ACCEPT KEY CONTROL-I

	DEFER INTERRUPT
      
        CALL inicio ()    #i
        CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT *, user 
    INTO   g_com_parametro.*, usuario 
    FROM   com_parametro

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMM0031" ATTRIBUTE(BORDER)
	DISPLAY " COMM003                  ESTRUCTURA COMERCIAL                                 " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	#SELECT nombre_uni_n5 INTO x_descripcion FROM com_nivel5
        #DISPLAY x_descripcion CLIPPED  AT 3,34 ATTRIBUTE(REVERSE)
#ff
	MENU "UNIDAD"
              COMMAND "Agrega" "Agrega Unidad"
	              CALL Inicializa()
	              CALL Agrega()
	              CALL Inicializa()
              COMMAND "Consulta" "Consulta Unidad"
	              CALL Inicializa()
                      CALL Consulta()
	              CALL Inicializa()
              COMMAND "Modifica" "Modifica Unidad"
		      LET ACCION = "M"
	              CALL Inicializa()
                      CALL Modifica()
	              CALL Inicializa()
		      LET ACCION = NULL
              COMMAND "Elimina" "Elimina Unidad"
	              CALL Inicializa()
                      CALL Elimina()
	              CALL Inicializa()
----              COMMAND "Calcula" "Calcula Metas"
----	              CALL Inicializa()
----                      CALL Calcula_metas()
----	              CALL Inicializa()
              COMMAND "Salir" "Salir de Programa"
		      EXIT MENU
	END MENU
	CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Inicializa()
	DEFINE i			SMALLINT
	CLEAR FORM
	FOR i = 1 TO 15
	    INITIALIZE g_reg[i] TO NULL
	    INITIALIZE x_reg1[i] TO NULL
	    DISPLAY g_reg[i].* TO scr_1[i].*
	    DISPLAY x_reg1[i].* TO scr_2[i].*
	END FOR
END FUNCTION
################################################################################
FUNCTION Inicializa2()
	DEFINE i			SMALLINT
	CLEAR FORM
	FOR i = 1 TO 13
	    INITIALIZE g_reg2[i] TO NULL
	    INITIALIZE x_reg2[i] TO NULL
	    DISPLAY g_reg2[i].* TO scr_1[i].*
	    DISPLAY x_reg2[i].* TO scr_2[i].*
	END FOR
END FUNCTION
################################################################################
FUNCTION Inicializa3()
	DEFINE i			SMALLINT
	CLEAR FORM
	FOR i = 1 TO 12
	    INITIALIZE g_reg3[i] TO NULL
	    INITIALIZE x_reg3[i] TO NULL
	    DISPLAY g_reg3[i].* TO scr_1[i].*
	    DISPLAY x_reg3[i].* TO scr_2[i].*
	END FOR
END FUNCTION
################################################################################
FUNCTION Inicializa5()
	DEFINE i			SMALLINT
	CLEAR FORM
	INITIALIZE x_reg11 TO NULL
	FOR i = 1 TO 11
	    INITIALIZE g_reg5[i] TO NULL
	    INITIALIZE x_reg11[i] TO NULL
	    DISPLAY g_reg5[i].* TO scr_1[i].*
	    DISPLAY x_reg11[i].* TO scr_2[i].*
	END FOR
END FUNCTION
################################################################################
FUNCTION Agrega()
	DEFINE arr_c SMALLINT
	DEFINE scr_l SMALLINT
	DEFINE i     SMALLINT
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY "[ Esc ] Graba  [ Ctrl-P ] Proximo Nivel  [ Ctrl-C ] Salir sin Grabar" AT 2,1 ATTRIBUTE(BOLD)

	DISPLAY "                                                                                     " AT 4,1 ATTRIBUTE(REVERSE)

	SELECT nombre_uni_n5 
	INTO   x_descripcion 
	FROM   com_nivel5

        DISPLAY x_descripcion CLIPPED  AT 4,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_4 CLIPPED AT 5,15 ATTRIBUTE(BOLD)

	INPUT ARRAY g_reg FROM scr_1.*
	      BEFORE FIELD coduni_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD coduni_n4
		    IF g_reg[arr_c].coduni_n4 IS NULL THEN
		       ERROR "Codigo de Unidad NO puede ser NULO"
		       NEXT FIELD coduni_n4
		    END IF
		    SELECT "X" FROM com_nivel4
		    WHERE coduni_n4 = g_reg[arr_c].coduni_n4
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Nivel Ya existe"
		       LET g_reg[arr_c].coduni_n4 = NULL
		       DISPLAY g_reg[arr_c].coduni_n4 TO scr_1[scr_l].coduni_n4
		       NEXT FIELD coduni_n4
		    END IF
		    FOR i = 1 TO arr_c - 1
		        IF g_reg[i].coduni_n4 = g_reg[arr_c].coduni_n4 THEN
			   ERROR "Codigo de Unidad ya Ingresado"
		           LET g_reg[arr_c].coduni_n4 = NULL
		        DISPLAY g_reg[arr_c].coduni_n4 TO scr_1[scr_l].coduni_n4
			   NEXT FIELD coduni_n4
		        END IF
		    END FOR
	      BEFORE FIELD nombre_uni_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n4
		    IF g_reg[arr_c].nombre_uni_n4 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n4
		    END IF
	      BEFORE FIELD uni_superior_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n4
		    IF g_reg[arr_c].uni_superior_n4 IS NULL THEN
		       SELECT coduni_n5 INTO 
		       g_reg[arr_c].uni_superior_n4 FROM com_nivel5
		       IF STATUS = NOTFOUND THEN
		          ERROR "Archivo de Nivel Superior Vacio"
		          NEXT FIELD uni_superior_n4
		       END IF
		    ELSE

		       SELECT "X" FROM com_nivel5
		       WHERE coduni_n5 = g_reg[arr_c].uni_superior_n4
		       IF STATUS = NOTFOUND THEN
 		          ERROR "Codigo Unidad Superior Inexistente"
		          NEXT FIELD uni_superior_n4
		       END IF
		    END IF
		    DISPLAY g_reg[arr_c].uni_superior_n4 TO scr_1[scr_l].uni_superior_n4
		    SELECT "X" FROM com_nivel5
		    WHERE coduni_n5 = g_reg[arr_c].uni_superior_n4

        	    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n4
		    END IF

		    INSERT INTO com_nivel4 VALUES (g_reg[arr_c].*,
                                                TODAY,user,1)
		    INITIALIZE reg_datos_uni.* TO NULL
		    CALL Agrega_datos_unidad(4,g_reg[arr_c].coduni_n4)
	   ON KEY ( INTERRUPT )
	      FOR i = 1 TO ARR_CURR()
		  IF g_reg[i].coduni_n4 IS NOT NULL THEN
		     DELETE FROM com_nivel4
		     WHERE coduni_n4 = g_reg[i].coduni_n4
		     DELETE FROM com_dat_uni_com
		     WHERE nivel   = 4
		       AND cod_uni = g_reg[i].coduni_n4
		  END IF
	      END FOR
	      CALL Inicializa()
	      EXIT INPUT
	   ON KEY ( ESC )
	      FOR i = 1 TO ARR_CURR()-1
		  SELECT "X" FROM com_nivel5
		  WHERE coduni_n5 = g_reg[i].uni_superior_n4
		  IF STATUS = NOTFOUND THEN
		     ERROR "Codigo Unidad Superior Inexistente"
		     NEXT FIELD uni_superior_n4
		  END IF
              END FOR
	      ERROR "REGISTRO AGREGADO" SLEEP 2 ERROR ""
              CALL Inicializa()
              EXIT INPUT
	   ON KEY ( CONTROL-P )
	      CALL Ingresa_nivel_numero_3()
	END INPUT
END FUNCTION
################################################################################
FUNCTION Ingresa_nivel_numero_3()
	DEFINE arr_c			SMALLINT
	DEFINE scr_l			SMALLINT
	DEFINE i    			SMALLINT

	OPEN WINDOW ventana_2 AT 6,2 WITH FORM "COMM0032"
	DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_3  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED AT 3,17 ATTRIBUTE(BOLD)

	INPUT ARRAY g_reg2 FROM scr_1.*
	      BEFORE FIELD coduni_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD coduni_n3
		    IF g_reg2[arr_c].coduni_n3 IS NULL THEN
		       ERROR "Codigo de Unidad NO puede ser NULO"
		       NEXT FIELD coduni_n3
		    END IF
		    SELECT "X" FROM com_nivel3
		    WHERE coduni_n3 = g_reg2[arr_c].coduni_n3
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Nivel Ya existe"
		       LET g_reg2[arr_c].coduni_n3 = NULL
		       DISPLAY g_reg2[arr_c].coduni_n3 TO scr_1[scr_l].coduni_n3
		       NEXT FIELD coduni_n3
		    END IF
		    FOR i = 1 TO arr_c - 1
		        IF g_reg2[i].coduni_n3 = g_reg2[arr_c].coduni_n3 THEN
			   ERROR "Codigo de Unidad ya Ingresado"
		           LET g_reg2[arr_c].coduni_n3 = NULL
		        DISPLAY g_reg2[arr_c].coduni_n3 TO scr_1[scr_l].coduni_n3
			   NEXT FIELD coduni_n3
		        END IF
		    END FOR
	      BEFORE FIELD nombre_uni_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n3
		    IF g_reg2[arr_c].nombre_uni_n3 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n3
		    END IF
	      BEFORE FIELD uni_superior_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n3
		    IF g_reg2[arr_c].uni_superior_n3 IS NULL THEN
		       CALL Despliega_codigos(4)
		       LET g_reg2[arr_c].uni_superior_n3 = G_RETORNO
		       SELECT coduni_n4 INTO 
		       g_reg2[arr_c].uni_superior_n3 FROM com_nivel4
		       WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n3
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel4
		       WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n3
		       END IF
		    END IF
		    DISPLAY g_reg2[arr_c].uni_superior_n3 TO scr_1[scr_l].uni_superior_n3
		    SELECT "X" FROM com_nivel4
		    WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n3
		    END IF
		    INSERT INTO com_nivel3 VALUES (g_reg2[arr_c].*,
                                                today,user,1)
		    INITIALIZE reg_datos_uni.* TO NULL
		    CALL Agrega_datos_unidad(3,g_reg2[arr_c].coduni_n3)
	   ON KEY ( INTERRUPT )
	      FOR i = 1 TO ARR_CURR()
		  IF g_reg2[i].coduni_n3 IS NOT NULL THEN
		     DELETE FROM com_nivel3
		     WHERE coduni_n3 = g_reg2[i].coduni_n3
		     DELETE FROM com_dat_uni_com
		     WHERE nivel   = 3
		       AND cod_uni = g_reg2[i].coduni_n3
		  END IF
	      END FOR
	      CALL Inicializa2()
	      EXIT INPUT
	   ON KEY ( ESC )
	      FOR i = 1 TO ARR_CURR()-1
		  SELECT "X" FROM com_nivel4
		  WHERE coduni_n4 = g_reg2[i].uni_superior_n3
		  IF STATUS = NOTFOUND THEN
		     ERROR "Codigo Unidad Superior Inexistente"
		     NEXT FIELD uni_superior_n3
		  END IF
              END FOR
	      ERROR "REGISTRO AGREGADO" SLEEP 2 ERROR ""
	      CALL Inicializa2()
              EXIT INPUT
	   ON KEY ( CONTROL-P )
	      CALL Ingresa_nivel_numero_2()
	END INPUT
	CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION Despliega_codigos(aux_val)
	DEFINE x ARRAY[200] OF RECORD 
	       codigo 		CHAR(10),
	       descripcion	CHAR(40)
	END RECORD
	DEFINE i		SMALLINT
	DEFINE aux_val		SMALLINT
	DEFINE txt		CHAR(100)

	CASE aux_val
	     WHEN 4
		   LET txt = "SELECT coduni_n4,nombre_uni_n4 FROM com_nivel4 ",
			     "ORDER BY 1" CLIPPED
	     WHEN 3
		   LET txt = "SELECT coduni_n3,nombre_uni_n3 FROM com_nivel3 ",
			     "ORDER BY 1" CLIPPED
	     WHEN 2
		   LET txt = "SELECT coduni_n2,nombre_uni_n2 FROM com_nivel2 ",
			     "ORDER BY 1" CLIPPED
	END CASE
	PREPARE cur_1 FROM txt
	LET i = 1
	DECLARE cursor_1 CURSOR FOR cur_1
	FOREACH cursor_1 INTO x[i].*
		LET i = i + 1
	END FOREACh
	CALL SET_COUNT(i-1)
	OPEN WINDOW vent_4 AT 7,15 WITH FORM "COMM0033" ATTRIBUTE(BORDER)
	DISPLAY ARRAY x TO scr_1.*
	  	ON KEY ( CONTROL-M )
		   LET i = ARR_CURR()
		   LET G_RETORNO = x[i].codigo
		   EXIT DISPLAY
	  	ON KEY ( INTERRUPT )
		   ERROR "NO PUEDE SALIR DE LA VENTANA MIENTRAS NO ESCOJA UNO"
		   LET i = ARR_CURR()
	END DISPLAY
	CLOSE WINDOW vent_4
END FUNCTION
################################################################################
FUNCTION Ingresa_nivel_numero_2()
	DEFINE arr_c			SMALLINT
	DEFINE scr_l			SMALLINT
	DEFINE i    			SMALLINT
	OPEN WINDOW ventana_4 AT 7,2 WITH FORM "COMM0034"
	DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_2  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED AT 3,18 ATTRIBUTE(BOLD)

	INPUT ARRAY g_reg3 FROM scr_1.*
	      BEFORE FIELD coduni_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD coduni_n2
		    IF g_reg3[arr_c].coduni_n2 IS NULL THEN
		       ERROR "Codigo de Unidad NO puede ser NULO"
		       NEXT FIELD coduni_n2
		    END IF
		    SELECT "X" FROM com_nivel2
		    WHERE coduni_n2 = g_reg3[arr_c].coduni_n2
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Nivel Ya existe"
		       LET g_reg3[arr_c].coduni_n2 = NULL
		       DISPLAY g_reg3[arr_c].coduni_n2 TO scr_1[scr_l].coduni_n2
		       NEXT FIELD coduni_n2
		    END IF
		    FOR i = 1 TO arr_c - 1
		        IF g_reg3[i].coduni_n2 = g_reg3[arr_c].coduni_n2 THEN
			   ERROR "Codigo de Unidad ya Ingresado"
		           LET g_reg3[arr_c].coduni_n2 = NULL
		        DISPLAY g_reg3[arr_c].coduni_n2 TO scr_1[scr_l].coduni_n2
			   NEXT FIELD coduni_n2
		        END IF
		    END FOR
	      BEFORE FIELD nombre_uni_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n2
		    IF g_reg3[arr_c].nombre_uni_n2 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n2
		    END IF
	      BEFORE FIELD uni_superior_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n2
		    IF g_reg3[arr_c].uni_superior_n2 IS NULL THEN
		       CALL Despliega_codigos(3)
		       LET g_reg3[arr_c].uni_superior_n2 = G_RETORNO
		       SELECT coduni_n3 INTO 
		       g_reg3[arr_c].uni_superior_n2 FROM com_nivel3
		       WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n2
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel3
		       WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n2
		       END IF
		    END IF
		    DISPLAY g_reg3[arr_c].uni_superior_n2 TO scr_1[scr_l].uni_superior_n2
		    SELECT "X" FROM com_nivel3
		    WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n2
		    END IF
		    INSERT INTO com_nivel2 VALUES (g_reg3[arr_c].*,
                                                today,user,1)
		    INITIALIZE reg_datos_uni.* TO NULL
		    CALL Agrega_datos_unidad(2,g_reg3[arr_c].coduni_n2)
	   ON KEY ( INTERRUPT )
	      FOR i = 1 TO ARR_CURR()
		  IF g_reg3[i].coduni_n2 IS NOT NULL THEN
		     DELETE FROM com_nivel2
		     WHERE coduni_n2 = g_reg3[i].coduni_n2
		     DELETE FROM com_dat_uni_com
		     WHERE nivel   = 2
		       AND cod_uni = g_reg3[i].coduni_n2
		  END IF
	      END FOR
	      CALL Inicializa3()
	      EXIT INPUT
	   ON KEY ( ESC )
	      FOR i = 1 TO ARR_CURR()-1
		  SELECT "X" FROM com_nivel3
		  WHERE coduni_n3 = g_reg3[i].uni_superior_n2
		  IF STATUS = NOTFOUND THEN
		     ERROR "Codigo Unidad Superior Inexistente"
		     NEXT FIELD uni_superior_n2
		  END IF
              END FOR
	      ERROR "REGISTRO AGREGADO" SLEEP 2 ERROR ""
	      CALL Inicializa3()
              EXIT INPUT
	   ON KEY ( CONTROL-P )
	      CALL Ingresa_nivel_numero_1()
	END INPUT
	CLOSE WINDOW ventana_4
END FUNCTION
################################################################################
FUNCTION Ingresa_nivel_numero_1()
	DEFINE arr_c			SMALLINT
	DEFINE scr_l			SMALLINT
	DEFINE i    			SMALLINT

	OPEN WINDOW ventana_6 AT 8,2 WITH FORM "COMM0036"

	DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_1  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_1 CLIPPED AT 3,18 ATTRIBUTE(BOLD)

	INPUT ARRAY g_reg5 FROM scr_1.*
	      BEFORE FIELD coduni_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD coduni_n1
		    IF g_reg5[arr_c].coduni_n1 IS NULL THEN
		       ERROR "Codigo de Unidad NO puede ser NULO"
		       NEXT FIELD coduni_n1
		    END IF
		    SELECT "X" FROM com_nivel1
		    WHERE coduni_n1 = g_reg5[arr_c].coduni_n1
		    IF STATUS <> NOTFOUND THEN
		       ERROR "Nivel Ya existe"
		       LET g_reg5[arr_c].coduni_n1 = NULL
		       DISPLAY g_reg5[arr_c].coduni_n1 TO scr_1[scr_l].coduni_n1
		       NEXT FIELD coduni_n1
		    END IF
		    FOR i = 1 TO arr_c - 1
		        IF g_reg5[i].coduni_n1 = g_reg5[arr_c].coduni_n1 THEN
			   ERROR "Codigo de Unidad ya Ingresado"
		           LET g_reg5[arr_c].coduni_n1 = NULL
		       DISPLAY g_reg5[arr_c].coduni_n1 TO scr_1[scr_l].coduni_n2
			   NEXT FIELD coduni_n1
		        END IF
		    END FOR
	      BEFORE FIELD nombre_uni_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n1
		    IF g_reg5[arr_c].nombre_uni_n1 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n1
		    END IF
	      BEFORE FIELD uni_superior_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n1
		    IF g_reg5[arr_c].uni_superior_n1 IS NULL THEN
		       CALL Despliega_codigos(2)
		       LET g_reg5[arr_c].uni_superior_n1 = G_RETORNO
		       SELECT coduni_n2 INTO 
		       g_reg5[arr_c].uni_superior_n1 FROM com_nivel2
		       WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n1
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel2
		       WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n1
		       END IF
		    END IF
		    DISPLAY g_reg5[arr_c].uni_superior_n1 TO scr_1[scr_l].uni_superior_n1
		    SELECT "X" FROM com_nivel2
		    WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n1
		    END IF
		    INITIALIZE reg_datos_uni.* TO NULL
		    INSERT INTO com_nivel1 VALUES (g_reg5[arr_c].*,
                                                today,user,1)
		    CALL Agrega_datos_unidad(1,g_reg5[arr_c].coduni_n1)
	   ON KEY ( INTERRUPT )
	      FOR i = 1 TO ARR_CURR()
		  IF g_reg5[i].coduni_n1 IS NOT NULL THEN
		     DELETE FROM com_nivel1
		     WHERE coduni_n1 = g_reg5[i].coduni_n1
		     DELETE FROM com_dat_uni_com
		     WHERE nivel   = 1
		       AND cod_uni = g_reg5[i].coduni_n1
		  END IF
	      END FOR
	      CALL Inicializa5()
	      EXIT INPUT
	   ON KEY ( ESC )
	      FOR i = 1 TO ARR_CURR()-1
		  SELECT "X" FROM com_nivel2
		  WHERE coduni_n2 = g_reg5[i].uni_superior_n1
		  IF STATUS = NOTFOUND THEN
		     ERROR "Codigo Unidad Superior Inexistente"
		     NEXT FIELD uni_superior_n1
		  END IF
              END FOR
	      ERROR "REGISTRO AGREGADO" SLEEP 2 ERROR ""
	      CALL Inicializa5()
              EXIT INPUT
	   ON KEY ( CONTROL-P )
	      ERROR "NO HAY MAS NIVELES"
	END INPUT
	CLOSE WINDOW ventana_6
END FUNCTION
################################################################################
FUNCTION Consulta()
   DEFINE 
      i		    SMALLINT,
      x_3	    CHAR(10),
      x_2	    CHAR(10),
      x_1	    CHAR(10),
      x_descripcion CHAR(50),
      a,b,c	    CHAR(3)

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
   DISPLAY "[Ctrl-B] Consulta Historico            [Ctrl-C] Salir       " AT 1,1 
   DISPLAY "[Ctrl-P] Datos Unidad                  [ENTER] Proximo Nivel" AT 2,1 ATTRIBUTE(BOLD)
   DISPLAY "                                                                                    " AT 4,1 ATTRIBUTE(REVERSE)

   SELECT nombre_uni_n5 
     INTO x_descripcion 
     FROM com_nivel5

   DISPLAY x_descripcion CLIPPED  AT 4,19 ATTRIBUTE(REVERSE) 
   DISPLAY g_com_parametro.nivel_4 CLIPPED AT 5,14 ATTRIBUTE(BOLD)

   LET a = g_com_parametro.nivel_3
   LET b = g_com_parametro.nivel_2
   LET c = g_com_parametro.nivel_1

   DISPLAY a AT 5,59 ATTRIBUTE(BOLD)
   DISPLAY b AT 5,65 ATTRIBUTE(BOLD)
   DISPLAY c AT 5,71 ATTRIBUTE(BOLD)

   LET i = 1

   ERROR "Buscando Informacion..." ATTRIBUTE(REVERSE,BOLD)

   DECLARE cursor_2 CURSOR FOR 
   SELECT coduni_n4,
          nombre_uni_n4,
          uni_superior_n4,
          0,
          0,
          0 
     FROM com_nivel4
    ORDER BY 1
   FOREACH cursor_2 INTO x_reg1[i].*
      DECLARE cursor_5 CURSOR FOR
      SELECT coduni_n3 
        FROM com_nivel3
       WHERE uni_superior_n3 = x_reg1[i].coduni_n4
      FOREACH cursor_5 INTO x_3
	 LET x_reg1[i].cont3 = x_reg1[i].cont3 + 1
         DECLARE cursor_6 CURSOR FOR
	 SELECT coduni_n2 
           FROM com_nivel2
          WHERE uni_superior_n2 = x_3
	 FOREACH cursor_6 INTO x_2
	    LET x_reg1[i].cont2 = x_reg1[i].cont2 + 1
            DECLARE cursor_7 CURSOR FOR
            SELECT coduni_n1 
              FROM com_nivel1
	     WHERE uni_superior_n1 = x_2
            FOREACH cursor_7 INTO x_1
               LET x_reg1[i].cont1 = x_reg1[i].cont1+1
	    END FOREACH
	 END FOREACH
      END FOREACH
      LET i = i + 1
   END FOREACH

   ERROR ""

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY x_reg1 TO scr_2.*
      ON KEY ( INTERRUPT )
         DISPLAY "" AT 4,1
         CALL Inicializa()
         EXIT DISPLAY
      ON KEY ( CONTROL-P ) 
         LET i = ARR_CURR()
         CALL Consulta_datos_unidad(4,x_reg1[i].coduni_n4)
      ON KEY ( CONTROL-M ) 
         LET i = ARR_CURR()
         CALL NIVEL_3(x_reg1[i].coduni_n4,x_reg1[i].nombre_uni_n4)
      ON KEY ( CONTROL-B ) 
         LET i = ARR_CURR()
         CALL Consulta_historico(x_reg1[i].coduni_n4)
   END DISPLAY
END FUNCTION
################################################################################
FUNCTION NIVEL_3(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE x_descripcion		CHAR(50)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE b,c			CHAR(3)

	OPEN WINDOW ventana_2 AT 6,2 WITH FORM "COMM0032"

	DISPLAY "                                                                             " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_4 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED AT 3,16 ATTRIBUTE(BOLD)
	LET b = g_com_parametro.nivel_2
	LET c = g_com_parametro.nivel_1
	DISPLAY b AT 3,63 ATTRIBUTE(BOLD)
	DISPLAY c AT 3,69 ATTRIBUTE(BOLD)
	LET i = 1
	ERROR "Buscando Informacion..." ATTRIBUTE(REVERSE,BOLD)
	DECLARE cursor_22 CURSOR FOR SELECT
	coduni_n3,nombre_uni_n3,uni_superior_n3,0,0 FROM com_nivel3
	WHERE uni_superior_n3 = VAL
	ORDER BY 1
	FOREACH cursor_22 INTO x_reg2[i].*
		        DECLARE cursor_66 CURSOR FOR
		        SELECT coduni_n2 FROM com_nivel2
		        WHERE uni_superior_n2 = x_reg2[i].coduni_n3
		        LET x_reg2[i].cont2 = 0
		        LET x_reg2[i].cont1 = 0
		        FOREACH cursor_66 INTO x_2
			        LET x_reg2[i].cont2 = x_reg2[i].cont2 + 1
		                DECLARE cursor_77 CURSOR FOR
		                SELECT coduni_n1 FROM com_nivel1
		                WHERE uni_superior_n1 = x_2
		                FOREACH cursor_77 INTO x_1
			                LET x_reg2[i].cont1 = x_reg2[i].cont1+1
		                END FOREACH
		        END FOREACH
		LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg2 TO scr_2.*
		ON KEY ( INTERRUPT )
		   CALL Inicializa2()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(3,x_reg2[i].coduni_n3)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL NIVEL_2(x_reg2[i].coduni_n3,x_reg2[i].nombre_uni_n3)
		ON KEY ( CONTROL-B ) 
		   LET i = ARR_CURR()
  		   CALL Consulta_historico(x_reg2[i].coduni_n3)
	END DISPLAY
	CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION NIVEL_2(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE c			CHAR(3)
	DEFINE x_descripcion		CHAR(50)
	OPEN WINDOW ventana_3 AT 7,2 WITH FORM "COMM0034"

	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_3 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED AT 3,18 ATTRIBUTE(BOLD)
	LET c = g_com_parametro.nivel_1
	DISPLAY c AT 3,68 ATTRIBUTE(BOLD)
	LET i = 1
	ERROR "Buscando Informacion..." ATTRIBUTE(REVERSE,BOLD)
	DECLARE cursor_222 CURSOR FOR SELECT
	coduni_n2,nombre_uni_n2,uni_superior_n2,0 FROM com_nivel2
	WHERE uni_superior_n2 = VAL
	ORDER BY 1
        LET x_reg2[i].cont1 = 0
	FOREACH cursor_222 INTO x_reg3[i].*
		DECLARE cursor_777 CURSOR FOR
		SELECT coduni_n1 FROM com_nivel1
		WHERE uni_superior_n1 = x_reg3[i].coduni_n2
		FOREACH cursor_777 INTO x_1
                        LET x_reg3[i].cont1 = x_reg3[i].cont1 + 1
                END FOREACH
		LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg3 TO scr_2.*
		ON KEY ( INTERRUPT )
		   CALL Inicializa3()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(2,x_reg3[i].coduni_n2)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL NIVEL_1(x_reg3[i].coduni_n2,x_reg3[i].nombre_uni_n2)
		ON KEY ( CONTROL-B ) 
		   LET i = ARR_CURR()
  		   CALL Consulta_historico(x_reg3[i].coduni_n2)
	END DISPLAY
	CLOSE WINDOW ventana_3
END FUNCTION
################################################################################
FUNCTION NIVEL_1(VAL,x_descripcion)
   DEFINE VAL		CHAR(10)
   DEFINE i		SMALLINT
   DEFINE x_1		CHAR(10)
   DEFINE x_descripcion CHAR(50)

   OPEN WINDOW ventana_7 AT 8,2 WITH FORM "COMM0036"

   DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY g_com_parametro.nivel_2 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)

   DISPLAY g_com_parametro.nivel_1 CLIPPED AT 3,18 ATTRIBUTE(BOLD)
   DISPLAY "PROMOTORES" AT 3,64
   ERROR "Buscando Informacion..." ATTRIBUTE(REVERSE,BOLD)

   LET i = 1

   DECLARE cursor_2222 CURSOR FOR 
   SELECT coduni_n1,
          nombre_uni_n1,
          uni_superior_n1,
          0 
   FROM   com_nivel1
   WHERE  uni_superior_n1 = VAL
   ORDER BY 1
   FOREACH cursor_2222 INTO x_reg11[i].*
      DECLARE cursor_100 CURSOR FOR
      SELECT "X" 
      FROM    pro_mae_promotor
      WHERE   agenc_cod = x_reg11[i].coduni_n1
      FOREACH cursor_100 INTO x_1
	 LET x_reg11[i].cont1 = x_reg11[i].cont1 + 1
      END FOREACH
      LET i = i + 1
   END FOREACH

   ERROR ""

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY x_reg11 TO scr_2.*
      ON KEY ( INTERRUPT )
	 CALL Inicializa5()
	 EXIT DISPLAY
      ON KEY ( CONTROL-P ) 
	 LET i = ARR_CURR()
         CALL Consulta_datos_unidad(1,x_reg11[i].coduni_n1)
      ON KEY ( CONTROL-M ) 
	 LET i = ARR_CURR()
         CALL NIVEL_0(x_reg11[i].coduni_n1,x_reg11[i].nombre_uni_n1)    
      ON KEY ( CONTROL-B ) 
	 LET i = ARR_CURR()
  	 CALL Consulta_historico(x_reg11[i].coduni_n1)
   END DISPLAY
   CLOSE WINDOW ventana_7
END FUNCTION
################################################################################
FUNCTION NIVEL_0(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE c			CHAR(10)
	DEFINE p,m,n,s			CHAR(50)
	DEFINE t  			SMALLINT
	DEFINE f  			DATE
	DEFINE x_descripcion		CHAR(50)

	OPEN WINDOW ventana_4 AT 9,2 WITH FORM "COMM0035"

	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)

	DISPLAY g_com_parametro.nivel_1 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)

	LET i = 1
	ERROR "Buscando Informacion..." ATTRIBUTE(REVERSE,BOLD)
	DECLARE cursor_3333 CURSOR FOR
            SELECT cod_promotor,
                   paterno,
                   materno,
                   nombres,
                   fingre,
                   nivel,
                   motivo_suspende
            FROM   pro_mae_promotor
	    WHERE  agenc_cod = VAL
	    ORDER  BY 2

	FOREACH cursor_3333 INTO c,p,m,n,f,t,s
		LET x_reg4[i].codven = c
		LET x_reg4[i].fingre = f
		LET x_reg4[i].cod_tipo_prom = t
		LET x_reg4[i].estado = s
		LET x_reg4[i].g_nombre = p CLIPPED," ",
		                         m CLIPPED," ",
		                         n CLIPPED," "
		LET i = i + 1
	END FOREACH
	ERROR ""
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg4 TO scr_1.*
		ON KEY ( INTERRUPT )
		   EXIT DISPLAY
	END DISPLAY
	CLOSE WINDOW ventana_4
END FUNCTION
################################################################################
FUNCTION Inicializa_datos_de_la_unidad()
	INITIALIZE reg_datos_uni.* TO NULL
        INITIALIZE reg_desc.* TO NULL
        CALL despliega_datos_uni()
END FUNCTION
################################################################################
FUNCTION despliega_datos_uni()
	DISPLAY BY NAME 
               reg_datos_uni.cod_resp_uni,
               reg_desc.g_nombre,
               reg_desc.vgdesc_puesto,
               reg_desc.vcodven, 
               reg_datos_uni.fecha_crea_uni,
               reg_datos_uni.calle_uni,
               reg_datos_uni.num_ext_uni,
               reg_datos_uni.num_int_uni,
               reg_datos_uni.cod_postal_uni,
               reg_datos_uni.colonia_uni,
               reg_datos_uni.deleg_uni,
               reg_desc.g_deleg,
               reg_datos_uni.estad_uni,
               reg_desc.g_estad,
               reg_datos_uni.telefono,
               reg_datos_uni.meta_factor,
               reg_datos_uni.meta_afilia,
               reg_datos_uni.meta_recauda
END FUNCTION
################################################################################
FUNCTION Agrega_datos_unidad(xx_nivel,xx_codigo_unidad)
	DEFINE xx_nivel				SMALLINT
	DEFINE xx_codigo_unidad			CHAR(10)
	
	OPEN WINDOW ventanita_1 AT 7,2 WITH FORM "COMM0021" ATTRIBUTE(BORDER)
	DISPLAY " COMM002           MANTENIMIENTO DATOS DE LA UNIDAD                          " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	CALL Inicializa_datos_de_la_unidad()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	IF ACCION = "M" THEN
	   DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)
	   SELECT cod_resp_uni,
                  #"",#g_nombre,
                  fecha_crea_uni,
                  calle_uni,
                  num_ext_uni,
                  num_int_uni,
                  cod_postal_uni,
                  colonia_uni,
                  deleg_uni,
                  #"",#g_deleg,
                  estad_uni,
                  #"",#g_estad,
                  telefono,
                  meta_factor,
                  meta_afilia,
                  meta_recauda
	   INTO reg_datos_uni.cod_resp_uni,
                #reg_desc.g_nombre,
                reg_datos_uni.fecha_crea_uni,
                reg_datos_uni.calle_uni,
                reg_datos_uni.num_ext_uni,
                reg_datos_uni.num_int_uni,
                reg_datos_uni.cod_postal_uni,
                reg_datos_uni.colonia_uni,
                reg_datos_uni.deleg_uni,
                #reg_desc.g_deleg,
                reg_datos_uni.estad_uni,
                #reg_desc.g_estad,
                reg_datos_uni.telefono,
                reg_datos_uni.meta_factor,
                reg_datos_uni.meta_afilia,
                reg_datos_uni.meta_recauda
	   FROM com_dat_uni_com
	   WHERE nivel   = xx_nivel
	     AND cod_uni = xx_codigo_unidad

	   IF STATUS <> NOTFOUND THEN
{
	      SELECT paterno,
                     materno,
                     nombres,
                     desc_puesto
---------------                     codven
                INTO vpaterno,
                     vmaterno,
                     vnombres,
                     reg_desc.vgdesc_puesto
---------------                     reg_desc.vcodven
	        FROM com_nomina,tab_puesto
	       WHERE nomina_cod = reg_datos_uni.cod_resp_uni 
                 AND com_nomina.cod_puesto = tab_puesto.cod_puesto

              LET reg_desc.g_nombre = vpaterno CLIPPED," ",
                                      vmaterno CLIPPED," ",
jerry                                 vnombres CLIPPED
}
              SELECT nombre_resp_uni,
                     desc_puesto
              INTO   reg_desc.g_nombre,
                     reg_desc.vgdesc_puesto
              FROM   com_respon_unidad,
                     tab_puesto
              WHERE  cod_resp_uni = reg_datos_uni.cod_resp_uni
              AND    puesto_resp  = cod_puesto
 
              DISPLAY reg_desc.vcodven TO vcodven

              SELECT deleg_desc 
                INTO reg_desc.g_deleg
                FROM tab_delegacion 
               WHERE deleg_cod = reg_datos_uni.deleg_uni
   
              SELECT estad_desc 
                INTO reg_desc.g_estad
                FROM tab_estado 
               WHERE estad_cod = reg_datos_uni.estad_uni

              DISPLAY reg_desc.vgdesc_puesto to vgdesc_puesto
	   ELSE
	      ERROR "No se han Ingresado Datos de la Unidad" SLEEP 2
	   END IF
	ELSE
	   DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE)
	END IF
	DISPLAY "[ Esc ] Graba" AT 1,1 ATTRIBUTE(BOLD)
	INPUT BY NAME  
               reg_datos_uni.cod_resp_uni,
               reg_desc.vcodven, 
               reg_datos_uni.calle_uni,
               reg_datos_uni.num_ext_uni,
               reg_datos_uni.num_int_uni,
               reg_datos_uni.cod_postal_uni,
               reg_datos_uni.colonia_uni,
               reg_datos_uni.deleg_uni,
               reg_datos_uni.estad_uni,
               reg_datos_uni.telefono,
               reg_datos_uni.meta_factor,
               reg_datos_uni.meta_afilia,
               reg_datos_uni.meta_recauda WITHOUT DEFAULTS

	      AFTER FIELD cod_resp_uni
		    IF reg_datos_uni.cod_resp_uni IS NOT NULL THEN
{

		       SELECT paterno,
                              materno,
                              nombre,
                              desc_puesto
------------                              codven
                         INTO vpaterno,
                              vmaterno,
                              vnombres,
                              reg_desc.vgdesc_puesto
------------                              reg_desc.vcodven
		         FROM com_nomina,tab_puesto
		        WHERE nomina_cod = reg_datos_uni.cod_resp_uni 
                          AND com_nomina.cod_puesto = tab_puesto.cod_puesto
		       IF STATUS = NOTFOUND THEN	
		          ERROR "Responsable_inexistente"
		          NEXT FIELD cod_resp_uni
		       END IF

                       LET reg_desc.g_nombre = vpaterno CLIPPED," ",
                                               vmaterno CLIPPED," ",
                                               vnombres CLIPPED
}

              SELECT nombre_resp_uni,
                     desc_puesto
              INTO   reg_desc.g_nombre,
                     reg_desc.vgdesc_puesto
              FROM   com_respon_unidad,
                     tab_puesto
              WHERE  cod_resp_uni = reg_datos_uni.cod_resp_uni
              AND    puesto_resp  = cod_puesto

		    ELSE
		       CALL Despliega_Responsables()
		    END IF

                    DISPLAY BY NAME reg_desc.g_nombre
                    DISPLAY reg_desc.vgdesc_puesto to vgdesc_puesto
                    DISPLAY reg_desc.vcodven TO vcodven

	      BEFORE FIELD calle_uni
--		       SELECT paterno,
--                              materno,
--                              nombres 
--                         INTO vpaterno,
--                              vmaterno,
--                              vnombres
--		         FROM com_nomina
--		        WHERE nomina_cod = reg_datos_uni.cod_resp_uni

--                       LET reg_desc.g_nombre = vpaterno CLIPPED," ",
--                                               vmaterno CLIPPED," ",
--                                               vnombres CLIPPED

		       LET reg_datos_uni.fecha_crea_uni = HOY

		       DISPLAY BY NAME reg_desc.g_nombre,
		                       reg_datos_uni.cod_resp_uni,
		                       reg_datos_uni.fecha_crea_uni,
                                       reg_desc.g_nombre
	      AFTER FIELD calle_uni
		    IF reg_datos_uni.calle_uni IS NULL THEN
		       ERROR "Calle NO puede ser NULO"
		       NEXT FIELD calle_uni
		    END IF
	      AFTER FIELD num_ext_uni
		    IF reg_datos_uni.num_ext_uni IS NULL THEN
		       ERROR "Numero Externo NO puede ser NULO"
		       NEXT FIELD num_ext_uni
		    END IF

	      AFTER FIELD cod_postal_uni
                 IF fgl_lastkey() = fgl_keyval("UP") THEN
                    NEXT FIELD num_ext_uni 
                 END IF
                 IF reg_datos_uni.cod_postal_uni IS NULL THEN 
                    CALL Despliega_codigo_postal()
                    RETURNING reg_datos_uni.cod_postal_uni,  
                              reg_datos_uni.colonia_uni, 
                              reg_datos_uni.deleg_uni, 
                              reg_desc.g_deleg, 
                              ciudad,
                              desciuda,
                              reg_datos_uni.estad_uni, 
                              reg_desc.g_estad
                    IF reg_datos_uni.colonia_uni IS NULL THEN
                       ERROR "Este Codigo Postal no existe en el catalogo"
                       NEXT FIELD cod_postal_uni
                    END IF                                       
                 ELSE
                    SELECT "X" FROM tab_codpos
                    WHERE cpos_cod = reg_datos_uni.cod_postal_uni
                    IF status = 100 THEN 
                       ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                       NEXT FIELD cod_postal_uni
                    END IF
                    CALL Despliega_colonias(reg_datos_uni.cod_postal_uni)
                    RETURNING reg_datos_uni.colonia_uni, 
                              reg_datos_uni.deleg_uni, 
                              reg_desc.g_deleg, 
                              ciudad,
                              desciuda,
                              reg_datos_uni.estad_uni, 
                              reg_desc.g_estad
                 END IF

                    --DISPLAY BY NAME reg_datos_uni.*
                    CALL despliega_datos_uni()
                    IF reg_datos_uni.colonia_uni IS NULL THEN	
                       ERROR "Necesita Eligir una Colonia"
                       NEXT FIELD cod_postal_uni 
                    END IF

                    NEXT FIELD telefono

                AFTER FIELD telefono
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD cod_postal_uni
                    END IF

	   ON KEY ( INTERRUPT )
#OJILELI
	      IF
	        reg_datos_uni.cod_resp_uni   IS NULL AND
                reg_desc.g_nombre       IS NULL AND
                reg_datos_uni.fecha_crea_uni IS NULL AND
                reg_datos_uni.calle_uni      IS NULL AND
                reg_datos_uni.num_ext_uni    IS NULL AND
                reg_datos_uni.num_int_uni    IS NULL AND
                reg_datos_uni.cod_postal_uni IS NULL AND
                reg_datos_uni.colonia_uni    IS NULL AND
                reg_datos_uni.deleg_uni      IS NULL AND
                reg_desc.g_deleg        IS NULL AND
                reg_datos_uni.estad_uni      IS NULL AND
                reg_desc.g_estad        IS NULL THEN
	      ERROR "No puede dejar nivel sin datos de LA UNIDAD COMERCIAL A LA QUE CORRESPONDE"
	     ELSE
		 EXIT INPUT
	     END IF
	   ON KEY ( ESC )
		    IF reg_datos_uni.cod_resp_uni IS NULL THEN
		       ERROR "Codigo de Responsable NO puede ser NULO"
		       NEXT FIELD cod_resp_uni
		    END IF
		    IF reg_datos_uni.fecha_crea_uni IS NULL THEN
		       ERROR "Fecha de Creacion NO puede ser NULA"
		       NEXT FIELD fecha_crea_uni
		    END IF
		    IF reg_datos_uni.calle_uni IS NULL THEN
		       ERROR "Calle NO puede ser NULO"
		       NEXT FIELD calle_uni
		    END IF
		    IF reg_datos_uni.num_ext_uni IS NULL THEN
		       ERROR "Numero Externo NO puede ser NULO"
		       NEXT FIELD num_ext_uni
		    END IF
		    IF reg_datos_uni.cod_postal_uni IS NULL THEN
		       ERROR "Codigo Postal NO puede ser NULO"
		       NEXT FIELD cod_postal_uni
		    END IF
		    IF reg_datos_uni.colonia_uni IS NULL THEN
		       ERROR "Colonia NO puede ser NULO"
		       NEXT FIELD colonia_uni
		    END IF
		    IF reg_datos_uni.estad_uni IS NULL THEN
		       ERROR "Estado NO puede ser NULO"
		       NEXT FIELD estad_uni
		    END IF
		    SELECT "X" FROM com_dat_uni_com
		    WHERE nivel   = xx_nivel
		    AND   cod_uni = xx_codigo_unidad

                    LET reg_datos_uni.nivel      = xx_nivel
                    LET reg_datos_uni.cod_uni    = xx_codigo_unidad 
                    LET reg_datos_uni.factualiza = TODAY
                    LET reg_datos_uni.usuario    = usuario

		    IF STATUS = NOTFOUND THEN
		       INSERT INTO com_dat_uni_com VALUES (reg_datos_uni.*)
	                ERROR "REGISTRO AGREGADO"
		    ELSE
		       UPDATE com_dat_uni_com SET
                              nivel          = reg_datos_uni.nivel,
                              cod_uni        = reg_datos_uni.cod_uni,
                              cod_resp_uni   = reg_datos_uni.cod_resp_uni,
                              fecha_crea_uni = reg_datos_uni.fecha_crea_uni,
                              calle_uni      = reg_datos_uni.calle_uni,
                              num_ext_uni    = reg_datos_uni.num_ext_uni,
                              num_int_uni    = reg_datos_uni.num_int_uni,
                              cod_postal_uni = reg_datos_uni.cod_postal_uni,
                              colonia_uni    = reg_datos_uni.colonia_uni,
                              deleg_uni      = reg_datos_uni.deleg_uni,
                              estad_uni      = reg_datos_uni.estad_uni,
                              telefono       = reg_datos_uni.telefono ,
                              meta_factor    = reg_datos_uni.meta_factor,
                              meta_afilia    = reg_datos_uni.meta_afilia,
                              meta_recauda   = reg_datos_uni.meta_recauda,
                              factualiza     = reg_datos_uni.factualiza,
                              usuario        = reg_datos_uni.usuario
		      WHERE nivel   = xx_nivel
		        AND cod_uni = xx_codigo_unidad
	                ERROR "REGISTRO MODIFICADO"
		    END IF
 		    SLEEP 2 ERROR ""
	            CALL Inicializa_datos_de_la_unidad()
		    EXIT INPUT
	END INPUT
	CLOSE WINDOW ventanita_1
END FUNCTION
###############################################################################
FUNCTION Despliega_Responsables_Unidades()
	 DEFINE reg_datos_uni_2 ARRAY[400] OF RECORD
               cod_resp_uni     CHAR(10),
               nombre_resp_uni  CHAR(60)
	END RECORD
	DEFINE i SMALLINT
	DECLARE cursor_1001 CURSOR FOR
	SELECT A.cod_resp_uni,
	       A.nombre_resp_uni 
	FROM responzable_unidad A,com_dat_uni_com B
	WHERE A.cod_resp_uni = B.cod_resp_uni
	ORDER BY 1
	LET i = 1
	FOREACH cursor_1001 INTO reg_datos_uni_2[i].*
		LET i = i + 1
	END FOREACH
	OPEN WINDOW ventanilla_2 AT 12,2 WITH FORM "COMM0012" ATTRIBUTE(BORDER)
	DISPLAY " COMM002           MANTENIMIENTO DATOS DE LA UNIDAD                          " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	#DISPLAY " [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(BOLD)
	IF reg_datos_uni_2[1].cod_resp_uni IS NULL THEN ERROR "NO HAY RESPONSABLES" END IF
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY reg_datos_uni_2 TO scr_1.*
	        ON KEY ( CONTROL-M )
		   LET i = ARR_CURR()
		   EXIT DISPLAY
	        ON KEY ( INTERRUPT )
		   LET i = 1
	END DISPLAY
	LET reg_datos_uni.cod_resp_uni = reg_datos_uni_2[i].cod_resp_uni
	CLOSE WINDOW ventanilla_2
END FUNCTION
###############################################################################
FUNCTION Despliega_Responsables()

   DEFINE c_reg ARRAY[14000] OF RECORD
      cod_resp_uni      LIKE com_respon_unidad.cod_resp_uni,
      nombre_resp_uni CHAR(40),
      desc_puesto     like tab_puesto.desc_puesto,
      codven          CHAR(10)
   END RECORD

   DEFINE i SMALLINT

	OPEN WINDOW co AT 05,02 WITH FORM "COMM0037" ATTRIBUTE(BORDER)
	DISPLAY "                       RESPONSABLE UNIDAD COMERCIAL                         " AT 2,1 ATTRIBUTE(REVERSE)
	DISPLAY "[ Enter ]  Selecciona " AT 1,27

        ERROR "Buscando Informacion..."

	DECLARE cur_2 CURSOR FOR 
              SELECT cod_resp_uni,
                     nombre_resp_uni,
                     desc_puesto,
                     codven
              FROM   com_respon_unidad,
                     tab_puesto
              WHERE  puesto_resp  = cod_puesto

{
        SELECT nomina_cod,
               paterno,
               materno,
               nombres,
               desc_puesto,""
----------               codven
	  FROM com_nomina,tab_puesto 
         WHERE com_nomina.cod_puesto = tab_puesto.cod_puesto
	 ORDER BY 1
}

	LET i = 1
	FOREACH cur_2 INTO c_reg[i].*
           LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY c_reg TO scr_1.*
		ON KEY ( CONTROL-M )
		   LET i = ARR_CURR()
		   LET reg_datos_uni.cod_resp_uni = c_reg[i].cod_resp_uni
                   LET reg_desc.g_nombre          = c_reg[i].nombre_resp_uni 
                   LET reg_desc.vgdesc_puesto     = c_reg[i].desc_puesto
                   LET reg_desc.vcodven           = c_reg[i].codven
		   EXIT DISPLAY
		ON KEY ( INTERRUPT )
		   ERROR "Debe Elegir un Responsable"
	END DISPLAY
	CLOSE WINDOW co
        ERROR ""
END FUNCTION
################################################################################
FUNCTION Consulta_datos_unidad(xx_nivel,xx_codigo_unidad)
	DEFINE xx_nivel				SMALLINT
	DEFINE xx_codigo_unidad			CHAR(10)
	
	OPEN WINDOW ventanita_100 AT 7,2 WITH FORM "COMM0021" ATTRIBUTE(BORDER)
	DISPLAY " COMM002           MANTENIMIENTO DATOS DE LA UNIDAD                          " AT 3,1 ATTRIBUTE(REVERSE)
	CALL Inicializa_datos_de_la_unidad()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
	SELECT cod_resp_uni,
               "",              #g_nombre,
               fecha_crea_uni,
               calle_uni,
               num_ext_uni,
               num_int_uni,
               cod_postal_uni,
               colonia_uni,
               deleg_uni,
               "",              #g_deleg,
               estad_uni,
               "",              #g_estad,
               telefono,
               meta_factor,
               meta_afilia,
               meta_recauda
	INTO reg_datos_uni.cod_resp_uni,
             reg_desc.g_nombre,
             reg_datos_uni.fecha_crea_uni,
             reg_datos_uni.calle_uni,
             reg_datos_uni.num_ext_uni,
             reg_datos_uni.num_int_uni,
             reg_datos_uni.cod_postal_uni,
             reg_datos_uni.colonia_uni,
             reg_datos_uni.deleg_uni,
             reg_desc.g_deleg,
             reg_datos_uni.estad_uni,
             reg_desc.g_estad,
             reg_datos_uni.telefono,
             reg_datos_uni.meta_factor,
             reg_datos_uni.meta_afilia,
             reg_datos_uni.meta_recauda
	FROM com_dat_uni_com
	WHERE nivel   = xx_nivel
	  AND cod_uni = xx_codigo_unidad
	IF STATUS <> NOTFOUND THEN
{
      SELECT paterno,
             materno,
             nombres,
             desc_puesto,""
             ---codven
      INTO   vpaterno,
             vmaterno,
             vnombres,
             reg_desc.vgdesc_puesto,
             reg_desc.vcodven
      FROM   com_nomina,tab_puesto
	   WHERE  nomina_cod  = reg_datos_uni.cod_resp_uni
      AND    com_nomina.cod_puesto = tab_puesto.cod_puesto
}
      LET reg_desc.g_nombre =""
      LET reg_desc.vgdesc_puesto = ""

      SELECT nombre_resp_uni,
             desc_puesto
      INTO   reg_desc.g_nombre,
             reg_desc.vgdesc_puesto 
      FROM   com_respon_unidad,
             tab_puesto
      WHERE  cod_resp_uni = reg_datos_uni.cod_resp_uni
      AND    puesto_resp  = cod_puesto

--display "cod_resp_uni ",reg_datos_uni.cod_resp_uni
--display "reg_desc.g_nombre ",reg_desc.g_nombre
--display "reg_desc.vgdesc_puesto ",reg_desc.vgdesc_puesto
--prompt '' for opc

-----           LET reg_desc.g_nombre = vpaterno CLIPPED," ",
-----                                   vmaterno CLIPPED," ",
-----                                   vnombres CLIPPED

           SELECT deleg_desc INTO reg_desc.g_deleg
           FROM tab_delegacion WHERE
           deleg_cod=reg_datos_uni.deleg_uni

           SELECT estad_desc INTO reg_desc.g_estad
           FROM tab_estado WHERE
           estad_cod =reg_datos_uni.estad_uni

--	   DISPLAY BY NAME reg_datos_uni.*

      CALL despliega_datos_uni()
      DISPLAY reg_desc.vgdesc_puesto to vgdesc_puesto
      DISPLAY reg_desc.vcodven to vcodven
	   PROMPT "Presione una Tecla para Salir" FOR CHAR aux_pausa
	ELSE
      ERROR "No se han Ingresado Datos de la Unidad" SLEEP 3
   END IF
	CLOSE WINDOW ventanita_100
END FUNCTION
##############################################################################
FUNCTION Modifica()
	DEFINE ii			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(10)
	DEFINE x_descripcion		CHAR(50)
	DEFINE a,b,c			CHAR(3)
	DEFINE arr_c,scr_l		SMALLINT
	DEFINE SALE 			CHAR(1)
	DEFINE mx_num 			SMALLINT

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY "[ Esc ] Graba            [ Ctrl-V ] Proximo Nivel" AT 1,1 ATTRIBUTE(BOLD)
        DISPLAY "[ Ctrl-P ] Datos Unidad  [ Ctrl-C ] Salir sin Grabar" AT 2,1 ATTRIBUTE(BOLD)
	DISPLAY g_com_parametro.nivel_4 CLIPPED AT 5,14 ATTRIBUTE(BOLD)
	DISPLAY "                                                                                    " AT 4,1 ATTRIBUTE(REVERSE)
	SELECT nombre_uni_n5 INTO x_descripcion FROM com_nivel5
        DISPLAY x_descripcion CLIPPED  AT 4,19 ATTRIBUTE(REVERSE)
	LET ii = 1
	DECLARE curso_2 CURSOR FOR SELECT
	coduni_n4,nombre_uni_n4,uni_superior_n4 FROM com_nivel4
	ORDER BY 1
	FOREACH curso_2 INTO g_reg[ii].*
            LET g_reg_1[ii].* = g_reg[ii].*
	    LET ii = ii + 1
	END FOREACH
	CALL SET_COUNT(ii-1)
	LET tipo_cam = 0
	LET mx_num = 0
	LET mx_num = ii
	LET SALE = FALSE
	WHILE TRUE
	INPUT ARRAY g_reg WITHOUT DEFAULTS FROM scr_1.*
	      BEFORE FIELD coduni_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
		     IF arr_c > (ii-1) THEN
		        ERROR "LLego al Final del Arreglo"
			EXIT INPUT
		     END IF
		     NEXT FIELD nombre_uni_n4
	      BEFORE FIELD nombre_uni_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n4
		    IF g_reg[arr_c].nombre_uni_n4 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n4
		    END IF
	      BEFORE FIELD uni_superior_n4
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n4
		    IF g_reg[arr_c].uni_superior_n4 IS NULL THEN
		       SELECT coduni_n5 INTO 
		       g_reg[arr_c].uni_superior_n4 FROM com_nivel5
		       IF STATUS = NOTFOUND THEN
		         ERROR "Archivo de Nivel Superior Vacio"
		         NEXT FIELD uni_superior_n4
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel5
		       WHERE coduni_n5 = g_reg[arr_c].uni_superior_n4
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n4
		       END IF
		    END IF
		    DISPLAY g_reg[arr_c].uni_superior_n4 TO scr_1[scr_l].uni_superior_n4

           	    SELECT "X" FROM com_nivel5
		    WHERE coduni_n5 = g_reg[arr_c].uni_superior_n4
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n4
		    END IF 
		ON KEY ( INTERRUPT )
	           DISPLAY "" AT 4,1
		   CALL Inicializa()
	           LET SALE = "S"
		   EXIT INPUT
		ON KEY ( CONTROL-P ) 
		   LET ii = ARR_CURR()
		   #CALL Consulta_datos_unidad(4,g_reg[ii].coduni_n4)
		   CALL Agrega_datos_unidad(4,g_reg[ii].coduni_n4)
                   LET ii = mx_num
	        ON KEY ( ESC )
	           FOR ii = 1 TO ARR_CURR()
		       SELECT "X" FROM com_nivel5
		       WHERE coduni_n5 = g_reg[ii].uni_superior_n4
		       IF STATUS = NOTFOUND THEN
		          ERROR "Codigo Unidad Superior Inexistente"
		          NEXT FIELD uni_superior_n4
		       END IF
                   END FOR
	           FOR ii = 1 TO mx_num
                       IF (g_reg_1[ii].nombre_uni_n4 <>
                           g_reg[ii].nombre_uni_n4) OR
                          (g_reg_1[ii].uni_superior_n4 <>
                           g_reg[ii].uni_superior_n4) THEN
                           LET vcod_uni = g_reg_1[ii].coduni_n4
                           LET vnom_uni = g_reg_1[ii].nombre_uni_n4
                           LET vuni_sup = g_reg_1[ii].uni_superior_n4
                           IF (g_reg_1[ii].nombre_uni_n4 <>
                               g_reg[ii].nombre_uni_n4) THEN
                               LET tipo_cam = 1
                           END IF
                           IF (g_reg_1[ii].uni_superior_n4 <>
                               g_reg[ii].uni_superior_n4) THEN
                               IF tipo_cam = 1 THEN
                                   LET tipo_cam = 3
                               ELSE
                                   LET tipo_cam = 2
                               END IF
                           END IF
                           CALL registra_historico(vcod_uni, vnom_uni, 
                                                   vuni_sup, tipo_cam) #rh
                       END IF
		       UPDATE com_nivel4 SET
			      nombre_uni_n4   = g_reg[ii].nombre_uni_n4,
			      uni_superior_n4 = g_reg[ii].uni_superior_n4,
                              factualiza      = TODAY,
                              usuario         = user
		       WHERE coduni_n4 = g_reg[ii].coduni_n4
                   END FOR
	           ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
                   CALL Inicializa()
		   LET SALE = "S"
                   EXIT INPUT
		ON KEY ( CONTROL-V ) 
		   LET ii = ARR_CURR()
		   CALL MODIFICA_NIVEL_3
                                   (g_reg[ii].coduni_n4,g_reg[ii].nombre_uni_n4)
                   LET ii = mx_num
	END INPUT
	IF SALE = "S" THEN
	   EXIT WHILE
	END IF
	END WHILE
END FUNCTION
################################################################################
FUNCTION MODIFICA_NIVEL_3(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE x_descripcion		CHAR(50)
	DEFINE j			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE b,c			CHAR(3)
	DEFINE arr_c,scr_l		SMALLINT
	DEFINE SALE			CHAR(1)
	DEFINE mx_num 			SMALLINT

	OPEN WINDOW ventana_2 AT 6,2 WITH FORM "COMM0032"

	DISPLAY "                                     r                                       " AT 1,1 ATTRIBUTE(REVERSE)

#A
	DISPLAY g_com_parametro.nivel_4 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED AT 3,17 ATTRIBUTE(BOLD)
	LET j = 1
	DECLARE curso_22 CURSOR FOR SELECT
	coduni_n3,nombre_uni_n3,uni_superior_n3 FROM com_nivel3
	WHERE uni_superior_n3 = VAL
	ORDER BY 1
	FOREACH curso_22 INTO g_reg2[j].*
            LET g_reg_2[j].* = g_reg2[j].*
	    LET j = j + 1
	END FOREACH
        IF j = 1 THEN  
           ERROR "No hay datos"
	   CLOSE WINDOW ventana_2
           RETURN
        END IF
	CALL SET_COUNT(j-1)
	LET mx_num = j
	LET SALE = "N"
	WHILE TRUE
	INPUT ARRAY g_reg2 WITHOUT DEFAULTS FROM  scr_1.*
	      BEFORE FIELD coduni_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
		     IF arr_c > (j-1) THEN
		        ERROR "LLego al Final del Arreglo"
			EXIT INPUT
                        EXIT WHILE
		     END IF
		     NEXT FIELD nombre_uni_n3
	      BEFORE FIELD nombre_uni_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n3
		    IF g_reg2[arr_c].nombre_uni_n3 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n3
		    END IF
	      BEFORE FIELD uni_superior_n3
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n3
		    IF g_reg2[arr_c].uni_superior_n3 IS NULL THEN
		       CALL Despliega_codigos(4)
		       LET g_reg2[arr_c].uni_superior_n3 = G_RETORNO
		       SELECT coduni_n4 INTO 
		       g_reg2[arr_c].uni_superior_n3 FROM com_nivel4
		       WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n3
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel4
		       WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n3
		       END IF
		    END IF
		    DISPLAY g_reg2[arr_c].uni_superior_n3 TO scr_1[scr_l].uni_superior_n3
		    SELECT "X" FROM com_nivel4
		    WHERE coduni_n4 = g_reg2[arr_c].uni_superior_n3
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n3
		    END IF
	        ON KEY ( ESC )
	           FOR j = 1 TO ARR_CURR()-1
		       SELECT "X" FROM com_nivel4
		       WHERE coduni_n4 = g_reg2[j].uni_superior_n3
		       IF STATUS = NOTFOUND THEN
		          ERROR "Codigo Unidad Superior Inexistente"
		          NEXT FIELD uni_superior_n3
		       END IF
                   END FOR
	           FOR j = 1 TO mx_num
                       IF (g_reg_2[j].nombre_uni_n3 <>
                           g_reg2[j].nombre_uni_n3) OR
                          (g_reg_2[j].uni_superior_n3 <>
                           g_reg2[j].uni_superior_n3) THEN
                           LET vcod_uni = g_reg_2[j].coduni_n3
                           LET vnom_uni = g_reg_2[j].nombre_uni_n3
                           LET vuni_sup = g_reg_2[j].uni_superior_n3
                           IF (g_reg_2[j].nombre_uni_n3 <>
                               g_reg2[j].nombre_uni_n3) THEN
                               LET tipo_cam = 1
                           END IF
                           IF (g_reg_2[j].uni_superior_n3 <>
                               g_reg2[j].uni_superior_n3) THEN
                               IF tipo_cam = 1 THEN
                                   LET tipo_cam = 3
                               ELSE
                                   LET tipo_cam = 2
                               END IF
                           END IF
                           CALL registra_historico(vcod_uni, vnom_uni, 
                                                   vuni_sup, tipo_cam) #rh
                       END IF
		       UPDATE com_nivel3 SET
			      nombre_uni_n3   = g_reg2[j].nombre_uni_n3,
			      uni_superior_n3 = g_reg2[j].uni_superior_n3,
                              factualiza      = TODAY,
                              usuario         = user
		       WHERE coduni_n3 = g_reg2[j].coduni_n3
                   END FOR
	           ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
	           CALL Inicializa2()
		   LET SALE = "S"
                   EXIT INPUT
		ON KEY ( INTERRUPT )
		   CALL Inicializa2()
		   LET SALE = "S"
		   EXIT INPUT
		ON KEY ( CONTROL-P ) 
		   LET j = ARR_CURR()
		   #CALL Consulta_datos_unidad(3,g_reg2[j].coduni_n3)
		   CALL Agrega_datos_unidad(3,g_reg2[j].coduni_n3)
                   LET j = mx_num
		ON KEY ( CONTROL-V ) 
		   LET j = ARR_CURR()
		   CALL MODIFICA_NIVEL_2(g_reg2[j].coduni_n3,g_reg2[j].nombre_uni_n3)
LET j = mx_num
	END INPUT
	IF SALE = "S" THEN
	   EXIT WHILE
	END IF
	END WHILE
	CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION MODIFICA_NIVEL_2(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE c			CHAR(3)
	DEFINE x_descripcion		CHAR(50)
	DEFINE arr_c,scr_l		SMALLINT
	DEFINE SALE 			CHAR(1)
	DEFINE mx_num 			SMALLINT

	OPEN WINDOW ventana_3 AT 7,2 WITH FORM "COMM0034"
	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED AT 3,18 ATTRIBUTE(BOLD)
	LET i = 1
	DECLARE curso_222 CURSOR FOR SELECT
	coduni_n2,nombre_uni_n2,uni_superior_n2 FROM com_nivel2
	WHERE uni_superior_n2 = VAL
	ORDER BY 1
        LET x_reg2[i].cont1 = 0
	FOREACH curso_222 INTO g_reg3[i].*
            LET g_reg_3[i].* = g_reg3[i].*
	    LET i = i + 1
	END FOREACH
        IF i = 1 THEN  
           ERROR "No hay datos"
	   CLOSE WINDOW ventana_3
           RETURN
        END IF
	CALL SET_COUNT(i-1)
        LET mx_num = i
        LET SALE = "N"
	WHILE TRUE
	INPUT ARRAY g_reg3 WITHOUT DEFAULTS FROM scr_1.*
	      BEFORE FIELD coduni_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
		     IF arr_c > (i-1) THEN
		        ERROR "LLego al Final del Arreglo"
			EXIT INPUT
		     END IF
		     NEXT FIELD nombre_uni_n2
	      BEFORE FIELD nombre_uni_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n2
		    IF g_reg3[arr_c].nombre_uni_n2 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n2
		    END IF
	      BEFORE FIELD uni_superior_n2
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n2
		    IF g_reg3[arr_c].uni_superior_n2 IS NULL THEN
		       CALL Despliega_codigos(3)
		       LET g_reg3[arr_c].uni_superior_n2 = G_RETORNO
		       SELECT coduni_n3 INTO 
		       g_reg3[arr_c].uni_superior_n2 FROM com_nivel3
		       WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n2
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel3
		       WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n2
		       END IF
		    END IF
		    DISPLAY g_reg3[arr_c].uni_superior_n2 TO scr_1[scr_l].uni_superior_n2
		    SELECT "X" FROM com_nivel3
		    WHERE coduni_n3 = g_reg3[arr_c].uni_superior_n2
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n2
		    END IF
	        ON KEY ( ESC )
	           FOR i = 1 TO ARR_CURR()-1
		       SELECT "X" FROM com_nivel3
		       WHERE coduni_n3 = g_reg3[i].uni_superior_n2
		       IF STATUS = NOTFOUND THEN
		          ERROR "Codigo Unidad Superior Inexistente"
		          NEXT FIELD uni_superior_n2
		       END IF
                   END FOR
	           FOR i = 1 TO mx_num
                       IF (g_reg_3[i].nombre_uni_n2 <>
                           g_reg3[i].nombre_uni_n2) OR
                          (g_reg_3[i].uni_superior_n2 <>
                           g_reg3[i].uni_superior_n2) THEN
                           LET vcod_uni = g_reg_3[i].coduni_n2
                           LET vnom_uni = g_reg_3[i].nombre_uni_n2
                           LET vuni_sup = g_reg_3[i].uni_superior_n2
                           IF (g_reg_3[i].nombre_uni_n2 <>
                               g_reg3[i].nombre_uni_n2) THEN
                               LET tipo_cam = 1
                           END IF
                           IF (g_reg_3[i].uni_superior_n2 <>
                               g_reg3[i].uni_superior_n2) THEN
                               IF tipo_cam = 1 THEN
                                   LET tipo_cam = 3
                               ELSE
                                   LET tipo_cam = 2
                               END IF
                           END IF
                           CALL registra_historico(vcod_uni, vnom_uni, 
                                                   vuni_sup, tipo_cam) #rh
                       END IF
		       UPDATE com_nivel2 SET
			      nombre_uni_n2   = g_reg3[i].nombre_uni_n2,
			      uni_superior_n2 = g_reg3[i].uni_superior_n2,
                              factualiza      = TODAY,
                              usuario         = user
		       WHERE coduni_n2 = g_reg3[i].coduni_n2
                   END FOR
	           ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
	           CALL Inicializa3()
		   LET SALE = "S"
                   EXIT INPUT
		ON KEY ( INTERRUPT )
		   CALL Inicializa3()
		   LET SALE = "S"
		   EXIT INPUT
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   #CALL Consulta_datos_unidad(2,g_reg3[i].coduni_n2)
		      CALL Agrega_datos_unidad(2,g_reg3[i].coduni_n2)
                   LET i = mx_num
		ON KEY ( CONTROL-V ) 
		   LET i = ARR_CURR()
		   CALL MODIFICA_NIVEL_1(g_reg3[i].coduni_n2,g_reg3[i].nombre_uni_n2)
                   LET i = mx_num
	END INPUT
	IF SALE = "S" THEN
	   EXIT WHILE
	END IF
	END WHILE
	CLOSE WINDOW ventana_3
END FUNCTION
################################################################################
FUNCTION MODIFICA_NIVEL_1(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_1			CHAR(1)
	DEFINE x_descripcion		CHAR(50)
	DEFINE arr_c,scr_l		SMALLINT
	DEFINE SALE			CHAR(1)
	DEFINE mx_num			SMALLINT

	OPEN WINDOW ventana_7 AT 8,2 WITH FORM "COMM0036"
	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_1 CLIPPED AT 3,18 ATTRIBUTE(BOLD)
	LET i = 1
	DECLARE curso_2222 CURSOR FOR SELECT
	coduni_n1,nombre_uni_n1,uni_superior_n1 FROM com_nivel1
	WHERE uni_superior_n1 = VAL
	ORDER BY 1
	FOREACH curso_2222 INTO g_reg5[i].*
            LET g_reg_5[i].* = g_reg5[i].*
            LET i = i + 1
	END FOREACH
        IF i = 1 THEN  
           ERROR "No hay datos"
	   CLOSE WINDOW ventana_7
           RETURN
        END IF
	CALL SET_COUNT(i-1)
	LET mx_num = i
	LET SALE = "N"
	WHILE TRUE
	INPUT ARRAY g_reg5 WITHOUT DEFAULTS FROM scr_1.*
	      BEFORE FIELD coduni_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
		     IF arr_c > (i-1) THEN
		        ERROR "LLego al Final del Arreglo"
			EXIT INPUT
		     END IF
		     NEXT FIELD nombre_uni_n1
	      BEFORE FIELD nombre_uni_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD nombre_uni_n1
		    IF g_reg5[arr_c].nombre_uni_n1 IS NULL THEN
		       ERROR "Nombre de Unidad NO puede ser NULO"
		       NEXT FIELD nombre_uni_n1
		    END IF
	      BEFORE FIELD uni_superior_n1
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()
	      AFTER FIELD uni_superior_n1
		    IF g_reg5[arr_c].uni_superior_n1 IS NULL THEN
		       CALL Despliega_codigos(2)
		       LET g_reg5[arr_c].uni_superior_n1 = G_RETORNO
		       SELECT coduni_n2 INTO 
		       g_reg5[arr_c].uni_superior_n1 FROM com_nivel2
		       WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		       IF STATUS = NOTFOUND THEN
		         ERROR "NO existe Nivel en Archivo de Nivel Superior"
		         NEXT FIELD uni_superior_n1
		       END IF
		    ELSE
		       SELECT "X" FROM com_nivel2
		       WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		       IF STATUS = NOTFOUND THEN
		         ERROR "Codigo Unidad Superior Inexistente"
		         NEXT FIELD uni_superior_n1
		       END IF
		    END IF
		    DISPLAY g_reg5[arr_c].uni_superior_n1 TO scr_1[scr_l].uni_superior_n1
		    SELECT "X" FROM com_nivel2
		    WHERE coduni_n2 = g_reg5[arr_c].uni_superior_n1
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo Unidad Superior Inexistente"
		       NEXT FIELD uni_superior_n1
		    END IF
	        ON KEY ( ESC )
	           FOR i = 1 TO ARR_CURR()-1
		       SELECT "X" FROM com_nivel2
		       WHERE coduni_n2 = g_reg5[i].uni_superior_n1
		       IF STATUS = NOTFOUND THEN
		          ERROR "Codigo Unidad Superior Inexistente"
		          NEXT FIELD uni_superior_n1
		       END IF
                   END FOR
	           FOR i = 1 TO mx_num
                       IF (g_reg_5[i].nombre_uni_n1 <>
                           g_reg5[i].nombre_uni_n1) OR
                          (g_reg_5[i].uni_superior_n1 <>
                           g_reg5[i].uni_superior_n1) THEN
                           LET vcod_uni = g_reg_5[i].coduni_n1
                           LET vnom_uni = g_reg_5[i].nombre_uni_n1
                           LET vuni_sup = g_reg_5[i].uni_superior_n1
                           IF (g_reg_5[i].nombre_uni_n1 <>
                               g_reg5[i].nombre_uni_n1) THEN
                               LET tipo_cam = 1
                           END IF
                           IF (g_reg_5[i].uni_superior_n1 <>
                               g_reg5[i].uni_superior_n1) THEN
                               IF tipo_cam = 1 THEN
                                   LET tipo_cam = 3
                               ELSE
                                   LET tipo_cam = 2
                               END IF
                           END IF
                           CALL registra_historico(vcod_uni, vnom_uni, 
                                                   vuni_sup, tipo_cam) #rh
                       END IF
		       UPDATE com_nivel1 SET
			      nombre_uni_n1   = g_reg5[i].nombre_uni_n1,
			      uni_superior_n1 = g_reg5[i].uni_superior_n1,
                              factualiza      = TODAY,
                              usuario         = user
		       WHERE coduni_n1 = g_reg5[i].coduni_n1
                   END FOR
	           LET SALE = "S"
	           ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
	           CALL Inicializa5()
                   EXIT INPUT
		ON KEY ( INTERRUPT )
		   CALL Inicializa5()
	           LET SALE = "S"
		   EXIT INPUT
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
#OJO
		   #CALL Consulta_datos_unidad(1,x_reg11[i].coduni_n1)
		      CALL Agrega_datos_unidad(1,g_reg5[i].coduni_n1)
                   LET i = mx_num
		ON KEY ( CONTROL-V ) 
		   ERROR "NO HAY MAS NIVELES"
	END INPUT
	IF SALE = "S" THEN
	   EXIT WHILE
	END IF
	END WHILE
	CLOSE WINDOW ventana_7
END FUNCTION
################################################################################
FUNCTION Elimina()
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(10)
	DEFINE x_descripcion		CHAR(50)
	DEFINE a,b,c			CHAR(3)
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY "[ Ctrl-B ] Elimina " AT 1,1 
	DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
	DISPLAY "[ Ctrl-P ] Datos Unidad  [ Ctrl-C ] Salir  [ ENTER ] Proximo Nivel" AT 2,1 ATTRIBUTE(BOLD)
	DISPLAY g_com_parametro.nivel_4 CLIPPED AT 5,14 ATTRIBUTE(BOLD)
	DISPLAY "                                                                                    " AT 4,1 ATTRIBUTE(REVERSE)
	SELECT nombre_uni_n5 INTO x_descripcion FROM com_nivel5
        DISPLAY x_descripcion CLIPPED  AT 4,19 ATTRIBUTE(REVERSE)
	LET a = g_com_parametro.nivel_3
	LET b = g_com_parametro.nivel_2
	LET c = g_com_parametro.nivel_1
	DISPLAY a AT 5,59 ATTRIBUTE(BOLD)
	DISPLAY b AT 5,65 ATTRIBUTE(BOLD)
	DISPLAY c AT 5,70 ATTRIBUTE(BOLD)
	LET i = 1
	DECLARE ursor_2 CURSOR FOR SELECT
	coduni_n4,nombre_uni_n4,uni_superior_n4,0,0,0 FROM com_nivel4
	ORDER BY 1
	FOREACH ursor_2 INTO x_reg1[i].*
		DECLARE ursor_5 CURSOR FOR
		SELECT coduni_n3 FROM com_nivel3
		WHERE uni_superior_n3 = x_reg1[i].coduni_n4
		FOREACH ursor_5 INTO x_3
			LET x_reg1[i].cont3 = x_reg1[i].cont3 + 1
		        DECLARE ursor_6 CURSOR FOR
		        SELECT coduni_n2 FROM com_nivel2
		        WHERE uni_superior_n2 = x_3
		        FOREACH ursor_6 INTO x_2
			        LET x_reg1[i].cont2 = x_reg1[i].cont2 + 1
		                DECLARE ursor_7 CURSOR FOR
		                SELECT coduni_n1 FROM com_nivel1
		                WHERE uni_superior_n1 = x_2
		                FOREACH ursor_7 INTO x_1
			                LET x_reg1[i].cont1 = x_reg1[i].cont1+1
		                END FOREACH
		        END FOREACH
		END FOREACH
		LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg1 TO scr_2.*
		ON KEY ( CONTROL-B )
		   LET i = ARR_CURR()
		   IF x_reg1[i].cont3 <> 0 THEN
		      ERROR "No puede Eliminar... Dependen ",x_reg1[i].cont3 USING "#,###"," ",g_com_parametro.nivel_3 CLIPPED," de ",x_reg1[i].nombre_uni_n4 CLIPPED
		   ELSE
		      CALL Consulta_si_borra()
		      IF aux_pausa MATCHES "[Ss]" THEN
		         DELETE FROM com_nivel4
		         WHERE coduni_n4 = x_reg1[i].coduni_n4
		         ERROR "REGISTRO ELIMINADO"
		      ELSE
		         ERROR "REGISTRO NO HA SIDO ELIMINADO"
		      END IF
 		      SLEEP 2 ERROR ""
	              DISPLAY "" AT 4,1
		      CALL Inicializa()
		      EXIT DISPLAY
		   END IF
		ON KEY ( INTERRUPT )
	           DISPLAY "" AT 4,1
		   CALL Inicializa()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(4,x_reg1[i].coduni_n4)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL Elimina_NIVEL_3(x_reg1[i].coduni_n4,x_reg1[i].nombre_uni_n4)
	END DISPLAY
END FUNCTION
################################################################################
FUNCTION Elimina_NIVEL_3(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE x_descripcion		CHAR(50)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE b,c			CHAR(3)
	OPEN WINDOW ventana_2 AT 6,2 WITH FORM "COMM0032"
	DISPLAY "                                                                             " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_4 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED AT 3,17 ATTRIBUTE(BOLD)
	LET b = g_com_parametro.nivel_2
	LET c = g_com_parametro.nivel_1
	DISPLAY b AT 3,63 ATTRIBUTE(BOLD)
	DISPLAY c AT 3,69 ATTRIBUTE(BOLD)
	LET i = 1
	DECLARE ursor_22 CURSOR FOR SELECT
	coduni_n3,nombre_uni_n3,uni_superior_n3,0,0 FROM com_nivel3
	WHERE uni_superior_n3 = VAL
	ORDER BY 1
	FOREACH ursor_22 INTO x_reg2[i].*
		        DECLARE ursor_66 CURSOR FOR
		        SELECT coduni_n2 FROM com_nivel2
		        WHERE uni_superior_n2 = x_reg2[i].coduni_n3
		        LET x_reg2[i].cont2 = 0
		        LET x_reg2[i].cont1 = 0
		        FOREACH ursor_66 INTO x_2
			        LET x_reg2[i].cont2 = x_reg2[i].cont2 + 1
		                DECLARE ursor_77 CURSOR FOR
		                SELECT coduni_n1 FROM com_nivel1
		                WHERE uni_superior_n1 = x_2
		                FOREACH ursor_77 INTO x_1
			                LET x_reg2[i].cont1 = x_reg2[i].cont1+1
		                END FOREACH
		        END FOREACH
		LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg2 TO scr_2.*
		ON KEY ( CONTROL-B )
		   LET i = ARR_CURR()
		   IF x_reg2[i].cont2 <> 0 THEN
		      ERROR "No puede Eliminar... Dependen ",x_reg2[i].cont2 USING "#,###"," ",g_com_parametro.nivel_2 CLIPPED," de ",x_reg2[i].nombre_uni_n3 CLIPPED
		   ELSE
		      CALL Consulta_si_borra()
		      IF aux_pausa MATCHES "[Ss]" THEN
		         DELETE FROM com_nivel3
		         WHERE coduni_n3 = x_reg2[i].coduni_n3
		         ERROR "REGISTRO ELIMINADO"
		      ELSE
		         ERROR "REGISTRO NO HA SIDO ELIMINADO"
		      END IF
 		      SLEEP 2 ERROR ""
	              DISPLAY "" AT 4,1
		      CALL Inicializa2()
		      EXIT DISPLAY
		   END IF
		ON KEY ( INTERRUPT )
		   CALL Inicializa2()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(3,x_reg2[i].coduni_n3)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL Elimina_NIVEL_2(x_reg2[i].coduni_n3,x_reg2[i].nombre_uni_n3)
	END DISPLAY
	CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION Elimina_NIVEL_2(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_3			CHAR(10)
	DEFINE x_2			CHAR(10)
	DEFINE x_1			CHAR(1)
	DEFINE c			CHAR(3)
	DEFINE x_descripcion		CHAR(50)
	OPEN WINDOW ventana_3 AT 7,2 WITH FORM "COMM0034"
	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_3 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED AT 3,17 ATTRIBUTE(BOLD)
	LET c = g_com_parametro.nivel_1
	DISPLAY c AT 3,68 ATTRIBUTE(BOLD)
	LET i = 1
	DECLARE ursor_222 CURSOR FOR SELECT
	coduni_n2,nombre_uni_n2,uni_superior_n2,0 FROM com_nivel2
	WHERE uni_superior_n2 = VAL
	ORDER BY 1
        LET x_reg2[i].cont1 = 0
	FOREACH ursor_222 INTO x_reg3[i].*
		DECLARE ursor_777 CURSOR FOR
		SELECT coduni_n1 FROM com_nivel1
		WHERE uni_superior_n1 = x_reg3[i].coduni_n2
		FOREACH ursor_777 INTO x_1
                        LET x_reg3[i].cont1 = x_reg3[i].cont1 + 1
                END FOREACH
		LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg3 TO scr_2.*
		ON KEY ( CONTROL-B )
		   LET i = ARR_CURR()
		   IF x_reg3[i].cont1 <> 0 THEN
		      ERROR "No puede Eliminar... Dependen ",x_reg3[i].cont1 USING "#,###"," ",g_com_parametro.nivel_1 CLIPPED," de ",x_reg3[i].nombre_uni_n2 CLIPPED
		   ELSE
		      CALL Consulta_si_borra()
		      IF aux_pausa MATCHES "[Ss]" THEN
		         DELETE FROM com_nivel2
		         WHERE coduni_n2 = x_reg3[i].coduni_n2
		         ERROR "REGISTRO ELIMINADO"
		      ELSE
		         ERROR "REGISTRO NO HA SIDO ELIMINADO"
		      END IF
		      SLEEP 2 ERROR ""
	              DISPLAY "" AT 4,1
		      CALL Inicializa3()
		      EXIT DISPLAY
		   END IF
		ON KEY ( INTERRUPT )
		   CALL Inicializa3()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(2,x_reg3[i].coduni_n2)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL Elimina_NIVEL_1(x_reg3[i].coduni_n2,x_reg3[i].nombre_uni_n2)
	END DISPLAY
	CLOSE WINDOW ventana_3
END FUNCTION
################################################################################
FUNCTION Elimina_NIVEL_1(VAL,x_descripcion)
	DEFINE VAL			CHAR(10)
	DEFINE i			SMALLINT
	DEFINE x_1			CHAR(1)
	DEFINE x_descripcion		CHAR(50)
	OPEN WINDOW ventana_7 AT 8,2 WITH FORM "COMM0036"
	DISPLAY "                                                                                  " AT 1,1 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_2 CLIPPED," ",x_descripcion CLIPPED  AT 1,19 ATTRIBUTE(REVERSE)
	DISPLAY g_com_parametro.nivel_1 CLIPPED AT 3,17 ATTRIBUTE(BOLD)
	DISPLAY "PROMOTORES" AT 3,64 sleep 3
	LET i = 1
	DECLARE ursor_2222 CURSOR FOR SELECT
	coduni_n1,nombre_uni_n1,uni_superior_n1,0 FROM com_nivel1
	WHERE uni_superior_n1 = VAL
	ORDER BY 1
	FOREACH ursor_2222 INTO x_reg11[i].*
		DECLARE ursor_100 CURSOR FOR
		SELECT "X" FROM pro_mae_promotor
		WHERE agenc_cod = x_reg11[i].coduni_n1
		FOREACH ursor_100 INTO x_1
			LET x_reg11[i].cont1 = x_reg11[i].cont1 + 1
		END FOREACH
		LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY x_reg11 TO scr_2.*
		ON KEY ( CONTROL-B )
		   LET i = ARR_CURR()
		   IF x_reg11[i].cont1 <> 0 THEN
		      ERROR "No puede Eliminar... Dependen ",x_reg11[i].cont1 USING "#,###"," PROMOTORES "," de ",x_reg11[i].nombre_uni_n1 CLIPPED
		   ELSE
		      CALL Consulta_si_borra()
		      IF aux_pausa MATCHES "[Ss]" THEN
		         DELETE FROM com_nivel1
		         WHERE coduni_n1 = x_reg11[i].coduni_n1
		         ERROR "REGISTRO ELIMINADO"
		      ELSE
		         ERROR "REGISTRO NO HA SIDO ELIMINADO"
		      END IF
                      SLEEP 2 ERROR ""
	              DISPLAY "" AT 4,1
		      CALL Inicializa5()
		      EXIT DISPLAY
		   END IF
		ON KEY ( INTERRUPT )
		   CALL Inicializa5()
		   EXIT DISPLAY
		ON KEY ( CONTROL-P ) 
		   LET i = ARR_CURR()
		   CALL Consulta_datos_unidad(1,x_reg11[i].coduni_n1)
		ON KEY ( CONTROL-M ) 
		   LET i = ARR_CURR()
		   CALL NIVEL_0(x_reg11[i].coduni_n1,x_reg11[i].nombre_uni_n1)
	END DISPLAY
	CLOSE WINDOW ventana_7
END FUNCTION
################################################################################
FUNCTION Consulta_si_borra()
	PROMPT "Esta Seguro de Eliminar Registro S/N " FOR CHAR aux_pausa
END FUNCTION

FUNCTION registra_historico(c_uni, n_uni, u_sup, t_cam)
#rh----------------------------------------------------
   DEFINE c_uni CHAR(10)
   DEFINE n_uni CHAR(40)
   DEFINE u_sup CHAR(10)
   DEFINE t_cam SMALLINT
   DEFINE vhora CHAR(8)

   LET vhora = TIME

   INSERT INTO com_his_estructura VALUES
          (TODAY,
           vhora,
           c_uni,
           n_uni,
           u_sup,
           t_cam, 
           user)
            
END FUNCTION

FUNCTION Consulta_historico(xx_codigo_unidad)
   DEFINE xx_nivel	   SMALLINT
   DEFINE xx_codigo_unidad CHAR(10)
   DEFINE r_his ARRAY[50] OF RECORD 
      fecha_cambio LIKE com_his_estructura.fecha_cambio,
      hora_cambio LIKE com_his_estructura.hora_cambio,
      coduni_n LIKE com_his_estructura.coduni_n,
      nombre_uni_n LIKE com_his_estructura.nombre_uni_n,
      uni_superior_n LIKE com_his_estructura.uni_superior_n,
      tipo_cambio LIKE com_his_estructura.tipo_cambio,
      usuario LIKE com_his_estructura.usuario
   END RECORD
   DEFINE i SMALLINT

   OPEN WINDOW w1 AT 10,2 WITH FORM "COMM00310"
   DISPLAY "               HISTORICO ESTRUCTURA COMERCIAL   CODIGO ",xx_codigo_unidad,"                        " AT 2,1 ATTRIBUTE (REVERSE)

   DECLARE curhist CURSOR FOR
   SELECT *   
     FROM com_his_estructura
    WHERE coduni_n = xx_codigo_unidad

   LET i = 1
   FOREACH curhist INTO r_his[i].*
      LET i = i + 1
   END FOREACH
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY r_his TO scr_1.*
      ON KEY (INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW w1
END FUNCTION
FUNCTION Calcula_metas()
   DEFINE reg4 RECORD
      agencia  CHAR(10),
      factor   DECIMAL(6,2),
      meta_afi DECIMAL(16,2),
      meta_rec DECIMAL(16,2)
   END RECORD
   DEFINE reg3 RECORD
      agencia  CHAR(10),
      factor   DECIMAL(6,2),
      meta_afi DECIMAL(16,2),
      meta_rec DECIMAL(16,2)
   END RECORD
   DEFINE reg2 RECORD
      agencia  CHAR(10),
      factor   DECIMAL(6,2),
      meta_afi DECIMAL(16,2),  
      meta_rec DECIMAL(16,2)
   END RECORD
   DEFINE reg1 RECORD
      agencia  CHAR(10),
      factor   DECIMAL(6,2),
      meta_afi DECIMAL(16,2),
      meta_rec DECIMAL(16,2)
   END RECORD

   ERROR "Actualizando Metas"
 
   ---- ACTUALIZA METAS NIVEL 1  ----
   DECLARE cur_nivel1 CURSOR FOR
   SELECT agenc_cod,
          meta_factor_n1,
          SUM(meta_afilia*meta_factor_n1/100),
          SUM(meta_recauda*meta_factor_n1/100)
     FROM pro_mae_promotor a,
          com_tipo_promotor b,
          com_parametro
    WHERE b.cod_tipo_prom = a.nivel
    GROUP BY 1,2

   FOREACH cur_nivel1 INTO reg1.*
      UPDATE com_dat_uni_com
         SET meta_factor  = reg1.factor,
             meta_afilia  = reg1.meta_afi,
             meta_recauda = reg1.meta_rec
       WHERE nivel = 1
         AND cod_uni = reg1.agencia
   END FOREACH

   ---- ACTUALIZA METAS NIVEL 2  ----
   DECLARE cur_nivel2 CURSOR FOR
   SELECT uni_superior_n1,
          meta_factor_n2,
          sum(meta_afilia*meta_factor_n2/100) mafi,
          sum(meta_recauda*meta_factor_n2/100) mrec
     FROM com_dat_uni_com  a,
          com_nivel1,
          com_parametro 
    WHERE nivel = 1
      AND coduni_n1 = cod_uni
    GROUP BY 1,2

   FOREACH cur_nivel2 INTO reg2.*
      UPDATE com_dat_uni_com
         SET meta_factor = reg2.factor,
             meta_afilia = reg2.meta_afi,
             meta_recauda = reg2.meta_rec
       WHERE nivel = 2
         AND cod_uni = reg2.agencia
   END FOREACH

   ---- ACTUALIZA METAS NIVEL 3  ----
   DECLARE cur_nivel3 CURSOR FOR
   SELECT uni_superior_n2,
          meta_factor_n3,
          sum(meta_afilia*meta_factor_n3/100) mafi,
          sum(meta_recauda*meta_factor_n3/100) mrec
     FROM com_dat_uni_com  a,
          com_nivel2,
          com_parametro
    WHERE nivel = 2
      AND coduni_n2 = cod_uni
    GROUP BY 1,2

   FOREACH cur_nivel3 INTO reg3.*
      UPDATE com_dat_uni_com
         SET meta_factor = reg3.factor,
             meta_afilia = reg3.meta_afi,
             meta_recauda = reg3.meta_rec
       WHERE nivel = 3
         AND cod_uni = reg3.agencia
   END FOREACH

   ---- ACTUALIZA METAS NIVEL 4  ----
   DECLARE cur_nivel4 CURSOR FOR
   SELECT uni_superior_n3,
          meta_factor_n4,
          sum(meta_afilia*meta_factor_n4/100) mafi,
          sum(meta_recauda*meta_factor_n4/100) mrec
     FROM com_dat_uni_com  a,
          com_nivel3,
          com_parametro
    WHERE nivel = 3
      AND coduni_n3 = cod_uni
    GROUP BY 1,2

   FOREACH cur_nivel4 INTO reg4.*
      UPDATE com_dat_uni_com
         SET meta_factor = reg4.factor,
             meta_afilia = reg4.meta_afi,
             meta_recauda = reg4.meta_rec
       WHERE nivel = 4
         AND cod_uni = reg4.agencia
   END FOREACH

   ERROR""
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

	DEFINE x_x		char(400),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT
	OPEN WINDOW vent_1 AT 05,07 WITH FORM "COMM00312" ATTRIBUTE(BORDER)
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
ERROR "BUSCANDO INFORMCAION ..."
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
	 OPEN WINDOW ventana_cp AT 5,07 WITH FORM "COMM00311" ATTRIBUTE (BORDER)
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

END FUNCTION
