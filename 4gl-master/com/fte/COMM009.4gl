################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => Carlos Welsh. 					       #
#Programa COMM009  => MANTENEDOR TABLA DE SALARIOS MINIMOS
#Sistema           => COM. 					               #
#By                => JUAN DAVID HERNANDEZ OYARCE. 			       #
#Fecha             => 05 Mayo 1997.	 				       #
#By                => GERARDO ALFONSO VEGA PAREDES.			       #
#Fecha             => 16 enero 2001.     				       #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
        DEFINE g_detalle ARRAY[100] OF RECORD 
 	       fecha_desde_sm  LIKE tab_salario_minimo.fecha_desde_sm,
 	       fecha_hasta_sm  LIKE tab_salario_minimo.fecha_hasta_sm,
 	       monto_sm        LIKE tab_salario_minimo.monto_sm,
 	       fecha_actualiza LIKE tab_salario_minimo.fecha_actualiza
	END RECORD
	DEFINE HOY			DATE
	DEFINE arr_c,scr_l		SMALLINT
	DEFINE g_com_parametro		RECORD LIKE com_parametro.*
	DEFINE g_usuario		CHAR(8)
	DEFINE DIO_CONTROL		SMALLINT
        DEFINE opc CHAR(01)
        DEFINE cla_sel CHAR(300)
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-i

	DEFER INTERRUPT

	LET HOY = DATE
	SELECT *,USER INTO g_com_parametro.*,g_usuario FROM com_parametro
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0091" ATTRIBUTE( BORDER)
	DISPLAY " COMM009              S A L A R I O S   M I N I M O S                           " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
	
	MENU "SALARIOS"
		COMMAND "Agrega" "Agrega Salario"
			 CALL Inicializa()
		         CALL Agrega()
			 CALL Inicializa()
                COMMAND "Consulta" "Consulta Salario"
			 CALL Inicializa()
		         CALL Consulta()
			 CALL Inicializa()
                COMMAND "Modifica" "Modifica Salario"
			 CALL Inicializa()
		         CALL Modifica()
			 CALL Inicializa()
                COMMAND "Elimina" "Elimina Salario"
			 CALL Inicializa()
		         CALL Elimina()
			 CALL Inicializa()
                COMMAND "Salir" "Salir del Programa"
		        EXIT MENU
	END MENU
              CLOSE WINDOW ventana_1
END MAIN
################################################################################
FUNCTION Inicializa()
	DEFINE i				SMALLINT
	INITIALIZE g_detalle TO NULL
	FOR i = 1 TO 11
	    DISPLAY g_detalle[i].* TO scr_1[i].*
	END FOR
END FUNCTION
################################################################################
FUNCTION Agrega()
	DEFINE i				SMALLINT
	DEFINE l_max_fecha			DATE

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-c ] Salir sin Agregar" AT 2,1 ATTRIBUTE(BOLD)
	 INPUT ARRAY g_detalle FROM scr_1.*
	       BEFORE FIELD fecha_desde_sm
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()
		      LET g_detalle[arr_c].fecha_desde_sm = TODAY
	       AFTER FIELD fecha_desde_sm
		     IF g_detalle[arr_c].fecha_desde_sm IS NULL THEN
			ERROR "Fecha Desde NO puede ser NULO"
		        NEXT FIELD fecha_desde_sm
		     END IF
	       BEFORE FIELD monto_sm
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()
		      SELECT MAX(fecha_desde_sm) INTO l_max_fecha 
		      FROM tab_salario_minimo
		      IF g_detalle[arr_c].fecha_desde_sm <= l_max_fecha THEN
			 ERROR "Fecha NO puede ser menor que ",
					      l_max_fecha USING "dd-mm-yyyy"
		         NEXT FIELD fecha_desde_sm
		      END IF
	       AFTER FIELD monto_sm
		     IF g_detalle[arr_c].monto_sm IS NULL THEN
		        ERROR "Monto Salario Minimo NO puede ser NULO"
		        NEXT FIELD monto_sm
		     END IF
		     LET g_detalle[arr_c].fecha_actualiza = TODAY
		     DISPLAY g_detalle[arr_c].fecha_actualiza TO
		                                  scr_1[scr_l].fecha_actualiza


	     PROMPT "Desea Grabar [S/N]... " FOR CHAR aux_pausa

		  IF aux_pausa MATCHES "[Ss]"THEN
		     UPDATE tab_salario_minimo SET
			    fecha_hasta_sm = (g_detalle[1].fecha_desde_sm-1)
		     WHERE fecha_hasta_sm IS NULL
		     ERROR "REGISTRO INGRESADO" SLEEP 2 ERROR ""
		     INSERT INTO tab_salario_minimo 
		         VALUES (g_detalle[1].fecha_desde_sm,
 	                         g_detalle[1].monto_sm,
 	                         g_detalle[1].fecha_hasta_sm,
 	                         g_detalle[1].fecha_actualiza,
				 g_usuario)
		  ELSE
		     ERROR "INGRESO CANCELADO" SLEEP 2 ERROR ""
		  END IF

		  CALL Inicializa()
		  EXIT INPUT
	      ON KEY ( INTERRUPT )
		 CALL Inicializa()
		 EXIT INPUT
	END INPUT
   ERROR ""
END FUNCTION
################################################################################
FUNCTION Esta_seguro()
	PROMPT "Esta Seguro S/N " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Consulta()
	DEFINE i			SMALLINT
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
        DECLARE cursor_1 CURSOR FOR SELECT fecha_desde_sm,
 				           fecha_hasta_sm,
				           monto_sm,
				           fecha_actualiza
      	FROM tab_salario_minimo ORDER BY 1 DESC
	LET i = 1
      	FOREACH cursor_1 INTO g_detalle[i].*
	      	LET i = i + 1
      	END FOREACH
      	CALL SET_COUNT(i-1)
      	DISPLAY ARRAY g_detalle TO scr_1.*
              	ON KEY ( INTERRUPT )
                   CALL Inicializa()
	           EXIT DISPLAY
      	END DISPLAY
END FUNCTION
###################################################################
FUNCTION Modifica()
	DEFINE i			SMALLINT
	DEFINE l_max_fecha		DATE
	DEFINE l_nro_registros		SMALLINT

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
        DECLARE cursor_2 CURSOR FOR SELECT fecha_desde_sm,
 				           fecha_hasta_sm,
					   monto_sm,
					   fecha_actualiza
      	FROM tab_salario_minimo 
	WHERE fecha_hasta_sm IS NULL
	LET i = 1
      	FOREACH cursor_2 INTO g_detalle[i].*
	      	LET i = i + 1
      	END FOREACH
      	CALL SET_COUNT(i-1)
      	INPUT ARRAY g_detalle WITHOUT DEFAULTS FROM scr_1.*
	       BEFORE FIELD fecha_desde_sm
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()

                      ERROR "Verificando si exiten comisiones con este salario"
                      LET cla_sel = "SELECT * ",
                                   " FROM com_comis_detalle ",
                    "WHERE fentcons >= ","'",g_detalle[arr_c].fecha_desde_sm,"'"
                      PREPARE claexe FROM cla_sel
                      DECLARE cursal CURSOR FOR claexe
                      OPEN cursal
                      FETCH cursal
                      IF STATUS <> NOTFOUND THEN
                         ERROR "No puedes modificar porque exiten comisiones con este salario"
                         SLEEP 2
		         CALL Inicializa()
                         ERROR ""
                         CLOSE cursal
                         EXIT INPUT
                      END IF
                      CLOSE cursal
                      ERROR ""

		      NEXT FIELD monto_sm
	       BEFORE FIELD monto_sm
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()
	       AFTER FIELD monto_sm
		     IF g_detalle[arr_c].monto_sm IS NULL THEN
		        ERROR "Monto Salario Minimo NO puede ser NULO"
		        NEXT FIELD monto_sm
		     END IF
		     LET g_detalle[arr_c].fecha_actualiza = TODAY
		     DISPLAY g_detalle[arr_c].fecha_actualiza TO
		                                  scr_1[scr_l].fecha_actualiza


	     PROMPT "Desea Grabar [S/N]... " FOR CHAR aux_pausa

		  IF aux_pausa MATCHES "[Ss]" THEN
		     UPDATE tab_salario_minimo SET
		            fecha_desde_sm  = g_detalle[1].fecha_desde_sm ,
 	                    monto_sm        = g_detalle[1].monto_sm ,
 	                    fecha_actualiza = g_detalle[1].fecha_actualiza ,
			    usuario         = g_usuario 
	             WHERE fecha_hasta_sm IS NULL
		     ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
		  ELSE
		     ERROR "INGRESO CANCELADO" SLEEP 2 ERROR ""
		  END IF

		  CALL Inicializa()
		  EXIT INPUT
	      ON KEY ( INTERRUPT )
		 CALL Inicializa()
		 EXIT INPUT
	END INPUT
   ERROR ""
END FUNCTION
###################################################################
FUNCTION Elimina()
	DEFINE i			SMALLINT
	DEFINE l_max_fecha		DATE
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-B ] Elimina                       [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
        DECLARE cursor_3 CURSOR FOR SELECT fecha_desde_sm,
 				           fecha_hasta_sm,
					   monto_sm,
				           fecha_actualiza
      	FROM tab_salario_minimo
	WHERE fecha_hasta_sm IS NULL
	LET i = 1
      	FOREACH cursor_3 INTO g_detalle[i].*
	      	LET i = i + 1
      	END FOREACH
      	CALL SET_COUNT(i-1)

        LET cla_sel= "SELECT * ",
                      " FROM com_comis_detalle ",
           " WHERE fentcons >=","'",g_detalle[1].fecha_desde_sm,"'"
        PREPARE claexe2 FROM cla_sel
        DECLARE cursal2 CURSOR FOR claexe2
        OPEN cursal2
        FETCH cursal2
        IF STATUS <> NOTFOUND THEN
           ERROR "No puedes modificar porque exiten comisiones con este salario"
           SLEEP 2
	   CALL Inicializa()
           ERROR ""
	   LET opc = "N"    
        ELSE
           LET opc = "S"
        END IF
        CLOSE cursal2
     IF opc = "S" THEN
      	DISPLAY ARRAY g_detalle TO scr_1.*
              	ON KEY ( CONTROL-B )
		   LET i = ARR_CURR()
		   DELETE FROM tab_salario_minimo
		   WHERE fecha_hasta_sm IS NULL
		      OR fecha_hasta_sm = ""

		   ERROR "REGISTRO ELIMINADO" SLEEP 2 ERROR ""
		   SELECT MAX(fecha_hasta_sm) INTO l_max_fecha 
		   FROM tab_salario_minimo
		        UPDATE tab_salario_minimo SET
		        fecha_hasta_sm = NULL WHERE
			fecha_hasta_sm = l_max_fecha
                   CALL Inicializa()
	           EXIT DISPLAY
              	ON KEY ( INTERRUPT )
                   CALL Inicializa()
	           EXIT DISPLAY
      	END DISPLAY
     END IF
END FUNCTION
