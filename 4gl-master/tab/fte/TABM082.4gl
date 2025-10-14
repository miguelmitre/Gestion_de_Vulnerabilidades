################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => Carlos Welsh. 					       #
#Programa TABM082  => MANTENEDOR TABLA DE SALARIOS MINIMOS                     #
#Sistema           => TAB. 					               #
#By                => MARCOS GODINEZ                		               #
#Fecha             => 16 DICIEMBRE 2003		                               #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa       CHAR(1)
        DEFINE g_detalle ARRAY[100] OF RECORD 
 	       zona_cod        LIKE tabsalario_minimo2.zona_cod,
 	       fecha_desde_sm  LIKE tabsalario_minimo2.fecha_desde_sm,
 	       fecha_hasta_sm  LIKE tabsalario_minimo2.fecha_hasta_sm,
 	       monto_sm        LIKE tabsalario_minimo2.monto_sm,
 	       fecha_actualiza LIKE tabsalario_minimo2.fecha_actualiza
	END RECORD
        DEFINE r_detalle       RECORD 
 	       zona_cod        LIKE tabsalario_minimo2.zona_cod,
 	       fecha_desde_sm  LIKE tabsalario_minimo2.fecha_desde_sm,
 	       fecha_hasta_sm  LIKE tabsalario_minimo2.fecha_hasta_sm,
 	       monto_sm        LIKE tabsalario_minimo2.monto_sm,
 	       fecha_actualiza LIKE tabsalario_minimo2.fecha_actualiza
	END RECORD
	DEFINE HOY	       DATE
	DEFINE arr_c,scr_l     SMALLINT
	DEFINE g_com_parametro RECORD LIKE seg_modulo.*
	DEFINE g_usuario       CHAR(8)
	DEFINE DIO_CONTROL     SMALLINT
        DEFINE opc             CHAR(01)
        DEFINE cla_sel         CHAR(300)
	DEFINE i 	       SMALLINT
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-i

	DEFER INTERRUPT

	CALL STARTLOG("TABM082.log")

	LET HOY = DATE
	SELECT *,USER INTO g_com_parametro.*,g_usuario FROM seg_modulo
        WHERE modulo_cod = 'tab'
	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM0821" ATTRIBUTE( BORDER)
	DISPLAY " TABM082              S A L A R I O S   M I N I M O S                           " AT 3,1 ATTRIBUTE(REVERSE) 
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
	DISPLAY "          " AT 4,21 
	 INPUT ARRAY g_detalle FROM scr_1.*
	       BEFORE FIELD zona_cod
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()
	       AFTER FIELD zona_cod
		     IF g_detalle[arr_c].zona_cod IS NULL THEN
			ERROR "La zona geografica no debe ser NULA"
		        NEXT FIELD zona_cod
		     END IF
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
		      FROM tabsalario_minimo2
		      WHERE zona_cod = g_detalle[arr_c].zona_cod 
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
		     UPDATE tabsalario_minimo2 SET
			    fecha_hasta_sm = (g_detalle[arr_c].fecha_desde_sm-1)
                     WHERE  fecha_hasta_sm IS NULL
		     AND    zona_cod = g_detalle[arr_c].zona_cod 
		     ERROR "REGISTRO INGRESADO" SLEEP 2 ERROR ""
		     INSERT INTO tabsalario_minimo2 
		         VALUES (g_detalle[arr_c].zona_cod,
		                 g_detalle[arr_c].fecha_desde_sm,
 	                         g_detalle[arr_c].monto_sm,
 	                         g_detalle[arr_c].fecha_hasta_sm,
 	                         g_detalle[arr_c].fecha_actualiza,
				 g_usuario)
		  ELSE
		     ERROR "INGRESO CANCELADO" SLEEP 2 ERROR ""
		  END IF

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
	DISPLAY "HASTA" AT 4,22 
        DECLARE cursor_1 CURSOR FOR SELECT zona_cod,
                                           fecha_desde_sm,
 				           fecha_hasta_sm,
				           monto_sm,
				           fecha_actualiza
      	FROM tabsalario_minimo2 ORDER BY 2 DESC,1 ASC
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
	DEFINE l_max_fecha		DATE
	DEFINE l_nro_registros		SMALLINT
        DEFINE zona_cod_or              CHAR(1)
        DEFINE fecha_desde_sm_or        DATE
 	DEFINE monto_sm_or              DECIMAL(12,2)

	LET i = 0

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-c ] Salir" AT 2,1 ATTRIBUTE(BOLD)
	DISPLAY "HASTA" AT 4,22 
        DECLARE cursor_2 CURSOR FOR SELECT zona_cod,
                                           fecha_desde_sm,
 				           fecha_hasta_sm,
					   monto_sm,
					   fecha_actualiza
      	FROM tabsalario_minimo2 
	WHERE fecha_hasta_sm IS NULL
        ORDER BY 2 DESC,1 ASC
      	FOREACH cursor_2 INTO r_detalle.* 
	      	LET i = i + 1
		LET g_detalle[i].zona_cod        = r_detalle.zona_cod
		LET g_detalle[i].fecha_desde_sm  = r_detalle.fecha_desde_sm
		LET g_detalle[i].fecha_hasta_sm  = r_detalle.fecha_hasta_sm
		LET g_detalle[i].monto_sm        = r_detalle.monto_sm
		LET g_detalle[i].fecha_actualiza = r_detalle.fecha_actualiza
      	END FOREACH
	FREE cursor_2

	IF (i) = 0 THEN
	   ERROR "No existe registros a modificar."
	   SLEEP 2
	   ERROR ""
	   RETURN
	END IF

      	CALL SET_COUNT(i)

      	INPUT ARRAY g_detalle WITHOUT DEFAULTS FROM scr_1.*
	ATTRIBUTE(MAXCOUNT = i, COUNT = i)

	       BEFORE FIELD zona_cod
		      NEXT FIELD fecha_desde_sm

	       BEFORE FIELD fecha_desde_sm
		      LET arr_c = ARR_CURR()
		      LET scr_l = SCR_LINE()
                      LET zona_cod_or       = g_detalle[arr_c].zona_cod
                      LET fecha_desde_sm_or = g_detalle[arr_c].fecha_desde_sm
 	              LET monto_sm_or       = g_detalle[arr_c].monto_sm
                      ERROR "Verificando si exiten comisiones con este salario"
                      LET cla_sel = "SELECT * ",
                                   " FROM com_comis_detalle ",
                    "WHERE fentcons >= ","'",g_detalle[arr_c].fecha_desde_sm,"'"
                      PREPARE claexe FROM cla_sel
                      DECLARE cursal CURSOR FOR claexe
                      OPEN cursal
                      FETCH cursal
                      IF STATUS <> NOTFOUND THEN
                         ERROR "No puedes modificar porque exiten ",
			       "comisiones con este salario"
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

                      LET zona_cod_or       = g_detalle[arr_c].zona_cod
                      LET fecha_desde_sm_or = g_detalle[arr_c].fecha_desde_sm
 	              LET monto_sm_or       = g_detalle[arr_c].monto_sm

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

		             UPDATE tabsalario_minimo2 SET
                                    zona_cod        = 
				    g_detalle[arr_c].zona_cod,
                                    fecha_desde_sm  = 
				    g_detalle[arr_c].fecha_desde_sm ,
 	                            monto_sm        = 
				    g_detalle[arr_c].monto_sm ,
 	                            fecha_actualiza = 
				    g_detalle[arr_c].fecha_actualiza ,
			            usuario         = g_usuario 
	                     WHERE fecha_hasta_sm IS NULL
                             AND   zona_cod         = zona_cod_or 
                             AND   fecha_desde_sm   = fecha_desde_sm_or 
 	                     AND   monto_sm         = monto_sm_or

		             ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
		          ELSE
		             ERROR "INGRESO CANCELADO" SLEEP 2 ERROR ""
		          END IF

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
        DEFINE zona_cod_or              CHAR(1)
        DEFINE fecha_desde_sm_or        DATE
 	DEFINE monto_sm_or              DECIMAL(12,2)

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
	DISPLAY " [ Ctrl-B ] Elimina                       [ Ctrl-c ] Salir" 
	AT 2,1 ATTRIBUTE(BOLD)
	DISPLAY "HASTA" AT 4,22 
        DECLARE cursor_3 CURSOR FOR SELECT zona_cod,
                                           fecha_desde_sm,
 				           fecha_hasta_sm,
					   monto_sm,
				           fecha_actualiza
      	FROM tabsalario_minimo2
	WHERE fecha_hasta_sm IS NULL
	ORDER BY 2,1
	LET i = 1
      	FOREACH cursor_3 INTO g_detalle[i].*
	      	LET i = i + 1
      	END FOREACH 

	IF (i-1) = 0 THEN
	   ERROR "No existe registros a modificar."
	   SLEEP 2
	   ERROR ""
	   RETURN
	END IF

      	CALL SET_COUNT(i-1)

        LET cla_sel= "SELECT * ",
                     " FROM com_comis_detalle ",
                     " WHERE fentcons >=","'",g_detalle[i].fecha_desde_sm,"'"
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
		   DELETE FROM tabsalario_minimo2
		   WHERE zona_cod       = g_detalle[i].zona_cod 
		     AND fecha_hasta_sm IS NULL

		   ERROR "REGISTRO ELIMINADO" SLEEP 2 ERROR ""
		   SELECT MAX(fecha_hasta_sm) INTO l_max_fecha 
		   FROM tabsalario_minimo2
		        UPDATE tabsalario_minimo2 SET fecha_hasta_sm = NULL 
		        WHERE zona_cod       = g_detalle[i].zona_cod 
			AND   fecha_hasta_sm = l_max_fecha
                   CALL Inicializa()
	           EXIT DISPLAY
              	ON KEY ( INTERRUPT )
                   CALL Inicializa()
	           EXIT DISPLAY
      	END DISPLAY
     END IF
END FUNCTION
