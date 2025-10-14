################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa COMM001  => MANTENIMIENTO RESPONSABLE DE UNIDAD COMERCIAL            #
#Sistema           => COM. 					               #
#By                => GERARDO ALFONSO VEGA PAREDES.      		       #
#Fecha             => 25 febrero 1999.    				       #
#Fecha             => 16 enero 2001.      				       #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE aux_pausa			CHAR(1)
	DEFINE g_record 				RECORD
               cod_resp_uni     CHAR(10),
               nombre_resp_uni  CHAR(60),
               puesto_resp      INTEGER,
               codven           CHAR(10)
	END RECORD
	DEFINE HOY		DATE
	DEFINE descrip		CHAR(50),
               vnombre          CHAR(60),
        vpaterno LIKE pro_mae_promotor.paterno,
        vmaterno LIKE pro_mae_promotor.materno,
        vnombres LIKE pro_mae_promotor.nombres
END GLOBALS
MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
	        ACCEPT KEY CONTROL-I

	DEFER INTERRUPT

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMM0011" ATTRIBUTE(BORDER)
	DISPLAY " COMM001           MANTENIMIENTO RESPONSABLE DE UNIDAD                       " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	MENU "RESPONSABLES"
              COMMAND "Agrega" "Agrega Responsables"
	              CALL Inicializa()
	              CALL Agrega()
	              CALL Inicializa()
              COMMAND "Consulta" "Consulta Responsables"
	              CALL Inicializa()
                      CALL Consulta()
	              CALL Inicializa()
              COMMAND "Modifica" "Modificacion Responsables"
	              CALL Inicializa()
                      CALL Modifica()
	              CALL Inicializa()
              COMMAND "Elimina" "Eliminacion Responsables"
	              CALL Inicializa()
                      CALL Elimina() 
	              CALL Inicializa()
              COMMAND "Salir" "Salir de Programa"
		      EXIT MENU
	END MENU
END MAIN
################################################################################
FUNCTION Inicializa()
	INITIALIZE g_record.* TO NULL
	LET descrip = NULL
	LET vnombre = NULL
	DISPLAY BY NAME g_record.*
	DISPLAY BY NAME descrip
	DISPLAY BY NAME vnombre
END FUNCTION
################################################################################
FUNCTION Agrega()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE)
	DISPLAY "[ Esc ] Graba  [ Ctrl-C ] Salir sin Grabar" AT 1,1 ATTRIBUTE(BOLD)
	INPUT BY NAME g_record.*
	   AFTER FIELD cod_resp_uni
		 IF g_record.cod_resp_uni IS NULL THEN
		    ERROR "Codigo Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD cod_resp_uni
		 END IF
		 SELECT "X" FROM com_respon_unidad
		 WHERE cod_resp_uni = g_record.cod_resp_uni
		 IF STATUS <> NOTFOUND THEN
		    ERROR "Codigo Responsable de la Unidad Ya Existe"
		    NEXT FIELD cod_resp_uni
		 END IF
	   AFTER FIELD nombre_resp_uni
		 IF g_record.nombre_resp_uni IS NULL THEN
		    ERROR "Nombre Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD nombre_resp_uni
		 END IF
	   AFTER FIELD puesto_resp
		 IF g_record.puesto_resp IS NULL THEN
		    CALL Despliega_puesto()
		 ELSE
		    SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo de puesto Inexistente"
		       NEXT FIELD puesto_resp
		    END IF
		 END IF
		 DISPLAY BY NAME g_record.puesto_resp,descrip

           AFTER FIELD codven
                 IF g_record.codven IS NULL THEN
                    CALL Despliega_codven() 
                 ELSE
                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                        IF STATUS = 100 THEN
                           LET vnombre = null
                           ERROR "Codigo Promotor Inexistente"
                        ELSE
                           LET vnombre = vpaterno CLIPPED," ",
                                         vmaterno CLIPPED," ",
                                         vnombres CLIPPED
                        END IF
                 END IF
		 DISPLAY BY NAME g_record.codven,vnombre

	   ON KEY ( INTERRUPT )
	      CALL Inicializa()
	      EXIT INPUT
	   ON KEY ( ESC )
		 IF g_record.cod_resp_uni IS NULL THEN
		    ERROR "Codigo Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD cod_resp_uni
		 END IF
		 IF g_record.nombre_resp_uni IS NULL THEN
		    ERROR "Nombre Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD nombre_resp_uni
		 END IF
		 IF g_record.puesto_resp IS NULL THEN
		    ERROR "Puesto Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD puesto_resp
		 END IF
		 IF g_record.codven IS NULL THEN
                       LET vnombre = null
		    ERROR "Codigo Promotor no puede ser NULO"
		 END IF
		 SELECT "X" FROM com_respon_unidad 
		 WHERE cod_resp_uni = g_record.cod_resp_uni
		 IF STATUS <> NOTFOUND THEN
		    ERROR "Codigo de Responsable YA EXISTE"
		    NEXT FIELD cod_resp_uni
		 END IF
		 INSERT INTO com_respon_unidad VALUES (g_record.*,0)
	         ERROR "REGISTRO AGREGADO" SLEEP 2 ERROR ""
		 CALL Inicializa()
		 NEXT FIELD cod_resp_uni
	END INPUT
END FUNCTION
###############################################################################
FUNCTION Despliega_Responsables_Unidades()
	 DEFINE g_record_2 ARRAY[3000] OF RECORD
               cod_resp_uni     CHAR(10),
               nombre_resp_uni  CHAR(60),
               codven           CHAR(10)
	END RECORD
	DEFINE i SMALLINT
	DECLARE cursor_1 CURSOR FOR
	SELECT cod_resp_uni,
               nombre_resp_uni,
               codven 
          FROM com_respon_unidad
--	 ORDER BY cod_resp_uni
	LET i = 1
	FOREACH cursor_1 INTO g_record_2[i].*
		LET i = i + 1
	END FOREACH
	OPEN WINDOW ventana_2 AT 12,2 WITH FORM "COMM0012" ATTRIBUTE(BORDER)
	DISPLAY " COMM001           MANTENIMIENTO  RESPONSABLE DE UNIDAD                       " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	#DISPLAY " [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(BOLD)
	IF g_record_2[1].cod_resp_uni IS NULL THEN ERROR "NO HAY RESPONSABLES" END IF
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY g_record_2 TO scr_1.*
	        ON KEY ( CONTROL-M )
		   LET i = ARR_CURR()
		   EXIT DISPLAY
	        ON KEY ( INTERRUPT )
	           LET g_record.cod_resp_uni = NULL
		   EXIT DISPLAY
	END DISPLAY
	LET g_record.cod_resp_uni = g_record_2[i].cod_resp_uni
	CLOSE WINDOW ventana_2
END FUNCTION
###############################################################################
FUNCTION Consulta_s_n()
	PROMPT "Desea Consultar Otro Registro S/N ? " FOR CHAR aux_pausa
END FUNCTION
###############################################################################
FUNCTION Consulta()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY " [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(BOLD)
	INPUT BY NAME g_record.*
	   AFTER FIELD cod_resp_uni
		 IF g_record.cod_resp_uni IS NOT NULL THEN
		    SELECT * INTO g_record.* FROM com_respon_unidad
		    WHERE cod_resp_uni = g_record.cod_resp_uni
--                    ORDER BY 1
		    IF STATUS = NOTFOUND THEN
		       ERROR "Responsable de la Unidad NO Existe"
		       NEXT FIELD cod_resp_uni
		    END IF
		    DISPLAY BY NAME g_record.* 
		    SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    DISPLAY BY NAME descrip
                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                     IF STATUS = NOTFOUND THEN
                        LET vpaterno = NULL
                        LET vmaterno = NULL
                        LET vnombres = NULL
                        LET vnombre  = NULL
                     ELSE
                        LET vnombre = vpaterno CLIPPED," ",
                                      vmaterno CLIPPED," ",
                                      vnombres CLIPPED
                     END IF
		    DISPLAY BY NAME g_record.codven,vnombre

		    NEXT FIELD cod_resp_uni
		 ELSE
		    CALL Despliega_Responsables_Unidades()
		    SELECT * INTO g_record.* FROM com_respon_unidad 
		    WHERE cod_resp_uni = g_record.cod_resp_uni
--                    ORDER BY 1
		    DISPLAY BY NAME g_record.* 
	            SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    DISPLAY BY NAME descrip

                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                    END IF
                     IF STATUS = NOTFOUND THEN
                        LET vpaterno = NULL
                        LET vmaterno = NULL
                        LET vnombres = NULL
                        LET vnombre  = NULL
                     ELSE
                        LET vnombre = vpaterno CLIPPED," ",
                                      vmaterno CLIPPED," ",
                                      vnombres CLIPPED
                     END IF
		    DISPLAY BY NAME g_record.codven,vnombre
		    NEXT FIELD cod_resp_uni

	   ON KEY ( INTERRUPT )
		  CALL Inicializa()
		  EXIT INPUT
	END INPUT
END FUNCTION
###############################################################################
FUNCTION Modifica()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY " [ ESC ] p/ procesar               [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(BOLD)
	INPUT BY NAME g_record.*
           BEFORE FIELD cod_resp_uni
               CLEAR FORM
               LET vnombre = null
	   AFTER FIELD cod_resp_uni
		 IF g_record.cod_resp_uni IS NOT NULL THEN
		    SELECT * INTO g_record.* FROM com_respon_unidad
		    WHERE cod_resp_uni = g_record.cod_resp_uni
		    IF STATUS = NOTFOUND THEN
		       ERROR "Responsable de la Unidad NO Existe"
		       NEXT FIELD cod_resp_uni
		    END IF
		 ELSE
		    CALL Despliega_Responsables_Unidades()
		    SELECT * INTO g_record.* FROM com_respon_unidad 
		    WHERE cod_resp_uni = g_record.cod_resp_uni
		    IF STATUS = NOTFOUND THEN
		       NEXT FIELD cod_resp_uni
		    END IF
		 END IF

                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                    IF STATUS = 100 THEN
                       LET vnombre = null
                       ERROR "Codigo Promotor Inexistente"
                    ELSE
                       LET vnombre = vpaterno CLIPPED," ",
                                     vmaterno CLIPPED," ",
                                     vnombres CLIPPED
                    END IF
		    DISPLAY BY NAME g_record.codven,vnombre

		 DISPLAY BY NAME g_record.* 
		 SELECT desc_puesto INTO descrip FROM tab_puesto
		 WHERE cod_puesto = g_record.puesto_resp
		 DISPLAY BY NAME descrip
		 NEXT FIELD nombre_resp_uni
	   AFTER FIELD nombre_resp_uni
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD nombre_resp_uni
                 END IF
		 IF g_record.nombre_resp_uni IS NULL THEN
		    ERROR "Nombre Responsable de la Unidad NO pueder ser NULO"
		    NEXT FIELD nombre_resp_uni
		 END IF
	   AFTER FIELD puesto_resp
		 IF g_record.puesto_resp IS NULL THEN
		    CALL Despliega_puesto()
		 ELSE
		    SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    IF STATUS = NOTFOUND THEN
		       ERROR "Codigo de puesto Inexistente"
		       NEXT FIELD puesto_resp
		    END IF
		 END IF
		 DISPLAY BY NAME g_record.puesto_resp,descrip
           AFTER FIELD codven
                 IF g_record.codven IS NULL THEN
                    CALL Despliega_codven() 
                 ELSE
                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                    IF STATUS = 100 THEN
                       LET vnombre = NULL
                       ERROR "Codigo Promotor Inexistente"
                    ELSE
                       LET vnombre = vpaterno CLIPPED," ",
                                     vmaterno CLIPPED," ",
                                     vnombres CLIPPED
                    END IF
                 END IF
		 DISPLAY BY NAME g_record.codven,vnombre
                 NEXT FIELD codven
              

	   ON KEY ( INTERRUPT )
	      CALL Inicializa()
              CLEAR FORM
	      EXIT INPUT
	   ON KEY ( ESC )
	      UPDATE com_respon_unidad SET
		     nombre_resp_uni = g_record.nombre_resp_uni,
		     puesto_resp = g_record.puesto_resp,
		     codven = g_record.codven
	      WHERE cod_resp_uni      = g_record.cod_resp_uni
	      ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
	      CALL Inicializa()
	      NEXT FIELD cod_resp_uni
	END INPUT
END FUNCTION
################################################################################
FUNCTION Elimina()
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE)
	DISPLAY " [ Ctrl-B ] Elimina     [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(BOLD)
	INPUT BY NAME g_record.*
	   AFTER FIELD cod_resp_uni
		 IF g_record.cod_resp_uni IS NOT NULL THEN
		    SELECT * INTO g_record.* FROM com_respon_unidad
		    WHERE cod_resp_uni = g_record.cod_resp_uni
--                    ORDER  BY 1
		    IF STATUS = NOTFOUND THEN
		       ERROR "Responsable de la Unidad NO Existe"
		       NEXT FIELD cod_resp_uni
		    END IF
		    SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    DISPLAY BY NAME descrip
		    DISPLAY BY NAME g_record.* 
                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                    IF STATUS = 100 THEN
                       LET vnombre = null
                       ERROR "Codigo Promotor Inexistente"
                       NEXT FIELD codven
                     ELSE 
                       LET vnombre = vpaterno CLIPPED," ",
                             vmaterno CLIPPED," ",
                             vnombres CLIPPED

                    END IF
		    DISPLAY BY NAME g_record.codven,vnombre
		    NEXT FIELD cod_resp_uni
		 ELSE
		    CALL Despliega_Responsables_Unidades()
		    SELECT * INTO g_record.* FROM com_respon_unidad 
		    WHERE cod_resp_uni = g_record.cod_resp_uni
		    IF STATUS = NOTFOUND THEN
		       NEXT FIELD cod_resp_uni
		    END IF
		    DISPLAY BY NAME g_record.* 
		    SELECT desc_puesto INTO descrip FROM tab_puesto
		    WHERE cod_puesto = g_record.puesto_resp
		    DISPLAY BY NAME descrip
                    SELECT paterno,materno,nombres 
                      INTO vpaterno,vmaterno,vnombres
                      FROM pro_mae_promotor
                     WHERE codven = g_record.codven
                    IF STATUS = 100 THEN
                       LET vnombre = null
                       ERROR "Codigo Promotor Inexistente"
                       NEXT FIELD codven
                    ELSE
                       LET vnombre = vpaterno CLIPPED," ",
                                     vmaterno CLIPPED," ",
                                     vnombres CLIPPED
                    END IF
		    DISPLAY BY NAME g_record.codven,vnombre
		    NEXT FIELD cod_resp_uni
		 END IF
	   ON KEY ( CONTROL-B )
	      CALL Esta_seguro()
	      IF aux_pausa MATCHES "[Ss]" THEN
	         DELETE FROM com_respon_unidad
	         WHERE cod_resp_uni      = g_record.cod_resp_uni
	         ERROR "REGISTRO ELIMINADO" SLEEP 2 ERROR ""
	      ELSE
	         ERROR "ELIMINACION CANCELADA" SLEEP 2 ERROR ""
	      END IF
	      CALL Inicializa()
	      NEXT FIELD cod_resp_uni
	   ON KEY ( INTERRUPT )
		  CALL Inicializa()
		  EXIT INPUT
	END INPUT
END FUNCTION
###############################################################################
FUNCTION Esta_seguro()
	PROMPT "Esta Seguro S/N ? " FOR CHAR aux_pausa
END FUNCTION
################################################################################
FUNCTION Despliega_puesto()
	DEFINE f_reg ARRAY [200] OF RECORD
	       cod_puesto		LIKE tab_puesto.cod_puesto,
	       desc_puesto		LIKE tab_puesto.desc_puesto
	END RECORD
	DEFINE i 			SMALLINT
	DECLARE cursor_100 CURSOR FOR
	SELECT cod_puesto,desc_puesto FROM tab_puesto ORDER BY 1
	LET i = 1
	FOREACH cursor_100 INTO f_reg[i].*
	        LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	OPEN WINDOW ventana_3 AT 10,10 WITH FORM "COMM0013" ATTRIBUTE(BORDER)
	DISPLAY "                     P U E S T O S                             " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY ARRAY f_reg TO scr_1.*
		ON KEY ( INTERRUPT )
		   EXIT DISPLAY 
		ON KEY ( CONTROL-M )
	 	   LET i = ARR_CURR()
		   LET g_record.puesto_resp = f_reg[i].cod_puesto
		   LET descrip = f_reg[i].desc_puesto
		   EXIT DISPLAY
	END DISPLAY
	CLOSE WINDOW ventana_3
END FUNCTION

FUNCTION Despliega_codven()
	DEFINE f_reg ARRAY [15000] OF RECORD
	       codven	LIKE com_respon_unidad.codven,
	       paterno		LIKE afi_mae_afiliado.paterno,
	       materno		LIKE afi_mae_afiliado.materno,
	       nombres		LIKE afi_mae_afiliado.nombres
	END RECORD
	DEFINE i 			SMALLINT,
        cla_sel,cla_where CHAR(300),
        paterno,materno,nombres CHAR(50),salida CHAR(01)

	OPEN WINDOW ventana3 AT 08,08 WITH FORM "COMM0014" ATTRIBUTE(BORDER)
	DISPLAY "                     P R O M O T O R                           " AT 3,1 ATTRIBUTE(REVERSE)

        LET INT_FLAG = TRUE
        LET salida ="N"
        CONSTRUCT cla_where ON paterno,materno,nombres 
                         FROM paterno,materno,nombres
           ON KEY (ESC)                 
              LET INT_FLAG = FALSE
              EXIT CONSTRUCT            
           ON KEY (INTERRUPT)                 
              LET INT_FLAG = FALSE
              LET salida ="S"
              EXIT CONSTRUCT            
        END CONSTRUCT                   
        IF salida ="S" THEN
	   LET g_record.codven = NULL
           LET vnombre = NULL
	   CLOSE WINDOW ventana3
           RETURN
        END IF

 	      LET cla_sel="SELECT cod_promotor,paterno,materno,nombres ",
                          "FROM pro_mae_promotor ",
                          "WHERE ",cla_where CLIPPED,
                          " ORDER BY 2,3,4"

	LET i = 1
        PREPARE claexe FROM cla_sel
	DECLARE cursor_101 CURSOR FOR claexe
	FOREACH cursor_101 INTO f_reg[i].*
	        LET i = i + 1
	END FOREACH
	CALL SET_COUNT(i-1)
	DISPLAY ARRAY f_reg TO scr_1.*
		ON KEY ( INTERRUPT )
		   EXIT DISPLAY 
		ON KEY ( CONTROL-M )
	 	   LET i = ARR_CURR()
		   LET g_record.codven = f_reg[i].codven
                   LET vnombre=f_reg[i].paterno CLIPPED," ",
                               f_reg[i].materno CLIPPED," ",
                               f_reg[i].nombres CLIPPED
		   EXIT DISPLAY
	END DISPLAY
	CLOSE WINDOW ventana3
END FUNCTION
