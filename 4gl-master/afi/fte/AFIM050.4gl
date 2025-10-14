##########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                       #
#Programa AFIM003  => MANTENIMIENTO A PATRONES DE SOLIC. DE AFILIAC.     #
#Fecha             => 12 ENERO DE 2001                                   #
#Autor             => MAURO MUNIZ CABALLERO.                             #
#Sistema           => AFI.                                               #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE pat ARRAY [50] OF RECORD
           reg_patronal        CHAR(11),
           razon_social        CHAR(59),
           reg_fed_contrib     CHAR(13),
           telefono            CHAR(10),
           calle               CHAR(40),
           numero              CHAR(12),
           cod_pos             CHAR(5),
           colonia             CHAR(30),
           delegacion          CHAR(30)
    END RECORD

    DEFINE ACCION	    CHAR(1)
    DEFINE tipo_solucion    CHAR(1)
    DEFINE enter            CHAR(1)
    DEFINE HOY	            DATE
    
    DEFINE
        arr    ,
        src    SMALLINT

    DEFINE g_afili		RECORD LIKE afi_solicitud.*
    DEFINE pate,mate,nome	CHAR(50)
    DEFINE x_seguro		CHAR(11)
    DEFINE x_unico		CHAR(18)
    DEFINE x_rfc   		CHAR(13)
    DEFINE aux_pausa	        CHAR(1)
    DEFINE x_fena  		DATE

    DEFINE delegap         ,
           ciudadp         ,
           estadop         SMALLINT ,
           desc_ciudadp_1  ,
           desc_estadop_1  CHAR(5)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
    INPUT WRAP              ,
    ACCEPT KEY CONTROL-I

    DEFER INTERRUPT

    CALL inicio()   #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

	LET g_afili.n_seguro = ARG_VAL(1)
	LET ACCION           = ARG_VAL(2)
        LET g_afili.n_folio  = ARG_VAL(3)
        LET g_afili.tipo_solicitud = ARG_VAL(4)
	LET HOY = TODAY

	IF g_afili.n_seguro = " " THEN
   ERROR "ESTE PROGRAMA SOLO PUEDE SER EJECUTADO DESDE SU MODULO PRINCIPAL" 
           SLEEP 3
	   EXIT PROGRAM
	END IF

	SELECT n_seguro,n_unico,n_rfc,paterno,materno,nombres,fena,n_folio
	INTO   x_seguro,x_unico,x_rfc,pate,mate,nome,x_fena,g_afili.n_folio
        FROM   afiliados
	WHERE  n_seguro = g_afili.n_seguro
        AND    n_folio = g_afili.n_folio   
        AND    tipo_solicitud = g_afili.tipo_solicitud

        IF STATUS = NOTFOUND THEN
          DISPLAY "No existen benefarios Asociados" AT 4,1   
	ELSE
	  DISPLAY "N.Seguro     ",x_seguro CLIPPED AT 2,1
	  DISPLAY "N.Rfc        ",x_rfc    CLIPPED AT 4,1
	  DISPLAY "Sr(a)        ",nome CLIPPED," ",pate CLIPPED,
                                  " ",mate CLIPPED AT 6,1
	  DISPLAY "F.Nacimiento ",x_fena USING "dd mmm yyyy" AT 8,1
	END IF
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 10,2 WITH FORM "AFIM0031" ATTRIBUTE(BORDER)
    DISPLAY " AFIM003                  P A T R O N E S                                  " AT 3,1 ATTRIBUTE(REVERSE)
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	CASE 
	   WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E" 
	        MENU " PATRONES "
                      COMMAND "Agrega " "Agrega Patrones"
		              LET ACCION = "A"
	                      CALL Inicializa()
	                      CALL Agrega()
	                      CALL Inicializa()
                      COMMAND "Consulta " "Consulta Patrones"
		              LET ACCION = "C"
	                      CALL Inicializa()
                              CALL Consulta()
	                      CALL Inicializa()
                      COMMAND "Modifica " "Modifica Patrones"
		              LET ACCION = "M"
	                      CALL Inicializa()
                              CALL Modifica()
	                      CALL Inicializa()
                      COMMAND "Elimina " "Elimina Patrones"
	                      CALL Inicializa()
                              CALL Elimina()
	                      CALL Inicializa()
                      COMMAND "Salir " "Salir de Programa"
		              EXIT MENU
	         END MENU
	   WHEN ACCION = "C"
	        MENU " PATRONES "
                      COMMAND "Consulta " "Consulta Patrones"
		              LET ACCION = "C"
	                      CALL Inicializa()
                              CALL Consulta()
                      COMMAND "Salir " "Salir de Programa"
		              EXIT MENU
	         END MENU
	   END CASE
	CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()
#-------------------

    INITIALIZE pat TO NULL
    CLEAR FORM

END FUNCTION

FUNCTION Agrega()
#----------------

    DEFINE i    SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,55 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " AT 1,1 ATTRIBUTE(BOLD)

    INPUT ARRAY pat WITHOUT DEFAULTS FROM scr_1.*  

        BEFORE FIELD reg_patronal
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
        AFTER FIELD reg_patronal
            IF pat[arr].reg_patronal IS NULL THEN
                ERROR "Registro Patronal NO puede ser NULO"
 	        NEXT FIELD reg_patronal
            END IF

               SELECT "X" FROM afipatro
	       WHERE  reg_patronal = pat[arr].reg_patronal
                AND  n_folio = g_afili.n_folio
               GROUP BY 1
               IF STATUS <> 100 THEN
                  ERROR "Este Registro Patronal ya existe para este Afiliado"
                  NEXT FIELD reg_patronal
               END IF

		SELECT razon_social 
                INTO   pat[arr].razon_social
                FROM   maepatro
		WHERE  reg_patronal = pat[arr].reg_patronal
	        IF STATUS <> NOTFOUND THEN
		    DISPLAY pat[arr].razon_social TO scr_1[src].razon_social
	            NEXT FIELD reg_fed_contrib
	        ELSE
	            #NEXT FIELD reg_patronal
	        END IF

        BEFORE FIELD razon_social
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
	    FOR i = 1 TO arr-1
                IF pat[i].reg_patronal = pat[arr].reg_patronal THEN
		    ERROR "Registro Patronal ya Ingresado"
		    LET pat[arr].reg_patronal = NULL
		    LET pat[arr].razon_social = NULL
		    DISPLAY pat[arr].reg_patronal TO scr_1[src].reg_patronal
		    DISPLAY pat[arr].razon_social TO scr_1[src].razon_social
		    NEXT FIELD reg_patronal
		END IF
	    END FOR

                SELECT "X"
                FROM   afipatro
                WHERE  reg_patronal = pat[arr].reg_patronal
                  AND  n_folio = g_afili.n_folio
                GROUP BY 1
                IF STATUS <> NOTFOUND THEN
		   ERROR "Patron ya fue ingresado a este trabajador"
		   LET pat[arr].reg_patronal = NULL
		   LET pat[arr].razon_social = NULL
		   DISPLAY pat[arr].reg_patronal TO scr_1[src].reg_patronal
		   DISPLAY pat[arr].razon_social TO scr_1[src].razon_social
		   NEXT FIELD reg_patronal
		END IF

        AFTER FIELD razon_social
            IF pat[arr].razon_social IS NULL THEN
                ERROR "Razon social NO puede ser NULA"
 	        NEXT FIELD razon_social
            END IF

        AFTER FIELD calle
            DISPLAY pat[arr].calle TO scr_1[src].calle
            NEXT FIELD numero

        AFTER FIELD numero
            DISPLAY pat[arr].numero TO scr_1[src].numero
            NEXT FIELD cod_pos

        AFTER FIELD cod_pos
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD numero
               END IF
               IF pat[arr].cod_pos IS NULL OR
                  pat[arr].cod_pos=" " THEN
                  CALL Despliega_codigo_postal()
                  RETURNING pat[arr].cod_pos,
                            pat[arr].colonia,
                            delegap,
                            pat[arr].delegacion,
                            ciudadp,
                            desc_ciudadp_1,
                            estadop,
                            desc_estadop_1

                  IF pat[arr].colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD cod_pos
                  END IF
               ELSE
                  SELECT "X" FROM tabcpos
                  WHERE cpos_cod = pat[arr].cod_pos
                  IF STATUS = 100 THEN
  ERROR "Cod. Post. no existe, dejar en NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_pos
                  END IF
                  CALL Despliega_colonias(pat[arr].cod_pos)
                  RETURNING pat[arr].colonia,
                            delegap,
                            pat[arr].delegacion,
                            ciudadp,
                            desc_ciudadp_1,
                            estadop,
                            desc_estadop_1
               END IF
               DISPLAY BY NAME pat[arr].* 

      ON KEY ( ESC )
            SELECT "X" FROM afipatro
	    WHERE  reg_patronal = pat[arr].reg_patronal
            AND    n_folio = g_afili.n_folio
            GROUP BY 1
            IF STATUS <> 100 THEN
               ERROR "Este Registro Patronal ya existe para este Afiliado"
               NEXT FIELD reg_patronal
            END IF
	 FOR i = 1 TO 50
             IF pat[i].reg_patronal IS NOT NULL THEN
                SELECT "X"
                FROM   maepatro
                WHERE  reg_patronal = pat[i].reg_patronal
                GROUP BY 1
                IF STATUS = NOTFOUND THEN
	            INSERT INTO maepatro VALUES (pat[i].reg_patronal,
		       		                 pat[i].razon_social)
                END IF

	            INSERT INTO afipatro VALUES (g_afili.n_folio,
			   	                 pat[i].reg_patronal,
			   	                 pat[i].reg_fed_contrib,
			   	                 pat[i].telefono,
                                                 pat[i].calle,
                                                 pat[i].numero,
                                                 pat[i].cod_pos,
                                                 pat[i].colonia,
                                                 pat[i].delegacion
                                                )
             END IF
	 END FOR
	 EXIT INPUT

      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT

      END INPUT

END FUNCTION

FUNCTION Consulta()
#------------------

    DEFINE x_y	SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,55 ATTRIBUTE(REVERSE)
    DISPLAY "[ Ctrl-C ] Para salir de la consulta" AT 1,1 ATTRIBUTE(BOLD)

    LET x_y = 1

    DECLARE cur_1 CURSOR FOR
    SELECT A.reg_patronal,
           A.razon_social,
           B.reg_fed_contrib,
           B.telefono,
           B.calle,
           B.numero,
           B.cod_pos,
           B.colonia,
           B.delegacion
    FROM   maepatro A,afipatro B
    WHERE  B.reg_patronal = A.reg_patronal
    AND    B.n_folio = g_afili.n_folio
    ORDER BY reg_patronal

    FOREACH cur_1 INTO pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH

    CALL SET_COUNT(x_y-1)
    DISPLAY array pat TO scr_1.*  
            ON KEY ( INTERRUPT )
	    EXIT DISPLAY
    END DISPLAY

END FUNCTION

FUNCTION Modifica()
#-----------------

    DEFINE 
        i      ,
        x_y    SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " AT 1,1 ATTRIBUTE(BOLD)

    DECLARE cur_2 CURSOR FOR
    SELECT A.reg_patronal,
           A.razon_social,
           B.reg_fed_contrib,
           B.telefono,
           B.calle,
           B.numero,
           B.cod_pos,
           B.colonia,
           B.delegacion
    FROM   maepatro A,afipatro B
    WHERE  B.reg_patronal = A.reg_patronal
    AND    B.n_folio = g_afili.n_folio
    ORDER BY reg_patronal

    LET x_y = 1
    FOREACH cur_2 INTO pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH

    CALL SET_COUNT(x_y-1)

    INPUT array  pat WITHOUT DEFAULTS FROM scr_1.*  

        BEFORE FIELD reg_patronal
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
	    NEXT FIELD razon_social

        BEFORE FIELD razon_social
	    FOR i = 1 TO arr-1
                IF pat[i].reg_patronal = pat[arr].reg_patronal THEN
		    ERROR "Registro Patronal ya Ingresado"
		    NEXT FIELD reg_patronal
		END IF
	    END FOR

        AFTER FIELD razon_social
            IF pat[arr].razon_social IS NULL THEN
                ERROR "Razon social NO puede ser NULA"
 	        NEXT FIELD razon_social
            END IF

        AFTER FIELD calle
            DISPLAY pat[arr].calle TO scr_1[src].calle
            NEXT FIELD numero

        AFTER FIELD numero
            DISPLAY pat[arr].numero TO scr_1[src].numero
            NEXT FIELD cod_pos

        AFTER FIELD cod_pos
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD numero
               END IF
               IF pat[arr].cod_pos IS NULL OR
                  pat[arr].cod_pos=" " THEN
                  CALL Despliega_codigo_postal()
                  RETURNING pat[arr].cod_pos,
                            pat[arr].colonia,
                            delegap,
                            pat[arr].delegacion,
                            ciudadp,
                            desc_ciudadp_1,
                            estadop,
                            desc_estadop_1

                  IF pat[arr].colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD cod_pos
                  END IF
               ELSE
                  SELECT "X" FROM tabcpos
                  WHERE cpos_cod = pat[arr].cod_pos
                  IF STATUS = 100 THEN
  ERROR "Cod. Post. no existe, dejar en NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_pos
                  END IF
                  CALL Despliega_colonias(pat[arr].cod_pos)
                  RETURNING pat[arr].colonia,
                            delegap,
                            pat[arr].delegacion,
                            ciudadp,
                            desc_ciudadp_1,
                            estadop,
                            desc_estadop_1
               END IF
               DISPLAY BY NAME pat[arr].* 

      ON KEY ( ESC )
	 FOR i = 1 TO 50
             IF pat[i].reg_patronal IS NOT NULL THEN
                    UPDATE maepatro
                    SET razon_social = pat[i].razon_social
                    WHERE  reg_patronal = pat[i].reg_patronal

                    UPDATE afipatro
                    SET    reg_fed_contrib = pat[i].reg_fed_contrib,
                           telefono        = pat[i].telefono,
                           calle           = pat[i].calle,
                           numero          = pat[i].numero,
                           cod_pos         = pat[i].cod_pos,
                           colonia         = pat[i].colonia,
                           delegacion      = pat[i].delegacion
                    WHERE  reg_patronal = pat[i].reg_patronal
                    AND    n_folio      = g_afili.n_folio
             END IF
	 END FOR
	 EXIT INPUT
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT
    END INPUT

END FUNCTION

FUNCTION Elimina()

    DEFINE x_y        SMALLINT
    DEFINE pos        SMALLINT
    DEFINE nro_reg    SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINAR " AT 1,55 ATTRIBUTE(REVERSE)
    DISPLAY "[ Ctrl-b ] Elimina          [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTE(BOLD)

    DECLARE cur_101 CURSOR FOR
    SELECT A.reg_patronal,
           A.razon_social,
           B.reg_fed_contrib,
           B.telefono,
           B.calle,
           B.numero,
           B.cod_pos,
           B.colonia,
           B.delegacion
    FROM   maepatro A,afipatro B
    WHERE  B.reg_patronal = A.reg_patronal
    AND    B.n_folio = g_afili.n_folio
    ORDER BY reg_patronal

    LET x_y = 1
    FOREACH cur_101 INTO pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH
    CALL SET_COUNT(x_y-1)
    DISPLAY array pat TO scr_1.*  
        ON KEY ( CONTROL-b )
	    LET pos = ARR_CURR()
	    CALL Pregunta()
	    IF aux_pausa MATCHES "[Ss]" THEN
		SELECT COUNT(*) INTO nro_reg FROM afipatro
		WHERE reg_patronal = pat[pos].reg_patronal
		IF nro_reg > 1 THEN
	   ERROR "NO PUEDE ELIMINAR PATRON... ESTA ASIGNADO A OTRO AFILIADO"
		ELSE
		    DELETE FROM afipatro
		    WHERE  reg_patronal = pat[pos].reg_patronal
                    AND    n_folio = g_afili.n_folio

		    ERROR "REGISTRO ELIMINADO" 
		    CLEAR FORM
                END IF
	    END IF
	       SLEEP 2 ERROR ""
	       EXIT DISPLAY
            ON KEY ( INTERRUPT )
	       EXIT DISPLAY
    END DISPLAY

END FUNCTION

FUNCTION Pregunta()
	PROMPT "Desea Eliminar Registros S/N ?" FOR aux_pausa
END FUNCTION
