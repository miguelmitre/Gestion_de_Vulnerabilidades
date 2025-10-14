################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => Carlos Welsh. 					       #
#Programa TABM011  => MANTENEDOR ARCHIVO safre_af
#Fecha             => 02 Noviembre 1996. 				       #
#By                => FRANCO ULLOA V.
#Sistema           => TAB. 					               #
################################################################################
DATABASE safre_af
GLOBALS
     DEFINE 
         aux_pausa             ,
         enter                 CHAR(1)
 
     DEFINE g_reg       RECORD  LIKE tab_afore_local.*

     DEFINE g_reg_des   RECORD 
         desc_delegacion       CHAR(60),
         desc_ciudad           CHAR(60),
         desc_estado           CHAR(60)
     END RECORD

     DEFINE
         des_afore             CHAR(60)

     DEFINE ya_existe             SMALLINT
	DEFINE HOY		DATE
	DEFINE HAY_AFORE			SMALLINT
END GLOBALS
MAIN
 	OPTIONS PROMPT LINE LAST,
 	        INPUT WRAP,
 		ACCEPT KEY control-o
 
 	DEFER INTERRUPT
 
	LET HOY = TODAY
 	OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TABM0111" ATTRIBUTE( BORDER)
 	DISPLAY " TABM011                  MANTENEDOR safre_af                                      " AT 3,1 ATTRIBUTE(REVERSE) 
	DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
 	
 	MENU "MANTENEDOR AFORE"
 	    COMMAND "Agrega"  "Agrega Afore"
		LET HAY_AFORE = FALSE
		SELECT COUNT(*) INTO HAY_AFORE FROM tab_afore_local
		IF NOT HAY_AFORE THEN
 	           CALL ingreso()
		ELSE
		   ERROR "YA EXISTE PAREMETRO TEPEYAC S.A."
		END IF
            COMMAND "Consulta" "Consulta Afores"
 	        CALL consulta()
            COMMAND "Modifica" "Modifica Afores"
 	        CALL modifica()
            COMMAND "Elimina" "Elimina Afores"
 	        CALL elimina()
            COMMAND "Salir" "Salir del Programa"
 	        EXIT MENU
 	END MENU
        CLOSE WINDOW ventana_1
 END MAIN

FUNCTION inicializa()
#---------------------

    INITIALIZE g_reg.* TO NULL
    DISPLAY BY NAME g_reg.*

    INITIALIZE g_reg_des.* TO NULL
    DISPLAY BY NAME g_reg_des.*

    CLEAR FORM
END FUNCTION

FUNCTION ingreso()
#----------------
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Ingreso        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " AGREGA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    INPUT BY NAME g_reg.*
        AFTER FIELD codigo_afore
            IF g_reg.codigo_afore IS NULL THEN
	        ERROR "Codigo NO puede ser nulo"
 	        NEXT FIELD codigo_afore
            END IF

            SELECT COUNT(*)
            INTO   ya_existe
            FROM   tab_afore_local
            WHERE  codigo_afore = g_reg.codigo_afore

            IF ya_existe > 0 then
                ERROR "Codigo ya existe "
                NEXT FIELD codigo_afore
            END IF         

            AFTER FIELD razon_social
                IF g_reg.razon_social IS NULL THEN
 		    ERROR "Debe ser ingresada la razon social de la empresa"
 		    NEXT FIELD  razon_social
                END IF
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD codigo_afore
                END IF

            AFTER FIELD representante
                IF g_reg.representante IS NULL THEN
 		    ERROR "Debe ingresar el representante legal "
 		    NEXT FIELD  representante
                END IF
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
 		    NEXT FIELD  razon_social
                END IF
 
            AFTER FIELD calle
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
 		    NEXT FIELD  representante
                END IF
                IF g_reg.calle IS NULL THEN
 		    ERROR "Debe ingresar el nombre de la calle"
 		    NEXT FIELD  calle
                END IF

            AFTER FIELD dis_numero
                IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
 		    NEXT FIELD  calle
                END IF
                IF g_reg.numero IS NULL THEN
 	            ERROR "La numeracion debe ser ingresada"
 	            NEXT FIELD  dis_numero
                END IF
#jerry1
            AFTER FIELD cod_postal
	       IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD dis_numero
	       END IF
	       IF g_reg.cod_postal IS NULL THEN
                  call Despliega_codigo_postal()
                  RETURNING g_reg.cod_postal,
                            g_reg.colonia,
                            g_reg.delegacion,
                            g_reg_des.desc_delegacion,
                            g_reg.ciudad,
                            g_reg_des.desc_ciudad,
                            g_reg.estado,
                            g_reg_des.desc_estado

                  IF g_reg.colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD cod_postal
                  END IF
               ELSE
                  SELECT "X" FROM tab_codpos
                  WHERE cpos_cod = g_reg.cod_postal
                  IF STATUS = 100 THEN
                     ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_postal
                  END IF
                  call Despliega_colonias(g_reg.cod_postal)
                  RETURNING g_reg.colonia,
                            g_reg.delegacion,
                            g_reg_des.desc_delegacion,
                            g_reg.ciudad,
                            g_reg_des.desc_ciudad,
                            g_reg.estado,
                            g_reg_des.desc_estado

               END IF
{
            AFTER FIELD cod_postal
	       IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD dis_numero
	       END IF
               IF g_reg.cod_postal IS NULL THEN
                  ERROR "Codigo Postal NO puede ser NULO"
                  NEXT FIELD cod_postal
               END IF
               SELECT * FROM tab_codpos
               WHERE cpos_cod = g_reg.cod_postal
               IF STATUS = 100 THEN
                  ERROR "Este Codigo Postal no existe en el catalogo"
                  NEXT FIELD cod_postal
	       END IF
               call Despliega_colonias(g_reg.cod_postal)
               RETURNING g_reg.colonia,
                         g_reg.delegacion,
                         g_reg_des.desc_delegacion,
                         g_reg.ciudad,
                         g_reg_des.desc_ciudad,
                         g_reg.estado,
                         g_reg_des.desc_estado
}
                DISPLAY BY NAME g_reg.*
                DISPLAY BY NAME g_reg_des.*

 	    ON KEY ( ESC )
                  SELECT "X" FROM tab_codpos
                  WHERE cpos_cod = g_reg.cod_postal
                  IF STATUS = 100 THEN
                     ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_postal
                  END IF
                  IF g_reg.colonia IS NULL THEN
                     ERROR "Los datos del Domicilio no pueden ser NULO"
                     NEXT FIELD cod_postal
                  END IF
                IF g_reg.codigo_afore IS NULL THEN
 		    ERROR "Codigo NO puede ser nulo"
 		    NEXT FIELD  codigo
                END IF
 
                IF g_reg.razon_social IS NULL THEN
 		    ERROR "Debe ser ingresada la razon social de la empresa"
 		    NEXT FIELD  razon_social
                END IF
 
                IF g_reg.representante IS NULL THEN
 		    ERROR "Debe ingresar el representante legal "
 		    NEXT FIELD  representante
                END IF

                IF g_reg.calle IS NULL THEN
 		    ERROR "Debe ingresar el nombre de la calle"
 		    NEXT FIELD  calle
                END IF

                IF g_reg.numero IS NULL THEN
 		    ERROR "La numeracion debe ser ingresada"
 		    NEXT FIELD  dis_numero
                END IF

                IF g_reg.colonia IS NULL THEN
 		    ERROR "La colonia debe ser ingresada "
 		    NEXT FIELD  colonia
                END IF

                IF g_reg.delegacion IS NULL THEN
 		    ERROR "La delegacion debe ser ingresada "
 		    NEXT FIELD  delegacion
                END IF

                IF g_reg.ciudad IS NULL THEN
 		    ERROR "La ciudad debe ser ingresada "
 		    NEXT FIELD  ciudad
                END IF

                IF g_reg.estado IS NULL THEN
 		    ERROR "El estado debe ser ingresado "
 		    NEXT FIELD  estado
                END IF

                INSERT INTO tab_afore_local VALUES ( g_reg.* ) 
 		ERROR "REGISTRO INGRESADO" SLEEP 1
 		ERROR ""
                CALL Inicializa()
 		NEXT FIELD codigo_afore
                ON KEY (INTERRUPT)
                CALL Inicializa()
                EXIT INPUT
 	END INPUT
END FUNCTION

FUNCTION consulta()
#------------------
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Consulta        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    CALL inicializa()
    INPUT BY NAME  g_reg.codigo_afore
        AFTER FIELD codigo_afore
            IF g_reg.codigo_afore IS NULL THEN
                CALL Despliega_safre_af()
                     RETURNING g_reg.codigo_afore,des_afore 
            END IF

            SELECT *
            INTO   g_reg.*
            FROM   tab_afore_local
            WHERE  codigo_afore = g_reg.codigo_afore
            IF STATUS = NOTFOUND  THEN
                ERROR "El codigo_afore ",g_reg.codigo_afore," no existe "
                CALL inicializa() 
                NEXT FIELD codigo_afore
            END IF

	        SELECT deleg_desc 
                INTO   g_reg_des.desc_delegacion FROM   tab_delegacion
	        WHERE deleg_cod = g_reg.delegacion

		SELECT ciudad_desc 
                INTO   g_reg_des.desc_ciudad FROM   tab_ciudad
		WHERE  ciudad_cod= g_reg.ciudad

		SELECT estad_desc
                INTO   g_reg_des.desc_estado FROM   tab_estado
		WHERE  estad_cod = g_reg.estado

                DISPLAY BY NAME g_reg.*
                DISPLAY BY NAME g_reg_des.*
            ON KEY (INTERRUPT)
               CALL Inicializa()
               EXIT INPUT
    END INPUT
END FUNCTION

FUNCTION modifica()
#------------------
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Modifica        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    INPUT BY NAME  g_reg.*
        AFTER FIELD codigo_afore
            IF g_reg.codigo_afore IS NULL THEN
                CALL Despliega_safre_af()
                     RETURNING g_reg.codigo_afore,des_afore 
            END IF
                
            SELECT *
            INTO   g_reg.*
            FROM   tab_afore_local
            WHERE  codigo_afore = g_reg.codigo_afore

            IF STATUS = NOTFOUND THEN
                ERROR"Codigo ",g_reg.codigo_afore," no existe "
                NEXT FIELD codigo_afore
            END IF

	    CALL des_delegacion(g_reg.delegacion) 
                RETURNING g_reg_des.desc_delegacion
	    CALL des_ciudad(g_reg.ciudad) 
                RETURNING g_reg_des.desc_ciudad
	    CALL des_estado(g_reg.estado) 
                RETURNING g_reg_des.desc_estado
  
            DISPLAY BY NAME g_reg.*
            DISPLAY BY NAME g_reg_des.*
  
        AFTER FIELD razon_social
            IF g_reg.razon_social IS NULL THEN
 	        ERROR "Debe ser ingresada la razon social de la empresa"
 	        NEXT FIELD  razon_social
            END IF
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD razon_social
            END IF

        AFTER FIELD representante
            IF g_reg.representante IS NULL THEN
 		ERROR "Debe ingresar el representante legal "
 		NEXT FIELD  representante
            END IF
 
        AFTER FIELD calle
            IF g_reg.calle IS NULL THEN
 		ERROR "Debe ingresar el nombre de la calle"
 		NEXT FIELD  calle
            END IF

        AFTER FIELD dis_numero
            IF g_reg.numero IS NULL THEN
 	        ERROR "La numeracion debe ser ingresada"
 	        NEXT FIELD  dis_numero
            END IF
#jerry2
            AFTER FIELD cod_postal
	       IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD dis_numero
	       END IF
	       IF g_reg.cod_postal IS NULL THEN
                  call Despliega_codigo_postal()
                  RETURNING g_reg.cod_postal,
                            g_reg.colonia,
                            g_reg.delegacion,
                            g_reg_des.desc_delegacion,
                            g_reg.ciudad,
                            g_reg_des.desc_ciudad,
                            g_reg.estado,
                            g_reg_des.desc_estado

                  IF g_reg.colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD cod_postal
                  END IF
               ELSE
                  SELECT "X" FROM tab_codpos
                  WHERE cpos_cod = g_reg.cod_postal
                  IF STATUS = 100 THEN
                     ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_postal
                  END IF
                  call Despliega_colonias(g_reg.cod_postal)
                  RETURNING g_reg.colonia,
                            g_reg.delegacion,
                            g_reg_des.desc_delegacion,
                            g_reg.ciudad,
                            g_reg_des.desc_ciudad,
                            g_reg.estado,
                            g_reg_des.desc_estado

               END IF
{
            AFTER FIELD cod_postal
	       IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD dis_numero
	       END IF
               IF g_reg.cod_postal IS NULL THEN
                  ERROR "Codigo Postal NO puede ser NULO"
                  NEXT FIELD cod_postal
               END IF
               SELECT * FROM tab_codpos
               WHERE cpos_cod = g_reg.cod_postal
               IF STATUS = 100 THEN
                  ERROR "Este Codigo Postal no existe en el catalogo"
                  NEXT FIELD cod_postal
	       END IF
               call Despliega_colonias(g_reg.cod_postal)
               RETURNING g_reg.colonia,
                         g_reg.delegacion,
                         g_reg_des.desc_delegacion,
                         g_reg.ciudad,
                         g_reg_des.desc_ciudad,
                         g_reg.estado,
                         g_reg_des.desc_estado
}

                DISPLAY BY NAME g_reg.*
                DISPLAY BY NAME g_reg_des.*
        AFTER FIELD telefono
           IF FGL_LASTKEY() = FGL_KEYVAL("UP") or
              FGL_LASTKEY() = FGL_KEYVAL("DOWN")  or
              FGL_LASTKEY() = 13 then
              NEXT FIELD razon_social
           END IF 
 
 	ON KEY ( ESC )
                  SELECT "X" FROM tab_codpos
                  WHERE cpos_cod = g_reg.cod_postal
                  IF STATUS = 100 THEN
                     ERROR "Cod. Post. no existe en catalogo, pon valor NULO p/desplegar pantalla de Codigos"
                     NEXT FIELD cod_postal
                  END IF
                  IF g_reg.colonia IS NULL THEN
                     ERROR "Los datos del Domicilio no pueden ser NULO"
                     NEXT FIELD cod_postal
                  END IF
            IF g_reg.codigo_afore IS NULL THEN
 		ERROR "Codigo NO puede ser nulo"
 		NEXT FIELD  codigo
            END IF
 
            IF g_reg.razon_social IS NULL THEN
 		ERROR "Debe ser ingresada la razon social de la empresa"
 		NEXT FIELD  razon_social
            END IF
 
            IF g_reg.representante IS NULL THEN
 		ERROR "Debe ingresar el representante legal "
 		NEXT FIELD  representante
            END IF

            IF g_reg.calle IS NULL THEN
 		ERROR "Debe ingresar el nombre de la calle"
 		NEXT FIELD  calle
            END IF

            IF g_reg.numero IS NULL THEN
 		ERROR "La numeracion debe ser ingresada"
 		NEXT FIELD  dis_numero
            END IF

            IF g_reg.colonia IS NULL THEN
 		ERROR "La colonia debe ser ingresada "
 		NEXT FIELD  colonia
            END IF

            IF g_reg.delegacion IS NULL THEN
 		ERROR "La delegacion debe ser ingresada "
 		NEXT FIELD  delegacion
            END IF

            IF g_reg.ciudad IS NULL THEN
 		ERROR "La ciudad debe ser ingresada "
 		NEXT FIELD  ciudad
            END IF

            IF g_reg.estado IS NULL THEN
 		ERROR "El estado debe ser ingresado "
 		NEXT FIELD  estado
            END IF

            UPDATE tab_afore_local
            SET    tab_afore_local.* = g_reg.*
            WHERE  codigo_afore = g_reg.codigo_afore

 	    ERROR "REGISTRO MODIFICADO" SLEEP 1
 	    ERROR ""

            CALL inicializa()
 		NEXT FIELD codigo_afore
            ON KEY (INTERRUPT)
            CALL Inicializa()
            EXIT INPUT
 	END INPUT
END FUNCTION

FUNCTION elimina()
#-----------------
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ( Esc ) Elimina        (Ctrl-c) Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
    CALL inicializa() 
    INPUT BY NAME  g_reg.codigo_afore
        AFTER FIELD codigo_afore
            IF g_reg.codigo_afore IS NULL THEN
                CALL Despliega_safre_af()
                     RETURNING g_reg.codigo_afore,des_afore 
            END IF

            SELECT *
            INTO   g_reg.*
            FROM   tab_afore_local
            WHERE  codigo_afore = g_reg.codigo_afore

 
            IF STATUS = NOTFOUND  THEN
                ERROR "El codigo_afore ",g_reg.codigo_afore," no existe "
                CALL inicializa() 
                NEXT FIELD codigo_afore
            END IF

	        SELECT deleg_desc 
                INTO   g_reg_des.desc_delegacion FROM   tab_delegacion
	        WHERE deleg_cod = g_reg.delegacion

		SELECT ciudad_desc 
                INTO   g_reg_des.desc_ciudad FROM   tab_ciudad
		WHERE  ciudad_cod= g_reg.ciudad

		SELECT estad_desc
                INTO   g_reg_des.desc_estado FROM   tab_estado
		WHERE  estad_cod = g_reg.estado

                DISPLAY BY NAME g_reg.*
                DISPLAY BY NAME g_reg_des.*

            PROMPT "Esta seguro de querer eliminar el registro ",g_reg.codigo_afore,  "  S/N " FOR CHAR enter
            IF enter = "s" or enter ="S" THEN
                DELETE FROM tab_afore_local
                WHERE  codigo_afore = g_reg.codigo_afore
                ERROR "REGISTRO ELIMINADO"
            ELSE
                ERROR "ELIMINAR CANCELADO"
            END IF
	    SLEEP 2 ERROR ""
            CALL inicializa() 
    END INPUT
END FUNCTION

FUNCTION des_delegacion(delegacion)
#------------------------
    DEFINE 
        delegacion         SMALLINT

    DEFINE 
        desc_delegacion    CHAR(40)

    SELECT deleg_desc
    INTO   desc_delegacion
    FROM   tab_delegacion
    WHERE  deleg_cod = delegacion

    IF STATUS = NOTFOUND THEN
        LET desc_delegacion =""
    END IF

    RETURN desc_delegacion 
END FUNCTION
 
FUNCTION des_ciudad(ciudad)
#------------------------
    DEFINE 
        ciudad             SMALLINT

    DEFINE 
        desc_ciudad        CHAR(40)

    SELECT ciudad_desc
    INTO   desc_ciudad
    FROM   tab_ciudad
    WHERE  ciudad_cod = ciudad

    IF STATUS = NOTFOUND THEN
        LET desc_ciudad =""
    END IF

    RETURN desc_ciudad 
END FUNCTION
 
FUNCTION des_estado(estado)
#------------------------
    DEFINE 
        estado             SMALLINT

    DEFINE 
        desc_estado        CHAR(40)

    SELECT estad_desc
    INTO   desc_estado
    FROM   tab_estado
    WHERE  estad_cod = estado

    IF STATUS = NOTFOUND THEN
        LET desc_estado =""
    END IF

    RETURN desc_estado 
END FUNCTION
 
