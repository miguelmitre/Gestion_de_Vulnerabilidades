##########################################################################
#Proyecto	   => Sistema de Afores (MEXICO)                         #
#Propietario        > EFP                                                #
#Programa AFIM004  => MANTENIMIENTO DE PRE-BENEFICIARIOS                 #
#Autor             => MAURO MUNIZ CABALLERO                              #
#Fecha             => 19 DE NOVIEMBRE DE 2000. 	                         #
#Sistema           => AFI.                                               #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_afili RECORD 
        n_seguro         LIKE afi_solicitud.n_seguro,
        n_rfc            CHAR(13),
        n_unico          CHAR(18),
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        n_folio          LIKE afi_solicitud.n_folio,
        tipo_solicitud   LIKE afi_solicitud.tipo_solicitud,
        pat_benef        CHAR(40),
        mat_benef        CHAR(40),
        nom_benef        CHAR(40),
        fena_benef       DATE,
        sexo_cod         SMALLINT,
        sexo_desc        CHAR(40),
        paren_cod        SMALLINT,
        paren_desc       CHAR(40),
        porcentaje       DECIMAL(5,2),
        consec           SMALLINT
    END RECORD

    DEFINE x_benef ARRAY[10] OF RECORD
        pat_benef  CHAR(40),
        mat_benef  CHAR(40),
        nom_benef  CHAR(40),
        fena_benef DATE,
        sexo_cod   SMALLINT,
        sexo_desc  CHAR(40),
        paren_cod  SMALLINT,
        paren_desc CHAR(40),
        porcentaje DECIMAL(5,2),
        consec      INTEGER
    END RECORD

    DEFINE g_afore RECORD LIKE tab_afore.*
    DEFINE l_afiliados RECORD LIKE afi_solicitud.*

    DEFINE 
        aux_pausa      ,
        enter          ,
        ACCION         CHAR(1) ,
        g_usuario      CHAR(8) ,
        tipo_solicitud CHAR(8) ,
        x_seguro       CHAR(11),
        x_rfc          CHAR(13),
        x_unico        CHAR(18),
        pat            CHAR(40),
        mat            CHAR(40),
        nom            CHAR(40),
        bandera        SMALLINT,
        g_parametro1   CHAR(11),
        g_parametro2   CHAR(1)      ,
        g_parametro3   SMALLINT     ,
        g_parametro4   DECIMAL(10,0)     

    DEFINE 
        x_fena    DATE,
        HOY       DATE

    DEFINE 
        valor_porcentaje SMALLINT,
        tot_porcentaje   SMALLINT,
        n_folio          DECIMAL(10,0)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        PROMPT LINE LAST
	
    DEFER INTERRUPT

    CALL STARTLOG("AFIM004.log")
    CALL inicio()   #i
    CALL proceso_principal()   #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET g_parametro1  = ARG_VAL(1)
    LET g_parametro2  = ARG_VAL(2)
    LET g_parametro3  = ARG_VAL(3)
    LET g_parametro4  = ARG_VAL(4)

    LET ACCION = g_parametro2

    LET HOY = TODAY

    IF g_afili.n_seguro = " " THEN
    ERROR "ESTE PROGRAMA SOLO PUEDE SER EJECUTADO DESDE SU MODULO PRINCIPAL" 
        SLEEP 3
        EXIT PROGRAM
    END IF

    SELECT *, user 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore
    WHERE  marca = 1

        SELECT a.n_rfc,
               a.n_unico,
               a.n_seguro,
               a.paterno,
               a.materno,
               a.nombres,
               a.fena,
               USER
        INTO   g_afili.n_rfc,
               g_afili.n_unico,
               g_afili.n_seguro,
               g_afili.paterno,
               g_afili.materno,
               g_afili.nombres,
               g_afili.fena,
               g_usuario
        FROM   afi_solicitud a
        WHERE  a.n_folio        = g_parametro4
        AND    a.tipo_solicitud = g_parametro3

     LET g_afili.tipo_solicitud = g_parametro3
     LET g_afili.n_folio        = g_parametro4

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_3 AT 2,2 WITH FORM "AFIM0041" ATTRIBUTE(BORDER)
    DISPLAY " AFIM004                 MANTENIMIENTO AFILIADOS                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        B E N E F I C I A R I O S                              " AT 8,1 ATTRIBUTE(REVERSE) 

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    CASE 
        WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E"
            MENU "BENEFICIARIOS"
            COMMAND "Agrega" "Agrega Beneficiarios"
		    LET ACCION = "A"
	            CALL Inicializa()
	            CALL Agrega(1,2)
	            CALL Inicializa()
            COMMAND "Consulta" "Consulta Beneficiarios"
		    LET ACCION = "C"
	            CALL Inicializa()
                    CALL Consulta()
	            CALL Inicializa()
            COMMAND "Modifica" "Modifica Beneficiarios"
		    LET ACCION = "M"
	            CALL Inicializa()
                    CALL Modifica()
	            CALL Inicializa()
            COMMAND "Elimina" "Elimina Beneficiarios"
		    LET ACCION = "E"
	            CALL Inicializa()
                    CALL Elimina()
	            CALL Inicializa()
            COMMAND "Salir" "Salir de Programa"
		    CALL Verifica_100()
		    IF valor_porcentaje = 100 OR
		        valor_porcentaje IS NULL THEN
		        EXIT MENU
		    ELSE
        ERROR "SUMA DE PORCENTAJES PARA BENEFICIARIOS DEBE SER 100% Y ES ",
                        valor_porcentaje USING "##&","%"
		    END IF
	    END MENU
        WHEN ACCION = "C"
            MENU "BENEFICIARIOS"
            COMMAND "Consulta" "Consulta Beneficiarios"
		    LET ACCION = "C"
	            CALL Inicializa()
                    CALL Consulta()
	            CALL Inicializa()
            COMMAND "Salir" "Salir de Programa"
		    EXIT MENU
            END MENU
    END CASE

    CLOSE WINDOW ventana_3

END FUNCTION

FUNCTION Inicializa()
#i-------------------

    DEFINE i SMALLINT

    FOR i = 1 TO 10
        INITIALIZE x_benef[i].* TO NULL 
    END FOR

END FUNCTION

FUNCTION Agrega(l_param,num)
#a--------------------------

    DEFINE 
        l_param CHAR(16)

    DEFINE 
        cons    ,
        num     ,
        arr_c   ,
        scr_l   ,
        j       ,
        suma    ,
        i       SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " AT 1,1 ATTRIBUTE(BOLD)

    LET bandera = 0

        SELECT a.*
        INTO   l_afiliados.*
        FROM   afi_solicitud a
        WHERE  a.n_folio  = g_afili.n_folio
        AND    a.tipo_solicitud = g_afili.tipo_solicitud

        IF STATUS = NOTFOUND THEN
            ERROR "Preafiliado no existe "
            RETURN
        ELSE  
            DISPLAY BY NAME g_afili.*
        END IF

    IF g_parametro2 = 'A' THEN
        CALL valor_afiliado()
    END IF

    INPUT ARRAY x_benef FROM scr_1.*
        BEFORE FIELD pat_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD pat_benef
            IF x_benef[arr_c].pat_benef IS NULL THEN
                ERROR "Campo Apellido Paterno NO puede ser NULO"
                NEXT FIELD pat_benef
            END IF

        BEFORE FIELD mat_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD mat_benef
		    IF x_benef[arr_c].mat_benef IS NULL THEN
		       ERROR "Apellido Materno es NULO"
		    END IF

        BEFORE FIELD nom_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD nom_benef
            IF x_benef[arr_c].nom_benef IS NULL THEN
                ERROR "Campo Nombre NO puede ser NULO"
                NEXT FIELD nom_benef
            END IF

	BEFORE FIELD fena_benef
	    LET arr_c = ARR_CURR()
	    LET scr_l = SCR_LINE()

	AFTER FIELD fena_benef
            IF x_benef[arr_c].fena_benef IS NULL THEN
		ERROR "Fecha Nacimiento es NULA"
            END IF

        BEFORE FIELD sexo_cod
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD sexo_cod
            IF x_benef[arr_c].sexo_cod IS NULL THEN
                CALL Despliega_sexos() RETURNING x_benef[arr_c].sexo_cod,
                                                 x_benef[arr_c].sexo_desc
                IF x_benef[arr_c].sexo_cod = 0 THEN 
                    NEXT FIELD sexo_cod 
                END IF
            ELSE
                SELECT sexo_desc 
                INTO   x_benef[arr_c].sexo_desc 
                FROM   tab_sexo
                WHERE  sexo_cod = x_benef[arr_c].sexo_cod

                IF STATUS = NOTFOUND THEN
                    ERROR "SEXO INEXISTENTE"
                    NEXT FIELD sexo_cod
                END IF
            END IF

        DISPLAY x_benef[arr_c].sexo_cod TO scr_1[scr_l].sexo_cod
        DISPLAY x_benef[arr_c].sexo_desc TO scr_1[scr_l].sexo_desc

	BEFORE FIELD paren_cod
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

	AFTER FIELD paren_cod
	    IF x_benef[arr_c].paren_cod IS NULL THEN
	        CALL Despliega_parentescos() 
                     RETURNING x_benef[arr_c].paren_cod,
                               x_benef[arr_c].paren_desc

           IF x_benef[arr_c].paren_cod = 0 THEN 
               NEXT FIELD paren_cod 
           END IF
	    ELSE
           SELECT paren_desc 
           INTO   x_benef[arr_c].paren_desc 
           FROM   tab_parentesco
           WHERE  paren_cod = x_benef[arr_c].paren_cod

           IF STATUS = NOTFOUND THEN
               ERROR "PARENTESCO INEXISTENTE"
               NEXT FIELD paren_cod
           END IF
       END IF

        DISPLAY x_benef[arr_c].paren_cod TO scr_1[scr_l].paren_cod
        DISPLAY x_benef[arr_c].paren_desc TO scr_1[scr_l].paren_desc

        BEFORE FIELD porcentaje
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD porcentaje
            IF x_benef[arr_c].porcentaje IS NULL THEN
                LET x_benef[arr_c].porcentaje = 0
                DISPLAY x_benef[arr_c].porcentaje TO scr_1[scr_l].porcentaje
            ELSE
                LET suma = 0
                FOR j = 1 TO 10
                    IF x_benef[j].porcentaje IS NOT NULL THEN
                        LET suma = suma + x_benef[j].porcentaje
                    END IF
                END FOR
                IF suma > 100 THEN
                    LET x_benef[arr_c].porcentaje = NULL
                    ERROR "La suma de porcentajes NO puede ser mayor a 100%"
		              NEXT FIELD porcentaje
                END IF
            END IF

        ON KEY ( ESC )
            LET tot_porcentaje = 0

	    SELECT SUM(porcentaje) 
            INTO   tot_porcentaje 
            FROM   afi_beneficiario
            WHERE  n_seguro = g_afili.n_seguro
            AND    n_folio  = g_afili.n_folio
            AND    tipo_solicitud = g_afili.tipo_solicitud

            IF tot_porcentaje IS NULL THEN 
                LET tot_porcentaje = 0 
            END IF

            FOR i = 1 TO 10
                IF x_benef[i].porcentaje IS NOT NULL THEN
                    LET tot_porcentaje = tot_porcentaje + x_benef[i].porcentaje
                END IF
            END FOR

            IF tot_porcentaje > 100 THEN
                ERROR "El total de porcentaje es igual a ",
                      tot_porcentaje USING "###.##",
                      "% y no debe ser mayor que el 100%"
                NEXT FIELD porcentaje
            END IF

            FOR i = 1 TO 10
                IF x_benef[i].paren_cod IS NOT NULL THEN
                    SELECT MAX(mc.consec)
                    INTO   cons
                    FROM   afi_beneficiario mc
                    WHERE  mc.n_seguro = g_afili.n_seguro
                    AND    mc.n_folio  = g_afili.n_folio
                    AND    mc.tipo_solicitud = g_afili.tipo_solicitud

                    IF cons IS NULL OR cons = 0 THEN
                        LET cons = 1
                    ELSE
                        LET cons = cons + 1
                    END IF

                    LET g_afili.consec = cons

		    INSERT INTO afi_beneficiario
                    VALUES (g_afili.n_seguro,
                            g_afili.n_folio,
                            g_afili.tipo_solicitud,
		    	                x_benef[i].paren_cod,
                            x_benef[i].pat_benef,
                            x_benef[i].mat_benef,
                            x_benef[i].nom_benef,
                            x_benef[i].fena_benef,
                            x_benef[i].sexo_cod,
                            x_benef[i].porcentaje,
                            g_afili.consec)
                END IF
            END FOR

            CALL inserta()

            CALL Inicializa()
            EXIT INPUT

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT

    END INPUT
    CALL borra_tmp_afiliado()

END FUNCTION


FUNCTION valor_afiliado()
#va----------------------

    --DELETE 
    --FROM   afi_solicitud 
    --WHERE  afi_solicitud.n_seguro       =  g_parametro1
    --AND    afi_solicitud.n_folio        =  g_parametro4
    --AND    afi_solicitud.tipo_solicitud =  g_parametro3

    INSERT INTO safre_tmp:tmp_afiliado
    VALUES(l_afiliados.n_seguro,
           l_afiliados.n_unico,
           l_afiliados.n_rfc,
           l_afiliados.paterno,
           l_afiliados.materno,
           l_afiliados.nombres,
           l_afiliados.fena,
           l_afiliados.n_folio,
           l_afiliados.tipo_solicitud)

END FUNCTION

FUNCTION inserta()
#ins--------------

    --IF g_parametro2 = 'A' THEN
        --IF NOT bandera THEN
            --INSERT INTO afi_solicitud VALUES(l_afiliados.*) 
            --LET bandera = 1
        --END IF
    --END IF
  
END FUNCTION

FUNCTION borra_tmp_afiliado()
#bta-------------------------

    DELETE FROM safre_tmp:tmp_afiliado
    WHERE n_folio = g_parametro4
    AND   tipo_solicitud = g_parametro3

END FUNCTION 

FUNCTION Consulta()
#c-----------------

    DEFINE 
        pos ,
        a   SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-C ] Salir  " AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    DECLARE cur_c CURSOR FOR
    SELECT ba.paterno,
           ba.materno,
           ba.nombres,
           ba.fena,
           ba.sexo,
           "",
           ba.parentesco,
           "",
           ba.porcentaje, 
           ba.consec
    FROM   afi_beneficiario ba
    WHERE  ba.n_seguro = g_afili.n_seguro
    AND    ba.n_folio  = g_afili.n_folio
    AND    ba.tipo_solicitud = g_afili.tipo_solicitud

    LET a = 1

    FOREACH cur_c INTO x_benef[a].*
        SELECT sexo_desc 
        INTO   x_benef[a].sexo_desc 
        FROM   tab_sexo
        WHERE  sexo_cod = x_benef[a].sexo_cod

        SELECT paren_desc 
        INTO   x_benef[a].paren_desc 
        FROM   tab_parentesco
        WHERE  paren_cod = x_benef[a].paren_cod
        LET a = a + 1
    END FOREACH

    CALL SET_COUNT(a-1)
    IF (a-1) >= 1 THEN
        DISPLAY ARRAY x_benef TO scr_1.*

        ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE 
        suma  ,
        j     ,
        i     ,
        a     ,
        pos   ,
	arr_c ,
        scr_l SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Modifica     [ Ctrl-C ] Salir sin Modificar  " AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    LET a = 1

    DECLARE cur_m CURSOR FOR
    SELECT bm.paterno,
           bm.materno,
           bm.nombres,
           bm.fena,
           bm.sexo,
           "",
           bm.parentesco,
           "",
           bm.porcentaje, 
           bm.consec
    FROM   afi_beneficiario bm
    WHERE  bm.n_seguro = g_afili.n_seguro
    AND    bm.n_folio  = g_afili.n_folio
    AND    bm.tipo_solicitud = g_afili.tipo_solicitud

    FOREACH cur_m INTO x_benef[a].*
        SELECT sexo_desc 
        INTO   x_benef[a].sexo_desc 
        FROM   tab_sexo
        WHERE  sexo_cod = x_benef[a].sexo_cod

        SELECT paren_desc 
        INTO   x_benef[a].paren_desc 
        FROM   tab_parentesco
        WHERE  paren_cod = x_benef[a].paren_cod

        LET a = a + 1
    END FOREACH

    IF (a-1) >= 1 THEN
        CALL SET_COUNT(a-1)

        INPUT ARRAY x_benef WITHOUT DEFAULTS FROM scr_1.*
        BEFORE FIELD pat_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD pat_benef
            IF x_benef[arr_c].pat_benef IS NULL THEN
                ERROR "Campo Apellido Paterno NO puede ser NULO"
                NEXT FIELD pat_benef
            END IF

        BEFORE FIELD mat_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD mat_benef
		    IF x_benef[arr_c].mat_benef IS NULL THEN
		       ERROR "Apellido Materno es NULO"
		    END IF

        BEFORE FIELD nom_benef
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD nom_benef
            IF x_benef[arr_c].nom_benef IS NULL THEN
                ERROR "Campo Nombre NO puede ser NULO"
                NEXT FIELD nom_benef
            END IF

	BEFORE FIELD fena_benef
	    LET arr_c = ARR_CURR()
	    LET scr_l = SCR_LINE()

	AFTER FIELD fena_benef
            IF x_benef[arr_c].fena_benef IS NULL THEN
		ERROR "Fecha Nacimiento es NULA"
            END IF

        BEFORE FIELD sexo_cod
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD sexo_cod
            IF x_benef[arr_c].sexo_cod IS NULL THEN
                CALL Despliega_sexos() RETURNING x_benef[arr_c].sexo_cod,
                                                 x_benef[arr_c].sexo_desc
		IF x_benef[arr_c].sexo_cod = 0 THEN 
                    NEXT FIELD sexo_cod 
                END IF
            ELSE
		SELECT sexo_desc 
                INTO   x_benef[arr_c].sexo_desc 
		FROM   tab_sexo
		WHERE  sexo_cod = x_benef[arr_c].sexo_cod

		IF STATUS = NOTFOUND THEN
	     	    ERROR "SEXO INEXISTENTE"
		    NEXT FIELD sexo_cod
		END IF
            END IF

        DISPLAY x_benef[arr_c].sexo_cod TO scr_1[scr_l].sexo_cod
        DISPLAY x_benef[arr_c].sexo_desc TO scr_1[scr_l].sexo_desc

	BEFORE FIELD paren_cod
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

	AFTER FIELD paren_cod
	    IF x_benef[arr_c].paren_cod IS NULL THEN
	        CALL Despliega_parentescos() 
                     RETURNING x_benef[arr_c].paren_cod,
                               x_benef[arr_c].paren_desc

		IF x_benef[arr_c].paren_cod = 0 THEN 
                    NEXT FIELD paren_cod 
                END IF
	    ELSE
		SELECT paren_desc 
                INTO   x_benef[arr_c].paren_desc 
		FROM   tab_parentesco
                WHERE  paren_cod = x_benef[arr_c].paren_cod

		IF STATUS = NOTFOUND THEN
	     	    ERROR "PARENTESCO INEXISTENTE"
		    NEXT FIELD paren_cod
		END IF
            END IF

        DISPLAY x_benef[arr_c].paren_cod TO scr_1[scr_l].paren_cod
        DISPLAY x_benef[arr_c].paren_desc TO scr_1[scr_l].paren_desc

        BEFORE FIELD porcentaje
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

        AFTER FIELD porcentaje
            IF x_benef[arr_c].porcentaje IS NULL THEN
		LET x_benef[arr_c].porcentaje = 0
		DISPLAY x_benef[arr_c].porcentaje TO scr_1[scr_l].porcentaje
            ELSE
		LET suma = 0
		FOR j = 1 TO 10
		    IF x_benef[j].porcentaje IS NOT NULL THEN
		        LET suma = suma + x_benef[j].porcentaje
		    END IF
		END FOR
		IF suma > 100 THEN
		    LET x_benef[arr_c].porcentaje = NULL
	            ERROR "La suma de porcentajes NO puede ser mayor a 100%"
		    NEXT FIELD porcentaje
		END IF
            END IF

        ON KEY ( ESC )
            FOR i = 1 TO 10
                IF x_benef[i].paren_cod IS NOT NULL THEN
            	    UPDATE afi_beneficiario 
                    SET    afi_beneficiario.parentesco = x_benef[i].paren_cod,
                           afi_beneficiario.porcentaje = x_benef[i].porcentaje,
                           afi_beneficiario.paterno    = x_benef[i].pat_benef,
                           afi_beneficiario.materno    = x_benef[i].mat_benef,
                           afi_beneficiario.nombres    = x_benef[i].nom_benef,
                           afi_beneficiario.sexo       = x_benef[i].sexo_cod,
                           afi_beneficiario.fena       = x_benef[i].fena_benef
                    WHERE  afi_beneficiario.n_seguro   = g_afili.n_seguro
		    AND    afi_beneficiario.n_folio    = g_afili.n_folio
		    AND    afi_beneficiario.tipo_solicitud 
                                                       = g_afili.tipo_solicitud
                    AND    afi_beneficiario.consec     = x_benef[i].consec  
                END IF
            END FOR

            ERROR "REGISTRO MODIFICADO" SLEEP 1 ERROR ""
            CALL Inicializa()
            EXIT INPUT

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT

        END INPUT
    END IF

END FUNCTION

FUNCTION Elimina()
#e----------------

    DEFINE 
        pos ,
        a   SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-b ] Elimina  [ Ctrl-C ] Salir  " AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    DECLARE cur_e CURSOR FOR
    SELECT be.paterno,
           be.materno,
           be.nombres,
           be.fena,
           be.sexo,
           "",
           be.parentesco,
           "",
           be.porcentaje, 
           be.consec
    FROM   afi_beneficiario be
    WHERE  be.n_seguro = g_afili.n_seguro
    AND    be.n_folio  = g_afili.n_folio
    AND    be.tipo_solicitud = g_afili.tipo_solicitud

    LET a = 1

    FOREACH cur_e INTO x_benef[a].*
        SELECT sexo_desc 
        INTO   x_benef[a].sexo_desc 
        FROM   tab_sexo
        WHERE  sexo_cod = x_benef[a].sexo_cod

        SELECT paren_desc 
        INTO   x_benef[a].paren_desc 
        FROM   tab_parentesco
        WHERE  paren_cod = x_benef[a].paren_cod

        LET a = a + 1
    END FOREACH

    CALL SET_COUNT(a-1)
    IF (a-1) >= 1 THEN
        DISPLAY ARRAY x_benef TO scr_1.*

        ON KEY ( CONTROL-b )
            LET a = ARR_CURR()


            WHILE TRUE
                CALL Pregunta()
                IF aux_pausa  MATCHES "[SsNn]" THEN
                    IF aux_pausa MATCHES "[Nn]" THEN
                        RETURN
                    ELSE
                        EXIT WHILE
                    END IF
                ELSE
                    DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                END IF
            END WHILE

            IF aux_pausa MATCHES "[Ss]" THEN 
                DELETE FROM afi_beneficiario
                WHERE  afi_beneficiario.n_seguro = g_afili.n_seguro
                AND    afi_beneficiario.n_folio  = g_afili.n_folio
                AND    afi_beneficiario.tipo_solicitud = g_afili.tipo_solicitud
                AND    afi_beneficiario.consec   = x_benef[a].consec
    
                ERROR "REGISTRO ELIMINADO" 
            END IF

            SLEEP 2 ERROR ""
            CALL Consulta()
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
            END DISPLAY
    END IF

END FUNCTION

FUNCTION Verifica_100()
#v100------------------

    SELECT SUM(porcentaje) 
    INTO   valor_porcentaje 
    FROM   afi_beneficiario
    WHERE  n_seguro = g_afili.n_seguro
    AND    n_folio  = g_afili.n_folio
    AND    tipo_solicitud = g_afili.tipo_solicitud

END FUNCTION

FUNCTION Pregunta()
#p-----------------

    PROMPT "Desea eliminar el registro [S/N] ?" FOR aux_pausa

END FUNCTION
 
