#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa SEPL0052 => CONSULTAS     DE DIRECCIONES SOLICITUDES AFILIACION   #
#Fecha             => 6 DE DICIEMBRE DE 2000                                #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Sistema           => AFI.                                                  #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        enter     CHAR(1)  ,
        aux_pausa CHAR(1)  ,
        g_hora    CHAR(8)  ,
        comma     CHAR(50) ,
        bandera   SMALLINT ,
        pos       SMALLINT ,
        HOY       DATE     ,
        sel_where CHAR(200),
        cla_where CHAR(200)

    DEFINE l_afiliados RECORD LIKE afi_solicitud.*

    DEFINE g_afiliados RECORD
        n_seguro    LIKE afi_solicitud.n_seguro,
        n_rfc       LIKE afi_solicitud.n_rfc   ,
        n_unico     LIKE afi_solicitud.n_unico ,
        paterno     LIKE afi_solicitud.paterno ,
        materno     LIKE afi_solicitud.materno ,
        nombres     LIKE afi_solicitud.nombres ,
        fena        LIKE afi_solicitud.fena    ,
        n_folio     LIKE afi_solicitud.n_folio ,
        tipo_solicitud SMALLINT                ,
        idrow       INTEGER                    ,
        calle       LIKE afi_domicilio.calle   ,
        calle_ref1  LIKE afi_domicilio.calle_ref1,
        calle_ref2  LIKE afi_domicilio.calle_ref2,  
        numero      LIKE afi_domicilio.numero  ,
        depto       LIKE afi_domicilio.depto   ,
        codpos      LIKE afi_domicilio.codpos  ,
        colonia     LIKE afi_domicilio.colonia ,
        delega      LIKE afi_domicilio.delega  ,
        desc_delega CHAR(50)                   ,
        ciudad      INTEGER                    ,
        desc_ciudad CHAR(50)                   ,
        estado      INTEGER                    ,
        desc_estado CHAR(50)                   ,
        pais_cod    LIKE afi_domicilio.pais_cod,
        pais_desc   CHAR(40)                   ,
        dom_cod     LIKE afi_domicilio.dom_cod ,
        dom_desc    CHAR(50)                   ,
        marca_envio CHAR(1)
    END RECORD

    DEFINE p_afiliados RECORD
        n_seguro LIKE afi_domicilio.nss      ,
        calle    LIKE afi_domicilio.calle    ,
        numero   LIKE afi_domicilio.numero   ,
        depto    LIKE afi_domicilio.depto    ,
        codpos   LIKE afi_domicilio.codpos   ,
        colonia  LIKE afi_domicilio.colonia  ,
        delega   LIKE afi_domicilio.delega   ,
        ciudad   INTEGER                     ,
        estado   INTEGER                     ,
        marca_envio CHAR(1)
    END RECORD

    DEFINE
        g_delegac_cod   INTEGER      ,
        g_delegac_desc  CHAR(50)     ,
        g_ciudad_cod    INTEGER      ,
        g_ciudad_desc   CHAR(50)     ,
        g_estado_cod    INTEGER      ,
        g_estado_desc   CHAR(50)     ,
        ACCION          CHAR(1)      ,
        comm            CHAR(300)    ,
        g_usuario       CHAR(8)      ,
        g_parametro1    DECIMAL(12,0),
        g_parametro2    CHAR(1)      ,
        g_parametro3    INTEGER      ,
        g_parametro4    SMALLINT     ,
        vcont           INTEGER      ,
        codpos_ant      CHAR(5)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("SEPL0052.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET g_parametro1 = ARG_VAL(1) #NUMERO DE FOLIO
    LET g_parametro2 = ARG_VAL(2) #ACCION A ESCOJER
    LET g_parametro3 = ARG_VAL(3) #CONTADOR
    LET g_parametro4 = ARG_VAL(4) #TIPO DE SOLICITUD

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           USER
    INTO   g_afiliados.n_seguro,
           g_afiliados.n_unico ,
           g_afiliados.n_rfc   ,
           g_afiliados.paterno ,
           g_afiliados.materno ,
           g_afiliados.nombres ,
           g_afiliados.fena    ,
           g_usuario 
    FROM   afi_solicitud af
    WHERE  af.n_folio        = g_parametro1
    AND    af.tipo_solicitud = g_parametro4

    LET g_afiliados.n_folio        = g_parametro1
    LET g_afiliados.tipo_solicitud = g_parametro4

    LET p_afiliados.calle       = NULL
    LET p_afiliados.numero      = NULL
    LET p_afiliados.depto       = NULL
    LET p_afiliados.codpos      = NULL
    LET p_afiliados.colonia     = NULL
    LET p_afiliados.delega      = NULL
    LET p_afiliados.ciudad      = NULL
    LET p_afiliados.estado      = NULL
    LET p_afiliados.marca_envio = NULL
    LET ACCION                  = g_parametro2
    LET vcont                   = g_parametro3
    LET p_afiliados.n_seguro    = g_afiliados.n_seguro

    LET HOY = TODAY

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_3 AT 2,2 WITH FORM "SEPM0021" ATTRIBUTE(BORDER)
    DISPLAY " SEPL0052          CONSULTAS SOLICITUDES AFILIACION                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                              DOMICILIO                                        " AT 8,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    CASE
        WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E" 
            MENU " DOMICILIOS "
                COMMAND "Consulta " "Consulta Domicilios"
                    LET ACCION = "C"
                    CALL Inicializa()
                    CALL Consulta()
                    CALL Inicializa() 
                COMMAND "Patronal " "Patronal Domicilios"
                    LET ACCION = "P"
                    CALL Inicializa()
                    CALL Agrega()
                    CALL Inicializa() 
                COMMAND "Salir " "Salir de Programa"
                    EXIT MENU
            END MENU
        WHEN ACCION = "C" 
            MENU " DOMICILIOS "
                COMMAND "Consulta " "Consulta Domicilios"
                    LET ACCION = "C"
                    CALL Inicializa()
                    CALL Consulta()
                    CALL Inicializa() 
                COMMAND "Salir " "Salir de Programa"
                    EXIT MENU
            END MENU
    END CASE

    --CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()
#I-------------------

    INITIALIZE g_afiliados.calle           TO NULL
    DISPLAY BY NAME g_afiliados.calle
    INITIALIZE g_afiliados.numero          TO NULL
    DISPLAY BY NAME g_afiliados.numero
    INITIALIZE g_afiliados.depto           TO NULL
    DISPLAY BY NAME g_afiliados.depto
    INITIALIZE g_afiliados.calle_ref1      TO NULL
    DISPLAY BY NAME g_afiliados.calle_ref1
    INITIALIZE g_afiliados.calle_ref2      TO NULL
    DISPLAY BY NAME g_afiliados.calle_ref2
    INITIALIZE g_afiliados.codpos          TO NULL
    DISPLAY BY NAME g_afiliados.codpos
    INITIALIZE g_afiliados.colonia         TO NULL
    DISPLAY BY NAME g_afiliados.colonia
    INITIALIZE g_afiliados.delega          TO NULL
    DISPLAY BY NAME g_afiliados.delega
    INITIALIZE g_afiliados.desc_delega     TO NULL
    DISPLAY BY NAME g_afiliados.desc_delega
    INITIALIZE g_afiliados.ciudad          TO NULL
    DISPLAY BY NAME g_afiliados.ciudad
    INITIALIZE g_afiliados.desc_ciudad     TO NULL
    DISPLAY BY NAME g_afiliados.desc_ciudad
    INITIALIZE g_afiliados.estado          TO NULL
    DISPLAY BY NAME g_afiliados.estado
    INITIALIZE g_afiliados.desc_estado     TO NULL
    DISPLAY BY NAME g_afiliados.desc_estado
    INITIALIZE g_afiliados.dom_cod	   TO NULL
    DISPLAY BY NAME g_afiliados.dom_cod
    INITIALIZE g_afiliados.dom_desc        TO NULL
    DISPLAY BY NAME g_afiliados.dom_desc
    INITIALIZE g_afiliados.pais_cod        TO NULL
    DISPLAY BY NAME g_afiliados.pais_cod
    INITIALIZE g_afiliados.pais_desc       TO NULL
    DISPLAY BY NAME g_afiliados.pais_desc
    INITIALIZE g_afiliados.marca_envio     TO NULL
    DISPLAY BY NAME g_afiliados.marca_envio

END FUNCTION

FUNCTION Agrega()
#A---------------

    DEFINE
        aux_paterno  CHAR(40),
        aux_materno  CHAR(40),
        aux_nombre   CHAR(40),
        bnd_marca    SMALLINT,
        vtotal       SMALLINT

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Grabar                               CONTROL [C] Salir sin Grabar     " AT 1,1 ATTRIBUTE(BOLD)

    LET HOY       = TODAY
    LET bandera   = 0
    LET bnd_marca = 0

    SELECT *
    INTO   l_afiliados.*
    FROM   afi_solicitud
    WHERE  n_folio  = g_parametro1
    AND    tipo_solicitud = g_parametro4

    IF SQLCA.SQLCODE <> 0 THEN
        ERROR "Solicitud de afiliacion NO existe"
        RETURN
    ELSE
        DISPLAY BY NAME g_afiliados.*
    END IF

    IF g_parametro2 = 'A' THEN
        CALL valor_afiliado()
    END IF

    IF ACCION = 'P' THEN
        LET g_afiliados.dom_cod = 7
    END IF

    INPUT BY NAME g_afiliados.* WITHOUT DEFAULTS

        BEFORE FIELD n_seguro
            NEXT FIELD calle

        AFTER FIELD calle
             IF bandera = 0 THEN
               	IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle 
               	END IF

               	IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD calle 
                END IF

	        IF g_afiliados.calle IS NULL OR 
                    g_afiliados.calle =" " THEN
		    ERROR "Calle NO puede ser nulo"
		    NEXT FIELD calle
	        END IF
	     ELSE
               	IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle 
               	END IF

               	IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD calle 
                END IF

	        IF g_afiliados.calle IS NULL OR 
                    g_afiliados.calle =" " THEN
		    ERROR "Campo calle no puede ser nulo"
		    NEXT FIELD calle
	        END IF
	     END IF

             NEXT FIELD numero

        AFTER FIELD numero
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle
               END IF

	       IF g_afiliados.numero IS NULL OR
                  g_afiliados.numero=" "  THEN
		  ERROR "ampo numero NO puede ser nulo"
		  NEXT FIELD numero
	       END IF

               NEXT FIELD depto

        AFTER FIELD depto
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD numero
               END IF

               NEXT FIELD calle_ref1

        AFTER FIELD calle_ref1
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD numero
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                  NEXT FIELD numero
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                   NEXT FIELD calle_ref2
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                   NEXT FIELD calle_ref2
               END IF

        AFTER FIELD calle_ref2
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD calle_ref1
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                  NEXT FIELD calle_ref1
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                   NEXT FIELD codpos
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                   NEXT FIELD codpos
               END IF

               NEXT FIELD codpos

        AFTER FIELD codpos
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD calle_ref2 
               END IF
               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
    	          IF g_afiliados.codpos IS NULL OR 
                     g_afiliados.codpos=" " THEN
                     NEXT FIELD codpos 
                  END IF
               END IF
               IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
    	          IF g_afiliados.codpos IS NULL OR 
                     g_afiliados.codpos=" " THEN
                     NEXT FIELD codpos 
                  END IF
               END IF
     	       IF g_afiliados.codpos[1] NOT MATCHES "[0-9]" OR
     	          g_afiliados.codpos[2] NOT MATCHES "[0-9]" OR
     	          g_afiliados.codpos[3] NOT MATCHES "[0-9]" OR
     	          g_afiliados.codpos[4] NOT MATCHES "[0-9]" OR
     	          g_afiliados.codpos[5] NOT MATCHES "[0-9]" THEN
		   ERROR "Codigo postal erroneo" SLEEP 2
		   NEXT FIELD codpos
               END IF
     	       IF g_afiliados.codpos IS NULL OR 
                  g_afiliados.codpos=" " THEN
                  CALL Despliega_codigo_postal()
                  RETURNING g_afiliados.codpos      , 
                            g_afiliados.colonia     ,
                            g_afiliados.delega      ,
                            g_afiliados.desc_delega , 
                            g_afiliados.ciudad      ,
                            g_afiliados.desc_ciudad ,
                            g_afiliados.estado      ,
                            g_afiliados.desc_estado
                  IF g_afiliados.colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD codpos
                  END IF
               ELSE
                  SELECT "X" 
                  FROM   tab_codpos
                  WHERE  cpos_cod = g_afiliados.codpos

                  IF STATUS = 100 THEN
                     ERROR "CP no existe en catalogo, deja valor NULO para desplegar pantalla de Codigos Postales"
                     NEXT FIELD codpos
                  END IF
                  CALL Despliega_colonias(g_afiliados.codpos)
                  RETURNING 
                            g_afiliados.colonia      ,
                            g_afiliados.delega       ,
                            g_afiliados.desc_delega,
                            g_afiliados.ciudad       ,
                            g_afiliados.desc_ciudad,
                            g_afiliados.estado       ,
                            g_afiliados.desc_estado
               END IF   
               DISPLAY BY NAME g_afiliados.*

               AFTER FIELD colonia
               IF g_afiliados.colonia IS NULL OR
                  g_afiliados.colonia MATCHES ' *' THEN
                   ERROR 'Colonia NO puede ser nulo'
                   SLEEP 2
                   ERROR ''
                   NEXT FIELD colonia
               ELSE
                   NEXT FIELD pais_cod
               END IF         

        BEFORE FIELD pais_cod
            IF g_afiliados.pais_cod IS NULL THEN
	       LET g_afiliados.pais_cod = "MEX"
	       DISPLAY BY NAME g_afiliados.pais_cod
            END IF

        AFTER FIELD pais_cod
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD dom_cod
            END IF

            IF g_afiliados.pais_cod IS NULL THEN
               CALL Despliega_pais() RETURNING g_afiliados.pais_cod,
                                               g_afiliados.pais_desc
            ELSE
               SELECT pais_desc
               INTO   g_afiliados.pais_desc
               FROM   tab_pais
               WHERE  pais_cod = g_afiliados.pais_cod

               IF STATUS = NOTFOUND THEN
                   ERROR "Pais inexistente"
                   NEXT FIELD pais_cod
               END IF
            END IF

	    DISPLAY BY NAME g_afiliados.pais_cod, g_afiliados.pais_desc

            NEXT FIELD dom_cod

        BEFORE FIELD dom_cod
            IF ACCION = 'A' THEN
                LET g_afiliados.dom_cod = 1
            ELSE
                LET g_afiliados.dom_cod = 7
            END IF

            DISPLAY BY NAME g_afiliados.dom_cod

        AFTER FIELD dom_cod
	       IF g_afiliados.dom_cod IS NULL THEN
	          CALL Despliega_tipos_dom() 
                       RETURNING g_afiliados.dom_cod,g_afiliados.dom_desc
	       ELSE
	          SELECT dom_desc 
	          INTO   g_afiliados.dom_desc 
	          FROM   tab_domicilio
	          WHERE  dom_cod = g_afiliados.dom_cod

 	          IF STATUS = NOTFOUND THEN
	             ERROR "Tipo Inexistente"
	             NEXT FIELD dom_cod
	          END IF
	       END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD numero 
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                     IF g_afiliados.dom_cod IS NULL THEN
                        ERROR "El campo Tipo Domicilio NO puede ser NULO"
                        NEXT FIELD dom_cod
                     END IF
               END IF

 	       DISPLAY BY NAME g_afiliados.dom_desc

               IF ACCION = 'P' OR
                  g_afiliados.dom_cod = 7 THEN
                   NEXT FIELD calle
               ELSE
                   NEXT FIELD marca_envio
               END IF

    BEFORE FIELD marca_envio
        SELECT COUNT(*)
        INTO   vtotal
        FROM   afi_domicilio
        WHERE  n_folio        = g_afiliados.n_folio
        AND    tipo_solicitud = g_afiliados.tipo_solicitud
        AND    marca_envio    = "X"
        AND    dom_cod        <> 7

        IF (vtotal = 0 OR vtotal IS NULL) AND
            g_afiliados.dom_cod <> 7 THEN 
            LET g_afiliados.marca_envio = 'X'
        ELSE
            LET g_afiliados.marca_envio = NULL
            LET bnd_marca = TRUE
        END IF 

        DISPLAY BY NAME g_afiliados.marca_envio

     AFTER FIELD marca_envio
        IF bnd_marca AND
           g_afiliados.marca_envio = 'X' THEN
            ERROR "Marca envio de correspondencia ya ingresada" SLEEP 3 
            LET g_afiliados.marca_envio = NULL
            DISPLAY BY NAME g_afiliados.marca_envio

            LET bnd_marca = FALSE
        END IF

        ON KEY ( INTERRUPT )
            EXIT INPUT

        ON KEY ( ESC )
            IF g_afiliados.calle IS NULL OR g_afiliados.calle = " " THEN
                ERROR "Calle NO puede ser nulo"
                NEXT FIELD calle
            END IF   

            IF g_afiliados.numero IS NULL OR g_afiliados.numero = " " THEN
                ERROR "Campo numero exterior NO puede ser nulo"
                NEXT FIELD numero
            END IF  

            IF g_afiliados.codpos[1] NOT MATCHES "[0-9]" OR
               g_afiliados.codpos[2] NOT MATCHES "[0-9]" OR
               g_afiliados.codpos[3] NOT MATCHES "[0-9]" OR
               g_afiliados.codpos[4] NOT MATCHES "[0-9]" OR
               g_afiliados.codpos[5] NOT MATCHES "[0-9]" THEN
                ERROR "Codigo postal erroneo" 
                NEXT FIELD codpos
            END IF   

            IF g_afiliados.colonia IS NULL THEN
                ERROR "Campo Codigo Postal NO puede ser NULO"
                NEXT FIELD codpos
            END IF  

            IF g_afiliados.dom_cod IS NULL THEN
                ERROR "Campo Tipo Domicilio NO puede ser NULO"
                NEXT FIELD dom_cod
            END IF  

            SELECT COUNT(*)
            INTO   vtotal
            FROM   afi_domicilio
            WHERE  n_folio        = g_afiliados.n_folio
            AND    tipo_solicitud = g_afiliados.tipo_solicitud
            AND    marca_envio    = "X"
            AND    dom_cod        <> 7

            IF (vtotal = 0 OR vtotal IS NULL) AND
                g_afiliados.dom_cod <> 7 THEN
                LET g_afiliados.marca_envio = 'X'
            ELSE
                LET g_afiliados.marca_envio = NULL
                LET bnd_marca = TRUE
            END IF 

            WHILE TRUE
                CALL pregunta()
                IF aux_pausa MATCHES "[SsNn]" THEN
                    EXIT WHILE
                END IF
            END WHILE

            CALL inserta()

            IF aux_pausa MATCHES "[Nn]" THEN
                EXIT INPUT
            END IF 
    END INPUT

    CALL borra_tmp_afiliado()

END FUNCTION

FUNCTION valor_afiliado()
#va----------------------

END FUNCTION

FUNCTION inserta()
#ins--------------
    ERROR ""
    CALL Inicializa()
    LET bandera = 1

END FUNCTION

FUNCTION crea_tmp_afiliado()
#cta------------------------

    SELECT "X" 
    FROM   safre_tmp:tmp_afiliado
    WHERE  n_folio        = l_afiliados.n_folio
    AND    tipo_solicitud = l_afiliados.tipo_solicitud
END FUNCTION

FUNCTION borra_tmp_afiliado()
#bta-------------------------
END FUNCTION

FUNCTION Consulta()
#C-----------------

    DEFINE a_afiliados  ARRAY[100] OF RECORD
        idrow         INTEGER                     ,
        calle         LIKE afi_domicilio.calle    ,
        numero        LIKE afi_domicilio.numero   ,
        depto         LIKE afi_domicilio.depto    ,
        calle_ref1    LIKE afi_domicilio.calle_ref1 ,
        calle_ref2    LIKE afi_domicilio.calle_ref2 ,
        codpos        LIKE afi_domicilio.codpos   ,
        colonia       LIKE afi_domicilio.colonia  ,
        delega        LIKE afi_domicilio.delega   ,
        desc_delega   CHAR(50)                    ,
        ciudad        LIKE afi_domicilio.ciudad   ,
        desc_ciudad   CHAR(50)                    ,
        estado        LIKE afi_domicilio.estado   ,
        desc_estado   CHAR(50)                    ,
        pais_cod      LIKE afi_domicilio.pais_cod ,
        pais_desc     CHAR(40)                    ,
        dom_cod       LIKE afi_domicilio.dom_cod  ,
        dom_desc      CHAR(50)                    ,
        marca_envio   CHAR(1)
    END RECORD

    DEFINE sale   SMALLINT
    DEFINE pos    SMALLINT
    DEFINE igual  CHAR(11)

    LET HOY = TODAY

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " CONTROL : [C] Salir                                                           " AT 1,1 ATTRIBUTE(BOLD)

        SELECT * 
        INTO   l_afiliados.*
        FROM   afi_solicitud
        WHERE  n_seguro = g_afiliados.n_seguro
        AND    n_folio  = g_parametro1
        AND    tipo_solicitud = g_parametro4

        IF STATUS = NOTFOUND THEN
            ERROR "Solicitud de Afiliacion NO existe"
        ELSE
            CALL Valores_afiliados(l_afiliados.*)
            DISPLAY BY NAME g_afiliados.*
        END IF

        LET pos   = 1
        LET sale  = FALSE
        LET igual =  g_afiliados.n_seguro

        DECLARE cursor_2 CURSOR FOR
        SELECT a.rowid       ,
               a.calle       ,
               a.numero      ,
               a.depto       ,
               a.calle_ref1  ,
               a.calle_ref2  ,
               a.codpos      ,
               a.colonia     ,
               a.delega      ,
               b.deleg_desc  ,
               a.ciudad      ,
               c.ciudad_desc ,
               a.estado      ,
               d.estad_desc  ,
               a.pais_cod    ,
               f.pais_desc   ,
               a.dom_cod     ,
               e.dom_desc    ,
               a.marca_envio
        FROM   afi_domicilio    a,
        OUTER  tab_delegacion b,
        OUTER  tab_ciudad     c,
        OUTER  tab_estado     d,
        OUTER  tab_domicilio  e,
        OUTER  tab_pais       f 
        WHERE  a.nss            = g_afiliados.n_seguro
        AND    a.n_folio        = g_parametro1
        AND    a.tipo_solicitud = g_parametro4
        AND    b.deleg_cod      = a.delega
        AND    c.ciudad_cod       = a.ciudad
        AND    d.estad_cod      = a.estado
        AND    e.dom_cod        = a.dom_cod
        AND    f.pais_cod       = a.pais_cod

        FOREACH cursor_2 INTO a_afiliados[pos].*

            LET pos = pos + 1

            IF pos > 10 THEN
               ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
               EXIT FOREACH
            END IF

        END FOREACH

        IF (pos-1) < 1 THEN
           ERROR "NO EXISTEN REGISTROS..."
           SLEEP 3
           ERROR ""
           CLEAR FORM
        END IF

        IF (pos-1) >= 1 THEN
           CALL SET_COUNT (pos-1)
           DISPLAY ARRAY a_afiliados TO scr.*

           ON KEY(INTERRUPT)
               CALL Inicializa()
               CLEAR FORM
               EXIT DISPLAY
           END DISPLAY
        END IF

END FUNCTION

FUNCTION Modifica()
#M-----------------

    DEFINE x_afiliados RECORD # Registro para "modifica"
        idrow     	INTEGER                    ,
        calle     	LIKE afi_domicilio.calle   ,
        numero    	LIKE afi_domicilio.numero  ,
        depto     	LIKE afi_domicilio.depto   ,
        calle_ref1	LIKE afi_domicilio.calle_ref1,
        calle_ref2	LIKE afi_domicilio.calle_ref2,
        codpos    	LIKE afi_domicilio.codpos  ,
        colonia  	LIKE afi_domicilio.colonia ,
        delega    	LIKE afi_domicilio.delega  ,
        ciudad    	INTEGER                    ,
        estado    	INTEGER                    ,
        dom_cod   	LIKE afi_domicilio.dom_cod ,
        pais_cod  	LIKE afi_domicilio.pais_cod,
        pais_desc 	CHAR(40)                   ,
        marca_envio 	CHAR(1)
    END RECORD

    DEFINE a_afiliados  ARRAY[100] OF RECORD
        idrow         	INTEGER                      ,
        calle         	LIKE afi_domicilio.calle     ,
        numero        	LIKE afi_domicilio.numero    ,
        depto        	LIKE afi_domicilio.depto     ,
        calle_ref1	LIKE afi_domicilio.calle_ref1  ,
        calle_ref2	LIKE afi_domicilio.calle_ref2  ,
        codpos        	LIKE afi_domicilio.codpos    ,
        colonia       	LIKE afi_domicilio.colonia   ,
        delega        	LIKE afi_domicilio.delega    ,
        desc_delega   	CHAR(50)	             ,
        ciudad        	INTEGER                      ,
        desc_ciudad   	CHAR(50)                     ,
        estado        	INTEGER                      ,
        desc_estado  	CHAR(50)                     ,
        pais_cod      	LIKE afi_domicilio.pais_cod  ,
        pais_desc     	CHAR(40)                     ,
        dom_cod       	LIKE afi_domicilio.dom_cod   ,
        dom_desc      	CHAR(50)                     ,
        marca_envio 	CHAR(1)
    END RECORD

    DEFINE sale   SMALLINT
    DEFINE pos    SMALLINT
    DEFINE igual  CHAR(11)

    LET HOY = TODAY

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Grabar                               CONTROL [C] Salir sin Grabar     " AT 1,1 ATTRIBUTE(BOLD)

        SELECT * 
        INTO   l_afiliados.*
        FROM   afi_solicitud
        WHERE  n_seguro = g_afiliados.n_seguro
        AND    n_folio  = g_parametro1
        AND    tipo_solicitud = g_parametro4

        IF STATUS = NOTFOUND THEN
            ERROR "Solicitud de afiliacion NO existe"
        ELSE
            CALL Valores_afiliados(l_afiliados.*)
            DISPLAY BY NAME g_afiliados.*
        END IF

        LET pos        = 1
        LET sale       = FALSE
        LET igual      = g_afiliados.n_seguro
        LET codpos_ant = NULL

        DECLARE cursor_3 CURSOR FOR
        SELECT a.rowid       ,
               a.calle       ,
               a.numero      ,
               a.depto       ,
               a.calle_ref1  ,
               a.calle_ref2  ,
               a.codpos      ,
               a.colonia     ,
               a.delega      ,
               " "           ,
               a.ciudad      ,
               " "           ,
               a.estado      ,
               " "           ,
               a.pais_cod    ,
               " "           ,
               a.dom_cod     ,
               " "           ,
               a.marca_envio
        FROM   afi_domicilio  a
        WHERE  a.nss            = g_afiliados.n_seguro
        AND    a.n_folio        = g_parametro1
        AND    a.tipo_solicitud = g_parametro4

       FOREACH cursor_3 INTO a_afiliados[pos].*
          IF pos > 10 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
          END IF

          SELECT deleg_desc 
          INTO   a_afiliados[pos].desc_delega 
          FROM   tab_delegacion
	  WHERE  deleg_cod = a_afiliados[pos].delega

	  SELECT ciudad_desc 
          INTO   a_afiliados[pos].desc_ciudad 
          FROM   tab_ciudad
	  WHERE  ciudad_cod = a_afiliados[pos].ciudad

  	  SELECT estad_desc 
          INTO   a_afiliados[pos].desc_estado 
          FROM   tab_estado
	  WHERE  estad_cod = a_afiliados[pos].estado
 
	  SELECT dom_desc 
          INTO   a_afiliados[pos].dom_desc 
          FROM   tab_domicilio
	  WHERE  dom_cod = a_afiliados[pos].dom_cod

  	  SELECT pais_desc
          INTO   a_afiliados[pos].pais_desc
          FROM   tab_pais
	  WHERE  pais_cod = a_afiliados[pos].pais_cod

          LET pos = pos + 1

       END FOREACH

        IF (pos-1) < 1 THEN
           ERROR "NO EXISTEN REGISTROS..."
           SLEEP 3
           ERROR ""
           CLEAR FORM
        END IF

        IF (pos-1) >= 1 THEN
           CALL SET_COUNT (pos-1)
           DISPLAY ARRAY a_afiliados TO scr.*
           ON KEY(INTERRUPT)
              # CALL Inicializa()
              CLEAR FORM
              EXIT DISPLAY
              RETURN
              EXIT PROGRAM

           ON KEY ( CONTROL-M )
              LET pos = ARR_CURR()
              LET g_afiliados.calle       = a_afiliados[pos].calle
              LET g_afiliados.numero      = a_afiliados[pos].numero
              LET g_afiliados.depto       = a_afiliados[pos].depto
              LET g_afiliados.calle_ref1  = a_afiliados[pos].calle_ref1
              LET g_afiliados.calle_ref2  = a_afiliados[pos].calle_ref2 
              LET g_afiliados.codpos      = a_afiliados[pos].codpos
              LET g_afiliados.colonia     = a_afiliados[pos].colonia
              LET g_afiliados.delega      = a_afiliados[pos].delega
              LET g_afiliados.desc_delega = a_afiliados[pos].desc_delega
              LET g_afiliados.ciudad      = a_afiliados[pos].ciudad
              LET g_afiliados.desc_ciudad = a_afiliados[pos].desc_ciudad
              LET g_afiliados.estado      = a_afiliados[pos].estado
              LET g_afiliados.desc_estado = a_afiliados[pos].desc_estado
              LET g_afiliados.dom_cod     = a_afiliados[pos].dom_cod
              LET g_afiliados.dom_desc    = a_afiliados[pos].dom_desc
              LET g_afiliados.pais_cod    = a_afiliados[pos].pais_cod
              LET g_afiliados.pais_desc   = a_afiliados[pos].pais_desc
              LET g_afiliados.marca_envio = a_afiliados[pos].marca_envio

              LET x_afiliados.idrow       = a_afiliados[pos].idrow
              LET x_afiliados.calle       = a_afiliados[pos].calle
              LET x_afiliados.numero      = a_afiliados[pos].numero
              LET x_afiliados.depto       = a_afiliados[pos].depto
              LET x_afiliados.calle_ref1  = a_afiliados[pos].calle_ref1
              LET x_afiliados.calle_ref2  = a_afiliados[pos].calle_ref2 
              LET x_afiliados.codpos      = a_afiliados[pos].codpos
              LET x_afiliados.colonia     = a_afiliados[pos].colonia
              LET x_afiliados.delega      = a_afiliados[pos].delega
              LET x_afiliados.ciudad      = a_afiliados[pos].ciudad
              LET x_afiliados.estado      = a_afiliados[pos].estado
              LET x_afiliados.dom_cod     = a_afiliados[pos].dom_cod
              LET x_afiliados.pais_cod    = a_afiliados[pos].pais_cod
              LET x_afiliados.marca_envio = a_afiliados[pos].marca_envio

              LET codpos_ant = g_afiliados.codpos

	INPUT BY NAME g_afiliados.* WITHOUT DEFAULTS

	BEFORE FIELD n_seguro
		NEXT FIELD calle

        AFTER FIELD calle
               	IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle 
               	END IF

               	IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                    NEXT FIELD calle 
                END IF

	        IF g_afiliados.calle IS NULL OR 
                    g_afiliados.calle =" " THEN
		    ERROR "Calle NO puede ser nulo"
		    NEXT FIELD calle
	        END IF

        AFTER FIELD numero
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle
               END IF

	       IF g_afiliados.numero IS NULL OR
                  g_afiliados.numero=" "  THEN
		  ERROR "Numero NO puede ser nulo"
		  NEXT FIELD numero
	       END IF

        AFTER FIELD calle_ref1
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD numero
               END IF

        AFTER FIELD calle_ref2
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                    NEXT FIELD calle_ref1
               END IF

        AFTER FIELD codpos
               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD calle_ref2 
               END IF
               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                  IF g_afiliados.codpos IS NULL OR 
                     g_afiliados.codpos=" " THEN
                     NEXT FIELD codpos 
                  END IF
               END IF
               IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                  IF g_afiliados.codpos IS NULL OR 
                     g_afiliados.codpos=" " THEN
                     NEXT FIELD codpos 
                  END IF
               END IF
               IF g_afiliados.codpos[1] NOT MATCHES "[0-9]" OR
                  g_afiliados.codpos[2] NOT MATCHES "[0-9]" OR
                  g_afiliados.codpos[3] NOT MATCHES "[0-9]" OR
                  g_afiliados.codpos[4] NOT MATCHES "[0-9]" OR
                  g_afiliados.codpos[5] NOT MATCHES "[0-9]" THEN
                   ERROR "Codigo postal erroneo" SLEEP 2
                   LET g_afiliados.codpos = codpos_ant
                   DISPLAY BY NAME g_afiliados.codpos
                   NEXT FIELD codpos
               END IF
               IF g_afiliados.codpos IS NULL OR 
                  g_afiliados.codpos=" " THEN
                  CALL Despliega_codigo_postal()
                  RETURNING g_afiliados.codpos      ,
                            g_afiliados.colonia     ,
                            g_afiliados.delega      ,
                            g_afiliados.desc_delega ,
                            g_afiliados.ciudad      ,
                            g_afiliados.desc_ciudad ,
                            g_afiliados.estado      ,
                            g_afiliados.desc_estado
                  IF g_afiliados.colonia IS NULL THEN
                     ERROR "Este Codigo Postal no existe en el catalogo"
                     NEXT FIELD codpos
                  END IF
               ELSE
                  IF g_afiliados.codpos <> codpos_ant THEN
                      SELECT "X" FROM tab_codpos
                      WHERE cpos_cod = g_afiliados.codpos
                      IF STATUS = 100 THEN
                          ERROR "CP no existe en catalogo, deja valor NULO para desplegar pantalla de Codigos"
                          NEXT FIELD codpos
                      END IF
                      CALL Despliega_colonias(g_afiliados.codpos)
                      RETURNING g_afiliados.colonia      ,
                                g_afiliados.delega       ,
                                g_afiliados.desc_delega,
                                g_afiliados.ciudad       ,
                                g_afiliados.desc_ciudad,
                                g_afiliados.estado       ,
                                g_afiliados.desc_estado
                  END IF
               END IF
               DISPLAY BY NAME g_afiliados.*

         AFTER FIELD colonia
		  IF g_afiliados.colonia IS NULL OR
			 g_afiliados.colonia MATCHES ' *' THEN
			 ERROR 'Colonia NO puede ser nulo'
			 SLEEP 2
			 ERROR ''
			 NEXT FIELD colonia
		  ELSE
		 	NEXT FIELD pais_cod
        END IF         

        BEFORE FIELD pais_cod
            IF g_afiliados.pais_cod IS NULL THEN
	       LET g_afiliados.pais_cod = "MEX"
	       DISPLAY BY NAME g_afiliados.pais_cod
            END IF

        AFTER FIELD pais_cod
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD dom_cod
            END IF

            IF g_afiliados.pais_cod IS NULL THEN
               CALL Despliega_pais() RETURNING g_afiliados.pais_cod,
                                               g_afiliados.pais_desc
            ELSE
               SELECT pais_desc
               INTO   g_afiliados.pais_desc
               FROM   tab_pais
               WHERE  pais_cod = g_afiliados.pais_cod

               IF STATUS = NOTFOUND THEN
                   ERROR "Pais inexistente"
                   NEXT FIELD pais_cod
               END IF
            END IF

	    DISPLAY BY NAME g_afiliados.pais_cod, g_afiliados.pais_desc

            NEXT FIELD dom_cod

        AFTER FIELD dom_cod
	       IF g_afiliados.dom_cod IS NULL THEN
	          CALL Despliega_tipos_dom() 
                       RETURNING g_afiliados.dom_cod,g_afiliados.dom_desc
	       ELSE
	          SELECT dom_desc 
	          INTO   g_afiliados.dom_desc 
	          FROM   tab_domicilio
	          WHERE  dom_cod = g_afiliados.dom_cod

 	          IF STATUS = NOTFOUND THEN
	             ERROR "Tipo Inexistente"
	             NEXT FIELD dom_cod
	          END IF
	       END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                  NEXT FIELD numero 
               END IF

               IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                     IF g_afiliados.dom_cod IS NULL THEN
                        ERROR "El campo Tipo Domicilio NO puede ser NULO"
                        NEXT FIELD dom_cod
                     END IF
               END IF

 	       DISPLAY BY NAME g_afiliados.dom_desc

        IF g_afiliados.dom_cod = 7 THEN
            NEXT FIELD calle
        ELSE
            NEXT FIELD marca_envio
        END IF

	ON KEY ( INTERRUPT )
             EXIT INPUT

	ON KEY ( ESC )
             IF bandera = 0 THEN
 	        IF g_afiliados.calle IS NULL OR
                   g_afiliados.calle =" " THEN
		   ERROR "Calle NO puede ser nulo"
		   NEXT FIELD calle
	        END IF

	        IF g_afiliados.numero IS NULL OR
                   g_afiliados.numero=" "  THEN
		   ERROR "Numero NO puede ser nulo"
		   NEXT FIELD numero
	        END IF

	        IF g_afiliados.codpos IS NULL OR 
                   g_afiliados.codpos=" " THEN
                   CALL Despliega_codigo_postal()
                   RETURNING g_afiliados.codpos      , 
                             g_afiliados.colonia     ,
                             g_afiliados.delega      ,
                             g_afiliados.desc_delega ,
                             g_afiliados.ciudad      ,
                             g_afiliados.desc_ciudad ,
                             g_afiliados.estado      ,
                             g_afiliados.desc_estado

                   IF g_afiliados.colonia IS NULL THEN
                      ERROR "Este Codigo Postal no existe en el catalogo"
                      NEXT FIELD codpos
                   END IF
	        END IF
	     END IF

             WHILE TRUE
                 CALL pregunta_modifica()
                 IF aux_pausa MATCHES "[SsNn]" THEN
                     EXIT WHILE
                 END IF
             END WHILE 
             EXIT INPUT
        END INPUT

        EXIT DISPLAY

        END DISPLAY

    END IF

END FUNCTION

FUNCTION pregunta()
#p-----------------

    PROMPT "Desea ingresar domicilio alterno? S/N " FOR aux_pausa

END FUNCTION

FUNCTION pregunta_modifica()
#pm-------------------------

    PROMPT "Desea actualizar registro ? S/N " FOR aux_pausa

END FUNCTION

FUNCTION Valores_afiliados(l_afiliados)
#Rva-----------------------------------

    DEFINE l_afiliados RECORD LIKE afi_solicitud.*
    DEFINE 
        aux_paterno CHAR(60),
        aux_materno CHAR(60),
        aux_nombre  CHAR(60),
        aux_fecha   DATE

    LET g_afiliados.n_seguro = l_afiliados.n_seguro
    LET g_afiliados.n_unico  = l_afiliados.n_unico
    LET g_afiliados.n_rfc    = l_afiliados.n_rfc
    LET g_afiliados.paterno  = l_afiliados.paterno
    LET g_afiliados.materno  = l_afiliados.materno
    LET g_afiliados.nombres  = l_afiliados.nombres
    LET g_afiliados.fena     = l_afiliados.fena

END FUNCTION

FUNCTION Despliega_tipos_dom()
#Dtd--------------------------

	DEFINE aux_val		SMALLINT

	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD

	DEFINE x_x		char(100),
	       x_buscar		char(30)
	DEFINE pos		SMALLINT

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "SEPM0018" ATTRIBUTE(BORDER)
	DISPLAY "           T I P O S   D E   D O M I C I L I O S      " AT 2,1 ATTRIBUTE(REVERSE)
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
	      LET x_x = " SELECT dom_cod,dom_desc FROM tab_domicilio ",
	                " WHERE dom_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 1 " CLIPPED
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
	         ERROR "ARCHIVO DE TIPOS..... VACIO"
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

