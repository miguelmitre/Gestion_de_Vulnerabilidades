##########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                       #
#Propietario       => E.F.P.                                             #
#Programa SEPM003  => MANTENIMIENTO A TELEFONOS DE AFILIADOS             #
#Fecha             => 5 DE NOVIEMBRE DE 2000                             #
#Autor             => MAURO MUNIZ CABLLERO                               #
#Sistema           => AFI.                                               #
##########################################################################

DATABASE safre_af

GLOBALS
    DEFINE sw1 smallint
    DEFINE tel ARRAY [8] OF RECORD
        tel_cod    SMALLINT,
        tel_desc   CHAR(10),
        pais_cod   CHAR(3) ,
        pais_desc  CHAR(20),
        cve_lada   CHAR(3),
        telefono   CHAR(60),
        extension  INTEGER,
        idrow      INTEGER
    END RECORD

    DEFINE ACCION    CHAR(1)
    DEFINE enter     CHAR(1)
    DEFINE g_usuario CHAR(8)
    DEFINE HOY       DATE

    DEFINE
        arr1      ,
        arr       ,
        src1      ,
        src       SMALLINT

    DEFINE g_afili RECORD 
        n_seguro         CHAR(11),
        n_unico          CHAR(18),
        n_rfc            CHAR(13),
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        n_folio          DECIMAL(8,0),
        tipo_solicitud   SMALLINT,
        tel_cod          SMALLINT,
        tel_desc         CHAR(10),
        pais_cod         CHAR(3) ,
        pais_desc        CHAR(20),
        cve_lada         CHAR(3),
        telefono         CHAR(60),
        extension        INTEGER
    END RECORD

    DEFINE pate,mate,nome CHAR(50)
    DEFINE x_seguro       CHAR(11)
    DEFINE x_unico        CHAR(18)
    DEFINE x_rfc          CHAR(13)
    DEFINE x_fena         DATE
    DEFINE
        aux_pausa         CHAR(1),
        bandera           SMALLINT,
        g_parametro1      CHAR(11),
        g_parametro2      CHAR(1)      ,
        g_parametro3      SMALLINT     ,
        g_parametro4      DECIMAL(8,0)

    DEFINE l_afiliados RECORD LIKE afi_solicitud.*

    DEFINE long_tel INTEGER 
    DEFINE long_cve SMALLINT
    DEFINE c_long   CHAR(15)
    DEFINE c_cve    CHAR(3)
    DEFINE opc      CHAR(01)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("SEPM003.log")
    CALL inicio()
    CALL proceso_principal()

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

    SELECT n_rfc, 
           n_unico,
           n_seguro,
           paterno, 
           materno, 
           nombres, 
           fena, 
           USER
    INTO   g_afili.n_rfc,
           g_afili.n_unico,
           g_afili.n_seguro,
           g_afili.paterno,
           g_afili.materno,
           g_afili.nombres,
           g_afili.fena,
           g_usuario
    FROM   afi_solicitud
    WHERE  n_folio = g_parametro4
    AND    tipo_solicitud = g_parametro3

    LET g_afili.tipo_solicitud = g_parametro3
    LET g_afili.n_folio        = g_parametro4

END FUNCTION

FUNCTION proceso_principal()
#pp------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPM0031" ATTRIBUTES(BORDER)
    DISPLAY " SEPM003        MANTENIMIENTO SOLICITUDES DE AFILIACION                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           T E L E F O N O S                                   " AT 8,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME g_afili.*

    CASE
        WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E" 
            MENU " TELEFONOS "
                COMMAND "Agrega " "Agrega Telefonos"
                    LET ACCION = "A"
                    CALL Inicializa()
                    CALL Agrega()
		    CLEAR FORM
                    CALL Inicializa()
                COMMAND "Consulta " "Consulta Telefonos"
                    LET ACCION = "C"
                    CALL Inicializa()
                    CALL Consulta()
		    CLEAR FORM
                    CALL Inicializa() 
                COMMAND "Modifica " "Modifica Telefonos"
                    LET ACCION = "M"
                    CALL Inicializa()
                    CALL Modifica()
		    CLEAR FORM
                    CALL Inicializa() 
                COMMAND "Elimina " "Elimina Telefonos"
                    CALL Inicializa()
                    CALL Elimina()
		    CLEAR FORM
                    CALL Inicializa() 
                COMMAND "Salir " "Salir de Programa"
                    EXIT MENU
            END MENU 
        WHEN ACCION = "C" 
            MENU " TELEFONOS "
                COMMAND "Consulta " "Consulta Telefonos"
                    LET ACCION = "C"
                    CALL Inicializa()
                    CALL Consulta()
                    CALL Inicializa() 
                COMMAND "Salir " "Salir de Programa"
                    EXIT MENU
            END MENU 
        END CASE

END FUNCTION

FUNCTION Inicializa()
#I-------------------

    DEFINE i SMALLINT

    FOR i = 1 TO 8
        INITIALIZE tel[i].* TO NULL
    END FOR

END FUNCTION

FUNCTION Agrega()
#A---------------

    DEFINE i         SMALLINT,
           tel_cod1  INTEGER,
           tel_desc1 CHAR(50),
           pos       SMALLINT


    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  "
            AT 1,1 ATTRIBUTE(BOLD)

    LET bandera = 0

    SELECT a.*
    INTO   l_afiliados.*
    FROM   afi_solicitud a
    WHERE  a.n_folio  = g_afili.n_folio
    AND    a.tipo_solicitud = g_afili.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        ERROR "Solicitud de afiliacion no existe"
        RETURN
    ELSE
        DISPLAY BY NAME g_afili.*
    END IF

    IF g_parametro2 = 'A' THEN
        CALL valor_afiliado()
    END IF

    INPUT ARRAY tel FROM scr_1.* 

        BEFORE FIELD tel_cod

            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD tel_cod
            IF tel[arr].tel_cod IS NULL THEN
                CALL llama_ven_tel()
                RETURNING tel[arr].tel_cod,tel[arr].tel_desc
                IF tel[arr].tel_cod = 0 OR tel[arr].tel_cod IS NULL THEN
                    ERROR "Tipo de telefono no existe en catalogo"
                    NEXT FIELD tel_cod
                END IF
            ELSE
                SELECT tel_desc
                INTO   tel[arr].tel_desc
                FROM   tab_telefono
                WHERE  tel_cod = tel[arr].tel_cod

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Tipo de telefono no existe en catalogo"
                    NEXT FIELD tel_cod
                END IF
            END IF
            DISPLAY tel[arr].tel_cod   TO scr_1[src].tel_cod
            DISPLAY tel[arr].tel_desc  TO scr_1[src].tel_desc

        AFTER FIELD pais_cod

            IF tel[arr].pais_cod IS NULL THEN
                LET src = SCR_LINE()
                LET tel[arr].pais_cod = "052"
		DISPLAY tel[arr].pais_cod  TO scr_1[src].pais_cod
                LET tel[arr].pais_desc = "MEXICO"
		DISPLAY tel[arr].pais_desc TO scr_1[src].pais_desc
            ELSE
                SELECT pais_desc
                INTO   tel[arr].pais_desc 
                FROM   tab_lada0
                WHERE  pais_cod = tel[arr].pais_cod

                IF STATUS = NOTFOUND THEN
                    LET tel[arr].pais_desc = NULL
                END IF
            END IF

        AFTER FIELD cve_lada
            LET long_cve = LENGTH(tel[arr].cve_lada)

            FOR i = 1 TO long_cve
                IF tel[arr].cve_lada[i] <> '1' AND
                   tel[arr].cve_lada[i] <> '2' AND
                   tel[arr].cve_lada[i] <> '3' AND
                   tel[arr].cve_lada[i] <> '4' AND
                   tel[arr].cve_lada[i] <> '5' AND
                   tel[arr].cve_lada[i] <> '6' AND
                   tel[arr].cve_lada[i] <> '7' AND
                   tel[arr].cve_lada[i] <> '8' AND
                   tel[arr].cve_lada[i] <> '9' AND
                   tel[arr].cve_lada[i] <> '0' THEN
                    ERROR "Clave LADA solo contiene digitos"
                    NEXT FIELD cve_lada
                END IF
            END FOR

            DISPLAY tel[arr].cve_lada TO scr_1[src].cve_lada

        AFTER FIELD telefono
            IF tel[arr].telefono IS NULL THEN
                ERROR "Telefono NO puede ser NULO"
                NEXT FIELD telefono
            END IF

            LET long_tel = LENGTH(tel[arr].telefono)

            IF tel[arr].tel_cod <> 7 THEN
                IF long_tel < 5 OR long_tel > 14 THEN
                    ERROR "Numero de telefono invalido"
                    SLEEP 3
                    NEXT FIELD telefono
                END IF
            END IF

            LET c_long = tel[arr].telefono

            IF tel[arr].tel_cod <> 7 THEN
                FOR i = 1 TO long_tel
                    IF c_long[i] <> '1' AND
                       c_long[i] <> '2' AND
                       c_long[i] <> '3' AND
                       c_long[i] <> '4' AND
                       c_long[i] <> '5' AND
                       c_long[i] <> '6' AND
                       c_long[i] <> '7' AND
                       c_long[i] <> '8' AND
                       c_long[i] <> '9' AND
                       c_long[i] <> '0' THEN
                        ERROR "Numero de telefono solo contiene digitos"
                        NEXT FIELD telefono
                    END IF
                END FOR
            END IF

        AFTER FIELD extension
        AFTER FIELD idrow

      ON KEY ( ESC )
          FOR i = 1 TO 8
             IF tel[i].telefono IS NOT NULL THEN
             INSERT INTO afi_telefono 
             VALUES (g_afili.n_seguro,
                     g_afili.n_folio,
                     g_afili.tipo_solicitud,
                     tel[i].pais_cod,
                     tel[i].cve_lada,
                     tel[i].extension,
                     tel[i].telefono,
                     tel[i].tel_cod,
                     g_usuario,
                     HOY)
             END IF
         END FOR

         ---CALL inserta()
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

    DELETE
    FROM   safre_tmp:tmp_afiliado
    WHERE  n_folio = g_parametro4
    AND    tipo_solicitud = g_parametro3

END FUNCTION 

FUNCTION Consulta()
#c--------- --------

    DEFINE
        i   SMALLINT,
        pos SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-C ] Salir "
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_c CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_c INTO tel[pos].*
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
        DISPLAY ARRAY tel TO scr_1.*

        ON KEY(INTERRUPT)
            CALL Inicializa()
            EXIT DISPLAY 

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE
        i   SMALLINT,
        pos SMALLINT

    DISPLAY "" at 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  "
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_m CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_m INTO tel[pos].*
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
    
        INPUT ARRAY tel WITHOUT DEFAULTS FROM scr_1.*  

        BEFORE FIELD tel_cod
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD tel_cod
            IF tel[arr].tel_cod IS NULL THEN
                CALL Despliega_tipos_tel()
                RETURNING g_afili.tel_cod,g_afili.tel_desc
            ELSE  
                SELECT tel_desc
                INTO   tel[arr].tel_desc
                FROM   tab_telefono
                WHERE  tel_cod = tel[arr].tel_cod

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "Tipo de telefono no existe en catalogo"
                    NEXT FIELD tel_cod
                END IF
            END IF

            DISPLAY tel[arr].tel_cod TO scr_1[src].tel_cod
            DISPLAY tel[arr].tel_desc TO scr_1[src].tel_desc

        AFTER FIELD pais_cod
            IF tel[arr].pais_cod IS NULL THEN
                LET tel[arr].pais_cod = "052"
                LET tel[arr].pais_desc = "MEXICO"
            ELSE
                SELECT pais_desc
                INTO   tel[arr].pais_desc 
                FROM   tab_lada0
                WHERE  pais_cod = tel[arr].pais_cod

                IF STATUS = NOTFOUND THEN
                    LET tel[arr].pais_desc = NULL
                END IF
            END IF

            DISPLAY tel[arr].pais_cod TO scr_1[src].pais_cod
            DISPLAY tel[arr].pais_desc TO scr_1[src].pais_desc
 
            NEXT FIELD telefono

        BEFORE FIELD telefono
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD telefono
            IF tel[arr].telefono IS NULL THEN
                ERROR "Tipo de telefono NO puede ser NULO"
                NEXT FIELD telefono
            END IF

            NEXT FIELD extension

        BEFORE FIELD extension
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        ON KEY ( ESC )
            FOR i = 1 TO 8
                IF tel[i].telefono IS NOT NULL THEN
                    UPDATE afi_telefono
                    SET    pais_cod   = tel[i].pais_cod,
                           extension  = tel[i].extension,
                           telefono   = tel[i].telefono,
                           tel_cod    = tel[i].tel_cod,
                           usuario    = g_usuario,
                           factualiza = HOY
                    WHERE  nss        = g_afili.n_seguro
                    AND    n_folio    = g_afili.n_folio
                    AND    tipo_solicitud = g_afili.tipo_solicitud
                    AND    rowid      = tel[i].idrow
                END IF
            END FOR

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
        i   SMALLINT,
        pos SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-C ] Salir     [ Ctrl-B ] Eliminar  "
            AT 1,1 ATTRIBUTE(BOLD)      

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_e CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_e INTO tel[pos].*
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
        DISPLAY ARRAY tel TO scr_1.*

        ON KEY ( CONTROL-b )
            LET pos = ARR_CURR()

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
                DELETE
                FROM   afi_telefono
                WHERE  nss      = g_afili.n_seguro
                AND    n_folio  = g_afili.n_folio
                AND    tipo_solicitud = tipo_solicitud
                AND    rowid    = tel[pos].idrow

                ERROR "REGISTRO ELIMINADO" SLEEP 1 ERROR ""
            END IF

            SLEEP 2 ERROR ""
            CALL Consulta()
            EXIT DISPLAY 

        ON KEY(INTERRUPT)
            CALL Inicializa()
            EXIT DISPLAY 

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Pregunta()
#p-----------------

    PROMPT "Desea eliminar el registro [S/N] ?" FOR aux_pausa

END FUNCTION

FUNCTION llama_ven_tel()
#Dtd--------------------------

    DEFINE
        aux_val SMALLINT,
        pos     SMALLINT

    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
    END RECORD

    DEFINE
        x_x      CHAR(100),
        x_buscar CHAR(30)

    OPEN WINDOW vent_1 AT 05,12 WITH FORM "SEPM00181" ATTRIBUTE(BORDER)
    DISPLAY "           T I P O S   D E   T E L E F O N O S        " AT 2,1 ATTRIBUTE(REVERSE)

        DECLARE cur_tel1 CURSOR FOR
	        SELECT tel_cod, tel_desc FROM tab_telefono
	        order by 1
                LET pos = 1
        FOREACH cur_tel1 INTO l_reg[pos].*

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

{
        ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
}
        ON KEY ( ESC )
            LET pos = ARR_CURR()
            EXIT DISPLAY

        ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

        END DISPLAY
    CLOSE WINDOW vent_1

    RETURN l_reg[pos].codigo,l_reg[pos].descripcion

END FUNCTION

FUNCTION Despliega_tipos_tel()
#Dtd--------------------------

    DEFINE
        aux_val SMALLINT,
        pos     SMALLINT

    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
    END RECORD

    DEFINE
        x_x      CHAR(100),
        x_buscar CHAR(30)

    OPEN WINDOW vent_1 AT 05,12 WITH FORM "SEPM0018" ATTRIBUTE(BORDER)
    DISPLAY "           T I P O S   D E   T E L E F O N O S        " AT 2,1 ATTRIBUTE(REVERSE)

        DECLARE cur_g3 CURSOR FOR
	        SELECT tel_cod, tel_desc FROM tab_telefono
	        order by 1
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

{
        ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
}
        ON KEY ( ESC )
            LET pos = ARR_CURR()
            EXIT DISPLAY

        ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

        END DISPLAY
    CLOSE WINDOW vent_1

    RETURN l_reg[pos].codigo,l_reg[pos].descripcion

END FUNCTION

