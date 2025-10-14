##########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                       #
#Propietario       => EFP                                                #
#Programa AFIM015  => MANTENIMIENTO A PATRONES DE AFILIADOS              #
#Autor             => MAURO MUNIZ CABALLERO                              #
#Fecha             => 12 ENERO DE 2001                                   #
#Sistema           => AFI.                                               #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_afili RECORD
        n_seguro         CHAR(11),
        n_rfc            CHAR(13),
        n_unico          CHAR(18),
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        n_folio          DECIMAL(10,0),
        tipo_solicitud   SMALLINT,
        idrow            INTEGER,
        reg_patronal     CHAR(11),
        reg_fed_contrib  CHAR(13),
        razon_social     CHAR(50),
        telefono         CHAR(15),
        calle            CHAR(40),
        numero           CHAR(20),
        codpos           CHAR(5) , 
        colonia          CHAR(60),
        delega           INTEGER,
        desc_delega      CHAR(50),
        ciudad           SMALLINT,
        desc_ciudad      CHAR(50),
        estado           SMALLINT,
        desc_estado      CHAR(50)
    END RECORD

    DEFINE 
        ACCION         CHAR(1),
        tipo_solucion  CHAR(1),
        enter          CHAR(1),
        aux_pausa      CHAR(1),
        g_usuario      CHAR(8),
        bandera        SMALLINT,
        g_parametro1   CHAR(11),
        g_parametro4   CHAR(1) ,
        g_parametro3   SMALLINT,
        g_parametro2   DECIMAL(10,0)

    DEFINE l_afiliados RECORD LIKE afi_mae_afiliado.*

    DEFINE c_pat ARRAY[6] OF RECORD
        idrow            INTEGER,
        reg_patronal     CHAR(11),
        reg_fed_contrib  CHAR(13),
        razon_social     CHAR(50),
        telefono         CHAR(15),
        calle            CHAR(40),
        numero           CHAR(20),
        codpos           CHAR(5) , 
        colonia          CHAR(60),
        delega           INTEGER,
        desc_delega      CHAR(50),
        ciudad           SMALLINT,
        desc_ciudad      CHAR(50),
        estado           SMALLINT,
        desc_estado      CHAR(50)
    END RECORD

    DEFINE
        HOY    DATE

    DEFINE
        arr    ,
        src    SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("AFIM015.log")
    CALL inicio()   #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET g_parametro1 = ARG_VAL(1)
    LET g_parametro2 = ARG_VAL(2)
    LET g_parametro3 = ARG_VAL(3)
    LET g_parametro4 = ARG_VAL(4)

    LET ACCION = g_parametro4

    LET HOY = TODAY

    IF g_afili.n_seguro = " " THEN
        ERROR "ESTE PROGRAMA SOLO PUEDE SER EJECUTADO DESDE SU MODULO PRINCIPAL"
        SLEEP 3
        EXIT PROGRAM
    END IF

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
    FROM   afi_mae_afiliado a
    WHERE  a.n_folio        = g_parametro2
    AND    a.tipo_solicitud = g_parametro3

    LET g_afili.tipo_solicitud = g_parametro3
    LET g_afili.n_folio        = g_parametro2

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0051" ATTRIBUTE(BORDER)
    DISPLAY " AFIM015        MANTENIMIENTO A SOLICITUDES DE AFILIACION                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                            P A T R O N E S                                    " AT 8,1 ATTRIBUTE(REVERSE) 

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME g_afili.*

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
                    CALL Inicializa()
                COMMAND "Salir " "Salir de Programa"
               EXIT MENU
           END MENU
    END CASE

    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()
#iniz---------------

    DEFINE i SMALLINT

    FOR i = 1 TO 6
        INITIALIZE c_pat[i].* TO NULL
    END FOR

    INITIALIZE g_afili.reg_patronal         TO NULL
    DISPLAY BY NAME g_afili.reg_patronal
    INITIALIZE g_afili.razon_social         TO NULL
    DISPLAY BY NAME g_afili.razon_social
    INITIALIZE g_afili.reg_fed_contrib      TO NULL
    DISPLAY BY NAME g_afili.reg_fed_contrib
    INITIALIZE g_afili.telefono             TO NULL
    DISPLAY BY NAME g_afili.telefono
    INITIALIZE g_afili.calle           TO NULL
    DISPLAY BY NAME g_afili.calle
    INITIALIZE g_afili.numero          TO NULL
    DISPLAY BY NAME g_afili.numero
    INITIALIZE g_afili.codpos          TO NULL
    DISPLAY BY NAME g_afili.codpos
    INITIALIZE g_afili.colonia         TO NULL
    DISPLAY BY NAME g_afili.colonia
    INITIALIZE g_afili.delega          TO NULL
    DISPLAY BY NAME g_afili.delega
    INITIALIZE g_afili.desc_delega     TO NULL
    DISPLAY BY NAME g_afili.desc_delega
    INITIALIZE g_afili.ciudad          TO NULL
    DISPLAY BY NAME g_afili.ciudad
    INITIALIZE g_afili.desc_ciudad     TO NULL
    DISPLAY BY NAME g_afili.desc_ciudad
    INITIALIZE g_afili.estado          TO NULL
    DISPLAY BY NAME g_afili.estado
    INITIALIZE g_afili.desc_estado     TO NULL
    DISPLAY BY NAME g_afili.desc_estado

END FUNCTION

FUNCTION Agrega()
#a---------------

    DEFINE i SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " 
            AT 1,1 ATTRIBUTE(BOLD)

    LET bandera = 0

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           USER
    INTO   g_afili.n_seguro,
           g_afili.n_unico ,
           g_afili.n_rfc   ,
           g_afili.paterno ,
           g_afili.materno ,
           g_afili.nombres ,
           g_afili.fena    ,
           g_usuario
    FROM   afi_mae_afiliado af
    WHERE  af.n_folio        = g_parametro2
    AND    af.tipo_solicitud = g_parametro3

    IF STATUS = NOTFOUND THEN
        ERROR "Solicitud de afiliacion no existe "
        RETURN
    ELSE
        DISPLAY BY NAME g_afili.*
    END IF

    INPUT BY NAME g_afili.* WITHOUT DEFAULTS
        BEFORE FIELD n_seguro
            NEXT FIELD reg_patronal

        AFTER FIELD reg_patronal
            IF g_afili.reg_patronal IS NULL THEN
                ERROR "Registro Patronal NO puede ser NULO"
                NEXT FIELD reg_patronal
            END IF

            SELECT tab_patron.razon_social
            INTO   g_afili.razon_social
            FROM   afi_patron, tab_patron
            WHERE  afi_patron.n_folio         = g_afili.n_folio
            AND    afi_patron.tipo_solicitud  = g_afili.tipo_solicitud
            AND    afi_patron.reg_patronal    = g_afili.reg_patronal
            AND    afi_patron.reg_patronal    = tab_patron.reg_patronal
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY BY NAME g_afili.razon_social
   ERROR "El Registro Patronal ya existe para esta solicitud de afiliacion"
                NEXT FIELD reg_patronal
            END IF

            SELECT P.reg_patronal,
                   P.razon_social,
                   P.reg_fed_contrib,
                   P.telefono    ,
                   P.calle       ,
                   P.numero      ,
                   P.codpos      ,
                   P.colonia     ,
                   P.delega      ,
                   B.deleg_desc  ,
                   P.ciudad      ,
                   C.ciudad_desc ,
                   P.estado      ,
                   D.estad_desc  
            INTO   g_afili.reg_patronal,
                   g_afili.razon_social,
                   g_afili.reg_fed_contrib,
                   g_afili.telefono    ,
                   g_afili.calle       ,
                   g_afili.numero      ,
                   g_afili.codpos      ,
                   g_afili.colonia     ,
                   g_afili.delega      ,
                   g_afili.desc_delega  ,
                   g_afili.ciudad      ,
                   g_afili.desc_ciudad ,
                   g_afili.estado      ,
                   g_afili.desc_estado  
            FROM   --afi_patron A, 
                   tab_patron P,
            OUTER  tab_delegacion b,
            OUTER  tab_ciudad     c,
            OUTER  tab_estado     d
            WHERE  P.reg_patronal   = g_afili.reg_patronal
            AND    B.deleg_cod      = P.delega
            AND    C.ciudad_cod     = P.ciudad
            AND    D.estad_cod      = P.estado

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY BY NAME g_afili.*
                NEXT FIELD reg_patronal
            END IF       

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD reg_patronal
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD reg_patronal
            END IF

        AFTER FIELD razon_social
            IF g_afili.razon_social IS NULL THEN
                ERROR "Razon social NO puede ser NULA"
                NEXT FIELD razon_social
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD reg_patronal
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD reg_patronal
            END IF

            NEXT FIELD reg_fed_contrib

        AFTER FIELD reg_fed_contrib
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD reg_patronal
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD razon_social
            END IF

            NEXT FIELD telefono

        AFTER FIELD telefono
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD razon_social
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD reg_fed_contrib
            END IF

            NEXT FIELD calle

        AFTER FIELD calle
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD reg_fed_contrib
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD telefono
            END IF

            NEXT FIELD numero

        AFTER FIELD codpos
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD calle
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD numero
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                IF g_afili.codpos IS NULL OR
                   g_afili.codpos=" " THEN
                    NEXT FIELD codpos
                ELSE
                    NEXT FIELD colonia
                END IF
            END IF

            IF g_afili.codpos[1] NOT MATCHES "[0-9]" OR
               g_afili.codpos[2] NOT MATCHES "[0-9]" OR
               g_afili.codpos[3] NOT MATCHES "[0-9]" OR
               g_afili.codpos[4] NOT MATCHES "[0-9]" OR
               g_afili.codpos[5] NOT MATCHES "[0-9]" THEN
                ERROR "Codigo postal erroneo" 
                SLEEP 2
                NEXT FIELD codpos
            END IF

            IF g_afili.codpos IS NULL OR
               g_afili.codpos=" " THEN
                CALL Despliega_codigo_postal()
                RETURNING g_afili.codpos      ,
                          g_afili.colonia     ,
                          g_afili.delega      ,
                          g_afili.desc_delega ,
                          g_afili.ciudad      ,
                          g_afili.desc_ciudad ,
                          g_afili.estado      ,
                          g_afili.desc_estado

                IF g_afili.colonia IS NULL THEN
                    ERROR "Este Codigo Postal no existe en el catalogo"
                    NEXT FIELD codpos
                END IF
            ELSE
                SELECT "X"
                FROM   tab_codpos
                WHERE  cpos_cod = g_afili.codpos

                IF STATUS = 100 THEN
                    ERROR "CP no existe en catalogo, deja valor NULO",
                          " para desplegar pantalla de Codigos Postales"
                    NEXT FIELD codpos
                END IF

                CALL Despliega_colonias(g_afili.codpos)
                RETURNING g_afili.colonia      ,
                          g_afili.delega       ,
                          g_afili.desc_delega,
                          g_afili.ciudad       ,
                          g_afili.desc_ciudad,
                          g_afili.estado       ,
                          g_afili.desc_estado
            END IF

            DISPLAY BY NAME g_afili.*

            AFTER FIELD colonia
                IF g_afili.colonia IS NULL OR
                   g_afili.colonia MATCHES ' *' THEN
                    ERROR 'Colonia NO puede ser nulo'
                    SLEEP 2
                    ERROR ''
                    NEXT FIELD colonia
                ELSE
                    NEXT FIELD reg_patronal
                END IF

        ON KEY ( ESC )
            SELECT "X" 
            FROM   afi_patron
            WHERE  n_folio         = g_afili.n_folio
            AND    tipo_solicitud  = g_afili.tipo_solicitud
            AND    reg_patronal    = g_afili.reg_patronal
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "El Registro Patronal ya existe para esta solicitud de afiliacion"
               NEXT FIELD reg_patronal
            END IF

            IF g_afili.reg_patronal IS NOT NULL AND
               g_afili.razon_social IS NOT NULL THEN
                SELECT "X"
                FROM   tab_patron
                WHERE  (reg_patronal    = g_afili.reg_patronal
                OR      reg_fed_contrib = g_afili.reg_fed_contrib)
                GROUP BY 1

                IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO tab_patron 
                    VALUES (g_afili.reg_patronal,
                            ''                  , --g_afili.rfc_patron       
                            ''                  , --g_afili.no_docto         
                            ''                  , --g_afili.periodo_pago     
                            g_afili.razon_social,
                            g_afili.calle       , --g_afili.calle            
                            g_afili.numero      , --g_afili.numero           
                            g_afili.colonia     , --g_afili.colonia          
                            g_afili.delega      , --g_afili.delega           
                            g_afili.ciudad      , --g_afili.ciudad           
                            g_afili.estado      , --g_afili.estado           
                            g_afili.codpos      , --g_afili.codpos           
                            g_afili.telefono    , --g_afili.telefono         
                            ''                  , --g_afili.delegacion_imss  
                            ''                  , --g_afili.ctividad_eco   
                            ''                  , --g_afili.tipo_cotizacion 
                            ''                  , --g_afili.no_cotizantes   
                            ''                  , --g_afili.no_afilados     
                            ''                  ,--g_afili.fecha_notafore
                            'M'                 ,--g_afili.tipo_patron
                            g_usuario  
                           )
                END IF

                INSERT INTO afi_patron 
                VALUES (g_afili.n_seguro,
                        g_afili.n_folio,
                        g_afili.tipo_solicitud,
                        g_afili.reg_patronal,
                        '',
                        '',
                        '',
                        '',
                        '',
                        '',
                        g_usuario)
            END IF

            EXIT INPUT

        ON KEY ( INTERRUPT )
            CALL Inicializa()
            EXIT INPUT

      END INPUT

END FUNCTION

FUNCTION Consulta()
#c-----------------

    DEFINE
        x_y ,
        i   SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Ctrl-C ] Para salir de la consulta" AT 1,1 ATTRIBUTE(BOLD)

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           USER
    INTO   g_afili.n_seguro,
           g_afili.n_unico ,
           g_afili.n_rfc   ,
           g_afili.paterno ,
           g_afili.materno ,
           g_afili.nombres ,
           g_afili.fena    ,
           g_usuario
    FROM   afi_mae_afiliado af
    WHERE  af.n_folio        = g_afili.n_folio
    AND    af.tipo_solicitud = g_afili.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        ERROR "Solicitud de Afiliacion NO existe"
    ELSE
        DISPLAY BY NAME g_afili.*
    END IF

    LET x_y = 1

    DECLARE cur_c CURSOR FOR
    SELECT A.rowid,
           A.reg_patronal,
           P.razon_social,
           A.reg_fed_contrib,
           P.telefono    ,
           P.calle       ,
           P.numero      ,
           P.codpos      ,
           P.colonia     ,
           P.delega      ,
           B.deleg_desc  ,
           P.ciudad      ,
           C.ciudad_desc ,
           P.estado      ,
           D.estad_desc  
    FROM   afi_patron A, 
           tab_patron P,
    OUTER (tab_delegacion b,
           tab_ciudad     c,
           tab_estado     d)
    WHERE  P.reg_patronal   = A.reg_patronal
    AND    A.n_folio        = g_afili.n_folio
    AND    A.tipo_solicitud = g_afili.tipo_solicitud
    AND    B.deleg_cod      = P.delega
    AND    C.ciudad_cod     = P.ciudad
    AND    D.estad_cod      = P.estado
    ORDER BY 1

    FOREACH cur_c INTO c_pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH

    IF (x_y-1) < 1 THEN
        ERROR "No existen registros patronales asociados"
        SLEEP 2
        ERROR ""
    END IF

    CALL SET_COUNT(x_y-1)
    IF (x_y-1) >= 1 THEN
        DISPLAY array c_pat TO scr_1.*  

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
        END DISPLAY
    END IF

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE 
        i      ,
        x_y    SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " AT 1,1 ATTRIBUTE(BOLD)

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           USER
    INTO   g_afili.n_seguro,
           g_afili.n_unico ,
           g_afili.n_rfc   ,
           g_afili.paterno ,
           g_afili.materno ,
           g_afili.nombres ,
           g_afili.fena    ,
           g_usuario
    FROM   afi_mae_afiliado af
    WHERE  af.n_folio        = g_afili.n_folio
    AND    af.tipo_solicitud = g_afili.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        ERROR "Solicitud de Afiliacion NO existe"
    ELSE
        DISPLAY BY NAME g_afili.*
    END IF

    LET x_y = 1

    DECLARE cur_m CURSOR FOR
    SELECT A.rowid,
           A.reg_patronal,
           P.razon_social,
           A.reg_fed_contrib,
           P.telefono    ,
           P.calle       ,
           P.numero      ,
           P.codpos      ,
           P.colonia     ,
           P.delega      ,
           B.deleg_desc  ,
           P.ciudad      ,
           C.ciudad_desc ,
           P.estado      ,
           D.estad_desc  
    FROM   afi_patron A, 
           tab_patron P,
    OUTER (tab_delegacion b,
           tab_ciudad     c,
           tab_estado     d)
    WHERE  P.reg_patronal   = A.reg_patronal
    AND    A.n_folio        = g_afili.n_folio
    AND    A.tipo_solicitud = g_afili.tipo_solicitud
    AND    B.deleg_cod      = P.delega
    AND    C.ciudad_cod     = P.ciudad
    AND    D.estad_cod      = P.estado
    ORDER BY 1

    FOREACH cur_m INTO c_pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH

    IF (x_y-1) < 1 THEN
        ERROR "No existen registros patronales asociados"
        SLEEP 2
        ERROR ""
    END IF

    CALL SET_COUNT(x_y-1)
    IF (x_y-1) >= 1 THEN
        DISPLAY array c_pat TO scr_1.*  

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
        END DISPLAY
    END IF

END FUNCTION

FUNCTION Elimina()
#e----------------

    DEFINE
        i        ,
        x_y      ,
        pos      ,
        nro_reg  SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ELIMINAR " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Ctrl-b ] Elimina          [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTE(BOLD)

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           USER
    INTO   g_afili.n_seguro,
           g_afili.n_unico ,
           g_afili.n_rfc   ,
           g_afili.paterno ,
           g_afili.materno ,
           g_afili.nombres ,
           g_afili.fena    ,
           g_usuario
    FROM   afi_mae_afiliado af
    WHERE  af.n_folio        = g_afili.n_folio
    AND    af.tipo_solicitud = g_afili.tipo_solicitud

    IF STATUS = NOTFOUND THEN
        ERROR "Solicitud de Afiliacion NO existe"
    ELSE
        DISPLAY BY NAME g_afili.*
    END IF

    LET x_y = 1

    DECLARE cur_e CURSOR FOR
    SELECT A.rowid,
           A.reg_patronal,
           P.razon_social,
           A.reg_fed_contrib,
           P.telefono    ,
           P.calle       ,
           P.numero      ,
           P.codpos      ,
           P.colonia     ,
           P.delega      ,
           B.deleg_desc  ,
           P.ciudad      ,
           C.ciudad_desc ,
           P.estado      ,
           D.estad_desc  
    FROM   afi_patron A, 
           tab_patron P,
    OUTER (tab_delegacion b,
           tab_ciudad     c,
           tab_estado     d)
    WHERE  P.reg_patronal   = A.reg_patronal
    AND    A.n_folio        = g_afili.n_folio
    AND    A.tipo_solicitud = g_afili.tipo_solicitud
    AND    B.deleg_cod      = P.delega
    AND    C.ciudad_cod     = P.ciudad
    AND    D.estad_cod      = P.estado
    ORDER BY 1

    FOREACH cur_e INTO c_pat[x_y].*
        LET x_y = x_y + 1
    END FOREACH

    IF (x_y-1) < 1 THEN
        ERROR "No existen registros patronales asociados"
        SLEEP 2
        ERROR ""
    END IF

    CALL SET_COUNT(x_y-1)
    IF (x_y-1) >= 1 THEN
        DISPLAY array c_pat TO scr_1.*  

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
                FROM   afi_patron
                WHERE  n_folio        = g_afili.n_folio
                AND    tipo_solicitud = g_afili.tipo_solicitud
                AND    reg_patronal   = c_pat[pos].reg_patronal
                AND    rowid          = c_pat[pos].idrow

                ERROR "REGISTRO ELIMINADO"
            END IF

            SLEEP 2
            ERROR ""
            CALL Consulta()
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            EXIT DISPLAY

        END DISPLAY

    END IF

END FUNCTION

FUNCTION Pregunta()
#p-----------------

    PROMPT "Desea eliminar el registro [S/N] ?" FOR aux_pausa

END FUNCTION

