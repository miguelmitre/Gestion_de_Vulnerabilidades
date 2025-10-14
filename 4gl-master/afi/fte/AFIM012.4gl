#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIM012  => MANTENIMIENTO DE DIRECCIONES AFILIADOS                #
#Fecha             => 6 DE DICIEMBRE DE 2000                                #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha Actualiza   => 31 DE ENERO DE 2004 MARCA DOMICILIO MODIFICADO        #
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

    DEFINE g_afiliados RECORD
        n_seguro    LIKE afi_mae_afiliado.n_seguro,
        n_rfc       LIKE afi_mae_afiliado.n_rfc   ,
        n_unico     LIKE afi_mae_afiliado.n_unico ,
        paterno     LIKE afi_mae_afiliado.paterno ,
        materno     LIKE afi_mae_afiliado.materno ,
        nombres     LIKE afi_mae_afiliado.nombres ,
        fena        LIKE afi_mae_afiliado.fena    ,
        n_folio     LIKE afi_mae_afiliado.n_folio ,
        tipo_solicitud SMALLINT                ,
        idrow       INTEGER                    ,
        calle       LIKE afi_domicilio.calle   ,
        numero      LIKE afi_domicilio.numero  ,
        depto       LIKE afi_domicilio.depto   ,
        calle_ref1  LIKE afi_domicilio.calle_ref1,
        calle_ref2  LIKE afi_domicilio.calle_ref2,
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

    DEFINE
        g_delegac_cod     INTEGER  ,
        g_delegac_desc    CHAR(50) ,
        g_ciudad_cod      INTEGER  ,
        g_ciudad_desc     CHAR(50) ,
        g_estado_cod      INTEGER  ,
        g_estado_desc     CHAR(50) ,
        ACCION            CHAR(1)  ,
        comm              CHAR(300),
        comm_arg          CHAR(300),
        g_usuario         CHAR(8)  ,
        g_parametro2      INTEGER  ,
        g_parametro3      SMALLINT ,
        g_parametro4      CHAR(1)  ,
        vcont             INTEGER  ,
        vtotal            SMALLINT

    DEFINE
        vidrow     INTEGER ,
        vsexo      SMALLINT,
        vedon      SMALLINT,
        vnac       CHAR(3) ,
        vtip       CHAR(1) ,
        f_reclama  INTEGER ,
        id_reclama CHAR(1) ,
        reclamo    SMALLINT,
        llama_val  SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("AFIM012.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET g_afiliados.n_seguro = ARG_VAL(1) #NSS
    LET g_parametro2         = ARG_VAL(2) #NUMERO DE FOLIO
    LET g_parametro3         = ARG_VAL(3) #TIPO DE SOLICITUD
    LET g_parametro4         = ARG_VAL(4) #ACCION A ESCOJER
    LET reclamo              = ARG_VAL(5)
    LET llama_val            = ARG_VAL(6)
    LET f_reclama            = ARG_VAL(7)
    LET id_reclama           = ARG_VAL(8)

    LET g_afiliados.n_folio        = g_parametro2
    LET g_afiliados.tipo_solicitud = g_parametro3

    IF f_reclama = 0 THEN
        LET f_reclama = NULL
    END IF

    IF id_reclama = 0 THEN
       LET id_reclama = NULL
    END IF

    LET ACCION = g_parametro4
    LET HOY    = TODAY
    LET vtotal = 0

    SELECT af.n_seguro,
           af.n_unico ,
           af.n_rfc   ,
           af.paterno ,
           af.materno ,
           af.nombres ,
           af.fena    ,
           af.n_unico ,
           af.sexo    ,
           af.estadon ,
           af.nacionalidad,
           af.tip_prob,
           USER
    INTO   g_afiliados.n_seguro,
           g_afiliados.n_unico ,
           g_afiliados.n_rfc   ,
           g_afiliados.paterno ,
           g_afiliados.materno ,
           g_afiliados.nombres ,
           g_afiliados.fena    ,
           g_afiliados.n_unico ,
           vsexo               ,
           vedon               ,
           vnac                ,
           vtip                ,
           g_usuario
    FROM   afi_mae_afiliado af
    WHERE  af.n_folio        = g_afiliados.n_folio
    AND    af.tipo_solicitud = g_afiliados.tipo_solicitud

    LET comm_arg = g_afiliados.n_seguro," ",
                   g_afiliados.n_folio," ",
                   g_afiliados.tipo_solicitud," ",
                   ACCION," ",
                   reclamo, " ",
                   llama_val, " ",
                   f_reclama, " ",
                   id_reclama CLIPPED

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_3 AT 2,2 WITH FORM "AFIM0121" ATTRIBUTE(BORDER)
    DISPLAY " AFIM012           MANTENIMIENTO MAESTRO DE AFILIADOS                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                              DOMICILIO                                        " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME g_afiliados.* 

    CASE
        WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E"
            MENU " DOMICILIOS "
                COMMAND "Agrega " "Agrega Domicilios"
                    LET ACCION = "A"
                    CALL Inicializa()
                    CALL Agrega()
                    CALL Inicializa()
                COMMAND "Consulta " "Consulta Domicilios"
                    LET ACCION = "C"
                    CALL Inicializa()
                    CALL Consulta()
                    CALL Inicializa() 
                COMMAND "Modifica " "Modifica Domicilios"
                    LET ACCION = "M"
                    CALL Inicializa()
                    CALL Modifica()
                    CALL Inicializa() 
              --COMMAND "Elimina " "Elimina Domicilios"
                  --CALL Inicializa()
                  --CALL Elimina()
                  --CALL Inicializa() 
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
    INITIALIZE g_afiliados.dom_cod         TO NULL
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

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Grabar   CONTROL[T] Telefonos        CONTROL [C] Salir sin Grabar     " AT 1,1 ATTRIBUTE(CYAN)

    ---DISPLAY " CONTROL : [T] Telefonos   [B] Beneficiarios   [V] Patrones   [N] Siefores     " AT 2,1 ATTRIBUTE(CYAN)

    LET HOY     = TODAY
    LET bandera = 0

    DISPLAY BY NAME g_afiliados.*

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
            ERROR "Codigo postal erroneo"
            SLEEP 2

            NEXT FIELD codpos
        END IF

        IF g_afiliados.codpos IS NULL OR
           g_afiliados.codpos=" " THEN
            CALL Despliega_codigo_postal()
            RETURNING g_afiliados.codpos     ,
                      g_afiliados.colonia    ,
                      g_afiliados.delega     ,
                      g_afiliados.desc_delega,
                      g_afiliados.ciudad     ,
                      g_afiliados.desc_ciudad,
                      g_afiliados.estado     ,
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
                 RETURNING g_afiliados.colonia     ,
                           g_afiliados.delega      ,
                           g_afiliados.desc_delega ,
                           g_afiliados.ciudad      ,
                           g_afiliados.desc_ciudad ,
                           g_afiliados.estado      ,
                           g_afiliados.desc_estado
        END IF

        DISPLAY BY NAME g_afiliados.*

        NEXT FIELD colonia

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
        LET g_afiliados.dom_cod = 1

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

        NEXT FIELD marca_envio

    AFTER FIELD marca_envio
        IF g_afiliados.marca_envio <> 'X' OR
           g_afiliados.marca_envio <> '' THEN
            ERROR "Marca envio solo debe ser 'X' o nulo"
            LET g_afiliados.marca_envio = NULL

            DISPLAY BY NAME g_afiliados.marca_envio

            NEXT FIELD marca_envio
        END IF

    ON KEY ( INTERRUPT )
        EXIT INPUT

    ON KEY ( CONTROL-T )
        LET comm = "fglgo AFIM013 ",comm_arg CLIPPED
        RUN comm

   {
    ON KEY ( CONTROL-B )
        LET comm = "fglgo AFIM014 ",comm_arg CLIPPED
        RUN comm

    ON KEY ( CONTROL-V )
        LET comm = "fglgo AFIM015 ",comm_arg
        RUN comm

    ON KEY ( CONTROL-N )
        LET comm = "fglgo AFIM016 ",comm_arg CLIPPED
        RUN comm

    ON KEY ( CONTROL-P )
          LET comm = "fglgo AFIM025 ",g_afiliados.n_folio," ", g_afiliados.tipo_solicitud
          RUN comm }

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

        CALL inserta()

        EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION inserta()
#ins--------------

    DEFINE
        bnd_x SMALLINT

   DEFINE ban_rdn             DATE

    LET bnd_x = 0

    IF g_afiliados.marca_envio = 'X' THEN
        UPDATE afi_domicilio
        SET    marca_envio    = ''
        WHERE  n_folio        = g_afiliados.n_folio
        AND    tipo_solicitud = g_afiliados.tipo_solicitud

        LET bnd_x = 1
    END IF

    IF ACCION = 'A' THEN
        INSERT INTO afi_domicilio
        VALUES (g_afiliados.n_seguro      ,
                g_afiliados.n_folio       ,
                g_afiliados.tipo_solicitud,
                g_afiliados.calle         ,
                g_afiliados.calle_ref1    ,
                g_afiliados.calle_ref2    ,
                g_afiliados.numero        ,
                g_afiliados.depto         ,
                g_afiliados.colonia       ,
                g_afiliados.delega        ,
                g_afiliados.ciudad        ,
                g_afiliados.estado        ,
                g_afiliados.codpos        ,
                g_afiliados.dom_cod       ,
                g_afiliados.pais_cod      ,
                g_afiliados.marca_envio   ,
                g_usuario                 ,
                HOY)

        ERROR "Ingresando registro"
    END IF

    IF ACCION = 'M' THEN
        UPDATE afi_domicilio 
        SET    nss            = g_afiliados.n_seguro      ,
               n_folio        = g_afiliados.n_folio       ,
               tipo_solicitud = g_afiliados.tipo_solicitud,
               calle          = g_afiliados.calle         ,
               numero         = g_afiliados.numero        ,
               depto          = g_afiliados.depto         ,
               calle_ref1     = g_afiliados.calle_ref1    ,
               calle_ref2     = g_afiliados.calle_ref2    ,
               colonia        = g_afiliados.colonia       ,
               delega         = g_afiliados.delega        ,
               ciudad         = g_afiliados.ciudad        ,
               estado         = g_afiliados.estado        ,
               codpos         = g_afiliados.codpos        ,
               dom_cod        = g_afiliados.dom_cod       ,
               pais_cod       = g_afiliados.pais_cod      ,
               usuario        = USER                      ,
               factualiza     = TODAY                     ,
               marca_envio    = g_afiliados.marca_envio
        WHERE  rowid          = vidrow

        #NOT DOM OP 17
        IF g_afiliados.tipo_solicitud = 1  OR
           g_afiliados.tipo_solicitud = 3  OR
           g_afiliados.tipo_solicitud = 6  OR
           g_afiliados.tipo_solicitud = 8  OR
           g_afiliados.tipo_solicitud = 10 OR
           g_afiliados.tipo_solicitud = 11 OR
           g_afiliados.tipo_solicitud = 12 THEN

           SELECT MAX(factualiza)
           INTO   ban_rdn
           FROM   afi_ctr_domicilio
           WHERE  n_folio         = g_afiliados.n_folio
           AND    tipo_solicitud  = g_afiliados.tipo_solicitud
           AND    nss             = g_afiliados.n_seguro
           OR     curp            = g_afiliados.n_unico
           AND    status_interno IN (20,40,42)
           AND    cod_resultado   = '02'
           --GROUP BY 1
           IF ban_rdn IS NOT NULL THEN
              WHILE TRUE
                PROMPT "Desea reenviar la notificacion ",
                       "del domcilio OP 17 [S/N]: " FOR aux_pausa
                IF aux_pausa MATCHES "[SsNn]" THEN
                   IF aux_pausa MATCHES "[Ss]" THEN
                      UPDATE afi_mae_afiliado
                      SET    documento_6    = 2
                      WHERE  n_folio        = g_afiliados.n_folio
                      AND    tipo_solicitud = g_afiliados.tipo_solicitud
                   END IF
                   EXIT WHILE
                END IF
              END WHILE
           END IF
        END IF
        #NOT DOM OP 17

        ERROR "Registro actualizado"
    END IF

    IF SQLCA.SQLCODE = 0 AND
       bnd_x THEN
        SELECT "X"
        FROM   afi_mae_modifica
        WHERE  @n_seguro        = g_afiliados.n_seguro
        AND    @cod_operacion   = 12
        AND    @fecha_actualiza IS NULL

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE afi_mae_modifica
            SET    fecha_modifica   = HOY
            WHERE  @n_seguro        = g_afiliados.n_seguro
            AND    @cod_operacion   = 12
            AND    @fecha_actualiza IS NULL
        ELSE
            LET g_afiliados.n_rfc = '  '

            INSERT INTO afi_mae_modifica
            VALUES(g_afiliados.tipo_solicitud,
                   g_afiliados.n_folio       ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   g_afiliados.paterno       ,
                   g_afiliados.materno       ,
                   g_afiliados.nombres       ,
                   g_afiliados.n_seguro      ,
                   g_afiliados.n_rfc         ,
                   g_afiliados.n_unico       ,
                   vsexo                     ,
                   ''                        ,
                   g_afiliados.fena          ,
                   ''                        ,
                   ''                        ,
                   vedon                     ,
                   vnac                      ,
                   vtip                      ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   HOY                       ,
                   g_usuario                 ,
                   12                        ,
                   516                       ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   ''                        ,
                   f_reclama                 ,
                   id_reclama)
        END IF
    END IF

    SLEEP 2
    ERROR ""

    SELECT count(*)
    INTO   vtotal
    FROM   afi_domicilio
    WHERE  @n_folio        = g_afiliados.n_folio
    AND    @tipo_solicitud = g_afiliados.tipo_solicitud
    AND    @marca_envio    = 'X'

    IF vtotal <> 1 OR
       vtotal IS NULL THEN
        UPDATE afi_domicilio
        SET    marca_envio    = ''
        WHERE  n_folio        = g_afiliados.n_folio
        AND    tipo_solicitud = g_afiliados.tipo_solicitud

        IF ACCION = 'A' THEN
            SELECT MIN(rowid)
            INTO   vidrow
            FROM   afi_domicilio
            WHERE  @n_folio        = g_afiliados.n_folio
            AND    @tipo_solicitud = g_afiliados.tipo_solicitud
        END IF

        UPDATE afi_domicilio
        SET    marca_envio = 'X'
        WHERE  rowid       = vidrow

        LET vtotal = 0
    END IF

    CALL Inicializa()

END FUNCTION

FUNCTION Consulta()
#C-----------------

    DEFINE a_afiliados  ARRAY[100] OF RECORD
        idrow       INTEGER                      ,
        calle       LIKE afi_domicilio.calle     ,
        numero      LIKE afi_domicilio.numero    ,
        depto       LIKE afi_domicilio.depto     ,
        calle_ref1  LIKE afi_domicilio.calle_ref1,
        calle_ref2  LIKE afi_domicilio.calle_ref2,
        codpos      LIKE afi_domicilio.codpos    ,
        colonia     LIKE afi_domicilio.colonia   ,
        delega      LIKE afi_domicilio.delega    ,
        desc_delega CHAR(50)                     ,
        ciudad      LIKE afi_domicilio.ciudad    ,
        desc_ciudad CHAR(50)                     ,
        estado      LIKE afi_domicilio.estado    ,
        desc_estado CHAR(50)                     ,
        pais_cod    LIKE afi_domicilio.pais_cod  ,
        pais_desc   CHAR(40)                     ,
        dom_cod     LIKE afi_domicilio.dom_cod   ,
        dom_desc    CHAR(50)                     ,
        marca_envio CHAR(1)
    END RECORD

    DEFINE sale   SMALLINT
    DEFINE pos    SMALLINT
    DEFINE igual  CHAR(11)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " CONTROL : [C] Salir         CONTROL[T]  Telefonos                             " AT 1,1 ATTRIBUTE(CYAN)
    ---DISPLAY " CONTROL : [T] Telefonos   [B] Beneficiarios   [V] Patrones   [N] Siefores     " AT 2,1 ATTRIBUTE(CYAN)

    DISPLAY BY NAME g_afiliados.*

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
    FROM   afi_domicilio  a,
    OUTER  tab_delegacion b,
    OUTER  tab_ciudad     c,
    OUTER  tab_estado     d,
    OUTER  tab_domicilio  e,
    OUTER  tab_pais       f
    WHERE  a.nss            = g_afiliados.n_seguro
    AND    a.n_folio        = g_afiliados.n_folio
    AND    a.tipo_solicitud = g_afiliados.tipo_solicitud
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
        ERROR "NO EXISTEN REGISTROS DE DOMICILIOS"
        SLEEP 3
        ERROR ""
    ELSE
        CALL SET_COUNT (pos-1)
        DISPLAY ARRAY a_afiliados TO scr.*

        ON KEY(INTERRUPT)
            CALL Inicializa()
            --CLEAR FORM
            EXIT DISPLAY

        ON KEY ( CONTROL-T )
            LET comm = "fglgo AFIM013 ",comm_arg CLIPPED
            RUN comm

       {
        ON KEY ( CONTROL-B )
            LET comm = "fglgo AFIM014 ",comm_arg CLIPPED
            RUN comm

        ON KEY ( CONTROL-V )
            LET comm = "fglgo AFIM015 ",comm_arg CLIPPED
            RUN comm

        ON KEY ( CONTROL-N )
            LET comm = "fglgo AFIM016 ",comm_arg CLIPPED
            RUN comm

       ON KEY ( CONTROL-P )
            LET comm = "fglgo AFIM025 ", g_afiliados.n_folio," ", g_afiliados.tipo_solicitud
            LET comm = comm CLIPPED
            RUN comm}

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Modifica()
#M-----------------

    DEFINE a_afiliados  ARRAY[100] OF RECORD
        idrow       INTEGER                      ,
        calle       LIKE afi_domicilio.calle     ,
        numero      LIKE afi_domicilio.numero    ,
        depto       LIKE afi_domicilio.depto     ,
        calle_ref1  LIKE afi_domicilio.calle_ref1,
        calle_ref2  LIKE afi_domicilio.calle_ref2,
        codpos      LIKE afi_domicilio.codpos    ,
        colonia     LIKE afi_domicilio.colonia   ,
        delega      LIKE afi_domicilio.delega    ,
        desc_delega CHAR(50)                     ,
        ciudad      INTEGER                      ,
        desc_ciudad CHAR(50)                     ,
        estado      INTEGER                      ,
        desc_estado CHAR(50)                     ,
        pais_cod    LIKE afi_domicilio.pais_cod  ,
        pais_desc   CHAR(40)                     ,
        dom_cod     LIKE afi_domicilio.dom_cod   ,
        dom_desc    CHAR(50)                     ,
        marca_envio CHAR(1)
    END RECORD

    DEFINE cod_ant  CHAR(5)
    DEFINE sale     SMALLINT
    DEFINE pos      SMALLINT
    DEFINE igual    CHAR(11)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Esc ] Grabar   CONTROL[T] Telefonos        CONTROL [C] Salir sin Grabar     " AT 1,1 ATTRIBUTE(CYAN)
    ---DISPLAY " CONTROL : [T] Telefonos   [B] Beneficiarios   [V] Patrones   [N] Siefores     " AT 2,1 ATTRIBUTE(CYAN)

    DISPLAY BY NAME g_afiliados.*

    LET pos   = 1
    LET sale  = FALSE
    LET igual =  g_afiliados.n_seguro

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
    FROM   afi_domicilio  a,
    OUTER  tab_delegacion b,
    OUTER  tab_ciudad     c,
    OUTER  tab_estado     d,
    OUTER  tab_domicilio  e,
    OUTER  tab_pais       f
    WHERE  a.nss            = g_afiliados.n_seguro
    AND    a.n_folio        = g_afiliados.n_folio
    AND    a.tipo_solicitud = g_afiliados.tipo_solicitud
    AND    b.deleg_cod      = a.delega
    AND    c.ciudad_cod     = a.ciudad
    AND    d.estad_cod      = a.estado
    AND    e.dom_cod        = a.dom_cod
    AND    f.pais_cod       = a.pais_cod

    FOREACH cursor_3 INTO a_afiliados[pos].*
        IF pos > 10 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
        END IF

        LET pos = pos + 1
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "NO EXISTEN REGISTROS.DE DOMICILIOS"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    ELSE
        IF (pos-1) >= 1 THEN
            CALL SET_COUNT (pos-1)
            DISPLAY ARRAY a_afiliados TO scr.*

            ON KEY(INTERRUPT)
                EXIT DISPLAY
                --RETURN
                --EXIT PROGRAM

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
                LET vidrow                  = a_afiliados[pos].idrow

                LET cod_ant = g_afiliados.codpos

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
                    IF g_afiliados.codpos <> cod_ant THEN
                        SELECT "X"
                        FROM tab_codpos
                        WHERE cpos_cod = g_afiliados.codpos

                        IF STATUS = 100 THEN
                            ERROR "CP no existe en catalogo, dejar valor NULO para desplegar pantalla de Codigos"
                            NEXT FIELD codpos
                        END IF

                        CALL Despliega_colonias(g_afiliados.codpos)
                            RETURNING g_afiliados.colonia    ,
                                      g_afiliados.delega     ,
                                      g_afiliados.desc_delega,
                                      g_afiliados.ciudad     ,
                                      g_afiliados.desc_ciudad,
                                      g_afiliados.estado     ,
                                      g_afiliados.desc_estado
                    ELSE
                        NEXT FIELD dom_cod
                    END IF
                END IF

                DISPLAY BY NAME g_afiliados.*

                NEXT FIELD colonia

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
                    CALL Despliega_pais()
                        RETURNING g_afiliados.pais_cod,
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
                        RETURNING g_afiliados.dom_cod,
                                  g_afiliados.dom_desc
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

                NEXT FIELD marca_envio

            AFTER FIELD marca_envio
                IF g_afiliados.marca_envio <> 'X' OR
                   g_afiliados.marca_envio <> '' THEN
                    ERROR "Marca envio solo debe ser 'X' o nulo"
                    LET g_afiliados.marca_envio = NULL
                    DISPLAY BY NAME g_afiliados.marca_envio
                    NEXT FIELD marca_envio
                END IF

            ON KEY ( INTERRUPT )
                EXIT INPUT

            ---AFTER INPUT
            ON KEY (ESC)
                IF bandera = 0 THEN
                    SELECT "X"
                    FROM   afi_domicilio
                    WHERE  @nss            = g_afiliados.n_seguro
                    AND    @n_folio        = g_afiliados.n_folio
                    AND    @tipo_solicitud = g_afiliados.tipo_solicitud
                    AND    @marca_envio    = 'X'

                    IF SQLCA.SQLCODE <> 0 AND
                       g_afiliados.marca_envio IS NULL THEN
                        NEXT FIELD marca_envio
                    END IF

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
                            RETURNING g_afiliados.codpos     ,
                                      g_afiliados.colonia    ,
                                      g_afiliados.delega     ,
                                      g_afiliados.desc_delega,
                                      g_afiliados.ciudad     ,
                                      g_afiliados.desc_ciudad,
                                      g_afiliados.estado     ,
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

                IF aux_pausa MATCHES "[Ss]" THEN
                    IF FIELD_TOUCHED(scr.*) THEN
                        CALL inserta()
                    END IF
                ELSE
                    ERROR "Proceso de actualizacion cancelado"
                    SLEEP 2
                    ERROR ""
                END IF

              EXIT INPUT

          END INPUT

          EXIT DISPLAY

          ON KEY ( CONTROL-T )
              LET comm = "fglgo AFIM013 ",comm_arg CLIPPED
              RUN comm

        {
          ON KEY ( CONTROL-B )
              LET comm = "fglgo AFIM014 ",comm_arg CLIPPED
              RUN comm

          ON KEY ( CONTROL-V )
              LET comm = "fglgo AFIM015 ",comm_arg CLIPPED
              RUN comm

          ON KEY ( CONTROL-N )
              LET comm = "fglgo AFIM016 ",comm_arg CLIPPED
              RUN comm

          ON KEY ( CONTROL-P )
               LET comm = "fglgo AFIM025 ",g_afiliados.n_folio," ", g_afiliados.tipo_solicitud
               LET comm = comm CLIPPED
               RUN comm}

           END DISPLAY
       END IF
   END IF

END FUNCTION

FUNCTION pregunta_modifica()
#pm-------------------------

    PROMPT "Desea actualizar registro ? S/N " FOR aux_pausa

END FUNCTION

FUNCTION Despliega_tipos_dom()
#Dtd--------------------------

    DEFINE
        aux_val ,
        pos     SMALLINT

    DEFINE
        x_x      CHAR(100),
        x_buscar CHAR(30)

    DEFINE l_reg ARRAY[20] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
    END RECORD

    OPEN WINDOW vent_1 AT 05,12 WITH FORM "AFIM0018" ATTRIBUTE(BORDER)
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

        LET pos = 1

        PREPARE curg3 FROM x_x

        DECLARE cur_g3 CURSOR FOR curg3

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
        DISPLAY ARRAY l_reg TO scr.*

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

