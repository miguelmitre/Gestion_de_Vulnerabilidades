#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM802  => MANTENEDOR DE REGISTRO DE CERTIFICADO IMSS - RETIRO POR   #
#                     NEGATIVA DE PENSION (TIPO RETIRO D)                       #
#                                  VERSION COPPEL                               #
#Fecha creacion    => 16 DE ENERO DE 2004                                       #
#By                => JOSE LUIS SALDIVAR CARDOSO                                #
#Fecha actualiz.   => 16 DE OCTUBRE DE 2007                                     #
#Actualizacion     => SILVERIA CONTRERAS GARCIA                                 #
#                  => Desarrollar la opcion para rechazar solicitudes de retiro #
#                     y seleccionar de catalogo el motivo de rechazo            #
#Fecha actualiz.   => 18 DE ABRIL DE 2008                                       #
#Actualizacion     => XAVIER TORRES RIOS                                        #
#                  => Se modifica para buscar el diagnostico de procesar de la  #
#                     tabla ret_solicitud_tx y el diagnostico de vivienda de la #
#                     tabla ret_monto_viv. En caso de no encontrarse alli se    #
#                     busca en ret_solicitud_rx                                 #
#Fecha actualiz.   => 1 DE SEPTIEMBRE DE 2010                                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se ajusta para que el programa tome correctamente los     #
#                     valores de estados de solicitud de la tabla ret_estado.   #
#                     Se modifica para que solo puedan confirmarse solicitudes  #
#                     en estado capturado y rechazar en estado capturado y      #
#                     precapturado                                              #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af
GLOBALS
    DEFINE  ga_solicitud   ARRAY[5000] OF RECORD #glo #ga_solicitud
        n_seguro           LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_retiro        CHAR(01)                               ,
        des_tipo_ret       CHAR(60)                               ,
        tipo_prestacion    LIKE ret_det_datamart.curp             ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        num_resolucion     INTEGER                                ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    DATE                                   ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        semanas_cotizadas  SMALLINT                               ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        referencia         CHAR(12)                               ,
        fecha_captura      DATE                                   ,
        --fecha_modificacion DATE                                   ,
        fecha_confirma     DATE                                   ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        descripcion_status CHAR(40)                               ,
        folio              INTEGER                                ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        --usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma,
        fecha_envio        DATE                                   ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv,
        fecha_ini_pen      LIKE ret_solicitud_tx.fecha_ini_pen
    END RECORD

    DEFINE  rg_datamart    RECORD #glo #rg_input
        nss                LIKE ret_det_datamart.nss              ,
        rfc                LIKE afi_mae_afiliado.n_rfc            ,
        curp               LIKE afi_mae_afiliado.n_unico          ,
        paterno_afore      LIKE afi_mae_afiliado.paterno          ,
        materno_afore      LIKE afi_mae_afiliado.materno          ,
        nombre_afore       LIKE afi_mae_afiliado.nombres          ,
        tipo_ret           CHAR(01)                               ,
        descripcion        CHAR(60)                               ,
        tipo_prestacion    LIKE ret_det_datamart.curp             ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        num_resolucion     INTEGER                                ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    DATE                                   ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        semanas_cotizadas  SMALLINT                               ,
        referencia         CHAR(12)                               ,
        fecha_captura      DATE                                   ,
        --fecha_modifica     DATE                                   ,
        fecha_confirma     DATE                                   ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        descripcion_status CHAR(40)                               ,
        folio              INTEGER                                ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        --usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma ,
        fecha_ini_pen      LIKE ret_solicitud_tx.fecha_ini_pen
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        precapturado       LIKE ret_estado.estado_solicitud ,
        capturado          LIKE ret_estado.estado_solicitud ,
        confirmado         LIKE ret_estado.estado_solicitud ,
        rechazado          LIKE ret_estado.estado_solicitud ,
        liquidado          LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE  #glo #date
        HOY                ,
        vfecha_ini_p       ,
        vfecha_resolucion  ,
    xx_fecha_solicitud ,
    vfecha_causa       ,
        fecha_max_habil    DATE

    DEFINE  #glo #char
        txt_1              CHAR(3000),
        txt_2              CHAR(3000),
        txt_3              CHAR(2200),
        txt_4              CHAR(2200),
        txt_5              CHAR(2200),
        x_busca            CHAR(2200),
        s_codigo_afore     CHAR(0004),
        usuario            CHAR(0012),
        option_afore       CHAR(0006),
        v_marca            CHAR(0100),
        v_desmarca         CHAR(0100),
        vdesmarca          CHAR(0100), --sil
        vmax_sec_pension   CHAR(0002),
        ejecuta            CHAR(0500),
        enter                        ,
        opc                CHAR(0001),
    x_error            CHAR(0500),   ---omar
        ok_datamart        CHAR(0007),
        desc_status_rech   CHAR(0020),
    x_usuario          CHAR(0012),
    x_estado_solicitud CHAR(0040),
    v_ejecuta          CHAR(0200),
        vaccion            CHAR(0001),
        xaccion            CHAR(0001),
        vnss               CHAR(0011),
        vregimen           CHAR(0002),
        vtipo_seguro       CHAR(0002),
        vtipo_pension      CHAR(0002),
        vtipo_retiro       CHAR(0001)

    DEFINE #glo #smallint
        arr_c          ,
        marca_ent          ,
        i                  ,
        pos                ,
        v_marca_res        ,
        v_marca_ent        ,
        v_cod_rechazo      ,
        s_tipo_movimiento  ,
        estado_convivencia ,
        sw_2               ,
    sw                 ,
        vtipo_prestacion   ,
        vestado_marca      ,
        vcodigo_rechazo    ,
        vmarca_causa       ,
        v_tipo_movimiento  , --sil
        codigo             , -- sil
        entidad            , --sil
        vsemanas_cotizadas ,
        flag               SMALLINT

    DEFINE #glo #integer
        ult_consecutivo    ,
        vmax_folio         INTEGER

    DEFINE varmov  like tab_retiro.movimiento
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM802.log")
    CALL init()

    OPEN WINDOW retm8021 AT 2,3 WITH FORM "RETM8021" ATTRIBUTE(BORDER)
    DISPLAY " RETM802                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)

    CASE vaccion
       WHEN "C"
            CALL inicializa()
            CALL agrega() #a
       WHEN "M"
            CALL inicializa()
            CALL modifica() #m
       OTHERWISE

    MENU "MENU"
        BEFORE MENU
            IF option_afore <> FALSE THEN
                HIDE OPTION ALL
                SHOW OPTION "Agrega","Consulta","Modifica","Elimina","Salida"
            ELSE
                HIDE OPTION ALL
                SHOW OPTION "Agrega","Consulta","Modifica",
                            "Elimina","conFirma","Salida"
            END IF

        COMMAND KEY("A") "Agrega" "Agrega Solicitud"
            LET xaccion = " "
            CALL inicializa()
            CALL agrega() #a

        COMMAND KEY("C") "Consulta" "Consulta Solicitud"
            LET xaccion = " "
            CALL inicializa()
            CALL consulta() #c

        COMMAND KEY("M") "Modifica" "Modifica Solicitud"
            LET xaccion = " "
            CALL inicializa()
            CALL modifica() #m

        COMMAND KEY("E") "Elimina" "Elimina Solicitud"
            LET xaccion = " "
            CALL inicializa()
            CALL elimina() #e

        COMMAND KEY("F") "conFirma" "Confirma Solicitud"
            LET xaccion = "F"
            CALL inicializa()
            CALL confirma() #c

        COMMAND KEY("S") "Salida" "Regresa al Menu"
            EXIT MENU

    END MENU
    END CASE
    CLOSE WINDOW retm8021
END MAIN

FUNCTION init()
#in-------------

    LET vaccion           = ARG_VAL(1)
    LET vnss              = ARG_VAL(2)
    LET vregimen          = ARG_VAL(3)
    LET vtipo_prestacion  = ARG_VAL(4)
    LET vtipo_seguro      = ARG_VAL(5)
    LET vtipo_pension     = ARG_VAL(6)
    LET vtipo_retiro      = ARG_VAL(7)
    LET vfecha_ini_p      = ARG_VAL(8)
    LET vfecha_resolucion = ARG_VAL(9)

    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_1.precapturado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRECAPTURADO"

    SELECT A.estado_solicitud
    INTO   reg_1.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   reg_1.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_1.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   reg_1.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"


    ######## MARCAJE ###################

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "

    PREPARE eje_marca FROM v_marca

    ####################################

END FUNCTION

FUNCTION inicializa()
#i--------------------
    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    DEFINE  sw_1               ,
            vestado_solicitud  SMALLINT

    DEFINE #loc #decimal
        vconsecutivo               LIKE ret_solicitud_tx.consecutivo

    LET sw_1 = 0

    OPEN WINDOW retm8022 AT 2,3 WITH FORM "RETM8022" ATTRIBUTE (BORDER)
    DISPLAY " RETM802                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " AGREGA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega    Ctrl-C : Salir   " AT 2,1

    INPUT BY NAME rg_datamart.* WITHOUT DEFAULTS

        BEFORE FIELD nss
        IF vaccion = 'C' OR
           vaccion = 'M' THEN

                LET rg_datamart.nss              = vnss
                LET rg_datamart.regimen          = vregimen
                LET rg_datamart.tipo_prestacion  = vtipo_prestacion
                LET rg_datamart.tipo_seguro      = vtipo_seguro
                LET rg_datamart.tipo_pension     = vtipo_pension
                LET rg_datamart.tipo_ret         = vtipo_retiro
                LET rg_datamart.fecha_resolucion = vfecha_resolucion

                DISPLAY rg_datamart.nss              TO nss
                DISPLAY rg_datamart.regimen          TO regimen
                DISPLAY rg_datamart.tipo_prestacion  TO tipo_prestacion
                DISPLAY rg_datamart.tipo_seguro      TO tipo_seguro
                DISPLAY rg_datamart.tipo_pension     TO tipo_pension
                DISPLAY rg_datamart.tipo_ret         TO tipo_ret
            DISPLAY rg_datamart.fecha_solicitud  TO fecha_solicitud
            DISPLAY rg_datamart.fecha_resolucion TO fecha_resolucion

                SELECT n_rfc   ,
                       n_unico ,
                       paterno ,
                       materno ,
                       nombres
                INTO  rg_datamart.rfc           ,
                      rg_datamart.curp          ,
                      rg_datamart.paterno_afore ,
                      rg_datamart.materno_afore ,
                      rg_datamart.nombre_afore
                FROM  afi_mae_afiliado
                WHERE n_seguro = rg_datamart.nss

                SELECT A.descripcion
                INTO   rg_datamart.descripcion_status
                FROM   ret_estado A
                WHERE  A.status = reg_1.capturado

                LET rg_datamart.descripcion   = cat_tipo_ret()

                SELECT descripcion
                INTO   rg_datamart.desc_prestacion
                FROM   tab_prestacion
                WHERE  tipo_prestacion = rg_datamart.tipo_prestacion

                SELECT descripcion
                INTO   rg_datamart.desc_seguro
                FROM   tab_seguro
                WHERE  clave = rg_datamart.tipo_seguro
                AND    clave IN ('CV','IM')

                IF rg_datamart.tipo_seguro = 'CV' THEN
                    SELECT descripcion
                    INTO   rg_datamart.desc_pension
                    FROM   tab_pension
                    WHERE  tipo_pension = rg_datamart.tipo_pension
                    AND    tipo_pension IN ('CE','VE')
                ELSE
                    SELECT descripcion
                    INTO   rg_datamart.desc_pension
                    FROM   tab_pension
                    WHERE  tipo_pension = rg_datamart.tipo_pension
                    AND    tipo_pension IN ('AS','OR','VI','VO','IN')
                END IF

                { SELECT MAX(folio)
                INTO   vmax_folio
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_resolucion = rg_datamart.fecha_resolucion
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion
                AND    diag_datamart    in (101,300,301)

                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_resolucion = rg_datamart.fecha_resolucion
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion
                AND    diag_datamart    in (101,300,301)
                AND    folio            = vmax_folio

                SELECT fecha_resolucion  ,
                       sec_pension       ,
                       diag_datamart     ,
                       semanas_cotizadas
                INTO   rg_datamart.fecha_resolucion,
                       rg_datamart.sec_pension     ,
                       rg_datamart.diag_datamart   ,
                       rg_datamart.semanas_cotizadas
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_resolucion = rg_datamart.fecha_resolucion
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion
                AND    diag_datamart    in (101,300,301)
                AND    folio            = vmax_folio
                AND    sec_pension      = vmax_sec_pension }

                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = vfecha_ini_p
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)

                SELECT MAX(folio)
                INTO   vmax_folio
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = vfecha_ini_p
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension

                SELECT fecha_resolucion  ,
                       sec_pension       ,
                       diag_datamart     ,
                       semanas_cotizadas
                INTO   rg_datamart.fecha_resolucion,
                       rg_datamart.sec_pension     ,
                       rg_datamart.diag_datamart   ,
                       rg_datamart.semanas_cotizadas
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = vfecha_ini_p
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension
                AND    folio            = vmax_folio

                DISPLAY BY NAME rg_datamart.materno_afore
                DISPLAY BY NAME rg_datamart.paterno_afore
                DISPLAY BY NAME rg_datamart.nombre_afore
                DISPLAY BY NAME rg_datamart.rfc
                DISPLAY BY NAME rg_datamart.curp
                DISPLAY BY NAME rg_datamart.descripcion_status
                DISPLAY rg_datamart.desc_seguro  TO desc_seguro
                DISPLAY rg_datamart.desc_pension TO desc_pension
                DISPLAY rg_datamart.descripcion  TO descripcion
                DISPLAY BY NAME rg_datamart.fecha_resolucion
                DISPLAY BY NAME rg_datamart.sec_pension
                DISPLAY BY NAME rg_datamart.diag_datamart
                DISPLAY BY NAME rg_datamart.semanas_cotizadas
                DISPLAY BY NAME rg_datamart.desc_prestacion

            END IF

        AFTER FIELD nss

            IF rg_datamart.nss IS NULL THEN
                ERROR " NSS NO REGISTRADO EN LA AFORE.. "
                NEXT FIELD nss
            END IF

            SELECT n_rfc   ,
                   n_unico ,
                   paterno ,
                   materno ,
                   nombres
            INTO   rg_datamart.rfc           ,
                   rg_datamart.curp          ,
                   rg_datamart.paterno_afore ,
                   rg_datamart.materno_afore ,
                   rg_datamart.nombre_afore
            FROM  afi_mae_afiliado
            WHERE n_seguro = rg_datamart.nss

            IF  SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " NO EXISTE NUMERO SE SEGURO SOCIAL "
                SLEEP 2
                NEXT FIELD nss
            END IF

            SELECT "OK"
            FROM   ret_solicitud_tx
            WHERE  nss = rg_datamart.nss
            AND    tipo_prestacion = 3
            AND    fecha_captura   = TODAY
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND THEN
                ERROR " YA EXISTE AL DIA DE HOY UNA SOLICITUD "
                ATTRIBUTE(REVERSE) SLEEP 2
                ERROR ""
                NEXT FIELD nss
            END IF

            SELECT A.descripcion
            INTO   rg_datamart.descripcion_status
            FROM   ret_estado A
            WHERE  A.status = reg_1.capturado

            DISPLAY BY NAME rg_datamart.materno_afore
            DISPLAY BY NAME rg_datamart.paterno_afore
            DISPLAY BY NAME rg_datamart.nombre_afore
            DISPLAY BY NAME rg_datamart.rfc
            DISPLAY BY NAME rg_datamart.curp
            DISPLAY BY NAME rg_datamart.descripcion_status

            IF sw_1 = 0   THEN
                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET rg_datamart.tipo_ret         = "D"
                LET rg_datamart.descripcion      = cat_tipo_ret()
                LET rg_datamart.consecutivo      = ult_consecutivo
                LET rg_datamart.tipo_prestacion  = 3
                LET rg_datamart.regimen          = '97'
                LET rg_datamart.fecha_solicitud  = HOY
                LET rg_datamart.estado_solicitud = 0
                LET rg_datamart.fecha_captura    = HOY
                LET rg_datamart.usuario_captura  = usuario

                DISPLAY rg_datamart.tipo_ret         TO tipo_ret
                DISPLAY rg_datamart.descripcion      TO descripcion
                DISPLAY rg_datamart.consecutivo      TO consecutivo
                DISPLAY rg_datamart.tipo_prestacion  TO tipo_prestacion
                DISPLAY rg_datamart.regimen          TO regimen
                DISPLAY rg_datamart.fecha_solicitud  TO fecha_solicitud
                DISPLAY rg_datamart.estado_solicitud TO estado_solicitud
                DISPLAY rg_datamart.consecutivo      TO consecutivo
                DISPLAY rg_datamart.fecha_captura    TO fecha_captura
                DISPLAY rg_datamart.usuario_captura  TO usuario_captura

                LET sw_1 = 1
            END IF
            NEXT FIELD tipo_seguro

        AFTER FIELD tipo_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nss
            END IF

            IF rg_datamart.tipo_seguro <> "CV" AND
               rg_datamart.tipo_seguro <> "IM" THEN
               ERROR " COMBINACION INVALIDA... " ATTRIBUTE(REVERSE) SLEEP 2
               ERROR ""
               NEXT FIELD tipo_seguro
            END IF

            SELECT descripcion
            INTO   rg_datamart.desc_seguro
            FROM   tab_seguro
            WHERE  clave = rg_datamart.tipo_seguro
            AND    clave IN ('CV','IM')

            IF rg_datamart.tipo_seguro IS NULL OR
               rg_datamart.tipo_seguro = 0     THEN
               CALL despliega_tipo_seguro() #dtp
            END IF
            DISPLAY rg_datamart.desc_seguro TO desc_seguro

        AFTER FIELD tipo_pension
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_seguro
            END IF
            IF rg_datamart.tipo_pension = "CV" OR
               rg_datamart.tipo_pension = "IM" THEN
               ERROR " COMBINACION INVALIDA... " ATTRIBUTE(REVERSE) SLEEP 2
               ERROR ""
               NEXT FIELD tipo_pension
            END IF
            IF rg_datamart.tipo_seguro = 'CV' THEN
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                AND    tipo_pension IN ('CE','VE')
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                AND    tipo_pension IN ('AS','OR','VI','VO','IN')
            END IF
            DISPLAY rg_datamart.desc_pension TO desc_pension
            IF rg_datamart.tipo_pension IS NULL OR
                rg_datamart.tipo_pension = 0     THEN
                CALL despliega_tipo_pension() #dtp
            END IF

            IF (rg_datamart.tipo_seguro  =  'CV' AND
                rg_datamart.tipo_pension <> 'AS' AND
                rg_datamart.tipo_pension <> 'OR' AND
                rg_datamart.tipo_pension <> 'VI' AND
                rg_datamart.tipo_pension <> 'VO' AND
                rg_datamart.tipo_pension <> 'IN') OR
               (rg_datamart.tipo_seguro  = 'IM'  AND
                rg_datamart.tipo_pension <> 'CE' AND
                rg_datamart.tipo_pension <> 'VE') THEN
               NEXT FIELD num_resolucion
            ELSE
               ERROR " COMBINACION INVALIDA... " ATTRIBUTE(REVERSE) SLEEP 2
               ERROR ""
               NEXT FIELD tipo_pension
            END IF
            DISPLAY rg_datamart.desc_pension TO desc_pension

        AFTER FIELD num_resolucion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_pension
            END IF

        LET vestado_solicitud  = " "

            SELECT MAX(consecutivo)
        INTO   vconsecutivo
        FROM   ret_solicitud_tx
        WHERE  nss              = rg_datamart.nss
            AND    tipo_retiro      = "D"
            AND    tipo_seguro      = rg_datamart.tipo_seguro
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_prestacion  = rg_datamart.tipo_prestacion

        SELECT estado_solicitud
        INTO   vestado_solicitud
        FROM   ret_solicitud_tx
        WHERE  nss              = rg_datamart.nss
            AND    tipo_retiro      = "D"
        AND    tipo_seguro      = rg_datamart.tipo_seguro
        AND    tipo_pension     = rg_datamart.tipo_pension
        AND    tipo_prestacion  = rg_datamart.tipo_prestacion
            AND    consecutivo      = vconsecutivo

        CASE vestado_solicitud
            WHEN 0
            DISPLAY "  YA EXISTE UNA SOLICITUD CAPTURADA",
                            "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD nss
            WHEN 3
            DISPLAY "  SOLICITUD EN ESTADO CONFIRMADO",
                            "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD nss
                WHEN 2
            DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                            "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD nss
                WHEN 4
            {SELECT "OK"
            FROM   ret_solicitud_rx A
            WHERE  A.nss         = rg_datamart.nss
                    AND    A.consecutivo = vconsecutivo

            IF STATUS = NOTFOUND THEN
                DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                        NEXT FIELD nss
                    ELSE}
                SELECT "OK"
                FROM   ret_solicitud_tx A
                WHERE  A.nss           = rg_datamart.nss
                        AND    A.consecutivo   = vconsecutivo
                AND    A.diag_registro = 400

                IF STATUS <> NOTFOUND THEN
                SELECT "OK"
                FROM   dis_cuenta A
                WHERE  A.nss              = rg_datamart.nss
                AND    A.consecutivo_lote = rg_datamart.consecutivo
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                        "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                                NEXT FIELD nss
                END IF
            END IF
                    --END IF
        END CASE

            NEXT FIELD fecha_ini_pen

        AFTER FIELD fecha_ini_pen

            IF rg_datamart.fecha_ini_pen    IS NULL THEN
                ERROR " LA FECHA INICIO DE PENSION NO PUEDE SER NULA "
                SLEEP 2
                NEXT FIELD  fecha_ini_pen
            END IF
            NEXT FIELD fecha_resolucion

        AFTER FIELD fecha_resolucion
            IF rg_datamart.fecha_resolucion > HOY THEN
                ERROR " LA FECHA DE RESOLUCION NO DEBE SER MAYOR A LA FECHA ",
                      "DEL DIA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_resolucion
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_resolucion
            END IF

            IF rg_datamart.fecha_resolucion IS NULL THEN
                ERROR " LA FECHA RESOLUCION NO PUDE SER NULA "
                SLEEP 2
                NEXT FIELD  fecha_resolucion
            END IF

            { SELECT MAX(folio)
            INTO   vmax_folio
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_resolucion = rg_datamart.fecha_resolucion
            AND    tipo_prestacion  = rg_datamart.tipo_prestacion
            AND    diag_datamart    in (101,300,301)

            SELECT MAX(sec_pension)
            INTO   vmax_sec_pension
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_resolucion = rg_datamart.fecha_resolucion
            AND    tipo_prestacion  = rg_datamart.tipo_prestacion
            AND    diag_datamart    in (101,300,301)
            AND    folio            = vmax_folio

            SELECT fecha_resolucion  ,
                   sec_pension       ,
                   diag_datamart     ,
                   semanas_cotizadas
            INTO   rg_datamart.fecha_resolucion,
                   rg_datamart.sec_pension     ,
                   rg_datamart.diag_datamart   ,
                   rg_datamart.semanas_cotizadas
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_resolucion = rg_datamart.fecha_resolucion
            AND    tipo_prestacion  = rg_datamart.tipo_prestacion
            AND    diag_datamart    in (101,300,301)
            AND    folio            = vmax_folio
            AND    sec_pension      = vmax_sec_pension }

            SELECT MAX(sec_pension)
            INTO   vmax_sec_pension
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      = rg_datamart.tipo_seguro
            AND    tipo_prestacion  = 3
            AND    diag_datamart    in (101,300,301,210,302,303)

            SELECT MAX(folio)
            INTO   vmax_folio
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      = rg_datamart.tipo_seguro
            AND    tipo_prestacion  = 3
            AND    diag_datamart    in (101,300,301,210,302,303)
            AND    sec_pension      = vmax_sec_pension

            SELECT fecha_resolucion  ,
                   sec_pension       ,
                   diag_datamart     ,
                   semanas_cotizadas
            INTO   rg_datamart.fecha_resolucion,
                   rg_datamart.sec_pension     ,
                   rg_datamart.diag_datamart   ,
                   rg_datamart.semanas_cotizadas
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      = rg_datamart.tipo_seguro
            AND    tipo_prestacion  = 3
            AND    diag_datamart    in (101,300,301,210,302,303)
            AND    sec_pension      = vmax_sec_pension
            AND    folio            = vmax_folio

            IF SQLCA.SQLCODE = NOTFOUND THEN
                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    tipo_seguro      IN ("CV","IM")
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)

                SELECT MAX(folio)
                INTO   vmax_folio
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    tipo_seguro      IN ("CV","IM")
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension

                SELECT fecha_resolucion  ,
                   sec_pension       ,
                   diag_datamart     ,
                   semanas_cotizadas
                INTO   rg_datamart.fecha_resolucion,
                   rg_datamart.sec_pension     ,
                   rg_datamart.diag_datamart   ,
                   rg_datamart.semanas_cotizadas
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                AND    tipo_seguro      IN("CV","IM")
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension
                AND    folio            = vmax_folio
                IF SQLCA.SQLCODE <> NOTFOUND THEN
                   ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
                   SLEEP 2
                   NEXT FIELD  tipo_seguro
                ELSE
                   ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
                   ATTRIBUTE(REVERSE)
                   SLEEP 2
                   NEXT FIELD  tipo_seguro
                END IF
            ELSE
               ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
               ATTRIBUTE(REVERSE)
               DISPLAY BY NAME rg_datamart.fecha_resolucion
               DISPLAY BY NAME rg_datamart.sec_pension
               DISPLAY BY NAME rg_datamart.diag_datamart
               DISPLAY BY NAME rg_datamart.semanas_cotizadas
            END IF

                { SELECT fecha_resolucion  ,
                       sec_pension       ,
                       diag_datamart     ,
                       semanas_cotizadas
                INTO   rg_datamart.fecha_resolucion,
                       rg_datamart.sec_pension     ,
                       rg_datamart.diag_datamart   ,
                       rg_datamart.semanas_cotizadas
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_resolucion = rg_datamart.fecha_resolucion
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension }

            {  IF SQLCA.SQLCODE = NOTFOUND THEN
                PROMPT " NO EXISTE REGISTRO EN DATAMART..DESEA CAPTURAR <S/N>: "
                FOR CHAR enter
                IF enter MATCHES "[Ss]" THEN
                    NEXT FIELD sol_folio
                ELSE
                    EXIT INPUT
                END IF
            ELSE
                ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
                ATTRIBUTE(REVERSE)
            END IF
            DISPLAY BY NAME rg_datamart.fecha_resolucion
            DISPLAY BY NAME rg_datamart.sec_pension
            DISPLAY BY NAME rg_datamart.diag_datamart
            DISPLAY BY NAME rg_datamart.semanas_cotizadas }

            IF rg_datamart.fecha_resolucion > "03/10/1999" THEN
            ELSE
                NEXT FIELD sol_folio
            END IF

        AFTER FIELD sol_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_resolucion
            END IF
        AFTER FIELD fecha_solicitud
            IF rg_datamart.fecha_solicitud IS NULL THEN
                ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
                ATTRIBUTE(REVERSE)
                NEXT FIELD  fecha_solicitud
            END IF

                IF rg_datamart.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

            IF HOY <> rg_datamart.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF rg_datamart.fecha_solicitud < xx_fecha_solicitud THEN
          WHILE TRUE
                     PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO,DESEA CONTINUAR S/N " FOR opc
                 IF opc MATCHES "[SsNn]" THEN
                EXIT WHILE
                     END IF
                  END WHILE

              IF opc MATCHES "[Nn]" THEN
             EXIT INPUT
                  END IF
               END IF
        END IF
            IF  rg_datamart.fecha_resolucion >
                rg_datamart.fecha_solicitud  THEN
                ERROR "LA FECHA DE RESOLUCION NO PUEDE SER MAYOR A LA FECHA DE",
                      " SOLICITUD" ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_solicitud
            END IF
        ON KEY ( ESC )

           IF rg_datamart.sec_pension IS NULL THEN
               LET rg_datamart.sec_pension = '0'
           END IF

           IF rg_datamart.tipo_seguro IS NULL THEN
               ERROR " EL CAMPO TIPO SEGURO NO PUEDE SER NULO..."
               NEXT FIELD tipo_seguro
           END IF
           IF rg_datamart.tipo_pension IS NULL THEN
               ERROR " EL CAMPO TIPO PENSION NO PUEDE SER NULO..."
               NEXT FIELD tipo_seguro
           ELSE
           LET vestado_solicitud  = " "

               SELECT MAX(consecutivo)
           INTO   vconsecutivo
           FROM   ret_solicitud_tx
           WHERE  nss              = rg_datamart.nss
               AND    tipo_retiro      = "D"
               AND    tipo_seguro      = rg_datamart.tipo_seguro
               AND    tipo_pension     = rg_datamart.tipo_pension
               AND    tipo_prestacion  = rg_datamart.tipo_prestacion

           SELECT estado_solicitud
           INTO   vestado_solicitud
           FROM   ret_solicitud_tx
           WHERE  nss              = rg_datamart.nss
               AND    tipo_retiro      = "D"
           AND    tipo_seguro      = rg_datamart.tipo_seguro
           AND    tipo_pension     = rg_datamart.tipo_pension
           AND    tipo_prestacion  = rg_datamart.tipo_prestacion
               AND    consecutivo      = vconsecutivo

           CASE vestado_solicitud
               WHEN 0
               DISPLAY "  YA EXISTE UNA SOLICITUD CAPTURADA",
                               "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                       NEXT FIELD nss
               WHEN 3
               DISPLAY "  SOLICITUD EN ESTADO CONFIRMADO",
                               "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                       NEXT FIELD nss
                   WHEN 2
               DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                               "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                       NEXT FIELD nss
                   WHEN 4
               {SELECT "OK"
               FROM   ret_solicitud_rx A
               WHERE  A.nss         = rg_datamart.nss
                       AND    A.consecutivo = vconsecutivo

               IF STATUS = NOTFOUND THEN
                   DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                   "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                           NEXT FIELD nss
                       ELSE}
                   SELECT "OK"
                   FROM   ret_solicitud_tx A
                   WHERE  A.nss           = rg_datamart.nss
                           AND    A.consecutivo   = vconsecutivo
                   AND    A.diag_registro = 400

                   IF STATUS <> NOTFOUND THEN
                   SELECT "OK"
                   FROM   dis_cuenta A
                   WHERE  A.nss              = rg_datamart.nss
                 AND    A.consecutivo_lote = rg_datamart.consecutivo
                   GROUP BY 1

                   IF STATUS = NOTFOUND THEN
                       DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                           "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                                   NEXT FIELD nss
                   END IF
               END IF
                       --END IF
           END CASE
           END IF

           IF rg_datamart.fecha_resolucion IS NULL THEN
               ERROR " LA FECHA RESOLUCION NO PUDE SER NULA "
               SLEEP 2
               NEXT FIELD  fecha_resolucion
           END IF
           IF rg_datamart.sol_folio IS NULL THEN
               ERROR " EL CAMPO FOLIO NO PUEDE SER NULO..."
               NEXT FIELD sol_folio
           END IF

           IF rg_datamart.fecha_solicitud IS NULL THEN
               ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
               ATTRIBUTE(REVERSE)
               NEXT FIELD  fecha_solicitud
           END IF

           IF rg_datamart.fecha_solicitud > HOY THEN
              ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
              ATTRIBUTE(NORMAL)
              NEXT FIELD fecha_solicitud
           END IF

            IF HOY <> rg_datamart.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF rg_datamart.fecha_solicitud < xx_fecha_solicitud THEN
          WHILE TRUE
                     PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO,DESEA CONTINUAR S/N " FOR opc
                 IF opc MATCHES "[SsNn]" THEN
                EXIT WHILE
                     END IF
                  END WHILE

              IF opc MATCHES "[Nn]" THEN
             EXIT INPUT
                  END IF
               END IF
        END IF
           --INICIO PROCESO DE MARCAJE DE CUENTAS------------------------------


           LET rg_datamart.referencia = 0

           SELECT movimiento
           INTO   s_tipo_movimiento
           FROM   tab_retiro
           WHERE  tipo_retiro = "D"

           CALL marca_cuenta (rg_datamart.nss          ,
                              s_tipo_movimiento        ,
                              rg_datamart.consecutivo
                             )#mc
           RETURNING v_marca_res   ,
                     v_cod_rechazo

           LET rg_datamart.referencia = v_cod_rechazo
           IF v_cod_rechazo > 0 THEN
               SELECT A.rechazo_desc
               INTO   desc_status_rech
               FROM   tab_rch_marca A
               WHERE  A.rechazo_cod = v_cod_rechazo
               CURRENT WINDOW IS retm8022

               PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                      desc_status_rech,") <ENTER> CONTINUAR" FOR CHAR enter

           END IF

           WHENEVER ERROR CONTINUE

           INSERT INTO ret_solicitud_tx
                VALUES(rg_datamart.nss               ,
                       rg_datamart.consecutivo       ,
                       rg_datamart.folio             ,
                       rg_datamart.sol_folio         ,
                       " "                           , #tipo_id
                       rg_datamart.curp              ,
                       rg_datamart.sec_pension       , #se agrego este campo
                       ' '                           , #tipo_documento
                       rg_datamart.tipo_ret          ,
                       rg_datamart.regimen           ,
                       rg_datamart.tipo_seguro       ,
                       rg_datamart.tipo_pension      ,
                       rg_datamart.tipo_prestacion   ,
                       rg_datamart.fecha_ini_pen     , #fecha_ini_pension
                       rg_datamart.fecha_resolucion  ,
                       rg_datamart.fecha_solicitud   ,
                       " "                           , #cve_doc_probatorio
                       " "                           , #fecha_nacimiento
                       " "                           , #aseguradora
                       " "                           , #actuario
                       " "                           , #num_plan_pension
                       0                             , #periodo_pago
                       0                             , #acc_ret97
                       0                             , #acc_cv
                       0                             , #acc_cuo_soc
                       0                             , #acc_ret92
                       " "                           , #fecha_valor_viv
                       0                             , #saldo_viv97
                       0                             , #saldo_viv92
                       0                             , #saldo_viv72
                       0                             , #diag_datamart
                       " "                           , #estado_sub_viv
                       rg_datamart.semanas_cotizadas ,
                       reg_1.capturado               , #estado_solicitud
                       ' '                           , #entidad
                       ' '                           , #cod_rechazo_ent
                       rg_datamart.referencia        , #cod_rechazo
                       HOY                           , #fecha_captura
                       " "                           , #fecha_confirma
                       " "                           , #fecha_modifica
                       " "                           , #fecha_envio
                       usuario                       , #usuario_captura
                       " "                           , #usuario_confirma
                       " "                           , #usuario_modifica
                       0                             , #carta
                       9                             , #grupo
                       "T"                           , #cve_destino
                       " "                           , #porcentaje_val
                       rg_datamart.num_resolucion    ,
                       " "                           , #paterno_sol
                       " "                           , #materno_sol
                       " "
                     )

---omar
       IF SQLCA.SQLCODE < 0 THEN
              LET x_error = err_get(SQLCA.SQLCODE)
          LET x_error = "INSERT ret_solicitud_tx:",
                "nss ",rg_datamart.nss,
                "consecutivo ",rg_datamart.consecutivo,
                             x_error CLIPPED

              CALL errorlog(x_error CLIPPED)

          PROMPT "  ERROR AL INSERTAR REGISTROS AVISAR A SISTEMAS "
          FOR enter
          EXIT PROGRAM
           END IF
           WHENEVER ERROR STOP

           SELECT "OK"
           FROM   ret_beneficiario
           WHERE  nss         =  rg_datamart.nss
           AND    consecutivo =  rg_datamart.consecutivo
       GROUP BY 1

           IF STATUS = NOTFOUND THEN
               LET ejecuta = "fglgo RETM810 ",rg_datamart.nss," ",
                                                rg_datamart.consecutivo," ",
                                                "A"
               RUN ejecuta
           ELSE
               EXIT INPUT
           END IF

           SELECT "OK"
           FROM   ret_beneficiario
           WHERE  nss         =  rg_datamart.nss
           AND    consecutivo =  rg_datamart.consecutivo
           GROUP BY 1

           IF STATUS = NOTFOUND THEN
           ERROR "    NO SE PUEDE CAPTURAR LA SOLICITUD SIN BENEFICIARIOS"
               ATTRIBUTE(NORMAL)

               DELETE
               FROM  ret_solicitud_tx
               WHERE nss         =  rg_datamart.nss
               AND   consecutivo =  rg_datamart.consecutivo

               NEXT FIELD nss
           END IF

       ERROR "REGISTRO INGRESADO"
           SLEEP 3

       INITIALIZE vaccion  TO NULL

       EXIT INPUT

        ON KEY ( CONTROL-C )
                EXIT INPUT

        ON KEY ( INTERRUPT )
                EXIT INPUT

    END INPUT

  CLOSE WINDOW retm8022
END FUNCTION

FUNCTION consulta()
#c-----------------

    OPEN WINDOW retm8023 AT 2,3 WITH FORM "RETM8023" ATTRIBUTE (BORDER)
    DISPLAY " RETM802                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Consulta   Ctrl-C : Salir   Ctrl-B  Beneficiarios" AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON a.nss               ,
                                 b.n_rfc             ,
                                 a.curp              ,
                                 b.paterno           ,
                                 b.materno           ,
                                 b.nombres           ,
                                 a.regimen           ,
                                 a.tipo_seguro       ,
                                 a.tipo_pension      ,
                                 a.fecha_captura     ,
                                 a.fecha_confirma    ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo
        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT


    END CONSTRUCT

    IF int_flag = TRUE  THEN
                LET int_flag = FALSE
       CLOSE WINDOW retm8023
       RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"
    LET   txt_2 =" SELECT A.nss                      , ",
                        " B.n_rfc                    , ", #n_rfc
                        " A.curp                     , ",
                        " B.paterno                  , ", #paterno
                        " B.materno                  , ", #materno
                        " B.nombres                  , ", #nombres
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_prestacion          , ", #tipo_prestacion
                        " ' '                        , ", #desc_prestacion
                        " A.regimen                  , ",
                        " A.tipo_seguro              , ",
                        " ' '                        , ", #desc_tipo_seg
                        " A.tipo_pension             , ",
                        " ' '                        , ", #desc_pension
                        " A.num_resolucion           , ", #num_resolucion
                        " A.fecha_resolucion         , ", #fecha_resolucion
                        " A.folio_solicitud          , ", #folio solicitud
                        " A.fecha_solicitud          , ",
                        " A.sec_pension              , ", #sec_pension
                        " ' '                        , ", #diag_datamart
                        " A.semanas_cotizadas        , ", #semanas cotizadas
                        " A.cod_rechazo_ent          , ",
                        " ' '                        , ", #referencia
                        " A.fecha_captura            , ",
                       -- " A.fecha_modifica           , ",
                        " A.fecha_confirma           , ",
                        " ' '                        , ", #fecha_liquida
                        " A.estado_solicitud         , ",
                        " ' '                        , ", #descripcion_status
                        " A.folio                    , ", #folio_int
                        " A.consecutivo              , ",
                        " A.usuario_captura          , ",
                        --" A.usuario_modifica         , ",
                        " A.usuario_confirma         , ",
            " A.fecha_envio              , ", #fecha_envio
              " A.diag_registro,",
              " D.estado_sub_viv ,",
              " A.fecha_ini_pen ",
                 " FROM   ret_solicitud_tx A,",
             "afi_mae_afiliado B,",
                 "OUTER ret_monto_viv D ",
                 " WHERE  ",x_busca CLIPPED             ,
                 " AND    A.nss    = B.n_seguro        ",
                 " AND    A.tipo_prestacion  = 3       ",
                 " AND   A.nss         = D.nss  ",
                 " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_2 FROM txt_2
    DECLARE cur_2 CURSOR FOR pre_2
    LET i = 1
    FOREACH cur_2 INTO ga_solicitud[i].*
         IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].n_seguro
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT n_rfc     ,
               paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].n_rfc,
               ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].n_seguro

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = B.consecutivo
        AND    B.estado_solicitud = reg_1.liquidado
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        {SELECT diag_registro
        INTO   ga_solicitud[i].diag_datamart
        FROM   ret_solicitud_tx
        WHERE  consecutivo = ga_solicitud[i].consecutivo
        }

        SELECT descripcion
        INTO   ga_solicitud[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = ga_solicitud[i].tipo_prestacion

        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud  = ga_solicitud[i].estado_solicitud

        SELECT diag_datamart
        INTO   ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].n_seguro
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_prestacion  = 3
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension
        AND    folio            = ga_solicitud[i].folio

        IF SQLCA.SQLCODE = NOTFOUND THEN

           SELECT diag_datamart
           INTO   ga_solicitud[i].diag_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].n_seguro
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN("CV","IM")
           AND    tipo_prestacion  = 3
           AND    diag_datamart    in (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension
           AND    folio            = ga_solicitud[i].folio
          IF SQLCA.SQLCODE <> NOTFOUND THEN
             ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
             SLEEP 2
          ELSE
             SELECT diag_registro
             INTO   ga_solicitud[i].diag_datamart
             FROM   ret_solicitud_tx
             WHERE  nss         = ga_solicitud[i].n_seguro
             AND    consecutivo = ga_solicitud[i].consecutivo

             SELECT rechazo_cod
             INTO   ga_solicitud[i].referencia
             FROM   ret_solicitud_tx
             WHERE  nss         = ga_solicitud[i].n_seguro
             AND    consecutivo = ga_solicitud[i].consecutivo

          END IF
        ELSE
           ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
           ATTRIBUTE(REVERSE)
        END IF

        LET i = i + 1

     END FOREACH

     IF i = 1 THEN
         INITIALIZE rg_datamart.* TO NULL
         CLEAR FORM
         ERROR "    NO EXISTE REGISTRO "
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8023
         RETURN
     END IF

     CALL SET_COUNT(i-1)

     ERROR ""

     DISPLAY ARRAY ga_solicitud TO scr_1.*

         ON KEY ( CONTROL-B )

           LET pos = ARR_CURR()
           SELECT "OK"
           FROM   ret_beneficiario
           WHERE  nss         =  ga_solicitud[pos].n_seguro
           AND    consecutivo =  ga_solicitud[pos].consecutivo
           GROUP BY 1

           IF STATUS <> NOTFOUND THEN
               LET ejecuta="fglgo RETM810 ",ga_solicitud[pos].n_seguro," ",
                                              ga_solicitud[pos].consecutivo," ",
                                              "C"
               RUN ejecuta
           ELSE
               ERROR " NO SE ENCONTRARON BENEFICIARIOS ..."
               ATTRIBUTE(REVERSE) SLEEP 2
               EXIT DISPLAY
           END IF

         ON KEY ( CONTROL-C )
             CALL inicializa()
             EXIT DISPLAY

         ON KEY ( INTERRUPT )
             CALL inicializa()
             EXIT DISPLAY

     END DISPLAY
     CLOSE WINDOW retm8023
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8024 AT 2,3 WITH FORM "RETM8023" ATTRIBUTE (BORDER)
    DISPLAY " RETM802                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Modifica   Ctrl-C : Salir   Ctrl-B : Beneficiarios" AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 1 ## modifica

    IF vaccion = "M" THEN

        IF int_flag = TRUE  THEN
            LET INT_FLAG = FALSE
            CLOSE WINDOW retm8024
            RETURN
        END IF

        LET txt_3 = " SELECT A.nss                      ,",
                    "        B.n_rfc                    ,", #n_rfc
                    "        A.curp                     ,",
                    "        B.paterno                  ,", #paterno
                    "        B.materno                  ,", #materno
                    "        B.nombres                  ,", #nombre
                    "        A.tipo_retiro              ,",
                    "        ' '                        ,", #des_tipo_ret
                    "        A.tipo_prestacion          ,", #tipo_prestacion
                    "        ' '                        ,", #desc_prestacion
                    "        A.regimen                  ,",
                    "        A.tipo_seguro              ,",
                    "        ' '                        ,", #desc_tipo_seg
                    "        A.tipo_pension             ,",
                    "        ' '                        ,", #desc_pension
                    "        A.num_resolucion           ,", #num_resolucion
                    "        A.fecha_resolucion         ,", #fecha_resolucion
                    "        A.folio_solicitud          ,", #folio solicitud
                    "        A.fecha_solicitud          ,",
                    "        A.sec_pension              ,", #se toma de aqui
                    "        ' '                        ,", #diag_datamart
                    "        A.semanas_cotizadas        ,", #semanas cotizadas
                    "        A.cod_rechazo_ent          ,",
                    "        ' '                        ,", #referencia
                    "        A.fecha_captura            ,",
                    --"        A.fecha_modifica           ,",
                    "        A.fecha_confirma           ,",
                    "        ' '                        ,", #fecha_liquida
                    "        A.estado_solicitud         ,",
                    "        ' '                        ,", #descripcion_status
                    "        A.folio                    ,", #folio_int
                    "        A.consecutivo              ,",
                    "        A.usuario_captura          ,",
                    --"        A.usuario_modifica         ,",
                    "        A.usuario_confirma         ,",
            "        A.fecha_envio              ,", #fecha_envio
            "        A.diag_registro            ,",
            "        D.estado_sub_viv           ,",
            "        A.fecha_ini_pen             ",
                    " FROM   ret_solicitud_tx A         ,",
            "        afi_mae_afiliado B         ,",
            " OUTER  ret_monto_viv D          ",
                    " WHERE  A.nss = ",vnss               ,
                    " AND    A.nss              = B.n_seguro    ",
                    " AND    A.tipo_prestacion  = 3             ",
                    " AND    A.nss              = D.nss         ",
                    " AND    A.consecutivo      = D.consecutivo "

    ELSE
        CONSTRUCT BY NAME x_busca ON a.nss               ,
                                     b.n_rfc             ,
                                     a.curp              ,
                                     b.paterno           ,
                                     b.materno           ,
                                     b.nombres           ,
                                     a.tipo_retiro       ,
                                     a.tipo_prestacion   ,
                                     a.regimen           ,
                                     a.tipo_seguro       ,
                                     a.tipo_pension      ,
                                     a.fecha_captura     ,
                                     a.fecha_confirma    ,
                                     b.fecha_conversion  ,
                                     a.estado_solicitud  ,
                                     a.folio             ,
                                     a.consecutivo

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF int_flag = TRUE  THEN
            LET INT_FLAG = FALSE
            CLOSE WINDOW retm8024
            RETURN
        END IF

        LET txt_3 =" SELECT A.nss                      ,",
                   "        B.n_rfc                    ,", #n_rfc
                   "        A.curp                     ,",
                   "        B.paterno                  ,", #paterno
                   "        B.materno                  ,", #materno
                   "        B.nombres                  ,", #nombre
                   "        A.tipo_retiro              ,",
                   "        ' '                        ,", #des_tipo_ret
                   "        A.tipo_prestacion          ,", #tipo_prestacion
                   "        ' '                        ,", #desc_prestacion
                   "        A.regimen                  ,",
                   "        A.tipo_seguro              ,",
                   "        ' '                        ,", #desc_tipo_seg
                   "        A.tipo_pension             ,",
                   "        ' '                        ,", #desc_pension
                   "        A.num_resolucion           ,", #num_resolucion
                   "        A.fecha_resolucion         ,", #fecha_resolucion
                   "        A.folio_solicitud          ,", #folio solicitud
                   "        A.fecha_solicitud          ,",
                   "        A.sec_pension              ,", #sec_pension
                   "        ' '                        ,", #diag_datamart
                   "        A.semanas_cotizadas        ,", #semanas cotizadas
                   "        A.cod_rechazo_ent          ,",
                   "        ' '                        ,", #referencia
                   "        A.fecha_captura            ,",
                   --"        A.fecha_modifica           ,",
                   "        A.fecha_confirma           ,",
                   "        ' '                        ,", #fecha_liquida
                   "        A.estado_solicitud         ,",
                   "        ' '                        ,", #descripcion_status
                   "        A.folio                    ,", #folio_int
                   "        A.consecutivo              ,",
                   "        A.usuario_captura          ,",
                   --"        A.usuario_modifica         ,",
                   "        A.usuario_confirma         ,",
               "        A.fecha_envio              ,", #fecha_envio
           "        A.diag_registro            ,",
           "        D.estado_sub_viv           ,",
           "        A.fecha_ini_pen             ",
                   " FROM   ret_solicitud_tx A         ,",
           "        afi_mae_afiliado B         ,",
           " OUTER  ret_monto_viv D          ",
                   " WHERE  ",x_busca CLIPPED            ,
                   " AND    A.nss    = B.n_seguro       ",
                   " AND    A.tipo_prestacion  = 3      ",
                   " AND    A.nss              = D.nss  ",
                   " AND    A.consecutivo      = D.consecutivo  "
    END IF

    PREPARE pre_3 FROM txt_3
    DECLARE cur_3 CURSOR FOR pre_3
    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_3 INTO ga_solicitud[i].*
        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].n_seguro
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

      IF ga_solicitud[i].estado_solicitud <> reg_1.capturado THEN

     PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].n_seguro,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
           FOR opc ATTRIBUTE(REVERSE)
     CONTINUE FOREACH
      END IF

        SELECT paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].n_seguro

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = ga_solicitud[i].tipo_prestacion

        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = B.consecutivo
        AND    A.estado   = reg_1.liquidado
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        SELECT diag_datamart
        INTO   ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].n_seguro
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_prestacion  = 3
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension
        AND    folio            = ga_solicitud[i].folio

        IF SQLCA.SQLCODE = NOTFOUND THEN

           SELECT diag_datamart
           INTO   ga_solicitud[i].diag_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].n_seguro
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN("CV","IM")
           AND    tipo_prestacion  = 3
           AND    diag_datamart    in (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension
           AND    folio            = ga_solicitud[i].folio
           IF SQLCA.SQLCODE <> NOTFOUND THEN
              ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
              SLEEP 2
           ELSE
              SELECT diag_registro
              INTO   ga_solicitud[i].diag_datamart
              FROM   ret_solicitud_tx
              WHERE  consecutivo = ga_solicitud[i].consecutivo

              SELECT rechazo_cod
              INTO   ga_solicitud[i].referencia
              FROM   ret_solicitud_tx
              WHERE  nss         = ga_solicitud[i].n_seguro
              AND    consecutivo = ga_solicitud[i].consecutivo
            END IF
         ELSE
            ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
            ATTRIBUTE(REVERSE)
            display ga_solicitud[i].diag_datamart to diag_datamart
            display ga_solicitud[i].sec_pension   to sec_pension
         END IF

        LET i = i + 1

    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY ga_solicitud TO scr_1.*

             ON KEY ( CONTROL-C )
                LET int_flag=TRUE
                EXIT DISPLAY

      ON KEY (CONTROL-T)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].n_seguro CLIPPED," ",
                         ga_solicitud[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
             ON KEY ( INTERRUPT )
                LET int_flag=TRUE
                EXIT DISPLAY

             ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                EXIT DISPLAY
         END DISPLAY
     ELSE
         ERROR "    NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8024
         RETURN
     END IF

     IF int_flag = TRUE THEN
         LET int_flag=FALSE
         CLOSE WINDOW retm8024
         RETURN
     END IF

     CALL construccion(ga_solicitud[pos].*,sw_2) #co

CLOSE WINDOW retm8024
END FUNCTION

FUNCTION elimina()
#e-----------------
    DEFINE #loc #smallint
        arr            ,
        src            SMALLINT

    OPEN WINDOW retm8025 AT 2,3 WITH FORM "RETM8023" ATTRIBUTE (BORDER)
    DISPLAY " RETM802                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER> : Elimina    Ctrl-C : Salir   Ctrl-B : Beneficiario" AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON a.nss               ,
                                 b.n_rfc             ,
                                 a.curp              ,
                                 b.paterno           ,
                                 b.materno           ,
                                 b.nombres           ,
                                 a.tipo_retiro       ,
                                 a.tipo_prestacion   ,
                                 a.regimen           ,
                                 a.tipo_seguro       ,
                                 a.tipo_pension      ,
                                 a.fecha_captura     ,
                                 a.fecha_confirma    ,
                               ##b.fecha_conversion  ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE  THEN
            LET int_flag = FALSE
       CLOSE WINDOW retm8025
       RETURN
    END IF
    LET   txt_4 =" SELECT A.nss                      , ",
                        " B.n_rfc                    , ", #n_rfc
                        " A.curp                     , ",
                        " B.paterno                  , ", #paterno
                        " B.materno                  , ", #materno
                        " B.nombres                  , ", #nombres
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_prestacion          , ",
                        " ' '                        , ", #desc_prestacion
                        " A.regimen                  , ",
                        " A.tipo_seguro              , ",
                        " ' '                        , ", #desc_tipo_seg
                        " A.tipo_pension             , ",
                        " ' '                        , ", #desc_pension
                        " A.num_resolucion           , ",
                        " A.fecha_resolucion         , ",
                        " A.folio_solicitud          , ",
                        " A.fecha_solicitud          , ",
                        " A.sec_pension              , ", #sec_pension
                        " ' '                        , ", #diag_datamart
                        " A.semanas_cotizadas        , ",
                        " A.cod_rechazo_ent          , ",
                        " A.rechazo_cod              , ", #referencia
                        " A.fecha_captura            , ",
                        --" A.fecha_modifica           , ",
                        " A.fecha_confirma           , ",
                        " ' '                        , ", #fecha_liquida
                        " A.estado_solicitud         , ",
                        " ' '                        , ", #descripcion_status
                        " A.folio                    , ",
                        " A.consecutivo              , ",
                        " A.usuario_captura          , ",
                        --" A.usuario_modifica         , ",
                        " A.usuario_confirma         , ",
                        " A.fecha_envio              , ", #fecha_envio
                        " A.diag_registro            , ",
                        " D.estado_sub_viv           , ",
                        " A.fecha_ini_pen              ",
                   " FROM   ret_solicitud_tx A,",
                           "afi_mae_afiliado B,",
                   " OUTER ret_monto_viv D ",
                 " WHERE  ",x_busca CLIPPED             ,
                 " AND    A.nss    = B.n_seguro        ",
                 " AND    A.tipo_prestacion  = 3       ",
                 " AND   A.nss         = D.nss  ",
                 " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_4 FROM txt_4
    DECLARE cur_4 CURSOR FOR pre_4
    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_4 INTO ga_solicitud[i].*
        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].n_seguro
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ((ga_solicitud[i].estado_solicitud <> reg_1.capturado) AND 
            (ga_solicitud[i].estado_solicitud <> reg_1.precapturado)  ) THEN

            PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].n_seguro,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].n_seguro

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = ga_solicitud[i].tipo_prestacion

        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = B.consecutivo
        AND    A.estado   = reg_1.liquidado
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        SELECT diag_datamart
        INTO   ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].n_seguro
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_prestacion  = 3
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension
        AND    folio            = ga_solicitud[i].folio

        IF SQLCA.SQLCODE = NOTFOUND THEN

           SELECT diag_datamart
           INTO   ga_solicitud[i].diag_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].n_seguro
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN("CV","IM")
           AND    tipo_prestacion  = 3
           AND    diag_datamart    in (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension
           AND    folio            = ga_solicitud[i].folio
           IF SQLCA.SQLCODE <> NOTFOUND THEN
              ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
              SLEEP 2
           ELSE
              SELECT diag_registro
              INTO   ga_solicitud[i].diag_datamart
              FROM   ret_solicitud_tx
              WHERE  nss         = ga_solicitud[i].n_seguro
              AND    consecutivo = ga_solicitud[i].consecutivo

              SELECT rechazo_cod
              INTO   ga_solicitud[i].referencia
              FROM   ret_solicitud_tx
              WHERE  nss         = ga_solicitud[i].n_seguro
              AND    consecutivo = ga_solicitud[i].consecutivo

           END IF
        ELSE
           ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
           ATTRIBUTE(REVERSE)
        END IF

        LET i = i + 1

     END FOREACH

     ERROR ""
     IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY ga_solicitud TO scr_1.*

              ON KEY ( CONTROL-C )
                  LET int_flag=1
                  EXIT DISPLAY

              ON KEY ( INTERRUPT )
                  LET int_flag=1
                  EXIT DISPLAY

              ON KEY ( CONTROL-B )

                  LET      arr_c           =  arr_curr()
                  LET pos = ARR_CURR()
                  SELECT "OK"
                  FROM   ret_beneficiario
                  WHERE  nss         =  ga_solicitud[pos].n_seguro
                  AND    consecutivo =  ga_solicitud[pos].consecutivo
                  GROUP BY 1

                  IF STATUS <> NOTFOUND THEN
                     LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].n_seguro CLIPPED,
                      " ", ga_solicitud[arr_c].consecutivo CLIPPED,
                      " ",'C' CLIPPED
                     RUN V_ejecuta
                  ELSE
                      ERROR " NO SE ENCONTRARON BENEFICIARIOS ..."
                      ATTRIBUTE(REVERSE) SLEEP 2
                      EXIT DISPLAY
                  END IF

              ON KEY ( CONTROL-M )
                  LET pos = ARR_CURR()
                  WHILE TRUE
                      PROMPT "DESEAS ELIMINAR EL REGISTRO S/N ? " FOR opc
                          IF opc NOT MATCHES "[SsNn]" THEN
                              CONTINUE WHILE
                          ELSE
                              EXIT WHILE
                          END IF
                  END WHILE

                  IF opc MATCHES "[Nn]"  THEN
                      ERROR " REGISTRO CANCELADO "
                      ATTRIBUTE(REVERSE)
                      SLEEP 2
                      CALL inicializa() #i
                      LET int_flag = 1
                      EXIT DISPLAY
                  END IF

                  LET arr = ARR_CURR()
                  LET src = SCR_LINE()

          WHENEVER ERROR CONTINUE
                  DELETE FROM ret_beneficiario
                  WHERE  nss = ga_solicitud[arr].n_seguro
                  AND  consecutivo = ga_solicitud[arr].consecutivo
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_beneficiario:",
                   "nss ",ga_solicitud[arr].n_seguro,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

             PROMPT " ERROR DE BORRADO ret_beneficiario AVISE A SISTEMAS"
             FOR enter
             EXIT PROGRAM
                  END IF
          WHENEVER ERROR STOP

          WHENEVER ERROR CONTINUE
                  DELETE FROM ret_solicitud_tx
                  WHERE  nss = ga_solicitud[arr].n_seguro
                  AND  consecutivo = ga_solicitud[arr].consecutivo
                  AND    estado_solicitud = 0
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_beneficiario:",
                   "nss ",ga_solicitud[arr].n_seguro,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

             PROMPT " ERROR DE BORRADO ret_beneficiario AVISE A SISTEMAS"
             FOR enter
             EXIT PROGRAM
                  END IF

                  WHENEVER ERROR STOP
                  ##### DESMARCAJE ##################################

                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = "D"

                  LET v_marca_ent     = s_tipo_movimiento
                  LET vestado_marca   = 40
                  LET vcodigo_rechazo = 0
                  LET vmarca_causa    = 0
                  LET vfecha_causa    = NULL

                  LET v_desmarca   = " EXECUTE PROCEDURE desmarca_cuenta('",
                                     ga_solicitud[arr].n_seguro,"',",
                                     v_marca_ent,",",
                                     ga_solicitud[arr].consecutivo,",",
                     vestado_marca,",",
                     vmarca_causa,",' ",
                     usuario,"')"
                  PREPARE eje_reversa_mar FROM v_desmarca
                  EXECUTE eje_reversa_mar

                  ####################################################


                  ERROR "REGISTRO ELIMINADO ",""
                  ATTRIBUTE(REVERSE)
                  EXIT DISPLAY
         END DISPLAY
     ELSE
         ERROR " NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         CLOSE WINDOW retm8025
         RETURN
     END IF

     IF int_flag = 1 THEN
         CLOSE WINDOW retm8025
         RETURN
         LET int_flag=0
     END IF

CLOSE WINDOW retm8025
END FUNCTION

FUNCTION confirma()
#cf----------------
    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8025 AT 2,3 WITH FORM "RETM8023" ATTRIBUTE (BORDER)
    DISPLAY " RETM802                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONFIRMA CAPTURA" AT 1,59 ATTRIBUTE(REVERSE)
    DISPLAY "                 DATOS DE LA SOLICITUD NEGATIVA DE PENSION                     " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 15,1 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER>:Selecciona         Ctrl-C :Salir          Ctrl-B :Beneficiario" AT 2,1
    LET int_flag = FALSE
    LET sw_2 = 2  #confirma
    CLEAR FORM

    CONSTRUCT BY NAME x_busca ON a.nss               ,
                                 b.n_rfc             ,
                                 a.curp              ,
                                 b.paterno           ,
                                 b.materno           ,
                                 b.nombres           ,
                                 a.tipo_retiro       ,
                                 a.tipo_prestacion   ,
                                 a.regimen           ,
                                 a.tipo_seguro       ,
                                 a.tipo_pension      ,
                                 a.fecha_captura     ,
                                 a.fecha_confirma    ,
                                 ##b.fecha_conversion  ,
                                 a.estado_solicitud  ,
                                 a.folio             ,
                                 a.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE  THEN
            LET int_flag = FALSE
       CLOSE WINDOW retm8025
       RETURN
    END IF

    LET   txt_5 =" SELECT A.nss                      , ",
                        " B.n_rfc                    , ", #n_rfc
                        " A.curp                     , ",
                        " B.paterno                  , ", #paterno_afore
                        " B.materno                  , ", #materno_afore
                        " B.nombres                  , ", #nombre_afore
                        " A.tipo_retiro              , ",
                        " ' '                        , ", #des_tipo_ret
                        " A.tipo_prestacion          , ", #tipo_prestacion
                        " ' '                        , ", #desc_prestacion
                        " A.regimen                  , ",
                        " A.tipo_seguro              , ",
                        " ' '                        , ", #desc_tipo_seg
                        " A.tipo_pension             , ",
                        " ' '                        , ", #desc_pension
                        " A.num_resolucion           , ",
                        " A.fecha_resolucion         , ",
                        " A.folio_solicitud          , ", #folio solicitud
                        " A.fecha_solicitud          , ",
                        " A.sec_pension              , ", #sec_pension
                        " ' '                        , ", #diag_datamart
                        " A.semanas_cotizadas        , ", #semanas cotizadas
                        " A.cod_rechazo_ent          , ", # sil
                        " ' '                        , ", #referencia
                        " A.fecha_captura            , ",
                        --" A.fecha_modifica           , ",
                        " A.fecha_confirma           , ",
                        " ' '                        , ", #fecha_liquida
                        " A.estado_solicitud         , ",
                        " ' '                        , ", #descripcion_status
                        " A.folio                    , ", #folio_int
                        " A.consecutivo              , ",
                        " A.usuario_captura          , ",
                        --" A.usuario_modifica         , ",
                        " A.usuario_confirma         , ",
                        " A.fecha_envio              , ",
                        " A.diag_registro            ,",
                        " D.estado_sub_viv           ,",
                        " A.fecha_ini_pen             ",
                        " FROM   ret_solicitud_tx A  ,",
                        "        afi_mae_afiliado B  ,",
                    " OUTER  ret_monto_viv D   ",
                    " WHERE  ",x_busca CLIPPED ,
                    " AND    A.estado_solicitud IN (?,?) ",
                    " AND    A.nss               = B.n_seguro ",
                    " AND    A.tipo_prestacion   = 3 ",
                    " AND    A.nss               = D.nss ",
                    " AND    A.consecutivo       = D.consecutivo  "

    PREPARE pre_8 FROM txt_5
    DECLARE cur_8 CURSOR FOR pre_8
    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_8 USING reg_1.precapturado, 
                        reg_1.capturado
                  INTO ga_solicitud[i].*

        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].n_seguro
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT usuario_captura
        INTO   x_usuario
        FROM   ret_solicitud_tx
        WHERE  nss         = ga_solicitud[i].n_seguro
        AND    consecutivo = ga_solicitud[i].consecutivo

        { IF x_usuario = usuario THEN
            PROMPT "  USUARIO ES EL MISMO DE CAPTURA " ATTRIBUTE (REVERSE)
            FOR opc ATTRIBUTE (REVERSE)
            LET sw = 1
            EXIT FOREACH
        END IF }

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ((ga_solicitud[i].estado_solicitud <> reg_1.capturado) AND 
            (ga_solicitud[i].estado_solicitud <> reg_1.precapturado)  ) THEN

            PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].n_seguro,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].n_seguro

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = ga_solicitud[i].tipo_retiro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = ga_solicitud[i].tipo_prestacion

        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT A.fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta A, ret_solicitud_tx B
        WHERE  A.nss      = ga_solicitud[i].n_seguro
        AND    B.nss      = A.nss
        AND    A.consecutivo_lote = B.consecutivo
        AND    A.estado   = reg_1.liquidado
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        SELECT diag_datamart
        INTO   ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].n_seguro
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_prestacion  = 3
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension
        AND    folio            = ga_solicitud[i].folio

        IF SQLCA.SQLCODE = NOTFOUND THEN
           SELECT diag_datamart
           INTO   ga_solicitud[i].diag_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].n_seguro
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN("CV","IM")
           AND    tipo_prestacion  = 3
           AND    diag_datamart    in (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension
           AND    folio            = ga_solicitud[i].folio
           IF SQLCA.SQLCODE <> NOTFOUND THEN
              ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
              SLEEP 2
           ELSE
              SELECT diag_registro
              INTO   ga_solicitud[i].diag_datamart
              FROM   ret_solicitud_tx
              WHERE  nss         = ga_solicitud[i].n_seguro
              AND    consecutivo = ga_solicitud[i].consecutivo

              SELECT rechazo_cod
              INTO   ga_solicitud[i].referencia
              FROM   ret_solicitud_tx
              WHERE  nss         = ga_solicitud[i].n_seguro
              AND    consecutivo = ga_solicitud[i].consecutivo
            END IF
         ELSE
            ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
            ATTRIBUTE(REVERSE)
            display ga_solicitud[i].diag_datamart to diag_datamart
            display ga_solicitud[i].sec_pension   to sec_pension
         END IF


        LET i = i + 1

    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY ga_solicitud TO scr_1.*

             ON KEY ( CONTROL-C )
                LET int_flag=TRUE
                EXIT DISPLAY

             ON KEY ( INTERRUPT )
                LET int_flag=TRUE
                EXIT DISPLAY

             ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                EXIT DISPLAY

           ON KEY (CONTROL-B)
              LET      arr_c           =  arr_curr()
             LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].n_seguro CLIPPED,
                         " ",ga_solicitud[arr_c].consecutivo CLIPPED," ",
                 'C' CLIPPED
             RUN v_ejecuta

        END DISPLAY

     ELSE
         ERROR "    NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8025
         RETURN
     END IF

     IF int_flag = TRUE THEN
     LET int_flag = FALSE
         CLOSE WINDOW retm8025
         RETURN
     END IF

     CALL construccion(ga_solicitud[pos].*,sw_2) #co

CLOSE WINDOW retm8025
END FUNCTION

FUNCTION construccion(reg_mod,sw_3)
#co--------------------------------
    DEFINE  reg_mod        RECORD #loc #reg_mod
        nss                LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_retiro        CHAR(01)                               ,
        des_tipo_ret       CHAR(60)                               ,
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion  ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        num_resolucion     INTEGER                                ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    DATE                                   ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        semanas_cotizadas  SMALLINT                               ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        referencia         CHAR(12)                               ,
        fecha_captura      DATE                                   ,
        --fecha_modificacion DATE                                   ,
        fecha_confirma     DATE                                   ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        descripcion_status CHAR(40)                               ,
        folio_int          INTEGER                                ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        --usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma,
        fecha_envio        DATE                                   ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv,
        fecha_ini_pen      LIKE ret_solicitud_tx.fecha_ini_pen
    END RECORD

    DEFINE  reg        RECORD #loc #reg
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion  ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension
    END RECORD

    DEFINE #loc #smallint
        sw_3               SMALLINT

    IF sw_3  = 2  THEN
        DISPLAY " ESc: Confirma   Ctr-F: Rechazar   Ctrl-C : Salir   Ctrl-B Beneficiarios" AT 2,1
    END IF

    LET reg.tipo_prestacion = reg_mod.tipo_prestacion
    LET reg.tipo_seguro     = reg_mod.tipo_seguro
    LET reg.tipo_pension    = reg_mod.tipo_pension

    INPUT BY NAME      reg_mod.nss                            ,
                       reg_mod.tipo_seguro                    ,
                       reg_mod.tipo_pension                   ,
                       reg_mod.num_resolucion                 ,
                       reg_mod.fecha_resolucion               ,
                       reg_mod.sol_folio                      ,
                       reg_mod.fecha_solicitud                ,
                       -- reg_mod.sec_pension                    ,
                       reg_mod.semanas_cotizadas  WITHOUT DEFAULTS


        BEFORE FIELD nss
            DISPLAY reg_mod.tipo_seguro  TO tipo_seguro
            DISPLAY reg_mod.desc_seguro  TO desc_seguro
            DISPLAY reg_mod.tipo_pension TO tipo_pension
            DISPLAY reg_mod.desc_pension TO desc_pension
            DISPLAY reg_mod.referencia   TO referencia
            NEXT FIELD tipo_seguro

        AFTER FIELD nss
            IF reg_mod.nss IS NULL THEN
                ERROR " EL CAMPO NSS NO PUDE SER NULO "
                ATTRIBUTE(REVERSE)
                NEXT FIELD nss
            END IF

        AFTER FIELD tipo_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_seguro
            END IF
            IF reg_mod.tipo_seguro IS NULL THEN
                 CALL despliega_tipo_seguro() #dts
                 LET reg_mod.tipo_seguro = rg_datamart.tipo_seguro
                 LET reg_mod.desc_seguro = rg_datamart.desc_seguro
                 DISPLAY reg_mod.tipo_seguro TO tipo_seguro
                 DISPLAY reg_mod.desc_seguro TO desc_seguro

                 NEXT FIELD  tipo_pension
            ELSE
                SELECT descripcion
                INTO   reg_mod.desc_seguro
                FROM   tab_seguro
                WHERE  clave = reg_mod.tipo_seguro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE SEGURO    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_seguro() #dts
                                LET reg_mod.tipo_seguro = rg_datamart.tipo_seguro
                                LET reg_mod.desc_seguro = rg_datamart.desc_seguro
                                DISPLAY reg_mod.tipo_seguro TO tipo_seguro
                                DISPLAY reg_mod.desc_seguro TO desc_seguro
                                NEXT FIELD tipo_pension
                            ELSE
                                NEXT FIELD tipo_seguro
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg_mod.desc_seguro TO desc_seguro

        AFTER FIELD tipo_pension
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_seguro
            END IF
            IF reg_mod.tipo_seguro = 'CV' THEN
                SELECT descripcion
                INTO   reg_mod.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = reg_mod.tipo_pension
                AND    tipo_pension IN ('CE','VE')
            ELSE
                SELECT descripcion
                INTO   reg_mod.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = reg_mod.tipo_pension
                AND    tipo_pension IN ('AS','OR','VI','VO','IN')
            END IF
            IF reg_mod.tipo_pension IS NULL OR
                reg_mod.tipo_pension = 0     THEN
                CALL despliega_tipo_pension() #dtp
                LET reg_mod.tipo_pension = rg_datamart.tipo_pension
                LET reg_mod.desc_pension = rg_datamart.desc_pension
                DISPLAY reg_mod.tipo_pension TO tipo_pension
                DISPLAY reg_mod.desc_pension TO desc_pension
            END IF

             { SELECT MAX(sec_pension)
             INTO   vmax_sec_pension
             FROM   ret_det_datamart
             WHERE  nss              = reg_mod.nss
             AND    fecha_ini_pen    = reg_mod.fecha_ini_pen
             AND    tipo_pension     = reg_mod.tipo_pension
             AND    tipo_seguro      = reg_mod.tipo_seguro
             AND    tipo_prestacion  = 3
             AND    diag_datamart    in (101,300,301,210,302,303) }

            SELECT diag_datamart
            INTO   reg_mod.diag_datamart
            FROM   ret_det_datamart
            WHERE  nss              = reg_mod.nss
            AND    fecha_ini_pen    = reg_mod.fecha_ini_pen
            AND    tipo_pension     = reg_mod.tipo_pension
            AND    tipo_seguro      = reg_mod.tipo_seguro
            AND    tipo_prestacion  = 3
            AND    diag_datamart    in (101,300,301,210,302,303)
            AND    sec_pension      = reg_mod.sec_pension

            IF SQLCA.SQLCODE = NOTFOUND THEN
                 { SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = reg_mod.nss
                AND    tipo_seguro      IN ("CV","IM")
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303) }

                SELECT diag_datamart
                INTO   reg_mod.diag_datamart
                FROM   ret_det_datamart
                WHERE  nss              = reg_mod.nss
                AND    fecha_ini_pen    = reg_mod.fecha_ini_pen
                AND    tipo_seguro      IN("CV","IM")
                AND    tipo_prestacion  = 3
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = reg_mod.sec_pension
                IF SQLCA.SQLCODE <> NOTFOUND THEN
                   ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
                    SLEEP 2
                    NEXT FIELD tipo_seguro
                ELSE
                   ERROR " NO SE ENCONTRO REGISTRO EN EL DATAMART .."
                   ATTRIBUTE(REVERSE)
                   SLEEP 2
                   SELECT diag_registro
                   INTO   reg_mod.diag_datamart
                   FROM   ret_solicitud_rx
                   WHERE  consecutivo = reg_mod.consecutivo

                   SELECT rechazo_cod
                   INTO   reg_mod.referencia
                   FROM   ret_solicitud_tx
                   WHERE  nss         = reg_mod.nss
                   AND    consecutivo = reg_mod.consecutivo

                END IF
            ELSE
                ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
                ATTRIBUTE(REVERSE)
            END IF

            IF (reg_mod.tipo_seguro  =  'CV' AND
                reg_mod.tipo_pension <> 'AS' AND
                reg_mod.tipo_pension <> 'OR' AND
                reg_mod.tipo_pension <> 'VI' AND
                reg_mod.tipo_pension <> 'VO' AND
                reg_mod.tipo_pension <> 'IN') OR
               (reg_mod.tipo_seguro  = 'IM'  AND
                reg_mod.tipo_pension <> 'CE' AND
                reg_mod.tipo_pension <> 'VE') THEN
               NEXT FIELD num_resolucion
            ELSE
               ERROR " COMBINACION INVALIDA... " ATTRIBUTE(REVERSE)
               NEXT FIELD tipo_pension
            END IF

        AFTER FIELD num_resolucion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_pension
            END IF
            NEXT FIELD sol_folio

        AFTER FIELD sol_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_resolucion
            END IF
        AFTER FIELD fecha_solicitud
            IF reg_mod.fecha_solicitud IS NULL THEN
                ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
                ATTRIBUTE(REVERSE)
                NEXT FIELD  fecha_solicitud
            END IF

                IF reg_mod.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

            IF HOY <> reg_mod.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF reg_mod.fecha_solicitud < xx_fecha_solicitud THEN
          WHILE TRUE
                     PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO,DESEA CONTINUAR S/N " FOR opc
                 IF opc MATCHES "[SsNn]" THEN
                EXIT WHILE
                     END IF
                  END WHILE

              IF opc MATCHES "[Nn]" THEN
             EXIT INPUT
                  END IF
               END IF
        END IF
            IF  reg_mod.fecha_resolucion >
                reg_mod.fecha_solicitud  THEN
                ERROR "LA FECHA DE RESOLUCION NO PUEDE SER MAYOR A LA FECHA DE",
                      " SOLICITUD" ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_solicitud
            END IF
        {
            CALL habil_siguiente(reg_mod.fecha_solicitud,3)
            RETURNING fecha_max_habil
            IF fecha_max_habil < TODAY THEN
                ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_solicitud
            END IF
        }
            NEXT FIELD semanas_cotizadas

             ON KEY (CONTROL-F)
             IF xaccion = "F" THEN
                LET pos = ARR_CURR()
        ------------------------sil---------------------
               CALL rechazar(reg_mod.nss,reg_mod.consecutivo,
                   reg_mod.tipo_prestacion,reg_mod.tipo_retiro)
               RETURNING codigo,entidad
               LET reg_mod.cod_rechazo_ent = codigo
               DISPLAY reg_mod.cod_rechazo_ent TO cod_rechazo_ent
               LET sw_3 = 3
             END IF
        ------------------------------------------------
--miguel
        ON KEY ( ESC )

           IF reg_mod.nss IS NULL THEN
               ERROR " EL CAMPO NSS NO PUEDE SER NULO "
               ATTRIBUTE(REVERSE)
               NEXT FIELD nss
           END IF

           IF reg_mod.tipo_seguro IS NULL OR
              reg_mod.tipo_seguro = 0     THEN
              CALL despliega_tipo_seguro() #dtp
              LET reg_mod.tipo_seguro = rg_datamart.tipo_seguro
              LET reg_mod.desc_seguro = rg_datamart.desc_seguro
              DISPLAY reg_mod.tipo_seguro TO tipo_seguro
              DISPLAY reg_mod.desc_seguro TO desc_seguro
           END IF

           IF reg_mod.tipo_pension IS NULL OR
               reg_mod.tipo_pension = 0     THEN
               CALL despliega_tipo_pension() #dtp
               LET reg_mod.tipo_pension = rg_datamart.tipo_pension
               LET reg_mod.desc_pension = rg_datamart.desc_pension
               DISPLAY reg_mod.tipo_pension TO tipo_pension
               DISPLAY reg_mod.desc_pension TO desc_pension
           END IF

           IF reg_mod.fecha_solicitud IS NULL THEN
               ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
               ATTRIBUTE(REVERSE)
               NEXT FIELD  fecha_solicitud
           END IF

                IF reg_mod.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

            IF HOY <> reg_mod.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF reg_mod.fecha_solicitud < xx_fecha_solicitud THEN
          WHILE TRUE
                     PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO,DESEA CONTINUAR S/N " FOR opc
                 IF opc MATCHES "[SsNn]" THEN
                EXIT WHILE
                     END IF
                  END WHILE

              IF opc MATCHES "[Nn]" THEN
             EXIT INPUT
                  END IF
               END IF
        END IF
           IF  reg_mod.fecha_resolucion >
               reg_mod.fecha_solicitud  THEN
               ERROR "LA FECHA DE RESOLUCION NO PUEDE SER MAYOR A LA FECHA DE",
                     " SOLICITUD" ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_solicitud
           END IF
       {
           CALL habil_siguiente(reg_mod.fecha_solicitud,3)
           RETURNING fecha_max_habil
           IF fecha_max_habil < TODAY THEN
               ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
               NEXT FIELD fecha_solicitud
           END IF }

           IF reg.tipo_seguro        <> reg_mod.tipo_seguro
              OR reg.tipo_pension    <> reg_mod.tipo_pension
              OR reg.tipo_prestacion <> reg_mod.tipo_prestacion THEN


                ##### DESMARCAJE ##################################

                SELECT movimiento
                INTO   s_tipo_movimiento
                FROM   tab_retiro
                WHERE  tipo_retiro = "D"

                LET v_marca_ent  = s_tipo_movimiento

                LET v_desmarca   = " EXECUTE PROCEDURE reversa_marca('",
                                   reg_mod.nss,"',",
                                   v_marca_ent,",",
                                   reg_mod.consecutivo,")"
                PREPARE eje_reversa_mar3 FROM v_desmarca
                EXECUTE eje_reversa_mar3

                ####################################################

                #####INICIO PROCESO DE MARCAJE DE CUENTAS###########

                CALL marca_cuenta (reg_mod.nss          ,
                                   s_tipo_movimiento    ,
                                   reg_mod.consecutivo
                                  )#mc
                RETURNING v_marca_res   ,
                          v_cod_rechazo

                LET reg_mod.referencia = v_cod_rechazo
                IF v_cod_rechazo > 0 THEN
                    SELECT A.rechazo_desc
                    INTO   desc_status_rech
                    FROM   tab_rch_marca A
                    WHERE  A.rechazo_cod = v_cod_rechazo

                    IF sw_3 = 1    THEN
                        CURRENT WINDOW IS retm8024
                    ELSE
                        CURRENT WINDOW IS retm8025
                    END IF

                    PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                           desc_status_rech,") <ENTER> CONTINUAR" FOR CHAR enter

                END IF

                ##########FIN MARCAJE DE CUENTAS######################
            ELSE
                SELECT rechazo_cod
                INTO   reg_mod.referencia
                FROM   ret_solicitud_tx
                WHERE  nss              = reg_mod.nss
                AND    consecutivo      = reg_mod.consecutivo
                AND    estado_solicitud IN (0,3)
                AND    rechazo_cod      > 0

                IF STATUS <> NOTFOUND THEN
                    SELECT A.rechazo_desc
                    INTO   desc_status_rech
                    FROM   tab_rch_marca A
                    WHERE  A.rechazo_cod = reg_mod.referencia

                    IF sw_3 = 1    THEN
                        CURRENT WINDOW IS retm8024
                    ELSE
                        CURRENT WINDOW IS retm8025
                    END IF
                    LET v_cod_rechazo = reg_mod.referencia

                    PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                           desc_status_rech,") <ENTER> CONTINUAR" FOR CHAR enter
                END IF
            END IF
            EXIT INPUT
        ON KEY ( CONTROL-C )
            EXIT INPUT

      ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",reg_mod.nss CLIPPED," ",
                         reg_mod.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
        ON KEY ( INTERRUPT )
            EXIT INPUT
    END INPUT

     IF int_flag = TRUE THEN
     ERROR "PROCESO CANCELADO"
     LET int_flag = FALSE
         RETURN
     END IF

     IF sw_3 = 1  THEN
     ------------------------------------------------
     -------- Se actualiza el registro modificado ---
         WHILE TRUE
               CURRENT WINDOW IS retm8024

                PROMPT " DESEAS ACTUALIZARLA   S/N ? " FOR opc
                IF opc NOT MATCHES "[SsNn]" THEN
                    CONTINUE WHILE
                ELSE
                    EXIT WHILE
                END IF
         END WHILE

         IF opc MATCHES "[Ss]"  THEN
            WHENEVER ERROR CONTINUE
            UPDATE ret_solicitud_tx
            SET    folio_solicitud  = reg_mod.sol_folio        ,
                   tipo_seguro      = reg_mod.tipo_seguro      ,
                   tipo_pension     = reg_mod.tipo_pension     ,
                   fecha_resolucion = reg_mod.fecha_resolucion ,
                   fecha_solicitud  = reg_mod.fecha_solicitud  ,
                   fecha_modifica   = TODAY                    ,
                   num_resolucion   = reg_mod.num_resolucion   ,
                   semanas_cotizadas= reg_mod.semanas_cotizadas,
                   usuario_modifica = usuario
            WHERE  nss      = reg_mod.nss
            AND  consecutivo      = reg_mod.consecutivo

            IF SQLCA.SQLCODE < 0 THEN
               LET x_error = "UPDATE ret_beneficiario:",
                             "nss ",reg_mod.nss,
                             "consecutivo ",reg_mod.consecutivo,
                             err_get(SQLCA.SQLCODE)
               CALL errorlog(x_error CLIPPED)

               PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS"
               FOR enter
               EXIT PROGRAM
            END IF

            ERROR " REGISTRO MODIFICADO "
            ATTRIBUTE(REVERSE) SLEEP 2

            INITIALIZE reg_mod.* TO NULL
            CALL inicializa()
         ELSE
            ERROR " CONFIRMACION CANCELADA "  ATTRIBUTE(REVERSE)
            SLEEP 2
            INITIALIZE reg_mod.* TO NULL
         END IF
     ---------------- fin sw_3 = 1 ------------------
     ELSE
         IF sw_3 = 2 THEN
            IF reg_mod.estado_solicitud = reg_1.capturado THEN
                ------------------------------------------------
                --------Se confirma la captura  ----------------
                  CURRENT WINDOW IS retm8025
                  WHILE TRUE
                        PROMPT " DESEA CONFIRMAR LA CAPTURA S/N ? " FOR opc
                        IF opc NOT MATCHES "[SsNn]" THEN
                           CONTINUE WHILE
                        ELSE
                           EXIT WHILE
                        END IF
                  END WHILE
                
                  IF opc MATCHES "[Ss]"  THEN
                      WHENEVER ERROR CONTINUE
                      UPDATE ret_solicitud_tx
                      SET    tipo_seguro      = reg_mod.tipo_seguro      ,
                                  tipo_pension     = reg_mod.tipo_pension     ,
                                  num_resolucion   = reg_mod.num_resolucion   ,
                                  folio_solicitud  = reg_mod.sol_folio        ,
                                  fecha_solicitud  = reg_mod.fecha_solicitud  ,
                                  semanas_cotizadas= reg_mod.semanas_cotizadas,
                                  estado_solicitud = reg_1.confirmado         ,
                                  fecha_confirma   = TODAY                    ,
                                  usuario_confirma = usuario
                      WHERE  nss         = reg_mod.nss
                      AND    consecutivo = reg_mod.consecutivo
                
                      IF SQLCA.SQLCODE < 0 THEN
                         LET x_error = " UPDATE ret_beneficiario:",
                                        " nss ",reg_mod.nss,
                                        " consecutivo ",reg_mod.consecutivo,
                                        err_get(SQLCA.SQLCODE)
                         CALL errorlog(x_error CLIPPED)
                
                         PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                         FOR enter
                         EXIT PROGRAM
                      END IF
                
                      ERROR " REGISTRO CONFIRMADO "  ATTRIBUTE(REVERSE)
                      SLEEP 2
                      INITIALIZE reg_mod.* TO NULL
                      CALL inicializa()
                  ELSE
                       ERROR " CONFIRMACION CANCELADA "  ATTRIBUTE(REVERSE)
                       SLEEP 3
                       CALL inicializa()
                       INITIALIZE reg_mod.* TO NULL
                  END IF
                  ----------------- fin sw_3 = 2  ---------------------------
            ELSE
                ERROR " EL REGISTRO DEBE ESTAR EN ESTADO CAPTURADO "  ATTRIBUTE(REVERSE)
                SLEEP 2
                CALL inicializa()
                INITIALIZE reg_mod.* TO NULL                
            END IF
            
         ELSE
             IF sw_3 =3  THEN
             --------------------------------------------
             ------ Se rechaza el registro --------------
                CURRENT WINDOW IS retm8025
                WHILE TRUE
                    PROMPT "ESTA SEGURO DE RECHAZAR LA SOLICITUD S/N ?" FOR opc
                    IF opc NOT MATCHES "[SsNn]" THEN
                       CONTINUE WHILE
                    ELSE
                       EXIT WHILE
                    END IF
                END WHILE

                IF opc MATCHES "[Ss]"  THEN
                   WHENEVER ERROR CONTINUE
                   UPDATE ret_solicitud_tx
                   SET cod_rechazo_ent  = reg_mod.cod_rechazo_ent,
                       entidad          = entidad,
                       estado_solicitud = reg_1.rechazado
                   WHERE  nss           = reg_mod.nss
                   AND    consecutivo   = reg_mod.consecutivo

                   IF SQLCA.SQLCODE < 0 THEN
                      LET x_error = " UPDATE ret_beneficiario:",
                                 " nss ",reg_mod.nss,
                                 " consecutivo ",reg_mod.consecutivo,
                                 err_get(SQLCA.SQLCODE)
                      CALL errorlog(x_error CLIPPED)
                    PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                    FOR enter
                    EXIT PROGRAM
                   END IF
                   ---------------------------------------------
                   ---------- Se desmarca la cuenta -------------
                   -- Se consulta tipo_movimento para retiro E --
                   SELECT movimiento
                   INTO v_tipo_movimiento
                   FROM tab_retiro
                   WHERE tipo_retiro = "D"
                   LET vestado_marca   = 0
                   LET vmarca_causa    = 0

                   LET vdesmarca = "EXECUTE PROCEDURE desmarca_cuenta('",
                      reg_mod.nss,"',",v_tipo_movimiento,",",
                      reg_mod.consecutivo,",",vestado_marca,",",
                      vmarca_causa,",' ",usuario,"')"
                      PREPARE exec_desmarca FROM vdesmarca
                      EXECUTE exec_desmarca
                   ---------------------------------------
                   ERROR " LA SOLICITUD SE RECHAZO CORRECTAMENTE "
                   ATTRIBUTE(REVERSE)
                   SLEEP 2
                   INITIALIZE reg_mod.* TO NULL
                   CALL inicializa()
               ELSE
                  DISPLAY " RECHAZO CANCELADO " AT 22,1
                  ATTRIBUTE(REVERSE) SLEEP 3
                  CALL inicializa()
               END IF
             END IF
             ---------------- fin sw_3 = 3 -------------
         END IF
     END IF
RETURN
END FUNCTION

FUNCTION rechazar(p_nss, p_consecutivo, p_tipo_prestacion,p_tipo_retiro)

  DEFINE
     p_nss             LIKE  ret_parcial.nss,
     p_consecutivo     LIKE  ret_parcial.consecutivo,
     p_tipo_prestacion LIKE  ret_parcial.tipo_prestacion,
     p_tipo_retiro     CHAR(1)

     DEFINE g_reg RECORD
        nss              LIKE ret_parcial.nss,
        consecutivo      LIKE ret_parcial.consecutivo,
        cod_rechazo_ent  LIKE ret_solicitud_tx.cod_rechazo_ent,
        cod_rec_ent_des  CHAR (60),
        tipo_prestacion  LIKE ret_parcial.tipo_prestacion,
        tipo_retiro  CHAR(1)
     END RECORD

     DEFINE
    vtipo_movimiento   SMALLINT,
    vestado_marca      SMALLINT,
    vmarca_causa       SMALLINT,
    vusuario           CHAR(12),
    ventidad           SMALLINT

    ----------------------------
   SELECT USER
   INTO vusuario
   FROM seg_modulo
   WHERE modulo_cod = "ret"

   SELECT e.entidad INTO ventidad
   FROM tab_entidad e
   WHERE e.descripcion = "AFORE"
   LET ventidad = 1

   LET g_reg.nss = p_nss
   LET g_reg.consecutivo = p_consecutivo
   LET g_reg.tipo_prestacion = p_tipo_prestacion
   LET g_reg.tipo_retiro = p_tipo_retiro

   INPUT BY NAME g_reg.cod_rechazo_ent
      -------------------------------------
         AFTER FIELD cod_rechazo_ent
      -------------------------------------
           IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
              LET g_reg.cod_rechazo_ent = null
              NEXT FIELD PREVIOUS
           END IF

            IF g_reg.cod_rechazo_ent IS NOT NULL THEN
               -- Valida que el codigo sea valido en catalogo de rechazos
              SELECT r.cod_rechazo_ent
              FROM ret_rechazo_grl r
              WHERE r.cod_rechazo_ent = g_reg.cod_rechazo_ent
              AND  r.tipo_retiro IN ("D","G")
              AND  r.entidad = ventidad

              IF SQLCA.SQLCODE = NOTFOUND THEN
                 ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"
                 LET g_reg.cod_rechazo_ent = null
                 NEXT FIELD cod_rechazo_ent
                 ELSE
                  DISPLAY BY NAME g_reg.cod_rechazo_ent
                  EXIT INPUT
              END IF
            ELSE
                  -- Se despliega ventana de ayuda de codigo de rechazo
                  CALL despliega_cod_rechazo_ent(g_reg.tipo_retiro)
                  RETURNING g_reg.cod_rechazo_ent, g_reg.cod_rec_ent_des

                  IF g_reg.cod_rechazo_ent <= 0 THEN
                     ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
                     LET g_reg.cod_rechazo_ent = NULL
                     NEXT FIELD cod_rechazo_ent
                  END IF

                  DISPLAY BY NAME g_reg.cod_rechazo_ent
                  EXIT INPUT
            END IF
   END INPUT
  RETURN g_reg.cod_rechazo_ent,ventidad
END FUNCTION

FUNCTION despliega_cod_rechazo_ent (tipo_retiro)

     DEFINE l_reg ARRAY[100] OF RECORD
        cod_rechazo_ent       SMALLINT,
        des_corta             CHAR(60)
     END RECORD

   DEFINE x_x               CHAR(200),
          x_buscar          CHAR(030),
          codigo            SMALLINT,
          descripcion       CHAR(60),
          i                 INTEGER,
          pos               SMALLINT,
          tipo_retiro       CHAR(1),
          tr                CHAR(1)

   let tr = tipo_retiro
   OPEN WINDOW retm8092 AT 05,10 WITH FORM "RETM0064" ATTRIBUTE(BORDER)
     DISPLAY "                      TIPOS DE RECHAZOS                  " AT 2,1 ATTRIBUTE(REVERSE)
INPUT   BY NAME x_buscar
          BEFORE FIELD x_buscar
             LET x_buscar = "*"

         AFTER FIELD x_buscar
             IF x_buscar IS NULL THEN
                 ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                 NEXT FIELD x_buscar
             ELSE
                 EXIT INPUT
             END IF
     END INPUT
     WHILE TRUE
         LET x_x = " SELECT cod_rechazo_ent, des_larga FROM ret_rechazo_grl",
                   " WHERE des_corta MATCHES ",'"',x_buscar CLIPPED,'"',
                   --" AND tipo_retiro = '",tr CLIPPED ,"'",
                   " AND (tipo_retiro = 'D' ",
                   " OR  tipo_retiro = 'G') ",
                   " AND entidad = 1 ",
                   " ORDER BY 1 " CLIPPED

         PREPARE pre_tr FROM x_x
         DECLARE cur_tr CURSOR FOR pre_tr
         LET pos = 1
         FOREACH cur_tr INTO l_reg[pos].*
           LET pos = pos + 1
             IF pos >= 1000 THEN
                 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                 EXIT FOREACH
             END IF
         END FOREACH

         IF (pos-1) < 1 THEN
             ERROR "ARCHIVO DE RECHAZOS VACIO   "
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
             ON KEY ( CONTROL-C )
              LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET codigo = l_reg[pos].cod_rechazo_ent
                 LET descripcion = l_reg[pos].des_corta
                 EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
     END WHILE
     CLOSE WINDOW retm8092
     RETURN codigo, descripcion
END FUNCTION

FUNCTION cat_tipo_ret()
#ctr-------------------
    DEFINE c_des_tipo_ret LIKE tab_retiro.descripcion

    SELECT descripcion
    INTO   c_des_tipo_ret
    FROM   tab_retiro
    WHERE  tipo_retiro = rg_datamart.tipo_ret

    RETURN c_des_tipo_ret

END FUNCTION

FUNCTION cat_tipo_prestacion()
#ctp--------------------------
    DEFINE c_descripcion LIKE tab_prestacion.descripcion

    SELECT descripcion
    INTO   c_descripcion
    FROM   tab_prestacion
    WHERE  tipo_prestacion = rg_datamart.tipo_prestacion

    RETURN c_descripcion
    display c_descripcion
    sleep 2
END FUNCTION

FUNCTION cat_tipo_pension()
#ctp-----------------------
    DEFINE c_descripcion LIKE tab_pension.descripcion

    SELECT descripcion
    INTO   c_descripcion
    FROM   tab_pension
    WHERE  tipo_pension = rg_datamart.tipo_pension

    RETURN c_descripcion

END FUNCTION

FUNCTION cat_tipo_seguro()
#ctp-----------------------
    DEFINE c_descripcion LIKE tab_seguro.descripcion

    SELECT descripcion
    INTO   c_descripcion
    FROM   tab_seguro
    WHERE  clave  = rg_datamart.tipo_seguro

    RETURN c_descripcion

END FUNCTION


FUNCTION despliega_tipo_prestacion()
#dtp--------------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm8024 AT 05,12 WITH FORM "RETM8024" ATTRIBUTE(BORDER)
    DISPLAY "               TIPO DE PRESTACION                          " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE

         LET prepare_1 = " SELECT * FROM tab_prestacion ",
                         " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                         " AND tipo_prestacion = 3 ",
                         " ORDER BY 1 " CLIPPED
         PREPARE pre_5 FROM prepare_1
         DECLARE cur_5 CURSOR FOR pre_5

         LET pos = 1

         FOREACH cur_5 INTO ra_reg[pos].*
              LET pos = pos + 1
              IF pos >= 1000 THEN
                  ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                   ATTRIBUTE(NORMAL)
                  EXIT FOREACH
              END IF

         END FOREACH

         IF (pos-1) < 1 THEN
             ERROR "    ARCHIVO TIPO DE SEGURO VACIO"
             ATTRIBUTE(NORMAL)
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY ra_reg TO scr_2.*
             ON KEY ( CONTROL-C )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET rg_datamart.tipo_prestacion = ra_reg[pos].codigo
                 LET rg_datamart.desc_prestacion = ra_reg[pos].descripcion
                 EXIT DISPLAY

         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
    END WHILE
    CLOSE WINDOW retm8024
END FUNCTION


FUNCTION despliega_tipo_seguro()
#dts----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm8025 AT 05,12 WITH FORM "RETM8024" ATTRIBUTE(BORDER)
    DISPLAY "                     TIPO DE SEGURO                        " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE

         LET prepare_1 = " SELECT * FROM tab_seguro ",
                         " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                         " AND clave IN ('CV','IM') ",
                         " ORDER BY 1 " CLIPPED
         PREPARE pre_6 FROM prepare_1
         DECLARE cur_6 CURSOR FOR pre_6

         LET pos = 1

         FOREACH cur_6 INTO ra_reg[pos].*
              LET pos = pos + 1
              IF pos >= 1000 THEN
                  ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                   ATTRIBUTE(NORMAL)
                  EXIT FOREACH
              END IF

         END FOREACH

         IF (pos-1) < 1 THEN
             ERROR "    ARCHIVO TIPO DE SEGURO VACIO"
             ATTRIBUTE(NORMAL)
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY ra_reg TO scr_2.*
             ON KEY ( CONTROL-C )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET rg_datamart.tipo_seguro   = ra_reg[pos].codigo
                 LET rg_datamart.desc_seguro   = ra_reg[pos].descripcion
                 EXIT DISPLAY

         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
    END WHILE
    CLOSE WINDOW retm8025
END FUNCTION

FUNCTION despliega_tipo_pension()
#dtp----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm8026 AT 05,12 WITH FORM "RETM8024" ATTRIBUTE(BORDER)
    DISPLAY "                    TIPO DE PENSION                        " AT 2,1  ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "    DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                ATTRIBUTE(NORMAL)
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        IF rg_datamart.tipo_seguro = 'CV' THEN
            LET prepare_1 = " SELECT * FROM tab_pension ",
                         " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                         " AND   tipo_pension IN ('CE','VE')",
                         " ORDER BY 1 " CLIPPED
        ELSE
            LET prepare_1 = " SELECT * FROM tab_pension ",
                         " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                         " AND   tipo_pension IN ('AS','OR','VI','VO','IN')",
                         " ORDER BY 1 " CLIPPED
        END IF

        PREPARE pre_7 FROM prepare_1
        DECLARE cur_7 CURSOR FOR pre_7

        LET pos = 1

        FOREACH cur_7 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF

        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO TIPO DE SEGURO VACIO"
            ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY ra_reg TO scr_2.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                LET rg_datamart.tipo_pension   = ra_reg[pos].codigo
                LET rg_datamart.desc_pension   = ra_reg[pos].descripcion
                EXIT DISPLAY

        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm8026
END FUNCTION

FUNCTION marca_cuenta(vl_nss,vl_marca_ent,vl_consecutivo)
#mc------------------------------------------------------

    DEFINE #loc #smallint
        vl_marca_ent                     ,
        vl_marca_res                     ,
        vl_convive_cod                   ,
        vl_cod_rechazo                   SMALLINT

    DEFINE #loc #char
        vl_nss                           CHAR(011)

    DEFINE #loc #integer
        vl_consecutivo                   INTEGER


    DEFINE #loc #reg_20
        reg_20 RECORD
              estado_marca          SMALLINT,
              codigo_rechazo        SMALLINT,
              marca_causa           SMALLINT,
              fecha_causa           DATE
    END RECORD


    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0
    INITIALIZE reg_20.fecha_causa TO NULL

    DECLARE cur_sp CURSOR FOR eje_marca
    OPEN cur_sp USING vl_nss                , # nss
                      vl_marca_ent          , # marca entrante
                      vl_consecutivo        , # correlativo
                      reg_20.estado_marca   , # estado_marca
                      reg_20.codigo_rechazo , # codigo de rechazo
                      reg_20.marca_causa    , # marca_causa
                      reg_20.fecha_causa    , # fecha_causa
                      usuario                 # usuario

     FETCH cur_sp INTO vl_marca_res   , # misma marca si convive o
                      vl_cod_rechazo    # marca activa que rechaza
                                        # codigo de rechazo

     CLOSE cur_sp
     RETURN vl_marca_res,
            vl_cod_rechazo

END FUNCTION

