#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM808  => MANTENEDOR DE REGISTRO DE CERTIFICADO IMSS - RETIRO       #
#                     PLAN PRIVADO REGIMEN 97 (TIPO RETIRO F)                   #
#                                  VERSION COPPEL                               #
#Fecha creacion    => 16 DE ENERO DE 2004                                       #
#By                => MARCOS GODINEZ JIMENEZ                                    #
#Fecha actualiz.   => 30 DE OCTUBRE DE 2007                                     #
#Actualizacion     => XAVIER TORRES RIOS                                        #
#                  => Desarrollar la opcion para rechazar solicitudes de retiro #
#                     y seleccionar de catalogo el motivo de rechazo            #
#Fecha actualiz.   => 18 DE ABRIL DE 2008                                       #
#Actualizacion     => XAVIER TORRES RIOS                                        #
#                  => Se modifica para buscar el diagnostico de procesar de la  #
#                     tabla ret_solicitud_tx y el diagnostico de vivienda de la #
#                     tabla ret_monto_viv. En caso de no encontrarse alli se    #
#                     busca en ret_solicitud_rx                                 #
#Fecha actualiz.   => 2 DE SEPTIEMBRE DE 2010                                   #
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
        nss                LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_retiro        LIKE ret_solicitud_tx.tipo_retiro      ,
        descripcion        LIKE tab_retiro.descripcion            ,
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion  ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        actuario           LIKE ret_solicitud_tx.actuario         ,
        aseguradora        LIKE ret_solicitud_tx.aseguradora      ,
        num_plan_pension   LIKE ret_solicitud_tx.num_plan_pension ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod      ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc        ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        desc_estado        CHAR(25)                               ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma ,
        fecha_captura      DATE                                   ,
        fecha_modifica     DATE                                   ,
        fecha_confirma     DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio          ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio    ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE  rg_datamart    RECORD #glo #rg_input
        nss                LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_retiro        LIKE ret_solicitud_tx.tipo_retiro      ,
        descripcion        LIKE tab_retiro.descripcion            ,
        tipo_prestacion    LIKE ret_det_datamart.curp             ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        actuario           LIKE ret_solicitud_tx.actuario         ,
        aseguradora        LIKE ret_solicitud_tx.aseguradora      ,
        num_plan_pension   LIKE ret_solicitud_tx.num_plan_pension ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod      ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc        ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        desc_estado        CHAR(25)                               ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma ,
        fecha_captura      DATE                                   ,
        fecha_modifica     DATE                                   ,
        fecha_confirma     DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio              ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio    ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        precapturado       LIKE ret_estado.estado_solicitud ,
        capturado          LIKE ret_estado.estado_solicitud ,
        rechazado          LIKE ret_estado.estado_solicitud ,
        confirmado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_20 RECORD
        estado_marca       SMALLINT ,
        codigo_rechazo     SMALLINT ,
        marca_causa        SMALLINT ,
        fecha_causa        DATE
    END RECORD

    DEFINE reg_rev RECORD
           nss             CHAR(11) ,
           marca_cod       SMALLINT ,
           correlativo     INTEGER
    END RECORD

    DEFINE  #glo #date
        vfecha_ini_p      ,
        vfecha_resolucion ,
        HOY               DATE

    DEFINE  #glo #char
        txt_1             CHAR(3000),
        txt_2             CHAR(2200),
        txt_3             CHAR(2200),
        txt_4             CHAR(2200),
        txt_5             CHAR(2200),
        x_busca           CHAR(1200),
        s_codigo_afore    CHAR(0004),
        usuario           CHAR(0012),
        option_afore      CHAR(0006),
        v_marca           CHAR(0100),
        enter             CHAR(0001),
        vaccion           CHAR(0001),
        xaccion           CHAR(0001),
        vnss              CHAR(0011),
        vregimen          CHAR(0002),
        vtipo_seguro      CHAR(0002),
        vtipo_pension     CHAR(0002),
        vtipo_retiro      CHAR(0001),
        v_desmarca        CHAR(0100),
        v_reversa         CHAR(0100),
        v_ejecuta         CHAR(0500),
        vmax_sec_pension  CHAR(0002),
        desc_cod_rechazo  CHAR(0040),
        opc               CHAR(0001),
        ok_datamart       CHAR(0007)

    DEFINE #glo #smallint
        v_cod_rechazo     ,
        v_marca_res       ,
        v_marca_ent       ,
        vtipo_prestacion  ,
        marca_ent         ,
        v_status          ,
        v_conviv          ,
        s_tipo_movimiento ,
        s_capturado       ,
        s_procesado       ,
        s_pagado          ,
        i                 ,
        pos               ,
        entidad           ,
        v_tipo_movimiento ,
        codigo            ,
        sw_2              SMALLINT

    DEFINE #glo #integer
        ult_consecutivo   ,
        vmax_folio        INTEGER

    DEFINE 
        x_error            CHAR(500),  ---omar
        arr_c              SMALLINT,
        x_usuario          CHAR(12),
        x_estado_solicitud CHAR(40),
        sw                 SMALLINT

    DEFINE xx_fecha_solicitud DATE

    DEFINE vestado_marca   ,
       vcodigo_rechazo ,
       vmarca_causa    SMALLINT,
       vfecha_causa    DATE
    DEFINE varmov  like tab_retiro.movimiento

END GLOBALS
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM808.log")    ---omar
    CALL init()
    LET xaccion = " "
    OPEN WINDOW retm8071 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE(BORDER)
    DISPLAY " RETM808                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)

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
            LET xaccion= " "
            CALL inicializa()
            CALL agrega() #a

        COMMAND KEY("C") "Consulta" "Consulta Solicitud"
            LET xaccion= " "
            CALL inicializa()
            CALL consulta() #c

        COMMAND KEY("M") "Modifica" "Modifica Solicitud"
            LET xaccion= " "
            CALL inicializa()
            CALL modifica() #m

        COMMAND KEY("E") "Elimina" "Elimina Solicitud"
            LET xaccion= " "
            CALL inicializa()
            CALL elimina() #e

        COMMAND KEY("F") "conFirma" "Confirma Solicitud"
            LET xaccion= "F"
            CALL inicializa()
            CALL confirma() #c

        COMMAND KEY("S") "Salida" "Regresa al Menu"
            EXIT MENU
    END MENU
    END CASE
    CLOSE WINDOW retm8071
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
    INTO   reg_1.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    ----- MARCAJE ------

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "

    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0
    INITIALIZE reg_20.fecha_causa TO NULL

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    ----- DESMARCAJE ------
    PREPARE eje_desmarca FROM v_desmarca

    LET v_marca_ent = 0

END FUNCTION

FUNCTION inicializa()
#i--------------------
    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a----------------

    DEFINE #loc #smallint
        sw_1                  ,
        s_grupo               ,
        s_subcta              ,
        vestado_solicitud     SMALLINT

    DEFINE  #loc  #char
        tipoprestacion     ,
        tiposeguro         ,
        tipopension        CHAR(02)

    DEFINE #loc #integer
        ult_consecutivo            INTEGER

    DEFINE #loc #decimal
        vconsecutivo               LIKE ret_solicitud_tx.consecutivo

    OPEN WINDOW retm8072 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE (BORDER)
    DISPLAY " RETM808                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " AGREGA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega   Ctrl-C : Salir " AT 2,1

    LET sw_1               = 0
    LET rg_datamart.fecha_captura  = HOY
    LET rg_datamart.rechazo_cod    = 0
    LET v_conviv           = 0

    DISPLAY BY NAME rg_datamart.fecha_captura

    INPUT BY NAME rg_datamart.nss                ,
                  rg_datamart.tipo_prestacion    ,
                  rg_datamart.regimen            ,
                  rg_datamart.tipo_seguro        ,
                  rg_datamart.tipo_pension       ,
                  rg_datamart.fecha_ini_pen      ,
                  rg_datamart.actuario           ,
                  rg_datamart.aseguradora        ,
                  rg_datamart.num_plan_pension   ,
                  rg_datamart.sol_folio          ,
                  rg_datamart.fecha_solicitud    ,
                  rg_datamart.semanas_cotizadas  WITHOUT DEFAULTS

        BEFORE FIELD nss
        IF vaccion = 'C' OR
           vaccion = 'M' THEN

                LET rg_datamart.nss              = vnss
                LET rg_datamart.regimen          = vregimen
                LET rg_datamart.tipo_prestacion  = vtipo_prestacion
                LET rg_datamart.tipo_seguro      = vtipo_seguro
                LET rg_datamart.tipo_pension     = vtipo_pension
                LET rg_datamart.tipo_retiro      = vtipo_retiro
                LET rg_datamart.fecha_ini_pen    = vfecha_ini_p

                DISPLAY rg_datamart.nss              TO nss
                DISPLAY rg_datamart.regimen          TO regimen
                DISPLAY rg_datamart.tipo_prestacion  TO tipo_prestacion
                DISPLAY rg_datamart.tipo_seguro      TO tipo_seguro
                DISPLAY rg_datamart.tipo_pension     TO tipo_pension
                DISPLAY rg_datamart.tipo_retiro      TO tipo_retiro
                DISPLAY rg_datamart.fecha_solicitud  TO fecha_solicitud
                DISPLAY rg_datamart.fecha_ini_pen    TO fecha_ini_pen

                SELECT n_rfc     ,
                       n_unico    ,
                       paterno    ,
                       materno    ,
                       nombres
                INTO  rg_datamart.n_rfc   ,
                      rg_datamart.n_unico ,
                      rg_datamart.paterno ,
                      rg_datamart.materno ,
                      rg_datamart.nombres
                FROM  afi_mae_afiliado
                WHERE n_seguro = rg_datamart.nss

                SELECT A.descripcion
                INTO   rg_datamart.desc_estado
                FROM   ret_status A
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

                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension

                DISPLAY BY NAME rg_datamart.materno
                DISPLAY BY NAME rg_datamart.paterno
                DISPLAY BY NAME rg_datamart.nombres
                DISPLAY BY NAME rg_datamart.n_rfc
                DISPLAY BY NAME rg_datamart.n_unico
                DISPLAY BY NAME rg_datamart.desc_estado
                DISPLAY rg_datamart.desc_seguro  TO desc_seguro
                DISPLAY rg_datamart.desc_pension TO desc_pension
                DISPLAY rg_datamart.descripcion  TO descripcion
                DISPLAY BY NAME rg_datamart.desc_prestacion
            END IF

            IF sw_1 = 0 THEN

                LET rg_datamart.tipo_seguro = "PP"
                LET rg_datamart.tipo_retiro    = "F"

                SELECT descripcion
                INTO   rg_datamart.descripcion
                FROM   tab_retiro
                WHERE  tipo_retiro = rg_datamart.tipo_retiro

                LET rg_datamart.tipo_prestacion = 4

                SELECT descripcion
                INTO   rg_datamart.desc_prestacion
                FROM   tab_prestacion
                WHERE  tipo_prestacion = rg_datamart.tipo_prestacion

                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET rg_datamart.consecutivo    = ult_consecutivo

                LET rg_datamart.regimen         = "97"
                LET rg_datamart.fecha_solicitud   = HOY
                LET rg_datamart.fecha_captura   = HOY
                LET rg_datamart.usuario_captura = usuario
                LET rg_datamart.tipo_seguro     = rg_datamart.tipo_seguro

                SELECT descripcion
                INTO   rg_datamart.desc_seguro
                FROM   tab_seguro
                WHERE  clave = rg_datamart.tipo_seguro
                GROUP BY 1

                DISPLAY BY NAME rg_datamart.tipo_retiro     ,
                                rg_datamart.descripcion     ,
                                rg_datamart.tipo_prestacion ,
                                rg_datamart.desc_prestacion ,
                                rg_datamart.tipo_seguro     ,
                                rg_datamart.desc_seguro     ,
                                rg_datamart.regimen         ,
                                rg_datamart.fecha_solicitud   ,
                                rg_datamart.fecha_captura   ,
                                rg_datamart.consecutivo     ,
                                rg_datamart.usuario_captura
                LET sw_1 = 1
            END IF

        AFTER FIELD nss
            LET rg_datamart.fecha_captura = HOY

            IF rg_datamart.nss IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            ELSE
                SELECT n_unico ,
                       n_rfc   ,
                       paterno ,
                       materno ,
                       nombres
                INTO   rg_datamart.n_unico ,
                       rg_datamart.n_rfc   ,
                       rg_datamart.paterno ,
                       rg_datamart.materno ,
                       rg_datamart.nombres
                FROM   afi_mae_afiliado
                WHERE  n_seguro = rg_datamart.nss
                GROUP BY 1,2,3,4,5

                IF STATUS = NOTFOUND THEN
                    SELECT "OK"
                    FROM   afi_solicitud
                    WHERE  n_seguro = rg_datamart.nss
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "    TRABAJADOR NO AFILIADO A LA AFORE"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD nss
                    ELSE
                        ERROR "    TRABAJADOR SE ENCUENTRA COMO PREAFILIADO"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD nss
                    END IF
                ELSE
                    DISPLAY BY NAME rg_datamart.n_unico
                    DISPLAY BY NAME rg_datamart.n_rfc
                    DISPLAY BY NAME rg_datamart.paterno
                    DISPLAY BY NAME rg_datamart.materno
                    DISPLAY BY NAME rg_datamart.nombres
                END IF
                    NEXT FIELD tipo_pension

            END IF

        AFTER FIELD tipo_pension
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD regimen
            END IF

            IF rg_datamart.tipo_pension IS NULL THEN
                 CALL despliega_tipo_pension() #dtp
                 DISPLAY rg_datamart.desc_pension TO desc_pension

                 NEXT FIELD fecha_ini_pen
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                GROUP BY 1

                 DISPLAY rg_datamart.desc_pension TO desc_pension
                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_pension() #dtp
                                DISPLAY rg_datamart.desc_pension TO desc_pension
                                NEXT FIELD tipo_pension
                            ELSE
                                NEXT FIELD fecha_ini_pen
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY rg_datamart.desc_pension TO desc_pension

        AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_pension
            END IF

            LET vestado_solicitud  = " "

                SELECT MAX(consecutivo)
                INTO   vconsecutivo
                FROM   ret_solicitud_tx
                WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "F"
                AND    regimen          = 97
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion

                SELECT estado_solicitud
                INTO   vestado_solicitud
                FROM   ret_solicitud_tx
                WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "F"
                AND    regimen          = 97
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
                       ELSE }
                   SELECT "OK"
                   FROM   ret_solicitud_tx A
                   WHERE  A.nss           = rg_datamart.nss
                   AND    A.consecutivo   = vconsecutivo
                   AND    A.diag_registro = 400

               IF STATUS <> NOTFOUND THEN
                    SELECT "OK"
                    FROM   dis_cuenta A
                    WHERE  A.nss           = rg_datamart.nss
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

            IF rg_datamart.fecha_ini_pen IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_ini_pen
            ELSE
                IF rg_datamart.fecha_ini_pen > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_ini_pen
                END IF
                IF rg_datamart.fecha_ini_pen > rg_datamart.fecha_solicitud THEN
                    ERROR "    FECHA INICIO DE PENSION > FECHA DE SOLICITUD" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                END IF

                IF rg_datamart.fecha_ini_pen >= "07011997" THEN

                    SELECT MAX(folio)
                    INTO   vmax_folio
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen

                    SELECT MAX(sec_pension)
                    INTO   vmax_sec_pension
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen
                    AND    folio           = vmax_folio

                    SELECT sec_pension       ,
                           diag_datamart     ,
                           semanas_cotizadas ,
                           nombre_datamart
                    INTO   rg_datamart.sec_pension       ,
                           rg_datamart.diag_datamart     ,
                           rg_datamart.semanas_cotizadas ,
                           rg_datamart.nombre_datamart
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen
                    AND    folio            = vmax_folio
                    AND    sec_pension      = vmax_sec_pension

                    IF STATUS = NOTFOUND THEN

                        PROMPT "  NO SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                               " PARA CONTINUAR"
                        FOR CHAR enter

                        LET rg_datamart.sec_pension       = ""
                        LET rg_datamart.diag_datamart     = ""
                        LET rg_datamart.semanas_cotizadas = ""
                        LET rg_datamart.nombre_datamart   = ""

                        DISPLAY rg_datamart.sec_pension       TO sec_pension
                        DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                        DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                        DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                        NEXT FIELD actuario

                   ELSE

                        PROMPT "  SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                               " PARA CONTINUAR"
                        FOR CHAR enter

                        DISPLAY rg_datamart.sec_pension       TO sec_pension
                        DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                        DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                        DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                        NEXT FIELD num_plan_pension
                   END IF
                ELSE
                    SELECT MAX(folio)
                    INTO   vmax_folio
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen

                    SELECT MAX(sec_pension)
                    INTO   vmax_sec_pension
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen
                    AND    folio           = vmax_folio

                   SELECT sec_pension       ,
                          diag_datamart     ,
                          semanas_cotizadas ,
                          nombre_datamart
                   INTO   rg_datamart.sec_pension       ,
                          rg_datamart.diag_datamart     ,
                          rg_datamart.semanas_cotizadas ,
                          rg_datamart.nombre_datamart
                   FROM   ret_det_datamart
                   WHERE  nss             = rg_datamart.nss
                   AND    tipo_seguro     = rg_datamart.tipo_seguro
                   AND    tipo_pension    = rg_datamart.tipo_pension
                   AND    regimen         = rg_datamart.regimen
                   AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen
                   AND    folio           = vmax_folio
                   AND    sec_pension     = vmax_sec_pension

                   IF STATUS = NOTFOUND THEN
                       PROMPT "  NO SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                              " PARA CONTINUAR"
                       FOR CHAR enter

                       LET rg_datamart.sec_pension       = ""
                       LET rg_datamart.diag_datamart     = ""
                       LET rg_datamart.semanas_cotizadas = ""
                       LET rg_datamart.nombre_datamart   = ""

                       DISPLAY rg_datamart.sec_pension       TO sec_pension
                       DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                       DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                       DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                       NEXT FIELD actuario

                   ELSE

                      PROMPT "  SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                             " PARA CONTINUAR"
                      FOR CHAR enter

                      DISPLAY rg_datamart.sec_pension       TO sec_pension
                      DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                      DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                      DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                      NEXT FIELD num_plan_pension
                   END IF
               END IF
{
                SELECT MAX(folio)
                INTO   vmax_folio
                FROM   ret_det_datamart
                WHERE  nss             = rg_datamart.nss
                AND    tipo_seguro     = rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    regimen         = rg_datamart.regimen
                AND    fecha_ini_pen  >= "07011997"

                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss             = rg_datamart.nss
                AND    tipo_seguro     = rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    regimen         = rg_datamart.regimen
                AND    fecha_ini_pen  >= "07011997"
                AND    folio           = vmax_folio

                SELECT sec_pension       ,
                       diag_datamart     ,
                       semanas_cotizadas ,
                       nombre_datamart
                INTO   rg_datamart.sec_pension       ,
                       rg_datamart.diag_datamart     ,
                       rg_datamart.semanas_cotizadas ,
                       rg_datamart.nombre_datamart
                FROM   ret_det_datamart
                WHERE  nss             = rg_datamart.nss
                AND    tipo_seguro     = rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    regimen         = rg_datamart.regimen
                AND    fecha_ini_pen  >= "07011997"
                AND    folio           = vmax_folio
                AND    sec_pension     = vmax_sec_pension


               IF STATUS = NOTFOUND THEN

                  IF rg_datamart.fecha_ini_pen < "07011997" THEN

                    PROMPT "  NO SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                           " PARA CONTINUAR"
                    FOR CHAR enter

                    LET rg_datamart.sec_pension       = ""
                    LET rg_datamart.diag_datamart     = ""
                    LET rg_datamart.semanas_cotizadas = ""
                    LET rg_datamart.nombre_datamart   = ""

                    DISPLAY rg_datamart.sec_pension       TO sec_pension
                    DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                    DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                    DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                    NEXT FIELD actuario

                  ELSE

                    PROMPT "  NO SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                           " PARA CONTINUAR"
                    FOR CHAR enter

                    LET rg_datamart.sec_pension       = ""
                    LET rg_datamart.diag_datamart     = ""
                    LET rg_datamart.semanas_cotizadas = ""
                    LET rg_datamart.nombre_datamart   = ""

                    DISPLAY rg_datamart.sec_pension       TO sec_pension
                    DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                    DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                    DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                    EXIT INPUT

                  END IF

                ELSE
                    PROMPT "  SE ENCONTRO REGISTRO EN DATAMART ... <ENTER>",
                           " PARA CONTINUAR"
                    FOR CHAR enter

                    DISPLAY rg_datamart.sec_pension       TO sec_pension
                    DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                    DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                    DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart

                NEXT FIELD sol_folio
                END IF
}
            END IF

        AFTER FIELD actuario
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_ini_pen
            END IF

            IF rg_datamart.actuario IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD actuario
            ELSE
                NEXT FIELD aseguradora
            END IF

        AFTER FIELD aseguradora
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD actuario
            END IF

            IF rg_datamart.actuario IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD aseguradora
            ELSE
                NEXT FIELD num_plan_pension
            END IF

        AFTER FIELD num_plan_pension
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD aseguradora
            END IF

        AFTER FIELD sol_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_plan_pension
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
            END IF

            IF rg_datamart.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF rg_datamart.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

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
        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF rg_datamart.semanas_cotizadas IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD semanas_cotizadas
            END IF

     ON KEY (ESC)
                IF rg_datamart.nss IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD nss
                END IF

                IF rg_datamart.tipo_seguro IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD tipo_seguro
                END IF

                IF rg_datamart.tipo_pension IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD tipo_pension
        ELSE

            LET vestado_solicitud  = " "

                SELECT MAX(consecutivo)
                INTO   vconsecutivo
                FROM   ret_solicitud_tx
                WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "F"
                AND    regimen          = 97
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion

                SELECT estado_solicitud
                INTO   vestado_solicitud
                FROM   ret_solicitud_tx
                WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "F"
                AND    regimen          = 97
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
                    WHERE  A.nss           = rg_datamart.nss
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

                IF rg_datamart.fecha_ini_pen IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                ELSE
                    IF rg_datamart.fecha_ini_pen > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_ini_pen
                    END IF
                END IF

                IF rg_datamart.fecha_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_solicitud
                ELSE
                    IF rg_datamart.fecha_solicitud > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_solicitud
                    END IF
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
                IF rg_datamart.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF

                ----- MARCAJE ------

                SELECT movimiento
                INTO   s_tipo_movimiento
                FROM   tab_retiro
                WHERE  tipo_retiro = "F"

                CALL marca_cuenta (rg_datamart.nss           ,
                                   s_tipo_movimiento ,
                                   rg_datamart.consecutivo
                                  )#mc
                RETURNING v_marca_res ,
                          v_cod_rechazo

                LET rg_datamart.rechazo_cod = v_cod_rechazo

                IF v_cod_rechazo > 0 THEN

                    SELECT A.rechazo_cod
                    INTO   desc_cod_rechazo
                    FROM   tab_rch_marca A
                    WHERE  A.rechazo_cod = v_cod_rechazo

                    PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",desc_cod_rechazo CLIPPED , ") <ENTER> CONTINUAR" FOR CHAR enter
                END IF

                --------------------

                CALL matriz_de_derecho1(
                                       rg_datamart.regimen,
                                       rg_datamart.tipo_seguro,
                                       rg_datamart.tipo_pension,
                                       rg_datamart.tipo_prestacion
                                      ) #mdd
                RETURNING s_grupo

                IF rg_datamart.rechazo_cod <> 0 THEN
                    LET v_conviv = 1
                END IF

        IF rg_datamart.aseguradora  IS NULL OR
                   rg_datamart.aseguradora = "0" THEN
                   LET rg_datamart.aseguradora = 0
        END IF

        WHENEVER ERROR CONTINUE
                INSERT INTO ret_solicitud_tx
                VALUES(rg_datamart.nss                 ,
                       rg_datamart.consecutivo         ,
                       " "                             , #folio
                       rg_datamart.sol_folio           ,
                       "S"                             ,
                       rg_datamart.n_unico             ,
                       ""                              ,
                       " "                             ,
                       rg_datamart.tipo_retiro         ,
                       rg_datamart.regimen             ,
                       rg_datamart.tipo_seguro         ,
                       rg_datamart.tipo_pension        ,
                       rg_datamart.tipo_prestacion     ,
                       rg_datamart.fecha_ini_pen       ,
                       ' '                             , #fecha_resolucion
                       rg_datamart.fecha_solicitud     ,
                       ' '                             , #cve_doc_probatorio
                       '01010001'                      , #fecha_nacimiento
                       rg_datamart.aseguradora         , #aseguradora
                       rg_datamart.actuario            , #actuario
                       rg_datamart.num_plan_pension    , #num_plan_pension
                       0                               , #periodo_pago
                       0                               , #acciones_ret97
                       0                               , #acciones_cv
                       0                               , #acciones_cuota_soc
                       0                               , #acciones_ret92
                       ' '                             , #fecha_valor_viv
                       0                               , #saldo_viv97
                       0                               , #saldo_viv92
                       0                               , #saldo_viv72
                       ' '                             , #diag_registro
                       ' '                             , #estado_sub_viv
                       rg_datamart.semanas_cotizadas   ,
                       reg_1.capturado                 , #estado_solicitud
                       " "                             , #entidad
                       " "                             , #cod_rechazo_ent
                       v_cod_rechazo                   , #rechazo_cod
                       rg_datamart.fecha_captura       ,
                       '01010001'                      , #fecha_confirma
                       '01010001'                      , #fecha_modifica
                       '01010001'                      , #fecha_envio
                       rg_datamart.usuario_captura     , #usuario_captura
                       ' '                             , #usuario_confirma
                       ' '                             , #usuario_modifica
                       0                               , #carta
                       s_grupo                         , #grupo
                       'T'                             , #cve destino
                       0                               , #porcenjaje val
                       0                               , #num_resolucion
                       ""                              , #materno sol
                       ""                              , #nombre sol
                       ""
                      )

---omar
        IF SQLCA.SQLCODE < 0 THEN
           LET x_error = "INSERT ret_solicitud_tx:",
                 "nss ",rg_datamart.nss,
                 "consecutivo ",rg_datamart.consecutivo,
                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

           PROMPT "  ERROR AL INSERTAR REGISTROS AVISE A SISTEMAS "
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
                    LET v_ejecuta = "fglgo RETM810 ",rg_datamart.nss," ",
                                                rg_datamart.consecutivo," ",
                                                "A"
                    RUN v_ejecuta
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

                DISPLAY "  REGISTRO INGRESADO  ","" AT 22,1
                ATTRIBUTE(REVERSE) SLEEP 3

                INITIALIZE vaccion           TO NULL
                INITIALIZE vnss              TO NULL
                INITIALIZE vtipo_prestacion  TO NULL
                INITIALIZE vtipo_retiro      TO NULL

                CALL inicializa() #i
                DISPLAY "                   ","" AT 20,1
                LET sw_1 = 0
                NEXT FIELD nss

            ON KEY(CONTROL-C)
                DELETE
                FROM   ret_consecutivo
                WHERE  consecutivo = ult_consecutivo

                EXIT INPUT

            ON KEY(INTERRUPT)
                DELETE
                FROM   ret_consecutivo
                WHERE  consecutivo = ult_consecutivo

                EXIT INPUT
    END INPUT

    CLOSE WINDOW retm8072
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm8073 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE (BORDER)
    DISPLAY " RETM808                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Consulta   Ctrl-B : Beneficiarios   Ctrl-C : Salir " AT 2,1

    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.n_unico          ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
                                  A.tipo_seguro      ,
                                  A.tipo_pension     ,
                                  A.actuario         ,
                                  A.aseguradora      ,
                                  A.num_plan_pension ,
                                  A.sol_folio        ,
                                  A.fecha_captura    ,
                                  A.fecha_confirma   ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm8073
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #descripcion
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " A.fecha_ini_pen     ,",
                      " A.actuario          ,",
                      " A.aseguradora       ,",
                      " A.num_plan_pension  ,",
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " ' '                 ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.consecutivo       ,",
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " A.folio             ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D ",
                  " WHERE ",x_busca CLIPPED,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'F'                ",
                  " AND   A.regimen         = 97                 ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_seguro     = C.tipo_seguro      ",
                  " AND   A.tipo_pension    = C.tipo_pension     ",
                  " AND   A.tipo_prestacion    = C.tipo_prestacion ",
                  " AND   A.nss         = D.nss  ",
                  " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_6 FROM txt_1
    DECLARE cur_6 CURSOR FOR pre_6

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_6 INTO ga_solicitud[i].*
        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].nss
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].descripcion,varmov
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
{
        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    sec_pension = ga_solicitud[i].sec_pension
}
        SELECT descripcion
        INTO   ga_solicitud[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   ga_solicitud[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = ga_solicitud[i].rechazo_cod
        END IF

        IF ga_solicitud[i].fecha_confirma = "01010001" THEN
            LET ga_solicitud[i].fecha_confirma = ""
        END IF

        IF ga_solicitud[i].fecha_modifica = "01010001" THEN
            LET ga_solicitud[i].fecha_modifica = ""
        END IF

        IF ga_solicitud[i].fecha_envio = "01010001" THEN
            LET ga_solicitud[i].fecha_envio = ""
        END IF

        SELECT MAX(folio)
        INTO   vmax_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen

        SELECT MAX(sec_pension)
        INTO   vmax_sec_pension
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    folio            = vmax_folio

        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    folio       = vmax_folio
        AND    sec_pension = vmax_sec_pension

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = ga_solicitud[i].nss
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart .* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8073
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY ga_solicitud TO scr_1.*
        ON KEY (CONTROL-B)
            LET arr_c = ARR_CURR()
            LET pos = ARR_CURR()

            SELECT "OK"
            FROM   ret_beneficiario
            WHERE  nss         =  ga_solicitud[pos].nss
            AND    consecutivo =  ga_solicitud[pos].consecutivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET v_ejecuta="fglgo RETM810 ",ga_solicitud[pos].nss," ",ga_solicitud[pos].consecutivo," ","C"
                RUN v_ejecuta
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

     CLOSE WINDOW retm8073
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE #loc #smallint
        i                     ,
        sw_2                  ,
        vmodif             INTEGER

    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8073 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE (BORDER)
    DISPLAY " RETM808                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Modifica   Ctrl-B : Beneficiarios   Ctrl-C : Salir " AT 2,1
    LET int_flag = FALSE
    LET sw_2 = 2 ## confirma
    LET vmodif = 1

    IF vaccion = "M"  THEN

        LET txt_1 =" SELECT A.nss           ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #descripcion
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " A.fecha_ini_pen     ,",
                      " A.actuario          ,",
                      " A.aseguradora       ,",
                      " A.num_plan_pension  ,",
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
--                    " D.sec_pension       ,",
                      " ' '                 ,",
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.consecutivo       ,",
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " A.folio             ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D ",
                  " WHERE  A.nss = ",vnss,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'F'                ",
                  " AND   A.regimen         = 97                 ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_seguro     = C.tipo_seguro      ",
                  " AND   A.tipo_pension    = C.tipo_pension     ",
                  " AND   A.tipo_prestacion    = C.tipo_prestacion     ",
                  " AND   A.nss         = D.nss  ",
                  " AND   A.consecutivo = D.consecutivo  "
    ELSE
        CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                      B.n_rfc            ,
                                      A.n_unico          ,
                                      B.paterno          ,
                                      B.materno          ,
                                      B.nombres          ,
                                      A.tipo_seguro      ,
                                      A.tipo_pension     ,
                                      A.actuario         ,
                                      A.aseguradora      ,
                                      A.num_plan_pension ,
                                      A.sol_folio        ,
                                      A.fecha_captura    ,
                                      A.fecha_confirma   ,
                                      A.estado_solicitud ,
                                      A.folio            ,
                                      A.consecutivo

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                LET int_flag = TRUE
                EXIT CONSTRUCT

            ON KEY (Esc)
                LET int_flag = FALSE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            CLOSE WINDOW retm8073
            RETURN
        END IF

        LET txt_1 =" SELECT A.nss           ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #descripcion
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " A.fecha_ini_pen     ,",
                      " A.actuario          ,",
                      " A.aseguradora       ,",
                      " A.num_plan_pension  ,",
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " ' '                 ,",
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.consecutivo       ,",
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " A.folio             ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D ",
                  " WHERE  ",x_busca CLIPPED,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'F'                ",
                  " AND   A.regimen         = 97                 ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_seguro     = C.tipo_seguro      ",
                  " AND   A.tipo_pension    = C.tipo_pension     ",
                  " AND   A.tipo_prestacion    = C.tipo_prestacion     ",
                  " AND   A.nss         = D.nss  ",
                  " AND   A.consecutivo = D.consecutivo  "
    END IF

    PREPARE pre_7 FROM txt_1
    DECLARE cur_7 CURSOR FOR pre_7

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_7 INTO ga_solicitud[i].*
        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].nss
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF


        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].estado_solicitud <> reg_1.capturado THEN

            PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].descripcion,varmov
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
{
        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    sec_pension = ga_solicitud[i].sec_pension
}
        SELECT descripcion
        INTO   ga_solicitud[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   ga_solicitud[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = ga_solicitud[i].rechazo_cod
        END IF

        IF ga_solicitud[i].fecha_confirma = "01010001" THEN
            LET ga_solicitud[i].fecha_confirma = ""
        END IF

        IF ga_solicitud[i].fecha_modifica = "01010001" THEN
            LET ga_solicitud[i].fecha_modifica = ""
        END IF

        IF ga_solicitud[i].fecha_envio = "01010001" THEN
            LET ga_solicitud[i].fecha_envio = ""
        END IF

        SELECT MAX(folio)
        INTO   vmax_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen

        SELECT MAX(sec_pension)
        INTO   vmax_sec_pension
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    folio            = vmax_folio

        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    folio       = vmax_folio
        AND    sec_pension = vmax_sec_pension

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = ga_solicitud[i].nss
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
       CALL SET_COUNT(i-1)

       DISPLAY ARRAY ga_solicitud TO scr_1.*
          ON KEY ( CONTROL-C )
             LET int_flag=TRUE
             EXIT DISPLAY

      ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].nss CLIPPED," ",
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
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8073
       RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm8073
        RETURN
    END IF

    CALL construccion(ga_solicitud[pos].*,vmodif)

CLOSE WINDOW retm8073
END FUNCTION


FUNCTION construccion(reg_mod,vmodif2)
#c------------------------------------
    DEFINE reg_mod RECORD
        nss                LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_ret           LIKE ret_solicitud_tx.tipo_retiro      ,
        descripcion        LIKE tab_retiro.descripcion            ,
        tipo_prestacion    LIKE ret_det_datamart.curp             ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        actuario           LIKE ret_solicitud_tx.actuario         ,
        aseguradora        LIKE ret_solicitud_tx.aseguradora      ,
        num_plan_pension   LIKE ret_solicitud_tx.num_plan_pension ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod      ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc        ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        desc_estado        CHAR(25)                               ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma ,
        fecha_captura      DATE                                   ,
        fecha_modifica     DATE                                   ,
        fecha_confirma     DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio             ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio      ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE #loc #smallint
        vexiste_datamart      ,
        i                     SMALLINT

    DEFINE #loc #char
        opc                   CHAR(01)

    DEFINE #loc #integer
        vmodif2               INTEGER

    LET rg_datamart.nss                = reg_mod.nss
    LET rg_datamart.n_rfc              = reg_mod.n_rfc
    LET rg_datamart.n_unico            = reg_mod.n_unico
    LET rg_datamart.paterno            = reg_mod.paterno
    LET rg_datamart.materno            = reg_mod.materno
    LET rg_datamart.nombres            = reg_mod.nombres
    LET rg_datamart.tipo_retiro        = reg_mod.tipo_ret
    LET rg_datamart.descripcion        = reg_mod.descripcion
    LET rg_datamart.tipo_prestacion    = reg_mod.tipo_prestacion
    LET rg_datamart.desc_prestacion    = reg_mod.desc_prestacion
    LET rg_datamart.regimen            = reg_mod.regimen
    LET rg_datamart.tipo_seguro        = reg_mod.tipo_seguro
    LET rg_datamart.desc_seguro        = reg_mod.desc_seguro
    LET rg_datamart.tipo_pension       = reg_mod.tipo_pension
    LET rg_datamart.desc_pension       = reg_mod.desc_pension
    LET rg_datamart.fecha_ini_pen      = reg_mod.fecha_ini_pen
    LET rg_datamart.actuario           = reg_mod.actuario
    LET rg_datamart.aseguradora        = reg_mod.aseguradora
    LET rg_datamart.num_plan_pension   = reg_mod.num_plan_pension
    LET rg_datamart.sol_folio          = reg_mod.sol_folio
    LET rg_datamart.fecha_solicitud    = reg_mod.fecha_solicitud
    LET rg_datamart.sec_pension        = reg_mod.sec_pension
    LET rg_datamart.diag_datamart      = reg_mod.diag_datamart
    LET rg_datamart.nombre_datamart    = reg_mod.nombre_datamart
    LET rg_datamart.semanas_cotizadas  = reg_mod.semanas_cotizadas
    LET rg_datamart.cod_rechazo_ent    = reg_mod.cod_rechazo_ent
    LET rg_datamart.rechazo_cod        = reg_mod.rechazo_cod
    LET rg_datamart.rechazo_desc       = reg_mod.rechazo_desc
    LET rg_datamart.fecha_captura      = reg_mod.fecha_captura
    LET rg_datamart.fecha_modifica     = reg_mod.fecha_modifica
    LET rg_datamart.fecha_confirma     = reg_mod.fecha_confirma
    LET rg_datamart.fecha_liquida      = reg_mod.fecha_liquida
    LET rg_datamart.estado_solicitud   = reg_mod.estado_solicitud
    LET rg_datamart.desc_estado        = reg_mod.desc_estado
    LET rg_datamart.folio              = reg_mod.folio
    LET rg_datamart.fecha_envio        = reg_mod.fecha_envio
    LET rg_datamart.consecutivo        = reg_mod.consecutivo
    LET rg_datamart.usuario_captura    = reg_mod.usuario_captura
    LET rg_datamart.usuario_modifica   = reg_mod.usuario_modifica
    LET rg_datamart.usuario_confirma   = reg_mod.usuario_confirma


    SELECT "OK"
    FROM   ret_det_datamart
    WHERE  nss             = rg_datamart.nss
    AND    tipo_seguro     = rg_datamart.tipo_seguro
    AND    tipo_pension    = rg_datamart.tipo_pension
    AND    regimen         = rg_datamart.regimen
    AND    tipo_prestacion = rg_datamart.tipo_prestacion
    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
       LET vexiste_datamart = 0
    ELSE
       LET vexiste_datamart = 1
    END IF

    IF vexiste_datamart = 1 THEN
        CALL cambios_si_existe_datamart(vmodif2)
    ELSE
        CALL cambios_no_existe_datamart(reg_mod.*,vmodif2)
    END IF

END FUNCTION

FUNCTION elimina()
#e-----------------

    DEFINE #loc #smallint
        cont_reg2             ,
        cont_reg              ,
        arr                   ,
        src                   ,
        i                     SMALLINT

    OPEN WINDOW retm8075 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE (BORDER)
    DISPLAY " RETM808                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ENTER : Elimina   Ctrl-B : Beneficiarios   Ctrl-C : Salir " AT 2,1
    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.n_unico          ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
                                  A.tipo_seguro      ,
                                  A.tipo_pension     ,
                                  A.fecha_captura    ,
                                  A.fecha_confirma   ,
                                  A.actuario         ,
                                  A.aseguradora      ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo
        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT
        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm8075
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #descripcion
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " A.fecha_ini_pen     ,",
                      " A.actuario          ,",
                      " A.aseguradora       ,",
                      " A.num_plan_pension  ,",
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " ' '                 ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.consecutivo       ,",
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " A.folio             ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D ",
                  " WHERE ",x_busca CLIPPED,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'F'                ",
                  " AND   A.regimen         = 97                 ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_seguro     = C.tipo_seguro      ",
                  " AND   A.tipo_pension    = C.tipo_pension     ",
                  " AND   A.tipo_prestacion    = C.tipo_prestacion     ",
                  " AND   A.nss         = D.nss  ",
                  " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_11 FROM txt_1
    DECLARE cur_11 CURSOR FOR pre_11

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_11 INTO ga_solicitud[i].*
        IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].nss
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ( (ga_solicitud[i].estado_solicitud <> reg_1.capturado) AND 
             (ga_solicitud[i].estado_solicitud <> reg_1.precapturado)  ) THEN
                
            PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].descripcion,varmov
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
{
        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    sec_pension = ga_solicitud[i].sec_pension
}
        SELECT descripcion
        INTO   ga_solicitud[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   ga_solicitud[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = ga_solicitud[i].rechazo_cod
        END IF

        IF ga_solicitud[i].fecha_confirma = "01010001" THEN
            LET ga_solicitud[i].fecha_confirma = ""
        END IF

        IF ga_solicitud[i].fecha_modifica = "01010001" THEN
            LET ga_solicitud[i].fecha_modifica = ""
        END IF

        IF ga_solicitud[i].fecha_envio = "01010001" THEN
            LET ga_solicitud[i].fecha_envio = ""
        END IF

        SELECT MAX(folio)
        INTO   vmax_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen

        SELECT MAX(sec_pension)
        INTO   vmax_sec_pension
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    folio            = vmax_folio

        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    folio       = vmax_folio
        AND    sec_pension = vmax_sec_pension

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = ga_solicitud[i].nss
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8075
        RETURN
    END IF

    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY ga_solicitud TO scr_1.*

      ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].nss CLIPPED," ",
                         ga_solicitud[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
        ON KEY ( CONTROL-M )

            PROMPT "  DESEA ELIMAR EL REGISTRO S/N " FOR CHAR enter
            IF enter MATCHES "[Ss]" THEN
                  
               LET arr = ARR_CURR()
               LET src = SCR_LINE()

               SELECT A.estado_solicitud
               INTO   v_status
               FROM   ret_solicitud_tx A
               WHERE  A.nss         = ga_solicitud[arr].nss
               AND    A.consecutivo = ga_solicitud[arr].consecutivo

               IF ( (v_status <> reg_1.capturado) AND 
                    (v_status <> reg_1.precapturado)  ) THEN
                  PROMPT "  NO PUEDE SER ELIMINADO ESTE REGISTRO" FOR CHAR enter
                  EXIT DISPLAY
               ELSE
                  DELETE
                  FROM  ret_beneficiario
                  WHERE ret_beneficiario.nss         = ga_solicitud[arr].nss
                  AND   ret_beneficiario.consecutivo = ga_solicitud[arr].consecutivo

                  DELETE
                  FROM  ret_solicitud_tx
                  WHERE ret_solicitud_tx.nss = ga_solicitud[arr].nss
                  AND ret_solicitud_tx.consecutivo = ga_solicitud[arr].consecutivo

                  DELETE
                  FROM  ret_consecutivo
                  WHERE consecutivo = ga_solicitud[arr].consecutivo

                  ----- REVERSAR MARCAJE -----

                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = "F"

                  LET reg_rev.marca_cod   = s_tipo_movimiento
                  LET reg_rev.nss         = ga_solicitud[arr].nss
                  LET reg_rev.correlativo = ga_solicitud[arr].consecutivo

                  LET vestado_marca   = 40
                  LET vcodigo_rechazo = 0
                  LET vmarca_causa    = 0
                  LET vfecha_causa    = NULL

                  LET v_reversa = " EXECUTE PROCEDURE desmarca_cuenta('",
                                    reg_rev.nss,"',",
                    reg_rev.marca_cod,",",
                                    reg_rev.correlativo,",",
                    vestado_marca,",",
                    vmarca_causa,",' ",
                    usuario,"')"
                  PREPARE eje_rever03 FROM v_reversa
                  EXECUTE eje_rever03

                  DISPLAY "  REGISTRO ELIMINADO  ",""
                          AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                  EXIT DISPLAY
               END IF
            END IF
            EXIT DISPLAY

        ON KEY ( CONTROL-C )
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW retm8075
END FUNCTION

FUNCTION confirma()
#cf----------------

    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER,
        vmodif                INTEGER

    OPEN WINDOW retm8075 AT 2,3 WITH FORM "RETM8081" ATTRIBUTE (BORDER)
    DISPLAY " RETM808                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONFIRMA CAPTURA" AT 1,59 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE PLAN PRIVADO 97                   " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Confirma   Ctrl-B : Beneficiarios  Ctrl-C : Salir  Ctrl-F Rechazar " AT 2,1

    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM

    LET int_flag              = FALSE
    LET vmodif            = 2

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.n_unico          ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
                                  A.tipo_seguro      ,
                                  A.tipo_pension     ,
                                  A.fecha_captura    ,
                                  A.fecha_confirma   ,
                                  A.actuario         ,
                                  A.aseguradora      ,
                                  A.estado_solicitud ,
                                  A.folio            ,
                                  A.consecutivo
        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag = TRUE THEN
            LET int_flag = FALSE
        CLOSE WINDOW retm8075
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #descripcion
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " A.fecha_ini_pen     ,",
                      " A.actuario          ,",
                      " A.aseguradora       ,",
                      " A.num_plan_pension  ,",
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " ' '                 ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.consecutivo       ,",
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " A.folio             ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D ",
                  " WHERE ",x_busca CLIPPED,
                  " AND   A.estado_solicitud IN (?,?)            ",
                  " AND   A.nss = B.n_seguro                     ",
                  " AND   A.tipo_retiro     = 'F'                ",
                  " AND   A.regimen         = 97                 ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_seguro     = C.tipo_seguro      ",
                  " AND   A.tipo_pension    = C.tipo_pension     ",
                  " AND   A.tipo_prestacion    = C.tipo_prestacion     ",
                  " AND   A.nss         = D.nss  ",
                  " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_12 FROM txt_1
    DECLARE cur_12 CURSOR FOR pre_12

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_12 USING reg_1.precapturado, 
                         reg_1.capturado
                   INTO ga_solicitud[i].*

         IF ga_solicitud[i].diag_registro IS NULL OR
           ga_solicitud[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  ga_solicitud[i].diag_registro,
                  ga_solicitud[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = ga_solicitud[i].nss
           AND   consecutivo =  ga_solicitud[i].consecutivo
        END IF

        SELECT usuario_captura
        INTO   x_usuario
        FROM   ret_solicitud_tx
        WHERE  nss = ga_solicitud[i].nss
        AND    consecutivo = ga_solicitud[i].consecutivo

   {  IF x_usuario = usuario THEN
      PROMPT "USUARIO ES EL MISMO DE CAPTURA" ATTRIBUTE (REVERSE)
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

            PROMPT "SOLICITUD DEL NSS <",ga_solicitud[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   ga_solicitud[i].descripcion,varmov
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
{
        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    sec_pension = ga_solicitud[i].sec_pension
}
        SELECT descripcion
        INTO   ga_solicitud[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = ga_solicitud[i].estado_solicitud

        IF ga_solicitud[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   ga_solicitud[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = ga_solicitud[i].rechazo_cod
        END IF

        IF ga_solicitud[i].fecha_confirma = "01010001" THEN
            LET ga_solicitud[i].fecha_confirma = ""
        END IF

        IF ga_solicitud[i].fecha_modifica = "01010001" THEN
            LET ga_solicitud[i].fecha_modifica = ""
        END IF

        IF ga_solicitud[i].fecha_envio = "01010001" THEN
            LET ga_solicitud[i].fecha_envio = ""
        END IF

        SELECT A.estado_solicitud
        INTO   v_status
        FROM   ret_solicitud_tx A
        WHERE  A.nss         = ga_solicitud[i].nss
        AND    A.consecutivo = ga_solicitud[i].consecutivo

        SELECT MAX(folio)
        INTO   vmax_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen

        SELECT MAX(sec_pension)
        INTO   vmax_sec_pension
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    tipo_prestacion  = ga_solicitud[i].tipo_prestacion
        AND    tipo_seguro      = ga_solicitud[i].tipo_seguro
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    regimen          = ga_solicitud[i].regimen
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    folio            = vmax_folio

        SELECT nombre_datamart ,
               diag_datamart
        INTO   ga_solicitud[i].nombre_datamart ,
               ga_solicitud[i].diag_datamart
        FROM   ret_det_datamart
        WHERE  nss         = ga_solicitud[i].nss
        AND    folio       = vmax_folio
        AND    sec_pension = vmax_sec_pension

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = ga_solicitud[i].nss
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1

    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8075
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY ga_solicitud TO scr_1.*
        ON KEY ( CONTROL-C )
            CALL inicializa()
            LET int_flag = TRUE
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
            LET int_flag = TRUE
            EXIT DISPLAY

        ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",ga_solicitud[arr_c].nss CLIPPED," ",
                         ga_solicitud[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
        
        ON KEY (CONTROL-M)
            LET pos = ARR_CURR()

            { IF ga_solicitud[pos].usuario_captura = usuario THEN
               ERROR "    NO SE PUEDE CONFIRMAR CON EL MISMO USUARIO DE CAPTURA"
               ATTRIBUTE(NORMAL)
               CALL inicializa()
               LET int_flag = TRUE
            END IF }

            EXIT DISPLAY
    END DISPLAY

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm8075
        RETURN
    ELSE
        CALL construccion(ga_solicitud[pos].*,vmodif)
        CLOSE WINDOW retm8075
    END IF
END FUNCTION

FUNCTION cat_tipo_ret()
#ctr-------------------
    DEFINE c_descripcion LIKE tab_retiro.descripcion

    SELECT descripcion
    INTO   c_descripcion
    FROM   tab_retiro
    WHERE  tipo_retiro = rg_datamart.tipo_retiro

    RETURN c_descripcion

END FUNCTION


FUNCTION cat_tipo_prestacion()
#ctp--------------------------
    DEFINE c_descripcion LIKE tab_prestacion.descripcion

    SELECT descripcion
    INTO   c_descripcion
    FROM   tab_prestacion
    WHERE  tipo_prestacion = rg_datamart.tipo_prestacion

    RETURN c_descripcion
    DISPLAY c_descripcion
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
    DEFINE  ga_solicitud ARRAY[100] OF RECORD
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
                         " AND tipo_prestacion IN (4) ",
                         " ORDER BY 1 " CLIPPED
         PREPARE pre_5 FROM prepare_1
         DECLARE cur_5 CURSOR FOR pre_5

         LET pos = 1

         FOREACH cur_5 INTO ga_solicitud[pos].*
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
         DISPLAY ARRAY ga_solicitud TO scr_2.*
             ON KEY ( CONTROL-C )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET rg_datamart.tipo_prestacion = ga_solicitud[pos].codigo
                 LET rg_datamart.desc_prestacion = ga_solicitud[pos].descripcion
                 EXIT DISPLAY

         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
    END WHILE
    CLOSE WINDOW RETM8024
END FUNCTION


FUNCTION despliega_tipo_seguro()
#dts----------------------------
    DEFINE  ga_solicitud ARRAY[100] OF RECORD
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
                         " AND clave IN ('PP') ",
                         " ORDER BY 1 " CLIPPED
         PREPARE pre_9 FROM prepare_1
         DECLARE cur_9 CURSOR FOR pre_9

         LET pos = 1

         FOREACH cur_9 INTO ga_solicitud[pos].*
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
         DISPLAY ARRAY ga_solicitud TO scr_2.*
             ON KEY ( CONTROL-C )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET rg_datamart.tipo_seguro   = ga_solicitud[pos].codigo
                 LET rg_datamart.desc_seguro   = ga_solicitud[pos].descripcion
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
    DEFINE  ga_solicitud ARRAY[100] OF RECORD
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

         LET prepare_1 = " SELECT * FROM tab_pension ",
                         " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
         " AND   tipo_pension IN ",
         " ('RE','AS','OR','VI','VO','IP','IN','CE','VE')",
         " ORDER BY 1 " CLIPPED

         PREPARE pre_10 FROM prepare_1
         DECLARE cur_10 CURSOR FOR pre_10

         LET pos = 1

         FOREACH cur_10 INTO ga_solicitud[pos].*
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
         DISPLAY ARRAY ga_solicitud TO scr_2.*
             ON KEY ( CONTROL-C )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY

             ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 LET rg_datamart.tipo_pension   = ga_solicitud[pos].codigo
                 LET rg_datamart.desc_pension   = ga_solicitud[pos].descripcion
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

    PREPARE eje_marca FROM v_marca
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
{
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
}
FUNCTION pre_gunta()
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "  DESEAS ACTUALIZARLA   S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
             EXIT WHILE
         END IF
     END WHILE

     RETURN opc
END FUNCTION

FUNCTION pre_gunta2()
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "  DESEAS CONFIRMAR LA CAPTURA  S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
            EXIT WHILE
         END IF
     END WHILE
     RETURN opc
END FUNCTION

FUNCTION pre_gunta3()
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "  DESEAS RECHAZAR  S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
            EXIT WHILE
         END IF
     END WHILE
     RETURN opc
END FUNCTION

FUNCTION cambios_no_existe_datamart(reg_mod,vmodif2)

    DEFINE reg_mod RECORD
        nss                LIKE ret_det_datamart.nss              ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc            ,
        n_unico            LIKE afi_mae_afiliado.n_unico          ,
        paterno            LIKE afi_mae_afiliado.paterno          ,
        materno            LIKE afi_mae_afiliado.materno          ,
        nombres            LIKE afi_mae_afiliado.nombres          ,
        tipo_ret           LIKE ret_solicitud_tx.tipo_retiro      ,
        descripcion        LIKE tab_retiro.descripcion            ,
        tipo_prestacion    LIKE ret_det_datamart.curp             ,
        desc_prestacion    LIKE tab_prestacion.descripcion        ,
        regimen            LIKE ret_det_datamart.regimen          ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        desc_seguro        LIKE tab_seguro.descripcion            ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension     ,
        desc_pension       LIKE tab_pension.descripcion           ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        actuario           LIKE ret_solicitud_tx.actuario         ,
        aseguradora        LIKE ret_solicitud_tx.aseguradora      ,
        num_plan_pension   LIKE ret_solicitud_tx.num_plan_pension ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod      ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc        ,
        fecha_liquida      DATE                                   ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud ,
        desc_estado        CHAR(25)                               ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo      ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura  ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma ,
        fecha_captura      DATE                                   ,
        fecha_modifica     DATE                                   ,
        fecha_confirma     DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio              ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio     ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE  reg        RECORD #loc #reg
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion  ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension
    END RECORD


    DEFINE #loc #smallint
        vmodif2            INTEGER

    INPUT BY NAME rg_datamart.tipo_pension       ,
                  rg_datamart.fecha_ini_pen      ,
                  rg_datamart.actuario           ,
                  rg_datamart.aseguradora        ,
                  rg_datamart.num_plan_pension   ,
                  rg_datamart.sol_folio          ,
                  rg_datamart.fecha_solicitud    ,
                  rg_datamart.semanas_cotizadas  WITHOUT DEFAULTS

        AFTER FIELD tipo_pension

            IF rg_datamart.tipo_pension IS NULL THEN
                 CALL despliega_tipo_pension() #dtp
                 DISPLAY rg_datamart.desc_pension TO desc_pension

                 NEXT FIELD fecha_ini_pen
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                GROUP BY 1

                 DISPLAY rg_datamart.desc_pension TO desc_pension
                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_pension() #dtp
                                DISPLAY rg_datamart.desc_pension TO desc_pension
                                NEXT FIELD tipo_pension
                            ELSE
                                NEXT FIELD fecha_ini_pen
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY rg_datamart.desc_pension TO desc_pension

        BEFORE FIELD fecha_ini_pen
            IF vmodif2 = 1 THEN
                LET rg_datamart.fecha_modifica   = HOY
                LET rg_datamart.usuario_modifica = usuario

                DISPLAY BY NAME rg_datamart.fecha_modifica  ,
                                rg_datamart.usuario_modifica

            END IF

            IF vmodif2 = 2 THEN
                LET rg_datamart.fecha_confirma   = HOY
                LET rg_datamart.usuario_confirma = usuario

                DISPLAY BY NAME rg_datamart.fecha_confirma  ,
                                rg_datamart.usuario_confirma
            END IF

        AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_pension
            END IF

            IF rg_datamart.fecha_ini_pen IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_ini_pen
            ELSE
                IF rg_datamart.fecha_ini_pen > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_ini_pen
                END IF
                IF rg_datamart.fecha_ini_pen > rg_datamart.fecha_solicitud THEN
      ERROR "    FECHA INICIO DE PENSION > FECHA DE SOLICITUD" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                END IF
            END IF

        AFTER FIELD actuario
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_ini_pen
            END IF

            IF rg_datamart.actuario IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD actuario
            ELSE
                NEXT FIELD aseguradora
            END IF

        AFTER FIELD aseguradora
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD actuario
            END IF

            IF rg_datamart.actuario IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD aseguradora
            ELSE
                NEXT FIELD num_plan_pension
            END IF

        AFTER FIELD num_plan_pension
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
            END IF

        AFTER FIELD sol_folio

            IF vmodif2 = 1 THEN
                LET rg_datamart.fecha_modifica   = HOY
                LET rg_datamart.usuario_modifica = usuario

                DISPLAY BY NAME rg_datamart.fecha_modifica  ,
                                rg_datamart.usuario_modifica

            END IF

            IF vmodif2 = 2 THEN
                LET rg_datamart.fecha_confirma   = HOY
                LET rg_datamart.usuario_confirma = usuario

                DISPLAY BY NAME rg_datamart.fecha_confirma  ,
                                rg_datamart.usuario_confirma
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_plan_pension
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
            END IF

            IF rg_datamart.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF rg_datamart.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

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
        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF rg_datamart.semanas_cotizadas IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD semanas_cotizadas
            END IF
            ------------- xavier------------------
             ON KEY (CONTROL-F)
             IF xaccion = "F" THEN
                LET arr_c =  arr_curr()
                CALL  rechazar(rg_datamart.nss,rg_datamart.consecutivo,
                          rg_datamart.tipo_prestacion,rg_datamart.tipo_retiro)
                RETURNING  codigo,entidad
                LET vmodif2 = 3
                LET rg_datamart.cod_rechazo_ent = codigo
                DISPLAY BY NAME rg_datamart.cod_rechazo_ent
              END IF
            --------------------------------------

      ON KEY (ESC)
                IF rg_datamart.fecha_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_solicitud
                ELSE
                    IF rg_datamart.fecha_solicitud > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_solicitud
                    END IF
                END IF

                IF rg_datamart.fecha_ini_pen > rg_datamart.fecha_solicitud THEN
                    ERROR "    FECHA INICIO DE PENSION > FECHA DE SOLICITUD" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                END IF

                IF rg_datamart.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF
                LET int_flag = FALSE
        EXIT INPUT

      ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",rg_datamart.nss CLIPPED," ",
                         rg_datamart.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
            ON KEY ( CONTROL-C )
                LET int_flag = TRUE
                EXIT INPUT

            ON KEY ( INTERRUPT )
                LET int_flag = TRUE
                EXIT INPUT


    END INPUT
   
   IF int_flag <> TRUE THEN
    IF vmodif2 = 1  THEN
       CALL pre_gunta() RETURNING opc
       IF opc MATCHES "[Nn]" THEN
          DISPLAY "  MODIFICACION CANCELADA  "
                        AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
          CALL inicializa() #i
          RETURN
       END IF
    ELSE
       IF vmodif2 = 2 THEN
        IF rg_datamart.estado_solicitud = reg_1.capturado THEN            

            CALL pre_gunta2() RETURNING opc
          
            IF opc MATCHES "[Nn]" THEN
               DISPLAY "  CONFIRMACION CANCELADA  "
                              AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
               CALL inicializa() #i
               RETURN
            END IF
        ELSE
            DISPLAY " EL REGISTRO DEBE ENCONTRARSE EN ESTADO CAPTURADO " AT 22,1 ATTRIBUTE(REVERSE) 
            SLEEP 3
            CALL inicializa() #i
            RETURN                            
        END IF
      ELSE
          IF vmodif2 = 3 THEN
             CALL pre_gunta3() RETURNING opc
             IF opc MATCHES "[Nn]" THEN
                 DISPLAY "  RECHAZO  CANCELADO  "
                            AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                 CALL inicializa() #i
                 RETURN
             END IF
          END IF
       END IF
    END IF

    IF vmodif2 = 1 THEN

       WHENEVER ERROR CONTINUE
       UPDATE ret_solicitud_tx
       SET ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
           ret_solicitud_tx.num_plan_pension  = rg_datamart.num_plan_pension,
           ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
           ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
           ret_solicitud_tx.tipo_pension      = rg_datamart.tipo_pension     ,
           ret_solicitud_tx.fecha_ini_pen     = rg_datamart.fecha_ini_pen    ,
           ret_solicitud_tx.actuario          = rg_datamart.actuario         ,
           ret_solicitud_tx.aseguradora       = rg_datamart.aseguradora      ,
           ret_solicitud_tx.usuario_modifica  = USER ,
           ret_solicitud_tx.fecha_modifica    = TODAY
       WHERE ret_solicitud_tx.nss = rg_datamart.nss
       AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo
---omar
        IF SQLCA.SQLCODE < 0 THEN
           LET x_error = "UPDATE ret_solicitud_tx:",
                 "nss ",rg_datamart.nss,
                 "consecutivo ",rg_datamart.consecutivo,
                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

           PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS"
           FOR enter
           EXIT PROGRAM
         END IF
         WHENEVER ERROR STOP

       DISPLAY "  REGISTRO MODIFICADO  ","" AT 22,1
       ATTRIBUTE(REVERSE)
       SLEEP 3
       CALL inicializa() #i
       RETURN
   ELSE
      IF vmodif2 = 2 THEN
        IF rg_datamart.estado_solicitud = reg_1.capturado THEN
             WHENEVER ERROR CONTINUE
             UPDATE ret_solicitud_tx
             SET  ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
                ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
                ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
                ret_solicitud_tx.tipo_pension      = rg_datamart.tipo_pension     ,
                ret_solicitud_tx.fecha_ini_pen     = rg_datamart.fecha_ini_pen    ,
                ret_solicitud_tx.actuario          = rg_datamart.actuario ,
                ret_solicitud_tx.aseguradora       = rg_datamart.aseguradora   ,
                ret_solicitud_tx.usuario_confirma  = USER,
                ret_solicitud_tx.fecha_confirma    = TODAY,
                ret_solicitud_tx.estado_solicitud  = reg_1.confirmado
             WHERE ret_solicitud_tx.nss = rg_datamart.nss
             AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo
---omar     
            IF SQLCA.SQLCODE < 0 THEN
                LET x_error = "UPDATE ret_solicitus_tx:",
                         "nss ",rg_datamart.nss,
                         "consecutivo ",rg_datamart.consecutivo,
                         err_get(SQLCA.SQLCODE)
            
                           CALL errorlog(x_error CLIPPED)
            
                   PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS"
                   FOR enter
                   EXIT PROGRAM
            END IF
            WHENEVER ERROR STOP
            
            
            DISPLAY "  REGISTRO CONFIRMADO  ","" AT 22,1
                            ATTRIBUTE(REVERSE)
            SLEEP 3
            CALL inicializa() #i
            RETURN
        ELSE
            DISPLAY " EL REGISTRO DEBE ENCONTRARSE EN ESTADO CAPTURADO " AT 22,1 ATTRIBUTE(REVERSE) 
            SLEEP 3
            CALL inicializa() #i
            RETURN                            
        END IF
     ELSE
           --------------------------xavier----------------------
           --------- se rechaza el e registro ----------------
           IF vmodif2 = 3 THEN
              WHENEVER ERROR CONTINUE
              UPDATE ret_solicitud_tx
              SET ret_solicitud_tx.estado_solicitud  = reg_1.rechazado,
                  ret_solicitud_tx.cod_rechazo_ent   =
                                                 rg_datamart.cod_rechazo_ent,
                  ret_solicitud_tx.entidad =entidad
             WHERE ret_solicitud_tx.nss = rg_datamart.nss
             AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo

             IF SQLCA.SQLCODE < 0 THEN
                   LET x_error = "UPDATE ret_solicitud_tx:",
                                 "nss ",rg_datamart.nss,
                                 "consecutivo ",rg_datamart.consecutivo,
                                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

                   PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                   FOR enter
                   EXIT PROGRAM
              END IF
              WHENEVER ERROR STOP

              DISPLAY "  REGISTRO RECHAZADO   ","" AT 22,1
                        ATTRIBUTE(REVERSE)
              SLEEP 3
              CALL inicializa() #i
              RETURN
           END IF---------------- fin rechaza solicitud ---------------
     END IF
  END IF
 END IF
 LET int_flag = FALSE

END FUNCTION


FUNCTION cambios_si_existe_datamart(vmodif2)

    DEFINE #loc #smallint
        vmodif2            INTEGER


    INPUT BY NAME rg_datamart.sol_folio          ,
                  rg_datamart.fecha_solicitud    ,
                  rg_datamart.semanas_cotizadas  WITHOUT DEFAULTS

        AFTER FIELD sol_folio

            IF vmodif2 = 1 THEN
                LET rg_datamart.fecha_modifica   = HOY
                LET rg_datamart.usuario_modifica = usuario

                DISPLAY BY NAME rg_datamart.fecha_modifica  ,
                                rg_datamart.usuario_modifica

            END IF

            IF vmodif2 = 2 THEN
                LET rg_datamart.fecha_confirma   = HOY
                LET rg_datamart.usuario_confirma = usuario

                DISPLAY BY NAME rg_datamart.fecha_confirma  ,
                                rg_datamart.usuario_confirma
            END IF



            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD aseguradora
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
            END IF

            IF rg_datamart.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF rg_datamart.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

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
        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF rg_datamart.semanas_cotizadas IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD semanas_cotizadas
            END IF
            ------------- xavier------------------
             ON KEY (CONTROL-F)
             IF xaccion = "F" THEN
                LET arr_c =  arr_curr()
                CALL  rechazar(rg_datamart.nss,rg_datamart.consecutivo,
                          rg_datamart.tipo_prestacion,rg_datamart.tipo_retiro)
                RETURNING  codigo,entidad
                LET vmodif2 = 3
                LET rg_datamart.cod_rechazo_ent = codigo
                DISPLAY BY NAME rg_datamart.cod_rechazo_ent
              END IF
            --------------------------------------

            ON KEY (ESC)
                IF rg_datamart.fecha_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_solicitud
                ELSE
                    IF rg_datamart.fecha_solicitud > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_solicitud
                    END IF
                END IF

                IF rg_datamart.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF
                LET int_flag = FALSE
                EXIT INPUT

      ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",rg_datamart.nss CLIPPED," ",
                         rg_datamart.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
            ON KEY ( CONTROL-C )
                LET int_flag = TRUE
                EXIT INPUT

            ON KEY ( INTERRUPT )
                LET int_flag = TRUE
                EXIT INPUT
    END INPUT
  IF int_flag <> TRUE THEN
   IF vmodif2 = 1  THEN
      CALL pre_gunta() RETURNING opc
      IF opc MATCHES "[Nn]" THEN
         DISPLAY "  MODIFICACION CANCELADA  "
                        AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
         CALL inicializa() #i
         RETURN
      END IF
   ELSE
      IF vmodif2 = 2 THEN
        IF rg_datamart.estado_solicitud = reg_1.capturado THEN            
            CALL pre_gunta2() RETURNING opc
            IF opc MATCHES "[Nn]" THEN
               DISPLAY "  CONFIRMACION CANCELADA  "
                               AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
               CALL inicializa() #i
               RETURN
            END IF
        ELSE
            DISPLAY "  NO PUEDE CONFIRMARSE EN ESTADO PRECAPTURADO  "
                              AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
            CALL inicializa() #i
            RETURN                        
        END IF
        
      ELSE
          IF vmodif2 = 3 THEN
             CALL pre_gunta3() RETURNING opc
             IF opc MATCHES "[Nn]" THEN
                 DISPLAY "  RECHAZO  CANCELADO  "
                            AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                 CALL inicializa() #i
                 RETURN
             END IF
          END IF
      END IF
   END IF

   IF vmodif2 = 1 THEN
      WHENEVER ERROR CONTINUE
      UPDATE ret_solicitud_tx
      SET  ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
         ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
         ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
         ret_solicitud_tx.usuario_modifica  = USER,
         ret_solicitud_tx.fecha_modifica    = TODAY
      WHERE ret_solicitud_tx.nss = rg_datamart.nss
      AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo
---omar
      IF SQLCA.SQLCODE < 0 THEN
           LET x_error = "UPDATE ret_solicitud_tx:",
                 "nss ",rg_datamart.nss,
                 "consecutivo ",rg_datamart.consecutivo,
                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

           PROMPT " ERROR DE UPDATE ret-solicitud_tx AVISE A SISTEMAS "
           FOR enter
           EXIT PROGRAM
      END IF
      WHENEVER ERROR STOP

      DISPLAY "  REGISTRO MODIFICADO  ","" AT 22,1
                    ATTRIBUTE(REVERSE)
      SLEEP 3
      CALL inicializa() #i
      RETURN
   ELSE
      IF vmodif2 = 2 THEN
        IF rg_datamart.estado_solicitud = reg_1.capturado THEN
            WHENEVER ERROR CONTINUE
             UPDATE ret_solicitud_tx
             SET ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
                ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
                ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
                ret_solicitud_tx.usuario_confirma  = USER,
                ret_solicitud_tx.fecha_confirma    = TODAY,
                ret_solicitud_tx.estado_solicitud  = reg_1.confirmado
             WHERE ret_solicitud_tx.nss = rg_datamart.nss
             AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo
---omar     
             IF SQLCA.SQLCODE < 0 THEN
               LET x_error = "UPDATE ret_solicitud_tx:",
                     "nss ",rg_datamart.nss,
                     "consecutivo ",rg_datamart.consecutivo,
                     err_get(SQLCA.SQLCODE)
            
                       CALL errorlog(x_error CLIPPED)
            
               PROMPT " ERROR DE UPDATE ret-solicitud_tx AVISE A SISTEMAS "
               FOR enter
               EXIT PROGRAM
              END IF
              WHENEVER ERROR STOP
            
              DISPLAY "  REGISTRO CONFIRMADO  ","" AT 22,1 ATTRIBUTE(REVERSE)
              SLEEP 3
              CALL inicializa() #i
              RETURN
        ELSE
            DISPLAY "  NO PUEDE CONFIRMARSE EN ESTADO PRECAPTURADO  " AT 22,1 ATTRIBUTE(REVERSE) 
            SLEEP 3
            CALL inicializa() #i
            RETURN                        
        END IF
      ELSE
           --------------------------xavier----------------------
           --------- se rechaza el e registro ----------------
           IF vmodif2 = 3 THEN
              WHENEVER ERROR CONTINUE
              UPDATE ret_solicitud_tx
              SET ret_solicitud_tx.estado_solicitud  = reg_1.rechazado,
                  ret_solicitud_tx.cod_rechazo_ent   =
                                                   rg_datamart.cod_rechazo_ent,
                  ret_solicitud_tx.entidad =entidad
             WHERE ret_solicitud_tx.nss = rg_datamart.nss
             AND ret_solicitud_tx.consecutivo = rg_datamart.consecutivo
             IF SQLCA.SQLCODE < 0 THEN
                   LET x_error = "UPDATE ret_solicitud_tx:",
                                 "nss ",rg_datamart.nss,
                                 "consecutivo ",rg_datamart.consecutivo,
                                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)
                   PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                   FOR enter
                   EXIT PROGRAM
              END IF
              WHENEVER ERROR STOP

              DISPLAY "  REGISTRO RECHAZADO   ","" AT 22,1
                        ATTRIBUTE(REVERSE)
              SLEEP 3
              CALL inicializa() #i
              RETURN
           END IF---------------- fin rechaza solicitud ---------------
       END IF
    END IF
  END IF
  LET int_flag = FALSE
END FUNCTION

FUNCTION matriz_de_derecho1(reg_2)
#rmd------------------------
    DEFINE reg_2 RECORD #glo #reg_2
        regimen                      ,
        tipo_seguro                  ,
        tipo_pension         CHAR(02),
        tipo_prestacion      SMALLINT
    END RECORD

    DEFINE #glo #char
        enter                CHAR(01)

    DEFINE #glo #smallint
        x_grupo              SMALLINT

    SELECT grupo
    INTO   x_grupo
    FROM   ret_matriz_derecho
    WHERE  regimen         = reg_2.regimen
    AND    tipo_seguro     = reg_2.tipo_seguro
    AND    tipo_pension    = reg_2.tipo_pension
    AND    tipo_prestacion = reg_2.tipo_prestacion
    GROUP BY 1

    IF SQLCA.SQLCODE = NOTFOUND  THEN
        PROMPT " NO EXISTE RESGISTRO EN LA RET_MATRIZ_DERECHO " FOR CHAR enter
        EXIT PROGRAM
    END IF

    RETURN x_grupo

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
        cod_rechazo_ent  LIKE ret_parcial.cod_rechazo_ent,
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
              AND   r.entidad         = ventidad
              AND   r.tipo_retiro     IN ("D","G")

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

         PREPARE pre_14 FROM x_x
         DECLARE cur_14 CURSOR FOR pre_14
         LET pos = 1
         FOREACH cur_14 INTO l_reg[pos].*
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
