#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM801  => MANTENEDOR DE REGISTRO DE CERTIFICADO IMSS - RETIRO POR   #
#                     REINGRESO (TIPO RETIRO J)                                 #
#                                  VERSION COPPEL                               #
#Fecha creacion    => 16 DE ENERO DE 2004                                       #
#By                => MARCOS GODINEZ JIMENEZ                                    #
#Fecha actualiz.   => 20 DE OCTUBRE DE 2007                                     #
#Actualizacion     => XAVIER TORRES RIOS                                        #
#                  => Desarrollar la opcion para rechazar solicitudes de retiro #
#                     y seleccionar de catalogo el motivo de rechazo            #
#Fecha actualiz.   => 18 DE ABRIL DE 2008                                       #
#Actualizacion     => XAVIER TORRES RIOS                                        #
#                  => Se modifica para buscar el diagnostico de procesar de la  #
#                     tabla ret_solicitud_tx y el diagnostico de vivienda de la #
#                     tabla ret_monto_viv. En caso de no encontrarse alli se    #
#                     busca en ret_solicitud_rx                                 #
#Fecha actualiz.   => 31 DE AGOSTO DE 2010                                      #
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
        num_resolucion     INTEGER                                ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        periodo_pago       LIKE ret_solicitud_tx.periodo_pago     ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,--sil
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
    fecha_envio        DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio            ,
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
        num_resolucion     INTEGER                                ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        periodo_pago       LIKE ret_solicitud_tx.periodo_pago     ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,--sil
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
        fecha_envio        DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio            ,
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
        f_fecha_corte     ,
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
        c10_fecha_paso    CHAR(0010),
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
        desc_cod_rechazo  CHAR(0040),
        vmax_sec_pension  CHAR(0002),
        v_marca           CHAR(0100),
        enter             ,
        opc               CHAR(0001)

    DEFINE  #glo #smallint
       vtipo_prestacion   ,
        v_cod_rechazo     ,
        v_marca_res       ,
        v_marca_ent       ,
        v_status          ,
        v_conviv          ,
        s_capturado       ,
        s_procesado       ,
        s_pagado          ,
        s_tipo_movimiento ,
        sw_2              ,
        entidad          , --sil
        v_tipo_movimiento , --sil
        codigo            , --sil
        pos               SMALLINT

    DEFINE #glo #integer
        ult_consecutivo  ,
        vmax_folio       INTEGER

    DEFINE x_error        CHAR(500), ---omar
        arr_c          SMALLINT,
    x_usuario          CHAR(12),
    x_estado_solicitud CHAR(40),
    sw                 SMALLINT,
    xx_fecha_solicitud DATE

    DEFINE #glo #smallint
        vestado_marca       SMALLINT ,
        vcodigo_rechazo     SMALLINT ,
        vmarca_causa        SMALLINT ,
        vfecha_causa        DATE

    DEFINE reg_10   RECORD
      monto_accion_97 ,
      monto_accion_cv,
      monto_accion_es ,
      monto_accion_so,
      monto_accion_esp,
      d6_monto_viv_pos DECIMAL(16,6)
    END RECORD
     DEFINE varmov  like tab_retiro.movimiento
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM801.log")  ---omar

    CALL init()
    LET xaccion = " "
    OPEN WINDOW retm8011 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE(BORDER)
    DISPLAY " RETM801                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
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

        COMMAND KEY("F") "conFirMa" "Confirma Solicitud"
            LET xaccion = "F"
            CALL inicializa()
            CALL confirma() #c

        COMMAND KEY("S") "Salida" "Regresa al Menu"
            EXIT MENU

    END MENU
    END CASE
    CLOSE WINDOW retm8011
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

{
    LET option_afore = FALSE
    IF  s_codigo_afore  = "544" THEN
        LET option_afore = FALSE
    END IF
}
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
    DEFINE #loc #decimal
        v_saldo_ini_pen    DECIMAL(16,6) ,
        vconsecutivo       LIKE ret_solicitud_tx.consecutivo

    DEFINE #loc #smallint
        sw_1               ,
        s_grupo            ,
        s_subcta           ,
        vestado_solicitud  SMALLINT

    DEFINE  tipoprestacion ,
        tiposeguro         ,
        tipopension        CHAR(02)

    DEFINE #loc #integer
        ult_consecutivo    INTEGER

    OPEN WINDOW retm8012 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE (BORDER)
    DISPLAY " RETM801                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " AGREGA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega   Ctrl-C : Salir " AT 2,1

    LET sw_1               = 0
    LET rg_datamart.fecha_captura  = HOY
    LET rg_datamart.rechazo_cod    = 0
    LET v_conviv           = 0

    DISPLAY BY NAME rg_datamart.fecha_captura

    INPUT BY NAME rg_datamart.nss                ,
                  rg_datamart.tipo_seguro        ,
                  rg_datamart.tipo_pension       ,
                  rg_datamart.num_resolucion     ,
                  rg_datamart.fecha_ini_pen      ,
                  rg_datamart.fecha_resolucion   ,
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
                   LET rg_datamart.fecha_resolucion = vfecha_resolucion
           LET rg_datamart.fecha_ini_pen    = vfecha_ini_p


                   DISPLAY rg_datamart.nss              TO nss
                   DISPLAY rg_datamart.regimen          TO regimen
                   DISPLAY rg_datamart.tipo_prestacion  TO tipo_prestacion
                   DISPLAY rg_datamart.tipo_seguro      TO tipo_seguro
                   DISPLAY rg_datamart.tipo_pension     TO tipo_pension
                   DISPLAY rg_datamart.tipo_retiro      TO tipo_retiro
               DISPLAY rg_datamart.fecha_solicitud  TO fecha_solicitud
               DISPLAY rg_datamart.fecha_resolucion TO fecha_resolucion
                   DISPLAY rg_datamart.fecha_ini_pen    TO fecha_ini_pen

                   SELECT n_rfc   ,
                          n_unico ,
                          paterno ,
                          materno ,
                          nombres
                   INTO   rg_datamart.n_rfc   ,
                          rg_datamart.n_unico ,
                          rg_datamart.paterno ,
                          rg_datamart.materno ,
                          rg_datamart.nombres
                   FROM   afi_mae_afiliado
                   WHERE  n_seguro = rg_datamart.nss

                   SELECT A.descripcion
                   INTO   rg_datamart.desc_estado
                   FROM   ret_estado A
                   WHERE  A.estado_solicitud = reg_1.capturado

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
                   INTO   rg_datamart.fecha_resolucion ,
                          rg_datamart.sec_pension      ,
                          rg_datamart.diag_datamart    ,
                          rg_datamart.semanas_cotizadas
                   FROM   ret_det_datamart
                   WHERE  nss              = rg_datamart.nss
                   AND    fecha_resolucion = rg_datamart.fecha_resolucion
                   AND    tipo_prestacion  = rg_datamart.tipo_prestacion
                   AND    diag_datamart    in (101,300,301)
                   AND    folio            = vmax_folio
                   AND    sec_pension      = vmax_sec_pension  }

                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro     IN ("IV","CV")
                AND    diag_datamart    in (101,300,301,210,302,303)

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
                AND    tipo_seguro      IN ("IV","CV")
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension

                DISPLAY BY NAME rg_datamart.materno
                DISPLAY BY NAME rg_datamart.paterno
                DISPLAY BY NAME rg_datamart.nombres
                DISPLAY BY NAME rg_datamart.n_rfc
                DISPLAY BY NAME rg_datamart.n_unico
                DISPLAY BY NAME rg_datamart.desc_estado
                DISPLAY rg_datamart.desc_seguro  TO desc_seguro
                DISPLAY rg_datamart.desc_pension TO desc_pension
                DISPLAY rg_datamart.descripcion  TO descripcion
                DISPLAY BY NAME rg_datamart.fecha_resolucion
                DISPLAY BY NAME rg_datamart.sec_pension
                DISPLAY BY NAME rg_datamart.diag_datamart
                DISPLAY BY NAME rg_datamart.semanas_cotizadas
                DISPLAY BY NAME rg_datamart.desc_prestacion

            END IF

            IF sw_1 = 0 THEN

                LET rg_datamart.tipo_seguro = "IV"

        SELECT descripcion
        INTO   rg_datamart.desc_seguro
        FROM   tab_seguro
        WHERE  clave = rg_datamart.tipo_seguro

                LET rg_datamart.tipo_retiro    = "J"

                SELECT descripcion
                INTO   rg_datamart.descripcion
                FROM   tab_retiro
                WHERE  tipo_retiro = rg_datamart.tipo_retiro

                LET rg_datamart.tipo_prestacion = 10

                SELECT descripcion
                INTO   rg_datamart.desc_prestacion
                FROM   tab_prestacion
                WHERE  tipo_prestacion = rg_datamart.tipo_prestacion

                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET rg_datamart.consecutivo    = ult_consecutivo

                LET rg_datamart.regimen         = "73"
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
                                rg_datamart.tipo_seguro     ,
                                rg_datamart.desc_seguro ,
                                rg_datamart.descripcion     ,
                                rg_datamart.tipo_prestacion ,
                                rg_datamart.desc_prestacion ,
                                rg_datamart.regimen         ,
                                rg_datamart.fecha_captura   ,
                                rg_datamart.fecha_solicitud   ,
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
                NEXT FIELD nss
            END IF

            IF rg_datamart.tipo_pension IS NULL THEN
                 CALL despliega_tipo_pension() #dtp
                 DISPLAY rg_datamart.desc_pension TO desc_pension

                 NEXT FIELD num_resolucion
            ELSE

                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                GROUP BY 1

                 DISPLAY rg_datamart.desc_pension TO desc_pension

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_pension() #dtp
                                DISPLAY rg_datamart.desc_pension TO desc_pension

                                NEXT FIELD num_resolucion
                            ELSE
                                NEXT FIELD tipo_pension
                            END IF
                        END IF
                    END WHILE
                END IF

                SELECT "OK"
                FROM   ret_matriz_derecho
                WHERE  tipo_retiro = "J"
                AND    tipo_seguro     = "RI"  --rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    tipo_prestacion = rg_datamart.tipo_prestacion

                IF STATUS = NOTFOUND THEN
                    DISPLAY "  TIPO DE PENSION INVALIDO ","" AT 22,1
                    ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD tipo_pension
                END IF
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
            AND    tipo_retiro      = "J"
            AND    tipo_seguro      = rg_datamart.tipo_seguro
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_prestacion  = rg_datamart.tipo_prestacion

        SELECT estado_solicitud
        INTO   vestado_solicitud
        FROM   ret_solicitud_tx
        WHERE  nss              = rg_datamart.nss
            AND    tipo_retiro      = "J"
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

        AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_resolucion
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

                IF YEAR(rg_datamart.fecha_ini_pen) < 1970 THEN
                    ERROR "    FECHA NO PUEDE SER MENOR A 1970"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_ini_pen
                END IF


    {           -------- APORTACIONES POSTERIORES INICIO DE PENSION --------
        ERROR "PROCESANDO INFORMACION"

                CALL crea_tablas(rg_datamart.nss)  #ct
                CALL primer_paso(rg_datamart.nss)  #pp
                LET v_saldo_ini_pen = 0
                CALL segundo_paso(rg_datamart.nss,rg_datamart.fecha_ini_pen) #sp
                RETURNING  v_saldo_ini_pen
    }

    CALL calcula_montos_posteriores(rg_datamart.nss,
                        rg_datamart.fecha_ini_pen )
        RETURNING reg_10.monto_accion_97 , reg_10.monto_accion_cv,
          reg_10.monto_accion_es , reg_10.monto_accion_so,
          reg_10.monto_accion_esp, reg_10.d6_monto_viv_pos

        IF reg_10.monto_accion_97 IS NULL OR reg_10.monto_accion_97 < 0 THEN
       LET reg_10.monto_accion_97 = 0
        END IF
        IF reg_10.monto_accion_cv IS NULL OR reg_10.monto_accion_cv < 0 THEN
       LET reg_10.monto_accion_cv = 0
        END IF
        IF reg_10.monto_accion_es IS NULL OR reg_10.monto_accion_es < 0 THEN
       LET reg_10.monto_accion_es = 0
        END IF
        IF reg_10.monto_accion_so IS NULL OR reg_10.monto_accion_so < 0 THEN
       LET reg_10.monto_accion_so = 0
        END IF
        IF reg_10.monto_accion_esp IS NULL OR reg_10.monto_accion_esp < 0 THEN
       LET reg_10.monto_accion_esp = 0
        END IF
        IF reg_10.d6_monto_viv_pos IS NULL OR reg_10.d6_monto_viv_pos < 0 THEN
       LET reg_10.d6_monto_viv_pos = 0
        END IF

    LET v_saldo_ini_pen = reg_10.monto_accion_esp +
                  reg_10.d6_monto_viv_pos +
                  reg_10.monto_accion_so +
                  reg_10.monto_accion_es +
                  reg_10.monto_accion_cv +
                  reg_10.monto_accion_97

                IF v_saldo_ini_pen IS NULL  THEN
                    LET v_saldo_ini_pen = 0
                END IF


                IF v_saldo_ini_pen = 0 OR
                   v_saldo_ini_pen < 0 THEN
                    ERROR "    SIN APORTACIONES POSTERIORES A FIP"
                     ATTRIBUTE(NORMAL)
--                     NEXT FIELD fecha_ini_pen
                END IF

                { IF rg_datamart.fecha_ini_pen >= "07011997" THEN

                    SELECT MAX(folio)
                    INTO   vmax_folio
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = "IV"  # rg_datamart.tipo_seguro
                    AND    tipo_pension    = rg_datamart.tipo_pension
                    AND    regimen         = rg_datamart.regimen
                    AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen

                    SELECT MAX(sec_pension)
                    INTO   vmax_sec_pension
                    FROM   ret_det_datamart
                    WHERE  nss             = rg_datamart.nss
                    AND    tipo_seguro     = "IV"  # rg_datamart.tipo_seguro
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
                    AND    tipo_seguro     = "IV"   # rg_datamart.tipo_seguro
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

                        NEXT FIELD fecha_resolucion

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
                ELSE
                   AND    tipo_seguro     = "IV"  # rg_datamart.tipo_seguro
                   AND    tipo_pension    = rg_datamart.tipo_pension
                   AND    regimen         = rg_datamart.regimen
                   AND    fecha_ini_pen   = rg_datamart.fecha_ini_pen

                   SELECT MAX(sec_pension)
                   INTO   vmax_sec_pension
                   FROM   ret_det_datamart
                   WHERE  nss             = rg_datamart.nss
                   AND    tipo_seguro     = "IV"  # rg_datamart.tipo_seguro
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
                   AND    tipo_seguro     = "IV"  # rg_datamart.tipo_seguro
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

                       NEXT FIELD fecha_resolucion

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
                END IF }

                SELECT MAX(sec_pension)
                INTO   vmax_sec_pension
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro     IN ("CV","IV")
                AND    diag_datamart    in (101,300,301,210,302,303)

                SELECT fecha_resolucion  ,
                       sec_pension       ,
                       diag_datamart     ,
                       semanas_cotizadas ,
                       nombre_datamart
                INTO   rg_datamart.fecha_resolucion  ,
                       rg_datamart.sec_pension       ,
                       rg_datamart.diag_datamart     ,
                       rg_datamart.semanas_cotizadas ,
                       rg_datamart.nombre_datamart
                FROM   ret_det_datamart
                WHERE  nss              = rg_datamart.nss
                AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_seguro     IN ("CV","IV")
                AND    diag_datamart    in (101,300,301,210,302,303)
                AND    sec_pension      = vmax_sec_pension

                IF SQLCA.SQLCODE = NOTFOUND THEN
                   SELECT MAX(sec_pension)
                   INTO   vmax_sec_pension
                   FROM   ret_det_datamart
                   WHERE  nss              = rg_datamart.nss
                   AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                   AND    tipo_seguro     IN  ("CV","IV")
                   AND    diag_datamart    in (101,300,301,210,302,303)

                   SELECT fecha_resolucion  ,
                          sec_pension       ,
                          diag_datamart     ,
                          semanas_cotizadas ,
                          nombre_datamart
                   INTO   rg_datamart.fecha_resolucion  ,
                          rg_datamart.sec_pension       ,
                          rg_datamart.diag_datamart     ,
                          rg_datamart.semanas_cotizadas ,
                          rg_datamart.nombre_datamart
                   FROM   ret_det_datamart
                   WHERE  nss              = rg_datamart.nss
                   AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
                   AND    tipo_seguro     IN ("CV","IV")
                   AND    diag_datamart    in (101,300,301,210,302,303)
                   AND    sec_pension      = vmax_sec_pension
                   IF SQLCA.SQLCODE <> NOTFOUND THEN
                      ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
                      SLEEP 2
                      NEXT FIELD  tipo_seguro
                   ELSE
                      ERROR " NO SE ENCONTRO REGISTRO EN EL DATAMART .."
                      ATTRIBUTE(REVERSE)
                      SLEEP 2
                      NEXT FIELD fecha_resolucion
                   END IF
              ELSE
                 ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
                 ATTRIBUTE(REVERSE)
                 DISPLAY rg_datamart.fecha_resolucion  TO fecha_resolucion
                 DISPLAY rg_datamart.sec_pension       TO sec_pension
                 DISPLAY rg_datamart.tipo_prestacion   TO tipo_prestacion
                 DISPLAY rg_datamart.diag_datamart     TO diag_datamart
                 DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
                 DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart
                 NEXT FIELD sol_folio
              END IF
            END IF

        AFTER FIELD fecha_resolucion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_ini_pen
            END IF

            IF rg_datamart.fecha_resolucion IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_resolucion
            ELSE
                IF rg_datamart.fecha_resolucion > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_resolucion
                END IF
            END IF

        AFTER FIELD sol_folio
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_resolucion
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
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

                IF rg_datamart.fecha_ini_pen > rg_datamart.fecha_solicitud THEN
      ERROR "    FECHA INICIO DE PENSION > FECHA DE SOLICITUD" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                ELSE
                    IF (rg_datamart.fecha_solicitud - 1 UNITS YEAR) <
                        rg_datamart.fecha_ini_pen                  THEN
ERROR "    FECHA DE SOLICITUD DEBE SER UN AÑO POSTERIOR A FIP" ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_ini_pen
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
                    AND    tipo_retiro      = "J"
                    AND    tipo_seguro      = rg_datamart.tipo_seguro
                    AND    tipo_pension     = rg_datamart.tipo_pension
                    AND    tipo_prestacion  = rg_datamart.tipo_prestacion

                SELECT estado_solicitud
                INTO   vestado_solicitud
                FROM   ret_solicitud_tx
                WHERE  nss              = rg_datamart.nss
                    AND    tipo_retiro      = "J"
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
                    AND A.consecutivo_lote = rg_datamart.consecutivo
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

                IF rg_datamart.fecha_resolucion IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_resolucion
                ELSE
                    IF rg_datamart.fecha_resolucion > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_resolucion
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

                IF rg_datamart.fecha_ini_pen > rg_datamart.fecha_solicitud THEN
      ERROR "    FECHA INICIO DE PENSION > FECHA DE SOLICITUD" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_ini_pen
                ELSE
                    IF (rg_datamart.fecha_solicitud - 1 UNITS YEAR) <
                        rg_datamart.fecha_ini_pen                  THEN
ERROR "    FECHA DE SOLICITUD DEBE SER UN AÑO POSTERIOR A FIP" ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_ini_pen
                    END IF
                END IF
                IF rg_datamart.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF

                ----- MARCAJE -----

                SELECT movimiento
                INTO   s_tipo_movimiento
                FROM   tab_retiro
                WHERE  tipo_retiro = "J"

                CALL marca_cuenta (rg_datamart.nss           ,
                                   s_tipo_movimiento ,
                                   rg_datamart.consecutivo
                                  )#mc
                RETURNING v_marca_res ,
                          v_cod_rechazo

                LET rg_datamart.rechazo_cod = v_cod_rechazo

                IF v_cod_rechazo > 0 THEN

                    SELECT A.rechazo_desc
                    INTO   desc_cod_rechazo
                    FROM   tab_rch_marca A
                    WHERE  A.rechazo_cod = v_cod_rechazo

                    PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                          desc_cod_rechazo CLIPPED,
                          ") <ENTER> CONTINUAR"
                          FOR CHAR enter
                END IF

                --------------------
                   LET rg_datamart.tipo_seguro = "RI"
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

        WHENEVER ERROR CONTINUE        ---omar
                INSERT INTO ret_solicitud_tx
                VALUES(rg_datamart.nss                 ,
                       rg_datamart.consecutivo         ,
                       0                               , #folio
                       rg_datamart.sol_folio           ,
                       ' '                             , #tipo_i
                       rg_datamart.n_unico             , #curp
                       rg_datamart.sec_pension         , #sec_pension
                       0                               , #tipo_documento sil
                       rg_datamart.tipo_retiro            ,
                       rg_datamart.regimen             ,
                       rg_datamart.tipo_seguro         ,
                       rg_datamart.tipo_pension        ,
                       rg_datamart.tipo_prestacion     ,
                       rg_datamart.fecha_ini_pen       ,
                       rg_datamart.fecha_resolucion    ,
                       rg_datamart.fecha_solicitud     ,
                       ' '                             , #cve_doc_probatorio
                       '01010001'                      , #fecha_nacimiento
                       ' '                             , #aseguradora
                       ' '                             , #actuario
                       ' '                             , #num_plan_pension
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
                       ' '                             , #entidad
                       ' '                             , #cod_rechazo_ent
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
                       rg_datamart.num_resolucion      ,  #num_resolucion
                       ' '                             ,  #paterno_sol
                       ' '                             , #materno_sol
                       ' '
                      )

---omar
                IF SQLCA.SQLCODE < 0 THEN
           LET x_error = "INSERT ret_solicitud_tx:",
                 "nss ",rg_datamart.nss,
                 "consecutivo ",rg_datamart.consecutivo,
                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

           PROMPT " ERROR AL INSERTAR REGISTROS AVISE A SISTEMAS "
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

    CLOSE WINDOW retm8012
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        v_existe_datamart     ,
        i                     INTEGER

    DEFINE
        li_max_folio          INTEGER

    OPEN WINDOW retm8013 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE (BORDER)
    DISPLAY " RETM801                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Consulta   Ctrl-B : Beneficiarios  Ctrl-C : Salir " AT 2,1

    INITIALIZE rg_datamart.* TO NULL
    CLEAR FORM

    LET int_flag = FALSE
    LET v_existe_datamart = 0

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.n_unico          ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
                                  A.tipo_seguro      ,
                                  A.tipo_pension     ,
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
        CLOSE WINDOW retm8013
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
                      " A.num_resolucion    ,", #num_resolucion
                      " A.fecha_ini_pen     ,",
                      " A.fecha_resolucion  ,",
                      " A.periodo_pago      ,", #periodo_pago
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.sec_pension       ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,", #sil
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
                      " A.fecha_envio       ,",
                      " A.folio             ,",
                      " A.diag_registro,",
                      " D.estado_sub_viv ",
                      " FROM  ret_solicitud_tx A,",
                      "afi_mae_afiliado B,",
                      "ret_matriz_derecho C,",
                      "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_retiro     = 'J'                ",
                      " AND   A.regimen         = 73                 ",
                      " AND   A.regimen         = C.regimen          ",
                      " AND   A.tipo_retiro     = C.tipo_retiro      ",
                      " AND   A.tipo_seguro   IN ('IV','CV','RI')         ",
                      " AND   A.tipo_pension    = C.tipo_pension     ",
                      " AND   A.tipo_prestacion    = C.tipo_prestacion     ",
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
        --
        LET ga_solicitud[i].tipo_seguro = "IV"
        --
        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT MAX(folio)
        INTO   li_max_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        SELECT fecha_resolucion  ,
               sec_pension       ,
               diag_datamart     ,
               semanas_cotizadas ,
               nombre_datamart
        INTO   ga_solicitud[i].fecha_resolucion  ,
               ga_solicitud[i].sec_pension       ,
               ga_solicitud[i].diag_datamart     ,
               ga_solicitud[i].semanas_cotizadas ,
               ga_solicitud[i].nombre_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    folio            = li_max_folio
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    IN (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        IF SQLCA.SQLCODE = NOTFOUND THEN

          SELECT fecha_resolucion  ,
                 sec_pension       ,
                 diag_datamart     ,
                 semanas_cotizadas ,
                 nombre_datamart
          INTO   ga_solicitud[i].fecha_resolucion  ,
                 ga_solicitud[i].sec_pension       ,
                 ga_solicitud[i].diag_datamart     ,
                 ga_solicitud[i].semanas_cotizadas ,
                 ga_solicitud[i].nombre_datamart
          FROM   ret_det_datamart
          WHERE  nss              = ga_solicitud[i].nss
          AND    folio            = li_max_folio
          AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
          AND    tipo_seguro      IN ("IV","CV","RI")
          AND    diag_datamart    IN (101,300,301,210,302,303)
          AND    sec_pension      = ga_solicitud[i].sec_pension
          IF SQLCA.SQLCODE <> NOTFOUND THEN
             ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
             SLEEP 2
          ELSE
             ERROR " NO SE ENCONTRO REGISTRO EN EL DATAMART .."
             ATTRIBUTE(REVERSE)
             SLEEP 2
          END IF
       ELSE
          ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
          ATTRIBUTE(REVERSE)
          SLEEP 2
       END IF

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

    SELECT UNIQUE fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
    FROM   dis_cuenta
    WHERE  nss   = ga_solicitud[i].nss
    AND    folio = ga_solicitud[i].folio
    AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart .* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8013
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
               LET v_ejecuta="fglgo RETM810 ",ga_solicitud[pos].nss," ",
                                              ga_solicitud[pos].consecutivo," ",
                                              "C"
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

     CLOSE WINDOW retm8013
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE #loc #smallint
        i                     ,
        sw_2                  ,
        vmodif             INTEGER

    DEFINE
        li_max_folio          INTEGER

    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8013 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE (BORDER)
    DISPLAY " RETM801                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Modifica   Ctrl-B : Beneficiarios  Ctrl-C : Salir " AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 2 ## confirma
    LET vmodif = 1

    IF vaccion = "M" THEN
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
                      " A.num_resolucion    ,", #num_resolucion
                      " A.fecha_ini_pen     ,",
                      " A.fecha_resolucion  ,",
                      " A.periodo_pago      ,", #periodo_pago
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.sec_pension       ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,", #sil
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
              " A.fecha_envio       ,",
                      " A.folio             ,",
              " A.diag_registro,",
              " D.estado_sub_viv ",
                      " FROM  ret_solicitud_tx A,",
              "afi_mae_afiliado B,",
              "ret_matriz_derecho C,",
              "OUTER ret_monto_viv D ",
                      " WHERE  A.nss = ",vnss,
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_retiro     = 'J'                ",
                      " AND   A.regimen         = 73                 ",
                      " AND   A.regimen         = C.regimen          ",
                      " AND   A.tipo_retiro     = C.tipo_retiro      ",
                      " AND   A.tipo_seguro     IN ('IV','CV','RI')       ",
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
            CLOSE WINDOW retm8013
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
                      " A.num_resolucion    ,", #num_resolucion
                      " A.fecha_ini_pen     ,",
                      " A.fecha_resolucion  ,",
                      " A.periodo_pago      ,", #periodo_pago
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.sec_pension       ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,", #sil
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
                      " A.fecha_envio       ,",
                      " A.folio             ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  "OUTER ret_monto_viv D ",
                  " WHERE ",x_busca CLIPPED,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'J'                ",
                  " AND   A.regimen         = 73                 ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.tipo_seguro    IN ('IV','CV','RI')       ",
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
        --
        LET ga_solicitud[i].tipo_seguro = "IV"
          --
        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT MAX(folio)
        INTO   li_max_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        SELECT fecha_resolucion  ,
               sec_pension       ,
               diag_datamart     ,
               semanas_cotizadas ,
               nombre_datamart
        INTO   ga_solicitud[i].fecha_resolucion  ,
               ga_solicitud[i].sec_pension       ,
               ga_solicitud[i].diag_datamart     ,
               ga_solicitud[i].semanas_cotizadas ,
               ga_solicitud[i].nombre_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    folio            = li_max_folio
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    IN (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        IF SQLCA.SQLCODE = NOTFOUND THEN
           SELECT fecha_resolucion  ,
                  sec_pension       ,
                  diag_datamart     ,
                  semanas_cotizadas ,
                  nombre_datamart
           INTO   ga_solicitud[i].fecha_resolucion  ,
                  ga_solicitud[i].sec_pension       ,
                  ga_solicitud[i].diag_datamart     ,
                  ga_solicitud[i].semanas_cotizadas ,
                  ga_solicitud[i].nombre_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].nss
           AND    folio            = li_max_folio
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN ("IV","CV","RI")
           AND    diag_datamart    IN (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension
           IF SQLCA.SQLCODE <> NOTFOUND THEN
              ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
              SLEEP 2
           END IF
        ELSE
           ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
           ATTRIBUTE(REVERSE)
           DISPLAY ga_solicitud[i].fecha_resolucion  TO fecha_resolucion
           DISPLAY ga_solicitud[i].sec_pension       TO sec_pension
           DISPLAY ga_solicitud[i].tipo_seguro       TO tipo_seguro
           DISPLAY ga_solicitud[i].tipo_prestacion   TO tipo_prestacion
           DISPLAY ga_solicitud[i].diag_datamart     TO diag_datamart
           DISPLAY ga_solicitud[i].semanas_cotizadas TO semanas_cotizadas
           DISPLAY ga_solicitud[i].nombre_datamart   TO nombre_datamart
        END IF

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

    SELECT UNIQUE fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
    FROM   dis_cuenta
    WHERE  nss   = ga_solicitud[i].nss
    AND    folio = ga_solicitud[i].folio
    AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov

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

          ON KEY ( CONTROL-B )

              LET pos = ARR_CURR()
              SELECT "OK"
              FROM   ret_beneficiario
              WHERE  nss         =  ga_solicitud[pos].nss
              AND    consecutivo =  ga_solicitud[pos].consecutivo
          GROUP BY 1

              IF STATUS <> NOTFOUND THEN
                  LET v_ejecuta="fglgo RETM810 ",
                               ga_solicitud[pos].nss," ",
                               ga_solicitud[pos].consecutivo," ",
                              "B"
                  RUN v_ejecuta
              ELSE
                  ERROR " NO SE ENCONTRARON BENEFICIARIOS ..."
                  ATTRIBUTE(REVERSE) SLEEP 2
                 EXIT DISPLAY
              END IF

          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
             EXIT DISPLAY
       END DISPLAY
    ELSE
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8013
       RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm8013
        RETURN
    END IF

    CALL construccion(ga_solicitud[pos].*,vmodif)

CLOSE WINDOW retm8013
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
        num_resolucion     INTEGER                                ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        periodo_pago       LIKE ret_solicitud_tx.periodo_pago     ,
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
        fecha_envio        DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio              ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE #loc #smallint
        vexiste_datamart      ,
        i                     SMALLINT

    DEFINE
        li_max_folio          INTEGER

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
    LET rg_datamart.num_resolucion     = reg_mod.num_resolucion
    LET rg_datamart.fecha_ini_pen      = reg_mod.fecha_ini_pen
    LET rg_datamart.fecha_resolucion   = reg_mod.fecha_resolucion
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
    LET rg_datamart.fecha_envio        = reg_mod.fecha_envio
    LET rg_datamart.estado_solicitud   = reg_mod.estado_solicitud
    LET rg_datamart.desc_estado        = reg_mod.desc_estado
    LET rg_datamart.folio              = reg_mod.folio
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

    DEFINE
        li_max_folio          INTEGER

    OPEN WINDOW retm8015 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE (BORDER)
    DISPLAY " RETM801                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " ENTER : Elimina   Ctrl-B : Beneficiarios  Ctrl-C : Salir " AT 2,1

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
        CLOSE WINDOW retm8015
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
                      " A.num_resolucion    ,", #num_resolucion
                      " A.fecha_ini_pen     ,",
                      " A.fecha_resolucion  ,",
                      " A.periodo_pago      ,", #periodo_pago
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.sec_pension       ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,", # sil
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
                      " A.fecha_envio       ,",
                      " A.folio             ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                  " OUTER ret_monto_viv D    ",
                  " WHERE ",x_busca CLIPPED,
                  " AND   A.nss = B.n_seguro  ",
                  " AND   A.tipo_retiro     = 'J'                ",
                  " AND   A.regimen         = 73                 ",
                  " AND   A.regimen         = C.regimen          ",
                  " AND   A.tipo_retiro     = C.tipo_retiro      ",
                  " AND   A.tipo_seguro    IN ('IV','CV','RI')        ",
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
       --
        LET ga_solicitud[i].tipo_seguro = "IV"
       --
        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT MAX(folio)
        INTO   li_max_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        SELECT fecha_resolucion  ,
               sec_pension       ,
               diag_datamart     ,
               semanas_cotizadas ,
               nombre_datamart
        INTO   ga_solicitud[i].fecha_resolucion  ,
               ga_solicitud[i].sec_pension       ,
               ga_solicitud[i].diag_datamart     ,
               ga_solicitud[i].semanas_cotizadas ,
               ga_solicitud[i].nombre_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    folio            = li_max_folio
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    IN (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        IF SQLCA.SQLCODE = NOTFOUND THEN

          SELECT fecha_resolucion  ,
                 sec_pension       ,
                 diag_datamart     ,
                 semanas_cotizadas ,
                 nombre_datamart
          INTO   ga_solicitud[i].fecha_resolucion  ,
                 ga_solicitud[i].sec_pension       ,
                 ga_solicitud[i].diag_datamart     ,
                 ga_solicitud[i].semanas_cotizadas ,
                 ga_solicitud[i].nombre_datamart
          FROM   ret_det_datamart
          WHERE  nss              = ga_solicitud[i].nss
          AND    folio            = li_max_folio
          AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
          AND    tipo_seguro      IN ("IV","CV","RI")
          AND    diag_datamart    IN (101,300,301,210,302,303)
          AND    sec_pension      = ga_solicitud[i].sec_pension
          IF SQLCA.SQLCODE <> NOTFOUND THEN
             ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
             SLEEP 2
          END IF
       ELSE
          ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
          ATTRIBUTE(REVERSE)
       END IF

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

        SELECT UNIQUE fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss   = ga_solicitud[i].nss
        AND    folio = ga_solicitud[i].folio
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8015
        RETURN
    END IF

    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY ga_solicitud TO scr_1.*

        ON KEY ( CONTROL-B )

                 LET pos = ARR_CURR()
                 SELECT "OK"
                 FROM   ret_beneficiario
                 WHERE  nss         =  ga_solicitud[pos].nss
                 AND    consecutivo =  ga_solicitud[pos].consecutivo
                 GROUP BY 1

                 IF STATUS <> NOTFOUND THEN
                     LET v_ejecuta="fglgo RETM810 ",
                                  ga_solicitud[pos].nss," ",
                                  ga_solicitud[pos].consecutivo," ",
                                  "B"
                     RUN v_ejecuta
                 ELSE
                     ERROR " NO SE ENCONTRARON BENEFICIARIOS ..."
                     ATTRIBUTE(REVERSE) SLEEP 2
                     EXIT DISPLAY
                 END IF

        ON KEY ( CONTROL-M )

            PROMPT "  DESEA ELIMINAR EL REGISTRO S/N " FOR CHAR enter
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
                  ----- REVERSAR MARCAJE -----

                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = "J"

                  LET reg_rev.marca_cod   = s_tipo_movimiento
                  LET reg_rev.nss         = ga_solicitud[arr].nss
                  LET reg_rev.correlativo = ga_solicitud[arr].consecutivo
   {
                  LET v_reversa = " EXECUTE PROCEDURE reversa_marca('",
                                    reg_rev.nss,"',",reg_rev.marca_cod,",",
                                    reg_rev.correlativo,")"
                  PREPARE eje_rever03 FROM v_reversa
                  EXECUTE eje_rever03
   }

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

                  WHENEVER ERROR CONTINUE

                  DELETE
                  FROM  ret_beneficiario
                  WHERE ret_beneficiario.nss         = ga_solicitud[arr].nss
                  AND   ret_beneficiario.consecutivo = ga_solicitud[arr].consecutivo

---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_beneficiario:",
                   "nss ",ga_solicitud[arr].nss,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

                     PROMPT "   ERROR AL BORRAR EL REGISTRO AVISE A SISTEMAS "
             FOR enter
             EXIT PROGRAM
                  END IF
                  WHENEVER ERROR STOP

                  DELETE
                  FROM  ret_solicitud_tx
                  WHERE ret_solicitud_tx.nss = ga_solicitud[arr].nss
                  AND ret_solicitud_tx.consecutivo = ga_solicitud[arr].consecutivo
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_beneficiario:",
                   "nss ",ga_solicitud[arr].nss,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

                     PROMPT "   ERROR AL BORRAR EL REGISTRO AVISE A SISTEMAS "
             FOR enter
             EXIT PROGRAM
                  END IF
          WHENEVER ERROR STOP

                  DELETE
                  FROM  ret_consecutivo
                  WHERE consecutivo = ga_solicitud[arr].consecutivo


{
                  ----- REVERSAR MARCAJE -----

                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = "J"

                  LET reg_rev.marca_cod   = s_tipo_movimiento
                  LET reg_rev.nss         = ga_solicitud[arr].nss
                  LET reg_rev.correlativo = ga_solicitud[arr].consecutivo

                  LET v_reversa = " EXECUTE PROCEDURE reversa_marca('",
                                    reg_rev.nss,"',",reg_rev.marca_cod,",",
                                    reg_rev.correlativo,")"
                  PREPARE eje_rever03 FROM v_reversa
                  EXECUTE eje_rever03
}
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
CLOSE WINDOW retm8015
END FUNCTION

FUNCTION confirma()
#cf----------------

    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER,
        vmodif                INTEGER

    DEFINE
        li_max_folio          INTEGER


    OPEN WINDOW retm8015 AT 2,3 WITH FORM "RETM8011" ATTRIBUTE (BORDER)
    DISPLAY " RETM801                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONFIRMA CAPTURA" AT 1,59 ATTRIBUTE(REVERSE)
    DISPLAY "                    DATOS DE LA SOLICITUD DE REINGRESO                         " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY " <Ctrl-F> Rechazar" AT 1,1
    DISPLAY " <ENTER>:Selecciona    ESC:Confirma  Ctrl-B:Beneficiarios   Ctrl-C:Salir " AT 2,1



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
        CLOSE WINDOW retm8015
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
                      " A.num_resolucion    ,", #num_resolucion
                      " A.fecha_ini_pen     ,",
                      " A.fecha_resolucion  ,",
                      " A.periodo_pago      ,", #periodo_pago
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.sec_pension       ,", #sec_pension
                      " 0                   ,", #diag_datamart
                      " ' '                 ,", #nombre_datamart
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,", #sil
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
                      " A.fecha_envio       ,",
                      " A.folio             ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
               " FROM  ret_solicitud_tx A   ,",
               "       afi_mae_afiliado B   ,",
               "       ret_matriz_derecho C ,",
               "       OUTER ret_monto_viv D ",
               " WHERE ", x_busca CLIPPED,
               " AND   A.estado_solicitud IN (?,?) ",
               " AND   A.nss = B.n_seguro  ",
               " AND   A.tipo_retiro     = 'J'                ",
               " AND   A.regimen         = 73                 ",
               " AND   A.regimen         = C.regimen          ",
               " AND   A.tipo_seguro     IN ('IV','CV','RI') ",
               " AND   A.tipo_retiro     = C.tipo_retiro      ",
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

   { IF x_usuario = usuario THEN
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
        --
        LET ga_solicitud[i].tipo_seguro = "IV"
       --
        SELECT descripcion
        INTO   ga_solicitud[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = ga_solicitud[i].tipo_seguro

        SELECT descripcion
        INTO   ga_solicitud[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = ga_solicitud[i].tipo_pension

        SELECT MAX(folio)
        INTO   li_max_folio
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    in (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        SELECT fecha_resolucion  ,
               sec_pension       ,
               diag_datamart     ,
               semanas_cotizadas ,
               nombre_datamart
        INTO   ga_solicitud[i].fecha_resolucion  ,
               ga_solicitud[i].sec_pension       ,
               ga_solicitud[i].diag_datamart     ,
               ga_solicitud[i].semanas_cotizadas ,
               ga_solicitud[i].nombre_datamart
        FROM   ret_det_datamart
        WHERE  nss              = ga_solicitud[i].nss
        AND    folio            = li_max_folio
        AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
        AND    tipo_pension     = ga_solicitud[i].tipo_pension
        AND    tipo_seguro      IN ("IV","CV","RI")
        AND    diag_datamart    IN (101,300,301,210,302,303)
        AND    sec_pension      = ga_solicitud[i].sec_pension

        IF SQLCA.SQLCODE = NOTFOUND THEN

           SELECT fecha_resolucion  ,
                  sec_pension       ,
                  diag_datamart     ,
                  semanas_cotizadas ,
                  nombre_datamart
           INTO   ga_solicitud[i].fecha_resolucion  ,
                  ga_solicitud[i].sec_pension       ,
                  ga_solicitud[i].diag_datamart     ,
                  ga_solicitud[i].semanas_cotizadas ,
                  ga_solicitud[i].nombre_datamart
           FROM   ret_det_datamart
           WHERE  nss              = ga_solicitud[i].nss
           AND    folio            = li_max_folio
           AND    fecha_ini_pen    = ga_solicitud[i].fecha_ini_pen
           AND    tipo_seguro      IN ("IV","CV","RI")
           AND    diag_datamart    IN (101,300,301,210,302,303)
           AND    sec_pension      = ga_solicitud[i].sec_pension

           IF SQLCA.SQLCODE <> NOTFOUND THEN
              ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
              SLEEP 2
           END IF
        ELSE
           ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
           ATTRIBUTE(REVERSE)
           DISPLAY ga_solicitud[i].fecha_resolucion  TO fecha_resolucion
           DISPLAY ga_solicitud[i].sec_pension       TO sec_pension
           DISPLAY ga_solicitud[i].tipo_prestacion   TO tipo_prestacion
           DISPLAY ga_solicitud[i].tipo_seguro       TO tipo_seguro
           DISPLAY ga_solicitud[i].diag_datamart     TO diag_datamart
           DISPLAY ga_solicitud[i].semanas_cotizadas TO semanas_cotizadas
           DISPLAY ga_solicitud[i].nombre_datamart   TO nombre_datamart
        END IF

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

        SELECT A.estado_solicitud
        INTO   v_status
        FROM   ret_solicitud_tx A
        WHERE  A.nss         = ga_solicitud[i].nss
        AND    A.consecutivo = ga_solicitud[i].consecutivo

    SELECT UNIQUE fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
    FROM   dis_cuenta
    WHERE  nss   = ga_solicitud[i].nss
    AND    folio = ga_solicitud[i].folio
    AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov

        LET i = i + 1
    END FOREACH

    ERROR ""
    IF i = 1 THEN
        INITIALIZE rg_datamart.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8015
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
        ------------------------------------------------
    END DISPLAY

    IF int_flag = TRUE THEN
        CLOSE WINDOW retm8015
        RETURN
        LET int_flag=FALSE
    ELSE
        CALL construccion(ga_solicitud[pos].*,vmodif)
        CLOSE WINDOW retm8015
    END IF
    LET xaccion = " "
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
                         " AND tipo_prestacion = 3 ",
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
    CLOSE WINDOW retm8024
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
                         " AND clave IN ('RI') ",
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

FUNCTION rechazar(p_nss, p_consecutivo, p_tipo_prestacion,p_tipo_retiro)

  DEFINE
     p_nss             LIKE  ret_parcial.nss,
     p_consecutivo     LIKE  ret_parcial.consecutivo,
     p_tipo_prestacion LIKE  ret_parcial.tipo_prestacion,
     p_tipo_retiro     LIKE  ret_rechazo_grl.tipo_retiro

     DEFINE g_reg RECORD
        nss              LIKE ret_parcial.nss,
        consecutivo      LIKE ret_parcial.consecutivo,
        cod_rechazo_ent  LIKE ret_solicitud_tx.cod_rechazo_ent,
        cod_rec_ent_des  CHAR (60),
        tipo_prestacion  LIKE ret_parcial.tipo_prestacion,
        tipo_retiro      LIKE ret_rechazo_grl.tipo_retiro
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

   SELECT e.entidad
   INTO ventidad
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
               AND  r.entidad = ventidad
               AND  r.tipo_retiro IN ("D","G")

               IF SQLCA.SQLCODE = NOTFOUND THEN
                  ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
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
                         " AND   tipo_pension IN ('CE','VE')",
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
         PROMPT " ESTA SEGURO DE RECHAZAR LA SOLICITUD S/N ? " FOR opc
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
        num_resolucion     INTEGER                                ,
        fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen    ,
        fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion ,
        periodo_pago       LIKE ret_solicitud_tx.periodo_pago     ,
        sol_folio          INTEGER                                ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud  ,
        sec_pension        LIKE ret_det_datamart.sec_pension      ,
        diag_datamart      LIKE ret_det_datamart.diag_datamart    ,
        nombre_datamart    CHAR(44)                               ,
        semanas_cotizadas  LIKE ret_det_datamart.semanas_cotizadas,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent  ,#sil
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
        fecha_envio        DATE                                   ,
        folio              LIKE ret_solicitud_tx.folio              ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE #loc #smallint
        vmodif2            INTEGER

    DEFINE  reg        RECORD #loc #reg
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion  ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro      ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension
    END RECORD

    DEFINE #loc #decimal
        v_saldo_ini_pen    DECIMAL(16,6)

    DEFINE li_tipo_movimiento SMALLINT

    INPUT BY NAME rg_datamart.tipo_seguro        ,
                  rg_datamart.tipo_pension       ,
                  rg_datamart.num_resolucion     ,
                  rg_datamart.fecha_ini_pen      ,
                  rg_datamart.fecha_resolucion   ,
                  rg_datamart.sol_folio          ,
                  rg_datamart.fecha_solicitud    ,
                  rg_datamart.semanas_cotizadas  WITHOUT DEFAULTS

        AFTER FIELD tipo_seguro

            IF rg_datamart.tipo_seguro IS NULL THEN
                 CALL despliega_tipo_seguro() #dts
                 DISPLAY rg_datamart.desc_seguro TO desc_seguro

                 NEXT FIELD  tipo_pension
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_seguro
                FROM   tab_seguro
                WHERE  clave = rg_datamart.tipo_seguro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE SEGURO    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_seguro() #dts
                                DISPLAY rg_datamart.desc_seguro TO desc_seguro
                                NEXT FIELD tipo_pension
                            ELSE
                                NEXT FIELD tipo_seguro
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY rg_datamart.desc_seguro TO desc_seguro

        AFTER FIELD tipo_pension

            IF rg_datamart.tipo_pension IS NULL THEN
                 CALL despliega_tipo_pension() #dtp
                 DISPLAY rg_datamart.desc_pension TO desc_pension

                 --NEXT FIELD num_resolucion
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_pension() #dtp
                                DISPLAY rg_datamart.desc_pension TO desc_pension
                                NEXT FIELD tipo_pension
                            --ELSE
                             --   NEXT FIELD num_resolucion
                            END IF
                        END IF
                    END WHILE
                END IF

                SELECT "OK"
                FROM   ret_matriz_derecho
                WHERE  tipo_retiro = "J"
                AND    tipo_seguro     = "RI"    --rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    tipo_prestacion = rg_datamart.tipo_prestacion

                IF STATUS = NOTFOUND THEN
                    DISPLAY "  TIPO DE PENSION INVALIDO ","" AT 22,1
                    ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD tipo_pension
                END IF
            END IF

            DISPLAY rg_datamart.desc_pension TO desc_pension

            { SELECT MAX(sec_pension)
            INTO   vmax_sec_pension
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      = "RI"
            AND    diag_datamart    in (101,300,301,210,302,303) }

            SELECT fecha_resolucion  ,
                   sec_pension       ,
                   diag_datamart     ,
                   semanas_cotizadas ,
                   nombre_datamart
            INTO   rg_datamart.fecha_resolucion  ,
                   rg_datamart.sec_pension       ,
                   rg_datamart.diag_datamart     ,
                   rg_datamart.semanas_cotizadas ,
                   rg_datamart.nombre_datamart
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      IN ("IV","CV","RI")
            AND    diag_datamart    in (101,300,301,210,302,303)
            AND    sec_pension      = rg_datamart.sec_pension

            IF SQLCA.SQLCODE = NOTFOUND THEN
               { SELECT MAX(sec_pension)
               INTO   vmax_sec_pension
               FROM   ret_det_datamart
               WHERE  nss              = rg_datamart.nss
               AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
               AND    tipo_seguro      = "RI"
               AND    diag_datamart    in (101,300,301,210,302,303) }

               SELECT fecha_resolucion  ,
                      sec_pension       ,
                      diag_datamart     ,
                      semanas_cotizadas ,
                      nombre_datamart
               INTO   rg_datamart.fecha_resolucion  ,
                      rg_datamart.sec_pension       ,
                      rg_datamart.diag_datamart     ,
                      rg_datamart.semanas_cotizadas ,
                      rg_datamart.nombre_datamart
               FROM   ret_det_datamart
               WHERE  nss              = rg_datamart.nss
               AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
               AND    tipo_seguro      IN ("IV","CV","RI")
               AND    diag_datamart    in (101,300,301,210,302,303)
               AND    sec_pension      = rg_datamart.sec_pension
               IF SQLCA.SQLCODE <> NOTFOUND THEN
                  ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
                  SLEEP 2
                  NEXT FIELD  tipo_seguro
               ELSE
                  ERROR " NO SE ENCONTRO REGISTRO EN EL DATAMART .."
                  ATTRIBUTE(REVERSE)
                  SLEEP 2
                  NEXT FIELD  tipo_seguro
               END IF
            ELSE
               ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
               ATTRIBUTE(REVERSE)
               DISPLAY rg_datamart.fecha_resolucion  TO fecha_resolucion
               DISPLAY rg_datamart.sec_pension       TO sec_pension
               DISPLAY rg_datamart.tipo_prestacion   TO tipo_prestacion
               DISPLAY rg_datamart.tipo_seguro       TO tipo_seguro
               DISPLAY rg_datamart.diag_datamart     TO diag_datamart
               DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
               DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart
               NEXT FIELD fecha_resolucion
            END IF

        BEFORE FIELD num_resolucion
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

        AFTER FIELD num_resolucion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD semanas_cotizadas
            END IF

            DISPLAY rg_datamart.num_resolucion TO num_resolucion

        AFTER FIELD fecha_ini_pen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_resolucion
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
                IF YEAR(rg_datamart.fecha_ini_pen) < 1970 THEN
                    ERROR "    FECHA NO PUEDE SER MENOR A 1970"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_ini_pen
                END IF

                -------- APORTACIONES POSTERIORES INICIO DE PENSION --------
{
                CALL crea_tablas(rg_datamart.nss)  #ct
                CALL primer_paso(rg_datamart.nss)  #pp
                LET v_saldo_ini_pen = 0
                CALL segundo_paso(rg_datamart.nss,rg_datamart.fecha_ini_pen) #sp
                RETURNING  v_saldo_ini_pen
}

    CALL calcula_montos_posteriores(rg_datamart.nss,
                        rg_datamart.fecha_ini_pen )
        RETURNING reg_10.monto_accion_97 , reg_10.monto_accion_cv,
          reg_10.monto_accion_es , reg_10.monto_accion_so,
          reg_10.monto_accion_esp, reg_10.d6_monto_viv_pos

        IF reg_10.monto_accion_97 IS NULL OR reg_10.monto_accion_97 < 0 THEN
       LET reg_10.monto_accion_97 = 0
        END IF
        IF reg_10.monto_accion_cv IS NULL OR reg_10.monto_accion_cv < 0 THEN
       LET reg_10.monto_accion_cv = 0
        END IF
        IF reg_10.monto_accion_es IS NULL OR reg_10.monto_accion_es < 0 THEN
       LET reg_10.monto_accion_es = 0
        END IF
        IF reg_10.monto_accion_so IS NULL OR reg_10.monto_accion_so < 0 THEN
       LET reg_10.monto_accion_so = 0
        END IF
        IF reg_10.monto_accion_esp IS NULL OR reg_10.monto_accion_esp < 0 THEN
       LET reg_10.monto_accion_esp = 0
        END IF
        IF reg_10.d6_monto_viv_pos IS NULL OR reg_10.d6_monto_viv_pos < 0 THEN
       LET reg_10.d6_monto_viv_pos = 0
        END IF

    LET v_saldo_ini_pen = reg_10.monto_accion_esp +
                  reg_10.d6_monto_viv_pos +
                  reg_10.monto_accion_so +
                  reg_10.monto_accion_es +
                  reg_10.monto_accion_cv +
                  reg_10.monto_accion_97

                IF v_saldo_ini_pen IS NULL  THEN
                    LET v_saldo_ini_pen = 0
                END IF

                IF v_saldo_ini_pen = 0 OR
                   v_saldo_ini_pen < 0 THEN
                    ERROR "    SIN APORTACIONES POSTERIORES A FIP"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_ini_pen
                END IF

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
                NEXT FIELD num_resolucion
            END IF

        AFTER FIELD fecha_resolucion
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_ini_pen
            END IF

            IF rg_datamart.fecha_resolucion IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_resolucion
            ELSE
                IF rg_datamart.fecha_resolucion > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_resolucion
                END IF
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD sol_folio
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

        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF rg_datamart.semanas_cotizadas IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD semanas_cotizadas
            END IF

            ------------- sil------------------
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
                ELSE
                    IF (rg_datamart.fecha_solicitud - 1 UNITS YEAR) <
                        rg_datamart.fecha_ini_pen                  THEN
                        ERROR "    FECHA DE SOLICITUD DEBE SER UN AÑO POSTERIOR A FIP" ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_ini_pen
                    END IF
                END IF

                IF rg_datamart.fecha_solicitud <
                   rg_datamart.fecha_resolucion THEN
                    ERROR "    FECHA SOLICITUD < FECHA RESOLUCION" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_resolucion
                END IF

                IF rg_datamart.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF
        EXIT INPUT
      ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",rg_datamart.nss CLIPPED," ",
                         rg_datamart.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
            ON KEY ( CONTROL-C )
                EXIT INPUT

            ON KEY ( INTERRUPT )
                EXIT INPUT

       END INPUT

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
         IF vmodif2 =3 THEN
            CALL pre_gunta3() RETURNING opc
            IF opc MATCHES "[Nn]" THEN
               DISPLAY "  RECHAZO CANCELADO  "
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
          ret_solicitud_tx.num_resolucion    = rg_datamart.num_resolucion  ,
          ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
          ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
          ret_solicitud_tx.tipo_pension      = rg_datamart.tipo_pension     ,
          ret_solicitud_tx.fecha_ini_pen     = rg_datamart.fecha_ini_pen    ,
          ret_solicitud_tx.fecha_resolucion  = rg_datamart.fecha_resolucion ,
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

           PROMPT " ERROR de UPDATE ret_solicitud_tx AVISE A SISTEMAS "
           FOR enter
       END IF
       WHENEVER ERROR STOP

       DISPLAY "  REGISTRO MODIFICADO  ","" AT 22,1
                    ATTRIBUTE(REVERSE)
       SLEEP 2
       CALL inicializa() #i
       RETURN
    ELSE
--       IF vmodif2 = 2 THEN

        IF (vmodif2 = 2) AND (rg_datamart.estado_solicitud = reg_1.capturado) THEN
          WHENEVER ERROR CONTINUE
          UPDATE ret_solicitud_tx
          SET  ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
             ret_solicitud_tx.num_resolucion    = rg_datamart.num_resolucion  ,
             ret_solicitud_tx.fecha_solicitud   = rg_datamart.fecha_solicitud  ,
             ret_solicitud_tx.semanas_cotizadas = rg_datamart.semanas_cotizadas,
             ret_solicitud_tx.tipo_pension      = rg_datamart.tipo_pension     ,
             ret_solicitud_tx.fecha_ini_pen     = rg_datamart.fecha_ini_pen    ,
             ret_solicitud_tx.fecha_resolucion  = rg_datamart.fecha_resolucion ,
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

           PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS "
           FOR enter
          END IF
         WHENEVER ERROR STOP

          DISPLAY "  REGISTRO CONFIRMADO  ","" AT 22,1
                        ATTRIBUTE(REVERSE)
          SLEEP 3
          CALL inicializa() #i
          RETURN
       ELSE
           --------------------------sil----------------------
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
                   --
                   --error "entre al update de la linea 3801"
                   --sleep 5
                   --

                   PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                   FOR enter
                   EXIT PROGRAM
              END IF
              WHENEVER ERROR STOP

              --*****DESMARCAR CUENTA****************************************
              SELECT movimiento
              INTO   li_tipo_movimiento
              FROM   tab_retiro
              WHERE  tipo_retiro = "J"

              LET vestado_marca   = 0
              LET vcodigo_rechazo = 0
              LET vmarca_causa    = 0
              LET vfecha_causa    = NULL

              EXECUTE eje_desmarca USING rg_datamart.nss,
                                         li_tipo_movimiento,
                                         rg_datamart.consecutivo,
                                         vestado_marca,
                                                 vmarca_causa,
                                                 usuario

              DISPLAY "  REGISTRO RECHAZADO   ","" AT 22,1
                        ATTRIBUTE(REVERSE)
              SLEEP 3

              CALL inicializa() #i
              RETURN
           END IF---------------- fin rechaza solicitud ---------------
       END IF
    END IF

END FUNCTION


FUNCTION cambios_si_existe_datamart(vmodif2)

    DEFINE #loc #smallint
        vmodif2            INTEGER

    DEFINE li_tipo_movimiento SMALLINT


    INPUT BY NAME rg_datamart.tipo_seguro        ,
                  rg_datamart.tipo_pension       ,
                  rg_datamart.sol_folio          ,
                  rg_datamart.fecha_solicitud    ,
                  rg_datamart.semanas_cotizadas  WITHOUT DEFAULTS

        AFTER FIELD tipo_seguro

            IF rg_datamart.tipo_seguro IS NULL THEN
                 CALL despliega_tipo_seguro() #dts
                 DISPLAY rg_datamart.desc_seguro TO desc_seguro

                 NEXT FIELD  tipo_pension
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_seguro
                FROM   tab_seguro
                WHERE  clave = rg_datamart.tipo_seguro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE SEGURO    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_seguro() #dts
                                DISPLAY rg_datamart.desc_seguro TO desc_seguro
                                NEXT FIELD tipo_pension
                            ELSE
                                NEXT FIELD tipo_seguro
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY rg_datamart.desc_seguro TO desc_seguro

        AFTER FIELD tipo_pension

            IF rg_datamart.tipo_pension IS NULL THEN
                 CALL despliega_tipo_pension() #dtp
                 DISPLAY rg_datamart.desc_pension TO desc_pension

                 --NEXT FIELD sol_folio
            ELSE
                SELECT descripcion
                INTO   rg_datamart.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = rg_datamart.tipo_pension
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_pension() #dtp
                                DISPLAY rg_datamart.desc_pension TO desc_pension
                                NEXT FIELD tipo_pension
                            --ELSE
                             --   NEXT FIELD sol_folio
                            END IF
                        END IF
                    END WHILE
                END IF

                SELECT "OK"
                FROM   ret_matriz_derecho
                WHERE  tipo_retiro = "J"
                AND    tipo_seguro     = "RI"   --rg_datamart.tipo_seguro
                AND    tipo_pension    = rg_datamart.tipo_pension
                AND    tipo_prestacion = rg_datamart.tipo_prestacion

                IF STATUS = NOTFOUND THEN
                    DISPLAY "  TIPO DE PENSION INVALIDO ","" AT 22,1
                    ATTRIBUTE(NORMAL) SLEEP 3
                    NEXT FIELD tipo_pension
                END IF
            END IF

            DISPLAY rg_datamart.desc_pension TO desc_pension

            { SELECT MAX(sec_pension)
            INTO   vmax_sec_pension
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      = "RI"
            AND    diag_datamart    in (101,300,301,210,302,303) }

            SELECT fecha_resolucion  ,
                   diag_datamart     ,
                   semanas_cotizadas ,
                   nombre_datamart
            INTO   rg_datamart.fecha_resolucion  ,
                   rg_datamart.diag_datamart     ,
                   rg_datamart.semanas_cotizadas ,
                   rg_datamart.nombre_datamart
            FROM   ret_det_datamart
            WHERE  nss              = rg_datamart.nss
            AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
            AND    tipo_pension     = rg_datamart.tipo_pension
            AND    tipo_seguro      IN ("IV","CV","RI")
            AND    diag_datamart    in (101,300,301,210,302,303)
            AND    sec_pension      = rg_datamart.sec_pension

            IF SQLCA.SQLCODE = NOTFOUND THEN
               { SELECT MAX(sec_pension)
               INTO   vmax_sec_pension
               FROM   ret_det_datamart
               WHERE  nss              = rg_datamart.nss
               AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
               AND    tipo_seguro      = "RI"
               AND    diag_datamart    in (101,300,301,210,302,303) }

               SELECT fecha_resolucion  ,
                      diag_datamart     ,
                      semanas_cotizadas ,
                      nombre_datamart
               INTO   rg_datamart.fecha_resolucion  ,
                      rg_datamart.diag_datamart     ,
                      rg_datamart.semanas_cotizadas ,
                      rg_datamart.nombre_datamart
               FROM   ret_det_datamart
               WHERE  nss              = rg_datamart.nss
               AND    fecha_ini_pen    = rg_datamart.fecha_ini_pen
               AND    tipo_seguro      IN ("IV","CV","RI")
               AND    diag_datamart    in (101,300,301,210,302,303)
               AND    sec_pension      = rg_datamart.sec_pension
               IF SQLCA.SQLCODE <> NOTFOUND THEN
                  ERROR " EL TIPO DE PENSION REGISTRADO EN DATAMART NO CORRESPONDE AL QUE SE HA CAPTURADO"
                  SLEEP 2
                  NEXT FIELD  tipo_seguro
               ELSE
                  ERROR " NO SE ENCONTRO REGISTRO EN EL DATAMART .."
                  ATTRIBUTE(REVERSE)
                  SLEEP 2
                  NEXT FIELD  tipo_seguro
               END IF
            ELSE
               ERROR " SE ENCONTRO REGISTRO EN EL DATAMART .."
               ATTRIBUTE(REVERSE)
               DISPLAY rg_datamart.fecha_resolucion  TO fecha_resolucion
               DISPLAY rg_datamart.sec_pension       TO sec_pension
               DISPLAY rg_datamart.tipo_prestacion   TO tipo_prestacion
               DISPLAY rg_datamart.tipo_seguro       TO tipo_seguro
               DISPLAY rg_datamart.diag_datamart     TO diag_datamart
               DISPLAY rg_datamart.semanas_cotizadas TO semanas_cotizadas
               DISPLAY rg_datamart.nombre_datamart   TO nombre_datamart
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
                NEXT FIELD num_resolucion
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

            ------------- sil------------------
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
        EXIT INPUT

         ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",rg_datamart.nss CLIPPED," ",
                         rg_datamart.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
            ON KEY ( CONTROL-C )
                EXIT INPUT

            ON KEY ( INTERRUPT )
                EXIT INPUT
    END INPUT

   IF vmodif2 = 1  THEN
      CALL pre_gunta() RETURNING opc
      IF opc MATCHES "[Nn]" THEN
         DISPLAY "  MODIFICACION CANCELADA  "
                        AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
         CALL inicializa() #i
         RETURN
      END IF
   ELSE
      IF (vmodif2 = 2) AND (rg_datamart.estado_solicitud = reg_1.capturado) THEN
         CALL pre_gunta2() RETURNING opc
         IF opc MATCHES "[Nn]" THEN
            DISPLAY "  CONFIRMACION CANCELADA  "
                            AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
            CALL inicializa() #i
            RETURN
         END IF
      ELSE
          --------sil -----------------------------------
          --- Verifica si es una operacion de rechazo ---
          IF vmodif2 =3  THEN
             CALL pre_gunta3() RETURNING opc
             IF opc MATCHES "[Nn]" THEN
                DISPLAY " RECHAZO CANCELADO "
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

                   --
                   --error "entre al update de la linea 3990"
                   --sleep 5
                   --
           PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
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
        IF (vmodif2 = 2) AND (rg_datamart.estado_solicitud = reg_1.capturado) THEN
--      IF vmodif2 = 2 THEN
         WHENEVER ERROR CONTINUE
         UPDATE ret_solicitud_tx
         SET  ret_solicitud_tx.folio_solicitud   = rg_datamart.sol_folio,
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

                   --
                   --error "entre al update de la linea 4022"
                   --sleep 5
                   --
           PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
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
           --------------------------sil----------------------
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

                   --
                   --error "entre al update de la linea 4053"
                   --sleep 5
                   --
                   PROMPT " ERROR UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                   FOR enter
                   EXIT PROGRAM
              END IF
              WHENEVER ERROR STOP

              DISPLAY "  REGISTRO RECHAZADO   ","" AT 22,1
                        ATTRIBUTE(REVERSE)

              --*****DESMARCAR CUENTA****************************************
              SELECT movimiento
              INTO   li_tipo_movimiento
              FROM   tab_retiro
              WHERE  tipo_retiro = "J"

              LET vestado_marca   = 0
              LET vcodigo_rechazo = 0
              LET vmarca_causa    = 0
              LET vfecha_causa    = NULL

              EXECUTE eje_desmarca USING rg_datamart.nss,
                                         li_tipo_movimiento,
                                         rg_datamart.consecutivo,
                                         vestado_marca,
                                                 vmarca_causa,
                                                 usuario

              SLEEP 3
              CALL inicializa() #i
              RETURN
           END IF---------------- fin rechaza solicitud ---------------
       END IF
    END IF

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
    AND    tipo_seguro     = "RI"   --reg_2.tipo_seguro
    AND    tipo_pension    = reg_2.tipo_pension
    AND    tipo_prestacion = reg_2.tipo_prestacion
    GROUP BY 1

    IF SQLCA.SQLCODE = NOTFOUND  THEN
        PROMPT " NO EXISTE REGISTRO EN LA RET_MATRIZ_DERECHO " FOR CHAR enter
        EXIT PROGRAM
    END IF

    RETURN x_grupo

END FUNCTION

FUNCTION crea_tablas(reg_03)
#ct-------------------------
    DEFINE reg_03 RECORD #reg_3
        nss             CHAR(11)
    END RECORD

    WHENEVER ERROR CONTINUE

    DATABASE safre_tmp

    DROP TABLE safre_tmp:ret97_dis_cuenta
    CREATE TABLE safre_tmp:ret97_dis_cuenta
    (
     nss                      char(11)      ,
     consecutivo_lote         integer       ,
     subcuenta                smallint      ,
     tipo_movimiento          smallint      ,
     fecha_valor              date          ,
     fecha_conversion         date          ,
     folio_sua                char(6)       ,
     id_aportante             char(11)      ,
     monto_en_acciones        decimal(16,6) ,
     monto_en_pesos           decimal(16,6)
    );

    DROP TABLE safre_tmp:ret97_det_aporte
    CREATE TABLE safre_tmp:ret97_det_aporte
    (
     n_seguro                 char(11) ,
     folio_pago_sua           char(6)  ,
     reg_patronal_imss        char(11) ,
     ano_y_bimestre           char(6)  ,
     consec_reg_lote          integer
    );

    WHENEVER ERROR STOP

    DATABASE safre_af

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta97
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta98
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta99
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta00
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta01
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    INSERT INTO safre_tmp:ret97_dis_cuenta
    SELECT nss              ,
           consecutivo_lote ,
           subcuenta        ,
           tipo_movimiento  ,
           fecha_valor      ,
           fecha_conversion ,
           folio_sua        ,
           id_aportante     ,
           monto_en_acciones,
           monto_en_pesos
    FROM   dis_cuenta
    WHERE  nss                 = reg_03.nss
    AND    subcuenta           = 1
    AND    tipo_movimiento NOT IN (888,999)

    WHENEVER ERROR CONTINUE

    DATABASE safre_tmp

    CREATE INDEX safre_tmp:ret97_dis_cuenta_1 ON safre_tmp:ret97_dis_cuenta
    (nss, subcuenta);

    CREATE INDEX safre_tmp:ret97_dis_cuenta_2 ON safre_tmp:ret97_dis_cuenta
    (fecha_conversion);

    CREATE INDEX safre_tmp:ret97_dis_cuenta_3 ON safre_tmp:ret97_dis_cuenta
    (nss, subcuenta, folio_sua);

    CREATE INDEX safre_tmp:ret97_dis_cuenta_4 ON safre_tmp:ret97_dis_cuenta
    (nss, subcuenta, tipo_movimiento);

    CREATE INDEX safre_tmp:ret97_dis_cuenta_5 ON safre_tmp:ret97_dis_cuenta
    (fecha_valor);

    UPDATE STATISTICS FOR TABLE safre_tmp:ret97_dis_cuenta

    DATABASE safre_af

    WHENEVER ERROR STOP
END FUNCTION

FUNCTION primer_paso(reg_1)
#pp------------------------
    DEFINE reg_1 RECORD #reg_1
        n_seguro              LIKE afi_mae_afiliado.n_seguro
    END RECORD

    DEFINE reg_2 RECORD #reg_2
        n_seguro              LIKE dis_det_aporte.n_seguro          ,
        folio_pago_sua        LIKE dis_det_aporte.folio_pago_sua    ,
        reg_patronal_imss     LIKE dis_det_aporte.reg_patronal_imss ,
        periodo_pago          LIKE dis_det_aporte.periodo_pago      ,
    consec_reg_lote       LIKE dis_det_aporte.consec_reg_lote
    END RECORD

    DEFINE reg_5 RECORD #reg_5
        folio_pago_sua        LIKE dis_det_aporte.folio_pago_sua    ,
        reg_patronal_imss     LIKE dis_det_aporte.reg_patronal_imss
    END RECORD

    DEFINE #loc #char
        c5_ano_bim_corte      CHAR(5) ,
        c5_ano_bimestre       CHAR(5)

    DEFINE #loc #smallint
        s_ano_bim_corte       ,
        s_ano                 ,
        s_bimestre            ,
        i_ano_y_bimestre      ,
        s_mes_periodo         ,
        s_cuociente           ,
        s_residuo             SMALLINT

    DEFINE #loc #decimal
        d6_tot_monto          DECIMAL(16,6) ,
        d6_monto              DECIMAL(16,6)

    LET d6_monto = 0

    --DETERMINA EL A¥O Y EL BIMESTRE DE CADA APORTACION------------------
    DECLARE cur_4a CURSOR FOR
    SELECT  n_seguro          ,
            folio_pago_sua    ,
            reg_patronal_imss ,
            periodo_pago      ,
            consec_reg_lote
    FROM    dis_det_aporte
    WHERE   n_seguro = reg_1.n_seguro

    FOREACH cur_4a INTO reg_2.*
        LET s_mes_periodo = reg_2.periodo_pago[5,6]
        LET s_cuociente   = s_mes_periodo/2
        LET s_residuo     = s_mes_periodo MOD 2

        IF s_residuo = 0 THEN ---Segundo mes del bimestre
            LET s_bimestre = s_cuociente
        ELSE
            LET s_bimestre = s_cuociente + 1
        END IF

        LET c5_ano_bimestre  = reg_2.periodo_pago[1,4],s_bimestre USING"&"
        LET i_ano_y_bimestre = c5_ano_bimestre

        INSERT INTO safre_tmp:ret97_det_aporte
        VALUES(reg_2.n_seguro          ,
               reg_2.folio_pago_sua    ,
               reg_2.reg_patronal_imss ,
               i_ano_y_bimestre         ,
           reg_2.consec_reg_lote
              )
    END FOREACH
END FUNCTION

FUNCTION segundo_paso(reg_6)
#sp-------------------------
    DEFINE reg_6 RECORD #reg_6
    nss                   CHAR(11),
        fecha_pen_trasp       DATE
    END RECORD

    DEFINE reg_8 RECORD #glo #reg_8
        monto_accion_97       ,
        monto_accion_cv       ,
        monto_accion_so       ,
        monto_accion_es       ,
        monto_accion_sar      ,
        monto_accion_esp      DECIMAL(16,6)
    END RECORD
    DEFINE reg_12 RECORD #glo #reg_12
        monto_accion_97       ,
        monto_accion_cv       ,
        monto_accion_so       ,
        monto_accion_es       ,
        monto_accion_sar      ,
        monto_accion_esp      DECIMAL(16,6)
    END RECORD

    DEFINE reg_13 RECORD #glo #reg_13
        monto_accion_tot      DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        c10_fecha_corte       CHAR(10)

    DEFINE #loc #date
--      f_fecha_corte         ,
        f_fecha_paso          DATE

    DEFINE #loc #smallint
        s_mes_corte           ,
        s_bimestre            ,
        s_cuociente           ,
        s_mes_periodo         ,
        s_ano                 ,
        s_residuo             ,
        s_mes_fecha           SMALLINT

    ------------------------------------------------------------------------
    --- DETERMINA FECHA DE CORTE.PRIMER DIA NATURAL POSTERIOR AL BIMESTRE DE
    --- LA FECHA DE INICIO DE PENSION

    LET c10_fecha_paso = reg_6.fecha_pen_trasp
    LET s_ano          = c10_fecha_paso[07,010]
    LET s_mes_periodo  = c10_fecha_paso[01,02]
    LET s_cuociente    = s_mes_periodo/2
    LET s_residuo      = s_mes_periodo MOD 2

    IF s_residuo = 0 THEN ---Segundo mes del bimestre
        LET s_bimestre = s_cuociente
    ELSE
        LET s_bimestre = s_cuociente + 1
    END IF

    LET s_mes_corte = s_bimestre * 2 + 1

    IF s_mes_corte = 13 THEN
        LET s_mes_corte = 1
        LET s_ano = s_ano + 1
    END IF

    LET c10_fecha_corte = s_mes_corte USING"&&","/01/",s_ano USING"&&&&"
    LET f_fecha_corte   = c10_fecha_corte

    -------------------------------------------------------------------------
    --- CALCULO DEL SALDO DE LAS APORTACIONES CUYA FECHA DE CONVERSION SEA
    --- MAYOR AL BIMESTRE DE LA FECHA DE INICIO DE PENSION PERO QUE
    --- CORRESPONDA A BIMESTRES POSTERIORES AL BIMESTRE DE LA FECHA DE INICIO
    --- DE PENSION

    LET reg_8.monto_accion_97 = 0
    CALL cal_aport_post_a_fecha_conversion(reg_6.nss ,
                                           1         ,
                                           reg_6.fecha_pen_trasp)#capafc
    RETURNING reg_8.monto_accion_97

    LET reg_8.monto_accion_cv = 0
    CALL cal_aport_post_a_fecha_conversion(reg_6.nss ,
                                           2         ,
                                           reg_6.fecha_pen_trasp)#capafc
    RETURNING reg_8.monto_accion_cv

    LET reg_8.monto_accion_so = 0
    CALL cal_aport_post_a_fecha_conversion(reg_6.nss ,
                                           5         ,
                                           reg_6.fecha_pen_trasp)#capafc
    RETURNING reg_8.monto_accion_so

    LET reg_8.monto_accion_es = 0
    CALL cal_aport_post_a_fecha_conversion(reg_6.nss ,
                                           6         ,
                                           reg_6.fecha_pen_trasp)#capafc
    RETURNING reg_8.monto_accion_es

    LET reg_8.monto_accion_esp = 0
    CALL cal_aport_post_a_fecha_conversion(reg_6.nss ,
                                           9         ,
                                           reg_6.fecha_pen_trasp)#capafc
    RETURNING reg_8.monto_accion_esp

    -------------------------------------------------------------------------

    --- CALCULO DEL SALDO POR CARGOS POSTERIORES A LA FECHA DE INICIO DE
    --- PENSION

    LET reg_12.monto_accion_97 = 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  1         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_97
    --------------------------------
    LET reg_12.monto_accion_cv = 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  2         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_cv
    --------------------------------
    LET reg_12.monto_accion_so = 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  5         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_so
    --------------------------------
    LET reg_12.monto_accion_es = 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  6         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_es
    --------------------------------
    LET reg_12.monto_accion_sar= 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  7         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_sar
    --------------------------------
    LET reg_12.monto_accion_esp = 0
    CALL calcula_saldo_de_cargos2(reg_6.nss ,
                                  9         ,
                                  reg_6.fecha_pen_trasp)#csdc
    RETURNING reg_12.monto_accion_esp
    --------------------------------
    IF reg_8.monto_accion_97 = " " OR
       reg_8.monto_accion_97 IS NULL THEN
        LET reg_8.monto_accion_97 = 0
    END IF

    IF reg_8.monto_accion_cv = " " OR
       reg_8.monto_accion_cv IS NULL THEN
       LET reg_8.monto_accion_cv = 0
    END IF
    IF reg_8.monto_accion_so = " " OR
       reg_8.monto_accion_so IS NULL THEN
        LET reg_8.monto_accion_so = 0
    END IF
    IF reg_8.monto_accion_es = " " OR
       reg_8.monto_accion_es IS NULL THEN
        LET reg_8.monto_accion_es = 0
    END IF
    IF reg_8.monto_accion_sar = " " OR
       reg_8.monto_accion_sar IS NULL THEN
        LET reg_8.monto_accion_sar = 0
    END IF
    IF reg_8.monto_accion_esp = " " OR
       reg_8.monto_accion_esp IS NULL THEN
        LET reg_8.monto_accion_esp = 0
    END IF
    IF reg_12.monto_accion_97 = " " OR
       reg_12.monto_accion_97 IS NULL THEN
        LET reg_12.monto_accion_97 = 0
    END IF
    IF reg_12.monto_accion_cv = " " OR
       reg_12.monto_accion_cv IS NULL THEN
        LET reg_12.monto_accion_cv = 0
    END IF
    IF reg_12.monto_accion_so = " " OR
       reg_12.monto_accion_so IS NULL THEN
        LET reg_12.monto_accion_so = 0
    END IF
    IF reg_12.monto_accion_es = " " OR
       reg_12.monto_accion_es IS NULL THEN
        LET reg_12.monto_accion_es = 0
    END IF
    IF reg_12.monto_accion_sar = " " OR
       reg_12.monto_accion_sar IS NULL THEN
        LET reg_12.monto_accion_sar = 0
    END IF
    IF reg_12.monto_accion_esp = " " OR
       reg_12.monto_accion_esp IS NULL THEN
        LET reg_12.monto_accion_esp = 0
    END IF

    LET reg_13.monto_accion_tot= reg_8.monto_accion_97   +
                                 reg_8.monto_accion_cv   +
                                 reg_8.monto_accion_so   +
                                 reg_8.monto_accion_es   +
                                 reg_8.monto_accion_sar  +
                                 reg_8.monto_accion_esp
--                               reg_12.monto_accion_97  +
--                               reg_12.monto_accion_cv  +
--                               reg_12.monto_accion_so  +
--                               reg_12.monto_accion_es  +
--                               reg_12.monto_accion_sar +
--                               reg_12.monto_accion_esp

    RETURN reg_13.monto_accion_tot

END FUNCTION

FUNCTION cal_aport_post_a_fecha_conversion(reg_9)
#capafc------------------------------------------
    DEFINE reg_9 RECORD #reg_9
        n_seguro              LIKE afi_mae_afiliado.n_seguro ,
        subcuenta             LIKE dis_cuenta.subcuenta      ,
        fecha_pen_trasp       LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE reg_10 RECORD #reg_10
        n_seguro              LIKE dis_det_aporte.n_seguro          ,
        folio_pago_sua        LIKE dis_det_aporte.folio_pago_sua    ,
        reg_patronal_imss     LIKE dis_det_aporte.reg_patronal_imss ,
        periodo_pago          LIKE dis_det_aporte.periodo_pago
    END RECORD

    DEFINE reg_11 RECORD #reg_11
        folio_pago_sua        LIKE dis_det_aporte.folio_pago_sua
    END RECORD

    DEFINE #loc #date
        f_fecha_ini_pen       DATE

    DEFINE #loc #char
        c5_ano_bim_corte      CHAR(5) ,
        c5_ano_bimestre       CHAR(5) ,
        c10_fecha_ini_pen     CHAR(10)

    DEFINE #loc #smallint
        s_ano_bim_corte       ,
        s_ano                 ,
        s_bimestre            ,
        s_ano_y_bimestre      ,
        s_mes_periodo         ,
        s_cuociente           ,
        s_residuo             SMALLINT

    DEFINE #loc #decimal
        d6_tot_monto          DECIMAL(16,6) ,
        d6_tot_monto_viv      DECIMAL(16,6) ,
        d6_monto_apo          DECIMAL(16,6) ,
        d6_monto_int          DECIMAL(16,6) ,
        d6_monto              DECIMAL(16,6) ,
    d6_monto_sin_per      DECIMAL(16,6)

    ------------------------------------------------------------------
    --- CALCULA LA FECHA INICIO DE PENSION AL PRIMER DIA DEL MES

    IF MONTH(reg_9.fecha_pen_trasp) = 12 THEN
        LET c10_fecha_ini_pen = "01/01/",
                                YEAR(reg_9.fecha_pen_trasp) + 1
        LET f_fecha_ini_pen   = c10_fecha_ini_pen
    ELSE
        LET c10_fecha_ini_pen = MONTH(reg_9.fecha_pen_trasp USING "&&"),
                                "/01/",
                                YEAR(reg_9.fecha_pen_trasp)
        LET f_fecha_ini_pen   = c10_fecha_ini_pen
    END IF
    ------------------------------------------------------------------
    --- DETERMINA EL A¥O Y EL BIMESTRE DE CADA APORTACION

    LET d6_monto = 0
    LET d6_monto_sin_per = 0
    LET c10_fecha_paso = reg_9.fecha_pen_trasp
    LET s_ano          = c10_fecha_paso[07,010]
    LET s_mes_periodo  = c10_fecha_paso[01,02]
    LET s_cuOciente    = s_mes_periodo/2
    LET s_residuo      = s_mes_periodo MOD 2

    IF s_residuo = 0 THEN --- SEGUNDO MES DEL BIMESTRE
        LET s_bimestre = s_cuociente
    ELSE
        LET s_bimestre = s_cuociente + 1
    END IF

    LET c5_ano_bim_corte = s_ano USING"&&&&",s_bimestre USING"&"
    LET s_ano_bim_corte  = c5_ano_bim_corte
    ------------------------------------------------------------------
    DECLARE cur_13 CURSOR FOR
    SELECT UNIQUE folio_pago_sua
    FROM   safre_tmp:ret97_det_aporte
    WHERE  n_seguro        = reg_9.n_seguro
    AND    ano_y_bimestre  >= s_ano_bim_corte
    --AND    ano_y_bimestre  <= s_ano_bim_corte

    LET d6_tot_monto     = 0
    LET d6_tot_monto_viv = 0

    FOREACH cur_13 INTO reg_11.*
        LET d6_monto_apo = 0
        LET d6_monto_int = 0
        LET d6_monto     = 0

        SELECT SUM(monto_en_acciones)
        INTO   d6_monto
        FROM   safre_tmp:ret97_dis_cuenta
        WHERE  nss               = reg_9.n_seguro
        AND    subcuenta         = reg_9.subcuenta
        AND    folio_sua         = reg_11.folio_pago_sua
    AND    fecha_conversion  >= f_fecha_corte
    AND    monto_en_acciones > 0

        IF STATUS = NOTFOUND THEN
            LET d6_monto = 0
        ELSE
            IF d6_monto IS NULL OR d6_monto = " " THEN
                LET d6_monto = 0
            END IF
        END IF

        LET d6_tot_monto = d6_tot_monto + d6_monto
    END FOREACH

    SELECT sum(monto_en_acciones)
    INTO   d6_monto_sin_per
    FROM   safre_tmp:ret97_dis_cuenta
    WHERE  nss               = reg_9.n_seguro
    AND    subcuenta         = reg_9.subcuenta
    AND    fecha_conversion  >= f_fecha_corte
    AND    monto_en_acciones > 0
    AND    folio_sua NOT IN (SELECT folio_pago_sua
                         FROM   safre_tmp:ret97_det_aporte B
                 WHERE  B.n_seguro = reg_9.n_seguro)

    IF d6_monto_sin_per IS NULL OR d6_monto_sin_per = " " OR
        d6_monto_sin_per = "" THEN
        LET d6_monto_sin_per = 0
    END IF

    LET d6_tot_monto = d6_tot_monto + d6_monto_sin_per

    IF d6_tot_monto = " " OR
       d6_tot_monto IS NULL THEN
        LET d6_tot_monto = 0
    END IF

    RETURN d6_tot_monto
END FUNCTION

FUNCTION calcula_saldo_de_cargos2(reg_14)
#capafc------------------------------------------
    DEFINE reg_14 RECORD #reg_14
        n_seguro              LIKE afi_mae_afiliado.n_seguro ,
        subcuenta             LIKE dis_cuenta.subcuenta      ,
        fecha_pen_trasp       LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE reg_15 RECORD #reg_15
        folio_pago_sua            LIKE dis_det_aporte.folio_pago_sua
    END RECORD

    DEFINE #loc #date
        f_fecha_ini_pen          DATE

    DEFINE #loc #char
        c5_ano_bim_corte         CHAR(5) ,
        c5_ano_bimestre          CHAR(5) ,
        c10_fecha_ini_pen        CHAR(10)

    DEFINE #loc #smallint
        s_ano_bim_corte          ,
        s_ano                    ,
        s_bimestre               ,
        s_ano_y_bimestre         ,
        s_mes_periodo            ,
        s_cuociente              ,
        s_residuo                SMALLINT

    DEFINE #loc #decimal
        d6_tot_monto             DECIMAL(16,6) ,
        d6_tot_monto_viv         DECIMAL(16,6) ,
        d6_monto_apo             DECIMAL(16,6) ,
        d6_monto_int             DECIMAL(16,6) ,
        d6_monto                 DECIMAL(16,6) ,
    d6_monto_sin_per         DECIMAL(16,6)

    ---------------------------------------------------------------
    --- CALCULA LA FECHA INICIO DE PENSION AL PRIMER DIA DEL MES

    IF MONTH(reg_14.fecha_pen_trasp) = 12 THEN
        LET c10_fecha_ini_pen = "01/01/",
                                YEAR(reg_14.fecha_pen_trasp) + 1
        LET f_fecha_ini_pen   = c10_fecha_ini_pen
    ELSE
        LET c10_fecha_ini_pen = MONTH(reg_14.fecha_pen_trasp USING "&&"),
                                "/01/",
                                YEAR(reg_14.fecha_pen_trasp)
        LET f_fecha_ini_pen   = c10_fecha_ini_pen
    END IF
    ---------------------------------------------------------------
    ---DETERMINA EL AÑO Y EL BIMESTRE DE CADA APORTACION

    LET d6_monto         = 0
    LET d6_monto_sin_per = 0
    LET c10_fecha_paso   = reg_14.fecha_pen_trasp
    LET s_ano            = c10_fecha_paso[07,010]
    LET s_mes_periodo    = c10_fecha_paso[01,02]
    LET s_cuOciente      = s_mes_periodo/2
    LET s_residuo        = s_mes_periodo MOD 2

    IF s_residuo = 0 THEN ---SEGUNDO MES DEL BIMESTRE
        LET s_bimestre = s_cuociente
    ELSE
        LET s_bimestre = s_cuociente + 1
    END IF

    LET c5_ano_bim_corte = s_ano USING"&&&&",s_bimestre USING"&"
    LET s_ano_bim_corte  = c5_ano_bim_corte

    ---------------------------------------------------------------------

    DECLARE cur_5car CURSOR FOR
    SELECT UNIQUE folio_pago_sua
    FROM   safre_tmp:ret97_det_aporte
    WHERE  n_seguro        = reg_14.n_seguro
    AND    ano_y_bimestre  <= s_ano_bim_corte

    LET d6_tot_monto     = 0
    LET d6_tot_monto_viv = 0

    FOREACH cur_5car INTO reg_15.*
        LET d6_monto_apo = 0
        LET d6_monto_int = 0
        LET d6_monto     = 0

        SELECT SUM(monto_en_acciones)
        INTO   d6_monto
        FROM   safre_tmp:ret97_dis_cuenta
        WHERE  nss               = reg_14.n_seguro
        AND    subcuenta         = reg_14.subcuenta
        AND    folio_sua         = reg_15.folio_pago_sua
    AND    fecha_conversion  >= f_fecha_corte
    AND    monto_en_acciones < 0

        IF STATUS = NOTFOUND THEN
            LET d6_monto = 0
        ELSE
            IF d6_monto IS NULL OR d6_monto = " " THEN
                LET d6_monto = 0
            END IF
        END IF

        LET d6_tot_monto = d6_tot_monto + d6_monto
    END FOREACH

    SELECT sum(monto_en_acciones)
    INTO   d6_monto_sin_per
    FROM   safre_tmp:ret97_dis_cuenta
    WHERE  nss               = reg_14.n_seguro
    AND    subcuenta         = reg_14.subcuenta
    AND    fecha_conversion  >= f_fecha_corte
    AND    monto_en_acciones < 0
    AND    folio_sua NOT IN (SELECT folio_pago_sua
                 FROM   safre_tmp:ret97_det_aporte B
                 WHERE  B.n_seguro = reg_14.n_seguro)

    IF d6_monto_sin_per IS NULL OR d6_monto_sin_per = " " OR
        d6_monto_sin_per = "" THEN
        LET d6_monto_sin_per = 0
    END IF

    LET d6_tot_monto = d6_tot_monto + d6_monto_sin_per

    IF d6_tot_monto IS NULL OR
       d6_tot_monto = " "   THEN
        LET d6_tot_monto = 0
    END IF

    RETURN d6_tot_monto

END FUNCTION


FUNCTION calcula_montos_posteriores(nssa, fecha_pen)
#cmp------------------------------------------------
    DEFINE #loc #smallint
       xtodo              ,
       subcta             ,
       mes                ,
       ano                ,
       residuo            SMALLINT

    DEFINE  #loc #decimal
       monto              ,
       monto1             ,
       monto2             ,
       monto5             ,
       monto6             ,
       monto9             ,
       monto4             DEC(16,6)

    DEFINE #loc #char
        periodo           CHAR(6)  ,
        xnss              CHAR(11) ,
        xprimero          CHAR(10) ,
        nssa              CHAR(11) ,
        enter             CHAR(01) ,
        c10_fecha_proceso CHAR(10)

    DEFINE #loc #date
        dprimero_mes      ,
        fecha_pen         ,
        fecha_val         DATE

    DEFINE reg_1 RECORD #loc #reg_1
        folio             LIKE ret_transf_rx.folio,
        consecutivo       LIKE ret_transf_rx.consecutivo
    END RECORD

    LET monto  = 0    LET monto1 = 0
    LET monto2 = 0    LET monto5 = 0
    LET monto4 = 0    LET monto6 = 0
    LET monto9 = 0

    LET mes = MONTH(fecha_pen)
    LET ano = YEAR (fecha_pen)

    LET residuo = mes MOD 2
    IF residuo > 0 THEN
       LET mes = mes + 1
    END IF

    CALL habil_siguiente(HOY,3)
    RETURNING dprimero_mes

    LET c10_fecha_proceso = dprimero_mes

    LET c10_fecha_proceso = c10_fecha_proceso[01,02],
                    "/01/",c10_fecha_proceso[07,10]

    LET dprimero_mes     = c10_fecha_proceso

    LET periodo = ano USING "&&&&",mes USING "&&"

    WHENEVER ERROR CONTINUE
    DROP TABLE tmp_cuenta_ret

    CREATE TEMP TABLE tmp_cuenta_ret(
    tipo_movimiento   smallint      ,
    subcuenta         smallint      ,
    siefore           smallint      ,
    folio             integer       ,
    consecutivo_lote  integer       ,
    nss               char(11)      ,
    curp              char(18)      ,
    folio_sua         char(06)      ,
    fecha_pago        date          ,
    fecha_valor       date          ,
    fecha_conversion  date          ,
    monto_en_pesos    decimal(22,6) ,
    monto_en_acciones decimal(22,6) ,
    precio_accion     decimal(22,6) ,
    dias_cotizados    smallint      ,
    sucursal          char(10)      ,
    id_aportante      char(11)      ,
    estado            smallint      ,
    fecha_proceso     date          ,
    usuario           char(8)       ,
    fecha_archivo     date          ,
    etiqueta          smallint
    )

    INSERT INTO tmp_cuenta_ret
    SELECT *
    FROM   dis_cuenta97 A
    WHERE  A.nss = nssa

    INSERT INTO tmp_cuenta_ret
    SELECT *
    FROM   dis_cuenta98 A
    WHERE  A.nss = nssa

    INSERT INTO tmp_cuenta_ret
    SELECT *
    FROM   dis_cuenta99 A
    WHERE  A.nss = nssa

    INSERT INTO tmp_cuenta_ret
    SELECT *
    FROM   dis_cuenta00 A
    WHERE  A.nss = nssa

    INSERT INTO tmp_cuenta_ret
    SELECT *
    FROM   dis_cuenta01 A
    WHERE  A.nss = nssa

    CREATE INDEX tmp1 ON tmp_cuenta_ret(subcuenta,folio,consecutivo_lote)
    WHENEVER ERROR STOP

    DECLARE cur1 CURSOR FOR
    SELECT A.folio, A.consec_reg_lote
    FROM   dis_det_aporte A
    WHERE  A.n_seguro     = nssa
    AND    A.periodo_pago > periodo

    FOREACH cur1 INTO reg_1.*

        ########## CALCULA RCV ##########

    DECLARE cur2 CURSOR FOR
        SELECT subcuenta, SUM(monto_en_acciones)
    FROM   tmp_cuenta_ret
    WHERE  subcuenta       IN (1,2,4,5,6,9)
    AND    folio            = reg_1.folio
    AND    consecutivo_lote = reg_1.consecutivo
    GROUP BY 1

    FOREACH cur2 INTO subcta, monto
       IF monto IS NULL THEN
          LET monto = 0
       END IF

       CASE subcta
          WHEN 1
           LET monto1 = monto1 + monto
          WHEN 2
           LET monto2 = monto2 + monto
          WHEN 4
           LET monto4 = monto4 + monto
          WHEN 5
           LET monto5 = monto5 + monto
          WHEN 6
           LET monto6 = monto6 + monto
          WHEN 9
           LET monto9 = monto9 + monto
           END CASE
    END FOREACH
    END FOREACH

    RETURN monto1, monto2, monto6, monto5, monto9, monto4
END FUNCTION


FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE
       diaTmp                   ,
       diaHabilSig              ,
       diaActual                DATE

   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador                 ,
       diaSemana                ,
       feriado                  ,
       finSemana                SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)

       IF diaSemana = 0 OR diaSemana = 6 THEN
           LET finSemana = 1
       ELSE
           SELECT *
           FROM   tab_feriado
           WHERE  feria_fecha = diaHabilSig

           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF

       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION
