#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM805  => MANTENEDOR DE REGISTRO DE CERTIFICADO IMSS - RETIRO POR   #
#                     TRAMITE JUDICIAL (TIPO RETIRO G)                          #
#                                  VERSION COPPEL                               #
#Fecha creacion    => 20 DE ENERO DE 2004                                       #
#By                => STEFANIE DANIELA VERA PIÑA                                #
#Fecha actualiz.   => 17 DE OCTUBRE DE 2007                                     #
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
    DEFINE reg RECORD #glo #reg
        nss                LIKE ret_solicitud_tx.nss               ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc             ,
        curp               LIKE afi_mae_afiliado.n_unico           ,
        paterno            LIKE afi_mae_afiliado.paterno           ,
        materno            LIKE afi_mae_afiliado.materno           ,
        nombres            LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro        LIKE ret_solicitud_tx.tipo_retiro       ,
        des_tipo_ret       LIKE tab_retiro.descripcion             ,
        tipo_prestacion    LIKE ret_solicitud_tx.tipo_prestacion   ,
        desc_prestacion    LIKE tab_prestacion.descripcion         ,
        regimen            LIKE ret_solicitud_tx.regimen           ,
        tipo_seguro        LIKE ret_solicitud_tx.tipo_seguro       ,
        desc_seguro        LIKE tab_seguro.descripcion             ,
        tipo_pension       LIKE ret_solicitud_tx.tipo_pension      ,
        desc_pension       LIKE tab_pension.descripcion            ,
        fecha_oficio       DATE                                    ,
        no_oficio          INTEGER                                 ,
        autoridad          CHAR(9)                                 ,
        folio_solicitud    LIKE ret_solicitud_tx.folio_solicitud   ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud   ,
        grupo              LIKE tab_grupo.grupo                    ,
        desc_grupo         LIKE tab_grupo.descripcion              ,
        semanas_cotizadas  LIKE ret_solicitud_tx.semanas_cotizadas ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent   ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod       ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc         ,
        fecha_captura      LIKE ret_solicitud_tx.fecha_captura     ,
        fecha_modifica     LIKE ret_solicitud_tx.fecha_modifica    ,
        fecha_confirma     LIKE ret_solicitud_tx.fecha_confirma    ,
        fecha_liquida      DATE                                    ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud  ,
        desc_estado        CHAR(25)                                ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura   ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica  ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma  ,
        folio              LIKE ret_solicitud_tx.folio             ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo       ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE arr_1 ARRAY[5000] OF RECORD #glo #arr_1
        nss                LIKE ret_solicitud_tx.nss               ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc             ,
        curp               LIKE afi_mae_afiliado.n_unico           ,
        paterno            LIKE afi_mae_afiliado.paterno           ,
        materno            LIKE afi_mae_afiliado.materno           ,
        nombres            LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro        LIKE ret_solicitud_tx.tipo_retiro       ,
        des_tipo_ret       LIKE tab_retiro.descripcion             ,
        tipo_prestacion    LIKE ret_solicitud_tx.tipo_prestacion   ,
        desc_prestacion    LIKE tab_prestacion.descripcion         ,
        regimen            LIKE ret_solicitud_tx.regimen           ,
        tipo_seguro        LIKE ret_solicitud_tx.tipo_seguro       ,
        desc_seguro        LIKE tab_seguro.descripcion             ,
        tipo_pension       LIKE ret_solicitud_tx.tipo_pension      ,
        desc_pension       LIKE tab_pension.descripcion            ,
        fecha_oficio       DATE                                    ,
        no_oficio          INTEGER                                 ,
        autoridad          CHAR(9)                                 ,
        folio_solicitud    LIKE ret_solicitud_tx.folio_solicitud   ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud   ,
        grupo              LIKE tab_grupo.grupo                    ,
        desc_grupo         LIKE tab_grupo.descripcion              ,
        semanas_cotizadas  LIKE ret_solicitud_tx.semanas_cotizadas ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent   ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod       ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc         ,
        fecha_captura      LIKE ret_solicitud_tx.fecha_captura     ,
        fecha_modifica     LIKE ret_solicitud_tx.fecha_modifica    ,
        fecha_confirma     LIKE ret_solicitud_tx.fecha_confirma    ,
        fecha_liquida      DATE                                    ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud  ,
        desc_estado        CHAR(25)                                ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura   ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica  ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma  ,
        folio              LIKE ret_solicitud_tx.folio             ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo       ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio       ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        precapturado       LIKE ret_estado.estado_solicitud ,
        capturado          LIKE ret_estado.estado_solicitud ,
        rechazado          LIKE ret_estado.estado_solicitud ,
        confirmado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_10 RECORD
        estado_marca       SMALLINT ,
        codigo_rechazo     SMALLINT ,
        marca_causa        SMALLINT ,
        fecha_causa        DATE
    END RECORD

    DEFINE reg_rev RECORD
        nss                CHAR(11) ,
        marca_cod          SMALLINT ,
        correlativo        INTEGER
    END RECORD

    DEFINE #glo #date
        vfecha_ini_p       ,
        vfecha_resolucion  ,
        vfecha_causa       ,
        xx_fecha_solicitud ,
        HOY                DATE

    DEFINE #glo #char
        txt_1              CHAR(2800) ,
        x_busca            CHAR(1200) ,
        enter              CHAR(0001) ,
        vaccion            CHAR(0001) ,
        xaccion            CHAR(0001) ,
        vnss               CHAR(0011) ,
        vtipo_seguro       CHAR(0002) ,
        vtipo_pension      CHAR(0002) ,
        vtipo_retiro       CHAR(0001) ,
        vregimen           CHAR(0002) ,
        v_marca            CHAR(0100) ,
        v_desmarca         CHAR(0100) ,
        vdesmarca          CHAR(100)  , --sil
        v_reversa          CHAR(0100) ,
        v_ejecuta          CHAR(0500) ,
        x_error            CHAR(0500) ,   ---omar
        x_usuario          CHAR(0012) ,
        x_estado_solicitud CHAR(0040) ,
        desc_rechazo_cod   CHAR(0040) ,
        opc                CHAR(0001) ,
        option_afore       CHAR(0006) ,
        usuario            CHAR(0008)

    DEFINE #glo #smallint
        arr_c          ,
        cont_reg           ,
        vestado_marca      ,
        vcodigo_rechazo    ,
        vmarca_causa       ,
        vtipo_prestacion   ,
        v_rechazo_cod      ,
        v_marca_res        ,
        v_marca_ent        ,
        v_status           ,
        s_tipo_movimiento  ,
        s_capturado        ,
        s_procesado        ,
        s_pagado           ,
        sw                 ,
        v_tipo_movimiento  , --sil
        entidad            , --sil
        codigo             , --sil
        pos        SMALLINT
   DEFINE varmov  like tab_retiro.movimiento

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM805.log")    ---omar
    CALL init()

    OPEN WINDOW retm8051 AT 2,3 WITH FORM "RETM8051" ATTRIBUTE (BORDER)
    DISPLAY " RETM805       SOLICITUD DE DISPOSICION DE RECURSOS TIPO G                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
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
                       SHOW OPTION "(A)grega","(C)onsulta","(M)odifica",
                                   "(E)limina","(S)alida"
                   ELSE
                       HIDE OPTION ALL
                       SHOW OPTION "(A)grega","(C)onsulta","(M)odifica",
                                   "(E)limina","con(F)irma","(S)alida"
                   END IF

               COMMAND KEY(A) "(A)grega" "Agrega solicitud"
                   LET xaccion=" "
                   CALL inicializa()
                   CALL agrega() #a

               COMMAND KEY(C) "(C)onsulta" "Consulta solicitud"
                   LET xaccion=" "
                   CALL inicializa()
                   CALL consulta() #c

               COMMAND KEY(M) "(M)odifica" "Modifica solicitud"
                   LET xaccion=" "
                   CALL inicializa()
                   CALL modifica() #m

               COMMAND KEY(E) "(E)limina" "Elimina solicitud"
                   LET xaccion=" "
                   CALL inicializa()
                   CALL elimina() #e

               COMMAND KEY("F") "con(F)irma" "Confirma Solicitud"
                   MESSAGE "<Ctrl-F> Rechazar"
                   LET xaccion="F"
                   CALL inicializa()
                   CALL confirma() #cc

               COMMAND KEY(S) "(S)alida" "Vuelve al menu"
                   EXIT PROGRAM
           END MENU
    END CASE
CLOSE WINDOW retm8051
END MAIN

FUNCTION init()
#i-------------
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

    SELECT USER
    INTO   usuario
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
    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

    LET reg_10.estado_marca   = 0
    LET reg_10.codigo_rechazo = 0
    LET reg_10.marca_causa    = 0
    INITIALIZE reg_10.fecha_causa TO NULL

    ----- DESMARCAJE ------
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    PREPARE eje_desmarca FROM v_desmarca

    LET v_marca_ent = 0
END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE #loc #smallint
        sw_1                       ,
        vestado_solicitud          SMALLINT

    DEFINE #loc #integer
        ult_consecutivo            INTEGER

    DEFINE #loc #decimal
        vconsecutivo               LIKE ret_solicitud_tx.consecutivo

    OPEN WINDOW retm8052 AT 2,3 WITH FORM "RETM8052" ATTRIBUTE (BORDER)
    DISPLAY " RETM805                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C:Salir       Ctrl-B:Dom Trab/Benef        Esc:Agrega" AT 1,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1
    ATTRIBUTE(REVERSE)

    LET sw_1              = 0
    LET reg.fecha_captura = HOY
    LET reg.rechazo_cod    = 0

    DISPLAY BY NAME reg.fecha_captura

    INPUT BY NAME reg.nss                ,
                  reg.regimen            ,
                  reg.fecha_oficio,
                  reg.no_oficio,
                  reg.autoridad          ,
                  reg.folio_solicitud    ,
                  reg.fecha_solicitud    ,
                  reg.grupo              ,
                  reg.semanas_cotizadas  WITHOUT DEFAULTS

        BEFORE FIELD nss
        IF vaccion = 'C' OR
           vaccion = 'M' THEN

                LET reg.nss              = vnss
                LET reg.regimen          = vregimen
                LET reg.tipo_prestacion  = vtipo_prestacion
                LET reg.tipo_seguro      = vtipo_seguro
                LET reg.tipo_pension     = vtipo_pension
                LET reg.tipo_retiro      = vtipo_retiro

                DISPLAY reg.nss              TO nss
                DISPLAY reg.regimen          TO regimen
                DISPLAY reg.tipo_prestacion  TO tipo_prestacion
                DISPLAY reg.tipo_seguro      TO tipo_seguro
                DISPLAY reg.tipo_pension     TO tipo_pension
                DISPLAY reg.tipo_retiro      TO tipo_retiro
            DISPLAY reg.fecha_solicitud  TO fecha_solicitud

                SELECT n_rfc   ,
                       n_unico ,
                       paterno ,
                       materno ,
                       nombres
                INTO  reg.n_rfc   ,
                      reg.curp    ,
                      reg.paterno ,
                      reg.materno ,
                      reg.nombres
                FROM  afi_mae_afiliado
                WHERE n_seguro = reg.nss

                SELECT A.descripcion
                INTO   reg.desc_estado
                FROM   ret_status A
                WHERE  A.status = reg_1.capturado

                LET reg.des_tipo_ret = cat_tipo_ret()

                SELECT descripcion
                INTO   reg.desc_prestacion
                FROM   tab_prestacion
                WHERE  tipo_prestacion = reg.tipo_prestacion

                SELECT descripcion
                INTO   reg.desc_seguro
                FROM   tab_seguro
                WHERE  clave = reg.tipo_seguro

                DISPLAY BY NAME reg.materno
                DISPLAY BY NAME reg.paterno
                DISPLAY BY NAME reg.nombres
                DISPLAY BY NAME reg.n_rfc
                DISPLAY BY NAME reg.curp
                DISPLAY BY NAME reg.desc_estado
                DISPLAY BY NAME reg.desc_seguro
                DISPLAY BY NAME reg.desc_pension
                DISPLAY BY NAME reg.des_tipo_ret
                DISPLAY BY NAME reg.desc_prestacion

            END IF

            IF sw_1 = 0 THEN

                LET reg.tipo_retiro = "G"

                SELECT descripcion
                INTO   reg.des_tipo_ret
                FROM   tab_retiro
                WHERE  tipo_retiro = reg.tipo_retiro

                LET reg.tipo_prestacion = 8

                SELECT descripcion
                INTO   reg.desc_prestacion
                FROM   tab_prestacion
                WHERE  tipo_prestacion = reg.tipo_prestacion

                LET reg.tipo_seguro = "TJ"

                SELECT descripcion
                INTO   reg.desc_seguro
                FROM   tab_seguro
                WHERE  clave = reg.tipo_seguro

                LET reg.tipo_pension = "RE"

                SELECT descripcion
                INTO   reg.desc_pension
                FROM   tab_pension
                WHERE  tipo_pension = reg.tipo_pension

                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET reg.consecutivo    = ult_consecutivo

                LET reg.fecha_solicitud   = HOY
                LET reg.fecha_captura     = HOY
                LET reg.usuario_captura   = usuario
                LET reg.semanas_cotizadas = 0
                LET reg.grupo = 9

                SELECT descripcion
                INTO   reg.desc_grupo
                FROM   tab_grupo
                WHERE  grupo = reg.grupo
                GROUP BY 1


                DISPLAY BY NAME reg.tipo_retiro       ,
                                reg.des_tipo_ret      ,
                                reg.tipo_prestacion   ,
                                reg.desc_prestacion   ,
                                reg.tipo_seguro       ,
                                reg.desc_seguro       ,
                                reg.tipo_pension      ,
                                reg.desc_pension      ,
                                reg.fecha_solicitud   ,
                                reg.fecha_captura     ,
                                reg.consecutivo       ,
                                reg.semanas_cotizadas ,
                                reg.grupo ,
                                reg.desc_grupo ,
                                reg.usuario_captura
                LET sw_1 = 1
            END IF

        AFTER FIELD nss
            LET reg.fecha_captura = HOY

            IF reg.nss IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            ELSE
            LET vestado_solicitud  = " "

                SELECT MAX(consecutivo)
            INTO   vconsecutivo
            FROM   ret_solicitud_tx
            WHERE  nss              = reg.nss
                AND    tipo_retiro      = "G"
                AND    tipo_seguro      = reg.tipo_seguro
                AND    tipo_pension     = reg.tipo_pension
                AND    tipo_prestacion  = reg.tipo_prestacion

            SELECT estado_solicitud
            INTO   vestado_solicitud
            FROM   ret_solicitud_tx
            WHERE  nss              = reg.nss
                AND    tipo_retiro      = "G"
            AND    tipo_seguro      = reg.tipo_seguro
            AND    tipo_pension     = reg.tipo_pension
            AND    tipo_prestacion  = reg.tipo_prestacion
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
               WHERE  A.nss         = reg.nss
                       AND    A.consecutivo = vconsecutivo

               IF STATUS = NOTFOUND THEN
                   DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                   "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                           NEXT FIELD nss
                       ELSE }
                   SELECT "OK"
                   FROM   ret_solicitud_tx A
                   WHERE  A.nss           = reg.nss
                           AND    A.consecutivo   = vconsecutivo
                   AND    A.diag_registro = 400

                   IF STATUS <> NOTFOUND THEN
                    SELECT "OK"
                    FROM   dis_cuenta A
                    WHERE  A.nss           = reg.nss
                 AND    A.consecutivo_lote = reg.consecutivo
                 GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                            "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                                    NEXT FIELD nss
                    END IF
               END IF
                       --END IF
            END CASE

                SELECT n_unico ,
                       n_rfc   ,
                       paterno ,
                       materno ,
                       nombres
                INTO   reg.curp ,
                       reg.n_rfc   ,
                       reg.paterno ,
                       reg.materno ,
                       reg.nombres
                FROM   afi_mae_afiliado
                WHERE  n_seguro = reg.nss
                GROUP BY 1,2,3,4,5

                IF STATUS = NOTFOUND THEN
                    SELECT "OK"
                    FROM   afi_solicitud
                    WHERE  n_seguro = reg.nss
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
                    DISPLAY BY NAME reg.curp
                    DISPLAY BY NAME reg.n_rfc
                    DISPLAY BY NAME reg.paterno
                    DISPLAY BY NAME reg.materno
                    DISPLAY BY NAME reg.nombres
                END IF
                    NEXT FIELD regimen
            END IF

        AFTER FIELD regimen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  nss
            END IF

            IF reg.regimen IS NULL THEN
                 CALL despliega_tipo_regimen() #dtr
                 DISPLAY reg.regimen TO regimen
                 NEXT FIELD  autoridad
            ELSE
                SELECT "OK"
                FROM   tab_regimen
                WHERE  regimen = reg.regimen
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE REGIMEN    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_regimen() #dtr
                                DISPLAY reg.regimen TO regimen
                                NEXT FIELD autoridad
                            ELSE
                                NEXT FIELD regimen
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.regimen TO regimen

        AFTER FIELD fecha_oficio
        AFTER FIELD no_oficio
        AFTER FIELD autoridad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  regimen
            END IF
{svera
            IF reg.autoridad IS NULL THEN
                 CALL despliega_autoridad() #da
                 DISPLAY reg.autoridad TO autoridad

                 NEXT FIELD folio_solicitud
            ELSE
                SELECT "OK"
                FROM   TABLA
                WHERE  CODIGO = reg.autoridad
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION"
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_autoridad() #da
                                DISPLAY reg.autoridad TO autoridad
                                NEXT FIELD folio_solicitud
                            ELSE
                               NEXT FIELD autoridad
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.autoridad TO autoridad
svera}

        AFTER FIELD folio_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD autoridad
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD folio_solicitud
            END IF

            IF reg.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF reg.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

            END IF

            IF HOY <> reg.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF reg.fecha_solicitud < xx_fecha_solicitud THEN
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

        AFTER FIELD grupo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF reg.grupo IS NULL THEN
                 CALL despliega_grupo() #dg
                 DISPLAY reg.grupo TO grupo
                 DISPLAY reg.desc_grupo TO desc_grupo
                 NEXT FIELD  semanas_cotizadas
            ELSE
                SELECT descripcion
                INTO   reg.desc_grupo
                FROM   tab_grupo
                WHERE  grupo = reg.grupo
                GROUP BY 1

                DISPLAY reg.desc_grupo TO desc_grupo
                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE GRUPO    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_grupo() #dg
                                DISPLAY reg.grupo TO grupo
                                DISPLAY reg.desc_grupo TO desc_grupo
                                NEXT FIELD semanas_cotizadas
                            ELSE
                                NEXT FIELD grupo
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.grupo TO grupo
            DISPLAY reg.desc_grupo TO desc_grupo

        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD grupo
            END IF

      ON KEY (ESC)

                IF reg.nss IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD nss
                ELSE
                LET vestado_solicitud  = " "

                    SELECT MAX(consecutivo)
                INTO   vconsecutivo
                FROM   ret_solicitud_tx
                WHERE  nss              = reg.nss
                    AND    tipo_retiro      = "G"
                    AND    tipo_seguro      = reg.tipo_seguro
                    AND    tipo_pension     = reg.tipo_pension
                    AND    tipo_prestacion  = reg.tipo_prestacion

                SELECT estado_solicitud
                INTO   vestado_solicitud
                FROM   ret_solicitud_tx
                WHERE  nss              = reg.nss
                    AND    tipo_retiro      = "G"
                AND    tipo_seguro      = reg.tipo_seguro
                AND    tipo_pension     = reg.tipo_pension
                AND    tipo_prestacion  = reg.tipo_prestacion
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
                    { SELECT "OK"
                    FROM   ret_solicitud_rx A
                    WHERE  A.nss         = reg.nss
                            AND    A.consecutivo = vconsecutivo

                    IF STATUS = NOTFOUND THEN
                        DISPLAY "  SOLICITUD EN PROCESO DE RETIRO",
                                        "" AT 22,1 ATTRIBUTE(NORMAL) SLEEP 3
                                NEXT FIELD nss
                            ELSE}
                        SELECT "OK"
                        FROM   ret_solicitud_tx A
                        WHERE  A.nss           = reg.nss
                                AND    A.consecutivo   = vconsecutivo
                        AND    A.diag_registro = 400

                        IF STATUS <> NOTFOUND THEN
                        SELECT "OK"
                        FROM   dis_cuenta A
                        WHERE  A.nss              = reg.nss
                        AND    A.consecutivo_lote = reg.consecutivo
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

                IF reg.regimen IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD regimen
                END IF

                IF reg.folio_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_solicitud
                END IF

                IF reg.fecha_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_solicitud
                ELSE
                    IF reg.fecha_solicitud > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_solicitud
                    END IF
                END IF

            IF HOY <> reg.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF reg.fecha_solicitud < xx_fecha_solicitud THEN
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
                IF reg.grupo IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD grupo
                END IF

                IF reg.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF

                ----- MARCAJE -----

        SELECT movimiento
        INTO   s_tipo_movimiento
        FROM   tab_retiro
        WHERE  tipo_retiro = "G"

                CALL marca_cuenta (reg.nss           ,
                                   s_tipo_movimiento ,
                                   reg.consecutivo
                                  )#mc
                RETURNING v_marca_res ,
                          v_rechazo_cod

                LET reg.rechazo_cod = v_rechazo_cod

                IF v_rechazo_cod > 0 THEN

                    SELECT A.rechazo_desc
                    INTO   desc_rechazo_cod
                    FROM   tab_rch_marca A
                    WHERE  A.rechazo_cod = v_rechazo_cod

                    PROMPT " SOLICITUD RECHAZADA(",v_rechazo_cod,"  ",
                desc_rechazo_cod CLIPPED,
                ")<ENTER> CONTINUAR" FOR CHAR enter
                END IF

        WHENEVER ERROR CONTINUE
                INSERT INTO ret_solicitud_tx
                VALUES(reg.nss                 ,
                       reg.consecutivo         ,
                       0                       , #folio
                       reg.folio_solicitud     ,
                       ' '                     , #tipo_id
                       reg.curp                ,
                       ' '                     , #sec_pension
                       ' '                     , #tipo_documento
                       reg.tipo_retiro         ,
                       reg.regimen             ,
                       reg.tipo_seguro         ,
                       reg.tipo_pension        ,
                       reg.tipo_prestacion     ,
                       "01010001"              , #fecha_ini_pen
                       "01010001"              , #fecha_resolucion
                       reg.fecha_solicitud     ,
                       ' '                     , #cve_doc_probatorio
                       "01010001"              , #fecha_nacimiento
                       ' '                     , #aseguradora
                       ' '                     , #actuario
                       ' '                     , #num_plan_pension
                       0                       , #periodo_pago
                       0                       , #acciones_ret97
                       0                       , #acciones_cv
                       0                       , #acciones_cuota_soc
                       0                       , #acciones_ret92
                       ' '                     , #fecha_valor_viv
                       0                       , #saldo_viv97
                       0                       , #saldo_viv92
                       0                       , #saldo_viv72
                       ' '                     , #diag_registro
                       ' '                     , #estado_sub_viv
                       reg.semanas_cotizadas   ,
                       reg_1.capturado         , #estado_solicitud
                       ' '                     , #entidad
                       ' '                     , #cod_rechazo_ent
                       reg.rechazo_cod         , #rechazo_cod
                       reg.fecha_captura       ,
                       "01010001"              , #fecha_confirma
                       "01010001"              , #fecha_modifica
                       "01010001"              , #fecha_envio
                       reg.usuario_captura     , #usuario_captura
                       ' '                     , #usuario_confirma
                       ' '                     , #usuario_modifica
                       0                       , #carta
                       reg.grupo               , #grupo
                       "T"                     , #cve_destino
                       0                       , #porcentaje_val
                       0                       , #num_resolucion
                       " "                     , #paterno_sol
                       " "                     , #materno_sol
                       " "
                      )

---omar
             IF SQLCA.SQLCODE < 0 THEN
        LET x_error = "INSERT ret_solicitud_tx:",
                  "nss ",reg.nss,
                  "consecutivo ",reg.consecutivo,
                  err_get(SQLCA.SQLCODE)

                CALL errorlog(x_error CLIPPED)

        PROMPT "   ERROR AL INGRESAR REGISTROS AVISE A SISTEMAS "
        FOR enter
        EXIT PROGRAM
             END IF

         WHENEVER ERROR STOP

        SELECT "OK"
        FROM   ret_beneficiario
        WHERE  nss         = reg.nss
        AND    consecutivo = reg.consecutivo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
                    LET v_ejecuta = "fglgo RETM810 ",
                     reg.nss CLIPPED," ",
                     reg.consecutivo CLIPPED," ",'A'

                    RUN v_ejecuta
                ELSE
            ERROR "    YA SE HAN CAPTURADO BENEFICIARIOS    "
                ATTRIBUTE(REVERSE)
            END IF

                SELECT "OK"
                FROM   ret_beneficiario
                WHERE  nss         =  reg.nss
                AND    consecutivo =  reg.consecutivo
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
               ERROR "    NO SE PUEDE CAPTURAR LA SOLICITUD SIN BENEFICIARIOS"
                    ATTRIBUTE(NORMAL)

                    DELETE
                    FROM  ret_solicitud_tx
                    WHERE nss         = reg.nss
                    AND   consecutivo = reg.consecutivo

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
     CLOSE WINDOW retm8052
END FUNCTION

FUNCTION cat_tipo_ret()
#ctr-------------------
    DEFINE c_des_tipo_ret LIKE tab_retiro.descripcion

    SELECT descripcion
    INTO   c_des_tipo_ret
    FROM   tab_retiro
    WHERE  tipo_retiro = reg.tipo_retiro

    RETURN c_des_tipo_ret

END FUNCTION

FUNCTION despliega_tipo_regimen()
#dtr-----------------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm8053 AT 05,12 WITH FORM "RETM8024" ATTRIBUTE(BORDER)
    DISPLAY "                    TIPO DE REGIMEN                        " AT 2,1
    ATTRIBUTE(REVERSE)
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
        LET prepare_1 = " SELECT * FROM tab_regimen ",
                        " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_1 FROM prepare_1
        DECLARE cur_1 CURSOR FOR pre_1

        LET pos = 1

        FOREACH cur_1 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF

        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO TIPO DE REGIMEN VACIO"
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
                LET reg.regimen   = ra_reg[pos].codigo
                EXIT DISPLAY

        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm8053
END FUNCTION

FUNCTION matriz_de_derecho(reg_1)
#rmd----------------------------
    DEFINE   reg_1  RECORD  #glo #reg_1
        regimen                       ,
        tipo_seguro                   ,
        tipo_pension         CHAR(02) ,
        tipo_prestacion      SMALLINT ,
        tipo_retiro          CHAR(1)
    END RECORD

    DEFINE #glo #char
        enter                CHAR(01)

    DEFINE #glo #smallint
        x_grupo              SMALLINT

    SELECT grupo
    INTO   x_grupo
    FROM   ret_matriz_derecho
    WHERE  regimen         = reg_1.regimen
    AND    tipo_seguro     = reg_1.tipo_seguro
    AND    tipo_pension    = reg_1.tipo_pension
    AND    tipo_prestacion = reg_1.tipo_prestacion
    AND    tipo_retiro     = reg_1.tipo_retiro
    GROUP BY 1

    IF SQLCA.SQLCODE = NOTFOUND  THEN
        PROMPT " NO EXISTE RESGISTRO EN LA RET_MATRIZ_DERECHO " FOR CHAR enter
        EXIT PROGRAM
    END IF

    RETURN x_grupo

END FUNCTION

FUNCTION despliega_grupo()
#dg----------------------
    DEFINE  ra_reg ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE   prepare_1    CHAR(200),
             x_buscar     CHAR(030)

    OPEN WINDOW retm8054 AT 05,12 WITH FORM "RETM8024" ATTRIBUTE(BORDER)
    DISPLAY "                    G  R  U  P  O  S                       " AT 2,1
    ATTRIBUTE(REVERSE)
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
        LET prepare_1 = " SELECT * FROM tab_grupo ",
                        " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                        " ORDER BY 1 " CLIPPED

        PREPARE pre_2 FROM prepare_1
        DECLARE cur_2 CURSOR FOR pre_2

        LET pos = 1

        FOREACH cur_2 INTO ra_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF

        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "    ARCHIVO DE GRUPO VACIO"
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
                LET reg.grupo      = ra_reg[pos].codigo
                LET reg.desc_grupo = ra_reg[pos].descripcion
                EXIT DISPLAY

        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF
    END WHILE
    CLOSE WINDOW retm8054
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm8055 AT 2,3 WITH FORM "RETM8053" ATTRIBUTE (BORDER)
    DISPLAY " RETM805                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C:Salir      Ctrl-B:Dom Trab/Benef    Esc:Consulta" AT 1,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag              = FALSE

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.curp             ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
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
        CLOSE WINDOW retm8055
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #des_tipo_ret
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " ' '                 ,", #fecha_oficio
                      " ' '                 ,", #nro_oficio
                      " ' '                 ,", #autoridad
              {
                      " A.fecha_oficio      ,", #fecha_oficio
                      " A.no_oficio         ,", #nro_oficio
                      " A.autoridad         ,", #autoridad
              }
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.grupo             ,", #grupo
                      " ' '                 ,", #desc_grupo
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.folio             ,",
                      " A.consecutivo       ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                  "afi_mae_afiliado B,",
                  "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_seguro     = 'TJ'",
                      " AND   A.tipo_pension    = 'RE'",
                      " AND   A.tipo_prestacion = 8",
                      " AND   A.nss         = D.nss  ",
                      " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_3 INTO arr_1[i].*
        IF arr_1[i].diag_registro IS NULL OR
           arr_1[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  arr_1[i].diag_registro,
                  arr_1[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = arr_1[i].nss
           AND   consecutivo =  arr_1[i].consecutivo
        END IF


        SELECT descripcion,movimiento
        INTO   arr_1[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT descripcion
        INTO   arr_1[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = arr_1[i].tipo_prestacion

        SELECT descripcion
        INTO   arr_1[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        IF arr_1[i].fecha_confirma = "01010001" THEN
            LET arr_1[i].fecha_confirma = ""
        END IF

        IF arr_1[i].fecha_modifica = "01010001" THEN
            LET arr_1[i].fecha_modifica = ""
        END IF

        IF arr_1[i].fecha_envio = "01010001" THEN
            LET arr_1[i].fecha_envio = ""
        END IF

        SELECT descripcion
        INTO   arr_1[i].desc_grupo
        FROM   tab_grupo
        WHERE  grupo = arr_1[i].grupo

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss
        AND    consecutivo_lote = arr_1[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        DISPLAY "  NO EXISTE REGISTRO  ","" AT 22,1
        ATTRIBUTE(NORMAL) SLEEP 3
        CLOSE WINDOW retm8055

        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1 TO scr_1.*
        ON KEY (CONTROL-B)
            LET arr_c = ARR_CURR()
            LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss CLIPPED," ",arr_1[arr_c].consecutivo CLIPPED," ",'A'
            RUN v_ejecuta
        ON KEY ( CONTROL-C )
            CALL inicializa()
            EXIT DISPLAY
        ON KEY ( INTERRUPT )
            CALL inicializa()
            EXIT DISPLAY
    END DISPLAY
CLOSE WINDOW retm8055
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE #loc #smallint
        i                     ,
        sw_2                  ,
        vmodif             INTEGER

    OPEN WINDOW retm8056 AT 2,3 WITH FORM "RETM8053" ATTRIBUTE (BORDER)
    DISPLAY " RETM805                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C:Salir     Ctrl-B:Dom Trab/Benef     Esc:Modifica" AT 1,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1    ATTRIBUTE(REVERSE)

    LET int_flag   = FALSE
    LET sw_2   = 1
    LET vmodif = 1

    IF vaccion = "M" THEN
        LET txt_1 = " SELECT A.nss               ,",
                    "        B.n_rfc             ,",
                    "        A.curp              ,",
                    "        B.paterno           ,",
                    "        B.materno           ,",
                    "        B.nombres           ,",
                    "        A.tipo_retiro       ,",
                    "        ' '                 ,", #des_tipo_ret
                    "        A.tipo_prestacion   ,",
                    "        ' '                 ,", #desc_prestacion
                    "        A.regimen           ,",
                    "        A.tipo_seguro       ,",
                    "        ' '                 ,", #desc_seguro
                    "        A.tipo_pension      ,",
                    "        ' '                 ,", #desc_pension
                    "        ' '                 ,", #fecha_oficio
                    "        ' '                 ,", #nro_oficio
                    "        ' '                 ,", #autoridad
            {
                    "        A.fecha_oficio      ,", #fecha_oficio
                    "        A.no_oficio         ,", #nro_oficio
                    "        A.autoridad         ,", #autoridad
            }
                    "        A.folio_solicitud   ,",
                    "        A.fecha_solicitud   ,",
                    "        A.grupo             ,", #grupo
                    "        ' '                 ,", #desc_grupo
                    "        A.semanas_cotizadas ,",
                    "        A.cod_rechazo_ent   ,",
                    "        A.rechazo_cod       ,",
                    "        ' '                 ,", #rechazo_desc
                    "        A.fecha_captura     ,",
                    "        A.fecha_modifica    ,",
                    "        A.fecha_confirma    ,",
                    "        ' '                 ,", #fecha_liquida
                    "        A.estado_solicitud  ,",
                    "        ' '                 ,", #desc_estado
                    "        A.usuario_captura   ,",
                    "        A.usuario_modifica  ,",
                    "        A.usuario_confirma  ,",
                    "        A.folio             ,",
                    "        A.consecutivo       ,",
                    "        A.fecha_envio       ,",
                    "        A.diag_registro     ,",
                    "        D.estado_sub_viv     ",
                    " FROM   ret_solicitud_tx A  ,",
                    "        afi_mae_afiliado B  ,",
                    " OUTER  ret_monto_viv D   ",
                    " WHERE  A.nss = ",vnss,
                    " AND    A.nss             = B.n_seguro ",
                    " AND    A.tipo_seguro     = 'TJ' ",
                    " AND    A.tipo_pension    = 'RE' ",
                    " AND    A.tipo_prestacion = 8 ",
                    " AND    A.nss             = D.nss  ",
                    " AND    A.consecutivo     = D.consecutivo "
    ELSE
        CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                      B.n_rfc            ,
                                      A.curp             ,
                                      B.paterno          ,
                                      B.materno          ,
                                      B.nombres          ,
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
            CLOSE WINDOW retm8056
            RETURN
        END IF

        LET txt_1 = " SELECT A.nss               ,",
                    "        B.n_rfc             ,",
                    "        A.curp              ,",
                    "        B.paterno           ,",
                    "        B.materno           ,",
                    "        B.nombres           ,",
                    "        A.tipo_retiro       ,",
                    "        ' '                 ,", #des_tipo_ret
                    "        A.tipo_prestacion   ,",
                    "        ' '                 ,", #desc_prestacion
                    "        A.regimen           ,",
                    "        A.tipo_seguro       ,",
                    "        ' '                 ,", #desc_seguro
                    "        A.tipo_pension      ,",
                    "        ' '                 ,", #desc_pension
                    "        ' '                 ,", #fecha_oficio
                    "        ' '                 ,", #nro_oficio
                    "        ' '                 ,", #autoridad
            {
                    "        A.fecha_oficio      ,", #fecha_oficio
                    "        A.no_oficio         ,", #nro_oficio
                    "        A.autoridad         ,", #autoridad
            }
                    "        A.folio_solicitud   ,",
                    "        A.fecha_solicitud   ,",
                    "        A.grupo             ,", #grupo
                    "        ' '                 ,", #desc_grupo
                    "        A.semanas_cotizadas ,",
                    "        A.cod_rechazo_ent   ,",
                    "        A.rechazo_cod       ,",
                    "        ' '                 ,", #rechazo_desc
                    "        A.fecha_captura     ,",
                    "        A.fecha_modifica    ,",
                    "        A.fecha_confirma    ,",
                    "        ' '                 ,", #fecha_liquida
                    "        A.estado_solicitud  ,",
                    "        ' '                 ,", #desc_estado
                    "        A.usuario_captura   ,",
                    "        A.usuario_modifica  ,",
                    "        A.usuario_confirma  ,",
                    "        A.folio             ,",
                    "        A.consecutivo       ,",
                    "        A.fecha_envio       ,",
            "        A.diag_registro     ,",
            "        D.estado_sub_viv     ",
                    " FROM   ret_solicitud_tx A  ,",
            "        afi_mae_afiliado B  ,",
            " OUTER  ret_monto_viv D   ",
                    " WHERE  ",x_busca CLIPPED,
                    " AND    A.nss             = B.n_seguro ",
                    " AND    A.tipo_seguro     = 'TJ' ",
                    " AND    A.tipo_pension    = 'RE' ",
                    " AND    A.tipo_prestacion = 8 ",
                    " AND    A.nss             = D.nss  ",
                    " AND    A.consecutivo     = D.consecutivo "
    END IF

    PREPARE pre_4 FROM txt_1
    DECLARE cur_4 CURSOR FOR pre_4

    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_4 INTO arr_1[i].*
        IF arr_1[i].diag_registro IS NULL OR
           arr_1[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  arr_1[i].diag_registro,
                  arr_1[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = arr_1[i].nss
           AND   consecutivo =  arr_1[i].consecutivo
        END IF


        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].estado_solicitud <> reg_1.capturado THEN

            PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   arr_1[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT descripcion
        INTO   arr_1[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = arr_1[i].tipo_prestacion

        SELECT descripcion
        INTO   arr_1[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        IF arr_1[i].fecha_confirma = "01010001" THEN
            LET arr_1[i].fecha_confirma = ""
        END IF

        IF arr_1[i].fecha_modifica = "01010001" THEN
            LET arr_1[i].fecha_modifica = ""
        END IF

        IF arr_1[i].fecha_envio = "01010001" THEN
            LET arr_1[i].fecha_envio = ""
        END IF

        SELECT descripcion
        INTO   arr_1[i].desc_grupo
        FROM   tab_grupo
        WHERE  grupo = arr_1[i].grupo

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss
        AND    consecutivo_lote = arr_1[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    ERROR ""
    LET cont_reg = i-1

    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF (i-1) >= 1 THEN
       CALL SET_COUNT(i-1)

       DISPLAY ARRAY arr_1 TO scr_1.*
          ON KEY ( CONTROL-C )
             LET int_flag=  TRUE
             EXIT DISPLAY

          ON KEY ( INTERRUPT )
             LET int_flag=  TRUE
             EXIT DISPLAY

      ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss CLIPPED," ",
                         arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
             EXIT DISPLAY
       END DISPLAY
    ELSE
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8056
       RETURN
    END IF

    IF int_flag = TRUE THEN
        LET int_flag=FALSE
        CLOSE WINDOW retm8056
        RETURN
    END IF

    CALL construccion(arr_1[pos].*,vmodif)

    CLOSE WINDOW retm8056
END FUNCTION

FUNCTION construccion(reg_mod,vmodif2)

#c------------------------------------
    DEFINE reg_mod RECORD
        nss                LIKE ret_solicitud_tx.nss               ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc             ,
        curp               LIKE afi_mae_afiliado.n_unico           ,
        paterno            LIKE afi_mae_afiliado.paterno           ,
        materno            LIKE afi_mae_afiliado.materno           ,
        nombres            LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro        LIKE ret_solicitud_tx.tipo_retiro       ,
        des_tipo_ret       LIKE tab_retiro.descripcion             ,
        tipo_prestacion    LIKE ret_solicitud_tx.tipo_prestacion   ,
        desc_prestacion    LIKE tab_prestacion.descripcion         ,
        regimen            LIKE ret_solicitud_tx.regimen           ,
        tipo_seguro        LIKE ret_solicitud_tx.tipo_seguro       ,
        desc_seguro        LIKE tab_seguro.descripcion             ,
        tipo_pension       LIKE ret_solicitud_tx.tipo_pension      ,
        desc_pension       LIKE tab_pension.descripcion            ,
        fecha_oficio       DATE                                    ,
        no_oficio          INTEGER                                 ,
        autoridad          CHAR(9)                                 ,
        folio_solicitud    LIKE ret_solicitud_tx.folio_solicitud   ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud   ,
        grupo              LIKE tab_grupo.grupo                    ,
        desc_grupo         LIKE tab_grupo.descripcion              ,
        semanas_cotizadas  LIKE ret_solicitud_tx.semanas_cotizadas ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent   ,
        rechazo_cod        LIKE ret_solicitud_tx.rechazo_cod       ,
        rechazo_desc       LIKE tab_rch_marca.rechazo_desc         ,
        fecha_captura      LIKE ret_solicitud_tx.fecha_captura     ,
        fecha_modifica     LIKE ret_solicitud_tx.fecha_modifica    ,
        fecha_confirma     LIKE ret_solicitud_tx.fecha_confirma    ,
        fecha_liquida      DATE                                    ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud  ,
        desc_estado        CHAR(25)                                ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura   ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica  ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma  ,
        folio              LIKE ret_solicitud_tx.folio             ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio       ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE #loc #smallint
        i                     SMALLINT

    DEFINE #loc #char
        opc                   CHAR(01)

    DEFINE #loc #integer
        vmodif2               INTEGER

    LET reg.nss                = reg_mod.nss
    LET reg.n_rfc              = reg_mod.n_rfc
    LET reg.curp               = reg_mod.curp
    LET reg.paterno            = reg_mod.paterno
    LET reg.materno            = reg_mod.materno
    LET reg.nombres            = reg_mod.nombres
    LET reg.tipo_retiro        = reg_mod.tipo_retiro
    LET reg.des_tipo_ret       = reg_mod.des_tipo_ret
    LET reg.tipo_prestacion    = reg_mod.tipo_prestacion
    LET reg.desc_prestacion    = reg_mod.desc_prestacion
    LET reg.regimen            = reg_mod.regimen
    LET reg.tipo_seguro        = reg_mod.tipo_seguro
    LET reg.desc_seguro        = reg_mod.desc_seguro
    LET reg.tipo_pension       = reg_mod.tipo_pension
    LET reg.desc_pension       = reg_mod.desc_pension
    LET reg.fecha_oficio       = reg_mod.fecha_oficio
    LET reg.no_oficio          = reg_mod.no_oficio
    LET reg.autoridad          = reg_mod.autoridad
    LET reg.folio_solicitud    = reg_mod.folio_solicitud
    LET reg.fecha_solicitud    = reg_mod.fecha_solicitud
    LET reg.grupo              = reg_mod.grupo
    LET reg.desc_grupo         = reg_mod.desc_grupo
    LET reg.semanas_cotizadas  = reg_mod.semanas_cotizadas
    LET reg.cod_rechazo_ent    = reg_mod.cod_rechazo_ent
    LET reg.rechazo_cod        = reg_mod.rechazo_cod
    LET reg.rechazo_desc       = reg_mod.rechazo_desc
    LET reg.fecha_captura      = reg_mod.fecha_captura
    LET reg.fecha_modifica     = reg_mod.fecha_modifica
    LET reg.fecha_confirma     = reg_mod.fecha_confirma
    LET reg.fecha_liquida      = reg_mod.fecha_liquida
    LET reg.estado_solicitud   = reg_mod.estado_solicitud
    LET reg.desc_estado        = reg_mod.desc_estado
    LET reg.usuario_captura    = reg_mod.usuario_captura
    LET reg.usuario_modifica   = reg_mod.usuario_modifica
    LET reg.usuario_confirma   = reg_mod.usuario_confirma
    LET reg.folio              = reg_mod.folio
    LET reg.consecutivo        = reg_mod.consecutivo

    INPUT BY NAME reg.regimen            ,
                  reg.autoridad          ,
                  reg.folio_solicitud    ,
                  reg.fecha_solicitud    ,
                  reg.grupo              ,
                  reg.semanas_cotizadas  WITHOUT DEFAULTS

        BEFORE FIELD regimen
            IF vmodif2 = 1 THEN
                LET reg.fecha_modifica   = HOY
                LET reg.usuario_modifica = usuario

                DISPLAY BY NAME reg.fecha_modifica  ,
                                reg.usuario_modifica

            END IF

            IF vmodif2 = 2 THEN
                LET reg.fecha_confirma   = HOY
                LET reg.usuario_confirma = usuario

                DISPLAY BY NAME reg.fecha_confirma  ,
                                reg.usuario_confirma
            END IF

        AFTER FIELD regimen
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  semanas_cotizadas
            END IF

            IF reg.regimen IS NULL THEN
                 CALL despliega_tipo_regimen() #dtr
                 DISPLAY reg.regimen TO regimen

                 NEXT FIELD  autoridad
            ELSE
                SELECT "OK"
                FROM   tab_regimen
                WHERE  regimen = reg.regimen
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE REGIMEN    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_tipo_regimen() #dtr
                                DISPLAY reg.regimen TO regimen
                                NEXT FIELD autoridad
                            ELSE
                                NEXT FIELD regimen
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.regimen TO regimen

        AFTER FIELD autoridad
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD  regimen
            END IF
{svera
            IF reg.autoridad IS NULL THEN
                 CALL despliega_autoridad() #da
                 DISPLAY reg.autoridad TO autoridad

                 NEXT FIELD folio_solicitud
            ELSE
                SELECT "OK"
                FROM   TABLA
                WHERE  CODIGO = reg.autoridad
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE PENSION"
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_autoridad() #da
                                DISPLAY reg.autoridad TO autoridad
                                NEXT FIELD folio_solicitud
                            ELSE
                               NEXT FIELD autoridad
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.autoridad TO autoridad
svera}

        AFTER FIELD folio_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD autoridad
            END IF

        AFTER FIELD fecha_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD folio_solicitud
            END IF

            IF reg.fecha_solicitud IS NULL THEN
                ERROR "    CAMPO NO PUEDE SER NULO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_solicitud
            ELSE
                IF reg.fecha_solicitud > HOY THEN
                    ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD fecha_solicitud
                END IF

            END IF

            IF HOY <> reg.fecha_solicitud THEN
               LET xx_fecha_solicitud = HOY - 2 UNITS DAY

           IF reg.fecha_solicitud < xx_fecha_solicitud THEN
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
        AFTER FIELD grupo
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_solicitud
            END IF

            IF reg.grupo IS NULL THEN
                 CALL despliega_grupo() #dg
                 DISPLAY reg.grupo TO grupo
                 DISPLAY reg.desc_grupo TO desc_grupo
                 NEXT FIELD  semanas_cotizadas
            ELSE
                SELECT "OK"
                FROM   tab_grupo
                WHERE  grupo = reg.grupo
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    CODIGO DIGITADO NO SE ENCUENTRA REGISTRADO EN ",
                          "CATALOGO TIPO DE GRUPO    "
                    WHILE TRUE
                        PROMPT "  DESEA PANTALLA DE AYUDA S/N " FOR CHAR enter
                        IF enter MATCHES "[SsNn]" THEN
                            IF enter MATCHES "[Ss]" THEN
                                CALL despliega_grupo() #dg
                                DISPLAY reg.grupo TO grupo
                                DISPLAY reg.desc_grupo TO desc_grupo
                                NEXT FIELD semanas_cotizadas
                            ELSE
                                NEXT FIELD grupo
                            END IF
                        END IF
                    END WHILE
                END IF
            END IF

            DISPLAY reg.grupo TO grupo
            DISPLAY reg.desc_grupo TO desc_grupo

        AFTER FIELD semanas_cotizadas
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD grupo
            END IF

            ------------- sil------------------
            ON KEY (CONTROL-F)
            IF xaccion = "F" THEN
               LET arr_c =  arr_curr()
               CALL  rechazar(reg.nss,reg.consecutivo,
                          reg.tipo_prestacion,reg.tipo_retiro)
               RETURNING  codigo,entidad
               LET vmodif2 = 3
               LET reg.cod_rechazo_ent = codigo
               DISPLAY BY NAME reg.cod_rechazo_ent
             END IF
            --------------------------------------

      ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",reg.nss CLIPPED," ",
                         reg.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
            ON KEY ( CONTROL-C )
                EXIT INPUT

            ON KEY ( INTERRUPT )
                EXIT INPUT

            ON KEY (ESC)

                IF reg.regimen IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD regimen
                END IF

                IF reg.folio_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_solicitud
                END IF

                IF reg.fecha_solicitud IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD fecha_solicitud
                ELSE
                    IF reg.fecha_solicitud > HOY THEN
                        ERROR "    FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_solicitud
                    END IF
                END IF

                IF reg.grupo IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD grupo
                END IF

                IF reg.semanas_cotizadas IS NULL THEN
                    ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                    NEXT FIELD semanas_cotizadas
                END IF
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
            IF reg.estado_solicitud = reg_1.capturado THEN
                CALL pre_gunta2() RETURNING opc
                IF opc MATCHES "[Nn]" THEN
                    DISPLAY "  CONFIRMACION CANCELADA  " AT 22,1 ATTRIBUTE(REVERSE) 
                    SLEEP 3
                    CALL inicializa() #i
                    RETURN
                END IF
            ELSE
                DISPLAY " EL REGISTRO DEBE ESTAR EN ESTADO CAPTURADO " AT 22,1 ATTRIBUTE(REVERSE) 
                SLEEP 3
                CALL inicializa() #i
                RETURN
            END IF

         ELSE
               IF vmodif2 = 3 THEN
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
         SET    ret_solicitud_tx.regimen          =reg.regimen          ,
                ret_solicitud_tx.folio_solicitud  =reg.folio_solicitud  ,
                ret_solicitud_tx.fecha_solicitud  =reg.fecha_solicitud  ,
                ret_solicitud_tx.grupo            =reg.grupo            ,
                ret_solicitud_tx.semanas_cotizadas=reg.semanas_cotizadas,
                ret_solicitud_tx.usuario_modifica =reg.usuario_modifica ,
                ret_solicitud_tx.fecha_modifica   =reg.fecha_modifica
         WHERE  ret_solicitud_tx.nss      =reg.nss
         AND  ret_solicitud_tx.consecutivo      =reg.consecutivo
---omar
         IF SQLCA.SQLCODE < 0 THEN
        LET x_error = "UPDATE ret_solicitud_tx:",
                 "nss ",reg.nss,
                 "consecutivo ",reg.consecutivo,
                 err_get(SQLCA.SQLCODE)

                   CALL errorlog(x_error CLIPPED)

           PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS "
           FOR enter
           EXIT PROGRAM
          END IF

          WHENEVER ERROR STOP
          DISPLAY "  REGISTRO MODIFICADO  ","" AT 22,1
                    ATTRIBUTE(REVERSE)
          SLEEP 3
          CALL inicializa() #i
          INITIALIZE reg_mod.* TO NULL
          RETURN
       ELSE
          IF vmodif2 = 2 THEN
            IF reg.estado_solicitud = reg_1.capturado THEN

                WHENEVER ERROR CONTINUE
                 UPDATE ret_solicitud_tx
                 SET ret_solicitud_tx.regimen         =reg.regimen          ,
                     ret_solicitud_tx.folio_solicitud  =reg.folio_solicitud  ,
                     ret_solicitud_tx.fecha_solicitud  =reg.fecha_solicitud  ,
                     ret_solicitud_tx.grupo            =reg.grupo            ,
                     ret_solicitud_tx.semanas_cotizadas=reg.semanas_cotizadas,
                     ret_solicitud_tx.usuario_confirma =reg.usuario_confirma ,
                     ret_solicitud_tx.fecha_confirma   =reg.fecha_confirma   ,
                     ret_solicitud_tx.estado_solicitud =reg_1.confirmado
                 WHERE  ret_solicitud_tx.nss  =reg.nss
                 AND  ret_solicitud_tx.consecutivo  =reg.consecutivo
---omar
                IF SQLCA.SQLCODE < 0 THEN
                    LET x_error = "UPDATE ret_solicitud_tx:",
                        "nss ",reg.nss,
                        "consecutivo ",reg.consecutivo,
                        err_get(SQLCA.SQLCODE)
                
                          CALL errorlog(x_error CLIPPED)
                
                    PROMPT " ERROR DE UPDATE ret_solicitud_tx AVISE A SISTEMAS "
                    FOR enter
                    EXIT PROGRAM
                END IF
                
                WHENEVER ERROR STOP
                DISPLAY "  REGISTRO CONFIRMADO  ","" AT 22,1
                           ATTRIBUTE(REVERSE)
                SLEEP 3
                CALL inicializa() #i
                INITIALIZE reg_mod.* TO NULL
                RETURN
            ELSE                                                                                  
                DISPLAY " EL REGISTRO DEBE ESTAR EN ESTADO CAPTURADO " AT 22,1 ATTRIBUTE(REVERSE) 
                SLEEP 3                                                                           
                CALL inicializa() #i                                                              
                INITIALIZE reg_mod.* TO NULL
                RETURN                                                                            
            END IF                                                                                

          ELSE -- Se rechaza la solicitud --
             -----------------------------
           IF  vmodif2 = 3 THEN
               CALL pre_gunta3() RETURNING opc
               IF opc MATCHES "[Ss]" THEN
                  -----------------------------
                  -- SE ACTUALIZA EL REGISTO --
                  UPDATE ret_solicitud_tx
                  SET ret_solicitud_tx.estado_solicitud  =
                      reg_1.rechazado,
                      ret_solicitud_tx.cod_rechazo_ent   =
                      reg.cod_rechazo_ent,
                      ret_solicitud_tx.entidad =entidad
                  WHERE ret_solicitud_tx.nss = reg.nss
                  AND ret_solicitud_tx.consecutivo = reg.consecutivo
                  ---------------------------------------------
                  ---------- Se desmarca la cuenta -------------
                  -- Se consulta tipo_movimento para retiro G --
                  SELECT movimiento
                  INTO v_tipo_movimiento
                  FROM tab_retiro
                  WHERE tipo_retiro = "G"
                  LET vestado_marca   = 0
                  LET vmarca_causa    = 0

                  LET vdesmarca = "EXECUTE PROCEDURE desmarca_cuenta('",
                      reg.nss,"',",v_tipo_movimiento,",",
                      reg.consecutivo,",",vestado_marca,",",
                      vmarca_causa,",' ",usuario,"')"
                      PREPARE exec_desmarca FROM vdesmarca
                      EXECUTE exec_desmarca
                  ---------------------------------------
                  DISPLAY " EL REGISTRO SE RECHAZO CORRECTAMENTE ","" AT 22,1
                    ATTRIBUTE(REVERSE)
                  SLEEP 3
                  CALL inicializa() #i
                  INITIALIZE reg_mod.* TO NULL
               ELSE
                DISPLAY " RECHAZO CANCELADO " AT 22,1 ATTRIBUTE(REVERSE) SLEEP 3
                CALL inicializa() #i
               END IF
             END IF -- fin vmodif=3
          END IF -- fin vmodif=2
      END IF -- fin vmodif=1

END FUNCTION

FUNCTION pre_gunta()
#------------------
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
#-------------------
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
#-------------------
     DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT "ESTA SEGURO DE RECHAZAR LA SOLICITUD  S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
            EXIT WHILE
         END IF
     END WHILE
     RETURN opc
END FUNCTION

FUNCTION elimina()
#e----------------
    DEFINE #loc #smallint
        cont_reg2             ,
        arr                   ,
        src                   ,
        i                     SMALLINT

    DEFINE #loc #integer
        arr_c                 INTEGER


    OPEN WINDOW retm8057 AT 2,3 WITH FORM "RETM8053" ATTRIBUTE (BORDER)
    DISPLAY " RETM805                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "ELIMINA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER> : Elimina   Ctrl-B : Beneficiarios  Ctrl-C : Salir " AT 2,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1 ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag              = FALSE
    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.curp             ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
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
        CLOSE WINDOW retm8057
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #des_tipo_ret
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " ' '                 ,", #fecha_oficio
                      " ' '                 ,", #nro_oficio
                      " ' '                 ,", #autoridad
              {
                      " A.fecha_oficio      ,", #fecha_oficio
                      " A.no_oficio         ,", #nro_oficio
                      " A.autoridad         ,", #autoridad
              }
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.grupo             ,", #grupo
                      " ' '                 ,", #desc_grupo
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " A.folio             ,",
                      " A.consecutivo       ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                      "OUTER ret_monto_viv D ",
                   " WHERE ",x_busca CLIPPED,
                   " AND   A.nss = B.n_seguro  ",
                   " AND   A.tipo_seguro     = 'TJ'",
                   " AND   A.tipo_pension    = 'RE'",
                   " AND   A.tipo_prestacion = 8",
                   " AND   A.nss         = D.nss  ",
                   " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_5 FROM txt_1
    DECLARE cur_5 CURSOR FOR pre_5

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_5 INTO arr_1[i].*
        IF arr_1[i].diag_registro IS NULL OR
           arr_1[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  arr_1[i].diag_registro,
                  arr_1[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = arr_1[i].nss
           AND   consecutivo =  arr_1[i].consecutivo
        END IF


        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF ((arr_1[i].estado_solicitud <> reg_1.capturado) AND 
            (arr_1[i].estado_solicitud <> reg_1.precapturado)  ) THEN

            PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   arr_1[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT descripcion
        INTO   arr_1[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = arr_1[i].tipo_prestacion

        SELECT descripcion
        INTO   arr_1[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        IF arr_1[i].fecha_confirma = "01010001" THEN
            LET arr_1[i].fecha_confirma = ""
        END IF

        IF arr_1[i].fecha_modifica = "01010001" THEN
            LET arr_1[i].fecha_modifica = ""
        END IF

        IF arr_1[i].fecha_envio = "01010001" THEN
            LET arr_1[i].fecha_envio = ""
        END IF

        SELECT descripcion
        INTO   arr_1[i].desc_grupo
        FROM   tab_grupo
        WHERE  grupo = arr_1[i].grupo

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss
        AND    consecutivo_lote = arr_1[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    LET cont_reg = i-1

    ERROR ""
    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_1 TO scr_1.*

      ON KEY (CONTROL-B)
         LET      arr_c           =  arr_curr()
         LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss CLIPPED," ",
                         arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
        ON KEY ( CONTROL-M )

            PROMPT "  DESEA ELIMINAR EL REGISTRO S/N " FOR CHAR enter
            IF enter MATCHES "[Ss]" THEN

               LET arr = ARR_CURR()
               LET src = SCR_LINE()

               SELECT A.estado_solicitud
               INTO   v_status
               FROM   ret_solicitud_tx A
               WHERE  A.nss         = arr_1[arr].nss
               AND    A.consecutivo = arr_1[arr].consecutivo

               IF ( (v_status <> reg_1.capturado) AND 
                     (v_status <> reg_1.precapturado)  ) THEN

                  PROMPT "  NO PUEDE SER ELIMINADO ESTE REGISTRO" FOR CHAR enter
                  EXIT DISPLAY
               ELSE
                  WHENEVER ERROR CONTINUE
                  DELETE FROM ret_beneficiario
                  WHERE  nss = arr_1[arr].nss
                  AND  consecutivo = arr_1[arr].consecutivo
                  
                  IF SQLCA.SQLCODE < 0 THEN
                     LET x_error = "DELETE ret_beneficiario:",
                            "nss ",arr_1[arr].nss,
                        "consecutivo ",arr_1[arr].consecutivo,
                        err_get(SQLCA.SQLCODE)
                  
                      CALL errorlog(x_error CLIPPED)
                  
                      PROMPT "  ERROR DELETE ret_beneficiario AVISE A SISTEMAS "
                        FOR enter
                      EXIT PROGRAM
                  END IF

                  DELETE
                  FROM  ret_solicitud_tx
                  WHERE ret_solicitud_tx.consecutivo = arr_1[arr].consecutivo
                  AND   ret_solicitud_tx.nss         = arr_1[arr].nss
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_solicitud_tx:",
                   "nss ",arr_1[arr].nss,
                   "consecutivo ",arr_1[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

             PROMPT " ERROR DE DELETE ret_solicitud_tx AVISE A SISTEMAS "
             FOR enter
             EXIT PROGRAM
                  END IF

                  DELETE
                  FROM  ret_consecutivo
                  WHERE consecutivo = arr_1[arr].consecutivo

                  ----- REVERSAR MARCAJE -----


                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = "G"

                  LET reg_rev.marca_cod   = s_tipo_movimiento
                  LET reg_rev.nss         = arr_1[arr].nss
                  LET reg_rev.correlativo = arr_1[arr].consecutivo

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

                  ----------------------------

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
CLOSE WINDOW retm8057
END FUNCTION

FUNCTION confirma()
#cc----------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER,
        vmodif                INTEGER

    OPEN WINDOW retm8056 AT 2,3 WITH FORM "RETM8053" ATTRIBUTE (BORDER)
    DISPLAY " RETM805                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C:Salir  Ctrl-F:Rechazar  Ctrl-B:Dom Trab/Benef   Esc:Confirma" AT 1,1
    DISPLAY "                         DATOS DE LA SOLICITUD                                 " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 16,1    ATTRIBUTE(REVERSE)

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET int_flag  = FALSE
    LET vmodif    = 2

    CONSTRUCT BY NAME x_busca ON  A.nss              ,
                                  B.n_rfc            ,
                                  A.curp             ,
                                  B.paterno          ,
                                  B.materno          ,
                                  B.nombres          ,
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
        CLOSE WINDOW retm8056
        RETURN
    END IF

    LET txt_1 =" SELECT A.nss               ,",
                      " B.n_rfc             ,",
                      " A.curp              ,",
                      " B.paterno           ,",
                      " B.materno           ,",
                      " B.nombres           ,",
                      " A.tipo_retiro       ,",
                      " ' '                 ,", #des_tipo_ret
                      " A.tipo_prestacion   ,",
                      " ' '                 ,", #desc_prestacion
                      " A.regimen           ,",
                      " A.tipo_seguro       ,",
                      " ' '                 ,", #desc_seguro
                      " A.tipo_pension      ,",
                      " ' '                 ,", #desc_pension
                      " ' '                 ,", #fecha_oficio
                      " ' '                 ,", #nro_oficio
                      " ' '                 ,", #autoridad
              {
                      " A.fecha_oficio      ,", #fecha_oficio
                      " A.no_oficio         ,", #nro_oficio
                      " A.autoridad         ,", #autoridad
              }
                      " A.folio_solicitud   ,",
                      " A.fecha_solicitud   ,",
                      " A.grupo             ,", #grupo
                      " ' '                 ,", #desc_grupo
                      " A.semanas_cotizadas ,",
                      " A.cod_rechazo_ent   ,",
                      " A.rechazo_cod       ,",
                      " ' '                 ,", #rechazo_desc
                      " A.fecha_captura     ,",
                      " A.fecha_modifica    ,",
                      " A.fecha_confirma    ,",
                      " ' '                 ,", #fecha_liquida
                      " A.estado_solicitud  ,",
                      " ' '                 ,", #desc_estado
                      " A.usuario_captura   ,",
                      " A.usuario_modifica  ,",
                      " A.usuario_confirma  ,",
                      " a.folio             ,",
                      " A.consecutivo       ,",
                      " A.fecha_envio       ,",
                      " A.diag_registro     ,",
                      " D.estado_sub_viv     ",
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                      "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND    A.estado_solicitud IN (?,?) ",
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_seguro     = 'TJ'",
                      " AND   A.tipo_pension    = 'RE'",
                      " AND   A.tipo_prestacion = 8",
                      " AND   A.nss         = D.nss  ",
                      " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_6 FROM txt_1
    DECLARE cur_6 CURSOR FOR pre_6

    LET i = 1
    ERROR "PROCESANDO INFORMACION"
    FOREACH cur_6 USING reg_1.precapturado, 
                        reg_1.capturado
                  INTO arr_1[i].*

        IF arr_1[i].diag_registro IS NULL OR
           arr_1[i].diag_registro = " " THEN
           SELECT diag_registro,estado_sub_viv INTO
                  arr_1[i].diag_registro,
                  arr_1[i].estado_sub_viv
           FROM ret_solicitud_rx
           WHERE nss = arr_1[i].nss
           AND   consecutivo =  arr_1[i].consecutivo
        END IF

        SELECT usuario_captura
        INTO   x_usuario
        FROM   ret_solicitud_tx
        WHERE  nss = arr_1[i].nss
        AND    consecutivo = arr_1[i].consecutivo

   { IF x_usuario = usuario THEN
      PROMPT "USUARIO ES EL MISMO DE CAPTURA" ATTRIBUTE (REVERSE)
             FOR opc ATTRIBUTE (REVERSE)
      LET sw = 1
      EXIT FOREACH
   END IF }

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF ((arr_1[i].estado_solicitud <> reg_1.capturado) AND 
            (arr_1[i].estado_solicitud <> reg_1.precapturado)  ) THEN

            PROMPT "SOLICITUD DEL NSS <",arr_1[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
     
            CONTINUE FOREACH
        END IF

        SELECT descripcion,movimiento
        INTO   arr_1[i].des_tipo_ret,varmov
        FROM   tab_retiro
        WHERE  tipo_retiro = arr_1[i].tipo_retiro

        SELECT descripcion
        INTO   arr_1[i].desc_prestacion
        FROM   tab_prestacion
        WHERE  tipo_prestacion = arr_1[i].tipo_prestacion

        SELECT descripcion
        INTO   arr_1[i].desc_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_pension
        FROM   tab_pension
        WHERE  tipo_pension = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_estado
        FROM   ret_estado
        WHERE  estado_solicitud = arr_1[i].estado_solicitud

        IF arr_1[i].rechazo_cod <> 0 THEN
            SELECT rechazo_desc
            INTO   arr_1[i].rechazo_desc
            FROM   tab_rch_marca
            WHERE  rechazo_cod = arr_1[i].rechazo_cod
        END IF

        IF arr_1[i].fecha_confirma = "01010001" THEN
            LET arr_1[i].fecha_confirma = ""
        END IF

        IF arr_1[i].fecha_modifica = "01010001" THEN
            LET arr_1[i].fecha_modifica = ""
        END IF

        IF arr_1[i].fecha_envio = "01010001" THEN
            LET arr_1[i].fecha_envio = ""
        END IF

        SELECT descripcion
        INTO   arr_1[i].desc_grupo
        FROM   tab_grupo
        WHERE  grupo = arr_1[i].grupo

        SELECT fecha_conversion
        INTO   arr_1[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss              = arr_1[i].nss
        AND    consecutivo_lote = arr_1[i].consecutivo
        AND    tipo_movimiento  = varmov
        GROUP BY 1

        LET i = i + 1
    END FOREACH

    LET cont_reg = i-1

    ERROR ""
    DISPLAY "  REGISTROS CARGADOS  ",cont_reg ,"" AT 22,51
    ATTRIBUTE(REVERSE)

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW retm8056
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_1 TO scr_1.*
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
         LET v_ejecuta = "fglgo RETM810 ",arr_1[arr_c].nss CLIPPED," ",
                         arr_1[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta
        ON KEY (CONTROL-M)
             LET pos = ARR_CURR()

            {IF arr_1[pos].usuario_captura = usuario THEN
               ERROR "    NO SE PUEDE CONFIRMAR CON EL MISMO USUARIO DE CAPTURA"
               ATTRIBUTE(NORMAL)
               CALL inicializa()
               LET int_flag = TRUE
            END IF }

            EXIT DISPLAY
        ------------------------------------------------

    END DISPLAY

    IF int_flag = TRUE THEN
    LET int_flag = FALSE
        CLOSE WINDOW retm8056
        RETURN
    ELSE
        CALL construccion(arr_1[pos].*,vmodif)
        CLOSE WINDOW retm8056
    END IF
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

FUNCTION marca_cuenta(vl_nss,vl_marca_ent,vl_consecutivo)
#mc------------------------------------------------------
    DEFINE #loc #smallint
     vl_marca_ent        ,
     vl_marca_res        ,
     vl_rechazo_cod      SMALLINT

    DEFINE #loc #char
         vl_nss              CHAR(011)

    DEFINE #loc #integer
     vl_consecutivo      INTEGER

    PREPARE eje_marca FROM v_marca
    DECLARE cur_sp CURSOR FOR eje_marca
    OPEN cur_sp USING vl_nss                , # nss
              vl_marca_ent          , # marca entrant
              vl_consecutivo        , # correlativo
              reg_10.estado_marca   , # estado_marca
                      reg_10.codigo_rechazo , # codigo rechazo
              reg_10.marca_causa    , # marca_causa
              reg_10.fecha_causa    , # fecha_causa
              usuario

    FETCH cur_sp INTO vl_marca_res   , # misma marca si convive o
                      vl_rechazo_cod   # marca activa que rechaza

    CLOSE cur_sp

    RETURN vl_marca_res,
           vl_rechazo_cod

END FUNCTION
