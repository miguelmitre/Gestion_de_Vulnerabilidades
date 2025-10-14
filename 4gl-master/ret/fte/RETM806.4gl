#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM806  => MANTENEDOR DE REGISTRO DE CERTIFICADO IMSS - RETIRO POR   #
#                     EDAD DE TRABAJADOR SAR-92 (TIPO RETIRO H)                 #
#                                  VERSION COPPEL                               #
#Fecha creacion    => 30 DE ENERO DE 2004                                       #
#By                => JOSE LUIS SALDIVAR CARDOSO                                #
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
    DEFINE  ga_solicitud   ARRAY[5000] OF RECORD #glo #ga_solicitud
        nss                LIKE ret_det_datamart.nss                ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc              ,
        curp               LIKE ret_solicitud_tx.curp               ,
        paterno            LIKE ret_det_datamart.paterno_afore      ,
        materno            LIKE ret_det_datamart.materno_afore      ,
        nombres            LIKE ret_det_datamart.nombre_afore       ,
        tipo_retiro        CHAR(01)                                 ,
        descripcion        CHAR(60)                                 ,
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion    ,
        desc_prestacion    LIKE tab_prestacion.descripcion          ,
        regimen            LIKE ret_det_datamart.regimen            ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro        ,
        desc_seguro        LIKE tab_seguro.descripcion              ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension       ,
        desc_pension       LIKE tab_pension.descripcion             ,
        fecha_nacimiento   LIKE ret_solicitud_tx.fecha_nacimiento   ,
        cve_doc_probatorio LIKE ret_solicitud_tx.cve_doc_probatorio ,
        cve_doc_desc       CHAR(30)                                 ,
        folio_solicitud    INTEGER                                  ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud    ,
        semanas_cotizadas  SMALLINT                                 ,
        --
        --entidad            LIKE ret_solicitud_tx.entidad            ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent    ,
        --
        referencia         SMALLINT                                 ,
        fecha_captura      DATE                                     ,
        fecha_modifica     DATE                                     ,
        fecha_confirma     DATE                                     ,
        fecha_liquida      DATE                                     ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud   ,
        descripcion_status CHAR(40)                                 ,
        folio              INTEGER                                  ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo        ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura    ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica   ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma   ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro      ,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv     ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio
    END RECORD

    DEFINE  rg_datamart    RECORD #glo #rg_input
        nss                LIKE ret_det_datamart.nss                ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc              ,
        curp               LIKE ret_solicitud_tx.curp               ,
        paterno            LIKE ret_det_datamart.paterno_afore      ,
        materno            LIKE ret_det_datamart.materno_afore      ,
        nombres            LIKE ret_det_datamart.nombre_afore       ,
        tipo_retiro        CHAR(01)                                 ,
        descripcion        CHAR(60)                                 ,
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion    ,
        desc_prestacion    LIKE tab_prestacion.descripcion          ,
        regimen            LIKE ret_det_datamart.regimen            ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro        ,
        desc_seguro        LIKE tab_seguro.descripcion              ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension       ,
        desc_pension       LIKE tab_pension.descripcion             ,
        fecha_nacimiento   LIKE ret_solicitud_tx.fecha_nacimiento   ,
        cve_doc_probatorio LIKE ret_solicitud_tx.cve_doc_probatorio ,
        cve_doc_desc       CHAR(30)                                 ,
        folio_solicitud    INTEGER                                  ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud    ,
        semanas_cotizadas  SMALLINT                                 ,
        --
        --entidad            LIKE ret_solicitud_tx.entidad            ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent    ,
        --
        referencia         SMALLINT                                 ,
        fecha_captura      DATE                                     ,
        fecha_modifica     DATE                                     ,
        fecha_confirma     DATE                                     ,
        fecha_liquida      DATE                                     ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud   ,
        descripcion_status CHAR(40)                                 ,
        folio              INTEGER                                  ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo        ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura    ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica   ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma   ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro      ,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv     ,
        fecha_envio        LIKE ret_solicitud_tx.fecha_envio
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        precapturado       LIKE ret_estado.estado_solicitud ,
        capturado          LIKE ret_estado.estado_solicitud ,
        rechazado          LIKE ret_estado.estado_solicitud ,
        confirmado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE  #glo #date
        HOY                 ,
        fecha_nac           ,
        xx_fecha_solicitud  ,
        vfecha_causa        ,
        vfecha_ini_p        ,
        vfecha_resolucion   ,
        fecha_max_habil     DATE

    DEFINE  #glo #char
        txt_1             CHAR(2200) ,
        txt_2             CHAR(3000) ,
        txt_3             CHAR(2200) ,
        txt_4             CHAR(2200) ,
        txt_5             CHAR(2200) ,
        x_busca           CHAR(2200) ,
        s_codigo_afore    CHAR(0004) ,
        usuario           CHAR(0012) ,
        option_afore      CHAR(0006) ,
        v_marca           CHAR(0100) ,
        v_desmarca        CHAR(0100) ,
        vaccion           CHAR(0001) ,
        xaccion           CHAR(0001) ,
        vnss              CHAR(0011) ,
        vregimen          CHAR(0002) ,
        vtipo_seguro      CHAR(0002) ,
        vtipo_pension     CHAR(0002) ,
        vtipo_retiro      CHAR(0001) ,
        ejecuta           CHAR(0500) ,
        enter                        ,
        opc               CHAR(0001) ,
        ok_datamart       CHAR(0007) ,
        nfena             CHAR(0010) ,
        c_fecha_nac       CHAR(0010) ,
        dEsc_status_rech  CHAR(0020)

    DEFINE #glo  #smallint
        edad               ,
        anyo_1             ,
        anyo_2             ,
        mes_1              ,
        mes_2              ,
        marca_ent          ,
        i                  ,
        pos                ,
        codigo             ,
        v_tipo_movimiento  ,
        entidad            ,
        v_marca_res        ,
        v_marca_ent        ,
        v_cod_rechazo      ,
        vestado_marca      ,
        vcodigo_rechazo    ,
        vmarca_causa       ,
        vtipo_prestacion   ,
        estado_convivencia ,
        s_tipo_movimiento  ,
        sw_2               SMALLINT

    DEFINE #glo #integer
        ult_consecutivo   INTEGER

    DEFINE  
        x_error       CHAR(500),   ---omar
        arr_c          SMALLINT,
        x_usuario          CHAR(12),
        x_estado_solicitud CHAR(40),
        v_ejecuta          CHAR(500) ,
        sw                 SMALLINT
    DEFINE varmov  like tab_retiro.movimiento
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM806.log")    ---omar
    CALL init()
    LET xaccion = " "
    OPEN WINDOW retm8061 AT 2,3 WITH FORM "RETM8061" ATTRIBUTE(BORDER)
    DISPLAY " RETM806                   DATOS DEL AFILIADO                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)

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
                       SHOW OPTION "Agrega","Consulta","Modifica",
                   "Elimina","Salida"
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
                   EXIT PROGRAM
           END MENU
    END CASE
    CLOSE WINDOW retm8061
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

    OPEN WINDOW retm8062 AT 2,3 WITH FORM "RETM8062" ATTRIBUTE (BORDER)
    DISPLAY " RETM806                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " AGREGA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega    Ctrl-C : Salir   Ctrl-B : Beneficiario" AT 2,1

    INPUT BY NAME rg_datamart.nss,
          rg_datamart.folio_solicitud,
          rg_datamart.fecha_solicitud WITHOUT DEFAULTS
        BEFORE FIELD  nss
        IF vaccion = 'C' OR
           vaccion = 'M' THEN

                LET rg_datamart.nss              = vnss
                LET rg_datamart.regimen          = vregimen
                LET rg_datamart.tipo_prestacion  = vtipo_prestacion
                LET rg_datamart.tipo_seguro      = vtipo_seguro
                LET rg_datamart.tipo_pension     = vtipo_pension
                LET rg_datamart.tipo_retiro      = vtipo_retiro

                DISPLAY rg_datamart.nss              TO nss
                DISPLAY rg_datamart.regimen          TO regimen
                DISPLAY rg_datamart.tipo_prestacion  TO tipo_prestacion
                DISPLAY rg_datamart.tipo_seguro      TO tipo_seguro
                DISPLAY rg_datamart.tipo_pension     TO tipo_pension
                DISPLAY rg_datamart.tipo_retiro      TO tipo_retiro
            DISPLAY rg_datamart.fecha_solicitud  TO fecha_solicitud

                SELECT n_rfc     ,
                       n_unico    ,
                       paterno    ,
                       materno    ,
                       nombres
                INTO  rg_datamart.n_rfc   ,
                      rg_datamart.curp    ,
                      rg_datamart.paterno ,
                      rg_datamart.materno ,
                      rg_datamart.nombres
                FROM  afi_mae_afiliado
                WHERE n_seguro = rg_datamart.nss

                SELECT A.descripcion
                INTO   rg_datamart.descripcion_status
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

                DISPLAY BY NAME rg_datamart.materno
                DISPLAY BY NAME rg_datamart.paterno
                DISPLAY BY NAME rg_datamart.nombres
                DISPLAY BY NAME rg_datamart.n_rfc
                DISPLAY BY NAME rg_datamart.curp
                DISPLAY BY NAME rg_datamart.descripcion_status
                DISPLAY rg_datamart.desc_seguro  TO desc_seguro
                DISPLAY rg_datamart.desc_pension TO desc_pension
                DISPLAY rg_datamart.descripcion  TO descripcion
                DISPLAY BY NAME rg_datamart.desc_prestacion
            END IF

            IF sw_1 = 0   THEN
                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   ret_consecutivo

                INSERT INTO ret_consecutivo VALUES (ult_consecutivo)

                LET rg_datamart.estado_solicitud   = reg_1.capturado
                LET rg_datamart.tipo_retiro        = 'H'
                LET rg_datamart.descripcion        = cat_tipo_ret()
                LET rg_datamart.tipo_prestacion    = 9
                LET rg_datamart.desc_prestacion    = cat_tipo_prestacion()
                LET rg_datamart.tipo_seguro        = 'IV'
                LET rg_datamart.desc_seguro        = cat_tipo_seguro()
                LET rg_datamart.tipo_pension       = 'VE'
                LET rg_datamart.desc_pension       = cat_tipo_pension()
                LET rg_datamart.fecha_captura      = TODAY
                LET rg_datamart.usuario_captura    = usuario
                LET rg_datamart.consecutivo        = ult_consecutivo
                LET rg_datamart.regimen            = '73'
                LET rg_datamart.fecha_solicitud    = HOY

                DISPLAY rg_datamart.fecha_solicitud TO fecha_solicitud

                SELECT A.descripcion
                INTO   rg_datamart.descripcion_status
                FROM   ret_estado A
                WHERE  A.estado_solicitud = rg_datamart.estado_solicitud

                LET sw_1 = 1
            END IF

        AFTER FIELD nss
            IF rg_datamart.nss IS NULL THEN
                ERROR " EL NSS NO PUDE SER NULO "
                SLEEP 2
                NEXT FIELD nss
        ELSE
            LET vestado_solicitud  = " "

                SELECT MAX(consecutivo)
            INTO   vconsecutivo
            FROM   ret_solicitud_tx
            WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "H"
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion

            SELECT estado_solicitud
            INTO   vestado_solicitud
            FROM   ret_solicitud_tx
            WHERE  nss              = rg_datamart.nss
                AND    regimen          = 97
                AND    tipo_retiro      = "H"
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

            SELECT n_rfc     ,
                  n_unico    ,
                  fena       ,
                  paterno    ,
                  materno    ,
                  nombres
            INTO  rg_datamart.n_rfc ,
                  rg_datamart.curp  ,
                  rg_datamart.fecha_nacimiento,
                  rg_datamart.paterno  ,
                  rg_datamart.materno,
                  rg_datamart.nombres
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
            AND    tipo_prestacion = 9
            AND    fecha_captura   = TODAY
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND THEN
                ERROR " YA EXISTE AL DIA DE HOY UNA SOLICITUD "
                ATTRIBUTE(REVERSE) SLEEP 2
                ERROR ""
                NEXT FIELD nss
            END IF

            DISPLAY BY NAME rg_datamart.paterno
            DISPLAY BY NAME rg_datamart.materno
            DISPLAY BY NAME rg_datamart.nombres
            DISPLAY BY NAME rg_datamart.n_rfc
            DISPLAY BY NAME rg_datamart.curp
            DISPLAY BY NAME rg_datamart.fecha_nacimiento
            DISPLAY BY NAME rg_datamart.estado_solicitud
            DISPLAY BY NAME rg_datamart.descripcion_status

            DISPLAY rg_datamart.tipo_retiro TO tipo_retiro
            DISPLAY rg_datamart.descripcion TO descripcion
            DISPLAY rg_datamart.tipo_prestacion TO tipo_prestacion
            DISPLAY rg_datamart.desc_prestacion TO desc_prestacion
            DISPLAY rg_datamart.tipo_seguro TO tipo_seguro
            DISPLAY rg_datamart.desc_seguro TO desc_seguro
            DISPLAY rg_datamart.tipo_pension TO tipo_pension
            DISPLAY rg_datamart.desc_pension TO desc_pension
            DISPLAY rg_datamart.regimen TO regimen
            DISPLAY rg_datamart.fecha_captura  TO fecha_captura
            DISPLAY rg_datamart.usuario_captura TO usuario_captura
            DISPLAY rg_datamart.consecutivo TO consecutivo

{
         AFTER FIELD fecha_nacimiento
            IF rg_datamart.fecha_nacimiento IS NULL THEN
                ERROR " EL CAMPO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                SLEEP 2
                NEXT FIELD fecha_nacimiento
            END IF

            LET anyo_2    = YEAR(TODAY)
            LET anyo_1    = YEAR(fecha_nac)
            LET mes_2     = MONTH(TODAY)
            LET mes_1     = MONTH(fecha_nac)

            IF mes_2 > mes_1 THEN
                LET edad = anyo_2 - anyo_1
            ELSE
                LET edad = anyo_2 - anyo_1 - 1
            END IF
}            

            LET fecha_nac   = rg_datamart.fecha_nacimiento
            LET edad        = f_obten_edad(fecha_nac)
            
            
{svera
            IF edad < 65 THEN
                ERROR "    SOLICITUD IMPROCEDENTE,",
                      "EDAD MINIMA 65,SU EDAD ES:",
                      edad ATTRIBUTE(NORMAL)
               -- PROMPT" DESEA CAPTURAR [S/N]" FOR CHAR enter
               -- ERROR ""
              --  IF enter MATCHES "[Ss]" THEN
              --  ELSE
                    CALL inicializa()
                    EXIT INPUT
              --  END IF
            END IF
svera}
            LET rg_datamart.cve_doc_probatorio = 1
        DISPLAY BY NAME rg_datamart.cve_doc_probatorio

            LET rg_datamart.cve_doc_desc = "ACT NAC"
        DISPLAY BY NAME rg_datamart.cve_doc_desc
{
            NEXT FIELD cve_doc_probatorio


         AFTER FIELD cve_doc_probatorio
             IF rg_datamart.cve_doc_probatorio IS NULL THEN
                 ERROR " EL DOCUMENTO PROBATORIO  NO PUDE SER NULO "
                 NEXT FIELD  cve_doc_probatorio
             END IF
}
         AFTER FIELD fecha_solicitud
             IF rg_datamart.fecha_solicitud IS NULL THEN
                 ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
                 ATTRIBUTE(REVERSE)
                 SLEEP 2
                 NEXT FIELD  fecha_solicitud
             END IF

             IF rg_datamart.fecha_solicitud <= HOY THEN
             ELSE
                 ERROR "LA FECHA DE SOLICITUD NO PUEDE SER MAYOR A LA DEL ",
                       "DIA" ATTRIBUTE(REVERSE) SLEEP 2
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
{
             CALL habil_siguiente(rg_datamart.fecha_solicitud,3)
             RETURNING fecha_max_habil
             IF fecha_max_habil < TODAY THEN
                 ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
                 SLEEP 2
                 EXIT INPUT
             END IF
}

    ON KEY ( ESC )

            IF rg_datamart.nss IS NULL THEN
                ERROR " EL NSS NO PUDE SER NULO "
                SLEEP 2
                NEXT FIELD nss
            ELSE
            LET vestado_solicitud  = " "

                SELECT MAX(consecutivo)
            INTO   vconsecutivo
            FROM   ret_solicitud_tx
            WHERE  nss              = rg_datamart.nss
                AND    tipo_retiro      = "H"
                AND    tipo_seguro      = rg_datamart.tipo_seguro
                AND    tipo_pension     = rg_datamart.tipo_pension
                AND    tipo_prestacion  = rg_datamart.tipo_prestacion

            SELECT estado_solicitud
            INTO   vestado_solicitud
            FROM   ret_solicitud_tx
            WHERE  nss              = rg_datamart.nss
                AND    regimen          = 97
                AND    tipo_retiro      = "H"
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

            IF rg_datamart.fecha_nacimiento IS NULL THEN
                ERROR " EL CAMPO NO PUEDE SER NULO "
                SLEEP 2
                NEXT FIELD fecha_nacimiento
            END IF

            LET fecha_nac   = rg_datamart.fecha_nacimiento
            LET edad        = f_obten_edad(fecha_nac)

            IF edad < 65 THEN
                ERROR "    SOLICITUD IMPROCEDENTE,",
                      "EDAD MINIMA 65,SU EDAD ES:",
                      edad ATTRIBUTE(NORMAL)
                PROMPT" DESEA CAPTURAR [S/N]" FOR CHAR enter
                ERROR ""
                IF enter MATCHES "[Ss]" THEN
                ELSE
                    CALL inicializa()
                    EXIT INPUT
                END IF
            END IF

             IF rg_datamart.cve_doc_probatorio IS NULL THEN
                 ERROR " EL DOCUMENTO A PROBATORIO  NO PUDE SER NULO "
                 NEXT FIELD  cve_doc_probatorio
             END IF

            IF rg_datamart.fecha_solicitud IS NULL THEN
                ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
                SLEEP 2
                NEXT FIELD  fecha_solicitud
            END IF

            IF rg_datamart.fecha_solicitud <= HOY THEN
            ELSE
                ERROR "LA FECHA DE SOLICITUD NO PUEDE SER MAYOR A LA DEL ",
                      "DIA" ATTRIBUTE(REVERSE) SLEEP 2
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
{
            CALL habil_siguiente(rg_datamart.fecha_solicitud,3)
            RETURNING fecha_max_habil
            IF fecha_max_habil < TODAY THEN
                ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
                SLEEP 2
                EXIT INPUT
            END IF
}
            --INICIO PROCESO DE MARCAJE DE CUENTAS------------------------------

            SELECT movimiento
            INTO   s_tipo_movimiento
            FROM   tab_retiro
            WHERE  tipo_retiro = 'H'

        LET v_cod_rechazo          = 0
            LET rg_datamart.referencia = 0

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
                WHERE  rechazo_cod = v_cod_rechazo
                CURRENT WINDOW IS retm8062

                PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                       desc_status_rech,") <ENTER> CONTINUAR" FOR CHAR enter

            END IF
            --FIN MARCAJE DE CUENTAS--------------------------------------------
            WHENEVER ERROR CONTINUE

            INSERT INTO ret_solicitud_tx
                VALUES(rg_datamart.nss               ,
                       rg_datamart.consecutivo       ,
                       rg_datamart.folio             ,
                       rg_datamart.folio_solicitud   ,
                       "S"                           ,
                       rg_datamart.curp              ,
                       ""                            ,
                       0                             ,
                       rg_datamart.tipo_retiro       ,
                       rg_datamart.regimen           ,
                       rg_datamart.tipo_seguro       ,
                       rg_datamart.tipo_pension      ,
                       rg_datamart.tipo_prestacion   ,
                       " "                           , #fecha_ini_pension
                       " "                           , #fecha_resolucion
                       rg_datamart.fecha_solicitud   ,
                       rg_datamart.cve_doc_probatorio, #cve_doc_probatorio
                       rg_datamart.fecha_nacimiento  , #fecha_nacimiento
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
                       " "                           , #diag_datamart
                       " "                           , #estado_sub_viv
                       rg_datamart.semanas_cotizadas ,
                       reg_1.capturado               , #estado_solicitud
                       " "                           , #entidad
                       " "                           , #cod_rechazo_ent
                       rg_datamart.referencia        , #cod_rechazo
                       HOY                           , #fecha_captura
                       " "                           , #fecha_confirma
                       " "                           , #fecha_modifica
                       " "                           , #fecha_envio
                       usuario                       , #usuario_captura
                       " "                           , #usuario_confirma
                       " "                           , #usuario_modifica
                       0                             , #carta
                       2                             , #grupo
                       'T'                           , #cve_destino
                       ' '                           , #porcentaje
                       0                             , #num_resolucion
                       ""                            , #paterno sol
                       ""                            , #materno sol
                       ""                              #nombre sol
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

            ERROR "SOLICITUD INGRESADA.... " ATTRIBUTE(REVERSE)
            SLEEP 2
            ERROR ""

            INITIALIZE vaccion           TO NULL
        INITIALIZE vnss              TO NULL
        INITIALIZE vtipo_prestacion  TO NULL
        INITIALIZE vtipo_retiro      TO NULL

            CALL inicializa()
            LET sw_1 = 0

            NEXT FIELD nss

            EXIT INPUT

        ON KEY ( CONTROL-C )
                EXIT INPUT

        ON KEY ( INTERRUPT )
                EXIT INPUT
    END INPUT

  CLOSE WINDOW retm8062
END FUNCTION

FUNCTION consulta()
#c-----------------
    OPEN WINDOW retm8063 AT 2,3 WITH FORM "RETM8063" ATTRIBUTE (BORDER)
    DISPLAY " RETM806                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Consulta   Ctrl-C : Salir   Ctrl-B : Beneficiario" AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON A.nss               ,
                                 B.n_rfc             ,
                                 A.curp              ,
                                 B.paterno     ,
                                 B.materno     ,
                                 B.nombres      ,
                                 A.fecha_captura     ,
                                 A.fecha_confirma    ,
                                 A.estado_solicitud  ,
                                 A.folio             ,
                                 A.consecutivo
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
       CLOSE WINDOW retm8063
       RETURN
    END IF

    LET   txt_2 =" SELECT A.nss                       , ",
                        " B.n_rfc                     , ", #n_rfc
                        " A.curp                      , ", #n_unico
                        " B.paterno                   , ", #paterno
                        " B.materno                   , ", #materno
                        " B.nombres                   , ", #nombres
                        " A.tipo_retiro               , ", #tipo_retiro
                        " ' '                         , ", #des_tipo_ret
                        " A.tipo_prestacion           , ",
                        " ' '                         , ", #desc_prestacion
                        " A.regimen                   , ",
                        " A.tipo_seguro               , ",
                        " ' '                         , ", #desc_seguro
                        " A.tipo_pension              , ",
                        " ' '                         , ", #desc_pension
                        " A.fecha_nacimiento          , ", #fecha_nacimiento
                        " A.cve_doc_probatorio        , ", #cve_doc_probatorio
                        " 'ACT NAC'                   , ",
                        " A.folio_solicitud           , ", #folio_solicitud
                        " A.fecha_solicitud           , ", #fecha_solicitud
                        " A.semanas_cotizadas         , ",
                        " A.cod_rechazo_ent           , ", #cod_rechazo_ent
                        " A.rechazo_cod               , ", #cod_rechazo
                        " A.fecha_captura             , ", #fecha_captura
                        " A.fecha_modifica            , ", #fecha_modifica
                        " A.fecha_confirma            , ", #fecha_confirma
                        " ' '                         , ", #fecha_liquida
                        " A.estado_solicitud          , ", #estado_solicitud
                        " ' '                         , ", #descripcion_status
                        " A.folio                     , ", #folio
                        " A.consecutivo               , ", #consecutivo
                        " A.usuario_captura           , ", #usuario_captura
                        " A.usuario_modifica          , ", #usuario_modifica
                        " A.usuario_confirma          , ", #usuario_confirma
                        " A.diag_registro             , ",
                        " D.estado_sub_viv            , ",
                        " A.fecha_envio                 ", #fecha_envio
                  " FROM  ret_solicitud_tx A,",
                         "afi_mae_afiliado B,",
                       "ret_matriz_derecho C,",
                      "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_prestacion  = 9        " ,
                      " AND   A.tipo_retiro     = 'H'                ",
                      " AND   A.tipo_retiro     = C.tipo_retiro      ",
                      " AND   A.tipo_seguro     = C.tipo_seguro      ",
                      " AND   A.tipo_pension    = C.tipo_pension     ",
                      " AND   A.nss         = D.nss  ",
                      " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_2 FROM txt_2
    DECLARE cur_2 CURSOR FOR pre_2
    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_2 INTO ga_solicitud[i].*
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

        SELECT n_rfc     ,
               paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].n_rfc,
               ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss      = ga_solicitud[i].nss
        AND    consecutivo_lote = ga_solicitud[i].consecutivo
        AND    tipo_movimiento  = varmov
    GROUP BY 1


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

        SELECT rechazo_cod
        INTO   ga_solicitud[i].referencia
        FROM   ret_solicitud_tx
        WHERE  nss         = ga_solicitud[i].nss
        AND    consecutivo = ga_solicitud[i].consecutivo

        LET i = i + 1

     END FOREACH

     IF i = 1 THEN
         INITIALIZE rg_datamart.* TO NULL
         CLEAR FORM
         ERROR "    NO EXISTE REGISTRO "
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8063
         RETURN
     END IF

     CALL SET_COUNT(i-1)

     ERROR ""
     DISPLAY ARRAY ga_solicitud TO scr_1.*

         ON KEY ( CONTROL-B )

             LET pos = ARR_CURR()
             SELECT "OK"
             FROM   ret_beneficiario
             WHERE  nss         =  ga_solicitud[pos].nss
             AND    consecutivo =  ga_solicitud[pos].consecutivo
         GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                 LET ejecuta="fglgo RETM810 ",ga_solicitud[pos].nss," ",
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
     CLOSE WINDOW retm8063
END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8064 AT 2,3 WITH FORM "RETM8063" ATTRIBUTE (BORDER)
    DISPLAY " RETM806                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " MODIFICA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Modifica   Ctrl-C : Salir  Ctrl-B : Beneficiario" AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 1 ## modifica

    IF vaccion = "M" THEN

        LET   txt_3 =" SELECT A.nss                       ,",
                     "        B.n_rfc                     ,", #n_rfc
                     "        A.curp                      ,", #n_unico
                     "        B.paterno                   ,", #paterno
                     "        B.materno                   ,", #materno
                     "        B.nombres                   ,", #nombres
                     "        A.tipo_retiro               ,", #tipo_retiro
                     "        ' '                         ,", #des_tipo_ret
                     "        A.tipo_prestacion           ,",
                     "        ' '                         ,", #desc_prestacion
                     "        A.regimen                   ,",
                     "        A.tipo_seguro               ,",
                     "        ' '                         ,", #desc_seguro
                     "        A.tipo_pension              ,",
                     "        ' '                         ,", #desc_pension
                     "        A.fecha_nacimiento          ,", #fecha_nacimiento
                     "        A.cve_doc_probatorio        ,", #cve_doc_probatorio
                     "        'ACT NAC'                   ,",
                     "        A.folio_solicitud           ,", #folio_solicitud
                     "        A.fecha_solicitud           ,", #fecha_solicitud
                     "        A.semanas_cotizadas         ,",
                     "        A.cod_rechazo_ent           ,", #cod_rechazo_ent
                     "        A.rechazo_cod               ,", #cod_rechazo
                     "        A.fecha_captura             ,", #fecha_captura
                     "        A.fecha_modifica            ,", #fecha_modifica
                     "        A.fecha_confirma            ,", #fecha_confirma
                     "        ' '                         ,", #fecha_liquida
                     "        A.estado_solicitud          ,", #estado_solicitud
                     "        ' '                         ,", #descripcion_status
                     "        A.folio                     ,", #folio
                     "        A.consecutivo               ,", #consecutivo
                     "        A.usuario_captura           ,", #usuario_captura
                     "        A.usuario_modifica          ,", #usuario_modifica
                     "        A.usuario_confirma          ,", #usuario_confirma
                     "        A.diag_registro             ,",
                     "        D.estado_sub_viv            ,",
                     "        A.fecha_envio                ", #fecha_envia
                     " FROM   ret_solicitud_tx A          ,",
                     "        afi_mae_afiliado B          ,",
                     "        ret_matriz_derecho C        ,",
                     " OUTER  ret_monto_viv D           ",
                     " WHERE  A.nss = ",vnss,
                     " AND    A.nss             = B.n_seguro     ",
                     " AND    A.tipo_prestacion = 9              ",
                     " AND    A.tipo_retiro     = 'H'            ",
                     " AND    A.tipo_retiro     = C.tipo_retiro  ",
                     " AND    A.regimen         = C.regimen      ",
                     " AND    A.tipo_seguro     = C.tipo_seguro  ",
                     " AND    A.tipo_pension    = C.tipo_pension ",
                     " AND    A.nss             = D.nss          ",
                     " AND    A.consecutivo     = D.consecutivo  "

    ELSE
        CONSTRUCT BY NAME x_busca ON A.nss               ,
                                     B.n_rfc             ,
                                     A.curp              ,
                                     B.paterno           ,
                                     B.materno           ,
                                     B.nombres           ,
                                     A.fecha_captura     ,
                                     A.fecha_confirma    ,
                                     A.folio             ,
                                     A.consecutivo

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
            CLOSE WINDOW retm8064
            RETURN
        END IF

        LET   txt_3 =" SELECT A.nss                       ,",
                     "        B.n_rfc                     ,", #n_rfc
                     "        A.curp                      ,", #n_unico
                     "        B.paterno                   ,", #paterno
                     "        B.materno                   ,", #materno
                     "        B.nombres                   ,", #nombres
                     "        A.tipo_retiro               ,", #tipo_retiro
                     "        ' '                         ,", #des_tipo_ret
                     "        A.tipo_prestacion           ,",
                     "        ' '                         ,", #desc_prestacion
                     "        A.regimen                   ,",
                     "        A.tipo_seguro               ,",
                     "        ' '                         ,", #desc_seguro
                     "        A.tipo_pension              ,",
                     "        ' '                         ,", #desc_pension
                     "        A.fecha_nacimiento          ,", #fecha_nacimiento
                     "        A.cve_doc_probatorio        ,", #cve_doc_probatorio
                     "        'ACT NAC'                   ,",
                     "        A.folio_solicitud           ,", #folio_solicitud
                     "        A.fecha_solicitud           ,", #fecha_solicitud
                     "        A.semanas_cotizadas         ,",
                     "        A.cod_rechazo_ent           ,", #cod_rechazo_ent
                     "        A.rechazo_cod               ,", #cod_rechazo
                     "        A.fecha_captura             ,", #fecha_captura
                     "        A.fecha_modifica            ,", #fecha_modifica
                     "        A.fecha_confirma            ,", #fecha_confirma
                     "        ' '                         ,", #fecha_liquida
                     "        A.estado_solicitud          ,", #estado_solicitud
                     "        ' '                         ,", #descripcion_status
                     "        A.folio                     ,", #folio
                     "        A.consecutivo               ,", #consecutivo
                     "        A.usuario_captura           ,", #usuario_captura
                     "        A.usuario_modifica          ,", #usuario_modifica
                     "        A.usuario_confirma          ,", #usuario_confirma
                     "        A.diag_registro             ,",
                     "        D.estado_sub_viv            ,",
                     "        A.fecha_envio                ", #fecha_envia
                     " FROM   ret_solicitud_tx A          ,",
                     "        afi_mae_afiliado B          ,",
                     "        ret_matriz_derecho C        ,",
                     " OUTER  ret_monto_viv D              ",
                     " WHERE  ",x_busca CLIPPED,
                     " AND    A.nss             = B.n_seguro     ",
                     " AND    A.tipo_prestacion = 9              ",
                     " AND    A.tipo_retiro     = 'H'            ",
                     " AND    A.tipo_retiro     = C.tipo_retiro  ",
                     " AND    A.regimen         = C.regimen      ",
                     " AND    A.tipo_seguro     = C.tipo_seguro  ",
                     " AND    A.tipo_pension    = C.tipo_pension ",
                     " AND    A.nss             = D.nss          ",
                     " AND    A.consecutivo     = D.consecutivo  "
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

        SELECT n_rfc     ,
               paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].n_rfc,
               ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss

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

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT rechazo_cod
        INTO   ga_solicitud[i].referencia
        FROM   ret_solicitud_tx
        WHERE  nss         = ga_solicitud[i].nss
        AND    consecutivo = ga_solicitud[i].consecutivo

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss      = ga_solicitud[i].nss
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

             ON KEY ( INTERRUPT )
                LET int_flag=TRUE
                EXIT DISPLAY

            ON KEY (CONTROL-B)
                LET      arr_c           =  arr_curr()
                LET v_ejecuta = "fglgo RETM810 ",
                ga_solicitud[arr_c].nss CLIPPED," ",
                                ga_solicitud[arr_c].consecutivo CLIPPED,
                " ",'C' CLIPPED
                RUN v_ejecuta
             ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                EXIT DISPLAY
         END DISPLAY
     ELSE
         ERROR "    NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8064
         RETURN
     END IF

     IF int_flag = TRUE THEN
     LET int_flag = FALSE
         CLOSE WINDOW retm8064
         RETURN
     END IF

     CALL construccion(ga_solicitud[pos].*,sw_2) #co

     CLOSE WINDOW retm8064
END FUNCTION

FUNCTION elimina()
#e-----------------
    DEFINE #loc #smallint
        arr            ,
        src            SMALLINT

    OPEN WINDOW retm8065 AT 2,3 WITH FORM "RETM8063" ATTRIBUTE (BORDER)
    DISPLAY " RETM806                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY " ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER> : Elimina        Ctrl-C : Salir " AT 2,1

    LET int_flag = FALSE

    CONSTRUCT BY NAME x_busca ON A.nss               ,
                                 B.n_rfc             ,
                                 A.curp              ,
                                 B.paterno     ,
                                 B.materno     ,
                                 B.nombres      ,
                                 A.fecha_captura     ,
                                 A.fecha_confirma    ,
                                 A.folio             ,
                                 A.consecutivo

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
       CLOSE WINDOW retm8065
       RETURN
    END IF

    LET   txt_4 =" SELECT A.nss                       , ",
                        " B.n_rfc                     , ", #n_rfc
                        " A.curp                      , ", #n_unico
                        " B.paterno                   , ", #paterno
                        " B.materno                   , ", #materno
                        " B.nombres                   , ", #nombres
                        " A.tipo_retiro               , ", #tipo_retiro
                        " ' '                         , ", #des_tipo_ret
                        " A.tipo_prestacion           , ",
                        " ' '                         , ", #desc_prestacion
                        " A.regimen                   , ",
                        " A.tipo_seguro               , ",
                        " ' '                         , ", #desc_seguro
                        " A.tipo_pension              , ",
                        " ' '                         , ", #desc_pension
                        " A.fecha_nacimiento          , ", #fecha_nacimiento
                        " A.cve_doc_probatorio        , ", #cve_doc_probatorio
                        " 'ACT NAC'                   , ",
                        " A.folio_solicitud           , ", #folio_solicitud
                        " A.fecha_solicitud           , ", #fecha_solicitud
                        " A.semanas_cotizadas         , ",
                        " A.cod_rechazo_ent           , ", #cod_rechazo_ent
                        " A.rechazo_cod               , ", #cod_rechazo
                        " A.fecha_captura             , ", #fecha_captura
                        " A.fecha_modifica            , ", #fecha_modifica
                        " A.fecha_confirma            , ", #fecha_confirma
                        " ' '                         , ", #fecha_liquida
                        " A.estado_solicitud          , ", #estado_solicitud
                        " ' '                         , ", #descripcion_status
                        " A.folio                     , ", #folio
                        " A.consecutivo               , ", #consecutivo
                        " A.usuario_captura           , ", #usuario_captura
                        " A.usuario_modifica          , ", #usuario_modifica
                        " A.usuario_confirma          , ", #usuario_confirma
                        " A.diag_registro             , ",
                        " D.estado_sub_viv            , ",
                        " A.fecha_envio                 ", #fecha_envia
                      " FROM  ret_solicitud_tx A,",
                             "afi_mae_afiliado B,",
                             "ret_matriz_derecho C,",
                      "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_prestacion  = 9        ",
                      " AND   A.tipo_retiro     = 'H'                ",
                      " AND   A.tipo_retiro     = C.tipo_retiro      ",
                      " AND   A.regimen         = C.regimen          ",
                      " AND   A.tipo_seguro     = C.tipo_seguro      ",
                      " AND   A.tipo_pension    = C.tipo_pension     ",
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

        SELECT n_rfc     ,
               paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].n_rfc,
               ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss

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

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT rechazo_cod
        INTO   ga_solicitud[i].referencia
        FROM   ret_solicitud_tx
        WHERE  nss         = ga_solicitud[i].nss
        AND    consecutivo = ga_solicitud[i].consecutivo

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss      = ga_solicitud[i].nss
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
                      LET ejecuta="fglgo RETM810 ",
                                   ga_solicitud[pos].nss," ",
                                   ga_solicitud[pos].consecutivo," ",
                                   "B"
                      RUN ejecuta
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
                      ERROR "REGISTRO CANCELADO " ATTRIBUTE(REVERSE)
                      SLEEP 2
                      CALL inicializa() #i
                      LET int_flag = TRUE
                      EXIT DISPLAY
                  END IF

                  LET arr = ARR_CURR()
                  LET src = SCR_LINE()

                  IF ( (ga_solicitud[arr].estado_solicitud <> reg_1.capturado) AND 
                       (ga_solicitud[arr].estado_solicitud <> reg_1.precapturado)  ) THEN
                  
                     ERROR " REGISTRO NO PUEDE SER ELIMINADO...YA QUE ESTA",
                           " ENVIADO O LIQUIDADO"
                     EXIT DISPLAY
                  END IF

          WHENEVER ERROR CONTINUE
                  DELETE FROM ret_beneficiario
                  WHERE  nss = ga_solicitud[arr].nss
                  AND  consecutivo = ga_solicitud[arr].consecutivo
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_beneficiario:",
                   "nss ",ga_solicitud[arr].nss,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

             PROMPT "  ERROR DELETE ret_beneficiario AVISE A SISTEMAS "
             FOR enter
             EXIT PROGRAM
                  END IF

                  DELETE FROM ret_solicitud_tx
                  WHERE  nss = ga_solicitud[arr].nss
                  AND  consecutivo = ga_solicitud[arr].consecutivo
---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "DELETE ret_solicitud_tx:",
                   "nss ",ga_solicitud[arr].nss,
                   "consecutivo ",ga_solicitud[arr].consecutivo,
                   err_get(SQLCA.SQLCODE)

                     CALL errorlog(x_error CLIPPED)

             PROMPT "  ERROR DELETE ret_beneficiario AVISE A SISTEMAS "
             FOR enter
             EXIT PROGRAM
                  END IF
                  WHENEVER ERROR STOP

                  ##### DESMARCAJE ##################################

                  SELECT movimiento
                  INTO   s_tipo_movimiento
                  FROM   tab_retiro
                  WHERE  tipo_retiro = 'H'

                  LET v_marca_ent  = s_tipo_movimiento

                  LET vestado_marca   = 40
                 LET vcodigo_rechazo = 0
                 LET vmarca_causa    = 0
                 LET vfecha_causa    = 0

                  LET v_desmarca   = " EXECUTE PROCEDURE desmarca_cuenta('",
                                     ga_solicitud[arr].nss,"',",
                                     v_marca_ent,",",
                                     ga_solicitud[arr].consecutivo,",",
                     vestado_marca,",",
                     vmarca_causa,",' ",
                     usuario,"')"
                  PREPARE eje_reversa_mar FROM v_desmarca
                  EXECUTE eje_reversa_mar

                  ####################################################

                  ERROR "REGISTRO ELIMINADO "
                  ATTRIBUTE(REVERSE)
                  EXIT DISPLAY
         END DISPLAY
     ELSE
         ERROR " NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         CLOSE WINDOW retm8065
         RETURN
     END IF

     IF int_flag = TRUE THEN
    LET int_flag = FALSE
         CLOSE WINDOW retm8065
         RETURN
     END IF
CLOSE WINDOW retm8065
END FUNCTION

FUNCTION confirma()
#cf----------------
    DEFINE  tipoprestacion     ,
            tiposeguro         ,
            tipopension        CHAR(02)

    OPEN WINDOW retm8066 AT 2,3 WITH FORM "RETM8063" ATTRIBUTE (BORDER)
    DISPLAY " RETM806                  DATOS DEL AFILIADO                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "CONFIRMA CAPTURA" AT 1,59 ATTRIBUTE(REVERSE)
    DISPLAY "       DATOS DE LA SOLICITUD POR EDAD DEL TRABAJADOR (TIPO RETIRO H)           " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                        DATOS DE CONTROL INTERNO                               " AT 14,1 ATTRIBUTE(REVERSE)
    DISPLAY " <ENTER> : Selecciona    Ctrl-C : Salir   Ctrl-B : Beneficiario" AT 2,1

    LET int_flag = FALSE
    LET sw_2 = 2  #confirma
    CLEAR FORM

    CONSTRUCT BY NAME x_busca ON A.nss               ,
                                 B.n_rfc             ,
                                 A.curp              ,
                                 B.paterno     ,
                                 B.materno     ,
                                 B.nombres      ,
                                 A.fecha_captura     ,
                                 A.fecha_confirma    ,
                                 A.folio             ,
                                 A.consecutivo

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
       CLOSE WINDOW retm8066
       RETURN
    END IF

    LET   txt_5 =" SELECT A.nss                       , ",
                        " B.n_rfc                     , ", #n_rfc
                        " A.curp                      , ", #n_unico
                        " B.paterno                   , ", #paterno
                        " B.materno                   , ", #materno
                        " B.nombres                   , ", #nombres
                        " A.tipo_retiro               , ", #tipo_retiro
                        " ' '                         , ", #des_tipo_ret
                        " A.tipo_prestacion           , ",
                        " ' '                         , ", #desc_prestacion
                        " A.regimen                   , ",
                        " A.tipo_seguro               , ",
                        " ' '                         , ", #desc_seguro
                        " A.tipo_pension              , ",
                        " ' '                         , ", #desc_pension
                        " A.fecha_nacimiento          , ", #fecha_nacimiento
                        " A.cve_doc_probatorio        , ", #cve_doc_probatorio
                        " 'ACT NAC'                   , ",
                        " A.folio_solicitud           , ", #folio_solicitud
                        " A.fecha_solicitud           , ", #fecha_solicitud
                        " A.semanas_cotizadas         , ",
                        " A.cod_rechazo_ent           , ", #cod_rechazo_ent
                        " A.rechazo_cod               , ", #cod_rechazo
                        " A.fecha_captura             , ", #fecha_captura
                        " A.fecha_modifica            , ", #fecha_modifica
                        " A.fecha_confirma            , ", #fecha_confirma
                        " ' '                         , ", #fecha_liquida
                        " A.estado_solicitud          , ", #estado_solicitud
                        " ' '                         , ", #descripcion_status
                        " A.folio                     , ", #folio
                        " A.consecutivo               , ", #consecutivo
                        " A.usuario_captura           , ", #usuario_captura
                        " A.usuario_modifica          , ", #usuario_modifica
                        " A.usuario_confirma          , ", #usuario_confirma
                        " A.diag_registro             , ",
                        " D.estado_sub_viv            , ",
                        " A.fecha_envio                 ", #fecha_envia
                        " FROM  ret_solicitud_tx A,",
                        "afi_mae_afiliado B,",
                        "ret_matriz_derecho C,",
                        "OUTER ret_monto_viv D ",
                      " WHERE ",x_busca CLIPPED,
                      " AND   A.estado_solicitud in (?,?) ",
                      " AND   A.nss = B.n_seguro  ",
                      " AND   A.tipo_prestacion  = 9        " ,
                      " AND   A.tipo_retiro     = 'H'                ",
                      " AND   A.tipo_retiro     = C.tipo_retiro      ",
                      " AND   A.regimen         = C.regimen          ",
                      " AND   A.tipo_seguro     = C.tipo_seguro      ",
                      " AND   A.tipo_pension    = C.tipo_pension     ",
                      " AND   A.nss         = D.nss  ",
                      " AND   A.consecutivo = D.consecutivo  "

    PREPARE pre_5 FROM txt_5
    DECLARE cur_5 CURSOR FOR pre_5
    LET i = 1
    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_5 USING reg_1.precapturado, 
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

        SELECT n_rfc     ,
               paterno   ,
               materno   ,
               nombres
        INTO   ga_solicitud[i].n_rfc,
               ga_solicitud[i].paterno,
               ga_solicitud[i].materno,
               ga_solicitud[i].nombres
        FROM   afi_mae_afiliado
        WHERE  n_seguro = ga_solicitud[i].nss

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

        SELECT A.descripcion
        INTO   ga_solicitud[i].descripcion_status
        FROM   ret_estado A
        WHERE  A.estado_solicitud = ga_solicitud[i].estado_solicitud

        SELECT rechazo_cod
        INTO   ga_solicitud[i].referencia
        FROM   ret_solicitud_tx
        WHERE  nss         = ga_solicitud[i].nss
        AND    consecutivo = ga_solicitud[i].consecutivo

        SELECT fecha_conversion
        INTO   ga_solicitud[i].fecha_liquida
        FROM   dis_cuenta
        WHERE  nss      = ga_solicitud[i].nss
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

             ON KEY ( INTERRUPT )
                LET int_flag=TRUE
                EXIT DISPLAY

            ON KEY (CONTROL-B)
               LET      arr_c           =  arr_curr()
               LET v_ejecuta = "fglgo RETM810 ",
                   ga_solicitud[arr_c].nss CLIPPED," ",
                         ga_solicitud[arr_c].consecutivo CLIPPED,
             " ",'C' CLIPPED
               RUN v_ejecuta
             ON KEY ( CONTROL-M )
                LET pos = ARR_CURR()
                EXIT DISPLAY
         END DISPLAY
     ELSE
         ERROR "    NO EXISTE REGISTRO"
         ATTRIBUTE(REVERSE)
         SLEEP 2
         ERROR ""
         CLOSE WINDOW retm8066
         RETURN
     END IF

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
         CLOSE WINDOW retm8066
         RETURN
     END IF

     CALL construccion(ga_solicitud[pos].*,sw_2) #co
     LET xaccion = " "
CLOSE WINDOW retm8066
END FUNCTION

FUNCTION construccion(reg_mod,sw_3)
#co--------------------------------
    DEFINE  reg_mod        RECORD #loc #reg_mod
        nss                LIKE ret_det_datamart.nss                ,
        n_rfc              LIKE afi_mae_afiliado.n_rfc              ,
        curp               LIKE ret_solicitud_tx.curp               ,
        paterno            LIKE ret_det_datamart.paterno_afore      ,
        materno            LIKE ret_det_datamart.materno_afore      ,
        nombres            LIKE ret_det_datamart.nombre_afore       ,
        tipo_retiro        CHAR(01)                                 ,
        descripcion        CHAR(60)                                 ,
        tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion    ,
        desc_prestacion    LIKE tab_prestacion.descripcion          ,
        regimen            LIKE ret_det_datamart.regimen            ,
        tipo_seguro        LIKE ret_det_datamart.tipo_seguro        ,
        desc_seguro        LIKE tab_seguro.descripcion              ,
        tipo_pension       LIKE ret_det_datamart.tipo_pension       ,
        desc_pension       LIKE tab_pension.descripcion             ,
        fecha_nacimiento   LIKE ret_solicitud_tx.fecha_nacimiento   ,
        cve_doc_probatorio LIKE ret_solicitud_tx.cve_doc_probatorio ,
        cve_doc_desc       CHAR(30),
        folio_solicitud          INTEGER                                  ,
        fecha_solicitud    LIKE ret_solicitud_tx.fecha_solicitud    ,
        semanas_cotizadas  SMALLINT                                 ,
        cod_rechazo_ent    LIKE ret_solicitud_tx.cod_rechazo_ent    ,
        referencia         SMALLINT                                 ,
        fecha_captura      DATE                                     ,
        fecha_modifica     DATE                                     ,
        fecha_confirma     DATE                                     ,
        fecha_liquida      DATE                                     ,
        estado_solicitud   LIKE ret_solicitud_tx.estado_solicitud   ,
        descripcion_status CHAR(40)                                 ,
        folio              INTEGER                                  ,
        consecutivo        LIKE ret_solicitud_tx.consecutivo        ,
        usuario_captura    LIKE ret_solicitud_tx.usuario_captura    ,
        usuario_modifica   LIKE ret_solicitud_tx.usuario_modifica   ,
        usuario_confirma   LIKE ret_solicitud_tx.usuario_confirma   ,
        diag_registro      LIKE ret_solicitud_rx.diag_registro      ,
        estado_sub_viv     LIKE ret_solicitud_rx.estado_sub_viv     ,
        fecha_envia        LIKE ret_solicitud_tx.fecha_envio
    END RECORD

    DEFINE #loc #smallint
        sw_3               SMALLINT

    IF sw_3  = 2  THEN
        DISPLAY " ESC : Confirma   Ctrl-C : Salir   Ctrl-B Beneficiarios Ctrl-F Rechazar " AT 2,1
    END IF

        INPUT BY NAME  reg_mod.fecha_nacimiento               ,
                       --reg_mod.cve_doc_probatorio             ,
                       reg_mod.folio_solicitud                      ,
                       reg_mod.fecha_solicitud WITHOUT DEFAULTS
                       --reg_mod.semanas_cotizadas  WITHOUT DEFAULTS

        BEFORE FIELD fecha_nacimiento
            DISPLAY reg_mod.referencia TO referencia

        AFTER FIELD fecha_nacimiento
            IF reg_mod.fecha_nacimiento IS NULL THEN
                ERROR " ESTE CAMPO NO PUEDE SER NULO.. " ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fecha_nacimiento
            END IF

            LET fecha_nac = reg_mod.fecha_nacimiento
            LET edad = f_obten_edad(fecha_nac)

            IF edad < 65 THEN
                ERROR "   SOLICITUD IMPROCEDENTE ,",
                      "EDAD MINIMA 65,SU EDAD ES:",
                      edad ATTRIBUTE(NORMAL)
                PROMPT" DESEA CAPTURAR [S/N]" FOR CHAR enter
                ERROR ""
                IF enter MATCHES "[Ss]" THEN
                    NEXT FIELD fecha_nacimiento
                ELSE
                    CALL inicializa()
                    LET sw_3 = 0
                    LET opc = "N"
                    EXIT INPUT
                END IF
            END IF
--            NEXT FIELD cve_doc_probatorio

        AFTER FIELD fecha_solicitud
            IF reg_mod.fecha_solicitud IS NULL THEN
                ERROR " LA FECHA SOLICITUD NO PUDE SER NULA "
                SLEEP 2
                NEXT FIELD  fecha_solicitud
            END IF

            IF reg_mod.fecha_solicitud > HOY THEN
                ERROR "LA FECHA DE SOLICITUD NO PUEDE SER MAYOR A LA DEL ",
                      "DIA" ATTRIBUTE(REVERSE) SLEEP 2
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
            ------------- xavier------------------
             ON KEY (CONTROL-F)
             IF xaccion = "F" THEN
                LET arr_c =  arr_curr()
                CALL  rechazar(reg_mod.nss,reg_mod.consecutivo,
                          reg_mod.tipo_prestacion,reg_mod.tipo_retiro)
                RETURNING  codigo,entidad
                LET sw_3 = 3
                LET reg_mod.cod_rechazo_ent = codigo
                DISPLAY BY NAME reg_mod.cod_rechazo_ent
              END IF
            --------------------------------------
{
            CALL habil_siguiente(reg_mod.fecha_solicitud,3)
            RETURNING fecha_max_habil

            IF fecha_max_habil < TODAY THEN
                ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
                SLEEP 2
                EXIT INPUT
            END IF
}


      ON KEY (CONTROL-B)
         LET v_ejecuta = "fglgo RETM810 ",reg_mod.nss CLIPPED," ",
                         reg_mod.consecutivo CLIPPED," ",'C' CLIPPED
         RUN v_ejecuta

        ON KEY ( ESC )

            IF reg_mod.fecha_nacimiento IS NULL THEN
                ERROR " ESTE CAMPO NO PUEDE SER NULO.. " ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fecha_nacimiento
            END IF

            LET fecha_nac = rg_datamart.fecha_nacimiento
            LET edad      = f_obten_edad(fecha_nac)

            IF edad < 65 THEN
                ERROR "   SOLICITUD IMPROCEDENTE ,",
                      "EDAD MINIMA 65,SU EDAD ES:",
                      edad ATTRIBUTE(NORMAL)
                PROMPT" DESEA CAPTURAR [S/N]" FOR CHAR enter
                ERROR ""
                IF enter MATCHES "[Ss]" THEN
                    NEXT FIELD fecha_nacimiento
                ELSE
                    CALL inicializa()
                    LET sw_3 = 0
                    LET opc = "N"
                    EXIT INPUT
                END IF
            END IF

            IF reg_mod.fecha_solicitud IS NULL THEN
                ERROR " LA FECHA SOLICITUD NO PUDE SER NULO "
                SLEEP 2
                NEXT FIELD  fecha_solicitud
            END IF

            IF reg_mod.fecha_solicitud <= HOY THEN
            ELSE
                ERROR "LA FECHA DE SOLICITUD NO PUEDE SER MAYOR A LA DEL ",
                      "DIA" ATTRIBUTE(REVERSE) SLEEP 2
                NEXT FIELD fecha_solicitud
            END IF
{
            CALL habil_siguiente(reg_mod.fecha_solicitud,3)
            RETURNING fecha_max_habil

            IF fecha_max_habil < TODAY THEN
                ERROR " LA FECHA DE SOLICITUD YA EXPIRO.. " ATTRIBUTE(REVERSE)
                SLEEP 2
                EXIT INPUT
            END IF
}
            SELECT rechazo_cod
            INTO   reg_mod.referencia
            FROM   ret_solicitud_tx
            WHERE  nss              = reg_mod.nss
            AND    consecutivo      = reg_mod.consecutivo
            AND    estado_solicitud IN (reg_1.capturado,reg_1.confirmado)
            AND    rechazo_cod      > 0

            IF STATUS <> NOTFOUND THEN
                SELECT A.rechazo_desc
                INTO   desc_status_rech
                FROM   tab_rch_marca A
                WHERE  A.rechazo_cod = reg_mod.referencia

                IF sw_3 = 1    THEN
                    CURRENT WINDOW IS retm8064
                ELSE
                    CURRENT WINDOW IS retm8066
                END IF

                LET v_cod_rechazo = reg_mod.referencia
                PROMPT " SOLICITUD RECHAZADA(",v_cod_rechazo," ",
                        desc_status_rech,") <ENTER> CONTINUAR" FOR CHAR enter
            END IF

            EXIT INPUT
        ON KEY ( CONTROL-C )
            EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT INPUT
    END INPUT

    IF sw_3 = 2    THEN
       CURRENT WINDOW IS retm8066

        IF reg_mod.estado_solicitud = reg_1.capturado THEN

            WHILE TRUE
               PROMPT " DESEA CONFIRMAR EL REGISTRO  S/N ? " FOR opc
               IF opc NOT MATCHES "[SsNn]" THEN
                  CONTINUE WHILE
               ELSE
                  EXIT WHILE
               END IF
            END WHILE

            IF opc MATCHES "[Ss]"  THEN
               UPDATE ret_solicitud_tx
               SET cve_doc_probatorio = reg_mod.cve_doc_probatorio,
                   --
                   cod_rechazo_ent    = reg_mod.cod_rechazo_ent,
                   --
                   fecha_nacimiento   = reg_mod.fecha_nacimiento,
                   folio_solicitud    = reg_mod.folio_solicitud,
                   fecha_solicitud    = reg_mod.fecha_solicitud,
                   semanas_cotizadas  = reg_mod.semanas_cotizadas,
                   estado_solicitud   = reg_1.confirmado,
                   fecha_confirma     = TODAY,
                   usuario_confirma   = usuario
               WHERE  nss         = reg_mod.nss
               AND    consecutivo = reg_mod.consecutivo
            
               ERROR  " REGISTRO CONFIRMADO "
               INITIALIZE reg_mod.* TO NULL
               CALL inicializa()
               RETURN
            ELSE
               ERROR " CONFIRMACION CANCELADA "
               INITIALIZE reg_mod.* TO NULL
               RETURN
            END IF
        ELSE
               ERROR " EL REGISTRO DEBE ENCONTRARSE EN ESTADO CAPTURADO "
               INITIALIZE reg_mod.* TO NULL
               RETURN            
        END IF
    END IF

    IF sw_3 = 1    THEN
       CURRENT WINDOW IS retm8064

       WHILE TRUE
          PROMPT " DESEAS ACTUALIZARLA   S/N ? " FOR opc
          IF opc NOT MATCHES "[SsNn]" THEN
             CONTINUE WHILE
          ELSE
             EXIT WHILE
          END IF
       END WHILE
       IF opc MATCHES "[Nn]"  THEN
          ERROR " REGISTRO CANCELADO "
          CALL inicializa() #i
          INITIALIZE reg_mod TO NULL
           RETURN
       ELSE

          WHENEVER ERROR CONTINUE
          UPDATE ret_solicitud_tx
          SET ret_solicitud_tx.fecha_nacimiento = reg_mod.fecha_nacimiento   ,
              ret_solicitud_tx.cod_rechazo_ent   = reg_mod.cod_rechazo_ent  ,
              ret_solicitud_tx.cve_doc_probatorio= reg_mod.cve_doc_probatorio ,
              ret_solicitud_tx.folio_solicitud   = reg_mod.folio_solicitud          ,
              ret_solicitud_tx.fecha_solicitud   = reg_mod.fecha_solicitud    ,
              ret_solicitud_tx.semanas_cotizadas = reg_mod.semanas_cotizadas  ,
              ret_solicitud_tx.fecha_modifica    = TODAY,
              ret_solicitud_tx.usuario_modifica  = usuario
          WHERE  ret_solicitud_tx.nss       = reg_mod.nss
          AND  ret_solicitud_tx.consecutivo       = reg_mod.consecutivo
           ---omar
          IF SQLCA.SQLCODE < 0 THEN
             LET x_error = "UPDATE ret_solicitud_tx:",
                 "nss ",reg_mod.nss,
                 "consecutivo ",reg_mod.consecutivo,
                 err_get(SQLCA.SQLCODE)

             CALL errorlog(x_error CLIPPED)

             PROMPT " REGISTRO CON PROBLEMAS DE ACTUALIZACION AVISE A SISTEMAS "
               FOR enter
             EXIT PROGRAM
          ELSE
             WHENEVER ERROR STOP

             ERROR " REGISTRO MODIFICADO "
             CALL inicializa() #i
             INITIALIZE reg_mod TO NULL
             RETURN
          END IF
       END IF

    ELSE
       CURRENT WINDOW IS retm8066
    END IF


    IF sw_3 = 3    THEN
       CURRENT WINDOW IS retm8066

       WHILE TRUE
          PROMPT " DESEAS RECHAZARLA     S/N ? " FOR opc
          IF opc NOT MATCHES "[SsNn]" THEN
             CONTINUE WHILE
          ELSE
             EXIT WHILE
          END IF
       END WHILE
    ELSE
       --CURRENT WINDOW IS retm8066
    END IF

    IF opc MATCHES "[Nn]"  THEN
       ERROR " REGISTRO CANCELADO "
       CALL inicializa() #i
       INITIALIZE reg_mod TO NULL
       RETURN
    END IF
    -----------------------------
    -- SE ACTUALIZA EL REGISTO --
    UPDATE ret_solicitud_tx
    SET ret_solicitud_tx.estado_solicitud  =
        reg_1.rechazado,
        ret_solicitud_tx.cod_rechazo_ent   =
        reg_mod.cod_rechazo_ent,
        ret_solicitud_tx.entidad =entidad
    WHERE ret_solicitud_tx.nss = reg_mod.nss
    AND ret_solicitud_tx.consecutivo = reg_mod.consecutivo
     ---------------------------------------------
     ---------- Se desmarca la cuenta -------------
     -- Se consulta tipo_movimento para retiro E --
     SELECT movimiento
     INTO v_tipo_movimiento
     FROM tab_retiro
     WHERE tipo_retiro = "H"
     LET vestado_marca   = 0
     LET vmarca_causa    = 0

     LET v_desmarca = "EXECUTE PROCEDURE desmarca_cuenta('",
         reg_mod.nss,"',",v_tipo_movimiento,",",
         reg_mod.consecutivo,",",vestado_marca,",",
         vmarca_causa,",' ",usuario,"')"
     PREPARE exec_desmarca FROM v_desmarca
     EXECUTE exec_desmarca
     ---------------------------------------
     DISPLAY " EL REGISTRO SE RECHAZO CORRECTAMENTE ","" AT 22,1
     ATTRIBUTE(REVERSE)
     SLEEP 3
     CALL inicializa() #i
    INITIALIZE reg_mod.* TO NULL
    RETURN
END FUNCTION

FUNCTION cat_tipo_ret()
#ctr-------------------
    DEFINE c_des_tipo_ret LIKE tab_retiro.descripcion

    SELECT descripcion
    INTO   c_des_tipo_ret
    FROM   tab_retiro
    WHERE  tipo_retiro = rg_datamart.tipo_retiro

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

         PREPARE pre_9 FROM x_x
         DECLARE cur_9 CURSOR FOR pre_9
         LET pos = 1
         FOREACH cur_9 INTO l_reg[pos].*
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

#---------------------------------------------------------------------------#
# f_obten_edad : Dada una fecha, calcula la edad del trabajador             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_edad(pdt_nacimiento)

    DEFINE 
        pdt_nacimiento      DATE

    DEFINE
       ldt_fecha            DATE

    DEFINE
       ls_anio_fin                 , 
       ls_anio_ini                 ,
       ls_edad                 ,
       ls_residuo              SMALLINT 

    -- -----------------------------------------------------------------------------

    IF pdt_nacimiento > HOY THEN
        ERROR " ERROR EN LA FECHAS DE RECEPCION Y AFILIACION "
    ELSE
        LET ls_anio_ini = YEAR(pdt_nacimiento)
        LET ls_anio_fin = YEAR(HOY)
        LET ls_edad     = ls_anio_fin - ls_anio_ini
    
        -- En caso de que el dia actual sea bisiesto se quita un año a la edad
        LET ldt_fecha   = MDY(MONTH(pdt_nacimiento), DAY(pdt_nacimiento), YEAR(HOY))
        LET ls_residuo  = YEAR(HOY) MOD 4
    
        IF (MONTH(pdt_nacimiento) = 2) AND (DAY(pdt_nacimiento) = 29) AND (ls_residuo <> 0) THEN
            LET ldt_fecha = MDY(MONTH(pdt_nacimiento), DAY(pdt_nacimiento-1), YEAR(HOY))
        END IF
    
        IF ldt_fecha > HOY THEN
            LET ls_edad = ls_edad - 1
        END IF
    END IF
    
    RETURN ls_edad

END FUNCTION


