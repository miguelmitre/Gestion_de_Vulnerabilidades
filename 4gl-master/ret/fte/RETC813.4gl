#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETC813  => LIQUIDACION DE RCV - RETIRO A                             #
#                     (IV Y RT HISTORICO)                                       #
#Fecha creacion    => 31 DE ENERO DEL 2004                                      #
#By                => DMR                                                       #
#Fecha actualiz.   => 04-Agosto-2004                                            #
#Fecha actualiza   => 4 DE ENERO DE 2008                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 7 DE OCTUBRE DE 2008                                      #
#                     Modificaciones para liquidacion del monto constitutivo    #
#                     y generacion del layout de la operacion 39                #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 22 DE JUNIO DE 2010                                       #
#                     Modificaciones para agregar el campo de clave de          #
#                     aseguradora en la generacion de la op. 39                 #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega la ejecucion del SPL que inserta en las tablas  #
#                     de las estadisticas de CONSAR (Req. EFPS-152)             #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE #glo #g_seg_modulo
        g_seg_modulo         RECORD LIKE seg_modulo.*

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_dat RECORD
        folio_oper_02         INTEGER ,
        fecha_val_acc         DATE
    END RECORD

    DEFINE gr_edo RECORD
        recibido              LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE #date
        gd_fecha_viv          ,
        HOY                   DATE

    DEFINE #glo #char
        gs_tipo_retiro        CHAR(001) ,
        G_LISTA               CHAR(100) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        comando               CHAR(110) ,
        gc_usuario            CHAR(008) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        enter                 CHAR(001) ,
        HORA                  CHAR(005) ,
        v_desmarca            CHAR(100)

    DEFINE
        gs_cod_tramite          ,
        gs_tipo_mov             ,
        gs_cod_afore            SMALLINT

    DEFINE #integer
        gi_liquidados           ,
        cont_reg_av             INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC813.log")

    CALL init() #i
    CALL f_abre_ventana()
    CALL f_obtiene_precios_accion(HOY)
    CALL f_captura_datos()

    SELECT COUNT(*)
    INTO   cont_reg_av
    FROM   ret_transf_rx
    WHERE  folio            = gr_dat.folio_oper_02
    AND    estado_solicitud = gr_edo.enviado
    AND    tipo_retiro      = gs_tipo_retiro

    DISPLAY " TOTAL REGISTROS A PROCESAR : ", cont_reg_av AT 11,21

    CALL primer_paso()  #pp --- AFECTA CUENTA INDIVIDUAL Y ACTUALIZA ESTADOS ---
    CALL segundo_paso() #sp --- DESMARCA CUENTAS ---
    CALL tercer_paso()  #tp --- GENERA EL LAYOUT DE LA OPERACION 39 ---

    CLOSE WINDOW retc8131
    CALL f_abre_ventana()

    DISPLAY "FOLIO NUMERO               : ",gr_dat.folio_oper_02  AT 08,21
    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_reg_av           AT 10,21
    DISPLAY "TOTAL REGISTROS LIQUIDADOS : ",gi_liquidados         AT 11,21

    DISPLAY "EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 13,21
    DISPLAY G_LISTA CLIPPED AT 14,21
    DISPLAY "CON EL NOMBRE : ",c12_nombre_plano AT 16,21

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc8131

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET gr_dat.fecha_val_acc    = TODAY USING "MM-DD-YYYY"
    LET HOY                     = TODAY
    LET HORA                    = TIME
    LET gs_tipo_retiro          = "A"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DETAV39"
    LET c12_nombre_plano = HOY USING "YYYYMMDD", ".39P"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    ----- FOLIO MAXIMO UTILIZADO -----
    SELECT MAX(folio)
    INTO   gr_dat.folio_oper_02
    FROM   ret_transf_rx
    WHERE  estado_solicitud = gr_edo.enviado
    AND    tipo_retiro      = gs_tipo_retiro

    ----- TIPO MOVIMIENTO -----
    SELECT A.movimiento
    INTO   gs_tipo_mov
    FROM   tab_retiro A
    WHERE  A.tipo_retiro = gs_tipo_retiro

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    --- LIQUIDACION ---
    LET lc_prepare = " EXECUTE PROCEDURE fn_liquida ( ?,?,?,?,? )"
    PREPARE eje_liquida FROM lc_prepare

    LET lc_prepare = " "

    --- FECHA VALOR VIVENDA ---
    LET lc_prepare = "EXECUTE FUNCTION fn_obten_fecha_val ( ? )"
    PREPARE eje_fecha_viv FROM lc_prepare

    EXECUTE eje_fecha_viv USING HOY
                          INTO  gd_fecha_viv

    LET lc_prepare = " "

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_consar(?,?,?,?)"
    PREPARE eje_CONSAR FROM lc_prepare

    LET lc_prepare = " "


END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Afecta montos en la cuenta individual y actualiza el        #
#               estado de la solicitud en los nss liquidados                #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE #loc #lr_dis_prov
        lr_dis_prov           RECORD LIKE dis_provision.*


    DEFINE
        ld_mto_const        LIKE ret_transf_rx.mto_constitutivo,
        ld_mto_const_afore  LIKE ret_transf_tx.mto_const_afore

    -- -----------------------------------------------------------------------------

    DECLARE cur_22 CURSOR FOR
    SELECT B.*,
           A.mto_constitutivo,
           C.mto_const_afore
    FROM   ret_transf_rx A,
           dis_provision B,
           ret_transf_tx C
    WHERE  A.nss              = B.nss
    AND    A.nss              = C.nss
    AND    A.consecutivo      = B.consecutivo_lote
    AND    A.consecutivo      = C.consecutivo
    AND    A.folio            = C.folio
    AND    A.folio            = gr_dat.folio_oper_02
    AND    A.estado_solicitud = gr_edo.enviado
    AND    B.tipo_movimiento  = gs_tipo_mov
    ORDER BY B.nss, B.subcuenta

    FOREACH cur_22 INTO lr_dis_prov.*,
                        ld_mto_const ,
                        ld_mto_const_afore

        CALL f_actualiza_cta_ind(lr_dis_prov.*, ld_mto_const, ld_mto_const_afore)

        UPDATE ret_transf_rx
        SET    estado_solicitud = gr_edo.liquidado
        WHERE  folio            = gr_dat.folio_oper_02
        AND    nss              = lr_dis_prov.nss
        AND    consecutivo      = lr_dis_prov.consecutivo_lote
        AND    tipo_retiro      = gs_tipo_retiro

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza la desmarca de las cuentas que fueron liquidadas   #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE pr_elimina RECORD
        nss         LIKE dis_provision.nss              ,
        consecu     LIKE dis_provision.consecutivo_lote
    END RECORD

    DEFINE lr_liquida RECORD 
        nss                 LIKE dis_cuenta.nss             ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio               
    END RECORD

    DEFINE
        ls_inserta              SMALLINT

    -- -----------------------------------------------------------------------------

    DECLARE cur_des CURSOR FOR
    SELECT UNIQUE nss      ,
           consecutivo_lote
    FROM   dis_provision
    WHERE  folio           = gr_dat.folio_oper_02
    AND    tipo_movimiento = gs_tipo_mov

    FOREACH cur_des INTO pr_elimina.*

        CALL f_desmarca_cuenta(pr_elimina.*)

        SELECT "OK"
        FROM   cta_act_marca
        WHERE  nss         = pr_elimina.nss
        AND    correlativo = pr_elimina.consecu
        AND    marca_cod   = 140

        IF STATUS <> NOTFOUND THEN
            CALL f_solicita_edo_cuenta(pr_elimina.nss)
        END IF

    END FOREACH

    -- Inserta montos a estadisticas CONSAR
    DECLARE cur_liquida CURSOR FOR
    SELECT UNIQUE(nss)      ,
           consecutivo_lote ,
           folio
    FROM   dis_cuenta
    WHERE  folio            = gr_dat.folio_oper_02
    AND    tipo_movimiento  = gs_tipo_mov

    FOREACH cur_liquida INTO lr_liquida.*

        EXECUTE eje_CONSAR USING lr_liquida.nss                 ,
                                 lr_liquida.consecutivo_lote    ,
                                 lr_liquida.folio               ,
                                 gs_cod_tramite
                           INTO  ls_inserta

        INITIALIZE lr_liquida.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Obtiene los datos para generar el archivo plano de la       #
#               operacion 39                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()
#sp-------------------

    DEFINE lr_datos_nss RECORD
        nss         LIKE ret_transf_rx.nss              ,
        curp        LIKE ret_transf_rx.curp             ,
        sec_pension LIKE ret_transf_rx.sec_pension      ,
        tipo_mov    LIKE ret_transf_rx.tipo_mov_procesar,
        mto_const   LIKE ret_transf_rx.mto_constitutivo ,
        cve_aseg    LIKE ret_transf_rx.cve_aseguradora
    END RECORD

    -- -----------------------------------------------------------------------------

    LET gi_liquidados    = 0
    LET G_LISTA_1 = G_LISTA CLIPPED, "/", gc_usuario CLIPPED, ".", c7_nombre_plano

    START REPORT r_listado_op39 TO G_LISTA_1

    DECLARE c_datos CURSOR FOR
        SELECT nss              ,
               curp             ,
               sec_pension      ,
               tipo_mov_procesar,
               mto_constitutivo ,
               cve_aseguradora
        FROM   ret_transf_rx
        WHERE  folio            = gr_dat.folio_oper_02
        AND    tipo_retiro      = gs_tipo_retiro
        AND    estado_solicitud = gr_edo.liquidado


    FOREACH c_datos INTO lr_datos_nss.*

        OUTPUT TO REPORT r_listado_op39(lr_datos_nss.*)
        LET gi_liquidados = gi_liquidados + 1
        DISPLAY "NUMERO DE REGISTROS PROCESADOS ", gi_liquidados AT 11,21

    END FOREACH

    FINISH REPORT r_listado_op39

    LET comando = " "
    LET comando = "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/", gc_usuario CLIPPED, ".", c7_nombre_plano
    RUN comando

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano

    LET comando = " "
    LET comando = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    RUN comando

    LET comando = " "
    LET comando = "chmod 777 ",G_LISTA_2
    RUN comando

    LET comando = " "
    LET comando = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/", c7_nombre_plano
    RUN comando

    UPDATE ret_ctr_envio_lote
    SET    fecha_envio   = HOY              ,
           hora_envio    = HORA             ,
           estado        = gr_edo.liquidado ,
           usuario_envio = gc_usuario
    WHERE  tipo_retiro   = gs_tipo_retiro
    AND    folio         = gr_dat.folio_oper_02

END FUNCTION

#---------------------------------------------------------------------------#
# f_solicita_edo_cuenta : Actualiza la tabla cta_ctr_cuenta para indicar    #
#                         una solicitud de edo. de cuenta                   #
#---------------------------------------------------------------------------#
FUNCTION f_solicita_edo_cuenta(pc_nss)

    DEFINE pc_nss LIKE cta_ctr_cuenta.nss

    -- -----------------------------------------------------------------------------

    UPDATE cta_ctr_cuenta
    SET    tipo_informe  = 5    ,
           fecha_informe = HOY
    WHERE  nss           = pc_nss

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana general del programa                     #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc8131 AT 4,4 WITH FORM "RETC8131" ATTRIBUTE(BORDER)
    DISPLAY "   < Ctrl-C > Salir                                        RETIRO 'A'       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC813           GENERA LIQUIDACION DE MONTOS DE IV-RT                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y la fecha de proceso para ejecutar la #
#                   liquidacion                                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE
        ls_status       SMALLINT

    DEFINE
        lc_mensaje_err  CHAR(100)

    -- -----------------------------------------------------------------------------

    INPUT BY NAME gr_dat.* WITHOUT DEFAULTS

        AFTER FIELD folio_oper_02
            IF gr_dat.folio_oper_02 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_transf_rx
                WHERE  folio       = gr_dat.folio_oper_02
                AND    tipo_retiro = gs_tipo_retiro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD folio_oper_02
                ELSE
                    CALL f_valida_edo_sol()
                        RETURNING ls_status, lc_mensaje_err

                    IF ls_status <> 0 THEN
                        ERROR lc_mensaje_err ATTRIBUTE(REVERSE)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

        AFTER FIELD fecha_val_acc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD folio_oper_02
            END IF

            IF gr_dat.fecha_val_acc IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_acc
            END IF

        ON KEY (ESC)
            IF gr_dat.folio_oper_02 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_transf_rx
                WHERE  folio  = gr_dat.folio_oper_02
                AND    tipo_retiro = gs_tipo_retiro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD folio_oper_02
                ELSE
                    CALL f_valida_edo_sol()
                        RETURNING ls_status, lc_mensaje_err

                    IF ls_status <> 0 THEN
                        ERROR lc_mensaje_err ATTRIBUTE(REVERSE)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

            IF gr_dat.fecha_val_acc IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_acc
            END IF
            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT" PROCESO CANCELADO...< ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT" PROCESO CANCELADO...< ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Valida y obtiene los precios de accion para el #
#                            dia en curso                                   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        ls_sie                SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " -- SIEFORE ", lc_siefore CLIPPED

            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_edo_sol : Valida que el estado de solicitud se el correcto       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_edo_sol()

    DEFINE
        ls_err      ,
        ls_estado   SMALLINT

    DEFINE
        lc_mensaje  CHAR(100)

    -- -----------------------------------------------------------------------------

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_transf_rx
    WHERE  folio       = gr_dat.folio_oper_02
    AND    tipo_retiro = gs_tipo_retiro

    CASE ls_estado
        WHEN gr_edo.recibido
            LET lc_mensaje = "    EL FOLIO INGRESADO AUN NO HA SIDO PROVISIONADO..."
            LET ls_err = 1

        WHEN gr_edo.enviado
            LET lc_mensaje = " "
            LET ls_err = 0

        WHEN gr_edo.liquidado
            LET lc_mensaje = "    EL FOLIO INGRESADO YA FUE LIQUIDADO..."
            LET ls_err = 1

        OTHERWISE
            LET lc_mensaje = "    ERROR: EL ESTATUS DEL FOLIO DEBE SER: 04 (ENVIADO)"
            LET ls_err = 1
    END CASE

    RETURN ls_err, lc_mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_cta_ind : Realiza la actualizacion en dis_cuenta dependiendo  #
#                       de si se paga o no el monto constitutivo            #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_cta_ind(pr_provision, pr_constitutivo)

    DEFINE pr_provision RECORD LIKE dis_provision.*

    DEFINE pr_constitutivo RECORD
        mto_recibido        LIKE ret_transf_rx.mto_constitutivo,
        mto_calculado       LIKE ret_transf_tx.mto_const_afore
    END RECORD

    DEFINE
        ls_siefore              SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_siefore = pr_provision.siefore

    -- Modificamos los valores que se insertaran en dis_cuenta
    LET pr_provision.fecha_pago        = HOY
    LET pr_provision.fecha_conversion  = HOY
    LET pr_provision.fecha_proceso     = HOY
    LET pr_provision.fecha_archivo     = HOY
    LET pr_provision.usuario           = gc_usuario

    IF pr_provision.subcuenta <> 4 THEN

        LET pr_provision.fecha_valor       = HOY
        LET pr_provision.precio_accion     = gar_precio_acc[ls_siefore].precio_dia

        IF pr_constitutivo.mto_recibido <= f_redondea_val(pr_constitutivo.mto_calculado, 2) THEN
            -- Si pagamos el monto constitutivo nos aseguramos de no rebasar dicho monto en pesos
            LET pr_provision.monto_en_acciones = pr_provision.monto_en_pesos / pr_provision.precio_accion
        ELSE
            -- Si pagamos el saldo de la cuenta nos aseguramos de pagar lo provisionado en acciones
            LET pr_provision.monto_en_pesos = pr_provision.monto_en_acciones * pr_provision.precio_accion
        END IF
    ELSE
        IF MONTH(pr_provision.fecha_valor) <> MONTH(HOY) THEN
            LET pr_provision.fecha_valor    = gd_fecha_viv
            LET pr_provision.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
            LET pr_provision.monto_en_pesos = pr_provision.monto_en_acciones * pr_provision.precio_accion
            LET pr_provision.monto_en_pesos = f_redondea_val(pr_provision.monto_en_pesos,2)
        END IF
    END IF

    INSERT INTO dis_cuenta
    VALUES (pr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se haya liquidado                  #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               gs_tipo_mov          ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_redondea_val : Redondea el monto indicado en pd_mto_redondear tantos    #
#                  decimales como se indique en ps_redondea                 #
#---------------------------------------------------------------------------#
FUNCTION f_redondea_val(pd_mto_redondear, ps_redondea)

    DEFINE
        pd_mto_redondear        DECIMAL(16,6)

    DEFINE
        ps_redondea             SMALLINT

    DEFINE
        ls_monto_return         DECIMAL(16,2)

    -- -----------------------------------------------------------------------------

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING pd_mto_redondear, ps_redondea
    FETCH round_cur INTO ls_monto_return

    CLOSE round_cur

    RETURN ls_monto_return

END FUNCTION

#---------------------------------------------------------------------------#
# r_listado_op39 : Genera el archivo plano de la operacion 39               #                                                                          #
#---------------------------------------------------------------------------#
REPORT r_listado_op39(lr_ret_transf)

    DEFINE lr_ret_transf RECORD
        nss         LIKE ret_transf_rx.nss              ,
        curp        LIKE ret_transf_rx.curp             ,
        sec_pension LIKE ret_transf_rx.sec_pension      ,
        tipo_mov    LIKE ret_transf_rx.tipo_mov_procesar,
        mto_const   LIKE ret_transf_rx.mto_constitutivo ,
        cve_aseg    LIKE ret_transf_rx.cve_aseguradora
    END RECORD

    DEFINE lr_det_liquida RECORD
        siefore     LIKE dis_cuenta.siefore          ,
        subcuenta   LIKE dis_cuenta.subcuenta        ,
        mto_pesos   LIKE dis_cuenta.monto_en_pesos   ,
        mto_acc     LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE lr_montos RECORD
        acc_ret97 DECIMAL(16,6),
        acc_cv    DECIMAL(16,6),
        acc_cs    DECIMAL(16,6),
        pes_ret97 DECIMAL(16,2),
        pes_cv    DECIMAL(16,2),
        pes_cs    DECIMAL(16,2)
    END RECORD

    DEFINE
        ls_num_regs   SMALLINT

    DEFINE
        c15_acc_ret97 ,
        c15_acc_cv    ,
        c15_acc_cs    CHAR(15),
        c14_acc_ret97 ,
        c14_acc_cv    ,
        c14_acc_cs    CHAR(14)

    DEFINE
        c16_mto_const CHAR(16),
        c11_pes_ret97 ,
        c11_pes_cv    ,
        c11_pes_cs    CHAR(11),
        c10_mto_const ,
        c10_pes_ret97 ,
        c10_pes_cv    ,
        c10_pes_cs    CHAR(10)

    DEFINE fecha_operacion   LIKE ret_cza_lote.fecha_operacion
    DEFINE fecha_valor_trans LIKE ret_cza_lote.fecha_valor_trans

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1000
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        FIRST PAGE HEADER

            LET ls_num_regs = 0

            -- Rescatamos las fechas de operacion y de valor
            SELECT A.fecha_operacion,
                   A.fecha_valor_trans
            INTO   fecha_operacion,
                   fecha_valor_trans
            FROM   ret_cza_lote A
            WHERE  A.folio = gr_dat.folio_oper_02

            -- Encabezado
            PRINT
                COLUMN 001, "01"                               ,--tipo de registro
                COLUMN 003, "04"                               ,--identificador de servicio
                COLUMN 005, "01"                               ,--tipo entidad origen
                COLUMN 007, gs_cod_afore USING "&&&"           ,--clave entidad origen
                COLUMN 010, "03"                               ,--tipo entidad destino
                COLUMN 012, "001"                              ,--clave entidad destino
                COLUMN 015, fecha_operacion   USING "YYYYMMDD" ,--fecha de operacion
                COLUMN 023, fecha_valor_trans USING "YYYYMMDD" ,--fecha de transferencia
                COLUMN 031, 120 SPACES


        ON EVERY ROW

            --Inicializamos variables
            LET lr_montos.acc_cv    = 0
            LET lr_montos.pes_cv    = 0
            LET lr_montos.acc_ret97 = 0
            LET lr_montos.pes_ret97 = 0
            LET lr_montos.acc_cs    = 0
            LET lr_montos.pes_cs    = 0
            LET ls_num_regs         = ls_num_regs + 1

            DECLARE c_montos CURSOR FOR
                SELECT siefore,
                       subcuenta,
                       monto_en_pesos,
                       monto_en_acciones
                FROM   dis_cuenta
                WHERE  folio     = gr_dat.folio_oper_02
                AND    nss       = lr_ret_transf.nss
                AND    subcuenta <> 4

            FOREACH c_montos INTO lr_det_liquida.*

                CASE lr_det_liquida.subcuenta
                    WHEN 1
                        LET lr_montos.acc_ret97 = lr_det_liquida.mto_acc
                        LET lr_montos.pes_ret97 = lr_det_liquida.mto_pesos
                    WHEN 2
                        LET lr_montos.acc_cv    = lr_montos.acc_cv + lr_det_liquida.mto_acc
                        LET lr_montos.pes_cv    = lr_montos.pes_cv + lr_det_liquida.mto_pesos
                    WHEN 5
                        LET lr_montos.acc_cs    = lr_det_liquida.mto_acc
                        LET lr_montos.pes_cs    = lr_det_liquida.mto_pesos
                    WHEN 6
                        LET lr_montos.acc_cv    = lr_montos.acc_cv + lr_det_liquida.mto_acc
                        LET lr_montos.pes_cv    = lr_montos.pes_cv + lr_det_liquida.mto_pesos
                    WHEN 9
                        LET lr_montos.acc_cv    = lr_montos.acc_cv + lr_det_liquida.mto_acc
                        LET lr_montos.pes_cv    = lr_montos.pes_cv + lr_det_liquida.mto_pesos
                END CASE

            END FOREACH

            LET lr_montos.pes_ret97 = f_redondea_val(lr_montos.pes_ret97, 2)
            LET lr_montos.pes_cv    = f_redondea_val(lr_montos.pes_cv, 2)
            LET lr_montos.pes_cs    = f_redondea_val(lr_montos.pes_cs, 2)


            LET c16_mto_const = lr_ret_transf.mto_const USING "&&&&&&&&&&&&&.&&"
            LET c10_mto_const = c16_mto_const[06,13], c16_mto_const[15,16]

            --Formateamos Retiro 97
            LET c15_acc_ret97 = lr_montos.acc_ret97 USING "&&&&&&&&.&&&&&&"
            LET c14_acc_ret97 = c15_acc_ret97[01,08], c15_acc_ret97[10,15]

            LET c11_pes_ret97 = lr_montos.pes_ret97 USING "&&&&&&&&.&&"
            LET c10_pes_ret97 = c11_pes_ret97[01,08], c11_pes_ret97[10,11]

            --Formateamos CV
            LET c15_acc_cv = lr_montos.acc_cv USING "&&&&&&&&.&&&&&&"
            LET c14_acc_cv = c15_acc_cv[01,08], c15_acc_cv[10,15]

            LET c11_pes_cv = lr_montos.pes_cv USING "&&&&&&&&.&&"
            LET c10_pes_cv = c11_pes_cv[01,08], c11_pes_cv[10,11]

            --Formateamos CS
            LET c15_acc_cs = lr_montos.acc_cs USING "&&&&&&&&.&&&&&&"
            LET c14_acc_cs = c15_acc_cs[01,08], c15_acc_cs[10,15]

            LET c11_pes_cs = lr_montos.pes_cs USING "&&&&&&&&.&&"
            LET c10_pes_cs = c11_pes_cs[01,08], c11_pes_cs[10,11]

            -- Detalle
            PRINT
                COLUMN 001, "03"                                   ,--tipo_registro
                COLUMN 003, "04"                                   ,
                COLUMN 005, "39"                                   ,--tipo_operacion
                COLUMN 007, lr_ret_transf.nss                      ,--nss
                COLUMN 018, lr_ret_transf.curp                     ,
                COLUMN 036, lr_ret_transf.sec_pension USING "&&"   ,--secuencia de pension
                COLUMN 038, lr_ret_transf.tipo_mov USING "&&&"     ,--tipo de mov procesar
                COLUMN 041, c10_mto_const                          ,--monto constitutivo
                COLUMN 051, c10_pes_ret97                          ,
                COLUMN 061, c10_pes_cv                             ,
                COLUMN 071, c10_pes_cs                             ,
                COLUMN 081, lr_det_liquida.siefore USING "&&"      ,--siefore
                COLUMN 083, c14_acc_ret97                          ,
                COLUMN 097, c14_acc_cv                             ,
                COLUMN 111, c14_acc_cs                             ,
                COLUMN 125, lr_ret_transf.cve_aseg USING "&&&"     ,
                COLUMN 128, 23 SPACES

        ON LAST ROW
            -- Sumario
            PRINT
                COLUMN 001, "09"                      ,--tipo de registro
                COLUMN 003, "04"                      ,--identificador de servicio
                COLUMN 005, "01"                      ,--tipo entidad origen
                COLUMN 007, gs_cod_afore USING"&&&"   ,--clave entidad origen
                COLUMN 010, "03"                      ,--tipo entidad destino
                COLUMN 012, "001"                     ,--clave entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"      ,--fecha de operacion
                COLUMN 023, ls_num_regs USING"&&&&&&" ,--numero de registros
                COLUMN 029, 122 SPACES

END REPORT
