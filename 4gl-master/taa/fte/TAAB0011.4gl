##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa TAAB001  => CARGA ARCHIVO DISPERSION CUENTA AFORE - AFORE          #
#Fecha             => 31 DE ENERO DE 2001                                    #
#Por               => MAURO MUNIZ CABALLERO.                                 #
#Actualiza         => MAURO MUÑIZ CABALLERO                                  #
#Fecha Actualiza   => 7 DE JULIO  DE 2004                                    #
#        Se adecuó el programa para circ 28-8 y participaciones              #
#Actualiza         => MAURO MUNIZ CABALLERO                                  #
#Fecha Actualiza   => 3 DE DICIEMBRE DE 2004                                 #
#        Se adecuo el programa para circ 28-9 ( multisiefores )              #
#Actualiza         => JOSUE LISANDRO HUERTA SIERRA                           #
#Fecha Actualiza   => 5 DE MARZO DE 2008                                     #
#        Se adecuo el programa para circ 69-2 ( multisiefores )              #
#Sistema           => TAA                                                    #
#Req: 1048         => JCPV 07/11/2022 ts 9,11 para tt 57                     #
#Req: 1038         => JCPV 15/11/2012 Apertura de Cuentas                    #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
       g_param_taa RECORD LIKE seg_modulo.*,
       w_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE reg_cza RECORD LIKE taa_cza_recepcion.*
    DEFINE reg_rcv RECORD LIKE taa_rcv_recepcion.*
    DEFINE reg_viv RECORD LIKE taa_viv_recepcion.*
    DEFINE reg_sum RECORD LIKE taa_sum_recepcion.*

    DEFINE reg_cza_tras RECORD LIKE safre_tmp:cza_tra_afo.*
    DEFINE reg_det_viv  RECORD LIKE safre_tmp:det_tra_viv.*
    DEFINE reg_det_rcv  RECORD LIKE safre_tmp:det_tra_rcv.*
    DEFINE reg_sum_tras RECORD LIKE safre_tmp:sum_tra_afo.*

    DEFINE reg_pagos RECORD
        folio             INTEGER,
        tipo_registro     CHAR(002),
        ident_servicio    CHAR(002),
        ident_pago        CHAR(016),
        importe           CHAR(015),
        fecha_liquidacion CHAR(008),
        impt_acept_rcv    CHAR(015),
        impt_acept_viv    CHAR(015),
        impt_aport_dev    CHAR(015),
        estado            CHAR(001),
        fecha_archivo     DATE
    END RECORD

    DEFINE
        HOY              DATE     ,
        cfecha_pat       CHAR(10) ,
        cfecha_ven       CHAR(10) ,
        vusuario         CHAR(8)  ,
        v_presenta       DATE     ,
        v_banxico        DATE     ,
        vfecha_pat       CHAR(10) ,
        vfecha_ven       CHAR(10) ,
        vfolio           INTEGER  ,
        vcontador        INTEGER  ,
        id_op            SMALLINT

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE reg_carta            RECORD LIKE int_ctr_carta.*
    DEFINE consulta_carta       CHAR(120)

    DEFINE fliquida             DATE
    DEFINE vcve_ced_cuenta      CHAR(03)

END GLOBALS

MAIN

    CALL STARTLOG(FGL_GETENV("USER")||".TAAB0011.log")
    CALL inicio()
    CALL abre_ventana()

END MAIN

FUNCTION abre_ventana()

    SELECT *,user
      INTO g_param_taa.*,vusuario
      FROM seg_modulo
     WHERE modulo_cod = 'taa'

    CALL proceso_principal() #pp

    IF (id_op = 3)  OR
       (id_op = 31) THEN
        CALL actualiza_afiliados() #aa
    END IF

    UPDATE taa_ctr_traspaso
       SET fin_incorpora = CURRENT
     WHERE fecha_presentacion = reg_cza_tras.fecha_presentacion
       AND id_operacion       = reg_cza_tras.ident_operacion
       AND folio              = vfolio

END FUNCTION

FUNCTION inicio()

    LET vfolio                 = ARG_VAL(1)
    LET fliquida               = ARG_VAL(2)
    LET reg_bat.pid            = ARG_VAL(3)
    LET reg_bat.proceso_cod    = ARG_VAL(4)
    LET reg_bat.opera_cod      = ARG_VAL(5)

    DISPLAY reg_bat.pid
    DISPLAY reg_bat.proceso_cod
    DISPLAY reg_bat.opera_cod
    DISPLAY vfolio
    DISPLAY fliquida

    INITIALIZE reg_cza.* TO NULL
    INITIALIZE reg_rcv.* TO NULL
    INITIALIZE reg_viv.* TO NULL
    INITIALIZE reg_sum.* TO NULL

    SELECT codigo_afore, user
    INTO   w_codigo_afore, vusuario
    FROM   tab_afore_local

    LET HOY       = TODAY
    LET vcontador = 0

    INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION proceso_principal()

    DEFINE
        cont_reg           INTEGER ,
        v_tipo_solicitud   SMALLINT

    SELECT *
    INTO   reg_cza_tras.*
    FROM   safre_tmp:cza_tra_afo

    CASE reg_cza_tras.ident_operacion
        WHEN '09' LET id_op = 3
        WHEN '12' LET id_op = 4
    END CASE

    SELECT unique cve_ced_cuenta
    INTO   vcve_ced_cuenta
    FROM   safre_tmp:det_tra_viv
    WHERE  fecha_presentacion = reg_cza_tras.fecha_presentacion
    AND    ident_operacion    = reg_cza_tras.ident_operacion
    AND    cve_ced_cuenta     = '531'
    GROUP BY 1
    IF STATUS <> NOTFOUND THEN
       CASE reg_cza_tras.ident_operacion
         WHEN '09' LET id_op = 31
         WHEN '12' LET id_op = 32
       END CASE
    END IF

    SELECT "X"
    FROM   taa_cza_recepcion
    WHERE  folio = vfolio
    AND    fecha_presentacion = reg_cza_tras.fecha_presentacion

    IF SQLCA.SQLCODE <> 0 THEN
        INSERT INTO taa_cza_recepcion VALUES (vfolio, reg_cza_tras.*, 1)
    END IF

    LET cont_reg  = 1

    #LET v_presenta = reg_cza_tras.fecha_presentacion

    #CALL fechas(v_presenta) RETURNING v_banxico

    LET v_banxico = fliquida

    DECLARE cur_3 CURSOR FOR
    SELECT *
      FROM safre_tmp:det_tra_viv
     WHERE estado_reg = 1

    FOREACH cur_3 INTO reg_det_viv.*

        DECLARE cur_5 CURSOR FOR
        SELECT *
        FROM   safre_tmp:det_tra_rcv
        WHERE  n_seguro = reg_det_viv.n_seguro
        AND    estado_reg = 1
        AND    cve_ced_cuenta = reg_det_viv.cve_ced_cuenta

        FOREACH cur_5 INTO reg_det_rcv.*

            LET reg_viv.folio              = vfolio
            LET reg_viv.tipo_registro      = "25"
            LET reg_viv.ident_operacion    = reg_det_viv.ident_operacion
            LET reg_viv.cont_servicio      = reg_det_viv.cont_servicio
            LET reg_viv.tipo_recep_cuenta  = reg_det_viv.tipo_recep_cuenta
            LET reg_viv.cve_recep_cuenta   = reg_det_viv.cve_recep_cuenta
            LET reg_viv.tipo_ced_cuenta    = reg_det_viv.tipo_ced_cuenta
            LET reg_viv.cve_ced_cuenta     = reg_det_viv.cve_ced_cuenta
            LET reg_viv.tipo_traspaso      = reg_det_viv.tipo_traspaso
            LET reg_viv.fecha_presentacion = reg_det_viv.fecha_presentacion
            LET reg_viv.fecha_mov_banxico  = v_banxico
            LET reg_viv.curp               = reg_det_viv.n_unico
            LET reg_viv.nss                = reg_det_viv.n_seguro
            LET reg_viv.rfc                = reg_det_viv.rfc
            LET reg_viv.paterno            = reg_det_viv.paterno
            LET reg_viv.materno            = reg_det_viv.materno
            LET reg_viv.nombres            = reg_det_viv.nombre
            LET reg_viv.ind_nom_mod        = reg_det_viv.ind_nom_mod
            LET reg_viv.cve_sector         = reg_det_viv.cve_sector
            LET reg_viv.fecha_afiliacion   = ""
            LET reg_viv.ident_lote_solic   = reg_det_viv.ident_lote_solic
            LET reg_viv.nss_cedente        = reg_det_viv.nss_cedente
            LET reg_viv.rfc_cedente        = reg_det_viv.rfc_cedente
            LET reg_viv.paterno_cedente    = reg_det_viv.paterno_cedente
            LET reg_viv.materno_cedente    = reg_det_viv.materno_cedente
            LET reg_viv.nombres_cedente    = reg_det_viv.nombre_cedente
            LET reg_viv.aivs_issste_08     = reg_det_viv.aivs_issste_08
            LET reg_viv.saldo_issste_08    = reg_det_viv.sdo_issste_08
            LET reg_viv.partic_viv97       = reg_det_viv.partic_viv97
            LET reg_viv.saldo_viv97        = reg_det_viv.sdo_viv_97
            LET reg_viv.aivs_issste        = reg_det_viv.aivs_issste
            LET reg_viv.saldo_issste       = reg_det_viv.sdo_issste
            LET reg_viv.partic_viv92       = reg_det_viv.partic_viv92
            LET reg_viv.saldo_viv92        = reg_det_viv.sdo_viv_92
            LET reg_viv.ind_nom_bdnsar     = reg_det_viv.ind_nom_bdnsar
            LET reg_viv.ind_reg_reenv      = reg_det_viv.ind_reg_reenv
            LET reg_viv.ident_garantia     = reg_det_viv.ident_garantia
            LET reg_viv.fecha_red_bono     = reg_det_viv.fecha_red_bono
            LET reg_viv.importe_bono       = reg_det_viv.importe_bono
            LET reg_viv.aivs_issste_08_bd  = reg_det_viv.aivs_issste_08_bd
            LET reg_viv.aivs_issste_bd     = reg_det_viv.aivs_issste_bd
            LET reg_viv.fecha_red_bono_bd  = reg_det_viv.fecha_red_bono_bd
            LET reg_viv.importe_bono_bd    = reg_det_viv.importe_bono_bd
            LET reg_viv.no_apor_viv        = reg_det_viv.no_aport_viv
            LET reg_viv.dias_pag_cuo_soc   = reg_det_viv.dias_cuot_soc
            LET reg_viv.no_apor_issste     = reg_det_viv.no_apor_issste
            LET reg_viv.estado             = 1

            SELECT "X"
              FROM taa_viv_recepcion
             WHERE @folio = vfolio
               AND @nss   = reg_viv.nss
               AND @fecha_mov_banxico = v_banxico
               AND @cont_servicio = reg_viv.cont_servicio

            IF SQLCA.SQLCODE <> 0 THEN
                LET vcontador = vcontador + 1

                INSERT INTO taa_viv_recepcion VALUES(reg_viv.*)

                UPDATE taa_ctr_traspaso
                   SET reg_incorpora = vcontador
                 WHERE folio = vfolio
            END IF

            LET reg_rcv.folio              = vfolio
            LET reg_rcv.tipo_registro      = "25"
            LET reg_rcv.ident_operacion    = reg_det_rcv.ident_operacion
            LET reg_rcv.cont_servicio      = reg_det_rcv.cont_servicio
            LET reg_rcv.tipo_recep_cuenta  = reg_det_viv.tipo_recep_cuenta
            LET reg_rcv.cve_recep_cuenta   = reg_det_viv.cve_recep_cuenta
            LET reg_rcv.tipo_ced_cuenta    = reg_det_viv.tipo_ced_cuenta
            LET reg_rcv.cve_ced_cuenta     = reg_det_viv.cve_ced_cuenta
            LET reg_rcv.tipo_traspaso      = reg_det_viv.tipo_traspaso
            LET reg_rcv.fecha_presentacion = reg_det_viv.fecha_presentacion
            LET reg_rcv.fecha_mov_banxico  = v_banxico
            LET reg_rcv.curp               = reg_det_viv.n_unico
            LET reg_rcv.nss                = reg_det_viv.n_seguro
            LET reg_rcv.rfc                = reg_det_viv.rfc
            LET reg_rcv.paterno            = reg_det_viv.paterno
            LET reg_rcv.materno            = reg_det_viv.materno
            LET reg_rcv.nombres            = reg_det_viv.nombre
            LET reg_rcv.cve_sector         = reg_det_viv.cve_sector
            LET reg_rcv.fecha_afiliacion   = ""
            LET reg_rcv.ident_lote_solic   = reg_det_viv.ident_lote_solic
            LET reg_rcv.nss_cedente        = reg_det_viv.nss_cedente
            LET reg_rcv.porcentaje_pat     = ""
            LET reg_rcv.porcentaje_vol     = ""
            LET reg_rcv.porcentaje_ret     = ""
            LET reg_rcv.porcentaje_cv      = ""
            LET reg_rcv.porcentaje_cs      = ""
            LET reg_rcv.porcentaje_sar     = ""
            LET reg_rcv.fecha_valua_acc    = reg_det_rcv.fecha_valua_acc
            LET reg_rcv.porcentaje_aar     = ""
            LET reg_rcv.porcentaje_acr     = ""
            LET reg_rcv.porcentaje_acp     = ""
            LET reg_rcv.ind_cta_transf     = reg_det_rcv.ind_cta_transf
            LET reg_rcv.cve_subcta_1       = reg_det_rcv.cve_subcta_1
            LET reg_rcv.prctj_subc_1       = reg_det_rcv.prctj_subc_1
            LET reg_rcv.saldo_subc_1       = reg_det_rcv.import_sub_1
            LET reg_rcv.no_tot_acc_1       = reg_det_rcv.no_tot_acc_1
            LET reg_rcv.siefore_1          = reg_det_rcv.siefore_1
            LET reg_rcv.precio_acc_1       = reg_det_rcv.precio_acc_1
            LET reg_rcv.cve_subcta_2       = reg_det_rcv.cve_subcta_2
            LET reg_rcv.prctj_subc_2       = reg_det_rcv.prctj_subc_2
            LET reg_rcv.saldo_subc_2       = reg_det_rcv.import_sub_2
            LET reg_rcv.no_tot_acc_2       = reg_det_rcv.no_tot_acc_2
            LET reg_rcv.siefore_2          = reg_det_rcv.siefore_2
            LET reg_rcv.precio_acc_2       = reg_det_rcv.precio_acc_2
            LET reg_rcv.cve_subcta_3       = reg_det_rcv.cve_subcta_3
            LET reg_rcv.prctj_subc_3       = reg_det_rcv.prctj_subc_3
            LET reg_rcv.saldo_subc_3       = reg_det_rcv.import_sub_3
            LET reg_rcv.no_tot_acc_3       = reg_det_rcv.no_tot_acc_3
            LET reg_rcv.siefore_3          = reg_det_rcv.siefore_3
            LET reg_rcv.precio_acc_3       = reg_det_rcv.precio_acc_3
            LET reg_rcv.cve_subcta_4       = reg_det_rcv.cve_subcta_4
            LET reg_rcv.prctj_subc_4       = reg_det_rcv.prctj_subc_4
            LET reg_rcv.saldo_subc_4       = reg_det_rcv.import_sub_4
            LET reg_rcv.no_tot_acc_4       = reg_det_rcv.no_tot_acc_4
            LET reg_rcv.siefore_4          = reg_det_rcv.siefore_4
            LET reg_rcv.precio_acc_4       = reg_det_rcv.precio_acc_4
            LET reg_rcv.cve_subcta_5       = reg_det_rcv.cve_subcta_5
            LET reg_rcv.prctj_subc_5       = reg_det_rcv.prctj_subc_5
            LET reg_rcv.saldo_subc_5       = reg_det_rcv.import_sub_5
            LET reg_rcv.no_tot_acc_5       = reg_det_rcv.no_tot_acc_5
            LET reg_rcv.siefore_5          = reg_det_rcv.siefore_5
            LET reg_rcv.precio_acc_5       = reg_det_rcv.precio_acc_5
            LET reg_rcv.cve_subcta_6       = reg_det_rcv.cve_subcta_6
            LET reg_rcv.prctj_subc_6       = reg_det_rcv.prctj_subc_6
            LET reg_rcv.saldo_subc_6       = reg_det_rcv.import_sub_6
            LET reg_rcv.no_tot_acc_6       = reg_det_rcv.no_tot_acc_6
            LET reg_rcv.siefore_6          = reg_det_rcv.siefore_6
            LET reg_rcv.precio_acc_6       = reg_det_rcv.precio_acc_6
            LET reg_rcv.cve_subcta_7       = reg_det_rcv.cve_subcta_7
            LET reg_rcv.prctj_subc_7       = reg_det_rcv.prctj_subc_7
            LET reg_rcv.saldo_subc_7       = reg_det_rcv.import_sub_7
            LET reg_rcv.no_tot_acc_7       = reg_det_rcv.no_tot_acc_7
            LET reg_rcv.siefore_7          = reg_det_rcv.siefore_7
            LET reg_rcv.precio_acc_7       = reg_det_rcv.precio_acc_7
            LET reg_rcv.cve_subcta_8       = reg_det_rcv.cve_subcta_8
            LET reg_rcv.prctj_subc_8       = reg_det_rcv.prctj_subc_8
            LET reg_rcv.saldo_subc_8       = reg_det_rcv.import_sub_8
            LET reg_rcv.no_tot_acc_8       = reg_det_rcv.no_tot_acc_8
            LET reg_rcv.siefore_8          = reg_det_rcv.siefore_8
            LET reg_rcv.precio_acc_8       = reg_det_rcv.precio_acc_8
            LET reg_rcv.cod_result_operac  = reg_det_viv.cod_result_operac
            LET reg_rcv.diag_proceso       = reg_det_viv.diag_proceso
            LET reg_rcv.periodo_ult_aport  = reg_det_rcv.periodo_ult_apor
            LET reg_rcv.ultimo_sdi         = reg_det_rcv.ultimo_sal_dia
            LET reg_rcv.nombre_imss        = ""
            LET reg_rcv.estado             = 1

            LET cfecha_pat = reg_det_rcv.fecha_vol_pat[5,6],"/",
                             reg_det_rcv.fecha_vol_pat[7,8],"/",
                             reg_det_rcv.fecha_vol_pat[1,4]

            LET vfecha_pat = cfecha_pat

            LET reg_rcv.fecha_vol_pat = vfecha_pat

            LET cfecha_ven = reg_det_rcv.fecha_vol_ven[5,6],"/",
                             reg_det_rcv.fecha_vol_ven[7,8],"/",
                             reg_det_rcv.fecha_vol_ven[1,4]

            LET vfecha_ven = cfecha_ven

            LET reg_rcv.fecha_vol_ven = vfecha_ven

            SELECT "X"
              FROM taa_rcv_recepcion
             WHERE @folio = vfolio
               AND @nss = reg_rcv.nss
               AND @fecha_mov_banxico = v_banxico
               AND @cont_servicio = reg_rcv.cont_servicio

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO taa_rcv_recepcion VALUES(reg_rcv.*)
            END IF
        END FOREACH

        IF (id_op = 3)  OR
           (id_op = 31) THEN
            SELECT t.tipo_solicitud, t.docto_cod
              INTO v_tipo_solicitud, reg_carta.docto_cod
              FROM tab_tipo_traspaso t
             WHERE t.tipo_traspaso = reg_viv.tipo_traspaso
               AND t.modulo_cod    = 'taa'
               AND t.id_opera      = reg_cza_tras.ident_operacion

            IF reg_carta.docto_cod IS NOT NULL OR
               reg_carta.docto_cod <> ' ' THEN

                CALL det_carta(v_tipo_solicitud)
            END IF

        END IF

        LET cont_reg = cont_reg + 1

    END FOREACH

    SELECT SUM(tot_sal_viv97 +
               tot_sal_viv92 +
               tot_sal_issste +
               tot_sal_issste_08),
           SUM(tot_subcta_1  +
               tot_subcta_2  +
               tot_subcta_3  +
               tot_subcta_4  +
               tot_subcta_5  +
               tot_subcta_6  +
               tot_subcta_7  +
               tot_subcta_8  +
               tot_subcta_9  +
               tot_subcta_10 +
               tot_subcta_11 +
               tot_subcta_12 +
               tot_subcta_13 +
               tot_subcta_14 +
               tot_subcta_15 +
               tot_subcta_16 +
               tot_subcta_17 +
               tot_subcta_18 +
               tot_subcta_19 )
    INTO   reg_pagos.impt_acept_viv,
           reg_pagos.impt_acept_rcv
    FROM   safre_tmp:sum_tra_afo

    LET reg_pagos.folio              = vfolio
    LET reg_pagos.tipo_registro      = "25"
    LET reg_pagos.ident_servicio     = reg_cza_tras.ident_servicio
    LET reg_pagos.ident_pago         = "             57"
    LET reg_pagos.fecha_liquidacion  = reg_rcv.fecha_mov_banxico
    LET reg_pagos.importe            = reg_pagos.impt_acept_viv +
                                       reg_pagos.impt_acept_rcv
    LET reg_pagos.impt_aport_dev     = 0
    LET reg_pagos.estado             = 1
    LET reg_pagos.fecha_archivo      = reg_cza_tras.fecha_presentacion

    SELECT "X"
    FROM   taa_recepcion_af
    WHERE  folio = vfolio
    AND    fecha_liquidacion = v_banxico

    IF SQLCA.SQLCODE <> 0 THEN
        INSERT INTO taa_recepcion_af
        VALUES(reg_pagos.folio,
               reg_pagos.tipo_registro,
               reg_pagos.ident_servicio,
               reg_pagos.ident_pago,
               reg_pagos.importe,
               v_banxico,
               reg_pagos.impt_acept_rcv,
               reg_pagos.impt_acept_viv,
               reg_pagos.impt_aport_dev,
               reg_pagos.estado,
               reg_pagos.fecha_archivo,
               TODAY)
    END IF

    SELECT *
    INTO   reg_sum_tras.*
    FROM   safre_tmp:sum_tra_afo

    SELECT "X"
    FROM   taa_sum_recepcion
    WHERE  folio = vfolio
    AND    cant_reg_det = reg_sum_tras.cant_reg_det

    IF SQLCA.SQLCODE <> 0 THEN
        INSERT INTO taa_sum_recepcion VALUES(vfolio, reg_sum_tras.*,1)
    END IF

END FUNCTION

{FUNCTION fechas(diaActual)

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE,
        numDias   SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO 4
        IF contador = 1 THEN
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        ELSE
            LET diaTmp = diaTmp + 1 UNITS DAY
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        END IF
    END FOR

    RETURN diaTmp

END FUNCTION

FUNCTION habil_siguiente(diaActual)

    DEFINE
        diaTmp      DATE,
        contador    SMALLINT,
        diaActual   DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
        ELSE
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION}

FUNCTION actualiza_operacion()

    UPDATE bat_ctr_operacion
    SET    estado_cod       = 4,
           fecha_fin        = CURRENT,
           folio            = vfolio
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    estado_proceso = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod

END FUNCTION

FUNCTION det_carta(tipo_sol)

   DEFINE fent     DATE
   DEFINE tipo_sol SMALLINT

   SELECT @tipo_solicitud, max(@fentcons)
     INTO reg_carta.tipo_solicitud, fent
     FROM afi_solicitud
    WHERE @n_seguro = reg_viv.nss
      AND @tipo_solicitud = tipo_sol
      AND @status_interno in(50,70)
    GROUP BY 1

   IF w_codigo_afore = 568 THEN --CPL
      ---- 1048 -> Para el traspaso 57 el tipo de solicitud puede ser 9 u 11
      ---- De entrada -tipo_sol- trae 11
         IF SQLCA.SQLCODE <> 0  THEN
           IF  reg_viv.tipo_traspaso = 57  THEN
              LET  tipo_sol = 9

              SELECT @tipo_solicitud, max(@fentcons)
              INTO reg_carta.tipo_solicitud, fent
              FROM afi_solicitud
              WHERE @n_seguro = reg_viv.nss
              AND @tipo_solicitud = tipo_sol
              AND @status_interno in(50,70)
              GROUP BY 1
           END IF
         END IF
      ---- 1048 <-
   END IF

   INITIALIZE reg_carta.n_folio TO NULL

   SELECT @n_folio
     INTO reg_carta.n_folio
     FROM afi_solicitud
    WHERE @n_seguro       = reg_viv.nss
      AND @tipo_solicitud = tipo_sol
      AND @fentcons       = fent
      AND @status_interno in(50,70)

   #OP13 PLUS
   #TODO LO DE ARRIBA ES PARA OBTENER N_FOLIO Y TIPO SOLICITUD
   IF reg_carta.n_folio IS NULL THEN
      SELECT n_folio       ,
             tipo_solicitud
      INTO   reg_carta.n_folio,
             reg_carta.tipo_solicitud
      FROM   afi_mae_afiliado
      WHERE  n_seguro = reg_viv.nss
   END IF

   LET reg_carta.nss            = reg_viv.nss --YA
   LET reg_carta.fecha_registro = v_banxico   --YA
   LET reg_carta.opera_cod      = NULL        --YA
   LET reg_carta.edo_genera     = 10          --YA
   LET reg_carta.fecha_genera   = TODAY       --YA
   LET reg_carta.hora_genera    = TIME        --YA
   LET reg_carta.lote_genera    = 0           --YA
   LET reg_carta.consecutivo    = 0           --YA
   LET reg_carta.id_sepomex     = 0           --YA


   LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
                        "?,?,?,?,?,?)"
   PREPARE sql_exe FROM consulta_carta
   EXECUTE sql_exe USING reg_carta.*

   INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION actualiza_afiliados()

    DEFINE reg_nss RECORD
        nss    CHAR(11),
        afo    CHAR(3) ,
        tipo_trasp SMALLINT                                           #1038
    END RECORD

    DEFINE vn_folio          DECIMAL(10,0)
    DEFINE vtipo_sol         SMALLINT

    DECLARE cursor_aa CURSOR FOR
    SELECT nss, cve_ced_cuenta,tipo_traspaso                          #1038
    FROM   taa_rcv_recepcion
    WHERE  folio = vfolio
    
    #OP13 PLUS
    #AL NO ENCONTRAR LOS DATOS EN EL FOREACH NO HAY IMPACTO

    FOREACH cursor_aa INTO reg_nss.*
        DECLARE cur_afi_sol CURSOR FOR
            SELECT a.n_folio, a.tipo_solicitud
              FROM afi_solicitud a
             WHERE n_seguro = reg_nss.nss
               AND status_interno IN(65,70,50)
             ORDER BY frecafor DESC, fentcons DESC

        FOREACH cur_afi_sol INTO vn_folio, vtipo_sol
            IF w_codigo_afore = 568 THEN --CPL
               IF  reg_nss.tipo_trasp   <> 73  AND   reg_nss.tipo_trasp   <>21 THEN                        #1038 	#CPL-1229
                  UPDATE afi_solicitud
                     SET status_interno = 100                              #1038 = 75
                         --finicta        = v_banxico
                   WHERE n_seguro       = reg_nss.nss
                     AND n_folio        = vn_folio
                     AND tipo_solicitud = vtipo_sol
                     AND status_interno in(65,70,50)
                     EXIT FOREACH                                           #1038
                ELSE
                 UPDATE afi_solicitud
                     SET status_interno = 75
                       --  finicta        = v_banxico		#1142
                   WHERE n_seguro       = reg_nss.nss
                     AND n_folio        = vn_folio
                     AND tipo_solicitud = vtipo_sol
                     AND status_interno in(65,70,50)
                     EXIT FOREACH
               END IF
            ELSE
               UPDATE afi_solicitud
                  SET status_interno = 75,
                      finicta        = v_banxico
                WHERE n_seguro       = reg_nss.nss
                  AND n_folio        = vn_folio
                  AND tipo_solicitud = vtipo_sol
                  AND status_interno in(65,70,50)

               EXIT FOREACH
            END IF
        END FOREACH

        IF w_codigo_afore <> 568 THEN --CPL
           UPDATE afi_solicitud
              SET status_interno = 45,
                  finicta        = v_banxico
            WHERE n_seguro       = reg_nss.nss
              AND status_interno in(65,70,50)
        ELSE
        	 UPDATE afi_solicitud
              SET status_interno = 45
                  --finicta        = v_banxico #1142
            WHERE n_seguro       = reg_nss.nss
              AND status_interno in(65,70,50)
        END IF
    END FOREACH
END FUNCTION
