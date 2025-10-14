#######################################################################
#Proyecto          => AFORES ( MEXICO )                               #
#Propietario       => E.F.P.                                          #
#Programa TAAB0231 => INCORPORAR REGISTROS VIRTUAL DE APORTACIONES    #
#Fecha             => 13 DE FEBRERO DE 2009                           #
#Autor             => FERNANDO HERRERA HERNANDEZ                      #
#Sistema           => TRA                                             #
#######################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        cza RECORD LIKE taa_cza_recepcion.*,
        rcv RECORD LIKE taa_rcv_recepcion.*,
        viv RECORD LIKE taa_viv_recepcion.*,
        sum RECORD LIKE taa_sum_recepcion.*

    DEFINE
        dia_liq  ,
        HOY      DATE

    DEFINE 
        cza_id_op CHAR(02) ,
        aportante CHAR(06) ,
        HORA      CHAR(10) ,
        usuario   CHAR(08) ,
        comma     CHAR(50)

    DEFINE
        vfolio          INTEGER

    DEFINE g_lista CHAR(100)

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

END GLOBALS

MAIN

    LET vfolio = ARG_VAL(1)

    CALL STARTLOG("TAAB0231.log")
    CALL inicio()
    CALL llena_rcv()
    CALL proceso_principal()
    CALL rpt_vol()

END MAIN

FUNCTION inicio()
#i---------------

    LET HORA = TIME
    LET HOY  = TODAY

    SELECT *, USER
    INTO   g_param_taa.*, usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET g_lista = g_param_taa.ruta_listados CLIPPED, "/",usuario CLIPPED,
                  ".rpt_vol.",HOY using "DDMMYY"

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE 
        bandera SMALLINT,
        vnss    CHAR(11)

    DEFINE
        TODOS1               INTEGER

    DEFINE reg_rcv RECORD
        folio                INTEGER      ,
        cont_servicio        DECIMAL(10,0),
        cve_ced_cuenta       CHAR(3)      ,
        tipo_traspaso        SMALLINT     ,
        fecha_mov_banxico    DATE         ,
        nss                  CHAR(11)     ,
        cve_subcta           CHAR(2)      ,
        prctj_subc           DECIMAL(16,6),
        saldo_subc           DECIMAL(15,2),
        no_tot_acc           DECIMAL(16,6),
        siefore              CHAR(8)      ,
        precio_acc           DECIMAL(15,6)
    END RECORD

    LET bandera = 0

    SELECT cr.*
    INTO   cza.*
    FROM   taa_cza_recepcion cr
    WHERE  folio = vfolio

    IF cza.ident_operacion = '09' THEN
        CALL traspasa_maestro()
    END IF
    
    UPDATE STATISTICS FOR TABLE afi_mae_af_virt
    UPDATE STATISTICS FOR TABLE cta_regimen_virt

    SELECT fecha_mov_banxico
    INTO   dia_liq
    FROM   taa_rcv_recepcion
    WHERE  folio  = vfolio
    AND    tipo_registro = "25"
    GROUP BY 1

    DECLARE cursor_v CURSOR FOR
    SELECT HV.*
      FROM taa_viv_recepcion HV
     WHERE HV.folio = vfolio

    FOREACH cursor_v INTO viv.*

      IF cza.ident_operacion = '09' THEN
         SELECT A.n_seguro
           INTO vnss
           FROM afi_mae_af_virt A
          WHERE A.n_seguro = viv.nss
 
         IF STATUS = NOTFOUND THEN
             LET bandera = 1
         ELSE
             LET bandera = 0
         END IF
      END IF

      IF cza.ident_operacion = '12' THEN
         SELECT A.n_seguro
           INTO vnss
           FROM afi_mae_afiliado A
          WHERE A.n_seguro = viv.nss
 
         IF STATUS = NOTFOUND THEN
             LET bandera = 1
         ELSE
             LET bandera = 0
         END IF
      END IF
 
        IF bandera = 0 THEN

            CALL dispersa(reg_rcv.*,viv.*,1)

            DECLARE cursor_r CURSOR FOR
             SELECT HR.*
               FROM recep_rcv HR
              WHERE HR.folio          = vfolio
                AND HR.nss            = viv.nss
                AND HR.cve_ced_cuenta = viv.cve_ced_cuenta

            FOREACH cursor_r INTO reg_rcv.*
                IF reg_rcv.cve_subcta IS NOT NULL THEN
                    CALL dispersa(reg_rcv.*,viv.*,2)
                END IF
            END FOREACH
        END IF

    END FOREACH

    UPDATE STATISTICS FOR TABLE dis_provision_virt

END FUNCTION

FUNCTION dispersa(x_historico,x_viv,it)
#d-------------------------------------

    DEFINE
        x_viv       RECORD LIKE taa_viv_recepcion.*,
        g_sie       RECORD LIKE cta_regimen.*,
        it          SMALLINT,
        precio_rcv  DECIMAL(15,6),
        precio_viv  DECIMAL(18,14),
        sdo_acc_viv DECIMAL(18,2),
        sdo_acc_fov DECIMAL(18,6),
        sdo_impt_b  DECIMAL(18,4)

    DEFINE
        txt            CHAR(100)

    DEFINE
        vsubcta   ,
        i         ,
        j         SMALLINT

    DEFINE
        valor_en_pesos  DECIMAL(18,6),
        vpesos_cta      DECIMAL(18,2)

    DEFINE reg_prov RECORD LIKE dis_provision.*

    DEFINE x_historico RECORD
        folio                INTEGER      ,
        cont_servicio        DECIMAL(10,0),
        cve_ced_cuenta       CHAR(3)      ,
        tipo_traspaso        SMALLINT     ,
        fecha_mov_banxico    DATE         ,
        nss                  CHAR(11)     ,
        cve_subcta           CHAR(2)      ,
        prctj_subc           DECIMAL(16,6),
        saldo_subc           DECIMAL(15,2),
        no_tot_acc           DECIMAL(16,6),
        siefore              CHAR(8)      ,
        precio_acc           DECIMAL(15,6)
    END RECORD

    IF cza.ident_operacion = '09' THEN
       LET txt = " SELECT * ",
                 " FROM   cta_regimen_virt cr ",
                 " WHERE  cr.nss = ","'",x_viv.nss,"'",
                 " AND    cr.subcuenta   = ?"
    ELSE
       LET txt = " SELECT * ",
                 " FROM   cta_regimen cr ",
                 " WHERE  cr.nss = ","'",x_viv.nss,"'",
                 " AND    cr.subcuenta   = ?"
    END IF

    PREPARE cla_exe FROM txt

    LET reg_prov.estado          = 5
    LET reg_prov.tipo_movimiento = 1
    LET reg_prov.folio           = x_viv.folio
    LET reg_prov.nss             = x_viv.nss
    LET reg_prov.curp            = ''
    LET reg_prov.folio_sua       = ''
    LET reg_prov.dias_cotizados  = 0
    LET reg_prov.sucursal        = ''
    LET reg_prov.fecha_proceso   = TODAY
    LET reg_prov.usuario         = usuario
    LET reg_prov.fecha_archivo   = cza.fecha_presentacion
    LET reg_prov.etiqueta        = 0

##### RECUPERA id_aportante del catalago de tipo de traspasos TAA Receptora
    SELECT tt.id_aportante
      INTO reg_prov.id_aportante
      FROM tab_tipo_traspaso tt
     WHERE tt.tipo_traspaso = viv.tipo_traspaso
       AND tt.modulo_cod    = 'taa'
       AND tt.id_opera      = cza.ident_operacion

    LET reg_prov.id_aportante = reg_prov.id_aportante CLIPPED, x_viv.cve_ced_cuenta CLIPPED

#####-RECALCULA FECHA VALOR
    LET reg_prov.fecha_pago       = x_viv.fecha_mov_banxico
    LET reg_prov.fecha_valor      = x_viv.fecha_mov_banxico
    LET reg_prov.fecha_conversion = x_viv.fecha_mov_banxico

    IF it = 1 THEN
        LET reg_prov.fecha_valor = MDY(MONTH(reg_prov.fecha_valor),
                                       1,YEAR(reg_prov.fecha_valor))

        FOR i = 1 TO 5
            CASE i
                WHEN 1
                    LET reg_prov.subcuenta = 4
                    LET valor_en_pesos     = 0
                    LET vpesos_cta         = 0
                    LET sdo_acc_viv        = 0

                    DECLARE cursor_4 CURSOR FOR cla_exe

                    FOREACH cursor_4 USING reg_prov.subcuenta INTO g_sie.*
                        SELECT precio_del_dia
                          INTO precio_viv
                          FROM glo_valor_accion
                         WHERE fecha_valuacion = reg_prov.fecha_valor
                           AND codigo_siefore  = g_sie.codigo_siefore

                        LET sdo_acc_viv                = x_viv.partic_viv97
                        LET valor_en_pesos             = x_viv.partic_viv97
                                                         * precio_viv
                        LET vpesos_cta                 = valor_en_pesos

                        LET reg_prov.monto_en_acciones = sdo_acc_viv
                        LET reg_prov.monto_en_pesos    = vpesos_cta
                        LET reg_prov.siefore           = g_sie.codigo_siefore
                        LET reg_prov.precio_accion     = precio_viv
                    END FOREACH
                WHEN 2
                    LET reg_prov.subcuenta = 8
                    LET valor_en_pesos     = 0
                    LET vpesos_cta         = 0
                    LET sdo_acc_viv        = 0

                    DECLARE cursor_8 CURSOR FOR cla_exe

                    FOREACH cursor_8 USING reg_prov.subcuenta INTO g_sie.*
                        SELECT precio_del_dia
                          INTO precio_viv
                          FROM glo_valor_accion
                         WHERE fecha_valuacion = reg_prov.fecha_valor
                           AND codigo_siefore  = g_sie.codigo_siefore

                        LET sdo_acc_viv                = x_viv.partic_viv92
                        LET valor_en_pesos             = x_viv.partic_viv92
                                                         * precio_viv
                        LET vpesos_cta                 = valor_en_pesos

                        LET reg_prov.monto_en_acciones = sdo_acc_viv
                        LET reg_prov.monto_en_pesos    = vpesos_cta
                        LET reg_prov.siefore           = g_sie.codigo_siefore
                        LET reg_prov.precio_accion     = precio_viv
                    END FOREACH
                WHEN 3
                    LET reg_prov.subcuenta         = 14
                    LET valor_en_pesos             = 0
                    LET vpesos_cta                 = 0
                    LET sdo_acc_viv                = 0

                    DECLARE cursor_9 CURSOR FOR cla_exe

                    FOREACH cursor_9 USING reg_prov.subcuenta INTO g_sie.*
                        SELECT precio_del_dia
                          INTO precio_viv
                          FROM glo_valor_accion
                         WHERE fecha_valuacion = reg_prov.fecha_valor
                           AND codigo_siefore  = g_sie.codigo_siefore

                        LET sdo_acc_fov                = x_viv.aivs_issste
                        LET valor_en_pesos             = x_viv.aivs_issste
                                                         * precio_viv
                        LET vpesos_cta                 = valor_en_pesos

                        LET reg_prov.monto_en_acciones = sdo_acc_fov
                        LET reg_prov.monto_en_pesos    = vpesos_cta
                        LET reg_prov.siefore           = g_sie.codigo_siefore
                        LET reg_prov.precio_accion     = precio_viv
                    END FOREACH
                WHEN 4
                    LET reg_prov.subcuenta         = 35
                    LET valor_en_pesos             = 0
                    LET vpesos_cta                 = 0
                    LET sdo_acc_viv                = 0

                    DECLARE cursor_10 CURSOR FOR cla_exe

                    FOREACH cursor_10 USING reg_prov.subcuenta INTO g_sie.*
                        SELECT precio_del_dia
                          INTO precio_viv
                          FROM glo_valor_accion
                         WHERE fecha_valuacion = reg_prov.fecha_valor
                           AND codigo_siefore  = g_sie.codigo_siefore

                        LET sdo_acc_fov                = x_viv.aivs_issste_08
                        LET valor_en_pesos             = x_viv.aivs_issste_08
                                                         * precio_viv
                        LET vpesos_cta                 = valor_en_pesos

                        LET reg_prov.monto_en_acciones = sdo_acc_fov
                        LET reg_prov.monto_en_pesos    = vpesos_cta
                        LET reg_prov.siefore           = g_sie.codigo_siefore
                        LET reg_prov.precio_accion     = precio_viv
                    END FOREACH
                    
                WHEN 5
                    LET reg_prov.subcuenta         = 36
                    LET valor_en_pesos             = 0
                    LET vpesos_cta                 = 0
                    LET sdo_impt_b                 = 0

                    DECLARE cursor_11 CURSOR FOR cla_exe

                    FOREACH cursor_11 USING reg_prov.subcuenta INTO g_sie.*
                        SELECT precio_del_dia
                          INTO precio_rcv
                          FROM glo_valor_accion
                         WHERE fecha_valuacion = x_viv.fecha_mov_banxico
                           AND codigo_siefore  = g_sie.codigo_siefore

                        LET sdo_impt_b                 = x_viv.importe_bono
                        LET valor_en_pesos             = x_viv.importe_bono
                                                         * precio_rcv
                        LET vpesos_cta                 = valor_en_pesos

                        LET reg_prov.monto_en_acciones = sdo_impt_b
                        LET reg_prov.monto_en_pesos    = vpesos_cta
                        LET reg_prov.siefore           = g_sie.codigo_siefore
                        LET reg_prov.precio_accion     = precio_rcv
                    END FOREACH                    

            END CASE

            LET reg_prov.consecutivo_lote = x_viv.cont_servicio

            IF reg_prov.monto_en_pesos IS NOT NULL AND
               reg_prov.monto_en_pesos > 0 THEN
                INSERT INTO dis_provision_virt VALUES(reg_prov.*)

                IF STATUS < 0 THEN
                    ERROR "Error al insertar provision virtual afore receptora ",STATUS
                END IF
            END IF
        END FOR
    END IF

    #-------RECALCULA VALORES ACTUALES
    
    LET reg_prov.monto_en_acciones = 0
    LET reg_prov.monto_en_pesos    = 0
    
    #####-RECALCULA FECHA VALOR
    LET reg_prov.fecha_pago       = TODAY -1
    LET reg_prov.fecha_valor      = TODAY -1
    LET reg_prov.fecha_conversion = TODAY -1

    IF it = 2 THEN
        SELECT @subcuenta
          INTO vsubcta
          FROM tab_subcta_taa
         WHERE sub_taa = x_historico.cve_subcta

        DECLARE cursor_1 CURSOR FOR cla_exe

        FOREACH cursor_1 USING vsubcta INTO g_sie.*
            SELECT @precio_del_dia
              INTO reg_prov.precio_accion
              FROM glo_valor_accion
             WHERE @fecha_valuacion = TODAY -1
               AND @codigo_siefore  = g_sie.codigo_siefore

            SELECT @precio_del_dia
              INTO precio_rcv
              FROM taa_accion_siefore
             WHERE @fecha_valuacion = x_historico.fecha_mov_banxico
               AND @desc_siefore    = x_historico.siefore

            LET reg_prov.monto_en_pesos    = x_historico.saldo_subc
                                             
            LET reg_prov.monto_en_acciones = reg_prov.monto_en_pesos /
                                             reg_prov.precio_accion

            LET reg_prov.consecutivo_lote = x_historico.cont_servicio
            LET reg_prov.subcuenta        = vsubcta
            LET reg_prov.siefore          = g_sie.codigo_siefore

            IF reg_prov.monto_en_acciones IS NOT NULL AND
               reg_prov.monto_en_acciones > 0 THEN
                INSERT INTO dis_provision_virt VALUES(reg_prov.*)

                IF STATUS < 0 THEN
                    ERROR "Error al insertar provision virtual afore receptora ",STATUS
                END IF
            END IF
        END FOREACH
    END IF

END FUNCTION

FUNCTION traspasa_maestro()

    LET comma = "fglgo TAAB024.4gi ", 1, vfolio
    DISPLAY comma
    RUN comma

END FUNCTION

FUNCTION llena_rcv()

  DEFINE habilitar_ix1  CHAR(10000)
  DEFINE habilitar_ix2  CHAR(10000)
  DEFINE habilitar_ix3  CHAR(10000)
  DEFINE habilitar_ix4  CHAR(10000)
  DEFINE habilitar_ix5  CHAR(10000)
  DEFINE habilitar_ix6  CHAR(10000)

    CREATE TEMP TABLE recep_rcv
        (folio                INTEGER      ,
         cont_servicio        DECIMAL(10,0),
         cve_ced_cuenta       CHAR(3)      ,
         tipo_traspaso        SMALLINT     ,
         fecha_mov_banxico    DATE         ,
         nss                  CHAR(11)     ,
         cve_subcta           CHAR(2)      ,
         prctj_subc           DECIMAL(16,6),
         saldo_subc           DECIMAL(15,2),
         no_tot_acc           DECIMAL(16,6),
         siefore              CHAR(8)      ,
         precio_acc           DECIMAL(15,6))

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_1     ,
           prctj_subc_1     ,
           saldo_subc_1     ,
           no_tot_acc_1     ,
           siefore_1        ,
           precio_acc_1
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_1 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_2     ,
           prctj_subc_2     ,
           saldo_subc_2     ,
           no_tot_acc_2     ,
           siefore_2        ,
           precio_acc_2
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_2 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_3     ,
           prctj_subc_3     ,
           saldo_subc_3     ,
           no_tot_acc_3     ,
           siefore_3        ,
           precio_acc_3
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_3 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_4     ,
           prctj_subc_4     ,
           saldo_subc_4     ,
           no_tot_acc_4     ,
           siefore_4        ,
           precio_acc_4
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_4 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_5     ,
           prctj_subc_5     ,
           saldo_subc_5     ,
           no_tot_acc_5     ,
           siefore_5        ,
           precio_acc_5
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_5 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_6     ,
           prctj_subc_6     ,
           saldo_subc_6     ,
           no_tot_acc_6     ,
           siefore_6        ,
           precio_acc_6
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_6 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_7     ,
           prctj_subc_7     ,
           saldo_subc_7     ,
           no_tot_acc_7     ,
           siefore_7        ,
           precio_acc_7
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_7 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           cve_subcta_8     ,
           prctj_subc_8     ,
           saldo_subc_8     ,
           no_tot_acc_8     ,
           siefore_8        ,
           precio_acc_8
      FROM taa_rcv_recepcion
     WHERE folio = vfolio
       AND cve_subcta_8 IS NOT NULL

    INSERT INTO recep_rcv
    SELECT folio            ,
           cont_servicio    ,
           cve_ced_cuenta   ,
           tipo_traspaso    ,
           fecha_mov_banxico,
           nss              ,
           97               ,
           100              ,
           0                ,
           1                ,
           11               ,
           0
      FROM taa_viv_recepcion
     WHERE folio = vfolio
       AND nss NOT IN(SELECT UNIQUE nss
                        FROM recep_rcv)

    DELETE FROM recep_rcv
    WHERE  no_tot_acc = 0
    OR     no_tot_acc IS NULL

    CREATE INDEX recep_rcv1
    ON recep_rcv(folio)

    CREATE INDEX recep_rcv2
    ON recep_rcv(nss)

    UPDATE STATISTICS FOR TABLE recep_rcv
    
    WHENEVER ERROR CONTINUE
    drop table afi_mae_af_virt;
    create table afi_mae_af_virt
      (
        n_seguro char(11) not null ,
        n_unico char(18),
        n_rfc char(13),
        fena date,
        n_folio decimal(10,0),
        frecafor date,
        tipo_solicitud smallint,
        fecha_elaboracion date,
        ind_edad smallint,
        fecha_edad date,
        criterio_edad smallint
      ) in afi_dbs1;
    
    drop table cta_solicitud_regimen_virt;
    create table cta_solicitud_regimen_virt
      (
        nss char(11),
        fecha_solicitud date,
        folio_solicitud serial not null ,
        medio char(2),
        fecha_inicio datetime year to second,
        fecha_fin datetime year to second,
        estado smallint,
        tipo_edad smallint,
        criterio smallint,
        ind_decimos smallint,
        tipo_proceso smallint
      ) in cta_dbs1;
    
    drop table cta_regimen_virt;
    create table cta_regimen_virt
      (
        nss char(11) not null ,
        subcuenta smallint not null ,
        codigo_siefore smallint not null ,
        porcentaje decimal(5,2) not null 
      )in cta_dbs1;
    
    drop table cta_nss_regimen_virt;
    create table cta_nss_regimen_virt
      (
        nss char(11),
        grupo_regimen smallint,
        codigo_siefore smallint,
        usuario char(8),
        factualiza date
      ) in cta_dbs1;
    
    drop table cta_his_edad_virt;
    create table cta_his_edad_virt
      (
        nss char(11),
        ind_edad smallint,
        fecha_ind_edad date,
        criterio_edad smallint,
        usuario char(8),
        factualiza date
      ) in cta_dbs1;
    
    drop table cta_his_regimen_virt;
    create table cta_his_regimen_virt
      (
        nss char(11) not null ,
        subcuenta smallint not null ,
        codigo_siefore smallint not null ,
        porcentaje decimal(5,2) not null ,
        usuario char(8),
        factualiza date
      ) in cta_dbs1;
    
    drop table cta_sol_regimen_total_virt;
    create table cta_sol_regimen_total_virt
      (
        folio integer,
        fecha_solicitud date,
        nss char(11),
        ind_edad smallint,
        grupo_regimen smallint,
        recha_cod smallint,
        tipo_traspaso smallint
      ) in cta_dbs1;
    
    drop table dis_provision_virt;
    create table dis_provision_virt
      (
        tipo_movimiento smallint not null ,
        subcuenta smallint not null ,
        siefore smallint,
        folio decimal(10,0) not null ,
        consecutivo_lote integer,
        nss char(11) not null ,
        curp char(18),
        folio_sua char(6),
        fecha_pago date,
        fecha_valor date,
        fecha_conversion date,
        monto_en_pesos decimal(16,6),
        monto_en_acciones decimal(16,6),
        precio_accion decimal(16,6),
        dias_cotizados integer,
        sucursal char(10),
        id_aportante char(11),
        estado smallint,
        fecha_proceso date,
        usuario char(8),
        fecha_archivo date,
        etiqueta integer
      )in cta_dbs1;

    WHENEVER ERROR STOP

    LET habilitar_ix1 = 
    "create index afi_mae_af_v1 on afi_mae_af_virt (n_seguro) in afi_dbs1; ",
    "create index cta_sol_reg_v1 on cta_solicitud_regimen_virt (folio_solicitud) in cta_dbs1; "

    LET habilitar_ix1 = habilitar_ix1 CLIPPED

    PREPARE eje_hab1 FROM habilitar_ix1
    EXECUTE eje_hab1

    LET habilitar_ix4 =
    "create index dis_provision_v1 on dis_provision_virt (folio,subcuenta,tipo_movimiento,estado) in cta_dbs1; "

    LET habilitar_ix4 = habilitar_ix4 CLIPPED

    PREPARE eje_hab4 FROM habilitar_ix4
    EXECUTE eje_hab4

    LET habilitar_ix2 =
    "create index cta_sol_reg_v2 on cta_solicitud_regimen_virt (nss) in cta_dbs1; ",
    "create index cta_regimen_virt1 on cta_regimen_virt (nss,codigo_siefore,subcuenta) in cta_dbs1; "

    LET habilitar_ix2 = habilitar_ix2 CLIPPED

    PREPARE eje_hab2 FROM habilitar_ix2
    EXECUTE eje_hab2

    LET habilitar_ix5 =
    "create index dis_provision_v2 on dis_provision_virt (nss) in cta_dbs1; "

    LET habilitar_ix5 = habilitar_ix5 CLIPPED

    PREPARE eje_hab5 FROM habilitar_ix5
    EXECUTE eje_hab5

    LET habilitar_ix3 =
    "create index cta_nss_regimen_v1 on cta_nss_regimen_virt (nss,grupo_regimen) in cta_dbs1; ",
    "create index cta_his_reg_v2 on cta_his_regimen_virt (nss,subcuenta,factualiza) in cta_dbs1; "

    LET habilitar_ix3 = habilitar_ix3 CLIPPED

    PREPARE eje_hab3 FROM habilitar_ix3
    EXECUTE eje_hab3

    LET habilitar_ix6 =
    "create index dis_provision_v3 on dis_provision_virt (folio,subcuenta) in cta_dbs1; "

    LET habilitar_ix6 = habilitar_ix6 CLIPPED

    PREPARE eje_hab6 FROM habilitar_ix6
    EXECUTE eje_hab6

    UPDATE STATISTICS FOR TABLE afi_mae_af_virt
    UPDATE STATISTICS FOR TABLE cta_solicitud_regimen_virt
    UPDATE STATISTICS FOR TABLE cta_regimen_virt
    UPDATE STATISTICS FOR TABLE cta_nss_regimen_virt
    UPDATE STATISTICS FOR TABLE cta_his_edad_virt
    UPDATE STATISTICS FOR TABLE cta_his_regimen_virt
    UPDATE STATISTICS FOR TABLE cta_sol_regimen_total_virt
    UPDATE STATISTICS FOR TABLE dis_provision_virt


END FUNCTION

FUNCTION rpt_vol()
#rv---------------

    DEFINE rpt_vol RECORD
         folio                INTEGER      ,
         nss                  CHAR(11)     ,
         cve_subcta           CHAR(2)      ,
         no_tot_acc           DECIMAL(16,6)
    END RECORD

    DEFINE vfecha_vol DATE

    LET vfecha_vol = ''

    DECLARE cur_vol CURSOR FOR
    SELECT folio       ,
           nss         ,
           cve_subcta  ,
           no_tot_acc  
    FROM   recep_rcv
    WHERE  cve_subcta in('05','13')

    START REPORT listado TO g_lista

    FOREACH cur_vol INTO rpt_vol.*

        IF rpt_vol.no_tot_acc <> 0 OR
           rpt_vol.no_tot_acc IS NOT NULL THEN
            IF rpt_vol.cve_subcta = '05' THEN
                SELECT fecha_vol_ven
                INTO   vfecha_vol
                FROM   taa_rcv_recepcion
                WHERE  folio = rpt_vol.folio
                AND    nss   = rpt_vol.nss
                GROUP BY 1
            ELSE
                SELECT fecha_vol_pat
                INTO   vfecha_vol
                FROM   taa_rcv_recepcion
                WHERE  folio = rpt_vol.folio
                AND    nss   = rpt_vol.nss
                GROUP BY 1
            END IF

            IF vfecha_vol = '01/01/0001' OR
               vfecha_vol IS NULL THEN
                 OUTPUT TO REPORT listado(rpt_vol.*)
            END IF
        END IF

        LET vfecha_vol = ''
    END FOREACH

    FINISH REPORT listado

END FUNCTION

REPORT listado(rpt_vol)
#l---------------------

    DEFINE rpt_vol RECORD
         folio                INTEGER      ,
         nss                  CHAR(11)     ,
         cve_subcta           CHAR(2)      ,
         no_tot_acc           DECIMAL(16,6)
    END RECORD

    OUTPUT
        PAGE LENGTH 60
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER

    PRINT COLUMN 5,"REPORTE NSS CON APORTACIONES VOLUNTARIAS CON FECHA ERRONEA"
    PRINT
    PRINT COLUMN 5,"FOLIO     NSS          SUBCTA "
    PRINT

    ON EVERY ROW
    PRINT COLUMN 5,rpt_vol.folio USING "&&&&&&&",
          COLUMN 15, rpt_vol.nss,
          COLUMN 28, rpt_vol.cve_subcta

END REPORT

