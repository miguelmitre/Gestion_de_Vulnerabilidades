##############################################################################
#Proyecto            => SAFRE  ( MEXICO )                                    #
#Owner               => E.F.P.                                               #
#Programa CTAB0401   => ESTADO DE CUENTA                                     #
#Sistema             => CTA                                                  #
#Por                 => CESAR DAVID CHAVEZ MARTINEZ                          #
#Fecha               => 12 de febrero de 2008                                #
##############################################################################
DATABASE safre_af

GLOBALS

   DEFINE reg_gen  RECORD
          ind_siefore        CHAR(1),
          siefore            SMALLINT
   END RECORD

   DEFINE w_aux RECORD
          seguro            CHAR(11),
          codven            LIKE afi_mae_afiliado.codven,
          n_unico           CHAR(18),
          n_folio           LIKE afi_mae_afiliado.n_folio,
          tipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud,
          paterno           LIKE afi_mae_afiliado.paterno,
          materno           LIKE afi_mae_afiliado.materno,
          nombres           CHAR(120),
          rfc               CHAR(13),
          infonavit         LIKE afi_mae_afiliado.ind_infonavit,
          fentcons          LIKE afi_mae_afiliado.fentcons,
          sucursal          LIKE afi_mae_afiliado.coduni_n1,
          callep            CHAR(70),
          numep             LIKE afi_domicilio.numero,
          deptop            LIKE afi_domicilio.depto,
          coloniap          CHAR(60),
          delegap           CHAR(60),
          estadop           CHAR(60),
          codposp           CHAR(5),
          ciudadp           CHAR(60),
          fonop             CHAR(15),--LIKE afi_telefono.telefono,
          fena              LIKE afi_mae_afiliado.fena,
          nacionalidad      LIKE afi_mae_afiliado.nacionalidad
   END RECORD

   DEFINE g_reg RECORD
          nss                    CHAR(11),
          cod_postal            CHAR(5),
          folio_ini              INTEGER,
          folio_top              INTEGER,
          fecha_ini              DATE,
          fecha_top              DATE,
          estado                 SMALLINT,
          status                 SMALLINT
   END RECORD

   DEFINE enter                 CHAR(001),
          imprime               CHAR(60),
          cat                   CHAR(950)

   DEFINE i_max_folio_envio,
          i_total_reg_lote,
          pos,
          i_cont_de_reg,
          i_total_registros     INTEGER

   DEFINE g_parametro        RECORD LIKE seg_modulo.*

   DEFINE g_afore            RECORD LIKE tab_afore_local.*

   DEFINE razon_social	     CHAR(40),
          razon_social1      CHAR(40),
          vcodigo_afore      CHAR(03),
          vafore_receptora   CHAR(03),
          vcodigo_siefore    CHAR(10),
          af_domicilio       CHAR(40),
          af_colonia         CHAR(40),
          af_telefono        CHAR(40),
          af_clave           CHAR(40),
          ue_domicilio       CHAR(40),
          ue_colonia         CHAR(40),
          ue_telefono        CHAR(40),
          ue_horario         CHAR(40),
          ue_tel_grat        CHAR(40)

   DEFINE total_aporte       DECIMAL(16,6),
          aporte_obl         DECIMAL(16,6),
          aporte_vol         DECIMAL(16,6),
          ret_total          DECIMAL(16,6),
          ret_obliga         DECIMAL(16,6),
          ret_voluntario     DECIMAL(16,6),
          parametrox         CHAR(5),
          HOY                DATE,
          AYER               DATE,
          f_folio            DATE,
          hora               CHAR(8),
          g_usuario          CHAR(8),
          aux_pausa          CHAR(1),
          vestado_envio      CHAR(1),
          vcentro_reparto    CHAR(5),
          mes                SMALLINT,
          ano                SMALLINT,
          dias_del_mes       SMALLINT,
          i                  INTEGER,
          folio_cta          INTEGER

   DEFINE periodo_pago_1      CHAR(6)
   DEFINE reg_patron_1        CHAR(11)
   DEFINE sbc_1               DECIMAL(16,6)
   DEFINE dias_cotizados_1    SMALLINT
   DEFINE periodo_pago_2      CHAR(6)
   DEFINE reg_patron_2        CHAR(11)
   DEFINE sbc_2               DECIMAL(16,6)
   DEFINE dias_cotizados_2    SMALLINT
   DEFINE periodo_pago_3      CHAR(6)
   DEFINE reg_patron_3        CHAR(11)
   DEFINE sbc_3               DECIMAL(16,6)
   DEFINE dias_cotizados_3    SMALLINT

   DEFINE total_general_ini,
          total_general_netas,
          total_general_retiros,
          total_general_fin,
          total_semestre           DECIMAL(16,2)

   DEFINE rendimiento_siefore1,
          rendimiento_siefore2,
          rendimiento_adicional,
          rendimiento_infonavit,
          rendimiento_fovissste   DECIMAL(16,2)

   DEFINE estructura_flujo1,
          comision_flujo1,
          estructura_flujo2,
          comision_flujo2,
          estructura_saldo,
          comision_saldo,
          total_comisiones,
          estructura_adicional,
          comision_adicional    DECIMAL(16,6)

   DEFINE fondo_retiro_1,
          fondo_voluntaria_1,
          fondo_retiro_2,
          fondo_voluntaria_2,
          fondo_adicional_vol  DECIMAL(16,2),
          folio                INTEGER

   DEFINE comircv            DECIMAL(16,2)
   DEFINE comivol            DECIMAL(16,2)
   DEFINE comisal            DECIMAL(16,2)
   DEFINE comiacr            DECIMAL(16,2)
   DEFINE comitod            DECIMAL(16,2)

   DEFINE sal_dia_cot_rcv    DECIMAL(16,2)
   DEFINE sal_dia_cot_viv    DECIMAL(16,2)
   DEFINE dias_cot_rcv       SMALLINT
   DEFINE dias_cot_viv       SMALLINT
   DEFINE numeaccin          DECIMAL(16,6)
   DEFINE precioini          DECIMAL(16,6)
   DEFINE preciofin          DECIMAL(16,6)
   DEFINE importe1           DECIMAL(16,2)
   DEFINE numeaccfin         DECIMAL(16,6)
   DEFINE importe2           DECIMAL(16,2)
   DEFINE porcenta           DECIMAL(5,2)

   DEFINE precioant          DECIMAL(16,6)
   DEFINE precioact          DECIMAL(16,6)
   DEFINE precioant_sie2     DECIMAL(16,6)
   DEFINE precioact_sie2     DECIMAL(16,6)

   DEFINE vabonos_sar        DECIMAL(16,2)
   DEFINE vcargos_sar        DECIMAL(16,2)
   DEFINE vabonos_rcv        DECIMAL(16,2)
   DEFINE vcargos_rcv        DECIMAL(16,2)
   DEFINE vabonos_viv92      DECIMAL(16,2)
   DEFINE vcargos_viv92      DECIMAL(16,2)
   DEFINE vabonos_viv97      DECIMAL(16,2)
   DEFINE vcargos_viv97      DECIMAL(16,2)
   DEFINE vabonos_vol        DECIMAL(16,2)
   DEFINE vcargos_vol        DECIMAL(16,2)
   DEFINE vabonos_com        DECIMAL(16,2)
   DEFINE vcargos_com        DECIMAL(16,2)

   DEFINE salapert           DECIMAL(16,2)
   DEFINE estad              CHAR(20)
   DEFINE delega             CHAR(20)
   DEFINE vleyenda           CHAR(1)
   DEFINE city               CHAR(40)

   DEFINE fechaini           DATE
   DEFINE fechafin           DATE
   DEFINE fechaconver        DATE
   DEFINE vfech_mov_banxico  DATE
   DEFINE vfecha_traspaso    DATE
   DEFINE vfech_aport_normal DATE
   DEFINE vfecha_ini         DATE
   DEFINE vfecha_ini_sie     DATE
   DEFINE vfecha_fin_sie     DATE
   DEFINE vfecha_ini_sie2    DATE
   DEFINE vfecha_fin_sie2    DATE
   DEFINE vfecha_ini_viv     DATE
   DEFINE vfecha_fin_viv     DATE
   DEFINE vfecha_top         DATE
   DEFINE vfecha_valor_rcv   DATE
   DEFINE vfecha_valor       DATE
   DEFINE vfecha_valor92     DATE
   DEFINE vfecha_5           DATE
   DEFINE vfecha_6           DATE
   DEFINE vfecha_7           DATE
   DEFINE vfemision_tra      DATE

   DEFINE vdia               CHAR(1)
   DEFINE xdias              SMALLINT
   DEFINE vdias_viv          SMALLINT
   DEFINE vmes_viv           SMALLINT
   DEFINE vanyo_viv          SMALLINT
   DEFINE xdias2             SMALLINT
   DEFINE vdesc_banco        CHAR(15)
   DEFINE vrfc               CHAR(13)
   DEFINE vcve_ced_cuenta    CHAR(3)
   DEFINE vdesc_afore        CHAR(30)
   DEFINE vmes               CHAR(2)
   DEFINE vtasa_oper         DECIMAL(8,6)
   DEFINE vcont              SMALLINT
   DEFINE vflag              SMALLINT
   DEFINE vperiodo_pago      CHAR(06)
   DEFINE vbimestre          CHAR(06)
   DEFINE vfolio_sua         CHAR(06)
   DEFINE vperiodo_int       CHAR(07)
   DEFINE vpesos             DECIMAL(16,2)
   DEFINE vconsecutivo       INTEGER

   DEFINE texto_inf          CHAR(500)
   DEFINE vhora_ini          CHAR(08)
   DEFINE vhora_fin          CHAR(08)
   DEFINE f_hoy              DATE
   DEFINE dia                SMALLINT
   DEFINE vestado_cuenta     SMALLINT
   DEFINE vcve_clasifica     CHAR(15)

   DEFINE acciones_ini       DECIMAL(16,6)
   DEFINE precio_ini         DECIMAL(16,6)
   DEFINE precio_fin         DECIMAL(16,6)
   DEFINE importe_ini        DECIMAL(16,2)
   DEFINE acciones_fin       DECIMAL(16,6)
   DEFINE importe_fin        DECIMAL(16,2)
   DEFINE porcentaje1        DECIMAL(5,2)
   DEFINE acciones_ini2      DECIMAL(16,6)
   DEFINE precio_ini2        DECIMAL(16,6)
   DEFINE precio_fin2        DECIMAL(16,6)
   DEFINE importe_ini2       DECIMAL(16,2)
   DEFINE acciones_fin2      DECIMAL(16,6)
   DEFINE importe_fin2       DECIMAL(16,2)
   DEFINE porcentaje2        DECIMAL(5,2)

   DEFINE txt_dom            CHAR(1000)
   DEFINE txt_tel            CHAR(1000)
   DEFINE v_nacimiento       CHAR(1000)
   DEFINE l_existe           SMALLINT
   DEFINE l_existe1          SMALLINT

   DEFINE x_tipo_proceso     SMALLINT,
          x_folio            INTEGER,
          x_tipo_salida      SMALLINT,
          x_tipo_informe     SMALLINT

   DEFINE ind_fiscal         CHAR(01)
   DEFINE ind_int_fiscal     CHAR(01)

   DEFINE fecha_rcv_fiscal   DATE
   DEFINE fecha_sar_fiscal   DATE
   DEFINE fecha_sar_issste_fiscal   DATE
   DEFINE fecha_vol_fiscal  DATE
   DEFINE fecha_com_fiscal   DATE
   DEFINE fecha_alp_fiscal   DATE
   DEFINE fecha_vol_fiscal2  DATE

   DEFINE monto_rcv_fiscal   DECIMAL(16,2)
   DEFINE imp_rcv_fiscal     DECIMAL(16,2)
   DEFINE monto_sar_fiscal   DECIMAL(16,2)
   DEFINE imp_sar_fiscal     DECIMAL(16,2)
   DEFINE monto_sar_issste_fiscal DECIMAL(16,2)
   DEFINE imp_sar_issste_fiscal   DECIMAL(16,2)
   DEFINE monto_vol_fiscal   DECIMAL(16,2)
   DEFINE imp_vol_fiscal     DECIMAL(16,2)
   DEFINE monto_com_fiscal   DECIMAL(16,2)
   DEFINE imp_com_fiscal     DECIMAL(16,2)
   DEFINE monto_alp_fiscal   DECIMAL(16,2)
   DEFINE imp_alp_fiscal     DECIMAL(16,2)
   DEFINE interes_real       DECIMAL(16,2)
   DEFINE interes_nominal    DECIMAL(16,2)

   DEFINE xx_fec_liq_tras      DATE,
          xx_fec_sol_tras      DATE,
          x_nacionalidad_desc CHAR(25)

   DEFINE x_nombres          CHAR(40),
          x_desc_afo_recep   CHAR(40),
          xx_fecha_reingreso  DATE,
          x_afore_recep      CHAR(3)

   DEFINE x_fec_liq_tras,
          x_fec_sol_tras,
          x_fecha_reingreso,
          x_fena              CHAR(8)

   DEFINE x_ano         SMALLINT,
          xx_ano        SMALLINT,
          xxx_ano       SMALLINT,
          sw            SMALLINT,
          sw_1          SMALLINT,
          sel_txt       CHAR(100),
          ano_cuenta    CHAR(4)

   DEFINE v_union_saldo  CHAR(600)

   DEFINE x_precio_par_ini  DECIMAL(19,14),
          x_precio_par_fin  DECIMAL(19,14)

   DEFINE v_saldo_dia   CHAR(100),
          xfecha_inix   DATE,
          ind_edad      SMALLINT

   DEFINE genera_encabezado CHAR(300),
          genera_detalle    CHAR(3000),
          genera_sum97      CHAR(3000),
          genera_sum98      CHAR(3000),
          genera_sum99      CHAR(3000)

   DEFINE v_arch              CHAR(100),
          permisos            CHAR(50)

   DEFINE sub_sem_rcv,
          sub_viv_sem,
          sub_vcom_sem         DECIMAL(16,6),
          x_correo_electronico CHAR(200),
          vfecha_comision      DATE,
          total_dias_annio     SMALLINT

   DEFINE comision_flujo_anterior,
          comision_saldo_anterior,
          total_comisiones_anterior,
          comision_flujo_actual,
          comision_saldo_actual,
          total_comisiones_actual    DECIMAL(16,6)

   DEFINE adi RECORD
          precio_accion_sb1     DECIMAL(18,6),
          precio_accion         DECIMAL(18,6),
          total_general_final   DECIMAL(16,2)
   END RECORD

   DEFINE retiro97_cv_cs_ini,
          retiro97_ini,
          cv_cs_ini,
          sar_imss92_ini,
          infonavit97_ini,
          sar_infonavit92_ini,
          sub_imss_ini,

          retiro_issste_ini,
          sar_issste_ini,
          fovissste_ini,
          sar_fovissste_ini,
          bono_pension_ini,
          sub_issste_ini,
          vol_ini,
          com_ini,
          largo_plazo_ini,
          solidario_ini,
          sub_vol_com_ini,

          retiro97_cv_cs_aporta,
          retiro97_aporta,
          cv_cs_aporta,
          sar_imss92_aporta,
          infonavit97_aporta,
          sar_infonavit92_aporta,
          sub_imss_aporta,

          retiro_issste_aporta,
          sar_issste_aporta,
          fovissste_aporta,
          sar_fovissste_aporta,
          bono_pension_aporta,
          sub_issste_aporta,
          vol_aporta,
          com_aporta,
          largo_plazo_aporta,
          solidario_aporta,
          sub_vol_com_aporta,
          retiro97_cv_cs_retiros,

          retiro97_retiros,
          cv_cs_retiros,
          sar_imss92_retiros,
          infonavit97_retiros,
          sar_infonavit92_retiros,
          sub_imss_retiros,

          retiro_issste_retiros,
          sar_issste_retiros,
          fovissste_retiros,
          sar_fovissste_retiros,
          bono_pension_retiros,
          sub_issste_retiros,

          vol_retiros,
          com_retiros,
          largo_plazo_retiros,
          solidario_retiros,
          sub_vol_com_retiros,

          retiro97_cv_cs_rend,
          retiro97_rend,
          cv_cs_rend,
          sar_imss92_rend,
          infonavit97_rend,
          sar_infonavit92_rend,
          sub_imss_rend,

          retiro_issste_rend,
          sar_issste_rend,
          fovissste_rend,
          sar_fovissste_rend,
          bono_pension_rend,
          sub_issste_rend,

          vol_rend,
          com_rend,
          largo_plazo_rend,
          solidario_rend,
          sub_vol_com_rend,

          retiro97_cv_cs_comis,
          retiro97_comis,
          cv_cs_comis,
          sar_imss92_comis,
          infonavit97_comis,
          sar_infonavit92_comis,
          sub_imss_comis,

          retiro_issste_comis,
          sar_issste_comis,
          fovissste_comis,
          sar_fovissste_comis,
          bono_pension_comis,
          sub_issste_comis,

          vol_comis,
          com_comis,
          largo_plazo_comis,
          solidario_comis,
          sub_vol_com_comis,
          vol_fin,
          com_fin,
          cv_cs_fin,
          sar_imss92_fin,
          infonavit97_fin,
          sar_infonavit92_fin,
          retiro_issste_fin,
          fovissste_fin,
          sar_fovissste_fin,
          largo_plazo_fin,
          solidario_fin,
          retiro97_cv_cs_fin,
          sub_imss_fin,
          sub_issste_fin,
          bono_pension_fin,
          sub_vol_com_fin,
          r_monto_pes,
          total_general_comis,
          total_general_aporta,
          total_general_rend,
          bono_pension_actual,
          bono_pension_nominal,
          ahorro_retiro_ini,
          ahorro_retiro_aporta,
          ahorro_retiro_retiros,
          ahorro_retiro_rend,
          ahorro_retiro_comis,
          ahorro_retiro_fin,
          ahorro_vol_ini,
          ahorro_vol_aporta,
          ahorro_vol_retiros,
          ahorro_vol_rend,
          ahorro_vol_comis,
          ahorro_vol_fin,
          vivienda_ini,
          vivienda_movimientos,
          retiro97_fin,
          sar_issste_fin,
          sub_fin_rcv,
          sub_ini_rcv,
          sub_viv_fin,
          sub_viv_ini,
          sub_vcom_fin,
          sub_vcom_ini,
          vivienda_fin        DECIMAL(22,2)

   DEFINE x_estado            SMALLINT,
          x_codigo_ini        CHAR(5),
          x_codigo_fin        CHAR(5)

   DEFINE null_nss            CHAR(11)

   DEFINE gi_ind_redencion              SMALLINT
   DEFINE gd_fecha_redencion            DATE
   DEFINE gi_bono_pension_nominal_pesos DECIMAL(22,6)
   DEFINE gi_bono_pension_actual_pesos  DECIMAL(22,6)


END GLOBALS
#####################################################################
MAIN

   LET x_tipo_proceso = ARG_VAL(1)
   LET x_folio        = ARG_VAL(2)
   LET x_estado       = ARG_VAL(3)

   DISPLAY "INICIA PROCESO DE ESTADO DE CUENTA",
           " CON FOLIO:",x_folio ," DEL ESTADO:",x_estado

   CALL STARTLOG("CTAB0401.log")

   CALL inicializa()
   CALL dias_del_annio()

   CALL crea_tablas()

   IF x_tipo_proceso = 0 THEN
      CALL prepara_nss_semestral()
   END IF

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA:",pos

END MAIN
#####################################################################
FUNCTION crea_tablas()
   CREATE TEMP TABLE tmp_folio
   (folio INTEGER )

   CREATE INDEX tmp_folio_1 ON tmp_folio(folio)

   CREATE TEMP TABLE tmp_cuenta
   (tipo_movimiento   SMALLINT NOT NULL ,
    subcuenta         SMALLINT NOT NULL ,
    siefore           SMALLINT,
    folio             INTEGER NOT NULL ,
    consecutivo_lote  INTEGER,
    nss               CHAR(11) NOT NULL ,
    curp              CHAR(18),
    folio_sua         CHAR(6),
    fecha_pago        DATE,
    fecha_valor       DATE,
    fecha_conversion  DATE,
    monto_en_pesos    DECIMAL(22,6),
    monto_en_acciones DECIMAL(22,6),
    precio_accion     DECIMAL(22,6),
    dias_cotizados    SMALLINT,
    sucursal          CHAR(10),
    id_aportante      CHAR(11),
    estado            SMALLINT,
    fecha_proceso     DATE,
    usuario           CHAR(8),
    fecha_archivo     DATE,
    etiqueta          SMALLINT
   )

   CREATE TEMP TABLE tmp_aporte
   (folio                INTEGER     ,
    n_seguro             CHAR(11)    ,
    periodo_pago         CHAR(6)     ,
    ult_salario_diario   DECIMAL(9,2),
    reg_patronal_imss    CHAR(11)    ,
    dias_cotz_bimestre   SMALLINT    ,
    dias_incap_bimest    SMALLINT    ,
    dias_ausent_bimest   SMALLINT    ,
    impt_ret             DECIMAL(9,2),
    impt_act_rec_ret     DECIMAL(9,2),
    impt_ces_vej         DECIMAL(9,2),
    impt_act_r_ces_vej   DECIMAL(9,2),
    impt_aport_vol       DECIMAL(9,2),
    impt_aport_pat       DECIMAL(9,2),
    impt_cuota_soc       DECIMAL(9,2),
    impt_aport_est       DECIMAL(9,2),
    impt_aport_esp       DECIMAL(9,2),
    impt_act_cuo_soc     DECIMAL(9,2),
    impt_act_aport_est   DECIMAL(9,2),
    impt_act_cuo_esp     DECIMAL(9,2)
    )
   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta,
                                            tipo_movimiento)
   CREATE INDEX tmp_cuenta_2 ON tmp_cuenta (fecha_conversion)
   CREATE INDEX tmp_cuenta_3 ON tmp_cuenta (id_aportante)

   CREATE INDEX tmp_aporte_1 ON tmp_aporte (n_seguro)
END FUNCTION
#####################################################################
FUNCTION inicializa()

   DEFINE hoy1 DATE

   DEFINE sel_1,
          sel_2,
          sel_2_1,
          sel_3,
          sel_3_1,
          sel_4,
          sel_5,
          sel_6,
          sel_7,
          sel_8,
          sel_9,
          sel_rendimiento     CHAR(1000)

   DEFINE lc_sql_bono CHAR(1000)

   LET HOY  = TODAY
   LET hoy1 = HOY -1 UNITS DAY
   LET hora = TIME

   IF x_tipo_proceso <> 0 THEN
      UPDATE cta_ctr_proceso
      SET    cta_ctr_proceso.estado = 2
      WHERE  cta_ctr_proceso.folio = x_folio
      AND    cta_ctr_proceso.estado = 1

      SELECT a.tipo_salida
      INTO   x_tipo_salida
      FROM   cta_ctr_proceso a
      WHERE  a.folio = x_folio
      AND    a.estado = 2
      GROUP BY 1
   END IF

   LET i_total_reg_lote = 0
   LET vleyenda = 0

   SELECT b.*
   INTO   g_parametro.*
   FROM   seg_modulo b
   WHERE  b.modulo_cod = "cta"

   SELECT *,
          USER
   INTO   g_afore.*,
          g_usuario
   FROM   tab_afore_local

   LET sel_1 = "SELECT a.n_seguro,",
                      "a.codven,",
                      "a.n_unico,",
                      "a.n_folio,",
                      "a.tipo_solicitud,",
                      "a.paterno,",
                      "a.materno,",
                      "a.nombres,",
                      "a.n_rfc,",
                      "a.ind_infonavit,",
                      "a.fentcons,",
                      "a.coduni_n1,",
                      "a.fena,",
                      "a.nacionalidad",
               " FROM   afi_mae_afiliado a ",
               " WHERE  a.n_seguro = ? "

   LET sel_2 = "SELECT FIRST 1 a.calle,",
                       " a.numero,",
                       " a.depto,",
                       " a.colonia,",
                       " a.delega,",
                       " a.estado,",
                       " a.codpos,",
                       " a.ciudad ",
                 " FROM  afi_domicilio a ",
                 " WHERE a.nss =  ? ",
                 " AND   a.n_folio = ? ",
                 " AND   a.tipo_solicitud = ? ",
                 " AND   a.marca_envio = 'X' "

   LET sel_3 =  " SELECT FIRST 1 b.telefono",
                " FROM  afi_telefono b ",
                " WHERE b.nss = ? ",
                " AND   b.n_folio = ? ",
                " AND   b.tipo_solicitud = ? "

   LET sel_4 =  " SELECT a.fecha_presentacion,",
                        "a.ident_lote_solici[3,5]",
                " FROM   taa_cd_det_cedido a",
                " WHERE  a.n_seguro = ? ",
                #" AND    a.estado = 103 ",
                " AND    a.fecha_trasp = ? ",
                " GROUP BY 1,2 "

   LET sel_5 =  " SELECT a.subcuenta, ",
                        "a.siefore, ",
                        "a.monto_en_acciones, ",
                        "a.monto_en_pesos ",
                " FROM   tmp_saldo_edocta a",
                " WHERE  a.nss = ? ",
                " AND    a.fecha_conversion = ? "

   LET sel_6 =  " SELECT b.* ",
                " FROM   cta_id_datos b ",
                " WHERE  nss = ? ",
                " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14 "

   LET sel_8 =  " SELECT 'X' ",
                " FROM   ret_pago_vol ",
                " WHERE  nss = ? ",
                " AND    fecha_liquidacion >= ? ",
                " AND    fecha_liquidacion <= ? ",
                " GROUP BY 1 "

   LET sel_9 =  " SELECT a.fecha_aporte, ",
                        "a.fecha_liquidacion, ",
                        "a.mto_neto * -1, ",
                        "a.mto_retencion * -1, ",
                        "a.mto_rendimiento ",
                " FROM   ret_pago_vol a ",
                " WHERE  a.nss   = ? ",
                " AND    a.fecha_liquidacion BETWEEN ? AND ? "

   LET lc_sql_bono = "EXECUTE FUNCTION fn_valua_bono(?,?)"

   PREPARE get_bono   FROM  lc_sql_bono
   PREPARE eje_sel_1  FROM  sel_1
   PREPARE eje_sel_2  FROM  sel_2
   PREPARE eje_sel_3  FROM  sel_3
   PREPARE eje_sel_4  FROM  sel_4
   PREPARE eje_sel_5  FROM  sel_5
   PREPARE eje_sel_6  FROM  sel_6
   PREPARE eje_sel_8  FROM  sel_8
   PREPARE eje_sel_9  FROM  sel_9

END FUNCTION
#####################################################################
FUNCTION ini()

   LET retiro97_cv_cs_ini    = 0
   LET retiro97_ini          = 0
   LET cv_cs_ini             = 0
   LET sar_imss92_ini        = 0
   LET infonavit97_ini       = 0
   LET sar_infonavit92_ini   = 0
   LET sub_imss_ini          = 0

   LET retiro_issste_ini     = 0
   LET sar_issste_ini        = 0
   LET fovissste_ini         = 0
   LET sar_fovissste_ini     = 0
   LET bono_pension_ini      = 0
   LET sub_issste_ini        = 0

   LET vol_ini               = 0
   LET com_ini               = 0
   LET largo_plazo_ini       = 0
   LET solidario_ini         = 0
   LET sub_vol_com_ini       = 0

   LET retiro97_cv_cs_aporta  = 0
   LET retiro97_aporta        = 0
   LET cv_cs_aporta           = 0
   LET sar_imss92_aporta      = 0
   LET infonavit97_aporta     = 0
   LET sar_infonavit92_aporta = 0
   LET sub_imss_aporta        = 0

   LET retiro_issste_aporta   = 0
   LET sar_issste_aporta      = 0
   LET fovissste_aporta       = 0
   LET sar_fovissste_aporta   = 0
   LET bono_pension_aporta    = 0
   LET sub_issste_aporta      = 0

   LET vol_aporta             = 0
   LET com_aporta             = 0
   LET largo_plazo_aporta     = 0
   LET solidario_aporta       = 0
   LET sub_vol_com_aporta     = 0

   LET retiro97_cv_cs_retiros   = 0
   LET retiro97_retiros         = 0
   LET cv_cs_retiros            = 0
   LET sar_imss92_retiros       = 0
   LET infonavit97_retiros      = 0
   LET sar_infonavit92_retiros  = 0
   LET sub_imss_retiros         = 0

   LET retiro_issste_retiros    = 0
   LET sar_issste_retiros       = 0
   LET fovissste_retiros        = 0
   LET sar_fovissste_retiros    = 0
   LET bono_pension_retiros     = 0
   LET sub_issste_retiros       = 0

   LET vol_retiros              = 0
   LET com_retiros              = 0
   LET largo_plazo_retiros      = 0
   LET solidario_retiros        = 0
   LET sub_vol_com_retiros      = 0

   LET retiro97_cv_cs_rend   = 0
   LET retiro97_rend         = 0
   LET cv_cs_rend            = 0
   LET sar_imss92_rend       = 0
   LET infonavit97_rend      = 0
   LET sar_infonavit92_rend  = 0
   LET sub_imss_rend         = 0

   LET retiro_issste_rend    = 0
   LET sar_issste_rend       = 0
   LET fovissste_rend        = 0
   LET sar_fovissste_rend    = 0
   LET bono_pension_rend     = 0
   LET sub_issste_rend       = 0

   LET vol_rend              = 0
   LET com_rend              = 0
   LET largo_plazo_rend      = 0
   LET solidario_rend        = 0
   LET sub_vol_com_rend      = 0

   LET retiro97_cv_cs_comis  = 0
   LET retiro97_comis        = 0
   LET cv_cs_comis           = 0
   LET sar_imss92_comis      = 0
   LET infonavit97_comis     = 0
   LET sar_infonavit92_comis = 0
   LET sub_imss_comis        = 0

   LET retiro_issste_comis   = 0
   LET sar_issste_comis      = 0
   LET fovissste_comis       = 0
   LET sar_fovissste_comis   = 0
   LET bono_pension_comis    = 0
   LET sub_issste_comis      = 0

   LET vol_comis             = 0
   LET com_comis             = 0
   LET largo_plazo_comis     = 0
   LET solidario_comis       = 0
   LET sub_vol_com_comis     = 0

   LET bono_pension_actual   = 0
   LET bono_pension_nominal  = 0

   LET cv_cs_fin             = 0
   LET sar_imss92_fin        = 0
   LET infonavit97_fin       = 0
   LET sar_infonavit92_fin   = 0
   LET retiro_issste_fin     = 0
   LET fovissste_fin         = 0
   LET sar_fovissste_fin     = 0
   LET largo_plazo_fin       = 0
   LET solidario_fin         = 0
   LET retiro97_cv_cs_fin    = 0
   LET sub_imss_fin          = 0
   LET bono_pension_fin      = 0
   LET sub_issste_fin        = 0
   LET sub_vol_com_fin       = 0
   LET total_general_rend    = 0
   LET ahorro_retiro_ini     = 0
   LET ahorro_retiro_aporta  = 0
   LET ahorro_retiro_retiros = 0
   LET ahorro_retiro_rend    = 0
   LET ahorro_retiro_comis   = 0
   LET ahorro_retiro_fin     = 0
   LET ahorro_vol_ini        = 0
   LET ahorro_vol_aporta     = 0
   LET ahorro_vol_retiros    = 0
   LET ahorro_vol_rend       = 0
   LET ahorro_vol_comis      = 0
   LET ahorro_vol_fin        = 0
   LET vivienda_ini          = 0
   LET vivienda_movimientos  = 0
   LET vivienda_fin          = 0
   LET retiro97_fin          = 0
   LET sar_issste_fin        = 0
   LET sub_fin_rcv           = 0
   LET sub_ini_rcv           = 0
   LET sub_viv_fin           = 0
   LET sub_viv_ini           = 0
   LET sub_vcom_fin          = 0
   LET sub_vcom_ini          = 0

   LET vol_ini               = 0    ---- 3,10
   LET com_ini               = 0    ---- 11,12
   LET vol_retiros           = 0
   LET com_retiros           = 0

   LET vol_fin               = 0
   LET com_fin               = 0

   LET total_general_ini     = 0
   LET total_general_netas   = 0
   LET total_general_retiros = 0
   LET total_general_fin     = 0

   LET total_semestre        = 0

   LET fondo_retiro_1      = 0
   LET fondo_voluntaria_1  = 0
   LET fondo_retiro_2      = 0
   LET fondo_voluntaria_2  = 0
   LET fondo_adicional_vol = 0

   LET sub_sem_rcv         = 0
   LET sub_viv_sem         = 0
   LET sub_vcom_sem        = 0

------------- variables de aportes

   LET periodo_pago_1   = ""
   LET reg_patron_1     = ""
   LET sbc_1            = 0
   LET dias_cotizados_1 = 0
   LET periodo_pago_2   = ""
   LET reg_patron_2     = ""
   LET sbc_2            = 0
   LET dias_cotizados_2 = 0
   LET periodo_pago_3   = ""
   LET reg_patron_3     = ""
   LET sbc_3            = 0
   LET dias_cotizados_3 = 0

   LET aporte_vol = 0
   LET aporte_obl = 0

   LET comision_flujo_anterior   = 0
   LET comision_saldo_anterior   = 0
   LET total_comisiones_anterior = 0
   LET comision_flujo_actual     = 0
   LET comision_saldo_actual     = 0
   LET total_comisiones_actual   = 0

   LET retiro97_rend             = 0
   LET vol_rend                  = 0
   LET com_rend                  = 0

   LET gi_ind_redencion              = 0
   LET gi_bono_pension_nominal_pesos = 0
   LET gi_bono_pension_actual_pesos  = 0


END FUNCTION
#####################################################################
FUNCTION estado_cuenta()

   DEFINE rend_infonavit,
          rend_fovissste SMALLINT

   LET rend_infonavit = 11
   LET rend_fovissste = 0

   CALL ini()             --limpia_vaiables
   CALL genera_patrones() --crea tmp_cuenta

   IF x_tipo_informe = 0 THEN

      LET rendimiento_infonavit = 3.99
      LET rendimiento_fovissste = 1.03

      CALL datos_trabajador_sem()
      CALL resumen_saldos_sem()

      --Obtener valor real y nominal del bono
      IF x_estado = 40 OR
      	 x_estado = 41 THEN
      	 CALL bono()
      END IF
   END IF

   --CALL resumen_comisiones()
   --CALL obtiene_aportes()
   --CALL obtiene_retiros()
   --CALL aportes_netos()

   --CALL rendimiento()

   --CALL resumengral()

   CALL ingresa_estado()

END FUNCTION
#####################################################################
FUNCTION prepara_nss()
   DEFINE v_arch              CHAR(100),
          permisos            CHAR(50)

   SELECT a.tipo_informe
   INTO   x_tipo_informe
   FROM   cta_ctr_proceso a
   WHERE  a.folio = x_folio
   AND    a.estado = 2
   GROUP BY 1

   DECLARE cur_fechas CURSOR FOR
   SELECT a.fecha_inicio,
          a.fecha_fin
   FROM   cta_ctr_proceso a
   WHERE  a.folio = x_folio
   AND    a.estado = 2
   GROUP BY 1,2
   ORDER BY 1,2

   LET pos = 0

   CALL Ingresa_etapa(x_folio,2,0,"Inicia calculo de estado de cuenta")

   LET v_arch = nombre_archivo(x_tipo_informe,HOY,x_folio,g_usuario)

   START REPORT r_report TO v_arch

   LET genera_encabezado = g_afore.codigo_afore,"|",
                           HOY USING "DDMMYYYY","|",
                           "1","|",
                           x_tipo_informe

   OUTPUT TO REPORT r_report(genera_encabezado,232,1)

   FOREACH cur_fechas INTO g_reg.fecha_ini,
                           g_reg.fecha_top

      CALL pide_parametros()

      DECLARE cur_nss CURSOR FOR
      SELECT b.tipo_informe,
             b.nss
      FROM   cta_ctr_proceso b
      WHERE  b.folio = x_folio
      AND    b.estado = 2
      AND    b.fecha_inicio = g_reg.fecha_ini
      AND    b.fecha_fin = g_reg.fecha_top

      FOREACH cur_nss INTO x_tipo_informe,
                           g_reg.nss

         LET pos = pos + 1

         CALL estado_cuenta()  #ec

         OUTPUT TO REPORT r_report(genera_detalle,232,2)
      END FOREACH

      --INITIALIZE g_reg.* TO NULL
   END FOREACH

   CALL sumarios()

   CALL Actualiza_etapa(x_folio,2,pos,"Termina calculo de estado de cuenta")

   FINISH REPORT r_report

   CALL Actualiza_etapa(x_folio,1,pos,v_arch)

   LET permisos = "chmod 777 ",v_arch CLIPPED
   RUN permisos

   UPDATE cta_ctr_proceso
   SET    cta_ctr_proceso.estado = 3
   WHERE  cta_ctr_proceso.folio = x_folio
   AND    cta_ctr_proceso.estado = 2

END FUNCTION
#####################################################################
FUNCTION prepara_nss_semestral()
   DEFINE v_arch_sem              CHAR(100),
          permisos            CHAR(100)

   LET x_tipo_informe = x_tipo_proceso
   LET pos = 0

   LET g_reg.fecha_ini = "07/01/2008"
   LET g_reg.fecha_top = "12/31/2008"

   CALL Ingresa_etapa(x_folio,2,x_estado,"Inicia calculo de estado de cuenta")

   LET v_arch_sem = g_parametro.ruta_envio CLIPPED,"/",
                    "ECS",x_folio USING "<<<<<<<<<"

   START REPORT r_report TO v_arch_sem

   LET genera_encabezado = g_afore.codigo_afore,"|",
                           HOY USING "DDMMYYYY","|",
                           "1","|",
                           x_tipo_informe

   OUTPUT TO REPORT r_report(genera_encabezado,232,1)

   CALL pide_parametros()

   DECLARE cur_nss_sem CURSOR FOR
   SELECT a.nss,
          a.status,
          a.ind_edad
   FROM   cta_nss_edo_cta a
   WHERE  a.estado = x_estado
   ORDER BY a.nss

   FOREACH cur_nss_sem INTO g_reg.nss,
                            g_reg.status,
                            reg_gen.siefore

      LET pos = pos + 1

      CALL estado_cuenta()  #ec

      OUTPUT TO REPORT r_report(genera_detalle,232,2)

      INSERT INTO  cta_nss_ctr
      VALUES(x_folio,
             g_reg.nss,
             g_reg.fecha_top,
             pos,
             x_estado,
             20)

      INITIALIZE g_reg.nss TO NULL
      INITIALIZE g_reg.status TO NULL
   END FOREACH

   CALL sumarios()

   CALL Actualiza_etapa(x_folio,2,pos,"Termina calculo de estado de cuenta")

   FINISH REPORT r_report

   CALL Actualiza_etapa(x_folio,1,pos,v_arch_sem)

   LET permisos = "chmod 777 ",v_arch_sem CLIPPED
   RUN permisos

END FUNCTION
#####################################################################
FUNCTION pide_parametros()

   LET vfecha_ini      = g_reg.fecha_ini
   LET vfecha_top      = g_reg.fecha_top

   IF vfecha_ini < "01/14/2005" THEN
      LET vfecha_ini_sie2 = "01/14/2005"
   ELSE
      LET vfecha_ini_sie2 = vfecha_ini
   END IF

   IF vfecha_top < "01/14/2005" THEN
      LET vfecha_fin_sie2 = "01/14/2005"
   ELSE
      LET vfecha_fin_sie2 = vfecha_top
   END IF

   LET vfecha_ini_sie = habil_anterior(g_reg.fecha_ini)

   LET vfecha_fin_sie = habil_anterior(g_reg.fecha_top)

   IF vfecha_ini_sie = g_reg.fecha_ini THEN
      LET vfecha_ini_sie = vfecha_ini_sie - 1 UNITS DAY
      LET vfecha_ini_sie = habil_anterior(vfecha_ini_sie)
   END IF

   LET precioant = precio_siefore (2,vfecha_ini_sie)
   LET precioact = precio_siefore (2,vfecha_fin_sie)

   LET precioant_sie2 = precio_siefore (1,vfecha_ini_sie2)
   LET precioact_sie2 = precio_siefore (1,vfecha_fin_sie2)

   LET x_precio_par_ini = precio_siefore(11,g_reg.fecha_ini)
   LET x_precio_par_fin = precio_siefore(11,g_reg.fecha_top)

   LET dias_del_mes = (vfecha_fin_sie - vfecha_ini_sie)
   LET rendimiento_siefore1       = ((precioact/precioant)-1)*360/(dias_del_mes*100)
   LET rendimiento_siefore2  = ((precioact_sie2/precioant_sie2)-1)*360 /
                                                    (dias_del_mes*100)

   LET x_precio_par_ini = precio_siefore(11,g_reg.fecha_ini)
   LET x_precio_par_fin = precio_siefore(11,g_reg.fecha_top)

   IF x_tipo_informe = 0 THEN
      LET rendimiento_siefore1 = 11.70
      LET rendimiento_siefore2 = 13.11
   ELSE
      LET rendimiento_siefore1 = 8.76
      LET rendimiento_siefore2 = 8.80
   END IF
{
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_folio
   WHENEVER ERROR STOP

      CREATE TEMP TABLE tmp_folio
      (folio INTEGER )
}
   DELETE FROM tmp_folio

      INSERT INTO tmp_folio
      SELECT a.folio
      FROM   dis_dep_aporte a
      WHERE  a.fech_liquidacion >= g_reg.fecha_ini
      AND    a.fech_liquidacion <= g_reg.fecha_top
      AND    a.estado = 3
      --AND    a.ident_pago[14,15] IN (11,23)
      AND    a.ident_pago[14,15] IN (40,41,42,45,46,49)

---      CREATE INDEX tmp_folio_1 ON tmp_folio(folio)
      UPDATE STATISTICS FOR TABLE tmp_folio

END FUNCTION
#####################################################################
FUNCTION genera_patrones()

   DEFINE mig_folio INTEGER
{
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cuenta
      DROP TABLE tmp_aporte
   WHENEVER ERROR STOP

      CREATE TEMP TABLE tmp_cuenta
      (tipo_movimiento   SMALLINT NOT NULL ,
       subcuenta         SMALLINT NOT NULL ,
       siefore           SMALLINT,
       folio             INTEGER NOT NULL ,
       consecutivo_lote  INTEGER,
       nss               CHAR(11) NOT NULL ,
       curp              CHAR(18),
       folio_sua         CHAR(6),
       fecha_pago        DATE,
       fecha_valor       DATE,
       fecha_conversion  DATE,
       monto_en_pesos    DECIMAL(22,6),
       monto_en_acciones DECIMAL(22,6),
       precio_accion     DECIMAL(22,6),
       dias_cotizados    SMALLINT,
       sucursal          CHAR(10),
       id_aportante      CHAR(11),
       estado            SMALLINT,
       fecha_proceso     DATE,
       usuario           CHAR(8),
       fecha_archivo     DATE,
       etiqueta          SMALLINT
      )

      CREATE TEMP TABLE tmp_aporte
      (folio                INTEGER     ,
       n_seguro             CHAR(11)    ,
       periodo_pago         CHAR(6)     ,
       ult_salario_diario   DECIMAL(9,2),
       reg_patronal_imss    CHAR(11)    ,
       dias_cotz_bimestre   SMALLINT    ,
       dias_incap_bimest    SMALLINT    ,
       dias_ausent_bimest   SMALLINT    ,
       impt_ret             DECIMAL(9,2),
       impt_act_rec_ret     DECIMAL(9,2),
       impt_ces_vej         DECIMAL(9,2),
       impt_act_r_ces_vej   DECIMAL(9,2),
       impt_aport_vol       DECIMAL(9,2),
       impt_aport_pat       DECIMAL(9,2),
       impt_cuota_soc       DECIMAL(9,2),
       impt_aport_est       DECIMAL(9,2),
       impt_aport_esp       DECIMAL(9,2),
       impt_act_cuo_soc     DECIMAL(9,2),
       impt_act_aport_est   DECIMAL(9,2),
       impt_act_cuo_esp     DECIMAL(9,2)
       )
}
   LET x_ano   = YEAR (TODAY)
   LET xx_ano  = YEAR (g_reg.fecha_ini)
   LET xxx_ano = YEAR (g_reg.fecha_top)

   IF xxx_ano = x_ano THEN
      LET xxx_ano = YEAR (g_reg.fecha_top) - 1
      LET sw = TRUE
   END IF

   LET i = 0

--   WHENEVER ERROR CONTINUE

   DELETE FROM tmp_cuenta
   DELETE FROM tmp_aporte

   IF xx_ano = 2008 OR
   	  xx_ano = 2009 THEN
      INSERT INTO tmp_cuenta
      SELECT a.*
      FROM   dis_cuenta a
      WHERE  a.nss = g_reg.nss

   ELSE
      LET sw_1 = TRUE

      FOR i = 1997 TO xxx_ano

         LET ano_cuenta = i

         LET sel_txt =  " INSERT INTO tmp_cuenta ",
                        " SELECT b.* ",
                        " FROM   dis_cuenta",ano_cuenta[3,4] CLIPPED, " b",
                        " WHERE  b.nss = ","'",g_reg.nss CLIPPED,"'"

         PREPARE cta_execute FROM sel_txt

         EXECUTE cta_execute
      END FOR

      IF sw THEN
         INSERT INTO tmp_cuenta
         SELECT c.*
         FROM   dis_cuenta c
         WHERE  c.nss = g_reg.nss
      END IF
   END IF
{
   WHENEVER ERROR STOP

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta,
                                            tipo_movimiento)
   CREATE INDEX tmp_cuenta_2 ON tmp_cuenta (fecha_conversion)
   CREATE INDEX tmp_cuenta_3 ON tmp_cuenta (id_aportante)
}
   UPDATE STATISTICS FOR TABLE tmp_cuenta

   IF sw_1 THEN
      DELETE
      FROM  tmp_cuenta
      WHERE subcuenta > 0
      AND   tipo_movimiento = 999

      UPDATE STATISTICS FOR TABLE tmp_cuenta
   END IF

   LET i = 0

END FUNCTION
#####################################################################
FUNCTION datos_trabajador()

   DEFINE     r_existe,
              r_edad,
              r_criterio   SMALLINT

   DEFINE     v_curp    CHAR(18),
              v_rfc     CHAR(13),
              v_fena    DATE

   INITIALIZE w_aux.callep   TO NULL
   INITIALIZE w_aux.numep    TO NULL
   INITIALIZE w_aux.deptop   TO NULL
   INITIALIZE w_aux.coloniap TO NULL
   INITIALIZE w_aux.delegap  TO NULL
   INITIALIZE w_aux.estadop  TO NULL
   INITIALIZE w_aux.codposp  TO NULL
   INITIALIZE w_aux.ciudadp  TO NULL

   INITIALIZE w_aux.fonop TO NULL

   LET x_fec_liq_tras  = " "
   LET x_fec_sol_tras  = " "
   LET x_fena          = " "
   LET x_nacionalidad_desc = " "
   LET x_nombres         = " "
   LET x_desc_afo_recep  = " "
   LET x_fecha_reingreso = "31121899"
   LET x_afore_recep     = " "

   LET r_existe = 0
   LET r_edad   = 0
   LET r_criterio = 0
   LET v_curp = " "
   LET v_rfc  = " "
   LET v_fena = " "

   EXECUTE eje_sel_1 USING  g_reg.nss
                     INTO   w_aux.seguro,
                            w_aux.codven,
                            w_aux.n_unico,
                            w_aux.n_folio,
                            w_aux.tipo_solicitud,
                            w_aux.paterno,
                            w_aux.materno,
                            w_aux.nombres,
                            w_aux.rfc,
                            w_aux.infonavit,
                            w_aux.fentcons,
                            w_aux.sucursal,
                            w_aux.fena,
                            w_aux.nacionalidad

   DECLARE c_dom CURSOR FOR eje_sel_2

   FOREACH c_dom USING g_reg.nss,
                       w_aux.n_folio,
                       w_aux.tipo_solicitud
                  INTO w_aux.callep,
                       w_aux.numep,
                       w_aux.deptop,
                       w_aux.coloniap,
                       w_aux.delegap,
                       w_aux.estadop,
                       w_aux.codposp,
                       w_aux.ciudadp

      LET l_existe = TRUE

   END FOREACH

   CLOSE c_dom
   FREE  c_dom

   LET vcentro_reparto = "00000"

   SELECT a.estad_desc
   INTO   w_aux.estadop
   FROM   tab_estado a
   WHERE  a.estad_cod = w_aux.estadop

   SELECT b.deleg_desc
   INTO   w_aux.delegap
   FROM   tab_delegacion b
   WHERE  b.deleg_cod = w_aux.delegap

   SELECT c.ciudad_desc
   INTO   w_aux.ciudadp
   FROM   tab_ciudad c
   WHERE  c.ciudad_cod = w_aux.ciudadp

   SELECT d.centro_reparto
   INTO   vcentro_reparto
   FROM   tab_reparto d
   WHERE  d.codigo_postal = w_aux.codposp

   IF SQLCA.SQLCODE <> 0 THEN
      LET vcentro_reparto = "00000"
   END IF

   LET vafore_receptora = ""

   IF w_aux.fentcons > vfecha_6 THEN
      LET comision_flujo1 = 1.30
   ELSE
      IF w_aux.fentcons > vfecha_7 THEN
         LET comision_flujo1 = 1.28
      ELSE
         LET comision_flujo1 = 1.26
      END IF
   END IF

   LET   vestado_envio = "0"

   IF x_tipo_informe = 3 OR
      x_tipo_informe = 4 OR
      x_tipo_informe = 2 OR
      x_tipo_informe = 9 OR
      x_tipo_informe = 10 THEN

      DECLARE c_tel CURSOR FOR eje_sel_3

      FOREACH c_tel USING g_reg.nss,
                          w_aux.n_folio,
                          w_aux.tipo_solicitud
                     INTO w_aux.fonop
         LET l_existe1 = TRUE
      END FOREACH

      FREE c_tel

      LET x_nacionalidad_desc = ""

      SELECT b.nacionalidad
      INTO   x_nacionalidad_desc
      FROM   tab_nacionalidad b
      WHERE  b.codigo_pais  = w_aux.nacionalidad
      GROUP BY 1

      LET xx_fec_liq_tras = g_reg.fecha_top

      EXECUTE eje_sel_4 USING g_reg.nss,
                        xx_fec_liq_tras
                   INTO xx_fec_sol_tras,
                        x_afore_recep

      LET x_nombres = w_aux.nombres

      SELECT b.afore_desc
      INTO   x_desc_afo_recep
      FROM   tab_afore  b
      WHERE  b.afore_cod = x_afore_recep

      LET xx_fecha_reingreso = xx_fec_liq_tras + total_dias_annio UNITS DAY

      LET x_fec_liq_tras    = xx_fec_liq_tras    USING "DDMMYYYY"
      LET x_fec_sol_tras    = xx_fec_sol_tras    USING "DDMMYYYY"
      LET x_fena            = w_aux.fena         USING "DDMMYYYY"
      LET x_fecha_reingreso = xx_fecha_reingreso USING "DDMMYYYY"
   END IF

   LET ind_edad = 0               --- actualizacion 2008
   LET reg_gen.ind_siefore = "0"
   LET reg_gen.siefore = 0

   LET v_nacimiento = " EXECUTE FUNCTION fn_fnacimiento ( ?,? ) "
   PREPARE eje_nacimiento FROM v_nacimiento

   DECLARE cur_fnacimiento CURSOR FOR eje_nacimiento
   OPEN cur_fnacimiento  USING g_reg.nss ,--nss
                               g_reg.fecha_top
   FETCH cur_fnacimiento INTO r_existe,
                              r_edad,
                              r_criterio,
                              reg_gen.siefore,
                              v_curp,
                              v_rfc,
                              v_fena

    LET reg_gen.siefore = reg_gen.siefore + 90

END FUNCTION
#####################################################################
FUNCTION datos_trabajador_sem()

    DEFINE vnom char(40)
    DEFINE vpat char(40)
    DEFINE vmat char(40)

    INITIALIZE w_aux.* TO NULL
    INITIALIZE vcentro_reparto TO NULL

    EXECUTE eje_sel_6 USING g_reg.nss
                      INTO w_aux.seguro,
                           w_aux.n_unico,
                           w_aux.n_folio,
                           w_aux.nombres,
                           w_aux.rfc,
                           w_aux.infonavit,
                           w_aux.fentcons,
                           w_aux.callep,
                           w_aux.coloniap,
                           w_aux.delegap,
                           w_aux.estadop,
                           w_aux.ciudadp,
                           w_aux.codposp,
                           vcentro_reparto

    LET vafore_receptora = ""

    LET vleyenda = "0"
    LET reg_gen.siefore = reg_gen.siefore + 90
END FUNCTION
#####################################################################
FUNCTION saldo_dia(pnss,
                   psubcuenta,
                   pgrupo,
                   pfecha_saldo)

   DEFINE pnss              CHAR(11),
          psubcuenta        SMALLINT,
          pgrupo            SMALLINT,
          pfecha_saldo      DATE

   DEFINE v_tipo            SMALLINT,
          v_subcuenta       SMALLINT,
          v_siefore         SMALLINT,
          num_siefore_g     SMALLINT,
          num_siefore_t     SMALLINT,
          num_siefore_v     SMALLINT,
          v_saldo_acc       DECIMAL(16,6),
          v_saldo_pes       DECIMAL(16,6),
          v_precio_viv      DECIMAL(19,14),
          v_feche_viv       DATE

   DEFINE v_fecha_viv       DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldo
      DROP TABLE tmp_saldo_dia
   WHENEVER ERROR STOP

   CALL g_tmp_cta(pnss)

   LET v_fecha_viv = MDY(MONTH(pfecha_saldo),1,YEAR(pfecha_saldo))

   IF pfecha_saldo < "08/01/2004" THEN
      LET v_tipo = 0
   ELSE
      LET v_tipo = 1

      SELECT "EXISTE"
      FROM   glo_valor_accion g
      WHERE  g.codigo_siefore = 0
      AND    g.fecha_valuacion = pfecha_saldo

      IF SQLCA.SQLCODE = NOTFOUND THEN
         INSERT INTO glo_valor_accion (codigo_siefore,
                                       precio_del_dia,
                                       fecha_valuacion)
         VALUES (0,0,pfecha_saldo)
      END IF

      SELECT COUNT(*)
      INTO   num_siefore_t
      FROM   tab_siefore_local

      IF num_siefore_t IS NULL THEN
         LET num_siefore_t = 0
      END IF

      IF pfecha_saldo < "01/14/2005" THEN
         LET num_siefore_t = num_siefore_t - 1
      END IF

      SELECT COUNT(DISTINCT a.codigo_siefore)
      INTO   num_siefore_g
      FROM   glo_valor_accion a
      WHERE  a.fecha_valuacion = pfecha_saldo
      AND    a.codigo_siefore <> 11

      SELECT COUNT(DISTINCT b.codigo_siefore)
      INTO   num_siefore_v
      FROM   glo_valor_accion b
      WHERE  b.fecha_valuacion = v_fecha_viv
      AND    b.codigo_siefore = 11

      IF num_siefore_g IS NULL THEN
         LET num_siefore_g = 0
      END IF

      IF num_siefore_v IS NULL THEN
         LET num_siefore_v = 0
      END IF

      LET num_siefore_g = num_siefore_g + num_siefore_v

      IF num_siefore_t <>  num_siefore_g THEN
         DISPLAY "Verifique siefores y precios de accion: ",pfecha_saldo,
                 " Vivienda: ",v_fecha_viv
      END IF

      SELECT c.precio_del_dia
      INTO   v_precio_viv
      FROM   glo_valor_accion c
      WHERE  c.fecha_valuacion = v_fecha_viv
      AND    c.codigo_siefore = 11
   END IF

   SELECT d.subcuenta,
          d.siefore,
          SUM(d.monto_en_acciones) monto_en_acciones,
          SUM(d.monto_en_pesos) monto_en_pesos
   FROM   tmp_dis_cuenta d
   WHERE  d.fecha_conversion <= pfecha_saldo
   GROUP BY 1,2
   ORDER BY 1,2
   INTO TEMP tmp_saldo

   IF v_tipo = 1 THEN
      LET v_union_saldo = " SELECT subcuenta,",
                                  "siefore,",
                                  "monto_en_acciones,",
                                 " CASE WHEN siefore = 0 THEN ",
                                           "monto_en_pesos ",
                                      " ELSE ",
                                           "monto_en_acciones * precio_del_dia",
                                  " END monto_en_pesos",
                          " FROM  tmp_saldo,glo_valor_accion",
                          " WHERE subcuenta NOT IN (4,8)",
                          " AND   siefore = codigo_siefore",
                          " AND   fecha_valuacion = ","'",pfecha_saldo,"'",
                          " UNION ",
                          " SELECT subcuenta,",
                                  "siefore,",
                                  "monto_en_acciones,",
                                  "monto_en_acciones * ",v_precio_viv,
                          " FROM   tmp_saldo",
                          " WHERE  subcuenta IN (4,8)",
                          " INTO TEMP tmp_saldo_dia"

      PREPARE tmp_union_saldo FROM v_union_saldo
      EXECUTE tmp_union_saldo
   ELSE
      SELECT g.subcuenta,
             g.siefore,
             g.monto_en_acciones,
             g.monto_en_acciones * c.precio_del_dia  monto_en_pesos
      FROM   tmp_saldo g,glo_valor_accion c
      WHERE  g.subcuenta NOT IN (4,8)
      AND    g.siefore = c.codigo_siefore
      AND    c.fecha_valuacion = pfecha_saldo
      UNION
      SELECT a.subcuenta,
             a.siefore,
             a.monto_en_acciones,
             a.monto_en_pesos
      FROM   tmp_saldo a
      WHERE  a.subcuenta IN (4,8)
      INTO TEMP tmp_saldo_dia
   END IF

END FUNCTION
####################################################
FUNCTION g_tmp_cta(p_nss)

   DEFINE v_nombre_tabla  CHAR(20),
          p_nss           CHAR(11),
          sel_his         CHAR(900)

   DEFINE mig_total       INTEGER

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   DECLARE cur_his CURSOR FOR
   SELECT tabname
   FROM   systables
   WHERE  tabname matches "dis_cuenta??"

   FOREACH cur_his INTO v_nombre_tabla
      LET sel_his = sel_his CLIPPED,
                    " SELECT * ",
                    " FROM   ",v_nombre_tabla CLIPPED,
                    " WHERE  nss = ","'",p_nss,"'",
                    " AND    tipo_movimiento NOT IN (888,999)",
                    " UNION ALL "
   END FOREACH
   CLOSE cur_his

   LET sel_his =  sel_his CLIPPED,
                 " SELECT * ",
                 " FROM   dis_cuenta ",
                 " WHERE  nss = ","'",p_nss,"'",
                 " AND    tipo_movimiento NOT IN (888,999)",
                 " INTO TEMP tmp_dis_cuenta "

   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta(subcuenta,
                                                  siefore,
                                                  folio,
                                                  consecutivo_lote)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION
################################################################################
FUNCTION resumen_saldos()

   DEFINE f_subcuenta       SMALLINT,
          f_siefore         SMALLINT,
          f_monto_acc       DECIMAL(22,6),
          f_monto_pes       DECIMAL(22,6),
          v_subcuenta       SMALLINT,
          v_grupo           SMALLINT

   LET f_subcuenta       = 0
   LET f_siefore         = 0
   LET f_monto_acc       = 0
   LET f_monto_pes       = 0
   LET xfecha_inix       = g_reg.fecha_ini - 1 UNITS DAY

   LET v_subcuenta = 0
   LET v_grupo     = 0
----------------------------------------------------------------------------
   CALL saldo_dia(g_reg.nss,
                  v_subcuenta,
                  v_grupo,
                  xfecha_inix)

   LET v_saldo_dia = " SELECT a.*",
                     " FROM   tmp_saldo_dia a",
                     " ORDER BY 1,2"
   PREPARE eje_saldo_dia FROM v_saldo_dia
--******************************************************************************
--SALDO INICIAL
--******************************************************************************
   DECLARE c_saldo CURSOR FOR  eje_saldo_dia
   FOREACH c_saldo INTO f_subcuenta,
                        f_siefore,
                        f_monto_acc,
                        f_monto_pes

      CASE
         WHEN f_subcuenta = 1
            LET retiro97_ini = retiro97_ini + f_monto_pes
         WHEN f_subcuenta = 2 OR
              f_subcuenta = 5 OR
              f_subcuenta = 6 OR
              f_subcuenta = 9
            LET cv_cs_ini  = cv_cs_ini  + f_monto_pes
         WHEN f_subcuenta = 7
            LET sar_imss92_ini = sar_imss92_ini + f_monto_pes
         WHEN f_subcuenta = 4
            LET infonavit97_ini = infonavit97_ini + f_monto_pes
         WHEN f_subcuenta = 8
            LET sar_infonavit92_ini = sar_infonavit92_ini + f_monto_pes

         WHEN f_subcuenta = 30 OR
              f_subcuenta = 31 OR
              f_subcuenta = 32
            LET retiro_issste_ini = retiro_issste_ini + f_monto_pes
         WHEN f_subcuenta = 13 OR
              f_subcuenta = 19
            LET sar_issste_ini = sar_issste_ini + f_monto_pes
         WHEN f_subcuenta = 35
            LET fovissste_ini = fovissste_ini + f_monto_pes
         WHEN f_subcuenta = 14
            LET sar_fovissste_ini = sar_fovissste_ini + f_monto_pes

         WHEN f_subcuenta = 3  OR
              f_subcuenta = 10 OR
              f_subcuenta = 22 OR
              f_subcuenta = 23
            LET vol_ini = vol_ini + f_monto_pes
         WHEN f_subcuenta = 11 OR
              f_subcuenta = 12 OR
              f_subcuenta = 24 OR
              f_subcuenta = 25
            LET com_ini = com_ini + f_monto_pes
         WHEN f_subcuenta = 15 OR
              f_subcuenta = 16 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27
            LET largo_plazo_ini = largo_plazo_ini + f_monto_pes
         WHEN f_subcuenta = 33 OR
              f_subcuenta = 34
            LET solidario_ini = solidario_ini + f_monto_pes

         WHEN f_subcuenta = 36
         	  LET bono_pension_ini = bono_pension_ini + f_monto_pes
      END CASE
   END FOREACH

   LET retiro97_cv_cs_ini = retiro97_ini   +
                            cv_cs_ini

   LET sub_imss_ini   = retiro97_ini         +
                        cv_cs_ini            +
                        sar_imss92_ini       +
                        infonavit97_ini      +
                        sar_infonavit92_ini

   LET sub_issste_ini = retiro_issste_ini  +
                        sar_issste_ini     +
                        fovissste_ini      +
                        sar_fovissste_ini  +
                        bono_pension_ini

   LET sub_vol_com_ini = vol_ini           +
                         com_ini           +
                         largo_plazo_ini   +
                         solidario_ini

   LET total_general_ini    = sub_imss_ini   +
                              sub_issste_ini +
                              sub_vol_com_ini
--******************************************************************************
--SALDO FINAL
--******************************************************************************
   CALL saldo_dia(g_reg.nss,
                  v_subcuenta,
                  v_grupo,
                  g_reg.fecha_top)

   DECLARE c_saldo1 CURSOR FOR eje_saldo_dia
   FOREACH c_saldo1 INTO f_subcuenta,
                         f_siefore,
                         f_monto_acc,
                         f_monto_pes
      CASE
         WHEN f_subcuenta = 1
            LET retiro97_fin = retiro97_fin + f_monto_pes
         WHEN f_subcuenta = 2 OR
              f_subcuenta = 5 OR
              f_subcuenta = 6 OR
              f_subcuenta = 9
            LET cv_cs_fin  = cv_cs_fin  + f_monto_pes
         WHEN f_subcuenta = 7
            LET sar_imss92_fin = sar_imss92_fin + f_monto_pes
         WHEN f_subcuenta = 4
            LET infonavit97_fin = infonavit97_fin + f_monto_pes
         WHEN f_subcuenta = 8
            LET sar_infonavit92_fin = sar_infonavit92_fin + f_monto_pes

         WHEN f_subcuenta = 30 OR
              f_subcuenta = 31 OR
              f_subcuenta = 32
            LET retiro_issste_fin = retiro_issste_fin + f_monto_pes
         WHEN f_subcuenta = 13 OR
              f_subcuenta = 19
            LET sar_issste_fin = sar_issste_fin + f_monto_pes
         WHEN f_subcuenta = 35
            LET fovissste_fin = fovissste_fin + f_monto_pes
         WHEN f_subcuenta = 14
            LET sar_fovissste_fin = sar_fovissste_fin + f_monto_pes

         WHEN f_subcuenta = 3  OR
              f_subcuenta = 10 OR
              f_subcuenta = 22 OR
              f_subcuenta = 23
            LET vol_fin = vol_fin + f_monto_pes
         WHEN f_subcuenta = 11 OR
              f_subcuenta = 12 OR
              f_subcuenta = 24 OR
              f_subcuenta = 25
            LET com_fin = com_fin + f_monto_pes
         WHEN f_subcuenta = 15 OR
              f_subcuenta = 16 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27
            LET largo_plazo_fin = largo_plazo_fin + f_monto_pes
         WHEN f_subcuenta = 33 OR
              f_subcuenta = 34
            LET solidario_fin = solidario_fin + f_monto_pes

         WHEN f_subcuenta = 36
         	  LET bono_pension_fin = bono_pension_fin + f_monto_pes
      END CASE
   END FOREACH

   LET retiro97_cv_cs_fin = retiro97_fin   +
                            cv_cs_fin

   LET sub_imss_fin   = retiro97_fin         +
                        cv_cs_fin            +
                        sar_imss92_fin       +
                        infonavit97_fin      +
                        sar_infonavit92_fin

   LET sub_issste_fin = retiro_issste_fin  +
                        sar_issste_fin     +
                        fovissste_fin      +
                        sar_fovissste_fin  +
                        bono_pension_fin

   LET sub_vol_com_fin = vol_fin           +
                         com_fin           +
                         largo_plazo_fin   +
                         solidario_fin

   LET total_general_fin    = sub_imss_fin   +
                              sub_issste_fin +
                              sub_vol_com_fin

   LET sub_sem_rcv         = sub_fin_rcv - sub_ini_rcv
   LET sub_viv_sem         = sub_viv_fin - sub_viv_ini
   LET sub_vcom_sem        = sub_vcom_fin - sub_vcom_ini

   LET total_semestre = sub_sem_rcv +
                        sub_viv_sem +
                        sub_vcom_sem
END FUNCTION
#####################################################################
FUNCTION resumen_saldos_sem()

--******************************************************************************
--SALDO FINAL
--******************************************************************************

   SELECT SUM(a.monto_en_pesos)
   INTO   total_general_fin
   FROM   tmp_saldo_edocta a
   WHERE  a.nss              = g_reg.nss
   AND    a.fecha_conversion = g_reg.fecha_top
   AND    a.subcuenta IN (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,
                          22,23,24,25,26,27,30,31,32,33,34,35)

   {DECLARE c_saldo1_sem CURSOR FOR eje_sel_5

   FOREACH c_saldo1_sem USING g_reg.nss,
                              g_reg.fecha_top
                          INTO f_subcuenta,
                               f_siefore,
                               f_monto_acc,
                               f_monto_pes

      CASE
         WHEN f_subcuenta = 1
            LET retiro97_fin = retiro97_fin + f_monto_pes
         WHEN f_subcuenta = 2 OR
              f_subcuenta = 5 OR
              f_subcuenta = 6 OR
              f_subcuenta = 9
            LET cv_cs_fin  = cv_cs_fin  + f_monto_pes
         WHEN f_subcuenta = 7
            LET sar_imss92_fin = sar_imss92_fin + f_monto_pes
         WHEN f_subcuenta = 4
            LET infonavit97_fin = infonavit97_fin + f_monto_pes
         WHEN f_subcuenta = 8
            LET sar_infonavit92_fin = sar_infonavit92_fin + f_monto_pes

         WHEN f_subcuenta = 30 OR
              f_subcuenta = 31 OR
              f_subcuenta = 32
            LET retiro_issste_fin = retiro_issste_fin + f_monto_pes
         WHEN f_subcuenta = 13 OR
              f_subcuenta = 19
            LET sar_issste_fin = sar_issste_fin + f_monto_pes
         WHEN f_subcuenta = 35
            LET fovissste_fin = fovissste_fin + f_monto_pes
         WHEN f_subcuenta = 14
            LET sar_fovissste_fin = sar_fovissste_fin + f_monto_pes

         WHEN f_subcuenta = 3  OR
              f_subcuenta = 10 OR
              f_subcuenta = 22 OR
              f_subcuenta = 23
            LET vol_fin = vol_fin + f_monto_pes
         WHEN f_subcuenta = 11 OR
              f_subcuenta = 12 OR
              f_subcuenta = 24 OR
              f_subcuenta = 25
            LET com_fin = com_fin + f_monto_pes
         WHEN f_subcuenta = 15 OR
              f_subcuenta = 16 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27
            LET largo_plazo_fin = largo_plazo_fin + f_monto_pes
         WHEN f_subcuenta = 33 OR
              f_subcuenta = 34
            LET solidario_fin = solidario_fin + f_monto_pes

         WHEN f_subcuenta = 36
         	  LET bono_pension_fin = bono_pension_fin + f_monto_pes
      END CASE
   END FOREACH

   FREE c_saldo1_sem}

   {LET retiro97_cv_cs_fin = retiro97_fin   +
                            cv_cs_fin

   LET sub_imss_fin   = retiro97_fin         +
                        cv_cs_fin            +
                        sar_imss92_fin       +
                        infonavit97_fin      +
                        sar_infonavit92_fin

   LET sub_issste_fin = retiro_issste_fin  +
                        sar_issste_fin     +
                        fovissste_fin      +
                        sar_fovissste_fin  +
                        bono_pension_fin

   LET sub_vol_com_fin = vol_fin           +
                         com_fin           +
                         largo_plazo_fin   +
                         solidario_fin

   LET total_general_fin    = sub_imss_fin   +
                              sub_issste_fin +
                              sub_vol_com_fin

   LET sub_sem_rcv         = sub_fin_rcv - sub_ini_rcv
   LET sub_viv_sem         = sub_viv_fin - sub_viv_ini
   LET sub_vcom_sem        = sub_vcom_fin - sub_vcom_ini

   LET total_semestre = sub_sem_rcv +
                        sub_viv_sem +
                        sub_vcom_sem}
END FUNCTION
#####################################################################
FUNCTION resumen_comisiones()

   DEFINE xxxx_cuantos  SMALLINT

   DEFINE c_subcuenta       SMALLINT,
          c_monto_pes       DECIMAL(16,2)

   LET comircv = 0
   LET comivol = 0
   LET comisal = 0

   LET estructura_saldo  = 0.20
   LET estructura_flujo1 = 1.03

   DECLARE cur_comisiones CURSOR FOR
   SELECT a.subcuenta,
          ROUND (SUM(a.monto_en_pesos),2)
   FROM   tmp_cuenta a
   WHERE  a.subcuenta   > 0
   AND    a.tipo_movimiento  IN (100,101,102,103,104,105,106,107,108,109,110,111)
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1
{   UNION
   SELECT b.subcuenta,
          ROUND (SUM(b.monto_en_pesos),2)
   FROM   safre_tmp:dis_cuenta_rec b
   WHERE  b.nss = g_reg.nss
   AND    b.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1
}
   FOREACH cur_comisiones INTO c_subcuenta,
                               c_monto_pes
      CASE
         WHEN c_subcuenta = 1
            LET retiro97_comis = retiro97_comis + c_monto_pes
         WHEN c_subcuenta = 2 OR
              c_subcuenta = 5 OR
              c_subcuenta = 6 OR
              c_subcuenta = 9
            LET cv_cs_comis  = cv_cs_comis  + c_monto_pes
         WHEN c_subcuenta = 7
            LET sar_imss92_comis = sar_imss92_comis + c_monto_pes
         WHEN c_subcuenta = 4
            LET infonavit97_comis = infonavit97_comis + c_monto_pes
         WHEN c_subcuenta = 8
            LET sar_infonavit92_comis = sar_infonavit92_comis + c_monto_pes

         WHEN c_subcuenta = 30 OR
              c_subcuenta = 31 OR
              c_subcuenta = 32
            LET retiro_issste_comis = retiro_issste_comis + c_monto_pes
         WHEN c_subcuenta = 13 OR
              c_subcuenta = 19
            LET sar_issste_comis = sar_issste_comis + c_monto_pes
         WHEN c_subcuenta = 35
            LET fovissste_comis = fovissste_comis + c_monto_pes
         WHEN c_subcuenta = 14
            LET sar_fovissste_comis = sar_fovissste_comis + c_monto_pes

         WHEN c_subcuenta = 3  OR
              c_subcuenta = 10 OR
              c_subcuenta = 22 OR
              c_subcuenta = 23
            LET vol_comis = vol_comis + c_monto_pes
         WHEN c_subcuenta = 11 OR
              c_subcuenta = 12 OR
              c_subcuenta = 24 OR
              c_subcuenta = 25
            LET com_comis = com_comis + c_monto_pes
         WHEN c_subcuenta = 15 OR
              c_subcuenta = 16 OR
              c_subcuenta = 26 OR
              c_subcuenta = 27
            LET largo_plazo_comis = largo_plazo_comis + c_monto_pes
         WHEN c_subcuenta = 33 OR
              c_subcuenta = 34
            LET solidario_comis = solidario_comis + c_monto_pes
      END CASE
   END FOREACH

   LET retiro97_cv_cs_comis = retiro97_comis  +
                              cv_cs_comis

   LET sub_imss_comis = retiro97_comis        +
                        cv_cs_comis           +
                        sar_imss92_comis      +
                        infonavit97_comis     +
                        sar_infonavit92_comis

   LET sub_issste_comis = retiro_issste_comis +
                          sar_issste_comis    +
                          fovissste_comis     +
                          sar_fovissste_comis +
                          bono_pension_comis

   LET sub_vol_com_comis = vol_comis         +
                           com_comis         +
                           largo_plazo_comis +
                           solidario_comis

   LET total_general_comis = sub_imss_comis    +
                             sub_issste_comis  +
                             sub_vol_com_comis

END FUNCTION
#####################################################################
FUNCTION obtiene_aportes()

   DEFINE reg_9 RECORD
          periodo_pago    CHAR(6),
          reg_patron      CHAR(11),
          sbc             DECIMAL(16,6),
          dias_cotizados  SMALLINT
   END RECORD

   DEFINE cont_apo      SMALLINT

   DEFINE sel_5   ,
          sel_6   CHAR(1000)

   INSERT INTO tmp_aporte
   SELECT a.folio,
          a.n_seguro,
          a.periodo_pago,
          a.ult_salario_diario,
          a.reg_patronal_imss,
          a.dias_cotz_bimestre,
          a.dias_incap_bimest,
          a.dias_ausent_bimest,
          a.impt_ret,
          a.impt_act_rec_ret,
          a.impt_ces_vej,
          a.impt_act_r_ces_vej,
          a.impt_aport_vol,
          a.impt_aport_pat,
          a.impt_cuota_soc,
          a.impt_aport_est,
          a.impt_aport_esp,
          a.impt_act_cuo_soc,
          a.impt_act_aport_est,
          a.impt_act_cuo_esp
   FROM   dis_det_aporte a
   WHERE  a.folio IN (SELECT b.folio
                      FROM   tmp_folio b)
   AND    a.n_seguro = g_reg.nss

--   CREATE INDEX tmp_aporte_1 ON tmp_aporte (n_seguro)
   UPDATE STATISTICS FOR TABLE tmp_aporte

   SELECT SUM(monto_en_pesos)
   INTO   aporte_vol
   FROM   tmp_cuenta
   WHERE  subcuenta        IN (10,12,16,23)
   AND    tipo_movimiento  = 1
   AND    fecha_conversion <= g_reg.fecha_top
   AND    id_aportante MATCHES "VE-*"

   IF aporte_vol is NULL THEN
      LET aporte_vol = 0
   END IF

   SELECT SUM(a.impt_ret+
              a.impt_act_rec_ret+
              a.impt_ces_vej+
              a.impt_act_r_ces_vej+
              a.impt_aport_vol+
              a.impt_aport_pat+
              a.impt_cuota_soc+
              a.impt_aport_est+
              a.impt_aport_esp+
              a.impt_act_cuo_soc+
              a.impt_act_aport_est+
              a.impt_act_cuo_esp)/100
   INTO   aporte_obl
   FROM   tmp_aporte a
   WHERE  a.n_seguro =  g_reg.nss

   IF aporte_obl is NULL THEN
      LET aporte_obl = 0
   END IF

   LET total_aporte  = 0
   LET total_aporte  = aporte_obl + aporte_vol

   LET cont_apo = 0

   DECLARE cur_aporte CURSOR FOR
   SELECT b.periodo_pago,
          b.reg_patronal_imss,
          ROUND(b.ult_salario_diario/100,2),
          b.dias_cotz_bimestre - b.dias_ausent_bimest
   FROM   tmp_aporte b
   WHERE  b.n_seguro = g_reg.nss
   ORDER BY 1 desc

   FOREACH cur_aporte INTO reg_9.*
      LET cont_apo = cont_apo + 1

      CASE cont_apo
         WHEN 1
            LET periodo_pago_1   = reg_9.periodo_pago
            LET reg_patron_1     = reg_9.reg_patron
            LET sbc_1            = reg_9.sbc
            LET dias_cotizados_1 = reg_9.dias_cotizados
         WHEN 2
            LET periodo_pago_2   = reg_9.periodo_pago
            LET reg_patron_2     = reg_9.reg_patron
            LET sbc_2            = reg_9.sbc
            LET dias_cotizados_2 = reg_9.dias_cotizados
         WHEN 3
            LET periodo_pago_3   = reg_9.periodo_pago
            LET reg_patron_3     = reg_9.reg_patron
            LET sbc_3            = reg_9.sbc
            LET dias_cotizados_3 = reg_9.dias_cotizados
         WHEN 4
            EXIT CASE
      END CASE
   END FOREACH
END FUNCTION #aporte_obl
#####################################################################
FUNCTION obtiene_retiros()

   DEFINE r_subcuenta       SMALLINT,
          r_id_aportante    CHAR(15),
          r_monto_pes       DECIMAL(16,2)

   LET r_monto_pes = 0

   DECLARE cursor_retiros CURSOR FOR
   SELECT a.subcuenta,
          a.id_aportante,
          ROUND (SUM(a.monto_en_pesos),2)
   FROM   tmp_cuenta a
   WHERE  a.nss = g_reg.nss
   AND    a.subcuenta > 0
   AND    a.tipo_movimiento IN ( SELECT b.codigo
                                 FROM   tab_movimiento b
                                 WHERE (b.codigo > 199
                                 OR     b.codigo = 7
                                 OR     b.codigo = 10
                                 OR     b.codigo = 27)
                                 AND    b.tipo = -1
                               )
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1,2

   FOREACH cursor_retiros INTO r_subcuenta,
                               r_id_aportante,
                               r_monto_pes

      IF r_id_aportante = "AJUSTE" OR
         r_id_aportante = "SOLASIG" OR
         r_id_aportante = "INDEBIDO" OR
         r_id_aportante = "CORTE EDAD" OR
         r_id_aportante = "CARGO-PEFOV" OR --tipo_movimiento 960
         r_id_aportante[1,3] = "TDC"  THEN
         CONTINUE FOREACH
      END IF

{      IF r_id_aportante = "CORTE EDAD" OR
         r_id_aportante = "TDC-2-SALDO" THEN
         CONTINUE FOREACH
      END IF
}
      CASE
         WHEN r_subcuenta = 1
            LET retiro97_retiros = retiro97_retiros + r_monto_pes
         WHEN r_subcuenta = 2 OR
              r_subcuenta = 5 OR
              r_subcuenta = 6 OR
              r_subcuenta = 9
            LET cv_cs_retiros  = cv_cs_retiros  + r_monto_pes
         WHEN r_subcuenta = 7
            LET sar_imss92_retiros = sar_imss92_retiros + r_monto_pes
         WHEN r_subcuenta = 4
            LET infonavit97_retiros = infonavit97_retiros + r_monto_pes
         WHEN r_subcuenta = 8
            LET sar_infonavit92_retiros = sar_infonavit92_retiros + r_monto_pes

         WHEN r_subcuenta = 30 OR
              r_subcuenta = 31 OR
              r_subcuenta = 32
            LET retiro_issste_retiros = retiro_issste_retiros + r_monto_pes
         WHEN r_subcuenta = 13 OR
              r_subcuenta = 19
            LET sar_issste_retiros = sar_issste_retiros + r_monto_pes
         WHEN r_subcuenta = 35
            LET fovissste_retiros = fovissste_retiros + r_monto_pes
         WHEN r_subcuenta = 14
            LET sar_fovissste_retiros = sar_fovissste_retiros + r_monto_pes

         WHEN r_subcuenta = 3  OR
              r_subcuenta = 10 OR
              r_subcuenta = 22 OR
              r_subcuenta = 23
            LET vol_retiros = vol_retiros + r_monto_pes
         WHEN r_subcuenta = 11 OR
              r_subcuenta = 12 OR
              r_subcuenta = 24 OR
              r_subcuenta = 25
            LET com_retiros = com_retiros + r_monto_pes
         WHEN r_subcuenta = 15 OR
              r_subcuenta = 16 OR
              r_subcuenta = 26 OR
              r_subcuenta = 27
            LET largo_plazo_retiros = largo_plazo_retiros + r_monto_pes
         WHEN r_subcuenta = 33 OR
              r_subcuenta = 34
            LET solidario_retiros = solidario_retiros + r_monto_pes
         WHEN r_subcuenta = 36
         	  LET bono_pension_retiros = bono_pension_retiros + r_monto_pes
      END CASE
   END FOREACH

   LET retiro97_cv_cs_retiros = retiro97_retiros   +
                                cv_cs_retiros

   LET sub_imss_retiros = retiro97_retiros         +
                          cv_cs_retiros            +
                          sar_imss92_retiros       +
                          infonavit97_retiros      +
                          sar_infonavit92_retiros

   LET sub_issste_retiros = retiro_issste_retiros  +
                            sar_issste_retiros     +
                            fovissste_retiros      +
                            sar_fovissste_retiros  +
                            bono_pension_retiros

   LET sub_vol_com_retiros = vol_retiros           +
                             com_retiros           +
                             largo_plazo_retiros   +
                             solidario_retiros

   LET total_general_retiros    = sub_imss_retiros +
                                  sub_issste_retiros +
                                  sub_vol_com_retiros

END FUNCTION
#####################################################################
FUNCTION aportes_netos()

   DEFINE a_subcuenta       SMALLINT,
          a_tipo_movimiento  SMALLINT,
          a_id_aportante    CHAR(18),
          a_monto_pes       DECIMAL(16,2)

   LET a_monto_pes = 0

   DECLARE cursor_netos CURSOR FOR
   SELECT a.subcuenta,
          a.tipo_movimiento,
          a.id_aportante,
          ROUND (SUM(a.monto_en_pesos),2)
   FROM   tmp_cuenta a
   WHERE  a.nss = g_reg.nss
   AND    a.subcuenta > 0
   AND    a.tipo_movimiento IN (1,2,3,4,5,17)
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1,2,3

   FOREACH cursor_netos INTO a_subcuenta,
                             a_tipo_movimiento,
                             a_id_aportante,
                             a_monto_pes

      IF a_id_aportante = "SOLASIG" OR
         a_id_aportante = "INDEBIDO" THEN
         CONTINUE FOREACH
      END IF

      IF (a_subcuenta = 4 OR
          a_subcuenta = 8 OR
          a_subcuenta = 14 ) AND
          a_tipo_movimiento = 3 THEN
         CONTINUE FOREACH
      END IF

      CASE
      	 WHEN a_subcuenta = 1
            LET retiro97_aporta = retiro97_aporta + a_monto_pes
         WHEN a_subcuenta = 2 OR
              a_subcuenta = 5 OR
              a_subcuenta = 6 OR
              a_subcuenta = 9
            LET cv_cs_aporta  = cv_cs_aporta  + a_monto_pes
         WHEN a_subcuenta = 7
            LET sar_imss92_aporta = sar_imss92_aporta + a_monto_pes
         WHEN a_subcuenta = 4
            LET infonavit97_aporta = infonavit97_aporta + a_monto_pes
         WHEN a_subcuenta = 8
            LET sar_infonavit92_aporta = sar_infonavit92_aporta + a_monto_pes

         WHEN a_subcuenta = 30 OR
              a_subcuenta = 31 OR
              a_subcuenta = 32
            LET retiro_issste_aporta = retiro_issste_aporta + a_monto_pes
         WHEN a_subcuenta = 13 OR
              a_subcuenta = 19
            LET sar_issste_aporta = sar_issste_aporta + a_monto_pes
         WHEN a_subcuenta = 35
            LET fovissste_aporta = fovissste_aporta + a_monto_pes
         WHEN a_subcuenta = 14
            LET sar_fovissste_aporta = sar_fovissste_aporta + a_monto_pes

         WHEN a_subcuenta = 3  OR
              a_subcuenta = 10 OR
              a_subcuenta = 22 OR
              a_subcuenta = 23
            LET vol_aporta = vol_aporta + a_monto_pes
         WHEN a_subcuenta = 11 OR
              a_subcuenta = 12 OR
              a_subcuenta = 24 OR
              a_subcuenta = 25
            LET com_aporta = com_aporta + a_monto_pes
         WHEN a_subcuenta = 15 OR
              a_subcuenta = 16 OR
              a_subcuenta = 26 OR
              a_subcuenta = 27
            LET largo_plazo_aporta = largo_plazo_aporta + a_monto_pes
         WHEN a_subcuenta = 33 OR
              a_subcuenta = 34
            LET solidario_aporta = solidario_aporta + a_monto_pes

         WHEN a_subcuenta = 36
         	  LET bono_pension_aporta = bono_pension_aporta + a_monto_pes
      END CASE
   END FOREACH

   LET retiro97_cv_cs_aporta = retiro97_aporta   +
                               cv_cs_aporta

   LET sub_imss_aporta   = retiro97_aporta         +
                           cv_cs_aporta            +
                           sar_imss92_aporta       +
                           infonavit97_aporta      +
                           sar_infonavit92_aporta

   LET sub_issste_aporta = retiro_issste_aporta  +
                           sar_issste_aporta     +
                           fovissste_aporta      +
                           sar_fovissste_aporta  +
                           bono_pension_aporta

   LET sub_vol_com_aporta = vol_aporta           +
                            com_aporta           +
                            largo_plazo_aporta   +
                            solidario_aporta

   LET total_general_aporta   = sub_imss_aporta  +
                               sub_issste_aporta +
                               sub_vol_com_aporta

END FUNCTION
#####################################################################
FUNCTION rendimiento()

LET retiro97_rend         = (((retiro97_fin - retiro97_ini)-
                               retiro97_aporta)-
                              (retiro97_retiros)-
                               retiro97_comis)

LET cv_cs_rend            = (((cv_cs_fin - cv_cs_ini)-
                               cv_cs_aporta)-
                              (cv_cs_retiros)-
                               cv_cs_comis)

LET retiro97_cv_cs_rend   = retiro97_rend + cv_cs_rend

LET sar_imss92_rend       = (((sar_imss92_fin - sar_imss92_ini)-
                               sar_imss92_aporta)-
                              (sar_imss92_retiros)-
                               sar_imss92_comis)

LET infonavit97_rend      = (((infonavit97_fin - infonavit97_ini)-
                               infonavit97_aporta)-
                              (infonavit97_retiros)-
                               infonavit97_comis)

LET sar_infonavit92_rend  = (((sar_infonavit92_fin - sar_infonavit92_ini)-
                               sar_infonavit92_aporta)-
                              (sar_infonavit92_retiros)-
                               sar_infonavit92_comis)

LET retiro_issste_rend    = (((retiro_issste_fin - retiro_issste_ini)-
                               retiro_issste_aporta)-
                              (retiro_issste_retiros)-
                               retiro_issste_comis)

LET sar_issste_rend       = (((sar_issste_fin - sar_issste_ini)-
                               sar_issste_aporta)-
                              (sar_issste_retiros)-
                               sar_issste_comis)

LET fovissste_rend        = (((fovissste_fin - fovissste_ini)-
                               fovissste_aporta)-
                              (fovissste_retiros)-
                               fovissste_comis)

LET sar_fovissste_rend    = (((sar_fovissste_fin - sar_fovissste_ini)-
                               sar_fovissste_aporta)-
                              (sar_fovissste_retiros)-
                               sar_fovissste_comis)

LET bono_pension_rend     = (((bono_pension_fin - bono_pension_ini)-
                               bono_pension_aporta)-
                              (bono_pension_retiros)-
                               bono_pension_comis)

LET vol_rend              = (((vol_fin - vol_ini)-
                               vol_aporta)-
                              (vol_retiros)-
                               vol_comis)

LET com_rend              = (((com_fin - com_ini)-
                               com_aporta)-
                              (com_retiros)-
                               com_comis)

LET largo_plazo_rend      = (((largo_plazo_fin - largo_plazo_ini)-
                               largo_plazo_aporta)-
                              (largo_plazo_retiros)-
                               largo_plazo_comis)

LET solidario_rend        = (((solidario_fin - solidario_ini)-
                               solidario_aporta)-
                              (solidario_retiros)-
                               solidario_comis)

   LET sub_imss_rend = retiro97_rend        +
                       cv_cs_rend           +
                       sar_imss92_rend      +
                       infonavit97_rend     +
                       sar_infonavit92_rend

   LET sub_issste_rend = retiro_issste_rend +
                         sar_issste_rend    +
                         fovissste_rend     +
                         sar_fovissste_rend +
                         bono_pension_rend

   LET sub_vol_com_rend = vol_rend          +
                          com_rend          +
                          largo_plazo_rend  +
                          solidario_rend

   LET total_general_rend = sub_imss_rend +
                            sub_issste_rend +
                            sub_vol_com_rend

END FUNCTION
#####################################################################
FUNCTION resumengral()
      LET ahorro_retiro_ini     = retiro97_ini         +
                                  cv_cs_ini            +
                                  sar_imss92_ini +
                                  retiro_issste_ini  +
                                  sar_issste_ini     +
                                  bono_pension_ini

      LET ahorro_vol_ini        = sub_vol_com_ini

      LET ahorro_retiro_aporta  = retiro97_aporta         +
                                  cv_cs_aporta            +
                                  sar_imss92_aporta       +
                                  retiro_issste_aporta    +
                                  sar_issste_aporta       +
                                  bono_pension_aporta

      LET ahorro_vol_aporta     = sub_vol_com_aporta

      LET ahorro_retiro_retiros = retiro97_retiros         +
                                  cv_cs_retiros            +
                                  sar_imss92_retiros +
                                  retiro_issste_retiros  +
                                  sar_issste_retiros     +
                                  bono_pension_retiros

      LET ahorro_vol_retiros    = sub_vol_com_retiros

      LET ahorro_retiro_rend    = retiro97_rend         +
                                  cv_cs_rend            +
                                  sar_imss92_rend +
                                  retiro_issste_rend  +
                                  sar_issste_rend     +
                                  bono_pension_rend

      LET ahorro_vol_rend       = sub_vol_com_rend

      LET ahorro_retiro_comis   = retiro97_comis         +
                                  cv_cs_comis            +
                                  sar_imss92_comis +
                                  retiro_issste_comis  +
                                  sar_issste_comis     +
                                  bono_pension_comis

      LET ahorro_vol_comis      = sub_vol_com_comis

      LET ahorro_retiro_fin     = retiro97_fin         +
                                  cv_cs_fin            +
                                  sar_imss92_fin +
                                  retiro_issste_fin  +
                                  sar_issste_fin     +
                                  bono_pension_fin

      LET ahorro_vol_fin        = sub_vol_com_fin

      LET vivienda_ini = infonavit97_ini + sar_infonavit92_ini +
                         fovissste_ini   + sar_fovissste_ini

      LET vivienda_fin = infonavit97_fin + sar_infonavit92_fin +
                         fovissste_fin   + sar_fovissste_fin

      LET vivienda_movimientos = vivienda_fin - vivienda_ini
END FUNCTION
#####################################################################
FUNCTION ingresa_estado()

   LET rendimiento_adicional = 0

   LET estructura_flujo2     = 0
   LET comision_flujo2       = 0
   LET estructura_adicional  = 0
   LET comision_adicional    = 0

   IF x_tipo_informe <> 0 THEN
      LET w_aux.nombres = w_aux.nombres[1,40] CLIPPED," ",
                          w_aux.paterno[1,40] CLIPPED," ",
                          w_aux.materno[1,40] CLIPPED

      LET w_aux.callep = w_aux.callep        CLIPPED," ",
                         w_aux.numep         CLIPPED," ",
                         w_aux.deptop        CLIPPED
   END IF

   IF x_tipo_informe = 3 OR
      x_tipo_informe = 4 OR
      x_tipo_informe = 2 OR
      x_tipo_informe = 9 OR
      x_tipo_informe = 10 THEN
   ELSE
      LET x_fec_liq_tras  = ""
      LET x_fec_sol_tras  = ""
      LET x_fena          = ""
      LET x_nacionalidad_desc = ""
      LET w_aux.fonop = ""

      LET x_nombres = ""
      LET x_desc_afo_recep = ""
      LET x_fecha_reingreso = ""
      LET x_afore_recep = " "
   END IF

   IF x_tipo_informe <> 4 AND
      x_tipo_informe <> 2 THEN
      LET x_correo_electronico = " "
   END IF

   IF g_reg.nss[1] = "I" THEN
      LET null_nss = ""
   ELSE
      LET null_nss = g_reg.nss
   END IF

   LET genera_detalle = pos,"|",
                        g_reg.fecha_ini           USING "DDMMYYYY","|",             --peridodo_desde
                        g_reg.fecha_top           USING "DDMMYYYY","|",             --periodo_hasta
                        w_aux.nombres,"|",                                          --nombre_completo
                        w_aux.callep ,"|",                                          --calle_num
                        w_aux.coloniap,"|",                                         --colonia
                        w_aux.codposp,"|",                                          --cp
                        vcentro_reparto,"|",                                        --Centro de reparto
                        w_aux.estadop,"|",                                          --entidad_federal
                        w_aux.delegap,"|",                                          --munic_delega
                        null_nss,"|",                                               --nss
                        w_aux.rfc,"|",                                              --rfc
                        w_aux.n_unico,"|",                                          --curp

                        total_general_fin             USING "&&&&&&&&.&&","|",      --total_general_fin
                        gi_ind_redencion              USING "&&","|",               --gi_ind_redencion
                        bono_pension_actual           USING "&&&&&&&&.&&","|",      --bono_pension_actual
                        bono_pension_nominal          USING "&&&&&&&&.&&","|",      --bono_pension_nominal
                        gi_bono_pension_actual_pesos  USING "&&&&&&&&.&&","|",      --gi_bono_pension_actual_pesos
                        gi_bono_pension_nominal_pesos USING "&&&&&&&&.&&"           --gi_bono_pension_nominal_pesos

{
display "________________________________________________________________"
display "g_reg.nss           :",g_reg.nss, " ,:",null_nss
display " "
display "retiro97_cv_cs_ini :",retiro97_cv_cs_ini
display "retiro97_ini       :",retiro97_ini
display "cv_cs_ini          :",cv_cs_ini
display " "
display "retiro97_ini       :",retiro97_ini
display "cv_cs_ini          :",cv_cs_ini
display "sar_imss92_ini     :",sar_imss92_ini
display "infonavit97_ini    :",infonavit97_ini
display "sar_infonavit92_ini:",sar_infonavit92_ini
display "sub_imss_ini       :",sub_imss_ini
display " "
display "retiro_issste_ini  :",retiro_issste_ini
display "sar_issste_ini     :",sar_issste_ini
display "fovissste_ini      :",fovissste_ini
display "sar_fovissste_ini  :",sar_fovissste_ini
display "bono_pension_ini   :",bono_pension_ini
display "sub_issste_ini     :",sub_issste_ini
display " "
display "vol_ini            :",vol_ini
display "com_ini            :",com_ini
display "largo_plazo_ini    :",largo_plazo_ini
display "solidario_ini      :",solidario_ini
display "sub_vol_com_ini    :",sub_vol_com_ini
display " "
display "sub_imss_ini       :",sub_imss_ini
display "sub_issste_ini     :",sub_issste_ini
display "sub_vol_com_ini    :",sub_vol_com_ini
display "total_general_ini  :",total_general_ini
display " "
display "retiro97_cv_cs_aporta :",retiro97_cv_cs_aporta
display "retiro97_aporta       :",retiro97_aporta
display "cv_cs_aporta          :",cv_cs_aporta
display " "
display "retiro97_aporta       :",retiro97_aporta
display "cv_cs_aporta          :",cv_cs_aporta
display "sar_imss92_aporta     :",sar_imss92_aporta
display "infonavit97_aporta    :",infonavit97_aporta
display "sar_infonavit92_aporta:",sar_infonavit92_aporta
display "sub_imss_aporta       :",sub_imss_aporta
display " "
display "retiro_issste_aporta  :",retiro_issste_aporta
display "sar_issste_aporta     :",sar_issste_aporta
display "fovissste_aporta      :",fovissste_aporta
display "sar_fovissste_aporta  :",sar_fovissste_aporta
display "bono_pension_aporta   :",bono_pension_aporta
display "sub_issste_aporta     :",sub_issste_aporta
display " "
display "vol_aporta            :",vol_aporta
display "com_aporta            :",com_aporta
display "largo_plazo_aporta    :",largo_plazo_aporta
display "solidario_aporta      :",solidario_aporta
display "sub_vol_com_aporta    :",sub_vol_com_aporta
display " "
display "sub_imss_aporta       :",sub_imss_aporta
display "sub_issste_aporta     :",sub_issste_aporta
display "sub_vol_com_aporta    :",sub_vol_com_aporta
display "total_general_aporta  :",total_general_aporta
display " "
display "retiro97_cv_cs_retiros :",retiro97_cv_cs_retiros
display "retiro97_retiros       :",retiro97_retiros
display "cv_cs_retiros          :",cv_cs_retiros
display " "
display "retiro97_retiros       :",retiro97_retiros
display "cv_cs_retiros          :",cv_cs_retiros
display "sar_imss92_retiros     :",sar_imss92_retiros
display "infonavit97_retiros    :",infonavit97_retiros
display "sar_infonavit92_retiros:",sar_infonavit92_retiros
display "sub_imss_retiros       :",sub_imss_retiros
display " "
display "retiro_issste_retiros  :",retiro_issste_retiros
display "sar_issste_retiros     :",sar_issste_retiros
display "fovissste_retiros      :",fovissste_retiros
display "sar_fovissste_retiros  :",sar_fovissste_retiros
display "bono_pension_retiros   :",bono_pension_retiros
display "sub_issste_retiros     :",sub_issste_retiros
display " "
display "vol_retiros            :",vol_retiros
display "com_retiros            :",com_retiros
display "largo_plazo_retiros    :",largo_plazo_retiros
display "solidario_retiros      :",solidario_retiros
display "sub_vol_com_retiros    :",sub_vol_com_retiros
display " "
display "sub_imss_retiros       :",sub_imss_retiros
display "sub_issste_retiros     :",sub_issste_retiros
display "sub_vol_com_retiros    :",sub_vol_com_retiros
display "total_general_retiros  :",total_general_retiros
display " "
display "retiro97_cv_cs_rend :",retiro97_cv_cs_rend
display "retiro97_rend       :",retiro97_rend
display "cv_cs_rend          :",cv_cs_rend
display " "
display "retiro97_rend       :",retiro97_rend
display "cv_cs_rend          :",cv_cs_rend
display "sar_imss92_rend     :",sar_imss92_rend
display "infonavit97_rend    :",infonavit97_rend
display "sar_infonavit92_rend:",sar_infonavit92_rend
display "sub_imss_rend       :",sub_imss_rend
display " "
display "retiro_issste_rend  :",retiro_issste_rend
display "sar_issste_rend     :",sar_issste_rend
display "fovissste_rend      :",fovissste_rend
display "sar_fovissste_rend  :",sar_fovissste_rend
display "bono_pension_rend   :",bono_pension_rend
display "sub_issste_rend     :",sub_issste_rend
display " "
display "vol_rend            :",vol_rend
display "com_rend            :",com_rend
display "largo_plazo_rend    :",largo_plazo_rend
display "solidario_rend      :",solidario_rend
display "sub_vol_com_rend    :",sub_vol_com_rend
display " "
display "sub_imss_rend       :",sub_imss_rend
display "sub_issste_rend     :",sub_issste_rend
display "sub_vol_com_rend    :",sub_vol_com_rend
display "total_general_rend  :",total_general_rend
display " "
display "retiro97_cv_cs_comis :",retiro97_cv_cs_comis
display "retiro97_comis       :",retiro97_comis
display "cv_cs_comis          :",cv_cs_comis
display " "
display "retiro97_comis       :",retiro97_comis
display "cv_cs_comis          :",cv_cs_comis
display "sar_imss92_comis     :",sar_imss92_comis
display "infonavit97_comis    :",infonavit97_comis
display "sar_infonavit92_comis:",sar_infonavit92_comis
display "sub_imss_comis       :",sub_imss_comis
display " "
display "retiro_issste_comis  :",retiro_issste_comis
display "sar_issste_comis     :",sar_issste_comis
display "fovissste_comis      :",fovissste_comis
display "sar_fovissste_comis  :",sar_fovissste_comis
display "bono_pension_comis   :",bono_pension_comis
display "sub_issste_comis     :",sub_issste_comis
display " "
display "vol_comis            :",vol_comis
display "com_comis            :",com_comis
display "largo_plazo_comis    :",largo_plazo_comis
display "solidario_comis      :",solidario_comis
display "sub_vol_com_comis    :",sub_vol_com_comis
display " "
display "sub_imss_comis       :",sub_imss_comis
display "sub_issste_comis     :",sub_issste_comis
display "sub_vol_com_comis    :",sub_vol_com_comis
display "total_general_comis  :",total_general_comis
display " "
display "retiro97_cv_cs_fin :",retiro97_cv_cs_fin
display "retiro97_fin       :",retiro97_fin
display "cv_cs_fin          :",cv_cs_fin
display " "
display "retiro97_fin       :",retiro97_fin
display "cv_cs_fin          :",cv_cs_fin
display "sar_imss92_fin     :",sar_imss92_fin
display "infonavit97_fin    :",infonavit97_fin
display "sar_infonavit92_fin:",sar_infonavit92_fin
display "sub_imss_fin       :",sub_imss_fin
display " "
display "retiro_issste_fin  :",retiro_issste_fin
display "sar_issste_fin     :",sar_issste_fin
display "fovissste_fin      :",fovissste_fin
display "sar_fovissste_fin  :",sar_fovissste_fin
display "bono_pension_fin   :",bono_pension_fin
display "sub_issste_fin     :",sub_issste_fin
display " "
display "vol_fin            :",vol_fin
display "com_fin            :",com_fin
display "largo_plazo_fin    :",largo_plazo_fin
display "solidario_fin      :",solidario_fin
display "sub_vol_com_fin    :",sub_vol_com_fin
display " "
display "sub_imss_fin       :",sub_imss_fin
display "sub_issste_fin     :",sub_issste_fin
display "sub_vol_com_fin    :",sub_vol_com_fin
display "total_general_fin  :",total_general_fin
display "________________________________________________________________"
}
END FUNCTION
#####################################################################
FUNCTION informacion_fiscal()

   DEFINE xsubcuenta    SMALLINT,
          xfecha_fis    DATE,
          xmonto_fis    DECIMAL(16,6),
          xtipo_fis     SMALLINT,
          fcont         SMALLINT,
          vfecha_fiscal DATE

   LET fecha_rcv_fiscal  = ""
   LET monto_rcv_fiscal = 0
   LET imp_rcv_fiscal   = 0

   LET fecha_sar_fiscal  = ""
   LET monto_sar_fiscal = 0
   LET imp_sar_fiscal   = 0

   LET fecha_sar_issste_fiscal = ""
   LET monto_sar_issste_fiscal = 0
   LET imp_sar_issste_fiscal   = 0

   LET fecha_vol_fiscal = ""
   LET monto_vol_fiscal = 0
   LET imp_vol_fiscal   = 0

   LET fecha_com_fiscal = ""
   LET monto_com_fiscal = 0
   LET imp_com_fiscal   = 0

   LET fecha_alp_fiscal = ""
   LET monto_alp_fiscal = 0
   LET imp_alp_fiscal   = 0

   LET fecha_vol_fiscal2 = ""

   LET ind_fiscal       = "0"
   LET ind_int_fiscal   = "0"

   LET interes_real     = 0
   LET interes_nominal  = 0

--IF g_reg.status = 50 THEN

   LET vfecha_fiscal    = ""
   LET dia           = DAY(g_reg.fecha_top)-1
   LET mes           = MONTH(g_reg.fecha_top)-1
   LET vfecha_fiscal = g_reg.fecha_top - dia UNITS DAY
   LET vfecha_fiscal = vfecha_fiscal - mes UNITS MONTH

   SELECT "X"
   FROM   tmp_cuenta
   WHERE  subcuenta        IN (1,2,3,5,6,7,9,10,11,12,13,16,23)
   AND    tipo_movimiento  = 10
   AND    fecha_conversion >= vfecha_fiscal
   AND    fecha_conversion <= g_reg.fecha_top
   GROUP BY 1

   IF  STATUS <> NOTFOUND THEN
      LET ind_fiscal       = "1"

      WHENEVER ERROR CONTINUE
      SET LOCK MODE TO WAIT
      DROP TABLE tmp_fiscal
      CREATE TEMP TABLE tmp_fiscal
       (tipo_movimiento   SMALLINT NOT NULL ,
        subcuenta         SMALLINT NOT NULL ,
        siefore           SMALLINT,
        folio             INTEGER NOT NULL ,
        consecutivo_lote  INTEGER,
        nss               CHAR(11) NOT NULL ,
        curp              CHAR(18),
        folio_sua         CHAR(6),
        fecha_pago        DATE,
        fecha_valor       DATE,
        fecha_conversion  DATE,
        monto_en_pesos    DECIMAL(22,6),
        monto_en_acciones DECIMAL(22,6),
        precio_accion     DECIMAL(22,6),
        dias_cotizados    SMALLINT,
        sucursal          CHAR(10),
        id_aportante      CHAR(11),
        estado            SMALLINT,
        fecha_proceso     DATE,
        usuario           CHAR(8),
        fecha_archivo     DATE,
        etiqueta          SMALLINT
       )
      SET LOCK MODE TO NOT WAIT

      INSERT INTO tmp_fiscal
      SELECT *
      FROM   dis_cuenta
      WHERE  nss = g_reg.nss
      AND    fecha_conversion >= vfecha_fiscal
      AND    fecha_conversion <= g_reg.fecha_top

      CREATE INDEX tmp_fiscal_1 on tmp_fiscal (subcuenta,
                                               tipo_movimiento)

      UPDATE STATISTICS FOR TABLE tmp_fiscal

      DECLARE vol_1 CURSOR FOR
      SELECT fecha_conversion,
             SUM(monto_en_pesos)*(-1),
             tipo_movimiento
      FROM   tmp_fiscal
      WHERE  subcuenta in(1,2,5,6,9)
      AND  ( tipo_movimiento =  10 OR
             tipo_movimiento BETWEEN 413 AND 485 OR
             tipo_movimiento BETWEEN 488 AND 490 OR
             tipo_movimiento BETWEEN 493 AND 499 OR
             tipo_movimiento IN ( 820,830,840,850,860,880)
           )
      GROUP BY 1,3

      LET fcont = 1

      FOREACH vol_1 INTO xfecha_fis,
                         xmonto_fis,
                         xtipo_fis

         IF xtipo_fis = 10 THEN
            LET imp_rcv_fiscal   = imp_rcv_fiscal   + xmonto_fis
         ELSE
            LET fecha_rcv_fiscal = xfecha_fis
            LET monto_rcv_fiscal = monto_rcv_fiscal + xmonto_fis
            LET fcont = fcont + 1
         END IF
      END FOREACH

      IF monto_rcv_fiscal IS NULL THEN
         LET monto_rcv_fiscal = 0
      END IF

      DECLARE com_3 CURSOR FOR
      SELECT subcuenta,
             fecha_conversion,
             SUM(monto_en_pesos)*(-1),
             tipo_movimiento
      FROM   tmp_fiscal
      WHERE  subcuenta IN (3,7,10,11,12,16,23)
      AND   (tipo_movimiento BETWEEN 400 AND 499
      OR     tipo_movimiento BETWEEN 800 AND 899
      OR     tipo_movimiento = 10)
      GROUP BY 1,2,4
      ORDER BY 1,2,4

      LET fcont = 1

      FOREACH com_3 INTO xsubcuenta,
                         xfecha_fis,
                         xmonto_fis,
                         xtipo_fis

         CASE
            WHEN xsubcuenta = 7
               IF xtipo_fis = 10 THEN
                  LET imp_sar_fiscal   = imp_sar_fiscal   + xmonto_fis
               ELSE
                  LET fecha_sar_fiscal = xfecha_fis
                  LET monto_sar_fiscal = monto_sar_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
            WHEN xsubcuenta = 13
               IF xtipo_fis = 10 THEN
                  LET imp_sar_issste_fiscal   = imp_sar_issste_fiscal   + xmonto_fis
               ELSE
                  LET fecha_sar_issste_fiscal = xfecha_fis
                  LET monto_sar_issste_fiscal = monto_sar_issste_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
               --- OJO falta 22 ?
            WHEN xsubcuenta = 3 OR xsubcuenta = 10 OR xsubcuenta = 23
               IF xtipo_fis = 10 THEN
                  LET imp_vol_fiscal   = imp_vol_fiscal   + xmonto_fis
               ELSE
                  LET fecha_vol_fiscal = xfecha_fis
                  LET monto_vol_fiscal = monto_vol_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
                ---  OJO falta 24 y 25 ?
            WHEN xsubcuenta = 11 OR xsubcuenta = 12
               IF xtipo_fis = 10 THEN
                  LET imp_com_fiscal   = imp_com_fiscal   + xmonto_fis
               ELSE
                  LET fecha_com_fiscal = xfecha_fis
                  LET monto_com_fiscal = monto_com_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
            WHEN xsubcuenta = 16
               IF xtipo_fis = 10 THEN
                  LET imp_alp_fiscal   = imp_alp_fiscal   + xmonto_fis
               ELSE
                  LET fecha_alp_fiscal = xfecha_fis
                  LET monto_alp_fiscal = monto_alp_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
         END CASE
      END FOREACH

      IF monto_sar_fiscal IS NULL THEN
         LET monto_sar_fiscal = 0
      END IF

      IF monto_sar_issste_fiscal IS NULL THEN
         LET monto_sar_issste_fiscal = 0
      END IF

      IF monto_vol_fiscal IS NULL THEN
         LET monto_vol_fiscal = 0
      END IF

      IF monto_com_fiscal IS NULL THEN
         LET monto_com_fiscal = 0
      END IF

      IF monto_alp_fiscal IS NULL THEN
         LET monto_alp_fiscal = 0
      END IF

      EXECUTE eje_sel_8 USING g_reg.nss,
                              vfecha_fiscal,
                              g_reg.fecha_top

      IF SQLCA.SQLCODE = 0 THEN
         LET ind_int_fiscal   = "1"

         CALL ren_fiscal(g_reg.nss,vfecha_fiscal)
              RETURNING interes_real,
                        interes_nominal
      END IF
   END IF
--END IF

END FUNCTION
###############################################################
FUNCTION ren_fiscal(x_nss,vfecha_fiscal)

   DEFINE x_nss           CHAR(11)
   DEFINE vfecha_fiscal   DATE

   DEFINE gr_prom RECORD
          fecha_apo        DATE,
          fecha_liq        DATE,
          mto_neto         DECIMAL(16,6),
          mto_retencion    DECIMAL(16,6),
          mto_rendimiento  DECIMAL(16,6)
   END RECORD

   DEFINE i_precio_fin         DECIMAL(16,6),
          i_precio_ini         DECIMAL(16,6),
          v_precio_fin         DECIMAL(16,6),
          v_precio_ini         DECIMAL(16,6),
          vtasa_tot            DECIMAL(16,6),
          vtasa_prom           DECIMAL(16,6),
          vdia_prom            INTEGER,
          vdias_prom           INTEGER,
          vconta_prom          INTEGER,
          vacum_tasa           DECIMAL(16,6),
          acum_vdia_prom       INTEGER,
          vrendimiento_nominal DECIMAL(16,6),
          factor_infla         DECIMAL(16,6),
          mac                  DECIMAL(16,6),
          infla_per            DECIMAL(16,6),
          ren_real             DECIMAL(16,6),
          ren_real_tot         DECIMAL(16,6),
          coe_ord              DECIMAL(16,6),
          tasa_real            DECIMAL(16,6),
          tasa_real_tot        DECIMAL(16,6),
          vtasa_rprom          DECIMAL(16,6),
          vret_tot             DECIMAL(16,6),
          vnet_tot             DECIMAL(16,6),
          total_accion         DECIMAL(16,6),
          importe_ini          DECIMAL(16,6),
          importe_fin          DECIMAL(16,6),
          vdatei               DECIMAL(16,6),
          vdatef               DECIMAL(16,6),
          vmf                  INTEGER,
          vmi                  INTEGER

   LET vtasa_tot        = 0
   LET vdias_prom       = 0
   LET vconta_prom      = 0
   LET vacum_tasa       = 0
   LET acum_vdia_prom   = 0
   LET ren_real_tot     = 0
   LET tasa_real_tot    = 0
   LET vrendimiento_nominal = 0

   DECLARE cur_col CURSOR FOR eje_sel_9

   FOREACH cur_col USING x_nss,
                         vfecha_fiscal,
                         g_reg.fecha_top
                    INTO gr_prom.*

      LET vdatei       = ""
      LET vdatef       = ""
      LET i_precio_fin = 0
      LET i_precio_ini = 0
      LET v_precio_fin = 0
      LET v_precio_ini = 0

      IF gr_prom.fecha_apo < "01/14/2005" THEN
         LET v_precio_ini = precio_siefore (2,gr_prom.fecha_apo)
      ELSE
         LET v_precio_ini = precio_siefore (1,gr_prom.fecha_apo)
      END IF

      LET v_precio_fin = precio_siefore (1,gr_prom.fecha_liq)

      SELECT valor_udi
      INTO   i_precio_ini
      FROM   tab_udi
      WHERE  fecha_udi = gr_prom.fecha_apo

      SELECT valor_udi
      INTO   i_precio_fin
      FROM   tab_udi
      WHERE  fecha_udi = gr_prom.fecha_liq

      LET factor_infla   = 0
      LET infla_per      = 0
      LET mac            = 0
      LET ren_real       = 0
      LET total_accion   = 0
      LET importe_ini    = 0
      LET importe_fin    = 0

      LET total_accion   = (gr_prom.mto_neto + gr_prom.mto_retencion)/
                           v_precio_fin

      LET importe_ini    = total_accion * v_precio_ini
      LET importe_fin    = total_accion * v_precio_fin
      LET gr_prom.mto_rendimiento = 0

      LET gr_prom.mto_rendimiento = importe_fin - importe_ini
      LET factor_infla   = (i_precio_fin / i_precio_ini) -1
      LET mac            = (gr_prom.mto_neto + gr_prom.mto_retencion) -
                            gr_prom.mto_rendimiento
      LET infla_per      = factor_infla * mac
      LET ren_real       = gr_prom.mto_rendimiento - infla_per
      LET ren_real_tot   = ren_real_tot + ren_real
      LET vrendimiento_nominal = vrendimiento_nominal + gr_prom.mto_rendimiento
      INITIALIZE gr_prom.* TO NULL

   END FOREACH

   RETURN ren_real_tot,
          vrendimiento_nominal

END FUNCTION
###################################################################
FUNCTION calcula_dias(fecha)
    DEFINE
        fecha           DATE

    CASE MONTH(fecha)            #Regresa dias del mes anterior
        WHEN  1   RETURN 31
        WHEN  2   IF YEAR(fecha) MOD 4 = 0 THEN
                      RETURN 29
                  ELSE
                      RETURN 28
                  END IF
        WHEN  3   RETURN 31
        WHEN  4   RETURN 30
        WHEN  5   RETURN 31
        WHEN  6   RETURN 30
        WHEN  7   RETURN 31
        WHEN  8   RETURN 31
        WHEN  9   RETURN 30
        WHEN 10   RETURN 31
        WHEN 11   RETURN 30
        WHEN 12   RETURN 31
    END CASE
END FUNCTION
####################################################
FUNCTION precio_siefore(x_siefore,x_fecha_valuacion)

   DEFINE x_siefore          SMALLINT,
          x_fecha_valuacion  DATE,
          x_precio_accion    DECIMAL(19,14)

   SELECT precio_del_dia
   INTO   x_precio_accion
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = x_fecha_valuacion
   AND    codigo_siefore  = x_siefore

   IF SQLCA.SQLCODE <> 0 THEN
      LET x_precio_accion = 0
   END IF

   RETURN x_precio_accion

END FUNCTION
################################################
FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vestado,vresultado)

   DEFINE vfolio         INTEGER,
          vetapa_cod     DECIMAL(2,0),
          vresultado     CHAR(50),
          vestado        SMALLINT

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES
      (TODAY,             -- fecha_proceso
       "CTA",             -- proceso_cod
       vetapa_cod,        -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       NULL,              -- parametro1
       vestado,           -- parametro2
       NULL,              -- parametro3
       NULL,              -- parametro4
       NULL,              -- parametro5
       vfolio,            -- folio
       vresultado,        -- resultado
       USER,              -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vpos,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          vpos         INTEGER,
          hoy          DATE

   DEFINE hora_inicial       CHAR(08),
          vhora_final        CHAR(08)

   DEFINE cla_sel            CHAR(400),
          vconsecutivo       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTA' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        parametro3 = ",vpos,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'CTA'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
#*********************************************************************
FUNCTION sumarios()

   DEFINE xc_fecha            DATE,
          cont_com            SMALLINT

   DEFINE com_1 ARRAY [25] OF RECORD
          afore_cod    SMALLINT,
          afore_desc   CHAR(30),
          porcentaje   DECIMAL(6,2),
          fecha        DATE
   END RECORD

   DEFINE com_2 ARRAY [25] OF RECORD
          afore_cod         SMALLINT,
          afore_desc        CHAR(30),
          rendimiento       DECIMAL(6,2),
          comision          DECIMAL(6,2),
          rendimiento_neto  DECIMAL(6,2),
          fecha_fin         DATE
   END RECORD

   DEFINE com_1x ARRAY [25] OF RECORD
          porcentaje   CHAR(5)
   END RECORD

   LET genera_sum99 = HOY USING "DDMMYYYY","|",
                      --"1","|",
                      x_folio USING "&&&&&&&&","|",
                      pos USING "&&&&&&&&","|"

   OUTPUT TO REPORT r_report(genera_sum99,232,99)

END FUNCTION
#####################################################################
FUNCTION habil_anterior(diaActual)

   DEFINE diaTmp          DATE,
          contador        SMALLINT,
          diaActual       DATE,
          diaHabilAnt     DATE,
          diaSemana       SMALLINT,
          feriado         SMALLINT,
          finSemana       SMALLINT

   LET diaHabilAnt = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilAnt)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = diaHabilAnt

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF feriado = 1 OR finSemana = 1 THEN
          LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilAnt

END FUNCTION #habil_anterior
#####################################################################
FUNCTION nombre_archivo(arc_tipo_informe,hoy,x_folio,usuario)

   DEFINE arc_tipo_informe    SMALLINT,
          arc_nombre          CHAR(100),
          x_archivo           CHAR(100)

   DEFINE archivo RECORD
          ruta_envio           CHAR(40),
          numero_parametro     SMALLINT,
          tipo_dato            SMALLINT,
          valor                CHAR(10),
          formato              CHAR(10)
   END RECORD

   DEFINE x_formato            CHAR(20),
          pos                  SMALLINT,
          numero_parametros    SMALLINT

   DEFINE hoy                  DATE,
          x_folio              INTEGER,
          usuario              CHAR(10)

   SELECT COUNT(*)
   INTO   numero_parametros
   FROM   cta_formato_archivos
   WHERE  modulo_cod  = "CTA"
   AND    tipo_informe = arc_tipo_informe

   DECLARE cur_archivo CURSOR FOR
   SELECT  ruta_envio,
           numero_parametro,
           tipo_dato,
           valor,
           formato
   FROM    cta_formato_archivos
   WHERE   modulo_cod  = "CTA"
   AND     tipo_informe = arc_tipo_informe
   ORDER BY 2

   LET pos = 1
   FOREACH cur_archivo INTO archivo.*
      IF archivo.tipo_dato = 2 THEN
         CASE
            WHEN archivo.valor = "HOY"
               LET x_formato = hoy USING archivo.formato CLIPPED
            WHEN archivo.valor = "g_usuario"
               LET x_formato = usuario CLIPPED
            WHEN archivo.valor = "x_folio"
               LET x_formato = x_folio USING archivo.formato CLIPPED
         END CASE
      END IF

      IF pos = numero_parametros THEN
         IF archivo.tipo_dato = 1 THEN
            LET arc_nombre = archivo.valor CLIPPED
         ELSE
            LET arc_nombre = x_formato CLIPPED
         END IF
      ELSE
         IF archivo.tipo_dato = 1 THEN
            IF pos = 1 THEN
               LET arc_nombre = archivo.ruta_envio CLIPPED,"/",
                                archivo.valor CLIPPED
            ELSE
               LET arc_nombre = archivo.valor CLIPPED
            END IF
         ELSE
            IF pos = 1 THEN
               LET arc_nombre = archivo.ruta_envio CLIPPED,"/",
                                x_formato CLIPPED
            ELSE
               LET arc_nombre = x_formato CLIPPED
            END IF
         END IF
      END IF
      LET x_archivo = x_archivo CLIPPED,arc_nombre CLIPPED

      LET x_formato = ""
      LET pos = pos + 1
   END FOREACH

   RETURN x_archivo
END FUNCTION
#############################################
FUNCTION correo()

   DEFINE   xx_mail    CHAR(200)
   DEFINE   xxx_mail   CHAR(200)
   DEFINE   x_cod_mail SMALLINT

   DEFINE sw_1     SMALLINT,
          sw       SMALLINT

{
   SELECT cod_correo_e,
          correo_elect
   FROM   afi_correo_elect
   WHERE  nss = g_reg.nss
   ORDER BY 1
}

   DECLARE cur_mail CURSOR FOR
   SELECT cod_correo_e,
          correo_elect
   FROM   afi_correo_elect
   WHERE  nss = g_reg.nss
   AND    factualiza IN (SELECT MAX(a.factualiza)
                         FROM   afi_correo_elect a
                         WHERE  a.nss = g_reg.nss
                         AND    a.cod_correo_e = 1)

   LET sw_1 = 0
   LET sw   = 0
   FOREACH cur_mail INTO x_cod_mail,
                         xx_mail

      IF xx_mail IS NULL OR xx_mail = " " THEN
         LET xx_mail = " "
      END IF

      CASE x_cod_mail
         WHEN 1
            LET xxx_mail = xx_mail CLIPPED,";"
            LET xxx_mail = xxx_mail
            LET sw_1 = 1
         OTHERWISE
            LET xxx_mail = xxx_mail CLIPPED,xx_mail CLIPPED,";"
            LET xxx_mail = xxx_mail
            LET sw = 1
      END CASE
   END FOREACH

   IF sw = 1 AND sw_1 = 0 THEN
      LET xxx_mail = xxx_mail,";"
   END IF

   IF sw = 0 AND sw_1 = 1 THEN
      LET xxx_mail = xxx_mail,";"
   END IF

   LET x_correo_electronico = xxx_mail

END FUNCTION
##############################################################3
FUNCTION dias_del_annio()

   DEFINE dia_inicial DATE,
          dia_final   DATE,
          dias_annio  SMALLINT

   LET dia_inicial = MDY(01,01,year(HOY))
   LET dia_final   = MDY(12,31,year(HOY))

   LET dias_annio = (dia_final - dia_inicial) + 1

   LET total_dias_annio = dias_annio

END FUNCTION
#####################################################################
FUNCTION obten_rendimientos_infonavit_fovissste()
   DEFINE li_tipo_informe,
          li_folio          SMALLINT

   DEFINE ld_fproceso,
          ld_frendimiento   DATE

   DECLARE cur_fecha_ini CURSOR FOR
      SELECT a.fecha_inicio,
             a.tipo_informe
      INTO   ld_fproceso, li_tipo_informe
      FROM   cta_ctr_proceso a
      WHERE  a.folio = x_folio
      GROUP BY 1,2

   FOREACH cur_fecha_ini INTO ld_fproceso, li_tipo_informe
   END FOREACH

   IF MONTH(ld_fproceso) > 6 THEN
   	  LET ld_frendimiento = MDY(06,30,YEAR(ld_fproceso))
   ELSE
   	  LET ld_frendimiento = MDY(12,31,YEAR(ld_fproceso)-1)
   END IF
{
display "x_folio: ", x_folio
display "ld_frendimiento: ", ld_frendimiento
display "li_tipo_informe: ", li_tipo_informe
}
   SELECT rendimiento
   INTO   rendimiento_infonavit
   FROM   cta_rendimiento_ctr a,
          cta_param_ctr b
   WHERE  a.folio_rendimiento = b.folio_rendimiento
   AND    b.fecha_fin         = ld_frendimiento
   AND    b.tipo_informe      = li_tipo_informe
   AND    a.codigo_siefore    = 11

   SELECT rendimiento
   INTO   rendimiento_fovissste
   FROM   cta_rendimiento_ctr a,
          cta_param_ctr b
   WHERE  a.folio_rendimiento = b.folio_rendimiento
   AND    b.fecha_fin         = ld_frendimiento
   AND    b.tipo_informe      = li_tipo_informe
   AND    a.codigo_siefore    = 0

END FUNCTION
################################################################################
FUNCTION obten_rendimientos_siefore1_2()
   DEFINE li_tipo_informe,
          li_folio          SMALLINT

   DEFINE ld_fproceso,
          ld_frendimiento   DATE

   DECLARE cur_fecha_ini2 CURSOR FOR
      SELECT a.fecha_inicio,
             a.tipo_informe
      FROM   cta_ctr_proceso a
      WHERE  a.folio = x_folio
      GROUP BY 1,2

   FOREACH cur_fecha_ini2 INTO ld_fproceso, li_tipo_informe
   END FOREACH

   IF MONTH(ld_fproceso) > 6 THEN
   	  LET ld_frendimiento = MDY(06,30,YEAR(ld_fproceso))
   ELSE
   	  LET ld_frendimiento = MDY(12,31,YEAR(ld_fproceso)-1)
   END IF
{
display "ld_frendimiento: ", ld_frendimiento
display "li_tipo_informe: ", li_tipo_informe
}
   SELECT a.rendimiento
   INTO   rendimiento_siefore1
   FROM   cta_rendimiento_ctr a,
          cta_param_ctr b
   WHERE  a.folio_rendimiento = b.folio_rendimiento
   AND    b.fecha_fin         = ld_frendimiento
   AND    b.tipo_informe      = li_tipo_informe
   AND    a.codigo_siefore    = 1

   SELECT a.rendimiento
   INTO   rendimiento_siefore2
   FROM   cta_rendimiento_ctr a,
          cta_param_ctr b
   WHERE  a.folio_rendimiento = b.folio_rendimiento
   AND    b.fecha_fin         = ld_frendimiento
   AND    b.tipo_informe      = li_tipo_informe
   AND    a.codigo_siefore    = 2
END FUNCTION
################################################################################
FUNCTION bono()
   DEFINE li_anio_edc,
          li_anio_reden SMALLINT

   EXECUTE get_bono USING g_reg.nss,
                          g_reg.fecha_top
                    INTO  gd_fecha_redencion           ,  --fecha_reden
                          bono_pension_nominal         ,  --bono_nominal
                          bono_pension_actual          ,  --bono_real
                          gi_bono_pension_nominal_pesos,  --bono_nominal_pesos
                          gi_bono_pension_actual_pesos    --bono_real_pesos

   LET li_anio_edc   = YEAR(g_reg.fecha_top) + 1
   LET li_anio_reden = YEAR(gd_fecha_redencion)

   IF li_anio_edc =  li_anio_reden THEN
   	  LET gi_ind_redencion = 1

   	  LET bono_pension_nominal          = 0
      LET bono_pension_actual           = 0
   	  LET gi_bono_pension_nominal_pesos = 0
      LET gi_bono_pension_actual_pesos  = 0
   END IF

END FUNCTION
################################################################################