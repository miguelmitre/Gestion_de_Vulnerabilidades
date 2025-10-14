##############################################################################
#Proyecto            => SAFRE  ( MEXICO )                                    #
#Owner               => E.F.P.                                               #
#Programa CTAB040    => ESTADO DE CUENTA                                     #
#Sistema             => CTA                                                  #
#Por                 => MIGUEL ANGEL HERNANDEZ MARTINEZ                      #
#Fecha               => 17 de julio de 2008                                  #
#Fecha               => 08 de agosto de 2008                                 #
#Fecha               => 11 de septiembre de 2008                             #
#CPL-1483            => EDC tercer cuatrimstre 2013                          #
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
   DEFINE gi_bono_pension_actual        DECIMAL(22,6)

   #Cuatrimestre1 2009
   DEFINE gd_periodo_pago_issste      CHAR(06)
   DEFINE gi_dias_cotizados_issste    SMALLINT
   DEFINE gi_sbc_issste               ,
          gi_vivienda_aporta          ,
          gi_vivienda_retiros         ,
          gi_vivienda_rend            ,
          gi_vivienda_comis           ,
          gi_solidario_trab_ini       ,
          gi_solidario_patron_ini     ,
          gi_solidario_trab_aporta    ,
          gi_solidario_patron_aporta  ,
          gi_solidario_trab_retiros   ,
          gi_solidario_patron_retiros ,
          gi_solidario_trab_rend      ,
          gi_solidario_patron_rend    ,
          gi_solidario_trab_comis     ,
          gi_solidario_patron_comis   ,
          gi_solidario_trab_fin       ,
          gi_solidario_patron_fin     ,
          gi_bono_udis_recibido       ,
          gi_bono_pesos_recibido      ,
          gi_comision_afore           DECIMAL(22,6)

   #Cuatrimestre3 2009
   DEFINE gc_folio CHAR(20)

   #Cuatrimestre1 2010
   DEFINE gs_ind_bono SMALLINT

   #Cuatrimestre1 2011
   DEFINE gs_ind_afil SMALLINT

   #Cuatrimestre2 2011
   DEFINE gs_redencion_anticipada SMALLINT

   #Cuatrimestre3 2011
   DEFINE gi_ap_obrero_pat_imss  ,
          gi_ap_estado_imss      ,
          gi_ap_obrero_pat_issste,
          gi_ap_estado_issste    DECIMAL(22,6)

   DEFINE gs_bono_uni            SMALLINT
END GLOBALS
#####################################################################
MAIN

   LET x_tipo_proceso = ARG_VAL(1)
   LET x_folio        = ARG_VAL(2)
   LET x_estado       = ARG_VAL(3)

   DISPLAY "INICIA PROCESO DE ESTADO DE CUENTA",
           " CON FOLIO:",x_folio ," DEL ESTADO:",x_estado

   CALL STARTLOG("CTAB040.log")

   CALL inicializa()
   CALL dias_del_annio()

   CALL crea_tablas()

   IF x_tipo_proceso = 0 THEN
      CALL prepara_nss_semestral()
   ELSE
      CALL prepara_nss()
   END IF

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA:",pos

END MAIN
#####################################################################
FUNCTION crea_tablas()
   CREATE TEMP TABLE tmp_folio
   (folio INTEGER )

   sql
   CREATE INDEX tmp_folio_1 ON tmp_folio(folio) IN tmp_dbs2
   END sql

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

   #Cuatrimestre3 2011
   CREATE TEMP TABLE tmp_aporte
   (folio                INTEGER     ,
    n_seguro             CHAR(11)    ,
    periodo_pago         CHAR(6)     ,
    fech_pago            CHAR(08)    ,
    consec_reg_lote      INTEGER     ,
    ult_salario_diario   DECIMAL(9,2),
    reg_patronal_imss    CHAR(11)    ,
    dias_cotz_bimestre   SMALLINT    ,
    dias_incap_bimest    SMALLINT    ,
    dias_ausent_bimest   SMALLINT    ,
    impt_ret             DECIMAL(12,2),
    impt_act_rec_ret     DECIMAL(12,2),
    impt_ces_vej         DECIMAL(12,2),
    impt_act_r_ces_vej   DECIMAL(12,2),
    impt_aport_vol       DECIMAL(12,2),
    impt_aport_compl     DECIMAL(12,2),
    impt_aport_pat       DECIMAL(12,2),
    impt_cuota_soc       DECIMAL(12,2),
    impt_aport_est       DECIMAL(12,2),
    impt_aport_esp       DECIMAL(12,2),
    impt_act_cuo_soc     DECIMAL(12,2),
    impt_act_aport_est   DECIMAL(12,2),
    impt_act_cuo_esp     DECIMAL(12,2)
    )
   sql
   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta, tipo_movimiento) IN tmp_dbs2
   END sql
   sql
   CREATE INDEX tmp_cuenta_2 ON tmp_cuenta (fecha_conversion) IN tmp_dbs2
   END sql
   sql
   CREATE INDEX tmp_cuenta_3 ON tmp_cuenta (id_aportante) IN tmp_dbs2
   END sql
   sql
   CREATE INDEX tmp_aporte_1 ON tmp_aporte (n_seguro) IN tmp_dbs2
   END sql

   #Cuatrimestre3 2011
   CREATE TEMP TABLE tmp_aporte_issste
   (
    folio                INTEGER      ,
    periodo_pago         CHAR(6)      ,
    fech_pago            CHAR(08)     ,
    consec_reg_lote      INTEGER      ,
    sueldo_base_cot_rcv  DECIMAL(12,2),
    dias_cot_bimestre    SMALLINT     ,
    dias_ausen_bimestre  SMALLINT     ,
    impt_sar_isss        DECIMAL(12,2),  --13 OBRERO
    impt_ret_isss        DECIMAL(12,2),  --30 OBRERO
    impt_cv_isss_pat     DECIMAL(12,2),  --31 OBRERO
    impt_cv_isss_tra     DECIMAL(12,2),  --31 OBRERO
    inte_ext_sar_isss    DECIMAL(12,2),  --13 OBRERO
    inte_ext_ret_isss    DECIMAL(12,2),  --30 OBRERO
    inte_ext_cv_isss_pat DECIMAL(12,2),  --31 OBRERO
    inte_ext_cv_isss_tra DECIMAL(12,2),  --31 OBRERO
    impt_cs_isss         DECIMAL(12,2)   --32 ESTADO
   )
   --CREATE INDEX tmp_aporte_issste_1 ON tmp_aporte_issste (n_unico)
   sql
   CREATE TEMP TABLE tmp_folio_issste
   (folio INTEGER ) IN tmp_dbs2
   END sql
   sql
   CREATE INDEX tmp_folio_issste_1 ON tmp_folio_issste(folio) IN tmp_dbs2
   END sql
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
          sel_10,
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

   LET sel_10 = "SELECT a.n_seguro, ",
                       "b.curp    , ",
                       "a.n_folio,  ",
                       "NVL(TRIM(nombres ),' ')|| ' ' || ",
                       "NVL(TRIM(a.paterno),' ')||' '|| ",
                       "NVL(TRIM(a.materno),' '), ",
                       "a.n_rfc, ",
                       "'0', ",
                       "a.fentcons ",
                "FROM   afi_mae_afiliado a, ",
                "       cta_nss_edo_cta b   ",
                "WHERE  a.n_seguro = b.nss  ",
                "AND    a.n_seguro = ? "

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
   PREPARE eje_sel_10 FROM  sel_10

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
   LET gi_bono_pension_actual        = 0

   #Cuatrimestre1 2009
   LET gd_periodo_pago_issste      = ""
   LET gi_dias_cotizados_issste    = 0
   LET gi_sbc_issste               = 0
   LET gi_vivienda_aporta          = 0
   LET gi_vivienda_retiros         = 0
   LET gi_vivienda_rend            = 0
   LET gi_vivienda_comis           = 0
   LET gi_solidario_trab_ini       = 0
   LET gi_solidario_patron_ini     = 0
   LET gi_solidario_trab_aporta    = 0
   LET gi_solidario_patron_aporta  = 0
   LET gi_solidario_trab_retiros   = 0
   LET gi_solidario_patron_retiros = 0
   LET gi_solidario_trab_rend      = 0
   LET gi_solidario_patron_rend    = 0
   LET gi_solidario_trab_comis     = 0
   LET gi_solidario_patron_comis   = 0
   LET gi_solidario_trab_fin       = 0
   LET gi_solidario_patron_fin     = 0
   LET gi_bono_udis_recibido       = 0
   LET gi_bono_pesos_recibido      = 0
   LET gi_comision_afore           = 0

   #Cuatrimestre2 2011
   LET gs_redencion_anticipada = 0

   #Cuatrimestre3 2011
   LET gi_ap_obrero_pat_imss   = 0
   LET gi_ap_estado_imss       = 0
   LET gi_ap_obrero_pat_issste = 0
   LET gi_ap_estado_issste     = 0

   #Cuatrimestre3 2012
   LET gs_bono_uni             = 0
END FUNCTION
#####################################################################
FUNCTION estado_cuenta()

   DEFINE rend_infonavit,
          rend_fovissste SMALLINT

   DEFINE ld_fecha_top DATE

   IF MONTH(hoy) > 6 THEN
      LET ld_fecha_top = MDY(06,30,YEAR(hoy))
   ELSE
   	  LET ld_fecha_top = MDY(12,31,YEAR(hoy)-1)
   END IF

   LET rend_infonavit = 11
   LET rend_fovissste = 0

   CALL ini()
   CALL genera_patrones()

   IF x_tipo_informe = 0 THEN

      LET rendimiento_infonavit = 3.99
      LET rendimiento_fovissste = 1.03

      CALL datos_trabajador_sem()
      CALL obtiene_retiros()
      --Obtener valor real y nominal del bono

      #Cuatrimestre2 2009
      IF x_estado = 40 OR
      	 x_estado = 41 OR

      	 #Cuatrimestre1 2010
      	 gs_ind_bono = 40 OR
      	 gs_ind_bono = 41 OR
      	 gs_ind_bono = 42 OR

      	 x_estado = 92 OR
      	 x_estado = 93 THEN
      	 CALL bono()
      END IF

      CALL resumen_saldos_sem()

   ELSE

      LET rendimiento_infonavit = 12.67
      LET rendimiento_fovissste = 11.75

      CALL datos_trabajador()
      CALL correo()

      CALL obtiene_retiros()

      SELECT "X"
      FROM   dis_det_bono
      WHERE  n_unico IN (SELECT n_unico
                         FROM   afi_mae_afiliado
                         WHERE  n_seguro = g_reg.nss)
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
      	 CALL bono()
      END IF

      #Cuatrimestre2 2009
      SELECT "X"
    	FROM   taa_viv_recepcion
    	WHERE  nss          = g_reg.nss
    	AND    importe_bono > 0
    	GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
      	 CALL bono()
      END IF

      #Cuatrimestre3 2012
      #Verificar si tiene bono en unificacion
      LET gs_bono_uni = existe_bono_uni(g_reg.nss      ,
                                        g_reg.fecha_ini,
                                        g_reg.fecha_top)
      IF gs_bono_uni > 0 THEN
      	 CALL bono()
      END IF

      CALL resumen_saldos()
   END IF

   CALL resumen_comisiones()
   CALL obtiene_aportes()

   CALL aportes_netos()

   CALL rendimiento()

   CALL resumengral()

   CALL ingresa_estado()

END FUNCTION
#####################################################################
FUNCTION prepara_nss()
   DEFINE v_arch              CHAR(100),
          permisos            CHAR(50)

   DEFINE ls_ano_alta   SMALLINT
   DEFINE ls_ano_actual SMALLINT

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

   OUTPUT TO REPORT r_report(genera_encabezado,231,1)

   FOREACH cur_fechas INTO g_reg.fecha_ini,
                           g_reg.fecha_top

      LET ls_ano_actual = YEAR(g_reg.fecha_top) - 2000

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

         #Cuatrimestre2 2010
         INITIALIZE gc_folio TO NULL

         sql
         SELECT folio
         INTO   $gc_folio
         FROM   cta_nss_edo_cta
         WHERE  nss = $g_reg.nss
         END sql

         #Cuatrimestre1 2011
         --Indicador de alta de la cuenta
         IF g_reg.nss[1] = 'I' THEN
            LET gs_ind_afil = 2; --98 y posteriores
         ELSE
       	    LET ls_ano_alta = g_reg.nss[3,4];

       	    IF ls_ano_alta <= 97 THEN
       	    	 LET gs_ind_afil = 1; --97 y anteriores
       	    ELSE
       	    	 LET gs_ind_afil = 2; --98 y posteriores
       	    END IF

       	    IF ls_ano_alta >= 0             AND
       	    	 ls_ano_alta <= ls_ano_actual THEN
       	    	 LET gs_ind_afil = 2; --98 y posteriores
       	    END IF
         END IF

         CALL estado_cuenta()  #ec

         OUTPUT TO REPORT r_report(genera_detalle,231,2)
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

   #CPL-1483
   LET g_reg.fecha_ini = "09/01/2013"
   LET g_reg.fecha_top = "12/31/2013"

   CALL Ingresa_etapa(x_folio,2,x_estado,"Inicia calculo de estado de cuenta")

   LET v_arch_sem = g_parametro.ruta_envio CLIPPED,"/",
                    "ECS",x_folio USING "<<<<<<<<<"

   START REPORT r_report TO v_arch_sem

   LET genera_encabezado = g_afore.codigo_afore,"|",
                           HOY USING "DDMMYYYY","|",
                           "1","|",
                           x_tipo_informe

   OUTPUT TO REPORT r_report(genera_encabezado,231,1)

   CALL pide_parametros()

   #Cuatrimestre3 2009
   --Recuperamos el folio
   #Cuatrimestre1 2010
   --Recuperamos indicador de bono
   #Cuatrimestre1 2011
   --Recuperamos indicador de alta de la cuenta
   DECLARE cur_nss_sem CURSOR FOR
   SELECT a.nss     ,
          a.status  ,
          a.ind_edad,
          a.folio   ,
          a.ind_bono,
          a.ind_afil
   FROM   cta_nss_edo_cta a
   WHERE  a.estado = x_estado
   --AND    a.nss in ("I0075903867",
   --                 "I0085413228",
   --                 "43825402274")
   ORDER BY a.nss

   FOREACH cur_nss_sem INTO g_reg.nss      ,
                            g_reg.status   ,
                            reg_gen.siefore,
                            gc_folio       ,
                            gs_ind_bono    ,
                            gs_ind_afil

      LET pos = pos + 1

      --Cambio de criterio para Coppel

      --LET reg_gen.siefore = 0

      --sql
      --
      --SELECT codigo_siefore
      --INTO   $reg_gen.siefore
      --FROM   cta_regimen
      --WHERE  nss       = $g_reg.nss
      --AND    subcuenta = 1
      --GROUP BY 1
      --
      --END sql

      CALL estado_cuenta()  #ec

      OUTPUT TO REPORT r_report(genera_detalle,231,2)

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

   DEFINE salir           SMALLINT

   DEFINE rend_sie1,
          rend_sie2 SMALLINT

   DEFINE ld_fecha_top DATE

   IF MONTH(hoy) > 6 THEN
      LET ld_fecha_top = MDY(06,30,YEAR(hoy))
   ELSE
   	  LET ld_fecha_top = MDY(12,31,YEAR(hoy)-1)
   END IF

   LET rend_sie1 = 1
   LET rend_sie2 = 2

   LET salir = FALSE

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
   #Cuatrimestre1 2009
   DELETE FROM tmp_folio
   DELETE FROM tmp_folio_issste

      INSERT INTO tmp_folio
      SELECT a.folio
      FROM   dis_dep_aporte a
      WHERE  a.fech_liquidacion >= g_reg.fecha_ini
      AND    a.fech_liquidacion <= g_reg.fecha_top
      AND    a.estado = 3
      --AND    a.ident_pago[14,15] IN (11,23)
      AND    a.ident_pago[14,15] IN (40, -- adicional
                                     41, -- rcv
                                     42, -- gubernamentales
                                     45, -- voluntarias
                                     46, -- complementarias de retiro
                                     49) -- largo plazo

---      CREATE INDEX tmp_folio_1 ON tmp_folio(folio)
      UPDATE STATISTICS FOR TABLE tmp_folio

   INSERT INTO tmp_folio_issste
   SELECT a.folio
   FROM   dis_dep_issste a
   WHERE  a.fech_liquidacion >= g_reg.fecha_ini
   AND    a.fech_liquidacion <= g_reg.fecha_top
   AND    a.estado = 3
   AND    a.ident_pago[14,15] IN (41,48,49,'4A','4B')
   GROUP BY 1

   UPDATE STATISTICS FOR TABLE tmp_folio_issste

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
   DEFINE lc_anio         CHAR(04)
   DEFINE lc_tabla        CHAR(12)
   DEFINE lc_sql          CHAR(2000)

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

   #Cuatrimestre1 2009
   DELETE FROM tmp_aporte_issste

   LET lc_anio = YEAR (g_reg.fecha_top)
   LET lc_sql  = " SELECT 'X'  ",
                 " FROM   dis_cuenta", lc_anio[3,4],
                 " WHERE 1 = 0 "

   WHENEVER ERROR CONTINUE
      PREPARE valida_split FROM lc_sql
   WHENEVER ERROR STOP

   IF SQLCA.SQLCODE <> 0 THEN
   	  #Si no existe split
      INSERT INTO tmp_cuenta
      SELECT a.*
      FROM   dis_cuenta a
      WHERE  a.nss = g_reg.nss
   ELSE
   	  #Si existe split
      DECLARE cur_split CURSOR FOR
      SELECT tabname
      FROM   systables
      WHERE  tabname matches "dis_cuenta??"

      LET lc_sql = "INSERT INTO tmp_cuenta"

      FOREACH cur_split INTO lc_tabla
         LET lc_sql = lc_sql CLIPPED,
                      " SELECT * ",
                      " FROM   ",lc_tabla CLIPPED,
                      " WHERE  nss = ","'",g_reg.nss,"'",
                      " AND    tipo_movimiento NOT IN (888,999)",
                      " UNION ALL "
      END FOREACH

      LET lc_sql =  lc_sql CLIPPED,
                    " SELECT * ",
                    " FROM   dis_cuenta ",
                    " WHERE  nss = ","'",g_reg.nss,"'",
                    " AND    tipo_movimiento NOT IN (888,999)"

      PREPARE eje_sel_split FROM lc_sql
      EXECUTE eje_sel_split
   END IF
{
   WHENEVER ERROR STOP

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta,
                                            tipo_movimiento)
   CREATE INDEX tmp_cuenta_2 ON tmp_cuenta (fecha_conversion)
   CREATE INDEX tmp_cuenta_3 ON tmp_cuenta (id_aportante)
}
   UPDATE STATISTICS FOR TABLE tmp_cuenta

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

    SELECT "X"
    FROM   cta_id_datos
    WHERE  nss = g_reg.nss
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN

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
    ELSE
    	 EXECUTE eje_sel_10 USING g_reg.nss
                         INTO w_aux.seguro,
                              w_aux.n_unico,
                              w_aux.n_folio,
                              w_aux.nombres,
                              w_aux.rfc,
                              w_aux.infonavit,
                              w_aux.fentcons
    END IF

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
                          " WHERE subcuenta NOT IN (4,8,14,35)",
                          " AND   siefore = codigo_siefore",
                          " AND   fecha_valuacion = ","'",pfecha_saldo,"'",
                          " UNION ",
                          " SELECT subcuenta,",
                                  "siefore, ",
                                  "monto_en_acciones,",
                                 " CASE WHEN siefore = 0 THEN ",
                                           "monto_en_pesos ",
                                      " ELSE ",
                                           "monto_en_acciones * precio_del_dia",
                                  " END monto_en_pesos",
                          " FROM  tmp_saldo,glo_valor_accion",
                          " WHERE subcuenta IN (4,8,14,35)",
                          " AND   siefore = codigo_siefore",
                          " AND   fecha_valuacion = ","'",v_fecha_viv,"'",
                          " INTO TEMP tmp_saldo_dia"

                          {" SELECT subcuenta,",
                                  "siefore,",
                                  "monto_en_acciones,",
                                  "monto_en_acciones * ",v_precio_viv,
                          " FROM   tmp_saldo",
                          " WHERE  subcuenta IN (4,8,14,35)",
                          " INTO TEMP tmp_saldo_dia"}
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

   sql
   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta(subcuenta,
                                                  siefore,
                                                  folio,
                                                  consecutivo_lote) IN tmp_dbs2
   END sql
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
              f_subcuenta = 32 OR
              f_subcuenta = 39
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
              f_subcuenta = 21 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27 OR
              f_subcuenta = 37
            LET largo_plazo_ini = largo_plazo_ini + f_monto_pes

         #Cuatrimestre1 2009
         WHEN f_subcuenta = 33
         	  LET solidario_ini = solidario_ini + f_monto_pes
         	  LET gi_solidario_patron_ini = gi_solidario_patron_ini + f_monto_pes
         WHEN f_subcuenta = 34
            LET solidario_ini = solidario_ini + f_monto_pes
            LET gi_solidario_trab_ini = gi_solidario_trab_ini + f_monto_pes

         {WHEN f_subcuenta = 36
         	  LET bono_pension_ini = bono_pension_ini + f_monto_pes}
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
              f_subcuenta = 32 OR
              f_subcuenta = 39
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
              f_subcuenta = 21 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27 OR
              f_subcuenta = 37
            LET largo_plazo_fin = largo_plazo_fin + f_monto_pes

         #Cuatrimestre1 2009
         WHEN f_subcuenta = 33
         	  LET solidario_fin = solidario_fin + f_monto_pes
         	  LET gi_solidario_patron_fin = gi_solidario_patron_fin + f_monto_pes
         WHEN f_subcuenta = 34
            LET solidario_fin = solidario_fin + f_monto_pes
            LET gi_solidario_trab_fin = gi_solidario_trab_fin + f_monto_pes

         {WHEN f_subcuenta = 36
         	  LET bono_pension_fin = bono_pension_fin + f_monto_pes}
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
--******************************************************************************
--SALDO INICIAL
--******************************************************************************

   DECLARE c_saldo_sem CURSOR FOR eje_sel_5

   FOREACH c_saldo_sem USING g_reg.nss,
                             xfecha_inix
                        INTO f_subcuenta,
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
              f_subcuenta = 32 OR
              f_subcuenta = 39
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
              f_subcuenta = 21 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27 OR
              f_subcuenta = 37
            LET largo_plazo_ini = largo_plazo_ini + f_monto_pes

         #Cuatrimestre1 2009
         WHEN f_subcuenta = 33
         	  LET solidario_ini = solidario_ini + f_monto_pes
         	  LET gi_solidario_patron_ini = gi_solidario_patron_ini + f_monto_pes
         WHEN f_subcuenta = 34
            LET solidario_ini = solidario_ini + f_monto_pes
            LET gi_solidario_trab_ini = gi_solidario_trab_ini + f_monto_pes

         {WHEN f_subcuenta = 36
         	  LET bono_pension_ini = bono_pension_ini + f_monto_pes}
      END CASE
   END FOREACH

   FREE c_saldo_sem

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

   DECLARE c_saldo1_sem CURSOR FOR eje_sel_5


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
              f_subcuenta = 32 OR
              f_subcuenta = 39
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
              f_subcuenta = 21 OR
              f_subcuenta = 26 OR
              f_subcuenta = 27 OR
              f_subcuenta = 37
            LET largo_plazo_fin = largo_plazo_fin + f_monto_pes

         #Cuatrimestre1 2009
         WHEN f_subcuenta = 33
         	  LET solidario_fin = solidario_fin + f_monto_pes
         	  LET gi_solidario_patron_fin = gi_solidario_patron_fin + f_monto_pes
         WHEN f_subcuenta = 34
            LET solidario_fin = solidario_fin + f_monto_pes
            LET gi_solidario_trab_fin = gi_solidario_trab_fin + f_monto_pes

         {WHEN f_subcuenta = 36
         	  LET bono_pension_fin = bono_pension_fin + f_monto_pes}
      END CASE
   END FOREACH

   FREE c_saldo1_sem

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
FUNCTION resumen_comisiones()

   DEFINE xxxx_cuantos  SMALLINT

   DEFINE c_subcuenta       SMALLINT,
          c_monto_pes       DECIMAL(16,2)

   #Cuatrimestre3 2009
   --Al iniciar la funcion cambiamos las fechas
   --Fechas especiales Coppel
   --IF x_tipo_proceso = 0 THEN
      #CPL-1483
      LET g_reg.fecha_ini = "09/01/2013"
      LET g_reg.fecha_top = "01/10/2014"
   --END IF

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
   AND    a.tipo_movimiento  IN (100,
                                 101,
                                 102,
                                 103,
                                 104,
                                 105,
                                 106,
                                 107,
                                 108,
                                 109,
                                 110,
                                 111)
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
              c_subcuenta = 32 OR
              c_subcuenta = 39
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
              c_subcuenta = 21 OR
              c_subcuenta = 26 OR
              c_subcuenta = 27 OR
              c_subcuenta = 37
            LET largo_plazo_comis = largo_plazo_comis + c_monto_pes

         #Cuatrimestre1 2009
         WHEN c_subcuenta = 33
         	  LET solidario_comis = solidario_comis + c_monto_pes
         	  LET gi_solidario_patron_comis = gi_solidario_patron_comis + c_monto_pes
         WHEN c_subcuenta = 34
            LET solidario_comis = solidario_comis + c_monto_pes
            LET gi_solidario_trab_comis = gi_solidario_trab_comis + c_monto_pes
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

   #Cuatrimestre1 2009
   LET gi_vivienda_comis = infonavit97_comis     +
                           sar_infonavit92_comis +
                           fovissste_comis       +
                           sar_fovissste_comis

   #Cuatrimestre3 2010
   --Devolvemos los valores originales Coppel
   --IF x_tipo_proceso = 0 THEN
      #CPL-1483
   	  LET g_reg.fecha_ini = "09/01/2013"
      LET g_reg.fecha_top = "12/31/2013"
   --END IF
END FUNCTION
#####################################################################
FUNCTION obtiene_aportes()
   #Cuatrimestre3 2011
   DEFINE reg_9 RECORD
          periodo_pago         CHAR(6)      ,
          fech_pago            CHAR(08)     ,
          consec_reg_lote      INTEGER      ,
          reg_patron           CHAR(11)     ,
          sbc                  DECIMAL(16,6),
          dias_cotizados       SMALLINT     ,
          impt_ret             DECIMAL(12,2),
          impt_act_rec_ret     DECIMAL(12,2),
          impt_ces_vej         DECIMAL(12,2),
          impt_act_r_ces_vej   DECIMAL(12,2),
          impt_aport_vol       DECIMAL(12,2),
          impt_aport_compl     DECIMAL(12,2),
          impt_aport_pat       DECIMAL(12,2),
          impt_cuota_soc       DECIMAL(12,2),
          impt_aport_est       DECIMAL(12,2),
          impt_aport_esp       DECIMAL(12,2),
          impt_act_cuo_soc     DECIMAL(12,2),
          impt_act_aport_est   DECIMAL(12,2),
          impt_act_cuo_esp     DECIMAL(12,2)
   END RECORD

   #Cuatrimestre3 2011
   DEFINE lr_aporte_issste RECORD
   	      perido_pago              CHAR(06)     ,
   	      fech_pago                CHAR(08)     ,
          consec_reg_lote          INTEGER      ,
          dias_cotizados           SMALLINT     ,
          sbc                      DECIMAL(22,6),
   	      impt_sar_isss       ,
          impt_ret_isss       ,
          impt_cv_isss_pat    ,
          impt_cv_isss_tra    ,
          inte_ext_sar_isss   ,
          inte_ext_ret_isss   ,
          inte_ext_cv_isss_pat,
          inte_ext_cv_isss_tra,
          impt_cs_isss        DECIMAL(12,2)
   END RECORD

   DEFINE cont_apo      SMALLINT

   DEFINE sel_5   ,
          sel_6   CHAR(1000)
          
   #Cuatrimestre3 2011
   INSERT INTO tmp_aporte
   SELECT a.folio              ,                                          --folio
          a.n_seguro           ,                                          --n_seguro
          a.periodo_pago       ,                                          --periodo_pago
          a.fech_pago          ,                                          --fech_pago
          a.consec_reg_lote    ,                                          --consec_reg_lote
          a.ult_salario_diario ,                                          --ult_salario_diario
          a.reg_patronal_imss  ,                                          --reg_patronal_imss
          a.dias_cotz_bimestre ,                                          --dias_cotz_bimestre
          a.dias_incap_bimest  ,                                          --dias_incap_bimest
          a.dias_ausent_bimest ,                                          --dias_ausent_bimest
          a.impt_ret           / 100 , --1 patronal      --dividir /100   --impt_ret
          a.impt_act_rec_ret   / 100 , --1 patronal      --dividir /100   --impt_act_rec_ret
          a.impt_ces_vej       / 100 , --2 patronal      --dividir /100   --impt_ces_vej
          a.impt_act_r_ces_vej / 100 , --2 patronal      --dividir /100   --impt_act_r_ces_vej
          a.impt_aport_vol     / 100 , --                                 --impt_aport_vol
          a.impt_aport_compl   / 100 , --11 PATRONAL CPL                  --impt_aport_compl
          a.impt_aport_pat     / 100 , --3  PATRONAL CPL --dividir /100   --impt_aport_vol
          a.impt_cuota_soc     / 100 , --5 estatal       --dividir /100   --impt_cuota_soc
          a.impt_aport_est     / 100 , --6 estatal       --dividir /100   --impt_aport_est
          a.impt_aport_esp     / 100 , --9 estatal       --dividir /100   --impt_aport_esp
          a.impt_act_cuo_soc   / 100 , --5 estatal       --dividir /100   --impt_act_cuo_soc
          a.impt_act_aport_est / 100 , --6 estatal       --dividir /100   --impt_act_aport_est
          a.impt_act_cuo_esp   / 100   --9 estatal       --dividir /100   --impt_act_cuo_esp
   FROM   dis_det_aporte a
   WHERE  a.folio IN (SELECT b.folio
                      FROM   tmp_folio b)
   AND    a.n_seguro         = g_reg.nss
   AND    a.result_operacion = "01"
   GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23

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

   #Cuatrimestre3 2011
   DECLARE cur_aporte_imss_rcv CURSOR FOR
   sql
   SELECT b.periodo_pago                             ,   --periodo_pago
          b.fech_pago                                ,   --fech_pago
          b.consec_reg_lote                          ,   --consec_reg_lote
          b.reg_patronal_imss                        ,   --reg_patron
          ROUND(b.ult_salario_diario/100,2)          ,   --sbc
          b.dias_cotz_bimestre - b.dias_ausent_bimest,   --dias_cotizados
          b.impt_ret                                 ,   --impt_ret
          b.impt_act_rec_ret                         ,   --impt_act_rec_ret
          b.impt_ces_vej                             ,   --impt_ces_vej
          b.impt_act_r_ces_vej                       ,   --impt_act_r_ces_vej
          b.impt_aport_vol                           ,   --impt_aport_vol
          b.impt_aport_compl                         ,   --impt_aport_compl
          b.impt_aport_pat                           ,   --impt_aport_pat
          b.impt_cuota_soc                           ,   --impt_cuota_soc
          b.impt_aport_est                           ,   --impt_aport_est
          b.impt_aport_esp                           ,   --impt_aport_esp
          b.impt_act_cuo_soc                         ,   --impt_act_cuo_soc
          b.impt_act_aport_est                       ,   --impt_act_aport_est
          b.impt_act_cuo_esp                             --impt_act_cuo_esp
   FROM   tmp_aporte b
   WHERE  b.n_seguro = $g_reg.nss
   AND    b.folio IN (SELECT folio
                      FROM   dis_cza_aporte
                      WHERE  ident_arch = 0)
   ORDER BY 1 DESC, 2 DESC, 3 DESC
   END sql

   #Obtener patronales IMSS
   INITIALIZE reg_9.* TO NULL
   FOREACH cur_aporte_imss_rcv INTO reg_9.*
      LET cont_apo = cont_apo + 1
      LET reg_9.sbc = reg_9.sbc * 30
      #Cuatrimestre3 2011
      CASE cont_apo
         WHEN 1
            LET periodo_pago_1   = reg_9.periodo_pago
            LET reg_patron_1     = reg_9.reg_patron
            LET sbc_1            = reg_9.sbc
            LET dias_cotizados_1 = reg_9.dias_cotizados

            LET gi_ap_obrero_pat_imss = reg_9.impt_ret           +
                                        reg_9.impt_act_rec_ret   +
                                        reg_9.impt_ces_vej       +
                                        reg_9.impt_act_r_ces_vej +
                                        reg_9.impt_aport_vol     +
                                        reg_9.impt_aport_compl
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

   #Obtener estatal con el periodo de pago de patronales IMSS
   DECLARE cur_aporte_imss_est CURSOR FOR
   	sql
   SELECT b.periodo_pago                             ,   --periodo_pago
          b.fech_pago                                ,   --fech_pago
          b.consec_reg_lote                          ,   --consec_reg_lote
          b.reg_patronal_imss                        ,   --reg_patron
          ROUND(b.ult_salario_diario/100,2)          ,   --sbc
          b.dias_cotz_bimestre - b.dias_ausent_bimest,   --dias_cotizados
          b.impt_ret                                 ,   --impt_ret
          b.impt_act_rec_ret                         ,   --impt_act_rec_ret
          b.impt_ces_vej                             ,   --impt_ces_vej
          b.impt_act_r_ces_vej                       ,   --impt_act_r_ces_vej
          b.impt_aport_vol                           ,   --impt_aport_vol
          b.impt_aport_compl                         ,   --impt_aport_compl
          b.impt_aport_pat                           ,   --impt_aport_pat
          b.impt_cuota_soc                           ,   --impt_cuota_soc
          b.impt_aport_est                           ,   --impt_aport_est
          b.impt_aport_esp                           ,   --impt_aport_esp
          b.impt_act_cuo_soc                         ,   --impt_act_cuo_soc
          b.impt_act_aport_est                       ,   --impt_act_aport_est
          b.impt_act_cuo_esp                             --impt_act_cuo_esp
   FROM   tmp_aporte b
   WHERE  b.n_seguro = $g_reg.nss
   AND    b.periodo_pago = $periodo_pago_1
   AND    b.folio IN (SELECT folio
                      FROM   dis_cza_aporte
                      WHERE  ident_arch = 1)
   ORDER BY 1 DESC, 2 DESC, 3 DESC
   END sql

   INITIALIZE reg_9.* TO NULL
   FOREACH cur_aporte_imss_est INTO reg_9.*
   	  LET gi_ap_estado_imss     = reg_9.impt_cuota_soc     +
                                  reg_9.impt_aport_est     +
                                  reg_9.impt_aport_esp     +
                                  reg_9.impt_act_cuo_soc   +
                                  reg_9.impt_act_aport_est +
                                  reg_9.impt_act_cuo_esp
      EXIT FOREACH
   END FOREACH

   #Cuatrimestre3 2011
   INSERT INTO tmp_aporte_issste
   SELECT a.folio                     ,                --folio
          a.periodo_pago              ,                --periodo_pago
          a.fech_pago                 ,                --fech_pago
          a.consec_reg_lote           ,                --consec_reg_lote
          a.sueldo_base_cot_rcv       ,                --sueldo_base_cot_rcv
          a.dias_cot_bimestre         ,                --dias_cot_bimestre
          a.dias_ausen_bimestre       ,                --dias_ausen_bimestre
          a.impt_sar_isss        / 100,  --13 OBRERO   --impt_sar_isss
          a.impt_ret_isss        / 100,  --30 OBRERO   --impt_ret_isss
          a.impt_cv_isss_pat     / 100,  --31 OBRERO   --impt_cv_isss_pat
          a.impt_cv_isss_tra     / 100,  --31 OBRERO   --impt_cv_isss_tra
          a.inte_ext_sar_isss    / 100,  --13 OBRERO   --inte_ext_sar_isss
          a.inte_ext_ret_isss    / 100,  --30 OBRERO   --inte_ext_ret_isss
          a.inte_ext_cv_isss_pat / 100,  --31 OBRERO   --inte_ext_cv_isss_pat
          a.inte_ext_cv_isss_tra / 100,  --31 OBRERO   --inte_ext_cv_isss_tra
          a.impt_cs_isss         / 100   --32 ESTADO   --impt_cs_isss
   FROM   dis_det_issste a
   WHERE  a.folio IN (SELECT b.folio
                      FROM   tmp_folio_issste b)
   AND    a.n_unico          = w_aux.n_unico
   AND    a.result_operacion <> '02'
   GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16

   UPDATE STATISTICS FOR TABLE tmp_aporte_issste

   #Obtener patronales ISSSTE
   DECLARE cur_aporte_issste_rcv CURSOR FOR
   sql
   SELECT b.periodo_pago                             , --
          b.fech_pago                                , --
          b.consec_reg_lote                          , --
          b.dias_cot_bimestre - b.dias_ausen_bimestre, --
          ROUND(b.sueldo_base_cot_rcv/100,2)         , --
          b.impt_sar_isss                            , --impt_sar_isss
          b.impt_ret_isss                            , --impt_ret_isss
          b.impt_cv_isss_pat                         , --impt_cv_isss_pat
          b.impt_cv_isss_tra                         , --impt_cv_isss_tra
          b.inte_ext_sar_isss                        , --inte_ext_sar_isss
          b.inte_ext_ret_isss                        , --inte_ext_ret_isss
          b.inte_ext_cv_isss_pat                     , --inte_ext_cv_isss_pat
          b.inte_ext_cv_isss_tra                     , --inte_ext_cv_isss_tra
          b.impt_cs_isss                               --impt_cs_isss
   FROM   tmp_aporte_issste b
   WHERE  b.folio IN (SELECT folio
                      FROM   dis_cza_issste
                      WHERE  ident_arch = 1)
   ORDER BY 1 DESC, 2 DESC, 3 DESC
   END sql

   FOREACH cur_aporte_issste_rcv INTO gd_periodo_pago_issste                ,
   	                                  lr_aporte_issste.fech_pago            ,
   	                                  lr_aporte_issste.consec_reg_lote      ,
                                      gi_dias_cotizados_issste              ,
                                      gi_sbc_issste                         ,
                                      lr_aporte_issste.impt_sar_isss        ,
                                      lr_aporte_issste.impt_ret_isss        ,
                                      lr_aporte_issste.impt_cv_isss_pat     ,
                                      lr_aporte_issste.impt_cv_isss_tra     ,
                                      lr_aporte_issste.inte_ext_sar_isss    ,
                                      lr_aporte_issste.inte_ext_ret_isss    ,
                                      lr_aporte_issste.inte_ext_cv_isss_pat ,
                                      lr_aporte_issste.inte_ext_cv_isss_tra ,
                                      lr_aporte_issste.impt_cs_isss

      LET gi_ap_obrero_pat_issste = lr_aporte_issste.impt_sar_isss       +
                                    lr_aporte_issste.impt_ret_isss       +
                                    lr_aporte_issste.impt_cv_isss_pat    +
                                    lr_aporte_issste.impt_cv_isss_tra    +
                                    lr_aporte_issste.inte_ext_sar_isss   +
                                    lr_aporte_issste.inte_ext_ret_isss   +
                                    lr_aporte_issste.inte_ext_cv_isss_pat+
                                    lr_aporte_issste.inte_ext_cv_isss_tra
      EXIT FOREACH
   END FOREACH

   #Obtener estatal con el periodo de pago de patronales ISSSTE
   DECLARE cur_aporte_issste_est CURSOR FOR
   sql
   SELECT b.periodo_pago                             , --
          b.fech_pago                                , --
          b.consec_reg_lote                          , --
          b.dias_cot_bimestre - b.dias_ausen_bimestre, --
          ROUND(b.sueldo_base_cot_rcv/100,2)         , --
          b.impt_sar_isss                            , --impt_sar_isss
          b.impt_ret_isss                            , --impt_ret_isss
          b.impt_cv_isss_pat                         , --impt_cv_isss_pat
          b.impt_cv_isss_tra                         , --impt_cv_isss_tra
          b.inte_ext_sar_isss                        , --inte_ext_sar_isss
          b.inte_ext_ret_isss                        , --inte_ext_ret_isss
          b.inte_ext_cv_isss_pat                     , --inte_ext_cv_isss_pat
          b.inte_ext_cv_isss_tra                     , --inte_ext_cv_isss_tra
          b.impt_cs_isss                               --impt_cs_isss
   FROM   tmp_aporte_issste b
   WHERE  b.folio IN (SELECT folio
                      FROM   dis_cza_issste
                      WHERE  ident_arch = 3)
   AND    b.periodo_pago = $gd_periodo_pago_issste
   ORDER BY 1 DESC, 2 DESC, 3 DESC
   END sql

   INITIALIZE lr_aporte_issste.* TO NULL
   FOREACH cur_aporte_issste_rcv INTO lr_aporte_issste.perido_pago          ,
   	                                  lr_aporte_issste.fech_pago            ,
   	                                  lr_aporte_issste.consec_reg_lote      ,
                                      lr_aporte_issste.dias_cotizados       ,
                                      lr_aporte_issste.sbc                  ,
                                      lr_aporte_issste.impt_sar_isss        ,
                                      lr_aporte_issste.impt_ret_isss        ,
                                      lr_aporte_issste.impt_cv_isss_pat     ,
                                      lr_aporte_issste.impt_cv_isss_tra     ,
                                      lr_aporte_issste.inte_ext_sar_isss    ,
                                      lr_aporte_issste.inte_ext_ret_isss    ,
                                      lr_aporte_issste.inte_ext_cv_isss_pat ,
                                      lr_aporte_issste.inte_ext_cv_isss_tra ,
                                      lr_aporte_issste.impt_cs_isss
      LET gi_ap_estado_issste     = lr_aporte_issste.impt_cs_isss
      EXIT FOREACH
   END FOREACH
END FUNCTION #aporte_obl
#####################################################################
FUNCTION obtiene_retiros()

   DEFINE r_subcuenta       SMALLINT,
          r_tipo_movimiento SMALLINT,
          r_id_aportante    CHAR(15),
          r_monto_pes       DECIMAL(16,2)

   LET r_monto_pes = 0

   DECLARE cursor_retiros CURSOR FOR
   SELECT a.subcuenta      ,
          a.tipo_movimiento,
          a.id_aportante   ,
          ROUND (SUM(a.monto_en_pesos),2)
   FROM   tmp_cuenta a
   WHERE  a.nss = g_reg.nss
   AND    a.subcuenta > 0
   AND    a.tipo_movimiento IN ( SELECT b.codigo
                                 FROM   tab_movimiento b
                                 WHERE (b.codigo > 199
                                 OR     b.codigo = 7
                                 OR     b.codigo = 10
                                 OR     b.codigo = 34
                                 OR     b.codigo = 87
                                 OR     b.codigo = 89
                                 OR     b.codigo = 27)
                                 AND    b.codigo NOT IN (921,923)
                                 AND    b.tipo = -1
                               )
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1,2,3

   FOREACH cursor_retiros INTO r_subcuenta      ,
   	                           r_tipo_movimiento,
                               r_id_aportante   ,
                               r_monto_pes

      --IF r_id_aportante = "AJUSTE" OR
      --   r_id_aportante = "SOLASIG" OR
      --   r_id_aportante = "INDEBIDO" OR
      --   r_id_aportante = "CORTE EDAD" OR
      --   r_id_aportante = "CARGO-PEFOV" OR --tipo_movimiento 960
      --   r_id_aportante[1,3] = "TDC"  THEN
      --   CONTINUE FOREACH
      --END IF

      #Cuatrimestre2 2011
      IF r_tipo_movimiento = 89 THEN
      	 LET gs_redencion_anticipada = 1
      END IF

      --IF r_id_aportante = "CORTE EDAD" OR
      --   r_id_aportante = "TDC-2-SALDO" THEN
      --   CONTINUE FOREACH
      --END IF

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
              r_subcuenta = 32 OR
              r_subcuenta = 39
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
              r_subcuenta = 21 OR
              r_subcuenta = 26 OR
              r_subcuenta = 27 OR
              r_subcuenta = 37
            LET largo_plazo_retiros = largo_plazo_retiros + r_monto_pes

         #Cuatrimestre1 2009
         WHEN r_subcuenta = 33
         	  LET solidario_retiros = solidario_retiros + r_monto_pes
         	  LET gi_solidario_patron_retiros = gi_solidario_patron_retiros + r_monto_pes
         WHEN r_subcuenta = 34
            LET solidario_retiros = solidario_retiros + r_monto_pes
            LET gi_solidario_trab_retiros = gi_solidario_trab_retiros + r_monto_pes

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

   #Cuatrimestre1 2009
   LET gi_vivienda_retiros = infonavit97_retiros     +
                             sar_infonavit92_retiros +
                             fovissste_retiros       +
                             sar_fovissste_retiros
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
   AND    a.tipo_movimiento IN (1  ,
                                2  ,
                                3  ,
                                4  ,
                                5  ,
                                6  ,
                                12 ,
                                14 ,
                                15 ,
                                17 ,
                                18 ,
                                28 ,
                                29 ,
                                33 ,
                                45 ,
                                55 ,
                                83 ,
                                86 ,
                                500,
                                560,
                                570,
                                580,
                                590,
                                595,
                                596,
                                597,
                                620,
                                640,
                                777,
                                778,
                                779,
                                926,
                                951,
                                961)
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1,2,3

   FOREACH cursor_netos INTO a_subcuenta,
                             a_tipo_movimiento,
                             a_id_aportante,
                             a_monto_pes

      --IF a_id_aportante = "SOLASIG" OR
      --   a_id_aportante = "INDEBIDO" THEN
      --   CONTINUE FOREACH
      --END IF

      #Cuatrimestre1 2009
      --IF (a_subcuenta = 4  OR
      --    a_subcuenta = 8  OR
      --    a_subcuenta = 14 OR
      --    a_subcuenta = 19 OR
      --    a_subcuenta = 35) THEN
      --
      --    #Cuatrimestre2 2009
      --    (#a_tipo_movimiento = 3 OR
      --     #a_tipo_movimiento = 620) THEN
      --   CONTINUE FOREACH
      --END IF

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
              a_subcuenta = 32 OR
              a_subcuenta = 39
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
              a_subcuenta = 21 OR
              a_subcuenta = 26 OR
              a_subcuenta = 27 OR
              a_subcuenta = 37
            LET largo_plazo_aporta = largo_plazo_aporta + a_monto_pes

         #Cuatrimestre1 2009
         WHEN a_subcuenta = 33
         	  LET solidario_aporta = solidario_aporta + a_monto_pes
         	  LET gi_solidario_patron_aporta = gi_solidario_patron_aporta + a_monto_pes
         WHEN a_subcuenta = 34
            LET solidario_aporta = solidario_aporta + a_monto_pes
            LET gi_solidario_trab_aporta = gi_solidario_trab_aporta + a_monto_pes

         {WHEN a_subcuenta = 36
         	  LET bono_pension_aporta = bono_pension_aporta + a_monto_pes}
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

   #Cuatrimestre1 2009
   LET gi_vivienda_aporta = infonavit97_aporta     +
                            sar_infonavit92_aporta +
                            fovissste_aporta       +
                            sar_fovissste_aporta
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

#Cuatrimestre1 2009
LET gi_solidario_patron_rend   = (((gi_solidario_patron_fin - gi_solidario_patron_ini)-
                               gi_solidario_patron_aporta)-
                              (gi_solidario_patron_retiros))

LET gi_solidario_trab_rend     = (((gi_solidario_trab_fin - gi_solidario_trab_ini)-
                               gi_solidario_trab_aporta)-
                              (gi_solidario_trab_retiros))

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

   #Cuatrimestre1 2009
   LET gi_vivienda_rend = infonavit97_rend     +
                          sar_infonavit92_rend +
                          fovissste_rend       +
                          sar_fovissste_rend
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
                        g_reg.fecha_ini           USING "DDMMYYYY","|",
                        g_reg.fecha_top           USING "DDMMYYYY","|",
                        w_aux.nombres,"|",
                        w_aux.callep ,"|",
                        w_aux.coloniap,"|",
                        w_aux.codposp,"|",
                        vcentro_reparto,"|",
                        w_aux.estadop,"|",
                        w_aux.delegap,"|",
                        w_aux.fentcons            USING "DDMMYYYY","|",
                        "0","|",
                        null_nss,"|",
                        w_aux.rfc,"|",
                        w_aux.n_unico,"|",

                        retiro97_cv_cs_ini        USING "&&&&&&&&.&&","|",
                        retiro97_ini              USING "&&&&&&&&.&&","|",
                        cv_cs_ini                 USING "&&&&&&&&.&&","|",
                        sar_imss92_ini            USING "&&&&&&&&.&&","|",
                        infonavit97_ini           USING "&&&&&&&&.&&","|",
                        sar_infonavit92_ini       USING "&&&&&&&&.&&","|",
                        sub_imss_ini              USING "&&&&&&&&.&&","|",

                        retiro97_cv_cs_aporta     USING "&&&&&&&&.&&","|",
                        retiro97_aporta           USING "&&&&&&&&.&&","|",
                        cv_cs_aporta              USING "&&&&&&&&.&&","|",
                        sar_imss92_aporta         USING "&&&&&&&&.&&","|",
                        infonavit97_aporta        USING "&&&&&&&&.&&","|",
                        sar_infonavit92_aporta    USING "&&&&&&&&.&&","|",
                        sub_imss_aporta           USING "&&&&&&&&.&&","|",

                        retiro97_cv_cs_retiros    USING "-&&&&&&&.&&","|",
                        retiro97_retiros          USING "-&&&&&&&.&&","|",
                        cv_cs_retiros             USING "-&&&&&&&.&&","|",
                        sar_imss92_retiros        USING "-&&&&&&&.&&","|",
                        infonavit97_retiros       USING "-&&&&&&&.&&","|",
                        sar_infonavit92_retiros   USING "-&&&&&&&.&&","|",
                        sub_imss_retiros          USING "-&&&&&&&.&&","|",

                        retiro97_cv_cs_rend       USING "-&&&&&&&.&&","|",
                        retiro97_rend             USING "-&&&&&&&.&&","|",
                        cv_cs_rend                USING "-&&&&&&&.&&","|",
                        sar_imss92_rend           USING "-&&&&&&&.&&","|",
                        infonavit97_rend          USING "-&&&&&&&.&&","|",
                        sar_infonavit92_rend      USING "-&&&&&&&.&&","|",
                        sub_imss_rend             USING "-&&&&&&&.&&","|",

                        retiro97_cv_cs_comis      USING "-&&&&&&&.&&","|",
                        retiro97_comis            USING "-&&&&&&&.&&","|",
                        cv_cs_comis               USING "-&&&&&&&.&&","|",
                        sar_imss92_comis          USING "-&&&&&&&.&&","|",
                        infonavit97_comis         USING "-&&&&&&&.&&","|",
                        sar_infonavit92_comis     USING "-&&&&&&&.&&","|",
                        sub_imss_comis            USING "-&&&&&&&.&&","|",

                        retiro97_cv_cs_fin        USING "&&&&&&&&.&&","|",
                        retiro97_fin              USING "&&&&&&&&.&&","|",
                        cv_cs_fin                 USING "&&&&&&&&.&&","|",
                        sar_imss92_fin            USING "&&&&&&&&.&&","|",
                        infonavit97_fin           USING "&&&&&&&&.&&","|",
                        sar_infonavit92_fin       USING "&&&&&&&&.&&","|",
                        sub_imss_fin              USING "&&&&&&&&.&&","|",

                        retiro_issste_ini         USING "&&&&&&&&.&&","|",
                        sar_issste_ini            USING "&&&&&&&&.&&","|",
                        fovissste_ini             USING "&&&&&&&&.&&","|",
                        sar_fovissste_ini         USING "&&&&&&&&.&&","|",
                        bono_pension_ini          USING "&&&&&&&&.&&","|",
                        sub_issste_ini            USING "&&&&&&&&.&&","|",

                        retiro_issste_aporta      USING "&&&&&&&&.&&","|",
                        sar_issste_aporta         USING "&&&&&&&&.&&","|",
                        fovissste_aporta          USING "&&&&&&&&.&&","|",
                        sar_fovissste_aporta      USING "&&&&&&&&.&&","|",
                        bono_pension_aporta       USING "&&&&&&&&.&&","|",
                        sub_issste_aporta         USING "&&&&&&&&.&&","|",

                        retiro_issste_retiros     USING "-&&&&&&&.&&","|",
                        sar_issste_retiros        USING "-&&&&&&&.&&","|",
                        fovissste_retiros         USING "-&&&&&&&.&&","|",
                        sar_fovissste_retiros     USING "-&&&&&&&.&&","|",
                        bono_pension_retiros      USING "-&&&&&&&.&&","|",
                        sub_issste_retiros        USING "-&&&&&&&.&&","|",

                        retiro_issste_rend        USING "-&&&&&&&.&&","|",
                        sar_issste_rend           USING "-&&&&&&&.&&","|",
                        fovissste_rend            USING "-&&&&&&&.&&","|",
                        sar_fovissste_rend        USING "-&&&&&&&.&&","|",
                        bono_pension_rend         USING "-&&&&&&&.&&","|",
                        sub_issste_rend           USING "-&&&&&&&.&&","|",

                        retiro_issste_comis       USING "-&&&&&&&.&&","|",
                        sar_issste_comis          USING "-&&&&&&&.&&","|",
                        fovissste_comis           USING "-&&&&&&&.&&","|",
                        sar_fovissste_comis       USING "-&&&&&&&.&&","|",
                        bono_pension_comis        USING "-&&&&&&&.&&","|",
                        sub_issste_comis          USING "-&&&&&&&.&&","|",

                        retiro_issste_fin         USING "&&&&&&&&.&&","|",
                        sar_issste_fin            USING "&&&&&&&&.&&","|",
                        fovissste_fin             USING "&&&&&&&&.&&","|",
                        sar_fovissste_fin         USING "&&&&&&&&.&&","|",
                        bono_pension_fin          USING "&&&&&&&&.&&","|",
                        sub_issste_fin            USING "&&&&&&&&.&&","|",

                        vol_ini                   USING "&&&&&&&&.&&","|",
                        com_ini                   USING "&&&&&&&&.&&","|",
                        largo_plazo_ini           USING "&&&&&&&&.&&","|",
                        solidario_ini             USING "&&&&&&&&.&&","|",
                        sub_vol_com_ini           USING "&&&&&&&&.&&","|",

                        vol_aporta                USING "&&&&&&&&.&&","|",
                        com_aporta                USING "&&&&&&&&.&&","|",
                        largo_plazo_aporta        USING "&&&&&&&&.&&","|",
                        solidario_aporta          USING "&&&&&&&&.&&","|",
                        sub_vol_com_aporta        USING "&&&&&&&&.&&","|",

                        vol_retiros               USING "-&&&&&&&.&&","|",
                        com_retiros               USING "-&&&&&&&.&&","|",
                        largo_plazo_retiros       USING "-&&&&&&&.&&","|",
                        solidario_retiros         USING "-&&&&&&&.&&","|",
                        sub_vol_com_retiros       USING "-&&&&&&&.&&","|",

                        vol_rend                  USING "-&&&&&&&.&&","|",
                        com_rend                  USING "-&&&&&&&.&&","|",
                        largo_plazo_rend          USING "-&&&&&&&.&&","|",
                        solidario_rend            USING "-&&&&&&&.&&","|",
                        sub_vol_com_rend          USING "-&&&&&&&.&&","|",

                        vol_comis                 USING "-&&&&&&&.&&","|",
                        com_comis                 USING "-&&&&&&&.&&","|",
                        largo_plazo_comis         USING "-&&&&&&&.&&","|",
                        solidario_comis           USING "-&&&&&&&.&&","|",
                        sub_vol_com_comis         USING "-&&&&&&&.&&","|",

                        vol_fin                   USING "&&&&&&&&.&&","|",
                        com_fin                   USING "&&&&&&&&.&&","|",
                        largo_plazo_fin           USING "&&&&&&&&.&&","|",
                        solidario_fin             USING "&&&&&&&&.&&","|",
                        sub_vol_com_fin           USING "&&&&&&&&.&&","|",

                        total_general_ini         USING "&&&&&&&&.&&","|",
                        total_general_aporta      USING "&&&&&&&&.&&","|",
                        total_general_retiros     USING "-&&&&&&&.&&","|",
                        total_general_rend        USING "-&&&&&&&.&&","|",
                        total_general_comis       USING "-&&&&&&&.&&","|",
                        total_general_fin         USING "&&&&&&&&.&&","|",

                        bono_pension_actual       USING "&&&&&&&&.&&","|",
                        bono_pension_nominal      USING "&&&&&&&&.&&","|",

                        periodo_pago_1,"|",
                        reg_patron_1,"|",
                        sbc_1                     USING "&&&&&&&&.&&","|",
                        dias_cotizados_1          USING "&&&","|",
                        periodo_pago_2,"|",
                        reg_patron_2,"|",
                        sbc_2                     USING "&&&&&&&&.&&","|",
                        dias_cotizados_2          USING "&&&","|",
                        periodo_pago_3,"|",
                        reg_patron_3,"|",
                        sbc_3                     USING "&&&&&&&&.&&","|",
                        dias_cotizados_3          USING "&&&","|",

                        ahorro_retiro_ini         USING "&&&&&&&&.&&","|",
                        ahorro_retiro_aporta      USING "&&&&&&&&.&&","|",
                        ahorro_retiro_retiros     USING "-&&&&&&&.&&","|",
                        ahorro_retiro_rend        USING "-&&&&&&&.&&","|",
                        ahorro_retiro_comis       USING "-&&&&&&&.&&","|",
                        ahorro_retiro_fin         USING "&&&&&&&&.&&","|",

                        ahorro_vol_ini            USING "&&&&&&&&.&&","|",
                        ahorro_vol_aporta         USING "&&&&&&&&.&&","|",
                        ahorro_vol_retiros        USING "-&&&&&&&.&&","|",
                        ahorro_vol_rend           USING "-&&&&&&&.&&","|",
                        ahorro_vol_comis          USING "-&&&&&&&.&&","|",
                        ahorro_vol_fin            USING "&&&&&&&&.&&","|",

                        vivienda_ini              USING "&&&&&&&&.&&","|",
                        vivienda_movimientos      USING "-&&&&&&&.&&","|",
                        vivienda_fin              USING "&&&&&&&&.&&","|",

                        reg_gen.siefore           USING "&&","|",

                        x_nacionalidad_desc       ,"|",
                        w_aux.fonop               ,"|",
                        gi_ind_redencion          USING "&&","|",
                        gi_bono_pension_nominal_pesos USING "&&&&&&&&.&&","|",
                        gi_bono_pension_actual        USING "&&&&&&&&.&&","|",
                        gi_bono_pension_actual_pesos  USING "&&&&&&&&.&&","|",

                        #Cuatrimestre1 2009
                        gd_periodo_pago_issste        ,"|",
                        gi_dias_cotizados_issste      USING "&&&","|",
                        gi_sbc_issste                 USING "&&&&&&&&.&&","|",
                        gi_vivienda_aporta            USING "&&&&&&&&.&&","|",
                        gi_vivienda_retiros           USING "&&&&&&&&.&&","|",
                        gi_vivienda_rend              USING "&&&&&&&&.&&","|",
                        gi_vivienda_comis             USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_ini         USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_ini       USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_aporta      USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_aporta    USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_retiros     USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_retiros   USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_rend        USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_rend      USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_comis       USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_comis     USING "&&&&&&&&.&&","|",
                        gi_solidario_trab_fin         USING "&&&&&&&&.&&","|",
                        gi_solidario_patron_fin       USING "&&&&&&&&.&&","|",
                        gi_bono_udis_recibido         USING "&&&&&&&&.&&","|",
                        gi_bono_pesos_recibido        USING "&&&&&&&&.&&","|",
                        gi_comision_afore             USING "&&&&&&&&.&&","|",

                        #Cuatrimestre3 2009
                        gc_folio                                         ,"|",
                        #Cuatrimestre1 2011
                        gs_ind_afil                   USING "&"          ,"|",
                        #Cuatrimestre3 2011
                        gi_ap_obrero_pat_imss         USING "&&&&&&&&.&&","|",
                        gi_ap_estado_imss             USING "&&&&&&&&.&&","|",
                        gi_ap_obrero_pat_issste       USING "&&&&&&&&.&&","|",
                        gi_ap_estado_issste           USING "&&&&&&&&.&&"

{
display "________________________________________________________________"
display "g_reg.nss           :",g_reg.nss, " ,:",null_nss
display "#3   g_reg.fecha_ini              : ", g_reg.fecha_ini              USING "DDMMYYYY"
display "#4   g_reg.fecha_top              : ", g_reg.fecha_top              USING "DDMMYYYY"
display "#5   w_aux.nombres                : ", w_aux.nombres
display "#6   w_aux.callep                 : ", w_aux.callep
display "#7   w_aux.coloniap               : ", w_aux.coloniap
display "#8   w_aux.codposp                : ", w_aux.codposp
display "#9   vcentro_reparto              : ", vcentro_reparto
display "#10  w_aux.estadop                : ", w_aux.estadop
display "#11  w_aux.delegap                : ", w_aux.delegap
display "#12  w_aux.fentcons               : ", w_aux.fentcons               USING "DDMMYYYY"
display "#13  0                            : "
display "#14  null_nss                     : ", null_nss
display "#15  w_aux.rfc                    : ", w_aux.rfc
display "#16  w_aux.n_unico                : ", w_aux.n_unico
display "#                              : "
display "#17  retiro97_cv_cs_ini           : ", retiro97_cv_cs_ini
display "#18  retiro97_ini                 : ", retiro97_ini
display "#19  cv_cs_ini                    : ", cv_cs_ini
display "#20  sar_imss92_ini               : ", sar_imss92_ini
display "#21  infonavit97_ini              : ", infonavit97_ini
display "#22  sar_infonavit92_ini          : ", sar_infonavit92_ini
display "#23  sub_imss_ini                 : ", sub_imss_ini
display "#                              : "
display "#24  retiro97_cv_cs_aporta        : ", retiro97_cv_cs_aporta
display "#25  retiro97_aporta              : ", retiro97_aporta
display "#26  cv_cs_aporta                 : ", cv_cs_aporta
display "#27  sar_imss92_aporta            : ", sar_imss92_aporta
display "#28  infonavit97_aporta           : ", infonavit97_aporta
display "#29  sar_infonavit92_aporta       : ", sar_infonavit92_aporta
display "#30  sub_imss_aporta              : ", sub_imss_aporta
display "#                              : "
display "#31  retiro97_cv_cs_retiros       : ", retiro97_cv_cs_retiros
display "#32  retiro97_retiros             : ", retiro97_retiros
display "#33  cv_cs_retiros                : ", cv_cs_retiros
display "#34  sar_imss92_retiros           : ", sar_imss92_retiros
display "#35  infonavit97_retiros          : ", infonavit97_retiros
display "#36  sar_infonavit92_retiros      : ", sar_infonavit92_retiros
display "#37  sub_imss_retiros             : ", sub_imss_retiros
display "#                              : "
display "#38  retiro97_cv_cs_rend          : ", retiro97_cv_cs_rend
display "#39  retiro97_rend                : ", retiro97_rend
display "#40  cv_cs_rend                   : ", cv_cs_rend
display "#41  sar_imss92_rend              : ", sar_imss92_rend
display "#42  infonavit97_rend             : ", infonavit97_rend
display "#43  sar_infonavit92_rend         : ", sar_infonavit92_rend
display "#44  sub_imss_rend                : ", sub_imss_rend
display "#                              : "
display "#45  retiro97_cv_cs_comis         : ", retiro97_cv_cs_comis
display "#46  retiro97_comis               : ", retiro97_comis
display "#47  cv_cs_comis                  : ", cv_cs_comis
display "#48  sar_imss92_comis             : ", sar_imss92_comis
display "#49  infonavit97_comis            : ", infonavit97_comis
display "#50  sar_infonavit92_comis        : ", sar_infonavit92_comis
display "#51  sub_imss_comis               : ", sub_imss_comis
display "#                              : "
display "#52  retiro97_cv_cs_fin           : ", retiro97_cv_cs_fin
display "#53  retiro97_fin                 : ", retiro97_fin
display "#54  cv_cs_fin                    : ", cv_cs_fin
display "#55  sar_imss92_fin               : ", sar_imss92_fin
display "#56  infonavit97_fin              : ", infonavit97_fin
display "#57  sar_infonavit92_fin          : ", sar_infonavit92_fin
display "#58  sub_imss_fin                 : ", sub_imss_fin
display "#                              : "
display "#59  retiro_issste_ini            : ", retiro_issste_ini
display "#60  sar_issste_ini               : ", sar_issste_ini
display "#61  fovissste_ini                : ", fovissste_ini
display "#62  sar_fovissste_ini            : ", sar_fovissste_ini
display "#63  bono_pension_ini             : ", bono_pension_ini
display "#64  sub_issste_ini               : ", sub_issste_ini
display "#                              : "
display "#65  retiro_issste_aporta         : ", retiro_issste_aporta
display "#66  sar_issste_aporta            : ", sar_issste_aporta
display "#67  fovissste_aporta             : ", fovissste_aporta
display "#68  sar_fovissste_aporta         : ", sar_fovissste_aporta
display "#69  bono_pension_aporta          : ", bono_pension_aporta
display "#70  sub_issste_aporta            : ", sub_issste_aporta
display "#                              : "
display "#71  retiro_issste_retiros        : ", retiro_issste_retiros
display "#72  sar_issste_retiros           : ", sar_issste_retiros
display "#73  fovissste_retiros            : ", fovissste_retiros
display "#74  sar_fovissste_retiros        : ", sar_fovissste_retiros
display "#75  bono_pension_retiros         : ", bono_pension_retiros
display "#76  sub_issste_retiros           : ", sub_issste_retiros
display "#                              : "
display "#77  retiro_issste_rend           : ", retiro_issste_rend
display "#78  sar_issste_rend              : ", sar_issste_rend
display "#79  fovissste_rend               : ", fovissste_rend
display "#80  sar_fovissste_rend           : ", sar_fovissste_rend
display "#81  bono_pension_rend            : ", bono_pension_rend
display "#82  sub_issste_rend              : ", sub_issste_rend
display "#                              : "
display "#83  retiro_issste_comis          : ", retiro_issste_comis
display "#84  sar_issste_comis             : ", sar_issste_comis
display "#85  fovissste_comis              : ", fovissste_comis
display "#86  sar_fovissste_comis          : ", sar_fovissste_comis
display "#87  bono_pension_comis           : ", bono_pension_comis
display "#88  sub_issste_comis             : ", sub_issste_comis
display "#                              : "
display "#89  retiro_issste_fin            : ", retiro_issste_fin
display "#90  sar_issste_fin               : ", sar_issste_fin
display "#91  fovissste_fin                : ", fovissste_fin
display "#92  sar_fovissste_fin            : ", sar_fovissste_fin
display "#93  bono_pension_fin             : ", bono_pension_fin
display "#94  sub_issste_fin               : ", sub_issste_fin
display "#                              : "
display "#95  vol_ini                      : ", vol_ini
display "#96  com_ini                      : ", com_ini
display "#97  largo_plazo_ini              : ", largo_plazo_ini
display "#98  solidario_ini                : ", solidario_ini
display "#99  sub_vol_com_ini              : ", sub_vol_com_ini
display "#                              : "
display "#100 vol_aporta                   : ", vol_aporta
display "#101 com_aporta                   : ", com_aporta
display "#102 largo_plazo_aporta           : ", largo_plazo_aporta
display "#103 solidario_aporta             : ", solidario_aporta
display "#104 sub_vol_com_aporta           : ", sub_vol_com_aporta
display "#                              : "
display "#105 vol_retiros                  : ", vol_retiros
display "#106 com_retiros                  : ", com_retiros
display "#107 largo_plazo_retiros          : ", largo_plazo_retiros
display "#108 solidario_retiros            : ", solidario_retiros
display "#109 sub_vol_com_retiros          : ", sub_vol_com_retiros
display "#                              : "
display "#110 vol_rend                     : ", vol_rend
display "#111 com_rend                     : ", com_rend
display "#112 largo_plazo_rend             : ", largo_plazo_rend
display "#113 solidario_rend               : ", solidario_rend
display "#114 sub_vol_com_rend             : ", sub_vol_com_rend
display "#                              : "
display "#115 vol_comis                    : ", vol_comis
display "#116 com_comis                    : ", com_comis
display "#117 largo_plazo_comis            : ", largo_plazo_comis
display "#118 solidario_comis              : ", solidario_comis
display "#119 sub_vol_com_comis            : ", sub_vol_com_comis
display "#                              : "
display "#120 vol_fin                      : ", vol_fin
display "#121 com_fin                      : ", com_fin
display "#122 largo_plazo_fin              : ", largo_plazo_fin
display "#123 solidario_fin                : ", solidario_fin
display "#124 sub_vol_com_fin              : ", sub_vol_com_fin
display "#                              : "
display "#125 total_general_ini            : ", total_general_ini
display "#126 total_general_aporta         : ", total_general_aporta
display "#127 total_general_retiros        : ", total_general_retiros
display "#128 total_general_rend           : ", total_general_rend
display "#129 total_general_comis          : ", total_general_comis
display "#130 total_general_fin            : ", total_general_fin
display "#                              : "
display "#131 bono_pension_actual          : ", bono_pension_actual
display "#132 bono_pension_nominal         : ", bono_pension_nominal
display "#                              : "
display "#133 periodo_pago_1               : ", periodo_pago_1
display "#134 reg_patron_1                 : ", reg_patron_1
display "#135 sbc_1                        : ", sbc_1
display "#136 dias_cotizados_1             : ", dias_cotizados_1
display "#137 periodo_pago_2               : ", periodo_pago_2
display "#138 reg_patron_2                 : ", reg_patron_2
display "#139 sbc_2                        : ", sbc_2
display "#140 dias_cotizados_2             : ", dias_cotizados_2
display "#141 periodo_pago_3               : ", periodo_pago_3
display "#142 reg_patron_3                 : ", reg_patron_3
display "#143 sbc_3                        : ", sbc_3
display "#144 dias_cotizados_3             : ", dias_cotizados_3
display "#                              : "
display "#145 ahorro_retiro_ini            : ", ahorro_retiro_ini
display "#146 ahorro_retiro_aporta         : ", ahorro_retiro_aporta
display "#147 ahorro_retiro_retiros        : ", ahorro_retiro_retiros
display "#148 ahorro_retiro_rend           : ", ahorro_retiro_rend
display "#149 ahorro_retiro_comis          : ", ahorro_retiro_comis
display "#150 ahorro_retiro_fin            : ", ahorro_retiro_fin
display "#                              : "
display "#151 ahorro_vol_ini               : ", ahorro_vol_ini
display "#152 ahorro_vol_aporta            : ", ahorro_vol_aporta
display "#153 ahorro_vol_retiros           : ", ahorro_vol_retiros
display "#154 ahorro_vol_rend              : ", ahorro_vol_rend
display "#155 ahorro_vol_comis             : ", ahorro_vol_comis
display "#156 ahorro_vol_fin               : ", ahorro_vol_fin
display "#                              : "
display "#157 vivienda_ini                 : ", vivienda_ini
display "#158 vivienda_movimientos         : ", vivienda_movimientos
display "#159 vivienda_fin                 : ", vivienda_fin
display "#                              : "
display "#160 reg_gen.siefore              : ", reg_gen.siefore
display "#                              : "
display "#161 x_nacionalidad_desc          : ", x_nacionalidad_desc
display "#162 w_aux.fonop                  : ", w_aux.fonop
display "#163 gi_ind_redencion             : ", gi_ind_redencion
display "#164 gi_bono_pension_nominal_pesos: ", gi_bono_pension_nominal_pesos
display "#165 gi_bono_pension_actual       : ", gi_bono_pension_actual
display "#166 gi_bono_pension_actual_pesos : ", gi_bono_pension_actual_pesos
display "#                              : "
display "#167 gd_periodo_pago_issste       : ", gd_periodo_pago_issste
display "#168 gi_dias_cotizados_issste     : ", gi_dias_cotizados_issste
display "#169 gi_sbc_issste                : ", gi_sbc_issste
display "#170 gi_vivienda_aporta           : ", gi_vivienda_aporta
display "#171 gi_vivienda_retiros          : ", gi_vivienda_retiros
display "#172 gi_vivienda_rend             : ", gi_vivienda_rend
display "#173 gi_vivienda_comis            : ", gi_vivienda_comis
display "#174 gi_solidario_trab_ini        : ", gi_solidario_trab_ini
display "#175 gi_solidario_patron_ini      : ", gi_solidario_patron_ini
display "#176 gi_solidario_trab_aporta     : ", gi_solidario_trab_aporta
display "#177 gi_solidario_patron_aporta   : ", gi_solidario_patron_aporta
display "#178 gi_solidario_trab_retiros    : ", gi_solidario_trab_retiros
display "#179 gi_solidario_patron_retiros  : ", gi_solidario_patron_retiros
display "#180 gi_solidario_trab_rend       : ", gi_solidario_trab_rend
display "#181 gi_solidario_patron_rend     : ", gi_solidario_patron_rend
display "#182 gi_solidario_trab_comis      : ", gi_solidario_trab_comis
display "#183 gi_solidario_patron_comis    : ", gi_solidario_patron_comis
display "#184 gi_solidario_trab_fin        : ", gi_solidario_trab_fin
display "#185 gi_solidario_patron_fin      : ", gi_solidario_patron_fin
display "#186 gi_bono_udis_recibido        : ", gi_bono_udis_recibido
display "#187 gi_bono_pesos_recibido       : ", gi_bono_pesos_recibido
display "#188 gi_comision_afore            : ", gi_comision_afore
display "#189 gc_folio                     : ", gc_folio
display "#190 gs_ind_afil                  : ", gs_ind_afil
display "#191 gi_ap_obrero_pat_imss        : ", gi_ap_obrero_pat_imss         USING "&&&&&&&&.&&"
display "#192 gi_ap_estado_imss            : ", gi_ap_estado_imss             USING "&&&&&&&&.&&"
display "#193 gi_ap_obrero_pat_issste      : ", gi_ap_obrero_pat_issste       USING "&&&&&&&&.&&"
display "#194 gi_ap_estado_issste          : ", gi_ap_estado_issste           USING "&&&&&&&&.&&"
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

      sql
      CREATE INDEX tmp_fiscal_1 on tmp_fiscal (subcuenta,
                                               tipo_movimiento) IN tmp_dbs2
      END sql

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

   #Cuatrimestre1 2009
   DEFINE lar_sum96 RECORD
   	   afore_cod         SMALLINT,
       afore_desc        CHAR(30),
       tasa              DECIMAL(6,2),
       comision          DECIMAL(6,2)
   END RECORD
----------------------------------------
{
   SELECT "X"
   FROM   tab_rendimiento_neto
   WHERE  fecha_fin = g_reg.fecha_top
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      SELECT MAX(fecha_fin)
      INTO   xc_fecha
      FROM   tab_rendimiento_neto
      WHERE  siefore_cod = 1
   ELSE
      LET xc_fecha = g_reg.fecha_top
   END IF
}
   LET xc_fecha = g_reg.fecha_top

   DECLARE comsie1 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.rendimiento,
          a.comision,
          a.rendimiento_neto,
          a.fecha_cifras_al
   FROM   tab_rendimiento_neto a ,OUTER tab_afore b
   WHERE  a.fecha_ini <= xc_fecha
   AND    a.fecha_fin >= xc_fecha
   AND    a.siefore_cod = 1
   AND    b.afore_cod = a.afore_cod
   ORDER BY orden_consar

   LET cont_com = 1

   FOREACH comsie1 INTO com_2[cont_com].*
      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_2[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,231,91)
      ELSE

         LET genera_sum97 = com_2[cont_com].afore_desc,"|",
                            com_2[cont_com].rendimiento      USING "##&&.&&","|",
                            com_2[cont_com].comision         USING "##&&.&&","|",
                            com_2[cont_com].rendimiento_neto USING "##&&.&&","|",
                            com_2[cont_com].fecha_fin        USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,231,91)
       END IF
   END FOR
----------------------------------------
   DECLARE comsie2 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.rendimiento,
          a.comision,
          a.rendimiento_neto,
          a.fecha_cifras_al
   FROM   tab_rendimiento_neto a ,OUTER tab_afore b
   WHERE  a.fecha_ini <= xc_fecha
   AND    a.fecha_fin >= xc_fecha
   AND    a.siefore_cod = 2
   AND    b.afore_cod = a.afore_cod
   ORDER BY orden_consar

   LET cont_com = 1

   FOREACH comsie2 INTO com_2[cont_com].*
      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_2[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,231,92)
      ELSE

         LET genera_sum97 = com_2[cont_com].afore_desc,"|",
                            com_2[cont_com].rendimiento      USING "##&&.&&","|",
                            com_2[cont_com].comision         USING "##&&.&&","|",
                            com_2[cont_com].rendimiento_neto USING "##&&.&&","|",
                            com_2[cont_com].fecha_fin        USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,231,92)
       END IF
   END FOR
----------------------------------------
   DECLARE comsie3 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.rendimiento,
          a.comision,
          a.rendimiento_neto,
          a.fecha_cifras_al
   FROM   tab_rendimiento_neto a ,OUTER tab_afore b
   WHERE  a.fecha_ini <= xc_fecha
   AND    a.fecha_fin >= xc_fecha
   AND    a.siefore_cod = 3
   AND    b.afore_cod = a.afore_cod
   ORDER BY orden_consar

   LET cont_com = 1

   FOREACH comsie3 INTO com_2[cont_com].*
      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_2[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,231,93)
      ELSE

         LET genera_sum97 = com_2[cont_com].afore_desc,"|",
                            com_2[cont_com].rendimiento      USING "##&&.&&","|",
                            com_2[cont_com].comision         USING "##&&.&&","|",
                            com_2[cont_com].rendimiento_neto USING "##&&.&&","|",
                            com_2[cont_com].fecha_fin        USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,231,93)
       END IF
   END FOR
----------------------------------------
   DECLARE comsie4 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.rendimiento,
          a.comision,
          a.rendimiento_neto,
          a.fecha_cifras_al
   FROM   tab_rendimiento_neto a ,OUTER tab_afore b
   WHERE  a.fecha_ini <= xc_fecha
   AND    a.fecha_fin >= xc_fecha
   AND    a.siefore_cod = 4
   AND    b.afore_cod = a.afore_cod
   ORDER BY orden_consar

   LET cont_com = 1

   FOREACH comsie4 INTO com_2[cont_com].*
      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_2[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,231,94)
      ELSE

         LET genera_sum97 = com_2[cont_com].afore_desc,"|",
                            com_2[cont_com].rendimiento      USING "##&&.&&","|",
                            com_2[cont_com].comision         USING "##&&.&&","|",
                            com_2[cont_com].rendimiento_neto USING "##&&.&&","|",
                            com_2[cont_com].fecha_fin        USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,231,94)
       END IF
   END FOR
----------------------------------------
   DECLARE comsie5 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.rendimiento,
          a.comision,
          a.rendimiento_neto,
          a.fecha_cifras_al
   FROM   tab_rendimiento_neto a ,OUTER tab_afore b
   WHERE  a.fecha_ini <= xc_fecha
   AND    a.fecha_fin >= xc_fecha
   AND    a.siefore_cod = 5
   AND    b.afore_cod = a.afore_cod
   ORDER BY orden_consar

   LET cont_com = 1

   FOREACH comsie5 INTO com_2[cont_com].*
      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_2[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,231,95)
      ELSE

         LET genera_sum97 = com_2[cont_com].afore_desc,"|",
                            com_2[cont_com].rendimiento      USING "##&&.&&","|",
                            com_2[cont_com].comision         USING "##&&.&&","|",
                            com_2[cont_com].rendimiento_neto USING "##&&.&&","|",
                            com_2[cont_com].fecha_fin        USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,231,95)
       END IF
   END FOR

   #Cuatrimestre1 2009
   LET lar_sum96.afore_desc = "BANXICO"
   LET lar_sum96.tasa       = 0
   LET lar_sum96.comision   = 0

   LET genera_sum97 = lar_sum96.afore_desc,"|",
                      lar_sum96.tasa      USING "-&&&.&&","|",
                      lar_sum96.comision  USING "&&&&.&&","|"

   OUTPUT TO REPORT r_report(genera_sum97,231,96)

   LET genera_sum99 = HOY USING "DDMMYYYY","|",
                      --"1","|",
                      x_folio USING "&&&&&&&&","|",
                      pos USING "&&&&&&&&","|"

   OUTPUT TO REPORT r_report(genera_sum99,231,99)

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

   DEFINE ld_fecha                       DATE         ,
          ld_fecha_redencion             DATE         ,
          li_bono_pension_nominal        DECIMAL(22,6),
          li_bono_pension_actual         DECIMAL(22,6),
          li_bono_pension_nominal_pesos  DECIMAL(22,6),
          li_bono_pension_actual_pesos   DECIMAL(22,6),
          li_bono_pesos_recibido         DECIMAL(22,6)

   DEFINE ls_status   SMALLINT

   #Verificar si es un bono de unificacion
   LET ls_status = 0
   IF gs_ind_bono = 42 OR
   	  gs_bono_uni = 1  THEN

   	  #Recopila los datos necesarios para el calculo temporalmente
   	  LET ls_status = graba_bono_tmp(g_reg.nss)
   END IF

   EXECUTE get_bono USING g_reg.nss,
                          g_reg.fecha_top
                    INTO  gd_fecha_redencion           ,  --fecha_reden
                          bono_pension_nominal         ,  --bono_nominal
                          bono_pension_actual          ,  --bono_real
                          gi_bono_pension_nominal_pesos,  --bono_nominal_pesos
                          gi_bono_pension_actual_pesos ,  --bono_real_pesos
                          gi_bono_pesos_recibido          --bono_recibido_pesos

   LET ld_fecha = g_reg.fecha_ini - 1 UNITS DAY
   EXECUTE get_bono USING g_reg.nss,
                          ld_fecha
                    INTO  ld_fecha_redencion           ,  --fecha_reden
                          li_bono_pension_nominal      ,  --bono_nominal
                          li_bono_pension_actual       ,  --bono_real
                          li_bono_pension_nominal_pesos,  --bono_nominal_pesos
                          li_bono_pension_actual_pesos ,  --bono_real_pesos
                          li_bono_pesos_recibido          --bono_recibido_pesos

   LET li_anio_edc   = YEAR(g_reg.fecha_top)
   LET li_anio_reden = YEAR(gd_fecha_redencion)
   LET gi_bono_pension_actual = bono_pension_actual

   #Cuatrimestre3 2010
   --En este periodo el bono se redime el 1 de enero
   --El bono se redime el 31 de diciembre del año anterior a la fecha de redencion

   IF li_anio_reden <> 2011 THEN
      IF MONTH(g_reg.fecha_top) = 12 THEN
         LET li_anio_reden = li_anio_reden - 1
      END IF
   END IF

   IF li_bono_pension_actual_pesos IS NULL THEN
      LET li_bono_pension_actual_pesos = 0
   END IF

   LET bono_pension_ini = li_bono_pension_actual_pesos

   #Cuatrimestre3 2009
   --El bono se redime el 1 de enero de 2010
   IF li_anio_edc >=  li_anio_reden OR
   	  gs_redencion_anticipada <> 0  THEN #Cuatrimestre2 2011
   	  LET gi_ind_redencion = 1

   	  LET gi_bono_pension_actual        = 0
   	  LET bono_pension_nominal          = 0
      LET bono_pension_actual           = 0
      LET gi_bono_pension_nominal_pesos = 0
      LET gi_bono_pension_actual_pesos  = 0
      LET gi_bono_pesos_recibido        = 0

      IF li_anio_edc <= li_anio_reden THEN
      	 --Si se redime en el periodo actual si se muestra el saldo inicial
      ELSE
      	 LET bono_pension_ini = 0
      END IF

   ELSE
   	  LET gi_bono_udis_recibido = bono_pension_nominal
   END IF

   LET bono_pension_fin = gi_bono_pension_actual_pesos

   #Cuatrimestre2 2009
   INITIALIZE gi_bono_udis_recibido  TO NULL
   INITIALIZE gi_bono_pesos_recibido TO NULL

   #Cuatrimestre3 2009
   CALL verifica_bono()

   IF ls_status > 0 THEN
      CALL elimina_bono_tmp(g_reg.nss,ls_status)
   END IF
END FUNCTION
################################################################################
#Cuatrimestre3 2009
FUNCTION verifica_bono()
   DEFINE ld_fecha_aporte  DATE
   DEFINE li_bono_original_pesos DECIMAL(22,6)

   DEFINE ld_fecha_redencion             DATE         ,
          li_bono_pension_nominal        DECIMAL(22,6),
          li_bono_pension_actual         DECIMAL(22,6),
          li_bono_pension_nominal_pesos  DECIMAL(22,6),
          li_bono_pension_actual_pesos   DECIMAL(22,6),
          li_bono_udis_recibido          DECIMAL(22,6),
          li_bono_pesos_recibido         DECIMAL(22,6)

   INITIALIZE ld_fecha_aporte TO NULL

   SELECT MAX(fecha_conversion)
   INTO   ld_fecha_aporte
   FROM   dis_cuenta
   WHERE  nss       = g_reg.nss
   AND    subcuenta = 36
   AND    tipo_movimiento <> 27 --REDENCION
   AND    fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   AND    monto_en_acciones > 0

   IF ld_fecha_aporte IS NOT NULL THEN
   	  IF ld_fecha_aporte < g_reg.fecha_ini THEN
         --Si el bono se recibio antes del periodo
      	 --se conserva el saldo incial calculado en la funcion bono()

      	 --No se debe calcular ningún aporte
      ELSE
      	 --Si el traspaso se hizo en el cuatrimestre el bono se muestra
         --como aporte usando fn_valua_bono con la fecha del traspaso
         LET bono_pension_ini = 0

         EXECUTE get_bono USING g_reg.nss,
                                ld_fecha_aporte
                          INTO  ld_fecha_redencion           ,  --fecha_reden       ,
                                li_bono_pension_nominal      ,  --bono_nominal      ,
                                li_bono_pension_actual       ,  --bono_real         ,
                                li_bono_pension_nominal_pesos,  --bono_nominal_pesos,
                                li_bono_pension_actual_pesos ,  --bono_real_pesos   ,
                                li_bono_udis_recibido        ,  --bono_recibido_udis,
                                li_bono_pesos_recibido          --bono_recibido_pesos;

         IF li_bono_pension_actual_pesos IS NULL THEN
         	 LET li_bono_pension_actual_pesos = 0
         END IF

         LET bono_pension_aporta = li_bono_pension_actual_pesos
      END IF
   END IF

   {SELECT "X"
   FROM   taa_viv_recepcion
   WHERE  nss          = g_reg.nss
   AND    importe_bono > 0
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN
      --Si el trabajador llegó por traspaso
      --Verificar la fecha del traspaso
      INITIALIZE ld_fecha_aporte TO NULL

      sql
      SELECT fecha_mov_banxico
      INTO   $ld_fecha_aporte
      FROM   taa_viv_recepcion
      WHERE  nss          = $g_reg.nss
      AND    importe_bono > 0
      GROUP BY 1
      END sql

      IF ld_fecha_aporte < g_reg.fecha_ini THEN
      --Si el traspaso se hizo antes del cuatrimestre el bono se muestra
      --como el saldo inicial calculado en la funcion bono()

      --No se debe calcular el aporte
      ELSE
      --Si el traspaso se hizo en el cuatrimestre el bono se muestra
      --como aporte usando fn_valua_bono con la fecha del traspaso
         LET bono_pension_ini = 0

         EXECUTE get_bono USING g_reg.nss,
                                ld_fecha_aporte
                          INTO  ld_fecha_redencion           ,  --fecha_reden       ,
                                li_bono_pension_nominal      ,  --bono_nominal      ,
                                li_bono_pension_actual       ,  --bono_real         ,
                                li_bono_pension_nominal_pesos,  --bono_nominal_pesos,
                                li_bono_pension_actual_pesos ,  --bono_real_pesos   ,
                                li_bono_udis_recibido        ,  --bono_recibido_udis,
                                li_bono_pesos_recibido          --bono_recibido_pesos;

         IF li_bono_pension_actual_pesos IS NULL THEN
         	 LET li_bono_pension_actual_pesos = 0
         END IF

         LET bono_pension_aporta = li_bono_pension_actual_pesos
      END IF
   END IF}
END FUNCTION
################################################################################
{FUNCTION verifica_bono()
   DEFINE ld_fecha_aporte  DATE
   DEFINE li_bono_original_pesos DECIMAL(22,6)

   DEFINE ld_fecha_redencion             DATE         ,
          li_bono_pension_nominal        DECIMAL(22,6),
          li_bono_pension_actual         DECIMAL(22,6),
          li_bono_pension_nominal_pesos  DECIMAL(22,6),
          li_bono_pension_actual_pesos   DECIMAL(22,6),
          li_bono_udis_recibido          DECIMAL(22,6),
          li_bono_pesos_recibido         DECIMAL(22,6)

   LET ld_fecha_aporte = NULL

   SELECT fecha_mov_banxico
   INTO   ld_fecha_aporte
   FROM   taa_viv_recepcion
   WHERE  nss          = g_reg.nss
   AND    fecha_mov_banxico BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   AND    importe_bono > 0

   IF ld_fecha_aporte IS NULL OR
      ld_fecha_aporte = "01/01/1899" THEN

      SELECT fecha_recepcion
      INTO   ld_fecha_aporte
      FROM   dis_det_bono
      WHERE  n_unico = w_aux.n_unico
      AND    ind_tipo_bono_isss = 1
      AND    fecha_recepcion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   END IF

   --LET bono_pension_ini = 0

   IF ld_fecha_aporte IS NOT NULL OR
      ld_fecha_aporte <> "01/01/1899" THEN
      EXECUTE get_bono USING g_reg.nss,
                             ld_fecha_aporte
                       INTO  ld_fecha_redencion           ,  --fecha_reden       ,
                             li_bono_pension_nominal      ,  --bono_nominal      ,
                             li_bono_pension_actual       ,  --bono_real         ,
                             li_bono_pension_nominal_pesos,  --bono_nominal_pesos,
                             li_bono_pension_actual_pesos ,  --bono_real_pesos   ,
                             li_bono_udis_recibido        ,  --bono_recibido_udis,
                             li_bono_pesos_recibido          --bono_recibido_pesos;
   END IF

   IF li_bono_pension_actual_pesos IS NULL THEN
   	 LET li_bono_pension_actual_pesos = 0
   END IF

   LET bono_pension_aporta = li_bono_pension_actual_pesos

END FUNCTION
################################################################################}
FUNCTION existe_bono_uni(lc_nss, ld_fecha_ini, ld_fecha_fin)
   DEFINE lc_nss        CHAR(11)
   DEFINE ld_fecha_ini,
          ld_fecha_fin  DATE
   DEFINE ls_existe     SMALLINT
   DEFINE ld_udis       DECIMAL(22,6)

   LET ls_existe = 0

   SELECT "X"
   FROM   uni_unificador
   WHERE  nss_uni = lc_nss
   AND    estado  = 100      --liquidado

   IF SQLCA.SQLCODE = 0 THEN
   	  #Saldo inicial
      LET ld_udis = ver_saldo_bono(lc_nss,ld_fecha_ini)
      IF ld_udis > 0 THEN
         LET ls_existe = 1
      ELSE

      	 #Saldo final
         LET ld_udis = ver_saldo_bono(lc_nss,ld_fecha_fin)
         IF ld_udis > 0 THEN
            LET ls_existe = 1
         END IF
      END IF
   END IF

   RETURN ls_existe
END FUNCTION
################################################################################
FUNCTION ver_saldo_bono(lc_nss,ld_fecha)
   DEFINE lc_nss        CHAR(11)
   DEFINE ld_fecha      DATE
   DEFINE ld_udis       DECIMAL(22,6)

   SELECT SUM(monto_en_acciones)
   INTO   ld_udis
   FROM   dis_cuenta
   WHERE  nss               = lc_nss
   AND    fecha_conversion <= ld_fecha
   AND    subcuenta         = 36

   RETURN ld_udis
END FUNCTION
################################################################################
FUNCTION graba_bono_tmp(lc_nss)
   DEFINE lc_nss      CHAR(11)
   DEFINE lc_n_unico  CHAR(18)
   DEFINE lc_nss_cta1 CHAR(11)
   DEFINE ls_status   SMALLINT

   LET ls_status = 0

   #El unificador debe tener los recursos del bono
   #Si el bono llego en la afore, es posible que la curp este registrada en
   #dis_det_bono
   SELECT "X"
   FROM   dis_det_bono
   WHERE  n_unico IN (SELECT n_unico
                      FROM   afi_mae_afiliado
                      WHERE  n_seguro = lc_nss)
   GROUP BY 1

   IF SQLCA.SQLCODE <> 0 THEN
   	  #Si no tiene bono en afore
   	  #Validar si tiene registro de bono traspaso
   	  SELECT "X"
    	FROM   taa_viv_recepcion
    	WHERE  nss          = lc_nss
    	AND    importe_bono > 0
    	GROUP BY 1

    	IF SQLCA.SQLCODE <> 0 THEN
         DECLARE cur_unificados CURSOR FOR
         SELECT nss_cta1
         FROM   uni_unificado
         WHERE  nss_uni = lc_nss
         AND    estado  = 100    --liquidado
         ORDER BY 1

         #Revisar todos los unificados
         FOREACH cur_unificados INTO lc_nss_cta1
         	  #Revisar si fue bono de afore
            SELECT "X"
            FROM   dis_det_bono
            WHERE  n_unico IN (SELECT n_unico
                               FROM   afi_mae_afiliado
                               WHERE  n_seguro = lc_nss_cta1)
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               WHENEVER ERROR CONTINUE
                  DROP TABLE tmp_bono_afore
               WHENEVER ERROR STOP

               SELECT *
               FROM   dis_det_bono
               WHERE  n_unico IN (SELECT n_unico
                                  FROM   afi_mae_afiliado
                                  WHERE  n_seguro = lc_nss_cta1)
               INTO TEMP tmp_bono_afore

               #Curp del unificador
               SELECT n_unico
               INTO   lc_n_unico
               FROM   afi_mae_afiliado
               WHERE  n_seguro = lc_nss

               UPDATE tmp_bono_afore
               SET    n_unico  = lc_n_unico,
                      n_seguro = lc_nss
               WHERE  1 = 1

               INSERT INTO dis_det_bono
               SELECT *
               FROM   tmp_bono_afore

               LET ls_status = 40
            ELSE
               #Revisar si fue bono de traspaso
               SELECT "X"
    	         FROM   taa_viv_recepcion
    	         WHERE  nss          = lc_nss_cta1
    	         AND    importe_bono > 0
    	         GROUP BY 1

    	         IF SQLCA.SQLCODE = 0 THEN
                  WHENEVER ERROR CONTINUE
                     DROP TABLE tmp_bono_traspaso
                  WHENEVER ERROR CONTINUE

                  SELECT *
    	            FROM   taa_viv_recepcion
    	            WHERE  nss          = lc_nss_cta1
    	            AND    importe_bono > 0
    	            INTO TEMP tmp_bono_traspaso

    	            UPDATE tmp_bono_traspaso
                  SET    nss = lc_nss
                  WHERE  1 = 1

                  INSERT INTO taa_viv_recepcion
                  SELECT *
                  FROM   tmp_bono_traspaso

                  LET ls_status = 41
    	         END IF
            END IF

            #Validar si ya se identificó al unificado con bono
            IF ls_status > 0 THEN
               EXIT FOREACH
            END IF
         END FOREACH
      END IF
   END IF

   RETURN ls_status
END FUNCTION
################################################################################
FUNCTION elimina_bono_tmp(lc_nss,ls_status)
   DEFINE lc_nss      CHAR(11)
   DEFINE lc_n_unico  CHAR(18)
   DEFINE ls_status   SMALLINT

   CASE ls_status
      WHEN 40
         SELECT n_unico
         INTO   lc_n_unico
         FROM   afi_mae_afiliado
         WHERE  n_seguro = lc_nss

         DELETE
         FROM  dis_det_bono
         WHERE n_unico  = lc_n_unico
         AND   n_seguro = lc_nss

      WHEN 41
         DELETE
    	   FROM   taa_viv_recepcion
    	   WHERE  nss          = lc_nss
    	   AND    importe_bono > 0
   END CASE
END FUNCTION
################################################################################