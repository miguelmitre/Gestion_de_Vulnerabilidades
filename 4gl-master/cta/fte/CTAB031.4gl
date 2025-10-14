##############################################################################
#Proyecto            => SAFRE  ( MEXICO )                                    #
#Owner               => E.F.P.                                               #
#Programa CTAB031    => ESTADO DE CUENTA INDEPENDIENTES                      #
#Sistema             => CTA                                                  #
#Por                 => OMAR SANDOVAL BADILLO                                #
#Fecha               => 17 de julio de 2006                                  #
#Por                 => MIGUEL ANGEL HERNANDEZ MARTINEZ                      #
#Fecha               => 18 de julio de 2006                                  #
#Fecha               => 3 de julio de 2007                                   #
#Fecha               => 11 de Septiembre de 2007                             #
#Fecha               => 08 de Enero de 2008                                  #
##############################################################################
DATABASE safre_af
GLOBALS

   DEFINE reg_gen  RECORD
          ind_siefore        CHAR(1)
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
          fonop             LIKE afi_telefono.telefono,
          fena              LIKE afi_mae_afiliado.fena,          --- carta
          nacionalidad      LIKE afi_mae_afiliado.nacionalidad   --- carta
   END RECORD

   DEFINE g_reg RECORD
          nss                    CHAR(11),
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

   DEFINE periodo_pago1      CHAR(6)
   DEFINE reg_patron1        CHAR(11)
   DEFINE sbc1               DECIMAL(16,6)
   DEFINE dias_cotizados1    SMALLINT
   DEFINE periodo_pago2      CHAR(6)
   DEFINE reg_patron2        CHAR(11)
   DEFINE sbc2               DECIMAL(16,6)
   DEFINE dias_cotizados2    SMALLINT
   DEFINE periodo_pago3      CHAR(6)
   DEFINE reg_patron3        CHAR(11)
   DEFINE sbc3               DECIMAL(16,6)
   DEFINE dias_cotizados3    SMALLINT

   DEFINE retiro97_ini,
          cv_ini,
          cs_ini,
          sar_imss_ini,
          sar_issste_ini,
          sub_ini_rcv,

          retiro97_netas,
          cv_netas,
          cs_netas,
          sar_netas_imss,
          sar_netas_issste,
          sub_netas_rcv,

          retiro97_retiros,
          cv_retiros,
          cs_retiros,
          sar_imss_retiros,
          sar_issste_retiros,
          sub_retiros_rcv,

          retiro97_fin,
          cv_fin,
          cs_fin,
          sar_imss_fin,
          sar_issste_fin,
          sub_fin_rcv,

          viv_97_imss_ini,
          viv_92_imss_ini,
          viv_92_issste_ini,
          sub_viv_ini,

          viv_97_imss_netas,
          viv_92_imss_netas,
          viv_92_issste_netas,
          sub_viv_netas,

          viv_97_imss_retiros,
          viv_92_imss_retiros,
          viv_92_issste_retiros,
          sub_viv_retiros,

          viv_97_imss_fin,
          viv_92_imss_fin,
          viv_92_issste_fin,
          sub_viv_fin,

          vol_ini,
          com_ini,
          alp_ini,         --- actualizacion para 01-07-2006
          sub_vcom_ini,

          vol_netas,
          com_netas,
          alp_netas,         --- actualizacion para 01-07-2006
          sub_vcom_netas,

          vol_retiros,
          com_retiros,
          alp_retiros,         --- actualizacion para 01-07-2006
          sub_vcom_retiros,

          vol_fin,
          com_fin,
          alp_fin,         --- actualizacion para 01-07-2006
          sub_vcom_fin,

          total_general_ini,
          total_general_netas,
          total_general_retiros,
          total_general_fin,

          total_semestre           DECIMAL(16,2)

   DEFINE rendimiento_siefore1,
          rendimiento_siefore2,
          rendimiento_adicional,
          rendimiento_infonavit,
          rendimiento_fovissste   DECIMAL(16,4)

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
   DEFINE comiadi            DECIMAL(16,2)
   DEFINE comiacr            DECIMAL(16,2)
   DEFINE comitod            DECIMAL(16,2)

   DEFINE comircv0           DECIMAL(16,2),
          comircv1           DECIMAL(16,2),
          comircv2           DECIMAL(16,2),
          id_comision        CHAR(1),
          vfecha_comision0   DATE,
          vfecha_comision1   DATE,
          con_comis          SMALLINT,
          comision_flujo0    DECIMAL(16,2)

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
   DEFINE fecha_alp_fiscal   DATE     --- actualizacion para 01-07-2006
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
   DEFINE monto_alp_fiscal   DECIMAL(16,2)    --- actualizacion para 01-07-2006
   DEFINE imp_alp_fiscal     DECIMAL(16,2)    --- actualizacion para 01-07-2006
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

   DEFINE v_saldo_dia   CHAR(100)
   DEFINE xfecha_inix   DATE

   DEFINE ind_edad      SMALLINT

   DEFINE genera_encabezado CHAR(300)
   DEFINE genera_detalle    CHAR(3000)
   DEFINE genera_sum97      CHAR(3000)
   DEFINE genera_sum98      CHAR(3000)
   DEFINE genera_sum99      CHAR(3000)

   DEFINE v_arch              CHAR(100),
          permisos            CHAR(50)

   DEFINE x_estado            SMALLINT

   DEFINE sub_sem_rcv,
          sub_viv_sem,
          sub_vcom_sem       DECIMAL(16,6)

   DEFINE x_correo_electronico  CHAR(200)

   DEFINE vfecha_comision    DATE

   DEFINE total_dias_annio   SMALLINT

   DEFINE rendimiento_ini       DECIMAL(16,6),
          rendimiento_fin       DECIMAL(16,6),
          rendimiento_calculado DECIMAL(16,6),
          total_rendimiento     DECIMAL(16,6),
          x_ren_precio_dia_ini  DECIMAL(16,6),
          x_ren_precio_dia_fin  DECIMAL(16,6)
END GLOBALS
#####################################################################
MAIN

   LET x_tipo_proceso = ARG_VAL(1)
   LET x_folio        = ARG_VAL(2)
   LET x_estado       = ARG_VAL(3)

   DISPLAY "INICIA PROCESO DE ESTADO DE CUENTA",
           " CON FOLIO:",x_folio ," DEL ESTADO:",x_estado

   CALL STARTLOG("CTAB009.log")

   CALL inicializa()
   CALL dias_del_annio()

   IF x_tipo_proceso = 0 THEN
      CALL prepara_nss_semestral()
   ELSE
      CALL prepara_nss()
   END IF

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA:",pos

END MAIN
#####################################################################
FUNCTION inicializa()

   DEFINE hoy1 DATE

   DEFINE sel_1,
          sel_2,
          sel_3,
          sel_4,
          sel_5,
          sel_6,
          sel_7,
          sel_8,
          sel_9     CHAR(1000)

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
                " AND   b.tipo_solicitud = ? ",
                " AND   b.tel_cod <> 7 "

   LET sel_4 =  " SELECT a.fecha_presentacion,",
                        "a.ident_lote_solici[3,5]",
                " FROM   taa_cd_det_cedido a",
                " WHERE  a.n_seguro = ? ",
                " AND    a.estado = 103 ",
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

------------- variables de resumen de saldos
   LET retiro97_ini          = 0    --- 1
   LET cv_ini                = 0    --- 2,6,9
   LET cs_ini                = 0    --- 5
   LET sar_imss_ini          = 0    --- 7
   LET sar_issste_ini        = 0    --- 13
   LET sub_ini_rcv           = 0

   LET retiro97_netas        = 0
   LET cv_netas              = 0
   LET cs_netas              = 0
   LET sar_netas_imss        = 0
   LET sar_netas_issste      = 0
   LET sub_netas_rcv         = 0

   LET retiro97_retiros      = 0
   LET cv_retiros            = 0
   LET cs_retiros            = 0
   LET sar_imss_retiros      = 0
   LET sar_issste_retiros    = 0
   LET sub_retiros_rcv       = 0

   LET retiro97_fin          = 0
   LET cv_fin                = 0
   LET cs_fin                = 0
   LET sar_imss_fin          = 0
   LET sar_issste_fin        = 0
   LET sub_fin_rcv           = 0

   LET viv_97_imss_ini       = 0   ---- 4
   LET viv_92_imss_ini       = 0   ---- 8
   LET viv_92_issste_ini     = 0   ---- 14
   LET sub_viv_ini           = 0

   LET viv_97_imss_netas     = 0
   LET viv_92_imss_netas     = 0
   LET viv_92_issste_netas   = 0
   LET sub_viv_netas         = 0

   LET viv_97_imss_retiros   = 0
   LET viv_92_imss_retiros   = 0
   LET viv_92_issste_retiros = 0
   LET sub_viv_retiros       = 0

   LET viv_97_imss_fin       = 0
   LET viv_92_imss_fin       = 0
   LET viv_92_issste_fin     = 0
   LET sub_viv_fin           = 0

   LET vol_ini               = 0    ---- 3,10
   LET com_ini               = 0    ---- 11,12
   LET alp_ini               = 0    ---- 16  --- actualizacion para 01-07-2006
   LET sub_vcom_ini          = 0

   LET vol_netas             = 0
   LET com_netas             = 0
   LET alp_netas             = 0   --- actualizacion para 01-07-2006
   LET sub_vcom_netas        = 0

   LET vol_retiros           = 0
   LET com_retiros           = 0
   LET alp_retiros           = 0   --- actualizacion para 01-07-2006
   LET sub_vcom_retiros      = 0

   LET vol_fin               = 0
   LET com_fin               = 0
   LET alp_fin               = 0   --- actualizacion para 01-07-2006
   LET sub_vcom_fin          = 0

   LET total_general_ini     = 0
   LET total_general_netas   = 0
   LET total_general_retiros = 0
   LET total_general_fin     = 0

   LET sub_netas_rcv         = 0
   LET sub_viv_netas         = 0
   LET sub_vcom_netas        = 0
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

   LET periodo_pago1   = ""
   LET reg_patron1     = ""
   LET sbc1            = 0
   LET dias_cotizados1 = 0
   LET periodo_pago2   = ""
   LET reg_patron2     = ""
   LET sbc2            = 0
   LET dias_cotizados2 = 0
   LET periodo_pago3   = ""
   LET reg_patron3     = ""
   LET sbc3            = 0
   LET dias_cotizados3 = 0

   LET aporte_vol = 0
   LET aporte_obl = 0

   LET total_rendimiento = 0

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

END FUNCTION
#####################################################################
FUNCTION estado_cuenta()
   CALL ini()
   CALL genera_patrones()

   IF x_tipo_informe = 0 THEN
      LET rendimiento_infonavit = 3.99 --12.00
      LET rendimiento_fovissste = 1.03 --2.29
      CALL datos_trabajador_sem()
      CALL resumen_saldos_sem()
   ELSE
      CALL rendimiento_viv()
      CALL rendimiento_fov()
      LET rendimiento_infonavit = 12.00
      LET rendimiento_fovissste = 2.29
      CALL datos_trabajador()
      CALL correo()
      CALL resumen_saldos()
   END IF

   CALL resumen_comisiones()
   CALL calculo_rendimiento()
   CALL obtiene_aportes()
   CALL obtiene_retiros()
   CALL aportes_netos()

   CALL informacion_fiscal()
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

   OUTPUT TO REPORT r_report(genera_encabezado,229,1,x_folio)

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

         OUTPUT TO REPORT r_report(genera_detalle,229,2,x_folio)
      END FOREACH

      INITIALIZE g_reg.* TO NULL
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

   LET g_reg.fecha_ini = "07/01/2007"
   LET g_reg.fecha_top = "12/31/2007"

   CALL Ingresa_etapa(x_folio,2,x_estado,"Inicia calculo de estado de cuenta")

   LET v_arch_sem = g_parametro.ruta_envio CLIPPED,"/",
                    "ECS",x_folio USING "<<<<<<<<<"

   START REPORT r_report TO v_arch_sem

   LET genera_encabezado = g_afore.codigo_afore,"|",
                           HOY USING "DDMMYYYY","|",
                           "1","|",
                           x_tipo_informe

   OUTPUT TO REPORT r_report(genera_encabezado,229,1,x_folio)

   CALL pide_parametros()

   DECLARE cur_nss_sem CURSOR FOR
   SELECT a.nss,
          a.status
   FROM   cta_nss_edo_cta a
   WHERE  a.estado = x_estado
   ORDER BY a.nss

   FOREACH cur_nss_sem INTO g_reg.nss,
                            g_reg.status

      LET pos = pos + 1

      CALL estado_cuenta()  #ec

      OUTPUT TO REPORT r_report(genera_detalle,229,2,x_folio)

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

{
display "RENDIMIENTO"
display vfecha_ini_sie
display vfecha_fin_sie
display dias_del_mes
display precioant
display precioact
display rendimiento_siefore1

display vfecha_ini_sie2
display vfecha_fin_sie2
display dias_del_mes
display precioant_sie2
display precioact_sie2
display rendimiento_siefore2
}

   LET rendimiento_siefore1 = 10.20 --10.72
   LET rendimiento_siefore2 = 11.81 --12.58

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_folio
   WHENEVER ERROR STOP

      CREATE TEMP TABLE tmp_folio
      (folio INTEGER )

      INSERT INTO tmp_folio
      SELECT a.folio
      FROM   dis_dep_aporte a
      WHERE  a.fech_liquidacion >= g_reg.fecha_ini
      AND    a.fech_liquidacion <= g_reg.fecha_top
      AND    a.estado = 3
      AND    a.ident_pago[14,15] IN (11,23)

      CREATE INDEX tmp_folio_1 ON tmp_folio(folio)
      UPDATE STATISTICS FOR TABLE tmp_folio

END FUNCTION
#####################################################################
FUNCTION genera_patrones()

   DEFINE mig_folio INTEGER

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

   LET x_ano   = YEAR (TODAY)
   LET xx_ano  = YEAR (g_reg.fecha_ini)
   LET xxx_ano = YEAR (g_reg.fecha_top)

   IF xxx_ano = x_ano THEN
      LET xxx_ano = YEAR (g_reg.fecha_top) - 1
      LET sw = TRUE
   END IF

   LET i = 0

   WHENEVER ERROR CONTINUE

   IF xx_ano = 2006 OR xx_ano = 2007 THEN
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

   WHENEVER ERROR STOP

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta (subcuenta,
                                            tipo_movimiento)
   CREATE INDEX tmp_cuenta_2 ON tmp_cuenta (fecha_conversion)
   CREATE INDEX tmp_cuenta_3 ON tmp_cuenta (id_aportante)
   UPDATE STATISTICS FOR TABLE tmp_cuenta

   IF sw_1 THEN
      DELETE
      FROM  tmp_cuenta
      WHERE subcuenta > 0
      AND   tipo_movimiento = 999

      UPDATE STATISTICS FOR TABLE tmp_cuenta
   END IF

   LET i = 0

END FUNCTION #genera_patrones
#####################################################################
FUNCTION datos_trabajador()

   INITIALIZE w_aux.callep   TO NULL
   INITIALIZE w_aux.numep    TO NULL
   INITIALIZE w_aux.deptop   TO NULL
   INITIALIZE w_aux.coloniap TO NULL
   INITIALIZE w_aux.delegap  TO NULL
   INITIALIZE w_aux.estadop  TO NULL
   INITIALIZE w_aux.codposp  TO NULL
   INITIALIZE w_aux.ciudadp  TO NULL

   INITIALIZE w_aux.fonop TO NULL

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

   LET   vestado_envio = "0"

   IF x_tipo_informe = 3 THEN

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


      EXECUTE eje_sel_4 USING g_reg.nss
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

      LET ind_edad = 0
      LET reg_gen.ind_siefore = "0"
{
      SELECT c.tipo_registro
      INTO   ind_edad
      FROM   safre_tmp:afi_edo_cta c
      WHERE  c.nss = g_reg.nss
}
      IF ind_edad = 1 THEN
         LET reg_gen.ind_siefore = "2"

         SELECT "X"
         FROM   afi_transf_siefore
         WHERE  nss = g_reg.nss
         AND    edo_sol = 0

         IF SQLCA.SQLCODE = 0 THEN
            LET reg_gen.ind_siefore = "1"
         END IF
      END IF
      IF ind_edad = 2 THEN
         LET reg_gen.ind_siefore = "1"
      END IF
   END IF

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

      IF pfecha_saldo < "01/31/2006" THEN
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

   DECLARE c_saldo CURSOR FOR  eje_saldo_dia
   FOREACH c_saldo INTO f_subcuenta,
                        f_siefore,
                        f_monto_acc,
                        f_monto_pes

      CASE
         WHEN f_subcuenta = 7
            LET sar_imss_ini = sar_imss_ini + f_monto_pes
         WHEN f_subcuenta = 13
            LET sar_issste_ini = sar_issste_ini + f_monto_pes

         WHEN f_subcuenta = 8
            LET viv_92_imss_ini = viv_92_imss_ini + f_monto_pes
         WHEN f_subcuenta = 14
            LET viv_92_issste_ini = viv_92_issste_ini + f_monto_pes

         WHEN f_subcuenta = 3 OR f_subcuenta = 10
            LET vol_ini = vol_ini + f_monto_pes
         WHEN f_subcuenta = 11 OR f_subcuenta = 12
            LET com_ini = com_ini + f_monto_pes
         WHEN f_subcuenta = 16
            LET alp_ini = alp_ini + f_monto_pes
      END CASE
   END FOREACH

   LET sub_ini_rcv = sar_imss_ini +
                     sar_issste_ini

   LET sub_viv_ini = viv_92_imss_ini +
                     viv_92_issste_ini

   LET sub_vcom_ini= vol_ini +
                     com_ini +
                     alp_ini --- actualizacion para 01-07-2006

   LET total_general_ini    = sub_ini_rcv +
                              sub_vcom_ini +
                              sub_viv_ini
-----------------------------------------------------------------------
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
         WHEN f_subcuenta = 7
            LET sar_imss_fin = sar_imss_fin + f_monto_pes
         WHEN f_subcuenta = 13
            LET sar_issste_fin = sar_issste_fin + f_monto_pes

         WHEN f_subcuenta = 8
            LET viv_92_imss_fin = viv_92_imss_fin + f_monto_pes
         WHEN f_subcuenta = 14
            LET viv_92_issste_fin = viv_92_issste_fin + f_monto_pes

         WHEN f_subcuenta = 3 OR f_subcuenta = 10
            LET vol_fin = vol_fin + f_monto_pes
         WHEN f_subcuenta = 11 OR f_subcuenta = 12
            LET com_fin = com_fin + f_monto_pes
         WHEN f_subcuenta = 16
            LET alp_fin = alp_fin + f_monto_pes
      END CASE

      CASE f_siefore
         WHEN 1
            CASE
               WHEN  f_subcuenta = 7 OR
                     f_subcuenta = 13
                  LET fondo_retiro_1 = fondo_retiro_1 + f_monto_pes

               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_voluntaria_1 = fondo_voluntaria_1 + f_monto_pes
            END CASE
         WHEN 2
            CASE
               WHEN  f_subcuenta = 7 OR
                     f_subcuenta = 13
                  LET fondo_retiro_2 = fondo_retiro_2 + f_monto_pes

               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_voluntaria_2 = fondo_voluntaria_2 + f_monto_pes
            END CASE
         WHEN 3
            CASE
               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_adicional_vol = fondo_adicional_vol + f_monto_pes
            END CASE
      END CASE
   END FOREACH

   LET sub_fin_rcv = sar_imss_fin +
                     sar_issste_fin

   LET sub_viv_fin = viv_92_imss_fin +
                     viv_92_issste_fin

   LET sub_vcom_fin= vol_fin +
                     com_fin +
                     alp_fin

   LET total_general_fin = sub_fin_rcv +
                           sub_vcom_fin +
                           sub_viv_fin

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
----------------------------------------------------------------------------
   DECLARE c_saldo_sem CURSOR FOR eje_sel_5

   FOREACH c_saldo_sem USING g_reg.nss,
                             xfecha_inix
                        INTO f_subcuenta,
                             f_siefore,
                             f_monto_acc,
                             f_monto_pes

      CASE
         WHEN f_subcuenta = 7
            LET sar_imss_ini = sar_imss_ini + f_monto_pes        --id21
         WHEN f_subcuenta = 13
            LET sar_issste_ini = sar_issste_ini + f_monto_pes    --id22

         WHEN f_subcuenta = 8
            LET viv_92_imss_ini = viv_92_imss_ini + f_monto_pes      --id43
         WHEN f_subcuenta = 14
            LET viv_92_issste_ini = viv_92_issste_ini + f_monto_pes  --id44

         WHEN f_subcuenta = 3 OR f_subcuenta = 10    --id60
            LET vol_ini = vol_ini + f_monto_pes
         WHEN f_subcuenta = 11 OR f_subcuenta = 12
            LET com_ini = com_ini + f_monto_pes
         WHEN f_subcuenta = 16
            LET alp_ini = alp_ini + f_monto_pes
      END CASE
   END FOREACH
   FREE c_saldo_sem

   LET sub_ini_rcv  = sar_imss_ini +
                      sar_issste_ini

   LET sub_viv_ini  = viv_92_imss_ini +
                      viv_92_issste_ini

   LET sub_vcom_ini= vol_ini +
                     com_ini +
                     alp_ini

   LET total_general_ini    = sub_ini_rcv +
                              sub_vcom_ini +
                              sub_viv_ini         --id70
-----------------------------------------------------------------------

   DECLARE c_saldo1_sem CURSOR FOR eje_sel_5
   FOREACH c_saldo1_sem USING g_reg.nss,
                              g_reg.fecha_top
                          INTO f_subcuenta,
                               f_siefore,
                               f_monto_acc,
                               f_monto_pes

      CASE
         WHEN f_subcuenta = 7
            LET sar_imss_fin = sar_imss_fin + f_monto_pes        --id39
         WHEN f_subcuenta = 13
            LET sar_issste_fin = sar_issste_fin + f_monto_pes    --id40

         WHEN f_subcuenta = 8
            LET viv_92_imss_fin = viv_92_imss_fin + f_monto_pes      --id55
         WHEN f_subcuenta = 14
            LET viv_92_issste_fin = viv_92_issste_fin + f_monto_pes  --id56

         WHEN f_subcuenta = 3 OR f_subcuenta = 10    --id69
            LET vol_fin = vol_fin + f_monto_pes
         WHEN f_subcuenta = 11 OR f_subcuenta = 12
            LET com_fin = com_fin + f_monto_pes
         WHEN f_subcuenta = 16
            LET alp_fin = alp_fin + f_monto_pes
      END CASE

      CASE f_siefore
         WHEN 1
            CASE
               WHEN  f_subcuenta = 7 OR
                     f_subcuenta = 13
                  LET fondo_retiro_1 = fondo_retiro_1 + f_monto_pes

               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_voluntaria_1 = fondo_voluntaria_1 + f_monto_pes
            END CASE
         WHEN 2
            CASE
               WHEN  f_subcuenta = 7 OR
                     f_subcuenta = 13
                  LET fondo_retiro_2 = fondo_retiro_2 + f_monto_pes

               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_voluntaria_2 = fondo_voluntaria_2 + f_monto_pes
            END CASE
         WHEN 3
            CASE
               WHEN f_subcuenta = 3 OR
                     f_subcuenta = 10 OR
                     f_subcuenta = 11 OR
                     f_subcuenta = 12 OR
                     f_subcuenta = 16
                 LET fondo_adicional_vol = fondo_adicional_vol + f_monto_pes
            END CASE
      END CASE
   END FOREACH
   FREE c_saldo1_sem

   IF x_estado = 34 THEN
      LET fondo_retiro_1 = fondo_retiro_1 + fondo_voluntaria_1
      LET fondo_retiro_2 = fondo_retiro_2 + fondo_voluntaria_2
   END IF

   LET sub_fin_rcv  = sar_imss_fin +
                      sar_issste_fin

   LET sub_viv_fin  = viv_92_imss_fin +
                      viv_92_issste_fin

   LET sub_vcom_fin= vol_fin +
                     com_fin +
                     alp_fin

   LET total_general_fin = sub_fin_rcv +
                           sub_vcom_fin +
                           sub_viv_fin

   LET sub_sem_rcv         = sub_fin_rcv - sub_ini_rcv
   LET sub_viv_sem         = sub_viv_fin - sub_viv_ini
   LET sub_vcom_sem        = sub_vcom_fin - sub_vcom_ini

   LET total_semestre = sub_sem_rcv +
                        sub_viv_sem +
                        sub_vcom_sem      --id73
END FUNCTION
#####################################################################
FUNCTION rendimiento_viv()

   DEFINE int_mes            DECIMAL(16,6)
   DEFINE vtasa_oper         DECIMAL(16,6)
   DEFINE vfecha_aplica      DATE
   DEFINE vfecha_aplica1     DATE
   DEFINE vmes               CHAR(2)
   DEFINE vtotal_mes         DECIMAL(16,6)
   DEFINE vdias              SMALLINT
   DEFINE vdia               SMALLINT
   DEFINE opc                CHAR(1)

   LET vfecha_aplica1 = g_reg.fecha_top

   IF DAY(g_reg.fecha_top) = 31 THEN
      LET vfecha_aplica1 = g_reg.fecha_top -1 UNITS DAY
   END IF

   DECLARE c_int CURSOR FOR
   SELECT a.tasa_valor,
          a.tasa_fecha - 1 UNITS MONTH
   FROM   tab_tasa_remanente a
   WHERE  a.tasa_origen = "VIV"
   AND    a.tasa_fecha  BETWEEN g_reg.fecha_ini + 1 UNITS DAY AND
                              vfecha_aplica1 + 3 UNITS DAY
   ORDER  BY 2

   LET int_mes = 0
   LET vdias   = 0
   LET vdia    = 0

   FOREACH c_int INTO vtasa_oper, vfecha_aplica

      LET vdia = calcula_dias(vfecha_aplica)

      LET int_mes = int_mes + (vtasa_oper * vdia / 360)
      LET vdias   = vdias + vdia
   END FOREACH

   LET rendimiento_infonavit = int_mes / vdias * 360

END FUNCTION
#############################################################################
FUNCTION rendimiento_fov()

   DEFINE int_mes            DECIMAL(16,6)
   DEFINE vtasa_oper         DECIMAL(16,6)
   DEFINE vfecha_aplica      DATE
   DEFINE vfecha_aplica1     DATE
   DEFINE vmes               CHAR(2)
   DEFINE vtotal_mes         DECIMAL(16,6)
   DEFINE vdias              SMALLINT
   DEFINE vdia               SMALLINT
   DEFINE opc                CHAR(1)

   LET vfecha_aplica1 = g_reg.fecha_top

   IF DAY(g_reg.fecha_top) = 31 THEN
      LET vfecha_aplica1 = g_reg.fecha_top -1 UNITS DAY
   END IF

   DECLARE c_int_fov CURSOR FOR
   SELECT b.tasa_valor,
          b.tasa_fecha - 1 UNITS MONTH
   FROM   tab_tasa_remanente b
   WHERE  b.tasa_origen = "FOV"
   AND    b.tasa_fecha  BETWEEN g_reg.fecha_ini + 1 UNITS DAY AND
                              vfecha_aplica1 + 3 UNITS DAY
   ORDER  BY 2

   LET int_mes = 0
   LET vdias   = 0
   LET vdia    = 0

   FOREACH c_int_fov INTO vtasa_oper, vfecha_aplica
      LET vdia = calcula_dias(vfecha_aplica)

      LET int_mes = int_mes + (vtasa_oper * vdia / 360)
      LET vdias   = vdias + vdia
   END FOREACH

   LET rendimiento_fovissste = int_mes / vdias * 360

END FUNCTION
#############################################################################
FUNCTION resumen_comisiones()

   DEFINE xxxx_cuantos  SMALLINT

   DEFINE con_comis SMALLINT
   DEFINE vtipo     SMALLINT
   DEFINE vacciones    DECIMAL(16,6)
   DEFINE xcomision    DECIMAL(16,6)
   DEFINE tcomision    DECIMAL(16,6)
   DEFINE vfecha_acc   DATE
   DEFINE xfecha_pri_general DATE
   DEFINE dias_antiguedad INTEGER

   LET comircv = 0
   LET comivol = 0
   LET comisal = 0
   LET comiadi = 0

   LET estructura_saldo = 2.67

    LET comircv0 = 0
    LET comircv1 = 0
    LET comircv2 = 0
    LET id_comision      = "0"
    LET vfecha_comision0 = ""
    LET vfecha_comision1 = ""

    LET   con_comis = 0
    LET   comision_flujo0 = 0
    LET   comision_flujo1 = 0
    LET   comision_flujo2 = 0

    LET  comircv = comircv0

    IF comircv IS NULL THEN
        LET comircv = 0
    END IF

    SELECT SUM(ROUND(monto_en_pesos,2))*(-1)
    INTO   comisal
    FROM   tmp_cuenta
    WHERE  subcuenta        IN (3,7,10,11,12,13,15,16)  --- actualizacion para 01-07-2006
    AND    tipo_movimiento  = 110
    AND    fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
    AND    siefore IN (1,2)

    IF comisal IS NULL THEN
       LET comisal = 0
    END IF

    SELECT SUM(ROUND(monto_en_pesos,2))*(-1)
    INTO   comiadi
    FROM   tmp_cuenta
    WHERE  subcuenta   IN (3,10,11,12,15,16)
    AND    tipo_movimiento  = 110
    AND    fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
    AND    siefore = 3

    IF comiadi IS NULL THEN
       LET comiadi = 0
    END IF

   LET comision_flujo1 = comircv
   LET comision_saldo  = comisal
   --LET comision_adicional  = comiadi

   LET total_comisiones = comisal + comivol + comircv + comiadi

END FUNCTION
#####################################################################
FUNCTION calculo_rendimiento()
   DEFINE rendimiento RECORD
          siefore           SMALLINT,
          fecha_conversion  DATE,
          monto_en_acciones DECIMAL(16,6)
   END RECORD

   DEFINE rendimiento_ini       DECIMAL(16,6),
          rendimiento_fin       DECIMAL(16,6),
          rendimiento_calculado DECIMAL(16,6),
          total_rendimiento     DECIMAL(16,6),
          x_ren_precio_dia_ini  DECIMAL(16,6),
          x_ren_precio_dia_fin  DECIMAL(16,6)

   LET rendimiento_ini       = 0
   LET rendimiento_fin       = 0
   LET rendimiento_calculado = 0
   LET total_rendimiento     = 0

   DECLARE cur_rendimiento CURSOR FOR
   SELECT siefore,
          fecha_conversion,
          monto_en_acciones
   FROM   tmp_cuenta
   WHERE  subcuenta <> 14
   AND    tipo_movimiento = 1
   AND    fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top

--display "nss:",g_reg.nss
   FOREACH cur_rendimiento INTO rendimiento.*
      SELECT precio_del_dia
      INTO   x_ren_precio_dia_ini
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = rendimiento.fecha_conversion
      AND    codigo_siefore  = rendimiento.siefore

      SELECT precio_del_dia
      INTO   x_ren_precio_dia_fin
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = g_reg.fecha_top
      AND    codigo_siefore  = rendimiento.siefore

--display "rendimiento.monto_en_acciones:",rendimiento.monto_en_acciones
--display "x_ren_precio_dia_ini:",x_ren_precio_dia_ini
--display "x_ren_precio_dia_fin:",x_ren_precio_dia_fin
      LET rendimiento_ini = rendimiento.monto_en_acciones * x_ren_precio_dia_ini
      LET rendimiento_fin = rendimiento.monto_en_acciones * x_ren_precio_dia_fin
--display "rendimiento_ini:",rendimiento_ini
--display "rendimiento_fin:",rendimiento_fin
      LET rendimiento_calculado = rendimiento_fin - rendimiento_ini
--display "rendimiento_calculado:",rendimiento_calculado
      LET total_rendimiento = total_rendimiento + rendimiento_calculado
--display "total_rendimiento:",total_rendimiento
--display "________________________________________________"
   END FOREACH

   LET comision_adicional = total_rendimiento
   DISPLAY g_reg.nss," total_rendimiento:", comision_adicional
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

   CREATE INDEX tmp_aporte_1 ON tmp_aporte (n_seguro)
   UPDATE STATISTICS FOR TABLE tmp_aporte

   SELECT SUM(monto_en_pesos)
   INTO   aporte_vol
   FROM   tmp_cuenta
   WHERE  subcuenta        IN (10,12,16) --- actualizacion para 01-07-2006
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
            LET periodo_pago1   = reg_9.periodo_pago
            LET reg_patron1     = reg_9.reg_patron
            LET sbc1           = reg_9.sbc
            LET dias_cotizados1 = reg_9.dias_cotizados
         WHEN 2
            LET periodo_pago2   = reg_9.periodo_pago
            LET reg_patron2     = reg_9.reg_patron
            LET sbc2           = reg_9.sbc
            LET dias_cotizados2 = reg_9.dias_cotizados
         WHEN 3
            LET periodo_pago3   = reg_9.periodo_pago
            LET reg_patron3     = reg_9.reg_patron
            LET sbc3           = reg_9.sbc
            LET dias_cotizados3 = reg_9.dias_cotizados
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

   DECLARE cursor_retiros CURSOR FOR
   SELECT a.subcuenta,
          a.id_aportante,
          ROUND (SUM(a.monto_en_pesos),2)
   FROM   tmp_cuenta a
   WHERE  a.subcuenta > 0
   AND    a.tipo_movimiento IN ( SELECT b.codigo
                                 FROM   tab_movimiento b
                                 WHERE (b.codigo > 199
                                 OR     b.codigo = 7
                                 OR     b.codigo = 10)
                                 AND    b.tipo = -1
                               )
   AND    a.fecha_conversion BETWEEN g_reg.fecha_ini AND g_reg.fecha_top
   GROUP BY 1,2

   FOREACH cursor_retiros INTO r_subcuenta,
                               r_id_aportante,
                               r_monto_pes

      IF r_id_aportante = "CORTE" THEN
         CONTINUE FOREACH
      END IF

      CASE
         WHEN r_subcuenta = 7
            LET sar_imss_retiros = sar_imss_retiros + r_monto_pes
         WHEN r_subcuenta = 13
            LET sar_issste_retiros = sar_issste_retiros + r_monto_pes

         WHEN r_subcuenta = 8
            LET viv_92_imss_retiros = viv_92_imss_retiros + r_monto_pes
         WHEN r_subcuenta = 14
            LET viv_92_issste_retiros = viv_92_issste_retiros + r_monto_pes

         WHEN r_subcuenta = 3 OR r_subcuenta = 10
            LET vol_retiros = vol_retiros + r_monto_pes
         WHEN r_subcuenta = 11 OR r_subcuenta = 12
            LET com_retiros = com_retiros + r_monto_pes
         WHEN r_subcuenta = 16          --- actualizacion para 01-07-2006
            LET alp_retiros = alp_retiros + r_monto_pes     --- actualizacion para 01-07-2006
      END CASE
   END FOREACH

   LET sub_retiros_rcv = sar_imss_retiros +
                         sar_issste_retiros

   LET sub_viv_retiros = viv_92_imss_retiros +
                         viv_92_issste_retiros

   LET sub_vcom_retiros= vol_retiros +
                         com_retiros +
                         alp_retiros    --- actualizacion para 01-07-2006

   LET total_general_retiros    = sub_retiros_rcv +
                                  sub_vcom_retiros +
                                  sub_viv_retiros

END FUNCTION
#####################################################################
FUNCTION aportes_netos()


   LET sar_netas_imss        = sar_imss_fin - sar_imss_ini + (sar_imss_retiros *-1)
   LET sar_netas_issste      = sar_issste_fin - sar_issste_ini + (sar_issste_retiros * -1)
   LET sub_netas_rcv         = sar_netas_imss +
                               sar_netas_issste

   LET viv_92_imss_netas     = viv_92_imss_fin - viv_92_imss_ini + (viv_92_imss_retiros * -1)
   LET viv_92_issste_netas   = viv_92_issste_fin - viv_92_issste_ini + (viv_92_issste_retiros * -1)
   LET sub_viv_netas         = viv_92_imss_netas +
                               viv_92_issste_netas

   LET vol_netas             = vol_fin - vol_ini + (vol_retiros * -1)
   LET com_netas             = com_fin - com_ini + (com_retiros * -1)
   LET alp_netas             = alp_fin - alp_ini + (alp_retiros * -1)
   LET sub_vcom_netas        = vol_netas +
                               com_netas +
                               alp_netas

   LET total_general_netas   = sub_netas_rcv +
                               sub_vcom_netas +
                               sub_viv_netas

END FUNCTION
#####################################################################
FUNCTION ingresa_estado()

   INITIALIZE reg_gen.*  TO NULL

   LET rendimiento_adicional = 0

   LET estructura_flujo2     = 0
   LET comision_flujo2       = 0
   LET estructura_adicional  = 0

   IF x_tipo_informe <> 0 THEN
      LET w_aux.nombres = w_aux.nombres[1,40] CLIPPED," ",
                          w_aux.paterno[1,40] CLIPPED," ",
                          w_aux.materno[1,40] CLIPPED

      LET w_aux.callep = w_aux.callep        CLIPPED," ",
                         w_aux.numep         CLIPPED," ",
                         w_aux.deptop        CLIPPED
   END IF

   IF x_tipo_informe <> 3 THEN
      LET x_fec_liq_tras  = ""
      LET x_fec_sol_tras  = ""
      LET x_fena      = ""
      LET x_nacionalidad_desc = ""
      LET w_aux.fonop = ""

      LET x_nombres = ""
      LET x_desc_afo_recep = ""
      LET x_fecha_reingreso = ""
   END IF

   IF x_tipo_informe <> 4 THEN
      LET x_correo_electronico = " "
   END IF

   LET genera_detalle = pos,"|",
                        g_reg.fecha_ini USING "DDMMYYYY","|",
                        g_reg.fecha_top USING "DDMMYYYY","|",
                        w_aux.nombres,"|",
                        w_aux.callep ,"|",
                        w_aux.coloniap,"|",
                        w_aux.codposp,"|",
                        vcentro_reparto,"|",
                        w_aux.estadop,"|",
                        w_aux.delegap,"|",
                        w_aux.fentcons USING "DDMMYYYY","|",
                        "0","|",
                        "0","|",

                        g_reg.nss,"|",
                        w_aux.rfc,"|",
                        w_aux.n_unico,"|",

                        retiro97_ini USING "-&&&&&&&.&&","|",
                        cv_ini USING "-&&&&&&&.&&","|",
                        cs_ini USING "-&&&&&&&.&&","|",
                        sar_imss_ini USING "-&&&&&&&.&&","|",
                        sar_issste_ini USING "-&&&&&&&.&&","|",
                        sub_ini_rcv USING "-&&&&&&&.&&","|",

                        retiro97_netas USING "-&&&&&&&.&&","|",
                        cv_netas USING "-&&&&&&&.&&","|",
                        cs_netas USING "-&&&&&&&.&&","|",
                        sar_netas_imss USING "-&&&&&&&.&&","|",
                        sar_netas_issste USING "-&&&&&&&.&&","|",
                        sub_netas_rcv USING "-&&&&&&&.&&","|",

                        retiro97_retiros USING "-&&&&&&&.&&","|",
                        cv_retiros USING "-&&&&&&&.&&","|",
                        cs_retiros USING "-&&&&&&&.&&","|",
                        sar_imss_retiros USING "-&&&&&&&.&&","|",
                        sar_issste_retiros USING "-&&&&&&&.&&","|",
                        sub_retiros_rcv USING "-&&&&&&&.&&","|",

                        retiro97_fin USING "-&&&&&&&.&&","|",
                        cv_fin USING "-&&&&&&&.&&","|",
                        cs_fin USING "-&&&&&&&.&&","|",
                        sar_imss_fin USING "-&&&&&&&.&&","|",
                        sar_issste_fin USING "-&&&&&&&.&&","|",
                        sub_fin_rcv USING "-&&&&&&&.&&","|",

                        viv_97_imss_ini USING "-&&&&&&&.&&","|",
                        viv_92_imss_ini USING "-&&&&&&&.&&","|",
                        viv_92_issste_ini USING "-&&&&&&&.&&","|",
                        sub_viv_ini USING "-&&&&&&&.&&","|",

                        viv_97_imss_netas USING "-&&&&&&&.&&","|",
                        viv_92_imss_netas USING "-&&&&&&&.&&","|",
                        viv_92_issste_netas USING "-&&&&&&&.&&","|",
                        sub_viv_netas USING "-&&&&&&&.&&","|",

                        viv_97_imss_retiros USING "-&&&&&&&.&&","|",
                        viv_92_imss_retiros USING "-&&&&&&&.&&","|",
                        viv_92_issste_retiros USING "-&&&&&&&.&&","|",
                        sub_viv_retiros USING "-&&&&&&&.&&","|",

                        viv_97_imss_fin USING "-&&&&&&&.&&","|",
                        viv_92_imss_fin USING "-&&&&&&&.&&","|",
                        viv_92_issste_fin USING "-&&&&&&&.&&","|",
                        sub_viv_fin USING "-&&&&&&&.&&","|",

                        vol_ini USING "-&&&&&&&.&&","|",
                        com_ini USING "-&&&&&&&.&&","|",
                        sub_vcom_ini USING "-&&&&&&&.&&","|",

                        vol_netas USING "-&&&&&&&.&&","|",
                        com_netas USING "-&&&&&&&.&&","|",
                        sub_vcom_netas USING "-&&&&&&&.&&","|",

                        vol_retiros USING "-&&&&&&&.&&","|",
                        com_retiros USING "-&&&&&&&.&&","|",
                        sub_vcom_retiros USING "-&&&&&&&.&&","|",

                        vol_fin USING "-&&&&&&&.&&","|",
                        com_fin USING "-&&&&&&&.&&","|",
                        sub_vcom_fin USING "-&&&&&&&.&&","|",

                        total_general_ini USING "-&&&&&&&.&&","|",
                        total_general_netas USING "-&&&&&&&.&&","|",
                        total_general_retiros USING "-&&&&&&&.&&","|",
                        total_general_fin USING "-&&&&&&&.&&","|",

                        sub_sem_rcv USING "-&&&&&&&.&&","|",                    #sub_netas_rcv
                        sub_viv_sem USING "-&&&&&&&.&&","|",                    #sub_viv_netas
                        sub_vcom_sem USING "-&&&&&&&.&&","|",                   #sub_vcom_netas

                        total_semestre USING "-&&&&&&&.&&","|",

                        periodo_pago1,"|",
                        reg_patron1,"|",
                        sbc1 USING "&&&&&&&&.&&","|",
                        dias_cotizados1 ,"|",
                        periodo_pago2,"|",
                        reg_patron2,"|",
                        sbc2 USING "&&&&&&&&.&&","|",
                        dias_cotizados2,"|",
                        periodo_pago3,"|",
                        reg_patron3,"|",
                        sbc3 USING "&&&&&&&&.&&","|",
                        dias_cotizados3,"|",

                        rendimiento_siefore1 USING "&&&.&&","|",
                        rendimiento_siefore2 USING "&&&.&&","|",
                        rendimiento_adicional USING "&&&.&&","|",
                        rendimiento_infonavit USING "&&&.&&","|",
                        rendimiento_fovissste USING "&&&.&&","|",
                        "0","|",

                        estructura_flujo1 USING "&&&.&&","|",
                        comision_flujo1 USING "&&&&&&&&.&&","|",

                        estructura_flujo2 USING "&&&.&&","|",
                        comision_flujo2 USING "&&&&&&&&.&&","|",

                        estructura_saldo USING "&&&.&&","|",
                        comision_saldo USING "&&&&&&&&.&&","|",

                        total_comisiones USING "&&&&&&&&.&&","|",

                        estructura_adicional USING "&&&.&&","|",
                        comision_adicional USING "&&&&&&&&.&&","|",
                        --total_rendimiento USING "&&&&&&&&.&&","|",

                        ind_fiscal,"|",
                        fecha_rcv_fiscal USING "DDMMYYYY","|",
                        monto_rcv_fiscal USING "&&&&&&&&.&&","|",
                        imp_rcv_fiscal USING "&&&&&&&&.&&","|",
                        fecha_sar_fiscal USING "DDMMYYYY","|",
                        monto_sar_fiscal USING "&&&&&&&&.&&","|",
                        imp_sar_fiscal USING "&&&&&&&&.&&","|",
                        fecha_sar_issste_fiscal USING "DDMMYYYY","|",
                        monto_sar_issste_fiscal USING "&&&&&&&&.&&","|",
                        imp_sar_issste_fiscal USING "&&&&&&&&.&&","|",
                        fecha_vol_fiscal USING "DDMMYYYY","|",
                        monto_vol_fiscal USING "&&&&&&&&.&&","|",
                        imp_vol_fiscal USING "&&&&&&&&.&&","|",
                        fecha_com_fiscal USING "DDMMYYYY","|",
                        monto_com_fiscal USING "&&&&&&&&.&&","|",
                        imp_com_fiscal USING "&&&&&&&&.&&","|",
                        interes_real USING "&&&&&&&&.&&","|",
                        interes_nominal USING "&&&&&&&&.&&","|",

                        fondo_retiro_1 USING "&&&&&&&&.&&","|",
                        fondo_voluntaria_1 USING "&&&&&&&&.&&","|",
                        fondo_retiro_2 USING "&&&&&&&&.&&","|",
                        fondo_voluntaria_2 USING "&&&&&&&&.&&","|",
                        fondo_adicional_vol USING "&&&&&&&&.&&","|",

                        w_aux.n_folio,"|",
                        x_fec_liq_tras  ,"|",
                        x_fec_sol_tras  ,"|",
                        x_fena     ,"|",
                        x_nacionalidad_desc ,"|",
                        w_aux.fonop ,"|",
                        x_nombres ,"|",
                        x_desc_afo_recep ,"|",
                        x_fecha_reingreso,"|",
                        --rendimiento_infonavit USING "&&&.&&&&","|",
                        --rendimiento_fovissste USING "&&&.&&&&","|",
                        x_correo_electronico,"|",

                        x_afore_recep USING "&&&","|",

                        alp_ini     USING "-&&&&&&&.&&","|",
                        alp_netas   USING "-&&&&&&&.&&","|",
                        alp_retiros USING "-&&&&&&&.&&","|",
                        alp_fin     USING "-&&&&&&&.&&","|",
                        fecha_alp_fiscal USING "DDMMYYYY","|",
                        monto_alp_fiscal USING "&&&&&&&&.&&","|",
                        imp_alp_fiscal   USING "&&&&&&&&.&&"

--DISPLAY g_reg.nss," total_rendimiento:", comision_adicional
END FUNCTION
#####################################################################
FUNCTION informacion_fiscal()

   DEFINE xsubcuenta SMALLINT
   DEFINE xfecha_fis DATE
   DEFINE xmonto_fis DECIMAL(16,6)
   DEFINE xtipo_fis  SMALLINT
   DEFINE fcont      SMALLINT
   DEFINE vfecha_fiscal DATE

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
   LET dia       = DAY(g_reg.fecha_top)-1
   LET mes       = MONTH(g_reg.fecha_top)-1
   LET vfecha_fiscal    = g_reg.fecha_top - dia UNITS DAY
   LET vfecha_fiscal    = vfecha_fiscal - mes UNITS MONTH

   SELECT "X"
   FROM   tmp_cuenta
   WHERE  subcuenta        IN (3,7,10,11,12,13,16)
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
      (
      tipo_movimiento smallint not null ,
      subcuenta smallint not null ,
      siefore smallint,
      folio integer not null ,
      consecutivo_lote integer,
      nss char(11) not null ,
      curp char(18),
      folio_sua char(6),
      fecha_pago date,
      fecha_valor date,
      fecha_conversion date,
      monto_en_pesos decimal(22,6),
      monto_en_acciones decimal(22,6),
      precio_accion decimal(22,6),
      dias_cotizados smallint,
      sucursal char(10),
      id_aportante char(11),
      estado smallint,
      fecha_proceso date,
      usuario char(8),
      fecha_archivo date,
      etiqueta smallint
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
      WHERE  subcuenta IN (3,7,10,11,12,16)   --- actualizacion para 01-07-2006
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
            WHEN xsubcuenta = 3 OR xsubcuenta = 10
               IF xtipo_fis = 10 THEN
                  LET imp_vol_fiscal   = imp_vol_fiscal   + xmonto_fis
               ELSE
                  LET fecha_vol_fiscal = xfecha_fis
                  LET monto_vol_fiscal = monto_vol_fiscal + xmonto_fis
                  LET fcont = fcont + 1
               END IF
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

   DEFINE
      i_precio_fin         DECIMAL(16,6),
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

   DEFINE com_1x ARRAY [25] OF RECORD
          porcentaje   CHAR(5)
   END RECORD

   DEFINE x_tipo_comparativo    SMALLINT

   LET x_tipo_comparativo = 1

   IF x_estado = 34  THEN
      LET x_tipo_comparativo = 3
   END IF

   IF x_estado = 36 THEN
      LET x_tipo_comparativo = 5
   END IF

   IF x_estado = 35 THEN
      LET x_tipo_comparativo = 7
   END IF

   SELECT "X"
   FROM   tab_comparativo
   WHERE  fecha = g_reg.fecha_top
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      SELECT MAX(fecha)
      INTO   xc_fecha
      FROM   tab_comparativo
      WHERE  tipo_comparativo = x_tipo_comparativo
   ELSE
      LET xc_fecha = g_reg.fecha_top
   END IF

   IF x_tipo_informe = 0 THEN
      --LET xc_fecha = "05/31/2007"
      LET xc_fecha = "11/30/2007"
   END IF

   DECLARE com1 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.porcentaje,
          a.fecha
   FROM   tab_comparativo a ,OUTER tab_afore b
   WHERE  a.fecha     = xc_fecha
   AND    a.tipo_comparativo = x_tipo_comparativo
   AND    b.afore_cod = a.afore_cod
   ORDER BY 3,2

   LET cont_com = 1

   FOREACH com1 INTO com_1[cont_com].*
      IF com_1[cont_com].afore_cod = 999 THEN
         LET com_1[cont_com].afore_desc = "Promedio"
      END IF

      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_1[cont_com].afore_desc IS NULL THEN
         LET genera_sum97 = "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum97,229,97,x_folio)
      ELSE

         LET genera_sum97 = com_1[cont_com].afore_desc,"|",
                            com_1[cont_com].porcentaje USING "&&.&&","|",
                            com_1[cont_com].fecha USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum97,229,97,x_folio)
       END IF
   END FOR

   LET x_tipo_comparativo = 2

   IF x_estado = 34 THEN
      LET x_tipo_comparativo = 4
   END IF

   IF x_estado = 36 THEN
      LET x_tipo_comparativo = 6
   END IF

   IF x_estado = 35 THEN
      LET x_tipo_comparativo = 8
   END IF

   SELECT "X"
   FROM   tab_comparativo
   WHERE  fecha = g_reg.fecha_top
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      SELECT MAX(fecha)
      INTO   xc_fecha
      FROM   tab_comparativo
      WHERE  tipo_comparativo = x_tipo_comparativo
   ELSE
      LET xc_fecha = g_reg.fecha_top
   END IF

   IF x_tipo_informe = 0 THEN
      --LET xc_fecha = "05/31/2007"
      LET xc_fecha = "11/30/2007"
   END IF

   DECLARE com2 CURSOR FOR
   SELECT a.afore_cod,
          b.afore_desc,
          a.porcentaje,
          a.fecha
   FROM   tab_comparativo a ,OUTER tab_afore b
   WHERE  a.fecha     = xc_fecha
   AND    a.tipo_comparativo = x_tipo_comparativo
   AND    b.afore_cod = a.afore_cod
   ORDER BY 3 desc,2

   LET cont_com = 1

   FOREACH com2 INTO com_1[cont_com].*
      IF com_1[cont_com].afore_cod = 999 THEN
         LET com_1[cont_com].afore_desc = "Promedio"
      END IF

      IF com_1[cont_com].porcentaje = 0.0 THEN
         LET com_1x[cont_com].porcentaje = "NA*"
      ELSE
         LET com_1x[cont_com].porcentaje = com_1[cont_com].porcentaje
      END IF

      LET cont_com = cont_com + 1
   END FOREACH

   FOR cont_com = 1 TO 25
      IF com_1[cont_com].afore_desc IS NULL THEN
         LET genera_sum98 = "|",
                            "|",
                            "|"

         OUTPUT TO REPORT r_report(genera_sum98,229,98,x_folio)
      ELSE

         LET genera_sum98 = com_1[cont_com].afore_desc,"|",
                            com_1x[cont_com].porcentaje ,"|",
                            com_1[cont_com].fecha USING "DDMMYYYY","|"

         OUTPUT TO REPORT r_report(genera_sum98,229,98,x_folio)
       END IF
   END FOR

   LET genera_sum99 = HOY USING "DDMMYYYY","|",
                      x_folio USING "&&&&&&&&","|",
                      pos USING "&&&&&&&&","|"

   OUTPUT TO REPORT r_report(genera_sum99,229,99,x_folio)

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

   DECLARE cur_mail CURSOR FOR
   SELECT cod_correo_e,
          correo_elect
   FROM   afi_correo_elect
   WHERE  nss = g_reg.nss
   ORDER BY 1

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
