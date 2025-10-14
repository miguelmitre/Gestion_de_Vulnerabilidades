####################################################################
#  Esta  Funcion Arma los Querys para el Programa  TCAAB006L.4gl   #
#  Autor:  JOSE FRANCISCO LUGO CORNEJO                             #
####################################################################
DATABASE  safre_af
GLOBALS
   DEFINE   g_sql_01                     ,
            g_sql_02                     ,
            g_sql_03                     ,
            g_sql_04                     ,
            g_sql_05                     ,
            g_sql_06                     ,
            g_sql_07                     ,
            g_sql_07_2                   ,
            g_sql_08                     ,
            g_sql_10                     ,
            g_sql_11                     ,
            g_sql_12                     ,
            g_sql_12_1                   ,
            g_sql_12_2                   ,
            g_sql_12_3                   ,
            g_sql_12_4                   ,
            g_sql_11_110                 ,
            g_sql_13                     ,
            g_sql_14                     ,
            g_sql_15                     ,
            g_sql_16                     ,
            g_sql_17                     ,
            g_sql_18                     ,
            g_sql_19                     ,
            g_sql_21                     ,
            g_sql_22                     ,
            g_sql_23                     ,
            g_sql_24                     ,
            g_sql_25                     ,
            g_sql_25_110                 ,
            g_sql_26                     ,
            g_sql_27                     ,
            g_sql_28                     ,
            g_sql_29                     ,
            g_sql_30                     ,
            g_sql_31                     ,
            g_sql_32                     ,
            g_sql_32_110                 ,
            g_sql_33                     ,
            g_sql_34                     ,
            g_sql_35                     ,
            g_sql_36                     ,
            g_sql_37                     ,
            g_sql_38                     ,
            g_sql_39                     ,
            g_sql_40                     ,
            g_sql_41                     ,
            g_sql_42                     ,
            g_sql_43                     ,
            g_sql_44                     ,
            g_sql_45                     ,
            g_sql_47                     ,
            g_sql_48                     ,
            g_sql_50_1                   ,
            g_sql_54                     ,
            g_sql_56                     ,
            g_sql_57                     CHAR(1000)

   DEFINE   g_tabname                    CHAR(30)
   DEFINE   l_part_viv97                 DEC(16,6)

END GLOBALS

FUNCTION  arma_querys_TCAAB006S()
   LET      g_sql_03               =
            ' SELECT   a.estado ',
            ' FROM     safre_af:taa_cd_edo_cedente  a ',
            ' WHERE    a.descripcion              =  "PROVISIONADA" ',
            ' AND      a.tipo                     =  3  ; '

   LET      g_sql_04               =
            ' SELECT   a.estado ',
            ' FROM     safre_af:taa_cd_edo_cedente  a ',
            ' WHERE    a.descripcion              =  "LIQUIDADA" ',
            ' AND      a.tipo                     =  3  ; ' 

   LET      g_sql_05               =
            ' SELECT   a.* ',
            ' FROM     safre_af:taa_cd_ctr_folio a ',
            ' WHERE    a.estado                  =  ? ',
            ' AND      a.tipo_traspaso           =  1 ',
            ' AND      a.fecha_envio_saldos      =  ? ',
            ' ORDER BY 1  ;'

   LET      g_sql_06               =
            ' SELECT   COUNT(*) ',
            ' FROM     safre_af:taa_cd_det_cedido a ',
            ' WHERE    a.estado                  =  ? ',
            ' AND      a.fecha_trasp            >=  ?  ;'

   LET      g_sql_07                 =
            ' SELECT   a.codigo_siefore,b.razon_social,a.precio_del_dia ',
            ' FROM     glo_valor_accion a, tab_siefore_local b ',
            ' WHERE    a.fecha_valuacion           =  ?  ',
            ' AND      a.codigo_siefore        NOT IN(0,11) ',
            ' AND      b.codigo_afore              =  ?  ',
            ' AND      a.codigo_siefore            =  b.codigo_siefore  ;'

   LET      g_sql_07_2             =
            ' SELECT   precio_del_dia     ',
            ' FROM     glo_valor_accion   ',
            ' WHERE    fecha_valuacion           =  ? ',
            ' AND      codigo_siefore            =  ?  ;'

   LET      g_sql_08               =
            ' SELECT   monto_minimo ',
            ' FROM     safre_af:taa_cd_minimo_traspaso  ;'

   LET      g_sql_10               =
            ' SELECT   * ',
            ' FROM     safre_af:seg_modulo ',
            ' WHERE    modulo_cod                =  "taa"  ;'

   LET      g_sql_11               =
            ' CREATE   TEMP  TABLE   taa_cd_fechas_vol ',
 	    ' (mov                   SMALLINT, ',
	    ' fecha                  DATE)     ;'

   LET      g_sql_11_110           =
            ' CREATE   TEMP  TABLE   taa_cd_fechas_vol_110 ',
 	    ' (mov                   SMALLINT, ',
	    ' fecha                  DATE)  ;'

   LET      g_sql_12               =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc          ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      dc.subcuenta            IN (2,6,9)  ',
            ' AND      dc.siefore                    =  ?  ;' 

   LET      g_sql_12_1             =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc          ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      dc.subcuenta            IN (3,22)  ',
            ' AND      dc.siefore                    =  ?  ;'

   LET      g_sql_12_2             =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc          ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      dc.subcuenta            IN (10,23)  ',
            ' AND      dc.siefore                    =  ?  ;'

   LET      g_sql_12_3             =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc          ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      dc.subcuenta            IN (11,24)  ',
            ' AND      dc.siefore                    =  ?  ;'

   LET      g_sql_12_4             =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc          ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      dc.subcuenta            IN (12,25)  ',
            ' AND      dc.siefore                    =  ?  ;'

   LET      g_sql_13               =
            ' SELECT   codigo_afore, razon_social, user ',
            ' FROM     safre_af:tab_afore_local ; '

   LET      g_sql_14               =
            ' SELECT   UNIQUE  a.*,c.tipo_solicitud,c.finicta   ',
            ' FROM     safre_af:taa_cd_det_cedido a ,',
            '          safre_af:afi_mae_afiliado  c ,',
            ' OUTER    safre_af:cta_act_marca b ',
            ' WHERE    a.estado                     =  103 ',
            ' AND      a.fecha_trasp               >=  ? ',
            ' AND      a.n_seguro                   =  b.nss ',
            ' AND      b.marca_cod            NOT  IN(237) ',
            ' AND      a.n_seguro                   =  c.n_seguro ',
            ' ORDER    BY  n_seguro  ;'

   LET      g_sql_15               =
            ' INSERT   INTO   safre_af:taa_cd_ctr_folio ',
              ' VALUES  (?,?,?,?,?,?,?,?)  ;'

   LET      g_sql_16               =
            ' INSERT   INTO   safre_tmp:taa_cd_sum_arch ',
                     ' VALUES  (?,?,?,?)  ;'
             
   LET      g_sql_17               =
            ' SELECT   "1" ',
            ' FROM     safre_af:cta_act_marca A ',
            ' WHERE    A.nss                      =  ? ',
            ' AND      A.marca_cod                =  610',
            ' GROUP    BY  1  ;'

   LET      g_sql_18               =
            ' SELECT   MIN(fecha_ini) ',
            ' FROM     safre_af:cta_act_marca ',
            ' WHERE    nss                        =  ? ',
            ' AND      marca_cod                  =  237  ;'

   LET      g_sql_19               =
            ' SELECT   "X" ',
            ' FROM     safre_af:cta_act_marca ',
            ' WHERE    nss                 =  ? ',
            ' AND      marca_cod                   =  230 ',
            ' GROUP    BY  1  ;'

   LET      g_sql_21               =
            ' SELECT   COUNT(*) ',
            ' FROM     safre_af:taa_cd_ctr_folio ',
            ' WHERE    tipo_traspaso              =  2 ',
            ' AND      estado                     =  ?  ;'
       
   LET      g_sql_22               =
            ' SELECT   COUNT(*) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                        =  ? ',
            ' AND      subcuenta                  =  4 ',
            ' AND      tipo_movimiento            =  1  ;'

   LET      g_sql_23               =
            ' INSERT  INTO   taa_cd_fechas_vol ' ,
            ' SELECT  1,MIN(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE   nss                          =  ? ',
            ' AND     subcuenta                    =  3 ' ,
            ' AND      tipo_movimiento             =  1  ;'

   LET      g_sql_24                 =
            ' INSERT   INTO  taa_cd_fechas_vol ' ,
            ' SELECT   490,MAX(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ? ',
            ' AND      subcuenta                   =  3 ' ,
            ' AND      tipo_movimiento             =  490  ;'

   LET      g_sql_25                 =
            ' INSERT   INTO taa_cd_fechas_vol ',
            ' SELECT   1,MIN(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ? ',
            ' AND      subcuenta                   =  10 ',
            ' AND      tipo_movimiento             =  1 ;'

   LET      g_sql_26                 =
            ' INSERT   INTO taa_cd_fechas_vol ',
            ' SELECT   490,MAX(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ?  ',
            ' AND      subcuenta                   =  10 ',
            ' AND      tipo_movimiento             =  490 ;'

   LET      g_sql_25_110                 =
            ' INSERT   INTO taa_cd_fechas_vol_110 ',
            ' SELECT   1,MIN(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ? ',
            ' AND      subcuenta                   =  10 ',
            ' AND      tipo_movimiento             =  110  ;'

   LET      g_sql_27               =
            ' SELECT   MAX(sct.fecha_valor) ',
            ' FROM     cta_saldo_fov  sct ' ,   #cambiar en Afore
            ' WHERE    sct.nss                      =  ? ',
            ' AND      sct.subcuenta                =  ?  ;'

   LET      g_sql_28                =
            ' SELECT  cta.fecha_valor,         ',
            '         cta.monto_en_pesos       ',
            ' FROM    dis_cuenta   cta         ',
            ' WHERE   cta.fecha_conversion          >=  ? ',
            ' AND     cta.fecha_valor               <=  ? ',
            ' AND     cta.subcuenta                  =  ? ',
            ' AND     cta.tipo_movimiento           <>  3 ',
            ' AND     cta.nss                        =  ? ',
            ' UNION   ALL                      ',
            ' SELECT  ctav.fecha_valor,        ',
            '         ctav.monto_en_pesos      ',
            ' FROM    cta_saldo_fov ctav        ',
            ' WHERE   ctav.nss                       =  ? ',
            ' AND     ctav.subcuenta                 =  ?; '

   LET      g_sql_29                =
            ' SELECT   tasa_valor ',
            ' FROM     tab_tasa_remanente ',
            ' WHERE    tasa_fecha                  =  ? ',
            ' AND      tasa_origen                 =  "FOV"  ;'
 
   LET      g_sql_31                =
            ' SELECT   MAX(A.fecha) ',
            ' FROM     safre_af:taa_cd_fechas_vol A ',
            ' WHERE    A.mov                      =  490  ;'

   LET      g_sql_32                =
            ' SELECT   MIN(A.fecha) ',
            ' FROM     safre_af:taa_cd_fechas_vol A ',
            ' WHERE    A.mov                       =  1  ;'

   LET      g_sql_32_110            =
            ' SELECT   MIN(A.fecha) ',
            ' FROM     taa_cd_fechas_vol_110 A ',
            ' WHERE    A.mov                       =  1  ;'

   LET      g_sql_33                =
            ' INSERT   INTO  safre_af:dis_provision ',
                   ' VALUES   (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ;'

   LET      g_sql_34                =
            ' INSERT   INTO safre_tmp:taa_cd_saldos_arch ',
                   ' VALUES   (?,?,?,?,?)  ;'

   LET      g_sql_35                =
            ' SELECT   * ',
            ' FROM     safre_af:tab_subcuenta   ',
            ' WHERE    subct_cod    >  0      ; '

   LET      g_sql_36                =
            ' INSERT   INTO  safre_af:taa_cd_det_comple ',
                   ' VALUES   (?,?,?,?,?,?)  ;'

   LET      g_sql_37                  =
            ' SELECT   MAX(subct_cod)              ',
            ' FROM     safre_af:tab_subcuenta               ; '

   LET      g_sql_38                  =
            ' SELECT   MAX(codigo_siefore)                    ',
            ' FROM     safre_af:tab_siefore_local             ',
            ' WHERE    codigo_siefore        NOT  IN(0,11)   ;   '

   LET      g_sql_39                =
            ' UPDATE   safre_af:taa_cd_ctr_folio ',
            ' SET      calculo_interes             =  1 ',
            ' WHERE    folio                       =  ?  ;'

   LET      g_sql_40                =
            ' CREATE   INDEX  fechtaab005  ON taa_cd_fechas_vol(fecha,mov) ;'

   LET      g_sql_41                =
            ' SELECT   nombre_tabla ',
            ' FROM     safre_af:taa_cd_tab_cuenta  ;'

   LET      g_sql_42                = 
            ' EXECUTE  FUNCTION  fn_saldo_dia (?,?,?,?)  ;'


   LET      g_sql_43                =
            ' EXECUTE  FUNCTION  fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?)  ;'

   LET      g_sql_45                =
            ' SELECT   sum(a.monto_en_acciones) ' ,
            ' FROM     dis_cuenta a ' ,
            ' WHERE    a.nss                       =  ? ' ,
            ' AND      a.subcuenta                 =  4 ' ,
            ' AND      a.tipo_movimiento           =  1 ' ,
            ' AND      a.fecha_conversion         >=  ?  ;'

   LET      g_sql_47                =
            ' SELECT   a.razon_social ' ,
            ' FROM     tab_siefore_local a ' ,
            ' WHERE    a.codigo_afore             =  ? ' ,
            ' AND      a.codigo_siefore           =  ?  ;'

   LET      g_sql_48                =              
            ' DELETE   FROM   taa_cd_fechas_vol  ;'
 
   LET      g_sql_50_1              =
            ' SELECT   A.marca_cod   , ',
	    '          A.id_aportante  ',
	    ' FROM     safre_af:taa_cd_tipo_traspaso A ' ,
	    ' WHERE    A.tipo_traspaso            =  ?  ;'

   LET      g_sql_54                =
	    ' SELECT   CASE  WHEN  a.ind_transferencia  =  1  THEN  "1" ',
	    '                ELSE   " " ',
	    '          END   CASE ',
	    ' FROM     cta_ctr_cuenta  a ',
	    ' WHERE    a.nss                      =  ?  ;'

   LET      g_sql_56                  =
            ' SELECT   MAX(A.fecha_vol_pat) ',
            ' FROM     safre_af:taa_rcv_recepcion  A ',
            ' WHERE    A.nss                      =  ?  ;'

   LET      g_sql_57                  =
            ' SELECT   MAX(B.fecha_vol_ven) ',
            ' FROM     safre_af:taa_rcv_recepcion  B ',
            ' WHERE    B.nss                      =  ?  ;'
END FUNCTION
