####################################################################
#  Esta  Funcion Arma los Querys para el Programa  TCAAB006L.4gl   #
#  Autor:  JOSE FRANCISCO LUGO CORNEJO                             #
#  Version: 090921   ELIMINA Fecha Redención y Bono de Pensión     #
####################################################################
#Modificacion: Se agrega validacion al query 14 para traspasos     #
#              complementarios. 14/Sept/2011 -->Alejandro Chagoya  #
####################################################################
#Modificacion: Se agrega validacion para ignorar tipo de traspaso 52
#              Reasignacion 18-Mayo-2012     -->Alejandro Chagoya  #
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
            g_sql_11_110                 ,
            g_sql_12                     ,
            g_sql_13                     ,
            g_sql_14                     ,
            g_sql_15                     ,
            g_sql_16                     ,
            g_sql_17                     ,
            g_sql_18                     ,
            g_sql_19                     ,
            g_sql_20                     ,
            g_sql_21                     ,
            g_sql_22                     ,
            g_sql_23                     ,
            g_sql_24                     ,
            g_sql_25                     ,
            g_sql_25_110                 ,
            g_sql_26                     ,
            g_sql_27                     ,
            g_sql_28                     ,
# libre     g_sql_29                     ,
            g_sql_30                     ,
            g_sql_31                     ,
            g_sql_32                     ,
            g_sql_32_110                 ,
            g_sql_33                     ,
            g_sql_34                     ,
            g_sql_35                     ,
            g_sql_36                     ,
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
            g_sql_51                     ,
            g_sql_52                     ,
            g_sql_53                     ,
            g_sql_54                     ,
            g_sql_56                     ,
            g_sql_57                     CHAR(1000)

   DEFINE   g_today                      DATE,
            g_fecha_trasp_desde          DATE,
            g_tabname                    CHAR(30),
            l_part_viv97                 DEC(16,6)

END GLOBALS

FUNCTION  arma_querys_TCAAB017S()

   LET      g_sql_02                 =
            ' SELECT   g.subcuenta_grupo ',
            ' FROM     safre_af:taa_cd_subcuenta_grupo  g  ',
            ' WHERE    g.subcuenta         =  ?;  '

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
            ' WHERE    a.estado                  =  103 ',
            ' AND      a.tipo_traspaso != 52      ', #ACS 18-mayo-2012
            ' AND      a.fecha_trasp            >=  ?  ;'

   LET      g_sql_07                 =
            ' SELECT   a.codigo_siefore,a.precio_del_dia ',
            ' FROM     glo_valor_accion a, tab_siefore_local b ',
            ' WHERE    a.fecha_valuacion           =  ?  ',
            ' AND      a.codigo_siefore        NOT IN(0) ',
            ' AND      a.codigo_siefore            =  b.codigo_siefore  ;'

   LET      g_sql_07_2             =
            ' SELECT   g.codigo_siefore,g.precio_del_dia     ',
            ' FROM     glo_valor_accion  g ',
            ' WHERE    g.fecha_valuacion           =  ? ',
            ' AND      g.codigo_siefore           IN(11,12)  ; '

   LET      g_sql_08               =
            ' SELECT   monto_minimo ',
            ' FROM     safre_af:taa_cd_minimo_traspaso  ;'

   LET      g_sql_10               =
            ' SELECT   * ',
            ' FROM     safre_af:seg_modulo ',
            ' WHERE    modulo_cod                =  "taa"  ;'

   LET      g_sql_12               =
            ' SELECT   SUM(monto_en_acciones)              ',
            ' FROM     safre_af:dis_cuenta     dc ,        ',
            '          safre_af:taa_cd_subcuenta_grupo  gp  ',
            ' WHERE    dc.nss                        =  ?  ',
            ' AND      dc.fecha_conversion          <=  ?  ',
            ' AND      gp.subcuenta_grupo            =  ?  ',
            ' AND      dc.subcuenta                  =  gp.subcuenta',
            ' AND      dc.siefore                    =  ?  ;'

   LET      g_sql_13               =
            ' SELECT   codigo_afore, razon_social, user ',
            ' FROM     safre_af:tab_afore_local ; '
--Se agrega validacion de marca 120_ACS-Sep2011
   LET      g_sql_14               =
            ' SELECT   UNIQUE  a.*  ',
            ' FROM     safre_af:taa_cd_det_cedido  a ,',
            '          safre_af:cta_act_marca      b  ',
            ' WHERE    a.fecha_trasp               >=  ?    ',
            ' AND      a.estado                     =  103 ',
            ' AND      a.n_seguro                   =  b.nss ',
            ' AND      a.tipo_traspaso != 52      ', #ACS 18-mayo-2012
            ' AND      b.marca_cod            NOT  IN(237) ',
            ' AND      b.marca_cod                  =  120 ',        --Inhabilitado ACS-Sep2011
            ' ORDER    BY  a.n_seguro,a.fecha_trasp  DESC  ;'

   IF       g_fecha_trasp_desde            =   g_today      THEN
            LET    g_sql_14                =
            ' SELECT   UNIQUE  b.*  ',
            '   FROM   safre_af:taa_cd_indebidos    a    ,',
            '          safre_af:taa_cd_det_cedido   b    ,',
            '          safre_af:cta_act_marca       c     ',
            '  WHERE   a.fecha_liquidacion          =  ?      ',
            '    AND   a.estado                     =  103        ',
            '    AND   a.nss                        =  b.n_seguro ',            
            '    AND   b.n_seguro                   =  c.nss      ',
            '    AND   b.tipo_traspaso != 52      ', #ACS 18-mayo-2012
            '    AND   b.estado                     =  103        ',
            '    AND   c.marca_cod                  =  120        ',        --Inhabilitado ACS-Sep2011
            ' ORDER    BY  b.n_seguro                      ;'
   END IF

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

   LET      g_sql_20                 =
            ' SELECT   min(fecha_ini) ',
            ' FROM     cta_act_marca ',
            ' WHERE    nss                          =  ? ',
            ' AND      marca_cod                    =  231 ; '

   LET      g_sql_21               =
            ' SELECT   COUNT(*) ',
            ' FROM     safre_tmp:taa_cd_op_12 ',
            ' WHERE    estado                     =  101 ',
            ' AND      fecha_envio_saldos        >=  ? '
       
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

   LET      g_sql_35                =
            ' SELECT   * ',
            ' FROM     safre_af:tab_subcuenta   ',
            ' WHERE    subct_cod    >  0      ; '

   LET      g_sql_38                  =
            ' SELECT   MAX(codigo_siefore)                    ',
            ' FROM     safre_af:tab_siefore_local             ',
            ' WHERE    codigo_siefore        NOT  IN(0)   ;   '

   LET      g_sql_42                = 
            ' EXECUTE  FUNCTION  fn_saldo_dia (?,?,?,?)  ;'

   LET      g_sql_45                =
            ' SELECT   sum(a.monto_en_acciones) ' ,
            ' FROM     dis_cuenta a ' ,
            ' WHERE    a.nss                       =  ? ' ,
            ' AND      a.subcuenta                 =  ? ' ,
            ' AND      a.tipo_movimiento           =  1 ' ,
            ' AND     (folio  IN(SELECT  folio                       ' ,
            '                      FROM  dis_dep_aporte              ' ,
            '                     WHERE  ident_pago[14,15]   =  44   ' ,
            '                       AND  estado              =  2)   ' ,
            '  OR      folio  IN(SELECT  folio                       ' ,
            '                      FROM  dis_dep_issste              ' ,
            '                     WHERE  ident_pago[14,15]   =  44   ' ,
            '                       AND  estado              =  2)); '

   LET      g_sql_47                =
            ' SELECT   a.razon_social ' ,
            ' FROM     tab_siefore_local a ' ,
            ' WHERE    a.codigo_afore             =  ? ' ,
            ' AND      a.codigo_siefore           =  ?  ;'

   LET      g_sql_51                =
            ' SELECT   1               ',
	    ' FROM     safre_af:cta_act_marca  MA ' ,
	    ' WHERE    MA.nss                 =  ?  ',
            ' AND      MA.marca_cod           =  120     ;'

   LET      g_sql_52                =
            ' SELECT   UNIQUE  1                       ',
	    ' FROM     safre_af:cta_his_marca  hma     ' ,
	    ' WHERE    hma.nss                 =  ?     ',
            ' AND      hma.marca_cod           =  120   ',
            ' AND      hma.fecha_ini           =  ?     ;'

   LET      g_sql_54                =
	    ' SELECT   CASE  WHEN  a.ind_transferencia  =  1  THEN  "1" ',
	    '                ELSE   " " ',
	    '          END   CASE ',
	    ' FROM     cta_ctr_cuenta  a ',
	    ' WHERE    a.nss                      =  ?  ;'

END FUNCTION
