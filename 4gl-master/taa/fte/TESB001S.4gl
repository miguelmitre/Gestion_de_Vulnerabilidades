DATABASE  safre_af
GLOBALS
   DEFINE   g_sql_01                  , #
            g_sql_02                  , #
            g_sql_03                  , #
            g_sql_04                  , #
            g_sql_05                  , #
            g_sql_06                  , #
            g_sql_07                  , #
            g_sql_08                  , #
            g_sql_10                  , #
            g_sql_12                  , #
            g_sql_13                  , #
            g_sql_14                  , #
            g_sql_15                  , #
            g_sql_31                  , #
            g_sql_32                  , #
            g_sql_42                  , #
            g_sql_42_3                , #
            g_sql_43                  , #
            g_sql_44                  , #
            g_sql_53                  , #
            g_sql_54                  , #
            g_sql_55                  , #
            g_sql_56                  , #
            g_sql_57                 CHAR(1000) #
   DEFINE   g_tabname                 CHAR(30)
   DEFINE   l_part_viv97              DEC(16,6)

END GLOBALS

FUNCTION  define_querys_TCAAB005()

   LET      g_sql_01  =                                      
           'SELECT MAX(a.folio)+1 ',
           'FROM   safre_af:glo_folio a '

   LET      g_sql_02  =
           'INSERT INTO safre_af:glo_folio ' ,              
           'VALUES (?) '

   LET      g_sql_03  =
           'SELECT  a.estado ',
           'FROM    safre_af:taa_cd_edo_cedente  a ',
           'WHERE   a.descripcion = "PROCEDENTE" ',
           'AND     a.tipo           = 2; '

   LET      g_sql_04  =
           'SELECT  a.estado ',
           'FROM    safre_af:taa_cd_edo_cedente  a ',
           'WHERE   a.descripcion    = "PROVISIONADA" ',
           'AND     a.tipo           = 3; ' 

   LET      g_sql_05  =
           'SELECT  a.estado ',
           'FROM    safre_af:taa_cd_edo_cedente  a ',
           'WHERE   a.descripcion = "RECIBIDO" ',
           'AND     a.tipo           = 3; '

   LET      g_sql_06  =
            'SELECT  COUNT(unique a.nss)   ',
            'FROM    tes_solicitud a       ',
            'WHERE   a.estado         =  ? ', 
            'AND     a.tipo_traspaso  =  ? '

   LET      g_sql_07  =
            'SELECT  a.codigo_siefore,b.razon_social,a.precio_del_dia ',
            'FROM    glo_valor_accion a, tab_siefore_local b ',
            'WHERE   a.fecha_valuacion    =  ?  ',
            'AND     a.codigo_siefore   NOT IN(0,11) ', --mod multisiefore
            'AND     b.codigo_afore      =  ?  ',
            'AND     a.codigo_siefore    =  b.codigo_siefore'

   LET      g_sql_08  =
            'SELECT a.*                     ',
            'FROM   tes_tipo_id_aportante a ',
            'WHERE  a.tipo_traspaso = ?     '

   LET      g_sql_10  =
            'SELECT  * ',
            'FROM    seg_modulo ',
            'WHERE   modulo_cod  =  "taa" '
  
   LET      g_sql_12  =    --SE QUITA UNIQUE POR GRUPO SCTA
            'SELECT  b.* ',
            'FROM    safre_af:tes_solicitud b ',
            'WHERE   b.folio       =  ? ',
            'AND     b.estado      =  ? ',
            'ORDER BY b.nss,b.grupo_regimen '

   LET      g_sql_13  =
            'SELECT codigo_afore,razon_social,user ',
            'FROM tab_afore_local '

   LET      g_sql_14  =
            'UPDATE  tes_solicitud ',
            'SET     estado    =  ? ',
            'WHERE   folio     =  ? ',
            'AND     estado    =  ? '

   LET      g_sql_15  =
            'UPDATE  tes_ctr_folio ',
            'SET     estado   =  ? ',
            'WHERE   folio    =  ? ',
            'AND     estado   =  ? '
             
   LET      g_sql_31 = 
            'INSERT INTO safre_af:tes_ctr_folio ',
            'VALUES (?,?,?,?,?,?,?) '

   LET      g_sql_32  =
            'UPDATE  tes_solicitud   ',
            'SET     folio     =  ? ,',
            '        estado    =  ?  ',
            'WHERE   estado    =  ?  ',
            'AND     tipo_traspaso = ? '

   LET      g_sql_42   = 
            'EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) '

   LET      g_sql_42_3 = 
            ' SELECT a.subcuenta         ,  ',
            '        a.siefore           ,  ',
            '        sum(a.monto_en_acciones) ,  ',
            '        sum(a.monto_en_pesos)       ',
            ' FROM   dis_cuenta a           ',
            ' WHERE  a.folio            = ? ',
            ' AND    a.nss              = ? ',
            ' AND    a.fecha_conversion = ? ',
            ' GROUP BY 1,2 '

   LET      g_sql_43  =
            'EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) '

   LET      g_sql_44  =
            'EXECUTE FUNCTION fn_prov_abono(?,?,?,?,?,?,?,?,?,?) '

   LET     g_sql_53   = 
	    ' SELECT COUNT(*) ' ,
	    ' FROM   safre_af:dis_provision ',
	    ' WHERE  folio = ? '

   LET     g_sql_54   = 
           ' SELECT a.precio_del_dia ' ,
           ' FROM   safre_af:glo_valor_accion a ' ,
           ' WHERE  a.codigo_siefore  = ? ',
           ' AND    a.fecha_valuacion = ? '

   LET     g_sql_55   = --SE MODIFICA PARA GRUPO MULTISIE
           ' SELECT a.codigo_siefore           ' , 
           ' FROM   safre_af:cta_nss_regimen a ' ,
           ' WHERE  a.nss           = ?        ' ,
           ' AND    a.grupo_regimen = ?        '

   LET     g_sql_56   = 
           ' SELECT a.*                     ', 
           ' FROM   tes_tipo_id_aportante a ',
           ' ORDER by a.orden_transferencia '

   LET     g_sql_57   = 
           ' SELECT a.*             ',
           ' FROM   tes_ctr_folio a ',
           ' WHERE  a.estado = ?    ',
           ' ORDER BY a.folio       '

END FUNCTION
