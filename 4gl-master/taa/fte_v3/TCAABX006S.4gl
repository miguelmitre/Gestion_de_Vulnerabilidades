####################################################################
#  Esta  Funcion Arma los Querys para el Programa  TCAAB006L.4gl   #
#  Autor:  JOSE FRANCISCO LUGO CORNEJO                             #
#  Version: 090921   ELIMINA Fecha Redención y Bono de Pensión     #
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
            g_sql_09                     ,
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
            g_sql_55                     ,
            g_sql_56                     ,
            g_sql_57                     CHAR(1000)

   DEFINE   g_today                             ,
            g_finicta                           ,
            g_fecha_liq_parti                   ,
            g_fecha_saldo_parti                 ,
            g_fecha_trasp_desde                 DATE
   DEFINE
            g_enter                             CHAR(001),
            g_comando                           CHAR(100),
            g_raz_social                        CHAR(050),
            g_afore_cod                         CHAR(003),
            g_usuario                           CHAR(008),
            g_tabname                           CHAR(30)

  DEFINE    g_sie_inf       ARRAY[50]      OF    RECORD  --siefore
            nom_siefore                         CHAR(008),
            precio_accion                       DEC(25,14)
                                          END   RECORD
   DEFINE
            cero                                ,
            g_siefore                           ,
            g_scta                              ,
            g_scta_ini                          ,
            g_scta_fin                          ,
            g_siefore_ini                       ,
            g_siefore_fin                       ,
            g_recibido                          ,
            g_liquidado                         ,
            g_provisionado                      SMALLINT


   DEFINE   g_current                           DATETIME YEAR TO SECOND
   DEFINE   g_seg_modulo           RECORD  LIKE safre_af:seg_modulo.*
   DEFINE   reg_ctr_folio          RECORD  LIKE safre_af:taa_cd_ctr_folio.*

END GLOBALS

FUNCTION  arma_querys_TCAAB006S()
   LET      cero                     =  0
   SELECT   fecha_valuacion
     INTO   g_today
     FROM   safre_tmp:taa_cd_fecha_valuacion;
   IF       g_today                  <  "01/01/1900"   THEN
            LET      g_today         =  TODAY
   END IF
   LET      g_fecha_liq_parti        =
            MDY(MONTH(reg_ctr_folio.fecha_liquidacion), "01" ,
            YEAR(reg_ctr_folio.fecha_liquidacion))
   LET      g_fecha_saldo_parti      =  MDY(MONTH(g_today),"01",YEAR(g_today))
   LET      g_scta_ini               =  1
   LET      g_siefore_ini            =  1
   LET      g_current                =  CURRENT

   LET      g_sql_02                 =
            ' SELECT   g.subcuenta_grupo ',
            ' FROM     safre_af:taa_cd_subcuenta_grupo  g  ',
            ' WHERE    g.subcuenta         =  ?;  '

   LET      g_sql_03               =
            ' SELECT   a.estado ',
            ' FROM     safre_af:taa_cd_edo_cedente  a ',
            ' WHERE    a.descripcion              =  "PROVISIONADA" ',
            ' AND      a.tipo                     =  3  ; '

   LET      g_sql_05               =
            ' SELECT   a.* ',
            ' FROM     safre_af:taa_cd_ctr_folio a ',
            ' WHERE    a.estado                  =  ? ',
            ' AND      a.tipo_traspaso           =  1 ',
            ' AND      a.fecha_envio_saldos      =  ? ',
            ' ORDER BY 1  ;'

   LET      g_sql_06               =
            ' SELECT   COUNT(UNIQUE nss) ',
            ' FROM     safre_tmp:taa_cd_op_12  a ,',
            '          safre_af:taa_cd_det_cedido  b ',
            ' WHERE    a.estado                  =  101  ',
            '   AND    a.nss                     =  b.n_seguro ',
            '   AND    a.fecha_envio_saldos = "03/09/2012"',
            '   AND    b.folio in (20910,20939)',
            '   AND    b.estado                  =  103 ; '

   LET      g_sql_07                 =
            ' SELECT   a.codigo_siefore,b.razon_social,a.precio_del_dia ',
            ' FROM     glo_valor_accion a, tab_siefore_local b ',
            ' WHERE    a.fecha_valuacion           =  ?  ',
            ' AND      a.codigo_siefore        NOT IN(0) ',
            ' AND      b.codigo_afore              =  ?  ',
            ' AND      a.codigo_siefore            =  b.codigo_siefore  ;'

   LET      g_sql_07_2             =
            ' SELECT   g.codigo_siefore,g.precio_del_dia     ',
            ' FROM     glo_valor_accion  g ',
            ' WHERE    g.fecha_valuacion           =  ? ',
            ' AND      g.codigo_siefore           IN(11,12)  ; '

   LET      g_sql_08               =
            ' SELECT   monto_minimo ',
            ' FROM     safre_af:taa_cd_minimo_traspaso  ;'

   LET      g_sql_09               =
            ' SELECT   UNIQUE  nss ',
            ' FROM     safre_tmp:taa_cd_op_12  a ,',
            '          safre_af:taa_cd_det_cedido  b ',
            ' WHERE    a.estado                  =  101  ',
            '   AND    a.fecha_envio_saldos = "03/09/2012"',  #fecha y folio fijos --> ACS
            '   AND    b.folio in (20910,20939)',
            '   AND    a.nss                     =  b.n_seguro ',
            '   AND    b.estado                  =  103 ; '



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
 
   LET      g_sql_14               =
            ' SELECT   UNIQUE  a.*,c.tipo_solicitud,c.finicta,c.n_unico  ',
            ' FROM     safre_af:taa_cd_det_cedido   a ,',
            '          safre_af:afi_mae_afiliado    c  ',
            ' WHERE    a.n_seguro                   =  ?     ',
            ' AND      a.folio                      =  ?   ',
            ' AND      a.estado                     =  103 ',
            ' AND      a.n_seguro                   =  c.n_seguro; '
 
   LET      g_sql_15               =
            ' INSERT   INTO   safre_af:taa_cd_ctr_folio ',
              ' VALUES  (?,?,?,?,?,?,?,?)  ;'

   LET      g_sql_16               =
            ' INSERT   INTO   safre_tmp:taa_cd_sum_arch ',
                     ' VALUES  (?,?,?,?)  ;'
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
            ' AND     subcuenta                    =  ? ' ,
            ' AND      tipo_movimiento             =  1  ;'

   LET      g_sql_24                 =
            ' INSERT   INTO  taa_cd_fechas_vol ' ,
            ' SELECT   490,MAX(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ? ',
            ' AND      subcuenta                   =  ? ' ,
            ' AND      tipo_movimiento             =  490  ;'

   LET      g_sql_25_110                 =
            ' INSERT   INTO taa_cd_fechas_vol_110 ',
            ' SELECT   1,MIN(fecha_conversion) ',
            ' FROM ',  g_tabname CLIPPED ,' ' ,
            ' WHERE    nss                         =  ? ',
            ' AND      subcuenta                   =  10 ',
            ' AND      tipo_movimiento             =  110  ;'

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
            ' WHERE    codigo_siefore        NOT  IN(0)   ;   '

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
            ' AND      a.subcuenta                 =  ? ' ,
            ' AND      a.tipo_movimiento           =  1 ' ,
            ' AND     (folio  IN(SELECT  folio                       ' ,
            '                      FROM  dis_dep_aporte              ' ,
            '                     WHERE  ident_pago[14,15]   =  44   ' ,
            '                       AND  estado              =  2)   ' ,
            '  OR      folio  IN(SELECT  folio                       ' ,
            '                      FROM  dis_dep_issste              ' ,
            '                     WHERE  ident_pago[14,15] IN(45,46) ' ,
            '                       AND  estado              =  2)); '

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

   LET      g_sql_55                  =
            ' SELECT   COUNT(*) ' ,
            '   FROM   safre_af:taa_cd_ctr_folio ',
            '  WHERE   fecha_envio_saldos         =  ? ',
            '    AND   tipo_traspaso             IN(1,3,4) ',
            '    AND   fecha_envio_saldos  BETWEEN  TODAY -4  AND  TODAY + 5',
            '    AND   estado                     =  102 ; '

   LET      g_sql_56                  =
            ' SELECT   MAX(A.fecha_vol_pat) ',
            ' FROM     safre_af:taa_rcv_recepcion  A ',
            ' WHERE    A.nss                      =  ?  ;'

   LET      g_sql_57                  =
            ' SELECT   MAX(B.fecha_vol_ven) ',
            ' FROM     safre_af:taa_rcv_recepcion  B ',
            ' WHERE    B.nss                      =  ?  ;'
END FUNCTION

FUNCTION  F_920_trae_parametros()
   PREPARE  sql_03          FROM  g_sql_03
   EXECUTE  sql_03          INTO  g_provisionado
   PREPARE  sql_07          FROM  g_sql_07
   PREPARE  sql_07_2        FROM  g_sql_07_2
   PREPARE  sql_10          FROM  g_sql_10
   EXECUTE  sql_10          INTO  g_seg_modulo.*
   PREPARE  sql_13          FROM  g_sql_13
   EXECUTE  sql_13          INTO  g_afore_cod,g_raz_social,g_usuario
   PREPARE  sql_38          FROM  g_sql_38
   EXECUTE  sql_38          INTO  g_siefore_fin
END FUNCTION


FUNCTION    F_930_arma_precios_acc_parti()
   DEFINE   l_pos                             ,
            l_sin_precio                      SMALLINT,
            l_nom_siefore                     CHAR(008),
            l_precio_accion                   DEC(19,14)
   LET      l_sin_precio                =  0
   FOR      g_siefore                   =  g_siefore_ini      TO  g_siefore_fin
            LET      g_sie_inf[g_siefore].nom_siefore     =  " "
            LET      g_sie_inf[g_siefore].precio_accion   =  0
   END FOR
   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie      USING    g_today ,g_afore_cod
   FOREACH  cur_sie       INTO    g_siefore,l_nom_siefore,l_precio_accion
            FOR      l_pos    =  1          TO   8
                     IF       l_nom_siefore[l_pos]        =  " "     THEN
                              LET      l_nom_siefore[l_pos]      =  "?"
                     END IF
            END FOR
            LET      g_sie_inf[g_siefore].nom_siefore     =  l_nom_siefore
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH
   DECLARE  cur_sie_viv      CURSOR   FOR   sql_07_2
   OPEN     cur_sie_viv       USING   g_fecha_liq_parti
   FOREACH  cur_sie_viv        INTO   g_siefore,l_precio_accion
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH
   IF      INT_FLAG                 =  FALSE     THEN
           DISPLAY " ** FECHAS DE VALUACION(dd/mm/aa)BASICAS:",
                   g_today USING "dd/mm/yyyy",
           " PARTI-AIVS.:",g_fecha_liq_parti  USING "dd/mm/yyyy"," **  "
            AT  12,1   ATTRIBUTE(REVERSE)
           DISPLAY  g_sie_inf[1].precio_accion      TO  g_precio_accion_b1
           DISPLAY  g_sie_inf[2].precio_accion      TO  g_precio_accion_b2
           DISPLAY  g_sie_inf[3].precio_accion      TO  g_precio_accion_b3
           DISPLAY  g_sie_inf[4].precio_accion      TO  g_precio_accion_b4
           DISPLAY  g_sie_inf[5].precio_accion      TO  g_precio_accion_b5
           DISPLAY  g_sie_inf[6].precio_accion      TO  g_precio_accion_b6
           DISPLAY  g_sie_inf[11].precio_accion     TO  precio_parti
           DISPLAY  g_sie_inf[12].precio_accion     TO  precio_aivs
           DISPLAY  g_sie_inf[13].precio_accion     TO  precio_udis
   END IF
   DECLARE  cur_checa_sie    CURSOR   FOR
   SELECT   codigo_siefore
     FROM   tab_siefore_local
    WHERE   codigo_siefore        <>  0
    ORDER   BY  1
   FOREACH  cur_checa_sie       INTO  g_siefore
       IF       g_sie_inf[g_siefore].precio_accion     IS  NULL   OR
                g_sie_inf[g_siefore].precio_accion      =  0      THEN
                ERROR   " NO HAY PRECIO DE ACCION PARA LA SIEFORE: ",
                         g_siefore    USING   "###"
                PROMPT  " TECLEE <Enter> PARA SALIR..."   FOR  g_enter
                EXIT  PROGRAM
       END IF
   END FOREACH
   IF       INT_FLAG                 =  FALSE     THEN
            PROMPT  '  VERIFIQUE FECHAS Y PRECIOS DE ACCION <Enter> PARA CONTINUAR ?...'
            FOR   g_enter
   END IF
   LET      g_sie_inf[10].nom_siefore     =  "MASBAS0?"
   LET      g_sie_inf[10].precio_accion   =  1
END FUNCTION

