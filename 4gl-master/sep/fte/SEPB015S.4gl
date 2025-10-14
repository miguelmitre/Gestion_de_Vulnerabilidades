DATABASE safre_af
GLOBALS
DEFINE g_sql_01                , #
       g_sql_02                , #
       g_sql_03                , #
       g_sql_03_1              , #
       g_sql_03_sar            , #
       g_sql_04                , #
       g_sql_05                , #
       g_sql_07                , #
       g_sql_08                , #
       g_sql_09                , #
       g_sql_09_1              , #
       g_sql_10                , #
       g_sql_11                , #
       g_sql_11_1              , #
       g_sql_12                , #
       g_sql_13                , #
       g_sql_13_x              , #
       g_sql_13_b              , #
       g_sql_13_c              , #
       g_sql_13_3              , #
       g_sql_14                , #
       g_sql_15                , #
       g_sql_16                , #
       g_sql_17                , #
       g_sql_17_1              , #
       g_sql_17_2              , #
       g_sql_18                , #
       g_sql_18_sar            , #
       g_sql_18_1              , #
       g_sql_19                , #
       g_sql_19_1              , #
       g_sql_19_sar            , #
       g_sql_21                , #
       g_sql_22                , #
       g_sql_22_1              , #
       g_sql_22_2              , #
       g_sql_22_3              , #
       g_sql_23                , #
       g_sql_24                , #
       g_sql_25                ,
       g_sql_26                ,
       g_sql_27                , 
       g_sql_28                ,
       g_sql_29                ,
       g_sql_30                ,
       g_sql_30_1              ,
       g_sql_31                ,
       g_sql_32                ,
       g_sql_33                ,
       g_sql_34     CHAR(1000) , 
       g_cur_name   CHAR(0050) ,
       g_tabname    CHAR(0050)
END GLOBALS

FUNCTION define_querys_SEPB015()

   LET      g_sql_01  =
            ' SELECT  a.nombre_tabla ',
            ' FROM    taa_cd_tab_cuenta a '

   LET      g_sql_02 = ' SELECT * ' ,
            ' FROM   ', g_tabname  CLIPPED,
            ' WHERE  nss  = ? '
 
   LET      g_sql_03 = 
            ' SELECT a.*                              ',
            ' FROM   sep_mov_para_separar a          ,    ',
            '        dis_det_aporte   b               ',
            ' WHERE  a.folio =  ?                     ',
            ' AND    a.nss   =  ?                     ',
            ' AND    a.consecutivo_lote   =  ?        ',
            ' AND    a.folio = b.folio                ',
            ' AND    a.nss   = b.n_seguro             ',
            ' AND    a.consecutivo_lote = b.consec_reg_lote ',
            ' AND    a.subcuenta not in (4,8,14)    '

   LET      g_sql_03_1 = 
            ' SELECT a.*                              ',
            ' FROM   sep_movimientos_separados a      ',
            ' WHERE  a.subcuenta not in (4,8,14)      ',
            ' ORDER BY a.fecha_conversion,a.subcuenta '

   LET      g_sql_03_sar = 
            ' SELECT a.*                              ',
            ' FROM   sep_movimientos_separados a          ',
            ' WHERE  a.nss   =  ?                     ',
            ' AND    a.subcuenta =  7 '

   LET      g_sql_04 = 
            'EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) '

   LET      g_sql_05 = 
            'EXECUTE FUNCTION fn_prov_abono (?,?,?,?,?,?,?,?,?,?) '

   LET      g_sql_07 = 
            " INSERT INTO sep_tmp_viv_280 VALUES (?,?,?,?,?,?,?,?,?,?,?,",
            "?,?,?,?,?,?,?,?,?,?,?) "

   LET      g_sql_08 = 
            " SELECT a.ind_edad           ,  ",
            "        a.ind_transferencia     ",
            " FROM   cta_ctr_cuenta a        ",
            " WHERE  a.nss = ? "

   LET      g_sql_09 = 
            " INSERT INTO dis_provision VALUES (?,?,?,?,?,?,?,?,?,?,?,",
            "?,?,?,?,?,?,?,?,?,?,?) "

   LET      g_sql_09_1 = 
            " INSERT INTO sep_his_dis_cuenta VALUES (?,?,?,?,?,?,?,?,?,?,?,",
            "?,?,?,?,?,?,?,?,?,?,?) "

   LET      g_sql_10 = 
            " SELECT a.n_unico ,USER    ",
            " FROM   afi_mae_afiliado a ",
            " WHERE  a.n_seguro = ? "  

   LET      g_sql_11_1 = 
            " SELECT a.reg_patronal_imss       ",
            " FROM   sep_reg_patro_separador a ",
            " WHERE  a.folio         = ?       ",
            " AND    a.nss_separado  = ?       ",
            " AND    a.nss_separador = ?       "

   LET      g_sql_11 = 
            " SELECT a.folio ,                ",
            "        a.consec_reg_lote        ",
            " FROM   dis_det_aporte a         ",
            " WHERE  a.n_seguro          = ?  ",
            " AND    a.reg_patronal_imss = ?  "

   LET      g_sql_12 = 
            " SELECT a.fecha_conversion, ",
            "        a.precio_accion     ",
            " FROM   sep_mov_para_separar a  ",
            " WHERE  a.nss = ?           ",
            " AND    a.tipo_movimiento = 210 ",
            " AND    a.id_aportante = 'CORTE' ",
            " GROUP BY 1,2               "

   LET      g_sql_13 = 
            " SELECT subcuenta,sum(a.monto_en_acciones) ",
            " FROM   dis_provision a          ",
            " WHERE  a.folio    = ?           ",
            " AND    a.nss      = ?           ",
            " AND    a.fecha_conversion <=  ? ",
            " GROUP BY 1                      ",
            " ORDER BY 1                      "

   LET      g_sql_13_x = 
            " SELECT subcuenta,sum(a.monto_en_acciones) ",
            " FROM   dis_provision a          ",
            " WHERE  a.folio    = ?           ",
            " AND    a.nss      = ?           ",
            " GROUP BY 1                      ",
            " ORDER BY 1                      "

   LET      g_sql_13_b = 
            " SELECT siefore,subcuenta,sum(a.monto_en_acciones) ",
            " FROM   dis_provision a          ",
            " WHERE  a.folio    = ?           ",
            " AND    a.nss      = ?           ",
            " GROUP BY 1,2                    ",
            " ORDER BY 1,2                    "

   LET      g_sql_13_c = 
            " SELECT sum(a.monto_en_acciones) ",
            " FROM   dis_provision a          ",
            " WHERE  a.folio      = ?         ",
            " AND    a.nss        = ?         ",
            " AND    a.subcuenta  = ?         "

   LET      g_sql_13_3 = 
            " SELECT a.* ",
            " FROM   dis_provision a          ",
            " WHERE  a.folio    = ?           ",
            " AND    a.nss      = ?           ",
            " ORDER BY 1                      "

   LET      g_sql_14 = 
            'EXECUTE FUNCTION fn_prov_abono_sep (?,?,?,?,?,?,?,?,?,?) '

   LET      g_sql_15 = 
            " SELECT a.precio_del_dia    ",
            " FROM   glo_valor_accion a  ",
            " WHERE  fecha_valuacion = ? ",
            " AND    codigo_siefore  = ? "

   LET      g_sql_16 = 
            " INSERT INTO dis_cuenta     ",
            " SELECT a.*                 ",
            " FROM   dis_provision a     ",
            " WHERE  folio = ?           "

   LET      g_sql_17 = 
            " INSERT INTO sep_mov_para_separar",
            " SELECT *                     ",
            " FROM ",g_tabname              ,
            " WHERE nss = ?                ",
            " AND   fecha_conversion >= ?  "

   LET      g_sql_17_1 =
            " UPDATE sep_mov_para_separar",
            " SET dias_cotizados = 0 "

   LET      g_sql_17_2 = 
            " INSERT INTO sep_movimientos_pendientes  " ,
            " SELECT a.*,'0'                      " ,
            " FROM   sep_mov_para_separar a           "

   LET      g_sql_18 = 
            " SELECT a.* " ,
            " FROM   sep_mov_para_separar a      ",
            " WHERE  a.subcuenta = 4             ",
            " AND    (a.monto_en_pesos <> 0 OR   ",
            "         a.monto_en_acciones <> 0 ) "

   LET      g_sql_18_sar = 
            " SELECT a.* " ,
            " FROM   sep_mov_para_separar a      ",
            " WHERE  a.subcuenta = 8             ",
            " AND    (a.monto_en_pesos <> 0 OR   ",
            "         a.monto_en_acciones <> 0 ) "

   LET      g_sql_18_1 = 
            " SELECT a.* " ,
            " FROM   sep_mov_para_separar a "
           -- " WHERE  a.subcuenta NOT IN (4,8,14) " 

   LET      g_sql_19 = 
            ' SELECT a.*                              ',
            ' FROM   sep_mov_para_separar     a ,        ',
            '        dis_det_aporte   b               ',
            ' WHERE  a.folio =  ?                     ',
            ' AND    a.nss   =  ?                     ',
            ' AND    a.consecutivo_lote   =  ?        ',
            ' AND    a.folio = b.folio                ',
            ' AND    a.nss   = b.n_seguro             ',
            ' AND    a.consecutivo_lote = b.consec_reg_lote ',
            ' AND    a.subcuenta = 4                ',
            ' AND    a.tipo_movimiento <> 3         '

   LET      g_sql_19_1 = 
            ' SELECT a.*                            ',
            ' FROM   sep_movimientos_separados a        ',
            ' WHERE  a.subcuenta = 4                ',
            ' AND    a.tipo_movimiento <> 3         '

   LET      g_sql_19_sar = 
            ' SELECT a.*                              ',
            ' FROM   sep_movimientos_separados a          ',
            ' WHERE  a.nss   =  ?                     ',
            ' AND    a.subcuenta = 8                  ',
            ' AND    a.tipo_movimiento <> 3           '

   LET      g_sql_21 = 
            'EXECUTE FUNCTION fn_prov_abono_sep_viv(?,?,?,?,?,?,?,?,?,?,?,?) '

   LET      g_sql_22 = 
            ' DELETE FROM sep_mov_para_separar ',
            ' WHERE tipo_movimiento  = ?    ',
            '   AND  subcuenta  = ?         ',
            '   AND  siefore  = ?           ',
            '   AND  folio  = ?             ',
            '   AND  consecutivo_lote  = ?  ',
            '   AND  nss  = ?               ',
         --   '   AND  curp  = ?              ',
         --   '   AND  folio_sua  = ?         ',
            '   AND  fecha_pago  = ?        ',
            '   AND  fecha_valor  = ?       ',
            '   AND  fecha_conversion  = ?  ',
            '   AND  monto_en_pesos  = ?    ',
            '   AND  monto_en_acciones  = ? ',
            '   AND  precio_accion  = ?     ',
            '   AND  dias_cotizados  = ?    ',
         --   '   AND  sucursal  = ?          ',
            '   AND  id_aportante  = ?      ',
            '   AND  estado  = ?            ',
            '   AND  fecha_proceso  = ?     ',
            '   AND  usuario  = ?           ',
          --  '   AND  fecha_archivo = ?      ',
            '   AND  etiqueta  = ?         '

   LET      g_sql_22_1 = 
            ' DELETE FROM sep_movimientos_pendientes  ',
            ' WHERE tipo_movimiento  = ?    ',
            '   AND  subcuenta  = ?         ',
            '   AND  siefore  = ?           ',
            '   AND  folio  = ?             ',
            '   AND  consecutivo_lote  = ?  ',
            '   AND  nss  = ?               ',
          --  '   AND  curp  = ?              ',
          --  '   AND  folio_sua  = ?         ',
            '   AND  fecha_pago  = ?        ',
            '   AND  fecha_valor  = ?       ',
            '   AND  fecha_conversion  = ?  ',
            '   AND  monto_en_pesos  = ?    ',
            '   AND  monto_en_acciones  = ? ',
            '   AND  precio_accion  = ?     ',
            '   AND  dias_cotizados  = ?    ',
          --  '   AND  sucursal  = ?          ',
            '   AND  id_aportante  = ?      ',
            '   AND  estado  = ?            ',
            '   AND  fecha_proceso  = ?     ',
            '   AND  usuario  = ?           ',
          --  '   AND  fecha_archivo = ?      ',
            '   AND  etiqueta  = ?         '

   LET g_sql_22_2  = 
            ' DELETE FROM sep_movimientos_pendientes            ',
            ' WHERE  tipo_movimiento in (3,999,991,990,110) ',
            ' AND    subcuenta in (4,8)                     '

   LET g_sql_22_3 = 
            ' DELETE FROM sep_movimientos_pendientes            ',
            ' WHERE  tipo_movimiento in (999,991,990,110)   ',
            ' AND    subcuenta not in (4,8)                 '

   LET g_sql_23 = 
            " EXECUTE FUNCTION fn_cambia_aporte(?,?,?) "

   LET g_sql_24 = 
            " DELETE FROM dis_provision ",
            " WHERE  folio = ? "

   LET g_sql_25 = 
            " UPDATE sep_det_reg_sol_reclamante " ,
            " SET    estado = 8  ,              " ,
            "        fecha_proceso = ?        " ,
            " WHERE  n_seguro = ?             " ,
            " AND    nss      = ?             " ,
            " AND    estado   = ?             "

   LET g_sql_26 = 
            " INSERT INTO sep_movimientos_separados VALUES      ",
            " (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "

   LET g_sql_27 = 
    " INSERT INTO sep_movimientos_separados ",
    " SELECT                ",
    " a.tipo_movimiento    ,",
    " a.subcuenta          ,",
    " a.siefore            ,",
    " a.folio              ,",
    " a.consecutivo_lote   ,",
    " a.nss                ,",
    " a.curp               ,",
    " a.folio_sua          ,",
    " a.fecha_pago         ,",
    " a.fecha_valor        ,",
    " a.fecha_conversion   ,",
    " a.monto_en_pesos     ,",
    " a.monto_en_acciones  ,",
    " a.precio_accion      ,",
    " a.dias_cotizados     ,",
    " a.sucursal           ,",
    " a.id_aportante       ,",
    " a.estado             ,",
    " a.fecha_proceso      ,",
    " a.usuario            ,",
    " a.fecha_archivo      ,",
    " a.etiqueta            ",
    " FROM safre_af:sep_movimientos_pendientes a ",
    " WHERE a.nss       = ?                  ",
    " AND   a.indicador = 1                  "

    LET g_sql_28 = 
    " SELECT a.subcuenta                       , ",
    "        SUM(a.monto_en_acciones)          , ",
    "        ROUND(sum(a.monto_en_acciones),2)   ",
    " FROM   sep_tmp_viv_280 a                     ",
    " WHERE  a.folio            = ?               ",   #g_folio
    " AND    a.nss              = ?               ",   #g_nss_separado
    " AND    a.fecha_conversion <= ?              ",   #fecha ajsuste 990-991
    " AND    a.tipo_movimiento  = 280             ",
    " AND    a.subcuenta       IN (4,8)          ",
    " GROUP BY 1                                 "

    LET g_sql_29 =
    " SELECT a.nss                             , ",
    "        a.subcuenta                       , ",
    "        SUM(a.monto_en_acciones)          , ",
    "        ROUND(sum(a.monto_en_acciones),2)   ",
    " FROM   dis_provision a                     ",
    " WHERE  a.folio            = ?               ", #g_folio 
    " AND    a.nss              in (?,?)          ", #(separado,separador)
    " AND    a.fecha_valor <= ?              ", #fecha ajuste 990-991
    " AND    a.tipo_movimiento in (590,596,597)     ",
    " AND    a.subcuenta        IN (4,8)          ",
    " GROUP BY 1,2                               "

    LET g_sql_30 = 
    " UPDATE dis_provision      ",
    " SET    siefore = 11       ",
    " WHERE  folio = ?          ",
    " AND    subcuenta in (4,8) ",
    " AND    monto_en_acciones <> 0 "

    LET g_sql_30_1 = 
    " UPDATE dis_provision      ",
    " SET    siefore = 0        ",
    " WHERE  folio = ?          ",
    " AND    subcuenta in (4,8) ",
    " AND    monto_en_acciones = 0 "

    LET g_sql_31 = 
    " INSERT INTO sep_no_encontrados VALUES(?,?,?) "

    LET g_sql_32 = 
    " SELECT a.* " ,
    " FROM seg_modulo a ", 
    " WHERE modulo_cod = ? "

    LET g_sql_33 = 
    " SELECT a.codigo_siefore ",
    " FROM   cta_regimen a    ",
    " WHERE  a.nss = ?        ",
    " AND    a.subcuenta = ?  "

    LET g_sql_34 = 
    " SELECT a.codigo_siefore   ",
    " FROM   cta_nss_regimen a  ",
    " WHERE  a.nss = ?          ",
    " AND    a.grupo_regimen = 1 "

END FUNCTION
