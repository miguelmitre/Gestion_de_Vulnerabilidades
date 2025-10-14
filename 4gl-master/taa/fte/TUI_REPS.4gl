DATABASE safre_af
GLOBALS 
DEFINE g_sql_01                     ,   #Este lo llama  el GLOB_REP.4gl
       g_sql_02                     ,   #Este lo llama  el GLOB_REP.4gl
       g_sql_03                     ,   #Este lo llama  el GLOB_REP.4gl
       g_sql_04                     ,   #Este lo llama  el GLOB_CON.4gl
       g_sql_05                     ,   #Este lo llama  el GLOB_CON.4gl
       g_sql_06                     ,   #Este lo llama  el GLOB_REP.4gl
       g_sql_07                     ,   #Este lo llama el TCAAL007.4gl
       g_sql_08                     CHAR(1000),#Este lo llama el TCAAL008.4gl
       g_tabname                    CHAR(50),
       g_estado                     SMALLINT,
       g_construct                  CHAR(1000),
       g_fecha_accion               ,
       g_fecha_parti                DATE
###### variables globales para reporte ######
    DEFINE  g_nombre_programa       CHAR(15)
    DEFINE  g_tipo_desc1            ,
            g_tipo_desc2            CHAR(45),
            g_usuario               CHAR(008)
    DEFINE  g_total_cuentas         INTEGER
    DEFINE  g_folio                 INTEGER
    DEFINE  g_seg_modulo            RECORD LIKE seg_modulo.*
    DEFINE  hoy                     DATE
    DEFINE  g_tip_rep               CHAR(04)
END GLOBALS

FUNCTION define_querys()
   LET      g_sql_01            =
           'SELECT  b.folio,b.siefore, ',
           '        b.tipo_movimiento,b.subcuenta,b.fecha_conversion, ',
           '        SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
           '  FROM  ',g_tabname ,
           ' WHERE  b.folio             =  ?',
           '   AND  b.estado            =  6 ',
           ' GROUP  BY  1,2,3,4,5 ',
           ' ORDER  BY  2,3,4,5 '
   LET     g_sql_02                     = 
           'SELECT  b.siefore , ',
           '        b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos)',
           '  FROM  ',g_tabname ,
           ' WHERE  b.folio             =  ? ',
           '   AND  b.estado            =  6 ',
           ' GROUP  BY  1,2 ',
           ' ORDER  BY  1,2 '
   LET     g_sql_03                     = 
          'SELECT  b.tipo_movimiento, ',
          '        SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
          '  FROM  ', g_tabname,
          ' WHERE  b.folio              =  ? ',
          '   AND  b.estado             =  6 ',
          '   AND  b.siefore            =  ? ',
          ' GROUP  BY  1 ',
          ' ORDER  BY  1 '
   LET    g_sql_04               =
          'SELECT  folio,b.siefore ,',
          '        b.subcuenta,b.tipo_movimiento,b.fecha_conversion, ',
          '        SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
          '  FROM  ', g_tabname,
          ' WHERE  b.folio              =  ? ',
          '   AND  b.estado             =  6 ',
          ' GROUP  BY 1,2,3,4,5 ',
          ' ORDER  BY 2,3,4;'
   LET    g_sql_05                      =
          'SELECT  UNIQUE  fecha_conversion ',
          '  FROM  ', g_tabname,
          ' WHERE  b.folio              =  ? ',
          ' GROUP  BY  1; '
   LET    g_sql_06                      =
          'SELECT  MAX (precio_accion) ',
          '  FROM   ', g_tabname,
          ' WHERE  b.folio              =  ? ',
          '   AND  b.siefore            =  ?;'
END FUNCTION

FUNCTION desp_arr_GLOB_CFOLS()
            #=====QRY PARA LA PROVISION=====
   LET       g_sql_07               =
          " SELECT  a.*,b.descripcion ",
          "   FROM  tui_ctr_folio a,taa_cd_edo_cedente b",
          "  WHERE  b.tipo                  IN(2,3)  ",
          "    AND  a.estado                 = ",g_estado ,
          "    AND  a.estado                 =  b.estado ",
          " ORDER   BY 1 DESC  "
            #=====QRY PARA LA LIQUIDACION=====
    LET      g_sql_08              =
          " SELECT  a.*,b.descripcion ",
          " FROM    tui_ctr_folio a,taa_cd_edo_cedente b",
          " WHERE   b.tipo                  IN(2,3)  ",
          "   AND   a.estado                 =  ",g_estado ,
          "   AND   a.estado                 =  b.estado ",
          "   AND   a.fecha_liquidacion     >=  TODAY  -  90",
          "   AND ",g_construct   CLIPPED,
          " ORDER   BY  1     DESC  "
END FUNCTION
