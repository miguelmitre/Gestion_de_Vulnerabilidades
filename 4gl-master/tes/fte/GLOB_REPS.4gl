DATABASE safre_af
GLOBALS 

DEFINE g_sql_01    ,
       g_sql_02    ,
       g_sql_03    CHAR(2000),
       g_tabname   CHAR(50)

###### variables globales para reporte ######


    DEFINE g_nombre_programa    CHAR(15)

    DEFINE g_tipo_desc1          ,
           g_tipo_desc2          CHAR(45),
           g_usuario             CHAR(008)

    DEFINE g_fecha_accion       ,
           g_fecha_parti        DATE

    DEFINE g_total_cuentas      INTEGER

    DEFINE g_folio              INTEGER

    DEFINE g_seg_modulo           RECORD LIKE seg_modulo.*

    DEFINE hoy                  DATE

    DEFINE g_tip_rep            CHAR(04)
############################################

END GLOBALS


FUNCTION define_querys()

LET g_sql_01 =
' SELECT b.folio,CASE WHEN b.siefore = 0 THEN (SELECT x.razon_social ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 0)  ',
          ' WHEN b.siefore = 1 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 1)  ',
          ' WHEN b.siefore = 2 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 2)  ',
          ' WHEN b.siefore = 3 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 3)  ',
          ' WHEN b.siefore = 4 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 4)  ',
          ' WHEN b.siefore = 5 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 5)  ',
          ' WHEN b.siefore = 6 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 6)  ',
          ' WHEN b.siefore = 11 THEN (SELECT x.razon_social      ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 11) ',
   ' END CASE ,',
   '       b.tipo_movimiento,b.subcuenta,b.fecha_conversion, ',
   '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio = ?',
   ' GROUP  BY  1,2,3,4,5 ',
   ' ORDER  BY  2,3,4,5 '

LET g_sql_02 = 
' SELECT CASE WHEN b.siefore = 0 THEN (SELECT x.razon_social     ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 0)  ',
          ' WHEN b.siefore = 1 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 1)  ',
          ' WHEN b.siefore = 2 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 2)  ',
          ' WHEN b.siefore = 3 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 3)  ',
          ' WHEN b.siefore = 4 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 4)  ',
          ' WHEN b.siefore = 5 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 5)  ',
          ' WHEN b.siefore = 6 THEN (SELECT x.razon_social       ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 6)  ',
          ' WHEN b.siefore = 11 THEN (SELECT x.razon_social      ',
                                  ' FROM tab_siefore_local x     ',
                                  ' WHERE x.codigo_siefore = 11) ',
   ' END CASE, ',
   '       b.subcuenta,SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
   '  FROM  ',g_tabname ,
   ' WHERE  b.folio = ? ',
   ' GROUP  BY  1,2 ',
   ' ORDER  BY  1,2 '

LET g_sql_03 = 
  'SELECT b.tipo_movimiento, ',
  '       SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
  '  FROM  ', g_tabname,
  ' WHERE  b.folio = ? ',
  '   AND  b.siefore = ? ',
  ' GROUP  BY  1 ',
  ' ORDER  BY  1 '

END FUNCTION
