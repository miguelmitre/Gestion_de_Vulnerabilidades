
GLOBALS 
DEFINE  g_sql_01_a                   CHAR(1000)
DEFINE  g_sql_02_a                   CHAR(1000)
DEFINE  g_hoy                        DATE
DEFINE  g_tabla                      CHAR(50)
DEFINE  g_estado                     SMALLINT
DEFINE  g_construct                  CHAR(1000)


END GLOBALS

################################################################################
FUNCTION desp_arr_GLOB_CFOLS()
#=====QRY PARA LA LIQUIDACION=====
LET g_sql_01_a =
          " SELECT a.*,b.descripcion ",
          " FROM ",g_tabla CLIPPED," a,taa_cd_edo_cedente b",
          " WHERE b.tipo    IN(2,3)  ",
          "   AND  a.estado = ",g_estado ,
          "   AND  a.estado = b.estado ",
          "   AND  a.fecha_liquidacion  >=  ","'",g_hoy  -  90,"'",
          "   AND ",g_construct   CLIPPED,
          " ORDER BY a.fecha_liquidacion DESC  "

#=====QRY PARA LA PROVISION=====
LET g_sql_02_a =
          " SELECT a.*,b.descripcion ",
          " FROM ",g_tabla CLIPPED," a,taa_cd_edo_cedente b",
          " WHERE b.tipo    IN(2,3)  ",
          "   AND  a.estado = ",g_estado ,
          "   AND  a.estado = b.estado ",
          " ORDER BY 1 DESC  "

END FUNCTION
