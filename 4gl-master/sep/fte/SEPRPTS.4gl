DATABASE   safre_af
GLOBALS

DEFINE g_sql_01     CHAR(1000),
       g_sql_02     CHAR(1000),
       g_tabname    CHAR(50),
       g_tabname_1  CHAR(50)
 
###### variables globales para reporte ######

    DEFINE g_nombre_prog         CHAR(15)

    DEFINE g_tipo_desc1          CHAR(45),
           g_usuario             CHAR(008)

    DEFINE g_total_cuentas      INTEGER

    DEFINE g_folio              INTEGER

    DEFINE g_seg_modulo           RECORD LIKE seg_modulo.*

    DEFINE hoy                  DATE

############################################

END GLOBALS

FUNCTION define_querys()

LET g_sql_01 =
    
   ' SELECT a.n_seguro,a.paterno,a.materno,a.nombres, ',
   '        a.diag_confronta,a.clasifica_separacion,cont_servicio ',
   '  FROM  ', g_tabname,
   ' WHERE  a.folio = ? ',
   ' ORDER BY 1 '

LET g_sql_02 =
   ' SELECT b.nss_asociado,b.tipo_entidad_nss_involucrado, ',
   '        b.clave_entidad_involucrado,b.resultado_operacion, ',
   '        b.diag_proc1,b.diag_proc2,b.diag_proc3 ',
   '  FROM  ', g_tabname_1,
   ' WHERE  b.folio = ? ',
   ' AND    b.cont_servicio = ? ',
   ' ORDER BY 1 '

END FUNCTION
