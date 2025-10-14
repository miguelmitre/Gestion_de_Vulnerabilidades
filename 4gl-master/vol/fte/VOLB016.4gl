###############################################################################
#Proyecto     => safre_af
#Propietario  => E.F.P.
#Programa     => VOLB016
#Descripcion  => LANZADO DE CONSULTA DE APORTACIONES DE AHORRO VOLUNTARIOS CON BENEFICIO FISCAL
#Fecha        => 2023
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ
#Sistema      => VOL
###############################################################################
DATABASE safre_af
################################################################################
GLOBALS

   DEFINE gr_seg_modulo RECORD
   	   modulo_cod    CHAR(04),
       ruta_rescate  CHAR(40),
       ruta_listados CHAR(40)
   END RECORD

   DEFINE gc_usuario CHAR(08)
   DEFINE gs_afore   SMALLINT

   DEFINE gr_parametros RECORD
  	  fecha_ini DATE    ,
      fecha_fin DATE
  END RECORD

  DEFINE HOY DATE

  DEFINE gc_hora           CHAR(08)

  DEFINE gr_archivos RECORD
  	cza,
  	det,
  	final CHAR(200)
  END RECORD

  DEFINE gd_hoy DATE

  DEFINE gc_sql CHAR(30000)

  DEFINE gr_detalle RECORD
  	 nss              CHAR(11)     ,
  	 folio            INTEGER      ,
  	 consecutivo      INTEGER      ,
  	 mov              SMALLINT     ,
  	 fecha_conversion DATE         ,
  	 nombres          CHAR(40)     ,
  	 paterno          CHAR(40)     ,
  	 materno          CHAR(40)     ,
  	 curp             CHAR(18)     ,
  	 rfc              CHAR(13)     ,
  	 desc_subcta      CHAR(40)     ,
  	 pesos            DECIMAL(22,2),
  	 subcuenta        SMALLINT
  END RECORD

  DEFINE gi_total INTEGER
END GLOBALS
################################################################################
MAIN
   CALL STARTLOG(FGL_GETENV("USER")||".VOLB016.log")
   CALL init()

   LET gr_parametros.fecha_ini     = ARG_VAL(1)
   LET gr_parametros.fecha_fin     = ARG_VAL(2)

   #Verificar que se reciban los parametros
   IF gr_parametros.fecha_ini IS NULL THEN
      DISPLAY "No se recibió fecha inicial"
      EXIT PROGRAM
   END IF

   IF gr_parametros.fecha_fin IS NULL THEN
      DISPLAY "No se recibió fecha final"
      EXIT PROGRAM
   END IF

   LET gc_hora = TIME
   DISPLAY "INICIA REPORTE DE LIQUIDACION DE APORTACIONES VOLUNTARIAS CON BENEFICIO FISCAL: ", gc_hora
   DISPLAY "FECHA INICIAL: ", gr_parametros.fecha_ini USING "DD/MM/YYYY"
   DISPLAY "FECHA FINAL  : ", gr_parametros.fecha_fin USING "DD/MM/YYYY"

   CALL fn_genera_query()
   CALL fn_reporte()

   DISPLAY "REPORTE GENERADO: ", gr_archivos.final CLIPPED
   DISPLAY "TOTAL DE REGISTROS DE DETALLE: ", gi_total

   LET gi_total = gi_total + 1

   LET gc_hora = TIME
   DISPLAY "TERMINA REPORTE DE LIQUIDACION DE APORTACIONES VOLUNTARIAS CON BENEFICIO FISCAL: ", gc_hora
END MAIN
################################################################################
FUNCTION init()
   DEFINE ls_cont SMALLINT
   LET gr_seg_modulo.modulo_cod = "vol"

   SELECT USER         ,
          ruta_rescate ,
          ruta_listados
   INTO   gc_usuario                 ,
          gr_seg_modulo.ruta_rescate ,
          gr_seg_modulo.ruta_listados
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   LET hoy    = TODAY

   LET gd_hoy = TODAY

   INITIALIZE gc_sql TO NULL
   LET gi_total = 0
END FUNCTION
################################################################################
FUNCTION fn_genera_query()
   DEFINE lc_tabla CHAR(12)
   DEFINE lc_sql   CHAR(1000)
   DEFINE ls_anio  SMALLINT

   #PROCESAR SPLITS DE AÑOS PASADOS
   IF YEAR(gr_parametros.fecha_ini) < YEAR(gr_parametros.fecha_fin)  OR
   	  (YEAR(gr_parametros.fecha_ini) = YEAR(gr_parametros.fecha_fin) AND
   	   YEAR(gr_parametros.fecha_fin) < YEAR(TODAY)                 ) THEN

   	  LET ls_anio = YEAR(gr_parametros.fecha_ini)
      WHILE ls_anio <= YEAR(gr_parametros.fecha_fin)

      	 #Verificar split
      	 LET lc_tabla = "dis_cuenta", (ls_anio-2000) USING "&&";

         WHENEVER ERROR CONTINUE
            SELECT "X"
            FROM   SYSTABLES
            WHERE  tab_name = lc_tabla
            GROUP BY 1
         WHENEVER ERROR STOP

         IF SQLCA.SQLCODE = 0 THEN
            LET lc_sql = " SELECT a.nss             ,                                       ",    --nss
                         "        a.folio           ,                                       ",    --folio
                         "        a.consecutivo_lote,                                       ",    --consecutivo
                         "        a.tipo_movimiento ,                                       ",    --mov
                         "        a.fecha_conversion,                                       ",    --fecha_conversion
                         "        a.monto_en_pesos  ,                                       ",    --pesos
                         "        a.subcuenta                                               ",    --subcuenta
                         " FROM   ",lc_tabla CLIPPED, " a                                   ",
                         " WHERE  a.subcuenta       IN (11,12,15,16,23)                     ",
                         " AND    a.tipo_movimiento IN (1   ,123 ,124 ,310 ,1340,1341,1347) ",
                         " AND    a.fecha_conversion >= '", gr_parametros.fecha_ini,     "' ",
                         " AND    a.fecha_conversion <= '", gr_parametros.fecha_fin,     "' "

            #Query general
            IF gc_sql IS NULL THEN
               LET gc_sql = lc_sql CLIPPED
            ELSE
            	 LET gc_sql = gc_sql CLIPPED, " UNION ALL ", lc_sql
            END IF
         END IF
         LET ls_anio = ls_anio + 1
      END WHILE
   END IF

   #PROCESAR DIS_CUENTA ACTUAL
   #Si no se enconctró ningun split
   IF YEAR(gr_parametros.fecha_fin) = YEAR(gd_hoy) OR
   	  gc_sql IS NULL                               THEN
   	  LET lc_sql = " SELECT a.nss             ,                                       ", --nss
                   "        a.folio           ,                                       ", --folio
                   "        a.consecutivo_lote,                                       ", --consecutivo
                   "        a.tipo_movimiento ,                                       ", --mov
                   "        a.fecha_conversion,                                       ", --fecha_conversion
                   "        a.monto_en_pesos  ,                                       ", --pesos
                   "        a.subcuenta                                               ", --subcuenta
                   " FROM   dis_cuenta             a                                  ",
                   " WHERE  a.subcuenta       IN (11,12,15,16,23)                     ",
                   " AND    a.tipo_movimiento IN (1   ,123 ,124 ,310 ,1340,1341,1347) ",
                   " AND    a.fecha_conversion >= '", gr_parametros.fecha_ini,     "' ",
                   " AND    a.fecha_conversion <= '", gr_parametros.fecha_fin,     "' "
                   --" ORDER BY 2,3,5 "

      IF gc_sql IS NULL THEN
         LET gc_sql = lc_sql CLIPPED
      ELSE
      	 LET gc_sql = gc_sql CLIPPED, " UNION ALL ", lc_sql
      END IF
   END IF

   IF gc_sql IS NOT NULL THEN
      LET gc_sql = gc_sql CLIPPED, " ORDER BY 2,3,5 "
   END IF
END FUNCTION
################################################################################
FUNCTION fn_reporte()
   DEFINE lc_comando CHAR(2000)
   DEFINE lc_sql     CHAR(1000)

   #Nombrar archivos
   LET gc_hora = TIME
   LET gr_archivos.cza = gr_seg_modulo.ruta_listados CLIPPED, "/",
                         gc_usuario     CLIPPED,'.',
                         "reporte_liquidaciones_AV_BF_CZA",
                         gd_hoy USING           "DDMMYYYY", "_",
                         gc_hora[1,2],gc_hora[4,5],gc_hora[7,8]

   LET gr_archivos.det = gr_seg_modulo.ruta_listados CLIPPED, "/",
                         gc_usuario     CLIPPED,'.',
                         "reporte_liquidaciones_AV_BF_DET",
                         gd_hoy USING           "DDMMYYYY", "_",
                         gc_hora[1,2],gc_hora[4,5],gc_hora[7,8]

   LET gr_archivos.final = gr_seg_modulo.ruta_listados CLIPPED, "/",
                           gc_usuario     CLIPPED,'.',
                           "reporte_liquidaciones_AV_BF_",
                           gd_hoy USING           "DDMMYYYY", "_",
                           gc_hora[1,2],gc_hora[4,5],gc_hora[7,8]

   LET lc_comando = "cat ", gr_archivos.cza CLIPPED, " ",
                            gr_archivos.det CLIPPED,
                            " > ", gr_archivos.final CLIPPED

   LET lc_sql = "SELECT subct_desc FROM tab_subcuenta WHERE subct_cod = ?"
   PREPARE get_desc_subcta FROM lc_sql

   LET lc_sql = "SELECT nombres,paterno,materno,n_unico,n_rfc FROM afi_mae_afiliado WHERE n_seguro = ?"
   PREPARE get_afi FROM lc_sql

   PREPARE eje_sql FROM gc_sql
   DECLARE cur_liquidados CURSOR FOR eje_sql

   START REPORT rtp_det TO gr_archivos.det
   FOREACH cur_liquidados INTO gr_detalle.nss             , --nss
   	                           gr_detalle.folio           , --folio
   	                           gr_detalle.consecutivo     , --consecutivo
   	                           gr_detalle.mov             , --mov
                               gr_detalle.fecha_conversion, --fecha_conversion
                               gr_detalle.pesos           , --pesos
                               gr_detalle.subcuenta         --subcuenta

      EXECUTE get_desc_subcta INTO gr_detalle.desc_subcta
                              USING gr_detalle.subcuenta

      EXECUTE get_afi INTO gr_detalle.nombres,
                           gr_detalle.paterno,
                           gr_detalle.materno,
                           gr_detalle.curp   ,
                           gr_detalle.rfc
                      USING gr_detalle.nss

      OUTPUT TO REPORT rtp_det()
      LET gi_total = gi_total + 1
   END FOREACH
   FINISH REPORT rtp_det


   START REPORT rpt_cza TO gr_archivos.cza
      OUTPUT TO REPORT rpt_cza()
   FINISH REPORT rpt_cza

   RUN lc_comando
END FUNCTION
################################################################################
REPORT rpt_cza()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, 'NSS/NTI'              ,'|',
                        'Folio SAFRE'          ,'|',
                        'Consecutivo lote'     ,'|',
                        'Tipo de movimiento'   ,'|',
                        'Fecha liquidación'    ,'|',
                        'Nombre del trabajador','|',
                        'Apellido paterno'     ,'|',
                        'Apellido materno'     ,'|',
                        'CURP'                 ,'|',
                        'RFC'                  ,'|',
                        'Descripción subcuenta','|',
                        'Monto en pesos'       ,'|',
                        'Subcuenta'            ,'|'
END REPORT
################################################################################
REPORT rtp_det()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001,gr_detalle.nss                                   ,'|',
                       gr_detalle.folio            USING "<<<<<<<<<<<<&",'|',
                       gr_detalle.consecutivo      USING "<<<<<<<<<<<<&",'|',
                       gr_detalle.mov              USING "<<<<<<<<<<<<&",'|',
                       gr_detalle.fecha_conversion USING "DD/MM/YYYY"   ,'|',
                       gr_detalle.nombres          CLIPPED              ,'|',
                       gr_detalle.paterno          CLIPPED              ,'|',
                       gr_detalle.materno          CLIPPED              ,'|',
                       gr_detalle.curp                                  ,'|',
                       gr_detalle.rfc              CLIPPED              ,'|',
                       gr_detalle.desc_subcta      CLIPPED              ,'|',
                       gr_detalle.pesos            USING "<<<<<<<<<<<&.&&",'|',
                       gr_detalle.subcuenta        USING "<<<<<<<<<<<&.&&&&&&",'|'
END REPORT
################################################################################