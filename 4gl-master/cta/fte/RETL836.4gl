#*******************************************************************************
#Modulo    : ESTADO DE CUENTA
#Objetivo  : EMITE ARCHIVO PLANO DE ESTADO DE CUENTA DE TRABAJADORES
#            PENSIONADOS POR TRANSFERENCIA Y/O DISPOSICION
#Funciones : ini()
#            archivo()
#            inicializa()
#            saldos()
#            saldos_disp()
#            saldo_dia()
#            saldo_final()
#            get_datos_ret()
#            Ingresa_etapa()
#            Actualiza_etapa()
#            despliega()
#            redondea()
#Autor     : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#Fecha     : 25 de JULIO DE 2012
#Modifica  : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#            Los saldos remanentes y finales se obtendrán realizando una
#            valuación al último día natural del mes que se reporta.
#Fecha     : 28 de SEPTIEMBRE DE 2012
#Lugar     : /safre/cta/fte/nueva_2008
#Argumentos: gi_folio. Folio en que se agrupan los NSS que se reportarán
#Formas    : ------------------------------------------------------------------
#-----------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => CPL-1626    09-Mayo-2014  Alejandro Chagoya Salazar      #
#Actualizacion     => Se agrega ciclo para saldos de dis_cuenta, cuando existe #
#                  => ms de un registro en el mismo mes                        #
#------------------------------------------------------------------------------#
#Requerimiento     => CPL-2167    Enero 2016  Emmanuel Reyes                   #
#Actualizacion     => Se modifica el proceso de obtención de datos para PMG de #
#                  => acuerdo a lo especidficado por la afore                  #
#------------------------------------------------------------------------------#
#Requerimiento     => CPL-2167    Marzo   2016  Emmanuel Reyes                 #
#Actualizacion     => Se modifica la obtención de la fecha de liquidación de la#
#                  => primer mensualidad evitando caidas de acuerdo a la observ#
#Requerimiento     => CPL-2167    Junio   2016  Emmanuel Reyes                 #
#Actualizacion     => Se modifica la obtención del saldo antes de la liq. de   #
#                  => PMG tomando solo las subcuentas involucradas en PMG      #
#Actualizacion     => CPL-2835                                                 #
################################################################################

DATABASE safre_af

GLOBALS
   DEFINE gi_folio                INTEGER
   DEFINE gc_usuario              CHAR(08)

   DEFINE gs_afore                SMALLINT
   DEFINE hoy                     DATE

   DEFINE gr_seg_modulo           RECORD
          modulo_cod              CHAR(04),
          ruta_envio              CHAR(40)
   END RECORD

   DEFINE gc_log                  CHAR(50)
   DEFINE gi_registros            INTEGER

   DEFINE gr_reporte              RECORD
          tipo_registro           SMALLINT     ,
          consecutivo_envio       DECIMAL(11,0),
          dia_corte               SMALLINT     ,
          mes_corte               SMALLINT     ,
          nombre_completo         CHAR(120)    ,
          calle_num               CHAR(62)     ,
          colonia                 CHAR(60)     ,
          cp                      CHAR(05)     ,
          centro_reparto          CHAR(05)     ,
          entidad_federal         CHAR(40)     ,
          munic_delega            CHAR(40)     ,
          nss                     CHAR(11)     ,
          rfc                     CHAR(13)     ,
          curp                    CHAR(18)     ,
          saldo_ini_ret           DECIMAL(22,6),
          saldo_ini_viv           DECIMAL(22,6),
          saldo_trans_ret         DECIMAL(22,6),
          saldo_trans_viv         DECIMAL(22,6),
          saldo_rem_ret           DECIMAL(22,6),
          saldo_rem_viv           DECIMAL(22,6),
          ley_pen                 CHAR(06)     ,
          regimen                 CHAR(02)     ,
          seguro                  CHAR(02)     ,
          pension                 CHAR(02)     ,
          pen_entidad             CHAR(50)     ,
          pen_monto_trans         DECIMAL(22,6),
          pen_fecha_trans         DATE         ,
          afo_entidad             CHAR(50)     ,
          afo_medio_pago          CHAR(40)     ,
          afo_rec_entregado       DECIMAL(22,6),
          afo_fecha_entrega       DATE         ,
          afo_isr                 DECIMAL(22,6),
          infonavit_entidad       CHAR(50)     ,
          infonavit_numcta        CHAR(20)     ,
          infonavit_entregado     DECIMAL(22,6),
          infonavit_fecha_entrega DATE         ,
          infonavit_isr           DECIMAL(22,6),
          fecha_emision           DATE         ,
          fecha_ini_pen           DATE         ,
          monto_pen               DECIMAL(22,6),
          saldo_fin               DECIMAL(22,6)
          END RECORD

   DEFINE gc_archivo              CHAR(500)
   DEFINE gs_tipo_informe         SMALLINT
   DEFINE gs_pmg                  SMALLINT  --CPL-2167
   DEFINE gc_mensaje              CHAR(300) --CPL-2835
   DEFINE VERSION                 CHAR(10)  --CPL-3046

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN

   LET VERSION = "v1.3046.01"   --CPL-3046

   LET gi_folio       = ARG_VAL(1)

   LET gc_mensaje = "\n====================================================================",
                    "\nINICIA PROCESO DE ESTADO DE CUENTA CON FOLIO: ", gi_folio USING "<<<<<<<<<"," ",VERSION,
                    "\n===================================================================="

   DISPLAY gc_mensaje CLIPPED

   CALL STARTLOG(FGL_GETENV("USER")||".RETL836.log")

   CALL ERRORLOG(gc_mensaje CLIPPED)

   LET hoy          = TODAY
   LET gi_registros = 0

   LET gr_seg_modulo.modulo_cod = "cta"

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   #Cambiar estado
   UPDATE cta_ctr_proceso
   SET    estado = 2
   WHERE  folio  = gi_folio
   AND    estado = 1

   SELECT tipo_informe
   INTO   gs_tipo_informe
   FROM   cta_ctr_proceso
   WHERE  folio  = gi_folio
   AND    estado = 2
   GROUP BY 1

   CALL ini()
   CALL archivo()
   LET gc_log = "chmod 777 *RETL836.log"
   RUN gc_log

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA:",gi_registros
END MAIN
################################################################################
#*******************************************************************************
#Función : ini()
#Objetivo: Prepares de datos generales, domicilio y accesos a datamart
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION ini()
   DEFINE lc_sql CHAR(2000)

   LET lc_sql = " SELECT n_unico                      , ",
                "        n_rfc                        , ",
                "        NVL(TRIM(nombres),' ')||' '||  ",
                "        NVL(TRIM(paterno),' ')||' '||  ",
                "        NVL(TRIM(materno),' ')         ",
                " FROM   afi_mae_afiliado               ",
                " WHERE  n_seguro = ?                   "
   PREPARE get_afil FROM lc_sql

   LET lc_sql = " SELECT FIRST 1                               ",
                "        NVL(TRIM(dom.calle),' ')  || ' ' ||   ",
                "        NVL(TRIM(dom.numero),' ') || ' ' ||   ",
                "        NVL(TRIM(dom.depto),' ')           ,  ", --calle_num
                "        dom.colonia                        ,  ", --colonia
                "        dom.codpos                         ,  ", --cp
                "        NVL(rep.centro_reparto,'00000')    ,  ", --centro_reparto
                "        edo.estad_desc                     ,  ", --entidad_federal
                "        del.deleg_desc                        ", --munic_delega
                " FROM   safre_af:afi_mae_afiliado       a,    ",
                "        safre_af:afi_domicilio        dom,    ",
                "        OUTER safre_af:tab_estado     edo,    ",
                "        OUTER safre_af:tab_delegacion del,    ",
                "        OUTER safre_af:tab_reparto    rep     ",
                " WHERE  dom.nss            = a.n_seguro       ",
                " AND    dom.nss            = ?                ",
                " AND    dom.n_folio        = a.n_folio        ",
                " AND    dom.tipo_solicitud = a.tipo_solicitud ",
                " AND    dom.marca_envio    = 'X'              ",
                " AND    edo.estad_cod      = dom.estado       ",
                " AND    del.estad_cod      = dom.estado       ",
                " AND    del.deleg_cod      = dom.delega       ",
                " AND    rep.codigo_postal  = dom.codpos       "
               #" AND    dom.dom_cod        = 1                " --personal #CPL-2093
    PREPARE get_domicilio FROM lc_sql

    --LET lc_sql = " SELECT DECODE (a.regimen, '73', 'Ley 73', ",
    --             "                           '97', 'Ley 97'),",
    --             "        a.regimen                         ,",
    --             "        a.tipo_seguro                     ,",
    --             "        a.tipo_pension                    ,",
    --             "        b.descripcion    destino          ,",
    --             "        a.fecha_ini_pen                   ,",
    --             "        NVL(a.mto_constitutivo,0)          ",
    --             " FROM   ret_transf_rx         a,           ",
    --             "        OUTER tab_destino_ret b            ",
    --             " WHERE  a.nss         = ?                  ",
    --             " AND    a.consecutivo = ?                  ",
    --             " AND    a.cve_destino = b.cve_destino      "

    LET lc_sql = " SELECT FIRST 1                                ",
                 "        DECODE (a.regimen, '73', 'LEY 73',     ",
                 "                           '97', 'LEY 97'),    ",
                 "        a.regimen                 ,    ",
                 "        a.tipo_seguro             ,    ",
                 "        a.tipo_pension            ,    ",
                 "        c.descripcion    destino  ,    ",
                 "        b.cve_destino             ,    ",
                 "        a.fecha_ini_pen           ,    ",
                 "        a.fecha_resolucion        ,    ",
                 "        NVL(a.monto_sol_imss,0)     ",
                 " FROM   ret_det_datamart      a,    ",
                 "        ret_matriz_derecho    b,    ",
                 "        OUTER tab_destino_ret c     ",
                 " WHERE  a.nss              = ?      ",
                 " AND    a.folio            = ?      ",
                 " AND    b.cve_destino      = c.cve_destino     ",
                 " AND    a.regimen          = b.regimen         ",
                 " AND    a.tipo_seguro      = b.tipo_seguro     ",
                 " AND    a.tipo_pension     = b.tipo_pension    ",
                 " AND    a.tipo_prestacion  = b.tipo_prestacion ",
                 " AND    a.tipo_retiro      = b.tipo_retiro     ",
                 " AND    a.estado      = 118 ",
                 " ORDER BY b.cve_destino "
    PREPARE get_pension_trans FROM lc_sql

#CPL-1626 INI
    LET lc_sql = " SELECT FIRST 1                                ",
                 "        DECODE (a.regimen, '73', 'LEY 73',     ",
                 "                           '97', 'LEY 97'),    ",
                 "        a.regimen                         ,    ",
                 "        a.tipo_seguro                     ,    ",
                 "        a.tipo_pension                    ,    ",
                 "        c.descripcion    destino          ,    ",
                 "        b.cve_destino                     ,    ",
                 "        a.fecha_ini_pen                   ,    ",
                 "        a.fecha_resolucion                ,    ",
                 "        NVL(a.monto_sol_imss,0)   ",
                 " FROM   ret_det_datamart_log   a, ",
                 "        ret_matriz_derecho    b,  ",
                 "        OUTER tab_destino_ret c   ",
                 " WHERE  a.nss              = ?    ",
                 " AND    a.folio            = ?    ",
                 " AND    b.cve_destino      = c.cve_destino     ",
                 " AND    a.regimen          = b.regimen         ",
                 " AND    a.tipo_seguro      = b.tipo_seguro     ",
                 " AND    a.tipo_pension     = b.tipo_pension    ",
                 " AND    a.tipo_prestacion  = b.tipo_prestacion ",
                 " AND    a.tipo_retiro      = b.tipo_retiro     ",
                 " AND    a.estado      = 118 ",
                 " ORDER BY b.cve_destino  "
    PREPARE get_pension_trans2 FROM lc_sql

    LET lc_sql = " SELECT FIRST 1 ",
                 "  DECODE (a.regimen, '73', 'LEY 73',  ",
                 "                     '97', 'LEY 97'), ",
                 "  a.regimen , ",
                 "  a.tipo_seguro , ",
                 "  a.tipo_pension , ",
                 "  b.descripcion    destino   , ",
                 "  a.cve_destino              , ",
                 "  a.fecha_ini_pen            , ",
                 "  a.fecha_resolucion           ",
                 --"  NVL(a.monto_sol_imss,0)      ",
                 " FROM   ret_solicitud_tx      a,     ",
                 "        OUTER tab_destino_ret b      ",
                 " WHERE  a.cve_destino      = b.cve_destino  ",
                 " AND    a.nss              = ?              ",
                 " AND    a.consecutivo      = ?              ",
                 " ORDER BY a.cve_destino "
    PREPARE get_pension_disp FROM lc_sql

    {LET lc_sql = " SELECT FIRST 1 ",
                 "  DECODE (a.regimen, '73', 'LEY 73',  ",
                 "                     '97', 'LEY 97'), ",
                 "  a.regimen , ",
                 "  a.tipo_seguro , ",
                 "  a.tipo_pension , ",
                 "  c.descripcion    destino   , ",
                 "  b.cve_destino              , ",
                 "  a.fecha_ini_pen            , ",
                 "  a.fecha_resolucion         , ",
                 "  NVL(a.monto_sol_imss,0)      ",
                 " FROM   ret_det_datamart_log      a,     ",
                 "        ret_solicitud_tx      b,     ",
                 "        OUTER tab_destino_ret c      ",
                 " WHERE  a.nss              = b.nss          ",
                 " AND    a.sec_pension      = b.sec_pension  ",
                 " AND    a.estado       = 118  ",
                 " AND    b.cve_destino      = c.cve_destino  ",
                 " AND    b.nss              = ?              ",
                 " AND    b.consecutivo      = ?              ",
                 " ORDER BY b.cve_destino "
    PREPARE get_pension_disp2 FROM lc_sql}

#CPL-1626 FIN

    LET lc_sql = " SELECT DECODE (b.regimen, '73', 'LEY 73',  ",
                 "                           '97', 'LEY 97'), ",
                 "        b.regimen                         , ",
                 "        b.tipo_seguro                     , ",
                 "        b.tipo_pension                    , ",
                 "        c.descripcion    destino          , ",
                 "        b.cve_destino                     , ",
                 "        b.fecha_ini_pen                   , ",
                 "        b.fecha_resolucion                , ",
                 "        '0'              monto_sol_imss     ",
                 " FROM   ret_solicitud_tx      b,            ",
                 "        OUTER tab_destino_ret c             ",
                 " WHERE  b.cve_destino      = c.cve_destino  ",
                 " AND    b.nss              = ?              ",
                 " AND    b.consecutivo      = ?              ",
                 " ORDER BY b.cve_destino                     "
    PREPARE get_pension_disp_mlm FROM lc_sql

   #CPL-2017
   LET lc_sql = " SELECT FIRST 1                                ",
                "        DECODE (a.regimen, '73', 'LEY 73',     ",
                "                           '97', 'LEY 97'),    ", --ley_pen
                "        a.regimen                         ,    ", --regimen
                "        a.tipo_seguro                     ,    ", --seguro
                "        a.tipo_pension                    ,    ", --pension
                "        c.descripcion    destino          ,    ", --pen_entidad
                "        ''                                ,    ", --lc_cve_destino
                "        a.fecha_ini_pen                   ,    ", --fecha_ini_pen
                "        a.fecha_resolucion                ,    ", --fecha_resolucion
                "        NVL(a.monto_sol_imss,0)                ", --monto_pen
                " FROM   ret_det_datamart      a,               ",
                "        pen_solicitud_pmg     b,               ",
                "        ret_matriz_derecho    d,               ",
                "        OUTER tab_destino_ret c                ",
                " WHERE  a.nss              = b.nss             ",
                " AND    a.sec_pension      = b.sec_pension     ",
                --" AND    a.estado           = 118               ",
                " AND    a.tipo_seguro      = d.tipo_seguro     ",
                " AND    a.tipo_pension     = d.tipo_pension    ",
                " AND    a.tipo_prestacion  = d.tipo_prestacion ",
                --" AND    a.tipo_retiro      = d.tipo_retiro     ",
                " AND    d.cve_destino      = c.cve_destino     ",
                " AND    b.nss              = ?                 ",
                " AND    b.consecutivo      = ?                 ",
                " ORDER BY d.cve_destino                        "
   PREPARE get_pension_pmg FROM lc_sql

   LET lc_sql = " SELECT FIRST 1                                ",
                "        DECODE (a.regimen, '73', 'LEY 73',     ",
                "                           '97', 'LEY 97'),    ", --ley_pen
                "        a.regimen                         ,    ", --regimen
                "        a.tipo_seguro                     ,    ", --seguro
                "        a.tipo_pension                    ,    ", --pension
                "        c.descripcion    destino          ,    ", --pen_entidad
                "        ''                                ,    ", --lc_cve_destino
                "        a.fecha_ini_pen                   ,    ", --fecha_ini_pen
                "        a.fecha_resolucion                ,    ", --fecha_resolucion
                "        NVL(a.monto_sol_imss,0)                ", --monto_pen
                " FROM   ret_det_datamart_log  a,               ",
                "        pen_solicitud_pmg     b,               ",
                "        ret_matriz_derecho    d,               ",
                "        OUTER tab_destino_ret c                ",
                " WHERE  a.nss              = b.nss             ",
                " AND    a.sec_pension      = b.sec_pension     ",
                --" AND    a.estado           = 118               ",
                " AND    a.tipo_seguro      = d.tipo_seguro     ",
                " AND    a.tipo_pension     = d.tipo_pension    ",
                " AND    a.tipo_prestacion  = d.tipo_prestacion ",
                --" AND    a.tipo_retiro      = d.tipo_retiro     ",
                " AND    d.cve_destino      = c.cve_destino     ",
                " AND    b.nss              = ?                 ",
                " AND    b.consecutivo      = ?                 ",
                " ORDER BY d.cve_destino                        "
   PREPARE get_pension_pmg2 FROM lc_sql

   LET lc_sql = "EXECUTE FUNCTION safre_tmp:fn_valua_bono_issste(?, ?, ?)"
   PREPARE get_bono_valuado FROM lc_sql
   
   
END FUNCTION
################################################################################
#*******************************************************************************
#Función : archivo()
#Objetivo: Flujo principal del proceso, ejecuta las funciones necesarias para
#          recopilar la información de cada Nss
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : gc_archivo Nombre y ruta del archivo
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION archivo()
   DEFINE ld_fecha_fin         DATE
   DEFINE lc_comando           CHAR(500)

   DEFINE ld_fecha_ini_reporte DATE
   DEFINE ld_fecha_fin_reporte DATE

   DEFINE li_folio             INTEGER
   DEFINE ld_consec_disp       DECIMAL(11,0)
   DEFINE lr_mensualidad       RECORD LIKE pen_ctr_pago_det.*



   LET gc_archivo = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    gc_usuario               CLIPPED,".",
                    "EC_FINAL."                         ,
                    gi_folio                 USING "<<<<<<<<<<<<<<<<<<<<"

   DISPLAY "SE GENERARA EL ARCHIVO: ",gc_archivo CLIPPED

   START REPORT rpt_edc_final TO gc_archivo

   #Encabezado
   OUTPUT TO REPORT rpt_edc_final(1)

   CALL Ingresa_etapa(gi_folio,2,0,"Inicia calculo de estado de cuenta")

   --SELECCIONA CADA UNA DE LAS INSTRUCCIONES DE GENERACION DE INFORMACION
   DECLARE cur_detalle CURSOR FOR
   SELECT nss         ,
          fecha_inicio,
          fecha_fin
   FROM   cta_ctr_proceso
   WHERE  folio = gi_folio

   FOREACH cur_detalle INTO gr_reporte.nss      ,
                             ld_fecha_ini_reporte,
                             ld_fecha_fin_reporte
      #Inicializar
      CALL inicializa()

      #curp, rfc, nombre
      EXECUTE get_afil USING gr_reporte.nss
                       INTO  gr_reporte.curp,
                             gr_reporte.rfc ,
                             gr_reporte.nombre_completo

      #domicilio
      EXECUTE get_domicilio USING gr_reporte.nss
                            INTO  gr_reporte.calle_num      ,
                                  gr_reporte.colonia        ,
                                  gr_reporte.cp             ,
                                  gr_reporte.centro_reparto ,
                                  gr_reporte.entidad_federal,
                                  gr_reporte.munic_delega
      #Saldos
      CASE
         WHEN gs_tipo_informe = 14 OR --TRANSFERENCIA 73
              gs_tipo_informe = 15    --TRANSFERENCIA 97

              CALL saldos(ld_fecha_ini_reporte,ld_fecha_fin_reporte) RETURNING gr_reporte.consecutivo_envio, --folio trans
                                                                          li_folio                    , --folio trans
                                                                          ld_consec_disp                --consecutivo disp

              LET gr_reporte.dia_corte = DAY(gr_reporte.pen_fecha_trans)
              LET gr_reporte.mes_corte = MONTH(gr_reporte.pen_fecha_trans)

         WHEN gs_tipo_informe = 16 OR --DISP 73
              gs_tipo_informe = 17    --DISP 97

              CALL saldos_disp(ld_fecha_ini_reporte,ld_fecha_fin_reporte) RETURNING gr_reporte.consecutivo_envio, --folio trans
                                                                                    li_folio                    , --folio trans
                                                                                    ld_consec_disp                --consecutivo disp

              LET gr_reporte.dia_corte = DAY(gr_reporte.afo_fecha_entrega)
              LET gr_reporte.mes_corte = MONTH(gr_reporte.afo_fecha_entrega)

         #CPL-2017
         WHEN gs_tipo_informe = 18 --PMG

              LET gc_mensaje = "PROCESANDO CONTROL(18) - NSS:", gr_reporte.nss," FOLIO: ",gi_folio USING "<<<<<<<<"
              CALL ERRORLOG(gc_mensaje CLIPPED)
              DISPLAY gc_mensaje CLIPPED

              --CPL-2835
              --SE RECUPERA LA MENSUALIDAD LIQUIDADA EN LA FECHA FIN DE PERIODO
              --PARA DETERMINAR SI SE DEBE GENERAR GENERA INFORMACION O NO
              SELECT *
              INTO   lr_mensualidad.*
              FROM   pen_ctr_pago_det
              WHERE  nss                  = gr_reporte.nss
            --AND    MONTH(fecha_liquida) = MONTH(ld_fecha_fin_reporte)
            --AND    YEAR(fecha_liquida)  = YEAR(ld_fecha_fin_reporte)
              AND    fecha_liquida BETWEEN ld_fecha_ini_reporte AND ld_fecha_fin_reporte
              AND    estado               = 70

              IF SQLCA.SQLCODE = NOTFOUND THEN
                 LET gc_mensaje = "NO SE LOCALIZO MENSUALIDAD LIQUIDADA PMG EN EL PERIODO INDICADO NSS ",gr_reporte.nss
                 CALL ERRORLOG(gc_mensaje CLIPPED)
                 DISPLAY gc_mensaje CLIPPED
                 CONTINUE FOREACH
              END IF

              IF (lr_mensualidad.num_mensualidad = 1) OR ( (lr_mensualidad.num_mensualidad MOD 12) = 0 ) OR
                 (lr_mensualidad.marca_ult_pago = "*" ) THEN   --CPL-2835
                 --SOLO SE GENERARÁ INFORMACION EN CASO DE MENSUALIDADES 1 12 O MULTIPLO DE 12
                 DISPLAY "GENERANDO INFORMACION PARA NSS:",lr_mensualidad.nss," MENSUALIDAD: ",lr_mensualidad.num_mensualidad," FOLIO: ",lr_mensualidad.folio_liquida USING "<<<<<<"
                 CALL saldos_pmg(ld_fecha_ini_reporte,ld_fecha_fin_reporte,lr_mensualidad.*)
                       RETURNING gr_reporte.consecutivo_envio, --folio trans
                                 li_folio                    , --folio trans
                                 ld_consec_disp                --consecutivo disp
              ELSE
                 LET gc_mensaje = "SE DESCARTA MENSUALIDAD ",lr_mensualidad.num_mensualidad USING "<<<",
                                  " YA QUE NO CORRESPONDE A UNA MENSUALIDAD 1, 12 o MULTIPLO DE 12"
                 CALL ERRORLOG(gc_mensaje CLIPPED)
                 DISPLAY gc_mensaje CLIPPED
                 CONTINUE FOREACH
              END IF

              LET gr_reporte.dia_corte = DAY(gr_reporte.afo_fecha_entrega)
              LET gr_reporte.mes_corte = MONTH(gr_reporte.afo_fecha_entrega)
      END CASE


      #detalle de la transferencia
      CALL get_datos_ret(gr_reporte.nss,  --nss
                         ld_consec_disp,  --consecutivo disp
                         li_folio       ) --folio trans

      #No lleva desglose infonavit
      IF gs_tipo_informe = 15 OR   --TRANSFERENCIA regimen 97
         gs_tipo_informe = 17 OR   --DISPOSICION   regimen 97
         gs_tipo_informe = 18 THEN --PMG #CPL-2017
         LET gr_reporte.afo_rec_entregado       = gr_reporte.afo_rec_entregado +
                                                  gr_reporte.infonavit_entregado

         LET gr_reporte.infonavit_entidad       = NULL
         LET gr_reporte.infonavit_numcta        = NULL
         LET gr_reporte.infonavit_entregado     = 0
         LET gr_reporte.infonavit_fecha_entrega = NULL
      END IF

      IF gs_afore = 568 THEN --Coppel
         #ultimo día del mes de corte
         LET gr_reporte.fecha_emision = ld_fecha_fin_reporte
         LET gr_reporte.dia_corte     = DAY(gr_reporte.fecha_emision)
         LET gr_reporte.mes_corte     = MONTH(gr_reporte.fecha_emision)
      END IF

      #Detalle 02
      OUTPUT TO REPORT rpt_edc_final(2)
      CALL despliega()

   END FOREACH

   #Sumario
   OUTPUT TO REPORT rpt_edc_final(99)

   FINISH REPORT rpt_edc_final

   #Permisos
   LET lc_comando = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_comando

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina calculo de estado de cuenta")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,gc_archivo)

   UPDATE cta_ctr_proceso
   SET    estado = 3
   WHERE  folio  = gi_folio
   AND    estado = 2
END FUNCTION
################################################################################
#*******************************************************************************
#Función : inicializa()
#Objetivo: Inicializa el record del reporte para c/Nss
#Parámetros de entrada: Ninguno
#Valores de retorno   : Ninguno
#Variables globales   : gr_reporte. Record del reporte
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION inicializa()
   LET gr_reporte.tipo_registro           = 2     --
   LET gr_reporte.consecutivo_envio       = NULL  --     --archivo
   LET gr_reporte.dia_corte               = NULL  --1    --archivo
   LET gr_reporte.mes_corte               = NULL  --2    --archivo
   LET gr_reporte.nombre_completo         = NULL  --3    --archivo
   LET gr_reporte.calle_num               = NULL  --3    --archivo
   LET gr_reporte.colonia                 = NULL  --3    --archivo
   LET gr_reporte.cp                      = NULL  --3    --archivo
   LET gr_reporte.centro_reparto          = NULL  --3    --archivo
   LET gr_reporte.entidad_federal         = NULL  --3    --archivo
   LET gr_reporte.munic_delega            = NULL  --3    --archivo
   --LET gr_reporte.nss                     =     --6    --archivo
   LET gr_reporte.rfc                     = NULL  --5    --archivo
   LET gr_reporte.curp                    = NULL  --4    --archivo
   LET gr_reporte.saldo_ini_ret           = 0     --7    --saldos
   LET gr_reporte.saldo_ini_viv           = 0     --8    --saldos
   LET gr_reporte.saldo_trans_ret         = 0     --9    --saldos
   LET gr_reporte.saldo_trans_viv         = 0     --10   --saldos
   LET gr_reporte.saldo_rem_ret           = 0     --11   --saldos
   LET gr_reporte.saldo_rem_viv           = 0     --12   --saldos
   LET gr_reporte.ley_pen                 = NULL  --13   --get_datos_ret
   LET gr_reporte.regimen                 = NULL  --14   --get_datos_ret
   LET gr_reporte.seguro                  = NULL  --15   --get_datos_ret
   LET gr_reporte.pension                 = NULL  --16   --get_datos_ret
   LET gr_reporte.pen_entidad             = NULL  --17   --get_datos_ret
   LET gr_reporte.pen_monto_trans         = 0     --18   --saldos
   LET gr_reporte.pen_fecha_trans         = NULL  --19   --saldos
   LET gr_reporte.afo_entidad             = NULL  --20   --get_datos_ret
   LET gr_reporte.afo_medio_pago          = NULL  --21   --get_datos_ret
   LET gr_reporte.afo_rec_entregado       = 0     --22   --saldos
   LET gr_reporte.afo_fecha_entrega       = NULL  --23   --saldos
   LET gr_reporte.afo_isr                 = 0     --24   --saldos
   LET gr_reporte.infonavit_entidad       = NULL  --25   --get_datos_ret
   LET gr_reporte.infonavit_numcta        = NULL  --26   --get_datos_ret
   LET gr_reporte.infonavit_entregado     = 0     --27   --saldos
   LET gr_reporte.infonavit_fecha_entrega = NULL  --28   --saldos
   LET gr_reporte.infonavit_isr           = 0     --29   --saldos
   LET gr_reporte.fecha_emision           = NULL  --HOY   --30
   LET gr_reporte.fecha_ini_pen           = NULL  --31   --get_datos_ret
   LET gr_reporte.monto_pen               = 0     --32   --get_datos_ret
   LET gr_reporte.saldo_fin               = 0     --33   --saldos
END FUNCTION
################################################################################
#*******************************************************************************
#Función : saldos
#Objetivo: Determina saldo inicial, saldo transferido y entregado, saldos de la
#          transferencia, saldos de la disposición(divididos en retiro y
#          vivienda)
#          Se emplea para nss con transferencia, busca las disposiciones del mes
#Parámetros de entrada: ld_fecha_ini_reporte
#                       ld_fecha_fin_reporte
#                       => Periodo del reporte
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.pen_fecha_trans        =>fecha liquidacion transferencia
#gr_reporte.saldo_ini_viv          =>saldo inicial de vivienda
#gr_reporte.saldo_ini_ret          =>saldo inicial de retiro
#gr_reporte.saldo_trans_viv        =>saldo transferido de vivienda
#gr_reporte.saldo_trans_ret        =>saldo transferido de retiro
#gr_reporte.pen_monto_trans        =>saldo total entregado en la transferencia
#gr_reporte.infonavit_entregado    =>saldo vivienda entregado en la disposición
#gr_reporte.afo_rec_entregado      =>saldo retiro entregado en la disposición
#gr_reporte.afo_isr                =>retención ISR en la disposición
#gr_reporte.infonavit_fecha_entrega=>fecha liquidacion disposición
#gr_reporte.afo_fecha_entrega      =>fecha liquidacion disposición
#
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldos(ld_fecha_ini_reporte, ld_fecha_fin_reporte)
   DEFINE ld_fecha_ini_reporte ,
          ld_fecha_fin_reporte DATE

   DEFINE ls_subcuenta SMALLINT     ,
          ls_siefore   SMALLINT     ,
          ld_pesos     DECIMAL(22,6),
          ld_acciones  DECIMAL(22,6)

   DEFINE ld_fecha_ini       DATE
   DEFINE ls_tipo_movimiento SMALLINT,
          l_transf           SMALLINT,
          ls_tipo_mov        SMALLINT

   DEFINE lr_consecutivo RECORD
           transferencia  DECIMAL(11,0),
           disposicion    DECIMAL(11,0)
   END RECORD

   DEFINE li_folio      INTEGER
   DEFINE ls_cuantos    SMALLINT
   DEFINE ld_fecha_disp DATE

   INITIALIZE lr_consecutivo.* TO NULL
   LET l_transf = 0

   --*******************************************************************************
   #Temporal de dis_cuenta
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
   WHENEVER ERROR STOP

   SELECT *
   FROM   dis_cuenta
   WHERE  nss = gr_reporte.nss
   INTO TEMP tmp_dis_cuenta

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(tipo_movimiento, fecha_conversion)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   #CPL-1626  INI
   #Fecha y consecutivo transferencia
   DECLARE cur_trans CURSOR FOR
   SELECT consecutivo_lote,
          fecha_conversion,
          folio

   FROM   tmp_dis_cuenta
   WHERE  tipo_movimiento IN (800,810,815,805,806)  #CPL-2297   --CPL-3046
   AND    fecha_conversion BETWEEN ld_fecha_ini_reporte AND ld_fecha_fin_reporte
   GROUP BY 1,2,3
   ORDER BY 1,2,3   #CPL-1626

   --DISPLAY "-----------------------------DISPOSICION"
   --
   --DISPLAY "ld_fecha_ini_reporte: ", ld_fecha_ini_reporte
   --DISPLAY "ld_fecha_fin_reporte: ", ld_fecha_fin_reporte

   --*******************************************************************************
   FOREACH cur_trans INTO lr_consecutivo.transferencia,
                          gr_reporte.pen_fecha_trans, li_folio

      ------------------------------
      --OBTENCION DE SALDO INICIAL
      ------------------------------
      LET ld_fecha_ini = gr_reporte.pen_fecha_trans - 1 UNITS DAY

      IF l_transf = 0 THEN  #--bandera para sacar saldo incial de la primera transferencia
          CALL saldo_dia(gr_reporte.nss, ld_fecha_ini)
          LET l_transf = 1

          DECLARE cur_saldo_ini CURSOR FOR
          SELECT subcuenta        ,
                 siefore          ,
                 monto_en_acciones,
                 monto_en_pesos
          FROM   tmp_saldo_dia
          ORDER BY 1,2

          FOREACH cur_saldo_ini INTO ls_subcuenta,
                                     ls_siefore  ,
                                     ld_acciones ,
                                     ld_pesos

             IF ls_subcuenta = 4  OR
                ls_subcuenta = 8  OR
                ls_subcuenta = 14 OR
                ls_subcuenta = 35 THEN
                LET gr_reporte.saldo_ini_viv = gr_reporte.saldo_ini_viv + ld_pesos
             ELSE
                LET gr_reporte.saldo_ini_ret = gr_reporte.saldo_ini_ret + ld_pesos
             END IF
          END FOREACH
      END IF  --bandera de la primera transferencia

      --*******************************************************************************
      #Saldo TRANSFERENCIA
      #En transferencias no hay ISR
      DECLARE cur_trasnf CURSOR FOR
      SELECT subcuenta              ,
             (monto_en_pesos) * (-1)
      FROM   tmp_dis_cuenta
      WHERE  nss              = gr_reporte.nss
      AND    consecutivo_lote = lr_consecutivo.transferencia
      AND    tipo_movimiento IN (800,810,815,805,806)  #CPL-2297  --CPL-3046

      FOREACH cur_trasnf INTO ls_subcuenta,
                             ld_pesos

        #Se Acumula saldos transferidos y entregados
        #Se divide en vivienda y el resto
        IF ls_subcuenta = 4  OR
           ls_subcuenta = 8  OR
           ls_subcuenta = 14 OR
           ls_subcuenta = 35 THEN
           LET gr_reporte.saldo_trans_viv = gr_reporte.saldo_trans_viv + ld_pesos
         ELSE
           LET gr_reporte.saldo_trans_ret = gr_reporte.saldo_trans_ret + ld_pesos
         END IF

         LET gr_reporte.pen_monto_trans = gr_reporte.pen_monto_trans + ld_pesos

      END FOREACH

   END FOREACH   --#END saldos liquidados
   #CPL-1626  FIN

   DECLARE cur_datos_disp_trans CURSOR FOR
   SELECT consecutivo_lote,
          fecha_conversion
   FROM   tmp_dis_cuenta
   WHERE  tipo_movimiento IN (820,825,830,840,850,860,880,817)
   AND    fecha_conversion BETWEEN ld_fecha_ini_reporte AND ld_fecha_fin_reporte
   GROUP BY 1,2
   ORDER BY 1,2

   FOREACH cur_datos_disp_trans INTO lr_consecutivo.disposicion,
                                     ld_fecha_disp

      --************************************************************************
      #Saldo de la DISPOSICION actual
      #Hay ISR
      DECLARE cur_disposicion CURSOR FOR
      SELECT subcuenta              ,
             tipo_movimiento        ,
             (monto_en_pesos) * (-1)
      FROM   tmp_dis_cuenta
      WHERE  nss              = gr_reporte.nss
      AND    consecutivo_lote = lr_consecutivo.disposicion
      AND    fecha_conversion = ld_fecha_disp
      AND    tipo_movimiento IN (10,820,825,830,840,850,860,880,817)

      FOREACH cur_disposicion INTO ls_subcuenta      ,
                                   ls_tipo_movimiento,
                                   ld_pesos

         #Se Acumula saldos transferidos y entregados
         #Se divide en vivienda y el resto
         IF ls_subcuenta = 4  OR
            ls_subcuenta = 8  OR
            ls_subcuenta = 14 OR
            ls_subcuenta = 35 THEN

            #No hay ISR en vivienda
            LET gr_reporte.saldo_trans_viv     = gr_reporte.saldo_trans_viv     + ld_pesos
            LET gr_reporte.infonavit_entregado = gr_reporte.infonavit_entregado + ld_pesos
         ELSE
         --   LET gr_reporte.saldo_trans_ret   = gr_reporte.saldo_trans_ret   + ld_pesos
            IF ls_tipo_movimiento <> 10 THEN
               LET gr_reporte.afo_rec_entregado = gr_reporte.afo_rec_entregado + ld_pesos
            ELSE
              LET gr_reporte.afo_isr           = gr_reporte.afo_isr           + ld_pesos
            END IF
         END IF
      END FOREACH

      LET gr_reporte.infonavit_fecha_entrega = ld_fecha_disp
      LET gr_reporte.afo_fecha_entrega       = ld_fecha_disp
   END FOREACH

   #redondear
   CALL redondea(gr_reporte.saldo_ini_ret      ) RETURNING gr_reporte.saldo_ini_ret
   CALL redondea(gr_reporte.saldo_ini_viv      ) RETURNING gr_reporte.saldo_ini_viv
   CALL redondea(gr_reporte.saldo_trans_ret    ) RETURNING gr_reporte.saldo_trans_ret
   CALL redondea(gr_reporte.saldo_trans_viv    ) RETURNING gr_reporte.saldo_trans_viv
   CALL redondea(gr_reporte.saldo_rem_ret      ) RETURNING gr_reporte.saldo_rem_ret
   CALL redondea(gr_reporte.saldo_rem_viv      ) RETURNING gr_reporte.saldo_rem_viv
   CALL redondea(gr_reporte.pen_monto_trans    ) RETURNING gr_reporte.pen_monto_trans
   CALL redondea(gr_reporte.afo_rec_entregado  ) RETURNING gr_reporte.afo_rec_entregado
   CALL redondea(gr_reporte.afo_isr            ) RETURNING gr_reporte.afo_isr
   CALL redondea(gr_reporte.infonavit_entregado) RETURNING gr_reporte.infonavit_entregado
   CALL redondea(gr_reporte.infonavit_isr      ) RETURNING gr_reporte.infonavit_isr
   CALL redondea(gr_reporte.saldo_fin          ) RETURNING gr_reporte.saldo_fin

   --*******************************************************************************
   #Saldo final
   IF gs_afore = 568 THEN --Coppel
      CALL saldo_dia(gr_reporte.nss, ld_fecha_fin_reporte)
      CALL saldo_final() RETURNING gr_reporte.saldo_rem_ret,
                                   gr_reporte.saldo_rem_viv
   ELSE
      LET gr_reporte.saldo_rem_ret = gr_reporte.saldo_ini_ret - gr_reporte.saldo_trans_ret
      LET gr_reporte.saldo_rem_viv = gr_reporte.saldo_ini_viv - gr_reporte.saldo_trans_viv
   END IF

   IF gr_reporte.saldo_rem_ret < 0 THEN
      LET gr_reporte.saldo_rem_ret = 0
   END IF

   IF gr_reporte.saldo_rem_viv < 0 THEN
      LET gr_reporte.saldo_rem_viv = 0
   END IF

   LET gr_reporte.saldo_fin     = gr_reporte.saldo_rem_ret + gr_reporte.saldo_rem_viv

   IF gr_reporte.saldo_fin < 0 THEN
      LET gr_reporte.saldo_fin = 0
   END IF

   RETURN li_folio                    ,
          li_folio                    ,
          lr_consecutivo.disposicion
END FUNCTION
################################################################################
#*******************************************************************************
#Función : saldos_disp
#Objetivo: Determina saldo inicial, saldo transferido y entregado, saldos de la
#          transferencia, saldos de la disposición(divididos en retiro y
#          vivienda)
#          Se emplea para nss sin transferencia, busca las disposiciones del mes
#Parámetros de entrada: ld_fecha_ini_reporte
#                       ld_fecha_fin_reporte
#                       => Periodo del reporte
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.saldo_ini_viv          =>saldo inicial de vivienda
#gr_reporte.saldo_ini_ret          =>saldo inicial de retiro
#gr_reporte.saldo_trans_viv        =>saldo transferido de vivienda
#gr_reporte.saldo_trans_ret        =>saldo transferido de retiro
#gr_reporte.infonavit_entregado    =>saldo vivienda entregado en la disposición
#gr_reporte.afo_rec_entregado      =>saldo retiro entregado en la disposición
#gr_reporte.afo_isr                =>retención ISR en la disposición
#gr_reporte.infonavit_fecha_entrega=>fecha liquidacion disposición
#gr_reporte.afo_fecha_entrega      =>fecha liquidacion disposición
#
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldos_disp(ld_fecha_ini_reporte, ld_fecha_fin_reporte)
   DEFINE ld_fecha_ini_reporte ,
          ld_fecha_fin_reporte DATE

   DEFINE ls_subcuenta         SMALLINT     ,
          ls_siefore           SMALLINT     ,
          ld_pesos             DECIMAL(22,6),
          ld_acciones          DECIMAL(22,6)

   DEFINE ld_fecha_ini         DATE
   DEFINE ls_tipo_movimiento   SMALLINT

   DEFINE lr_consecutivo       RECORD
          transferencia        DECIMAL(11,0),
          disposicion          DECIMAL(11,0)
          END RECORD

   DEFINE li_folio             INTEGER
   DEFINE ls_cuantos           SMALLINT
   DEFINE ld_fecha_disp        DATE
   
   DEFINE ld_consecutivo_disp  DECIMAL(11,0)
   DEFINE ls_tip_mov           SMALLINT

   INITIALIZE lr_consecutivo.* TO NULL

   --*******************************************************************************
   #Temporal de dis_cuenta
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
   WHENEVER ERROR STOP

   SELECT *
   FROM   dis_cuenta
   WHERE  nss = gr_reporte.nss
   INTO TEMP tmp_dis_cuenta

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(tipo_movimiento, fecha_conversion)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   LET ls_cuantos = 0

   DECLARE cur_datos_disp CURSOR FOR
   SELECT consecutivo_lote,
          fecha_conversion,
          tipo_movimiento
   FROM   tmp_dis_cuenta
   WHERE  tipo_movimiento IN (820,825,830,840,850,860,880,817,835) --CPL-3046
   AND    fecha_conversion BETWEEN ld_fecha_ini_reporte AND ld_fecha_fin_reporte
   GROUP BY 1,2,3
   ORDER BY 2,1

   FOREACH cur_datos_disp INTO lr_consecutivo.disposicion,
                               ld_fecha_disp,
                               ls_tip_mov

      LET gr_reporte.infonavit_fecha_entrega = ld_fecha_disp
      LET gr_reporte.afo_fecha_entrega       = ld_fecha_disp

      LET ls_cuantos = ls_cuantos + 1

      --************************************************************************
      #PRIMERA DISPOSICION
      --************************************************************************
      IF ls_cuantos = 1 THEN
         LET ld_fecha_ini = ld_fecha_disp - 1 UNITS DAY
         CALL saldo_dia(gr_reporte.nss, ld_fecha_ini)

         DECLARE cur_saldo_ini_disp CURSOR FOR
         SELECT subcuenta        ,
                siefore          ,
                monto_en_acciones,
                monto_en_pesos
         FROM   tmp_saldo_dia
         ORDER BY 1,2

         FOREACH cur_saldo_ini_disp INTO ls_subcuenta,
                                         ls_siefore  ,
                                         ld_acciones ,
                                         ld_pesos
            IF ls_subcuenta = 4  OR
               ls_subcuenta = 8  OR
               ls_subcuenta = 14 OR
               ls_subcuenta = 35 THEN
               LET gr_reporte.saldo_ini_viv = gr_reporte.saldo_ini_viv + ld_pesos
            ELSE
               LET gr_reporte.saldo_ini_ret = gr_reporte.saldo_ini_ret + ld_pesos
            END IF
         END FOREACH
      END IF
      
      IF (ls_tip_mov <> 817) THEN
      	  LET ld_consecutivo_disp = lr_consecutivo.disposicion
      END IF

      --************************************************************************
      #Saldo de la DISPOSICION actual
      #Hay ISR
      DECLARE cur_disposicion_disp CURSOR FOR
      SELECT subcuenta              ,
             tipo_movimiento        ,
             (monto_en_pesos) * (-1)
      FROM   tmp_dis_cuenta
      WHERE  nss              = gr_reporte.nss
      AND    consecutivo_lote = lr_consecutivo.disposicion
      AND    fecha_conversion = ld_fecha_disp

      FOREACH cur_disposicion_disp INTO ls_subcuenta      ,
                                        ls_tipo_movimiento,
                                        ld_pesos

         #Se Acumula saldos transferidos y entregados
         #Se divide en vivienda y el resto
         IF ls_subcuenta = 4  OR
            ls_subcuenta = 8  OR
            ls_subcuenta = 14 OR
            ls_subcuenta = 35 THEN

            #No hay ISR en vivienda
            LET gr_reporte.saldo_trans_viv     = gr_reporte.saldo_trans_viv     + ld_pesos
            LET gr_reporte.infonavit_entregado = gr_reporte.infonavit_entregado + ld_pesos
         ELSE
             LET gr_reporte.saldo_trans_ret   = gr_reporte.saldo_trans_ret   + ld_pesos

             IF ls_tipo_movimiento <> 10 THEN
                LET gr_reporte.afo_rec_entregado = gr_reporte.afo_rec_entregado + ld_pesos
             ELSE
                 LET gr_reporte.afo_isr           = gr_reporte.afo_isr           + ld_pesos
             END IF
         END IF
      END FOREACH
   END FOREACH

   #redondear
   CALL redondea(gr_reporte.saldo_ini_ret      ) RETURNING gr_reporte.saldo_ini_ret
   CALL redondea(gr_reporte.saldo_ini_viv      ) RETURNING gr_reporte.saldo_ini_viv
   CALL redondea(gr_reporte.saldo_trans_ret    ) RETURNING gr_reporte.saldo_trans_ret
   CALL redondea(gr_reporte.saldo_trans_viv    ) RETURNING gr_reporte.saldo_trans_viv
   CALL redondea(gr_reporte.saldo_rem_ret      ) RETURNING gr_reporte.saldo_rem_ret
   CALL redondea(gr_reporte.saldo_rem_viv      ) RETURNING gr_reporte.saldo_rem_viv
   CALL redondea(gr_reporte.pen_monto_trans    ) RETURNING gr_reporte.pen_monto_trans
   CALL redondea(gr_reporte.afo_rec_entregado  ) RETURNING gr_reporte.afo_rec_entregado
   CALL redondea(gr_reporte.afo_isr            ) RETURNING gr_reporte.afo_isr
   CALL redondea(gr_reporte.infonavit_entregado) RETURNING gr_reporte.infonavit_entregado
   CALL redondea(gr_reporte.infonavit_isr      ) RETURNING gr_reporte.infonavit_isr
   CALL redondea(gr_reporte.saldo_fin          ) RETURNING gr_reporte.saldo_fin

   --*******************************************************************************
   #Saldo final
   IF gs_afore = 568 THEN --Coppel
      CALL saldo_dia(gr_reporte.nss, ld_fecha_fin_reporte)
      CALL saldo_final() RETURNING gr_reporte.saldo_rem_ret,
                                   gr_reporte.saldo_rem_viv
   ELSE
      LET gr_reporte.saldo_rem_ret = gr_reporte.saldo_ini_ret - gr_reporte.saldo_trans_ret
      LET gr_reporte.saldo_rem_viv = gr_reporte.saldo_ini_viv - gr_reporte.saldo_trans_viv
   END IF

   IF gr_reporte.saldo_rem_ret < 0 THEN
      LET gr_reporte.saldo_rem_ret = 0
   END IF

   IF gr_reporte.saldo_rem_viv < 0 THEN
      LET gr_reporte.saldo_rem_viv = 0
   END IF

   LET gr_reporte.saldo_fin     = gr_reporte.saldo_rem_ret + gr_reporte.saldo_rem_viv

   IF gr_reporte.saldo_fin < 0 THEN
      LET gr_reporte.saldo_fin = 0
   END IF
   
   LET lr_consecutivo.disposicion = ld_consecutivo_disp

   RETURN lr_consecutivo.disposicion,
          li_folio                  ,
          lr_consecutivo.disposicion
END FUNCTION
################################################################################
################################################################################
#*******************************************************************************
#Función : saldos_pmg
#Objetivo: Determina saldo inicial, saldo transferido y entregado, saldos de la
#          transferencia, saldos de la disposición(divididos en retiro y
#          vivienda)
#          Se emplea para nss sin transferencia, busca retiros PMG del mes
#Parámetros de entrada: ld_fecha_ini_reporte
#                       ld_fecha_fin_reporte
#                       => Periodo del reporte
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.saldo_ini_viv          =>saldo inicial de vivienda
#gr_reporte.saldo_ini_ret          =>saldo inicial de retiro
#gr_reporte.saldo_trans_viv        =>saldo transferido de vivienda
#gr_reporte.saldo_trans_ret        =>saldo transferido de retiro
#gr_reporte.infonavit_entregado    =>saldo vivienda entregado en la disposición
#gr_reporte.afo_rec_entregado      =>saldo retiro entregado en la disposición
#gr_reporte.afo_isr                =>retención ISR en la disposición
#gr_reporte.infonavit_fecha_entrega=>fecha liquidacion disposición
#gr_reporte.afo_fecha_entrega      =>fecha liquidacion disposición
#
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldos_pmg(ld_fecha_ini_reporte, ld_fecha_fin_reporte, lr_mensualidad)

   DEFINE ld_fecha_ini_reporte DATE
   DEFINE ld_fecha_fin_reporte DATE
   DEFINE lr_mensualidad       RECORD LIKE pen_ctr_pago_det.*

   DEFINE ls_subcuenta         SMALLINT     ,
          ls_siefore           SMALLINT     ,
          ld_pesos             DECIMAL(22,6),
          ld_acciones          DECIMAL(22,6)

   DEFINE ld_fecha_ini         DATE --CPL-2167
   DEFINE ld_fecha_sdo_previo  DATE --CPL-2167
   DEFINE ls_tipo_movimiento   SMALLINT

   DEFINE lr_consecutivo       RECORD
          transferencia        DECIMAL(11,0),
          disposicion          DECIMAL(11,0)
          END RECORD

   DEFINE lr_acciones          RECORD   --Arreglo para acumular los montos en acciones
          ret_ini              DECIMAL(22,6), --sdo inicial para retiro
          ret_fin              DECIMAL(22,6), --sdo final para retiro
          trans_ret            DECIMAL(22,6), --transferido a retiro
          trans_viv            DECIMAL(22,6), --transferido a vivienda
          viv_ini              DECIMAL(22,6), --sdo inicial vivienda
          viv_fin              DECIMAL(22,6)  --sdo final vivienda
          END RECORD --CPL-2167

   DEFINE li_folio             INTEGER
   DEFINE ld_fecha_disp        DATE
 --DEFINE ld_fecha_conversion  DATE
   DEFINE lc_sql               CHAR(5000)

   -----------------------------------------------------------------------------

   INITIALIZE lr_consecutivo.* TO NULL

   LET gs_pmg                = 1 --Activa la bandera de PMG
   LET ls_subcuenta          = 0
   LET ls_siefore            = 0
   LET ld_pesos              = 0
   LET ld_acciones           = 0
   LET lr_acciones.ret_ini   = 0
   LET lr_acciones.ret_fin   = 0
   LET lr_acciones.trans_ret = 0
   LET lr_acciones.trans_viv = 0
   LET lr_acciones.viv_ini   = 0
   LET lr_acciones.viv_fin   = 0


   --GENERA TEMPORAL DE dis_cuenta
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
   WHENEVER ERROR STOP

   --Se elimina creación anterior de tabla, se genera tabla con historicos

   --GENERA TABLA TEMPORAL CON LOS MOVIMIENTOS DE dis_cuenta HISTORICOS
   --Y DE TRANSFERENCIAS   SE GENERA <<<tmp_dis_cuenta>>>

 --CALL f_pmg_tmp_dis_cta(gr_reporte.nss) --CPL-2167
   CALL f_pmg_tmp_dis_cta2(gr_reporte.nss) --cpl-2835


   ----OBTIENE LA FECHA DE LIQUIDACION DE MENSUALIDAD 1 PMG
   CALL f_obten_fec_liq_pmg(gr_reporte.nss) RETURNING ld_fecha_ini                                  --CPL-2835

   --GENERA LA FECHA PREVIA A LA LIQUIDACIÓN DE PMG
   LET ld_fecha_sdo_previo = ld_fecha_ini - 1 UNITS DAY

   DISPLAY "\nLiquidacion de primer pago de contrato:"                                              --CPL-2835
   DISPLAY "\tnss                  : ", gr_reporte.nss
   DISPLAY "\tmensualidad actual   : ", lr_mensualidad.num_mensualidad
   DISPLAY "\tsecuencia contrato   : ", lr_mensualidad.sec_contrato
   DISPLAY "\tld_fecha_ini(1er liq): ", ld_fecha_ini         USING "dd-mm-yyyy"
   DISPLAY "\tfecha saldo previo   : ", ld_fecha_sdo_previo  USING "dd-mm-yyyy"
   DISPLAY "\tFecha Ini reporte    : ", ld_fecha_ini_reporte USING "dd-mm-yyyy"
   DISPLAY "\tFecha Fin reporte    : ", ld_fecha_fin_reporte USING "dd-mm-yyyy"
   DISPLAY "\n"

   -----------------------------------------------------------------------------
   -- OBTIENE SALDO PREVIO AL PRIMER RETIRO
   -----------------------------------------------------------------------------
   CALL f_saldo_pmg_his(gr_reporte.nss,ld_fecha_sdo_previo) RETURNING lr_acciones.ret_ini     ,
                                                                      lr_acciones.viv_ini     ,
                                                                      gr_reporte.saldo_ini_ret,
                                                                      gr_reporte.saldo_ini_viv
   DISPLAY "\tFecha saldo previo       : ", ld_fecha_sdo_previo
   DISPLAY "\tSaldo previo RCV acciones: ", lr_acciones.ret_ini
   DISPLAY "\tSaldo previo VIV acciones: ", lr_acciones.viv_ini
   DISPLAY "\tSaldo previo RCV pesos   : ", gr_reporte.saldo_ini_ret
   DISPLAY "\tSaldo previo VIV pesos   : ", gr_reporte.saldo_ini_viv

   --Obtiene el saldo previo a la liquidacion y lo genera en <<<tmp_saldo_dia>>>

   CALL saldo_dia(gr_reporte.nss, ld_fecha_sdo_previo)
{
   ------------------------------------------------------------------------------------------
   --RECUPERA SALDO DE TODAS LAS SUBCUENTAS DE LA CUENTA AL INICIO DEL CONTRATO   --CPL-2835
   ------------------------------------------------------------------------------------------
   LET gr_reporte.saldo_ini_ret = 0
   LET gr_reporte.saldo_ini_viv = 0

   --RECUPERACION DE SALDOS

   DISPLAY "RECUPERANDO SALDOS..."

   DECLARE cur_saldo_ini_pmg CURSOR FOR
   SELECT subcuenta        ,
          siefore          ,
          monto_en_acciones,
          monto_en_pesos
   FROM   tmp_saldo_dia
   WHERE  subcuenta IN (1,2,5,6,9,4) --CPL-2167
   ORDER  BY 1,2
unload TO "paso" SELECT * FROM tmp_saldo_dia

   FOREACH cur_saldo_ini_pmg INTO ls_subcuenta,
                                  ls_siefore  ,
                                  ld_acciones ,
                                  ld_pesos
       DISPLAY ">>>", ls_subcuenta,ls_siefore  ,ld_acciones ,ld_pesos

       --CPL-2167 Se agrupa solo x subcta viv 4 el resto NO forman parte de PMG
       IF ls_subcuenta = 4 THEN
          LET gr_reporte.saldo_ini_viv = gr_reporte.saldo_ini_viv + ld_pesos
          LET lr_acciones.viv_ini      = lr_acciones.viv_ini + ld_acciones --acumula acciones
       ELSE
          LET gr_reporte.saldo_ini_ret = gr_reporte.saldo_ini_ret + ld_pesos
          LET lr_acciones.ret_ini      = lr_acciones.ret_ini + ld_acciones
       END IF

       --Limpiar variables
       LET ls_subcuenta = 0
       LET ls_siefore   = 0
       LET ld_acciones  = 0
       LET ld_pesos     = 0

   END FOREACH
}

   --Limpiar variables
   LET ls_subcuenta = 0
   LET ls_siefore   = 0
   LET ld_acciones  = 0
   LET ld_pesos     = 0

   -------------------------------------------------------------------
   --SE OBTIENE MONTO TRANSFERIDO (PAGADO) EN EL PERIODO INDICADO
   -------------------------------------------------------------------

   DISPLAY "\nRECUPERANDO MONTOS PAGADOS..."

   WHENEVER ERROR CONTINUE

   LET gr_reporte.saldo_trans_ret = 0
   LET gr_reporte.saldo_trans_viv = 0


   LET lc_sql = "\n SELECT subcuenta          ,                              ",
                "\n        tipo_movimiento    ,                              ",
                "\n        ABS(pesos)         ,                              ",
                "\n        ABS(acciones)                                     ",
                "\n FROM  (SELECT subcuenta                      ,           ",
                "\n               tipo_movimiento                ,           ",
                "\n               SUM(monto_en_pesos)    pesos   ,           ",
                "\n               SUM(monto_en_acciones) acciones            ",
                "\n        FROM   tmp_dis_cuenta                             ",
                "\n        WHERE  nss              = '",lr_mensualidad.nss,"'",
                "\n        AND    fecha_conversion BETWEEN '",ld_fecha_ini,"'",
                "\n                               AND '",ld_fecha_fin_reporte,"'", --CPL-2167  --CPL-2835
                "\n        AND                                               ",
                "\n        (                                                 ",   --SUBCUENTAS PMG
                "\n           (  subcuenta IN(1,2,5,6,9) AND                 ",   --SUBCUENTAS PMG
                "\n               tipo_movimiento  IN (841,620)              ",
                "\n           )                                              ",
                "\n           OR                                             ",
                "\n           (  subcuenta IN(7) AND                         ",
                "\n              tipo_movimiento BETWEEN 800 AND 899 OR      ",
                "\n             (tipo_movimiento = 10 AND                    ",
                "\n              folio IN (SELECT folio                      ",
                "\n                        FROM   tmp_dis_cuenta             ",
                "\n                        WHERE  fecha_conversion BETWEEN '",ld_fecha_ini,"'",
                "\n                               AND '",ld_fecha_fin_reporte,"'", --CPL-2167  --CPL-2835
                "\n                        AND    tipo_movimiento  BETWEEN 800 AND 899 ",
                "\n                        AND    subcuenta IN(7)            ",
                "\n                       )                                  ",
                "\n             )                                            ",
                "\n           )                                              ",
                "\n        )                                                 ",
                "\n        GROUP BY 1,2                                      ",
                "\n        UNION ALL  -----------------------------------    ",
                "\n        SELECT subcuenta                      ,           ",
                "\n               tipo_movimiento                ,           ",
                "\n               SUM(monto_en_pesos)    pesos   ,           ",
                "\n               SUM(monto_en_acciones) acciones            ",
                "\n        FROM   tmp_dis_cuenta                             ",
                "\n        WHERE  nss              = '",lr_mensualidad.nss,"'",
                "\n        AND    fecha_conversion BETWEEN '",ld_fecha_ini,"'",
                "\n                               AND '",ld_fecha_fin_reporte,"'", --CPL-2167  --CPL-2835
                "\n        AND    subcuenta IN(4,8)                          ",
                "\n        AND (  tipo_movimiento  BETWEEN 800 and 899 OR    ",
                "\n               tipo_movimiento = 620  OR                  ",
                "\n              (tipo_movimiento  = 10 AND                  ",
                "\n               folio IN (SELECT folio                     ",
                "\n                         FROM   tmp_dis_cuenta            ",
                "\n                         WHERE  fecha_conversion BETWEEN '",ld_fecha_ini,"'",
                "\n                                    AND '",ld_fecha_fin_reporte,"'", --CPL-2167  --CPL-2835
                "\n                         AND    tipo_movimiento  BETWEEN 800 AND 899 ",
                "\n                         AND    subcuenta IN(4,8)         ",
                "\n                        )                                 ",
                "\n              )                                           ",
                "\n            )                                             ",
                "\n        GROUP BY 1,2                                      ",
                "\n        )                                                 "

   PREPARE exe_transf_pmg FROM lc_sql

   IF sqlca.sqlcode < 0 THEN
      DISPLAY "ERROR AL PREPARAR INSTRUCCION:"
      DISPLAY lc_sql CLIPPED
      EXIT PROGRAM
   END IF

   DECLARE cur_transf_pmg CURSOR FOR exe_transf_pmg

   FOREACH cur_transf_pmg INTO ls_subcuenta          ,
                               ls_tipo_movimiento ,
                             --ld_fecha_conversion,
                               ld_pesos           ,
                               ld_acciones          --CPL-2167

         #Se Acumula saldos transferidos y entregados
         #Se divide en vivienda y el resto
         --CPL-2167 Se agrupa solo x subcta viv 4 el resto NO forman parte de PMG
         IF ls_subcuenta = 4 OR ls_subcuenta = 8 THEN
            #No hay ISR en vivienda
            IF ls_tipo_movimiento = 620 THEN
               LET gr_reporte.saldo_trans_viv     = gr_reporte.saldo_trans_viv     - ld_pesos
               LET lr_acciones.trans_viv          = lr_acciones.trans_viv          - ld_acciones
               LET gr_reporte.infonavit_entregado = gr_reporte.infonavit_entregado - ld_pesos
            ELSE
               LET gr_reporte.saldo_trans_viv     = gr_reporte.saldo_trans_viv     + ld_pesos
               LET lr_acciones.trans_viv          = lr_acciones.trans_viv          + ld_acciones
               LET gr_reporte.infonavit_entregado = gr_reporte.infonavit_entregado + ld_pesos
            END IF

         ELSE
            DISPLAY "\tAcumulando Subcuenta: ",ls_subcuenta," Pesos: ",  ld_pesos
            IF ls_tipo_movimiento = 620 THEN
               LET gr_reporte.saldo_trans_ret   = gr_reporte.saldo_trans_ret   - ld_pesos
               LET lr_acciones.trans_ret        = lr_acciones.trans_ret        - ld_acciones
            ELSE
               LET gr_reporte.saldo_trans_ret   = gr_reporte.saldo_trans_ret   + ld_pesos
               LET lr_acciones.trans_ret        = lr_acciones.trans_ret        + ld_acciones
            END IF


            IF ls_tipo_movimiento <> 10 THEN
               LET gr_reporte.afo_rec_entregado = gr_reporte.afo_rec_entregado + ld_pesos
            ELSE
                LET gr_reporte.afo_isr           = gr_reporte.afo_isr           + ld_pesos
            END IF
         END IF

   END FOREACH

   DISPLAY "ACUMULADO PAGADO RET:",gr_reporte.saldo_trans_ret
   DISPLAY "ACUMULADO PAGADO VIV:",gr_reporte.saldo_trans_viv

   --RECUPERA LA FECHA DE LIQUIDACION
   DECLARE cur_datos_pmg CURSOR FOR
   SELECT consecutivo_lote,
          fecha_conversion
   FROM   tmp_dis_cuenta
   WHERE  tipo_movimiento IN (841)
   AND    fecha_conversion BETWEEN ld_fecha_ini AND ld_fecha_fin_reporte --CPL-2167
   GROUP  BY 1,2
   ORDER  BY 2,1

   FOREACH cur_datos_pmg INTO lr_consecutivo.disposicion,   --CONSECUTIVO
                              ld_fecha_disp

     LET gr_reporte.infonavit_fecha_entrega = ld_fecha_disp
     LET gr_reporte.afo_fecha_entrega       = ld_fecha_disp
   END FOREACH

   #redondear
   CALL redondea(gr_reporte.saldo_ini_ret      ) RETURNING gr_reporte.saldo_ini_ret
   CALL redondea(gr_reporte.saldo_ini_viv      ) RETURNING gr_reporte.saldo_ini_viv
   CALL redondea(gr_reporte.saldo_trans_ret    ) RETURNING gr_reporte.saldo_trans_ret
   CALL redondea(gr_reporte.saldo_trans_viv    ) RETURNING gr_reporte.saldo_trans_viv
   CALL redondea(gr_reporte.pen_monto_trans    ) RETURNING gr_reporte.pen_monto_trans
   CALL redondea(gr_reporte.afo_rec_entregado  ) RETURNING gr_reporte.afo_rec_entregado
   CALL redondea(gr_reporte.afo_isr            ) RETURNING gr_reporte.afo_isr
   CALL redondea(gr_reporte.infonavit_entregado) RETURNING gr_reporte.infonavit_entregado
   CALL redondea(gr_reporte.infonavit_isr      ) RETURNING gr_reporte.infonavit_isr
   CALL redondea(gr_reporte.saldo_fin          ) RETURNING gr_reporte.saldo_fin

   -----------------------------------------------------------------------------
   --SE CALCULA SALDO REMANENTES (A LA FECHA FIN DE REPORTE)
   -----------------------------------------------------------------------------
   IF gs_afore = 568 THEN --Coppel
      --CPL-2167 El saldo final para PMG sera la diferencia entre saldo incial y saldos transferidos
      --CPL-2167 Pero será en acciones, que se deben valuar a la fecha de ejecución del programa
      --CPL-2835 se modica calculo de remanente por "calculo" y no por diferencia


      CALL f_saldo_pmg_his(gr_reporte.nss,ld_fecha_fin_reporte)
           RETURNING lr_acciones.ret_ini     ,
                     lr_acciones.viv_ini     ,
                     gr_reporte.saldo_rem_ret,
                     gr_reporte.saldo_rem_viv


      --LET lr_acciones.ret_fin = lr_acciones.ret_ini - lr_acciones.trans_ret
      --LET lr_acciones.viv_fin = lr_acciones.viv_ini - lr_acciones.trans_viv

      DISPLAY ""
      DISPLAY "\tAcciones remanentes rcv: ",lr_acciones.ret_fin
      DISPLAY "\tAcciones remanentes viv: ",lr_acciones.viv_fin

      --CALL f_valua_remanente(lr_acciones.ret_fin,lr_acciones.viv_fin)
      --     RETURNING gr_reporte.saldo_rem_ret,
      --               gr_reporte.saldo_rem_viv

      --CALL f_valua_remanente_pmg(gr_reporte.nss,lr_acciones.ret_fin,lr_acciones.viv_fin,ld_fecha_fin_reporte)
      --     RETURNING gr_reporte.saldo_rem_ret,
      --               gr_reporte.saldo_rem_viv


      DISPLAY ""
      DISPLAY "\tSaldo remanente rcv: ", gr_reporte.saldo_rem_ret
      DISPLAY "\tSaldo remanente viv: ", gr_reporte.saldo_rem_viv

      CALL redondea(gr_reporte.saldo_rem_ret      ) RETURNING gr_reporte.saldo_rem_ret
      CALL redondea(gr_reporte.saldo_rem_viv      ) RETURNING gr_reporte.saldo_rem_viv
   ELSE
      LET gr_reporte.saldo_rem_ret = gr_reporte.saldo_ini_ret - gr_reporte.saldo_trans_ret
      LET gr_reporte.saldo_rem_viv = gr_reporte.saldo_ini_viv - gr_reporte.saldo_trans_viv
   END IF

   IF (gr_reporte.saldo_rem_ret < 0) OR (gr_reporte.saldo_rem_ret IS NULL) THEN
      LET gr_reporte.saldo_rem_ret = 0
   END IF

   IF (gr_reporte.saldo_rem_viv < 0) OR (gr_reporte.saldo_rem_viv IS NULL) THEN
      LET gr_reporte.saldo_rem_viv = 0
   END IF

   LET gr_reporte.saldo_fin     = gr_reporte.saldo_rem_ret + gr_reporte.saldo_rem_viv

   IF (gr_reporte.saldo_fin < 0) OR (gr_reporte.saldo_fin IS NULL) THEN
      LET gr_reporte.saldo_fin = 0
   END IF

   LET gs_pmg = 0 --Desactiva la bandera de PMG

   RETURN lr_consecutivo.disposicion,
          li_folio                  ,
          lr_consecutivo.disposicion

END FUNCTION
################################################################################
#*******************************************************************************
#Función : saldo_dia
#Objetivo: Valua el saldo al día del nss
#Parámetros de entrada: lc_nss         => Nss Trabajador
#                       ld_fecha_saldo => Fecha a la que se obtiene el saldo
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldo_dia(lc_nss, ld_fecha_saldo)
   DEFINE lc_nss           CHAR(11),
          ld_fecha_saldo   DATE

   DEFINE ls_num_siefore_g SMALLINT      ,
          ls_num_siefore_t SMALLINT      ,
          ls_num_siefore_v SMALLINT      ,
          ld_fecha_viv     DATE

   DEFINE ld_acciones_nominal,
          ld_acciones_real   ,
          ld_pesos_real       DECIMAL(22,6)

   DEFINE lc_paso CHAR(100)
   DEFINE ls_cont_sief        SMALLINT,
         ls_query            CHAR(1000)

   LET gc_mensaje = "CALCULANDO SALDO AL DIA: ", ld_fecha_saldo USING "DD/MM/YYYY"

   DISPLAY gc_mensaje CLIPPED
   CALL ERRORLOG(gc_mensaje CLIPPED)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldo
      DROP TABLE tmp_saldo_dia
   WHENEVER ERROR STOP

   LET ld_fecha_viv = MDY(MONTH(ld_fecha_saldo),1,YEAR(ld_fecha_saldo))

   #Verificar que existan precios
   SELECT DISTINCT "X"
   FROM   glo_valor_accion
   WHERE  codigo_siefore  = 0
   AND    fecha_valuacion = ld_fecha_saldo

   IF SQLCA.SQLCODE = NOTFOUND THEN
      INSERT INTO glo_valor_accion (codigo_siefore,
                                    precio_del_dia,
                                    fecha_valuacion)
      VALUES (0,0,ld_fecha_saldo)
   END IF
      SELECT subcuenta                               ,
          siefore                                 ,
          SUM(monto_en_acciones) monto_en_acciones,
          SUM(monto_en_pesos)    monto_en_pesos
   FROM   tmp_dis_cuenta
   WHERE  fecha_conversion <= ld_fecha_saldo
   GROUP BY 1,2
   ORDER BY 1,2
   INTO TEMP tmp_saldo

    LET ls_query =
    "SELECT COUNT (*) ",                             
    "FROM (SELECT siefore ",                         
    "FROM tmp_saldo ",                               
    "WHERE siefore NOT IN (11) ",                    
    "AND siefore NOT IN (SELECT codigo_siefore   ",  
    "FROM glo_valor_accion                       ",  
    "WHERE fecha_valuacion = ?)     ",  
    "AND monto_en_acciones <> 0                  ",  
    "UNION                                       ",  
    "SELECT siefore                              ",  
    "FROM tmp_saldo                              ",  
    "WHERE siefore IN (11)                       ",  
    "AND siefore NOT IN (SELECT codigo_siefore   ",  
    "FROM glo_valor_accion                       ",  
    "WHERE fecha_valuacion = ?)       ",  
    "AND monto_en_acciones <> 0 )                " 
    
     PREPARE prp_con_sief FROM ls_query 
     EXECUTE prp_con_sief INTO ls_cont_sief
                          USING ld_fecha_saldo, ld_fecha_viv

   --IF ls_num_siefore_t <>  ls_num_siefore_g THEN
    IF ls_cont_sief > 0 THEN
      LET gc_mensaje = "\nFaltan precios:",
                       "\nVerifique siefores y precios de accion: ",ld_fecha_saldo USING "dd/mm/yyyy",
                       " Vivienda: ",ld_fecha_viv USING "dd/mm/yyyy"
      DISPLAY gc_mensaje CLIPPED
      CALL ERRORLOG(gc_mensaje CLIPPED)
   END IF

   sql
   SELECT subcuenta        ,
          siefore          ,
          monto_en_acciones,
          CASE WHEN siefore = 0 THEN
                   monto_en_pesos
               ELSE
                   monto_en_acciones * precio_del_dia
           END monto_en_pesos
   FROM  tmp_saldo       ,
         glo_valor_accion
   WHERE subcuenta NOT IN (4,8,14,35)
   AND   siefore         = codigo_siefore
   AND   fecha_valuacion = $ld_fecha_saldo
   UNION 
   SELECT subcuenta                         ,
          siefore                           ,
          monto_en_acciones                 ,
          monto_en_acciones * precio_del_dia monto_en_pesos
   FROM   tmp_saldo       ,
          glo_valor_accion
   WHERE  subcuenta IN (4,8,14,35)
   AND    siefore         = codigo_siefore
   AND    fecha_valuacion = $ld_fecha_viv
   INTO TEMP tmp_saldo_dia
   END sql

   SELECT monto_en_acciones
   INTO   ld_acciones_nominal
   FROM   tmp_saldo_dia
   WHERE  subcuenta = 36

   IF ld_acciones_nominal IS NULL THEN
       LET ld_acciones_nominal = 0
   END IF

   #Valuar bono de pension
   IF ld_acciones_nominal > 0 THEN
      EXECUTE get_bono_valuado USING gr_reporte.nss     ,
                                      ld_acciones_nominal,
                                      gr_reporte.pen_fecha_trans
                                INTO  ld_acciones_real,
                                     ld_pesos_real

      UPDATE tmp_saldo_dia
      SET    monto_en_pesos = ld_pesos_real
      WHERE  subcuenta = 36
   END IF

END FUNCTION
################################################################################
#*******************************************************************************
#Función : saldo_final
#Objetivo: Determina saldo final del trabajador dividido en retiro y vivienda
#Parámetros de entrada: Ninguno
#Valores de retorno   : ld_saldo_fin_ret => saldo final retiro
#                       ld_saldo_fin_viv => saldo final vivienda
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION saldo_final()
   DEFINE ls_subcuenta SMALLINT     ,
          ls_siefore   SMALLINT     ,
          ld_pesos     DECIMAL(22,6),
          ld_acciones  DECIMAL(22,6)

   DEFINE ld_saldo_fin_ret,
          ld_saldo_fin_viv DECIMAL(22,6)

   DECLARE cur_saldo_fin CURSOR FOR
   SELECT subcuenta        ,
          siefore          ,
          monto_en_acciones,
          monto_en_pesos
   FROM   tmp_saldo_dia
   ORDER BY 1,2

   LET ld_saldo_fin_ret = 0
   LET ld_saldo_fin_viv = 0

   FOREACH cur_saldo_fin INTO ls_subcuenta,
                              ls_siefore  ,
                              ld_acciones ,
                              ld_pesos
         IF ls_subcuenta = 4  OR
            ls_subcuenta = 8  OR
            ls_subcuenta = 14 OR
            ls_subcuenta = 35 THEN
            LET ld_saldo_fin_viv = ld_saldo_fin_viv + ld_pesos
         ELSE
            LET ld_saldo_fin_ret = ld_saldo_fin_ret + ld_pesos
         END IF
   END FOREACH

   #redondear
   LET ld_saldo_fin_ret = redondea(ld_saldo_fin_ret)
   LET ld_saldo_fin_viv = redondea(ld_saldo_fin_viv)

   RETURN ld_saldo_fin_ret,
          ld_saldo_fin_viv
END FUNCTION
################################################################################
#*******************************************************************************
#Función : get_datos_ret
#Objetivo: Obtiene los detalles datamart de la transferencia y disposicion
#Parámetros de entrada: lc_nss              => Nss trabajador
#                       ld_consecutivo_disp => Consecutivo disposición
#                       li_folio_trans      => Folio liquidación de transferencia
#Valores de retorno   : Ninguno
#Variables globales   :
#gr_reporte.ley_pen          => Ley pension
#gr_reporte.regimen          => Regimen
#gr_reporte.seguro           => Tipo seguro
#gr_reporte.pension          => Tipo pensión
#gr_reporte.pen_entidad      => Entidad a que se transfiere saldos transferencia
#gr_reporte.fecha_ini_pen    => Fecha inicio de pensión
#gr_reporte.fecha_emision    => Fecha resolución
#gr_reporte.monto_pen        => Monto constitutivo
#gr_reporte.infonavit_numcta => Número cuenta infonavit
#gr_reporte.afo_entidad      => Entidad a que se transfiere saldos retiro
#                               disposición
#gr_reporte.afo_medio_pago   => Medio pago de la disposición
#gr_reporte.infonavit_entidad=> Entidad a que se transfiere saldos vivienda
#                               disposición
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION get_datos_ret(lc_nss, ld_consecutivo_disp, li_folio_trans)
   DEFINE lc_nss              CHAR(11)     ,
          ld_consecutivo_disp DECIMAL(11,0),
          li_folio_trans      INTEGER

   DEFINE lc_num_cuenta,
          lc_clabe       CHAR(20)
   DEFINE lc_cve_destino CHAR(08)

   CASE
      WHEN gs_tipo_informe = 14 OR   --TRANSFERENCIA
           gs_tipo_informe = 15      --TRANSFERENCIA

           EXECUTE get_pension_trans USING lc_nss,
                                           li_folio_trans
                                     INTO  gr_reporte.ley_pen      ,
                                           gr_reporte.regimen      ,
                                           gr_reporte.seguro       ,
                                           gr_reporte.pension      ,
                                           gr_reporte.pen_entidad  ,
                                           lc_cve_destino          ,
                                           gr_reporte.fecha_ini_pen,
                                           gr_reporte.fecha_emision,
                                           gr_reporte.monto_pen
           #CPL-1626 INI  -busca historico
           IF SQLCA.SQLCODE = 100 THEN #-- NO ENCUENTRA NADA

              EXECUTE get_pension_trans2 USING lc_nss,
                                              li_folio_trans
                                        INTO  gr_reporte.ley_pen      ,
                                              gr_reporte.regimen      ,
                                              gr_reporte.seguro       ,
                                              gr_reporte.pension      ,
                                              gr_reporte.pen_entidad  ,
                                              lc_cve_destino          ,
                                              gr_reporte.fecha_ini_pen,
                                              gr_reporte.fecha_emision,
                                              gr_reporte.monto_pen

           END IF

      WHEN gs_tipo_informe = 16 OR   --DISPOSICION
           gs_tipo_informe = 17      --DISPOSICION

           IF gs_afore = 564 THEN --MLM
              EXECUTE get_pension_disp_mlm USING lc_nss,
                                                 ld_consecutivo_disp
                                           INTO  gr_reporte.ley_pen      ,
                                                 gr_reporte.regimen      ,
                                                 gr_reporte.seguro       ,
                                                 gr_reporte.pension      ,
                                                 gr_reporte.pen_entidad  ,
                                                 lc_cve_destino          ,
                                                 gr_reporte.fecha_ini_pen,
                                                 gr_reporte.fecha_emision,
                                                 gr_reporte.monto_pen
           ELSE
              EXECUTE get_pension_disp USING lc_nss,
                                             ld_consecutivo_disp
                                       INTO  gr_reporte.ley_pen      ,
                                             gr_reporte.regimen      ,
                                             gr_reporte.seguro       ,
                                             gr_reporte.pension      ,
                                             gr_reporte.pen_entidad  ,
                                             lc_cve_destino          ,
                                             gr_reporte.fecha_ini_pen,
                                             gr_reporte.fecha_emision--,
                                             --gr_reporte.monto_pen

              {IF SQLCA.SQLCODE = 100 THEN #-- NO ENCUENTRA NADA
                 EXECUTE get_pension_disp2 USING lc_nss,
                                                 ld_consecutivo_disp
                                           INTO  gr_reporte.ley_pen      ,
                                                 gr_reporte.regimen      ,
                                                 gr_reporte.seguro       ,
                                                 gr_reporte.pension      ,
                                                 gr_reporte.pen_entidad  ,
                                                 lc_cve_destino          ,
                                                 gr_reporte.fecha_ini_pen,
                                                 gr_reporte.fecha_emision,
                                                 gr_reporte.monto_pen
              END IF #CPL-1626 FIN}
           END IF

      #CPL-2017
      WHEN gs_tipo_informe = 18 --PMG
           EXECUTE get_pension_pmg USING lc_nss,
                                          ld_consecutivo_disp
                                    INTO  gr_reporte.ley_pen      ,
                                          gr_reporte.regimen      ,
                                          gr_reporte.seguro       ,
                                          gr_reporte.pension      ,
                                          gr_reporte.pen_entidad  ,
                                          lc_cve_destino          ,
                                          gr_reporte.fecha_ini_pen,
                                          gr_reporte.fecha_emision,
                                          gr_reporte.monto_pen

           IF SQLCA.SQLCODE = 100 THEN #-- NO ENCUENTRA NADA
              EXECUTE get_pension_pmg2 USING lc_nss,
                                              ld_consecutivo_disp
                                        INTO  gr_reporte.ley_pen      ,
                                              gr_reporte.regimen      ,
                                              gr_reporte.seguro       ,
                                              gr_reporte.pension      ,
                                              gr_reporte.pen_entidad  ,
                                              lc_cve_destino          ,
                                              gr_reporte.fecha_ini_pen,
                                              gr_reporte.fecha_emision,
                                              gr_reporte.monto_pen
           END IF
   END CASE

   IF ld_consecutivo_disp IS NOT NULL THEN
      #Medio de pago y cuenta bancaria
      CASE gs_afore
         WHEN 564 --MLM
            sql
            SELECT trim(a.num_cuenta),
                   trim(a.clabe)     ,
                   b.descripcion
            INTO   $lc_num_cuenta            ,
                   $lc_clabe                 ,
                   $gr_reporte.afo_medio_pago
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql

            IF lc_clabe IS NOT NULL THEN
               LET gr_reporte.infonavit_numcta = lc_clabe
            ELSE
               LET gr_reporte.infonavit_numcta = lc_num_cuenta
            END IF

            LET gr_reporte.fecha_emision = TODAY

         WHEN 568 --Coppel
            sql
             SELECT trim(a.num_cuenta)     ,
                   trim(b.descripcion[9,40]) tipo_pago
            INTO   $gr_reporte.infonavit_numcta,
                   $gr_reporte.afo_entidad
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql

            IF gr_reporte.afo_entidad = 'BANCOMER' THEN
               LET gr_reporte.afo_entidad = 'OTROS BANCOS'
            END IF

            CASE
               WHEN gr_reporte.afo_entidad = 'BANCOPPEL'    OR
                    gr_reporte.afo_entidad = 'OTROS BANCOS'

                  LET gr_reporte.afo_medio_pago = 'TRANSFERENCIA'

               WHEN gr_reporte.afo_entidad = 'CAJAS COPPEL'

                  LET gr_reporte.afo_medio_pago = 'VENTANILLA'
            END CASE

            LET gr_reporte.infonavit_entidad = 'BANCO'

         OTHERWISE
            sql
             SELECT trim(a.num_cuenta)     ,
                   b.descripcion tipo_pago
            INTO   $gr_reporte.infonavit_numcta,
                   $gr_reporte.afo_medio_pago
            FROM   ret_beneficiario a,
                   OUTER tab_pago   b
            WHERE  a.nss         = $lc_nss
            AND    a.consecutivo = $ld_consecutivo_disp
            AND    a.tipo_pago   = b.tipo_pago
            END sql
      END CASE

      IF gs_afore = 562 OR    --Invercap
         gs_afore = 564 THEN  --MLM

          LET gr_reporte.afo_entidad       = gr_reporte.pen_entidad
          LET gr_reporte.infonavit_entidad = gr_reporte.pen_entidad
      END IF
   END IF

   IF gs_afore = 568 THEN--Coppel
      #CPL-1099
      #Todo caso que no cuente con una resolución
      #debe ser considerado como regimen 97
      IF gr_reporte.regimen IS NULL THEN
         LET gr_reporte.ley_pen = 'LEY 97'
         LET gr_reporte.regimen = '97'
      END IF
   END IF
END FUNCTION
################################################################################
#*******************************************************************************
#Función : rpt_edc_final
#Objetivo: Reporte de archivo plano de estado de cuenta de pensionados
#Parámetros de entrada: ls_tipo_registro => tipo de registro a imprimir
#                                           encabezado, detalle, sumario
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
REPORT rpt_edc_final(ls_tipo_registro)
   DEFINE ls_tipo_registro SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      CASE ls_tipo_registro
         WHEN 1  --Encabezado
            PRINT COLUMN 001, '01'                            ,
                  COLUMN 003, '30'                            ,
                  COLUMN 005, '01'                            ,
                  COLUMN 007, gs_afore        USING "&&&"     ,
                  COLUMN 010, '15'                            ,
                  COLUMN 012, hoy             USING "DDMMYYYY",
                  COLUMN 020, '001'                           ,
                  COLUMN 023, gs_tipo_informe USING "&&"

         WHEN 2 --Detalle
             PRINT COLUMN 001, gr_reporte.tipo_registro                     USING "&&"           ,  --01
                   COLUMN 003, gr_reporte.consecutivo_envio                 USING "&&&&&&&&&&&"  ,  --02
                   COLUMN 014, gr_reporte.dia_corte                         USING "&&"           ,  --03
                   COLUMN 016, gr_reporte.mes_corte                         USING "&&"           ,  --04
                   COLUMN 018, gr_reporte.nombre_completo                   CLIPPED              ,  --05
                   COLUMN 138, gr_reporte.calle_num                         CLIPPED              ,  --06
                   COLUMN 200, gr_reporte.colonia                           CLIPPED              ,  --07
                   COLUMN 260, gr_reporte.cp                                CLIPPED              ,  --08
                   COLUMN 265, gr_reporte.centro_reparto                    CLIPPED              ,  --09
                   COLUMN 270, gr_reporte.entidad_federal                   CLIPPED              ,  --10
                   COLUMN 310, gr_reporte.munic_delega                      CLIPPED              ,  --11
                   COLUMN 350, gr_reporte.nss                               CLIPPED              ,  --12
                   COLUMN 361, gr_reporte.rfc                               CLIPPED              ,  --13
                   COLUMN 374, gr_reporte.curp                              CLIPPED              ,  --14
                   COLUMN 392, gr_reporte.saldo_ini_ret                     USING "&&&&&&&&&&.&&",  --15
                   COLUMN 405, gr_reporte.saldo_ini_viv                     USING "&&&&&&&&&&.&&",  --16
                   COLUMN 418, gr_reporte.saldo_trans_ret                   USING "&&&&&&&&&&.&&",  --17
                   COLUMN 431, gr_reporte.saldo_trans_viv                   USING "&&&&&&&&&&.&&",  --18
                   COLUMN 444, gr_reporte.saldo_rem_ret                     USING "&&&&&&&&&&.&&",  --19
                   COLUMN 457, gr_reporte.saldo_rem_viv                     USING "&&&&&&&&&&.&&",  --20
                   COLUMN 470, gr_reporte.ley_pen                           CLIPPED              ,  --21
                   COLUMN 476, gr_reporte.regimen                           CLIPPED              ,  --22
                   COLUMN 478, gr_reporte.seguro                            CLIPPED              ,  --23
                   COLUMN 480, gr_reporte.pension                           CLIPPED              ,  --24
                   COLUMN 482, gr_reporte.pen_entidad                       CLIPPED              ,  --25
                   COLUMN 532, gr_reporte.pen_monto_trans                   USING "&&&&&&&&&&.&&",  --26
                   COLUMN 545, gr_reporte.pen_fecha_trans                   USING "DDMMYYYY"     ,  --27
                   COLUMN 553, gr_reporte.afo_entidad                       CLIPPED              ,  --28
                   COLUMN 603, gr_reporte.afo_medio_pago                    CLIPPED              ,  --29
                   COLUMN 643, gr_reporte.afo_rec_entregado                 USING "&&&&&&&&&&.&&",  --30
                   COLUMN 656, gr_reporte.afo_fecha_entrega                 USING "DDMMYYYY"     ,  --31
                   COLUMN 664, gr_reporte.afo_isr                           USING "&&&&&&&&&&.&&",  --32
                   COLUMN 677, gr_reporte.infonavit_entidad                 CLIPPED              ,  --33
                   COLUMN 727, gr_reporte.infonavit_numcta                  CLIPPED              ,  --34
                   COLUMN 747, gr_reporte.infonavit_entregado               USING "&&&&&&&&&&.&&",  --35
                   COLUMN 760, gr_reporte.infonavit_fecha_entrega           USING "DDMMYYYY"     ,  --36
                   COLUMN 768, gr_reporte.infonavit_isr                     USING "&&&&&&&&&&.&&",  --37
                   COLUMN 781, gr_reporte.fecha_emision                     USING "DDMMYYYY"     ,  --38
                   COLUMN 789, gr_reporte.fecha_ini_pen                     USING "DDMMYYYY"     ,  --39
                   COLUMN 797, gr_reporte.monto_pen                         USING "&&&&&&&&&&.&&",  --40
                   COLUMN 810, gr_reporte.saldo_fin                         USING "&&&&&&&&&&.&&"   --41

            LET gi_registros = gi_registros + 1

         WHEN 99 --Sumario
             PRINT COLUMN 001, '99'                         ,
                  COLUMN 003, hoy          USING "DDMMYYYY",
                  COLUMN 011, '00000001'                          ,
                  COLUMN 019, gi_registros USING "&&&&&&&&"
      END CASE

END REPORT
################################################################################
#*******************************************************************************
#Función : Ingresa_etapa
#Objetivo: Inserta etapa de control en dis_ctrl_proceso
#Parámetros de entrada:
#li_folio     => Folio proceso
#ls_etapa_cod => Num. etapa
#ls_estado    => parametro2
#lc_resultado => descripcion
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION Ingresa_etapa(li_folio,ls_etapa_cod,ls_estado,lc_resultado)

   DEFINE li_folio         INTEGER     ,
          ls_etapa_cod     DECIMAL(2,0),
          lc_resultado     CHAR(50)    ,
          ls_estado        SMALLINT

   DEFINE lc_hora_inicial  CHAR(08),
          lc_hora_final    CHAR(08)

   LET lc_hora_inicial = TIME
   LET lc_hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES(
       TODAY          ,   -- fecha_proceso
       "CTA"          ,   -- proceso_cod
       ls_etapa_cod   ,   -- etapa_cod
       lc_hora_inicial,   -- lc_hora_inicial
       lc_hora_final  ,   -- lc_hora_final
       NULL           ,   -- parametro1
       ls_estado      ,   -- parametro2
       NULL           ,   -- parametro3
       NULL           ,   -- parametro4
       NULL           ,   -- parametro5
       li_folio       ,   -- folio
       lc_resultado   ,   -- resultado
       USER           ,   -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            ls_etapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
################################################################################
#*******************************************************************************
#Función : Actualiza_etapa
#Objetivo: Actualiza etapa de control en dis_ctrl_proceso
#Parámetros de entrada:
#li_folio     => Folio proceso
#ls_etapa_cod => Num. etapa
#li_pos       => Numero registros
#lc_resultado => descripcion
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION Actualiza_etapa(li_folio,ls_etapa_cod,li_pos,lc_resultado)

   DEFINE li_folio         INTEGER     ,
          ls_etapa_cod     DECIMAL(2,0),
          lc_resultado     CHAR(50)    ,
          li_pos           INTEGER

   DEFINE lc_hora_final    CHAR(08)

   LET lc_hora_final   = TIME

   UPDATE dis_ctrl_proceso
   SET    hora_final = lc_hora_final,
          parametro3 = li_pos       ,
          resultado  = lc_resultado
   WHERE  folio       = li_folio
   AND    proceso_cod = 'CTA'
   AND    etapa_cod   = ls_etapa_cod

END FUNCTION
################################################################################
#*******************************************************************************
#Función : despliega
#Objetivo: despliega valores que se imprimen en el reporte
#Parámetros de entrada:
#Valores de retorno   : Ninguno
#Variables globales   : Ninguno
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION despliega()
DISPLAY "_____________________________________________________________________"
DISPLAY "nss: ",gr_reporte.nss
DISPLAY "#1    tipo_registro          :", gr_reporte.tipo_registro           USING "&&"
DISPLAY "#2    consecutivo_envio      :", gr_reporte.consecutivo_envio       USING "&&&&&&&&&&&"
DISPLAY "#3    dia_corte              :", gr_reporte.dia_corte               USING "&&"
DISPLAY "#4    mes_corte              :", gr_reporte.mes_corte               USING "&&"
DISPLAY "#5    nombre_completo        :", gr_reporte.nombre_completo         CLIPPED
DISPLAY "#6    calle_num              :", gr_reporte.calle_num               CLIPPED
DISPLAY "#7    colonia                :", gr_reporte.colonia                 CLIPPED
DISPLAY "#8    cp                     :", gr_reporte.cp                      CLIPPED
DISPLAY "#9    Centro de reparto      :", gr_reporte.centro_reparto          CLIPPED
DISPLAY "#10  entidad_federal        :", gr_reporte.entidad_federal         CLIPPED
DISPLAY "#11  munic_delega           :", gr_reporte.munic_delega            CLIPPED
DISPLAY "#12  nss                    :", gr_reporte.nss                     CLIPPED
DISPLAY "#13  rfc                    :", gr_reporte.rfc                     CLIPPED
DISPLAY "#14  curp                   :", gr_reporte.curp                    CLIPPED
DISPLAY "#15  saldo_ini_ret          :", gr_reporte.saldo_ini_ret           USING "&&&&&&&&&&.&&"
DISPLAY "#16  saldo_ini_viv          :", gr_reporte.saldo_ini_viv           USING "&&&&&&&&&&.&&"
DISPLAY "#17  saldo_trans_ret        :", gr_reporte.saldo_trans_ret         USING "&&&&&&&&&&.&&"
DISPLAY "#18  saldo_trans_viv        :", gr_reporte.saldo_trans_viv         USING "&&&&&&&&&&.&&"
DISPLAY "#19  saldo_rem_ret          :", gr_reporte.saldo_rem_ret           USING "&&&&&&&&&&.&&"
DISPLAY "#20  saldo_rem_viv          :", gr_reporte.saldo_rem_viv           USING "&&&&&&&&&&.&&"
DISPLAY "#21  ley_pen                :", gr_reporte.ley_pen                 CLIPPED
DISPLAY "#22  regimen                :", gr_reporte.regimen                 CLIPPED
DISPLAY "#23  seguro                 :", gr_reporte.seguro                  CLIPPED
DISPLAY "#24  pension                :", gr_reporte.pension                 CLIPPED
DISPLAY "#25  pen_entidad            :", gr_reporte.pen_entidad             CLIPPED
DISPLAY "#26  pen_monto_trans        :", gr_reporte.pen_monto_trans         USING "&&&&&&&&&&.&&"
DISPLAY "#27  pen_fecha_trans        :", gr_reporte.pen_fecha_trans         USING "DDMMYYYY"
DISPLAY "#28  afo_entidad            :", gr_reporte.afo_entidad             CLIPPED
DISPLAY "#29  afo_medio_pago         :", gr_reporte.afo_medio_pago          CLIPPED
DISPLAY "#30  afo_rec_entregado      :", gr_reporte.afo_rec_entregado       USING "&&&&&&&&&&.&&"
DISPLAY "#31  afo_fecha_entrega      :", gr_reporte.afo_fecha_entrega       USING "DDMMYYYY"
DISPLAY "#32  afo_isr                :", gr_reporte.afo_isr                 USING "&&&&&&&&&&.&&"
DISPLAY "#33  infonavit_entidad      :", gr_reporte.infonavit_entidad       CLIPPED
DISPLAY "#34  infonavit_numcta       :", gr_reporte.infonavit_numcta        CLIPPED
DISPLAY "#35  infonavit_entregado    :", gr_reporte.infonavit_entregado     USING "&&&&&&&&&&.&&"
DISPLAY "#36  infonavit_fecha_entrega:", gr_reporte.infonavit_fecha_entrega USING "DDMMYYYY"
DISPLAY "#37  infonavit_isr          :", gr_reporte.infonavit_isr           USING "&&&&&&&&&&.&&"
DISPLAY "#38  fecha_emision          :", gr_reporte.fecha_emision           USING "DDMMYYYY"
DISPLAY "#39  fecha_ini_pen          :", gr_reporte.fecha_ini_pen           USING "DDMMYYYY"
DISPLAY "#40  monto_pen              :", gr_reporte.monto_pen               USING "&&&&&&&&&&.&&"
DISPLAY "#41  saldo_fin              :", gr_reporte.saldo_fin               USING "&&&&&&&&&&.&&"
DISPLAY "_____________________________________________________________________"
END FUNCTION
################################################################################
#*******************************************************************************
#Función : redondea
#Objetivo: Redondea 2 decimales
#Parámetros de entrada: ld_pesos6 => Saldo a 6 decimales
#Valores de retorno   : ld_pesos2 => Saldo a 2 decimales
#Variables globales   :
#Pantallas utilizadas : Ninguno
#Autor   : CÉSAR DAVID CHÁVEZ MARTÍNEZ
#*******************************************************************************
FUNCTION redondea(ld_pesos6)
   DEFINE ld_pesos6 DECIMAL(22,6)
   DEFINE ld_pesos2 DECIMAL(22,2)

   LET ld_pesos2 = ld_pesos6 * 1

   RETURN ld_pesos2
END FUNCTION
################################################################################
################################################################################
#*******************************************************************************
#Función : f_pmg_tmp_dis_cta
#Objetivo: Crea la tabla temporal con todos los movimientos de dis_cuenta para
#          obtener el saldo del trabajador previo a la liquidación
#Parámetros de entrada: p_nss => NSS del cual se obtendrán los movimientos
#Valores de retorno   : Ninguno
#Variables globales   :
#Pantallas utilizadas : Ninguno
#Autor   : EMMANUEL REYES PÉREZ
#Rquerimiento: CPL-2167
#*******************************************************************************
FUNCTION f_pmg_tmp_dis_cta(p_nss)

   DEFINE p_nss              CHAR(11)
   DEFINE lc_sel_his         CHAR(8000)
   DEFINE lc_nom_tabla       CHAR(20)

   DEFINE ls_ano_actual      SMALLINT
   DEFINE ls_ano_ini         SMALLINT
   DEFINE lc_nombre_tabla    CHAR(12)
   DEFINE ls_anio_cta        SMALLINT
   DEFINE ls_historico       SMALLINT
   DEFINE lc_nomarchivo      CHAR(100)

   DEFINE lr_transferencias RECORD
          folio       INTEGER,
          consecutivo DECIMAL(11,0)
   END RECORD


   LET ls_ano_actual   = YEAR(TODAY)
   LET ls_anio_cta     = ls_ano_ini - 2000
   LET lc_nombre_tabla = 'dis_cuenta'
   LET lc_nombre_tabla = lc_nombre_tabla CLIPPED, ls_anio_cta USING "&&"

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
      DROP TABLE tmp_transferencias
      SELECT * 
      FROM   dis_cuenta
      WHERE  1=2 INTO TEMP tmp_dis_cuenta
   WHENEVER ERROR STOP

   LET lc_sel_his = ''
    DECLARE cur_his CURSOR FOR
    SELECT tabname
    FROM   systables
    WHERE  tabname matches "dis_cuenta??"
    OR     tabname =  "dis_cuenta"

    FOREACH cur_his INTO lc_nom_tabla
      
      LET lc_sel_his =  " INSERT INTO tmp_dis_cuenta         \n",
                        " SELECT   *                         \n",
                        " FROM     ",lc_nom_tabla CLIPPED ," \n",
                        " WHERE    nss         =      ?      \n",
                        " AND tipo_movimiento NOT IN (888,999) "

      LET lc_sel_his = lc_sel_his CLIPPED
      PREPARE sql_totales FROM lc_sel_his
      EXECUTE sql_totales USING p_nss

    END FOREACH

    CLOSE cur_his
    
DISPLAY "HISTORICOS:"
--DISPLAY lc_sel_his CLIPPED

   #Borrar movimientos de transferencia entre siefores
   SELECT folio          ,
          folio_solicitud
   FROM   tes_solicitud
   WHERE  nss    = p_nss
   AND    estado = 103
   INTO TEMP tmp_transferencias

   DECLARE cur_borra_transf CURSOR FOR
   SELECT *
   FROM   tmp_transferencias

   FOREACH cur_borra_transf INTO lr_transferencias.folio,
                                     lr_transferencias.consecutivo

      DELETE
      FROM   tmp_dis_cuenta
      WHERE  tipo_movimiento IN (1,210)
      AND    folio            = lr_transferencias.folio
      AND    consecutivo_lote = lr_transferencias.consecutivo

   END FOREACH

   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta(subcuenta,
                                                  siefore  ,
                                                  folio    ,
                                                  consecutivo_lote)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta



END FUNCTION


#==============================================================================#
# Separa movimientos de dis_cuenta por nss                                     #
#==============================================================================#
FUNCTION f_pmg_tmp_dis_cta2(p_nss)

   DEFINE p_nss              CHAR(11)
   DEFINE lc_sel_his         CHAR(8000)
   DEFINE lc_nom_tabla       CHAR(20)
   DEFINE lc_nomarchivo      CHAR(100)


   DISPLAY "\nGENERANDO TEMPORALES DE HISTORICOS DE dis_cuenta PARA PMG..."

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
   WHENEVER ERROR STOP

   --BUSCA CADA UNA DE LAS dis_cuentas
   DECLARE cur_his2 CURSOR FOR
   SELECT tabname
   FROM   SYSTABLES
   WHERE  tabname matches "dis_cuenta??"

   FOREACH cur_his2 INTO lc_nom_tabla
         LET lc_sel_his = lc_sel_his CLIPPED,
                       "\n SELECT A.* ",
                       "\n FROM   ",lc_nom_tabla CLIPPED, " A",
                       "\n WHERE  A.nss = ","'",p_nss,"'",
                       "\n AND    A.tipo_movimiento NOT IN (888,999)",
                       "\n AND    A.subcuenta IN(1,2,5,6,9,7,4,8)   ",
                       "\n UNION ALL "
         LET lc_nom_tabla = ''
   END FOREACH

   CLOSE cur_his2

   LET lc_sel_his =  lc_sel_his CLIPPED,
                    "\n SELECT A.* ",
                    "\n FROM   dis_cuenta A",
                    "\n WHERE  A.nss = ","'",p_nss,"'",
                    "\n AND    A.tipo_movimiento NOT IN (888,999)",
                    "\n AND    A.subcuenta IN(1,2,5,6,9,7,4,8)   ",
                    "\n INTO TEMP tmp_dis_cuenta "

   PREPARE eje_sel_his2 FROM lc_sel_his
   EXECUTE eje_sel_his2

   IF fgl_getenv("DEBUGEFP") = "1" THEN
      DISPLAY "UNLOAD"
      LET lc_nomarchivo = "tmp_dis_cuenta_",p_nss
      UNLOAD TO lc_nomarchivo SELECT * FROM TMP_DIS_CUENTA
   END IF

END FUNCTION



#==============================================================================#
# Objetivo: OBTIENE EL SALDO DE PMG EN TABLAS HISTORICAS                       #
#           EN CASO DE SER SALDO AL DIA SE TOMA DE dis_cuenta                  #
# Autor   : ISAI JIMENEZ ROJAS                                                 #
#==============================================================================#
FUNCTION f_saldo_pmg_his(p_nss,p_fecha_his)

   DEFINE p_nss              CHAR(11)
   DEFINE p_fecha_his        DATE             --fecha a la que se recupera saldo

   DEFINE ld_fecha_viv       DATE             --fecha para valuar vivienda
   DEFINE lc_sql             CHAR(1000)
   DEFINE lc_nom_tabla       CHAR(20)
   DEFINE lc_anio            CHAR(4)
   DEFINE ls_anio            SMALLINT
   DEFINE lc_nomarchivo      CHAR(100)
   DEFINE ld_saldo_rcv_acc   DECIMAL(16,6)
   DEFINE ld_saldo_viv_acc   DECIMAL(16,6)
   DEFINE ld_saldo_rcv_pes   DECIMAL(12,2)
   DEFINE ld_saldo_viv_pes   DECIMAL(12,2)
   DEFINE v_contador         SMALLINT


   --DETERMINA FECHA DE VIVIENDA HISTORICA
   LET ld_fecha_viv = MDY(MONTH(p_fecha_his),"01",YEAR(p_fecha_his))

   DISPLAY "OBTENIENDO SALDOS PMG - RCV+SAR AL: ",p_fecha_his  USING "DD/MM/YYYY",
                                  " VIV+VIV ANT AL: ",ld_fecha_viv USING "DD/MM/YYYY"


   --RECUPERA LA dis_cuenta a PROCESAR
   IF YEAR(p_fecha_his) = YEAR(HOY) THEN
      LET lc_nom_tabla  = "dis_cuenta"
   ELSE
      LET lc_anio      = YEAR(p_fecha_his)
      LET lc_nom_tabla = "dis_cuenta",lc_anio[3,4]

      SELECT COUNT(*)
      INTO   v_contador
      FROM   systables
      WHERE  tabname = lc_nom_tabla

      IF (v_contador == 0) THEN
         LET lc_nom_tabla  = "dis_cuenta"
      END IF

      DISPLAY "\nANIO BASE: ", lc_anio

   END IF

   DISPLAY "\nSe recupera saldo de: ",lc_nom_tabla

   --RECUPERA SALDO HISTORICO DE RCV
   LET lc_sql = "\n SELECT NVL(SUM(A.monto_en_acciones),0) acciones,               ",
                "\n        NVL(SUM(A.monto_en_acciones*B.precio_del_dia),0) pesos  ",
                "\n FROM  ",lc_nom_tabla CLIPPED,"  A,                             ",
                "\n        glo_valor_accion B                                      ",
                "\n WHERE  A.nss              = '",p_nss,"'                        ",
                "\n AND    A.subcuenta        IN(1,2,5,6,9,7)                      ",
                "\n AND    A.fecha_conversion <= '",p_fecha_his,"'                 ",
                "\n AND    A.siefore          = B.codigo_siefore                   ",
                "\n AND    B.fecha_valuacion  = '",p_fecha_his,"'                  "


   PREPARE exe_rcv FROM lc_sql
   EXECUTE exe_rcv INTO ld_saldo_rcv_acc, ld_saldo_rcv_pes

   --RECUPERA SALDO HISTORICO DE VIV
   LET lc_sql = "\n SELECT NVL(sum(A.monto_en_acciones),0) acciones,               ",
                "\n        NVL(sum(A.monto_en_acciones*B.precio_del_dia),0) pesos  ",
                "\n FROM  ",lc_nom_tabla CLIPPED,"  A,                             ",
                "\n        glo_valor_accion B                                      ",
                "\n WHERE  A.nss              = '",p_nss,"'                        ",
                "\n AND    A.subcuenta        IN(4,8)                              ",
                "\n AND    A.fecha_conversion <= '",p_fecha_his,"'                 ",
                "\n AND    A.siefore          = B.codigo_siefore                   ",
                "\n AND    B.fecha_valuacion  = '",ld_fecha_viv,"'                 "


   PREPARE exe_viv FROM lc_sql
   EXECUTE exe_viv INTO ld_saldo_viv_acc, ld_saldo_viv_pes

   RETURN ld_saldo_rcv_acc,
          ld_saldo_viv_acc,
          ld_saldo_rcv_pes,
          ld_saldo_viv_pes

END FUNCTION


################################################################################
################################################################################
#*******************************************************************************
#Función : f_obten_fec_liq_pmg
#Objetivo: Obtiene la fecha de liquidación de la mensualidad 1 del contrato PMG
#Parámetros de entrada: p_nss => NSS del cual se obtendrán la fecha
#Valores de retorno   : ld_fecha_liq fecha de liquidacion mens 1 PMG
#Variables globales   :
#Pantallas utilizadas : Ninguno
#Autor   : EMMANUEL REYES PÉREZ
#Rquerimiento: CPL-2167
#Modificación: CPL-2520 Se va a buscar la mínima fecha de liquidación para el NSS
#              sin importar si la solicitud fue rechazada, tampoco se toma en cuenta
#              el numero de la mensualidad, como criterios se toma:
#              que tenga folio de liqudiación y que se encuentre en 70-LIQUIDADO
#*******************************************************************************

FUNCTION f_obten_fec_liq_pmg(pc_nss)

   DEFINE pc_nss       CHAR(11)
   DEFINE ld_fecha_liq DATE
   DEFINE ls_error     CHAR(500)

   LET ld_fecha_liq = NULL
   LET ls_error     = ""

   WHENEVER ERROR CONTINUE
      -- NUEVA MANERA DE OBTENER LA FECHA A PARTIR DE CPL-2520:
      SELECT MIN(fecha_liquida)
        INTO ld_fecha_liq
        FROM pen_ctr_pago_det
       WHERE nss           = pc_nss
         AND estado        = 70 --LIQUIDADO
         AND folio_liquida IS NOT NULL

      IF SQLCA.SQLCODE <> 0 THEN
         LET ls_error = "Error obteniendo fecha de liquidación primer mensualidad PMG para NSS: ",
                        pc_nss," Error: ", ERR_GET(SQLCA.SQLCODE)
         LET ls_error = ls_error CLIPPED
         DISPLAY "--------\nError:\n--------\n",ls_error
         CALL ERRORLOG(ls_error)
      END IF

   WHENEVER ERROR STOP

   RETURN ld_fecha_liq

END FUNCTION

################################################################################
# Función desarrollada con base en el req CPL-2167, para poder realizar la     #
# valuación del sdo en acciones remanente de PMG, a la fecha de generación del #
# archivo.                                                                     #
# Entrada: recibe el sdo en acciones separado por retiro y vivienda            #
# Salida : devuelve el sdo en pesos calculado con la valuación del mes (viv al #
# día 1 del mes y ret al día ultimo del mes)                                   #
# Emmanuel Reyes Julio 2016                                                    #
################################################################################
FUNCTION f_valua_remanente(pr_montos)

   DEFINE pr_montos      RECORD --montos en acciones de vivienda y retiro(remanentes)
          sdo_ret         DECIMAL(22,6),
          sdo_viv         DECIMAL(22,6)
   END RECORD

   DEFINE lr_acciones    RECORD --precio de las acciones
          precio_ret      DECIMAL(19,14),
          precio_viv      DECIMAL(19,14)
   END RECORD

   DEFINE lr_saldos_rem  RECORD
          saldo_rem_ret   DECIMAL(22,6),
          saldo_rem_viv   DECIMAL(22,6)
   END RECORD

   DEFINE ld_fecha_tabla  DATE   ,  --Fecha que recupera de la tabla
          ld_fecha_viv    DATE   ,  --Fecha para el dìa 1 del mes
          ld_fecha_ret    DATE   ,  --Fecha para el ultimo dìa del mes
          ld_fecha_aux    DATE      --Auxiliar para formar fechas

   DEFINE ls_mes          SMALLINT ,--Obtiene mes para fecha aux
          ls_anio         SMALLINT ,--Obtiene año para fecha aux
          ls_siefore_ret  SMALLINT ,--Obtiene las siefores a valuar
          ls_siefore_viv  SMALLINT  --Obtiene las siefores a valuar

   DEFINE lc_mensaje      CHAR(800) --Mensaje error

   WHENEVER ERROR CONTINUE
      --Recupera la fecha inicial de la tabla
      SELECT fecha_inicio
      INTO   ld_fecha_tabla
      FROM   cta_ctr_proceso
      WHERE  folio = gi_folio
      AND    nss   = gr_reporte.nss

      IF SQLCA.SQLCODE <> 0 THEN
         LET lc_mensaje = " Error recuperando fecha inicio    \n",
                        " Tabla    : cta_ctr_proceso        \n",
                        " NSS      : ",gr_reporte.nss,     "\n",
                        " Folio    : ",gi_folio      ,     "\n",
                        " Error    : ",ERR_GET(SQLCA.SQLCODE)
         CALL ERRORLOG(lc_mensaje CLIPPED)
      END IF

   WHENEVER ERROR STOP

   --Obtiene el mes y año para formar las fechas
   LET ls_mes  = MONTH(ld_fecha_tabla)
   LET ls_anio = YEAR(ld_fecha_tabla)

   -- Forma la fecha de valuacion vivienda (primer dia del mes)
   LET ld_fecha_viv = MDY(ls_mes,1,ls_anio)

   -- Forma la fecha de valuaciòn retiro (ultimo dia del mes)
   LET ld_fecha_aux = ld_fecha_viv + 1 UNITS MONTH
   LET ld_fecha_ret = ld_fecha_aux - 1 UNITS DAY

   --Recupera siefore retiro
   DECLARE cur_siefore_ret CURSOR FOR
      SELECT UNIQUE siefore
      FROM   tmp_dis_cuenta
      WHERE  subcuenta IN (1,2,5,6,9)
      AND    siefore NOT IN (10) --Chequera
      AND    fecha_conversion >= ld_fecha_tabla --CPL-2629 que la siefore sea del mes de corte

   FOREACH cur_siefore_ret INTO ls_siefore_ret
      IF ls_siefore_ret > 0 THEN
         EXIT FOREACH
      END IF
   END FOREACH

   --Recupera siefore viv
   SELECT UNIQUE siefore
   INTO   ls_siefore_viv
   FROM   tmp_saldo_dia
   WHERE  subcuenta IN (4)

   --Recupera precio ret
   SELECT precio_del_dia
   INTO   lr_acciones.precio_ret
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_ret
   AND    codigo_siefore  = ls_siefore_ret

   IF SQLCA.SQLCODE <> 0 THEN
      LET lc_mensaje = " Error recuperando precio del día  \n",
                       " Tabla    : glo_valor_accion       \n",
                       " Siefore  : ",ls_siefore_ret,     "\n",
                       " fecha_val: ",ld_fecha_ret  ,     "\n",
                       " Error    : ",ERR_GET(SQLCA.SQLCODE)
      CALL ERRORLOG(lc_mensaje CLIPPED)
   END IF

   --Recupera precio viv
   SELECT precio_del_dia
   INTO   lr_acciones.precio_viv
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_viv
   AND    codigo_siefore  = ls_siefore_viv

   IF SQLCA.SQLCODE <> 0 THEN
      LET lc_mensaje = " Error recuperando precio del día  \n",
                       " Tabla    : glo_valor_accion       \n",
                       " Siefore  : ",ls_siefore_viv,     "\n",
                       " fecha_val: ",ld_fecha_viv  ,     "\n",
                       " Error    : ",ERR_GET(SQLCA.SQLCODE)
      CALL ERRORLOG(lc_mensaje CLIPPED)
   END IF

   IF lr_acciones.precio_ret IS NULL THEN
      LET lr_acciones.precio_ret = 0
   END IF

   IF lr_acciones.precio_viv IS NULL THEN
      LET lr_acciones.precio_viv = 0
   END IF

   -- Obtiene los montos remanentes en pesos
   LET lr_saldos_rem.saldo_rem_ret = pr_montos.sdo_ret * lr_acciones.precio_ret
   LET lr_saldos_rem.saldo_rem_viv = pr_montos.sdo_viv * lr_acciones.precio_viv

   IF lr_saldos_rem.saldo_rem_ret IS NULL THEN
      LET lr_saldos_rem.saldo_rem_ret = 0
   END IF

   IF lr_saldos_rem.saldo_rem_viv IS NULL THEN
      LET lr_saldos_rem.saldo_rem_viv = 0
   END IF

   RETURN lr_saldos_rem.*

END FUNCTION

#==============================================================================#
# VALUA LAS ACCIONES A LA FECHA INDICADA                                       #
#==============================================================================#
FUNCTION f_valua_remanente_pmg(pc_nss, pd_sdo_ret, pd_sdo_viv, pd_fecha)

   DEFINE pc_nss             CHAR(11)
   DEFINE pd_sdo_ret         DECIMAL(22,6)
   DEFINE pd_sdo_viv         DECIMAL(22,6)
   DEFINE pd_fecha           DATE

   DEFINE ld_precio_rcv      DECIMAL(19,14)
   DEFINE ld_precio_viv      DECIMAL(19,14)

   DEFINE ld_saldo_rem_ret   DECIMAL(12,2)
   DEFINE ld_saldo_rem_viv   DECIMAL(12,2)

   DEFINE ld_fecha_viv       DATE      --Fecha para el dìa 1 del mes
   DEFINE lc_mensaje         CHAR(800) --Mensaje error

   DEFINE ls_siefore_rcv     SMALLINT
   DEFINE ls_siefore_viv     SMALLINT

   DISPLAY "\nValuando remanentes..."

   --OBTIENE CODIGO SIEFORE PARA RCV
   SELECT DISTINCT codigo_siefore
   INTO   ls_siefore_rcv
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta IN(1,2,5,6,9)

   DISPLAY "\tSiefore RCV: ",ls_siefore_rcv

   --OBTIENE CODIGO SIEFORE PARA VIV
   SELECT DISTINCT codigo_siefore
   INTO   ls_siefore_viv
   FROM   cta_regimen
   WHERE  nss        = pc_nss
   AND    subcuenta IN(4)

   DISPLAY "\tSiefore VIV: ",ls_siefore_viv


   --PREPARA FECHA PARA VALUACION DE VIVIENDA
   LET ld_fecha_viv = MDY(MONTH(pd_fecha),1,YEAR(pd_fecha))

   DISPLAY "\tFecha   RCV: ",pd_fecha
   DISPLAY "\tFecha   VIV: ",ld_fecha_viv

   --RECUPERA PRECIO DE RCV
   SELECT precio_del_dia
   INTO   ld_precio_rcv
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = pd_fecha
   AND    codigo_siefore  = ls_siefore_rcv

   IF SQLCA.SQLCODE <> 0 THEN
      LET lc_mensaje = " Error recuperando precio del día  \n",
                       " Tabla    : glo_valor_accion       \n",
                       " Siefore  : ",ls_siefore_rcv,     "\n",
                       " fecha_val: ",pd_fecha      ,     "\n",
                       " Error    : ",ERR_GET(SQLCA.SQLCODE)
      DISPLAY lc_mensaje CLIPPED
      CALL ERRORLOG(lc_mensaje CLIPPED)
   END IF

   DISPLAY "\tPrecio  RCV: ",ld_precio_rcv

   --RECUPERA PRECIO VIV
   SELECT precio_del_dia
   INTO   ld_precio_viv
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = ld_fecha_viv
   AND    codigo_siefore  = ls_siefore_viv

   IF SQLCA.SQLCODE <> 0 THEN
      LET lc_mensaje = " Error recuperando precio del día  \n",
                       " Tabla    : glo_valor_accion       \n",
                       " Siefore  : ",ls_siefore_viv,     "\n",
                       " fecha_val: ",ld_fecha_viv  ,     "\n",
                       " Error    : ",ERR_GET(SQLCA.SQLCODE)
      CALL ERRORLOG(lc_mensaje CLIPPED)
   END IF

   IF ld_precio_rcv IS NULL THEN
      LET ld_precio_rcv = 0
   END IF

   IF ld_precio_viv IS NULL THEN
      LET ld_precio_viv = 0
   END IF

   DISPLAY "\tPrecio  VIV: ",ld_precio_viv

   -- Obtiene los montos remanentes en pesos
   LET ld_saldo_rem_ret = pd_sdo_ret * ld_precio_rcv
   LET ld_saldo_rem_viv = pd_sdo_viv * ld_precio_viv

   IF ld_saldo_rem_ret IS NULL THEN
      LET ld_saldo_rem_ret = 0
   END IF

   IF ld_saldo_rem_viv IS NULL THEN
      LET ld_saldo_rem_viv = 0
   END IF

   RETURN ld_saldo_rem_ret,
          ld_saldo_rem_viv

END FUNCTION

