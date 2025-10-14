######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRB052                                        #
#Descripcion       => GENERA ARCHIVO OPER.09 NOTIFICACION DE SALDOS  #
#                     DE ANUALIDADES GARANTIZADAS                    #
#Sistema           => ACR                                            #
#Fecha creacion    => 19 NOVIEMBRE 2009                              #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
#Modificacion      => DMR 27/ago/2013   INV-2313 ESTANDARIZACION     #
#                  => la variable g_reg manejaba 2 dec cambio a 6 dec#
#Modificado        => DMR 05 SEPTIEMBRE 2013                         #
#                  => Estandarizacion de Programas de Anual. Garanti.#
#                  => y adicionalmente montos en sumario DEC(18,6)   #
#                  => MPT 13/09/2012 version 2.0.1    MLM-2163       #
#Modificado        => DMR 31/oct/2013 STARTLOG Personalizado         #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE reg_cza_tra_ag RECORD
      tipo_registro         CHAR(02),
      ident_servicio        CHAR(02),
      ident_operacion       CHAR(02),
      tipo_ent_origen       CHAR(02),
      cve_ent_origen        CHAR(03),
      tipo_ent_destino      CHAR(02),
      cve_ent_destino       CHAR(03),
      ent_fed_envio_lote    CHAR(03),
      fecha_presentacion    CHAR(08),
      consec_lote_dia       SMALLINT,
      cod_result_operac     CHAR(02),
      mot_rechazo_lote      CHAR(09)
   END RECORD

   DEFINE reg_det_tra_ag RECORD
      tipo_registro         CHAR(002)    ,
      cont_servicio         DECIMAL(10,0),
      tipo_recep_cuenta     CHAR(002)    ,
      cve_recep_cuenta      CHAR(003)    ,
      tipo_ced_cuenta       CHAR(002)    ,
      cve_ced_cuenta        CHAR(003)    ,
      tipo_transferencia    CHAR(002)    ,
      fecha_presentacion    CHAR(008)    ,
      curp_infonavit        CHAR(018)    ,
      nss_infonavit         CHAR(011)    ,
      rfc_infonavit         CHAR(013)    ,
      paterno_infonavit     CHAR(040)    ,
      materno_infonavit     CHAR(040)    ,
      nombres_infonavit     CHAR(040)    ,
      ident_lote_devol      CHAR(016)    ,
      nss_afore             CHAR(011)    ,
      rfc_afore             CHAR(013)    ,
      paterno_afore         CHAR(040)    ,
      materno_afore         CHAR(040)    ,
      nombres_afore         CHAR(040)    ,
      aivs_v92              DECIMAL(22,6),
      ind_dif_v92           CHAR(002)    ,
      aivs_v97              DECIMAL(22,6),
      ind_dif_v97           CHAR(002)    ,
      cod_result_operac     CHAR(002)    ,
      diag_proceso          CHAR(015)    ,
      nombre_imss           CHAR(050)    ,
      num_cred_infonavit    DECIMAL(10,0)
   END RECORD

   DEFINE reg_sum_tra_ag RECORD
      tipo_registro         CHAR(02)     ,
      cantidad_reg_det      INTEGER      ,
      sum_aivs_v92          DECIMAL(22,6),
      sum_aivs_v97          DECIMAL(22,6)
   END RECORD

   DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

   DEFINE g_reg RECORD
      nss                   CHAR(11),
      par97                 DECIMAL(22,6),
      viv97                 DECIMAL(15,2),
      par92                 DECIMAL(22,6),
      viv92                 DECIMAL(15,2)
   END RECORD

   DEFINE l_record ARRAY[10] OF RECORD
      folio                 INTEGER,
      tipo_movimiento       ,
      subcuenta             SMALLINT,
      fecha_valor           DATE,
      monto_en_acciones     DECIMAL(22,6),
      monto_en_pesos        DECIMAL(16,6)
   END RECORD

   DEFINE g RECORD
      folio                 INTEGER,
      tipo_movimiento       SMALLINT,
      subcuenta             SMALLINT,
      fecha_valor           DATE,
      monto_en_acciones     DECIMAL(22,6),
      monto_en_pesos        DECIMAL(18,6)
   END RECORD

   DEFINE
      HOY                   ,
      fecha_envio           ,
      fecha_envio2          ,
      fecha_envio3          ,
      fecha_envio_banx      ,
      fecha_presentacion    DATE

   DEFINE
      c5_HORA               CHAR(5)  ,
      c8_HOY                CHAR(8)  ,
      cat                   CHAR(300),
      comma                 CHAR(50) ,
      enter                 CHAR(1)  ,
      g_cza                 CHAR(100),
      g_det                 CHAR(100),
      g_sum                 CHAR(100),
      HORA                  CHAR(5)  ,
      nom_archivo           CHAR(100),
      permisos              CHAR(100),
      periodo_1             CHAR(6)  ,
      vusuario              CHAR(8)

   DEFINE
      existe                ,
      s_periodo             ,
      sw_1                  ,
      s_lotes_num           ,
      s_lotes_correlativo   ,
      s_codigo_afore        ,
      pos                   SMALLINT,
      vfolio                INTEGER

   DEFINE
      cont_reg              DECIMAL(10,0),
      vsaldo_viv_97         DECIMAL(15,2),
      vsum_sdo_viv92        DECIMAL(15,2),
      ult_aport_hist        ,
      ult_aport_v97         ,
      sdo_apo_v97           ,
      sdo_int_v97           ,
      sdo_apo_v92           ,
      sdo_int_v92           ,
      d_152_tt_ap_92        DECIMAL(15,6),
      sdo_tot_v97           ,
      sdo_tot_v92           ,
      saldo_total92         ,
      saldo_total97         DECIMAL(15,2)

END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB052.log")
   CALL inicio() #i
   CALL proceso_principal()  #pp
   CALL impresion_reporte()
END MAIN


FUNCTION inicio()
#i---------------
   LET HOY     = TODAY
   LET HORA    = TIME
   LET c5_HORA = HORA
   LET s_lotes_num = 4

   SELECT codigo_afore
   INTO   s_codigo_afore
   FROM   tab_afore_local

   SELECT *, user
   INTO   g_seg_modulo.*, vusuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET cont_reg = 1
   LET reg_det_tra_ag.aivs_v92 = 0
   LET reg_det_tra_ag.aivs_v97 = 0
   LET reg_sum_tra_ag.sum_aivs_v92 = 0
   LET reg_sum_tra_ag.sum_aivs_v97 = 0
   LET ult_aport_v97  = 0
   LET ult_aport_hist = 0
   LET sdo_apo_v97    = 0
   LET sdo_int_v97    = 0
   LET sdo_tot_v97    = 0
   LET sdo_apo_v92    = 0
   LET sdo_int_v92    = 0
   LET sdo_tot_v92    = 0
   LET saldo_total92  = 0
   LET saldo_total97  = 0

   INITIALIZE g_reg.* TO NULL
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW v1 AT 4,4 WITH FORM "ACRB0521" ATTRIBUTE(BORDER)
   DISPLAY " ACRB052   GENERA ARCHIVO OPER.09 ANUALIDADES GARANTIZADAS                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "<ESC> Ejecutar                                          <Ctrl-C> Salir " AT 1,1
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

   LET fecha_envio = HOY

   INPUT BY NAME fecha_envio WITHOUT DEFAULTS
      AFTER FIELD fecha_envio
         IF fecha_envio IS NULL THEN
            ERROR " LA FECHA DE ENVIO NO PUEDE SER NULA "
            NEXT FIELD fecha_envio
         END IF

      ON KEY (ESC)

         LET fecha_envio2 = MDY(MONTH(fecha_envio),1,YEAR(fecha_envio))
         LET fecha_envio3 = fecha_envio2 + 1 UNITS MONTH

         SELECT MAX(folio) + 1
         INTO   vfolio
         FROM   glo_folio

         INSERT INTO glo_folio
         VALUES(vfolio)

         DISPLAY " FOLIO :  ", vfolio USING "#######" AT 10,10

         INSERT INTO taa_folio VALUES(vfolio,14,HOY,vusuario)

         CALL crea_tablas()          #ct
         CALL genera_cza_tra_ag()    #gcta
         CALL genera_det_tra_ag()    #gdta
         CALL genera_sum_tra_ag()    #gsta

         LET cat = "cat ",g_seg_modulo.ruta_envio CLIPPED,"/CTAG ",
                   g_seg_modulo.ruta_envio CLIPPED,"/DTAG ",
                   g_seg_modulo.ruta_envio CLIPPED,"/STAG > ",
                   g_seg_modulo.ruta_envio CLIPPED,"/",
                   "TRANS_ANU_GAR.",HOY USING "MMDD","-",
                   HORA[1,2],HORA[4,5]
         RUN cat

         LET permisos = " chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/",
                        "TRANS_ANU_GAR.",HOY USING "MMDD","-",
                        HORA[1,2],HORA[4,5]
         RUN permisos

         LET nom_archivo = "TRANS_ANU_GAR.",HOY USING "MMDD","-",
                           HORA[1,2],HORA[4,5]

         DISPLAY " EL ARCHIVO HA SIDO GENERADO EN LA RUTA : " AT 14,10
         DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 15,11
         DISPLAY " CON EL NOMBRE : ", nom_archivo AT 17,10

         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
         FOR CHAR enter

         EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
         FOR enter
         EXIT INPUT
   END INPUT
END FUNCTION


FUNCTION crea_tablas()
#ct-------------------
   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE sdo_acr_ag
   WHENEVER ERROR STOP

   CREATE TABLE sdo_acr_ag
   (nss     CHAR(11),
    par97   DECIMAL(22,6),
    viv97   DECIMAL(15,2),
    par92   DECIMAL(22,6),
    viv92   DECIMAL(15,2))

   DATABASE safre_af
END FUNCTION


FUNCTION genera_cza_tra_ag()
#gcta------------------------
   LET reg_cza_tra_ag.tipo_registro      = "01"
   LET reg_cza_tra_ag.ident_servicio     = "02"
   LET reg_cza_tra_ag.ident_operacion    = "09"
   LET reg_cza_tra_ag.tipo_ent_origen    = "01"
   LET reg_cza_tra_ag.cve_ent_origen     = s_codigo_afore
   LET reg_cza_tra_ag.tipo_ent_destino   = "04"
   LET reg_cza_tra_ag.cve_ent_destino    = "002"
   LET reg_cza_tra_ag.ent_fed_envio_lote = "009"
   LET reg_cza_tra_ag.fecha_presentacion = fecha_envio USING"YYYYMMDD"
   LET reg_cza_tra_ag.consec_lote_dia    = 4
   LET reg_cza_tra_ag.cod_result_operac  = NULL
   LET reg_cza_tra_ag.mot_rechazo_lote   = NULL

   LET g_cza = g_seg_modulo.ruta_envio CLIPPED,"/CTAG"

   LET permisos = " chmod 777 ",g_cza CLIPPED
   RUN permisos

   START REPORT listado_1 TO g_cza
      OUTPUT TO REPORT listado_1(reg_cza_tra_ag.*) #1
   FINISH REPORT listado_1
END FUNCTION


REPORT listado_1(reg_cza_tra_ag)
#1--------------------------------
   DEFINE reg_cza_tra_ag RECORD
      tipo_registro        CHAR(02),
      ident_servicio       CHAR(02),
      ident_operacion      CHAR(02),
      tipo_ent_origen      CHAR(02),
      cve_ent_origen       CHAR(03),
      tipo_ent_destino     CHAR(02),
      cve_ent_destino      CHAR(03),
      ent_fed_envio_lote   CHAR(03),
      fecha_presentacion   CHAR(08),
      consec_lote_dia      SMALLINT,
      cod_result_operac    CHAR(02),
      mot_rechazo_lote     CHAR(09)
   END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW

         PRINT
            COLUMN 01,reg_cza_tra_ag.tipo_registro      ,
                      reg_cza_tra_ag.ident_servicio     ,
                      reg_cza_tra_ag.ident_operacion    ,
                      reg_cza_tra_ag.tipo_ent_origen    ,
                      reg_cza_tra_ag.cve_ent_origen     ,
                      reg_cza_tra_ag.tipo_ent_destino   ,
                      reg_cza_tra_ag.cve_ent_destino    ,
                      reg_cza_tra_ag.ent_fed_envio_lote ,
                      reg_cza_tra_ag.fecha_presentacion ,
                      reg_cza_tra_ag.consec_lote_dia    USING "&&&",
                      2 spaces                          ,
                      reg_cza_tra_ag.cod_result_operac  ,
                      reg_cza_tra_ag.mot_rechazo_lote   ,
                      687 spaces
END REPORT


FUNCTION genera_det_tra_ag()
#gdta-------------------------
   DEFINE
      dt_fc_vl              ,
      fecha_envio_archivo   DATE

   DEFINE
      c8_banxico            CHAR(08)

   DEFINE
      sdo_part_v97           DECIMAL(22,6),
      sdo_part_v92           DECIMAL(22,6),
      vprecio_accion         DECIMAL(19,14),
      sdo_pa97_2d            DECIMAL(18,2),
      sdo_pa92_2d            DECIMAL(18,2),
      sdo_pe97_2d            DECIMAL(18,2),
      sdo_pe92_2d            DECIMAL(18,2)

   SELECT @precio_del_dia
   INTO   vprecio_accion
   FROM   glo_valor_accion
   WHERE  @fecha_valuacion = fecha_envio2
   AND    @codigo_siefore  = 11

   DECLARE cur_1 CURSOR FOR
   SELECT tipo_transferencia,
          curp_infonavit,
          nss_infonavit,
          rfc_infonavit,
          paterno_infonavit,
          materno_infonavit,
          nombres_infonavit,
          ident_lote_devol,
          nss_afore,
          rfc_afore,
          paterno_afore,
          materno_afore,
          nombres_afore,
          aivs_v92,
          aivs_v97,
          "",
          "",
          nombre_imss,
          num_cred_infonavit
   FROM   safre_tmp:det_acr_ag
   WHERE  estado = 0

   LET reg_det_tra_ag.tipo_registro      = "02"
   LET reg_det_tra_ag.cont_servicio      = cont_reg
   LET reg_det_tra_ag.tipo_recep_cuenta  = "04"
   LET reg_det_tra_ag.cve_recep_cuenta   = "002"
   LET reg_det_tra_ag.tipo_ced_cuenta    = "01"
   LET reg_det_tra_ag.cve_ced_cuenta     = s_codigo_afore
   LET reg_det_tra_ag.fecha_presentacion = fecha_envio  USING"YYYYMMDD"

   LET vsaldo_viv_97  = 0
   LET vsum_sdo_viv92 = 0

   LET g_det = g_seg_modulo.ruta_envio CLIPPED,"/DTAG"

   LET permisos = " chmod 777 ",g_det CLIPPED
   RUN permisos

   START REPORT listado_2 TO g_det

   LET sw_1 = 0

   FOREACH cur_1 INTO reg_det_tra_ag.tipo_transferencia ,
                      reg_det_tra_ag.curp_infonavit     ,
                      reg_det_tra_ag.nss_infonavit      ,
                      reg_det_tra_ag.rfc_infonavit      ,
                      reg_det_tra_ag.paterno_infonavit  ,
                      reg_det_tra_ag.materno_infonavit  ,
                      reg_det_tra_ag.nombres_infonavit  ,
                      reg_det_tra_ag.ident_lote_devol   ,
                      reg_det_tra_ag.nss_afore          ,
                      reg_det_tra_ag.rfc_afore          ,
                      reg_det_tra_ag.paterno_afore      ,
                      reg_det_tra_ag.materno_afore      ,
                      reg_det_tra_ag.nombres_afore      ,
                      reg_det_tra_ag.aivs_v92           ,
                      reg_det_tra_ag.aivs_v97           ,
                      reg_det_tra_ag.cod_result_operac  ,
                      reg_det_tra_ag.diag_proceso       ,
                      reg_det_tra_ag.nombre_imss        ,
                      reg_det_tra_ag.num_cred_infonavit 

      LET g_reg.nss   = reg_det_tra_ag.nss_afore
      LET g_reg.viv97 = 0
      LET g_reg.viv92 = 0

      SELECT TRUNC(NVL(sum(monto_en_acciones),0),2)
      INTO   sdo_part_v97
      FROM   dis_cuenta
      WHERE  nss = reg_det_tra_ag.nss_afore
      AND    subcuenta = 4
      AND    tipo_movimiento <> 888
      AND    fecha_valor <= fecha_envio2

      IF reg_det_tra_ag.aivs_v97 <= sdo_part_v97 THEN
         LET sdo_part_v97 = reg_det_tra_ag.aivs_v97

         LET reg_det_tra_ag.ind_dif_v97 = "  "
      ELSE
         LET reg_det_tra_ag.ind_dif_v97 = "01"
      END IF

      IF sdo_part_v97 IS NULL OR sdo_part_v97 < 0.01 THEN
         LET sdo_apo_v97  = 0
         LET sdo_part_v97 = 0
         LET sdo_pa97_2d  = 0
         LET sdo_pe97_2d  = 0
      ELSE
         LET sdo_apo_v97 = sdo_part_v97 * vprecio_accion
         LET sdo_pa97_2d = sdo_part_v97
         LET sdo_pe97_2d = sdo_apo_v97
      END IF

      LET sdo_int_v97 = 0

      SELECT TRUNC(NVL(sum(monto_en_acciones),0),2)
      INTO   sdo_part_v92
      FROM   dis_cuenta
      WHERE  nss = reg_det_tra_ag.nss_afore
      AND    subcuenta = 8
      AND    tipo_movimiento <> 888
      AND    fecha_valor <= fecha_envio2

      IF reg_det_tra_ag.aivs_v92 <= sdo_part_v92 THEN
         LET sdo_part_v92 = reg_det_tra_ag.aivs_v92

         LET reg_det_tra_ag.ind_dif_v92 = "  "
      ELSE
         LET reg_det_tra_ag.ind_dif_v92 = "01"
      END IF

      IF sdo_part_v92 IS NULL OR sdo_part_v92 < 0.01 THEN
         LET sdo_apo_v92  = 0
         LET sdo_part_v92 = 0
         LET sdo_pa92_2d  = 0
         LET sdo_pe92_2d  = 0
      ELSE
         LET sdo_apo_v92 = sdo_part_v92 * vprecio_accion
         LET sdo_pa92_2d = sdo_part_v92
         LET sdo_pe92_2d = sdo_apo_v92
      END IF

      LET sdo_int_v92 = 0

      LET sdo_tot_v97 = sdo_apo_v97 + sdo_int_v97
      LET sdo_tot_v92 = sdo_apo_v92 + sdo_int_v92

      LET g_reg.viv97 = sdo_pe97_2d
      LET g_reg.par97 = sdo_part_v97
      LET g_reg.viv92 = sdo_pe92_2d
      LET g_reg.par92 = sdo_part_v92

      LET reg_det_tra_ag.aivs_v97 = sdo_part_v97 
      LET reg_det_tra_ag.aivs_v92 = sdo_part_v92 

      LET sdo_pe97_2d  = sdo_pe97_2d * (-1)
      LET sdo_pe92_2d  = sdo_pe92_2d * (-1)
      LET sdo_pa97_2d  = sdo_pa97_2d * (-1)
      LET sdo_pa92_2d  = sdo_pa92_2d * (-1)
      LET sdo_part_v97 = sdo_part_v97 * (-1)
      LET sdo_part_v92 = sdo_part_v92 * (-1)

      IF sdo_pe97_2d < 0 THEN
         INSERT INTO dis_provision
         VALUES (234,                            #tipo_movimiento
                 4,                              #subcuenta
                 11,                             #siefore
                 vfolio,                         #folio
                 reg_det_tra_ag.cont_servicio,   #consecutivo_lote
                 reg_det_tra_ag.nss_afore,       #nss
                 "",                             #curp
                 "9999",                         #folio_sua
                 fecha_envio3,                   #fecha_pago
                 fecha_envio3,                   #fecha_valor
                 fecha_envio3,                   #fecha_conversion
                 sdo_pe97_2d,                    #monto_en_pesos
                 sdo_part_v97,                   #monto_en_acciones
                 vprecio_accion,                 #precio_accion
                 0,                              #dias_cotizados
                 "",                             #sucursal
                 "ACR-TRA-AG",                   #id_aportante
                 5,                              #estado
                 HOY,                            #fecha_proceso
                 vusuario,                       #usuario
                 fecha_envio,                    #fecha_archivo
                 0)                              #etiqueta
      END IF

      IF sdo_pe92_2d < 0 THEN
         INSERT INTO dis_provision 
         VALUES(234,                             #tipo_movimiento
                8,                               #subcuenta
                11,                              #siefore
                vfolio,                          #folio
                reg_det_tra_ag.cont_servicio,    #consecutivo_lote
                reg_det_tra_ag.nss_afore,        #nss
                "",                              #curp
                "9999",                          #folio_sua
                fecha_envio3,                    #fecha_pago
                fecha_envio3,                    #fecha_valor
                fecha_envio3,                    #fecha_conversion
                sdo_pe92_2d,                     #monto_en_pesos
                sdo_part_v92,                    #monto_en_acciones
                vprecio_accion,                  #precio_accion
                0,                               #dias_cotizados
                "",                              #sucursal
                "ACR-TRA-AG",                    #id_aportante
                5,                               #estado
                HOY,                             #fecha_proceso
                vusuario,                        #usuario
                fecha_envio,                     #fecha_archivo
                0)                               #etiqueta
      END IF

      LET sw_1 = 1

      OUTPUT TO REPORT listado_2(reg_det_tra_ag.*) #2

      DISPLAY " TOTAL DE REGISTROS : ", cont_reg AT 12,10

      INSERT INTO safre_tmp:sdo_acr_ag VALUES(g_reg.*)

      LET cont_reg = cont_reg + 1

      LET ult_aport_v97  = 0
      LET sdo_apo_v97    = 0
      LET sdo_int_v97    = 0
      LET sdo_tot_v97    = 0
      LET sdo_apo_v92    = 0
      LET sdo_int_v92    = 0
      LET sdo_tot_v92    = 0
      LET saldo_total92  = 0
      LET saldo_total97  = 0
      LET sdo_part_v97   = 0
      LET sdo_part_v92   = 0

   END FOREACH

   IF sw_1 = 0 THEN
      DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
      SLEEP 3
      EXIT PROGRAM
   END IF

   LET cont_reg = cont_reg - 1

   FINISH REPORT listado_2 #2
END FUNCTION


REPORT listado_2(reg_det_tra_ag)
#2------------------------------
   DEFINE reg_det_tra_ag RECORD
      tipo_registro        CHAR(002)     ,
      cont_servicio        DECIMAL(10,0) ,
      tipo_recep_cuenta    CHAR(002)     ,
      cve_recep_cuenta     CHAR(003)     ,
      tipo_ced_cuenta      CHAR(002)     ,
      cve_ced_cuenta       CHAR(003)     ,
      tipo_transferencia   CHAR(002)     ,
      fecha_presentacion   CHAR(008)     ,
      curp_infonavit       CHAR(018)     ,
      nss_infonavit        CHAR(011)     ,
      rfc_infonavit        CHAR(013)     ,
      paterno_infonavit    CHAR(040)     ,
      materno_infonavit    CHAR(040)     ,
      nombres_infonavit    CHAR(040)     ,
      ident_lote_devol     CHAR(016)     ,
      nss_afore            CHAR(011)     ,
      rfc_afore            CHAR(013)     ,
      paterno_afore        CHAR(040)     ,
      materno_afore        CHAR(040)     ,
      nombres_afore        CHAR(040)     ,
      aivs_v92             DECIMAL(22,6) ,
      ind_dif_v92          CHAR(002)     ,
      aivs_v97             DECIMAL(22,6) ,
      ind_dif_v97          CHAR(002)     ,
      cod_result_operac    CHAR(002)     ,
      diag_proceso         CHAR(015)     ,
      nombre_imss          CHAR(050)     ,
      num_cred_infonavit   DECIMAL(10,0)
   END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW
         PRINT
            COLUMN 01,reg_det_tra_ag.tipo_registro        ,
                      cont_reg                            USING "&&&&&&&&&&",
                      reg_det_tra_ag.tipo_recep_cuenta    ,
                      reg_det_tra_ag.cve_recep_cuenta     ,
                      reg_det_tra_ag.tipo_ced_cuenta      ,
                      reg_det_tra_ag.cve_ced_cuenta       USING "&&&",
                      reg_det_tra_ag.tipo_transferencia   ,
                      reg_det_tra_ag.fecha_presentacion   ,
                      8 spaces                            ,
                      reg_det_tra_ag.curp_infonavit       ,
                      reg_det_tra_ag.nss_infonavit        ,
                      15 spaces                           ,
                      reg_det_tra_ag.rfc_infonavit        ,
                      reg_det_tra_ag.paterno_infonavit    ,
                      reg_det_tra_ag.materno_infonavit    ,
                      reg_det_tra_ag.nombres_infonavit    ,
                      22 spaces                           ,
                      reg_det_tra_ag.ident_lote_devol     ,
                      15 spaces                           ,
                      reg_det_tra_ag.nss_afore            ,
                      reg_det_tra_ag.rfc_afore            ,
                      30 spaces                           ,
                      reg_det_tra_ag.paterno_afore        ,
                      reg_det_tra_ag.materno_afore        ,
                      reg_det_tra_ag.nombres_afore        ,
                      30 spaces                           ,
                      reg_det_tra_ag.aivs_v92 * 1000000 USING "&&&&&&&&&&&&&&&",
                      reg_det_tra_ag.ind_dif_v92          ,
                      58 spaces                           ,
                      reg_det_tra_ag.aivs_v97 * 1000000 USING "&&&&&&&&&&&&&&&",
                      reg_det_tra_ag.ind_dif_v97          ,
                      16 spaces                           ,
                      reg_det_tra_ag.cod_result_operac    ,
                      reg_det_tra_ag.diag_proceso         ,
                      reg_det_tra_ag.nombre_imss          ,
                      reg_det_tra_ag.num_cred_infonavit USING "&&&&&&&&&&",
                      71 SPACES
END REPORT


FUNCTION genera_sum_tra_ag()
#gsta-----------------------

   LET reg_sum_tra_ag.tipo_registro      = "09"
   LET reg_sum_tra_ag.cantidad_reg_det   = cont_reg

   SELECT SUM(par97),
          SUM(par92)
   INTO   reg_sum_tra_ag.sum_aivs_v97,
          reg_sum_tra_ag.sum_aivs_v92
   FROM   safre_tmp:sdo_acr_ag

   LET g_sum = g_seg_modulo.ruta_envio CLIPPED,"/STAG"

   LET permisos = " chmod 777 ",g_sum CLIPPED
   RUN permisos

   START REPORT listado_3 TO g_sum
      OUTPUT TO REPORT listado_3(reg_sum_tra_ag.*) #3
   FINISH REPORT listado_3
END FUNCTION


REPORT listado_3(reg_sum_tra_ag)
#3---------------------------------
   DEFINE
      reg_sum_tra_ag RECORD
                        tipo_registro        CHAR(02),
                        cantidad_reg_det     INTEGER,
                        sum_aivs_v92         DECIMAL(22,6),
                        sum_aivs_v97         DECIMAL(22,6)
                     END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW
         PRINT
            COLUMN 01,reg_sum_tra_ag.tipo_registro          ,
                      reg_sum_tra_ag.cantidad_reg_det       USING"&&&&&&&&&",
                      30 spaces                             ,
                      reg_sum_tra_ag.sum_aivs_v92 * 1000000 USING"&&&&&&&&&&&&&&&&&&",
                      reg_sum_tra_ag.sum_aivs_v97 * 1000000 USING"&&&&&&&&&&&&&&&&&&",
                      653 SPACES
END REPORT


FUNCTION periodo()
#p----------------
   DEFINE
      s_anyo      SMALLINT,
      c_anyo      CHAR(4),
      mes         CHAR(2),
      c_periodo   CHAR(6)

   LET s_anyo = YEAR(fecha_envio)

   CASE MONTH(fecha_envio)
      WHEN 1
         LET mes = "06"
         LET s_anyo = s_anyo - 1
      WHEN 2
         LET mes = "06"
         LET s_anyo = s_anyo - 1
      WHEN 3
         LET mes = "01"
      WHEN 4
         LET mes = "01"
      WHEN 5
         LET mes = "02"
      WHEN 6
         LET mes = "02"
      WHEN 7
         LET mes = "03"
      WHEN 8
         LET mes = "03"
      WHEN 9
         LET mes = "04"
      WHEN 10
         LET mes = "04"
      WHEN 11
         LET mes = "05"
      WHEN 12
         LET mes = "05"
   END CASE

   LET c_anyo = s_anyo
   LET c_periodo = c_anyo,mes

   RETURN c_periodo
END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------
   DEFINE
      G_IMPRE    CHAR(300),
      gimpresion CHAR(300)

   LET HORA = TIME

   DECLARE cursor_1 CURSOR FOR
   SELECT folio,
          tipo_movimiento,
          subcuenta,
          fecha_valor,
          sum(monto_en_acciones),
          sum(monto_en_pesos)
   FROM   dis_provision
   WHERE  folio = vfolio
   AND    subcuenta in(4,8)
   AND    tipo_movimiento = 234
   GROUP BY 1,2,3,4
   ORDER BY 1,2,3

   LET pos = 1

   FOREACH cursor_1 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   INITIALIZE l_record[pos].* TO NULL

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
   END IF

   LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",vusuario CLIPPED,
                 ".LIS_PROV_AG_",HOY USING "DDMMYY","_",HORA[1,2],HORA[4,5]

   LET permisos = " chmod 777 ",G_IMPRE CLIPPED
   RUN permisos

   START REPORT rpt_cuenta_imp TO G_IMPRE
   CALL cuentas_impri()

   IF s_codigo_afore = 578 THEN               # 578 PEISSSTE
      --  LET gimpresion = "lp ",G_IMPRE
      --  RUN gimpresion
   ELSE
      LET gimpresion = "lp ",G_IMPRE
      RUN gimpresion
   END IF
END FUNCTION


FUNCTION cuentas_impri()
#-----------------------
   DEFINE
      i       INTEGER

   FOR i=1 TO pos - 1
      LET g.folio              = l_record[i].folio
      LET g.tipo_movimiento    = l_record[i].tipo_movimiento
      LET g.subcuenta          = l_record[i].subcuenta
      LET g.fecha_valor        = l_record[i].fecha_valor
      LET g.monto_en_acciones  = l_record[i].monto_en_acciones
      LET g.monto_en_pesos     = l_record[i].monto_en_pesos

      OUTPUT TO REPORT rpt_cuenta_imp(g.*)
   END FOR

   FINISH REPORT rpt_cuenta_imp
END FUNCTION


REPORT rpt_cuenta_imp(g)
#-----------------------
   DEFINE g RECORD
      folio               INTEGER,
      tipo_movimiento     SMALLINT,
      subcuenta           SMALLINT,
      fecha_valor         DATE,
      monto_en_acciones   DECIMAL(18,6),
      monto_en_pesos      DECIMAL(18,6)
   END RECORD

   DEFINE
      tot_viv             DECIMAL(18,6)

   DEFINE
      tot_reg             INTEGER

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

   FORMAT
      PAGE HEADER

         SELECT SUM(monto_en_pesos)
         INTO   tot_viv
         FROM   dis_provision
         WHERE  folio = vfolio

         SELECT COUNT(*)
         INTO   tot_reg
         FROM   safre_tmp:det_acr_ag d
         WHERE  d.estado = 0

         PRINT COLUMN  2,"ACRB052",
               COLUMN 15,"LISTADO DE PROVISION DE ANUALIDADES GARANTIZADAS",
               COLUMN 65, HOY USING "dd/mm/yyyy"          

         PRINT
         PRINT COLUMN  2,"NOMBRE ARCHIVO A ENVIAR : ",
                         g_seg_modulo.ruta_envio CLIPPED,"/",
                         "TRANS_ANU_GAR.",HOY USING "MMDD","-",HORA[1,2],HORA[4,5]

         SKIP 2 LINE

         PRINT COLUMN  3,"FOLIO",
               COLUMN 10,"MOV",
               COLUMN 15,"SUB",
               COLUMN 20,"FECHA VALOR",
               COLUMN 33,"MONTO PARTICIP",
               COLUMN 55,"MONTO EN PESOS"

         SKIP 2 LINE

      ON EVERY ROW
         PRINT COLUMN  3,g.folio USING "<<<&&",
               COLUMN 10,g.tipo_movimiento USING "#####",
               COLUMN 15,g.subcuenta USING "#####",
               COLUMN 20,g.fecha_valor USING "dd-mm-yyyy",
               COLUMN 33,g.monto_en_acciones USING "##########&.######",
               COLUMN 55,g.monto_en_pesos USING "##########&.######"

      PAGE TRAILER

        SKIP 2 LINES
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"

        PAUSE "Presione enter para continuar...."

      ON LAST ROW
         SKIP 4 LINES
         PRINT COLUMN 2, "Total saldos viv         : ",
                         tot_viv USING "############&.######"

         PRINT
         PRINT COLUMN 2, "Total registros a enviar : ", tot_reg
END REPORT

