#############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                           #
#Propietario       => E.F.P.                                                #
#Programa ACRB019  => GENERA TRANSFERENCIA DE SALDOS DE USO DE GARANTIA     #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 30 de abril de 1999                                   #
#Fecha actualiza   => 4 DE AGOSTO DE 2004                                   #
#Modifica          => MAURO MUNIZ CABALLERO                                 #
#               Se adecuo para proceso de participaciones                   #
#Sistema           => ACR                                                   #
#Modifico          => DMR 30/JULIO/2014 MLM-2683 STARTLOG PERSONALIZADO,    #
#                  => Se modifico validacion para no permitir que se provi- #
#                     sione y liquide monto mayor al saldo es decir, evitar #
#                     quebrantos verificando saldo y montos comprometidos   #
#Modifico          => PST-1596 confirmacion de impresion de listados        #
#############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      HOY                   ,
      fecha_envio           ,
      fecha_envio2          ,
      fecha_envio3          ,
      fecha_envio4          ,
      fecha_envio_banx      ,
      fecha_presentacion    DATE

   DEFINE
      enter                 CHAR(1),
      c4_HORA               CHAR(4),
      HORA                  CHAR(5),
      periodo_1             CHAR(6),
      c8_HOY                CHAR(8),
      vusuario              CHAR(8),
      nom_arch              CHAR(50),
      g_cza                 CHAR(100),
      g_det                 CHAR(100),
      g_sum                 CHAR(100),
      cat                   CHAR(300)

   DEFINE
      dias_mes              ,
      existe                ,
      s_periodo             ,
      sw_1                  ,
      s_lotes_num           ,
      s_lotes_correlativo   ,
      s_codigo_afore        SMALLINT,
      vfolio                INTEGER

   DEFINE
      cont_reg              DECIMAL(10,0),
      vsum_sdo_viv_97       DECIMAL(15,2),
      vsum_sdo_viv92        DECIMAL(15,2)

   DEFINE
      sdo_part_v97          ,
      sdo_tot_v97           DECIMAL(22,6)

   DEFINE
      reg_cza_tra_ctas      RECORD
                               tipo_registro        CHAR(02),
                               ident_servicio       CHAR(02),
                               ident_operacion      CHAR(02),
                               tipo_ent_origen      CHAR(02),
                               cve_ent_origen       CHAR(03),
                               tipo_ent_destino     CHAR(02),
                               cve_ent_destino      CHAR(03),
                               ent_fed_envio_lote   CHAR(03),
                               fecha_envio          CHAR(08),
                               consec_lote_dia      SMALLINT,
                               cod_result_operac    CHAR(02),
                               mot_rechazo_lote     CHAR(09) 
                            END RECORD

   DEFINE
      reg_det_tra_ctas      RECORD
                               tipo_registro        CHAR(002),
                               cont_servicio        DECIMAL(10,0),
                               tipo_recep_cuenta    CHAR(002),
                               cve_recep_cuenta     CHAR(003),
                               tipo_ced_cuenta      CHAR(002),
                               cve_ced_cuenta       CHAR(003),
                               tipo_transferencia   CHAR(002),
                               fecha_envio          CHAR(8),
                               fecha_banxico        CHAR(8),
                               n_unico_infonavit    CHAR(018),
                               nss_infonavit        CHAR(011),
                               rfc_infonavit        CHAR(013),
                               paterno_infonavit    CHAR(040),
                               materno_infonavit    CHAR(040),
                               nombres_infonavit    CHAR(040),
                               ident_lote_devol     CHAR(016),
                               nss_afore            CHAR(011),
                               rfc_afore            CHAR(013),
                               paterno_afore        CHAR(040),
                               materno_afore        CHAR(040),
                               nombres_afore        CHAR(040),
                               partic_v97           DECIMAL(22,6),
                               sdo_apo_viv_97       DECIMAL(15,2),
                               sdo_apo_viv_92       DECIMAL(15,2),
                               cod_result_operac    CHAR(002),
                               diag_proceso         CHAR(015),
                               nombre_imss          CHAR(050),
                               num_cred_infonavit   DECIMAL(10,0),
                               ints_viv_97          DECIMAL(15,2),
                               ints_viv_92          DECIMAL(15,2),
                               periodo_pago         CHAR(6)
                            END RECORD

   DEFINE
      reg_sum_tra_ctas      RECORD
                               tipo_registro        CHAR(02),
                               cantidad_reg_det     INTEGER,
                               sum_partic_v97       DECIMAL(22,6),
                               sum_sdo_viv_97       DECIMAL(15,2),
                               sum_sdo_viv_92       DECIMAL(15,2),
                               ints_viv_97          DECIMAL(15,2),
                               ints_viv_92          DECIMAL(15,2)
                            END RECORD

   DEFINE
      g_reg                 RECORD
                               nss                  CHAR(11),
                               par97                DECIMAL(22,2),
                               viv97                DECIMAL(15,2)
                            END RECORD

   DEFINE
      g_param_taa           RECORD LIKE seg_modulo.*

END GLOBALS
 
 
MAIN
   DEFER INTERRUPT
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST

   CALL STARTLOG(FGL_GETENV("USER")||".ACRB019.log")    #MLM-2683
   CALL inicio()              #i
   CALL proceso_principal()   #pp
END MAIN


FUNCTION inicio()
#i---------------
   LET HOY     = TODAY
   LET HORA    = TIME
   LET c4_HORA = HORA[1,2],HORA[4,5]

   LET s_lotes_num = 4

   SELECT codigo_afore
   INTO   s_codigo_afore
   FROM   tab_afore_local

   SELECT *, user
   INTO   g_param_taa.*, vusuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   LET cont_reg = 1
   LET reg_det_tra_ctas.sdo_apo_viv_97 = 0
   LET reg_det_tra_ctas.sdo_apo_viv_92 = 0
   LET reg_sum_tra_ctas.sum_sdo_viv_97 = 0
   LET reg_sum_tra_ctas.sum_sdo_viv_92 = 0
   LET sdo_part_v97   = 0
   LET sdo_tot_v97    = 0

   INITIALIZE g_reg.* TO NULL
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------
   OPEN WINDOW acrb0031 AT 4,4 WITH FORM "ACRB0021" ATTRIBUTE(BORDER)
   DISPLAY " ACRB019     GENERA ARCHIVO SALDOS USO CREDITO GARANTIA                        " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " < CTRL-C > Salir " AT 1,26              
   DISPLAY HOY USING"DD-MM-YYYY" AT 2,60 ATTRIBUTE(REVERSE)

   INITIALIZE fecha_envio TO NULL
   LET c8_HOY      = HOY USING "YYYYMMDD"

   INPUT BY NAME fecha_envio WITHOUT DEFAULTS
      AFTER FIELD fecha_envio
         IF fecha_envio IS NULL THEN
            ERROR "LA FECHA DE ENVIO NO PUEDE SER NULA"
            NEXT FIELD fecha_envio
         ELSE
            SELECT "OK"
            FROM   taa_folio
            WHERE  MONTH(fecha) = MONTH(fecha_envio)
            AND    YEAR(fecha) = YEAR(fecha_envio)
            AND    tipo = 9
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR " YA SE CARGO UN ARCHIVO DE USO DE GARANTIA PARA ESTE MES Y AÑO !!"
               NEXT FIELD fecha_envio
            END IF
         END IF
      EXIT INPUT

      ON KEY ( INTERRUPT )
         EXIT PROGRAM
   END INPUT

   WHILE TRUE
      PROMPT "ESTA SEGURO S/N ? " FOR enter
      IF enter MATCHES "[sSnN]" THEN
         IF enter MATCHES "[sS]" THEN
            EXIT WHILE
         ELSE
            DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
            EXIT PROGRAM
         END IF
      END IF
   END WHILE

   LET fecha_envio2 = MDY(MONTH(fecha_envio),1,YEAR(fecha_envio))
   LET fecha_envio3 = fecha_envio2 + 1 UNITS MONTH 

   SELECT folio
   INTO   vfolio
   FROM   taa_folio
   WHERE  MONTH(fecha) = MONTH(fecha_envio)
   AND    YEAR(fecha) = YEAR(fecha_envio)
   AND    tipo = 9

   IF STATUS = NOTFOUND THEN
      INSERT INTO glo_folio
      VALUES (0)                               # folio de liquidacion

      SELECT MAX(folio)
      INTO   vfolio
      FROM   glo_folio

      INSERT INTO taa_folio VALUES(vfolio,9,hoy,vusuario)
   END IF 

   DISPLAY "F O L I O      :  ", vfolio USING "#######" AT 15,11
   SLEEP 3

   DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

   CALL crea_tablas()            #ct
   CALL genera_cza_tra_ctas()    #gctc
   CALL genera_det_tra_ctas()    #gdtc
   CALL genera_sum_tra_ctas()    #gstc

   LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CUC ",
                    g_param_taa.ruta_envio CLIPPED,"/DUC ",
                    g_param_taa.ruta_envio CLIPPED,"/SUC > ",
                    g_param_taa.ruta_envio CLIPPED,"/",
                    "USO_CRED.",hoy USING "MMDD","-",c4_HORA

   RUN cat

   DISPLAY "Archivo generado : ",
           g_param_taa.ruta_envio CLIPPED,"/USO_CRED.",hoy USING "MMDD","-",
           c4_HORA AT 11,7  

   PROMPT "PROCESO FINALIZADO,   [Enter] para salir" FOR enter

   CLOSE WINDOW acrb0031
END FUNCTION


FUNCTION crea_tablas()
#ct-------------------
    WHENEVER ERROR CONTINUE
       DROP TABLE sdo_uso_cred
       DROP TABLE cred_nss
    WHENEVER ERROR STOP

    CREATE TEMP TABLE sdo_uso_cred
       (nss    CHAR(11),
        par97  DECIMAL(22,2),
        viv97  DECIMAL(15,2))

    CREATE TEMP TABLE cred_nss
       (nss    CHAR(11),
        acc    DECIMAL(22,2),
        pesos  DECIMAL(18,2))
END FUNCTION


FUNCTION genera_cza_tra_ctas()
#gctc--------------------------
   LET reg_cza_tra_ctas.tipo_registro      = "01"
   LET reg_cza_tra_ctas.ident_servicio     = "02"
   LET reg_cza_tra_ctas.ident_operacion    = "09"
   LET reg_cza_tra_ctas.tipo_ent_origen    = "01"
   LET reg_cza_tra_ctas.cve_ent_origen     = s_codigo_afore
   LET reg_cza_tra_ctas.tipo_ent_destino   = "04"
   LET reg_cza_tra_ctas.cve_ent_destino    = "002"
   LET reg_cza_tra_ctas.ent_fed_envio_lote = "009"
   LET reg_cza_tra_ctas.fecha_envio        = fecha_envio USING"YYYYMMDD"
   LET reg_cza_tra_ctas.consec_lote_dia    = 6
   LET reg_cza_tra_ctas.cod_result_operac  = NULL
   LET reg_cza_tra_ctas.mot_rechazo_lote   = NULL
  
   LET g_cza = g_param_taa.ruta_envio CLIPPED,"/CUC"

   START REPORT listado_1 TO g_cza
      OUTPUT TO REPORT listado_1(reg_cza_tra_ctas.*) #1
   FINISH REPORT listado_1
END FUNCTION


REPORT listado_1(reg_cza_tra_ctas)
#1--------------------------------
   DEFINE
      reg_cza_tra_ctas RECORD
                          tipo_registro        CHAR(02),
                          ident_servicio       CHAR(02),
                          ident_operacion      CHAR(02),
                          tipo_ent_origen      CHAR(02),
                          cve_ent_origen       CHAR(03),
                          tipo_ent_destino     CHAR(02),
                          cve_ent_destino      CHAR(03),
                          ent_fed_envio_lote   CHAR(03),
                          fecha_envio          CHAR(08),
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
         PRINT COLUMN 01,reg_cza_tra_ctas.tipo_registro,
                         reg_cza_tra_ctas.ident_servicio,
                         reg_cza_tra_ctas.ident_operacion,
                         reg_cza_tra_ctas.tipo_ent_origen,
                         reg_cza_tra_ctas.cve_ent_origen,
                         reg_cza_tra_ctas.tipo_ent_destino,
                         reg_cza_tra_ctas.cve_ent_destino,
                         reg_cza_tra_ctas.ent_fed_envio_lote,
                         reg_cza_tra_ctas.fecha_envio,
                         reg_cza_tra_ctas.consec_lote_dia  USING "&&&",
                         2 spaces,
                         reg_cza_tra_ctas.cod_result_operac,
                         reg_cza_tra_ctas.mot_rechazo_lote,
                         687 spaces
END REPORT


FUNCTION genera_det_tra_ctas()
#gdtc-------------------------
   DEFINE
      fecha_prov           ,
      dt_fc_vl             ,
      fecha_envio_archivo  DATE 

   DEFINE
      c8_banxico           CHAR(08)

   DEFINE
      vprecio_accion       DECIMAL(19,14),
      vprecio_sig          DECIMAL(19,14)

   DEFINE
      sdo_viv_v97          DECIMAL(22,6),
      tot_viv_v97          DECIMAL(18,2),
      tot_par_v97          DECIMAL(18,2)

   SELECT @precio_del_dia
   INTO vprecio_accion
   FROM glo_valor_accion
   WHERE fecha_valuacion = fecha_envio2
   AND   codigo_siefore  = 11

   SELECT @precio_del_dia
   INTO vprecio_sig
   FROM glo_valor_accion
   WHERE fecha_valuacion = fecha_envio3
   AND   codigo_siefore  = 11

   INSERT INTO cred_nss
   SELECT nss, sum(monto_en_acciones), sum(monto_en_pesos)
   FROM   dis_cuenta
   WHERE  nss IN(SELECT UNIQUE @nss_afore
                 FROM   safre_tmp:det_garantia
                 WHERE  @estado = 0)
   AND    subcuenta = 4
   GROUP BY 1

   DECLARE cur_1 CURSOR FOR
   SELECT  tipo_transferencia,
           n_unico_infonavit,
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
           partic_v97,
           saldo_viv_97,
           saldo_viv_92,
           cod_result_operac,
           diag_proceso,
           nombre_imss,
           num_cred_infonavit,
           cont_servicio,
           periodo_pago
   FROM    safre_tmp:det_garantia
   WHERE   estado = 0

   LET reg_det_tra_ctas.tipo_registro      = "02"
   LET reg_det_tra_ctas.cont_servicio      = cont_reg
   LET reg_det_tra_ctas.tipo_recep_cuenta  = "04"
   LET reg_det_tra_ctas.cve_recep_cuenta   = "002"
   LET reg_det_tra_ctas.tipo_ced_cuenta    = "01"
   LET reg_det_tra_ctas.cve_ced_cuenta     = s_codigo_afore
   LET reg_det_tra_ctas.fecha_envio        = fecha_envio  USING"YYYYMMDD"
   LET reg_det_tra_ctas.ints_viv_97        = 0 
   LET reg_det_tra_ctas.ints_viv_92        = 0 

   LET reg_det_tra_ctas.sdo_apo_viv_92 = 0

   LET g_det = g_param_taa.ruta_envio CLIPPED,"/DUC" 

   START REPORT listado_2 TO g_det
      LET sw_1 = 0
      FOREACH cur_1 INTO reg_det_tra_ctas.tipo_transferencia,
                         reg_det_tra_ctas.n_unico_infonavit,
                         reg_det_tra_ctas.nss_infonavit,
                         reg_det_tra_ctas.rfc_infonavit,
                         reg_det_tra_ctas.paterno_infonavit,
                         reg_det_tra_ctas.materno_infonavit,
                         reg_det_tra_ctas.nombres_infonavit,
                         reg_det_tra_ctas.ident_lote_devol,
                         reg_det_tra_ctas.nss_afore,
                         reg_det_tra_ctas.rfc_afore,
                         reg_det_tra_ctas.paterno_afore,
                         reg_det_tra_ctas.materno_afore,
                         reg_det_tra_ctas.nombres_afore,
                         reg_det_tra_ctas.partic_v97,
                         reg_det_tra_ctas.sdo_apo_viv_97,
                         reg_det_tra_ctas.sdo_apo_viv_92,
                         reg_det_tra_ctas.cod_result_operac,
                         reg_det_tra_ctas.diag_proceso,
                         reg_det_tra_ctas.nombre_imss,
                         reg_det_tra_ctas.num_cred_infonavit,
                         reg_det_tra_ctas.cont_servicio,
                         reg_det_tra_ctas.periodo_pago

         LET reg_det_tra_ctas.fecha_banxico = fecha_envio3 USING 'YYYYMMDD'

         LET g_reg.nss   = reg_det_tra_ctas.nss_afore

         LET g_reg.par97  = 0
         LET g_reg.viv97  = 0
         LET sdo_part_v97 = 0

         SELECT NVL(@acc, 0)
         INTO   sdo_part_v97
         FROM   cred_nss
         WHERE  @nss = reg_det_tra_ctas.nss_afore

         IF sdo_part_v97 <= 0 OR sdo_part_v97 IS NULL THEN
            LET reg_det_tra_ctas.sdo_apo_viv_97 = 0
            LET reg_det_tra_ctas.partic_v97     = 0
            LET tot_par_v97                     = 0
            LET tot_viv_v97                     = 0
         ELSE
            LET tot_par_v97 = sdo_part_v97

            IF tot_par_v97 < reg_det_tra_ctas.partic_v97 THEN
               LET reg_det_tra_ctas.partic_v97 = tot_par_v97
               LET sdo_viv_v97 = reg_det_tra_ctas.partic_v97 * vprecio_sig
               LET reg_det_tra_ctas.sdo_apo_viv_97 =  sdo_viv_v97
            ELSE
               LET sdo_viv_v97 = reg_det_tra_ctas.partic_v97 * vprecio_sig
               LET reg_det_tra_ctas.sdo_apo_viv_97 =  sdo_viv_v97
            END IF
         END IF

         UPDATE cred_nss
         SET    acc   = acc   - reg_det_tra_ctas.partic_v97,
                pesos = pesos - reg_det_tra_ctas.sdo_apo_viv_97
         WHERE  @nss  = reg_det_tra_ctas.nss_afore

         LET reg_det_tra_ctas.fecha_banxico = fecha_envio3 USING 'YYYYMMDD'
         LET fecha_prov = fecha_envio3

         LET g_reg.viv97  = reg_det_tra_ctas.sdo_apo_viv_97
         LET g_reg.par97  = reg_det_tra_ctas.partic_v97
         LET sdo_tot_v97  = reg_det_tra_ctas.sdo_apo_viv_97 * (-1)
         LET sdo_part_v97 = reg_det_tra_ctas.partic_v97     * (-1)

         IF sdo_tot_v97 < 0 THEN
            INSERT INTO dis_provision VALUES
              (236,                              #tipo_movimiento
               4,                                #subcuenta
               11,                               #siefore
               vfolio,                           #folio
               reg_det_tra_ctas.cont_servicio,   #consecutivo_lote
               reg_det_tra_ctas.nss_afore,       #nss
               "",                               #curp
               "9999",                           #folio_sua
               fecha_envio3,                     #fecha_pago
               fecha_envio3,                     #fecha_valor
               fecha_envio3,                     #fecha_conversion
               sdo_tot_v97,                      #monto_en_pesos
               sdo_part_v97,                     #monto_en_acciones
               vprecio_accion,                   #precio_accion
               0,                                #dias_cotizados
               "",                               #sucursal
               "USO-CRED",                       #id_aportante
               5,                                #estado
               today,                            #fecha_proceso
               vusuario,                         #usuario
               fecha_envio,                      #fecha_archivo
               0)                                #etiqueta
         END IF

         LET sw_1 = 1
         OUTPUT TO REPORT listado_2(reg_det_tra_ctas.*) #2

         DISPLAY "Registros Procesados : ", cont_reg AT 17,11

         INSERT INTO sdo_uso_cred
         VALUES(g_reg.*)

         LET cont_reg = cont_reg + 1

         LET sdo_tot_v97  = 0
         LET sdo_part_v97 = 0
         LET tot_viv_v97  = 0
         LET tot_par_v97  = 0
      END FOREACH

      IF sw_1 = 0 THEN
         DISPLAY "NO SE ENCONTRARON REGISTROS " AT 19,1 ATTRIBUTE(REVERSE)
         SLEEP 3
         EXIT PROGRAM
      END IF
      
      LET cont_reg = cont_reg - 1
      
   FINISH REPORT listado_2 #2
END FUNCTION


REPORT listado_2(reg_det_tra_ctas)
#2--------------------------------
   DEFINE
      reg_det_tra_ctas RECORD
                          tipo_registro        CHAR(002),
                          cont_servicio        DECIMAL(10,0),
                          tipo_recep_cuenta    CHAR(002),
                          cve_recep_cuenta     CHAR(003),
                          tipo_ced_cuenta      CHAR(002),
                          cve_ced_cuenta       CHAR(003),
                          tipo_transferencia   CHAR(002),
                          fecha_envio          CHAR(8),
                          fecha_banxico        CHAR(8),
                          n_unico_infonavit    CHAR(018),
                          nss_infonavit        CHAR(011),
                          rfc_infonavit        CHAR(013),
                          paterno_infonavit    CHAR(040),
                          materno_infonavit    CHAR(040),
                          nombres_infonavit    CHAR(040),
                          ident_lote_devol     CHAR(016),
                          nss_afore            CHAR(011),
                          rfc_afore            CHAR(013),
                          paterno_afore        CHAR(040),
                          materno_afore        CHAR(040),
                          nombres_afore        CHAR(040),
                          partic_v97           DECIMAL(22,6),
                          sdo_apo_viv_97       DECIMAL(15,2),
                          sdo_apo_viv_92       DECIMAL(15,2),
                          cod_result_operac    CHAR(002),
                          diag_proceso         CHAR(015),
                          nombre_imss          CHAR(050),
                          num_cred_infonavit   DECIMAL(10,0),
                          ints_viv_97          DECIMAL(15,2),
                          ints_viv_92          DECIMAL(15,2),
                          periodo_pago         CHAR(6)
                       END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 
         PRINT COLUMN 01,reg_det_tra_ctas.tipo_registro,
                         cont_reg                           USING "&&&&&&&&&&",
                         reg_det_tra_ctas.tipo_recep_cuenta,
                         reg_det_tra_ctas.cve_recep_cuenta,
                         reg_det_tra_ctas.tipo_ced_cuenta,
                         reg_det_tra_ctas.cve_ced_cuenta    USING "&&&",
                         reg_det_tra_ctas.tipo_transferencia,
                         reg_det_tra_ctas.fecha_envio,
                         reg_det_tra_ctas.fecha_banxico,
                         reg_det_tra_ctas.n_unico_infonavit,
                         reg_det_tra_ctas.nss_infonavit,
                         15 spaces,
                         reg_det_tra_ctas.rfc_infonavit,
                         reg_det_tra_ctas.paterno_infonavit,
                         reg_det_tra_ctas.materno_infonavit,
                         reg_det_tra_ctas.nombres_infonavit,
                         22 spaces,
                         reg_det_tra_ctas.ident_lote_devol,
                         15 spaces,
                         reg_det_tra_ctas.nss_afore,
                         reg_det_tra_ctas.rfc_afore,
                         30 spaces,
                         reg_det_tra_ctas.paterno_afore,
                         reg_det_tra_ctas.materno_afore,
                         reg_det_tra_ctas.nombres_afore,
                         30 spaces,
                         reg_det_tra_ctas.partic_v97     * 1000000 USING "&&&&&&&&&&&&&&&",
                         reg_det_tra_ctas.sdo_apo_viv_97 * 100     USING "&&&&&&&&&&&&&&&",
                         78 spaces,
                         reg_det_tra_ctas.cod_result_operac,
                         reg_det_tra_ctas.diag_proceso,
                         reg_det_tra_ctas.nombre_imss,
                         reg_det_tra_ctas.num_cred_infonavit USING "&&&&&&&&&&",
                         53 spaces,
                         reg_det_tra_ctas.periodo_pago,
                         12 spaces
END REPORT


FUNCTION genera_sum_tra_ctas()
#gstc-------------------------
   LET reg_sum_tra_ctas.tipo_registro      = "09"
   LET reg_sum_tra_ctas.cantidad_reg_det   = cont_reg
   LET reg_sum_tra_ctas.ints_viv_97        = 0
   LET reg_sum_tra_ctas.ints_viv_92        = 0

   SELECT SUM(viv97),
          SUM(par97)
   INTO   reg_sum_tra_ctas.sum_sdo_viv_97,
          reg_sum_tra_ctas.sum_partic_v97
   FROM   safre_tmp:sdo_uso_cred

   LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SUC"

   START REPORT listado_3 TO g_sum
      OUTPUT TO REPORT listado_3(reg_sum_tra_ctas.*) #3
   FINISH REPORT listado_3
END FUNCTION


REPORT listado_3(reg_sum_tra_ctas)
#3--------------------------------
   DEFINE
      reg_sum_tra_ctas RECORD
                          tipo_registro        CHAR(02),
                          cantidad_reg_det     INTEGER,
                          sum_partic_v97       DECIMAL(22,6),
                          sum_sdo_viv_97       DECIMAL(15,2),
                          sum_sdo_viv_92       DECIMAL(15,2),
                          ints_viv_97          DECIMAL(15,2),
                          ints_viv_92          DECIMAL(15,2)
                       END RECORD

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW 
         PRINT COLUMN 01,reg_sum_tra_ctas.tipo_registro,
                         reg_sum_tra_ctas.cantidad_reg_det   USING"&&&&&&&&&",
                         30 spaces,
                         reg_sum_tra_ctas.sum_partic_v97 * 1000000 USING"&&&&&&&&&&&&&&&&&&",
                         reg_sum_tra_ctas.sum_sdo_viv_97 * 100     USING"&&&&&&&&&&&&&&&",
                         656 spaces
END REPORT


FUNCTION periodo()
#p----------------
   DEFINE 
      s_anyo        SMALLINT,
      c_anyo        CHAR(4),
      mes           CHAR(2),
      c_periodo     CHAR(6)

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

