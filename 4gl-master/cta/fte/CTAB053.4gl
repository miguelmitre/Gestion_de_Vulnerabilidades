################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAB053    => REPORTE DE CIFRAS DEL ESTADO DE CUENTA                 #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 22 de SEPTIEMBRE DE 2010                               #
################################################################################
DATABASE safre_af
################################################################################
GLOBALS
   DEFINE gc_mensaje1           ,
          gc_mensaje2           ,
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(8)  ,
          gc_respuesta          CHAR(001)

   DEFINE gd_fecha_corte  DATE,
          gd_fecha_inicio DATE
   DEFINE gi_afore        SMALLINT

   DEFINE gr_seg_modulo RECORD
   	      modulo_cod  CHAR(04),
          ruta_envio  CHAR(40)
   END RECORD

   DEFINE gc_archivo CHAR(300)

   DEFINE gi_paso,
          gi_mes SMALLINT

   DEFINE hoy        DATE

   DEFINE gr_cifras_adm_reg RECORD
   	     activas    INTEGER,
   	     inactivas  INTEGER,
   	     saldo_cero INTEGER,
   	     total      INTEGER
   END RECORD

   DEFINE gr_cifras_adm_asig RECORD
   	     activas    INTEGER,
   	     inactivas  INTEGER,
   	     saldo_cero INTEGER,
   	     total      INTEGER
   END RECORD

   DEFINE gr_extra_traspaso RECORD
   	      activas    INTEGER,
   	      inactivas  INTEGER,
   	      saldo_cero INTEGER,
   	      total      INTEGER
   END RECORD

   DEFINE gr_extra_retiro RECORD
   	      activas    INTEGER,
   	      inactivas  INTEGER,
   	      saldo_cero INTEGER,
   	      total      INTEGER
   END RECORD

   DEFINE gar_det_subcta_act ARRAY[14] OF RECORD
   	   desc_subcuenta CHAR(16)     ,
   	   det_siefore ARRAY[6] OF RECORD
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),
   	      pesos          DECIMAL(22,6)
   	   END RECORD
   END RECORD

   DEFINE gar_det_subcta_inac ARRAY[14] OF RECORD
   	   desc_subcuenta CHAR(16)     ,
   	   det_siefore ARRAY[6] OF RECORD
   	      siefore        SMALLINT     ,
   	      acciones       DECIMAL(22,6),
   	      pesos          DECIMAL(22,6)
   	   END RECORD
   END RECORD

   DEFINE gar_sumario ARRAY[2] OF RECORD
   	   detalle ARRAY[6] OF RECORD
   	      acciones       DECIMAL(22,6),
   	      pesos          DECIMAL(22,6)
   	   END RECORD
   END RECORD


   DEFINE gs_activos   SMALLINT,
          gs_inactivos SMALLINT
END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   gi_afore
   FROM   safre_af:tab_afore_local

   LET gr_seg_modulo.modulo_cod = "cta"

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   CALL inicializa()

   CALL crea_tablas()
   CALL proceso()
END MAIN
################################################################################
FUNCTION inicializa()
   DEFINE ls_cont    SMALLINT
   DEFINE ls_siefore SMALLINT

   LET hoy    = TODAY
   LET gi_mes = MONTH(hoy)

   LET gr_cifras_adm_reg.activas     = 0
   LET gr_cifras_adm_reg.inactivas   = 0
   LET gr_cifras_adm_reg.saldo_cero  = 0
   LET gr_cifras_adm_reg.total       = 0

   LET gr_cifras_adm_asig.activas    = 0
   LET gr_cifras_adm_asig.inactivas  = 0
   LET gr_cifras_adm_asig.saldo_cero = 0
   LET gr_cifras_adm_asig.total      = 0

   LET gr_extra_traspaso.activas    = 0
   LET gr_extra_traspaso.inactivas  = 0
   LET gr_extra_traspaso.saldo_cero = 0
   LET gr_extra_traspaso.total      = 0

   LET gr_extra_retiro.activas    = 0
   LET gr_extra_retiro.inactivas  = 0
   LET gr_extra_retiro.saldo_cero = 0
   LET gr_extra_retiro.total      = 0

   LET gs_activos   = 1
   LET gs_inactivos = 0

   FOR ls_cont = 1 TO 14
   	  #Inicializa detalles de subcuenta
   	  CASE ls_cont
   	  	 WHEN  1 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'RETIR0 92 IMSS'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'RETIR0 92 IMSS'
         WHEN  2 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'RCV 97'
         	       LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'RCV 97'
         WHEN  3 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'CUOTA SOCIAL'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'CUOTA SOCIAL'
         WHEN  4 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'COMPLEMENTARIAS'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'COMPLEMENTARIAS'
         WHEN  5 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'VOLUNTARIAS'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'VOLUNTARIAS'
         WHEN  6 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'VIV92 IMSS'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'VIV92 IMSS'
         WHEN  7 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'VIV97 IMSS'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'VIV97 IMSS'
         WHEN  8 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'RETIRO 92 ISSSTE'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'RETIRO 92 ISSSTE'
         WHEN  9 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'RCV ISSSTE'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'RCV ISSSTE'
         WHEN 10 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'AHORRO SOL.'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'AHORRO SOL.'
         WHEN 11 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'CS ISSSTE'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'CS ISSSTE'
         WHEN 12 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'BONO PENSION'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'BONO PENSION'
         WHEN 13 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'FOVISSSTE 92'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'FOVISSSTE 92'
         WHEN 14 LET gar_det_subcta_act[ls_cont].desc_subcuenta  = 'FOVISSSTE 2008'
                 LET gar_det_subcta_inac[ls_cont].desc_subcuenta = 'FOVISSSTE 2008'
   	  END CASE

      #Cada subcuenta tiene 6 siefores
   	  FOR ls_siefore = 1 TO 6
   	  	 LET gar_det_subcta_act[ls_cont].det_siefore[ls_siefore].siefore  = ls_siefore
   	  	 LET gar_det_subcta_inac[ls_cont].det_siefore[ls_siefore].siefore = ls_siefore

   	  	 LET gar_det_subcta_act[ls_cont].det_siefore[ls_siefore].acciones  = 0
   	  	 LET gar_det_subcta_inac[ls_cont].det_siefore[ls_siefore].acciones = 0

   	  	 LET gar_det_subcta_act[ls_cont].det_siefore[ls_siefore].pesos     = 0
   	  	 LET gar_det_subcta_inac[ls_cont].det_siefore[ls_siefore].pesos    = 0
   	  END FOR
   END FOR

   #Inicializar sumario
   FOR ls_cont = 1 TO 2
   	  FOR ls_siefore = 1 TO 6
   	  	  LET gar_sumario[ls_cont].detalle[ls_siefore].acciones = 0
   	  	  LET gar_sumario[ls_cont].detalle[ls_siefore].pesos    = 0
   	  END FOR
   END FOR

END FUNCTION
################################################################################
FUNCTION crea_tablas()
   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldo_edocta_bono
   WHENEVER ERROR STOP

   CREATE TABLE tmp_saldo_edocta_bono(
      nss               CHAR(11)     ,
      fecha_conversion  DATE         ,
      monto_en_acciones DECIMAL(22,6),
      monto_en_pesos    DECIMAL(22,6),
      udis              DECIMAL(22,6)
   )

   DATABASE safre_af
END FUNCTION
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB0531" ATTRIBUTE(BORDER)
   DISPLAY "CTAB053 CIFRAS DE EMISION DE ESTADOS DE CUENTA CONSAR <Ctrl-C> SALIR   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "CIFRAS ESTADO DE CUENTA"
      COMMAND "Reporte de cifras"
         CALL lee_fecha()

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION lee_fecha()

   DEFINE li_paso  SMALLINT,
          lc_opc   CHAR(1)

   DEFINE li_flag SMALLINT

   DEFINE lc_error_msg CHAR(40)

   LET gc_mensaje1 = "ESTE PROCESO GENERARA EL REPORTE DE CIFRAS"
   LET gc_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

   INITIALIZE gd_fecha_corte TO NULL

   INPUT BY NAME gd_fecha_corte WITHOUT DEFAULTS
      BEFORE INPUT
         DISPLAY BY NAME gc_mensaje1,
                         gc_mensaje2

      BEFORE FIELD gd_fecha_corte
         IF gi_mes <= 4 THEN
         	  LET gd_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
         ELSE
            IF gi_mes > 4 AND gi_mes <= 8 THEN
            	  LET gd_fecha_corte = MDY (4,30, YEAR(TODAY))
            ELSE
            	  IF gi_mes > 8 AND gi_mes <= 12 THEN
            	  	 LET gd_fecha_corte = MDY (8,31, YEAR(TODAY))
            	  END IF
            END IF
         END IF

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DEL PERIODO"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         ELSE
         	  --VERIFICA SI YA SE HA EJECUTADO LA PREPARACION PARA LA FECHA CORTE
         	  CALL valida_preparacion(gd_fecha_corte) RETURNING li_flag
            IF li_flag = 0 THEN
            	 NEXT FIELD gd_fecha_corte
            END IF
         END IF

      WHILE TRUE
         PROMPT "ESTA SEGURO DE CONTINUAR EJECUCION S/N:" FOR CHAR lc_opc

         IF lc_opc  MATCHES "[SsNn]" THEN
            IF lc_opc MATCHES "[Ss]" THEN
            	 CALL archivo()
            	 SLEEP 2
               ERROR ""
               CLEAR FORM
            	 EXIT INPUT
            ELSE
               ERROR "PROCESO CANCELADO."
               SLEEP 2
               ERROR ""
               CLEAR FORM
               EXIT INPUT
            END IF
         ELSE
         	  ERROR "SOLO INDIQUE S o N "
            SLEEP 2
            ERROR ""
            CONTINUE WHILE
         END IF
      END WHILE

      ON KEY (CONTROL-C)
         ERROR "PROCESO CANCELADO"
         SLEEP 2
         ERROR ""
         CLEAR FORM
         EXIT INPUT
   END INPUT

END FUNCTION
################################################################################
FUNCTION valida_preparacion(ld_fecha_corte)
  DEFINE ld_fecha_corte DATE
  DEFINE lar_cta_ctr_edocta   ARRAY[7] OF RECORD
            fecha_corte       DATE                    ,
            paso              SMALLINT                ,
            fecha_ini         DATETIME  YEAR TO SECOND,
            fecha_fin         DATETIME  YEAR TO SECOND,
            usuario           CHAR(8)                 ,
            fecha_proceso     DATE
         END RECORD

  DEFINE li_cont              SMALLINT,
         li_pos               SMALLINT,
         lc_message           CHAR(500),
         lc_ok                CHAR(1),
         lc_pasos_faltantes   CHAR(5),
         lc_linea             SMALLINT

  --Inicializar
  LET li_cont  = 1
  LET lc_linea = 15
  INITIALIZE lar_cta_ctr_edocta TO NULL
  INITIALIZE lc_message TO NULL

  DECLARE cur_val_prep CURSOR FOR
  SELECT *
  FROM   safre_tmp:cta_ctr_edocta
  WHERE  fecha_corte = ld_fecha_corte
  ORDER BY paso

  FOREACH cur_val_prep INTO lar_cta_ctr_edocta[li_cont].*
  	 IF (lar_cta_ctr_edocta[li_cont].fecha_fin IS NULL) THEN
  	 	  LET lc_message =    "El paso " CLIPPED,
  	 	  	                  lar_cta_ctr_edocta[li_cont].paso,
  	 	  	                  " no ha concluido" CLIPPED
  	 	  DISPLAY lc_message CLIPPED AT lc_linea,1 ATTRIBUTE (REVERSE)
  	 	  LET lc_linea = lc_linea + 1
  	 END IF
  	 LET li_cont = li_cont + 1
  END FOREACH

  LET lc_pasos_faltantes = "xxxxxx"

  FOR li_pos = 1 TO 5
  	 CASE lar_cta_ctr_edocta[li_pos].paso
  	 	 WHEN 1
  	 	 	 LET lc_pasos_faltantes[1] = '1'
  	 	 WHEN 2
  	 	 	 LET lc_pasos_faltantes[2] = '2'
  	 	 WHEN 3
  	 	 	 LET lc_pasos_faltantes[3] = '3'
  	 	 WHEN 4
  	 	 	 LET lc_pasos_faltantes[4] = '4'
  	 	 WHEN 5
  	 	 	 LET lc_pasos_faltantes[5] = '5'
  	 	 WHEN 6
  	 	 	 LET lc_pasos_faltantes[6] = '6'
  	 END CASE
  END FOR

  FOR li_pos = 1 TO 5
  	 IF lc_pasos_faltantes[li_pos] = 'x' THEN
  	 	  LET lc_message = "Falta ejecutar el paso: ", li_pos
  	 	  DISPLAY lc_message CLIPPED AT lc_linea,1 ATTRIBUTE (REVERSE)
  	 	  LET lc_linea = lc_linea + 1
  	 END IF
  END FOR

  IF (lc_message IS NOT NULL) THEN
     DISPLAY "Preparación Incompleta para el semestre" AT lc_linea,1 ATTRIBUTE (REVERSE)
     PROMPT "Presione <Enter> para continuar" CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)
     CALL limpia_msg()
     RETURN 0
  ELSE
  	 RETURN 1
  END IF

END FUNCTION
################################################################################
FUNCTION limpia_msg()
   DEFINE li_linea  SMALLINT
   FOR li_linea = 15 TO 20
      DISPLAY "                                                               "
      AT li_linea,1
   END FOR
END FUNCTION
################################################################################
FUNCTION archivo()
   DEFINE lc_nomarch_cifras  CHAR(100) ,
          lc_nomarch_saldos  CHAR(100) ,
          lc_nomarch_ini     CHAR(100) ,
          lc_comando_chmod   CHAR(1000),
          lc_enter           CHAR(01)  ,
          lc_msg1            CHAR(100)  ,
          lc_msg2            CHAR(100)  ,
          lc_msg3            CHAR(100)

   #REPORTE DE CIFRAS
   LET lc_nomarch_ini    = gc_usuario CLIPPED,
                           ".RPT_CIFRAS_EDC_",
                           gd_fecha_corte USING "DDMMYYYY"
   LET lc_nomarch_cifras = gr_seg_modulo.ruta_envio CLIPPED,"/",
                           lc_nomarch_ini CLIPPED

   LET lc_comando_chmod = "chmod 777 ",
                          lc_nomarch_cifras CLIPPED, ";"

   CALL cifras(lc_nomarch_cifras)

   #REPORTE DE SALDOS
   LET lc_nomarch_ini    = gc_usuario CLIPPED,
                           ".RPT_SALDOS_EDC_",
                           gd_fecha_corte USING "DDMMYYYY"
   LET lc_nomarch_saldos = gr_seg_modulo.ruta_envio CLIPPED,"/",
                           lc_nomarch_ini CLIPPED

   LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                          lc_nomarch_saldos CLIPPED, ";"

   CALL saldos(lc_nomarch_saldos)

   #Permisos
   RUN lc_comando_chmod

   #MOSTRAR ARCHIVOS
   ERROR ""
   LET lc_msg1 = "ARCHIVOS GENERADOS"
   LET lc_msg2 = "CIFRAS: ", lc_nomarch_cifras
   LET lc_msg3 = "SALDOS: ", lc_nomarch_saldos

   DISPLAY lc_msg1 CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY lc_msg2 CLIPPED AT 19,1 ATTRIBUTE(REVERSE)
   DISPLAY lc_msg3 CLIPPED AT 20,1 ATTRIBUTE(REVERSE)

   PROMPT "PROCESANDO FINALIZADO" FOR CHAR lc_enter

   DISPLAY "                                                                           " AT 18,1
   DISPLAY "                                                                           " AT 19,1
   DISPLAY "                                                                           " AT 20,1

END FUNCTION
################################################################################
FUNCTION cifras(lc_nomarch_cifras)
   DEFINE lc_nomarch_cifras   CHAR(100),
          ld_fecha_aporte     DATE

   ERROR "PROCESANDO INFORMACION... REPORTE DE CIFRAS"

   #CIFRAS REGISTRADOS
   SELECT COUNT(*)
   INTO   gr_cifras_adm_reg.activas
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab <> 5
   AND    ind_actividad = 1
   AND    estado NOT IN (50)

   SELECT COUNT(*)
   INTO   gr_cifras_adm_reg.inactivas
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab <> 5
   AND    ind_actividad = 0
   AND    estado NOT IN (50)

   SELECT COUNT(*)
   INTO   gr_cifras_adm_reg.saldo_cero
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab <> 5
   AND    estado = 50

   LET gr_cifras_adm_reg.total = gr_cifras_adm_reg.activas   +
                                 gr_cifras_adm_reg.inactivas +
                                 gr_cifras_adm_reg.saldo_cero

   #CIFRAS ASIGNADOS
   SELECT COUNT(*)
   INTO   gr_cifras_adm_asig.activas
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab = 5
   AND    ind_actividad = 1
   AND    estado NOT IN (50)

   SELECT COUNT(*)
   INTO   gr_cifras_adm_asig.inactivas
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab = 5
   AND    ind_actividad = 0
   AND    estado NOT IN (50)

   SELECT COUNT(*)
   INTO   gr_cifras_adm_asig.saldo_cero
   FROM   cta_nss_edo_cta
   WHERE  tipo_trab = 5
   AND    estado = 50

   LET gr_cifras_adm_asig.total = gr_cifras_adm_asig.activas   +
                                  gr_cifras_adm_asig.inactivas +
                                  gr_cifras_adm_asig.saldo_cero

   #CIFRAS ADICIONALES TRASPASO
   LET ld_fecha_aporte = gd_fecha_corte - 1 UNITS YEAR

   LET gd_fecha_inicio = MDY(MONTH(gd_fecha_corte), 1, YEAR(gd_fecha_corte))
   LET gd_fecha_inicio = gd_fecha_inicio - 3 UNITS MONTH

   SELECT nss
   FROM   cta_ctr_cuenta
   WHERE  fecha_ult_general < ld_fecha_aporte
   INTO TEMP tmp_inactivos

   SELECT COUNT(*)
   INTO   gr_extra_traspaso.activas
   FROM   taa_cd_det_cedido
   WHERE  tipo_traspaso  NOT IN (51,20)
   AND    fecha_trasp    BETWEEN gd_fecha_inicio AND gd_fecha_corte
   AND    estado         IN(12,99,103)
   AND    n_seguro NOT IN (SELECT nss FROM tmp_inactivos)

   SELECT COUNT(*)
   INTO   gr_extra_traspaso.inactivas
   FROM   taa_cd_det_cedido
   WHERE  tipo_traspaso  NOT IN (51,20)
   AND    fecha_trasp    BETWEEN gd_fecha_inicio AND gd_fecha_corte
   AND    estado         IN(12,99,103)
   AND    n_seguro IN (SELECT nss FROM tmp_inactivos)

   SELECT COUNT(*)
   INTO   gr_extra_traspaso.saldo_cero
   FROM   taa_cd_det_cedido
   WHERE  tipo_traspaso  NOT IN (51,20)
   AND    fecha_trasp    BETWEEN gd_fecha_inicio AND gd_fecha_corte
   AND    estado         IN(12,99,103)
   AND    n_seguro IN (SELECT nss
                       FROM   cta_act_marca
                       WHERE  marca_cod = 150
                       )

   LET gr_extra_traspaso.total = gr_extra_traspaso.activas   +
                                 gr_extra_traspaso.inactivas +
                                 gr_extra_traspaso.saldo_cero

   #CIFRAS ADICIONALES RETIRO
   SELECT COUNT(*)
   INTO   gr_extra_retiro.activas
   FROM   cta_ctr_proceso
   WHERE  factualiza >= gd_fecha_inicio
   AND    factualiza <= gd_fecha_corte
   AND    tipo_informe = 5
   AND    estado       = 3
   AND    nss NOT IN (SELECT nss FROM tmp_inactivos)

   SELECT COUNT(*)
   INTO   gr_extra_retiro.inactivas
   FROM   cta_ctr_proceso
   WHERE  factualiza >= gd_fecha_inicio
   AND    factualiza <= gd_fecha_corte
   AND    tipo_informe = 5
   AND    estado       = 3
   AND    nss IN (SELECT nss FROM tmp_inactivos)

   SELECT COUNT(*)
   INTO   gr_extra_retiro.saldo_cero
   FROM   cta_ctr_proceso
   WHERE  factualiza >= gd_fecha_inicio
   AND    factualiza <= gd_fecha_corte
   AND    tipo_informe = 5
   AND    estado       = 3
   AND    nss IN (SELECT nss
                  FROM   cta_act_marca
                  WHERE  marca_cod = 150
                  )

   LET gr_extra_retiro.total = gr_extra_retiro.activas   +
                               gr_extra_retiro.inactivas +
                               gr_extra_retiro.saldo_cero

   START REPORT rpt_cifras TO lc_nomarch_cifras
      OUTPUT TO REPORT rpt_cifras()
   FINISH REPORT rpt_cifras
END FUNCTION
################################################################################
FUNCTION saldos(lc_nomarch_saldos)
   DEFINE lc_nomarch_saldos CHAR(100)

   DEFINE lr_saldo RECORD
          tipo_actividad SMALLINT     ,
          subcuenta      SMALLINT     ,
          siefore        SMALLINT     ,
          acciones       DECIMAL(22,6),
          pesos          DECIMAL(22,6)
   END RECORD

   DEFINE ls_siefore SMALLINT
   DEFINE ls_subcta  SMALLINT

   ERROR "PROCESANDO INFORMACION... VALUANDO BONO..."
   CALL valua_bono()
   ERROR "PROCESANDO INFORMACION... REPORTE DE SALDOS..."

   DECLARE cur_saldos CURSOR FOR
   SELECT a.ind_actividad         ,
          b.subcuenta             ,
          b.siefore               ,
          SUM(b.monto_en_acciones),
          SUM(b.monto_en_pesos)
   FROM   cta_nss_edo_cta  a,
          tmp_saldo_edocta b
   WHERE  a.nss              = b.nss
   AND    b.fecha_conversion = gd_fecha_corte
   GROUP BY 1,2,3
   ORDER BY 1,2,3

   FOREACH cur_saldos INTO lr_saldo.tipo_actividad,
                           lr_saldo.subcuenta     ,
                           lr_saldo.siefore       ,
                           lr_saldo.acciones      ,
                           lr_saldo.pesos

      IF lr_saldo.acciones IS NULL THEN
      	 LET lr_saldo.acciones = 0
      END IF

      IF lr_saldo.pesos IS NULL THEN
      	 LET lr_saldo.pesos = 0
      END IF

      IF lr_saldo.siefore >= 1 AND
         lr_saldo.siefore <= 5 THEN
         #Siefores Básicas
         LET ls_siefore = lr_saldo.siefore
      ELSE
      	 #Siefores Adicionales
      	 LET ls_siefore = 6
      END IF

      #Idenftificar en que subuenta del arreglo se almacenará el saldo
      LET ls_subcta = 0

      CASE
      	 #RETIR0 92 IMSS
      	 WHEN lr_saldo.subcuenta = 7
      	    LET ls_subcta = 1

      	 #RCV 97
   	  	 WHEN lr_saldo.subcuenta = 1 OR
   	  	      lr_saldo.subcuenta = 2 OR
   	  	      lr_saldo.subcuenta = 6 OR
   	  	      lr_saldo.subcuenta = 9
   	  	    LET ls_subcta = 2

   	  	 #CUOTA SOCIAL
   	  	 WHEN lr_saldo.subcuenta = 5
   	  	 	  LET ls_subcta = 3
   	  	 	  
   	  	 #COMPLEMENTARIAS
   	  	 WHEN lr_saldo.subcuenta = 11 OR
   	  	 	    lr_saldo.subcuenta = 12 OR
   	  	 	    lr_saldo.subcuenta = 24 OR
   	  	 	    lr_saldo.subcuenta = 25
   	  	 	  LET ls_subcta = 4

   	  	 #VOLUNTARIAS
   	  	 WHEN lr_saldo.subcuenta = 3  OR
   	  	      lr_saldo.subcuenta = 10 OR
   	  	      lr_saldo.subcuenta = 15 OR
   	  	      lr_saldo.subcuenta = 16 OR
   	  	      (lr_saldo.subcuenta >= 20 AND lr_saldo.subcuenta <=23) OR
   	  	      (lr_saldo.subcuenta >= 26 AND lr_saldo.subcuenta <=29)
   	  	    LET ls_subcta = 5

   	  	 #VIV92 IMSS
   	  	 WHEN lr_saldo.subcuenta = 8
   	  	 	  LET ls_subcta = 6

   	  	 #VIV97 IMSS
   	  	 WHEN lr_saldo.subcuenta = 4
   	  	 	  LET ls_subcta = 7

   	  	 #RETIRO 92 ISSSTE
   	  	 WHEN lr_saldo.subcuenta = 13 OR
   	  	 	    lr_saldo.subcuenta = 19
   	  	 	  LET ls_subcta = 8

   	  	 #RCV ISSSTE
   	  	 WHEN lr_saldo.subcuenta = 30 OR
   	  	 	    lr_saldo.subcuenta = 31
   	  	 	  LET ls_subcta = 9

   	  	 #AHORRO SOL.
   	  	 WHEN lr_saldo.subcuenta = 33 OR
   	  	 	    lr_saldo.subcuenta = 34
   	  	 	  LET ls_subcta = 10

   	  	 #CS ISSSTE
   	  	 WHEN lr_saldo.subcuenta = 32
   	  	 	  LET ls_subcta = 11

   	  	 #FOVISSSTE 92
   	  	 WHEN lr_saldo.subcuenta = 14
   	  	 	  LET ls_subcta = 13

   	  	 #FOVISSSTE 2008
   	  	 WHEN lr_saldo.subcuenta = 35
   	  	 	  LET ls_subcta = 14
      END CASE

      IF ls_subcta > 0 THEN
         CASE lr_saldo.tipo_actividad
      	    WHEN 0 --Inactivos
      	 	       LET gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones =
 	 	  	 	           gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones + lr_saldo.acciones

 	               LET gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos    =
 	                   gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos    + lr_saldo.pesos

 	          WHEN 1 --Activos
 	          	   LET gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones =
 	 	  	 	           gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones + lr_saldo.acciones

 	               LET gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos    =
 	                   gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos    + lr_saldo.pesos
 	       END CASE
 	    END IF
   END FOREACH

   DECLARE cur_bono CURSOR FOR
   SELECT a.ind_actividad      ,
          SUM(b.udis)          ,
          SUM(b.monto_en_pesos)
   FROM   cta_nss_edo_cta                 a,
          safre_tmp:tmp_saldo_edocta_bono b
   WHERE  a.nss              = b.nss
   GROUP BY 1
   ORDER BY 1

   LET ls_siefore = 6 #Siefores Adicionales
   LET ls_subcta = 12 #Bono

   FOREACH cur_bono INTO lr_saldo.tipo_actividad,
                         lr_saldo.acciones      ,
                         lr_saldo.pesos

      IF lr_saldo.acciones IS NULL THEN
      	 LET lr_saldo.acciones = 0
      END IF

      IF lr_saldo.pesos IS NULL THEN
      	 LET lr_saldo.pesos = 0
      END IF

      CASE lr_saldo.tipo_actividad
         WHEN 0 --Inactivos
         	    LET gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones =
         	        gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones + lr_saldo.acciones

 	            LET gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos    =
 	                gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos    + lr_saldo.pesos

 	       WHEN 1 --Activos
 	       	    LET gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones =
 	 	  	          gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones + lr_saldo.acciones

 	            LET gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos    =
 	                gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos    + lr_saldo.pesos
 	    END CASE
   END FOREACH

   #Sumario
   FOR ls_subcta = 1 TO 14
   	  FOR ls_siefore = 1 TO 6
   	  	 LET gar_sumario[1].detalle[ls_siefore].acciones =
   	  	 	   gar_sumario[1].detalle[ls_siefore].acciones +
   	  	 	   gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones

   	  	 LET gar_sumario[2].detalle[ls_siefore].acciones =
   	  	 	   gar_sumario[2].detalle[ls_siefore].acciones +
   	  	 	   gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones

   	  	 LET gar_sumario[1].detalle[ls_siefore].pesos =
   	  	 	   gar_sumario[1].detalle[ls_siefore].pesos +
   	  	 	   gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos

   	  	 LET gar_sumario[2].detalle[ls_siefore].pesos =
   	  	 	   gar_sumario[2].detalle[ls_siefore].pesos +
   	  	 	   gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos
   	  END FOR
   END FOR

   START REPORT rpt_saldos TO lc_nomarch_saldos
      OUTPUT TO REPORT rpt_saldos()
   FINISH REPORT rpt_saldos
END FUNCTION
################################################################################
FUNCTION valua_bono()
   DEFINE lc_nss      CHAR(11)     ,
          ld_acciones DECIMAL(22,6),
          ld_udis     DECIMAL(22,6),
          ld_pesos    DECIMAL(22,6),
          lc_sql      CHAR(300)

   INSERT INTO safre_tmp:tmp_saldo_edocta_bono
   SELECT nss              ,  --nss
          fecha_conversion ,  --fecha_conversion
          monto_en_acciones,  --monto_en_acciones
          0                ,  --monto_en_pesos
          0                   --udis
   FROM   tmp_saldo_edocta
   WHERE  subcuenta        = 36
   AND    fecha_conversion = gd_fecha_corte

   DATABASE safre_tmp
      sql
      CREATE INDEX ix_01_tmp_saldo_edocta_bono ON tmp_saldo_edocta_bono(nss) IN tmp_dbs1
      END sql

      UPDATE STATISTICS FOR TABLE tmp_saldo_edocta_bono

      DECLARE cur_valua_bono CURSOR FOR
      SELECT nss              ,
	 	         monto_en_acciones
	 	  FROM   tmp_saldo_edocta_bono

	 	  LET lc_sql = "EXECUTE FUNCTION fn_valua_bono_issste(?, ?, ?)"
	 	  PREPARE get_bono_valuado FROM lc_sql

	 	  FOREACH cur_valua_bono INTO lc_nss     ,
	 	                              ld_acciones
	 	     IF ld_acciones > 0 THEN
	 	        EXECUTE get_bono_valuado USING lc_nss      ,
	 	     	                                 ld_acciones ,
	 	     	                                 gd_fecha_corte
	 	     	                           INTO  ld_udis,
	 	     	                                 ld_pesos
	 	     ELSE
	 	     	  LET ld_udis  = 0;
            LET ld_pesos = 0;
	 	     END IF

	 	     IF ld_udis  IS NULL THEN LET ld_udis  = 0 END IF
	 	     IF ld_pesos IS NULL THEN LET ld_pesos = 0 END IF

	 	     UPDATE tmp_saldo_edocta_bono
	 	     SET    monto_en_pesos = ld_pesos,
	 	            udis           = ld_udis
	 	     WHERE  nss       = lc_nss

	 	     INITIALIZE ld_acciones TO NULL
	 	  END FOREACH
	 DATABASE safre_af
END FUNCTION
################################################################################
REPORT rpt_cifras()
   DEFINE ls_num_cuatrimestre SMALLINT
   DEFINE ls_anio             SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   25

   FORMAT
   PAGE HEADER
      LET ls_num_cuatrimestre = MONTH(gd_fecha_corte) / 4
      LET ls_anio             = YEAR(gd_fecha_corte)
      PRINT COLUMN 001, "REPORTE DE EMISION DE ESTADOS DE CUENTA CUATRIMESTRE ",
                        ls_num_cuatrimestre  USING "&", " DE ", ls_anio USING "&&&&"
      SKIP 1 LINE

   ON EVERY ROW
      PRINT COLUMN 001, "CTAS ADMINISTRADAS AL ", gd_fecha_corte USING "DD/MM/YYYY",
                        "    ACTIVAS INACTIVAS SALDO CERO      TOTAL"
      PRINT COLUMN 001, "Registrados",
            COLUMN 036, gr_cifras_adm_reg.activas     USING "#######&",
            COLUMN 046, gr_cifras_adm_reg.inactivas   USING "#######&",
            COLUMN 057, gr_cifras_adm_reg.saldo_cero  USING "#######&",
            COLUMN 068, gr_cifras_adm_reg.total       USING "#######&"
      PRINT COLUMN 001, "Asignados",
            COLUMN 036, gr_cifras_adm_asig.activas    USING "#######&",
            COLUMN 046, gr_cifras_adm_asig.inactivas  USING "#######&",
            COLUMN 057, gr_cifras_adm_asig.saldo_cero USING "#######&",
            COLUMN 068, gr_cifras_adm_asig.total      USING "#######&"

      SKIP 1 LINE
      PRINT COLUMN 001, "ESTADOS DE CUENTA EMITIDOS          ACTIVAS INACTIVAS SALDO CERO      TOTAL"
      PRINT COLUMN 001, "Registrados",
            COLUMN 036, gr_cifras_adm_reg.activas     USING "#######&",
            COLUMN 046, gr_cifras_adm_reg.inactivas   USING "#######&",
            COLUMN 057, gr_cifras_adm_reg.saldo_cero  USING "#######&",
            COLUMN 068, gr_cifras_adm_reg.total       USING "#######&"
      PRINT COLUMN 001, "Asignados",
            COLUMN 036, gr_cifras_adm_asig.activas    USING "#######&",
            COLUMN 046, gr_cifras_adm_asig.inactivas  USING "#######&",
            COLUMN 057, gr_cifras_adm_asig.saldo_cero USING "#######&",
            COLUMN 068, gr_cifras_adm_asig.total      USING "#######&"

      SKIP 1 LINE
      PRINT COLUMN 001, "ESTADOS DE CUENTA ADICIONALES       ACTIVAS INACTIVAS SALDO CERO      TOTAL"
      PRINT COLUMN 001, "Constancias liquidacion Traspaso",
            COLUMN 036, gr_extra_traspaso.activas     USING "#######&",
            COLUMN 046, gr_extra_traspaso.inactivas   USING "#######&",
            COLUMN 057, gr_extra_traspaso.saldo_cero  USING "#######&",
            COLUMN 068, gr_extra_traspaso.total       USING "#######&"
      PRINT COLUMN 001, "Disposicion Total de Recursos",
            COLUMN 036, gr_extra_retiro.activas       USING "#######&",
            COLUMN 046, gr_extra_retiro.inactivas     USING "#######&",
            COLUMN 057, gr_extra_retiro.saldo_cero    USING "#######&",
            COLUMN 068, gr_extra_retiro.total         USING "#######&"
END REPORT
################################################################################
{
REPORTE DE EMISION DE ESTADOS DE CUENTA CUATRIMESTRE & DE YYYY

CTAS ADMINISTRADAS AL DD/MM/YYYY    ACTIVAS INACTIVAS SALDO CERO      TOTAL
Registrados                        &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&
Asignados                          &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&

ESTADOS DE CUENTA EMITIDOS          ACTIVAS INACTIVAS SALDO CERO      TOTAL
Registrados                        &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&
Asignados                          &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&

Folios inicial Registrados Activas    ####################
Folios final   Registrados Activas    ####################

Folios inicial Registrados Inactivas  ####################
Folios final   Registrados Inactivas  ####################

Folios inicial Registrados Saldo Cero ####################
Folios final   Registrados Saldo Cero ####################

Folios inicial Asignados   Activas    ####################
Folios final   Asignados   Activas    ####################

Folios inicial Asignados   Inactivas  ####################
Folios final   Asignados   Inactivas  ####################

Folios inicial Asignados   Saldo Cero ####################
Folios final   Asignados   Saldo Cero ####################

ESTADOS DE CUENTA ADICIONALES       ACTIVAS INACTIVAS SALDO CERO      TOTAL
Constancias liquidación Traspaso   &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&
Disposición Total de Recursos      &&&&&&&&  &&&&&&&&   &&&&&&&&   &&&&&&&&
}
################################################################################
REPORT rpt_saldos()
   DEFINE ls_num_cuatrimestre SMALLINT
   DEFINE ls_siefore          SMALLINT
   DEFINE ls_subcta           SMALLINT
   DEFINE lc_titulo_sb        CHAR(17)
   DEFINE ls_anio             SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   25

   FORMAT
   {PAGE HEADER
      LET ls_num_cuatrimestre = MONTH(gd_fecha_corte) / 4
      PRINT COLUMN 001, "REPORTE DE EMISION DE ESTADOS DE CUENTA CUATRIMESTRE ",
                        ls_num_cuatrimestre  USING "&", " DE ", YEAR (gd_fecha_corte) USING "&&&&"
      PRINT COLUMN 032, "VALUACION MONETARIA"
}
   ON EVERY ROW
      LET ls_num_cuatrimestre = MONTH(gd_fecha_corte) / 4
      LET ls_anio             = YEAR(gd_fecha_corte)
      PRINT COLUMN 001, "REPORTE DE EMISION DE ESTADOS DE CUENTA CUATRIMESTRE ",
                        ls_num_cuatrimestre  USING "&", " DE ", ls_anio USING "&&&&"
      PRINT COLUMN 032, "VALUACION MONETARIA"

      #IMPRIMIR ACTIVAS
      FOR ls_siefore = 1 TO 6
      	 IF ls_siefore = 6 THEN
      	    LET lc_titulo_sb = "SIEFORE ADICIONAL"
      	 ELSE
      	 	  LET lc_titulo_sb = "SIEFORE BASICA ", ls_siefore USING "&"
      	 END IF

         SKIP 1 LINE
      	 PRINT COLUMN 001, "--------------------------------------------------------------------------------"
      	 PRINT COLUMN 001, "CUENTAS ACTIVAS       TITULOS Y/O AIVS            PESOS   ", lc_titulo_sb CLIPPED
      	 PRINT COLUMN 001, "--------------------------------------------------------------------------------"

      	 FOR ls_subcta = 1 TO 14
      	 	  PRINT COLUMN 001, gar_det_subcta_act[ls_subcta].desc_subcuenta,
      	 	        COLUMN 020, gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].acciones USING "###########&.&&&&&&",
      	 	        COLUMN 041, gar_det_subcta_act[ls_subcta].det_siefore[ls_siefore].pesos    USING "###########&.&&"
      	 END FOR

      	 PRINT COLUMN 001, "________________________________________________________________________________"
      	 PRINT COLUMN 001, "TOTAL",
      	       COLUMN 020, gar_sumario[1].detalle[ls_siefore].acciones USING "###########&.&&&&&&",
      	       COLUMN 041, gar_sumario[1].detalle[ls_siefore].pesos    USING "###########&.&&"
      END FOR

      #IMPRIMIR INACTIVAS
      FOR ls_siefore = 1 TO 6
      	 IF ls_siefore = 6 THEN
      	    LET lc_titulo_sb = "SIEFORE ADICIONAL"
      	 ELSE
      	 	  LET lc_titulo_sb = "SIEFORE BASICA ", ls_siefore USING "&"
      	 END IF

         SKIP 1 LINE
      	 PRINT COLUMN 001, "--------------------------------------------------------------------------------"
      	 PRINT COLUMN 001, "CUENTAS INACTIVAS     TITULOS Y/O AIVS            PESOS   ", lc_titulo_sb CLIPPED
      	 PRINT COLUMN 001, "--------------------------------------------------------------------------------"

      	 FOR ls_subcta = 1 TO 14
      	 	  PRINT COLUMN 001, gar_det_subcta_inac[ls_subcta].desc_subcuenta,
      	 	        COLUMN 020, gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].acciones USING "###########&.&&&&&&",
      	 	        COLUMN 041, gar_det_subcta_inac[ls_subcta].det_siefore[ls_siefore].pesos    USING "###########&.&&"
      	 END FOR

      	 PRINT COLUMN 001, "________________________________________________________________________________"
      	 PRINT COLUMN 001, "TOTAL",
      	       COLUMN 020, gar_sumario[2].detalle[ls_siefore].acciones USING "###########&.&&&&&&",
      	       COLUMN 041, gar_sumario[2].detalle[ls_siefore].pesos    USING "###########&.&&"
      END FOR
END REPORT
################################################################################
{
REPORTE DE EMISION DE ESTADOS DE CUENTA CUATRIMESTRE & DE YYYY
                      VALUACION MONETARIA


--------------------------------------------------------------------------------
CUENTAS ACTIVAS    TITULOS Y/O AIVS     PESOS             SIEFORE ADICIONAL
--------------------------------------------------------------------------------
RETIR0 92 IMSS     &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
RCV 97             &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
VOLUNTARIAS        &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
VIV92 IMSS         &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
VIV97 IMSS         &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
RETIRO 92 ISSSTE   &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
RCV ISSSTE         &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
AHORRO SOL.        &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
CS ISSSTE          &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
BONO PENSION       &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
FOVISSSTE 92       &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
FOVISSSTE 2008     &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
________________________________________________________________________________
TOTAL              &&&&&&&&&&.&&&&&&    &&&&&&&&&&&&.&&
}                                       ###########&.&&