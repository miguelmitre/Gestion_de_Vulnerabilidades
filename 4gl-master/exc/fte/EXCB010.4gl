#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB010                                         #
#Descripcion       => DEVOLUCION DE PAGOS EN EXCESO ISSSTE GENERACION #
#                     DE ARCHIVO DE RESPUESTA A PROCESAR              #
#Fecha Inicio      => 03 FEBRERO 2010                                 #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#                     STEFANIE DANIELA VERA PIÑA                      #
#Sistema           => EXC.                                            #
#*********************************************************************#
DATABASE safre_af

GLOBALS
   DEFINE hoy                 DATE,
          hoy1                DATE,
          hora                CHAR(8),
          opc                 CHAR(01),
          i                   SMALLINT,
          fecha_proceso, 
          fecha_ini,
          v_fecha,
          v_fin_mes,
          fecha_fin           DATE,
          dias , 
          v_dias              SMALLINT,
          v_rep               CHAR(1500), 
          vreport             CHAR(800),
          v_rep_08,
          v_ruta, 
          v_arch              CHAR(100),
          t_fecha             CHAR(12)

   DEFINE p_tabafore  RECORD  LIKE tab_afore_local.*

   DEFINE g_cza  RECORD   LIKE exc_cza_exceso_issste.*

   DEFINE g_det_imss RECORD LIKE exc_det_exceso.*

   DEFINE g_det  RECORD   LIKE exc_det_exceso_issste.*

   DEFINE g_dep_imss  RECORD   LIKE exc_dep_exceso.*

   DEFINE g_dep  RECORD   LIKE exc_dep_exceso_issste.*

   DEFINE g_sum_imss RECORD
      tipo_ent_origen            CHAR(2),
      clave_ent_origen           CHAR(3),
      fecha_creac_lote           DATE,
      consecutivo_dia            CHAR(3),
      monto_tot_rcv              DECIMAL(18,6),
      monto_tot_pat              DECIMAL(18,6),
      monto_tot_gub              DECIMAL(18,6),
      num_reg_aport_lote         CHAR(8),
      num_apor_dev               CHAR(6),
      num_apor_rechazada         CHAR(6),
      num_apor_pendiente         CHAR(6),
      num_reg_pagar              CHAR(6),
      num_tot_reg                CHAR(6),
      monto_tot_par_viv          DECIMAL(18,6)
   END RECORD

   DEFINE g_sum  RECORD
      tipo_ent_origen            CHAR(2),
      clave_ent_origen           CHAR(3),
      fecha_creac_lote           DATE,
      consecutivo_dia            CHAR(3),
      tot_impt_sar_issste        DECIMAL(18,6),
      tot_impt_ret_issste        DECIMAL(18,6),
      tot_impt_cv_patron         DECIMAL(18,6),
      tot_impt_fondo_viv92       DECIMAL(18,6),
      tot_aplic_int_fondo_viv92  DECIMAL(18,6),
      tot_impt_fondo_viv08       DECIMAL(18,6),
      tot_aplic_int_fondo_viv08  DECIMAL(18,6),
      tot_impt_ahorro_solid      DECIMAL(18,6),
      num_reg_aport_lote         CHAR(8),
      num_aport_dev              CHAR(6),
      num_aport_dev_par          CHAR(6),
      num_aport_rechazada        CHAR(6),
      num_aport_pendiente         CHAR(6),
      num_reg_pagar              CHAR(6),
      num_tot_reg                CHAR(6)
   END RECORD

   DEFINE vmonto_comi_ret     DECIMAL(16,6),
          vmonto_comi_ces_vej DECIMAL(16,6),
          vmonto_plus_ret     DECIMAL(16,6),
          vmonto_plus_ces_vej DECIMAL(16,6),
          vmonto_plus_pat     DECIMAL(16,6),
          vmonto_min_ret      DECIMAL(16,6),
          vmonto_min_ces_vej  DECIMAL(16,6),
          vmonto_sub_acep     DECIMAL(16,6),
          vmonto_sub_rech     DECIMAL(16,6),
          vmonto_sub_pend     DECIMAL(16,6),
          vmonto_tot_acep     DECIMAL(16,6),
          vmonto_tot_plus     DECIMAL(16,6),
          vmonto_tot_min      DECIMAL(16,6),
          vmonto_tot_comi     DECIMAL(16,6)

   DEFINE vfolio              INTEGER,
          vtotal_registros    SMALLINT,
          usuario             CHAR(10),
          pos2                SMALLINT,
          pos3                SMALLINT,
          pos4                SMALLINT,
          pos5                SMALLINT,
          pos8                SMALLINT,
          vtipo_ejec          SMALLINT

   DEFINE sum1  RECORD
          aceptados_tot       DECIMAL(16,6),
          monto_ret           DECIMAL(16,6),
          monto_act_ret       DECIMAL(16,6),
          monto_ces_vej_pat   DECIMAL(16,6),
          monto_ces_vej_tra   DECIMAL(16,6),
          monto_act_ces_vej   DECIMAL(16,6),
          monto_aport_pat     DECIMAL(16,6),
          monto_comi_ret      DECIMAL(16,6),
          monto_comi_ces_vej  DECIMAL(16,6),
          monto_plus_ret      DECIMAL(16,6),
          monto_plus_ces_vej  DECIMAL(16,6),
          monto_plus_pat      DECIMAL(16,6),
          monto_min_ret       DECIMAL(16,6),
          monto_min_ces_vej   DECIMAL(16,6),
          monto_par_viv       DECIMAL(16,6)
   END RECORD

   DEFINE sum2  RECORD
          rechazados_tot      DECIMAL(16,6),
          monto_ret           DECIMAL(16,6),
          monto_act_ret       DECIMAL(16,6),
          monto_ces_vej_pat   DECIMAL(16,6),
          monto_ces_vej_tra   DECIMAL(16,6),
          monto_act_ces_vej   DECIMAL(16,6),
          monto_aport_pat     DECIMAL(16,6),
          monto_par_viv       DECIMAL(16,6)
   END RECORD

   DEFINE sum3  RECORD
          pendientes_tot      DECIMAL(16,6),
          monto_ret           DECIMAL(16,6),
          monto_act_ret       DECIMAL(16,6),
          monto_ces_vej_pat   DECIMAL(16,6),
          monto_ces_vej_tra   DECIMAL(16,6),
          monto_act_ces_vej   DECIMAL(16,6),
          monto_aport_pat     DECIMAL(16,6),
          monto_par_viv       DECIMAL(16,6)
   END RECORD

   DEFINE cla_sel             CHAR(400),
          vhora_max           CHAR(8),
          vconsecutivo        INTEGER,
          vhora_final         CHAR(8),
          vfecha_proceso      DATE,
          vresultado          CHAR(50),
          rfolio              INTEGER,
          vident_pago         CHAR(02),
          vcodigo_siefore     SMALLINT,
          v_exc_dep_cuantos   SMALLINT

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE vvident_pago         CHAR(20),
          vvmonto_soli_institu CHAR(23),
          vvfecha_liquidacion  CHAR(10),
          vvmonto_aceptado     CHAR(23),
          vvmonto_pendiente    CHAR(23),
          vvmonto_rechazado    CHAR(23)

   DEFINE permisos             CHAR(100)
   DEFINE log1                 CHAR(40)

END GLOBALS
#*********************************************************************
MAIN 
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET vtipo_ejec = ARG_VAL(1)
   LET vfolio = ARG_VAL(2)

   SELECT *,
          USER
   INTO   p_tabafore.*,
          usuario
   FROM   tab_afore_local

   SELECT ruta_envio
   INTO   v_ruta
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET log1 = "/tmp/",usuario CLIPPED,".EXCB010.log"

   CALL STARTLOG(log1)

   LET hoy = TODAY

   CALL inicio()

   CALL proceso()

END MAIN
#*********************************************************************
FUNCTION inicio ()

   LET i                        = 0 
   LET dias                     = 0 
   LET v_dias                   = 0

   LET vmonto_comi_ret          = 0
   LET vmonto_comi_ces_vej      = 0
   LET vmonto_plus_ret          = 0
   LET vmonto_plus_ces_vej      = 0
   LET vmonto_plus_pat          = 0
   LET vmonto_min_ret           = 0
   LET vmonto_min_ces_vej       = 0
   LET vmonto_sub_acep          = 0
   LET vmonto_sub_rech          = 0
   LET vmonto_sub_pend          = 0
   LET vmonto_tot_acep          = 0
   LET vmonto_tot_plus          = 0
   LET vmonto_tot_min           = 0
   LET vmonto_tot_comi          = 0

   LET vtotal_registros         = 0
   LET rfolio                   = 0
   LET pos2                     = 0
   LET pos3                     = 0
   LET pos4                     = 0
   LET pos5                     = 0
   LET pos8                     = 0

   LET sum1.aceptados_tot       = 0
   LET sum1.monto_ret           = 0
   LET sum1.monto_act_ret       = 0
   LET sum1.monto_ces_vej_pat   = 0
   LET sum1.monto_ces_vej_tra   = 0
   LET sum1.monto_act_ces_vej   = 0
   LET sum1.monto_aport_pat     = 0
   LET sum1.monto_comi_ret      = 0
   LET sum1.monto_comi_ces_vej  = 0
   LET sum1.monto_plus_ret      = 0
   LET sum1.monto_plus_ces_vej  = 0
   LET sum1.monto_plus_pat      = 0
   LET sum1.monto_min_ret       = 0
   LET sum1.monto_min_ces_vej   = 0
   LET sum1.monto_par_viv       = 0

   LET sum2.rechazados_tot      = 0
   LET sum2.monto_ret           = 0
   LET sum2.monto_act_ret       = 0
   LET sum2.monto_ces_vej_pat   = 0
   LET sum2.monto_ces_vej_tra   = 0
   LET sum2.monto_act_ces_vej   = 0
   LET sum2.monto_aport_pat     = 0
   LET sum2.monto_par_viv       = 0

   LET sum3.pendientes_tot      = 0
   LET sum3.monto_ret           = 0
   LET sum3.monto_act_ret       = 0
   LET sum3.monto_ces_vej_pat   = 0
   LET sum3.monto_ces_vej_tra   = 0
   LET sum3.monto_act_ces_vej   = 0
   LET sum3.monto_aport_pat     = 0
   LET sum3.monto_par_viv       = 0

END FUNCTION
#*********************************************************************
FUNCTION proceso()

   IF vfolio IS NOT NULL THEN

DISPLAY "INICIANDO PROCESO DE GENERACION DE ARCHIVO ",vfolio

      CALL proceso_principal()
      RETURNING vfolio

      LET vhora_final = TIME

      IF vfolio = 0 THEN
         LET vresultado = "ESTE ARCHIVO YA HA SIDO PROCESADO"
      ELSE 
         LET vresultado = "Archivo de respuesta a PROCESAR generado"
      END IF

      CALL Actualiza_etapa(vfolio,6,vresultado)

   END IF

END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50)

--   LET cla_sel = " SELECT MAX(hora_inicial) ",
   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
--      FETCH cur_proceso3 INTO vhora_max
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'EXC'",
                 " AND    etapa_cod     = ",vetapa_cod,
--                 " AND    hora_inicial  = ","'",vhora_max,"'"
                 " AND    consecutivo  = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
#*********************************************************************
FUNCTION proceso_principal()

    IF vtipo_ejec = 1 THEN #-- ARCHIVO IMSS --#
       SELECT a.fecha_creac_lote
       INTO   hoy1
       FROM   exc_cza_exceso a
       WHERE  a.folio  = vfolio
    ELSE
       SELECT a.fecha_creac_lote
       INTO   hoy1
       FROM   exc_cza_exceso_issste a
       WHERE  a.folio  = vfolio
    END IF

    LET hora = TIME 

    IF vtipo_ejec = 1 THEN #-- ARCHIVO IMSS --#
       LET v_arch = v_ruta CLIPPED,"/",hoy1 USING "yyyymmdd" ,".DEV",vfolio USING "<<<<<<"
    ELSE
       LET v_arch = v_ruta CLIPPED,"/",hoy1 USING "yyyymmdd" ,".DEV_ISSSTE_",vfolio USING "<<<<<<"
    END IF

    START REPORT r_report TO v_arch

       IF vtipo_ejec = 1 THEN #-- ARCHIVO IMSS --#
          SELECT fecha_creac_lote,
                 consecutivo_dia,
                 fecha_limite_resp
          INTO   g_cza.fecha_creac_lote,
                 g_cza.consecutivo_dia,
                 g_cza.fecha_limite_resp
          FROM   exc_cza_exceso
          WHERE  folio  = vfolio
       ELSE
          SELECT fecha_creac_lote,
                 consecutivo_dia,
                 fecha_limite_resp
          INTO   g_cza.fecha_creac_lote,
                 g_cza.consecutivo_dia,
                 g_cza.fecha_limite_resp
          FROM   exc_cza_exceso_issste
          WHERE  folio  = vfolio
       END IF

       LET v_rep = "01","|",
                   p_tabafore.codigo_afore,"|",
                   g_cza.fecha_creac_lote,"|",
                   g_cza.consecutivo_dia,"|",
                   g_cza.fecha_limite_resp,"|"

       IF vtipo_ejec = 1 THEN #-- ARCHIVO IMSS --#
          OUTPUT TO REPORT r_report(v_rep,356,1)
       ELSE
          OUTPUT TO REPORT r_report(v_rep,926,1)
       END IF

       CALL reg_02()

    FINISH REPORT r_report

    LET permisos = "chmod 777 ",v_arch CLIPPED
    RUN permisos

    RETURN vfolio
END FUNCTION
#*********************************************************************
FUNCTION reg_02()

   DEFINE
      vmonto_en_pesos    DECIMAL(16,6)

   DEFINE
      vsubcuenta         ,
      vtipo_movimiento   SMALLINT

   IF vtipo_ejec = 1 THEN #-- ARCHIVO IMSS --#
      DECLARE cursor_4 CURSOR FOR
      SELECT * 
      FROM   exc_det_exceso
      WHERE  folio  = vfolio
      ORDER BY consec_reg_lote

      LET pos2 = 0
      LET pos3 = 0
      LET pos4 = 0
      LET pos5 = 0

      FOREACH cursor_4 INTO g_det_imss.*

         IF g_det_imss.result_operacion = "04" THEN
            DECLARE cursor_7 CURSOR FOR
            SELECT subcuenta,
                   tipo_movimiento,
                   monto_en_pesos
            FROM   dis_cuenta
            WHERE  folio = vfolio
            AND    nss = g_det_imss.nss

            FOREACH cursor_7 INTO vsubcuenta,
                               vtipo_movimiento,
                               vmonto_en_pesos

               CASE vtipo_movimiento
                  WHEN 540
                     IF vsubcuenta = 1 THEN
                        IF vmonto_en_pesos <> g_det_imss.monto_ret THEN
                           LET g_det_imss.monto_ret = vmonto_en_pesos
                        END IF
                     END IF

                     IF vsubcuenta = 2 THEN
                        IF vmonto_en_pesos <> g_det_imss.monto_ces_vej_pat THEN
                           LET g_det_imss.monto_ces_vej_pat = vmonto_en_pesos
                        END IF
                     END IF

                     IF g_det_imss.clave_ent_orig = "002" THEN
                        IF vsubcuenta = 4 THEN
                           IF vmonto_en_pesos <> g_det_imss.monto_aport_pat THEN
                              LET g_det_imss.monto_aport_pat = vmonto_en_pesos
                           END IF
                        END IF
                     END IF

                   WHEN 545
                     IF vsubcuenta = 1 THEN
                        IF vmonto_en_pesos <> g_det_imss.monto_act_ret THEN
                           LET g_det_imss.monto_act_ret = vmonto_en_pesos
                        END IF
                     END IF

                     IF vsubcuenta = 2 THEN
                        IF vmonto_en_pesos <> g_det_imss.monto_act_cv_pat THEN
                           LET g_det_imss.monto_act_cv_pat = vmonto_en_pesos
                        END IF
                     END IF

                    WHEN 550
                       IF vsubcuenta = 1 THEN
                          IF vmonto_en_pesos <> g_det_imss.mto_act_ret_rend THEN
                             LET g_det_imss.mto_act_ret_rend = vmonto_en_pesos
                          END IF
                       END IF

                       IF vsubcuenta = 2 THEN
                          IF vmonto_en_pesos <> g_det_imss.mto_act_cv_rend_pat THEN
                             LET g_det_imss.mto_act_cv_rend_pat = vmonto_en_pesos
                          END IF
                       END IF
               END CASE
            END FOREACH
         END IF

         LET v_rep =g_det_imss.consec_reg_lote,"|",
                    g_det_imss.nss,"|",
                    g_det_imss.clave_ent_orig,"|",
                    g_det_imss.rfc,"|",
                    g_det_imss.curp,"|",
                    g_det_imss.nom_trabajador,"|",
                    g_det_imss.periodo_pago,"|",
                    g_det_imss.fecha_pago,"|",
                    g_det_imss.fecha_valor_rcv,"|",
                    g_det_imss.fecha_valor_viv,"|",
                    g_det_imss.folio_pago_sua,"|",
                    g_det_imss.clave_ent_recep,"|",
                    g_det_imss.reg_patronal_imss,"|",
                    g_det_imss.monto_ret USING "&&&&&&&.&&","|",
                    g_det_imss.monto_ces_vej_pat USING "&&&&&&&.&&","|",
                    g_det_imss.monto_ces_vej_tra USING "&&&&&&&.&&","|",
                    g_det_imss.monto_cuo_soc USING "&&&&&&&.&&","|",
                    g_det_imss.monto_act_ret USING "&&&&&&&.&&","|",
                    g_det_imss.monto_act_cv_pat USING "&&&&&&&.&&","|",
                    g_det_imss.monto_act_cv_tra USING "&&&&&&&.&&","|",
                    g_det_imss.monto_aport_pat USING "&&&&&&&.&&","|",
                    g_det_imss.dias_cotz_bimestre,"|",
                    g_det_imss.monto_par_viv USING "&&&&&&&&&.&&&&&&","|",
                    g_det_imss.mto_act_ret_rend USING "&&&&&&&.&&","|",
                    g_det_imss.mto_act_cv_rend_pat USING "&&&&&&&.&&","|",
                    g_det_imss.mto_act_cv_rend_tra USING "&&&&&&&.&&","|",
                    g_det_imss.fecha_envio_orig,"|",
                    g_det_imss.result_operacion,"|",
                    g_det_imss.tipo_diagnostico,"|"

         LET pos2 = pos2 + 1

         OUTPUT TO REPORT r_report(v_rep,356,2)
      END FOREACH

      CALL actualiza_imss(g_det_imss.*,p_tabafore.codigo_afore)
   ELSE
      DECLARE cursor_1 CURSOR FOR
      SELECT * 
      FROM   exc_det_exceso_issste
      WHERE  folio  = vfolio
      ORDER BY consec_reg_lote

      LET pos2 = 0
      LET pos3 = 0
      LET pos4 = 0
      LET pos5 = 0

      FOREACH cursor_1 INTO g_det.*

         LET v_rep =g_det.consec_reg_lote,"|",
                    g_det.ident_pago,"|",
                    g_det.periodo_pago,"|",
                    g_det.fecha_pago,"|",
                    g_det.fecha_valor,"|",
                    g_det.linea_captura,"|",
                    g_det.dias_pago,"|",
                    g_det.mto_global_pagado USING "&&&&&&&&&&.&&","|",
                    g_det.nom_centro_pago,"|",
                    g_det.rfc_patron,"|",
                    g_det.modalidad_incorp,"|",
                    g_det.clave_ent_recep,"|",
                    g_det.tipo_pago,"|",
                    g_det.rfc,"|",
                    g_det.curp,"|",
                    g_det.paterno,"|",
                    g_det.materno,"|",
                    g_det.nombre,"|",
                    g_det.nss_issste,"|",
                    g_det.nss,"|",
                    g_det.regimen,"|",
                    g_det.clave_ent_orig,"|",
                    g_det.dias_cotizados,"|",
                    g_det.impt_sar_issste USING "&&&&&&&&&&.&&","|",
                    g_det.impt_ret_issste USING "&&&&&&&&&&.&&","|",
                    g_det.impt_cv_patron USING "&&&&&&&&&&.&&","|",
                    g_det.impt_fondo_viv92 USING "&&&&&&&&&&.&&","|",
                    g_det.aplic_int_fondo_viv92 USING "&&&&&&&&&&&&.&&&&&&","|",
                    g_det.impt_fondo_viv08 USING "&&&&&&&&&&.&&","|",
                    g_det.aplic_int_fondo_viv08 USING "&&&&&&&&&&&&.&&&&&&","|",
                    g_det.impt_ahorro_solid USING "&&&&&&&&&&.&&","|",
                    g_det.result_operacion,"|",
                    g_det.tipo_diagnostico,"|"

         LET pos2 = pos2 + 1

         OUTPUT TO REPORT r_report(v_rep,926,2)
      END FOREACH

      CALL actualiza_issste(g_det.*,p_tabafore.codigo_afore)
   END IF
END FUNCTION
#*********************************************************************
FUNCTION actualiza_imss(g_det_imss,codigo_afore)

   DEFINE g_det_imss  RECORD   LIKE exc_det_exceso.*

   DEFINE recha  RECORD   LIKE exc_dep_exceso.*

   DEFINE reg_2 RECORD
      c2ident_pago         CHAR(2),
      codigo_siefore       SMALLINT,
      estado               SMALLINT,
      fecha_estado         DATE,
      hora_estado          CHAR(8),
      tipo_registro        CHAR(2),
      ident_servicio       CHAR(2),
      ident_pago           CHAR(16),
      monto_soli_instituto DECIMAL(18,6),
      fecha_liquidacion    DATE
   END RECORD

   DEFINE 
      codigo_afore         CHAR(20)

   DEFINE sie RECORD
      codigo_siefore       SMALLINT,
      siefore_desc         CHAR(20)
   END RECORD

   DEFINE 
      sie_pos              SMALLINT

   DEFINE total_temp       DECIMAL(16,2),
          total_temp_sie   DECIMAL(16,2),
          diferencia       DECIMAL(16,2)

   DEFINE x_sub_acep       DECIMAL(16,2),
          x_sub_plu        DECIMAL(16,2),
          x_sub_comi       DECIMAL(16,2),
          sel_txt          CHAR(1000)

   LET total_temp = 0
   LET total_temp_sie = 0
   LET diferencia = 0

   DECLARE cursor_5 CURSOR FOR
   SELECT g.ident_pago[14,15],
          g.codigo_siefore,
          g.estado,
          g.fecha_estado,
          g.hora_estado,
          g.tipo_registro,
          g.ident_servicio,
          g.ident_pago,
          g.monto_soli_institu,
          g.fecha_liquidacion
   FROM   exc_dep_exceso g
   WHERE  g.folio = vfolio

   FOREACH cursor_5 INTO reg_2.*

      CASE reg_2.c2ident_pago
         WHEN "71"  #-- RCV --#
            CALL act_deposito_imss(vfolio,reg_2.*)
         WHEN "73"  #-- VIVIENDA --#
            CALL act_deposito_imss(vfolio,reg_2.*)
      END CASE
   END FOREACH

   DECLARE cursor_6 CURSOR FOR
   SELECT r.ident_pago,
          r.monto_soli_institu,
          r.fecha_liquidacion,
          r.monto_aceptado,
          r.monto_pendiente,
          r.monto_rechazado,
          r.monto_par_vivienda,
          r.mto_par_viv_acep,
          r.mto_par_viv_pend,
          r.mto_par_viv_rech,
          r.monto_vivienda,
          r.monto_viv_pend,
          r.monto_viv_rech,
          r.siefore_desc,
          r.codigo_siefore
   FROM   exc_dep_exceso r
   WHERE  r.folio  = vfolio
   ORDER BY 1

   LET pos8 = 0

   FOREACH cursor_6 INTO g_dep_imss.ident_pago,
                         g_dep_imss.monto_soli_institu,
                         g_dep_imss.fecha_liquidacion,
                         g_dep_imss.monto_aceptado,
                         g_dep_imss.monto_pendiente,
                         g_dep_imss.monto_rechazado,
                         g_dep_imss.monto_par_vivienda,
                         g_dep_imss.mto_par_viv_acep,
                         g_dep_imss.mto_par_viv_pend,
                         g_dep_imss.mto_par_viv_rech,
                         g_dep_imss.monto_vivienda,
                         g_dep_imss.monto_viv_pend,
                         g_dep_imss.monto_viv_rech,
                         g_dep_imss.siefore_desc,
                         g_dep_imss.codigo_siefore

      LET v_rep = g_dep_imss.ident_pago,"|",
                  g_dep_imss.monto_soli_institu USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.fecha_liquidacion,"|",
                  g_dep_imss.monto_aceptado USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.monto_pendiente USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.monto_rechazado USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.monto_par_vivienda USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep_imss.mto_par_viv_acep USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep_imss.mto_par_viv_pend USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep_imss.mto_par_viv_rech USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep_imss.monto_vivienda USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.monto_viv_pend USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.monto_viv_rech USING "&&&&&&&&&&&&&.&&","|",
                  g_dep_imss.siefore_desc,"|",
                  g_dep_imss.codigo_siefore USING "&&&","|"

      LET pos8 = pos8 + 1

      OUTPUT TO REPORT r_report(v_rep,356,8)

   END FOREACH

   LET vtotal_registros = pos2 + pos8

   UPDATE exc_sum_exceso
   SET    num_reg_pagar = pos2,
          num_tot_reg   = vtotal_registros
   WHERE  folio = vfolio

   SELECT A.tipo_ent_destino,
          A.clave_ent_destino,
          A.fecha_creac_lote,
          A.consecutivo_dia,
          A.monto_tot_rcv,
          A.monto_tot_pat,
          A.monto_tot_gub,
          A.num_reg_aport_lote,
          A.num_apor_dev,
          A.num_apor_rechazada,
          A.num_apor_pendiente,
          A.num_reg_pagar,
          A.num_tot_reg,
          A.monto_tot_par_viv
   INTO   g_sum_imss.*
   FROM   exc_sum_exceso A
   WHERE  A.folio  = vfolio

   LET v_rep =g_sum_imss.tipo_ent_origen           ,"|",
              g_sum_imss.clave_ent_origen          ,"|",
              g_sum_imss.fecha_creac_lote          ,"|",
              g_sum_imss.consecutivo_dia           ,"|",
              g_sum_imss.monto_tot_rcv             USING "&&&&&&&&&&&&&.&&","|",
              g_sum_imss.monto_tot_pat             USING "&&&&&&&&&&&&&.&&","|",
              g_sum_imss.monto_tot_gub             USING "&&&&&&&&&&&&&.&&","|",
              g_sum_imss.num_reg_aport_lote        ,"|",
              g_sum_imss.num_apor_dev              ,"|",
              g_sum_imss.num_apor_rechazada        ,"|",
              g_sum_imss.num_apor_pendiente        ,"|",
              g_sum_imss.num_reg_pagar             ,"|",
              g_sum_imss.num_tot_reg               ,"|",
              g_sum_imss.monto_tot_par_viv         USING "&&&&&&&&&&&&&.&&&&&&","|"

   OUTPUT TO REPORT r_report(v_rep,356,9)

END FUNCTION
#*********************************************************************
FUNCTION actualiza_issste(g_det,codigo_afore)

   DEFINE g_det  RECORD   LIKE exc_det_exceso_issste.*
   DEFINE recha  RECORD   LIKE exc_dep_exceso_issste.*

   DEFINE reg_2 RECORD
      c2ident_pago         CHAR(2),
      codigo_siefore       SMALLINT,
      estado               SMALLINT,
      fecha_estado         DATE,
      hora_estado          CHAR(8),
      tipo_registro        CHAR(2),
      ident_servicio       CHAR(2),
      ident_pago           CHAR(16),
      monto_soli_instituto DECIMAL(18,6),
      fecha_liquidacion    DATE
   END RECORD

   DEFINE 
      codigo_afore         CHAR(20)

   DEFINE sie RECORD
      codigo_siefore       SMALLINT,
      siefore_desc         CHAR(20)
   END RECORD

   DEFINE 
      sie_pos              SMALLINT

   DEFINE total_temp       DECIMAL(16,2),
          total_temp_sie   DECIMAL(16,2),
          diferencia       DECIMAL(16,2)

   DEFINE x_sub_acep       DECIMAL(16,2),
          x_sub_plu        DECIMAL(16,2),
          x_sub_comi       DECIMAL(16,2),
          sel_txt          CHAR(1000)

   LET total_temp = 0
   LET total_temp_sie = 0
   LET diferencia = 0

   DECLARE cursor_2 CURSOR FOR
   SELECT g.ident_pago[14,15],
          g.codigo_siefore,
          g.estado,              
          g.fecha_estado,        
          g.hora_estado,         
          g.tipo_registro,       
          g.ident_servicio,      
          g.ident_pago,          
          g.monto_soli_instituto,
          g.fecha_liquidacion   
   FROM   exc_dep_exceso_issste g
   WHERE  g.folio = vfolio

   FOREACH cursor_2 INTO reg_2.*

      CASE reg_2.c2ident_pago
         WHEN "71"  --RCV ISSSTE
         	  CALL act_deposito_issste(vfolio,reg_2.*)
         WHEN "72"  --VIVIENDA 92
         	  CALL act_deposito_issste(vfolio,reg_2.*)
         WHEN "73"  --RETIRO SAR ISSSTE
         	  CALL act_deposito_issste(vfolio,reg_2.*)
         WHEN "74"  --VIVIENDA 08
         	  CALL act_deposito_issste(vfolio,reg_2.*)
         WHEN "77"  --AHORRO SOLIDARIO
         	  CALL act_deposito_issste(vfolio,reg_2.*)
      END CASE
   END FOREACH
          
   DECLARE cursor_3 CURSOR FOR
   SELECT r.ident_pago,
          r.monto_soli_instituto,
          r.fecha_liquidacion,
          r.mto_acep_dev_afo_rcv,      
          r.mto_dev_parc_afo_rcv,      
          r.mto_pend_afo_rcv,          
          r.mto_rech_afo_rcv,          
          r.mto_acep_dev_afo_sar,      
          r.mto_dev_parc_afo_sar,      
          r.mto_pend_afo_sar,          
          r.mto_rech_afo_sar,          
          r.mto_acep_dev_afo_viv92,    
          r.mto_dev_parc_afo_viv92,    
          r.mto_pend_afo_viv92,        
          r.mto_rech_afo_viv92,        
          r.mto_acep_dev_afo_viv08,    
          r.mto_dev_parc_afo_viv08,    
          r.mto_pend_afo_viv08,        
          r.mto_rech_afo_viv08,        
          r.mto_acep_dev_afo_solid,    
          r.mto_dev_parc_afo_sar_solid,
          r.mto_pend_afo_sar_solid,    
          r.mto_rech_afo_sar_solid,
          r.siefore_desc,
          r.codigo_siefore
   FROM   exc_dep_exceso_issste r
   WHERE  r.folio  = vfolio
   ORDER BY 1

   LET pos8 = 0

   FOREACH cursor_3 INTO g_dep.ident_pago,
                         g_dep.monto_soli_instituto,
                         g_dep.fecha_liquidacion,
                         g_dep.mto_acep_dev_afo_rcv,      
                         g_dep.mto_dev_parc_afo_rcv,      
                         g_dep.mto_pend_afo_rcv,          
                         g_dep.mto_rech_afo_rcv,          
                         g_dep.mto_acep_dev_afo_sar,      
                         g_dep.mto_dev_parc_afo_sar,      
                         g_dep.mto_pend_afo_sar,          
                         g_dep.mto_rech_afo_sar,          
                         g_dep.mto_acep_dev_afo_viv92,    
                         g_dep.mto_dev_parc_afo_viv92,    
                         g_dep.mto_pend_afo_viv92,        
                         g_dep.mto_rech_afo_viv92,        
                         g_dep.mto_acep_dev_afo_viv08,    
                         g_dep.mto_dev_parc_afo_viv08,    
                         g_dep.mto_pend_afo_viv08,        
                         g_dep.mto_rech_afo_viv08,        
                         g_dep.mto_acep_dev_afo_solid,    
                         g_dep.mto_dev_parc_afo_sar_solid,
                         g_dep.mto_pend_afo_sar_solid,    
                         g_dep.mto_rech_afo_sar_solid,                            
                         g_dep.siefore_desc,
                         g_dep.codigo_siefore

      LET v_rep = g_dep.ident_pago,"|",
                  g_dep.monto_soli_instituto USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.fecha_liquidacion,"|",
                  g_dep.mto_acep_dev_afo_rcv USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_dev_parc_afo_rcv USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_pend_afo_rcv USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_rech_afo_rcv USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_acep_dev_afo_sar USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_dev_parc_afo_sar USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_pend_afo_sar USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_rech_afo_sar USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_acep_dev_afo_viv92 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_dev_parc_afo_viv92 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_pend_afo_viv92 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_rech_afo_viv92 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_acep_dev_afo_viv08 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_dev_parc_afo_viv08 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_pend_afo_viv08 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_rech_afo_viv08 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_acep_dev_afo_solid USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_dev_parc_afo_sar_solid USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.mto_pend_afo_sar_solid USING "&&&&&&&&&&&&&.&&","|", 
                  g_dep.mto_rech_afo_sar_solid USING "&&&&&&&&&&&&&.&&","|",                   
                  g_dep.siefore_desc,"|",
                  g_dep.codigo_siefore USING "&&&","|"

      LET pos8 = pos8 + 1

      OUTPUT TO REPORT r_report(v_rep,926,8)

   END FOREACH

   LET vtotal_registros = pos2 + pos8
   
   UPDATE exc_sum_exceso_issste
   SET    num_reg_pagar = pos2,
          num_tot_reg   = vtotal_registros
   WHERE  folio = vfolio

   SELECT A.tipo_ent_destino,
          A.clave_ent_destino,
          A.fecha_creac_lote,
          A.consecutivo_dia,
          A.tot_impt_sar_issste,
          A.tot_impt_ret_issste,
          A.tot_impt_cv_patron,
          A.tot_impt_fondo_viv92,
          A.tot_aplic_int_fondo_viv92,
          A.tot_impt_fondo_viv08,
          A.tot_aplic_int_fondo_viv08,
          A.tot_impt_ahorro_solid,
          A.num_reg_aport_lote,
          A.num_aport_dev,
          A.num_aport_dev_par,
          A.num_aport_rechazada,
          A.num_aport_pendiente,
          A.num_reg_pagar,
          A.num_tot_reg
   INTO  g_sum.*
   FROM   exc_sum_exceso_issste A
   WHERE  A.folio  = vfolio

   LET v_rep =g_sum.tipo_ent_origen           ,"|",
              g_sum.clave_ent_origen          ,"|",
              g_sum.fecha_creac_lote          ,"|",
              g_sum.consecutivo_dia           ,"|",
              g_sum.tot_impt_sar_issste       USING "&&&&&&&&&&&&&.&&","|",
              g_sum.tot_impt_ret_issste       USING "&&&&&&&&&&&&&.&&","|",
              g_sum.tot_impt_cv_patron        USING "&&&&&&&&&&&&&.&&","|",
              g_sum.tot_impt_fondo_viv92      USING "&&&&&&&&&&&&&.&&","|",
              g_sum.tot_aplic_int_fondo_viv92 USING "&&&&&&&&&&&&&.&&&&&&","|",
              g_sum.tot_impt_fondo_viv08      USING "&&&&&&&&&&&&&.&&","|",
              g_sum.tot_aplic_int_fondo_viv08 USING "&&&&&&&&&&&&&.&&&&&&","|",
              g_sum.tot_impt_ahorro_solid     USING "&&&&&&&&&&&&&.&&","|",
              g_sum.num_reg_aport_lote        ,"|",
              g_sum.num_aport_dev             ,"|",
              g_sum.num_aport_dev_par         ,"|",
              g_sum.num_aport_rechazada       ,"|",
              g_sum.num_aport_pendiente        ,"|",
              g_sum.num_reg_pagar             ,"|",
              g_sum.num_tot_reg               ,"|"
   
   OUTPUT TO REPORT r_report(v_rep,926,9)

END FUNCTION
######################################################################
FUNCTION act_deposito_imss(vfolio,reg_2)

   DEFINE
      vfolio                 INTEGER

   DEFINE
      qry1                   CHAR(1000),
      vident_pago            CHAR(2),
      vsiefore_desc          CHAR(8)

   DEFINE
      vmonto_tot_pat         ,
      vmonto_tot_par_viv     ,
      vmonto_tot_rcv         DECIMAL(18,6)

   DEFINE
      vcodigo_siefore        SMALLINT,
      vestado                SMALLINT,
      hay_reg_08             SMALLINT

   DEFINE reg_2 RECORD
      c2ident_pago           CHAR(2),
      codigo_siefore         SMALLINT,
      estado                 SMALLINT,
      fecha_estado           DATE,
      hora_estado            CHAR(8),
      tipo_registro          CHAR(2),
      ident_servicio         CHAR(2),
      ident_pago             CHAR(16),
      monto_soli_instituto   DECIMAL(18,6),
      fecha_liquidacion      DATE
   END RECORD

   DEFINE reg_acep RECORD
      tot_reg                SMALLINT,
      monto_ret              DECIMAL(18,6),
      monto_act_ret          DECIMAL(18,6),
      monto_ces_vej_pat      DECIMAL(18,6),
      monto_act_cv_pat       DECIMAL(18,6),
      monto_ces_vej_tra      DECIMAL(18,6),
      monto_act_cv_tra       DECIMAL(18,6),
      monto_cuo_soc          DECIMAL(18,6),
      monto_aport_pat        DECIMAL(18,6),
      monto_par_viv          DECIMAL(18,6),
      mto_act_ret_rend       DECIMAL(18,6),
      mto_act_cv_rend_pat    DECIMAL(18,6),
      mto_act_cv_rend_tra    DECIMAL(18,6)
    END RECORD

   DEFINE reg_rech RECORD
      tot_reg                SMALLINT,
      monto_ret              DECIMAL(18,6),
      monto_act_ret          DECIMAL(18,6),
      monto_ces_vej_pat      DECIMAL(18,6),
      monto_act_cv_pat       DECIMAL(18,6),
      monto_ces_vej_tra      DECIMAL(18,6),
      monto_act_cv_tra       DECIMAL(18,6),
      monto_cuo_soc          DECIMAL(18,6),
      monto_aport_pat        DECIMAL(18,6),
      monto_par_viv          DECIMAL(18,6),
      mto_act_ret_rend       DECIMAL(18,6),
      mto_act_cv_rend_pat    DECIMAL(18,6),
      mto_act_cv_rend_tra    DECIMAL(18,6)
    END RECORD

   DEFINE reg_pend RECORD
      tot_reg                SMALLINT,
      monto_ret              DECIMAL(18,6),
      monto_act_ret          DECIMAL(18,6),
      monto_ces_vej_pat      DECIMAL(18,6),
      monto_act_cv_pat       DECIMAL(18,6),
      monto_ces_vej_tra      DECIMAL(18,6),
      monto_act_cv_tra       DECIMAL(18,6),
      monto_cuo_soc          DECIMAL(18,6),
      monto_aport_pat        DECIMAL(18,6),
      monto_par_viv          DECIMAL(18,6),
      mto_act_ret_rend       DECIMAL(18,6),
      mto_act_cv_rend_pat    DECIMAL(18,6),
      mto_act_cv_rend_tra    DECIMAL(18,6)
    END RECORD

   DEFINE reg_1 RECORD
      siefore                SMALLINT,
      subcuenta              SMALLINT,
      tipo_movimiento        SMALLINT,
      monto_en_pesos         DECIMAL(22,6),
      monto_en_acciones      DECIMAL(22,6)
   END RECORD

   DEFINE reg_3 RECORD
      monto_aceptado         DECIMAL(18,6),
      mto_par_viv_acep       DECIMAL(18,6),
      monto_vivienda         DECIMAL(18,6)
   END RECORD

   SELECT COUNT(*)
   INTO   reg_acep.tot_reg
   FROM   exc_det_exceso a
   WHERE  a.folio            = vfolio
   AND    a.result_operacion IN ("01","04")

   SELECT SUM(monto_en_pesos) * -1
   INTO   vmonto_tot_rcv
   FROM   dis_provision
   WHERE  folio = vfolio
   AND    subcuenta in (1,2)

   SELECT SUM(monto_en_pesos) * -1,
          SUM(monto_en_acciones) * -1 
   INTO   vmonto_tot_pat,
          vmonto_tot_par_viv
   FROM   dis_provision 
   WHERE  folio = vfolio
   AND    subcuenta = 4

   IF reg_acep.tot_reg > 0 THEN
      UPDATE exc_sum_exceso
      SET monto_tot_rcv = vmonto_tot_rcv,
          monto_tot_pat = vmonto_tot_pat,
          monto_tot_par_viv = vmonto_tot_par_viv,
          num_apor_dev = reg_acep.tot_reg
      WHERE folio = vfolio
   END IF

   SELECT COUNT(*),
          NVL(SUM(b.monto_ret),0),
          NVL(SUM(b.monto_act_ret),0),
          NVL(SUM(b.monto_ces_vej_pat),0),
          NVL(SUM(b.monto_act_cv_pat),0),
          NVL(SUM(b.monto_ces_vej_tra),0),
          NVL(SUM(b.monto_act_cv_tra),0),
          NVL(SUM(b.monto_cuo_soc),0),
          NVL(SUM(b.monto_aport_pat),0),
          NVL(SUM(b.monto_par_viv),0),
          NVL(SUM(b.mto_act_ret_rend),0),
          NVL(SUM(b.mto_act_cv_rend_pat),0),
          NVL(SUM(b.mto_act_cv_rend_tra),0)
   INTO   reg_rech.*
   FROM   exc_det_exceso b
   WHERE  b.folio            = vfolio
   AND    b.result_operacion = "02"

   IF reg_rech.tot_reg > 0 THEN

DISPLAY "RECHAZOS"
DISPLAY "reg_2.ident_pago ",reg_2.ident_pago
DISPLAY "reg_2.codigo_siefore ",reg_2.codigo_siefore

      CASE reg_2.c2ident_pago 
         WHEN "71"  #-- RCV --#
            UPDATE exc_dep_exceso
            SET    monto_rechazado  = reg_rech.monto_ret          +
                                      reg_rech.monto_act_ret      +
                                      reg_rech.monto_ces_vej_pat  +
                                      reg_rech.monto_act_cv_pat   +
                                      reg_rech.monto_ces_vej_tra  +
                                      reg_rech.monto_act_cv_tra   +
                                      reg_rech.monto_cuo_soc      +
                                      reg_rech.mto_act_ret_rend   +
                                      reg_rech.mto_act_cv_rend_pat+
                                      reg_rech.mto_act_cv_rend_tra
            WHERE  folio = vfolio
            AND    ident_pago     = reg_2.ident_pago
            AND    codigo_siefore = reg_2.codigo_siefore
            
         WHEN "73"  #-- VIVIENDA --#      
            UPDATE exc_dep_exceso
            SET    mto_par_viv_rech = reg_rech.monto_par_viv,
                   monto_viv_rech   = reg_rech.monto_aport_pat
            WHERE  folio = vfolio
            AND    ident_pago     = reg_2.ident_pago
            AND    codigo_siefore = reg_2.codigo_siefore
      END CASE
      
      UPDATE exc_sum_exceso
      SET   num_apor_rechazada  = reg_rech.tot_reg
      WHERE folio = vfolio

   END IF

   SELECT COUNT(*),
          NVL(SUM(c.monto_ret),0),
          NVL(SUM(c.monto_act_ret),0),
          NVL(SUM(c.monto_ces_vej_pat),0),
          NVL(SUM(c.monto_act_cv_pat),0),
          NVL(SUM(c.monto_ces_vej_tra),0),
          NVL(SUM(c.monto_act_cv_tra),0),
          NVL(SUM(c.monto_cuo_soc),0),
          NVL(SUM(c.monto_aport_pat),0),
          NVL(SUM(c.monto_par_viv),0),
          NVL(SUM(c.mto_act_ret_rend),0),
          NVL(SUM(c.mto_act_cv_rend_pat),0),
          NVL(SUM(c.mto_act_cv_rend_tra),0)
   INTO   reg_pend.*
   FROM   exc_det_exceso c
   WHERE  c.folio            = vfolio
   AND    c.result_operacion = "03"

   IF reg_pend.tot_reg > 0 THEN

DISPLAY "PENDIENTE"
DISPLAY "reg_2.c2ident_pago ",reg_2.c2ident_pago
DISPLAY "reg_2.codigo_siefore ",reg_2.codigo_siefore

      UPDATE exc_dep_exceso
      SET    monto_pendiente  = reg_rech.monto_act_ret      +
                                reg_rech.monto_ces_vej_pat  +
                                reg_rech.monto_act_cv_pat   +
                                reg_rech.monto_ces_vej_tra  +
                                reg_rech.monto_act_cv_tra   +
                                reg_rech.monto_cuo_soc      +
                                reg_rech.mto_act_ret_rend   +
                                reg_rech.mto_act_cv_rend_pat+
                                reg_rech.mto_act_cv_rend_tra,
             mto_par_viv_pend = reg_rech.monto_par_viv,
             monto_viv_pend   = reg_rech.monto_aport_pat
      WHERE  folio = vfolio
      AND    ident_pago = reg_2.ident_pago
      AND    codigo_siefore = reg_2.codigo_siefore

      UPDATE exc_sum_exceso
      SET   num_apor_pendiente = reg_pend.tot_reg
      WHERE folio = vfolio

   END IF

   SELECT estado
   INTO   vestado
   FROM   exc_dep_exceso
   WHERE  folio          = vfolio
   AND    ident_pago     = reg_2.ident_pago
   AND    codigo_siefore = reg_2.codigo_siefore
   GROUP BY 1 

   IF vestado = 2 THEN
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 1, 1, 2, 1, 4, 3), ",
                        "DECODE(tipo_movimiento, 540, 1, 545, 1, 550, 1), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_provision ",
                 "WHERE  folio = ",vfolio," ",
                 "GROUP BY 1,2,3 ",
                 "ORDER BY 1,2,3 " CLIPPED
   ELSE
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 1, 1, 2, 1, 4, 3), ",
                        "DECODE(tipo_movimiento, 540, 1, 545, 1, 550, 1), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_cuenta ",
                 "WHERE  folio = ",vfolio," ",
--                 "AND    siefore = ",reg_2.codigo_siefore," ",
                 "GROUP BY 1,2,3 ",
                 "ORDER BY 1,2,3 " CLIPPED
   END IF

--subcuenta 1 = 1 --RCV
--subcuenta 2 = 2 --RCV le corresponde 1 pero se puso el 2 para poder sumarlo
--subcuenta 4 = 3 --VIVENDA

-- tipo_movimiento 540 = 1 --TOTAL Y PARCIAL
-- tipo_movimiento 545 = 1 --TOTAL Y PARCIAL
-- tipo_movimiento 550 = 1 --TOTAL Y PARCIAL


   PREPARE claexe2 FROM qry1

   DECLARE cur_2 CURSOR FOR claexe2

   LET reg_3.monto_aceptado      = 0
   LET reg_3.mto_par_viv_acep    = 0
   LET reg_3.monto_vivienda      = 0

   LET hay_reg_08 = 0

   FOREACH cur_2 INTO reg_1.*
      SELECT siefore_desc
      INTO   vsiefore_desc
      FROM   tab_siefore
      WHERE  afore_cod = p_tabafore.codigo_afore
      AND    siefore_cod = reg_1.siefore

      IF reg_1.tipo_movimiento = 1 AND
         reg_2.c2ident_pago[2] = reg_1.subcuenta THEN

         CASE reg_1.subcuenta
            WHEN 1
               LET reg_3.monto_aceptado = reg_1.monto_en_pesos * -1
               LET reg_3.monto_vivienda = 0
               LET reg_3.mto_par_viv_acep = 0
            WHEN 3
               LET reg_3.monto_vivienda = reg_1.monto_en_pesos * -1
               LET reg_3.mto_par_viv_acep = reg_1.monto_en_acciones * -1
               LET vsiefore_desc = " "
               LET reg_1.siefore = 0 
         END CASE

         SELECT COUNT(*)
         INTO   hay_reg_08
         FROM   exc_dep_exceso
         WHERE  folio          = vfolio
         AND    ident_pago     = reg_2.ident_pago
         AND    codigo_siefore = reg_2.codigo_siefore

         IF hay_reg_08 = 0 THEN
            INSERT INTO exc_dep_exceso
            VALUES (vfolio,                      ---folio
                    reg_2.estado,                ---estado
                    reg_2.fecha_estado,          ---fecha_estado
                    reg_2.hora_estado,           ---hora_estado
                    reg_2.tipo_registro,         ---tipo_registro
                    reg_2.ident_servicio,        ---ident_servicio
                    reg_2.ident_pago,            ---ident_pago
                    reg_2.monto_soli_instituto,  ---monto_soli_instituto
                    reg_2.fecha_liquidacion,     ---fecha_liquidacion
                    reg_3.monto_aceptado,        ---monto_aceptado
                    0,                           ---monto_parcial
                    0,                           ---monto_pendiente
                    0,                           ---monto_rechazado
                    0,                           ---monto_par_solicit
                    0,                           ---monto_par_aceptado
                    0,                           ---monto_par_parcial
                    0,                           ---monto_par_pendiente
                    0,                           ---monto_par_rechaza
                    0,                           ---monto_par_vivienda
                    reg_3.mto_par_viv_acep,      ---reg_3.mto_par_viv_acep
                    0,                           ---mto_par_viv_pend
                    0,                           ---mto_par_viv_rech
                    reg_3.monto_vivienda,        ---monto_vivienda
                    0,                           ---monto_viv_pend
                    0,                           ---monto_viv_rech
                    vsiefore_desc,               ---siefore_desc
                    reg_1.siefore                ---codigo_siefore
                   )
         ELSE
            UPDATE exc_dep_exceso
            SET    monto_aceptado   = reg_3.monto_aceptado,
                   mto_par_viv_acep = reg_3.mto_par_viv_acep,
                   monto_vivienda   = reg_3.monto_vivienda,
                   siefore_desc     = vsiefore_desc,
                   codigo_siefore   = reg_1.siefore
            WHERE  folio          = vfolio
            AND    ident_pago     = reg_2.ident_pago
         END IF
      END IF

   END FOREACH
END FUNCTION
######################################################################
FUNCTION act_deposito_issste(vfolio,reg_2)

   DEFINE 
      vfolio            INTEGER
      
   DEFINE
      qry1              CHAR(1000),
      vident_pago       CHAR(2),
      vsiefore_desc     CHAR(8)
      
   DEFINE
      vcodigo_siefore   SMALLINT,
      vestado           SMALLINT,
      hay_reg_08        SMALLINT

   DEFINE reg_2 RECORD
   	  c2ident_pago         CHAR(2),
      codigo_siefore       SMALLINT,
      estado               SMALLINT,
      fecha_estado         DATE,
      hora_estado          CHAR(8),
      tipo_registro        CHAR(2),
      ident_servicio       CHAR(2),
      ident_pago           CHAR(16),
      monto_soli_instituto DECIMAL(18,6),
      fecha_liquidacion    DATE
   END RECORD
        
   DEFINE reg_acep RECORD
      tot_reg                SMALLINT,
      impt_sar_issste        DECIMAL(18,6),
      impt_ret_issste        DECIMAL(18,6),
      impt_cv_patron         DECIMAL(18,6),  
      impt_fondo_viv92       DECIMAL(18,6),
      aplic_int_fondo_viv92  DECIMAL(18,6),
      impt_fondo_viv08       DECIMAL(18,6),
      aplic_int_fondo_viv08  DECIMAL(18,6),      
      impt_ahorro_solid      DECIMAL(18,6)
    END RECORD

   DEFINE reg_rech RECORD     
      tot_reg            SMALLINT,
      impt_sar_issste    DECIMAL(18,6),
      impt_ret_issste    DECIMAL(18,6),
      impt_cv_patron     DECIMAL(18,6),  
      impt_fondo_viv92   DECIMAL(18,6),
      impt_fondo_viv08   DECIMAL(18,6),
      impt_ahorro_solid  DECIMAL(18,6)
    END RECORD
    
   DEFINE reg_pend RECORD     
      tot_reg            SMALLINT,
      impt_sar_issste    DECIMAL(18,6),
      impt_ret_issste    DECIMAL(18,6),
      impt_cv_patron     DECIMAL(18,6),
      impt_fondo_viv92   DECIMAL(18,6),
      impt_fondo_viv08   DECIMAL(18,6),
      impt_ahorro_solid  DECIMAL(18,6)
    END RECORD
    
   DEFINE reg_parc RECORD     
      tot_reg                SMALLINT,
      impt_sar_issste        DECIMAL(18,6),
      impt_ret_issste        DECIMAL(18,6),
      impt_cv_patron         DECIMAL(18,6),  
      impt_fondo_viv92       DECIMAL(18,6),
      aplic_int_fondo_viv92  DECIMAL(18,6),
      impt_fondo_viv08       DECIMAL(18,6),
      aplic_int_fondo_viv08  DECIMAL(18,6),
      impt_ahorro_solid      DECIMAL(18,6)
    END RECORD

   DEFINE reg_1 RECORD
   	 siefore              SMALLINT,
   	 subcuenta            SMALLINT,
   	 tipo_movimiento      SMALLINT,
   	 monto_en_pesos       DECIMAL(22,6),
   	 monto_en_acciones    DECIMAL(22,6)
   END RECORD
   	      
   DEFINE reg_3 RECORD
          mto_acep_dev_afo_rcv   DECIMAL(18,6),
          mto_acep_dev_afo_viv92 DECIMAL(18,6),
          mto_acep_dev_afo_sar   DECIMAL(18,6),
          mto_acep_dev_afo_viv08 DECIMAL(18,6),
          mto_acep_dev_afo_solid DECIMAL(18,6),
          mto_dev_parc_afo_rcv   DECIMAL(18,6),
          mto_dev_parc_afo_viv92 DECIMAL(18,6),
          mto_dev_parc_afo_sar   DECIMAL(18,6),
          mto_dev_parc_afo_viv08 DECIMAL(18,6),
          mto_dev_parc_afo_solid DECIMAL(18,6)
   END RECORD

   SELECT COUNT(*),
          NVL(SUM(a.impt_sar_issste),0),
          NVL(SUM(a.impt_ret_issste),0),
          NVL(SUM(a.impt_cv_patron),0),
          NVL(SUM(a.impt_fondo_viv92),0),
          NVL(SUM(a.aplic_int_fondo_viv92),0),
          NVL(SUM(a.impt_fondo_viv08),0),
          NVL(SUM(a.aplic_int_fondo_viv08),0),
          NVL(SUM(a.impt_ahorro_solid),0)
   INTO   reg_acep.*  
   FROM   exc_det_exceso_issste a
   WHERE  a.folio            = vfolio
   AND    a.result_operacion = "01"

   IF reg_acep.tot_reg > 0 THEN
      UPDATE exc_sum_exceso_issste
      SET tot_impt_sar_issste      = reg_acep.impt_sar_issste,
          tot_impt_ret_issste      = reg_acep.impt_ret_issste,
          tot_impt_cv_patron       = reg_acep.impt_cv_patron,
          tot_impt_fondo_viv92     = reg_acep.impt_fondo_viv92,
          tot_aplic_int_fondo_viv92= reg_acep.aplic_int_fondo_viv92,
          tot_impt_fondo_viv08     = reg_acep.impt_fondo_viv08, 
          tot_aplic_int_fondo_viv08= reg_acep.aplic_int_fondo_viv08,
          tot_impt_ahorro_solid    = reg_acep.impt_ahorro_solid,
          num_aport_dev            = reg_acep.tot_reg
      WHERE folio = vfolio
   END IF
   	
   SELECT COUNT(*),
          NVL(SUM(b.impt_sar_issste),0),
          NVL(SUM(b.impt_ret_issste),0),
          NVL(SUM(b.impt_cv_patron),0),
          NVL(SUM(b.impt_fondo_viv92),0),
          NVL(SUM(b.impt_fondo_viv08),0),
          NVL(SUM(b.impt_ahorro_solid),0)
   INTO   reg_rech.*
   FROM   exc_det_exceso_issste b
   WHERE  b.folio            = vfolio
   AND    b.result_operacion = "02"

   IF reg_rech.tot_reg > 0 THEN

DISPLAY "RECHAZOS"
DISPLAY "reg_2.ident_pago ",reg_2.ident_pago
DISPLAY "reg_2.codigo_siefore ",reg_2.codigo_siefore

   	  UPDATE exc_dep_exceso_issste
   	  SET    mto_rech_afo_rcv = reg_rech.impt_ret_issste + reg_rech.impt_cv_patron, 
   	         mto_rech_afo_sar = reg_rech.impt_sar_issste,
   	         mto_rech_afo_viv92 = reg_rech.impt_fondo_viv92,
   	         mto_rech_afo_viv08 = reg_rech.impt_fondo_viv08,
   	         mto_rech_afo_sar_solid = reg_rech.impt_ahorro_solid
   	  WHERE  folio = vfolio 
   	  AND    ident_pago     = reg_2.ident_pago
   	  AND    codigo_siefore = reg_2.codigo_siefore        

      UPDATE exc_sum_exceso_issste                                         
      SET   num_aport_rechazada  = reg_rech.tot_reg                      
      WHERE folio = vfolio                                                 
   	  
   END IF  

   SELECT COUNT(*),
          NVL(SUM(c.impt_sar_issste),0),
          NVL(SUM(c.impt_ret_issste),0),
          NVL(SUM(c.impt_cv_patron),0),
          NVL(SUM(c.impt_fondo_viv92),0),
          NVL(SUM(c.impt_fondo_viv08),0),
          NVL(SUM(c.impt_ahorro_solid),0)
   INTO   reg_pend.*
   FROM   exc_det_exceso_issste c
   WHERE  c.folio            = vfolio
   AND    c.result_operacion = "03"

   IF reg_pend.tot_reg > 0 THEN
DISPLAY "PENDIENTE"   	
DISPLAY "reg_2.c2ident_pago ",reg_2.c2ident_pago
DISPLAY "reg_2.codigo_siefore ",reg_2.codigo_siefore
	
   	  UPDATE exc_dep_exceso_issste
   	  SET    mto_pend_afo_rcv       = reg_pend.impt_ret_issste + reg_pend.impt_cv_patron, 
   	         mto_pend_afo_sar       = reg_pend.impt_sar_issste,
   	         mto_pend_afo_viv92     = reg_pend.impt_fondo_viv92,
   	         mto_pend_afo_viv08     = reg_pend.impt_fondo_viv08,
   	         mto_pend_afo_sar_solid = reg_pend.impt_ahorro_solid
   	  WHERE  folio = vfolio 
   	  AND    ident_pago = reg_2.ident_pago
   	  AND    codigo_siefore = reg_2.codigo_siefore

      UPDATE exc_sum_exceso_issste                                         
      SET   num_aport_pendiente   = reg_pend.tot_reg                      
      WHERE folio = vfolio                   
         	  
   END IF  

   SELECT COUNT(*),
          NVL(SUM(d.impt_sar_issste),0),
          NVL(SUM(d.impt_ret_issste),0),           
          NVL(SUM(d.impt_cv_patron),0),            
          NVL(SUM(d.impt_fondo_viv92),0),          
          NVL(SUM(d.impt_fondo_viv08),0),
          NVL(SUM(d.impt_ahorro_solid),0)
   INTO   reg_parc.*
   FROM   exc_det_exceso_issste d
   WHERE  d.folio            = vfolio
   AND    d.result_operacion = "04"

   IF reg_parc.tot_reg > 0 THEN
      UPDATE exc_sum_exceso_issste
      SET tot_impt_sar_issste      = reg_acep.impt_sar_issste       + reg_parc.impt_sar_issste,
          tot_impt_ret_issste      = reg_acep.impt_ret_issste       + reg_parc.impt_ret_issste,
          tot_impt_cv_patron       = reg_acep.impt_cv_patron        + reg_parc.impt_cv_patron,
          tot_impt_fondo_viv92     = reg_acep.impt_fondo_viv92      + reg_parc.impt_fondo_viv92,
          tot_aplic_int_fondo_viv92= reg_acep.aplic_int_fondo_viv92 + reg_parc.aplic_int_fondo_viv92,
          tot_impt_fondo_viv08     = reg_acep.impt_fondo_viv08      + reg_parc.impt_fondo_viv08, 
          tot_aplic_int_fondo_viv08= reg_acep.aplic_int_fondo_viv08 + reg_parc.aplic_int_fondo_viv08,
          tot_impt_ahorro_solid    = reg_acep.impt_ahorro_solid     + reg_parc.impt_ahorro_solid,
          num_aport_dev_par        = reg_parc.tot_reg
      WHERE folio = vfolio
   END IF

   SELECT estado
   INTO   vestado
   FROM   exc_dep_exceso_issste
   WHERE  folio          = vfolio
   AND    ident_pago     = reg_2.ident_pago
   AND    codigo_siefore = reg_2.codigo_siefore
   
   IF vestado = 2 THEN
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 30, 1, 31, 1, 14, 2, 13 , 3, 19 , 3, 35, 4, 33, 7), ",
                        "DECODE(tipo_movimiento, 543, 1, 553, 1, 544, 2, 554, 2), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_provision ",
                 "WHERE  folio = ",vfolio," ",
                 "GROUP BY 1,2,3 ",
                 "ORDER BY 1,2,3 " CLIPPED
   ELSE
      LET qry1 = "SELECT siefore, ",
                        "DECODE(subcuenta, 30, 1, 31, 1, 14, 2, 13 , 3, 19 , 3, 35, 4, 33, 7), ",
                        "DECODE(tipo_movimiento, 543, 1, 553, 1, 544, 2, 554, 2), ",
                        "SUM(monto_en_pesos), ",
                        "SUM(monto_en_acciones) ",
                 "FROM   dis_cuenta ",
                 "WHERE  folio = ",vfolio," ",
					  "AND    siefore = ",reg_2.codigo_siefore," ",
                 "GROUP BY 1,2,3 ",
                 "ORDER BY 1,2,3 " CLIPPED
   END IF
   
--subcuenta 30 = 1 --RCV ISSSTE
--subcuenta 31 = 1
--subcuenta 14 = 2 --VIVIENDA 92
--subcuenta 13 = 3 --RETIRO SAR ISSSTE
--subcuenta 19 = 3 --SAR ISSSTE BANXICO
--subcuenta 35 = 4 --VIVIENDA 08
--subcuenta 33 = 7 --AHORRO SOLIDARIO

-- tipo_movimiento 543 = 1 --TOTAL
-- tipo_movimiento 553 = 1 --TOTAL
-- tipo_movimiento 544 = 2 --PARCIAL
-- tipo_movimiento 554 = 2 --PARCIAL

   PREPARE claexe1 FROM qry1

   DECLARE cur_1 CURSOR FOR claexe1

   LET reg_3.mto_acep_dev_afo_rcv   = 0
   LET reg_3.mto_acep_dev_afo_viv92 = 0
   LET reg_3.mto_acep_dev_afo_sar   = 0
   LET reg_3.mto_acep_dev_afo_viv08 = 0
   LET reg_3.mto_acep_dev_afo_solid = 0
          
   LET reg_3.mto_dev_parc_afo_rcv   = 0
   LET reg_3.mto_dev_parc_afo_viv92 = 0
   LET reg_3.mto_dev_parc_afo_sar   = 0
   LET reg_3.mto_dev_parc_afo_viv08 = 0
   LET reg_3.mto_dev_parc_afo_solid = 0
   
   LET hay_reg_08 = 0
   
   FOREACH cur_1 INTO reg_1.*
   	  SELECT siefore_desc
   	  INTO   vsiefore_desc
   	  FROM   tab_siefore
   	  WHERE  afore_cod = p_tabafore.codigo_afore
   	  AND    siefore_cod = reg_1.siefore

   	  IF reg_1.tipo_movimiento = 1 AND 
   	  	 reg_2.c2ident_pago[2] = reg_1.subcuenta THEN

   	     CASE reg_1.subcuenta
   	     	 WHEN 1
   	     	 	  LET reg_3.mto_acep_dev_afo_rcv   = reg_1.monto_en_pesos * -1
   	     	 WHEN 2
   	     	 	  LET reg_3.mto_acep_dev_afo_viv92 = reg_1.monto_en_pesos * -1
   	     	 	  LET vsiefore_desc = "0"
   	     	 	  LET reg_1.siefore = 0 	 	     	  	 	  
   	     	 WHEN 3
   	     	 	  LET reg_3.mto_acep_dev_afo_sar   = reg_1.monto_en_pesos * -1
   	     	 WHEN 4
   	     	 	  LET reg_3.mto_acep_dev_afo_viv08 = reg_1.monto_en_pesos * -1 
   	     	 	  LET vsiefore_desc = "0"
   	     	 	  LET reg_1.siefore = 0 	 	     	  	 	  
   	     	 WHEN 7
   	     	 	  LET reg_3.mto_acep_dev_afo_solid = reg_1.monto_en_pesos * -1
   	     END CASE  	  	 	     	  	 	     	  	 	  
   	     
   	     SELECT COUNT(*)
   	     INTO   hay_reg_08
   	     FROM   exc_dep_exceso_issste   
   	     WHERE  folio          = vfolio
   	     AND    ident_pago     = reg_2.ident_pago
   	     AND    codigo_siefore = reg_2.codigo_siefore
   	     
   	     IF hay_reg_08 = 0 THEN
   	  	    INSERT INTO exc_dep_exceso_issste
   	  	    VALUES (vfolio,                      ---folio                      
   	  	            reg_2.estado,                ---estado                     
                    reg_2.fecha_estado,          ---fecha_estado               
                    reg_2.hora_estado,           ---hora_estado                
                    reg_2.tipo_registro,         ---tipo_registro              
                    reg_2.ident_servicio,        ---ident_servicio             
                    reg_2.ident_pago,            ---ident_pago                 
                    reg_2.monto_soli_instituto,  ---monto_soli_instituto       
                    reg_2.fecha_liquidacion,      ---fecha_liquidacion          
                    reg_3.mto_acep_dev_afo_rcv,  ---mto_acep_dev_afo_rcv       
                    0,                           ---mto_dev_parc_afo_rcv       
                    0,                           ---mto_pend_afo_rcv           
                    0,                           ---mto_rech_afo_rcv           
                    reg_3.mto_acep_dev_afo_sar,  ---mto_acep_dev_afo_sar       
                    0,                           ---mto_dev_parc_afo_sar       
                    0,                           ---mto_pend_afo_sar           
                    0,                           ---mto_rech_afo_sar           
                    reg_3.mto_acep_dev_afo_viv92,---mto_acep_dev_afo_viv92     
                    0,                           ---mto_dev_parc_afo_viv92     
                    0,                           ---mto_pend_afo_viv92         
                    0,                           ---mto_rech_afo_viv92         
                    reg_3.mto_acep_dev_afo_viv08,---mto_acep_dev_afo_viv08     
                    0,                           ---mto_dev_parc_afo_viv08     
                    0,                           ---mto_pend_afo_viv08         
                    0,                           ---mto_rech_afo_viv08         
                    reg_3.mto_acep_dev_afo_solid,---mto_acep_dev_afo_solid     
                    0,                           ---mto_dev_parc_afo_sar_solid 
                    0,                           ---mto_pend_afo_sar_solid     
                    0,                           ---mto_rech_afo_sar_solid     
                    vsiefore_desc,               ---siefore_desc               
                    reg_1.siefore                ---codigo_siefore             
                   )
         ELSE
         	  UPDATE exc_dep_exceso_issste
         	  SET    mto_acep_dev_afo_rcv   = reg_3.mto_acep_dev_afo_rcv,
         	         mto_acep_dev_afo_sar   = reg_3.mto_acep_dev_afo_sar,
         	         mto_acep_dev_afo_viv92 = reg_3.mto_acep_dev_afo_viv92,
         	         mto_acep_dev_afo_viv08 = reg_3.mto_acep_dev_afo_viv08,
         	         mto_acep_dev_afo_solid = reg_3.mto_acep_dev_afo_solid,
         	         siefore_desc           = vsiefore_desc,
         	         codigo_siefore         = reg_1.siefore
         	  WHERE  folio          = vfolio
   	        AND    ident_pago     = reg_2.ident_pago
   	        AND    codigo_siefore = reg_2.codigo_siefore
         END IF
   	  END IF
   	  
   	  IF reg_1.tipo_movimiento = 2 AND 
   	  	 reg_2.c2ident_pago[2] = reg_1.subcuenta THEN
   	  	 
   	     CASE reg_1.subcuenta
   	     	 WHEN 1
   	     	 	  LET reg_3.mto_dev_parc_afo_rcv   = reg_1.monto_en_pesos * -1
   	     	 WHEN 2
   	     	 	  LET reg_3.mto_dev_parc_afo_viv92 = reg_1.monto_en_pesos * -1
   	     	 	  LET vsiefore_desc = "0"
   	     	 	  LET reg_1.siefore = 0 	 	     	  	 	  
   	     	 WHEN 3
   	     	 	  LET reg_3.mto_dev_parc_afo_sar   = reg_1.monto_en_pesos * -1
   	     	 WHEN 4
   	     	 	  LET reg_3.mto_dev_parc_afo_viv08 = reg_1.monto_en_pesos * -1  
   	     	 	  LET vsiefore_desc = "0"
   	     	 	  LET reg_1.siefore = 0 	 	     	  	 	  	  	 	     	  	 	  
   	     	 WHEN 7
   	     	 	  LET reg_3.mto_dev_parc_afo_solid = reg_1.monto_en_pesos * -1
   	     END CASE  	  	 	     	  	 	     	  	 	  
   	     
   	     SELECT COUNT(*)
   	     INTO   hay_reg_08
   	     FROM   exc_dep_exceso_issste   
   	     WHERE  folio          = vfolio
   	     AND    ident_pago     = reg_2.ident_pago
   	     AND    codigo_siefore = reg_2.codigo_siefore
   	     
   	     IF hay_reg_08 = 0 THEN
   	  	    INSERT INTO exc_dep_exceso_issste
   	  	    VALUES (vfolio,                      ---folio                      
   	  	            reg_2.estado,                ---estado                     
                    reg_2.fecha_estado,          ---fecha_estado               
                    reg_2.hora_estado,           ---hora_estado                
                    reg_2.tipo_registro,         ---tipo_registro              
                    reg_2.ident_servicio,        ---ident_servicio             
                    reg_2.ident_pago,            ---ident_pago                 
                    reg_2.monto_soli_instituto,  ---monto_soli_instituto       
                    reg_2.fecha_liquidacion,     ---fecha_liquidacion          
                    0,                           ---mto_acep_dev_afo_rcv       
                    reg_3.mto_dev_parc_afo_rcv,  ---mto_dev_parc_afo_rcv       
                    0,                           ---mto_pend_afo_rcv           
                    0,                           ---mto_rech_afo_rcv           
                    0,                           ---mto_acep_dev_afo_sar       
                    reg_3.mto_dev_parc_afo_sar,  ---mto_dev_parc_afo_sar       
                    0,                           ---mto_pend_afo_sar           
                    0,                           ---mto_rech_afo_sar           
                    0,                           ---mto_acep_dev_afo_viv92     
                    reg_3.mto_dev_parc_afo_viv92,---mto_dev_parc_afo_viv92     
                    0,                           ---mto_pend_afo_viv92         
                    0,                           ---mto_rech_afo_viv92         
                    0,                           ---mto_acep_dev_afo_viv08     
                    reg_3.mto_dev_parc_afo_viv08,---mto_dev_parc_afo_viv08     
                    0,                           ---mto_pend_afo_viv08         
                    0,                           ---mto_rech_afo_viv08         
                    0,                           ---mto_acep_dev_afo_solid     
                    reg_3.mto_dev_parc_afo_solid,---mto_dev_parc_afo_sar_solid 
                    0,                           ---mto_pend_afo_sar_solid     
                    0,                           ---mto_rech_afo_sar_solid     
                    vsiefore_desc,               ---siefore_desc               
                    reg_1.siefore                ---codigo_siefore             
                   )
         ELSE
         	  UPDATE exc_dep_exceso_issste
         	  SET    mto_dev_parc_afo_rcv       = reg_3.mto_dev_parc_afo_rcv,
         	         mto_dev_parc_afo_sar       = reg_3.mto_dev_parc_afo_sar,
         	         mto_dev_parc_afo_viv92     = reg_3.mto_dev_parc_afo_viv92,
         	         mto_dev_parc_afo_viv08     = reg_3.mto_dev_parc_afo_viv08,
         	         mto_dev_parc_afo_sar_solid = reg_3.mto_dev_parc_afo_solid,
         	         siefore_desc               = vsiefore_desc,
         	         codigo_siefore             = reg_1.siefore
         	  WHERE  folio          = vfolio
   	        AND    ident_pago     = reg_2.ident_pago
   	        AND    codigo_siefore = reg_2.codigo_siefore
   	     END IF
   	  END IF   	  
   END FOREACH
END FUNCTION
