#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB003                                         #
#Descripcion       => RESPUESTA A PROCESAR DE PAGOS EN EXCESO BATCH   #
#Fecha Inicio      => 14 de diciembre de 2000.                        #
#Fecha Termino     => 28 de marzo de 2001.                            #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#Actualizado       => 21 de noviembre de 2001                         #
#Actualizado       => 21 de febrero de 2002.                          #
#Actualizado       => 06 de junio de 2002.                            #
#Actualizado       => 08 de enero de 2005.                            #
#Fecha modifica    => 11 de abril 2005.                               #
#Por               => OMAR SANDOVAL BADILLO                           #
#Fecha modifica    => 24 DE AGOSTO 2005                               #
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
          v_rep               CHAR(400), 
          vreport             CHAR(800),
          v_rep_08,
          v_ruta, 
          v_arch              CHAR(100),
          t_fecha             CHAR(12)

   DEFINE p_tabafore  RECORD  LIKE tab_afore_local.*

   DEFINE g_cza  RECORD   LIKE exc_cza_exceso.*

   DEFINE g_det  RECORD   LIKE exc_det_exceso.*

   DEFINE g_dep  RECORD   LIKE exc_dep_exceso.*

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

   DEFINE g_sum  RECORD
          fecha_creac_lote    DATE,
          consecutivo_dia     CHAR(3),
          num_reg_aport_lote  CHAR(8)
   END RECORD

   DEFINE vfolio              INTEGER,
          vtotal_registros    SMALLINT,
          usuario             CHAR(10),
          pos2                SMALLINT,
          pos3                SMALLINT,
          pos4                SMALLINT,
          pos5                SMALLINT,
          pos8                SMALLINT

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
          vconsecutivo          INTEGER,
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

   LET vfolio = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   p_tabafore.*,
          usuario
   FROM   tab_afore_local

   SELECT ruta_envio 
   INTO   v_ruta 
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET log1 = "/tmp/",usuario CLIPPED,".EXCB003.log"

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

    SELECT a.fecha_creac_lote
    INTO   hoy1
    FROM   exc_cza_exceso a
    WHERE  a.folio  = vfolio

    LET hora = TIME 

    LET v_arch = v_ruta CLIPPED,"/",hoy1 USING "yyyymmdd" ,".DEV"

    START REPORT r_report TO v_arch

       SELECT fecha_creac_lote,
              consecutivo_dia,
              fecha_limite_resp
       INTO   g_cza.fecha_creac_lote,
              g_cza.consecutivo_dia,
              g_cza.fecha_limite_resp
       FROM   exc_cza_exceso
       WHERE  folio  = vfolio

       LET v_rep = "01","|",
                   p_tabafore.codigo_afore,"|",
                   g_cza.fecha_creac_lote,"|",
                   g_cza.consecutivo_dia,"|",
                   g_cza.fecha_limite_resp,"|"

       OUTPUT TO REPORT r_report(v_rep,356,1)

       CALL reg_02()

    FINISH REPORT r_report

    LET permisos = "chmod 777 ",v_arch CLIPPED
    RUN permisos

    RETURN vfolio
END FUNCTION
#*********************************************************************
FUNCTION reg_02()

   DECLARE cursor_1 CURSOR FOR
   SELECT * 
   FROM   exc_det_exceso
   WHERE  folio  = vfolio
   ORDER BY consec_reg_lote

   LET pos2 = 0
   LET pos3 = 0
   LET pos4 = 0
   LET pos5 = 0

   FOREACH cursor_1 INTO g_det.*

      LET v_rep = g_det.consec_reg_lote,"|",
                  g_det.nss,"|",
                  g_det.clave_ent_orig,"|",
                  g_det.rfc,"|",
                  g_det.curp,"|",
                  g_det.nom_trabajador,"|",
                  g_det.periodo_pago,"|",
                  g_det.fecha_pago,"|",
                  g_det.fecha_valor_rcv,"|",
                  g_det.fecha_valor_viv,"|",
                  g_det.folio_pago_sua,"|",
                  g_det.clave_ent_recep,"|",
                  g_det.reg_patronal_imss,"|",
                  g_det.monto_ret USING "&&&&&&&.&&","|",
                  g_det.monto_act_ret USING "&&&&&&&.&&","|",
                  g_det.monto_ces_vej_pat USING "&&&&&&&.&&","|",
                  g_det.monto_ces_vej_tra USING "&&&&&&&.&&","|",
                  g_det.monto_act_ces_vej USING "&&&&&&&.&&","|",
                  g_det.monto_cuo_soc USING "&&&&&&&.&&","|",
                  g_det.monto_aport_est USING "&&&&&&&.&&","|",
                  g_det.monto_aport_esp USING "&&&&&&&.&&","|",
                  g_det.monto_aport_pat USING "&&&&&&&.&&","|",
                  g_det.dias_cotz_bimestre USING "&&&&&&&.&&","|",
                  g_det.monto_par_viv USING "&&&&&&&&&.&&&&&&","|",
                  g_det.result_operacion,"|",
                  g_det.tipo_diagnostico,"|"

      LET pos2 = pos2 + 1

      OUTPUT TO REPORT r_report(v_rep,356,2)

      LET vmonto_comi_ret = 0
      LET vmonto_comi_ces_vej = 0

      SELECT ROUND(monto_comi_ret,2),
             ROUND(monto_comi_ces_vej,2)
      INTO   vmonto_comi_ret,
             vmonto_comi_ces_vej
      FROM   exc_exceso_comis
      WHERE  folio  = vfolio
      AND    nss    = g_det.nss
      AND    consec_reg_lote = g_det.consec_reg_lote

      LET v_rep = g_det.consec_reg_lote,"|",
                  g_det.reg_patronal_imss,"|",
                  g_det.nss,"|",
                  g_det.monto_ret USING "&&&&&&&.&&","|",
                  g_det.monto_act_ret USING "&&&&&&&.&&","|",
                  vmonto_comi_ret USING "&&&&&&&.&&","|",  -- importe comisiones de retiro,"|",
                  g_det.monto_ces_vej_pat USING "&&&&&&&.&&","|",
                  g_det.monto_ces_vej_tra USING "&&&&&&&.&&","|",
                  g_det.monto_act_ces_vej USING "&&&&&&&.&&","|",
                  vmonto_comi_ces_vej USING "&&&&&&&.&&","|", --imp comi de ces y vejez,"|",
                  g_det.monto_aport_pat USING "&&&&&&&.&&","|",
                  g_det.monto_par_viv USING "&&&&&&&&&.&&&&&&","|",
                  g_det.result_operacion,"|",
                  g_det.tipo_diagnostico,"|"

      LET pos3 = pos3 + 1

      OUTPUT TO REPORT r_report(v_rep,356,3)

      IF g_det.result_operacion = "01" THEN
         LET vmonto_plus_ret = 0
         LET vmonto_plus_ces_vej = 0
         LET vmonto_plus_pat = 0

         SELECT ROUND(monto_plus_ret,2),
                ROUND(monto_plus_ces_vej,2),
                ROUND(monto_plus_pat,2)
         INTO   vmonto_plus_ret,
                vmonto_plus_ces_vej,
                vmonto_plus_pat
         FROM   exc_exceso_plu_min
         WHERE  folio  = vfolio
         AND    nss = g_det.nss
         AND    consec_reg_lote = g_det.consec_reg_lote
         AND    tipo_registro = "04"

         LET v_rep = g_det.consec_reg_lote,"|",
                     g_det.reg_patronal_imss,"|",
                     g_det.nss,"|",
                     vmonto_plus_ret USING "&&&&&&&.&&","|",     --imp. plus. por retiro
                     vmonto_plus_ces_vej USING "&&&&&&&.&&","|" --imp. plus. por ces y vejez
                     #vmonto_plus_pat USING "&&&&&&&.&&","|"      --imp. plus. de viv.

         LET pos4 = pos4 + 1

         OUTPUT TO REPORT r_report(v_rep,356,4)

         LET vmonto_min_ret = 0
         LET vmonto_min_ces_vej = 0

         SELECT ROUND(monto_plus_ret,2),
                ROUND(monto_plus_ces_vej,2)
         INTO   vmonto_min_ret,
                vmonto_min_ces_vej
         FROM   exc_exceso_plu_min
         WHERE  folio  = vfolio
         AND    nss = g_det.nss
         AND    consec_reg_lote = g_det.consec_reg_lote
         AND    tipo_registro = "05"

         LET v_rep = g_det.consec_reg_lote,"|",
                     g_det.reg_patronal_imss,"|",
                     g_det.nss,"|",
                     vmonto_min_ret USING "&&&&&&&.&&","|"       --imp. minus. por retiro
                     #vmonto_min_ces_vej USING "&&&&&&&.&&","|"    --imp. min. ces y vejez

         LET pos5 = pos5 + 1

         OUTPUT TO REPORT r_report(v_rep,356,5)
      END IF
   END FOREACH

   CALL actualiza(g_det.*,p_tabafore.codigo_afore)

END FUNCTION
#*********************************************************************
FUNCTION actualiza(g_det,codigo_afore)

   DEFINE g_det  RECORD   LIKE exc_det_exceso.*
   DEFINE recha  RECORD   LIKE exc_dep_exceso.*

   DEFINE codigo_afore  CHAR(20)

   DEFINE sie RECORD
	  codigo_siefore    SMALLINT,
          siefore_desc      CHAR(20)
   END RECORD

   DEFINE sie_pos       SMALLINT

   DEFINE total_temp      DECIMAL(16,2),
          total_temp_sie  DECIMAL(16,2),
          diferencia      DECIMAL(16,2)

   DEFINE x_sub_acep         DECIMAL(16,2),
          x_sub_plu          DECIMAL(16,2),
          x_sub_comi         DECIMAL(16,2),
          sel_txt            CHAR(1000)

   LET total_temp = 0
   LET total_temp_sie = 0
   LET diferencia = 0

   SELECT COUNT(*),
          SUM(ROUND(a.monto_ret,2)),
          SUM(ROUND(a.monto_act_ret,2)),
          SUM(ROUND(a.monto_ces_vej_pat,2)),
          SUM(ROUND(a.monto_ces_vej_tra,2)),
          SUM(ROUND(a.monto_act_ces_vej,2)),
          SUM(ROUND(a.monto_aport_pat,2)),
          SUM(a.monto_par_viv)
   INTO   sum1.aceptados_tot,
          sum1.monto_ret,
          sum1.monto_act_ret,
          sum1.monto_ces_vej_pat,
          sum1.monto_ces_vej_tra,
          sum1.monto_act_ces_vej,
          sum1.monto_aport_pat,
          sum1.monto_par_viv #participacion
   FROM   exc_det_exceso a
   WHERE  a.folio            = vfolio
   AND    a.result_operacion = "01"

   SELECT COUNT(*),
          SUM(ROUND(b.monto_ret,2)),
          SUM(ROUND(b.monto_act_ret,2)),
          SUM(ROUND(b.monto_ces_vej_pat,2)),
          SUM(ROUND(b.monto_ces_vej_tra,2)),
          SUM(ROUND(b.monto_act_ces_vej,2)),
          SUM(ROUND(b.monto_aport_pat,2)),
          SUM(b.monto_par_viv)
   INTO   sum2.rechazados_tot,
          sum2.monto_ret,
          sum2.monto_act_ret,
          sum2.monto_ces_vej_pat,
          sum2.monto_ces_vej_tra,
          sum2.monto_act_ces_vej,
          sum2.monto_aport_pat,
          sum2.monto_par_viv #participacion
   FROM   exc_det_exceso b
   WHERE  b.folio            = vfolio
   AND    b.result_operacion = "02"  

   SELECT COUNT(*),
          SUM(ROUND(c.monto_ret,2)),
          SUM(ROUND(c.monto_act_ret,2)),
          SUM(ROUND(c.monto_ces_vej_pat,2)),
          SUM(ROUND(c.monto_ces_vej_tra,2)),
          SUM(ROUND(c.monto_act_ces_vej,2)),
          SUM(ROUND(c.monto_aport_pat,2)),
          SUM(c.monto_par_viv)
   INTO   sum3.pendientes_tot,
          sum3.monto_ret,
          sum3.monto_act_ret,
          sum3.monto_ces_vej_pat,
          sum3.monto_ces_vej_tra,
          sum3.monto_act_ces_vej,
          sum3.monto_aport_pat,
          sum3.monto_par_viv #participacion
   FROM   exc_det_exceso c
   WHERE  c.folio            = vfolio
   AND    c.result_operacion = "03"

   SELECT SUM(ROUND(d.monto_comi_ret,2)),
          SUM(ROUND(d.monto_comi_ces_vej,2))
   INTO   sum1.monto_comi_ret,
          sum1.monto_comi_ces_vej
   FROM   exc_exceso_comis d
   WHERE  d.folio            = vfolio

   SELECT SUM(ROUND(e.monto_plus_ret,2)),
          SUM(ROUND(e.monto_plus_ces_vej,2)),
          SUM(ROUND(e.monto_plus_pat,2))
   INTO   sum1.monto_plus_ret,
          sum1.monto_plus_ces_vej,
          sum1.monto_plus_pat
   FROM   exc_exceso_plu_min e
   WHERE  e.folio  = vfolio
   AND    e.tipo_registro = "04"

   IF sum1.monto_plus_ret IS NULL THEN
      LET sum1.monto_plus_ret     = 0
   END IF

   IF sum1.monto_plus_ces_vej IS NULL THEN
      LET sum1.monto_plus_ces_vej = 0
   END IF

   IF sum1.monto_plus_pat IS NULL THEN
      LET sum1.monto_plus_pat     = 0
   END IF

   SELECT SUM(ROUND(f.monto_plus_ret,2)),
          SUM(ROUND(f.monto_plus_ces_vej,2))
   INTO   sum1.monto_min_ret,
          sum1.monto_min_ces_vej
   FROM   exc_exceso_plu_min f
   WHERE  f.folio         = vfolio
   AND    f.tipo_registro = "05"

   IF sum1.monto_min_ret IS NULL THEN
      LET sum1.monto_min_ret     = 0
   END IF

   IF sum1.monto_min_ces_vej IS NULL THEN
      LET sum1.monto_min_ces_vej = 0
   END IF

   DECLARE cursor_2 CURSOR FOR
   SELECT g.ident_pago[14,15],
          g.codigo_siefore
   FROM   exc_dep_exceso g
   WHERE  g.folio = vfolio

   FOREACH cursor_2 INTO vident_pago,
                         vcodigo_siefore

      CASE vident_pago
         WHEN "71"   -- RCV patron
            WHENEVER ERROR CONTINUE
               DROP TABLE tmp_exc_dep_exceso
            WHENEVER ERROR STOP

            CALL genera_tmp_cuenta (vfolio)

	    DECLARE cur_cuenta CURSOR FOR 
            SELECT a.siefore,
                   b.razon_social,
                   SUM(ROUND(a.monto_en_pesos,2))*-1
            FROM   tmp_dis_cuenta a, tab_siefore_local b
            WHERE  a.folio = vfolio
            AND    a.subcuenta IN (1,2)
	    AND    a.tipo_movimiento BETWEEN 540 AND 555
            AND    a.siefore = b.codigo_siefore
            GROUP BY 1,2
            ORDER BY 1

            LET sie_pos = 1

            FOREACH cur_cuenta INTO sie.codigo_siefore,
                                    sie.siefore_desc,
                                    vmonto_sub_acep

               IF sie.codigo_siefore <> 11 THEN
                  LET sel_txt = "SELECT SUM(ROUND(monto_ret,2) + ",
                               " ROUND(monto_act_ret,2) + ",
                               " ROUND(monto_ces_vej_pat,2) + ",
                               " ROUND(monto_act_ces_vej,2)) ",
                       " FROM   exc_det_exceso ",
                       " WHERE  folio =", vfolio ,
                       " AND    nss IN (SELECT UNIQUE a.nss ",
                                       "FROM tmp_dis_cuenta a ",
                                       "WHERE a.folio =", vfolio,
                                       "AND  a.siefore =", sie.codigo_siefore," ) ",
                       " AND    result_operacion = '01' "

                  PREPARE exe_sel1 FROM sel_txt
                  DECLARE cursor_11 CURSOR FOR exe_sel1

                  FOREACH cursor_11 INTO x_sub_acep
                  END FOREACH

                  LET sel_txt = "SELECT SUM(ROUND(monto_plus_ret,2) + ",
                                "ROUND(monto_plus_ces_vej,2)) ",
                       "FROM   exc_exceso_plu_min ",
                       "WHERE  folio = ",vfolio,
                       "AND    nss IN (SELECT UNIQUE a.nss ",
                                      "FROM tmp_dis_cuenta a ",
                                      "WHERE a.folio = ",vfolio,
                                      "AND  a.siefore = ",sie.codigo_siefore," )"

                  PREPARE exe_sel2 FROM sel_txt
                  DECLARE cursor_12 CURSOR FOR exe_sel2

                  FOREACH cursor_12 INTO x_sub_plu
                  END FOREACH

                  LET sel_txt = "SELECT SUM(ROUND(monto_comi_ret,2) + ",
                              "ROUND(monto_comi_ces_vej,2))*-1 ",
                       "FROM   exc_exceso_comis ",
                       "WHERE  folio = ",vfolio,
                       "AND    nss IN (SELECT UNIQUE a.nss ",
                                      "FROM   tmp_dis_cuenta a ",
                                      "WHERE  a.folio = ",vfolio,
                                      "AND    a.siefore = ",sie.codigo_siefore," )"
                  PREPARE exe_sel3 FROM sel_txt
                  DECLARE cursor_13 CURSOR FOR exe_sel3

                  FOREACH cursor_13 INTO x_sub_comi
                  END FOREACH
               END IF
{
      CASE x_siefore
         WHEN 1 LET vmonto_sub_rcv  = x_sub_acep + x_sub_plu + x_sub_comi
         WHEN 2 LET vmonto_sub_rcv2 = x_sub_acep + x_sub_plu + x_sub_comi
      END CASE
}
               LET vmonto_sub_acep = x_sub_acep + x_sub_plu + x_sub_comi

               SELECT COUNT(*)
               INTO   v_exc_dep_cuantos
               FROM   exc_dep_exceso
               WHERE  folio = vfolio
               AND    codigo_siefore = sie.codigo_siefore

               IF v_exc_dep_cuantos > 0 THEN
                  UPDATE exc_dep_exceso
                  SET    exc_dep_exceso.codigo_siefore = sie.codigo_siefore,
                         exc_dep_exceso.siefore_desc = sie.siefore_desc,
                         exc_dep_exceso.monto_aceptado = vmonto_sub_acep
                  WHERE  exc_dep_exceso.folio = vfolio
                  AND    exc_dep_exceso.ident_pago[14,15] = "71"
                  AND    exc_dep_exceso.codigo_siefore = sie.codigo_siefore
               ELSE
                  SELECT *
                  FROM   exc_dep_exceso
                  WHERE  folio = vfolio
                  AND    ident_pago[14,15] = 71
                  INTO TEMP tmp_exc_dep_exceso

                  UPDATE tmp_exc_dep_exceso
                  SET    codigo_siefore = sie.codigo_siefore,
                         siefore_desc = sie.siefore_desc,
                         monto_aceptado = vmonto_sub_acep

                  INSERT INTO exc_dep_exceso
                  SELECT *
                  FROM   tmp_exc_dep_exceso
               END IF
               LET sie_pos = sie_pos + 1
            END FOREACH

            LET vmonto_sub_pend = sum3.monto_ret +            # a
                                  sum3.monto_act_ret +        # b
                                  sum3.monto_ces_vej_pat +    # c
                                  sum3.monto_act_ces_vej      # e

            LET vmonto_sub_rech = sum2.monto_ret +            # a
                                  sum2.monto_act_ret +        # b
                                  sum2.monto_ces_vej_pat +    # c
                                  sum2.monto_act_ces_vej      # e

            IF vmonto_sub_rech > 0 THEN
               DECLARE cur_rechazo CURSOR FOR
               SELECT *
               FROM   exc_dep_exceso
               WHERE  folio = vfolio
               AND    ident_pago[14,15] = 71
               AND    codigo_siefore > 0

               FOREACH cur_rechazo INTO recha.*
                  DELETE FROM exc_dep_exceso
                  WHERE  folio = vfolio
                  AND    ident_pago[14,15] = 71
                  AND    codigo_siefore = 0

                  LET recha.codigo_siefore = 0
                  LET recha.siefore_desc = NULL
                  LET recha.monto_aceptado = 0
                  LET recha.monto_pendiente = vmonto_sub_pend
                  LET recha.monto_rechazado = vmonto_sub_rech

                  INSERT INTO exc_dep_exceso
                  VALUES (recha.*)

                  EXIT FOREACH
               END FOREACH
            END IF

            IF sie_pos = 1 AND vcodigo_siefore <> 0 THEN
               DELETE FROM exc_dep_exceso
               WHERE  folio = vfolio
               AND    ident_pago[14,15] = "71"
               AND    codigo_siefore = vcodigo_siefore
            ELSE
               IF sie_pos > 1            AND
                  vcodigo_siefore   <> 0 AND
                  sie.codigo_siefore = 1 THEN

                  DELETE FROM exc_dep_exceso
                  WHERE  folio = vfolio
                  AND    ident_pago[14,15] = "71"
                  AND    codigo_siefore = vcodigo_siefore
               END IF
            END IF            

            IF vmonto_sub_acep IS NULL THEN
               LET vmonto_sub_acep = 0
            END IF

            IF vmonto_sub_pend IS NULL THEN
               LET vmonto_sub_pend = 0
            END IF

            IF vmonto_sub_rech IS NULL THEN
               LET vmonto_sub_rech = 0
            END IF
{
            UPDATE exc_dep_exceso
            SET   monto_pendiente   = vmonto_sub_pend
                   --monto_rechazado   = vmonto_sub_rech
            WHERE  folio             = vfolio
            AND    ident_pago[14,15] = "71"
}
         WHEN "73"   -- Vivienda patron
            LET vmonto_sub_acep = sum1.monto_par_viv

            LET vmonto_sub_pend = sum3.monto_par_viv 

            LET vmonto_sub_rech = sum2.monto_par_viv 

            IF vmonto_sub_acep IS NULL THEN
               LET vmonto_sub_acep = 0
            END IF

            IF vmonto_sub_pend IS NULL THEN
               LET vmonto_sub_pend = 0
            END IF

            IF vmonto_sub_rech IS NULL THEN
               LET vmonto_sub_rech = 0
            END IF

            UPDATE exc_dep_exceso
            SET    monto_par_aceptado    = vmonto_sub_acep,
                   monto_par_pendiente   = vmonto_sub_pend,
                   monto_par_rechazado   = vmonto_sub_rech
            WHERE  folio             = vfolio
            AND    ident_pago[14,15] = "73"

         WHEN "81"   -- RCV Trabajador
            LET vmonto_sub_acep = sum1.monto_ces_vej_tra +    #d
                                  sum1.monto_comi_ces_vej +   #m
                                  sum1.monto_plus_ces_vej +   #o
                                  sum1.monto_min_ces_vej      #r

            LET vmonto_sub_pend = sum3.monto_ces_vej_tra      #d

            LET vmonto_sub_rech = sum2.monto_ces_vej_tra      #d

            IF vmonto_sub_acep IS NULL THEN
               LET vmonto_sub_acep = 0
            END IF

            IF vmonto_sub_pend IS NULL THEN
               LET vmonto_sub_pend = 0
            END IF

            IF vmonto_sub_rech IS NULL THEN
               LET vmonto_sub_rech = 0
            END IF

            UPDATE exc_dep_exceso
            SET    monto_aceptado    = vmonto_sub_acep,
                   monto_pendiente   = vmonto_sub_pend,
                   monto_rechazado   = vmonto_sub_rech
            WHERE  folio             = vfolio
            AND    ident_pago[14,15] = "81"

         WHEN "83"   -- Vivienda trabajador
            LET vmonto_sub_acep = sum1.monto_par_viv

            LET vmonto_sub_pend = sum3.monto_par_viv

            LET vmonto_sub_rech = sum2.monto_par_viv

            IF vmonto_sub_acep IS NULL THEN
               LET vmonto_sub_acep = 0
            END IF

            IF vmonto_sub_pend IS NULL THEN
               LET vmonto_sub_pend = 0
            END IF

            IF vmonto_sub_rech IS NULL THEN
               LET vmonto_sub_rech = 0
            END IF

            UPDATE exc_dep_exceso
            SET    monto_par_aceptado    = vmonto_sub_acep,
                   monto_par_pendiente   = vmonto_sub_pend,
                   monto_par_rechazado   = vmonto_sub_rech
            WHERE  folio             = vfolio
            AND    ident_pago[14,15] = "83"
      END CASE
   END FOREACH
{
   LET vmonto_tot_acep = sum1.monto_ret +
			 sum1.monto_act_ret +
			 sum1.monto_ces_vej_pat +
			 sum1.monto_ces_vej_tra +
		 	 sum1.monto_act_ces_vej

   LET vmonto_tot_plus = sum1.monto_plus_ret + sum1.monto_plus_ces_vej

   LET vmonto_tot_min  = sum1.monto_min_ret + sum1.monto_min_ces_vej

   LET vmonto_tot_comi = sum1.monto_comi_ret + sum1.monto_comi_ces_vej



   LET total_temp = vmonto_tot_acep + 
		    vmonto_tot_plus - 
		    vmonto_tot_min - 
		    vmonto_tot_comi

   SELECT SUM(monto_aceptado)
   INTO   total_temp_sie
   FROM   exc_dep_exceso
   WHERE  folio = vfolio
   AND    ident_pago[14,15] = "71"

   IF total_temp <> total_temp_sie THEN
      LET diferencia = total_temp - total_temp_sie
   END IF
}
   DECLARE cursor_3 CURSOR FOR
   SELECT r.ident_pago,
          r.monto_soli_institu,
          r.fecha_liquidacion,
          r.monto_aceptado,
          r.monto_pendiente,
          r.monto_rechazado,
          r.monto_par_solicitado,
          r.monto_par_aceptado,
          r.monto_par_pendiente,
          r.monto_par_rechazado,
	  r.siefore_desc,
          r.codigo_siefore
   FROM   exc_dep_exceso r
   WHERE  r.folio  = vfolio
   ORDER BY 1

   LET pos8 = 0

   FOREACH cursor_3 INTO g_dep.ident_pago,
                         g_dep.monto_soli_institu,
                         g_dep.fecha_liquidacion,
                         g_dep.monto_aceptado,
                         g_dep.monto_pendiente,
                         g_dep.monto_rechazado,
                         g_dep.monto_par_solicitado,
                         g_dep.monto_par_aceptado,
                         g_dep.monto_par_pendiente,
                         g_dep.monto_par_rechazado,
                         g_dep.siefore_desc,
                         g_dep.codigo_siefore
{
      IF pos8 = 0 THEN
         LET g_dep.monto_aceptado = g_dep.monto_aceptado + diferencia
      END IF
}      
      LET v_rep = g_dep.ident_pago,"|",
                  g_dep.monto_soli_institu USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.fecha_liquidacion,"|",
                  g_dep.monto_aceptado USING "&&&&&&&&&&&&&.&&","|",
                  0 USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.monto_pendiente USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.monto_rechazado USING "&&&&&&&&&&&&&.&&","|",
                  g_dep.monto_par_solicitado USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep.monto_par_aceptado USING "&&&&&&&&&&&&.&&&&&&","|",
                  0 USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep.monto_par_pendiente USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep.monto_par_rechazado USING "&&&&&&&&&&&&.&&&&&&","|",
                  g_dep.siefore_desc,"|",
                  g_dep.codigo_siefore USING "&&&","|"
display "08 ",v_rep

      LET pos8 = pos8 + 1

      OUTPUT TO REPORT r_report(v_rep,356,8)

   END FOREACH

   SELECT A.fecha_creac_lote,
          A.consecutivo_dia,
          A.num_reg_aport_lote
   INTO   g_sum.fecha_creac_lote,
          g_sum.consecutivo_dia,
          g_sum.num_reg_aport_lote 
   FROM   exc_sum_exceso A
   WHERE  A.folio  = vfolio

   LET vtotal_registros = pos2 + pos3 + pos4 + pos5 + pos8

   LET vmonto_tot_acep = sum1.monto_ret +
                         sum1.monto_act_ret +
                         sum1.monto_ces_vej_pat +
                         sum1.monto_ces_vej_tra +
                         sum1.monto_act_ces_vej 

   LET vmonto_tot_plus = sum1.monto_plus_ret + sum1.monto_plus_ces_vej

   LET vmonto_tot_min  = sum1.monto_min_ret + sum1.monto_min_ces_vej

   LET vmonto_tot_comi = sum1.monto_comi_ret + sum1.monto_comi_ces_vej

   LET v_rep = "01","|",             --tipo de entidad origen
                codigo_afore,"|",
                g_sum.fecha_creac_lote,"|",
                g_sum.consecutivo_dia,"|",
                vmonto_tot_acep USING "&&&&&&&&&&&&&.&&","|", --imp. tot ret ces y vejez 
                vmonto_tot_plus USING "&&&&&&&&&&&&&.&&","|", --imp. tot plus. ret ces y vejez
                vmonto_tot_min USING "&&&&&&&&&&&&&.&&","|",  --imp. tot mins. ret ces y vejez
                vmonto_tot_comi USING "&&&&&&&&&&&&&.&&","|", --imp. tot comi ret ces y vejez
                sum1.monto_aport_pat USING "&&&&&&&&&&&&&.&&","|", --imp. tot. vivienda 
                sum1.monto_plus_pat USING "&&&&&&&&&&&&&.&&","|",     --imp. tot. plus. vivienda
                g_sum.num_reg_aport_lote,"|",
                sum1.aceptados_tot,"|",  --num. aceptadas
                0,"|",                   --num. parcialmente
                sum2.rechazados_tot,"|", --num. rechazadas
                sum3.pendientes_tot,"|", --num. pendientes
                pos8,"|",                --num. reg dis_cuenta por pagar
                vtotal_registros,"|",     --num. total de registros
                sum1.monto_par_viv  USING "&&&&&&&&&&&&.&&&&&&","|"     --imp. tot. PARTICIPACIONES vivienda

   OUTPUT TO REPORT r_report(v_rep,356,9)

END FUNCTION

######################################################################
FUNCTION genera_tmp_cuenta (p_folio)

   DEFINE p_folio          INTEGER,
          v_nombre_tabla   CHAR(20),
          sel_his          CHAR(2000),
          tot_registros    SMALLINT,
          x_cuantos        INTEGER

   SELECT count(*)
   INTO   x_cuantos
   FROM   dis_cuenta
   WHERE  folio = p_folio

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   IF x_cuantos > 0 THEN
      LET sel_his = " SELECT a.* ",
                    " FROM   dis_cuenta a ",
                    " WHERE a.nss IN(SELECT  b.nss FROM exc_det_exceso b ",
                                   " WHERE b.folio = ",p_folio,")",
                    " AND    a.subcuenta IN (1,2,4) ",
                    " AND    a.tipo_movimiento BETWEEN 540 AND 555 ",
                    " INTO TEMP tmp_dis_cuenta " CLIPPED

      LET sel_his = sel_his CLIPPED

      PREPARE eje_sel_his FROM sel_his

      EXECUTE eje_sel_his
   ELSE
      LET sel_his = " SELECT a.* ",
                    " FROM   dis_provision a ",
                    " WHERE a.nss IN(SELECT  b.nss FROM exc_det_exceso b ",
                                   " WHERE b.folio = ",p_folio,")",
                    " AND    a.subcuenta IN (1,2,4) ",
                    " AND    a.tipo_movimiento BETWEEN 540 AND 555 ",
                    " INTO TEMP tmp_dis_cuenta " CLIPPED

      LET sel_his = sel_his CLIPPED

      PREPARE eje_sel_his1 FROM sel_his

      EXECUTE eje_sel_his1
   END IF

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(folio,
                                                  subcuenta,
                                                  tipo_movimiento
                                                 )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION
######################################################################
{
            LET vmonto_sub_acep = sum1.monto_ret +            # a
                                  sum1.monto_act_ret +        # b
                                  sum1.monto_ces_vej_pat +    # c
                                  sum1.monto_act_ces_vej +    # e
                                  sum1.monto_comi_ret +       # i
                                  sum1.monto_comi_ces_vej +   # m
                                  sum1.monto_plus_ret +       # ñ
                                  sum1.monto_plus_ces_vej +   # o
                                  sum1.monto_min_ret +        # q
                                  sum1.monto_min_ces_vej      # r

}
