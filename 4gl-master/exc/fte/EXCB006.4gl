#*********************************************************************#
#Proyecto          => Sistema de safre_af.( MEXICO )                  #
#Propietario       => E.F.P.                                          #
#Programa          => EXCB006                                         #
#Descripcion       => DEVOLUCION DE PAGOS EN EXCESO LIQUIDACION       #
#Fecha Inicio      => 06 de diciembre de 2000.                        #
#Fecha Termino     => 28 de marzo de 2001    .                        #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                 #
#Actualizado       => 19 de diciembre de 2001.                        #
#Actualizado       => 06 de junio de 2002.                            #
#Actualizado       => 19 de ogosto de 2002.                           #
#Sistema           => EXC.                                            #
#*********************************************************************#
DATABASE safre_af

GLOBALS

   DEFINE g_bat  RECORD LIKE dis_ctrl_proceso.*

   DEFINE g_cue RECORD LIKE dis_cuenta.*

   DEFINE vhora_max          CHAR(08),
          vconsecutivo         INTEGER,
          vhora_final        CHAR(08),
          vresultado         CHAR(50),
          vetapa_cod         SMALLINT,
          vproc              CHAR(06),
          vrech              CHAR(06),
          vpend              CHAR(06),
          hoy                DATE,
          hoy2               DATE,
          generar            CHAR(12),
          gusuario           CHAR(08),
          vnom_archivo       CHAR(21),
          vreporte           CHAR(200),
          vcont_rech         INTEGER,
          vcont_acep         INTEGER,
          vcont_pend         INTEGER,
          vfecha_lote        CHAR(08),
          vtipo_reporte      CHAR(01),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          cla_sel            CHAR(500)

   DEFINE vfolio             INTEGER

   DEFINE gparam_dev  RECORD LIKE seg_modulo.*

   DEFINE opc                CHAR(1),
          vparametro1        CHAR(3),
          vparametro2        INTEGER

   DEFINE generar1           DATE,
          generar2           CHAR(10),
          generar3           INTEGER

################


   DEFINE g_reg  RECORD LIKE exc_det_exceso.*

   DEFINE det  RECORD
          monto_ret          DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_ces_vej_pat  DECIMAL(16,6),
          monto_ces_vej_tra  DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_cuo_soc      DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE g_dep  RECORD
          ident_pago         CHAR(2),
          monto_soli_institu DECIMAL(16,6),
          monto_aceptado     DECIMAL(16,6),
          monto_parcial      DECIMAL(16,6),
          monto_pendiente    DECIMAL(16,6),
          monto_rechazado    DECIMAL(16,6),
          monto_par_solicitado   DECIMAL(18,6),
          monto_par_aceptado     DECIMAL(18,6),
          monto_par_parcial      DECIMAL(18,6),
          monto_par_pendiente    DECIMAL(18,6),
          monto_par_rechazado    DECIMAL(18,6),
          codigo_siefore         SMALLINT
   END RECORD

   DEFINE dep  RECORD
          monto_soli_institu DECIMAL(16,6),
          monto_aceptado     DECIMAL(16,6),
          monto_parcial      DECIMAL(16,6),
          monto_pendiente    DECIMAL(16,6),
          monto_rechazado    DECIMAL(16,6),
          monto_par_solicitado   DECIMAL(18,6),
          monto_par_aceptado     DECIMAL(18,6),
          monto_par_parcial      DECIMAL(18,6),
          monto_par_pendiente    DECIMAL(18,6),
          monto_par_rechazado    DECIMAL(18,6)
   END RECORD

   DEFINE g_sum  RECORD
          monto_tot_rcv      DECIMAL(16,6),
          monto_tot_plu_rcv  DECIMAL(16,6),
          monto_tot_min_rcv  DECIMAL(16,6),
          monto_tot_com_rcv  DECIMAL(16,6),
          monto_tot_pat      DECIMAL(16,6),
          monto_tot_gub      DECIMAL(16,6),
          monto_tot_plu_gub  DECIMAL(16,6),
          monto_tot_min_gub  DECIMAL(16,6),
          monto_tot_com_gub  DECIMAL(16,6),
          monto_tot_plu_pat  DECIMAL(16,6),
          monto_tot_par_viv  DECIMAL(18,6)
   END RECORD

   DEFINE d_sum  RECORD
          monto_tot_rcv      DECIMAL(16,6),
          monto_tot_plu_rcv  DECIMAL(16,6),
          monto_tot_min_rcv  DECIMAL(16,6),
          monto_tot_com_rcv  DECIMAL(16,6),
          monto_tot_pat      DECIMAL(16,6),
          monto_tot_gub      DECIMAL(16,6),
          monto_tot_plu_gub  DECIMAL(16,6),
          monto_tot_min_gub  DECIMAL(16,6),
          monto_tot_com_gub  DECIMAL(16,6),
          monto_tot_plu_pat  DECIMAL(16,6),
          monto_tot_par_viv  DECIMAL(18,6)
   END RECORD

   DEFINE neto  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi2  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE tot  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_cuota_soc    DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE tot_1  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE total  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE vces_vej           DECIMAL(16,2),
          rval_porcentaje    DECIMAL(16,6),
          precio_acc         DECIMAL(16,6),
          xxfecha_liqui      DATE,
          precio_dia         DECIMAL(16,6),
          vtipo_registro     CHAR(2)

   DEFINE vfecha_aplica      DATE,
          vtasa_aplica_viv   DECIMAL(16,6)

   DEFINE vaport_pat         CHAR(01)


   DEFINE monto_ret_acc      DECIMAL(16,6),
          impt_ret_acc       DECIMAL(16,6),
          comi_impt_ret_acc  DECIMAL(16,6),
          comi2_impt_ret_acc DECIMAL(16,6),
          tot_monto_ret_acc  DECIMAL(16,6)

   DEFINE vces_vej_acc           DECIMAL(16,6),
          impt_ces_vej_acc       DECIMAL(16,6),
          comi_impt_ces_vej_acc  DECIMAL(16,6),
          comi2_impt_ces_vej_acc DECIMAL(16,6),
          tot_monto_ces_vej_acc  DECIMAL(16,6)

   DEFINE dep_ident_pago        CHAR(2)

   DEFINE total_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc_acla RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE log1                  CHAR(40)

   DEFINE ejecuta_procedure     CHAR(500),
          ejecuta_marca         CHAR(200),
          ejecuta_desmarca      CHAR(200),
          ejecuta_fn_mayor      CHAR(200),
          ejecuta_fn_paga       CHAR(200),
          ejecuta_fn_menor      CHAR(200) -- sie_dos

   DEFINE hay_registros         SMALLINT

################
END GLOBALS
#*********************************************************************
MAIN

   OPTIONS 
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET vparametro1 = ARG_VAL(1)
   LET vparametro2 = ARG_VAL(2)

   CALL inicializa()

   CALL Proceso()

END MAIN
#*********************************************************************
FUNCTION inicializa()

   LET hoy = TODAY

   SELECT *,
          USER
   INTO   gparam_dev.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET ejecuta_procedure = "EXECUTE PROCEDURE fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE clausula_sql3 FROM ejecuta_procedure

   LET ejecuta_fn_mayor = "EXECUTE PROCEDURE fn_ap_exc_mayores (?,?,?,?)" --- sie_dos
   PREPARE cla_fn_mayor FROM ejecuta_fn_mayor  -- sie_dos

   LET ejecuta_fn_menor = "EXECUTE PROCEDURE fn_ap_exc_menores (?,?,?,?)" --- sie_dos
   PREPARE cla_fn_menor FROM ejecuta_fn_menor  -- sie_dos

   LET ejecuta_fn_paga = "EXECUTE PROCEDURE fn_paga_exc (?,?,?)" --- sie_dos
   PREPARE cla_fn_paga FROM ejecuta_fn_paga  -- sie_dos

END FUNCTION
#*********************************************************************
FUNCTION Proceso()

   LET hoy = TODAY

   IF vparametro1 = "RCV" THEN
      LET cla_sel = " SELECT * ",
                    " FROM   dis_ctrl_proceso ",
                    " WHERE  folio = ",vparametro2,
                    " AND    proceso_cod = 'EXC' ",
                    " AND    etapa_cod = 5 "        -- ETAPA 5

      PREPARE claexe2 FROM cla_sel

      DECLARE cur_proceso2 CURSOR FOR claexe2

      OPEN cur_proceso2
         FETCH cur_proceso2 INTO g_bat.*
      CLOSE cur_proceso2

      LET generar1 = g_bat.parametro1 CLIPPED
      LET generar2 = g_bat.parametro2 CLIPPED
      LET generar3 = g_bat.folio CLIPPED

      CALL Proceso_principal(generar1,
                             generar2,
                             generar3)
           RETURNING vfolio

      LET vhora_final = TIME

      LET vresultado   = "Terminada liquidacion de registros de RCV "

      CALL Actualiza_etapa(vfolio,5,vresultado)

      ERROR "PROCESO TERMINADO DE PAGOS EN EXCESO"
   ELSE
      LET cla_sel = " SELECT * ",
                    " FROM   dis_ctrl_proceso ",
                    " WHERE  folio = ",vparametro2,
                    " AND    proceso_cod = 'EXC' ",
                    " AND    etapa_cod = 7 "        -- ETAPA 7

      PREPARE claexe3 FROM cla_sel

      DECLARE cur_proceso3 CURSOR FOR claexe3

      OPEN cur_proceso3
         FETCH cur_proceso3 INTO g_bat.*
      CLOSE cur_proceso3

      LET generar1 = g_bat.parametro1 CLIPPED
      LET generar2 = g_bat.parametro2 CLIPPED
      LET generar3 = g_bat.folio CLIPPED

      CALL Proceso_principal(generar1,
                             generar2,
                             generar3)
           RETURNING vfolio

      LET vhora_final = TIME

      LET vresultado   = "Terminada liquidacion de registros de VIVIENDA"

      CALL Actualiza_etapa(vfolio,7,vresultado)

      ERROR "PROCESO TERMINADO DE PAGOS EN EXCESO"
   END IF

END FUNCTION
#*********************************************************************
FUNCTION Proceso_principal(x_fecha,
                           x_origen,
                           x_folio)

   DEFINE x_fecha                DATE,
          x_origen               CHAR(10),
          x_folio                INTEGER,
          x_precio_acc_dia       DECIMAL(16,6),
          x_monto_en_pesos       DECIMAL(16,6),
          x_fecha_valor          DATE

   DEFINE x_nss                  CHAR(11),
          x_consec_reg_lote      INTEGER,
          x_subcuenta            SMALLINT,
          x_tipo_movimiento      SMALLINT,
	  x_siefore              SMALLINT,
          x_monto_en_acciones    DECIMAL(16,6),
          x_total_plus_ret       DECIMAL(16,6),
          x_total_plus_ces       DECIMAL(16,6)

   DEFINE cla_sel                CHAR(200),
          vfolio                 INTEGER

   DEFINE x_acc_plus_ret,
          x_acc_plus_ces_vej,
          x_precio_accion_ini,
          ini_monto_plus_ret,
          ini_monto_plus_ces,
          fin_monto_plus_ret,
          fin_monto_plus_ces,
          total_plus_ret,
          total_plus_ces     DECIMAL(16,6)

   DEFINE g_reg RECORD LIKE dis_cuenta.*

   DEFINE hoy2                   DATE,
          x_fecha_conversion     DATE

   DEFINE vmarca_cod             SMALLINT,
          vrechazo_cod           SMALLINT,
          ejecuta_procedure      CHAR(200),
          x_status               SMALLINT

   DEFINE opc                    CHAR(1)

   LET hoy2 = TODAY

   LET x_origen = x_origen CLIPPED

   IF x_origen = "RCV" THEN

      SELECT count(*)
      INTO   hay_registros
      FROM   dis_provision
      WHERE  folio = x_folio
      AND    subcuenta IN (1,2)
      AND    fecha_conversion = hoy2 

      IF hay_registros = 0 THEN
         CALL validacion(x_folio)
      END IF

      LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                    " FROM   dis_provision ",
                    " WHERE  folio = ",x_folio CLIPPED,
                    " AND    subcuenta IN (1,2) ",
                    " AND    estado = 6 ",
		    " GROUP BY 1,2,3,4 "
		   

      PREPARE claexe5 FROM cla_sel

      DECLARE cur_proceso5 CURSOR FOR claexe5

      FOREACH cur_proceso5 INTO g_cue.folio,
				g_cue.nss,
				g_cue.consecutivo_lote,
				g_cue.subcuenta
         LET x_fecha_valor = x_fecha

         LET x_fecha_conversion = x_fecha

   LET ejecuta_procedure = "EXECUTE PROCEDURE fn_liquida_exc (?,?,?,?,?,?)"

   PREPARE clausula_sql2 FROM ejecuta_procedure

   DECLARE cursor_liquida1 CURSOR FOR clausula_sql2

   OPEN cursor_liquida1 USING g_cue.folio,
                              g_cue.nss,
                              g_cue.consecutivo_lote,
                              g_cue.subcuenta,
                              x_fecha_conversion,
                              x_fecha_conversion


      FETCH cursor_liquida1 INTO x_status

   CLOSE cursor_liquida1

         CALL des_marca(g_cue.nss,                 -- nss
                        540,                       -- marca_entra
                        g_cue.consecutivo_lote,    -- correlativo
                        0,                         -- estado_marca
                        0,                         -- marca_causa
                        gusuario                   -- usuario
                       )

      END FOREACH
   ELSE
      LET cla_sel = " SELECT folio,nss,consecutivo_lote,subcuenta ",
                    " FROM   dis_provision ",
                    " WHERE  folio = ",x_folio CLIPPED,
                    " AND    subcuenta = 4 ",
                    " AND    estado = 6 ",
		    " GROUP BY 1,2,3,4"
		   

      PREPARE claexe9 FROM cla_sel

      DECLARE cur_proceso9 CURSOR FOR claexe9

      FOREACH cur_proceso9 INTO g_cue.folio,
				g_cue.nss,
				g_cue.consecutivo_lote,
				g_cue.subcuenta

         LET x_fecha_valor = hoy2

         LET x_fecha_conversion = hoy2

  LET ejecuta_procedure = "EXECUTE PROCEDURE fn_liquida_exc (?,?,?,?,?,?)"

   PREPARE clausula_sql31 FROM ejecuta_procedure

   DECLARE cursor_liquida31 CURSOR FOR clausula_sql31

   OPEN cursor_liquida31 USING g_cue.folio,
                              g_cue.nss,
                              g_cue.consecutivo_lote,
                              g_cue.subcuenta,
                              x_fecha_valor,
                              x_fecha_conversion


      FETCH cursor_liquida31 INTO x_status
   CLOSE cursor_liquida31

         CALL des_marca(g_cue.nss,                 -- nss
                        542,                       -- marca_entra
                        g_cue.consecutivo_lote,    -- correlativo
                        0,                         -- estado_marca
                        0,                         -- marca_causa
                        gusuario                   -- usuario
                       )

      END FOREACH

   END IF
   RETURN x_folio

END FUNCTION
#*********************************************************************
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50)

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod = 'EXC' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe6 FROM cla_sel

   DECLARE cur_proceso6 CURSOR FOR claexe6

   OPEN cur_proceso6
      FETCH cur_proceso6 INTO vconsecutivo
   CLOSE cur_proceso6

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'EXC'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo  = ",vconsecutivo CLIPPED

   PREPARE claexe7 FROM cla_sel

   EXECUTE claexe7

END FUNCTION
#******************************************************************************
FUNCTION des_marca(x_nss,
                   x_marca_entra,
                   x_consecutivo_lote,
                   x_marca_estado,
                   x_marca_causa,
                   x_usuario )

   DEFINE x_nss             CHAR(11),
          x_operacion       CHAR(1),
          x_marca_entra     SMALLINT,
          x_consecutivo_lote     SMALLINT,
          x_marca_estado    SMALLINT,
          x_marca_causa     SMALLINT,
          x_codigo_rechazo  SMALLINT,
          x_usuario         CHAR(8),
          xx_codigo_marca   SMALLINT,
          xx_codigo_rechazo SMALLINT,
          ejecuta_procedure CHAR(200)

   LET ejecuta_procedure = "EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

   PREPARE clausula_sql FROM ejecuta_procedure

   EXECUTE clausula_sql USING x_nss,
                               x_marca_entra,
                               x_consecutivo_lote,
                               x_marca_estado,
                               x_marca_causa,
                               x_usuario

END FUNCTION
###############################################################################
FUNCTION validacion(vfolio)      ---ETAPA 4

   DEFINE vfolio                INTEGER,
          vestado_proceso       SMALLINT,
          tipo_rech             CHAR(03),
          auxfecha_pago         CHAR(10),
          auxfecha_pago1        CHAR(8),
          vfentcons             DATE,
          vfinitmte             DATE,
          vfecha_pri_general    DATE,
          mig_marca_entra       SMALLINT,
          mig_codigo_rechazo    SMALLINT

   DEFINE v  RECORD
          folio                 INTEGER,
          consec_reg_lote       INTEGER,
          fecha_recepcion       DATE,
          impt_ret              DECIMAL(16,6),
          impt_ces_vej          DECIMAL(16,6),
          impt_act_rec_ret      DECIMAL(16,6),
          impt_act_r_ces_vej    DECIMAL(16,6),
          impt_cuota_soc        DECIMAL(16,6),
          impt_aport_est        DECIMAL(16,6),
          impt_aport_esp        DECIMAL(16,6),
          impt_aport_pat        DECIMAL(16,6)
   END RECORD

   DEFINE vtipo_solicitud       SMALLINT

   DEFINE vces_vej              DECIMAL(16,6)

   DEFINE aux_monto_ces_vej_pat DECIMAL(16,6),
          aux_monto_ces_vej_tra DECIMAL(16,6),
          aux_monto_aport_pat   DECIMAL(16,6)

   DEFINE opc                   CHAR(1)
   DEFINE dep_ident_pago        CHAR(2)

   DEFINE mar_activo_marca      SMALLINT,
          mar_fecha_act_marca   DATE,
          mar_marca_cod         SMALLINT

   DEFINE x_afore_local         SMALLINT,
          uni_sw                SMALLINT

   DEFINE x_ident_viv_garantia  CHAR(1)
   DEFINE x_credito_garantia    CHAR(1)
   DEFINE vvfentcons            DATE

   DEFINE x_saldo_acc        DECIMAL(18,6),
          x_saldo_pes        DECIMAL(18,6)

   DISPLAY "VALIDACION Y PROVISION,ETAPA 4"

   DELETE FROM exc_exceso_plu_min
   WHERE  folio = vfolio

   DELETE FROM dis_provision
   WHERE  folio = vfolio
   AND    subcuenta <> 4

   SELECT codigo_afore
   INTO   x_afore_local
   FROM   tab_afore_local
   GROUP BY 1

   DECLARE cursor_exc CURSOR FOR
   SELECT *
   FROM   exc_det_exceso
   WHERE  folio = vfolio
   AND    result_operacion = "01"
   AND    clave_ent_orig = "001"

   FOREACH cursor_exc INTO g_reg.*

      CALL genera_tmp_cuenta (g_reg.nss)

      CALL saldo_al_dia(g_reg.nss,
                        0,
                        0)
           RETURNING x_saldo_acc,
                     x_saldo_pes

      LET x_saldo_acc = 0
      LET x_saldo_pes = 0

      IF g_reg.clave_ent_orig = "001" THEN
         LET mig_marca_entra = 540
      ELSE
         LET mig_marca_entra = 542
      END IF

      LET auxfecha_pago = g_reg.fecha_pago

      LET auxfecha_pago1 = auxfecha_pago[7,10],
                           auxfecha_pago[1,2],
                           auxfecha_pago[4,5]

      SELECT folio,
             consec_reg_lote,
             fecha_recepcion,
             impt_ret/100,
             impt_ces_vej/100,
             impt_act_rec_ret/100,
             impt_act_r_ces_vej/100,
             impt_cuota_soc/100,
             impt_aport_est/100,
             impt_aport_esp/100,
             impt_aport_pat/100,
             ident_viv_garantia
      INTO   v.folio,
             v.consec_reg_lote,
             v.fecha_recepcion,
             v.impt_ret,
             v.impt_ces_vej,
             v.impt_act_rec_ret,
             v.impt_act_r_ces_vej,
             v.impt_cuota_soc,
             v.impt_aport_est,
             v.impt_aport_esp,
             v.impt_aport_pat,
             x_ident_viv_garantia
      FROM   dis_det_aporte a
      WHERE  a.n_seguro          = g_reg.nss
      AND    a.periodo_pago      = g_reg.periodo_pago
      AND    a.fech_pago         = auxfecha_pago1
      AND    a.folio_pago_sua    = g_reg.folio_pago_sua
      AND    a.reg_patronal_imss = g_reg.reg_patronal_imss
      AND    a.cve_ent_receptora = g_reg.clave_ent_recep
      AND    a.folio NOT IN(SELECT b.folio
                            FROM   safre_tmp:tmp_folios_pgo13 b)
      AND    a.result_operacion[2] = "1"
      GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12

      IF SQLCA.SQLCODE = 0 THEN
          CALL actualiza_control(g_reg.*,
                                 v.*)
      END IF
   END FOREACH

END FUNCTION
#*********************************************************************
FUNCTION actualiza_control(reg_1,r)

   DEFINE reg_1  RECORD LIKE exc_det_exceso.* 

   DEFINE vfentcons          DATE,
          vfinitmte          DATE,
          vfecha_pri_general DATE

   DEFINE r  RECORD
          folio              INTEGER,
          consec_reg_lote    INTEGER,
          fecha_recepcion    DATE,
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6),
          impt_act_rec_ret   DECIMAL(16,6),
          impt_act_r_ces_vej DECIMAL(16,6),
          impt_cuota_soc     DECIMAL(16,6),
          impt_aport_est     DECIMAL(16,6),
          impt_aport_esp     DECIMAL(16,6),
          impt_aport_pat     DECIMAL(16,6)
   END RECORD 

   DEFINE neto  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE comi2  RECORD
          impt_ret           DECIMAL(16,6),
          impt_ces_vej       DECIMAL(16,6)
   END RECORD

   DEFINE tot  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_cuota_soc    DECIMAL(16,6),
          monto_aport_est    DECIMAL(16,6),
          monto_aport_esp    DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE tot_1  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6),
          monto_par_viv      DECIMAL(18,6)
   END RECORD

   DEFINE acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE total  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE vces_vej           DECIMAL(16,6),
          rval_porcentaje    DECIMAL(16,6),
          precio_acc         DECIMAL(16,6),
          xxfecha_liqui      DATE,
          precio_dia         DECIMAL(16,6),
          vtipo_registro     CHAR(2),
          hoy                DATE,
          opc                CHAR(1)

   DEFINE vfecha_aplica      DATE,
          vtasa_aplica_viv   DECIMAL(16,6)

   DEFINE tot_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_1_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE tot_2_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE acc_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE porce  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6),
          monto_aport_pat    DECIMAL(16,6)
   END RECORD

   DEFINE aclafecha_pago     CHAR(10),
          aclafecha_pago1    CHAR(8)

   DEFINE acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6),
          monto_act_ret      DECIMAL(16,6),
          monto_act_ces_vej  DECIMAL(16,6)
   END RECORD

   DEFINE monto_ret_acc      DECIMAL(16,6),
          impt_ret_acc       DECIMAL(16,6),
          comi_impt_ret_acc  DECIMAL(16,6),
          comi2_impt_ret_acc DECIMAL(16,6),
          tot_monto_ret_acc  DECIMAL(16,6)

   DEFINE vces_vej_acc           DECIMAL(16,6),
          impt_ces_vej_acc       DECIMAL(16,6),
          comi_impt_ces_vej_acc  DECIMAL(16,6),
          comi2_impt_ces_vej_acc DECIMAL(16,6),
          tot_monto_ces_vej_acc  DECIMAL(16,6)

   DEFINE total_acla  RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE total_acc_acla RECORD
          monto_ret          DECIMAL(16,6),
          monto_ces_vej      DECIMAL(16,6)
   END RECORD

   DEFINE x_saldo_acc        DECIMAL(18,6),
          x_saldo_pes        DECIMAL(18,6)

   DEFINE x_ind_transferencia SMALLINT,
          v_subcuenta         SMALLINT,
          v_siefore           SMALLINT,
          v_tipo_movimiento   SMALLINT,
          v_fecha_conversion  DATE,
          v_precio_accion     DECIMAL(16,6),
          v_monto_acc         DECIMAL(16,6),
          v_monto_pesos       DECIMAL(16,6),
          v_acciones          DECIMAL(16,6),
          v_pesos             DECIMAL(16,6),
          x_movimiento        SMALLINT,
          r_subcuenta         SMALLINT,
          r_siefore           SMALLINT,
          r_acciones          DECIMAL(16,6),
          r_pesos             DECIMAL(16,6),
          n_pesos             DECIMAL(16,6)

   DEFINE ban_ret             SMALLINT,
	  ban_act_ret         SMALLINT,
  	  ban_ces_vej         SMALLINT,
	  ban_act_ces_vej     SMALLINT

   ---Montos de retiro a devolver-----------------

   LET aclafecha_pago = reg_1.fecha_pago

   LET aclafecha_pago1 = aclafecha_pago[7,10],
                         aclafecha_pago[1,2],
                         aclafecha_pago[4,5]

---- SIE_DOS INI
   LET hoy = TODAY


IF reg_1.clave_ent_orig = "001" THEN
   SELECT ind_transferencia
   INTO   x_ind_transferencia
   FROM   cta_ctr_cuenta
   WHERE  nss = reg_1.nss

   IF x_ind_transferencia = 1 THEN
      LET ban_ret         = 0
      LET ban_act_ret     = 0
      LET ban_ces_vej = 0
      LET ban_act_ces_vej = 0

display "mayor"
display "reg_1.nss:",reg_1.nss
display "r.folio:",r.folio
display "r.consec_reg_lote:",r.consec_reg_lote
display "hoy:",hoy

      DECLARE cur_fn_mayor CURSOR FOR  cla_fn_mayor 
      FOREACH cur_fn_mayor USING reg_1.nss,
                                 r.folio,
                                 r.consec_reg_lote,
                                 hoy
                            INTO v_subcuenta,
                                 v_siefore,
                                 v_tipo_movimiento,
                                 v_fecha_conversion,
                                 v_precio_accion,
                                 v_monto_acc,
                                 v_monto_pesos,
                                 v_acciones,
                                 v_pesos


display ""
display "v_subcuenta:",v_subcuenta
display "v_siefore:",v_siefore
display "v_tipo_movimiento:",v_tipo_movimiento
display "v_fecha_conversion:",v_fecha_conversion
display "v_precio_accion:",v_precio_accion
display "v_monto_acc:",v_monto_acc
display "v_monto_pesos:",v_monto_pesos
display "v_acciones:",v_acciones
display "v_pesos:",v_pesos

         IF v_subcuenta = 1 THEN
            CASE 
              WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ret > 0 THEN
		    LET porce.monto_ret = reg_1.monto_ret * 100/r.impt_ret

display "porce"
display reg_1.monto_ret
display r.impt_ret
display porce.monto_ret

                 --   LET tot_2.monto_ret = (v_pesos - v_monto_pesos) * porce.monto_ret/100
                    LET acc.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                    LET ban_ret = 1

display tot_2.monto_ret
display acc.monto_ret
display v_pesos
display v_acciones


                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ret > 0 THEN
		    LET porce.monto_act_ret = reg_1.monto_act_ret * 100/r.impt_act_rec_ret
                    --LET tot_2.monto_act_ret = (v_pesos - v_monto_pesos) * porce.monto_act_ret/100
                    LET acc.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                    LET ban_act_ret = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF

              WHEN v_tipo_movimiento = 3
                 IF ban_ret = 1 THEN
                    LET tot_1_acla.monto_ret = v_pesos * porce.monto_ret/100
                    LET acc_acla.monto_ret   = v_acciones  * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ret = 1 THEN
                    LET tot_1_acla.monto_act_ret = v_pesos  * porce.monto_act_ret/100
                    LET acc_acla.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ret = 1 THEN
                    LET comi2.impt_ret = (v_monto_pesos * -1) * porce.monto_ret/100
display "comi2.impt_ret:",comi2.impt_ret
                 ELSE
                    CONTINUE FOREACH
                 END IF
            END CASE
         ELSE
            CASE
               WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ces_vej_pat > 0 THEN
		    LET porce.monto_ces_vej = reg_1.monto_ces_vej_pat * 100/r.impt_ces_vej
display "porce"
display reg_1.monto_ces_vej_pat
display r.impt_ces_vej
display porce.monto_ces_vej
                    --LET tot_2.monto_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_ces_vej/100
                    LET acc.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                    LET ban_ces_vej = 1
display tot_2.monto_ces_vej
display acc.monto_ces_vej
display v_pesos
display v_acciones
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ces_vej > 0 THEN
		    LET porce.monto_act_ces_vej = reg_1.monto_act_ces_vej * 100/r.impt_act_r_ces_vej
                    --LET tot_2.monto_act_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_act_ces_vej/100
                    LET acc.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                    LET ban_act_ces_vej = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF

              WHEN v_tipo_movimiento = 3 
                 IF ban_ces_vej = 1 THEN
                    LET tot_1_acla.monto_ces_vej = v_pesos * porce.monto_ces_vej/100
                    LET acc_acla.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ces_vej = 1 THEN
                    LET tot_1_acla.monto_act_ces_vej = v_pesos * porce.monto_act_ces_vej/100
                    LET acc_acla.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ces_vej = 1 THEN
                    LET comi2.impt_ces_vej = (v_monto_pesos * -1) * porce.monto_ces_vej/100
display "comi2.impt_ces_vej:",comi2.impt_ces_vej
                 ELSE
                    CONTINUE FOREACH
                 END IF
            END CASE
         END IF

         IF v_tipo_movimiento < 100 THEN
            CASE v_tipo_movimiento
               WHEN 1 LET x_movimiento = 540
               WHEN 2 LET x_movimiento = 545
               WHEN 3 LET x_movimiento = 550
               WHEN 4 LET x_movimiento = 555
            END CASE

display "PAGA"
display "v_subcuenta:",v_subcuenta
display "v_siefore:",v_siefore
display "v_acciones:",v_acciones

            DECLARE cur_fn_paga CURSOR FOR  cla_fn_paga 
            LET n_pesos = 0
            FOREACH cur_fn_paga USING v_subcuenta,
                                      v_siefore,
                                      v_acciones
                                 INTO r_subcuenta,
                                      r_siefore,
                                      r_acciones,
                                      r_pesos

               LET precio_dia =  precio_acc_dia (r_siefore,hoy)

display "r_subcuenta:",r_subcuenta
display "r_siefore:",r_siefore
display "r_acciones:",r_acciones
display "r_pesos:",r_pesos
display "precio_dia:",precio_dia

--prompt "entro" for opc
IF r_acciones > 0 THEN
               CALL provisiona_cuenta(r_pesos,
                                      r_acciones,
                                      reg_1.*,
                                      r_subcuenta,
                                      x_movimiento,
                                      precio_dia,
                                      hoy,
                                      r_siefore)

            LET n_pesos = n_pesos + r_pesos
END IF
            END FOREACH
         END IF

display "n_pesos:", n_pesos
         IF v_subcuenta = 1 THEN
            CASE 
              WHEN v_tipo_movimiento = 1
                    --LET tot_2.monto_ret = (n_pesos - v_monto_pesos) * porce.monto_ret/100
                    LET tot_2.monto_ret = n_pesos - (v_monto_pesos * porce.monto_ret/100)
              WHEN v_tipo_movimiento = 2
                    --LET tot_2.monto_act_ret = (n_pesos - v_monto_pesos) * porce.monto_act_ret/100
                    LET tot_2.monto_act_ret = n_pesos - (v_monto_pesos * porce.monto_act_ret/100)
            END CASE
         ELSE
            CASE
               WHEN v_tipo_movimiento = 1
                    --LET tot_2.monto_ces_vej = (n_pesos - v_monto_pesos) * porce.monto_ces_vej/100
                    LET tot_2.monto_ces_vej = n_pesos - (v_monto_pesos * porce.monto_ces_vej/100)
               WHEN v_tipo_movimiento = 2
                    --LET tot_2.monto_act_ces_vej = (n_pesos - v_monto_pesos) * porce.monto_act_ces_vej/100
                    LET tot_2.monto_act_ces_vej = n_pesos - (v_monto_pesos * porce.monto_act_ces_vej/100)
            END CASE
         END IF

display "tot_2.monto_ret:",tot_2.monto_ret
display "tot_2.monto_ces_vej:",tot_2.monto_ces_vej
      END FOREACH
   ELSE
      LET ban_ret         = 0
      LET ban_act_ret     = 0
      LET ban_ces_vej = 0
      LET ban_act_ces_vej = 0

      DECLARE cur_fn_menor CURSOR FOR  cla_fn_menor 
      FOREACH cur_fn_menor USING reg_1.nss,
                                 r.folio,
                                 r.consec_reg_lote,
                                 hoy
                            INTO v_subcuenta,
                                 v_siefore,
                                 v_tipo_movimiento,
                                 v_fecha_conversion,
                                 v_precio_accion,
                                 v_monto_acc,
                                 v_monto_pesos,
                                 v_acciones,
                                 v_pesos

{
display v_subcuenta
display v_siefore
display v_tipo_movimiento
display v_fecha_conversion
display v_monto_acc
display v_monto_pesos
display v_acciones
display v_pesos
}
         IF v_subcuenta = 1 THEN
            CASE 
              WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ret > 0 THEN
		    LET porce.monto_ret = reg_1.monto_ret * 100/r.impt_ret
                    LET tot_2.monto_ret = (v_pesos - v_monto_pesos) * porce.monto_ret/100
                    LET acc.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                    LET ban_ret = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ret > 0 THEN
		    LET porce.monto_act_ret = reg_1.monto_act_ret * 100/r.impt_act_rec_ret
                    LET tot_2.monto_act_ret = (v_pesos - v_monto_pesos) * porce.monto_act_ret/100
                    LET acc.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                    LET ban_act_ret = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 3
                 IF ban_ret = 1 THEN
                    LET tot_1_acla.monto_ret = v_pesos * porce.monto_ret/100
                    LET acc_acla.monto_ret   = v_acciones * porce.monto_ret/100
		    LET v_pesos = v_pesos  * porce.monto_ret/100
		    LET v_acciones = v_acciones  * porce.monto_ret/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento = 4
                 IF ban_act_ret = 1 THEN
                    LET tot_1_acla.monto_act_ret = v_pesos * porce.monto_act_ret/100
                    LET acc_acla.monto_act_ret   = v_acciones * porce.monto_act_ret/100
		    LET v_pesos = v_pesos  * porce.monto_act_ret/100
		    LET v_acciones = v_acciones  * porce.monto_act_ret/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ret = 1 THEN
                    LET comi2.impt_ret = (v_monto_pesos * -1)* porce.monto_ret/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
            END CASE
         ELSE
            CASE
               WHEN v_tipo_movimiento = 1
                 IF reg_1.monto_ces_vej_pat > 0 THEN
		    LET porce.monto_ces_vej = reg_1.monto_ces_vej_pat * 100/r.impt_ces_vej
                    LET tot_2.monto_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_ces_vej/100
                    LET acc.monto_ces_vej   = v_acciones * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                    LET ban_ces_vej = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF

              WHEN v_tipo_movimiento = 2
                 IF reg_1.monto_act_ces_vej > 0 THEN
		    LET porce.monto_act_ces_vej = reg_1.monto_act_ces_vej * 100/r.impt_act_r_ces_vej
                    LET tot_2.monto_act_ces_vej = (v_pesos - v_monto_pesos) * porce.monto_act_ces_vej/100
                    LET acc.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                    LET ban_act_ces_vej = 1
                 ELSE
                    CONTINUE FOREACH
                 END IF

              WHEN v_tipo_movimiento = 3 
                 IF ban_ces_vej = 1 THEN
                    LET tot_1_acla.monto_ces_vej = v_pesos  * porce.monto_ces_vej/100
                    LET acc_acla.monto_ces_vej   = v_acciones  * porce.monto_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_ces_vej/100
                 ELSE
                    CONTINUE FOREACH
                 END IF

              WHEN v_tipo_movimiento = 4
                 IF ban_act_ces_vej = 1 THEN
                    LET tot_1_acla.monto_act_ces_vej = v_pesos * porce.monto_act_ces_vej/100
                    LET acc_acla.monto_act_ces_vej   = v_acciones * porce.monto_act_ces_vej/100
		    LET v_pesos = v_pesos  * porce.monto_act_ces_vej/100
		    LET v_acciones = v_acciones  * porce.monto_act_ces_vej/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
              WHEN v_tipo_movimiento >= 100
                 IF ban_ces_vej = 1 THEN
                    LET comi2.impt_ces_vej = (v_monto_pesos * -1)* porce.monto_ces_vej/100
                 ELSE
                    CONTINUE FOREACH
                 END IF
            END CASE
         END IF

         IF v_tipo_movimiento < 100 THEN
            CASE v_tipo_movimiento
               WHEN 1 LET x_movimiento = 540
               WHEN 2 LET x_movimiento = 545
               WHEN 3 LET x_movimiento = 550
               WHEN 4 LET x_movimiento = 555
            END CASE

            LET precio_dia = precio_acc_dia (v_siefore,hoy)

if v_acciones < 0 THEN
display "ACCIONES NEGATIVAS"
display reg_1.nss
display v_pesos
display v_acciones
display v_subcuenta
display x_movimiento
display precio_dia
display hoy
display v_siefore
END IF

IF v_acciones > 0 THEN
            CALL provisiona_cuenta(v_pesos,
                                   v_acciones,
                                   reg_1.*,
                                   v_subcuenta,
                                   x_movimiento,
                                   precio_dia,
                                   hoy,
                                   v_siefore)

END IF
         END IF
      END FOREACH
   END IF
ELSE
   IF reg_1.monto_par_viv > 0 THEN

      LET hoy = TODAY
      LET precio_acc = precio_acc_dia (11,hoy)

      LET  tot.monto_aport_pat = reg_1.monto_par_viv * precio_acc

      LET tot_2.monto_aport_pat = tot.monto_aport_pat

      LET tot_2.monto_par_viv = reg_1.monto_par_viv

      CALL provisiona_cuenta(tot.monto_aport_pat,
                             tot_2.monto_par_viv,
                             reg_1.*,
                             4,
                             540,
                             0,
                             hoy,
                             11)
   END IF
END IF
---- SIE_DOS FIN

   IF vaport_pat = "N" THEN
      IF tot_1.monto_ces_vej IS NULL THEN
         LET tot_1.monto_ces_vej = 0
      END IF

      IF tot_1.monto_act_ces_vej IS NULL THEN
         LET tot_1.monto_act_ces_vej = 0 
      END IF

      IF tot_1_acla.monto_ces_vej IS NULL THEN
         LET tot_1_acla.monto_ces_vej = 0
      END IF

      IF tot_1_acla.monto_act_ces_vej IS NULL THEN
         LET tot_1_acla.monto_act_ces_vej = 0
      END IF

      IF acc.monto_ces_vej IS NULL THEN
         LET acc.monto_ces_vej = 0
      END IF

      IF acc.monto_act_ces_vej IS NULL THEN
         LET acc.monto_act_ces_vej = 0 
      END IF

      IF acc_acla.monto_ces_vej IS NULL THEN
         LET tot_1_acla.monto_ces_vej = 0
      END IF

      IF acc_acla.monto_act_ces_vej IS NULL THEN
         LET tot_1_acla.monto_act_ces_vej = 0
      END IF

      CALL provisiona_cuenta(tot_1.monto_ces_vej + tot_1.monto_act_ces_vej + tot_1_acla.monto_ces_vej + tot_1_acla.monto_act_ces_vej,
                          acc.monto_ces_vej + acc.monto_act_ces_vej + acc_acla.monto_ces_vej + acc_acla.monto_act_ces_vej,
                          reg_1.*,
                          3,
                          1,
                          precio_dia,
                          hoy,
                          2)
   END IF

   -----Montos de aportes patronales (vivienda)--------------------------


   --- llenar tabla historica de plusvalia y minusvalia-----------

   IF tot_2.monto_ret IS NULL THEN
      LET tot_2.monto_ret = 0
   END IF

   IF tot_2.monto_act_ret IS NULL THEN
      LET tot_2.monto_act_ret = 0
   END IF

   IF tot_1_acla.monto_ret IS NULL THEN
      LET tot_1_acla.monto_ret = 0
   END IF

   IF tot_1_acla.monto_act_ret IS NULL THEN
      LET tot_1_acla.monto_act_ret = 0
   END IF
   IF tot_2.monto_ces_vej IS NULL THEN
      LET tot_2.monto_ces_vej = 0
   END IF

   IF tot_2.monto_act_ces_vej IS NULL THEN
      LET tot_2.monto_act_ces_vej = 0
   END IF

   IF tot_1_acla.monto_ces_vej IS NULL THEN
      LET tot_1_acla.monto_ces_vej = 0
   END IF

   IF tot_1_acla.monto_act_ces_vej IS NULL THEN
      LET tot_1_acla.monto_act_ces_vej = 0
   END IF

   IF acc.monto_ret IS NULL THEN
      LET acc.monto_ret = 0
   END IF

   IF acc_acla.monto_ret IS NULL THEN
      LET acc_acla.monto_ret = 0
   END IF

   IF acc.monto_act_ret IS NULL THEN
      LET acc.monto_act_ret = 0
   END IF

   IF acc_acla.monto_act_ret IS NULL THEN
      LET acc_acla.monto_act_ret = 0
   END IF

   IF acc.monto_ces_vej IS NULL THEN
      LET acc.monto_ces_vej = 0
   END IF

   IF acc_acla.monto_ces_vej IS NULL THEN
      LET acc_acla.monto_ces_vej = 0
   END IF

   IF acc.monto_act_ces_vej IS NULL THEN
      LET acc.monto_act_ces_vej = 0
   END IF

   IF acc_acla.monto_act_ces_vej IS NULL THEN
      LET acc_acla.monto_act_ces_vej = 0
   END IF

   IF tot_2.monto_ret > 0 THEN
      LET total.monto_ret     = tot_2.monto_ret +
                                tot_2.monto_act_ret +
                                tot_1_acla.monto_ret +
                                tot_1_acla.monto_act_ret

      LET total_acc.monto_ret = acc.monto_ret +
                                acc.monto_act_ret +
                                acc_acla.monto_ret +
                                acc_acla.monto_act_ret

   ELSE
      LET total.monto_ret     = tot_2.monto_ret +
                                tot_2.monto_act_ret

      LET total_acla.monto_ret = tot_1_acla.monto_ret +
                                 tot_1_acla.monto_act_ret

      LET total_acc.monto_ret = acc.monto_ret +
                                acc.monto_act_ret

      LET total_acc_acla.monto_ret = acc_acla.monto_ret +
                                     acc_acla.monto_act_ret
   END IF

   IF tot_2.monto_ces_vej > 0 THEN
      LET total.monto_ces_vej = tot_2.monto_ces_vej +
                                tot_2.monto_act_ces_vej +
                                tot_1_acla.monto_ces_vej +
                                tot_1_acla.monto_act_ces_vej

      LET total_acc.monto_ces_vej = acc.monto_ces_vej +
                                    acc.monto_act_ces_vej +
                                    acc_acla.monto_ces_vej +
                                    acc_acla.monto_act_ces_vej

   ELSE
      LET total.monto_ces_vej = tot_2.monto_ces_vej +
                                tot_2.monto_act_ces_vej

      LET total_acla.monto_ces_vej = tot_1_acla.monto_ces_vej +
                                     tot_1_acla.monto_act_ces_vej

      LET total_acc.monto_ces_vej = acc.monto_ces_vej +
                                    acc.monto_act_ces_vej

      LET total_acc_acla.monto_ces_vej = acc_acla.monto_ces_vej +
                                         acc_acla.monto_act_ces_vej

   END IF

   LET precio_acc = precio_acc_dia (v_siefore,v_fecha_conversion)

   IF total.monto_ret> 0 OR
      total.monto_ces_vej > 0 OR
      tot_2.monto_aport_pat > 0 THEN

      LET vtipo_registro = "04"

      IF tot_2.monto_aport_pat IS NULL THEN
         LET tot_2.monto_aport_pat = 0
      END IF

      IF tot_2.monto_par_viv IS NULL THEN
         LET tot_2.monto_par_viv = 0
      END IF

      IF precio_acc IS NULL THEN
         LET precio_acc = 0
      END IF

      INSERT INTO exc_exceso_plu_min
      VALUES (reg_1.folio,              -- folio
              vtipo_registro,           -- tipo registro
              reg_1.ident_servicio,     -- identificador de servicio
              reg_1.consec_reg_lote,    -- ret_consecutivo lote
              reg_1.reg_patronal_imss,  -- registro patronal imss
              reg_1.nss,                -- nss
              total.monto_ret,          -- monto minusvalia retiro
              total.monto_ces_vej,      -- monto minusvalia ces y vej
              0,                        -- monto minusvalia cuo soc
              0,                        -- monto minusvalia estatal
              0,                        -- monto minusvalia especial
              0,                        -- monto plusvalia patronal (viv)
              total_acc.monto_ret,      -- acciones minusvalia retiro
              total_acc.monto_ces_vej,  -- acciones minusvalia ces y vej
              0,                        -- acciones minusvalia cuo soc
              0,                        -- acciones minusvalia estatal
              0,                        -- acciones minusvalia especial
              tot_2.monto_par_viv,       -- participaciones de vivienda
              v_fecha_conversion,        -- fecha liquidacion inicio
              v_precio_accion                -- precio accion de inicio
             )
   ELSE
      IF total.monto_ret < 0 OR
         total.monto_ces_vej < 0 OR
         tot_2.monto_aport_pat < 0 THEN

         LET vtipo_registro = "05"

         IF tot_2.monto_aport_pat IS NULL THEN
            LET tot_2.monto_aport_pat = 0
         END IF

         IF tot_2.monto_par_viv IS NULL THEN
            LET tot_2.monto_par_viv = 0
         END IF

         IF precio_acc IS NULL THEN
            LET precio_acc = 0
         END IF

         INSERT INTO exc_exceso_plu_min
         VALUES (reg_1.folio,              -- folio
                 vtipo_registro,           -- tipo registro
                 reg_1.ident_servicio,     -- identificador de servicio
                 reg_1.consec_reg_lote,    -- ret_consecutivo lote
                 reg_1.reg_patronal_imss,  -- registro patronal imss
                 reg_1.nss,                -- nss
                 total.monto_ret,          -- monto plusvalia retiro
                 total.monto_ces_vej,      -- monto plusvalia ces y vej
                 0,                        -- monto plusvalia cuo soc
                 0,                        -- monto plusvalia estatal
                 0,                        -- monto plusvalia especial
                 0,                        -- monto plusvalia patronal (viv)
                 total_acc.monto_ret,      -- acciones minusvalia retiro
                 total_acc.monto_ces_vej,  -- acciones minusvalia ces y vej
                 0,                        -- acciones minusvalia cuo soc
                 0,                        -- acciones minusvalia estatal
                 0,                        -- acciones minusvalia especial
                 tot_2.monto_par_viv,       -- participaciones de vivienda
                 v_fecha_conversion,            -- fecha liquidacion inicio
                 v_precio_accion                -- precio accion de inicio
                )

         IF total_acla.monto_ret > 0 OR
            total_acla.monto_ces_vej > 0 THEN
            LET vtipo_registro = "04"

            IF tot_2.monto_aport_pat IS NULL THEN
               LET tot_2.monto_aport_pat = 0
            END IF

            IF tot_2.monto_par_viv IS NULL THEN
               LET tot_2.monto_par_viv = 0
            END IF

            IF precio_acc IS NULL THEN
               LET precio_acc = 0
            END IF

            INSERT INTO exc_exceso_plu_min
            VALUES (reg_1.folio,                   -- folio
                    vtipo_registro,                -- tipo registro
                    reg_1.ident_servicio,          -- identificador de servicio
                    reg_1.consec_reg_lote,         -- consecutivo lote
                    reg_1.reg_patronal_imss,       -- registro patronal imss
                    reg_1.nss,                     -- nss
                    total_acla.monto_ret,          -- monto minusvalia retiro
                    total_acla.monto_ces_vej,      -- monto minusvalia ces y vej
                    0,                             -- monto minusvalia cuo soc
                    0,                             -- monto minusvalia estatal
                    0,                             -- monto minusvalia especial
                    0,                                   -- monto plusvalia patronal (viv)
                    total_acc_acla.monto_ret,      -- acciones minusvalia retiro
                    total_acc_acla.monto_ces_vej,  -- acciones minusvalia ces y vej
                    0,                             -- acciones minusvalia cuo soc
                    0,                             -- acciones minusvalia estatal
                    0,                             -- acciones minusvalia especial
                    tot_2.monto_par_viv,       -- participaciones de vivienda
                    v_fecha_conversion,            -- fecha liquidacion inicio
                    0                              -- precio accion de inicio
                   )
         END IF
      END IF
   END IF

   IF comi2.impt_ret IS NULL THEN
      LET comi2.impt_ret = 0
   END IF

   IF comi2.impt_ces_vej IS NULL THEN
      LET comi2.impt_ces_vej = 0
   END IF

END FUNCTION
######################################################################
FUNCTION genera_tmp_cuenta (p_nss)

   DEFINE p_nss            CHAR(11),
          v_nombre_tabla   CHAR(20),
          sel_his          CHAR(2000)

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   DECLARE cur_his CURSOR FOR
   SELECT  tabname
   FROM    systables
   WHERE   tabname MATCHES "dis_cuenta??"

   FOREACH cur_his INTO v_nombre_tabla

      LET sel_his = sel_his CLIPPED,
               " SELECT * ",
               " FROM ",v_nombre_tabla,
               " WHERE nss = ","'",p_nss,"'"  ,
	       " AND    subcuenta IN (1,2,4) ",
               " AND   tipo_movimiento NOT IN (888,999) ",
               " UNION ALL "
   END FOREACH
   CLOSE cur_his

   LET sel_his = sel_his CLIPPED,
               " SELECT * ",
               " FROM   dis_cuenta ",
               " WHERE  nss = ","'",p_nss,"'"  ,
	       " AND    subcuenta IN (1,2,4) ",
               " AND    tipo_movimiento NOT IN (888,999) ",
               " INTO TEMP tmp_dis_cuenta "

   LET sel_his = sel_his CLIPPED

   PREPARE eje_sel_his FROM sel_his

   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta(folio,
                                                  consecutivo_lote,
                                                  subcuenta,
                                                  siefore
                                                 )
   CREATE INDEX tmp_dis_cuenta2 ON tmp_dis_cuenta(subcuenta)

   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION
######################################################################
FUNCTION saldo_al_dia(v_nss,
                      v_subcuenta,
                      v_grupo)

   DEFINE v_saldo_dia        CHAR(100),
          v_nss              CHAR(11),
          v_subcuenta        SMALLINT,
          v_grupo            SMALLINT

   DEFINE f_subcuenta        SMALLINT,
          f_siefore          SMALLINT,
          f_monto_acc        DECIMAL(16,6),
          f_monto_pes        DECIMAL(16,6)

   DEFINE v_saldo_acc        DECIMAL(16,6),
          v_saldo_pes        DECIMAL(16,6),
          hoy          DATE

   LET hoy = TODAY

   WHENEVER ERROR CONTINUE
      DROP TABLE temp_saldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE temp_saldo
   (subcuenta      SMALLINT,
    siefore        SMALLINT,
    acciones       DECIMAL(16,6),
    pesos          DECIMAL(16,6)
   )

   LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? ) "
 
   PREPARE eje_saldo_dia FROM v_saldo_dia

   LET v_saldo_acc  = 0
   LET v_saldo_pes  = 0

   DECLARE c_saldo CURSOR FOR  eje_saldo_dia 
   FOREACH c_saldo  USING v_nss,
                          v_subcuenta,
                          v_grupo,
                          hoy
                     INTO f_subcuenta,
                          f_siefore,
                          f_monto_acc,
                          f_monto_pes

        INSERT INTO temp_saldo
        VALUES (f_subcuenta,
                f_siefore,
                f_monto_acc,
                f_monto_pes
               )

        LET v_saldo_acc  = v_saldo_acc + f_monto_acc 
        LET v_saldo_pes  = v_saldo_pes + f_monto_pes
   END FOREACH

   RETURN v_saldo_acc,
          v_saldo_pes
END FUNCTION
###########################################################################
FUNCTION precio_acc_dia (x_siefore,fecha_recepcion)

   DEFINE fecha_recepcion  DATE,
          precio_dia       DECIMAL(16,6),
          x_siefore         SMALLINT

   LET cla_sel = " SELECT precio_del_dia ",
                 " FROM   glo_valor_accion ",
                 " WHERE  fecha_valuacion = ","'",fecha_recepcion,"'",
                 " AND    codigo_siefore = ",x_siefore

   PREPARE claexe_pre FROM cla_sel

   DECLARE cursor_pre CURSOR FOR claexe_pre

   OPEN cursor_pre
      FETCH cursor_pre INTO precio_dia

      IF SQLCA.SQLCODE = 0 THEN
         CLOSE cursor_pre
         RETURN precio_dia
      END IF

      --DISPLAY "NO HAY PRECIO DE ACCION DEL DIA CON SIEFORE:",x_siefore CLIPPED," ",STATUS,fecha_recepcion

      LET precio_dia = 0
      RETURN precio_dia

END FUNCTION
#*********************************************************************
FUNCTION provisiona_cuenta(monto_reg,
                        monto_acc_reg,
                        reg_2,
                        subcuenta,
                        movimiento,
                        precio_dia,
                        x_fecha_proceso,
                        siefore)

   DEFINE  monto_reg          DECIMAL(16,6),
           monto_acc_reg      DECIMAL(16,6),
           subcuenta          INTEGER,
           movimiento         INTEGER,
           precio_dia         DECIMAL(16,6),
           monto_en_pesos     DECIMAL(16,6),
           monto_en_acciones  DECIMAL(16,6),
           vfecha_archivo     DATE,
           aux_id_aportante   CHAR(20),
           x_fecha_proceso    DATE,
           siefore            CHAR(6),
           x_status           SMALLINT

   DEFINE reg_2  RECORD LIKE exc_det_exceso.*

   IF subcuenta <> 3 THEN
      LET monto_en_pesos    = monto_reg * -1

      LET monto_en_acciones = monto_acc_reg * -1
      LET aux_id_aportante = "PAG-EXC-PAT"
   ELSE
      LET aux_id_aportante = "PAG-EXC-VOL"
   END IF

   DECLARE cursor_prov_cargo CURSOR FOR clausula_sql3

   OPEN cursor_prov_cargo USING reg_2.folio,
                                reg_2.folio_pago_sua,
                                reg_2.nss,
                                subcuenta,
                                movimiento,
                                reg_2.consec_reg_lote,
                                siefore,
                                monto_en_acciones,
                                monto_en_pesos,
                                aux_id_aportante,
                                x_fecha_proceso

      FETCH cursor_prov_cargo INTO x_status

   CLOSE cursor_prov_cargo

END FUNCTION
#*********************************************************************
