DROP FUNCTION fn_recibe_sol_vu;
CREATE FUNCTION fn_recibe_sol_vu(
                 pc_folio_t_procesar    CHAR(50),
                 pc_nss                  CHAR(11),
                 pc_curp                 CHAR(18),
                 pc_entidad_origen       CHAR(3),
                 ps_tipo_tramite         CHAR(3),
                 pv_nombre               VARCHAR(40),
                 pv_paterno              VARCHAR(40),
                 pv_materno              VARCHAR(40),
                 pc_num_issste           CHAR(10),
                 pc_num_fol_issste       CHAR(14),
                 pc_fec_sol_tra          CHAR(8),
                 pc_fec_ip               CHAR(8),
                 pc_ind_sol_saldo_fip    CHAR(1),
                 pc_cve_pension          CHAR(2),
                 pc_ind_port             CHAR(1),
                 pc_ind_tra_post_fip     CHAR(1),
                 pd_ret97                DECIMAL(13,2),
                 pd_cs                   DECIMAL(13,2),
                 pd_cv                   DECIMAL(13,2),
                 pd_acum_viv97           DECIMAL(13,2),
                 pd_act_viv97            DECIMAL(13,2),
                 pd_ret_iss08            DECIMAL(13,2),
                 pd_cv_iss               DECIMAL(13,2),
                 pd_cs_iss               DECIMAL(13,2),
                 pd_ahorro_sol           DECIMAL(13,2),
                 pd_aport_comp           DECIMAL(13,2),
                 pd_aport_vol            DECIMAL(13,2),
                 pd_aport_lp             DECIMAL(13,2),
                 pd_acumfov08            DECIMAL(13,2),
                 pd_act_fov08            DECIMAL(13,2),
                 pc_status_viv           CHAR(1),
                 pc_edo_viv_fovissste    CHAR(01),
                 ps_ind_vivienda         CHAR(02),
                 pc_fec_venc             CHAR(8)
         )
                             
    RETURNING CHAR(50), CHAR(11), CHAR(18), CHAR(3), VARCHAR(255);

--Errores
    DEFINE   lv_err_tec_desc       VARCHAR(255);
    DEFINE   lc_diagnostico        CHAR(3);
    DEFINE   ld_id_sol_consec      DECIMAL(11,0);
    DEFINE   lv_user               VARCHAR(20);
    DEFINE   ld_folio              DECIMAL(15,0);
    DEFINE   ls_count_marca        SMALLINT;
    DEFINE   ls_marca_sol          SMALLINT;
    DEFINE   ls_cero               SMALLINT;
    DEFINE   ls_edo_reinversion    SMALLINT;
    DEFINE   lf_fecha              DATE;
    DEFINE   ls_marca              SMALLINT;
    DEFINE   ls_rechazo            SMALLINT;
    DEFINE   ls_estado_sol         SMALLINT;
    DEFINE   lc_ok                 CHAR(2);
    DEFINE   lf_fec_habil          DATE;       --fecha habil + 30 dias
    DEFINE   lf_fec_habilc         CHAR(10);   --fecha habil + 30 dias CHAR
    DEFINE   lf_fec_venc           CHAR(19);
    DEFINE   lf_fec_trasp          DATE;
    DEFINE   lf_fec_asig          DATE;
    DEFINE   lf_fec_ip             DATE;

--Variables de solicitud vieja
    DEFINE   ld_id_old             DECIMAL(11,0);
    DEFINE   ld_folio_old          DECIMAL(11,0);
    DEFINE   lf_fv_old             DATETIME YEAR TO SECOND; --fec. vencimiento

--Variables de exception
    DEFINE li_sql_err    INT;
    DEFINE li_isam_err   INT;
    DEFINE lc_error_info CHAR(100);

    ON EXCEPTION
      SET li_sql_err, li_isam_err, lc_error_info
           LET lc_diagnostico = "000";
           LET lv_err_tec_desc = li_isam_err||" "||li_sql_err||" "||lc_error_info;

       RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;

    END EXCEPTION;

--SET debug file TO "/qa/mln/safre/ret/fte/fn_recibe_sol_vu.txt";
--SET debug file TO "/afore/mlm/safre/ret/fte/fn_recibe_sol_vu.txt";
--trace ON;

    LET ls_marca_sol = 921;
    LET ls_cero      = 0;
    LET lf_fecha     = TODAY;
    LET ls_estado_sol = 0;
    LET lv_err_tec_desc = " ";
    LET lc_diagnostico = "101";
    LET ls_edo_reinversion = 0;
    LET ld_id_sol_consec = 0;
    LET lv_user = " ";
    LET ld_folio = 0;
    LET ls_count_marca = 0;
    LET lc_ok ="NE";
    LET ld_id_old     = 0;
    LET ld_folio_old  = 0;
    LET lf_fv_old     = NULL;
    LET lf_fec_habil  = TODAY;
    LET lf_fec_venc   = " ";

    LET lf_fec_habil = fn_habil_siguiente (lf_fecha,30);
    LET lf_fec_habilc = lf_fec_habil;
    LET lf_fec_venc = lf_fec_habilc[7,10]||"-"||lf_fec_habilc[1,2]||"-"||lf_fec_habilc[4,5]||" 00:00:00";

     SET LOCK MODE TO WAIT;
     LOCK TABLE safre_af:ret_consecutivo IN EXCLUSIVE MODE;

      SELECT NVL(MAX(consecutivo + 1),1), USER INTO ld_id_sol_consec,lv_user
      FROM safre_af:ret_consecutivo;
      
      INSERT INTO safre_af:ret_consecutivo VALUES(ld_id_sol_consec);

     UNLOCK TABLE safre_af:ret_consecutivo;
     SET LOCK MODE TO NOT WAIT;

    IF pc_entidad_origen != "1" AND pc_entidad_origen != "2" AND pc_entidad_origen != "3"
       AND pc_entidad_origen != "4" AND pc_entidad_origen != "5" THEN
          LET lc_diagnostico = "222";
          LET pc_ind_tra_post_fip = " ";
          LET lv_err_tec_desc = "ENTIDAD ORIGEN INCORRECTA: "||pc_entidad_origen ;
          LET ls_estado_sol = 110 ;
             SET LOCK MODE TO WAIT;
             LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
                 INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                             ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                             pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                             pc_num_issste, pc_num_fol_issste,
                             pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                             pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                             pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                             pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                             pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                             pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                             pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                             CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
                 );
             UNLOCK TABLE safre_af:ret_solicitud_saldo;
             SET LOCK MODE TO NOT WAIT;
          RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
    ELSE 
        IF pc_entidad_origen = "2" THEN  --issste
           SELECT n_seguro INTO pc_nss FROM safre_af:afi_mae_afiliado
           WHERE n_unico = pc_curp;
        ELSE
           SELECT "OK" INTO lc_ok FROM safre_af:afi_mae_afiliado
           WHERE n_seguro = pc_nss;
        END IF
        
        IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
           LET lc_diagnostico = "220";   --Cuenta no administrada por AFORE
           LET lv_err_tec_desc = " ";
        
             LET ls_estado_sol = 110 ;
              SET LOCK MODE TO WAIT ;
              LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
                  INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                              ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                              pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                              pc_num_issste, pc_num_fol_issste,
                              pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                              pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                              pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                              pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                              pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                              pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                              pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                              CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
                  );
              UNLOCK TABLE safre_af:ret_solicitud_saldo;
              SET LOCK MODE TO NOT WAIT;
        
              RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
        END IF;
    END IF
    
   --valida el tipo tramite 
   IF ps_tipo_tramite != "1" AND ps_tipo_tramite != "2" THEN
       LET lc_diagnostico = "222";
       LET pc_ind_tra_post_fip = " ";
       LET lv_err_tec_desc = "TIPO TRAMITE INCORRECTO: "||ps_tipo_tramite ;
       LET ls_estado_sol = 110 ;
          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

       RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
   END IF;
   
   --valida estatus de vivienda 
   IF pc_status_viv != "1" AND pc_status_viv != "2" AND pc_status_viv != "3" AND pc_status_viv != "4" THEN
       LET lc_diagnostico = "222";
       LET pc_ind_tra_post_fip = " ";
       LET lv_err_tec_desc = "ESTATUS DE VIVIENDA INCORRECTO: "||pc_status_viv ;
       LET ls_estado_sol = 110 ;
          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

       RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
   END IF;
    
   --valida el indicador de solicitud a la fip
   IF pc_ind_sol_saldo_fip != "1" AND pc_ind_sol_saldo_fip != "2" THEN 
       LET lc_diagnostico = "216";   --Indicador Solicitud de saldo a FIP invalido 
       LET pc_ind_sol_saldo_fip = " ";
       LET lv_err_tec_desc = "INDICADOR SOLICITUD DE SALDO A FIP INVALIDO" ;
       LET ls_estado_sol = 110 ;
          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

       RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
   END IF 

   --valida el indicador traspaso de solicitud a la fip
   IF pc_ind_tra_post_fip != "0" AND pc_ind_tra_post_fip != "1" THEN
       LET lc_diagnostico = "222";
       LET pc_ind_tra_post_fip = " ";
       LET lv_err_tec_desc = "INDICADOR DE TRASPASO POSTERIOR A FIP INCORRECTO: "||pc_ind_tra_post_fip ;
       LET ls_estado_sol = 110 ;
          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

       RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
   END IF;
   
   --valida fecha inicio de pension y fecha traspaso
   IF pc_ind_sol_saldo_fip == "2" AND pc_ind_tra_post_fip == "0" THEN 
   
       SELECT finicta INTO lf_fec_trasp 
       FROM safre_af:afi_mae_afiliado
       WHERE n_seguro = pc_nss;
       
       LET lf_fec_ip = pc_fec_ip[5,6]||"/"||pc_fec_ip[7,8]||"/"||pc_fec_ip[1,4];
       
       IF lf_fec_ip < lf_fec_trasp THEN

          SELECT fecha_asignacion INTO lf_fec_asig
          FROM safre_af:afi_det_asignado
          WHERE n_seguro = pc_nss
          AND fecha_asignacion = (SELECT MAX(fecha_asignacion)
                                  FROM safre_af:afi_det_asignado
                                  WHERE n_seguro = pc_nss);

          IF DBINFO('SQLCA.SQLERRD2') > 0 THEN  --Si existe como asignado
               IF lf_fec_ip < lf_fec_asig THEN
                    LET lc_diagnostico = "216";   --Indicador Solicitud de saldo a FIP invalido
                    LET lv_err_tec_desc = "FECHA INICIO DE PENSION MENOR A FECHA DE ASIGNACION";
               END IF
          ELSE
              LET lc_diagnostico = "216";   --Indicador Solicitud de saldo a FIP invalido
              LET lv_err_tec_desc = "FECHA INICIO DE PENSION MENOR A FECHA DE TRASPASO";
          END IF
          
          IF lc_diagnostico = "216" THEN
                LET ls_estado_sol = 110 ;
                SET LOCK MODE TO WAIT;
                LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
                    INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                                ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                                pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                                pc_num_issste, pc_num_fol_issste,
                                pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                                pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                                pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                                pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                                pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                                pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                                pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                               CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
                   );
                UNLOCK TABLE safre_af:ret_solicitud_saldo;
                SET LOCK MODE TO NOT WAIT;

                RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
          END IF
       END IF
   END IF 

    SET LOCK MODE TO WAIT;
       EXECUTE PROCEDURE marca_cuenta
           (pc_nss, ls_marca_sol, ld_id_sol_consec, ls_cero, ls_cero, ls_marca_sol, lf_fecha, lv_user) 
             INTO ls_marca, ls_rechazo;
    SET LOCK MODE TO NOT WAIT;

    IF ls_rechazo != 0 THEN -- si NO marco
         LET lc_diagnostico = "000";
         SELECT diag_recep_afore INTO lc_diagnostico FROM safre_af:ret_diag_solicitud
         WHERE marca_cod = ls_marca;

         IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
            LET lc_diagnostico = "222";   --Marca para Proceso Especial detectada para cualquier módulo
            LET lv_err_tec_desc = " " ;
            LET ls_estado_sol = 110 ;

          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

            RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
         ELSE
            LET lv_err_tec_desc = " ";

            --si es rechazado por ya tener solicitud de saldo
            IF lc_diagnostico = 218 THEN

               IF ps_tipo_tramite = "2" OR pc_ind_port = "9" THEN    --ampliacion de vigencia

                  SELECT id_solicitud_saldo, folio_cargo, fecha_vencimiento
                  INTO ld_id_old, ld_folio_old, lf_fv_old
                  FROM safre_af:ret_solicitud_saldo
                  WHERE nss = pc_nss
                  AND estado_solicitud = 106;  --liquidado saldo

                  IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
                     LET lc_diagnostico = "225";
                     LET lv_err_tec_desc = " "; --NO procede ampliacion de evigencia
                     LET ls_estado_sol = 110 ;
                     SET LOCK MODE TO WAIT;
                     LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
                         INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                                     ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                                     pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                                     pc_num_issste, pc_num_fol_issste,
                                     pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                                     pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                                     pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                                     pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                                     pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                                     pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                                     pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                                     CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
                         );
                     UNLOCK TABLE safre_af:ret_solicitud_saldo;
                     SET LOCK MODE TO NOT WAIT;

                     RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
                  END IF;  --NO existe solicitud de saldos

                SET LOCK MODE TO WAIT;
                LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;

                  UPDATE safre_af:ret_solicitud_saldo SET fecha_vencimiento = lf_fec_venc,
                                                          f_recep_procesar = CURRENT
                  WHERE nss = pc_nss
                  AND estado_solicitud = 106   --liquidado saldo
                  AND id_solicitud_saldo = ld_id_old;

                UNLOCK TABLE safre_af:ret_solicitud_saldo;
                SET LOCK MODE TO NOT WAIT;

                SET LOCK MODE TO WAIT;
                LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;

                   INSERT INTO safre_af:ret_folio_vigencia VALUES (
                              ld_folio_old,     --folio anterior
                              null,            --folio actual
                              CURRENT,         --fecha actual
                              lf_fv_old,        --fecha de vencimiento anterior
                              pc_nss
                  );

                UNLOCK TABLE safre_af:ret_solicitud_saldo;
                SET LOCK MODE TO NOT WAIT;

                  LET lc_diagnostico = "101";
                  LET lv_err_tec_desc = " ";

                  RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;

               END IF; --pc_ind_port = 9 --ampliacion de vig
            END IF;   --rechazo por que ya existe una sol.
         END IF;   --NO existe marca_diagnostico
    END IF;   --si es rechazada por convivencia de marcas

--trace lc_diagnostico;

    IF lc_diagnostico = "101" THEN --aceptada
       LET ls_estado_sol = 100 ;
    ELSE     --rechazada
       LET ls_estado_sol = 110 ;
    END IF;

          SET LOCK MODE TO WAIT;
          LOCK TABLE safre_af:ret_solicitud_saldo IN EXCLUSIVE MODE;
              INSERT INTO safre_af:ret_solicitud_saldo VALUES (
                          ld_id_sol_consec, pc_folio_t_procesar, ld_folio, pc_nss, pc_curp,
                          pc_entidad_origen,ps_tipo_tramite, pv_nombre, pv_paterno, pv_materno,
                          pc_num_issste, pc_num_fol_issste,
                          pc_fec_sol_tra[1,4]||"-"||pc_fec_sol_tra[5,6]||"-"||pc_fec_sol_tra[7,8]||" 00:00:00",
                          pc_fec_ip[1,4]||"-"||pc_fec_ip[5,6]||"-"||pc_fec_ip[7,8]||" 00:00:00",
                          pc_ind_sol_saldo_fip, pc_cve_pension, pc_ind_port,
                          pc_ind_tra_post_fip, pd_ret97, pd_cs, pd_cv, pd_acum_viv97,
                          pd_act_viv97, pd_ret_iss08, pd_cv_iss, pd_cs_iss, pd_ahorro_sol,
                          pd_aport_comp, pd_aport_vol, pd_aport_lp, pd_acumfov08, pd_act_fov08,
                          pc_status_viv, pc_edo_viv_fovissste, ps_ind_vivienda, lf_fec_venc,
                          CURRENT, "", "", "", lc_diagnostico, "", "", lv_user, ls_estado_sol
              );
          UNLOCK TABLE safre_af:ret_solicitud_saldo;
          SET LOCK MODE TO NOT WAIT;

           RETURN pc_folio_t_procesar, pc_nss, pc_curp, lc_diagnostico, lv_err_tec_desc;
--trace OFF;
END FUNCTION;

