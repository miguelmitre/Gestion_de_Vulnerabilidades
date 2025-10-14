#############################################################################
#Owner             => E.F.P.
#Programa TRACI01  => RECEPCIONA ARCHIVOS DE TRASPASOS ENVIADOS POR PROCESAR
#Fecha creacion    => 30 DE SEPTIEMBRE DE 1997
#By                => JESUS DAVID YANEZ MORENO
#Version para      => XXI,INVERCAP,COPPEL Y METLIFE
#Modificado  By    => MARCO é ISABEL                        
#                  => 29 -JUN- 2009
#Ultima Mod        => 26 -FEB- 2010
#Ultima Mod        => 22 -ABR- 2010
#Objetivo de la Mod=> La Modificacion consistio en contemplar las AIV'S.
#                     -- UNIFICACION SAR-ISSSTE --
#                     Version: 2.0 Sustituye a: 1.0 (26/03/2007)
#
#Objetivo de la Mod --Fecha de Emision: 01/12/2009
#                   --Version : 1:0
#                   --Sustituye a : Ninguna
#Sistema           => TRA-ICE-ISSSTE
#Req:717           => JCPV 02/12/2011. correct sintax error ln 3085
##############################################################################
DATABASE safre_af

GLOBALS

DEFINE a_fecha_genera              CHAR(010)
DEFINE vvvfecha                    DATE
DEFINE nn_no_act                   INTEGER
DEFINE #glo #reg_1
        reg_1                      RECORD LIKE seg_modulo.*

DEFINE v_correlativo               LIKE tra_mae_icefa_issste.correlativo

DEFINE reg                         RECORD #glo #reg
       nom_archivo                 CHAR(16) ,
       devol_solicit               CHAR(01) ,
       trasp_cuentas               CHAR(01) ,
       trasp_complem               CHAR(01) ,
       solic_no_aten               CHAR(01) ,
       solic_rechaza               CHAR(01) ,
       solic_devol                 CHAR(01) ,
       devol_planchadas            CHAR(01) ,
       devol_rechazadas            CHAR(01)
                                   END RECORD

    DEFINE dev_cza_planchada RECORD #glo #cza_planchadas
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
    END RECORD

    DEFINE dev_cza_rechazada RECORD #glo #cza_rechazadas
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
    END RECORD

    DEFINE reg_cza_devol RECORD #glo #reg_cza_devol
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     CHAR(08) ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(03)
    END RECORD

    DEFINE reg_cza_trasp RECORD #reg_cza_trasp
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        rechazo               CHAR(03)
    END RECORD

    DEFINE reg_cza_sol_no_ate RECORD #glo #reg_cza_sol_no_ate
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        fech_presentacion     CHAR(08) ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
    END RECORD
    
    DEFINE reg_cza_sol_confronta RECORD
        tipo_registro         SMALLINT ,
        icefa                 CHAR(09) ,
        sentido               CHAR(01) ,
        plaza_presentacion    SMALLINT ,
        servicio_procesar     SMALLINT ,
        nro_archivo           SMALLINT ,
        fecha_generacion      DATE     ,
        nro_secuencia         INTEGER
    END RECORD

    DEFINE reg_cza_devol_conf RECORD #glo #reg_cza_devol_conf
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        tipo_ent_destino      CHAR(02) ,
        cve_ent_destino       CHAR(03) ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02)
    END RECORD

    DEFINE dev_det_planchada RECORD #glo #det_planchadas
        tipo_registro         CHAR(02) ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_trasp       CHAR(002)     ,
        fecha_presentacion    DATE          ,
        fecha_liquidacion     DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)     ,
        acciones_sar_92       DECIMAL(15,6) ,
        saldo_sar_92          DECIMAL(15,2) ,
        saldo_sar_92_banx     DECIMAL(15,2) ,
        saldo_sar_pagar       DECIMAL(15,2) ,
        saldo_sar_pagar_p     DECIMAL(15,2) ,
        saldo_viv_92          DECIMAL(15,2) ,
        saldo_viv_92_p        DECIMAL(15,2) ,
        comis_saldo_sar       DECIMAL(15,2) ,
        planchado_sar         CHAR(002)     ,
        planchado_viv         CHAR(002)
    END RECORD

    DEFINE dev_det_rechazada RECORD #glo #det_rechazadas
        tipo_registro         CHAR(02) ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_trasp       CHAR(002)     ,
        fecha_presentacion    DATE          ,
        fecha_liquidacion     DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)     ,
        saldo_sar_92          DECIMAL(15,2) ,
        saldo_sar_92_banx     DECIMAL(15,2) ,
        saldo_sar_pagar       DECIMAL(15,2) ,
        saldo_viv_92          DECIMAL(15,2)
    END RECORD

    DEFINE reg_det_devol RECORD #glo #reg_det_devol
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_traspaso    CHAR(002)     ,
        fech_solicitud        DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombre                CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recepc_solic     DATE          ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)     ,
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(003)     ,
        nombre_imss           CHAR(050)     ,
        status_registro       CHAR(002)     
    END RECORD
    
    DEFINE reg_det_trasp_sal RECORD #glo #reg_det_trasp_sal
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_traspaso    CHAR(002)     ,
        fech_presentacion     DATE          ,
        fech_mov_banxico      DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombre                CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recepc_solic     DATE          ,
        ident_lote_solic      CHAR(016)     ,
        id_procesar           CHAR(08)      ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_ent            CHAR(120)     ,
        saldo_ret_97          DECIMAL(15,2) ,
        saldo_ces_vej         DECIMAL(15,2) ,
        saldo_aport_vol       DECIMAL(15,2) ,
        saldo_viv_97          DECIMAL(15,2) ,
        saldo_cuo_soc         DECIMAL(15,2) ,
        saldo_aport_est       DECIMAL(16,6) ,
        saldo_cuo_esp         DECIMAL(15,2) ,
        saldo_sar_92          DECIMAL(15,2) ,
        saldo_viv_92          DECIMAL(15,2) ,
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(003)     ,
        nombre_imss           CHAR(050)
    END RECORD

    DEFINE reg_det_trasp_int RECORD
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_traspaso    CHAR(002)     ,
        fech_presentacion     DATE          ,
        fech_mov_banxico      DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc_afo_recep         CHAR(013)     ,
        paterno_afo_recep     CHAR(040)     ,
        materno_afo_recep     CHAR(040)     ,
        nombres_afo_recep     CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      DATE          ,
        ident_lote_solic      CHAR(016)     ,
        id_procesar           CHAR(08)      ,
        n_seguro_ent_ced      CHAR(011)     ,
        rfc_ent_ced           CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)     ,
        int_ret_97            DECIMAL(15,2) ,
        int_ces_vej           DECIMAL(15,2) ,
        int_aport_vol         DECIMAL(15,2) ,
        int_viv_97            DECIMAL(15,2) ,
        int_cuo_soc           DECIMAL(15,2) ,
        int_aport_est         DECIMAL(16,6) ,
        int_cuo_esp           DECIMAL(15,2) ,
        int_sar_92            DECIMAL(15,2) ,
        int_viv_92            DECIMAL(15,2) ,
        nombre_imss           CHAR(050)
    END RECORD

    DEFINE reg_det_tot_aport_his RECORD
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_traspaso    CHAR(002)     ,
        fech_presentacion     CHAR(008)     ,
        fech_mov_banxico      CHAR(008)     ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc_afo_recep         CHAR(013)     ,
        paterno_afo_recep     CHAR(040)     ,
        materno_afo_recep     CHAR(040)     ,
        nombres_afo_recep     CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      CHAR(008)     ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent_ced      CHAR(011)     ,
        rfc_ent_ced           CHAR(013)     ,
        imp_tot_ret_97        DECIMAL(15,2) ,
        imp_tot_ces_vej       DECIMAL(15,2) ,
        imp_tot_aport_vol     DECIMAL(15,2) ,
        imp_tot_viv_97        DECIMAL(15,2) ,
        imp_tot_cuo_soc       DECIMAL(15,2) ,
        imp_tot_est           DECIMAL(15,2) ,
        imp_tot_cuo_esp       DECIMAL(15,2) ,
        num_aport_rcv         SMALLINT      ,
        num_aport_viv_97      SMALLINT      ,
        num_aport_vol         SMALLINT      ,
        cod_result_operac     CHAR(002)     ,
        diag_proceso          CHAR(015)     ,
        nombre_imss           CHAR(050)
    END RECORD

    DEFINE reg_det_sol_no_ate RECORD #glo #reg_det_sol_no_ate
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_traspaso    CHAR(002)     ,
        fech_presentacion     DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        fech_recep_solic      DATE          ,
        ident_lote_solic      CHAR(016)     ,
        n_seguro_ent_ced      CHAR(011)     ,
        rfc_ent_ced           CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nom_comp_tra_icefa    CHAR(120)
    END RECORD

    DEFINE reg_det_sol_confronta RECORD #reg_det_sol_confronta
        tipo_registro         CHAR(02)      ,
        cod_operacion         SMALLINT      ,
        fecha_presentacion    DATE          ,
        afore_emisora         CHAR(09)      ,
        icefa_receptora       CHAR(09)      ,
        total                 DECIMAL(15,2) ,
        mot_devol             SMALLINT      ,
        rfc_icefa             CHAR(13)      ,
        nro_afili_imss        CHAR(11)      ,
        nro_ctrl_icefa        CHAR(30)      ,
        nom_icefa             CHAR(120)     ,
        rfc_afore             CHAR(013)     ,
        nro_afili_afore       CHAR(011)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nom_afore             CHAR(040)     ,
        tot_saldo_sar_92      DECIMAL(15,2) ,
        tot_saldo_viv_92      DECIMAL(15,2) ,
        cta_sar_solicitada    SMALLINT      ,
        origen_trasp          SMALLINT      ,
        trasp_icefa_afore     SMALLINT      ,
        mot_rechazo           CHAR(020)     ,
        nro_secuencia         INTEGER
    END RECORD

    DEFINE reg_det_devol_conf RECORD #reg_det_devol_conf
        tipo_registro         CHAR(02)      ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(03)      ,
        cve_recep_cuenta      CHAR(03)      ,
        tipo_ced_cuenta       CHAR(02)      ,
        cve_ced_cuenta        CHAR(03)      ,
        orig_tipo_trasp       CHAR(02)      ,
        fecha_presentacion    DATE          ,
        periodo_liquid        CHAR(06)      ,
        n_unico               CHAR(18)      ,
        n_seguro              CHAR(11)      ,
        rfc                   CHAR(13)      ,
        paterno               CHAR(40)      ,
        materno               CHAR(40)      ,
        nombres               CHAR(40)      ,
        cve_sector            CHAR(01)      ,
        ident_lote_solic      CHAR(16)      ,
        n_seguro_ent          CHAR(11)      ,
        rfc_ent               CHAR(13)      ,
        nro_ctrl_icefa        CHAR(30)      ,
        nombre_trab_icefa     CHAR(120)     ,
        saldo_sar_92          DECIMAL(15,2) ,
        saldo_viv_92          DECIMAL(15,2)
    END RECORD

    DEFINE reg_det_conf_parcial RECORD #glo #reg_det_conf_parcial
        tipo_registro         CHAR(02)      ,
        cod_operacion         SMALLINT      ,
        fecha_presentacion    DATE          ,
        afore_emisora         CHAR(09)      ,
        icefa_receptora       CHAR(09)      ,
        total                 DECIMAL(15,2) ,
        mot_devol             SMALLINT      ,
        rfc_icefa             CHAR(13)      ,
        nro_afili_imss        CHAR(11)      ,
        nro_ctrl_icefa        CHAR(30)      ,
        nom_icefa             CHAR(120)     ,
        rfc_afore             CHAR(013)     ,
        nro_afili_afore       CHAR(011)     ,
        paterno_afore         CHAR(040)     ,
        materno_afore         CHAR(040)     ,
        nom_afore             CHAR(040)     ,
        tot_saldo_sar_92      DECIMAL(15,2) ,
        tot_saldo_viv_92      DECIMAL(15,2) ,
        cta_sar_solicitada    SMALLINT      ,
        origen_trasp          SMALLINT      ,
        trasp_icefa_afore     SMALLINT      ,
        mot_rechazo           CHAR(020)     ,
        nro_secuencia         INTEGER       ,
        par_saldo_sar_92      DECIMAL(15,2) ,
        par_saldo_viv_92      DECIMAL(15,2)
    END RECORD

    DEFINE dev_sum_planchada RECORD #glo #sum_planchadas
        tipo_registro         CHAR(02)      ,
        cantidad_reg_det      INTEGER       ,
        total_sar_92          DECIMAL(15,2) ,
        total_sar_92_banx     DECIMAL(15,2) ,
        total_sar_pagar       DECIMAL(15,2) ,
        total_viv_92          DECIMAL(15,2)
    END RECORD

    DEFINE dev_sum_rechazada RECORD #glo #sum_rechazadas
        tipo_registro         CHAR(02)      ,
        cantidad_reg_det      INTEGER       ,
        total_sar_92          DECIMAL(15,2) ,
        total_sar_92_banx     DECIMAL(15,2) ,
        total_sar_pagar       DECIMAL(15,2) ,
        total_viv_92          DECIMAL(15,2)
    END RECORD

    DEFINE reg_sum_devol_conf RECORD #glo #reg_sum_devol_conf
        tipo_registro         CHAR(02)      ,
        cantidad_reg_det      INTEGER       ,
        total_saldo_sar_92    DECIMAL(15,2) ,
        total_saldo_viv_92    DECIMAL(15,2)
    END RECORD

    DEFINE reg_sum_devol RECORD #glo #reg_sum_devol
        tipo_registro         CHAR(002) ,
        cant_reg_det          INTEGER
    END RECORD

    DEFINE reg_sum_trasp RECORD 
           tipo_registro    char(2),
           cant_reg_det     integer,
           tot_sal_sar_o61  decimal(15,2),
           tot_sal_viv_o61  decimal(15,2),
           tot_part_viv_o61 decimal(15,2),
           int_sal_sar_o61  decimal(15,2),
           tot_sal_sar_o62  decimal(15,2),
           tot_sal_viv_o62  decimal(15,2),
           tot_part_viv_o62 decimal(15,2),
           int_sal_sar_o62  decimal(15,2),
           tot_sal_sar_o63  decimal(15,2),
           tot_sal_viv_o63  decimal(15,2),
           tot_part_viv_o63 decimal(15,2),
           int_sal_sar_o63  decimal(15,2),
           tot_sal_sar_o65  decimal(15,2),
           tot_sal_viv_o65  decimal(15,2),
           tot_part_viv_o65 decimal(15,2),
           int_sal_sar_o65  decimal(15,2)
    END RECORD	

    DEFINE reg_sum_sol_no_ate RECORD #glo #reg_sum_sol_no_ate
        tipo_registro         CHAR(02) ,
        cantidad_reg_det      INTEGER
    END RECORD

    DEFINE reg_cza_sol_rech RECORD #reg_cza_sol_rech
        tipo_registro         CHAR(02) ,
        ident_servicio        CHAR(02) ,
        ident_operacion       CHAR(02) ,
        tipo_ent_origen       CHAR(02) ,
        cve_ent_origen        CHAR(03) ,
        tipo_ent_destino      CHAR(02) ,
        ent_fed_envio_lote    CHAR(03) ,
        fech_presentacion     DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(02) ,
        cod_result_operac     CHAR(02) ,
        mot_rechazo_lote_1    CHAR(03) ,
        mot_rechazo_lote_2    CHAR(03) ,
        mot_rechazo_lote_3    CHAR(03)
    END RECORD

    DEFINE reg_det_sol_rech RECORD #reg_det_sol_rech
        tipo_registro         CHAR(002)     ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(002)     ,
        cve_recep_cuenta      CHAR(003)     ,
        tipo_ced_cuenta       CHAR(002)     ,
        cve_ced_cuenta        CHAR(003)     ,
        orig_tipo_trasp       CHAR(002)     ,
        fech_presentacion     DATE          ,
        n_unico               CHAR(018)     ,
        n_seguro              CHAR(011)     ,
        rfc                   CHAR(013)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        nombres               CHAR(040)     ,
        cve_sector            CHAR(001)     ,
        fech_recep_solic      DATE          ,
        ident_lote_solic      CHAR(016)     ,
        id_procesar           CHAR(08)      ,
        n_seguro_ent          CHAR(011)     ,
        rfc_ent               CHAR(013)     ,
        nro_ctrl_icefa        CHAR(030)     ,
        nombre_trab_icefa     CHAR(120)     ,
        cod_result_operac     CHAR(002)     ,
        diag_proceso_1        CHAR(003)     ,
        diag_proceso_2        CHAR(003)     ,
        diag_proceso_3        CHAR(003)     ,
        diag_proceso_4        CHAR(003)     ,
        diag_proceso_5        CHAR(003)     ,
        afore_receptora       CHAR(003)
    END RECORD

    DEFINE reg_sum_sol_rech RECORD #reg_sum_sol_rech
        tipo_registro         CHAR(02) ,
        cantidad_reg_det      INTEGER
    END RECORD

    DEFINE rzo_lote RECORD
        result_operacion      CHAR(02) ,
        motivo_rechazo1       CHAR(03) ,
        motivo_rechazo2       CHAR(03) ,
        motivo_rechazo3       CHAR(03)
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        enviada               SMALLINT ,
        aceptada_procesar     SMALLINT ,
        aceptada_icefa        SMALLINT ,
        devuelta              SMALLINT ,
        no_atendida           SMALLINT ,
        rechazada             SMALLINT ,
        complementarios       SMALLINT ,
        confrontada           SMALLINT ,
        dev_confrontada       SMALLINT ,
        solic_devol           SMALLINT ,
        conf_parcial          SMALLINT ,
	pendiente_asig        SMALLINT ,
	liberada_icefa        SMALLINT 
    END RECORD

    DEFINE #date
        HOY                   DATE
     
    DEFINE #glo #char
        k                     char(500) ,
        txt_1                 char(500) ,
        txt_insert            CHAR(100) ,
        txt_2                 char(121) ,
        tmp_pla_trasp2      char(200) ,
        usuario               CHAR(008) ,
        c2_marca              CHAR(004) ,
        archivo_tipo_trasp    CHAR(200) ,
	carga_archivo         CHAR(300) ,
	archivo_paso          CHAR(004) ,
        c_16_saldo             CHAR(016) ,
	c16_total             CHAR(016) , 
	c10_fecha             CHAR(010) ,
	c16_int_sar_92        CHAR(016) ,
        c16_int_viv_92        CHAR(016) ,
        c16_saldo_sar_92      CHAR(016) ,
        c16_saldo_viv_92      CHAR(016) ,
        c16_sal_sar_92        CHAR(016) ,
        c16_sal_viv_92        CHAR(016) ,
        c16_part_viv_92       CHAR(016) ,
        c16_saldo_parti        CHAR(016),
        enter    	      CHAR(001) ,
        archivo_traspaso      CHAR(200) ,
        lote_rechazado        CHAR(002) ,
        cat                   CHAR(300)

    DEFINE #glo #smallint
        s_nro_icefa           ,
        s_estado              ,
        s_cuantos_tipos       ,
        cuantos               ,
        s_codigo_afore        SMALLINT

    DEFINE #integer
        cont                  ,
        ultimo_folio          ,
        i_sol_devueltas       INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST


LET nn_no_act = 0

   CALL init()
    LET tmp_pla_trasp2 =  'xx'
    WHENEVER ERROR CONTINUE
       DROP TABLE tmp_pla_trasp2
    WHENEVER ERROR STOP

                CREATE TEMP TABLE xx
					 (n_registros          CHAR(730))


   OPEN WINDOW trac0011 AT 4,4 WITH FORM "TRACIU011" ATTRIBUTE(BORDER)
   DISPLAY " TRACIU01    CARGA ARCHIVO ENVIADO POR PROCESAR UNI-SAR-ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg.* WITHOUT DEFAULTS
       AFTER FIELD nom_archivo
	   IF reg.nom_archivo IS NULL THEN
	       ERROR "Campo NO puede ser NULO"
	       NEXT FIELD nom_archivo
	   END IF

       AFTER FIELD devol_solicit
	   IF reg.devol_solicit <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD devol_solicit
	   END IF

       AFTER FIELD trasp_cuentas
	   IF reg.trasp_cuentas <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD trasp_cuentas
	   END IF

       AFTER FIELD trasp_complem
	   IF reg.trasp_complem <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD trasp_complem
	   END IF

       AFTER FIELD solic_no_aten
	   IF reg.solic_no_aten <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD solic_no_aten
	   END IF

       AFTER FIELD solic_rechaza
	   IF reg.solic_rechaza <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD solic_rechaza
	   END IF

       AFTER FIELD solic_devol
	   IF reg.solic_devol <> "X" THEN
	       ERROR "SOLO PUEDE MARCAR CON 'X' "
	       NEXT FIELD solic_devol   
	   END IF

   
       AFTER FIELD devol_planchadas
           IF reg.devol_planchadas <> "X" THEN
               ERROR "SOLO PUEDE MARCAR CON 'X' "
               NEXT FIELD devol_planchadas
           END IF

       AFTER FIELD devol_rechazadas
           IF reg.devol_rechazadas <> "X" THEN
               ERROR "SOLO PUEDE MARCAR CON 'X' "
               NEXT FIELD devol_rechazadas
           END IF

    ON KEY (ESC)
           IF reg.devol_solicit = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "06"
           END IF


	   IF reg.trasp_cuentas = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED

               LET c2_marca      = "09"
           END IF


           IF reg.trasp_complem = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "12"
           END IF


	   IF reg.solic_no_aten = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "14"
           END IF


	   IF reg.solic_rechaza = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "01"
           END IF

	   IF reg.solic_devol    = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "16"
           END IF

	   IF reg.devol_planchadas    = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca      = "18"
           END IF

	   IF reg.devol_rechazadas = "X" THEN
               LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
               LET c2_marca = "19"
           END IF

           WHENEVER ERROR CONTINUE
               DISPLAY "PROCESANDO INFORMACION " AT 19,2

               CASE c2_marca
                   WHEN "09"
                       CALL limpieza(reg_1.ruta_rescate) #l
                   WHEN "12"
                       CALL limpieza(reg_1.ruta_rescate) #l
               END CASE

               LET txt_insert = " INSERT INTO ",tmp_pla_trasp2 CLIPPED 
 
               LOAD FROM archivo_tipo_trasp 
                DELIMITER "+" 
               txt_insert 

               LET txt_1 = " SELECT count(*) ",
                           " FROM   ",tmp_pla_trasp2 CLIPPED
             
               PREPARE pre_21 FROM txt_1
               DECLARE cur_21 CURSOR FOR pre_21
               FOREACH cur_21 INTO cuantos
               END FOREACH     
               IF cuantos = 0 THEN
                   DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                   AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD nom_archivo
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT
   
   CALL clasifica() #c

END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
  
    SELECT codigo_afore   ,
           USER 
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT estado
    INTO   reg_4.enviada
    FROM   tra_status
    WHERE  des_estado = "ENVIADA"

    SELECT estado
    INTO   reg_4.rechazada
    FROM   tra_status
    WHERE  des_estado = "RECHAZADA"

    SELECT estado
    INTO   reg_4.devuelta
    FROM   tra_status
    WHERE  des_estado = "DEVUELTA"

    SELECT estado
    INTO   reg_4.aceptada_procesar
    FROM   tra_status
    WHERE  des_estado = "ACEPTADA_PROCESAR"

    SELECT estado
    INTO   reg_4.aceptada_icefa
    FROM   tra_status
    WHERE  des_estado = "ACEPTADA_ICEFA"

    SELECT estado
    INTO   reg_4.solic_devol
    FROM   tra_status
    WHERE  des_estado = "SOLIC_DEVOL"

    SELECT estado
    INTO   reg_4.solic_devol
    FROM   tra_status
    WHERE  des_estado = "NO ATENDIDA"
   
    SELECT *
    INTO   reg_1.*
    FROM   seg_modulo
	 WHERE modulo_cod = "tra"
    --let reg_1.ruta_rescate = "/afore/cpl/safre_prc/tra/rescate"     #temp

END FUNCTION

FUNCTION clasifica()
#c------------------

    IF c2_marca <> "18" THEN
        SELECT MAX(folio) + 1
        INTO   ultimo_folio
        FROM   glo_folio

    INSERT INTO glo_folio VALUES(ultimo_folio)
    END IF
#pp
    CASE c2_marca
        WHEN "06"
            SELECT estado
            INTO   reg_4.devuelta
            FROM   tra_status
            WHERE  des_estado = "DEVUELTA"

          --CALL devolucion_solicitudes() #ds
            EXIT CASE
        WHEN "09"
            SELECT estado
            INTO   reg_4.aceptada_icefa
            FROM   tra_status
            WHERE  des_estado = "ACEPTADA_ICEFA"

            CALL traspaso_cuentas() #tc
            EXIT CASE

        WHEN "12"
            SELECT estado
            INTO   reg_4.complementarios
            FROM   tra_status
            WHERE  des_estado = "COMPLEMENTARIOS"

            CALL traspasos_complement() #tc
            EXIT CASE

        WHEN "14"
            SELECT estado
            INTO   reg_4.no_atendida 
            FROM   tra_status
            WHERE  des_estado = "NO_ATENDIDA"

         -- CALL solic_no_atendidas() #sna
            EXIT CASE

        WHEN "01"
            SELECT estado
            INTO   reg_4.rechazada
            FROM   tra_status
            WHERE  des_estado = "RECHAZADA"

            CALL solic_rechazadas() #sr
            EXIT CASE

        WHEN "16"
            SELECT estado
            INTO   reg_4.solic_devol
            FROM   tra_status
            WHERE  des_estado = "SOLIC_DEVOL"

         -- CALL solic_devolucion() #sd
            EXIT CASE
        WHEN "18"
         -- CALL planchadas() #p
            EXIT CASE

        WHEN "19"
         -- CALL devoluciones_rechazadas() #dr
            EXIT CASE

    END CASE
    DISPLAY "REGISTROS NO ACTUALIZADOS : ",nn_no_act AT 18,2
    PROMPT " REGISTROS PROCESADOS      : ",cont,"....<ENTER> PARA CONTINUAR"
    FOR CHAR enter

END FUNCTION

{
FUNCTION devolucion_solicitudes()
#ds------------------------------
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)


    LET txt_1 = " SELECT  *  ",
                " FROM  ",tmp_pla_trasp2 CLIPPED
   
    PREPARE pre_2 FROM txt_1
    DECLARE cur_2 CURSOR FOR pre_2
   
    LET cont = 0
    FOREACH cur_2 INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "02"   #---DETALLE DEVOLUCION DE SOLICITUDES---#
            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_devol.tipo_registro      = carga_reg[001,002]
            LET reg_det_devol.cont_servicio      = carga_reg[003,012]
            LET reg_det_devol.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_devol.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_devol.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_devol.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_det_devol.orig_tipo_traspaso = carga_reg[023,024]

            LET c10_fecha                        = carga_reg[029,030],"/",
                                                   carga_reg[031,032],"/",
                                                   carga_reg[025,028]

            LET reg_det_devol.fech_solicitud     = c10_fecha
            LET reg_det_devol.n_unico            = carga_reg[041,058]
            LET reg_det_devol.n_seguro           = carga_reg[059,069] #AFILI
  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_devol.n_seguro IS NULL OR
                   reg_det_devol.n_seguro = " "   OR
                   reg_det_devol.n_seguro = ""    OR
                   reg_det_devol.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_devol.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_devol.n_unico 
                   AND   a.n_seguro[1]="I"

               END IF

  #----Fin de La Modificacion

            LET reg_det_devol.rfc                = carga_reg[085,097]
            LET reg_det_devol.paterno            = carga_reg[098,137]
            LET reg_det_devol.materno            = carga_reg[138,177]
            LET reg_det_devol.nombre             = carga_reg[178,217]
            LET reg_det_devol.cve_sector         = carga_reg[221,221]

            LET c10_fecha                        = carga_reg[236,237],"/",
                                                   carga_reg[238,239],"/",
                                                   carga_reg[232,235]
            LET reg_det_devol.fech_recepc_solic  = c10_fecha

            LET reg_det_devol.ident_lote_solic   = carga_reg[240,255]
            LET reg_det_devol.n_seguro_ent       = carga_reg[271,281] #ICEFA
            LET reg_det_devol.rfc_ent            = carga_reg[282,294]
            LET reg_det_devol.nro_ctrl_icefa     = carga_reg[295,324]
            LET reg_det_devol.nombre_trab_icefa  = carga_reg[325,444]
            LET reg_det_devol.cve_ced_cuenta     = carga_reg[445,447]
            LET reg_det_devol.cod_result_operac  = carga_reg[583,584]
            LET reg_det_devol.diag_proceso       = carga_reg[585,599]
            LET reg_det_devol.nombre_imss        = carga_reg[600,649]
            LET reg_det_devol.status_registro    = carga_reg[703,704]

            INSERT INTO tra_det_devol VALUES(ultimo_folio                  ,
                                             reg_det_devol.*               ,
                                             reg_cza_trasp.consec_lote_dia ,
                                             reg_4.devuelta
                                             )


    LET a_fecha_genera = reg_det_devol.ident_lote_solic[10,11],"/",
                         reg_det_devol.ident_lote_solic[12,13],"/",
                         reg_det_devol.ident_lote_solic[06,09]

            SELECT A.correlativo
	    INTO   v_correlativo
	    FROM   tra_mae_icefa_issste A 
	    WHERE  A.n_seguro        = reg_det_devol.n_seguro
	    AND    A.nss             = reg_det_devol.n_seguro_ent
	    AND    A.rfc             = reg_det_devol.rfc_ent
	    AND    A.icefa_cod       = reg_det_devol.cve_ced_cuenta
	    AND    A.nro_int_cta     = reg_det_devol.nro_ctrl_icefa
	    AND    A.status          = reg_4.enviada
	    AND    A.fecha_genera    = a_fecha_genera

            CALL actualiza_maeicefa(reg_det_devol.n_seguro         ,
                                    reg_det_devol.n_seguro_ent     ,
                                    reg_det_devol.rfc_ent          ,
                                    reg_det_devol.cve_ced_cuenta   ,
                                    reg_det_devol.nro_ctrl_icefa   ,
                                    reg_det_devol.ident_lote_solic ,
                                    reg_4.enviada,
                                    reg_4.devuelta
                                   ) #am


            LET vvvfecha = MDY(reg_cza_devol.fech_presentacion[5,6],
			       reg_cza_devol.fech_presentacion[7,8],
			       reg_cza_devol.fech_presentacion[1,4])
  
           UPDATE tra_mae_icefa_issste
	   SET fecha_solic_tra = vvvfecha
	   WHERE correlativo = v_correlativo

         CALL f_desmarca_cuenta_ic(reg_det_devol.n_seguro,260,v_correlativo,91) 

       
            EXIT CASE

            WHEN "01"    #---ENCABEZADO DEVOLUCION DE SOLICITUDES---#
            LET reg_cza_devol.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_devol.ident_servicio    = carga_reg[003,004]
            LET reg_cza_devol.ident_operacion   = carga_reg[005,006]
            LET reg_cza_devol.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_devol.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_devol.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_devol.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_devol.ent_fed_envio_lote= carga_reg[017,019]
            LET reg_cza_devol.fech_presentacion = carga_reg[020,027]
            LET reg_cza_devol.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_devol.cve_mod_recepcion = carga_reg[031,032]
            LET reg_cza_devol.cod_result_operac = carga_reg[033,034]
            LET reg_cza_devol.rechazo           = carga_reg[035,043]

          
            IF reg_cza_devol.ident_operacion <> "06" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER
		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF


            SELECT "OK"
	         FROM   tra_cza_devol
	         WHERE  fech_presentacion = reg_cza_devol.fech_presentacion
	         AND    consec_lote_dia   = reg_cza_devol.consec_lote_dia
           
         
	         IF STATUS = NOTFOUND THEN
                INSERT INTO tra_cza_devol VALUES(ultimo_folio    ,
                                                 reg_cza_devol.* ,
                                                 reg.nom_archivo
                                                )
           ELSE
                DISPLAY "                                        " AT 18,2
                PROMPT " ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                       "PARA CONTINUAR " FOR CHAR enter
                EXIT PROGRAM
           END IF
           EXIT CASE
           
   
            WHEN "09"   #---SUMARIO DEVOLUCIONES DE SOLICITUDES---#
            LET reg_sum_devol.tipo_registro = carga_reg[001,002]
            LET reg_sum_devol.cant_reg_det  = carga_reg[282,290]

            INSERT INTO tra_sum_devol VALUES(ultimo_folio,reg_sum_devol.*)
        END CASE
    END FOREACH
END FUNCTION

}

FUNCTION traspaso_cuentas()
#tc------------------------
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)

    LET txt_1 = " SELECT  *  ",
                " FROM   ",tmp_pla_trasp2 CLIPPED
   
    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET cont = 0

    FOREACH cur_3 INTO carga_reg

        CASE carga_reg[1,2]

            WHEN "02"   #---DETALLE TRASPASO DE dis_cuenta (SALDOS)---#

            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_trasp_sal.tipo_registro      = carga_reg[001,002]
            LET reg_det_trasp_sal.cont_servicio      = carga_reg[003,012]
            LET reg_det_trasp_sal.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_trasp_sal.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_trasp_sal.tipo_ced_cuenta    = carga_reg[018,019]
            ## De la posision 020,022 no se considera para la carga
            ## ya que trae 001 
            LET reg_det_trasp_sal.orig_tipo_traspaso = carga_reg[023,024]
    
            LET c10_fecha                            = carga_reg[029,030],"/",
                                                       carga_reg[031,032],"/",
                                                       carga_reg[025,028]
            LET reg_det_trasp_sal.fech_presentacion  = c10_fecha
    
            LET c10_fecha                            = carga_reg[037,038],"/",
                                                       carga_reg[039,040],"/",
                                                       carga_reg[033,036]
            LET reg_det_trasp_sal.fech_mov_banxico   = c10_fecha

            LET reg_det_trasp_sal.n_unico            = carga_reg[041,058]
            LET reg_det_trasp_sal.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_trasp_sal.n_seguro IS NULL OR
                   reg_det_trasp_sal.n_seguro = " "   OR
                   reg_det_trasp_sal.n_seguro = ""    OR
                   reg_det_trasp_sal.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_trasp_sal.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_trasp_sal.n_unico
                   AND   a.n_seguro[1] = "I"

               END IF

  #----Fin de La Modificacion

            LET reg_det_trasp_sal.rfc                = carga_reg[085,097]
            LET reg_det_trasp_sal.paterno            = carga_reg[098,137]
            LET reg_det_trasp_sal.materno            = carga_reg[138,177]
            LET reg_det_trasp_sal.nombre             = carga_reg[178,217]
            LET reg_det_trasp_sal.cve_sector         = carga_reg[221,221]

            LET c10_fecha                            = carga_reg[236,237],"/",
                                                       carga_reg[238,239],"/",
                                                       carga_reg[232,235]
            LET reg_det_trasp_sal.fech_recepc_solic  = c10_fecha

            LET reg_det_trasp_sal.ident_lote_solic   = carga_reg[240,255]

            ## --Se agrega el Campo id Procesar--##

            LET reg_det_trasp_sal.id_procesar        = carga_reg[256,263]

            ## --                              --##

            LET reg_det_trasp_sal.n_seguro_ent       = carga_reg[271,281]
            LET reg_det_trasp_sal.rfc_ent            = carga_reg[282,294]
            LET reg_det_trasp_sal.nro_ctrl_icefa     = carga_reg[295,324]
            LET reg_det_trasp_sal.nombre_ent         = carga_reg[325,444]

            ## Este campo va a traer 178
            LET reg_det_trasp_sal.cve_ced_cuenta     = carga_reg[445,447]

            LET reg_det_trasp_sal.saldo_ret_97       = 0 
            LET reg_det_trasp_sal.saldo_ces_vej      = 0
            LET reg_det_trasp_sal.saldo_aport_vol    = 0
            LET reg_det_trasp_sal.saldo_viv_97       = 0
            LET reg_det_trasp_sal.saldo_cuo_soc      = 0

---         SALDOS      ---

            LET c16_saldo_parti                      = carga_reg[515,523],".",
                                                       carga_reg[524,529]
            LET reg_det_trasp_sal.saldo_aport_est    = c16_saldo_parti
  

            LET reg_det_trasp_sal.saldo_cuo_esp      = 0

            LET c16_saldo_sar_92                     = carga_reg[550,562],".",
                                                       carga_reg[563,564]
            LET reg_det_trasp_sal.saldo_sar_92       = c16_saldo_sar_92 

            LET c16_saldo_viv_92                     = carga_reg[565,577],".",
                                                       carga_reg[578,579]
            LET reg_det_trasp_sal.saldo_viv_92       = c16_saldo_viv_92

            LET reg_det_trasp_sal.cod_result_operac  = 0 
            LET reg_det_trasp_sal.diag_proceso       = NULL 
            LET reg_det_trasp_sal.nombre_imss        = NULL

            INSERT INTO tra_det_trasp_sal_issste VALUES(ultimo_folio          ,
                                                 reg_det_trasp_sal.*          ,
                                                 reg_cza_trasp.consec_lote_dia,
                                                 reg_4.aceptada_icefa         ,
                                                 "N"
                                                 )


            LET a_fecha_genera = reg_det_trasp_sal.ident_lote_solic[10,11],"/",
                                 reg_det_trasp_sal.ident_lote_solic[12,13],"/",
                                 reg_det_trasp_sal.ident_lote_solic[06,09]
---- 717 ---->
            IF   reg_det_trasp_sal.id_procesar = " "  THEN
              UPDATE tra_mae_icefa_issste
               SET id_procesar = " "
               WHERE  n_seguro        = reg_det_trasp_sal.n_seguro       
               AND    nss             = reg_det_trasp_sal.n_seguro_ent   #ICE
               AND    rfc             = reg_det_trasp_sal.rfc_ent        #ICE
               AND    icefa_cod       = reg_det_trasp_sal.cve_ced_cuenta #ICE
               AND    status          = reg_4.enviada
               AND    id_procesar     IS NULL
	   --  AND    fecha_genera    = a_fecha_genera
            END IF
---- 717 <----
            SELECT A.correlativo
	    INTO   v_correlativo
	    FROM   tra_mae_icefa_issste A 
	    WHERE  A.n_seguro        = reg_det_trasp_sal.n_seguro       #AFI
	    AND    A.nss             = reg_det_trasp_sal.n_seguro_ent   #ICE
	    AND    A.rfc             = reg_det_trasp_sal.rfc_ent        #ICE
	    AND    A.icefa_cod       = reg_det_trasp_sal.cve_ced_cuenta #ICE
	    AND    A.id_procesar     = reg_det_trasp_sal.id_procesar    #ICE
	    AND    A.status          = reg_4.enviada
	--  AND    A.fecha_genera    = a_fecha_genera

            UPDATE tra_mae_icefa_issste
            SET    fecha_solic_tra   =         reg_cza_trasp.fech_presentacion
            WHERE  correlativo       =         v_correlativo


            CALL actualiza_maeicefa(reg_det_trasp_sal.n_seguro         ,#AFI
                                    reg_det_trasp_sal.n_seguro_ent     ,#ICE
                                    reg_det_trasp_sal.rfc_ent          ,#ICE
                                    reg_det_trasp_sal.cve_ced_cuenta   ,#ICE
                                    reg_det_trasp_sal.nro_ctrl_icefa   ,#ICE
                                    reg_det_trasp_sal.id_procesar      ,#ICE
                                    reg_det_trasp_sal.ident_lote_solic ,
                                    reg_4.enviada                      ,
                                    reg_4.aceptada_icefa
                                  ) #am

EXIT CASE

            WHEN "03"   #---DETALLE TRASPASO DE dis_cuenta (INTERESES)---#
            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_trasp_int.tipo_registro        = carga_reg[001,002]
            LET reg_det_trasp_int.cont_servicio        = carga_reg[003,012]
            LET reg_det_trasp_int.tipo_recep_cuenta    = carga_reg[013,014]
            LET reg_det_trasp_int.cve_recep_cuenta     = carga_reg[015,017]
            LET reg_det_trasp_int.tipo_ced_cuenta      = carga_reg[018,019]
            ## De la posision 020,022 no se considera para la carga
            ## ya que trae 001 
            LET reg_det_trasp_int.orig_tipo_traspaso   = carga_reg[023,024]

            LET c10_fecha                              = carga_reg[029,030],"/",
                                                         carga_reg[031,032],"/",
                                                         carga_reg[025,028]
            LET reg_det_trasp_int.fech_presentacion    = c10_fecha

            LET c10_fecha                              = carga_reg[037,038],"/",
                                                         carga_reg[039,040],"/",
                                                         carga_reg[033,036]
            LET reg_det_trasp_int.fech_mov_banxico     = c10_fecha

            LET reg_det_trasp_int.n_unico              = carga_reg[041,058]
            LET reg_det_trasp_int.n_seguro             = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_trasp_int.n_seguro IS NULL OR
                   reg_det_trasp_int.n_seguro = " "   OR
                   reg_det_trasp_int.n_seguro = ""    OR
                   reg_det_trasp_int.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_trasp_int.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_trasp_int.n_unico 
                   AND   a.n_seguro[1] = "I"

               END IF

  #----Fin de La Modificacion

            LET reg_det_trasp_int.rfc_afo_recep        = carga_reg[085,097]
            LET reg_det_trasp_int.paterno_afo_recep    = carga_reg[098,137]
            LET reg_det_trasp_int.materno_afo_recep    = carga_reg[138,177]
            LET reg_det_trasp_int.nombres_afo_recep    = carga_reg[178,217]
            LET reg_det_trasp_int.cve_sector           = carga_reg[221,221]

            LET c10_fecha                              = carga_reg[236,237],"/",
                                                         carga_reg[238,239],"/",
                                                         carga_reg[232,235]
            LET reg_det_trasp_int.fech_recep_solic     = c10_fecha

            LET reg_det_trasp_int.ident_lote_solic     = carga_reg[240,255]

            ## --Se agrega el Campo id Procesar--##

            LET reg_det_trasp_int.id_procesar        = carga_reg[256,263]

            ## --                              --##

            LET reg_det_trasp_int.n_seguro_ent_ced     = carga_reg[271,281]
            LET reg_det_trasp_int.rfc_ent_ced          = carga_reg[282,294]
            LET reg_det_trasp_int.nro_ctrl_icefa       = carga_reg[295,324]
            LET reg_det_trasp_int.nombre_trab_icefa    = carga_reg[325,444]
            LET reg_det_trasp_int.cve_ced_cuenta       = carga_reg[445,447]
--- SALDOS ---

            LET reg_det_trasp_int.int_ces_vej          = 0
            LET reg_det_trasp_int.int_aport_vol        = 0
            LET reg_det_trasp_int.int_viv_97           = 0
            LET reg_det_trasp_int.int_cuo_soc          = 0
            LET reg_det_trasp_int.int_aport_est        = 0

            LET reg_det_trasp_int.int_cuo_esp          = 0

            LET c16_int_sar_92                         = carga_reg[550,562],".",
                                                         carga_reg[563,564]
            LET reg_det_trasp_int.int_sar_92           = c16_int_sar_92
       
            LET reg_det_trasp_int.int_viv_92           = 0

            LET reg_det_trasp_int.nombre_imss          = NULL

            INSERT INTO tra_det_trasp_int_issste VALUES(ultimo_folio          ,
                                                 reg_det_trasp_int.*          ,
                                                 reg_cza_trasp.consec_lote_dia,
                                                 reg_4.aceptada_icefa         ,
                                                 "N"
                                                 )

---- 717 ---->
            IF   reg_det_trasp_int.id_procesar = " "  THEN
              UPDATE tra_mae_icefa_issste
               SET id_procesar = " "
               WHERE  n_seguro        = reg_det_trasp_int.n_seguro         #AFI
               AND    nss             = reg_det_trasp_int.n_seguro_ent_ced #ICE
               AND    rfc             = reg_det_trasp_int.rfc_ent_ced      #ICE
               AND    icefa_cod       = reg_det_trasp_int.cve_ced_cuenta   #ICE
               AND    id_procesar     IS NULL                                  
               AND    status          = reg_4.enviada
            END IF
---- 717 <----

            SELECT correlativo
	    INTO   v_correlativo
	    FROM   tra_mae_icefa_issste
	    WHERE  n_seguro        = reg_det_trasp_int.n_seguro         #AFI
	    AND    nss             = reg_det_trasp_int.n_seguro_ent_ced #ICE 
	    AND    rfc             = reg_det_trasp_int.rfc_ent_ced      #ICE
	    AND    icefa_cod       = reg_det_trasp_int.cve_ced_cuenta   #ICE
	    AND    id_procesar     = reg_det_trasp_int.id_procesar      #ICE
	    AND    status          = reg_4.enviada

            UPDATE tra_mae_icefa_issste
	    SET fecha_solic_tra    = reg_cza_trasp.fech_presentacion 
	    WHERE correlativo      = v_correlativo

            EXIT CASE

            WHEN "01"    #---ENCABEZADO TRASPASO DE CUENTAS---#
            LET reg_cza_trasp.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_trasp.ident_servicio    = carga_reg[003,004]
            LET reg_cza_trasp.ident_operacion   = carga_reg[005,006]
            LET reg_cza_trasp.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_trasp.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_trasp.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_trasp.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_trasp.ent_fed_envio_lote= carga_reg[017,019]

            LET c10_fecha                       = carga_reg[024,025],"/",
                                                  carga_reg[026,027],"/",
                                                  carga_reg[020,023]
            LET reg_cza_trasp.fech_presentacion = c10_fecha

            LET reg_cza_trasp.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_trasp.cve_mod_recepcion = carga_reg[031,032]

            LET reg_cza_trasp.cod_result_operac = 0                            
            LET reg_cza_trasp.rechazo           = 0 

            IF reg_cza_trasp.ident_operacion <> "09" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ", 
		      "PARA CONTINUAR "FOR CHAR ENTER 
		DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE
              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF

            SELECT "OK"
	         FROM   tra_cza_trasp_issste
	         WHERE  fech_presentacion = reg_cza_trasp.fech_presentacion
	         AND    consec_lote_dia   = reg_cza_trasp.consec_lote_dia

	    IF STATUS = NOTFOUND THEN
                INSERT INTO tra_cza_trasp_issste VALUES(ultimo_folio         ,
                                             reg_cza_trasp.*      ,
                                             reg_4.aceptada_icefa ,#estado
                                             "N"             ,#tipo_icefa
                                             reg.nom_archivo  #nom_archivo
                                            )
            ELSE
                PROMPT "ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                       "PARA CONTINUAR " FOR CHAR enter
                EXIT PROGRAM
            END IF

            EXIT CASE

            WHEN "09"   #---SUMARIO DE TRASPASO DE CUENTAS---#
         #1  id 1 Tipo de Registro
            LET reg_sum_trasp.tipo_registro     = carga_reg[001,002]

         #2  id 2 Cantidad de Registros de detalle
            LET reg_sum_trasp.cant_reg_det      = carga_reg[003,011]

            LET c_16_saldo                       = carga_reg[117,129],".",
                                                   carga_reg[130,131]
         #3  id 4 Total Saldo Ahorro para el Retiro (origen 61 )
            LET reg_sum_trasp.tot_sal_sar_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[132,144],".",
                                                   carga_reg[145,146]
         #4  id 5 Total Saldo Fondo de Vivienda 92 (origen 61)
            LET reg_sum_trasp.tot_sal_viv_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[162,170],".",
                                                   carga_reg[171,176]

         # 5 id 7 Tot de Aplic de Int de Fondo de Viv 92 (origen 61)
            LET reg_sum_trasp.tot_part_viv_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[267,279],".",
                                                   carga_reg[280,281]

         # 6 id 9 Tot de Int de Ahorro para el Retiro(origen 61)
            LET reg_sum_trasp.int_sal_sar_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[292,304],".",
                                                   carga_reg[305,306]

         # 7 id 11 Tot sdo Ahorro para el ret(origen 62)
            LET reg_sum_trasp.tot_sal_sar_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[307,319],".",
                                                   carga_reg[320,321]

        # 8 id 12 tot sdo fondo de viv 92(origen 62)

            LET reg_sum_trasp.tot_sal_viv_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[332,340],".",
                                                   carga_reg[341,346]

         # 9 id 14 tot de aplic Int de fondo Viv 92(origen 62)

            LET reg_sum_trasp.tot_part_viv_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[357,369],".",
                                                   carga_reg[370,371]

         # 10 id Tot de Int de Ahorro para el retiro (origen 62)

            LET reg_sum_trasp.int_sal_sar_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[382,394],".",
                                                   carga_reg[395,396]
        # 11 id Tot sdo ahorro para el ret(origen 63)

            LET reg_sum_trasp.tot_sal_sar_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[397,409],".",
                                                   carga_reg[410,411]

        # 12 id tot sdo fondo de viv 92 (origen 63)
            LET reg_sum_trasp.tot_sal_viv_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[422,430],".",
                                                   carga_reg[431,436]

        # 13 id tot de aplic de int fondo de viv 92(origen 63(
            LET reg_sum_trasp.tot_part_viv_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[447,459],".",
                                                   carga_reg[460,461]

        # 14 id tot de int de Ahorro para el retiro (origen(63)
            LET reg_sum_trasp.int_sal_sar_o63 = c_16_saldo

#-- Hasta Aqui van a venir Datos ,apartir del id 24 pocision de la 462 a la
#-- 730 Vendra filler, de cualquier forma aunque deje estos campos no tiene
#---que afectar el proceso los cargaría en BLANCO.

            LET c_16_saldo                       = carga_reg[472,484],".",
                                                   carga_reg[485,486]
 
        # 15
            LET reg_sum_trasp.tot_sal_sar_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[487,499],".",
                                                   carga_reg[500,501]

        # 16
            LET reg_sum_trasp.tot_sal_viv_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[512,520],".",
                                                   carga_reg[521,526]

        # 17
            LET reg_sum_trasp.tot_part_viv_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[537,549],".",
                                                   carga_reg[550,551]

        # 18
            LET reg_sum_trasp.int_sal_sar_o65 = c_16_saldo

            CASE s_codigo_afore
               
                WHEN   564  #AFORE METLIFE 

                   INSERT INTO tra_sum_trasp_issste VALUES(ultimo_folio    ,
                                                           reg_sum_trasp.* ,
                                                            "N"
                                                            )
                OTHERWISE   #DEMAS  AFORES

                   INSERT INTO tra_sum_trasp_uissste VALUES(ultimo_folio    ,
                                                            reg_sum_trasp.* ,
                                                            "N"
                                                             )
           END CASE
        
        END CASE

    END FOREACH

END FUNCTION

 
FUNCTION traspasos_complement()
#tcp---------------------------
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)



    LET txt_1 = " SELECT  *  ",
                " FROM   ",tmp_pla_trasp2 CLIPPED
   
    PREPARE pre_4 FROM txt_1
    DECLARE cur_4 CURSOR FOR pre_4
   
    LET cont = 0

    FOREACH cur_4 INTO carga_reg

        CASE carga_reg[1,2]

            WHEN "02"   #---DETALLE DE TRASPASOS COMPLEMENTARIOS (SALDOS)---#

            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_trasp_sal.tipo_registro      = carga_reg[001,002]
            LET reg_det_trasp_sal.cont_servicio      = carga_reg[003,012]
            LET reg_det_trasp_sal.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_trasp_sal.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_trasp_sal.tipo_ced_cuenta    = carga_reg[018,019]
            ## De la posision 020,022 no se considera para la carga
            ## ya que trae 001 
            LET reg_det_trasp_sal.orig_tipo_traspaso = carga_reg[023,024]

            LET c10_fecha                            = carga_reg[029,030],"/",
                                                       carga_reg[031,032],"/",
                                                       carga_reg[025,028]
            LET reg_det_trasp_sal.fech_presentacion  = c10_fecha

            LET c10_fecha                            = carga_reg[037,038],"/",
                                                       carga_reg[039,040],"/",
                                                       carga_reg[033,036]
            LET reg_det_trasp_sal.fech_mov_banxico   = c10_fecha

            LET reg_det_trasp_sal.n_unico            = carga_reg[041,058]
            LET reg_det_trasp_sal.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_trasp_sal.n_seguro IS NULL OR
                   reg_det_trasp_sal.n_seguro = " "   OR
                   reg_det_trasp_sal.n_seguro = ""    OR
                   reg_det_trasp_sal.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_trasp_sal.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_trasp_sal.n_unico
                   AND   a.n_seguro[1] = "I"

               END IF

  #----Fin de La Modificacion

            LET reg_det_trasp_sal.rfc                = carga_reg[085,097]
            LET reg_det_trasp_sal.paterno            = carga_reg[098,137]
            LET reg_det_trasp_sal.materno            = carga_reg[138,177]
            LET reg_det_trasp_sal.nombre             = carga_reg[178,217]
            LET reg_det_trasp_sal.cve_sector         = carga_reg[221,221]

            LET c10_fecha                            = carga_reg[236,237],"/",
                                                       carga_reg[238,239],"/",
                                                       carga_reg[232,235]
            LET reg_det_trasp_sal.fech_recepc_solic  = c10_fecha

            LET reg_det_trasp_sal.ident_lote_solic   = carga_reg[240,255]

            ## --Se agrega el Campo id Procesar--##

            LET reg_det_trasp_sal.id_procesar        = carga_reg[256,263]

            ## --                              --## 

            LET reg_det_trasp_sal.n_seguro_ent       = carga_reg[271,281]
            LET reg_det_trasp_sal.rfc_ent            = carga_reg[282,294]
            LET reg_det_trasp_sal.nro_ctrl_icefa     = carga_reg[295,324]
            LET reg_det_trasp_sal.nombre_ent         = carga_reg[325,444]
     
            ## Este campo va a traer 178
            LET reg_det_trasp_sal.cve_ced_cuenta     = carga_reg[445,447]
                 
                        
            LET reg_det_trasp_sal.saldo_ret_97       = 0
            LET reg_det_trasp_sal.saldo_ces_vej      = 0
            LET reg_det_trasp_sal.saldo_aport_vol    = 0
            LET reg_det_trasp_sal.saldo_viv_97       = 0
            LET reg_det_trasp_sal.saldo_cuo_soc      = 0

           ---         SALDOS      ---

            LET c16_saldo_parti                      = carga_reg[515,523],".",
                                                       carga_reg[524,529]
            LET reg_det_trasp_sal.saldo_aport_est    = c16_saldo_parti

            LET reg_det_trasp_sal.saldo_cuo_esp      = 0

            LET c16_saldo_sar_92                     = carga_reg[550,562],".",
                                                       carga_reg[563,564]
            LET reg_det_trasp_sal.saldo_sar_92       = c16_saldo_sar_92 

            LET c16_saldo_viv_92                     = carga_reg[565,577],".",
                                                       carga_reg[578,579]
            LET reg_det_trasp_sal.saldo_viv_92       = c16_saldo_viv_92

            LET reg_det_trasp_sal.cod_result_operac  = 0
            LET reg_det_trasp_sal.diag_proceso       = NULL
            LET reg_det_trasp_sal.nombre_imss        = NULL 

            INSERT INTO tra_det_trasp_sal_issste VALUES(ultimo_folio       ,
                                             reg_det_trasp_sal.*           ,
                                             reg_cza_trasp.consec_lote_dia ,
                                             reg_4.complementarios         ,
                                             "C"
                                            )
            EXIT CASE

            WHEN "03"   #---DETALLE DE TRASPASOS COMPLEMENTARIOS (INTERESES)---#
            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_trasp_int.tipo_registro        = carga_reg[001,002]
            LET reg_det_trasp_int.cont_servicio        = carga_reg[003,012]
            LET reg_det_trasp_int.tipo_recep_cuenta    = carga_reg[013,014]
            LET reg_det_trasp_int.cve_recep_cuenta     = carga_reg[015,017]
            LET reg_det_trasp_int.tipo_ced_cuenta      = carga_reg[018,019]
            ## De la posision 020,022 no se considera para la carga
            ## ya que trae 001 
            LET reg_det_trasp_int.orig_tipo_traspaso   = carga_reg[023,024]

            LET c10_fecha                              = carga_reg[029,030],"/",
                                                         carga_reg[031,032],"/",
                                                         carga_reg[025,028]
            LET reg_det_trasp_int.fech_presentacion    = c10_fecha

            LET c10_fecha                              = carga_reg[037,038],"/",
                                                         carga_reg[039,040],"/",
                                                         carga_reg[033,036]
            LET reg_det_trasp_int.fech_mov_banxico     = c10_fecha

            LET reg_det_trasp_int.n_unico              = carga_reg[041,058]
            LET reg_det_trasp_int.n_seguro             = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_trasp_int.n_seguro IS NULL OR
                   reg_det_trasp_int.n_seguro = " "   OR
                   reg_det_trasp_int.n_seguro = ""    OR
                   reg_det_trasp_int.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_trasp_int.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_trasp_int.n_unico
                   AND   a.n_seguro[1] = "I"                    


               END IF

  #----Fin de La Modificacion

            LET reg_det_trasp_int.rfc_afo_recep        = carga_reg[085,097]
            LET reg_det_trasp_int.paterno_afo_recep    = carga_reg[098,137]
            LET reg_det_trasp_int.materno_afo_recep    = carga_reg[138,177]
            LET reg_det_trasp_int.nombres_afo_recep    = carga_reg[178,217]
            LET reg_det_trasp_int.cve_sector           = carga_reg[221,221]

            LET c10_fecha                              = carga_reg[236,237],"/",
                                                         carga_reg[238,239],"/",
                                                         carga_reg[232,235]
            LET reg_det_trasp_int.fech_recep_solic     = c10_fecha

            LET reg_det_trasp_int.ident_lote_solic     = carga_reg[240,255]

            ## --Se agrega el Campo id Procesar--##

            LET reg_det_trasp_int.id_procesar        = carga_reg[256,263]

            ## --                              --##
   
            LET reg_det_trasp_int.n_seguro_ent_ced     = carga_reg[271,281]
            LET reg_det_trasp_int.rfc_ent_ced          = carga_reg[282,294]
            LET reg_det_trasp_int.nro_ctrl_icefa       = carga_reg[295,324]
            LET reg_det_trasp_int.nombre_trab_icefa    = carga_reg[325,444]
            LET reg_det_trasp_int.cve_ced_cuenta       = carga_reg[445,447]
--- SALDOS ---
           
           LET reg_det_trasp_int.int_ces_vej          = 0
           LET reg_det_trasp_int.int_aport_vol        = 0
           LET reg_det_trasp_int.int_viv_97           = 0
           LET reg_det_trasp_int.int_cuo_soc          = 0
           LET reg_det_trasp_int.int_aport_est        = 0
           LET reg_det_trasp_int.int_cuo_esp          = 0 

            LET c16_int_sar_92                         = carga_reg[550,562],".",
                                                         carga_reg[563,564]
            LET reg_det_trasp_int.int_sar_92           = c16_int_sar_92

            LET reg_det_trasp_int.int_viv_92           = 0
            LET reg_det_trasp_int.nombre_imss          =NULL 

            INSERT INTO tra_det_trasp_int_issste VALUES(ultimo_folio       ,
                                             reg_det_trasp_int.*           ,
                                             reg_cza_trasp.consec_lote_dia ,
                                             reg_4.complementarios         ,
                                             "C"
                                            )
            EXIT CASE

            WHEN "01"    #---ENCABEZADO TRASPASOS COMPLEMENTARIOS---#
                LET reg_cza_trasp.tipo_registro     = carga_reg[001,002] 
                LET reg_cza_trasp.ident_servicio    = carga_reg[003,004]
                LET reg_cza_trasp.ident_operacion   = carga_reg[005,006]
                LET reg_cza_trasp.tipo_ent_origen   = carga_reg[007,008]
                LET reg_cza_trasp.cve_ent_origen    = carga_reg[009,011]
                LET reg_cza_trasp.tipo_ent_destino  = carga_reg[012,013]
                LET reg_cza_trasp.cve_ent_destino   = carga_reg[014,016]
                LET reg_cza_trasp.ent_fed_envio_lote= carga_reg[017,019]

                LET c10_fecha                       = carga_reg[024,025],"/",
                                                      carga_reg[026,027],"/",
                                                      carga_reg[020,023]
                LET reg_cza_trasp.fech_presentacion = c10_fecha

                LET reg_cza_trasp.consec_lote_dia   = carga_reg[028,030]
                LET reg_cza_trasp.cve_mod_recepcion = carga_reg[031,032]

                LET reg_cza_trasp.cod_result_operac = 0                  
                LET reg_cza_trasp.rechazo           = 0                  


            IF reg_cza_trasp.ident_operacion <> "12" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM

            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF
                SELECT "OK"
	        FROM   tra_cza_trasp_issste
	        WHERE  fech_presentacion = reg_cza_trasp.fech_presentacion
	        AND    consec_lote_dia   = reg_cza_trasp.consec_lote_dia

	        IF STATUS = NOTFOUND THEN
               
                   INSERT INTO tra_cza_trasp_issste 
                           VALUES(ultimo_folio          ,
                                  reg_cza_trasp.*       ,
                                  reg_4.complementarios ,
                                  "C"                   ,#tipo_icefa
                                  reg.nom_archivo        #nom_archivo
                                 )
              
               ELSE
                    PROMPT "ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                           "PARA CONTINUAR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
             
                EXIT CASE

            WHEN "09"   #---SUMARIO DE TRASPASO DE CUENTAS COMPLEMENTARIOS---#
        #1  id 1 Tipo de Registro
            LET reg_sum_trasp.tipo_registro     = carga_reg[001,002]

        #2  id 2 Cantidad de Registros de detalle 
            LET reg_sum_trasp.cant_reg_det      = carga_reg[003,011]

            LET c_16_saldo                       = carga_reg[117,129],".",
                                                   carga_reg[130,131]

        #3  id 4 Total Saldo Ahorro para el Retiro (origen 61 )
            LET reg_sum_trasp.tot_sal_sar_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[132,144],".",
                                                   carga_reg[145,146]

        #4  id 5 Total Saldo Fondo de Vivienda 92 (origen 61)
            LET reg_sum_trasp.tot_sal_viv_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[162,170],".",
                                                   carga_reg[171,176]

        # 5 id 7 Tot de Aplic de Int de Fondo de Viv 92 (origen 61)
            LET reg_sum_trasp.tot_part_viv_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[267,279],".",
                                                   carga_reg[280,281]
      
        # 6 id 9 Tot de Int de Ahorro para el Retiro(origen 61)
            LET reg_sum_trasp.int_sal_sar_o61 = c_16_saldo

            LET c_16_saldo                       = carga_reg[292,304],".",
                                                   carga_reg[305,306]

        # 7 id 11 Tot sdo Ahorro para el ret(origen 62)
            LET reg_sum_trasp.tot_sal_sar_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[307,319],".",
                                                   carga_reg[320,321]

        # 8 id 12 tot sdo fondo de viv 92(origen 62) 
            LET reg_sum_trasp.tot_sal_viv_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[332,340],".",

                                                   carga_reg[341,346]

         # 9 id 14 tot de aplic Int de fondo Viv 92(origen 62)

            LET reg_sum_trasp.tot_part_viv_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[357,369],".",
                                                   carga_reg[370,371]

        # 10 id Tot de Int de Ahorro para el retiro (origen 62)

            LET reg_sum_trasp.int_sal_sar_o62 = c_16_saldo

            LET c_16_saldo                       = carga_reg[382,394],".",
                                                   carga_reg[395,396]

        # 11 id Tot sdo ahorro para el ret(origen 63)

            LET reg_sum_trasp.tot_sal_sar_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[397,409],".",
                                                   carga_reg[410,411]
         
        # 12 id tot sdo fondo de viv 92 (origen 63)
	
            LET reg_sum_trasp.tot_sal_viv_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[422,430],".",
                                                   carga_reg[431,436]
          
        # 13 id tot de aplic de int fondo de viv 92(origen 63) 
            LET reg_sum_trasp.tot_part_viv_o63 = c_16_saldo

            LET c_16_saldo                       = carga_reg[447,459],".",
                                                   carga_reg[460,461]
          
        # 14 id tot de int de Ahorro para el retiro (origen(63)
            LET reg_sum_trasp.int_sal_sar_o63 = c_16_saldo

#-- Hasta Aqui van a venir Datos ,apartir del id 24 pocision de la 462 a la
#-- 730 Vendra filler, de cualquier forma aunque deje estos campos no tiene
#---que afectar el proceso los cargaría en BLANCO.

            LET c_16_saldo                       = carga_reg[472,484],".",
                                                   carga_reg[485,486]
           
        # 15
            LET reg_sum_trasp.tot_sal_sar_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[487,499],".",
                                                   carga_reg[500,501]
        # 16
            LET reg_sum_trasp.tot_sal_viv_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[512,520],".",
                                                   carga_reg[521,526]

        # 17
            LET reg_sum_trasp.tot_part_viv_o65 = c_16_saldo

            LET c_16_saldo                       = carga_reg[537,549],".",
                                                   carga_reg[550,551]
        # 18
            LET reg_sum_trasp.int_sal_sar_o65 = c_16_saldo

            CASE s_codigo_afore
               
               WHEN   564  #AFORE METLIFE
                  
                  INSERT INTO tra_sum_trasp_issste VALUES(ultimo_folio    ,
                                                          reg_sum_trasp.* ,
                                                          "C"
                                                          )
               OTHERWISE   #DEMAS  AFORES
 
                  INSERT INTO tra_sum_trasp_uissste VALUES(ultimo_folio    ,
                                                           reg_sum_trasp.* ,
                                                          "C"
                                                           )
           END CASE

        END CASE

    END FOREACH

END FUNCTION

{
FUNCTION solic_no_atendidas()
#sna-------------------------
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)



    LET txt_1 = " SELECT * ",
                " FROM  ",tmp_pla_trasp2 CLIPPED

    PREPARE pre_5 FROM txt_1
    DECLARE cur_5 CURSOR FOR pre_5
   
    LET cont = 0
    FOREACH cur_5 INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "02"   #---DETALLE DE SOLICITUDES NO ATENDIDAS---#
            LET cont = cont + 1
            DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

            LET reg_det_sol_no_ate.tipo_registro      = carga_reg[001,002]
            LET reg_det_sol_no_ate.cont_servicio      = carga_reg[003,012]
            LET reg_det_sol_no_ate.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_sol_no_ate.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_sol_no_ate.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_sol_no_ate.orig_tipo_traspaso = carga_reg[023,024]

            LET c10_fecha                             = carga_reg[029,030],"/",
                                                        carga_reg[031,032],"/",
                                                        carga_reg[025,028]
            LET reg_det_sol_no_ate.fech_presentacion  = c10_fecha

            LET reg_det_sol_no_ate.n_unico            = carga_reg[041,058]
            LET reg_det_sol_no_ate.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_sol_no_ate.n_seguro IS NULL OR
                   reg_det_sol_no_ate.n_seguro = " "   OR
                   reg_det_sol_no_ate.n_seguro = ""    OR
                   reg_det_sol_no_ate.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_sol_no_ate.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_sol_no_ate.n_unico
                   AND   a.n_seguro[1]="I"


               END IF

  #----Fin de La Modificacion

            LET reg_det_sol_no_ate.rfc                = carga_reg[085,097]
            LET reg_det_sol_no_ate.paterno            = carga_reg[098,137]
            LET reg_det_sol_no_ate.materno            = carga_reg[138,177]
            LET reg_det_sol_no_ate.nombres            = carga_reg[178,217]

            LET c10_fecha                             = carga_reg[236,237],"/",
                                                        carga_reg[238,239],"/",
                                                        carga_reg[232,235]
            LET reg_det_sol_no_ate.fech_recep_solic   = c10_fecha

            LET reg_det_sol_no_ate.ident_lote_solic   = carga_reg[240,255]
            LET reg_det_sol_no_ate.n_seguro_ent_ced   = carga_reg[271,281]
            LET reg_det_sol_no_ate.rfc_ent_ced        = carga_reg[282,294]
            LET reg_det_sol_no_ate.nro_ctrl_icefa     = carga_reg[295,324]
            LET reg_det_sol_no_ate.nom_comp_tra_icefa = carga_reg[325,444]
            LET reg_det_sol_no_ate.cve_ced_cuenta     = carga_reg[445,447]

            INSERT INTO tra_det_no_aten VALUES(ultimo_folio,
                                             reg_det_sol_no_ate.*,
                                             reg_cza_sol_no_ate.consec_lote_dia,
                                             reg_4.no_atendida 
                                            )

            SELECT A.correlativo
	    INTO   v_correlativo
	    FROM   tra_mae_icefa_issste A
	    WHERE  A.n_seguro        = reg_det_sol_no_ate.n_seguro
	    AND    A.nss             = reg_det_sol_no_ate.n_seguro_ent_ced
	    AND    A.rfc             = reg_det_sol_no_ate.rfc_ent_ced
	    AND    A.icefa_cod       = reg_det_sol_no_ate.cve_ced_cuenta
	    AND    A.nro_int_cta     = reg_det_sol_no_ate.nro_ctrl_icefa
	    AND    A.status          = reg_4.enviada

            IF reg_det_sol_no_ate.orig_tipo_traspaso = "11" THEN
                CALL actualiza_maeicefa(reg_det_sol_no_ate.n_seguro         ,
                                        reg_det_sol_no_ate.n_seguro_ent_ced ,
                                        reg_det_sol_no_ate.rfc_ent_ced      ,
                                        reg_det_sol_no_ate.cve_ced_cuenta   ,
                                        reg_det_sol_no_ate.nro_ctrl_icefa   ,
                                        reg_det_sol_no_ate.ident_lote_solic ,
                                        reg_4.enviada,
                                        reg_4.no_atendida
                                       ) #am
            END IF 

            IF reg_det_sol_no_ate.orig_tipo_traspaso = "10" THEN
                CALL actualiza_maeicefa(reg_det_sol_no_ate.n_seguro         ,
                                        reg_det_sol_no_ate.n_seguro_ent_ced ,
                                        reg_det_sol_no_ate.rfc_ent_ced      ,
                                        reg_det_sol_no_ate.cve_ced_cuenta   ,
                                        reg_det_sol_no_ate.nro_ctrl_icefa   ,
                                        reg_det_sol_no_ate.ident_lote_solic ,
                                        reg_4.enviada ,
                                        reg_4.liberada_icefa
                                       ) #am
            END IF 


            LET vvvfecha = MDY(reg_cza_sol_no_ate.fech_presentacion[5,6],
			       reg_cza_sol_no_ate.fech_presentacion[7,8],
			       reg_cza_sol_no_ate.fech_presentacion[1,4])
  
           UPDATE tra_mae_icefa_issste
	   SET fecha_solic_tra = vvvfecha
	   WHERE correlativo = v_correlativo

           
       CALL f_desmarca_cuenta_ic(reg_det_sol_no_ate.n_seguro,260,v_correlativo,92)

            EXIT CASE

            WHEN "01"    #---ENCABEZADO DE SOLICITUDES NO ATENDIDAS---#
            LET reg_cza_sol_no_ate.tipo_registro     = carga_reg[001,002]
            LET reg_cza_sol_no_ate.ident_servicio    = carga_reg[003,004]
            LET reg_cza_sol_no_ate.ident_operacion   = carga_reg[005,006]
            LET reg_cza_sol_no_ate.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_sol_no_ate.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_sol_no_ate.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_sol_no_ate.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_sol_no_ate.fech_presentacion = carga_reg[020,027]
            LET reg_cza_sol_no_ate.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_sol_no_ate.cve_mod_recepcion = carga_reg[031,032]

            IF reg_cza_sol_no_ate.ident_operacion <> "14" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF


            SELECT "OK"
	    FROM   tra_cza_sol_no_ate
	    WHERE  fech_presentacion = reg_cza_sol_no_ate.fech_presentacion
	    AND    consec_lote_dia   = reg_cza_sol_no_ate.consec_lote_dia

	    IF STATUS = NOTFOUND THEN
                INSERT INTO tra_cza_sol_no_ate VALUES(ultimo_folio         ,
                                                  reg_cza_sol_no_ate.* ,
                                                  reg.nom_archivo
                                                 )
            ELSE
                PROMPT "ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                       "PARA CONTINUAR " FOR CHAR enter
                EXIT PROGRAM
            END IF
            EXIT CASE

            WHEN "09"   #---SUMARIO DE SOLICITUDES NO ATENDIDAS---#
            LET reg_sum_sol_no_ate.tipo_registro      = carga_reg[001,002]
            LET reg_sum_sol_no_ate.cantidad_reg_det   = carga_reg[003,011]

            INSERT INTO tra_sum_no_aten VALUES(ultimo_folio ,
                                              reg_sum_sol_no_ate.*
                                             )
        END CASE
    END FOREACH
END FUNCTION

}
 
FUNCTION solic_rechazadas()
#sr------------------------
    DEFINE reg_2 RECORD
        nss                   CHAR(11) ,
        rfc                   CHAR(13) ,
        icefa_cod             SMALLINT ,
        nro_int_cta           CHAR(30)
    END RECORD

    DEFINE reg_paso RECORD
        fecha_genera          CHAR(010)
    END RECORD             

    DEFINE 
        carga_reg             CHAR(730) ,
        c2_carga_reg          CHAR(002)


    LET txt_1 = " SELECT * ",
                " FROM ",tmp_pla_trasp2 CLIPPED

    PREPARE pre_7 FROM txt_1
    DECLARE cur_7 CURSOR FOR pre_7
   
    LET cont = 0

    FOREACH cur_7 INTO carga_reg

        CASE carga_reg[1,2]

#-- EL layout es identico a la de la Op 01 envio de Solicitudes---

            WHEN "02"   #---DETALLE DE SOLICITUDES RECHAZADAS---#
                LET cont = cont + 1
                DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40
                LET reg_det_sol_rech.tipo_registro        = carga_reg[001,002]
                LET reg_det_sol_rech.cont_servicio        = carga_reg[003,012]
                LET reg_det_sol_rech.tipo_recep_cuenta    = carga_reg[013,014]
                LET reg_det_sol_rech.cve_recep_cuenta     = carga_reg[015,017]
                LET reg_det_sol_rech.tipo_ced_cuenta      = carga_reg[018,019]
                ## De la posision 020,022 no se considera para la carga
                ## ya que trae 001 

                LET reg_det_sol_rech.orig_tipo_trasp      = carga_reg[023,024]

                LET c10_fecha                        = carga_reg[029,030],"/",
                                                       carga_reg[031,032],"/",
                                                       carga_reg[025,028]
                LET reg_det_sol_rech.fech_presentacion    = c10_fecha

                LET reg_det_sol_rech.n_unico              = carga_reg[041,058]
                LET reg_det_sol_rech.n_seguro             = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_sol_rech.n_seguro IS NULL OR
                   reg_det_sol_rech.n_seguro = " "   OR
                   reg_det_sol_rech.n_seguro = ""    OR
                   reg_det_sol_rech.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_sol_rech.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_sol_rech.n_unico
                   AND   a.n_seguro[1] = "I"

               END IF

  #----Fin de La Modificacion

                LET reg_det_sol_rech.rfc                  = carga_reg[085,097]
                LET reg_det_sol_rech.paterno              = carga_reg[098,137]
                LET reg_det_sol_rech.materno              = carga_reg[138,177]
                LET reg_det_sol_rech.nombres              = carga_reg[178,217]
                LET reg_det_sol_rech.cve_sector           = carga_reg[221,221]

                LET c10_fecha                          = carga_reg[236,237],"/",
                                                         carga_reg[238,239],"/",
                                                         carga_reg[232,235]
                LET reg_det_sol_rech.fech_recep_solic     = c10_fecha

                LET reg_det_sol_rech.ident_lote_solic     = carga_reg[240,255]

                ## --Se agrega el Campo id Procesar--##
               
                LET reg_det_sol_rech.id_procesar        = carga_reg[256,263]
               
                ## --                              --##

                LET reg_det_sol_rech.n_seguro_ent         = carga_reg[271,281]
                LET reg_det_sol_rech.rfc_ent              = carga_reg[282,294]
                LET reg_det_sol_rech.nro_ctrl_icefa       = carga_reg[295,324]
                LET reg_det_sol_rech.nombre_trab_icefa    = carga_reg[325,444]
                ## Este campo va a traer 178  
                LET reg_det_sol_rech.cve_ced_cuenta       = carga_reg[445,447]
                LET reg_det_sol_rech.cod_result_operac    = carga_reg[583,584]

                LET reg_det_sol_rech.diag_proceso_1       = carga_reg[585,587]
                LET reg_det_sol_rech.diag_proceso_2       = carga_reg[588,590]
                LET reg_det_sol_rech.diag_proceso_3       = carga_reg[591,593]
                LET reg_det_sol_rech.diag_proceso_4       = carga_reg[594,596]
                LET reg_det_sol_rech.diag_proceso_5       = carga_reg[597,599] 
                INSERT INTO tra_det_rechazo_iss VALUES(ultimo_folio       ,
                                                   reg_det_sol_rech.* ,
                                                   reg_4.rechazada
                                                   )
#ojo
              LET a_fecha_genera = reg_det_sol_rech.ident_lote_solic[10,11],"/",
                                   reg_det_sol_rech.ident_lote_solic[12,13],"/",
                                   reg_det_sol_rech.ident_lote_solic[06,09]

---- 717 ---->
            IF   reg_det_sol_rech.id_procesar  = " "  THEN
              UPDATE tra_mae_icefa_issste 
               SET id_procesar = " "
               WHERE  n_seguro        = reg_det_sol_rech.n_seguro        #AFI
               AND    nss             = reg_det_sol_rech.n_seguro_ent    #ICE
               AND    rfc             = reg_det_sol_rech.rfc_ent         #ICE
               AND    icefa_cod       = reg_det_sol_rech.cve_ced_cuenta  #ICE
               AND    id_procesar     IS NULL                                
               AND    status          = reg_4.enviada
           --- AND    fecha_genera    = a_fecha_genera
            END IF
---- 717 <----

             SELECT MIN(A.correlativo)
	     INTO   v_correlativo
	     FROM   tra_mae_icefa_issste A
	     WHERE  A.n_seguro        = reg_det_sol_rech.n_seguro        #AFI
	     AND    A.nss             = reg_det_sol_rech.n_seguro_ent    #ICE
	     AND    A.rfc             = reg_det_sol_rech.rfc_ent         #ICE
	     AND    A.icefa_cod       = reg_det_sol_rech.cve_ced_cuenta  #ICE
	     AND    A.id_procesar     = reg_det_sol_rech.id_procesar     #ICE
	     AND    A.status          = reg_4.enviada
         --- AND    A.fecha_genera    = a_fecha_genera

        CALL actualiza_maeicefa_rechazada(reg_det_sol_rech.n_seguro        ,#AFI
                                          reg_det_sol_rech.n_seguro_ent    ,#ICE
                                          reg_det_sol_rech.rfc_ent         ,#ICE
                                          reg_det_sol_rech.cve_ced_cuenta  ,#ICE
                                          reg_det_sol_rech.nro_ctrl_icefa  ,#ICE
                                          reg_det_sol_rech.id_procesar     ,#ICE
                                          reg_det_sol_rech.ident_lote_solic,
                                          reg_4.enviada                    ,
                                          reg_4.rechazada
                                         ) #amr


           UPDATE tra_mae_icefa_issste
	   SET fecha_solic_tra          =     reg_cza_sol_rech.fech_presentacion
	   WHERE correlativo            =     v_correlativo

         CALL f_desmarca_cuenta_ic(reg_det_sol_rech.n_seguro,260,v_correlativo,90)

                EXIT CASE

            WHEN "01"  #---ENCABEZADO DE SOLICITUDES RECHAZADAS---#
                LET reg_cza_sol_rech.tipo_registro      = carga_reg[001,002]
                LET reg_cza_sol_rech.ident_servicio     = carga_reg[003,004]
                LET reg_cza_sol_rech.ident_operacion    = carga_reg[005,006]
                LET reg_cza_sol_rech.tipo_ent_origen    = carga_reg[007,008]
                LET reg_cza_sol_rech.cve_ent_origen     = carga_reg[009,011]
                LET reg_cza_sol_rech.tipo_ent_destino   = carga_reg[012,013]
                LET reg_cza_sol_rech.ent_fed_envio_lote = carga_reg[017,019]

                LET c10_fecha                        = carga_reg[024,025],"/",
                                                       carga_reg[026,027],"/",
                                                       carga_reg[020,023]
                LET reg_cza_sol_rech.fech_presentacion  = c10_fecha

                LET reg_cza_sol_rech.consec_lote_dia    = carga_reg[028,030]
                LET reg_cza_sol_rech.cve_mod_recepcion  = carga_reg[031,032]
                LET reg_cza_sol_rech.cod_result_operac  = carga_reg[033,034]
                LET reg_cza_sol_rech.mot_rechazo_lote_1 = carga_reg[035,037]
                LET reg_cza_sol_rech.mot_rechazo_lote_2 = carga_reg[038,040]
                LET reg_cza_sol_rech.mot_rechazo_lote_3 = carga_reg[041,043]


            IF reg_cza_trasp.ident_operacion <> "01" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF

                SELECT "OK"
	             FROM   tra_cza_sol_rch A
	             WHERE  A.fech_presentacion = reg_cza_sol_rech.fech_presentacion
	             AND    A.consec_lote_dia   = reg_cza_sol_rech.consec_lote_dia
                GROUP BY 1

	             IF STATUS = NOTFOUND THEN

                       INSERT INTO tra_cza_sol_rch VALUES(ultimo_folio       ,
                                                         reg_cza_sol_rech.* ,
                                                         reg_4.rechazada    ,
                                                         reg.nom_archivo
                                                         )
                     ELSE
                       DISPLAY "" AT 18,2
                       PROMPT "ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                              "PARA CONTINUAR " FOR CHAR enter
                    EXIT PROGRAM

                END IF

            EXIT CASE

            WHEN "09"   #---SUMARIO DE SOLICITUDES RECHAZADAS---#
                LET reg_sum_sol_rech.tipo_registro    = carga_reg[001,002]
                LET reg_sum_sol_rech.cantidad_reg_det = carga_reg[003,011]

                INSERT INTO tra_sum_rechazo VALUES(ultimo_folio ,
                                                   reg_sum_sol_rech.*
                                                   )
        END CASE

    END FOREACH
     
END FUNCTION


{
FUNCTION solic_devolucion()
#sd------------------------
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)


    LET txt_1 = " SELECT * ",
                " FROM ",tmp_pla_trasp2 CLIPPED

    PREPARE pre_9 FROM txt_1
    DECLARE cur_9 CURSOR FOR pre_9
   
    LET cont = 0
    FOREACH cur_9 INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "02"  #--DETALLE DE SOLICITUDES A DEVOLVER---#
                LET cont = cont + 1
                DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

                LET reg_det_devol_conf.tipo_registro      = carga_reg[001,002]
                LET reg_det_devol_conf.cont_servicio      = carga_reg[003,012]
                LET reg_det_devol_conf.tipo_recep_cuenta  = carga_reg[013,014]
                LET reg_det_devol_conf.cve_recep_cuenta   = carga_reg[015,017]
                LET reg_det_devol_conf.tipo_ced_cuenta    = carga_reg[018,019]
                LET reg_det_devol_conf.cve_ced_cuenta     = carga_reg[020,022]
                LET reg_det_devol_conf.orig_tipo_trasp    = carga_reg[023,024]


                LET c10_fecha                         = carga_reg[029,030],"/",
                                                        carga_reg[031,032],"/",
                                                        carga_reg[025,028]

                LET reg_det_devol_conf.fecha_presentacion = c10_fecha

                LET reg_det_devol_conf.periodo_liquid     = carga_reg[033,038]
                LET reg_det_devol_conf.n_unico            = carga_reg[041,058]
                LET reg_det_devol_conf.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (reg_det_devol_conf.n_seguro IS NULL OR
                   reg_det_devol_conf.n_seguro = " "   OR
                   reg_det_devol_conf.n_seguro = ""    OR
                   reg_det_devol_conf.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  reg_det_devol_conf.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = reg_det_devol_conf.n_unico

               END IF

  #----Fin de La Modificacion

                LET reg_det_devol_conf.rfc                = carga_reg[085,097]
                LET reg_det_devol_conf.paterno            = carga_reg[098,137]
                LET reg_det_devol_conf.materno            = carga_reg[138,177]
                LET reg_det_devol_conf.nombres            = carga_reg[178,217]
                LET reg_det_devol_conf.cve_sector         = carga_reg[221,221]
                LET reg_det_devol_conf.ident_lote_solic   = carga_reg[240,255]
                LET reg_det_devol_conf.n_seguro_ent       = carga_reg[271,281]
                LET reg_det_devol_conf.rfc_ent            = carga_reg[282,294]
                LET reg_det_devol_conf.nro_ctrl_icefa     = carga_reg[295,324]
                LET reg_det_devol_conf.nombre_trab_icefa  = carga_reg[325,444]

    
                LET c16_saldo_sar_92                  = carga_reg[550,562],".",
                                                        carga_reg[563,564]
                LET reg_det_devol_conf.saldo_sar_92   = c16_saldo_sar_92 

                LET c16_saldo_viv_92                  = carga_reg[565,577],".",
                                                        carga_reg[578,579]
                LET reg_det_devol_conf.saldo_viv_92   = c16_saldo_viv_92

                INSERT INTO dev_det_confronta VALUES(ultimo_folio         ,
                                                  reg_det_devol_conf.* ,
                                                  "NO"         ,#confirma_devol
                                                  ""           ,#motivo_no_devol
                                                  reg_4.solic_devol #estado
                                                 )
                EXIT CASE

            WHEN "01"  #---ENCABEZADO DE SOLICITUDES A DEVOLVER---#
                LET reg_cza_devol_conf.tipo_registro      = carga_reg[001,002]
                LET reg_cza_devol_conf.ident_servicio     = carga_reg[003,004]
                LET reg_cza_devol_conf.ident_operacion    = carga_reg[005,006]
                LET reg_cza_devol_conf.tipo_ent_origen    = carga_reg[007,008]
                LET reg_cza_devol_conf.tipo_ent_destino   = carga_reg[012,013]
                LET reg_cza_devol_conf.cve_ent_destino    = carga_reg[014,016]


                LET c10_fecha                         = carga_reg[024,025],"/",
                                                        carga_reg[026,027],"/",
                                                        carga_reg[020,023]

                LET reg_cza_devol_conf.fecha_presentacion = c10_fecha

                LET reg_cza_devol_conf.consec_lote_dia    = carga_reg[028,030]
                LET reg_cza_devol_conf.cve_mod_recepcion  = carga_reg[031,032]


            IF reg_cza_trasp.ident_operacion <> "16" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF

                SELECT "OK"
	             FROM   dev_cza_confronta
	             WHERE  fecha_presentacion= reg_cza_devol_conf.fecha_presentacion

	             IF STATUS = NOTFOUND THEN
                    INSERT INTO dev_cza_confronta VALUES(ultimo_folio         ,
                                                      reg_cza_devol_conf.* ,
                                                      reg_4.solic_devol    ,
                                                      reg.nom_archivo
                                                     )
                ELSE
                    DISPLAY "" AT 18,2
                    PROMPT "ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER> ",
                           "PARA CONTINUAR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
               
                EXIT CASE

            WHEN "09"  #---SUMARIO DE SOLICITUDES A DEVOLVER---#
             LET reg_sum_devol_conf.tipo_registro      = carga_reg[001,002]
             LET reg_sum_devol_conf.cantidad_reg_det   = carga_reg[003,011]

             LET c16_saldo_sar_92                      = carga_reg[117,129],".",
                                                         carga_reg[130,131]
             LET reg_sum_devol_conf.total_saldo_sar_92 = c16_saldo_sar_92


             LET c16_saldo_viv_92                      = carga_reg[132,144],".",
                                                         carga_reg[145,146]
             LET reg_sum_devol_conf.total_saldo_viv_92 = c16_saldo_viv_92

             INSERT INTO dev_sum_confronta VALUES(ultimo_folio          ,
                                               reg_sum_devol_conf.*  ,
                                               reg_4.solic_devol
                                              )
                EXIT CASE
        END CASE
    END FOREACH
END FUNCTION
}

FUNCTION actualiza_maeicefa(reg_3)
#am-------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        n_seguro              LIKE tra_mae_icefa_issste.n_seguro             ,
        nss                   LIKE tra_mae_icefa_issste.nss                  ,
        rfc                   LIKE tra_mae_icefa_issste.rfc                  ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod            ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta          ,
        id_procesar           LIKE tra_mae_icefa_issste.id_procesar          ,
        ident_lote_solic      LIKE tra_det_trasp_sal_issste.ident_lote_solic ,
        estado_actual         SMALLINT                            ,
        estado_nuevo          SMALLINT  
    END RECORD

    DEFINE reg_4 RECORD #loc #reg_4
        fecha_genera          CHAR(10) ,
        lote_genera           SMALLINT
    END RECORD

        LET reg_4.fecha_genera  =    reg_3.ident_lote_solic[10,11],"/",
                                     reg_3.ident_lote_solic[12,13],"/",
                                     reg_3.ident_lote_solic[06,09]

        LET reg_4.lote_genera  =     reg_3.ident_lote_solic[14,16]

        LET   s_nro_icefa                        = 0
---- 717 ---->
            IF   reg_3.id_procesar              = " "    THEN
              UPDATE tra_mae_icefa_issste
               SET id_procesar = " "
             WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro     #AFI
             AND    tra_mae_icefa_issste.nss          = reg_3.nss          #ICE
             AND    tra_mae_icefa_issste.rfc          = reg_3.rfc          #ICE
             AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod    #ICE
             AND    tra_mae_icefa_issste.id_procesar  IS NULL                  
             AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

            END IF
---- 717 <----

        SELECT COUNT(*)
        INTO   s_nro_icefa
        FROM   tra_mae_icefa_issste
        WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro     #AFI
        AND    tra_mae_icefa_issste.nss          = reg_3.nss          #ICE
        AND    tra_mae_icefa_issste.rfc          = reg_3.rfc          #ICE
        AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod    #ICE
        AND    tra_mae_icefa_issste.id_procesar  = reg_3.id_procesar  #ICE
        AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

           IF s_nro_icefa = 1 THEN

               UPDATE tra_mae_icefa_issste
               SET    tra_mae_icefa_issste.status = reg_3.estado_nuevo
               WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro  #AFI
               AND    tra_mae_icefa_issste.nss          = reg_3.nss       #ICE
               AND    tra_mae_icefa_issste.rfc          = reg_3.rfc       #ICE
               AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod #ICE
               AND    tra_mae_icefa_issste.id_procesar  = reg_3.id_procesar#ICE
            -- AND    tra_mae_icefa_issste.fecha_genera = reg_4.fecha_genera
            -- AND    tra_mae_icefa_issste.lote_genera  = reg_4.lote_genera
               AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

                    RETURN

                ELSE

                    INSERT INTO tra_no_actualiza_iss VALUES(ultimo_folio  ,
                                                   reg_3.estado_nuevo     ,
                                                   reg_3.n_seguro         ,#AFI
                                                   reg_3.nss              ,#ICE
                                                   reg_3.rfc              ,#ICE
                                                   reg_3.icefa_cod        ,#ICE
                                                   reg_3.nro_int_cta      ,#ICE
                                                   reg_3.id_procesar      ,#ICE
                                                   reg_3.ident_lote_solic ,
                                                   reg_4.fecha_genera     ,
                                                   reg_4.lote_genera      ,
                                                   s_nro_icefa
                                                   )
                    LET nn_no_act = nn_no_act + 1

                    RETURN

                END IF
END FUNCTION

FUNCTION actualiza_maeicefa_rechazada(reg_3)
#amr----------------------------------------

    DEFINE reg_3 RECORD #loc #reg_3
        n_seguro              LIKE tra_mae_icefa_issste.n_seguro              ,
        nss                   LIKE tra_mae_icefa_issste.nss                   ,
        rfc                   LIKE tra_mae_icefa_issste.rfc                   ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod             ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta           ,
        id_procesar           LIKE tra_mae_icefa_issste.id_procesar           ,
        ident_lote_solic      LIKE tra_det_trasp_sal_issste.ident_lote_solic  ,
        estado_actual         SMALLINT                            ,
        estado_nuevo          SMALLINT  
    END RECORD

    DEFINE reg_4 RECORD #loc #reg_4
        fecha_genera          CHAR(10) ,
        lote_genera           SMALLINT
    END RECORD

    LET reg_4.fecha_genera = reg_3.ident_lote_solic[10,11],"/",
                             reg_3.ident_lote_solic[12,13],"/",
                             reg_3.ident_lote_solic[06,09]

    LET reg_4.lote_genera  = reg_3.ident_lote_solic[14,16]

       LET    s_nro_icefa                       =  0
---- 717 ---->
            IF   reg_3.id_procesar             = " "  THEN
              UPDATE tra_mae_icefa_issste
               SET id_procesar = " "
            WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro       #AFI
            AND    tra_mae_icefa_issste.nss          = reg_3.nss            #ICE
            AND    tra_mae_icefa_issste.rfc          = reg_3.rfc            #ICE
            AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod      #ICE
            AND    tra_mae_icefa_issste.id_procesar  IS NULL                    
            AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

            END IF
---- 717 <----

       SELECT COUNT(*)
       INTO   s_nro_icefa
       FROM   tra_mae_icefa_issste
       WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro       #AFI
       AND    tra_mae_icefa_issste.nss          = reg_3.nss            #ICE
       AND    tra_mae_icefa_issste.rfc          = reg_3.rfc            #ICE
       AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod      #ICE
       AND    tra_mae_icefa_issste.id_procesar  = reg_3.id_procesar    #ICE
       AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

        IF s_nro_icefa = 1 THEN

          UPDATE tra_mae_icefa_issste
          SET    tra_mae_icefa_issste.status = reg_3.estado_nuevo
          WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro    #AFI
	  AND    tra_mae_icefa_issste.nss          = reg_3.nss         #ICE
          AND    tra_mae_icefa_issste.rfc          = reg_3.rfc         #ICE
          AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod   #ICE
          AND    tra_mae_icefa_issste.id_procesar  = reg_3.id_procesar #ICE
      --  AND    tra_mae_icefa_issste.fecha_genera = reg_4.fecha_genera
       -- AND    tra_mae_icefa_issste.lote_genera  = reg_4.lote_genera
          AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

           RETURN

       ELSE
                    INSERT INTO tra_no_actualiza_iss VALUES(ultimo_folio ,
                                                   reg_3.estado_nuevo    ,
                                                   reg_3.n_seguro        ,#AFI
                                                   reg_3.nss             ,#ICE
                                                   reg_3.rfc             ,#ICE
                                                   reg_3.icefa_cod       ,#ICE
                                                   reg_3.nro_int_cta     ,#ICE
                                                   reg_3.id_procesar      ,#ICE
                                                   reg_3.ident_lote_solic,
                                                   reg_4.fecha_genera    ,
                                                   reg_4.lote_genera     ,
                                                   s_nro_icefa
                                                   )
                   LET nn_no_act = nn_no_act + 1

                    RETURN

                END IF
END FUNCTION

{
FUNCTION planchadas()
#p-------------------
    DEFINE reg_5 RECORD #glo #reg_5
        saldo_sar_pagar      LIKE dev_det_normal.saldo_sar_pagar ,
        saldo_viv_92         LIKE dev_det_normal.saldo_viv_92    ,
        acciones_sar_92      LIKE dev_det_normal.acciones_sar_92 ,
        comis_saldo_sar      LIKE dev_det_normal.comis_saldo_sar
    END RECORD

    DEFINE #loc #char
        c16_sar_pagar        CHAR(016) ,
        c16_sar_92_banx      CHAR(016) ,
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)

    DEFINE #loc #integer
        folio_devol          INTEGER

    LET txt_1 = " SELECT * ",
                " FROM ",tmp_pla_trasp2 CLIPPED

    PREPARE pre_11 FROM txt_1
    DECLARE cur_11 CURSOR FOR pre_11
   
    LET cont = 0
    FOREACH cur_11 INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "02"  #--DETALLE DE PLANCHADAS--#
                LET cont = cont + 1
                DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

                LET dev_det_planchada.tipo_registro      = carga_reg[001,002]
                LET dev_det_planchada.cont_servicio      = carga_reg[003,012]
                LET dev_det_planchada.tipo_recep_cuenta  = carga_reg[013,014]
                LET dev_det_planchada.cve_recep_cuenta   = carga_reg[015,017]
                LET dev_det_planchada.tipo_ced_cuenta    = carga_reg[018,019]
                LET dev_det_planchada.cve_ced_cuenta     = carga_reg[020,022]
                LET dev_det_planchada.orig_tipo_trasp    = carga_reg[023,024]

                LET c10_fecha                         = carga_reg[029,030],"/",
                                                        carga_reg[031,032],"/",
                                                        carga_reg[025,028]

                LET dev_det_planchada.fecha_presentacion = c10_fecha

                LET c10_fecha                         = carga_reg[037,038],"/",
                                                        carga_reg[039,040],"/",
                                                        carga_reg[033,036]
                LET dev_det_planchada.fecha_liquidacion  = c10_fecha

                LET dev_det_planchada.n_unico            = carga_reg[041,058]
                LET dev_det_planchada.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF ( dev_det_planchada.n_seguro IS NULL OR
                    dev_det_planchada.n_seguro = " "   OR
                    dev_det_planchada.n_seguro = ""    OR 
                    dev_det_planchada.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  dev_det_planchada.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = dev_det_planchada.n_unico

               END IF

  #----Fin de La Modificacion

                LET dev_det_planchada.rfc                = carga_reg[085,097]
                LET dev_det_planchada.paterno            = carga_reg[098,137]
                LET dev_det_planchada.materno            = carga_reg[138,177]
                LET dev_det_planchada.nombres            = carga_reg[178,217]
                LET dev_det_planchada.cve_sector         = carga_reg[221,221]
                LET dev_det_planchada.ident_lote_solic   = carga_reg[240,255]
                LET dev_det_planchada.n_seguro_ent       = carga_reg[271,281]
                LET dev_det_planchada.rfc_ent            = carga_reg[282,294]
                LET dev_det_planchada.nro_ctrl_icefa     = carga_reg[295,324]
                LET dev_det_planchada.nombre_trab_icefa  = carga_reg[325,444]

                LET c16_saldo_sar_92                  = carga_reg[520,532],".",
                                                        carga_reg[533,534]
                LET dev_det_planchada.saldo_sar_92       = c16_saldo_sar_92 

                LET c16_sar_92_banx                   = carga_reg[535,547],".",
                                                        carga_reg[548,549]
                LET dev_det_planchada.saldo_sar_92_banx  = c16_sar_92_banx

                LET c16_sar_pagar                     = carga_reg[550,562],".",
                                                        carga_reg[563,564]
                LET dev_det_planchada.saldo_sar_pagar_p  = c16_sar_pagar  

                LET c16_saldo_viv_92                  = carga_reg[565,577],".",
                                                        carga_reg[578,579]
                LET dev_det_planchada.saldo_viv_92_p     = c16_saldo_viv_92

                LET dev_det_planchada.acciones_sar_92 = 0
                LET dev_det_planchada.saldo_sar_pagar = 0
                LET dev_det_planchada.saldo_viv_92    = 0
                LET dev_det_planchada.comis_saldo_sar = 0
                LET dev_det_planchada.planchado_sar   = "NO"
                LET dev_det_planchada.planchado_viv   = "NO"
         
                WHENEVER ERROR STOP

                SELECT B.saldo_sar_pagar ,
                       B.saldo_viv_92    ,
                       B.acciones_sar_92 ,
                       B.comis_saldo_sar
                INTO   reg_5.*
                FROM   dev_det_normal B
                WHERE  B.folio_devol      = folio_devol
                AND    B.n_seguro         = dev_det_planchada.n_seguro
                AND    B.n_seguro_ent     = dev_det_planchada.n_seguro_ent
                AND    B.rfc_ent          = dev_det_planchada.rfc_ent
                AND    B.cve_ced_cuenta   = dev_det_planchada.cve_ced_cuenta
                AND    B.nro_ctrl_icefa   = dev_det_planchada.nro_ctrl_icefa
                AND    B.ident_lote_solic = dev_det_planchada.ident_lote_solic
                AND    B.nombre_ent       = dev_det_planchada.nombre_trab_icefa
                GROUP BY 1,2,3,4

                LET dev_det_planchada.acciones_sar_92 = reg_5.acciones_sar_92
                LET dev_det_planchada.saldo_sar_pagar = reg_5.saldo_sar_pagar
                LET dev_det_planchada.saldo_viv_92    = reg_5.saldo_viv_92
                LET dev_det_planchada.comis_saldo_sar = reg_5.comis_saldo_sar

           IF reg_5.saldo_sar_pagar <> dev_det_planchada.saldo_sar_pagar_p THEN
                LET dev_det_planchada.planchado_sar   = "SI"
                LET dev_det_planchada.acciones_sar_92 = 0
           END IF

               IF reg_5.saldo_viv_92 <> dev_det_planchada.saldo_viv_92_p THEN
                   LET dev_det_planchada.planchado_viv   = "SI"
               END IF

                INSERT INTO dev_det_planchada VALUES(folio_devol      ,
                                                  dev_det_planchada.* ,
                                                  1                 #estado
                                                 )
                EXIT CASE

            WHEN "01"  #---ENCABEZADO DE SOLICITUDES A DEVOLVER---#
                LET dev_cza_planchada.tipo_registro      = carga_reg[001,002]
                LET dev_cza_planchada.ident_servicio     = carga_reg[003,004]
                LET dev_cza_planchada.ident_operacion    = carga_reg[005,006]
                LET dev_cza_planchada.tipo_ent_origen    = carga_reg[007,008]
                LET dev_cza_planchada.cve_ent_origen     = carga_reg[009,011]
                LET dev_cza_planchada.tipo_ent_destino   = carga_reg[012,013]
                LET dev_cza_planchada.ent_fed_envio_lote = carga_reg[017,019]

                LET c10_fecha                         = carga_reg[024,025],"/",
                                                        carga_reg[026,027],"/",
                                                        carga_reg[020,023]
                LET dev_cza_planchada.fecha_presentacion = c10_fecha

                LET dev_cza_planchada.consec_lote_dia    = carga_reg[028,030]
                LET dev_cza_planchada.cve_mod_recepcion  = carga_reg[031,032]


            IF reg_cza_trasp.ident_operacion <> "18" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF

                SELECT MAX(folio)
                INTO   folio_devol
                FROM   dev_cza_confronta

                DISPLAY "FOLIO NUMERO         : ",folio_devol  AT 17,2

                INSERT INTO dev_cza_planchada VALUES(folio_devol      ,
                                                  dev_cza_planchada.* ,
                                                  1                ,
                                                  reg.nom_archivo
                                                 )
                EXIT CASE

            WHEN "09"  #---SUMARIO DE SOLICITUDES A DEVOLVER---#
                LET dev_sum_planchada.tipo_registro     = carga_reg[001,002]
                LET dev_sum_planchada.cantidad_reg_det  = carga_reg[003,011]

                LET c16_saldo_sar_92                 = carga_reg[087,099],".",
                                                       carga_reg[100,101]
                LET dev_sum_planchada.total_sar_92      = c16_saldo_sar_92

                LET c16_sar_92_banx                  = carga_reg[102,114],".",
                                                       carga_reg[115,116]
                LET dev_sum_planchada.total_sar_92_banx = c16_sar_92_banx
   
                LET c16_sar_pagar                    = carga_reg[117,129],".",
                                                       carga_reg[130,131]
                LET dev_sum_planchada.total_sar_pagar   = c16_sar_pagar
   
                LET c16_saldo_viv_92                 = carga_reg[132,144],".",
                                                       carga_reg[145,146]
                LET dev_sum_planchada.total_viv_92      = c16_saldo_viv_92

                INSERT INTO dev_sum_planchada VALUES(folio_devol          ,
                                                  dev_sum_planchada.* ,
                                                  1
                                                 )
                EXIT CASE
        END CASE
    END FOREACH
END FUNCTION
}
{
FUNCTION devoluciones_rechazadas()
#dr------------------------------
    DEFINE #loc #char
        c16_sar_pagar        CHAR(016) ,
        c16_sar_92_banx      CHAR(016) ,
        carga_reg            CHAR(730) ,
        c2_carga_reg         CHAR(002)

 define folio_devol integer

    LET txt_1 = " SELECT  *  ",
                " FROM   ",tmp_pla_trasp2 CLIPPED
   
    PREPARE pre_12 FROM txt_1
    DECLARE cur_12 CURSOR FOR pre_12



    LET cont = 0
    FOREACH cur_12 INTO carga_reg
        CASE carga_reg[1,2]
            WHEN "02"  #--DETALLE DE PLANCHADAS--#
                LET cont = cont + 1
                DISPLAY "REGISTROS PROCESADOS : ",cont AT 19,40

                LET dev_det_rechazada.tipo_registro      = carga_reg[001,002]
                LET dev_det_rechazada.cont_servicio      = carga_reg[003,012]
                LET dev_det_rechazada.tipo_recep_cuenta  = carga_reg[013,014]
                LET dev_det_rechazada.cve_recep_cuenta   = carga_reg[015,017]
                LET dev_det_rechazada.tipo_ced_cuenta    = carga_reg[018,019]
                LET dev_det_rechazada.cve_ced_cuenta     = carga_reg[020,022]
                LET dev_det_rechazada.orig_tipo_trasp    = carga_reg[023,024]

                LET c10_fecha                         = carga_reg[029,030],"/",
                                                        carga_reg[031,032],"/",
                                                        carga_reg[025,028]

                LET dev_det_rechazada.fecha_presentacion = c10_fecha

                LET c10_fecha                         = carga_reg[037,038],"/",
                                                        carga_reg[039,040],"/",
                                                        carga_reg[033,036]
                LET dev_det_rechazada.fecha_liquidacion  = c10_fecha

                LET dev_det_rechazada.n_unico            = carga_reg[041,058]
                LET dev_det_rechazada.n_seguro           = carga_reg[059,069]

  #----Modificacion para TRABADADORES IND ES DECIR LOS DE T.S = 8 ----#
  #    Ya que en el Archivo de Recepcion el n_seguro viene en Blanco.  

               IF (dev_det_rechazada.n_seguro IS NULL OR
                   dev_det_rechazada.n_seguro = " "   OR
                   dev_det_rechazada.n_seguro = ""    OR
                   dev_det_rechazada.n_seguro = "00000000000" ) THEN

                   SELECT a.n_seguro
                   INTO  dev_det_rechazada.n_seguro 
                   FROM  afi_mae_afiliado a
                   WHERE a.n_unico = dev_det_rechazada.n_unico

               END IF

  #----Fin de La Modificacion

                LET dev_det_rechazada.rfc                = carga_reg[085,097]
                LET dev_det_rechazada.paterno            = carga_reg[098,137]
                LET dev_det_rechazada.materno            = carga_reg[138,177]
                LET dev_det_rechazada.nombres            = carga_reg[178,217]
                LET dev_det_rechazada.cve_sector         = carga_reg[221,221]
                LET dev_det_rechazada.ident_lote_solic   = carga_reg[240,255]
                LET dev_det_rechazada.n_seguro_ent       = carga_reg[271,281]
                LET dev_det_rechazada.rfc_ent            = carga_reg[282,294]
                LET dev_det_rechazada.nro_ctrl_icefa     = carga_reg[295,324]
                LET dev_det_rechazada.nombre_trab_icefa  = carga_reg[325,444]

                LET c16_saldo_sar_92                  = carga_reg[520,532],".",
                                                        carga_reg[533,534]
                LET dev_det_rechazada.saldo_sar_92       = c16_saldo_sar_92 

                LET c16_sar_92_banx                   = carga_reg[535,547],".",
                                                        carga_reg[548,549]
                LET dev_det_rechazada.saldo_sar_92_banx  = c16_sar_92_banx

                LET c16_sar_pagar                     = carga_reg[550,562],".",
                                                        carga_reg[563,564]
                LET dev_det_rechazada.saldo_sar_pagar    = c16_sar_pagar  

                LET c16_saldo_viv_92                  = carga_reg[565,577],".",
                                                        carga_reg[578,579]
                LET dev_det_rechazada.saldo_viv_92       = c16_saldo_viv_92

                SELECT MAX(a.folio_devol)
                INTO   folio_devol
                FROM   dev_det_normal a

                DISPLAY "FOLIO NUMERO         : ",folio_devol  AT 17,2

                INSERT INTO dev_det_rechazada VALUES(folio_devol    ,
                                                     dev_det_rechazada.* ,
                                                     1                 #estado
                                                     )
                EXIT CASE

            WHEN "01"  #---ENCABEZADO DE SOLICITUDES A DEVOLVER---#
                LET dev_cza_rechazada.tipo_registro      = carga_reg[001,002]
                LET dev_cza_rechazada.ident_servicio     = carga_reg[003,004]
                LET dev_cza_rechazada.ident_operacion    = carga_reg[005,006]
                LET dev_cza_rechazada.tipo_ent_origen    = carga_reg[007,008]
                LET dev_cza_rechazada.cve_ent_origen     = carga_reg[009,011]
                LET dev_cza_rechazada.tipo_ent_destino   = carga_reg[012,013]
                LET dev_cza_rechazada.ent_fed_envio_lote = carga_reg[017,019]

                LET c10_fecha                         = carga_reg[024,025],"/",
                                                        carga_reg[026,027],"/",
                                                        carga_reg[020,023]
                LET dev_cza_rechazada.fecha_presentacion = c10_fecha

                LET dev_cza_rechazada.consec_lote_dia    = carga_reg[028,030]
                LET dev_cza_rechazada.cve_mod_recepcion  = carga_reg[031,032]



            IF reg_cza_trasp.ident_operacion <> "19" THEN
	       PROMPT "ARCHIVO DIFIERE CON OPERACION SELECCIONADA...<ENTER> ",
	              "PARA CONTINUAR "FOR CHAR ENTER

		      DELETE FROM glo_folio WHERE folio =  ultimo_folio    
               EXIT PROGRAM
            ELSE

              DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 17,2
            END IF


                INSERT INTO dev_cza_rechazada VALUES(ultimo_folio     ,
                                                  dev_cza_rechazada.* ,
                                                  1                ,
                                                  reg.nom_archivo
                                                 )
                EXIT CASE

            WHEN "09"  #---SUMARIO DE SOLICITUDES A DEVOLVER---#
                LET dev_sum_rechazada.tipo_registro     = carga_reg[001,002]
                LET dev_sum_rechazada.cantidad_reg_det  = carga_reg[003,011]

                LET c16_saldo_sar_92                 = carga_reg[087,099],".",
                                                       carga_reg[100,101]
                LET dev_sum_rechazada.total_sar_92      = c16_saldo_sar_92

                LET c16_sar_92_banx                  = carga_reg[102,114],".",
                                                       carga_reg[115,116]
                LET dev_sum_rechazada.total_sar_92_banx = c16_sar_92_banx
   
                LET c16_sar_pagar                    = carga_reg[117,129],".",
                                                       carga_reg[130,131]
                LET dev_sum_rechazada.total_sar_pagar   = c16_sar_pagar
   
                LET c16_saldo_viv_92                 = carga_reg[132,144],".",
                                                       carga_reg[145,146]
                LET dev_sum_rechazada.total_viv_92      = c16_saldo_viv_92

                INSERT INTO dev_sum_rechazada VALUES(ultimo_folio     ,
                                                  dev_sum_rechazada.* ,
                                                  1
                                                 )
                EXIT CASE
        END CASE
    END FOREACH
END FUNCTION
}

FUNCTION f_desmarca_cuenta_ic(vnss,vmarca_entra,vcorr,vcodigo_rechazo)
#fd--------------------------------------------------------------------

DEFINE ejecuta CHAR(300)
DEFINE tipo_rech SMALLINT
DEFINE vcorr integer
DEFINE vnss char(11),
       vmarca_entra smallint ,
       vestado_marca smallint,
       vmarca_causa  smallint,
       vcodigo_rechazo smallint,
       vusuario   char(008)
DEFINE xcodigo_marca smallint,
       xcodigo_rechazo smallint


LET vestado_marca = 30
LET vmarca_causa  =  260

select user
INTO vusuario
FROM tab_afore_local
group by 1
 
---- 717 ---->
  IF vcorr = ' '
  OR vcorr IS NULL THEN
    LET vcorr = 0
  END IF
---- 717 <----

  LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(","'",vnss,"',",
						       vmarca_entra,",",
						       vcorr       ,",",
						       vestado_marca,",",
						       vmarca_causa ,",",
 						   "'",vusuario,"')"


LET ejecuta = ejecuta CLIPPED
PREPARE clausula_spl2 FROM ejecuta
EXECUTE clausula_spl2

END FUNCTION



FUNCTION limpieza(ruta)
#l---------------------
    DEFINE 
        ruta                  CHAR(50) ,
        comandol              CHAR(300)

   LET comandol = "cd ",ruta CLIPPED,
            "/; cp ",reg.nom_archivo CLIPPED," RESPALDO",HOY USING"YYYYMMDD"
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/",ascii 92,ascii 92,"/N/g' ",reg.nom_archivo CLIPPED," > LIMP2 "
   RUN comandol

   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/",ascii 92,"//N/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/|/N/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/@/N/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/,/*/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
{
   LET comandol = "cd ",reg_1.ruta_trasp CLIPPED,
            "/;sed 's/",ascii 92,"+/*/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",reg_1.ruta_trasp CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
}
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/",ascii 13,"//g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/&/N/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/;sed 's/#/N/g' ",reg.nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
            "/; mv LIMP2 ",reg.nom_archivo
   RUN comandol
--------------------------------------------------------------------
END FUNCTION
