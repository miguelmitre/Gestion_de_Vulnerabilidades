#Proyecto          => Sistema de Afores.( MEXICO )                             #
#                  => E.F.P.                                                   #
#Programa DISC002  => CARGA DE ARCHIVO PARA LA DISPERSION DE dis_cuenta           #
#Fecha             => 14 ENERO DE 1997                                         #
#By                => FRANCO ULLOA V.                                          #
#Sistema           => DIS. 			                               #
#Fecha ulti. actuli=> 07 enero de 1997.(Modificacion campo 16)                 #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        g_param   RECORD LIKE dis_parametro.* ,
        w_codigo_afore         LIKE tab_afore_local.codigo_afore 

    DEFINE reg_cza RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_operacion              CHAR(02) ,
        tipo_ent_origen              CHAR(02) ,
        clave_ent_origen             CHAR(03) ,
        tipo_ent_destino             CHAR(02) ,
        clave_ent_destino            CHAR(03) ,
        fech_creac_lote              CHAR(08) ,
        lote_del_dia                 CHAR(03) ,
        mod_recp_envio               CHAR(02) ,
        fech_limite_resp             CHAR(08) ,
        result_operacion             CHAR(02) ,
        motivo_rechazo1              CHAR(03) ,
        motivo_rechazo2              CHAR(03) ,
        motivo_rechazo3              CHAR(03) ,
        filler                       CHAR(247)      
    END RECORD

    DEFINE reg_det RECORD
        folio                        INTEGER  ,
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        consec_reg_lote              CHAR(08) ,
        NSS                          CHAR(11) ,
        RFC                          CHAR(13) ,
        CURP                         CHAR(18) ,
        nom_trabajador               CHAR(50) ,
        periodo_pago                 CHAR(06) ,
        fech_pago                    CHAR(08) ,
        fech_valor_rcv               CHAR(08) ,
        fech_valor_viv               CHAR(08) ,
        ult_salario_diario           CHAR(07) ,
        folio_pago_SUA               CHAR(06) ,
        reg_patronal_IMSS            CHAR(11) ,
        rfc_patron                   CHAR(13) ,
        cve_ent_receptora            CHAR(03) ,
        dias_cotz_bimest             CHAR(02) ,
        dias_incap_bimest            CHAR(02) ,
        dias_ausent_bimest           CHAR(02) ,
        impt_ret                     CHAR(07) ,
        impt_act_rec_ret             CHAR(07) ,
        impt_ces_vej                 CHAR(07) ,
        impt_act_r_ces_vej           CHAR(07) ,
        impt_aport_vol               CHAR(07) ,
        impt_aport_pat               CHAR(07) ,
        impt_cuota_soc               CHAR(07) ,
        impt_aport_est               CHAR(07) ,
        impt_aport_esp               CHAR(07) ,
        impt_act_cuo_soc             CHAR(07) ,
        impt_act_aport_est           CHAR(07) ,
        impt_act_cuo_esp             CHAR(07) ,  # 31
        fecha_pago_gub               CHAR(08) ,
        filler0                      CHAR(10) ,
####        ind_rcv_salida               CHAR(09) ,
####        ind_viv_salida               CHAR(09) ,
        result_operacion             CHAR(02) ,  # 34
        det_mot_rechazo1             CHAR(03) ,
        det_mot_rechazo2             CHAR(03) ,
        det_mot_rechazo3             CHAR(03) ,
        filler                       CHAR(02) 
    END RECORD

    DEFINE reg_det_i RECORD
         folio                        INTEGER  ,
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         consec_reg_lote              CHAR(08) ,
         NSS                          CHAR(11) ,
         RFC                          CHAR(13) ,
         CURP                         CHAR(18) ,
         nom_trabajador               CHAR(50) ,
         periodo_pago                 CHAR(06) ,
         fech_pago                    CHAR(08) ,
         fech_valor_rcv               CHAR(08) ,
         fech_valor_viv               CHAR(08) ,
         ult_salario_diario           CHAR(07) ,
         folio_pago_SUA               CHAR(06) ,
         reg_patronal_IMSS            CHAR(11) ,
         rfc_patron                   CHAR(13) ,
         cve_ent_receptora            CHAR(03) ,
         dias_cotz_bimest             CHAR(02) ,
         dias_incap_bimest            CHAR(02) ,
         dias_ausent_bimest           CHAR(02) ,
         impt_int_ret                 CHAR(07) ,
         impt_int_act_rr              CHAR(07) ,
         impt_int_ces_vej             CHAR(07) ,
         impt_int_act_rcv             CHAR(07) ,
         impt_int_aport_vol           CHAR(07) ,
         impt_int_aport_pat           CHAR(07) ,
         impt_int_cuota_soc           CHAR(07) ,
         impt_int_aport_est           CHAR(07) ,
         impt_int_aport_esp           CHAR(07) ,
         impt_int_act_cuo_s           CHAR(07) ,
         impt_int_act_est             CHAR(07) ,
         impt_int_a_cuo_esp           CHAR(07) ,
         fecha_pago_gub               CHAR(08) ,
         filler0                      CHAR(10) ,
####         ind_rcv_salida               CHAR(09) ,
####         ind_vivieda_salida           CHAR(09) ,
         result_operacion             CHAR(02) ,
         det_mot_rechazo1             CHAR(03) ,
         det_mot_rechazo2             CHAR(03) ,
         det_mot_rechazo3             CHAR(03) ,
         filler                       CHAR(02)
        END RECORD

    DEFINE reg_pagos RECORD
        folio                        INTEGER   ,
        tipo_registro                CHAR(002) ,
        ident_servicio               CHAR(002) ,
        ident_pago                   CHAR(016) ,
        importe                      CHAR(015) ,
        fech_liquidacion             CHAR(008) ,
        impt_aport_acept             CHAR(015) ,
        impt_aport_dev               CHAR(015) ,
-------        filler                       CHAR(222) ,
        fecha_archivo                DATE,
        fecha_envio                  DATE,
        estado                       CHAR(001)
    END RECORD

    DEFINE reg_sum RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_operacion              CHAR(02) ,
        tipo_ent_origen              CHAR(02) ,
        clave_ent_origen             CHAR(03) ,
        tipo_ent_destino             CHAR(02) ,
        clave_ent_destino            CHAR(03) ,
        fech_creac_lote              CHAR(08) ,
        lote_del_dia                 CHAR(03) ,
        impt_total_rcv               CHAR(15) ,
        impt_total_int_rcv           CHAR(15) ,
        impt_total_patr              CHAR(15) ,
        impt_tatal_int_pat           CHAR(15) ,
        impt_total_guber             CHAR(15) ,
        impt_total_int_gub           CHAR(15) ,
        num_de_reg_aport             CHAR(08) ,
        num_cuenta_xpagar            CHAR(06) ,
        impt_total_xpagar            CHAR(15) ,
        filler                       CHAR(149)   
    END RECORD

    DEFINE reg_hdep RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_pago                   CHAR(16) ,
        importe                      CHAR(15) ,
        fech_liquidacion             CHAR(08) ,
        impt_aport_acept             CHAR(15) ,
        impt_aport_dev               CHAR(15) ,
        estado                       CHAR(01) ,
        filler                       CHAR(222)
    END RECORD

    DEFINE reg_hdep2 RECORD
        tipo_registro                CHAR(02)      ,
        ident_servicio               CHAR(02)      ,
        ident_pago                   CHAR(16)      ,
        importe                      DECIMAL(15,2) ,
        fech_liquidacion             CHAR(08)      ,
        impt_aport_acept             CHAR(15)      ,
        impt_aport_dev               DECIMAL(15,2) ,
        estado                       CHAR(01)      ,
        filler                       CHAR(222)
    END RECORD
    DEFINE auk RECORD
        result_operacion             CHAR(02) ,
        det_mot_rechazo1             CHAR(03) ,
        det_mot_rechazo2             CHAR(03) ,
        det_mot_rechazo3             CHAR(03)
    END RECORD

    DEFINE auh RECORD
        result_operacion             CHAR(02) ,
        det_mot_inconsist1           CHAR(03) ,
        det_mot_inconsist2           CHAR(03) ,
        det_mot_inconsist3           CHAR(03)
    END RECORD

    DEFINE aux RECORD
        result_operacion             CHAR(02) ,
        motivo_rechazo1              CHAR(03) ,
        motivo_rechazo2              CHAR(03) ,
        motivo_rechazo3              CHAR(03)
    END RECORD

    DEFINE 
        HOY                          ,
        d_fecha                      DATE
     
    DEFINE 
        enter    		     CHAR(001) ,
        aux_pausa    		     CHAR(001) ,
        ff                           CHAR(010) ,           
        AAAA                         CHAR(004) ,
        MM                           CHAR(002) ,
        DD                           CHAR(002) ,
        generar                      CHAR(012) ,
        aaa                          CHAR(002) ,
        ejecuta                      CHAR(030) ,
        c_fecha                      CHAR(010) ,
        comando                      CHAR(200) ,
        c_tipo_registro              CHAR(002) ,
        c_fech_creac_lote            CHAR(010) ,
        ejecuta_2                    CHAR(200) ,
        nom_arch_generado            CHAR(021)

    DEFINE
        sw_1                         ,
        sw_2                         ,
        sw_3                         ,
        s_aaaa                       ,
        s_mm                         ,
        s_dd                         ,
        num_reg_rchzo                ,
        num_reg_inconsist            ,
        registro_numero              ,
        det_sw_1                     ,
        det_sw_2                     ,
        det_sw_3                     ,
        s_inconsist_1                ,
        s_inconsist_2                ,
        s_inconsist_3                ,
        digito                       ,
        cuantos                      ,
        n_incons                     ,
        s_largo                      ,
        s_lote_del_dia               ,
        s_lotes_correlativo          ,
        s_impt_aport_acept           INTEGER

    DEFINE 
        sw_inconsist                 ,
        sw_rchzo                     ,
        i_consec_reg_lote            ,
        i_consec_rec_lote            ,
        i_num_de_reg_aport           INTEGER

    DEFINE 
        d15_impt_total_rcv           ,
        d15_impt_total_int_rcv       ,
        d15_impt_aport_pat           ,
        d15_impt_total_int_pat       ,
        d15_impt_total_guber         ,
        d15_impt_total_int_gub       ,
        d15_impt_total_xpagar        ,
        d15_impt_total_xpagari       ,
        d15_impt_aport_acept         ,
        d15_impt_aport_dev           ,
        d13_impt_aport_acept         DECIMAL(15,2)
     DEFINE vfecha_archivo           CHAR(10),
            opc CHAR(01),
            vrecauda CHAR(01)
     DEFINE  G_LISTA           CHAR(500)
     DEFINE vsuma INTEGER,
            cuantos2 INTEGER
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    LET HOY = TODAY
    SELECT *
    INTO   g_param.*
    FROM   dis_parametro

	OPEN WINDOW ventana_1 AT 4,4 WITH FORM "DISC0031" ATTRIBUTE(BORDER)
        DISPLAY " DISC002  CARGA ARCHIVO tmp_pla PARA LA DISPERSION DE CUOTAS                                 " AT 3,1 ATTRIBUTE(REVERSE)

	DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
 	DISPLAY HOY USING"DD-MM-YYYY" AT 3,63 ATTRIBUTE(REVERSE)

        OPEN WINDOW v1 AT 10,24 WITH 01 ROWS, 33 COLUMNS ATTRIBUTE(BORDER)
        MENU "Recaudacion"
           COMMAND "Normal"
              LET vrecauda = "N"
              EXIT MENU
           COMMAND "Especial"
              LET vrecauda = "E"
              EXIT MENU
        END MENU
        CLOSE WINDOW v1

        IF vrecauda = "E" THEN
           ERROR "Buscando nss validados ..."
           CALL prepara_especial() 
           ERROR ""
        END IF

	INPUT BY NAME generar
	    AFTER FIELD generar
	        IF generar IS NULL THEN
		    ERROR "Campo NO puede ser NULO"
		    NEXT FIELD generar
	        END IF
                PROMPT "Es correcto el Archivo [S/N]... " FOR opc
                IF opc MATCHES '[sS]' THEN

                   CALL init()
                   WHENEVER ERROR CONTINUE
        ERROR "PROCESANDO INFORMACION "
                      CALL crea_tablas()
  
                      LET comando=g_param.ruta_rescate CLIPPED,"/",generar

                      LOAD FROM comando INSERT INTO safre_tmp:tmp_pla_disp
                      
                      IF STATUS = NOTFOUND THEN
                         DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
                         EXIT PROGRAM
                      ELSE
                         EXIT INPUT
                      END IF
                    WHENEVER ERROR STOP

                ELSE
                   NEXT FIELD generar
                END IF
           ON KEY(INTERRUPT)
              ERROR "PROCESO CANCELADO"
              SLEEP 2
              EXIT PROGRAM 
	END INPUT
        ERROR "PROCESANDO INFORMACION "

        CALL Actualiza_con_datos_entregados_por_consar_cuotas() #adc

        DECLARE cur_1 CURSOR FOR
        SELECT UNIQUE(tipo_registro)
        FROM   safre_tmp:detalle
        ORDER BY 1

        FOREACH cur_1 INTO c_tipo_registro
            CASE c_tipo_registro
                WHEN "02"
                    CALL valida_02() #02
                WHEN "03"
                    CALL valida_03() #03
            END CASE
        END FOREACH
        CALL valida_01() #01
        CALL valida_09() #09

        DECLARE cur_7 CURSOR FOR
        SELECT *
        FROM   safre_tmp:rechzo
        ORDER BY consec_reg_lote,tipo_registro

        LET G_LISTA = g_param.ruta_envio CLIPPED,"/det_03"

        #LET i_consec_rec_lote = 0
        START REPORT det_listado TO G_LISTA
            FOREACH cur_7 INTO reg_det.*
               #LET i_consec_rec_lote = i_consec_rec_lote + 1
               #LET reg_det.consec_reg_lote = i_consec_rec_lote USING "&&&&&&&&"
                OUTPUT TO REPORT det_listado(reg_det.*)
            END FOREACH
        FINISH REPORT det_listado


       SELECT count(*)
       INTO  cuantos
       FROM   safre_tmp:det_disp_cuotas

       SELECT count(*)
       INTO  cuantos2
       FROM   safre_tmp:det_disp_int

       IF STATUS = 100 OR cuantos2 IS NULL THEN
          LET cuantos2 = 0
       END IF

       LET vsuma = cuantos + cuantos2

       SELECT count(*) 
       INTO n_incons
       FROM dis_incons_interes 
       WHERE fech_pago = reg_det_i.fech_pago
       AND   nss =       reg_det_i.nss

       DISPLAY " Total Procesados : ",vsuma  AT 13,3
       DISPLAY "       Rechazados : ",num_reg_rchzo  AT 15,3

---       DISPLAY       "  Inconsistencias : ",n_incons AT 17,3

    ERROR ""

   PROMPT "Oprima una tecla para finalizar" for aux_pausa
{
      PROMPT  "  Deseas Generar Reporte de Inconsistencias  S/N ?" for CHAR aux_pausa
	IF aux_pausa MATCHES "[Ss]" THEN
        call reporte()
      ELSE
        ERROR "Proceso Terminado " ; sleep 1
        exit PROGRAM
      END IF
}
END MAIN

FUNCTION valida_01()
#01-----------------
    LET G_LISTA = g_param.ruta_envio CLIPPED,"/cza"
    START REPORT cza_listado TO G_LISTA
        LET reg_cza.tipo_registro      = "01" 
        LET reg_cza.ident_servicio     = "03" 
        LET reg_cza.ident_operacion    = "10" 
        LET reg_cza.tipo_ent_origen    = "01" 
        LET reg_cza.clave_ent_origen   = w_codigo_afore 
        LET reg_cza.tipo_ent_destino   = "03" 
        LET reg_cza.clave_ent_destino  = "001" 
       # LET reg_cza.fech_creac_lote    = ""
        #LET reg_cza.lote_del_dia       = ""
        LET reg_cza.mod_recp_envio     ="02" 
        LET reg_cza.fech_limite_resp   = reg_cza.fech_limite_resp 
        LET reg_cza.result_operacion   = "" 
        LET reg_cza.motivo_rechazo1    = ""  
        LET reg_cza.motivo_rechazo2    = "" 
        LET reg_cza.motivo_rechazo3    = "" 
        LET reg_cza.filler             = "" 

	OUTPUT TO REPORT cza_listado (reg_cza.*)
     FINISH REPORT cza_listado
END FUNCTION

FUNCTION valida_02()
#02----------------
    DEFINE 
        i_ind_rcv_salida      ,
        i_ind_viv_salida      INTEGER

    DEFINE 
        _ind_rcv_salida        ,
        _ind_viv_salida        ,
        i_impt_ret             ,
        i_impt_a_rec_ret     ,
        i_impt_ces_vej         , 
        i_ip_act_r_ces_vej   ,
        i_impt_aport_vol       , 
        i_impt_aport_pat       , 
        i_impt_cuota_soc       , 
        i_impt_aport_est       ,
        i_impt_aport_esp       ,
        i_impt_act_cuo_soc     ,
        i_ip_act_aport_est   ,
        i_impt_act_cuo_esp     INTEGER

    DEFINE 
        d97_ind_viv_salida     DECIMAL(9,7) ,
        d97_ind_rcv_salida     DECIMAL(9,7) ,
        d7_2_impt_ret          DECIMAL(7,2),
        d72_ip_act_rec_ret     DECIMAL(7,2),
        d7_2_impt_ces_vej      DECIMAL(7,2),
        d72_ip_a_r_ces_vej     DECIMAL(7,2),
        d72_impt_aport_vol     DECIMAL(7,2),
        d72_impt_aport_pat     DECIMAL(7,2),
        d72_impt_cuota_soc     DECIMAL(7,2),
        d72_impt_aport_est     DECIMAL(7,2),
        d72_impt_aport_esp     DECIMAL(7,2),
        d72_ip_act_cuo_soc     DECIMAL(7,2),
        d72_ip_a_aport_est     DECIMAL(7,2),
        d72_ip_act_cuo_esp     DECIMAL(7,2)
         
    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   safre_tmp:detalle
    WHERE  tipo_registro = c_tipo_registro
 
    FOREACH cur_2 INTO reg_det.*
        LET i_impt_ret          = reg_det.impt_ret
        LET i_impt_a_rec_ret    = reg_det.impt_act_rec_ret
        LET i_impt_ces_vej      = reg_det.impt_ces_vej 
        LET i_ip_act_r_ces_vej  = reg_det.impt_act_r_ces_vej
        LET i_impt_aport_vol    = reg_det.impt_aport_vol 
        LET i_impt_aport_pat    = reg_det.impt_aport_pat 
        LET i_impt_cuota_soc    = reg_det.impt_cuota_soc 
        LET i_impt_aport_est    = reg_det.impt_aport_est
        LET i_impt_aport_esp    = reg_det.impt_aport_esp
        LET i_impt_act_cuo_soc  = reg_det.impt_act_cuo_soc
        LET i_ip_act_aport_est  = reg_det.impt_act_aport_est
        LET i_impt_act_cuo_esp  = reg_det.impt_act_cuo_esp

        LET d7_2_impt_ret       = i_impt_ret/100
        LET d72_ip_act_rec_ret  = i_impt_a_rec_ret/100     
        LET d7_2_impt_ces_vej   = i_impt_ces_vej/100         
        LET d72_ip_a_r_ces_vej  = i_ip_act_r_ces_vej/100   
        LET d72_impt_aport_vol  = i_impt_aport_vol/100       
        LET d72_impt_aport_pat  = i_impt_aport_pat/100       
        LET d72_impt_cuota_soc  = i_impt_cuota_soc/100       
        LET d72_impt_aport_est  = i_impt_aport_est/100       
        LET d72_impt_aport_esp  = i_impt_aport_esp/100       
        LET d72_ip_act_cuo_soc  = i_impt_act_cuo_soc/100     
        LET d72_ip_a_aport_est  = i_ip_act_aport_est/100
        LET d72_ip_act_cuo_esp  = i_impt_act_cuo_esp/100     

{
        LET i_ind_rcv_salida    = reg_det.ind_rcv_salida[1,2]
        LET _ind_rcv_salida     = reg_det.ind_rcv_salida[3,9]
        LET d97_ind_rcv_salida  = i_ind_rcv_salida +(_ind_rcv_salida/10000000) 

        LET i_ind_viv_salida    = reg_det.ind_viv_salida[1,2]
        LET _ind_viv_salida     = reg_det.ind_viv_salida[3,9]
        LET d97_ind_viv_salida  = i_ind_viv_salida +(_ind_viv_salida/10000000) 
}

       INSERT INTO safre_tmp:det_disp_cuotas VALUES(reg_det.tipo_registro      ,
                                           reg_det.ident_servicio     ,
                                           reg_det.consec_reg_lote    ,
                                           reg_det.NSS                ,
                                           reg_det.RFC                ,
                                           reg_det.CURP               ,
                                           reg_det.nom_trabajador     ,
                                           reg_det.periodo_pago       ,
                                           reg_det.fech_pago          ,
                                           reg_det.fech_valor_rcv     ,
                                           reg_det.fech_valor_viv     ,
                                           reg_det.ult_salario_diario ,
                                           reg_det.folio_pago_SUA     ,
                                           reg_det.reg_patronal_IMSS  ,
                                           reg_det.rfc_patron         ,
                                           reg_det.cve_ent_receptora  ,
                                           reg_det.dias_cotz_bimest   ,
                                           reg_det.dias_incap_bimest  ,
                                           reg_det.dias_ausent_bimest ,
                                           d7_2_impt_ret              ,
                                           d72_ip_act_rec_ret         ,
                                           d7_2_impt_ces_vej          ,
                                           d72_ip_a_r_ces_vej         ,
                                           d72_impt_aport_vol         ,
                                           d72_impt_aport_pat         ,
                                           d72_impt_cuota_soc         ,
                                           d72_impt_aport_est         ,
                                           d72_impt_aport_esp         ,
                                           d72_ip_act_cuo_soc         ,
                                           d72_ip_a_aport_est         ,
                                           d72_ip_act_cuo_esp         ,
                                           reg_det.fecha_pago_gub,
                                           reg_det.filler0,
                                        ####  d97_ind_rcv_salida         ,
                                        ####   d97_ind_viv_salida         ,
                                           "01", ---reg_det.result_operacion   ,
                                           reg_det.det_mot_rechazo1   ,
                                           reg_det.det_mot_rechazo2   ,
                                           reg_det.det_mot_rechazo3   ,
                                           reg_det.filler         
                                          );
    END FOREACH
    CALL valida_det_cuotas() #vdc

    SELECT SUM(impt_ret           +
               impt_act_rec_ret   +
               impt_ces_vej       +
               impt_act_r_ces_vej +
               impt_aport_vol
              )
    INTO   d15_impt_total_rcv
    FROM   safre_tmp:det_disp_cuotas
    WHERE  result_operacion = "02"

    SELECT SUM(impt_aport_pat)
    INTO   d15_impt_aport_pat
    FROM   safre_tmp:det_disp_cuotas
    WHERE  result_operacion = "02"

    SELECT SUM(impt_cuota_soc     +
               impt_aport_est     +
               impt_aport_esp     +
               impt_act_cuo_soc   +
               impt_act_aport_est +
               impt_act_cuo_esp
              )
    INTO   d15_impt_total_guber
    FROM   safre_tmp:det_disp_cuotas
    WHERE  result_operacion = "02"

    SELECT COUNT(*)
    INTO   i_num_de_reg_aport
    FROM   safre_tmp:det_disp_cuotas
    WHERE  result_operacion = "02"

    SELECT SUM(impt_ret           +
               impt_act_rec_ret   +
               impt_ces_vej       +
               impt_act_r_ces_vej +
               impt_aport_vol     +
               impt_aport_pat     +
               impt_cuota_soc     +
               impt_aport_est     +
               impt_aport_esp     +
               impt_act_cuo_soc   +
               impt_act_aport_est +
               impt_act_cuo_esp
              )
    INTO   d15_impt_aport_acept
    FROM   safre_tmp:det_disp_cuotas
    WHERE  result_operacion = "01"

    LET d15_impt_aport_dev = d15_impt_total_rcv   +
                             d15_impt_aport_pat   +
                             d15_impt_total_guber +
                             i_num_de_reg_aport

END FUNCTION

FUNCTION valida_03()
#03----------------
    DEFINE
        _ind_rcv_salida        ,
        _ind_viv_salida        INTEGER
    
    DEFINE
        i_impt_i_ret           ,
        i_impt_i_act_rr        ,
        i_impt_i_ces_vej       ,
        i_impt_i_act_rcv       ,
        i_impt_i_aport_vol     ,
        i_impt_i_aport_pat     ,
        i_impt_i_cuota_soc     ,
        i_impt_i_aport_est     ,
        i_impt_i_aport_esp     ,
        i_impt_i_act_cuo_s     ,
        i_impt_i_act_est       ,
        i_impt_i_a_cuo_esp     ,
        i_ind_rcv_salida       ,
        i_ind_viv_salida       INTEGER

    DEFINE
        d_impt_i_ret           ,
        d_impt_i_act_rr        ,
        d_impt_i_ces_vej       ,
        d_impt_i_act_rcv       ,
        d_impt_i_aport_vol     ,
        d_impt_i_aport_pat     ,
        d_impt_i_cuota_soc     ,
        d_impt_i_aport_est     ,
        d_impt_i_aport_esp     ,
        d_impt_i_act_cuo_s     ,
        d_impt_i_act_est       ,
        d_impt_i_a_cuo_esp     DECIMAL(5,2) ,
        d97_ind_rcv_salida     ,
        d97_ind_viv_salida     DECIMAL(9,7)

  
    DECLARE cur_6 CURSOR FOR
    SELECT *
    FROM   safre_tmp:detalle
    WHERE  tipo_registro = c_tipo_registro
 
    FOREACH cur_6 INTO reg_det_i.*
        LET i_impt_i_ret       = reg_det_i.impt_int_ret
        LET i_impt_i_act_rr    = reg_det_i.impt_int_act_rr
        LET i_impt_i_ces_vej   = reg_det_i.impt_int_ces_vej 
        LET i_impt_i_act_rcv   = reg_det_i.impt_int_act_rcv
        LET i_impt_i_aport_vol = reg_det_i.impt_int_aport_vol 
        LET i_impt_i_aport_pat = reg_det_i.impt_int_aport_pat 
        LET i_impt_i_cuota_soc = reg_det_i.impt_int_cuota_soc 
        LET i_impt_i_aport_est = reg_det_i.impt_int_aport_est
        LET i_impt_i_aport_esp = reg_det_i.impt_int_aport_esp
        LET i_impt_i_act_cuo_s = reg_det_i.impt_int_act_cuo_s
        LET i_impt_i_act_est   = reg_det_i.impt_int_act_est
        LET i_impt_i_a_cuo_esp = reg_det_i.impt_int_a_cuo_esp

        LET d_impt_i_ret       = i_impt_i_ret/100
        LET d_impt_i_act_rr    = i_impt_i_act_rr/100     
        LET d_impt_i_ces_vej   = i_impt_i_ces_vej/100         
        LET d_impt_i_act_rcv   = i_impt_i_act_rcv/100   
        LET d_impt_i_aport_vol = i_impt_i_aport_vol/100       
        LET d_impt_i_aport_pat = i_impt_i_aport_pat/100       
        LET d_impt_i_cuota_soc = i_impt_i_cuota_soc/100       
        LET d_impt_i_aport_est = i_impt_i_aport_est/100       
        LET d_impt_i_aport_esp = i_impt_i_aport_esp/100       
        LET d_impt_i_act_cuo_s = i_impt_i_act_cuo_s/100     
        LET d_impt_i_act_est   = i_impt_i_act_est/100
        LET d_impt_i_a_cuo_esp = i_impt_i_a_cuo_esp/100
{
        LET i_ind_rcv_salida   = reg_det_i.ind_rcv_salida[1,2]
        LET _ind_rcv_salida    = reg_det_i.ind_rcv_salida[3,9]
        LET d97_ind_rcv_salida = i_ind_rcv_salida +(_ind_rcv_salida/10000000) 
        LET i_ind_viv_salida   = reg_det_i.ind_vivieda_salida[1,2]
        LET _ind_viv_salida    = reg_det_i.ind_vivieda_salida[3,9]
        LET d97_ind_viv_salida = i_ind_viv_salida +(_ind_viv_salida/10000000) 
}

        INSERT INTO safre_tmp:det_disp_int VALUES(reg_det_i.tipo_registro      ,
                                        reg_det_i.ident_servicio     ,
                                        reg_det_i.consec_reg_lote    ,
                                        reg_det_i.NSS                ,
                                        reg_det_i.RFC                ,
                                        reg_det_i.CURP               ,
                                        reg_det_i.nom_trabajador     ,
                                        reg_det_i.periodo_pago       ,
                                        reg_det_i.fech_pago          ,
                                        reg_det_i.fech_valor_rcv     ,
                                        reg_det_i.fech_valor_viv     ,
                                        reg_det_i.ult_salario_diario ,
                                        reg_det_i.folio_pago_SUA     ,
                                        reg_det_i.reg_patronal_IMSS  ,
                                        reg_det_i.rfc_patron         ,
                                        reg_det_i.cve_ent_receptora  ,
                                        reg_det_i.dias_cotz_bimest   ,
                                        reg_det_i.dias_incap_bimest  ,
                                        reg_det_i.dias_ausent_bimest ,
                                        d_impt_i_ret               ,
                                        d_impt_i_act_rr              ,
                                        d_impt_i_ces_vej           ,
                                        d_impt_i_act_rcv             ,
                                        d_impt_i_aport_vol         ,
                                        d_impt_i_aport_pat         ,
                                        d_impt_i_cuota_soc         ,
                                        d_impt_i_aport_est         ,
                                        d_impt_i_aport_esp         ,
                                        d_impt_i_act_cuo_s         ,
                                        d_impt_i_act_est           ,
                                        d_impt_i_a_cuo_esp         ,
                                        reg_det_i.fecha_pago_gub,
                                        reg_det_i.filler0,
                                      ####  d97_ind_rcv_salida         ,
                                      ####  d97_ind_viv_salida         ,
                                        "01", ----reg_det_i.result_operacion   ,
                                        reg_det_i.det_mot_rechazo1   ,
                                        reg_det_i.det_mot_rechazo2   ,
                                        reg_det_i.det_mot_rechazo3   ,
                                        reg_det_i.filler         
                                        );
    END FOREACH
   CALL valida_det_int() #vdi

    SELECT SUM(impt_int_ret       +
               impt_int_act_rr    +
               impt_int_ces_vej   +
               impt_int_act_rcv   +
               impt_int_aport_vol
              )
    INTO   d15_impt_total_int_rcv
    FROM   safre_tmp:det_disp_int
    WHERE  result_operacion = "02"

    SELECT SUM(impt_int_aport_pat)
    INTO   d15_impt_total_int_pat
    FROM   safre_tmp:det_disp_int
    WHERE  result_operacion = "02"
 
    SELECT SUM(impt_int_cuota_soc +
               impt_int_aport_est +
               impt_int_aport_esp +
               impt_int_act_cuo_s +
               impt_int_act_est   +
               impt_int_a_cuo_esp
              )
    INTO   d15_impt_total_int_gub
    FROM   safre_tmp:det_disp_int
    WHERE  result_operacion = "02"
END FUNCTION

FUNCTION valida_09()
#09---------------
    DEFINE 
        c15_impt_total_rcv           ,
        c15_impt_total_int_rcv       ,
        c15_impt_aport_pat           ,
        c15_impt_total_int_pat       ,
        c15_impt_total_guber         ,
        c15_impt_total_int_gub       ,
        c15_impt_total_xpagar        ,
        c15_impt_aport_acept         CHAR(15), 
        c16_impt_total_rcv           , 
        c16_impt_total_int_rcv       ,
        c16_impt_aport_pat           ,
        c16_impt_total_int_pat       ,
        c16_impt_total_guber         ,
        c16_impt_total_int_gub       ,
        c16_impt_total_xpagar        ,
        c16_impt_aport_acept         CHAR(16) ,
        vtotal_aport_rech,
        vtotal_int_rech DECIMAL(15,2)

  
    IF d15_impt_total_rcv IS NULL THEN
       LET  c15_impt_total_rcv = "000000000000000"
    ELSE
       LET c16_impt_total_rcv = d15_impt_total_rcv USING"&&&&&&&&&&&&&.&&"
       LET c15_impt_total_rcv = c16_impt_total_rcv[01,13],
                                c16_impt_total_rcv[15,16]
    END IF

    IF d15_impt_total_int_rcv IS NULL THEN
       LET  c15_impt_total_int_rcv = "000000000000000"
    ELSE
    LET c16_impt_total_int_rcv = d15_impt_total_int_rcv USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_int_rcv = c16_impt_total_int_rcv[01,13],
                                 c16_impt_total_int_rcv[15,16]
    END IF

    IF d15_impt_aport_pat IS NULL  THEN
       LET  c15_impt_aport_pat = "000000000000000"
    ELSE
     LET c16_impt_aport_pat     = d15_impt_aport_pat USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_aport_pat     = c16_impt_aport_pat[1,13],
                                  c16_impt_aport_pat[15,16]
    END IF

    IF d15_impt_total_int_pat IS NULL THEN
        LET c15_impt_total_int_pat = "000000000000000"
    ELSE
     LET c16_impt_total_int_pat = d15_impt_total_int_pat USING"&&&&&&&&&&&&&.&&"
     LET c15_impt_total_int_pat = c16_impt_total_int_pat[1,13],
                                  c16_impt_total_int_pat[15,16]
    END IF

    IF d15_impt_total_guber IS NULL  THEN
       LET  c15_impt_total_guber = "000000000000000"
    ELSE
    LET c16_impt_total_guber = d15_impt_total_guber USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_guber = c16_impt_total_guber[1,13],
                               c16_impt_total_guber[15,16]
    END IF

   IF d15_impt_total_int_gub IS NULL THEN
    LET c15_impt_total_int_gub = "000000000000000"
   ELSE
    LET c16_impt_total_int_gub = d15_impt_total_int_gub USING"&&&&&&&&&&&&&.&&"
    LET c15_impt_total_int_gub = c16_impt_total_int_gub[1,13],
                                 c16_impt_total_int_gub[15,16]
   END IF
           SELECT sum(impt_ret +
	              impt_act_rec_ret +
	              impt_ces_vej     +
	              impt_act_r_ces_vej +
	              impt_aport_vol    +
	              impt_aport_pat    +
	              impt_cuota_soc    +
	              impt_aport_est    +
	              impt_aport_esp    +
	              impt_act_cuo_soc  +
	              impt_act_aport_est +
	              impt_act_cuo_esp )
	   INTO d15_impt_total_xpagar
	   FROM safre_tmp:det_disp_cuotas

           SELECT sum(impt_int_ret +
	              impt_int_act_rr +
	              impt_int_ces_vej     +
	              impt_int_act_rcv+
	              impt_int_aport_vol    +
	              impt_int_aport_pat    +
	              impt_int_cuota_soc    +
	              impt_int_aport_est    +
	              impt_int_aport_esp    +
	              impt_int_act_cuo_s  +
	              impt_int_act_est +
	              impt_int_a_cuo_esp )
	   INTO d15_impt_total_xpagari
	   FROM safre_tmp:det_disp_int

           SELECT sum(impt_ret +
	              impt_act_rec_ret +
	              impt_ces_vej     +
	              impt_act_r_ces_vej +
	              impt_aport_vol    +
	              impt_aport_pat    +
	              impt_cuota_soc    +
	              impt_aport_est    +
	              impt_aport_esp    +
	              impt_act_cuo_soc  +
	              impt_act_aport_est +
	              impt_act_cuo_esp )
	   INTO vtotal_aport_rech
	   FROM safre_tmp:det_disp_cuotas
           WHERE result_operacion = "02"

           IF vtotal_aport_rech IS NULL THEN
              LET vtotal_aport_rech = 0
           END IF

           SELECT sum(impt_int_ret +
	              impt_int_act_rr +
	              impt_int_ces_vej     +
	              impt_int_act_rcv+
	              impt_int_aport_vol    +
	              impt_int_aport_pat    +
	              impt_int_cuota_soc    +
	              impt_int_aport_est    +
	              impt_int_aport_esp    +
	              impt_int_act_cuo_s  +
	              impt_int_act_est +
	              impt_int_a_cuo_esp )
	   INTO vtotal_int_rech
	   FROM safre_tmp:det_disp_int
           WHERE result_operacion = "02"
        
           IF vtotal_int_rech IS NULL THEN
              LET vtotal_int_rech = 0
           END IF

    IF d15_impt_total_xpagari IS NULL  THEN
       LET  d15_impt_total_xpagari = 0
    END IF

    IF d15_impt_total_xpagar IS NULL  THEN
       LET  c15_impt_total_xpagar = "000000000000000"
    ELSE
 LET d15_impt_total_xpagar  =(d15_impt_total_xpagar + d15_impt_total_xpagari)
                            -(vtotal_aport_rech + vtotal_int_rech)

 LET c16_impt_total_xpagar    = d15_impt_total_xpagar USING"&&&&&&&&&&&&&.&&"
 LET c15_impt_total_xpagar    = c16_impt_total_xpagar[1,13],
                                c16_impt_total_xpagar[15,16]
    END IF

    IF d15_impt_aport_acept IS NULL  THEN
       LET  c15_impt_aport_acept = "000000000000000"
    ELSE
 LET c16_impt_aport_acept    = d15_impt_aport_acept USING"&&&&&&&&&&&&&.&&"
 LET c15_impt_aport_acept    = c16_impt_aport_acept[1,13],
                               c16_impt_aport_acept[15,16]
    END IF

    LET G_LISTA = g_param.ruta_envio CLIPPED,"/sum"
    START REPORT sum_listado TO G_LISTA
        LET reg_sum.tipo_registro          = "09" 
        LET reg_sum.ident_servicio         = "03" 
        LET reg_sum.ident_operacion        = "10" 
        LET reg_sum.tipo_ent_origen        = "01" 
        LET reg_sum.clave_ent_origen       = w_codigo_afore 
        LET reg_sum.tipo_ent_destino       = "03" 
        LET reg_sum.clave_ent_destino      = "001" 
        LET reg_sum.fech_creac_lote        = reg_cza.fech_creac_lote 
        LET reg_sum.lote_del_dia           = reg_cza.lote_del_dia 
        LET reg_sum.impt_total_rcv         = c15_impt_total_rcv 
        LET reg_sum.impt_total_int_rcv     = c15_impt_total_int_rcv
--        LET reg_sum.impt_total_int_rcv     = reg_sum.impt_total_int_rcv 
        LET reg_sum.impt_total_patr        = c15_impt_aport_pat 
        LET reg_sum.impt_tatal_int_pat     = c15_impt_total_int_pat
        LET reg_sum.impt_total_guber       = c15_impt_total_guber
        LET reg_sum.impt_total_int_gub     = c15_impt_total_int_gub
        LET reg_sum.num_de_reg_aport       = i_num_de_reg_aport
                                             USING"&&&&&&&&"
        LET reg_sum.impt_total_xpagar      = c15_impt_total_xpagar

        OUTPUT  TO REPORT sum_listado(reg_sum.*)
    FINISH REPORT sum_listado
END FUNCTION

FUNCTION init()
#--------------
    INITIALIZE reg_cza.*  TO NULL
    INITIALIZE reg_det.*  TO NULL
    INITIALIZE reg_sum.*  TO NULL
    INITIALIZE aux.*      TO NULL
    LET num_reg_rchzo     = 0
    LET num_reg_inconsist = 0
    LET registro_numero   = 0
    LET HOY               = TODAY

    SELECT codigo_afore
    INTO   w_codigo_afore
    FROM   tab_afore_local

    #LET nom_arch_generado = "PREFT.DP.A01",w_codigo_afore USING"&&&","CONFAF"
    LET nom_arch_generado = generar[1,8],".CONFAF"

END FUNCTION

FUNCTION valida_cza()
#vc------------------
    LET aux.result_operacion       = "01"
    LET aux.motivo_rechazo1        = NULL
    LET aux.motivo_rechazo2        = NULL
    LET aux.motivo_rechazo3        = NULL
    LET sw_1                       = 0
    LET sw_2                       = 0
    LET sw_3                       = 0

    SELECT *
    INTO   reg_cza.*
    FROM   safre_tmp:cza_disp_int

    IF reg_cza.tipo_registro <> "01" THEN
        LET aux.result_operacion = "02"
        LET aux.motivo_rechazo1  = "004"
        LET sw_1                 = 1 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO DE REGISTRO (ENCABEZADO), DEBE SER 01"
    END IF

    IF reg_cza.ident_servicio <> "03" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "005"
            LET sw_1                = 1
        ELSE
            LET aux.motivo_rechazo2 = "005"
            LET sw_2                = 1
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "IDENTIFICADOR DE SERVICIO (ENCABEZADO), DEBE SER 03"
    END IF
    
    IF reg_cza.ident_operacion <> "10" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "006"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "006"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "006"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                     "IDENTIFICADOR DE OPERACION (ENCABEZADO), DEBE SER 10 o 13"
                RETURN 
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "IDENTIFICADOR DE OPERACION (ENCABEZADO), DEBE SER 10 o 13"
    END IF
  
    IF reg_cza.tipo_ent_origen <> "03" THEN
        LET aux.result_operacion = "03"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "007"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "007"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "007"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN (ENCABEZADO), DEBE SER 03"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN (ENCABEZADO), DEBE SER 03"
    END IF

    IF reg_cza.clave_ent_origen <> "001" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "008"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "008"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "008"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "CLAVE ENTIDAD ORIGEN (ENCABEZADO), DEBE SER 001"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "CLAVE ENTIDAD ORIGEN (ENCABEZADO), DEBE SER 001"
    END IF
  
    IF reg_cza.tipo_ent_destino <> "01" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "009"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "009"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "009"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD DESTINO (ENCABEZADO), DEBE SER 01"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD DESTINO (ENCABEZADO), DEBE SER 01"
    END IF
  
    IF reg_cza.clave_ent_destino <> w_codigo_afore THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "010"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "010"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "010"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "CLAVE ENTIDAD DESTINO (ENCABEZADO), DEBE SER ",w_codigo_afore
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "CLAVE ENTIDAD DESTINO (ENCABEZADO), DEBE SER ",w_codigo_afore
    END IF

    IF (NOT val_formato_fech(reg_cza.fech_creac_lote)) OR d_fecha > HOY THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "011"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "011"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "011"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "FECHA DE CREACION LOTE ERRONEO (ENCABEZADO) "
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "FECHA DE CREACION LOTE ERRONEO (ENCABEZADO) "
    END IF

    IF reg_cza.lote_del_dia < 1 THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "021"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "021"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "021"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                        "SECUENCIA DE LOTE (ENCABEZADO), ERRONEO"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "SECUENCIA DE LOTE (ENCABEZADO), ERRONEO"
    ELSE 
        SELECT lotes_correlativo
        FROM   tab_lote
        WHERE  lotes_correlativo = reg_cza.lote_del_dia
        AND    lotes_fecha       = HOY
        AND    lotes_cod         = 1

        IF STATUS <> NOTFOUND THEN
            LET aux.result_operacion = "02"
            IF sw_1 = 0 THEN
                LET aux.motivo_rechazo1 = "030"
                LET sw_1                = 1
            ELSE
                IF sw_2 = 0 THEN      
                    LET aux.motivo_rechazo2 = "030"
                    LET sw_2                = 1
                ELSE
                    LET aux.motivo_rechazo3 = "030"
                    LET sw_3                = 1
                    DISPLAY "SE RECHAZA EL LOTE. ",
                            "LOTE DUPLICADO (ENCABEZADO)"
                    RETURN
                END IF
            END IF 
            DISPLAY "SE RECHAZA EL LOTE. ",
                    "LOTE DUPLICADO (ENCABEZADO)"
        END IF
    END IF
  
    IF reg_cza.mod_recp_envio <> "01" and reg_cza.mod_recp_envio <> "02"
                                      and reg_cza.mod_recp_envio <> "05" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "013"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "013"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "013"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "MODALIDAD DE RECEPCION DEL ARCHIVO INVALIDO (ENCABEZADO) ",
                "DEBE SER 01 o 02 o 05"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "MODALIDAD DE RECEPCION DEL ARCHIVO INVALIDO (ENCABEZADO) ",
                "DEBE SER 01 o 02 o 05"
    END IF

    IF  NOT val_formato_fech(reg_cza.fech_limite_resp) THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "111"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "111"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "111"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "FORMATO FECHA LIMITE DE RESPUESTA ERRONEO (ENCABEZADO) ",
                "DEBE SER AAAAMMDD"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "FORMATO FECHA LIMITE DE RESPUESTA ERRONEO (ENCABEZADO) ",
                "DEBE SER AAAAMMDD"
    END IF
END FUNCTION


FUNCTION valida_det_int()
#vdi--------------------
    DEFINE suma_acept RECORD
        impt_ret                  decimal(07,2)
    END RECORD
#            LET contrec = 0

    DECLARE cur_5 CURSOR FOR
    SELECT *
    FROM   safre_tmp:detalle
    WHERE  tipo_registro = c_tipo_registro

    FOREACH cur_5 INTO reg_det_i.*
        CALL func_detalle_int()#fdi

        IF auk.result_operacion = "02" THEN
 #           LET contrec = contrec + 1
            LET reg_det_i.result_operacion = "02"
            LET reg_det_i.det_mot_rechazo1 = auk.det_mot_rechazo1

            SELECT MAX(folio)+1  
            INTO  reg_det_i.folio 
            FROM  glo_folio

            INSERT INTO safre_tmp:rechzo VALUES(reg_det_i.*)
        END IF

        IF s_inconsist_1 = 1 THEN
            LET reg_det.result_operacion = auh.result_operacion
            LET reg_det.det_mot_rechazo1 = auh.det_mot_inconsist1
            LET reg_det.det_mot_rechazo2 = auh.det_mot_inconsist2
            LET reg_det.det_mot_rechazo3 = auh.det_mot_inconsist3

            INSERT INTO dis_incons_interes VALUES(reg_det_i.*)
        END IF
    END FOREACH
END FUNCTION


FUNCTION func_detalle_int()
#fdi------------------
    DEFINE 
        c_HOY                 CHAR(10) ,
        c_periodo             CHAR(06)

    DEFINE 
        s1_periodo            ,
        s2_periodo            INTEGER

    LET auk.result_operacion = "01"
    LET auh.result_operacion = "01"
    LET auk.det_mot_rechazo1 = NULL
    LET auk.det_mot_rechazo2 = NULL
    LET auk.det_mot_rechazo3 = NULL
    LET det_sw_1             = 0
    LET det_sw_2             = 0
    LET det_sw_3             = 0
    LET s_inconsist_1        = 0
    LET s_inconsist_2        = 0
    LET s_inconsist_3        = 0
    LET registro_numero      = registro_numero + 1
    LET sw_inconsist         = 0
    LET sw_rchzo             = 0

    IF reg_det_i.NSS IS NULL OR reg_det_i.NSS = " " THEN
        CALL cont_rchzo()
        LET auk.result_operacion = "02"
        LET auk.det_mot_rechazo1 = "169"
        LET det_sw_1             = 1 
        UPDATE safre_tmp:det_disp_int
        SET safre_tmp:det_disp_int.result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_int.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_int.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_int.det_mot_rechazo3  = auk.det_mot_rechazo3 
        WHERE NSS = reg_det_i.NSS     
        RETURN
    ELSE
       IF vrecauda="N" THEN
          SELECT "X" FROM afi_mae_afiliado
          where n_seguro = reg_det_i.NSS 
	  group by 1
       ELSE
          SELECT "X" FROM safre_tmp:maeafi_esp
          where n_seguro = reg_det_i.NSS 
	  group by 1
       END IF

        IF STATUS = NOTFOUND THEN
           CALL cont_rchzo()
           LET auk.result_operacion = "02"
           LET auk.det_mot_rechazo1 = "169"
           LET det_sw_1             = 1 
           UPDATE safre_tmp:det_disp_int
           SET safre_tmp:det_disp_int.result_operacion  = auk.result_operacion ,
               safre_tmp:det_disp_int.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
               safre_tmp:det_disp_int.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
               safre_tmp:det_disp_int.det_mot_rechazo3  = auk.det_mot_rechazo3 
           WHERE NSS = reg_det_i.NSS     
           RETURN
        ELSE
           LET s_largo = LENGTH(reg_det_i.NSS)
           IF s_largo < 10 THEN  
               CALL cont_rchzo()
               LET auk.result_operacion = "02"
               LET auk.det_mot_rechazo1 = "169"
               LET det_sw_1             = 1 
               UPDATE safre_tmp:det_disp_int
               SET safre_tmp:det_disp_int.result_operacion  = auk.result_operacion ,
                   safre_tmp:det_disp_int.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
                   safre_tmp:det_disp_int.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
                   safre_tmp:det_disp_int.det_mot_rechazo3  = auk.det_mot_rechazo3 
               WHERE NSS = reg_det_i.NSS     
               RETURN
           ELSE
               IF NOT valida_NSS(reg_det_i.NSS,s_largo) THEN
                   CALL cont_rchzo()
                   LET auk.result_operacion = "02"
                   LET auk.det_mot_rechazo1 = "169"
                   LET det_sw_1             = 1 
                   UPDATE safre_tmp:det_disp_int
                   SET safre_tmp:det_disp_int.result_operacion  = auk.result_operacion ,
                       safre_tmp:det_disp_int.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
                       safre_tmp:det_disp_int.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
                       safre_tmp:det_disp_int.det_mot_rechazo3  = auk.det_mot_rechazo3 
                   WHERE NSS = reg_det_i.NSS     
                   RETURN
               END IF
           END IF
       END IF
    END IF

{
    IF reg_det_i.RFC IS NULL OR reg_det_i.RFC = " " THEN
        CALL cont_inconsist()
        IF s_inconsist_1 = 0 THEN
            LET auh.det_mot_inconsist1 = "187"
            LET s_inconsist_1          = 1
        END IF 
    ELSE
        IF LENGTH(reg_det_i.RFC) < 10 THEN
            CALL cont_inconsist()
            IF s_inconsist_1 = 0 THEN
                LET auh.det_mot_inconsist1 = "187"
                LET s_inconsist_1          = 1
            ELSE
                LET auh.det_mot_inconsist2 = "187"
                LET s_inconsist_2          = 1
            END IF 
        ELSE
            IF NOT valida_fecha_rfc(reg_det_i.RFC[5,10]) OR
                             NOT valida_4letras_rfc(reg_det_i.RFC[1,4]) THEN #v4r
                CALL cont_inconsist()
                IF s_inconsist_1 = 0 THEN
                    LET auh.det_mot_inconsist1 = "187"
                    LET s_inconsist_1          = 1
                ELSE
                    LET auh.det_mot_inconsist2 = "187"
                    LET s_inconsist_2          = 1
                END IF 
            END IF
        END IF
    END IF
        IF vrecauda="N" THEN
           SELECT  "X" from afi_mae_afiliado
           WHERE n_rfc = reg_det_i.rfc 
            AND  n_seguro = reg_det_i.NSS
	    group by 1
         ELSE
           SELECT  "X" from safre_tmp:maeafi_esp
           WHERE n_rfc = reg_det_i.rfc 
            AND  n_seguro = reg_det_i.NSS
	    group by 1
         END IF
              IF STATUS = NOTFOUND THEN
                CALL cont_inconsist()
                IF s_inconsist_1 = 0 THEN
                    LET auh.det_mot_inconsist1 = "187"
                    LET s_inconsist_1          = 1
                END IF
             END IF

    IF reg_det_i.CURP IS NULL OR reg_det_i.CURP = " " THEN
        CALL cont_inconsist()
        IF det_sw_1 = 0 THEN
            LET auk.det_mot_rechazo1 = "188"
            LET det_sw_1                = 1
        ELSE
            IF det_sw_2 = 0 THEN
                LET auk.det_mot_rechazo2 = "188"
                LET det_sw_2                = 1
            ELSE
                LET auk.det_mot_rechazo3 = "188"
                LET det_sw_3 = 1
                RETURN
            END IF
        END IF 
    END IF
        
    IF reg_det_i.NSS IS NOT NULL THEN
        UPDATE safre_tmp:det_disp_int
        SET safre_tmp:det_disp_int.result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_int.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_int.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_int.det_mot_rechazo3  = auk.det_mot_rechazo3 
        WHERE NSS = reg_det_i.NSS     
    ELSE
        IF reg_det_i.CURP IS NOT NULL THEN
            UPDATE safre_tmp:det_disp_int
            SET safre_tmp:det_disp_int.result_operacion = auk.result_operacion ,
                safre_tmp:det_disp_int.det_mot_rechazo1 = auk.det_mot_rechazo1 ,
                safre_tmp:det_disp_int.det_mot_rechazo2 = auk.det_mot_rechazo2 ,
                safre_tmp:det_disp_int.det_mot_rechazo3 = auk.det_mot_rechazo3 
            WHERE CURP = reg_det_i.CURP   
        END IF
    END IF
}
END FUNCTION

FUNCTION valida_sum()
#vs------------------
    LET aux.result_operacion       = "01"
    LET aux.motivo_rechazo1        = NULL
    LET aux.motivo_rechazo2        = NULL
    LET aux.motivo_rechazo3        = NULL
    LET sw_1                       = 0
    LET sw_2                       = 0
    LET sw_3                       = 0

    SELECT *
    INTO   reg_sum.*
    FROM   sum_disp_int
  
    IF reg_sum.tipo_registro <> "09" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "004"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "004"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "004"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                        "TIPO DE REGISTRO (SUMARIO), DEBE SER 09"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO DE REGISTRO (SUMARIO), DEBE SER 09"
    END IF
 
    IF reg_sum.ident_servicio <> "03" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "035"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "035"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "035"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                        "IDENTIFICADOR DE SERVICIO ERRONEO(SUMARIO),DEBE SER 03"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "IDENTIFICADOR DE SERVICIO ERRONEO(SUMARIO),DEBE SER 03"
    END IF
  
    IF reg_sum.ident_operacion <> "10" AND reg_sum.ident_operacion <> "13" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "036"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "036"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "036"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                  "IDENTIFICADOR DE OPERACION ERRONEO(SUMARIO),DEBE SER 10 o 13"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "IDENTIFICADOR DE OPERACION ERRONEO(SUMARIO),DEBE SER 10 o 13"
    END IF
  
    IF reg_sum.tipo_ent_origen <> "03" THEN
        LET aux.result_operacion = "02"
        IF sw_1                  = 0 THEN
            LET aux.motivo_rechazo1 = "037"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "037"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "037"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN ERRONEO (SUMARIO),DEBE SER 03 "
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN ERRONEO (SUMARIO),DEBE SER 03 "
    END IF

    IF reg_sum.clave_ent_origen <> "001" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "038"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "038"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "038"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                        "CLAVE ENTIDAD ORIGEN ERRONEA (SUMARIO), DEBE SER 001"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "CLAVE ENTIDAD ORIGEN ERRONEA (SUMARIO), DEBE SER 001"
    END IF

    IF reg_sum.tipo_ent_destino <> "01" THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "049"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "049"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "049"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                        "TIPO ENTIDAD DESTINO (SUMARIO), DEBE SER 01"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD DESTINO (SUMARIO), DEBE SER 01"
    END IF

    IF reg_sum.clave_ent_destino <> w_codigo_afore THEN
        LET aux.result_operacion = "02"
        IF sw_1                  = 0 THEN
            LET aux.motivo_rechazo1 = "050"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "050"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "050"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN ERRONEO (SUMARIO),DEBE SER ",w_codigo_afore
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "TIPO ENTIDAD ORIGEN ERRONEO (SUMARIO),DEBE SER ",w_codigo_afore
    END IF

    IF reg_sum.fech_creac_lote <> reg_cza.fech_creac_lote THEN
        LET aux.result_operacion = "02"
        IF sw_1                  = 0 THEN
            LET aux.motivo_rechazo1 = "039"
            LET sw_1                = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "039"
                LET sw_2                = 1
            ELSE
                LET aux.motivo_rechazo3 = "039"
                LET sw_3                = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
              "FECHA DE CREACION LOTE (SUMARIO),NO ES IGUAL A LA DEL ENCAVEZADO"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
              "FECHA DE CREACION LOTE (SUMARIO),NO ES IGUAL A LA DEL ENCAVEZADO"
    END IF

    IF reg_sum.lote_del_dia <> reg_cza.lote_del_dia THEN
        LET aux.result_operacion = "02"
        IF sw_1 = 0 THEN
            LET aux.motivo_rechazo1 = "051"
            LET sw_1            = 1
        ELSE
            IF sw_2 = 0 THEN      
                LET aux.motivo_rechazo2 = "051"
                LET sw_2 = 1
            ELSE
                LET aux.motivo_rechazo3 = "051"
                LET sw_3 = 1
                DISPLAY "SE RECHAZA EL LOTE. ",
                       "CONSECUTIVO LOTES (SUMARIO),DIFERENTE AL DEL ENCABEZADO"
                RETURN
            END IF
        END IF 
        DISPLAY "SE RECHAZA EL LOTE. ",
                "CONSECUTIVO LOTES (SUMARIO),DIFERENTE AL DEL ENCABEZADO"
    END IF
END FUNCTION

FUNCTION val_formato_fech(AAAAMMDD)
#vff------------------------------
    DEFINE
        AAAAMMDD      CHAR(8) 

    DEFINE
        x ARRAY[8] OF CHAR(1)

    DEFINE
        t             INTEGER

    LET x[1] =AAAAMMDD[1]
    LET x[2] =AAAAMMDD[2]
    LET x[3] =AAAAMMDD[3]
    LET x[4] =AAAAMMDD[4]
    LET x[5] =AAAAMMDD[5]
    LET x[6] =AAAAMMDD[6]
    LET x[7] =AAAAMMDD[7]
    LET x[8] =AAAAMMDD[8]
#f

    FOR t = 1 TO 6
        IF x[t] <> "0" AND
           x[t] <> "1" AND
           x[t] <> "2" AND
           x[t] <> "3" AND
           x[t] <> "4" AND
           x[t] <> "5" AND
           x[t] <> "6" AND
           x[t] <> "7" AND
           x[t] <> "8" AND
           x[t] <> "9" THEN
           RETURN  FALSE
        END IF
    END FOR

    LET AAAA                = NULL
    LET MM                  = NULL
    LET DD                  = NULL
    LET s_aaaa              = NULL
    LET s_dd                = NULL
    LET ff                  = NULL 
    LET AAAA                = AAAAMMDD[1,4]
    LET MM                  = AAAAMMDD[5,6]
    LET DD                  = AAAAMMDD[7,8]
    LET s_aaaa              = AAAA
    LET s_mm                = MM 
    LET s_dd                = DD
    LET c_fecha             = MM,"/",DD,"/",AAAA
    LET d_fecha             = c_fecha

    IF s_aaaa < 1900  OR  s_dd < 1 THEN
        RETURN FALSE
    ELSE
        CASE
            WHEN s_mm = 1  OR   s_mm = 3 
            OR   s_mm = 5  OR   s_mm = 7 
            OR   s_mm = 8  OR   s_mm = 10 
            OR   s_mm = 12
                IF s_dd > 31 THEN
                    RETURN FALSE
                END IF

            WHEN s_mm = 4 OR s_mm = 6 
            OR   s_mm = 9 OR s_mm = 11
                IF s_dd > 30  THEN
                    RETURN FALSE
                END IF

            WHEN s_mm = 2
                IF s_dd > 29 THEN
                    RETURN FALSE
                END IF
            OTHERWISE
                RETURN FALSE
        END CASE
    END IF
    RETURN TRUE
END FUNCTION

FUNCTION cont_inconsist()
#------------------------
    IF sw_inconsist = 0 THEN
        LET sw_inconsist      = 1
        LET num_reg_inconsist = num_reg_inconsist + 1
    END IF
END FUNCTION

FUNCTION cont_rchzo()
#--------------------
    IF sw_rchzo = 0 THEN
        LET sw_rchzo      = 1
        LET num_reg_rchzo = num_reg_rchzo + 1
    END IF
END FUNCTION

FUNCTION digito_verif(valor,longitud)
#dv----------------------------------
    DEFINE x array[10] OF CHAR(1)

    DEFINE
        cadena          CHAR(20) ,
        valor           CHAR(10) ,                                             
        sumachar        CHAR(02) ,                                         
        temp            CHAR(2)                                                

    DEFINE
        longitud        ,
        suma            ,                                              
        i               ,
	j               ,                                               
        ultima          ,
        t               INTEGER
                                                                               
    LET x[1] =valor[1]                                                      
    LET x[2] =valor[2]                                                      
    LET x[3] =valor[3]                                                      
    LET x[4] =valor[4]                                                      
    LET x[5] =valor[5]                                                      
    LET x[6] =valor[6]                                                      
    LET x[7] =valor[7]                                                      
    LET x[8] =valor[8]                                                      
    LET x[9] =valor[9]                                                      
    LET x[10] =valor[10]                                                    
                                                                               
    FOR t = 1 TO longitud 
        IF x[t] <> "0" AND 
           x[t] <> "1" AND  
           x[t] <> "2" AND   
           x[t] <> "3" AND    
           x[t] <> "4" AND     
           x[t] <> "5" AND      
           x[t] <> "6" AND       
           x[t] <> "7" AND        
           x[t] <> "8" AND         
           x[t] <> "9" THEN         

           LET digito = 32000       
           RETURN  digito      
        END IF                  
    END FOR                      

    LET j = 0                   
    FOR i = 1 TO longitud              
        LET j = j + 1              
        IF i MOD 2 = 0 THEN         
            LET temp      = valor[i] * 2  
            LET cadena[j] = temp[1]   
                                    
            IF LENGTH(temp) > 1 THEN    
                LET j = j + 1            
                LET cadena[j] = temp[2]   
            END IF                
        ELSE                        
            LET cadena[j] = valor[i]
        END IF                    
    END FOR                      
                                
    LET suma = 0                   
    FOR i = 1 TO j                  
        LET suma = suma + cadena[i]   
    END FOR                           
                                     
    LET sumachar = suma                 
    LET ultima   = LENGTH(sumachar)          
                                        
    LET digito = 10 - sumachar[ultima]     
    IF digito = 10 THEN  
        LET digito = 0    
    END IF       
    RETURN digito  
END FUNCTION      

FUNCTION valida_fecha_rfc(a)
#vfr-----------------------
    DEFINE
	x array[6]      OF CHAR(1)

    DEFINE
        a               CHAR(6)

    DEFINE
	t               INTEGER

    LET x[1] =a[1]
    LET x[2] =a[2]
    LET x[3] =a[3]
    LET x[4] =a[4]
    LET x[5] =a[5]
    LET x[6] =a[6]

    FOR t = 1 TO 6
        IF x[t] <> "0" AND
           x[t] <> "1" AND
           x[t] <> "2" AND
           x[t] <> "3" AND
           x[t] <> "4" AND
           x[t] <> "5" AND
           x[t] <> "6" AND
           x[t] <> "7" AND
           x[t] <> "8" AND
           x[t] <> "9" THEN
           RETURN  FALSE
        END IF
    END FOR
    RETURN TRUE
END FUNCTION

FUNCTION valida_4letras_rfc(c_4letras_rfc)
#v4r--------------------------------------
    DEFINE
	x array[4] OF CHAR(1)

    DEFINE
        c_4letras_rfc         CHAR(6)

    DEFINE
	t                     INTEGER

    LET x[1] =c_4letras_rfc[1]
    LET x[2] =c_4letras_rfc[2]
    LET x[3] =c_4letras_rfc[3]
    LET x[4] =c_4letras_rfc[4]

    FOR t = 1 TO 4
        IF x[t] = "0" OR
           x[t] = "1" OR
           x[t] = "2" OR
           x[t] = "3" OR
           x[t] = "4" OR
           x[t] = "5" OR
           x[t] = "6" OR
           x[t] = "7" OR
           x[t] = "8" OR
           x[t] = "9" OR 
           x[t] = " " THEN
           RETURN  FALSE
        END IF
    END FOR
    RETURN TRUE
END FUNCTION

FUNCTION valida_NSS(c11_NSS,s_largo)
#-----------------------------------
    DEFINE
	x array[11]  OF CHAR(1)

    DEFINE
        c11_NSS               CHAR(11)

    DEFINE
	t                     ,
        s_largo               INTEGER

    LET x[01] = c11_NSS[01]
    LET x[02] = c11_NSS[02]
    LET x[03] = c11_NSS[03]
    LET x[04] = c11_NSS[04]
    LET x[05] = c11_NSS[05]
    LET x[06] = c11_NSS[06]
    LET x[07] = c11_NSS[07]
    LET x[08] = c11_NSS[08]
    LET x[09] = c11_NSS[09]
    LET x[10] = c11_NSS[10]
    LET x[11] = c11_NSS[11]

    FOR t = 1 TO s_largo
        IF x[t] <> "0" AND
           x[t] <> "1" AND
           x[t] <> "2" AND
           x[t] <> "3" AND
           x[t] <> "4" AND
           x[t] <> "5" AND
           x[t] <> "6" AND
           x[t] <> "7" AND
           x[t] <> "8" AND
           x[t] <> "9" THEN
           RETURN  FALSE
        END IF
    END FOR
    RETURN TRUE
END FUNCTION

FUNCTION Actualiza_con_datos_entregados_por_consar_cuotas()
#adcc------------------------------------------------------
    DEFINE 
        cont_reg            ,
        total_reg           INTEGER
 
    DEFINE 
        carga_reg           CHAR(295),
	c16_impt_total_xpagar CHAR(16),
	c15_impt_total_xpagar CHAR(16),
	d15_impt_total_xpagar DECIMAL(13,2)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:tmp_pla_disp

    IF total_reg < 3 THEN
        DISPLAY "SE RECHAZA EL LOTE. ",
                "NUMERO DE REGISTROS NO SATISFACE EL MINIMO (minimo = 3) "
        EXIT PROGRAM
    END IF
        
    DECLARE cur_3 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:tmp_pla_disp

    FOREACH cur_3 INTO carga_reg
        LET cont_reg = cont_reg + 1
        IF cont_reg = 1 THEN
            LET reg_cza.tipo_registro         = carga_reg[001,002] 
            LET reg_cza.ident_servicio        = carga_reg[003,004]
            LET reg_cza.ident_operacion       = carga_reg[005,006]
            LET reg_cza.tipo_ent_origen       = carga_reg[007,008]
            LET reg_cza.clave_ent_origen      = carga_reg[009,011]
            LET reg_cza.tipo_ent_destino      = carga_reg[012,013]
            LET reg_cza.clave_ent_destino     = carga_reg[014,016]
            LET reg_cza.fech_creac_lote       = carga_reg[017,024]
            LET reg_cza.lote_del_dia          = carga_reg[025,027]
            LET reg_cza.mod_recp_envio        = carga_reg[028,029]
            LET reg_cza.fech_limite_resp      = carga_reg[030,037]
            LET reg_cza.result_operacion      = carga_reg[038,039]
            LET reg_cza.motivo_rechazo1       = carga_reg[040,042]
            LET reg_cza.motivo_rechazo2       = carga_reg[043,045]
            LET reg_cza.motivo_rechazo3       = carga_reg[046,048]
            LET reg_cza.filler                = carga_reg[049,295]

      	    INSERT INTO safre_tmp:cza_disp_cuotas VALUES(reg_cza.*)
        ELSE
           IF  cont_reg < total_reg THEN
               IF carga_reg[001,002] <> "08" THEN
                   LET reg_det.tipo_registro        = carga_reg[001,002]
                   LET reg_det.ident_servicio       = carga_reg[003,004]
                   LET reg_det.consec_reg_lote      = carga_reg[005,012]
                   LET reg_det.NSS                  = carga_reg[013,023]
                   LET reg_det.RFC                  = carga_reg[024,036]
                   LET reg_det.CURP                 = carga_reg[037,054]
                   LET reg_det.nom_trabajador       = carga_reg[055,104]
                   LET reg_det.periodo_pago         = carga_reg[105,110]
                   LET reg_det.fech_pago            = carga_reg[111,118]
                   LET reg_det.fech_valor_rcv       = carga_reg[119,126]
                   LET reg_det.fech_valor_viv       = carga_reg[127,134]
                   LET reg_det.ult_salario_diario   = carga_reg[135,141]
                   LET reg_det.folio_pago_SUA       = carga_reg[142,147]
                   LET reg_det.reg_patronal_IMSS    = carga_reg[148,158]
                   LET reg_det.rfc_patron           = carga_reg[159,171]
                   LET reg_det.cve_ent_receptora    = carga_reg[172,174]
                   LET reg_det.dias_cotz_bimest     = carga_reg[175,176]
                   LET reg_det.dias_incap_bimest    = carga_reg[177,178]
                   LET reg_det.dias_ausent_bimest   = carga_reg[179,180]
                   LET reg_det.impt_ret             = carga_reg[181,187]
                   LET reg_det.impt_act_rec_ret     = carga_reg[188,194]
                   LET reg_det.impt_ces_vej         = carga_reg[195,201]
                   LET reg_det.impt_act_r_ces_vej   = carga_reg[202,208]
                   LET reg_det.impt_aport_vol       = carga_reg[209,215]
                   LET reg_det.impt_aport_pat       = carga_reg[216,222]
                   LET reg_det.impt_cuota_soc       = carga_reg[223,229]
                   LET reg_det.impt_aport_est       = carga_reg[230,236]
                   LET reg_det.impt_aport_esp       = carga_reg[237,243]
                   LET reg_det.impt_act_cuo_soc     = carga_reg[244,250]
                   LET reg_det.impt_act_aport_est   = carga_reg[251,257]
                   LET reg_det.impt_act_cuo_esp     = carga_reg[258,264]
                   LET reg_det.fecha_pago_gub       = carga_reg[265,272]
                   LET reg_det.filler0              = carga_reg[273,282]
           ####        LET reg_det.ind_rcv_salida       = carga_reg[265,273]
           ####        LET reg_det.ind_viv_salida       = carga_reg[274,282]
                   LET reg_det.result_operacion     = carga_reg[283,284]
                   LET reg_det.det_mot_rechazo1     = carga_reg[285,287]
                   LET reg_det.det_mot_rechazo2     = carga_reg[288,290]
                   LET reg_det.det_mot_rechazo3     = carga_reg[291,293]
                   LET reg_det.filler               = carga_reg[294,295] 

      	           INSERT INTO safre_tmp:detalle VALUES(reg_det.*)
               ELSE
                   LET reg_pagos.tipo_registro      = carga_reg[001,002]
                   LET reg_pagos.ident_servicio     = carga_reg[003,004]
                   LET reg_pagos.ident_pago         = carga_reg[005,020]
                   LET reg_pagos.importe            = carga_reg[021,035]
                   LET reg_pagos.fech_liquidacion   = carga_reg[036,043]

               LET d13_impt_aport_acept = carga_reg[044,056]
               LET s_impt_aport_acept   = carga_reg[057,058]
               LET reg_pagos.impt_aport_acept = d13_impt_aport_acept +
                                               (s_impt_aport_acept/100)

                   LET reg_pagos.impt_aport_acept   = carga_reg[044,058]
                   LET reg_pagos.impt_aport_dev     = carga_reg[059,073]
----                   LET reg_pagos.filler             = carga_reg[074,295]
                   LET reg_pagos.estado             = 1 

                   LET vfecha_archivo = reg_cza.fech_creac_lote[5,6],"/",
                                        reg_cza.fech_creac_lote[7,8],"/",
                                        reg_cza.fech_creac_lote[1,4]
                   LET reg_pagos.fecha_archivo =  vfecha_archivo
                   LET reg_pagos.fecha_envio = TODAY
      	           INSERT INTO safre_tmp:pagos VALUES(reg_pagos.*)
                   
               END IF
           ELSE

               LET reg_sum.tipo_registro        = carga_reg[001,002] 
               LET reg_sum.ident_servicio       = carga_reg[003,004] 
               LET reg_sum.ident_operacion      = carga_reg[005,006] 
               LET reg_sum.tipo_ent_origen      = carga_reg[007,008] 
               LET reg_sum.clave_ent_origen     = carga_reg[009,011] 
               LET reg_sum.tipo_ent_destino     = carga_reg[012,013] 
               LET reg_sum.clave_ent_destino    = carga_reg[014,016] 
               LET reg_sum.fech_creac_lote      = carga_reg[017,024] 
               LET reg_sum.lote_del_dia         = carga_reg[025,027] 
               LET reg_sum.impt_total_rcv       = carga_reg[028,042]
               LET reg_sum.impt_total_int_rcv   = carga_reg[043,057]
               LET reg_sum.impt_total_patr      = carga_reg[058,072] 
               LET reg_sum.impt_tatal_int_pat   = carga_reg[073,087]
               LET reg_sum.impt_total_guber     = carga_reg[088,102] 
               LET reg_sum.impt_total_int_gub   = carga_reg[103,117] 
               LET reg_sum.num_de_reg_aport     = carga_reg[118,125] 
               LET reg_sum.num_cuenta_xpagar    = carga_reg[126,131] 
               LET reg_sum.impt_total_xpagar    = carga_reg[132,146] 
               LET reg_sum.filler               = carga_reg[147,295]
                
      	       INSERT INTO safre_tmp:sum_disp_cuotas VALUES(reg_sum.*)
           END IF
        END IF
    END FOREACH
END FUNCTION

FUNCTION valida_det_cuotas()
#vdc----------------------
    DEFINE suma_acept RECORD
        impt_ret                  decimal(07,2)
    END RECORD
     DEFINE contrec               INTEGER
     DEFINE continf               INTEGER

    LET contrec = 0
    LET continf = 0

    DECLARE cur_4 CURSOR FOR
    SELECT *
    FROM   safre_tmp:detalle
    WHERE  tipo_registro = c_tipo_registro

-----	UPDATE safre_tmp:det_disp_cuotas
-----	set result_operacion = "01"

    FOREACH cur_4 INTO reg_det.*
        CALL func_detalle_cuotas()#fdc

        IF auk.result_operacion = "02" THEN
            LET reg_det.result_operacion = "02"
            LET reg_det.det_mot_rechazo1 = auk.det_mot_rechazo1
            LET contrec = contrec + 1

            SELECT MAX(folio)+1  
            INTO  reg_det.folio 
            FROM  glo_folio

            INSERT INTO safre_tmp:rechzo VALUES(reg_det.*)
        END IF

        IF s_inconsist_1 = 1 THEN
            LET reg_det.result_operacion = auh.result_operacion
            LET reg_det.det_mot_rechazo1 = auh.det_mot_inconsist1
            LET reg_det.det_mot_rechazo2 = auh.det_mot_inconsist2
            LET reg_det.det_mot_rechazo3 = auh.det_mot_inconsist3

            LET continf = continf + 1
            INSERT INTO dis_incons_aporte VALUES(reg_det.*)
        END IF
    END FOREACH
END FUNCTION

FUNCTION func_detalle_cuotas()  #valida rechazos e inconsistencias
#fdc--------------------------
    DEFINE 
        c_HOY                 CHAR(10) ,
        vrfc                  CHAR(13) ,
        vpaterno              CHAR(40) ,
        c_periodo             CHAR(06)

    DEFINE 
        s1_periodo            ,
        s2_periodo            INTEGER

    LET auk.result_operacion = "01"
    LET auh.result_operacion = "01"
    LET auk.det_mot_rechazo1 = NULL
    LET auk.det_mot_rechazo2 = NULL
    LET auk.det_mot_rechazo3 = NULL
    LET det_sw_1             = 0
    LET det_sw_2             = 0
    LET det_sw_3             = 0
    LET s_inconsist_1        = 0
    LET s_inconsist_2        = 0
    LET s_inconsist_3        = 0
    LET registro_numero      = registro_numero + 1
    LET sw_inconsist         = 0
    LET sw_rchzo             = 0

    IF reg_det.NSS IS NULL OR reg_det.NSS = " " THEN
        CALL cont_rchzo()
        LET auk.result_operacion = "02"
        LET auk.det_mot_rechazo1 = "169"
        LET det_sw_1             = 1 
        UPDATE safre_tmp:det_disp_cuotas
        SET safre_tmp:det_disp_cuotas.result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo3  = auk.det_mot_rechazo3 
        WHERE NSS = reg_det.NSS     
        RETURN
    ELSE
      IF vrecauda="N" THEN
        SELECT n_rfc,paterno 
        INTO  vrfc,vpaterno FROM afi_mae_afiliado
        where n_seguro = reg_det.NSS 
      ELSE
        SELECT n_rfc,paterno 
        INTO  vrfc,vpaterno FROM safre_tmp:maeafi_esp
        where n_seguro = reg_det.NSS 
      END IF

        IF STATUS = NOTFOUND THEN
           CALL cont_rchzo()
           LET auk.result_operacion = "02"
           LET auk.det_mot_rechazo1 = "169"
           LET det_sw_1             = 1 
           UPDATE safre_tmp:det_disp_cuotas
           SET result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo3  = auk.det_mot_rechazo3 
           WHERE NSS = reg_det.NSS     
           RETURN
        ELSE
           LET s_largo = LENGTH(reg_det.NSS)
           IF s_largo < 10 THEN  
               CALL cont_rchzo()
               LET auk.result_operacion = "02"
               LET auk.det_mot_rechazo1 = "169"
               LET det_sw_1             = 1 
       UPDATE safre_tmp:det_disp_cuotas
        SET safre_tmp:det_disp_cuotas.result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo3  = auk.det_mot_rechazo3 
               WHERE NSS = reg_det.NSS     
               RETURN
           ELSE
               IF NOT valida_NSS(reg_det.NSS,s_largo) THEN
                   CALL cont_rchzo()
                   LET auk.result_operacion = "02"
                   LET auk.det_mot_rechazo1 = "169"
                   LET det_sw_1             = 1 
       UPDATE safre_tmp:det_disp_cuotas
        SET safre_tmp:det_disp_cuotas.result_operacion  = auk.result_operacion ,
           safre_tmp:det_disp_cuotas.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
           safre_tmp:det_disp_cuotas.det_mot_rechazo3  = auk.det_mot_rechazo3 
                   WHERE NSS = reg_det.NSS     
                   RETURN
               END IF
           END IF
       END IF
    END IF

{
        IF vrfc IS NULL OR vrfc = " " THEN 
        CALL cont_inconsist()
        IF s_inconsist_1 = 0 THEN
            LET auh.det_mot_inconsist1 = "187"
            LET s_inconsist_1          = 1
        END IF 
      END IF 
       
    IF reg_det.RFC IS NULL OR reg_det.RFC = " " THEN
        CALL cont_inconsist()
        IF s_inconsist_1 = 0 THEN
            LET auh.det_mot_inconsist1 = "187"
            LET s_inconsist_1          = 1
        END IF 
    ELSE
        IF LENGTH(reg_det.RFC) < 10 THEN
            CALL cont_inconsist()
            IF s_inconsist_1 = 0 THEN
                LET auh.det_mot_inconsist1 = "187"
                LET s_inconsist_1          = 1
            ELSE
                LET auh.det_mot_inconsist2 = "187"
                LET s_inconsist_2          = 1
            END IF 
        ELSE
            IF NOT valida_fecha_rfc(reg_det.RFC[5,10]) OR
                             NOT valida_4letras_rfc(reg_det.RFC[1,4]) THEN #v4r
                CALL cont_inconsist()
                IF s_inconsist_1 = 0 THEN
                    LET auh.det_mot_inconsist1 = "187"
                    LET s_inconsist_1          = 1
                ELSE
                    LET auh.det_mot_inconsist2 = "187"
                    LET s_inconsist_2          = 1
                END IF 
            END IF
        END IF
    END IF

    IF reg_det.CURP IS NULL OR reg_det.CURP = " " THEN
        CALL cont_inconsist()
        IF det_sw_1 = 0 THEN
            LET auk.det_mot_rechazo1 = "188"
            LET det_sw_1                = 1
        ELSE
            IF det_sw_2 = 0 THEN
                LET auk.det_mot_rechazo2 = "188"
                LET det_sw_2                = 1
            END IF
        END IF 
    END IF

    IF reg_det.nom_trabajador IS NULL OR reg_det.nom_trabajador = " " THEN
        CALL cont_inconsist()
        IF det_sw_1 = 0 THEN
            LET auk.det_mot_rechazo1 = "029" #A. paterno invalido en afi_canase
            LET det_sw_1     = 1
        ELSE
            IF det_sw_2 = 0 THEN
                LET auk.det_mot_rechazo2 = "029"
                LET det_sw_2 = 1
            ELSE
                LET auk.det_mot_rechazo3 = "029"
                LET det_sw_3 = 1
                RETURN
            END IF
        END IF 
    ELSE
        CALL valida_paterno(vpaterno) #vp
    END IF
    IF reg_det.NSS IS NOT NULL THEN
        UPDATE safre_tmp:det_disp_cuotas
        SET safre_tmp:det_disp_cuotas.result_operacion  = auk.result_operacion ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo1  = auk.det_mot_rechazo1 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo2  = auk.det_mot_rechazo2 ,
            safre_tmp:det_disp_cuotas.det_mot_rechazo3  = auk.det_mot_rechazo3 
        WHERE NSS = reg_det.NSS     
    ELSE
        IF reg_det.CURP IS NOT NULL THEN
          UPDATE safre_tmp:det_disp_cuotas
         SET safre_tmp:det_disp_cuotas.result_operacion = auk.result_operacion ,
             safre_tmp:det_disp_cuotas.det_mot_rechazo1 = auk.det_mot_rechazo1 ,
             safre_tmp:det_disp_cuotas.det_mot_rechazo2 = auk.det_mot_rechazo2 ,
             safre_tmp:det_disp_cuotas.det_mot_rechazo3 = auk.det_mot_rechazo3 
            WHERE CURP = reg_det.CURP   
        END IF
    END IF
}

END FUNCTION

FUNCTION valida_paterno(vpaterno)
#vp----------------------
    DEFINE
        vpaterno             CHAR(40)

    DEFINE
        cont                  ,
        largo                 ,
        hasta                 ,
        cont_diferencias      ,
        resta                 ,
        i                     INTEGER

    LET cont = 0
    FOR i = 1 TO 40  
        IF reg_det.nom_trabajador[i] = "$" THEN
            EXIT FOR
        ELSE  
            LET cont = cont + 1
        END IF
    END FOR 

    LET largo = LENGTH(vpaterno)

    IF cont > largo THEN
        LET resta = cont - largo
        LET hasta = largo
        LET cont_diferencias = cont - largo
    ELSE
        LET resta = largo - cont
        LET hasta = cont
        LET cont_diferencias = largo - cont
    END IF

    IF resta > 2 THEN
        CALL cont_inconsist()
        LET auk.result_operacion = "01"
        IF det_sw_1 = 0 THEN
            LET auk.det_mot_rechazo1 = "029" #A. paterno invalido en afi_canase
            LET det_sw_1     = 1
        ELSE
            IF det_sw_2 = 0 THEN
                LET auk.det_mot_rechazo2 = "029"
                LET det_sw_2 = 1
            ELSE
                LET auk.det_mot_rechazo3 = "029"
                LET det_sw_3 = 1
                RETURN
            END IF
        END IF 
    ELSE 
        FOR i = 1 TO hasta
            IF reg_det.nom_trabajador[i] <> vpaterno[i] THEN
                LET cont_diferencias = cont_diferencias + 1
            END IF
        END FOR
        IF cont_diferencias > 2 THEN
            CALL cont_inconsist()
            LET auk.result_operacion = "01"
            IF det_sw_1 = 0 THEN
                LET auk.det_mot_rechazo1 = "029" #A. paterno invalido en afi_canase
                LET det_sw_1     = 1
            ELSE
                IF det_sw_2 = 0 THEN
                    LET auk.det_mot_rechazo2 = "029"
                    LET det_sw_2 = 1
                ELSE
                    LET auk.det_mot_rechazo3 = "029"
                    LET det_sw_3 = 1
                    RETURN
                END IF
            END IF
        END IF 

    END IF
END FUNCTION

REPORT det_listado(reg_det)
#dl------------------------
    DEFINE reg_det RECORD
        folio                        INTEGER  ,
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        consec_reg_lote              CHAR(08) ,
        NSS                          CHAR(11) ,
        RFC                          CHAR(13) ,
        CURP                         CHAR(18) ,
        nom_trabajador               CHAR(50) ,
        periodo_pago                 CHAR(06) ,
        fech_pago                    CHAR(08) ,
        fech_valor_rcv               CHAR(08) ,
        fech_valor_viv               CHAR(08) ,
        ult_salario_diario           CHAR(07) ,
        folio_pago_SUA               CHAR(06) ,
        reg_patronal_IMSS            CHAR(11) ,
        rfc_patron                   CHAR(13) ,
        cve_ent_receptora            CHAR(03) ,
        dias_cotz_bimest             CHAR(02) ,
        dias_incap_bimest            CHAR(02) ,
        dias_ausent_bimest           CHAR(02) ,
        impt_ret                     CHAR(07) ,
        impt_act_rec_ret             CHAR(07) ,
        impt_ces_vej                 CHAR(07) ,
        impt_act_r_ces_vej           CHAR(07) ,
        impt_aport_vol               CHAR(07) ,
        impt_aport_pat               CHAR(07) ,
        impt_cuota_soc               CHAR(07) ,
        impt_aport_est               CHAR(07) ,
        impt_aport_esp               CHAR(07) ,
        impt_act_cuo_soc             CHAR(07) ,
        impt_act_aport_est           CHAR(07) ,
        impt_act_cuo_esp             CHAR(07) ,
        fecha_pago_gub               CHAR(08) ,
        filler0                      CHAR(10) ,
####        ind_rcv_salida               CHAR(09) ,
####        ind_viv_salida               CHAR(09) ,
        result_operacion             CHAR(02) ,
        det_mot_rechazo1             CHAR(03) ,
        det_mot_rechazo2             CHAR(03) ,
        det_mot_rechazo3             CHAR(03) ,
        filler                       CHAR(02) 
    END RECORD

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
        IF reg_det.filler IS NULL THEN
           LET reg_det.filler = "00"
        END IF
        PRINT
            COLUMN 1,reg_det.tipo_registro               ,
                     reg_det.ident_servicio              ,
                     reg_det.consec_reg_lote             ,
                     reg_det.NSS                         ,
                     reg_det.RFC                         ,
                     reg_det.CURP                        ,
                     reg_det.nom_trabajador              ,
                     reg_det.periodo_pago                ,
                     reg_det.fech_pago                   ,
                     reg_det.fech_valor_rcv              ,
                     reg_det.fech_valor_viv              ,
                     reg_det.ult_salario_diario          ,
                     reg_det.folio_pago_SUA              ,
                     reg_det.reg_patronal_IMSS           ,
                     reg_det.rfc_patron                  ,
                     #"001"                               , #cve_ent_receptora
                     reg_det.cve_ent_receptora           ,  #cve_ent_receptora
                     reg_det.dias_cotz_bimest            ,
                     reg_det.dias_incap_bimest           ,
                     reg_det.dias_ausent_bimest        ,
                     reg_det.impt_ret                    ,
                     reg_det.impt_act_rec_ret            ,
                     reg_det.impt_ces_vej                ,
                     reg_det.impt_act_r_ces_vej          ,
                     reg_det.impt_aport_vol              ,
                     reg_det.impt_aport_pat              ,
                     reg_det.impt_cuota_soc              ,
                     reg_det.impt_aport_est              ,
                     reg_det.impt_aport_esp              ,
                     reg_det.impt_act_cuo_soc            ,
                     reg_det.impt_act_aport_est          ,
                     reg_det.impt_act_cuo_esp            ,
                     reg_det.fecha_pago_gub,
                     reg_det.filler0,
                 ####    reg_det.ind_rcv_salida              ,
                 ####    reg_det.ind_viv_salida              ,
                     reg_det.result_operacion            ,
                     reg_det.det_mot_rechazo1            ,
                     reg_det.det_mot_rechazo2            ,
                     reg_det.det_mot_rechazo3            ,
                     reg_det.filler                       
END REPORT
REPORT cza_listado(reg_cza)
#cl-----------------------
    DEFINE reg_cza RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_operacion              CHAR(02) ,
        tipo_ent_origen              CHAR(02) ,
        clave_ent_origen             CHAR(03) ,
        tipo_ent_destino             CHAR(02) ,
        clave_ent_destino            CHAR(03) ,
        fech_creac_lote              CHAR(08) ,
        lote_del_dia                 CHAR(03) ,
        mod_recp_envio               CHAR(02) ,
        fech_limite_resp             CHAR(08) ,
        result_operacion             CHAR(02) ,
        motivo_rechazo1              CHAR(03) ,
        motivo_rechazo2              CHAR(03) ,
        motivo_rechazo3              CHAR(03) ,
        filler                       CHAR(247)      
    END RECORD

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
       # LET c_fech_creac_lote        = HOY
        ##LET reg_cza.fech_creac_lote  = c_fech_creac_lote[7,10] ,
        #                               c_fech_creac_lote[1,2]  ,
        #                               c_fech_creac_lote[4,5]

        SELECT MAX(lotes_correlativo)
        INTO   s_lotes_correlativo
        FROM   tab_lote
        WHERE  lotes_cod = 1

        IF s_lotes_correlativo IS NULL THEN
           LET s_lotes_correlativo = 0
        END IF 

        IF STATUS = NOTFOUND THEN
            LET s_lotes_correlativo = 1
        ELSE
            LET s_lotes_correlativo = s_lotes_correlativo + 1
        END IF

        #SELECT MAX(lotes_num) 
        #INTO   s_lote_del_dia
        #FROM   tab_lote
        #WHERE  lotes_cod   = 1
        #AND    lotes_fecha = HOY

        #IF s_lote_del_dia IS NULL THEN
            #LET reg_cza.lote_del_dia = "001"
            #LET s_lote_del_dia       = 1
        #ELSE
            #LET reg_cza.lote_del_dia = s_lote_del_dia + 1 USING "&&&"
            #LET s_lote_del_dia       = s_lote_del_dia + 1
        #END IF

        PRINT COLUMN 1,reg_cza.tipo_registro     
                      ,reg_cza.ident_servicio     
                      ,reg_cza.ident_operacion    
                      ,reg_cza.tipo_ent_origen   
                      ,reg_cza.clave_ent_origen   
                      ,reg_cza.tipo_ent_destino    
                      ,reg_cza.clave_ent_destino   
                      ,reg_cza.fech_creac_lote 
                      ,reg_cza.lote_del_dia
                      ,reg_cza.mod_recp_envio
                      ,reg_cza.fech_limite_resp
                      ,reg_cza.result_operacion   
                      ,reg_cza.motivo_rechazo1    
                      ,reg_cza.motivo_rechazo2    
                      ,reg_cza.motivo_rechazo3    
                      ,reg_cza.filler

END REPORT

REPORT sum_listado(reg_sum)
#sl-----------------------
    DEFINE reg_sum RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_operacion              CHAR(02) ,
        tipo_ent_origen              CHAR(02) ,
        clave_ent_origen             CHAR(03) ,
        tipo_ent_destino             CHAR(02) ,
        clave_ent_destino            CHAR(03) ,
        fech_creac_lote              CHAR(08) ,
        lote_del_dia                 CHAR(03) ,
        impt_total_rcv               CHAR(15) ,
        impt_total_inter_rcv         CHAR(15) ,
        impt_total_patr              CHAR(15) ,
        impt_tatal_int_pat           CHAR(15) ,
        impt_total_guber             CHAR(15) ,
        impt_total_inter_guber       CHAR(15) ,
        num_de_reg_aport             CHAR(08) ,
        num_cuenta_xpagar            CHAR(06) ,
        impt_total_xpagar            CHAR(15) ,
        filler                       CHAR(149)   
    END RECORD

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
        #LET c_fech_creac_lote        = HOY
        #LET reg_sum.fech_creac_lote  = c_fech_creac_lote[7,10] ,
                                       #c_fech_creac_lote[1,2]  ,
                                       #c_fech_creac_lote[4,5]
        #SELECT MAX(lotes_correlativo)
        #INTO   s_lotes_correlativo
        #FROM   tab_lote
        #WHERE  lotes_cod = 1
 
        #IF STATUS = NOTFOUND THEN
            #LET s_lotes_correlativo = 1
        #ELSE
            #LET s_lotes_correlativo = s_lotes_correlativo + 1
        #END IF

        #SELECT MAX(lotes_num) 
        #INTO   s_lote_del_dia
        #FROM   tab_lote
        #WHERE  lotes_cod   = 1
        #AND    lotes_fecha = HOY

        #IF s_lote_del_dia IS NULL THEN
            #LET reg_sum.lote_del_dia = "001"
            #LET s_lote_del_dia       = 1
        #ELSE
            #LET reg_sum.lote_del_dia = s_lote_del_dia + 1 USING "&&&"
            #LET s_lote_del_dia       = s_lote_del_dia + 1
        #END IF

        PRINT
            COLUMN 1,reg_sum.tipo_registro          ,
                     reg_sum.ident_servicio         ,
                     reg_sum.ident_operacion        ,
                     reg_sum.tipo_ent_origen        ,
                     w_codigo_afore             USING"&&&" ,# clave_ent_origen
                     reg_sum.tipo_ent_destino       ,
                     reg_sum.clave_ent_destino      ,
                     reg_sum.fech_creac_lote        ,
                     reg_sum.lote_del_dia           ,
                     reg_sum.impt_total_rcv         ,
                     reg_sum.impt_total_inter_rcv   ,
                     reg_sum.impt_total_patr        ,
                     reg_sum.impt_tatal_int_pat  ,
                     reg_sum.impt_total_guber       ,
                     reg_sum.impt_total_inter_guber ,
                     reg_sum.num_de_reg_aport       ,
                     reg_sum.num_cuenta_xpagar      ,
                     reg_sum.impt_total_xpagar      ,
                     reg_sum.filler

        INSERT INTO tab_lote VALUES(HOY                      ,
                                    1                        ,
                                    "DISPERSION DE CUENTAS"  ,
                                    s_lotes_correlativo      ,
                                    s_lote_del_dia
                                   )
END REPORT

{
REPORT pagos_listados(reg_pagos)
#---------------------
    DEFINE reg_pagos RECORD
        tipo_registro                CHAR(02)  ,
        ident_servicio               CHAR(02)  ,
        ident_pago                   CHAR(16)  ,
        importe                      CHAR(15)  ,
        fech_liquidacion             CHAR(08)  ,
        impt_aport_acept             CHAR(15)  ,
        impt_aport_dev               CHAR(15)  ,
        filler                       CHAR(222)
    END RECORD

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_pagos.tipo_registro    ,
                      reg_pagos.ident_servicio   ,
                      reg_pagos.ident_pago       , 
                      reg_pagos.importe          , 
                      reg_pagos.fech_liquidacion ,
                      reg_pagos.impt_aport_acept ,
                      reg_pagos.impt_aport_dev   , 
                      reg_pagos.filler           
END REPORT
}

{
REPORT hdep2_listado(reg_hdep)
#hl--------------------------
    DEFINE reg_hdep RECORD
        tipo_registro                CHAR(02)      ,
        ident_servicio               CHAR(02)      ,
        ident_pago                   CHAR(16)      ,
        importe                      CHAR(15)      ,
        fech_liquidacion             CHAR(08)      ,
        impt_aport_acept             CHAR(15)      ,
        impt_aport_dev               CHAR(15)      ,
        estado                       CHAR(01)      ,
        filler                       CHAR(222)
    END RECORD

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_hdep.tipo_registro    ,
                      reg_hdep.ident_servicio   ,
                      reg_hdep.ident_pago       ,
                      reg_hdep.importe          ,
                      reg_hdep.fech_liquidacion ,
                      reg_hdep.impt_aport_acept ,
                      reg_hdep.impt_aport_dev   ,
                      reg_hdep.filler        
           
            UPDATE dis_dep_aporte
            SET    estado = 2
            WHERE  tipo_registro    = deg_hdep.tipo_registro
            AND    ident_servicio   = deg_hdep.ident_servicio
            AND    ident_pago       = deg_hdep.ident_pago
            AND    fech_liquidacion = reg_hdep.fech_liquidacion 
            AND    impt_aport_acept = reg_hdep.impt_aport_acept
            AND    estado           = reg_hdep.estado           

END REPORT
}
FUNCTION reporte()
         DEFINE  G_LISTA           CHAR(500)
        DEFINE g_param    RECORD  LIKE dis_parametro.*

	DEFINE w_aux   RECORD  
            nss                   LIKE dis_incons_aporte.nss,
            rfc                   LIKE dis_incons_aporte.rfc,
            curp                  LIKE dis_incons_aporte.curp,
            nom_trabajador        LIKE dis_incons_aporte.nom_trabajador,
            fech_pago             LIKE dis_incons_aporte.fech_pago,
            ult_salario_diario    LIKE dis_incons_aporte.ult_salario_diario,
            folio_pago_sua        LIKE dis_incons_aporte.folio_pago_sua,
            cve_ent_receptora     LIKE dis_incons_aporte.cve_ent_receptora,
            det_mot_rechazo1      LIKE dis_incons_aporte.det_mot_rechazo1,
            det_mot_rechazo2      LIKE dis_incons_aporte.det_mot_rechazo2,
            det_mot_rechazo3      LIKE dis_incons_aporte.det_mot_rechazo3
         END RECORD

           SELECT *
           INTO g_param.*
           FROM dis_parametro    

	    LET G_LISTA = g_param.ruta_spool CLIPPED,"/REPORTE_DE_INCONSISTENCIAS_",HOY USING "dd-mm-yy"
            START REPORT listado TO G_LISTA
               DECLARE cur_re1 CURSOR for 
                SELECT nss,rfc,curp,nom_trabajador,
                       fech_pago,ult_salario_diario,
                       folio_pago_sua,cve_ent_receptora,det_mot_rechazo1,
                       det_mot_rechazo2,det_mot_rechazo3
                from dis_incons_aporte
                #WHERE fech_pago = reg_det.fech_pago

                FOREACH cur_re1 INTO w_aux.*

                    OUTPUT TO REPORT listado(w_aux.*)       
                END FOREACH 
            FINISH REPORT listado
                ERROR"LISTADO GENERADO" SLEEP 2
	    LET G_LISTA = "chmod 777 ",g_param.ruta_spool CLIPPED,"/REPORTE_DE_INCONSISTENCIAS_",HOY USING "dd-mm-yy"
            RUN G_LISTA
END FUNCTION
REPORT listado(w_aux)
#--------------------
    DEFINE w_aux  RECORD
            nss                   LIKE dis_incons_aporte.nss,
            rfc                   LIKE dis_incons_aporte.rfc,
            curp                  LIKE dis_incons_aporte.curp,
            nom_trabajador        LIKE dis_incons_aporte.nom_trabajador,
            fech_pago             LIKE dis_incons_aporte.fech_pago,
            ult_salario_diario    LIKE dis_incons_aporte.ult_salario_diario,
            folio_pago_sua        LIKE dis_incons_aporte.folio_pago_sua,
            cve_ent_receptora     LIKE dis_incons_aporte.cve_ent_receptora,
            det_mot_rechazo1      LIKE dis_incons_aporte.det_mot_rechazo1,
            det_mot_rechazo2      LIKE dis_incons_aporte.det_mot_rechazo2,
            det_mot_rechazo3      LIKE dis_incons_aporte.det_mot_rechazo3
         END RECORD
	DEFINE 
            razon_social                CHAR(40),
            c_fech_pago                 CHAR(10)
	DEFINE 
            fecha_pago1                  DATE 
    OUTPUT
        PAGE LENGTH 90
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

	PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================",
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"========="
        PRINT
            COLUMN 001,razon_social                                         ,
            COLUMN 140,"FECHA   :",hoy USING "DD/MM/YYYY "                     
        PRINT
            COLUMN 001,"DISC002"                                            ,
            COLUMN 035,"                        L I S T A D O S   D E  I N C O N S I S T E N C I A S   ", 
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"           ,
            COLUMN 040,"----------------------------------------"           ,
            COLUMN 080,"----------------------------------------"           ,
            COLUMN 120,"----------------------------------------"           ,
            COLUMN 160,"---------"
        PRINT
            COLUMN 01,"No. Seguro"                         ,
            COLUMN 13,"R.F.C."                          ,
            COLUMN 26,"C.U.R.P."                          ,
            COLUMN 49,"Trabajador"                          ,
            COLUMN 75,"F. Pago"                          ,
            COLUMN 90,"U. Salario"                       ,
            COLUMN 100,"Folio Sua"                       ,
            COLUMN 113,"Ent rec."                 ,
            COLUMN 123,"Mot.Inc1"                 ,
            COLUMN 133,"Mot.Inc2"                 ,
            COLUMN 143,"Mot.Inc3"                 
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"========="
    ON EVERY ROW
        LET c_fech_pago = w_aux.fech_pago [5,6],"/",
                              w_aux.fech_pago [7,8],"/",
                              w_aux.fech_pago [1,4]
        LET fecha_pago1 = c_fech_pago
         
        PRINT
            COLUMN 01,w_aux.nss USING "&&&&&&&&&&&"             ,
            COLUMN 13,w_aux.rfc 				,
            COLUMN 29,w_aux.curp USING "&&&&&&&&&&&&&&&&&&"     ,
            COLUMN 49,w_aux.nom_trabajador  [1,24]              ,
            COLUMN 75,fecha_pago1  USING  "DD/MM/YYYY"  , 
            COLUMN 90,w_aux.ult_salario_diario  USING "&&&&&.&&"  ,
            COLUMN 100,w_aux.folio_pago_sua  USING "&&&&&&&&"    ,
            COLUMN 113,w_aux.cve_ent_receptora USING "&&&&&&"     ,
            COLUMN 123,w_aux.det_mot_rechazo1  USING "&&&&"       ,
            COLUMN 133,w_aux.det_mot_rechazo2  USING "&&&&"       ,     
            COLUMN 143,w_aux.det_mot_rechazo3  USING "&&&&"                                            
        ON LAST ROW
        PRINT
        PRINT
            COLUMN 001,"========================================"           ,
            COLUMN 040,"========================================"           ,
            COLUMN 080,"========================================"           ,
            COLUMN 120,"========================================"           ,
            COLUMN 160,"========="
         PRINT 
            COLUMN 50,"TOTAL DE INCONSISTENCIAS : " ,COUNT(*) USING "&&&"

END REPORT

FUNCTION crea_tablas()
DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_pla_disp
        DROP TABLE cza_disp_cuotas
        DROP TABLE det_disp_cuotas
        DROP TABLE sum_disp_cuotas
        DROP TABLE detalle
        DROP TABLE tmp_pla_disp
        DROP TABLE cza_disp_int
        DROP TABLE det_disp_int
        DROP TABLE sum_disp_int
        DROP TABLE pagos
        DROP TABLE rechzo
    WHENEVER ERROR STOP

    CREATE TABLE tmp_pla_disp
        (
         n_registros                  CHAR(295)
        )

    CREATE TABLE detalle
        (
         folio                        INTEGER  ,
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         consec_reg_lote              CHAR(08) ,
         NSS                          CHAR(11) ,
         RFC                          CHAR(13) ,
         CURP                         CHAR(18) ,
         nom_trabajador               CHAR(50) ,
         periodo_pago                 CHAR(06) ,
         fech_pago                    CHAR(08) ,
         fech_valor_rcv               CHAR(08) ,
         fech_valor_viv               CHAR(08) ,
         ult_salario_diario           CHAR(07) ,
         folio_pago_SUA               CHAR(06) ,
         reg_patronal_IMSS            CHAR(11) ,
         rfc_patron                   CHAR(13) ,
         cve_ent_receptora            CHAR(03) ,
         dias_cotz_bimest             CHAR(02) ,
         dias_incap_bimest            CHAR(02) ,
         dias_ausent_bimest           CHAR(02) ,
         impt_ret                     CHAR(09) ,
         impt_act_rec_ret             CHAR(09) ,
         impt_ces_vej                 CHAR(09) ,
         impt_act_r_ces_vej           CHAR(09) ,
         impt_aport_vol               CHAR(09) ,
         impt_aport_pat               CHAR(09) ,
         impt_cuota_soc               CHAR(09) ,
         impt_aport_est               CHAR(09) ,
         impt_aport_esp               CHAR(09) ,
         impt_act_cuo_soc             CHAR(09) ,
         impt_act_aport_est           CHAR(09) ,
         impt_act_cuo_esp             CHAR(09) ,
         fecha_pago_gub               CHAR(08) ,
         filler0                      CHAR(10) ,
####         ind_rcv_salida               CHAR(10) ,
####         ind_vivieda_salida           CHAR(09) ,
         result_operacion             CHAR(02) ,
         det_mot_rechazo1             CHAR(03) ,
         det_mot_rechazo2             CHAR(03) ,
         det_mot_rechazo3             CHAR(03) ,
         filler                       CHAR(02)
        );
    CREATE INDEX detalle_1 on detalle (tipo_registro);
    CREATE TABLE pagos
       (
        folio                        INTEGER,
        tipo_registro                CHAR(02),
        ident_servicio               CHAR(02),
        ident_pago                   CHAR(16),
        importe                      CHAR(15),
        fech_liquidacion             CHAR(08),
        impt_aport_acept             DECIMAL(15,2),
        impt_aport_dev               DECIMAL(15,2),
        fecha_archivo                DATE,
        fecha_envio                  DATE,
        estado                       INTEGER
       );

    CREATE TABLE cza_disp_cuotas
        (
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         ident_operacion              CHAR(02) ,
         tipo_ent_origen              CHAR(02) ,
         clave_ent_origen             CHAR(03) ,
         tipo_ent_destino             CHAR(02) ,
         clave_ent_destino            CHAR(03) ,
         fech_creac_lote              CHAR(08) ,
         lote_del_dia                 CHAR(03) ,
         mod_recp_envio               CHAR(02) ,
         fech_limite_resp             CHAR(08) ,
         result_operacion             CHAR(02) ,
         motivo_rechazo1              CHAR(03) ,
         motivo_rechazo2              CHAR(03) ,
         motivo_rechazo3              CHAR(03) ,
         filler                       CHAR(247)      
        );
        
    CREATE TABLE cza_disp_int
        (
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         ident_operacion              CHAR(02) ,
         tipo_ent_origen              CHAR(02) ,
         clave_ent_origen             CHAR(03) ,
         tipo_ent_destino             CHAR(02) ,
         clave_ent_destino            CHAR(03) ,
         fech_creac_lote              CHAR(08) ,
         lote_del_dia                 CHAR(03) ,
         mod_recp_envio               CHAR(02) ,
         fech_limite_resp             CHAR(08) ,
         result_operacion             CHAR(02) ,
         motivo_rechazo1              CHAR(03) ,
         motivo_rechazo2              CHAR(03) ,
         motivo_rechazo3              CHAR(03) ,
         filler                       CHAR(247)      
        );

    CREATE TABLE det_disp_cuotas
        (
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         consec_reg_lote              CHAR(08) ,
         NSS                          CHAR(11) ,
         RFC                          CHAR(13) ,
         CURP                         CHAR(18) ,
         nom_trabajador               CHAR(50) ,
         periodo_pago                 CHAR(06) ,
         fech_pago                    CHAR(08) ,
         fech_valor_rcv               CHAR(08) ,
         fech_valor_viv               CHAR(08) ,
         ult_salario_diario           CHAR(07) ,
         folio_pago_SUA               CHAR(06) ,
         reg_patronal_IMSS            CHAR(11) ,
         rfc_patron                   CHAR(13) ,
         cve_ent_receptora            CHAR(03) ,
         dias_cotz_bimest             CHAR(02) ,
         dias_incap_bimest            CHAR(02) ,
         dias_ausent_bimest           CHAR(02) ,
         impt_ret                     DECIMAL(15,2) ,
         impt_act_rec_ret             DECIMAL(15,2) ,
         impt_ces_vej                 DECIMAL(15,2) ,
         impt_act_r_ces_vej           DECIMAL(15,2) ,
         impt_aport_vol               DECIMAL(15,2) ,
         impt_aport_pat               DECIMAL(15,2) ,
         impt_cuota_soc               DECIMAL(15,2) ,
         impt_aport_est               DECIMAL(15,2) ,
         impt_aport_esp               DECIMAL(15,2) ,
         impt_act_cuo_soc             DECIMAL(15,2) ,
         impt_act_aport_est           DECIMAL(15,2) ,
         impt_act_cuo_esp             DECIMAL(15,2) ,
         fecha_pago_gub               CHAR(08) ,
         filler0                      CHAR(10) ,
####         ind_rcv_salida               DECIMAL(9,7) ,
####         ind_vivieda_salida           DECIMAL(9,7) ,
         result_operacion             CHAR(02) ,
         det_mot_rechazo1             CHAR(03) ,
         det_mot_rechazo2             CHAR(03) ,
         det_mot_rechazo3             CHAR(03) ,
         filler                       CHAR(02)
        );
	CREATE INDEX det_disp_cuot1 on det_disp_cuotas (result_operacion);
	CREATE INDEX det_disp_cuot2 on det_disp_cuotas (nss);

    CREATE TABLE rechzo
        (
        folio                        INTEGER  ,
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        consec_reg_lote              CHAR(08) ,
        NSS                          CHAR(11) ,
        RFC                          CHAR(13) ,
        CURP                         CHAR(18) ,
        nom_trabajador               CHAR(50) ,
        periodo_pago                 CHAR(06) ,
        fech_pago                    CHAR(08) ,
        fech_valor_rcv               CHAR(08) ,
        fech_valor_viv               CHAR(08) ,
        ult_salario_diario           CHAR(07) ,
        folio_pago_SUA               CHAR(06) ,
        reg_patronal_IMSS            CHAR(11) ,
        rfc_patron                   CHAR(13) ,
        cve_ent_receptora            CHAR(03) ,
        dias_cotz_bimest             CHAR(02) ,
        dias_incap_bimest            CHAR(02) ,
        dias_ausent_bimest           CHAR(02) ,
        impt_ret                     CHAR(09) ,
        impt_act_rec_ret             CHAR(09) ,
        impt_ces_vej                 CHAR(09) ,
        impt_act_r_ces_vej           CHAR(09) ,
        impt_aport_vol               CHAR(09) ,
        impt_aport_pat               CHAR(09) ,
        impt_cuota_soc               CHAR(09) ,
        impt_aport_est               CHAR(09) ,
        impt_aport_esp               CHAR(09) ,
        impt_act_cuo_soc             CHAR(09) ,
        impt_act_aport_est           CHAR(09) ,
        impt_act_cuo_esp             CHAR(09) ,
        fecha_pago_gub               CHAR(08) ,
        filler0                      CHAR(10) ,
####        ind_rcv_salida               CHAR(09) ,
####        ind_viv_salida               CHAR(09) ,
        result_operacion             CHAR(02) ,
        det_mot_rechazo1             CHAR(03) ,
        det_mot_rechazo2             CHAR(03) ,
        det_mot_rechazo3             CHAR(03) ,
        filler                       CHAR(02)
        );

    CREATE TABLE sum_disp_cuotas
        (
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         ident_operacion              CHAR(02) ,
         tipo_ent_origen              CHAR(02) ,
         clave_ent_origen             CHAR(03) ,
         tipo_ent_destino             CHAR(02) ,
         clave_ent_destino            CHAR(03) ,
         fech_creac_lote              CHAR(08) ,
         lote_del_dia                 CHAR(03) ,
         impt_total_rcv               CHAR(15) ,
         impt_total_int_rcv           CHAR(15) ,
         impt_total_patr              CHAR(15) ,
         impt_tatal_int_pat           CHAR(15) ,
         impt_total_guber             CHAR(15) ,
         impt_total_int_gub           CHAR(15) ,
         num_de_reg_aport             CHAR(08) ,
         num_cuenta_xpagar            CHAR(06) ,
         impt_total_xpagar            CHAR(15) ,
         filler                       CHAR(149)   
        );

    CREATE TABLE det_disp_int
        (
         tipo_registro                CHAR(02) ,
         ident_servicio               CHAR(02) ,
         consec_reg_lote              CHAR(08) ,
         NSS                          CHAR(11) ,
         RFC                          CHAR(13) ,
         CURP                         CHAR(18) ,
         nom_trabajador               CHAR(50) ,
         periodo_pago                 CHAR(06) ,
         fech_pago                    CHAR(08) ,
         fech_valor_rcv               CHAR(08) ,
         fech_valor_viv               CHAR(08) ,
         ult_salario_diario           CHAR(07) ,
         folio_pago_SUA               CHAR(06) ,
         reg_patronal_IMSS            CHAR(11) ,
         rfc_patron                   CHAR(13) ,
         cve_ent_receptora            CHAR(03) ,
         dias_cotz_bimest             CHAR(02) ,
         dias_incap_bimest            CHAR(02) ,
         dias_ausent_bimest           CHAR(02) ,
         impt_int_ret                 DECIMAL(15,2) ,
         impt_int_act_rr              DECIMAL(15,2) ,
         impt_int_ces_vej             DECIMAL(15,2) ,
         impt_int_act_rcv             DECIMAL(15,2) ,
         impt_int_aport_vol           DECIMAL(15,2) ,
         impt_int_aport_pat           DECIMAL(15,2) ,
         impt_int_cuota_soc           DECIMAL(15,2) ,
         impt_int_aport_est           DECIMAL(15,2) ,
         impt_int_aport_esp           DECIMAL(15,2) ,
         impt_int_act_cuo_s           DECIMAL(15,2) ,
         impt_int_act_est             DECIMAL(15,2) ,
         impt_int_a_cuo_esp           DECIMAL(15,2) ,
         fecha_pago_gub               CHAR(08) ,
         filler0                      CHAR(10) ,
####         ind_rcv_salida               DECIMAL(9,7) ,
####         ind_vivieda_salida           DECIMAL(9,7) ,
         result_operacion             CHAR(02) ,
         det_mot_rechazo1             CHAR(03) ,
         det_mot_rechazo2             CHAR(03) ,
         det_mot_rechazo3             CHAR(03) ,
         filler                       CHAR(02)
        );

CREATE INDEX det_disp_int_1 ON det_disp_int(nss)
CREATE INDEX det_disp_int_2 ON det_disp_int(result_operacion)

DATABASE safre_af

END FUNCTION

FUNCTION prepara_especial()

   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
      DROP TABLE maeafi_esp
      DROP TABLE nss_esp
   WHENEVER ERROR STOP

   CREATE TABLE maeafi_esp
  (
    n_seguro char(11),
    n_unico char(18),
    n_rfc char(13),
    paterno char(40),
    materno char(40),
    nombres char(40),
    fena date,
    n_folio decimal(8,0),
    edo_civil smallint,
    localn smallint,
    estadon smallint,
    tiptr smallint,
    cod_promotor char(10),
    sexo smallint,
    n_operac serial,
    frecafor date,
    fentcons date,
    femision date,
    finitmte date,
    finicta date,
    status smallint,
    agenc_cod char(10),
    status_interno smallint,
    nacionalidad char(3),
    tip_prob char(1),
    fol_prob char(10),
    doc_prob char(16),
    ind_infonavit char(1),
    documento_1 char(1),
    documento_2 char(1),
    documento_3 char(1),
    documento_4 char(1),
    documento_5 char(1),
    documento_6 char(1),
    envio_dom smallint,
    entidad_curp integer,
    asigna_curp char(1),
    const_curp smallint,
    usuario char(8),
    hora char(8),
    status_captura smallint,
    tipo_solicitud smallint,
    fecha_elaboracion date,
    lote smallint,
    fecha_envio date,
    cod_esq_comision smallint,
    ubicacion integer,
    indicador_b char(15),
    indicador_c char(15),
    indicador_d char(15),
    indicador_e char(15),
    cod_error_origen smallint,
    folio_edo_cta char(8),
    cod_afore_ced smallint,
    salario_base_comis decimal(12,2),
    salario_actual decimal(12,2),
    fecha_actualiza_sa date,
    coduni_n1 char(10),
    indicador_comision smallint,
    codven char(10),
    ident_envio smallint,
    coor_captura smallint,
    lote_captura smallint,
    folio_captura smallint,
    sello_electronico char(24)
  );

   create index maeafi_esp_1 on maeafi_esp (n_seguro);

   create table nss_esp
    (nss CHAR(11))
   
   WHENEVER ERROR STOP

   LET comando = g_param.ruta_rescate CLIPPED,"/validados.txt"
--   LET comando = "/imss/DISPER/ESPRECA/VALIDADOS.txt"

   LOAD FROM comando INSERT INTO nss_esp
                      
   IF STATUS = NOTFOUND THEN
      DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
      EXIT PROGRAM
   END IF
   WHENEVER ERROR STOP

   INSERT INTO maeafi_esp
   SELECT b.*
     FROM safre_af:afi_mae_afiliado b
    WHERE b.n_seguro in (SELECT UNIQUE(a.nss)
                           FROM nss_esp a
                          WHERE b.n_seguro = a.nss)

   DATABASE safre_af
END FUNCTION
