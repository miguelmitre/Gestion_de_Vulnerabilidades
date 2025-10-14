############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TCAAB006L=> GENERA ARCHIVO TRASPASO COMPLEMENTARIO (CEDENTE)     #
#Sistema           => TCAA                                                 #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                       #
#Elaborado         => 24 Septiembre 2004                                   #
#version           => 27052010 Agrupa subcuentas  33 y 34                  #
############################################################################
DATABASE     safre_af
GLOBALS
   DEFINE 
             g_por_curp                        ,
             g_tipo_mov                        ,
             g_tipo_solicitud                  ,
             g_credito_fovissste               ,
             g_tot_bloques_rcv                 ,
             g_si_hay_comple                   SMALLINT
   DEFINE
             g_num_aport_viv92                 ,
             g_dias_cuota_social               ,
             g_num_aport_viv_issste            ,
             g_num_registro                    INTEGER
   DEFINE
             g_acciones                        DEC(18,6) ,
             g_pesos                           DEC(16,2),
             g_imp_2_dec                       DEC(15,2),
             g_monto_minimo                    DEC(15,2)
   DEFINE    
             g_vol_patronal                    ,            # id_08
             g_vol_ventanilla                  ,            # id_09
             g_aport_l_plazo                   ,            # id_10
             g_perspect_l_plazo                ,            # id_11
             g_fecha_redencion                 CHAR(008),   # id_30
             g_cred_en_garantia                CHAR(001),
             g_cod_result_operac               CHAR(002),
             g_diag_proceso                    CHAR(015),
             g_id_aportante                    CHAR(011),
             g_nss_unificador                  CHAR(011),
             g_det                             CHAR(100),
             g_cza                             CHAR(100),
             g_sum                             CHAR(100),
             g_hora                            CHAR(005), 
             g_cat                             CHAR(300),
             g_bloques_rcv                     CHAR(416),
             g_bloques_rcv_reg_1               CHAR(416),
             g_sum_rcv                         CHAR(476),
             g_fecha_nula                      CHAR(008),
             g_n_unico                         CHAR(018),
             g_curp_unificador                 CHAR(018)
   DEFINE
             g_sie          ARRAY[50,50]   OF   RECORD
             acciones                          DEC(16,6),
             pesos                             DEC(16,2),
             pesos2d                           DEC(16,2)
                                          END  RECORD
   DEFINE
             g_saldos       ARRAY[50]     OF   RECORD
             acciones                          DEC(16,6),
             pesos                             DEC(16,2)
                                          END  RECORD
   DEFINE
             g_sumario      ARRAY[50]     OF   RECORD
             nom_scta                          CHAR(30),
             scta_proc                         CHAR(02),
             pesos                             DEC(15,2),
             acciones                          DEC(16,6)
                                          END  RECORD
   DEFINE
             g_sum_rep      ARRAY[50,50]   OF   RECORD
             pesos                             DEC(16,2),
             acciones                          DEC(16,6)
                                          END  RECORD

   DEFINE    reg_solicitud  RECORD  LIKE  safre_af:taa_cd_det_cedido.*
   DEFINE    mr_folio       RECORD  LIKE safre_af:taa_cd_ctr_folio.*
END GLOBALS

GLOBALS  "TCAAB006S.4gl"   #  ARMA LOS QUERYS PARA EL PROCESO

MAIN
   OPTIONS
            PROMPT   LINE LAST,
            ACCEPT   KEY CONTROL-I
            DEFER    INTERRUPT

   CALL     F_010_inicio()
   CALL     F_100_proceso()
   CALL     F_200_fin()

END MAIN
 
FUNCTION    F_010_inicio()
   LET      reg_ctr_folio.folio                      =  ARG_VAL(1)
   LET      reg_ctr_folio.fecha_envio_saldos         =  ARG_VAL(2)
   LET      reg_ctr_folio.fecha_liquidacion          =  ARG_VAL(3)
   LET      g_fecha_liq_parti                        =  ARG_VAL(4)
   LET      g_fecha_saldo_parti                      =  ARG_VAL(5)
   LET      g_today                                  =  ARG_VAL(6)
   CALL     STARTLOG("TCAAB006L.log")
   CALL     arma_querys_TCAAB006S()
   CALL     F_015_prepara_querys_TCAAB006()
   LET      INT_FLAG                   =  TRUE
   CALL     F_920_trae_parametros()
   CALL     F_020_trae_parametros()
   CALL     F_930_arma_precios_acc_parti()
   LET      g_current                      =  CURRENT
   DISPLAY  " "
   DISPLAY  ".1"
   DISPLAY  g_current,": TCAAB006: "
   DISPLAY  g_current,": TRASPASO COMPLEMENTARIO AFORE-AFORE  CEDENTE"
   LET      cero                           =  0
   LET      g_hora                         =  TIME
   LET      g_num_registro                 =  cero
   LET      g_scta_ini                     =  1
   LET      g_siefore_ini                  =  1
   LET      g_nss_unificador               =  " "
   CALL     F_030_muestra_folio_comple()
END FUNCTION

FUNCTION    F_015_prepara_querys_TCAAB006()
   PREPARE  sql_02               FROM  g_sql_02
   PREPARE  sql_03               FROM  g_sql_03
   PREPARE  sql_06               FROM  g_sql_06
   PREPARE  sql_08               FROM  g_sql_08
   PREPARE  sql_09               FROM  g_sql_09
   PREPARE  sql_11               FROM  g_sql_11
   EXECUTE  sql_11
   PREPARE  sql_11_110           FROM  g_sql_11_110
   EXECUTE  sql_11_110
   PREPARE  sql_40               FROM  g_sql_40
   EXECUTE  sql_40
   PREPARE  sql_35               FROM  g_sql_35
   PREPARE  sql_12               FROM  g_sql_12
   PREPARE  sql_13               FROM  g_sql_13
   PREPARE  sql_14               FROM  g_sql_14
   PREPARE  sql_15               FROM  g_sql_15
   PREPARE  sql_16               FROM  g_sql_16
   PREPARE  sql_18               FROM  g_sql_18
   PREPARE  sql_19               FROM  g_sql_19
   PREPARE  sql_20               FROM  g_sql_20
   PREPARE  sql_31               FROM  g_sql_31
   PREPARE  sql_32               FROM  g_sql_32
   PREPARE  sql_32_110           FROM  g_sql_32_110
   PREPARE  sql_41               FROM  g_sql_41
   DECLARE  c_dis_cuentas        CURSOR   FOR  sql_41 
   PREPARE  sql_42               FROM  g_sql_42   # nuevo para saldos
   PREPARE  sql_43               FROM  g_sql_43   # nuevo para provision
   PREPARE  sql_45               FROM  g_sql_45   # monto de credito en garantia
   PREPARE  sql_47               FROM  g_sql_47   # monto de credito en garantia
   PREPARE  sql_48               FROM  g_sql_48   # borra taa_cd_fechas_vol
   PREPARE  sql_50_1             FROM  g_sql_50_1 # extrae tipo traspaso
   PREPARE  sql_55               FROM  g_sql_55   # consec_lote_dia
   PREPARE  sql_56               FROM  g_sql_56
   PREPARE  sql_57               FROM  g_sql_57
   PREPARE  sql_34               FROM  g_sql_34
   PREPARE  sql_36               FROM  g_sql_36
   PREPARE  sql_37               FROM  g_sql_37
   PREPARE  sql_38               FROM  g_sql_38
   PREPARE  sql_39               FROM  g_sql_39
   EXECUTE  sql_03               INTO  g_provisionado
   EXECUTE  sql_08               INTO  g_monto_minimo  
   EXECUTE  sql_13               INTO  g_afore_cod,g_raz_social,g_usuario
   EXECUTE  sql_37               INTO  g_scta_fin
   EXECUTE  sql_38               INTO  g_siefore_fin
END FUNCTION

FUNCTION    F_020_trae_parametros()
   DEFINE   l_folios_pendientes                 SMALLINT
   DEFINE   l_tab_subcuenta      RECORD  LIKE safre_af:tab_subcuenta.*
   LET      g_fecha_nula              =  "00010101"
   DECLARE  c_scta      CURSOR  FOR    sql_35
   FOREACH  c_scta        INTO  l_tab_subcuenta.*
            LET      g_scta                               =
                     l_tab_subcuenta.subct_cod
            LET      g_sumario[g_scta].scta_proc          =
                     l_tab_subcuenta.subct_prc
            LET      g_sumario[g_scta].nom_scta           =
                     l_tab_subcuenta.subct_desc
   END FOREACH
   LET      g_det            =  g_seg_modulo.ruta_envio CLIPPED,"/DET_12"
END FUNCTION

FUNCTION    F_030_muestra_folio_comple()
   DEFINE   l_cad_desc_rech                   CHAR(100),
            l_registros                       INTEGER
   LET      reg_ctr_folio.calculo_interes          =  1
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :TRASPASO  COMPLEMENTARIO          : "
   DISPLAY  g_current," :FOLIO A PROCESAR                  : ",
            reg_ctr_folio.folio      USING "######"
   DISPLAY  g_current," :FECHA DE VALUACION RCV Y VIV      : ",
            g_today,"  ",g_fecha_saldo_parti
   DISPLAY  g_current," :FECHA DE ENVIO DE SALDOS          : ",
            reg_ctr_folio.fecha_envio_saldos
   DISPLAY  g_current," :FECHA DE LIQUIDACION              : ",
            reg_ctr_folio.fecha_liquidacion
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 1        : ",
            g_sie_inf[1].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 2        : ",
            g_sie_inf[2].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 3        : ",
            g_sie_inf[3].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 4        : ",
            g_sie_inf[4].precio_accion    USING  "###.######"
   #DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 5        : ",
   #         g_sie_inf[5].precio_accion    USING  "###.######" # CPL-1225
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 6        : ",
            g_sie_inf[6].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE PARTICIPACION VIVIENDA  : ",
            g_sie_inf[11].precio_accion    USING "###.##############"
   DISPLAY  g_current," :PRECIO DE AIVS VIVIENDA FOVISSSTE : ",
            g_sie_inf[12].precio_accion    USING "###.##############"
   DISPLAY  g_current," :PRECIO DE UDIS BONO PENSION ISSSTE: ",
            g_sie_inf[13].precio_accion    USING "###.##############"
   EXECUTE  sql_06               INTO  l_registros
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :NUMERO DE REGISTROS A PROCESAR    : ",         
                     l_registros   USING  "######"
END FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_regs_proc                    ,
            l_folio                        INTEGER,
            l_leidos                       INTEGER,
            l_sin_marca                    SMALLINT,
            l_nss                          CHAR(11),
            l_comple                       SMALLINT
            
   LET      l_leidos                       =  cero 
   LET      l_regs_proc                    =  cero
   CALL     F_110_inicializa_sumarios()
   START    REPORT R_150_arma_detalles        TO  g_det
########         GRABA  FOLIO       #############
   EXECUTE  sql_15         USING  reg_ctr_folio.folio,"2",g_today,
                                  reg_ctr_folio.fecha_envio_saldos,
                                  reg_ctr_folio.fecha_liquidacion,
                                  reg_ctr_folio.calculo_interes,
                                 "102",
                                  g_usuario
   LET      g_current              =  CURRENT
   DISPLAY  g_current," :PROCESANDO  INFORMACION ...... "
   DECLARE  c_cedidos     CURSOR   FOR  sql_09 
   OPEN     c_cedidos
   FOREACH  c_cedidos       INTO  l_nss
            SELECT   MAX(folio)
              INTO   l_folio
              FROM   safre_af:taa_cd_det_cedido
             WHERE   n_seguro          =  l_nss
               AND   estado            =  103;
               
                       #Selecciona informacion del folio
            INITIALIZE mr_folio.* TO NULL
              SELECT * INTO mr_folio.*
              FROM safre_af:taa_cd_ctr_folio
              WHERE folio = l_folio
              AND estado = 103
             IF STATUS = NOTFOUND THEN  #no existe control de folio
                CONTINUE FOREACH
             END IF
            
            #Se valida si ha tenido traspaso complementario
            LET l_comple = 0
            SELECT COUNT(*) INTO l_comple
            FROM taa_cd_det_comple
            WHERE n_seguro = l_nss
            AND   estado            =  103;

            IF l_comple > 0 THEN
               INITIALIZE mr_folio.* TO NULL
               SELECT * INTO mr_folio.*
               FROM safre_af:taa_cd_ctr_folio
               WHERE folio = (SELECT MAX(folio) FROM taa_cd_det_comple
                              WHERE n_seguro = l_nss
                              AND   estado            =  103)
               AND estado = 103
            END IF

            EXECUTE  sql_14
                     USING     l_nss,
                               l_folio
                      INTO     reg_solicitud.*,
                               g_tipo_solicitud,g_finicta,g_n_unico
            LET      g_por_curp                        =  0
            IF       reg_solicitud.tipo_traspaso       = "71"   OR
                     reg_solicitud.tipo_traspaso       = "72"   OR
                     reg_solicitud.tipo_traspaso       = "73"   OR
                     reg_solicitud.tipo_traspaso       = "74"   OR
                     reg_solicitud.tipo_traspaso       = "83"   OR   
                     reg_solicitud.tipo_traspaso       = "84"   OR   
                     reg_solicitud.tipo_traspaso       = "85"   THEN 
                     LET      g_por_curp               =  1
            END IF
            
            LET      g_si_hay_comple             =  0
            CALL     F_130_arma_saldo_viv()
            CALL     F_133_arma_saldo_rcv()
##########              Arma  Registros  de  Detalle        ###########
            IF       g_si_hay_comple        THEN
                     LET      l_leidos                    =  l_leidos    +   1
                     CALL     F_120_arma_datos_generales()
                     CALL     F_145_insert_tmp_envio_2dec()
                     OUTPUT   TO   REPORT     R_150_arma_detalles() #2
                     EXECUTE  sql_36
                              USING    reg_ctr_folio.folio,
                                       reg_solicitud.n_seguro,
                                       reg_ctr_folio.fecha_envio_saldos,
                                       reg_solicitud.fecha_trasp,
                                       reg_ctr_folio.fecha_liquidacion,
                                       g_provisionado
            END IF 
            IF       l_leidos                 =  100    THEN
                     LET      l_regs_proc     =  l_regs_proc   +  100
                     LET      l_leidos        =  cero
                     LET      g_current       =  CURRENT
                     DISPLAY  g_current," :REGISTROS PROCESADOS     :",
                              l_regs_proc  
            END IF
   END FOREACH
   LET      l_regs_proc             =  l_regs_proc     +  l_leidos
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :TOTAL DE REGISTROS PROCESADOS     : " ,
            l_regs_proc         USING  "######"
   FINISH   REPORT R_150_arma_detalles
   UPDATE   safre_af:taa_cd_det_cedido
      SET   estado                  =  12
    WHERE   estado                  =  g_liquidado
      AND   n_seguro     IN(SELECT   n_seguro 
                              FROM   taa_cd_det_comple
                             WHERE   folio    =  reg_ctr_folio.folio) 
END FUNCTION

FUNCTION    F_110_inicializa_sumarios()
   FOR      g_scta                  =  g_scta_ini     TO  g_scta_fin
            LET      g_sumario[g_scta].pesos          =  cero
            LET      g_sumario[g_scta].acciones       =  cero
            FOR      g_siefore          =  g_siefore_ini   TO  g_siefore_fin
                     LET      g_sum_rep[g_scta,g_siefore].pesos      =  cero
                     LET      g_sum_rep[g_scta,g_siefore].acciones   =  cero
            END FOR
   END FOR
END FUNCTION

FUNCTION    F_120_arma_datos_generales()
   DEFINE   l_f_ini_cr_garantia                 DATE
   DEFINE   l_fecha                             DATE
   #--- TOTAL APORTACIONES VIVIENDA
   CALL     F_121_arma_num_aport_viv92()
   LET      g_num_aport_viv_issste        =  cero
   IF       g_num_aport_viv92            IS  NULL   THEN
            LET      g_num_aport_viv92            =  cero
   END IF
   #------ IDENTIFICADOR DE CREDITO EN GARANTIA ------#
   LET      g_cred_en_garantia            = '0'
   LET      l_f_ini_cr_garantia           =  NULL
   EXECUTE  sql_18      USING    reg_solicitud.n_seguro
                         INTO    l_f_ini_cr_garantia
   IF       l_f_ini_cr_garantia         IS  NOT  NULL   THEN
            LET      g_cred_en_garantia        = '1'
   ELSE
            EXECUTE  sql_19             USING  reg_solicitud.n_seguro
            IF       SQLCA.SQLCODE          =  0  THEN
                              LET      g_cred_en_garantia        =  '2'
            END IF
   END IF
   #--- CHECA  SI HAY CREDITO  EN GARANTIA  FOVISSSTE  -----
   LET      g_credito_fovissste           =  0
   LET      l_f_ini_cr_garantia           =  NULL
   EXECUTE  sql_20      USING    reg_solicitud.n_seguro
                         INTO    l_f_ini_cr_garantia
   IF       l_f_ini_cr_garantia          IS  NOT  NULL   THEN
            LET      g_credito_fovissste       =  1
   END IF
END FUNCTION

FUNCTION    F_121_arma_num_aport_viv92()
   DEFINE   l_num_aport_viv92                 SMALLINT
   LET      g_num_aport_viv92                 =  cero
   LET      l_num_aport_viv92                 =  cero
   FOREACH  c_dis_cuentas            INTO  g_tabname 
            CALL     arma_querys_TCAAB006S()
            PREPARE  sql_22          FROM  g_sql_22
            EXECUTE  sql_22         USING  reg_solicitud.n_seguro 
                                     INTO  l_num_aport_viv92 
            LET      g_num_aport_viv92          =
                     g_num_aport_viv92          +  l_num_aport_viv92
   END FOREACH
END FUNCTION

FUNCTION    F_130_arma_saldo_viv()
   DEFINE   l_grupo                             SMALLINT
   DEFINE   l_acciones                          DECIMAL(16,6),
            l_pesos                             DECIMAL(16,2)
   DEFINE   l_id_prefijo                        CHAR(003)
   LET      l_grupo                   =  4   
   CALL     F_132_1_inicializa_g_saldos()
   EXECUTE  sql_50_1           USING  reg_solicitud.tipo_traspaso
                                INTO  g_tipo_mov , l_id_prefijo
   LET      g_id_aportante            =  l_id_prefijo     CLIPPED ,
	    reg_solicitud.cve_recep_cuenta
   DECLARE  c_saldo       CURSOR     FOR  sql_42
   FOREACH  c_saldo
            USING     reg_solicitud.n_seguro,
                      cero        ,   #  subcuenta
                      l_grupo     ,
                      g_today
             INTO     g_scta      ,
                      g_siefore   ,
                      g_acciones  ,
                      g_pesos
            LET       g_imp_2_dec        =
                     (g_acciones         *  g_sie_inf[g_siefore].precio_accion)
            LET       g_pesos            =  g_imp_2_dec
            IF        g_scta             =  4      THEN
                      IF       g_cred_en_garantia             = '1'  THEN
                               CALL      F_131_resta_credito_en_garantia()
                               LET       g_imp_2_dec       =  g_acciones
			       LET       g_acciones        =  g_imp_2_dec
                      END  IF
            END   IF
            IF        g_scta             =  14     OR
                      g_scta             =  35     THEN
                      IF       g_credito_fovissste            =  1   THEN
                               CALL      F_131_resta_credito_en_garantia()
                      END  IF
            END   IF
            IF       g_pesos             >=  g_monto_minimo      THEN
                     LET      g_imp_2_dec                     =  g_pesos
                     LET      g_saldos[g_scta].pesos          =  g_imp_2_dec
                     LET      g_saldos[g_scta].acciones       =  g_acciones
                     LET      g_sumario[g_scta].pesos         = 
                              g_sumario[g_scta].pesos         +  g_imp_2_dec
                     LET      g_sumario[g_scta].acciones      =  
                              g_sumario[g_scta].acciones      +  g_acciones
                     LET      g_sum_rep[g_scta,1].pesos       = 
                              g_sum_rep[g_scta,1].pesos       +  g_pesos
                     LET      g_sum_rep[g_scta,1].acciones    =  
                              g_sum_rep[g_scta,1].acciones    +  g_acciones
                     CALL     F_140_inserta_dis_provision()
            END   IF
   END FOREACH
END FUNCTION

FUNCTION    F_131_resta_credito_en_garantia()
   DEFINE   l_acciones                          DECIMAL(16,6),
            l_pesos                             DECIMAL(16,2)
   LET      l_acciones                  =  cero
   EXECUTE  sql_45
            USING   reg_solicitud.n_seguro,
                    g_scta
             INTO   l_acciones
   IF       l_acciones                  >  cero   THEN
            LET     g_acciones          =
                    g_acciones          -  l_acciones
            LET     g_imp_2_dec         =
                    g_acciones          *
                    g_sie_inf[g_siefore].precio_accion
            LET     g_pesos             =  g_imp_2_dec
   END IF
END FUNCTION

FUNCTION    F_132_1_inicializa_g_saldos()
   LET      g_siefore                  =  cero
   FOR      g_scta                     =  g_scta_ini   TO  g_scta_fin
            LET      g_saldos[g_scta].acciones      =  cero
            LET      g_saldos[g_scta].pesos         =  cero
   END FOR
   LET      g_scta                     =  cero
   LET      g_acciones                 =  cero
   LET      g_pesos                    =  cero
END FUNCTION


FUNCTION    F_133_arma_saldo_rcv()
   DEFINE   l_scta                         SMALLINT
   DEFINE   l_acc_ret_92                   DECIMAL(16,6),
            l_pesos                        DECIMAL(16,2)
   #CPL-1635
   DEFINE   l_acc_36     ,
            l_p_acc_36   ,
            l_acc_37     ,
            l_p_acc_37   ,
            l_pesos_36   ,
            l_pesos_37    DECIMAL(16,6),
            l_id_scta_31,
            tm_queb,
            l_veces,                   #siempre = 1 solo 2 cuando scta 31
            l_i,
            ind_quebranto   SMALLINT,
            txt_queb        CHAR(200)
            
   LET      g_tot_bloques_rcv              =  1
   CALL     F_133_1_inicializa_rcv()
   LET      g_bloques_rcv                  =  NULL
   LET      g_bloques_rcv_reg_1            =  NULL
###################  QUERYS  DE  F_133_arma_saldo_rcv()  ###############
   DECLARE  c_saldo_rcv  CURSOR  FOR  sql_42
   FOREACH  c_saldo_rcv   USING  reg_solicitud.n_seguro,
                                 cero        ,  #  subcuenta
                                 cero        ,  #  grupo
                                 g_today
                           INTO  g_scta,
                                 g_siefore   ,
                                 g_acciones  ,
                                 g_pesos
            LET l_pesos = 0
            LET l_acc_ret_92 = 0
            IF      (g_scta                  =  4       OR 
                     g_scta                  =  8       OR 
                     g_scta                  =  14      OR
                     g_scta                  =  35      OR 
                     g_scta                  =  36 )    THEN
                     CONTINUE  FOREACH 
            END IF
            
            #Si no tiene saldo en la cuenta
            IF (g_acciones + g_pesos ) <= 0 THEN
                CONTINUE FOREACH
            END IF
            LET      l_scta                  =  0
            EXECUTE  sql_02
                     USING   g_scta
                     INTO    l_scta     ##    checa  subcuenta  agrupada
            IF       l_scta                  >  0    THEN
                     EXECUTE   sql_12
                       USING   reg_solicitud.n_seguro ,
                               g_today                        ,
                               l_scta,
                               g_siefore
                     INTO      l_acc_ret_92
                     LET       l_pesos       =
                               l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            ELSE
                     LET      l_scta                  =  g_scta
                     LET      l_pesos                 =  g_pesos
            END IF
            
            #CPL-1635
            --- Se agrega funcionalidad  de separacion de los saldos de CV
            --- de acuerdo a funcionalidad de traspaso NORMAL(OP09) a peticion de PEIS
            --- desagrupar la scta 31 en 36 y 37 procesar


            LET l_veces = 1
            LET l_id_scta_31 = FALSE
           
            SELECT "OK" 
            FROM   taa_cd_desagrupa_subcta a
            WHERE  a.subcta_cod = l_scta
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN 

              LET l_id_scta_31 = TRUE
              LET l_veces      =  2
              LET l_acc_36     = 0
              LET l_p_acc_36   = 0
              LET l_p_acc_37   = 0
              LET l_pesos_36   = 0
              LET l_acc_37     = 0
              LET l_pesos_37   = 0
              LET ind_quebranto = FALSE
              
              FOREACH  c_dis_cuentas             INTO   g_tabname

                 CALL     arma_querys_TCAAB006S()
                 LET txt_queb = " SELECT  max(tipo_movimiento) " ,
                                " FROM  ",g_tabname CLIPPED ,
                                " WHERE nss = ? " ,
                                " AND   tipo_movimiento between 800 and 899 "

                 PREPARE sql_queb FROM txt_queb

                 PREPARE  sql_66    FROM   g_sql_66 -- hist de dis_cuenta
                 EXECUTE  sql_66    USING   reg_solicitud.n_seguro ,
                                            mr_folio.fecha_envio_saldos, 
                                            l_scta  ,
                                            g_siefore,
                                            mr_folio.folio
                                    INTO    l_p_acc_36
                 LET tm_queb = 0

                 EXECUTE sql_queb USING reg_solicitud.n_seguro
                                  INTO tm_queb

                 IF tm_queb >= 800 AND tm_queb <= 899 THEN
                    LET ind_quebranto = TRUE
                    EXIT FOREACH
                 END IF

              IF l_p_acc_36 is null  OR
                 l_p_acc_36 = 0      THEN

                 LET l_p_acc_36 = 0
              ELSE
                 LET l_acc_36 = l_acc_36 + l_p_acc_36
              END IF

              PREPARE  sql_67       FROM   g_sql_67 -- hist de dis_cuenta
              EXECUTE  sql_67       USING   reg_solicitud.n_seguro ,
                                            mr_folio.fecha_envio_saldos,
                                            l_scta  ,
                                            g_siefore,
                                            mr_folio.folio
                                    INTO    l_p_acc_37
               LET tm_queb = 0

               EXECUTE sql_queb USING reg_solicitud.n_seguro
                                INTO tm_queb

                IF tm_queb >= 800 AND tm_queb <= 899 THEN
                    LET ind_quebranto = TRUE
                   EXIT FOREACH
                END IF

                IF l_p_acc_37 is null  OR
                    l_p_acc_37 = 0      THEN

                    LET l_p_acc_37 = 0
                ELSE
                    LET l_acc_37 = l_acc_37 + l_p_acc_37
                END IF

            END FOREACH --dis_cuenta(s)
            LET      l_pesos_36 = l_acc_36 * g_sie_inf[g_siefore].precio_accion
            LET      l_pesos_37 = l_acc_37 * g_sie_inf[g_siefore].precio_accion

            IF ind_quebranto = TRUE THEN
               LET l_veces = 1
            END IF

            END IF -- si encuentra en taa_cd_desagrupa_subcta
            ---- fin desagrupar la scta 31 en 36 y 37 procesar

            #Se valida que el saldo a provisionar no sea mayor al saldo al dia
            IF ((l_acc_36 + l_acc_37) > g_acciones) AND g_scta = 31 THEN
               DISPLAY "POSIBLE QUEBRANTO NSS: ",reg_solicitud.n_seguro, " CURP:", reg_solicitud.n_unico,
                       " SALDO DIA: ",g_acciones , " SALDO SUB31: ",l_acc_36,
                       " SALDO SUB39: ",l_acc_37
            
               CONTINUE FOREACH
            END IF

            FOR l_i = 1 TO l_veces
                    IF l_id_scta_31 = TRUE THEN
                       CASE l_i
                         WHEN 1
                         IF ind_quebranto = FALSE THEN
                            LET g_acciones = l_acc_36
                            LET l_pesos = l_pesos_36
                            LET g_pesos = l_pesos_36
                            LET l_scta = 31
                            LET g_scta = 31
                         ELSE
                            LET l_scta = 31
                            LET g_scta = 31
                        END IF
                            EXIT CASE
                         WHEN 2
                            LET g_acciones = l_acc_37
                            LET l_pesos = l_pesos_37
                            LET g_pesos = l_pesos_37
                            LET l_scta = 39
                            LET g_scta = 39
                            EXIT CASE
                       END CASE
                    END IF
                    ##################################################################################################
                    
                    
                    
                    LET      g_vol_patronal          =  g_fecha_nula  
                    LET      g_vol_ventanilla        =  g_fecha_nula 
                    LET      g_aport_l_plazo         =  g_fecha_nula
                    LET      g_perspect_l_plazo      =  g_fecha_nula
                    
                    IF       l_pesos              >=  g_monto_minimo    THEN
                             CALL     F_140_inserta_dis_provision()
                             LET      g_imp_2_dec                       =  g_pesos
                             LET      g_sie[l_scta,g_siefore].acciones  =
                                      g_sie[l_scta,g_siefore].acciones  +  g_acciones
                             LET      g_sie[l_scta,g_siefore].pesos     =
                                      g_sie[l_scta,g_siefore].pesos     +  g_pesos
                             LET      g_sie[l_scta,g_siefore].pesos2d   =
                                      g_sie[l_scta,g_siefore].pesos2d   +  g_imp_2_dec
                             LET      g_saldos[l_scta].acciones         =
                                      g_saldos[l_scta].acciones         +  g_acciones
                             LET      g_saldos[l_scta].pesos            =
                                      g_saldos[l_scta].pesos            +  g_imp_2_dec
                             LET      g_sumario[g_scta].acciones        = 
                                      g_sumario[g_scta].acciones        +  g_acciones
            
                             IF      (g_scta            <>  2       AND
                                      g_scta            <>  3       AND
                                      g_scta            <>  10      AND
                                      g_scta            <>  11      AND
                                      g_scta            <>  12      AND
                                      g_scta            <>  16      AND
                                      g_scta            <>  33)    THEN
                                      LET      g_sumario[g_scta].pesos   =
                                               g_sumario[g_scta].pesos   +  g_imp_2_dec
                             END IF
                             LET      g_sum_rep[g_scta,g_siefore].acciones       =
                                      g_sum_rep[g_scta,g_siefore].acciones + g_acciones
                             LET      g_sum_rep[g_scta,g_siefore].pesos          =
                                      g_sum_rep[g_scta,g_siefore].pesos    +  g_pesos
            
                    END IF
            END FOR --li 1 o 2 por subcuenta 31
   END FOREACH 
   FOR      g_scta                  =  2     TO  33
            IF       g_scta                  <>  2      AND
                     g_scta                  <>  3      AND
                     g_scta                  <>  10     AND
                     g_scta                  <>  11     AND
                     g_scta                  <>  12     AND
                     g_scta                  <>  16     AND
                     g_scta                  <>  33     THEN
                     CONTINUE  FOR
            END  IF
            LET      g_saldos[g_scta].pesos       =  cero
            FOR      g_siefore       =  g_siefore_ini     TO  g_siefore_fin
	             LET      g_sie[g_scta,g_siefore].pesos2d       =
	                      g_sie[g_scta,g_siefore].pesos

                     LET      g_saldos[g_scta].pesos                =
	                      g_saldos[g_scta].pesos                +
                              g_sie[g_scta,g_siefore].pesos2d

                     LET      g_sumario[g_scta].pesos               =
	                      g_sumario[g_scta].pesos               +
                              g_sie[g_scta,g_siefore].pesos2d
            END FOR
   END FOR
   CALL     F_134_bloques_siefore_rcv()
   CALL     F_137_fecha_vol_patronales()    #APORT.  VOLUNTARIAS  PATRONALES
   CALL     F_138_fecha_vol_ventanilla()    #APORT.  VOLUNTARIAS  VENTANILLA
   CALL     F_139_fecha_aport_l_plazo()     #APORT.  VOLUNTARIAS  PATRONALES
   CALL     F_139_fecha_perspect_l_plazo()  #APORT.  VOLUNTARIAS  VENTANILLA

END FUNCTION

FUNCTION    F_133_1_inicializa_rcv()
   FOR      g_scta                 =  g_scta_ini      TO  g_scta_fin
            IF       g_scta                  =  4     OR
                     g_scta                  =  8     OR
                     g_scta                  =  14    OR
                     g_scta                  =  35    OR
                     g_scta                  =  36    THEN
                     CONTINUE  FOR
            END IF
            LET      g_saldos[g_scta].acciones    =  cero
            LET      g_saldos[g_scta].pesos       =  cero
            FOR      g_siefore         =  g_siefore_ini       TO  g_siefore_fin
                     LET      g_sie[g_scta,g_siefore].acciones    =  cero
                     LET      g_sie[g_scta,g_siefore].pesos       =  cero
                     LET      g_sie[g_scta,g_siefore].pesos2d     =  cero
            END FOR
   END FOR
END FUNCTION

FUNCTION    F_134_bloques_siefore_rcv()
   FOR      g_scta            =  g_scta_ini    TO  g_scta_fin
            IF       g_scta                     =  4      OR
                     g_scta                     =  8      OR      
                     g_scta                     =  14     OR
                     g_scta                     =  35     OR
                     g_scta                     =  36     THEN
                     CONTINUE  FOR
            END IF
            FOR      g_siefore         =  g_siefore_ini       TO  g_siefore_fin
                     IF       g_sie[g_scta,g_siefore].pesos    =  cero   THEN
                              CONTINUE  FOR
                     END IF
                     CALL     F_135_arma_bloques_siefore_rcv()
                     LET      g_tot_bloques_rcv     =  g_tot_bloques_rcv   +  1
            END FOR
   END FOR
   FOR      g_scta    =  1          TO  416
            IF       g_bloques_rcv[g_scta]           =  "?"      THEN
                     LET      g_bloques_rcv[g_scta]           =  " "
            END IF
   END FOR
   IF       g_tot_bloques_rcv       >=  8        THEN
            FOR      g_scta          =  1        TO  416
                     IF       g_bloques_rcv_reg_1[g_scta]     =  "?"      THEN
                              LET      g_bloques_rcv_reg_1[g_scta]     =  " "
                     END IF
            END FOR
   END IF
   LET      g_tot_bloques_rcv       =  g_tot_bloques_rcv     -  1
END FUNCTION

FUNCTION    F_135_arma_bloques_siefore_rcv()
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sumario[g_scta].scta_proc     USING    "&&"       CLIPPED
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sie[g_scta,g_siefore].pesos2d    *   100
                             USING     "&&&&&&&&&&&&&&&"        CLIPPED
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sie_inf[g_siefore].nom_siefore                    CLIPPED
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sie[g_scta,g_siefore].acciones                 *  1000000
                             USING     "&&&&&&&&&&&&&&&&"       CLIPPED
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sie_inf[g_siefore].precio_accion               *  1000000
                             USING     "&&&&&&&&&&&"            CLIPPED
   IF       g_tot_bloques_rcv       =  8        THEN
            LET      g_bloques_rcv_reg_1     =  g_bloques_rcv   CLIPPED
            LET      g_bloques_rcv           =  NULL
   END IF
END FUNCTION

FUNCTION    F_137_fecha_vol_patronales()
## fecha de ultimo retiro o primera aportacion  Voluntaria Patronal
   DEFINE   l_fecha                         DATE
   DEFINE   l_mov_aporte                    ,
            l_mov_retiro                    ,
            l_siefore                       ,
            l_scta                          SMALLINT,
            l_acciones                      DECIMAL(16,6),
            l_pesos                         DECIMAL(16,2)
   
   # CPL-1153 Validando que exista saldo en la subcuentas de Aportaciones Voluntarias Patronales
   # en caso de no existir saldo, la fecha será nula
   LET      g_vol_patronal      =  g_fecha_nula
   CALL     arma_querys_TCAAB006S()
   
   DECLARE  c_saldo_pat       CURSOR     FOR  sql_42
   FOREACH  c_saldo_pat    
            USING     reg_solicitud.n_seguro,
                      cero        ,   #  subcuenta
                      cero        ,
                      g_today
             INTO     l_scta      ,
                      l_siefore   ,
                      l_acciones  ,
                      l_pesos
             
      IF (l_scta = 3 OR l_scta = 22) AND 
         (l_acciones > 0.000000) THEN
         LET      g_c_scta             = " 3, 22 " # CPL-1153
         EXECUTE  sql_48
         FOREACH  c_dis_cuentas    INTO  g_tabname
                  CALL     arma_querys_TCAAB006S()
                  PREPARE  sql_23          FROM     g_sql_23
                  PREPARE  sql_24          FROM     g_sql_24
                  EXECUTE  sql_23      USING  reg_solicitud.n_seguro
                  EXECUTE  sql_24      USING  reg_solicitud.n_seguro
         END FOREACH
         EXECUTE  sql_31   INTO  l_fecha
         IF       l_fecha                 IS  NULL   THEN
                  EXECUTE   sql_32      INTO  l_fecha
         END IF
         
         IF       g_tipo_solicitud        =  2    THEN
                  IF       g_finicta                =  l_fecha     THEN
                           EXECUTE  sql_56      USING  reg_solicitud.n_seguro
                                                 INTO  l_fecha
                  END IF
         END IF
         IF       l_fecha       IS  NOT  NULL    THEN
                  LET      g_vol_patronal      =  l_fecha    USING  "YYYYMMDD"
         END IF
      END IF
   END FOREACH  
END FUNCTION

FUNCTION    F_138_fecha_vol_ventanilla()
## fecha de ultimo retiro o primera aportacion  Voluntaria Ventanilla
   DEFINE   l_fecha                              DATE
   DEFINE   l_mov_aporte                    ,
            l_mov_retiro                    ,
            l_siefore                       ,
            l_scta                          SMALLINT,
            l_acciones                      DECIMAL(16,6),
            l_pesos                         DECIMAL(16,2)
     
   # CPL-1153 Validando que exista saldo en la subcuentas de Aportaciones Voluntarias Ventanilla
   # en caso de no existir saldo, la fecha será nula
   LET      g_vol_ventanilla      =  g_fecha_nula
   CALL     arma_querys_TCAAB006S()
   
   DECLARE  c_saldo_vol       CURSOR     FOR  sql_42
   FOREACH  c_saldo_vol    
            USING     reg_solicitud.n_seguro,
                      cero        ,   #  subcuenta
                      cero        ,
                      g_today
             INTO     l_scta      ,
                      l_siefore   ,
                      l_acciones  ,
                      l_pesos
             
      IF (l_scta = 10 OR l_scta = 23) AND 
         (l_acciones > 0.000000) THEN
         LET      g_c_scta             =  " 10, 23 " # CPL-1153
         EXECUTE  sql_48
         FOREACH  c_dis_cuentas INTO g_tabname
                  CALL     arma_querys_TCAAB006S()
                  PREPARE  sql_23_10       FROM     g_sql_23
                  PREPARE  sql_24_10       FROM     g_sql_24
                  PREPARE  sql_25_110    FROM  g_sql_25_110
                  EXECUTE  sql_23_10    USING  reg_solicitud.n_seguro
                  EXECUTE  sql_24_10    USING  reg_solicitud.n_seguro
                  EXECUTE  sql_25_110   USING  reg_solicitud.n_seguro
         END FOREACH
         EXECUTE  sql_31                 INTO  l_fecha
         IF       l_fecha                  IS  NULL        THEN
                  EXECUTE   sql_32       INTO  l_fecha
         END IF
         IF       l_fecha                  IS  NULL        THEN
                  EXECUTE   sql_32_110   INTO  l_fecha
         END IF
         
         IF       g_tipo_solicitud       =  2     THEN
                  IF       g_finicta                 =  l_fecha     THEN
                           EXECUTE   sql_57      USING  reg_solicitud.n_seguro
                                                  INTO  l_fecha
                  END IF
         END IF
         IF       l_fecha          IS  NOT  NULL  THEN
                  LET      g_vol_ventanilla       =  l_fecha   USING  "YYYYMMDD"
         END IF
      END IF
   END FOREACH   
END FUNCTION

FUNCTION    F_139_fecha_aport_l_plazo()
## fecha de ultimo retiro o primera aportacion  largo plazo
   DEFINE   l_fecha                         DATE
   
   LET      g_c_scta             = " 15, 16, 26, 27 " # CPL-1153
   EXECUTE  sql_48
   FOREACH  c_dis_cuentas    INTO  g_tabname
            CALL     arma_querys_TCAAB006S()
            PREPARE  sql_23_26       FROM     g_sql_23
            PREPARE  sql_24_26       FROM     g_sql_24
            EXECUTE  sql_23_26   USING  reg_solicitud.n_seguro
            EXECUTE  sql_24_26   USING  reg_solicitud.n_seguro
   END FOREACH
   EXECUTE  sql_31   INTO  l_fecha
   IF       l_fecha                 IS  NULL   THEN
            EXECUTE   sql_32      INTO  l_fecha
   END IF
   
   IF       l_fecha       IS  NOT  NULL    THEN
            LET      g_aport_l_plazo            =  l_fecha    USING  "YYYYMMDD"
   ELSE
            LET      g_aport_l_plazo            =  g_fecha_nula
   END IF
END FUNCTION

FUNCTION    F_139_fecha_perspect_l_plazo()
## fecha de ultimo retiro o primera aportacion de perspectiva  largo plazo
   DEFINE   l_fecha                         DATE
   
   LET      g_c_scta             = " 20, 21, 28, 29 " # CPL-1153
   EXECUTE  sql_48
   FOREACH  c_dis_cuentas    INTO  g_tabname
            CALL     arma_querys_TCAAB006S()
            PREPARE  sql_23_28       FROM     g_sql_23
            PREPARE  sql_24_28       FROM     g_sql_24
            EXECUTE  sql_23_28   USING  reg_solicitud.n_seguro
            EXECUTE  sql_24_28   USING  reg_solicitud.n_seguro
   END FOREACH
   EXECUTE  sql_31   INTO  l_fecha
   IF       l_fecha                 IS  NULL   THEN
            EXECUTE   sql_32      INTO  l_fecha
   END IF
   
   IF       l_fecha       IS  NOT  NULL    THEN
            LET      g_perspect_l_plazo       =  l_fecha   USING  "YYYYMMDD"
   ELSE
            LET      g_perspect_l_plazo       =  g_fecha_nula
   END IF
END FUNCTION

FUNCTION    F_140_inserta_dis_provision()
   DEFINE   verifica_provision                 ,
            v_folio_sua                        SMALLINT
   DEFINE   l_acciones                         DEC(16,6),
            l_pesos                            DEC(16,2) 
   LET      l_acciones = 0
   LET      l_pesos = 0
   LET      v_folio_sua                  =  9999
   LET      l_acciones                   =  g_acciones      *   -1
   LET      l_pesos                      =  g_pesos         *   -1
   
   
   IF       g_pesos              >=  g_monto_minimo    THEN
      LET      g_si_hay_comple              =  1
      DECLARE  spl_provisiona     CURSOR  FOR  sql_43
      FOREACH  spl_provisiona           USING
               reg_ctr_folio.folio             ,  #folio
               v_folio_sua                            ,  #folio_sua
               reg_solicitud.n_seguro         ,  #nss
               g_scta                                 ,  #subcuenta
               g_tipo_mov                             ,  #tipo_movimiento
               reg_solicitud.cont_servicio    ,  #consecutivo
               g_siefore                              ,  #siefore
               l_acciones                             ,  #monto_en_acciones  
               l_pesos                                ,  #monto_en_pesos
               g_id_aportante                         ,  #id_aportante
               g_today                                   #fecha_proceso
                                        INTO     verifica_provision
      END FOREACH
   END IF
END FUNCTION

FUNCTION    F_145_insert_tmp_envio_2dec()
   FOR      g_scta         =  g_scta_ini         TO  g_scta_fin
            IF       g_saldos[g_scta].pesos       >  cero  THEN
                     EXECUTE   sql_34
                       USING   reg_ctr_folio.folio,
                               reg_solicitud.n_seguro,
                               g_scta,
                               g_saldos[g_scta].pesos,
                               g_provisionado
            END IF
   END FOR
END FUNCTION

REPORT      R_150_arma_detalles()
   DEFINE   g_fecha_nula_ven                DATE
   DEFINE   lc_nss_aux                      CHAR(11)
   OUTPUT
            PAGE      LENGTH    1
            LEFT      MARGIN    0
            RIGHT     MARGIN    0
            TOP       MARGIN    0
            BOTTOM    MARGIN    0
   FORMAT

   ON EVERY ROW 

  ###########     registro  de  detalle_02  de   vivienda       ############
     LET      g_num_registro       =  g_num_registro       +  1
     IF      (reg_solicitud.id_regs_reenviados    <>  1  AND
              reg_solicitud.id_regs_reenviados    <>  2)     THEN
              LET      reg_solicitud.id_regs_reenviados     =  " "
     END  IF
     
     LET lc_nss_aux = reg_solicitud.n_seguro
     IF       g_por_curp             THEN
              LET       reg_solicitud.n_seguro         =
                        reg_solicitud.n_seguro_cedente
     END IF
     PRINT    COLUMN   01                                ,
             "02"                                        ,   #id_1
              g_num_registro      USING  "&&&&&&&&&&"    ,   #id_2
              reg_solicitud.tipo_recep_cuenta            ,   #id_3
              3       SPACES                             ,   #id_4
              reg_solicitud.tipo_ced_cuenta              ,   #id_5
              reg_solicitud.cve_ced_cuenta               ,   #id_6
              reg_solicitud.tipo_traspaso                ,   #id_7
              reg_ctr_folio.fecha_envio_saldos USING "YYYYMMDD", #id_8
              reg_solicitud.n_unico                      ,   #id_9_viv
              g_curp_unificador                          ,   #id_10_viv
              reg_solicitud.n_seguro                     ,   #id_11_viv
              g_nss_unificador                           ,   #id_12_viv
              reg_solicitud.paterno                      ,   #id_13_viv
              reg_solicitud.materno                      ,   #id_14_viv
              reg_solicitud.nombre                       ,   #id_15_viv
              reg_solicitud.fecha_recep_solic  USING "YYYYMMDD" ,#id_16
              g_saldos[35].acciones  * 1000000 USING "&&&&&&&&&&&&&&&",#id_17
              g_saldos[35].pesos     * 100     USING "&&&&&&&&&&&&&&&",#id_18
              g_saldos[4].acciones   * 1000000 USING "&&&&&&&&&&&&&&&",#id_19
              g_saldos[4].pesos      * 100     USING "&&&&&&&&&&&&&&&",#id_20
              g_saldos[14].acciones  * 1000000 USING "&&&&&&&&&&&&&&&",#id_21
              g_saldos[14].pesos     * 100     USING "&&&&&&&&&&&&&&&",#id_22
              g_saldos[8].acciones   * 1000000 USING "&&&&&&&&&&&&&&&",#id_23
              g_saldos[8].pesos      * 100     USING "&&&&&&&&&&&&&&&",#id_24
              reg_solicitud.id_credito_fovissste          ,            #id_25
              reg_solicitud.id_credito_infonavit          ,            #id_26
              2       SPACES                              ,            #id_27
              15      SPACES                              ,            #id_28
              reg_solicitud.id_regimen_pension            ,            #id_29
              78      SPACES                              ,            #id_30-33
              g_num_aport_viv92        USING  "&&&&&"     ,            #id_34
              g_dias_cuota_social      USING  "&&&&&"     ,            #id_35
              g_num_aport_viv_issste   USING  "&&&&&"     ,            #id_36
              198     spaces                                           #id_37
     LET      g_num_registro          =  g_num_registro  +  1
     IF       g_tot_bloques_rcv       <  8   THEN
              LET      g_bloques_rcv_reg_1    =  g_bloques_rcv   CLIPPED
     END IF
              #####    registro  1 de  detalle_05    rcv     ############
     PRINT    COLUMN   01,
              "05"                                       ,   #id_01
              g_num_registro   USING  "&&&&&&&&&&"       ,   #id_02
              reg_solicitud.tipo_recep_cuenta            ,   #id_03
              reg_solicitud.cve_recep_cuenta             ,   #id_04
              reg_solicitud.tipo_ced_cuenta              ,   #id_05
              reg_solicitud.cve_ced_cuenta               ,   #id_06
              reg_solicitud.tipo_traspaso                ,   #id_07
              g_vol_patronal                             ,   #id_08
              g_vol_ventanilla                           ,   #id_09
              g_aport_l_plazo                            ,   #id_10
              g_perspect_l_plazo                         ,   #id_11
              reg_solicitud.n_unico                      ,   #id_12
              reg_solicitud.n_seguro                     ,   #id_13
              g_today         USING  "YYYYMMDD"          ,   #id_14
              g_bloques_rcv_reg_1                        ,   #id_15_54
              141     SPACES                                 #id_55_59
  #########   arma registro detalle_05 de los bloques 9 en adelante ##########
     IF       g_tot_bloques_rcv           >   8   THEN
              LET       g_num_registro       =  g_num_registro       +  1
     PRINT    COLUMN  01,
              "05"                                       ,    #id_01
              g_num_registro          USING  "&&&&&&&&&&",    #id_02
              reg_solicitud.tipo_recep_cuenta            ,    #id_03
              reg_solicitud.cve_recep_cuenta             ,    #id_04
              reg_solicitud.tipo_ced_cuenta              ,    #id_05
              reg_solicitud.cve_ced_cuenta               ,    #id_06
              reg_solicitud.tipo_traspaso                ,    #id_07
              g_vol_patronal                             ,    #id_08
              g_vol_ventanilla                           ,    #id_09
              g_aport_l_plazo                            ,    #id_10
              g_perspect_l_plazo                         ,    #id_11
              reg_solicitud.n_unico                      ,    #id_12
              reg_solicitud.n_seguro                     ,    #id_13
              g_today         USING  "YYYYMMDD"          ,    #id_14
              g_bloques_rcv                              ,    #id_15_54
              141     SPACES                                  #id_55_59
     END IF
    IF        g_por_curp            THEN
      LET       reg_solicitud.n_seguro  = lc_nss_aux
    END IF
END REPORT

FUNCTION    F_200_fin()
   DEFINE   l_ruta                       CHAR(300)
   CALL     F_210_arma_encabezado()
   CALL     F_220_arma_sumario()
   CALL     F_240_genera_reporte_saldos()
   LET      l_ruta         =  g_seg_modulo.ruta_envio     CLIPPED,"/",
                             "TRASP_SAL_COMP.",g_today    USING   "MMDD","-",
                              g_hora[1,2],g_hora[4,5]
   LET      g_current       =  CURRENT
   DISPLAY g_current," :RUTA Y NOMBRE DEL ARCHIVO COMPLE. : "
   DISPLAY  l_ruta CLIPPED 
   CALL     F_250_genera_archivo_saldos()   #gst
   IF       reg_ctr_folio.calculo_interes     =  1    THEN
            EXECUTE  sql_39     USING  reg_ctr_folio.folio
   END IF
   LET      g_comando             =  "lp ", g_comando
   RUN      g_comando
   LET      g_comando             =
           "echo \'chmod  777 ",g_seg_modulo.ruta_envio CLIPPED,
           "/*_12*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN      g_comando
   LET      g_comando             = "rm  arch_paso "
   RUN      g_comando
   LET      g_comando             = "rm  err_paso "
   RUN      g_comando
   LET      g_current             =  CURRENT
   DISPLAY  g_current," :FIN DE PROGRAMA       : TCAAB006"
END FUNCTION

FUNCTION    F_210_arma_encabezado()
   LET      g_cza            =  g_seg_modulo.ruta_envio CLIPPED,"/CZA_12"
   START    REPORT     R_211_arch_encabezado        TO  g_cza
            OUTPUT     TO  REPORT    R_211_arch_encabezado()
   FINISH   REPORT     R_211_arch_encabezado
END FUNCTION

REPORT      R_211_arch_encabezado()
   DEFINE   l_consec_lote_dia              SMALLINT
    OUTPUT
            PAGE     LENGTH  1
  	    LEFT     MARGIN  0
	    RIGHT    MARGIN  0
	    TOP      MARGIN  0
	    BOTTOM   MARGIN  0
    FORMAT
    ON EVERY ROW 
       ###########     registro  de  encabezado  del  archivo      ############
      LET      l_consec_lote_dia       =  cero
      EXECUTE  sql_55        USING   reg_ctr_folio.fecha_envio_saldos
                              INTO   l_consec_lote_dia
      PRINT    COLUMN   01,
               "01"                        ,  #id_01
               "02"                        ,  #id_02
               "12"                        ,  #id_03
               "01"                        ,  #id_04
               g_afore_cod      USING "&&&",  #id_05
               "01"                        ,  #id_06
               "   "                       ,  #id_07
               "009"                       ,  #id_08
               reg_ctr_folio.fecha_envio_saldos  USING "YYYYMMDD", #id_09
               l_consec_lote_dia  USING  "&&&"         ,  #id_10
               "02"                        ,  #id_11
               "  "                        ,  #id_12
               "         "                 ,  #id_13
               607     SPACES                            
END REPORT

FUNCTION    F_220_arma_sumario()
   LET      g_sum_rcv               =   NULL
   FOR      g_scta                  =  g_scta_ini   TO  g_scta_fin
            IF       g_scta                =  6     OR
                     g_scta                =  9     OR
                     g_scta                =  4     OR
                     g_scta                =  8     OR
                     g_scta                =  14    OR
                     g_scta                =  22    OR
                     g_scta                =  23    OR
                     g_scta                =  24    OR
                     g_scta                =  25    OR
                     g_scta                =  27    OR
                     #g_scta                =  34    OR          # CPL-1854
                     g_scta                =  35    OR
                     g_scta                =  36    OR
                     g_scta                =  37    THEN
                     CONTINUE  FOR
            END IF
            IF       g_sumario[g_scta].pesos        <>  cero     THEN
                     EXECUTE   sql_16      USING    reg_ctr_folio.folio,
                                                    g_scta,
                                                    g_sumario[g_scta].pesos,
                                                    g_num_registro
            END IF
            IF       g_sumario[g_scta].pesos        <>  cero     THEN
                     LET       g_sum_rcv             =  g_sum_rcv       CLIPPED,
                               g_sumario[g_scta].scta_proc  USING  "&&" CLIPPED
                     LET       g_sum_rcv             =  g_sum_rcv       CLIPPED,
                               g_sumario[g_scta].pesos     *  100
                               USING   "&&&&&&&&&&&&&&&"   CLIPPED
            END IF
   END FOR
   LET      g_sum             =  g_seg_modulo.ruta_envio CLIPPED,"/SUM_12" 
   START    REPORT    R_221_arma_reg_sumario        TO  g_sum
            OUTPUT    TO    REPORT     R_221_arma_reg_sumario() #3
   FINISH   REPORT    R_221_arma_reg_sumario
END FUNCTION

REPORT  R_221_arma_reg_sumario()
   OUTPUT
            PAGE     LENGTH   1
            LEFT     MARGIN   0
            RIGHT    MARGIN   0
            TOP      MARGIN   0
            BOTTOM   MARGIN   0
   FORMAT
   ON    EVERY  ROW 
       ###########     registro  de  sumario  del  archivo     ############
   PRINT   COLUMN  01,
         "09"                                                          , #id_01
          g_num_registro                     USING "&&&&&&&&&"         , #id_02
          g_sumario[4].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_03
          g_sumario[4].pesos      * 100      USING "&&&&&&&&&&&&&&&"   , #id_04
          g_sumario[14].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_05
          g_sumario[14].pesos     * 100      USING "&&&&&&&&&&&&&&&"   , #id_06
          g_sumario[8].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_07
          g_sumario[8].pesos      * 100      USING "&&&&&&&&&&&&&&&"   , #id_08
          g_sumario[35].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_09
          g_sumario[35].pesos     * 100      USING "&&&&&&&&&&&&&&&"   , #id_10
          g_sumario[36].acciones  * 10000    USING "&&&&&&&&&&&&&&&&&" , #id_11
          g_sum_rcv                                             ,  #id_12_al_67
          14      SPACES                                                 #id_68

END REPORT

FUNCTION    F_250_genera_archivo_saldos()
   LET      g_cat     =  "cat ",g_seg_modulo.ruta_envio  CLIPPED,"/CZA_12 ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/DET_12 ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/SUM_12 > ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/",
                         "TRASP_SAL_COMP.",g_today       USING "MMDD","-",
                          g_hora[1,2],g_hora[4,5]
   RUN      g_cat
END FUNCTION 

FUNCTION    F_240_genera_reporte_saldos()
   LET      g_comando         =  g_seg_modulo.ruta_listados CLIPPED, "/",
            g_usuario    CLIPPED ,".REP_COMP.",g_today  USING  "DDMMYY","-",
            g_hora[1,2],g_hora[4,5]  CLIPPED
   DISPLAY  g_current," :RUTA Y NOMBRE DEL REPORTE OPE. 12: "
   DISPLAY  g_comando    CLIPPED
   START    REPORT       R_241_rep_saldos_tot     TO  g_comando
            OUTPUT       TO       REPORT   R_241_rep_saldos_tot()
   FINISH   REPORT       R_241_rep_saldos_tot
END FUNCTION 

REPORT R_241_rep_saldos_tot()
   OUTPUT
            PAGE      LENGTH   66
            LEFT      MARGIN   0
	    RIGHT     MARGIN   132
            TOP       MARGIN   0
	    BOTTOM    MARGIN   0
   FORMAT
   PAGE HEADER
	PRINT    COLUMN   2,"TCAAB006",
	         COLUMN   72,"Pagina:",PAGENO USING "<<<<"
	PRINT    COLUMN   02,'\033(s7B',"RESUMEN DE TRASPASOS COMPLEMENTARIOS",
                                     " AFORE (CEDENTE)",
                 COLUMN   63,"FECHA:",
                 COLUMN   70, g_today USING "dd-mm-yyyy",'\033(s0B' CLIPPED
        PRINT    COLUMN   02,g_afore_cod,"     ",g_raz_social
        PRINT    COLUMN   02,'\033(s7B',"FOLIO   : ",reg_ctr_folio.folio
                 USING "########",'\033(s0B' CLIPPED
        PRINT    COLUMN   02,"ARCHIVO : ",'\033(s7B',"TRASP_SAL_COMP.",
                          g_today     USING  "MMDD","-",
                          g_hora[1,2],g_hora[4,5],'\033(s0B'    CLIPPED
        PRINT    COLUMN   02,"RUTA    : /safre_prc/taa/envio"
        PRINT    COLUMN   02,"FECHA DE PRESENTACION : ",g_today
                          USING     "DD/MM/YYYY"
  ON EVERY ROW
       SKIP     2     LINE
      PRINT  COLUMN  02,"__________________________________________________",
                        "______________________________"
      PRINT  COLUMN  20,'\033(s7B',"TOTALES DE VIVIENDA",'\033(s0B'  CLIPPED
      PRINT  COLUMN  03,'\033(s7B',"VALOR PARTICIPACION : ",
                        g_sie_inf[11].precio_accion   USING
                                "##.##############",'\033(s0B'   CLIPPED,
                        "  AIVS: ", g_sie_inf[12].precio_accion 
                          USING    "##.##############",'\033(s0B'   CLIPPED
      PRINT  COLUMN  28, "MONTO EN PARTICIPACIONES/AIVS",
             COLUMN  68, "MONTO EN PESOS"
      PRINT  COLUMN  02,"__________________________________________________",
                        "______________________________"
      PRINT  COLUMN  01,g_sumario[04].nom_scta,
             COLUMN  35,g_sum_rep[04,1].acciones USING "###,###,###,###.######",
             COLUMN  60,g_sum_rep[04,1].pesos    USING "###,###,###,###.######"
      PRINT  COLUMN  01,g_sumario[14].nom_scta,
             COLUMN  35,g_sum_rep[14,1].acciones USING "###,###,###,###.######",
             COLUMN  60,g_sum_rep[14,1].pesos    USING "###,###,###,###.######"
      PRINT  COLUMN  01,g_sumario[08].nom_scta,
             COLUMN  35,g_sum_rep[08,1].acciones USING "###,###,###,###.######",
             COLUMN  60,g_sum_rep[08,1].pesos    USING "###,###,###,###.######"
      PRINT  COLUMN  01,g_sumario[35].nom_scta,
             COLUMN  35,g_sum_rep[35,1].acciones USING "###,###,###,###.######",
             COLUMN  60,g_sum_rep[35,1].pesos    USING "###,###,###,###.######"
      PRINT  COLUMN  01,g_sumario[36].nom_scta,
             COLUMN  35,g_sum_rep[36,1].acciones USING "###,###,###,###.######",
             COLUMN  60,g_sum_rep[36,1].pesos    USING "###,###,###,###.######"
      FOR    g_siefore               =  g_siefore_ini  TO  g_siefore_fin
             IF      (g_siefore           <>  1    AND 
                      g_siefore           <>  2    AND
                      g_siefore           <>  3    AND 
                      g_siefore           <>  4    AND
                      g_siefore           <>  5    AND
                      g_siefore           <>  6 )  THEN
                      CONTINUE  FOR
             END IF
             FOR      g_scta               =  g_scta_ini     TO  g_scta_fin
                   IF    (g_scta           =  4     OR
                          g_scta           =  8     OR
                          g_scta           =  14    OR
                          g_scta           =  35    OR   
                          g_scta           =  36  ) THEN
                          CONTINUE  FOR
                   END IF
                   IF     g_scta           =  1  THEN
                             SKIP   2  LINE
	                 PRINT    COLUMN   02,"_______________________________",
                                           "__________________________________",
                                           "_______________"
                         PRINT    COLUMN   05,'\033(s7B',
                                  "TOTALES RCV SIEFORE BASICA ",
                                  g_siefore  USING  "##"," ","VALOR ACCION :",
                                  g_sie_inf[g_siefore].precio_accion,
                                  '\033(s0B' CLIPPED
                         PRINT    COLUMN   40,"MONTO EN ACCIONES",
                                  COLUMN   68,"MONTO EN PESOS"
	                 PRINT    COLUMN   02,"_______________________________",
                                           "_________________________________",
                                           "________________"
                   END IF
                   IF      g_siefore                =  6     THEN
                           IF     (g_scta          <>  3     AND
                                   g_scta          <>  10    AND
                                   g_scta          <>  11    AND
                                   g_scta          <>  12    AND
                                   g_scta          <>  13    AND
                                   g_scta          <>  15)   THEN
                                   CONTINUE  FOR
                           END IF
                   END IF
                PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                         COLUMN   35,g_sum_rep[g_scta,g_siefore].acciones
                                  USING    "###,###,###,###.######",
                         COLUMN   60,g_sum_rep[g_scta,g_siefore].pesos
                                  USING    "###,###,###,###.######"
              END FOR
      END FOR
      PRINT    COLUMN   02,"________________________________________________",
                           "________________________________"
      SKIP     TO    TOP   OF  PAGE
      SKIP     2     LINE
      PRINT    COLUMN   10, '\033(s7B', "TOTALES DEL ARCHIVO DE SALDOS",
               COLUMN   64, "MONTO EN PESOS",'\033(s0B' CLIPPED
      PRINT    COLUMN   02,"________________________________________________",
                           "________________________________"
      SKIP     2     LINE
      PRINT    COLUMN   01,g_sumario[04].nom_scta,
               COLUMN   60,g_sumario[04].pesos     USING  "###,###,###,###.##"
      PRINT    COLUMN   01,g_sumario[08].nom_scta,
               COLUMN   60,g_sumario[08].pesos     USING  "###,###,###,###.##"
      PRINT    COLUMN   01,g_sumario[14].nom_scta,
               COLUMN   60,g_sumario[14].pesos     USING  "###,###,###,###.##"
      PRINT    COLUMN   01,g_sumario[35].nom_scta,
               COLUMN   60,g_sumario[35].pesos     USING  "###,###,###,###.##"
      PRINT    COLUMN   01,g_sumario[36].nom_scta,
               COLUMN   60,g_sumario[36].pesos     USING  "###,###,###,###.##"
      FOR      g_scta             =  g_scta_ini       TO  g_scta_fin
                IF      (g_scta              =  4     OR
                         g_scta              =  8     OR
                         g_scta              =  6     OR
                         g_scta              =  9     OR
                         g_scta              =  14    OR
                         g_scta              =  22    OR
                         g_scta              =  23    OR
                         g_scta              =  24    OR
                         g_scta              =  25    OR
                         g_scta              =  27    OR
                         #g_scta              =  34    OR      # CPL-1854
                         g_scta              =  35    OR
                         g_scta              =  36    OR
                         g_scta              =  37)   THEN
                         CONTINUE  FOR
                END IF
                PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                         COLUMN   60,g_sumario[g_scta].pesos
                                  USING    "###,###,###,###.##"
      END FOR
      PRINT    COLUMN   02,"________________________________________________",
                           "________________________________"
END REPORT
