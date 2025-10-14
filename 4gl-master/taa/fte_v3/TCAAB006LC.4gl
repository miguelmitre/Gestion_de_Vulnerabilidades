############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TCAAB006L=> GENERA ARCHIVO TRASPASO COMPLEMENTARIO (CEDENTE)     #
#Sistema           => TCAA                                                 #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                       #
#Creado            => 24 Septiembre 2004                                   #
############################################################################
DATABASE     safre_af
GLOBALS
   DEFINE    g_current                         DATETIME YEAR TO SECOND
   DEFINE 
             g_finicta                         ,
             g_fecha_liq_parti                 ,
             g_fecha_saldo_parti               ,
             g_fecha_trasp_desde               ,
             g_today                           DATE
   DEFINE 
             g_siefore                         ,
             g_tipo_mov                        ,
             g_scta                            ,
             g_scta_ini                        ,
             g_scta_fin                        ,
             g_siefore_ini                     ,
             g_siefore_fin                     ,
             g_provisionado                    ,
             g_tipo_solicitud                  ,
             g_liquidada                       ,
             cero                              ,  
             g_tot_bloques_rcv                 ,
             g_si_hay_comple                   SMALLINT
   DEFINE
             g_num_aport_viv92                 ,
             g_num_aport_viv_issste            ,
             g_num_registro                    INTEGER
   DEFINE
             g_precio_parti                    DEC(25,14),
             g_acciones                        DEC(18,6) ,
             g_pesos                           DEC(16,6),
             g_imp_2_dec                       DEC(15,2),
             g_tot_rcv                         DEC(16,2),
             g_monto_minimo                    DEC(15,2)
   DEFINE    
             g_fecha_ult_pri_pat               CHAR(008),
             g_fecha_ult_pri_ven               CHAR(008),
             g_cta_transfe_56_anios            CHAR(001),
             g_mod_nombre_bdnsar               CHAR(001),
             g_iden_cta_mod_nombre             CHAR(001),
             g_cred_en_garantia                CHAR(001),
             g_cod_result_operac               CHAR(002),
             g_diag_proceso                    CHAR(015),
             g_id_aportante                    CHAR(011),
             g_nss_unificador                  CHAR(011),
             g_det                             CHAR(100),
             g_cza                             CHAR(100),
             g_sum                             CHAR(100),
             g_enter                           CHAR(001),
             g_hora                            CHAR(005), 
             g_cat                             CHAR(300),
             g_bloques_rcv                     CHAR(488),
             g_bloques_rcv_reg_1               CHAR(488),
             g_sum_rcv                         CHAR(153),
             g_fecha_nula                      CHAR(008),
             g_comando                         CHAR(100),
             g_afore_cod                       CHAR(003),
             g_raz_social                      CHAR(050),
             g_usuario                         CHAR(008) 
   DEFINE
             g_sie          ARRAY[15,3]   OF   RECORD
             acciones                          DEC(16,6),
             pesos                             DEC(16,6),
             pesos2d                           DEC(16,2)
                                          END  RECORD
   DEFINE
             g_saldos       ARRAY[15]     OF   RECORD
             acciones                          DEC(16,6),
             pesos                             DEC(16,2),
             porc                              DEC(9,6)
                                          END  RECORD
   DEFINE
             g_sumario      ARRAY[15]     OF   RECORD
             nom_scta                          CHAR(30),
             scta_proc                         CHAR(02),
             pesos                             DEC(15,2),
             acciones                          DEC(16,6)
                                          END  RECORD
   DEFINE
             g_sum_rep      ARRAY[15,3]   OF   RECORD
             pesos                             DEC(16,6),
             acciones                          DEC(16,6)
                                          END  RECORD

   DEFINE    g_sie_inf      ARRAY[3]      OF   RECORD
             nom_siefore                       CHAR(008),
             precio_accion                     DEC(11,6)
                                          END  RECORD

   DEFINE    reg_taa_cd_ctr_folio   RECORD  LIKE  safre_af:taa_cd_ctr_folio.*
   DEFINE    reg_taa_cd_det_cedido  RECORD  LIKE  safre_af:taa_cd_det_cedido.*
   DEFINE    g_seg_modulo           RECORD  LIKE  seg_modulo.*
END GLOBALS

GLOBALS  "TCAAB006SC.4gl"   #  ARMA LOS QUERYS PARA EL PROCESO

MAIN
   OPTIONS
            PROMPT   LINE LAST,
            ACCEPT   KEY CONTROL-I

   CALL     F_010_inicio()
   CALL     F_100_proceso()
   CALL     F_200_fin()

END MAIN
 
FUNCTION    F_010_inicio()
   LET      reg_taa_cd_ctr_folio.folio               =  ARG_VAL(1)
   LET      reg_taa_cd_ctr_folio.fecha_envio_saldos  =  ARG_VAL(2)
   LET      reg_taa_cd_ctr_folio.fecha_liquidacion   =  ARG_VAL(3)
   LET      g_fecha_liq_parti                        =  ARG_VAL(4)
   LET      g_fecha_saldo_parti                      =  ARG_VAL(5)
   LET      g_fecha_trasp_desde                      =  ARG_VAL(6)
   LET      g_today                                  =  ARG_VAL(7)
   CALL     STARTLOG("TCAAB006C.log")
   CALL     arma_querys_TCAAB006S()
   CALL     F_015_prepara_querys_TCAAB006()
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
   CALL     F_020_trae_parametros()
   CALL     F_030_muestra_folio_comple()
END FUNCTION

FUNCTION    F_015_prepara_querys_TCAAB006()
   PREPARE  sql_03               FROM  g_sql_03
   PREPARE  sql_04               FROM  g_sql_04
   PREPARE  sql_06               FROM  g_sql_06
   PREPARE  sql_07               FROM  g_sql_07
   PREPARE  sql_07_2             FROM  g_sql_07_2
   PREPARE  sql_08               FROM  g_sql_08
   PREPARE  sql_10               FROM  g_sql_10
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
   PREPARE  sql_17               FROM  g_sql_17
   PREPARE  sql_18               FROM  g_sql_18
   PREPARE  sql_19               FROM  g_sql_19
   PREPARE  sql_20               FROM  g_sql_20
   PREPARE  sql_21               FROM  g_sql_21
   PREPARE  sql_27               FROM  g_sql_27
   PREPARE  sql_28               FROM  g_sql_28
   PREPARE  sql_29               FROM  g_sql_29
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
   PREPARE  sql_54               FROM  g_sql_54   # ind 56 anios
   PREPARE  sql_56               FROM  g_sql_56
   PREPARE  sql_57               FROM  g_sql_57
   PREPARE  sql_34               FROM  g_sql_34
   PREPARE  sql_36               FROM  g_sql_36
   PREPARE  sql_37               FROM  g_sql_37
   PREPARE  sql_38               FROM  g_sql_38
   PREPARE  sql_39               FROM  g_sql_39
   EXECUTE  sql_03               INTO  g_provisionado
   EXECUTE  sql_04               INTO  g_liquidada
   EXECUTE  sql_13               INTO  g_afore_cod,g_raz_social,g_usuario
   EXECUTE  sql_37               INTO  g_scta_fin
   EXECUTE  sql_38               INTO  g_siefore_fin
END FUNCTION

FUNCTION    F_020_trae_parametros()
   DEFINE   l_folios_pendientes                 SMALLINT
   DEFINE   l_taa_cd_subcuentas      RECORD  LIKE safre_af:taa_cd_subcuentas.*
   EXECUTE  sql_21      INTO     l_folios_pendientes
                        USING    g_provisionado
   IF       l_folios_pendientes          THEN
            LET       g_current          =  CURRENT
            PROMPT    "  Proceso Generado Anteriormente  Teclee",
                      " <Enter> Para Salir   "  FOR  g_enter
            EXIT PROGRAM 
   END IF
   LET      g_fecha_nula              =  "00010101"
   EXECUTE  sql_08        INTO  g_monto_minimo  
   EXECUTE  sql_10        INTO  g_seg_modulo.*
   DECLARE  c_scta      CURSOR  FOR    sql_35
   FOREACH  c_scta        INTO  l_taa_cd_subcuentas.*
            LET      g_scta                               =
                     l_taa_cd_subcuentas.subcuenta_safre
            LET      g_sumario[g_scta].scta_proc          =
                     l_taa_cd_subcuentas.subcuenta_procesar
            LET      g_sumario[g_scta].nom_scta           =
                     l_taa_cd_subcuentas.nom_subcuenta
   END FOREACH
   LET      g_det            =  g_seg_modulo.ruta_envio CLIPPED,"/DTT_v3"
END FUNCTION

FUNCTION    F_030_muestra_folio_comple()
   DEFINE   l_cad_desc_rech                   CHAR(100),
            l_registros                       INTEGER
   CALL     F_035_precio_acc_parti()
   LET      reg_taa_cd_ctr_folio.calculo_interes          =  1
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :FOLIO A PROCESAR                  : ",
            reg_taa_cd_ctr_folio.folio      USING "######"
   DISPLAY  g_current," :FECHA DE ENVIO DE SALDOS          : ",
            reg_taa_cd_ctr_folio.fecha_envio_saldos
   DISPLAY  g_current," :FECHA DE LIQUIDACION              : ",
            reg_taa_cd_ctr_folio.fecha_liquidacion
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE BASICA 1 : ",
            g_sie_inf[1].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE BASICA 2 : ",
            g_sie_inf[2].precio_accion    USING  "###.######"
   IF       g_siefore_fin                     =  3      THEN
            DISPLAY  g_current," :PRECIO DE ACCION SIEFORE BASICA 3 : ",
                     g_sie_inf[3].precio_accion    USING  "###.######"
   END IF 
   DISPLAY  g_current," :PRECIO DE PARTICIPACION VIVIENDA  : ",
            g_precio_parti                USING "###.##############"
   EXECUTE  sql_06  #           USING  g_liquidada,g_fecha_trasp_desde
                                 INTO  l_registros
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :NUMERO DE REGISTROS A PROCESAR    : ",
            l_registros   USING  "######"
END FUNCTION

FUNCTION    F_035_precio_acc_parti()
   DEFINE   l_siefore_rcv                     ,
            l_siefore_viv                     SMALLINT
   DEFINE   l_nom_siefore                     CHAR(008),
            l_precio_accion                   DEC(11,6)
   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie      USING    g_today ,g_afore_cod
   FOREACH  cur_sie       INTO    g_siefore,l_nom_siefore,l_precio_accion
            FOR      g_scta    =  1          TO   8
                     IF       l_nom_siefore[g_scta]       =  " "     THEN
                              LET      l_nom_siefore[g_scta]      =  "?"
                     END IF
            END FOR
            LET      g_sie_inf[g_siefore].nom_siefore     =  l_nom_siefore
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH
   LET      l_siefore_viv         =  11
   EXECUTE  sql_07_2     USING    g_fecha_liq_parti , l_siefore_viv
                          INTO    g_precio_parti
   IF       STATUS                =  NOTFOUND    THEN
            LET      g_precio_parti              =  cero
   END IF
END FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_regs_proc                    INTEGER
   DEFINE   l_leidos                       SMALLINT
   LET      l_leidos                       =  cero 
   LET      l_regs_proc                    =  cero
   CALL     F_110_inicializa_sumarios()
   START    REPORT R_150_arma_detalles        TO  g_det
########         GRABA  FOLIO       #############
   EXECUTE  sql_15         USING  reg_taa_cd_ctr_folio.folio,"2",g_today,
                                  reg_taa_cd_ctr_folio.fecha_envio_saldos,
                                  reg_taa_cd_ctr_folio.fecha_liquidacion,
                                  reg_taa_cd_ctr_folio.calculo_interes,
                                 "102",
                                  g_usuario
   LET      g_current              =  CURRENT
   DISPLAY  g_current," :PROCESANDO  INFORMACION ...... "
   DECLARE  c_cedidos      CURSOR   FOR  sql_14
   OPEN     c_cedidos #      USING   g_fecha_trasp_desde
   FOREACH  c_cedidos        INTO   reg_taa_cd_det_cedido.*,
                                    g_tipo_solicitud,g_finicta
            LET      l_leidos                    =  l_leidos    +   1
            LET      g_si_hay_comple             =  0
            CALL     F_130_arma_saldo_viv()
            CALL     F_133_arma_saldo_rcv()
##########              Arma  Registros  de  Detalle        ###########
            IF       g_si_hay_comple        THEN
                     CALL     F_120_arma_datos_generales()
                     CALL     F_145_insert_tmp_envio_2dec()
                     OUTPUT   TO   REPORT     R_150_arma_detalles() #2
                     EXECUTE  sql_36
                              USING    reg_taa_cd_ctr_folio.folio,
                                       reg_taa_cd_det_cedido.n_seguro,
                                       reg_taa_cd_ctr_folio.fecha_envio_saldos,
                                       reg_taa_cd_det_cedido.fecha_trasp,
                                       reg_taa_cd_ctr_folio.fecha_liquidacion,
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
   IF       g_num_registro          =  cero   THEN
            DISPLAY  "Program stopped"
            DISPLAY  "ERROR NO HAY TRASPASOS COMPLEMENTARIOS "
            EXIT     PROGRAM
   END IF
   FINISH   REPORT R_150_arma_detalles
   UPDATE   safre_af:taa_cd_det_cedido
      SET   estado                  =  12
    WHERE   estado                  =  g_liquidada
      AND   n_seguro     IN(SELECT   n_seguro 
                              FROM   taa_cd_det_comple
                             WHERE   folio    =  reg_taa_cd_ctr_folio.folio) 
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
   LET      g_mod_nombre_bdnsar           = ' '
   LET      g_iden_cta_mod_nombre         = ' '
   EXECUTE  sql_17      USING    reg_taa_cd_det_cedido.n_seguro
                         INTO    g_mod_nombre_bdnsar
   #--- TOTAL APORTACIONES VIVIENDA
   CALL     F_121_arma_num_aport_viv92()
   LET      g_num_aport_viv_issste        =  cero
   IF       g_num_aport_viv92            IS  NULL   THEN
            LET      g_num_aport_viv92            =  cero
   END IF
   #------ IDENTIFICADOR DE CREDITO EN GARANTIA ------#
   LET      g_cred_en_garantia            = '0'
   LET      l_f_ini_cr_garantia           =  NULL
   EXECUTE  sql_18      USING    reg_taa_cd_det_cedido.n_seguro
                         INTO    l_f_ini_cr_garantia
   IF       l_f_ini_cr_garantia         IS  NOT  NULL   THEN
            LET      g_cred_en_garantia        = '1'
   ELSE
            EXECUTE  sql_19             USING  reg_taa_cd_det_cedido.n_seguro
            IF       SQLCA.SQLCODE          =  0  THEN
                     EXECUTE  sql_20    USING  reg_taa_cd_det_cedido.n_seguro
                     IF       SQLCA.SQLCODE       <>  cero      THEN
                              LET      g_cred_en_garantia        =  '2'
                     END IF
            END IF
   END IF
#------MARCA DE TRANSFERENCIA POR 56 AÑOS 1 CON MARCA " " SIN MARCA
   EXECUTE  sql_54        USING    reg_taa_cd_det_cedido.n_seguro
                           INTO    g_cta_transfe_56_anios
END FUNCTION

FUNCTION    F_121_arma_num_aport_viv92()
   DEFINE   l_num_aport_viv92                 SMALLINT
   LET      g_num_aport_viv92                 =  cero
   LET      l_num_aport_viv92                 =  cero
   FOREACH  c_dis_cuentas            INTO  g_tabname 
            CALL     arma_querys_TCAAB006S()
            PREPARE  sql_22          FROM  g_sql_22
            EXECUTE  sql_22         USING  reg_taa_cd_det_cedido.n_seguro 
                                     INTO  l_num_aport_viv92 
            LET      g_num_aport_viv92          =
                     g_num_aport_viv92          +  l_num_aport_viv92
   END FOREACH
END FUNCTION

FUNCTION    F_130_arma_saldo_viv()
   DEFINE   l_grupo                             SMALLINT
   DEFINE   l_acciones                          ,
            l_pesos                             DECIMAL(16,6)
   DEFINE   l_id_prefijo                        CHAR(003)
   LET      l_grupo                   =  4   
   CALL     F_132_1_inicializa_g_saldos()
   EXECUTE  sql_50_1           USING  reg_taa_cd_det_cedido.tipo_traspaso
                                INTO  g_tipo_mov , l_id_prefijo
   LET      g_id_aportante            =  l_id_prefijo     CLIPPED ,
	    reg_taa_cd_det_cedido.cve_recep_cuenta
   DECLARE  c_saldo       CURSOR     FOR  sql_42
   FOREACH  c_saldo    
            USING     reg_taa_cd_det_cedido.n_seguro,
                      cero        ,   #  subcuenta
                      l_grupo     ,
                      g_today
             INTO     g_scta      ,
                      g_siefore   ,
                      g_acciones  ,
                      g_pesos
            IF        g_scta               =   4     THEN
                      LET      g_imp_2_dec           =
                               (g_acciones           *  g_precio_parti)
                      LET      g_pesos               =  g_imp_2_dec
                      IF       g_cred_en_garantia             =  1    THEN
                               LET      l_acciones            =  cero
                               EXECUTE  sql_45   
                                        USING   reg_taa_cd_det_cedido.n_seguro,
                                                g_fecha_saldo_parti
                                         INTO   l_acciones
                       ######  descuenta saldo de credito en garantia  #######
                               IF       l_acciones            >  cero   THEN
                                        LET     g_acciones    =
                                                g_acciones    -  l_acciones
                                        LET     g_imp_2_dec   = 
                                                g_acciones    *  g_precio_parti
                                        LET     g_pesos       =  g_imp_2_dec
                               END IF 
                      END  IF 
            ELSE 
            IF        g_scta               =   8     THEN
                      LET      g_imp_2_dec           =
                              (g_acciones            *  g_precio_parti)
                      LET      g_pesos               =  g_imp_2_dec
            ELSE 
            IF        g_scta               =   14    THEN
                      IF   MONTH(reg_taa_cd_ctr_folio.fecha_envio_saldos)  <
                           MONTH(reg_taa_cd_ctr_folio.fecha_liquidacion )  THEN
                           CALL     F_131_con_interes_vivienda_fovissste()
                      END IF
            END   IF
            END   IF
            END   IF
            IF        g_pesos             >=  g_monto_minimo      THEN
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

FUNCTION    F_131_con_interes_vivienda_fovissste()
   DEFINE
            l_fecha_valor                       ,
            l_fecha_hasta                       ,
            l_fecha_desde                       ,
            l_fecha_tasa_valor                  ,
            l_mes_tasa_anterior                 DATE
   DEFINE 
            l_mes_mas_1                         SMALLINT
   DEFINE
            l_acciones                          ,
            l_pesos                             ,
            l_tasa_del_mes                      ,
            l_interes                           DECIMAL(16,6)

   LET      g_pesos                 =  cero
   EXECUTE  sql_27         USING    reg_taa_cd_det_cedido.n_seguro
                            INTO    l_fecha_desde
   IF       l_fecha_desde          IS  NULL                OR
            l_fecha_desde           <  '01/01/1997'        THEN
            LET      l_fecha_desde        =
                     MDY(MONTH(g_today),1,YEAR(g_today))
   END IF
   DECLARE  cur_interes   CURSOR   FOR   sql_28
   OPEN     cur_interes    USING   l_fecha_desde,
                                   l_fecha_desde,
                                   reg_taa_cd_det_cedido.n_seguro,
                                   reg_taa_cd_det_cedido.n_seguro
   FOREACH  cur_interes     INTO   l_fecha_valor, l_pesos
            LET      l_mes_mas_1                =  MONTH(l_fecha_valor)  +  1
            IF       l_mes_mas_1                =  13   THEN
                     LET      l_fecha_tasa_valor               =
                              MDY(1,1,YEAR(l_fecha_valor)      +   1)
            ELSE
                     LET      l_fecha_tasa_valor               = 
                              MDY(l_mes_mas_1,1,YEAR(l_fecha_valor))
            END IF
            LET      l_mes_tasa_anterior              =  l_fecha_valor
            WHILE    TRUE
                     EXECUTE  sql_29   USING    l_fecha_tasa_valor
                                        INTO    l_tasa_del_mes
                     IF       STATUS               =  NOTFOUND     THEN
                              DISPLAY "Program stopped"
                              DISPLAY "ERROR: NO HAY TASA DE INTERES FOVISSSTE"
                              EXIT  PROGRAM
                     END IF
                     LET      l_interes            =  l_pesos    *
                             (l_fecha_tasa_valor   -  l_mes_tasa_anterior)  *
                             (l_tasa_del_mes       /  36000)
                     LET      l_pesos              =  l_pesos    +  l_interes
                     IF       reg_taa_cd_ctr_folio.calculo_interes   =  0  THEN
                              LET   reg_taa_cd_ctr_folio.calculo_interes   =  1
                     END IF
                     IF       l_fecha_tasa_valor  >=  g_fecha_liq_parti  THEN
                              EXIT   WHILE
                     END IF
                     LET      l_fecha_tasa_valor   =
                              l_fecha_tasa_valor   +  1   UNITS  MONTH
                     LET      l_mes_tasa_anterior  = 
                              l_fecha_tasa_valor   -  1   UNITS  MONTH
            END      WHILE
            LET      g_pesos         =  g_pesos    +  l_pesos
   END  FOREACH
END FUNCTION

FUNCTION    F_132_1_inicializa_g_saldos()
   LET      g_siefore                  =  cero
   LET      g_tot_rcv                  =  cero
   FOR      g_scta                     =  g_scta_ini   TO  g_scta_fin
            LET      g_saldos[g_scta].acciones      =  cero
            LET      g_saldos[g_scta].pesos         =  cero
            LET      g_saldos[g_scta].porc          =  cero
   END FOR
   LET      g_scta                     =  cero
   LET      g_acciones                 =  cero
   LET      g_pesos                    =  cero
END FUNCTION

FUNCTION    F_133_arma_saldo_rcv()
   DEFINE   l_scta                         SMALLINT
   DEFINE   l_tot_porc                     ,
            l_acc_ret_92                   ,
            l_pesos                        DECIMAL(16,6)
   LET      g_tot_bloques_rcv              =  1
   CALL     F_133_1_inicializa_rcv()
   LET      g_bloques_rcv                  =  NULL
   LET      g_bloques_rcv_reg_1            =  NULL
###################  QUERYS  DE  F_133_arma_saldo_rcv()  ###############
   DECLARE  c_saldo_rcv  CURSOR  FOR  sql_42
   FOREACH  c_saldo_rcv   USING  reg_taa_cd_det_cedido.n_seguro,
                                 cero        ,  #  subcuenta
                                 cero        ,  #  grupo
                                 g_today
                           INTO  g_scta,
                                 g_siefore   ,
                                 g_acciones  ,
                                 g_pesos
            IF      (g_scta                  =  4       OR 
                     g_scta                  =  8       OR 
                     g_scta                  =  14 )    THEN
                     CONTINUE  FOREACH 
            END IF
            LET      g_pesos                 = 
                     g_acciones    *    g_sie_inf[g_siefore].precio_accion
            LET      l_pesos                 =  g_pesos
            LET      l_scta                  =  g_scta    
            IF       g_scta                  =  6       OR
                     g_scta                  =  9       THEN
                     LET      l_scta         =  2
                     EXECUTE  sql_12  
                                         USING  reg_taa_cd_det_cedido.n_seguro ,
                                                g_siefore
                                          INTO  l_acc_ret_92
                     LET      l_pesos        =  l_acc_ret_92      *
                              g_sie_inf[g_siefore].precio_accion
            END IF
            IF       l_pesos                >=  g_monto_minimo     THEN
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
                     IF       g_scta                 <>  2         THEN
                              LET    g_sumario[g_scta].pesos    =
                                     g_sumario[g_scta].pesos    +  g_imp_2_dec
                     END IF
                     LET      g_sum_rep[g_scta,g_siefore].acciones       =
                              g_sum_rep[g_scta,g_siefore].acciones + g_acciones
                     LET      g_sum_rep[g_scta,g_siefore].pesos          =
                              g_sum_rep[g_scta,g_siefore].pesos    +  g_pesos
                     IF       l_scta                 <>  2      THEN
                              LET      g_tot_rcv                =  
		   	               g_tot_rcv                +  g_imp_2_dec
                     END IF
            END IF
   END FOREACH 
   LET      g_saldos[2].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
	    LET      g_sie[2,g_siefore].pesos2d         =
	             g_sie[2,g_siefore].pesos
            LET      g_saldos[2].pesos                  =
	             g_saldos[2].pesos      +   g_sie[2,g_siefore].pesos2d
            LET      g_sumario[2].pesos              =
	             g_sumario[2].pesos     +   g_sie[2,g_siefore].pesos2d
   END FOR
   LET      g_tot_rcv       =  g_tot_rcv    +   g_saldos[2].pesos
   CALL     F_134_bloques_siefore_rcv()
#------     Calcula  Porcentajes  globales  ------
   LET      l_scta                 =  0
   LET      l_tot_porc             =  0
   FOR      g_scta                 =  g_scta_ini      TO  g_scta_fin
            IF       g_scta                     =  4      OR
                     g_scta                     =  8      OR      
                     g_scta                     =  14     THEN
                     CONTINUE  FOR
            END IF
            IF       g_saldos[g_scta].pesos     >  cero   THEN
                     LET      g_saldos[g_scta].porc    =
                              g_saldos[g_scta].pesos   /  g_tot_rcv   *  100
                     LET      l_tot_porc               =
                              l_tot_porc               +  g_saldos[g_scta].porc
                     LET      l_scta                   =  g_scta
            END IF
   END FOR
 ---------------------    AJUSTA  PORCENTAJE   ----------------
   IF       g_tot_rcv                    >  0         THEN
            IF       l_tot_porc         <>  100       THEN
                     IF       g_saldos[2].porc        >  0    THEN
                              LET      l_scta         =  2
                     END IF 
                     LET      l_tot_porc              =
                              l_tot_porc              -  g_saldos[l_scta].porc
                     LET      g_saldos[l_scta].porc   =  100  -  l_tot_porc
            END IF
   END IF
   CALL     F_137_ult_pri_apor_vol_patro()    #APORT.  VOLUNTARIAS  PATRONALES
   CALL     F_138_ult_pri_apor_vol_venta()    #APORT.  VOLUNTARIAS  VENTANILLA
END FUNCTION

FUNCTION    F_133_1_inicializa_rcv()
   LET      g_tot_rcv                 =  cero
   FOR      g_scta                    =  g_scta_ini      TO  g_scta_fin
            IF       g_scta           =  4     OR
                     g_scta           =  8     OR
                     g_scta           =  14    THEN
                     CONTINUE  FOR
            END IF
            LET      g_saldos[g_scta].acciones    =  cero
            LET      g_saldos[g_scta].pesos       =  cero
            FOR      g_siefore        =  g_siefore_ini       TO  g_siefore_fin
                     LET      g_sie[g_scta,g_siefore].acciones    =  cero
                     LET      g_sie[g_scta,g_siefore].pesos       =  cero
                     LET      g_sie[g_scta,g_siefore].pesos2d     =  cero
            END FOR
   END FOR
END FUNCTION

FUNCTION    F_134_bloques_siefore_rcv()
   FOR      g_scta          =  g_scta_ini      TO  g_scta_fin
            IF       g_scta                     =  4      OR
                     g_scta                     =  8      OR      
                     g_scta                     =  14     THEN
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
   FOR      g_scta    =  1          TO  488
            IF       g_bloques_rcv[g_scta]           =  "?"      THEN
                     LET      g_bloques_rcv[g_scta]           =  " "
            END IF
   END FOR
   IF       g_tot_bloques_rcv       >=  8        THEN
            FOR      g_scta          =  1        TO  488
                     IF       g_bloques_rcv_reg_1[g_scta]     =  "?"      THEN
                              LET      g_bloques_rcv_reg_1[g_scta]     =  " "
                     END IF
            END FOR
   END IF
   LET      g_tot_bloques_rcv       =  g_tot_bloques_rcv     -  1
END FUNCTION

FUNCTION    F_135_arma_bloques_siefore_rcv()
   DEFINE   l_porcentaje                    DEC(20,6)
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            g_sumario[g_scta].scta_proc     USING    "&&"       CLIPPED
   LET      l_porcentaje            =
            g_sie[g_scta,g_siefore].pesos2d  /  g_saldos[g_scta].pesos   *  100
   LET      l_porcentaje            =  l_porcentaje          *  1000000
   LET      g_bloques_rcv           =  g_bloques_rcv            CLIPPED,
            l_porcentaje     USING     "&&&&&&&&&"              CLIPPED
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

FUNCTION    F_137_ult_pri_apor_vol_patro()
   DEFINE   l_fecha                         DATE
   LET      g_fecha_ult_pri_pat          =  NULL
   IF       g_saldos[3].pesos            =  cero  OR
            g_saldos[3].pesos            <  g_monto_minimo  THEN
            LET      l_fecha             =  NULL
   ELSE
            EXECUTE  sql_48 
            FOREACH  c_dis_cuentas       INTO  g_tabname 
                  CALL     arma_querys_TCAAB006S()
                  PREPARE  sql_23        FROM  g_sql_23 
                  PREPARE  sql_24        FROM  g_sql_24 
                  EXECUTE  sql_23       USING  reg_taa_cd_det_cedido.n_seguro
                  EXECUTE  sql_24       USING  reg_taa_cd_det_cedido.n_seguro
            END FOREACH
            EXECUTE  sql_31   INTO  l_fecha
            IF       l_fecha                 IS  NULL   THEN
                     EXECUTE   sql_32      INTO  l_fecha
            END IF
   END IF
   IF       g_tipo_solicitud             =  2    THEN
            IF       g_finicta                   =  l_fecha     THEN
                     EXECUTE  sql_56      USING  reg_taa_cd_det_cedido.n_seguro
                                           INTO  l_fecha
            END IF
   END IF
   IF       l_fecha       IS  NOT  NULL    THEN
            LET      g_fecha_ult_pri_pat      =  l_fecha    USING  "YYYYMMDD"
   ELSE
            LET      g_fecha_ult_pri_pat      =  g_fecha_nula
   END IF
END FUNCTION

FUNCTION    F_138_ult_pri_apor_vol_venta()
   DEFINE   l_fecha                             DATE
   LET      g_fecha_ult_pri_ven              =  NULL
   IF       g_saldos[10].pesos               =  cero   OR
            g_saldos[10].pesos               <  g_monto_minimo  THEN
            LET      l_fecha                 =  NULL
   ELSE
            EXECUTE  sql_48
            FOREACH  c_dis_cuentas INTO g_tabname 
                     CALL     arma_querys_TCAAB006S()
                     PREPARE  sql_25       FROM  g_sql_25 
                     PREPARE  sql_26       FROM  g_sql_26 
                     PREPARE  sql_25_110   FROM  g_sql_25_110
                     EXECUTE  sql_25      USING  reg_taa_cd_det_cedido.n_seguro
                     EXECUTE  sql_26      USING  reg_taa_cd_det_cedido.n_seguro
                     EXECUTE  sql_25_110  USING  reg_taa_cd_det_cedido.n_seguro
            END FOREACH
            EXECUTE  sql_31                INTO   l_fecha 
            IF       l_fecha                 IS  NULL    THEN
                     EXECUTE   sql_32      INTO   l_fecha 
            END IF
            IF       l_fecha                 IS  NULL     THEN 
                     EXECUTE   sql_32_110  INTO  l_fecha
            END IF
   END IF
   IF       g_tipo_solicitud       =  2     THEN
            IF       g_finicta                 =  l_fecha     THEN
                     EXECUTE   sql_57      USING  reg_taa_cd_det_cedido.n_seguro
                                            INTO  l_fecha
            END IF
   END IF
   IF       l_fecha          IS  NOT  NULL  THEN
            LET      g_fecha_ult_pri_ven       =  l_fecha   USING  "YYYYMMDD"
   ELSE 
            LET      g_fecha_ult_pri_ven       =  g_fecha_nula
   END IF
END FUNCTION

FUNCTION    F_140_inserta_dis_provision()
   DEFINE   verifica_provision                 ,
            v_folio_sua                        SMALLINT
   DEFINE   l_acciones                         ,
            l_pesos                            DEC(16,6) 
   LET      v_folio_sua                  =  9999
   LET      l_acciones                   =  g_acciones      *   -1
   LET      l_pesos                      =  g_pesos         *   -1
   LET      g_si_hay_comple              =  1
   DECLARE  spl_provisiona     CURSOR  FOR  sql_43
   FOREACH  spl_provisiona           USING
            reg_taa_cd_ctr_folio.folio             ,  #folio
            v_folio_sua                            ,  #folio_sua
            reg_taa_cd_det_cedido.n_seguro         ,  #nss
            g_scta                                 ,  #subcuenta
            g_tipo_mov                             ,  #tipo_movimiento
            reg_taa_cd_det_cedido.cont_servicio    ,  #consecutivo
            g_siefore                              ,  #siefore
            l_acciones                             ,  #monto_en_acciones  
            l_pesos                                ,  #monto_en_pesos
            g_id_aportante                         ,  #id_aportante
            g_today                                   #fecha_proceso
                                     INTO     verifica_provision
   END FOREACH
END FUNCTION

FUNCTION    F_145_insert_tmp_envio_2dec()
   FOR      g_scta         =  g_scta_ini         TO  g_scta_fin
            IF       g_saldos[g_scta].pesos       >  cero  THEN
                     EXECUTE   sql_34
                       USING   reg_taa_cd_ctr_folio.folio,
                               reg_taa_cd_det_cedido.n_seguro,
                               g_scta,
                               g_saldos[g_scta].pesos,
                               g_provisionado
            END IF
   END FOR
END FUNCTION

REPORT      R_150_arma_detalles()
   DEFINE   g_fecha_nula_ven                DATE
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
     IF      (reg_taa_cd_det_cedido.id_regs_reenviados    <>  1  AND
              reg_taa_cd_det_cedido.id_regs_reenviados    <>  2)     THEN
              LET      reg_taa_cd_det_cedido.id_regs_reenviados     =  " "
     END  IF
     PRINT    COLUMN   01                                ,
             "02"                                        ,           #id_1
              g_num_registro      USING  "&&&&&&&&&&"    ,           #id_2
              reg_taa_cd_det_cedido.tipo_recep_cuenta    ,           #id_3
              reg_taa_cd_det_cedido.cve_recep_cuenta     ,           #id_4
              reg_taa_cd_det_cedido.tipo_ced_cuenta      ,           #id_5
              reg_taa_cd_det_cedido.cve_ced_cuenta       ,           #id_6
              reg_taa_cd_det_cedido.tipo_traspaso        ,           #id_7
              reg_taa_cd_ctr_folio.fecha_envio_saldos USING "YYYYMMDD", #id_8
              8       SPACES                             ,           #id_9 _viv
              reg_taa_cd_det_cedido.n_unico              ,           #id_10_viv
              reg_taa_cd_det_cedido.n_seguro             ,           #id_11_viv
              g_nss_unificador                           ,           #id_12_viv
              04      SPACES                             ,           #id_13_viv
              reg_taa_cd_det_cedido.rfc                  ,           #id_14_viv
              reg_taa_cd_det_cedido.paterno              ,           #id_15_viv
              reg_taa_cd_det_cedido.materno              ,           #id_16_viv
              reg_taa_cd_det_cedido.nombre               ,           #id_17_viv
              3       SPACES                             ,           #id_18_viv
              reg_taa_cd_det_cedido.cve_sector           ,           #id_19_viv
              10      SPACES                             ,           #id_20_viv
              reg_taa_cd_det_cedido.fecha_recep_solic  USING "YYYYMMDD" ,#id_21
              reg_taa_cd_det_cedido.ident_lote_solici    ,           #id_22_viv
              15      SPACES                             ,           #id_23_viv
              reg_taa_cd_det_cedido.n_seguro_cedente     ,           #id_24_viv
              reg_taa_cd_det_cedido.rfc_cedente          ,           #id_25_viv
              30      SPACES                             ,           #id_26_viv
              reg_taa_cd_det_cedido.paterno_cedente      ,           #id_27_viv
              reg_taa_cd_det_cedido.materno_cedente      ,           #id_28_viv
              reg_taa_cd_det_cedido.nombre_cedente       ,           #id_29_viv
              30      SPACES                             ,           #id_30_viv
              g_saldos[4].acciones * 1000000 USING "&&&&&&&&&&&&&&&",#id_31
              g_saldos[4].pesos    * 100     USING "&&&&&&&&&&&&&&&",#id_32
              30      SPACES                         ,               #id_33
              g_saldos[14].pesos   * 100     USING "&&&&&&&&&&&&&&&",#id_34
              g_saldos[8].acciones * 1000000 USING "&&&&&&&&&&&&&&&",#id_35
              g_saldos[8].pesos    * 100     USING "&&&&&&&&&&&&&&&",#id_36
              03      SPACES                          ,              #id_37_viv
              02      SPACES                          ,              #id_38_viv
              15      SPACES                          ,              #id_39_viv
              98      SPACES                          ,              #id_40_viv
              g_num_aport_viv92        USING  "&&&&&" ,              #id_41_viv
              2       SPACES                          ,              #id_42_viv
              "00000"                  USING  "&&&&&" ,              #id_43_viv
              10      spaces                          ,              #id_44_viv
              g_num_aport_viv_issste    USING  "&&&&&"    ,          #id_45_viv
              6       spaces                                         #id_46_viv
     LET      g_num_registro          =  g_num_registro  +  1
     IF       g_tot_bloques_rcv       <  8   THEN
              LET      g_bloques_rcv_reg_1    =  g_bloques_rcv   CLIPPED
     END IF
              #####    registro  1 de  detalle_05    rcv     ############
     PRINT    COLUMN   01,
              "05",                                                   #id_01_rcv
              g_num_registro          USING  "&&&&&&&&&&",           #id_02_rcv
              reg_taa_cd_det_cedido.tipo_recep_cuenta    ,           #id_03_rcv
              reg_taa_cd_det_cedido.cve_recep_cuenta     ,           #id_04_rcv
              reg_taa_cd_det_cedido.tipo_ced_cuenta      ,           #id_05_rcv
              reg_taa_cd_det_cedido.cve_ced_cuenta       ,           #id_06_rcv
              reg_taa_cd_det_cedido.tipo_traspaso        ,           #id_07_rcv
              g_fecha_ult_pri_pat                        ,           #id_08
              g_fecha_ult_pri_ven                        ,           #id_09
              reg_taa_cd_det_cedido.n_unico              ,           #id_10_rcv
              reg_taa_cd_det_cedido.n_seguro             ,           #id_11_rcv
              g_saldos[03].porc    *  1000000    USING  "&&&&&&&&&", #id_12_rcv
              g_saldos[10].porc    *  1000000    USING  "&&&&&&&&&", #id_13_rcv
              g_saldos[01].porc    *  1000000    USING  "&&&&&&&&&", #id_14_rcv
              g_saldos[02].porc    *  1000000    USING  "&&&&&&&&&", #id_15_rcv
              g_saldos[05].porc    *  1000000    USING  "&&&&&&&&&", #id_16_rcv
              g_saldos[07].porc    *  1000000    USING  "&&&&&&&&&", #id_17_rcv
              g_today                            USING  "YYYYMMDD",  #id_18_rcv
              g_saldos[13].porc    *  1000000    USING  "&&&&&&&&&", #id_19
              g_saldos[12].porc    *  1000000    USING  "&&&&&&&&&", #id_20
              g_saldos[11].porc    *  1000000    USING  "&&&&&&&&&", #id_21
              g_cta_transfe_56_anios                               , #id_22
              33       SPACES,                                       #id_22_rcv
              g_bloques_rcv_reg_1,                                #id_23_70_rcv
              g_cod_result_operac,                                   #id_71_rcv
              g_diag_proceso,                                        #id_72_rcv
              33       SPACES                                        #id_73_rcv
  #########   arma registro detalle_05 de los bloques 9 en adelante ##########
     IF       g_tot_bloques_rcv           >   8   THEN
              LET       g_num_registro       =  g_num_registro       +  1
     PRINT    COLUMN  01,
              "05",                                                   #id_01_rcv
              g_num_registro          USING  "&&&&&&&&&&",            #id_02_rcv
              reg_taa_cd_det_cedido.tipo_recep_cuenta    ,            #id_03_rcv
              reg_taa_cd_det_cedido.cve_recep_cuenta     ,            #id_04_rcv
              reg_taa_cd_det_cedido.tipo_ced_cuenta      ,            #id_05_rcv
              reg_taa_cd_det_cedido.cve_ced_cuenta       ,            #id_06_rcv
              reg_taa_cd_det_cedido.tipo_traspaso        ,            #id_07_rcv
              g_fecha_ult_pri_pat                        ,            #id_08
              g_fecha_ult_pri_ven                        ,            #id_09
              reg_taa_cd_det_cedido.n_unico              ,            #id_10_rcv
              reg_taa_cd_det_cedido.n_seguro             ,            #id_11_rcv
              g_saldos[03].porc    *  1000000    USING  "&&&&&&&&&",  #id_12_rcv
              g_saldos[10].porc    *  1000000    USING  "&&&&&&&&&",  #id_13_rcv
              g_saldos[01].porc    *  1000000    USING  "&&&&&&&&&",  #id_14_rcv
              g_saldos[02].porc    *  1000000    USING  "&&&&&&&&&",  #id_15_rcv
              g_saldos[05].porc    *  1000000    USING  "&&&&&&&&&",  #id_16_rcv
              g_saldos[07].porc    *  1000000    USING  "&&&&&&&&&",  #id_17_rcv
              g_today                            USING  "YYYYMMDD",   #id_18_rcv
              g_saldos[13].porc    *  1000000    USING  "&&&&&&&&&",  #id_19
              g_saldos[12].porc    *  1000000    USING  "&&&&&&&&&",  #id_20
              g_saldos[11].porc    *  1000000    USING  "&&&&&&&&&",  #id_21
              g_cta_transfe_56_anios                               ,  #id_22
              33      SPACES,                                         #id_22_rcv
              g_bloques_rcv      ,                                    #id_23_70
              g_cod_result_operac,                                    #id_71
              g_diag_proceso,                                         #id_72
              33      SPACES                                          #id_73
     END IF
END REPORT

FUNCTION    F_200_fin()
   DEFINE   l_ruta                       CHAR(300)
   CALL     F_210_arma_encabezado()
   CALL     F_220_arma_saldos_sumario()
   CALL     F_240_genera_reporte_saldos()
   LET      l_ruta         =  g_seg_modulo.ruta_envio     CLIPPED,"/",
                             "TRASP_SAL_COMP.",g_today    USING   "MMDD","-",
                              g_hora[1,2],g_hora[4,5]
   LET      g_current       =  CURRENT
   DISPLAY g_current," :RUTA Y NOMBRE DEL ARCHIVO COMPLE. : "
   DISPLAY  l_ruta CLIPPED 
   CALL     F_250_genera_archivo_saldos()   #gst
   IF       reg_taa_cd_ctr_folio.calculo_interes     =  1    THEN
            EXECUTE  sql_39     USING  reg_taa_cd_ctr_folio.folio
   END IF
   LET      g_comando             =  "lp ", g_comando
   RUN      g_comando
   LET      g_comando             =
           "echo \'chmod  777 ",g_seg_modulo.ruta_envio CLIPPED,
           "/*TT_v3*", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN      g_comando
   LET      g_comando             = "rm  arch_paso "
   RUN      g_comando
   LET      g_comando             = "rm  err_paso "
   RUN      g_comando
   LET      g_current             =  CURRENT
   DISPLAY  g_current," :FIN DE PROGRAMA       : TCAAB006"
END FUNCTION

FUNCTION    F_210_arma_encabezado()
   LET      g_cza            =  g_seg_modulo.ruta_envio CLIPPED,"/CTT_v3"
   START    REPORT     R_211_arch_encabezado        TO  g_cza
            OUTPUT     TO  REPORT    R_211_arch_encabezado()
   FINISH   REPORT     R_211_arch_encabezado
END FUNCTION

REPORT      R_211_arch_encabezado()
    OUTPUT
            PAGE     LENGTH  1
  	    LEFT     MARGIN  0
	    RIGHT    MARGIN  0
	    TOP      MARGIN  0
	    BOTTOM   MARGIN  0
    FORMAT
    ON EVERY ROW 
       ###########     registro  de  encabezado  del  archivo      ############
      PRINT    COLUMN   01,
               "01"                        ,  #id_01
               "02"                        ,  #id_02
               "12"                        ,  #id_03
               "01"                        ,  #id_04
               g_afore_cod      USING "&&&",  #id_05
               "01"                        ,  #id_06
               "   "                       ,  #id_07
               "009"                       ,  #id_08
               reg_taa_cd_ctr_folio.fecha_envio_saldos  USING "YYYYMMDD", #id_09
               "001"                       ,  #id_10
               "02"                        ,  #id_11
               "  "                        ,  #id_12
               "         "                 ,  #id_13
               687     SPACES                            
END REPORT

FUNCTION   F_220_arma_saldos_sumario()
   LET     g_sum_rcv               =   NULL
   FOR     g_scta                  =  g_scta_ini         TO  g_scta_fin
           IF       g_scta                    =  6       OR
                    g_scta                    =  9       THEN
                    CONTINUE  FOR
           END IF
           IF       g_sumario[g_scta].pesos        <>  cero     THEN
                    EXECUTE  sql_16    USING    reg_taa_cd_ctr_folio.folio,
                                                g_scta,
                                                g_sumario[g_scta].pesos,
                                                g_num_registro
           END IF
           IF       g_scta                    =  4     OR
                    g_scta                    =  8     OR
                    g_scta                    =  14    THEN
                    CONTINUE   FOR
           END IF
           IF       g_sumario[g_scta].pesos        <>  cero     THEN
                    LET      g_sum_rcv                =  g_sum_rcv CLIPPED,
                             g_sumario[g_scta].scta_proc  USING  "&&"  CLIPPED
                    LET      g_sum_rcv                =  g_sum_rcv CLIPPED,
                             g_sumario[g_scta].pesos  *  100
                             USING    "&&&&&&&&&&&&&&&"   CLIPPED
           END IF
   END FOR
   LET      g_sum             =  g_seg_modulo.ruta_envio CLIPPED,"/STT_v3" 
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
           "09"                                         , #id_01_sum
            g_num_registro            USING  "&&&&&&&&&", #id_02_sum
            30       SPACES                             , #id_03_sum
            g_sumario[4].acciones * 1000000  
                     USING  "&&&&&&&&&&&&&&&&&&"        , #id_04_sum
            g_sumario[4].pesos * 100 
                     USING  "&&&&&&&&&&&&&&&"           , #id_05_sum
            30       SPACES                             , #id_06_sum
            g_sumario[14].pesos * 100
                     USING  "&&&&&&&&&&&&&&&"           , #id_07_sum
            g_sumario[8].acciones * 1000000  
                     USING  "&&&&&&&&&&&&&&&&&&"        , #id_08_sum
            g_sumario[8].pesos * 100
                     USING  "&&&&&&&&&&&&&&&"           , #id_09_sum
            240      SPACES                             , #id_10_sum
            g_sum_rcv                                   , #id_11_al_26_sum
            185      SPACES                                #id_27_sum
END REPORT

FUNCTION    F_250_genera_archivo_saldos()
   LET      g_cat     =  "cat ",g_seg_modulo.ruta_envio  CLIPPED,"/CTT_v3 ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/DTT_v3 ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/STT_v3 > ",
                          g_seg_modulo.ruta_envio        CLIPPED,"/",
                         "TRASP_SAL_COMP.",g_today       USING "MMDD","-",
                          g_hora[1,2],g_hora[4,5]
   RUN      g_cat
END FUNCTION 

FUNCTION    F_240_genera_reporte_saldos()
   LET      g_comando         =  g_seg_modulo.ruta_listados CLIPPED, "/",
            g_usuario    CLIPPED ,".REP_COMP.",g_today  USING  "DDMMYY","-",
            g_hora[1,2],g_hora[4,5]  CLIPPED
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
        PRINT    COLUMN   02,'\033(s7B',"FOLIO   : ",reg_taa_cd_ctr_folio.folio
                 USING "########",'\033(s0B' CLIPPED
        PRINT    COLUMN   02,"ARCHIVO : ",'\033(s7B',"TRASP_SAL_COMP.",
                          g_today     USING  "MMDD","-",
                          g_hora[1,2],g_hora[4,5],'\033(s0B'    CLIPPED
        PRINT    COLUMN   02,"RUTA    : /safre_prc/taa/envio"
        PRINT    COLUMN   02,"FECHA DE PRESENTACION : ",g_today
                          USING     "DD/MM/YYYY"
   ON EVERY ROW
	PRINT    COLUMN   02,"_______________________________________________",
                             "_________________________________"
        PRINT    COLUMN   05, '\033(s7B',"TOTALES DE VIVIENDA  VALOR",
                            " PARTICIPACION : ", g_precio_parti  
                            USING    "##.##############",'\033(s0B' CLIPPED
        PRINT    COLUMN   30, "MONTO EN PARTICIPACIONES",
                 COLUMN   65, "MONTO EN PESOS"
	PRINT    COLUMN   02,"________________________________________________",
                           "________________________________"
        PRINT    COLUMN   01,g_sumario[04].nom_scta,
                 COLUMN   35,g_sum_rep[04,1].acciones
                          USING    "####,###,###.######",
                 COLUMN   60,g_sum_rep[04,1].pesos  
                          USING    "####,###,###.######"
        PRINT    COLUMN   01,g_sumario[08].nom_scta,
                 COLUMN   35,g_sum_rep[08,1].acciones
                          USING    "####,###,###.######",
                 COLUMN   60,g_sum_rep[08,1].pesos 
                          USING    "####,###,###.######"
        PRINT    COLUMN   01,g_sumario[14].nom_scta,
                 COLUMN   60,g_sum_rep[14,1].pesos 
                          USING    "####,###,###.######"
        FOR      g_siefore      =  g_siefore_ini       TO  g_siefore_fin
                 FOR      g_scta                 =  g_scta_ini   TO  g_scta_fin
                          IF       g_scta               =  4     OR
                                   g_scta               =  8     OR
                                   g_scta               =  14    THEN
                                   CONTINUE  FOR
                          END IF
                          IF       g_scta               =  1     THEN
	                           PRINT   COLUMN  02,
                                           "______________________________",
                                           "______________________________",
                                           "____________________"
                                   PRINT   COLUMN  05, '\033(s7B',
                                           "TOTALES SALDOS DE RCV SIEFORE",
                                           " BASICA ",g_siefore  USING "#"," ",
                                           "VALOR ACCION :",
                                            g_sie_inf[g_siefore].precio_accion,
                                           '\033(s0B' CLIPPED
                                   PRINT   COLUMN  37,"MONTO EN ACCIONES",
                                           COLUMN  65,"MONTO EN PESOS"
	                           PRINT   COLUMN  02,
                                           "______________________________",
                                           "______________________________",
                                           "____________________"
                          END IF
                          IF       g_siefore           =  3        THEN
                                   IF      g_scta           <>  3        AND
                                           g_scta           <>  10       AND
                                           g_scta           <>  11       AND
                                           g_scta           <>  12       AND
                                           g_scta           <>  13       AND
                                           g_scta           <>  15       THEN
                                           CONTINUE  FOR
                                   END IF
                          END IF
                          PRINT   COLUMN   1,g_sumario[g_scta].nom_scta," ",
                                  COLUMN 35,g_sum_rep[g_scta,g_siefore].acciones
                                             USING    "####,###,###.######",
                                  COLUMN   60,g_sum_rep[g_scta,g_siefore].pesos
                                            USING "####,###,###.######"
                 END FOR
        END FOR
	PRINT  COLUMN 02,"___________________________________________________",
                         "_____________________________"
        SKIP     TO    TOP   OF  PAGE
        SKIP     2     LINE
        PRINT  COLUMN  10,'\033(s7B',"TOTALES DEL ARCHIVO DE SALDOS",
               COLUMN  61,"MONTO EN PESOS", '\033(s0B' CLIPPED
	PRINT  COLUMN  02,"__________________________________________________",
                          "______________________________"
       SKIP     2     LINE
        PRINT  COLUMN  01,g_sumario[04].nom_scta,
               COLUMN  60,g_sumario[04].pesos       USING  "####,###,###.##"
        PRINT  COLUMN  01,g_sumario[08].nom_scta,
               COLUMN  60,g_sumario[08].pesos       USING  "####,###,###.##"
        PRINT  COLUMN  01,g_sumario[14].nom_scta,
               COLUMN  60,g_sumario[14].pesos       USING  "####,###,###.##"
        FOR      g_scta          =  g_scta_ini          TO  g_scta_fin
                 IF       g_scta               =  4     OR
                          g_scta               =  8     OR
                          g_scta               =  6     OR
                          g_scta               =  9     OR
                          g_scta               =  14    THEN
                          CONTINUE  FOR
                 END IF
                 PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                          COLUMN   60,g_sumario[g_scta].pesos 
                          USING    "####,###,###.##"
        END FOR
        PRINT    COLUMN   02,"________________________________________________",
                             "________________________________"
END REPORT
