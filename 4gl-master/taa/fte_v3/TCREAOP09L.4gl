###########################################################################
#Proyecto          => AFORE ( MEXICO )                                    #
#Propietario       => E.F.P.                                              #
#Programa TCREAOP09L=> GENERA ARCHIVO DE TRASPASO NORMAL-INTERNET(CEDENTE) #
#Sistema           => TCAA                                                #
#Fecha Creacion    => 23/06/2004                                          #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                      #
#Version    del    => 05/Abr/2011  (CURP LAYOUT RECORTADO)                #
###########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE 
            g_tipo_mov                          ,
            g_tipo_solicitud                    ,
            g_tot_bloques_rcv                   SMALLINT
   DEFINE
            g_aport_viv92                       ,
            g_aport_viv_issste                  ,
            g_aport_fovissste                   ,
            g_dias_cuot_soc                     ,
            g_num_registro                      INTEGER
   DEFINE
            g_acciones                          DEC(18,6),
            g_pesos                             DEC(16,6),
            g_imp_2_dec                         ,
            g_monto_minimo                      DEC(15,2)

   DEFINE
            g_vol_patronal                      ,            # id_08
            g_vol_ventanilla                    ,            # id_09
            g_aport_l_plazo                     ,            # id_10
            g_perspect_l_plazo                  ,            # id_11
            g_id_credito_fovi                   CHAR(001),
            g_id_credito_infonavit              CHAR(001),
            g_diag_proceso                      CHAR(015),
            g_id_aportante                      CHAR(011),
            g_id_prefijo                        CHAR(007),
            g_det                               CHAR(100),
            g_cza                               CHAR(100),
            g_sum                               CHAR(100),
            g_hora                              CHAR(005), 
            g_cat                               CHAR(300),
            g_bloques_rcv                       CHAR(416),
            g_bloques_rcv_reg_1                 CHAR(416),
            g_sum_rcv                           CHAR(476),
            g_fecha_nula                        CHAR(008),
            g_n_unico                           CHAR(018)

   DEFINE   g_sie           ARRAY[50,50]   OF    RECORD --subcuenta,siefore
            acciones                            DEC(16,6),
            pesos                               DEC(16,6),
            pesos2d                             DEC(16,2)
                                          END   RECORD
   DEFINE   g_saldos        ARRAY[50]     OF    RECORD --subcuenta
            acciones                            DEC(16,6),
            pesos                               DEC(16,2)
                                          END   RECORD
   DEFINE   g_sumario       ARRAY[50]     OF    RECORD   -- subcuenta
            nom_scta                            CHAR(30),
            scta_proc                           CHAR(02),
            pesos                               DEC(15,2),
            acciones                            DEC(16,6)
                                          END   RECORD
   DEFINE   g_sum_rep       ARRAY[50,50]   OF    RECORD  ---subcuenta,siefore
            pesos                               DEC(16,6),
            acciones                            DEC(16,6)
                                          END   RECORD

   DEFINE   reg_solicitud  RECORD  LIKE    safre_af:taa_cd_det_cedido.*
END GLOBALS

GLOBALS   "TCREAOP09S.4gl"    ###  Programa  para  la  Definicion  de  Querys

MAIN
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     F_010_inicio()
   CALL     F_100_proceso()
   CALL     F_200_fin()
END MAIN
 
FUNCTION    F_010_inicio()
   CALL     STARTLOG("TCREAOP09L.log")
   LET      reg_ctr_folio.folio               =  ARG_VAL(1)
   SELECT   *
     INTO   reg_ctr_folio.*
     FROM   taa_cd_ctr_folio
   WHERE    folio                =  reg_ctr_folio.folio
   CALL     F_910_arma_querys_TCREAOP09S()
   CALL     F_012_prepara_querys_TCREAOP09L()
   LET      g_hora                     =  TIME
   LET      g_num_registro             =  cero
   LET      INT_FLAG                   =  TRUE
   DISPLAY  " "
   DISPLAY  ".1"
   DISPLAY  g_current,": TCREAOP09L: "
   DISPLAY  g_current,": TRASPASO AFORE-AFORE  CEDENTE  OPERACIÓN  09 "
   CALL     F_920_trae_parametros()
   CALL     F_020_trae_parametros()
   CALL     F_930_arma_precios_acc_parti()
   CALL     F_030_despliega_folio()

END FUNCTION

FUNCTION    F_012_prepara_querys_TCREAOP09L()
   PREPARE  sql_02          FROM     g_sql_02
   PREPARE  sql_06          FROM     g_sql_06
   PREPARE  sql_07          FROM     g_sql_07
   PREPARE  sql_07_2        FROM     g_sql_07_2
   PREPARE  sql_08          FROM     g_sql_08
   PREPARE  sql_09          FROM     g_sql_09
   PREPARE  sql_11          FROM     g_sql_11
   EXECUTE  sql_11
   PREPARE  sql_11_110      FROM     g_sql_11_110
   EXECUTE  sql_11_110
   PREPARE  sql_40          FROM     g_sql_40
   EXECUTE  sql_40
   PREPARE  sql_35          FROM     g_sql_35
   PREPARE  sql_36          FROM     g_sql_36
   PREPARE  sql_38          FROM     g_sql_38
   PREPARE  sql_38_1        FROM     g_sql_38_1
   PREPARE  sql_12          FROM     g_sql_12
   PREPARE  sql_12_2        FROM     g_sql_12_2
   PREPARE  sql_14          FROM     g_sql_14
   PREPARE  sql_15          FROM     g_sql_15
   PREPARE  sql_16          FROM     g_sql_16
   PREPARE  sql_18          FROM     g_sql_18
   PREPARE  sql_19          FROM     g_sql_19
   PREPARE  sql_21          FROM     g_sql_21
   PREPARE  sql_41          FROM     g_sql_41
   DECLARE  c_dis_cuentas   CURSOR      FOR     sql_41 
   PREPARE  sql_42          FROM     g_sql_42   # nuevo para saldos
   PREPARE  sql_43          FROM     g_sql_43   # nuevo para provision
   PREPARE  sql_45          FROM     g_sql_45   # monto de credito en garantia
   PREPARE  sql_48          FROM     g_sql_48   # borra taa_cd_fechas_vol
   PREPARE  sql_49          FROM     g_sql_49   # marca rechazos por conviv.
   PREPARE  sql_50          FROM     g_sql_50   # marca rechazos por conviv.
   PREPARE  sql_50_1        FROM     g_sql_50_1 # extrae  id_aportante
   PREPARE  sql_51          FROM     g_sql_51   # marca rechazos por conviv.
   PREPARE  sql_52          FROM     g_sql_52   # marca rechazos por conviv.
   PREPARE  sql_53          FROM     g_sql_53   # marca rechazos por conviv.
   PREPARE  sql_31          FROM     g_sql_31
   PREPARE  sql_32          FROM     g_sql_32
   PREPARE  sql_32_110      FROM     g_sql_32_110
   PREPARE  sql_34          FROM     g_sql_34
   PREPARE  sql_39          FROM     g_sql_39
   PREPARE  sql_55          FROM     g_sql_55   # calcula  consec_lote_dia
   PREPARE  sql_56          FROM     g_sql_56 
   PREPARE  sql_57          FROM     g_sql_57
   PREPARE  sql_62          FROM     g_sql_62
   EXECUTE  sql_36          INTO     g_scta_fin
END FUNCTION

FUNCTION    F_020_trae_parametros()
   DEFINE   l_tab_subcuenta      RECORD  LIKE safre_af:tab_subcuenta.*
   LET      g_fecha_nula               = "00010101"
   EXECUTE  sql_08           INTO   g_monto_minimo  
   DECLARE  c_scta         CURSOR   FOR  sql_35
   FOREACH  c_scta           INTO   l_tab_subcuenta.*
            LET      g_scta                               =
                     l_tab_subcuenta.subct_cod
            LET      g_sumario[g_scta].scta_proc          =
                     l_tab_subcuenta.subct_prc
            LET      g_sumario[g_scta].nom_scta           =
                     l_tab_subcuenta.subct_desc
   END FOREACH
   LET      g_det              =  g_seg_modulo.ruta_envio    CLIPPED,"/DET_09"
END FUNCTION

FUNCTION    F_030_despliega_folio()
   DEFINE   l_cad_desc_rech                   CHAR(100),
            l_tipo_traspaso                   CHAR(20),
            l_registros                       INTEGER
   LET      reg_ctr_folio.calculo_interes          =  1
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :FOLIO A PROCESAR                  : ",
            reg_ctr_folio.folio      USING "######"
   IF       reg_ctr_folio.tipo_traspaso       =  1    THEN
            LET     l_tipo_traspaso        =  "PROMOTOR"
   ELSE
   IF       reg_ctr_folio.tipo_traspaso       =  3    THEN
            LET     l_tipo_traspaso        =  "INTERNET"
   ELSE
   IF       reg_ctr_folio.tipo_traspaso       =  4    THEN
            LET     l_tipo_traspaso        =  "DIVERSOS"
   END IF
   END IF
   END IF
   DISPLAY  g_current," :TIPO DE TRASPASO                  : ",
            l_tipo_traspaso
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
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 5        : ",
            g_sie_inf[5].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE ACCION SIEFORE 6        : ",
            g_sie_inf[6].precio_accion    USING  "###.######"
   DISPLAY  g_current," :PRECIO DE PARTICIPACION VIVIENDA  : ",
            g_sie_inf[11].precio_accion    USING "###.##############"
   DISPLAY  g_current," :PRECIO DE AIVS VIVIENDA FOVISSSTE : ",
            g_sie_inf[12].precio_accion    USING "###.##############"
   DISPLAY  g_current," :PRECIO DE UDIS BONO PENSION ISSSTE: ",
            g_sie_inf[13].precio_accion    USING "###.##############"
   EXECUTE  sql_06     USING  reg_ctr_folio.folio,g_recibido
                        INTO  l_registros
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :NUMERO DE REGISTROS A PROCESAR    : ",
                     l_registros   USING  "######"
END FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_regs_proc                         ,
            l_leidos                            INTEGER
   LET      l_leidos                       =  cero
   LET      l_regs_proc                    =  cero
   LET      g_tipo_mov     =  290
   LET      g_id_prefijo   =  'TCREA-'
   CALL     F_110_inicializa_sumarios()
   START    REPORT R_150_arma_detalles    TO  g_det
   LET      g_current              =  CURRENT
   DISPLAY  g_current," :PROCESANDO  INFORMACION ...... "
   DECLARE  cur_nss_ced         CURSOR   FOR   sql_12
   OPEN     cur_nss_ced          USING   reg_ctr_folio.folio,g_recibido
   FOREACH  cur_nss_ced           INTO   reg_solicitud.*
            CALL     F_120_arma_datos_generales()
            CALL     F_133_arma_saldo_rcv()
                     ######    Arma  Registros  de  Detalle   #########
            OUTPUT   TO   REPORT     R_150_arma_detalles()
            LET      l_leidos             =  l_leidos       +  1
            IF       l_leidos                 =  5000    THEN
                     LET      l_regs_proc     =  l_regs_proc   +  5000
                     LET      l_leidos        =  cero
                     LET      g_current       =  CURRENT
                     DISPLAY  g_current," :REGISTROS PROCESADOS     :",
                              l_regs_proc USING  "#,###,###"
            END IF
   END FOREACH
   LET      l_regs_proc             =  l_regs_proc     +  l_leidos
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :TOTAL DE REGISTROS PROCESADOS     : " ,
            l_regs_proc         USING  "#,###,###"

   FINISH  REPORT   R_150_arma_detalles
END FUNCTION

FUNCTION    F_110_inicializa_sumarios()
   FOR      g_scta                  =  g_scta_ini    TO  g_scta_fin
            LET      g_sumario[g_scta].pesos          =  cero
            LET      g_sumario[g_scta].acciones       =  cero
            FOR      g_siefore         =  g_siefore_ini    TO  g_siefore_fin
                     LET      g_sum_rep[g_scta,g_siefore].pesos      =  cero
                     LET      g_sum_rep[g_scta,g_siefore].acciones   =  cero
            END FOR
   END FOR
END FUNCTION

FUNCTION    F_120_arma_datos_generales()
   #--- TOTAL DIAS COTIZADOS DE CUOTA SOCIAL
   EXECUTE  sql_21         USING   reg_solicitud.n_seguro
                            INTO   g_dias_cuot_soc
   IF       g_dias_cuot_soc              IS  NULL  THEN
            LET      g_dias_cuot_soc           =  cero
   END IF
 
   LET      g_id_aportante     =   g_id_prefijo    CLIPPED ,
	    reg_solicitud.ident_lote_solici[3,5]
END FUNCTION

FUNCTION    F_121_arma_num_aport_viv92()
   DEFINE   l_num_aport_viv92                SMALLINT
   LET      l_num_aport_viv92            =  cero
   FOREACH  c_dis_cuentas             INTO   g_tabname 
            CALL     F_910_arma_querys_TCREAOP09S()
            PREPARE  sql_22           FROM   g_sql_22
            EXECUTE  sql_22          USING   reg_solicitud.n_seguro 
                                      INTO   l_num_aport_viv92 
            LET      g_aport_viv92          =
                     g_aport_viv92          +  l_num_aport_viv92
   END FOREACH
END FUNCTION

FUNCTION    F_130_arma_saldo_viv()
END FUNCTION

FUNCTION    F_131_resta_credito_en_garantia()
   DEFINE   l_acciones                          ,
            l_pesos                             DECIMAL(16,6)
   LET      l_acciones                  =  cero
   EXECUTE  sql_45   
            USING   reg_solicitud.n_seguro,
                    g_fecha_saldo_parti,
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

FUNCTION    F_133_arma_saldo_rcv()
----------------------------------

   DEFINE   l_veces      SMALLINT     -- siempre = 1 solo 2 cuando scta 31
   DEFINE   l_acc_36     DECIMAL(16,6)
   DEFINE   l_p_acc_36   DECIMAL(16,6)
   DEFINE   l_acc_37     DECIMAL(16,6)
   DEFINE   l_p_acc_37   DECIMAL(16,6)
   DEFINE   l_pesos_36   DECIMAL(16,6)
   DEFINE   l_pesos_37   DECIMAL(16,6)
   DEFINE   l_dif        DECIMAL(16,6)
   DEFINE   l_id_scta_31 SMALLINT
   DEFINE   l_i          SMALLINT,
            ind_quebranto SMALLINT,
            txt_queb    CHAR(200),
            tm_queb     SMALLINT

   DEFINE   l_scta                         ,
            l_sin_esep                     SMALLINT
   DEFINE   l_acc_ret_92                   ,
            l_pesos                        DECIMAL(16,6)
   LET      g_tot_bloques_rcv              =  1
   LET      l_sin_esep                     =  0
   CALL     F_132_1_inicializa()
   LET      g_bloques_rcv                  =  NULL
   LET      g_bloques_rcv_reg_1            =  NULL
###################  QUERYS  DE  F_133_arma_saldo_rcv()  ###############
   DECLARE  c_saldo_rcv  CURSOR  FOR  sql_42
   FOREACH  c_saldo_rcv   USING  reg_solicitud.n_seguro
                           INTO  g_scta,
                                 g_siefore   ,
                                 g_acciones  ,
                                 g_pesos

            IF       g_scta            =  4     OR
                     g_scta            =  8     OR   
                     g_scta            =  35    THEN
                     CALL      F_132_1_arma_viv()
            END IF
            LET      l_scta                  =  0

            EXECUTE  sql_02
                     USING   g_scta
                     INTO    l_scta     ##    checa  subcuenta  agrupada

            IF       l_scta                  >  0    THEN

                     EXECUTE   sql_09
                       USING   reg_solicitud.n_seguro ,
                               g_today                        ,
                               l_scta,
                               g_siefore
                     INTO      l_acc_ret_92
                     LET       l_pesos       = 
                               l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion

            ELSE

                     LET       l_scta                  =  g_scta
                     LET       l_pesos                 =  g_pesos

            END IF

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
                              g_scta            <>  4       AND
                              g_scta            <>  8       AND
                              g_scta            <>  35)     THEN

                              LET      g_sumario[g_scta].pesos   =
                                       g_sumario[g_scta].pesos   +  g_imp_2_dec
                     END IF

                     LET      g_sum_rep[g_scta,g_siefore].acciones       = 
                              g_sum_rep[g_scta,g_siefore].acciones + g_acciones
                     LET      g_sum_rep[g_scta,g_siefore].pesos          =
                              g_sum_rep[g_scta,g_siefore].pesos    +  g_pesos
            END IF
   END FOREACH 
   FOR      g_scta                  =  2     TO  g_scta_fin
            IF      (g_scta            <>  2       AND
                     g_scta            <>  4       AND
                     g_scta            <>  8       AND
                     g_scta            <>  35)     THEN
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
END FUNCTION

FUNCTION    F_132_1_arma_viv()
   DEFINE   l_ini_credito_infonavit                 DATE
   LET       g_imp_2_dec        =
             (g_acciones  *  g_sie_inf[g_siefore].precio_accion)
   LET       g_pesos            =  g_imp_2_dec
   IF        g_scta             =  4      THEN
           #------  IDENTIFICADOR DE CREDITO EN GARANTIA ------#
           LET      l_ini_credito_infonavit           =  NULL
           EXECUTE  sql_18      USING    reg_solicitud.n_seguro
                                 INTO    l_ini_credito_infonavit
           IF       l_ini_credito_infonavit          IS  NOT  NULL   THEN
                    LET      g_id_credito_infonavit   = '1'
           ELSE
                    EXECUTE  sql_19     USING   reg_solicitud.n_seguro
                    IF       SQLCA.SQLCODE             =  0  THEN
                             LET      g_id_credito_infonavit   =  '2'
                    END IF
           END IF
            #---    TOTAL APORTACIONES VIVIENDA
           CALL     F_121_arma_num_aport_viv92()
           IF       g_aport_viv92            IS  NULL   THEN
                    LET      g_aport_viv92            =  cero
           END IF
           IF       g_id_credito_infonavit     = '1'  THEN
                    CALL      F_131_resta_credito_en_garantia()
		    LET       g_imp_2_dec       =  g_acciones
		    LET       g_acciones        =  g_imp_2_dec
           END  IF 
   END IF
   IF        g_scta             =  35     THEN
             CALL      F_131_resta_credito_en_garantia()
   END   IF
END   FUNCTION  

FUNCTION    F_132_1_inicializa()
   FOR      g_scta                 =  g_scta_ini      TO  g_scta_fin
            LET      g_saldos[g_scta].acciones    =  cero
            LET      g_saldos[g_scta].pesos       =  cero
            FOR      g_siefore         =  g_siefore_ini       TO  g_siefore_fin
                     LET      g_sie[g_scta,g_siefore].acciones    =  cero
                     LET      g_sie[g_scta,g_siefore].pesos       =  cero
                     LET      g_sie[g_scta,g_siefore].pesos2d     =  cero
            END FOR
   END FOR
   LET      g_scta                      =  cero
   LET      g_acciones                  =  cero
   LET      g_pesos                     =  cero
   LET      g_id_credito_infonavit      = '0'
   LET      g_aport_viv_issste          =  cero
   LET      g_aport_viv92               =  cero
END FUNCTION

FUNCTION    F_134_bloques_siefore_rcv()
   FOR      g_scta            =  g_scta_ini    TO  g_scta_fin
            IF       g_scta                     =  4      OR
                     g_scta                     =  8      OR
                     g_scta                     =  35     THEN      
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


FUNCTION    F_140_inserta_dis_provision()
   DEFINE   r_scta smallint
   DEFINE   verifica_provision                 ,
            v_folio_sua                        SMALLINT
   DEFINE   l_acciones                         ,
            l_pesos                            DEC(16,6) 
   IF       g_pesos                 =  0   THEN
            RETURN
   END IF
   LET      v_folio_sua                  =  9999
   LET      l_acciones                   =  g_acciones      *   -1
   LET      l_pesos                      =  g_pesos         *   -1
   LET      r_scta = g_scta

   DECLARE  spl_provisiona    CURSOR   FOR  sql_43
   FOREACH  spl_provisiona           USING
            reg_ctr_folio.folio             ,  #folio
            v_folio_sua                            ,  #folio_sua
            reg_solicitud.n_seguro         ,  #nss
            r_scta                                 ,  #subcuenta
            g_tipo_mov                             ,  #tipo_movimiento
            reg_solicitud.cont_servicio    ,  #consecutivo
            g_siefore                              ,  #siefore
            l_acciones                             ,  #monto_en_acciones  
            l_pesos                                ,  #monto_en_pesos
            g_id_aportante                         ,  #id_aportante
            g_today                                   #fecha_proceso
                                     INTO     verifica_provision
   END FOREACH
END FUNCTION

REPORT      R_150_arma_detalles()
   OUTPUT
            PAGE     LENGTH  1
            LEFT     MARGIN  0
            RIGHT    MARGIN  0
            TOP      MARGIN  0
            BOTTOM   MARGIN  0
   FORMAT
   ON EVERY ROW 
     ###########     Registro  de  detalle  de   vivienda       ############
     LET      g_num_registro       =  g_num_registro       +  1
     PRINT    COLUMN   01                                ,
              "02"                                       ,            # id_1
              g_num_registro      USING  "&&&&&&&&&&"    ,            # id_2
              reg_solicitud.tipo_recep_cuenta            ,            # id_3
              3       SPACES                             ,            # id_4
              reg_solicitud.tipo_ced_cuenta              ,            # id_5
              reg_solicitud.cve_ced_cuenta               ,            # id_6
              reg_solicitud.tipo_traspaso                ,            # id_7
              reg_ctr_folio.fecha_envio_saldos USING "YYYYMMDD",      # id_8
              reg_solicitud.n_unico                      ,            # id_09
              18      SPACES                             ,            # id_10
              reg_solicitud.n_seguro                     ,            # id_11
              11      SPACES                             ,            # id_12
              reg_solicitud.paterno                      ,            # id_13
              reg_solicitud.materno                      ,            # id_14
              reg_solicitud.nombre                       ,            # id_15
              reg_solicitud.fecha_recep_solic   USING  "YYYYMMDD" ,   # id_16
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
              8       SPACES                              ,            #id_30
              g_saldos[36].acciones  * 10000  USING "&&&&&&&&&&&&&&&&",#id_31
              15      SPACES                              ,            #id_32
              15      SPACES                              ,            #id_33
              8       SPACES                              ,            #id_34
              16      SPACES                              ,            #id_35
              g_aport_viv92            USING   "&&&&&"    ,            #id_36
              g_dias_cuot_soc          USING   "&&&&&"    ,            #id_37
              g_aport_fovissste        USING   "&&&&&"    ,            #id_38
              198     SPACES                                           #id_39
  ###########     Registro  1 de  detalle 05  de  rcv            ############
     LET      g_num_registro               =  g_num_registro     +  1
     IF       g_tot_bloques_rcv            <  8     THEN
              LET      g_bloques_rcv_reg_1          =  g_bloques_rcv   CLIPPED
     END IF
     PRINT    COLUMN   01,
              "05",                                          # id_01
              g_num_registro          USING  "&&&&&&&&&&",   # id_02
              reg_solicitud.tipo_recep_cuenta            ,   # id_03
              reg_solicitud.cve_recep_cuenta             ,   # id_04
              reg_solicitud.tipo_ced_cuenta              ,   # id_05
              reg_solicitud.cve_ced_cuenta               ,   # id_06
              reg_solicitud.tipo_traspaso                ,   # id_07
              g_fecha_nula                               ,   # id_08
              g_fecha_nula                               ,   # id_09
              g_fecha_nula                               ,   # id_10
              g_fecha_nula                               ,   # id_11
              reg_solicitud.n_unico                      ,   # id_12
              reg_solicitud.n_seguro                     ,   # id_13
              g_today        USING    "YYYYMMDD"         ,   # id_14
              g_bloques_rcv_reg_1                        ,   # id_15_54
              2  SPACES                                  ,   # id_55
              g_diag_proceso                             ,   # id_56
              124       SPACES                               # id_57-59
  #########     Arma  registro  2 de  detalle 05  de  rcv    ############
   IF     g_tot_bloques_rcv                  >  8   THEN
      LET     g_num_registro            =  g_num_registro  +  1
      PRINT   COLUMN   01,
              "05",                                          # id_01
              g_num_registro          USING  "&&&&&&&&&&",   # id_02
              reg_solicitud.tipo_recep_cuenta            ,   # id_03
              reg_solicitud.cve_recep_cuenta             ,   # id_04
              reg_solicitud.tipo_ced_cuenta              ,   # id_05
              reg_solicitud.cve_ced_cuenta               ,   # id_06
              reg_solicitud.tipo_traspaso                ,   # id_07
              g_fecha_nula                               ,   # id_08
              g_fecha_nula                               ,   # id_09
              g_fecha_nula                               ,   # id_10
              g_fecha_nula                               ,   # id_11
              reg_solicitud.n_unico                      ,   # id_12
              reg_solicitud.n_seguro                     ,   # id_13
              g_today        USING    "YYYYMMDD"         ,   # id_14
              g_bloques_rcv                              ,   # id_15_54
              2  SPACES                                  ,   # id_55
              g_diag_proceso                             ,   # id_56
              124       SPACES                               # id_57-59
    END IF
END REPORT

FUNCTION    F_200_fin()
   DEFINE   l_ruta                        CHAR(300)
   CALL     F_210_arma_encabezado()
   CALL     F_220_arma_sumario()
   CALL     F_240_genera_reporte_saldos()
   LET      l_ruta            =
            g_seg_modulo.ruta_envio     CLIPPED, "/", "TRASP_SALDOS.",
            reg_ctr_folio.tipo_traspaso     USING    "#",".",
            g_today      USING  "MMDD","-",g_hora[1,2],g_hora[4,5]
   LET      g_current       =  CURRENT
   DISPLAY  g_current," :RUTA Y NOMBRE DEL ARCHIVO OPE.  09: "
   DISPLAY  l_ruta    CLIPPED
   CALL     F_250_genera_archivo_saldos()
   IF       reg_ctr_folio.calculo_interes         =  1   THEN
            EXECUTE  sql_39     USING  reg_ctr_folio.folio
   END IF
   LET      g_comando         =  "lp ", g_comando
   RUN      g_comando
   LET      g_comando         =
            "echo \'chmod  777 ",g_seg_modulo.ruta_envio CLIPPED,
            "/*_09", "\' > arch_paso;" , "sh arch_paso 1>>err_paso 2>&1"
   RUN      g_comando
   LET      g_comando         =  "rm  arch_paso "
   RUN      g_comando
   LET      g_comando         =  "rm  err_paso "
   RUN      g_comando
   EXECUTE  sql_14   USING  g_provisionado,reg_ctr_folio.folio,g_recibido
   EXECUTE  sql_15   USING  g_provisionado,reg_ctr_folio.folio,g_recibido
   DISPLAY  g_current," :FIN DE PROGRAMA       : TCREAOP09L"
END FUNCTION

FUNCTION    F_210_arma_encabezado()
   LET      g_cza          =  g_seg_modulo.ruta_envio     CLIPPED,"/CZA_09"
   START    REPORT     R_211_arch_encabezado        TO  g_cza
            OUTPUT        TO  REPORT      R_211_arch_encabezado()
   FINISH   REPORT     R_211_arch_encabezado
END FUNCTION

REPORT      R_211_arch_encabezado()
   DEFINE   l_consec_lote_dia              SMALLINT
   OUTPUT
            PAGE      LENGTH  1
	    LEFT      MARGIN  0
	    RIGHT     MARGIN  0
	    TOP       MARGIN  0
	    BOTTOM    MARGIN  0
   FORMAT
   ON  EVERY  ROW 
   ###########     Registro  de  Encabezado  del  Archivo      ############
      LET      l_consec_lote_dia       =  cero
      EXECUTE  sql_55        USING   reg_ctr_folio.fecha_envio_saldos
                              INTO   l_consec_lote_dia
      LET       l_consec_lote_dia      =  l_consec_lote_dia   +  1
      PRINT    COLUMN  01,
               "01"                                       , #tipo_registro
               "02"                                       , #ident_servicio
               "09"                                       , #ident_operacion
               "01"                                       , #tipo_ent_origen
               g_afore_cod            USING  "&&&"        ,
               "01"                                       , #tipo_ent_destino
               "   "                                      , #cve_ent_destino
               "009"                                      , #ent_fed_envio_lote
               reg_ctr_folio.fecha_envio_saldos
                                      USING  "YYYYMMDD"   , #fecha_presentacion
               l_consec_lote_dia      USING  "&&&"        , #consec_lote_dia
               "02"                                       , #cve_mod_recepcion
               "  "                                       , #cod_result_operac
               "         "                                , #motivo_rechazo
               607      SPACES                            
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
                     g_scta                =  35    OR
                     g_scta                =  36    THEN
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
   LET      g_sum           =  g_seg_modulo.ruta_envio   CLIPPED,"/SUM_09" 
   START    REPORT    R_221_arma_reg_sumario        TO  g_sum
            OUTPUT    TO    REPORT     R_221_arma_reg_sumario()
   FINISH   REPORT   R_221_arma_reg_sumario
END FUNCTION

REPORT      R_221_arma_reg_sumario()
   OUTPUT
            PAGE     LENGTH   1
            LEFT     MARGIN   0
            RIGHT    MARGIN   0
            TOP      MARGIN   0
            BOTTOM   MARGIN   0
  FORMAT
      ON EVERY ROW 
  ###########     Registro  de  Sumario  del  Archivo     ############
      PRINT   COLUMN 01,
         "09"                                                          , #id_01
          g_num_registro                     USING "&&&&&&&&&"         , #id_02
          g_sumario[4].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_03
          g_sumario[4].pesos      * 100      USING "&&&&&&&&&&&&&&&",    #id_04
          g_sumario[14].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_05
          g_sumario[14].pesos     * 100      USING "&&&&&&&&&&&&&&&",    #id_06
          g_sumario[8].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_07
          g_sumario[8].pesos      * 100      USING "&&&&&&&&&&&&&&&",    #id_08
          g_sumario[35].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_09
          g_sumario[35].pesos     * 100      USING "&&&&&&&&&&&&&&&",    #id_10
          g_sumario[36].acciones  * 10000    USING "&&&&&&&&&&&&&&&&&",  #id_11
          g_sum_rcv                                             ,  #id_12_al_67
          14      SPACES                                                 #id_68
END REPORT

FUNCTION    F_250_genera_archivo_saldos()
   LET      g_cat          =
            "cat ",g_seg_modulo.ruta_envio       CLIPPED,"/CZA_09 ",
            g_seg_modulo.ruta_envio              CLIPPED,"/DET_09 ",
            g_seg_modulo.ruta_envio              CLIPPED,"/SUM_09 > ",
            g_seg_modulo.ruta_envio              CLIPPED,"/","TRASP_SALDOS.",
            reg_ctr_folio.tipo_traspaso   USING   "#",".",
            g_today                              USING   "MMDD","-",
            g_hora[1,2],g_hora[4,5]
   RUN      g_cat
END FUNCTION 

FUNCTION    F_240_genera_reporte_saldos()
   LET      g_comando                      = 
            g_seg_modulo.ruta_listados           CLIPPED, "/",
            g_usuario                            CLIPPED ,".TOTAL_TRA.",
            reg_ctr_folio.tipo_traspaso   USING  "#",".",
            g_today    USING    "DDMMYY","-", g_hora[1,2],g_hora[4,5]  CLIPPED
   DISPLAY  g_current," :RUTA Y NOMBRE DEL REPORTE OPE.  09: "
   DISPLAY  g_comando    CLIPPED

   START    REPORT     R_241_rep_saldos_tot      TO  g_comando
            OUTPUT     TO    REPORT  R_241_rep_saldos_tot()
   FINISH   REPORT     R_241_rep_saldos_tot
END FUNCTION 

REPORT      R_241_rep_saldos_tot()
   OUTPUT
            PAGE     LENGTH   66
            LEFT     MARGIN   0
            RIGHT    MARGIN   132
            TOP      MARGIN   0
            BOTTOM   MARGIN   0
   FORMAT
   PAGE     HEADER
       PRINT  COLUMN  71,"Pagina:",PAGENO USING "<<<<"
       PRINT  COLUMN  02,"TCREAOP09L",
	      COLUMN  13,'\033(s7B',"RESUMEN DE TRASPASOS ",
                       "  AFORE (CEDENTE)",
              COLUMN  64,"FECHA:",g_today USING "dd-mm-yyyy",'\033(s0B'  CLIPPED
       PRINT  COLUMN  02,g_afore_cod,"     ",g_raz_social
       PRINT  COLUMN  02,'\033(s7B',"FOLIO   : ",reg_ctr_folio.folio
                      USING     "########",'\033(s0B'    CLIPPED 
       PRINT  COLUMN  02,"ARCHIVO : " ,'\033(s7B',"TRASP_SALDOS.",
                      reg_ctr_folio.tipo_traspaso    USING    "#",".",
                      g_today    USING   "MMDD","-",
                      g_hora[1,2],g_hora[4,5],'\033(s0B'    CLIPPED
       PRINT  COLUMN  02,"RUTA    : /safre_prc/taa/envio"
       PRINT  COLUMN  02,"FECHA DE PRESENTACION : ",g_today USING "DD/MM/YYYY"

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
                         g_scta              =  35    OR
                         g_scta              =  36)   THEN
                         CONTINUE  FOR
                END IF
                PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                         COLUMN   60,g_sumario[g_scta].pesos
                                  USING    "###,###,###,###.##"
      END FOR
      PRINT    COLUMN   02,"________________________________________________",
                           "________________________________"
END REPORT
