###########################################################################
#Proyecto          => AFORE ( MEXICO )                                    #
#Propietario       => E.F.P.                                              #
#Programa TCAAB005 => GENERA ARCHIVO DE TRASPASO NORMAL-INTERNET(CEDENTE) #
#Sistema           => TCAA                                                #
#Fecha             => 23/06/2004                                          #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                      #
#Modifico          => JESUS DAVID YANEZ MORENO                            #
#Fecha             => 16/03/2008                                          #
#                     AJUSTES PARA SOPORTAR MULTISIEFORE Y NUEVAS         #
#                     SUBCUENTAS                                          #
###########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE 
            g_finicta                           ,
            g_fecha_liq_parti                   ,
            g_fecha_saldo_parti                 ,
            g_today                             DATE
   DEFINE 
            g_siefore                           ,
            g_tipo_mov                          ,
            g_scta                              ,
            g_scta_ini                          ,
            g_scta_fin                          ,
            g_siefore_ini                       ,
            g_siefore_fin                       ,
            g_recibido                          ,
            g_provisionado                      ,
            g_tipo_solicitud                    ,
            cero                                ,
            g_tot_bloques_rcv                   SMALLINT
   DEFINE
            g_num_aport_viv92                   ,
            g_num_aport_viv_issste              ,
            g_dias_cuot_soc                     ,
            g_num_registro                      INTEGER
   DEFINE
            g_precio_parti                      DEC(25,14),
            g_acciones                          DEC(18,6) ,
            g_pesos                             DEC(16,6),
            g_imp_2_dec                         DEC(15,2),
            g_monto_minimo                      DEC(15,2)
   DEFINE
            g_fecha_ult_pri_pat                 CHAR(008),
            g_fecha_ult_pri_ven                 CHAR(008),
            g_mod_nombre_bdnsar                 CHAR(001),
            g_cta_transfe_56_anios              CHAR(001), 
            g_iden_cta_mod_nombre               CHAR(001),
            g_cred_en_garantia                  CHAR(001),
            g_cod_result_operac                 CHAR(002),
            g_diag_proceso                      CHAR(015),
            g_id_aportante                      CHAR(011),
            g_id_prefijo                        CHAR(003),
            g_det                               CHAR(100),
            g_cza                               CHAR(100),
            g_sum                               CHAR(100),
            g_enter                             CHAR(001),
            g_hora                              CHAR(005), 
            g_cat                               CHAR(300),
            g_bloques_rcv                       CHAR(488),
            g_bloques_rcv_reg_1                 CHAR(488),
            g_sum_rcv                           CHAR(323),
            g_fecha_nula                        CHAR(008),
            g_comando                           CHAR(100),
            g_afore_cod                         CHAR(003),
            g_raz_social                        CHAR(050),
     	    g_usuario                           CHAR(008) 

   DEFINE   g_sie_inf       ARRAY[40]      OF    RECORD  --siefore
     	    nom_siefore                         CHAR(008),
            precio_accion                       DEC(11,6)
                                          END   RECORD
   DEFINE   g_sie           ARRAY[35,40]   OF    RECORD --subcuenta,siefore
            acciones                            DEC(16,6),
            pesos                               DEC(16,6),
            pesos2d                             DEC(16,2)
                                          END   RECORD
   DEFINE   g_saldos        ARRAY[35]     OF    RECORD --subcuenta
            acciones                            DEC(16,6),
            pesos                               DEC(16,2)
                                          END   RECORD
   DEFINE   g_sumario       ARRAY[35]     OF    RECORD   -- subcuenta
            nom_scta                            CHAR(30),
            scta_proc                           CHAR(02),
            pesos                               DEC(15,2),
            acciones                            DEC(16,6)
                                          END   RECORD
   DEFINE   g_sum_rep       ARRAY[35,40]   OF    RECORD  ---subcuenta,siefore
            pesos                               DEC(16,6),
            acciones                            DEC(16,6)
                                          END   RECORD

   DEFINE   reg_taa_cd_ctr_folio   RECORD  LIKE  safre_af:taa_cd_ctr_folio.*
   DEFINE   reg_taa_cd_det_cedido  RECORD  LIKE  safre_af:taa_cd_det_cedido.*
   DEFINE   g_seg_modulo           RECORD  LIKE  seg_modulo.*
END GLOBALS

GLOBALS   "TCAAB005S.4gl"    ###  Programa  para  la  Definicion  de  Querys

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
   CALL     STARTLOG("TCAAB005.log")
   CALL     arma_querys_TCAAB005S()
   CALL     F_012_prepara_querys_TCAAB005()
   LET      cero                       =  0
   LET      g_today                    =  TODAY
   LET      g_hora                     =  TIME
   LET      g_num_registro             =  cero
   LET      g_scta_ini                 =  1
   LET      g_siefore_ini              =  1
   CALL     F_020_busca_folio_pendiente()
   CALL     F_015_verifica_rechazo_convive()
   CALL     F_030_trae_parametros()
END FUNCTION

FUNCTION    F_012_prepara_querys_TCAAB005()
   PREPARE  sql_03          FROM     g_sql_03
   PREPARE  sql_04          FROM     g_sql_04
   PREPARE  sql_05          FROM     g_sql_05
   PREPARE  sql_06          FROM     g_sql_06
   PREPARE  sql_07          FROM     g_sql_07
   PREPARE  sql_07_2        FROM     g_sql_07_2
   PREPARE  sql_08          FROM     g_sql_08
   PREPARE  sql_09          FROM     g_sql_09
   PREPARE  sql_09_1        FROM     g_sql_09_1
   PREPARE  sql_09_2        FROM     g_sql_09_2
   PREPARE  sql_09_3        FROM     g_sql_09_3
   PREPARE  sql_09_4        FROM     g_sql_09_4
   PREPARE  sql_10          FROM     g_sql_10
   PREPARE  sql_11          FROM     g_sql_11
   EXECUTE  sql_11
   PREPARE  sql_11_110      FROM     g_sql_11_110
   EXECUTE  sql_11_110
   PREPARE  sql_40          FROM     g_sql_40
   EXECUTE  sql_40
   PREPARE  sql_35          FROM     g_sql_35
   PREPARE  sql_36          FROM     g_sql_36
   PREPARE  sql_37          FROM     g_sql_37
   PREPARE  sql_12          FROM     g_sql_12
   PREPARE  sql_13          FROM     g_sql_13
   PREPARE  sql_14          FROM     g_sql_14
   PREPARE  sql_15          FROM     g_sql_15
   PREPARE  sql_16          FROM     g_sql_16
   PREPARE  sql_17          FROM     g_sql_17
   PREPARE  sql_18          FROM     g_sql_18
   PREPARE  sql_19          FROM     g_sql_19
   PREPARE  sql_21          FROM     g_sql_21
   PREPARE  sql_27          FROM     g_sql_27
   PREPARE  sql_28          FROM     g_sql_28
   PREPARE  sql_29          FROM     g_sql_29
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
   PREPARE  sql_54          FROM     g_sql_54   # marca trasnsferencia de 56 
   PREPARE  sql_31          FROM     g_sql_31
   PREPARE  sql_32          FROM     g_sql_32
   PREPARE  sql_32_110      FROM     g_sql_32_110
   PREPARE  sql_34          FROM     g_sql_34
   PREPARE  sql_39          FROM     g_sql_39
   PREPARE  sql_55          FROM     g_sql_55   # calcula  consec_lote_dia
   PREPARE  sql_56          FROM     g_sql_56 
   PREPARE  sql_57          FROM     g_sql_57
   EXECUTE  sql_03          INTO     g_recibido
   EXECUTE  sql_04          INTO     g_provisionado
   EXECUTE  sql_13          INTO     g_afore_cod,g_raz_social,g_usuario
   EXECUTE  sql_36          INTO     g_scta_fin
   EXECUTE  sql_37          INTO     g_siefore_fin
END FUNCTION

FUNCTION    F_015_verifica_rechazo_convive()
   DEFINE   l_marca_causa                    ,
            l_estado_marca                   , 
            l_codigo_rechazo                 ,
            xcodigo_marca                    ,
            xcodigo_rechazo                  ,
            l_marca_entra                    SMALLINT
   DEFINE   l_fecha_causa                    DATE
   DECLARE  cur_convivencia    CURSOR   FOR      sql_49
   OPEN     cur_convivencia     USING   reg_taa_cd_ctr_folio.folio,g_recibido
   FOREACH  cur_convivencia      INTO   reg_taa_cd_det_cedido.*
            INITIALIZE  l_marca_entra        TO  NULL
            EXECUTE  sql_50     USING   reg_taa_cd_det_cedido.tipo_traspaso
 		                 INTO   l_marca_entra
            IF       l_marca_entra           IS  NULL    THEN 
                     PROMPT   " TIPO TRASPASO : ",
                              reg_taa_cd_det_cedido.tipo_traspaso, 
                              " NO EXISTE EN CATALOGO <Enter> PARA",
                              " CONTINUAR"   FOR   g_enter
                     PROMPT   " CONTACTAR CON SISTEMAS...<Enter>",
                              " PARA SALIR"  FOR   g_enter
            END IF
            LET      l_marca_causa                  =  cero
            LET      l_estado_marca                 =  cero
            LET      l_codigo_rechazo               =  cero
            LET      l_fecha_causa                  =  NULL
            DECLARE  cursor_marca        CURSOR   FOR   sql_51
            OPEN     cursor_marca        USING   
                     reg_taa_cd_det_cedido.n_seguro,
                     l_marca_entra                , # marca
                     reg_taa_cd_ctr_folio.folio   , #envia folio como correlati
                     l_estado_marca               , #envia 0-acept 30-rech prev
                     l_codigo_rechazo             , # envia  cod_rech  en  0
                     l_marca_causa                , # envia  0
                     l_fecha_causa                , # envia  NULLA
                     g_usuario                      # envia  usuario de sesion
            FETCH    cursor_marca  
                     INTO     xcodigo_marca,xcodigo_rechazo
                     IF       xcodigo_rechazo            =  0    THEN
                              EXECUTE   sql_52
                                USING   g_recibido ,
		                        reg_taa_cd_ctr_folio.folio,
		                        reg_taa_cd_det_cedido.n_seguro,
		                        reg_taa_cd_det_cedido.estado
                     END IF
                     CLOSE    cursor_marca
   END FOREACH
END FUNCTION

FUNCTION    F_020_busca_folio_pendiente()
   DEFINE   l_verifica_prov                     SMALLINT
   DEFINE   l_tot_registros                     INTEGER
   OPEN     WINDOW    TCAAB0051      AT  2,2 
            WITH      FORM     "TCAAB0051"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB005            TRASPASOS  AFORE-AFORE (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)   
   DISPLAY  "     GENERACION DE ARCHIVO DE TRASPASOS DE CUENTAS INDIVIDUALES ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)   
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)
   DECLARE  cur_sql_05         CURSOR  FOR    sql_05
   FOREACH  cur_sql_05          USING  g_recibido
                                 INTO  reg_taa_cd_ctr_folio.*
            EXECUTE  sql_06     USING  reg_taa_cd_ctr_folio.folio,g_recibido
                                 INTO  l_tot_registros
            IF       reg_taa_cd_ctr_folio.tipo_traspaso       =  1    THEN
                     DISPLAY  "NORMAL"     TO   FORMONLY.tipo_traspaso
            ELSE
                     DISPLAY  "INTERNET"   TO   FORMONLY.tipo_traspaso
            END IF
            DISPLAY  BY  NAME  reg_taa_cd_ctr_folio.fecha_presentacion
            DISPLAY  BY  NAME  reg_taa_cd_ctr_folio.fecha_envio_saldos
            DISPLAY  BY  NAME  reg_taa_cd_ctr_folio.fecha_liquidacion
            DISPLAY  BY  NAME  l_tot_registros
            DISPLAY  BY  NAME  reg_taa_cd_ctr_folio.folio 
            WHILE    TRUE
                     PROMPT    " PARA SELECCIONAR FOLIO TECLEE <S>  o",
                               "  <N> PARA FOLIO SIGUIENTE  [S/N] ? "
                               FOR      g_enter
                     IF        g_enter      MATCHES   "[sSnN]"       THEN
                               IF       g_enter      MATCHES   "[sS]"    THEN
                                        EXIT FOREACH
                               ELSE
                                        LET    reg_taa_cd_ctr_folio.folio  =  0
                                        CLEAR     FORM
                                        CONTINUE  FOREACH
                               END IF
                     END  IF
            END      WHILE
   END FOREACH
   IF       reg_taa_cd_ctr_folio.folio      IS  NULL     OR  
            reg_taa_cd_ctr_folio.folio       =  cero     THEN
            ERROR    "  NO  HAY  ARCHIVO  PENDIENTE  PARA  GENERAR:   "
            PROMPT   "  TECLEE ENTER PARA SALIR...  "  FOR  g_enter
            EXIT     PROGRAM
   END IF
   LET      g_fecha_liq_parti          =
            MDY(MONTH(reg_taa_cd_ctr_folio.fecha_liquidacion), "01" ,
            YEAR(reg_taa_cd_ctr_folio.fecha_liquidacion))
   LET      g_fecha_saldo_parti        =  MDY(MONTH(TODAY),"01",YEAR(TODAY))
   CALL     F_022_precio_acc_parti()
   WHILE    TRUE
            PROMPT   "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? "  FOR  g_enter
            IF       g_enter      MATCHES   "[sSnN]"       THEN
                     IF       g_enter       MATCHES   "[sS]"       THEN
                              EXECUTE  sql_53 
                                       USING     reg_taa_cd_ctr_folio.folio 
	 		                INTO     l_verifica_prov
                              IF      (l_verifica_prov       =  0         OR 
			               l_verifica_prov      IS  NULL)     THEN
                                       EXIT WHILE
                              ELSE 
		  	               PROMPT    " ERROR.. REGISTROS PROVISION",
                                                 "ADOS LLAME A EFP <Enter> ",
                                                 " para salir..."  FOR  g_enter
		   	               EXIT      PROGRAM
                              END IF
                     ELSE
                              EXIT     PROGRAM
                     END IF
            END  IF
   END      WHILE
   ERROR    "  PROCESANDO  INFORMACION .........    "
END FUNCTION

FUNCTION    F_022_precio_acc_parti()
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
   FOR      g_siefore             =  g_siefore_ini      TO  g_siefore_fin
            IF       g_sie_inf[g_siefore].precio_accion        =  cero   THEN
                     ERROR    "  NO HAY PRECIO DE ACCION SIEFORE RCV BASICA ",
                               g_siefore   USING   "#","                     "
                     PROMPT   "TECLEE ENTER PARA SALIR..."     FOR  g_enter
                     EXIT     PROGRAM
            END IF
   END FOR
   DISPLAY  g_sie_inf[1].precio_accion      TO  g_precio_accion_b1
   DISPLAY  g_sie_inf[2].precio_accion      TO  g_precio_accion_b2
   DISPLAY  g_sie_inf[3].precio_accion      TO  g_precio_accion_b3
   DISPLAY  g_sie_inf[4].precio_accion      TO  g_precio_accion_b4
   DISPLAY  g_sie_inf[5].precio_accion      TO  g_precio_accion_b5
   DISPLAY  g_sie_inf[6].precio_accion      TO  g_precio_accion_b6

   IF       g_precio_parti                   =  cero     THEN
            ERROR    "NO HAY PRECIO DE PARTICIPACION DE VIVIENDA            "
            PROMPT   "TECLEE ENTER PARA SALIR..." FOR g_enter
            EXIT     PROGRAM
   END IF
   DISPLAY  BY  NAME  g_precio_parti 
END FUNCTION

FUNCTION    F_030_trae_parametros()
   DEFINE   l_tab_subcuenta      RECORD  LIKE safre_af:tab_subcuenta.*
   LET      g_fecha_nula               = "00010101"
   EXECUTE  sql_08           INTO   g_monto_minimo  
   EXECUTE  sql_10           INTO   g_seg_modulo.*
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

FUNCTION    F_100_proceso()
   DEFINE   l_leidos                            INTEGER
   CALL     F_110_inicializa_sumarios()
   START    REPORT R_150_arma_detalles    TO  g_det
   DECLARE  cur_nss_ced         CURSOR   FOR   sql_12
   OPEN     cur_nss_ced          USING   reg_taa_cd_ctr_folio.folio,g_recibido
   FOREACH  cur_nss_ced           INTO   reg_taa_cd_det_cedido.*,
                                         g_tipo_solicitud,g_finicta
            CALL     F_120_arma_datos_generales()
            CALL     F_130_arma_saldo_viv()
            CALL     F_133_arma_saldo_rcv()
            CALL     F_145_insert_tmp_envio_2dec()
                     ######    Arma  Registros  de  Detalle   #########
            OUTPUT   TO   REPORT     R_150_arma_detalles()
            LET      l_leidos             =  l_leidos       +  1
            DISPLAY  "REGISTROS  PROCESADOS: ", l_leidos   AT  16,2
   END FOREACH
   IF       l_leidos                  =  cero  THEN
            ERROR    "  NO SE ENCONTRARON REGISTROS PARA SER TRASPASADOS"
                       ATTRIBUTE(REVERSE)
            PROMPT   "  TECLEE ENTER PARA SALIR..." FOR g_enter
            EXIT     PROGRAM
    END IF
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
   DEFINE   l_f_ini_cr_garantia                 DATE
   LET      g_mod_nombre_bdnsar           =  ' '
   LET      g_iden_cta_mod_nombre         =  ' '
   EXECUTE  sql_17      USING   reg_taa_cd_det_cedido.n_seguro
                         INTO   g_mod_nombre_bdnsar
    #---    TOTAL APORTACIONES VIVIENDA
   CALL     F_121_arma_num_aport_viv92()
   LET      g_num_aport_viv_issste        =  cero
   IF       g_num_aport_viv92            IS  NULL   THEN
            LET      g_num_aport_viv92            =  cero
   END IF
   #------  IDENTIFICADOR DE CREDITO EN GARANTIA ------#
   LET      g_cred_en_garantia            = '0'
   LET      l_f_ini_cr_garantia           =  NULL
   EXECUTE  sql_18      USING    reg_taa_cd_det_cedido.n_seguro
                         INTO    l_f_ini_cr_garantia
   IF       l_f_ini_cr_garantia          IS  NOT  NULL   THEN
            LET      g_cred_en_garantia        = '1'
   ELSE
            EXECUTE  sql_19     USING   reg_taa_cd_det_cedido.n_seguro
            IF       SQLCA.SQLCODE             =  0  THEN
                     LET      g_cred_en_garantia        =  '2'
            END IF
   END IF
   #--- TOTAL DIAS COTIZADOS DE CUOTA SOCIAL
   EXECUTE  sql_21         USING   reg_taa_cd_det_cedido.n_seguro
                            INTO   g_dias_cuot_soc
   IF       g_dias_cuot_soc         IS  NULL  THEN
            LET      g_dias_cuot_soc         =  cero
   END IF
   EXECUTE  sql_50_1       USING   reg_taa_cd_det_cedido.tipo_traspaso
	     	            INTO   g_tipo_mov , g_id_prefijo
   LET      g_id_aportante     =   g_id_prefijo    CLIPPED ,
	    reg_taa_cd_det_cedido.cve_recep_cuenta
   #------MARCA DE TRANSFERENCIA POR 56 AÑOS 1 CON MARCA " " SIN MARCA
   EXECUTE  sql_54         USING   reg_taa_cd_det_cedido.n_seguro
                            INTO   g_cta_transfe_56_anios
END FUNCTION

FUNCTION    F_121_arma_num_aport_viv92()
   DEFINE   l_num_aport_viv92                SMALLINT
   LET      g_num_aport_viv92                =  cero
   LET      l_num_aport_viv92                =  cero
   FOREACH  c_dis_cuentas             INTO   g_tabname 
            CALL     arma_querys_TCAAB005S()
            PREPARE  sql_22           FROM   g_sql_22
            EXECUTE  sql_22          USING   reg_taa_cd_det_cedido.n_seguro 
                                      INTO   l_num_aport_viv92 
            LET      g_num_aport_viv92          =
                     g_num_aport_viv92          +  l_num_aport_viv92
   END FOREACH
END FUNCTION

FUNCTION    F_130_arma_saldo_viv()
   DEFINE   l_grupo                             SMALLINT
   DEFINE   l_acciones                          ,
            l_pesos                             DECIMAL(16,6)
   LET      l_grupo                    =  4   
   CALL     F_132_1_inicializa_g_saldos()
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
            IF        g_scta               =   14    OR
                      g_scta               =   35    THEN
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
   EXECUTE  sql_27         USING    reg_taa_cd_det_cedido.n_seguro,g_scta
                            INTO    l_fecha_desde
   IF       l_fecha_desde          IS  NULL                OR
            l_fecha_desde           <  '01/01/1997'        THEN
            LET      l_fecha_desde        =
                     MDY(MONTH(g_today),1,YEAR(g_today))
   END IF
   DECLARE  cur_interes   CURSOR   FOR   sql_28
   OPEN     cur_interes    USING   l_fecha_desde,
                                   l_fecha_desde,
                                   g_scta,
                                   reg_taa_cd_det_cedido.n_seguro,
                                   reg_taa_cd_det_cedido.n_seguro,
                                   g_scta
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
                     IF       STATUS             =  NOTFOUND     THEN
                              EXECUTE  sql_15       USING    g_provisionado,
                                       reg_taa_cd_ctr_folio.folio,g_recibido
                              DISPLAY  "  NO HAY TASA INTERES FOVISSSTE DE :",
                                       l_fecha_tasa_valor   AT  16,2
                                       ATTRIBUTE (REVERSE)
                              PROMPT   "  TECLEE  ENTER  PARA  SALIR....  ? ",
                                       "                  "    FOR    g_enter
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
   DEFINE   l_acc_ret_92                   ,
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
                     g_scta                  =  14      OR 
                     g_scta                  =  35 )    THEN 
                     CONTINUE  FOREACH                      
            END IF                                          

            LET l_pesos = g_pesos
            LET l_scta  = g_scta

            IF (g_scta = 2 OR
                g_scta = 6 OR
                g_scta = 9   ) THEN

                     LET l_scta = 2  --agrupa la 2,6,9

                     EXECUTE sql_09
                     USING   reg_taa_cd_det_cedido.n_seguro ,
                             g_today                        ,
                             g_siefore
                     INTO    l_acc_ret_92

                     LET l_pesos = l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            END IF

            IF (g_scta = 3  OR 
                g_scta = 22  ) THEN

                   LET l_scta = 3  -- agrupa la 3,22

                   EXECUTE sql_09_1
                     USING   reg_taa_cd_det_cedido.n_seguro ,
                             g_today                        ,
                             g_siefore
                     INTO    l_acc_ret_92

                     LET l_pesos = l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            END IF

            IF (g_scta = 10  OR 
                g_scta = 23  ) THEN
                   LET l_scta = 10  -- agrupa la 3,22
                   EXECUTE sql_09_2
                     USING   reg_taa_cd_det_cedido.n_seguro ,
                             g_today                        ,
                             g_siefore
                     INTO    l_acc_ret_92
                     LET l_pesos = l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            END IF

            IF (g_scta = 11  OR 
                g_scta = 24  ) THEN
                   LET l_scta = 11  -- agrupa la 3,22
                   EXECUTE sql_09_3
                     USING   reg_taa_cd_det_cedido.n_seguro ,
                             g_today                        ,
                             g_siefore
                     INTO    l_acc_ret_92
                     LET l_pesos = l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            END IF

            IF (g_scta = 12  OR 
                g_scta = 25  ) THEN
                   LET l_scta = 12  -- agrupa la 3,22
                   EXECUTE sql_09_4
                     USING   reg_taa_cd_det_cedido.n_seguro ,
                             g_today                        ,
                             g_siefore
                     INTO    l_acc_ret_92
                     LET l_pesos = l_acc_ret_92 * g_sie_inf[g_siefore].precio_accion
            END IF

            IF l_pesos >= g_monto_minimo THEN

                     CALL     F_140_inserta_dis_provision() -- se provisiona g_scta
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

                     IF (g_scta <> 2  AND 
                         g_scta <> 3  AND 
                         g_scta <> 10 AND 
                         g_scta <> 11 AND 
                         g_scta <> 12    )THEN
                         LET g_sumario[g_scta].pesos = g_sumario[g_scta].pesos + 
                                                       g_imp_2_dec
                     END IF

                     LET      g_sum_rep[g_scta,g_siefore].acciones       = 
                              g_sum_rep[g_scta,g_siefore].acciones + g_acciones
                     LET      g_sum_rep[g_scta,g_siefore].pesos          =
                              g_sum_rep[g_scta,g_siefore].pesos    +  g_pesos

            END IF
   END FOREACH 
   LET      g_saldos[2].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
            LET      g_sie[2,g_siefore].pesos2d         =
                     g_sie[2,g_siefore].pesos
            LET      g_saldos[2].pesos                  =
                     g_saldos[2].pesos      +   g_sie[2,g_siefore].pesos2d
            LET      g_sumario[2].pesos                 =
                     g_sumario[2].pesos     +   g_sie[2,g_siefore].pesos2d
   END FOR

   LET      g_saldos[3].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
            LET      g_sie[3,g_siefore].pesos2d         =
                     g_sie[3,g_siefore].pesos
            LET      g_saldos[3].pesos                  =
                     g_saldos[3].pesos      +   g_sie[3,g_siefore].pesos2d
            LET      g_sumario[3].pesos                 =
                     g_sumario[3].pesos     +   g_sie[3,g_siefore].pesos2d
   END FOR

   LET      g_saldos[10].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
            LET      g_sie[10,g_siefore].pesos2d         =
                     g_sie[10,g_siefore].pesos
            LET      g_saldos[10].pesos                  =
                     g_saldos[10].pesos      +   g_sie[10,g_siefore].pesos2d
            LET      g_sumario[10].pesos                 =
                     g_sumario[10].pesos     +   g_sie[10,g_siefore].pesos2d
   END FOR

   LET      g_saldos[11].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
            LET      g_sie[11,g_siefore].pesos2d         =
                     g_sie[11,g_siefore].pesos
            LET      g_saldos[11].pesos                  =
                     g_saldos[11].pesos      +   g_sie[11,g_siefore].pesos2d
            LET      g_sumario[11].pesos                 =
                     g_sumario[11].pesos     +   g_sie[11,g_siefore].pesos2d
   END FOR

   LET      g_saldos[12].pesos      =  cero
   FOR      g_siefore              =  g_siefore_ini     TO  g_siefore_fin
            LET      g_sie[12,g_siefore].pesos2d         =
                     g_sie[12,g_siefore].pesos
            LET      g_saldos[12].pesos                  =
                     g_saldos[12].pesos      +   g_sie[12,g_siefore].pesos2d
            LET      g_sumario[12].pesos                 =
                     g_sumario[12].pesos     +   g_sie[12,g_siefore].pesos2d
   END FOR

   CALL     F_134_bloques_siefore_rcv()

   CALL     F_137_ult_pri_apor_vol_patro()    #APORT.  VOLUNTARIAS  PATRONALES 
   CALL     F_138_ult_pri_apor_vol_venta()    #APORT.  VOLUNTARIAS  VENTANILLA
END FUNCTION

FUNCTION    F_133_1_inicializa_rcv()
   FOR      g_scta                 =  g_scta_ini      TO  g_scta_fin
            IF       g_scta                  =  4     OR
                     g_scta                  =  8     OR
                     g_scta                  =  14    OR
                     g_scta                  =  35    THEN      
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
   IF       g_saldos[3].pesos            =  cero               OR
            g_saldos[3].pesos            <  g_monto_minimo     THEN
            LET      l_fecha             =  NULL
   ELSE
            EXECUTE  sql_48 
            FOREACH  c_dis_cuentas    INTO  g_tabname 
                     CALL     arma_querys_TCAAB005S()
                     PREPARE  sql_23       FROM  g_sql_23 
                     PREPARE  sql_24       FROM  g_sql_24 
                     EXECUTE  sql_23      USING  reg_taa_cd_det_cedido.n_seguro
                     EXECUTE  sql_24      USING  reg_taa_cd_det_cedido.n_seguro
            END FOREACH
            EXECUTE  sql_31   INTO  l_fecha
            IF       l_fecha                 IS  NULL   THEN
                     EXECUTE   sql_32      INTO  l_fecha
            END IF
   END IF
   IF       g_tipo_solicitud       =  2    THEN
            IF       g_finicta                =  l_fecha     THEN
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
   DEFINE   l_fecha                              DATE
   LET      g_fecha_ult_pri_ven               =  NULL
   IF       g_saldos[10].pesos                =  cero              OR
            g_saldos[10].pesos                <  g_monto_minimo    THEN
            LET      l_fecha                  =  NULL
   ELSE
            EXECUTE  sql_48
            FOREACH  c_dis_cuentas INTO g_tabname 
                     CALL     arma_querys_TCAAB005S()
                     PREPARE  sql_25        FROM  g_sql_25 
                     PREPARE  sql_26        FROM  g_sql_26 
                     PREPARE  sql_25_110    FROM  g_sql_25_110
                     EXECUTE  sql_25       USING  reg_taa_cd_det_cedido.n_seguro
                     EXECUTE  sql_26       USING  reg_taa_cd_det_cedido.n_seguro
                     EXECUTE  sql_25_110   USING  reg_taa_cd_det_cedido.n_seguro
            END FOREACH
            EXECUTE  sql_31                 INTO  l_fecha 
            IF       l_fecha                  IS  NULL        THEN
                     EXECUTE   sql_32       INTO  l_fecha 
            END IF
            IF       l_fecha                  IS  NULL        THEN 
                     EXECUTE   sql_32_110   INTO  l_fecha
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
   DECLARE  spl_provisiona    CURSOR   FOR  sql_43
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
   FOR      g_scta             =  g_scta_ini         TO  g_scta_fin
            IF       g_saldos[g_scta].pesos           >  cero  THEN
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
     IF      (reg_taa_cd_det_cedido.id_regs_reenviados    <>  1      AND
              reg_taa_cd_det_cedido.id_regs_reenviados    <>  2)     THEN
              LET      reg_taa_cd_det_cedido.id_regs_reenviados      =  " "
     END  IF
     PRINT    COLUMN   01                                ,
              "02"                                       ,            # id_1
              g_num_registro      USING  "&&&&&&&&&&"    ,            # id_2
              reg_taa_cd_det_cedido.tipo_recep_cuenta    ,            # id_3
              3       SPACES                             ,            # id_4
              reg_taa_cd_det_cedido.tipo_ced_cuenta      ,            # id_5
              reg_taa_cd_det_cedido.cve_ced_cuenta       ,            # id_6
              reg_taa_cd_det_cedido.tipo_traspaso        ,            # id_7
              reg_taa_cd_ctr_folio.fecha_envio_saldos USING "YYYYMMDD",#id_8
              8       SPACES                             ,            # id_9 
              reg_taa_cd_det_cedido.n_unico              ,            # id_10
              reg_taa_cd_det_cedido.n_seguro             ,            # id_11
              15      SPACES                             ,            # id_12
              reg_taa_cd_det_cedido.rfc                  ,            # id_13
              reg_taa_cd_det_cedido.paterno              ,            # id_14
              reg_taa_cd_det_cedido.materno              ,            # id_15
              reg_taa_cd_det_cedido.nombre               ,            # id_16
              2       SPACES                             ,            # id_17
              g_mod_nombre_bdnsar                        ,            # id_18
              reg_taa_cd_det_cedido.cve_sector           ,            # id_19
              10      SPACES                             ,            # id_20
              reg_taa_cd_det_cedido.fecha_recep_solic USING "YYYYMMDD",#id_21
              reg_taa_cd_det_cedido.ident_lote_solici    ,            # id_22
              15      SPACES                             ,            # id_23
              reg_taa_cd_det_cedido.n_seguro_cedente     ,            # id_24
              reg_taa_cd_det_cedido.rfc_cedente          ,            # id_25
              30      SPACES                             ,            # id_26
              reg_taa_cd_det_cedido.paterno_cedente      ,            # id_27
              reg_taa_cd_det_cedido.materno_cedente      ,            # id_28
              reg_taa_cd_det_cedido.nombre_cedente       ,            # id_29
              g_saldos[35].acciones  * 1000000 USING "&&&&&&&&&&&&&&&",#id_30
              g_saldos[35].pesos     * 100     USING "&&&&&&&&&&&&&&&",#id_31
              g_saldos[4].acciones   * 1000000 USING "&&&&&&&&&&&&&&&",#id_32
              g_saldos[4].pesos      * 100     USING "&&&&&&&&&&&&&&&",#id_33
              15      SPACES                           ,               #id_34
              g_saldos[14].acciones  * 1000000 USING "&&&&&&&&&&&&&&&",#id_35
              g_saldos[14].pesos     * 100     USING "&&&&&&&&&&&&&&&",#id_36
              g_saldos[8].acciones   * 1000000 USING "&&&&&&&&&&&&&&&",#id_37
              g_saldos[8].pesos      * 100     USING "&&&&&&&&&&&&&&&",#id_38
              g_iden_cta_mod_nombre                      ,            # id_39 
              reg_taa_cd_det_cedido.id_regs_reenviados   ,            # id_40
              g_cred_en_garantia                         ,            # id_41
              2       SPACES                             ,            # id_42
              15      SPACES                             ,            # id_43
              98      SPACES                             ,            # id_44
              g_num_aport_viv92        USING  "&&&&&"    ,            # id_45
              2       SPACES                             ,            # id_46
              g_dias_cuot_soc          USING  "&&&&&"    ,            # id_47
              10      SPACES                             ,            # id_48
              g_num_aport_viv_issste   USING  "&&&&&"    ,            # id_49
              6       SPACES                                          # id_50
  ###########     Registro  1 de  detalle 05  de  rcv            ############
     LET      g_num_registro               =  g_num_registro     +  1
     IF       g_tot_bloques_rcv            <  8     THEN
              LET      g_bloques_rcv_reg_1          =  g_bloques_rcv   CLIPPED
     END IF
     PRINT    COLUMN   01,
              "05",                                                   # id_01
              g_num_registro          USING  "&&&&&&&&&&",            # id_02
              reg_taa_cd_det_cedido.tipo_recep_cuenta    ,            # id_03
              reg_taa_cd_det_cedido.cve_recep_cuenta     ,            # id_04
              reg_taa_cd_det_cedido.tipo_ced_cuenta      ,            # id_05
              reg_taa_cd_det_cedido.cve_ced_cuenta       ,            # id_06
              reg_taa_cd_det_cedido.tipo_traspaso        ,            # id_07
              g_fecha_ult_pri_pat                        ,            # id_08
              g_fecha_ult_pri_ven                        ,            # id_09
              reg_taa_cd_det_cedido.n_unico              ,            # id_10
              reg_taa_cd_det_cedido.n_seguro             ,            # id_11
              54       SPACES,                                        # id_12
              g_today                            USING  "YYYYMMDD",   # id_13
              27      SPACES ,                                        #id_14
              g_cta_transfe_56_anios                               ,  # id_15
              33      SPACES ,                                        # id_16
              g_bloques_rcv_reg_1,                                    # id_17_64
              g_cod_result_operac,                                    # id_65
              g_diag_proceso,                                         # id_66
              33      SPACES                                          # id_67_69

  #########     Arma  registro  2 de  detalle 05  de  rcv    ############
   IF     g_tot_bloques_rcv                  >  8   THEN
          LET      g_num_registro            =  g_num_registro  +  1
          PRINT    COLUMN   01,
                  "05",                                              # id_01
                   g_num_registro          USING  "&&&&&&&&&&",      # id_02
                   reg_taa_cd_det_cedido.tipo_recep_cuenta    ,      # id_03
                   reg_taa_cd_det_cedido.cve_recep_cuenta     ,      # id_04
                   reg_taa_cd_det_cedido.tipo_ced_cuenta      ,      # id_05
                   reg_taa_cd_det_cedido.cve_ced_cuenta       ,      # id_06
                   reg_taa_cd_det_cedido.tipo_traspaso        ,      # id_07
                   g_fecha_ult_pri_pat                        ,      # id_08
                   g_fecha_ult_pri_ven                        ,      # id_09
                   reg_taa_cd_det_cedido.n_unico              ,      # id_10
                   reg_taa_cd_det_cedido.n_seguro             ,      # id_11
                   54       SPACES,                                  # id_12
                   g_today                      USING  "YYYYMMDD",   # id_13
                   27      SPACES ,                                  # id_14
                   g_cta_transfe_56_anios                          , # id_15
                   33      SPACES ,                                  # id_16
                   g_bloques_rcv      ,                              # id_17_64
                   g_cod_result_operac,                              # id_65
                   g_diag_proceso,                                   # id_66
                   33       SPACES                                   # id_67_69
    END IF
END REPORT

FUNCTION    F_200_fin()
   DEFINE   l_ruta                        CHAR(300)
   CALL     F_210_arma_encabezado()
   CALL     F_220_arma_sumario()
   CALL     F_240_genera_reporte_saldos()
   LET      l_ruta            =
            g_seg_modulo.ruta_envio     CLIPPED, "/", "TRASP_SALDOS.",
            reg_taa_cd_ctr_folio.tipo_traspaso     USING    "#",".",
            g_today      USING  "MMDD","-",g_hora[1,2],g_hora[4,5]
   DISPLAY  "Ruta Archivo:",l_ruta       AT  18,2
   PROMPT   " Proceso Finalizado Teclee  <<Enter>> para Salir "  FOR  g_enter
   CALL     F_250_genera_archivo_saldos()
   IF       reg_taa_cd_ctr_folio.calculo_interes         =  1   THEN
            EXECUTE  sql_39     USING  reg_taa_cd_ctr_folio.folio
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
   EXECUTE  sql_14   USING  g_provisionado,reg_taa_cd_ctr_folio.folio,g_recibido
   EXECUTE  sql_15   USING  g_provisionado,reg_taa_cd_ctr_folio.folio,g_recibido
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
      EXECUTE  sql_55        USING   reg_taa_cd_ctr_folio.fecha_envio_saldos
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
               reg_taa_cd_ctr_folio.fecha_envio_saldos
                                      USING  "YYYYMMDD"   , #fecha_presentacion
               l_consec_lote_dia      USING  "&&&"        , #consec_lote_dia
               "02"                                       , #cve_mod_recepcion
               "  "                                       , #cod_result_operac
               "         "                                , #motivo_rechazo
               687      SPACES                            
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
                     g_scta                =  35    THEN
                     CONTINUE  FOR
            END IF
            IF       g_sumario[g_scta].pesos        <>  cero     THEN
                     EXECUTE   sql_16      USING    reg_taa_cd_ctr_folio.folio,
                                                    g_scta,
                                                    g_sumario[g_scta].pesos,
                                                    g_num_registro
            END IF
            IF       g_sumario[g_scta].pesos        <>  cero     THEN
                     LET       g_sum_rcv             =  g_sum_rcv     CLIPPED,
                               g_sumario[g_scta].scta_proc  USING "&&"  CLIPPED
                     LET       g_sum_rcv             =  g_sum_rcv     CLIPPED,
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
           g_num_registro                    USING  "&&&&&&&&&"         , #id_02
           30      SPACES                                               , #id_03
           g_sumario[4].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_04
           g_sumario[4].pesos      * 100      USING "&&&&&&&&&&&&&&&",    #id_05
           12      SPACES                         ,                       #id_06
           g_sumario[14].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_07
           g_sumario[14].pesos     * 100      USING "&&&&&&&&&&&&&&&",    #id_08
           g_sumario[8].acciones   * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_09
           g_sumario[8].pesos      * 100      USING "&&&&&&&&&&&&&&&",    #id_10
           g_sumario[35].acciones  * 1000000  USING "&&&&&&&&&&&&&&&&&&", #id_11
           g_sumario[35].pesos     * 100      USING "&&&&&&&&&&&&&&&",    #id_12
           207     SPACES                         ,                       #id_13
           g_sum_rcv                                             , #id_14_al_51
           15      SPACES                                                #id_52

END REPORT

FUNCTION    F_250_genera_archivo_saldos()
   LET      g_cat          =
            "cat ",g_seg_modulo.ruta_envio       CLIPPED,"/CZA_09 ",
            g_seg_modulo.ruta_envio              CLIPPED,"/DET_09 ",
            g_seg_modulo.ruta_envio              CLIPPED,"/SUM_09 > ",
            g_seg_modulo.ruta_envio              CLIPPED,"/","TRASP_SALDOS.",
            reg_taa_cd_ctr_folio.tipo_traspaso   USING   "#",".",
            g_today                              USING   "MMDD","-",
            g_hora[1,2],g_hora[4,5]
   RUN      g_cat
END FUNCTION 

FUNCTION    F_240_genera_reporte_saldos()
   LET      g_comando            = 
            g_seg_modulo.ruta_listados           CLIPPED, "/",
            g_usuario                            CLIPPED ,".TOTAL_TRA.",
            reg_taa_cd_ctr_folio.tipo_traspaso   USING  "#",".",
            g_today    USING    "DDMMYY","-", g_hora[1,2],g_hora[4,5]  CLIPPED
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
       PRINT  COLUMN  02,"TCAAB005",
	      COLUMN  13,'\033(s7B',"RESUMEN DE TRASPASOS DE SALDOS",
                       "  AFORE (CEDENTE)",
              COLUMN  64,"FECHA:",g_today USING "dd-mm-yyyy",'\033(s0B'  CLIPPED
       PRINT  COLUMN  02,g_afore_cod,"     ",g_raz_social
       PRINT  COLUMN  02,'\033(s7B',"FOLIO   : ",reg_taa_cd_ctr_folio.folio
                      USING     "########",'\033(s0B'    CLIPPED 
       PRINT  COLUMN  02,"ARCHIVO : " ,'\033(s7B',"TRASP_SALDOS.",
                      reg_taa_cd_ctr_folio.tipo_traspaso    USING    "#",".",
                      g_today    USING   "MMDD","-",
                      g_hora[1,2],g_hora[4,5],'\033(s0B'    CLIPPED
       PRINT  COLUMN  02,"RUTA    : /safre_prc/taa/envio"
       PRINT  COLUMN  02,"FECHA DE PRESENTACION : ",g_today USING "DD/MM/YYYY"
   ON EVERY ROW
       SKIP     2     LINE
       PRINT  COLUMN  02,"__________________________________________________",
                         "______________________________"
       PRINT  COLUMN  05,'\033(s7B',"TOTALES DE VIVIENDA  VALOR",
                         " PARTICIPACION : ", g_precio_parti 
                           USING    "##.##############",'\033(s0B'   CLIPPED
       PRINT  COLUMN  30, "MONTO EN PARTICIPACIONES",
              COLUMN  65, "MONTO EN PESOS"
       PRINT  COLUMN  02,"__________________________________________________",
                         "______________________________"
       PRINT  COLUMN  01,g_sumario[04].nom_scta,
              COLUMN  35,g_sum_rep[04,1].acciones  USING  "####,###,###.######",
              COLUMN  60,g_sum_rep[04,1].pesos     USING  "####,###,###.######"
       PRINT  COLUMN  01,g_sumario[14].nom_scta,
              COLUMN  35,g_sum_rep[14,1].acciones  USING  "####,###,###.######",
              COLUMN  60,g_sum_rep[14,1].pesos     USING  "####,###,###.######"
       PRINT  COLUMN  01,g_sumario[08].nom_scta,
              COLUMN  35,g_sum_rep[08,1].acciones  USING  "####,###,###.######",
              COLUMN  60,g_sum_rep[08,1].pesos     USING  "####,###,###.######"
       PRINT  COLUMN  01,g_sumario[35].nom_scta,
              COLUMN  35,g_sum_rep[35,1].acciones  USING  "####,###,###.######",
              COLUMN  60,g_sum_rep[35,1].pesos     USING  "####,###,###.######"


       FOR g_siefore = g_siefore_ini TO g_siefore_fin
             IF (g_siefore <> 1 AND 
                 g_siefore <> 2 AND
                 g_siefore <> 3 AND 
                 g_siefore <> 4 AND
                 g_siefore <> 5 AND
                 g_siefore <> 6 ) THEN
                 CONTINUE FOR
              END IF
                
              FOR g_scta = g_scta_ini TO g_scta_fin
               IF (g_scta = 4     OR
                   g_scta = 8     OR
                   g_scta = 14    OR
                   g_scta = 35  ) THEN
                   CONTINUE  FOR
                END IF

                IF g_scta = 1 THEN
                         SKIP 2 LINE
	                 PRINT    COLUMN   02,"_______________________________",
                                           "__________________________________",
                                           "_______________"
                         PRINT    COLUMN   05,'\033(s7B',
                                  "TOTALES SALDOS DE RCV SIEFORE BASICA ",
                                  g_siefore    USING   "##"," ","VALOR ACCION :",
                                  g_sie_inf[g_siefore].precio_accion,
                                  '\033(s0B' CLIPPED
                         PRINT    COLUMN   37,"MONTO EN ACCIONES",
                                  COLUMN   65,"MONTO EN PESOS"
	                 PRINT    COLUMN   02,"_______________________________",
                                           "_________________________________",
                                           "________________"
                END IF
                IF g_siefore = 6 THEN
                        IF (g_scta <> 3  AND
                            g_scta <> 10 AND
                            g_scta <> 11 AND
                            g_scta <> 12 AND
                            g_scta <> 13 AND
                            g_scta <> 15) THEN
                            CONTINUE  FOR
                         END IF
                END IF
                PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                         COLUMN   35,g_sum_rep[g_scta,g_siefore].acciones
                                  USING    "####,###,###.######",
                         COLUMN   60,g_sum_rep[g_scta,g_siefore].pesos
                                  USING    "####,###,###.######"
              END FOR
       END FOR
       PRINT    COLUMN   02,"________________________________________________",
                            "________________________________"
       SKIP     TO    TOP   OF  PAGE
       SKIP     2     LINE
       PRINT    COLUMN   10, '\033(s7B', "TOTALES DEL ARCHIVO DE SALDOS",
                COLUMN   61, "MONTO EN PESOS",'\033(s0B' CLIPPED
       PRINT    COLUMN   02,"________________________________________________",
                            "________________________________"
       SKIP     2     LINE
       PRINT    COLUMN   01,g_sumario[04].nom_scta,
                COLUMN   60,g_sumario[04].pesos       USING   "####,###,###.##"
       PRINT    COLUMN   01,g_sumario[08].nom_scta,
                COLUMN   60,g_sumario[08].pesos       USING   "####,###,###.##"
       PRINT    COLUMN   01,g_sumario[14].nom_scta,
                COLUMN   60,g_sumario[14].pesos       USING   "####,###,###.##"
       PRINT    COLUMN   01,g_sumario[35].nom_scta,
                COLUMN   60,g_sumario[35].pesos       USING   "####,###,###.##"

       FOR      g_scta            =  g_scta_ini       TO  g_scta_fin
                IF       g_scta              =  4     OR
                         g_scta              =  8     OR
                         g_scta              =  6     OR
                         g_scta              =  9     OR
                         g_scta              =  14    OR
                         g_scta              =  35    THEN
                         CONTINUE  FOR
                END IF
                PRINT    COLUMN   1,g_sumario[g_scta].nom_scta," ",
                         COLUMN   60,g_sumario[g_scta].pesos
                                  USING    "####,###,###.##"
       END FOR
       PRINT    COLUMN   02,"________________________________________________",
                            "________________________________"
END REPORT
