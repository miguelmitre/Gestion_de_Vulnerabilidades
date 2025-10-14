############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TCAAB017L=> CLASIFICA  CUENTAS PARA TRASPASO COMPLEMENTARIO      #
#Sistema           => TCAA                                                 #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                       #
#Elaborado         => 25 Enero 2010                                        #
############################################################################
DATABASE     safre_af
GLOBALS
   DEFINE    g_current                         DATETIME YEAR TO SECOND
   DEFINE 
             g_fecha_liq_parti                 ,
             g_fecha_envio_saldos              ,
             g_fecha_saldo_parti               DATE
   DEFINE 
             g_siefore                         ,
             g_scta                            ,
             g_liquidada                       ,
             g_provisionado                    ,
             cero                              ,  
             g_cred_en_garantia                ,
             g_credito_fovissste               ,
             g_con_saldo                       ,
             g_si_hay_comple                   SMALLINT
   DEFINE
             g_acciones                        DEC(18,6) ,
             g_pesos                           DEC(16,6),
             g_imp_2_dec                       DEC(15,2),
             g_monto_minimo                    DEC(15,2)
   DEFINE    
             g_enter                           CHAR(001),
             g_hora                            CHAR(005), 
             g_comando                         CHAR(100),
             g_afore_cod                       CHAR(003),
             g_raz_social                      CHAR(050),
             g_usuario                         CHAR(008) 

   DEFINE    g_sie_inf      ARRAY[50]      OF   RECORD
             precio_accion                     DEC(25,14)
                                          END  RECORD

   DEFINE    reg_taa_cd_det_cedido  RECORD  LIKE  safre_af:taa_cd_det_cedido.*
   DEFINE    g_seg_modulo           RECORD  LIKE  seg_modulo.*
END GLOBALS

GLOBALS  "TCAAB017S.4gl"   #  ARMA LOS QUERYS PARA EL PROCESO

MAIN
   OPTIONS
            PROMPT   LINE LAST,
            ACCEPT   KEY CONTROL-I
            DEFER    INTERRUPT
   CALL     F_010_inicio()
   CALL     F_100_proceso()
END MAIN
 
FUNCTION    F_010_inicio()
   LET      g_fecha_envio_saldos                     =  ARG_VAL(1)
   LET      g_fecha_liq_parti                        =  ARG_VAL(2)
   LET      g_fecha_saldo_parti                      =  ARG_VAL(3)
   LET      g_fecha_trasp_desde                      =  ARG_VAL(4)
   LET      g_today                                  =  ARG_VAL(5)
   CALL     STARTLOG("TCAAB017L.log")
   CALL     arma_querys_TCAAB017S()
   CALL     F_015_prepara_querys_TCAAB017S()
   LET      g_current                      =  CURRENT
   DISPLAY  " "
   DISPLAY  ".1"
   DISPLAY  g_current,": TCAAB017L: "
   DISPLAY  g_current,": CLASIFICA CUENTAS PARA TARSPASO COMPLEMENTARIO CEDENTE"
   DISPLAY  g_current,": A PARTIR DE  (ddmmaa)  ==> ",
            g_fecha_trasp_desde    USING   "dd/mm/yyyy"
   LET      cero                           =  0
   LET      g_hora                         =  TIME
   LET      g_si_hay_comple                =  cero
   CALL     F_020_trae_parametros()
   CALL     F_030_muestra_folio_comple()
END FUNCTION

FUNCTION    F_015_prepara_querys_TCAAB017S()
   PREPARE  sql_02               FROM  g_sql_02
   PREPARE  sql_06               FROM  g_sql_06
   PREPARE  sql_07               FROM  g_sql_07
   PREPARE  sql_07_2             FROM  g_sql_07_2
   PREPARE  sql_08               FROM  g_sql_08
   PREPARE  sql_10               FROM  g_sql_10
   PREPARE  sql_12               FROM  g_sql_12
   PREPARE  sql_14               FROM  g_sql_14
   PREPARE  sql_18               FROM  g_sql_18
   PREPARE  sql_19               FROM  g_sql_19
   PREPARE  sql_20               FROM  g_sql_20
   PREPARE  sql_42               FROM  g_sql_42   # nuevo para saldos
   PREPARE  sql_45               FROM  g_sql_45   # monto de credito en garantia
END FUNCTION

FUNCTION    F_020_trae_parametros()
   DEFINE   l_folios_pendientes                 SMALLINT
   DEFINE   l_tab_subcuenta      RECORD  LIKE safre_af:tab_subcuenta.*
   EXECUTE  sql_08        INTO  g_monto_minimo  
   EXECUTE  sql_10        INTO  g_seg_modulo.*
END FUNCTION

FUNCTION    F_030_muestra_folio_comple()
   DEFINE   l_cad_desc_rech                   CHAR(100),
            l_registros                       INTEGER
   CALL     F_035_precio_acc_parti()
   LET      g_current        =   CURRENT
   DISPLAY  g_current," :FECHA DE ENVIO DE SALDOS          : ",
            g_fecha_envio_saldos
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
   EXECUTE  sql_06              USING  g_fecha_trasp_desde
                                 INTO  l_registros
   LET      g_current               =  CURRENT
   DISPLAY  g_current," :NUMERO DE REGISTROS A PROCESAR    : ",
            l_registros   USING  "######"
END FUNCTION

FUNCTION    F_035_precio_acc_parti()
   DEFINE   l_siefore_rcv                     ,
            l_siefore_viv                     SMALLINT
   DEFINE   l_nom_siefore                     CHAR(008),
            l_precio_accion                   DEC(19,14)
   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie      USING    g_today
   FOREACH  cur_sie       INTO    g_siefore,l_precio_accion
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH
   DECLARE  cur_sie_viv      CURSOR   FOR   sql_07_2
   OPEN     cur_sie_viv       USING   g_fecha_liq_parti
   FOREACH  cur_sie_viv        INTO   g_siefore,l_precio_accion
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH
END FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_regs_proc                    ,
            l_leidos                       INTEGER,
            l_sin_marca                    SMALLINT
   LET      l_leidos                       =  cero 
   LET      l_regs_proc                    =  cero
   LET      g_con_saldo                    =  cero
   LET      g_si_hay_comple                =  cero
########         GRABA  FOLIO       #############
   LET      g_current              =  CURRENT
   DISPLAY  g_current," :PROCESANDO  INFORMACION ...... "
   DECLARE  c_cedidos     CURSOR   FOR  sql_14
   OPEN     c_cedidos      USING   g_fecha_trasp_desde
   FOREACH  c_cedidos       INTO   reg_taa_cd_det_cedido.*
            LET      l_leidos                    =  l_leidos    +   1
            CALL     F_130_checa_saldos()
            IF       g_con_saldo           THEN
                     INSERT   INTO  safre_tmp:taa_cd_op_12 
                            VALUES (reg_taa_cd_det_cedido.n_seguro,
                                    g_fecha_envio_saldos,
                                    101)
                     LET      g_si_hay_comple           =  1
            END  IF
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
   IF       g_si_hay_comple         =  cero   THEN
            DISPLAY  "Program stopped"
            DISPLAY  "ERROR NO HAY TRASPASOS COMPLEMENTARIOS "
            EXIT     PROGRAM
   END IF
END FUNCTION

FUNCTION    F_130_checa_saldos()
   DEFINE   l_scta                         SMALLINT
   DEFINE   l_acc_ret_92                   ,
            l_pesos                        DECIMAL(16,6)
   LET      g_con_saldo                    =  cero
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
                     g_scta                  =  14      OR
                     g_scta                  =  35      OR 
                     g_scta                  =  36 )    THEN
                     CALL     F_140_arma_saldo_viv()
            END IF
            LET      l_scta                  =  0
            EXECUTE  Sql_02
                     USING   g_scta
                     INTO    l_scta     ##    checa  subcuenta  agrupada
            IF       l_scta                  >  0    THEN
                     EXECUTE   sql_12
                       USING   reg_taa_cd_det_cedido.n_seguro ,
                               g_today                        ,
                               l_scta,
                               g_siefore
                     INTO      l_acc_ret_92
                     LET       l_pesos       =
                               l_acc_ret_92  *  g_sie_inf[g_siefore].precio_accion
            ELSE
                     LET       l_scta                  =  g_scta
                     LET       l_pesos                 =  g_pesos
            END IF
            IF       l_pesos              >=  g_monto_minimo THEN
                     LET        g_con_saldo            =  1
                     EXIT  FOREACH
            END IF
   END FOREACH 
END FUNCTION

FUNCTION    F_140_arma_saldo_viv()
   DEFINE   l_f_ini_cr_garantia                 DATE
   DEFINE   l_fecha                             DATE
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
                              LET      g_cred_en_garantia        =  '2'
            END IF
   END IF
   #--- CHECA  SI HAY CREDITO  EN GARANTIA  FOVISSSTE  -----
   LET      g_credito_fovissste           =  0
   LET      l_f_ini_cr_garantia           =  NULL
   EXECUTE  sql_20      USING    reg_taa_cd_det_cedido.n_seguro
                         INTO    l_f_ini_cr_garantia
   IF       l_f_ini_cr_garantia          IS  NOT  NULL   THEN
            LET      g_credito_fovissste       =  1
   END IF

   LET      g_imp_2_dec        =
            (g_acciones        *  g_sie_inf[g_siefore].precio_accion)
   LET      g_pesos            =  g_imp_2_dec
   IF       g_scta             =  4      THEN
            IF       g_cred_en_garantia             = '1'  THEN
                     CALL      F_141_resta_credito_en_garantia()
                     LET       g_imp_2_dec       =  g_acciones
	   	     LET       g_acciones        =  g_imp_2_dec
            END  IF
   END   IF
   IF       g_scta             =  14     OR
            g_scta             =  35     THEN
            IF       g_credito_fovissste            =  1   THEN
                     CALL      F_141_resta_credito_en_garantia()
            END  IF
   END   IF
END FUNCTION

FUNCTION    F_141_resta_credito_en_garantia()
   DEFINE   l_acciones                          ,
            l_pesos                             DECIMAL(16,6)
   LET      l_acciones                  =  cero
   EXECUTE  sql_45
            USING   reg_taa_cd_det_cedido.n_seguro,
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

