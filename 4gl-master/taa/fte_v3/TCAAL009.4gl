##########################################################################
#Programa   :TCAAL009                                                    #
#Objetivo   :Genera Reporte de Indebidos.                                #
#Autor      :José Francisco Lugo Cornejo                                 #
#Elaborado  :9/Octubre/2007.                                             #
##########################################################################
DATABASE   safre_af

GLOBALS
   DEFINE    g_fecha_recep_cta                   ,
             g_today                             ,
             g_fecha_valor                       ,
             g_fecha_trasp_cta                   DATE
   DEFINE    g_folio                             INTEGER
   DEFINE    g_nss                               CHAR(11)
   DEFINE    g_afore_cod                         CHAR(003)
   DEFINE    g_usuario                           CHAR(008)
   DEFINE    g_raz_social                        CHAR(050)
   DEFINE    g_pesos_rendimiento                 DEC(16,6)
   DEFINE    g_enter                             CHAR(1)
   DEFINE    g_linea                             CHAR(130) 
   DEFINE    g_sie           ARRAY[50]      OF    RECORD
             nom_siefore                         CHAR(008),
             precio_accion                       DEC(11,6)
                                           END   RECORD
   DEFINE    scta                         ,
             g_tipo_proceso               ,
             sie                                 SMALLINT
   DEFINE    g_seg_modulo       RECORD  LIKE  safre_af:seg_modulo.*
END GLOBALS

MAIN
   CALL     f_010_inicio()
   CALL     f_100_proceso()
END MAIN

FUNCTION    f_010_inicio()
   LET      g_tipo_proceso         =  ARG_VAL(1)
   LET      g_folio                =  ARG_VAL(2)
   LET      g_today                =  TODAY
   SELECT   codigo_afore,razon_social
     INTO   g_afore_cod,g_raz_social
     FROM   tab_afore_local ;

   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod                  =  "taa" ;
END FUNCTION

FUNCTION    f_100_proceso()
   DECLARE  cur_nss_nde            CURSOR  FOR
   SELECT   nss,b.fecha_trasp
     FROM   safre_af:taa_cd_indebidos  a,  safre_af:taa_cd_det_cedido  b
    WHERE   a.folio                    =  g_folio
      AND   a.nss                      =  b.n_seguro
      AND   b.tipo_traspaso            IN ("21","73") # CPL-1285
      AND   b.estado                   =  103 ;
   FOREACH  cur_nss_nde             INTO  g_nss,
                                          g_fecha_trasp_cta
            CREATE   TEMP  TABLE   taa_cd_rep_inde(registro CHAR   (130));
            CALL     f_103_trae_fecha_recep_cta()
            CALL     f_105_precios_accion()
            CALL     f_110_arma_detalle_calculo()
            CALL     f_120_arma_resarcimiento()
            CALL     f_130_arma_resumen()
            CALL     f_140_imprime_archivo()
            DROP     TABLE      taa_cd_rep_inde;
   END FOREACH
END FUNCTION

FUNCTION    f_103_trae_fecha_recep_cta()
   DEFINE   l_fecha_mov_banxico                 DATE
   LET      g_fecha_recep_cta            =  NULL
   SELECT   MAX(A.fecha_mov_banxico)
     INTO   l_fecha_mov_banxico
     FROM   safre_af:taa_rcv_recepcion  A
    WHERE   A.nss                        =  g_nss
      AND   A.ident_operacion            = "09";

   SELECT   MAX(A.fecha_calculo_rendi)
     INTO   g_fecha_recep_cta
     FROM   safre_af:taa_cd_his_rendi_dia  A
    WHERE   A.nss                        =  g_nss
      AND   A.folio                     <>  g_folio;
   IF       g_fecha_recep_cta                IS  NULL             OR
            g_fecha_recep_cta                 <  "12/31/1998"     THEN
            LET      g_fecha_recep_cta        =  l_fecha_mov_banxico
   END IF
END FUNCTION

FUNCTION    f_105_precios_accion()
   DEFINE   l_nom_siefore                     CHAR(008),
            l_precio_accion                   DEC(11,6)
   IF       g_tipo_proceso          =  1      THEN
            LET      g_fecha_valor  =  g_fecha_trasp_cta
   ELSE
            LET      g_fecha_valor  =  g_today
   END IF
   FOR      sie      =  1         TO  50
            LET      g_sie[sie].precio_accion         =  0
   END FOR
   DECLARE  cur_sie     CURSOR    FOR
   SELECT   a.codigo_siefore,b.siefore_desc,a.precio_del_dia
     FROM   glo_valor_accion  a, tab_siefore  b
    WHERE   a.fecha_valuacion           =  g_fecha_valor
      AND   a.codigo_siefore           IN(1,2,3)
      AND   b.afore_cod                 =  g_afore_cod
      AND   a.codigo_siefore            =  b.siefore_cod;
   FOREACH  cur_sie       INTO    sie,l_nom_siefore,l_precio_accion
            LET      g_sie[sie].nom_siefore     =  l_nom_siefore  CLIPPED
            LET      g_sie[sie].precio_accion   =  l_precio_accion
   END FOREACH
END FUNCTION

FUNCTION    f_110_arma_detalle_calculo()
   DEFINE   l_rg       RECORD  LIKE  taa_cd_his_rendi_dia.*
   LET      g_linea                  =
           "NSS:",g_nss                                         ,4  SPACES,
           "FECHA_RECEPTORA: ",g_fecha_recep_cta USING "DD-MM-YYYY" ,3  SPACES,
           "FECHA_CEDENTE   : ",g_fecha_trasp_cta   USING "DD-MM-YYYY" 
   CALL    imprime_linea()
   LET      g_linea                  =
           "SCTA    F_CAL_REND    AFO_MAX_REND   SIE_MAX_REND    MAX_RENDIMIENTO   PORC_COMISION     FACTOR_COMISION        SALDO_ACUMULADO"
   CALL     imprime_linea()
   LET      scta                     =  0
   LET      sie                      =  0
   DECLARE  cur_his_rendi_dia        CURSOR   FOR
    SELECT  *
      FROM  safre_af:taa_cd_his_rendi_dia
     WHERE  folio                    =  g_folio
       AND  nss                      =  g_nss
     ORDER  BY  subcuenta,fecha_calculo_rendi;
   FOREACH  cur_his_rendi_dia     INTO   l_rg.*
      IF      scta                >  0    THEN
              IF      l_rg.subcuenta        <>  scta     THEN
                      CALL     f_120_arma_resarcimiento()
              END IF
      END IF
      LET     scta                     =  l_rg.subcuenta
      LET     sie                      =  l_rg.siefore_max_endi
      LET     g_pesos_rendimiento      =  l_rg.pesos_rendimiento
      LET     g_linea                  =
              l_rg.subcuenta           USING "#&"                 , 6 SPACES,
              l_rg.fecha_calculo_rendi USING "DD-MM-YYYY"         , 4 SPACES,
              l_rg.afore_max_rendi     USING "###"                ,12 SPACES,
              l_rg.siefore_max_endi    USING "#"                  ,12 SPACES,
              l_rg.factor_porcentaje   USING "#&.&&&&&&&&&&&&&&&" , 7 SPACES,
              l_rg.max_rendimiento     USING "#&.&&&&&&"          , 7 SPACES,
              l_rg.factor_comision     USING "#&.&&&&&&&&&&"      , 6 SPACES,
              l_rg.pesos_rendimiento   USING "--,---,--&.&&&&&&" 
      CALL    imprime_linea()
   END  FOREACH

END FUNCTION

FUNCTION    imprime_linea()
   INSERT   INTO    taa_cd_rep_inde   VALUES (g_linea);
END FUNCTION

FUNCTION    f_120_arma_resarcimiento()
   DEFINE   
            l_sdo_trasp                        ,
            l_acc_resarcir                     ,
            l_pesos_resarcir                   ,
            l_val_accion                       ,
            l_tot_resarcir                     DEC(16,6)
   SELECT   codigo_siefore
     INTO   sie
     FROM   safre_af:cta_regimen
    WHERE   nss                     =  g_nss
      AND   subcuenta               =  scta;
   SELECT   SUM(monto_en_pesos * -1)
     INTO   l_sdo_trasp
     FROM   safre_af:dis_cuenta
    WHERE   nss                     =  g_nss
      AND   subcuenta               =  scta
      AND   fecha_conversion        =  g_fecha_trasp_cta
      AND   tipo_movimiento       IN(SELECT  b.marca_cod
                                       FROM  safre_af:taa_cd_tipo_traspaso b);
   LET      l_tot_resarcir          =  g_pesos_rendimiento     -  l_sdo_trasp
   LET      g_linea                 =  NULL
   CALL     imprime_linea()
   LET      g_linea                 =
            "REGIMEN SUBCUENTA: ",scta            USING  "#&   ",
            g_sie[sie].nom_siefore,
            "   PRECIO AL: ",g_fecha_valor        USING  "DD-MM-YYYY",": ",
            g_sie[sie].precio_accion    USING  "#&.&&&&&&"
   CALL     imprime_linea()
   LET      g_linea                 =
            12    SPACES,
           "SALDO_MAX_RENDIMIENTO",     9  SPACES,
           "TRASPASO CEDENTE",          8  SPACES,
           "MONTO A RESARCIR" 
   CALL     imprime_linea()
   LET      g_linea                 =
           "PESOS ",
            11    SPACES,
            g_pesos_rendimiento    USING   "#####,##&.&&&&&&" ,09  SPACES,
            l_sdo_trasp            USING   "#####,##&.&&&&&&" ,08  SPACES,
            l_tot_resarcir         USING   "#####,##&.&&&&&&" 
   CALL     imprime_linea()
   IF       g_tipo_proceso          =  1     THEN
            LET      l_acc_resarcir    =
                     l_tot_resarcir    /  g_sie[sie].precio_accion
            LET      g_linea                 =
                     "ACCIONES A RESARCIR:",
                     l_acc_resarcir    USING   "#####,##&.&&&&&&"
   ELSE
            SELECT   monto_en_acciones,monto_en_pesos
              INTO   l_acc_resarcir,l_pesos_resarcir
              FROM   safre_af:dis_cuenta
             WHERE   folio              =  g_folio
               AND   nss                =  g_nss
               AND   subcuenta          =  scta
               AND   siefore            =  sie;
            LET      g_linea                 =
                     "ACCIONES RESARCIDAS:",
                     l_acc_resarcir      USING   "#####,##&.&&&&&&",
                     13     SPACES,
                     "PESOS RESARCIDOS:",
                     l_pesos_resarcir    USING   "#####,##&.&&&&&&"
   END IF
   CALL     imprime_linea()
   LET      g_linea                 =  NULL
   CALL     imprime_linea()
END FUNCTION

FUNCTION    f_130_arma_resumen()
   DEFINE   l_sql_saldo                  CHAR(500)
   DEFINE   l_tab_saldo                  CHAR(50)
   DEFINE   l_acciones                   ,
            l_val_acc                    ,
            l_pesos                      DEC(16,6)
   DEFINE   l_max_subct                  SMALLINT

   DEFINE
            l_imp           ARRAY[50,50]   OF    RECORD
            acc                                 DEC(16,6),
            pesos                               DEC(16,6)
                                          END   RECORD
   LET      l_val_acc             =  0
   FOR      scta   =  1          TO   16
            FOR       sie         =  1        TO  50
                      LET       l_imp[scta,sie].acc           =  0
                      LET       l_imp[scta,sie].pesos         =  0
            END FOR
   END FOR
   LET      g_linea                 =
           "RESUMEN DEL NSS: ",g_nss
   CALL     imprime_linea()
   LET      g_linea                 =
            25          SPACES,
            "SB1",36    SPACES,
            "SB2",36    SPACES,
            "SB3"
   CALL     imprime_linea()
   LET      g_linea                 =
            10  SPACES,
           "PRECIO AL : ",g_fecha_valor    USING  "DD-MM-YYYY","  ",
            g_sie[1].precio_accion   USING  "#&.&&&&&&",06  SPACES,
           "PRECIO AL : ",g_fecha_valor    USING  "DD-MM-YYYY","  ",
            g_sie[2].precio_accion   USING  "#&.&&&&&&",06  SPACES,
           "PRECIO AL : ",g_fecha_valor    USING  "DD-MM-YYYY","  ",
            g_sie[3].precio_accion   USING  "#&.&&&&&&"
   CALL     imprime_linea()
   LET      g_linea                 =
           "SCTA.       ",
           "ACCIONES",15 SPACES,"PESOS",12  SPACES,
           "ACCIONES",15 SPACES,"PESOS",12  SPACES,
           "ACCIONES",15 SPACES,"PESOS"
   CALL     imprime_linea()
   IF       g_tipo_proceso                 =  1    THEN
            LET      l_tab_saldo           =  " FROM   safre_af:dis_provision "
   ELSE
            LET      l_tab_saldo           =  " FROM   safre_af:dis_cuenta "
   END  IF

   LET      l_sql_saldo             = 
           'SELECT   subcuenta,siefore,',
           '         SUM(monto_en_acciones),SUM(monto_en_pesos) ',l_tab_saldo,
           ' WHERE   folio                   =  ',g_folio,
           '   AND   nss                     =  ','"',g_nss,'"',
           ' GROUP   BY   1,2 ',
           ' ORDER   BY   1,2; '
   PREPARE  sql_saldo             FROM  l_sql_saldo
   DECLARE  cur_resumen         CURSOR  FOR  sql_saldo
   FOREACH  cur_resumen           INTO  scta,sie,l_acciones,l_pesos
            LET     l_imp[scta,sie].acc    =  l_acciones
            LET     l_imp[scta,sie].pesos  =  l_pesos
            LET     l_imp[16,sie].acc      =  l_imp[16,sie].acc   + l_acciones
            LET     l_imp[16,sie].pesos    =  l_imp[16,sie].pesos + l_pesos
   END  FOREACH
   
   #CPL-1285 Obteniendo el máximo de subcuentas
   SELECT MAX(subct_cod)
   INTO l_max_subct 
   FROM tab_subcuenta
   
   FOR      scta     =  1                  TO   l_max_subct
            IF        scta        =  4     OR
                      scta        =  8     OR
                      scta        =  14    THEN
                      CONTINUE  FOR
            END IF
            LET      g_linea            =
                     scta                 USING "#&"             CLIPPED,"    ",
                     l_imp[scta,1].acc    USING "###,##&.&&&&&&" ,6  SPACES,
                     l_imp[scta,1].pesos  USING "###,##&.&&&&&&" ,6  SPACES,
                     l_imp[scta,2].acc    USING "###,##&.&&&&&&" ,6  SPACES,
                     l_imp[scta,2].pesos  USING "###,##&.&&&&&&" ,6  SPACES,
                     l_imp[scta,3].acc    USING "###,##&.&&&&&&" ,6  SPACES,
                     l_imp[scta,3].pesos  USING "###,##&.&&&&&&" 
            IF       scta                       =  16    THEN
                     LET     g_linea[1,5]       =  "Total"
            END IF
            CALL     imprime_linea()
   END FOR
END FUNCTION

FUNCTION    f_140_imprime_archivo()
   DEFINE   l_cont                     SMALLINT
   DEFINE   l_comando                  CHAR(200)
   DEFINE   l_aux              CHAR(10)
   DEFINE   l_folio            CHAR(10)
   LET      l_comando                   =
            g_seg_modulo.ruta_listados     CLIPPED, "/", g_usuario  CLIPPED ,
#           "/safre/taa/fte_v3/indebidos/", g_usuario  CLIPPED ,
            ".CAL_RES.",g_tipo_proceso  USING "&",".",
             g_TODAY USING "DDMMYY" CLIPPED ,".",
            g_nss  CLIPPED

   START    REPORT     R_220_rep_saldos_inde      TO  l_comando
   DECLARE  cur_rep     CURSOR     FOR  
   SELECT   *
     FROM   taa_cd_rep_inde
   FOREACH  cur_rep           INTO  g_linea
        OUTPUT     TO    REPORT  R_220_rep_saldos_inde()
   END  FOREACH
   FINISH   REPORT     R_220_rep_saldos_inde

END FUNCTION

REPORT    R_220_rep_saldos_inde()
   DEFINE    l_tipo_proceso          CHAR(12)
   OUTPUT
            PAGE     LENGTH   66
            LEFT     MARGIN   0
            RIGHT    MARGIN   132
            TOP      MARGIN   0
            BOTTOM   MARGIN   0

   FORMAT
   PAGE     HEADER
       IF         g_tipo_proceso             =  1   THEN
                  LET      l_tipo_proceso       =  "PROVISION"
       ELSE
                  LET      l_tipo_proceso       =  "LIQUIDACION"
       END IF
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
       PRINT  COLUMN  71,"Pagina:",PAGENO USING "<<<<"
       PRINT  COLUMN  02,"TCAAL009",
              COLUMN  13,'\033(s7B',"RESUMEN DE ",l_tipo_proceso CLIPPED,
                         " DE CALCULO PARA RESARCIMIENTO DE INDEBIDOS",
                       "  AFORE (CEDENTE)",
              COLUMN  64,"FECHA:",g_today USING "dd-mm-yyyy",'\033(s0B'  CLIPPED
       PRINT  COLUMN  02,g_afore_cod,"     ",g_raz_social
       PRINT  COLUMN  02,'\033(s7B',"FOLIO   : ",g_folio
                      USING     "########",'\033(s0B'    CLIPPED

   ON  EVERY  ROW
       PRINT    g_linea

END REPORT
   
