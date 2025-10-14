#######################################################################
#PROGRAMA:SEPC003                                                     #
#OBJETIVO:SEPARA MONTOS DE CUENTA INVADIDA Y RECLAMANTE POR PORCENTAJE#
#AUTOR:JOSE FRANCISCO LUGO CORNEJO                                    #
#DESARROLLADO :  25/SEP/2009                                          #
#######################################################################
DATABASE   safre_af
GLOBALS  
   DEFINE  g_nss_inv                        CHAR(11),
           g_nss_recla                      CHAR(11),
           g_sql                            CHAR(500),
           g_tabname                        CHAR(30),
           g_usuario                        CHAR(08),
           g_today                          DATE,
           g_f_conver                       DATE,
           g_enter                          CHAR,
           g_arr_curr                       ,
           g_scr_line                       ,
           g_reg                            ,
           g_arr_count                      ,
           i                                ,
           g_lastkey                        ,
           g_tot_reg                        SMALLINT,
           g_folio_sep                      DEC(10,0),
           g_folio_trasp                    DEC(10,0)


   DEFINE   g_mv          ARRAY[100]   OF   RECORD
            porc_sep                        DEC(5,2),
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD

   DEFINE   g_sie           ARRAY[50]      OF    RECORD  --siefore
            precio_accion                       DEC(19,14)
                                          END   RECORD
END  GLOBALS
   DEFINE   reg_cta          RECORD   LIKE   dis_cuenta.*
   DEFINE   g_current               DATETIME   YEAR  TO  SECOND
MAIN
   CALL STARTLOG ("SEPC003.log")
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
   CALL     F_010_inicio()
   CALL     F_100_proceso()
END MAIN

FUNCTION    F_010_inicio()
   DEFINE   l_registros                  ,
            l_no_existe                  ,
            l_separado                   SMALLINT
   DEFINE   l_ano_tra                    ,
            l_ano_act                    CHAR(04)
   DEFINE   l_reg_cta     RECORD         LIKE   dis_cuenta.*
   DEFINE   l_f_banxico                  DATE
   LET      g_today                      =   TODAY
   LET      g_reg                        =  1
   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "SEPC003"  ATTRIBUTE(BORDER)
   DISPLAY  "<<SEPC003>>            SEPARACION  DE  CUENTAS            ",
     g_today  USING  "DD/MM/YYYY", "         "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  " SEPARA MONTOS DE TRASPASO POR PORCENTAJE DE CUENTAS INVADIDA Y RECLAMANTE ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  " <CTRL-C>     PARA  CANCELAR     O   <ESC>  PARA  SEPARAR  " AT 2,2
   INPUT  BY  NAME  g_nss_inv
         AFTER   FIELD   g_nss_inv
         IF      g_nss_inv                     IS  NULL   THEN
                 ERROR  " NSS  NO PUEDE SER NULO   "
                 NEXT  FIELD    g_nss_inv
         ELSE
                 SELECT   folio,nss         INTO    g_folio_sep,g_nss_recla
                   FROM   safre_af:sep_det_reg_sol_reclamante
                  WHERE   n_seguro                    =  g_nss_inv
                   AND    estado                     IN(8);
                 IF       g_folio_sep                IS  NULL   OR
                          g_folio_sep                 =  0     THEN
                          ERROR   "   NO EXISTE CUENTA PARA SEPARAR IMPORTES"
                          LET      g_nss_inv          =  NULL
                          NEXT  FIELD    g_nss_inv
                 END  IF
                 DISPLAY  g_nss_recla                TO   g_nss_recla
                 DISPLAY  g_folio_sep                TO  folio
                 CALL     F_080_trae_nombres()
         END IF
         ON      KEY    (ESC)
                 IF      g_nss_inv                     IS  NULL   THEN
                         ERROR  " NSS  NO PUEDE SER NULO   "
                         NEXT  FIELD    g_nss_inv
                 END IF
                 EXIT   INPUT
         ON KEY ( INTERRUPT )
                 EXIT PROGRAM
   END INPUT
   DISPLAY  "   PROCESANDO  INFORMACION....."    AT  22,2
   LET       g_enter                       =  0
   LET       g_tabname                     =  "dis_cuenta"
   SELECT    MAX(folio),user
     INTO    g_folio_trasp,g_usuario
     FROM    safre_af:taa_viv_recepcion
   WHERE     nss                           =  g_nss_inv
     AND     ident_operacion               =  "09";
   IF        g_folio_trasp     IS  NULL    OR
             g_folio_trasp      =  0    THEN
             ERROR    "   NO  EXISTE  EN  RECEPTORA      "
             PROMPT  "  TECLEE  ENTER  PARA  SALIR   "  FOR  g_enter
             EXIT  PROGRAM
   END  IF
   SELECT    fecha_mov_banxico
     INTO    l_f_banxico
     FROM    safre_af:taa_viv_recepcion
   WHERE     nss                           =  g_nss_inv
     AND     folio                         =  g_folio_trasp;
   LET       l_ano_tra                     =  l_f_banxico   USING  "YYYY"
   LET       l_ano_act                     =  g_today       USING  "YYYY"
   IF        l_ano_tra                     <  l_ano_act     THEN
             LET       g_tabname           =  "dis_cuenta",l_ano_tra[3,4] 
   END IF

   LET       g_sql                    =
            ' SELECT   *  ',
            '   FROM ', g_tabname  CLIPPED ,
            '  WHERE    nss                     =  ','"',g_nss_inv,'"',
            '    AND    folio                   =  ',g_folio_trasp,
            '    AND    tipo_movimiento         =  1 ',
            '  ORDER    BY  1,2,3 ;' 
   PREPARE  sql_m         FROM   g_sql
   DECLARE  c_movtos      CURSOR     FOR  sql_m
   FOREACH  c_movtos      INTO   reg_cta.*
         LET      l_separado                  =  0
         SELECT   UNIQUE 1
           INTO   l_separado
           FROM   safre_af:dis_cuenta
          WHERE   nss                         =  reg_cta.nss
            AND   folio                       =  g_folio_sep
            AND   id_aportante                =  reg_cta.id_aportante
            AND   subcuenta                   =  reg_cta.subcuenta
            AND   consecutivo_lote            =  reg_cta.consecutivo_lote 
            AND   tipo_movimiento             =  280;
         IF       l_separado                  =  1     THEN
                  CONTINUE   FOREACH
         END  IF
         LET      g_mv[g_reg].porc_sep          =  0
         LET      g_mv[g_reg].fecha_conversion  =  reg_cta.fecha_conversion
         LET      g_mv[g_reg].subcuenta         =  reg_cta.subcuenta
         LET      g_mv[g_reg].tipo_movimiento   =  reg_cta.tipo_movimiento
         LET      g_mv[g_reg].id_aportante      =  reg_cta.id_aportante
         LET      g_mv[g_reg].acciones          =  reg_cta.monto_en_acciones
         LET      g_mv[g_reg].pesos             =  reg_cta.monto_en_pesos
         LET      g_reg                         =  g_reg      +  1
   END FOREACH
   CALL     SET_COUNT(g_reg -1)
END  FUNCTION

FUNCTION    F_080_trae_nombres()
   DEFINE   l_nombres                       ,
            l_paterno                       ,
            l_materno                       ,
            l_nombre                        CHAR(60)
   SELECT   nombres,paterno,materno
     INTO   l_nombres,l_paterno,l_materno
     FROM   safre_af:afi_mae_afiliado
    WHERE   n_seguro               =  g_nss_inv;
   LET      l_nombre               =  l_nombres  CLIPPED," ",l_paterno  CLIPPED,
                                      " ",l_materno  CLIPPED
   DISPLAY  l_nombre              TO  FORMONLY.nombre_invadido
   SELECT   nombres,paterno,materno
     INTO   l_nombres,l_paterno,l_materno
     FROM   safre_af:sep_det_reg_sol_reclamante
    WHERE   nss                    =  g_nss_recla
      AND   estado               IN(5,8);
   LET      l_nombre               =  l_nombres  CLIPPED," ",l_paterno  CLIPPED,
                                      " ",l_materno  CLIPPED
   DISPLAY  l_nombre              TO  FORMONLY.nombre_recla

END  FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_cur                           ,
            l_arr_count                     ,
            l_scr_line                      ,
            l_perfil_cod                    ,
            l_arr_curr                      SMALLINT
   INPUT   ARRAY    g_mv  WITHOUT DEFAULTS  FROM  scr_movtos.*
           BEFORE  ROW
                   LET      l_arr_curr         =  ARR_CURR()
                   LET      l_arr_count        =  ARR_COUNT() + 1
                   LET      l_scr_line         =  SCR_LINE()
                   IF       l_arr_curr         >  l_arr_count    THEN
                            EXIT INPUT
                   END IF
                   DISPLAY  g_mv[l_arr_curr].*   TO
                            scr_movtos[l_scr_line].*
                                ATTRIBUTE(REVERSE)
           AFTER    FIELD    porc_sep
                   IF       l_arr_curr     >=  (l_arr_count)  THEN
                            LET      g_lastkey     =   FGL_LASTKEY()
                            IF     ((g_lastkey     =
                                     FGL_KEYVAL("down"))      OR
                                    (g_lastkey     =
                                     FGL_KEYVAL("return"))    OR
                                    (g_lastkey     =
                                     FGL_KEYVAL("tab"))       OR
                                    (g_lastkey     =
                                      FGL_KEYVAL("right"))) THEN
                                    ERROR    "    NO HAY MAS REGISTROS",
                                    " EN ESTA DIRECCION ........      "
                                    NEXT    FIELD     porc_sep
                            END IF
                   END IF
           AFTER   ROW
                   LET      l_arr_curr         =  ARR_CURR()
                   LET      l_scr_line         =  SCR_LINE()
                   DISPLAY  g_mv[l_arr_curr].*   TO
                            scr_movtos[l_scr_line].*
           ON KEY (ESC)
                 CALL     F_120_trae_precios_acc_parti()
                 CALL     F_120_divide_montos()
                 CALL     F_999_fin()
                 EXIT     PROGRAM

   END INPUT
END FUNCTION

FUNCTION    F_120_trae_precios_acc_parti()
   DEFINE   l_pos                             ,
            l_precio_accion                   DEC(19,14),
            l_sie                             SMALLINT
   LET      g_f_conver               =  MDY(MONTH(today),"03",YEAR(today))
   DECLARE  cur_sie       CURSOR    FOR  
   SELECT   v.codigo_siefore,v.precio_del_dia
     FROM   safre_af:glo_valor_accion   v
    WHERE   fecha_valuacion          =  g_f_conver
      AND   codigo_siefore           NOT IN(0)
   FOREACH  cur_sie       INTO    l_sie,l_precio_accion
            LET      g_sie[l_sie].precio_accion     =  l_precio_accion
   END FOREACH
END FUNCTION

FUNCTION    F_120_divide_montos()
   DEFINE   l_porc_acc                DEC(10,6)
   DEFINE   l_sie                     SMALLINT
   LET      g_current                =  CURRENT
   DECLARE  c_porc        CURSOR     FOR  sql_m
   FOREACH  c_porc        INTO   reg_cta.*
         FOR       i                 =  1      TO  g_reg
                   IF     reg_cta.subcuenta        =  g_mv[i].subcuenta  AND
                          g_mv[i].porc_sep         >  0                  THEN
                          EXIT   FOR
                   END  IF
         END  FOR
         IF        i                           >=  g_reg    THEN
                   CONTINUE  FOREACH
         END IF
         IF        reg_cta.subcuenta            =  4    OR
                   reg_cta.subcuenta            =  8    OR
                   reg_cta.subcuenta            =  14   OR
                   reg_cta.subcuenta            =  35  THEN
                   IF     reg_cta.monto_en_acciones    =  0      THEN
                          LET   reg_cta.monto_en_acciones   =  g_mv[i].acciones
                   END IF
         END IF
         SELECT    codigo_siefore 
           INTO    l_sie
           FROM    cta_regimen     reg
          WHERE    nss                          =  reg_cta.nss
            AND    subcuenta                    =  reg_cta.subcuenta;
         LET       reg_cta.monto_en_acciones    = 
                  (g_mv[i].acciones    *   g_mv[i].porc_sep  / 100)  *  -1
         LET       g_mv[i].porc_sep             =  0   
         LET       reg_cta.folio                =  g_folio_sep
         LET       reg_cta.siefore              =  l_sie
         LET       reg_cta.precio_accion        =  g_sie[l_sie].precio_accion
         LET       reg_cta.tipo_movimiento      =  280
         LET       reg_cta.monto_en_pesos       = 
                  (reg_cta.monto_en_acciones    *  g_sie[l_sie].precio_accion )
         CALL      insert_dis_cuenta()
         LET       reg_cta.nss                  =  g_nss_recla
         SELECT    codigo_siefore
           INTO    l_sie
           FROM    cta_regimen   reg
          WHERE    nss                          =  reg_cta.nss
            AND    reg.subcuenta                =  reg_cta.subcuenta;
         LET       reg_cta.siefore              =  l_sie
         LET       reg_cta.precio_accion        =  g_sie[l_sie].precio_accion
         LET       reg_cta.tipo_movimiento      =  590
         LET       reg_cta.monto_en_acciones    = 
                   reg_cta.monto_en_acciones    *   -1
         LET       reg_cta.monto_en_pesos       =
                  (reg_cta.monto_en_acciones    *  g_sie[l_sie].precio_accion )
         CALL      insert_dis_cuenta()
   END FOREACH
END FUNCTION

FUNCTION  insert_dis_cuenta()
   INSERT   INTO    safre_af:dis_cuenta 
            VALUES (
                   reg_cta.tipo_movimiento    ,
                   reg_cta.subcuenta          ,
                   reg_cta.siefore            ,
                   reg_cta.folio              ,
                   reg_cta.consecutivo_lote   ,
                   reg_cta.nss                ,
                   reg_cta.curp               ,
                   reg_cta.folio_sua          ,
                   reg_cta.fecha_pago         ,
                   reg_cta.fecha_valor        ,
                   g_f_conver                 ,
                   reg_cta.monto_en_pesos     ,
                   reg_cta.monto_en_acciones  ,
                   reg_cta.precio_accion      ,
                   reg_cta.dias_cotizados     ,
                   reg_cta.sucursal           ,
                   reg_cta.id_aportante       ,
                   reg_cta.estado             ,
                   g_today                    ,
                   reg_cta.usuario            ,
                   reg_cta.fecha_archivo      ,
                   reg_cta.tipo_movimiento      ) 
END FUNCTION

FUNCTION   F_999_fin()
   LET     g_enter                    =  "F"
   PROMPT  "  Separación Realizada teclee <Enter para Continuar>?..  " 
           FOR    g_enter
   PROMPT  "  Teclee <Enter para Salir> o <R> Reversa la Separación efectuada?..  "  FOR  g_enter
   IF      g_enter                   IS   NULL   THEN
           EXIT  PROGRAM
   END IF
       
   IF      g_enter                    = "R"      OR
           g_enter                    = "r"      THEN
           DELETE  FROM  safre_af:dis_cuenta
            WHERE  nss                        =  g_nss_inv
              AND  folio                      =  g_folio_sep
              AND  id_aportante               =  reg_cta.id_aportante
              AND  tipo_movimiento            =  280
              AND  etiqueta                   =  280;
           DELETE  FROM  safre_af:dis_cuenta
            WHERE  nss                        =  g_nss_recla
              AND  folio                      =  g_folio_sep
              AND  id_aportante               =  reg_cta.id_aportante
              AND  tipo_movimiento            =  590
              AND  etiqueta                   =  590;
          PROMPT   " Fin de Reverso teclee <Enter> para Salir... "  FOR  g_enter
   END IF
END FUNCTION
