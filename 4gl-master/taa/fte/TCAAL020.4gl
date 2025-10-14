###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TCAAL020  => CONSTANCIA DE LIQUIDACION DE TRASPASO AFORE CEDENTE#
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha              => 03 SEPTIEMBRE  2009                                #
#Sistema            => TCAA                                               #
###########################################################################
DATABASE   safre_af
GLOBALS
   DEFINE
      g_lastkey                          ,
      g_arr_menu                         ,
      g_arr_curr                         ,
      g_scr_line                         ,
      g_arr_count                        ,
      g_liquidada                        ,  
      g_long                             ,  
      i                                  ,  
      g_grupo                            ,
      g_tipo_proceso                     SMALLINT
   DEFINE
      g_enter                            CHAR(001),
      g_tipo_traspaso                    CHAR(002),
      g_desc_tipo_traspaso               CHAR(023),
      g_afore_cedente                    CHAR(030),
      g_ejecuta                          CHAR(300)
   DEFINE 
      reg_taa_cd_ctr_folio            RECORD  LIKE  taa_cd_ctr_folio.*
   DEFINE 
      g_num_registros                    INTEGER
   DEFINE 
      g_today                            DATE,
      g_usuario                          CHAR(008),
      g_lista                            CHAR(500),
      g_archivo                          CHAR(600),
      g_archivo2                         CHAR(600),
      g_archivo3                         CHAR(600),
      g_hora                             CHAR(008)
   DEFINE  
      g_seg_modulo                    RECORD LIKE safre_af:seg_modulo.*
   DEFINE 
      cza                             RECORD
          tipo_registro                  CHAR(02),
          tipo_carta                     CHAR(05),
          leyenda_cza                    CHAR(120),
          codigo_afore                   CHAR(03),
          fecha_generacion               CHAR(10),
          consec_lote                    CHAR(08),
          num_registros                  CHAR(08)
                                      END RECORD
   DEFINE
      det                             RECORD
          tipo_registro                  CHAR(02),
          tipo_carta                     CHAR(05),
          nombre                         CHAR(40),
          paterno                        CHAR(40),
          materno                        CHAR(40),
          calle                          CHAR(40),
          numero                         CHAR(10),
          num_interno                    CHAR(10),
          colonia                        CHAR(60),
          delegacion                     CHAR(40),
          ciudad                         CHAR(40),
          estado                         CHAR(40),
          codigo_postal                  CHAR(05),
          centro_reparto                 CHAR(05),
          lada                           CHAR(05),
          telefono                       CHAR(10),
          fecha_generacion               CHAR(10),
          nss                            CHAR(11),
          sexo                           CHAR(01),
          curp                           CHAR(18),
          id_20a21                       CHAR(20),
          n_folio                        CHAR(10),
          id_23a24                       CHAR(04),
          afore_cedente                  CHAR(30),
          id_26a32                       CHAR(79),
          siefore_1                      CHAR(08),
          siefore_7                      CHAR(08),
          siefore_10                     CHAR(08),
          siefore_11                     CHAR(08),
          siefore_13                     CHAR(08),
          siefore_15                     CHAR(08),
          id_39a42                       CHAR(46),
          rfc                            CHAR(13),
          id_44                          CHAR(10),
          fecha_recep_recur              CHAR(10),
          id_46                          CHAR(10),
          nacionalidad                   CHAR(20),
          id_48                          CHAR(40),
          rendimientos                   CHAR(1014),
          id_153                         CHAR(02),
          saldos_1a4                     CHAR(52),
          afo_recep                      CHAR(03),
          nom_afo_recep                  CHAR(30),
          id_160                         CHAR(40),
          udis_bono                      CHAR(13),
          fecha_redencion                CHAR(10),
          codigo_barras                  CHAR(28),
          saldos_issste                  CHAR(26)
                                      END RECORD
END GLOBALS

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
   CALL     STARTLOG('TCAAL020.log')
   CALL     F_100_inicio()
   CALL     F_500_proceso()
END MAIN

FUNCTION    F_100_inicio()
   LET      g_today                      =  TODAY 
   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod                   =  'taa';
   LET      g_liquidada                  =  103
   INITIALIZE      cza.*,  det.*        TO    NULL
END FUNCTION

FUNCTION    F_500_proceso()
   DEFINE   l_registros                   INTEGER,
            l_day                         DATE
   OPEN WINDOW  TCAAL020   AT 2,2  WITH  FORM  "TCAAL020"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAL020  GENERA CONSTANCIAS DE LIQUIDACIÓN DE TRASPASOS (A-A CEDENTE) ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar            < CTRL-C > Salir                     ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today USING "DD-MM-YYYY" AT 1,64 ATTRIBUTE(REVERSE)
   SELECT   MAX(fecha_liquidacion)
     INTO   l_day
     FROM   taa_cd_ctr_folio a
    WHERE   estado                   =   103
      AND   tipo_traspaso           IN(1,3,4)
   DECLARE  cur_folios         CURSOR   FOR
   SELECT   a.*
     FROM   safre_af:taa_cd_ctr_folio a
    WHERE   a.estado                 =  103
      AND   tipo_traspaso          IN(1,3,4)
      AND   a.fecha_liquidacion      =  l_day
    ORDER   BY  tipo_traspaso  desc ; 
   FOREACH  cur_folios         INTO   reg_taa_cd_ctr_folio.*
            SELECT   COUNT(*)
              INTO   l_registros
              FROM   safre_af:taa_cd_det_cedido
             WHERE   folio                     =  reg_taa_cd_ctr_folio.folio
               AND   estado                    =  g_liquidada;
            LET      g_tipo_proceso   =  reg_taa_cd_ctr_folio.tipo_traspaso
            IF       g_tipo_proceso            =  1     THEN
                     LET      g_desc_tipo_traspaso      =  'PROMOTOR'
            ELSE
            IF       g_tipo_proceso            =  3     THEN
                     LET      g_desc_tipo_traspaso      =  'INTERNET'
            ELSE
            IF       g_tipo_proceso            =  4     THEN
                     LET      g_desc_tipo_traspaso      =  'DIVERSOS'
            END IF
            END IF
            END IF
            DISPLAY  reg_taa_cd_ctr_folio.folio     TO  FORMONLY.folio
            DISPLAY  g_desc_tipo_traspaso           TO  FORMONLY.desc_tipo
            DISPLAY  reg_taa_cd_ctr_folio.fecha_presentacion    TO
                     FORMONLY.fecha_presentacion
            DISPLAY  reg_taa_cd_ctr_folio.fecha_envio_saldos    TO
                     FORMONLY.fecha_envio_saldos
            DISPLAY  reg_taa_cd_ctr_folio.fecha_liquidacion     TO
                     FORMONLY.fecha_liquidacion
            DISPLAY  l_registros                TO  FORMONLY.l_registros
            WHILE    TRUE
                     PROMPT    " ES EL FOLIO DESEADO TECLEE [S/N] ?..."
                               FOR      g_enter
                     IF        g_enter      MATCHES   "[sSnN]"       THEN
                               IF     g_enter      MATCHES   "[sS]"    THEN
                                      EXIT FOREACH
                               ELSE
                                      LET       reg_taa_cd_ctr_folio.folio  =  0
                                      CONTINUE  FOREACH
                               END IF
                     END  IF
            END      WHILE
   END FOREACH
   IF       reg_taa_cd_ctr_folio.folio      IS  NULL     OR
            reg_taa_cd_ctr_folio.folio       =  0        THEN
            ERROR    "  NO  HAY  FOLIO PARA GENERAR CONSTANCIAS:    "
            PROMPT   "  TECLEE ENTER PARA SALIR..."  FOR  g_enter
            EXIT     PROGRAM
   END IF
   DISPLAY  "  PROCESANDO INFORMACION ......                                "
               AT   18,1   ATTRIBUTE(REVERSE)
   CALL     F_520_genera_archivo()
   DISPLAY  "                                                                  "
               AT   18,1
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE    WINDOW   TCAAL020
END FUNCTION

FUNCTION    F_520_genera_archivo()
   DEFINE   l_registros                   INTEGER
   LET      g_hora                       =  TIME
   LET      g_num_registros              =  0
   LET      g_archivo                    =
            g_seg_modulo.ruta_envio   CLIPPED,"/",g_usuario   CLIPPED,
            ".TCAAL020.",g_today      USING   "YYMMDD", "_",g_hora    CLIPPED
   START    REPORT       R_590_imprime_archivo     TO  g_archivo
   DISPLAY  "      PROCESANDO  INFORMACION ..............         "  AT  16,1
   DISPLAY  "ARCHIVO:",g_archivo," "    AT  16,1    ATTRIBUTE(REVERSE)
   CALL     F_530_selecciona_tipo_traspaso()
   LET      cza.tipo_registro            =  "01"
   LET      cza.tipo_carta               =  "30225"
   LET      cza.leyenda_cza          =  "Constancia de Liquidación de Traspaso"
   LET      cza.fecha_generacion         =  g_today   USING  "DD/MM/YYYY"
   LET      cza.consec_lote              =  g_tipo_proceso  USING '&&&&&&&&'
   SELECT   codigo_afore,razon_social
     INTO   cza.codigo_afore,g_afore_cedente
     FROM   safre_af:tab_afore_local;
   IF       g_tipo_proceso               =  1     THEN
            SELECT   1
             FROM    tmp_tipo_traspaso
            WHERE    tipo_traspaso                = '12'
            IF       status                      <>    NOTFOUND   THEN
                      LET      cza.consec_lote         =  2  USING  '&&&&&&&&'
            END IF
   END IF
   SELECT   COUNT(UNIQUE  n_seguro)
     INTO   l_registros
     FROM   safre_af:taa_cd_det_cedido  cd
    WHERE   cd.folio                    =  reg_taa_cd_ctr_folio.folio
      AND   cd.tipo_traspaso           IN(SELECT  tipo_traspaso
                                           FROM  tmp_tipo_traspaso)
      AND   cd.estado                   =  103;
   DISPLAY  l_registros                TO  FORMONLY.l_registros
   DECLARE  cur_ced       CURSOR  FOR
   SELECT   n_seguro,ident_lote_solici[3,5]
     FROM   safre_af:taa_cd_det_cedido  cd
    WHERE   cd.folio                =  reg_taa_cd_ctr_folio.folio
      AND   cd.tipo_traspaso       IN(SELECT  tipo_traspaso 
                                        FROM  tmp_tipo_traspaso)
      AND   cd.estado               =  103;
   FOREACH  cur_ced         INTO  det.nss,det.afo_recep
          LET      g_num_registros         =  g_num_registros     +  1
          DISPLAY  g_num_registros        TO  l_procesados 
          CALL     F_550_arma_datos_generales()
          CALL     F_550_trae_saldos()
          OUTPUT                    TO   REPORT    R_590_imprime_archivo()
          INITIALIZE    det.*       TO  NULL
#if g_num_registros  = 100  then exit  foreach  end if
   END  FOREACH
   FINISH   REPORT   R_590_imprime_archivo
   LET      g_lista                       =  "chmod   777 ",g_archivo CLIPPED
   RUN      g_lista
   ERROR   "                                                         "
END FUNCTION

FUNCTION    F_530_selecciona_tipo_traspaso()
   DEFINE   l_cur                           ,
            l_arr_count                     ,
            l_hay_proceso                   ,
            l_scr_line                      ,
            l_arr_curr                      SMALLINT,
            l_registros                     INTEGER
   DEFINE   l_tipo_tra_desc                 CHAR(50),
            l_tipo_traspaso                 CHAR(02)
   DEFINE   l_sql                           CHAR(1000)
   DEFINE   l_construct                     CHAR(1000)
   DEFINE   l_t_trasp                    ARRAY[100]  OF   RECORD
            cursor                          CHAR(01),
            tipo_traspaso                   CHAR(02),
            tipo_tra_desc                   CHAR(40),
            registros                       INTEGER  
                                                        END   RECORD
   LET      l_hay_proceso              =  0
   CREATE   TEMP   TABLE   tmp_tipo_traspaso
           (tipo_traspaso        CHAR(02));
   OPEN     WINDOW   TCAAL0201        AT  4,2 
            WITH     FORM    "TCAAL0201"   ATTRIBUTE(BORDER)
   DISPLAY  "TCAAL0201   <<  ORIGEN DE TRASPASOS A GENERAR MARCADOS CON * >>                                   "    AT  1,1   ATTRIBUTE    (REVERSE)
   DISPLAY  " <Esc>Salir   <Enter> Omite Generar Origen Traspaso ",
            " <Control-c>Cancelar            "   AT  3,1  ATTRIBUTE  (REVERSE)
   FOR      i                        =  1        TO  100
            INITIALIZE     l_t_trasp[i].*      TO  NULL
   END FOR
   LET      i                        =  1
   LET      l_registros              =  0
   LET      l_tipo_traspaso          =  NULL
   LET      l_tipo_tra_desc          =  NULL
   CONSTRUCT  l_construct    ON  a.tipo_traspaso   FROM  tipo_traspaso_sol
            ON KEY  ( RETURN )
                     LET      INT_FLAG        =  FALSE
                     EXIT     CONSTRUCT
            ON KEY  ( INTERRUPT )
                     EXIT     CONSTRUCT
   END CONSTRUCT
   IF       INT_FLAG                 =  FALSE     THEN
            LET       l_sql                   =
                      " SELECT   UNIQUE  '*',a.tipo_traspaso,  ",
                      "          t.descripcion ,COUNT(n_seguro)  ",
                      "   FROM   safre_af:taa_cd_det_cedido   a ,",
                      "          safre_af:taa_cd_tipo_traspaso  t ",
                      "  WHERE  ",l_construct   CLIPPED,
                      "    AND   a.folio      =   ",reg_taa_cd_ctr_folio.folio,
                      "    AND   a.tipo_traspaso   =  t.tipo_traspaso ",
                      "    AND   a.estado          =  103             ",
                      "  GROUP   BY  1,2,3                            ",
                      "  ORDER   BY  1,2,3                            "
            LET       l_sql                =  l_sql    CLIPPED
            DISPLAY  "  EXTRAE ORIGEN DE TRASPASO Y NUMERO DE REGISTROS........      "
               AT   18,1   ATTRIBUTE(REVERSE)
            PREPARE   qry_consul         FROM    l_sql
            DECLARE   cursor_c     CURSOR FOR    qry_consul
            FOREACH   cursor_c           INTO    l_t_trasp[i].*
                      LET      i           =  i   +  1
            END FOREACH
   END IF
   IF      (i  -  1)          >=  1    THEN
            CALL     SET_COUNT(i  - 1)
            INPUT    ARRAY    l_t_trasp  WITHOUT DEFAULTS  FROM  scr_tipo_tra.*
                     BEFORE   ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_arr_count        =  ARR_COUNT()
                              LET      l_scr_line         =  SCR_LINE()
                              IF       l_arr_curr         >  l_arr_count    THEN
                                       EXIT INPUT
                              END IF
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                                       ATTRIBUTE(REVERSE)
                                       DISPLAY  "  GENERA UNICAMENTE ORIGEN DE TRASPASO",
                                       " MARCADO CON *                       "
                                       AT   18,1   ATTRIBUTE(REVERSE)

                     AFTER    ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_scr_line         =  SCR_LINE()
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                     AFTER    FIELD    cursor
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
                                                ERROR    "    NO HAY MAS OPCI",
                                                         "ONES EN ESA",
                                                     " DIRECCION ........      "
                                                NEXT    FIELD     cursor
                                       END IF
                              END IF
                     ON KEY   ( RETURN )
                              INITIALIZE  l_t_trasp[l_arr_curr].*    TO  NULL
                              DISPLAY  l_t_trasp[l_arr_curr].*   TO
                                       scr_tipo_tra[l_scr_line].*
                                       ATTRIBUTE(REVERSE)
                     ON KEY  ( INTERRUPT )
                              FOR   i      =  1        TO  100
                                    INITIALIZE   l_t_trasp[i].*    TO  NULL
                              END FOR
                              EXIT     INPUT
                     ON KEY  ( ESC )
                              EXIT     INPUT
            END INPUT
   END IF
   FOR      i           =  1       TO  l_arr_count
            IF       l_t_trasp[i].cursor         =  "*"     THEN
                     LET      l_hay_proceso      =  1
                     LET      l_registros        =  l_registros   +
                              l_t_trasp[i].registros
                     INSERT   INTO  tmp_tipo_traspaso
                              VALUES  ( l_t_trasp[i].tipo_traspaso)
            END IF
   END FOR
   IF       NOT    l_hay_proceso             THEN
            PROMPT   " NO ELIGIÓ ORIGEN DE TRASPASO  TECLEE <Enter> para Salir",
                     " ..................   "   FOR    g_enter
            EXIT  PROGRAM
   END IF
   LET      cza.num_registros           =  l_registros   USING "&&&&&&&&"
   LET      INT_FLAG                    =  FALSE
   CLEAR    FORM 
   CLOSE    WINDOW  TCAAL0201
END FUNCTION

FUNCTION    F_550_arma_datos_generales()
   DEFINE   l_fecha                           CHAR(08),
            l_fecha_reden                     DATE
   LET      det.tipo_registro           =  '02'
   LET      det.tipo_carta              =  '30225'
   LET      det.fecha_generacion        =  g_today   USING "DD/MM/YYYY"
   LET      cza.fecha_generacion        =  g_today   USING "DD/MM/YYYY"
   SELECT   afi.n_unico  ,  afi.n_rfc   ,  n_folio,
            afi.paterno  ,  afi.materno ,  afi.nombres,
            nac.nacionalidad
     INTO   det.curp     ,  det.rfc     ,  det.n_folio,
            det.paterno  ,  det.materno ,  det.nombre,
            det.nacionalidad
     FROM   safre_af:afi_mae_afiliado  afi, OUTER tab_nacionalidad nac
    WHERE   afi.n_seguro                 =  det.nss
      AND   afi.nacionalidad             =  nac.codigo_pais;
   CALL     F_522_arma_siefores()
   CALL     F_523_trae_telefono()
   CALL     F_570_trae_domicilio()
   SELECT   MAX(fecha_reden)
     INTO   l_fecha
     FROM   safre_af:dis_det_bono
    WHERE   n_unico                      =  det.curp;
   LET      det.fecha_recep_recur        = 
            reg_taa_cd_ctr_folio.fecha_liquidacion    USING "DD/MM/YYYY"
   IF       l_fecha      IS   NOT   NULL  THEN
            LET    l_fecha_reden         =
                   MDY(l_fecha[5,6],l_fecha[7,8],l_fecha[1,4])
   END IF
   CALL     F_580_arma_tabla_comisiones()
   IF       l_fecha_reden                IS    NULL          OR
            l_fecha_reden                 <   "01/01/2009"   THEN
            SELECT   MAX(fecha_red_bono)
              INTO   l_fecha_reden        
              FROM   safre_af:taa_viv_recepcion
             WHERE   nss                  =  det.nss
   END IF
   IF       l_fecha_reden                IS  NOT  NULL       AND
            l_fecha_reden                <> "01/01/0001"     AND 
            l_fecha_reden                <> "12/31/1899"     THEN
            LET      det.fecha_redencion    = l_fecha_reden USING "DD/MM/YYYY"
   ELSE
            LET      det.fecha_redencion    =  "          "  
   END IF
########    No_carta  700 ,  ref  21  ,  000 ,  nss
   LET      det.codigo_barras          =  "C70021000",det.nss,
            reg_taa_cd_ctr_folio.fecha_liquidacion     USING   "YYYYMMDD"
   LET      det.afore_cedente          =  g_afore_cedente
   SELECT   afore_desc
     INTO   det.nom_afo_recep
     FROM   tab_afore
    WHERE   afore_cod                    =  det.afo_recep
   IF       det.nom_afo_recep      IS  NULL      THEN
            LET      det.nom_afo_recep       =  " "
   END IF 
END FUNCTION

FUNCTION  F_522_arma_siefores()
   DEFINE   l_siefore                         CHAR(08),
            l_subcuenta                       SMALLINT
   DECLARE  cur_sie         CURSOR   FOR
   SELECT   reg.subcuenta,sie.razon_social[1,8]
     FROM   cta_regimen  reg,  tab_siefore_local  sie
    WHERE   reg.nss                      =  det.nss
      AND   reg.subcuenta               IN(1,7,10,11,13,15)
      AND   reg.codigo_siefore           =  sie.codigo_siefore;
   FOREACH  cur_sie         INTO  l_subcuenta,l_siefore
            CASE     l_subcuenta
                      WHEN    1     LET     det.siefore_1       =  l_siefore 
                      WHEN    7     LET     det.siefore_7       =  l_siefore 
                      WHEN    10    LET     det.siefore_10      =  l_siefore 
                      WHEN    11    LET     det.siefore_11      =  l_siefore 
                      WHEN    13    LET     det.siefore_13      =  l_siefore 
                      WHEN    15    LET     det.siefore_15      =  l_siefore 
            END CASE
   END   FOREACH
END FUNCTION

FUNCTION  F_523_trae_telefono()
   DEFINE   l_telefono           CHAR(15)
   DECLARE  c_tel           CURSOR     FOR
   SELECT   cve_lada, telefono
     INTO   det.lada,   det.telefono
     FROM   safre_af:afi_mae_afiliado afi, OUTER safre_af:afi_telefono  tel
    WHERE   tel.nss                    =  det.nss
      AND   tel.nss                    =  afi.n_seguro
      AND   tel.n_folio                =  afi.n_folio
      AND   tel.tipo_solicitud         =  afi.tipo_solicitud
      AND   tel.telefono     IS       NOT   NULL
      AND   tel.tel_cod                <   7
    ORDER   BY  factualiza     DESC
   FOREACH  c_tel               INTO  det.lada,det.telefono
            EXIT  FOREACH
   END FOREACH
END FUNCTION

FUNCTION  F_550_trae_saldos()
   DEFINE    l_pesos                      ,
             l_acciones                   DEC(16,6)
   DECLARE  cur_grupo       CURSOR  FOR
    SELECT  UNIQUE     (grupo)
      FROM  safre_af:taa_cd_inf_grupo
     ORDER  BY  grupo;
   FOREACH  cur_grupo       INTO    g_grupo
         SELECT   SUM(monto_en_pesos * -100),SUM(monto_en_acciones * -100)
           INTO   l_pesos,l_acciones
           FROM   safre_af:dis_cuenta     dc ,
                  safre_af:taa_cd_inf_grupo      gp
          WHERE   dc.nss                  =  det.nss
            AND   dc.folio                =  reg_taa_cd_ctr_folio.folio
            AND   gp.grupo                =  g_grupo
            AND   dc.subcuenta            =  gp.subcuenta
         IF       l_pesos                IS   NULL    THEN
                  LET      l_pesos        =  0
         END IF
         IF       l_pesos                IS   NULL    THEN
                  LET      l_acciones     =  0
         END IF
         IF       g_grupo                <=  4  THEN 
                  LET      det.saldos_1a4    =  det.saldos_1a4  CLIPPED,
                              l_pesos  USING  "&&&&&&&&&&&&&"  CLIPPED
         END IF
         IF       g_grupo                 =  5       THEN
                  LET      det.udis_bono  =  l_acciones USING "&&&&&&&&&&&&&"
         END IF
         IF       g_grupo                >=  6  THEN 
                  LET      det.saldos_issste    =  det.saldos_issste CLIPPED,
                           l_pesos     USING  "&&&&&&&&&&&&&"  CLIPPED
         END IF
   END  FOREACH
END FUNCTION

FUNCTION    F_570_trae_domicilio()
    DEFINE    domi        INTEGER
    DEFINE    l_delegacion         LIKE   afi_domicilio.delega,
              l_estado             LIKE   afi_domicilio.estado,
              l_ciudad             LIKE   afi_domicilio.ciudad
   SELECT   MIN(s.ROWID)
     INTO   domi
     FROM   afi_mae_afiliado  afi, OUTER afi_domicilio s
    WHERE   s.nss                    =  det.nss
      AND   s.nss                    =  afi.n_seguro
      AND   s.n_folio                =  afi.n_folio
      AND   s.tipo_solicitud         =  afi.tipo_solicitud
      AND   s.marca_envio            = "X";
   IF       SQLCA.SQLCODE           <>  0    THEN
            RETURN
   END IF
   SELECT   o.calle, o.numero, o.depto, o.colonia, o.codpos,
            o.delega, o.estado, o.ciudad
     INTO   det.calle, det.numero, det.num_interno,  det.colonia,
            det.codigo_postal, l_delegacion, l_estado , l_ciudad  
     FROM   afi_domicilio o
    WHERE   o.rowid                  =  domi;
   SELECT   am.centro_reparto
     INTO   det.centro_reparto
     FROM   tab_reparto am
    WHERE   am.codigo_postal         =  det.codigo_postal;
   SELECT   p.estad_desc
     INTO   det.estado
     FROM   tab_estado p
    WHERE   p.estad_cod              =  l_estado;
   SELECT   q.deleg_desc
     INTO   det.delegacion
     FROM   tab_delegacion q
    WHERE   q.deleg_cod              =  l_delegacion;
   SELECT   q.ciudad_desc
     INTO   det.ciudad
     FROM   tab_ciudad q
    WHERE   q.ciudad_cod             =  l_ciudad;
END  FUNCTION

FUNCTION F_580_arma_tabla_comisiones()
  DEFINE   cat            RECORD
           rendim_afo                            DECIMAL(4,2),
           comis_afo                             DECIMAL(5,2),
           rendim_neto                           DECIMAL(4,2)
                          END RECORD,
           l_rendi                               CHAR(1014),
           afore                                 CHAR(20),
           cuantos                               SMALLINT,
           vsiefore                              SMALLINT
   SELECT   c.codigo_siefore
     INTO   vsiefore
     FROM   cta_nss_regimen c
    WHERE   c.nss                    =  det.nss
      AND   grupo_regimen            =  1;
   LET      cuantos                  =  0
   SELECT   COUNT(*) 
     INTO   cuantos
     FROM   tab_rendimiento_neto a
    WHERE   reg_taa_cd_ctr_folio.fecha_liquidacion 
            BETWEEN   a.fecha_ini       AND   a.fecha_fin
      AND   a.siefore_cod            =  vsiefore;
--Se actualiza contador a "13", antes 14 ACS-30/Ene/2012
   IF       cuantos                  <  13       THEN 
            PROMPT "No estan completas las Afores en el Comparativo, <Enter> "
            FOR  g_enter
   END IF
   LET      l_rendi                  =  ' '
   DECLARE  cur_tab_comi         CURSOR  FOR
   SELECT   m.afore_desc, a.rendimiento, a.comision, a.rendimiento_neto
     FROM   tab_rendimiento_neto a,  tab_afore m
    WHERE   reg_taa_cd_ctr_folio.fecha_liquidacion 
            BETWEEN   a.fecha_ini         AND  a.fecha_fin
      AND   a.siefore_cod                   =  vsiefore
      AND   a.afore_cod                     =  m.afore_cod
      AND   a.afore_cod              NOT   IN(999) 
    ORDER   BY  a.rendimiento_neto       DESC;
   FOREACH  cur_tab_comi          INTO  afore,cat.*
           FOR      i              =   1   TO  20
                    IF       afore[i]      IS NULL    OR
                             afore[i]       = ' '     THEN
                             LET      afore[i]        =  "?"
                    END  IF
           END FOR
           LET      l_rendi                    =  l_rendi           CLIPPED,
                    afore                                           CLIPPED
           LET      l_rendi                    =  l_rendi           CLIPPED,
                    cat.rendim_afo     USING   "#&.&&","%"          CLIPPED
           LET      l_rendi                    =  l_rendi           CLIPPED,
                    cat.comis_afo      USING   "##&.&&","%"         CLIPPED
           LET      l_rendi                    =  l_rendi           CLIPPED,
                    cat.rendim_neto    USING   "#&.&&","%"          CLIPPED
   END FOREACH
   FOR    i         =  1      TO  1014
          IF        l_rendi[i]           =  '?'       THEN
                    LET      l_rendi[i]           =  ' '
          END IF
   END FOR
   LET    det.rendimientos        =  l_rendi
END FUNCTION

REPORT      R_590_imprime_archivo()
   OUTPUT
       TOP      MARGIN   0
       BOTTOM   MARGIN   0
       LEFT     MARGIN   0
       RIGHT    MARGIN   0
       PAGE     LENGTH   2
   FORMAT
        FIRST   PAGE   HEADER
                PRINT    COLUMN   01,cza.*
        ON EVERY ROW
                PRINT    COLUMN   01,det.*
END REPORT
