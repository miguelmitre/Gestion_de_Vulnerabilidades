###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TCAAL020  => CONSTANCIA DE CAMBIO DE AFORE                      #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha              => 07 AGOSTO  2009                                    #
#Sistema            => TCAA                                               #
###########################################################################
DATABASE   safre_af

GLOBALS
   DEFINE
      g_liquidada                     ,  
      g_long                          ,  
      i                               ,  
      g_tipo_traspaso                 SMALLINT

   DEFINE
      g_enter                         CHAR(001),
      g_desc_tipo_traspaso            CHAR(023),
      g_ejecuta                       CHAR(300)

   DEFINE 
      reg_taa_cd_ctr_folio            RECORD LIKE taa_cd_ctr_folio.*

   DEFINE 
      g_num_registros                 INTEGER

   DEFINE 
      g_today                         DATE,
      g_usuario                       CHAR(008),
      g_lista                         CHAR(200),
      g_archivo                       CHAR(300),
      g_hora                          CHAR(008)

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
          id_26a42                       CHAR(173),
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
          id_159a160                     CHAR(70),
          udis_bono                      CHAR(13),
          fecha_redencion                CHAR(10),
          codigo_barras                  CHAR(28),
          saldos_issste                  CHAR(26)
                                      END RECORD

   DEFINE
      g_grupo                         SMALLINT,
      g_fecha                         CHAR(08),
      g_n_unico                       CHAR(18)
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
    WHERE   modulo_cod               =  'taa';

   SELECT   a.estado
     INTO   g_liquidada
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = 'LIQUIDADA'
      AND   a.tipo                   =  3;

   INITIALIZE      cza.*                TO    NULL
   INITIALIZE      det.*                TO    NULL
END FUNCTION


FUNCTION    F_500_proceso()
   DEFINE   l_registros                   INTEGER,
            l_day                         DATE

   OPEN WINDOW  TCAAL020   AT 2,2  WITH  FORM  "TCAAL020"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAL020    GENERA  CONSTANCIAS DE TRASPASOS  (CEDENTE) ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar                       < CTRL-C > Salir  ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

   SELECT   MAX(fecha_liquidacion)
     INTO   l_day
     FROM   taa_cd_ctr_folio a
    WHERE   estado              =   103

   FOR      g_tipo_traspaso     =  1     TO  3
            IF       g_tipo_traspaso       =  2   THEN
                     CONTINUE  FOR
            END IF

            SELECT   a.*     INTO   reg_taa_cd_ctr_folio.*
              FROM   taa_cd_ctr_folio a
             WHERE   a.tipo_traspaso           =  g_tipo_traspaso
               AND   a.fecha_liquidacion       =  l_day
               AND   a.estado                  =  g_liquidada

            SELECT   COUNT(*)
            INTO   l_registros
            FROM   safre_af:taa_cd_det_cedido
            WHERE   folio             =  reg_taa_cd_ctr_folio.folio
            AND   estado            =  g_liquidada

            LET   cza.num_registros       =  l_registros   USING "&&&&&&&&"
            LET      g_desc_tipo_traspaso          =  'NORMAL'

            IF       g_tipo_traspaso               =  3     THEN  
                     LET      g_desc_tipo_traspaso          =  'INTERNET'
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
               PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? "  FOR  g_enter
               IF       g_enter    MATCHES    "[sS]"      OR  
                        g_enter    MATCHES    "[nN]"      THEN
                        EXIT     WHILE
               END IF
            END WHILE

            IF       g_enter    MATCHES    "[sS]"      THEN
                 EXIT    FOR
            ELSE
               IF       g_enter    MATCHES    "[nN]"      THEN
                  IF         g_tipo_traspaso        =  3    THEN
                       EXIT PROGRAM
                  END IF
               END  IF
            END  IF
   END FOR

   IF       STATUS                =  NOTFOUND    THEN
            PROMPT   "  No hay folio para Generar Constancias",
                     " <<Enter para Salir>>"  FOR  g_enter
            EXIT PROGRAM
   END IF

   DISPLAY  "  PROCESANDO INFORMACION ......                                "
               AT   18,1   ATTRIBUTE(REVERSE)
   CALL     F_520_genera_archivo()
   DISPLAY  "                                                                  "
               AT   18,1
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter

   CLOSE  WINDOW   TCAAL020
END FUNCTION


FUNCTION    F_520_genera_archivo()
   DEFINE   l_fecha_prov                  DATE
   DEFINE   l_scta                        ,
            l_scta_fin                    ,
            verifica_liq                  SMALLINT,
            descr_afore                   CHAR(30)

   LET      g_hora                       =  TIME
   LET      g_num_registros              =  0
   LET      g_archivo                    =
            g_seg_modulo.ruta_envio   CLIPPED,"/",g_usuario   CLIPPED,
            ".TCAAL020.",g_today      USING   "YYMMDD", "_",g_hora    CLIPPED
####       '/safre/taa/fte_v3'       CLIPPED,"/", "TCAAL020.REPORTE" CLIPPED


   START    REPORT       R_590_imprime_archivo     TO  g_archivo
      DISPLAY  "      PROCESANDO  INFORMACION ..............         "  AT  16,1
      DISPLAY  "ARCHIVO:",g_archivo," "   AT  16,1    ATTRIBUTE(REVERSE)
      LET      cza.tipo_registro          =  "01"
      LET      cza.tipo_carta             =  "30221"
      LET      cza.leyenda_cza            =  "CONSTANCIA DE TRASPASO"
      LET      cza.fecha_generacion       =  g_today   USING  "DD/MM/YYYY"
      LET      cza.consec_lote            =  g_tipo_traspaso  USING '&&&&&&&&'

      IF       g_tipo_traspaso            =  3     THEN
            LET      cza.consec_lote   =  g_tipo_traspaso -1  USING '&&&&&&&&'
      END IF

      DECLARE  cur_ced       CURSOR  FOR
      SELECT   n_seguro,ident_lote_solici[3,5]
      FROM   safre_af:taa_cd_det_cedido cd
      WHERE   cd.folio                =  reg_taa_cd_ctr_folio.folio
      --and  cd.n_seguro = "01068725587"
      AND   cd.estado               =  103;

      FOREACH  cur_ced         INTO  det.nss,det.afo_recep
          
          SELECT afore_desc
          INTO descr_afore 
          FROM tab_afore
          WHERE afore_cod = det.afo_recep

          IF descr_afore IS NOT NULL THEN
             LET det.id_159a160 = descr_afore , "                                        "
          END IF 

          LET     g_num_registros         =  g_num_registros     +  1
          DISPLAY  g_num_registros        TO  l_procesados 
          CALL     F_550_arma_datos_generales()
          CALL     F_550_trae_saldos()
          OUTPUT                    TO   REPORT    R_590_imprime_archivo()
          INITIALIZE    det.*       TO  NULL
      END  FOREACH

   FINISH   REPORT   R_590_imprime_archivo

   LET      g_lista                       =  "chmod 777 ",g_archivo CLIPPED
   RUN      g_lista
   ERROR   "                                                         "
END FUNCTION


FUNCTION    F_550_arma_datos_generales()
   DEFINE   l_fecha                           CHAR(08),
            l_fecha_reden                     DATE

   LET      det.tipo_registro           =  '02'
   LET      det.tipo_carta              =  '30221'
   LET      det.centro_reparto          =  '55555'
   LET      det.fecha_generacion        =  g_today   USING "DD/MM/YYYY"
   LET      cza.fecha_generacion        =  g_today   USING "DD/MM/YYYY"

   SELECT   codigo_afore,razon_social
   INTO     cza.codigo_afore,det.afore_cedente
   FROM     safre_af:tab_afore_local;

   SELECT   afi.n_unico  ,  afi.n_rfc   ,  n_folio,
            afi.paterno  ,  afi.materno ,  afi.nombres,
            afi.nacionalidad
   INTO     det.curp     ,  det.rfc     ,  det.n_folio,
            det.paterno  ,  det.materno ,  det.nombre,
            det.nacionalidad
   FROM     safre_af:afi_mae_afiliado  afi
   WHERE    afi.n_seguro               =  det.nss;

    CALL    F_523_trae_telefono()
    CALL    F_570_trae_domicilio()

   SELECT   MAX(fecha_reden)
   INTO     l_fecha
   FROM     safre_af:dis_det_bono
   WHERE    n_unico                 =  g_n_unico;

   LET      det.fecha_recep_recur   = 
            reg_taa_cd_ctr_folio.fecha_liquidacion    USING "DD/MM/YYYY"

   IF       l_fecha IS NOT NULL  THEN
            LET      l_fecha_reden               =
                     l_fecha[5,6]||'/'||l_fecha[7,8]||'/'||l_fecha[1,4]
   END IF

   CALL     F_580_trae_comision()

   IF       l_fecha_reden             IS    NULL          OR
            l_fecha_reden              <   "01/01/2009"   THEN
            SELECT   MAX(fecha_red_bono)
              INTO   l_fecha_reden        
              FROM   safre_af:taa_viv_recepcion
             WHERE   nss               =  det.nss
   END IF

   IF l_fecha_reden IS NOT NULL AND l_fecha_reden <> "01/01/0001" AND 
      l_fecha_reden <> "12/31/1899" THEN
      LET   det.fecha_redencion  =  l_fecha_reden USING "DD/MM/YYYY"
   ELSE
      LET   det.fecha_redencion  =  "          "  
   END IF

########    No_carta  700 ,  ref  21  ,  000 ,  nss
   LET      det.codigo_barras          =  "C70021000",det.nss,
            reg_taa_cd_ctr_folio.fecha_liquidacion     USING   "YYYYMMDD"
END FUNCTION


FUNCTION  F_523_trae_telefono()
   DEFINE   l_telefono           CHAR(15)

   DECLARE  c_tel           CURSOR     FOR
   SELECT   cve_lada, telefono
     INTO   det.lada,   det.telefono
     FROM   safre_af:afi_telefono  tel,  afi_mae_afiliado afi
    WHERE   tel.nss                =  det.nss
      AND   tel.nss                =  afi.n_seguro
      AND   tel.n_folio            =  afi.n_folio
      AND   tel.tipo_solicitud     =  afi.tipo_solicitud
      AND   tel.telefono     IS   NOT   NULL
      AND   tel.tel_cod            <   7
    ORDER   BY  factualiza     DESC

   FOREACH  c_tel          INTO  det.lada,det.telefono
      EXIT FOREACH
   END FOREACH
END FUNCTION


FUNCTION  F_550_trae_saldos()
   DEFINE    l_pesos                      ,
             l_acciones                   DEC(16,6)

   DECLARE  cur_grupo       CURSOR  FOR
    SELECT  UNIQUE (grupo)
      FROM  safre_af:taa_cd_inf_grupo
     ORDER  BY  grupo

   FOREACH  cur_grupo       INTO    g_grupo
            SELECT   SUM(monto_en_pesos * -100),SUM(monto_en_acciones * -100)
              INTO   l_pesos,l_acciones
              FROM   safre_af:dis_cuenta     dc ,
                     safre_af:taa_cd_inf_grupo      gp
             WHERE   dc.nss                  =  det.nss
               AND   dc.folio                =  reg_taa_cd_ctr_folio.folio
               AND   gp.grupo                =  g_grupo
               AND   dc.subcuenta            =  gp.subcuenta
            IF       l_pesos          IS   NULL    THEN
                     LET      l_pesos      =  0
            END IF
            IF       l_pesos          IS   NULL    THEN
                     LET      l_acciones   =  0
            END IF
            IF       g_grupo                <=  4  THEN 
                     LET      det.saldos_1a4    =  det.saldos_1a4  CLIPPED,
                              l_pesos  USING  "&&&&&&&&&&&&&"  CLIPPED
            END IF
            IF       g_grupo                 =  5       THEN
                     LET     det.udis_bono  = l_acciones USING "&&&&&&&&&&&&&"
            END IF
            IF       g_grupo                >=  6  THEN 
                     LET    det.saldos_issste =  det.saldos_issste  CLIPPED,
                            l_pesos  USING  "&&&&&&&&&&&&&"  CLIPPED
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
     FROM   afi_domicilio s,  afi_mae_afiliado  afi
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


FUNCTION F_580_trae_comision()
  DEFINE   cat            RECORD
           afore_x                               CHAR(03),
           rendim_afo                            DECIMAL(4,2),
           comis_afo                             DECIMAL(5,2),
           rendim_neto                           DECIMAL(4,2)
                          END RECORD,

           l_rendi                               CHAR(1014),
           afore                                 CHAR(20),
           comision                              CHAR(07),
           rendim_afo                            CHAR(06),
           rendim_neto                           CHAR(06),
           cuantos                               SMALLINT,
           afo_pro                               CHAR(20),
           com_pro                               CHAR(07),
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

   IF       cuantos                  <  16       THEN
            PROMPT "No estan completas las Afores en el Comparativo, <Enter> "
            FOR  g_enter
   END IF

   INITIALIZE afo_pro, com_pro, l_rendi        TO  NULL
   DECLARE  apt_comis_trasp      CURSOR  FOR
   SELECT   a.afore_cod, a.rendimiento, a.comision, a.rendimiento_neto
     FROM   tab_rendimiento_neto a
    WHERE   reg_taa_cd_ctr_folio.fecha_liquidacion
            BETWEEN a.fecha_ini       AND  a.fecha_fin
      AND   a.siefore_cod = vsiefore
    ORDER   BY  a.rendimiento_neto     DESC;

   FOREACH  apt_comis_trasp       INTO  cat.*
           INITIALIZE  afore, comision, rendim_afo, rendim_neto    TO  NULL
           IF       cat.afore_x         =  "999"    THEN
                    LET      afo_pro    = "PROMEDIO            "
                    LET      com_pro    =  cat.comis_afo USING "##&.&&","%"
                    CONTINUE FOREACH
           END IF

           SELECT   m.afore_desc 
             INTO   afore 
             FROM   tab_afore m
            WHERE   m.afore_cod          =  cat.afore_x;

           FOR      i        =   1   TO  20
                    IF      afore[i]      IS NULL    OR
                            afore[i]       = ' '     THEN
                            LET      afore[i]      =  "?"
                    END  IF
           END FOR

           LET      l_rendi                    =  l_rendi             CLIPPED,
                    afore                                             CLIPPED
           LET      l_rendi                    =  l_rendi             CLIPPED,
                    cat.rendim_afo  USING "#&.&&","%"                 CLIPPED
           LET      l_rendi                    =  l_rendi             CLIPPED,
                    cat.comis_afo   USING "##&.&&","%"                CLIPPED
           LET      l_rendi                    =  l_rendi             CLIPPED,
                    cat.rendim_neto USING "#&.&&","%"                 CLIPPED
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
      PAGE     LENGTH   90

   FORMAT
      PAGE HEADER
        PRINT    COLUMN   01,cza.*

   ON EVERY ROW
        PRINT    COLUMN   01,
                 det.*
END REPORT

