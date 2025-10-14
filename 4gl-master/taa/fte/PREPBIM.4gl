#########################################################################
#PROGRAMA  PREPBIM.4gl: PREPARA LAS SOLICITUDES DE TRASPASOS NORMAL E   #
#                       INTERNET DEL BIMESTRE PARA SER ENVIADAS EN UN   #
#                       SOLO ARCHIVO  EL  19-ENE-2009.                  #
# AUTOR : JOSE FRANCISCO LUGO CORNEJO.                                  #
#########################################################################

DATABASE  safre_af
GLOBALS
    DEFINE    g_enter                  CHAR(02),
              g_tipo_traspaso          LIKE  taa_cd_ctr_folio.folio,
              g_nss                    CHAR(11),
              g_usuario                CHAR(08),
              g_reg_procesar           INTEGER,
              g_reg_procesados         INTEGER,
              g_reg_actualizados       INTEGER,
              g_today                  DATE,
              g_folio_ant              LIKE  taa_cd_ctr_folio.folio,
              g_folio_bim              LIKE  taa_cd_ctr_folio.folio
END GLOBALS
MAIN
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     f_010_inicio()   
   CALL     f_050_habilita_sol_reciente()
   LET      g_tipo_traspaso         =  1  #Normal
   CALL     f_100_proceso()
   LET      g_tipo_traspaso         =  3  #Internet
   CALL     f_100_proceso()
   PROMPT   "   PROCESO FINALIZADO TECLEE ENTER PARA SALIR ...? "  for  g_enter
END MAIN

FUNCTION    f_010_inicio()
   DEFINE   l_marcas_activas           ,
            l_reasignado               SMALLINT
   CALL     STARTLOG("PREPBIM.log")
   LET      g_today                    =  TODAY
   LET      g_reg_procesar             =  0
   LET      g_reg_procesados           =  0
   LET      g_reg_actualizados         =  0
   LET      l_marcas_activas           =  0
   LET      l_reasignado               =  0

   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "PREPBIM"  ATTRIBUTE(BORDER)
   DISPLAY  "<<PREPBIM>>         TRASPASOS  AFORE-AFORE (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "     REESTRUCTURACION  DE  SOLICITUDES  PARA  PROCESO  BIMESTRAL",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)
   SELECT   COUNT(*)
     INTO   l_marcas_activas
     FROM   safre_af:cta_convivencia
    WHERE   marca_activa     IN(SELECT  marca_cod  FROM  taa_cd_tipo_traspaso)
      AND   marca_entra      IN( 231,232,236,540,542,490,820,825,830,840,850,
                                 860,880,870,875)
      AND   convive_cod       =  0
      AND   rechazo_cod       =  0;
   IF       l_marcas_activas        >  0   THEN
            DISPLAY  " CONVIVENCIA DE RETIROS ACTIVA PARA CONTINUAR EL PROCESO DEBE CERRARLA... "   AT  8,2
            PROMPT  "    TECLEE  <ENTER> PARA SALIR...."   FOR  g_enter
            EXIT   PROGRAM
   END IF
   SELECT   COUNT(UNIQUE  n_seguro),user
     INTO   g_reg_procesar,g_usuario
     FROM   safre_af:taa_cd_det_cedido
    WHERE   fecha_trasp             BETWEEN   "12/01/2008"  AND  "01/29/2009"
      AND   estado                       IN(101,102);
    DISPLAY  " REGISTROS A PROCESAR ===> ", g_reg_procesar  AT  10,5
   PROMPT   "       TECLEE  SI  PARA EJECUTAR EL PROCESO  ...? "  for  g_enter
   IF       g_enter           IS  NULL    OR    
            g_enter           <>  "SI"    THEN
             PROMPT  "PROCESO  CANCELADO  TECLEE ENTER  PARA  SALIR ......"
                     FOR   g_enter
             EXIT  PROGRAM
   END IF
   DISPLAY  "    PROCESANDO  INFORMACION ......"  AT 16,5
   SELECT   estado
     INTO   l_reasignado
     FROM   safre_af:taa_cd_edo_cedente
    WHERE   estado               =  109;
   IF       l_reasignado        IS  NULL   OR
            l_reasignado                 <>  109     THEN
            INSERT   INTO    safre_af:taa_cd_edo_cedente
                    VALUES  (109,"REASIGNADO","safre",g_today,3);         
   END IF
END FUNCTION

FUNCTION    f_050_habilita_sol_reciente()
   DEFINE   l_es_28_20              SMALLINT,
            l_max_fecha_presen      DATE
   DEFINE   l_folio_ant             LIKE  taa_cd_ctr_folio.folio
   LET      l_es_28_20                  =  0
   DECLARE  c_dupli     CURSOR    FOR
   SELECT   UNIQUE  n_seguro
     FROM   safre_af:taa_cd_det_cedido
    WHERE   estado           =  11
      AND   fecha_trasp   BETWEEN  "12/01/2008"  AND  "01/29/2009"
    ORDER   BY   1;
   FOREACH  c_dupli       INTO  g_nss
      LET      l_es_28_20                   =  0
      SELECT   COUNT(*)         INTO  l_es_28_20
        FROM   safre_af:taa_cd_det_cedido
       WHERE   n_seguro            =  g_nss
         AND   fecha_trasp         BETWEEN  "12/01/2008"  AND  "01/29/2009"
         AND   estado             IN(101,102);
      IF       l_es_28_20                >  0      THEN
               SELECT   MAX(fecha_presentacion)
                 INTO   l_max_fecha_presen
                 FROM   safre_af:taa_cd_det_cedido
                WHERE   n_seguro              =  g_nss
                  AND   fecha_trasp    BETWEEN "12/01/2008" AND "01/29/2009"
               
               UPDATE   safre_af:taa_cd_det_cedido
                  SET   estado                =  11
                WHERE   n_seguro              =  g_nss
                  AND   fecha_trasp    BETWEEN "12/01/2008" AND "01/29/2009"

               UPDATE   safre_af:taa_cd_det_cedido
                  SET   estado                =  101
                WHERE   n_seguro              =  g_nss
                  AND   fecha_presentacion    =  l_max_fecha_presen;
      END IF
   END  FOREACH
END  FUNCTION
     
FUNCTION    f_100_proceso()
   SELECT   MAX(folio)
     INTO   g_folio_bim
     FROM   safre_af:taa_cd_ctr_folio
   WHERE    tipo_traspaso             =  g_tipo_traspaso
     AND    fecha_liquidacion     BETWEEN    "12/01/2008"  AND  "01/29/2009";

   UPDATE   taa_cd_ctr_folio
      SET   estado                    =  109          
    WHERE   tipo_traspaso             =  g_tipo_traspaso
      AND   fecha_liquidacion     BETWEEN    "12/01/2008"  AND  "01/29/2009";

   UPDATE   taa_cd_ctr_folio
      SET   estado                    =  101,
            fecha_envio_saldos        =  "01/19/2009",
            fecha_liquidacion         =  "01/29/2009"
    WHERE   folio                     =  g_folio_bim;
   UPDATE   safre_af:taa_cd_det_cedido
      SET   fecha_trasp         = "01/29/2009"
    WHERE   folio               =  g_folio_bim;
   CALL     f_130_asigna_folio_bimestral()
   CALL     f_160_desmarca_ctas_con_retiro()
END FUNCTION

FUNCTION    f_130_asigna_folio_bimestral()
   DEFINE   l_tipo_traspaso            SMALLINT
   DEFINE   l_marca_cod                SMALLINT
   DEFINE   l_act_marca      RECORD       LIKE  cta_act_marca.*
   DECLARE  c_ctr_folio    CURSOR  FOR
   SELECT   folio 
     FROM   safre_af:taa_cd_ctr_folio
    WHERE   tipo_traspaso            =  g_tipo_traspaso
      AND   estado                 IN(101,102,109)
      AND   fecha_liquidacion     BETWEEN    "12/01/2008"  AND  "01/29/2009"
    ORDER   BY  1   DESC;
   FOREACH  c_ctr_folio      INTO   g_folio_ant
          DECLARE   c_det_cedido      CURSOR  FOR
           SELECT   UNIQUE  n_seguro
             FROM   safre_af:taa_cd_det_cedido
            WHERE   folio                        =  g_folio_ant
              AND   estado                      IN (101,102)
            ORDER   BY  1;
          FOREACH   c_det_cedido       INTO   g_nss
               LET      g_reg_procesados         =  g_reg_procesados   +  1 
               DISPLAY  " REGISTROS PROCESADOS ===> ", g_reg_procesados AT 11,5
                SELECT  UNIQUE  *  
                  INTO  l_act_marca.*
                  FROM  safre_af:cta_act_marca
                 WHERE  nss                      =  g_nss
                   AND  estado_marca             =  0
                   AND  marca_cod    IN(SELECT  marca_cod
                                          FROM  taa_cd_tipo_traspaso);
                IF      l_act_marca.marca_cod   IS  NULL  OR
                        l_act_marca.marca_cod   =  0   THEN
                        CONTINUE  FOREACH
                END IF
                DELETE  FROM    safre_af:cta_act_marca
                 WHERE  nss                      =  g_nss
                   AND  estado_marca             =  0
                   AND  marca_cod    IN(SELECT  marca_cod
                                          FROM  taa_cd_tipo_traspaso);
                UPDATE  safre_af:cta_his_marca
                   SET  correlativo              =  g_folio_bim
                 WHERE  nss                      =  g_nss
                   AND  estado_marca             =  0
                   AND  marca_cod    IN(SELECT  marca_cod
                                          FROM  taa_cd_tipo_traspaso);
                LET     l_act_marca.correlativo  =  g_folio_bim
                INSERT   INTO  safre_af:cta_act_marca
                         VALUES   (l_act_marca.*);
                UPDATE  taa_cd_det_cedido
                   SET  folio                    =  g_folio_bim,
                        estado                   =  101,
                        fecha_trasp              = "01/29/2009"
                 WHERE  n_seguro                 =  g_nss
                   AND  estado                  IN(101,102)
                   AND  folio                    =  g_folio_ant;
          END  FOREACH
          DELETE   FROM    safre_af:dis_provision
           WHERE   folio                         =  g_folio_ant;
   END  FOREACH

END  FUNCTION

FUNCTION    f_160_desmarca_ctas_con_retiro()
   DEFINE   l_marca_entra                      ,
            l_con_retiro                       ,
            l_causa                            SMALLINT
   DEFINE   l_ejecuta                          CHAR(300)
   DECLARE  c_desmarca        CURSOR   FOR
   SELECT   n_seguro,tp.marca_cod
     FROM   safre_af:taa_cd_det_cedido cd, safre_af:taa_cd_tipo_traspaso tp
    WHERE   folio                 =  g_folio_bim
      AND   estado                =  101
      AND   cd.tipo_traspaso      =  tp.tipo_traspaso
   FOREACH  c_desmarca           INTO  g_nss,l_marca_entra
        LET      l_con_retiro     =  0
        SELECT   MAX(marca_cod)
          INTO   l_con_retiro
          FROM   safre_af:cta_act_marca
         WHERE   nss              =  g_nss
           AND   marca_cod       IN( 231, 232, 236, 490, 540, 542, 820, 825,
                                     830, 840, 850, 860, 870, 875, 880)
        IF       l_con_retiro          IS  NULL    OR
                 l_con_retiro           <  1        THEN
                 CONTINUE  FOREACH
        END IF
                 
        LET      l_causa               =  30
        LET      l_ejecuta             =
                 "EXECUTE PROCEDURE safre_af:desmarca_cuenta(?,?,?,?,?,?)"
        LET      l_ejecuta             =  l_ejecuta    CLIPPED
        PREPARE  des_rch_sal  FROM   l_ejecuta
        EXECUTE  des_rch_sal  USING  g_nss                   ,
                                     l_marca_entra           ,
                                     g_folio_bim             ,
                                     l_causa                 ,
                                     l_marca_entra           ,
                                     g_usuario
        UPDATE   safre_af:taa_cd_det_cedido
           SET   estado               =  4
         WHERE   n_seguro             =  g_nss
           AND   folio                =  g_folio_bim;
   END FOREACH
END FUNCTION
