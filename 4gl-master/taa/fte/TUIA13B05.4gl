##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TUIA13B05  => REVERSA  Intra-Afore Cuentas ISSSTE ACTIVAS       #
#Fecha             => 29 DE AGOSTO     DE 2011                           #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TUI Intra-Afore Cuentas ISSSTE ACTIVAS Oper.13     #
##########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   reg_1           RECORD
            folio                           INTEGER        ,
            estado                          CHAR(17)       ,
            f_presentacion                  DATE           ,
            f_provision                     DATE           ,
            f_liquidacion                   DATE
                                      END   RECORD
   DEFINE
            g_today                           DATE,
            g_vencida                       SMALLINT
   DEFINE
            g_enter                         CHAR
END GLOBALS

MAIN
   OPTIONS 
            PROMPT    LINE LAST,
            INPUT     WRAP,
            ACCEPT    KEY      CONTROL-I
#           DEFER     INTERRUPT
   CALL     F_001_modifica_folios()
END MAIN

FUNCTION    F_001_modifica_folios()
   DEFINE   l_select                  ,
            l_construct               CHAR(1000)
   CALL     STARTLOG  ("TUIA13B05.log")
   LET      g_today               =  TODAY
   OPEN     WINDOW    ventana_1      AT  2,2 
            WITH      FORM    "TUIA13B05"     ATTRIBUTE(BORDER)
   DISPLAY  " <TUIA13B05>  REVERSA OPERACIONES TRASPASOS UNIF. INTRA-AFORE CTAS. ISSSTE ACTIVAS  ", "                 "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  "      <CONTROL-B>   CARGA OP. 13                                              ", "                 "    AT  3,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING   "DD-MM-YYYY"   AT   5,63   ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Enter>> Muestra Folios             <<Ctrl-c>> Salir",
            "                         "      AT   5,2    ATTRIBUTE(REVERSE)
   LET      int_flag                  =  FALSE
   SELECT   MAX(F.folio)
     INTO   reg_1.folio
     FROM   safre_af:tui13_ctr_folio   F
   IF       reg_1.folio        IS  NULL    OR
            reg_1.folio               =  0     THEN
            ERROR    "   NO   EXISTE  FOLIO  A  REVERSAR...         "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            EXIT  PROGRAM
   END IF

    SELECT  a.folio,
            b.descripcion,
            a.fecha_presentacion,
            a.fecha_provision,
            a.fecha_liquidacion 
     INTO   reg_1.*
     FROM   safre_af:tui13_ctr_folio a, tui09_estados b
    WHERE   a.folio             =  reg_1.folio
      AND   a.estado            =  b.estado
   DISPLAY  reg_1.folio               TO  FORMONLY.folio
   DISPLAY  reg_1.estado              TO  FORMONLY.desc_estado
   DISPLAY  reg_1.f_presentacion      TO  FORMONLY.f_presentacion
#  DISPLAY  reg_1.f_provision         TO  FORMONLY.f_provision
#  DISPLAY  reg_1.f_liquidacion       TO  FORMONLY.f_liquidacion
   INPUT    BY NAME   reg_1.folio  WITHOUT  DEFAULTS 
            ON KEY ( CONTROL-B )
                     ERROR    " PROCESANDO  REVERSO...                     "
                     CALL     f_020_reversa_carga_op13() 
            ON KEY(INTERRUPT)
                     ERROR    " MODIFICACION  TERMINADA..."
                     SLEEP    2
                     ERROR    ""
                     CLEAR    FORM
                     EXIT     INPUT
   END      INPUT
   CLOSE    WINDOW    ventana_1
END FUNCTION


FUNCTION    f_020_reversa_carga_op13()
   DEFINE   l_num_bim_acum             ,
            l_tipo_cta                 ,
            l_marca_cod                SMALLINT,
            l_bimestres                CHAR(3),
            l_procedente               SMALLINT,
            g_det_noti    RECORD   LIKE    safre_af:tui13_det_noti_fin_uni.*
   SELECT   COUNT(*)
     INTO   l_procedente
     FROM   safre_af:tui13_ctr_folio  a
    WHERE   a.folio                  =  reg_1.folio
      AND   a.estado                 =  101;
   IF       l_procedente          IS  NULL    OR
            l_procedente           =  0      THEN
            ERROR   "   EL FOLIO NO ESTÁ PROCEDENTE FAVOR DE VERIFICAR...   "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            RETURN
   END IF
   --DECLARE  cur_saldos       CURSOR FOR
   --SELECT   *
   --  FROM   tui13_det_noti_fin_uni
   -- WHERE   folio                  =  reg_1.folio
   --   AND   tipo_diagnostico      IN(90,91,92)
   --FOREACH  cur_saldos         INTO    g_det_noti.*
   --         SELECT   num_bim_acum
   --           INTO   l_num_bim_acum
   --           FROM   afi_icefa_issste
   --          WHERE   nti               =  g_det_noti.nss;
   --         LET      l_num_bim_acum    =
   --                  l_num_bim_acum    -  g_det_noti.bimestres_cot_unificada
   --         LET      l_bimestres       =  l_num_bim_acum   USING "&&&"
   --
   --         UPDATE   afi_icefa_issste
   --            SET   num_bim_acum      =  l_bimestres
   --          WHERE   nti               =  g_det_noti.nss;
   --
   --         DELETE   FROM      afi_his_marcaje_peis
   --          WHERE   nti               =  g_det_noti.nss
   --            AND   tipo_cta          =  l_tipo_cta
   --            AND   fecha   BETWEEN   TODAY  AND  TODAY  -1;
   --END FOREACH

   DELETE   FROM   safre_af:tui13_cza_noti_fin_uni
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui13_det_noti_fin_uni
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui13_sum_noti_fin_uni
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui13_ctr_folio 
    WHERE   folio                  =  reg_1.folio;

   ERROR   "   REVERSO  DE  CARGA DE OP.13  FINALIZADO...         "
   PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
   EXIT  PROGRAM
END FUNCTION
