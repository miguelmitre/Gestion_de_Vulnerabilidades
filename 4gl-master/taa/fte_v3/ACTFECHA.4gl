####################################################################
#  Cambia fechas de inhabilitación por traspaso de "07/28/2010" a  #
#  05/28/2010                                                      #
####################################################################
DATABASE   safre_af
GLOBALS
    DEFINE    g_enter                  CHAR(02),
              g_tipo_traspaso          LIKE  taa_cd_ctr_folio.folio,
              g_nss                    CHAR(11),
              g_usuario                CHAR(08),
              g_reg_procesar           INTEGER,
              g_reg_procesados         INTEGER,
              g_reg_actualizados       INTEGER,
              g_today                  DATE
END GLOBALS

MAIN
     DEFINE   act          RECORD  LIKE  cta_act_marca.*
     DEFINE   his          RECORD  LIKE  cta_his_marca.*
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT

   LET     g_today    =  TODAY
   LET     g_reg_procesar       =  0
   LET     g_reg_procesados     =  0
   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "ACTFECHA"  ATTRIBUTE(BORDER)
   DISPLAY  "<<ACTFECHA>>         TRASPASOS  AFORE-AFORE (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  " ACTUALIZA FECHA DE INHABI. POR TRASPASO DE 07/28/2010 a 05/28/2010          ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)
   DISPLAY  "    PROCESANDO  INFORMACION ......"  AT 16,5
   SELECT   COUNT(*)
     INTO   g_reg_procesar
     FROM   cta_act_marca
    WHERE   marca_cod         =  120
      AND   fecha_ini         =  "07/28/2010"
      AND   nss   IN(SELECT   n_seguro
                       FROM   taa_cd_det_cedido
                      WHERE   fecha_trasp      =  "05/28/2010"
                        AND   estado           =  103);
   DISPLAY  "                                  "  AT 16,5
   DISPLAY  "    REGISTROS A PROCESAR :  ",g_reg_procesar  USING "###,###"  AT 10,5
   PROMPT   "    TECLEE  SI  PARA EJECUTAR EL PROCESO  ...? "  for  g_enter
   IF       g_enter               IS  NULL     OR
            g_enter               <>  "SI"     THEN
            EXIT  PROGRAM
   END IF
   DISPLAY  "    PROCESANDO  INFORMACION ......"  AT 16,5

   DECLARE  C_FECHA   CURSOR  FOR
   SELECT   *
     FROM   cta_act_marca
    WHERE   marca_cod         =  120
      AND   fecha_ini         = "07/28/2010"
      AND   nss   IN(SELECT   n_seguro
                       FROM   taa_cd_det_cedido
                      WHERE   fecha_trasp      =  "05/28/2010"
                        AND   estado           =  103)
   ORDER BY nss
   FOREACH  C_FECHA    INTO   act.*
        SELECT   *
          INTO   his.*
          FROM   cta_his_marca
         WHERE   nss                 =  act.nss
           AND   marca_cod           =  120
           AND   fecha_ini           =  "07/28/2010";
 
        LET      his.fecha_ini       =  "05/28/2010"
        LET      his.fecha_causa     =  "05/28/2010"
        INSERT    INTO       cta_his_marca
                  VALUES    (his.*)
        UPDATE    cta_act_marca
           SET    fecha_ini          =  "05/28/2010",
                  fecha_causa        =  "05/28/2010"
         WHERE    nss                =  act.nss
           AND    marca_cod          =  120
           AND    fecha_ini          =  "07/28/2010";
        DELETE    FROM    cta_his_marca
         WHERE    nss                =  act.nss
           AND    marca_cod          =  120
           AND    fecha_ini          =  "07/28/2010";
   LET    g_reg_procesados   =  g_reg_procesados  +  1
   DISPLAY  "    REGISTROS PROCESADOS :  ",g_reg_procesados USING "###,###"  AT 11,5
   END  FOREACH
   PROMPT  "  PROGRAMA TERMINADO....  <ENTER> PARA SALIR ...."  FOR  g_enter
END MAIN

