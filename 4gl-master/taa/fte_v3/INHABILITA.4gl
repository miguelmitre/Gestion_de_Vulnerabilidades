#####################################################################
#  Modifica Fechas de inhabilitación  por traspaso   de Noviembre#
#  de  29/07/2011  a  07/29/2011                                    #
#####################################################################
DATABASE   safre_af
GLOBALS
    DEFINE    g_enter                  CHAR(02),
              g_tipo_traspaso          LIKE  taa_cd_ctr_folio.folio,
              g_nss                    CHAR(11),
              g_ejecuta                CHAR(1000),
              g_usuario                CHAR(08),
              g_origen                 CHAR(02),
              g_folio                  INTEGER,
              g_reg_procesar           INTEGER,
              g_reg_procesados         INTEGER,
              g_reg_actualizados       INTEGER,
              g_today                  DATE
   DEFINE   g_marca_entra                      ,
            cero                             SMALLINT
END GLOBALS

MAIN
     DEFINE   act          RECORD  LIKE  cta_act_marca.*
     DEFINE   his          RECORD  LIKE  cta_his_marca.*
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT

   LET     g_today    =  TODAY
   LET     cero               =  0
   LET     g_reg_procesar       =  0
   LET     g_reg_procesados     =  0
   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "INHABILITA"  ATTRIBUTE(BORDER)
   DISPLAY  "<<MODFECHA>>         TRASPASOS  AFORE-AFORE (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "     ACTUALIZA  FECHAS  DE  LIQUIDACION DE LA MARCA 120 DEL BIMESTRAL                    ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)
   DISPLAY  "    PROCESANDO  INFORMACION ......"  AT 16,5
   SELECT   COUNT(*)
     INTO   g_reg_procesar
      from taa_cd_det_cedido a
     where  fecha_trasp = "07/27/2011"
#    AND    n_seguro   = "01018310506"
      and  estado  =  103
     and  n_seguro not in(select nss from cta_act_marca
                  where  marca_cod  =  120);

   DISPLAY  "                                  "  AT 16,5
   DISPLAY  "    REGISTROS A PROCESAR :  ",g_reg_procesar  USING "###,###"  AT 10,5
   PROMPT   "    TECLEE  SI  PARA EJECUTAR EL PROCESO  ...? "  for  g_enter
   IF       g_enter               IS  NULL     OR
            g_enter               <>  "SI"     THEN
            EXIT  PROGRAM
   END IF
   DISPLAY  "    PROCESANDO  INFORMACION ......"  AT 16,5

   CALL     f_100_marca_desmarca()
   DECLARE  C_FECHA   CURSOR  FOR
   SELECT   *
     FROM   cta_act_marca
    WHERE   marca_cod         =  120
#    AND    nss        = "01018310506"
      AND   fecha_ini         = "08/01/2011"
      AND   nss   IN(SELECT   n_seguro
                       FROM   taa_cd_det_cedido
                      WHERE   fecha_trasp      =  "07/27/2011"
                        AND   estado           =  103)
   ORDER BY nss
   FOREACH  C_FECHA    INTO   act.*
        SELECT   *
          INTO   his.*
          FROM   cta_his_marca
         WHERE   nss                 =  act.nss
           AND   marca_cod           =  120
           AND   fecha_ini           =  "08/01/2011";
 
        LET    his.fecha_ini         =  "07/27/2011"
        LET    his.fecha_causa       =  "07/27/2011"
        INSERT    INTO       cta_his_marca
                  VALUES    (his.*)
        UPDATE    cta_act_marca
           SET    fecha_ini          =  "07/27/2011",
                  fecha_causa        =  "07/27/2011"
         WHERE    nss                =  act.nss
           AND    marca_cod          =  120
           AND    fecha_ini          =  "08/01/2011";
        DELETE    FROM    cta_his_marca
         WHERE    nss                =  act.nss
           AND    marca_cod          =  120
           AND    fecha_ini          =  "08/01/2011";
        UPDATE    cta_his_marca
           SET    fecha_ini          =  "07/14/2011",
                  fecha_fin          =  "07/27/2011"
         WHERE    nss                =  act.nss
           AND    fecha_ini          =  "08/01/2011"
           AND    fecha_fin          =  "08/01/2011";
        LET       g_reg_procesados   =  g_reg_procesados  +  1
        DISPLAY  "    REGISTROS PROCESADOS :  ",g_reg_procesados USING "###,###"  AT 11,5
   END  FOREACH
   PROMPT  "  PROGRAMA TERMINADO....  <ENTER> PARA SALIR ...."  FOR  g_enter
END MAIN

FUNCTION  f_100_marca_desmarca()
   DEFINE    g_sql_51             CHAR(1000)
   LET      g_sql_51                  =
            ' EXECUTE  PROCEDURE safre_af:marca_cuenta(',
            ' ?,?,?,?,?,?,?,?) ; '
 PREPARE  sql_51          FROM     g_sql_51 
   declare   c_esp    cursor  for  
    select folio,n_seguro,tipo_traspaso
      from taa_cd_det_cedido a
     where  fecha_trasp = "07/27/2011"
      and  estado  =  103
#    AND    n_seguro   = "01018310506"
     and  n_seguro not in(select nss from cta_act_marca
                  where  marca_cod  =  120)
   FOREACH   c_esp        INTO   g_folio,g_nss,g_origen
        SELECT   A.marca_cod,user
             INTO    g_marca_entra,g_usuario
              FROM     safre_af:taa_cd_tipo_traspaso A 
              WHERE    A.tipo_traspaso            =  g_origen
        CALL     f_100_marca()
        CALL     F_225_desmarcar()
   END FOREACH
END  FUNCTION
FUNCTION  f_100_marca()
   DEFINE   l_marca_causa                    ,
            l_estado_marca                   ,
            l_codigo_rechazo                 ,
            xcodigo_marca                    ,
            xcodigo_rechazo                  SMALLINT
   DEFINE   l_fecha_causa                    ,
            l_today                          DATE
            LET      l_marca_causa                  =  cero
            LET      l_estado_marca                 =  cero
            LET      l_codigo_rechazo               =  cero
            LET      l_fecha_causa                  =  NULL
            DECLARE  cursor_marca        CURSOR   FOR   sql_51
            OPEN     cursor_marca        USING
                     g_nss,
                     g_marca_entra                , # marca
                     g_folio   , #envia folio como correlati
                     l_estado_marca               , #envia 0-acept 30-rech prev
                     l_codigo_rechazo             , # envia  cod_rech  en  0
                     l_marca_causa                , # envia  0
                     l_fecha_causa                , # envia  NULLA
                     g_usuario                      # envia  usuario de sesion
            FETCH    cursor_marca
                     INTO     xcodigo_marca,xcodigo_rechazo
                     CLOSE    cursor_marca

#            IF xcodigo_rechazo <> 0 THEN
#                 display  "xcodigo_rechazo=",xcodigo_rechazo
#            END IF

END FUNCTION

FUNCTION    F_225_desmarcar()
   LET      g_ejecuta               =
           "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta               =  g_ejecuta   CLIPPED
   PREPARE  clausula_spl1        FROM  g_ejecuta
   EXECUTE  clausula_spl1       USING  g_nss,
                                       g_marca_entra,
                                       g_folio,
                                       cero,
                                       g_marca_entra,
                                       g_usuario
END FUNCTION

