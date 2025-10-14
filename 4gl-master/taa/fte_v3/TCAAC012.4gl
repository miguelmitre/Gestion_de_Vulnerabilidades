#############################################################################
#Proyecto          => AFORE ( MEXICO )                                      #
#Propietario       => E.F.P.                                                #
#Programa TCAAC012 => REVERSA CALCULO DE RESARCIMIENTO  Y                   #
#                  => LIQUIDACION DE RESARCIMIENTO.                         #
#                  => AFORE CEDENTE                                         #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                           #
#Fecha creacion    => 24 DE AGOSTO DE 2007.                                 #
#Sistema           => TCAA                                                  #
#############################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   s_codigo_afore                 LIKE  tab_afore_local.codigo_afore
   DEFINE   taa_cd_ctr_folio_ind   RECORD  LIKE  taa_cd_ctr_folio_ind.* 
   DEFINE
            vfecha                         ,
            g_fecha_ini                    ,
            g_today                        DATE
   DEFINE
            g_estado                       ,
            g_procedente                   ,
            g_provisionado                 ,
            g_liquidado                    ,
            g_marca_cod                    ,
            f_opc_reverso                  SMALLINT
   DEFINE
            g_enter                        CHAR(1),
            g_usuario                      CHAR(10),
            ejecuta                        CHAR(100)
END GLOBALS

MAIN
   OPTIONS
            PROMPT    LINE LAST,
            ACCEPT    KEY CONTROL-I
            DEFER     INTERRUPT
   CALL     f_001_inicio()
   CALL     f_200_proceso()
END MAIN

FUNCTION    f_001_inicio()
   CALL     STARTLOG("TCAAC012.log")
   LET      g_today                         =  TODAY
   LET      taa_cd_ctr_folio_ind.folio      =  0
   SELECT   codigo_afore,user
     INTO   s_codigo_afore,g_usuario
     FROM   tab_afore_local;
   SELECT   a.estado
     INTO   g_procedente
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'PROCEDENTE'
      AND   a.tipo                    = 2;
   SELECT   a.estado
     INTO   g_provisionado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'PROVISIONADA'
      AND   a.tipo                    = 3;
   SELECT   a.estado
     INTO   g_liquidado
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'LIQUIDADA'
      AND   a.tipo                    = 3;
END FUNCTION

FUNCTION    f_200_proceso()
   OPEN     WINDOW     TCAAC0081              AT  2,2 
            WITH     FORM     "TCAAC012"     ATTRIBUTE(BORDER)
   DISPLAY  "TCAAC012   REVERSA OPERACIONES DE TRASPASOS INDEBIDOS  ",
            " (CEDENTE)                       "   AT  1,1  ATTRIBUTE(REVERSE)   
   DISPLAY  " < ESC >  Reversa Operacion      < CTRL-C >  ",
            " Cancela Reverso    "    AT   9,10
   DISPLAY  g_today      USING  "DD-MM-YYYY"   AT  1,69  ATTRIBUTE(REVERSE)
   DISPLAY  "          TECLEE   EL  TIPO  DE  REVERSO                ",
            "                                 "  AT   3,1  ATTRIBUTE(REVERSE)   
   DISPLAY  "                         DATOS  DEL  FOLIO  A  REVERSAR",
            "                                 "  AT  11,1  ATTRIBUTE(REVERSE)
   INPUT BY NAME    f_opc_reverso
            AFTER     FIELD    f_opc_reverso
                      IF       f_opc_reverso          IS  NULL   THEN
                               ERROR    "  Tipo de Reverso  : 1=Calc. Resarcir",
                                        "   2=Liquidacion de Resarcimiento "
                               NEXT     FIELD     f_opc_reverso
                      END IF
            ON KEY ( ESCAPE )
                      EXIT INPUT
            ON KEY ( INTERRUPT )
                      EXIT PROGRAM
   END      INPUT
   CASE     f_opc_reverso
            WHEN     1
                     CALL    f_220_checa_provision()
            WHEN     2
                     CALL    f_230_checa_liquidacion()
   END CASE
   CALL     f_240_display_fechas()
   WHILE    TRUE
            PROMPT   "  ESTA SEGURO DE CONTINUAR EL REVERSO [S/N] ? "
                     FOR  g_enter
            IF       g_enter        MATCHES "[sSnN]"     THEN 
                     IF       g_enter MATCHES "[sS]"       THEN
                              EXIT WHILE
                     ELSE
                              EXIT PROGRAM
                     END IF
            END IF
   END WHILE
   DISPLAY  " REVERSANDO  OPERACION.............   " 
            AT  18,1 ATTRIBUTE(REVERSE)
   CASE     f_opc_reverso
            WHEN     1
                     CALL     f_260_reversa_provision()
            WHEN     2
                     CALL     f_270_reversa_liquidacion()
   END CASE
   DISPLAY  "                                                             " 
            AT  18,1
   PROMPT   " REVERSO  FINALIZADO  CORRECTAMENTE.....  TECLEE",
            "  <Enter> Para Salir    "  ATTRIBUTE(REVERSE)   FOR  g_enter
   CLOSE    WINDOW   TCAAC0081
END FUNCTION

FUNCTION    f_220_checa_provision()
   SELECT   f.*
     INTO   taa_cd_ctr_folio_ind.*
     FROM   safre_af:taa_cd_ctr_folio_ind  f
    WHERE   f.fecha_provision             <=  g_today
      AND   f.estado                       =  g_provisionado;
   IF       taa_cd_ctr_folio_ind.folio     =  0    THEN
            PROMPT   "  NO  HAY  FOLIO  A  REVERSAR  TECLEE  <ENTER>",
                              "  PARA SALIR   "     FOR g_enter
            EXIT     PROGRAM
   END IF
END FUNCTION

FUNCTION    f_230_checa_liquidacion()
   SELECT   f.*
     INTO   taa_cd_ctr_folio_ind.*
     FROM   safre_af:taa_cd_ctr_folio_ind  f
    WHERE   f.fecha_liquidacion            =  g_today
      AND   f.estado                       =  g_liquidado 
   IF       taa_cd_ctr_folio_ind.folio     =  0    THEN
            PROMPT  "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR"  FOR  g_enter
            EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION    f_240_display_fechas()
   DISPLAY  BY  NAME     f_opc_reverso
   DISPLAY  taa_cd_ctr_folio_ind.folio              TO  F_folio
   DISPLAY  taa_cd_ctr_folio_ind.fecha_provision    TO  F_fecha_envio_saldos
   DISPLAY  taa_cd_ctr_folio_ind.fecha_liquidacion  TO  F_fecha_liquidacion
   ERROR    "   ESTE  ES  EL  FOLIO  A  REVERSAR  FAVOR ",
            " DE  VERIFICARLO ......... " 
END FUNCTION

FUNCTION    f_260_reversa_provision()
   DELETE   FROM     safre_af:dis_provision
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio;

   DELETE   FROM     safre_af:taa_cd_his_rendi_dia
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio;

   UPDATE   safre_af:taa_cd_indebidos
      SET   folio                     =  NULL,
            estado                    =  g_procedente
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio
      AND   estado                    IN(102,921);

   DELETE   FROM    safre_af:taa_cd_ctr_folio_ind
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio;
END FUNCTION

FUNCTION    f_270_reversa_liquidacion()
   UPDATE   safre_af:taa_cd_indebidos
      SET   estado                    =  g_provisionado
     WHERE  folio                     =  taa_cd_ctr_folio_ind.folio
      AND   estado                    =  g_liquidado;
 
   DELETE   FROM   safre_af:dis_cuenta
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio
     AND    subcuenta                   BETWEEN  1     AND    20
     AND    tipo_movimiento             BETWEEN  777   AND   779;

   UPDATE   safre_af:taa_cd_ctr_folio_ind
      SET   estado                    =  g_provisionado
    WHERE   folio                     =  taa_cd_ctr_folio_ind.folio;
END FUNCTION
