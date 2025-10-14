#############################################################################
#Proyecto          => AFORE ( MEXICO )                                      #
#Propietario       => E.F.P.                                                #
#Programa TCAAC008 => REVERSA RECEPCION DE ARCHIVO DE SOLICITUDES,          #
#                  => GENERACION DE SALDOS,LIQUIDACION Y RECHAZOS DE SALDOS #
#                  => AFORE CEDENTE                                         #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                           #
#Fecha creacion    => 31 DE ENERO DE 2004                                   #
#Sistema           => TCAA                                                  #
#############################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   s_codigo_afore                 LIKE  tab_afore_local.codigo_afore
   DEFINE   reg_folio   RECORD  LIKE  taa_cd_ctr_folio.* 
   DEFINE
            vfecha                         ,
            g_fecha_ini                    ,
            g_today                        DATE
   DEFINE
            g_procedente                   ,
            g_edo_recibido                 ,
            g_provisionado                 ,
            g_liquidado                    ,
            g_rechazo_saldo                ,
            g_cero                         ,
            g_marca_cod                    ,
            f_opc_reverso                  ,
            F_tipo_traspaso                SMALLINT
   DEFINE
            g_enter                        CHAR(1),
            g_usuario                      CHAR(10),
            g_nss                          CHAR(11),
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
   CALL     STARTLOG("TCAAC008.log")
   LET      g_today                   =  TODAY
   LET      g_cero                    =  0
   LET      reg_folio.folio      =  0
   SELECT   codigo_afore,user
     INTO   s_codigo_afore,g_usuario
     FROM   tab_afore_local
   SELECT   a.estado
     INTO   g_procedente
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'PROCEDENTE'
   SELECT   a.estado
     INTO   g_rechazo_saldo
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'RECHAZO SALDO'
   SELECT   a.estado
     INTO   g_edo_recibido
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion             = 'RECIBIDO'
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
            WITH     FORM     "TCAAC0081"     ATTRIBUTE(BORDER)
   DISPLAY  "TCAAC008   REVERSA OPERACIONES DE TRASPASOS AFORE-AFORE",
            " (CEDENTE)                       "   AT  1,1  ATTRIBUTE(REVERSE)   
   DISPLAY  " < ESC >  Reversa Operacion      < CTRL-C >  ",
            " Cancela Reverso    "    AT   9,10
   DISPLAY  g_today      USING  "DD-MM-YYYY"   AT  1,69  ATTRIBUTE(REVERSE)
   DISPLAY  "          TECLEE   EL  TIPO  DE  TRASPASO  Y  TIPO  DE  ",
            "REVERSO                          "  AT   3,1  ATTRIBUTE(REVERSE)   
   DISPLAY  "                         DATOS  DEL  FOLIO  A  REVERSAR",
            "                                 "  AT  11,1  ATTRIBUTE(REVERSE)
   INPUT BY NAME   F_tipo_traspaso, f_opc_reverso
            AFTER     FIELD    F_tipo_traspaso
                      IF       F_tipo_traspaso        IS  NULL   THEN
                               ERROR     "  Tipo de Traspaso : 1=Solicitud ",
                               " 2=Complementario  3=Internet  4=Procesos   "
                               NEXT     FIELD          F_tipo_traspaso
                      END IF 
            AFTER     FIELD    f_opc_reverso
                      IF       f_opc_reverso           =  2      OR
                               f_opc_reverso          IS  NULL   THEN
                               ERROR    "  Tipo de Reverso  : 1=Carga Op. 01 ",
                                        "   3=Notificación de Saldos ",
                                        "4=Liquidacion   "
                               NEXT     FIELD     f_opc_reverso
                      END IF
            ON KEY ( ESCAPE )
                      EXIT INPUT
            ON KEY ( INTERRUPT )
                      EXIT PROGRAM
   END      INPUT
   IF       f_tipo_traspaso                    =  1   OR   
            f_tipo_traspaso                    =  3   OR
            f_tipo_traspaso                    =  4   THEN
            CASE     f_opc_reverso
                     WHEN     1
                              CALL    f_210_checa_carga_de_solicitudes()
                     WHEN     3
                              CALL    f_220_checa_provision()
                     WHEN     4
                              CALL    f_230_checa_liquidacion()
                     WHEN     5
                              CALL    f_220_checa_provision()
            END CASE
   ELSE                              
            CASE     f_opc_reverso
                     WHEN     3
                              CALL    f_220_checa_provision()
                     WHEN     4
                              CALL    f_230_checa_liquidacion()
                     WHEN     5
                              CALL    f_220_checa_provision()
            END CASE
   END IF
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
                     CALL     f_250_reversa_carga_de_solicitudes()
            WHEN     3
                     CALL     f_260_reversa_provision()
            WHEN     4
                     CALL     f_270_reversa_liquidacion()
            WHEN     5
                     CALL     f_275_reversa_rechazos()
   END CASE
   PROMPT   " REVERSO  FINALIZADO  CORRECTAMENTE.....  TECLEE",
            "  <Enter> Para Salir    "  ATTRIBUTE(REVERSE)   FOR  g_enter
   CLOSE    WINDOW   TCAAC0081
END FUNCTION

FUNCTION    f_210_checa_carga_de_solicitudes()
   DEFINE   l_folio                        DEC(10,0)
   SELECT   MAX(folio) 
     INTO   l_folio
     FROM   taa_cd_ctr_folio  f
    WHERE   f.estado                      IN(100,101)
      AND   f.tipo_traspaso                =  F_tipo_traspaso;
   SELECT   f.*
     INTO   reg_folio.*
     FROM   taa_cd_ctr_folio  f
    WHERE   f.folio                        =  l_folio
      AND   f.tipo_traspaso                =  F_tipo_traspaso
   IF       reg_folio.folio         IS  NULL    OR
            reg_folio.folio          =  0       THEN
            PROMPT    " NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR"
                      FOR      g_enter
            EXIT      PROGRAM
   END IF
END FUNCTION

FUNCTION    f_220_checa_provision()
   DEFINE   l_hay_rechazo                  SMALLINT
   DEFINE   l_folio                        DEC(10,0)
   SELECT   MAX(folio) 
     INTO   l_folio
     FROM   taa_cd_ctr_folio  f
    WHERE   f.estado                  =  g_provisionado
      AND   f.tipo_traspaso           =  F_tipo_traspaso;
   IF       l_folio                   =  0      OR
            l_folio                  IS NULL    THEN
            PROMPT   "  NO  HAY  FOLIO  A  REVERSAR  TECLEE  <ENTER>",
                     "  PARA SALIR   "     FOR g_enter
            EXIT     PROGRAM
   END IF
   SELECT   f.*
     INTO   reg_folio.*
     FROM   taa_cd_ctr_folio  f
    WHERE   f.folio                   =  l_folio;
   IF       f_opc_reverso             =  5     THEN
            SELECT   "1"
              INTO   l_hay_rechazo
              FROM   safre_af:taa_cd_cza_rch_sal  r
             WHERE   r.folio               =  reg_folio.folio;
            IF       NOT       l_hay_rechazo      THEN
                     PROMPT   "  NO EXISTE FOLIO CON RECHAZO A REVERSAR ",
                              "TECLEE  <ENTER>  PARA SALIR   "   FOR  g_enter
                     EXIT  PROGRAM
            END  IF
    END IF
END FUNCTION

FUNCTION    f_230_checa_liquidacion()
   SELECT   f.*
     INTO   reg_folio.*
     FROM   taa_cd_ctr_folio  f
    WHERE   f.fecha_liquidacion            =  g_today
      AND   f.estado                       =  g_liquidado 
      AND   f.tipo_traspaso                =  F_tipo_traspaso
   IF       reg_folio.folio       IS  NULL    OR 
            reg_folio.folio        =  0       THEN
            PROMPT  "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR"  FOR  g_enter
            EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION    f_240_display_fechas()
   DISPLAY  BY  NAME     f_tipo_traspaso
   DISPLAY  BY  NAME     f_opc_reverso
   DISPLAY  reg_folio.folio                 TO  F_folio
   DISPLAY  reg_folio.fecha_presentacion    TO  F_fecha_presentacion
   DISPLAY  reg_folio.fecha_envio_saldos    TO  F_fecha_envio_saldos
   DISPLAY  reg_folio.fecha_liquidacion     TO  F_fecha_liquidacion
   ERROR    "   ESTE  ES  EL  FOLIO  A  REVERSAR  FAVOR ",
            " DE  VERIFICARLO ......... " 
END FUNCTION

FUNCTION    f_250_reversa_carga_de_solicitudes()
   CALL     f_280_reversa_marcas()
   DELETE   FROM     taa_cd_rch_convivencia
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_ced_valida
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_sum_cedido
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_det_cedido
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_cza_cedido
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_correo_elect
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_af:afi_correo_elect
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     taa_cd_ctr_folio
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_tmp:taa_cd_cza_sol_tra_ind;
   DELETE   FROM     safre_tmp:taa_cd_det_sol_tra_ind;
   DELETE   FROM     safre_tmp:taa_cd_sum_sol_tra_ind;
   DELETE   FROM     safre_tmp:taa_cd_num_salarios
    WHERE   folio                     =  reg_folio.folio;
END FUNCTION

FUNCTION    f_260_reversa_provision()
   DELETE   FROM     dis_provision
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_tmp:taa_cd_saldos_arch
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_tmp:taa_cd_sum_arch
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_af:taa_cd_cza_rch_sal 
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_af:taa_cd_det_02_rch_sal 
    WHERE   folio                     =  reg_folio.folio;
   DELETE   FROM     safre_af:taa_cd_det_05_rch_sal 
    WHERE   folio                     =  reg_folio.folio;
   IF       F_tipo_traspaso           =  1   OR
            F_tipo_traspaso           =  4   THEN
            UPDATE   taa_cd_det_cedido
               SET   estado              =  101
             WHERE   folio               =  reg_folio.folio
               AND   estado            IN(102,106);
            UPDATE   taa_cd_ctr_folio
               SET   estado              =  101
             WHERE   folio               =  reg_folio.folio;
   ELSE
            DELETE   FROM     taa_cd_det_comple
             WHERE   folio               =  reg_folio.folio;
            DELETE   FROM     taa_cd_ctr_folio
             WHERE   folio               =  reg_folio.folio;
            UPDATE   taa_cd_det_cedido
               SET   estado              =  103
             WHERE   estado              =  12;
   END IF
END FUNCTION

FUNCTION    f_270_reversa_liquidacion()
   DELETE   FROM   dis_cuenta
    WHERE   folio                     =  reg_folio.folio;
   UPDATE   taa_cd_ctr_folio
      SET   estado                    =  102
    WHERE   folio                     =  reg_folio.folio;
   IF       F_tipo_traspaso           =  1   OR
            F_tipo_traspaso           =  4   THEN
            UPDATE   taa_cd_det_cedido
               SET   estado                 =  102
             WHERE   folio                  =  reg_folio.folio
               AND   estado                 =  103;
   ELSE
            UPDATE   taa_cd_det_cedido
               SET   estado                 =  12
             WHERE   estado                 =  103
               AND   n_seguro   IN(SELECT  n_seguro 
                                     FROM  taa_cd_det_comple
                                    WHERE  folio =  reg_folio.folio
                                      AND  estado    =  103);
            UPDATE   taa_cd_det_comple
               SET   estado                 =  102
             WHERE   folio                  =  reg_folio.folio
             AND     estado                 =  103;
   END IF
END FUNCTION

FUNCTION    f_275_reversa_rechazos()
   DEFINE   l_scta                         SMALLINT
   DEFINE   l_monto_en_pesos               DEC(16,2)

   UPDATE   dis_provision
      SET   estado                    =  6
    WHERE   estado                    =  106
      AND   folio                     =  reg_folio.folio;

   IF       F_tipo_traspaso           =  1   OR
            F_tipo_traspaso           =  4   THEN
            CALL     f_290_reversa_desmarcas()
            UPDATE   taa_cd_det_cedido
               SET   estado                 =  102
             WHERE   folio                  =  reg_folio.folio
               AND   estado                 =  106
   END IF
   IF       F_tipo_traspaso            =  2   THEN
            UPDATE   taa_cd_det_cedido
               SET   estado                 =  12
             WHERE   estado                 =  103
               AND   n_seguro    IN(SELECT  cmp.n_seguro 
                                      FROM  taa_cd_det_comple   cmp
                                     WHERE  cmp.folio   = reg_folio.folio
                                       AND  cmp.estado  =  106);
            UPDATE   taa_cd_det_comple
               SET   estado                 =  102
             WHERE   folio                  =  reg_folio.folio
               AND   estado                 =  106; 
   END IF
   DELETE   FROM     taa_cd_cza_rch_sal
    WHERE   folio                     =  reg_folio.folio;

   DELETE   FROM     taa_cd_det_02_rch_sal
    WHERE   folio                     =  reg_folio.folio;

   DELETE   FROM     taa_cd_det_05_rch_sal
    WHERE   folio                     =  reg_folio.folio;

   DELETE   FROM     taa_cd_sum_rch_sal
    WHERE   folio                     =  reg_folio.folio;
END FUNCTION

FUNCTION    f_280_reversa_marcas()
   DECLARE  cur_rev_marca         CURSOR   FOR
   SELECT   a.n_seguro,c.marca_cod,c.fecha_ini
     FROM   taa_cd_det_cedido a, taa_cd_tipo_traspaso b, cta_act_marca c
    WHERE   a.folio                   =  reg_folio.folio
      AND   a.n_seguro                =  c.nss
      AND   a.tipo_traspaso           =  b.tipo_traspaso
      AND   c.correlativo             =  reg_folio.folio
   FOREACH  cur_rev_marca         INTO   g_nss,g_marca_cod,g_fecha_ini
            LET      ejecuta          = 
                     "EXECUTE  PROCEDURE  reversa_marca(?,?,?)"
            LET      ejecuta          =  ejecuta  CLIPPED
            PREPARE  rev_marcas           FROM     ejecuta
            EXECUTE  rev_marcas    
                     USING    g_nss, g_marca_cod, reg_folio.folio
   END FOREACH
END FUNCTION

FUNCTION    f_290_reversa_desmarcas()
   DEFINE   l_folio                INTEGER
   DECLARE  cur_rev_desmarca      CURSOR   FOR
    SELECT  b.n_seguro,h.marca_cod,h.fecha_ini
      FROM  taa_cd_det_cedido b,  cta_his_marca  h
     WHERE  b.folio                   =  reg_folio.folio
       AND  b.estado                  =  106
       AND  b.n_seguro                =  h.nss
       AND  b.folio                   =  h.correlativo
       AND  h.estado_marca            =  30
   FOREACH  cur_rev_desmarca       INTO  g_nss,g_marca_cod,g_fecha_ini
            IF       g_marca_cod                 IS  NULL     OR
                     g_marca_cod                  =  0        THEN
                     CONTINUE  FOREACH
            END IF
            LET      ejecuta                 =
                     "EXECUTE  PROCEDURE  reversa_desmarca(?,?,?,?)"
            LET      ejecuta                 =  ejecuta  CLIPPED
            PREPARE  rev_desmarcas        FROM  ejecuta
            EXECUTE  rev_desmarcas       USING  g_nss,
                                                g_marca_cod,
                                                reg_folio.folio,
                                                g_fecha_ini
   END FOREACH
END FUNCTION
