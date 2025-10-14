##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TCAAC008  => REVERSA PROVISION Y/O LIQUIDACION TRASPASOS CEDIDOS#
#                  => AFORE CEDENTE                                      #
#Autor             => MAURO MUNIZ CABALLERO                              #
#Fecha creacion    => 31 DE ENERO DE 2001                                #
#Sistema           => TRA                                                #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE  s_codigo_afore              LIKE  tab_afore_local.codigo_afore
    DEFINE  reg_taa_cd_ctr_folio   RECORD  LIKE  taa_cd_ctr_folio.* 

    DEFINE
            vfecha                   ,
            g_fecha_ini              ,
            g_hoy                    DATE

    DEFINE
            g_procedente             ,
            g_edo_recibido           ,
            g_provisionada           ,
            g_liquidada              ,
            g_rechazo_saldo          ,
            g_cero                   ,
            g_marca_cod              ,
            f_opc_reverso            ,
            F_tipo_traspaso          SMALLINT
    DEFINE
            g_enter                  CHAR(1),
            g_usuario                CHAR(10),
            g_nss                    CHAR(11),
            ejecuta                  CHAR(100)
            

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
 #  DEFER INTERRUPT

    CALL    inicio()            #i
    CALL    proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET     g_hoy                 =  TODAY
    LET     g_cero                =  0
    LET     reg_taa_cd_ctr_folio.folio           =  0

    SELECT  codigo_afore,user
      INTO  s_codigo_afore,g_usuario
      FROM  tab_afore_local

    SELECT  a.estado
      INTO  g_procedente
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'PROCEDENTE'

    SELECT  a.estado
      INTO  g_rechazo_saldo
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'RECHAZO SALDO'

    SELECT  a.estado
      INTO  g_edo_recibido
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'RECIBIDO'

    SELECT  a.estado
      INTO  g_provisionada
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'PROVISIONADA'
       AND  a.tipo           = 3;

    SELECT  a.estado
      INTO  g_liquidada
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'LIQUIDADA'
       AND  a.tipo           = 3;
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TCAAC0081 AT 4,4 WITH FORM "TCAAC0081" ATTRIBUTE(BORDER)
    DISPLAY "TCAAC008         REVERSO DE OPERACIONES DE TRASPASOS CEDIDOS                    " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < ESC >  Continuar      < CTRL-C >  Salir " AT 1,10
    DISPLAY g_hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
     
    INPUT BY NAME F_tipo_traspaso, f_opc_reverso

        AFTER  FIELD  F_tipo_traspaso
               IF     F_tipo_traspaso        IS  NULL   THEN
                      ERROR  " EL TIPO DE TRASPASO NO PUEDE SER NULO "
                      NEXT FIELD  F_tipo_traspaso
               END IF 
        AFTER  FIELD  f_opc_reverso
               IF     f_opc_reverso          IS  NULL   THEN
                      ERROR  " EL TIPO DE REVERSO NO PUEDE SER NULO "
                      NEXT FIELD  f_opc_reverso
               END IF
        ON KEY ( ESCAPE )
            EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT PROGRAM
    END INPUT
    IF     f_tipo_traspaso         =  1   THEN
           CASE     f_opc_reverso
               WHEN  1
                     CALL   checa_recepcion_arch()
#              WHEN  2
#                    CALL   checa_devoluciones()
               WHEN  3
                     CALL   checa_provision()
               WHEN  4
                     CALL   checa_liquidacion()
               END CASE
    ELSE
           CASE     f_opc_reverso
               WHEN  3
                     CALL   checa_provision()
               WHEN  4
                     CALL   checa_liquidacion()
           END CASE
    END IF

    CALL    display_fechas()

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR g_enter
        IF     g_enter        MATCHES "[sSnN]"     THEN 
               IF     g_enter MATCHES "[sS]"       THEN
                      EXIT WHILE
               ELSE
                      EXIT PROGRAM
               END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    CASE      f_opc_reverso
         WHEN  1
            CALL    reversa_recep_archivo()
         WHEN  2
            CALL    reversa_devoluciones() 
         WHEN  3
            CALL    reversa_provision()
         WHEN  4
            CALL    reversa_liquidacion()
    END CASE

    PROMPT "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE) 
            FOR g_enter

    CLOSE WINDOW TCAAC0081

END FUNCTION
FUNCTION  checa_recepcion_arch()
    SELECT  f.*
      INTO  reg_taa_cd_ctr_folio.*
      FROM  taa_cd_ctr_folio  f
     WHERE  f.fecha_envio_saldos       >=  g_hoy
       AND  f.estado                    =  g_procedente
       AND  f.tipo_traspaso             =  F_tipo_traspaso
    IF      reg_taa_cd_ctr_folio.folio     =  0    THEN
            PROMPT "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR" FOR g_enter
            EXIT PROGRAM
    END IF
    
END FUNCTION
FUNCTION  checa_devoluciones()
    SELECT  f.*
      INTO  reg_taa_cd_ctr_folio.*
      FROM  taa_cd_ctr_folio  f
     WHERE  f.fecha_envio_saldos       >=  g_hoy
       AND  f.estado                    =  g_procedente
       AND  f.tipo_traspaso             =  F_tipo_traspaso
    IF      reg_taa_cd_ctr_folio.folio     =  0    THEN
            PROMPT "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR" FOR g_enter
            EXIT PROGRAM
    END IF
END FUNCTION

FUNCTION  checa_provision()
    SELECT  f.*
      INTO  reg_taa_cd_ctr_folio.*
      FROM  taa_cd_ctr_folio  f
     WHERE  f.fecha_envio_saldos       >=  g_hoy
       AND  f.estado                    =  g_provisionada
       AND  f.tipo_traspaso             =  F_tipo_traspaso
    IF      reg_taa_cd_ctr_folio.folio     =  0    THEN
            PROMPT "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR" FOR g_enter
            EXIT PROGRAM
    END IF
END FUNCTION

FUNCTION checa_liquidacion()

    SELECT  f.*
      INTO  reg_taa_cd_ctr_folio.*
      FROM  taa_cd_ctr_folio  f
     WHERE  f.fecha_liquidacion        =  g_hoy
       AND  f.estado                   =  g_liquidada 
       AND  f.tipo_traspaso             =  F_tipo_traspaso

    IF      reg_taa_cd_ctr_folio.folio           =  0    THEN
            PROMPT "NO HAY FOLIO A REVERSAR <ENTER> PARA SALIR" FOR g_enter
            EXIT PROGRAM
    END IF
END FUNCTION

FUNCTION display_fechas()
    DISPLAY BY  NAME  f_tipo_traspaso
    DISPLAY BY  NAME  f_opc_reverso
    DISPLAY reg_taa_cd_ctr_folio.folio                    TO  F_folio
    DISPLAY reg_taa_cd_ctr_folio.fecha_presentacion       TO
            F_fecha_presentacion
    DISPLAY reg_taa_cd_ctr_folio.fecha_envio_saldos       TO
            F_fecha_envio_saldos
    DISPLAY reg_taa_cd_ctr_folio.fecha_liquidacion        TO
            F_fecha_liquidacion
    ERROR  "   ESTE  ES  EL  FOLIO  POSIBLE  DE  REVERSAR " 
END FUNCTION
FUNCTION reversa_recep_archivo()
#rp-------------------------

   CALL    reversa_marcas()

   DELETE  FROM  taa_cd_rch_convivencia
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
   DELETE  FROM  taa_cd_ced_valida
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
   DELETE  FROM  taa_cd_sum_cedido
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
   DELETE  FROM  taa_cd_det_cedido
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
   DELETE  FROM  taa_cd_cza_cedido
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
   DELETE  FROM  taa_cd_ctr_folio
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
END FUNCTION

FUNCTION reversa_devoluciones()
#rp-------------------------

   UPDATE  taa_cd_det_cedido
      SET  estado      =  g_procedente
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio
      AND  estado      =  g_procedente;

   UPDATE  taa_cd_ctr_folio
      SET  estado      =  g_procedente
    WHERE  folio       =  reg_taa_cd_ctr_folio.folio
      AND  estado      =  g_procedente;

END FUNCTION

FUNCTION reversa_provision()
#rp-------------------------

    DELETE  FROM   dis_provision
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    DELETE  FROM   safre_tmp:taa_cd_saldos_arch
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    DELETE  FROM   safre_tmp:taa_cd_sum_arch
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    DELETE  FROM   safre_af:taa_cd_cza_rch_sal 
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    DELETE  FROM   safre_af:taa_cd_det_02_rch_sal 
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    DELETE  FROM   safre_af:taa_cd_det_05_rch_sal 
     WHERE  folio               =  reg_taa_cd_ctr_folio.folio;
    IF      F_tipo_traspaso     =  1   THEN

            CALL    reversa_desmarcas()

            UPDATE  taa_cd_det_cedido
               SET  estado      =  g_procedente
             WHERE  folio       =  reg_taa_cd_ctr_folio.folio
               AND (estado      =  g_provisionada
                OR  estado      =  g_rechazo_saldo);

            UPDATE  taa_cd_ctr_folio
               SET  estado      =  g_procedente
             WHERE  folio       =  reg_taa_cd_ctr_folio.folio;
    ELSE
            DELETE  FROM    taa_cd_det_comple
             WHERE  folio       =  reg_taa_cd_ctr_folio.folio;

            DELETE  FROM    taa_cd_ctr_folio
             WHERE  folio       =  reg_taa_cd_ctr_folio.folio;

            UPDATE  taa_cd_det_cedido
            SET     estado      =  g_liquidada
            WHERE   estado      =  108;
    END IF

END FUNCTION

FUNCTION reversa_liquidacion()
#rp---------------------------

    DELETE  FROM   dis_cuenta
     WHERE  folio          =  reg_taa_cd_ctr_folio.folio
      AND   subcuenta         BETWEEN  1     AND    14
      AND   tipo_movimiento   BETWEEN  200   AND  299;

    UPDATE  dis_provision
       SET  estado         =  5
     WHERE  folio          =  reg_taa_cd_ctr_folio.folio
      AND   subcuenta         BETWEEN  1     AND  14
      AND   tipo_movimiento   BETWEEN  200   AND  299;

    UPDATE  taa_cd_ctr_folio
       SET  estado         =  g_provisionada
     WHERE  folio          =  reg_taa_cd_ctr_folio.folio;

    IF      F_tipo_traspaso      =  1   THEN

            CALL    reversa_desmarcas()

            UPDATE  cta_ctr_cuenta
            SET     tipo_informe   =  NULL,
                    fecha_informe  =  NULL
            WHERE   nss            IN(SELECT d.n_seguro
                           FROM   safre_af:taa_cd_det_cedido d
                           WHERE  d.folio        =  reg_taa_cd_ctr_folio.folio
                           AND    d.tipo_traspaso   IN('01','12')
                           AND    d.estado       =  g_liquidada)

            UPDATE   taa_cd_det_cedido
               SET   estado      =  g_provisionada
             WHERE   folio       =  reg_taa_cd_ctr_folio.folio
               AND   estado      =  g_liquidada;
    ELSE
            UPDATE   taa_cd_det_cedido
               SET   estado      =  108
             WHERE   estado      =  g_liquidada
               AND   n_seguro   IN(SELECT  n_seguro  FROM  taa_cd_det_comple
                                    WHERE  folio = reg_taa_cd_ctr_folio.folio)
    END IF
END FUNCTION

FUNCTION  reversa_marcas()
   DECLARE q_marcas      CURSOR  FOR
   SELECT  a.n_seguro,b.marca_cod,c.fecha_ini
     FROM  taa_cd_det_cedido a, taa_cd_tipo_traspaso b, cta_act_marca c
    WHERE  a.folio             =  reg_taa_cd_ctr_folio.folio
      AND  a.estado            =  g_procedente
      AND  a.tipo_traspaso     =  b.tipo_traspaso
      AND  a.n_seguro          =  c.nss
      AND  c.correlativo       =  reg_taa_cd_ctr_folio.folio
   FOREACH q_marcas      INTO   g_nss,g_marca_cod,g_fecha_ini
      LET      ejecuta      =  "EXECUTE PROCEDURE reversa_marca(",
                                  "?,?,?)"
      LET      ejecuta           =  ejecuta  CLIPPED

      PREPARE  rev_marcas      FROM   ejecuta
      EXECUTE  rev_marcas      USING 
               g_nss,
               g_marca_cod,
               reg_taa_cd_ctr_folio.folio
      END FOREACH
END FUNCTION

FUNCTION  reversa_desmarcas()

   DECLARE q_desmarcas    CURSOR  FOR

   SELECT  a.nss,c.marca_cod,a.fecha_ini
     FROM  cta_act_marca a, taa_cd_det_cedido b, taa_cd_tipo_traspaso c
    WHERE  b.folio             =  reg_taa_cd_ctr_folio.folio
      AND  b.estado            =  g_liquidada
      AND  a.nss               =  b.n_seguro
      AND  b.tipo_traspaso     =  c.tipo_traspaso
      AND  a.correlativo       =  reg_taa_cd_ctr_folio.folio
      AND  a.marca_cod         =  120 

   FOREACH q_desmarcas    INTO   g_nss,g_marca_cod,g_fecha_ini

      LET    ejecuta           =  "EXECUTE PROCEDURE reversa_desmarca(",
                                  "?,?,?,?)"
      LET    ejecuta           =  ejecuta  CLIPPED
      PREPARE  rev_desmarcas   FROM   ejecuta
      EXECUTE  rev_desmarcas   USING 
               g_nss,
               g_marca_cod,
               reg_taa_cd_ctr_folio.folio,
               g_fecha_ini
   END FOREACH
END FUNCTION

