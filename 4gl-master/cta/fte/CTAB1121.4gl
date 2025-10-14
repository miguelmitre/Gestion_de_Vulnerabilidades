#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa CTAB112  => CONFORMACION DE SALDOS / TRANSFERENCIA DE DECIMOS     #
#Sistema           => CTA                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 10 DE MAYO DE 2005                                    #
#Actualizacion     => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 09 JUNIO 2005 (INCLUYE fn_saldo_dia_sie)              #
#############################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_fecha_corte DATE
   DEFINE HOY           DATE
   DEFINE v_time        DATETIME HOUR TO SECOND
   DEFINE fecha_inicio  DATETIME HOUR TO SECOND
   DEFINE hora_inicio   DATETIME HOUR TO SECOND
   DEFINE fecha_fin     DATETIME HOUR TO SECOND
   DEFINE hora_fin      DATETIME HOUR TO SECOND
   DEFINE fecha_fin2    DATE
   DEFINE v_nss         CHAR(11)

   DEFINE decimo        RECORD
          subcuenta     SMALLINT,
          monto_acc     DECIMAL(19,6),
          monto_pesos   DECIMAL(19,6)
   END RECORD

   DEFINE v_subcuenta   SMALLINT
   DEFINE v_siefore     SMALLINT
   DEFINE fn_saldo      CHAR(50)
   DEFINE tot_acc_sie1  DECIMAL(19,6)
   DEFINE deci_acc      DECIMAL(19,6)
   DEFINE var_usr       CHAR(8)

END GLOBALS

MAIN

   OPTIONS PROMPT LINE LAST -1

   CALL STARTLOG("CTAB112.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY           = TODAY
    LET v_time        = CURRENT HOUR TO SECOND
    LET fecha_inicio  = CURRENT
    LET hora_inicio   = v_time
    LET v_subcuenta   = 0
    LET v_siefore     = 2
    LET fecha_fin     = NULL

    SELECT USER
      INTO var_usr
      FROM systables
     WHERE tabid=1;

    LET fn_saldo = " EXECUTE FUNCTION fn_saldo_dia_sie(?,?,?) "

    PREPARE eje_ind_transf FROM "EXECUTE FUNCTION fn_ind_transferencia(?,?,?)"
    PREPARE sql1 FROM fn_saldo

    LET g_fecha_corte = ARG_VAL(1)

END FUNCTION

FUNCTION proceso_principal()

   DEFINE v_saldo   ,
          v_status  SMALLINT

   DISPLAY "CTAB112   CONFORMACION DE SALDOS, TRANSFERENCIA DE DECIMOS    "

   ---------------------------------------
   -- PROCESAMIENTO PRINCIPAL DE CADA NSS
   ---------------------------------------
   LET v_nss = NULL
   INITIALIZE tot_acc_sie1 TO NULL
   INITIALIZE deci_acc     TO NULL
   INITIALIZE decimo.*     TO NULL

   DECLARE cur_princ CURSOR FOR
   SELECT  a.nss
   FROM    cta_his_nss_decimo a, cta_ctr_cuenta b
   WHERE   a.nss         = b.nss
   AND     a.fecha_corte = g_fecha_corte
   AND     a.proceso_cod = 1
   AND     b.ind_transferencia = 1

   FOREACH cur_princ INTO v_nss
      LET v_saldo = 0

      DECLARE cur_tabla CURSOR FOR sql1

      FOREACH cur_tabla USING v_nss,v_siefore,g_fecha_corte
         INTO decimo.*

         LET v_saldo = 1
         LET deci_acc = decimo.monto_acc / 10

         INSERT INTO cta_saldo_decimo
         VALUES (v_nss,     --nss
                 decimo.subcuenta,  --subcuenta
                 v_siefore,         --numero de la siefore(2)
                 fecha_inicio,      --fecha_congela
                 decimo.monto_acc,  --monto_en_acciones(saldo de la subcte de la SB1)
                 deci_acc,          --monto_decimo (decima parte del saldo total de la subcuenta)
                 var_usr,           --usuario
                 TODAY)             --fecha hoy

         INITIALIZE decimo.* TO NULL
         INITIALIZE deci_acc TO NULL
      END FOREACH


      UPDATE cta_his_nss_decimo
         SET proceso_cod   = 2,
             fecha_proceso = TODAY,
             usuario       = USER
       WHERE nss = v_nss

       IF v_saldo = 0 THEN
          EXECUTE eje_ind_transf INTO  v_status
                                 USING v_nss, "5", HOY
       END IF

      LET v_nss = NULL
   END FOREACH


   UPDATE cta_ctr_decimo
      SET fecha_fin = CURRENT
    WHERE fecha_corte    = g_fecha_corte
      AND proceso_cod    = 2

   LET fecha_fin2    = TODAY
   LET v_time        = CURRENT HOUR TO SECOND
   LET hora_fin      = v_time

--   DISPLAY "INICIO...            "
   DISPLAY "FIN   ... Concluido  ", fecha_fin2 USING "DD-MM-YYYY"
   DISPLAY hora_fin

END FUNCTION

