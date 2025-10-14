###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS.                                                    #
#Programa          => DISB078B                                                #
#Versión           => TODAS LAS AFORES                                        #
#Descripcion       => Identifica Transferencias indebidas al momento de       #
#                  => liquidar lo provisionado                                #
#Fecha Inicio      => 13 enero 2005.                                          #
#Fecha Termino     => 13 enero 2005.                                          #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Modificacion      => DMR 22/01/2013  Se agrego funcion fn_sol_transf_sie para#
#                     trasferencia entre siefores                             #
#Modi.             => DMR 15 Febrero 2013                                     #
#Descr.            => Estandarizacion de version afores, SQLCA.SQLCODE y Liq  #
#                  => sin prioridad en prog liq, se aplica en las rutinas de  #
#                  => liq. liq_apor_est_esp y liq_apor_rcv_esp                #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vfolio       INTEGER

   DEFINE 
      rsie         SMALLINT

   DEFINE
      rapido       CHAR(30),
      lento        CHAR(30)

   DEFINE
      hora_inicial CHAR(08),
      hora_final   CHAR(08)

   DEFINE
      cla_sel      CHAR(450),
      cla_upd      CHAR(450),
      vrow         INTEGER,
      usuario      CHAR(12)

   DEFINE
      r_liq        RECORD
                      lnss  CHAR(11),
                      lsub  SMALLINT,
                      lsie  SMALLINT
                   END RECORD
END GLOBALS


MAIN
   CALL STARTLOG("DISB078B.log")

   LET vfolio = ARG_VAL(1)

   DISPLAY "INICIO IDENTIFICACION NSS CAMBIO SIEFORE"

   CALL Inserta_inicio_proceso()
   CALL Inicializa()
   CALL Llena_tablas()
   CALL Identifica()
   CALL Inserta_fin_proceso()

   DISPLAY "FIN IDENTIFICACION NSS CAMBIO SIEFORE"
END MAIN


FUNCTION Inserta_inicio_proceso()
   SELECT user
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB078B",              -- proceso_cod
       1,                       -- etapa_cod  
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       vfolio,                  -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       vfolio,                  -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Identificacion ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Inicializa()
   LET rapido = "SET PDQPRIORITY HIGH"
   LET rapido = rapido CLIPPED
   LET lento  = "SET PDQPRIORITY OFF"
   LET lento  = lento CLIPPED
END FUNCTION


FUNCTION Llena_tablas()
   PREPARE rapido_e FROM rapido
   EXECUTE rapido_e
   
   CREATE TEMP TABLE tmp_nss_liq
      (nss CHAR(11),
       sub SMALLINT,
       sie SMALLINT
      )

   INSERT INTO tmp_nss_liq
   SELECT UNIQUE nss,
          subcuenta,
          siefore
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta not in (3,4,14,35)

   CREATE INDEX tmp_nss_liq_1 ON tmp_nss_liq(nss)

   PREPARE lento_e FROM lento
   EXECUTE lento_e

   UPDATE STATISTICS FOR TABLE tmp_nss_liq
END FUNCTION


FUNCTION Identifica()
   DEFINE sol_tran_sie   CHAR(200)
   DEFINE si_existe      SMALLINT
   DEFINE rechaz         SMALLINT
   DEFINE fol_sol        INTEGER
   DEFINE val3, val10    SMALLINT

   LET val3  = 3
   LET val10 = 10

   PREPARE rapido_e2 FROM rapido
   EXECUTE rapido_e2

   DECLARE cur_liq CURSOR FOR
   SELECT liq.nss,
          liq.sub,
          liq.sie
   FROM   tmp_nss_liq liq

   FOREACH cur_liq INTO r_liq.*

      SELECT codigo_siefore
      INTO rsie 
      FROM cta_regimen
      WHERE nss       = r_liq.lnss      
      AND   subcuenta = r_liq.lsub

      IF r_liq.lsie <> rsie THEN

         LET sol_tran_sie = "EXECUTE FUNCTION fn_sol_transf_sie (?,?,?,?,?,?,?)"
         PREPARE eje_sol_sie FROM sol_tran_sie

         DECLARE cur_sol_tran CURSOR FOR eje_sol_sie
         OPEN cur_sol_tran USING r_liq.lnss,
                                 r_liq.lsub,
                                 r_liq.lsie,
                                 rsie,
                                 val3,         ## LIQ. INDEB. DISPER
                                 vfolio,
                                 val10         ## SIEMPRE 10 
             FETCH cur_sol_tran INTO si_existe,
                                     rechaz,
                                     fol_sol                         
         CLOSE cur_sol_tran

      END IF 
   END FOREACH
END FUNCTION


FUNCTION Inserta_fin_proceso()
   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM   dis_ctrl_proceso ",
                 "WHERE  proceso_cod = 'DISB078B' "," ",
                 "AND    etapa_cod = 1" CLIPPED

   PREPARE claexe9 FROM cla_sel
   DECLARE cur_proceso9 CURSOR FOR claexe9
   OPEN cur_proceso9
      FETCH cur_proceso9 INTO vrow
   CLOSE cur_proceso9

   LET hora_final = TIME

   LET cla_upd = "UPDATE dis_ctrl_proceso ",
                 "SET    hora_final  = ","'",hora_final,"'",",",
                 "       resultado   = 'NSS IDENTIFICADOS' ",
                " WHERE  proceso_cod = 'DISB078B' ",
                " AND    etapa_cod   = 1 ",
                " AND    consecutivo = ",vrow CLIPPED

   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe
END FUNCTION

