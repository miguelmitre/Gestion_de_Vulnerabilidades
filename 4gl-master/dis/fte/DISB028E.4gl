###############################################################################
#Proyecto          => AFORES                                                  #
#Propietario       => E.F.P                                                   #
#Modulo            => DIS                                                     #
#Programa          => DISB028E                                                #
#Descripcion       => ACTUALIZA CTAS INHABILITADAS A ACTIVAS Dispersion ISSSTE#
#Autor             => DMR                                                     #
#Fecha             => 14 Febrero 2008.                                        #
#Autor cambio      =>                                                         #
#Fecha cambio      =>                                                         #
#Descripcion       => Adecuacion para que utilice nuevo modelo de marcaje     #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vfolio                 INTEGER,
      vregistros             SMALLINT

   DEFINE
      contador               SMALLINT

   DEFINE
      monto_ret_i            DEC(16,6),
      monto_sar_i            DEC(16,6),
      monto_ces_i            DEC(16,6),
      monto_vol              DEC(16,6),
      monto_cs_i             DEC(16,6),
      monto_foviv            DEC(16,6)

   DEFINE
      g_pag1 RECORD
                cont         SMALLINT,
                estado       SMALLINT
             END RECORD

   DEFINE
      g_reg  RECORD
                nss          CHAR(11),
                fecha_reh    DATE,
                fecha_inh    DATE,
                marca_cod    SMALLINT,
                correlativo  INTEGER,
                estado       SMALLINT,
                marca_causa  SMALLINT,
                usuario      CHAR(08)
             END RECORD
END GLOBALS


MAIN
   CALL STARTLOG("DISB028E.log")

   LET vfolio = ARG_VAL(1)

   CALL Actualiza_activos()
END MAIN


FUNCTION Actualiza_activos()
   DEFINE
      vfecha_ini   DATE,
      vhora_ini    DATETIME HOUR TO SECOND

   DECLARE cur_nss CURSOR FOR
   SELECT unique cta.nss,
          dep.fech_liquidacion,
          act.fecha_ini,
          act.marca_cod,
          act.correlativo,
          0,
          101,
          user
   FROM   dis_cuenta       cta,
          dis_dep_issste   dep,
          cta_act_marca    act,
          tab_marca        tab
   WHERE  cta.folio             = vfolio
   AND    cta.folio             = dep.folio
   AND    dep.ident_pago[14,15]IN ("41","42","43","44","47","48","81","82","83")
   AND    dep.estado            = 3
   AND    cta.nss               = act.nss
   AND    act.marca_cod         = tab.marca_cod
   AND    tab.ind_habilita      = 2
   AND    dep.fech_liquidacion  > act.fecha_ini
   --OUP  BY 1,7,8,9,10,11,12,13

   FOREACH cur_nss INTO g_reg.*

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_ret_i
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta = 30

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_sar_i
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta = 13

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_ces_i
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta = 31

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_vol
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta = 3

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_foviv
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta IN (14,35)

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  monto_cs_i
      FROM  dis_cuenta
      WHERE folio  = vfolio
      AND   nss    = g_reg.nss
      AND   subcuenta = 32

      INSERT INTO cta_rehabilitada VALUES
         (
          vfolio,           -- folio
          g_reg.nss,        -- nss
          monto_ret_i,      -- monto_retiro issste
          monto_ces_i,      -- monto_cesantia issste
          monto_vol,        -- monto_voluntaria
          monto_foviv,      -- monto_vivienda97
          monto_cs_i,       -- monto_couta_soc issste
          monto_sar_i,      -- monto_sar
          0,                -- monto_vivienda92
          g_reg.fecha_reh,  -- fecha_rehabilita
          g_reg.marca_cod,  -- marca_cod
          TODAY,            -- fecha_actualiza
          0,                -- estado
          g_reg.usuario     -- usuario
         )

      PREPARE spl_exe FROM
      "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) "
      EXECUTE spl_exe USING g_reg.nss,
                            g_reg.marca_cod,
                            g_reg.correlativo,
                            g_reg.estado,
                            g_reg.marca_causa,
                            g_reg.usuario

   END FOREACH
END FUNCTION

