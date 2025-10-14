###############################################################################
#Proyecto          => AFORES                                                  #
#Propietario       => E.F.P                                                   #
#Modulo            => DIS                                                     #
#Programa          => DISB028                                                 #
#Descripcion       => ACTUALIZA CTAS INHABILITADAS A ACTIVAS.                 #
#Autor             => GERARDO ALFONSO VEGA PAREDES.                           #
#Fecha             => 17 Noviembre 2002.                                      #
#Autor cambio      => GERARDO ALFONSO VEGA PAREDES.                           #
#Fecha cambio      => 13 Noviembre 2003.                                      #
#Autor cambio      => DMR                                                     #
#Fecha cambio      => 28 Noviembre 2008.                                      #
#Descripcion       => Adecuacion para que utilice nuevo modelo de marcaje     #
#Autor cambio      => DMR                                                     #
#Fecha cambio      => 07 Octubre 2011.                                        #
#Descripcion       => Estandarizacion Versiones de las afores                 #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      vfolio                 INTEGER,
      vregistros             SMALLINT

   DEFINE
      contador               SMALLINT

   DEFINE
      g_pag1 RECORD
                cont         SMALLINT,
                estado       SMALLINT
             END RECORD

   DEFINE
      g_reg  RECORD
                nss          CHAR(11),
                ret          DECIMAL(12,2),
                ces          DECIMAL(12,2),
                vol          DECIMAL(12,2),
                viv          DECIMAL(12,2),
                soc          DECIMAL(12,2),
                fecha_liq    DATE,
                marca_cod    SMALLINT,
                correlativo  INTEGER,
                estado       SMALLINT,
                marca_causa  SMALLINT,
                usuario      CHAR(08)
             END RECORD
END GLOBALS


MAIN
   CALL STARTLOG("DISB028.log")

   LET vfolio = ARG_VAL(1)

   CALL Actualiza_activos()
END MAIN


FUNCTION Actualiza_activos()
   DEFINE
      vfecha_ini   DATE,
      vhora_ini    DATETIME HOUR TO SECOND

   DECLARE cur_nss CURSOR FOR
   SELECT det.n_seguro,
          sum(det.impt_ret)       / 100,
          sum(det.impt_ces_vej)   / 100,
          sum(det.impt_aport_vol) / 100,
          sum(det.impt_aport_pat + det.impt_rem_viv + det.inte_ext_viv) / 100,
          sum(det.impt_cuota_soc) / 100,
          dep.fech_liquidacion,
          act.marca_cod,
          act.correlativo,
          0,
          101,
          user
   FROM   dis_det_aporte det, 
          dis_dep_aporte dep, 
          cta_act_marca  act,
          tab_marca      tab  
   WHERE  det.folio             = vfolio
   AND    det.folio             = dep.folio
   AND    dep.ident_pago[14,15] IN ("41","42","43","45","46","47","48","49")
-- AND    dep.estado            = 3
   AND    det.n_seguro          = act.nss
   AND    act.marca_cod         = tab.marca_cod
   AND    tab.ind_habilita      = 2
   AND    dep.fech_liquidacion  >= act.fecha_ini
   GROUP  BY 1,7,8,9,10,11,12

   FOREACH cur_nss INTO g_reg.*
      INSERT INTO cta_rehabilitada VALUES
         (
          vfolio,           -- folio
          g_reg.nss,        -- nss
          g_reg.ret,        -- monto_retiro
          g_reg.ces,        -- monto_cesantia
          g_reg.vol,        -- monto_voluntaria
          g_reg.viv,        -- monto_vivienda97
          g_reg.soc,        -- monto_couta_soc
          0,                -- monto_sar
          0,                -- monto_vivienda92
          g_reg.fecha_liq,  -- fecha_rehabilita
          g_reg.marca_cod,  -- marca_cod
          TODAY,            -- fecha_actualiza
          g_reg.estado,     -- estado
          g_reg.usuario     -- usuario
         )

      INSERT INTO cta_his_inhabilitada
      SELECT act.*
      FROM   cta_act_marca act,
             tab_marca     tab
      WHERE  act.nss          = g_reg.nss
      AND    act.marca_cod    = tab.marca_cod
      AND    act.correlativo  = g_reg.correlativo
      AND    tab.ind_habilita = 2

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

