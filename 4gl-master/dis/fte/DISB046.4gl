--------------------------------------------------------------------------------
-- Proyecto           => Sistema de Afore (México).                           --
-- Propietario        => E.F.P.                                               --
-- Programa           => DISB046.                                             --
-- Descripcion        => Actualiza fecha vol pat en cta_ctr_cuenta            --
-- Modulo             => DIS.                                                 --
-- Fecha Inicio       => 31 julio 2002.                                       --
-- Hecho por          => Gerardo Alfonso Vega Paredes.                        --
-- Fecha ultima modif.=>                                                      --
-- Hecho por          =>                                                      --
--------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   
   DEFINE g_reg RECORD
      nss   CHAR(11)
   END RECORD

   DEFINE vfolio INTEGER,
          vfecha DATE

END GLOBALS

MAIN
 
   CALL STARTLOG("DISB046.log")

   LET vfolio = ARG_VAL(1)
   LET vfecha = ARG_VAL(2)

   CALL Ejecuta_actualizacion()

END MAIN

FUNCTION Ejecuta_actualizacion()

   DECLARE cur_fecha CURSOR FOR
   SELECT unique a.nss
   FROM   dis_cuenta a,
          cta_ctr_cuenta b
   WHERE  folio           = vfolio
   AND    subcuenta       = 3
   AND    tipo_movimiento = 1
   AND    a.nss           = b.nss
   AND    fecha_vol_pat IS NULL
                                                         
   FOREACH cur_fecha INTO g_reg.*

      UPDATE cta_ctr_cuenta
      SET    fecha_vol_pat = vfecha
      WHERE  nss = g_reg.nss

   END FOREACH

END FUNCTION
