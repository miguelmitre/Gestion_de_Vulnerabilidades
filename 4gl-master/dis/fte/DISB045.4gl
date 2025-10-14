--------------------------------------------------------------------------------
-- Proyecto           => Sistema de Afore (México).                           --
-- Propietario        => E.F.P.                                               --
-- Programa           => DISB045.                                             --
-- Descripcion        => Actualiza fecha_pri_general y fecha_ult_general      --
--                       cta_ctr_cuenta                                       --
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
 
   CALL STARTLOG("DISB045.log")

   LET vfolio = ARG_VAL(1)
   LET vfecha = ARG_VAL(2)

   CALL Actualiza_pri_general()

   CALL Actualiza_ult_general()

END MAIN

FUNCTION Actualiza_pri_general()

   DECLARE cur_pri_general CURSOR FOR
   SELECT unique a.nss
   FROM   dis_cuenta a,
          cta_ctr_cuenta b
   WHERE  folio           = vfolio
   AND    subcuenta       in (1,2,3,4,5,6,9)
   AND    tipo_movimiento = 1
   AND    a.nss           = b.nss
   AND    (fecha_pri_general IS NULL OR fecha_pri_general = "01/01/0001")

   FOREACH cur_pri_general INTO g_reg.*

      UPDATE cta_ctr_cuenta
      SET    fecha_pri_general = vfecha
      WHERE  nss               = g_reg.nss

   END FOREACH

END FUNCTION


FUNCTION Actualiza_ult_general()

   DECLARE cur_ult_general CURSOR FOR
   SELECT unique nss
   FROM   dis_cuenta
   WHERE  folio           = vfolio
   AND    subcuenta       in (1,2,3,4,5,6,9)
   AND    tipo_movimiento = 1

   FOREACH cur_ult_general INTO g_reg.*

      UPDATE cta_ctr_cuenta
      SET    fecha_ult_general = vfecha
      WHERE  nss = g_reg.nss

   END FOREACH

END FUNCTION
