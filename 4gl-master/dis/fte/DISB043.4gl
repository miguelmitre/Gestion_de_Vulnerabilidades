--------------------------------------------------------------------------------
-- Proyecto           => Sistema de Afore (México).                           --
-- Propietario        => E.F.P.                                               --
-- Programa           => DISB043.                                             --
-- Descripcion        => Actualiza dias cotizados en cta_ctr_cuenta al momento--
--                    => de liquidar aportaciones EST.                        --
-- Modulo             => DIS.                                                 --
-- Fecha Inicio       => 31 julio 2002.                                       --
-- Hecho por          => Gerardo Alfonso Vega Paredes.                        --
-- Fecha ultima modif.=>                                                      --
-- Hecho por          =>                                                      --
--------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   
   DEFINE g_reg RECORD
      nss       CHAR(11),
      dias_cot  INTEGER,
      dias_inca INTEGER,
      dias_ause INTEGER
   END RECORD

   DEFINE vfolio INTEGER

END GLOBALS

MAIN
 
   CALL STARTLOG("DISB043.log")

   LET vfolio = ARG_VAL(1)

   CALL Ejecuta_actualizacion()

END MAIN

FUNCTION Ejecuta_actualizacion()

   DECLARE cur_dias CURSOR FOR
   SELECT n_seguro,
          SUM(dias_cotz_bimestre),
          SUM(dias_incap_bimest),
          SUM(dias_ausent_bimest)
   FROM   dis_det_aporte
   WHERE  folio = vfolio
   GROUP  BY 1
                                                         
   FOREACH cur_dias INTO g_reg.*
      IF g_reg.dias_cot IS NULL THEN
         LET g_reg.dias_cot = 0
      END IF
      IF g_reg.dias_inca IS NULL THEN
         LET g_reg.dias_inca = 0
      END IF
      IF g_reg.dias_ause IS NULL THEN
         LET g_reg.dias_ause = 0
      END IF

      LET g_reg.dias_cot = g_reg.dias_cot - g_reg.dias_inca - g_reg.dias_ause

      UPDATE cta_ctr_cuenta
      SET    dias_cotizados = dias_cotizados + g_reg.dias_cot
      WHERE  nss = g_reg.nss

      LET g_reg.dias_cot = 0
      LET g_reg.dias_inca = 0
      LET g_reg.dias_ause = 0

   END FOREACH

END FUNCTION
