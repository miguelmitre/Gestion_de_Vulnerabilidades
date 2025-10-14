-------------------------------------------------------------------------------
--Proyecto          => Safre                                                  -
--Propietario       => E.F.P.                                                 -
--Modulo            => DIS.                                                   -
--Programa          => DISB090                                                -
--Descripcion       => Sustitución de participaciones de vivienda con el      - 
--                  => archivo de dispersion F050404.050.DISPAF en dis_cuenta -
--                  => y dis_det_aporte.
--Fecha Inicio      => 02-mayo-2005.                                          -
--Fecha Termino     =>                                                        -
--Autor             => GERARDO ALFONSO VEGA PAREDES                           -
-------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      nss               CHAR(11),
      folio_sua         CHAR(06),
      reg_patronal_imss CHAR(11),
      part_viv          DECIMAL(18,6)
   END RECORD

   DEFINE vconsecutivo INTEGER

   DEFINE vfolio INTEGER,
          vfolio2 INTEGER

END GLOBALS

MAIN

   CALL Principal()

END MAIN

FUNCTION Principal()

   PROMPT 'Folio actual: ' for vfolio
   PROMPT 'Folio a ser actualizado: ' for vfolio2

   DECLARE cur1 CURSOR FOR
   SELECT n_seguro,
          folio_pago_sua,
          reg_patronal_imss,
          part_viv
   FROM   dis_det_aporte
   WHERE  folio = vfolio

   FOREACH cur1 INTO g_reg.*

      DECLARE cur_det CURSOR FOR
      SELECT consec_reg_lote
      FROM   dis_det_aporte
      WHERE  folio             = vfolio2
      AND    n_seguro          = g_reg.nss
      AND    reg_patronal_imss = g_reg.reg_patronal_imss
      AND    folio_pago_sua    = g_reg.folio_sua
      FOR UPDATE

      FOREACH cur_det INTO vconsecutivo
         UPDATE dis_det_aporte
         SET    part_viv = g_reg.part_viv
         WHERE  CURRENT OF cur_det

         UPDATE dis_cuenta
         SET    monto_en_acciones = g_reg.part_viv / 1000000,
                monto_en_pesos    = (g_reg.part_viv / 1000000)*1.02312709400771
         WHERE  folio            = vfolio2
         AND    subcuenta        = 4
         AND    tipo_movimiento  = 1
         AND    nss              = g_reg.nss
         AND    consecutivo_lote = vconsecutivo
         AND    folio_sua        = g_reg.folio_sua
      END FOREACH
       
   END FOREACH

END FUNCTION
