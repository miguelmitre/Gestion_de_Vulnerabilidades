################################################################################
##   PROGRAMA DISB01CV.4gl   
##
##   Programa de barrido de aportaciones de CV ISSSTE para colocar
##   id aportantante APOR-TRAB en aportaciones trabajador e INT.EXT-TRA
##   para los intereses extemporaneos del Trabajador.
##
##   DMR  12 Mayo 2011 
################################################################################
DATABASE safre_af

DEFINE
   anno      SMALLINT,
   cve_afore SMALLINT,
   conta     INTEGER,
   cont_ap   INTEGER,
   cont_ap2  INTEGER,
   folioa    INTEGER,
   cad_1     CHAR(50),
   cadena    CHAR(500),
   fecha_liq DATE,
   reg_det   RECORD 
                folio           INTEGER,
                nss             CHAR(11),
                consecutivo     INTEGER,
                tipo_movimiento SMALLINT,
                monto_pesos     DEC(16,6)
             END RECORD

MAIN
   LET conta = 0
   DISPLAY " INICIA BARRIDO "

   SELECT codigo_afore                   --v5
   INTO   cve_afore                      --v5
   FROM   tab_afore_local                --v5

   DECLARE cur CURSOR FOR
   SELECT folio FROM dis_cza_issste
   ORDER BY 1

   FOREACH cur INTO folioa
      DISPLAY "FOLIO BARRIDO ",folioa

      SELECT count(*)
      INTO cont_ap
      FROM dis_det_issste
      WHERE folio = folioa
      AND impt_cv_isss_tra > 0

      SELECT count(*)
      INTO cont_ap2
      FROM dis_det_issste
      WHERE folio = folioa
      AND inte_ext_cv_isss_tra > 0

      IF cont_ap > 0  OR cont_ap2 > 0  THEN
 
         DECLARE cur1 CURSOR FOR
         SELECT a.folio, b.n_seguro, a.consec_reg_lote, 1,
                a.impt_cv_isss_tra/100 
         FROM  dis_det_issste a, afi_mae_afiliado b
         WHERE a.folio     = folioa
         AND   a.impt_cv_isss_tra > 0
         AND   a.result_operacion <> "02"
         AND   a.n_unico = b.n_unico
         UNION 
         SELECT a.folio, b.n_seguro, a.consec_reg_lote, 3,
                a.inte_ext_cv_isss_tra/100
         FROM  dis_det_issste a, afi_mae_afiliado b
         WHERE a.folio     = folioa
         AND   a.inte_ext_cv_isss_tra > 0 
         AND   a.result_operacion <> "02"
         AND   a.n_unico = b.n_unico

         FOREACH cur1 INTO reg_det.*
            LET conta = conta + 1
--          DISPLAY conta AT 10,10

            SELECT max(fech_liquidacion)
            INTO fecha_liq
            FROM dis_dep_issste
            WHERE folio = reg_det.folio
            AND ident_pago[14,15] = 41

            IF cve_afore = 568  THEN              ### 568 COPPEL solo dis_cuenta
               LET cad_1 = "UPDATE dis_cuenta"
            ELSE
               LET anno = YEAR(fecha_liq)
     
               CASE anno
                  WHEN 2008
                     LET cad_1 = "UPDATE dis_cuenta08 "
                  WHEN 2009
                     LET cad_1 = "UPDATE dis_cuenta09 "
                  WHEN 2010
                     LET cad_1 = "UPDATE dis_cuenta10 "
                  WHEN 2011
                     LET cad_1 = "UPDATE dis_cuenta "
               END CASE
            END IF

            IF reg_det.tipo_movimiento = 1 THEN
               LET cadena= cad_1," SET id_aportante = 'APOR-TRAB' ",
                                 " WHERE nss      = '",reg_det.nss,"'",
                                 " AND subcuenta  = 31 ",
                              " AND tipo_movimiento = ",reg_det.tipo_movimiento,
                                 " AND folio           = ",reg_det.folio,
                                 " AND consecutivo_lote= ",reg_det.consecutivo,
                                 " AND monto_en_pesos  = ",reg_det.monto_pesos
            ELSE
               LET cadena= cad_1," SET id_aportante = 'INT.EXT-TRA' ",
                                 " WHERE nss      = '",reg_det.nss,"'",
                                 " AND subcuenta  = 31 ",
                              " AND tipo_movimiento = ",reg_det.tipo_movimiento,
                                 " AND folio           = ",reg_det.folio,
                                 " AND consecutivo_lote= ",reg_det.consecutivo,
                                 " AND monto_en_pesos  = ",reg_det.monto_pesos

            END IF

            PREPARE cad FROM cadena
            EXECUTE cad
         END FOREACH
      END IF
   END FOREACH
   DISPLAY " TERMINA BARRIDO "
END MAIN

