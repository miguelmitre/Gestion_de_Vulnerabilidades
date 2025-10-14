## DISB001_SM.4gl Programa de barrido para SM    DMR 01/oct/2010
## VERSION 2  Se captura fecha de corte se comvierte a ano mes y
##            se obtienen aportes solo menores a ese periodo.
## MOD  12 Abril 2011

DATABASE safre_af

DEFINE
   pnss           CHAR(11),
   pcurp          CHAR(18),
   pfolio         INTEGER,
   folio_tab      INTEGER,
   pconsec        INTEGER,
   par_per_pago   INTEGER,
   pperiodo_pago  CHAR(6),
   per_en_tab     CHAR(6),
   vperio_pago    CHAR(6),
   psueldo_base   DECIMAL(12,2),
   psueldo_tab    DECIMAL(12,2),
   num_salarios   DECIMAL(12,2),
   sal_min_vig    LIKE tabsalario_minimo2.monto_sm,
   fecha2         DATE,
   fec_act_vig    DATE,
   contador       INTEGER,
   pident_pago    CHAR(16),
   nom_spl        CHAR(100),
   hora_ini       CHAR(8),
   hora_fin       CHAR(8),
   pid_aportante  CHAR(13)


MAIN
   CALL STARTLOG("NUEVO2.log")

   LET par_per_pago = ARG_VAL(1)

   LET hora_ini = TIME
   DISPLAY "INICIA PROCESO ... ", hora_ini

   DATABASE safre_tmp

   LET nom_spl = " EXECUTE PROCEDURE ambiente_salario(?)"
   PREPARE eje_spl1 FROM nom_spl
   EXECUTE eje_spl1 USING par_per_pago

   DATABASE safre_af

   UPDATE STATISTICS FOR TABLE cta_his_sm

   LET hora_ini = TIME
   DISPLAY "AMBIENTE CREADO ... ", hora_ini

   LET contador = 0
   DECLARE cur CURSOR FOR
   SELECT n_seguro, n_unico FROM safre_af:afi_mae_afiliado
   FOREACH cur INTO pnss, pcurp

#### IMSS  ####

      DECLARE cur2 CURSOR FOR
      SELECT folio, periodo_pago, ult_salario_diario, id_aportante
      FROM  safre_tmp:dis_aporte_cta
      WHERE n_seguro = pnss

      FOREACH cur2 INTO pfolio, pperiodo_pago, psueldo_base, pid_aportante
         SELECT MAX(fech_liquidacion)
         INTO  fecha2
         FROM  safre_af:dis_dep_aporte
         WHERE folio = pfolio
         AND   ident_pago[14,15] <> "44"
         AND   importe > 0

         SELECT monto_sm
         INTO  sal_min_vig
         FROM  safre_af:tabsalario_minimo2
         WHERE zona_cod = "A"
         AND   fecha_desde_sm IN (SELECT MAX(fecha_desde_sm)
                                  FROM safre_af:tabsalario_minimo2
                                  WHERE zona_cod = "A")

         LET num_salarios = psueldo_base / sal_min_vig

         SELECT "OK"
         FROM  safre_af:cta_his_sm
         WHERE nss = pnss
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            INSERT INTO safre_af:cta_his_sm
            VALUES(pnss, pfolio, fecha2, pperiodo_pago, psueldo_base,
                   num_salarios, "", TODAY, pid_aportante, user)
         ELSE
            SELECT periodo_pago, fecha_act, folio, salario_dia_int
            INTO   per_en_tab, fec_act_vig, folio_tab, psueldo_tab
            FROM   safre_af:cta_his_sm
            WHERE  nss = pnss

            IF pperiodo_pago > per_en_tab THEN
               UPDATE safre_af:cta_his_sm
               SET folio           = pfolio,
                   fecha_liq       = fecha2,
                   periodo_pago    = pperiodo_pago,
                   salario_dia_int = psueldo_base,
                   num_sal_min     = num_salarios,
                   fecha_act_ant   = fec_act_vig,
                   fecha_act       = today,
                   id_aportante    = pid_aportante,
                   usuario         = user
               WHERE  nss = pnss
            ELSE
               IF pperiodo_pago = per_en_tab AND pfolio > folio_tab THEN
                  UPDATE safre_af:cta_his_sm
                  SET folio           = pfolio,
                      fecha_liq       = fecha2,
                      periodo_pago    = pperiodo_pago,
                      salario_dia_int = psueldo_base,
                      num_sal_min     = num_salarios,
                      fecha_act_ant   = fec_act_vig,
                      fecha_act       = today,
                      id_aportante    = pid_aportante,
                      usuario         = user
                  WHERE  nss = pnss
               ELSE
                  IF pperiodo_pago = per_en_tab AND pfolio = folio_tab AND
                     psueldo_base  > psueldo_tab
                  THEN
                     UPDATE safre_af:cta_his_sm
                     SET folio           = pfolio,
                         fecha_liq       = fecha2,
                         periodo_pago    = pperiodo_pago,
                         salario_dia_int = psueldo_base,
                         num_sal_min     = num_salarios,
                         fecha_act_ant   = fec_act_vig,
                         fecha_act       = today,
                         id_aportante    = pid_aportante,
                         usuario         = user
                      WHERE  nss = pnss
                  END IF
               END IF
            END IF
         END IF

      END FOREACH
      CLOSE cur2

#### ISSSTE ####

      DECLARE cur3 CURSOR FOR
      SELECT folio, periodo_pago, ult_salario_diario, id_aportante
      FROM  safre_tmp:dis_apo_cta_iste
      WHERE n_unico = pcurp

      FOREACH cur3 INTO pfolio, pperiodo_pago, psueldo_base, pid_aportante
         SELECT MAX(fech_liquidacion)
         INTO  fecha2
         FROM  safre_af:dis_dep_issste
         WHERE folio = pfolio
         AND   (ident_pago[14,15] <> "45" AND ident_pago[14,15] <> "46") 
         AND   importe > 0

         SELECT monto_sm
         INTO  sal_min_vig
         FROM  safre_af:tabsalario_minimo2
         WHERE zona_cod = "A"
         AND   fecha_desde_sm IN (SELECT MAX(fecha_desde_sm)
                                  FROM safre_af:tabsalario_minimo2
                                  WHERE zona_cod = "A")

         LET num_salarios = psueldo_base / sal_min_vig

         SELECT "OK"
         FROM  safre_af:cta_his_sm
         WHERE nss = pnss
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            INSERT INTO safre_af:cta_his_sm
            VALUES(pnss, pfolio, fecha2, pperiodo_pago, psueldo_base,
                   num_salarios, "", TODAY, pid_aportante, user)
         ELSE
            SELECT periodo_pago, fecha_act, folio, salario_dia_int
            INTO  per_en_tab, fec_act_vig, folio_tab, psueldo_tab
            FROM  safre_af:cta_his_sm
            WHERE nss = pnss

            IF pperiodo_pago > per_en_tab THEN
               UPDATE safre_af:cta_his_sm
               SET folio           = pfolio,
                   fecha_liq       = fecha2,
                   periodo_pago    = pperiodo_pago,
                   salario_dia_int = psueldo_base,
                   num_sal_min     = num_salarios,
                   fecha_act_ant   = fec_act_vig,
                   fecha_act       = today,
                   id_aportante    = pid_aportante,
                   usuario         = user
               WHERE  nss = pnss
            ELSE 
               IF pperiodo_pago = per_en_tab  AND  pfolio > folio_tab THEN
                  UPDATE safre_af:cta_his_sm
                  SET folio           = pfolio,
                      fecha_liq       = fecha2,
                      periodo_pago    = pperiodo_pago,
                      salario_dia_int = psueldo_base,
                      num_sal_min     = num_salarios,
                      fecha_act_ant   = fec_act_vig,
                      fecha_act       = today,
                      id_aportante    = pid_aportante,
                      usuario         = user
                  WHERE  nss = pnss
               ELSE
                  IF pperiodo_pago = per_en_tab  AND  pfolio = folio_tab AND
                     psueldo_base  > psueldo_tab
                  THEN
                     UPDATE safre_af:cta_his_sm
                     SET folio           = pfolio,
                         fecha_liq       = fecha2,
                         periodo_pago    = pperiodo_pago,
                         salario_dia_int = psueldo_base,
                         num_sal_min     = num_salarios,
                         fecha_act_ant   = fec_act_vig,
                         fecha_act       = today,
                         id_aportante    = pid_aportante,
                         usuario         = user
                     WHERE  nss = pnss
                  END IF
               END IF
            END IF
         END IF

      END FOREACH
      CLOSE cur3
   END FOREACH

   DATABASE safre_af
   UPDATE statistics FOR TABLE cta_his_sm
 
   LET hora_fin = TIME
   DISPLAY "PROCESO TERMINADO ... ",hora_fin
END MAIN

