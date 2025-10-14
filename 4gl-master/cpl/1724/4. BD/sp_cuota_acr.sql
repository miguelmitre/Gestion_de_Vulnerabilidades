DROP PROCEDURE sp_cuota_acr;
CREATE PROCEDURE sp_cuota_acr(pd_fecha_fin DATE)
DEFINE lc_nss CHAR(11);

DROP TABLE cuota_acr;
CREATE TABLE cuota_acr
  ( nss                CHAR(11),
    curp               CHAR(18),
    tipo_solicitud     SMALLINT,
    finicta            DATE
    );

INSERT INTO cuota_acr
SELECT a.n_seguro,
       a.n_unico,
       a.tipo_solicitud,
       a.finicta 
FROM   safre_af:afi_mae_afiliado a
WHERE  a.finicta <= pd_fecha_fin
AND a.tipo_solicitud != 5;

DELETE FROM safre_tmp:cuota_acr
WHERE finicta > pd_fecha_fin;

DELETE FROM cuota_acr
WHERE nss IN
(SELECT b.n_seguro
 FROM safre_af:taa_cd_det_cedido b
 WHERE b.fecha_trasp <= pd_fecha_fin
 AND   b.estado IN (103,12));

DELETE FROM cuota_acr
WHERE nss IN
(SELECT b.nss_cta1
 FROM safre_af:uni_unificado b
 WHERE b.fnotifica <= pd_fecha_fin
 AND   b.estado = 100); 

DELETE FROM cuota_acr
WHERE nss IN
(SELECT b.nss
 FROM safre_af:cta_act_marca b
 WHERE b.marca_cod IN(120,130,150) 
 AND   b.fecha_ini <= pd_fecha_fin);


 
CREATE INDEX cuota_acr1 ON cuota_acr(nss);

UPDATE STATISTICS FOR TABLE cuota_acr;

  DROP TABLE tmp_dis_cuenta_acr;

   CREATE RAW TABLE tmp_dis_cuenta_acr
    ( 
     tipo_movimiento      SMALLINT,
     subcuenta            SMALLINT,
     siefore              SMALLINT,
     folio                INTEGER,
     consecutivo_lote     INTEGER,
     nss                  CHAR(11),
     curp                 CHAR(18),
     folio_sua            CHAR(6),
     fecha_pago           DATE,
     fecha_valor          DATE,
     fecha_conversion     DATE,
     monto_en_pesos       DECIMAL(22,6),
     monto_en_acciones    DECIMAL(22,6),
     precio_accion        DECIMAL(22,6),
     dias_cotizados       SMALLINT,
     sucursal             CHAR(10),
     id_aportante         CHAR(11),
     estado               SMALLINT,
     fecha_proceso        DATE,
     usuario              CHAR(8),
     fecha_archivo        DATE,
     etiqueta             SMALLINT
    );

  FOREACH
    SELECT nss 
    INTO lc_nss
    FROM safre_tmp:cuota_acr

   INSERT INTO safre_tmp:tmp_dis_cuenta_acr
    SELECT * FROM safre_af:dis_cuenta
    WHERE nss = lc_nss
    AND tipo_movimiento = 1;

  END FOREACH;

END PROCEDURE;
