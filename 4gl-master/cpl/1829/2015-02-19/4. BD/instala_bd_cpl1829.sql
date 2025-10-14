
CREATE TABLE taa_cza_irn(
archivo     CHAR(40),
folio       INTEGER ,
fecha_carga DATE    ,
usuario     CHAR(08));

CREATE INDEX ix01_taa_cza_irn ON taa_cza_irn (folio)
;

CREATE TABLE taa_det_irn(
folio             INTEGER     , --
id_porcentaje     SMALLINT    , --ID 01
siefore           SMALLINT    , --ID 03
afore_ced         SMALLINT    , --ID 04
pos_rend          SMALLINT    , --ID 05
signo_rend_bruto  CHAR(01)    , --ID 06
rend_bruto        DECIMAL(9,5), --ID 07
comision          DECIMAL(9,5), --ID 08
signo_rend_neto   CHAR(01)    , --ID 09
rend_neto         DECIMAL(9,5), --ID 10
ind_cambio_anio   SMALLINT    , --ID 11
fecha_inicio      DATE        , --ID 12
motivo_trasp      SMALLINT    , --ID 14
afore_recep       SMALLINT    , --ID 15
leyenda           SMALLINT      --ID 16
)
;

CREATE INDEX ix01_taa_det_irn ON taa_det_irn (folio)
;


