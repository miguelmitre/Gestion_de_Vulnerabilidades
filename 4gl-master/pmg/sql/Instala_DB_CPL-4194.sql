CREATE TABLE pen_ctr_notifica_det
  (
    nss             CHAR(11) NOT NULL ,
    consecutivo     DECIMAL(11,0) NOT NULL ,
    num_mensualidad SMALLINT NOT NULL ,
    fecha_indicador DATE,
    indicador       SMALLINT,
    PRIMARY KEY (nss,consecutivo,num_mensualidad)
  );

REVOKE ALL ON pen_ctr_notifica_det FROM "public" AS "safre";


CREATE INDEX pen_ctr_notifica_det_01 ON pen_ctr_notifica_det
    (nss,consecutivo) using btree ;
