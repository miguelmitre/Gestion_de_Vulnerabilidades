CREATE TABLE taa_cd_acr_det (
folio                 DECIMAL(10,0),
tipo_trabajador       SMALLINT,
nss                   CHAR(11),
curp                  CHAR(18), 
ind_rcv               SMALLINT,
tot_rcv               DECIMAL(22,6),
ind_acr               SMALLINT,
tot_acr               DECIMAL(22,6),
estado                SMALLINT,
result_operacion      CHAR(2),
motivo_rechazo        CHAR(3),
 PRIMARY KEY (folio,nss)
) IN taa_dbs1
;

CREATE TABLE taa_cd_acr_folio (
folio              DECIMAL(10,0) PRIMARY KEY,
fecha_identifica   DATE,
fecha_generacion   DATE,
fecha_recepcion    DATE,
estado             SMALLINT,
result_operacion   CHAR(2),
motivo_rechazo     CHAR(9)
) IN taa_dbs1 LOCK MODE ROW;
