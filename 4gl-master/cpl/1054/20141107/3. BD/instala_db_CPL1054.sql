CREATE TABLE ret_recepcion
  (
    folio               DECIMAL(11,0)           ,
    tipo_operacion      SMALLINT                ,
    fecha_recepcion     DATE                    ,
    hora_recepcion      DATETIME HOUR TO SECOND ,
    nom_archivo         CHAR(20)                ,
    tot_registros       INTEGER                 ,
    usuario             CHAR(15)                ,
    estado              SMALLINT                ,
    mot_rechazo_01      CHAR(3)                 ,
    mot_rechazo_02      CHAR(3)                 ,
    mot_rechazo_03      CHAR(3)                 ,
    PRIMARY KEY (folio, tipo_operacion)
  ) 
IN ret_dbs1  
extent size 64
next size 32
lock mode PAGE
;
CREATE INDEX idx_ret_recepcion_01 
ON ret_recepcion(folio)
USING btree 
IN ret_dbs1
;
UPDATE STATISTICS FOR TABLE ret_recepcion
;
-- -----------------------------------------------------------------------------
CREATE TABLE ret_notifica_vivienda(
    folio_carga         DECIMAL(11,0)           ,
    nss                 CHAR(11)                ,
    consecutivo         DECIMAL(11,0)           ,
    folio_notifica      CHAR(29)                ,
    folio_operacion     CHAR(10)                ,
    clabe               CHAR(18)                ,
    grupo_trabajador    CHAR(04)                ,
    sec_pension         CHAR(02)                ,
    regimen             CHAR(02)                ,
    tipo_retiro         CHAR(01)                ,
    tipo_seguro         CHAR(02)                ,
    tipo_pension        CHAR(02)                ,
    tipo_prestacion     CHAR(02)                ,
    semanas_cotizadas   SMALLINT                ,
    nombre              CHAR(40)                ,
    paterno             CHAR(40)                ,
    materno             CHAR(40)                ,
    rfc                 CHAR(13)                ,
    curp                CHAR(18)                ,
    entidad             SMALLINT                ,
    id_benef            SMALLINT                ,
    nombre_benef        CHAR(40)                ,
    paterno_benef       CHAR(40)                ,
    materno_benef       CHAR(40)                ,
    rfc_benef           CHAR(13)                ,
    curp_benef          CHAR(18)                ,
    comentarios         CHAR(127)               ,
    referencia_pago     CHAR(20)                ,
    observaciones       CHAR(127)               ,
    fecha_captura       DATE                    ,
    indicador           CHAR(01)                ,
    diag_infonavit      CHAR(03)                ,
    usuario_carga       CHAR(15)                ,
    fecha_carga         DATE                    ,
    hora_carga          DATETIME HOUR TO SECOND ,
    estado              SMALLINT                ,
    PRIMARY KEY(folio_carga, nss, consecutivo)
  ) 
IN ret_dbs1
extent size 10000 
NEXT size 5000 
LOCK MODE ROW
;
CREATE INDEX idx_ret_notifica_vivienda_01 
ON ret_notifica_vivienda(folio_carga)
USING btree 
IN ret_dbs1
;
UPDATE STATISTICS FOR TABLE ret_notifica_vivienda
; 
-- -----------------------------------------------------------------------------
CREATE TABLE ret_folio_infonavit
  (
    nss                 CHAR(11) NOT NULL       ,   -- NSS Capturado
    consecutivo         DECIMAL(11,0) NOT NULL  ,   -- Consecutivo de la captura
    folio_infonavit     CHAR(14)                ,   -- Folio INFONAVIT capturado
    folio_solicitud     INTEGER                 ,   -- Folio de la solicitud de Retiro
    tipo_retiro         CHAR(1) NOT NULL        ,   -- Mismos campos que los capturados
    fecha_captura       DATE NOT NULL           ,
    usuario_captura     CHAR(12) NOT NULL       ,
    PRIMARY KEY(nss, consecutivo)
  ) 
IN ret_dbs1
extent size 540 
next size 270 
LOCK MODE ROW
;
UPDATE STATISTICS FOR TABLE ret_folio_infonavit
;
-- -----------------------------------------------------------------------------
ALTER TABLE ret_ajuste_vivienda_nss
ADD otros_importes      DECIMAL(16,6)
BEFORE neto_depositado
;
UPDATE STATISTICS FOR TABLE ret_ajuste_vivienda_nss
;
-- -----------------------------------------------------------------------------
ALTER TABLE ret_beneficiario
ADD nss_benef char(11)
BEFORE rfc_benef
;
ALTER TABLE ret_beneficiario
ADD curp_benef CHAR(18)
BEFORE rfc_benef
;
ALTER TABLE ret_beneficiario
ADD correo_elect CHAR(100)
BEFORE porcentaje
;
ALTER TABLE ret_beneficiario
ADD tipo_cuenta CHAR(1)
BEFORE num_cheque
;
ALTER TABLE ret_beneficiario
ADD clabe CHAR(18)
BEFORE num_cheque
;
ALTER TABLE ret_beneficiario
ADD num_ord_pago CHAR(20)
BEFORE fecha_captura
;
UPDATE STATISTICS FOR TABLE ret_beneficiario
;
-- -----------------------------------------------------------------------------
INSERT INTO tab_marca
VALUES (819,"PROCESO DE DEVOLUCION DE VIVIENDA",0,0,0,TODAY,USER)
;
INSERT INTO tab_rch_marca
VALUES (819,"PROCESO DE DEVOLUCION DE VIVIENDA",TODAY,USER)
;
INSERT INTO cta_convivencia
SELECT 819          ,
       marca_activa ,
       convive_cod  ,
       rechazo_cod  ,
       TODAY        ,
       USER
FROM   cta_convivencia
WHERE  marca_entra  = 830
;
INSERT INTO cta_convivencia
SELECT marca_entra  ,
       819          ,
       convive_cod  ,
       rechazo_cod  ,
       TODAY        ,
       USER
FROM   cta_convivencia
WHERE  marca_activa = 830
;
UPDATE cta_convivencia
SET    rechazo_cod  = 819
WHERE  marca_activa = 819
AND    rechazo_cod  = 830
