--TABLA DE CONTROL DE CONTRATOS LIQUIDADOS RENOVADOS
CREATE TABLE pen_solicitud_ren
(                             
    nss                  CHAR(11)    NOT NULL,          
    consecutivo          DECIMAL(11) NOT NULL,
    sec_contrato         SMALLINT    NOT NULL,
    consecutivo_ren      DECIMAL(11) NOT NULL,
    sec_contrato_ren     SMALLINT    NOT NULL,
    fecha_ren            DATE        NOT NULL,
    usuario_ren          CHAR(15)    NOT NULL,
    PRIMARY KEY (nss, consecutivo, sec_contrato) CONSTRAINT pk_pensolren
);
