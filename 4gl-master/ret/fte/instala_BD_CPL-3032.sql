INSERT INTO tab_movimiento VALUES(494,"DEVOLUCION AV CONTRACARGOS",1);

INSERT INTO tab_marca VALUES (494,"DEVOLUCION AV CONTRACARGOS",140,1,1,TODAY,USER);

INSERT INTO tab_rch_marca VALUES (494,"DEVOLUCION AV CONTRACARGOS",TODAY,USER)
;

INSERT INTO cta_convivencia
SELECT 494          ,
marca_activa ,
convive_cod  ,
rechazo_cod  ,
TODAY        ,
USER
FROM   cta_convivencia
WHERE  marca_entra  = 490
;
INSERT INTO cta_convivencia
SELECT marca_entra  ,
494          ,
convive_cod  ,
rechazo_cod  ,
TODAY        ,
USER
FROM   cta_convivencia
WHERE  marca_activa = 490
;

UPDATE cta_convivencia
SET    rechazo_cod  = 494
WHERE  marca_activa = 494
AND    rechazo_cod  = 490
;

CREATE TABLE ret_dev_ap_vol
(
folio             INTEGER,
consecutivo       DECIMAL(11,0) ,
nss               CHAR(11)      ,
curp              CHAR(18)      ,
fecha_seleccion   DATE          ,
monto             DECIMAL(13,2) ,
tpo_apor         INTEGER       ,
consec_liquida    DECIMAL(10,0) ,
folio_liquida     INTEGER       ,
usuario_marca     CHAR(10)      ,
usuario_prov      CHAR(10)      ,
usuario_liquida   CHAR(10)      ,
fecha_provision   DATE          ,
fecha_liquida     DATE     
);

insert into men_safre values ('010906050000',4,'Devolucion Aportacion Voluntaria por Contracargo','RETM030','ret');

insert into men_privilegios values(4,'010906050000','RETM030');
insert into men_privilegios values(1,'010906050000','RETM030');
