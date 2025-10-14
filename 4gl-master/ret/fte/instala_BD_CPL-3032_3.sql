
unload to tab_movimiento26062020.unl
select * from tab_movimiento
WHERE   codigo = 494;

unload to tab_marca26062020.unl
select * FROM  tab_marca
WHERE   marca_cod = 494;

unload to tab_rch_marca26062020.unl
select * FROM tab_rch_marca
WHERE  rechazo_cod = 494;

unload to cta_convivencia_26062020.unl
select * FROM  cta_convivencia
WHERE  marca_entra = 494;


--

DELETE FROM  tab_movimiento
WHERE   codigo = 494;

DELETE FROM   cta_convivencia
WHERE  marca_activa = 494;

DELETE FROM   cta_convivencia
WHERE  marca_entra = 494;

DELETE FROM tab_rch_marca
WHERE  rechazo_cod = 494;

DELETE FROM  tab_marca
WHERE   marca_cod = 494;

--

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

