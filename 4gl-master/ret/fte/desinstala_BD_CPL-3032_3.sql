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

load from tab_movimiento26062020.unl
insert into tab_movimiento;

load from tab_marca26062020.unl
insert into tab_marca;

load from tab_rch_marca26062020.unl
insert into tab_rch_marca;

load from cta_convivencia_26062020.unl
insert into cta_convivencia;
