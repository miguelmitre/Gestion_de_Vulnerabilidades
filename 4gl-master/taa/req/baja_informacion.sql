CREATE TEMP TABLE tc008
(
nssocial  char(11)
);

LOAD FROM "archivo_C008.unl"
INSERT INTO tc008;

UNLOAD TO "afi_mae_c008.unl"
SELECT * FROM  afi_mae_afiliado
WHERE n_seguro IN (SELECT nssocial FROM tc008);

UNLOAD TO "afi_sol_c008.unl"
SELECT * FROM  afi_solicitud
WHERE n_seguro IN (SELECT nssocial FROM tc008);


CREATE TEMP TABLE tc019
(
nssocial  char(11)
);

LOAD FROM "archivo_C019.unl"
INSERT INTO tc019;

UNLOAD TO "afi_mae_tc019.unl"
SELECT * FROM  afi_mae_afiliado
WHERE n_seguro IN (SELECT nssocial FROM tc019);

UNLOAD TO "afi_sol_tc019.unl"
SELECT * FROM  afi_solicitud
WHERE n_seguro IN (SELECT nssocial FROM tc019);


