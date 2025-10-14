UNLOAD TO res_int_det_voldom_pre_0623.unl
SELECT *
FROM int_det_voldom_pre;

ALTER TABLE int_det_voldom_pre ADD origen_presolicitud CHAR(2) BEFORE usuario;
ALTER TABLE int_det_voldom_pre ADD fecha_vigencia CHAR(5) BEFORE usuario;

UPDATE STATISTICS FOR TABLE int_det_voldom_pre;