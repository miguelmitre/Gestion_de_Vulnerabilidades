ALTER TABLE int_det_voldom_pre DROP origen_presolicitud CHAR(2);
ALTER TABLE int_det_voldom_pre DROP fecha_vigencia CHAR(5);

UPDATE STATISTICS FOR TABLE int_det_voldom_pre;