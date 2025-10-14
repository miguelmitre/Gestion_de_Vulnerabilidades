ALTER TABLE safre_af:ret_tipo_origen_reinv ADD clave_motivo char(2);

UPDATE ret_tipo_origen_reinv set clave_motivo = '07' WHERE id_tipo_origen_reinv = 1;
UPDATE ret_tipo_origen_reinv set clave_motivo = '07' WHERE id_tipo_origen_reinv = 2;
UPDATE ret_tipo_origen_reinv set clave_motivo = '01' WHERE id_tipo_origen_reinv = 3;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 4;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 5;
UPDATE ret_tipo_origen_reinv set clave_motivo = '04' WHERE id_tipo_origen_reinv = 6;
UPDATE ret_tipo_origen_reinv set clave_motivo = '05' WHERE id_tipo_origen_reinv = 7;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 8;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 9;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 10;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 11;
UPDATE ret_tipo_origen_reinv set clave_motivo = '06' WHERE id_tipo_origen_reinv = 12;
UPDATE ret_tipo_origen_reinv set clave_motivo = '02' WHERE id_tipo_origen_reinv = 13;
UPDATE ret_tipo_origen_reinv set clave_motivo = '03' WHERE id_tipo_origen_reinv = 14;
