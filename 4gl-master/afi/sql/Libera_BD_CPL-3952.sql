DELETE FROM tab_ocupacion WHERE ocupa_cod = 15;
INSERT INTO tab_ocupacion (ocupa_cod,ocupa_desc,ocupa_desc_corta) VALUES (15,'ESTUDIANTE','ESTUDIANTE');

ALTER TABLE afi_solicitud_movil ADD (ocupacion CHAR(2) BEFORE status);
ALTER TABLE afi_solicitud_movil ADD (nivel_estudios CHAR(2) BEFORE status);

ALTER TABLE afi_his_solicitud_movil ADD (ocupacion CHAR(2) BEFORE status);
ALTER TABLE afi_his_solicitud_movil ADD (nivel_estudios CHAR(2) BEFORE status);
