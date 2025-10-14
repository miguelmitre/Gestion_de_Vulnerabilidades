DELETE FROM tab_ocupacion WHERE ocupa_cod = 15;
DROP PROCEDURE sp_inserta_sol_movil;

ALTER TABLE afi_solicitud_movil DROP (ocupacion);
ALTER TABLE afi_solicitud_movil DROP (nivel_estudios);

ALTER TABLE afi_his_solicitud_movil DROP (ocupacion);
ALTER TABLE afi_his_solicitud_movil DROP (nivel_estudios);