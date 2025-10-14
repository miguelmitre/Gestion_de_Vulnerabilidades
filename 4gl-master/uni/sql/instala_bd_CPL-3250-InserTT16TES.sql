-- Respaldo
UNLOAD TO "tes_tipo_id_aportante-CPL-3250.unl"
SELECT *
FROM   tes_tipo_id_aportante
;
-- Borrado
DELETE
FROM  tes_tipo_id_aportante
WHERE tipo_traspaso = 16
;
-- Insercion
INSERT INTO  tes_tipo_id_aportante VALUES (16,"UIMSS","UNIFICACION IMSS APCTA",11,0)