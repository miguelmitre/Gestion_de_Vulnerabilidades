CREATE INDEX idx_umi_det_solicitud_2
ON umi_det_solicitud (cve_ent_nss,cve_ent_curp,nss,curp,estado)
IN uni_dbs1;

UPDATE STATISTICS FOR TABLE umi_det_solicitud;
