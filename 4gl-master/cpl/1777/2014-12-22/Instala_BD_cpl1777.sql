UNLOAD TO "/safre_lst/b_uni_unificado_cpl1777.unl"
SELECT * FROM uni_unificado
WHERE nss_cta1 = "13735310313"
AND estado = 100
;

UPDATE uni_unificado SET estado = 60
WHERE nss_cta1 = "13735310313"
AND estado = 100
;


