UNLOAD TO "/safre_lst/b_dis_cuenta_cpl777-1.unl"
SELECT * FROM dis_cuenta
WHERE  nss  IN ("76895300275","13735310313")
AND folio = 38236
AND tipo_movimiento IN (1,242)
;

DELETE FROM dis_cuenta
WHERE  nss  IN ("76895300275","13735310313")
AND folio = 38236
AND tipo_movimiento IN (1,242)
;

