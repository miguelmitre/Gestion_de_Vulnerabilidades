UNLOAD TO "/safre_lst/b_dis_cuenta_cpl777.unl"
SELECT * FROM dis_cuenta
WHERE  nss  IN ("76895300275","13735310313")
AND folio = 38224
AND tipo_movimiento IN (280,590)
;
UNLOAD TO "/safre_lst/b_uni_unificado_cpl1777-1.unl"
SELECT * FROM uni_unificado
WHERE nss_cta1 = "13735310313"
AND estado = 60
;
--invadido
UPDATE dis_cuenta SET monto_en_acciones = -1162.101136 
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 1
;
UPDATE dis_cuenta SET monto_en_acciones = -2579.772041 
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 2
;
UPDATE dis_cuenta SET monto_en_acciones = -2539.410
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 4
;
UPDATE dis_cuenta SET monto_en_acciones = -2393.611163 
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 5
;
UPDATE dis_cuenta SET monto_en_acciones = -33.884994 
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 6
;
UPDATE dis_cuenta SET monto_en_acciones = -5855.976036 
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 7
;
UPDATE dis_cuenta SET monto_en_acciones = -8488.010000
WHERE  nss   = "76895300275"
AND folio = 38224
AND tipo_movimiento = 280
AND subcuenta = 8
;

--asociado
UPDATE dis_cuenta SET monto_en_acciones = 1162.101136 
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 1
;
UPDATE dis_cuenta SET monto_en_acciones = 2579.772041 
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 2
;
UPDATE dis_cuenta SET monto_en_acciones = 2539.410
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 4
;
UPDATE dis_cuenta SET monto_en_acciones = 2393.611163 
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 5
;
UPDATE dis_cuenta SET monto_en_acciones = 33.884994 
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 6
;
UPDATE dis_cuenta SET monto_en_acciones = 5855.976036 
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 7
;
UPDATE dis_cuenta SET monto_en_acciones = 8488.010000
WHERE  nss   = "13735310313"
AND folio = 38224
AND tipo_movimiento = 590
AND subcuenta = 8
;

--pesos 
UPDATE dis_cuenta SET monto_en_pesos = monto_en_acciones * precio_accion
WHERE  nss  IN ("76895300275","13735310313")
AND folio = 38224
AND tipo_movimiento IN (280,590)
;

UPDATE uni_unificado SET estado = 110
WHERE nss_cta1 = "13735310313"
AND estado = 60
;



