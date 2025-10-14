ALTER TABLE ret_beneficiario
DROP num_ord_pago
;
ALTER TABLE ret_beneficiario
DROP clabe
;
ALTER TABLE ret_beneficiario
DROP tipo_cuenta
;
ALTER TABLE ret_beneficiario
DROP correo_elect
;
ALTER TABLE ret_beneficiario
DROP curp_benef
;
ALTER TABLE ret_beneficiario
DROP nss_benef
;
ALTER TABLE ret_ajuste_vivienda_nss
DROP otros_importes
;
DROP TABLE ret_notifica_vivienda
;
DROP TABLE ret_folio_infonavit
;
DROP TABLE ret_recepcion
; 
DELETE
FROM   cta_convivencia
WHERE  marca_entra = 819
;
DELETE
FROM   cta_convivencia
WHERE  marca_activa = 819
;
DELETE
FROM   tab_rch_marca
WHERE  rechazo_cod = 819
;
DELETE
FROM   tab_marca
WHERE  marca_cod = 819
