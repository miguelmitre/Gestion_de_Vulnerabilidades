DELETE FROM  tab_movimiento
WHERE   codigo = 494;

DELETE FROM  tab_marca
WHERE   marca_cod = 494;

DELETE FROM tab_rch_marca
WHERE  rechazo_cod = 494;

DELETE FROM   cta_convivencia
WHERE  marca_entra = 494;

DELETE FROM   cta_convivencia
WHERE  marca_activa = 494;

DROP TABLE ret_dev_ap_vol;

DELETE FROM men_safre WHERE menu_cod='010906050000';
DELETE FROM men_privilegios WHERE menu_cod='010906050000';