DROP TABLE ret_trabajador_atributo_pension;
DROP SEQUENCE seq_ret_trab_atrib_pension;

DELETE FROM cta_convivencia
WHERE marca_entra = 935 OR marca_activa = 935;

DELETE FROM tab_marca
WHERE marca_cod = 935;

DELETE FROM tab_rch_marca
WHERE rechazo_cod = 935;
