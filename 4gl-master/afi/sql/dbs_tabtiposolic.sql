-- se respalda la informacion existente en catalogo
UNLOAD TO resp_tabtipotraspaso_CPL3431.unl
SELECT * FROM tab_tipo_traspaso WHERE tipo_traspaso = '76';

-- se elimina la informacion previo a la insercion
DELETE FROM tab_tipo_traspaso WHERE tipo_traspaso = '76';

-- se inserta la informacion en catalogo
INSERT INTO tab_tipo_traspaso VALUES ('76','taa','09',110,'TC-','Traspaso via APP por NSS-CURP/CURP',NULL,30221,'safre',TODAY);
INSERT INTO tab_tipo_traspaso VALUES ('76','taa','12',110,'TC-','Traspaso via APP por NSS-CURP/CURP',NULL,NULL,'safre',TODAY);

-- se respalda la información existente en catálogo
UNLOAD TO resp_tabtiposolic_CPL3431.unl
SELECT * FROM tab_tipo_solic WHERE tipo_solicitud IN (42,43);

-- se elimina la informacion previo a la insercion
DELETE FROM tab_tipo_solic WHERE tipo_solicitud IN (42,43);

-- se inserta la informacion en catalogo
INSERT INTO tab_tipo_solic VALUES (42, 'TRASPASO WEB POR NSS');
INSERT INTO tab_tipo_solic VALUES (43, 'TRASPASO WEB POR CURP');
