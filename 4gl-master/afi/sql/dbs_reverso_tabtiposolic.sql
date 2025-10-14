-- se elimina la informacion previo a la insercion
DELETE FROM tab_tipo_traspaso WHERE tipo_traspaso = '76';

-- se carga la informacion respaldada
LOAD FROM resp_tabtipotraspaso_CPL3431.unl
INSERT INTO tab_tipo_traspaso;

-- se elimina la informacion previo a la insercion
DELETE FROM tab_tipo_solic WHERE tipo_solicitud IN (42,43);

-- se carga la informacion respaldada
LOAD FROM resp_tabtiposolic_CPL3431.unl
INSERT INTO tab_tipo_solic;


