--IMSS
INSERT INTO tab_diag_procesar_disp VALUES ('L36','SIN EXISTENCIA DE UNA DISPOSICIÓN PREVIA Y LIQUIDADA',0);
INSERT INTO tab_diag_procesar_disp VALUES ('L37','SIN DERECHO CARGADO Y VIGENTE EN DATAMART',0);
INSERT INTO tab_diag_procesar_disp VALUES ('C61','ORIGEN DEL TRÁMITE INVALIDO',0);
INSERT INTO tab_diag_procesar_disp VALUES ('E89','ESTRUCTURA DEL FOLIO INFONAVIT INVALIDO',0);

--ISSSTE
INSERT INTO tab_diag_procesar_disp VALUES ('L38','SIN EXISTENCIA DE UNA DISPOSICIÓN PREVIA Y LIQUIDADA',0);
INSERT INTO tab_diag_procesar_disp VALUES ('L39','SIN DERECHO CARGADO Y VIGENTE EN DATAMART',0);

CREATE TABLE ret_pagos_complementarios (
	nss                      CHAR(11) NOT NULL,
	consecutivo			DECIMAL(11,0) NOT NULL,
	consecutivo_origen   	DECIMAL(11,0),
	ind_pregunta			SMALLINT, -- NULL,O o 1
	ind_complementario		SMALLINT, -- 1
	ind_proceso			CHAR(6) --IMSS O ISSSTE
) IN ret_dbs1;