ALTER TABLE int_det_vol_rc
ADD ( tipo_de_ahorrador CHAR(02) BEFORE estado );

ALTER TABLE int_det_vol_rc
ADD ( subcuenta SMALLINT BEFORE estado );
--------------------------------------------------
CREATE TABLE tab_vol_redcom_subcta(
tipo_apor    SMALLINT,
cve_red      CHAR(03),
subcuenta    SMALLINT,
tipo_mov     SMALLINT,
id_aportante CHAR(11),
usuario      CHAR(08),
factualiza   DATE
) IN vol_dbs1
;
----------------------------------------------------------------------------------
INSERT INTO tab_vol_redcom_subcta VALUES(1,'012',16,310,'AFOREMOVIL',USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(1,'028',16,311,'AFOREMOVIL',USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(1,'032',16,310,'AFOREMOVIL',USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(1,'033',16,123,'VE-REDCO'  ,USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(1,'016',16,123,'VE-REDCO'  ,USER,TODAY);


INSERT INTO tab_vol_redcom_subcta VALUES(2,'012',10,310,'AFOREMOVIL',USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(2,'028',10,311,'AFOREMOVIL',USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(2,'032',10,310,'AFOREMOVIL',USER,TODAY);

INSERT INTO tab_vol_redcom_subcta VALUES(3,'016',12,123,'VE-REDCO' ,USER,TODAY);
INSERT INTO tab_vol_redcom_subcta VALUES(3,'033',12,123,'VE-REDCO' ,USER,TODAY);

INSERT INTO tab_vol_redcom_subcta VALUES(4,'032',23,310,'AFOREMOVIL',USER,TODAY);

CREATE INDEX ix01_tab_vol_redcom_subcta ON tab_vol_redcom_subcta(tipo_apor,
cve_red);
UPDATE STATISTICS FOR TABLE tab_vol_redcom_subcta;

-------Excepciones
CREATE TABLE tab_vol_redcom_subcta_excep(
tipo_apor            SMALLINT ,
cve_red              CHAR(03) ,
subcuenta            SMALLINT ,
tipo_mov             SMALLINT ,
id_aportante         CHAR(11) ,
usuario              CHAR(08) ,
factualiza           DATE
) IN vol_dbs1
;

INSERT INTO tab_vol_redcom_subcta_excep VALUES(4,'016',23,123,'VE-REDCO' ,USER,TODAY);
---------------------------------------------------------
INSERT INTO tab_movimiento VALUES(310,'APORTACION VOLUNTARIA REDES COMERCIALES SWAP',1);
INSERT INTO tab_movimiento VALUES(311,'APORTACION VOLUNTARIA REDES COMERCIALES SANTANDER',1);
---------------------------------------------------------