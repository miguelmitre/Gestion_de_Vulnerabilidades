CREATE TABLE ret_cat_isr_reten
  (
    fecha_ini date,
    fecha_fin date,
    tasa_reten decimal(16,6),
    fecha_actualizacion date,
    usuario char(20)
  );

INSERT INTO ret_cat_isr_reten VALUES ('01/01/2021','12/31/2021',0.97,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2020','12/31/2020',1.45,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2019','12/31/2019',1.04,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2018','12/31/2018',0.46,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2017','12/31/2017',0.58,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2016','12/31/2016',0.5,'12/30/2020','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2022','12/31/2022',0.08,'01/01/2022','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2023','12/31/2023',0.15,'01/01/2023','safre');
INSERT INTO ret_cat_isr_reten VALUES ('01/01/2024',NULL,0.50,'01/01/2024','safre');
