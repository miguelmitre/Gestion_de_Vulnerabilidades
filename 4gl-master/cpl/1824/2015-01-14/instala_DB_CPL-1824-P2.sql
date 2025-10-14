INSERT INTO cta_corte_saldo  VALUES ("01/02/2015","02/02/2015","02/03/2015");
INSERT INTO cta_corte_saldo  VALUES ("02/03/2015","03/01/2015","03/02/2015");
INSERT INTO cta_corte_saldo  VALUES ("03/02/2015","03/31/2015","04/01/2015");
INSERT INTO cta_corte_saldo  VALUES ("04/01/2015","05/03/2015","05/04/2015");
INSERT INTO cta_corte_saldo  VALUES ("05/04/2015","05/31/2015","06/01/2015");
INSERT INTO cta_corte_saldo  VALUES ("06/01/2015","06/30/2015","07/01/2015");
INSERT INTO cta_corte_saldo  VALUES ("07/01/2015","08/02/2015","08/03/2015");
INSERT INTO cta_corte_saldo  VALUES ("08/03/2015","09/01/2015","09/02/2015");
INSERT INTO cta_corte_saldo  VALUES ("09/02/2015","09/30/2015","10/01/2015");
INSERT INTO cta_corte_saldo  VALUES ("10/01/2015","11/02/2015","11/03/2015");
INSERT INTO cta_corte_saldo  VALUES ("11/03/2015","11/30/2015","12/01/2015");
INSERT INTO cta_corte_saldo  VALUES ("12/01/2015","01/03/2016","01/04/2016");

UNLOAD  TO "/safre_prc/dis/rescate/tasa_com_2014.txt"
SELECT * FROM dis_val_comision                       
WHERE tipo_comision = 110;

UPDATE dis_val_comision 
SET val_porcentaje = 1.20,    
    val_porcentaje2 = 1.20,
    fecha_cambio  = TODAY  
WHERE tipo_comision = 110;    

