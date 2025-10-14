SELECT folio
FROM   glo_folio
WHERE  1 = 2
INTO TEMP tmp3874;

LOAD FROM /safre_lst/folio_dis_cuenta_3874.unl INSERT INTO tmp3874;          

DELETE  
FROM   dis_cuenta
WHERE  folio = (SELECT folio FROM tmp3874)
AND    nss IN ('15643810136','62926809690');

DELETE 
FROM   glo_folio
WHERE  folio = (SELECT folio FROM tmp3874);
