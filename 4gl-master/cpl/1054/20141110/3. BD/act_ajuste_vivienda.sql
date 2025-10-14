UNLOAD TO resp_ret_ajuste_vivienda_nss.unl
SELECT *
FROM   ret_ajuste_vivienda_nss
ORDER BY nss
;
SELECT nss                          ,
       consecutivo                  ,
       fecha_pago[4,5] || "/" || 
       fecha_pago[1,2] || "/" || 
       fecha_pago[7,10] fecha_pago  ,
       fecha_pago[4,5] || "/" || 
       "01/" || 
       fecha_pago[7,10] fecha_id_aivs
FROM   ret_ajuste_vivienda_nss
WHERE  fecha_pago[4,5] <= "12"
UNION
SELECT nss                          ,
       consecutivo                  ,
       fecha_pago                   ,
       fecha_id_aivs
FROM   ret_ajuste_vivienda_nss
WHERE  fecha_pago[4,5] > "12"
INTO TEMP tmp_fechas_ajuste
;
UPDATE tmp_fechas_ajuste
SET    fecha_pago       = NULL      ,
       fecha_id_aivs    = NULL
WHERE  fecha_pago = "  /  /    "
;
SELECT A.nss                ,
       A.consecutivo        ,
       A.folio              ,
       A.viv_92_aivs        ,
       A.viv_92_aivs_cta    , 
       A.viv_92_aivs_dif    , 
       A.viv_97_aivs        ,
       A.viv_97_aivs_cta    , 
       A.viv_97_aivs_dif    , 
       A.viv_92_pesos       ,
       A.viv_92_pesos_cta   ,  
       A.viv_92_pesos_dif   ,  
       A.viv_97_pesos       ,
       A.viv_97_pesos_cta   ,  
       A.viv_97_pesos_dif   ,  
       A.neto_depositado    , 
       B.fecha_pago         ,
       A.precio_accion      ,
       B.fecha_id_aivs      ,
       A.estado    
FROM   ret_ajuste_vivienda_nss A        ,
       tmp_fechas_ajuste B
WHERE  A.nss            = B.nss
AND    A.consecutivo    = B.consecutivo
INTO TEMP tmp_ajuste_vivienda_nss
;
DELETE
FROM   ret_ajuste_vivienda_nss
WHERE  1 = 1
;
ALTER TABLE ret_ajuste_vivienda_nss
MODIFY fecha_pago DATE 
;
ALTER TABLE ret_ajuste_vivienda_nss
MODIFY fecha_id_aivs DATE 
;
INSERT INTO ret_ajuste_vivienda_nss
SELECT *
FROM   tmp_ajuste_vivienda_nss
;
DROP TABLE tmp_fechas_ajuste
;
DROP TABLE tmp_ajuste_vivienda_nss
