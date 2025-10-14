SELECT MAX(folio)+1 folio
FROM glo_folio
INTO TEMP tmp3874;

INSERT INTO glo_folio
SELECT folio FROM tmp3874;

INSERT INTO dis_cuenta VALUES (640,2,90, (SELECT folio FROM tmp3874),0,"15643810136"," ",0,TODAY,TODAY,TODAY,2590.370000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,5,90, (SELECT folio FROM tmp3874),0,"15643810136"," ",0,TODAY,TODAY,TODAY,3280.040000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,6,90, (SELECT folio FROM tmp3874),0,"15643810136"," ",0,TODAY,TODAY,TODAY,93.760000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);

INSERT INTO dis_cuenta VALUES (640,2,16, (SELECT folio FROM tmp3874),0,"62926809690"," ",0,TODAY,TODAY,TODAY,53983.010000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,5,16, (SELECT folio FROM tmp3874),0,"62926809690"," ",0,TODAY,TODAY,TODAY,24878.570000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,6,16, (SELECT folio FROM tmp3874),0,"62926809690"," ",0,TODAY,TODAY,TODAY,1386.730000,0,0,0," ","IMSS-3874",8,TODAY,USER,TODAY,0);


UPDATE dis_cuenta
set precio_accion = (SELECT glo_valor_accion.precio_del_dia FROM glo_valor_accion
                    WHERE glo_valor_accion.fecha_valuacion = TODAY
                    AND   glo_valor_accion.codigo_siefore = dis_cuenta.siefore
                    AND dis_cuenta.folio =(SELECT folio FROM tmp3874) ) 
WHERE nss IN ("15643810136","62926809690")
AND folio = (SELECT folio FROM tmp3874);

UPDATE dis_cuenta
set monto_en_acciones = monto_en_pesos / precio_accion
WHERE nss IN ("15643810136","62926809690")
AND folio = (SELECT folio FROM tmp3874);

UNLOAD TO /safre_lst/folio_dis_cuenta_3874.unl
SELECT folio                                  
FROM   tmp3874; 
