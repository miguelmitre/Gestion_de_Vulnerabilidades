SELECT MAX(folio)+1 folio
FROM glo_folio
INTO TEMP tmp3654;

INSERT INTO glo_folio
SELECT folio FROM tmp3654;

INSERT INTO dis_cuenta VALUES (640,2,14, (SELECT folio FROM tmp3654),0,"49825810069"," ",0,TODAY,TODAY,TODAY,2840.970000,0,0,0," ","IMSS-3654",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,5,14, (SELECT folio FROM tmp3654),0,"49825810069"," ",0,TODAY,TODAY,TODAY,1231.310000,0,0,0," ","IMSS-3654",8,TODAY,USER,TODAY,0);
INSERT INTO dis_cuenta VALUES (640,6,14, (SELECT folio FROM tmp3654),0,"49825810069"," ",0,TODAY,TODAY,TODAY,3.390000,0,0,0," ","IMSS-3654",8,TODAY,USER,TODAY,0);

UPDATE dis_cuenta
set precio_accion = (SELECT glo_valor_accion.precio_del_dia FROM glo_valor_accion
                    WHERE glo_valor_accion.fecha_valuacion = TODAY
                    AND   glo_valor_accion.codigo_siefore = dis_cuenta.siefore
                    AND dis_cuenta.folio =(SELECT folio FROM tmp3654) ) 
WHERE nss IN ("49825810069")
AND folio = (SELECT folio FROM tmp3654);

UPDATE dis_cuenta
set monto_en_acciones = monto_en_pesos / precio_accion
WHERE nss IN ("49825810069")
AND folio = (SELECT folio FROM tmp3654);

UNLOAD TO /safre_lst/folio_dis_cuenta_3654.unl
SELECT folio                                  
FROM   tmp3654; 