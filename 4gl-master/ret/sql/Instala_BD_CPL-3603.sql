SELECT MAX(folio)+1 folio
FROM glo_folio
INTO TEMP tmp3603;

INSERT INTO glo_folio  SELECT folio FROM tmp3603;

--01715375349
INSERT INTO dis_cuenta VALUES (640,1,90, (SELECT folio FROM tmp3603),0,"01715375349","",0,TODAY,TODAY,TODAY,22615.80,0,0,0," ","IMSS-3603",8,TODAY,USER,TODAY,0);


UPDATE dis_cuenta set precio_accion = (SELECT glo_valor_accion.precio_del_dia FROM glo_valor_accion
                                       WHERE glo_valor_accion.fecha_valuacion = TODAY
                                       AND   glo_valor_accion.codigo_siefore = dis_cuenta.siefore
                                       AND dis_cuenta.folio =(SELECT folio FROM tmp3603) ) 
WHERE nss IN ("01715375349")
AND folio = (SELECT folio FROM tmp3603);


UPDATE dis_cuenta set monto_en_acciones = monto_en_pesos / precio_accion
WHERE nss IN ("01715375349")
AND folio = (SELECT folio FROM tmp3603)
AND precio_accion != 0;