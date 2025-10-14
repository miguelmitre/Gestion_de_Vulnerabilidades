UPDATE pen_solicitud_pmg
SET    estado_solicitud = 70
WHERE  estado_solicitud = 50
AND    ((nss = '01765829690' AND consecutivo = 13268546) OR 
        (nss = '19786014233' AND consecutivo = 12318409) OR
        (nss = '49815805400' AND consecutivo = 11498199) OR
        (nss = '65856061174' AND consecutivo = 11882701));
        
UNLOAD TO pen_ctr_pago_det_CPL-3934.unl
SELECT *
FROM   pen_ctr_pago_det
WHERE  ((nss = '01765829690' AND consecutivo = 13268546) OR 
        (nss = '19786014233' AND consecutivo = 12318409) OR 
        (nss = '49815805400' AND consecutivo = 11498199) OR 
        (nss = '65856061174' AND consecutivo = 11882701))
AND    estado = 10
AND    fecha_pago_estimada between '01/01/2023' AND '01/31/2023';

DELETE 
FROM   pen_ctr_pago_det
WHERE  ((nss = '01765829690' AND consecutivo = 13268546) OR 
        (nss = '19786014233' AND consecutivo = 12318409) OR 
        (nss = '49815805400' AND consecutivo = 11498199) OR 
        (nss = '65856061174' AND consecutivo = 11882701))
AND    estado = 10
AND    fecha_pago_estimada between '01/01/2023' AND '01/31/2023';

UNLOAD TO cta_act_marca_CPL-3934.unl
SELECT *
FROM   cta_act_marca
WHERE  ((nss = '01765829690' AND correlativo = 13268546) OR 
        (nss = '19786014233' AND correlativo = 12318409) OR 
        (nss = '49815805400' AND correlativo = 11498199) OR 
        (nss = '65856061174' AND correlativo = 11882701))
AND    marca_cod = 841;

DELETE 
FROM   cta_act_marca
WHERE  ((nss = '01765829690' AND correlativo = 13268546) OR 
        (nss = '19786014233' AND correlativo = 12318409) OR 
        (nss = '49815805400' AND correlativo = 11498199) OR 
        (nss = '65856061174' AND correlativo = 11882701))
AND    marca_cod = 841;
        
UPDATE cta_his_marca
SET    fecha_fin = TODAY,
       fecha_causa = TODAY,
       usr_desmarca = 'safre'
WHERE  ((nss = '01765829690' AND correlativo = 13268546) OR 
        (nss = '19786014233' AND correlativo = 12318409) OR 
        (nss = '49815805400' AND correlativo = 11498199) OR 
        (nss = '65856061174' AND correlativo = 11882701))
AND    marca_cod = 841;
