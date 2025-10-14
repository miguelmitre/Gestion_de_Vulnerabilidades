UNLOAD TO cta_ctr_proceso_CPL-3593-2.unl
SELECT *
FROM   cta_ctr_proceso
WHERE  nss IN ('01715287593','61866908298','45068513493','72795711042')
AND    fecha_inicio = '02/01/2022'
AND    fecha_fin    = '02/28/2022';

DELETE
FROM   cta_ctr_proceso
WHERE  nss IN ('01715287593','61866908298','45068513493','72795711042')
AND    fecha_inicio = '02/01/2022'
AND    fecha_fin    = '02/28/2022';
