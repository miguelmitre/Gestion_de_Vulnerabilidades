
UNLOAD TO '/safre_lst/uni_ado_CPL-3939.unl'
SELECT *
FROM   uni_unificado
WHERE  folio = 640
AND    nss_uni  = '21947566069'
AND    nss_cta1 = '25957404574'
AND    estado   = 100
;

UPDATE uni_unificado
SET    estado = 60
WHERE  folio = 640
AND    nss_uni  = '21947566069'
AND    nss_cta1 = '25957404574'
AND    estado   = 100
;
 
UPDATE uni_unificado
SET    estado_traspaso = 1
WHERE  folio = 640
AND    nss_uni  = '21947566069'
AND    nss_cta1 = '21077408355'
;