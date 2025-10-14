UPDATE uni_unificador
SET    cve_afo_aclara = 538
WHERE  nss_uni = '63998300790'
AND    folio   = 140
;

UPDATE uni_unificador
SET    cve_afo_aclara = 538
WHERE  nss_uni = '72937231487'
AND    folio   = 140 
; 

UPDATE uni_unificado
SET    cve_afo_aclara = 538
WHERE  nss_uni = '72937231487'
AND    folio   = 140 
; 

UPDATE uni_unificador
SET    cve_afo_recep = 568
WHERE  nss_uni = '54886999918'
AND    folio   = 140 
; 

UPDATE tab_afore
SET    marca = 1,
       afore_fusion = 538
WHERE  afore_cod = 532
;