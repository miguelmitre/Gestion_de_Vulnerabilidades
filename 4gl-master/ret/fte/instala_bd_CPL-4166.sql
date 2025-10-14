DELETE
FROM  tra_cza_trasp_issste        
WHERE folio = 108958    
;
DELETE
FROM  tra_det_trasp_sal_issste
WHERE folio = 108958
;
DELETE
FROM  tra_det_trasp_int_issste
WHERE folio = 108958
;
DELETE
FROM  tra_sum_trasp_issste
WHERE folio = 108958
;
DELETE
FROM  tra_no_actualiza_iss
WHERE folio = 108958
;
DELETE
FROM  tra_cza_trasp_issste    
WHERE fech_presentacion = "08/18/2023"