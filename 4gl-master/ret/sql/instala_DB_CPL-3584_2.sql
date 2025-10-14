UNLOAD TO ret_ctr_benef_67816385644.unl
SELECT * 
FROM   ret_ctr_benef
WHERE  nss = '67816385644'
AND    consecutivo_solic = 13372590
AND    tipo_benef = 3
AND    tipo_retiro = 'D';

DELETE 
FROM   ret_ctr_benef
WHERE  nss = '67816385644'
AND    consecutivo_solic = 13372590
AND    tipo_benef = 3
AND    tipo_retiro = 'D';
