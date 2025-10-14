SELECT * 
FROM   ret_solicitud_tx 
WHERE  1=2 INTO TEMP cta;

LOAD FROM "/safre_lst/resp-ret_solicitud_tx-CPL3884.unl"
INSERT INTO cta;

SELECT UNIQUE nss, consecutivo
FROM   cta
INTO TEMP tmp_nss_ret;

DELETE
FROM  ret_solicitud_tx
WHERE nss         IN (SELECT nss         FROM tmp_nss_ret)
AND   consecutivo IN (SELECT consecutivo FROM tmp_nss_ret);

LOAD FROM "/safre_lst/resp-ret_solicitud_tx-CPL3884.unl"
INSERT INTO ret_solicitud_tx;