UNLOAD TO "/safre_lst/resp-ret_solicitud_tx-CPL3884.unl"
SELECT *
FROM   ret_solicitud_tx
WHERE  diag_registro = 400
AND    estado_solicitud = 20
AND    fecha_captura > '10/04/2021';

UPDATE ret_solicitud_tx
SET    diag_registro = NULL
WHERE  diag_registro = 400
AND    estado_solicitud = 20
AND    fecha_captura > '10/04/2021';