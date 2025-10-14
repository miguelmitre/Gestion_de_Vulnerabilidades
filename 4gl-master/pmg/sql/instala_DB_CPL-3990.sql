UPDATE pen_ctr_pago_det_iss
SET    mto_pago_pesos  = 6121.31
WHERE  ((nss = '11755510382' AND consecutivo = 13282066 AND num_mensualidad = 23) OR
        (nss = '12755600421' AND consecutivo = 13646535 AND num_mensualidad = 8) OR
        (nss = '54825540302' AND consecutivo = 13646831 AND num_mensualidad = 8))
AND    estado = 10;