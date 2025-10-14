INSERT INTO dis_cuenta
SELECT *
FROM   dis_provision
WHERE  nss IN ("65644519533",  
               "12684618411"
              )
AND    folio        = 37962
AND    siefore      = 11
;
UPDATE dis_cuenta
SET    fecha_pago       = MDY(12,17,2014),
       fecha_conversion = MDY(12,17,2014),
       fecha_proceso    = MDY(12,17,2014),
       fecha_archivo    = MDY(12,17,2014)
WHERE  nss IN ("65644519533",  
               "12684618411"
              ) 
AND    folio        = 37962
AND    siefore      = 11
;
UPDATE dis_cuenta
SET    curp = "AAGA461025HGTLRN09"
WHERE  nss  = "12684618411"
AND    folio        = 37962
AND    siefore      = 11
;
UPDATE dis_cuenta
SET    curp = "GURM450115HVZVMR02"
WHERE  nss  = "65644519533"
AND    folio        = 37962
AND    siefore      = 11
