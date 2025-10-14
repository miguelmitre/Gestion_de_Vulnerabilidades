-- Cerrando las cuentas por el proceso de Devolucion Saldo FOVISSSTE  
UNLOAD TO "desmarca_cuenta_CPL1809.sql" DELIMITER ';'     
SELECT unique "EXECUTE PROCEDURE desmarca_cuenta ("||
                                  "'"||afi.n_seguro||"',"||  -- nss         
                                  "231,"||                   -- marca_entra 
                                  mar.correlativo||","||     -- correlativo 
                                  "0,"||                     -- estado_marca
                                  "0,"||                     -- marca_causa 
                                  "'safrefov')"              -- usuario     
FROM   acr_det_dev_issste dev, 
       afi_mae_afiliado   afi,
       cta_act_marca      mar,
       cta_ctr_cuenta     ctr           
WHERE  dev.estado = 0
AND    dev.curp_fovissste = afi.n_unico
AND    afi.n_seguro       = mar.nss
AND    mar.marca_cod      = 231
AND    mar.nss = ctr.nss;



