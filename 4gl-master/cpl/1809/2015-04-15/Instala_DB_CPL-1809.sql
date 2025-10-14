-- Creando el respaldo de las tablas de marcas
UNLOAD TO "cta_his_marca_CPL1809.unl" 
SELECT * 
FROM   cta_his_marca
WHERE  marca_cod = 231;

UNLOAD TO "cta_act_marca_CPL1809.unl" 
SELECT * 
FROM   cta_act_marca
WHERE  marca_cod = 231;

-- Creando el script para insertar en marca actual
UNLOAD TO "marca_cuenta_CPL1809.sql" DELIMITER ';'  
SELECT unique "INSERT INTO cta_act_marca VALUES("||
                                  "'"||a.nss_fovissste||"',"||     --nss         
                                  "231,"||                       --marca_cod   
                                  "'"||a.fecha_movimiento||"',"||  --fecha_ini   
                                  "CURRENT,"||                   --hora_ini    
                                  "0,"||                         --estado_marca
                                  "0,"||                         --marca_causa 
                                  "'"||a.fecha_movimiento||"',"||  --fecha_causa 
                                  a.cont_servicio||","||           --correlativo 
                                  "'safrefov')"                  --usuario     
FROM   acr_det_ced_issste a,
       cta_ctr_cuenta b           
WHERE  a.estado = 1
AND    a.tipo_movimiento = 1
AND    a.nss_fovissste = b.nss
AND    a.nss_fovissste NOT IN ("","00000000000")
AND    a.nss_fovissste NOT IN (SELECT c.nss 
                             FROM   cta_his_marca c 
                             WHERE  c.marca_cod = 231 
                             AND    c.fecha_fin IS NULL)
AND    a.nss_fovissste NOT IN (SELECT d.nss 
                             FROM   cta_act_marca d 
                             WHERE  d.marca_cod = 231);

                             
-- Creando el script para insertar en historico de marcas
UNLOAD TO "his_marca_cuenta_CPL1809.sql" DELIMITER ';'  
SELECT unique "INSERT INTO cta_his_marca VALUES("||
                                  "'"||a.nss_fovissste||"',"||     --nss          
                                  "231,"||                       --marca_cod    
                                  "'"||a.fecha_movimiento||"',"||  --fecha_ini    
                                  "NULL," ||                     --fecha_fin    
                                  "CURRENT,"||                   --hora_ini     
                                  "0,"||                         --estado_marca 
                                  "0,"||                         --rechazo_cod  
                                  "0,"||                         --marca_causa  
                                  "'"||a.fecha_movimiento||"',"||  --fecha_causa  
                                  a.cont_servicio||","||           --correlativo  
                                  "'safrefov',"||                --usr_marca    
                                  "NULL)"                        --usr_desmarca 
FROM   acr_det_ced_issste a,
       cta_ctr_cuenta b                
WHERE  a.estado = 1
AND    a.tipo_movimiento = 1
AND    a.nss_fovissste = b.nss
AND    a.nss_fovissste NOT IN ("","00000000000")
AND    a.nss_fovissste NOT IN (SELECT c.nss 
                             FROM   cta_his_marca c 
                             WHERE  c.marca_cod = 231 
                             AND    c.fecha_fin IS NULL)
AND    a.nss_fovissste NOT IN (SELECT d.nss 
                             FROM   cta_act_marca d 
                             WHERE  d.marca_cod = 231);
                             
                             
UNLOAD TO "marca_sol_faltantes_CPL1809.sql" DELIMITER ";"
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta06 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta07 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta08 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta09 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta10 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta11 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta12 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
UNION ALL
SELECT unique "EXECUTE PROCEDURE marca_cuenta("||
                                  "'"||a.nss||"',"||                 --nss           
                                  "231,"||                           --marca_entra   
                                  "'"||a.consecutivo_lote||"',"||    --correlativo   
                                  "0,"||                             --estado_marca  
                                  "0,"||                             --codigo_rechazo
                                  "0,"||                             --marca_causa   
                                  "'"||a.fecha_conversion||"',"||    --fecha_causa   
                                  "'safrefov')"                      --usuario
FROM   dis_cuenta13 a
WHERE  a.nss NOT IN (SELECT unique b.nss_fovissste
                   FROM   acr_det_ced_issste b
                   WHERE  b.folio = a.folio 
                   )
AND    a.id_aportante = "ACR-ISSSTE"
AND    a.folio_sua    = "FOV"
GROUP BY a.nss,a.consecutivo_lote,
         a.fecha_conversion,a.usuario
;