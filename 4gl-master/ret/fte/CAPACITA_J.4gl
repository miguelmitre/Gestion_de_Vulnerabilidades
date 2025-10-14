GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroJ()

   DELETE from dis_provision
   WHERE nss IN ("55663710022","55753310436")
 
   DELETE from ret_solicitud_rx
   WHERE nss IN ("55663710022","55753310436")

   DELETE from ret_cza_lote
   WHERE nom_archivo = "OP06-J" 

   DELETE from afi_mae_afiliado
   WHERE n_seguro IN ("55663710022","55753310436")

   LOAD FROM "afi_mae_afiliadoJ.unl"
   INSERT INTO afi_mae_afiliado

   DELETE from dis_det_aporte
   WHERE n_seguro IN ("55663710022","55753310436")

   LOAD FROM "dis_det_aporteJ.unl"
   INSERT INTO dis_det_aporte

   DELETE FROM ret_ctr_envio_lote
   WHERE tipo_retiro = "J"
   AND fecha_envio = today 

   DELETE FROM cta_act_marca
   WHERE nss IN ("55663710022","55753310436")

   DELETE FROM cta_his_marca
   WHERE nss IN ("55663710022","55753310436")

   DELETE from cta_ctr_cuenta
   WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "cta_ctr_cuentaJ.unl"
   INSERT INTO cta_ctr_cuenta

   DELETE from dis_cuenta
   WHERE nss IN ("55663710022","55753310436")
   
 #  DELETE from dis_cuenta97
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta97J.unl"
   INSERT INTO dis_cuenta 

 #  DELETE from dis_cuenta98
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta98J.unl"
   INSERT INTO dis_cuenta

 #  DELETE from dis_cuenta99
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta99J.unl"
   INSERT INTO dis_cuenta

 #  DELETE from dis_cuenta00
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta00J.unl"
   INSERT INTO dis_cuenta
  
 #  DELETE from dis_cuenta01
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta01J.unl"
   INSERT INTO dis_cuenta

 #  DELETE from dis_cuenta02
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta02J.unl"
   INSERT INTO dis_cuenta

 #  DELETE from dis_cuenta03
 #  WHERE nss IN ("55663710022","55753310436")

   LOAD FROM "dis_cuenta03J.unl"
   INSERT INTO dis_cuenta


   LOAD FROM "dis_cuentaJ.unl"
   INSERT INTO dis_cuenta
 
    
   DELETE from ret_solicitud_tx
   WHERE nss IN ("55663710022","55753310436")
    
--   LOAD FROM "ret_solicitud_txJ.unl"
--   INSERT INTO ret_solicitud_tx
    
END FUNCTION
