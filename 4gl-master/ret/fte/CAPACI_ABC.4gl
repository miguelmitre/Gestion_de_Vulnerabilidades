GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroABC()
   DELETE from ret_ctr_envio_lote
   WHERE tipo_retiro IN ("A","B","C")

   DELETE from ret_det_datamart
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_transf_tx
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_transf_rx
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from ret_cza_lote
   WHERE  nom_archivo = "20040902.ABC"

   DELETE from cta_act_marca
   WHERE nss IN ("65644320072","05685005414","51825100541")
  
   DELETE from cta_his_marca
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from cta_ctr_cuenta
   WHERE nss IN ("65644320072","05685005414","51825100541")
 
   LOAD FROM "cta_ctr_cuentaABC.unl"
   INSERT INTO cta_ctr_cuenta

   DELETE from dis_provision
   WHERE nss IN ("65644320072","05685005414","51825100541")

   DELETE from afi_mae_afiliado
   WHERE n_seguro IN ("65644320072","05685005414","51825100541")

   LOAD FROM "afi_mae_afiliadoABC.unl"
   INSERT INTO afi_mae_afiliado

   DELETE from dis_det_aporte
   WHERE n_seguro IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_det_aporteABC.unl"
   INSERT INTO dis_det_aporte

   {
   DELETE from dis_cuenta97
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta97ABC.unl"
   INSERT INTO dis_cuenta97

   DELETE from dis_cuenta98
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta98ABC.unl"
   INSERT INTO dis_cuenta98

   DELETE from dis_cuenta99
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta99ABC.unl"
   INSERT INTO dis_cuenta99

   DELETE from dis_cuenta00
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta00ABC.unl"
   INSERT INTO dis_cuenta00
  
   DELETE from dis_cuenta01
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta01ABC.unl"
   INSERT INTO dis_cuenta01

   DELETE from dis_cuenta02
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta02ABC.unl"
   INSERT INTO dis_cuenta02

   DELETE from dis_cuenta03
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuenta03ABC.unl"
   INSERT INTO dis_cuenta03
   }

   DELETE from dis_cuenta
   WHERE nss IN ("65644320072","05685005414","51825100541")

   LOAD FROM "dis_cuentaABC.unl"
   INSERT INTO dis_cuenta
END FUNCTION

