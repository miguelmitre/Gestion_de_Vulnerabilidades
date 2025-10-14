GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroE()
#trE-------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  nss             = "48017717546"
   
    LOAD FROM "dis_cuentaE.unl" INSERT INTO dis_cuenta

    DELETE 
    FROM   afi_mae_siefore
    WHERE  n_seguro = "48017717546"

    LOAD FROM "afi_mae_sieforeE.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "48017717546"

    LOAD FROM "afi_mae_afiliadoE.unl" INSERT INTO afi_mae_afiliado
  
    DELETE
    FROM   cta_act_marca
    WHERE  nss = "48017717546"

    DELETE
    FROM   cta_his_marca
    WHERE  nss = "48017717546"
 
    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss = "48017717546"
    
    LOAD FROM "cta_ctr_cuentaE.unl" INSERT INTO cta_ctr_cuenta
   # LOAD FROM "cta_his_marcaE.unl" INSERT INTO cta_his_marca
   # LOAD FROM "cta_act_marcaE.unl" INSERT INTO cta_act_marca

{jj DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo = 19

    LOAD FROM "ret_consecutivoE.unl" INSERT INTO ret_consecutivo
jj}
    DELETE
    FROM   ret_solicitud_rx
    WHERE  nss = "48017717546"

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-E"

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        = "48017717546"
    AND    tipo_retiro = "E"

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "E"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "E"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision
    WHERE  nss = "48017717546"

END FUNCTION

