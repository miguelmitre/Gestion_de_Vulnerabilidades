GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroF()
#trF-------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  nss = "07038301060"

    LOAD FROM "dis_cuentaF.unl" INSERT INTO dis_cuenta

    DELETE 
    FROM   afi_mae_siefore
    WHERE  n_seguro = "07038301060";

    LOAD FROM "afi_mae_sieforeF.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "07038301060";

    LOAD FROM "afi_mae_afiliadoF.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   cta_act_marca
    WHERE  nss = "07038301060";

    DELETE
    FROM   cta_his_marca
    WHERE  nss = "07038301060";

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss = "07038301060";

    LOAD FROM "cta_ctr_cuentaF.unl" INSERT INTO cta_ctr_cuenta;
   # LOAD FROM "cta_his_marcaF.unl" INSERT INTO cta_his_marca
   # LOAD FROM "cta_act_marcaF.unl" INSERT INTO cta_act_marca

{jj DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo = 21;

    LOAD FROM "ret_consecutivoF.unl" INSERT INTO ret_consecutivo
jj}
    DELETE
    FROM   ret_solicitud_rx
    WHERE  nss = "07038301060";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-F";

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        = "07038301060"
    AND    tipo_retiro = "F";

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "F"
    AND    estado      = 2;

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "F"
    AND    fecha_genera = TODAY;

    DELETE
    FROM   dis_provision
    WHERE  nss = "07038301060";

END FUNCTION


