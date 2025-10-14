GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroD()
#trD-------------------
    DELETE
    FROM   dis_cuenta 
    WHERE  nss = "14038201720"

    LOAD FROM "dis_cuentaD.unl" INSERT INTO dis_cuenta

    ---DELETE
    ---FROM   afi_mae_siefore
    ---WHERE  n_seguro = "14038201720";

    ---LOAD FROM "afi_mae_siefore.invD.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "14038201720"; 

    LOAD FROM "afi_mae_afiliadoD.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM   afi_domicilio
    WHERE  nss = "14038201720"

    DELETE
    FROM   cta_act_marca
    WHERE  nss = "14038201720";

    DELETE 
    FROM   cta_his_marca
    WHERE  nss = "14038201720";

    DELETE  
    FROM   cta_ctr_cuenta
    WHERE  nss = "14038201720";

    LOAD FROM "cta_ctr_cuentaD.unl" INSERT INTO cta_ctr_cuenta;

    DELETE 
    FROM   ret_solicitud_rx
    WHERE  nss = "14038201720";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-D"

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        IN("14038201720")
    AND    tipo_retiro = "D";

    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-D"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "D"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "D"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision 
    WHERE  nss = "14038201720"
    { 
    LET cp = "cp OP06-D ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-D"
    RUN ch
    }
END FUNCTION
