GLOBALS "CAPACITA2.4gl"
{
DATABASE safre_af 
globals
end globals
main
    call tipo_retiroH()
end main
}

FUNCTION tipo_retiroH()
#trH-------------------
    DELETE
    FROM   dis_cuenta 
    WHERE  nss = "07038301060"

    LOAD FROM "dis_cuentaH.unl" INSERT INTO dis_cuenta

    ---DELETE
    ---FROM   afi_mae_siefore
    ---WHERE  n_seguro = "07038301060";

    ---LOAD FROM "afi_mae_siefore.invH.unl" INSERT INTO afi_mae_siefore

    DELETE
    FROM   afi_mae_afiliado
    WHERE  n_seguro = "07038301060"; 

    LOAD FROM "afi_mae_afiliadoH.unl" INSERT INTO afi_mae_afiliado

    ---DELETE
    ---FROM   afi_domicilio
    ---WHERE  nss = "07038301060"

    DELETE
    FROM   cta_act_marca
    WHERE  nss = "07038301060";

    DELETE 
    FROM   cta_his_marca
    WHERE  nss = "07038301060";

    DELETE  
    FROM   cta_ctr_cuenta
    WHERE  nss = "07038301060";

    LOAD FROM "cta_ctr_cuentaH.unl" INSERT INTO cta_ctr_cuenta;

    ---DELETE
    ---FROM   ret_consecutivo
    ---WHERE  consecutivo = 20;

    ---LOAD FROM "ret_consecutivo.invH.unl" INSERT INTO ret_consecutivo 

    DELETE 
    FROM   ret_solicitud_rx
    WHERE  nss = "07038301060";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP06-H"

    DELETE
    FROM   ret_solicitud_tx
    WHERE  nss        IN("07038301060")
    AND    tipo_retiro = "H";

    DELETE
    FROM   ret_solicitud_rx
    WHERE  folio IN(SELECT folio
                    FROM   ret_cza_lote
                    WHERE  nom_archivo = "OP06-H"
                   )

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro = "H"
    AND    estado      = 2

    DELETE
    FROM   ret_ctr_envio_lote
    WHERE  tipo_retiro  = "H"
    AND    fecha_genera = TODAY

    DELETE
    FROM   dis_provision 
    WHERE  nss = "07038301060"

    {
    LET cp = "cp OP06-H ",g_param_ret.ruta_rescate
    RUN cp

    LET ch = "chmod 777 ",g_param_ret.ruta_rescate CLIPPED,"/OP06-H"
    RUN ch
    }
END FUNCTION
