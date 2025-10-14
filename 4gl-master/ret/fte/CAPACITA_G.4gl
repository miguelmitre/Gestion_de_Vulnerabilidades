GLOBALS "CAPACITA2.4gl"

FUNCTION tipo_retiroG()
#trG-------------------
    DELETE
    FROM  ret_solicitud_rx
    WHERE nss = "20037600119";

    DELETE
    FROM  ret_solicitud_tx
    WHERE nss = "20037600119";

    DELETE
    FROM  dis_cuenta
    WHERE nss = "20037600119";

    DELETE
    FROM  dis_provision
    WHERE nss = "20037600119";

    DELETE
    FROM  cta_act_marca
    WHERE nss = "20037600119";

    DELETE
    FROM  cta_his_marca
    WHERE nss = "20037600119";

    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro = "20037600119";

    DELETE
    FROM  afi_domicilio
    WHERE nss = "20037600119";

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo IN("OP06-G");

    DELETE
    FROM  cta_ctr_cuenta
    WHERE nss = "20037600119";

    DELETE
    FROM  ret_ctr_envio_lote
    WHERE tipo_retiro = "G"
    AND   fecha_envio = TODAY;
{
    SELECT "OK"
    FROM   ret_consecutivo
    WHERE  consecutivo = 520275

    IF STATUS = NOTFOUND THEN
        INSERT INTO ret_consecutivo
        VALUES (520275);
    END IF
}
    LOAD FROM "dis_cuentaG.unl"
    INSERT INTO dis_cuenta;

--    LOAD FROM "ret_solicitud_txG.unl"
--    INSERT INTO ret_solicitud_tx;

    LOAD FROM "afi_mae_afiliadoG.unl"
    INSERT INTO afi_mae_afiliado;

--    LOAD FROM "afi_domicilioG.unl"
--    INSERT INTO afi_domicilio;

    LOAD FROM "cta_ctr_cuentaG.unl"
    INSERT INTO cta_ctr_cuenta;

{
    LOAD FROM "cta_act_marcaG.unl"
    INSERT INTO cta_act_marca;

    LOAD FROM "cta_his_marcaG.unl"
    INSERT INTO cta_his_marca;

    UPDATE cta_act_marca
    SET    fecha_ini = TODAY
    WHERE  nss = "20037600119";

    UPDATE cta_his_marca
    SET    fecha_ini = TODAY
    WHERE  nss = "20037600119";
}
END FUNCTION
