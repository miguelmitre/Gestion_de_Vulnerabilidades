GLOBALS "CAPACITA2.4gl"

---------------------------------------
--
-- Responsable Isai Jimenez Rojas
-- Ultima Modificacion: 13/Oct/2004
--
---------------------------------------

FUNCTION ret_parcial()   -- IJR
#rp-------------------

    OPTIONS MESSAGE LINE LAST

    MESSAGE "PROCESANDO..." ATTRIBUTE(REVERSE)

    -- OK -------------------------------------------------------

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "OP13-I"

    DELETE FROM afi_mae_afiliado
     WHERE n_seguro IN ("20038202071","39967733468")

    LOAD FROM "afi_mae_afiliadoI.unl" INSERT INTO afi_mae_afiliado

    DELETE FROM ret_parcial_tx
     WHERE nss IN ("20038202071","39967733468")

    DELETE FROM ret_beneficiario 
     WHERE nss IN ("20038202071","39967733468")

    DELETE FROM ret_parcial 
     WHERE nss IN ("20038202071","39967733468")

    DELETE FROM dis_cuenta
     WHERE nss IN ("20038202071","39967733468")
       AND tipo_movimiento in (870,875)
	
    LOAD FROM "dis_cuentaI.unl" INSERT INTO dis_cuenta


    -- VALOR DE LA ACCION 

    SELECT "X"
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = TODAY
    AND    codigo_siefore = 1

    IF SQLCA.SQLCODE = NOTFOUND THEN
       LOAD FROM "glo_valor_accionI.unl" INSERT INTO glo_valor_accion;

       UPDATE glo_valor_accion
       SET    fecha_valuacion = TODAY
       WHERE  fecha_valuacion = "01/01/0001"
       AND    codigo_siefore=1;
    END IF

    DELETE FROM ret_his_det_av13
     WHERE n_seguro IN ("20038202071","39967733468")

    DELETE FROM cta_act_marca
     WHERE nss IN ("20038202071","39967733468")

    DELETE FROM cta_his_marca
     WHERE nss IN ("20038202071","39967733468")

    DELETE
    FROM   cta_ctr_cuenta
    WHERE nss IN ("20038202071","39967733468")

    LOAD FROM "cta_ctr_cuentaI.unl" INSERT INTO cta_ctr_cuenta

    -------------------------------------------------------------

    --DELETE
    --FROM  glo_folio

    --FOR i = 1 TO 10
        --INSERT INTO glo_folio VALUES(i)
    --END FOR

    DELETE
    FROM   ret_ctr_envio
    WHERE  fecha_envio = HOY

    DELETE
    FROM   ret_sum_envio
  --WHERE  fecha_envio = HOY
    WHERE  tipo_operacion IN ("AV12","AV16")

    --DELETE
    --FROM   ret_seguimiento
    --WHERE  folio > 10

    --DELETE
    --FROM   ret_resol_retiro
    --WHERE  n_seguro IN("21897354664","07906918797","33876770323")

    --LOAD FROM "resol_parcial.unl" INSERT INTO ret_resol_retiro

    --DELETE
    --FROM   ret_cza_av13

    --DELETE
    --FROM   ret_sum_av13

END FUNCTION
