GLOBALS "CAPACITA2.4gl"
{
   -- FUNCIONES INCLUIDAS

   FUNCTION ret_iv_rt()
   FUNCTION ret_total()
   FUNCTIO aclaraciones()
   FUNCTIO voluntarias()
   FUNCTIO ini_global()
}

FUNCTION ret_iv_rt()
#rir----------------
    DEFINE #loc #integer
        s_folio               SMALLINT

    DECLARE cur_1 CURSOR FOR
    SELECT folio
    FROM   ret_cza_pe03

    FOREACH cur_1 INTO s_folio
        DELETE
        FROM  ret_cza_lote
        WHERE folio = s_folio

        DELETE
        FROM  ret_seguimiento
        WHERE folio = s_folio
    END FOREACH

    DELETE
    FROM  ret_consecutivo
    WHERE consecutivo > 28
   
    INSERT INTO  ret_consecutivo
    VALUES (29)

    DELETE
    FROM  ret_cza_pe03;

    DELETE
    FROM  ret_his_det_pe03;

    DELETE
    FROM  ret_sum_pe03;

    DELETE
    FROM  ret_cza_pe05;

    DELETE
    FROM  ret_his_det_pe05;

    DELETE
    FROM  ret_sum_pe05;

    DELETE
    FROM  cta_act_marca
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");
    DELETE
    FROM  cta_his_marca
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    LOAD FROM "ctr2.unl" INSERT INTO cta_ctr_cuenta

    DELETE 
    FROM dis_cuenta
    WHERE  nss in ("32755814657",
                   "01765500770",
                   "01755565817",
                   "32786227366",
                   "24773901582");

    LOAD FROM "dis_cuenta_ivrt.unl" INSERT INTO  dis_cuenta

END FUNCTION

FUNCTION ret_total()
#rt-----------------
    DEFINE #loc #smallint
        i                     SMALLINT

    DELETE
    FROM   ret_consecutivo
    WHERE  consecutivo > 6

    FOR i = 1 TO 6
        INSERT INTO ret_consecutivo VALUES(i)
    END FOR

    DELETE
    FROM   glo_folio
    WHERE  folio > 6

    DELETE
    FROM   ret_resol_retiro
    WHERE  folio > 6

    DELETE
    FROM   ret_resol_retiro
    WHERE  n_seguro IN("72703311570","03633704626")

    LOAD FROM "resol_total_nagativa.unl" INSERT INTO ret_resol_retiro

    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in("72703311570","03633704626")

    LOAD FROM "ctr_cta_total_negativa.unl" INSERT INTO cta_ctr_cuenta

    DELETE
    FROM   dis_cuenta
    WHERE  nss in("72703311570","03633704626")
    AND    subcuenta = 4

    LOAD FROM "cuenta_total_negativa.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   ret_seguimiento
    WHERE  folio > 6

    DELETE
    FROM   ret_ctr_envio
    WHERE  fecha_envio = HOY

    SELECT folio
    INTO   i_folio
    FROM   ret_cza_lote
    WHERE  nom_archivo = "RET001221.13NEG"

    DELETE 
    FROM   ret_cza_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_his_det_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_sum_pe13
    WHERE  folio = i_folio

    DELETE
    FROM   ret_cza_lote
    WHERE  nom_archivo = "RET001221.13NEG"

    DELETE
    FROM   ret_his_det_pe12
END FUNCTION


FUNCTION aclaraciones()
#a---------------------
         DELETE
	 FROM   ret_cza43

	 DELETE
	 FROM   ret_his_det_trab43

	 DELETE
	 FROM   ret_his_det_mov43

	 DELETE
	 FROM   ret_sum43

	 DELETE
	 FROM   ret_acl_icefa

	 DELETE
	 FROM   ret_acl_retiro

         DELETE
	 FROM   ret_resol_retiro
         WHERE  n_seguro IN("01633918675","24552901050","39885005460",
	    	            "45897059148","49742300616")

    LOAD FROM "resol_retiros_aclara.unl" INSERT INTO ret_resol_retiro


    DELETE
    FROM   ret_his_det_pe29
    WHERE  n_seguro IN("01633918675","24552901050","39885005460",
	   	       "45897059148","49742300616")

    LOAD FROM "his_det_pe_29_aclara.unl" INSERT INTO ret_his_det_pe29


    DELETE
    FROM   tra_det_trasp_sal

    LOAD FROM "det_trasp_sal_aclara.unl" INSERT INTO tra_det_trasp_sal

    DELETE
    FROM   tra_det_trasp_int

    LOAD FROM "det_trasp_int_aclara.unl" INSERT INTO tra_det_trasp_int

    DELETE
    FROM   dis_cuenta
    WHERE  nss IN("01633918675","24552901050","39885005460",
	  	  "45897059148","49742300616")

    LOAD FROM "cuenta_aclara.unl" INSERT INTO dis_cuenta;
END FUNCTION


FUNCTION voluntarias()
#v--------------------
    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro = "01623934153"

    LOAD FROM "maeafili_voluntaria.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM  dis_cuenta
    WHERE nss       = "01623934153"
    AND   subcuenta = 3
   
    LOAD FROM "cuentas_voluntarias.unl" INSERT INTO dis_cuenta

    DELETE
    FROM   ret_cta_vol
    WHERE  n_seguro = "01623934153"

    DELETE
    FROM   tab_retiro

    LOAD FROM "tab_retiro.unl" INSERT INTO tab_retiro

    DELETE
    FROM   tab_pago

    LOAD FROM "tab_pago.unl" INSERT INTO tab_pago

    DELETE
    FROM   tab_identificacion

    LOAD FROM "tab_identificacion.unl" INSERT INTO tab_identificacion
END FUNCTION


FUNCTION ini_global()
#ig------------------
    DELETE
    FROM  ret_cza_lote

    DELETE
    FROM  ret_seguimiento

    DELETE
    FROM  ret_cza_pe03

    DELETE
    FROM  ret_his_det_pe03

    DELETE
    FROM  ret_sum_pe03

    DELETE
    FROM  ret_cza_pe05;

    DELETE
    FROM  ret_his_det_pe05;

    DELETE
    FROM  ret_sum_pe05;

    UPDATE cta_ctr_cuenta
    SET    estado_cuenta  = 1,
           estado_proceso = 0
    WHERE  nss in("01554101053","01564009866","01573653472");
 
    DELETE
    FROM   ret_cza_pe17

    DELETE
    FROM   ret_his_det_pe17

    DELETE
    FROM   ret_sum_pe17

    DELETE
    FROM   ret_cza_pe25

    DELETE
    FROM   ret_his_det_pe24

    DELETE
    FROM   ret_RETC008

    DELETE
    FROM   ret_his_det_pe25

    DELETE
    FROM   ret_sum_pe25

    DELETE
    FROM   ret_cza_pe28

    DELETE
    FROM   ret_his_det_pe28

    DELETE
    FROM   ret_his_det_pe29

    DELETE
    FROM   ret_sum_pe28

    DELETE
    FROM   ret_sum_envio

    DELETE
    FROM  ret_cza_pe36

    DELETE
    FROM  ret_his_det_pe36

    DELETE
    FROM  ret_sum_pe36

    DELETE
    FROM  ret_cza_pe39

    DELETE
    FROM  ret_his_det_pe39

    DELETE
    FROM  ret_his_det_pe40

    DELETE
    FROM  ret_sum_pe39

    DELETE
    FROM   ret_ctr_envio

    DELETE
    FROM   ret_resol_retiro

    DELETE
    FROM  glo_folio  

    DELETE
    FROM  ret_consecutivo

    DELETE
    FROM  afi_mae_afiliado
    WHERE n_seguro IN("04695322851","54917801448","54843810166")

    LOAD FROM "maeafili_ivrt.unl" INSERT INTO afi_mae_afiliado

    DELETE
    FROM  dis_cuenta 
    WHERE nss IN("04695322851","54917801448","54843810166")
    AND   tipo_movimiento BETWEEN 401 AND 499

    DELETE
    FROM   dis_cuenta 
    WHERE  nss in("01554101053","01564009866","01573653472")

    LOAD FROM "cuenta_sar.unl" INSERT INTO dis_cuenta


    DELETE
    FROM   cta_ctr_cuenta
    WHERE  nss in ("04695322851","54843810166","54917801448")

    LOAD FROM "ctr_cuenta_ivrt.unl" INSERT INTO cta_ctr_cuenta

    DELETE 
    FROM   cta_ctr_cuenta
    WHERE  nss in("01554101053","01564009866","01573653472");

    LOAD FROM "ctr_cuenta_sar92.unl" INSERT INTO cta_ctr_cuenta;

    DELETE
    FROM   ret_status

    LOAD FROM "ret_status.unl" INSERT INTO ret_status
END FUNCTION


