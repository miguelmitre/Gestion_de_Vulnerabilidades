{=============================================================================}
{  Objetivo: Crear ambiente de prueba de transferencia para elvaluar regs.    }
{          : con diagnostico 302.                                             }
{          : El archivo de solicitudes operacion 02 es "ARCHIVO.05"           }
{=============================================================================}

DATABASE safre_af

MAIN

   DEFINE v_ruta_rescate LIKE seg_modulo.ruta_rescate
   DEFINE v_comando      CHAR(100)
   DEFINE tecla          CHAR(1)
   
   CLEAR SCREEN

   DISPLAY "Se creara ambiente para los NSS: 31634713031"
   DISPLAY "                                 31624314055"
   DISPLAY "                                 23572810028"
   DISPLAY "                                 32874900643"
   DISPLAY "                                 24863701645"

   PROMPT "Continuar (S/N):" FOR CHAR tecla

   IF tecla NOT MATCHES "[sS]" THEN
      DISPLAY "SALIDA"
      EXIT PROGRAM
   END IF

   DELETE from ret_det_datamart
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de ret_det_datamart.......:",sqlca.sqlerrd[3]

   DELETE from cta_act_marca
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de cta_act_marca..........:",sqlca.sqlerrd[3]
  
   DELETE from cta_his_marca
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de cta_his_marca..........:",sqlca.sqlerrd[3]

   DELETE from dis_provision
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de dis_provision..........:",sqlca.sqlerrd[3]

   DELETE from ret_monto_siefore
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de ret_monto_siefore......:",sqlca.sqlerrd[3]

   DELETE from ret_transf_tx
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de ret_transf_tx..........:",sqlca.sqlerrd[3]

   DELETE from ret_transf_rx
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de ret_transf_rx..........:",sqlca.sqlerrd[3]

   DELETE FROM ret_ctr_envio_lote
   WHERE  tipo_retiro IN ("A","B","C")
   AND    folio in ( SELECT folio FROM ret_cza_lote
                     WHERE  nom_archivo IN ("ARCHIVO.05","SOLTRAN302") )

   DISPLAY "Eliminados de ret_ctr_envio_lote.....:",sqlca.sqlerrd[3]

   ------------------------------------------------------------------
{
   DELETE from cta_ctr_cuenta
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de cta_ctr_cuenta.........:",sqlca.sqlerrd[3]
 
   LOAD FROM "cta_ctr_cuentaABC.unl"
   INSERT INTO cta_ctr_cuenta
}
   ------------------------------------------------------------------
{
   DELETE from afi_mae_afiliado
   WHERE n_seguro IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de afi_mae_afiliado.......:",sqlca.sqlerrd[3]

   LOAD FROM "afi_mae_afiliadoABC.unl"
   INSERT INTO afi_mae_afiliado
}
   ------------------------------------------------------------------
   DELETE from dis_det_aporte
   WHERE n_seguro IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de dis_det_aporte.........:",sqlca.sqlerrd[3]

   LOAD FROM "dis_det_aporteABC.unl"
   INSERT INTO dis_det_aporte

   ------------------------------------------------------------------

   DELETE FROM dis_cuenta
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643",
                 "24863701645")
   DISPLAY "Eliminados de dis_cuenta.............:",sqlca.sqlerrd[3]

   LOAD FROM "dis_cuentaABC.unl"
   INSERT INTO dis_cuenta

   LOAD FROM "devoluciones.unl"
   INSERT INTO dis_cuenta

   ------------------------------------------------------------------
{   
   DELETE from tes_solicitud
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de tes_solicitud..........:",sqlca.sqlerrd[3]

   LOAD FROM "tes_solicitudABC.unl"
   INSERT INTO tes_solicitud
}

{
   DELETE from cta_nss_regimen
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de cta_nss_regimen........:",sqlca.sqlerrd[3]

   LOAD FROM "cta_nss_regimenABC.unl"
   INSERT INTO cta_nss_regimen
}

   ------------------------------------------------------------------
	-- MOVIMIENTO DE TRASPASO
   ------------------------------------------------------------------

   DELETE FROM taa_rcv_recepcion
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de taa_rcv_recepcion......:",sqlca.sqlerrd[3]

   LOAD FROM "taa_rcv_recepcionABC.unl"
   INSERT INTO taa_rcv_recepcion



   -- VIVIENDA

   DELETE FROM taa_viv_recepcion
   WHERE nss IN ("31634713031",
                 "31624314055",
                 "23572810028",
                 "32874900643")
   DISPLAY "Eliminados de taa_viv_recepcion......:",sqlca.sqlerrd[3]

   LOAD FROM "taa_viv_recepcionABC.unl"
   INSERT INTO taa_viv_recepcion

   -------------------------------------------------------------------
	--BORRA EL ARCHIVO DE RECEPCION y EL FOLIO
   -------------------------------------------------------------------

   DELETE FROM glo_folio
   WHERE  folio = ( SELECT folio
	                FROM   ret_cza_lote
                        WHERE  nom_archivo IN("ARCHIVO.05","SOLTRAN302") )
   DISPLAY "Eliminados de glo_folio..............:",sqlca.sqlerrd[3]

   DELETE from ret_cza_lote
   WHERE nom_archivo IN ("ARCHIVO.05","SOLTRAN302")
   DISPLAY "Eliminados de ret_cza_lote...........:",sqlca.sqlerrd[3]
   
   -------------------------------------------------------------------
   --COPIA EL ARCHIVO DE LA OPERACION 02 AL DIRECTORIO CORRESPONDIENTE
   -------------------------------------------------------------------
   
   SELECT ruta_rescate
   INTO   v_ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "ret"

   LET v_comando = "cp ARCHIVO.05 ",v_ruta_rescate CLIPPED
   RUN v_comando
   LET v_comando = "cp SOLTRAN302 ",v_ruta_rescate CLIPPED
   RUN v_comando

   LET v_comando = "ls -l ",v_ruta_rescate CLIPPED,"/ARCHIVO.05"
   RUN v_comando
   LET v_comando = "ls -l ",v_ruta_rescate CLIPPED,"/SOLTRAN302"
   RUN v_comando

   DISPLAY "NSS: 32874900643  Retiro A"
   DISPLAY "NSS: 31634713031  Retiro B"
   DISPLAY "NSS: 31624314055  Retiro C"
   DISPLAY "NSS: 23572810028  Retiro A"
   DISPLAY "NSS: 24863701645  Retiro A"

END MAIN
