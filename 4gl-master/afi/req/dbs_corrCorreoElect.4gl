################################################################################
# Objetivo     => Actualiza bandera de envío de la tabla de correos ya que existe
#                 duplicidad
# Modulo       => AFI
# Proceso      => Integracion de la informacion proveniente de APP Movil
# Jira         => CPL-3532
################################################################################

DATABASE safre_af

MAIN
  CALL fn_actualiza_bandera_envio_correoElect()
END MAIN

FUNCTION fn_actualiza_bandera_envio_correoElect()
  DEFINE v_num_reg_act    INTEGER
  DEFINE v_qry_txt        VARCHAR(250)
  DEFINE v_nombre_resp    VARCHAR(100)
  DEFINE v_nss            CHAR(11)
  DEFINE v_n_folio        DECIMAL(10,0)
  DEFINE v_tipo_solicitud SMALLINT
  DEFINE v_rowid          INTEGER
  DEFINE v_bandExiste     SMALLINT

  # se inicializan variables
  LET v_num_reg_act = 0
  LET v_bandExiste = 0

  DISPLAY "\nCORRECCION DE LA MARCA DE ENVIO PARA CORREO ELECTRONICO"
  SLEEP 2

  DISPLAY "\n== Respaldando informacion de correo electronico..."
  SLEEP 2

  # se respalda informacion de correo electronico
  LET v_nombre_resp = "respaldo_afiCorreoElec_" || (TODAY USING "YYYYMMDD") || ".unl"
  UNLOAD TO v_nombre_resp
  SELECT * FROM afi_correo_elect;

  DISPLAY "   Se ha generado respaldo de la informacion de correo electronico"
  DISPLAY "   ",v_nombre_resp

  DISPLAY "\n== Actualizando informacion de correo electronico..."
  SLEEP 2

  # se obtienen los NSS que tienen doble marca de envío
  DECLARE cur_marca_env CURSOR FOR
  SELECT nss, n_folio, tipo_solicitud, COUNT(*)
    FROM afi_correo_elect
   WHERE marca_envio = "X"
   GROUP BY 1, 2, 3
  HAVING COUNT(*) > 1

  FOREACH cur_marca_env INTO v_nss, v_n_folio, v_tipo_solicitud
     LET v_bandExiste = 1

     # si no se obtiene folio continua con el siguiente registro
     IF v_n_folio IS NULL THEN
        CONTINUE FOREACH
     END IF

     # se valida si el nss es nulo, para este caso se realiza solo por folio y tipo solicitud
     IF v_nss IS NULL OR v_nss = '' OR v_nss = ' ' THEN
        # se obtiene el último registro insertado con marca de envío para el nss en proceso
        LET v_qry_txt = " SELECT FIRST 1 rowid",
                        "   FROM afi_correo_elect",
                        "  WHERE n_folio = ", v_n_folio,
                        "    AND tipo_solicitud = ", v_tipo_solicitud,
                        "    AND marca_envio = 'X'",
                        "  ORDER BY factualiza DESC"
     ELSE
        # se obtiene el último registro insertado con marca de envío para el nss en proceso
        LET v_qry_txt = " SELECT FIRST 1 rowid",
                        "   FROM afi_correo_elect",
                        "  WHERE nss = '",v_nss CLIPPED,"'",
                        "    AND n_folio = ", v_n_folio,
                        "    AND tipo_solicitud = ", v_tipo_solicitud,
                        "    AND marca_envio = 'X'",
                        "  ORDER BY factualiza DESC"
     END IF

     PREPARE prp_obtn_rowId FROM v_qry_txt
     EXECUTE prp_obtn_rowId INTO v_rowid

     # se deshabilita bandera de envío para los demás registros
     UPDATE afi_correo_elect
       SET marca_envio = NULL
      WHERE nss = v_nss
        AND n_folio = v_n_folio
        AND tipo_solicitud = v_tipo_solicitud
        AND marca_envio = "X"
        AND rowid <> v_rowid;

     LET v_num_reg_act = v_num_reg_act + SQLCA.SQLERRD[3]

     DISPLAY "   NSS actualizado: ", v_nss, " - ", v_num_reg_act
  END FOREACH

  IF v_bandExiste THEN
    DISPLAY "\n   Numero de Reg. actualizados: ",v_num_reg_act
    SLEEP 1
  ELSE
    DISPLAY "   No se encontro informacion inconsistente en tabla de correo electronico"
    SLEEP 1
  END IF

  DISPLAY "\nFIN DE PROCESO"
END FUNCTION
