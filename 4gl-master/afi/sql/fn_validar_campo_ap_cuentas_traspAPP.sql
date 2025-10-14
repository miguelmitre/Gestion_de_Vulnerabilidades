DROP FUNCTION fn_validar_campo_ap_cuentas_traspAPP;
CREATE FUNCTION fn_validar_campo_ap_cuentas_traspAPP(
   p_fechaTramiteApp               CHAR(10),
   p_aplicacionOrigen              CHAR(2),
   p_claveAdminTransferente        CHAR(3),
   p_claveAdminReceptora           CHAR(3),
   p_fechaFirmaSolicitud           CHAR(10),
   p_folioSolicitudTraspaso        CHAR(10),
   p_fechaProbableLiquiTrasp       CHAR(10),
   p_tipoNotificacion              SMALLINT,
   p_motivoCancelacion             SMALLINT,
   p_nssTrabajador                 CHAR(11),
   p_curpTrabajador                CHAR(18),
   p_apellidoPaterno               CHAR(40),
   p_apellidoMaterno               CHAR(40),
   p_nombre                        CHAR(40),
   p_fechaNacTrabajador            CHAR(10),
   p_generoTrabajador              CHAR(1),
   p_entidadFederativaNac          CHAR(2),
   p_nacionalidad                  CHAR(3),
   p_rfcTrabajador                 CHAR(13),
   p_origenTipoTraspaso            SMALLINT,
   p_fechaPrimerRegistro           CHAR(10),
   p_fechaAltaAforeActual          CHAR(10),
   p_fechaAltaUltimoTraspComercial CHAR(10),
   p_indicadorPortabilidad         SMALLINT,
   p_ocupacionProfesionTrab        CHAR(2),
   p_actividadGiroNegocioTrab      CHAR(2),
   p_nivelEstudioTrab              CHAR(2),
   p_idProcesoExpe                 SMALLINT,
   p_calle                         CHAR(65),
   p_numeroExterior                CHAR(15),
   p_numeroInterior                CHAR(15),
   p_colonia                       CHAR(65),
   p_ciudadPoblacion               CHAR(65),
   p_delegacion                    CHAR(65),
   p_codigoPostal                  CHAR(5),
   p_entidadFederativaDom          CHAR(2),
   p_pais                          CHAR(3),
   p_telefonoMovil                 DECIMAL(10,0),
   p_telefonoFijo                  DECIMAL(10,0),
   p_extension                     CHAR(5),
   p_correoElecTrabajador          CHAR(50),
   p_fechaCertificacion            CHAR(15),
   p_tiposolicitud                 CHAR(2),
   p_nombreBeneficiario1           CHAR(40),
   p_apPaternoBeneficiario1	       CHAR(40),
   p_apMaternoBeneficiario1	       CHAR(40),
   p_curpBeneficiario1             CHAR(18),
   p_idCatParentesco1              CHAR(2),
   p_porcentajeBeneficiario1	     DECIMAL(5,2),
   p_nombreBeneficiario2           CHAR(40),
   p_apPaternoBeneficiario2	       CHAR(40),
   p_apMaternoBeneficiario2	       CHAR(40),
   p_curpBeneficiario2             CHAR(18),
   p_idCatParentesco2              CHAR(2),
   p_porcentajeBeneficiario2	     DECIMAL(5,2),
   p_nombreBeneficiario3           CHAR(40),
   p_apPaternoBeneficiario3	       CHAR(40),
   p_apMaternoBeneficiario3	       CHAR(40),
   p_curpBeneficiario3             CHAR(18),
   p_idCatParentesco3              CHAR(2),
   p_porcentajeBeneficiario3	     DECIMAL(5,2),
   p_nombreBeneficiario4           CHAR(40),
   p_apPaternoBeneficiario4	       CHAR(40),
   p_apMaternoBeneficiario4	       CHAR(40),
   p_curpBeneficiario4             CHAR(18),
   p_idCatParentesco4              CHAR(2),
   p_porcentajeBeneficiario4	     DECIMAL(5,2),
   p_nombreBeneficiario5           CHAR(40),
   p_apPaternoBeneficiario5	       CHAR(40),
   p_apMaternoBeneficiario5	       CHAR(40),
   p_curpBeneficiario5             CHAR(18),
   p_idCatParentesco5              CHAR(2),
   p_porcentajeBeneficiario5	     DECIMAL(5,2))

   RETURNING CHAR(2), SMALLINT, VARCHAR(100);

   -- registro de retorno
   DEFINE v_respuesta_codRespuesta   CHAR(2);  -- 01 Registro válido  02 Registro con error
   DEFINE v_respuesta_codDiagnostico SMALLINT;
   DEFINE v_respuesta_descripcion    VARCHAR(100);
   DEFINE v_codigo_afore             SMALLINT;
   DEFINE v_existe_reg               SMALLINT;
   DEFINE v_deleg_cod                INTEGER;
   -- variables para la marca
   DEFINE v_mc_nss                      CHAR(11);
   DEFINE v_mc_marca_entra              SMALLINT;
   DEFINE v_mc_correlativo              INTEGER;
   DEFINE v_mc_estado_marca             SMALLINT;
   DEFINE v_mc_codigo_rechazo           SMALLINT;
   DEFINE v_mc_marca_causa              SMALLINT;
   DEFINE v_mc_fecha_causa              DATE;
   DEFINE v_mc_usuario                  CHAR(08);
   DEFINE v_ret_marca_entra             SMALLINT;
   DEFINE v_ret_cod_rechazo             SMALLINT;
   
   -- se asigna el resultado de la validacion
   LET v_respuesta_codRespuesta = '01';
   LET v_respuesta_codDiagnostico = 0;
   LET v_respuesta_descripcion = 'Validación de campo Aceptado';

   -- se valida campo fechaTramiteApp
   IF p_fechaTramiteApp IS NULL OR p_fechaTramiteApp = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 1;
      LET v_respuesta_descripcion = 'Campo fechaTramiteApp nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaTramiteApp = MDY(p_fechaTramiteApp[6,7], p_fechaTramiteApp[9,10], p_fechaTramiteApp[1,4]);

      -- se valida fechaTramiteApp. No puede ser mayor a TODAY
      IF p_fechaTramiteApp > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 1;
         LET v_respuesta_descripcion = 'Campo fechaTramiteApp es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo aplicacionOrigen
   IF p_aplicacionOrigen IS NULL OR p_aplicacionOrigen = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 38;
      LET v_respuesta_descripcion = 'Campo aplicacionOrigen nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor de campo aplicacionOrigen. Debe ser 01 o 02
      IF p_aplicacionOrigen <> '01' AND p_aplicacionOrigen <> '02' AND p_aplicacionOrigen <> '07' THEN
         -- se cambia est	atus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 38;
         LET v_respuesta_descripcion = 'El valor del campo aplicacionOrigen es inválido';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo claveAdminTransferente
   IF p_claveAdminTransferente IS NULL OR p_claveAdminTransferente = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 39;
      LET v_respuesta_descripcion = 'Campo claveAdminTransferente nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se consulta el código de AFORE
      SELECT afore_cod
        INTO v_codigo_afore
        FROM tab_afore
       WHERE afore_cod = p_claveAdminTransferente;

      -- se valida si se encontró regsitro en catálogo de la claveAdminTransferente
      IF v_codigo_afore IS NULL THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 39;
         LET v_respuesta_descripcion = 'El valor del campo claveAdminTransferente no existe en catálogo';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo claveAdminReceptora
   IF p_claveAdminReceptora IS NULL OR p_claveAdminReceptora = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 2;
      LET v_respuesta_descripcion = 'Campo claveAdminReceptora nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se consulta el código de AFORE
      SELECT codigo_afore
        INTO v_codigo_afore
        FROM tab_afore_local;

      -- se valida valor de campo claveAdminReceptora debe ser igual a codigo_afore de la tabla tab_afore_local
      IF p_claveAdminReceptora <> v_codigo_afore THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 2;
         LET v_respuesta_descripcion = 'El valor del campo claveAdminReceptora no es válido';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo claveAdminReceptora y claveAdminTransferente
   IF p_claveAdminTransferente = p_claveAdminReceptora THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 39;
      LET v_respuesta_descripcion = 'El valor del campo p_claveAdminTransferente no puede ser igual a claveAdminReceptora';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo fechaFirmaSolicitud
   IF p_fechaFirmaSolicitud IS NULL OR p_fechaFirmaSolicitud = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 3;
      LET v_respuesta_descripcion = 'Campo fechaFirmaSolicitud nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaFirmaSolicitud = MDY(p_fechaFirmaSolicitud[6,7], p_fechaFirmaSolicitud[9,10], p_fechaFirmaSolicitud[1,4]);

      -- se valida fechaFirmaSolicitud. No puede ser mayor a TODAY
      IF p_fechaFirmaSolicitud > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 3;
         LET v_respuesta_descripcion = 'Campo fechaFirmaSolicitud es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo folioSolicitudTraspaso
   IF p_folioSolicitudTraspaso IS NULL OR p_folioSolicitudTraspaso = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 4;
      LET v_respuesta_descripcion = 'Campo folioSolicitudTraspaso nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida la existencia de folioSolicitudTraspaso en tabla con estatus ACEPTADO (01)
      SELECT COUNT(*)
        INTO v_existe_reg
        FROM afi_sol_traspaso_app
       WHERE folio_sol_traspaso = p_folioSolicitudTraspaso
         AND resultado_operacion = '01';

      -- se valida si ya existe el folioSolicitudTraspaso
      IF v_existe_reg >= 1 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 37;
         LET v_respuesta_descripcion = 'El folioSolicitudTraspaso ya existe';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   ---- se valida campo fechaProbableLiquiTrasp
   --IF p_fechaProbableLiquiTrasp IS NULL OR p_fechaProbableLiquiTrasp = " " THEN
   --   -- se cambia estatus de registro a Registro con error (02)
   --   LET v_respuesta_codRespuesta = '02';
   --   LET v_respuesta_codDiagnostico = 5;
   --   LET v_respuesta_descripcion = 'Campo fechaProbableLiquiTrasp nulo';
   --
   --   RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   --ELSE
   --   -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
   --   LET p_fechaProbableLiquiTrasp = MDY(p_fechaProbableLiquiTrasp[6,7], p_fechaProbableLiquiTrasp[9,10], p_fechaProbableLiquiTrasp[1,4]);
   --
   --   -- se valida fechaProbableLiquiTrasp. No puede ser mayor a TODAY
   --   IF p_fechaProbableLiquiTrasp > TODAY THEN
   --      -- se cambia estatus de registro a Registro con error (02)
   --      LET v_respuesta_codRespuesta = '02';
   --      LET v_respuesta_codDiagnostico = 5;
   --      LET v_respuesta_descripcion = 'Campo fechaProbableLiquiTrasp es mayor a la fecha actual';
   --
   --      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   --   END IF
   --END IF

   -- se valida campo fechaProbableLiquiTrasp
   IF p_fechaProbableLiquiTrasp IS NULL OR p_fechaProbableLiquiTrasp = " " THEN
      LET p_fechaProbableLiquiTrasp = NULL;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaProbableLiquiTrasp = MDY(p_fechaProbableLiquiTrasp[6,7], p_fechaProbableLiquiTrasp[9,10], p_fechaProbableLiquiTrasp[1,4]);
   END IF

   -- se valida campo tipoNotificacion
   IF p_tipoNotificacion IS NULL OR p_tipoNotificacion = 0 THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 40;
      LET v_respuesta_descripcion = 'Campo tipoNotificacion nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor del campo tipoNotificacion. Debe ser igual a 1
      IF p_tipoNotificacion <> 1 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 40;
         LET v_respuesta_descripcion = 'El valor del campo tipoNotificacion es inválido';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo curpTrabajador
   IF p_curpTrabajador IS NULL OR p_curpTrabajador = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 7;
      LET v_respuesta_descripcion = 'Campo curpTrabajador nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo nssTrabajador y tiposolicitud
   IF p_nssTrabajador IS NULL OR p_nssTrabajador = " " THEN
      IF p_tiposolicitud <> '28' AND p_tiposolicitud <> '43' THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 16;
         LET v_respuesta_descripcion = 'Campo nssTrabajador es nulo y tiposolicitud diferente de (28, 43)';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   ELSE
      IF p_tiposolicitud <> '29' AND p_tiposolicitud <> '42' THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 16;
         LET v_respuesta_descripcion = 'Campo nssTrabajador no es nulo y tiposolicitud diferente de (29, 42)';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo apellidoPaterno
   IF p_apellidoPaterno IS NULL OR p_apellidoPaterno = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 8;
      LET v_respuesta_descripcion = 'Campo apellidoPaterno nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo nombre
   IF p_nombre IS NULL OR p_nombre = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 10;
      LET v_respuesta_descripcion = 'Campo nombre nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo fechaNacTrabajador
   IF p_fechaNacTrabajador IS NULL OR p_fechaNacTrabajador = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 11;
      LET v_respuesta_descripcion = 'Campo fechaNacTrabajador nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaNacTrabajador = MDY(p_fechaNacTrabajador[6,7], p_fechaNacTrabajador[9,10], p_fechaNacTrabajador[1,4]);

      -- se valida fechaNacTrabajador. No puede ser mayor a TODAY
      IF p_fechaNacTrabajador > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 11;
         LET v_respuesta_descripcion = 'Campo fechaNacTrabajador es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo entidadFederativaNac
   IF p_entidadFederativaNac IS NULL OR p_entidadFederativaNac = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 17;
      LET v_respuesta_descripcion = 'Campo entidadFederativaNac nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida la existencia de entidadFederativaNac en catálogo
      SELECT COUNT(*)
        INTO v_existe_reg
        FROM tab_estado
       WHERE estad_cod = p_entidadFederativaNac;

      -- se valida la clave de entidadFederativaNac existe en catálogo
      IF v_existe_reg = 0 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 17;
         LET v_respuesta_descripcion = 'El valor del campo entidadFederativaNac no existe en catálogo';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo nacionalidad
   IF p_nacionalidad IS NULL OR p_nacionalidad = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 18;
      LET v_respuesta_descripcion = 'Campo nacionalidad nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida la existencia de nacionalidad en catálogo
      SELECT COUNT(*)
        INTO v_existe_reg
        FROM tab_nacionalidad
       WHERE codigo_pais = p_nacionalidad;

      -- se valida la clave de nacionalidad existe en catálogo
      IF v_existe_reg = 0 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 18;
         LET v_respuesta_descripcion = 'El valor del campo nacionalidad no existe en catálogo';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo rfcTrabajador
   IF p_rfcTrabajador IS NULL OR p_rfcTrabajador = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 41;
      LET v_respuesta_descripcion = 'Campo rfcTrabajador nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor de campo rfcTrabajador cumpla con el formato [A-Z][A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9][0-9][0-9]
      -- se validan los primeros caracteres
      IF p_rfcTrabajador[1,4] NOT MATCHES '[A-Z][A-Z][A-Z][A-Z]' THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 41;
         LET v_respuesta_descripcion = 'Los primeros caracteres del campo rfcTrabajador no son válidos';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
      -- se validan los caracteres que deben ser numericos
      IF p_rfcTrabajador[5,10] NOT MATCHES '[0-9][0-9][0-9][0-9][0-9][0-9]' THEN
         -- se cambia estatus de registro a Registro con error (02)
        LET v_respuesta_codRespuesta = '02';
        LET v_respuesta_codDiagnostico = 41;
        LET v_respuesta_descripcion = 'Los caracteres numericos del campo rfcTrabajador no son válidos';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo origenTipoTraspaso
   IF p_origenTipoTraspaso IS NULL OR p_origenTipoTraspaso = 0 THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 42;
      LET v_respuesta_descripcion = 'Campo origenTipoTraspaso nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor de campo origenTipoTraspaso. Debe ser 75 o 76
      IF p_origenTipoTraspaso <> 75 AND p_origenTipoTraspaso <> 76 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 42;
         LET v_respuesta_descripcion = 'El valor del campo origenTipoTraspaso no es válido';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo fechaPrimerRegistro
   IF p_fechaPrimerRegistro IS NULL OR p_fechaPrimerRegistro = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 12;
      LET v_respuesta_descripcion = 'Campo fechaPrimerRegistro nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaPrimerRegistro = MDY(p_fechaPrimerRegistro[6,7], p_fechaPrimerRegistro[9,10], p_fechaPrimerRegistro[1,4]);

      -- se valida fechaPrimerRegistro. No puede ser mayor a TODAY
      IF p_fechaPrimerRegistro > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 12;
         LET v_respuesta_descripcion = 'Campo fechaPrimerRegistro es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo fechaAltaAforeActual
   IF p_fechaAltaAforeActual IS NULL OR p_fechaAltaAforeActual = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 13;
      LET v_respuesta_descripcion = 'Campo fechaAltaAforeActual nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaAltaAforeActual = MDY(p_fechaAltaAforeActual[6,7], p_fechaAltaAforeActual[9,10], p_fechaAltaAforeActual[1,4]);

      -- se valida fechaAltaAforeActual. No puede ser mayor a TODAY
      IF p_fechaAltaAforeActual > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 13;
         LET v_respuesta_descripcion = 'Campo fechaAltaAforeActual es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo fechaAltaUltimoTraspComercial
   IF NOT (p_fechaAltaUltimoTraspComercial IS NULL OR p_fechaAltaUltimoTraspComercial = " ") THEN
      -- se asigna la fecha en formato correcto de AAAA/MM/DD a MM/DD/AAAA
      LET p_fechaAltaUltimoTraspComercial = MDY(p_fechaAltaUltimoTraspComercial[6,7], p_fechaAltaUltimoTraspComercial[9,10], p_fechaAltaUltimoTraspComercial[1,4]);

      -- se valida fechaAltaUltimoTraspComercial. No puede ser mayor a TODAY
      IF p_fechaAltaUltimoTraspComercial > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         --LET v_respuesta_codRespuesta = '02';
         --LET v_respuesta_descripcion = 'Campo fechaAltaUltimoTraspComercial es mayor a la fecha actual';
      END IF
   END IF

  -- duda se debe validar el valor?? 0,1,2
   -- se valida campo indicadorPortabilidad
   IF p_indicadorPortabilidad IS NULL THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 43;
      LET v_respuesta_descripcion = 'Campo indicadorPortabilidad nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo ocupacionProfesionTrab
   IF p_ocupacionProfesionTrab IS NULL OR p_ocupacionProfesionTrab = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 19;
      LET v_respuesta_descripcion = 'Campo ocupacionProfesionTrab nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo actividadGiroNegocioTrab
   IF p_actividadGiroNegocioTrab IS NULL OR p_actividadGiroNegocioTrab = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 21;
      LET v_respuesta_descripcion = 'Campo actividadGiroNegocioTrab nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   --15/06/2021 10:18:54 p. m. no se valida nivel de estudios
   ---- se valida campo nivelEstudioTrab
   --IF p_nivelEstudioTrab IS NULL OR p_nivelEstudioTrab = " " THEN
   --   -- se cambia estatus de registro a Registro con error (02)
   --   LET v_respuesta_codRespuesta = '02';
   --   LET v_respuesta_codDiagnostico = 20;
   --   LET v_respuesta_descripcion = 'Campo nivelEstudioTrab nulo';
   --
   --   RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   --END IF

   -- se valida campo idProcesoExpe
   IF p_idProcesoExpe IS NULL OR p_idProcesoExpe = 0 THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 44;
      LET v_respuesta_descripcion = 'Campo idProcesoExpe nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor de campo idProcesoExpe Debe ser 77 o 78
      IF p_idProcesoExpe <> 77 AND p_idProcesoExpe <> 78 THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 44;
         LET v_respuesta_descripcion = 'El valor del campo idProcesoExpe no es válido: ' || p_idProcesoExpe;

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo calle
   IF p_calle IS NULL OR p_calle = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 23;
      LET v_respuesta_descripcion = 'Campo calle nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo colonia
   IF p_colonia IS NULL OR p_colonia = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 30;
      LET v_respuesta_descripcion = 'Campo colonia nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo entidadFederativaDom
   IF p_entidadFederativaDom IS NULL OR p_entidadFederativaDom = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 17;
      LET v_respuesta_descripcion = 'Campo entidadFederativaDom nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo codigoPostal
   IF p_codigoPostal IS NULL OR p_codigoPostal = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 29;
      LET v_respuesta_descripcion = 'Campo codigoPostal nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo delegacion
   IF p_delegacion IS NULL OR p_delegacion = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 27;
      LET v_respuesta_descripcion = 'Campo delegacion nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      SELECT deleg_cod
        INTO v_deleg_cod
        FROM tab_delegacion
       WHERE deleg_desc = p_delegacion
         AND estad_cod = p_entidadFederativaDom;

      IF v_deleg_cod IS NULL THEN
         SELECT d.deleg_cod
           INTO v_deleg_cod
           FROM tab_codpos c, tab_delegacion d
          WHERE c.cpos_cod = p_codigoPostal
           AND c.deleg_cod = d.deleg_cod
           AND d.deleg_desc = p_delegacion;

         IF v_deleg_cod IS NULL THEN
            LET v_respuesta_codRespuesta = '02';
            LET v_respuesta_codDiagnostico = 27;
            LET v_respuesta_descripcion = 'Campo Delegacion NO existe en el catalogo';

            RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
         END IF
      END IF
   END IF

   -- se valida campo pais
   IF p_pais IS NULL OR p_pais = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 25;
      LET v_respuesta_descripcion = 'Campo pais nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo telefonoMovil
   IF p_telefonoMovil IS NULL OR p_telefonoMovil = 0 THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 45;
      LET v_respuesta_descripcion = 'Campo telefonoMovil nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo correoElecTrabajador
   IF p_correoElecTrabajador IS NULL OR p_correoElecTrabajador = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 46;
      LET v_respuesta_descripcion = 'Campo correoElecTrabajador nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   END IF

   -- se valida campo fechaCertificación
   IF p_fechaCertificacion IS NULL OR p_fechaCertificacion = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 15;
      LET v_respuesta_descripcion = 'Campo fechaCertificacion nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se asigna la fecha en formato correcto de AAAAMMDD HHMMSS a MM/DD/AAAA
      LET p_fechaCertificacion = MDY(p_fechaCertificacion[5,6], p_fechaCertificacion[7,8], p_fechaCertificacion[1,4]);

      -- se valida fechaCertificacion. No puede ser mayor a TODAY
      IF p_fechaCertificacion > TODAY THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 15;
         LET v_respuesta_descripcion = 'Campo fechaCertificacion es mayor a la fecha actual';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   -- se valida campo tiposolicitud
   IF p_tiposolicitud IS NULL OR p_tiposolicitud = " " THEN
      -- se cambia estatus de registro a Registro con error (02)
      LET v_respuesta_codRespuesta = '02';
      LET v_respuesta_codDiagnostico = 16;
      LET v_respuesta_descripcion = 'Campo tiposolicitud nulo';

      RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
   ELSE
      -- se valida valor de campo tiposolicitud. Debe ser 28, 29, 42 o 43
      IF p_tiposolicitud <> '28' AND p_tiposolicitud <> '29' AND p_tiposolicitud <> '42' AND p_tiposolicitud <> '43' THEN
         -- se cambia estatus de registro a Registro con error (02)
         LET v_respuesta_codRespuesta = '02';
         LET v_respuesta_codDiagnostico = 16;
         LET v_respuesta_descripcion = 'El valor del campo tiposolicitud no es válido';

         RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
      END IF
   END IF

   RETURN v_respuesta_codRespuesta, v_respuesta_codDiagnostico, v_respuesta_descripcion;
END FUNCTION











