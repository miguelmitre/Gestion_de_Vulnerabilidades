UPDATE ret_tipo_origen_reinv set clave_motivo = null WHERE id_tipo_origen_reinv > 0;

ALTER TABLE safre_af:ret_tipo_origen_reinv DROP clave_motivo;
