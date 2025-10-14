-------cza_tra_afo
CREATE INDEX safre_tmp:ix_cza_tra_afo_1 ON 
             safre_tmp:cza_tra_afo(nom_archivo);

-------det_tra_viv  
CREATE INDEX safre_tmp:ix_det_tra_viv_5 ON 
             safre_tmp:det_tra_viv(estado_reg, nom_archivo);
             
CREATE INDEX safre_tmp:ix_det_tra_viv_6 ON 
             safre_tmp:det_tra_viv(fecha_presentacion, ident_operacion, cve_ced_cuenta, nom_archivo);
             
CREATE INDEX safre_tmp:ix_det_tra_viv_7 ON 
             safre_tmp:det_tra_viv(tipo_registro, cve_ced_cuenta, nom_archivo);