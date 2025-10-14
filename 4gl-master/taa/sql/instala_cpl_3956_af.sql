

-------taa_ctr_traspaso
CREATE INDEX safre_af:ix_taa_ctr_traspaso_1 ON 
             safre_af:taa_ctr_traspaso(fecha_presentacion, id_operacion, folio, nombre_archivo);
             
CREATE INDEX safre_af:ix_taa_ctr_traspaso_2 ON 
             safre_af:taa_ctr_traspaso(nombre_archivo);
