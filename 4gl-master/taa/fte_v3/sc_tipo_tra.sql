
DROP     TABLE   taa_cd_tipo_traspaso;

CREATE   TABLE   taa_cd_tipo_traspaso (
                                        tipo_traspaso        CHAR(2),
                                        marca_cod            SMALLINT,
                                        id_aportante         CHAR(8),
                                        ind_cierre_marca     SMALLINT,
                                        descripcion          CHAR(30),
                                        tipo_traspaso_proc   SMALLINT  
                                      );

LOAD     FROM   tipo_traspaso
INSERT   INTO   taa_cd_tipo_traspaso;
