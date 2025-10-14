CREATE TABLE ret_trabajador_atributo_pension(
id_ret_trab_atrib_pension DECIMAL(11,0) PRIMARY KEY,
nss                  CHAR(11),
curp                 CHAR(18),
nombre_trabajador    CHAR(120),
edad_trabajador      SMALLINT,
tipo_proceso         CHAR(6),
regimen              CHAR(2),
tipo_seguro          CHAR(2),
tipo_pension         CHAR(2),
tipo_prestacion      SMALLINT,
tipo_retiro          CHAR,
diagnostico          CHAR(3),
fecha_recepcion      DATE,
codigo_marca         SMALLINT,
fecha_inicio_marca   DATE,
fecha_fin_marca      DATE,
fecha_identificacion DATE,
folio_identificacion DECIMAL(11,0)
) in ret_dbs1;

CREATE SEQUENCE seq_ret_trab_atrib_pension 
INCREMENT BY 1 START WITH 1 
NOCYCLE CACHE 20 ORDER;

UNLOAD TO 'convmarcas_ret_atrib_CPL-4114.unl'
SELECT * FROM cta_convivencia;

INSERT INTO tab_marca VALUES (935,                         --marca_cod
                             'TRABAJADOR CON ATRIBUTO DE PENSION', --marca_desc
                             0,                            --marca_resulta
                             0,                            --ind_habilita
                             0,                            --ind_saldo
                             TODAY,                        --fecha_actualiza  
                             "SACI" ); 
							 
INSERT INTO tab_rch_marca VALUES (935,                          --marca_cod        
                                  'TRABAJADOR CON ATRIBUTO DE PENSION', --marca_desc       
                                  TODAY,                        --fecha_actualiza  
                                  'SACI' );
								  
-- Insertar convivencia
INSERT INTO cta_convivencia
SELECT 935,
	   marca_activa,
	   0,
	   0,
	   TODAY,
	   "SACI"
  FROM cta_convivencia
WHERE marca_entra = 870;

INSERT INTO cta_convivencia
SELECT marca_entra,
	   935,
	   0,
	   0,
	   TODAY,
	   "SACI"
  FROM cta_convivencia
 WHERE marca_activa =  870;

UPDATE STATISTICS FOR TABLE cta_convivencia;
