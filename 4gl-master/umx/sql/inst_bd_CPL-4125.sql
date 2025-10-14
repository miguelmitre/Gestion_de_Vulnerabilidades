CREATE TABLE umx_solicitud(
      id_uni_solicitud   decimal(10,0),
      tipo_unificacion   smallint,
      estado_solicitud   smallint,
      curp_empleado      char(18),
      folio_interno      char(14),
      curp_solicitante   char(18), 
      nss_solicitud      char(11),
      fecha_captura      date,
      usuario_captura    char(20),
      resul_operacion    char(2),
      diagnostico        char(3),     
      n_unico_dor        char(18), 
      n_rfc_dor          char(13), 
      n_seguro_dor       char(11), 
      paterno_dor        char(40), 
      materno_dor        char(40), 
      nombres_dor        char(40), 
      sexo_dor           char(1),  
      tipo_solicitud_dor smallint, 
      fena_dor           date,     
      estadon_dor        smallint,
      n_unico_ado        char(18), 
      n_rfc_ado          char(13), 
      n_seguro_ado       char(11), 
      paterno_ado        char(40), 
      materno_ado        char(40), 
      nombres_ado        char(40), 
      sexo_ado           char(1),  
      tipo_solicitud_ado smallint, 
      fena_ado           date,     
      estadon_ado        smallint,
      fecha_actualiza    date,
      usuario_actualiza  char(20)
      ) IN uni_dbs1;
      
ALTER TABLE umx_solicitud ADD CONSTRAINT PRIMARY KEY (id_uni_solicitud);       

CREATE SEQUENCE seq_umx_solicitud INCREMENT BY 1 START WITH 1;  

CREATE TABLE umx_resp_wsop01(
id_uni_resp_ws   DECIMAL(10,0),
id_uni_solicitud DECIMAL(10,0),
cod_header       CHAR(3),
resul_operacion  CHAR(2),
motivo_rechazo   CHAR(3),
fecha_envio      DATE,
usuario          CHAR(20)
) IN uni_dbs1;

ALTER TABLE umx_resp_wsop01 ADD CONSTRAINT PRIMARY KEY (id_uni_resp_ws);

CREATE SEQUENCE seq_umx_resp_wsop01 INCREMENT BY 1 START WITH 1;  


insert into seg_modulo (modulo_cod,modulo_desc,ruta_fte,ruta_exp,ruta_envio,ruta_rescate,ruta_listados,usuario,factualiza) 
VALUES('umx','Unificación mixta','/safre/umx/fte','/safre/umx/exp','/safre_prc/umx/envio','/safre_prc/umx/rescate','/safre_lst/umx','safre',today);