DROP PROCEDURE sp_inserta_sol_movil;

CREATE PROCEDURE sp_inserta_sol_movil(
pc_curp_ahorrador         CHAR(18),
pc_curp_tutor             CHAR(18),
pc_nickname               CHAR(15),
pc_nss                    CHAR(11),
pc_nombre                 VARCHAR(50),
pc_paterno                VARCHAR(50),
pc_materno                VARCHAR(50),
pc_ent_nac                CHAR(02),
pc_sexo                   CHAR(1),
pdt_fena                  DATE,
pc_nacionalidad           CHAR(03),
pc_tel_fijo               CHAR(10),
pc_tel_celular            CHAR(10),
pc_correo_elect           VARCHAR(65),
pc_calle                  VARCHAR(60),
pc_num_ext                VARCHAR(10),
pc_num_int                VARCHAR(10),
pc_colonia                VARCHAR(60),
pc_ciudad                 VARCHAR(40),
pc_pais_cod               VARCHAR(3),
pc_estado                 VARCHAR(40),
pc_delega                 VARCHAR(40),
pc_codpos                 VARCHAR(05),
pc_nombre_benef           VARCHAR(40),
pc_paterno_benef          VARCHAR(40),
pc_materno_benef          VARCHAR(40),
ps_parentesco_benef       INTEGER,
pc_curp_benef             CHAR(18),
pd_porcentaje_benef       DECIMAL(5,2),
pc_nombre_benef2          VARCHAR(40),
pc_paterno_benef2         VARCHAR(40),
pc_materno_benef2         VARCHAR(40),
ps_parentesco_benef2      INTEGER,
pc_curp_benef2            CHAR(18),
pd_porcentaje_benef2      DECIMAL(5,2),
pc_nombre_benef3          VARCHAR(40),
pc_paterno_benef3         VARCHAR(40),
pc_materno_benef3         VARCHAR(40),
ps_parentesco_benef3      INTEGER,
pc_curp_benef3            CHAR(18),
pd_porcentaje_benef3      DECIMAL(5,2),
pc_nombre_benef4          VARCHAR(40),
pc_paterno_benef4         VARCHAR(40),
pc_materno_benef4         VARCHAR(40),
ps_parentesco_benef4      INTEGER,
pc_curp_benef4            CHAR(18),
pd_porcentaje_benef4      DECIMAL(5,2),
pc_nombre_benef5          VARCHAR(40),
pc_paterno_benef5         VARCHAR(40),
pc_materno_benef5         VARCHAR(40),
ps_parentesco_benef5      INTEGER,
pc_curp_benef5            CHAR(18),
pd_porcentaje_benef5      DECIMAL(5,2),
ps_actividad_economica    SMALLINT,
ps_id_proceso_exp         SMALLINT,
pdt_fecha_envio           DATE,
ps_tipo_solicitud         SMALLINT,
pd_n_folio                DECIMAL(10,0),
pc_tipo_afiliacion        CHAR(02),
pc_ind_credito_infonavit  CHAR(01),
pc_ind_siefore            CHAR(01),
pc_ind_portabilidad       CHAR(01),
pc_fecha_primerReg        CHAR(08),
pc_fecha_altaAfore        CHAR(08),
ps_status_interno         SMALLINT,
pc_aplicacion_origen      CHAR(02),
pc_ocupacion              CHAR(02),
pc_nivel_estudios         CHAR(02))

RETURNING SMALLINT, CHAR(200);

DEFINE ldt_fechaAlta             DATE;
DEFINE lc_hora                   CHAR(8);
DEFINE ls_cont_tel               INTEGER;
DEFINE ls_cont_correo            SMALLINT;
DEFINE lc_err_tec_desc           CHAR(255);
DEFINE li_delega                 INTEGER;
DEFINE ls_ciudad                 SMALLINT;
DEFINE ls_cod_rech               SMALLINT;
DEFINE lc_desc_rechazo           CHAR(200);
DEFINE ld_registro               DECIMAL(10,0);
DEFINE ld_val_total_procentaje   DECIMAL(5,2);
DEFINE ld_val_porcentaje         DECIMAL(5,2);
DEFINE ld_val_porcentaje2        DECIMAL(5,2);
DEFINE ld_val_porcentaje3        DECIMAL(5,2);
DEFINE ld_val_porcentaje4        DECIMAL(5,2);
DEFINE ld_val_porcentaje5        DECIMAL(5,2);
DEFINE ls_estadon                SMALLINT;
DEFINE ls_sexo                   SMALLINT;
DEFINE li_marca                  INTEGER;
DEFINE lc_cve_cel                CHAR(3);
DEFINE lc_tip_prob               CHAR(1);
DEFINE lc_fol_prob               CHAR(10);
DEFINE lc_doc_prob               CHAR(16);
DEFINE ls_calle                  SMALLINT;
DEFINE ls_numero                 SMALLINT;
DEFINE ls_colonia                SMALLINT;
DEFINE ls_delega                 SMALLINT;
DEFINE ls_codpos                 SMALLINT;
DEFINE ls_pais_cod               SMALLINT;
DEFINE ls_estado                 SMALLINT;
DEFINE li_colon_cod              INTEGER;
DEFINE li_deleg_cod              INTEGER;
DEFINE ls_estad_cod              SMALLINT;
DEFINE ls_ciudad_cod             SMALLINT;
DEFINE ls_valida_dom             SMALLINT;
DEFINE ps_estadon                SMALLINT;
DEFINE li_solicitud              INTEGER;
DEFINE li_cod_parentesco         CHAR(2);
DEFINE li_colonia                INTEGER;
DEFINE li_diagnostico            INTEGER;
DEFINE lc_usuario_safre          CHAR(08);
DEFINE ls_status_promotor        SMALLINT;
DEFINE lc_n_rfc                  CHAR(10);
DEFINE li_recepcion              INTEGER;
DEFINE lc_cve_fijo               CHAR(3);
DEFINE lc_tel_fijo               CHAR(10);
DEFINE ls_id_proceso_exp         INTEGER;
DEFINE lc_fecha_primerReg        CHAR(10);
DEFINE lc_fecha_altaAfore        CHAR(10);
DEFINE lc_fecha_primerReg_8      CHAR(08);
DEFINE lc_fecha_altaAfore_8      CHAR(08);
DEFINE ls_actividad              INTEGER;
DEFINE lc_pdt_fena               CHAR(10);
DEFINE v_count_rch               SMALLINT;
DEFINE v_cont_codpos             INTEGER;
DEFINE li_sql_err                INT;
DEFINE li_isam_err               INT;
DEFINE lc_error_info             CHAR(100);

DEFINE ls_valida_catalogo        INTEGER;
DEFINE ls_valida_catalogo_c      VARCHAR(10);
DEFINE v_id_duplicado            DECIMAL(11,0);
DEFINE v_status                  CHAR(2);
DEFINE v_motivo_rechazo          VARCHAR(40);

DEFINE v_existe                  SMALLINT;
 
ON EXCEPTION
   SET li_sql_err, li_isam_err, lc_error_info
   LET lc_err_tec_desc = li_sql_err ||" "||li_isam_err ||" "||lc_error_info ;

   LET lc_desc_rechazo = "PROBLEMA INESPERADO, VERIFIQUE " || lc_err_tec_desc ;
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech,  lc_desc_rechazo;
   EXECUTE PROCEDURE error (-746,lc_err_tec_desc);
END EXCEPTION;

LET ldt_fechaAlta       = TODAY;
LET lc_n_rfc            = pc_curp_ahorrador[1,10];
LET lc_hora             = "";
LET ls_cont_tel         = 0;
LET ls_cont_correo      = 0;
LET li_delega           = 0;
LET ls_ciudad           = 0;
LET ls_estado           = 0;
LET ls_cod_rech         = 0;
LET ld_registro         = 0;
LET ld_val_total_procentaje = 0;
LET li_marca            = 0;
LET lc_cve_cel          = "";
LET lc_tip_prob         = "5"; -- DOCUMENTO CURP
LET lc_fol_prob         = "";
LET lc_doc_prob         = pc_curp_ahorrador[1,16];
LET li_colon_cod        = 0;
LET li_deleg_cod        = 0;
LET ls_estad_cod        = 0;
LET ls_ciudad_cod       = 0;
LET ls_numero           = 0;
LET ls_colonia          = 0;
LET ls_delega           = 0;
LET ls_codpos           = 0;
LET ls_pais_cod         = 0;
LET li_colon_cod        = 0;
LET ls_valida_dom       = 0;
LET ps_estadon          = 0;
LET li_solicitud        = 0;
LET lc_desc_rechazo     = "CAPTURA EXITOSA";
LET li_cod_parentesco   ="";
LET li_recepcion        = 0;
LET li_diagnostico      = 0;
LET li_colonia          = "";
LET ls_status_promotor  = 0;
LET lc_fecha_primerReg  = "";
LET lc_fecha_altaAfore  = "";
LET ls_actividad        = 0;
LET v_count_rch         = 0;
LET v_existe            = 0; 

--CPL-3430 Se realiza reemplazo de caracteres especiales “!” por la letra “ñ” minuscula, “#” por la letra “Ñ” mayúscula
--CPL-3852 se elimina el reemplazo de caracteres especiales por ñ y Ñ

-- SE ASUME QUE TODAS LAS VALIDACIONES SON CORRECTAS
LET ls_cod_rech  = 0;

SELECT to_char(extend (current, hour to second),'%H:%M:%S') ,
       USER
  INTO lc_hora,
       lc_usuario_safre
  FROM systables
 WHERE tabid = 1;

IF pc_fecha_primerReg IS NOT NULL AND pc_fecha_primerReg[1,1] <> " " THEN
   LET lc_fecha_primerReg   = TRIM(pc_fecha_primerReg[5,6]) || '/' || TRIM(pc_fecha_primerReg[7,8]) || '/' || TRIM(pc_fecha_primerReg[1,4]);
   LET lc_fecha_primerReg_8 = TRIM(pc_fecha_primerReg[5,6]) || '/' || TRIM(pc_fecha_primerReg[7,8]) || '/' || TRIM(pc_fecha_primerReg[3,4]);  -- CPL-3137
END IF

IF pc_fecha_altaAfore IS NOT NULL AND pc_fecha_altaAfore[1,1] <> " " THEN
   LET lc_fecha_altaAfore   = TRIM(pc_fecha_altaAfore[5,6]) || '/' || TRIM(pc_fecha_altaAfore[7,8]) || '/' || TRIM(pc_fecha_altaAfore[1,4]);
   LET lc_fecha_altaAfore_8 = TRIM(pc_fecha_altaAfore[5,6]) || '/' || TRIM(pc_fecha_altaAfore[7,8]) || '/' || TRIM(pc_fecha_altaAfore[3,4]);  -- CPL-3137
END IF

SET LOCK MODE TO WAIT;

--Se valida la existencia de solicitudes duplicadas
SELECT FIRST 1
   id_folio_movil,
   status,
   motivo_rechazo
INTO
   v_id_duplicado,
   v_status,
   v_motivo_rechazo
FROM afi_solicitud_movil
WHERE n_folio = pd_n_folio
AND tipo_solicitud = ps_tipo_solicitud;

IF v_id_duplicado IS NOT NULL AND v_id_duplicado > 0 THEN
   --Se localizo una solicitud duplicada
   IF v_status = '01' THEN
      --Solicitud aceptada, se rechaza...
      LET lc_desc_rechazo = "YA EXISTE UN TRAMITE PARA EL MISMO FOLIO Y TIPO DE SOLICITUD.";
      LET ls_cod_rech       = 1;

      RETURN ls_cod_rech, lc_desc_rechazo;
   ELSE
      --La solicitud previa fue rechazada, se respalda para volver a evaluar el registro
      INSERT INTO afi_his_solicitud_movil
      SELECT * FROM afi_solicitud_movil
      WHERE id_folio_movil = v_id_duplicado;
      
      DELETE FROM afi_solicitud_movil WHERE id_folio_movil = v_id_duplicado;
   END IF
END IF

-- SE OBTIENE EL IDENTIFICADOR PARA ACTUALIZAR EL DIAGNOSTICO
SELECT seq_afi_solicitud_movil.NEXTVAL
INTO ld_registro
FROM seg_modulo 
WHERE modulo_cod = 'afi';

INSERT INTO afi_solicitud_movil VALUES (ld_registro,
                                        pc_curp_ahorrador,
                                        pc_curp_tutor,
                                        pc_nickname,
                                        pc_aplicacion_origen,
                                        pc_nss,
                                        pc_nombre,
                                        pc_paterno,
                                        pc_materno,
                                        pc_ent_nac,
                                        pc_sexo,
                                        pdt_fena,
                                        pc_nacionalidad,
                                        pc_tel_fijo,
                                        pc_tel_celular,
                                        pc_correo_elect,
                                        pc_calle,
                                        pc_num_ext,
                                        pc_num_int,
                                        pc_colonia,
                                        pc_ciudad,
                                        pc_pais_cod,
                                        pc_estado,
                                        pc_delega,
                                        pc_codpos,
                                        pc_nombre_benef,
                                        pc_paterno_benef,
                                        pc_materno_benef,
                                        ps_parentesco_benef,
                                        pc_curp_benef,
                                        pd_porcentaje_benef,
                                        pc_nombre_benef2,
                                        pc_paterno_benef2,
                                        pc_materno_benef2,
                                        ps_parentesco_benef2,
                                        pc_curp_benef2,
                                        pd_porcentaje_benef2,
                                        pc_nombre_benef3,
                                        pc_paterno_benef3,
                                        pc_materno_benef3,
                                        ps_parentesco_benef3,
                                        pc_curp_benef3,
                                        pd_porcentaje_benef3,
                                        pc_nombre_benef4,
                                        pc_paterno_benef4,
                                        pc_materno_benef4,
                                        ps_parentesco_benef4,
                                        pc_curp_benef4,
                                        pd_porcentaje_benef4,
                                        pc_nombre_benef5,
                                        pc_paterno_benef5,
                                        pc_materno_benef5,
                                        ps_parentesco_benef5,
                                        pc_curp_benef5,
                                        pd_porcentaje_benef5,
                                        ps_actividad_economica,
                                        ps_id_proceso_exp,
                                        pdt_fecha_envio,
                                        ps_tipo_solicitud,
                                        pd_n_folio,
                                        pc_tipo_afiliacion,
                                        pc_ind_credito_infonavit,
                                        pc_ind_siefore,
                                        pc_ind_portabilidad,
                                        lc_fecha_primerReg_8,
                                        lc_fecha_altaAfore_8,
                                        pc_ocupacion,
                                        pc_nivel_estudios,
                                        '',
                                        '',
                                        ldt_fechaAlta);
SET LOCK MODE TO NOT WAIT;



IF pc_aplicacion_origen IS NULL OR pc_aplicacion_origen = "00" THEN
   LET lc_desc_rechazo = "APLICACION ORIGEN NULO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
ELSE
   -- SE COTEJA QUE SE INGRESE UN VALOR VALIDO
   SELECT 
      cod_aplicacion
   INTO
      ls_valida_catalogo_c
   FROM tab_aplicacion_origen
   WHERE cod_aplicacion = pc_aplicacion_origen;

   IF ls_valida_catalogo_c IS NULL OR ls_valida_catalogo_c[1] = ' ' THEN
      LET lc_desc_rechazo = "APLICACION ORIGEN NO EXISTE EN CATALOGO ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

LET lc_pdt_fena = pdt_fena;
IF pdt_fena IS NULL OR lc_pdt_fena[1,1] = "" THEN
   LET lc_desc_rechazo = "FECHA NACIMIENTO REQUERIDA";
   LET ls_cod_rech     = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech, lc_desc_rechazo;
END IF;

IF pc_curp_ahorrador IS NULL OR pc_curp_ahorrador[1,1] = "" THEN
   LET lc_desc_rechazo = "CURP AHORRADOR REQUERIDA";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF


IF(pc_aplicacion_origen = "07")THEN
   IF pc_aplicacion_origen = "07" AND ps_tipo_solicitud NOT IN (38,39,40,41) THEN
       LET lc_desc_rechazo = "EL TIPO DE SOLICITUD NO CORRESPONDE AL PORTAL WEB";
       LET ls_cod_rech       = 1;

       EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
       RETURN ls_cod_rech    ,
              lc_desc_rechazo;
   ELSE -- AFORE WEB
      -- Ocupacion y nivel de estudios son obligatorios para  pc_aplicacion_origen = "07" AND ps_tipo_solicitud IN (38,39,40,41)
      LET v_existe = 0;
      --PROFESION
      SELECT FIRST 1 1
        INTO v_existe
        FROM tab_ocupacion
       WHERE ocupa_cod = pc_ocupacion;
      IF v_existe IS NULL OR v_existe = 0 THEN
         LET lc_desc_rechazo = "OCUPACION NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF

      LET v_existe = 0;
      --ESTUDIOS
      SELECT FIRST 1 1
        INTO v_existe
        FROM tab_nivel
       WHERE nivel_cod = pc_nivel_estudios;
      IF v_existe IS NULL OR v_existe = 0 THEN
         LET lc_desc_rechazo = "NIVEL ESTUDIOS NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF
   END IF
END IF

IF pc_aplicacion_origen IN ("01","02") AND ps_tipo_solicitud IN (23,24,26,28,29,30) THEN -- AFORE MOVIL
   -- Ocupacion y nivel de estudios son obligatorios para  pc_aplicacion_origen IN ("01","02") AND ps_tipo_solicitud IN (23,24,26,28,29,30)
   LET v_existe = 0;
   --PROFESION
   SELECT FIRST 1 1
     INTO v_existe
     FROM tab_ocupacion
    WHERE ocupa_cod = pc_ocupacion;
   IF v_existe IS NULL OR v_existe = 0 THEN
      LET lc_desc_rechazo = "OCUPACION NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF

   LET v_existe = 0;
   --ESTUDIOS
   SELECT FIRST 1 1
     INTO v_existe
     FROM tab_nivel
    WHERE nivel_cod = pc_nivel_estudios;
   IF v_existe IS NULL OR v_existe = 0 THEN
      LET lc_desc_rechazo = "NIVEL ESTUDIOS NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF


IF pc_ocupacion IS NULL OR pc_ocupacion = "" OR pc_ocupacion[1] = " " THEN
ELSE
   LET v_existe = 0;
   SELECT FIRST 1 1
     INTO v_existe
     FROM tab_ocupacion
    WHERE ocupa_cod = pc_ocupacion;
   IF v_existe IS NULL OR v_existe = 0 THEN
      LET lc_desc_rechazo = "OCUPACION NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   --Genera registro
   --PROFESION
   LET v_existe = 0;
   SELECT FIRST 1 profesion_cod
     INTO v_existe
     FROM afi_ctr_actividad
    WHERE n_folio = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF v_existe IS NOT NULL AND v_existe > 0 THEN
      SET LOCK MODE TO WAIT;
      UPDATE afi_ctr_actividad
         SET profesion_cod = pc_ocupacion ,
             usuario = lc_usuario_safre,
             factualiza = ldt_fechaAlta
       WHERE n_folio = pd_n_folio
         AND tipo_solicitud = ps_tipo_solicitud;
      SET LOCK MODE TO NOT WAIT;
   ELSE
      INSERT INTO afi_ctr_actividad
        VALUES(pc_nss,
               pd_n_folio,
               ps_tipo_solicitud,
               pc_ocupacion,
               '',
               lc_usuario_safre,
               ldt_fechaAlta);
   END IF
END IF

IF pc_nivel_estudios IS NULL OR pc_nivel_estudios = "" OR pc_nivel_estudios[1] = " " THEN
ELSE
   LET v_existe = 0;
   SELECT FIRST 1 1
     INTO v_existe
     FROM tab_nivel
    WHERE nivel_cod = pc_nivel_estudios;
   IF v_existe IS NULL OR v_existe = 0 THEN
      LET lc_desc_rechazo = "NIVEL ESTUDIOS NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF

   --Genera registro
   --ESTUDIOS
   LET v_existe = 0;
   SELECT FIRST 1 nivel_cod
     INTO v_existe
     FROM afi_nivel_estudios
    WHERE n_folio = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF v_existe IS NOT NULL AND v_existe > 0 THEN
      SET LOCK MODE TO WAIT;
      UPDATE afi_nivel_estudios
         SET nivel_cod = pc_nivel_estudios,
             usuario = lc_usuario_safre,
             factualiza = ldt_fechaAlta
       WHERE n_folio = pd_n_folio
         AND tipo_solicitud = ps_tipo_solicitud;
      SET LOCK MODE TO NOT WAIT;
   ELSE
      INSERT INTO afi_nivel_estudios
        VALUES(pc_nss,
               pd_n_folio,
               ps_tipo_solicitud,
               pc_nivel_estudios,
               lc_usuario_safre,
               ldt_fechaAlta);
   END IF
END IF

IF ps_tipo_solicitud = 26 OR ps_tipo_solicitud = 40 THEN
   IF pc_curp_tutor IS NULL OR pc_curp_tutor[1,1] = "" THEN
      LET lc_desc_rechazo = "CURP TUTOR REQUERIDO ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF

   IF pc_nickname IS NULL OR pc_nickname[1,1] = "" THEN
      LET lc_desc_rechazo = "NICKNAME REQUERIDO ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF ps_tipo_solicitud IN (23,30,38,41) THEN
   IF pc_nss IS NULL OR  pc_nss [1] = ' ' OR  pc_nss [1] = '00000000000' THEN
      LET lc_desc_rechazo = "NSS REQUERIDO ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF pc_nombre IS NULL OR pc_nombre[1] = " " THEN
   LET lc_desc_rechazo = "NOMBRE NULO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pc_paterno IS NULL OR pc_paterno[1] = " " THEN
   LET lc_desc_rechazo = "APELLIDO PATERNO NULO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF ps_id_proceso_exp IS NULL OR ps_id_proceso_exp = 0 THEN
   LET lc_desc_rechazo = "ID PROCESO EXPEDIENTE NULO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
ELSE
   -- SE COTEJA QUE SE INGRESE UN VALOR VALIDO
   SELECT id_proceso_exp
     INTO ls_id_proceso_exp
     FROM tab_proceso_exp
    WHERE cve_proceso_exp = ps_id_proceso_exp;

   IF ls_id_proceso_exp is null OR ls_id_proceso_exp <= 0 THEN
      LET lc_desc_rechazo = "ID PROCESO EXPEDIENTE NO EXISTE EN CATALOGO ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF pdt_fecha_envio IS NULL THEN
   LET lc_desc_rechazo = "FECHA DE ENVIO NULA ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pc_tipo_afiliacion IN ("60","61","02","03","04","2","3","4") THEN     --- CPL-3137
ELSE
   LET lc_desc_rechazo = "TIPO DE AFILIACION NO IDENTIFICADO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pd_n_folio  IS NULL OR pd_n_folio  = 0 THEN
   LET lc_desc_rechazo = "FOLIO DE LA SOLICITUD NULO ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pc_fecha_primerReg IS NULL OR pc_fecha_primerReg = " " THEN
   LET lc_desc_rechazo = "FECHA DE PRIMER REGISTRO NO VALIDA ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pc_fecha_altaAfore IS NULL OR pc_fecha_altaAfore = " " THEN
   LET lc_desc_rechazo = "FECHA ALTA AFORE NO VALIDA ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF ps_tipo_solicitud IS NULL  THEN
   LET lc_desc_rechazo = "TIPO DE SOLICITUD NULA ";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
ELSE
   IF ps_tipo_solicitud IN (23,24,26,30,38,39,40,41) THEN
   ELSE
      LET lc_desc_rechazo = "TIPO DE SOLICITUD NO VALIDA ";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
              lc_desc_rechazo;
   END IF
END IF

SELECT FIRST 1 tipo_solicitud
  INTO li_solicitud
  FROM afi_solicitud
 WHERE n_folio = pd_n_folio
   AND tipo_solicitud = ps_tipo_solicitud;

IF li_solicitud IS NOT NULL AND li_solicitud > 0 THEN
   LET lc_desc_rechazo = "YA EXISTE UN TRAMITE PARA EL MISMO FOLIO Y TIPO DE SOLICITUD.";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

IF pc_nss IS NULL OR pc_nss[1] = ' ' OR pc_nss = '00000000000' THEN
   EXECUTE FUNCTION fn_obtiene_nti(pc_curp_ahorrador) INTO pc_nss;

   SET LOCK MODE TO WAIT;
   -- se actualiza tabla de paso
   UPDATE afi_solicitud_movil
      SET nss     = pc_nss
    WHERE id_folio_movil = ld_registro;
   SET LOCK MODE TO NOT WAIT;
END IF

LET li_solicitud = 0;
IF pc_tipo_afiliacion = "60" THEN
ELSE
   SELECT FIRST 1 tipo_solicitud
     INTO li_solicitud
     FROM afi_mae_afiliado
    WHERE n_seguro = pc_nss
      AND tipo_solicitud <> 5 -- CPL-3798 Registro por asignación (Tipo solicitud 5) debe aceptarse
    ;
END IF

IF li_solicitud IS NOT NULL AND li_solicitud > 0 THEN
   LET v_count_rch = 0;
   SELECT COUNT(*)
     INTO v_count_rch
     FROM cta_act_marca
    WHERE nss = pc_nss
      AND marca_cod IN (120,130,150);

   IF v_count_rch >= 1 THEN

   ELSE
      LET lc_desc_rechazo = "YA EXISTE UN TRAMITE EN EL MAESTRO DE AFILIADOS.";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech, lc_desc_rechazo;
   END IF
END IF

LET pc_calle    = TRIM(pc_calle);
LET pc_num_ext  = TRIM(pc_num_ext);
LET pc_colonia  = TRIM(pc_colonia);
LET pc_delega   = TRIM(pc_delega);
LET pc_codpos   = TRIM(pc_codpos);
LET pc_pais_cod = TRIM(pc_pais_cod);
LET pc_estado   = TRIM(pc_estado);

-- SE ASIGNA LA LONGITUD DE LA INFORMACION
LET ls_calle      = LENGTH(pc_calle);
LET ls_numero     = LENGTH(pc_num_ext);
LET ls_colonia    = LENGTH(pc_colonia);
LET ls_delega     = LENGTH(pc_delega);
LET ls_ciudad     = LENGTH(pc_ciudad);
LET ls_codpos     = LENGTH(pc_codpos);
LET ls_pais_cod   = LENGTH(pc_pais_cod);
LET ls_estado     = LENGTH(pc_estado);

-- SE VERIFICA DOMICILIO LABORAL
IF ls_calle > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_numero > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_colonia > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_delega > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_codpos > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_pais_cod > 1 THEN
   LET ls_valida_dom = 1;
ELIF ls_estado > 1 THEN
   LET ls_valida_dom = 1;
END IF

-- SI EXISTE INFORMACION EN EL DOMICILIO SE TRATA COMO OBLIGATORIO
IF ls_valida_dom = 1 THEN
   IF pc_calle IS NULL OR pc_calle[1,1] = " " THEN
      LET lc_desc_rechazo = "CALLE  NULA, VERIFIQUE";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF

   SELECT FIRST 1 colon_cod
     INTO li_colonia
     FROM tab_colonia
    WHERE colon_desc = pc_colonia
      AND cpos_cod    = pc_codpos;

   IF  li_colonia IS NULL OR li_colonia <= 0 THEN
      SELECT MAX(colon_cod) + 1
        INTO li_colon_cod
        FROM tab_colonia;

      
      INSERT INTO tab_colonia VALUES(pc_codpos,li_colon_cod,pc_colonia);
     
   END IF
   
   
   SELECT FIRST 1
      cp.estad_cod  ,
      cp.deleg_cod  ,
      cp.ciudad_cod
   INTO 
      ls_estad_cod  ,
      li_deleg_cod  ,
      ls_ciudad_cod
   FROM tab_codpos cp
   INNER JOIN tab_delegacion del ON del.deleg_cod = cp.deleg_cod
   INNER JOIN tab_estado es ON es.estad_cod = cp.estad_cod
   WHERE del.deleg_desc = pc_delega
   AND es.estad_desc = pc_estado;

   IF DBINFO('sqlca.sqlerrd2') = 0 THEN

   SELECT FIRST 1                                                                               -- CPL-3137
      del.deleg_cod
   INTO
      li_deleg_cod
   FROM tab_delegacion del
   INNER JOIN tab_estado es ON es.estad_cod = del.estad_cod
   WHERE del.deleg_desc = pc_delega;

      SELECT FIRST 1 estad_cod
        INTO ls_estad_cod
        FROM tab_estado
       WHERE estad_desc = pc_estado;

      SELECT FIRST 1           -- CPL-3137
             cp.ciudad_cod 
        INTO ls_ciudad_cod
        FROM tab_codpos cp
             INNER JOIN tab_estado es ON es.estad_cod = cp.estad_cod
       WHERE cp.cpos_cod = pc_codpos
         AND es.estad_desc = pc_estado;

      IF ls_ciudad_cod IS NULL THEN
         SELECT FIRST 1 
            ciudad_cod 
         INTO
            ls_ciudad_cod 
         FROM tab_codpos 
         WHERE deleg_cod = li_deleg_cod;
      END IF

      IF pc_codpos IS NULL THEN
         LET lc_desc_rechazo = "CODIGO POSTAL  NULO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF

      IF ls_estad_cod IS NULL THEN
         LET lc_desc_rechazo = " ESTADO  NULO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF

      IF li_deleg_cod IS NULL THEN
         LET lc_desc_rechazo = " DELEGACION  NULA";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF

      IF ls_ciudad_cod IS NULL THEN
         LET lc_desc_rechazo = "CIUDAD  NULA";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF

      SELECT FIRST 1 cpos_cod
        INTO v_cont_codpos
        FROM tab_codpos
       WHERE cpos_cod = pc_codpos;

      IF (v_cont_codpos IS NULL OR v_cont_codpos <= 0) THEN
         
         INSERT INTO tab_codpos VALUES(pc_codpos,ls_estad_cod,li_deleg_cod,ls_ciudad_cod);
        
      END IF
   END IF

   SELECT FIRST 1 colon_cod
     INTO li_colonia
     FROM tab_colonia
    WHERE colon_desc = pc_colonia
      AND cpos_cod   = pc_codpos;

   IF  li_colonia IS NULL OR li_colonia <= 0 THEN
      SELECT MAX(colon_cod) + 1 INTO li_colon_cod
        FROM tab_colonia;
      
      INSERT INTO tab_colonia VALUES(pc_codpos,li_colon_cod,pc_colonia);
      
   END IF

   IF pc_num_ext IS NULL OR pc_num_ext[1,1] = " " THEN
      LET lc_desc_rechazo = "NUMERO EXTERIOR  NULO";
      LET ls_cod_rech     = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech, lc_desc_rechazo;
   END IF

   IF pc_colonia IS NULL OR pc_colonia[1,1] = " " THEN
      LET lc_desc_rechazo = "COLONIA  NULA";
      LET ls_cod_rech     = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   ELSE
      SELECT UNIQUE colon_desc
        INTO pc_colonia
        FROM tab_colonia
       WHERE colon_desc = pc_colonia
         AND cpos_cod    = pc_codpos;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "COLONIA  NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                 lc_desc_rechazo;
      END IF
   END IF

   IF pc_delega IS NULL OR pc_delega[1,1] = " " THEN
      LET lc_desc_rechazo = "DELEGACION  NULA";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
              lc_desc_rechazo;
   ELSE
      SELECT UNIQUE deleg_desc
        INTO pc_delega
        FROM tab_delegacion
       WHERE deleg_desc = pc_delega
         AND estad_cod IN ( SELECT UNIQUE estad_cod FROM tab_estado WHERE estad_desc = pc_estado) ;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "DELEGACON  NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;
         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech    ,
                lc_desc_rechazo;
      END IF
   END IF

   IF pc_codpos IS NULL OR pc_codpos[1,1] = " " THEN
      LET lc_desc_rechazo = "CODIGO POSTAL  NULO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   ELSE
      SELECT UNIQUE cpos_cod
        INTO pc_codpos
        FROM tab_codpos
       WHERE cpos_cod = pc_codpos;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "CODIGO POSTAL NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;
         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech, lc_desc_rechazo;
      END IF
   END IF

   IF pc_ciudad IS NULL OR pc_ciudad[1,1] = " " THEN
      LET lc_desc_rechazo = "CIUDAD  NULA";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   ELSE

      SELECT UNIQUE ciudad_desc
        INTO pc_ciudad
        FROM tab_ciudad
       WHERE ciudad_desc = pc_ciudad;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "CIUDAD NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech, lc_desc_rechazo;
      END IF
   END IF

   IF pc_pais_cod IS NULL OR pc_pais_cod[1,1] = " " THEN
      LET lc_desc_rechazo = "CODIGO PAIS  NULO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
              lc_desc_rechazo;
   ELSE
      SELECT UNIQUE pais_cod
        INTO pc_pais_cod
        FROM tab_pais
       WHERE pais_cod = pc_pais_cod;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "PAIS NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech, lc_desc_rechazo;
      END IF
   END IF

   IF pc_estado IS NULL OR pc_estado[1,1] = " " THEN
      LET lc_desc_rechazo = "ESTADO  NULO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   ELSE
      SELECT UNIQUE estad_desc
        INTO pc_estado
        FROM tab_estado
       WHERE estad_desc = pc_estado;

      IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
         LET lc_desc_rechazo = "ESTADO NO EXISTE EN CATALOGO";
         LET ls_cod_rech       = 1;

         EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
         RETURN ls_cod_rech,lc_desc_rechazo;
      END IF
   END IF
END IF

--DATOS CONTACTO NO SON REQUERIDOS PERO SI TRAEN INFORMACION SE TRATAN
IF pc_tel_fijo IS NOT NULL AND pc_tel_fijo[1,1] <> " " THEN
   IF LENGTH(pc_tel_fijo) <> 10 THEN
      LET lc_desc_rechazo = "TELEFONO FIJO DIFERENTE DE 10 POSICIONES";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF pc_tel_celular IS NOT NULL AND pc_tel_celular[1,1] <> " " THEN
   IF LENGTH(pc_tel_celular) <> 10 THEN
      LET lc_desc_rechazo = "TELEFONO CELUAR DIFERENTE DE 10 POSICIONES";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF
IF pc_correo_elect IS NOT NULL AND pc_correo_elect[1,1] <> " " THEN
   IF pc_correo_elect NOT MATCHES "*@*" THEN
      LET lc_desc_rechazo = "CORREO ELECTRONICO NO VALIDO, VERIFIQUE";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

-- ESTADO DE NACIMIENTO
IF pc_ent_nac IS NOT NULL AND pc_ent_nac[1,1] <> " " THEN
   SELECT cve_edo
     INTO ps_estadon
     FROM tab_estado_curp
    WHERE cve_edo   = pc_ent_nac;

   IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
      LET lc_desc_rechazo = "ESTADO DE NACIMIENTO NO EXISTE EN EL CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF pc_nacionalidad  IS NOT NULL AND pc_nacionalidad[1,1] <> " " THEN
   SELECT codigo_pais
     INTO pc_nacionalidad
     FROM tab_nacionalidad
     WHERE codigo_pais = pc_nacionalidad;

   IF  DBINFO('sqlca.sqlerrd2') = 0 THEN
      LET lc_desc_rechazo = "NACIONALIDAD NO EXISTE EN EL CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

-- BENEFICIARIOS bloque no requerido si viene se trata
IF pc_curp_benef IS NOT NULL AND pc_curp_benef[1] <> " " THEN
   --se valida que NO supere el 100%
   SELECT SUM(porcentaje)
     INTO ld_val_porcentaje
     FROM afi_beneficiario
    WHERE n_seguro       = pc_nss
      AND n_folio        = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ld_val_porcentaje IS NULL THEN
      LET ld_val_porcentaje = 0;
   END IF

   IF (ld_val_porcentaje + pd_porcentaje_benef ) > 100.00 THEN
      LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   LET ld_val_total_procentaje = ld_val_total_procentaje + pd_porcentaje_benef;
END IF
IF pc_curp_benef2 IS NOT NULL AND pc_curp_benef2[1,1] <> "" THEN
   --se valida que NO supere el 100%
   SELECT SUM(porcentaje)
     INTO ld_val_porcentaje2
     FROM afi_beneficiario
    WHERE n_seguro       = pc_nss
      AND n_folio        = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ld_val_porcentaje2 IS NULL THEN
      LET ld_val_porcentaje2 = 0;
   END IF

   IF (ld_val_porcentaje2 + pd_porcentaje_benef2 ) > 100.00 THEN
      LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   LET ld_val_total_procentaje = ld_val_total_procentaje + pd_porcentaje_benef2;
END IF

IF pc_curp_benef3 IS NOT NULL AND pc_curp_benef3[1,1] <> "" THEN
   --se valida que NO supere el 100%
   SELECT SUM(porcentaje)
     INTO ld_val_porcentaje3
     FROM afi_beneficiario
    WHERE n_seguro       = pc_nss
      AND n_folio        = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ld_val_porcentaje3 IS NULL THEN
      LET ld_val_porcentaje3 = 0;
   END IF

   IF (ld_val_porcentaje3 + pd_porcentaje_benef3 ) > 100.00 THEN
      LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   LET ld_val_total_procentaje = ld_val_total_procentaje + pd_porcentaje_benef3;
END IF

IF pc_curp_benef4 IS NOT NULL AND pc_curp_benef4[1,1] <> "" THEN
   --se valida que NO supere el 100%
   SELECT SUM(porcentaje)
     INTO ld_val_porcentaje4
     FROM afi_beneficiario
    WHERE n_seguro       = pc_nss
      AND n_folio        = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ld_val_porcentaje4 IS NULL THEN
      LET ld_val_porcentaje4 = 0;
   END IF

   IF (ld_val_porcentaje4 + pd_porcentaje_benef4 ) > 100.00 THEN
      LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   LET ld_val_total_procentaje = ld_val_total_procentaje + pd_porcentaje_benef4;
END IF

IF pc_curp_benef5 IS NOT NULL AND pc_curp_benef5[1,1] <> "" THEN
   --se valida que NO supere el 100%
   SELECT SUM(porcentaje)
     INTO ld_val_porcentaje5
     FROM afi_beneficiario
    WHERE n_seguro       = pc_nss
      AND n_folio        = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ld_val_porcentaje5 IS NULL THEN
      LET ld_val_porcentaje5 = 0;
   END IF

   IF (ld_val_porcentaje5 + pd_porcentaje_benef5 ) > 100.00 THEN
      LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
   LET ld_val_total_procentaje = ld_val_total_procentaje + pd_porcentaje_benef5;
END IF

IF ld_val_total_procentaje > 100 THEN
   LET lc_desc_rechazo = "SUMA PORCENTAJE BENEF MAYOR A 100";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

--INICIO NEGOCIO, SE OBTIENE ESTADON YA QUE NO ES OBLIGATORIO PERO SI REQUERIDO EN LA SOLICITUD
SELECT cve_edo
  INTO ls_estadon
  FROM tab_estado_curp
 WHERE cve_edo_curp =  pc_curp_ahorrador[12,13];

IF pc_sexo IS NULL OR pc_sexo[1,1] = " " THEN
   LET pc_sexo = pc_curp_ahorrador[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF
ELSE
   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   --CPL-3430 en caso de enviar el sexo en el formato correcto se asigna directo a la variable
   IF pc_sexo = '1' OR pc_sexo = '2' THEN
      LET ls_sexo = pc_sexo;
   END IF
END IF

LET li_cod_parentesco = ""; 
IF  pc_paterno_benef IS NOT NULL  AND pc_paterno_benef[1] <> " " AND pc_nombre_benef IS NOT NULL  AND pc_nombre_benef[1]  <> " "  AND ps_parentesco_benef IS NOT NULL AND ps_parentesco_benef > 0 THEN
   SELECT FIRST 1 paren_cod 
   INTO li_cod_parentesco
     FROM tab_parentesco_cuo
    WHERE paren_cod = ps_parentesco_benef;

   IF li_cod_parentesco IS NULL OR li_cod_parentesco <= 0 THEN
      LET lc_desc_rechazo = "CODIGO PARENTESCO BENEF 1 NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

LET li_cod_parentesco = "";
IF  pc_paterno_benef2 IS NOT NULL  AND pc_paterno_benef2[1,1] <> "" AND pc_nombre_benef2 IS NOT NULL  AND pc_nombre_benef2[1,1]  <> "" AND ps_parentesco_benef2 IS NOT NULL AND ps_parentesco_benef2 > 0 THEN
   SELECT FIRST 1 paren_cod
     INTO li_cod_parentesco
     FROM tab_parentesco_cuo
    WHERE paren_cod = ps_parentesco_benef2;

   IF li_cod_parentesco IS NULL OR li_cod_parentesco <= 0 THEN
      LET lc_desc_rechazo = "CODIGO PARENTESCO BENEF 2 NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;

      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

LET li_cod_parentesco = "";
IF  pc_paterno_benef3 IS NOT NULL  AND pc_paterno_benef3[1,1] <> "" AND pc_nombre_benef3 IS NOT NULL  AND pc_nombre_benef3[1,1]  <> "" AND ps_parentesco_benef3 IS NOT NULL AND ps_parentesco_benef3 > 0 THEN
   SELECT FIRST 1 paren_cod
     INTO li_cod_parentesco
     FROM tab_parentesco_cuo
    WHERE paren_cod = ps_parentesco_benef3;

   IF li_cod_parentesco IS NULL OR li_cod_parentesco <= 0 THEN
      LET lc_desc_rechazo = "CODIGO PARENTESCO BENEF 3 NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

LET li_cod_parentesco = "";
IF  pc_paterno_benef4 IS NOT NULL  AND pc_paterno_benef4[1,1] <> "" AND pc_nombre_benef4 IS NOT NULL  AND pc_nombre_benef4[1,1]  <> "" AND ps_parentesco_benef4 IS NOT NULL AND ps_parentesco_benef4 > 0 THEN
   SELECT FIRST 1 paren_cod
     INTO li_cod_parentesco
     FROM tab_parentesco_cuo
    WHERE paren_cod = ps_parentesco_benef4;

   IF li_cod_parentesco IS NULL OR li_cod_parentesco <= 0 THEN
      LET lc_desc_rechazo = "CODIGO PARENTESCO BENEF 4 NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

LET li_cod_parentesco = "";
IF  pc_paterno_benef5 IS NOT NULL  AND pc_paterno_benef5[1,1] <> "" AND pc_nombre_benef5 IS NOT NULL  AND pc_nombre_benef5[1,1]  <> "" AND ps_parentesco_benef5 IS NOT NULL AND ps_parentesco_benef5 > 0 THEN
   SELECT FIRST 1 paren_cod
     INTO li_cod_parentesco
     FROM tab_parentesco_cuo
    WHERE paren_cod = ps_parentesco_benef5;

   IF li_cod_parentesco IS NULL OR li_cod_parentesco <= 0 THEN
      LET lc_desc_rechazo = "CODIGO PARENTESCO BENEF 5 NO EXISTE EN CATALOGO";
      LET ls_cod_rech       = 1;
      EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
      RETURN ls_cod_rech    ,
             lc_desc_rechazo;
   END IF
END IF

IF ps_actividad_economica IS NOT NULL THEN
   SELECT actividad_cod
     INTO ps_actividad_economica
     FROM tab_actividad_ecocuo
    WHERE actividad_cod = ps_actividad_economica;

   IF ps_actividad_economica = 0  OR ps_actividad_economica IS NULL THEN
       LET lc_desc_rechazo = "CODIGO ACTIVIDAD ECONOMICA NO EXISTE EN EL CATÁLOGO";
       LET ls_cod_rech       = 1;
       EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
       RETURN ls_cod_rech    ,
              lc_desc_rechazo;
   END IF

   SELECT FIRST 1 tipo_solicitud
   INTO ls_actividad
     FROM afi_ctr_actividad
    WHERE n_folio = pd_n_folio
      AND tipo_solicitud = ps_tipo_solicitud;

   IF ls_actividad IS NOT NULL AND ls_actividad > 0 THEN
      SET LOCK MODE TO WAIT;
      UPDATE afi_ctr_actividad
         SET actividad_cod = ps_actividad_economica ,
             usuario = lc_usuario_safre       ,
             factualiza = ldt_fechaAlta
       WHERE n_folio = pd_n_folio
         AND tipo_solicitud = ps_tipo_solicitud;
      SET LOCK MODE TO NOT WAIT;
   ELSE
      
      INSERT INTO afi_ctr_actividad
        VALUES(pc_nss,
               pd_n_folio,
               ps_tipo_solicitud,
               '',
               ps_actividad_economica,
               lc_usuario_safre,
               ldt_fechaAlta);
   END IF
END IF

SELECT COUNT(*) INTO li_recepcion
  FROM afi_recepcion
 WHERE n_folio        = pd_n_folio
   AND tipo_solicitud = ps_tipo_solicitud;

IF DBINFO('sqlca.sqlerrd2') = 0 THEN
   LET lc_desc_rechazo = "SOLICITUD NO TIENE RECEPCION, VERIFIQUE";
   LET ls_cod_rech       = 1;

   EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro,lc_desc_rechazo);
   RETURN ls_cod_rech    ,
          lc_desc_rechazo;
END IF

-- AFI_SOLICITUD SE AGREGA INFORMACION EN AFI_SOLICITUD
SET LOCK MODE TO WAIT;
INSERT INTO afi_solicitud VALUES( pc_nss,                  -- n_seguro
                                  pc_curp_ahorrador,       -- n_unico
                                  lc_n_rfc,                -- n_rfc
                                  pc_paterno,              -- paterno
                                  pc_materno,              -- materno
                                  pc_nombre,               -- nombres
                                  pdt_fena,                -- fena
                                  pd_n_folio ,             -- n_folio
                                  0,                       -- edo_civil
                                  "",                      -- localn
                                  ls_estadon,              -- estadon
                                  0,                       -- tiptr
                                  0,                       -- cod_promotor
                                  ls_sexo,                 -- sexo
                                  0,                       -- n_operac
                                  lc_fecha_altaAfore,      -- frecafor
                                  "",                      -- fentcons
                                  "",                      -- femision
                                  pdt_fecha_envio,         -- finitmte
                                  "",                      -- finicta
                                  0,                       -- status
                                  0,                       -- agenc_cod
                                  ps_status_interno,       --60,                      -- status_interno
                                  pc_nacionalidad,         -- nacionalidad
                                  lc_tip_prob,             -- tip_prob
                                  lc_fol_prob,             -- fol_prob
                                  lc_doc_prob,             -- doc_prob
                                  "",                      -- ind_infonavit
                                  "",                      -- documento_1
                                  "",                      -- documento_2
                                  "",                      -- documento_3
                                  "",                      -- documento_4
                                  "",                      -- documento_5
                                  "",                      -- documento_6
                                  "",                      -- envio_dom
                                  "",                      -- entidad_curp
                                  "",                      -- asigna_curp
                                  "",                      -- const_curp
                                  lc_usuario_safre,        -- usuario
                                  lc_hora,                 -- hora
                                  0,                       -- status_captura
                                  ps_tipo_solicitud,       -- tipo_solicitud
                                  "",                      -- fecha_elaboracion
                                  0,                       -- lote
                                  pdt_fecha_envio,         -- fecha_envio
                                  1,                       -- cod_esq_comision
                                  "",                      -- ubicacion
                                  lc_fecha_primerReg,      -- fecha_1a_afil
                                  "",                      -- indicador_c
                                  "",                      -- indicador_d
                                  "",                      -- indicador_e
                                  "",                      -- cod_error_origen
                                  "",                      -- folio_edo_cta
                                  0 ,                      -- cod_afore_ced
                                  "",                      -- pd_salario_base_comis,
                                  0,                       -- salario_actual
                                  "",                      -- fecha_actualiza_sa
                                  "",                      -- coduni_n1
                                  0 ,                      -- indicador_comision
                                  0,                       -- codven
                                  "",                      -- coor_captura
                                  "",                      -- lote_captura
                                  "",                      -- folio_captura
                                  ""                       -- sello_electronico
                                  );


-- AFI_DOMICILIO PARTICULAR

DELETE FROM afi_domicilio
WHERE n_folio        = pd_n_folio
AND tipo_solicitud = ps_tipo_solicitud
AND dom_cod        = 1;

INSERT INTO afi_domicilio VALUES (  pc_nss,
                                    pd_n_folio,
                                    ps_tipo_solicitud,
                                    pc_calle,
                                    '',
                                    '',
                                    pc_num_ext,
                                    pc_num_int,
                                    pc_colonia,
                                    li_deleg_cod,
                                    ls_ciudad_cod,
                                    ls_estad_cod,
                                    pc_codpos,
                                    1, -- dom_cod
                                    pc_pais_cod,
                                    'X',
                                    lc_usuario_safre,
                                    ldt_fechaAlta);


--TELEFONOS
DELETE FROM afi_telefono
WHERE n_folio = pd_n_folio
AND tipo_solicitud = ps_tipo_solicitud
AND tel_cod IN (1,4);

 -- PARTICULAR

   -- ASIGNACION CLAVES LADAS
   LET pc_tel_fijo    = TRIM(pc_tel_fijo);

   IF pc_tel_fijo[1,2] = '55' THEN
      LET lc_cve_fijo  = pc_tel_fijo[1,2];
      LET lc_tel_fijo  = pc_tel_fijo[3,10];
   ELSE
      LET lc_cve_fijo  = pc_tel_fijo[1,3];
      LET lc_tel_fijo  = pc_tel_fijo[4,10];
   END IF

   INSERT INTO afi_telefono VALUES (pc_nss,
                                    pd_n_folio,
                                    ps_tipo_solicitud,
                                    '',
                                    lc_cve_fijo,
                                    '',
                                    lc_tel_fijo,
                                    1,
                                    lc_hora,
                                    '',
                                    '',
                                    '',
                                    '',
                                    lc_usuario_safre,
                                    ldt_fechaAlta);


-- CELULAR

   LET pc_tel_celular = TRIM(pc_tel_celular);

   IF pc_tel_celular[1,2] = '55' THEN
      LET lc_cve_cel = "044";
   ELSE
      LET lc_cve_cel = "045";
   END IF

   INSERT INTO afi_telefono VALUES (pc_nss,
                                    pd_n_folio,
                                    ps_tipo_solicitud,
                                    '',
                                    lc_cve_cel,
                                    '',
                                    pc_tel_celular,
                                    4,
                                    lc_hora,
                                    '',
                                    '',
                                    '',
                                    '',
                                    lc_usuario_safre,
                                    ldt_fechaAlta);

--CORREO ELECTRONICO
IF pc_correo_elect IS NOT NULL OR pc_correo_elect[1,1] <> "" THEN

   DELETE FROM afi_correo_elect
   WHERE n_folio        = pd_n_folio
   AND tipo_solicitud = ps_tipo_solicitud;

   INSERT INTO afi_correo_elect VALUES (pc_nss,
                                        pd_n_folio,
                                        ps_tipo_solicitud,
                                        3,  -- CORREO PERSONAL
                                        pc_correo_elect,
                                        'X',
                                        '', --folio
                                        ldt_fechaAlta,
                                        lc_usuario_safre);
END IF

--BENEFICIARIOS
DELETE FROM afi_beneficiario
WHERE n_folio = pd_n_folio
AND tipo_solicitud = ps_tipo_solicitud;

IF pc_curp_benef IS NOT NULL AND pc_curp_benef[1] <> " " THEN
   -- se obtiene sexo del beneficiario 1
   LET pc_sexo = pc_curp_benef[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   INSERT INTO afi_beneficiario
        VALUES (pc_nss,
                pd_n_folio,
                ps_tipo_solicitud,
                ps_parentesco_benef,
                pc_paterno_benef,
                pc_materno_benef,
                pc_nombre_benef,
                '',
                ls_sexo,
                pd_porcentaje_benef,
                1,
                pc_curp_benef);

END IF

IF pc_curp_benef2 IS NOT NULL AND pc_curp_benef2[1] <> " " THEN
   LET pc_sexo = pc_curp_benef2[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   INSERT INTO afi_beneficiario
        VALUES (pc_nss,
                pd_n_folio,
                ps_tipo_solicitud,
                ps_parentesco_benef2,
                pc_paterno_benef2,
                pc_materno_benef2,
                pc_nombre_benef2,
                '',
                ls_sexo,
                pd_porcentaje_benef2,
                2,
                pc_curp_benef2);
END IF;

IF pc_curp_benef3 IS NOT NULL AND pc_curp_benef3[1] <> " " THEN
   -- se obtiene sexo del beneficiario 3
   LET pc_sexo = pc_curp_benef3[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   INSERT INTO afi_beneficiario
        VALUES (pc_nss,
                pd_n_folio,
                ps_tipo_solicitud,
                ps_parentesco_benef3,
                pc_paterno_benef3,
                pc_materno_benef3,
                pc_nombre_benef3,
                '',
                ls_sexo,
                pd_porcentaje_benef3,
                3,
                pc_curp_benef3);
END IF;

IF pc_curp_benef4 IS NOT NULL AND pc_curp_benef4[1] <> " " THEN
   -- se obtiene sexo del beneficiario 4
   LET pc_sexo = pc_curp_benef4[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   INSERT INTO afi_beneficiario
        VALUES (pc_nss,
                pd_n_folio,
                ps_tipo_solicitud,
                ps_parentesco_benef4,
                pc_paterno_benef4,
                pc_materno_benef4,
                pc_nombre_benef4,
                '',
                ls_sexo,
                pd_porcentaje_benef4,
                4,
                pc_curp_benef4);
END IF;

IF pc_curp_benef5 IS NOT NULL AND pc_curp_benef5[1] <> " " THEN
   -- se obtiene sexo del beneficiario 5
   LET pc_sexo = pc_curp_benef5[11];

   IF pc_sexo = 'H' THEN
      LET ls_sexo = 1;
   ELIF pc_sexo = 'M' THEN
      LET ls_sexo = 2;
   END IF

   INSERT INTO afi_beneficiario
        VALUES (pc_nss,
                pd_n_folio,
                ps_tipo_solicitud,
                ps_parentesco_benef5,
                pc_paterno_benef5,
                pc_materno_benef5,
                pc_nombre_benef5,
                '',
                ls_sexo,
                pd_porcentaje_benef5,
                5,
                pc_curp_benef5);
   
END IF;

EXECUTE PROCEDURE sp_act_status_movil(ls_cod_rech,ld_registro, lc_desc_rechazo);

RETURN  ls_cod_rech    ,
        lc_desc_rechazo;
END PROCEDURE;