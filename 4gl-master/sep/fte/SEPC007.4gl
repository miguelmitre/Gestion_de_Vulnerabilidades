################################################################################
#Owner             => E.F.P                                                    #
#Programa SEPC007  => RECIBE RESPUESTA OP 27                                   #
#Fecha creacion    => 12 DE ABRIL DEL 2005                                     #
#Por               => JESUS YAÑEZ MORENO                                       #
#Sistema           => SEP                                                      #
################################################################################
#Actualizacion     => Se agrega log de errores. 06Jun2012 Alejandro Chagoya    #
################################################################################
#Actualizacion     => Se agrega funcion para insertar en sep_bitacora          #
#                  => un historial de los cambios de estado que se hacen       #
#                  => en sep_det_reg_sol_reclamante                            #
#Autor             => Alejandro Chagoya Salazar                                #
#Fecha             => 14 Marzo 2013                                            #
#Requerimiento     => INV-1872                                                 #
################################################################################
#Actualizacion     => Se valida que solo cuando esta la solicitud en eestado 4 #
#                  => actualiza a los estado 52 o 53                           #
#Autor             => Alejandro Chagoya Salazar                                #
#Fecha             => 04 Junio 2013                                            #
#Requerimiento     => INV-2115                                                 #
################################################################################
#Modificacion      => Se actualizan registros improcedentes                    #
#Autor             => Alejandro Chagoya Salazar    => 29 Octubre 2014          #
#Requerimiento     => INV-2797                                                 #
################################################################################
#Modificacion      => Se hace actualizacion de la cuo                          #
#                     registros que no tengan E.I. se rechazan con estado 7    #
#Autor             => Cristian Morales Roblero  Fecha : 02 de marzo 2015       #
#Requerimiento     => INV-3169 MLM-3028 CPL-1878                               #
################################################################################

DATABASE safre_af
GLOBALS

   DEFINE reg_cza_separa RECORD
          tipo_registro       CHAR(2),
          ident_servicio      CHAR(2),
          ident_operacion     CHAR(2),
          tipo_ent_origen     CHAR(2),
          cve_ent_origen      CHAR(3),
          tipo_ent_destino    CHAR(2),
          cve_ent_destino     CHAR(3),
          fecha_lote          DATE,
          consec_lote         SMALLINT,
          resulta_operacion   CHAR(2),
          diag_1              CHAR(3),
          diag_2              CHAR(3),
          diag_3              CHAR(3),
          estado              smallint
   END RECORD

   DEFINE reg_det_separa RECORD
          tipo_registro         CHAR(2),
          cont_servicio         INTEGER,
          nss_asociado          CHAR(11),
          tipo_entidad_nss_involucrado char(02),
          clave_entidad_involucrado char(03),
          resulta_operacion     CHAR(2),
          diag_proceso1         CHAR(3),
          diag_proceso2         CHAR(3),
          diag_proceso3         CHAR(3)
   END RECORD

   DEFINE reg_sum_separa RECORD
          tipo_registro         CHAR(02),
          total_detalle_02      INTEGER ,
          total_detalle_03      INTEGER
   END RECORD

   DEFINE
          carga_reg              CHAR(330),
          usuario                VARCHAR(20),
          enter                  CHAR(001),
          motivo_rechazo_1       CHAR(003),
          motivo_rechazo_2       CHAR(003),
          motivo_rechazo_3       CHAR(003),
          resulta_oper           CHAR(002),
          generar                CHAR(020),
          nombre_archivo         CHAR(020),
          archivo_separa         CHAR(200),
          c10_fecha_presenta     CHAR(010),
          c10_fecha_nac_sep      CHAR(010),
          c10_fecha_afilia       CHAR(010),
           c10_fecha_marca       CHAR(010)

   DEFINE s_recibido,
          s_rechazado,
          cont_det   ,
          cuantos   ,
          cont                   SMALLINT

   DEFINE ultimo_folio           INTEGER
   DEFINE gr_modulo              RECORD LIKE seg_modulo.*
   DEFINE gs_afore               SMALLINT
   DEFINE gc_comando             CHAR(200)

END GLOBALS

 GLOBALS "SEPRPTS.4gl"

DEFINE m_estado_ant INTEGER,
       m_edo_impro  SMALLINT,        #improcedente
       m_cadena     CHAR(300)
################################################################################
MAIN

DEFINE m_log VARCHAR(30)

  OPTIONS
  PROMPT LINE LAST
  DEFER INTERRUPT

LET m_log = ""
CALL init()
LET m_log = usuario CLIPPED, ".SEPC007.log"
LET m_log = m_log CLIPPED

CALL STARTLOG(m_log)

   WHENEVER ERROR CONTINUE
      DROP TABLE sep_plano
      DROP TABLE tmp_rpt_exp

      CREATE TEMP TABLE sep_plano
      (
      n_registros          CHAR(300)
      )

      CREATE TEMP TABLE tmp_rpt_exp # MODIFICACION DE LA CUO
      (
       tipo_reg            SMALLINT,
       nss                 CHAR(11),
       n_folio             DECIMAL(10,0),
       tipo_solicitud      SMALLINT,
       correlativo         SERIAL,
       expediente          CHAR(2)
       )
       IF SQLCA.SQLCODE != 0 THEN
         ERROR "LA TABLA TMP_RPT_EXP NO FUE CREADA"
         SLEEP 2
         EXIT PROGRAM
      END IF

   WHENEVER ERROR STOP

   OPEN WINDOW sepc0011 AT 2,2 WITH FORM "SEPC0011" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Iniciar [Ctrl-C]  Salir                                                        " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " SEPC007              RECIBE RESPUESTA OPERACION 27                                   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS
      BEFORE FIELD generar
         LET generar = NULL
         CLEAR FORM

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "CAMPO NO PUEDE SER NULO"
            NEXT FIELD generar
         END IF

         SELECT   nombre
         INTO     nombre_archivo
         FROM     sep_ctr_archivo
         WHERE    nombre = generar

         IF STATUS <> NOTFOUND THEN
            ERROR " ESTE ARCHIVO YA SE RECIBIO "
            NEXT FIELD generar
         END IF

      WHENEVER ERROR CONTINUE
      SELECT *
      INTO   g_seg_modulo.*
      FROM   seg_modulo
      WHERE  modulo_cod = "sep"

      LET archivo_separa = g_seg_modulo.ruta_rescate CLIPPED,"/",
            generar CLIPPED

      LOAD FROM archivo_separa DELIMITER "+"
      INSERT INTO sep_plano

      SELECT   count(*)
      INTO     cuantos
      FROM     sep_plano

      IF cuantos = 0 THEN
         DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
         AT 19,2 ATTRIBUTE(REVERSE)
         SLEEP 2
         NEXT FIELD generar
      ELSE
         EXIT INPUT
      END IF
      WHENEVER ERROR STOP

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
   END INPUT
   DISPLAY " PROCESANDO INFORMACION " AT 20,1 ATTRIBUTE(REVERSE)

   CALL lee_archivo_plano() #lap
   CALL asigna_globales()

   SELECT 'X' #MODIFICACION DE LA CUO
   FROM tmp_rpt_exp
   GROUP BY 1

   IF SQLCA.SQLCODE = 0 THEN #MODIFICACION DE LA CUO
      CALL f_reporte()
   END IF

   PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
   FOR CHAR enter

   CLOSE WINDOW sepc0011
END MAIN
################################################################################
FUNCTION init()

   LET HOY  = TODAY

   SELECT   estado
            ,USER
   INTO     s_recibido
            ,usuario
   FROM     sep_estado_separacion
   WHERE    des_estado = "RECIBIDO OP 27"

   SELECT   estado
   INTO     s_rechazado
   FROM     sep_estado_separacion
   WHERE    des_estado = "RECHAZO OP 27"

   SELECT   estado
   INTO     m_edo_impro
   FROM     sep_estado_separacion
   WHERE    des_estado
   MATCHES "*HISTORICO IMPROCEDEN*"

   SELECT *
   INTO   gr_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'sep'

   SELECT   codigo_afore
   INTO     gs_afore
   FROM     safre_af:tab_afore_local
   GROUP BY 1

   LET   m_cadena =  " "
   LET   m_cadena =  "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?)"
   LET   m_cadena =  m_cadena CLIPPED
   PREPARE pre_desmarca FROM m_cadena
   LET   m_cadena = "EXECUTE FUNCTION fn_verifica_expediente (?,?,?)" #MODIFICACION DE LA CUO
   LET   m_cadena =  m_cadena CLIPPED
   PREPARE pre_verifica FROM m_cadena

END FUNCTION
################################################################################
FUNCTION lee_archivo_plano()

   DECLARE cur_1 CURSOR FOR
   SELECT   *
   FROM     sep_plano

   INITIALIZE carga_reg TO NULL
   LET cont    = 0
   FOREACH cur_1 INTO carga_reg
      INITIALIZE reg_det_separa.* TO NULL
      DISPLAY " TOTAL REGISTROS PROCESADOS  : ",cont     AT 12,8
      #---Encabezado por lote ---#
      IF carga_reg[1,2] = "01" THEN
         CALL carga_cza() #cc
      END IF

      #---Detalle de nss---#
      IF carga_reg[1,2] = "02" THEN
         IF carga_det02_nss() = 1 THEN    #INV-2797
            CONTINUE FOREACH
         END IF
      END IF

      IF carga_reg[1,2] = "03" THEN
         LET cont = cont + 1
         CALL carga_det_nss()  #cdn
      END IF
      INITIALIZE carga_reg TO NULL
   END FOREACH

END FUNCTION
################################################################################
FUNCTION carga_cza()
   DEFINE ls_folio            SMALLINT

      LET c10_fecha_presenta = carga_reg[021,022],"/",
                               carga_reg[023,024],"/",
                               carga_reg[017,020]
      LET reg_cza_separa.fecha_lote       = c10_fecha_presenta

      LET reg_cza_separa.ident_operacion  = carga_reg[005,006]
   IF gs_afore = 568 THEN  #afore copel
      LET ls_folio = " "
   ELSE
      LET ls_folio = 1
   END IF

      INSERT INTO sep_ctr_archivo
      VALUES      (generar,                         #nombre
                   ls_folio,                               #folio
                   reg_cza_separa.fecha_lote,       #fecha_recepcion
                   reg_cza_separa.ident_operacion)  #tipo_archivo
END FUNCTION

################################################################################
FUNCTION carga_det_nss()

DEFINE l_diag_confronta       CHAR(2),
       l_clasifica_separacion CHAR(1),
       lidSolicitudSeparacion INTEGER

   INITIALIZE l_diag_confronta, l_clasifica_separacion,
           lidSolicitudSeparacion TO NULL
   LET reg_det_separa.tipo_registro                    = carga_reg[001,002]

   IF reg_det_separa.tipo_registro                     = '03' THEN
      LET reg_det_separa.cont_servicio                 = carga_reg[003,012]
      LET reg_det_separa.nss_asociado                  = carga_reg[013,023]
      LET reg_det_separa.tipo_entidad_nss_involucrado  = carga_reg[024,025]
      LET reg_det_separa.clave_entidad_involucrado     = carga_reg[026,028]
      LET reg_det_separa.resulta_operacion             = carga_reg[029,030]
      LET reg_det_separa.diag_proceso1                 = carga_reg[031,033]
      LET reg_det_separa.diag_proceso2                 = carga_reg[034,036]
      LET reg_det_separa.diag_proceso3                 = carga_reg[037,039]
   END IF

   IF reg_det_separa.nss_asociado  IS NULL
      OR reg_det_separa.nss_asociado = ""
      OR LENGTH(reg_det_separa.nss_asociado CLIPPED ) != 11 THEN
      RETURN
   END IF

   INITIALIZE ultimo_folio TO NULL
   SELECT   a.folio
            , a.correlativo
   INTO     ultimo_folio
            , lidSolicitudSeparacion
   FROM     sep_det_reg_sol_reclamante a
   WHERE    a.nss = reg_det_separa.nss_asociado
   AND      a.estado in (4,6)

   IF SQLCA.SQLCODE = 0 THEN #modificacion cuo
      INSERT INTO sep_det03_solicitud
      VALUES(ultimo_folio,
             reg_det_separa.*,
             lidSolicitudSeparacion)
   END IF

   IF gs_afore = 568 THEN #MODIFICACION DE LA CUO
      UPDATE  sep_det_reg_sol_reclamante
       SET     estado = s_recibido
       WHERE   correlativo = lidSolicitudSeparacion
       AND     estado = 4

   ELSE
      SELECT   "ok"
      FROM     afi_mae_afiliado a
      WHERE    a.n_seguro  =  reg_det_separa.nss_asociado
      AND      a.tipo_solicitud <> 5
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         SELECT   "ok"
         FROM     cta_act_marca a
         WHERE    a.nss = reg_det_separa.nss_asociado
         AND      a.marca_cod = 120
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN

            #modificacion cuo
            SELECT   estado
            INTO     m_estado_ant
            FROM     safre_af:sep_det_reg_sol_reclamante
            WHERE    correlativo = lidSolicitudSeparacion
            #modificacion cuo

             UPDATE   sep_det_reg_sol_reclamante
            SET      estado = s_recibido
            WHERE    correlativo = lidSolicitudSeparacion
            AND      estado = 4
            IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
               CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, s_recibido)
            END IF
         ELSE
            SELECT   a.diag_confronta  ,
                     a.clasifica_separacion
            INTO     l_diag_confronta ,
                     l_clasifica_separacion
            FROM     sep_det_solicitud  a
            WHERE    a.idSolicitudSeparacion = lidSolicitudSeparacion

            IF (l_diag_confronta = "01") THEN
               IF (l_clasifica_separacion = "B") THEN
                  # INV-1872 INI                                 --#
                  LET m_estado_ant= NULL

                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------
                  #INV-1872 FIN                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 53
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------

                  IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
                     CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 53)
                  END IF

               END IF
               IF (l_clasifica_separacion = "C") THEN
                  # INV-1872 INI                                 --#
                  LET m_estado_ant= NULL

                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#
                  #INV-1872 INI                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 52
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#

                  IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
                     CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 52)
                  END IF

               END IF
               IF (l_clasifica_separacion = "D") THEN

                  # INV-1872 INI                                 --#
                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#
                  #   INV-1872 FIN                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 52
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND     estado = 4 #-----------------INV-2115-----------------#

                  IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
                     CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 52)
                  END IF

               END IF
            ELSE

               #INV-1872 INI                                 --#
               SELECT   estado
               INTO     m_estado_ant
               FROM     safre_af:sep_det_reg_sol_reclamante
               WHERE    correlativo = lidSolicitudSeparacion
               #INV-1872 FIN                                 --#

               UPDATE   sep_det_reg_sol_reclamante
               SET      estado = 5
               WHERE    correlativo = lidSolicitudSeparacion
               AND      estado <> 7    # cuo

               IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
                  CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 5)
               END IF
            END IF
         END IF
      ELSE

         SELECT   estado
         INTO     m_estado_ant
         FROM     safre_af:sep_det_reg_sol_reclamante
         WHERE    correlativo = lidSolicitudSeparacion

          UPDATE  sep_det_reg_sol_reclamante
          SET     estado = s_recibido
          WHERE   correlativo = lidSolicitudSeparacion
          AND     estado = 4

         IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
            CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, s_recibido)
         END IF
      END IF
   END IF

END FUNCTION
################################################################################
FUNCTION carga_det02_nss()
   DEFINE l_nss CHAR(11)

   DEFINE l02_n_seguro           CHAR(011)
   DEFINE lidSolicitudSeparacion INTEGER
   DEFINE l_diagnostico          CHAR(2)
   DEFINE ld_folio               DECIMAL(10,0)
   DEFINE ls_solicitud           SMALLINT

   INITIALIZE l02_n_seguro, lidSolicitudSeparacion,l_nss, l_diagnostico TO NULL

   LET l02_n_seguro                     = carga_reg[013,023]
   LET reg_det_separa.resulta_operacion = carga_reg[261,262]
   LET reg_det_separa.diag_proceso1     = carga_reg[263,265]
   LET reg_det_separa.diag_proceso2     = carga_reg[266,268]
   LET reg_det_separa.diag_proceso3     = carga_reg[269,271]
   LET l_diagnostico = carga_reg[257,258]

#Se saca select y update a sep_det_solicitud del IF
#ACS 06062012

   SELECT   a.correlativo, a.nss
   INTO     lidSolicitudSeparacion, l_nss
   FROM     sep_det_reg_sol_reclamante a
   WHERE    a.n_seguro = l02_n_seguro
   AND      a.estado IN (4,7)   -- enviado Modificacion cuo

   UPDATE sep_det_solicitud
   SET   resultado_operacion     = reg_det_separa.resulta_operacion ,
         diag_proc1              = reg_det_separa.diag_proceso1     ,
         diag_proc2              = reg_det_separa.diag_proceso2     ,
         diag_proc3              = reg_det_separa.diag_proceso3
   WHERE idSolicitudSeparacion   = lidSolicitudSeparacion

#INV-2797 INI
   IF l_diagnostico = "02" THEN
      UPDATE sep_det_reg_sol_reclamante
      SET    estado    = m_edo_impro
      WHERE  n_seguro  = l02_n_seguro
      AND    correlativo = lidSolicitudSeparacion

      IF gs_afore != 568 THEN
         IF SQLCA.SQLERRD[3] = 1 THEN
            CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, 4, m_edo_impro)
         END IF

         CALL f_desmarca_cuenta(l02_n_seguro)   #desmarca invadido
         CALL f_desmarca_cuenta(l_nss)          #desmarca asociado
      END IF
      RETURN 1
   END IF

#INV-2797 fin

   CASE reg_det_separa.resulta_operacion #modificacion cuo
      WHEN "02"

         --IF  reg_det_separa.resulta_operacion = "02" THEN
         #-- INV-1872 INI                                 --#
         SELECT   estado
         INTO     m_estado_ant
         FROM     safre_af:sep_det_reg_sol_reclamante
         WHERE    correlativo = lidSolicitudSeparacion
         #-- INV-1872 FIN                                 --#
         UPDATE   sep_det_reg_sol_reclamante
         SET      estado    = s_rechazado
         WHERE    n_seguro  = l02_n_seguro
         AND      correlativo = lidSolicitudSeparacion

         IF gs_afore != 568 THEN
            IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
               CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, s_rechazado)
            END IF
         END IF
      WHEN "01" #modificacion cuo inicio
         IF lidSolicitudSeparacion IS NULL THEN
            RETURN 1
         ELSE

            SELECT   n_folio
                     ,tipo_solicitud
            INTO     ld_folio
                     ,ls_solicitud
            FROM     afi_mae_afiliado
            WHERE    n_seguro  = l02_n_seguro

            IF f_verifica(l02_n_seguro,ld_folio,ls_solicitud) = TRUE THEN

               SELECT   estado
               INTO     m_estado_ant
               FROM     safre_af:sep_det_reg_sol_reclamante
               WHERE    correlativo = lidSolicitudSeparacion

               UPDATE sep_det_reg_sol_reclamante
               SET estado = 4
               WHERE correlativo = lidSolicitudSeparacion
               AND estado = 7


               IF gs_afore != 568 THEN
                  IF SQLCA.SQLERRD[3] = 1 THEN
                        CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 4)
                  END IF
               END IF

               UPDATE afi_motivo_expediente
               SET   fecha_envio    = NULL
               WHERE nss            = l02_n_seguro
               AND   n_folio        =  ld_folio
               AND   tipo_solicitud =  ls_solicitud

               IF SQLCA.SQLERRD[3] = 1 THEN
                  INSERT INTO tmp_rpt_exp
                  VALUES (1,l02_n_seguro,ld_folio,ls_solicitud,lidSolicitudSeparacion,"SI")
               END IF

            ELSE
               SELECT   estado
               INTO     m_estado_ant
               FROM     safre_af:sep_det_reg_sol_reclamante
               WHERE    correlativo = lidSolicitudSeparacion

               UPDATE   sep_det_reg_sol_reclamante
               SET      estado    = 7
               WHERE    n_seguro  = l02_n_seguro
               AND      correlativo = lidSolicitudSeparacion

               IF gs_afore = 568 THEN
                   IF SQLCA.SQLERRD[3] = 1 THEN
                     INSERT INTO tmp_rpt_exp
                  VALUES (2,l02_n_seguro,ld_folio,ls_solicitud,lidSolicitudSeparacion,"NO")
                  END IF
               ELSE
                  IF SQLCA.SQLERRD[3] = 1 THEN
                     CALL f_inserta_sep_bitacora(lidSolicitudSeparacion, 27, m_estado_ant, 7)
                     INSERT INTO tmp_rpt_exp
                  VALUES (2,l02_n_seguro,ld_folio,ls_solicitud,lidSolicitudSeparacion,"NO")
                  END IF
               END IF

               RETURN 1
            END IF
         END IF
   END CASE#modificacion cuo fin
   --END IF

RETURN 0
END FUNCTION

################################################################################
FUNCTION asigna_globales()
#-------------------------
#== ASIGNACION DE VARIABLES QUE SE UTILIZA EL PROGRAMA SEPREPGRAL.4gl===

   LET  g_tabname      =  "sep_det_solicitud a"
   LET  g_tabname_1    =  "sep_det03_solicitud b"
   LET  g_nombre_prog  =  "SEPC007"
   LET  g_tipo_desc1   =  "REPORTE DE RESPUESTA OP(27)"
   let g_total_cuentas = cont

END FUNCTION

######################################################################--INV-1872
FUNCTION f_inserta_sep_bitacora(l_id, l_operacion, l_estado_anterior, l_estado_actualiza)

DEFINE l_id                 INTEGER,
       l_operacion          SMALLINT,
       l_estado_anterior    SMALLINT,
       l_estado_actualiza   SMALLINT,
       l_programa           CHAR(15)

LET l_programa = ARG_VAL(0)

INSERT INTO safre_af:sep_bitacora
VALUES (
         l_id
         ,l_operacion
         ,l_estado_anterior
         ,l_estado_actualiza
         ,CURRENT
         ,l_programa
         ,USER)

END FUNCTION

######################################################################--INV-2797
FUNCTION f_desmarca_cuenta(p_nss)
   DEFINE p_nss CHAR(11)
   DEFINE lr_act  RECORD LIKE safre_af:cta_act_marca.*
   DEFINE lr_marca RECORD
          nss            CHAR(11),
          marca_entra    SMALLINT,
          correlativo    INTEGER,
          estado         SMALLINT,
          marca_causa    SMALLINT,
          usuario        CHAR(08)
     END RECORD

   INITIALIZE lr_act.*, lr_marca.* TO NULL

   SELECT * INTO lr_act.*
   FROM cta_act_marca
   WHERE nss = p_nss
   AND marca_cod = 280   #marca de separacion

   IF SQLCA.SQLCODE = 100 THEN   # no esta marcada
      RETURN
   END IF

   LET lr_marca.nss          = p_nss
   LET lr_marca.marca_entra  = 280
   LET lr_marca.correlativo  = lr_act.correlativo
   LET lr_marca.estado       = 30
   LET lr_marca.marca_causa  = 280
   LET lr_marca.usuario      = usuario

   EXECUTE pre_desmarca USING lr_marca.*

END FUNCTION
################################################################################
FUNCTION f_verifica(lc_nss,ld_folio,ls_solicitud)

   DEFINE
      lc_nss                  CHAR(11)
      ,ld_folio               DECIMAL(10,0)
      ,ls_solicitud
      ,ls_res_exp
      ,ls_res_ref             SMALLINT

   EXECUTE pre_verifica USING lc_nss ,ld_folio,ls_solicitud
   INTO  ls_res_exp
         ,ls_res_ref

   IF ls_res_exp = 1  THEN
      RETURN TRUE
   END IF

RETURN FALSE

END FUNCTION
################################################################################
FUNCTION f_reporte()

  DEFINE
      lc_archivo_det       CHAR(300)                        #archivo detalle
      ,lc_archivo_cza      CHAR(300)                        #archivo encabezado
      ,li_folio            INTEGER                          #folio

   #se genera archivo para el detalle
   LET lc_archivo_det  =   gr_modulo.ruta_listados CLIPPED,"/",
                           usuario CLIPPED,"sep"

   #se genera archivo para el encabezado
   LET lc_archivo_cza  =   gr_modulo.ruta_listados CLIPPED, "/",
                           usuario CLIPPED,
                           ".SEP_OP_27.",
                           HOY USING "yymmdd",".txt"
   #se genera el archivo directo de la tabla
   UNLOAD TO lc_archivo_det
   SELECT   nss
            ,n_folio
            ,tipo_solicitud
            ,correlativo
            ,expediente
   FROM tmp_rpt_exp
   ORDER BY 1
   #se llama el reporte de la generacion del encabezado
   START REPORT rpt_cza TO lc_archivo_cza
      OUTPUT TO REPORT rpt_cza()
   FINISH REPORT rpt_cza
   #se concatena en un solo archivo el encabezado y el detalle
   LET gc_comando = "cat ", lc_archivo_det     CLIPPED, " >> ",
                            lc_archivo_cza     CLIPPED
   RUN gc_comando
   #se borra el archivo generado del detalle
   LET gc_comando = "rm ", lc_archivo_det CLIPPED
   RUN gc_comando
   # se informa el termino del proceso y la ruta del archivo y nnombre
   OPEN WINDOW aviso AT 13,16   WITH 3 ROWS, 54 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY "ARCHIVO GENERADO" AT 1,2
   DISPLAY lc_archivo_cza CLIPPED AT 2,2
   PROMPT "ENTER PARA CONTINUAR" FOR CHAR ENTER
   CLOSE WINDOW aviso

END FUNCTION
################################################################################
REPORT rpt_cza()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
   PRINT
      'NSS'             ,'|'
     ,'N_folio'         ,'|'
     ,'Tipo_solicitud'  ,'|'
     ,'Correlativo'     ,'|'
     ,'Expediente'
END REPORT