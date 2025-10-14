#+ Objetivo    => Programa de carga del archivo de fechas
# Proyecto     => AFORES ( MEXICO )
# Versión      => SAFRE2
# Descripción  => Realizara la carga de un archivo que actualizara la fecha de notificación de los casos
#                 historicos de las resoluciones de negativa de pensión recibidas por archivo plano.
--============================================================================================

DATABASE safre_af

GLOBALS

   DEFINE g_modulo               RECORD LIKE seg_modulo.*

   DEFINE g_nom_archivo          CHAR(24)

   DEFINE g_usuario              CHAR(012),
          g_ruta_archivo         CHAR(200),
          g_enter                CHAR(001)

   DEFINE g_codigo_afore         SMALLINT

   DEFINE g_hoy                  DATE

END GLOBALS

MAIN
   DEFER INTERRUPT
   OPTIONS
   INPUT WRAP           ,
   PROMPT LINE LAST     ,
   ACCEPT KEY CONTROL-I

   CALL STARTLOG(FGL_GETENV("USER")||".RETE736.log")
   CALL fn_init()

   CALL fn_carga_archivo()

END MAIN


#+ Objetivo     => Función que inicializa variables globales
#+ Descripción  => Inicializa las variables globales que se usaran en el programa
#+
FUNCTION fn_init()

  LET g_hoy  = TODAY

   SELECT *
     INTO g_modulo.*
     FROM seg_modulo
    WHERE modulo_cod = "ret"

   ----- CODIGOS AFORES -----
   SELECT codigo_afore    ,
          USER
     INTO g_codigo_afore ,
          g_usuario
     FROM tab_afore_local

END FUNCTION

#+ Objetivo     => Función que carga el archivo
#+ Descripción  => Captura el nombre del archivo y busca en la ruta de rescate si existe.
#+
FUNCTION fn_carga_archivo()

   DEFINE v_num_reg               ,
          v_num_rech              INTEGER
          
   DEFINE v_nom_arch_rech         CHAR(150)

    -- -----------------------------------------------------------------------------

   OPEN WINDOW rete7361 AT 2,2 WITH FORM "RETE7361" ATTRIBUTE(BORDER)
   DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETE736            CARGA DE ARCHIVO DE FECHAS NOTIFICA                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY g_hoy USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

      CALL fn_tablas_tmp()
      
      LET v_num_reg   = 0
      LET v_num_rech  = 0
      
      INPUT BY NAME g_nom_archivo WITHOUT DEFAULTS
         BEFORE FIELD g_nom_archivo
            LET g_nom_archivo = NULL
      
         AFTER FIELD g_nom_archivo
            IF g_nom_archivo IS NULL THEN
               ERROR "   CAMPO NO PUEDE SER NULO  "
               NEXT FIELD g_nom_archivo
            END IF
            
            IF g_nom_archivo <> 'RET_DET_FEC_NOTIFICA.UNL' THEN
               ERROR "   NOMBRE DE ARCHIVO INCORRECTO  "
               NEXT FIELD g_nom_archivo
            END IF
      
            LET g_ruta_archivo = g_modulo.ruta_rescate CLIPPED,"/",
                                 g_nom_archivo CLIPPED
      
            WHENEVER ERROR CONTINUE
      
               LOAD FROM g_ruta_archivo
               INSERT INTO tmp_archivo_carga
      
            WHENEVER ERROR STOP
      
            SELECT COUNT(*)
              INTO v_num_reg
              FROM tmp_archivo_carga
      
            IF v_num_reg = 0 THEN
               ERROR "  EL ARCHIVO ESTÁ VACIO  "
               NEXT FIELD g_nom_archivo
            ELSE
               WHILE TRUE
                  PROMPT " ESTA SEGURO S/N ? " FOR CHAR g_enter
                  IF g_enter MATCHES "[sSnN]" THEN
                     IF g_enter MATCHES "[sS]" THEN
      
                        CALL fn_valida_archivo()
                        
                        CALL fn_actualiza_resolucion()
      
                        SELECT NVL(COUNT(*), 0)
                          INTO v_num_rech
                          FROM tmp_archivo_rechazados
                          
                        IF v_num_rech > 0 THEN -- hubo errores en la carga se genera archivo de rechazos.
                           LET v_nom_arch_rech = g_modulo.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".FEC_NOTIFICA_RECH.",g_hoy USING "YYYYMMDD"
                           
                           UNLOAD TO v_nom_arch_rech
                           SELECT *
                             FROM tmp_archivo_rechazados
                           
                           DISPLAY "REPORTE DE RECHAZOS : " AT 10,5
                           DISPLAY v_nom_arch_rech CLIPPED AT 11,5
                        END IF
                        
                        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR g_enter
      
                        EXIT INPUT
                     ELSE
                        PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR g_enter
                        EXIT INPUT
                     END IF
                  END IF
               END WHILE
            END IF
      
         ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
      END INPUT

   CLEAR SCREEN
   CLOSE WINDOW rete7361

END FUNCTION

#+ Objetivo     => Función que crea las tablas temporales
#+ Descripción  => Crea las tablas temporales que se usaran en el proceso.
#+
FUNCTION fn_tablas_tmp()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_archivo_carga
      DROP TABLE tmp_archivo_aceptados
      DROP TABLE tmp_archivo_rechazados
      DROP TABLE tmp_datamart_respaldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_archivo_carga
   (
    nss               CHAR(11),
    curp              CHAR(18),
    sec_pension       CHAR(02),
    tipo_retiro       CHAR(01),
    regimen           CHAR(02),
    tipo_seguro       CHAR(02),
    tipo_pension      CHAR(02),
    tipo_prestacion   CHAR(02),
    cve_pension       CHAR(02),
    fecha_notifica    CHAR(10)
   )

   CREATE TEMP TABLE tmp_archivo_aceptados
   (
    nss               CHAR(11),
    curp              CHAR(18),
    sec_pension       CHAR(02),
    tipo_retiro       CHAR(01),
    regimen           CHAR(02),
    tipo_seguro       CHAR(02),
    tipo_pension      CHAR(02),
    tipo_prestacion   CHAR(02),
    cve_pension       CHAR(02),
    fecha_notifica    DATE
   )

   CREATE TEMP TABLE tmp_archivo_rechazados
   (
    nss               CHAR(11),
    curp              CHAR(18),
    sec_pension       CHAR(02),
    tipo_retiro       CHAR(01),
    regimen           CHAR(02),
    tipo_seguro       CHAR(02),
    tipo_pension      CHAR(02),
    tipo_prestacion   CHAR(02),
    cve_pension       CHAR(02),
    fecha_notifica    CHAR(10),
    descripcion       CHAR(100)
   )
   
   SELECT *
    FROM   ret_det_datamart
    WHERE  1 = 0
    INTO TEMP tmp_datamart_respaldo

END FUNCTION

#+ Objetivo     => Función que valida el archivo
#+ Descripción  => Ejecuta las validaciones del archivo que se cargo.
#+
FUNCTION fn_valida_archivo()

   DEFINE v_datos RECORD
      nss               CHAR(11),
      curp              CHAR(18),
      sec_pension       CHAR(02),
      tipo_retiro       CHAR(01),
      regimen           CHAR(02),
      tipo_seguro       CHAR(02),
      tipo_pension      CHAR(02),
      tipo_prestacion   CHAR(02),
      cve_pension       CHAR(02),
      fecha_notifica    CHAR(10)
   END RECORD

   DEFINE v_cont             INTEGER

   INITIALIZE v_datos.* TO NULL

   --- Valida registro ---

   DECLARE cur_registros CURSOR FOR
   SELECT *
     FROM tmp_archivo_carga

   FOREACH cur_registros INTO v_datos.*

      -- Valida que exista resolución
      SELECT "OK"
        FROM ret_det_datamart
       WHERE nss         = v_datos.nss
         AND sec_pension = v_datos.sec_pension
         AND tipo_retiro = 'D'
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         CALL fn_inserta_rechazados(v_datos.*, 'No existe resolución de negativa de pensión.')
         CONTINUE FOREACH
      END IF

      --- Valida registros duplicado ---
      SELECT "OK"
        FROM tmp_archivo_aceptados
       WHERE nss = v_datos.nss
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         CALL fn_inserta_rechazados(v_datos.*, 'El registro se encuentra duplicado en el archivo.')
         CONTINUE FOREACH
      END IF

      -- Se valida la fecha de notificación
      IF v_datos.fecha_notifica IS NULL THEN
         CALL fn_inserta_rechazados(v_datos.*, 'La fecha de notificación es nula.')
         CONTINUE FOREACH
      END IF

      -- Valida que no exista mas de una resolución con el mismo nss, secuencia, diagnostico y fecha carga afore
      SELECT NVL(COUNT(*), 0)
        INTO v_cont
        FROM ret_det_datamart
       WHERE nss         = v_datos.nss
         AND sec_pension = v_datos.sec_pension
         AND tipo_retiro = 'D'

      IF v_cont > 1 THEN

         SELECT NVL(COUNT(*), 0)
           INTO v_cont
           FROM ret_det_datamart
          WHERE nss           = v_datos.nss
            AND sec_pension   = v_datos.sec_pension
            AND tipo_retiro   = 'D'
            AND diag_datamart = 101

         IF v_cont > 1 THEN

            SELECT NVL(COUNT(*), 0)
              INTO v_cont
              FROM ret_det_datamart
             WHERE nss           = v_datos.nss
               AND sec_pension   = v_datos.sec_pension
               AND tipo_retiro   = 'D'
               AND diag_datamart = 101
               AND fecha_carga_afore = (SELECT UNIQUE MAX(fecha_carga_afore)
                                          FROM ret_det_datamart
                                         WHERE nss           = v_datos.nss
                                           AND sec_pension   = v_datos.sec_pension
                                           AND tipo_retiro   = 'D'
                                           AND diag_datamart = 101)

            IF v_cont > 1 THEN
               CALL fn_inserta_rechazados(v_datos.*, 'Existe mas de una resolución con la misma secuencia de pensión, diagnostico y fecha carga afore.')
               CONTINUE FOREACH
            END IF
         END IF

      END IF

      CALL fn_inserta_aceptados(v_datos.*)

   END FOREACH

END FUNCTION

#+ Objetivo     => Función que inserta los registros rechazados
#+ Descripción  => Inserta los registros rechazados en la tabla temporal.
#+
FUNCTION fn_inserta_rechazados(p_datos, p_descripcion)

   DEFINE p_datos RECORD
      nss               CHAR(11),
      curp              CHAR(18),
      sec_pension       CHAR(02),
      tipo_retiro       CHAR(01),
      regimen           CHAR(02),
      tipo_seguro       CHAR(02),
      tipo_pension      CHAR(02),
      tipo_prestacion   CHAR(02),
      cve_pension       CHAR(02),
      fecha_notifica    CHAR(10)
   END RECORD
   DEFINE p_descripcion       CHAR(100)
   
   INSERT INTO tmp_archivo_rechazados VALUES(p_datos.*, p_descripcion)

END FUNCTION

#+ Objetivo     => Función que inserta los registros aceptados
#+ Descripción  => Inserta los registros aceptados en la tabla temporal.
#+
FUNCTION fn_inserta_aceptados(p_datos)

   DEFINE p_datos RECORD
      nss               CHAR(11),
      curp              CHAR(18),
      sec_pension       CHAR(02),
      tipo_retiro       CHAR(01),
      regimen           CHAR(02),
      tipo_seguro       CHAR(02),
      tipo_pension      CHAR(02),
      tipo_prestacion   CHAR(02),
      cve_pension       CHAR(02),
      fecha_notifica    CHAR(10)
   END RECORD

   DEFINE v_aceptados RECORD
      nss               CHAR(11),
      curp              CHAR(18),
      sec_pension       CHAR(02),
      tipo_retiro       CHAR(01),
      regimen           CHAR(02),
      tipo_seguro       CHAR(02),
      tipo_pension      CHAR(02),
      tipo_prestacion   CHAR(02),
      cve_pension       CHAR(02),
      fecha_notifica    DATE
   END RECORD
   
   DEFINE v_fecha       CHAR(10)
   
   LET v_aceptados.nss               = p_datos.nss
   LET v_aceptados.curp              = p_datos.curp
   LET v_aceptados.sec_pension       = p_datos.sec_pension
   LET v_aceptados.tipo_retiro       = p_datos.tipo_retiro
   LET v_aceptados.regimen           = p_datos.regimen
   LET v_aceptados.tipo_seguro       = p_datos.tipo_seguro
   LET v_aceptados.tipo_pension      = p_datos.tipo_pension
   LET v_aceptados.tipo_prestacion   = p_datos.tipo_prestacion
   LET v_aceptados.cve_pension       = p_datos.cve_pension
   LET v_fecha                       = p_datos.fecha_notifica[4,5],"/",
                                       p_datos.fecha_notifica[1,2],"/",
                                       p_datos.fecha_notifica[7,10]
   LET v_aceptados.fecha_notifica    = v_fecha
   
   INSERT INTO tmp_archivo_aceptados VALUES(v_aceptados.*)

END FUNCTION

#+ Objetivo     => Función que actualiza la fecha de notificación
#+ Descripción  => Actualiza el impacto datamart con la información cargada en el archivo
#+                 e inserta un registro en la tabla concentradora de las resoluciones datamart
#+                 que llegan por el BUS.
#+
FUNCTION fn_actualiza_resolucion()

   DEFINE v_datamart      RECORD LIKE ret_det_datamart.*
   DEFINE v_datamart_do   RECORD LIKE ret_datamart_do.*

   DEFINE v_aceptados RECORD
      nss               CHAR(11),
      curp              CHAR(18),
      sec_pension       CHAR(02),
      tipo_retiro       CHAR(01),
      regimen           CHAR(02),
      tipo_seguro       CHAR(02),
      tipo_pension      CHAR(02),
      tipo_prestacion   CHAR(02),
      cve_pension       CHAR(02),
      fecha_notifica    DATE
   END RECORD
   
   DEFINE v_cont               SMALLINT
   DEFINE v_nom_arch_respaldo  CHAR(150)
   
   INITIALIZE v_aceptados.* TO NULL

   DECLARE cur_aceptados CURSOR FOR
   SELECT *
     FROM tmp_archivo_aceptados

   FOREACH cur_aceptados INTO v_aceptados.*

      INITIALIZE v_datamart.*, v_datamart_do.* TO NULL
      
      SELECT NVL(COUNT(*), 0)
        INTO v_cont
        FROM ret_det_datamart
       WHERE nss         = v_aceptados.nss
         AND sec_pension = v_aceptados.sec_pension
         AND tipo_retiro = 'D'

      IF v_cont > 1 THEN

         SELECT NVL(COUNT(*), 0)
           INTO v_cont
           FROM ret_det_datamart
          WHERE nss           = v_aceptados.nss
            AND sec_pension   = v_aceptados.sec_pension
            AND tipo_retiro   = 'D'
            AND diag_datamart = 101

         IF v_cont > 1 THEN

            SELECT *
              INTO v_datamart.*
              FROM ret_det_datamart
             WHERE nss               = v_aceptados.nss
               AND sec_pension       = v_aceptados.sec_pension
               AND tipo_retiro       = 'D'
               AND diag_datamart     = 101
               AND fecha_carga_afore = (SELECT UNIQUE MAX(fecha_carga_afore)
                                          FROM ret_det_datamart
                                         WHERE nss           = v_aceptados.nss
                                           AND sec_pension   = v_aceptados.sec_pension
                                           AND tipo_retiro   = 'D'
                                           AND diag_datamart = 101)
         ELSE
            SELECT *
              INTO v_datamart.*
              FROM ret_det_datamart
             WHERE nss           = v_aceptados.nss
               AND sec_pension   = v_aceptados.sec_pension
               AND tipo_retiro   = 'D'
               AND diag_datamart = 101
         END IF

      ELSE
         SELECT *
           INTO v_datamart.*
           FROM ret_det_datamart
          WHERE nss         = v_aceptados.nss
            AND sec_pension = v_aceptados.sec_pension
            AND tipo_retiro = 'D'
      END IF
      
      -- Se inserta en la tabla temporal la resolución a respaldar
      INSERT INTO tmp_datamart_respaldo VALUES (v_datamart.*)

      -- Actualiza resolución
      LET v_datamart.regimen         = v_aceptados.regimen
      LET v_datamart.tipo_seguro     = v_aceptados.tipo_seguro
      LET v_datamart.tipo_pension    = v_aceptados.tipo_pension
      LET v_datamart.tipo_prestacion = v_aceptados.tipo_prestacion
      LET v_datamart.clave_pension   = v_aceptados.cve_pension

      UPDATE ret_det_datamart
         SET regimen         = v_datamart.regimen,
             tipo_seguro     = v_datamart.tipo_seguro,
             tipo_pension    = v_datamart.tipo_pension,
             tipo_prestacion = v_datamart.tipo_prestacion,
             clave_pension   = v_datamart.clave_pension
       WHERE nss               = v_datamart.nss
         AND sec_pension       = v_datamart.sec_pension
         AND diag_datamart     = v_datamart.diag_datamart
         AND fecha_carga_afore = v_datamart.fecha_carga_afore


      -- Se inserta el registro en la tabla concentradora 
      INITIALIZE v_datamart_do.* TO NULL

      LET v_datamart_do.folio_t_procesar    = v_datamart.folio_t_procesar
      LET v_datamart_do.ent_origen          = v_datamart.entidad_origen
      LET v_datamart_do.tipo_tramite        = v_datamart.tipo_tramite
      LET v_datamart_do.nss                 = v_datamart.nss
      LET v_datamart_do.curp                = v_datamart.curp
      LET v_datamart_do.num_issste          = v_datamart.num_issste
      LET v_datamart_do.nombre_datamart     = v_datamart.nombre_datamart
      LET v_datamart_do.nombre_afore        = v_datamart.nombre_afore
      LET v_datamart_do.paterno_afore       = v_datamart.paterno_afore
      LET v_datamart_do.materno_afore       = v_datamart.materno_afore
      LET v_datamart_do.sec_pension         = v_datamart.sec_pension
      LET v_datamart_do.tipo_movimiento     = v_datamart.tipo_movimiento
      LET v_datamart_do.regimen             = v_datamart.regimen
      LET v_datamart_do.tipo_retiro         = v_datamart.tipo_retiro
      LET v_datamart_do.tipo_seguro         = v_datamart.tipo_seguro
      LET v_datamart_do.tipo_pension        = v_datamart.tipo_pension
      LET v_datamart_do.tipo_prestacion     = v_datamart.tipo_prestacion
      LET v_datamart_do.cve_pension         = v_datamart.clave_pension
      LET v_datamart_do.art_negativa        = v_datamart.art_negativa
      LET v_datamart_do.frac_negativa       = v_datamart.frac_negativa
      LET v_datamart_do.num_considerando    = v_datamart.num_considerando
      LET v_datamart_do.fecha_ini_pen       = v_datamart.fecha_ini_pen
      LET v_datamart_do.fec_ini_pago        = v_datamart.fec_ini_pago
      LET v_datamart_do.fecha_resolucion    = v_datamart.fecha_resolucion
      LET v_datamart_do.porcentaje_val      = v_datamart.porcentaje_val
      LET v_datamart_do.clave_aseguradora   = v_datamart.clave_aseguradora
      LET v_datamart_do.semanas_cotizadas   = v_datamart.semanas_cotizadas
      LET v_datamart_do.fec_carga_datamart  = CURRENT
      LET v_datamart_do.diag_datamart       = v_datamart.diag_datamart
      LET v_datamart_do.estado_sub_viv      = v_datamart.estado_sub_viv
      LET v_datamart_do.monto_sol_imss      = v_datamart.monto_sol_imss
      LET v_datamart_do.transf_previa       = v_datamart.transf_previa
      LET v_datamart_do.consec_procesar     = v_datamart.num_consec_procesar
      LET v_datamart_do.tot_resol_porcesar  = v_datamart.total_resol_procesar
      LET v_datamart_do.usuario             = g_usuario
      LET v_datamart_do.fecha_recep         = v_aceptados.fecha_notifica
      LET v_datamart_do.estado              = 5
      
      INSERT INTO ret_datamart_do VALUES(v_datamart_do.*)

      INITIALIZE v_aceptados.* TO NULL
   END FOREACH
   
   LET v_nom_arch_respaldo = g_modulo.ruta_rescate CLIPPED, "/RESPALDO_RESOLUCIONES.UNL"
                           
   UNLOAD TO v_nom_arch_respaldo
   SELECT *
     FROM tmp_datamart_respaldo

END FUNCTION
