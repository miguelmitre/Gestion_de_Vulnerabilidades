#+ Objetivo    => Programa de reverso del archivo de fechas
# Proyecto     => AFORES ( MEXICO )
# Versión      => SAFRE2
# Descripción  => Realizara el reverso del archivo de fecha de notificación de los casos
#                 historicos de las resoluciones de negativa de pensión recibidas por archivo plano.
--============================================================================================

DATABASE safre_af

GLOBALS

   DEFINE g_modulo               RECORD LIKE seg_modulo.*

   DEFINE g_fec_archivo          DATE

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

   CALL STARTLOG(FGL_GETENV("USER")||".RETR736.log")
   CALL fn_init()

   CALL fn_reverso_archivo()

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

#+ Objetivo     => Función que realiza el reverso del archivo de fechas notifica
#+ Descripción  => Realizara el reverso del ultimo archivo cargado en el sistema,
#+                 borrando los registros insertados en la tabla concentradora y
#+                 recuperara el impacto Datamart.
#+
FUNCTION fn_reverso_archivo()

   DEFINE v_count_insert          ,
          v_count_resp            INTEGER

    -- -----------------------------------------------------------------------------

   OPEN WINDOW retr7361 AT 2,2 WITH FORM "RETR7361" ATTRIBUTE(BORDER)
   DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETR736            REVERSO DE ARCHIVO DE FECHAS NOTIFICA                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY g_hoy USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

      CALL fn_tabla_tmp()
      
      LET v_count_insert = 0
      LET v_count_resp   = 0
      
      INPUT BY NAME g_fec_archivo WITHOUT DEFAULTS
         BEFORE FIELD g_fec_archivo
            LET g_fec_archivo = NULL
      
         AFTER FIELD g_fec_archivo
            -- Se valida que la fecha no sea nula
            IF g_fec_archivo IS NULL THEN
               ERROR "   CAMPO NO PUEDE SER NULO  "
               NEXT FIELD g_fec_archivo
            END IF
            
            -- Se valida que la fecha no sea mayor al día de hoy
            IF g_fec_archivo > g_hoy THEN
               ERROR "   LA FECHA NO PUEDE SER MAYOR AL DIA DE HOY  "
               NEXT FIELD g_fec_archivo
            END IF
      
            LET g_ruta_archivo = g_modulo.ruta_rescate CLIPPED,"/RESPALDO_RESOLUCIONES.UNL"
      
            WHENEVER ERROR CONTINUE
      
               LOAD FROM g_ruta_archivo
               INSERT INTO tmp_datamart_respaldo
      
            WHENEVER ERROR STOP
      
            -- Se valida que encuentre información en el archivo respaldado
            SELECT COUNT(*)
              INTO v_count_resp
              FROM tmp_datamart_respaldo
      
            IF v_count_resp = 0 THEN
               ERROR "  NO HAY REGISTROS PARA REVERSAR  "
               NEXT FIELD g_fec_archivo
            END IF

            -- Se valida que existan registro en la tabla concentradora con la fecha dada
            SELECT NVL(COUNT(*), 0)
              INTO v_count_insert
              FROM ret_datamart_do
             WHERE DATE(fec_carga_datamart) = g_fec_archivo
               AND estado = 5
               
            IF v_count_insert = 0 THEN
               ERROR "   NO SE ENCONTRARON REGISTROS CON LA FECHA PROPORCIONADA  "
               NEXT FIELD g_fec_archivo
            END IF
               
            -- Se valida que el total de registros insertados en la tabla concentradora
            -- sea igual a los respaldados
            IF v_count_insert <> v_count_resp THEN
               ERROR "   LOS REGISTROS A REVERSAR NO CORRESPONDEN CON EL ULTIMO ARCHIVO CARGADO  "
               NEXT FIELD g_fec_archivo
            ELSE
               -- Si el total es igual se valida que la cuentas respaldadas sean las que se insertaron
               IF fn_valida_reverso() = 0 THEN
                  ERROR "   LOS REGISTROS A REVERSAR NO CORRESPONDEN CON EL ULTIMO ARCHIVO CARGADO  "
                  NEXT FIELD g_fec_archivo
               END IF
            END IF

            WHILE TRUE
               PROMPT " ESTA SEGURO S/N ? " FOR CHAR g_enter
               IF g_enter MATCHES "[sSnN]" THEN
                  IF g_enter MATCHES "[sS]" THEN

                     CALL fn_reversa_registros()
                     
                     PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR g_enter

                     EXIT INPUT
                  ELSE
                     PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR g_enter
                     EXIT INPUT
                  END IF
               END IF
            END WHILE

         ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
      END INPUT

   CLEAR SCREEN
   CLOSE WINDOW retr7361

END FUNCTION

#+ Objetivo     => Función que valida los registros respaldados
#+ Descripción  => Valida que los registros respaldados exitan en la tabla concentradora.
#+
#+ @returnType SMALLINT
#+ @return Retorna un 0 cuando algun registro respaldado no exista en la tabla concentradora
#+
FUNCTION fn_valida_reverso()

   DEFINE v_datos   RECORD LIKE ret_det_datamart.*
   DEFINE v_existe  SMALLINT
   
   INITIALIZE v_datos.* TO NULL
   LET v_existe = 1

   DECLARE cur_registros CURSOR FOR
   SELECT *
     FROM tmp_datamart_respaldo

   FOREACH cur_registros INTO v_datos.*

      -- Valida que exista en la tabla concentradora con la fecha capturada
      SELECT "OK"
        FROM ret_datamart_do
       WHERE nss         = v_datos.nss
         AND sec_pension = v_datos.sec_pension
         AND DATE(fec_carga_datamart) = g_fec_archivo
         AND estado = 5
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         LET v_existe = 0
         EXIT FOREACH
      END IF

      INITIALIZE v_datos.* TO NULL

   END FOREACH

   RETURN v_existe

END FUNCTION

#+ Objetivo     => Función que crea la tabla temporal
#+ Descripción  => Crea la tabla temporal que se usara en el proceso.
#+
FUNCTION fn_tabla_tmp()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_datamart_respaldo
   WHENEVER ERROR STOP

   SELECT *
    FROM   ret_det_datamart
    WHERE  1 = 0
    INTO TEMP tmp_datamart_respaldo

END FUNCTION

#+ Objetivo     => Función que realiza el reverso de los registros
#+ Descripción  => Recupera las resoluciones y elimina los registros de la tabla concentradora.
#+
FUNCTION fn_reversa_registros()

   DEFINE v_datos   RECORD LIKE ret_det_datamart.*
   DEFINE v_count   INTEGER
   
   INITIALIZE v_datos.* TO NULL
   LET v_count = 0

   DECLARE cur_reverso CURSOR FOR
   SELECT *
     FROM tmp_datamart_respaldo

   FOREACH cur_reverso INTO v_datos.*

      -- Elimina registro de la tabla concentradora
      DELETE FROM ret_datamart_do
       WHERE nss         = v_datos.nss
         AND sec_pension = v_datos.sec_pension
         AND DATE(fec_carga_datamart) = g_fec_archivo
         AND estado = 5

      UPDATE ret_det_datamart
         SET regimen         = v_datos.regimen,
             tipo_seguro     = v_datos.tipo_seguro,
             tipo_pension    = v_datos.tipo_pension,
             tipo_prestacion = v_datos.tipo_prestacion,
             clave_pension   = v_datos.clave_pension
       WHERE nss               = v_datos.nss
         AND sec_pension       = v_datos.sec_pension
         AND diag_datamart     = v_datos.diag_datamart
         AND fecha_carga_afore = v_datos.fecha_carga_afore
      
      LET v_count = v_count + 1

      INITIALIZE v_datos.* TO NULL

   END FOREACH

   DISPLAY "REGISTROS REVERSADOS          : ", v_count
            USING "<<<,<<&" AT 10,09

END FUNCTION
