#******************************************************************************#
#Proyecto     => safre_af                                                      #
#Propietario  => E.F.P.                                                        #
#Programa     => CTAB035                                                       #
#Descripcion  => MULTISIEFORES                                                 #
#Fecha        => 26 de septiembre de 2007.                                     #
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ                                   #
#Sistema      => CTA.                                                          #
#Fecha        => 27 de Febrero de 2007                                         #
#******************************************************************************#

DATABASE safre_tmp
################################################################################
GLOBALS
   DEFINE gc_mensaje1           ,
          gc_mensaje2           ,
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(8)  ,
          gc_respuesta          CHAR(001)

   DEFINE g_cancela       SMALLINT

   DEFINE gd_fecha_corte,
          gd_fecha_valuacion  DATE

   DEFINE arr_c,
          arr_l,
          arr_t      SMALLINT,
          GUSER       CHAR(08),
          hoy        DATE,
          pos        SMALLINT,
          pos_subcta SMALLINT

   DEFINE gi_siefore,
          gi_estatus,
          gi_siefore_ced,
          gi_siefore_rec SMALLINT

   DEFINE gi_folio_liq INTEGER

   DEFINE gar_consulta_liq ARRAY[400] OF RECORD
             folio            INTEGER,
             subcuenta        SMALLINT,
             siefore_ced      SMALLINT,
             pesos_ced        DECIMAL(22,6),
             acciones_ced     DECIMAL(22,6),
             siefore_rec      SMALLINT,
             pesos_rec        DECIMAL(22,6),
             acciones_rec     DECIMAL(22,6)
      END RECORD

   DEFINE gr_reporte RECORD
  	        afore_desc CHAR(14),
            precio1    DECIMAL(7,6),
            precio2    DECIMAL(7,6),
            precio3    DECIMAL(7,6),
            precio4    DECIMAL(7,6),
            precio5    DECIMAL(7,6),
            siefore1   CHAR(9),
            siefore2   CHAR(9),
            siefore3   CHAR(9),
            siefore4   CHAR(9),
            siefore5   CHAR(9)
   END RECORD

   DEFINE gar_regimen ARRAY[20] OF RECORD
          codigo_siefore SMALLINT,
          desc_siefore   CHAR(30),
          total          INTEGER
   END RECORD

END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT USER
   INTO   GUSER
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY
   CALL proceso()
END MAIN
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB0351" ATTRIBUTE(BORDER)
           #-----------------------------------------------------------------------------
   DISPLAY "CTAB035            CORTE   MULTISIEFORES          < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "CORTE MULTISIEFORES"
      COMMAND "Preparacion" "Preparacion de Corte Transversal"
      MENU "Preparacion"
         COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
            CALL lee_paso(1)
         COMMAND "Genera Saldos" "Genera saldos para el corte multisiefore."
            CALL lee_paso(2)
         COMMAND "Prepara NSS" "Identifica NSS del periodo"
            CALL lee_paso(3)
         COMMAND "Regresar" "Regresar al menu anterior"
            EXIT MENU
      END MENU

      COMMAND "Consultas" "Consultas de los pasos ejecutados"
      MENU "Consultas"
         COMMAND "Consulta" "Consulta de saldos para corte multisiefore"
            CALL consulta(1)
         COMMAND "Consulta Estimada" "Consulta estimada de saldos"
            CALL consulta(2)
         COMMAND "Consulta Liquidacion" "Consulta liquidacion de saldos"
            CALL consulta_liquidacion()
         COMMAND "Consulta Regimen" "Consulta el regimen"
            CALL consulta_regimen()
         COMMAND "Regresar" "Regresar al menu anterior"
            EXIT MENU
      END MENU

      COMMAND "Archivos" "Generación de archivos del corte"
      MENU "Archvios"
        COMMAND "Archivo 2 siefores" "Genera reporte del corte multisiefore"
           CALL lee_paso(7)
        COMMAND "Archivo 5 siefores" "Genera reporte del corte multisiefore"
           CALL lee_paso(8)
        COMMAND "Reporte de acciones a transferir" "Genera reporte de acciones a transferir antes del corte"
           CALL lee_paso(10)
        COMMAND "Anexo B" "Validación de Cuentas de Trabajadores"
           CALL anexo_b()
        COMMAND "Regresar" "Regresar al menu anterior"
           EXIT MENU
      END MENU
      
      COMMAND "Aplicar corte" "Aplicar corte en la cuenta individual"
      MENU "Aplicar corte"
         COMMAND "Liquidacion" "Realiza la liquidacion del corte"
            CALL lee_paso(4)            
         COMMAND "Registro de transfrencia de siefore"
         COMMAND "Regresar" "Regresar al menu anterior"
           EXIT MENU
      END MENU

      COMMAND "Regimen" "Actualizacion de Regimen de acuerdo a la edad"
      MENU "Regimen"
         COMMAND "Prepara Regimen" "Prepara la actualización del régimen"
            CALL lee_paso(5)
         COMMAND "Actualiza Regimen" "Realiza la actualización del régimen"
            CALL lee_paso(6)
         COMMAND "Actualización de indicador edad"
         COMMAND "Regresar" "Regresar al menu anterior"
           EXIT MENU
      END MENU

      COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
         CALL revisa_proceso()
         
      COMMAND "Elimina ejecucion" "Elimina la ejecución de la fecha indicada"
         CALL elimina_ejecucion(9)

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION lee_paso(li_paso)

   DEFINE li_paso  SMALLINT,
          lc_opc   CHAR(1)

   CASE li_paso
   	  WHEN 1
   	     LET gc_mensaje1 = "ESTE PROCESO ELIMINARA LA INFORMACION DE LA GENERACION",
                           "DEL ULTIMO CORTE MULTISIEFORE."
         LET gc_mensaje4 = "INDIQUE FECHA DE VALUACION:"
      WHEN 2
      	 LET gc_mensaje1 = "ESTE PROCESO IDENTIFICARA LOS SALDOS DE LA FECHA INDICADA",
                           "LA INFORMACIÓN DEL CORTE MULTISIEFORE ANTERIOR SE BORRARÁ."
      WHEN 3
      	 LET gc_mensaje1 = "ESTE PROCESO IDENTIFICARA LOS NSS DE LA FECHA INDICADA",
                           "LA INFORMACIÓN DEL CORTE MULTISIEFORE ANTERIOR SE BORRARÁ."
      WHEN 4
      	 LET gc_mensaje1 = "ESTE PROCESO REALIZARA LA LIQUIDACION DE LA FECHA INDICADA"
      WHEN 5
      	 LET gc_mensaje1 = "ESTE PROCESO PREPARA LA ACTUALIZACION DEL REGIMEN"
      WHEN 6
      	 LET gc_mensaje1 = "ESTE PROCESO REALIZA LA ACTUALIZACION DEL REGIMEN"
      WHEN 10
      	 LET gc_mensaje1 = "ESTE PROCESO GENERA REPORTE ESTIMADO DE ACCIONES"
   END CASE

   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"
   LET gc_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"
   LET g_cancela = FALSE

   INITIALIZE gd_fecha_corte TO NULL
   INITIALIZE gd_fecha_valuacion TO NULL
   --LET gd_fecha_corte = "10/31/2007"

   IF li_paso = 1 THEN
      INPUT BY NAME gd_fecha_corte,
      	            gd_fecha_valuacion WITHOUT DEFAULTS
         BEFORE INPUT
            DISPLAY BY NAME gc_mensaje1,
                            gc_mensaje2

         AFTER FIELD gd_fecha_corte
            IF gd_fecha_corte IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE CORTE"
               SLEEP 2
               ERROR ""
               NEXT FIELD gd_fecha_corte
            ELSE
            	  --VERIFICA SI YA SE HA EJECUTADO LA INICIALIACION PARA LA FECHA CORTE
               IF valida_proceso(li_paso,gd_fecha_corte) != 0 THEN
                  NEXT FIELD gd_fecha_corte
               ELSE
               	 --VERIFICAR QUE SEA ÚLTIMO DÍA DEL MES
               	 {IF valida_dia(gd_fecha_corte) != 0 THEN
               	    NEXT FIELD gd_fecha_corte
               	 END IF}
               END IF
            END IF

         BEFORE FIELD gd_fecha_valuacion
            DISPLAY BY NAME gc_mensaje4
         AFTER FIELD gd_fecha_valuacion
            IF gd_fecha_valuacion IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE VALUACION"
               SLEEP 2
               ERROR ""
               NEXT FIELD gd_fecha_valuacion
            ELSE
            	  --VERIFICA PRECIO DE ACCION
            	  SELECT "X"
            	  FROM   safre_af:glo_valor_accion
            	  WHERE  fecha_valuacion = gd_fecha_valuacion
            	  GROUP BY 1

            	  IF SQLCA.SQLCODE <> 0 THEN
            	  	 ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
            	  	 SLEEP 2
                   ERROR ""
                   NEXT FIELD gd_fecha_valuacion
            	  END IF
            END IF

         WHILE TRUE
            PROMPT "ESTA SEGURO DE CONTINUAR EJECUCION S/N:" FOR CHAR lc_opc

            IF lc_opc  MATCHES "[SsNn]" THEN
               IF lc_opc MATCHES "[Ss]" THEN
               	 CALL ejecuta_paso(li_paso)
               	 SLEEP 2
                  ERROR ""
                  CLEAR FORM
               	 EXIT INPUT
               ELSE
                  ERROR "PROCESO CANCELADO."
                  SLEEP 2
                  ERROR ""
                  CLEAR FORM
                  EXIT INPUT
               END IF
            ELSE
            	  ERROR "SOLO INDIQUE S o N "
               SLEEP 2
               ERROR ""
               CONTINUE WHILE
            END IF
         END WHILE

         ON KEY (CONTROL-C,INTERRUPT)
           ERROR "PROCESO CANCELADO"
           SLEEP 2
           ERROR ""
           CLEAR FORM
           EXIT INPUT
      END INPUT
   ELSE
   	  INPUT BY NAME gd_fecha_corte WITHOUT DEFAULTS
         BEFORE INPUT
            DISPLAY BY NAME gc_mensaje1,
                            gc_mensaje2

         AFTER FIELD gd_fecha_corte
            IF gd_fecha_corte IS NULL THEN
               ERROR "DEBE INDICAR FECHA DE CORTE"
               SLEEP 2
               ERROR ""
               NEXT FIELD gd_fecha_corte
            ELSE
            	  --VERIFICA SI YA SE HA EJECUTADO LA INICIALIACION PARA LA FECHA CORTE
               IF valida_proceso(li_paso,gd_fecha_corte) != 0 THEN
                  NEXT FIELD gd_fecha_corte
               ELSE
               	 --VERIFICAR QUE SEA ÚLTIMO DÍA DEL MES
{               	 IF valida_dia(gd_fecha_corte) != 0 THEN
               	    NEXT FIELD gd_fecha_corte
               	 END IF}
               END IF
            END IF

         WHILE TRUE
            PROMPT "ESTA SEGURO DE CONTINUAR EJECUCION S/N:" FOR CHAR lc_opc

            IF lc_opc  MATCHES "[SsNn]" THEN
               IF lc_opc MATCHES "[Ss]" THEN
               	 CALL ejecuta_paso(li_paso)
               	 SLEEP 2
                  ERROR ""
                  CLEAR FORM
               	 EXIT INPUT
               ELSE
                  ERROR "PROCESO CANCELADO."
                  SLEEP 2
                  ERROR ""
                  CLEAR FORM
                  EXIT INPUT
               END IF
            ELSE
            	  ERROR "SOLO INDIQUE S o N "
               SLEEP 2
               ERROR ""
               CONTINUE WHILE
            END IF
         END WHILE

         ON KEY (CONTROL-C,INTERRUPT)
           ERROR "PROCESO CANCELADO"
           SLEEP 2
           ERROR ""
           CLEAR FORM
           EXIT INPUT
      END INPUT
   END IF
END FUNCTION
################################################################################
FUNCTION inserta_paso(li_paso, ld_fecha_corte, lc_resultado)

   DEFINE li_paso          SMALLINT,
          ld_fecha_corte   DATE,
          ld_fecha_inicio  DATE,
          lc_hora_inicio   CHAR(8),
          lc_resultado     CHAR(80),
          li_folio_proceso INTEGER

   LET ld_fecha_inicio = TODAY
   LET lc_hora_inicio  = TIME

   SELECT "X"
   FROM safre_af:cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   safre_af:cta_folio

      UPDATE safre_af:cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO safre_af:cta_folio
      VALUES (li_folio_proceso)
   END IF

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES (ld_fecha_corte,          -- fecha_proceso
           "CTAB035",               -- proceso_cod
           li_paso,                 -- etapa_cod    -- LECTURA
           lc_hora_inicio,          -- hora_inicial
           NULL,                    -- hora_final
           ld_fecha_inicio,         -- parametro1   --fecha_inicio
           NULL,                    -- parametro2   --fecha_fin
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           gd_fecha_valuacion,      -- parametro5   --fecha_valuacion
           li_folio_proceso,        -- folio
           lc_resultado,            -- resultado
           GUSER,                    -- usuario
           0                        -- consecutivo
          )
END FUNCTION
################################################################################
FUNCTION finaliza_paso(li_paso, ld_fecha_corte, lc_resultado)

   DEFINE li_paso        SMALLINT,
          ld_fecha_corte DATE,
          ld_fecha_fin   DATE,
          lc_hora_fin    CHAR(8),
          lc_resultado   CHAR(80)

   LET ld_fecha_fin = TODAY
   LET lc_hora_fin  = TIME

   UPDATE safre_af:dis_ctrl_proceso
   SET    hora_final = lc_hora_fin,
          parametro2 = ld_fecha_fin,
          usuario    = GUSER,
          resultado  = lc_resultado
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTAB035"
END FUNCTION
################################################################################
FUNCTION borra_paso(li_paso, ld_fecha_corte)
   DEFINE li_paso        SMALLINT,
          ld_fecha_corte DATE

   DELETE
   FROM   safre_af:dis_ctrl_proceso
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTAB035"
END FUNCTION
################################################################################
FUNCTION borra_informacion()
   DEFINE li_resultado SMALLINT

   DEFINE lr_glo_valor_accion RECORD
   	           codigo_siefore     SMALLINT,
               precio_del_dia     DECIMAL(19,14),
               fecha_operacion    DATE,
               fecha_valuacion    DATE,
               monto_comis_saldos DECIMAL(17,2),
               acc_circ_cap_fijo  DECIMAL(21,4),
               acc_circ_res_esp   DECIMAL(21,4),
               acc_circ_inv_perm  DECIMAL(21,4),
               acc_circ_traba     DECIMAL(21,4),
               acc_circ_traba_vol DECIMAL(21,4),
               acc_circ_inv_afore DECIMAL(21,4),
               acc_circ_totales   DECIMAL(21,4)
          END RECORD

   DEFINE li_folio,
          li_siefore_ced,
          li_siefore_rec    INTEGER

   DEFINE li_cont    SMALLINT
   DEFINE lc_comando CHAR(400)

   WHENEVER ERROR CONTINUE
      DROP TABLE safre_tmp:cta_cuota_multisie_nss
      DROP TABLE safre_tmp:cta_estatus_saldos
      DROP TABLE safre_tmp:cta_estatus_total_sie

      DROP TABLE safre_tmp:cta_estatus_saldos_est
      DROP TABLE safre_tmp:cta_estatus_total_sie_est

      DROP TABLE safre_tmp:glo_valor_accion
      DROP TABLE safre_tmp:tmp_saldo_corte_multisie

      DROP TABLE safre_tmp:tmp_nss_liquidados
      DROP TABLE safre_tmp:tmp_folio_corte

      --DROP TABLE safre_tmp:tmp_cta_regimen
      --DROP TABLE safre_tmp:tmp_cta_nss_regimen
   WHENEVER ERROR STOP

   CREATE TABLE safre_tmp:cta_cuota_multisie_nss
  (nss              CHAR(11),
   tipo_solicitud   SMALLINT,
   curp             CHAR(18),
   rfc              CHAR(13),
   fecha_nacimiento DATE,
   edad             SMALLINT,
   estatus_cuenta   SMALLINT,
   id_sector        SMALLINT,
   criterio         SMALLINT,
   siefore          SMALLINT,
   fecha_corte      DATE,
   factualiza       DATE,
   usuario          CHAR(18)
  )

  LET li_resultado = SQLCA.SQLCODE

  CREATE TABLE safre_tmp:cta_estatus_total_sie
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore           SMALLINT,
   total_nss         INTEGER,
   monto_en_acciones DECIMAL(22,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE safre_tmp:cta_estatus_saldos
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore           SMALLINT,
   subcuenta         SMALLINT,
   monto_en_acciones DECIMAL(16,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE safre_tmp:cta_estatus_total_sie_est
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore_ced       SMALLINT,
   siefore           SMALLINT,
   total_nss         INTEGER,
   monto_en_acciones DECIMAL(22,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE safre_tmp:cta_estatus_saldos_est
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore_ced       SMALLINT,
   siefore           SMALLINT,
   subcuenta         SMALLINT,
   monto_en_acciones DECIMAL(16,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE


  CREATE TABLE safre_tmp:glo_valor_accion
  (
    codigo_siefore     SMALLINT not null ,
    precio_del_dia     DECIMAL(19,14),
    fecha_operacion    DATE,
    fecha_valuacion    DATE not null ,
    monto_comis_saldos DECIMAL(17,2),
    acc_circ_cap_fijo  DECIMAL(21,4),
    acc_circ_res_esp   DECIMAL(21,4),
    acc_circ_inv_perm  DECIMAL(21,4),
    acc_circ_traba     DECIMAL(21,4),
    acc_circ_traba_vol DECIMAL(21,4),
    acc_circ_inv_afore DECIMAL(21,4),
    acc_circ_totales   DECIMAL(21,4)
  )
  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE safre_tmp:tmp_saldo_corte_multisie
  (
    nss               CHAR(11),
    subcuenta         SMALLINT,
    siefore           SMALLINT,
    fecha_conversion  DATE,
    precio_del_dia    DECIMAL(22,6),
    monto_en_acciones DECIMAL(22,6),
    monto_en_pesos    DECIMAL(22,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE safre_tmp:tmp_folio_corte
  (
    folio         INTEGER,
    siefore_ced   SMALLINT,
    siefore_recep SMALLINT)

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  --LET lc_comando = "dbaccess safre_tmp /safre/cta/fte/multi_sie/crea_tab_regimen_tmp.sql"
  --RUN lc_comando
  
  CREATE TABLE safre_tmp:tmp_nss_liquidados
  ( nss         CHAR(11),
    folio       INTEGER,
    subcuenta   SMALLINT,
    siefore_ced SMALLINT,
    siefore_rec SMALLINT
  )
  LET li_resultado = li_resultado + SQLCA.SQLCODE

#CARGA GLO_VALOR_ACCION
  INSERT INTO safre_tmp:glo_valor_accion
  SELECT *
  FROM   safre_af:glo_valor_accion
  WHERE  codigo_siefore NOT IN (3,4,5)
  AND    fecha_valuacion = gd_fecha_valuacion

  SELECT *
  INTO   lr_glo_valor_accion.*
  FROM   safre_tmp:glo_valor_accion
  WHERE  codigo_siefore = 2

  LET lr_glo_valor_accion.codigo_siefore = 3
  INSERT INTO safre_tmp:glo_valor_accion VALUES (lr_glo_valor_accion.*)

  LET lr_glo_valor_accion.codigo_siefore = 4
  INSERT INTO safre_tmp:glo_valor_accion VALUES (lr_glo_valor_accion.*)

  LET lr_glo_valor_accion.codigo_siefore = 5
  INSERT INTO safre_tmp:glo_valor_accion VALUES (lr_glo_valor_accion.*)

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  UPDATE safre_tmp:glo_valor_accion
  SET    fecha_valuacion = gd_fecha_corte

  --******************************************************************************
-- FOLIO DE LIQUIDACION PARA EL CORTE
--******************************************************************************

  FOR li_cont = 1 TO 10
   	  INSERT INTO safre_af:glo_folio VALUES ( 0 );
   	  SELECT MAX(folio)
   	  INTO   li_folio
   	  FROM   safre_af:glo_folio

   	  CASE li_cont
   	  WHEN 1
   	   	 LET li_siefore_ced = 1
   	   	 LET li_siefore_rec = 1
   	  WHEN 2
   	   	 LET li_siefore_ced = 1
   	   	 LET li_siefore_rec = 2
   	  WHEN 3
   	   	 LET li_siefore_ced = 1
   	   	 LET li_siefore_rec = 3
   	  WHEN 4
   	   	 LET li_siefore_ced = 1
   	   	 LET li_siefore_rec = 4
   	  WHEN 5
   	   	 LET li_siefore_ced = 1
   	   	 LET li_siefore_rec = 5
   	  WHEN 6
   	   	 LET li_siefore_ced = 2
   	   	 LET li_siefore_rec = 1
   	  WHEN 7
   	   	 LET li_siefore_ced = 2
   	   	 LET li_siefore_rec = 2
   	  WHEN 8
   	   	 LET li_siefore_ced = 2
   	   	 LET li_siefore_rec = 3
   	  WHEN 9
   	   	 LET li_siefore_ced = 2
   	   	 LET li_siefore_rec = 4
   	  WHEN 10
   	   	 LET li_siefore_ced = 2
   	   	 LET li_siefore_rec = 5
   	  END CASE

   	  INSERT INTO safre_tmp:tmp_folio_corte VALUES(li_folio,
   	                                               li_siefore_ced,
   	                                               li_siefore_rec
   	                                               )
  END FOR

  CREATE INDEX ix_tmp_folio_corte1 ON tmp_folio_corte(folio)
  UPDATE STATISTICS FOR TABLE tmp_folio_corte

  RETURN li_resultado
END FUNCTION
################################################################################
FUNCTION valida_proceso (li_paso, ld_fecha_corte)

   DEFINE li_paso         ,
          li_paso_ant     ,
          li_status       ,
          li_existe       SMALLINT

   DEFINE ld_fecha_corte  DATE,
          ld_fecha_fin   DATE

   LET li_paso_ant    = li_paso - 1
   LET li_status      = 0
   LET ld_fecha_fin   = NULL
   LET li_existe      = NULL

   IF li_paso = 4 OR      -- LIQUIDACION
   	  li_paso = 7 OR      -- ARCHIVO 2 SIEFORES
   	  li_paso = 8 OR      -- ARCHIVO 5 SIEFORES
   	  li_paso = 10 THEN   -- REPORTE ACCIONES ESTIMADO
   	  LET li_paso_ant = 3 -- PREPARACION DE NSS
   END IF

   IF li_paso = 9 THEN    --CONSULTA LIQUIDACION
   	  LET li_paso_ant = 4 --LIQUIDACION
   END IF

   IF li_paso > 1 THEN
       --  Verifica ejecucion del paso anterior
      SELECT etapa_cod, --paso
             parametro2 --fecha_fin
      INTO   li_existe,
             ld_fecha_fin
      FROM   safre_af:dis_ctrl_proceso
      WHERE  fecha_proceso = ld_fecha_corte
      AND    etapa_cod     = li_paso_ant
      AND    proceso_cod   = "CTAB035"

      IF li_existe IS NULL THEN
         ERROR "FALTA EJECUCION DEL PASO ANTERIOR. VERIFIQUE"
         SLEEP 2
         ERROR ""
         LET li_status = 1
      ELSE
         IF ld_fecha_fin IS NULL THEN
            ERROR "PROCESO ANTERIOR AUN NO FINALIZA."
            SLEEP 2
            ERROR ""
            LET li_status = 1
         END IF
      END IF
   END IF
   ---  Verifica si ya se ha ejecutado el proceso anteriormente
   SELECT etapa_cod,
          parametro2 --fecha_fin
   INTO   li_existe,
          ld_fecha_fin
   FROM   safre_af:dis_ctrl_proceso
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTAB035"

   IF li_existe = li_paso THEN
   	 IF ld_fecha_fin IS NULL THEN
   	 	  ERROR "PROCESO YA EN EJECUCION, PARA ESTA FECHA."
         SLEEP 2
         ERROR ""
         LET li_status = 1
   	 ELSE
   	 	  ERROR "PROCESO YA EJECUTADO, PARA ESTA FECHA."
         SLEEP 2
         ERROR ""
         LET li_status = 1
      END IF
   END IF

   RETURN li_status
END FUNCTION
################################################################################
FUNCTION valida_dia(ld_fecha_corte)
   DEFINE ld_fecha_corte,
          ld_mes_siguiente DATE,
          li_status        SMALLINT

   LET ld_mes_siguiente = MDY(MONTH(ld_fecha_corte) + 1, 1, YEAR(ld_fecha_corte))
   LET ld_mes_siguiente = ld_mes_siguiente - 1 UNITS DAY

   IF DAY (ld_mes_siguiente) != DAY(ld_fecha_corte) THEN
   	  ERROR "DEBE INTRODUCIR EL ÚLTIMO DÍA DEL MES"
      SLEEP 2
      ERROR ""
   	  LET li_status = 1
   ELSE
   	  LET li_status = 0
   END IF

   RETURN li_status
END FUNCTION
################################################################################
FUNCTION ejecuta_paso(li_paso)
   DEFINE li_paso          SMALLINT,
          lc_comando       CHAR(200),
          li_totnss        INTEGER,
          li_folio_proceso INTEGER,
          lc_msg           CHAR(400),
          lc_ok            CHAR(1)

   DEFINE lr_curp_rep      RECORD
   	         curp CHAR(18),
   	         num  SMALLINT
   	      END RECORD
   DEFINE lc_prioridad CHAR (100)

   CASE li_paso
   	  WHEN 1 --Inicializa
   	  	 CALL inserta_paso(li_paso, gd_fecha_corte, "Empieza Inicializacion")
   	  	 IF borra_informacion() = 0 THEN
            CALL finaliza_paso(li_paso, gd_fecha_corte, "Finaliza Inicializacion")
            ERROR "INICIALIZACION EFECTUADA."
         ELSE
            CALL borra_paso(li_paso, gd_fecha_corte)
            ERROR "NO SE PUDO INICIALIZAR, AVISE AL ADMINISTRADOR."
         END IF
      WHEN 2 --Genera Saldo Corte
      	 CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia generacion de saldos Multisiefore")
      	    LET lc_comando = "nohup time ./eje_saldo_multisie.sh ",
      	                     gd_fecha_corte," ",
                             GUSER CLIPPED,
                             " 1> multisie_saldo.salida ",
                             " 2> multisie_saldo.error &"
            ERROR "EJECUTANDO PROCESO POR NOHUP."
            SLEEP 2
            ERROR ""
            RUN   lc_comando
            ERROR "VERIFIQUE ARCHIVOS: multisie_saldo.salida Y multisie_saldo.error"
            SLEEP 3
      WHEN 3 --Identifica NSS
      	 --VERIFICA CURP REPETIDAS
      	 DECLARE cur_curp_rep CURSOR FOR
      	 SELECT curp, COUNT(*)
         FROM   safre_af:cta_ctr_reg_ind
         GROUP  BY 1
         HAVING COUNT(*) > 1

         INITIALIZE lr_curp_rep.* TO NULL

         FOREACH cur_curp_rep INTO lr_curp_rep.*
         	  IF lr_curp_rep.num IS NOT NULL THEN
         	  	 ERROR "EXISTEN CURPS REPETIDAS EN cta_ctr_reg_ind, NOTIFIQUE AL ADMINISTRADOR"
         	  	 SLEEP 3
         	  	 ERROR ""
         	  	 RETURN
         	  END IF
         END FOREACH

         CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Identificacion de NSS Multisiefore")
      	 LET lc_comando = "nohup time ./eje_multisie_nss.sh ",
      	                  gd_fecha_corte," ",
                          GUSER CLIPPED,
                          " 1> multisie_nss.salida ",
                          " 2> multisie_nss.error &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_nss.salida Y multisie_nss.error"
         SLEEP 3
         --END IF
      WHEN 4 --Liquidacion
         CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Liquidacion de NSS Multisiefore")
      	 LET lc_comando = "nohup time ./eje_liquida_multisie.sh ",
      	                  gd_fecha_corte," ",
                          GUSER CLIPPED,
                          " 1> multisie_liquida.salida ",
                          " 2> multisie_liquida.error &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_liquida.salida Y multisie_liquida.error"
         SLEEP 3
      WHEN 5 --Prepara Regimen
         CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Preparacion de Regimen")
      	 LET lc_comando = "nohup time fglgo CTAREGIMEN.4gi ",
                          " 1> multisie_prep_regimen.salida ",
                          " 2> multisie_prep_regimen.error &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_prep_regimen.salida Y multisie_prep_regimen.error"
         SLEEP 3
      WHEN 6 --Actualiza Regimen
         CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Actualizacion de Regimen")
      	 LET lc_comando = "nohup time dbaccess safre_af actualiza_regimen_inv.sql ",
                          " 1> multisie_act_regimen.salida ",
                          " 2> multisie_act_regimen.error &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_act_regimen.salida Y multisie_act_regimen.error"
         SLEEP 3
      WHEN 7 --Reporte con 2 Siefores
      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTAB036") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo CTAB036.4gi " CLIPPED,
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> multisie_rpt2S.salida ",
                           "2> multisie_rpt2S.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_rpt2S.salida Y multisie_rpt2S.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTAB036"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 8 --Reporte con 5 Siefores
      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTAB037") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo CTAB037.4gi " CLIPPED,
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> multisie_rpt5S.salida ",
                           "2> multisie_rpt5S.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: multisie_rpt5S.salida Y multisie_rpt5S.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTAB037"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 9 --Borra Ejecucion
      	 SELECT "X"
      	 FROM   safre_af:dis_ctrl_proceso
      	 WHERE  fecha_proceso = gd_fecha_corte
      	 AND    proceso_cod = "CTAB035"
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "NO EXISTE EJECUCIÓN PARA ESTA FECHA"
         ELSE
         	  ERROR "PROCESANDO INFORMACION"
      	    DELETE
      	    FROM  safre_af:dis_ctrl_proceso
      	    WHERE fecha_proceso = gd_fecha_corte
      	    AND   proceso_cod = "CTAB035"
            ERROR "EJECUCION ELIMINADA."
            SLEEP 2
            ERROR ""
         END IF
      WHEN 10 --Reporte Estimado de Acciones
      	 LET lc_comando = "fglgo CTAB039.4gi ",
                           gd_fecha_corte   CLIPPED
         ERROR "PROCESANDO INFORMACION"                  
         RUN lc_comando
         ERROR "LISTADO GENERADO"
         SLEEP 2
         ERROR ""         
         
   END CASE
END FUNCTION
################################################################################
FUNCTION revisa_proceso()
   LET gc_mensaje1 = "ESTA OPCION MUESTRA EL ESTADO DE EJECUCION DE LOS PASOS."
   LET gc_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

   INPUT BY NAME gd_fecha_corte

      BEFORE INPUT
         DISPLAY BY NAME gc_mensaje1,
                         gc_mensaje2

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 3
            ERROR ""
            NEXT FIELD gd_fecha_corte
         ELSE
            IF despliega_pasos(gd_fecha_corte) != 0 THEN
               ERROR "NO EXISTE EJECUCION PARA LA FECHA."
               SLEEP 3
               ERROR ""
               NEXT FIELD gd_fecha_corte
            ELSE
            	 EXIT INPUT
            END IF
         END IF

      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO."
         SLEEP 2
         ERROR ""
         EXIT INPUT
   END INPUT
   CLEAR FORM

END FUNCTION
################################################################################
FUNCTION despliega_pasos(ld_fecha_corte)

   DEFINE ld_fecha_corte	DATE,
          li_pos,
          li_status  SMALLINT,
          lc_sql     CHAR(600)

   DEFINE lar_consulta ARRAY[10] OF RECORD
          paso             CHAR(1),
          desc_paso        CHAR(12),
          fecha_inicio     CHAR(19),
          fecha_fin        CHAR(19),
          usuario          CHAR(8)
          END RECORD,
          lc_fecha_inicio     CHAR(23),
          lc_fecha_fin        CHAR(23)

   OPEN WINDOW  ventana_2 AT 6,4 WITH FORM "CTAB0352" ATTRIBUTES(BORDER)

   LET lc_sql = "SELECT etapa_cod, ",
                "DECODE(etapa_cod,1,'INICIALIZA  ', ",
                                 "2,'SALDOS      ', ",
                                 "3,'PREPARA NSS ', ",
                                 "4,'LIQUIDACION ', ",
                                 "5,'PREP REGIMEN', ",
                                 "6,'ACT REGIMEN '  ",
                        ") desc_paso, ",
                "parametro1 || hora_inicial, ",
                "parametro2 || hora_final,   ",
                "usuario ",
                "FROM   safre_af:dis_ctrl_proceso ",
                "WHERE  fecha_proceso = '", ld_fecha_corte, "' ",
                "AND    proceso_cod   = 'CTAB035' ",
                "ORDER BY etapa_cod "

   {DISPLAY lc_sql
   EXIT PROGRAM}

   PREPARE p_proceso FROM lc_sql

   DECLARE cur_proceso CURSOR FOR p_proceso

   LET li_pos = 1
   FOREACH cur_proceso INTO lar_consulta[li_pos].paso        ,
                            lar_consulta[li_pos].desc_paso   ,
                            lc_fecha_inicio,
                            lc_fecha_fin,
                            lar_consulta[li_pos].usuario

      LET lar_consulta[li_pos].fecha_inicio = lc_fecha_inicio[1,10], lc_fecha_inicio[15,23]
      LET lar_consulta[li_pos].fecha_fin    = lc_fecha_fin[1,10],    lc_fecha_fin[15,23]
      LET li_pos = li_pos + 1
   END FOREACH

   IF (li_pos-1) > 0 THEN
      LET li_status = 0
      CALL SET_COUNT(li_pos-1)
      DISPLAY ld_fecha_corte TO fecha_corte
      DISPLAY ARRAY lar_consulta TO scr_1.*
          ON KEY ( INTERRUPT )
             LET li_pos = 0
             EXIT DISPLAY
      END DISPLAY
   ELSE
      LET li_status = 1
   END IF

   CLOSE WINDOW ventana_2

   RETURN li_status
END FUNCTION
################################################################################
FUNCTION inserta_paso_reporte(ld_fecha_corte, lc_proceso_cod)
   DEFINE li_folio_proceso INTEGER,
          lc_hora_inicial  CHAR(08),
          li_totnss        INTEGER,
          ld_fecha_corte   DATE,
          lc_proceso_cod   CHAR(10)

   LET lc_hora_inicial  = TIME

   SELECT "X"
   FROM safre_af:cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   safre_af:cta_folio

      UPDATE safre_af:cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO safre_af:cta_folio
      VALUES (li_folio_proceso)
   END IF

   SELECT COUNT(*)
   INTO   li_totnss
   FROM   safre_tmp:cta_cuota_multisie_nss

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           lc_proceso_cod,          -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           lc_hora_inicial,         -- hora_inicial
           NULL,                    -- hora_final
           "corte_multisie",        -- parametro1
           ld_fecha_corte,          -- parametro2 --fecha_corte
           li_totnss,               -- parametro3 --tot_nss
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           li_folio_proceso,        -- folio
           NULL,                    -- resultado
           GUSER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA safre_af:dis_ctrl_proceso ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   RETURN li_totnss, li_folio_proceso

END FUNCTION
################################################################################
FUNCTION consulta(li_tipo_consulta)
   DEFINE li_tipo_consulta SMALLINT

   DEFINE li_consultar,
          li_pos,
          cur_row,
          scr_row      SMALLINT,
          lc_sql,
          lc_where     CHAR(500)

   DEFINE lar_consulta_gral ARRAY[200] OF RECORD
   	       opcion          SMALLINT,
   	       estatus_cuenta  SMALLINT,
   	       desc_estatus    CHAR(32),
   	       siefore         SMALLINT,
   	       totNss          INTEGER,
   	       acciones        DECIMAL(18,2)
      END RECORD

   DEFINE tot_nss_sie      INTEGER,
          tot_acciones_sie DECIMAL(18,2)

   DEFINE lc_comando       CHAR(200),
          li_totnss        INTEGER,
          li_folio_proceso INTEGER,
          lc_msg           CHAR(400),
          lc_ok            CHAR(1)

   LET tot_nss_sie      = 0
   LET tot_acciones_sie = 0

   FOR li_pos = 1 TO 200
   	  INITIALIZE lar_consulta_gral[li_pos].* TO NULL
   END FOR

   INITIALIZE gd_fecha_corte TO NULL
   INITIALIZE gi_siefore TO NULL
   INITIALIZE gi_estatus TO NULL

   --LET gd_fecha_corte = "11/30/2007"
   OPEN WINDOW ventana_3 AT 2,2 WITH FORM "CTAB0353" ATTRIBUTE(BORDER)
              #------------------------------------------------------------------------------
   DISPLAY    "<CTAB035>           C O N S U L T A     D E     S A L D O S                   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY    "<ESC> CONSULTAR                                              <CTRL-C> CANCELAR" AT 2,1

      INPUT BY NAME gd_fecha_corte,
      	            gi_siefore,
                    gi_estatus      WITHOUT DEFAULTS

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         ELSE
         	  --VERIFICA QUE EXISTA INF PARA CONSULTA
            IF valida_proceso(7, gd_fecha_corte) != 0 THEN
               NEXT FIELD gd_fecha_corte
            ELSE
            	 LET li_consultar = 1
            END IF
         END IF

        ON KEY (ESC)
          LET li_consultar = 1
          EXIT INPUT

        ON KEY (CONTROL-C,INTERRUPT)
        	 LET li_consultar = 0
           ERROR "CONSULTA CANCELADA"
           SLEEP 2
           ERROR ""
           CLEAR FORM
           EXIT INPUT
      END INPUT

      IF li_consultar = 1 THEN
      	 ERROR "PROCESANDO INFORMACION"
      	 CALL my_construct() RETURNING lc_where
      	 CASE li_tipo_consulta
      	    WHEN 1--CONSULTA ACTUAL
      	    LET lc_sql = "SELECT estatus_cuenta, ",
      	                 "DECODE(estatus_cuenta, ",
                                "0,'Asignado',   ",
                                "1,'Registrado', ",
                                "2,'Saldo Cero', ",
                                "3,'Cedidos con Saldo', ",
                                "4,'Cedidos con Negativos') desc_estatus, ",
      	                 "siefore, ",
                         "SUM(total_nss) NssTot,          ",
                         "SUM(monto_en_acciones) Acciones ",
                         "FROM   safre_tmp:cta_estatus_total_sie ",
                         "WHERE  fecha_corte = '", gd_fecha_corte, "' ",
                         lc_where CLIPPED,
                         "GROUP BY 1,3 ",
                         "ORDER BY 1,3 "

            WHEN 2--CONSULTA FUTURA
            DISPLAY "                              <CTRL-F GENERAR REPORTE>                        "
            AT 3,1 ATTRIBUTE(REVERSE)
      	    LET lc_sql = "SELECT estatus_cuenta, ",
      	                 "DECODE(estatus_cuenta, ",
                                "0,'Asignado',   ",
                                "1,'Registrado', ",
                                "2,'Saldo Cero', ",
                                "3,'Cedidos con Saldo', ",
                                "4,'Cedidos con Negativos') desc_estatus, ",
      	                 "siefore, ",
                         "SUM(total_nss) NssTot,          ",
                         "SUM(monto_en_acciones) Acciones ",
                         "FROM   safre_tmp:cta_estatus_total_sie_est ",
                         "WHERE  fecha_corte = '", gd_fecha_corte, "' ",
                         lc_where CLIPPED,
                         "GROUP BY 1,3 ",
                         "ORDER BY 1,3 "
         END CASE

         PREPARE query_gral FROM lc_sql
      	 DECLARE cur_consulta CURSOR FOR query_gral

         LET li_pos = 1

         FOREACH cur_consulta INTO lar_consulta_gral[li_pos].estatus_cuenta,
         	                         lar_consulta_gral[li_pos].desc_estatus  ,
                                   lar_consulta_gral[li_pos].siefore       ,
                                   lar_consulta_gral[li_pos].totNss        ,
                                   lar_consulta_gral[li_pos].acciones

            LET tot_nss_sie      = tot_nss_sie      + lar_consulta_gral[li_pos].totNss
            LET tot_acciones_sie = tot_acciones_sie + lar_consulta_gral[li_pos].acciones

         	  LET li_pos = li_pos + 1
         END FOREACH

         LET li_pos = li_pos - 1

         IF li_pos > 0 THEN
         	  ERROR ""
         	  CALL display_mensajes(1)
         	  DISPLAY BY NAME tot_nss_sie, tot_acciones_sie

         	  INPUT ARRAY lar_consulta_gral WITHOUT DEFAULTS FROM scr_sie.*
            ATTRIBUTES(MAXCOUNT = li_pos,COUNT = li_pos)
               BEFORE ROW
                  LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()

                  CALL muestra_subcuentas(lar_consulta_gral[cur_row].siefore,
                                          lar_consulta_gral[cur_row].estatus_cuenta,
                                          gd_fecha_corte,
                                          li_tipo_consulta)
         	     ON KEY(CONTROL-C,INTERRUPT)
                  CLEAR FORM
                  EXIT INPUT

               ON KEY(CONTROL-F)
                  IF li_tipo_consulta = 2 THEN --CONSULTA ESTIMADA
                     LET lc_comando = "fglgo CTAB038.4gi " CLIPPED,
                                       li_folio_proceso CLIPPED, " ",
                                       gd_fecha_corte   CLIPPED, " ",
                                       li_totnss        CLIPPED, " "
                     RUN lc_comando
                     ERROR "LISTADO GENERADO"
                     SLEEP 2
                     ERROR ""
                     CLEAR FORM
                     EXIT INPUT
                  ELSE
                  	 CONTINUE INPUT
                  END IF

               ON KEY (CONTROL-M)
        	        LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()

                  CALL display_subcuentas(lar_consulta_gral[cur_row].siefore,
                                          lar_consulta_gral[cur_row].estatus_cuenta,
                                          gd_fecha_corte,
                                          li_tipo_consulta)
            END INPUT
         ELSE
            ERROR "REGISTRO NO ENCONTRADO"
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
         END IF
      END IF
   CLOSE WINDOW ventana_3
END FUNCTION
################################################################################
FUNCTION display_mensajes(li_modo)
   DEFINE li_modo SMALLINT

   IF li_modo = 1 THEN
   	  DISPLAY "<ENTER> VER DETALLE" AT 2,29
   	  DISPLAY "   Estatus                         Siefore  NssTot      Monto en Acciones     " AT 5,1 ATTRIBUTE(REVERSE)
      DISPLAY "Subcuenta                  Monto en Acciones Precio Vig.   Monto en Pesos     " AT 12,1 ATTRIBUTE(REVERSE)
      DISPLAY "         T O T A L         " AT 21,1 ATTRIBUTE(REVERSE)
      DISPLAY "                 T O T A L                 " AT 11,1 ATTRIBUTE(REVERSE)
   ELSE
   	  DISPLAY "                                                                              " AT 5,1 ATTRIBUTE(REVERSE)
      DISPLAY "                                                                              " AT 12,1 ATTRIBUTE(REVERSE)
   END IF
END FUNCTION
################################################################################
FUNCTION muestra_subcuentas(li_siefore, li_estatus, ld_fecha_corte, li_tipo_consulta)
   DEFINE
      li_siefore,
      li_estatus,
      li_pos,
      li_cont,
      li_tipo_consulta     SMALLINT

   DEFINE ld_fecha_corte DATE

   DEFINE
      lar_subcta ARRAY[11] OF RECORD
      	   subcuenta   SMALLINT,
           desc_subcta CHAR(49),
           acciones    DECIMAL(18,2),
           precio      DECIMAL(7,6),
           pesos       DECIMAL(18,2)
      END RECORD

   DEFINE tot_acciones    DECIMAL(18,2),
          tot_precio      DECIMAL(7,6),
          tot_pesos       DECIMAL(18,2)

   DEFINE lc_query CHAR(500)

   FOR li_pos = 1 TO 10
   	  INITIALIZE lar_subcta[li_pos].* TO NULL
   END FOR

   LET tot_acciones = 0
   LET tot_precio   = 0
   LET tot_pesos    = 0

   CASE li_tipo_consulta
   	  WHEN 1 --CONSULTA ACTUAL
         LET lc_query = "SELECT b.subct_cns, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   safre_tmp:cta_estatus_saldos a, "  ,
                               "safre_af:tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_cns IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
      WHEN 2 --CONSULTA FUTURA
         LET lc_query = "SELECT b.subct_cns, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   safre_tmp:cta_estatus_saldos_est a, "  ,
                               "safre_af:tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_cns IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
   END CASE

   {DISPLAY lc_query
   EXIT PROGRAM}

   PREPARE query_subcta FROM lc_query
   DECLARE cur_subcta CURSOR FOR query_subcta

   LET li_pos = 1

   FOREACH cur_subcta INTO lar_subcta[li_pos].subcuenta,
   	                       lar_subcta[li_pos].acciones
   	  --DESC SUBCTA
   	  CASE           lar_subcta[li_pos].subcuenta
   	  	 WHEN  1 LET lar_subcta[li_pos].desc_subcta = "Retiro 97"
   	  	 WHEN  2 LET lar_subcta[li_pos].desc_subcta = "Cesantía y Vejez"
   	  	 WHEN  3 LET lar_subcta[li_pos].desc_subcta = "Cuota Social y Especial"
   	  	 WHEN  5 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Voluntarias Ventanilla"
   	  	 WHEN  8 LET lar_subcta[li_pos].desc_subcta = "Retiro 92 IMSS"
   	  	 WHEN 13 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Voluntarias Patronales"
   	  	 WHEN 15 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Complementarias de Retiro Patronales"
   	  	 WHEN 17 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Complementarias de Retiro Ventanilla"
   	  	 WHEN 18 LET lar_subcta[li_pos].desc_subcta = "Aportaciones a Largo Plazo"
   	  	 WHEN 22 LET lar_subcta[li_pos].desc_subcta = "Ahorro para el retiro ISSSTE SIEFORE"
   	  END CASE

   	  --PRECIO
   	  CASE li_tipo_consulta
   	     WHEN 1
   	        SELECT precio_del_dia
   	        INTO   lar_subcta[li_pos].precio
   	        FROM   safre_tmp:glo_valor_accion
   	        WHERE  codigo_siefore  = li_siefore
   	        AND    fecha_valuacion = gd_fecha_corte
   	     WHEN 2
   	        SELECT precio_del_dia
   	        INTO   lar_subcta[li_pos].precio
   	        FROM   safre_tmp:glo_valor_accion
   	        WHERE  codigo_siefore  = li_siefore
   	        AND    fecha_valuacion = gd_fecha_corte
   	  END CASE

   	  --PESOS
   	  LET lar_subcta[li_pos].pesos = lar_subcta[li_pos].acciones * lar_subcta[li_pos].precio
   	  LET li_pos = li_pos + 1

   END FOREACH

   LET li_pos = li_pos - 1

   IF li_pos > 0 THEN
   	  FOR li_cont = 1 TO 8
   	  	 DISPLAY lar_subcta[li_cont].* TO scr_cta[li_cont].*
   	  END FOR

   	  LET tot_precio   = tot_precio   + lar_subcta[1].precio

   	  FOR li_cont = 1 TO li_pos
   	  	 IF lar_subcta[li_cont].acciones IS NOT NULL THEN
   	  	    LET tot_acciones = tot_acciones + lar_subcta[li_cont].acciones
            LET tot_pesos    = tot_pesos    + lar_subcta[li_cont].pesos
         END IF

   	  	 DISPLAY BY NAME tot_acciones,
                         tot_precio  ,
                         tot_pesos
   	  END FOR

   ELSE
   	  ERROR "REGISTRO NO ENCONTRADO"
       SLEEP 1
       ERROR ""
   END IF

END FUNCTION
################################################################################
FUNCTION display_subcuentas(li_siefore, li_estatus, ld_fecha_corte, li_tipo_consulta)
   DEFINE
      li_siefore,
      li_estatus,
      li_pos,
      li_cont,
      li_tipo_consulta     SMALLINT

   DEFINE ld_fecha_corte DATE

   DEFINE
      lar_subcta ARRAY[11] OF RECORD
      	   subcuenta   SMALLINT,
           desc_subcta CHAR(49),
           acciones    DECIMAL(18,2),
           precio      DECIMAL(7,6),
           pesos       DECIMAL(18,2)
      END RECORD

   DEFINE tot_acciones    DECIMAL(18,2),
          tot_precio      DECIMAL(7,6),
          tot_pesos       DECIMAL(18,2)

   DEFINE lc_query CHAR(500)

   FOR li_pos = 1 TO 10
   	  INITIALIZE lar_subcta[li_pos].* TO NULL
   END FOR

   LET tot_acciones = 0
   LET tot_precio   = 0
   LET tot_pesos    = 0

   CASE li_tipo_consulta
   	  WHEN 1 --CONSULTA ACTUAL
         LET lc_query = "SELECT b.subct_cns, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   safre_tmp:cta_estatus_saldos a, "  ,
                               "safre_af:tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_cns IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
      WHEN 2 --CONSULTA FUTURA
         LET lc_query = "SELECT b.subct_cns, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   safre_tmp:cta_estatus_saldos_est a, "  ,
                               "safre_af:tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_cns IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
   END CASE

   PREPARE query_subcta2 FROM lc_query
   DECLARE cur_subcta2 CURSOR FOR query_subcta2

   LET li_pos = 1

   FOREACH cur_subcta2 INTO lar_subcta[li_pos].subcuenta,
   	                        lar_subcta[li_pos].acciones
   	  --DESC SUBCTA
   	  CASE           lar_subcta[li_pos].subcuenta
   	  	 WHEN  1 LET lar_subcta[li_pos].desc_subcta = "Retiro 97"
   	  	 WHEN  2 LET lar_subcta[li_pos].desc_subcta = "Cesantía y Vejez"
   	  	 WHEN  3 LET lar_subcta[li_pos].desc_subcta = "Cuota Social y Especial"
   	  	 WHEN  5 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Voluntarias Ventanilla"
   	  	 WHEN  8 LET lar_subcta[li_pos].desc_subcta = "Retiro 92 IMSS"
   	  	 WHEN 13 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Voluntarias Patronales"
   	  	 WHEN 15 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Complementarias de Retiro Patronales"
   	  	 WHEN 17 LET lar_subcta[li_pos].desc_subcta = "Aportaciones Complementarias de Retiro Ventanilla"
   	  	 WHEN 18 LET lar_subcta[li_pos].desc_subcta = "Aportaciones a Largo Plazo"
   	  	 WHEN 22 LET lar_subcta[li_pos].desc_subcta = "Ahorro para el retiro ISSSTE SIEFORE"
   	  END CASE

   	  --PRECIO
   	  CASE li_tipo_consulta
   	     WHEN 1
   	        SELECT precio_del_dia
   	        INTO   lar_subcta[li_pos].precio
   	        FROM   safre_tmp:glo_valor_accion
   	        WHERE  codigo_siefore  = li_siefore
   	        AND    fecha_valuacion = gd_fecha_corte
   	     WHEN 2
   	        SELECT precio_del_dia
   	        INTO   lar_subcta[li_pos].precio
   	        FROM   safre_tmp:glo_valor_accion
   	        WHERE  codigo_siefore  = li_siefore
   	        AND    fecha_valuacion = gd_fecha_corte
   	  END CASE

   	  --PESOS
   	  LET lar_subcta[li_pos].pesos = lar_subcta[li_pos].acciones * lar_subcta[li_pos].precio
   	  LET li_pos = li_pos + 1

   END FOREACH

   LET li_pos = li_pos - 1

   IF li_pos > 0 THEN
   	  CALL SET_COUNT(li_pos)
   	  LET tot_precio   = tot_precio   + lar_subcta[1].precio
   	  FOR li_cont = 1 TO li_pos
   	  	 LET tot_acciones = tot_acciones + lar_subcta[li_cont].acciones
         LET tot_pesos    = tot_pesos    + lar_subcta[li_cont].pesos

   	  	 DISPLAY BY NAME tot_acciones,
                         tot_precio  ,
                         tot_pesos
      END FOR

   	  DISPLAY ARRAY lar_subcta TO scr_cta.*
   	     ON KEY(INTERRUPT)
   	     	   EXIT DISPLAY
   	  END DISPLAY
   ELSE
   	  ERROR "REGISTRO NO ENCONTRADO"
       SLEEP 1
       ERROR ""
   END IF

END FUNCTION
################################################################################
FUNCTION elimina_ejecucion(li_paso)
   DEFINE li_paso SMALLINT,
          lc_opc  CHAR(1)

   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"

   INITIALIZE gd_fecha_corte TO NULL
   --LET gd_fecha_corte = "10/31/2007"

   INPUT BY NAME gd_fecha_corte WITHOUT DEFAULTS
      BEFORE INPUT
         DISPLAY BY NAME gc_mensaje1,
                         gc_mensaje2

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         END IF

      WHILE TRUE
         PROMPT "ESTA SEGURO DE CONTINUAR EJECUCION S/N:" FOR CHAR lc_opc

         IF lc_opc  MATCHES "[SsNn]" THEN
            IF lc_opc MATCHES "[Ss]" THEN
            	 CALL ejecuta_paso(li_paso)
            	 SLEEP 2
               ERROR ""
               CLEAR FORM
            	 EXIT INPUT
            ELSE
               ERROR "PROCESO CANCELADO."
               SLEEP 2
               ERROR ""
               CLEAR FORM
               EXIT INPUT
            END IF
         ELSE
         	  ERROR "SOLO INDIQUE S o N "
            SLEEP 2
            ERROR ""
            CONTINUE WHILE
         END IF
      END WHILE

      ON KEY (CONTROL-C,INTERRUPT)
        ERROR "PROCESO CANCELADO"
        SLEEP 2
        ERROR ""
        CLEAR FORM
        EXIT INPUT
   END INPUT

END FUNCTION
################################################################################
FUNCTION my_construct()
   DEFINE lc_where CHAR(400)

   INITIALIZE lc_where TO NULL

   IF gi_siefore IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND siefore = ", gi_siefore CLIPPED
   END IF

   IF gi_estatus IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND estatus_cuenta = ", gi_estatus CLIPPED
   END IF

   RETURN lc_where
END FUNCTION
################################################################################
FUNCTION consulta_liquidacion()

   DEFINE li_consultar,
          li_pos       SMALLINT,
          lc_sql       CHAR(1000),
          lc_where     CHAR(500)

   FOR li_pos = 1 TO 400
   	  INITIALIZE gar_consulta_liq[li_pos].* TO NULL
   END FOR

   INITIALIZE gd_fecha_corte TO NULL
   INITIALIZE gi_siefore_ced TO NULL
   INITIALIZE gi_siefore_rec TO NULL
   INITIALIZE gi_folio_liq   TO NULL

   OPEN WINDOW ventana_4 AT 2,2 WITH FORM "CTAB0354" ATTRIBUTE(BORDER)
              #------------------------------------------------------------------------------
   DISPLAY    "<CTAB035>      C O N S U L T A    D E    L I Q U I D A C I O N                " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY    "<ESC> CONSULTAR                                              <CTRL-C> CANCELAR" AT 2,1

      INPUT BY NAME gd_fecha_corte,
                    gi_siefore_ced,
                    gi_siefore_rec,
                    gi_folio_liq   WITHOUT DEFAULTS

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         ELSE
         	  --VERIFICA QUE EXISTA INF PARA CONSULTA
            IF valida_proceso(9, gd_fecha_corte) != 0 THEN
               NEXT FIELD gd_fecha_corte
            ELSE
            	 LET li_consultar = 1
            END IF
         END IF

      AFTER FIELD gi_folio_liq
         IF gi_folio_liq IS NOT NULL THEN
            SELECT "X"
            FROM   safre_tmp:tmp_folio_corte
            WHERE  folio = gi_folio_liq
            GROUP BY 1

            IF SQLCA.SQLCODE != 0 THEN
            	 ERROR "NO EXISTE ESTE FOLIO DE LIQUIDACON"
            	 NEXT FIELD gi_folio_liq
            END IF
         END IF

        ON KEY (ESC)
          LET li_consultar = 1
          EXIT INPUT

        ON KEY (CONTROL-C,INTERRUPT)
        	 LET li_consultar = 0
           ERROR "CONSULTA CANCELADA"
           SLEEP 2
           ERROR ""
           CLEAR FORM
           EXIT INPUT
      END INPUT

      IF li_consultar = 1 THEN
      	 CALL construct_liquidacion() RETURNING lc_where

      	 LET lc_sql = "SELECT a.siefore,               ",
      	                     "SUM(a.monto_en_pesos),   ",
                             "SUM(a.monto_en_acciones) ",
                      "FROM   safre_af:dis_cuenta a ",
                      "WHERE  a.folio       = ?  ",
                      "AND    a.subcuenta   = ?  ",
                      "AND    a.siefore     <> ? ",
                      " GROUP  by 1",
                      " ORDER  by 1"

         PREPARE get_monto_recibido FROM lc_sql

      	 LET lc_sql = "SELECT a.folio,                 ",
                             "a.subcuenta,             ",
                             "a.siefore,               ",
                             "SUM(a.monto_en_pesos),   ",
                             "SUM(a.monto_en_acciones) ",
                      "FROM   safre_af:dis_cuenta a ",
                      "WHERE  a.folio IN (SELECT folio ",
                                         "FROM   safre_tmp:tmp_folio_corte ",
                                         ") ",
                      --"AND    a.siefore IN (1,2) ",
                      "AND    a.monto_en_pesos < 0",
                      lc_where CLIPPED,
                      " GROUP  by 1,2,3",
                      " ORDER  by 1,3,2 DESC"

         PREPARE query_liq FROM lc_sql
      	 DECLARE cur_liq CURSOR FOR query_liq

         LET li_pos = 1

         FOREACH cur_liq INTO gar_consulta_liq[li_pos].folio,
         	                      gar_consulta_liq[li_pos].subcuenta,
         	                      gar_consulta_liq[li_pos].siefore_ced,
         	                      gar_consulta_liq[li_pos].pesos_ced,
         	                      gar_consulta_liq[li_pos].acciones_ced


         	  EXECUTE get_monto_recibido USING gar_consulta_liq[li_pos].folio,
         	                                   gar_consulta_liq[li_pos].subcuenta,
                                             gar_consulta_liq[li_pos].siefore_ced
                                       INTO  gar_consulta_liq[li_pos].siefore_rec,
                                             gar_consulta_liq[li_pos].pesos_rec,
                                             gar_consulta_liq[li_pos].acciones_rec
            LET li_pos = li_pos + 1
         END FOREACH

         LET li_pos = li_pos - 1

         IF li_pos > 0 THEN
         	  DISPLAY "Folio   Sct Sie    Pesos          Acciones  Sie  Pesos           Acciones     " AT 5,1 ATTRIBUTE(REVERSE)
         	  DISPLAY "                              <CTRL-F GENERAR REPORTE>                        "
            AT 3,1 ATTRIBUTE(REVERSE)
         	  CALL SET_COUNT(li_pos)
         	  DISPLAY ARRAY gar_consulta_liq TO scr_liq.*
         	     ON KEY(INTERRUPT)
         	     	   EXIT DISPLAY

         	     ON KEY(CONTROL-F)
                  ERROR "PROCESANDO INFORMACION"
                  CALL archivo_liquidacion()
                  ERROR "LISTADO GENERADO"
                  SLEEP 1
                  ERROR ""

         	  END DISPLAY
         ELSE
         	  ERROR "REGISTRO NO ENCONTRADO"
             SLEEP 1
             ERROR ""
         END IF
      END IF
   CLOSE WINDOW ventana_4
END FUNCTION
################################################################################
FUNCTION construct_liquidacion()
   DEFINE lc_where      CHAR(500),
          li_sifore_ced SMALLINT,
          li_folio      INTEGER

   INITIALIZE lc_where TO NULL

   IF gi_siefore_ced IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED,
   	                 " AND a.siefore = ", gi_siefore_ced CLIPPED
   END IF

   IF gi_siefore_rec IS NOT NULL THEN
   	  IF gi_siefore_ced IS NULL THEN
   	  	 IF gi_folio_liq IS NULL THEN
   	  	 	  DECLARE cur_folio_liq CURSOR FOR
   	  	 	  SELECT folio
   	  	 	  FROM   safre_tmp:tmp_folio_corte
   	  	 	  WHERE  siefore_recep = gi_siefore_rec

   	  	 	  LET lc_where = lc_where CLIPPED,
   	  	 	                 " AND a.folio IN ("

   	  	 	  FOREACH cur_folio_liq INTO li_folio
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	  	                li_folio CLIPPED, ","
   	  	 	  END FOREACH
   	  	 	     LET lc_where = lc_where CLIPPED,
   	  	 	                    "0)"
   	  	 ELSE
   	  	 	  SELECT "X"
   	  	 	  FROM   safre_tmp:tmp_folio_corte
   	  	 	  WHERE  siefore_recep = gi_siefore_rec
   	  	 	  AND    folio         = gi_folio_liq
   	  	 	  GROUP BY 1

   	  	 	  IF SQLCA.SQLCODE = 0 THEN
   	  	 	     LET lc_where = lc_where CLIPPED,
   	  	 	                    " AND folio = ", gi_folio_liq CLIPPED
   	  	 	  ELSE
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	  	                " AND folio < 0 "
   	  	 	  END IF

   	  	 END IF
   	  ELSE
   	  	 IF gi_folio_liq IS NULL THEN
   	  	 	  SELECT folio
   	  	 	  INTO   li_folio
   	  	 	  FROM   safre_tmp:tmp_folio_corte
   	  	 	  WHERE  siefore_ced   = gi_siefore_ced
   	  	 	  AND    siefore_recep = gi_siefore_rec

   	  	 	  IF li_folio IS NOT NULL THEN
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	                    " AND folio = ", li_folio CLIPPED
   	  	 	  ELSE
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	                    " AND folio < 0 "
   	  	 	  END IF
   	  	 ELSE
   	  	 	  SELECT "X"
   	  	 	  FROM   safre_tmp:tmp_folio_corte
   	  	 	  WHERE  siefore_ced   = gi_siefore_ced
   	  	 	  AND    siefore_recep = gi_siefore_rec
   	  	 	  AND    folio         = gi_folio_liq

   	  	 	  IF SQLCA.SQLCODE = 0 THEN
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	                  " AND folio = ", gi_folio_liq CLIPPED
   	  	 	  ELSE
   	  	 	  	 LET lc_where = lc_where CLIPPED,
   	  	 	                  " AND folio < 0 "
   	  	 	  END IF
   	  	 END IF
   	  END IF
   END IF

   IF gi_folio_liq IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED,
   	  	 	         " AND folio = ", gi_folio_liq CLIPPED
   END IF

   RETURN lc_where

END FUNCTION
################################################################################
FUNCTION archivo_liquidacion()
   DEFINE lc_nomarch   CHAR(80),
          lc_comando   CHAR(200)
   DEFINE li_cont      SMALLINT

   CALL det_afore()

   SELECT ruta_listados
   INTO   lc_nomarch
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   IF lc_nomarch IS NULL THEN
   	  ERROR "NO EXISTE LA RUTA INDICADA PARA EL REPORTE"
   	  RETURN
   END IF

   LET lc_nomarch = lc_nomarch CLIPPED, "/liquidacion_corte",
                    gd_fecha_corte USING "DDMMYYYY"

   START REPORT rpt_liquidacion TO lc_nomarch
   FOR li_cont = 1 TO 300
   	  IF gar_consulta_liq[li_cont].folio IS NOT NULL THEN
   	  	 OUTPUT TO REPORT rpt_liquidacion(gar_consulta_liq[li_cont].*)
   	  ELSE
   	  	 EXIT FOR
   	  END IF
   END FOR

   FINISH REPORT rpt_liquidacion

   LET lc_comando = "lp ",lc_nomarch
   RUN lc_comando

END FUNCTION
################################################################################
FUNCTION det_afore()
   DEFINE li_siefore SMALLINT,
   	      lc_desc_siefore CHAR(9)

   SELECT razon_social
   INTO   gr_reporte.afore_desc
   FROM   safre_af:tab_afore_local

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 1
   INTO   gr_reporte.precio1
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 1
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio1 IS NULL THEN
   	 LET gr_reporte.precio1 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 2
   INTO   gr_reporte.precio2
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 2
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio2 IS NULL THEN
   	 LET gr_reporte.precio2 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 3
   INTO   gr_reporte.precio3
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 3
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio3 IS NULL THEN
   	 LET gr_reporte.precio3 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 4
   INTO   gr_reporte.precio4
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 4
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio4 IS NULL THEN
   	 LET gr_reporte.precio4 = 0
   END IF

   SELECT a.precio_del_dia  --PRECIO DE ACCION SIEFORE 5
   INTO   gr_reporte.precio5
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 5
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio5 IS NULL THEN
   	 LET gr_reporte.precio5 = 0
   END IF

   DECLARE cur_det_sie CURSOR FOR
   SELECT codigo_siefore,
          razon_social
   FROM   safre_af:tab_siefore_local
   WHERE  codigo_siefore IN (1,2,3,4,5)

   FOREACH cur_det_sie INTO li_siefore,
   	                        lc_desc_siefore
   	  CASE li_siefore
   	  	 WHEN 1 LET gr_reporte.siefore1 = lc_desc_siefore
   	  	 WHEN 2 LET gr_reporte.siefore2 = lc_desc_siefore
   	  	 WHEN 3 LET gr_reporte.siefore3 = lc_desc_siefore
   	  	 WHEN 4 LET gr_reporte.siefore4 = lc_desc_siefore
   	  	 WHEN 5 LET gr_reporte.siefore5 = lc_desc_siefore
   	  END CASE
   END FOREACH
END FUNCTION
################################################################################
REPORT rpt_liquidacion(lr_reporte)
   DEFINE lr_reporte  RECORD
             folio            INTEGER,
             subcuenta        SMALLINT,
             siefore_ced      SMALLINT,
             pesos_ced        DECIMAL(22,6),
             acciones_ced     DECIMAL(22,6),
             siefore_rec      SMALLINT,
             pesos_rec        DECIMAL(22,6),
             acciones_rec     DECIMAL(22,6)
       END RECORD

   DEFINE lc_siefore_desc_ced CHAR(9),
          lc_siefore_desc_rec CHAR(9),
          lc_desc_subcta      CHAR(45)

   DEFINE ld_pesos_ced   ,
          ld_acciones_ced,
          ld_pesos_rec   ,
          ld_acciones_rec     DECIMAL(22,6)

   DEFINE li_total_nss INTEGER

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
   FIRST PAGE HEADER
      PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s9H',
            '\033(s3B',        --NEGRITAS
            '\033*c4500a70B',  --#A Longitud, #B Grosor
            '\033*c0P',        --INVERSO
            '\033*v1t1O'       --TEXTO EN BCO.
--******************************************************************************
-- ENCABEZADO
--******************************************************************************
      PRINT COLUMN 021, "REPORTE DE DE LIQUIDACION DE CORTE TRANSVERSAL POR MULTISIEFORE",
            '\033(s15H',
            '\033(s3B',
            '\033*v0t1O',
            '\033015'
      PRINT COLUMN 001, gr_reporte.afore_desc,
            --COLUMN 016, "Folio:", gi_folio USING "<<<,<<<,<<<",
            COLUMN 016, "Fecha:", TODAY USING "DD/MM/YYYY"
--******************************************************************************
-- PRECIOS DE ACCION
--******************************************************************************
      PRINT COLUMN 40, "Precio de la accion por siefore al ", gd_fecha_corte USING "DD/MM/YYYY", '\033015'
      PRINT '\033(s15H\033(s3B\033015',
            COLUMN  72 , gr_reporte.siefore1,
            COLUMN  82 , gr_reporte.precio1,
            COLUMN  98 , gr_reporte.siefore2,
            COLUMN 110, gr_reporte.precio2,
            COLUMN 126, gr_reporte.siefore3,
            COLUMN 138, gr_reporte.precio3,'\033015'
      PRINT '\033(s15H\033(s3B\033015',
            COLUMN  72, gr_reporte.siefore4,
            COLUMN  82, gr_reporte.precio4,
            COLUMN  98, gr_reporte.siefore5,
            COLUMN 110, gr_reporte.precio5,
            '\033(s27H',
            '\033015'

   BEFORE GROUP OF lr_reporte.folio
   	  SELECT razon_social
   	  INTO   lc_siefore_desc_ced
      FROM   safre_af:tab_siefore_local
      WHERE  codigo_siefore = lr_reporte.siefore_ced

      SELECT razon_social
   	  INTO   lc_siefore_desc_rec
      FROM   safre_af:tab_siefore_local
      WHERE  codigo_siefore = lr_reporte.siefore_rec

      LET ld_pesos_ced    = 0
      LET ld_acciones_ced = 0
      LET ld_pesos_rec    = 0
      LET ld_acciones_rec = 0

      PRINT '\033e\033(s15H',
            '\033&18d',
            '\033(s3B',        --NEGRITAS
            '\033*c4500a150B',  --#A Longitud, #B Grosor
            '\033*c0P',        --INVERSO
            '\033*v1t1O'       --TEXTO EN BCO.
--******************************************************************************
-- ENCABEZADO DETALLE
--******************************************************************************
      PRINT  COLUMN 93, "SALDOS A TRANSFERIR DE ", lc_siefore_desc_ced, " A ", lc_siefore_desc_rec,
            '\033(s15H',
            '\033(s3B',
            '\033&18d',
            '\033015'

      PRINT '\033(s15H',
      	    '\033(s3B',
      	    '\033&18d',
      	    '\033015',
      	    COLUMN 102, "SALDOS A TRANSFERIR",
      	    COLUMN 156, lc_siefore_desc_rec,
      	    '\033(s16H',
            '\033(s3B',
            '\033&l8d',
      	    '\033015'

      PRINT COLUMN 001, "Folio   Siefore Subcta  Subcuentas Obligatorias",
            --COLUMN 108, "Saldo en pesos   Saldo en acciones",
            --COLUMN 154, "Saldo en pesos   Saldo en acciones",
            COLUMN  80, "Saldo en pesos   Saldo en acciones",
            COLUMN 115, "Siefore",
            COLUMN 134, "Saldo en pesos   Saldo en acciones",
            '\033*v0t1O',
            '\033015'
      SKIP 1 LINES

      BEFORE GROUP OF lr_reporte.subcuenta
   	  	 SELECT subct_desc
   	  	 INTO   lc_desc_subcta
   	  	 FROM   safre_af:tab_subcuenta
   	  	 WHERE  subct_cod = lr_reporte.subcuenta
--******************************************************************************
-- DETALLE DE LIQUIDACION
--******************************************************************************
   	  AFTER GROUP OF lr_reporte.subcuenta
   	  	LET ld_pesos_ced     = ld_pesos_ced    + lr_reporte.pesos_ced
   	  	LET ld_acciones_ced  = ld_acciones_ced + lr_reporte.acciones_ced
        LET ld_pesos_rec     = ld_pesos_rec    + lr_reporte.pesos_rec
   	  	LET ld_acciones_rec  = ld_acciones_rec + lr_reporte.acciones_rec

   	  	PRINT COLUMN 001, lr_reporte.folio        USING "######&",
   	  	      COLUMN 011, lr_reporte.siefore_ced  USING "#&",
   	  	      COLUMN 018, lr_reporte.subcuenta    USING "#&",
   	  	      COLUMN 024, lc_desc_subcta,
   	  	      COLUMN 069, lr_reporte.pesos_ced     USING "#################&.&&&&&&",
   	  	      COLUMN 095, lr_reporte.acciones_ced  USING "###########&.&&&&&&",
   	  	      COLUMN 117, lr_reporte.siefore_rec   USING "#&",
   	  	      COLUMN 123, lr_reporte.pesos_rec     USING "#################&.&&&&&&",
   	  	      COLUMN 149, lr_reporte.acciones_rec  USING "###########&.&&&&&&",
   	  	      '\033015'
--******************************************************************************
-- TOTAL DETALLE LIQUIDACION
--******************************************************************************
      AFTER GROUP OF lr_reporte.folio
      	SELECT COUNT(*)
      	INTO   li_total_nss
      	FROM   safre_tmp:tmp_nss_liquidados
      	WHERE  folio = lr_reporte.folio

   	    PRINT '\033(s16H',
              '\033(s3B',
              '\033&l8d',
              '\033*c4500a90B',   --#A Longitud, #B Grosor
              '\033*c0P',        --INVERSO
              '\033*v1t1O',      --TEXTO EN BCO.
              '\033015'
	  	  PRINT COLUMN 001, "Saldo total de la siefore Transferente y Receptora",
	  	        COLUMN 069, ld_pesos_ced     USING "#################&.&&&&&&",
	  	        COLUMN 095, ld_acciones_ced  USING "###########&.&&&&&&",
	  	        COLUMN 123, ld_pesos_rec     USING "#################&.&&&&&&",
	  	        COLUMN 149, ld_acciones_rec  USING "###########&.&&&&&&",
	  	        '\033015'
	  	  PRINT COLUMN 001, "Total de clientes liquidados",
	  	        COLUMN 115, li_total_nss       USING "######&",
	  	        '\033015'
	  	  INITIALIZE lc_siefore_desc_rec TO NULL
	  	  SKIP 1 LINES
END REPORT
################################################################################
FUNCTION consulta_regimen()
   DEFINE li_pos,
          li_consultar SMALLINT

   FOR li_pos = 1 TO 20
   	  INITIALIZE gar_regimen[li_pos].* TO NULL
   END FOR
   INITIALIZE gd_fecha_corte TO NULL

   DECLARE cur_regimen CURSOR FOR
   SELECT codigo_siefore,
          COUNT(UNIQUE nss)
   FROM   safre_af:cta_regimen
   GROUP BY 1
   ORDER BY 1

   OPEN WINDOW ventana_5 AT 2,2 WITH FORM "CTAB0355" ATTRIBUTE(BORDER)
              #------------------------------------------------------------------------------
   DISPLAY    "<CTAB035>         C O N S U L T A    D E    R E G I M E N                     " AT 1,1 ATTRIBUTE(REVERSE)


   LET li_pos = 1
   ERROR "PROCESANDO INFORMACION"
   FOREACH cur_regimen INTO gar_regimen[li_pos].codigo_siefore,
   	                        gar_regimen[li_pos].total
   	  SELECT razon_social
   	  INTO   gar_regimen[li_pos].desc_siefore
   	  FROM   safre_af:tab_siefore_local
   	  WHERE  codigo_siefore = gar_regimen[li_pos].codigo_siefore

      LET li_pos = li_pos + 1
   END FOREACH
   ERROR ""

   LET li_pos = li_pos - 1

   IF li_pos > 0 THEN
   	  DISPLAY "               Siefore                          Trabajadores                  " AT 5,1 ATTRIBUTE(REVERSE)
   	  DISPLAY "                                                             <CTRL-C> CANCELAR" AT 2,1
      DISPLAY "                              <CTRL-F GENERAR REPORTE>                        " AT 2,1 ATTRIBUTE(REVERSE)
   	  CALL SET_COUNT(li_pos)
   	  DISPLAY ARRAY gar_regimen TO scr_reg.*
   	     ON KEY(INTERRUPT)
   	     	   EXIT DISPLAY

   	     ON KEY(CONTROL-F)
            ERROR "PROCESANDO INFORMACION"
            CALL archivo_regimen()
            ERROR "LISTADO GENERADO"
            SLEEP 1
            ERROR ""
   	  END DISPLAY
   ELSE
   	  ERROR "REGISTRO NO ENCONTRADO"
       SLEEP 1
       ERROR ""
   END IF

   CLOSE WINDOW ventana_5

END FUNCTION
################################################################################
FUNCTION archivo_regimen()
   DEFINE lc_nomarch   CHAR(100),
          lc_comando   CHAR(200)
   DEFINE li_cont      SMALLINT
   DEFINE lc_current   CHAR(24)

   CALL det_afore()

   SELECT ruta_listados
   INTO   lc_nomarch
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   IF lc_nomarch IS NULL THEN
   	  ERROR "NO EXISTE LA RUTA INDICADA PARA EL REPORTE"
   	  RETURN
   END IF

   LET lc_current = CURRENT
   LET lc_nomarch = lc_nomarch CLIPPED,"/",GUSER CLIPPED,
                    ".regimen_corte.",
                    lc_current[1,10],lc_current[12,24]

   START REPORT rpt_regimen TO lc_nomarch
   FOR li_cont = 1 TO 20
   	  IF gar_regimen[li_cont].codigo_siefore IS NOT NULL THEN
   	  	 OUTPUT TO REPORT rpt_regimen(gar_regimen[li_cont].*)
   	  ELSE
   	  	 EXIT FOR
   	  END IF
   END FOR

   FINISH REPORT rpt_regimen

   LET lc_comando = "lp ",lc_nomarch
   RUN lc_comando

END FUNCTION
################################################################################
REPORT rpt_regimen(lr_reporte)
   DEFINE lr_reporte RECORD
          codigo_siefore SMALLINT,
          desc_siefore   CHAR(30),
          total          INTEGER
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
   FIRST PAGE HEADER
      PRINT COLUMN 024, "REPORTE DE REGIMEN DE TRABAJADORES"
      PRINT COLUMN 001, gr_reporte.afore_desc,
            COLUMN 065, "Fecha:", TODAY USING "DD/MM/YYYY"
      SKIP 1 LINE
      PRINT COLUMN 017, "SIEFORE                               TRABAJADORES"
   ON EVERY ROW
      PRINT COLUMN 019, lr_reporte.codigo_siefore USING "#&",
            COLUMN 024, lr_reporte.desc_siefore,
            COLUMN 056, lr_reporte.total          USING "##########&"
END REPORT
################################################################################
FUNCTION anexo_b()
   DEFINE lc_nomarch   CHAR(100),
          lc_comando   CHAR(200)
   DEFINE li_cont      SMALLINT
   DEFINE lc_current   CHAR(24)

   DEFINE lr_report RECORD
   	         siefore_ced SMALLINT,
             siefore_rec SMALLINT,
             total       INTEGER
          END RECORD

   CALL det_afore()

   SELECT ruta_listados
   INTO   lc_nomarch
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   IF lc_nomarch IS NULL THEN
   	  ERROR "NO EXISTE LA RUTA INDICADA PARA EL REPORTE"
   	  RETURN
   END IF

   LET lc_current = CURRENT
   LET lc_nomarch = lc_nomarch CLIPPED,"/",GUSER CLIPPED,
                    ".multisie_anexo_b.",
                    lc_current[1,10],lc_current[12,24]

   WHENEVER ERROR CONTINUE
      DROP TABLE multisie_anexo_b
   WHENEVER ERROR STOP

   CREATE TEMP TABLE multisie_anexo_b(
      nss         CHAR(11),
      sector      SMALLINT,
      siefore_ced SMALLINT,
      siefore_rec SMALLINT
   )
   
   ERROR "PROCESANDO INFORMACION"

   INSERT INTO multisie_anexo_b
   SELECT a.nss,            --nss
          a.id_sector,      --sector
          b.codigo_siefore, --siefore_ced
          a.siefore         --siefore_rec
   FROM   cta_cuota_multisie_nss a,
          safre_af:cta_regimen   b
   WHERE  a.nss = b.nss
   AND    b.subcuenta = 1

   CREATE INDEX ix_sector_anb  ON multisie_anexo_b(sector)
   CREATE INDEX ix_sie_ced_anb ON multisie_anexo_b(siefore_ced)
   CREATE INDEX ix_sie_rec_anb ON multisie_anexo_b(siefore_rec)

   UPDATE STATISTICS FOR TABLE multisie_anexo_b

   DECLARE cur_anexob CURSOR FOR
   SELECT siefore_ced,
          siefore_rec,
          COUNT(*)
   FROM   multisie_anexo_b
   GROUP BY 1,2
   ORDER BY 1,2      

   START REPORT rpt_anexob TO lc_nomarch
   FOREACH cur_anexob INTO lr_report.*
   	  OUTPUT TO REPORT rpt_anexob(lr_report.*)   	  
   END FOREACH   
   FINISH REPORT rpt_anexob
   
   LET lc_comando = "lp ", lc_nomarch CLIPPED
   RUN lc_comando
   
   ERROR "LISTADO GENERADO"
   SLEEP 3
   ERROR ""
END FUNCTION
################################################################################
REPORT rpt_anexob(lr_report)
   DEFINE lr_report RECORD
   	         siefore_ced SMALLINT,
             siefore_rec SMALLINT,
             total       INTEGER
   END RECORD

   DEFINE li_tot_rec_1,
          li_tot_rec_2,
          li_tot_rec_3,
          li_tot_rec_4,
          li_tot_rec_5,
          li_total_rec_hor,
          li_total_ced_hor,
          li_total_rec_ver,
          li_total_ced_ver,
          li_tot_rec_1_ver,
          li_tot_rec_2_ver,
          li_tot_rec_3_ver,
          li_tot_rec_4_ver,
          li_tot_rec_5_ver,
          li_procesar,
          li_procesar1,
          li_procesar2,
          li_procesar3,
          li_procesar4,
          li_procesar5,
          li_procesar_hor,
          li_independientes,
          li_independientes1,
          li_independientes2,
          li_independientes3,
          li_independientes4,
          li_independientes5,
          li_independientes_hor,
          li_total_trab,
          li_total_trab1,
          li_total_trab2,
          li_total_trab3,
          li_total_trab4,
          li_total_trab5,
          li_total_trab_ver

INTEGER

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
   FIRST PAGE HEADER
      PRINT COLUMN 017, "REPORTE DE VALIDACION DE CUENTAS DE CORTE TRANSVERSAL"
      SKIP 1 LINE
      PRINT COLUMN 001, "PROGRAMA: CTAB035"
      PRINT COLUMN 001, "AFORE ", gr_reporte.afore_desc,
            COLUMN 060, "Fecha:", TODAY USING "DD/MM/YYYY"
      PRINT COLUMN 001, "--------------------------------------------------------------------------------"
      PRINT COLUMN 001, "                       TOTAL DE TRABAJADORES POR SIEFORE                        "
      PRINT COLUMN 001, "--------------------------------------------------------------------------------"

      LET li_total_ced_ver = 0
      LET li_total_rec_ver = 0

      LET li_tot_rec_1_ver = 0
      LET li_tot_rec_2_ver = 0
      LET li_tot_rec_3_ver = 0
      LET li_tot_rec_4_ver = 0
      LET li_tot_rec_5_ver = 0

   BEFORE GROUP OF lr_report.siefore_ced

      LET li_tot_rec_1 = 0
      LET li_tot_rec_2 = 0
      LET li_tot_rec_3 = 0
      LET li_tot_rec_4 = 0
      LET li_tot_rec_5 = 0
      LET li_total_ced_hor = 0
      LET li_total_rec_hor = 0

      IF lr_report.siefore_ced = 2 THEN
      	 SKIP 1 LINE
      	 PRINT COLUMN 001, "     #Ctas SB2        SB1     SB2     SB3     SB4     SB5    TOTAL DIREFENCIA"      	 
      ELSE
      	 PRINT COLUMN 001, "     #Ctas SB1        SB1     SB2     SB3     SB4     SB5    TOTAL DIREFENCIA"      	 
      END IF

   ON EVERY ROW
      CASE lr_report.siefore_rec
      	  WHEN 1 LET li_tot_rec_1 = li_tot_rec_1 + lr_report.total
      	  WHEN 2 LET li_tot_rec_2 = li_tot_rec_2 + lr_report.total
      	  WHEN 3 LET li_tot_rec_3 = li_tot_rec_3 + lr_report.total
          WHEN 4 LET li_tot_rec_4 = li_tot_rec_4 + lr_report.total
          WHEN 5 LET li_tot_rec_5 = li_tot_rec_5 + lr_report.total
      END CASE

      LET li_total_rec_hor = li_total_rec_hor + lr_report.total

   AFTER GROUP OF lr_report.siefore_ced
   	  SELECT COUNT(*)
   	  INTO   li_total_ced_hor
   	  FROM   multisie_anexo_b
   	  WHERE  siefore_ced = lr_report.siefore_ced

   	  LET li_tot_rec_1_ver = li_tot_rec_1_ver + li_tot_rec_1
   	  LET li_tot_rec_2_ver = li_tot_rec_2_ver + li_tot_rec_2
   	  LET li_tot_rec_3_ver = li_tot_rec_3_ver + li_tot_rec_3
   	  LET li_tot_rec_4_ver = li_tot_rec_4_ver + li_tot_rec_4
   	  LET li_tot_rec_5_ver = li_tot_rec_5_ver + li_tot_rec_5

   	  LET li_total_rec_ver = li_total_rec_ver + li_total_rec_hor

   	  PRINT COLUMN 008, li_total_ced_hor                     USING "######&",
   	        COLUMN 019, li_tot_rec_1                         USING "######&",
   	        COLUMN 027, li_tot_rec_2                         USING "######&",
   	        COLUMN 035, li_tot_rec_3                         USING "######&",
   	        COLUMN 043, li_tot_rec_4                         USING "######&",
   	        COLUMN 051, li_tot_rec_5                         USING "######&",
   	        COLUMN 060, li_total_rec_hor                     USING "######&",
   	        COLUMN 071, li_total_ced_hor - li_total_rec_hor  USING "######&"

   ON LAST ROW
      SELECT COUNT(*)
      INTO   li_total_ced_ver
      FROM   multisie_anexo_b

      PRINT COLUMN 001, "--------------------------------------------------------------------------------"
      PRINT COLUMN 001, "TOTAL",
            COLUMN 008, li_total_ced_ver                     USING "######&",
            COLUMN 019, li_tot_rec_1_ver                     USING "######&",
            COLUMN 027, li_tot_rec_2_ver                     USING "######&",
            COLUMN 035, li_tot_rec_3_ver                     USING "######&",
            COLUMN 043, li_tot_rec_4_ver                     USING "######&",
            COLUMN 051, li_tot_rec_5_ver                     USING "######&",
            COLUMN 060, li_total_rec_ver                     USING "######&",
            COLUMN 071, li_total_ced_ver - li_total_rec_ver  USING "######&"

   SKIP 1 line
   PRINT COLUMN 001, "--------------------------------------------------------------------------------"
   PRINT COLUMN 001, "                              TOTAL TRABAJADORES                                "
   PRINT COLUMN 001, "--------------------------------------------------------------------------------"

   LET li_procesar           = 0 
   LET li_procesar1          = 0
   LET li_procesar2          = 0
   LET li_procesar3          = 0
   LET li_procesar4          = 0
   LET li_procesar5          = 0
   LET li_procesar_hor       = 0
   LET li_independientes     = 0
   LET li_independientes1    = 0
   LET li_independientes2    = 0
   LET li_independientes3    = 0
   LET li_independientes4    = 0
   LET li_independientes5    = 0
   LET li_independientes_hor = 0
   LET li_total_trab         = 0
   LET li_total_trab1        = 0
   LET li_total_trab2        = 0
   LET li_total_trab3        = 0
   LET li_total_trab4        = 0
   LET li_total_trab5        = 0
   LET li_total_trab_ver     = 0

   SELECT COUNT(*)
   INTO   li_procesar
   FROM   multisie_anexo_b
   WHERE  sector = 0

   SELECT COUNT(*)
   INTO   li_procesar1
   FROM   multisie_anexo_b
   WHERE  sector = 0
   AND    siefore_rec = 1

   SELECT COUNT(*)
   INTO   li_procesar2
   FROM   multisie_anexo_b
   WHERE  sector = 0
   AND    siefore_rec = 2

   SELECT COUNT(*)
   INTO   li_procesar3
   FROM   multisie_anexo_b
   WHERE  sector = 0
   AND    siefore_rec = 3

   SELECT COUNT(*)
   INTO   li_procesar4
   FROM   multisie_anexo_b
   WHERE  sector = 0
   AND    siefore_rec = 4

   SELECT COUNT(*)
   INTO   li_procesar5
   FROM   multisie_anexo_b
   WHERE  sector = 0
   AND    siefore_rec = 5

   SELECT COUNT(*)
   INTO   li_independientes
   FROM   multisie_anexo_b
   WHERE  sector <> 0

   SELECT COUNT(*)
   INTO   li_independientes1
   FROM   multisie_anexo_b
   WHERE  sector <> 0
   AND    siefore_rec = 1

   SELECT COUNT(*)
   INTO   li_independientes2
   FROM   multisie_anexo_b
   WHERE  sector <> 0
   AND    siefore_rec = 2

   SELECT COUNT(*)
   INTO   li_independientes3
   FROM   multisie_anexo_b
   WHERE  sector <> 0
   AND    siefore_rec = 3

   SELECT COUNT(*)
   INTO   li_independientes4
   FROM   multisie_anexo_b
   WHERE  sector <> 0
   AND    siefore_rec = 4

   SELECT COUNT(*)
   INTO   li_independientes5
   FROM   multisie_anexo_b
   WHERE  sector <> 0
   AND    siefore_rec = 5

   LET li_procesar_hor = li_procesar1 +
                         li_procesar2 +
                         li_procesar3 +
                         li_procesar4 +
                         li_procesar5

   LET li_independientes_hor = li_independientes1 +
                               li_independientes2 +
                               li_independientes3 +
                               li_independientes4 +
                               li_independientes5

   SELECT COUNT(*)
   INTO   li_total_trab
   FROM   multisie_anexo_b

   SELECT COUNT(*)
   INTO   li_total_trab1
   FROM   multisie_anexo_b
   WHERE  siefore_rec = 1

   SELECT COUNT(*)
   INTO   li_total_trab2
   FROM   multisie_anexo_b
   WHERE  siefore_rec = 2

   SELECT COUNT(*)
   INTO   li_total_trab3
   FROM   multisie_anexo_b
   WHERE  siefore_rec = 3

   SELECT COUNT(*)
   INTO   li_total_trab4
   FROM   multisie_anexo_b
   WHERE  siefore_rec = 4

   SELECT COUNT(*)
   INTO   li_total_trab5
   FROM   multisie_anexo_b
   WHERE  siefore_rec = 5

   LET li_total_trab_ver = li_total_trab1 +
                           li_total_trab2 +
                           li_total_trab3 +
                           li_total_trab4 +
                           li_total_trab5

   PRINT COLUMN 001, "      PROCESAR        SB1     SB2     SB3     SB4     SB5    TOTAL DIREFENCIA"
   PRINT COLUMN 008, li_procesar                          USING "######&",
   	     COLUMN 019, li_procesar1                         USING "######&",
   	     COLUMN 027, li_procesar2                         USING "######&",
   	     COLUMN 035, li_procesar3                         USING "######&",
   	     COLUMN 043, li_procesar4                         USING "######&",
   	     COLUMN 051, li_procesar5                         USING "######&",
   	     COLUMN 060, li_procesar_hor                      USING "######&",
   	     COLUMN 071, li_procesar - li_procesar_hor        USING "######&"
   	     
   SKIP 1 LINE
   PRINT COLUMN 001, "INDEPENDIENTES        SB1     SB2     SB3     SB4     SB5    TOTAL DIREFENCIA"
   PRINT COLUMN 008, li_independientes                          USING "######&",
   	     COLUMN 019, li_independientes1                         USING "######&",
   	     COLUMN 027, li_independientes2                         USING "######&",
   	     COLUMN 035, li_independientes3                         USING "######&",
   	     COLUMN 043, li_independientes4                         USING "######&",
   	     COLUMN 051, li_independientes5                         USING "######&",
   	     COLUMN 060, li_independientes_hor                      USING "######&",
   	     COLUMN 071, li_independientes - li_independientes_hor  USING "######&"

   PRINT COLUMN 001, "--------------------------------------------------------------------------------"
   PRINT COLUMN 001, "TOTAL",
         COLUMN 008, li_total_trab                      USING "######&",
         COLUMN 019, li_total_trab1                     USING "######&",
         COLUMN 027, li_total_trab2                     USING "######&",
         COLUMN 035, li_total_trab3                     USING "######&",
         COLUMN 043, li_total_trab4                     USING "######&",
         COLUMN 051, li_total_trab5                     USING "######&",
         COLUMN 060, li_total_trab_ver                  USING "######&",
         COLUMN 071, li_total_trab - li_total_trab_ver  USING "######&"
         
   SKIP 1 LINE
   PRINT COLUMN 001, "--------------------------------------------------------------------------------"
   PRINT COLUMN 001, "                                   DIFERENCIAS"
   PRINT COLUMN 001, "--------------------------------------------------------------------------------"
   PRINT COLUMN 008, li_total_ced_ver - li_total_trab     USING "######&",
         COLUMN 019, li_tot_rec_1_ver - li_total_trab1    USING "######&",
         COLUMN 027, li_tot_rec_2_ver - li_total_trab2    USING "######&",
         COLUMN 035, li_tot_rec_3_ver - li_total_trab3    USING "######&",
         COLUMN 043, li_tot_rec_4_ver - li_total_trab4    USING "######&",
         COLUMN 051, li_tot_rec_5_ver - li_total_trab5    USING "######&",
         COLUMN 060, li_total_rec_ver - li_total_trab_ver USING "######&",
         COLUMN 071, (li_total_ced_ver - li_total_rec_ver)
                     - (li_total_trab - li_total_trab_ver)    USING "######&"

END REPORT
