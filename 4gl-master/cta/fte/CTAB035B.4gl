#******************************************************************************#
#Proyecto     => safre_af                                                      #
#Propietario  => E.F.P.                                                        #
#Programa     => CTAB035                                                       #
#Descripcion  => MULTISIEFORES                                                 #
#Fecha        => 26 de septiembre de 2007.                                     #
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ                                   #
#Sistema      => CTA.                                                          #
#Fecha               => 27 de Diciembre de 2007                                #
#******************************************************************************#

DATABASE safre_af
################################################################################
GLOBALS
   DEFINE gc_mensaje1           ,
          gc_mensaje2           ,
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(8)  ,
          gc_respuesta          CHAR(001)

   DEFINE g_cancela       SMALLINT

   DEFINE gd_fecha_corte  DATE

   DEFINE arr_c,
          arr_l,
          arr_t      SMALLINT,
          USER       CHAR(08),
          hoy        DATE,
          pos        SMALLINT,
          pos_subcta SMALLINT

   DEFINE gi_siefore,
          gi_estatus SMALLINT


END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT usuario
   INTO   USER
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY
   CALL proceso()
END MAIN
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB0351" ATTRIBUTE(BORDER)
           #-----------------------------------------------------------------------------
   DISPLAY "CTAB035B           CORTE   MULTISIEFORES          < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "CORTE MULTISIEFORES"
      COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
         CALL lee_paso(1)
      COMMAND "Prepara NSS" "Identifica NSS del periodo"
         CALL lee_paso(2)
      COMMAND "Consulta" "Consulta de saldos para corte multisiefore"
         CALL consulta(1)
      COMMAND "Consulta Estimada" "Consulta estimada de saldos"
         CALL consulta(2)
      COMMAND "Archivo 2 siefores" "Genera reporte del corte multisiefore"
         CALL lee_paso(3)
      COMMAND "Archivo 5 siefores" "Genera reporte del corte multisiefore"
         CALL lee_paso(4)
      COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
         CALL revisa_proceso()
      COMMAND "Elimina ejecucion" "Elimina la ejecución de la fecha indicada"
         CALL elimina_ejecucion(5)
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
      WHEN 2
      	 LET gc_mensaje1 = "ESTE PROCESO IDENTIFICARA LOS NSS DE LA FECHA INDICADA",
                           "LA INFORMACIÓN DEL CORTE MULTISIEFORE ANTERIOR SE BORRARÁ."
   END CASE

   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"
   LET gc_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"
   LET g_cancela = FALSE

   INITIALIZE gd_fecha_corte TO NULL
   LET gd_fecha_corte = "10/31/2007"

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
            	 IF valida_dia(gd_fecha_corte) != 0 THEN
            	    NEXT FIELD gd_fecha_corte
            	 END IF
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
   FROM cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO cta_folio
      VALUES (li_folio_proceso)
   END IF

   INSERT INTO dis_ctrl_proceso
   VALUES (ld_fecha_corte,          -- fecha_proceso
           "CTAB035",               -- proceso_cod
           li_paso,                 -- etapa_cod   -- LECTURA
           lc_hora_inicio,          -- hora_inicial
           NULL,                    -- hora_final
           ld_fecha_inicio,         -- parametro1 --fecha_inicio
           NULL,                    -- parametro2 --fecha_fin
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           li_folio_proceso,        -- folio
           lc_resultado,            -- resultado
           USER,                    -- usuario
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

   UPDATE dis_ctrl_proceso
   SET    hora_final = lc_hora_fin,
          parametro2 = ld_fecha_fin,
          usuario    = USER,
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
   FROM   dis_ctrl_proceso
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

   WHENEVER ERROR CONTINUE
      DROP TABLE cta_cuota_multisie_nss
      DROP TABLE cta_estatus_saldos
      DROP TABLE cta_estatus_total_sie

      DROP TABLE cta_estatus_saldos_est;
      DROP TABLE cta_estatus_total_sie_est;
      DROP TABLE safre_tmp:glo_valor_accion
   WHENEVER ERROR STOP

   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
      DROP TABLE safre_tmp:glo_valor_accion
   WHENEVER ERROR STOP

   DATABASE safre_af

   CREATE TABLE cta_cuota_multisie_nss
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

  CREATE TABLE cta_estatus_total_sie
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore           SMALLINT,
   total_nss         INTEGER,
   monto_en_acciones DECIMAL(22,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE cta_estatus_saldos
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore           SMALLINT,
   subcuenta         SMALLINT,
   monto_en_acciones DECIMAL(16,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE cta_estatus_total_sie_est
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore_ced       SMALLINT,
   siefore           SMALLINT,
   total_nss         INTEGER,
   monto_en_acciones DECIMAL(22,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  CREATE TABLE cta_estatus_saldos_est
  (estatus_cuenta    SMALLINT,
   fecha_corte       DATE,
   siefore_ced       SMALLINT,
   siefore           SMALLINT,
   subcuenta         SMALLINT,
   monto_en_acciones DECIMAL(16,6)
  )

  LET li_resultado = li_resultado + SQLCA.SQLCODE

  DATABASE safre_tmp
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
  );

  DATABASE safre_af

  LET li_resultado = li_resultado + SQLCA.SQLCODE

#CARGA GLO_VALOR_ACCION
  INSERT INTO safre_tmp:glo_valor_accion
  SELECT *
  FROM   safre_af:glo_valor_accion
  WHERE  fecha_valuacion = gd_fecha_corte

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

   IF li_paso = 4 THEN
   	  LET li_paso_ant = li_paso_ant - 1
   END IF

   IF li_paso > 1 THEN
       --  Verifica ejecucion del paso anterior
      SELECT etapa_cod, --paso
             parametro2 --fecha_fin
      INTO   li_existe,
             ld_fecha_fin
      FROM   dis_ctrl_proceso
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
   FROM   dis_ctrl_proceso
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
      WHEN 2 --Identifica NSS
      	 --VERIFICA CURP REPETIDAS
      	 DECLARE cur_curp_rep CURSOR FOR
      	 SELECT curp, COUNT(*)
         FROM   cta_ctr_reg_ind
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

      	 SELECT "X"
      	 FROM   safre_tmp:cta_ctr_fmto_a
      	 WHERE  fecha_corte = gd_fecha_corte
      	 AND    paso = 2
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "LA INFORMACION DE LA TABLA tmp_slado_corte AÚN NO ESTA LISTA"
         ELSE
         	  CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Identificacion de NSS Multisiefore")
      	    LET lc_comando = "nohup time ./eje_multisie_nss.sh ",
      	                     gd_fecha_corte," ",
                             USER CLIPPED,
                             " 1> multisie_nss.salida ",
                             " 2> multisie_nss.error &"
            ERROR "EJECUTANDO PROCESO POR NOHUP."
            SLEEP 2
            ERROR ""
            RUN   lc_comando
            ERROR "VERIFIQUE ARCHIVOS: multisie_nss.salida Y multisie_nss.error"
            SLEEP 3
         END IF
      WHEN 3 --Reporte con 2 Siefores
      	 SELECT "X"
      	 FROM   safre_tmp:cta_ctr_fmto_a
      	 WHERE  fecha_corte = gd_fecha_corte
      	 AND    paso = 2
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "LA INFORMACION DE LA TABLA tmp_slado_corte AÚN NO ESTA LISTA"
         ELSE
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

         END IF
      WHEN 4 --Reporte con 5 Siefores
      	 SELECT "X"
      	 FROM   safre_tmp:cta_ctr_fmto_a
      	 WHERE  fecha_corte = gd_fecha_corte
      	 AND    paso = 2
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "LA INFORMACION DE LA TABLA tmp_slado_corte AÚN NO ESTA LISTA"
         ELSE
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
         END IF
      WHEN 5 --Borra Ejecucion
      	 SELECT "X"
      	 FROM   dis_ctrl_proceso
      	 WHERE  fecha_proceso = gd_fecha_corte
      	 AND    proceso_cod = "CTAB035"
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "NO EXISTE EJECUCIÓN PARA ESTA FECHA"
         ELSE
      	    DELETE
      	    FROM  dis_ctrl_proceso
      	    WHERE fecha_proceso = gd_fecha_corte
      	    AND   proceso_cod = "CTAB035"

            ERROR "EJECUCION ELIMINADA."
            SLEEP 2
            ERROR ""
         END IF
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
          lc_sql     CHAR(400)

   DEFINE lar_consulta ARRAY[3] OF RECORD
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
                                 "2,'PREPARA NSS ', ",
                                 "3,'REPORTE     '  ",
                        ") desc_paso, ",
                "parametro1 || hora_inicial, ",
                "parametro2 || hora_final,   ",
                "usuario ",
                "FROM   dis_ctrl_proceso ",
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
   FROM cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO cta_folio
      VALUES (li_folio_proceso)
   END IF

   SELECT COUNT(*)
   INTO   li_totnss
   FROM   cta_cuota_multisie_nss

   INSERT INTO dis_ctrl_proceso
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
           USER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso ",STATUS
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

   LET gd_fecha_corte = "11/30/2007"
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
            IF valida_proceso(3, gd_fecha_corte) != 0 THEN
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
                         "FROM   cta_estatus_total_sie ",
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
                         "FROM   cta_estatus_total_sie_est ",
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
         LET lc_query = "SELECT b.subct_prc, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   cta_estatus_saldos a, "  ,
                               "tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_prc IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
      WHEN 2 --CONSULTA FUTURA
         LET lc_query = "SELECT b.subct_prc, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   cta_estatus_saldos_est a, "  ,
                               "tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_prc IN (1,2,3,5,8,13,15,17,18,22) ",
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
   	        FROM   glo_valor_accion
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
         LET lc_query = "SELECT b.subct_prc, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   cta_estatus_saldos a, "  ,
                               "tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_prc IN (1,2,3,5,8,13,15,17,18,22) ",
                        "GROUP BY 1 ",
                        "ORDER BY 1 "
      WHEN 2 --CONSULTA FUTURA
         LET lc_query = "SELECT b.subct_prc, "           ,
                              "SUM(a.monto_en_acciones) ",
                        "FROM   cta_estatus_saldos_est a, "  ,
                               "tab_subcuenta b "        ,
                        "WHERE  a.siefore        = ", li_siefore,  " ",
                        "AND    a.estatus_cuenta = ", li_estatus,  " ",
                        "AND    a.subcuenta      = b.subct_cod ",
                        "AND    a.fecha_corte   = '", ld_fecha_corte, "' ",
                        "AND    b.subct_prc IN (1,2,3,5,8,13,15,17,18,22) ",
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
   	        FROM   glo_valor_accion
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
   LET gd_fecha_corte = "10/31/2007"

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
