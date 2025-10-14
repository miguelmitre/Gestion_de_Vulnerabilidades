################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX23    => ASIGNACION DE CUENTAS ASIGNADAS                        #
#                       A PRESTADORA DE SERVICIOS                              #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 28 de MARZO DE 2011                                    #
################################################################################
DATABASE safre_tmp
################################################################################
GLOBALS
  DEFINE  gd_fecha_corte   DATE

  DEFINE gs_afore     SMALLINT
  DEFINE hoy          DATE

  DEFINE gc_log    CHAR(50)

    DEFINE gr_seg_modulo RECORD
  	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
  END RECORD

  DEFINE gc_mensaje1           CHAR(100),
         gc_mensaje2           CHAR(100),
         gc_mensaje3           CHAR(100),
         gc_mensaje4           CHAR(100),
         gc_usuario            CHAR(008)

  DEFINE gc_archivo            CHAR(100)
  DEFINE gd_total_nss DECIMAL(10,0)
  DEFINE gs_unifica   SMALLINT

  DEFINE gi_paso SMALLINT

  DEFINE gr_parametros RECORD
  	  cve_proceso    CHAR(10),
  	  archivo_salida CHAR(50),
  	  archivo_error  CHAR(50)
  END RECORD

END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O
   DEFER INTERRUPT

   CALL init()
   CALL STARTLOG(gc_log)
   CALL proceso()
END MAIN
################################################################################
FUNCTION init()
   LET gr_seg_modulo.modulo_cod = "ctx"

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   LET hoy    = TODAY

   LET gc_log = gc_usuario CLIPPED, "CTANX23.log"
   LET gs_unifica = 1
   
   LET gr_parametros.cve_proceso = "CTANX24"
END FUNCTION
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTANX231" ATTRIBUTE(BORDER)
           #-----------------------------------------------------------------------------
   --DISPLAY "CTANX23                  HOMOLOGACION DE BASES DE DATOS  <Ctrl-C> SALIR" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "CTANX23                  HOMOLOGACION DE BASES DE DATOS                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "Homologacion"
      COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior"
         CALL lee_paso(1)

      COMMAND "Cuota" "Identifica NSS del periodo"
         CALL lee_paso(2)

      COMMAND "Archivo" "Reporte de Homologacion"
         CALL lee_paso(3)

      COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
         CALL revisa_proceso()

      COMMAND "Elimina ejecucion" "Elimina la ejecución de la fecha indicada"
         CALL elimina_ejecucion(10)

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
END FUNCTION
################################################################################
FUNCTION lee_paso(li_paso)

   DEFINE li_paso  SMALLINT,
          lc_opc   CHAR(1)

   DEFINE lc_error_msg CHAR(40)

   DISPLAY "CTANX23                  HOMOLOGACION DE BASES DE DATOS  <Ctrl-C> SALIR" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   CASE li_paso
   	  WHEN 1
   	  	 #Inicialización
   	  	 LET gc_mensaje1  = "ESTE PROCESO REINICIALIZARA LAS TABLAS DEL PROCESO"
   	     LET lc_error_msg = "DEBE INDICAR FECHA DE CORTE"
      WHEN 2
   	  	 #Cuota
   	  	 LET gc_mensaje1  = "ESTE PROCESO IDENTIFICARA LOS NSS"
   	     LET lc_error_msg = "DEBE INDICAR FECHA DE CORTE"
   	  WHEN 3
   	  	 #Archivo
   	  	 LET gc_mensaje1  = "REPORTE DE HOMOLOGACION"
   	     LET lc_error_msg = "DEBE INDICAR FECHA DE CORTE"
   END CASE

   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"
   LET gc_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

   INITIALIZE gd_fecha_corte TO NULL
   --LET gd_fecha_corte = MDY (3,31,2011)

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
         	  --VERIFICA SI YA SE HA EJECUTADO LA PREPARACION PARA LA FECHA CORTE
         	  IF li_paso = 2 THEN
         	  	 IF valida_proceso71(li_paso,gd_fecha_corte) != 0 THEN
         	  	 	  NEXT FIELD gd_fecha_corte
               END IF
         	  END IF

            IF valida_proceso(li_paso,gd_fecha_corte) != 0 THEN
               NEXT FIELD gd_fecha_corte
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

   DISPLAY "CTANX23                  HOMOLOGACION DE BASES DE DATOS                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)
END FUNCTION
################################################################################
FUNCTION valida_proceso71(li_paso, ld_fecha_corte)
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

   IF li_paso = 2 THEN    --Cuota
   	  LET li_paso_ant = 3 --PREPARACION DE NSS
   END IF

   #Verifica ejecucion del paso anterior
   SELECT etapa_cod, --paso
          parametro2 --fecha_fin
   INTO   li_existe,
          ld_fecha_fin
   FROM   safre_af:dis_ctrl_proceso
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso_ant
   AND    proceso_cod   = "CTANX01"

   IF li_existe IS NULL THEN
      ERROR "LA PREPARACI0N DEL ANEXO 71 NO HA CONCLUIDO"
      SLEEP 2
      ERROR ""
      LET li_status = 1
   ELSE
      IF ld_fecha_fin IS NULL THEN
         ERROR "LA PREPARACI0N DEL ANEXO 71 CONTINUA EJECUTANDOSE"
         SLEEP 2
         ERROR ""
         LET li_status = 1
      END IF
   END IF

   RETURN li_status
END FUNCTION
################################################################################
FUNCTION valida_proceso (li_paso, ld_fecha_corte)
   DEFINE li_paso         ,
          li_paso_ant     ,
          li_status       ,
          li_existe       SMALLINT

   DEFINE ld_fecha_corte  DATE,
          ld_fecha_fin   DATE

   LET li_status      = 0
   LET ld_fecha_fin   = NULL
   LET li_existe      = NULL

   LET li_paso_ant = li_paso - 1

   IF li_paso > 1 THEN
      --Verifica ejecucion del paso anterior
      SELECT etapa_cod ,--paso
             parametro2 --fecha_fin
      INTO   li_existe,
             ld_fecha_fin
      FROM   safre_af:dis_ctrl_proceso
      WHERE  fecha_proceso = ld_fecha_corte
      AND    etapa_cod     = li_paso_ant
      AND    proceso_cod   = "CTANX23"

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

   IF li_paso >= 1 THEN
      --Verifica si ya se ha ejecutado el proceso anteriormente
      SELECT etapa_cod,
             parametro2 --fecha_fin
      INTO   li_existe,
             ld_fecha_fin
      FROM   safre_af:dis_ctrl_proceso
      WHERE  fecha_proceso = ld_fecha_corte
      AND    etapa_cod     = li_paso
      AND    proceso_cod   = "CTANX23"

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
   END IF

   RETURN li_status
END FUNCTION
################################################################################
FUNCTION ejecuta_paso(li_paso)
   DEFINE li_paso          SMALLINT,
          lc_comando       CHAR(300),
          li_totnss        INTEGER,
          li_folio_proceso INTEGER,
          lc_msg           CHAR(400),
          lc_ok            CHAR(1)

   DEFINE lr_curp_rep      RECORD
   	         curp CHAR(18),
   	         num  SMALLINT
   	      END RECORD
   DEFINE lc_prioridad CHAR (100)

   DEFINE lc_sql CHAR(300)

   DEFINE ls_prueba SMALLINT
   DEFINE ls_resultado SMALLINT
   DEFINE lc_reporte   CHAR(20)
   DEFINE lc_enter     CHAR(01)


   CASE li_paso
   	  WHEN 1 --Inicializa
   	  	 CALL inserta_paso(li_paso, gd_fecha_corte, "Empieza Inicializacion")
   	  	 IF borra_informacion(0,0) = 0 THEN
            CALL finaliza_paso(li_paso, gd_fecha_corte, "Finaliza Inicializacion")
            ERROR "INICIALIZACION EFECTUADA."
         ELSE
            CALL borra_paso(li_paso, gd_fecha_corte)
            ERROR "NO SE PUDO INICIALIZAR, AVISE AL ADMINISTRADOR."
         END IF
      WHEN 2 --Cuota
      	 CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Identificacion de Cuota")
      	 --LET lc_comando = "nohup time ./eje_cuota_homologa.sh ",
      	 --                 gd_fecha_corte," ",
         --                 gc_usuario CLIPPED,
         --                 " 1> ", gc_usuario CLIPPED, ".homologa_cuota.salida ",
         --                 " 2> ", gc_usuario CLIPPED, ".homologa_cuota.error &"
         --ERROR "EJECUTANDO PROCESO POR NOHUP."
         --SLEEP 2
         --ERROR ""
         --RUN   lc_comando
         --ERROR "VERIFIQUE ARCHIVOS: ", gc_usuario CLIPPED, ".homologa_cuota.salida Y ", gc_usuario CLIPPED,".homologa_cuota.error"
         --SLEEP 3

         #Archivos de salida y error
         LET gr_parametros.archivo_salida = gc_usuario CLIPPED,
                                            ".homologa_cuota.salida"
         LET gr_parametros.archivo_error  = gc_usuario CLIPPED,
                                            ".homologa_cuota.error"

         #Ejecutar proceso
         LET lc_comando = "nohup time ./eje_cuota_homologa.sh ",
      	                  gd_fecha_corte," ",
                          gc_usuario CLIPPED,
                          " 1> ", gr_parametros.archivo_salida CLIPPED,
                          " 2> ", gr_parametros.archivo_error  CLIPPED, " &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando

         #Desplegar datos en pantalla
         DISPLAY "PROCESO EJECUTADO POR NOHUP"                                        AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY "VERIFIQUE ARCHIVO SALIDA  : ", gr_parametros.archivo_salida CLIPPED AT 19,1 ATTRIBUTE(REVERSE)
         DISPLAY "VERIFIQUE ARCHIVO ERROR   : ", gr_parametros.archivo_error  CLIPPED AT 20,1 ATTRIBUTE(REVERSE)

         PROMPT "<ENTER PARA CONTINUAR>" FOR CHAR lc_enter
         DISPLAY "                                                                           " AT 18,1
         DISPLAY "                                                                           " AT 19,1
         DISPLAY "                                                                           " AT 20,1
      WHEN 3 --Archivo 2004
      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX24") RETURNING li_totnss, li_folio_proceso

      	 --LET lc_comando = "nohup time fglgo CTANX24.4gi " CLIPPED,
         --                  li_folio_proceso CLIPPED, " ",
         --                  gd_fecha_corte   CLIPPED, " ",
         --                  li_totnss        CLIPPED, " ",
         --                  "1> ", gc_usuario CLIPPED, ".rtp_homologa.salida ",
         --                  "2> ", gc_usuario CLIPPED, ".rtp_homologa.error &"
         --
         --ERROR "EJECUTANDO PROCESO POR NOHUP."
         --SLEEP 2
         --RUN lc_comando
         --ERROR "VERIFIQUE ARCHIVOS: ", gc_usuario CLIPPED, ".rtp_homologa.salida Y ", gc_usuario CLIPPED, ".rtp_homologa.error"
         --SLEEP 3
         --ERROR ""
         --
         --LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX24"
         --PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

         #Archivos de salida y error
         LET gr_parametros.archivo_salida = gc_usuario CLIPPED,
                                            ".rpt_homologa.salida"
         LET gr_parametros.archivo_error  = gc_usuario CLIPPED,
                                            ".rpt_homologa.error"

         #Ejecutar programa de reporte
         LET lc_comando = "nohup time fglgo CTANX24.4gi " CLIPPED,
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           " 1> ", gr_parametros.archivo_salida CLIPPED,
                           " 2> ", gr_parametros.archivo_error  CLIPPED, " &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando

         #Desplegar datos en pantalla
         DISPLAY "PROCESO EJECUTADO POR NOHUP"                                        AT 16,1 ATTRIBUTE(REVERSE)
         DISPLAY "FOLIO DEL PROCESO         : ", li_folio_proceso                     AT 17,1 ATTRIBUTE(REVERSE)
         DISPLAY "CLAVE DEL PROCESO         : ", gr_parametros.cve_proceso    CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY "VERIFIQUE ARCHIVO SALIDA  : ", gr_parametros.archivo_salida CLIPPED AT 19,1 ATTRIBUTE(REVERSE)
         DISPLAY "VERIFIQUE ARCHIVO ERROR   : ", gr_parametros.archivo_error  CLIPPED AT 20,1 ATTRIBUTE(REVERSE)

         PROMPT "<ENTER PARA CONTINUAR>" FOR CHAR lc_enter
         DISPLAY "                                                                           " AT 16,1
         DISPLAY "                                                                           " AT 17,1
         DISPLAY "                                                                           " AT 18,1
         DISPLAY "                                                                           " AT 19,1
         DISPLAY "                                                                           " AT 20,1

      WHEN 10 --Borra Ejecucion
      	 SELECT "X"
      	 FROM   safre_af:dis_ctrl_proceso
      	 WHERE  fecha_proceso = gd_fecha_corte
      	 AND    proceso_cod   = "CTANX23"
      	 AND    etapa_cod     = gi_paso
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "NO EXISTE EJECUCIÓN PARA ESTA FECHA"
         ELSE
         	  ERROR "PROCESANDO INFORMACION"

      	    DELETE
      	    FROM  safre_af:dis_ctrl_proceso
      	    WHERE fecha_proceso = gd_fecha_corte
      	    AND   proceso_cod   = "CTANX23"
      	    AND   etapa_cod     = gi_paso

      	    CALL borra_informacion(1,gi_paso) RETURNING ls_resultado

            ERROR "EJECUCION ELIMINADA."
            SLEEP 2
            ERROR ""
         END IF
   END CASE
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
   VALUES (ld_fecha_corte  ,        -- fecha_proceso
           "CTANX23"       ,        -- proceso_cod
           li_paso         ,        -- etapa_cod    -- LECTURA
           lc_hora_inicio  ,        -- hora_inicial
           NULL            ,        -- hora_final
           ld_fecha_inicio ,        -- parametro1   --fecha_inicio
           NULL            ,        -- parametro2   --fecha_fin
           NULL            ,        -- parametro3
           NULL            ,        -- parametro4
           NULL            ,        -- parametro5   --fecha_valuacion
           li_folio_proceso,        -- folio
           lc_resultado    ,        -- resultado
           gc_usuario      ,        -- usuario
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
          usuario    = gc_usuario,
          resultado  = lc_resultado
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTANX23"
END FUNCTION
################################################################################
FUNCTION borra_paso(li_paso, ld_fecha_corte)
   DEFINE li_paso        SMALLINT,
          ld_fecha_corte DATE

   DELETE
   FROM   safre_af:dis_ctrl_proceso
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTANX23"
END FUNCTION
################################################################################
FUNCTION elimina_ejecucion(li_paso)
   DEFINE li_paso SMALLINT,
          lc_opc  CHAR(1)

   LET gc_mensaje1 = "ELIMINAR UN PASO"
   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"
   LET gc_mensaje3 = "INDIQUE PASO:"

   INITIALIZE gd_fecha_corte, gi_paso TO NULL
   --LET gd_fecha_corte = "10/31/2007"

   INPUT BY NAME gd_fecha_corte,
   	             gi_paso         WITHOUT DEFAULTS
      BEFORE INPUT
         DISPLAY BY NAME gc_mensaje1,
                         gc_mensaje2,
                         gc_mensaje3

      AFTER FIELD gd_fecha_corte
         IF gd_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE"
            SLEEP 2
            ERROR ""
            NEXT FIELD gd_fecha_corte
         END IF

      AFTER FIELD gi_paso
         IF gi_paso IS NULL THEN
            ERROR "DEBE INDICAR EL PASO"
            SLEEP 2
            ERROR ""
            NEXT FIELD gi_paso
         ELSE
         	  --Validar que haya algo que borrar
         	  SELECT "X"
            FROM   safre_af:dis_ctrl_proceso
            WHERE  fecha_proceso = gd_fecha_corte
            AND    etapa_cod     = gi_paso
            AND    proceso_cod   = "CTANX23"
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "PROCESO NO EXISTE, PARA ESTA FECHA."
               SLEEP 3
               ERROR ""
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
FUNCTION borra_informacion(ls_tipo, ls_paso)
   DEFINE ls_tipo     ,
          ls_paso     ,
          li_resultado,
          li_cont     SMALLINT

   DEFINE lc_comando CHAR(400)

   WHENEVER ERROR CONTINUE
      IF ls_tipo = 0 OR
   	    (ls_tipo = 1 AND
   	     ls_paso = 2) THEN --cuota

         DROP TABLE safre_tmp:tmp_cuota_homologa
      END IF
   WHENEVER ERROR STOP

   LET li_resultado = 0

   IF ls_tipo = 0 OR
      (ls_tipo = 1 AND
       ls_paso = 2) THEN --cuota

      CREATE TABLE safre_tmp:tmp_cuota_homologa
      (
        nss              CHAR(11)     ,
        tipo_solicitud   SMALLINT     ,
        curp             CHAR(18)     ,
        rfc              CHAR(13)     ,
        paterno          CHAR(40)     ,
        materno          CHAR(40)     ,
        nombres          CHAR(40)     ,
        sexo             SMALLINT     ,
        fecha_nacimiento DATE         ,
        estadon          SMALLINT     ,
        tipo_cuenta      CHAR(01)     ,
        ind_tram_jud     SMALLINT     ,
        ind_pensionado   SMALLINT     ,
        ind_edad         SMALLINT     ,
        factualiza       DATE         ,
        usuario          CHAR(08)
      )

      LET li_resultado = li_resultado + SQLCA.SQLCODE
   END IF

   RETURN li_resultado
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

   LET li_totnss = 0

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES (TODAY           ,   -- fecha_proceso
           lc_proceso_cod  ,   -- proceso_cod
           1               ,   -- etapa_cod   -- LECTURA
           lc_hora_inicial ,   -- hora_inicial
           NULL            ,   -- hora_final
           "rpt_homologa"  ,   -- parametro1
           ld_fecha_corte  ,   -- parametro2 --fecha_corte
           li_totnss       ,   -- parametro3 --tot_nss
           NULL            ,   -- parametro4
           NULL            ,   -- parametro5
           li_folio_proceso,   -- folio
           NULL            ,   -- resultado
           gc_usuario      ,   -- usuario
           0                   -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA safre_af:dis_ctrl_proceso ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   RETURN li_totnss, li_folio_proceso
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

   OPEN WINDOW  ventana_2 AT 6,4 WITH FORM "CTANX142" ATTRIBUTES(BORDER)

   LET lc_sql = "SELECT etapa_cod, ",
                "DECODE(etapa_cod,1,'INICIALIZA  ', ",
                                 "2,'CUOTA       '  ",

                        ") desc_paso, ",
                "parametro1 || hora_inicial, ",
                "parametro2 || hora_final,   ",
                "usuario ",
                "FROM   safre_af:dis_ctrl_proceso ",
                "WHERE  fecha_proceso = '", ld_fecha_corte, "' ",
                "AND    proceso_cod   = 'CTANX23' ",
                "ORDER BY etapa_cod "

   PREPARE p_proceso FROM lc_sql

   DECLARE cur_proceso CURSOR FOR p_proceso

   LET li_pos = 1
   FOREACH cur_proceso INTO lar_consulta[li_pos].paso        ,
                            lar_consulta[li_pos].desc_paso   ,
                            lc_fecha_inicio                  ,
                            lc_fecha_fin                     ,
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