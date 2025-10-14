#******************************************************************************#
#Proyecto     => safre_af                                                      #
#Propietario  => E.F.P.                                                        #
#Programa     => CTANX14                                                       #
#Descripcion  => SALDOS ISSSTE                                                 #
#Fecha        => 04 DE JUNIO 2010                                              #
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ                                   #
#Sistema      => CTA.                                                          #
#******************************************************************************#

DATABASE safre_tmp
################################################################################
GLOBALS
   DEFINE gc_mensaje1           CHAR(100),
          gc_mensaje2           CHAR(100),
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(008)

   DEFINE gd_fecha_corte  DATE

   DEFINE GUSER      CHAR(08),
          HOY        DATE

   DEFINE gr_seg_modulo RECORD
   	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
   END RECORD

   DEFINE gi_afore   ,
          gi_paso    ,
          gi_activos SMALLINT
          
   DEFINE gs_mes SMALLINT
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

   SELECT codigo_afore
   INTO   gi_afore
   FROM   safre_af:tab_afore_local

   LET gr_seg_modulo.modulo_cod = "cta"

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   LET hoy    = TODAY
   LET gs_mes = MONTH(hoy)
   CALL proceso()
END MAIN
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTANX141" ATTRIBUTE(BORDER)
           #-----------------------------------------------------------------------------
   DISPLAY "CTANX14       SALDOS CUENTAS ISSSTE POR REGIMEN   < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   IF gi_afore = 578 THEN -- PENSIONISSSTE
      MENU "SALDOS CTAS ISSSTE"
         COMMAND "Preparacion" "Preparar Informacion"
         MENU "Preparacion"
            COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
               CALL lee_paso(1)
            COMMAND "Saldos" "Genera saldos a la fecha de corte"
               CALL lee_paso(2)
            COMMAND "Cuota" "Identifica NSS del periodo"
               CALL lee_paso(3)
            COMMAND "Regresar" "Regresar al menu anterior"
               EXIT MENU
         END MENU

         COMMAND "Archivos ACTIVOS" "Generación de archivos de Activos"
         MENU "Archivos"
           COMMAND "Formato 2004" "Reporte Formato 2004"
              LET gi_activos = 1
              CALL lee_paso(4)

           COMMAND "Formato 2005" "Reporte Formato 2005"
              LET gi_activos = 1
              CALL lee_paso(5)

           COMMAND "Formato 2006" "Reporte Formato 2006"
              LET gi_activos = 1
              CALL lee_paso(6)

           COMMAND "Formato 2007" "Reporte Formato 2007"
              LET gi_activos = 1
              CALL lee_paso(7)

           COMMAND "Formato 2009" "Reporte Formato 2009"
              LET gi_activos = 1
              CALL lee_paso(9)

           COMMAND "Regresar" "Regresar al menu anterior"
              EXIT MENU
         END MENU

         COMMAND "Archivos INACTIVOS" "Generación de archivos de inactivos"
         MENU "Archivos"
           COMMAND "Formato 2006" "Reporte Formato 2006"
              LET gi_activos = 0
              CALL lee_paso(6)

           COMMAND "Formato 2007" "Reporte Formato 2007"
              LET gi_activos = 0
              CALL lee_paso(7)

           COMMAND "Regresar" "Regresar al menu anterior"
              EXIT MENU
         END MENU

         COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
            CALL revisa_proceso()

         COMMAND "Elimina ejecucion" "Elimina la ejecución de la fecha indicada"
            CALL elimina_ejecucion(10)

         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
      END MENU
   ELSE
      MENU "SALDOS CTAS ISSSTE"
         COMMAND "Preparacion" "Preparar Informacion"
         MENU "Preparacion"
            COMMAND "Inicializa" "Limpia estructura, elimina informacion anterior."
               CALL lee_paso(1)
            COMMAND "Saldos" "Genera saldos a la fecha de corte"
               CALL lee_paso(2)
            COMMAND "Cuota" "Identifica NSS del periodo"
               CALL lee_paso(3)
            COMMAND "Regresar" "Regresar al menu anterior"
               EXIT MENU
         END MENU

         COMMAND "Archivos" "Generación de archivos"
         MENU "Archivos"
           COMMAND "Formato 2004" "Reporte Formato 2004"
              CALL lee_paso(4)

           COMMAND "Formato 2005" "Reporte Formato 2005"
              CALL lee_paso(5)

           COMMAND "Formato 2007" "Reporte Formato 2007"
              LET gi_activos = 1
              CALL lee_paso(7)

           COMMAND "Formato 2009" "Reporte Formato 2009"
              LET gi_activos = 1
              CALL lee_paso(9)

           COMMAND "Regresar" "Regresar al menu anterior"
              EXIT MENU
         END MENU

         COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
            CALL revisa_proceso()

         COMMAND "Elimina ejecucion" "Elimina la ejecución de la fecha indicada"
            CALL elimina_ejecucion(10)

         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
      END MENU
   END IF

   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION lee_paso(li_paso)

   DEFINE li_paso  SMALLINT,
          lc_opc   CHAR(1)

   DEFINE lc_error_msg CHAR(40)

   CASE li_paso
   	  WHEN 1
   	  	 #Inicializa
   	     LET gc_mensaje1 = "ESTE PROCESO ELIMINARA LA INFORMACION PREPARADA ANTERIORMENTE"
         LET lc_error_msg = "DEBE INDICAR FECHA DE CORTE"
      WHEN 2
      	#Saldos
      	 LET gc_mensaje1 = "ESTE PROCESO IDENTIFICARA LOS SALDOS DE LA FECHA INDICADA",
                           "LA INFORMACIÓN PREPARADA ANTERIORMENTE SE BORRARÁ."
      WHEN 3
      	 #Cuota
      	 LET gc_mensaje1 = "ESTE PROCESO IDENTIFICARA LOS NSS DE LA FECHA INDICADA",
                           "LA INFORMACIÓN PREPARADA ANTERIORMENTE SE BORRARÁ."

      OTHERWISE
         #Archivos
         LET gc_mensaje1 = "ESTE PROCESO GENERARA EL REPORTE 200", li_paso USING "&"

   END CASE

   LET gc_mensaje2 = "INDIQUE LA FECHA DE CORTE:"
   LET gc_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

   INITIALIZE gd_fecha_corte TO NULL
   --LET gd_fecha_corte = "10/31/2007"


   INPUT BY NAME gd_fecha_corte WITHOUT DEFAULTS
      BEFORE INPUT
         DISPLAY BY NAME gc_mensaje1,
                         gc_mensaje2
                   
      BEFORE FIELD gd_fecha_corte
         IF gs_mes <= 3 THEN
            LET gd_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
         ELSE
            IF gs_mes > 3 AND gs_mes <= 6 THEN
               LET gd_fecha_corte = MDY (3,31, YEAR(TODAY))
            ELSE
               IF gs_mes > 6 AND gs_mes <= 9 THEN
                  LET gd_fecha_corte = MDY (6,30, YEAR(TODAY))
               ELSE
                  IF gs_mes > 9 AND gs_mes <= 12 THEN
                     LET gd_fecha_corte = MDY (9,30, YEAR(TODAY))
                  END IF
               END IF
            END IF
         END IF

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
           "CTANX14",               -- proceso_cod
           li_paso,                 -- etapa_cod    -- LECTURA
           lc_hora_inicio,          -- hora_inicial
           NULL,                    -- hora_final
           ld_fecha_inicio,         -- parametro1   --fecha_inicio
           NULL,                    -- parametro2   --fecha_fin
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5   --fecha_valuacion
           li_folio_proceso,        -- folio
           lc_resultado,            -- resultado
           GUSER,                   -- usuario
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
   AND    proceso_cod   = "CTANX14"
END FUNCTION
################################################################################
FUNCTION borra_paso(li_paso, ld_fecha_corte)
   DEFINE li_paso        SMALLINT,
          ld_fecha_corte DATE

   DELETE
   FROM   safre_af:dis_ctrl_proceso
   WHERE  fecha_proceso = ld_fecha_corte
   AND    etapa_cod     = li_paso
   AND    proceso_cod   = "CTANX14"
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
   	     ls_paso = 2) THEN --saldos

         DROP TABLE safre_tmp:tmp_saldo_formato2000
      END IF

      IF ls_tipo = 0 OR
        (ls_tipo = 1 AND
         ls_paso = 3) THEN --cuota

      END IF
   WHENEVER ERROR STOP

   LET li_resultado = 0


   IF ls_tipo = 0 OR
      (ls_tipo = 1 AND
       ls_paso = 2) THEN --saldos

      CREATE TABLE safre_tmp:tmp_saldo_formato2000
      (
        nss               CHAR(11),
        subcuenta         SMALLINT,
        siefore           SMALLINT,
        fecha_conversion  DATE,
        precio_del_dia    DECIMAL(22,6),
        monto_en_acciones DECIMAL(22,6),
        monto_en_pesos    DECIMAL(22,6),
        udis              DECIMAL(22,6)
      )

      LET li_resultado = li_resultado + SQLCA.SQLCODE
   END IF

   IF ls_tipo = 0 OR
   	  (ls_tipo = 1 AND
   	   ls_paso = 3) THEN --cuota

   END IF

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

   LET li_status      = 0
   LET ld_fecha_fin   = NULL
   LET li_existe      = NULL

   IF li_paso = 4 OR    --Archivo 2004
      li_paso = 5 OR    --Archivo 2005
      li_paso = 6 OR    --Archivo 2006
      li_paso = 7 OR    --Archivo 2007
      li_paso = 9 THEN  --Archivo 2009
   	  LET li_paso_ant = 3 --Cuota
   ELSE
   	  LET li_paso_ant = li_paso - 1
   END IF

   IF gi_activos = 0 THEN
   	  LET li_paso_ant = 1
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
      AND    proceso_cod   = "CTANX14"

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
      ---  Verifica si ya se ha ejecutado el proceso anteriormente
      SELECT etapa_cod,
             parametro2 --fecha_fin
      INTO   li_existe,
             ld_fecha_fin
      FROM   safre_af:dis_ctrl_proceso
      WHERE  fecha_proceso = ld_fecha_corte
      AND    etapa_cod     = li_paso
      AND    proceso_cod   = "CTANX14"

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
   DEFINE lr_domicilio RECORD
   	         nss            CHAR(11)     ,
         	   n_folio        DECIMAL(10,0),
         	   tipo_solicitud SMALLINT     ,
         	   cuantos        SMALLINT
          END RECORD

   DEFINE ls_resultado SMALLINT
   DEFINE lc_reporte   CHAR(20)


   LET ls_prueba = 0;

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
      WHEN 2 --Saldo
      	 CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia generacion de saldos")
      	    LET lc_comando = "nohup time ./eje_saldo_formato2000.sh ",
      	                     gd_fecha_corte," ",
                             GUSER CLIPPED,
                             " 1> ", GUSER CLIPPED, ".formato2000_saldo.salida ",
                             " 2> ", GUSER CLIPPED, ".formato2000_saldo.error &"
            ERROR "EJECUTANDO PROCESO POR NOHUP."
            SLEEP 2
            ERROR ""
            RUN   lc_comando
            ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato2000_saldo.salida Y ", GUSER CLIPPED ,".formato2000_saldo.error"
            SLEEP 3
      WHEN 3 --Cuota
      	 {ERROR "VERIFICANDO CURPS..."
      	 --VERIFICA CURP REPETIDAS
      	 LET lc_sql = "SELECT curp, COUNT(*) ",
                      "FROM   safre_af:cta_ctr_reg_ind "

         IF gi_afore = 578 THEN -- PENSIONISSSTE
         	  LET lc_sql = lc_sql CLIPPED,
         	               " WHERE  tipo_administracion = '01'"
         END IF

         LET lc_sql = lc_sql CLIPPED,
         	            " GROUP  BY 1 ",
                      " HAVING COUNT(*) > 1"

         PREPARE stmt_curp_rep FROM lc_sql
      	 DECLARE cur_curp_rep CURSOR FOR stmt_curp_rep

      	 INITIALIZE lr_curp_rep.* TO NULL

         FOREACH cur_curp_rep INTO lr_curp_rep.*
         	  IF lr_curp_rep.num IS NOT NULL THEN
         	  	 ERROR "EXISTEN CURPS REPETIDAS EN cta_ctr_reg_ind, NOTIFIQUE AL ADMINISTRADOR"
         	  	 SLEEP 3
         	  	 ERROR ""
         	  	 RETURN
         	  END IF
         END FOREACH

         ERROR ""}

         CALL inserta_paso(li_paso, gd_fecha_corte, "Inicia Identificacion de Cuota")
      	 LET lc_comando = "nohup time ./eje_cuota_formato2000.sh ",
      	                  gd_fecha_corte," ",
                          GUSER CLIPPED,
                          " 1> ", GUSER CLIPPED, ".formato2000_cuota.salida ",
                          " 2> ", GUSER CLIPPED, ".formato2000_cuota.error &"
         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         ERROR ""
         RUN   lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato2000_cuota.salida Y ", GUSER CLIPPED,".formato2000_cuota.error"
         SLEEP 3
      WHEN 4 --Archivo 2004
      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX15") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo CTANX15.4gi " CLIPPED,
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> ", GUSER CLIPPED, ".formato_2004.salida ",
                           "2> ", GUSER CLIPPED, ".formato_2004.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato_2004.salida Y ", GUSER CLIPPED, ".formato_2004.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX15"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 5 --Archivo 2005
      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX16") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo CTANX16.4gi " CLIPPED,
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> ", GUSER CLIPPED, ".formato_2005.salida ",
                           "2> ", GUSER CLIPPED, ".formato_2005.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato_2005.salida Y ", GUSER CLIPPED, ".formato_2005.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX16"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 6 --Archivo 2006
      	 IF gi_activos = 1 THEN --activo
      	 	  LET lc_reporte = "CTANX17.4gi"
      	 ELSE --inactivo
      	 	  LET lc_reporte = "CTANX17I.4gi"
      	 END IF

      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX17") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo ", lc_reporte CLIPPED, " ",
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> ", GUSER CLIPPED, ".formato_2006.salida ",
                           "2> ", GUSER CLIPPED, ".formato_2006.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato_2006.salida Y ", GUSER CLIPPED, ".formato_2006.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX17"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 7 --Archivo 2007
      	 IF gi_activos = 1 THEN --activo
      	 	  LET lc_reporte = "CTANX18.4gi"
      	 ELSE --inactivo
      	 	  LET lc_reporte = "CTANX18I.4gi"
      	 END IF

      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX18") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo ", lc_reporte CLIPPED, " ",
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> ", GUSER CLIPPED, ".formato_2007.salida ",
                           "2> ", GUSER CLIPPED, ".formato_2007.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato_2007.salida Y ", GUSER CLIPPED, ".formato_2007.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX18"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

      WHEN 9 --Archivo 2009
      	 IF gi_activos = 1 THEN --activo
      	 	  LET lc_reporte = "CTANX21.4gi"
      	 ELSE --inactivo
      	 	  LET lc_reporte = "CTANX21I.4gi"
      	 END IF

      	 CALL inserta_paso_reporte(gd_fecha_corte, "CTANX21") RETURNING li_totnss, li_folio_proceso

      	 LET lc_comando = "nohup time fglgo ", lc_reporte CLIPPED, " ",
                           li_folio_proceso CLIPPED, " ",
                           gd_fecha_corte   CLIPPED, " ",
                           li_totnss        CLIPPED, " ",
                           "1> ", GUSER CLIPPED, ".formato_2009.salida ",
                           "2> ", GUSER CLIPPED, ".formato_2009.error &"

         ERROR "EJECUTANDO PROCESO POR NOHUP."
         SLEEP 2
         RUN lc_comando
         ERROR "VERIFIQUE ARCHIVOS: ", GUSER CLIPPED, ".formato_2009.salida Y ", GUSER CLIPPED, ".formato_2009.error"
         SLEEP 3
         ERROR ""

         LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTANX21"
         PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)


      WHEN 10 --Borra Ejecucion
      	 SELECT "X"
      	 FROM   safre_af:dis_ctrl_proceso
      	 WHERE  fecha_proceso = gd_fecha_corte
      	 AND    proceso_cod = "CTANX14"
      	 AND    etapa_cod   = gi_paso
      	 GROUP BY 1

      	 IF SQLCA.SQLCODE != 0 THEN
      	 	  ERROR "NO EXISTE EJECUCIÓN PARA ESTA FECHA"
         ELSE
         	  ERROR "PROCESANDO INFORMACION"

      	    DELETE
      	    FROM  safre_af:dis_ctrl_proceso
      	    WHERE fecha_proceso = gd_fecha_corte
      	    AND   proceso_cod   = "CTANX14"
      	    AND   etapa_cod     = gi_paso

      	    CALL borra_informacion(1,gi_paso) RETURNING ls_resultado

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
                         
      BEFORE FIELD gd_fecha_corte
         IF gs_mes <= 3 THEN
            LET gd_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
         ELSE
            IF gs_mes > 3 AND gs_mes <= 6 THEN
               LET gd_fecha_corte = MDY (3,31, YEAR(TODAY))
            ELSE
               IF gs_mes > 6 AND gs_mes <= 9 THEN
                  LET gd_fecha_corte = MDY (6,30, YEAR(TODAY))
               ELSE
                  IF gs_mes > 9 AND gs_mes <= 12 THEN
                     LET gd_fecha_corte = MDY (9,30, YEAR(TODAY))
                  END IF
               END IF
            END IF
         END IF

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
                                 "2,'SALDOS      ', ",
                                 "3,'CUOTA       '  ",

                        ") desc_paso, ",
                "parametro1 || hora_inicial, ",
                "parametro2 || hora_final,   ",
                "usuario ",
                "FROM   safre_af:dis_ctrl_proceso ",
                "WHERE  fecha_proceso = '", ld_fecha_corte, "' ",
                "AND    proceso_cod   = 'CTANX14' ",
                "ORDER BY etapa_cod "

   {DISPLAY lc_sql
   EXIT PROGRAM}

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

   {SELECT COUNT(*)
   INTO   li_totnss
   FROM   safre_tmp:tmp_cuota_issste}

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           lc_proceso_cod,          -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           lc_hora_inicial,         -- hora_inicial
           NULL,                    -- hora_final
           "saldos_issste",         -- parametro1
           ld_fecha_corte,          -- parametro2 --fecha_corte
           li_totnss,               -- parametro3 --tot_nss
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           li_folio_proceso,        -- folio
           NULL,                    -- resultado
           GUSER,                   -- usuario
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
                         
      BEFORE FIELD gd_fecha_corte
         IF gs_mes <= 3 THEN
            LET gd_fecha_corte = MDY (12,31, YEAR(TODAY) - 1)
         ELSE
            IF gs_mes > 3 AND gs_mes <= 6 THEN
               LET gd_fecha_corte = MDY (3,31, YEAR(TODAY))
            ELSE
               IF gs_mes > 6 AND gs_mes <= 9 THEN
                  LET gd_fecha_corte = MDY (6,30, YEAR(TODAY))
               ELSE
                  IF gs_mes > 9 AND gs_mes <= 12 THEN
                     LET gd_fecha_corte = MDY (9,30, YEAR(TODAY))
                  END IF
               END IF
            END IF
         END IF

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
            AND    proceso_cod   = "CTANX14"
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