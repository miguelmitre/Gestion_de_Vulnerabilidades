################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX22    => ASIGNACION DE CUENTAS ASIGNADAS                        #
#                       A PRESTADORA DE SERVICIOS                              #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 28 de MARZO DE 2011                                    #
################################################################################
DATABASE safre_af
################################################################################
GLOBALS
  DEFINE  gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE

  DEFINE gs_afore     SMALLINT
  DEFINE hoy          DATE

  DEFINE gr_seg_modulo RECORD
  	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
  END RECORD

  DEFINE gc_log    CHAR(50)

  DEFINE gc_mensaje1           CHAR(100),
         gc_mensaje2           CHAR(100),
         gc_mensaje3           CHAR(100),
         gc_mensaje4           CHAR(100),
         gc_usuario            CHAR(008)

  DEFINE gc_archivo            CHAR(100)

  DEFINE gd_total_nss DECIMAL(10,0)
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

   LET gc_log = gc_usuario CLIPPED, "CTANX22.log"

   LET gi_tot_nss = 0
END FUNCTION
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTANX221" ATTRIBUTE(BORDER)
           #-----------------------------------------------------------------------------
   --DISPLAY "CTANX22  ASIGNACION ASIGNADOS A PRESTADORA DE SERVICIOS  <Ctrl-C> SALIR" AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "CTANX22        ASIGNACION ASIGNADOS A PRESTADORA DE SERVICIOS            " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "Asignados"
      COMMAND "Formato 011102" "Identifica las cuentas y genera el formato 011102"
         CALL lee_paso(1)

      COMMAND "Total de Nss y Saldos" "Genera el reporte del Total de Nss y Saldos globales"
         CALL lee_paso(2)

      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
END FUNCTION
################################################################################
FUNCTION lee_paso(li_paso)

   DEFINE li_paso  SMALLINT,
          lc_opc   CHAR(1)

   DEFINE lc_error_msg CHAR(40)

   DISPLAY "CTANX22 ASIGNACION ASIGNADOS  PRESTADORA DE SERVICIOS <Ctrl-C>SALIR " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   CASE li_paso
   	  WHEN 1
   	  	 #Identifica
   	  	 LET gc_mensaje1  = "ESTE PROCESO GENERARA EL FORMATO 011102"
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
         	  IF li_paso = 1 OR
         	  	 li_paso = 2 THEN
               IF valida_proceso(li_paso,gd_fecha_corte) != 0 THEN
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

   DISPLAY "CTANX22        ASIGNACION ASIGNADOS A PRESTADORA DE SERVICIOS            " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)
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

   IF li_paso = 1 THEN    --FORMATO 011102
   	  LET li_paso_ant = 3 -- PREPARACION DE NSS
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
FUNCTION ejecuta_paso(li_paso)
   DEFINE li_paso          SMALLINT

   CASE li_paso
   	  WHEN 1 --Formato 011102
   	  	 CALL formato_011102()

   	  WHEN 2 --Cifras Globales
   	  	 CALL cifras_globales()
   END CASE
END FUNCTION
################################################################################
FUNCTION formato_011102()
   DEFINE lr_trab RECORD
   	      nss          CHAR(11),
   	      f_asigancion DATE
   END RECORD

   DEFINE lc_chmod     CHAR(200),
          lc_enter     CHAR(001)

   LET gd_total_nss = 0
   LET gc_archivo = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    gc_usuario               CLIPPED,
                    ".fmto_011102.", gd_fecha_corte USING "DDMMYYYY"

   LET lc_chmod = "chmod 777 ", gc_archivo CLIPPED

   #/safre_back/usuario.fmto_011102.DDMMAAAA

   DECLARE cur_asig CURSOR FOR
   SELECT nss
   FROM   safre_tmp:cta_cuota_anexo71_nss
   WHERE  tipo_solicitud = 5
   AND    estatus_cuenta <> 3 --Cedidos con saldo

   START REPORT rpt_011102 TO gc_archivo

   ERROR "PROCESANDO INFORMACION"
   INITIALIZE lr_trab.f_asigancion TO NULL

   FOREACH cur_asig INTO lr_trab.nss
   	  #Identificar fecha de asigancion
   	  SELECT MAX(fecha_asignacion)
   	  INTO   lr_trab.f_asigancion
   	  FROM   afi_det_asignado
   	  WHERE  n_seguro = lr_trab.nss

   	  IF lr_trab.f_asigancion <= "08/24/2009" THEN
   	     OUTPUT TO REPORT rpt_011102(lr_trab.*)
   	     INITIALIZE lr_trab.f_asigancion TO NULL
   	  END IF
   END FOREACH

   FINISH REPORT rpt_011102
   RUN lc_chmod
   ERROR ""

   DISPLAY "ARCHIVO GENERADO" AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY gc_archivo CLIPPED AT 19,1 ATTRIBUTE(REVERSE)


   PROMPT "<ENTER PARA CONTINUAR>" FOR CHAR lc_enter
   DISPLAY "                                                                           " AT 18,1
   DISPLAY "                                                                           " AT 19,1
END FUNCTION
################################################################################
REPORT rpt_011102(lr_trab)
   DEFINE lr_trab RECORD
   	      nss          CHAR(11),
   	      f_asigancion DATE
   END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, gs_afore              USING "&&&"     ,
            COLUMN 04, lr_trab.nss                           ,
            COLUMN 15, lr_trab.f_asigancion  USING "YYYYMMDD"

      LET gd_total_nss = gd_total_nss + 1
      LET gi_tot_nss = gi_tot_nss + 1
END REPORT
################################################################################
FUNCTION cifras_globales()
   DEFINE lr_saldo RECORD
   	      subcuenta         SMALLINT,
          siefore           SMALLINT,
          acciones          DECIMAL(22,6),
          pesos             DECIMAL(22,6)
   END RECORD

   DEFINE lc_chmod     CHAR(200),
          lc_enter     CHAR(001)

   LET gc_archivo = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    gc_usuario               CLIPPED,
                    ".cifras_011102.", gd_fecha_corte USING "DDMMYYYY"

   LET lc_chmod = "chmod 777 ", gc_archivo CLIPPED
   LET gi_tot_nss = 0

   ERROR "PROCESANDO INFORMACION"

   SELECT nss
   FROM   safre_tmp:cta_cuota_anexo71_nss
   WHERE  tipo_solicitud = 5
   AND    estatus_cuenta <> 3 --Cedidos con saldo
   INTO TEMP tmp_asignados

   CREATE INDEX ix_tmp_asignadosnx22 ON tmp_asignados(nss)
   UPDATE STATISTICS FOR TABLE tmp_asignados;

   DELETE
   FROM  tmp_asignados
   WHERE nss = "23846608091"

   SELECT COUNT(*)
   INTO   gd_total_nss
   FROM   tmp_asignados

   START REPORT rpt_cifras TO gc_archivo

   DECLARE cur_saldos CURSOR FOR
   SELECT a.subcuenta             ,
          a.siefore               ,
          SUM(a.monto_en_acciones),
          SUM(a.monto_en_pesos   )
   FROM   safre_tmp:tmp_saldo_anexo71 a,
          safre_tmp:tmp_asignados     b
   WHERE  a.nss = b.nss
   GROUP BY 1,2
   ORDER BY 1,2

   INITIALIZE lr_saldo.* TO NULL
   OUTPUT TO REPORT rpt_cifras(lr_saldo.*)

   FOREACH cur_saldos INTO lr_saldo.subcuenta,
                           lr_saldo.siefore  ,
                           lr_saldo.acciones ,
                           lr_saldo.pesos
      OUTPUT TO REPORT rpt_cifras(lr_saldo.*)
   END FOREACH

   FINISH REPORT rpt_cifras

   RUN lc_chmod
   ERROR ""

   DISPLAY "ARCHIVO GENERADO" AT 18,1 ATTRIBUTE(REVERSE)
   DISPLAY gc_archivo CLIPPED AT 19,1 ATTRIBUTE(REVERSE)


   PROMPT "<ENTER PARA CONTINUAR>" FOR CHAR lc_enter
   DISPLAY "                                                                           " AT 18,1
   DISPLAY "                                                                           " AT 19,1
END FUNCTION
################################################################################
REPORT rpt_cifras(lr_saldo)
   DEFINE lr_saldo RECORD
   	      subcuenta         SMALLINT,
          siefore           SMALLINT,
          acciones          DECIMAL(22,6),
          pesos             DECIMAL(22,6)
   END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
      IF gi_tot_nss = 0 THEN
      	 PRINT COLUMN 004, "REPORTE DE CIFRAS GLOBALES ASIGNACION ASIGNADOS A PRESTADORA DE SERVICIOS"
      	 PRINT COLUMN 001, "PROGRAMA: CTANX22                        FECHA GENERACION:", TODAY USING "DD/MM/YYYY"
      	 PRINT COLUMN 001, "FECHA IDENTIFICACION:", gd_fecha_corte USING "DD/MM/YYYY"
      	 SKIP 1 LINE
      	 PRINT COLUMN 023, "TOTAL DE CUENTAS ASIGNADAS"
      	 PRINT COLUMN 029, gd_total_nss USING "#########&"
      	 SKIP 1 LINE
      	 PRINT COLUMN 001, "SUBCUENTA SIEFORE                   ACCIONES                      PESOS"

      	 LET gi_tot_nss = gi_tot_nss + 1
      ELSE
         PRINT COLUMN 008, lr_saldo.subcuenta   USING "#&",
               COLUMN 016, lr_saldo.siefore     USING "#&",
               COLUMN 019, lr_saldo.acciones    USING "##################&.&&&&&&",
               COLUMN 046, lr_saldo.pesos       USING "##################&.&&&&&&"

         LET gi_tot_nss = gi_tot_nss + 1
      END IF
END REPORT
################################################################################
{
   REPORTE DE CIFRAS GLOBALES ASIGNACION ASIGNADOS A PRESTADORA DE SERVICIOS
PROGRAMA: CTANX22                        FECHA GENERACION:DD/MM/YYYY
FECHA IDENTIFICACION:DD/MM/YYYY

                      TOTAL DE CUENTAS ASIGNADAS
                            #########&

SUBCUENTA SIEFORE                   ACCIONES                      PESOS
       #&      #& ##################&.&&&&&& ##################&.&&&&&&
}