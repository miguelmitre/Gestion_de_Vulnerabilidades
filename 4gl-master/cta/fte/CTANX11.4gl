################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX11    => RECIBE ARCHIVO RESPUESTA ANEXO 71                      #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 25 de AGOSTO    de 2008                                #
################################################################################

DATABASE safre_tmp
################################################################################
GLOBALS
   DEFINE  gi_folio,
           gi_registros,
           gi_tot_nss       INTEGER,
           gd_fecha_corte   DATE

   DEFINE gc_usuario   CHAR(08)

   DEFINE gi_afore     SMALLINT
   DEFINE hoy          DATE

   DEFINE gr_estado RECORD
 	        iniciado,
 	        generado,
 	        concatenado SMALLINT
 	        END RECORD

 	 DEFINE gr_seg_modulo RECORD
   	      modulo_cod           char(04),
          ruta_envio           char(40),
          ruta_rescate         char(40),
          ruta_listados        char(40)
   END RECORD

   DEFINE gr_archivo RECORD
   	      nom_archivo CHAR(120)
   END RECORD

   DEFINE gr_anexo71 RECORD
   	      tipo_reg       SMALLINT     ,
          nss            CHAR(11)     ,
          curp           CHAR(18)     ,
          rfc            CHAR(13)     ,
          estado         SMALLINT     ,
          cod_postal     CHAR(5)      ,
          status_cta     SMALLINT     ,
          sector         SMALLINT     ,
          subcuenta      SMALLINT     ,
          siefore        SMALLINT     ,
          acciones       DECIMAL(22,6),
          resultado      SMALLINT
   END RECORD

   DEFINE gc_texto_plano CHAR(207)

   DEFINE gr_encabezado RECORD
   	  tipo_reg       SMALLINT,
   	  cve_afore      SMALLINT,
   	  fecha_envio    DATE
   END RECORD

   DEFINE gr_consulta RECORD
   	      nom_archivo CHAR(32),
          fecha_envio DATE,
          tot_lineas  INTEGER,
          folio       INTEGER,
          nss_unicos  INTEGER
   END RECORD

   DEFINE gar_consulta_det ARRAY[32000] OF RECORD
   	      nss         CHAR(11),
          curp        CHAR(18),
          status      SMALLINT,
          desc_status CHAR(28),
          subcta      SMALLINT,
          diag_nss    SMALLINT
   END RECORD

   DEFINE gar_consulta ARRAY[32000] OF RECORD
   	      diagnostico  SMALLINT,
          desc_diag    CHAR (56),
          tot_diag     INTEGER
   END RECORD

   DEFINE gar_det_nss ARRAY[1] OF RECORD
   	      nss            CHAR(11)     ,
          curp           CHAR(18)     ,
          rfc            CHAR(13)     ,
          estado         SMALLINT     ,
          cod_postal     CHAR(5)      ,
          status_cta     SMALLINT     ,
          status_desc    CHAR(30)     ,
          sector         SMALLINT     ,
          sector_desc    CHAR(30)     ,
          subcuenta      SMALLINT     ,
          subcuenta_desc CHAR(30)     ,
          siefore        SMALLINT     ,
          siefore_desc   CHAR(30)     ,
          acciones       DECIMAL(22,6),
          resultado      SMALLINT     ,
          desc_result    CHAR (56)
   END RECORD

END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT
   CALL STARTLOG("CTANX11.log")

   CALL init()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTANX111" ATTRIBUTE(BORDER)
   DISPLAY "CTANX11               ARCHIVOS    DE    RESPUESTA    ANEXO 71          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "ANEXO 71"
      COMMAND "Recibir" "Recibe archivo de respuesta Anexo 71"
         CALL carga_archivo(1)
      COMMAND "Consultar" "Consultar archivo de respuesta Anexo 71"
         CALL consultar()
      COMMAND "Reversar" "Reversa archivo de respuesta Anexo 71"
         CALL carga_archivo(2)
      COMMAND "Salir" "Salir"
         EXIT MENU
   END MENU

END MAIN
################################################################################
FUNCTION init()
   LET HOY  = TODAY

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   gi_afore
   FROM   safre_af:tab_afore_local

   LET gr_seg_modulo.modulo_cod = "ctx"

   SELECT ruta_envio   ,
          ruta_rescate ,
          ruta_listados
   INTO   gr_seg_modulo.ruta_envio  ,
          gr_seg_modulo.ruta_rescate,
          gr_seg_modulo.ruta_listados
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

END FUNCTION
################################################################################
FUNCTION carga_archivo(li_modo)
   DEFINE li_modo    SMALLINT
   DEFINE li_flag    SMALLINT
   DEFINE lc_enter   CHAR(001)
   DEFINE lc_comando CHAR(300)
   DEFINE ls_estado  SMALLINT
   DEFINE lc_opc     CHAR(01)

   OPEN WINDOW ventana_2 AT 4,3 WITH FORM "CTANX112" ATTRIBUTE(BORDER)
   DISPLAY " <Esc> Aceptar                                           < Ctrl-C > Salir  " AT 1,1 ATTRIBUTE(REVERSE)

   IF li_modo = 1 THEN
      DISPLAY " CTANX11   RECEPCIONA ARCHIVOS    DE    RESPUESTA    ANEXO 71                  " AT 3,1 ATTRIBUTE(REVERSE)
   ELSE
   	  DISPLAY " CTANX11   REVERSA RECEPCION DE ARCHIVO DE RESPUESTA ANEXO 71                  " AT 3,1 ATTRIBUTE(REVERSE)
   END IF

   DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET li_flag = 0

   INITIALIZE  gr_archivo.nom_archivo TO NULL

   INPUT BY NAME gr_archivo.nom_archivo WITHOUT DEFAULTS

      AFTER FIELD nom_archivo
         IF gr_archivo.nom_archivo IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO"
            NEXT FIELD nom_archivo
         END IF

         INITIALIZE gi_folio TO NULL

         #El registro esta en la tabal de control del anexo71
         SELECT folio
         INTO   gi_folio
         FROM   safre_af:ctr_anexo71
         WHERE  archivo      = gr_archivo.nom_archivo
         AND    tipo_reporte = 3
         GROUP BY 1

         IF li_modo = 1 THEN
         	  IF gi_folio IS NOT NULL THEN
         	     ERROR "ARCHIVO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
         	     NEXT FIELD nom_archivo
         	  END IF
         ELSE
         	  IF gi_folio IS NULL THEN
         	     ERROR "NO EXISTE INFORMACION PARA ESTE ARCHIVO" ATTRIBUTE(NORMAL)
         	     SLEEP 2
         	     NEXT FIELD nom_archivo
         	  END IF
         END IF

         #Verificar existencia del archivo
         LET lc_comando = "ls ", gr_seg_modulo.ruta_rescate CLIPPED, " | grep '",
                                 gr_archivo.nom_archivo CLIPPED, "' 1>/dev/null"

         RUN lc_comando RETURNING ls_estado

         IF ls_estado <> 0 THEN
         	  ERROR "ARCHIVO INCORRECTO O NO SE ENCUENTRA EN LA RUTA /safre_prc/cta/rescate"
         	  SLEEP 2
         	  ERROR ""
            NEXT FIELD nom_archivo
         END IF

      ON KEY (esc)
      	 IF gr_archivo.nom_archivo IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO"
            NEXT FIELD nom_archivo
         END IF

      	 INITIALIZE gi_folio TO NULL

         SELECT folio
         INTO   gi_folio
         FROM   safre_af:ctr_anexo71
         WHERE  archivo      = gr_archivo.nom_archivo
         AND    tipo_reporte = 3
         GROUP BY 1

         IF li_modo = 1 THEN
         	  IF gi_folio IS NOT NULL THEN
         	     ERROR "ARCHIVO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
         	     SLEEP 2
         	     NEXT FIELD nom_archivo
         	  END IF
         ELSE
         	  IF gi_folio IS NULL THEN
         	     ERROR "NO EXISTE INFORMACION PARA ESTE ARCHIVO" ATTRIBUTE(NORMAL)
         	     SLEEP 2
         	     NEXT FIELD nom_archivo
         	  END IF
         END IF

         #Verificar existencia del archivo
         LET lc_comando = "ls ", gr_seg_modulo.ruta_rescate CLIPPED, " | grep '",
                                 gr_archivo.nom_archivo CLIPPED, "' 1>/dev/null"

         RUN lc_comando RETURNING ls_estado

         IF ls_estado <> 0 THEN
         	  ERROR "ARCHIVO INCORRECTO O NO SE ENCUENTRA EN LA RUTA /safre_prc/cta/rescate"
         	  SLEEP 2
         	  ERROR ""
            NEXT FIELD nom_archivo
         END IF

         LET li_flag = 0
         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET li_flag = 1
         EXIT INPUT
   END INPUT

   IF li_flag = 1 THEN
      --PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR lc_enter
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
   ELSE

   	  IF li_modo = 1 THEN
   	  	 ERROR "PROCESANDO INFORMACION..."
   	     CALL carga_plano()
   	  ELSE

   	  	 #Pedir confirmación para reverso
   	  	 WHILE TRUE
            PROMPT "DESEA BORRAR LA INFORMACION DE ESTE ARCHIVO S/N:" ATTRIBUTE(REVERSE) FOR CHAR lc_opc

            IF lc_opc  MATCHES "[SsNn]" THEN
               IF lc_opc MATCHES "[Ss]" THEN
               	  ERROR "PROCESANDO INFORMACION..."
               	  CALL reversa()
               	  EXIT WHILE
               ELSE
                  ERROR "PROCESO CANCELADO..."
                  SLEEP 2
                  ERROR ""
                  EXIT WHILE
               END IF
            ELSE
            	  ERROR "SOLO INDIQUE S o N "
                SLEEP 2
                ERROR ""
                CONTINUE WHILE
            END IF
         END WHILE
   	  END IF

   	  ERROR ""
   END IF

   CLOSE WINDOW ventana_2
END FUNCTION
################################################################################
FUNCTION carga_plano()

   DEFINE li_cuantos INTEGER
   DEFINE lc_archivo CHAR(100)

   WHENEVER ERROR CONTINUE
      DROP TABLE ctanx_pla_carga
   WHENEVER ERROR STOP

   CREATE TEMP TABLE ctanx_pla_carga(
          texto_plano CHAR(207)
   )

   LET lc_archivo = gr_seg_modulo.ruta_rescate CLIPPED,"/",
   	                            gr_archivo.nom_archivo

   LOAD FROM lc_archivo DELIMITER "+"
   INSERT INTO ctanx_pla_carga

   SELECT count(*)
   INTO   li_cuantos
   FROM   ctanx_pla_carga

   IF li_cuantos = 0 THEN
      ERROR "   NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
      SLEEP 2
      ERROR ""
   ELSE
      CALL carga_detalle()
   END IF
END FUNCTION
################################################################################
FUNCTION carga_detalle()
   DEFINE li_cuantos  INTEGER
   DEFINE lc_enter    CHAR(1)
   DEFINE li_flag_enc SMALLINT
   DEFINE ls_mes      ,
          ls_dia      ,
          ls_anio     SMALLINT

   DECLARE cur_plano CURSOR FOR
   SELECT *
   FROM   ctanx_pla_carga

   SELECT texto_plano[5,7],
          texto_plano[12,13],
          texto_plano[14,15],
          texto_plano[8,11]
   INTO   gr_encabezado.cve_afore,
          ls_mes,
          ls_dia,
          ls_anio
   FROM   ctanx_pla_carga
   WHERE  texto_plano[1,2] = '01'

   LET li_flag_enc = 0

   IF SQLCA.SQLCODE = 0 THEN
   	  LET gr_encabezado.fecha_envio = MDY (ls_mes, ls_dia, ls_anio)
   	  CALL ingresa_enc()
   	  LET li_flag_enc = 1

   	  FOREACH cur_plano INTO gc_texto_plano
   	  	 LET gr_anexo71.tipo_reg = gc_texto_plano[1,2]

   	  	 IF gr_anexo71.tipo_reg = 02 THEN
            --DETALLE
            INITIALIZE gr_anexo71.nss       ,
                       gr_anexo71.curp      ,
                       gr_anexo71.rfc       ,
                       gr_anexo71.estado    ,
                       gr_anexo71.cod_postal,
                       gr_anexo71.status_cta,
                       gr_anexo71.sector    ,
                       gr_anexo71.subcuenta ,
                       gr_anexo71.siefore   ,
                       gr_anexo71.acciones  ,
                       gr_anexo71.resultado TO NULL

            LET gr_anexo71.nss        = gc_texto_plano[03, 13]
            LET gr_anexo71.curp       = gc_texto_plano[14, 31]
            LET gr_anexo71.rfc        = gc_texto_plano[32, 44]
            LET gr_anexo71.estado     = gc_texto_plano[45, 46]
            LET gr_anexo71.cod_postal = gc_texto_plano[47, 51]
            LET gr_anexo71.status_cta = gc_texto_plano[52, 52]
            LET gr_anexo71.sector     = gc_texto_plano[53, 53]
            LET gr_anexo71.subcuenta  = gc_texto_plano[54, 55]
            LET gr_anexo71.siefore    = gc_texto_plano[56, 57]
            LET gr_anexo71.acciones   = gc_texto_plano[58, 73] / 1000000
            LET gr_anexo71.resultado  = gc_texto_plano[74, 76]

            CALL ingresa_det()
         END IF
      END FOREACH
   END IF

   SELECT COUNT(*)
   INTO   li_cuantos
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio = gi_folio

   UPDATE safre_af:ctr_anexo71
   SET    registros    = li_cuantos
   WHERE  folio        = gi_folio
   AND    tipo_reporte = 3

   ERROR ""

   IF li_flag_enc = 1 THEN

      DISPLAY " ARCHIVO CARGADO "   AT 16,6
      DISPLAY " FOLIO     : ", gi_folio   USING "########&" AT 16,7
      DISPLAY " REGISTROS : ", li_cuantos USING "########&" AT 16,8

      PROMPT  "ARCHIVO CARGADO <ENTER> PARA SALIR " FOR CHAR lc_enter
   ELSE
   	  PROMPT  "ARCHIVO SIN ENCABEZADO, VERIFIQUE SU ESTRUCTURA. <ENTER> PARA SALIR " FOR CHAR lc_enter
   END IF
END FUNCTION
################################################################################
FUNCTION ingresa_enc()
   SET LOCK MODE TO WAIT

   INITIALIZE gi_folio TO NULL

   SELECT MAX(folio)
   INTO   gi_folio
   FROM   safre_af:glo_folio

   IF gi_folio IS NULL THEN
   	  LET gi_folio = 0
   END IF

   LET gi_folio = gi_folio + 1

   INSERT INTO safre_af:glo_folio VALUES(gi_folio)

   SET LOCK MODE TO NOT WAIT

   INSERT INTO safre_af:ctr_anexo71 VALUES(
   gi_folio,                   --folio
   gr_encabezado.fecha_envio,  --fecha_corte
   3,                          --tipo_reporte
   1,                          --tipo_reg
   0,                          --consecutivo
   gr_archivo.nom_archivo,     --archivo
   0,                          --registros
   0,                          --estado
   hoy,                        --fecha_proceso
   gc_usuario                  --usuario
   )

END FUNCTION
################################################################################
FUNCTION ingresa_det()

   INSERT INTO safre_af:cta_txt_anexo71 VALUES(
   gi_folio                 ,  --folio
   gr_anexo71.nss           ,  --nss
   gr_anexo71.curp          ,  --curp
   gr_anexo71.rfc           ,  --rfc
   gr_anexo71.estado        ,  --estado
   gr_anexo71.cod_postal    ,  --cod_postal
   gr_anexo71.status_cta    ,  --status_cta
   gr_anexo71.sector        ,  --sector
   gr_anexo71.subcuenta     ,  --subcuenta
   gr_anexo71.siefore       ,  --siefore
   gr_anexo71.acciones      ,  --acciones
   gr_anexo71.resultado        --resultado
   )
END FUNCTION
################################################################################
FUNCTION reversa()
   DEFINE lc_enter CHAR(1)

   DELETE
   FROM  safre_af:cta_txt_anexo71
   WHERE folio = gi_folio

   DELETE
   FROM  safre_af:ctr_anexo71
   WHERE folio = gi_folio

   ERROR ""
   PROMPT  "REVERSO CONCLUIDO <ENTER> PARA SALIR " FOR CHAR lc_enter

END FUNCTION
################################################################################
FUNCTION consultar()
   DEFINE li_flag          SMALLINT
   DEFINE li_pos           SMALLINT
   DEFINE li_total_archivo INTEGER
   DEFINE li_curp_unicas   INTEGER

   OPEN WINDOW ventana_3 AT 4,2 WITH FORM "CTANX113" ATTRIBUTE(BORDER)
   DISPLAY " CTANX11      CONSULTAR DIAGNOSTICOS DE RESPUESTA DE ANEXO 71              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING "DD-MM-YYYY" AT 1,69 ATTRIBUTE(REVERSE)
   DISPLAY " <Esc> Aceptar                                           < Ctrl-C > Salir     " AT 2,1 ATTRIBUTE(REVERSE)

   LET li_flag = 0

   INITIALIZE  gr_consulta.nom_archivo TO NULL

   INPUT BY NAME gr_consulta.nom_archivo WITHOUT DEFAULTS

      AFTER FIELD nom_archivo
         IF gr_consulta.nom_archivo IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO"
            NEXT FIELD nom_archivo
         END IF

         INITIALIZE gr_consulta.folio TO NULL

         SELECT folio
         INTO   gr_consulta.folio
         FROM   safre_af:ctr_anexo71
         WHERE  archivo      = gr_consulta.nom_archivo
         AND    tipo_reporte = 3
         GROUP BY 1

         IF gr_consulta.folio IS NULL THEN
         	  ERROR "NO EXISTE INFORMACION PARA ESTE ARCHIVO" ATTRIBUTE(NORMAL)
         	  SLEEP 2
         	  NEXT FIELD nom_archivo
         END IF

      ON KEY (esc)
      	 IF gr_consulta.nom_archivo IS NULL THEN
            ERROR "   CAMPO NO PUEDE SER NULO"
            NEXT FIELD nom_archivo
         END IF

      	 INITIALIZE gr_consulta.folio TO NULL

         SELECT folio
         INTO   gr_consulta.folio
         FROM   safre_af:ctr_anexo71
         WHERE  archivo      = gr_consulta.nom_archivo
         AND    tipo_reporte = 3
         GROUP BY 1

         IF gr_consulta.folio IS NULL THEN
         	  ERROR "NO EXISTE INFORMACION PARA ESTE ARCHIVO" ATTRIBUTE(NORMAL)
         	  SLEEP 2
         	  NEXT FIELD nom_archivo
         END IF

         LET li_flag = 0
         EXIT INPUT

      ON KEY (CONTROL-C, INTERRUPT)
         LET li_flag = 1
         EXIT INPUT
   END INPUT

   IF li_flag = 1 THEN
      ERROR "CONSULTA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLOSE WINDOW ventana_3
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION..."

   --Recuperar Información del encabezado
   SELECT fecha_corte  ,
          registros    ,
          folio
   INTO   gr_consulta.fecha_envio,
          gr_consulta.tot_lineas ,
          gr_consulta.folio
   FROM   safre_af:ctr_anexo71
   WHERE  archivo = gr_consulta.nom_archivo
   AND    tipo_reporte = 3


   SELECT COUNT(UNIQUE nss)
   INTO   gr_consulta.nss_unicos
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio = gr_consulta.folio
   AND    nss IS NOT NULL
   AND    nss <> '00000000000'

   SELECT COUNT(UNIQUE curp)
   INTO   li_curp_unicas
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio = gr_consulta.folio
   AND    curp IS NOT NULL

   #Total de trabajadores únicos
   LET gr_consulta.nss_unicos = gr_consulta.nss_unicos + li_curp_unicas

   DISPLAY "Fecha envio:"    AT 3,54
   DISPLAY " Folio Interno:" AT 4,1
   DISPLAY "Registros:"      AT 4,54
   DISPLAY " Nss Unicos:"    AT 5,1

   DISPLAY BY NAME gr_consulta.fecha_envio,
                   gr_consulta.tot_lineas ,
                   gr_consulta.folio      ,
                   gr_consulta.nss_unicos

   DECLARE cur_consulta_gral CURSOR FOR
   SELECT resultado,
          COUNT(*)
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio = gr_consulta.folio
   GROUP BY 1
   ORDER BY 1

   LET li_pos = 1
   LET li_total_archivo = 0

   FOREACH cur_consulta_gral INTO gar_consulta[li_pos].diagnostico,
   	                              gar_consulta[li_pos].tot_diag
      CALL get_desc_diagnostico(gar_consulta[li_pos].diagnostico) RETURNING
                                gar_consulta[li_pos].desc_diag

      LET li_total_archivo = li_total_archivo + gar_consulta[li_pos].tot_diag

      LET li_pos = li_pos + 1

      IF li_pos > 32000 THEN
      	 ERROR "FUE SOBREPASADA LA CAPACIDAD MÁXIMA DEL ARREGLO"
      	 SLEEP 3
      	 ERROR ""
      	 EXIT FOREACH
      END IF
   END FOREACH

   ERROR ""

   LET li_pos = li_pos - 1

   IF li_pos = 0 THEN
   	  ERROR "NO EXISTE INFORMACIÓN DE ESTE ARCHIVO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLOSE WINDOW ventana_3
      RETURN
   END IF

   --Mostrar el total de registros
   DISPLAY " TOTAL                                                         " AT 12,1 ATTRIBUTE(REVERSE)
   DISPLAY li_total_archivo TO total_archivo


   --TOTAL POR DIAGNOSTICO
   CALL SET_COUNT(li_pos)
   DISPLAY " <Enter> Listar Nss                                      < Ctrl-C > Regresar  " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "           Diagnostico                                          Registros     " AT 6,1 ATTRIBUTE (REVERSE)
   DISPLAY ARRAY gar_consulta TO scr_consulta.*
      ON KEY ( CONTROL-C, INTERRUPT)
         LET li_pos = 0
         EXIT DISPLAY

      ON KEY ( CONTROL-M )
         LET li_pos = ARR_CURR()
         CALL muestra_det_diag(gr_consulta.folio,
                               gar_consulta[li_pos].diagnostico)

   END DISPLAY

   IF li_pos = 0 THEN
   	  CLEAR FORM
      CLOSE WINDOW ventana_3
      RETURN
   END IF

END FUNCTION
################################################################################
FUNCTION get_desc_diagnostico(li_diagnostico)
   DEFINE li_diagnostico SMALLINT
   DEFINE lc_desc_diag   CHAR(56)

   INITIALIZE lc_desc_diag TO NULL

   SELECT descripcion
   INTO   lc_desc_diag
   FROM   safre_af:tab_rechazo_anexo71
   WHERE  clave = li_diagnostico

   RETURN lc_desc_diag
END FUNCTION
################################################################################
FUNCTION muestra_det_diag(li_folio,li_diagnostico)
   DEFINE li_diagnostico SMALLINT
   DEFINE li_folio       INTEGER
   DEFINE li_cont        SMALLINT

   DECLARE cur_consulta_det CURSOR FOR
   SELECT nss,
          curp,
          status_cta,
          DECODE(status_cta,0,"ASIGNADO",
                            1,"REGISTRADO",
                            2,"SALDO CERO",
                            3,"NO REG. CON AP EXTEMPORANEAS",
                            4,"BANXICO") desc_status,
          subcuenta,
          resultado
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio     = li_folio
   AND    resultado = li_diagnostico

   LET li_cont = 1

   ERROR "PROCESANDO INFORMACION ..."

   --DETALLE DEL DIAGNOSTICO
   FOREACH cur_consulta_det INTO gar_consulta_det[li_cont].nss        ,
                                 gar_consulta_det[li_cont].curp       ,
                                 gar_consulta_det[li_cont].status     ,
                                 gar_consulta_det[li_cont].desc_status,
                                 gar_consulta_det[li_cont].subcta     ,
                                 gar_consulta_det[li_cont].diag_nss

      LET li_cont = li_cont + 1

      IF li_cont > 32000 THEN
      	 ERROR "FUE SOBREPASADA LA CAPACIDAD MÁXIMA DEL ARREGLO"
      	 SLEEP 3
      	 ERROR ""
      	 EXIT FOREACH
      END IF
   END FOREACH

   LET li_cont = li_cont - 1

   IF li_cont = 0 THEN
   	  ERROR "NO EXISTE INFORMACIÓN DE ESTE DIAGNOSTICO..."
      SLEEP 2
      ERROR ""
      RETURN
   END IF

   ERROR ""

   --DETALLES POR DIAGNOSTICO
   CALL SET_COUNT(li_cont)
   DISPLAY " < Enter > Ver Detalle                                   < Ctrl-C > Regresar  " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "       Nss           Curp           Estatus Cuenta             Subcta Diag    " AT 14,1 ATTRIBUTE (REVERSE)
   DISPLAY ARRAY gar_consulta_det TO scr_consulta_det.*
      ON KEY ( CONTROL-C, INTERRUPT)
         LET li_cont = 0
         EXIT DISPLAY

      ON KEY ( CONTROL-M )
         LET li_cont = ARR_CURR()
         CALL muestra_det_nss(li_folio,
                              li_diagnostico,
                              gar_consulta_det[li_cont].nss,
                              gar_consulta_det[li_cont].curp,
                              gar_consulta_det[li_cont].subcta
                              )
   END DISPLAY

   IF li_cont = 0 THEN
   	  DISPLAY " <Enter> Listar Nss                                      < Ctrl-C > Regresar  " AT 2,1 ATTRIBUTE(REVERSE)
   	  DISPLAY "                                                                              " AT 14,1

   	  FOR li_cont = 1 TO 5
   	     CLEAR scr_consulta_det[li_cont].*
   	  END FOR
      RETURN
   END IF
END FUNCTION
################################################################################
FUNCTION muestra_det_nss(li_folio,li_diagnostico,lc_nss,lc_curp,li_subcta)
   DEFINE li_folio       INTEGER
   DEFINE li_diagnostico SMALLINT
   DEFINE lc_nss         CHAR(11)
   DEFINE lc_curp        CHAR(18)
   DEFINE li_subcta      SMALLINT
   
   INITIALIZE gar_det_nss[1].* TO NULL

   SELECT nss       ,
          curp      ,
          rfc       ,
          estado    ,
          cod_postal,
          status_cta,
          DECODE(status_cta,0,"ASIGNADO",
                            1,"REGISTRADO",
                            2,"SALDO CERO",
                            3,"CED CON SALDO",
                            4,"BANXICO") desc_status,
          sector    ,
          DECODE(sector,    0,"IMSS",
                            1,"ISSSTE",
                            2,"INDEPENDIENTE",
                            3,"MIXTOS",
                            4,"AHORRO VOL.") desc_status,
          subcuenta ,
          DECODE(subcuenta, 01, "RETIRO 97"              ,
                            02, "CV Y CUOTA ESP"         ,
                            03, "CUOTA SOCIAL"           ,
                            05, "VOLUNTARIAS VENTANILLA" ,
                            13, "VOLUNTARIAS PATRONALES" ,
                            15, "COMPRETIRO PATRONALES"  ,
                            17, "COMPRETIRO VENTANILLA"  ,
                            18, "AP LARGO PLAZO"         ,
                            30, "VOLUNTARIAS LARGO PLAZO",
                            08, "RETIRO 92 IMSS"         ,
                            22, "RETIRO ISSSTE 92"       ,
                            04, "VIVIENDA 97 IMSS"       ,
                            09, "VIVIENDA 92 IMSS"       ,
                            16, "FOVISSSTE 92"           ,
                            24, "FOVISSSTE 2008"         ,
                            25, "RETIRO 08 ISSSTE"       ,
                            27, "CV ISSSSTE"             ,
                            28, "AHORRO SOLIDARIO"       ,
                            29, "CUOTA SOCIAL ISSSTE"    ,
                            31, "BONO DE PENSIÓN") desc_subcuenta,
          siefore   ,
          DECODE(siefore,   01, "SIE B1"           ,
                            02, "SIE B2"           ,
                            03, "SIE B3"           ,
                            04, "SIE B4"           ,
                            05, "SIE B5"           ,
                            06, "SIE ADICIONAL CP" ,
                            07, "SIE ADICIONAL LP" ,
                            00, "VIV INFONAVIT"    ,
                            09, "VIV FOVISSSTE"    ,
                            08, "BONO DE PENSION"  ,
                            10, "PESOS EN BANXICO") desc_siefore,
          acciones  ,
          resultado
   INTO   gar_det_nss[1].*
   FROM   safre_af:cta_txt_anexo71
   WHERE  folio     = li_folio
   AND    resultado = li_diagnostico
   AND    nss       = lc_nss
   AND    curp      = lc_curp
   AND    subcuenta = li_subcta

   CALL get_desc_diagnostico(gar_det_nss[1].resultado) RETURNING
                             gar_det_nss[1].desc_result

   OPEN WINDOW ventana_4 AT 16,2 WITH FORM "CTANX114" ATTRIBUTE(BORDER)

   CALL SET_COUNT(1)
   DISPLAY "                       < Ctrl-C > Regresar                                    " AT 2,1 ATTRIBUTE(REVERSE)

   DISPLAY ARRAY gar_det_nss TO scr_det_nss.*
      ON KEY ( CONTROL-C, INTERRUPT)
         EXIT DISPLAY
   END DISPLAY

   CLOSE WINDOW ventana_4
END FUNCTION
################################################################################