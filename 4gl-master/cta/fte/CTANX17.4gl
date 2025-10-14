################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX17    => FORMATO 2006                                           #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 11 de JUNIO DE 2010                                    #
################################################################################
DATABASE safre_tmp
################################################################################
GLOBALS
  DEFINE  gi_folio                ,
          gi_registros            ,
          gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE

  DEFINE gc_usuario   CHAR(08)

  DEFINE gi_afore     SMALLINT
  DEFINE hoy          DATE

  DEFINE gr_seg_modulo RECORD
  	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
  END RECORD

  DEFINE gd_saldo_total   DECIMAL(22,6)

  DEFINE gr_encabezado RECORD
  	     tipo_registro  SMALLINT,
  	     tipo_archivo   SMALLINT,
  	     tipo_entidad   SMALLINT,
  	     entidad        SMALLINT,
  	     fecha_envio    DATE    ,
  	     longitud       SMALLINT,
  	     num_registros  INTEGER
  END RECORD

  DEFINE gr_subenc RECORD
  	     tipo_registro          SMALLINT     ,
  	     tipo_entidad           SMALLINT     ,
  	     entidad                SMALLINT     ,
  	     periodicidad           SMALLINT     ,
  	     tipo_proceso           SMALLINT     ,
  	     fecha_corte            DATE
  END RECORD

  DEFINE gr_det1 RECORD
  	 tipo_registro   SMALLINT     ,
  	 sector          SMALLINT     ,
  	 clasificacion   SMALLINT     ,
  	 rango_semanas   SMALLINT     ,
  	 total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det2 RECORD
  	  tipo_registro   SMALLINT     ,
  	  sector          SMALLINT     ,
  	  clasificacion   SMALLINT     ,
  	  rango_densidad  SMALLINT     ,
  	  rango_edad      SMALLINT     ,
  	  rango_salario   SMALLINT     ,
  	  sexo            SMALLINT     ,
  	  total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det3 RECORD
  	  tipo_registro           SMALLINT     ,
  	  total_trabs_issste      DECIMAL(10,0),
  	  total_trabs_mixtos      DECIMAL(10,0),
  	  total_hasta27           DECIMAL(10,0),
  	  total_hasta36           DECIMAL(10,0),
  	  total_hasta45           DECIMAL(10,0),
  	  total_hasta55           DECIMAL(10,0),
  	  total_hasta99           DECIMAL(10,0),
  	  total_mujeres           DECIMAL(10,0),
  	  total_hombres           DECIMAL(10,0),
  	  total_densidad_hasta25  DECIMAL(10,0),
  	  total_densidad_hasta50  DECIMAL(10,0),
  	  total_densidad_hasta75  DECIMAL(10,0),
  	  total_densidad_hasta100 DECIMAL(10,0)
  END RECORD

  DEFINE gr_det4 RECORD
  	  tipo_registro               SMALLINT     ,
  	  dif_total_trabs_issste      DECIMAL(10,0),
  	  dif_total_trabs_mixtos      DECIMAL(10,0),
  	  dif_total_hasta27           DECIMAL(10,0),
  	  dif_total_hasta36           DECIMAL(10,0),
  	  dif_total_hasta45           DECIMAL(10,0),
  	  dif_total_hasta55           DECIMAL(10,0),
  	  dif_total_hasta99           DECIMAL(10,0),
  	  dif_total_mujeres           DECIMAL(10,0),
  	  dif_total_hombres           DECIMAL(10,0),
  	  dif_total_densidad_hasta25  DECIMAL(10,0),
  	  dif_total_densidad_hasta50  DECIMAL(10,0),
  	  dif_total_densidad_hasta75  DECIMAL(10,0),
  	  dif_total_densidad_hasta100 DECIMAL(10,0)
  END RECORD

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE FORMATO 2004",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX17.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación formato 2004")

   LET gi_registros = 0
   LET hoy          = TODAY

   SELECT codigo_afore
   INTO   gi_afore
   FROM   safre_af:tab_afore_local

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   LET gr_seg_modulo.modulo_cod = "cta"

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   LET gd_saldo_total = 0
   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
#####################################################################
FUNCTION archivo()
   DEFINE lc_nomarch       CHAR(80),
          lc_nomarch_ini   CHAR(80),
          lc_comando_cat   CHAR(15000),
          lc_comando_rm    CHAR(15000),
          lc_comando_chmod CHAR(15000),

          lc_encabezado    ,
          lc_sub_encabezado,
          lc_detalle1      ,
          lc_detalle2      ,
          lc_detalle3      ,
          lc_detalle4      ,
          lc_archivo_final CHAR(80)


   DEFINE lc_prioridad CHAR (100)
   DEFINE li_estatus_cuenta,
          li_edad            SMALLINT

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   CALL nombra_archivo() RETURNING lc_encabezado    ,
                                   lc_sub_encabezado,
                                   lc_detalle1      ,
                                   lc_detalle2      ,
                                   lc_detalle3      ,
                                   lc_detalle4      ,
                                   lc_archivo_final

   LET lc_encabezado     = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado     CLIPPED
   LET lc_sub_encabezado = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_sub_encabezado CLIPPED
   LET lc_detalle1       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle1       CLIPPED
   LET lc_detalle2       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle2       CLIPPED
   LET lc_detalle3       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle3       CLIPPED
   LET lc_detalle4       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle4       CLIPPED
   LET lc_archivo_final  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final  CLIPPED

   LET lc_comando_cat = lc_comando_cat    CLIPPED, " ",
                        lc_encabezado     CLIPPED, " ",
                        lc_sub_encabezado CLIPPED, " ",
                        lc_detalle1       CLIPPED, " ",
                        lc_detalle2       CLIPPED, " ",
                        lc_detalle3       CLIPPED, " ",
                        lc_detalle4       CLIPPED, " > ",
                        lc_archivo_final  CLIPPED

   LET lc_comando_chmod = "chmod 777 ",
                          lc_encabezado     CLIPPED, ";",
                          "chmod 777 ",
                          lc_sub_encabezado CLIPPED, ";",
                          "chmod 777 ",
                          lc_detalle1       CLIPPED, ";",
                          "chmod 777 ",
                          lc_detalle2       CLIPPED, ";",
                          "chmod 777 ",
                          lc_detalle3       CLIPPED, ";",
                          "chmod 777 ",
                          lc_detalle4       CLIPPED, ";",
                          "chmod 777 ",
                          lc_archivo_final  CLIPPED, ";"

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   --EXECUTE prep_prioridadh

   #/ruta/formato2006_enc_DDMMYYYYY
   #/ruta/formato2006_sub_DDMMYYYYY
   #/ruta/formato2006_det1_DDMMYYYYY
   #/ruta/formato2006_det2_DDMMYYYYY
   #/ruta/formato2006_det3_DDMMYYYYY
   #/ruta/FORMATO_2006_DDMMYYYYY_xxx

   CALL detalle1(lc_detalle1)
   CALL detalle2(lc_detalle2)
   CALL detalle3(lc_detalle3)
   CALL detalle4(lc_detalle4)
   CALL sub_encabezado(lc_sub_encabezado)
   CALL encabezado(lc_encabezado)

   #Concatenar y Permisos
   RUN lc_comando_cat
   RUN lc_comando_chmod

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_archivo_final)

   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   --EXECUTE prep_prioridadl
END FUNCTION
################################################################################
FUNCTION nombra_archivo()
   DEFINE lc_encabezado    ,
          lc_sub_encabezado,
          lc_detalle1      ,
          lc_detalle2      ,
          lc_detalle3      ,
          lc_detalle4      ,
          lc_archivo_final CHAR(80)

   LET lc_encabezado     = "formato2006_enc_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_sub_encabezado = "formato2006_sub_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle1       = "formato2006_det1_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle2       = "formato2006_det2_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle3       = "formato2006_det3_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle3       = "formato2006_det4_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_archivo_final  = "FORMATO_2006_"    ,gd_fecha_corte USING "DDMMYYYY",
                            "_", gi_afore USING "&&&"

   RETURN lc_encabezado    ,
          lc_sub_encabezado,
          lc_detalle1      ,
          lc_detalle2      ,
          lc_detalle3      ,
          lc_archivo_final
END FUNCTION
################################################################################
FUNCTION encabezado(lc_encabezado)
   DEFINE lc_encabezado    CHAR(200)

   LET gr_encabezado.tipo_registro = 0
   LET gr_encabezado.tipo_archivo  = 2006
   LET gr_encabezado.tipo_entidad  = 1
   LET gr_encabezado.entidad       = gi_afore
   CALL habil_siguiente(gd_fecha_corte, 5) RETURNING gr_encabezado.fecha_envio
   LET gr_encabezado.longitud      = 119

   #gi_registros se incrementa con cada linea impresa
   LET gr_encabezado.num_registros = gi_registros + 1

   START REPORT rpt_encabezado TO lc_encabezado
      OUTPUT TO REPORT rpt_encabezado(gr_encabezado.*)
   FINISH REPORT rpt_encabezado
END FUNCTION
################################################################################
REPORT rpt_encabezado(lr_encabezado)
   DEFINE lr_encabezado RECORD
   	   tipo_registro  SMALLINT,
  	   tipo_archivo   SMALLINT,
  	   tipo_entidad   SMALLINT,
  	   entidad        SMALLINT,
  	   fecha_envio    DATE    ,
  	   longitud       SMALLINT,
  	   num_registros  INTEGER
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, lr_encabezado.tipo_registro USING "&&&"     ,
            COLUMN 04, lr_encabezado.tipo_archivo  USING "&&&&"    ,
            COLUMN 08, lr_encabezado.tipo_entidad  USING "&&&"     ,
            COLUMN 11, lr_encabezado.entidad       USING "&&&"     ,
            COLUMN 14, lr_encabezado.fecha_envio   USING "YYYYMMDD",
            COLUMN 22, lr_encabezado.longitud      USING "&&&"     ,
            COLUMN 25, lr_encabezado.num_registros USING "&&&&&"   ,
            COLUMN 30, 90 SPACES
END REPORT
################################################################################
FUNCTION sub_encabezado(lc_sub_enc)
   DEFINE lc_sub_enc    CHAR(200)

   LET gr_subenc.tipo_registro   = 101
   LET gr_subenc.tipo_entidad    = 1
   LET gr_subenc.entidad         = gi_afore
   LET gr_subenc.periodicidad    = 3
   LET gr_subenc.tipo_proceso    = 5
   LET gr_subenc.fecha_corte     = gd_fecha_corte

   START REPORT rpt_sub_enc TO lc_sub_enc
      OUTPUT TO REPORT rpt_sub_enc(gr_subenc.*)
   FINISH REPORT rpt_sub_enc
END FUNCTION
################################################################################
REPORT rpt_sub_enc(lr_subenc)
   DEFINE lr_subenc RECORD
   	     tipo_registro          SMALLINT     ,
  	     tipo_entidad           SMALLINT     ,
  	     entidad                SMALLINT     ,
  	     periodicidad           SMALLINT     ,
  	     tipo_proceso           SMALLINT     ,
  	     fecha_corte            DATE
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW

      PRINT COLUMN 001, lr_subenc.tipo_registro          USING "&&&"      ,
            COLUMN 004, lr_subenc.tipo_entidad           USING "&&&"      ,
            COLUMN 007, lr_subenc.entidad                USING "&&&"      ,
            COLUMN 010, lr_subenc.periodicidad           USING "&"        ,
            COLUMN 011, lr_subenc.tipo_proceso           USING "&&&"      ,
            COLUMN 014, lr_subenc.fecha_corte            USING "YYYYMMDD" ,
            COLUMN 022, 98 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle1(lc_detalle1)
   DEFINE lc_detalle1    CHAR(200)

   LET gr_det1.tipo_registro  = 301

   #Falta
   LET gr_det1.sector        = 0
   LET gr_det1.clasificacion = 0
   LET gr_det1.rango_semanas = 0
   LET gr_det1.total_trabs   = 0

   START REPORT rpt_detalle1 TO lc_detalle1
      OUTPUT TO REPORT rpt_detalle1(gr_det1.*)
   FINISH REPORT rpt_detalle1
END FUNCTION
################################################################################
REPORT rpt_detalle1(lr_det1)
   DEFINE lr_det1 RECORD
   	      tipo_registro   SMALLINT     ,
  	      sector          SMALLINT     ,
  	      clasificacion   SMALLINT     ,
  	      rango_semanas   SMALLINT     ,
  	      total_trabs     DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, lr_det1.tipo_registro USING "&&&"      ,
            COLUMN 04, lr_det1.sector        USING "&"        ,
            COLUMN 05, lr_det1.clasificacion USING "&"        ,
            COLUMN 06, lr_det1.rango_semanas USING "&&&"      ,
            COLUMN 09, lr_det1.total_trabs   USING "&&&&&&&&&",
            COLUMN 18, 102 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle2(lc_detalle2)
   DEFINE lc_detalle2    CHAR(200)

   LET gr_det2.tipo_registro = 302

   #Falta
   LET gr_det2.tipo_registro  = 0
   LET gr_det2.sector         = 0
   LET gr_det2.clasificacion  = 0
   LET gr_det2.rango_densidad = 0
   LET gr_det2.rango_edad     = 0
   LET gr_det2.rango_salario  = 0
   LET gr_det2.sexo           = 0
   LET gr_det2.total_trabs    = 0

   START REPORT rpt_detalle2 TO lc_detalle2
      OUTPUT TO REPORT rpt_detalle2(gr_det2.*)
   FINISH REPORT rpt_detalle2
END FUNCTION
################################################################################
REPORT rpt_detalle2(lr_det2)
   DEFINE lr_det2 RECORD
   	     tipo_registro   SMALLINT     ,
  	     sector          SMALLINT     ,
  	     clasificacion   SMALLINT     ,
  	     rango_densidad  SMALLINT     ,
  	     rango_edad      SMALLINT     ,
  	     rango_salario   SMALLINT     ,
  	     sexo            SMALLINT     ,
  	     total_trabs     DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, lr_det2.tipo_registro  USING "&&&"      ,
            COLUMN 04, lr_det2.sector         USING "&"        ,
            COLUMN 05, lr_det2.clasificacion  USING "&"        ,
            COLUMN 06, lr_det2.rango_densidad USING "&&&"      ,
            COLUMN 09, lr_det2.rango_edad     USING "&&&"      ,
            COLUMN 12, lr_det2.rango_salario  USING "&&&"      ,
            COLUMN 15, lr_det2.sexo           USING "&"        ,
            COLUMN 15, lr_det2.total_trabs    USING "&&&&&&&&&",
            COLUMN 25, 95 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle3(lc_detalle3)
   DEFINE lc_detalle3    CHAR(200)

   LET gr_det3.tipo_registro           = 303 - 300

   #Falta
   LET gr_det3.total_trabs_issste      = 0
   LET gr_det3.total_trabs_mixtos      = 0
   LET gr_det3.total_hasta27           = 0
   LET gr_det3.total_hasta36           = 0
   LET gr_det3.total_hasta45           = 0
   LET gr_det3.total_hasta55           = 0
   LET gr_det3.total_hasta99           = 0
   LET gr_det3.total_mujeres           = 0
   LET gr_det3.total_hombres           = 0
   LET gr_det3.total_densidad_hasta25  = 0
   LET gr_det3.total_densidad_hasta50  = 0
   LET gr_det3.total_densidad_hasta75  = 0
   LET gr_det3.total_densidad_hasta100 = 0

   #Guardar histórico
   CALL inserta_historico(gr_det3.*)

   START REPORT rpt_detalle3 TO lc_detalle3
      OUTPUT TO REPORT rpt_detalle3(gr_det3.*)
   FINISH REPORT rpt_detalle3
END FUNCTION
################################################################################
REPORT rpt_detalle3(lr_det3)
   DEFINE lr_det3 RECORD
   	     tipo_registro           SMALLINT     ,
  	     total_trabs_issste      DECIMAL(10,0),
  	     total_trabs_mixtos      DECIMAL(10,0),
  	     total_hasta27           DECIMAL(10,0),
  	     total_hasta36           DECIMAL(10,0),
  	     total_hasta45           DECIMAL(10,0),
  	     total_hasta55           DECIMAL(10,0),
  	     total_hasta99           DECIMAL(10,0),
  	     total_mujeres           DECIMAL(10,0),
  	     total_hombres           DECIMAL(10,0),
  	     total_densidad_hasta25  DECIMAL(10,0),
  	     total_densidad_hasta50  DECIMAL(10,0),
  	     total_densidad_hasta75  DECIMAL(10,0),
  	     total_densidad_hasta100 DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, lr_det3.tipo_registro           USING "&&"       ,
            COLUMN 003, lr_det3.total_trabs_issste      USING "&&&&&&&&&",
            COLUMN 012, lr_det3.total_trabs_mixtos      USING "&&&&&&&&&",
            COLUMN 021, lr_det3.total_hasta27           USING "&&&&&&&&&",
            COLUMN 030, lr_det3.total_hasta36           USING "&&&&&&&&&",
            COLUMN 039, lr_det3.total_hasta45           USING "&&&&&&&&&",
            COLUMN 048, lr_det3.total_hasta55           USING "&&&&&&&&&",
            COLUMN 057, lr_det3.total_hasta99           USING "&&&&&&&&&",
            COLUMN 066, lr_det3.total_mujeres           USING "&&&&&&&&&",
            COLUMN 075, lr_det3.total_hombres           USING "&&&&&&&&&",
            COLUMN 084, lr_det3.total_densidad_hasta25  USING "&&&&&&&&&",
            COLUMN 093, lr_det3.total_densidad_hasta50  USING "&&&&&&&&&",
            COLUMN 102, lr_det3.total_densidad_hasta75  USING "&&&&&&&&&",
            COLUMN 111, lr_det3.total_densidad_hasta100 USING "&&&&&&&&&"

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle4(lc_detalle4)
   DEFINE lc_detalle4    CHAR(200)

   DEFINE lr_timestre_ant,
          lr_timestre_act RECORD
             fecha_corte             DATE         ,
             total_trabs_issste      DECIMAL(10,0),
  	         total_trabs_mixtos      DECIMAL(10,0),
  	         total_hasta27           DECIMAL(10,0),
  	         total_hasta36           DECIMAL(10,0),
  	         total_hasta45           DECIMAL(10,0),
  	         total_hasta55           DECIMAL(10,0),
  	         total_hasta99           DECIMAL(10,0),
  	         total_mujeres           DECIMAL(10,0),
  	         total_hombres           DECIMAL(10,0),
  	         total_densidad_hasta25  DECIMAL(10,0),
  	         total_densidad_hasta50  DECIMAL(10,0),
  	         total_densidad_hasta75  DECIMAL(10,0),
  	         total_densidad_hasta100 DECIMAL(10,0)
   END RECORD

   #Obtenemos la fecha de corte del periodo anterior
   CALL trimestre_anterior(gd_fecha_corte) RETURNING lr_timestre_ant.fecha_corte

   #Calcular valores actuales
   LET lr_timestre_act.fecha_corte = gd_fecha_corte

   SELECT *
   INTO   lr_timestre_ant.*
   FROM   safre_af:cta_hist_formato2006
   WHERE  fecha_corte = lr_timestre_ant.fecha_corte

   SELECT *
   INTO   lr_timestre_act.*
   FROM   safre_af:cta_hist_formato2006
   WHERE  fecha_corte = gd_fecha_corte

   LET gr_det4.tipo_registro     = 304 - 300

   #Falta
   LET gr_det4.dif_total_trabs_issste      = lr_timestre_act.total_trabs_issste      - lr_timestre_ant.total_trabs_issste
   LET gr_det4.dif_total_trabs_mixtos      = lr_timestre_act.total_trabs_mixtos      - lr_timestre_ant.total_trabs_mixtos
   LET gr_det4.dif_total_hasta27           = lr_timestre_act.total_hasta27           - lr_timestre_ant.total_hasta27
   LET gr_det4.dif_total_hasta36           = lr_timestre_act.total_hasta36           - lr_timestre_ant.total_hasta36
   LET gr_det4.dif_total_hasta45           = lr_timestre_act.total_hasta45           - lr_timestre_ant.total_hasta45
   LET gr_det4.dif_total_hasta55           = lr_timestre_act.total_hasta55           - lr_timestre_ant.total_hasta55
   LET gr_det4.dif_total_hasta99           = lr_timestre_act.total_hasta99           - lr_timestre_ant.total_hasta99
   LET gr_det4.dif_total_mujeres           = lr_timestre_act.total_mujeres           - lr_timestre_ant.total_mujeres
   LET gr_det4.dif_total_hombres           = lr_timestre_act.total_hombres           - lr_timestre_ant.total_hombres
   LET gr_det4.dif_total_densidad_hasta25  = lr_timestre_act.total_densidad_hasta25  - lr_timestre_ant.total_densidad_hasta25
   LET gr_det4.dif_total_densidad_hasta50  = lr_timestre_act.total_densidad_hasta50  - lr_timestre_ant.total_densidad_hasta50
   LET gr_det4.dif_total_densidad_hasta75  = lr_timestre_act.total_densidad_hasta75  - lr_timestre_ant.total_densidad_hasta75
   LET gr_det4.dif_total_densidad_hasta100 = lr_timestre_act.total_densidad_hasta100 - lr_timestre_ant.total_densidad_hasta100

   START REPORT rpt_detalle4 TO lc_detalle4
      OUTPUT TO REPORT rpt_detalle4(gr_det4.*)
   FINISH REPORT rpt_detalle4
END FUNCTION
################################################################################
REPORT rpt_detalle4(lr_det4)
   DEFINE lr_det4 RECORD
   	     tipo_registro               SMALLINT     ,
  	     dif_total_trabs_issste      DECIMAL(10,0),
  	     dif_total_trabs_mixtos      DECIMAL(10,0),
  	     dif_total_hasta27           DECIMAL(10,0),
  	     dif_total_hasta36           DECIMAL(10,0),
  	     dif_total_hasta45           DECIMAL(10,0),
  	     dif_total_hasta55           DECIMAL(10,0),
  	     dif_total_hasta99           DECIMAL(10,0),
  	     dif_total_mujeres           DECIMAL(10,0),
  	     dif_total_hombres           DECIMAL(10,0),
  	     dif_total_densidad_hasta25  DECIMAL(10,0),
  	     dif_total_densidad_hasta50  DECIMAL(10,0),
  	     dif_total_densidad_hasta75  DECIMAL(10,0),
  	     dif_total_densidad_hasta100 DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, lr_det4.tipo_registro               USING "&&"       ,
            COLUMN 003, lr_det4.dif_total_trabs_issste      USING "&&&&&&&&&",
            COLUMN 012, lr_det4.dif_total_trabs_mixtos      USING "&&&&&&&&&",
            COLUMN 021, lr_det4.dif_total_hasta27           USING "&&&&&&&&&",
            COLUMN 030, lr_det4.dif_total_hasta36           USING "&&&&&&&&&",
            COLUMN 039, lr_det4.dif_total_hasta45           USING "&&&&&&&&&",
            COLUMN 048, lr_det4.dif_total_hasta55           USING "&&&&&&&&&",
            COLUMN 057, lr_det4.dif_total_hasta99           USING "&&&&&&&&&",
            COLUMN 066, lr_det4.dif_total_mujeres           USING "&&&&&&&&&",
            COLUMN 075, lr_det4.dif_total_hombres           USING "&&&&&&&&&",
            COLUMN 084, lr_det4.dif_total_densidad_hasta25  USING "&&&&&&&&&",
            COLUMN 093, lr_det4.dif_total_densidad_hasta50  USING "&&&&&&&&&",
            COLUMN 102, lr_det4.dif_total_densidad_hasta75  USING "&&&&&&&&&",
            COLUMN 111, lr_det4.dif_total_densidad_hasta100 USING "&&&&&&&&&"

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION Ingresa_etapa(li_folio,li_etapa_cod,lc_resultado)
   DEFINE li_folio      INTEGER,
          li_etapa_cod  DECIMAL(2,0),
          lc_resultado  CHAR(50)

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES
      (TODAY,             -- fecha_proceso
       "CTANX17",         -- proceso_cod
       li_etapa_cod,      -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       "saldos_isste",    -- parametro1
       gd_fecha_corte,    -- parametro2  fecha_corte
       gi_tot_nss,        -- parametro3  tot_nss
       NULL,              -- parametro4
       NULL,              -- parametro5
       li_folio,          -- folio
       lc_resultado,      -- resultado
       USER,              -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso etapa ",
            li_etapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
################################################################################
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vpos,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          vpos         INTEGER

   DEFINE hora_inicial       CHAR(08),
          vhora_final        CHAR(08)

   DEFINE cla_sel            CHAR(400),
          vconsecutivo       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   safre_af:dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTANX17' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE safre_af:dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        parametro4 = ",vpos,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'CTANX17'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################
FUNCTION habil_siguiente(diaActual,numDiaHabil)
   DEFINE
       diaTmp	              ,
       diaHabilSig	        ,
       diaActual	          DATE

   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	              ,
       feriado	                ,
       finSemana	        SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       ELSE
           SELECT *
           FROM   safre_af:tab_feriado
           WHERE  feria_fecha = diaHabilSig

           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF

       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION
################################################################################
FUNCTION trimestre_anterior(ld_fecha_corte)
   DEFINE ld_fecha_corte,
          ld_paso       DATE

   LET ld_paso = MDY(MONTH (ld_fecha_corte) + 1, 1, YEAR(ld_fecha_corte))
   LET ld_paso = ld_paso - 3 UNITS MONTH
   LET ld_paso = ld_paso - 1 UNITS DAY

   RETURN ld_paso
END FUNCTION
################################################################################
FUNCTION inserta_historico(lr_formato2006)
   DEFINE lr_formato2006 RECORD
      tipo_registro           SMALLINT     ,
  	  total_trabs_issste      DECIMAL(10,0),
  	  total_trabs_mixtos      DECIMAL(10,0),
  	  total_hasta27           DECIMAL(10,0),
  	  total_hasta36           DECIMAL(10,0),
  	  total_hasta45           DECIMAL(10,0),
  	  total_hasta55           DECIMAL(10,0),
  	  total_hasta99           DECIMAL(10,0),
  	  total_mujeres           DECIMAL(10,0),
  	  total_hombres           DECIMAL(10,0),
  	  total_densidad_hasta25  DECIMAL(10,0),
  	  total_densidad_hasta50  DECIMAL(10,0),
  	  total_densidad_hasta75  DECIMAL(10,0),
  	  total_densidad_hasta100 DECIMAL(10,0)
   END RECORD

   DELETE
   FROM   safre_af:cta_hist_formato2006
   WHERE  fecha_corte = gd_fecha_corte

   INSERT INTO safre_af:cta_hist_formato2006 VALUES(
      gd_fecha_corte                        , --fecha_corte
      lr_formato2006.total_trabs_issste     , --total_trabs_issste
      lr_formato2006.total_trabs_mixtos     , --total_trabs_mixtos
      lr_formato2006.total_hasta27          , --total_hasta27
      lr_formato2006.total_hasta36          , --total_hasta36
      lr_formato2006.total_hasta45          , --total_hasta45
      lr_formato2006.total_hasta55          , --total_hasta55
      lr_formato2006.total_hasta99          , --total_hasta99
      lr_formato2006.total_mujeres          , --total_mujeres
      lr_formato2006.total_hombres          , --total_hombres
      lr_formato2006.total_densidad_hasta25 , --total_densidad_hasta25
      lr_formato2006.total_densidad_hasta50 , --total_densidad_hasta50
      lr_formato2006.total_densidad_hasta75 , --total_densidad_hasta75
      lr_formato2006.total_densidad_hasta100, --total_densidad_hasta100
      hoy                                   , --factualiza
      gc_usuario                              --usuario
   )
END FUNCTION
################################################################################
FUNCTION formato_saldo(ld_saldo)
   DEFINE ld_saldo DECIMAL(22,2)
   DEFINE lc_saldo CHAR(16)

   IF ld_saldo < 0 THEN
      LET lc_saldo = ld_saldo * 100 USING "-&&&&&&&&&&&&&&&"
   ELSE
   	  LET lc_saldo = ld_saldo * 100 USING "&&&&&&&&&&&&&&&&"
   END IF

   RETURN lc_saldo
END FUNCTION
################################################################################