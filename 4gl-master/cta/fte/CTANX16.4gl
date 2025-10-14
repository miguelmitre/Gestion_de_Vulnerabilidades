################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX16    => FORMATO 2005                                           #
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
  	     fecha_corte            DATE         ,
  	     saldo_total_imss_ant   DECIMAL(22,6),
  	     saldo_total_issste_ant DECIMAL(22,6),
  	     saldo_total_indepe_ant DECIMAL(22,6),
  	     saldo_total_mixtos_ant DECIMAL(22,6),
  	     trabs_imss_act         DECIMAL(10,0),
  	     trabs_issste_act       DECIMAL(10,0),
  	     trabs_indepe_act       DECIMAL(10,0),
  	     trabs_mixtos_act       DECIMAL(10,0)
  END RECORD

  DEFINE gr_det1 RECORD
  	 tipo_registro   SMALLINT     ,
  	 tipo_saldo      SMALLINT     ,
  	 saldo_total     DECIMAL(22,6),
  	 saldo_promedio  DECIMAL(22,6),
  	 saldo_viv       DECIMAL(22,6),
  	 saldo_viv_prom  DECIMAL(22,6),
  	 total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det2 RECORD
  	  tipo_registro   SMALLINT     ,
  	  sector          SMALLINT     ,
  	  clasificacion   SMALLINT     ,
  	  subcuenta       SMALLINT     ,
  	  saldo           DECIMAL(22,6),
  	  total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det3 RECORD
  	  tipo_registro           SMALLINT     ,
  	  dif_saldo_total_imss    DECIMAL(22,6),
      dif_saldo_total_issste  DECIMAL(22,6),
      dif_saldo_total_indepe  DECIMAL(22,6),
      dif_saldo_total_mixtos  DECIMAL(22,6),
      dif_trabs_imss          DECIMAL(10,0),
      dif_trabs_issste        DECIMAL(10,0),
      dif_trabs_indepe        DECIMAL(10,0),
      dif_trabs_mixtos        DECIMAL(10,0)
  END RECORD
END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE FORMATO 2004",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX16.log")

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

   CALL limpia_historico()
   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
#####################################################################
FUNCTION limpia_historico()
   DELETE
   FROM  cta_his_fmto2005_det1
   WHERE fecha_corte = gd_fecha_corte

   DELETE
   FROM  cta_his_fmto2005_det2
   WHERE fecha_corte = gd_fecha_corte

   DELETE
   FROM  cta_his_fmto2005_det21
   WHERE fecha_corte = gd_fecha_corte
END FUNCTION
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
                                   lc_archivo_final

   LET lc_encabezado     = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado     CLIPPED
   LET lc_sub_encabezado = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_sub_encabezado CLIPPED
   LET lc_detalle1       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle1       CLIPPED
   LET lc_detalle2       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle2       CLIPPED
   LET lc_detalle3       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle3       CLIPPED
   LET lc_archivo_final  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final  CLIPPED

   LET lc_comando_cat = lc_comando_cat    CLIPPED, " ",
                        lc_encabezado     CLIPPED, " ",
                        lc_sub_encabezado CLIPPED, " ",
                        lc_detalle1       CLIPPED, " ",
                        lc_detalle2       CLIPPED, " ",
                        lc_detalle3       CLIPPED, " > ",
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
                          lc_archivo_final  CLIPPED, ";"

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   --EXECUTE prep_prioridadh

   #/ruta/formato2005_enc_DDMMYYYYY
   #/ruta/formato2005_sub_DDMMYYYYY
   #/ruta/formato2005_det1_DDMMYYYYY
   #/ruta/formato2005_det2_DDMMYYYYY
   #/ruta/formato2005_det3_DDMMYYYYY
   #/ruta/FORMATO_2005_DDMMYYYYY_xxx

   CALL detalle1(lc_detalle1)
   CALL detalle2(lc_detalle2)
   CALL detalle3(lc_detalle3)
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
          lc_archivo_final CHAR(80)

   LET lc_encabezado     = "formato2005_enc_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_sub_encabezado = "formato2005_sub_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle1       = "formato2005_det1_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle2       = "formato2005_det2_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle3       = "formato2005_det3_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_archivo_final  = "FORMATO_2005_"    ,gd_fecha_corte USING "DDMMYYYY",
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
   LET gr_encabezado.tipo_archivo  = 2005
   LET gr_encabezado.tipo_entidad  = 1
   LET gr_encabezado.entidad       = gi_afore
   CALL habil_siguiente(gd_fecha_corte, 7) RETURNING gr_encabezado.fecha_envio
   LET gr_encabezado.longitud      = 129

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
            COLUMN 30, 100 SPACES
END REPORT
################################################################################
FUNCTION sub_encabezado(lc_sub_enc)
   DEFINE lc_sub_enc    CHAR(200)

   LET gr_subenc.tipo_registro   = 101
   LET gr_subenc.tipo_entidad    = 1
   LET gr_subenc.entidad         = gi_afore
   LET gr_subenc.periodicidad    = 3
   LET gr_subenc.tipo_proceso    = 2
   LET gr_subenc.fecha_corte     = gd_fecha_corte

   #Falta
   #Se obtienen de lo que se calcula en el detalle3
   --LET gr_subenc.saldo_total_imss_ant   = 0
   --LET gr_subenc.saldo_total_issste_ant = 0
   --LET gr_subenc.saldo_total_indepe_ant = 0
   --LET gr_subenc.saldo_total_mixtos_ant = 0
   --LET gr_subenc.trabs_imss_act         = 0
   --LET gr_subenc.trabs_issste_act       = 0
   --LET gr_subenc.trabs_indepe_act       = 0
   --LET gr_subenc.trabs_mixtos_act       = 0

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
  	     fecha_corte            DATE         ,
  	     saldo_total_imss_ant   DECIMAL(22,6),
  	     saldo_total_issste_ant DECIMAL(22,6),
  	     saldo_total_indepe_ant DECIMAL(22,6),
  	     saldo_total_mixtos_ant DECIMAL(22,6),
  	     trabs_imss_act         DECIMAL(10,0),
  	     trabs_issste_act       DECIMAL(10,0),
  	     trabs_indepe_act       DECIMAL(10,0),
  	     trabs_mixtos_act       DECIMAL(10,0)
	 END RECORD

	 DEFINE ld_saldo_total_imss_ant   DECIMAL(22,2),
  	      ld_saldo_total_issste_ant DECIMAL(22,2),
  	      ld_saldo_total_indepe_ant DECIMAL(22,2),
  	      ld_saldo_total_mixtos_ant DECIMAL(22,2)

   DEFINE lc_saldo_total_imss_ant   CHAR(18),
  	      lc_saldo_total_issste_ant CHAR(18),
  	      lc_saldo_total_indepe_ant CHAR(18),
  	      lc_saldo_total_mixtos_ant CHAR(18)

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET ld_saldo_total_imss_ant   = lr_subenc.saldo_total_imss_ant   * 1
      LET ld_saldo_total_issste_ant = lr_subenc.saldo_total_issste_ant * 1
      LET ld_saldo_total_indepe_ant = lr_subenc.saldo_total_indepe_ant * 1
      LET ld_saldo_total_mixtos_ant = lr_subenc.saldo_total_mixtos_ant * 1

      CALL formato_saldo(ld_saldo_total_imss_ant)   RETURNING lc_saldo_total_imss_ant
      CALL formato_saldo(ld_saldo_total_issste_ant) RETURNING lc_saldo_total_issste_ant
      CALL formato_saldo(ld_saldo_total_indepe_ant) RETURNING lc_saldo_total_indepe_ant
      CALL formato_saldo(ld_saldo_total_mixtos_ant) RETURNING lc_saldo_total_mixtos_ant

      PRINT COLUMN 001, lr_subenc.tipo_registro          USING "&&&"      ,
            COLUMN 004, lr_subenc.tipo_entidad           USING "&&&"      ,
            COLUMN 007, lr_subenc.entidad                USING "&&&"      ,
            COLUMN 010, lr_subenc.periodicidad           USING "&"        ,
            COLUMN 011, lr_subenc.tipo_proceso           USING "&&&"      ,
            COLUMN 014, lr_subenc.fecha_corte            USING "YYYYMMDD" ,
            COLUMN 022, lc_saldo_total_imss_ant                           ,
            COLUMN 040, lc_saldo_total_issste_ant                         ,
            COLUMN 058, lc_saldo_total_indepe_ant                         ,
            COLUMN 076, lc_saldo_total_mixtos_ant                         ,
            COLUMN 094, lr_subenc.trabs_imss_act         USING "&&&&&&&&&",
            COLUMN 103, lr_subenc.trabs_issste_act       USING "&&&&&&&&&",
            COLUMN 112, lr_subenc.trabs_indepe_act       USING "&&&&&&&&&",
            COLUMN 121, lr_subenc.trabs_mixtos_act       USING "&&&&&&&&&"

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle1(lc_detalle1)
   DEFINE lc_detalle1    CHAR(200)
   DEFINE ls_tipo_saldo  SMALLINT
   DEFINE ls_detalle1    SMALLINT

   DEFINE lr_saldo RECORD
   	      subcuenta SMALLINT,
   	      pesos     DECIMAL(22,6)
   END RECORD

   LET gr_det1.tipo_registro  = 301
   LET ls_detalle1            = 1

   #Falta
   LET gr_det1.tipo_saldo     = 0
   LET gr_det1.saldo_total    = 0
   LET gr_det1.saldo_promedio = 0
   LET gr_det1.saldo_viv      = 0
   LET gr_det1.saldo_viv_prom = 0
   LET gr_det1.total_trabs    = 0

   START REPORT rpt_detalle1 TO lc_detalle1

   #Detalle por tipo de saldo
   FOR gr_det1.tipo_saldo = 1 TO 3
   	  CASE gr_det1.tipo_saldo
   	  	 WHEN 1 --IMSS
   	  	 	  IF gi_afore <> 578 THEN --ISSSTE
   	  	 	     #Todo menos viv
   	  	 	     SELECT SUM(a.monto_en_pesos)
   	  	 	     INTO   gr_det1.saldo_total
   	  	 	     FROM   tmp_saldo_formato2000              a,
   	                  tmp_oficio_cta_adm                 b,
   	                  safre_af:tab_subcuenta_formato2000 c
   	           WHERE  a.nss            = b.nss
   	           AND    a.subcuenta      = c.subcta_safre
   	           AND    c.subcta_agrupa  IN (1,2,3,4,5,7)
   	           AND    b.sector_trab IN (0,  --IMSS
   	                                    3)  --MIXTOS

   	           #Viv
   	           SELECT SUM(a.monto_en_pesos)
   	  	 	     INTO   gr_det1.saldo_viv
   	  	 	     FROM   tmp_saldo_formato2000              a,
   	                  tmp_oficio_cta_adm                 b,
   	                  safre_af:tab_subcuenta_formato2000 c
   	           WHERE  a.nss            = b.nss
   	           AND    a.subcuenta      = c.subcta_safre
   	           AND    c.subcta_agrupa  IN (8,9)
   	           AND    b.sector_trab IN (0,  --IMSS
   	                                    3)  --MIXTOS

               SELECT COUNT(*)
               INTO   gr_det1.total_trabs
               FROM   tmp_oficio_cta_adm
               WHERE  sector_trab IN (0,  --IMSS
   	                                  3)  --MIXTOS
   	        ELSE
   	        	 LET gr_det1.saldo_total = 0
   	        	 LET gr_det1.saldo_viv   = 0
   	        	 LET gr_det1.total_trabs = 0
   	        END IF

         WHEN 2 --ISSSTE
         	  #Todo menos viv
         	  SELECT SUM(a.monto_en_pesos)
   	  	 	  INTO   gr_det1.saldo_total
   	  	 	  FROM   tmp_saldo_formato2000              a,
   	               tmp_oficio_cta_adm                 b,
   	               safre_af:tab_subcuenta_formato2000 c
   	        WHERE  a.nss            = b.nss
   	        AND    a.subcuenta      = c.subcta_safre
   	        AND    c.subcta_agrupa  IN (10,11,12,13,14)
   	        AND    b.sector_trab IN (1,  --ISSSTE
   	                                 3)  --MIXTOS

   	  	 	  #Viv
   	  	 	  SELECT SUM(a.monto_en_pesos)
   	  	 	  INTO   gr_det1.saldo_viv
   	  	 	  FROM   tmp_saldo_formato2000              a,
   	               tmp_oficio_cta_adm                 b,
   	               safre_af:tab_subcuenta_formato2000 c
   	        WHERE  a.nss            = b.nss
   	        AND    a.subcuenta      = c.subcta_safre
   	        AND    c.subcta_agrupa  IN (15,16)
   	        AND    b.sector_trab IN (1,  --ISSSTE
   	                                 3)  --MIXTOS

            SELECT COUNT(*)
            INTO   gr_det1.total_trabs
            FROM   tmp_oficio_cta_adm
            WHERE  sector_trab IN (1,  --ISSSTE
   	                               3)  --MIXTOS

         WHEN 3 --INDEPENDIENTES
         	  IF gi_afore <> 578 THEN --ISSSTE
         	     #Todo menos viv
         	     SELECT SUM(a.monto_en_pesos)
   	  	 	     INTO   gr_det1.saldo_total
   	  	 	     FROM   tmp_saldo_formato2000              a,
   	                  tmp_oficio_cta_adm                 b,
   	                  safre_af:tab_subcuenta_formato2000 c
   	           WHERE  a.nss            = b.nss
   	           AND    a.subcuenta      = c.subcta_safre
   	           AND    c.subcta_agrupa  IN (17,18,19,20,21,22,23)
   	           AND    b.sector_trab    = 2  --INDEPENDIENTES

   	  	 	     #Viv
   	  	 	     #En el tipo de saldo 3 se reportan solo voluntarias
   	  	 	     LET gr_det1.saldo_viv = 0

               SELECT COUNT(*)
               INTO   gr_det1.total_trabs
               FROM   tmp_oficio_cta_adm
               WHERE  sector_trab = 2  --INDEPENDIENTES
            ELSE
   	        	 LET gr_det1.saldo_total = 0
   	        	 LET gr_det1.saldo_viv   = 0
   	        	 LET gr_det1.total_trabs = 0
   	        END IF
   	  END CASE

      IF gr_det1.saldo_total IS NULL THEN
      	 LET gr_det1.saldo_total = 0
      END IF

      IF gr_det1.saldo_viv IS NULL THEN
      	 LET gr_det1.saldo_viv = 0
      END IF

      IF gr_det1.total_trabs IS NULL THEN
      	 LET gr_det1.total_trabs = 0
      END IF

      #Promedios
   	  IF gr_det1.total_trabs > 0 THEN
   	     LET gr_det1.saldo_promedio = gr_det1.saldo_total / gr_det1.total_trabs
         LET gr_det1.saldo_viv_prom = gr_det1.saldo_viv   / gr_det1.total_trabs
      ELSE
      	 LET gr_det1.saldo_promedio = 0
         LET gr_det1.saldo_viv_prom = 0
      END IF

      #Imprime detalle por tipo de trabajador y guarda historico
      --IF gr_det1.total_trabs > 0 THEN
         OUTPUT TO REPORT rpt_detalle1(gr_det1.*)
         CALL inserta_historico(ls_detalle1, gr_det2.*) #gr_det2 aqui no se ocupa
      --END IF

      #Inicializa saldos
      LET gr_det1.saldo_total    = 0
      LET gr_det1.saldo_viv      = 0
   END FOR

   FINISH REPORT rpt_detalle1
END FUNCTION
################################################################################
REPORT rpt_detalle1(lr_det1)
   DEFINE lr_det1 RECORD
   	      tipo_registro   SMALLINT     ,
  	      tipo_saldo      SMALLINT     ,
  	      saldo_total     DECIMAL(22,6),
  	      saldo_promedio  DECIMAL(22,6),
  	      saldo_viv       DECIMAL(22,6),
  	      saldo_viv_prom  DECIMAL(22,6),
  	      total_trabs     DECIMAL(10,0)
	 END RECORD

	 DEFINE ld_saldo_total    DECIMAL(22,2),
          ld_saldo_promedio DECIMAL(22,2),
          ld_saldo_viv      DECIMAL(22,2),
          ld_saldo_viv_prom DECIMAL(22,2)

   DEFINE lc_saldo_total    CHAR(18),
          lc_saldo_promedio CHAR(18),
          lc_saldo_viv      CHAR(18),
          lc_saldo_viv_prom CHAR(18)

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET ld_saldo_total    = lr_det1.saldo_total    * 1
      LET ld_saldo_promedio = lr_det1.saldo_promedio * 1
      LET ld_saldo_viv      = lr_det1.saldo_viv      * 1
      LET ld_saldo_viv_prom = lr_det1.saldo_viv_prom * 1

      CALL formato_saldo(ld_saldo_total   ) RETURNING lc_saldo_total
      CALL formato_saldo(ld_saldo_promedio) RETURNING lc_saldo_promedio
      CALL formato_saldo(ld_saldo_viv     ) RETURNING lc_saldo_viv
      CALL formato_saldo(ld_saldo_viv_prom) RETURNING lc_saldo_viv_prom

      PRINT COLUMN 01, lr_det1.tipo_registro USING "&&&"      ,
            COLUMN 04, lr_det1.tipo_saldo    USING "&"        ,
            COLUMN 05, lc_saldo_total                         ,
            COLUMN 23, lc_saldo_promedio                      ,
            COLUMN 41, lc_saldo_viv                           ,
            COLUMN 59, lc_saldo_viv_prom                      ,
            COLUMN 77, lr_det1.total_trabs   USING "&&&&&&&&&",
            COLUMN 86, 44 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle2(lc_detalle2)
   DEFINE lc_detalle2    CHAR(200)
   DEFINE lr_saldo RECORD
   	      subcuenta SMALLINT,
   	      pesos     DECIMAL(22,6)
   END RECORD

   DEFINE ls_detalle2 SMALLINT
   DEFINE ls_detalle21 SMALLINT

   DEFINE lr_det2 RECORD
  	  tipo_registro   SMALLINT     ,
  	  sector          SMALLINT     ,
  	  clasificacion   SMALLINT     ,
  	  subcuenta       SMALLINT     ,
  	  saldo           DECIMAL(22,6),
  	  total_trabs     DECIMAL(10,0)
  END RECORD

   LET gr_det2.tipo_registro = 302
   LET ls_detalle2           = 2
   LET ls_detalle21          = 21

   #Falta
   LET gr_det2.sector        = 0
   LET gr_det2.clasificacion = 0
   LET gr_det2.subcuenta     = 0
   LET gr_det2.saldo         = 0
   LET gr_det2.total_trabs   = 0

   START REPORT rpt_detalle2 TO lc_detalle2

   FOR gr_det2.sector = 0 TO 3
   	  FOR gr_det2.clasificacion = 0 TO 9
   	  	 LET gr_det2.subcuenta   = 0
         LET gr_det2.saldo       = 0
         LET gr_det2.total_trabs = 0

         IF gi_afore <> 578 OR   --Pst
         	  (gi_afore = 578 AND  --Pst
         	   gr_det2.sector = 1) THEN --ISSSTE

         	   DECLARE cur_saldo_det2 CURSOR FOR
             SELECT b.subcta_agrupa      ,
                    SUM(a.monto_en_pesos),
                    COUNT(UNIQUE a.nss)
             FROM   tmp_saldo_formato2000              a,
                    safre_af:tab_subcuenta_formato2000 b,
                    tmp_oficio_cta_adm                 c
             WHERE  a.subcuenta     = b.subcta_safre
             AND    a.nss           = c.nss
             AND    c.sector_trab   = gr_det2.sector
             AND    c.id_trabajador = gr_det2.clasificacion
             GROUP BY 1
             ORDER BY 1

             LET lr_det2.sector        = gr_det2.sector
  	         LET lr_det2.clasificacion = gr_det2.clasificacion
  	         LET lr_det2.tipo_registro = gr_det2.tipo_registro

  	         SELECT COUNT(*)
             INTO   lr_det2.total_trabs
             FROM   tmp_oficio_cta_adm
             WHERE  sector_trab   = gr_det2.sector
             AND    id_trabajador = gr_det2.clasificacion

             #Solo se imprimen si hay saldo en alguna subcuenta
             FOREACH cur_saldo_det2 INTO gr_det2.subcuenta  ,
                                         gr_det2.saldo      ,
                                         gr_det2.total_trabs

  	            LET lr_det2.subcuenta     = gr_det2.subcuenta
  	            LET lr_det2.saldo         = gr_det2.saldo

                OUTPUT TO REPORT rpt_detalle2(gr_det2.*)
             	  CALL inserta_historico(ls_detalle2, lr_det2.*)  #Se guarda un registro con Nss únicos
             	  CALL inserta_historico(ls_detalle21, gr_det2.*) #Se guarda un registro de detalle 2 tal cual se imprimio
             END FOREACH
         END IF
      END FOR
   END FOR

   FINISH REPORT rpt_detalle2
END FUNCTION
################################################################################
REPORT rpt_detalle2(lr_det2)
   DEFINE lr_det2 RECORD
   	     tipo_registro   SMALLINT     ,
  	     sector          SMALLINT     ,
  	     clasificacion   SMALLINT     ,
  	     subcuenta       SMALLINT     ,
  	     saldo           DECIMAL(22,6),
  	     total_trabs     DECIMAL(10,0)
	 END RECORD

	 DEFINE ld_saldo DECIMAL(22,2)
	 DEFINE lc_saldo CHAR(18)

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET ld_saldo = lr_det2.saldo * 1
      CALL formato_saldo(ld_saldo) RETURNING lc_saldo

      PRINT COLUMN 01, lr_det2.tipo_registro USING "&&&"      ,
            COLUMN 04, lr_det2.sector        USING "&"        ,
            COLUMN 05, lr_det2.clasificacion USING "&"        ,
            COLUMN 06, lr_det2.subcuenta     USING "&&"       ,
            COLUMN 08, lc_saldo                               ,
            COLUMN 26, lr_det2.total_trabs   USING "&&&&&&&&&",
            COLUMN 35, 95 SPACES
      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle3_ant(lc_detalle3)
   DEFINE lc_detalle3    CHAR(200)

   DEFINE lr_trimestre_ant,
          lr_trimestre_act RECORD
             fecha_corte         DATE         ,
             saldo_total_imss    DECIMAL(22,6),
             saldo_total_issste  DECIMAL(22,6),
             saldo_total_indepe  DECIMAL(22,6),
             saldo_total_mixtos  DECIMAL(22,6),
             trabs_imss          DECIMAL(10,0),
             trabs_issste        DECIMAL(10,0),
             trabs_indepe        DECIMAL(10,0),
             trabs_mixtos        DECIMAL(10,0)
   END RECORD

   DEFINE ls_sector SMALLINT

   DEFINE lc_sql CHAR(1000)

   #Calcular valores actuales
   LET lr_trimestre_act.fecha_corte = gd_fecha_corte

   LET lc_sql = "SELECT SUM(a.monto_en_pesos)          ",
                "FROM   tmp_saldo_formato2000 a,       ",
                "       tmp_oficio_cta_adm    b        ",
                "WHERE  a.nss         = b.nss          ",
                "AND    a.subcuenta NOT IN (4,8,14,35) ",
                "AND    b.sector_trab = ?              "

   PREPARE prep_saldos_det3 FROM lc_sql

   LET lc_sql = "SELECT COUNT(*)           ",
                "FROM   tmp_oficio_cta_adm ",
                "WHERE  sector_trab = ?    "

   PREPARE prep_cuantos_det3 FROM lc_sql

   LET lr_trimestre_act.saldo_total_imss   = 0
   LET lr_trimestre_act.saldo_total_issste = 0
   LET lr_trimestre_act.saldo_total_indepe = 0
   LET lr_trimestre_act.saldo_total_mixtos = 0
   LET lr_trimestre_act.trabs_imss         = 0
   LET lr_trimestre_act.trabs_issste       = 0
   LET lr_trimestre_act.trabs_indepe       = 0
   LET lr_trimestre_act.trabs_mixtos       = 0

   #Por sector
   FOR ls_sector = 0 TO 3

   	  IF gi_afore <> 578 OR   --Pst
         (gi_afore = 578 AND  --Pst
          ls_sector = 1) THEN --ISSSTE

         CASE ls_sector
         	  WHEN 0 #IMSS
         	  	     EXECUTE prep_saldos_det3  USING ls_sector
         	  	                               INTO  lr_trimestre_act.saldo_total_imss
         	  	     EXECUTE prep_cuantos_det3 USING ls_sector
         	  	                               INTO  lr_trimestre_act.trabs_imss

         	  	     IF lr_trimestre_act.saldo_total_imss IS NULL THEN
         	  	     	  LET lr_trimestre_act.saldo_total_imss = 0
         	  	     END IF

         	  	     IF lr_trimestre_act.trabs_imss IS NULL THEN
         	  	     	  LET lr_trimestre_act.trabs_imss = 0
         	  	     END IF

         	  WHEN 1  #ISSSTE
         	  	     EXECUTE prep_saldos_det3  USING ls_sector
         	  	                               INTO  lr_trimestre_act.saldo_total_issste
         	  	     EXECUTE prep_cuantos_det3 USING ls_sector
         	  	                               INTO  lr_trimestre_act.trabs_issste

         	  	     IF lr_trimestre_act.saldo_total_issste IS NULL THEN
         	  	     	  LET lr_trimestre_act.saldo_total_issste = 0
         	  	     END IF

         	  	     IF lr_trimestre_act.trabs_issste IS NULL THEN
         	  	     	  LET lr_trimestre_act.trabs_issste = 0
         	  	     END IF

         	  WHEN 2 #INDEPENDIENTES
         	  	     EXECUTE prep_saldos_det3  USING ls_sector
         	  	                               INTO  lr_trimestre_act.saldo_total_indepe
         	  	     EXECUTE prep_cuantos_det3 USING ls_sector
         	  	                               INTO  lr_trimestre_act.trabs_indepe

         	  	     IF lr_trimestre_act.saldo_total_indepe IS NULL THEN
         	  	     	  LET lr_trimestre_act.saldo_total_indepe = 0
         	  	     END IF

         	  	     IF lr_trimestre_act.trabs_indepe IS NULL THEN
         	  	     	  LET lr_trimestre_act.trabs_indepe = 0
         	  	     END IF

         	  WHEN 3 #MIXTOS
         	  	     EXECUTE prep_saldos_det3  USING ls_sector
         	  	                               INTO  lr_trimestre_act.saldo_total_mixtos
         	  	     EXECUTE prep_cuantos_det3 USING ls_sector
         	  	                               INTO  lr_trimestre_act.trabs_mixtos

         	  	     IF lr_trimestre_act.saldo_total_mixtos IS NULL THEN
         	  	     	  LET lr_trimestre_act.saldo_total_mixtos = 0
         	  	     END IF

         	  	     IF lr_trimestre_act.trabs_mixtos IS NULL THEN
         	  	     	  LET lr_trimestre_act.trabs_mixtos = 0
         	  	     END IF
         END CASE
      END IF
   END FOR

   #Insertar valores en la tabla
   --CALL inserta_historico(lr_trimestre_act.*)

   #Obtenemos la fecha de corte del periodo anterior
   CALL trimestre_anterior(gd_fecha_corte) RETURNING lr_trimestre_ant.fecha_corte

   #Saldo IMSS (Excepto viv)
   SELECT SUM(saldo)
   INTO   lr_trimestre_ant.saldo_total_imss
   FROM   cta_his_fmto2005_det2
   WHERE  sector    = 0        --IMSS
   AND    subcuenta NOT IN ( 8, --VIV IMSS   oficio
                             9, --VIV IMSS   oficio
                            15, --VIV ISSSTE oficio
                            16) --VIV ISSSTE oficio

   #Saldo ISSSTE (Excepto viv)
   SELECT SUM(saldo)
   INTO   lr_trimestre_ant.saldo_total_issste
   FROM   cta_his_fmto2005_det2
   WHERE  sector    = 1        --ISSSTE
   AND    subcuenta NOT IN ( 8, --VIV IMSS   oficio
                             9, --VIV IMSS   oficio
                            15, --VIV ISSSTE oficio
                            16) --VIV ISSSTE oficio


   IF gd_fecha_corte = "03/31/2010" AND
   	  gi_afore <> 578 THEN          --Pst
   	  LET lr_trimestre_ant.fecha_corte = "03/31/2010"
   END IF

   SELECT *
   INTO   lr_trimestre_ant.*
   FROM   cta_hist_formato2005
   WHERE  fecha_corte = lr_trimestre_ant.fecha_corte

   SELECT *
   INTO   lr_trimestre_act.*
   FROM   cta_hist_formato2005
   WHERE  fecha_corte = gd_fecha_corte

   LET gr_det3.tipo_registro     = 303 - 300

   LET gr_det3.dif_saldo_total_imss   = lr_trimestre_act.saldo_total_imss   - lr_trimestre_ant.saldo_total_imss
   LET gr_det3.dif_saldo_total_issste = lr_trimestre_act.saldo_total_issste - lr_trimestre_ant.saldo_total_issste
   LET gr_det3.dif_saldo_total_indepe = lr_trimestre_act.saldo_total_indepe - lr_trimestre_ant.saldo_total_indepe
   LET gr_det3.dif_saldo_total_mixtos = lr_trimestre_act.saldo_total_mixtos - lr_trimestre_ant.saldo_total_mixtos
   LET gr_det3.dif_trabs_imss         = lr_trimestre_act.trabs_imss         - lr_trimestre_ant.trabs_imss
   LET gr_det3.dif_trabs_issste       = lr_trimestre_act.trabs_issste       - lr_trimestre_ant.trabs_issste
   LET gr_det3.dif_trabs_indepe       = lr_trimestre_act.trabs_indepe       - lr_trimestre_ant.trabs_indepe
   LET gr_det3.dif_trabs_mixtos       = lr_trimestre_act.trabs_mixtos       - lr_trimestre_ant.trabs_mixtos

   #Subencabezado
   LET gr_subenc.saldo_total_imss_ant   = lr_trimestre_ant.saldo_total_imss
   LET gr_subenc.saldo_total_issste_ant = lr_trimestre_ant.saldo_total_issste
   LET gr_subenc.saldo_total_indepe_ant = lr_trimestre_ant.saldo_total_indepe
   LET gr_subenc.saldo_total_mixtos_ant = lr_trimestre_ant.saldo_total_mixtos
   LET gr_subenc.trabs_imss_act         = lr_trimestre_act.trabs_imss
   LET gr_subenc.trabs_issste_act       = lr_trimestre_act.trabs_issste
   LET gr_subenc.trabs_indepe_act       = lr_trimestre_act.trabs_indepe
   LET gr_subenc.trabs_mixtos_act       = lr_trimestre_act.trabs_mixtos

   START REPORT rpt_detalle3 TO lc_detalle3
      OUTPUT TO REPORT rpt_detalle3(gr_det3.*)
   FINISH REPORT rpt_detalle3
END FUNCTION
################################################################################
FUNCTION detalle3(lc_detalle3)
   DEFINE lc_detalle3    CHAR(200)

   DEFINE lr_trimestre_ant,
          lr_trimestre_act RECORD
             fecha_corte         DATE         ,
             saldo_total_imss    DECIMAL(22,6),
             saldo_total_issste  DECIMAL(22,6),
             saldo_total_indepe  DECIMAL(22,6),
             saldo_total_mixtos  DECIMAL(22,6),
             trabs_imss          DECIMAL(10,0),
             trabs_issste        DECIMAL(10,0),
             trabs_indepe        DECIMAL(10,0),
             trabs_mixtos        DECIMAL(10,0)
   END RECORD

   #Obtenemos la fecha de corte del periodo anterior
   CALL trimestre_anterior(gd_fecha_corte) RETURNING lr_trimestre_ant.fecha_corte
   LET lr_trimestre_act.fecha_corte = gd_fecha_corte

   IF gd_fecha_corte = "03/31/2010" AND
   	  gi_afore <> 578 THEN          --Pst
   	  LET lr_trimestre_ant.fecha_corte = "03/31/2010"
   END IF

   CALL get_historico(lr_trimestre_act.fecha_corte) RETURNING lr_trimestre_act.*
   CALL get_historico(lr_trimestre_ant.fecha_corte) RETURNING lr_trimestre_ant.*

   #Buscar la historia de marzo con las tablas anteriores
   IF lr_trimestre_ant.fecha_corte = "03/31/2010" AND
   	  lr_trimestre_act.fecha_corte = "06/30/2010" THEN
   	  SELECT *
      INTO   lr_trimestre_ant.*
      FROM   cta_hist_formato2005
      WHERE  fecha_corte = lr_trimestre_ant.fecha_corte
   END IF

   LET gr_det3.tipo_registro     = 303

   LET gr_det3.dif_saldo_total_imss   = lr_trimestre_ant.saldo_total_imss   - lr_trimestre_act.saldo_total_imss
   LET gr_det3.dif_saldo_total_issste = lr_trimestre_ant.saldo_total_issste - lr_trimestre_act.saldo_total_issste
   LET gr_det3.dif_saldo_total_indepe = lr_trimestre_ant.saldo_total_indepe - lr_trimestre_act.saldo_total_indepe
   LET gr_det3.dif_saldo_total_mixtos = lr_trimestre_ant.saldo_total_mixtos - lr_trimestre_act.saldo_total_mixtos
   LET gr_det3.dif_trabs_imss         = lr_trimestre_ant.trabs_imss         - lr_trimestre_act.trabs_imss
   LET gr_det3.dif_trabs_issste       = lr_trimestre_ant.trabs_issste       - lr_trimestre_act.trabs_issste
   LET gr_det3.dif_trabs_indepe       = lr_trimestre_ant.trabs_indepe       - lr_trimestre_act.trabs_indepe
   LET gr_det3.dif_trabs_mixtos       = lr_trimestre_ant.trabs_mixtos       - lr_trimestre_act.trabs_mixtos

   #Subencabezado
   LET gr_subenc.saldo_total_imss_ant   = lr_trimestre_ant.saldo_total_imss
   LET gr_subenc.saldo_total_issste_ant = lr_trimestre_ant.saldo_total_issste
   LET gr_subenc.saldo_total_indepe_ant = lr_trimestre_ant.saldo_total_indepe
   LET gr_subenc.saldo_total_mixtos_ant = lr_trimestre_ant.saldo_total_mixtos
   LET gr_subenc.trabs_imss_act         = lr_trimestre_act.trabs_imss
   LET gr_subenc.trabs_issste_act       = lr_trimestre_act.trabs_issste
   LET gr_subenc.trabs_indepe_act       = lr_trimestre_act.trabs_indepe
   LET gr_subenc.trabs_mixtos_act       = lr_trimestre_act.trabs_mixtos

   START REPORT rpt_detalle3 TO lc_detalle3
      OUTPUT TO REPORT rpt_detalle3(gr_det3.*)
   FINISH REPORT rpt_detalle3
END FUNCTION
################################################################################
REPORT rpt_detalle3(lr_det3)
   DEFINE lr_det3 RECORD
   	     tipo_registro           SMALLINT     ,
  	     dif_saldo_total_imss    DECIMAL(22,6),
         dif_saldo_total_issste  DECIMAL(22,6),
         dif_saldo_total_indepe  DECIMAL(22,6),
         dif_saldo_total_mixtos  DECIMAL(22,6),
         dif_trabs_imss          DECIMAL(10,0),
         dif_trabs_issste        DECIMAL(10,0),
         dif_trabs_indepe        DECIMAL(10,0),
         dif_trabs_mixtos        DECIMAL(10,0)
	 END RECORD

	 DEFINE ld_saldo_total_imss    DECIMAL(22,2),
          ld_saldo_total_issste  DECIMAL(22,2),
          ld_saldo_total_indepe  DECIMAL(22,2),
          ld_saldo_total_mixtos  DECIMAL(22,2)

   DEFINE lc_saldo_total_imss    CHAR(18),
          lc_saldo_total_issste  CHAR(18),
          lc_saldo_total_indepe  CHAR(18),
          lc_saldo_total_mixtos  CHAR(18)

   DEFINE lc_dif_trabs_imss          CHAR(09),
          lc_dif_trabs_issste        CHAR(09),
          lc_dif_trabs_indepe        CHAR(09),
          lc_dif_trabs_mixtos        CHAR(09)

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET ld_saldo_total_imss   = lr_det3.dif_saldo_total_imss   * 1
      LET ld_saldo_total_issste = lr_det3.dif_saldo_total_issste * 1
      LET ld_saldo_total_indepe = lr_det3.dif_saldo_total_indepe * 1
      LET ld_saldo_total_mixtos = lr_det3.dif_saldo_total_mixtos * 1

      CALL formato_saldo(ld_saldo_total_imss  ) RETURNING lc_saldo_total_imss
      CALL formato_saldo(ld_saldo_total_issste) RETURNING lc_saldo_total_issste
      CALL formato_saldo(ld_saldo_total_indepe) RETURNING lc_saldo_total_indepe
      CALL formato_saldo(ld_saldo_total_mixtos) RETURNING lc_saldo_total_mixtos

      IF lr_det3.dif_trabs_imss   < 0 THEN LET lc_dif_trabs_imss   = lr_det3.dif_trabs_imss   USING "-&&&&&&&&" ELSE LET lc_dif_trabs_imss   = lr_det3.dif_trabs_imss   USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_issste < 0 THEN LET lc_dif_trabs_issste = lr_det3.dif_trabs_issste USING "-&&&&&&&&" ELSE LET lc_dif_trabs_issste = lr_det3.dif_trabs_issste USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_indepe < 0 THEN LET lc_dif_trabs_indepe = lr_det3.dif_trabs_indepe USING "-&&&&&&&&" ELSE LET lc_dif_trabs_indepe = lr_det3.dif_trabs_indepe USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_mixtos < 0 THEN LET lc_dif_trabs_mixtos = lr_det3.dif_trabs_mixtos USING "-&&&&&&&&" ELSE LET lc_dif_trabs_mixtos = lr_det3.dif_trabs_mixtos USING "&&&&&&&&&" END IF

      PRINT COLUMN 001, lr_det3.tipo_registro    USING "&&&"      ,
            COLUMN 004, lc_saldo_total_imss                       ,
            COLUMN 022, lc_saldo_total_issste                     ,
            COLUMN 040, lc_saldo_total_indepe                     ,
            COLUMN 058, lc_saldo_total_mixtos                     ,
            COLUMN 076, lc_dif_trabs_imss                         ,
            COLUMN 085, lc_dif_trabs_issste                       ,
            COLUMN 094, lc_dif_trabs_indepe                       ,
            COLUMN 103, lc_dif_trabs_mixtos                       ,
            COLUMN 112, 18 SPACES

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
       "CTANX16",         -- proceso_cod
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
                 " AND    proceso_cod = 'CTANX16' ",
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
                 " AND    proceso_cod   = 'CTANX16'",
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
   IF numDiaHabil > 0 THEN
      LET diaHabilSig = diaActual + 1 UNITS DAY
   ELSE
   	  LET diaHabilSig = diaActual
   END IF

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

   --LET ld_paso = MDY(MONTH (ld_fecha_corte) + 1, 1, YEAR(ld_fecha_corte))

   LET ld_paso = MDY(MONTH (ld_fecha_corte), 1, YEAR(ld_fecha_corte))
   LET ld_paso = ld_paso + 1 UNITS MONTH

   LET ld_paso = ld_paso - 3 UNITS MONTH
   LET ld_paso = ld_paso - 1 UNITS DAY

   RETURN ld_paso
END FUNCTION
################################################################################
FUNCTION inserta_historico(ls_tipo_detalle, lr_det2)
   DEFINE ls_tipo_detalle SMALLINT

   DEFINE lr_det2 RECORD
  	  tipo_registro   SMALLINT     ,
  	  sector          SMALLINT     ,
  	  clasificacion   SMALLINT     ,
  	  subcuenta       SMALLINT     ,
  	  saldo           DECIMAL(22,6),
  	  total_trabs     DECIMAL(10,0)
   END RECORD

   CASE ls_tipo_detalle
   	  WHEN 1
   	  	 INSERT INTO cta_his_fmto2005_det1 VALUES(
   	  	    gd_fecha_corte        ,          --fecha_corte
   	  	    gr_det1.tipo_saldo    ,          --tipo_saldo
   	  	    gr_det1.saldo_total   ,          --saldo_total
   	  	    gr_det1.saldo_promedio,          --saldo_promedio
   	  	    gr_det1.saldo_viv     ,          --saldo_viv
   	  	    gr_det1.saldo_viv_prom,          --saldo_viv_prom
   	  	    gr_det1.total_trabs   ,          --total_trabs
   	  	    hoy                   ,          --factualiza
            gc_usuario                       --usuario
   	  	 )
   	  WHEN 2
   	  	 INSERT INTO cta_his_fmto2005_det2 VALUES(
   	  	    gd_fecha_corte       ,           --fecha_corte
   	  	    lr_det2.sector       ,           --sector
            lr_det2.clasificacion,           --clasificacion
            lr_det2.subcuenta    ,           --subcuenta
            lr_det2.saldo        ,           --saldo
            lr_det2.total_trabs  ,           --total_trabs
   	  	    hoy                  ,           --factualiza
            gc_usuario                       --usuario
   	  	 )
   	  WHEN 21
   	  	 INSERT INTO cta_his_fmto2005_det21 VALUES(
   	  	    gd_fecha_corte       ,           --fecha_corte
   	  	    gr_det2.sector       ,           --sector
            gr_det2.clasificacion,           --clasificacion
            gr_det2.subcuenta    ,           --subcuenta
            gr_det2.saldo        ,           --saldo
            gr_det2.total_trabs  ,           --total_trabs
   	  	    hoy                  ,           --factualiza
            gc_usuario                       --usuario
   	  	 )
   END CASE
END FUNCTION
################################################################################
FUNCTION formato_saldo(ld_saldo)
   DEFINE ld_saldo DECIMAL(22,2)
   DEFINE lc_saldo CHAR(18)

   IF ld_saldo < 0 THEN
      LET lc_saldo = ld_saldo * 100 USING "-&&&&&&&&&&&&&&&&&"
   ELSE
   	  LET lc_saldo = ld_saldo * 100 USING "&&&&&&&&&&&&&&&&&&"
   END IF

   RETURN lc_saldo
END FUNCTION
################################################################################
FUNCTION get_historico(ld_fecha_corte)
   DEFINE ld_fecha_corte DATE

   DEFINE lr_formato_2005 RECORD
             fecha_corte         DATE         ,
             saldo_total_imss    DECIMAL(22,6),
             saldo_total_issste  DECIMAL(22,6),
             saldo_total_indepe  DECIMAL(22,6),
             saldo_total_mixtos  DECIMAL(22,6),
             trabs_imss          DECIMAL(10,0),
             trabs_issste        DECIMAL(10,0),
             trabs_indepe        DECIMAL(10,0),
             trabs_mixtos        DECIMAL(10,0)
   END RECORD

   DEFINE lr_saldos RECORD
   	         tipo      SMALLINT,
   	         subcuenta SMALLINT,
   	         saldo     DECIMAL(22,6)
   END RECORD

   DEFINE lr_cuantos RECORD
   	         tipo  SMALLINT,
   	         trabs DECIMAL(10,0)
   END RECORD

   LET lr_formato_2005.fecha_corte        = ld_fecha_corte
   LET lr_formato_2005.saldo_total_imss   = 0
   LET lr_formato_2005.saldo_total_issste = 0
   LET lr_formato_2005.saldo_total_indepe = 0
   LET lr_formato_2005.saldo_total_mixtos = 0
   LET lr_formato_2005.trabs_imss         = 0
   LET lr_formato_2005.trabs_issste       = 0
   LET lr_formato_2005.trabs_indepe       = 0
   LET lr_formato_2005.trabs_mixtos       = 0

   DECLARE cur_saldos_hist CURSOR FOR
   SELECT sector   ,
          subcuenta,
          SUM(saldo)
   FROM   cta_his_fmto2005_det2
   WHERE  fecha_corte = ld_fecha_corte
   AND    subcuenta NOT IN (8,9,15,16)
   GROUP BY 1,2
   ORDER BY 1,2

   #SALDOS
   FOREACH cur_saldos_hist INTO  lr_saldos.tipo,
   	                             lr_saldos.subcuenta,
   	                             lr_saldos.saldo
   	  CASE lr_saldos.tipo
   	  	 WHEN 0 --IMSS
   	  	 	  IF lr_saldos.subcuenta = 01 OR
   	  	 	  	 lr_saldos.subcuenta = 02 OR
   	  	 	  	 lr_saldos.subcuenta = 03 OR
   	  	 	  	 lr_saldos.subcuenta = 04 OR
   	  	 	  	 lr_saldos.subcuenta = 05 OR
   	  	 	  	 lr_saldos.subcuenta = 07 THEN
   	  	 	     LET lr_formato_2005.saldo_total_imss    = lr_formato_2005.saldo_total_imss +
   	  	 	                                               lr_saldos.saldo
   	  	 	  END IF
   	  	 WHEN 1 --ISSSTE
   	  	 	  IF lr_saldos.subcuenta = 10 OR
   	  	 	  	 lr_saldos.subcuenta = 11 OR
   	  	 	  	 lr_saldos.subcuenta = 12 OR
   	  	 	  	 lr_saldos.subcuenta = 13 OR
   	  	 	  	 lr_saldos.subcuenta = 14 THEN
   	  	 	     LET lr_formato_2005.saldo_total_issste   = lr_formato_2005.saldo_total_issste +
   	  	 	                                                lr_saldos.saldo
   	  	 	  END IF
   	  	 WHEN 2 --INDEPENDIENTES
   	  	 	  IF lr_saldos.subcuenta = 17 OR
   	  	 	  	 lr_saldos.subcuenta = 18 OR
   	  	 	  	 lr_saldos.subcuenta = 19 OR
   	  	 	  	 lr_saldos.subcuenta = 20 OR
   	  	 	  	 lr_saldos.subcuenta = 21 OR
   	  	 	  	 lr_saldos.subcuenta = 22 OR
   	  	 	  	 lr_saldos.subcuenta = 23 THEN
   	  	 	     LET lr_formato_2005.saldo_total_indepe   = lr_formato_2005.saldo_total_indepe +
   	  	 	                                                lr_saldos.saldo
   	  	 	  END IF
   	  	 WHEN 3 --MIXTOS
   	  	 	  IF lr_saldos.subcuenta = 01 OR
   	  	 	  	 lr_saldos.subcuenta = 02 OR
   	  	 	  	 lr_saldos.subcuenta = 03 OR
   	  	 	  	 lr_saldos.subcuenta = 04 OR
   	  	 	  	 lr_saldos.subcuenta = 05 OR
   	  	 	  	 lr_saldos.subcuenta = 07 OR
   	  	 	  	 lr_saldos.subcuenta = 10 OR
   	  	 	  	 lr_saldos.subcuenta = 11 OR
   	  	 	  	 lr_saldos.subcuenta = 12 OR
   	  	 	  	 lr_saldos.subcuenta = 13 OR
   	  	 	  	 lr_saldos.subcuenta = 14 THEN
   	  	 	     LET lr_formato_2005.saldo_total_mixtos   = lr_formato_2005.saldo_total_mixtos +
   	  	 	                                                lr_saldos.saldo
   	  	 	  END IF
   	  END CASE
   END FOREACH

   DECLARE cur_cuantos_hist CURSOR FOR
   SELECT sector     ,
          total_trabs
   FROM   cta_his_fmto2005_det2
   WHERE  fecha_corte = ld_fecha_corte
   GROUP BY 1,2
   ORDER BY 1

   #TOTAL DE TRABAJADORES
   FOREACH cur_cuantos_hist INTO lr_cuantos.tipo,
   	                             lr_cuantos.trabs
   	  CASE lr_cuantos.tipo
   	  	 WHEN 0 LET lr_formato_2005.trabs_imss   = lr_formato_2005.trabs_imss   + lr_cuantos.trabs
   	  	 WHEN 1 LET lr_formato_2005.trabs_issste = lr_formato_2005.trabs_issste + lr_cuantos.trabs
   	  	 WHEN 2 LET lr_formato_2005.trabs_indepe = lr_formato_2005.trabs_indepe + lr_cuantos.trabs
   	  	 WHEN 3 LET lr_formato_2005.trabs_mixtos = lr_formato_2005.trabs_mixtos + lr_cuantos.trabs
   	  END CASE
   END FOREACH

   RETURN lr_formato_2005.*
END FUNCTION
################################################################################