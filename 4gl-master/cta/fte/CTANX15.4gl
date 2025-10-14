################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX15    => FORMATO 2004                                           #
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
  	     tipo_registro   SMALLINT     ,
  	     tipo_entidad    SMALLINT     ,
  	     entidad         SMALLINT     ,
  	     periodicidad    SMALLINT     ,
  	     tipo_proceso    SMALLINT     ,
  	     fecha_corte     DATE         ,
  	     total_trabs     DECIMAL(10,0),
  	     total_trabs_ant DECIMAL(10,0)
  END RECORD

  DEFINE gr_det1 RECORD
  	 tipo_registro   SMALLINT     ,
  	 sector          SMALLINT     ,
  	 edad            SMALLINT     ,
  	 sexo            SMALLINT     ,
  	 estado          SMALLINT     ,
  	 total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det2 RECORD
  	  tipo_registro   SMALLINT     ,
  	  total_trabs     DECIMAL(10,0),
  	  total_edos      SMALLINT     ,

  	  trabs_imss      DECIMAL(10,0),
  	  trabs_issste    DECIMAL(10,0),
  	  trabs_indepe    DECIMAL(10,0),
  	  trabs_mixtos    DECIMAL(10,0),

  	  hasta_27        DECIMAL(10,0),
  	  hasta_36        DECIMAL(10,0),
  	  hasta_45        DECIMAL(10,0),
  	  hasta_55        DECIMAL(10,0),
  	  hasta_99        DECIMAL(10,0),
  	  sin_edad        DECIMAL(10,0),

  	  total_mujeres   DECIMAL(10,0),
  	  total_hombres   DECIMAL(10,0),
  	  sin_sexo        DECIMAL(10,0)
  END RECORD

  DEFINE gr_det3 RECORD
  	  tipo_registro       SMALLINT     ,
  	  dif_total_trabs     DECIMAL(10,0),
  	  dif_total_edos      SMALLINT     ,

  	  dif_trabs_imss      DECIMAL(10,0),
  	  dif_trabs_issste    DECIMAL(10,0),
  	  dif_trabs_indepe    DECIMAL(10,0),
  	  dif_trabs_mixtos    DECIMAL(10,0),

  	  dif_hasta_27        DECIMAL(10,0),
  	  dif_hasta_36        DECIMAL(10,0),
  	  dif_hasta_45        DECIMAL(10,0),
  	  dif_hasta_55        DECIMAL(10,0),
  	  dif_hasta_99        DECIMAL(10,0),
  	  dif_sin_edad        DECIMAL(10,0),

  	  dif_total_mujeres   DECIMAL(10,0),
  	  dif_total_hombres   DECIMAL(10,0),
  	  dif_sin_sexo        DECIMAL(10,0)
  END RECORD
END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE FORMATO 2004",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX15.log")

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
   FROM  cta_his_fmto2004_det1
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

   #/ruta/formato2004_enc_DDMMYYYYY
   #/ruta/formato2004_sub_DDMMYYYYY
   #/ruta/formato2004_det1_DDMMYYYYY
   #/ruta/formato2004_det2_DDMMYYYYY
   #/ruta/formato2004_det3_DDMMYYYYY
   #/ruta/FORMATO_2004_DDMMYYYYY_xxx

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

   LET lc_encabezado     = "formato2004_enc_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_sub_encabezado = "formato2004_sub_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle1       = "formato2004_det1_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle2       = "formato2004_det2_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle3       = "formato2004_det3_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_archivo_final  = "FORMATO_2004_"    ,gd_fecha_corte USING "DDMMYYYY",
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
   LET gr_encabezado.tipo_archivo  = 2004
   LET gr_encabezado.tipo_entidad  = 1
   LET gr_encabezado.entidad       = gi_afore
   CALL habil_siguiente(gd_fecha_corte, 7) RETURNING gr_encabezado.fecha_envio
   LET gr_encabezado.longitud      = 131

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
            COLUMN 30, 102 SPACES
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

   #Se llenan en el detalle3
   --LET gr_subenc.total_trabs     = 0
   --LET gr_subenc.total_trabs_ant = 0

   START REPORT rpt_sub_enc TO lc_sub_enc
      OUTPUT TO REPORT rpt_sub_enc(gr_subenc.*)
   FINISH REPORT rpt_sub_enc
END FUNCTION
################################################################################
REPORT rpt_sub_enc(lr_subenc)
   DEFINE lr_subenc RECORD
   	     tipo_registro   SMALLINT     ,
  	     tipo_entidad    SMALLINT     ,
  	     entidad         SMALLINT     ,
  	     periodicidad    SMALLINT     ,
  	     tipo_proceso    SMALLINT     ,
  	     fecha_corte     DATE         ,
  	     total_trabs     DECIMAL(10,0),
  	     total_trabs_ant DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, lr_subenc.tipo_registro   USING "&&&"      ,
            COLUMN 04, lr_subenc.tipo_entidad    USING "&&&"      ,
            COLUMN 07, lr_subenc.entidad         USING "&&&"      ,
            COLUMN 10, lr_subenc.periodicidad    USING "&"        ,
            COLUMN 11, lr_subenc.tipo_proceso    USING "&&&"      ,
            COLUMN 14, lr_subenc.fecha_corte     USING "YYYYMMDD" ,
            COLUMN 22, lr_subenc.total_trabs     USING "&&&&&&&&&",
            COLUMN 31, lr_subenc.total_trabs_ant USING "&&&&&&&&&",
            COLUMN 40, 92 SPACES
      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle1(lc_detalle1)
   DEFINE lc_detalle1    CHAR(200)

   LET gr_det1.tipo_registro = 301

   #Falta
   {LET gr_det1.sector        = 0
   LET gr_det1.edad          = 0
   LET gr_det1.sexo          = 0
   LET gr_det1.estado        = 0
   LET gr_det1.total_trabs   = 0}

   DECLARE cur_det1 CURSOR FOR
   SELECT sector_trab,
          rango_edad ,
          sexo       ,
          entidad_fed,
          COUNT(*)
   FROM   tmp_oficio_cta_adm
   GROUP BY 1,2,3,4
   ORDER BY 1,2,3,4

   START REPORT rpt_detalle1 TO lc_detalle1
      FOREACH cur_det1 INTO gr_det1.sector     ,
                            gr_det1.edad       ,
                            gr_det1.sexo       ,
                            gr_det1.estado     ,
                            gr_det1.total_trabs
         OUTPUT TO REPORT rpt_detalle1(gr_det1.*)
         CALL inserta_historico()
      END FOREACH
   FINISH REPORT rpt_detalle1
END FUNCTION
################################################################################
REPORT rpt_detalle1(lr_det1)
   DEFINE lr_det1 RECORD
   	     tipo_registro   SMALLINT     ,
         sector          SMALLINT     ,
         edad            SMALLINT     ,
         sexo            SMALLINT     ,
         estado          SMALLINT     ,
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
            COLUMN 05, lr_det1.edad          USING "&"        ,
            COLUMN 06, lr_det1.sexo          USING "&"        ,
            COLUMN 07, lr_det1.estado        USING "&&"       ,
            COLUMN 09, lr_det1.total_trabs   USING "&&&&&&&&&",
            COLUMN 18, 114 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle2(lc_detalle2)
   DEFINE lc_detalle2    CHAR(200)
   DEFINE lr_his_formato2004 RECORD
             fecha_corte     DATE         ,
             total_trabs     DECIMAL(10,0),
             total_edos      SMALLINT     ,
             trabs_imss      DECIMAL(10,0),
             trabs_issste    DECIMAL(10,0),
             trabs_indepe    DECIMAL(10,0),
             trabs_mixtos    DECIMAL(10,0),
             hasta_27        DECIMAL(10,0),
             hasta_36        DECIMAL(10,0),
             hasta_45        DECIMAL(10,0),
             hasta_55        DECIMAL(10,0),
             hasta_99        DECIMAL(10,0),
             sin_edad        DECIMAL(10,0),
             total_mujeres   DECIMAL(10,0),
             total_hombres   DECIMAL(10,0),
             sin_sexo        DECIMAL(10,0)
   END RECORD

   LET gr_det2.tipo_registro = 302

   #Faltan
   LET gr_det2.total_trabs   = 0
   LET gr_det2.total_edos    = 0

   LET gr_det2.trabs_imss    = 0
   LET gr_det2.trabs_issste  = 0
   LET gr_det2.trabs_indepe  = 0
   LET gr_det2.trabs_mixtos  = 0

   LET gr_det2.hasta_27      = 0
   LET gr_det2.hasta_36      = 0
   LET gr_det2.hasta_45      = 0
   LET gr_det2.hasta_55      = 0
   LET gr_det2.hasta_99      = 0
   LET gr_det2.sin_edad      = 0

   LET gr_det2.total_mujeres = 0
   LET gr_det2.total_hombres = 0
   LET gr_det2.sin_sexo = 0

   #Todo se obtiene del detalle 1
   CALL get_historico(gd_fecha_corte) RETURNING lr_his_formato2004.*

   LET gr_det2.total_trabs   = lr_his_formato2004.total_trabs
   LET gr_det2.total_edos    = lr_his_formato2004.total_edos

   LET gr_det2.trabs_imss    = lr_his_formato2004.trabs_imss
   LET gr_det2.trabs_issste  = lr_his_formato2004.trabs_issste
   LET gr_det2.trabs_indepe  = lr_his_formato2004.trabs_indepe
   LET gr_det2.trabs_mixtos  = lr_his_formato2004.trabs_mixtos

   LET gr_det2.hasta_27      = lr_his_formato2004.hasta_27
   LET gr_det2.hasta_36      = lr_his_formato2004.hasta_36
   LET gr_det2.hasta_45      = lr_his_formato2004.hasta_45
   LET gr_det2.hasta_55      = lr_his_formato2004.hasta_55
   LET gr_det2.hasta_99      = lr_his_formato2004.hasta_99
   LET gr_det2.sin_edad      = lr_his_formato2004.sin_edad

   LET gr_det2.total_mujeres = lr_his_formato2004.total_mujeres
   LET gr_det2.total_hombres = lr_his_formato2004.total_hombres
   LET gr_det2.sin_sexo      = lr_his_formato2004.sin_sexo

   START REPORT rpt_detalle2 TO lc_detalle2
      OUTPUT TO REPORT rpt_detalle2(gr_det2.*)
   FINISH REPORT rpt_detalle2

END FUNCTION
################################################################################
REPORT rpt_detalle2(lr_det2)
   DEFINE lr_det2 RECORD
   	     tipo_registro   SMALLINT     ,
  	     total_trabs     DECIMAL(10,0),
  	     total_edos      SMALLINT     ,

  	     trabs_imss      DECIMAL(10,0),
  	     trabs_issste    DECIMAL(10,0),
  	     trabs_indepe    DECIMAL(10,0),
  	     trabs_mixtos    DECIMAL(10,0),

  	     hasta_27        DECIMAL(10,0),
  	     hasta_36        DECIMAL(10,0),
  	     hasta_45        DECIMAL(10,0),
  	     hasta_55        DECIMAL(10,0),
  	     hasta_99        DECIMAL(10,0),
  	     sin_edad        DECIMAL(10,0),

  	     total_mujeres   DECIMAL(10,0),
  	     total_hombres   DECIMAL(10,0),
  	     sin_sexo        DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, lr_det2.tipo_registro USING "&&&"      ,
            COLUMN 004, lr_det2.total_trabs   USING "&&&&&&&&&",
            COLUMN 013, lr_det2.total_edos    USING "&&"       ,
            COLUMN 015, lr_det2.trabs_imss    USING "&&&&&&&&&",
            COLUMN 024, lr_det2.trabs_issste  USING "&&&&&&&&&",
            COLUMN 033, lr_det2.trabs_indepe  USING "&&&&&&&&&",
            COLUMN 042, lr_det2.trabs_mixtos  USING "&&&&&&&&&",
            COLUMN 051, lr_det2.hasta_27      USING "&&&&&&&&&",
            COLUMN 060, lr_det2.hasta_36      USING "&&&&&&&&&",
            COLUMN 069, lr_det2.hasta_45      USING "&&&&&&&&&",
            COLUMN 078, lr_det2.hasta_55      USING "&&&&&&&&&",
            COLUMN 087, lr_det2.hasta_99      USING "&&&&&&&&&",
            COLUMN 096, lr_det2.sin_edad      USING "&&&&&&&&&",
            COLUMN 105, lr_det2.total_mujeres USING "&&&&&&&&&",
            COLUMN 114, lr_det2.total_hombres USING "&&&&&&&&&",
            COLUMN 123, lr_det2.sin_sexo      USING "&&&&&&&&&"

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle3(lc_detalle3)
   DEFINE lc_detalle3    CHAR(200)

   DEFINE lr_trimestre_ant,
          lr_trimestre_act RECORD
             fecha_corte     DATE         ,
             total_trabs     DECIMAL(10,0),
             total_edos      SMALLINT     ,
             trabs_imss      DECIMAL(10,0),
             trabs_issste    DECIMAL(10,0),
             trabs_indepe    DECIMAL(10,0),
             trabs_mixtos    DECIMAL(10,0),
             hasta_27        DECIMAL(10,0),
             hasta_36        DECIMAL(10,0),
             hasta_45        DECIMAL(10,0),
             hasta_55        DECIMAL(10,0),
             hasta_99        DECIMAL(10,0),
             sin_edad        DECIMAL(10,0),
             total_mujeres   DECIMAL(10,0),
             total_hombres   DECIMAL(10,0),
             sin_sexo        DECIMAL(10,0)
   END RECORD

   #Obtenemos la fecha de corte del periodo anterior
   CALL trimestre_anterior(gd_fecha_corte) RETURNING lr_trimestre_ant.fecha_corte
   LET lr_trimestre_act.fecha_corte = gd_fecha_corte

   IF gd_fecha_corte = "03/31/2010" AND
   	  gi_afore <> 578 THEN          --Pst
   	  LET lr_trimestre_ant.fecha_corte = "03/31/2010"
   END IF

   CALL get_historico(lr_trimestre_ant.fecha_corte) RETURNING lr_trimestre_ant.*
   CALL get_historico(lr_trimestre_act.fecha_corte) RETURNING lr_trimestre_act.*

   #Buscar la historia de marzo con las tablas anteriores
   IF lr_trimestre_ant.fecha_corte = "03/31/2010" AND
   	  lr_trimestre_act.fecha_corte = "06/30/2010" AND
   	  gi_afore <> 578 THEN          --Pst
   	  SELECT *
      INTO   lr_trimestre_ant.fecha_corte  ,
             lr_trimestre_ant.total_trabs  ,
             lr_trimestre_ant.total_edos   ,
             lr_trimestre_ant.trabs_imss   ,
             lr_trimestre_ant.trabs_issste ,
             lr_trimestre_ant.trabs_indepe ,
             lr_trimestre_ant.trabs_mixtos ,
             lr_trimestre_ant.hasta_27     ,
             lr_trimestre_ant.hasta_36     ,
             lr_trimestre_ant.hasta_45     ,
             lr_trimestre_ant.hasta_55     ,
             lr_trimestre_ant.hasta_99     ,
             lr_trimestre_ant.total_mujeres,
             lr_trimestre_ant.total_hombres
      FROM   cta_hist_formato2004
      WHERE  fecha_corte = lr_trimestre_ant.fecha_corte

      LET lr_trimestre_ant.sin_edad = 0
      LET lr_trimestre_ant.sin_sexo = 0
   END IF

   LET gr_det3.tipo_registro     = 303

   LET gr_det3.dif_total_trabs   = lr_trimestre_ant.total_trabs   - lr_trimestre_act.total_trabs
   LET gr_det3.dif_total_edos    = lr_trimestre_ant.total_edos    - lr_trimestre_act.total_edos
   LET gr_det3.dif_trabs_imss    = lr_trimestre_ant.trabs_imss    - lr_trimestre_act.trabs_imss
   LET gr_det3.dif_trabs_issste  = lr_trimestre_ant.trabs_issste  - lr_trimestre_act.trabs_issste
   LET gr_det3.dif_trabs_indepe  = lr_trimestre_ant.trabs_indepe  - lr_trimestre_act.trabs_indepe
   LET gr_det3.dif_trabs_mixtos  = lr_trimestre_ant.trabs_mixtos  - lr_trimestre_act.trabs_mixtos
   LET gr_det3.dif_hasta_27      = lr_trimestre_ant.hasta_27      - lr_trimestre_act.hasta_27
   LET gr_det3.dif_hasta_36      = lr_trimestre_ant.hasta_36      - lr_trimestre_act.hasta_36
   LET gr_det3.dif_hasta_45      = lr_trimestre_ant.hasta_45      - lr_trimestre_act.hasta_45
   LET gr_det3.dif_hasta_55      = lr_trimestre_ant.hasta_55      - lr_trimestre_act.hasta_55
   LET gr_det3.dif_hasta_99      = lr_trimestre_ant.hasta_99      - lr_trimestre_act.hasta_99
   LET gr_det3.dif_sin_edad      = lr_trimestre_ant.sin_edad      - lr_trimestre_act.sin_edad
   LET gr_det3.dif_total_mujeres = lr_trimestre_ant.total_mujeres - lr_trimestre_act.total_mujeres
   LET gr_det3.dif_total_hombres = lr_trimestre_ant.total_hombres - lr_trimestre_act.total_hombres
   LET gr_det3.dif_sin_sexo      = lr_trimestre_ant.sin_sexo      - lr_trimestre_act.sin_sexo

   #Subencabezado
   LET gr_subenc.total_trabs     = lr_trimestre_act.total_trabs

   IF gd_fecha_corte = "31/03/2010" THEN
   	  LET gr_subenc.total_trabs_ant = lr_trimestre_act.total_trabs
   ELSE
      LET gr_subenc.total_trabs_ant = lr_trimestre_ant.total_trabs
   END IF

   START REPORT rpt_detalle3 TO lc_detalle3
      OUTPUT TO REPORT rpt_detalle3(gr_det3.*)
   FINISH REPORT rpt_detalle3
END FUNCTION
################################################################################
REPORT rpt_detalle3(lr_det3)
   DEFINE lr_det3 RECORD
   	      tipo_registro       SMALLINT     ,
  	      dif_total_trabs     DECIMAL(10,0),
  	      dif_total_edos      SMALLINT     ,

  	      dif_trabs_imss      DECIMAL(10,0),
  	      dif_trabs_issste    DECIMAL(10,0),
  	      dif_trabs_indepe    DECIMAL(10,0),
  	      dif_trabs_mixtos    DECIMAL(10,0),

  	      dif_hasta_27        DECIMAL(10,0),
  	      dif_hasta_36        DECIMAL(10,0),
  	      dif_hasta_45        DECIMAL(10,0),
  	      dif_hasta_55        DECIMAL(10,0),
  	      dif_hasta_99        DECIMAL(10,0),
  	      dif_sin_edad        DECIMAL(10,0),

  	      dif_total_mujeres   DECIMAL(10,0),
  	      dif_total_hombres   DECIMAL(10,0),
  	      dif_sin_sexo        DECIMAL(10,0)
	 END RECORD

	 DEFINE lc_dif_total_trabs     CHAR(09),
          lc_dif_total_edos      CHAR(02),
          lc_dif_trabs_imss      CHAR(09),
          lc_dif_trabs_issste    CHAR(09),
          lc_dif_trabs_indepe    CHAR(09),
          lc_dif_trabs_mixtos    CHAR(09),
          lc_dif_hasta_27        CHAR(09),
          lc_dif_hasta_36        CHAR(09),
          lc_dif_hasta_45        CHAR(09),
          lc_dif_hasta_55        CHAR(09),
          lc_dif_hasta_99        CHAR(09),
          lc_dif_sin_edad        CHAR(09),
          lc_dif_total_mujeres   CHAR(09),
          lc_dif_total_hombres   CHAR(09),
          lc_dif_sin_sexo        CHAR(09)

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      IF lr_det3.dif_total_trabs   < 0 THEN LET lc_dif_total_trabs   = lr_det3.dif_total_trabs   USING "-&&&&&&&&" ELSE LET lc_dif_total_trabs   = lr_det3.dif_total_trabs   USING "&&&&&&&&&" END IF
      IF lr_det3.dif_total_edos    < 0 THEN LET lc_dif_total_edos    = lr_det3.dif_total_edos    USING "-&"        ELSE LET lc_dif_total_edos    = lr_det3.dif_total_edos    USING "&&"        END IF
      IF lr_det3.dif_trabs_imss    < 0 THEN LET lc_dif_trabs_imss    = lr_det3.dif_trabs_imss    USING "-&&&&&&&&" ELSE LET lc_dif_trabs_imss    = lr_det3.dif_trabs_imss    USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_issste  < 0 THEN LET lc_dif_trabs_issste  = lr_det3.dif_trabs_issste  USING "-&&&&&&&&" ELSE LET lc_dif_trabs_issste  = lr_det3.dif_trabs_issste  USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_indepe  < 0 THEN LET lc_dif_trabs_indepe  = lr_det3.dif_trabs_indepe  USING "-&&&&&&&&" ELSE LET lc_dif_trabs_indepe  = lr_det3.dif_trabs_indepe  USING "&&&&&&&&&" END IF
      IF lr_det3.dif_trabs_mixtos  < 0 THEN LET lc_dif_trabs_mixtos  = lr_det3.dif_trabs_mixtos  USING "-&&&&&&&&" ELSE LET lc_dif_trabs_mixtos  = lr_det3.dif_trabs_mixtos  USING "&&&&&&&&&" END IF
      IF lr_det3.dif_hasta_27      < 0 THEN LET lc_dif_hasta_27      = lr_det3.dif_hasta_27      USING "-&&&&&&&&" ELSE LET lc_dif_hasta_27      = lr_det3.dif_hasta_27      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_hasta_36      < 0 THEN LET lc_dif_hasta_36      = lr_det3.dif_hasta_36      USING "-&&&&&&&&" ELSE LET lc_dif_hasta_36      = lr_det3.dif_hasta_36      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_hasta_45      < 0 THEN LET lc_dif_hasta_45      = lr_det3.dif_hasta_45      USING "-&&&&&&&&" ELSE LET lc_dif_hasta_45      = lr_det3.dif_hasta_45      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_hasta_55      < 0 THEN LET lc_dif_hasta_55      = lr_det3.dif_hasta_55      USING "-&&&&&&&&" ELSE LET lc_dif_hasta_55      = lr_det3.dif_hasta_55      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_hasta_99      < 0 THEN LET lc_dif_hasta_99      = lr_det3.dif_hasta_99      USING "-&&&&&&&&" ELSE LET lc_dif_hasta_99      = lr_det3.dif_hasta_99      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_sin_edad      < 0 THEN LET lc_dif_sin_edad      = lr_det3.dif_sin_edad      USING "-&&&&&&&&" ELSE LET lc_dif_sin_edad      = lr_det3.dif_sin_edad      USING "&&&&&&&&&" END IF
      IF lr_det3.dif_total_mujeres < 0 THEN LET lc_dif_total_mujeres = lr_det3.dif_total_mujeres USING "-&&&&&&&&" ELSE LET lc_dif_total_mujeres = lr_det3.dif_total_mujeres USING "&&&&&&&&&" END IF
      IF lr_det3.dif_total_hombres < 0 THEN LET lc_dif_total_hombres = lr_det3.dif_total_hombres USING "-&&&&&&&&" ELSE LET lc_dif_total_hombres = lr_det3.dif_total_hombres USING "&&&&&&&&&" END IF
      IF lr_det3.dif_sin_sexo      < 0 THEN LET lc_dif_sin_sexo      = lr_det3.dif_sin_sexo      USING "-&&&&&&&&" ELSE LET lc_dif_sin_sexo      = lr_det3.dif_sin_sexo      USING "&&&&&&&&&" END IF

      PRINT COLUMN 001, lr_det3.tipo_registro     USING "&&&"       ,
            COLUMN 004, lc_dif_total_trabs                          ,
            COLUMN 013, lc_dif_total_edos                           ,
            COLUMN 015, lc_dif_trabs_imss                           ,
            COLUMN 024, lc_dif_trabs_issste                         ,
            COLUMN 033, lc_dif_trabs_indepe                         ,
            COLUMN 042, lc_dif_trabs_mixtos                         ,
            COLUMN 051, lc_dif_hasta_27                             ,
            COLUMN 060, lc_dif_hasta_36                             ,
            COLUMN 069, lc_dif_hasta_45                             ,
            COLUMN 078, lc_dif_hasta_55                             ,
            COLUMN 087, lc_dif_hasta_99                             ,
            COLUMN 096, lc_dif_sin_edad                             ,
            COLUMN 105, lc_dif_total_mujeres                        ,
            COLUMN 114, lc_dif_total_hombres                        ,
            COLUMN 123, lc_dif_sin_sexo
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
       "CTANX15",         -- proceso_cod
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
                 " AND    proceso_cod = 'CTANX15' ",
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
                 " AND    proceso_cod   = 'CTANX15'",
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
FUNCTION inserta_historico()
   INSERT INTO cta_his_fmto2004_det1 VALUES(
      gd_fecha_corte     ,   --fecha_corte
      gr_det1.sector     ,   --sector
      gr_det1.edad       ,   --edad
      gr_det1.sexo       ,   --sexo
      gr_det1.estado     ,   --estado
      gr_det1.total_trabs,   --total_trabs
      hoy                ,   --factualiza
      gc_usuario             --usuario
      )
END FUNCTION
################################################################################
FUNCTION get_historico(ld_fecha_corte)
   DEFINE ld_fecha_corte DATE

   DEFINE lr_his_formato2004 RECORD
             fecha_corte     DATE         ,
             total_trabs     DECIMAL(10,0),
             total_edos      SMALLINT     ,
             trabs_imss      DECIMAL(10,0),
             trabs_issste    DECIMAL(10,0),
             trabs_indepe    DECIMAL(10,0),
             trabs_mixtos    DECIMAL(10,0),
             hasta_27        DECIMAL(10,0),
             hasta_36        DECIMAL(10,0),
             hasta_45        DECIMAL(10,0),
             hasta_55        DECIMAL(10,0),
             hasta_99        DECIMAL(10,0),
             sin_edad        DECIMAL(10,0),
             total_mujeres   DECIMAL(10,0),
             total_hombres   DECIMAL(10,0),
             sin_sexo        DECIMAL(10,0)
   END RECORD

   DEFINE lr_tipo_trab RECORD
   	      tipo         SMALLINT,
   	      total_trabs DECIMAL(10,0)
   END RECORD

   LET lr_his_formato2004.fecha_corte = ld_fecha_corte

   LET lr_his_formato2004.total_trabs   = 0
   LET lr_his_formato2004.total_edos    = 0
   LET lr_his_formato2004.trabs_imss    = 0
   LET lr_his_formato2004.trabs_issste  = 0
   LET lr_his_formato2004.trabs_indepe  = 0
   LET lr_his_formato2004.trabs_mixtos  = 0
   LET lr_his_formato2004.hasta_27      = 0
   LET lr_his_formato2004.hasta_36      = 0
   LET lr_his_formato2004.hasta_45      = 0
   LET lr_his_formato2004.hasta_55      = 0
   LET lr_his_formato2004.hasta_99      = 0
   LET lr_his_formato2004.sin_edad      = 0
   LET lr_his_formato2004.total_mujeres = 0
   LET lr_his_formato2004.total_hombres = 0
   LET lr_his_formato2004.sin_sexo      = 0

   #Total de estados
   SELECT COUNT(UNIQUE estado)
   INTO   lr_his_formato2004.total_edos
   FROM   cta_his_fmto2004_det1
   WHERE  fecha_corte = ld_fecha_corte

   #Totales por sector (IMSS, ISSSTE, INDEPE, MIXTOS)
   DECLARE cur_tipo_trab_his CURSOR FOR
   SELECT sector,
          SUM(total_trabs)
   FROM   cta_his_fmto2004_det1
   WHERE  fecha_corte = ld_fecha_corte
   GROUP BY 1
   ORDER BY 1

   FOREACH cur_tipo_trab_his INTO lr_tipo_trab.tipo,
   	                              lr_tipo_trab.total_trabs
   	  CASE lr_tipo_trab.tipo
   	  	 WHEN 0 LET lr_his_formato2004.trabs_imss    = lr_tipo_trab.total_trabs
   	  	 WHEN 1 LET lr_his_formato2004.trabs_issste  = lr_tipo_trab.total_trabs
   	  	 WHEN 2 LET lr_his_formato2004.trabs_indepe  = lr_tipo_trab.total_trabs
   	  	 WHEN 3 LET lr_his_formato2004.trabs_mixtos  = lr_tipo_trab.total_trabs
   	  END CASE
   	  INITIALIZE lr_tipo_trab.* TO NULL
   END FOREACH

   #Total de trabajadores
   LET lr_his_formato2004.total_trabs = lr_his_formato2004.trabs_imss   +
                                        lr_his_formato2004.trabs_issste +
                                        lr_his_formato2004.trabs_indepe +
                                        lr_his_formato2004.trabs_mixtos

   #Totales por rango edad
   DECLARE cur_rango_edad_his CURSOR FOR
   SELECT edad,
          SUM(total_trabs)
   FROM   cta_his_fmto2004_det1
   WHERE  fecha_corte = ld_fecha_corte
   GROUP BY 1
   ORDER BY 1

   FOREACH cur_rango_edad_his INTO lr_tipo_trab.tipo,
   	                               lr_tipo_trab.total_trabs
   	  CASE lr_tipo_trab.tipo
   	  	 WHEN 1 LET lr_his_formato2004.hasta_27 = lr_tipo_trab.total_trabs
   	  	 WHEN 2 LET lr_his_formato2004.hasta_36 = lr_tipo_trab.total_trabs
   	  	 WHEN 3 LET lr_his_formato2004.hasta_45 = lr_tipo_trab.total_trabs
   	  	 WHEN 4 LET lr_his_formato2004.hasta_55 = lr_tipo_trab.total_trabs
   	  	 WHEN 5 LET lr_his_formato2004.hasta_99 = lr_tipo_trab.total_trabs
   	  	 WHEN 6 LET lr_his_formato2004.sin_edad = lr_tipo_trab.total_trabs
   	  END CASE
   	  INITIALIZE lr_tipo_trab.* TO NULL
   END FOREACH

   #Totales por sexo
   DECLARE cur_sexo_his CURSOR FOR
   SELECT sexo   ,
          SUM(total_trabs)
   FROM   cta_his_fmto2004_det1
   WHERE  fecha_corte = ld_fecha_corte
   GROUP BY 1
   ORDER BY 1

   FOREACH cur_sexo_his INTO lr_tipo_trab.tipo,
   	                         lr_tipo_trab.total_trabs
   	  CASE lr_tipo_trab.tipo
   	  	 WHEN 1 LET lr_his_formato2004.total_hombres = lr_tipo_trab.total_trabs
   	  	 WHEN 2 LET lr_his_formato2004.total_mujeres = lr_tipo_trab.total_trabs
   	  	 WHEN 3 LET lr_his_formato2004.sin_sexo      = lr_tipo_trab.total_trabs
   	  END CASE
   	  INITIALIZE lr_tipo_trab.* TO NULL
   END FOREACH

   RETURN lr_his_formato2004.*
END FUNCTION
################################################################################