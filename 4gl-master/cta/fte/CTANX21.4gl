################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX21    => FORMATO 2009                                           #
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
  	 rango_salario   SMALLINT     ,
  	 total_trabs     DECIMAL(10,0)
  END RECORD

  DEFINE gr_det2 RECORD
  	  tipo_registro       SMALLINT     ,
  	  total_imss          DECIMAL(10,0),
  	  total_issste        DECIMAL(10,0),
  	  total_independiente DECIMAL(10,0),
  	  total_mixto         DECIMAL(10,0)
  END RECORD

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE FORMATO 2009",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX21.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación formato 2009")

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

   CALL limpia_historico()
   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
#####################################################################
FUNCTION limpia_historico()
   DELETE
   FROM  cta_his_fmto2009_det1
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
                                   lc_archivo_final

   LET lc_encabezado     = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado     CLIPPED
   LET lc_sub_encabezado = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_sub_encabezado CLIPPED
   LET lc_detalle1       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle1       CLIPPED
   LET lc_detalle2       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle2       CLIPPED
   LET lc_archivo_final  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final  CLIPPED

   LET lc_comando_cat = lc_comando_cat    CLIPPED, " ",
                        lc_encabezado     CLIPPED, " ",
                        lc_sub_encabezado CLIPPED, " ",
                        lc_detalle1       CLIPPED, " ",
                        lc_detalle2       CLIPPED, " > ",
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
                          lc_archivo_final  CLIPPED, ";"

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   --EXECUTE prep_prioridadh

   #/ruta/formato2009_enc_DDMMYYYYY
   #/ruta/formato2009_sub_DDMMYYYYY
   #/ruta/formato2009_det1_DDMMYYYYY
   #/ruta/formato2009_det2_DDMMYYYYY
   #/ruta/FORMATO_2009_DDMMYYYYY_xxx

   CALL detalle1(lc_detalle1)
   CALL detalle2(lc_detalle2)
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
          lc_archivo_final CHAR(80)

   LET lc_encabezado     = "formato2009_enc_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_sub_encabezado = "formato2009_sub_" , gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle1       = "formato2009_det1_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle2       = "formato2009_det2_", gd_fecha_corte USING "DDMMYYYY"
   LET lc_archivo_final  = "FORMATO_2009_"    , gd_fecha_corte USING "DDMMYYYY",
                            "_", gi_afore USING "&&&"

   RETURN lc_encabezado    ,
          lc_sub_encabezado,
          lc_detalle1      ,
          lc_detalle2      ,
          lc_archivo_final
END FUNCTION
################################################################################
FUNCTION encabezado(lc_encabezado)
   DEFINE lc_encabezado    CHAR(200)

   LET gr_encabezado.tipo_registro = 0
   LET gr_encabezado.tipo_archivo  = 2009
   LET gr_encabezado.tipo_entidad  = 1
   LET gr_encabezado.entidad       = gi_afore
   CALL habil_siguiente(gd_fecha_corte, 7) RETURNING gr_encabezado.fecha_envio
   LET gr_encabezado.longitud      = 30

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
            COLUMN 30, 1 SPACES
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
            COLUMN 022, 9 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle1(lc_detalle1)
   DEFINE lc_detalle1    CHAR(200)
   DEFINE ls_detalle1    SMALLINT

   LET gr_det1.tipo_registro  = 301
   LET ls_detalle1            = 1

   #Falta
   LET gr_det1.sector        = 0
   LET gr_det1.clasificacion = 0
   LET gr_det1.rango_salario = 0
   LET gr_det1.total_trabs   = 0

   START REPORT rpt_detalle1 TO lc_detalle1

   DECLARE cur_det1 CURSOR FOR
   SELECT sector_trab  ,
          id_trabajador,
          ind_sal_min  ,
          COUNT(*)
   FROM   tmp_oficio_cta_adm
   GROUP BY 1,2,3
   ORDER BY 1,2,3

   FOREACH cur_det1 INTO gr_det1.sector       ,
                         gr_det1.clasificacion,
                         gr_det1.rango_salario,
                         gr_det1.total_trabs
                         
      #Los independientes no se reportan
      IF gr_det1.sector <> 2 THEN
         OUTPUT TO REPORT rpt_detalle1(gr_det1.*)         
      END IF
      
      #Aunque los independientes no se reportan, se guardan datos en historico
      CALL inserta_historico()
   END FOREACH

   FINISH REPORT rpt_detalle1
END FUNCTION
################################################################################
REPORT rpt_detalle1(lr_det1)
   DEFINE lr_det1 RECORD
   	      tipo_registro   SMALLINT     ,
  	      sector          SMALLINT     ,
  	      clasificacion   SMALLINT     ,
  	      rango_salario   SMALLINT     ,
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
            COLUMN 06, lr_det1.rango_salario USING "&&"       ,
            COLUMN 08, lr_det1.total_trabs   USING "&&&&&&&&&",
            COLUMN 17, 14 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION detalle2(lc_detalle2)
   DEFINE lc_detalle2    CHAR(200)
   DEFINE ls_detalle2    SMALLINT

   LET gr_det2.tipo_registro  = 302
   LET ls_detalle2            = 2

   #Falta
   LET gr_det2.total_imss          = 0
   LET gr_det2.total_issste        = 0
   LET gr_det2.total_independiente = 0
   LET gr_det2.total_mixto         = 0


   START REPORT rpt_detalle2 TO lc_detalle2
      CALL get_historico(gd_fecha_corte)
           RETURNING gr_det2.total_imss         ,
                     gr_det2.total_issste       ,
                     gr_det2.total_independiente,
                     gr_det2.total_mixto

      OUTPUT TO REPORT rpt_detalle2(gr_det2.*)
   FINISH REPORT rpt_detalle2
END FUNCTION
################################################################################
REPORT rpt_detalle2(lr_det2)
   DEFINE lr_det2 RECORD
   	     tipo_registro       SMALLINT     ,
  	     total_imss          DECIMAL(10,0),
  	     total_issste        DECIMAL(10,0),
  	     total_independiente DECIMAL(10,0),
  	     total_mixto         DECIMAL(10,0)
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, lr_det2.tipo_registro USING "&&&"      ,
            COLUMN 04, lr_det2.total_issste  USING "&&&&&&&&&",
            COLUMN 13, lr_det2.total_imss    USING "&&&&&&&&&",
            COLUMN 22, lr_det2.total_mixto   USING "&&&&&&&&&"

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
       "CTANX21",         -- proceso_cod
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
                 " AND    proceso_cod = 'CTANX21' ",
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
                 " AND    proceso_cod   = 'CTANX21'",
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
FUNCTION inserta_historico()
   INSERT INTO cta_his_fmto2009_det1 VALUES(
      gd_fecha_corte       ,  --fecha_corte
      gr_det1.sector       ,  --sector
      gr_det1.clasificacion,  --clasificacion
      gr_det1.rango_salario,  --rango_salario
      gr_det1.total_trabs  ,  --total_trabs
      hoy                  ,  --factualiza
      gc_usuario              --usuario
   )
END FUNCTION
################################################################################
FUNCTION get_historico(ld_fecha_corte)
   DEFINE ld_fecha_corte DATE

   DEFINE lr_formato_2009 RECORD
  	  total_imss          DECIMAL(10,0),
  	  total_issste        DECIMAL(10,0),
  	  total_independiente DECIMAL(10,0),
  	  total_mixto         DECIMAL(10,0)
   END RECORD

   DEFINE ls_sector       SMALLINT,
          ld_total_trabs  DECIMAL(10,0)

   LET lr_formato_2009.total_imss          = 0
   LET lr_formato_2009.total_issste        = 0
   LET lr_formato_2009.total_independiente = 0
   LET lr_formato_2009.total_mixto         = 0

   DECLARE cur_historico CURSOR FOR
   SELECT sector, SUM(total_trabs)
   FROM   cta_his_fmto2009_det1
   WHERE  fecha_corte = ld_fecha_corte
   GROUP BY 1
   ORDER BY 1

   FOREACH cur_historico INTO ls_sector     ,
                              ld_total_trabs
      CASE ls_sector
      	 WHEN 0 LET lr_formato_2009.total_imss          = ld_total_trabs
      	 WHEN 1 LET lr_formato_2009.total_issste        = ld_total_trabs
      	 WHEN 2 LET lr_formato_2009.total_independiente = ld_total_trabs
      	 WHEN 3 LET lr_formato_2009.total_mixto         = ld_total_trabs
      END CASE
   END FOREACH

   RETURN lr_formato_2009.*
END FUNCTION
################################################################################