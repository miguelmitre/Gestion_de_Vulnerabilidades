################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX03    => ESTADO DE CUENTA                                       #
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
END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO DE ANEXO 71 CONSAR",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX03.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de anexo 71 CNS")

   CALL inicializa()
   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
################################################################################
FUNCTION inicializa()
   LET gi_registros = 0
   LET hoy          = TODAY

   SELECT codigo_afore
   INTO   gi_afore
   FROM   safre_af:tab_afore_local

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   LET gr_estado.iniciado    = 0
   LET gr_estado.generado    = 10
   LET gr_estado.concatenado = 20

   --RECUPERAR EL TOTAL DE REGISTROS
   SELECT SUM(registros)
   INTO   gi_registros
   FROM   safre_af:ctr_anexo71
   WHERE  fecha_corte  = gd_fecha_corte
   AND    tipo_reporte = 1
   AND    tipo_reg     = 2
   AND    estado       = 20
END FUNCTION
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
       "CTANX03",         -- proceso_cod
       li_etapa_cod,      -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       "corte multi_sie", -- parametro1
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
FUNCTION archivo()
   DEFINE lc_nomarch       CHAR(80),
          lc_comando_cat   CHAR(8000),
          lc_encabezado    CHAR(80),
          lc_detalle03     CHAR(80),
          lc_archivo_final CHAR(80)

   DEFINE lc_totales CHAR(80)
   DEFINE lr_archivo_procesar RECORD
   	      consecutivo   SMALLINT ,
          archivo       CHAR(120),
          registros     INTEGER
          END RECORD

   DEFINE lc_sumario CHAR(80)

   --LIMPIAR REGISTROS
   DELETE
   FROM   safre_af:ctr_anexo71
   WHERE  fecha_corte  = gd_fecha_corte
   AND    tipo_reporte = 2

   LET lc_comando_cat = "cat"
   INITIALIZE lc_sumario TO NULL

   CALL nombra_archivo() RETURNING lc_encabezado,
                                   lc_archivo_final

   IF gi_afore = 574 THEN --SCOTIA
      LET lc_encabezado    = "/safre_prc/cta/envio/", lc_encabezado CLIPPED
      LET lc_archivo_final = "/safre_prc/cta/envio/", lc_archivo_final CLIPPED
   	  LET lc_detalle03     = "/safre_prc/cta/envio/"
   ELSE
   	  LET lc_encabezado    = "/safre_back/", lc_encabezado CLIPPED
   	  LET lc_archivo_final = "/safre_back/", lc_archivo_final CLIPPED
   	  LET lc_detalle03     = "/safre_back/"
   END IF

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_encabezado CLIPPED

   DECLARE cur_prc CURSOR FOR
   SELECT consecutivo,
          archivo    ,
          registros
   FROM   safre_af:ctr_anexo71
   WHERE  fecha_corte  = gd_fecha_corte
   AND    tipo_reporte = 1
   AND    tipo_reg     = 2
   AND    estado       = 20
   ORDER BY 1

   --RECUPERAR DETALLE 02
   FOREACH cur_prc INTO lr_archivo_procesar.consecutivo,
                        lr_archivo_procesar.archivo    ,
                        lr_archivo_procesar.registros

      LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                           lr_archivo_procesar.archivo CLIPPED

      CALL registra_etapa(gd_fecha_corte,  --fecha_corte
                          2,               --tipo_reporte
                          2,               --tipo_reg
                          lr_archivo_procesar.consecutivo,  --consecutivo
                          lr_archivo_procesar.archivo,      --archivo
                          lr_archivo_procesar.registros,    --registros
                          10,               --estado
                          TODAY,            --fecha_proceso
                          gc_usuario        --usuario
                          )
   END FOREACH

   CALL registra_etapa(gd_fecha_corte,  --fecha_corte
                       2,               --tipo_reporte
                       4,               --tipo_reg
                       1,               --consecutivo
                       lc_archivo_final,--archivo
                       0,               --registros
                       0,               --estado
                       TODAY,           --fecha_proceso
                       gc_usuario       --usuario
                       )

   --DETALLE 03
   LET lc_detalle03 = lc_detalle03 CLIPPED,
                      "anexo_71_det03_",
                      gd_fecha_corte USING "DDMMYYYY"

   CALL detalle03(lc_detalle03,
                  gi_afore)

   --ENCABEZADO
   CALL encabezado(lc_encabezado)

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_detalle03   CLIPPED


   --CONCATENAR
   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_archivo_final CLIPPED
   RUN lc_comando_cat

   CALL actualiza_etapa_archivo(gd_fecha_corte,  --fecha_corte
                                2,               --tipo_reporte
                                4,               --tipo_reg
                                1,               --consecutivo
                                1,               --registros
                                10               --estado
                                )

   CALL finaliza_etapa_archivo(gd_fecha_corte,   --fecha_corte
                               2             ,   --tipo_reporte
                               gi_registros + 1 ,--registros
                               20                --estado
                               )

   IF gi_afore = 574 THEN
   	  SELECT archivo
   	  INTO   lc_sumario
      FROM   safre_af:ctr_anexo71
      WHERE  fecha_corte  = gd_fecha_corte
      AND    tipo_reporte = 1
      AND    tipo_reg     = 99
      AND    estado       = 20

      IF SQLCA.SQLCODE = 0 THEN
      	 LET lc_comando_cat = "cat ",
      	                      lc_sumario CLIPPED, " ",
      	                      lc_detalle03 CLIPPED, " > ",
      	                      lc_sumario
      	 RUN lc_comando_cat
      END IF
   END IF

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

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
                 " AND    proceso_cod = 'CTANX03' ",
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
                 " AND    proceso_cod   = 'CTANX03'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################
FUNCTION get_precio_del_dia(li_siefore, ld_fecha_corte)
   DEFINE
      li_siefore     SMALLINT,
      ld_fecha_corte DATE,
      ld_precio      DECIMAL(19,14)

   SELECT precio_del_dia
   INTO   ld_precio
   FROM   safre_af:glo_valor_accion
   WHERE  codigo_siefore  = li_siefore
   AND    fecha_valuacion = ld_fecha_corte

   IF ld_precio IS NULL THEN
      LET ld_precio = 0
   END IF

   RETURN ld_precio
END FUNCTION
################################################################################
FUNCTION nombra_archivo()
   DEFINE lc_archivo_final CHAR(80),
          lc_encabezado    CHAR(80)

   DEFINE lr_archivo RECORD
   	         codigo_afore SMALLINT,
   	         fecha_envio  DATE,
   	         tipo_inf     SMALLINT
   	      END RECORD

   DEFINE ld_fecha_envio DATE

   SELECT codigo_afore
   INTO   lr_archivo.codigo_afore
   FROM   safre_af:tab_afore_local

   LET lr_archivo.fecha_envio = hoy
   LET lr_archivo.tipo_inf    = 1 --5 SIEFORES

   CALL get_fecha_archivo(gd_fecha_corte) RETURNING lr_archivo.fecha_envio

   LET lc_archivo_final = lr_archivo.fecha_envio  USING "YYYYMMDD", "_",
                          "AF_",
                          lr_archivo.codigo_afore USING "&&&", "_",
                          "016_",
                          "001"

   LET lc_encabezado = "anexo_enc_",
                       lr_archivo.tipo_inf USING "&", "_",
                       lr_archivo.fecha_envio USING "DDMMYYYY"

   RETURN lc_encabezado,
          lc_archivo_final
END FUNCTION
################################################################################
FUNCTION encabezado(lc_encabezado)
   DEFINE lc_encabezado    CHAR(80)

   DEFINE lr_encabezado RECORD
   	         codigo_afore SMALLINT,
   	         fecha_envio  DATE,
   	         tipo_inf     SMALLINT
   	      END RECORD

   SELECT codigo_afore
   INTO   lr_encabezado.codigo_afore
   FROM   safre_af:tab_afore_local

   SELECT parametro5
	 INTO   lr_encabezado.fecha_envio
   FROM   safre_af:dis_ctrl_proceso
   WHERE  folio IN (SELECT MAX(folio)
                    FROM   safre_af:dis_ctrl_proceso
                    WHERE  proceso_cod = 'CTANX01'
                    AND    etapa_cod = 1)
   AND    proceso_cod   = 'CTANX01'
   AND    etapa_cod     = 1

   LET lr_encabezado.tipo_inf    = 1 --5 SIEFORES

   LET lr_encabezado.fecha_envio = MDY(MONTH(gd_fecha_corte),01, YEAR(gd_fecha_corte))

   START REPORT rpt_encabezado TO lc_encabezado
      CALL registra_etapa(gd_fecha_corte, --fecha_corte
                       2,              --tipo_reporte
                       1,              --tipo_reg
                       1,              --consecutivo
                       lc_encabezado,  --archivo
                       0,              --registros
                       0,              --estado
                       TODAY,          --fecha_proceso
                       gc_usuario      --usuario
                       )

      OUTPUT TO REPORT rpt_encabezado(lr_encabezado.*)
   FINISH REPORT rpt_encabezado

   CALL actualiza_etapa_archivo(gd_fecha_corte,  --fecha_corte
                                2,               --tipo_reporte
                                1,               --tipo_reg
                                1,               --consecutivo
                                1,               --registros
                                10               --estado
                                )

END FUNCTION
################################################################################
REPORT rpt_encabezado(lr_encabezado)
   DEFINE lr_encabezado RECORD
   	         codigo_afore SMALLINT,
   	         fecha_envio  DATE,
   	         tipo_inf     SMALLINT
   	      END RECORD

   DEFINE lc_filler CHAR(228)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 01, "01",
                       lr_encabezado.codigo_afore USING "&&&",
                       lr_encabezado.fecha_envio  USING "YYYYMMDD",
                       lr_encabezado.tipo_inf     USING "&",
                       gi_registros        + 1    USING "&&&&&&&&&&",
                       180 SPACES

END REPORT
################################################################################
FUNCTION get_fecha_archivo(ld_fecha_corte)
   DEFINE ld_fecha_corte   DATE
   DEFINE ld_fecha_archivo DATE
   DEFINE li_mes_corte     SMALLINT
   DEFINE li_mes_archivo   SMALLINT
   DEFINE li_dia_archivo   SMALLINT

   LET li_mes_corte = MONTH (ld_fecha_corte)

   IF li_mes_corte = 12 THEN
   	  LET li_mes_archivo = 1
   ELSE
      LET li_mes_archivo = li_mes_corte + 1
   END IF

   CASE li_mes_corte
   	  WHEN 1 LET li_dia_archivo = 13
   	  WHEN 2 LET li_dia_archivo = 13
   	  WHEN 3 LET li_dia_archivo = 8
   	  WHEN 4 LET li_dia_archivo = 15
   	  WHEN 5 LET li_dia_archivo = 12
   	  WHEN 6 LET li_dia_archivo = 10
   	  WHEN 7 LET li_dia_archivo = 14
   	  WHEN 8 LET li_dia_archivo = 11
   	  WHEN 9 LET li_dia_archivo = 16
   	  WHEN 10 LET li_dia_archivo = 13
   	  WHEN 11 LET li_dia_archivo = 11
   	  WHEN 12 LET li_dia_archivo = 16
   END CASE

   LET ld_fecha_archivo = MDY(li_mes_archivo,li_dia_archivo,YEAR(ld_fecha_corte))

   RETURN ld_fecha_archivo
END FUNCTION
################################################################################
FUNCTION registra_etapa(lr_ctr_anexo71)

   DEFINE lr_ctr_anexo71 RECORD
   	      fecha_corte   DATE     ,
   	      tipo_reporte  SMALLINT ,
          tipo_reg      SMALLINT ,
          consecutivo   SMALLINT ,
          archivo       CHAR(120),
          registros     INTEGER  ,
          estado        SMALLINT ,
          fecha_proceso DATE     ,
          usuario       CHAR(8)
   	      END RECORD

   INSERT INTO safre_af:ctr_anexo71 VALUES(lr_ctr_anexo71.*)
END FUNCTION
################################################################################
FUNCTION actualiza_etapa_archivo(lr_ctr_anexo71)

   DEFINE lr_ctr_anexo71 RECORD
   	      fecha_corte   DATE     ,
   	      tipo_reporte  SMALLINT ,
          tipo_reg      SMALLINT ,
          consecutivo   SMALLINT ,
          registros     INTEGER  ,
          estado        SMALLINT
   	      END RECORD

   UPDATE safre_af:ctr_anexo71
   SET    estado        = lr_ctr_anexo71.estado,
          fecha_proceso = TODAY,
          registros     = lr_ctr_anexo71.registros
   WHERE  fecha_corte   = lr_ctr_anexo71.fecha_corte
   AND    tipo_reporte  = lr_ctr_anexo71.tipo_reporte
   AND    tipo_reg      = lr_ctr_anexo71.tipo_reg
   AND    consecutivo   = lr_ctr_anexo71.consecutivo

END FUNCTION
################################################################################
FUNCTION finaliza_etapa_archivo(lr_ctr_anexo71)

   DEFINE lr_ctr_anexo71 RECORD
   	      fecha_corte   DATE     ,
   	      tipo_reporte  SMALLINT ,
   	      registros     INTEGER  ,
          estado        SMALLINT
   	      END RECORD

   UPDATE safre_af:ctr_anexo71
   SET    estado        = lr_ctr_anexo71.estado
   WHERE  fecha_corte   = lr_ctr_anexo71.fecha_corte
   AND    tipo_reporte  = lr_ctr_anexo71.tipo_reporte

   UPDATE safre_af:ctr_anexo71
   SET    registros     = lr_ctr_anexo71.registros
   WHERE  fecha_corte   = lr_ctr_anexo71.fecha_corte
   AND    tipo_reporte  = lr_ctr_anexo71.tipo_reporte
   AND    tipo_reg      = 4

END FUNCTION
################################################################################
FUNCTION detalle03(lc_detalle03,
                   li_codigo_afore)

   DEFINE lc_detalle03    CHAR(80)

   DEFINE lr_detalle03 RECORD
   	      tipo_ent      SMALLINT,
          cve_ent       SMALLINT,
          cve_sub_ent   SMALLINT,
          precio_accion    DECIMAL(19,14),
          precio_infonavit DECIMAL(19,14),
          precio_fovissste DECIMAL(19,14),
          precio_udi       DECIMAL(19,14)
   	      END RECORD

   DEFINE li_codigo_afore,
          li_siefore      SMALLINT

   DEFINE li_precio_accion DECIMAL(19,14)
   DEFINE li_registros INTEGER

   DEFINE ld_fecha_viv  DATE
   DEFINE ld_fecha_paso  DATE
   DEFINE ld_fecha_udi  DATE

   LET li_registros = 0

   LET ld_fecha_viv = MDY(MONTH(gd_fecha_corte),1,YEAR(gd_fecha_corte))
   LET ld_fecha_paso = MDY(MONTH(gd_fecha_corte) + 1,1,YEAR(gd_fecha_corte))

   CALL habil_anterior(ld_fecha_paso, 1) RETURNING ld_fecha_udi

   --DISPLAY "ld_fecha_viv: ", ld_fecha_viv USING "DD/MM/YYYY"
   --DISPLAY "ld_fecha_udi: ", ld_fecha_udi USING "DD/MM/YYYY"

   DECLARE cur_det_03 CURSOR FOR
   SELECT tipo_ent,
          cve_ent ,
          siefore ,
          cve_sub_ent
   FROM   safre_af:tab_anexo71_ent
   WHERE  afore_cod = li_codigo_afore
   ORDER  BY 1,3

   START REPORT rpt_detalle_03 TO lc_detalle03
      CALL registra_etapa(gd_fecha_corte, --fecha_corte
                          2,              --tipo_reporte
                          3,              --tipo_reg
                          1,              --consecutivo
                          lc_detalle03,   --archivo
                          0,              --registros
                          0,              --estado
                          TODAY,          --fecha_proceso
                          gc_usuario      --usuario
                          )

      FOREACH cur_det_03 INTO lr_detalle03.tipo_ent,
      	                      lr_detalle03.cve_ent ,
      	                      li_siefore           ,
      	                      lr_detalle03.cve_sub_ent

      	 IF li_siefore = 11 OR
      	 	  li_siefore = 12 THEN
      	 	  CALL get_precio_del_dia(li_siefore,ld_fecha_viv)
      	    RETURNING li_precio_accion
      	 ELSE
      	 	  IF li_siefore = 13 THEN
      	 	  	 CALL get_precio_del_dia(li_siefore,ld_fecha_udi)
      	       RETURNING li_precio_accion
      	    ELSE
      	    	 CALL get_precio_del_dia(li_siefore,gd_fecha_corte)
      	       RETURNING li_precio_accion
      	 	  END IF
      	 END IF

      	 CASE
      	    WHEN li_siefore = 1 OR
      	    	   li_siefore = 2 OR
      	    	   li_siefore = 3 OR
      	    	   li_siefore = 4 OR
      	    	   li_siefore = 5 OR
      	    	   li_siefore = 6

      	       LET lr_detalle03.precio_accion    = li_precio_accion
      	       LET lr_detalle03.precio_infonavit = 0
      	       LET lr_detalle03.precio_fovissste = 0
      	       LET lr_detalle03.precio_udi       = 0

      	   WHEN li_siefore = 11
      	   	   LET lr_detalle03.precio_accion    = 0
      	       LET lr_detalle03.precio_infonavit = li_precio_accion
      	       LET lr_detalle03.precio_fovissste = 0
      	       LET lr_detalle03.precio_udi       = 0

      	   WHEN li_siefore = 12
      	   	   LET lr_detalle03.precio_accion    = 0
      	       LET lr_detalle03.precio_infonavit = 0
      	       LET lr_detalle03.precio_fovissste = li_precio_accion
      	       LET lr_detalle03.precio_udi       = 0

      	   WHEN li_siefore = 13
      	   	   LET lr_detalle03.precio_accion    = 0
      	       LET lr_detalle03.precio_infonavit = 0
      	       LET lr_detalle03.precio_fovissste = 0
      	       LET lr_detalle03.precio_udi       = li_precio_accion
      	 END CASE

         OUTPUT TO REPORT rpt_detalle_03(lr_detalle03.*)

         LET li_registros = li_registros + 1

      END FOREACH
   FINISH REPORT rpt_detalle_03

   CALL actualiza_etapa_archivo(gd_fecha_corte,  --fecha_corte
                                2,               --tipo_reporte
                                3,               --tipo_reg
                                1,               --consecutivo
                                li_registros,    --registros
                                10               --estado
                                )
END FUNCTION
################################################################################
REPORT rpt_detalle_03(lr_detalle03)
   DEFINE lr_detalle03 RECORD
   	      tipo_ent      SMALLINT,
          cve_ent       SMALLINT,
          cve_sub_ent   SMALLINT,
          precio_accion    DECIMAL(19,14),
          precio_infonavit DECIMAL(19,14),
          precio_fovissste DECIMAL(19,14),
          precio_udi       DECIMAL(19,14)
   	      END RECORD

   DEFINE lc_precio_accion   ,
          lc_precio_infonavit,
          lc_precio_fovissste,
          lc_precio_udi      CHAR(17)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW

      LET lc_precio_accion    = lr_detalle03.precio_accion    USING "&&.&&&&&&"
      LET lc_precio_infonavit = lr_detalle03.precio_infonavit USING "&&.&&&&&"
      LET lc_precio_fovissste = lr_detalle03.precio_fovissste USING "&&.&&&&&&&&&&&&&&"
      LET lc_precio_udi       = lr_detalle03.precio_udi       USING "&&.&&&&&&"

      PRINT COLUMN 01, "03",
                       lr_detalle03.tipo_ent      USING "&&",
                       lr_detalle03.cve_ent       USING "&&&",
                       lr_detalle03.cve_sub_ent   USING "&&&",

                       lc_precio_accion[1,2],    lc_precio_accion[4,9],
                       lc_precio_infonavit[1,2], lc_precio_infonavit[4,8],
                       lc_precio_fovissste[1,2], lc_precio_fovissste[4,17],
                       lc_precio_udi[1,2],       lc_precio_udi[4,9],

                       155 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION habil_anterior(diaActual,numDiaHabil)
   DEFINE
       diaTmp	              ,
       diaHabilAnt	        ,
       diaActual	          DATE

   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	              ,
       feriado	                ,
       finSemana	        SMALLINT

   LET cont_1      = 0
   LET diaHabilAnt = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilAnt)

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       ELSE
           SELECT *
           FROM   safre_af:tab_feriado
           WHERE  feria_fecha = diaHabilAnt

           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF

       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilAnt
END FUNCTION
################################################################################