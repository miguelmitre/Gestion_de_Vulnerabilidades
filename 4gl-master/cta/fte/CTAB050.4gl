################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAB050    => FOLIOS DEL ESTADO DE CUENTA                            #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 06 de ENERO DE 2010                                    #
################################################################################
DATABASE safre_af
################################################################################
DEFINE gi_folio,
       gi_estado  INTEGER

DEFINE gr_reporte RECORD
	     nss   CHAR(11),
	     curp  CHAR(18),
	     folio CHAR(20)
	    END RECORD

DEFINE gr_encabezado RECORD
          afore        SMALLINT,
          fecha_envio  DATE    ,
          consecutivo  SMALLINT,
          cuatrimestre CHAR(6)
       END RECORD

DEFINE gr_seg_modulo RECORD
   	      modulo_cod           char(04),
          ruta_envio           char(40)
   END RECORD

DEFINE gi_registros INTEGER,
       hoy          DATE,
       gc_usuario   CHAR(08)

DEFINE gc_archivo CHAR(100)

DEFINE gi_mes SMALLINT

DEFINE gd_fecha_corte DATE
################################################################################
MAIN
   LET gd_fecha_corte  = ARG_VAL(1)
   LET gi_folio        = ARG_VAL(2)
   LET gi_estado       = ARG_VAL(3)

   DISPLAY "INICIA PROCESO DE ESTADO DE CUENTA (FOLIOS)",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTAB050.log")

   CALL Ingresa_etapa(gi_folio,2,gi_estado,"Inicia folio de estado de cuenta")

   CALL inicializa()
   CALL tabla_estados()
   CALL archivo()

   DISPLAY "TERMINA PROCESO DE ESTADO DE CUENTA: (FOLIOS)",gi_registros
END MAIN
#####################################################################
FUNCTION inicializa()
   DEFINE lc_sql CHAR(500)
   DEFINE ld_fecha_paso DATE

   LET gi_registros = 0
   LET hoy          = TODAY

   SELECT codigo_afore
   INTO   gr_encabezado.afore
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

   LET gi_mes = MONTH(gd_fecha_corte)

   LET gr_encabezado.cuatrimestre[1,2] = gi_mes / 4 USING "&&"
   LET gr_encabezado.cuatrimestre[3,6] = YEAR(gd_fecha_corte)

   LET ld_fecha_paso = MDY(MONTH(gd_fecha_corte), 1, YEAR(gd_fecha_corte))

   LET ld_fecha_paso = ld_fecha_paso + 2 UNITS MONTH

   CALL habil_anterior(ld_fecha_paso, 2) RETURNING gr_encabezado.fecha_envio
END FUNCTION
################################################################################
FUNCTION tabla_estados()
   DEFINE ls_estado SMALLINT

DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_estados
   WHENEVER ERROR STOP

   CREATE TABLE tmp_estados(
   estado SMALLINT
   )


   {FOR ls_estado = 1 TO 32
   	  INSERT INTO safre_tmp:tmp_estados VALUES(ls_estado)
   END FOR

   INSERT INTO safre_tmp:tmp_estados VALUES(40) --Bono Afore
   INSERT INTO safre_tmp:tmp_estados VALUES(41) --Bono Traspaso}

   INSERT INTO safre_tmp:tmp_estados
   SELECT UNIQUE estado
   FROM   safre_af:cta_nss_edo_cta

   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_estados

   DELETE
   FROM   safre_tmp:tmp_estados
   WHERE  estado IN (64,65,66)

DATABASE safre_af
END FUNCTION
################################################################################
FUNCTION archivo()
   DEFINE lc_nomarch       CHAR(80),
          lc_nomarch_ini   CHAR(80),
          lc_comando_cat   CHAR(9000),
          lc_comando_rm    CHAR(9000),
          lc_comando_chmod CHAR(9000),
          lc_encabezado    CHAR(100),
          lc_archivo_final CHAR(100),
          lc_sumario       CHAR(100)

   DEFINE ls_estado    SMALLINT

   DEFINE lc_prioridad CHAR (100)
   DEFINE li_registros INTEGER

   {LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   EXECUTE prep_prioridadh}

   CALL nombra_archivo() RETURNING lc_encabezado   ,
                                   lc_archivo_final,
                                   lc_sumario

   LET lc_encabezado = gr_seg_modulo.ruta_envio CLIPPED, "/",
                       lc_encabezado CLIPPED

   LET lc_sumario = gr_seg_modulo.ruta_envio CLIPPED, "/",
                       lc_sumario CLIPPED

   LET lc_comando_cat = "cat ", lc_encabezado CLIPPED
   LET lc_comando_chmod = ""

   #Nomenclatura archivo inicial
   LET lc_nomarch_ini = gr_seg_modulo.ruta_envio CLIPPED,
                        "/ecs_",gd_fecha_corte  USING "DDMMYYYY"

   #Obtenemos los estados a emitir
   DECLARE cur_estado CURSOR FOR
   SELECT a.estado
   FROM   safre_tmp:tmp_estados a
   ORDER BY 1

   #Un archivo por cada estado
   FOREACH cur_estado INTO ls_estado

   	  #Nombramos archivo
   	  LET lc_nomarch = lc_nomarch_ini CLIPPED ,
                       "_edo" ,ls_estado USING "&&"

      #Concatenar archivo
      LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                           lc_nomarch CLIPPED

      #Permisos de archivo
      LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                             lc_nomarch CLIPPED, ";"

      DECLARE cur_folio CURSOR FOR
      SELECT a.nss    ,
             a.curp   ,
             a.folio
      FROM   cta_nss_edo_cta  a
      WHERE  a.estado = ls_estado
      ORDER BY 1
      
      --SELECT a.nss    ,
      --       b.n_unico,
      --       a.folio
      --FROM   cta_nss_edo_cta  a,
      --       afi_mae_afiliado b
      --WHERE  a.nss    = b.n_seguro
      --AND    a.estado = ls_estado
      --ORDER BY 1

      --DISPLAY "lc_nomarch: ", lc_nomarch CLIPPED

      DISPLAY "Inicia archivo de folios del estado  :", ls_estado USING "&&&"
      LET li_registros = gi_registros

      START REPORT rpt_folio_edocta TO lc_nomarch

      FOREACH cur_folio INTO gr_reporte.nss  ,
      	                     gr_reporte.curp ,
      	                     gr_reporte.folio

      	  LET gi_registros = gi_registros + 1

      	  OUTPUT TO REPORT rpt_folio_edocta(gr_reporte.*)
      END FOREACH

      FINISH REPORT rpt_folio_edocta

      LET li_registros = gi_registros - li_registros
      DISPLAY "Termina archivo de folios del estado :", ls_estado  USING "&&&", " registros: ", li_registros

   END FOREACH

   #Encabezado
   CALL encabezado(lc_encabezado)

   #Sumario
   CALL sumario(lc_sumario)

   --DISPLAY "lc_encabezado: ", lc_encabezado CLIPPED
   --DISPLAY "lc_sumario: ", lc_sumario CLIPPED

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_sumario CLIPPED

   #Archivo final
   LET lc_nomarch = gr_seg_modulo.ruta_envio CLIPPED,
                    "/", lc_archivo_final CLIPPED

   #Concatenar todo al archivo final
   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_nomarch CLIPPED
   RUN lc_comando_cat

   #Permisos al archivo final
   LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                          lc_nomarch CLIPPED, ";"
   RUN lc_comando_chmod

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   EXECUTE prep_prioridadl
END FUNCTION
################################################################################
REPORT rpt_folio_edocta(lr_reporte)
   DEFINE lr_reporte RECORD
   	   nss   CHAR(11),
	     curp  CHAR(18),
	     folio CHAR(20)
	 END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      IF lr_reporte.nss[1] = "I" THEN
      	 LET lr_reporte.nss = "           "
      END IF

      PRINT COLUMN 001, "02",
            COLUMN 003, gi_registros USING "&&&&&&&&&&",
            COLUMN 013, 028 SPACES,
            COLUMN 041, lr_reporte.curp,
            COLUMN 059, lr_reporte.nss,
            COLUMN 070, lr_reporte.folio,
            COLUMN 090, 030 SPACES,
            COLUMN 120, 002 SPACES,
            COLUMN 122, 009 SPACES
END REPORT
################################################################################
FUNCTION nombra_archivo()
   DEFINE lc_archivo_final CHAR(80),
          lc_encabezado    CHAR(80),
          lc_sumario       CHAR(80)

   LET lc_archivo_final = "RPT_EDC_FOLIOS.",gd_fecha_corte USING "DDMMYYYY"
   LET lc_encabezado    = "rpt_edc_folios_enc_",gd_fecha_corte USING "DDMMYYYY"
   LET lc_sumario       = "rpt_edc_folios_sum_",gd_fecha_corte USING "DDMMYYYY"

   RETURN lc_encabezado,
          lc_archivo_final,
          lc_sumario
END FUNCTION
################################################################################
FUNCTION encabezado(lc_encabezado)
   DEFINE lc_encabezado    CHAR(200)

   START REPORT rpt_encabezado TO lc_encabezado
      OUTPUT TO REPORT rpt_encabezado()
   FINISH REPORT rpt_encabezado
END FUNCTION
################################################################################
REPORT rpt_encabezado()

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, "01",
            COLUMN 003, "03",
            COLUMN 005, "65",
            COLUMN 007, "01",
            COLUMN 009, gr_encabezado.afore USING "&&&",
            COLUMN 012, 008 SPACES,
            COLUMN 020, gr_encabezado.fecha_envio USING "YYYYMMDD",
            COLUMN 028, "001",
            COLUMN 031, gr_encabezado.cuatrimestre,
            COLUMN 037, 002 SPACES,
            COLUMN 039, 002 SPACES,
            COLUMN 041, 009 SPACES,
            COLUMN 050, 081 SPACES
END REPORT
################################################################################
FUNCTION sumario(lc_sumario)
   DEFINE lc_sumario    CHAR(200)

   START REPORT rpt_sumario TO lc_sumario
      OUTPUT TO REPORT rpt_sumario()
   FINISH REPORT rpt_sumario
END FUNCTION
################################################################################
REPORT rpt_sumario()
  OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, "09",
            COLUMN 003, gi_registros USING "&&&&&&&&&",
            COLUMN 012, 119 SPACES
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

   LET cont_1      = 1
   LET diaHabilAnt = diaActual - 1 UNITS DAY

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
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
           END IF
           LET cont_1 = cont_1 + 1
       END IF
   END WHILE

   RETURN diaHabilAnt
END FUNCTION
################################################################################
FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vestado,vresultado)

   DEFINE vfolio         INTEGER,
          vetapa_cod     DECIMAL(2,0),
          vresultado     CHAR(50),
          vestado        SMALLINT

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES
      (TODAY,             -- fecha_proceso
       "CTA",             -- proceso_cod
       vetapa_cod,        -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       NULL,              -- parametro1
       vestado,           -- parametro2
       NULL,              -- parametro3
       NULL,              -- parametro4
       NULL,              -- parametro5
       vfolio,            -- folio
       vresultado,        -- resultado
       USER,              -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
            vetapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
################################################################################
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vpos,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          vpos         INTEGER,
          hoy          DATE

   DEFINE hora_inicial       CHAR(08),
          vhora_final        CHAR(08)

   DEFINE cla_sel            CHAR(400),
          vconsecutivo       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTA' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        parametro3 = ",vpos,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'CTA'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################