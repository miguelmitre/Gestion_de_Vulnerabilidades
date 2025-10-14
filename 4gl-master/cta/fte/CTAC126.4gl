################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAC126    => ARCHIVO OPERACION 81                                   #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 03 de JULIO     de 2009                                #
################################################################################
DATABASE safre_af
################################################################################
GLOBALS
   DEFINE gi_folio,
          gi_registros,
          gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE

   DEFINE hoy DATE

   DEFINE gi_afore   SMALLINT
   DEFINE gc_usuario CHAR(08)

   DEFINE  gr_estado RECORD
 	         iniciado,
 	         generado,
 	         concatenado SMALLINT
 	         END RECORD

 	 DEFINE gr_seg_modulo RECORD
   	      modulo_cod           char(04),
          ruta_envio           char(40)
   END RECORD
END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO OP. 81",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTAC126.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo Op. 81")

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

   LET gr_estado.iniciado    = 0
   LET gr_estado.generado    = 10
   LET gr_estado.concatenado = 20

   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
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
       "CTAC126",         -- proceso_cod
       li_etapa_cod,      -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       "Op. 81",          -- parametro1
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
          lc_nomarch_ini   CHAR(80),
          lc_comando_cat   CHAR(8000),
          lc_comando_rm    CHAR(8000),
          lc_encabezado    CHAR(80),
          lc_archivo_final CHAR(80)

   DEFINE lr_trabajador RECORD
   	      nss              CHAR(11),
          curp             CHAR(18),
          rfc              CHAR(13),
          id_sector        SMALLINT,
          ind_edad         SMALLINT
          END RECORD

   DEFINE lc_prioridad CHAR (100)


   DEFINE li_rango_edad SMALLINT

   DEFINE li_consecutivo SMALLINT
   DEFINE li_registros_ini,
          li_registros_archivo INTEGER

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   DELETE
   FROM   safre_af:ctr_op81
   WHERE  fecha_corte  = gd_fecha_corte
   AND    tipo_reporte = 1

   CALL nombra_archivo() RETURNING lc_encabezado,
                                   lc_archivo_final

   LET lc_encabezado  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado CLIPPED
   LET lc_nomarch_ini = gr_seg_modulo.ruta_envio CLIPPED,"/", "Op81_"

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_encabezado CLIPPED

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cta_transf_edad
   WHENEVER ERROR STOP

  SQL

  CREATE TEMP TABLE tmp_cta_transf_edad
  (
    nss              CHAR(11),
    tipo_solicitud   SMALLINT,
    curp             CHAR(18),
    rfc              CHAR(13),
    fecha_nacimiento DATE,
    edad             SMALLINT,
    estatus_cuenta   SMALLINT,
    id_sector        SMALLINT,
    criterio         SMALLINT,
    ind_edad         SMALLINT,
    fecha_corte      DATE,
    estado           SMALLINT,
    factualiza       DATE,
    usuario          CHAR(18)
  ) IN tmp_dbs1;

  END SQL

  LET lc_prioridad = "SET PDQPRIORITY HIGH"
  PREPARE prep_prioridadh FROM lc_prioridad
  EXECUTE prep_prioridadh

  INSERT INTO tmp_cta_transf_edad
  SELECT *
  FROM   cta_transf_edad
  WHERE  fecha_corte = gd_fecha_corte
  AND    nss NOT IN (SELECT nss
                     FROM   cta_det_sie_acep)

  SQL
     CREATE INDEX tmp_cta_transf_edad1 ON tmp_cta_transf_edad (nss,ind_edad) in tmp_dbs1;
  END SQL

  UPDATE STATISTICS FOR TABLE tmp_cta_transf_edad

  FOR li_rango_edad = 1 TO 5
     LET lc_nomarch = lc_nomarch_ini CLIPPED ,
                     "sie",
                     li_rango_edad USING "&",
                     "_",
                     gd_fecha_corte USING "DDMMYYYY"

     LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                          lc_nomarch CLIPPED

     START REPORT rtp_op81 TO lc_nomarch
        LET li_registros_ini = gi_registros

        --Ingresar nombre detalle 02 a la tabla de control
        INITIALIZE li_consecutivo TO NULL

        SELECT MAX(consecutivo)
        INTO   li_consecutivo
        FROM   safre_af:ctr_op81
        WHERE  fecha_corte  = gd_fecha_corte
        AND    tipo_reporte = 1
        AND    tipo_reg     = 2

        IF li_consecutivo IS NULL THEN
        	 LET li_consecutivo = 1
        ELSE
        	 LET li_consecutivo = li_consecutivo + 1
        END IF

        CALL registra_etapa(gd_fecha_corte,  --fecha_corte
                            1,               --tipo_reporte
                            2,               --tipo_reg
                            li_consecutivo,  --consecutivo
                            lc_nomarch,      --archivo
                            0,               --registros
                            0,               --estado
                            TODAY,           --fecha_proceso
                            gc_usuario       --usuario
                            )

        DECLARE cur_siefore CURSOR FOR
        SELECT a.nss      ,
               a.curp     ,
               a.rfc      ,
               a.id_sector,
               a.ind_edad
        FROM   tmp_cta_transf_edad a
        WHERE  a.ind_edad = li_rango_edad
        ORDER BY 1

        FOREACH cur_siefore INTO lr_trabajador.nss      ,
                                 lr_trabajador.curp     ,
                                 lr_trabajador.rfc      ,
                                 lr_trabajador.id_sector,
                                 lr_trabajador.ind_edad

           OUTPUT TO REPORT rtp_op81(lr_trabajador.*)

        END FOREACH
     FINISH REPORT rtp_op81

     LET li_registros_archivo = gi_registros - li_registros_ini

     CALL actualiza_etapa_archivo(gd_fecha_corte,       --fecha_corte
                                  1,                    --tipo_reporte
                                  2,                    --tipo_reg
                                  li_consecutivo,       --consecutivo
                                  li_registros_archivo, --registros
                                  10                    --estado
                                  )
  END FOR

  CALL encabezado(lc_encabezado)

  LET lc_nomarch = gr_seg_modulo.ruta_envio CLIPPED, "/", lc_archivo_final CLIPPED

  CALL registra_etapa(gd_fecha_corte,  --fecha_corte
                      1,               --tipo_reporte
                      4,               --tipo_reg
                      1,               --consecutivo
                      lc_nomarch,      --archivo
                      0,               --registros
                      0,               --estado
                      TODAY,           --fecha_proceso
                      gc_usuario       --usuario
                      )

   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_nomarch CLIPPED
   RUN lc_comando_cat

   CALL actualiza_etapa_archivo(gd_fecha_corte,  --fecha_corte
                                1,               --tipo_reporte
                                4,               --tipo_reg
                                1,               --consecutivo
                                1,               --registros
                                10               --estado
                                )

   CALL finaliza_etapa_archivo(gd_fecha_corte,   --fecha_corte
                               1             ,   --tipo_reporte
                               gi_registros  + 1,--registros
                               20                --estado
                               )

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   EXECUTE prep_prioridadl

END FUNCTION
################################################################################
FUNCTION registra_etapa(lr_ctr_op81)

   DEFINE lr_ctr_op81 RECORD
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

   INSERT INTO safre_af:ctr_op81 VALUES(NULL, lr_ctr_op81.*)
END FUNCTION
################################################################################
FUNCTION actualiza_etapa_archivo(lr_ctr_op81)

   DEFINE lr_ctr_op81 RECORD
   	      fecha_corte   DATE     ,
   	      tipo_reporte  SMALLINT ,
          tipo_reg      SMALLINT ,
          consecutivo   SMALLINT ,
          registros     INTEGER  ,
          estado        SMALLINT
   	      END RECORD

   UPDATE safre_af:ctr_op81
   SET    estado        = lr_ctr_op81.estado,
          fecha_proceso = TODAY,
          registros     = lr_ctr_op81.registros
   WHERE  fecha_corte   = lr_ctr_op81.fecha_corte
   AND    tipo_reporte  = lr_ctr_op81.tipo_reporte
   AND    tipo_reg      = lr_ctr_op81.tipo_reg
   AND    consecutivo   = lr_ctr_op81.consecutivo
END FUNCTION
################################################################################
FUNCTION finaliza_etapa_archivo(lr_ctr_op81)

   DEFINE lr_ctr_op81 RECORD
   	      fecha_corte   DATE     ,
   	      tipo_reporte  SMALLINT ,
   	      registros     INTEGER  ,
          estado        SMALLINT
   	      END RECORD

   UPDATE safre_af:ctr_op81
   SET    estado        = lr_ctr_op81.estado
   WHERE  fecha_corte   = lr_ctr_op81.fecha_corte
   AND    tipo_reporte  = lr_ctr_op81.tipo_reporte

   UPDATE safre_af:ctr_op81
   SET    registros     = lr_ctr_op81.registros
   WHERE  fecha_corte   = lr_ctr_op81.fecha_corte
   AND    tipo_reporte  = lr_ctr_op81.tipo_reporte
   AND    tipo_reg      = 4

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
                 " AND    proceso_cod = 'CTAC126' ",
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
                 " AND    proceso_cod   = 'CTAC126'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

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

   LET lc_archivo_final = "AFO_",
                          lr_archivo.codigo_afore USING "&&&", "_",
                          gd_fecha_corte USING "DDMMYYYY", ".BDSELSIE"

   LET lc_encabezado = "op81_enc_",
                       gd_fecha_corte USING "DDMMYYYY"

   RETURN lc_encabezado,
          lc_archivo_final
END FUNCTION
################################################################################
REPORT rtp_op81(lr_trabajador)

   DEFINE lr_trabajador RECORD
   	      nss              CHAR(11),
          curp             CHAR(18),
          rfc              CHAR(13),
          id_sector        SMALLINT,
          ind_edad         SMALLINT
          END RECORD

   DEFINE lc_desc_siefore CHAR(08)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT

   ON EVERY ROW
      INITIALIZE lc_desc_siefore TO NULL

      SELECT razon_social
      INTO   lc_desc_siefore
      FROM   tab_siefore_local
      WHERE  codigo_siefore = lr_trabajador.ind_edad

      IF lr_trabajador.nss[1] = "I" THEN
         LET lr_trabajador.nss = "00000000000"
      END IF

      PRINT COLUMN 001, "02"                              ,
            COLUMN 003, lr_trabajador.nss                 ,
            COLUMN 014, lr_trabajador.curp                ,
            COLUMN 032, lr_trabajador.rfc                 ,
            COLUMN 045, lr_trabajador.id_sector USING "&" ,
            --COLUMN 046, lr_trabajador.ind_edad  USING "&&&&&&&&" ,
            COLUMN 046, lc_desc_siefore,
            COLUMN 054, 97 SPACES

      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION encabezado(lc_encabezado)
   DEFINE lc_encabezado    CHAR(80)

   DEFINE lr_encabezado RECORD
   	         id_servicio  CHAR(02),
   	         id_operacion CHAR(02),
   	         codigo_afore SMALLINT,
   	         fecha_envio  DATE,
   	         tipo_inf     SMALLINT
   	      END RECORD

   LET lr_encabezado.id_servicio = "03"
   LET lr_encabezado.id_operacion = "81"

   SELECT codigo_afore
   INTO   lr_encabezado.codigo_afore
   FROM   safre_af:tab_afore_local

   LET lr_encabezado.fecha_envio = "07/08/2009"
   LET lr_encabezado.tipo_inf    = 1


   START REPORT rpt_encabezado TO lc_encabezado
      CALL registra_etapa(gd_fecha_corte, --fecha_corte
                       1,              --tipo_reporte
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
                                1,               --tipo_reporte
                                1,               --tipo_reg
                                1,               --consecutivo
                                1,               --registros
                                10               --estado
                                )

END FUNCTION
################################################################################
REPORT rpt_encabezado(lr_encabezado)
   DEFINE lr_encabezado RECORD
   	         id_servicio  CHAR(02),
   	         id_operacion CHAR(02),
   	         codigo_afore SMALLINT,
   	         fecha_envio  DATE,
   	         tipo_inf     SMALLINT
   	      END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      PRINT COLUMN 001, "01",
            COLUMN 003, lr_encabezado.id_servicio ,
            COLUMN 005, lr_encabezado.id_operacion,
            COLUMN 007, lr_encabezado.codigo_afore USING "&&&",
            COLUMN 010, lr_encabezado.fecha_envio  USING "YYYYMMDD",
            COLUMN 018, lr_encabezado.tipo_inf     USING "&",
            COLUMN 019, gi_registros        + 1    USING "&&&&&&&&&&",
            COLUMN 019, "001",
            119 SPACES

END REPORT
################################################################################