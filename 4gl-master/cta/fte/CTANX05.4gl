################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX05    => ESTADO DE CUENTA                                       #
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

  DEFINE gr_reporte RECORD
            precio1   DECIMAL(7,6),
            precio2   DECIMAL(7,6)
  END RECORD

  DEFINE gc_usuario   CHAR(08)

  DEFINE gi_afore           SMALLINT
  DEFINE hoy          DATE

 DEFINE  gr_sub_saldos ARRAY[20] OF RECORD
         subcta        CHAR(2),
         gr_sie_saldos ARRAY[10] OF RECORD
         siefore       SMALLINT,
         precio_dia    DECIMAL(22,6),
         acciones      DECIMAL(22,6),
         pesos      DECIMAL(22,6)
         END RECORD
         END RECORD

 DEFINE  gr_tot_saldos ARRAY[20] OF RECORD
         subcta        CHAR(2),
         gr_sie_saldos ARRAY[10] OF RECORD
         siefore       SMALLINT,
         acciones      DECIMAL(22,6),
         pesos      DECIMAL(22,6)
         END RECORD
         END RECORD

 DEFINE  gr_estado RECORD
 	       iniciado,
 	       generado,
 	       concatenado SMALLINT
 	       END RECORD

 DEFINE v_precio       ,
        v_precio_sb1   ,
        v_precio_sb2   ,
        v_precio_sb3   ,
        v_precio_sb4   ,
        v_precio_sb5   DECIMAL(22,6)

 DEFINE v_sb,i,j           SMALLINT

   DEFINE lr_reporte RECORD
            nss          CHAR(11),
            tipo_solicitud SMALLINT,
            curp         CHAR(18),
            rfc          CHAR(13),
            fena         CHAR(10),--DATE,
            fnacimiento  CHAR(08),
            fanio        CHAR(08),
            status_cta   SMALLINT,
            sector       SMALLINT,
            criterio_sie SMALLINT,
            precio1   DECIMAL(7,6),
            precio2   DECIMAL(7,6),
            precio3   DECIMAL(7,6),
            precio4   DECIMAL(7,6),
            precio5   DECIMAL(7,6)
   END RECORD

   DEFINE lr_saldos ARRAY[10] OF RECORD
          c_acciones   CHAR(16),
          c_pesos   CHAR(18)
          END RECORD

   DEFINE gr_seg_modulo RECORD
   	      modulo_cod           char(04),
          ruta_envio           char(40)
   END RECORD

   DEFINE gd_fecha_transferencia,
          gd_fecha_regimen        DATE
END GLOBALS
################################################################################
MAIN
   LET gi_folio         = ARG_VAL(1)
   LET gd_fecha_corte   = ARG_VAL(2)
   LET gi_tot_nss       = ARG_VAL(3)
   LET gd_fecha_regimen = ARG_VAL(4)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO DE ANEXO 71",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX05.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de anexo 71")

   LET gi_registros = 0
   LET hoy          = TODAY
   LET gd_fecha_transferencia = MDY(6,30,YEAR(HOY))

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

   CALL inicializa()

   CALL crea_tablas()

   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
################################################################################
FUNCTION inicializa()

DECLARE c_precio CURSOR FOR
      SELECT a.codigo_siefore, NVL(a.precio_del_dia,0)    --PRECIO DE ACCION SIEFORE 1
      FROM   glo_valor_accion a
      WHERE  a.codigo_siefore  BETWEEN 1 AND 5
      AND    a.fecha_valuacion = gd_fecha_corte

      FOREACH c_precio INTO v_sb, v_precio
         CASE v_sb
              WHEN 1
                   LET v_precio_sb1 = v_precio
              WHEN 2
                   LET v_precio_sb2 = v_precio
              WHEN 3
                   LET v_precio_sb3 = v_precio
              WHEN 4
                   LET v_precio_sb4 = v_precio
              WHEN 5
                   LET v_precio_sb5 = v_precio
          END CASE
      END FOREACH
CLOSE c_precio

      FOR i = 1 TO 20
          CASE i
               WHEN 1
                    LET gr_tot_saldos[i].subcta = "01"
               WHEN 2
                    LET gr_tot_saldos[i].subcta = "02"
               WHEN 3
                    LET gr_tot_saldos[i].subcta = "03"
               WHEN 4
                    LET gr_tot_saldos[i].subcta = "04"
               WHEN 5
                    LET gr_tot_saldos[i].subcta = "05"
               WHEN 6
                    LET gr_tot_saldos[i].subcta = "08"
               WHEN 7
                    LET gr_tot_saldos[i].subcta = "09"
               WHEN 8
                    LET gr_tot_saldos[i].subcta = "13"
               WHEN 9
                    LET gr_tot_saldos[i].subcta = "15"
               WHEN 10
                    LET gr_tot_saldos[i].subcta = "16"
               WHEN 11
                    LET gr_tot_saldos[i].subcta = "17"
               WHEN 12
                    LET gr_tot_saldos[i].subcta = "18"
               WHEN 13
                    LET gr_tot_saldos[i].subcta = "22"
               WHEN 14
                    LET gr_tot_saldos[i].subcta = "24"
               WHEN 15
                    LET gr_tot_saldos[i].subcta = "25"
               WHEN 16
                    LET gr_tot_saldos[i].subcta = "27"
               WHEN 17
                    LET gr_tot_saldos[i].subcta = "28"
               WHEN 18
                    LET gr_tot_saldos[i].subcta = "29"
               WHEN 19
                    LET gr_tot_saldos[i].subcta = "30"
               WHEN 20
                    LET gr_tot_saldos[i].subcta = "31"
             END CASE
          FOR j = 1 TO 10
          	  CASE j
          	  	WHEN 6 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore   = 7
          	  	WHEN 7 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore   = 6
          	  	WHEN 8 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore   = 11
          	    WHEN 9 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore   = 12
          	  	WHEN 10 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore  = 13
          	  	OTHERWISE LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = j
          	  END CASE

              LET gr_tot_saldos[i].gr_sie_saldos[j].acciones = 0
              LET gr_tot_saldos[i].gr_sie_saldos[j].pesos    = 0
          END FOR
      END FOR
END FUNCTION
################################################################################
FUNCTION crea_tablas()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cuota_trasferencia
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_cuota_trasferencia
   (nss              CHAR(11),
    tipo_solicitud   SMALLINT,
    curp             CHAR(18),
    rfc              CHAR(13),
    fecha_nacimiento DATE    ,
    edad             SMALLINT,
    estatus_cuenta   SMALLINT,
    id_sector        SMALLINT,
    criterio         SMALLINT,
    ind_edad         SMALLINT,
    fecha_corte      DATE    ,
    estado           SMALLINT,
    factualiza       DATE    ,
    usuario          CHAR(18)
   )

   {SELECT UNIQUE nss
   FROM   safre_af:cta_solicitud_regimen
   WHERE  fecha_solicitud >= gd_fecha_regimen
   AND    tipo_proceso = 13
   AND    estado = 0}

   SELECT UNIQUE nss
   FROM   safre_af:cta_transf_ed_gpo
   WHERE  estado      = 0
   AND    fecha_corte = "06/30/2009"
   INTO TEMP tmp_aceptados_regimen

   INSERT INTO tmp_cuota_trasferencia
   SELECT *
   FROM   safre_af:cta_transf_edad
   WHERE  fecha_corte = gd_fecha_transferencia --"06/30/2009"
   AND    nss IN (SELECT nss
                  FROM   tmp_aceptados_regimen)

   CREATE INDEX ix_tmp_cta_transf_edad1 on tmp_cuota_trasferencia
    (nss,ind_edad,estado)

   UPDATE STATISTICS FOR TABLE tmp_cuota_trasferencia

   DELETE
   FROM   tmp_cuota_trasferencia
   WHERE  nss IN ("15826213181", "48068113215")

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_aceptados_regimen
   WHENEVER ERROR STOP

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
       "CTANX05",         -- proceso_cod
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
          lc_nomarch_ini   CHAR(80),
          lc_comando_cat   CHAR(8000),
          lc_comando_rm    CHAR(8000),
          lc_encabezado    CHAR(80),
          lc_archivo_final CHAR(80),
          lc_detalle03     CHAR(80)

   DEFINE lr_monto_nss  RECORD
             nss         CHAR(11),
             subcta      CHAR(02),
             siefore     SMALLINT,
             acciones    DECIMAL(22,6),
             pesos       DECIMAL(22,6)
          END RECORD

   DEFINE ld_precio     DECIMAL(22,6)
   DEFINE i SMALLINT
   DEFINE lc_prioridad CHAR (100)

   DEFINE li_siefore_ori SMALLINT
   DEFINE li_bnd_vol     SMALLINT
   DEFINE li_siefore,
          li_estatus_cuenta,
          li_edad            SMALLINT

   DEFINE lc_totales CHAR(80)

   DEFINE li_rango_edad SMALLINT

   DEFINE li_consecutivo SMALLINT
   DEFINE li_registros_ini,
          li_registros_archivo INTEGER

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   DELETE
   FROM   safre_af:ctr_op81
   WHERE  fecha_corte  = gd_fecha_corte

   CALL nombra_archivo() RETURNING lc_encabezado,
                                   lc_archivo_final

   LET lc_encabezado  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado CLIPPED
   LET lc_nomarch_ini = gr_seg_modulo.ruta_envio CLIPPED,"/", "Op71_edad02_reg"

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_encabezado CLIPPED

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldos_cns_71
   WHENEVER ERROR STOP

  SQL

   CREATE TABLE tmp_saldos_cns_71
   ( nss            CHAR(11)     ,
     subcta         CHAR(02)     ,
     siefore        SMALLINT     ,
     acciones       DECIMAL(22,6),
     pesos          DECIMAL(22,6)
   ) FRAGMENT BY ROUND ROBIN IN tmp_dbs1,tmp_dbs2;

  END SQL

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   EXECUTE prep_prioridadh

   INSERT INTO tmp_saldos_cns_71
            SELECT c.nss                   ,
                   b.subct_cns             ,
                   a.siefore               ,
                   SUM(a.monto_en_acciones),
                   SUM(a.monto_en_pesos)
            FROM   tmp_cuota_trasferencia c        ,
                   OUTER tmp_saldo_anexo71 a,
                   safre_af:tab_subcuenta b
            WHERE  a.nss       = c.nss
            AND    a.subcuenta = b.subct_cod
            GROUP BY 1,2,3

   SQL
      CREATE INDEX tmp_saldos_cns711 ON tmp_saldos_cns_71 ( nss,subcta) in tmp_dbs2;
   END SQL

   UPDATE STATISTICS FOR TABLE tmp_saldos_cns_71

   LET lc_nomarch = lc_nomarch_ini CLIPPED,"_",
                    gd_fecha_corte USING "DDMMYYYY"

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_nomarch CLIPPED

   #Archivo de detalle
   START REPORT multi_sie TO lc_nomarch
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

   DECLARE cur_siefore_subcta CURSOR FOR
   SELECT a.nss     ,
          a.subcta  ,
          a.siefore ,
          a.acciones,
          a.pesos
   FROM   tmp_saldos_cns_71 a
   ORDER BY 1,2,3

   FOREACH cur_siefore_subcta INTO lr_monto_nss.nss     ,
   	                               lr_monto_nss.subcta  ,
   	                               lr_monto_nss.siefore ,
                                   lr_monto_nss.acciones,
                                   lr_monto_nss.pesos


      IF lr_monto_nss.acciones IS NULL THEN
      	  LET lr_monto_nss.acciones = 0
      END IF

      IF lr_monto_nss.pesos IS NULL THEN
      	  LET lr_monto_nss.pesos = 0
      END IF

      OUTPUT TO REPORT multi_sie(lr_monto_nss.*)

   END FOREACH
   FINISH REPORT multi_sie

   LET li_registros_archivo = gi_registros - li_registros_ini

   CALL actualiza_etapa_archivo(gd_fecha_corte,       --fecha_corte
                                1,                    --tipo_reporte
                                2,                    --tipo_reg
                                li_consecutivo,       --consecutivo
                                li_registros_archivo, --registros
                                10                    --estado
                                )

   #Archivo de detalle 03
   LET lc_detalle03 = gr_seg_modulo.ruta_envio CLIPPED,"/",
                      "Op71_edad03_reg_",
                      gd_fecha_corte USING "DDMMYYYY"

   CALL detalle03(lc_detalle03,
                  gi_afore)

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_detalle03   CLIPPED

   #Archivo de encabezado
   CALL encabezado(lc_encabezado)

   LET lc_nomarch = gr_seg_modulo.ruta_envio CLIPPED, "/",lc_archivo_final CLIPPED

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

   #Archivo de totales
   LET lc_totales = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    "Op71_edad_sumario_reg_", gd_fecha_corte USING "DDMMYYYY"

   START REPORT rpt_sumario TO lc_totales
      CALL registra_etapa(gd_fecha_corte,  --fecha_corte
                       1,               --tipo_reporte
                       99,              --tipo_reg
                       1,               --consecutivo
                       lc_totales,      --archivo
                       0,               --registros
                       0,               --estado
                       TODAY,           --fecha_proceso
                       gc_usuario       --usuario
                       )

      OUTPUT TO REPORT rpt_sumario()
   FINISH REPORT rpt_sumario

   CALL actualiza_etapa_archivo(gd_fecha_corte,  --fecha_corte
                                1,               --tipo_reporte
                                99,              --tipo_reg
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
REPORT multi_sie(lr_monto_nss)
   DEFINE lr_monto_nss  RECORD
             nss         CHAR(11),
             subcta      CHAR(2),
             siefore     SMALLINT,
             acciones    DECIMAL(22,6),
             pesos    DECIMAL(22,6)
          END RECORD

   DEFINE li_saldo SMALLINT

   DEFINE lc_msg_error CHAR(800)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

      ORDER EXTERNAL BY lr_monto_nss.nss

   FORMAT

   BEFORE GROUP OF lr_monto_nss.nss
      FOR i = 1 TO 20
          CASE i
               WHEN 1
                    LET gr_sub_saldos[i].subcta = "01"
               WHEN 2
                    LET gr_sub_saldos[i].subcta = "02"
               WHEN 3
                    LET gr_sub_saldos[i].subcta = "03"
               WHEN 4
                    LET gr_sub_saldos[i].subcta = "04"
               WHEN 5
                    LET gr_sub_saldos[i].subcta = "05"
               WHEN 6
                    LET gr_sub_saldos[i].subcta = "08"
               WHEN 7
                    LET gr_sub_saldos[i].subcta = "09"
               WHEN 8
                    LET gr_sub_saldos[i].subcta = "13"
               WHEN 9
                    LET gr_sub_saldos[i].subcta = "15"
               WHEN 10
                    LET gr_sub_saldos[i].subcta = "16"
               WHEN 11
                    LET gr_sub_saldos[i].subcta = "17"
               WHEN 12
                    LET gr_sub_saldos[i].subcta = "18"
               WHEN 13
                    LET gr_sub_saldos[i].subcta = "22"
               WHEN 14
                    LET gr_sub_saldos[i].subcta = "24"
               WHEN 15
                    LET gr_sub_saldos[i].subcta = "25"
               WHEN 16
                    LET gr_sub_saldos[i].subcta = "27"
               WHEN 17
               	    LET gr_sub_saldos[i].subcta = "28"
               WHEN 18
                    LET gr_sub_saldos[i].subcta = "29"
               WHEN 19
                    LET gr_sub_saldos[i].subcta = "30"
               WHEN 20
                    LET gr_sub_saldos[i].subcta = "31"
             END CASE
          FOR j = 1 TO 10
              LET gr_sub_saldos[i].gr_sie_saldos[j].siefore  = j
              LET gr_sub_saldos[i].gr_sie_saldos[j].acciones = 0
              LET gr_sub_saldos[i].gr_sie_saldos[j].pesos    = 0
          END FOR
      END FOR

      SELECT nss             ,
             tipo_solicitud  ,
             curp            ,
             rfc             ,
             fecha_nacimiento,
             estatus_cuenta  ,
             id_sector       ,
             criterio
      INTO   lr_reporte.nss,
             lr_reporte.tipo_solicitud,
             lr_reporte.curp,
             lr_reporte.rfc,
             lr_reporte.fena,
             lr_reporte.status_cta,
             lr_reporte.sector,
             lr_reporte.criterio_sie
      FROM   tmp_cuota_trasferencia a
      WHERE  a.nss         = lr_monto_nss.nss

   #Recibe saldos
   ON EVERY ROW
      CASE  lr_monto_nss.siefore
      	 WHEN 1
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[1].acciones = gr_tot_saldos[1].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[1].acciones = gr_tot_saldos[2].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[1].acciones = gr_tot_saldos[3].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[1].acciones = gr_tot_saldos[5].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[1].acciones = gr_tot_saldos[6].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[1].acciones = gr_tot_saldos[8].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[1].acciones = gr_tot_saldos[9].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[1].acciones = gr_tot_saldos[11].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[1].acciones = gr_tot_saldos[12].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[1].acciones = gr_tot_saldos[13].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[1].acciones = gr_tot_saldos[15].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[1].acciones = gr_tot_saldos[16].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[1].acciones = gr_tot_saldos[17].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[1].acciones = gr_tot_saldos[18].gr_sie_saldos[1].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[1].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[1].acciones = gr_tot_saldos[19].gr_sie_saldos[1].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 2
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[2].acciones = gr_tot_saldos[1].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[2].acciones = gr_tot_saldos[2].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[2].acciones = gr_tot_saldos[3].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[2].acciones = gr_tot_saldos[5].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[2].acciones = gr_tot_saldos[6].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[2].acciones = gr_tot_saldos[8].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[2].acciones = gr_tot_saldos[9].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[2].acciones = gr_tot_saldos[11].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[2].acciones = gr_tot_saldos[12].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[2].acciones = gr_tot_saldos[13].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[2].acciones = gr_tot_saldos[15].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[2].acciones = gr_tot_saldos[16].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[2].acciones = gr_tot_saldos[17].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[2].acciones = gr_tot_saldos[18].gr_sie_saldos[2].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[2].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[2].acciones = gr_tot_saldos[19].gr_sie_saldos[2].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 3
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[3].acciones = gr_tot_saldos[1].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[3].acciones = gr_tot_saldos[2].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[3].acciones = gr_tot_saldos[3].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[3].acciones = gr_tot_saldos[5].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[3].acciones = gr_tot_saldos[6].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[3].acciones = gr_tot_saldos[8].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[3].acciones = gr_tot_saldos[9].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[3].acciones = gr_tot_saldos[11].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[3].acciones = gr_tot_saldos[12].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[3].acciones = gr_tot_saldos[13].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[3].acciones = gr_tot_saldos[15].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[3].acciones = gr_tot_saldos[16].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[3].acciones = gr_tot_saldos[17].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[3].acciones = gr_tot_saldos[18].gr_sie_saldos[3].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[3].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[3].acciones = gr_tot_saldos[19].gr_sie_saldos[3].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 4
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[4].acciones = gr_tot_saldos[1].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[4].acciones = gr_tot_saldos[2].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[4].acciones = gr_tot_saldos[3].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[4].acciones = gr_tot_saldos[5].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[4].acciones = gr_tot_saldos[6].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[4].acciones = gr_tot_saldos[8].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[4].acciones = gr_tot_saldos[9].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[4].acciones = gr_tot_saldos[11].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[4].acciones = gr_tot_saldos[12].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[4].acciones = gr_tot_saldos[13].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[4].acciones = gr_tot_saldos[15].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[4].acciones = gr_tot_saldos[16].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[4].acciones = gr_tot_saldos[17].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[4].acciones = gr_tot_saldos[18].gr_sie_saldos[4].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[4].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[4].acciones = gr_tot_saldos[19].gr_sie_saldos[4].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 5
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[5].acciones = gr_tot_saldos[1].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[5].acciones = gr_tot_saldos[2].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[5].acciones = gr_tot_saldos[3].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[5].acciones = gr_tot_saldos[5].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[5].acciones = gr_tot_saldos[6].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[5].acciones = gr_tot_saldos[8].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[5].acciones = gr_tot_saldos[9].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[5].acciones = gr_tot_saldos[11].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[5].acciones = gr_tot_saldos[12].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[5].acciones = gr_tot_saldos[13].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[5].acciones = gr_tot_saldos[15].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[5].acciones = gr_tot_saldos[16].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[5].acciones = gr_tot_saldos[17].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[5].acciones = gr_tot_saldos[18].gr_sie_saldos[5].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[5].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[5].acciones = gr_tot_saldos[19].gr_sie_saldos[5].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 6 --SIEFORE ADICIONAL LARGO PLAZO
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[7].acciones = gr_tot_saldos[1].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[7].acciones = gr_tot_saldos[2].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[7].acciones = gr_tot_saldos[3].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[7].acciones = gr_tot_saldos[5].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[7].acciones = gr_tot_saldos[6].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[7].acciones = gr_tot_saldos[8].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[7].acciones = gr_tot_saldos[9].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[7].acciones = gr_tot_saldos[11].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[7].acciones = gr_tot_saldos[12].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[7].acciones = gr_tot_saldos[13].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[7].acciones = gr_tot_saldos[15].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[7].acciones = gr_tot_saldos[16].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[7].acciones = gr_tot_saldos[17].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[7].acciones = gr_tot_saldos[18].gr_sie_saldos[7].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[7].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[7].acciones = gr_tot_saldos[19].gr_sie_saldos[7].acciones + lr_monto_nss.acciones
            END CASE
      	 WHEN 7 --SIEFORE ADICIONAL CORTO PLAZO
      	 	  CASE lr_monto_nss.subcta
               WHEN "01"
               	  LET gr_sub_saldos[1].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                  LET gr_tot_saldos[1].gr_sie_saldos[6].acciones = gr_tot_saldos[1].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "02"
               	 LET gr_sub_saldos[2].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[2].gr_sie_saldos[6].acciones = gr_tot_saldos[2].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "03"
               	 LET gr_sub_saldos[3].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[3].gr_sie_saldos[6].acciones = gr_tot_saldos[3].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "05"
               	 LET gr_sub_saldos[5].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[5].gr_sie_saldos[6].acciones = gr_tot_saldos[5].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "08"
               	 LET gr_sub_saldos[6].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[6].gr_sie_saldos[6].acciones = gr_tot_saldos[6].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "13"
               	 LET gr_sub_saldos[8].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[8].gr_sie_saldos[6].acciones = gr_tot_saldos[8].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "15"
               	 LET gr_sub_saldos[9].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[9].gr_sie_saldos[6].acciones = gr_tot_saldos[9].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "17"
               	 LET gr_sub_saldos[11].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[11].gr_sie_saldos[6].acciones = gr_tot_saldos[11].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "18"
               	 LET gr_sub_saldos[12].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[12].gr_sie_saldos[6].acciones = gr_tot_saldos[12].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "22"
               	 LET gr_sub_saldos[13].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[13].gr_sie_saldos[6].acciones = gr_tot_saldos[13].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "25"
               	 LET gr_sub_saldos[15].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[15].gr_sie_saldos[6].acciones = gr_tot_saldos[15].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "27"
               	 LET gr_sub_saldos[16].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[16].gr_sie_saldos[6].acciones = gr_tot_saldos[16].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "28"
               	 LET gr_sub_saldos[17].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[17].gr_sie_saldos[6].acciones = gr_tot_saldos[17].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "29"
               	 LET gr_sub_saldos[18].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[18].gr_sie_saldos[6].acciones = gr_tot_saldos[18].gr_sie_saldos[6].acciones + lr_monto_nss.acciones

               WHEN "30"
               	 LET gr_sub_saldos[19].gr_sie_saldos[6].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[19].gr_sie_saldos[6].acciones = gr_tot_saldos[19].gr_sie_saldos[6].acciones + lr_monto_nss.acciones
            END CASE
          WHEN 11 --VIVIENDA INFONAVIT
            CASE lr_monto_nss.subcta
               WHEN "04"
               	 LET gr_sub_saldos[4].gr_sie_saldos[8].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[4].gr_sie_saldos[8].acciones = gr_tot_saldos[4].gr_sie_saldos[8].acciones + lr_monto_nss.acciones

               WHEN "09"
               	 LET gr_sub_saldos[7].gr_sie_saldos[8].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[7].gr_sie_saldos[8].acciones = gr_tot_saldos[7].gr_sie_saldos[8].acciones + lr_monto_nss.acciones
             END CASE
          WHEN 12 --VIVIENDA FOVISSSTE
            CASE lr_monto_nss.subcta

               WHEN "16"
               	 LET gr_sub_saldos[10].gr_sie_saldos[9].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[10].gr_sie_saldos[9].acciones = gr_tot_saldos[10].gr_sie_saldos[9].acciones + lr_monto_nss.acciones

               WHEN "24"
               	 LET gr_sub_saldos[14].gr_sie_saldos[9].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[14].gr_sie_saldos[9].acciones = gr_tot_saldos[14].gr_sie_saldos[9].acciones + lr_monto_nss.acciones
            END CASE
          WHEN 13 --BONO DE PENSION
            CASE lr_monto_nss.subcta
               WHEN "31"
               	 LET gr_sub_saldos[20].gr_sie_saldos[10].acciones = lr_monto_nss.acciones

                 LET gr_tot_saldos[20].gr_sie_saldos[10].acciones = gr_tot_saldos[20].gr_sie_saldos[10].acciones + lr_monto_nss.acciones

            END CASE
      END CASE

      #Formatea
      AFTER GROUP OF lr_monto_nss.nss
      	 IF lr_monto_nss.nss[1] = "I" THEN
      	    LET lr_monto_nss.nss = "00000000000"
      	 END IF

      	 LET li_saldo = 0

      	 FOR i = 1 TO 20
             FOR j = 1 TO 10
             	   IF gr_sub_saldos[i].gr_sie_saldos[j].acciones = 0 THEN
             	   	  LET li_saldo = li_saldo + 1
             	   END IF
             END FOR
         END FOR

         IF li_saldo = 200 THEN
         	  PRINT COLUMN 001, "02",
                  COLUMN 003, lr_monto_nss.nss        ,
                  COLUMN 014, lr_reporte.curp         ,
                  COLUMN 032, lr_reporte.rfc          ,
                  COLUMN 045, lr_reporte.status_cta   USING "&",
                  COLUMN 046, lr_reporte.sector       USING "&",
                  COLUMN 047, gr_sub_saldos[1].subcta ,
                  COLUMN 049, "0000000000000000"  ,
                  COLUMN 065, "0000000000000000"  ,
                  COLUMN 081, "0000000000000000"  ,
                  COLUMN 097, "0000000000000000"  ,
                  COLUMN 113, "0000000000000000"  ,
                  COLUMN 129, "0000000000000000"  ,
                  COLUMN 145, "0000000000000000"  ,
                  COLUMN 161, "000000000000"      , --INFONAVIT
                  COLUMN 173, "0000000000000000"  , --FOVISSSTE
                  COLUMN 189, "0000000000000000"    --BONO
                              #&&&&&&&&&&&&&&&&
            LET gi_registros = gi_registros + 1
         ELSE
         #Imprimir totas las subcuentas
            FOR i = 1 TO 20
                FOR j = 1 TO 10
                    IF gr_sub_saldos[i].gr_sie_saldos[j].acciones < 0 THEN

                    	 IF gi_afore = 574 THEN
                    	    LET lc_msg_error = "ERROR: SALDO NEGATIVO. Nss: ", lr_monto_nss.nss, " ",
                    	                       "CURP: ", lr_reporte.curp , " ",
                    	                       "SUBCUENTA: ", gr_sub_saldos[i].subcta, " ",
                    	                       "SALDO: ", gr_sub_saldos[i].gr_sie_saldos[j].acciones

                    	    CALL errorlog(lc_msg_error CLIPPED)

                    	    DISPLAY "***************ERROR: SALDO NEGATIVO***************"
                    	    DISPLAY "Nss: ", lr_monto_nss.nss
                          DISPLAY "CURP: ", lr_reporte.curp
                          DISPLAY "SUBCUENTA: ", gr_sub_saldos[i].subcta
                          DISPLAY "SALDO: ", gr_sub_saldos[i].gr_sie_saldos[j].acciones
                       END IF

                       LET lr_saldos[j].c_acciones = gr_sub_saldos[i].gr_sie_saldos[j].acciones * 1000000 USING "-&&&&&&&&&&&&&&&"
                    ELSE
                       LET lr_saldos[j].c_acciones = gr_sub_saldos[i].gr_sie_saldos[j].acciones * 1000000 USING "&&&&&&&&&&&&&&&&"
                    END IF
                END FOR

                PRINT COLUMN 001, "02",
                      COLUMN 003, lr_monto_nss.nss        ,
                      COLUMN 014, lr_reporte.curp         ,
                      COLUMN 032, lr_reporte.rfc          ,
                      COLUMN 045, lr_reporte.status_cta   USING "&",
                      COLUMN 046, lr_reporte.sector       USING "&",
                      COLUMN 047, gr_sub_saldos[i].subcta ,
                      COLUMN 049, lr_saldos[1].c_acciones   ,
                      COLUMN 065, lr_saldos[2].c_acciones   ,
                      COLUMN 081, lr_saldos[3].c_acciones   ,
                      COLUMN 097, lr_saldos[4].c_acciones   ,
                      COLUMN 113, lr_saldos[5].c_acciones   ,
                      COLUMN 129, lr_saldos[6].c_acciones   ,    --CP
                      COLUMN 145, lr_saldos[7].c_acciones   ,    --LP
                      COLUMN 161, lr_saldos[8].c_acciones[1,12], --INFONAVIT
                      COLUMN 173, lr_saldos[9].c_acciones   ,    --FOVISSSTE
                      COLUMN 189, lr_saldos[10].c_acciones       --BONO
                LET gi_registros = gi_registros + 1
            END FOR
         END IF
END REPORT
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
                 " AND    proceso_cod = 'CTANX05' ",
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
                 " AND    proceso_cod   = 'CTANX05'",
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
      ld_precio      DECIMAL(7,6)


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

   --CALL get_fecha_archivo(gd_fecha_corte) RETURNING lr_archivo.fecha_envio

   --LET lr_archivo.fecha_envio = "09/04/2009"
   LET lr_archivo.fecha_envio = gd_fecha_corte

   LET lc_archivo_final = lr_archivo.fecha_envio  USING "YYYYMMDD", "_",
                          "AF_",
                          lr_archivo.codigo_afore USING "&&&", "_",
                          "016_",
                          "001_edad_reg"

   LET lc_encabezado = "Op71_enc_reg_",
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
REPORT rpt_sumario()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      FOR i = 1 TO 20
          FOR j = 1 TO 10

            PRINT COLUMN 01, gr_tot_saldos[i].subcta," SB",
                             gr_tot_saldos[i].gr_sie_saldos[j].siefore USING "&&"," ",
                             gr_tot_saldos[i].gr_sie_saldos[j].acciones USING "&&&&&&&&&&&&&&&&&&&.&&&&&&"
                             {," ",
                             gr_tot_saldos[i].gr_sie_saldos[j].pesos    USING "&&&&&&&&&&&&&&&&&&&.&&&&&&"}
         END FOR
       END FOR

END REPORT
################################################################################
FUNCTION get_fecha_envio()
   DEFINE ld_fecha_envio DATE
   DEFINE li_cont        SMALLINT
   DEFINE li_dia         SMALLINT
   DEFINE li_dias_viernes SMALLINT

   LET ld_fecha_envio = gd_fecha_corte
   LET li_dias_viernes = 0

   FOR li_cont = 1 TO 20
   	  LET ld_fecha_envio = ld_fecha_envio + 1 UNITS DAY

   	  IF WEEKDAY (ld_fecha_envio) = 5 THEN --viernes
   	  	 LET li_dias_viernes = li_dias_viernes + 1
   	  END IF

   	  IF li_dias_viernes >= 2 THEN
   	  	 EXIT FOR
   	  END IF
   END FOR

   RETURN ld_fecha_envio
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
FUNCTION get_fecha_archivo(ld_fecha_corte)
   DEFINE ld_fecha_corte   DATE
   DEFINE ld_fecha_archivo DATE
   DEFINE li_mes_corte     SMALLINT
   DEFINE li_mes_archivo   SMALLINT
   DEFINE li_dia_archivo   SMALLINT

   LET li_mes_corte = MONTH (ld_fecha_corte)
   LET li_mes_archivo = li_mes_corte + 1

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
   END CASE

   LET ld_fecha_archivo = MDY(li_mes_archivo,li_dia_archivo,YEAR(ld_fecha_corte))

   RETURN ld_fecha_archivo
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

   LET ld_fecha_viv  = MDY(MONTH(gd_fecha_corte),1,YEAR(gd_fecha_corte))
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