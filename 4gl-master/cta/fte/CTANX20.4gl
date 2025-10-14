################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX20    => ESTADO DE CUENTA                                       #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 19 FEBRERO 2010                                        #
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

 DEFINE  gr_sub_saldos ARRAY[20] OF RECORD
         subcta           CHAR(2),
            gr_sie_saldos ARRAY[10] OF RECORD
               siefore       SMALLINT,
               precio_dia    DECIMAL(22,6),
               acciones      DECIMAL(22,6),
               pesos         DECIMAL(22,6)
            END RECORD
         END RECORD

 DEFINE  gr_tot_saldos ARRAY[20] OF RECORD
         subcta           CHAR(2),
            gr_sie_saldos ARRAY[10] OF RECORD
               siefore       SMALLINT,
               acciones      DECIMAL(22,6),
               pesos         DECIMAL(22,6)
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

   DEFINE gr_reporte RECORD
            nss            CHAR(11),
            curp           CHAR(18),
            rfc            CHAR(13),
            estado         SMALLINT,
            cod_postal     INTEGER,
            status_cta     SMALLINT,
            sector         SMALLINT,
            precio1        DECIMAL(7,6),
            precio2        DECIMAL(7,6),
            precio3        DECIMAL(7,6),
            precio4        DECIMAL(7,6),
            precio5        DECIMAL(7,6)
   END RECORD

   DEFINE lr_saldos RECORD
             c_acciones   CHAR(16),
             c_pesos      CHAR(18)
          END RECORD

   DEFINE gr_seg_modulo RECORD
   	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
   END RECORD

   DEFINE gs_tipo_reporte SMALLINT

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO DE ANEXO 71",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX20.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de anexo 71")

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

   LET gr_seg_modulo.modulo_cod = "ctx"

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   LET gs_tipo_reporte = 2 --CONSAR

   CALL inicializa()

   CALL archivo()

   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
################################################################################
FUNCTION inicializa()
   DEFINE lc_sql CHAR(500)

   #PRECIOS DE ACCION
   DECLARE c_precio CURSOR FOR
   SELECT a.codigo_siefore,
          NVL(a.precio_del_dia,0)
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
       	    #Inicializa arreglo de subcuentas
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

       #i: Subcuenta
       #j: Siefore

       FOR j = 1 TO 10
       #Inicializa arreglo de siefores
       	  CASE j  --cve_programa                                      cve_consar
       	  	WHEN  6
       	  		IF gi_afore = 578 THEN --PENSIONISSSTE
       	  			 LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 10  --BANXICO
       	  		ELSE
       	  		   LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 7   --SIEFORE ADICIONAL DE CORTO PLAZO
       	  	  END IF
       	  	WHEN  7   LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 6   --SIEFORE ADICIONAL DE LARGO PLAZO
       	  	WHEN  8   LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 11  --INFONAVIT
       	    WHEN  9   LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 12  --FOVISSSTE
       	  	WHEN 10   LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = 13  --BONO
       	  	OTHERWISE LET gr_tot_saldos[i].gr_sie_saldos[j].siefore = j
       	  END CASE

           LET gr_tot_saldos[i].gr_sie_saldos[j].acciones = 0
           LET gr_tot_saldos[i].gr_sie_saldos[j].pesos    = 0
       END FOR
   END FOR

   LET lc_sql = " SELECT fn_es_numero( ? ) + ",
                "        fn_es_numero( ? ) + ",
                "        fn_es_numero( ? ) + ",
                "        fn_es_numero( ? ) + ",
                "        fn_es_numero( ? )   ",
                " FROM   safre_af:systables  ",
                " WHERE  tabid = 1           "

   PREPARE valida_codpos FROM lc_sql
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
       "CTANX20",         -- proceso_cod
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
          lc_comando_cat   CHAR(15000),
          lc_comando_rm    CHAR(15000),
          lc_comando_chmod CHAR(15000),
          lc_encabezado    CHAR(80),
          lc_detalle03     CHAR(80),
          lc_archivo_final CHAR(80)

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

   DEFINE li_consecutivo SMALLINT
   DEFINE li_registros_ini,
          li_registros_archivo INTEGER

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   #Borrar registros de control
   DELETE
   FROM   safre_af:ctr_anexo71
   WHERE  fecha_corte  = gd_fecha_corte
   AND    tipo_reporte = gs_tipo_reporte

   CALL nombra_archivo() RETURNING lc_encabezado,
                                   lc_archivo_final

   LET lc_encabezado  = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado CLIPPED
   LET lc_nomarch_ini = gr_seg_modulo.ruta_envio CLIPPED,"/", "anexo71_edad"

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_encabezado CLIPPED

   LET lc_comando_chmod = "chmod 777 ",
                          lc_encabezado CLIPPED, ";"

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldos_cns71_edad
   WHENEVER ERROR STOP

  SQL

   CREATE TABLE tmp_saldos_cns71_edad
   ( estatus_cuenta SMALLINT,
     nss CHAR(11),
     subcta CHAR(02),
     siefore  SMALLINT,
     acciones DECIMAL(22,6),
     pesos    DECIMAL(22,6)
   ) IN tmp_dbs2;

  END SQL

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   EXECUTE prep_prioridadh

   INSERT INTO tmp_saldos_cns71_edad
            SELECT c.estatus_cuenta,
                   c.nss,
                   b.subct_cns,
                   a.siefore,
                   SUM(a.monto_en_acciones),
                   SUM(a.monto_en_pesos)
            FROM   cta_cuota_anexo71_nss  c,
                   tmp_saldo71_edad      a,
                   safre_af:tab_subcuenta b
            WHERE  a.nss = c.nss
            AND    a.subcuenta = b.subct_cod
            GROUP BY 1,2,3,4

   --No tienen saldo y se imprime un solo registro
   --La siefore se obtiene del regimen al imprimir el archivo
   INSERT INTO tmp_saldos_cns71_edad
            SELECT c.estatus_cuenta        ,
                   c.nss                   ,
                   '01'                    ,
                   '1'                     ,
                   '0'                     ,
                   '0'
            FROM   cta_cuota_anexo71_nss  c
            WHERE  c.nss NOT IN (SELECT nss FROM tmp_saldo71_edad)

   SQL
      CREATE INDEX ix_tmp_saldos_cns71_edad1 ON tmp_saldos_cns71_edad ( nss,subcta) in tmp_dbs2;
   END SQL
   SQL
      CREATE INDEX ix_tmp_saldos_cns71_edad2 ON tmp_saldos_cns71_edad ( estatus_cuenta ) ;
   END SQL

   UPDATE STATISTICS FOR TABLE tmp_saldos_cns71_edad

   FOR li_estatus_cuenta = 0 TO 4
         LET lc_nomarch = lc_nomarch_ini CLIPPED ,
                          "_sc",
                          li_estatus_cuenta USING "&",
                          "_",
                          gd_fecha_corte USING "DDMMYYYY"

          #/ruta/anexo71_edad_sc0_DDMMYYYY
          #/ruta/anexo71_edad_enc_DDMMYYYY
          #/ruta/anexo71_edad_det03_DDMMYYYY
          #/ruta/YYYYMMDD_AFedad_afore_016_00000.004

          #Concatenar
          LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                               lc_nomarch     CLIPPED

          #Permisos
          LET lc_comando_chmod = lc_comando_chmod CLIPPED,
                                 "chmod 777 ", lc_nomarch CLIPPED, ";"

          #Borrar
          LET lc_comando_rm = lc_comando_rm CLIPPED, " rm ",
                              lc_nomarch    CLIPPED, ";"

          START REPORT multi_sie TO lc_nomarch
               DECLARE cur_siefore_subcta CURSOR FOR
               SELECT a.nss     ,
                      a.subcta  ,
                      a.siefore ,
                      a.acciones,
                      a.pesos
               FROM   tmp_saldos_cns71_edad a
               WHERE  a.estatus_cuenta = li_estatus_cuenta
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
                  OUTPUT TO REPORT multi_sie(lr_monto_nss.*,li_estatus_cuenta)

               END FOREACH
            FINISH REPORT multi_sie
   END FOR

#DETALLE 03
   LET lc_detalle03 = gr_seg_modulo.ruta_envio CLIPPED,"/",
                      "anexo71_edad_det03_",
                      gd_fecha_corte USING "DDMMYYYY"

   CALL detalle03(lc_detalle03)

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_detalle03   CLIPPED

   #Encabezado
   CALL encabezado(lc_encabezado)

   #Archivo final
   LET lc_nomarch = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final CLIPPED

   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_nomarch CLIPPED
   RUN lc_comando_cat

   LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                          lc_nomarch CLIPPED, ";"

   --RUN lc_comando_rm

   --**TOTALES***********************************************************
   LET lc_totales = gr_seg_modulo.ruta_envio CLIPPED,"/",
                    "anexo_sumario_cns_", gd_fecha_corte USING "DDMMYYYY"

   START REPORT rpt_sumario TO lc_totales
      OUTPUT TO REPORT rpt_sumario()
   FINISH REPORT rpt_sumario

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   EXECUTE prep_prioridadl

   LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                          lc_totales CLIPPED, ";"

   #Permiso a todos los archivos
   RUN lc_comando_chmod

END FUNCTION
################################################################################
REPORT multi_sie(lr_monto_nss,v_estatus_cuenta)
   DEFINE lr_monto_nss  RECORD
             nss         CHAR(11),
             subcta      CHAR(2),
             siefore     SMALLINT,
             acciones    DECIMAL(22,6),
             pesos       DECIMAL(22,6)
          END RECORD,
             v_estatus_cuenta SMALLINT

   DEFINE li_saldo SMALLINT

   DEFINE lc_msg_error CHAR(800),
          lc_codpos    CHAR(05)

   DEFINE li_cod_siefore SMALLINT,
          li_valida      SMALLINT

   DEFINE ld_acciones_2 DECIMAL(22,2),
          ld_acciones_6 DECIMAL(22,6)

   DEFINE lc_codpos1,
          lc_codpos2,
          lc_codpos3,
          lc_codpos4,
          lc_codpos5 CHAR(1)

   DEFINE lc_nti CHAR(11)

   DEFINE ls_tipo_solicitud SMALLINT

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

      INITIALIZE lc_codpos TO NULL

      SELECT nss           ,
             curp          ,
             rfc           ,
             estado        ,
             cod_postal    ,
             estatus_cuenta,
             id_sector     ,
             '0'
      INTO   gr_reporte.nss       ,
             gr_reporte.curp      ,
             gr_reporte.rfc       ,
             gr_reporte.estado    ,
             lc_codpos            ,
             gr_reporte.status_cta,
             gr_reporte.sector    ,
             li_valida
      FROM   cta_cuota_anexo71_nss  a
      WHERE  a.nss = lr_monto_nss.nss

      LET lc_codpos1 = lc_codpos[1]
      LET lc_codpos2 = lc_codpos[2]
      LET lc_codpos3 = lc_codpos[3]
      LET lc_codpos4 = lc_codpos[4]
      LET lc_codpos5 = lc_codpos[5]

      #Validar código postal
      IF lc_codpos IS NOT NULL THEN
         EXECUTE valida_codpos USING lc_codpos1,
                                     lc_codpos2,
                                     lc_codpos3,
                                     lc_codpos4,
                                     lc_codpos5
                               INTO  li_valida

         IF li_valida <> 5 THEN
         	 LET gr_reporte.cod_postal = 0
         ELSE
         	 LET gr_reporte.cod_postal = lc_codpos
         END IF
      ELSE
      	 LET gr_reporte.cod_postal = 0
      END IF

      INITIALIZE ls_tipo_solicitud TO NULL

      SELECT tipo_solicitud
      INTO   ls_tipo_solicitud
      FROM   safre_af:afi_mae_afiliado
      WHERE  n_seguro = lr_monto_nss.nss

      IF ls_tipo_solicitud = 5 THEN
      	 LET gr_reporte.cod_postal = 0
      	 LET gr_reporte.estado     = 0
      END IF

   #Recibe saldos
   ON EVERY ROW
      IF gi_afore = 578 THEN --PENSIONISSSTE
      	 IF lr_monto_nss.acciones < 0 AND
      	 	  lr_monto_nss.acciones >= (-1) THEN
      	 	  LET lr_monto_nss.acciones = 0
      	 END IF
      END IF

      IF lr_monto_nss.siefore = 11 THEN
      	 #Los saldos de vivienda se reportan a 2 decimales
         #Valores (-0.01 a 0.01)
         LET ld_acciones_2 = lr_monto_nss.acciones * 1

         IF ld_acciones_2 > -0.01 AND
            ld_acciones_2 <  0.01 THEN

            LET lr_monto_nss.acciones = 0
         END IF
      END IF

      IF gi_afore <> 578 THEN --PENSIONISSSTE
      	 IF lr_monto_nss.acciones < 0 THEN
            LET lc_msg_error = "ERROR: SALDO NEGATIVO. Nss: ", lr_monto_nss.nss, " ",
                       	      "CURP: ",                       gr_reporte.curp , " ",
                       	      "SUBCUENTA (SAFRE): ",          lr_monto_nss.subcta, " ",
                       	      "SIEFORE (SAFRE):   ",          lr_monto_nss.siefore, " ",
                       	      "SALDO: ",                      lr_monto_nss.acciones

            CALL errorlog(lc_msg_error CLIPPED)

            LET lr_monto_nss.acciones = 0
         END IF
      END IF

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
      	 --WHEN 7 --SIEFORE ADICIONAL CORTO PLAZO
      	 WHEN 0 ----PENSIONISSSTE
      	    #Ninguna afore tiene SIEFORE ADICIONAL CORTO PLAZO
      	    #En el caso de PENSIONISSSTE se utiliza la posicion 6 del arreglo de siefores
      	    #para guardar los saldos de SAR ISSSTE en Banxico (Siefore 0)
      	    IF gi_afore = 578 THEN --PENSIONISSSTE
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
            END IF
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
      	 LET lc_nti           = lr_monto_nss.nss

      	 IF lr_monto_nss.nss[1] = "I" THEN
      	    LET lr_monto_nss.nss = "00000000000"
      	 END IF

      	 LET li_saldo = 0

      	 FOR i = 1 TO 20
             FOR j = 1 TO 10
             	  --VIVIENDAS Se reportan 2 decimales
             	  --Pero la validacion re realiza desde que se recibe el monto original
             	  IF gr_sub_saldos[i].gr_sie_saldos[j].acciones = 0 THEN
             	      LET li_saldo = li_saldo + 1
             	  END IF
             END FOR
         END FOR

         IF li_saldo = 200 THEN
         	  IF gi_afore = 578 THEN --PENSIONISSSTE
         	     LET gr_reporte.status_cta = 2
         	  END IF

         	  INITIALIZE li_cod_siefore TO NULL

            #Obtener siefore del trabajador
            IF gi_afore = 578 THEN --PENSIONISSSTE
         	     SELECT codigo_siefore
         	     INTO   li_cod_siefore
         	     FROM   safre_af:cta_regimen
         	     WHERE  nss       = lc_nti
         	     AND    subcuenta = 30
         	  ELSE
         	  	 SELECT codigo_siefore
         	     INTO   li_cod_siefore
         	     FROM   safre_af:cta_regimen
         	     WHERE  nss       = lc_nti
         	     AND    subcuenta = 1
         	  END IF

         	  IF li_cod_siefore IS NULL THEN
         	  	 LET li_cod_siefore = 0
         	  END IF

         	  IF gi_afore = 578 THEN --PENSIONISSSTE
         	     IF li_cod_siefore = 0 THEN
         	     	  LET li_cod_siefore = 10
         	     	  LET gr_reporte.status_cta = 4
         	     END IF
         	  END IF

         	  PRINT COLUMN 001, "02",
         	        COLUMN 003, lr_monto_nss.nss        ,
         	        COLUMN 014, gr_reporte.curp         ,
         	        COLUMN 032, gr_reporte.rfc          ,
         	        COLUMN 045, gr_reporte.estado       USING "&&",
         	        COLUMN 047, gr_reporte.cod_postal   USING "&&&&&",
         	        COLUMN 052, gr_reporte.status_cta   USING "&",
         	        COLUMN 053, gr_reporte.sector       USING "&",
         	        COLUMN 054, gr_sub_saldos[1].subcta ,
         	        COLUMN 056, li_cod_siefore          USING "&&",
         	        COLUMN 058, "0000000000000000"

            LET gi_registros = gi_registros + 1
         ELSE
         	  #i: Subcuentas
         	  #j: Siefores

            #i: Subcuentas
            FOR i = 1 TO 20

         	  	 #j: Siefores
         	  	 FOR j = 1 TO 10

         	  	 	  #Solo se procesan las subcuentas con saldo
         	  	 	  IF gr_sub_saldos[i].gr_sie_saldos[j].acciones <> 0 THEN

         	  	 	  	 #Formatear siefore
         	  	 	  	 #Formatear siefore
         	  	 	  	 CASE j --siefore
         	  	 	  	 	 WHEN  6
         	  	 	  	 	 	     IF gi_afore = 578 THEN --PENSIONISSSTE
         	  	 	  	 	 	     	  LET li_cod_siefore = 10 --SALDO EN PESOS EN BANXICO
         	  	 	  	 	       ELSE
         	  	 	  	 	        	LET li_cod_siefore = 7 --CODIGO CORTO PLAZO
         	  	 	  	 	       END IF
         	  	 	  	 	 WHEN  7 LET li_cod_siefore = 6 --CODIGO LARGO PLAZO
         	  	 	  	 	 WHEN  8 LET li_cod_siefore = 0 --INFONAVIT
         	  	 	  	 	 WHEN  9 LET li_cod_siefore = 9 --FOVISSSTE
         	  	 	  	 	 WHEN 10 LET li_cod_siefore = 8 --BONO
         	  	 	  	 	 OTHERWISE LET li_cod_siefore = j --SIEFORES BASICAS
         	  	 	  	 END CASE

         	  	 	  	 #Formatear montos de acuerdo a la subcuenta
         	  	 	  	 CASE
         	  	 	  	 	 WHEN i =  4 OR  --infonavit en la cve_program
         	  	 	  	 	 	    i =  7       --infonavit en la cve_program
         	  	 	  	 	 	    --i = 10 OR  --fovissste en la cve_programa
         	  	 	  	 	 	    --i = 14     --fovissste en la cve_programa

         	  	 	  	 	 	  #Pasar a 2 decimales (adicionalmente se redondea)
         	  	 	  	 	 	  LET ld_acciones_2 = gr_sub_saldos[i].gr_sie_saldos[j].acciones * 1
         	  	 	  	 	 	  LET ld_acciones_6 = ld_acciones_2 * 1
         	  	 	  	 	 OTHERWISE
         	  	 	  	 	    LET ld_acciones_6 = gr_sub_saldos[i].gr_sie_saldos[j].acciones * 1
         	  	 	  	 END CASE

         	  	 	  	 IF gr_sub_saldos[i].gr_sie_saldos[j].acciones < 0 THEN

         	  	 	  	 	  #Mandar mensaje
         	  	 	  	 	  LET lc_msg_error = "ERROR: SALDO NEGATIVO. Nss: ", lr_monto_nss.nss, " ",
                    	                     "CURP: ", gr_reporte.curp , " ",
                    	                     "SUBCUENTA: ", gr_sub_saldos[i].subcta, " ",
                    	                     "SIEFORE:   ", li_cod_siefore, " ",
                    	                     "SALDO: ", gr_sub_saldos[i].gr_sie_saldos[j].acciones

                    	  CALL errorlog(lc_msg_error CLIPPED)

         	  	 	  	 	  LET lr_saldos.c_acciones = ld_acciones_6 * 1000000 USING "-&&&&&&&&&&&&&&&"
         	  	 	  	 ELSE
                        LET lr_saldos.c_acciones = ld_acciones_6 * 1000000 USING "&&&&&&&&&&&&&&&&"
         	  	 	  	 END IF

         	  	 	  	 --Al redondear vivienda el saldo puede hacerse cero
         	  	 	  	 IF ld_acciones_6 <> 0 THEN
         	  	 	  	    #Imprimir Detalle 02
         	  	 	  	    PRINT COLUMN 001, "02",
         	                    COLUMN 003, lr_monto_nss.nss        ,
         	                    COLUMN 014, gr_reporte.curp         ,
         	                    COLUMN 032, gr_reporte.rfc          ,
         	                    COLUMN 045, gr_reporte.estado       USING "&&",
         	                    COLUMN 047, gr_reporte.cod_postal   USING "&&&&&",
         	                    COLUMN 052, gr_reporte.status_cta   USING "&",
         	                    COLUMN 053, gr_reporte.sector       USING "&",
         	                    COLUMN 054, gr_sub_saldos[i].subcta ,
         	                    COLUMN 056, li_cod_siefore          USING "&&",
         	                    COLUMN 058, lr_saldos.c_acciones

         	              LET gi_registros = gi_registros + 1
         	           END IF
         	  	 	  END IF --<>0
         	     END FOR
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
                 " AND    proceso_cod = 'CTANX20' ",
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
                 " AND    proceso_cod   = 'CTANX20'",
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
   	         fecha_envio  DATE    ,
   	         tipo_inf     SMALLINT
   	      END RECORD

   DEFINE ld_fecha_envio DATE

   LET lr_archivo.tipo_inf    = 1 --5 SIEFORES

   LET lc_archivo_final = gd_fecha_corte  USING "YYYYMMDD", "_",
                          "AFedad_",
                          gi_afore        USING "&&&", "_",
                          "016_"  , --Clave proceso
                          "00000.", --Consecutivo
                          "004"     --Clave del formato segun CONSAR

   LET lc_encabezado = "anexo71_edad_enc",
                       gd_fecha_corte USING "DDMMYYYY"

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

   LET lr_encabezado.codigo_afore = gi_afore
   LET lr_encabezado.fecha_envio  = MDY (MONTH(gd_fecha_corte), 1, YEAR(gd_fecha_corte))

   IF gi_afore = 578 THEN --PENSIONISSSTE
   	  LET lr_encabezado.tipo_inf = 2 --Saldos Pensionissste
   ELSE
   	  LET lr_encabezado.tipo_inf = 1 --Saldos Afores
   END IF

   START REPORT rpt_encabezado TO lc_encabezado
      OUTPUT TO REPORT rpt_encabezado(lr_encabezado.*)
   FINISH REPORT rpt_encabezado
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
      PRINT COLUMN 01, "01",                                          --Tipo registro
                       "01",                                          --Tipo entidad 01 AFORE
                       lr_encabezado.codigo_afore USING "&&&",        --Clave Afore
                       lr_encabezado.fecha_envio  USING "YYYYMMDD",   --Fecha informacion
                       lr_encabezado.tipo_inf     USING "&",
                       gi_registros        + 1    USING "&&&&&&&&&&",
                       47 SPACES
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

   IF gi_afore = 568 OR   --coppel
   	  gi_afore = 562 THEN --invercap
   	  INSERT INTO safre_af:ctr_anexo71 VALUES(NULL, lr_ctr_anexo71.*)
   ELSE
   	  INSERT INTO safre_af:ctr_anexo71 VALUES(lr_ctr_anexo71.*)
   END IF
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
FUNCTION get_consecutivo(ld_fecha_corte, li_tipo_reporte, li_tipo_reg)
   DEFINE ld_fecha_corte DATE
   DEFINE li_tipo_reporte,
          li_tipo_reg    SMALLINT

   DEFINE li_consecutivo INTEGER

   INITIALIZE li_consecutivo TO NULL

   SELECT MAX(consecutivo)
   INTO   li_consecutivo
   FROM   safre_af:ctr_anexo71
   WHERE  fecha_corte  = ld_fecha_corte
   AND    tipo_reporte = li_tipo_reporte
   AND    tipo_reg     = li_tipo_reg

   IF li_consecutivo IS NULL THEN
   	  LET li_consecutivo = 1
   ELSE
   	  LET li_consecutivo = li_consecutivo + 1
   END IF

   RETURN li_consecutivo
END FUNCTION
################################################################################
FUNCTION detalle03(lc_detalle03)
   DEFINE lc_detalle03    CHAR(80),
          lc_comando      CHAR(100)

   DEFINE lr_detalle03 RECORD
   	         siefore   SMALLINT,
   	         precio    DECIMAL(19,14)
   	      END RECORD

   DEFINE li_siefore      SMALLINT
   DEFINE li_registros    INTEGER

   DEFINE ld_fecha_viv    DATE
   DEFINE ld_fecha_paso   DATE
   DEFINE ld_ultimo_habil DATE

   LET li_registros = 0

   LET ld_fecha_viv  = MDY(MONTH(gd_fecha_corte),1,YEAR(gd_fecha_corte))

   --LET ld_fecha_paso = ld_fecha_viv + 1 UNITS MONTH
   --CALL habil_anterior(ld_fecha_paso, 1) RETURNING ld_ultimo_habil

   --DISPLAY "ld_fecha_viv   : ", ld_fecha_viv    USING "DD/MM/YYYY"
   --DISPLAY "ld_ultimo_habil: ", ld_ultimo_habil USING "DD/MM/YYYY"

   DECLARE cur_siefore CURSOR FOR
   SELECT codigo_siefore
   FROM   safre_af:tab_siefore_local
   WHERE  codigo_siefore <> 0
   ORDER  BY 1

   START REPORT rpt_detalle_03 TO lc_detalle03
      FOREACH cur_siefore INTO li_siefore
      	 INITIALIZE lr_detalle03.* TO NULL

      	 IF li_siefore = 11 OR
      	 	  li_siefore = 12 THEN
      	 	  CALL get_precio_del_dia(li_siefore,ld_fecha_viv)
      	    RETURNING lr_detalle03.precio
      	 ELSE
      	 	  --CALL get_precio_del_dia(li_siefore,ld_ultimo_habil)
                    CALL get_precio_del_dia(li_siefore,gd_fecha_corte)
      	    RETURNING lr_detalle03.precio
      	 END IF

      	 CASE li_siefore
      	 	  WHEN 6  --ADICIONAL LARGO PLAZO SEGUN AFORE
      	 	  	 LET lr_detalle03.siefore = 7
      	 	  WHEN 11 --INFONAVIT
      	 	  	 LET lr_detalle03.siefore = 0
      	 	  WHEN 12 --FOVISSSTE
      	 	  	 LET lr_detalle03.siefore = 9
      	 	  WHEN 13 --BONO
      	 	  	 LET lr_detalle03.siefore = 8
      	 	  OTHERWISE
      	 	     LET lr_detalle03.siefore = li_siefore --SIEFORES BASICAS
      	 END CASE

      	 OUTPUT TO REPORT rpt_detalle_03(lr_detalle03.*)

         LET li_registros = li_registros + 1

      END FOREACH
   FINISH REPORT rpt_detalle_03

   LET lc_comando = "chmod 777 ", lc_detalle03 CLIPPED
   RUN lc_comando
END FUNCTION
################################################################################
REPORT rpt_detalle_03(lr_detalle03)
   DEFINE lr_detalle03 RECORD
   	         siefore   SMALLINT,
   	         precio    DECIMAL(19,14)
   	      END RECORD

   DEFINE lc_precio_accion CHAR(17)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET lc_precio_accion = lr_detalle03.precio USING "&&.&&&&&&&&&&&&&&"

      PRINT COLUMN 01, "03",
                       lr_detalle03.siefore       USING "&&",
                       lc_precio_accion[1,2], lc_precio_accion[4,17],
                       53 SPACES
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
{
INSERT INTO seg_modulo VALUES(
"ctx",                              --modulo_cod
"Anexo71",                          --modulo_desc
"/safre/cta/fte/multi_sie/anexo71", --ruta_fte
"/safre/cta/exp",                   --ruta_exp
"/safre_back",                      --ruta_envio
"/safre_prc/cta/rescate",           --ruta_rescate
"/safre_lst",                       --ruta_listados
"safre",                            --usuario
TODAY                               --factualiza
)
}