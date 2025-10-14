################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX13    => FOLIOS DEL ESTADO DE CUENTA                            #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 06 de ENERO DE 2010                                    #
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

   DEFINE gd_saldo_total   DECIMAL(22,6)

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO SALDOS ISSSTE",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTANX13.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación rpt saldos issste")

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
          lc_encabezado    CHAR(80),
          lc_sumario       CHAR(80),
          lc_detalle03     CHAR(80),
          lc_detalle       CHAR(80),
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

   CALL nombra_archivo() RETURNING lc_encabezado   ,
                                   lc_sumario      ,
                                   lc_detalle      ,
                                   lc_archivo_final

   LET lc_encabezado    = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_encabezado    CLIPPED
   LET lc_detalle       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_detalle       CLIPPED
   LET lc_sumario       = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_sumario       CLIPPED
   LET lc_archivo_final = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final CLIPPED

   LET lc_comando_cat = lc_comando_cat   CLIPPED, " ",
                        lc_encabezado    CLIPPED, " ",
                        lc_sumario       CLIPPED, " ",
                        lc_detalle       CLIPPED, " > ",
                        lc_archivo_final CLIPPED

   LET lc_comando_chmod = "chmod 777 ",
                          lc_encabezado CLIPPED, ";",
                          "chmod 777 ",
                          lc_sumario CLIPPED, ";",
                          "chmod 777 ",
                          lc_detalle CLIPPED, ";",
                          "chmod 777 ",
                          lc_archivo_final CLIPPED, ";"

{   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_saldos_issste
   WHENEVER ERROR STOP

   SQL

    CREATE TABLE tmp_saldos_issste
    ( nss      CHAR(11),
      subcta   CHAR(02),
      siefore  SMALLINT,
      acciones DECIMAL(22,6),
      pesos    DECIMAL(22,6),
      udis     DECIMAL(22,6),
    );

   END SQL}

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   EXECUTE prep_prioridadh


   {INSERT INTO tmp_saldos_issste
      SELECT a.nss              ,
             a.subcuenta        ,
             a.siefore          ,
             a.monto_en_acciones,
             a.monto_en_pesos   ,
             a.udis
      FROM   tmp_saldo_issste   a,
             tmp_ident_bono   b
      WHERE  a.nss       = b.nss

   SQL
      CREATE INDEX ix_tmp_saldos_issste1 ON tmp_saldos_issste ( nss,subcta);
   END SQL

   UPDATE STATISTICS FOR TABLE tmp_saldos_issste}

   #/ruta/rpt_issste_det_DDMMYYYYY
   #/ruta/rpt_issste_enc_DDMMYYYYY
   #/ruta/rpt_issste_sum_DDMMYYYYY
   #/ruta/DDMMYYYY_REPORTE0001_A&&&

   #Encabezado
   CALL encabezado(lc_encabezado)

   #Detalle
   CALL detalle(lc_detalle)

   #Subencabezado
   CALL sumario(lc_sumario)

   RUN lc_comando_cat
   RUN lc_comando_chmod

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_archivo_final)

   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   EXECUTE prep_prioridadl

   #Permiso a todos los archivos
   RUN lc_comando_chmod

END FUNCTION
################################################################################
FUNCTION nombra_archivo()
   DEFINE lc_archivo_final CHAR(80),
          lc_encabezado    CHAR(80),
          lc_detalle       CHAR(80),
          lc_sumario       CHAR(80)

   LET lc_archivo_final = gd_fecha_corte USING "DDMMYYYY", "_REPORTE0001_A",gi_afore USING "&&&"
   LET lc_encabezado    = "rpt_issste_enc_",gd_fecha_corte USING "DDMMYYYY"
   LET lc_detalle       = "rpt_issste_det_",gd_fecha_corte USING "DDMMYYYY"
   LET lc_sumario       = "rpt_issste_sum_",gd_fecha_corte USING "DDMMYYYY"

   RETURN lc_encabezado   ,
          lc_detalle      ,
          lc_sumario      ,
          lc_archivo_final
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
   DEFINE lr_encabezado RECORD
   	   tipo_registro SMALLINT,
   	   tipo_archivo  SMALLINT,
   	   tipo_entidad  SMALLINT,
   	   entidad       SMALLINT,
   	   fecha_envio   DATE    ,
   	   longitud      SMALLINT,
   	   registros     SMALLINT
	 END RECORD

	 OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET lr_encabezado.tipo_registro = 0
      LET lr_encabezado.tipo_archivo  = 1
      LET lr_encabezado.tipo_entidad  = 1
      LET lr_encabezado.entidad       = gi_afore
      LET lr_encabezado.fecha_envio   = MDY(6,4, YEAR(TODAY))
      LET lr_encabezado.longitud      = 66
      LET lr_encabezado.registros     = 3

      PRINT COLUMN 01, lr_encabezado.tipo_registro USING "&&&"     ,
            COLUMN 04, lr_encabezado.tipo_archivo  USING "&&&&"     ,
            COLUMN 08, lr_encabezado.tipo_entidad  USING "&&&"     ,
            COLUMN 11, lr_encabezado.entidad       USING "&&&"     ,
            COLUMN 14, lr_encabezado.fecha_envio   USING "YYYYMMDD",
            COLUMN 22, lr_encabezado.longitud      USING "&&&"     ,
            COLUMN 25, lr_encabezado.registros     USING "&&&&&"   ,
            COLUMN 30, 37 SPACES
END REPORT
################################################################################
FUNCTION sumario(lc_sumario)
   DEFINE lc_sumario    CHAR(200)

   DEFINE lr_sumario RECORD
   	  tipo_registro SMALLINT     ,
   	  tipo_entidad  SMALLINT     ,
   	  entidad       SMALLINT     ,
   	  fecha_corte   DATE         ,
   	  saldo_total   DECIMAL(22,6),
   	  trabajadores  DECIMAL(10,0)
   END RECORD

   LET lr_sumario.tipo_registro = 101
   LET lr_sumario.tipo_entidad  = 1
   LET lr_sumario.entidad       = gi_afore
   LET lr_sumario.fecha_corte   = gd_fecha_corte

   #Saldo Total
   LET lr_sumario.saldo_total = gd_saldo_total

   #Total de trabajadores
   SELECT COUNT(*)
   INTO   lr_sumario.trabajadores
   FROM   tmp_ident_bono

   START REPORT rpt_sumario TO lc_sumario
      OUTPUT TO REPORT rpt_sumario(lr_sumario.*)
   FINISH REPORT rpt_sumario
END FUNCTION
################################################################################
REPORT rpt_sumario(lr_sumario)
   DEFINE lr_sumario RECORD
   	  tipo_registro SMALLINT     ,
   	  tipo_entidad  SMALLINT     ,
   	  entidad       SMALLINT     ,
   	  fecha_corte   DATE         ,
   	  saldo_total   DECIMAL(22,6),
   	  trabajadores  DECIMAL(10,0)
   END RECORD

   DEFINE lc_paso  CHAR(13),
          lc_saldo CHAR(12)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      LET lc_paso  = lr_sumario.saldo_total * 1 USING "&&&&&&&&&&.&&"
      LET lc_saldo = lc_paso[1,10], lc_paso[12,13]

   PRINT COLUMN 01, lr_sumario.tipo_registro USING "&&&"      ,
         COLUMN 04, lr_sumario.tipo_entidad  USING "&&&"      ,
         COLUMN 07, lr_sumario.entidad       USING "&&&"      ,
         COLUMN 10, lr_sumario.fecha_corte   USING "YYYYMMDD" ,
         COLUMN 18, lc_saldo                                  ,
         COLUMN 30, lr_sumario.trabajadores  USING "&&&&&&&&&",
         COLUMN 39, 28 SPACES
END REPORT
################################################################################
FUNCTION detalle(lc_detalle)
   DEFINE lc_detalle    CHAR(200)

   DEFINE lr_detalle RECORD
   	   tipo_registro      SMALLINT     ,
   	   con_bono           DECIMAL(10,0),
   	   sin_bono           DECIMAL(10,0),
   	   transitorio        DECIMAL(10,0),
   	   saldo_bono         DECIMAL(22,6),
   	   saldo_sin_bono     DECIMAL(22,6),
   	   saldo_transitorio  DECIMAL(22,6)
   END RECORD

   DEFINE ld_saldo_bono         DECIMAL(22,2),
   	      ld_saldo_sin_bono     DECIMAL(22,2),
   	      ld_saldo_transitorio  DECIMAL(22,2)

   LET lr_detalle.tipo_registro = 301

   #Con bono
   SELECT COUNT(UNIQUE nss)
   INTO   lr_detalle.con_bono
   FROM   tmp_ident_bono
   WHERE  ind_bono_dis  = 1 --Con Bono en dis_cuenta
   --OR     ind_trab_bono = 1 --Con Bono redimido

   #Sin bono
   SELECT COUNT(UNIQUE nss)
   INTO   lr_detalle.sin_bono
   FROM   tmp_ident_bono
   WHERE  ind_bono_dis  = 0 --Sin Bono en dis_cuenta
   AND    ind_trab_bono IS NOT NULL --Identificado sin bono
   --AND    ind_trab_bono = 0 --Identificado sin bono

   #Transitorio
   SELECT COUNT(UNIQUE nss)
   INTO   lr_detalle.transitorio
   FROM   tmp_ident_bono
   WHERE  ind_bono_dis  = 0 --Sin Bono en dis_cuenta
   AND    ind_trab_bono IS NULL  --Identificado transitorio

   #Saldos con bono
   SELECT SUM(monto_en_pesos)
   INTO   lr_detalle.saldo_bono
   FROM   tmp_saldo_issste
   WHERE  nss IN (SELECT UNIQUE nss
                  FROM   tmp_ident_bono
                  WHERE  ind_bono_dis  = 1 --Con Bono en dis_cuenta
                  --OR     ind_trab_bono = 1 --Con Bono redimido
                  )

   #Saldos sin bono
   SELECT SUM(monto_en_pesos)
   INTO   lr_detalle.saldo_sin_bono
   FROM   tmp_saldo_issste
   WHERE  nss IN (SELECT UNIQUE nss
                  FROM   tmp_ident_bono
                  WHERE  ind_bono_dis  = 0 --Sin Bono en dis_cuenta
                  AND    ind_trab_bono IS NOT NULL --Identificado sin bono
                  --AND    ind_trab_bono = 0 --Identificado sin bono
                  )

   #Saldos transitorio
   SELECT SUM(monto_en_pesos)
   INTO   lr_detalle.saldo_transitorio
   FROM   tmp_saldo_issste
   WHERE  nss IN (SELECT UNIQUE nss
                  FROM   tmp_ident_bono
                  WHERE  ind_bono_dis  = 0 --Sin Bono en dis_cuenta
                  AND    ind_trab_bono IS NULL  --Identificado transitorio
                  )

   LET ld_saldo_bono        = lr_detalle.saldo_bono        * 1
   LET ld_saldo_sin_bono    = lr_detalle.saldo_sin_bono    * 1
   LET ld_saldo_transitorio = lr_detalle.saldo_transitorio * 1


   LET gd_saldo_total = ld_saldo_bono       +
                        ld_saldo_sin_bono   +
                        ld_saldo_transitorio

   START REPORT rpt_detalle TO lc_detalle
      OUTPUT TO REPORT rpt_detalle(lr_detalle.*)
   FINISH REPORT rpt_detalle
END FUNCTION
################################################################################
REPORT rpt_detalle(lr_detalle)
   DEFINE lr_detalle RECORD
   	   tipo_registro      SMALLINT     ,
   	   con_bono           DECIMAL(10,0),
   	   sin_bono           DECIMAL(10,0),
   	   transitorio        DECIMAL(10,0),
   	   saldo_bono         DECIMAL(22,6),
   	   saldo_sin_bono     DECIMAL(22,6),
   	   saldo_transitorio  DECIMAL(22,6)
   END RECORD

   DEFINE lc_paso              CHAR(13),
          lc_saldo_con_bono    CHAR(12),
          lc_saldo_sin_bono    CHAR(12),
          lc_saldo_transitorio CHAR(12)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      #Con bono
      LET lc_paso  = lr_detalle.saldo_bono * 1 USING "&&&&&&&&&&.&&"
      LET lc_saldo_con_bono = lc_paso[1,10], lc_paso[12,13]

      #Sin bono
      LET lc_paso  = lr_detalle.saldo_sin_bono * 1 USING "&&&&&&&&&&.&&"
      LET lc_saldo_sin_bono = lc_paso[1,10], lc_paso[12,13]

      #Transitorio
      LET lc_paso  = lr_detalle.saldo_transitorio * 1 USING "&&&&&&&&&&.&&"
      LET lc_saldo_transitorio = lc_paso[1,10], lc_paso[12,13]

    PRINT COLUMN 01, lr_detalle.tipo_registro USING "&&&"      ,
          COLUMN 04, lr_detalle.con_bono      USING "&&&&&&&&&",
          COLUMN 13, lr_detalle.sin_bono      USING "&&&&&&&&&",
          COLUMN 22, lr_detalle.transitorio   USING "&&&&&&&&&",
          COLUMN 31, lc_saldo_con_bono                         ,
          COLUMN 43, lc_saldo_sin_bono                         ,
          COLUMN 55, lc_saldo_transitorio

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
       "CTANX13",         -- proceso_cod
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
                 " AND    proceso_cod = 'CTANX13' ",
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
                 " AND    proceso_cod   = 'CTANX13'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED

   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################