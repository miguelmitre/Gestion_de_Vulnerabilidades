################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTANX24    => ARCHIVO DE HOMOLOGACION                                #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 04 ABRIL   2011                                        #
################################################################################

DATABASE safre_tmp
################################################################################
GLOBALS
  DEFINE  gi_folio    ,
          gi_registros,
          gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE

  DEFINE gc_usuario   CHAR(08)

  DEFINE gs_afore     SMALLINT
  DEFINE hoy          DATE

  DEFINE gr_reporte RECORD
            nss              CHAR(11)     ,
            tipo_solicitud   SMALLINT     ,
            curp             CHAR(18)     ,
            rfc              CHAR(13)     ,
            paterno          CHAR(40)     ,
            materno          CHAR(40)     ,
            nombres          CHAR(40)     ,
            sexo             SMALLINT     ,
            fecha_nacimiento DATE         ,
            siefore          SMALLINT     ,
            estadon          SMALLINT     ,
            doc_prob         SMALLINT     ,
            f_alta_sar       DATE         ,
            f_alta_afore     DATE         ,
            tipo_cuenta      CHAR(01)     ,
            estatus_cuenta   SMALLINT     ,
            tipo_trabajador  SMALLINT     ,
            ind_infonavit    SMALLINT     ,
            ind_fovissste    SMALLINT     ,
            marca            DECIMAL(10,0),
            perido_pago      CHAR(06)     ,
            salario_di       DECIMAL(22,6),
            fecha_pago       DATE         ,
            nrp              CHAR(13),
            ind_actividad    SMALLINT     ,
            acciones_viv_97      DECIMAL(22,6),
            acciones_viv_92      DECIMAL(22,6),
            acciones_fov_92      DECIMAL(22,6),
            acciones_fov_08      DECIMAL(22,6),
            acciones_bono        DECIMAL(22,6)
   END RECORD

   DEFINE gr_seg_modulo RECORD
   	   modulo_cod  CHAR(04),
       ruta_envio  CHAR(40)
   END RECORD

   DEFINE gc_log    CHAR(50)

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO DE HOMOLOGACION",
           " CON FOLIO:",gi_folio

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de homologacion")

   LET gi_registros = 0
   LET hoy          = TODAY

   LET gr_seg_modulo.modulo_cod = "ctx"

   SELECT USER
   INTO   gc_usuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   SELECT codigo_afore
   INTO   gs_afore
   FROM   safre_af:tab_afore_local

   SELECT ruta_envio
   INTO   gr_seg_modulo.ruta_envio
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = gr_seg_modulo.modulo_cod

   LET gc_log = gc_usuario CLIPPED, "CTANX24.log"

   CALL STARTLOG(gc_log)

   CALL crea_tablas()

   CALL archivo()
   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
################################################################################
FUNCTION crea_tablas()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_tab_marca_oficio
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_tab_marca_oficio(
      marca_cod    SMALLINT,
      marca_oficio SMALLINT
   )

   INSERT INTO tmp_tab_marca_oficio VALUES(260,2 )
   INSERT INTO tmp_tab_marca_oficio VALUES(270,3 )
   INSERT INTO tmp_tab_marca_oficio VALUES(210,4 )
   INSERT INTO tmp_tab_marca_oficio VALUES(231,5 )
   INSERT INTO tmp_tab_marca_oficio VALUES(854,6 )
   INSERT INTO tmp_tab_marca_oficio VALUES(605,9 )
   INSERT INTO tmp_tab_marca_oficio VALUES(232,10)
   INSERT INTO tmp_tab_marca_oficio VALUES(600,11)
   INSERT INTO tmp_tab_marca_oficio VALUES(231,12)

   INSERT INTO tmp_tab_marca_oficio VALUES(800,13)
   INSERT INTO tmp_tab_marca_oficio VALUES(810,13)
   INSERT INTO tmp_tab_marca_oficio VALUES(815,13)

   INSERT INTO tmp_tab_marca_oficio VALUES(803,14)

   INSERT INTO tmp_tab_marca_oficio VALUES(820,15)
   INSERT INTO tmp_tab_marca_oficio VALUES(825,15)
   INSERT INTO tmp_tab_marca_oficio VALUES(830,15)
   INSERT INTO tmp_tab_marca_oficio VALUES(840,15)
   INSERT INTO tmp_tab_marca_oficio VALUES(860,15)

   INSERT INTO tmp_tab_marca_oficio VALUES(870,16)
   INSERT INTO tmp_tab_marca_oficio VALUES(875,16)

   INSERT INTO tmp_tab_marca_oficio VALUES(851,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(852,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(853,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(854,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(858,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(881,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(882,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(883,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(884,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(885,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(886,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(887,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(888,17)
   INSERT INTO tmp_tab_marca_oficio VALUES(889,17)

   INSERT INTO tmp_tab_marca_oficio VALUES(855,18)
   INSERT INTO tmp_tab_marca_oficio VALUES(856,18)

   INSERT INTO tmp_tab_marca_oficio VALUES(220,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(225,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(226,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(240,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(247,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(290,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(291,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(292,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(293,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(294,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(295,20)
   INSERT INTO tmp_tab_marca_oficio VALUES(297,20)

   INSERT INTO tmp_tab_marca_oficio VALUES(232,22)

   INSERT INTO tmp_tab_marca_oficio VALUES(863,24)
   INSERT INTO tmp_tab_marca_oficio VALUES(865,24)
   INSERT INTO tmp_tab_marca_oficio VALUES(866,24)

   INSERT INTO tmp_tab_marca_oficio VALUES(540,27)
   INSERT INTO tmp_tab_marca_oficio VALUES(542,27)
   INSERT INTO tmp_tab_marca_oficio VALUES(543,27)
   INSERT INTO tmp_tab_marca_oficio VALUES(544,27)

   INSERT INTO tmp_tab_marca_oficio VALUES(540,30)
   INSERT INTO tmp_tab_marca_oficio VALUES(542,30)
   INSERT INTO tmp_tab_marca_oficio VALUES(543,30)
   INSERT INTO tmp_tab_marca_oficio VALUES(544,30)

   INSERT INTO tmp_tab_marca_oficio VALUES(241,31)
   INSERT INTO tmp_tab_marca_oficio VALUES(242,31)
   INSERT INTO tmp_tab_marca_oficio VALUES(243,31)
   INSERT INTO tmp_tab_marca_oficio VALUES(244,31)

   INSERT INTO tmp_tab_marca_oficio VALUES(280,33)
   INSERT INTO tmp_tab_marca_oficio VALUES(281,33)

   CREATE INDEX ix_tmp_tab_marca_oficio1 ON tmp_tab_marca_oficio(marca_cod)
   UPDATE STATISTICS FOR TABLE tmp_tab_marca_oficio
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
             nss              CHAR(11)     ,
             tipo_solicitud   SMALLINT     ,
             curp             CHAR(18)     ,
             rfc              CHAR(13)     ,
             paterno          CHAR(40)     ,
             materno          CHAR(40)     ,
             nombres          CHAR(40)     ,
             sexo             SMALLINT     ,
             fecha_nacimiento DATE         ,
             estadon          SMALLINT     ,
             tipo_cuenta      CHAR(01)     ,
             ind_tram_jud     SMALLINT     ,
             ind_pensionado   SMALLINT
          END RECORD

   DEFINE ld_precio     DECIMAL(22,6)
   DEFINE i SMALLINT
   DEFINE lc_sql CHAR (500)

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

   DEFINE lc_tipo_cuenta CHAR(01)

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   --CALL nombra_archivo() RETURNING lc_archivo_final

   LET lc_nomarch_ini = gr_seg_modulo.ruta_envio CLIPPED,"/",
                        gc_usuario CLIPPED,
                        ".homologa"

   LET lc_comando_cat   = 'cat'
   LET lc_comando_chmod = ''
   LET lc_comando_rm    = ''

   LET lc_sql = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_sql
   EXECUTE prep_prioridadh

   FOR li_estatus_cuenta = 1 TO 2
   	  CASE li_estatus_cuenta
   	     WHEN 1 LET lc_tipo_cuenta = ' ' --Registrados
   	     WHEN 2 LET lc_tipo_cuenta = '1' --Asignados
   	  END CASE

      FOR li_rango_edad = 1 TO 5
         LET lc_nomarch = lc_nomarch_ini CLIPPED ,
                          "_sc",
                          li_estatus_cuenta USING "&",
                          "_re",
                          li_rango_edad USING "&",
                          "_",
                          gd_fecha_corte USING "DDMMYYYY"

         #/ruta/user.homologa_sc0_re1_DDMMYYYY
         #/ruta/user.rpt_homologacion.YYYYMMDD

         #Concatenar
         LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                              lc_nomarch     CLIPPED

         #Permisos
         LET lc_comando_chmod = lc_comando_chmod CLIPPED,
                                "chmod 777 ", lc_nomarch CLIPPED, ";"

         #Borrar
         LET lc_comando_rm = lc_comando_rm CLIPPED, " rm ",
                             lc_nomarch    CLIPPED, ";"

         START REPORT rpt_homologa TO lc_nomarch
            DECLARE cur_cuota CURSOR FOR
            SELECT nss             ,  --nss
                   tipo_solicitud  ,  --tipo_solicitud
                   curp            ,  --curp
                   rfc             ,  --rfc
                   paterno         ,  --paterno
                   materno         ,  --materno
                   nombres         ,  --nombres
                   sexo            ,  --sexo
                   fecha_nacimiento,  --fecha_nacimiento
                   estadon         ,  --estadon
                   tipo_cuenta     ,  --tipo_cuenta
                   ind_tram_jud    ,  --ind_tram_jud
                   ind_pensionado     --ind_pensionado
            FROM   tmp_cuota_homologa
            WHERE  tipo_cuenta = lc_tipo_cuenta
            AND    ind_edad    = li_rango_edad
            ORDER BY nss

            FOREACH cur_cuota INTO lr_monto_nss.*
               OUTPUT TO REPORT rpt_homologa(lr_monto_nss.*,li_estatus_cuenta)
            END FOREACH
         FINISH REPORT rpt_homologa
      END FOR
   END FOR

   #Archivo final
   LET lc_archivo_final = gc_usuario CLIPPED, ".rpt_homologacion.", gd_fecha_corte USING "YYYYMMDD"
   LET lc_nomarch = gr_seg_modulo.ruta_envio CLIPPED,"/", lc_archivo_final CLIPPED

   #Concatenar
   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_nomarch CLIPPED
   RUN lc_comando_cat

   LET lc_comando_chmod = lc_comando_chmod CLIPPED, " chmod 777 ",
                          lc_nomarch CLIPPED, ";"

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")
   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

   LET lc_sql = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_sql
   EXECUTE prep_prioridadl

   #Permiso a todos los archivos
   RUN lc_comando_chmod

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
      (TODAY            , -- fecha_proceso
       "CTANX24"        , -- proceso_cod
       li_etapa_cod     , -- etapa_cod
       hora_inicial     , -- hora_inicial
       hora_final       , -- hora_final
       "rpt_homologa"   , -- parametro1
       gd_fecha_corte   , -- parametro2  fecha_corte
       gi_tot_nss       , -- parametro3  tot_nss
       NULL             , -- parametro4
       NULL             , -- parametro5
       li_folio         , -- folio
       lc_resultado     , -- resultado
       USER             , -- usuario
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
                 " AND    proceso_cod = 'CTANX24' ",
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
                 " AND    proceso_cod   = 'CTANX24'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED


   {DISPLAY "vhora_final:  ", vhora_final CLIPPED
   DISPLAY "vfolio:       ", vfolio CLIPPED
   DISPLAY "vpos:         ", vpos CLIPPED
   DISPLAY "vresultado:   ", vresultado CLIPPED
   DISPLAY "vetapa_cod:   ", vetapa_cod CLIPPED
   DISPLAY "vconsecutivo: ", vconsecutivo CLIPPED

   DISPLAY "cla_sel: ", cla_sel CLIPPED}
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################
REPORT rpt_homologa(lr_monto_nss, ls_tipo_cuenta)
   DEFINE lr_monto_nss  RECORD
             nss              CHAR(11)     ,
             tipo_solicitud   SMALLINT     ,
             curp             CHAR(18)     ,
             rfc              CHAR(13)     ,
             paterno          CHAR(40)     ,
             materno          CHAR(40)     ,
             nombres          CHAR(40)     ,
             sexo             SMALLINT     ,
             fecha_nacimiento DATE         ,
             estadon          SMALLINT     ,
             tipo_cuenta      CHAR(01)     ,
             ind_tram_jud     SMALLINT     ,
             ind_pensionado   SMALLINT
          END RECORD,
             ls_tipo_cuenta SMALLINT

   DEFINE lc_sexo      CHAR(01),
          lc_estadon   CHAR(02),
          lc_doc_prob  CHAR(01),
          lc_fecha_nac CHAR(08),
          lc_nss       CHAR(11)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT

   ON EVERY ROW
      #Aplicar validaciones de los asignados
      IF lr_monto_nss.tipo_cuenta = '1' THEN
         #En el caso de los trabajadores asignados no se proporcionarán:
         #CURP, RFC, Sexo, Fecha de Nacimiento y Entidad de Nacimiento
         INITIALIZE lr_monto_nss.curp    TO NULL
         INITIALIZE lr_monto_nss.rfc     TO NULL
         INITIALIZE lr_monto_nss.paterno TO NULL
         INITIALIZE lr_monto_nss.materno TO NULL
         INITIALIZE lr_monto_nss.nombres TO NULL

         LET lr_monto_nss.fecha_nacimiento = '01/01/0001'
         LET lc_estadon = '  '
      ELSE
      	 IF lr_monto_nss.estadon IS NOT NULL AND
      	 	  lr_monto_nss.estadon > 0 THEN
      	    LET lc_estadon = lr_monto_nss.estadon USING "&&"
      	 ELSE
            LET lc_estadon = '  '
      	 END IF
      END IF
      
      IF lr_monto_nss.nss[1,1] = 'I' THEN
         LET lc_nss = '           '
      ELSE
      	 LET lc_nss = lr_monto_nss.nss
      END IF

      PRINT COLUMN 001, lc_nss                                        ,
            COLUMN 012, lr_monto_nss.curp                             ,
            COLUMN 030, lr_monto_nss.rfc                              ,
            COLUMN 043, lr_monto_nss.paterno                          ,
            COLUMN 083, lr_monto_nss.materno                          ,
            COLUMN 123, lr_monto_nss.nombres                          ,
            COLUMN 163, lr_monto_nss.sexo             USING "&"       ,
            COLUMN 164, lr_monto_nss.fecha_nacimiento USING "YYYYMMDD",
            COLUMN 172, lc_estadon                                    ,
            COLUMN 199, lr_monto_nss.tipo_cuenta                      ,
            COLUMN 209, lr_monto_nss.ind_tram_jud     USING "&"       ,
            COLUMN 210, lr_monto_nss.ind_pensionado   USING "&"       ,
            COLUMN 342, gs_afore                      USING "&&&"
      LET gi_registros = gi_registros + 1
END REPORT
################################################################################
FUNCTION formato_saldo_132(ld_saldo)
   DEFINE ld_saldo DECIMAL(22,2)
   DEFINE lc_saldo CHAR(15)

   IF ld_saldo < 0 THEN
      LET lc_saldo = ld_saldo * 100 USING "-&&&&&&&&&&&&&&"
   ELSE
   	  LET lc_saldo = ld_saldo * 100 USING "&&&&&&&&&&&&&&&"
   END IF

   RETURN lc_saldo
END FUNCTION
################################################################################
FUNCTION formato_saldo_106(ld_saldo)
   DEFINE ld_saldo DECIMAL(22,2)
   DEFINE lc_saldo CHAR(16)

   IF ld_saldo < 0 THEN
      LET lc_saldo = ld_saldo * 1000000 USING "-&&&&&&&&&&&&&&&"
   ELSE
   	  LET lc_saldo = ld_saldo * 1000000 USING "&&&&&&&&&&&&&&&&"
   END IF

   RETURN lc_saldo
END FUNCTION
################################################################################
FUNCTION verifica_marca(lc_nss)
   DEFINE lc_nss    CHAR(11)
   DEFINE lc_marca  CHAR(10)
   DEFINE ls_indice SMALLINT
   DEFINE ls_marca  SMALLINT

   LET lc_marca  = "0000000000"
   LET ls_indice = 0

   DECLARE cur_marcas CURSOR FOR
   SELECT b.marca_oficio
   FROM   safre_af:cta_act_marca a,
          tmp_tab_marca_oficio   b
   WHERE  a.marca_cod = b.marca_cod
   AND    a.nss       = lc_nss
   GROUP BY 1
   ORDER BY 1

   FOREACH cur_marcas INTO ls_marca
   	  LET ls_indice = ls_indice + 1

   	  IF ls_indice > 5 THEN
   	  	 EXIT FOREACH
   	  END IF

   	  CASE ls_indice
   	  	 WHEN 1 LET lc_marca[1,2]  = ls_marca USING "&&"
   	  	 WHEN 2 LET lc_marca[3,4]  = ls_marca USING "&&"
   	  	 WHEN 3 LET lc_marca[5,6]  = ls_marca USING "&&"
   	  	 WHEN 4 LET lc_marca[7,8]  = ls_marca USING "&&"
   	  	 WHEN 5 LET lc_marca[9,10] = ls_marca USING "&&"
   	  END CASE
   END FOREACH

   RETURN lc_marca
END FUNCTION
################################################################################