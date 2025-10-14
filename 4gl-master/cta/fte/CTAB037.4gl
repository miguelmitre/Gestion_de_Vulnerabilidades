################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAB037    => ESTADO DE CUENTA                                       #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 27 de Diciembre de 2007                                #
#Fecha               => 31 de Enero     de 2007                                #
#Fecha               => 18 de marzo     de 2008                                #
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

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO CORTE MULTISIEFORE",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTAB037.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de corte multisie")
   LET gi_registros = 0
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
       "CTAB037",         -- proceso_cod
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
          lc_comando_rm    CHAR(8000),
          lc_encabezado    CHAR(80),
          lc_archivo_final CHAR(80)

   DEFINE lr_monto_nss  RECORD
             nss         CHAR(11),
             siefore     SMALLINT,
             subcta      SMALLINT,
             acciones    DECIMAL(22,6),
             pesos       DECIMAL(22,6),
             bnd_siefore SMALLINT,
             siefore_sol SMALLINT
          END RECORD

   DEFINE ld_precio     DECIMAL(22,6)
   DEFINE i SMALLINT
   DEFINE lc_prioridad CHAR (100)

   DEFINE li_siefore_ori SMALLINT
   DEFINE li_bnd_vol     SMALLINT
   DEFINE li_siefore,        
          li_estatus_cuenta,                  
          li_edad            SMALLINT

   --LET lc_nomarch = "/safre_back/corte_multi_sie_",gd_fecha_corte

   LET lc_comando_cat = "cat"
   INITIALIZE lc_comando_rm TO NULL

   CALL nombra_archivo() RETURNING lc_encabezado,
                                   lc_archivo_final

   LET lc_encabezado = "/safre_back/", lc_encabezado CLIPPED

   LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                        lc_encabezado CLIPPED

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   EXECUTE prep_prioridadh
   
   FOR li_siefore = 1 TO 5
      FOR li_estatus_cuenta = 0 to 4
      	  LET lc_nomarch = "/safre_back/corte_multi_5sie_s",
                          li_siefore USING "&",
                          "sc",
                          li_estatus_cuenta USING "&",
                          "_",
                          gd_fecha_corte USING "DDMMYYYY"
      
         LET lc_comando_cat = lc_comando_cat CLIPPED, " ",
                              lc_nomarch CLIPPED
      
         LET lc_comando_rm = lc_comando_rm CLIPPED, " rm ",
                             lc_nomarch CLIPPED, ";"
      
         DECLARE cur_nss CURSOR FOR
         SELECT c.nss,
                c.siefore,
                c.edad,
                d.siefore_sol
         FROM   safre_tmp:cta_cuota_multisie_nss c,
                OUTER safre_af:afi_solicitud_siefore d
         WHERE  c.nss = d.nss
         AND    c.siefore        = li_siefore
         AND    c.estatus_cuenta = li_estatus_cuenta
         AND    d.edo_sol = 1 --SOLICITUD ACEPTADA
         ORDER BY 1
      
         START REPORT multi_sie TO lc_nomarch
         LET li_bnd_vol = 0
      
         FOREACH cur_nss INTO lr_monto_nss.nss,
         	                   lr_monto_nss.siefore,
         	                   li_edad,
         	                   lr_monto_nss.siefore_sol
      
         	 IF lr_monto_nss.siefore_sol IS NULL THEN
         	 	  --NO TIENE SOLICTUD DE SIEFORE
         	 	  LET lr_monto_nss.bnd_siefore = 0
         	 	  LET lr_monto_nss.siefore_sol = 0
         	 ELSE
         	 	  --TIENE SOLICTUD DE SIEFORE
         	 	  LET lr_monto_nss.bnd_siefore = 1
         	 	  --LET lr_monto_nss.siefore     = lr_monto_nss.siefore_sol
         	 END IF
--******************************************************************************
--MENORES A 56 AÑOS
--******************************************************************************
            IF li_edad < 56 THEN
               DECLARE cur_subcta CURSOR FOR
               SELECT b.subct_cns,
                      SUM(a.monto_en_pesos)
               FROM   safre_tmp:cta_cuota_multisie_nss c,
                      OUTER safre_tmp:tmp_saldo_corte_multisie a,
                      safre_af:tab_subcuenta b
               WHERE  a.nss = c.nss
               AND    c.nss = lr_monto_nss.nss
               AND    a.subcuenta NOT IN (4,8,14,19)
               AND    a.subcuenta = b.subct_cod
               AND    a.siefore IN (1,2)
               GROUP BY 1
               ORDER BY 1
               
               FOREACH cur_subcta INTO lr_monto_nss.subcta,
                                       lr_monto_nss.pesos
               
                  --VERIFICAR SI SE TRANSFIERE SUBCUENTA
               
                  IF li_bnd_vol = 1 THEN
                  	 LET lr_monto_nss.siefore = li_siefore_ori
                  END IF
               
                  IF lr_monto_nss.subcta = 5 OR lr_monto_nss.subcta = 13 THEN
                     --NO SE TRANSFIERE
                     LET li_siefore_ori = lr_monto_nss.siefore
                     LET li_bnd_vol = 1
                     LET lr_monto_nss.siefore = 01
                  ELSE
                  	 LET li_bnd_vol = 0
                  END IF
               
                  IF lr_monto_nss.pesos IS NULL THEN
                  	  LET lr_monto_nss.pesos = 0
                  END IF
               
                  IF lr_monto_nss.pesos <= 0 THEN
               	    LET lr_monto_nss.acciones = 0
               	    LET lr_monto_nss.pesos    = 0
               	 ELSE
               	 	  --OBTENER PRECIO DE LA SIEFORE
               	 	  CALL get_precio_del_dia(lr_monto_nss.siefore, gd_fecha_corte)
               	 	  RETURNING ld_precio
               
               	 	  LET lr_monto_nss.acciones = lr_monto_nss.pesos / ld_precio
                  END IF
               
                  OUTPUT TO REPORT multi_sie(lr_monto_nss.*)
               END FOREACH
--******************************************************************************
--MAYORES A 56 AÑOS
--******************************************************************************
            ELSE
               DECLARE cur_siefore_subcta CURSOR FOR
               SELECT b.subct_cns,
                      a.siefore,
                      SUM(a.monto_en_pesos),
                      SUM(a.monto_en_acciones)
               FROM   safre_tmp:cta_cuota_multisie_nss c,
                      OUTER safre_tmp:tmp_saldo_corte_multisie a,
                      safre_af:tab_subcuenta b
               WHERE  a.nss = c.nss
               AND    c.nss = lr_monto_nss.nss
               AND    a.subcuenta NOT IN (4,8,14,19)
               AND    a.subcuenta = b.subct_cod
               AND    a.siefore IN (1,2)
               GROUP BY 1,2
               ORDER BY 1,2
      
               FOREACH cur_siefore_subcta INTO lr_monto_nss.subcta,
               	                              lr_monto_nss.siefore,
                                               lr_monto_nss.pesos,
                                               lr_monto_nss.acciones
      
                  IF lr_monto_nss.pesos IS NULL THEN
                  	  LET lr_monto_nss.pesos = 0
                  END IF
      
                  IF lr_monto_nss.pesos <= 0 THEN
               	    LET lr_monto_nss.acciones = 0
               	    LET lr_monto_nss.pesos    = 0
                  END IF
      
                  OUTPUT TO REPORT multi_sie(lr_monto_nss.*)
               END FOREACH
            END IF
         END FOREACH
         FINISH REPORT multi_sie
      END FOR
   END FOR

   CALL encabezado(lc_encabezado)

   LET lc_nomarch = "/safre_back/", lc_archivo_final CLIPPED
   {corte_multi_5sie_",
                    gd_fecha_corte USING "DDMMYYYY"}

   LET lc_comando_cat = lc_comando_cat CLIPPED, " > ", lc_nomarch CLIPPED
   RUN lc_comando_cat
   --RUN lc_comando_rm

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
             siefore     SMALLINT,
             subcta      SMALLINT,
             acciones    DECIMAL(22,6),
             pesos       DECIMAL(22,6),
             bnd_siefore SMALLINT,
             siefore_sol SMALLINT
          END RECORD

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

   DEFINE lr_reporte_01 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_02 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_03 RECORD
           acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_05 RECORD
           acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_08 RECORD
           acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_13 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_15 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_17 RECORD
             acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_18 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_22 RECORD
            acciones1 DECIMAL(22,6),
            pesos1    DECIMAL(22,6),
            acciones2 DECIMAL(22,6),
            pesos2    DECIMAL(22,6),
            acciones3 DECIMAL(22,6),
            pesos3    DECIMAL(22,6),
            acciones4 DECIMAL(22,6),
            pesos4    DECIMAL(22,6),
            acciones5 DECIMAL(22,6),
            pesos5    DECIMAL(22,6),
            subcta    SMALLINT
   END RECORD

   DEFINE li_estatus_cuenta SMALLINT

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT

   BEFORE GROUP OF lr_monto_nss.nss
      LET lr_reporte_01.acciones1 = 0
      LET lr_reporte_01.pesos1    = 0
      LET lr_reporte_01.acciones2 = 0
      LET lr_reporte_01.pesos2    = 0
      LET lr_reporte_01.acciones3 = 0
      LET lr_reporte_01.pesos3    = 0
      LET lr_reporte_01.acciones4 = 0
      LET lr_reporte_01.pesos4    = 0
      LET lr_reporte_01.acciones5 = 0
      LET lr_reporte_01.pesos5    = 0
      LET lr_reporte_01.subcta    = 01

      LET lr_reporte_02.acciones1 = 0
      LET lr_reporte_02.pesos1    = 0
      LET lr_reporte_02.acciones2 = 0
      LET lr_reporte_02.pesos2    = 0
      LET lr_reporte_02.acciones3 = 0
      LET lr_reporte_02.pesos3    = 0
      LET lr_reporte_02.acciones4 = 0
      LET lr_reporte_02.pesos4    = 0
      LET lr_reporte_02.acciones5 = 0
      LET lr_reporte_02.pesos5    = 0
      LET lr_reporte_02.subcta    = 02

      LET lr_reporte_03.acciones1 = 0
      LET lr_reporte_03.pesos1    = 0
      LET lr_reporte_03.acciones2 = 0
      LET lr_reporte_03.pesos2    = 0
      LET lr_reporte_03.acciones3 = 0
      LET lr_reporte_03.pesos3    = 0
      LET lr_reporte_03.acciones4 = 0
      LET lr_reporte_03.pesos4    = 0
      LET lr_reporte_03.acciones5 = 0
      LET lr_reporte_03.pesos5    = 0
      LET lr_reporte_03.subcta    = 03

      LET lr_reporte_05.acciones1 = 0
      LET lr_reporte_05.pesos1    = 0
      LET lr_reporte_05.acciones2 = 0
      LET lr_reporte_05.pesos2    = 0
      LET lr_reporte_05.acciones3 = 0
      LET lr_reporte_05.pesos3    = 0
      LET lr_reporte_05.acciones4 = 0
      LET lr_reporte_05.pesos4    = 0
      LET lr_reporte_05.acciones5 = 0
      LET lr_reporte_05.pesos5    = 0
      LET lr_reporte_05.subcta    = 05

      LET lr_reporte_08.acciones1 = 0
      LET lr_reporte_08.pesos1    = 0
      LET lr_reporte_08.acciones2 = 0
      LET lr_reporte_08.pesos2    = 0
      LET lr_reporte_08.acciones3 = 0
      LET lr_reporte_08.pesos3    = 0
      LET lr_reporte_08.acciones4 = 0
      LET lr_reporte_08.pesos4    = 0
      LET lr_reporte_08.acciones5 = 0
      LET lr_reporte_08.pesos5    = 0
      LET lr_reporte_08.subcta    = 08

      LET lr_reporte_13.acciones1 = 0
      LET lr_reporte_13.pesos1    = 0
      LET lr_reporte_13.acciones2 = 0
      LET lr_reporte_13.pesos2    = 0
      LET lr_reporte_13.acciones3 = 0
      LET lr_reporte_13.pesos3    = 0
      LET lr_reporte_13.acciones4 = 0
      LET lr_reporte_13.pesos4    = 0
      LET lr_reporte_13.acciones5 = 0
      LET lr_reporte_13.pesos5    = 0
      LET lr_reporte_13.subcta    = 13

      LET lr_reporte_15.acciones1 = 0
      LET lr_reporte_15.pesos1    = 0
      LET lr_reporte_15.acciones2 = 0
      LET lr_reporte_15.pesos2    = 0
      LET lr_reporte_15.acciones3 = 0
      LET lr_reporte_15.pesos3    = 0
      LET lr_reporte_15.acciones4 = 0
      LET lr_reporte_15.pesos4    = 0
      LET lr_reporte_15.acciones5 = 0
      LET lr_reporte_15.pesos5    = 0
      LET lr_reporte_15.subcta    = 15

      LET lr_reporte_17.acciones1 = 0
      LET lr_reporte_17.pesos1    = 0
      LET lr_reporte_17.acciones2 = 0
      LET lr_reporte_17.pesos2    = 0
      LET lr_reporte_17.acciones3 = 0
      LET lr_reporte_17.pesos3    = 0
      LET lr_reporte_17.acciones4 = 0
      LET lr_reporte_17.pesos4    = 0
      LET lr_reporte_17.acciones5 = 0
      LET lr_reporte_17.pesos5    = 0
      LET lr_reporte_17.subcta    = 17

      LET lr_reporte_18.acciones1 = 0
      LET lr_reporte_18.pesos1    = 0
      LET lr_reporte_18.acciones2 = 0
      LET lr_reporte_18.pesos2    = 0
      LET lr_reporte_18.acciones3 = 0
      LET lr_reporte_18.pesos3    = 0
      LET lr_reporte_18.acciones4 = 0
      LET lr_reporte_18.pesos4    = 0
      LET lr_reporte_18.acciones5 = 0
      LET lr_reporte_18.pesos5    = 0
      LET lr_reporte_18.subcta    = 18

      LET lr_reporte_22.acciones1 = 0
      LET lr_reporte_22.pesos1    = 0
      LET lr_reporte_22.acciones2 = 0
      LET lr_reporte_22.pesos2    = 0
      LET lr_reporte_22.acciones3 = 0
      LET lr_reporte_22.pesos3    = 0
      LET lr_reporte_22.acciones4 = 0
      LET lr_reporte_22.pesos4    = 0
      LET lr_reporte_22.acciones5 = 0
      LET lr_reporte_22.pesos5    = 0
      LET lr_reporte_22.subcta    = 22

      SELECT nss,
             tipo_solicitud,
             curp,
             rfc,
             fecha_nacimiento,
             estatus_cuenta,
             id_sector,
             criterio
      INTO   lr_reporte.nss,
             lr_reporte.tipo_solicitud,
             lr_reporte.curp,
             lr_reporte.rfc,
             lr_reporte.fena,
             lr_reporte.status_cta,
             lr_reporte.sector,
             lr_reporte.criterio_sie
      FROM   cta_cuota_multisie_nss a
      WHERE  a.nss = lr_monto_nss.nss

      IF lr_reporte.status_cta = 4 THEN
      	 LET lr_reporte.status_cta = 3
      END IF

      --ASIGNADOS
      IF lr_reporte.tipo_solicitud = 5 THEN
      	 LET lr_reporte.fanio = lr_reporte.fena[7,10], --YYYY
      	                                   "0101"      --NVO LAYOUT
      	                             --YYYYMMDD
      	 LET lr_reporte.fnacimiento = "19000101"
      ELSE
      	 LET lr_reporte.fanio       = "19000101"       --NVO LAYOUT
      	 LET lr_reporte.fnacimiento = lr_reporte.fena[7,10], --YYYY
      	                              lr_reporte.fena[1,2],  --MM
      	                              lr_reporte.fena[4,5]   --DD
      END IF

      SELECT NVL(a.precio_del_dia,0)    --PRECIO DE ACCION SIEFORE 1
      INTO   lr_reporte.precio1
      FROM   safre_tmp:glo_valor_accion a
      WHERE  a.codigo_siefore  = 1
      AND    a.fecha_valuacion = gd_fecha_corte

      IF lr_reporte.precio1 IS NULL THEN
      	 LET lr_reporte.precio1 = 0
      END IF

      SELECT NVL(a.precio_del_dia,0)   --PRECIO DE ACCION SIEFORE 2
      INTO   lr_reporte.precio2
      FROM   safre_tmp:glo_valor_accion a
      WHERE  a.codigo_siefore  = 2
      AND    a.fecha_valuacion = gd_fecha_corte

      IF lr_reporte.precio2 IS NULL THEN
      	 LET lr_reporte.precio2 = 0
      END IF

      SELECT NVL(a.precio_del_dia,0)    --PRECIO DE ACCION SIEFORE 3
      INTO   lr_reporte.precio3
      FROM   safre_tmp:glo_valor_accion a
      WHERE  a.codigo_siefore  = 3
      AND    a.fecha_valuacion = gd_fecha_corte

      IF lr_reporte.precio3 IS NULL THEN
      	 LET lr_reporte.precio3 = 0
      END IF

      SELECT NVL(a.precio_del_dia,0)   --PRECIO DE ACCION SIEFORE 4
      INTO   lr_reporte.precio4
      FROM   safre_tmp:glo_valor_accion a
      WHERE  a.codigo_siefore  = 4
      AND    a.fecha_valuacion = gd_fecha_corte

      IF lr_reporte.precio4 IS NULL THEN
      	 LET lr_reporte.precio4 = 0
      END IF

      SELECT NVL(a.precio_del_dia,0)   --PRECIO DE ACCION SIEFORE 5
      INTO   lr_reporte.precio5
      FROM   safre_tmp:glo_valor_accion a
      WHERE  a.codigo_siefore  = 5
      AND    a.fecha_valuacion = gd_fecha_corte

      IF lr_reporte.precio5 IS NULL THEN
      	 LET lr_reporte.precio5 = 0
      END IF

      LET lr_reporte.precio3 = lr_reporte.precio2
      LET lr_reporte.precio4 = lr_reporte.precio2
      LET lr_reporte.precio5 = lr_reporte.precio2

   ON EVERY ROW
      CASE  lr_monto_nss.siefore
      	 WHEN 1
            CASE lr_monto_nss.subcta
               WHEN "01"
                  LET lr_reporte_01.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_01.pesos1    = lr_monto_nss.pesos
               WHEN "02"
                  LET lr_reporte_02.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_02.pesos1    = lr_monto_nss.pesos
               WHEN "03"
                  LET lr_reporte_03.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_03.pesos1    = lr_monto_nss.pesos
               WHEN "05"
                  LET lr_reporte_05.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_05.pesos1    = lr_monto_nss.pesos
               WHEN "08"
                  LET lr_reporte_08.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_08.pesos1    = lr_monto_nss.pesos
               WHEN "13"
                  LET lr_reporte_13.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_13.pesos1    = lr_monto_nss.pesos
               WHEN "15"
                  LET lr_reporte_15.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_15.pesos1    = lr_monto_nss.pesos
               WHEN "17"
                  LET lr_reporte_17.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_17.pesos1    = lr_monto_nss.pesos
               WHEN "18"
                  LET lr_reporte_18.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_18.pesos1    = lr_monto_nss.pesos
               WHEN "22"
                  LET lr_reporte_22.acciones1 = lr_monto_nss.acciones
                  LET lr_reporte_22.pesos1    = lr_monto_nss.pesos
            END CASE
         WHEN 2
         	  CASE lr_monto_nss.subcta
               WHEN "01"
                  LET lr_reporte_01.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_01.pesos2    = lr_monto_nss.pesos
               WHEN "02"
                  LET lr_reporte_02.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_02.pesos2    = lr_monto_nss.pesos
               WHEN "03"
                  LET lr_reporte_03.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_03.pesos2    = lr_monto_nss.pesos
               WHEN "05"
                  LET lr_reporte_05.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_05.pesos2    = lr_monto_nss.pesos
               WHEN "08"
                  LET lr_reporte_08.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_08.pesos2    = lr_monto_nss.pesos
               WHEN "13"
                  LET lr_reporte_13.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_13.pesos2    = lr_monto_nss.pesos
               WHEN "15"
                  LET lr_reporte_15.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_15.pesos2    = lr_monto_nss.pesos
               WHEN "17"
                  LET lr_reporte_17.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_17.pesos2    = lr_monto_nss.pesos
               WHEN "18"
                  LET lr_reporte_18.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_18.pesos2    = lr_monto_nss.pesos
               WHEN "22"
                  LET lr_reporte_22.acciones2 = lr_monto_nss.acciones
                  LET lr_reporte_22.pesos2    = lr_monto_nss.pesos
            END CASE
         WHEN 3
         	  CASE lr_monto_nss.subcta
               WHEN "01"
                  LET lr_reporte_01.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_01.pesos3    = lr_monto_nss.pesos
               WHEN "02"
                  LET lr_reporte_02.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_02.pesos3    = lr_monto_nss.pesos
               WHEN "03"
                  LET lr_reporte_03.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_03.pesos3    = lr_monto_nss.pesos
               WHEN "05"
                  LET lr_reporte_05.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_05.pesos3    = lr_monto_nss.pesos
               WHEN "08"
                  LET lr_reporte_08.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_08.pesos3    = lr_monto_nss.pesos
               WHEN "13"
                  LET lr_reporte_13.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_13.pesos3    = lr_monto_nss.pesos
               WHEN "15"
                  LET lr_reporte_15.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_15.pesos3    = lr_monto_nss.pesos
               WHEN "17"
                  LET lr_reporte_17.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_17.pesos3    = lr_monto_nss.pesos
               WHEN "18"
                  LET lr_reporte_18.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_18.pesos3    = lr_monto_nss.pesos
               WHEN "22"
                  LET lr_reporte_22.acciones3 = lr_monto_nss.acciones
                  LET lr_reporte_22.pesos3    = lr_monto_nss.pesos
            END CASE
         WHEN 4
         	  CASE lr_monto_nss.subcta
               WHEN "01"
                  LET lr_reporte_01.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_01.pesos4    = lr_monto_nss.pesos
               WHEN "02"
                  LET lr_reporte_02.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_02.pesos4    = lr_monto_nss.pesos
               WHEN "03"
                  LET lr_reporte_03.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_03.pesos4    = lr_monto_nss.pesos
               WHEN "05"
                  LET lr_reporte_05.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_05.pesos4    = lr_monto_nss.pesos
               WHEN "08"
                  LET lr_reporte_08.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_08.pesos4    = lr_monto_nss.pesos
               WHEN "13"
                  LET lr_reporte_13.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_13.pesos4    = lr_monto_nss.pesos
               WHEN "15"
                  LET lr_reporte_15.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_15.pesos4    = lr_monto_nss.pesos
               WHEN "17"
                  LET lr_reporte_17.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_17.pesos4    = lr_monto_nss.pesos
               WHEN "18"
                  LET lr_reporte_18.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_18.pesos4    = lr_monto_nss.pesos
               WHEN "22"
                  LET lr_reporte_22.acciones4 = lr_monto_nss.acciones
                  LET lr_reporte_22.pesos4    = lr_monto_nss.pesos
            END CASE
         WHEN 5
         	  CASE lr_monto_nss.subcta
               WHEN "01"
                  LET lr_reporte_01.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_01.pesos5    = lr_monto_nss.pesos
               WHEN "02"
                  LET lr_reporte_02.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_02.pesos5    = lr_monto_nss.pesos
               WHEN "03"
                  LET lr_reporte_03.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_03.pesos5    = lr_monto_nss.pesos
               WHEN "05"
                  LET lr_reporte_05.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_05.pesos5    = lr_monto_nss.pesos
               WHEN "08"
                  LET lr_reporte_08.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_08.pesos5    = lr_monto_nss.pesos
               WHEN "13"
                  LET lr_reporte_13.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_13.pesos5    = lr_monto_nss.pesos
               WHEN "15"
                  LET lr_reporte_15.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_15.pesos5    = lr_monto_nss.pesos
               WHEN "17"
                  LET lr_reporte_17.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_17.pesos5    = lr_monto_nss.pesos
               WHEN "18"
                  LET lr_reporte_18.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_18.pesos5    = lr_monto_nss.pesos
               WHEN "22"
                  LET lr_reporte_22.acciones5 = lr_monto_nss.acciones
                  LET lr_reporte_22.pesos5    = lr_monto_nss.pesos
            END CASE
      END CASE

      AFTER GROUP OF lr_monto_nss.nss
         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_01.subcta    USING "&&",                       ",",
                         lr_reporte_01.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_01.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_01.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_01.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_01.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_01.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_01.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_01.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_01.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_01.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_02.subcta    USING "&&",                       ",",
                         lr_reporte_02.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_02.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_02.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_02.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_02.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_02.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_02.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_02.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_02.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_02.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_03.subcta    USING "&&",                       ",",
                         lr_reporte_03.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_03.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_03.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_03.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_03.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_03.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_03.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_03.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_03.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_03.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_05.subcta    USING "&&",                       ",",
                         lr_reporte_05.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_05.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_05.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_05.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_05.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_05.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_05.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_05.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_05.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_05.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_08.subcta    USING "&&",                       ",",
                         lr_reporte_08.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_08.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_08.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_08.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_08.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_08.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_08.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_08.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_08.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_08.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_13.subcta    USING "&&",                       ",",
                         lr_reporte_13.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_13.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_13.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_13.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_13.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_13.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_13.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_13.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_13.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_13.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_15.subcta    USING "&&",                       ",",
                         lr_reporte_15.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_15.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_15.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_15.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_15.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_15.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_15.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_15.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_15.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_15.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_17.subcta    USING "&&",                       ",",
                         lr_reporte_17.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_17.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_17.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_17.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_17.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_17.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_17.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_17.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_17.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_17.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_18.subcta    USING "&&",                       ",",
                         lr_reporte_18.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_18.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_18.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_18.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_18.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_18.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_18.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_18.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_18.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_18.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

         PRINT COLUMN 01,lr_reporte.nss          ,",",
                         lr_reporte.curp         ,",",
                         lr_reporte.rfc          ,",",
                         lr_reporte.fnacimiento  ,",",
                         lr_reporte.fanio        ,",",

                         lr_reporte.status_cta   USING "&",",",
                         lr_reporte.sector       USING "&",",",
                         lr_reporte.criterio_sie USING "&",",",

                         lr_reporte_22.subcta    USING "&&",                       ",",
                         lr_reporte_22.acciones1 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio1      USING "######&.&&&&&&",           ",",
                         lr_reporte_22.pesos1    USING "#################&.&&&&&&",",",
                         lr_reporte_22.acciones2 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio2      USING "######&.&&&&&&",           ",",
                         lr_reporte_22.pesos2    USING "#################&.&&&&&&",",",
                         lr_reporte_22.acciones3 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio3      USING "######&.&&&&&&",           ",",
                         lr_reporte_22.pesos3    USING "#################&.&&&&&&",",",
                         lr_reporte_22.acciones4 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio4      USING "######&.&&&&&&",           ",",
                         lr_reporte_22.pesos4    USING "#################&.&&&&&&",",",
                         lr_reporte_22.acciones5 USING "##############&.&&&&&&",      ",",
                         lr_reporte.precio5      USING "######&.&&&&&&",           ",",
                         lr_reporte_22.pesos5    USING "#################&.&&&&&&",",",
                         lr_monto_nss.bnd_siefore USING "&",",",
                         lr_monto_nss.siefore_sol USING "&"

      LET gi_registros = gi_registros + 1
END REPORT
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
                 " FROM   safre_af:dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTAB037' ",
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
                 " AND    proceso_cod   = 'CTAB037'",
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
   FROM   safre_tmp:glo_valor_accion
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

   SELECT codigo_afore
   INTO   lr_archivo.codigo_afore
   FROM   safre_af:tab_afore_local

   LET lr_archivo.fecha_envio = gd_fecha_corte
   LET lr_archivo.tipo_inf    = 2 --5 SIEFORES

   LET lc_archivo_final = lr_archivo.fecha_envio  USING "YYYYMMDD", "_",
                          "AF_",
                          lr_archivo.codigo_afore USING "&&&", "_",
                          lr_archivo.tipo_inf     USING "&&&", ".",
                          "0000"

   LET lc_encabezado = "encabezado_inf",
                       lr_archivo.tipo_inf USING "&", "_",
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

   SELECT codigo_afore
   INTO   lr_encabezado.codigo_afore
   FROM   safre_af:tab_afore_local

   SELECT parametro5
	 INTO   lr_encabezado.fecha_envio
   FROM   safre_af:dis_ctrl_proceso
   WHERE  folio IN (SELECT MAX(folio)
                    FROM   safre_af:dis_ctrl_proceso
                    WHERE  proceso_cod = 'CTAB035'
                    AND    etapa_cod = 1)
   AND    proceso_cod   = 'CTAB035'
   AND    etapa_cod     = 1
   
   LET lr_encabezado.tipo_inf    = 2 --5 SIEFORES

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

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
      --LET gi_registros = gi_registros + 1
      PRINT COLUMN 01, lr_encabezado.codigo_afore USING "&&&",      ",",
                       lr_encabezado.fecha_envio  USING "YYYYMMDD", ",",
                       lr_encabezado.tipo_inf     USING "&", ",",
                       (gi_registros * 10) + 1    USING "#########&"
END REPORT
################################################################################