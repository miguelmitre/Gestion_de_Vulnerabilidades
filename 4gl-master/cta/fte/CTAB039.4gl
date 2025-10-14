################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAB039    => ESTADO DE CUENTA                                       #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 11 de Marzo de 2007                                    #
################################################################################

DATABASE safre_tmp
################################################################################
GLOBALS
  DEFINE  gi_folio,
          gi_registros,
          gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE

  DEFINE gr_reporte RECORD
  	        afore_desc CHAR(14),
            precio1    DECIMAL(7,6),
            precio2    DECIMAL(7,6),
            precio3    DECIMAL(7,6),
            precio4    DECIMAL(7,6),
            precio5    DECIMAL(7,6),
            siefore1   CHAR(9),
            siefore2   CHAR(9),
            siefore3   CHAR(9),
            siefore4   CHAR(9),
            siefore5   CHAR(9),
            nss_ced1   INTEGER,
            nss_ced2   INTEGER,
            nss_sie1a2 INTEGER,
            nss_sie1a3 INTEGER,
            nss_sie1a4 INTEGER,
            nss_sie1a5 INTEGER,
            nss_sie2a1 INTEGER,
            nss_sie2a3 INTEGER,
            nss_sie2a4 INTEGER,
            nss_sie2a5 INTEGER,
            nss1       INTEGER,
            nss2       INTEGER,
            nss3       INTEGER,
            nss4       INTEGER,
            nss5       INTEGER
   END RECORD

END GLOBALS
################################################################################
MAIN
   LET gd_fecha_corte = ARG_VAL(1)
   
   DISPLAY "INICIA GENERACIÓN DE REPORTE ESTIMADO DE ACCIONES"

   CALL STARTLOG("CTAB039.log")

   CALL archivo()
END MAIN
################################################################################
FUNCTION archivo()
   DEFINE lc_nomarch     CHAR(80)

   DEFINE lr_reporte  RECORD
             siefore_ced    SMALLINT,
             acciones_ced   DECIMAL(22,6),
             pesos_ced      DECIMAL(22,6),
             subcta         SMALLINT,
             siefore_est    SMALLINT,
             acciones_est   DECIMAL(22,6)
          END RECORD

   DEFINE lc_prioridad CHAR(100),
          lc_comando   CHAR(200)

   LET lc_prioridad = "SET PDQPRIORITY HIGH"
   PREPARE prep_prioridadh FROM lc_prioridad
   --EXECUTE prep_prioridadh

   CALL det_afore()

   DECLARE cur_siefore CURSOR FOR
   SELECT a.siefore,
          a.subcuenta,
          SUM(a.monto_en_acciones)
   FROM   safre_tmp:cta_estatus_saldos a
   GROUP BY 1,2
   ORDER BY 1 DESC,2

   SELECT ruta_listados
   INTO   lc_nomarch
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   IF lc_nomarch IS NULL THEN
   	  ERROR "NO EXISTE LA RUTA INDICADA PARA EL REPORTE"
   	  RETURN
   END IF

   LET lc_nomarch = lc_nomarch CLIPPED,"/corte_acciones_est",
                    gd_fecha_corte USING "DDMMYYYY"
      
   START REPORT multisie_print TO lc_nomarch
   FOREACH cur_siefore INTO lr_reporte.siefore_ced,
   	                        lr_reporte.subcta,
   	                        lr_reporte.acciones_ced

   	  DECLARE cur_subcta CURSOR FOR
   	  SELECT a.siefore,
             SUM(a.monto_en_acciones)
      FROM   safre_tmp:cta_estatus_saldos_est a
      WHERE  a.subcuenta   = lr_reporte.subcta
      AND    a.siefore_ced = lr_reporte.siefore_ced
      GROUP BY 1
      ORDER BY 1

      FOREACH cur_subcta INTO lr_reporte.siefore_est,
      	                      lr_reporte.acciones_est
      OUTPUT TO REPORT multisie_print(lr_reporte.*)
      END FOREACH
   END FOREACH
   FINISH REPORT multisie_print


   LET lc_prioridad = "SET PDQPRIORITY LOW"
   PREPARE prep_prioridadl FROM lc_prioridad
   --EXECUTE prep_prioridadl

   LET lc_comando = "lp ",lc_nomarch
   RUN lc_comando

END FUNCTION
################################################################################
FUNCTION det_afore()
   DEFINE li_siefore SMALLINT,
   	      lc_desc_siefore CHAR(9)

   SELECT razon_social
   INTO   gr_reporte.afore_desc
   FROM   safre_af:tab_afore_local

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 1
   INTO   gr_reporte.precio1
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 1
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio1 IS NULL THEN
   	 LET gr_reporte.precio1 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 2
   INTO   gr_reporte.precio2
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 2
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio2 IS NULL THEN
   	 LET gr_reporte.precio2 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 3
   INTO   gr_reporte.precio3
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 3
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio3 IS NULL THEN
   	 LET gr_reporte.precio3 = 0
   END IF

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 4
   INTO   gr_reporte.precio4
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 4
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio4 IS NULL THEN
   	 LET gr_reporte.precio4 = 0
   END IF

   SELECT a.precio_del_dia  --PRECIO DE ACCION SIEFORE 5
   INTO   gr_reporte.precio5
   FROM   safre_tmp:glo_valor_accion a
   WHERE  a.codigo_siefore  = 5
   AND    a.fecha_valuacion = gd_fecha_corte

   IF gr_reporte.precio5 IS NULL THEN
   	 LET gr_reporte.precio5 = 0
   END IF

   DECLARE cur_det_sie CURSOR FOR
   SELECT codigo_siefore,
          razon_social
   FROM   safre_af:tab_siefore_local
   WHERE  codigo_siefore IN (1,2,3,4,5)

   FOREACH cur_det_sie INTO li_siefore,
   	                        lc_desc_siefore
   	  CASE li_siefore
   	  	 WHEN 1 LET gr_reporte.siefore1 = lc_desc_siefore
   	  	 WHEN 2 LET gr_reporte.siefore2 = lc_desc_siefore
   	  	 WHEN 3 LET gr_reporte.siefore3 = lc_desc_siefore
   	  	 WHEN 4 LET gr_reporte.siefore4 = lc_desc_siefore
   	  	 WHEN 5 LET gr_reporte.siefore5 = lc_desc_siefore
   	  END CASE
   END FOREACH

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_ced1
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 1

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_ced2
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 2

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie1a2
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 1
  AND    siefore = 2

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie1a3
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 1
  AND    siefore = 3

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie1a4
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 1
  AND    siefore = 4

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie1a5
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 1
  AND    siefore = 5

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie2a1
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 2
  AND    siefore = 1

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie2a3
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 2
  AND    siefore = 3

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie2a4
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 2
  AND    siefore = 4

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss_sie2a5
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore_ced = 2
  AND    siefore = 5

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss1
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore = 1

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss2
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore = 2

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss3
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore = 3

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss4
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore = 4

  SELECT SUM(total_nss)
  INTO   gr_reporte.nss5
  FROM   safre_tmp:cta_estatus_total_sie_est
  WHERE  siefore = 5

END FUNCTION
################################################################################
REPORT multisie_print(lr_reporte)
   DEFINE lr_reporte  RECORD
          siefore_ced    SMALLINT,
          acciones_ced   DECIMAL(22,6),
          pesos_ced      DECIMAL(22,6),
          subcta         SMALLINT,
          siefore_est    SMALLINT,
          acciones_est   DECIMAL(22,6)
       END RECORD

   DEFINE lc_siefore_desc CHAR(9),
          lc_desc_subcta  CHAR(49)

   DEFINE lr_reporte_sie1 RECORD
   	         acciones    DECIMAL(22,6),
   	         pesos       DECIMAL(22,6)
   	    END RECORD

   DEFINE lr_reporte_sie2 RECORD
   	         acciones    DECIMAL(22,6),
   	         pesos       DECIMAL(22,6)
   	    END RECORD

   DEFINE lr_reporte_sie3 RECORD
   	         acciones    DECIMAL(22,6),
   	         pesos       DECIMAL(22,6)
   	    END RECORD

   DEFINE lr_reporte_sie4 RECORD
   	         acciones    DECIMAL(22,6),
   	         pesos       DECIMAL(22,6)
   	    END RECORD

   DEFINE lr_reporte_sie5 RECORD
   	         acciones    DECIMAL(22,6),
   	         pesos       DECIMAL(22,6)
   	    END RECORD

   DEFINE ld_pesos_ced   ,
          ld_acciones_ced,
          ld_pesos1      ,
          ld_acciones1   ,
          ld_pesos2      ,
          ld_acciones2   ,
          ld_pesos3      ,
          ld_acciones3   ,
          ld_pesos4      ,
          ld_acciones4   ,
          ld_pesos5      ,
          ld_acciones5   DECIMAL(22,6)

   DEFINE lr_total RECORD
   	          subcuenta ,
   	          siefore   SMALLINT,
   	          pesos     ,
   	          acciones  ,
   	          pesos1    ,
   	          acciones1 ,
   	          pesos2    ,
   	          acciones2 ,
   	          pesos3    ,
   	          acciones3 ,
   	          pesos4    ,
   	          acciones4 ,
   	          pesos5    ,
   	          acciones5 DECIMAL(22,6)
   END RECORD

   DEFINE lr_subcuenta19 RECORD
   	   subcuenta ,
   	   siefore   SMALLINT,
   	   pesos     ,
   	   acciones  DECIMAL(22,6)
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      --PAGE LENGTH   1

   FORMAT
   FIRST PAGE HEADER
      PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s9H',
            '\033(s3B',        --NEGRITAS
            '\033*c4500a70B',  --#A Longitud, #B Grosor
            '\033*c0P',        --INVERSO
            '\033*v1t1O'       --TEXTO EN BCO.

      PRINT COLUMN 026, "REPORTE DE ACCIONES A TRANSFERIRI ANTES DEL CORTE",
            '\033(s15H',
            '\033(s3B',
            '\033*v0t1O',
            '\033015'
      PRINT COLUMN 001, gr_reporte.afore_desc,
            COLUMN 016, "Folio:", gi_folio USING "<<<,<<<,<<<",
            COLUMN 034, "Fecha:", TODAY USING "DD/MM/YYYY"
      PRINT COLUMN 32, "Precio de la accion por siefore",'\033015'
      PRINT '\033(s15H\033(s3B\033015',
            COLUMN  72 , gr_reporte.siefore1,
            COLUMN  82 , gr_reporte.precio1,
            COLUMN  98 , gr_reporte.siefore2,
            COLUMN 110, gr_reporte.precio2,
            COLUMN 126, gr_reporte.siefore3,
            COLUMN 138, gr_reporte.precio3,'\033015'
      PRINT '\033(s15H\033(s3B\033015',
            COLUMN  72, gr_reporte.siefore4,
            COLUMN  82, gr_reporte.precio4,
            COLUMN  98, gr_reporte.siefore5,
            COLUMN 110, gr_reporte.precio5,
            '\033(s27H',
            '\033015'

   BEFORE GROUP OF lr_reporte.siefore_ced
   	  SELECT razon_social
   	  INTO   lc_siefore_desc
      FROM   safre_af:tab_siefore_local
      WHERE  codigo_siefore = lr_reporte.siefore_ced

      --PRINT '\033(s15H\033(s3B\033015',
      --PRINT '\033e\033(s218T\033*p070x\033(s15H\033(s3B\033*c4500a120B',
        --    ' \033*c0P\033*v1t1O'
      PRINT '\033e\033(s15H',
            '\033&18d',
            '\033(s3B',        --NEGRITAS
            '\033*c4500a200B',  --#A Longitud, #B Grosor
            '\033*c0P',        --INVERSO
            '\033*v1t1O'       --TEXTO EN BCO.

      PRINT  COLUMN 93, "SALDOS A TRANSFERIR ", lc_siefore_desc,
            '\033(s15H',
            '\033(s3B',
            '\033&18d',
            '\033015'

      --PRINT '\033(s15H\033*c0P\033*v0t1O'
      PRINT COLUMN 80, "SIEFORE RECEPTORA EN LA QUE SE INVERTIRAN LOS RECURSOS",'\033015'
      IF lr_reporte.siefore_ced = 1 THEN
      	 PRINT '\033(s15H',
      	       '\033(s3B',
      	       '\033&18d',
      	       '\033015',
      	       COLUMN 46, "SALDOS A TRANSFERIR           SIE1               SIE2               SIE3               SIE4               SIE5",
      	       '\033(s16H',
               '\033(s3B',
               '\033&l8d',
      	       '\033015'
      ELSE
      	 PRINT '\033(s15H',
      	       '\033(s3B',
      	       '\033&18d',
      	       '\033015',
      	       COLUMN 46, "SALDOS A TRANSFERIR           SIE2               SIE1               SIE3               SIE4               SIE5",
      	       '\033(s16H',
               '\033(s3B',
               '\033&l8d',
               '\033015'

      END IF

      PRINT COLUMN 001, "Subcta  Subcuentas Obligatorias",
            '\033(s32H',
            '\033015',

            COLUMN 108, "Saldo en pesos   Saldo en acciones",
            COLUMN 154, "Saldo en pesos   Saldo en acciones",
            COLUMN 200, "Saldo en pesos   Saldo en acciones",
            COLUMN 246, "Saldo en pesos   Saldo en acciones",
            COLUMN 292, "Saldo en pesos   Saldo en acciones",
            COLUMN 338, "Saldo en pesos   Saldo en acciones",
            '\033*v0t1O',
            '\033015'
      SKIP 1 LINES

      LET ld_pesos_ced     = 0
      LET ld_acciones_ced  = 0
      LET ld_pesos1        = 0
      LET ld_acciones1     = 0
      LET ld_pesos2        = 0
      LET ld_acciones2     = 0
      LET ld_pesos3        = 0
      LET ld_acciones3     = 0
      LET ld_pesos4        = 0
      LET ld_acciones4     = 0
      LET ld_pesos5        = 0
      LET ld_acciones5     = 0

   	  BEFORE GROUP OF lr_reporte.subcta
   	  	 SELECT subct_desc
   	  	 INTO   lc_desc_subcta
   	  	 FROM   safre_af:tab_subcuenta
   	  	 WHERE  subct_cod = lr_reporte.subcta

         LET lr_reporte_sie1.acciones = 0
         LET lr_reporte_sie1.pesos    = 0
         LET lr_reporte_sie2.acciones = 0
         LET lr_reporte_sie2.pesos    = 0
         LET lr_reporte_sie3.acciones = 0
         LET lr_reporte_sie3.pesos    = 0
         LET lr_reporte_sie4.acciones = 0
         LET lr_reporte_sie4.pesos    = 0
         LET lr_reporte_sie5.acciones = 0
         LET lr_reporte_sie5.pesos    = 0

   	 ON EVERY ROW
   	    IF lr_reporte.siefore_ced = 1 THEN
   	  	   CASE lr_reporte.siefore_est
   	  	   	  WHEN 1 LET lr_reporte_sie1.pesos    = lr_reporte.acciones_est * gr_reporte.precio1
   	  	   	  	     LET lr_reporte_sie1.acciones = lr_reporte_sie1.pesos   / gr_reporte.precio1

   	  	   	  WHEN 2 LET lr_reporte_sie2.pesos    = lr_reporte.acciones_est * gr_reporte.precio2
   	  	   	  	     LET lr_reporte_sie2.acciones = lr_reporte_sie2.pesos   / gr_reporte.precio1

   	  	   	  WHEN 3 LET lr_reporte_sie3.pesos    = lr_reporte.acciones_est * gr_reporte.precio3
   	  	   	  	     LET lr_reporte_sie3.acciones = lr_reporte_sie3.pesos  / gr_reporte.precio1

   	  	   	  WHEN 4 LET lr_reporte_sie4.pesos    = lr_reporte.acciones_est * gr_reporte.precio4
   	  	   	  	     LET lr_reporte_sie4.acciones = lr_reporte_sie4.pesos   / gr_reporte.precio1

   	  	   	  WHEN 5 LET lr_reporte_sie5.pesos    = lr_reporte.acciones_est * gr_reporte.precio5
   	  	   	  	     LET lr_reporte_sie5.acciones = lr_reporte_sie5.pesos   / gr_reporte.precio1
   	  	   END CASE
   	  	ELSE
   	  	 	 CASE lr_reporte.siefore_est
   	  	  	  WHEN 1 LET lr_reporte_sie1.acciones = lr_reporte.acciones_est
   	  	  	  	     LET lr_reporte_sie1.pesos    = lr_reporte.acciones_est * gr_reporte.precio1
   	  	  	  WHEN 2 LET lr_reporte_sie2.acciones = lr_reporte.acciones_est
   	  	  	  	     LET lr_reporte_sie2.pesos    = lr_reporte.acciones_est * gr_reporte.precio2
   	  	  	  WHEN 3 LET lr_reporte_sie3.acciones = lr_reporte.acciones_est
   	  	  	  	     LET lr_reporte_sie3.pesos    = lr_reporte.acciones_est * gr_reporte.precio3
   	  	  	  WHEN 4 LET lr_reporte_sie4.acciones = lr_reporte.acciones_est
   	  	  	  	     LET lr_reporte_sie4.pesos    = lr_reporte.acciones_est * gr_reporte.precio4
   	  	  	  WHEN 5 LET lr_reporte_sie5.acciones = lr_reporte.acciones_est
   	  	  	  	     LET lr_reporte_sie5.pesos    = lr_reporte.acciones_est * gr_reporte.precio5
   	  	   END CASE
   	  	END IF

   	  	 IF lr_reporte.siefore_ced = 1 THEN
      	    LET lr_reporte.pesos_ced = lr_reporte.acciones_ced * gr_reporte.precio1
         ELSE
      	    LET lr_reporte.pesos_ced = lr_reporte.acciones_ced * gr_reporte.precio2
         END IF

   	  AFTER GROUP OF lr_reporte.subcta
   	  	LET ld_pesos_ced     = ld_pesos_ced    + lr_reporte.pesos_ced
   	  	LET ld_acciones_ced  = ld_acciones_ced + lr_reporte.acciones_ced
        LET ld_pesos1        = ld_pesos1       + lr_reporte_sie1.pesos
        LET ld_acciones1     = ld_acciones1    + lr_reporte_sie1.acciones
        LET ld_pesos2        = ld_pesos2       + lr_reporte_sie2.pesos
        LET ld_acciones2     = ld_acciones2    + lr_reporte_sie2.acciones
        LET ld_pesos3        = ld_pesos3       + lr_reporte_sie3.pesos
        LET ld_acciones3     = ld_acciones3    + lr_reporte_sie3.acciones
        LET ld_pesos4        = ld_pesos4       + lr_reporte_sie4.pesos
        LET ld_acciones4     = ld_acciones4    + lr_reporte_sie4.acciones
        LET ld_pesos5        = ld_pesos5       + lr_reporte_sie5.pesos
        LET ld_acciones5     = ld_acciones5    + lr_reporte_sie5.acciones

   	  	IF lr_reporte.siefore_ced = 1 THEN
   	  	   PRINT COLUMN 003, lr_reporte.subcta        USING "#&",
   	  	         COLUMN 009, lc_desc_subcta,
   	  	         COLUMN 059, lr_reporte.pesos_ced     USING "#################&.&&&&&&",
   	  	         COLUMN 085, lr_reporte.acciones_ced  USING "###########&.&&&&&&",
   	  	         COLUMN 105, lr_reporte_sie1.pesos    USING "#################&.&&&&&&",
   	  	         COLUMN 131, lr_reporte_sie1.acciones USING "###########&.&&&&&&",
   	  	         COLUMN 151, lr_reporte_sie2.pesos    USING "#################&.&&&&&&",
   	  	         COLUMN 177, lr_reporte_sie2.acciones USING "###########&.&&&&&&",
   	  	         COLUMN 197, lr_reporte_sie3.pesos    USING "#################&.&&&&&&",
   	  	         COLUMN 223, lr_reporte_sie3.acciones USING "###########&.&&&&&&",
   	  	         COLUMN 243, lr_reporte_sie4.pesos    USING "#################&.&&&&&&",
   	  	         COLUMN 269, lr_reporte_sie4.acciones USING "###########&.&&&&&&",
   	  	         COLUMN 289, lr_reporte_sie5.pesos    USING "#################&.&&&&&&",
   	  	         COLUMN 315, lr_reporte_sie5.acciones USING "###########&.&&&&&&",
   	  	         '\033015'
   	    ELSE
   	    	  PRINT COLUMN 003, lr_reporte.subcta        USING "#&",
   	  	          COLUMN 009, lc_desc_subcta,
   	  	          COLUMN 059, lr_reporte.pesos_ced     USING "#################&.&&&&&&",
   	  	          COLUMN 085, lr_reporte.acciones_ced  USING "###########&.&&&&&&",
   	  	       	  COLUMN 105, lr_reporte_sie2.pesos    USING "#################&.&&&&&&",
   	  	          COLUMN 131, lr_reporte_sie2.acciones USING "###########&.&&&&&&",
   	  	          COLUMN 151, lr_reporte_sie1.pesos    USING "#################&.&&&&&&",
   	  	          COLUMN 177, lr_reporte_sie1.acciones USING "###########&.&&&&&&",
   	  	          COLUMN 197, lr_reporte_sie3.pesos    USING "#################&.&&&&&&",
   	  	          COLUMN 223, lr_reporte_sie3.acciones USING "###########&.&&&&&&",
   	  	          COLUMN 243, lr_reporte_sie4.pesos    USING "#################&.&&&&&&",
   	  	          COLUMN 269, lr_reporte_sie4.acciones USING "###########&.&&&&&&",
   	  	          COLUMN 289, lr_reporte_sie5.pesos    USING "#################&.&&&&&&",
   	  	          COLUMN 315, lr_reporte_sie5.acciones USING "###########&.&&&&&&",      '\033015'
   	  	END IF
   AFTER GROUP OF lr_reporte.siefore_ced
   	  IF lr_reporte.siefore_ced = 1 THEN
   	  	 PRINT '\033(s32H',
               '\033(s3B',
               '\033&l8d',
               '\033*c4500a100B',   --#A Longitud, #B Grosor
               '\033*c0P',        --INVERSO
               '\033*v1t1O',      --TEXTO EN BCO.
               '\033015'
   	  	 PRINT COLUMN 001, "Saldo total de la siefore Transferente y Receptora",
   	  	       COLUMN 059, ld_pesos_ced     USING "#################&.&&&&&&",
   	  	       COLUMN 085, ld_acciones_ced  USING "###########&.&&&&&&",
   	  	       COLUMN 105, ld_pesos1        USING "#################&.&&&&&&",
   	  	       COLUMN 131, ld_acciones1     USING "###########&.&&&&&&",
   	  	       COLUMN 151, ld_pesos2        USING "#################&.&&&&&&",
   	  	       COLUMN 177, ld_acciones2     USING "###########&.&&&&&&",
   	  	       COLUMN 197, ld_pesos3        USING "#################&.&&&&&&",
   	  	       COLUMN 223, ld_acciones3     USING "###########&.&&&&&&",
   	  	       COLUMN 243, ld_pesos4        USING "#################&.&&&&&&",
   	  	       COLUMN 269, ld_acciones4     USING "###########&.&&&&&&",
   	  	       COLUMN 289, ld_pesos5        USING "#################&.&&&&&&",
   	  	       COLUMN 315, ld_acciones5     USING "###########&.&&&&&&",
   	  	       '\033015'
   	  	       
          PRINT COLUMN 001, " "
   	  	 {PRINT COLUMN 001, "Total de clientes de la siefore Transferente y Receptora",
   	  	       COLUMN 076, gr_reporte.nss_ced1   USING "###,###,###,##&",
   	  	       COLUMN 168, gr_reporte.nss_sie1a2 USING "###,###,###,##&",
   	  	       COLUMN 214, gr_reporte.nss_sie1a3 USING "###,###,###,##&",
   	  	       COLUMN 260, gr_reporte.nss_sie1a4 USING "###,###,###,##&",
   	  	       COLUMN 306, gr_reporte.nss_sie1a5 USING "###,###,###,##&",
   	  	       '\033015'}
   	  	       SKIP 2 LINES
   	  ELSE
   	  	 PRINT '\033(s32H',
               '\033(s3B',
               '\033&l8d',
               '\033*c4500a100B', --#A Longitud, #B Grosor
               '\033*c0P',        --INVERSO
               '\033*v1t1O',      --TEXTO EN BCO.
               '\033015'
   	  	 PRINT COLUMN 001, "Saldo total de la siefore Transferente y Receptora",
   	  	       COLUMN 059, ld_pesos_ced     USING "#################&.&&&&&&",
   	  	       COLUMN 085, ld_acciones_ced  USING "###########&.&&&&&&",
   	  	       COLUMN 105, ld_pesos2        USING "#################&.&&&&&&",
   	  	       COLUMN 131, ld_acciones2     USING "###########&.&&&&&&",
   	  	       COLUMN 151, ld_pesos1        USING "#################&.&&&&&&",
   	  	       COLUMN 177, ld_acciones1     USING "###########&.&&&&&&",
   	  	       COLUMN 197, ld_pesos3        USING "#################&.&&&&&&",
   	  	       COLUMN 223, ld_acciones3     USING "###########&.&&&&&&",
   	  	       COLUMN 243, ld_pesos4        USING "#################&.&&&&&&",
   	  	       COLUMN 269, ld_acciones4     USING "###########&.&&&&&&",
   	  	       COLUMN 289, ld_pesos5        USING "#################&.&&&&&&",
   	  	       COLUMN 315, ld_acciones5     USING "###########&.&&&&&&",
   	  	       '\033015'
          
          PRINT COLUMN 001, " "
   	  	 {PRINT COLUMN 001, "Total de clientes de la siefore Transferente y Receptora",
   	  	       COLUMN 076, gr_reporte.nss_ced2   USING "###,###,###,##&",
   	  	       COLUMN 168, gr_reporte.nss_sie2a1 USING "###,###,###,##&",
   	  	       COLUMN 214, gr_reporte.nss_sie2a3 USING "###,###,###,##&",
   	  	       COLUMN 260, gr_reporte.nss_sie2a4 USING "###,###,###,##&",
   	  	       COLUMN 306, gr_reporte.nss_sie2a5 USING "###,###,###,##&",
   	  	       '\033015'}

   	  END IF
{
   	ON LAST ROW
   	   LET ld_pesos1        = 0
       LET ld_acciones1     = 0
       LET ld_pesos2        = 0
       LET ld_acciones2     = 0
       LET ld_pesos3        = 0
       LET ld_acciones3     = 0
       LET ld_pesos4        = 0
       LET ld_acciones4     = 0
       LET ld_pesos5        = 0
       LET ld_acciones5     = 0

   	   SKIP 2 LINE
   	   PRINT '\033e\033(s15H',
            '\033&18d',
            '\033(s3B',        --NEGRITAS
            '\033*c4500a160B',  --#A Longitud, #B Grosor
            '\033*c0P',        --INVERSO
            '\033*v1t1O'       --TEXTO EN BCO.
   	   PRINT COLUMN  93, "S A L D O S  G L O B A L E S",
   	         '\033015'
       PRINT COLUMN  80, "SIEFORE RECEPTORA EN LA QUE SE INVERTIRAN LOS RECURSOS",
             '\033015'
       PRINT COLUMN  45, "SIE1                      SIE2                      SIE3",
             COLUMN 101,     "                      SIE4                      SIE5",

            '\033(s16H',
            '\033(s3B',
            '\033&l8d',
            '\033015'

      PRINT COLUMN 001, "Subcta  Subcuentas Obligatorias",
            '\033(s27H',
            '\033015',
             COLUMN 108, "Saldo en pesos   Saldo en acciones",
             COLUMN 154, "Saldo en pesos   Saldo en acciones",
             COLUMN 200, "Saldo en pesos   Saldo en acciones",
             COLUMN 246, "Saldo en pesos   Saldo en acciones",
             COLUMN 292, "Saldo en pesos   Saldo en acciones",
            '\033*v0t1O',      --TEXTO EN NEGRO
            '\033015'

   	   DECLARE cur_total_subcuenta_print CURSOR FOR
   	   SELECT subcuenta
       FROM   safre_tmp:cta_estatus_saldos_est
       --WHERE  subcuenta IN (1,2,5,6,7,9,13,17,18)
       GROUP BY 1
       ORDER BY 1

   	   FOREACH cur_total_subcuenta_print INTO lr_total.subcuenta
   	   	  DECLARE cur_totales_print CURSOR FOR
   	   	  SELECT siefore,
   	   	         SUM(a.monto_en_acciones) *
                 (SELECT precio_del_dia
                  FROM   safre_tmp:glo_valor_accion
                  WHERE  codigo_siefore  = a.siefore
                  AND    fecha_valuacion = gd_fecha_corte
                 ),
                 SUM(a.monto_en_acciones)
          FROM   safre_tmp:cta_estatus_saldos_est a
          WHERE  subcuenta = lr_total.subcuenta
          GROUP BY 1
          ORDER BY 1

          LET lr_total.pesos1    = 0
          LET lr_total.acciones1 = 0
          LET lr_total.pesos2    = 0
          LET lr_total.acciones2 = 0
          LET lr_total.pesos3    = 0
          LET lr_total.acciones3 = 0
          LET lr_total.pesos4    = 0
          LET lr_total.acciones4 = 0
          LET lr_total.pesos5    = 0
          LET lr_total.acciones5 = 0

          FOREACH cur_totales_print INTO lr_total.siefore,
          	                       lr_total.pesos,
          	                       lr_total.acciones
             CASE lr_total.siefore
             	  WHEN 1 LET lr_total.pesos1    = lr_total.pesos
             	  	     LET lr_total.acciones1 = lr_total.acciones
             	  WHEN 2 LET lr_total.pesos2    = lr_total.pesos
             	  	     LET lr_total.acciones2 = lr_total.acciones
             	  WHEN 3 LET lr_total.pesos3    = lr_total.pesos
             	  	     LET lr_total.acciones3 = lr_total.acciones
             	  WHEN 4 LET lr_total.pesos4    = lr_total.pesos
             	  	     LET lr_total.acciones4 = lr_total.acciones
             	  WHEN 5 LET lr_total.pesos5    = lr_total.pesos
             	  	     LET lr_total.acciones5 = lr_total.acciones
             END CASE
          END FOREACH

          SELECT subct_desc
   	  	  INTO   lc_desc_subcta
   	  	  FROM   safre_af:tab_subcuenta
   	  	  WHERE  subct_cod = lr_total.subcuenta

          PRINT COLUMN 003, lr_total.subcuenta  USING "#&",
   	  	        COLUMN 009, lc_desc_subcta,
   	  	        COLUMN 059, lr_total.pesos1     USING "#################&.&&&&&&",
   	  	        COLUMN 085, lr_total.acciones1  USING "###########&.&&&&&&",
   	  	       	COLUMN 105, lr_total.pesos2     USING "#################&.&&&&&&",
   	  	        COLUMN 131, lr_total.acciones2  USING "###########&.&&&&&&",
   	  	        COLUMN 151, lr_total.pesos3     USING "#################&.&&&&&&",
   	  	        COLUMN 177, lr_total.acciones3  USING "###########&.&&&&&&",
   	  	        COLUMN 197, lr_total.pesos4     USING "#################&.&&&&&&",
   	  	        COLUMN 223, lr_total.acciones4  USING "###########&.&&&&&&",
   	  	        COLUMN 243, lr_total.pesos5     USING "#################&.&&&&&&",
   	  	        COLUMN 269, lr_total.acciones5  USING "###########&.&&&&&&",
   	  	        '\033015'

   	  	   LET ld_pesos1        = ld_pesos1    + lr_total.pesos1
           LET ld_acciones1     = ld_acciones1 + lr_total.acciones1
           LET ld_pesos2        = ld_pesos2    + lr_total.pesos2
           LET ld_acciones2     = ld_acciones2 + lr_total.acciones2
           LET ld_pesos3        = ld_pesos3    + lr_total.pesos3
           LET ld_acciones3     = ld_acciones3 + lr_total.acciones3
           LET ld_pesos4        = ld_pesos4    + lr_total.pesos4
           LET ld_acciones4     = ld_acciones4 + lr_total.acciones4
           LET ld_pesos5        = ld_pesos5    + lr_total.pesos5
           LET ld_acciones5     = ld_acciones5 + lr_total.acciones5
   	   END FOREACH

       PRINT '\033(s27H',
             '\033(s3B',
             '\033&l8d',
             '\033*c4500a100B',   --#A Longitud, #B Grosor
             '\033*c0P',        --INVERSO
             '\033*v1t1O',      --TEXTO EN BCO.
             '\033015'
   	   PRINT COLUMN 001, "Saldo global de la siefore Transferente y Receptora",
 	  	       COLUMN 059, ld_pesos1    USING "#################&.&&&&&&",
 	  	       COLUMN 085, ld_acciones1 USING "###########&.&&&&&&",
 	  	       COLUMN 105, ld_pesos2    USING "#################&.&&&&&&",
 	  	       COLUMN 131, ld_acciones2 USING "###########&.&&&&&&",
 	  	       COLUMN 151, ld_pesos3    USING "#################&.&&&&&&",
 	  	       COLUMN 177, ld_acciones3 USING "###########&.&&&&&&",
 	  	       COLUMN 197, ld_pesos4    USING "#################&.&&&&&&",
 	  	       COLUMN 223, ld_acciones4 USING "###########&.&&&&&&",
 	  	       COLUMN 243, ld_pesos5    USING "#################&.&&&&&&",
 	  	       COLUMN 269, ld_acciones5 USING "###########&.&&&&&&",
 	  	       '\033015'
       PRINT COLUMN 001, " "
 	  	 PRINT COLUMN 001, "Total de clientes de la siefore Transferente y Receptora",
 	  	       COLUMN 076, gr_reporte.nss1 USING "###,###,###,##&",
 	  	       COLUMN 122, gr_reporte.nss2 USING "###,###,###,##&",
 	  	       COLUMN 168, gr_reporte.nss3 USING "###,###,###,##&",
 	  	       COLUMN 214, gr_reporte.nss4 USING "###,###,###,##&",
 	  	       COLUMN 260, gr_reporte.nss5 USING "###,###,###,##&",
 	  	       '\033015'

}
END REPORT
################################################################################