################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa RETL921  => REPORTE DE PRELIQUIACION DISPOSICIONES ISSSTE (PESOS)    #
#By                => CESAR DAVID CHAVEZ MARTINEZ                              #
#Fecha             => 26 DE OCTUBRE 2009                                       #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE gar_precios ARRAY[5] OF RECORD
   	         siefore SMALLINT,
   	         precio  LIKE glo_valor_accion.precio_del_dia
          END RECORD

   DEFINE gr_afore RECORD
   	      codigo_afore   SMALLINT,
          razon_social   CHAR(50)
        END RECORD

   DEFINE hoy DATE

   DEFINE gr_seg_modulo         RECORD LIKE seg_modulo.* ,
          w_tabafore            RECORD LIKE tab_afore_local.*

   DEFINE enter CHAR (1)
   DEFINE gc_archivo CHAR(200)
   DEFINE gc_usuario CHAR(8)

   DEFINE gr_captura RECORD
   	         folio           INTEGER,
   	         fecha_liquida DATE
   END RECORD

   DEFINE gr_disp RECORD
   	         nss                CHAR(11)     ,
             consecutivo        DECIMAL(11,0),
             tipo_retiro        CHAR(1)      ,
             tipo_seguro        CHAR(2)      ,
             tipo_pension       CHAR(2)      ,
             tipo_prestacion    SMALLINT     ,
             siefore            SMALLINT     ,
             regimen            CHAR(2)      ,
             nombre             CHAR(30)     ,

             pesos_retencion     ,
             pesos_bruto         ,
             pesos_neto            ,
             pesos_ret08         ,
             pesos_cv_cs         ,
             pesos_comp_ret      ,
             pesos_sar_92        ,
             pesos_ahorro_sol    ,
             pesos_fov08         ,
             pesos_fov92         ,
             pesos_rcv           DECIMAL(22,6)
   END RECORD

   DEFINE gr_parcial RECORD
          nss              CHAR(11)     ,
          consecutivo      DECIMAL(11,0),
          siefore          SMALLINT     ,
          diag_cuenta_ind  SMALLINT     ,
          tipo_desempleo   CHAR(01)     ,
          tipo_pago        SMALLINT     ,
          pago_desempleo   DECIMAL(22,6),
          nombre           CHAR(30)     ,
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6)
   END RECORD

   DEFINE gd_fecha_genera,
          gd_fecha_operacion DATE

   DEFINE gr_edo RECORD
        preliquidado SMALLINT,
        liquidado    SMALLINT
   END RECORD
END GLOBALS
################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETL921.log")
    CALL init()

    OPEN WINDOW RETL9211 AT 4,4 WITH FORM "RETL9211" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL921   REPORTE PRELIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE PESOS      " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INITIALIZE gr_captura.* TO null

    INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
    	  AFTER FIELD folio
    	  	IF gr_captura.folio IS NOT NULL THEN
    	  		 SELECT fecha_conversion
    	  		 INTO   gr_captura.fecha_liquida
    	  		 FROM   safre_tmp:tmp_ret_preliquida
    	  		 WHERE  folio = gr_captura.folio
    	  		 AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                        852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                        853, --C DISP.TRAB. BENEF. AMPARO PPP
                                        854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                        855) --E DISP.TRAB. AL AMPARO DE SU EDAD
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
             	  DISPLAY BY NAME gr_captura.fecha_liquida
             ELSE
             	  ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
             END IF
    	    END IF

    	  AFTER FIELD fecha_liquida
    	  	IF gr_captura.fecha_liquida IS NULL THEN
    	  		 IF gr_captura.folio IS NULL THEN
    	  		 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
    	  		 END IF
    	  	ELSE
    	  		 SELECT folio
    	  		 INTO   gr_captura.folio
    	  		 FROM   safre_tmp:tmp_ret_preliquida
    	  		 WHERE  fecha_conversion = gr_captura.fecha_liquida
    	  		 AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                        852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                        853, --C DISP.TRAB. BENEF. AMPARO PPP
                                        854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                        855) --E DISP.TRAB. AL AMPARO DE SU EDAD
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
             	  DISPLAY BY NAME gr_captura.folio
             ELSE
             	  ERROR "NO EXISTE INFORMACION PARA ESTA FECHA"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
             END IF
    	    END IF

        ON KEY (ESC)
        	 IF gr_captura.folio IS NULL AND gr_captura.fecha_liquida IS NULL THEN
        	 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
    	  		 	SLEEP 2
    	  		 	ERROR ""
    	  		 	NEXT FIELD folio
    	  	 END IF

    	  	 SELECT fecha_conversion
           INTO   gr_captura.fecha_liquida
           FROM   safre_tmp:tmp_ret_preliquida
           WHERE  folio = gr_captura.folio
           AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                      852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                      853, --C DISP.TRAB. BENEF. AMPARO PPP
                                      854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                      855) --E DISP.TRAB. AL AMPARO DE SU EDAD

           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
           	  ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
           	  SLEEP 2
           	  ERROR ""
           	  NEXT FIELD folio
           END IF

    	  	 EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
           PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " --AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso()
    DISPLAY " ARCHIVO:", gc_archivo --CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN
################################################################################
FUNCTION init()
    DEFINE lc_sql     CHAR(800)

    SELECT codigo_afore,
           razon_social
    INTO   gr_afore.codigo_afore,
           gr_afore.razon_social
    FROM   tab_afore_local

    LET HOY = TODAY

    SELECT *, USER
    INTO   w_tabafore.*, gc_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PRELIQUIDADO"
    
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

    #OBTENER NOMBRE
    LET lc_sql = " SELECT NVL(TRIM(nombres),' ')||' '||",
                        " NVL(TRIM(paterno),' ')||' '||",
                        " NVL(TRIM(materno),' ') ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "

    PREPARE get_nombre FROM lc_sql

    #Obtener montos por subcuenta
    LET lc_sql  = " SELECT subcuenta             ,      ",
      	          "        siefore               ,      ",
      	          "        SUM(monto_en_acciones),      ",
      	          "        SUM(monto_en_pesos)          ",
      	          " FROM   safre_tmp:tmp_ret_preliquida ",
      	          " WHERE  nss              =  ?        ",
      	          " AND    consecutivo_lote =  ?        ",
      	          " AND    folio            =  ?        ",
      	          " GROUP BY 1,2                        ",
      	          " ORDER BY 1,2                        "

    PREPARE eje_montos FROM lc_sql
    DECLARE cur_montos CURSOR FOR eje_montos

    #Obtener la retencion
    LET lc_sql  = " SELECT SUM(monto_en_pesos)          ",
      	          " FROM   safre_tmp:tmp_ret_preliquida ",
      	          " WHERE  nss              =  ?        ",
      	          " AND    consecutivo_lote =  ?        ",
      	          " AND    folio            =  ?        ",
      	          " AND    tipo_movimiento  = 10        "

    PREPARE get_retencion FROM lc_sql

    #Asignar ruta y nombre del reporte
    LET gc_archivo = gr_seg_modulo.ruta_listados CLIPPED,
                     "/", gc_usuario CLIPPED,
                     ".RPT_LIQ_",HOY USING"DDMMYYYY",".921"
END FUNCTION
################################################################################
FUNCTION primer_paso()
   DEFINE lr_montos RECORD
   	       subcuenta     SMALLINT,
           siefore       SMALLINT,
           acciones      DECIMAL(22,6),
           pesos         DECIMAL(22,6)
          END RECORD

   DEFINE li_siefore              ,
          li_cont_siefore SMALLINT,
          ld_precio       LIKE glo_valor_accion.precio_del_dia

   DEFINE lc_mensaje CHAR(100)

   #Verificar precios de accion
   SELECT UNIQUE fecha_conversion
   INTO   gr_captura.fecha_liquida
   FROM   safre_tmp:tmp_ret_preliquida
   WHERE  folio       = gr_captura.folio

   #Obtener precios de accion
   DECLARE cur_precios CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = gr_captura.fecha_liquida
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   LET li_cont_siefore = 0

   FOREACH cur_precios INTO li_siefore,
   	                        ld_precio

   	  LET gar_precios[li_siefore].siefore = li_siefore
   	  LET gar_precios[li_siefore].precio  = ld_precio

       LET li_cont_siefore = li_cont_siefore + 1
   END FOREACH

   IF li_cont_siefore < 5 THEN
   	  LET    lc_mensaje = "FALTA PRECIOS DE ACCION DEL DIA DE SIEFORES BASICAS DEL DIA:", gd_fecha_genera USING "DD/MM/YYYY"
      PROMPT lc_mensaje FOR CHAR enter
      EXIT PROGRAM
   END IF

   DECLARE cur_liq CURSOR FOR
   SELECT a.nss            ,
          a.consecutivo    ,
          a.tipo_retiro    ,
          a.tipo_seguro    ,
          a.tipo_pension   ,
          a.tipo_prestacion,
          b.siefore        ,
          a.regimen
   FROM   ret_sol_issste_tx a,
          ret_monto_issste  b
   WHERE  a.estado_solicitud IN (gr_edo.preliquidado, --30
                                 gr_edo.liquidado   ) --80
   AND    a.folio            = gr_captura.folio
   AND    a.curp             = b.curp
   AND    a.consecutivo      = b.consecutivo
   AND    a.folio            = b.folio
   ORDER  BY tipo_retiro    ,
             siefore        ,
             tipo_seguro    ,
             tipo_pension   ,
             tipo_prestacion,
             regimen

   START REPORT rpt_liquida TO gc_archivo

   FOREACH cur_liq INTO  gr_disp.nss            ,
                         gr_disp.consecutivo    ,
                         gr_disp.tipo_retiro    ,
                         gr_disp.tipo_seguro    ,
                         gr_disp.tipo_pension   ,
                         gr_disp.tipo_prestacion,
                         gr_disp.siefore        ,
                         gr_disp.regimen

      INITIALIZE lr_montos.* TO NULL

      #Obtener nombre
      EXECUTE get_nombre USING gr_disp.nss INTO gr_disp.nombre

      LET gr_disp.pesos_retencion  = 0
      LET gr_disp.pesos_bruto      = 0
      LET gr_disp.pesos_neto         = 0
      LET gr_disp.pesos_ret08      = 0
      LET gr_disp.pesos_cv_cs      = 0
      LET gr_disp.pesos_comp_ret   = 0
      LET gr_disp.pesos_sar_92     = 0
      LET gr_disp.pesos_ahorro_sol = 0
      LET gr_disp.pesos_fov08      = 0
      LET gr_disp.pesos_fov92      = 0
      LET gr_disp.pesos_rcv      = 0

      FOREACH cur_montos USING gr_disp.nss        ,
                               gr_disp.consecutivo,
                               gr_captura.folio
                         INTO  lr_montos.subcuenta,
                               lr_montos.siefore  ,
                               lr_montos.acciones ,
                               lr_montos.pesos

         IF lr_montos.acciones IS NULL THEN
         	 LET lr_montos.acciones = 0
         END IF

         IF lr_montos.pesos IS NULL THEN
         	 LET lr_montos.pesos = 0
         END IF

         CASE
         	  WHEN lr_montos.subcuenta = 30
         	  	 LET gr_disp.pesos_ret08 = gr_disp.pesos_ret08 + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 31 OR
         	  	   lr_montos.subcuenta = 32
         	  	 LET gr_disp.pesos_cv_cs = gr_disp.pesos_cv_cs + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 24 OR
         	  	   lr_montos.subcuenta = 25
         	  	 LET gr_disp.pesos_comp_ret = gr_disp.pesos_comp_ret + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 13
         	  	 LET gr_disp.pesos_sar_92 = gr_disp.pesos_sar_92 + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 33 OR
         	  	   lr_montos.subcuenta = 34
         	  	 LET gr_disp.pesos_ahorro_sol = gr_disp.pesos_ahorro_sol + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 35
         	  	 LET gr_disp.pesos_fov08 = gr_disp.pesos_fov08 + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 14
         	  	 LET gr_disp.pesos_fov92 = gr_disp.pesos_fov92 + lr_montos.pesos
         END CASE
      END FOREACH

      #Obtener retencion
      EXECUTE get_retencion USING gr_disp.nss        ,
                                  gr_disp.consecutivo,
                                  gr_captura.folio
                            INTO  gr_disp.pesos_retencion

      IF gr_disp.pesos_retencion IS NULL THEN
      	 LET gr_disp.pesos_retencion = 0
      END IF

      #Bruto
      LET gr_disp.pesos_bruto = gr_disp.pesos_ret08      +
                                gr_disp.pesos_cv_cs      +
                                gr_disp.pesos_comp_ret   +
                                gr_disp.pesos_sar_92     +
                                gr_disp.pesos_ahorro_sol

      #A pagar
      LET gr_disp.pesos_neto  = gr_disp.pesos_bruto     -
                                gr_disp.pesos_retencion +
                                gr_disp.pesos_fov08     +
                                gr_disp.pesos_fov92

      #RCV
      LET gr_disp.pesos_rcv = gr_disp.pesos_ret08      +
                              gr_disp.pesos_cv_cs

      OUTPUT TO REPORT rpt_liquida(gr_disp.*)
   END FOREACH

   FINISH REPORT rpt_liquida

   LET lc_mensaje = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_mensaje
   
   LET lc_mensaje = "lp ", gc_archivo CLIPPED
   RUN lc_mensaje
END FUNCTION
################################################################################
REPORT rpt_liquida(lr_rpt)
   DEFINE lr_rpt RECORD
             nss                CHAR(11)     ,
             consecutivo        DECIMAL(11,0),
             tipo_retiro        CHAR(1)      ,
             tipo_seguro        CHAR(2)      ,
             tipo_pension       CHAR(2)      ,
             tipo_prestacion    SMALLINT     ,
             siefore            SMALLINT     ,
             regimen            CHAR(2)      ,
             nombre             CHAR(30)     ,

             pesos_retencion     ,
             pesos_bruto         ,
             pesos_neto            ,
             pesos_ret08         ,
             pesos_cv_cs         ,
             pesos_comp_ret      ,
             pesos_sar_92        ,
             pesos_ahorro_sol    ,
             pesos_fov08         ,
             pesos_fov92         ,
             pesos_rcv           DECIMAL(22,6)
   END RECORD

   DEFINE
        encabezado            CHAR(60) ,
        var2                  CHAR(10) ,
        var1                  CHAR(10) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L3                    CHAR(03) ,
        L4                    CHAR(04) ,
        L5                    CHAR(05) ,
        L6                    CHAR(06) ,
        L7                    CHAR(07) ,
        L8                    CHAR(08) ,
        L9                    CHAR(09) ,
        L10                   CHAR(10) ,
        L11                   CHAR(11)

   DEFINE lar_total_retiro ARRAY[5] OF RECORD
   	         tipo_retiro       CHAR(01),

   	         lar_total_siefore array[5] OF RECORD
   	         	  siefore             SMALLINT,
   	            nss_tot             INTEGER ,
   	            pesos_retencion     ,
                pesos_bruto         ,
                pesos_neto            ,
                pesos_ret08         ,
                pesos_cv_cs         ,
                pesos_comp_ret      ,
                pesos_sar_92        ,
                pesos_ahorro_sol    ,
                pesos_fov08         ,
                pesos_fov92         ,
                pesos_rcv           DECIMAL(22,6)
             END RECORD
        END RECORD

   DEFINE lar_gran_total_siefore ARRAY[5] OF RECORD
   	      nss_tot             INTEGER ,
   	      siefore             SMALLINT,
   	      pesos_retencion     ,
          pesos_bruto         ,
          pesos_neto            ,
          pesos_ret08         ,
          pesos_cv_cs         ,
          pesos_comp_ret      ,
          pesos_sar_92        ,
          pesos_ahorro_sol    ,
          pesos_fov08         ,
          pesos_fov92         ,
          pesos_rcv           DECIMAL(22,6)
        END RECORD

   DEFINE lar_gran_total_retiro ARRAY[5] OF RECORD
   	      tipo_retiro       CHAR(01),
   	      nss_tot           INTEGER ,
          pesos_fov08         ,
          pesos_fov92         ,

          acciones_fov08      ,
          acciones_fov92      DECIMAL(22,6)
        END RECORD

   DEFINE lr_total_gral RECORD
   	      nss_tot             INTEGER ,
   	      pesos_retencion     ,
          pesos_bruto         ,
          pesos_neto            ,
          pesos_ret08         ,
          pesos_cv_cs         ,
          pesos_comp_ret      ,
          pesos_sar_92        ,
          pesos_ahorro_sol    ,
          pesos_fov08         ,
          pesos_fov92         ,
          pesos_rcv           DECIMAL(22,6)
        END RECORD

   DEFINE li_cont SMALLINT

   DEFINE li_siefore SMALLINT
   DEFINE li_page_length,
          li_page_length_totales SMALLINT

   OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    FIRST PAGE HEADER
       #Inicializar acumulador por tipo de retiro y siefore
       FOR li_cont = 1 TO 5

       	 #Inicializar acumulador por tipo de retiro
       	 CASE li_cont
       	 	  WHEN 1 LET lar_total_retiro[li_cont].tipo_retiro = "A" --DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
       	 	  WHEN 2 LET lar_total_retiro[li_cont].tipo_retiro = "B" --DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
       	 	  WHEN 3 LET lar_total_retiro[li_cont].tipo_retiro = "C" --DISP.TRAB. BENEF. AMPARO PPP
       	 	  WHEN 4 LET lar_total_retiro[li_cont].tipo_retiro = "D" --DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
       	 	  WHEN 5 LET lar_total_retiro[li_cont].tipo_retiro = "E" --DISP.TRAB. AL AMPARO DE SU EDAD
       	 END CASE

       	 #Inicializar acumulador por siefore
       	 FOR li_siefore = 1 TO 5
       	 	  LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].siefore          = li_siefore
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion  = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto         = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret   = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92     = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv        = 0
       	 END FOR

       	 LET lar_gran_total_retiro[li_cont].tipo_retiro    = ""
         LET lar_gran_total_retiro[li_cont].nss_tot        = 0
         LET lar_gran_total_retiro[li_cont].pesos_fov08    = 0
         LET lar_gran_total_retiro[li_cont].pesos_fov92    = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov08 = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov92 = 0

         LET lar_gran_total_siefore[li_cont].nss_tot          = 0
         LET lar_gran_total_siefore[li_cont].siefore          = li_cont
         LET lar_gran_total_siefore[li_cont].pesos_retencion  = 0
         LET lar_gran_total_siefore[li_cont].pesos_bruto      = 0
         LET lar_gran_total_siefore[li_cont].pesos_neto         = 0
         LET lar_gran_total_siefore[li_cont].pesos_ret08      = 0
         LET lar_gran_total_siefore[li_cont].pesos_cv_cs      = 0
         LET lar_gran_total_siefore[li_cont].pesos_comp_ret   = 0
         LET lar_gran_total_siefore[li_cont].pesos_sar_92     = 0
         LET lar_gran_total_siefore[li_cont].pesos_ahorro_sol = 0
         LET lar_gran_total_siefore[li_cont].pesos_fov08      = 0
         LET lar_gran_total_siefore[li_cont].pesos_fov92      = 0
         LET lar_gran_total_siefore[li_cont].pesos_rcv        = 0

      END FOR

       LET lr_total_gral.nss_tot          = 0
       LET lr_total_gral.pesos_retencion  = 0
       LET lr_total_gral.pesos_bruto      = 0
       LET lr_total_gral.pesos_neto         = 0
       LET lr_total_gral.pesos_ret08      = 0
       LET lr_total_gral.pesos_cv_cs      = 0
       LET lr_total_gral.pesos_comp_ret   = 0
       LET lr_total_gral.pesos_sar_92     = 0
       LET lr_total_gral.pesos_ahorro_sol = 0
       LET lr_total_gral.pesos_fov08      = 0
       LET lr_total_gral.pesos_fov92      = 0
       LET lr_total_gral.pesos_rcv        = 0

       LET li_page_length         = 45 - 7 --5 titulos, 1 detalle
       LET li_page_length_totales = 45 - 3 --5 titulos, 1 detalle

       LET L1  = "\304"
       LET L2  = "\304\304"
       LET L3  = "\304\304\304"
       LET L4  = "\304\304\304\304"
       LET L5  = "\304\304\304\304\304"
       LET L6  = "\304\304\304\304\304\304"
       LET L7  = "\304\304\304\304\304\304\304"
       LET L8  = "\304\304\304\304\304\304\304\304"
       LET L9 = "\304\304\304\304\304\304\304\304\304"
       LET L10 = "\304\304\304\304\304\304\304\304\304\304"
       LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

       LET encabezado = "M O D U L O   D E   R E T I R O S"

       PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
              '\033e\033(s23H'

       PRINT
            COLUMN 058,encabezado,
            '\033015'

       SKIP 1 LINES

       PRINT COLUMN 55,"REPORTE DE PRE-LIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE",
            '\033015'
       SKIP 2 LINES

       PRINT COLUMN 001, "AFORE               :", gr_afore.razon_social CLIPPED,
             COLUMN 206, "PAGINA              :" ,PAGENO USING "####",
             '\033015'

       PRINT COLUMN 001, "PROGRAMA            :RETL921",
             COLUMN 206, "FECHA LIQUIDACION   :",gr_captura.fecha_liquida USING"DD/MM/YYYY",
             '\033015'

       PRINT COLUMN 001, "FOLIO INTERNO       :",gr_captura.folio USING "#########&",
             COLUMN 206, "FECHA GENERACION    :",HOY USING "DD/MM/YYYY",
             '\033015'


       PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
             COLUMN 047, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
             COLUMN 093, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
             COLUMN 139, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
             COLUMN 185, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
             '\033015'

    PAGE HEADER
      PRINT COLUMN 001, "FOLIO INTERNO       :", gr_captura.folio USING "#########&",
            COLUMN 055, "REPORTE DE PRE-LIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE",
            COLUMN 206, "PAGINA              :" ,PAGENO USING "####",
            '\033015'

      PRINT COLUMN 001,L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L5, L2,L5,
                       '\033015'

    BEFORE GROUP OF lr_rpt.tipo_retiro
    	CASE lr_rpt.tipo_retiro
    		 WHEN "A" LET li_cont = 1
    		 WHEN "B" LET li_cont = 2
    		 WHEN "C" LET li_cont = 3
    		 WHEN "D" LET li_cont = 4
    		 WHEN "E" LET li_cont = 5
    	END CASE

   	  IF LINENO > li_page_length THEN
      	 SKIP TO TOP OF PAGE
      END IF

      #BLOQUE TITULOS 1
      PRINT COLUMN 001,"\332",L10,L1,              --nss
                       "\302",L10,L10,L10,         --nombre
                       "\302", L2,L1,              --tipo_seguro
                       "\302", L2,L1,              --tipo_pension
                       "\302", L2,                 --regimen
                       "\302",L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L10,L10,L5,L2,   --LINEA
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 2          --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",      --nss
            COLUMN 044,"|TP.",   --nombre
            COLUMN 048,"|TP.",   --tipo_seguro
            COLUMN 052,"|",      --tipo_pension
            COLUMN 055,"|",      --regimen
            COLUMN 110,"MONTOS PRE-LIQUIDADOS DE LA CUENTA INDIVIDUAL (PESOS) DEL TIPO RETIRO ",lr_rpt.tipo_retiro,
            COLUMN 243,"|",
                       '\033015'

     #BLOQUE TITULOS 3                    --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",               --nss
            COLUMN 044,"|",               --nombre
            COLUMN 048,"|",               --tipo_seguro
            COLUMN 052,"|",               --tipo_pension
            COLUMN 055,"\303",L2,         --regimen
            COLUMN 058,"\302",            --siefore
                               L10,L2,L2, --retencion
                       "\302", L10,L5,L1, --bruto
                       "\302", L10,L5,L1, --neto
                       "\301", L10,L5,L1, --retiro08
                       "\301", L10,L5,L1, --cv_cs
                       "\302", L10,L5,L1, --comp_ret
                       "\302", L10,L5,L1, --sar92
                       "\302", L10,L5,L1, --ahorro_sol
                       "\302", L10,L5,L1, --pesos_fov08
                       "\302", L10,L5,L1, --pesos_fov92
                       "\302", L10,L5,L1, --rcv
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 4
      PRINT COLUMN 001,"|    NSS",
            COLUMN 013,"|     NOMBRE DEL TRABAJADOR",
            COLUMN 044,"|D E",
            COLUMN 048,"|D E",
            COLUMN 052,"|RG",
            COLUMN 055,"|SB",
            COLUMN 058,"|  RETENCION",
            COLUMN 073,"|    IMPORTE",
            COLUMN 090,"|    IMPORTE",
            COLUMN 107,"|  RETIRO  08",
            COLUMN 124,"|    CV / CS",
            COLUMN 141,"| COMP.  RETIRO",
            COLUMN 158,"|     SAR 92",
            COLUMN 175,"|  AHORRO  SOL.",
            COLUMN 192,"| FOVISSSTE 08",
            COLUMN 209,"| FOVISSSTE 92",
            COLUMN 226,"|      RCV",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 5
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",
            COLUMN 044,"|SEG",
            COLUMN 048,"|PEN",
            COLUMN 052,"|",
            COLUMN 055,"|",
            COLUMN 058,"|    TOTAL",
            COLUMN 073,"|     BRUTO",
            COLUMN 090,"|      NETO",
            COLUMN 107,"|     PESOS",
            COLUMN 124,"|     PESOS",
            COLUMN 141,"|     PESOS",
            COLUMN 158,"|     PESOS",
            COLUMN 175,"|     PESOS",
            COLUMN 192,"|     PESOS",
            COLUMN 209,"|     PESOS",
            COLUMN 226,"|     PESOS",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 6                          --CIERRA
      PRINT COLUMN 001,"\300",L10,L1,            --nss
                       "\301",L10,L10,L10,       --nombre
                       "\301", L2,L1,            --tipo_seguro
                       "\301", L2,L1,            --tipo_pension
                       "\301", L2,               --regimen
                       "\301", L2,               --siefore
                       "\301",L10,L2,L2,         --retencion
                       "\301",L10,L5,L1,         --bruto
                       "\301",L10,L5,L1,         --neto
                       "\301",L10,L5,L1,         --retiro08
                       "\301",L10,L5,L1,         --cv_cs
                       "\301",L10,L5,L1,         --comp_ret
                       "\301",L10,L5,L1,         --sar92
                       "\301",L10,L5,L1,         --ahorro_sol
                       "\301",L10,L5,L1,         --pesos_fov08
                       "\301",L10,L5,L1,         --pesos_fov92
                       "\301",L10,L5,L1,         --rcv
                       "\331",
                       '\033015'
    ON EVERY ROW
      PRINT COLUMN 002, lr_rpt.nss                             ,
            COLUMN 014, lr_rpt.nombre                          ,
            COLUMN 046, lr_rpt.tipo_seguro                     ,
            COLUMN 050, lr_rpt.tipo_pension                    ,
            COLUMN 053, lr_rpt.regimen                         ,
            COLUMN 056, lr_rpt.siefore               USING "#&",

            COLUMN 059, lr_rpt.pesos_retencion       USING "##########&.&&",
            COLUMN 074, lr_rpt.pesos_bruto           USING "############&.&&",
            COLUMN 091, lr_rpt.pesos_neto            USING "############&.&&",
            COLUMN 108, lr_rpt.pesos_ret08           USING "############&.&&",
            COLUMN 125, lr_rpt.pesos_cv_cs           USING "############&.&&",
            COLUMN 142, lr_rpt.pesos_comp_ret        USING "############&.&&",
            COLUMN 159, lr_rpt.pesos_sar_92          USING "############&.&&",
            COLUMN 176, lr_rpt.pesos_ahorro_sol      USING "############&.&&",
            COLUMN 193, lr_rpt.pesos_fov08           USING "############&.&&",
            COLUMN 210, lr_rpt.pesos_fov92           USING "############&.&&",
            COLUMN 227, lr_rpt.pesos_rcv             USING "############&.&&",
            '\033015'

      LET li_siefore = lr_rpt.siefore

      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          + 1
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion  = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion  + lr_rpt.pesos_retencion
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto      + lr_rpt.pesos_bruto
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto       = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto       + lr_rpt.pesos_neto
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08      + lr_rpt.pesos_ret08
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs      + lr_rpt.pesos_cv_cs
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret   = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret   + lr_rpt.pesos_comp_ret
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92     = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92     + lr_rpt.pesos_sar_92
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol + lr_rpt.pesos_ahorro_sol
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08      + lr_rpt.pesos_fov08
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92      + lr_rpt.pesos_fov92
      LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv        = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv        + lr_rpt.pesos_rcv

    AFTER GROUP OF lr_rpt.tipo_retiro
    	SKIP 1 LINE

    	#IMPRIMIR TOTALES POR SIEFORE DEL TIPO RETIRO QUE TERMINA
    	FOR li_siefore = 1 TO 5
    		 IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN

    		 	  #Verificar que quede espacio en la pagina
    		 	  IF LINENO > li_page_length_totales THEN
      	       SKIP TO TOP OF PAGE
            END IF

            #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "\332",L10,L1,                    --nss
                              "\302",L10,L10,L10,L10,L1,        --totales
                              "\302",L2,                        --siefore
                              "\302",L10,L2,L2,                 --retencion
                              "\302",L10,L5,L1,                 --bruto
                              "\302",L10,L5,L1,                 --neto
                              "\302",L10,L5,L1,                 --retiro08
                              "\302",L10,L5,L1,                 --cv_cs
                              "\302",L10,L5,L1,                 --comp_ret
                              "\302",L10,L5,L1,                 --sar92
                              "\302",L10,L5,L1,                 --ahorro_sol
                              "\302",L10,L10,L10,L2,L1,         --viviendas
                              "\302",L10,L5,L1,                 --rcv
                              "\277",
                              '\033015'

            #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                  COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                  COLUMN 055, "|S", li_siefore USING "&",

                  COLUMN 058, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion       USING "##########&.&&",
                  COLUMN 073, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto           USING "############&.&&",
                  COLUMN 090, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto            USING "############&.&&",
                  COLUMN 107, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08           USING "############&.&&",
                  COLUMN 124, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs           USING "############&.&&",
                  COLUMN 141, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret        USING "############&.&&",
                  COLUMN 158, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92          USING "############&.&&",
                  COLUMN 175, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol      USING "############&.&&",
                  COLUMN 192, "|",
                  COLUMN 226, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv             USING "############&.&&",
                  COLUMN 243, "|",
                  '\033015'

            #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "\300",L10,L1,                    --nss
                              "\301",L10,L10,L10,L10,L1,        --totales
                              "\301",L2,                        --siefore
                              "\301",L10,L2,L2,                 --retencion
                              "\301",L10,L5,L1,                 --bruto
                              "\301",L10,L5,L1,                 --neto
                              "\301",L10,L5,L1,                 --retiro08
                              "\301",L10,L5,L1,                 --cv_cs
                              "\301",L10,L5,L1,                 --comp_ret
                              "\301",L10,L5,L1,                 --sar92
                              "\301",L10,L5,L1,                 --ahorro_sol
                              "\301",L10,L10,L10,L2,L1,         --viviendas
                              "\301",L10,L5,L1,                 --rcv
                              "\331",
                              '\033015'
            SKIP 1 LINE
            #Acumular al gran total por tipo de retiro
            LET lar_gran_total_retiro[li_cont].tipo_retiro    = lar_total_retiro[li_cont].tipo_retiro
            LET lar_gran_total_retiro[li_cont].nss_tot        = lar_gran_total_retiro[li_cont].nss_tot        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
            LET lar_gran_total_retiro[li_cont].pesos_fov08    = lar_gran_total_retiro[li_cont].pesos_fov08    + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08
            LET lar_gran_total_retiro[li_cont].pesos_fov92    = lar_gran_total_retiro[li_cont].pesos_fov92    + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92
    		 END IF
      END FOR

      #IMPRIMIR TOTAL POR TIPO DE RETIRO
      IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	   #Verificar que quede espacio en la pagina
    	   IF LINENO > li_page_length_totales THEN
            SKIP TO TOP OF PAGE
         END IF

         #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                           "\302",L10,L5,L2,                 --nss
                           "\302",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,
                                  L10,L5,L2,L1,              --espacios
                           "\302",L10,L5,L1,                 --pesos_fov08
                           "\302",L10,L5,L1,                 --pesos_fov92
                           "\302",L10,L5,L1,                 --rcv
                           "\277",
                           '\033015'

         #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
               COLUMN 055, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "################&",
               COLUMN 073, "|",
               COLUMN 192, "|", lar_gran_total_retiro[li_cont].pesos_fov08           USING "############&.&&",
               COLUMN 209, "|", lar_gran_total_retiro[li_cont].pesos_fov92           USING "############&.&&",
               COLUMN 226, "|",
               COLUMN 243, "|",
               '\033015'

         #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
         PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                           "\301",L10,L5,L2,                 --nss
                           "\301",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,
                                  L10,L5,L2,L1,              --espacios
                           "\301",L10,L5,L1,                 --pesos_fov08
                           "\301",L10,L5,L1,                 --pesos_fov92
                           "\301",L10,L5,L1,                 --rcv
                           "\331",
                           '\033015'
      END IF
      SKIP 2 LINES

    ON LAST ROW
       PRINT COLUMN 001, "R   E   S   U   M   E   N", '\033015'

       #IMPRIMIR TOTALES POR TIPO DE RETIRO Y SIEFORE
       FOR li_cont = 1 TO 5 --TIPO DE RETIRO
       	  FOR li_siefore = 1 TO 5
       	  	 IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN

    		 	      #Verificar que quede espacio en la pagina
    		 	      IF LINENO > li_page_length_totales THEN
      	           SKIP TO TOP OF PAGE
                END IF

                #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "\332",L10,L1,                    --nss
                                  "\302",L10,L10,L10,L10,L1,        --totales
                                  "\302",L2,                        --siefore
                                  "\302",L10,L2,L2,                 --retencion
                                  "\302",L10,L5,L1,                 --bruto
                                  "\302",L10,L5,L1,                 --neto
                                  "\302",L10,L5,L1,                 --retiro08
                                  "\302",L10,L5,L1,                 --cv_cs
                                  "\302",L10,L5,L1,                 --comp_ret
                                  "\302",L10,L5,L1,                 --sar92
                                  "\302",L10,L5,L1,                 --ahorro_sol
                                  "\302",L10,L10,L10,L2,L1,         --viviendas
                                  "\302",L10,L5,L1,                 --rcv
                                  "\277",
                                  '\033015'

                #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                      COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                      COLUMN 055, "|S", li_siefore USING "&",

                      COLUMN 058, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion       USING "##########&.&&",
                      COLUMN 073, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto           USING "############&.&&",
                      COLUMN 090, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto            USING "############&.&&",
                      COLUMN 107, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08           USING "############&.&&",
                      COLUMN 124, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs           USING "############&.&&",
                      COLUMN 141, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret        USING "############&.&&",
                      COLUMN 158, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92          USING "############&.&&",
                      COLUMN 175, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol      USING "############&.&&",
                      COLUMN 192, "|",
                      COLUMN 226, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv             USING "############&.&&",
                      COLUMN 243, "|",
                      '\033015'

                #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "\300",L10,L1,                    --nss
                                  "\301",L10,L10,L10,L10,L1,        --totales
                                  "\301",L2,                        --siefore
                                  "\301",L10,L2,L2,                 --retencion
                                  "\301",L10,L5,L1,                 --bruto
                                  "\301",L10,L5,L1,                 --neto
                                  "\301",L10,L5,L1,                 --retiro08
                                  "\301",L10,L5,L1,                 --cv_cs
                                  "\301",L10,L5,L1,                 --comp_ret
                                  "\301",L10,L5,L1,                 --sar92
                                  "\301",L10,L5,L1,                 --ahorro_sol
                                  "\301",L10,L10,L10,L2,L1,         --viviendas
                                  "\301",L10,L5,L1,                 --rcv
                                  "\331",
                                  '\033015'

                SKIP 1 LINE
                #Acumular a los totales por siefore
                LET lar_gran_total_siefore[li_siefore].nss_tot          = lar_gran_total_siefore[li_siefore].nss_tot          + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
                LET lar_gran_total_siefore[li_siefore].pesos_retencion  = lar_gran_total_siefore[li_siefore].pesos_retencion  + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_retencion
                LET lar_gran_total_siefore[li_siefore].pesos_bruto      = lar_gran_total_siefore[li_siefore].pesos_bruto      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_bruto
                LET lar_gran_total_siefore[li_siefore].pesos_neto       = lar_gran_total_siefore[li_siefore].pesos_neto       + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_neto
                LET lar_gran_total_siefore[li_siefore].pesos_ret08      = lar_gran_total_siefore[li_siefore].pesos_ret08      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ret08
                LET lar_gran_total_siefore[li_siefore].pesos_cv_cs      = lar_gran_total_siefore[li_siefore].pesos_cv_cs      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_cv_cs
                LET lar_gran_total_siefore[li_siefore].pesos_comp_ret   = lar_gran_total_siefore[li_siefore].pesos_comp_ret   + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_comp_ret
                LET lar_gran_total_siefore[li_siefore].pesos_sar_92     = lar_gran_total_siefore[li_siefore].pesos_sar_92     + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_sar_92
                LET lar_gran_total_siefore[li_siefore].pesos_ahorro_sol = lar_gran_total_siefore[li_siefore].pesos_ahorro_sol + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_ahorro_sol
                LET lar_gran_total_siefore[li_siefore].pesos_fov08      = lar_gran_total_siefore[li_siefore].pesos_fov08      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08
                LET lar_gran_total_siefore[li_siefore].pesos_fov92      = lar_gran_total_siefore[li_siefore].pesos_fov92      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92
                LET lar_gran_total_siefore[li_siefore].pesos_rcv        = lar_gran_total_siefore[li_siefore].pesos_rcv        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_rcv
             END IF
       	  END FOR --TERMINA DE IMPRIMIR TOTALES POR TIPO DE RETIRO Y SIEFORE

       	  #IMPRIMIR TOTAL POR TIPO DE RETIRO
          IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	       #Verificar que quede espacio en la pagina
    	       IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
             END IF

             #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                               "\302",L10,L5,L2,                 --nss
                               "\302",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,
                                      L10,L5,L2,L1,              --espacios
                               "\302",L10,L5,L1,                 --pesos_fov08
                               "\302",L10,L5,L1,                 --pesos_fov92
                               "\302",L10,L5,L1,                 --rcv
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
                   COLUMN 055, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "################&",
                   COLUMN 073, "|",
                   COLUMN 192, "|", lar_gran_total_retiro[li_cont].pesos_fov08           USING "############&.&&",
                   COLUMN 209, "|", lar_gran_total_retiro[li_cont].pesos_fov92           USING "############&.&&",
                   COLUMN 226, "|",
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
             PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                               "\301",L10,L5,L2,                 --nss
                               "\301",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,
                                      L10,L5,L2,L1,              --espacios
                               "\301",L10,L5,L1,                 --pesos_fov08
                               "\301",L10,L5,L1,                 --pesos_fov92
                               "\301",L10,L5,L1,                 --rcv
                               "\331",
                               '\033015'
             SKIP 1 LINE
          END IF
       END FOR --TERMINA DE IMPRIMIR TOTALES POR TIPO DE RETIRO

       SKIP 1 LINE
       #IMPRIMIR TOTALES POR SIEFORE
       FOR li_siefore = 1 TO 5
       	  IF lar_gran_total_siefore[li_siefore].nss_tot > 0 THEN
       	  	 IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
             END IF

             #BLOQUE 1 TITULO TOTALES POR SIEFORE
             PRINT COLUMN 001, "\332",L10,L1,                    --nss
                               "\302",L10,L10,L10,L10,L1,        --totales
                               "\302",L2,                        --siefore
                               "\302",L10,L2,L2,                 --retencion
                               "\302",L10,L5,L1,                 --bruto
                               "\302",L10,L5,L1,                 --neto
                               "\302",L10,L5,L1,                 --retiro08
                               "\302",L10,L5,L1,                 --cv_cs
                               "\302",L10,L5,L1,                 --comp_ret
                               "\302",L10,L5,L1,                 --sar92
                               "\302",L10,L5,L1,                 --ahorro_sol
                               "\302",L10,L10,L10,L2,L1,         --viviendas
                               "\302",L10,L5,L1,                 --rcv
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR SIEFORE
             PRINT COLUMN 001, "|", lar_gran_total_siefore[li_siefore].nss_tot  USING "##########&",
                   COLUMN 013, "|TOTAL POR SIEFORE",
                   COLUMN 055, "|S", li_siefore USING "&",
                   COLUMN 058, "|", lar_gran_total_siefore[li_siefore].pesos_retencion  USING "##########&.&&",
                   COLUMN 073, "|", lar_gran_total_siefore[li_siefore].pesos_bruto      USING "############&.&&",
                   COLUMN 090, "|", lar_gran_total_siefore[li_siefore].pesos_neto       USING "############&.&&",
                   COLUMN 107, "|", lar_gran_total_siefore[li_siefore].pesos_ret08      USING "############&.&&",
                   COLUMN 124, "|", lar_gran_total_siefore[li_siefore].pesos_cv_cs      USING "############&.&&",
                   COLUMN 141, "|", lar_gran_total_siefore[li_siefore].pesos_comp_ret   USING "############&.&&",
                   COLUMN 158, "|", lar_gran_total_siefore[li_siefore].pesos_sar_92     USING "############&.&&",
                   COLUMN 175, "|", lar_gran_total_siefore[li_siefore].pesos_ahorro_sol USING "############&.&&",
                   COLUMN 192, "|",
                   COLUMN 226, "|", lar_gran_total_siefore[li_siefore].pesos_rcv        USING "############&.&&",
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR SIEFORE
             PRINT COLUMN 001, "\300",L10,L1,                    --nss
                               "\301",L10,L10,L10,L10,L1,        --totales
                               "\301",L2,                        --siefore
                               "\301",L10,L2,L2,                 --retencion
                               "\301",L10,L5,L1,                 --bruto
                               "\301",L10,L5,L1,                 --neto
                               "\301",L10,L5,L1,                 --retiro08
                               "\301",L10,L5,L1,                 --cv_cs
                               "\301",L10,L5,L1,                 --comp_ret
                               "\301",L10,L5,L1,                 --sar92
                               "\301",L10,L5,L1,                 --ahorro_sol
                               "\301",L10,L10,L10,L2,L1,         --viviendas
                               "\301",L10,L5,L1,                 --rcv
                               "\331",
                               '\033015'
             SKIP 1 LINE
             #Acumular a la ultima linea de totales
             LET lr_total_gral.nss_tot          = lr_total_gral.nss_tot          + lar_gran_total_siefore[li_siefore].nss_tot
             LET lr_total_gral.pesos_retencion  = lr_total_gral.pesos_retencion  + lar_gran_total_siefore[li_siefore].pesos_retencion
             LET lr_total_gral.pesos_bruto      = lr_total_gral.pesos_bruto      + lar_gran_total_siefore[li_siefore].pesos_bruto
             LET lr_total_gral.pesos_neto       = lr_total_gral.pesos_neto       + lar_gran_total_siefore[li_siefore].pesos_neto
             LET lr_total_gral.pesos_ret08      = lr_total_gral.pesos_ret08      + lar_gran_total_siefore[li_siefore].pesos_ret08
             LET lr_total_gral.pesos_cv_cs      = lr_total_gral.pesos_cv_cs      + lar_gran_total_siefore[li_siefore].pesos_cv_cs
             LET lr_total_gral.pesos_comp_ret   = lr_total_gral.pesos_comp_ret   + lar_gran_total_siefore[li_siefore].pesos_comp_ret
             LET lr_total_gral.pesos_sar_92     = lr_total_gral.pesos_sar_92     + lar_gran_total_siefore[li_siefore].pesos_sar_92
             LET lr_total_gral.pesos_ahorro_sol = lr_total_gral.pesos_ahorro_sol + lar_gran_total_siefore[li_siefore].pesos_ahorro_sol
             LET lr_total_gral.pesos_fov08      = lr_total_gral.pesos_fov08      + lar_gran_total_siefore[li_siefore].pesos_fov08
             LET lr_total_gral.pesos_fov92      = lr_total_gral.pesos_fov92      + lar_gran_total_siefore[li_siefore].pesos_fov92
             LET lr_total_gral.pesos_rcv        = lr_total_gral.pesos_rcv        + lar_gran_total_siefore[li_siefore].pesos_rcv
       	  END IF
       END FOR

       #IMPRIMIR TOTAL DE NSS Y TOTALES DE VIVIENDA
       #Verificar que quede espacio en la pagina
    	 IF LINENO > li_page_length_totales THEN
          SKIP TO TOP OF PAGE
       END IF

       #BLOQUE 1 TITULO DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                         "\302",L10,L5,L2,                  --nss
                         "\302",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,
                                L10,L5,L2,L1,              --espacios
                         "\302",L10,L5,L1,                 --pesos_fov08
                         "\302",L10,L5,L1,                 --pesos_fov92
                         "\302",L10,L5,L1,                 --rcv
                         "\277",
                         '\033015'

       #BLOQUE 2 DETALLE DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS",
             COLUMN 055, "|", lr_total_gral.nss_tot               USING "################&",
             COLUMN 073, "|",
             COLUMN 192, "|", lr_total_gral.pesos_fov08           USING "############&.&&",
             COLUMN 209, "|", lr_total_gral.pesos_fov92           USING "############&.&&",
             COLUMN 226, "|",
             COLUMN 243, "|",
             '\033015'

       #BLOQUE 3 TITULO DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                         "\301",L10,L5,L2,                  --nss
                         "\301",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,
                                L10,L5,L2,L1,              --espacios
                         "\301",L10,L5,L1,                 --pesos_fov08
                         "\301",L10,L5,L1,                 --pesos_fov92
                         "\301",L10,L5,L1,                 --rcv
                         "\331",
                         '\033015'
END REPORT
################################################################################