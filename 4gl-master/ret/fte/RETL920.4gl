################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa RETL920  => REPORTE DE PROVISION DE DISPOSICIONES ISSSTE             #
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
   	         fecha_provision DATE
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
             nombre             CHAR(33)     ,

             acciones_tot        ,
             acciones_ret08      ,
             acciones_cv_cs      ,
             acciones_comp_ret   ,
             acciones_sar_92     ,
             acciones_ahorro_sol ,

             pesos_fov08         ,
             pesos_fov92         ,

             acciones_fov08      ,
             acciones_fov92      DECIMAL(22,6)
   END RECORD

   DEFINE gr_parcial RECORD
          nss              CHAR(11)     ,
          consecutivo      DECIMAL(11,0),
          siefore          SMALLINT     ,
          diag_cuenta_ind  SMALLINT     ,
          tipo_desempleo   CHAR(01)     ,
          tipo_pago        SMALLINT     ,
          pago_desempleo   DECIMAL(22,6),
          nombre           CHAR(33)     ,
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6)
   END RECORD

   DEFINE gd_fecha_genera,
          gd_fecha_operacion DATE

   DEFINE gr_edo RECORD
        enviado      SMALLINT,
        provisionado SMALLINT,
        recibido     SMALLINT
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

    CALL STARTLOG("RETL920.log")
    CALL init()

    OPEN WINDOW RETL9201 AT 4,4 WITH FORM "RETL9201" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL920    REPORTE, PROVISION DISPOSICION DE RECURSOS ISSSTE                  " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INITIALIZE gr_captura.* TO null

    INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
    	  AFTER FIELD folio
    	  	IF gr_captura.folio IS NOT NULL THEN
    	  		 SELECT fecha_conversion
    	  		 INTO   gr_captura.fecha_provision
    	  		 FROM   dis_provision
    	  		 WHERE  folio = gr_captura.folio
    	  		 AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                        852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                        853, --C DISP.TRAB. BENEF. AMPARO PPP
                                        854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                        855) --E DISP.TRAB. AL AMPARO DE SU EDAD
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
             	  DISPLAY BY NAME gr_captura.fecha_provision
             ELSE
             	  ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
             END IF
    	    END IF

    	  AFTER FIELD fecha_provision
    	  	IF gr_captura.fecha_provision IS NULL THEN
    	  		 IF gr_captura.folio IS NULL THEN
    	  		 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PROVISION"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
    	  		 END IF
    	  	ELSE
    	  		 SELECT folio
    	  		 INTO   gr_captura.folio
    	  		 FROM   dis_provision
    	  		 WHERE  fecha_conversion = gr_captura.fecha_provision
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
        	 IF gr_captura.folio IS NULL AND gr_captura.fecha_provision IS NULL THEN
        	 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PROVISION"
    	  		 	SLEEP 2
    	  		 	ERROR ""
    	  		 	NEXT FIELD folio
    	  	 END IF

    	  	 SELECT fecha_conversion
           INTO   gr_captura.fecha_provision
           FROM   dis_provision
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

    --LET gr_captura.folio = 34058

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
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECIBIDO"

    #OBTENER NOMBRE
    LET lc_sql = " SELECT NVL(TRIM(nombres),' ')||' '||",
                        " NVL(TRIM(paterno),' ')||' '||",
                        " NVL(TRIM(materno),' ') ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "

    PREPARE get_nombre FROM lc_sql

    #Obtener montos por subcuenta
    LET lc_sql  = " SELECT subcuenta             , ",
      	          "        siefore               , ",
      	          "        SUM(monto_en_acciones), ",
      	          "        SUM(monto_en_pesos)     ",
      	          " FROM   dis_provision           ",
      	          " WHERE  nss              =  ?   ",
      	          " AND    consecutivo_lote =  ?   ",
      	          " AND    folio            =  ?   ",
      	          " GROUP BY 1,2                   ",
      	          " ORDER BY 1,2                   "

    PREPARE eje_montos FROM lc_sql
    DECLARE cur_montos CURSOR FOR eje_montos

    #Asignar ruta y nombre del reporte
    LET gc_archivo = gr_seg_modulo.ruta_listados CLIPPED,
                     "/", gc_usuario CLIPPED,
                     ".RPT_PROV_",HOY USING"DDMMYYYY",".920"
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

   {SELECT fecha_operacion
   INTO   gd_fecha_operacion
   FROM   ret_cza_lote
   WHERE  folio = gr_folio.folio_12

   IF SQLCA.SQLCODE <> 0 THEN
      LET gd_fecha_operacion = ""
   END IF}

   #Verificar precios de accion

   #Obtener precios de accion

   DECLARE cur_prov CURSOR FOR
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
   WHERE  a.estado_solicitud IN (gr_edo.provisionado,
                                 gr_edo.enviado     ,
                                 gr_edo.recibido    )
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

   START REPORT rpt_provision TO gc_archivo

   FOREACH cur_prov INTO gr_disp.nss            ,
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

      LET gr_disp.acciones_tot        = 0
      LET gr_disp.acciones_ret08      = 0
      LET gr_disp.acciones_cv_cs      = 0
      LET gr_disp.acciones_comp_ret   = 0
      LET gr_disp.acciones_sar_92     = 0
      LET gr_disp.acciones_ahorro_sol = 0

      LET gr_disp.pesos_fov08         = 0
      LET gr_disp.pesos_fov92         = 0

      LET gr_disp.acciones_fov08      = 0
      LET gr_disp.acciones_fov92      = 0

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
         	  	 LET gr_disp.acciones_ret08 = gr_disp.acciones_ret08 + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 31 OR
         	  	   lr_montos.subcuenta = 32
         	  	 LET gr_disp.acciones_cv_cs = gr_disp.acciones_cv_cs + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 24 OR
         	  	   lr_montos.subcuenta = 25
         	  	 LET gr_disp.acciones_comp_ret = gr_disp.acciones_comp_ret + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 13
         	  	 LET gr_disp.acciones_sar_92 = gr_disp.acciones_sar_92 + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 33 OR
         	  	   lr_montos.subcuenta = 34
         	  	 LET gr_disp.acciones_ahorro_sol = gr_disp.acciones_ahorro_sol + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 35
         	  	 LET gr_disp.acciones_fov08 = gr_disp.acciones_fov08 + lr_montos.acciones
         	  	 LET gr_disp.pesos_fov08 = gr_disp.pesos_fov08 + lr_montos.pesos
         	  WHEN lr_montos.subcuenta = 14
         	  	 LET gr_disp.acciones_fov92 = gr_disp.acciones_fov92 + lr_montos.acciones
         	  	 LET gr_disp.pesos_fov92 = gr_disp.pesos_fov92 + lr_montos.pesos
         END CASE
      END FOREACH

      LET gr_disp.acciones_tot = gr_disp.acciones_ret08      +
                                 gr_disp.acciones_cv_cs      +
                                 gr_disp.acciones_comp_ret   +
                                 gr_disp.acciones_sar_92     +
                                 gr_disp.acciones_ahorro_sol

      OUTPUT TO REPORT rpt_provision(gr_disp.*)
   END FOREACH

   FINISH REPORT rpt_provision

   LET lc_mensaje = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_mensaje

   LET lc_mensaje = "lp ", gc_archivo CLIPPED
   RUN lc_mensaje
END FUNCTION
################################################################################
REPORT rpt_provision(lr_rpt)
   DEFINE lr_rpt RECORD
          nss                CHAR(11)     ,
          consecutivo        DECIMAL(11,0),
          tipo_retiro        CHAR(1)      ,
          tipo_seguro        CHAR(2)      ,
          tipo_pension       CHAR(2)      ,
          tipo_prestacion    SMALLINT     ,
          siefore            SMALLINT     ,
          regimen            CHAR(2)      ,
          nombre             CHAR(33)     ,

          acciones_tot        ,
          acciones_ret08      ,
          acciones_cv_cs      ,
          acciones_comp_ret   ,
          acciones_sar_92     ,
          acciones_ahorro_sol ,

          pesos_fov08         ,
          pesos_fov92         ,

          acciones_fov08      ,
          acciones_fov92      DECIMAL(22,6)
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
   	            acciones_tot        ,
                acciones_ret08      ,
                acciones_cv_cs      ,
                acciones_comp_ret   ,
                acciones_sar_92     ,
                acciones_ahorro_sol ,

                pesos_fov08         ,
                pesos_fov92         ,

                acciones_fov08      ,
                acciones_fov92      DECIMAL(22,6)
             END RECORD
        END RECORD

   DEFINE lar_gran_total_siefore ARRAY[5] OF RECORD
   	      nss_tot             INTEGER ,
   	      siefore             SMALLINT,
   	      acciones_tot        ,
          acciones_ret08      ,
          acciones_cv_cs      ,
          acciones_comp_ret   ,
          acciones_sar_92     ,
          acciones_ahorro_sol ,

          pesos_fov08         ,
          pesos_fov92         ,

          acciones_fov08      ,
          acciones_fov92      DECIMAL(22,6)
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
   	      acciones_tot        ,
          acciones_ret08      ,
          acciones_cv_cs      ,
          acciones_comp_ret   ,
          acciones_sar_92     ,
          acciones_ahorro_sol ,
          pesos_fov08         ,
          pesos_fov92         ,
          acciones_fov08      ,
          acciones_fov92      DECIMAL(22,6)
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
       	 	  LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].siefore             = li_siefore
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot             = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot        = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08         = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92         = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov08      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov92      = 0
       	 END FOR

       	 LET lar_gran_total_retiro[li_cont].tipo_retiro    = ""
         LET lar_gran_total_retiro[li_cont].nss_tot        = 0
         LET lar_gran_total_retiro[li_cont].pesos_fov08    = 0
         LET lar_gran_total_retiro[li_cont].pesos_fov92    = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov08 = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov92 = 0


       	 LET lar_gran_total_siefore[li_cont].nss_tot             = 0
         LET lar_gran_total_siefore[li_cont].siefore             = li_cont
         LET lar_gran_total_siefore[li_cont].acciones_tot        = 0
         LET lar_gran_total_siefore[li_cont].acciones_ret08      = 0
         LET lar_gran_total_siefore[li_cont].acciones_cv_cs      = 0
         LET lar_gran_total_siefore[li_cont].acciones_comp_ret   = 0
         LET lar_gran_total_siefore[li_cont].acciones_sar_92     = 0
         LET lar_gran_total_siefore[li_cont].acciones_ahorro_sol = 0
         LET lar_gran_total_siefore[li_cont].pesos_fov08         = 0
         LET lar_gran_total_siefore[li_cont].pesos_fov92         = 0
         LET lar_gran_total_siefore[li_cont].acciones_fov08      = 0
         LET lar_gran_total_siefore[li_cont].acciones_fov92      = 0
      END FOR

       LET lr_total_gral.nss_tot             = 0
       LET lr_total_gral.acciones_tot        = 0
       LET lr_total_gral.acciones_ret08      = 0
       LET lr_total_gral.acciones_cv_cs      = 0
       LET lr_total_gral.acciones_comp_ret   = 0
       LET lr_total_gral.acciones_sar_92     = 0
       LET lr_total_gral.acciones_ahorro_sol = 0
       LET lr_total_gral.pesos_fov08         = 0
       LET lr_total_gral.pesos_fov92         = 0
       LET lr_total_gral.acciones_fov08      = 0
       LET lr_total_gral.acciones_fov92      = 0

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

       PRINT COLUMN 63,"REPORTE DE PROVISION DE DISPOSICION DE RECURSOS ISSSTE",
            '\033015'
       SKIP 2 LINES

       PRINT COLUMN 001, "AFORE               :", gr_afore.razon_social CLIPPED,
             COLUMN 206, "PAGINA              :" ,PAGENO USING "####",
             '\033015'

       PRINT COLUMN 001, "PROGRAMA            :RETL920",
             COLUMN 206, "FECHA PROVISION     :",gr_captura.fecha_provision USING"DD/MM/YYYY",
             '\033015'

       PRINT COLUMN 001, "FOLIO INTERNO       :",gr_captura.folio USING "#########&",
             COLUMN 206, "FECHA GENERACION    :",HOY USING "DD/MM/YYYY",
             '\033015'


    PAGE HEADER
      PRINT COLUMN 001, "FOLIO INTERNO       :", gr_captura.folio USING "#########&",
            COLUMN 063, "REPORTE DE PROVISION DE DISPOSICION DE RECURSOS ISSSTE",
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
                       "\302",L10,L10,L10,L3,      --nombre
                       "\302", L5,L1,              --tipo_seguro
                       "\302", L5,L2,              --tipo_pension
                       "\302", L5,L1,              --tipo_prestacion
                       "\302", L2,L1,              --regimen
                       "\302",L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L2,L2,L5,        --LINEA
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 2          --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",      --nss
            COLUMN 047,"| TIPO", --nombre
            COLUMN 054,"| TIPO", --tipo_seguro
            COLUMN 062,"| TIPO", --tipo_pension
            COLUMN 069,"|",      --tipo_prestacion
            COLUMN 073,"|",      --regimen
            COLUMN 125,"MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES) RETIRO TIPO ",lr_rpt.tipo_retiro,
            COLUMN 243,"|",
                       '\033015'

     #BLOQUE TITULOS 3                    --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",               --nss
            COLUMN 047,"|",               --nombre
            COLUMN 054,"|",               --tipo_seguro
            COLUMN 062,"|",               --tipo_pension
            COLUMN 069,"|",               --tipo_prestacion
            COLUMN 073,"\303",L2,L1,      --regimen
            COLUMN 077,"\302",            --siefore
                               L10,L5,L1, --total_acciones
                       "\302", L10,L5,L1, --ret08
                       "\302", L10,L5,L1, --cv_cs
                       "\302", L10,L5,L1, --comp_ret
                       "\302", L10,L5,L1, --sar92
                       "\302", L10,L5,L1, --ahorro_vol
                       "\302", L10,L5,L1, --acciones_fov08
                       "\302", L10,L5,L1, --acciones_fov92
                       "\302", L10,L2,L2, --pesos_fov08
                       "\302", L10,L2,L2, --pesos_fov92
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 4
      PRINT COLUMN 001,"|    NSS",
            COLUMN 013,"|       NOMBRE DEL TRABAJADOR",
            COLUMN 047,"|  DE",
            COLUMN 054,"|  DE",
            COLUMN 062,"|  DE",
            COLUMN 069,"|REG",
            COLUMN 073,"|SIE",
            COLUMN 077,"|     TOTAL",
            COLUMN 094,"|    RETIRO 08",
            COLUMN 111,"|    CV / CS",
            COLUMN 128,"| COMP. RETIRO",
            COLUMN 145,"|     SAR 92",
            COLUMN 162,"|  AHORRO  SOL.",
            COLUMN 179,"|  FOVISSSTE 08",
            COLUMN 196,"|  FOVISSSTE 92",
            COLUMN 213,"| FOVISSSTE 08",
            COLUMN 228,"| FOVISSSTE 92",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 5
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",
            COLUMN 047,"|SEGURO",
            COLUMN 054,"|PENSION",
            COLUMN 062,"|PREST.",
            COLUMN 069,"|",
            COLUMN 073,"|",
            COLUMN 077,"|    ACCIONES",
            COLUMN 094,"|    ACCIONES",
            COLUMN 111,"|    ACCIONES",
            COLUMN 128,"|    ACCIONES",
            COLUMN 145,"|    ACCIONES",
            COLUMN 162,"|    ACCIONES",
            COLUMN 179,"| PARTICIPACIONES",
            COLUMN 196,"| PARTICIPACIONES",
            COLUMN 213,"|     PESOS",
            COLUMN 228,"|     PESOS",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 6                          --CIERRA
      PRINT COLUMN 001,"\300",L10,L1,            --nss
                       "\301",L10,L10,L10,L2,L1, --nombre
                       "\301", L5,L1,            --tipo_seguro
                       "\301", L5,L2,            --tipo_pension
                       "\301", L5,L1,            --tipo_prestacion
                       "\301", L2,L1,            --regimen
                       "\301", L2,L1,            --siefore
                       "\301",L10,L5,L1,         --total_acciones
                       "\301",L10,L5,L1,         --retiro08
                       "\301",L10,L5,L1,         --cv_cs
                       "\301",L10,L5,L1,         --comp_ret
                       "\301",L10,L5,L1,         --sar92
                       "\301",L10,L5,L1,         --ahorro_vol
                       "\301",L10,L5,L1,         --acciones_fov08
                       "\301",L10,L5,L1,         --acciones_fov92
                       "\301",L10,L2,L2,         --pesos_fov08
                       "\301",L10,L2,L2,         --pesos_fov92
                       "\331",
                       '\033015'

    ON EVERY ROW
      PRINT COLUMN 002, lr_rpt.nss                             ,
            COLUMN 014, lr_rpt.nombre                          ,
            COLUMN 050, lr_rpt.tipo_seguro                     ,
            COLUMN 057, lr_rpt.tipo_pension                    ,
            COLUMN 065, lr_rpt.tipo_prestacion       USING "#&",
            COLUMN 071, lr_rpt.regimen                         ,
            COLUMN 075, lr_rpt.siefore               USING "#&",
            COLUMN 078, lr_rpt.acciones_tot          USING "########&.&&&&&&",
            COLUMN 095, lr_rpt.acciones_ret08        USING "########&.&&&&&&",
            COLUMN 112, lr_rpt.acciones_cv_cs        USING "########&.&&&&&&",
            COLUMN 129, lr_rpt.acciones_comp_ret     USING "########&.&&&&&&",
            COLUMN 146, lr_rpt.acciones_sar_92       USING "########&.&&&&&&",
            COLUMN 163, lr_rpt.acciones_ahorro_sol   USING "########&.&&&&&&",
            COLUMN 180, lr_rpt.acciones_fov08        USING "########&.&&&&&&",
            COLUMN 197, lr_rpt.acciones_fov92        USING "########&.&&&&&&",
            COLUMN 214, lr_rpt.pesos_fov08           USING "##########&.&&"  ,
            COLUMN 229, lr_rpt.pesos_fov92           USING "##########&.&&"  ,
                        '\033015'

      LET li_siefore = lr_rpt.siefore

      IF li_siefore <> 0 THEN
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot             = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot             + 1
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot        = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot        + lr_rpt.acciones_tot
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      + lr_rpt.acciones_ret08
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      + lr_rpt.acciones_cv_cs
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   + lr_rpt.acciones_comp_ret
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     + lr_rpt.acciones_sar_92
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol + lr_rpt.acciones_ahorro_sol
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08         = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08         + lr_rpt.pesos_fov08
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92         = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92         + lr_rpt.pesos_fov92
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov08      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov08      + lr_rpt.acciones_fov08
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov92      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov92      + lr_rpt.acciones_fov92
      END IF

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
                              "\302",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                              "\302",L2,L1,                     --siefore
                              "\302",L10,L5,L1,                 --total_acciones
                              "\302",L10,L5,L1,                 --retiro08
                              "\302",L10,L5,L1,                 --cv_cs
                              "\302",L10,L5,L1,                 --comp_ret
                              "\302",L10,L5,L1,                 --sar92
                              "\302",L10,L5,L1,                 --ahorro_vol
                              "\302",L10,L5,L1,                 --acciones_fov08
                              "\302",L10,L5,L1,                 --acciones_fov92
                              "\302",L10,L2,L2,                 --pesos_fov08
                              "\302",L10,L2,L2,              --pesos_fov92
                              "\277",
                              '\033015'

            #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "##########&",
                  COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                  COLUMN 073, "|SB", li_siefore USING "&",
                  COLUMN 077, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot          USING "########&.&&&&&&",
                  COLUMN 094, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08        USING "########&.&&&&&&",
                  COLUMN 111, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs        USING "########&.&&&&&&",
                  COLUMN 128, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret     USING "########&.&&&&&&",
                  COLUMN 145, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92       USING "########&.&&&&&&",
                  COLUMN 162, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol   USING "########&.&&&&&&",
                  COLUMN 243, "|",
                  '\033015'

            #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001,"\300",L10,L1,                    --nss
                             "\301",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                             "\301",L2,L1,                     --siefore
                             "\301",L10,L5,L1,                 --total_acciones
                             "\301",L10,L5,L1,                 --retiro08
                             "\301",L10,L5,L1,                 --cv_cs
                             "\301",L10,L5,L1,                 --comp_ret
                             "\301",L10,L5,L1,                 --sar92
                             "\301",L10,L5,L1,                 --ahorro_vol
                             "\301",L10,L5,L1,                 --acciones_fov08
                             "\301",L10,L5,L1,                 --acciones_fov92
                             "\301",L10,L2,L2,                 --pesos_fov08
                             "\301",L10,L2,L2,              --pesos_fov92
                             "\331",
                             '\033015'

            SKIP 1 LINE
            #Acumular al gran total por tipo de retiro
            LET lar_gran_total_retiro[li_cont].tipo_retiro    = lar_total_retiro[li_cont].tipo_retiro
            LET lar_gran_total_retiro[li_cont].nss_tot        = lar_gran_total_retiro[li_cont].nss_tot        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
            LET lar_gran_total_retiro[li_cont].pesos_fov08    = lar_gran_total_retiro[li_cont].pesos_fov08    + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08
            LET lar_gran_total_retiro[li_cont].pesos_fov92    = lar_gran_total_retiro[li_cont].pesos_fov92    + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92
            LET lar_gran_total_retiro[li_cont].acciones_fov08 = lar_gran_total_retiro[li_cont].acciones_fov08 + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov08
            LET lar_gran_total_retiro[li_cont].acciones_fov92 = lar_gran_total_retiro[li_cont].acciones_fov92 + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov92
    		 END IF
      END FOR

      #IMPRIMIR TOTAL POR TIPO DE RETIRO
      IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	   #Verificar que quede espacio en la pagina
    	   IF LINENO > li_page_length_totales THEN
            SKIP TO TOP OF PAGE
         END IF

         #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L2, --titulo
                           "\302",L10,L10,                   --nss
                           "\302",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,L1,    --espacios
                           "\302",L10,L5,L1,                 --acciones_fov08
                           "\302",L10,L5,L1,                 --acciones_fov92
                           "\302",L10,L2,L2,                 --pesos_fov08
                           "\302",L10,L2,L2,              --pesos_fov92
                           "\277",
                           '\033015'

         #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
               COLUMN 056, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "###################&",
               COLUMN 077, "|",
               COLUMN 179, "|", lar_gran_total_retiro[li_cont].acciones_fov08        USING "########&.&&&&&&",
               COLUMN 196, "|", lar_gran_total_retiro[li_cont].acciones_fov92        USING "########&.&&&&&&",
               COLUMN 213, "|", lar_gran_total_retiro[li_cont].pesos_fov08           USING "##########&.&&"  ,
               COLUMN 228, "|", lar_gran_total_retiro[li_cont].pesos_fov92           USING "##########&.&&"  ,
               COLUMN 243, "|",
               '\033015'

         #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
         PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L2, --titulo
                           "\301",L10,L10,                   --nss
                           "\301",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,L1,    --espacios
                           "\301",L10,L5,L1,                 --acciones_fov08
                           "\301",L10,L5,L1,                 --acciones_fov92
                           "\301",L10,L2,L2,                 --pesos_fov08
                           "\301",L10,L2,L2,              --pesos_fov92
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
                                  "\302",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                                  "\302",L2,L1,                     --siefore
                                  "\302",L10,L5,L1,                 --total_acciones
                                  "\302",L10,L5,L1,                 --retiro08
                                  "\302",L10,L5,L1,                 --cv_cs
                                  "\302",L10,L5,L1,                 --comp_ret
                                  "\302",L10,L5,L1,                 --sar92
                                  "\302",L10,L5,L1,                 --ahorro_vol
                                  "\302",L10,L5,L1,                 --acciones_fov08
                                  "\302",L10,L5,L1,                 --acciones_fov92
                                  "\302",L10,L2,L2,                 --pesos_fov08
                                  "\302",L10,L2,L2,              --pesos_fov92
                                  "\277",
                                  '\033015'

                #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "##########&",
                      COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                      COLUMN 073, "|SB", li_siefore USING "&",
                      COLUMN 077, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot          USING "########&.&&&&&&",
                      COLUMN 094, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08        USING "########&.&&&&&&",
                      COLUMN 111, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs        USING "########&.&&&&&&",
                      COLUMN 128, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret     USING "########&.&&&&&&",
                      COLUMN 145, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92       USING "########&.&&&&&&",
                      COLUMN 162, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol   USING "########&.&&&&&&",
                      COLUMN 243, "|",
                      '\033015'

                #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001,"\300",L10,L1,                    --nss
                                 "\301",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                                 "\301",L2,L1,                     --siefore
                                 "\301",L10,L5,L1,                 --total_acciones
                                 "\301",L10,L5,L1,                 --retiro08
                                 "\301",L10,L5,L1,                 --cv_cs
                                 "\301",L10,L5,L1,                 --comp_ret
                                 "\301",L10,L5,L1,                 --sar92
                                 "\301",L10,L5,L1,                 --ahorro_vol
                                 "\301",L10,L5,L1,                 --acciones_fov08
                                 "\301",L10,L5,L1,                 --acciones_fov92
                                 "\301",L10,L2,L2,                 --pesos_fov08
                                 "\301",L10,L2,L2,              --pesos_fov92
                                 "\331",
                                 '\033015'
                SKIP 1 LINE

                #Acumular a los totales por siefore
                LET lar_gran_total_siefore[li_siefore].nss_tot             = lar_gran_total_siefore[li_siefore].nss_tot             + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
                LET lar_gran_total_siefore[li_siefore].acciones_tot        = lar_gran_total_siefore[li_siefore].acciones_tot        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot
                LET lar_gran_total_siefore[li_siefore].acciones_ret08      = lar_gran_total_siefore[li_siefore].acciones_ret08      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08
                LET lar_gran_total_siefore[li_siefore].acciones_cv_cs      = lar_gran_total_siefore[li_siefore].acciones_cv_cs      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs
                LET lar_gran_total_siefore[li_siefore].acciones_comp_ret   = lar_gran_total_siefore[li_siefore].acciones_comp_ret   + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret
                LET lar_gran_total_siefore[li_siefore].acciones_sar_92     = lar_gran_total_siefore[li_siefore].acciones_sar_92     + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92
                LET lar_gran_total_siefore[li_siefore].acciones_ahorro_sol = lar_gran_total_siefore[li_siefore].acciones_ahorro_sol + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol
                LET lar_gran_total_siefore[li_siefore].pesos_fov08         = lar_gran_total_siefore[li_siefore].pesos_fov08         + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov08
                LET lar_gran_total_siefore[li_siefore].pesos_fov92         = lar_gran_total_siefore[li_siefore].pesos_fov92         + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].pesos_fov92
                LET lar_gran_total_siefore[li_siefore].acciones_fov08      = lar_gran_total_siefore[li_siefore].acciones_fov08      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov08
                LET lar_gran_total_siefore[li_siefore].acciones_fov92      = lar_gran_total_siefore[li_siefore].acciones_fov92      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_fov92

             END IF
       	  END FOR --TERMINA DE IMPRIMIR TOTALES POR TIPO DE RETIRO Y SIEFORE

       	  #IMPRIMIR TOTAL POR TIPO DE RETIRO
          IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	       #Verificar que quede espacio en la pagina
    	       IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
             END IF

             #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L2, --titulo
                               "\302",L10,L10,                   --nss
                               "\302",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,L1,    --espacios
                               "\302",L10,L5,L1,                 --acciones_fov08
                               "\302",L10,L5,L1,                 --acciones_fov92
                               "\302",L10,L2,L2,                 --pesos_fov08
                               "\302",L10,L2,L2,                 --pesos_fov92
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
                   COLUMN 056, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "###################&",
                   COLUMN 077, "|",
                   COLUMN 179, "|", lar_gran_total_retiro[li_cont].acciones_fov08        USING "########&.&&&&&&",
                   COLUMN 196, "|", lar_gran_total_retiro[li_cont].acciones_fov92        USING "########&.&&&&&&",
                   COLUMN 213, "|", lar_gran_total_retiro[li_cont].pesos_fov08           USING "##########&.&&"  ,
                   COLUMN 228, "|", lar_gran_total_retiro[li_cont].pesos_fov92           USING "##########&.&&"  ,
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
             PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L2, --titulo
                               "\301",L10,L10,                   --nss
                               "\301",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,L1,    --espacios
                               "\301",L10,L5,L1,                 --acciones_fov08
                               "\301",L10,L5,L1,                 --acciones_fov92
                               "\301",L10,L2,L2,                 --pesos_fov08
                               "\301",L10,L2,L2,                 --pesos_fov92
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
                               "\302",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                               "\302",L2,L1,                     --siefore
                               "\302",L10,L5,L1,                 --total_acciones
                               "\302",L10,L5,L1,                 --retiro08
                               "\302",L10,L5,L1,                 --cv_cs
                               "\302",L10,L5,L1,                 --comp_ret
                               "\302",L10,L5,L1,                 --sar92
                               "\302",L10,L5,L1,                 --ahorro_vol
                               "\302",L10,L5,L1,                 --acciones_fov08
                               "\302",L10,L5,L1,                 --acciones_fov92
                               "\302",L10,L2,L2,                 --pesos_fov08
                               "\302",L10,L2,L2,                 --pesos_fov92
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
             PRINT COLUMN 001, "|", lar_gran_total_siefore[li_siefore].nss_tot  USING "##########&",
                   COLUMN 013, "|TOTAL POR SIEFORE",
                   COLUMN 073, "|SB", li_siefore USING "&",
                   COLUMN 077, "|", lar_gran_total_siefore[li_siefore].acciones_tot          USING "########&.&&&&&&",
                   COLUMN 094, "|", lar_gran_total_siefore[li_siefore].acciones_ret08        USING "########&.&&&&&&",
                   COLUMN 111, "|", lar_gran_total_siefore[li_siefore].acciones_cv_cs        USING "########&.&&&&&&",
                   COLUMN 128, "|", lar_gran_total_siefore[li_siefore].acciones_comp_ret     USING "########&.&&&&&&",
                   COLUMN 145, "|", lar_gran_total_siefore[li_siefore].acciones_sar_92       USING "########&.&&&&&&",
                   COLUMN 162, "|", lar_gran_total_siefore[li_siefore].acciones_ahorro_sol   USING "########&.&&&&&&",
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR SIEFORE
             PRINT COLUMN 001,"\300",L10,L1,                    --nss
                              "\301",L10,L10,L10,L10,L10,L5,L2,L2, --totales
                              "\301",L2,L1,                     --siefore
                              "\301",L10,L5,L1,                 --total_acciones
                              "\301",L10,L5,L1,                 --retiro08
                              "\301",L10,L5,L1,                 --cv_cs
                              "\301",L10,L5,L1,                 --comp_ret
                              "\301",L10,L5,L1,                 --sar92
                              "\301",L10,L5,L1,                 --ahorro_vol
                              "\301",L10,L5,L1,                 --acciones_fov08
                              "\301",L10,L5,L1,                 --acciones_fov92
                              "\301",L10,L2,L2,                 --pesos_fov08
                              "\301",L10,L2,L2,                 --pesos_fov92
                              "\331",
                              '\033015'
             SKIP 1 LINE
             #Acumular a la ultima linea de totales
             LET lr_total_gral.nss_tot             = lr_total_gral.nss_tot             + lar_gran_total_siefore[li_siefore].nss_tot
             LET lr_total_gral.acciones_tot        = lr_total_gral.acciones_tot        + lar_gran_total_siefore[li_siefore].acciones_tot
             LET lr_total_gral.acciones_ret08      = lr_total_gral.acciones_ret08      + lar_gran_total_siefore[li_siefore].acciones_ret08
             LET lr_total_gral.acciones_cv_cs      = lr_total_gral.acciones_cv_cs      + lar_gran_total_siefore[li_siefore].acciones_cv_cs
             LET lr_total_gral.acciones_comp_ret   = lr_total_gral.acciones_comp_ret   + lar_gran_total_siefore[li_siefore].acciones_comp_ret
             LET lr_total_gral.acciones_sar_92     = lr_total_gral.acciones_sar_92     + lar_gran_total_siefore[li_siefore].acciones_sar_92
             LET lr_total_gral.acciones_ahorro_sol = lr_total_gral.acciones_ahorro_sol + lar_gran_total_siefore[li_siefore].acciones_ahorro_sol
             LET lr_total_gral.pesos_fov08         = lr_total_gral.pesos_fov08         + lar_gran_total_siefore[li_siefore].pesos_fov08
             LET lr_total_gral.pesos_fov92         = lr_total_gral.pesos_fov92         + lar_gran_total_siefore[li_siefore].pesos_fov92
             LET lr_total_gral.acciones_fov08      = lr_total_gral.acciones_fov08      + lar_gran_total_siefore[li_siefore].acciones_fov08
             LET lr_total_gral.acciones_fov92      = lr_total_gral.acciones_fov92      + lar_gran_total_siefore[li_siefore].acciones_fov92
       	  END IF
       END FOR

       #IMPRIMIR TOTAL DE NSS Y TOTALES DE VIVIENDA
       #Verificar que quede espacio en la pagina
    	 IF LINENO > li_page_length_totales THEN
          SKIP TO TOP OF PAGE
       END IF

       #BLOQUE 1 TITULO DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L2, --titulo
                         "\302",L10,L10,                   --nss
                         "\302",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,L1,    --espacios
                         "\302",L10,L5,L1,                 --acciones_fov08
                         "\302",L10,L5,L1,                 --acciones_fov92
                         "\302",L10,L2,L2,                 --pesos_fov08
                         "\302",L10,L2,L2,                 --pesos_fov92
                         "\277",
                         '\033015'

       #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
       PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS",
             COLUMN 056, "|", lr_total_gral.nss_tot      USING "###################&",
             COLUMN 077, "|",
             COLUMN 179, "|", lr_total_gral.acciones_fov08        USING "########&.&&&&&&",
             COLUMN 196, "|", lr_total_gral.acciones_fov92        USING "########&.&&&&&&",
             COLUMN 213, "|", lr_total_gral.pesos_fov08           USING "##########&.&&"  ,
             COLUMN 228, "|", lr_total_gral.pesos_fov92           USING "##########&.&&"  ,
             COLUMN 243, "|",
             '\033015'

       #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
       PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L2, --titulo
                         "\301",L10,L10,                   --nss
                         "\301",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,L1,    --espacios
                         "\301",L10,L5,L1,                 --acciones_fov08
                         "\301",L10,L5,L1,                 --acciones_fov92
                         "\301",L10,L2,L2,                 --pesos_fov08
                         "\301",L10,L2,L2,                 --pesos_fov92
                         "\331",
                         '\033015'
END REPORT
################################################################################