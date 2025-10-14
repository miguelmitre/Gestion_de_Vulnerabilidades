################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa RETL825  => REPORTE DE PROVISION DE RETIRO PARCIAL A, B Y D          #
#By                => CESAR DAVID CHAVEZ MARTINEZ                              #
#Fecha             => 8 DE MAYO DE 2009                                        #
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

   DEFINE gr_folio RECORD
   	         folio_13,
   	         folio_12 INTEGER
   	      END RECORD

   DEFINE gr_parcial RECORD
          nss              CHAR(11)     ,
          consecutivo      DECIMAL(11,0),
          siefore          SMALLINT     ,
          diag_cuenta_ind  SMALLINT     ,
          tipo_desempleo   CHAR(01)     ,
          tipo_pago        SMALLINT     ,
          pago_desempleo   DECIMAL(22,6),
          nombre           CHAR(35)     ,
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6)
   END RECORD

   DEFINE gd_fecha_genera,
          gd_fecha_operacion DATE
END GLOBALS
################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETL825.log")
    CALL init()

    OPEN WINDOW retl8251 AT 4,4 WITH FORM "RETL8251" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL825    REPORTE, PROVISION RETIRO PARCIAL POR DESEMPLEO                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME gr_folio.folio_12 WITHOUT DEFAULTS
        AFTER FIELD folio_12
           IF gr_folio.folio_12 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_12
           END IF

           SELECT "X"
           FROM   ret_parcial a
           WHERE  a.folio       = gr_folio.folio_12
           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
              ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
              SLEEP 2
              ERROR ""
              NEXT FIELD folio_12
           END IF

        ON KEY (ESC)
           IF gr_folio.folio_12 IS NULL THEN
               ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
               NEXT FIELD folio_12
           END IF

           SELECT "X"
           FROM   ret_parcial a
           WHERE  a.folio       = gr_folio.folio_12
           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
              ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
              SLEEP 2
              ERROR ""
              NEXT FIELD folio_12
           END IF

           EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
           PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso()
    DISPLAY " ARCHIVO:", gc_archivo CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN
################################################################################
FUNCTION init()
    DEFINE lc_sql     CHAR(200)

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

    {SELECT fecha_operacion
    INTO   gd_fecha_operacion
    FROM   ret_cza_lote
    WHERE  folio = gr_folio.folio_12

    IF SQLCA.SQLCODE <> 0 THEN
       LET gd_fecha_operacion = ""
    END IF}

    #OBTENER NOMBRE
    LET lc_sql = " SELECT NVL(TRIM(nombres),' ')||' '||",
                        " NVL(TRIM(paterno),' ')||' '||",
                        " NVL(TRIM(materno),' ') ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "

    PREPARE get_nombre FROM lc_sql

    #Obtener montos por subcuenta
    LET lc_sql  = "EXECUTE FUNCTION fn_calcula_pago_sbc_parciales(?,?,?)"

    PREPARE eje_pago_subcta FROM lc_sql
    DECLARE cur_pago_subcta CURSOR FOR eje_pago_subcta

    #Asignar ruta y nombre del reporte
    LET gc_archivo = gr_seg_modulo.ruta_listados CLIPPED,
                     "/RPT_PROV_",HOY USING"DDMMYYYY",".825"
END FUNCTION
################################################################################
FUNCTION primer_paso()
   DEFINE lr_pago_subcta RECORD
   	       subcuenta     SMALLINT,
           siefore       SMALLINT,
           saldo_actual  DECIMAL(16,6),
           acciones_pago DECIMAL(16,6),
           pesos_pago    DECIMAL(16,6)
          END RECORD

   DEFINE li_siefore              ,
          li_cont_siefore SMALLINT,
          ld_precio       LIKE glo_valor_accion.precio_del_dia

   DEFINE lc_mensaje CHAR(100)

   SELECT fecha_operacion
   INTO   gd_fecha_operacion
   FROM   ret_cza_lote
   WHERE  folio = gr_folio.folio_12

   IF SQLCA.SQLCODE <> 0 THEN
      LET gd_fecha_operacion = ""
   END IF

   #Verificar precios de accion
   SELECT UNIQUE fecha_genera
   INTO   gd_fecha_genera
   FROM   ret_parcial
   WHERE  folio       = gr_folio.folio_12

   #Obtener precios de accion
   DECLARE cur_precios CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = gd_fecha_genera
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

   DECLARE cur_prov CURSOR FOR
   SELECT a.nss            ,
          a.consecutivo    ,
          b.siefore        ,
          a.diag_cuenta_ind,
          a.tipo_desempleo ,
          a.tipo_pago      ,
          a.pago_desempleo
   FROM   ret_parcial       a,
          ret_monto_siefore b
   WHERE  a.estado_solicitud = 2
   AND    a.tipo_prestacion  = 6
   AND    a.folio            = gr_folio.folio_12
   AND    a.nss              = b.nss
   AND    a.consecutivo      = b.consecutivo
   AND    b.tipo_operacion   = 12
   ORDER  BY tipo_desempleo, siefore, nss

   START REPORT rpt_provision TO gc_archivo

   FOREACH cur_prov INTO gr_parcial.nss            ,
   	                     gr_parcial.consecutivo    ,
   	                     gr_parcial.siefore        ,
   	                     gr_parcial.diag_cuenta_ind,
   	                     gr_parcial.tipo_desempleo ,
   	                     gr_parcial.tipo_pago      ,
   	                     gr_parcial.pago_desempleo

      INITIALIZE lr_pago_subcta.* TO NULL
      #Obtener nombre
      EXECUTE get_nombre USING gr_parcial.nss INTO gr_parcial.nombre

      LET gr_parcial.acciones_est_esp = 0

      IF gr_parcial.diag_cuenta_ind <> 400 THEN
      	 LET gr_parcial.pago_desempleo      = 0
      	 LET gr_parcial.acciones_ret97      = 0
      	 LET gr_parcial.acciones_cv         = 0
      	 LET gr_parcial.acciones_cs         = 0
      	 LET gr_parcial.acciones_est_esp    = 0
      ELSE
      	 #Obtener monto por subcuenta
         FOREACH cur_pago_subcta  USING gr_parcial.nss           ,
         	                              gr_parcial.pago_desempleo,
         	                              gd_fecha_genera
         	                        INTO lr_pago_subcta.subcuenta    ,
                                       lr_pago_subcta.siefore      ,
                                       lr_pago_subcta.saldo_actual ,
                                       lr_pago_subcta.acciones_pago,
                                       lr_pago_subcta.pesos_pago
            CASE
            	 WHEN lr_pago_subcta.subcuenta = 1
            	 	    LET gr_parcial.acciones_ret97 = lr_pago_subcta.acciones_pago
            	 WHEN lr_pago_subcta.subcuenta = 2
            	 	    LET gr_parcial.acciones_cv = lr_pago_subcta.acciones_pago
            	 WHEN lr_pago_subcta.subcuenta = 5
            	 	    LET gr_parcial.acciones_cs = lr_pago_subcta.acciones_pago
            	 WHEN lr_pago_subcta.subcuenta = 6 OR
            	 	    lr_pago_subcta.subcuenta = 9
            	 	    LET gr_parcial.acciones_est_esp = gr_parcial.acciones_est_esp +
            	 	                                      lr_pago_subcta.acciones_pago
            END CASE
         END FOREACH
      END IF

      OUTPUT TO REPORT rpt_provision(gr_parcial.*)
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
          nss              CHAR(11)     ,
          consecutivo      DECIMAL(11,0),
          siefore          SMALLINT     ,
          diag_cuenta_ind  SMALLINT     ,
          tipo_desempleo   CHAR(01)     ,
          tipo_pago        SMALLINT     ,
          pago_desempleo   DECIMAL(22,6),
          nombre           CHAR(35)     ,
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6)
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

   DEFINE lar_total_por_desempleo ARRAY[5] OF RECORD
   	      nss_tot INTEGER ,
   	      siefore SMALLINT,
   	      tipo_desempleo   CHAR(01)     ,
          pago_desempleo   DECIMAL(22,6),
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6),
          total_acciones   DECIMAL(22,6)
        END RECORD

   DEFINE lar_gran_total ARRAY[5] OF RECORD
   	      nss_tot INTEGER ,
   	      siefore SMALLINT,
   	      pago_desempleo   DECIMAL(22,6),
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6),
          total_acciones   DECIMAL(22,6)
        END RECORD

   DEFINE lr_total_gral RECORD
   	      nss_tot INTEGER ,
   	      pago_desempleo   DECIMAL(22,6),
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6),
          total_acciones   DECIMAL(22,6)
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
       FOR li_cont = 1 TO 5
       	 LET lar_gran_total[li_cont].siefore = li_cont
   	     LET lar_gran_total[li_cont].nss_tot = 0
   	     LET lar_gran_total[li_cont].pago_desempleo   = 0
         LET lar_gran_total[li_cont].acciones_ret97   = 0
         LET lar_gran_total[li_cont].acciones_cv      = 0
         LET lar_gran_total[li_cont].acciones_cs      = 0
         LET lar_gran_total[li_cont].acciones_est_esp = 0
         LET lar_gran_total[li_cont].total_acciones   = 0
       END FOR

       LET lr_total_gral.nss_tot = 0
   	   LET lr_total_gral.pago_desempleo   = 0
       LET lr_total_gral.acciones_ret97   = 0
       LET lr_total_gral.acciones_cv      = 0
       LET lr_total_gral.acciones_cs      = 0
       LET lr_total_gral.acciones_est_esp = 0
       LET lr_total_gral.total_acciones   = 0

       LET li_page_length = 45 - 6 --5 titulos, 1 detalle
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
              '\033e\033(s18H'

       PRINT
            COLUMN 058,encabezado,
            '\033015'

       SKIP 1 LINES

       PRINT COLUMN 63,"RETIRO PARCIAL DESEMPLEO",
            '\033015'
       SKIP 2 LINES

       PRINT
            COLUMN 001,"FECHA DEL LOTE    : ",gd_fecha_operacion USING"DD/MM/YYYY",
            COLUMN 157,"PROG.    : RETL825",
            '\033015'
       --PRINT
       PRINT
            COLUMN 1,"FOLIO INTERNO     : ", gr_folio.folio_12 USING"##########",
            COLUMN 157,"PAGINA   :    ",PAGENO USING "####",
            '\033015'
       --PRINT
       PRINT
            COLUMN 1,"TIPO DE OPERACION : REPORTE DE PROVISION DE RETIRO PARCIAL POR DESEMPLEO     ",
            COLUMN 157,"FECHA : ", HOY USING "DD/MM/YYYY",
            '\033015'
       --PRINT
       PRINT
            COLUMN 1,"FECHA GENERA      : ",gd_fecha_genera USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",gar_precios[1].precio USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",gar_precios[2].precio USING "######&.&&&&&&" ,
            '\033015'
      PRINT COLUMN 034,"VALOR ACCION SB3 :",gar_precios[3].precio USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",gar_precios[4].precio USING "######&.&&&&&&" ,
            '\033015'
      PRINT COLUMN 034,"VALOR ACCION SB5 :",gar_precios[5].precio USING "######&.&&&&&&" ,
            '\033015'

      {PRINT COLUMN 001,"\332",L10,             --consecutivo
                       "\302",L10,L1,          --nss
                       "\302",L10,L10,L10,L5,  --nombre
                       "\302", L3,             --siefore
                       "\302", L4,             --diag
                       "\302", L4,             --tipo_retiro
                       "\302", L4,             --tipo_pago
                       "\302",L10,L5,          --pago desempleo
                       "\302",L10,L5,          --ret97
                       "\302",L10,L5,          --cv
                       "\302",L10,L5,          --cs
                       "\302",L10,L5,          --est_esp
                       "\302",L10,L5,          --total
                       "\277",
                       '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|", --consecutivo
            COLUMN 024,"|", --nss
            COLUMN 060,"|", --nombre
            COLUMN 064,"|", --siefore
            COLUMN 069,"|", --diag
            COLUMN 074,"|", --tipo_retiro
            COLUMN 079,"|", --tipo_pago
            COLUMN 095,"|", --pago desempleo
            COLUMN 111,"|", --ret97
            COLUMN 127,"|", --cv
            COLUMN 143,"|", --cs
            COLUMN 159,"|", --est_esp
            COLUMN 175,"|", --total
            '\033015'

      PRINT COLUMN 001,"|  CONSEC.",
            COLUMN 012,"|    NSS",
            COLUMN 024,"|       NOMBRE DEL TRABAJADOR",
            COLUMN 060,"|SIE",
            COLUMN 064,"|DIAG",
            COLUMN 069,"|TIPO",
            COLUMN 074,"|TIPO",
            COLUMN 079,"|PAGO  DESEMPLEO",
            COLUMN 095,"|   RETIRO 97",
            COLUMN 111,"| CES. Y VEJEZ",
            COLUMN 127,"| CUOTA  SOCIAL",
            COLUMN 143,"|ESTATAL/ESPECIA",
            COLUMN 159,"|     TOTAL",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|",
            COLUMN 024,"|",
            COLUMN 060,"|",
            COLUMN 064,"|",
            COLUMN 069,"|RET.",
            COLUMN 074,"|PAGO",
            COLUMN 079,"|    (PESOS)",
            COLUMN 095,"|(ACCIONES EST)",
            COLUMN 111,"|(ACCIONES EST)",
            COLUMN 127,"|(ACCIONES EST)",
            COLUMN 143,"|(ACCIONES EST)",
            COLUMN 159,"|   (ACCIONES)",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"\300",L10,             --consecutivo
                       "\301",L10,L1,          --nss
                       "\301",L10,L10,L10,L5,  --nombre
                       "\301", L3,             --siefore
                       "\301", L4,             --diag
                       "\301", L4,             --tipo_retiro
                       "\301", L4,             --tipo_pago
                       "\301",L10,L5,          --pago desempleo
                       "\301",L10,L5,          --ret97
                       "\301",L10,L5,          --cv
                       "\301",L10,L5,          --cs
                       "\301",L10,L5,          --est_esp
                       "\301",L10,L5,          --total
                       "\331",
                       '\033015'}

    PAGE HEADER
      PRINT COLUMN 1,"FOLIO INTERNO     : ", gr_folio.folio_12 USING"##########",
                     "          REPORTE DE PROVISION DE RETIRO PARCIAL POR DESEMPLEO",
            COLUMN 157,"PAGINA   :    ",PAGENO USING "####",
            '\033015'

      PRINT COLUMN 001,L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L5, L2, L1,
                       '\033015'

      {PRINT COLUMN 001,"\332",L10,             --consecutivo
                       "\302",L10,L1,          --nss
                       "\302",L10,L10,L10,L5,  --nombre
                       "\302", L3,             --siefore
                       "\302", L4,             --diag
                       "\302", L4,             --tipo_retiro
                       "\302", L4,             --tipo_pago
                       "\302",L10,L5,          --pago desempleo
                       "\302",L10,L5,          --ret97
                       "\302",L10,L5,          --cv
                       "\302",L10,L5,          --cs
                       "\302",L10,L5,          --est_esp
                       "\302",L10,L5,          --total
                       "\277",
                       '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|", --consecutivo
            COLUMN 024,"|", --nss
            COLUMN 060,"|", --nombre
            COLUMN 064,"|", --siefore
            COLUMN 069,"|", --diag
            COLUMN 074,"|", --tipo_retiro
            COLUMN 079,"|", --tipo_pago
            COLUMN 095,"|", --pago desempleo
            COLUMN 111,"|", --ret97
            COLUMN 127,"|", --cv
            COLUMN 143,"|", --cs
            COLUMN 159,"|", --est_esp
            COLUMN 175,"|", --total
            '\033015'

      PRINT COLUMN 001,"|  CONSEC.",
            COLUMN 012,"|    NSS",
            COLUMN 024,"|       NOMBRE DEL TRABAJADOR",
            COLUMN 060,"|SIE",
            COLUMN 064,"|DIAG",
            COLUMN 069,"|TIPO",
            COLUMN 074,"|TIPO",
            COLUMN 079,"|PAGO  DESEMPLEO",
            COLUMN 095,"|   RETIRO 97",
            COLUMN 111,"| CES. Y VEJEZ",
            COLUMN 127,"| CUOTA  SOCIAL",
            COLUMN 143,"|ESTATAL/ESPECIA",
            COLUMN 159,"|     TOTAL",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|",
            COLUMN 024,"|",
            COLUMN 060,"|",
            COLUMN 064,"|",
            COLUMN 069,"|RET.",
            COLUMN 074,"|PAGO",
            COLUMN 079,"|    (PESOS)",
            COLUMN 095,"|(ACCIONES EST)",
            COLUMN 111,"|(ACCIONES EST)",
            COLUMN 127,"|(ACCIONES EST)",
            COLUMN 143,"|(ACCIONES EST)",
            COLUMN 159,"|   (ACCIONES)",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"\300",L10,             --consecutivo
                       "\301",L10,L1,          --nss
                       "\301",L10,L10,L10,L5,  --nombre
                       "\301", L3,             --siefore
                       "\301", L4,             --diag
                       "\301", L4,             --tipo_retiro
                       "\301", L4,             --tipo_pago
                       "\301",L10,L5,          --pago desempleo
                       "\301",L10,L5,          --ret97
                       "\301",L10,L5,          --cv
                       "\301",L10,L5,          --cs
                       "\301",L10,L5,          --est_esp
                       "\301",L10,L5,          --total
                       "\331",
                       '\033015'}

   BEFORE GROUP OF lr_rpt.tipo_desempleo
   	  FOR li_cont = 1 TO 5
   	     LET lar_total_por_desempleo[li_cont].nss_tot = 0
   	     LET lar_total_por_desempleo[li_cont].siefore = li_cont
   	     LET lar_total_por_desempleo[li_cont].tipo_desempleo   = ""
   	     LET lar_total_por_desempleo[li_cont].pago_desempleo   = 0
         LET lar_total_por_desempleo[li_cont].acciones_ret97   = 0
         LET lar_total_por_desempleo[li_cont].acciones_cv      = 0
         LET lar_total_por_desempleo[li_cont].acciones_cs      = 0
         LET lar_total_por_desempleo[li_cont].acciones_est_esp = 0
         LET lar_total_por_desempleo[li_cont].total_acciones   = 0
      END FOR

      IF LINENO > li_page_length THEN
      	 SKIP TO TOP OF PAGE
      END IF

      PRINT COLUMN 001,"\332",L10,             --consecutivo
                       "\302",L10,L1,          --nss
                       "\302",L10,L10,L10,L5,  --nombre
                       "\302", L3,             --siefore
                       "\302", L4,             --diag
                       "\302", L4,             --tipo_retiro
                       "\302", L4,             --tipo_pago
                       "\302",L10,L5,          --pago desempleo
                       "\302",L10,L5,          --ret97
                       "\302",L10,L5,          --cv
                       "\302",L10,L5,          --cs
                       "\302",L10,L5,          --est_esp
                       "\302",L10,L5,          --total
                       "\277",
                       '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|", --consecutivo
            COLUMN 024,"|", --nss
            COLUMN 060,"|", --nombre
            COLUMN 064,"|", --siefore
            COLUMN 069,"|", --diag
            COLUMN 074,"|", --tipo_retiro
            COLUMN 079,"|", --tipo_pago
            COLUMN 095,"|", --pago desempleo
            COLUMN 111,"|", --ret97
            COLUMN 127,"|", --cv
            COLUMN 143,"|", --cs
            COLUMN 159,"|", --est_esp
            COLUMN 175,"|", --total
            '\033015'

      PRINT COLUMN 001,"|  CONSEC.",
            COLUMN 012,"|    NSS",
            COLUMN 024,"|       NOMBRE DEL TRABAJADOR",
            COLUMN 060,"|SIE",
            COLUMN 064,"|DIAG",
            COLUMN 069,"|TIPO",
            COLUMN 074,"|TIPO",
            COLUMN 079,"|PAGO  DESEMPLEO",
            COLUMN 095,"|   RETIRO 97",
            COLUMN 111,"| CES. Y VEJEZ",
            COLUMN 127,"| CUOTA  SOCIAL",
            COLUMN 143,"|ESTATAL/ESPECIA",
            COLUMN 159,"|     TOTAL",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"|",
            COLUMN 012,"|",
            COLUMN 024,"|",
            COLUMN 060,"|",
            COLUMN 064,"|",
            COLUMN 069,"|RET.",
            COLUMN 074,"|PAGO",
            COLUMN 079,"|    (PESOS)",
            COLUMN 095,"|(ACCIONES EST)",
            COLUMN 111,"|(ACCIONES EST)",
            COLUMN 127,"|(ACCIONES EST)",
            COLUMN 143,"|(ACCIONES EST)",
            COLUMN 159,"|   (ACCIONES)",
            COLUMN 175,"|",
            '\033015'

      PRINT COLUMN 001,"\300",L10,             --consecutivo
                       "\301",L10,L1,          --nss
                       "\301",L10,L10,L10,L5,  --nombre
                       "\301", L3,             --siefore
                       "\301", L4,             --diag
                       "\301", L4,             --tipo_retiro
                       "\301", L4,             --tipo_pago
                       "\301",L10,L5,          --pago desempleo
                       "\301",L10,L5,          --ret97
                       "\301",L10,L5,          --cv
                       "\301",L10,L5,          --cs
                       "\301",L10,L5,          --est_esp
                       "\301",L10,L5,          --total
                       "\331",
                       '\033015'
   ON EVERY ROW
      PRINT COLUMN 002, lr_rpt.consecutivo         USING "##########",
            COLUMN 013, lr_rpt.nss   ,
            COLUMN 025, lr_rpt.nombre,
            COLUMN 061, lr_rpt.siefore              USING "#&",
            COLUMN 065, lr_rpt.diag_cuenta_ind      USING "&&&",
            COLUMN 071, lr_rpt.tipo_desempleo       ,
            COLUMN 076, lr_rpt.tipo_pago            USING "&",
            COLUMN 080, lr_rpt.pago_desempleo       USING "###########&.&&",
            COLUMN 096, lr_rpt.acciones_ret97       USING "#######&.&&&&&&",
            COLUMN 112, lr_rpt.acciones_cv          USING "#######&.&&&&&&",
            COLUMN 128, lr_rpt.acciones_cs          USING "#######&.&&&&&&",
            COLUMN 144, lr_rpt.acciones_est_esp     USING "#######&.&&&&&&",
            COLUMN 160, lr_rpt.acciones_ret97  +
                        lr_rpt.acciones_cv     +
                        lr_rpt.acciones_cs     +
                        lr_rpt.acciones_est_esp     USING "#######&.&&&&&&",
                        '\033015'

      LET li_siefore = lr_rpt.siefore

      LET lar_total_por_desempleo[li_siefore].nss_tot          = lar_total_por_desempleo[li_siefore].nss_tot + 1
   	  LET lar_total_por_desempleo[li_siefore].pago_desempleo   = lar_total_por_desempleo[li_siefore].pago_desempleo   + lr_rpt.pago_desempleo
   	  LET lar_total_por_desempleo[li_siefore].acciones_ret97   = lar_total_por_desempleo[li_siefore].acciones_ret97   + lr_rpt.acciones_ret97
      LET lar_total_por_desempleo[li_siefore].acciones_cv      = lar_total_por_desempleo[li_siefore].acciones_cv      + lr_rpt.acciones_cv
      LET lar_total_por_desempleo[li_siefore].acciones_cs      = lar_total_por_desempleo[li_siefore].acciones_cs      + lr_rpt.acciones_cs
      LET lar_total_por_desempleo[li_siefore].acciones_est_esp = lar_total_por_desempleo[li_siefore].acciones_est_esp + lr_rpt.acciones_est_esp
      LET lar_total_por_desempleo[li_siefore].total_acciones   = lar_total_por_desempleo[li_siefore].total_acciones   + lr_rpt.acciones_ret97  +
                                                                                                                        lr_rpt.acciones_cv     +
                                                                                                                        lr_rpt.acciones_cs     +
                                                                                                                        lr_rpt.acciones_est_esp

      AFTER GROUP OF lr_rpt.tipo_desempleo
      	 SKIP 1 LINE
         FOR li_cont = 1 TO 5
         	 IF lar_total_por_desempleo[li_cont].nss_tot > 0 THEN
         	 	  IF LINENO > li_page_length_totales THEN
      	         SKIP TO TOP OF PAGE
              END IF

         	 	  PRINT COLUMN 001, "\332",L10,L10,L2,     --nss
                                "\302",L10,L10,L10,L5, --totales
                                "\302",L3,             --siefore
                                "\302",L5,L2,L2,       --tipo_retiro
                                "\302",L10,L10,        --pago desempleo
                                "\302",L10,L5,         --ret97
                                "\302",L10,L5,         --cv
                                "\302",L10,L5,         --cs
                                "\302",L10,L5,         --est_esp
                                "\302",L10,L5,         --total
                                "\277",
                                '\033015'

               PRINT COLUMN 001, "|", lar_total_por_desempleo[li_cont].nss_tot USING "#####################&",
                     COLUMN 024, "| TOTALES POR TIPO RETIRO Y SIEFORE",
                     COLUMN 060, "|SB"   ,lar_total_por_desempleo[li_cont].siefore     USING "&",
                     COLUMN 064, "|    " ,lr_rpt.tipo_desempleo                        ,
                     COLUMN 074, "|",lar_total_por_desempleo[li_cont].pago_desempleo   USING "################&.&&",
                     COLUMN 095, "|",lar_total_por_desempleo[li_cont].acciones_ret97   USING "#######&.&&&&&&",
                     COLUMN 111, "|",lar_total_por_desempleo[li_cont].acciones_cv      USING "#######&.&&&&&&",
                     COLUMN 127, "|",lar_total_por_desempleo[li_cont].acciones_cs      USING "#######&.&&&&&&",
                     COLUMN 143, "|",lar_total_por_desempleo[li_cont].acciones_est_esp USING "#######&.&&&&&&",
                     COLUMN 159, "|",lar_total_por_desempleo[li_cont].total_acciones   USING "#######&.&&&&&&","|",
                     '\033015'

               PRINT COLUMN 001,"\300",L10,L10,L2,     --nss
                                "\301",L10,L10,L10,L5, --totales
                                "\301",L3,             --siefore
                                "\301",L5,L2,L2,       --tipo_retiro
                                "\301",L10,L10,        --pago desempleo
                                "\301",L10,L5,         --ret97
                                "\301",L10,L5,         --cv
                                "\301",L10,L5,         --cs
                                "\301",L10,L5,         --est_esp
                                "\301",L10,L5,         --total
                                "\331",
                                '\033015'


              SKIP 1 LINE
              #Acumular gran total
              LET lar_gran_total[li_cont].nss_tot          = lar_gran_total[li_cont].nss_tot          + lar_total_por_desempleo[li_cont].nss_tot
   	          LET lar_gran_total[li_cont].pago_desempleo   = lar_gran_total[li_cont].pago_desempleo   + lar_total_por_desempleo[li_cont].pago_desempleo
              LET lar_gran_total[li_cont].acciones_ret97   = lar_gran_total[li_cont].acciones_ret97   + lar_total_por_desempleo[li_cont].acciones_ret97
              LET lar_gran_total[li_cont].acciones_cv      = lar_gran_total[li_cont].acciones_cv      + lar_total_por_desempleo[li_cont].acciones_cv
              LET lar_gran_total[li_cont].acciones_cs      = lar_gran_total[li_cont].acciones_cs      + lar_total_por_desempleo[li_cont].acciones_cs
              LET lar_gran_total[li_cont].acciones_est_esp = lar_gran_total[li_cont].acciones_est_esp + lar_total_por_desempleo[li_cont].acciones_est_esp
              LET lar_gran_total[li_cont].total_acciones   = lar_gran_total[li_cont].total_acciones   + lar_total_por_desempleo[li_cont].total_acciones
         	 END IF
         END FOR

    SKIP 2 LINES

    ON LAST ROW
       #Imprimir totales por siefore
       FOR li_cont = 1 TO 5
         	IF lar_gran_total[li_cont].nss_tot > 0 THEN
         		 IF LINENO > li_page_length_totales THEN
      	         SKIP TO TOP OF PAGE
             END IF

             PRINT COLUMN 001,"\332",L10,L10,L2,     --nss
                              "\302",L10,L10,L10,L5, --totales
                              "\302",L3,             --siefore
                              "\302",L10,L10,L10,    --pago desempleo
                              "\302",L10,L5,         --ret97
                              "\302",L10,L5,         --cv
                              "\302",L10,L5,         --cs
                              "\302",L10,L5,         --est_esp
                              "\302",L10,L5,         --total
                              "\277",
                              '\033015'

             PRINT COLUMN 001, "|", lar_gran_total[li_cont].nss_tot USING "#####################&",
                   COLUMN 024, "|       TOTALES POR SIEFORE",
                   COLUMN 060, "|SB"   ,lar_gran_total[li_cont].siefore     USING "&",
                   COLUMN 064, "|" ,
                   COLUMN 075,     lar_gran_total[li_cont].pago_desempleo   USING "################&.&&",
                   COLUMN 095, "|",lar_gran_total[li_cont].acciones_ret97   USING "#######&.&&&&&&",
                   COLUMN 111, "|",lar_gran_total[li_cont].acciones_cv      USING "#######&.&&&&&&",
                   COLUMN 127, "|",lar_gran_total[li_cont].acciones_cs      USING "#######&.&&&&&&",
                   COLUMN 143, "|",lar_gran_total[li_cont].acciones_est_esp USING "#######&.&&&&&&",
                   COLUMN 159, "|",lar_gran_total[li_cont].total_acciones   USING "#######&.&&&&&&","|",
                   '\033015'

             PRINT COLUMN 001,"\300",L10,L10,L2,     --nss
                              "\301",L10,L10,L10,L5, --totales
                              "\301",L3,             --siefore
                              "\301",L10,L10,L10,    --pago desempleo
                              "\301",L10,L5,         --ret97
                              "\301",L10,L5,         --cv
                              "\301",L10,L5,         --cs
                              "\301",L10,L5,         --est_esp
                              "\301",L10,L5,         --total
                              "\331",
                              '\033015'

             SKIP 1 LINES
             #Acumular total general
             LET lr_total_gral.nss_tot          = lr_total_gral.nss_tot          + lar_gran_total[li_cont].nss_tot
   	         LET lr_total_gral.pago_desempleo   = lr_total_gral.pago_desempleo   + lar_gran_total[li_cont].pago_desempleo
             LET lr_total_gral.acciones_ret97   = lr_total_gral.acciones_ret97   + lar_gran_total[li_cont].acciones_ret97
             LET lr_total_gral.acciones_cv      = lr_total_gral.acciones_cv      + lar_gran_total[li_cont].acciones_cv
             LET lr_total_gral.acciones_cs      = lr_total_gral.acciones_cs      + lar_gran_total[li_cont].acciones_cs
             LET lr_total_gral.acciones_est_esp = lr_total_gral.acciones_est_esp + lar_gran_total[li_cont].acciones_est_esp
             LET lr_total_gral.total_acciones   = lr_total_gral.total_acciones   + lar_gran_total[li_cont].total_acciones
          END IF
       END FOR

       SKIP 2 LINES

       IF LINENO > li_page_length_totales THEN
      	  SKIP TO TOP OF PAGE
       END IF

       #Imprimir total final
       PRINT COLUMN 001,"\332",L10,L10,L10,L10,L10,L10,L10,L2, --nss
                        "\302",L10,L10,                        --pago desempleo
                        "\277",
                        '\033015'

       PRINT COLUMN 001, "|             TOTAL DE NSS UNICOS:", lr_total_gral.nss_tot USING "###################&",
             COLUMN 074, "|",lr_total_gral.pago_desempleo   USING "################&.&&","|",
             '\033015'

       PRINT COLUMN 001,"\300",L10,L10,L10,L10,L10,L10,L10,L2, --nss
                        "\301",L10,L10,                        --pago desempleo
                        "\331",
                        '\033015'
END REPORT
################################################################################
