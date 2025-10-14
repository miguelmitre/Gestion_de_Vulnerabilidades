#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENL101  => REPORTE DE LIQUIDACION DE PMG (ACCIONES)                  #
#Fecha creacion    => 16 DE FEBRERO DE 2011                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#Sistema           => PEN                                                       #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_edo RECORD
        en_proceso_pago     LIKE pen_estado_pmg.estado_solicitud ,
        liquidado           LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gar_precios ARRAY[5] OF RECORD
        siefore             SMALLINT                            ,
        precio              LIKE glo_valor_accion.precio_del_dia
    END RECORD

    DEFINE gr_afore RECORD
        codigo_afore        SMALLINT,
        razon_social        CHAR(50)
    END RECORD

    DEFINE gr_captura RECORD
        folio               INTEGER ,
        fecha_liquida       DATE
    END RECORD

    DEFINE gr_disp RECORD
        nss                     CHAR(11)        ,
        consecutivo             DECIMAL(11,0)   ,
        tipo_retiro             CHAR(1)         ,
        tipo_seguro             CHAR(2)         ,
        tipo_pension            CHAR(2)         ,
        tipo_prestacion         SMALLINT        ,
        siefore                 SMALLINT        ,
        mensualidad             SMALLINT        ,
        regimen                 CHAR(2)         ,
        nombre                  CHAR(30)        ,
        acciones_tot_liquidado  DECIMAL(22,6)   ,
        acciones_viv            DECIMAL(22,6)   ,
        acciones_ret97          DECIMAL(22,6)   ,
        acciones_cv             DECIMAL(22,6)   ,
        acciones_est            DECIMAL(22,6)   ,
        acciones_esp            DECIMAL(22,6)   ,
        acciones_cs             DECIMAL(22,6)   ,
        acciones_rcv            DECIMAL(22,6)
    END RECORD

    DEFINE
        gr_seg_modulo           RECORD LIKE seg_modulo.*

    DEFINE
        enter               CHAR(001)   ,
        gc_archivo          CHAR(200)   ,
        gc_usuario          CHAR(008)

    DEFINE
        HOY                 ,
        gd_fecha_genera     ,
        gd_fecha_operacion  DATE

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PENL101.log")
    CALL init()

    CALL f_captura_datos() RETURNING gr_captura.*
    CALL f_genera_reporte(gr_captura.*)

    DISPLAY "ARCHIVO GENERADO EN LA RUTA :" AT 12,17
    DISPLAY gc_archivo CLIPPED AT 13,17
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " ATTRIBUTE(REVERSE) FOR CHAR enter

    CLOSE WINDOW penl1011

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(800)

    -- -----------------------------------------------------------------------------

    ----- VARIABLES GLOBALES -----
    LET HOY = TODAY

    SELECT codigo_afore ,
           razon_social ,
           USER
    INTO   gr_afore.codigo_afore    ,
           gr_afore.razon_social    ,
           gc_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pmg"

    LET gc_archivo = gr_seg_modulo.ruta_listados CLIPPED    ,
                     "/", gc_usuario CLIPPED                ,
                     ".RPT_LIQ_"                            ,
                     HOY USING"DDMMYYYY"                    ,
                     ".101"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.en_proceso_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- OBTIENE NOMBRE -----
    LET lc_prepare = " SELECT NVL(TRIM(nombres),' ')||' '||",
                     " NVL(TRIM(paterno),' ')||' '||",
                     " NVL(TRIM(materno),' ') ",
                     " FROM   afi_mae_afiliado ",
                     " WHERE  n_seguro = ? "

    PREPARE get_nombre FROM lc_prepare
    LET lc_prepare = " "

    ----- OBTIENE MONTOS POR SUBCUENTA-----
    LET lc_prepare  = " SELECT subcuenta             , ",
                      "        siefore               , ",
                      "        SUM(monto_en_acciones), ",
                      "        SUM(monto_en_pesos)     ",
                      " FROM   dis_cuenta              ",
                      " WHERE  nss              =  ?   ",
                      " AND    consecutivo_lote =  ?   ",
                      " AND    folio            =  ?   ",
                      " GROUP BY 1,2                   ",
                      " ORDER BY 1,2                   "

    PREPARE eje_montos FROM lc_prepare
    DECLARE cur_montos CURSOR FOR eje_montos
    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura y despliega los datos de consulta necesarios    #
#                   para el proceso                                         #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD
        folio               INTEGER ,
        fecha_liquida       DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    OPEN WINDOW penl1011 AT 4,4 WITH FORM "PENL1011" ATTRIBUTE (BORDER)
    DISPLAY " <ESC> - Ejecutar                                     <Ctrl-C> - Cancelar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENL101    REPORTE DE LIQUIDACION DE RECURSOS PMG - ACCIONES                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INITIALIZE lr_captura.* TO NULL

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        --------------------------------
        AFTER FIELD folio
        --------------------------------
            IF lr_captura.folio IS NOT NULL THEN
                SELECT fecha_conversion
                INTO   lr_captura.fecha_liquida
                FROM   dis_cuenta
                WHERE  folio            = lr_captura.folio
                AND    tipo_movimiento  = 841
                GROUP BY 1

                IF SQLCA.SQLCODE <> NOTFOUND THEN
                    DISPLAY BY NAME lr_captura.fecha_liquida
                ELSE
                    ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD folio
                END IF
            END IF

        --------------------------------
        AFTER FIELD fecha_liquida
        --------------------------------
            IF lr_captura.fecha_liquida IS NULL THEN
                IF lr_captura.folio IS NULL THEN
                    ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD folio
                END IF
            ELSE
                SELECT folio
                INTO   lr_captura.folio
                FROM   dis_cuenta
                WHERE  fecha_conversion = lr_captura.fecha_liquida
                AND    tipo_movimiento  = 841
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    DISPLAY BY NAME lr_captura.folio
                ELSE
                    ERROR "NO EXISTE INFORMACION PARA ESTA FECHA"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD folio
                END IF
            END IF

        --------------------------------
        ON KEY (ESC)
        --------------------------------
            IF lr_captura.folio IS NULL AND lr_captura.fecha_liquida IS NULL THEN
                ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

            SELECT fecha_conversion
            INTO   lr_captura.fecha_liquida
            FROM   dis_cuenta
            WHERE  folio            = lr_captura.folio
            AND    tipo_movimiento  = 841
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

            EXIT INPUT

        --------------------------------
        ON KEY (INTERRUPT, CONTROL-C)
        --------------------------------
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    RETURN lr_captura.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_reporte : Obtiene los datos que seran mostrados en el reporte    #
#---------------------------------------------------------------------------#
FUNCTION f_genera_reporte(pr_captura)

    DEFINE pr_captura RECORD
        folio               INTEGER ,
        fecha_liquida       DATE
    END RECORD

    DEFINE lr_montos RECORD
        subcuenta     SMALLINT       ,
        siefore       SMALLINT       ,
        acciones      DECIMAL(22,6)  ,
        pesos         DECIMAL(22,6)
    END RECORD

    DEFINE ld_precio LIKE glo_valor_accion.precio_del_dia

    DEFINE
        li_siefore              ,
        li_cont_siefore         SMALLINT


    DEFINE
        lc_mensaje              CHAR(100)

    -- -----------------------------------------------------------------------------

    DECLARE cur_precios CURSOR FOR
    SELECT codigo_siefore,
           precio_del_dia
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = pr_captura.fecha_liquida
    AND    codigo_siefore IN (1,2,3,4,5)
    ORDER BY 1

    LET li_cont_siefore = 0

    FOREACH cur_precios INTO li_siefore ,
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
    SELECT UNIQUE b.nss     ,
           a.consecutivo    ,
           a.tipo_retiro    ,
           a.tipo_seguro    ,
           a.tipo_pension   ,
           a.tipo_prestacion,
           b.siefore        ,
           b.mensualidad    ,
           a.regimen
    FROM   pen_solicitud_pmg   a ,
           pen_preliquida_pmg  b
    WHERE  b.folio              = pr_captura.folio
    AND    a.nss                = b.nss
    AND    a.consecutivo        = b.consecutivo_lote
    AND    a.estado_solicitud   IN (gr_edo.en_proceso_pago, -- 50
                                    gr_edo.liquidado      ) -- 70
    AND    b.estado_pmg         = gr_edo.liquidado
    AND    b.siefore           <> 11
    ORDER  BY tipo_retiro    ,
              siefore        ,
              tipo_seguro    ,
              tipo_pension   ,
              tipo_prestacion,
              regimen

    START REPORT rpt_liquida TO gc_archivo

    FOREACH cur_liq INTO gr_disp.nss            ,
                         gr_disp.consecutivo    ,
                         gr_disp.tipo_retiro    ,
                         gr_disp.tipo_seguro    ,
                         gr_disp.tipo_pension   ,
                         gr_disp.tipo_prestacion,
                         gr_disp.siefore        ,
                         gr_disp.mensualidad    ,
                         gr_disp.regimen

        INITIALIZE lr_montos.* TO NULL

        EXECUTE get_nombre USING gr_disp.nss INTO gr_disp.nombre

        LET gr_disp.acciones_tot_liquidado = 0
        LET gr_disp.acciones_viv           = 0
        LET gr_disp.acciones_ret97         = 0
        LET gr_disp.acciones_cv            = 0
        LET gr_disp.acciones_est           = 0
        LET gr_disp.acciones_esp           = 0
        LET gr_disp.acciones_cs            = 0
        LET gr_disp.acciones_rcv           = 0

        FOREACH cur_montos USING gr_disp.nss        ,
                                 gr_disp.consecutivo,
                                 pr_captura.folio
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
                WHEN lr_montos.subcuenta = 1
                    LET gr_disp.acciones_ret97 = gr_disp.acciones_ret97 + lr_montos.acciones

                WHEN lr_montos.subcuenta = 2
                    LET gr_disp.acciones_cv   = gr_disp.acciones_cv + lr_montos.acciones

                WHEN lr_montos.subcuenta = 4
                    LET gr_disp.acciones_viv = gr_disp.acciones_viv + lr_montos.acciones

                WHEN lr_montos.subcuenta = 5
                    LET gr_disp.acciones_cs = gr_disp.acciones_cs + lr_montos.acciones

                WHEN lr_montos.subcuenta = 6
                    LET gr_disp.acciones_est = gr_disp.acciones_est + lr_montos.acciones

                WHEN lr_montos.subcuenta = 9
                    LET gr_disp.acciones_esp = gr_disp.acciones_esp + lr_montos.acciones
            END CASE

        END FOREACH

        -- Total Liquidado
        LET gr_disp.acciones_tot_liquidado = gr_disp.acciones_ret97   +
                                             gr_disp.acciones_cv      +
                                             gr_disp.acciones_est     +
                                             gr_disp.acciones_esp     +
                                             gr_disp.acciones_cs

        LET gr_disp.acciones_rcv           = gr_disp.acciones_cv  +
                                             gr_disp.acciones_est +
                                             gr_disp.acciones_esp


        OUTPUT TO REPORT rpt_liquida(gr_disp.*)

   END FOREACH

   FINISH REPORT rpt_liquida

   LET lc_mensaje = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_mensaje

   LET lc_mensaje = "lp ", gc_archivo CLIPPED
   RUN lc_mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_liquida : Genera el reporte de liquidacion de PMG                     #
#---------------------------------------------------------------------------#
REPORT rpt_liquida(pr_rpt)

    DEFINE pr_rpt RECORD
        nss                     CHAR(11)        ,
        consecutivo             DECIMAL(11,0)   ,
        tipo_retiro             CHAR(1)         ,
        tipo_seguro             CHAR(2)         ,
        tipo_pension            CHAR(2)         ,
        tipo_prestacion         SMALLINT        ,
        siefore                 SMALLINT        ,
        mensualidad             SMALLINT        ,
        regimen                 CHAR(2)         ,
        nombre                  CHAR(30)        ,
        acciones_tot_liquidado  DECIMAL(22,6)   ,
        acciones_viv            DECIMAL(22,6)   ,
        acciones_ret97          DECIMAL(22,6)   ,
        acciones_cv             DECIMAL(22,6)   ,
        acciones_est            DECIMAL(22,6)   ,
        acciones_esp            DECIMAL(22,6)   ,
        acciones_cs             DECIMAL(22,6)   ,
        acciones_rcv            DECIMAL(22,6)
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
        lar_total_siefore ARRAY[5] OF RECORD
            siefore                 SMALLINT        ,
            nss_tot                 INTEGER         ,
            acciones_tot_liquidado  DECIMAL(22,6)   ,
            acciones_viv            DECIMAL(22,6)   ,
            acciones_ret97          DECIMAL(22,6)   ,
            acciones_cv             DECIMAL(22,6)   ,
            acciones_est            DECIMAL(22,6)   ,
            acciones_esp            DECIMAL(22,6)   ,
            acciones_cs             DECIMAL(22,6)   ,
            acciones_rcv            DECIMAL(22,6)
        END RECORD
    END RECORD

    DEFINE lar_gran_total_siefore ARRAY[5] OF RECORD
        nss_tot                     INTEGER         ,
        siefore                     SMALLINT        ,
        acciones_tot_liquidado      DECIMAL(22,6)   ,
        acciones_viv                DECIMAL(22,6)   ,
        acciones_ret97              DECIMAL(22,6)   ,
        acciones_cv                 DECIMAL(22,6)   ,
        acciones_est                DECIMAL(22,6)   ,
        acciones_esp                DECIMAL(22,6)   ,
        acciones_cs                 DECIMAL(22,6)   ,
        acciones_rcv                DECIMAL(22,6)
    END RECORD

    DEFINE lar_gran_total_retiro ARRAY[5] OF RECORD
        tipo_retiro             CHAR(01)        ,
        nss_tot                 INTEGER         ,
        acciones_fov08          DECIMAL(22,6)   ,
        acciones_fov92          DECIMAL(22,6)
    END RECORD

    DEFINE lr_total_gral RECORD
        nss_tot                     INTEGER         ,
        acciones_tot_liquidado      DECIMAL(22,6)   ,
        acciones_viv                DECIMAL(22,6)   ,
        acciones_ret97              DECIMAL(22,6)   ,
        acciones_cv                 DECIMAL(22,6)   ,
        acciones_est                DECIMAL(22,6)   ,
        acciones_esp                DECIMAL(22,6)   ,
        acciones_cs                 DECIMAL(22,6)   ,
        acciones_rcv                DECIMAL(22,6)
    END RECORD

    DEFINE
        li_cont                     ,
        li_siefore                  ,
        li_page_length              ,
        li_page_length_totales      SMALLINT

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER

        -- Inicializar acumulador por tipo de retiro y siefore
        -- Actualmente solo reportamos el tipo S, por lo que li_cont no vale
        -- mas de 1
        FOR li_cont = 1 TO 1

            -- Inicializar acumulador por tipo de retiro
            CASE li_cont
                WHEN 1
                    LET lar_total_retiro[li_cont].tipo_retiro = "S"
            END CASE

            -- Inicializar acumulador por siefore
            FOR li_siefore = 1 TO 5
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].siefore             = li_siefore
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot             = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv           = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97         = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv            = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est           = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp           = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs            = 0
                LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv           = 0
            END FOR -- Por siefore

            LET lar_gran_total_retiro[li_cont].tipo_retiro          = ""
            LET lar_gran_total_retiro[li_cont].nss_tot              = 0
            LET lar_gran_total_retiro[li_cont].acciones_fov08       = 0
            LET lar_gran_total_retiro[li_cont].acciones_fov92       = 0

            LET lar_gran_total_siefore[li_cont].nss_tot             = 0
            LET lar_gran_total_siefore[li_cont].siefore             = li_cont
            LET lar_gran_total_siefore[li_cont].acciones_tot_liquidado = 0
            LET lar_gran_total_siefore[li_cont].acciones_viv           = 0
            LET lar_gran_total_siefore[li_cont].acciones_ret97         = 0
            LET lar_gran_total_siefore[li_cont].acciones_cv            = 0
            LET lar_gran_total_siefore[li_cont].acciones_est           = 0
            LET lar_gran_total_siefore[li_cont].acciones_esp           = 0
            LET lar_gran_total_siefore[li_cont].acciones_cs            = 0
            LET lar_gran_total_siefore[li_cont].acciones_rcv           = 0

        END FOR -- Por tipo de retiro

        LET lr_total_gral.nss_tot               = 0
        LET lr_total_gral.acciones_tot_liquidado   = 0
        LET lr_total_gral.acciones_viv             = 0

        LET lr_total_gral.acciones_ret97           = 0
        LET lr_total_gral.acciones_cv              = 0
        LET lr_total_gral.acciones_est             = 0
        LET lr_total_gral.acciones_esp             = 0
        LET lr_total_gral.acciones_cs              = 0
        LET lr_total_gral.acciones_rcv             = 0

        LET li_page_length          = 45 - 7 --5 titulos, 1 detalle
        LET li_page_length_totales  = 45 - 3 --5 titulos, 1 detalle

        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9  = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

        LET encabezado = "M O D U L O   D E   R E T I R O S"

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
              '\033e\033(s23H'

        PRINT
            COLUMN 058,encabezado,
            '\033015'

        SKIP 1 LINES

        PRINT COLUMN 49,"REPORTE DE LIQUIDACION DE PENSION MINIMA GARANTIZADA",
            '\033015'

        SKIP 2 LINES

        PRINT COLUMN 001, "AFORE               : ", gr_afore.razon_social CLIPPED,
              COLUMN 165, "PAGINA              : " ,PAGENO USING "####",
              '\033015'

        PRINT COLUMN 001, "PROGRAMA            : PENL101",
              COLUMN 165, "FECHA LIQUIDACION   : ",gr_captura.fecha_liquida USING"DD/MM/YYYY",
              '\033015'

        PRINT COLUMN 001, "FOLIO INTERNO       : ",gr_captura.folio USING "#########&",
              COLUMN 165, "FECHA GENERACION    : ",HOY USING "DD/MM/YYYY",
              '\033015'

        PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
              COLUMN 042, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
              COLUMN 082, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
              COLUMN 122, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
              COLUMN 167, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
              '\033015'

    PAGE HEADER
        PRINT COLUMN 001, "FOLIO INTERNO       :", gr_captura.folio USING "#########&",
              COLUMN 049, "REPORTE DE LIQUIDACION DE PENSION MINIMA GARANTIZADA",
              COLUMN 181, "PAGINA              :" ,PAGENO USING "####",
              '\033015'

        PRINT COLUMN 001,L10,L10,L10,L10,L10,
                         L10,L10,L10,L10,L10,
                         L10,L10,L10,L10,L10,
                         L10,L10,L10,L10,L10,
                         L10,L10,L10,L5, L2,L5,
                         '\033015'

    BEFORE GROUP OF pr_rpt.tipo_retiro

        -- De ser necesario, aqui se deben agragar mas tipos de retiro
        CASE pr_rpt.tipo_retiro
             WHEN "S" LET li_cont = 1
        END CASE

        IF LINENO > li_page_length THEN
            SKIP TO TOP OF PAGE
        END IF

        -- Bloque titulos 1
        PRINT COLUMN 001,"\332",L10,L1,              -- nss
                         "\302",L10,L10,L10,         -- nombre
                         "\302", L2,L1,              -- tipo_seguro
                         "\302", L2,L1,              -- tipo_pension
                         "\302", L2,                 -- regimen
                         "\302", L10,L10,L10,L10,L10,
                                 L10,L10,L10,L10,L10,
                                 L10,L10,L10,L10,L1,   -- LINEA
                         "\277",
                         '\033015'

        -- Bloque titulos 2,   cierra
        PRINT COLUMN 001, "|",
              COLUMN 013, "|",      -- nss
              COLUMN 044, "|TP.",   -- nombre
              COLUMN 048, "|TP.",   -- tipo_seguro
              COLUMN 052, "|",      -- tipo_pension
              COLUMN 055, "|",      -- regimen
              COLUMN 058, "|",      -- siefore
              COLUMN 063, "|",      -- mensualidad
              COLUMN 104, "MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (ACCIONES) DEL TIPO RETIRO ", pr_rpt.tipo_retiro,
              COLUMN 197, "|",
                          '\033015'

        -- Bloque titulos 3,   cierra
        PRINT COLUMN 001,"|",
              COLUMN 013,"|",               -- nss
              COLUMN 044,"|",               -- nombre
              COLUMN 048,"|",               -- tipo_seguro
              COLUMN 052,"|",               -- tipo_pension
              COLUMN 055,"|",               -- regimen
              COLUMN 058,"|",               -- siefore

              COLUMN 063,"\302", L10,L2,L2, -- acciones_tot_liquidado
                         "\302", L10,L5,L1, -- acciones_ret97
                         "\302", L10,L5,L1, -- acciones_cv
                         "\301", L10,L5,L1, -- acciones_cs
                         "\301", L10,L5,L1, -- acciones_est
                         "\302", L10,L5,L1, -- acciones_esp
                         "\302", L10,L5,L1, -- acciones_viv
                         "\302", L10,L5,L1, -- acciones_rcv
                         "\277",
                         '\033015'

        -- Bloque titulos 4
        PRINT COLUMN 001,"|    NSS",
              COLUMN 013,"|     NOMBRE DEL TRABAJADOR",
              COLUMN 044,"|D E",
              COLUMN 048,"|D E",
              COLUMN 052,"|RG",
              COLUMN 055,"|SB",
              COLUMN 058,"|MENS",
              COLUMN 063,"|   MONTO SIE",
              COLUMN 078,"|    RETIRO 97",
              COLUMN 095,"|      CV",
              COLUMN 112,"|      CS",
              COLUMN 129,"|    ESTATAL",
              COLUMN 146,"|    ESPECIAL",
              COLUMN 163,"|   VIVIENDA 97",
              COLUMN 180,"|      RCV",
              COLUMN 197,"|",
              '\033015'

        -- Bloque titulos 5
        PRINT COLUMN 001,"|",
              COLUMN 013,"|",
              COLUMN 044,"|SEG",
              COLUMN 048,"|PEN",
              COLUMN 052,"|",
              COLUMN 055,"|",
              COLUMN 058,"|",
              COLUMN 063,"|   LIQUIDADO",
              COLUMN 078,"|    ACCIONES",
              COLUMN 095,"|    ACCIONES",
              COLUMN 112,"|    ACCIONES",
              COLUMN 129,"|    ACCIONES",
              COLUMN 146,"|    ACCIONES",
              COLUMN 163,"|    ACCIONES",
              COLUMN 180,"|    ACCIONES",
              COLUMN 197,"|",
              '\033015'

        -- Bloque titulos 6         cierra
        PRINT COLUMN 001,"\300",L10,L1,         -- nss
                         "\301",L10,L10,L10,    -- nombre
                         "\301", L2,L1,         -- tipo_seguro
                         "\301", L2,L1,         -- tipo_pension
                         "\301", L2,            -- regimen
                         "\301", L2,            -- siefore
                         "\301", L4,            -- mensualidad
                         "\301",L10,L2,L2,      -- acciones_tot_liquidado
                         "\301",L10,L5,L1,      -- acciones_ret97
                         "\301",L10,L5,L1,      -- acciones_cv
                         "\301",L10,L5,L1,      -- acciones_cs
                         "\301",L10,L5,L1,      -- acciones_est
                         "\301",L10,L5,L1,      -- acciones_esp
                         "\301",L10,L5,L1,      -- acciones_viv
                         "\301",L10,L5,L1,      -- acciones_rcv
                         "\331",
                         '\033015'
    ON EVERY ROW
        PRINT COLUMN 002, pr_rpt.nss                             ,
              COLUMN 014, pr_rpt.nombre                          ,
              COLUMN 046, pr_rpt.tipo_seguro                     ,
              COLUMN 050, pr_rpt.tipo_pension                    ,
              COLUMN 053, pr_rpt.regimen                         ,
              COLUMN 056, pr_rpt.siefore                USING "#&",
              COLUMN 060, pr_rpt.mensualidad            USING "#&",
              COLUMN 063, pr_rpt.acciones_tot_liquidado    USING "########&.&&&&&&",
              COLUMN 080, pr_rpt.acciones_ret97            USING "########&.&&&&&&",
              COLUMN 097, pr_rpt.acciones_cv               USING "########&.&&&&&&",
              COLUMN 114, pr_rpt.acciones_cs               USING "########&.&&&&&&",
              COLUMN 131, pr_rpt.acciones_est              USING "########&.&&&&&&",
              COLUMN 148, pr_rpt.acciones_esp              USING "########&.&&&&&&",
              COLUMN 165, pr_rpt.acciones_viv              USING "########&.&&&&&&",
              COLUMN 182, pr_rpt.acciones_rcv              USING "########&.&&&&&&",
              '\033015'

        LET li_siefore = pr_rpt.siefore

        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot             = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot + 1
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado + pr_rpt.acciones_tot_liquidado
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97         = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97 + pr_rpt.acciones_ret97
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv            = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv + pr_rpt.acciones_cv
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs            = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs + pr_rpt.acciones_cs
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est           = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est + pr_rpt.acciones_est
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp           = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp + pr_rpt.acciones_esp
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv           = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv + pr_rpt.acciones_viv
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv           = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv   + pr_rpt.acciones_rcv

    AFTER GROUP OF pr_rpt.tipo_retiro

        SKIP 1 LINE

        -- Imprimir totales por siefore del tipo retiro que termina
        FOR li_siefore = 1 TO 5
            IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN

                -- Verificar que quede espacio en la pagina
                IF LINENO > li_page_length_totales THEN
                    SKIP TO TOP OF PAGE
                END IF

                -- Bloque 1 : Titulo totales por tipo retiro y siefore
                PRINT COLUMN 001, "\332",L10,L1,                    -- nss
                                  "\302",L10,L10,L10,L10,L1,        -- totales
                                  "\302",L2,                        -- siefore
                                  "\302",L4,                        -- mensualidad
                                  "\302",L10,L2,L2,                 -- acciones_tot_liquidado
                                  "\302",L10,L5,L1,                 -- acciones_ret97
                                  "\302",L10,L5,L1,                 -- acciones_cv
                                  "\302",L10,L5,L1,                 -- acciones_cs
                                  "\302",L10,L5,L1,                 -- acciones_est
                                  "\302",L10,L5,L1,                 -- acciones_esp
                                  "\302",L10,L5,L1,                 -- acciones_viv
                                  "\302",L10,L5,L1,                 -- acciones_rcv
                                  "\277",
                                  '\033015'

                -- Bloque 2 : Detalle totales por tipo retiro y siefore
                PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                      COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                      COLUMN 055, "|S", li_siefore USING "&",
                      COLUMN 058, "|",
                      COLUMN 063, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado  USING "#######&.&&&&&&",
                      COLUMN 078, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97          USING "########&.&&&&&&",
                      COLUMN 095, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv             USING "########&.&&&&&&",
                      COLUMN 112, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs             USING "########&.&&&&&&",
                      COLUMN 129, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est            USING "########&.&&&&&&",
                      COLUMN 146, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp            USING "########&.&&&&&&",
                      COLUMN 163, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv            USING "########&.&&&&&&",
                      COLUMN 180, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv            USING "########&.&&&&&&",
                      COLUMN 197, "|",
                      '\033015'

                -- Bloque 3 : Titulo totales por tipo retiro y siefore
                PRINT COLUMN 001, "\300",L10,L1,                    --nss
                                  "\301",L10,L10,L10,L10,L1,        --totales
                                  "\301",L2,                        --siefore
                                  "\301",L4,                        --mensualidad
                                  "\301",L10,L2,L2,                 --acciones_tot_liquidado
                                  "\301",L10,L5,L1,                 --acciones_ret97
                                  "\301",L10,L5,L1,                 --acciones_cv
                                  "\301",L10,L5,L1,                 --acciones_cs
                                  "\301",L10,L5,L1,                 --acciones_est
                                  "\301",L10,L5,L1,                 --acciones_esp
                                  "\301",L10,L5,L1,                 --acciones_viv
                                  "\301",L10,L5,L1,                 --acciones_rcv
                                  "\331",
                                  '\033015'
                SKIP 1 LINE

                -- Acumular al gran total por tipo de retiro
                LET lar_gran_total_retiro[li_cont].tipo_retiro    = lar_total_retiro[li_cont].tipo_retiro
                LET lar_gran_total_retiro[li_cont].nss_tot        = lar_gran_total_retiro[li_cont].nss_tot        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
            END IF -- Si Total por siefore mayor que cero
        END FOR -- Total por siefore

        SKIP 2 LINES

    -------------------------------
    ON LAST ROW
    -------------------------------
        PRINT COLUMN 001, "R   E   S   U   M   E   N", '\033015'

        -- Imprimir totales por tipo de retiro y siefore
        FOR li_cont = 1 TO 1 -- Tipo de retiro

            FOR li_siefore = 1 TO 5
                IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN
                    -- Verificar que quede espacio en la pagina
                    IF LINENO > li_page_length_totales THEN
                        SKIP TO TOP OF PAGE
                    END IF

                    #Bloque 1 : Titulo totales por tipo retiro y siefore
                    PRINT COLUMN 001, "\332",L10,L1,                    --nss
                                      "\302",L10,L10,L10,L10,L1,        --totales
                                      "\302",L2,                        --siefore
                                      "\302",L4,                        --mensualidad
                                      "\302",L10,L2,L2,                 --acciones_tot_liquidado
                                      "\302",L10,L5,L1,                 --acciones_ret97
                                      "\302",L10,L5,L1,                 --acciones_cv
                                      "\302",L10,L5,L1,                 --acciones_cs
                                      "\302",L10,L5,L1,                 --acciones_est
                                      "\302",L10,L5,L1,                 --acciones_esp
                                      "\302",L10,L5,L1,                 --acciones_viv
                                      "\302",L10,L5,L1,                 --acciones_rcv
                                      "\277",
                                      '\033015'

                    -- Bloque 2 : detalle totales por tipo retiro y siefore
                    PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                          COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                          COLUMN 055, "|S", li_siefore USING "&",
                          COLUMN 058, "|",
                          COLUMN 063, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado USING "#######&.&&&&&&",
                          COLUMN 078, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97  USING "########&.&&&&&&",
                          COLUMN 095, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv USING "########&.&&&&&&",
                          COLUMN 112, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs USING "########&.&&&&&&",
                          COLUMN 129, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est USING "########&.&&&&&&",
                          COLUMN 146, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp USING "########&.&&&&&&",
                          COLUMN 163, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv USING "########&.&&&&&&",
                          COLUMN 180, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv USING "########&.&&&&&&",
                          COLUMN 197, "|",
                          '\033015'

                    -- Bloque 3 : Titulo totales por tipo retiro y siefore
                    PRINT COLUMN 001, "\300",L10,L1,                    --nss
                                      "\301",L10,L10,L10,L10,L1,        --totales
                                      "\301",L2,                        --siefore
                                      "\301",L4,                        --mensualidad
                                      "\301",L10,L2,L2,                 --acciones_tot_liquidado
                                      "\301",L10,L5,L1,                 --acciones_ret97
                                      "\301",L10,L5,L1,                 --acciones_cv
                                      "\301",L10,L5,L1,                 --acciones_cs
                                      "\301",L10,L5,L1,                 --acciones_est
                                      "\301",L10,L5,L1,                 --acciones_esp
                                      "\301",L10,L5,L1,                 --acciones_viv
                                      "\301",L10,L5,L1,                 --acciones_rcv
                                      "\331",
                                      '\033015'

                    SKIP 1 LINE

                    -- Acumular a los totales por siefore
                    LET lar_gran_total_siefore[li_siefore].nss_tot = lar_gran_total_siefore[li_siefore].nss_tot          + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
                    LET lar_gran_total_siefore[li_siefore].acciones_tot_liquidado = lar_gran_total_siefore[li_siefore].acciones_tot_liquidado  + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_tot_liquidado
                    LET lar_gran_total_siefore[li_siefore].acciones_ret97  = lar_gran_total_siefore[li_siefore].acciones_ret97      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret97
                    LET lar_gran_total_siefore[li_siefore].acciones_cv     = lar_gran_total_siefore[li_siefore].acciones_cv      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv
                    LET lar_gran_total_siefore[li_siefore].acciones_cs     = lar_gran_total_siefore[li_siefore].acciones_cs + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cs
                    LET lar_gran_total_siefore[li_siefore].acciones_est    = lar_gran_total_siefore[li_siefore].acciones_est   + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_est
                    LET lar_gran_total_siefore[li_siefore].acciones_esp    = lar_gran_total_siefore[li_siefore].acciones_esp     + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_esp
                    LET lar_gran_total_siefore[li_siefore].acciones_viv    = lar_gran_total_siefore[li_siefore].acciones_viv      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_viv
                    LET lar_gran_total_siefore[li_siefore].acciones_rcv    = lar_gran_total_siefore[li_siefore].acciones_rcv        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv

                END IF -- Total por retiro > 0
            END FOR -- Termina de imprimir totales por tipo de retiro y siefore


        END FOR -- Termina de imprimir totales por tipo de retiro

        SKIP 1 LINE

        -- Imprimir totales por siefore
        FOR li_siefore = 1 TO 5
            IF lar_gran_total_siefore[li_siefore].nss_tot > 0 THEN

                IF LINENO > li_page_length_totales THEN
                    SKIP TO TOP OF PAGE
                END IF

                -- Bloque 1 : Titulo totales por siefore
                PRINT COLUMN 001, "\332",L10,L1,                    --nss
                                  "\302",L10,L10,L10,L10,L1,        --totales
                                  "\302",L2,                        --siefore
                                  "\302",L4,                        --mensualidad
                                  "\302",L10,L2,L2,                 --acciones_tot_liquidado
                                  "\302",L10,L5,L1,                 --acciones_ret97
                                  "\302",L10,L5,L1,                 --acciones_cv
                                  "\302",L10,L5,L1,                 --acciones_cs
                                  "\302",L10,L5,L1,                 --acciones_est
                                  "\302",L10,L5,L1,                 --acciones_esp
                                  "\302",L10,L5,L1,                 --acciones_viv
                                  "\302",L10,L5,L1,                 --acciones_rcv
                                  "\277",
                                  '\033015'

                -- Bloque 2 : Detalle totales por siefore
                PRINT COLUMN 001, "|", lar_gran_total_siefore[li_siefore].nss_tot  USING "##########&",
                      COLUMN 013, "|TOTAL POR SIEFORE",
                      COLUMN 055, "|S", li_siefore USING "&",
                      COLUMN 058, "|",
                      COLUMN 063, "|", lar_gran_total_siefore[li_siefore].acciones_tot_liquidado USING "#######&.&&&&&&",
                      COLUMN 078, "|", lar_gran_total_siefore[li_siefore].acciones_ret97 USING "########&.&&&&&&",
                      COLUMN 095, "|", lar_gran_total_siefore[li_siefore].acciones_cv USING "########&.&&&&&&",
                      COLUMN 112, "|", lar_gran_total_siefore[li_siefore].acciones_cs USING "########&.&&&&&&",
                      COLUMN 129, "|", lar_gran_total_siefore[li_siefore].acciones_est USING "########&.&&&&&&",
                      COLUMN 146, "|", lar_gran_total_siefore[li_siefore].acciones_esp USING "########&.&&&&&&",
                      COLUMN 163, "|", lar_gran_total_siefore[li_siefore].acciones_viv USING "########&.&&&&&&",
                      COLUMN 180, "|", lar_gran_total_siefore[li_siefore].acciones_rcv USING "########&.&&&&&&",
                      COLUMN 197, "|",
                      '\033015'

                -- Bloque 3 : Titulo totales por siefore
                PRINT COLUMN 001, "\300",L10,L1,                    --nss
                                  "\301",L10,L10,L10,L10,L1,        --totales
                                  "\301",L2,                        --siefore
                                  "\301",L4,                        --mensualidad
                                  "\301",L10,L2,L2,                 --acciones_tot_liquidado
                                  "\301",L10,L5,L1,                 --acciones_ret97
                                  "\301",L10,L5,L1,                 --acciones_cv
                                  "\301",L10,L5,L1,                 --acciones_cs
                                  "\301",L10,L5,L1,                 --acciones_est
                                  "\301",L10,L5,L1,                 --acciones_esp
                                  "\301",L10,L5,L1,                 --acciones_viv
                                  "\301",L10,L5,L1,                 --acciones_rcv
                                  "\331",
                                  '\033015'
                SKIP 1 LINE

                -- Acumular a la ultima linea de totales
                LET lr_total_gral.nss_tot = lr_total_gral.nss_tot + lar_gran_total_siefore[li_siefore].nss_tot
                LET lr_total_gral.acciones_tot_liquidado = lr_total_gral.acciones_tot_liquidado  + lar_gran_total_siefore[li_siefore].acciones_tot_liquidado
                LET lr_total_gral.acciones_ret97 = lr_total_gral.acciones_ret97 + lar_gran_total_siefore[li_siefore].acciones_ret97
                LET lr_total_gral.acciones_cv    = lr_total_gral.acciones_cv + lar_gran_total_siefore[li_siefore].acciones_cv
                LET lr_total_gral.acciones_cs    = lr_total_gral.acciones_cs + lar_gran_total_siefore[li_siefore].acciones_cs
                LET lr_total_gral.acciones_est   = lr_total_gral.acciones_est + lar_gran_total_siefore[li_siefore].acciones_est
                LET lr_total_gral.acciones_esp   = lr_total_gral.acciones_esp + lar_gran_total_siefore[li_siefore].acciones_esp
                LET lr_total_gral.acciones_viv   = lr_total_gral.acciones_viv + lar_gran_total_siefore[li_siefore].acciones_viv
                LET lr_total_gral.acciones_rcv   = lr_total_gral.acciones_rcv + lar_gran_total_siefore[li_siefore].acciones_rcv

            END IF -- Si gran total > 0
        END FOR

{
        -- Imprimir total de nss y totales de vivienda
        -- Verificar que quede espacio en la pagina
        IF LINENO > li_page_length_totales THEN
            SKIP TO TOP OF PAGE
        END IF

        -- Bloque 1 : Titulo de nss y totales de vivienda
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

        -- Bloque 2 detalle de nss y totales de vivienda
        PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS",
              COLUMN 055, "|", lr_total_gral.nss_tot               USING "################&",
              COLUMN 073, "|",
              COLUMN 192, "|", lr_total_gral.pesos_fov08           USING "############&.&&",
              COLUMN 209, "|", lr_total_gral.pesos_fov92           USING "############&.&&",
              COLUMN 226, "|",
              COLUMN 243, "|",
              '\033015'

        -- Bloque 3 titulo de nss y totales de vivienda
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

}
END REPORT
