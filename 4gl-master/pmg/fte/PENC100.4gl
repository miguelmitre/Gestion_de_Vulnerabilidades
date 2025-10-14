################################################################################
#Owner             => E.F.P.                                                   #
#Programa PENC100  => CONSULTA GENERAL DE SOLICITUDES DE RETIROS PROGRAMADOS   #
#                     PENSION MINIMA GARANTIZADA (RETIRO S)                    #
#Fecha creacion    => 18 DE MARZO DE 2010                                      #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     => v1.2 ISAI JIMENEZ ROJAS MLM-1936                         #
#                  => Mostrar en pantalla identificador de ultimo pago         #
#Sistema           => PEN                                                      #
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        capturado   LIKE pen_estado_pmg.estado_solicitud    ,
        confirmado  LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado   LIKE pen_estado_pmg.estado_solicitud    ,
        rechazado   LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE
        enter               CHAR(001) ,
        gc_usuario          CHAR(020)

    DEFINE
        gs_cod_afore        SMALLINT

    DEFINE
        HOY                 DATE

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PENC100.log")
    CALL init()

    CALL f_busca_provision()


END MAIN


#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY         = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

{
    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"
}

    ----- CALCULA PROPORCIONAL PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_calcula_prop_pmg(?,?,?,?) "
    PREPARE eje_prop_pmg FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


#---------------------------------------------------------------------------#
# f_busca_provision : Realiza la busqueda de registros provisionados de PMG #
#---------------------------------------------------------------------------#
FUNCTION f_busca_provision()

    DEFINE lr_rango_fec RECORD
        inicial         DATE,
        final           DATE
    END RECORD

    DEFINE
        ls_salida          SMALLINT


    LET ls_salida   = 0

    WHILE ls_salida = 0
        CALL f_captura_fechas() RETURNING lr_rango_fec.*, ls_salida

        IF ls_salida = 0 THEN
            CALL f_despliega_datos(lr_rango_fec.*, HOY)
            CALL f_despliega_pantalla()
            
            WHILE TRUE
                PROMPT "¿ REALIZAR OTRA CONSULTA ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[nN]" THEN
                        LET ls_salida = 1
                    END IF

                    EXIT WHILE

                END IF
            END WHILE

            CLOSE WINDOW penc1002
        END IF
    END WHILE

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el rango de fechas con el que se realiza la    #
#                    busqueda en el programa                                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_fechas RECORD
        inicio        DATE,
        fin           DATE
    END RECORD

    DEFINE
        ls_exit        ,
        ls_estado      SMALLINT

    DEFINE
        lc_mensaje     CHAR(100)

    OPEN WINDOW penc1001 AT 2,2 WITH FORM "PENC1001" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                            <CTRL-C> - Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC100  CONSULTA GENERAL RETIROS - PENSION MINIMA GARANTIZADA               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET ls_exit           = 0

    LET lr_fechas.inicio  = HOY
    LET lr_fechas.fin     = HOY


    DISPLAY BY NAME lr_fechas.inicio
    DISPLAY BY NAME lr_fechas.fin

    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD inicio
            CALL f_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

        AFTER FIELD fin
            CALL f_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD inicio
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT

        #--
        ON KEY (ESC)
            CALL f_valida_fechas(lr_fechas.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

            CALL f_valida_fechas(lr_fechas.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fin
            ELSE
                IF lr_fechas.inicio > lr_fechas.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD inicio
                END IF
            END IF

            CALL f_valida_ejecucion(lr_fechas.*) RETURNING ls_exit
            EXIT INPUT

    END INPUT

    CLOSE WINDOW penc1001

    RETURN lr_fechas.*, ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_pantalla : Despliega la pantalla de consulta de registros     #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_pantalla()

    OPEN WINDOW penc1002 AT 2,2 WITH FORM "PENC1002" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-E> - Detalle de Montos                             <CTRL-C> - Regresar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " <CTRL-P> - Detalle de Pagos                                                  " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC100   CONSULTA DE PROVISION DE PENSION MINIMA GARANTIZADA                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         DATOS DE CONTROL INTERNO                             " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_datos : Realiza la consulta general y muestra los registros   #
#                     que cumplen con los parametros indicados              #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_datos(pdt_fechas, pdt_fecha_proceso)

    DEFINE pdt_fechas RECORD
        inicio          DATE,
        fin             DATE
    END RECORD

    DEFINE
        pdt_fecha_proceso       DATE

    DEFINE lr_datos_gen RECORD
        fecha_rcv       DATE            ,
        fecha_viv       DATE            ,
        precio_sie1     DECIMAL(10,6)   ,
        precio_sie11    DECIMAL(10,6)
    END RECORD

    DEFINE lr_datos_mto RECORD
        sec_contrato    LIKE pen_solicitud_pmg.sec_contrato         ,
        nss             LIKE pen_ctr_pago_det.nss                   ,
        fecha_pago_est  LIKE pen_ctr_pago_det.fecha_pago_estimada   ,
        num_mens        LIKE pen_ctr_pago_det.num_mensualidad       ,
        id_viv          LIKE pen_solicitud_pmg.estado_sub_viv       ,
        total_pesos     LIKE pen_ctr_pago_det.mto_pago_pesos        ,
        marca_ult_pago  LIKE pen_ctr_pago_det.marca_ult_pago      
    END RECORD

    DEFINE lar_det_montos ARRAY[100] OF RECORD
        sec_contrato    LIKE pen_solicitud_pmg.sec_contrato         ,
        nss             LIKE pen_ctr_pago_det.nss                   ,
        fecha_pago_est  LIKE pen_ctr_pago_det.fecha_pago_estimada   ,
        num_mens        LIKE pen_ctr_pago_det.num_mensualidad       ,
        id_viv          LIKE pen_solicitud_pmg.estado_sub_viv       ,
        total_pesos     LIKE pen_ctr_pago_det.mto_pago_pesos        ,
        marca_ult_pago  LIKE pen_ctr_pago_det.marca_ult_pago      
    END RECORD

    DEFINE lar_consecutivo ARRAY[100] OF RECORD
        nss             LIKE pen_ctr_pago_det.nss           ,
        consecutivo     LIKE pen_solicitud_pmg.consecutivo
    END RECORD

    DEFINE lr_pago_prop RECORD
        subcuenta       SMALLINT        ,
        siefore         SMALLINT        ,
        monto_acciones  DECIMAL(16,6)   ,
        acciones_pagar  DECIMAL(16,6)   ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    DEFINE lr_totales RECORD
        tot_acc_viv     DECIMAL(16,6),
        tot_pes_viv     DECIMAL(16,6),
        tot_acc_rcv     DECIMAL(16,6),
        tot_pes_rcv     DECIMAL(16,6)
    END RECORD

    DEFINE li_consec LIKE pen_solicitud_pmg.consecutivo

    DEFINE
        li_tot_elem         ,
        li_arr_elem         ,
        li_scr_elem         ,
        li_cnt              INTEGER

    DEFINE
        lc_fecha_viv        CHAR(10)

    #-- --------------------------------------------------------------------------------
    CALL f_despliega_pantalla()

    FOR  li_cnt = 1 TO 100
        INITIALIZE lar_det_montos[li_cnt].* TO NULL
        INITIALIZE lar_consecutivo[li_cnt].* TO NULL
    END FOR

    LET li_cnt      = 1
    LET li_tot_elem = 0
    LET li_arr_elem = 0
    LET li_scr_elem = 0

    LET lr_totales.tot_acc_viv      = 0
    LET lr_totales.tot_pes_viv      = 0
    LET lr_totales.tot_acc_rcv      = 0
    LET lr_totales.tot_pes_rcv      = 0

    LET lr_datos_gen.fecha_rcv      = gar_precio_acc[1].fecha
    LET lr_datos_gen.fecha_viv      = gar_precio_acc[11].fecha
    LET lr_datos_gen.precio_sie1    = gar_precio_acc[1].precio_dia
    LET lr_datos_gen.precio_sie11   = gar_precio_acc[11].precio_dia

    DISPLAY BY NAME lr_datos_gen.*

    DECLARE cur_nss CURSOR FOR
        SELECT  B.sec_contrato          ,
                A.nss                   ,
                A.fecha_pago_estimada   ,
                A.num_mensualidad       ,
                B.estado_sub_viv        ,
                A.mto_pago_pesos        ,
                A.marca_ult_pago        ,
                B.consecutivo
        FROM    pen_ctr_pago_det A      ,
                pen_solicitud_pmg B
        WHERE  A.nss         = B.nss
        AND    A.consecutivo = B.consecutivo
        AND    A.fecha_pago_estimada BETWEEN pdt_fechas.inicio AND pdt_fechas.fin

    FOREACH cur_nss INTO lr_datos_mto.*,
                         li_consec

        LET lar_det_montos[li_cnt].*            = lr_datos_mto.*
        LET lar_consecutivo[li_cnt].nss         = lr_datos_mto.nss
        LET lar_consecutivo[li_cnt].consecutivo = li_consec

        LET li_cnt = li_cnt + 1

        DECLARE cur_prop CURSOR FOR eje_prop_pmg

        FOREACH cur_prop USING lr_datos_mto.nss         ,
                               li_consec                ,
                               lr_datos_mto.total_pesos ,
                               pdt_fecha_proceso
                         INTO  lr_pago_prop.*

            IF lr_pago_prop.subcuenta = 4 THEN
                LET lr_totales.tot_acc_viv = lr_pago_prop.acciones_pagar + lr_totales.tot_acc_viv
                LET lr_totales.tot_pes_viv = lr_pago_prop.pesos_pagar    + lr_totales.tot_pes_viv
            ELSE
                LET lr_totales.tot_acc_rcv = lr_pago_prop.acciones_pagar + lr_totales.tot_acc_rcv
                LET lr_totales.tot_pes_rcv = lr_pago_prop.pesos_pagar    + lr_totales.tot_pes_rcv
            END IF

        END FOREACH -- Pago prop

    END FOREACH

    DISPLAY BY NAME lr_totales.*


    CALL SET_COUNT(li_cnt-1)
    DISPLAY ARRAY lar_det_montos TO scr_montos.*

        -- Muestra la pantalla de Detalle de montos del pago
        ON KEY (CONTROL-E)
            LET li_arr_elem = ARR_CURR()
            LET li_scr_elem = SCR_LINE()

            CALL f_detalle_montos(lar_det_montos[li_arr_elem].nss           ,
                                  lar_consecutivo[li_arr_elem].consecutivo  ,
                                  lar_det_montos[li_arr_elem].sec_contrato  ,
                                  lar_det_montos[li_arr_elem].total_pesos   ,
                                  pdt_fecha_proceso                         )

        -- Muestra la pantalla de Detalle de Pagos
        ON KEY (CONTROL-P)
            LET li_arr_elem = ARR_CURR()
            LET li_scr_elem = SCR_LINE()

            CALL f_detalle_pagos(lar_det_montos[li_arr_elem].nss            ,
                                 lar_consecutivo[li_arr_elem].consecutivo   ,
                                 lar_det_montos[li_arr_elem].sec_contrato   )

    END DISPLAY


    CLOSE WINDOW penc1002

END FUNCTION


#---------------------------------------------------------------------------#
# f_detalle_montos : Realiza la consulta de los montos a provisionar del    #
#                    nss seleccionado en la consulta general                #
#---------------------------------------------------------------------------#
FUNCTION f_detalle_montos(pr_detalle)

    DEFINE pr_detalle RECORD
        nss             LIKE pen_ctr_pago_det.nss           ,
        consecutivo     LIKE pen_solicitud_pmg.consecutivo  ,
        sec_contrato    LIKE pen_solicitud_pmg.sec_contrato ,
        mensualidad     LIKE pen_ctr_pago_det.mto_pago_pesos,
        fecha_proc      DATE
    END RECORD

    DEFINE lar_det_mto ARRAY[1] OF RECORD
        pesos_ret97         DECIMAL(16,6),
        pesos_cs            DECIMAL(16,6),
        pesos_cv            DECIMAL(16,6),
        pesos_estatal       DECIMAL(16,6),
        pesos_especial      DECIMAL(16,6),
        pesos_total_cv      DECIMAL(16,6),
        pesos_total_rcv     DECIMAL(16,6),
        pesos_vivenda       DECIMAL(16,6),
        pesos_total         DECIMAL(16,6),
        acc_ret97           DECIMAL(16,6),
        acc_cs              DECIMAL(16,6),
        acc_cv              DECIMAL(16,6),
        acc_estatal         DECIMAL(16,6),
        acc_especial        DECIMAL(16,6),
        acc_total_cv        DECIMAL(16,6),
        acc_total_rcv       DECIMAL(16,6),
        acc_vivenda         DECIMAL(16,6),
        acc_total           DECIMAL(16,6)
    END RECORD

    DEFINE lr_afi RECORD
        paterno     LIKE afi_mae_afiliado.paterno,
        materno     LIKE afi_mae_afiliado.materno,
        nombre      LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE lr_pago RECORD
        subcuenta       SMALLINT        ,
        siefore         SMALLINT        ,
        monto_acciones  DECIMAL(16,6)   ,
        acciones_pagar  DECIMAL(16,6)   ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    #-- ----------------------------------------------------------------

    OPEN WINDOW penc1003 AT 2,2 WITH FORM "PENC1003" ATTRIBUTE(BORDER)
    DISPLAY "                                                          <CTRL-C> - Regresar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC100   CONSULTA DE PROVISION DE PENSION MINIMA GARANTIZADA                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                      DETALLE DE PAGOS POR SUBCUENTA                          " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET lar_det_mto[1].pesos_ret97      = 0
    LET lar_det_mto[1].pesos_total_rcv  = 0
    LET lar_det_mto[1].acc_ret97        = 0
    LET lar_det_mto[1].acc_total_rcv    = 0
    LET lar_det_mto[1].pesos_cv         = 0
    LET lar_det_mto[1].pesos_total_cv   = 0
    LET lar_det_mto[1].pesos_total_rcv  = 0
    LET lar_det_mto[1].acc_cv           = 0
    LET lar_det_mto[1].acc_total_cv     = 0
    LET lar_det_mto[1].acc_total_rcv    = 0
    LET lar_det_mto[1].pesos_estatal    = 0
    LET lar_det_mto[1].pesos_total_cv   = 0
    LET lar_det_mto[1].pesos_total_rcv  = 0
    LET lar_det_mto[1].acc_estatal      = 0
    LET lar_det_mto[1].acc_total_cv     = 0
    LET lar_det_mto[1].acc_total_rcv    = 0
    LET lar_det_mto[1].pesos_especial   = 0
    LET lar_det_mto[1].pesos_total_cv   = 0
    LET lar_det_mto[1].pesos_total_rcv  = 0
    LET lar_det_mto[1].acc_especial     = 0
    LET lar_det_mto[1].acc_total_cv     = 0
    LET lar_det_mto[1].acc_total_rcv    = 0
    LET lar_det_mto[1].pesos_cs         = 0
    LET lar_det_mto[1].pesos_total_rcv  = 0
    LET lar_det_mto[1].acc_cs           = 0
    LET lar_det_mto[1].acc_total_rcv    = 0
    LET lar_det_mto[1].pesos_vivenda    = 0
    LET lar_det_mto[1].acc_vivenda      = 0
    LET lar_det_mto[1].pesos_total      = 0
    LET lar_det_mto[1].acc_total        = 0

    CALL f_obtiene_nombres(pr_detalle.nss) RETURNING lr_afi.*

    DISPLAY pr_detalle.consecutivo TO consecutivo
    
    DISPLAY BY NAME pr_detalle.nss          ,
                    pr_detalle.mensualidad  ,
                    lr_afi.*

    DECLARE cur_deta CURSOR FOR eje_prop_pmg

    FOREACH cur_deta USING pr_detalle.nss           ,
                           pr_detalle.consecutivo   ,
                           pr_detalle.mensualidad   ,
                           pr_detalle.fecha_proc
                     INTO  lr_pago.*

        LET lar_det_mto[1].pesos_total  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total
        LET lar_det_mto[1].acc_total    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total

        CASE lr_pago.subcuenta

            WHEN 1
                LET lar_det_mto[1].pesos_ret97      = lr_pago.pesos_pagar + lar_det_mto[1].pesos_ret97
                LET lar_det_mto[1].pesos_total_rcv  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_rcv

                LET lar_det_mto[1].acc_ret97        = lr_pago.acciones_pagar + lar_det_mto[1].acc_ret97
                LET lar_det_mto[1].acc_total_rcv    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_rcv

            WHEN 2
                LET lar_det_mto[1].pesos_cv         = lr_pago.pesos_pagar + lar_det_mto[1].pesos_cv
                LET lar_det_mto[1].pesos_total_cv   = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_cv
                LET lar_det_mto[1].pesos_total_rcv  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_rcv

                LET lar_det_mto[1].acc_cv           = lr_pago.acciones_pagar + lar_det_mto[1].acc_cv
                LET lar_det_mto[1].acc_total_cv     = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_cv
                LET lar_det_mto[1].acc_total_rcv    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_rcv

            WHEN 6
                LET lar_det_mto[1].pesos_estatal    = lr_pago.pesos_pagar + lar_det_mto[1].pesos_estatal
                LET lar_det_mto[1].pesos_total_cv   = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_cv
                LET lar_det_mto[1].pesos_total_rcv  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_rcv

                LET lar_det_mto[1].acc_estatal      = lr_pago.acciones_pagar + lar_det_mto[1].acc_estatal
                LET lar_det_mto[1].acc_total_cv     = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_cv
                LET lar_det_mto[1].acc_total_rcv    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_rcv

            WHEN 9
                LET lar_det_mto[1].pesos_especial   = lr_pago.pesos_pagar + lar_det_mto[1].pesos_especial
                LET lar_det_mto[1].pesos_total_cv   = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_cv
                LET lar_det_mto[1].pesos_total_rcv  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_rcv

                LET lar_det_mto[1].acc_especial     = lr_pago.acciones_pagar + lar_det_mto[1].acc_especial
                LET lar_det_mto[1].acc_total_cv     = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_cv
                LET lar_det_mto[1].acc_total_rcv    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_rcv

            WHEN 5
                LET lar_det_mto[1].pesos_cs         = lr_pago.pesos_pagar + lar_det_mto[1].pesos_cs
                LET lar_det_mto[1].pesos_total_rcv  = lr_pago.pesos_pagar + lar_det_mto[1].pesos_total_rcv

                LET lar_det_mto[1].acc_cs           = lr_pago.acciones_pagar + lar_det_mto[1].acc_cs
                LET lar_det_mto[1].acc_total_rcv    = lr_pago.acciones_pagar + lar_det_mto[1].acc_total_rcv

            WHEN 4
                LET lar_det_mto[1].pesos_vivenda    = lr_pago.pesos_pagar + lar_det_mto[1].pesos_vivenda
                LET lar_det_mto[1].acc_vivenda      = lr_pago.acciones_pagar + lar_det_mto[1].acc_vivenda

        END CASE

    END FOREACH -- Pago prop

    CALL SET_COUNT(1)
    DISPLAY ARRAY lar_det_mto TO scr_detalle.*

    CLOSE WINDOW penc1003

END FUNCTION

#---------------------------------------------------------------------------#
# f_detalle_pagos : Realiza la consulta de la relacion de pagos para el nss #
#---------------------------------------------------------------------------#
FUNCTION f_detalle_pagos(pr_pagos)

    DEFINE pr_pagos RECORD
        nss             LIKE pen_ctr_pago_det.nss           ,
        consecutivo     LIKE pen_solicitud_pmg.consecutivo  ,
        sec_contrato    LIKE pen_solicitud_pmg.sec_contrato
    END RECORD

    DEFINE lar_pagos ARRAY[50] OF RECORD
        sec_contrato            LIKE pen_ctr_pago_det.sec_contrato        ,
        num_mensualidad         LIKE pen_ctr_pago_det.num_mensualidad     ,
        fecha_pago_estimada     LIKE pen_ctr_pago_det.fecha_pago_estimada ,
        folio_liquida           LIKE pen_ctr_pago_det.folio_liquida       ,
        fecha_liquida           LIKE pen_ctr_pago_det.fecha_liquida       ,
        estado                  LIKE pen_estado_pmg.descripcion           ,
        mto_pago_pesos          LIKE pen_ctr_pago_det.mto_pago_pesos
    END RECORD

    DEFINE lr_pago_det RECORD LIKE pen_ctr_pago_det.*

    DEFINE lr_afi RECORD
        paterno     LIKE afi_mae_afiliado.paterno,
        materno     LIKE afi_mae_afiliado.materno,
        nombre      LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE
        ls_cont         SMALLINT

    -- ---------------------------------------------------------------------

    OPEN WINDOW penc1004 AT 2,2 WITH FORM "PENC1004" ATTRIBUTE(BORDER)
    DISPLAY "                                                          <CTRL-C> - Regresar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC100       CONSULTA DE PAGOS DE PENSION MINIMA GARANTIZADA                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                       DETALLE DE PROGRAMACION DE PAGOS                       " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET ls_cont = 1

    CALL f_obtiene_nombres(pr_pagos.nss) RETURNING lr_afi.*

    DISPLAY pr_pagos.consecutivo TO consecutivo
    DISPLAY BY NAME pr_pagos.nss          ,
                    lr_afi.*

    DECLARE cur_pago CURSOR FOR
    SELECT *
    FROM   pen_ctr_pago_det
    WHERE  nss          = pr_pagos.nss
    AND    consecutivo  = pr_pagos.consecutivo
    AND    sec_contrato = pr_pagos.sec_contrato
    ORDER BY num_mensualidad

    FOREACH cur_pago INTO lr_pago_det.*

        LET lar_pagos[ls_cont].sec_contrato         = lr_pago_det.sec_contrato
        LET lar_pagos[ls_cont].num_mensualidad      = lr_pago_det.num_mensualidad
        LET lar_pagos[ls_cont].fecha_pago_estimada  = lr_pago_det.fecha_pago_estimada
        LET lar_pagos[ls_cont].folio_liquida        = lr_pago_det.folio_liquida
        LET lar_pagos[ls_cont].fecha_liquida        = lr_pago_det.fecha_liquida
        LET lar_pagos[ls_cont].mto_pago_pesos       = lr_pago_det.mto_pago_pesos

        SELECT A.descripcion
        INTO   lar_pagos[ls_cont].estado
        FROM   pen_estado_pmg A
        WHERE  A.estado_solicitud = lr_pago_det.estado

        LET ls_cont = ls_cont + 1

    END FOREACH

    CALL SET_COUNT(ls_cont-1)
    DISPLAY ARRAY lar_pagos TO scr_pagos.*

    CLOSE WINDOW penc1004

END FUNCTION



#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(pdt_fecha)

    DEFINE
        pdt_fecha           DATE

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado = 0

    IF pdt_fecha IS NULL THEN
        LET lc_mensaje = " LA FECHA NO DEBE SER NULA"
        LET ls_estado = 1
    ELSE
        IF pdt_fecha < "01/01/1900" THEN
            LET lc_mensaje = " FECHA INVALIDA"
            LET ls_estado = 1
        ELSE
{
            IF pdt_fecha > HOY THEN
                LET lc_mensaje = " LA FECHA NO DEBE SER MAYOR A LA FECHA DEL DIA"
                LET ls_estado = 1
            END IF
}
        END IF
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_ejecucion : Valida que se cumplan las condiciones previas para   #
#                      ejecutar el programa                                 #
#---------------------------------------------------------------------------#
FUNCTION f_valida_ejecucion(pr_fechas)

    DEFINE pr_fechas RECORD #glo #reg_1
        inicio        DATE,
        fin           DATE
    END RECORD

    DEFINE
        ls_exit        SMALLINT

    DEFINE
        li_regs        INTEGER

    LET li_regs = 0
    LET ls_exit = 0

    -- Validamos y obtenemos precios de accion
    CALL f_obtiene_precios_accion(HOY)

    -- Validamos que existan registros
    SELECT nvl(COUNT(UNIQUE B.folio_lote),0)
    INTO   li_regs
    FROM   pen_ctr_pago_det A ,
           pen_ctr_pago     B
    WHERE  A.nss         = B.nss
    AND    A.consecutivo = B.consecutivo
    AND    A.fecha_pago_estimada BETWEEN pr_fechas.inicio AND pr_fechas.fin

    IF li_regs = 0 THEN
        CALL f_error_msg ("NO EXISTEN REGISTROS EN EL INTERVALO CAPTURADO")
        LET ls_exit = 1
    END IF

    RETURN ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_nombres : Regresa los nombres del nss dado                      #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_nombres(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE lr_afi RECORD
        ap_paterno      LIKE afi_mae_afiliado.paterno   ,
        ap_materno      LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres
    END RECORD

    --  ------------------------------------------------------------------------

    SELECT paterno  ,
           materno  ,
           nombres
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    RETURN lr_afi.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por el usuario                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE
        pdt_fec_precios       DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_exe_precios        CHAR(100) ,
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE
        li_cont               SMALLINT

    LET li_cont = 0

    LET lc_exe_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_exe_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", pdt_fec_precios,
                             ", SIEFORE: ", lc_siefore

            LET lc_mensaje = lc_mensaje CLIPPED
            CALL f_error_msg(lc_mensaje)
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION
