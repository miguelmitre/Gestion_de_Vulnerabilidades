#################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                 #
#Owner             => E.F.P.                                                    #
#Programa RETC855  => LIQUIDA Y GENERA LA OPERACION 16 PARA LOS RETIROS         #
#                     PARCIALES POR DESEMPLEO, TIPOS A Y B                      #
#Fecha creacion    => 22 DE MAYO DE 2009                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 16 DE JUNIO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrego el tipo desempleo C a la liquidacion            #
#Fecha actualiz.   => 26 DE JUNIO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se elimina la generacion de la operacion 16 desde este    #
#                  => programa y se traslada al programa RETC857                #
#Fecha actualiz.   => 31 DE MARZO DE 2011                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega la ejecucion del SPL que inserta en las tablas  #
#                     de las estadisticas de CONSAR (Req. EFPS-152)             #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD #gr_edo
        procesado             LIKE ret_estado.estado_solicitud ,
        provisionado          LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_cont RECORD
        tipo_a      INTEGER,
        tipo_b      INTEGER,
        tipo_c      INTEGER
    END RECORD

    DEFINE gr_fechas RECORD
        fecha_inicio        DATE,
        fecha_fin           DATE
    END RECORD

    DEFINE #glo #smallint
        gs_salida              ,
        gs_codigo_afore        SMALLINT

    DEFINE #glo #char
        gc_usuario            CHAR(012) ,
        enter                 CHAR(001)

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #decimal
        gd_tot_monto_pesos    DECIMAL(15,2)

    DEFINE gc_id_aportante  LIKE dis_cuenta.id_aportante
    DEFINE gs_cod_sct       LIKE tab_afore_local.codigo_afore
    DEFINE gd_sal_minimo    LIKE tabsalario_minimo2.monto_sm
    DEFINE gd_limite_pago_a LIKE ret_ctr_pago.mto_pago

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC855.log")

    CALL init()
    WHILE gs_salida = 0
        CALL f_captura_fechas() RETURNING gr_fechas.*, gs_salida

        IF gs_salida = 0 THEN
            CALL f_obtiene_folios(gr_fechas.*)
        END IF
    END WHILE

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------


    LET HOY                 = TODAY
    LET gd_tot_monto_pesos  = 0
    LET gs_salida           = 0
    LET gc_id_aportante     = "RETIRO"
    LET gr_cont.tipo_a      = 0
    LET gr_cont.tipo_b      = 0
    LET gr_cont.tipo_c      = 0

    -- Obtenemos el salario minimo vigente para el DF
    SELECT monto_sm
    INTO   gd_sal_minimo
    FROM   tabsalario_minimo2
    WHERE  fecha_hasta_sm IS NULL
    AND    zona_cod = "A"

    -- El maximo monto a retirar para los Retiros Tipo A
    -- debe ser 10 veces el Salario Minimo Mensual del DF
    LET gd_limite_pago_a = (gd_sal_minimo * 30) * 10

    SELECT codigo_afore   ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

    --- Claves especiales de afore ---
    SELECT afore_cod
    INTO   gs_cod_sct
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*SCOTIA*"

    --- Estados de solicitud ---
    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    ----- PAGO DE DIAS DE SALARIO BASE COT  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_calcula_pago_sbc_parciales (?,?,?)"
    PREPARE eje_parcial_sbc FROM lc_prepare

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_parciales_consar(?,?,?,?)"
    PREPARE eje_CONSAR_parcial FROM lc_prepare

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " "
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el intervalo de fechas para realizar la        #
#                    de registros a liquidar                                #
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

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc8551 AT 4,4 WITH FORM "RETC8551" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC855 CONSULTA Y LIQUIDACION DE PARCIALES - RETIROS A, B Y C              " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET ls_exit           = 0
    LET lr_fechas.fin     = HOY
    LET lr_fechas.inicio  = HOY

    DISPLAY BY NAME lr_fechas.fin
    DISPLAY BY NAME lr_fechas.inicio

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

        ON KEY(ESC)
            LET ls_exit = f_valida_ejecucion(lr_fechas.*)
            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT
    END INPUT

    CLOSE WINDOW retc8551

    RETURN lr_fechas.*, ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_folios : Recupera los folios por liquidar que se encuentren en  #
#                    el rango de fechas capturado                           #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_folios(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio        DATE,
        fin           DATE
    END RECORD

    DEFINE lar_folios ARRAY[20] OF RECORD
        id_liquida      CHAR(1)                             ,
        fecha_liquida   LIKE ret_ctr_pago_det.fecha_liquida ,
        num_folios      INTEGER                             ,
        folio_01        LIKE ret_ctr_pago.folio_op12        ,
        folio_02        LIKE ret_ctr_pago.folio_op12        ,
        folio_03        LIKE ret_ctr_pago.folio_op12        ,
        folio_04        LIKE ret_ctr_pago.folio_op12        ,
        folio_05        LIKE ret_ctr_pago.folio_op12
    END RECORD

    DEFINE li_folio_op12 LIKE ret_ctr_pago.folio_op12

    DEFINE
        ld_fecha_cont         DATE

    DEFINE
        ls_flag               SMALLINT

    DEFINE #glo #integer
        i                     ,
        li_cont               ,
        li_folios             ,
        li_dias               ,
        v_arr_elemento        ,
        v_scr_elemento        INTEGER

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc8552 AT 4,4 WITH FORM "RETC8552" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-E> - Detalle                                        <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC855   CONSULTA Y LIQUIDACION DE PARCIALES - RETIROS A Y B               " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    FOR i = 1 TO 20
        INITIALIZE lar_folios[i].* TO NULL
    END FOR

    LET ls_flag         = 0 -- Indica si existen folios a liquidar el dia de hoy
    LET li_cont         = 1
    LET ld_fecha_cont   = pr_fechas.inicio
    LET li_dias         = (pr_fechas.fin - pr_fechas.inicio) + 1

    FOR i = 1 TO li_dias

        LET li_folios = 0

        DECLARE cur_fec CURSOR FOR
        SELECT B.folio_op12
        FROM   ret_ctr_pago_det A ,
               ret_ctr_pago     B ,
               ret_parcial      C
        WHERE  A.nss             = B.nss
        AND    A.consecutivo     = B.consecutivo
        AND    A.nss             = C.nss
        AND    A.consecutivo     = C.consecutivo
        AND    C.diag_cuenta_ind = 400
        AND    A.fecha_liquida   = ld_fecha_cont
        AND    A.estado          = gr_edo.provisionado
        AND    B.estado          = gr_edo.provisionado
        GROUP BY 1
        ORDER BY 1

        FOREACH cur_fec INTO li_folio_op12

            LET li_folios = li_folios + 1

            CASE li_folios
                WHEN 1
                    LET lar_folios[li_cont].folio_01 = li_folio_op12
                WHEN 2
                    LET lar_folios[li_cont].folio_02 = li_folio_op12
                WHEN 3
                    LET lar_folios[li_cont].folio_03 = li_folio_op12
                WHEN 4
                    LET lar_folios[li_cont].folio_04 = li_folio_op12
                WHEN 5
                    LET lar_folios[li_cont].folio_05 = li_folio_op12
            END CASE

        END FOREACH

        IF li_folios > 0 THEN
            IF ld_fecha_cont = HOY THEN
                IF f_regs_por_liquidar(ld_fecha_cont) > 0 THEN
                    LET ls_flag = 1
                    LET lar_folios[li_cont].id_liquida = "*"
                END IF
            END IF

            LET lar_folios[li_cont].fecha_liquida = ld_fecha_cont
            LET lar_folios[li_cont].num_folios    = li_folios
            LET li_cont = li_cont + 1
        END IF

        LET ld_fecha_cont = ld_fecha_cont + 1

    END FOR

    IF ls_flag = 1 THEN
        DISPLAY "<CTRL-I> - LIQUIDAR" AT 1,30 ATTRIBUTE(REVERSE)
        DISPLAY  "TIENE FOLIOS PENDIENTES DE LIQUIDACION PARA EL DIA DE HOY..."
                 AT 19,10 ATTRIBUTE(REVERSE)
    END IF

    CALL SET_COUNT(li_cont-1)
    DISPLAY ARRAY lar_folios TO scr_folios.*

        -- Detalle de folios
        ON KEY (CONTROL-E)
            LET v_arr_elemento = ARR_CURR()
            LET v_scr_elemento = SCR_LINE()

            CALL f_obtiene_detalle(lar_folios[v_arr_elemento].fecha_liquida)

        -- Liquidacion
        ON KEY (CONTROL-I)
            IF ls_flag = 1 THEN

                WHILE TRUE
                    PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            IF gs_codigo_afore = gs_cod_sct THEN
                                CALL f_recalcula_montos(HOY)
                            END IF

                            CALL f_liquida_registros(HOY)

                            EXIT DISPLAY
                        ELSE
                            PROMPT " LIQUIDACION CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                            EXIT WHILE
                        END IF
                    END IF
                END WHILE
            ELSE
                ERROR "NO EXISTEN FOLIOS PARA LIQUIDAR HOY ... "
                SLEEP 2
                ERROR " "
            END IF

    END DISPLAY

    CLOSE WINDOW retc8552

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_detalle : Muestra el detalle de montos a pagar por siefore de   #
#                     los folios por liquidar                               #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_detalle(p_fecha_liq)

    DEFINE
        p_fecha_liq     DATE

    DEFINE lr_totales RECORD
        folio_op12      LIKE ret_ctr_pago.folio_op12    ,
        siefore         SMALLINT                        ,
        total_pesos     DECIMAL(14,2)                   ,
        total_accion    DECIMAL(14,6)
    END RECORD

    DEFINE lar_det_montos ARRAY[30] OF RECORD
        folio           LIKE ret_ctr_pago.folio_op12    ,
        pesos_sie1      DECIMAL(14,2)                   ,
        pesos_sie2      DECIMAL(14,2)                   ,
        pesos_sie3      DECIMAL(14,2)                   ,
        pesos_sie4      DECIMAL(14,2)                   ,
        pesos_sie5      DECIMAL(14,2)
    END RECORD

    DEFINE
        li_cnt          INTEGER

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc8554 AT 4,4 WITH FORM "RETC8554" ATTRIBUTE(BORDER)
    DISPLAY "                                                        <CTRL-C> - Regresar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC855         DETALLE DE LIQUIDACION DE PARCIALES (EN PESOS)              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    FOR  li_cnt = 1 TO 30
        INITIALIZE lar_det_montos[li_cnt].* TO NULL
    END FOR

    LET li_cnt = 1

    DISPLAY p_fecha_liq TO fecha_liquida

    DECLARE cur_mto CURSOR FOR
    SELECT A.folio_op12          ,
           C.codigo_siefore      ,
           SUM(B.mto_mensualidad) pesos_tot
    FROM   ret_ctr_pago     A   ,
           ret_ctr_pago_det B   ,
           cta_nss_regimen  C   ,
           ret_parcial      D
    WHERE  A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo
    AND    A.nss             = D.nss
    AND    A.consecutivo     = D.consecutivo
    AND    B.fecha_liquida   = p_fecha_liq
    AND    D.diag_cuenta_ind = 400
    AND    B.nss             = C.nss
    AND    C.grupo_regimen   = 1
    AND    A.estado          = gr_edo.provisionado
    AND    B.estado          = gr_edo.provisionado
    GROUP BY 1,2
    ORDER BY 1,2

    FOREACH cur_mto INTO lr_totales.*
        IF li_cnt = 1 THEN
            LET lar_det_montos[li_cnt].folio = lr_totales.folio_op12
        ELSE
            -- Avanzamos al siguiente elemento del arreglo cuando sea
            -- un folio diferente
            IF lar_det_montos[li_cnt -1].folio <> lr_totales.folio_op12 THEN
                LET lar_det_montos[li_cnt].folio = lr_totales.folio_op12
            ELSE
                LET li_cnt = li_cnt - 1
            END IF
        END IF

        CASE lr_totales.siefore
            WHEN 1
                LET lar_det_montos[li_cnt].pesos_sie1 = lr_totales.total_pesos

            WHEN 2
                LET lar_det_montos[li_cnt].pesos_sie2 = lr_totales.total_pesos

            WHEN 3
                LET lar_det_montos[li_cnt].pesos_sie3 = lr_totales.total_pesos

            WHEN 4
                LET lar_det_montos[li_cnt].pesos_sie4 = lr_totales.total_pesos

            WHEN 5
                LET lar_det_montos[li_cnt].pesos_sie5 = lr_totales.total_pesos
        END CASE

        LET li_cnt = li_cnt + 1
    END FOREACH

    CALL SET_COUNT(li_cnt-1)
    DISPLAY ARRAY lar_det_montos TO scr_montos.*

    CLOSE WINDOW retc8554

END FUNCTION

#---------------------------------------------------------------------------#
# f_regs_por_liquidar : Cuenta el numero de registros por liquidar para una #
#                       fecha dada                                          #
#---------------------------------------------------------------------------#
FUNCTION f_regs_por_liquidar(p_fecha)

    DEFINE
        p_fecha     DATE

    DEFINE
        ls_regs     INTEGER

    LET ls_regs = 0

    -- Se valida el numero de registros sobre ret_parcial_tx
    -- para asegurarnos que ya se haya recibido la operación 13
    SELECT COUNT(*)
    INTO   ls_regs
    FROM   ret_ctr_pago      A ,
           ret_ctr_pago_det  B ,
           ret_parcial_tx    C ,
           ret_parcial       D
    WHERE  A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo
    AND    A.nss             = C.nss
    AND    A.consecutivo     = C.consecutivo
    AND    A.nss             = D.nss
    AND    A.consecutivo     = D.consecutivo
    AND    B.fecha_liquida   = p_fecha
    AND    D.diag_cuenta_ind = 400
    AND    A.estado          = gr_edo.provisionado
    AND    B.estado          = gr_edo.provisionado

    RETURN ls_regs

END FUNCTION

#---------------------------------------------------------------------------#
# f_liquida_registros : Realiza la liquidacion de los registros que         #
#                       corresponda pagarse en la fecha dada                #
#---------------------------------------------------------------------------#
FUNCTION f_liquida_registros(p_fecha_liq)

    DEFINE
        p_fecha_liq     DATE

    DEFINE lr_liquida RECORD
        nss                 LIKE ret_ctr_pago.nss                 ,
        consecutivo         LIKE ret_ctr_pago.consecutivo         ,
        mensualidades       LIKE ret_ctr_pago.mensualidades       ,
        folio_op12          LIKE ret_ctr_pago.folio_op12          ,
        num_mensualidad     LIKE ret_ctr_pago_det.num_mensualidad ,
        mto_mensualidad     LIKE ret_ctr_pago_det.mto_mensualidad ,
        num_resolucion      LIKE ret_parcial.num_resolucion
    END RECORD

    DEFINE lr_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_acc       DECIMAL(16,6) ,
        acc_pagar       DECIMAL(16,6) ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    DEFINE lr_montos RECORD
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv_tot   DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6)
    END RECORD

    DEFINE lr_marca RECORD #loc #lr_marca
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD

    DEFINE ld_pesos_liq LIKE ret_ctr_pago_det.mto_mensualidad

    DEFINE
        ls_resp             ,
        ls_cod_tramite      ,
        ls_inserta          ,
        v_marca_ent         ,
        ls_movimiento       ,
        f_siefore           SMALLINT

    DEFINE
        li_tot_regs         ,
        li_cont_regs        ,
        li_folio_op16       INTEGER

    -- -----------------------------------------------------------------------------

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    OPEN WINDOW retc8553 AT 4,4 WITH FORM "RETC8553" ATTRIBUTE(BORDER)
    DISPLAY "                                                                             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC855 CONSULTA Y LIQUIDACION DE PARCIALES - RETIROS A, B Y C              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET lr_marca.estado_marca = 0
    LET lr_marca.marca_causa  = 0
    LET li_cont_regs          = 0
    LET v_marca_ent           = 875
    LET li_tot_regs           = f_regs_por_liquidar(p_fecha_liq)
    LET li_folio_op16         = f_ultimo_folio()

    DISPLAY "FOLIO NUMERO               : ", li_folio_op16  AT 06,21
    DISPLAY "TOTAL REGISTROS A PROCESAR : ", li_tot_regs    AT 08,21
    DISPLAY "TOTAL REGISTROS PROCESADOS : ", li_cont_regs   AT 09,21
    DISPLAY "LIQUIDADOS RETIRO A        : ", gr_cont.tipo_a AT 11,21
    DISPLAY "LIQUIDADOS RETIRO B        : ", gr_cont.tipo_b AT 12,21
    DISPLAY "LIQUIDADOS COMPLEMENTARIOS : ", gr_cont.tipo_c AT 13,21

    -- Se valida sobre ret_parcial_tx que los registros ya hayan recibido
    -- la operacion 13 para poder ser liquidados
    DECLARE cur_reg CURSOR FOR
    SELECT A.nss                ,
           A.consecutivo        ,
           A.mensualidades      ,
           A.folio_op12         ,
           B.num_mensualidad    ,
           B.mto_mensualidad    ,
           D.num_resolucion
    FROM   ret_ctr_pago      A ,
           ret_ctr_pago_det  B ,
           ret_parcial_tx    C ,
           ret_parcial       D
    WHERE  A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo
    AND    A.nss             = C.nss
    AND    A.consecutivo     = C.consecutivo
    AND    A.nss             = D.nss
    AND    A.consecutivo     = D.consecutivo
    AND    B.fecha_liquida   = p_fecha_liq
    AND    D.tipo_desempleo   IN ("A","B","C")
    AND    D.diag_cuenta_ind = 400
    AND    A.estado          = gr_edo.provisionado
    AND    B.estado          = gr_edo.provisionado
    ORDER BY 1

    -- Ciclo sobre cada NSS a liquidar
    FOREACH cur_reg INTO lr_liquida.*

        LET li_cont_regs = li_cont_regs + 1
        LET lr_montos.acciones_ret97  = 0
        LET lr_montos.acciones_cs     = 0
        LET lr_montos.acciones_cv_tot = 0
        LET ld_pesos_liq              = 0

        SELECT A.tipo_movimiento
        INTO   ls_movimiento
        FROM   tab_tipo_desempleo A,
               ret_parcial        B
        WHERE  B.nss            = lr_liquida.nss
        AND    B.consecutivo    = lr_liquida.consecutivo
        AND    A.tipo_desempleo = B.tipo_desempleo

        DISPLAY "TOTAL REGISTROS PROCESADOS : ", li_cont_regs   AT 09,21

        CASE ls_movimiento
            WHEN 876
                LET gr_cont.tipo_a = gr_cont.tipo_a + 1
                DISPLAY "LIQUIDADOS RETIRO A        : ", gr_cont.tipo_a AT 11,21
            WHEN 877
                LET gr_cont.tipo_b = gr_cont.tipo_b + 1
                DISPLAY "LIQUIDADOS RETIRO B        : ", gr_cont.tipo_b AT 12,21
            WHEN 878
                LET gr_cont.tipo_c = gr_cont.tipo_c + 1
                DISPLAY "LIQUIDADOS COMPLEMENTARIOS : ", gr_cont.tipo_c AT 13,21
        END CASE

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_liq CURSOR FOR eje_parcial_sbc
        FOREACH cur_liq USING lr_liquida.nss             ,
                              lr_liquida.mto_mensualidad ,
                              p_fecha_liq
                        INTO  lr_pago.*

            LET f_siefore          = lr_pago.siefore
            LET gd_tot_monto_pesos = gd_tot_monto_pesos + lr_pago.pesos_pagar

            CALL f_actualiza_cta_ind(li_folio_op16                        ,
                                     lr_liquida.nss                       ,
                                     lr_pago.subcta                       ,
                                     lr_pago.siefore                      ,
                                     ls_movimiento                        ,
                                     p_fecha_liq                          ,
                                     p_fecha_liq                          ,
                                     p_fecha_liq                          ,
                                     gar_precio_acc[f_siefore].precio_dia ,
                                     -lr_pago.acc_pagar                   ,
                                     -lr_pago.pesos_pagar                 ,
                                     lr_liquida.consecutivo               ,
                                     gc_usuario
                                     )

            LET ld_pesos_liq = ld_pesos_liq + lr_pago.pesos_pagar

            CASE lr_pago.subcta
                WHEN 1
                    LET lr_montos.acciones_ret97  = lr_montos.acciones_ret97  + lr_pago.acc_pagar
                WHEN 5
                    LET lr_montos.acciones_cs     = lr_montos.acciones_cs     + lr_pago.acc_pagar
                WHEN 2
                    LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                WHEN 6
                    LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
                WHEN 9
                    LET lr_montos.acciones_cv_tot = lr_montos.acciones_cv_tot + lr_pago.acc_pagar
            END CASE

        END FOREACH -- Siguiente subcuenta

        EXECUTE eje_CONSAR_parcial USING lr_liquida.nss            ,
                                         lr_liquida.consecutivo    ,
                                         lr_liquida.num_resolucion ,
                                         li_folio_op16
                                   INTO  ls_inserta

        LET ls_resp     = 0

        EXECUTE eje_marca_pen USING lr_liquida.nss          ,
                                    li_folio_op16           ,
                                    lr_liquida.consecutivo  ,
                                    "I"                     ,
                                    ls_cod_tramite          ,
                                    HOY
                              INTO  ls_resp

        UPDATE ret_ctr_pago_det
        SET    folio_op16      = li_folio_op16    ,
               estado          = gr_edo.liquidado
        WHERE  nss             = lr_liquida.nss
        AND    consecutivo     = lr_liquida.consecutivo
        AND    estado          = gr_edo.provisionado
        AND    num_mensualidad = lr_liquida.num_mensualidad
        AND    fecha_liquida   = p_fecha_liq


        INSERT INTO ret_monto_siefore
        VALUES (lr_liquida.nss              ,--nss
                lr_liquida.consecutivo      ,--consecutivo
                li_folio_op16               ,--folio
                "I"                         ,--tipo_retiro
                16                          ,--tipo_operacion
                lr_pago.siefore             ,--siefore
                lr_montos.acciones_ret97    ,--acciones_ret97
                lr_montos.acciones_cv_tot   ,--acciones_cv
                lr_montos.acciones_cs       ,--acciones_cs
                0                            --acciones_ret92
               )

        -- Si se liquidaron ya todas las mensualidades se actualizan las tablas
        -- de la solicitud y de control de pagos
        IF lr_liquida.num_mensualidad = lr_liquida.mensualidades THEN

            UPDATE ret_ctr_pago
            SET    estado          = gr_edo.liquidado
            WHERE  nss             = lr_liquida.nss
            AND    consecutivo     = lr_liquida.consecutivo
            AND    estado          = gr_edo.provisionado
            AND    folio_op12      = lr_liquida.folio_op12
            AND    mensualidades   = lr_liquida.mensualidades

            UPDATE ret_parcial
            SET    estado_solicitud = gr_edo.liquidado
            WHERE  nss              = lr_liquida.nss
            AND    consecutivo      = lr_liquida.consecutivo
            AND    folio            = lr_liquida.folio_op12

            UPDATE ret_parcial_tx
            SET    fecha_valuacion  = HOY   ,
                   fecha_pago       = HOY   ,
                   pago_desempleo   = ld_pesos_liq
            WHERE  nss              = lr_liquida.nss
            AND    consecutivo      = lr_liquida.consecutivo

            EXECUTE eje_desmarca USING lr_liquida.nss           ,
                                       v_marca_ent              ,
                                       lr_liquida.consecutivo   ,
                                       lr_marca.estado_marca    ,
                                       lr_marca.marca_causa     ,
                                       gc_usuario
        END IF


    END FOREACH -- Siguiente NSS

    -- Marcamos la generacion de la operacion 16 especial para identificar los
    -- registros generados
    INSERT INTO ret_ctr_envio
        VALUES (li_folio_op16             ,-- folio
                "AV16AB"                  ,-- tipo_operacion
                "CZAAV16 DETAV16 SUMAV16" ,-- estructura
                gr_edo.procesado          ,-- status
                0                         ,-- orden_de_envio
                p_fecha_liq                -- fecha_envio
               )

    INSERT INTO ret_sum_envio
        VALUES(li_folio_op16      ,-- folio
               "16"               ,-- tipo_operacion
               NULL               ,-- diag_cuenta_ind
               "AV"               ,-- area_origen
               li_cont_regs       ,-- tot_registros
               gd_tot_monto_pesos ,-- impt_tot_oper
               gr_edo.procesado   ,-- status
               p_fecha_liq         -- fecha_envio
              )

    -- -------------------------------------------------------------------------

--    DISPLAY "TOTAL REGISTROS PROCESADOS : ",li_cont_regs        AT 09,21

    PROMPT  " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retc8553

END FUNCTION

#---------------------------------------------------------------------------#
# f_recalcula_montos : Recalcula los montos a pagar en base al precio de    #
#                      accion de la fecha dada                              #
#---------------------------------------------------------------------------#
FUNCTION f_recalcula_montos(p_fecha_liq)

    DEFINE
        p_fecha_liq     DATE

    DEFINE lr_regs_liq RECORD
        nss                 LIKE ret_parcial.nss                    ,
        consecutivo         LIKE ret_parcial.consecutivo            ,
        folio               LIKE ret_parcial.folio                  ,
        tipo_desempleo      LIKE ret_parcial.tipo_desempleo         ,
        tipo_pago           LIKE ret_parcial.tipo_pago              ,
        pago_des            LIKE ret_parcial.pago_desempleo         ,
        sal_base_cot        LIKE ret_parcial.salario_base_cot       ,
        num_mensualidad     LIKE ret_ctr_pago_det.num_mensualidad
    END RECORD

    DEFINE lr_montos RECORD
        mto_1       LIKE ret_ctr_pago.mto_1,
        mto_2       LIKE ret_ctr_pago.mto_2
    END RECORD

    DEFINE
        ld_rcv              DECIMAL(16,2)

    -- -----------------------------------------------------------------------------

    -- Se valida sobre ret_parcial_tx que los registros ya hayan recibido
    -- la operacion 13 para poder ser liquidados
    DECLARE cur_sct CURSOR FOR
    SELECT A.nss             ,
           A.consecutivo     ,
           A.folio_op12      ,
           D.tipo_desempleo  ,
           D.tipo_pago       ,
           D.pago_desempleo  ,
           D.salario_base_cot,
           B.num_mensualidad
    FROM   ret_ctr_pago      A ,
           ret_ctr_pago_det  B ,
           ret_parcial_tx    C ,
           ret_parcial       D
    WHERE  A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo
    AND    A.nss             = C.nss
    AND    A.consecutivo     = C.consecutivo
    AND    A.nss             = D.nss
    AND    A.consecutivo     = D.consecutivo
    AND    B.fecha_liquida   = p_fecha_liq
    AND    D.tipo_desempleo   IN ("A","B","C")
    AND    D.diag_cuenta_ind = 400
    AND    A.estado          = gr_edo.provisionado
    AND    B.estado          = gr_edo.provisionado
    ORDER BY 4,5,1

    FOREACH cur_sct INTO lr_regs_liq.*

        LET ld_rcv       = f_obten_rcv(lr_regs_liq.nss)

        IF (lr_regs_liq.tipo_desempleo = "A") OR
           (lr_regs_liq.tipo_desempleo = "B" AND lr_regs_liq.tipo_pago = 1) THEN

            -- Si el tipo es A o B-1 se verifica que no varie el monto
            -- en caso de que
            IF lr_regs_liq.pago_des > ld_rcv THEN

                UPDATE ret_ctr_pago
                SET    mto_pago     = ld_rcv
                WHERE  nss          = lr_regs_liq.nss
                AND    consecutivo  = lr_regs_liq.consecutivo
                AND    folio_op12   = lr_regs_liq.folio

                UPDATE ret_ctr_pago_det
                SET    mto_mensualidad  = ld_rcv
                WHERE  nss              = lr_regs_liq.nss
                AND    consecutivo      = lr_regs_liq.consecutivo
            END IF
        END IF

        IF (lr_regs_liq.tipo_desempleo = "B" AND lr_regs_liq.tipo_pago = 2) THEN

            LET lr_montos.mto_1 = lr_regs_liq.sal_base_cot * 90
            LET lr_montos.mto_2 = ld_rcv * 0.115

            IF lr_montos.mto_1 < lr_montos.mto_2 THEN

                UPDATE ret_ctr_pago
                SET    mto_1        = lr_montos.mto_1 ,
                       mto_2        = lr_montos.mto_2 ,
                       mto_pago     = lr_montos.mto_1
                WHERE  nss          = lr_regs_liq.nss
                AND    consecutivo  = lr_regs_liq.consecutivo
                AND    folio_op12   = lr_regs_liq.folio

                UPDATE ret_ctr_pago_det
                SET    mto_mensualidad  = lr_montos.mto_1
                WHERE  nss              = lr_regs_liq.nss
                AND    consecutivo      = lr_regs_liq.consecutivo
            ELSE
                UPDATE ret_ctr_pago
                SET    mto_1        = lr_montos.mto_1 ,
                       mto_2        = lr_montos.mto_2 ,
                       mto_pago     = lr_montos.mto_2
                WHERE  nss          = lr_regs_liq.nss
                AND    consecutivo  = lr_regs_liq.consecutivo
                AND    folio_op12   = lr_regs_liq.folio

                UPDATE ret_ctr_pago_det
                SET    mto_mensualidad  = lr_montos.mto_2
                WHERE  nss              = lr_regs_liq.nss
                AND    consecutivo      = lr_regs_liq.consecutivo
            END IF
        END IF
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_ejecucion : Realiza las validaciones previas a la ejecucion      #
#                      de la liquidacion                                    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_ejecucion(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio        DATE,
        fin           DATE
    END RECORD

    DEFINE
        ls_exit        SMALLINT

    DEFINE
        li_regs        INTEGER

    -- -----------------------------------------------------------------------------

    LET li_regs = 0
    LET ls_exit = 0

    -- Validamos y obtenemos precios de accion
    CALL f_obtiene_precios_accion(HOY)

    -- Validamos que existan registros
    SELECT COUNT(UNIQUE B.folio_op12)
    INTO   li_regs
    FROM   ret_ctr_pago_det A ,
           ret_ctr_pago     B
    WHERE  A.nss         = B.nss
    AND    A.consecutivo = B.consecutivo
    AND    A.fecha_liquida BETWEEN pr_fechas.inicio AND pr_fechas.fin

    IF li_regs = 0 THEN
        PROMPT " NO EXISTEN REGISTROS EN EL INTERVALO CAPTURADO ...<ENTER> PARA SALIR "
        FOR CHAR enter
        LET ls_exit = 1
    END IF

    RETURN ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(p_fecha)

    DEFINE
        p_fecha     DATE

    DEFINE
        estado      SMALLINT

    DEFINE
        mensaje     CHAR(100)

    LET estado = 0

    IF p_fecha IS NULL THEN
        LET mensaje = "LA FECHA NO DEBE SER NULA ... "
        LET estado = 1
    ELSE
        IF p_fecha < "01/01/1980" THEN
            LET mensaje = "FECHA INVALIDA ... "
            LET estado = 1
        END IF
    END IF

    RETURN estado, mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_cta_ind : Inserta los movimientos de liquidacion en la cuenta #
#                       individual                                          #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_cta_ind(pr_liquida)

    DEFINE pr_liquida RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        siefore               LIKE dis_cuenta.siefore           ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           LIKE dis_cuenta.consecutivo_lote  ,
        usuario               LIKE dis_cuenta.usuario
    END RECORD

    INSERT INTO safre_af:dis_cuenta
        VALUES(pr_liquida.tipo_movimiento   ,
               pr_liquida.subcuenta         ,
               pr_liquida.siefore           ,
               pr_liquida.folio             ,
               pr_liquida.consecutivo       ,
               pr_liquida.n_seguro          ,
               ""                           , -- curp
               ""                           , -- folio_sua
               pr_liquida.fecha_pago        , -- fecha_pago
               pr_liquida.fecha_valor       , -- fecha_valor
               pr_liquida.fecha_conversion  , -- fecha_conversion
               pr_liquida.monto_en_pesos    , -- monto_en_pesos
               pr_liquida.monto_en_acciones , -- monto_en_acciones
               pr_liquida.precio_accion     , -- precio_accion
               0                            , -- dias_cotizados
               ""                           , -- sucursal
               gc_id_aportante              , -- id_aportante
               gr_edo.liquidado             , -- status
               HOY                          , -- fecha_proceso
               pr_liquida.usuario           , -- usuario
               HOY                          , -- fecha_archivo
               0                              -- etiqueta
              )
END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el folio actual que se usara en la liquidacion   #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER

    -- -----------------------------------------------------------------------------

    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion correspondientes #
#                            a la fecha indicada                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        li_cont               SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont = 0

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", fecha_precios, ", SIEFORE: ", lc_siefore, " ... <ENTER> PARA SALIR " CLIPPED
            LET lc_mensaje = lc_mensaje CLIPPED
            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_rcv : Obtiene los montos correspondientes a las subcuentas de rcv #
#               del nss indicado                                            #
#---------------------------------------------------------------------------#
FUNCTION f_obten_rcv(p_nss)

    DEFINE p_nss        LIKE ret_parcial.nss

    DEFINE lr_rcv RECORD
        ret_97          DECIMAL(16,2),
        ces_vej         DECIMAL(16,2),
        cuota_soc       DECIMAL(16,2),
        tot_sub_rcv     DECIMAL(16,2)
    END RECORD

    DEFINE
        ls_sie_act      LIKE cta_nss_regimen.codigo_siefore

    -- Obtiene la siefore actual del trabajador
    SELECT codigo_siefore
    INTO   ls_sie_act
    FROM   cta_nss_regimen
    WHERE  grupo_regimen = 1
    AND    nss           = p_nss

    -- SE OBTIENEN LOS MONTOS QUE SE INSERTARAN EN LA TABLA
    SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie_act].precio_dia
    INTO   lr_rcv.ret_97
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    siefore          = ls_sie_act
    AND    subcuenta        = 1
    AND    fecha_conversion <= HOY
    AND    tipo_movimiento  > 0

    IF lr_rcv.ret_97 IS NULL THEN
       LET lr_rcv.ret_97 = 0
    END IF

    SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie_act].precio_dia
    INTO   lr_rcv.ces_vej
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    siefore          = ls_sie_act
    AND    subcuenta        IN (2,6,9)
    AND    fecha_conversion <= HOY
    AND    tipo_movimiento  > 0

    IF lr_rcv.ces_vej IS NULL THEN
       LET lr_rcv.ces_vej = 0
    END IF

    SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie_act].precio_dia
    INTO   lr_rcv.cuota_soc
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    siefore          = ls_sie_act
    AND    subcuenta        = 5
    AND    fecha_conversion <= HOY
    AND    tipo_movimiento  > 0

    IF lr_rcv.cuota_soc IS NULL THEN
       LET lr_rcv.cuota_soc = 0
    END IF

    LET lr_rcv.tot_sub_rcv = lr_rcv.ret_97 + lr_rcv.ces_vej + lr_rcv.cuota_soc

--    RETURN lr_rcv.*

    RETURN lr_rcv.tot_sub_rcv

END FUNCTION

