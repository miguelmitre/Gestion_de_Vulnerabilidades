################################################################################
#Owner             => E.F.P.                                                   #
#Programa PENC105  => PRELIQUIDACION DE RETIROS PROGRAMADOS -PENSION MINIMA    #
#                     GARANTIZADA (RETIRO S)                                   #
#Fecha creacion    => 6 DE ABRIL DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 15 DE FEBRERO DE 2011                                    #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se modifica para ajustarse a los cambios realizados en el#
#                     proceso de acuerdo a lo realizado para la generacion de  #
#                     la operación 78 y carga de la 79                         #
#Actualizacion     => v1.2 ISAI JIMENEZ ROJAS                                  #
#                  => Liquidar el SALDO en caso de ser el ultimo pago          #
#Actualizacion     => v1.3 ISAI JIMENEZ ROJAS                                  #
#                  => Descarta pago de vienda en caso de diagnostico 440 y 401 #
#Sistema           => PEN                                                      #
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc     ARRAY [20] OF RECORD #Arreglo para los precios_accion
           estado             SMALLINT     ,
           fecha              DATE         ,
           siefore            SMALLINT     ,
           precio_dia         DECIMAL(16,6)
           END RECORD

    DEFINE gr_edo             RECORD
           capturado          LIKE pen_estado_pmg.estado_solicitud    ,
           recibido           LIKE pen_estado_pmg.estado_solicitud    ,
           en_pago            LIKE pen_estado_pmg.estado_solicitud    ,
           preliquidado       LIKE pen_estado_pmg.estado_solicitud    ,
           liquidado          LIKE pen_estado_pmg.estado_solicitud    ,
           rechazado          LIKE pen_estado_pmg.estado_solicitud
           END RECORD

    DEFINE gr_noti            RECORD
           agotamiento        LIKE tab_tipo_notifica_pmg.clave        ,
           conclusion         LIKE tab_tipo_notifica_pmg.clave        
           END RECORD
    
    DEFINE enter              CHAR(001)
    DEFINE gc_usuario         CHAR(020)

    DEFINE gs_procesa         SMALLINT
    DEFINE gs_cod_afore       SMALLINT
    DEFINE gc_mensaje         CHAR(80)

    DEFINE HOY                DATE

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init()
    
    CALL STARTLOG(gc_usuario CLIPPED||".PENC105.log")

    CALL f_busca_regs_pago() RETURNING gs_procesa

    IF gs_procesa THEN
        CALL f_preliquida_registros()
        CALL f_actualiza_tablas()
    ELSE
        CALL f_error_msg("PROCESO CANCELADO")
    END IF

    CLOSE WINDOW penc1051

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


    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.en_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

    ----- TIPO DE NOTIFICACION -----
    SELECT clave
    INTO   gr_noti.agotamiento
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*AGOTAMIENTO*'
    
    SELECT clave
    INTO   gr_noti.conclusion
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*CONCLUSION*'

    ----- CALCULA PROPORCIONAL PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_calcula_prop_pmg(?,?,?,?) "
    PREPARE eje_prop_pmg FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


#---------------------------------------------------------------------------#
# f_busca_regs_pago : Realiza la busqueda de los registros correspondientes #
#                     al periodo indicado por el usuario                    #
#---------------------------------------------------------------------------#
FUNCTION f_busca_regs_pago()

    DEFINE lr_rango_fec RECORD
        inicial         DATE,
        final           DATE
    END RECORD

    DEFINE
        ls_salida          SMALLINT


    CALL f_captura_fechas() RETURNING lr_rango_fec.*, ls_salida

    IF ls_salida = 1 THEN
        CALL f_despliega_datos(lr_rango_fec.*, HOY) RETURNING ls_salida
    END IF

    RETURN ls_salida

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

    OPEN WINDOW penc1051 AT 2,2 WITH FORM "PENC1051" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                            <CTRL-C> - Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC105        PRELIQUIDACION MENSUAL DE REGISTROS DE PMG                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET ls_exit           = 1

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
            LET ls_exit = 0
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

    RETURN lr_fechas.*, ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_pantalla : Despliega la pantalla de consulta de registros     #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_pantalla()

    OPEN WINDOW penc1052 AT 2,2 WITH FORM "PENC1052" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-P> - Liquidar cuentas                              <CTRL-C> - Regresar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC105          PRELIQUIDACION MENSUAL DE REGISTROS DE PMG                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           DATOS DE CONTROL INTERNO                           " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_datos : Realiza la consulta general y muestra los registros   #
#                     que cumplen con los parametros indicados              #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_datos(pdt_fechas, pdt_fecha_proceso)

    DEFINE pdt_fechas         RECORD
           inicio             DATE,
           fin                DATE
           END RECORD

    DEFINE pdt_fecha_proceso  DATE

    DEFINE lr_datos_gen       RECORD
           fecha_rcv          DATE            ,
           fecha_viv          DATE            ,
           precio_sie1        DECIMAL(10,6)   ,
           precio_sie11       DECIMAL(10,6)
           END RECORD

    DEFINE lr_datos_mto       RECORD
           sec_contrato       LIKE pen_solicitud_pmg.sec_contrato         ,
           nss                LIKE pen_ctr_pago_det.nss                   ,
           folio_lote         LIKE pen_solicitud_pmg.folio_lote           ,
           fecha_pago_est     LIKE pen_ctr_pago_det.fecha_pago_estimada   ,
           num_mens           LIKE pen_ctr_pago_det.num_mensualidad       ,
           id_viv             LIKE pen_solicitud_pmg.estado_sub_viv       ,
           total_pesos        LIKE pen_ctr_pago_det.mto_pago_pesos        ,
           diag_reg           LIKE pen_solicitud_pmg.diag_registro        ,
           marca_ult_pago     LIKE pen_ctr_pago_det.marca_ult_pago
           END RECORD

    DEFINE lar_det_montos     ARRAY[100] OF RECORD
           sec_contrato       LIKE pen_solicitud_pmg.sec_contrato         ,
           nss                LIKE pen_ctr_pago_det.nss                   ,
           folio_lote         LIKE pen_solicitud_pmg.folio_lote           ,
           fecha_pago_est     LIKE pen_ctr_pago_det.fecha_pago_estimada   ,
           num_mens           LIKE pen_ctr_pago_det.num_mensualidad       ,
           id_viv             LIKE pen_solicitud_pmg.estado_sub_viv       ,
           total_pesos        LIKE pen_ctr_pago_det.mto_pago_pesos        ,
           diag_reg           LIKE pen_solicitud_pmg.diag_registro        ,
           marca_ult_pago     LIKE pen_ctr_pago_det.marca_ult_pago
           END RECORD


    DEFINE lr_pago_prop       RECORD
           subcuenta          SMALLINT        ,
           siefore            SMALLINT        ,
           monto_acciones     DECIMAL(16,6)   ,
           acciones_pagar     DECIMAL(16,6)   ,
           pesos_pagar        DECIMAL(16,6)
           END RECORD

    DEFINE lr_totales         RECORD
           tot_acc_viv        DECIMAL(16,6),
           tot_pes_viv        DECIMAL(16,6),
           tot_acc_rcv        DECIMAL(16,6),
           tot_pes_rcv        DECIMAL(16,6)
           END RECORD

    DEFINE li_consec          LIKE pen_solicitud_pmg.consecutivo

    DEFINE ls_opera           SMALLINT

    DEFINE li_tot_elem        INTEGER
    DEFINE li_arr_elem        INTEGER
    DEFINE li_scr_elem        INTEGER
    DEFINE li_cnt             INTEGER

    DEFINE lc_fecha_viv       CHAR(10)

    #-- --------------------------------------------------------------------------------
    CALL f_despliega_pantalla()
    CALL f_tablas_tmp()

    FOR  li_cnt = 1 TO 100
        INITIALIZE lar_det_montos[li_cnt].* TO NULL
    END FOR

    LET ls_opera    = 1
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
                B.folio_lote            ,
                A.fecha_pago_estimada   ,
                A.num_mensualidad       ,
                B.estado_sub_viv        ,
                A.mto_pago_pesos        ,
                B.diag_registro         ,
                A.marca_ult_pago        ,
                B.consecutivo
        FROM    pen_ctr_pago_det  A     ,
                pen_solicitud_pmg B
        WHERE  A.nss            = B.nss
        AND    A.consecutivo    = B.consecutivo
        AND    A.estado         = gr_edo.recibido
        AND    A.fecha_pago_estimada BETWEEN pdt_fechas.inicio AND pdt_fechas.fin
        AND    B.estado_solicitud IN (gr_edo.recibido , -- Para el 1er pago
                                      gr_edo.en_pago  ) -- Para pagos siguientes
        ORDER BY 5,2

    FOREACH cur_nss INTO lr_datos_mto.*,
                         li_consec

        LET lar_det_montos[li_cnt].* = lr_datos_mto.*

        INSERT INTO tmp_por_liquidar
        VALUES(li_consec, lr_datos_mto.*)

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

    IF (li_cnt - 1 = 0) THEN
        LET ls_opera = 0
        CALL f_error_msg("NO EXISTEN REGISTROS EN EL PERIODO CAPTURADO")
    ELSE
        CALL SET_COUNT(li_cnt-1)
        DISPLAY ARRAY lar_det_montos TO scr_montos.*
        
            ON KEY (CONTROL-P)
                WHILE TRUE
                    PROMPT "¿ DESEA EJECUTAR LA PRELIQUIDACION DE PMG ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_opera = 1
                        ELSE
                            LET ls_opera = 0
                        END IF
                        EXIT DISPLAY
                    END IF
                END WHILE
            
            ON KEY (CONTROL-C, INTERRUPT)
                LET ls_opera = 0
                EXIT DISPLAY
        
        END DISPLAY
    END IF

    CLOSE WINDOW penc1052

    RETURN ls_opera

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida_registros : Realiza la preliquidacion de los nss que entran  #
#                          en el rango de fechas capturado por el usuario   #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida_registros()

    DEFINE lr_pagos           RECORD
           consecutivo        LIKE pen_solicitud_pmg.consecutivo          ,
           sec_contrato       LIKE pen_solicitud_pmg.sec_contrato         ,
           nss                LIKE pen_ctr_pago_det.nss                   ,
           folio_lote         LIKE pen_solicitud_pmg.folio_lote           ,
           fecha_pago_est     LIKE pen_ctr_pago_det.fecha_pago_estimada   ,
           num_mens           LIKE pen_ctr_pago_det.num_mensualidad       ,
           id_viv             LIKE pen_solicitud_pmg.estado_sub_viv       ,
           total_pesos        LIKE pen_ctr_pago_det.mto_pago_pesos        ,
           diag_registro      LIKE pen_solicitud_pmg.diag_registro        ,
           marca_ult_pago     LIKE pen_ctr_pago_det.marca_ult_pago
           END RECORD

    DEFINE lr_detalle_pago    RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida      RECORD LIKE pen_preliquida_pmg.*

    DEFINE ls_flag            SMALLINT
    DEFINE ls_sie             SMALLINT

    -------------------------------------------------------------------------

    -- Inicializamos los valores que son generales para la preliquidacion
    LET lr_preliquida.folio             = -1
    LET lr_preliquida.folio_sua         = NULL
    LET lr_preliquida.fecha_pago        = HOY
    LET lr_preliquida.fecha_conversion  = HOY
    LET lr_preliquida.dias_cotizados    = 0
    LET lr_preliquida.sucursal          = " "
    LET lr_preliquida.id_aportante      = "RETIRO" #-- PENDIENTE ****
    LET lr_preliquida.estado            = 6
    LET lr_preliquida.fecha_proceso     = HOY
    LET lr_preliquida.usuario           = gc_usuario
    LET lr_preliquida.fecha_archivo     = HOY
    LET lr_preliquida.etiqueta          = 1
    LET lr_preliquida.estado_pmg        = gr_edo.preliquidado

    -------------------------------------------------------------------------
    
    SELECT movimiento
    INTO   lr_preliquida.tipo_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = "S"

    ---------------------------------------------------
    --SELECCIONA LOS REGISTROS IDENTIFICADOS PARA PAGO
    ---------------------------------------------------
    DECLARE cur_uno CURSOR FOR
    SELECT *
    FROM   tmp_por_liquidar
    ORDER BY nss

    FOREACH cur_uno INTO lr_pagos.*

        -- Verificamos si el monto a pagar no sobregira la cuenta
        CALL f_verifica_sobregiro(lr_pagos.nss          ,
                                  lr_pagos.consecutivo  ,
                                  lr_pagos.id_viv       ,
                                  lr_pagos.total_pesos  )
            RETURNING ls_flag

        IF ls_flag = 0 AND lr_pagos.marca_ult_pago IS NULL THEN
            
            --DETERMINA MONTO A PAGAR CON BASE A LO CALCULADO EN PEN_DETALLE_SOL
            
            -- Inicializamos los valores que son propios de cada nss
            LET lr_preliquida.nss               = lr_pagos.nss
            LET lr_preliquida.consecutivo_lote  = lr_pagos.consecutivo
            LET lr_preliquida.mensualidad       = lr_pagos.num_mens
            
            --SELECT curp                                   --??Para qué?
            --INTO   lr_preliquida.curp                     --??Para qué?
            --FROM   pen_solicitud_pmg                      --??Para qué?
            --WHERE  nss          = lr_pagos.nss            --??Para qué?
            --AND    consecutivo  = lr_pagos.consecutivo    --??Para qué?
            --AND    folio_lote   = lr_pagos.folio_lote     --??Para qué?
            
            INITIALIZE lr_detalle_pago.* TO NULL

            DECLARE cur_det CURSOR FOR
            SELECT *
            FROM   pen_detalle_sol
            WHERE  nss             = lr_pagos.nss
            AND    consecutivo     = lr_pagos.consecutivo
            AND    num_mensualidad = lr_pagos.num_mens
            ORDER BY siefore, subcuenta
            
            FOREACH cur_det INTO lr_detalle_pago.*
            
                -- Insertamos la tabla pen_detalle_sol en su tabla temporal correspondiente
                INSERT INTO tmp_detalle_sol
                VALUES (lr_detalle_pago.*)
            
                LET ls_sie                          = lr_detalle_pago.siefore
                LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
                LET lr_preliquida.subcuenta         = lr_detalle_pago.subcuenta
                LET lr_preliquida.siefore           = lr_detalle_pago.siefore
                LET lr_preliquida.monto_en_pesos    = lr_detalle_pago.monto_en_pesos * -1
            
                IF lr_detalle_pago.subcuenta = 4 THEN
                    LET lr_preliquida.monto_en_acciones = lr_detalle_pago.monto_en_acciones * -1
                    LET lr_preliquida.fecha_valor       = lr_detalle_pago.fecha_valuacion
                ELSE
                    LET lr_preliquida.monto_en_acciones = (lr_detalle_pago.monto_en_pesos/lr_preliquida.precio_accion) * -1
                    LET lr_preliquida.fecha_valor       = HOY
            
                    UPDATE tmp_detalle_sol
                    SET    fecha_valuacion      = lr_preliquida.fecha_valor      ,
                           monto_en_acciones    = lr_preliquida.monto_en_acciones
                    WHERE  nss                  = lr_pagos.nss
                    AND    consecutivo          = lr_pagos.consecutivo
                    AND    subcuenta            = lr_detalle_pago.subcuenta
                    AND    num_mensualidad      = lr_pagos.num_mens
                END IF
            
                IF lr_detalle_pago.subcuenta = 4 AND 
                  (lr_pagos.diag_registro = 440 OR lr_pagos.diag_registro = 401) THEN
                   --NO PRELIQUIDA VIVIENDA
                   LET gc_mensaje = "NO SE PRELIQUIDO VIVIENDA NSS:",lr_pagos.nss, 
                                    " MENSUALIDAD:",lr_pagos.num_mens
                   CALL ERRORLOG(gc_mensaje)
                ELSE
                   --INSERTA MOVIMIENTO DE PRELIQUIDACION
                   INSERT INTO tmp_preliquida_pmg
                   VALUES (lr_preliquida.*)
                END IF
            
                INITIALIZE lr_detalle_pago.* TO NULL
            
            END FOREACH -- Detalles de pago por subcuenta
        ELSE
            --REGISTRA MOVIMIENTOS DE PRELIQUIDACION CON BASE AL SALDO
            CALL f_agota_saldo(lr_pagos.nss         ,
                               lr_pagos.consecutivo ,
                               lr_pagos.num_mens    )
            
            -- Se detecto un sobregiro, se manda a op. 70 como agotamiento de recursos
            CALL f_agrega_det_op70(lr_pagos.nss          ,
                                   lr_pagos.consecutivo  ,
                                   gr_noti.agotamiento   )
        END IF


    END FOREACH -- NSS con pago 1

END FUNCTION
#==============================================================================#
# f_agota_saldo : inserta movimientos de liquidacion con base al saldo que     #
#                 tenga el trabajador en la subcuenta                          #
# Autor : ISAI JIMENEZ ROJAS                                                   #
#==============================================================================#
FUNCTION f_agota_saldo(p_nss, p_consecutivo, p_num_mensualidad)
   
    DEFINE p_nss              LIKE pen_detalle_sol.nss
    DEFINE p_consecutivo      LIKE pen_detalle_sol.consecutivo
    DEFINE p_num_mensualidad  LIKE pen_detalle_sol.num_mensualidad

    DEFINE lr_detalle_pago    RECORD LIKE pen_detalle_sol.*
    DEFINE lr_preliquida      RECORD LIKE pen_preliquida_pmg.*
    DEFINE lr_saldo           RECORD
           subcuenta          SMALLINT        ,
           siefore            SMALLINT        ,
           monto_acc          DECIMAL(16,6)   ,
           monto_pesos        DECIMAL(16,6)
           END RECORD

    DEFINE ls_subcta          SMALLINT
    DEFINE ls_sie             SMALLINT 
    DEFINE ls_gr_saldo        SMALLINT
    DEFINE ls_grupo           SMALLINT
    DEFINE lc_diag_registro   CHAR(3)
    
    ---------------------------------------------------------------------------
    INITIALIZE lr_preliquida.* TO NULL
    
    
    -- Inicializamos los valores que son propios de cada nss
    LET lr_preliquida.nss               = p_nss
    LET lr_preliquida.consecutivo_lote  = p_consecutivo
    LET lr_preliquida.mensualidad       = p_num_mensualidad
            
    LET lr_preliquida.folio             = -1
    LET lr_preliquida.folio_sua         = NULL
    LET lr_preliquida.fecha_pago        = HOY
    LET lr_preliquida.fecha_conversion  = HOY
    LET lr_preliquida.dias_cotizados    = 0
    LET lr_preliquida.sucursal          = " "
    LET lr_preliquida.id_aportante      = "RETIRO" #-- PENDIENTE ****
    LET lr_preliquida.estado            = 6
    LET lr_preliquida.fecha_proceso     = HOY
    LET lr_preliquida.usuario           = gc_usuario
    LET lr_preliquida.fecha_archivo     = HOY
    LET lr_preliquida.etiqueta          = 1
    LET lr_preliquida.estado_pmg        = gr_edo.preliquidado

    --RECUPERA EL TIPO DE MOVIMIENTO QUE DEBE APLICARSE 
    SELECT movimiento
    INTO   lr_preliquida.tipo_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = "S"

    --OBTIENE EL GRUPO ASOCIADO CON LA SOLICITUD
    SELECT grupo, diag_registro
    INTO   ls_grupo, lc_diag_registro
    FROM   pen_solicitud_pmg
    WHERE  nss          = p_nss        
    AND    consecutivo  = p_consecutivo
    
    --SELECCIONA SUBCUENTAS ASOCIADAS AL GRUPO DE LA SOLICITUD
    DECLARE cur_subcta CURSOR FOR
    SELECT subcuenta
    FROM   tab_agrupa_subcta
    WHERE  grupo = ls_grupo
    ORDER BY 1
    
    --DE CADA SUBCUENTA RECUPERA EL SALDO
    FOREACH cur_subcta INTO ls_subcta
        
        DECLARE cur_agota_saldo CURSOR FOR eje_saldo_dia

        FOREACH cur_agota_saldo USING p_nss ,
                                      ls_subcta        ,
                                      ls_gr_saldo      ,
                                      HOY               
                                INTO  lr_saldo.*

             SELECT *
             INTO   lr_detalle_pago.*
             FROM   pen_detalle_sol
             WHERE  nss             = p_nss
             AND    consecutivo     = p_consecutivo
             AND    num_mensualidad = p_num_mensualidad
             AND    subcuenta       = ls_subcta

             --inserta el registro identificado a actualizar
                -- Insertamos la tabla pen_detalle_sol en su tabla temporal correspondiente
                INSERT INTO tmp_detalle_sol
                VALUES (lr_detalle_pago.*)
            
                LET ls_sie                          = lr_saldo.siefore
                LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
                LET lr_preliquida.subcuenta         = lr_saldo.subcuenta
                LET lr_preliquida.siefore           = lr_saldo.siefore
                LET lr_preliquida.monto_en_pesos    = lr_saldo.monto_pesos * -1
            
                IF lr_saldo.subcuenta = 4 THEN
                    LET lr_preliquida.monto_en_acciones = lr_saldo.monto_acc * -1
                    LET lr_preliquida.fecha_valor       = lr_detalle_pago.fecha_valuacion
                ELSE
                    LET lr_preliquida.monto_en_acciones = lr_saldo.monto_acc * -1
                    LET lr_preliquida.fecha_valor       = HOY
            
                    UPDATE tmp_detalle_sol
                    SET    fecha_valuacion      = lr_preliquida.fecha_valor      ,
                           monto_en_acciones    = lr_preliquida.monto_en_acciones
                    WHERE  nss                  = p_nss
                    AND    consecutivo          = p_consecutivo
                    AND    subcuenta            = ls_subcta
                    AND    num_mensualidad      = p_num_mensualidad
                END IF
            

                IF lr_saldo.subcuenta = 4 AND 
                  (lc_diag_registro = 440 OR lc_diag_registro = 401) THEN
                   --NO PRELIQUIDA VIVIENDA
                   LET gc_mensaje = "NO SE PRELIQUIDO VIVIENDA(2) NSS:",p_nss, 
                                    " MENSUALIDAD:",p_num_mensualidad
                   CALL ERRORLOG(gc_mensaje)
                ELSE
                   INSERT INTO tmp_preliquida_pmg
                   VALUES (lr_preliquida.*)
                END IF
            
                INITIALIZE lr_detalle_pago.* TO NULL


        END FOREACH
        
    END FOREACH 
            
            --DETERMINA MONTO A PAGAR CON BASE A LO CALCULADO EN PEN_DETALLE_SOL
 {           

            
            SELECT curp
            INTO   lr_preliquida.curp
            FROM   pen_solicitud_pmg
            WHERE  nss          = lr_pagos.nss
            AND    consecutivo  = lr_pagos.consecutivo
            AND    folio_lote   = lr_pagos.folio_lote
            
            INITIALIZE lr_detalle_pago.* TO NULL

            DECLARE cur_det CURSOR FOR
             SELECT *
             FROM   pen_detalle_sol
             WHERE  nss             = lr_pagos.nss
             AND    consecutivo     = lr_pagos.consecutivo
             AND    num_mensualidad = lr_pagos.num_mens
             ORDER BY siefore, subcuenta
            
            FOREACH cur_det INTO lr_detalle_pago.*
            
                -- Insertamos la tabla pen_detalle_sol en su tabla temporal correspondiente
                INSERT INTO tmp_detalle_sol
                VALUES (lr_detalle_pago.*)
            
                LET ls_sie                          = lr_detalle_pago.siefore
                LET lr_preliquida.precio_accion     = gar_precio_acc[ls_sie].precio_dia
            
                LET lr_preliquida.subcuenta         = lr_detalle_pago.subcuenta
                LET lr_preliquida.siefore           = lr_detalle_pago.siefore
                LET lr_preliquida.monto_en_pesos    = lr_detalle_pago.monto_en_pesos * -1
            
                IF lr_detalle_pago.subcuenta = 4 THEN
                    LET lr_preliquida.monto_en_acciones = lr_detalle_pago.monto_en_acciones * -1
                    LET lr_preliquida.fecha_valor       = lr_detalle_pago.fecha_valuacion
                ELSE
                    LET lr_preliquida.monto_en_acciones = (lr_detalle_pago.monto_en_pesos/lr_preliquida.precio_accion) * -1
                    LET lr_preliquida.fecha_valor       = HOY
            
                    UPDATE tmp_detalle_sol
                    SET    fecha_valuacion      = lr_preliquida.fecha_valor      ,
                           monto_en_acciones    = lr_preliquida.monto_en_acciones
                    WHERE  nss                  = lr_pagos.nss
                    AND    consecutivo          = lr_pagos.consecutivo
                    AND    subcuenta            = lr_detalle_pago.subcuenta
                    AND    num_mensualidad      = lr_pagos.num_mens
                END IF
            
                INSERT INTO tmp_preliquida_pmg
                VALUES (lr_preliquida.*)
            
                INITIALIZE lr_detalle_pago.* TO NULL
            
            END FOREACH -- Detalles de pago por subcuenta
 }           
END FUNCTION


#---------------------------------------------------------------------------#
# f_actualiza_tablas : Se inserta en las tablas fisicas los registros       #
#                      almacenados en las tablas temporales y se actualizan #
#                      el resto de las tablas involucradas                  #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tablas()

    DEFINE lar_cont ARRAY[13] OF INTEGER

    DEFINE lr_tmp_detalle  RECORD LIKE pen_detalle_sol.*

    DEFINE lr_preliquida RECORD
        nss             LIKE pen_preliquida_pmg.nss                 ,
        consecutivo     LIKE pen_preliquida_pmg.consecutivo_lote    ,
        mensualidad     LIKE pen_ctr_pago_det.num_mensualidad
    END RECORD

    DEFINE
        li_folio            ,
        li_cont_tot         INTEGER

    DEFINE
        ls_mes              ,
        ls_edo_pago         SMALLINT

    #-- --------------------------------------------------------------------

    LET li_folio    = f_obtiene_folio()
    LET li_cont_tot = 0

    FOR ls_mes = 1 TO 13
        LET lar_cont[ls_mes] = 0
    END FOR

    UPDATE tmp_preliquida_pmg
    SET    folio    = li_folio
    WHERE  folio    = -1

    DECLARE cur_tmp_det CURSOR FOR
      SELECT *
      FROM   tmp_detalle_sol
      WHERE  siefore <> 11
      ORDER BY nss, siefore, subcuenta

    FOREACH cur_tmp_det INTO lr_tmp_detalle.*

        UPDATE pen_detalle_sol
        SET    monto_en_acciones    = lr_tmp_detalle.monto_en_acciones  ,
               fecha_valuacion      = lr_tmp_detalle.fecha_valuacion
        WHERE  nss                  = lr_tmp_detalle.nss
        AND    consecutivo          = lr_tmp_detalle.consecutivo
        AND    folio_lote           = lr_tmp_detalle.folio_lote
        AND    siefore              <> 11
        AND    subcuenta            = lr_tmp_detalle.subcuenta

    END FOREACH -- Detalle sol

    #-- --------------------------------------------------------------------
    
    DECLARE cur_preliq CURSOR FOR
     SELECT UNIQUE(nss)         ,
            consecutivo_lote    ,
            mensualidad
     FROM   tmp_preliquida_pmg
     WHERE  folio = li_folio

    FOREACH cur_preliq INTO lr_preliquida.*

        LET ls_mes              = lr_preliquida.mensualidad
        LET li_cont_tot         = li_cont_tot + 1
        
        IF ls_mes <= 12 THEN
            LET lar_cont[ls_mes]    = lar_cont[ls_mes] + 1
        ELSE
            LET lar_cont[13]        = lar_cont[13] + 1
        END IF
        
        IF lr_preliquida.mensualidad = 12 THEN
--            LET ls_edo_pago = gr_edo.liquidado
        ELSE
            LET ls_edo_pago = gr_edo.en_pago
        END IF

--        IF lr_preliquida.mensualidad = 1 OR lr_preliquida.mensualidad = 12 THEN
        
        IF lr_preliquida.mensualidad = 1 THEN
            UPDATE pen_ctr_pago
            SET    estado       = ls_edo_pago
            WHERE  nss          = lr_preliquida.nss
            AND    consecutivo  = lr_preliquida.consecutivo

            UPDATE pen_solicitud_pmg
            SET    fecha_pago       = HOY           ,
                   estado_solicitud = ls_edo_pago
            WHERE  nss              = lr_preliquida.nss
            AND    consecutivo      = lr_preliquida.consecutivo
            AND    estado_solicitud <> gr_edo.rechazado
        END IF

        UPDATE pen_ctr_pago_det
        SET    fecha_liquida    = HOY               ,
               folio_liquida    = li_folio          ,
               estado           = gr_edo.liquidado
        WHERE  nss              = lr_preliquida.nss
        AND    consecutivo      = lr_preliquida.consecutivo
        AND    num_mensualidad  = lr_preliquida.mensualidad
        AND    estado           = gr_edo.recibido

        INSERT INTO pen_preliquida_pmg
        SELECT *
        FROM   tmp_preliquida_pmg
        WHERE  nss              = lr_preliquida.nss
        AND    consecutivo_lote = lr_preliquida.consecutivo
        AND    folio            = li_folio


{
        -- Si es la ultima mensualidad pagada se desmarca la cuenta y se avisa en una 
        -- operacion 70

        IF lr_preliquida.mensualidad = 12 THEN
            CALL f_desmarca_cuenta(lr_preliquida.nss         ,
                                   lr_preliquida.consecutivo ,
                                   "S"
                                  )

            CALL f_agrega_det_op70(lr_preliquida.nss         ,
                                   lr_preliquida.consecutivo ,
                                   gr_noti.conclusion       
                                  )

        END IF
}
    
    END FOREACH -- Siguiente NSS

    #-- Despliega resultados
    DISPLAY "TOTAL DE REGISTROS LIQUIDADOS : ", li_cont_tot AT 5,21
    DISPLAY "FOLIO DE PRELIQUIDACION       : ", li_folio AT 6,21

    DISPLAY "                                                                            " AT 8,1
    DISPLAY "                          MENSUALIDADES PAGADAS                             " AT 9,1 ATTRIBUTE(REVERSE)
    DISPLAY "  # MES    REGISTROS       # MES    REGISTROS        # MES    REGISTROS     " AT 10,1


    DISPLAY "1" AT 11, 7            DISPLAY lar_cont[1] AT 11, 14
    DISPLAY "2" AT 12, 7            DISPLAY lar_cont[2] AT 12, 14
    DISPLAY "3" AT 13, 7            DISPLAY lar_cont[3] AT 13, 14
    DISPLAY "4" AT 14, 7            DISPLAY lar_cont[4] AT 14, 14

    DISPLAY "5" AT 11, 31           DISPLAY lar_cont[5] AT 11, 39
    DISPLAY "6" AT 12, 31           DISPLAY lar_cont[6] AT 12, 39
    DISPLAY "7" AT 13, 31           DISPLAY lar_cont[7] AT 13, 39
    DISPLAY "8" AT 14, 31           DISPLAY lar_cont[8] AT 14, 39

    DISPLAY " 9" AT 11, 56          DISPLAY lar_cont[9]  AT 11, 65
    DISPLAY "10" AT 12, 56          DISPLAY lar_cont[10] AT 12, 65
    DISPLAY "11" AT 13, 56          DISPLAY lar_cont[11] AT 13, 65
    DISPLAY "12" AT 14, 56          DISPLAY lar_cont[12] AT 14, 65

    DISPLAY "OTROS" AT 15, 56       DISPLAY lar_cont[13] AT 15, 65

    CALL f_error_msg("PROCESO DE PRELIQUIDACION TERMINADO")


END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo  ,
        tipo_retiro LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_movim            SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    SELECT movimiento
    INTO   ls_movim
    FROM   tab_retiro
    WHERE  tipo_retiro = pr_desmarca.tipo_retiro

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               ls_movim             ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_agrega_det_op70 : Inserta un registro en la tabla de detalle de la op70 #
#                     de acuerdo al tipo de notificacion                    #
#---------------------------------------------------------------------------#
FUNCTION f_agrega_det_op70(pr_datos)

    DEFINE pr_datos RECORD
        nss          LIKE pen_solicitud_pmg.nss         ,
        consecutivo  LIKE pen_solicitud_pmg.consecutivo ,
        id_notifica  LIKE tab_tipo_notifica_pmg.clave  
    END RECORD 

    DEFINE lr_preliq RECORD
        subcuenta           SMALLINT        ,
        monto_pesos         DECIMAL(22,6)
    END RECORD
    
    DEFINE lr_dat_saldo RECORD
        subcuenta           SMALLINT    ,
        grupo               SMALLINT
    END RECORD 

    DEFINE lr_saldo_dia RECORD
        subcuenta           SMALLINT        ,
        siefore             SMALLINT        ,
        monto_acc           DECIMAL(16,6)   ,
        monto_pesos         DECIMAL(16,6)
    END RECORD
    
    DEFINE lr_op70 RECORD LIKE pen_detalle_op70.*
        
    -- --------------------------------------------------------------------
    LET lr_dat_saldo.subcuenta      = 0
    LET lr_dat_saldo.grupo          = 0

    LET lr_op70.folio_envio         = 0
    LET lr_op70.folio_datamart      = 0
    LET lr_op70.id_tipo_notifica    = pr_datos.id_notifica
    LET lr_op70.nss                 = pr_datos.nss
    LET lr_op70.cve_pension         = NULL
    LET lr_op70.diag_operacion      = NULL
    LET lr_op70.codigo_rechazo      = 0
    LET lr_op70.fecha_fallecimiento = NULL
 
    LET lr_op70.origen_informacion  = 0
    LET lr_op70.fecha_carga         = HOY
    LET lr_op70.hora_carga          = CURRENT HOUR TO SECOND
    LET lr_op70.usuario             = gc_usuario
    LET lr_op70.estado              = gr_edo.capturado
    LET lr_op70.num_men_calculadas  = 0 


    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
        LET lr_op70.fecha_agotamiento = HOY 
    ELSE
        LET lr_op70.fecha_agotamiento = NULL
    END IF

    SELECT curp             ,
           sec_pension      ,
           tipo_retiro      ,
           regimen          ,
           tipo_seguro      ,
           tipo_pension     ,
           "   "            ,
           tipo_prestacion  ,
           fecha_ini_pen
    INTO   lr_op70.curp             ,
           lr_op70.sec_pension      ,
           lr_op70.tipo_retiro      ,
           lr_op70.regimen          ,
           lr_op70.tipo_seguro      ,
           lr_op70.tipo_pension     ,
           lr_op70.cve_pension      ,
           lr_op70.tipo_prestacion  ,
           lr_op70.fecha_ini_pen
    FROM   pen_solicitud_pmg
    WHERE  nss         = pr_datos.nss         
    AND    consecutivo = pr_datos.consecutivo 
    
    SELECT fecha_liquida
    INTO   lr_op70.fecha_primer_pago     
    FROM   pen_ctr_pago_det
    WHERE  nss              = pr_datos.nss         
    AND    consecutivo      = pr_datos.consecutivo 
    AND    num_mensualidad  = 1

    SELECT MAX(num_mensualidad)
    INTO   lr_op70.num_men_pagadas
    FROM   pen_ctr_pago_det
    WHERE  nss          = pr_datos.nss         
    AND    consecutivo  = pr_datos.consecutivo 
    AND    estado       = gr_edo.liquidado
    
    SELECT fecha_liquida
    INTO   lr_op70.fecha_ultimo_pago  
    FROM   pen_ctr_pago_det
    WHERE  nss              = pr_datos.nss         
    AND    consecutivo      = pr_datos.consecutivo 
    AND    estado           = gr_edo.liquidado
    AND    num_mensualidad  = lr_op70.num_men_pagadas
    
    SELECT SUM(mto_pago_pesos)
    INTO   lr_op70.mto_total_pmg
    FROM   pen_ctr_pago_det
    WHERE  nss          = pr_datos.nss         
    AND    consecutivo  = pr_datos.consecutivo 
    AND    estado       = gr_edo.liquidado

    -- Montos de la ultima mensualidad pagada
    LET lr_op70.mto_retiro97    = 0
    LET lr_op70.mto_cv          = 0
    LET lr_op70.mto_cs          = 0
    LET lr_op70.mto_viv97       = 0

    IF lr_op70.id_tipo_notifica = gr_noti.agotamiento THEN
        DECLARE cur_op70pre CURSOR FOR
        SELECT subcuenta            ,
               monto_en_pesos * -1
        FROM   pen_preliquida_pmg
        WHERE  nss              = pr_datos.nss         
        AND    consecutivo_lote = pr_datos.consecutivo 
        AND    mensualidad      = lr_op70.num_men_pagadas
        
        FOREACH cur_op70pre INTO lr_preliq.*
        
            CASE lr_preliq.subcuenta
                WHEN 1
                    LET lr_op70.mto_retiro97 = lr_op70.mto_retiro97 + lr_preliq.monto_pesos
                WHEN 2                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 6                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 9                       
                    LET lr_op70.mto_cv       = lr_op70.mto_cv + lr_preliq.monto_pesos
                WHEN 5                       
                    LET lr_op70.mto_cs       = lr_op70.mto_cs + lr_preliq.monto_pesos
                WHEN 4                       
                    LET lr_op70.mto_viv97    = lr_op70.mto_viv97 + lr_preliq.monto_pesos
            END CASE
        
        END FOREACH
    END IF

    -- Saldo de la subcuenta
    LET lr_op70.saldo_retiro97  = 0
    LET lr_op70.saldo_cv        = 0
    LET lr_op70.saldo_cs        = 0
    LET lr_op70.saldo_viv97     = 0
    
    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo USING pr_datos.nss            ,
                            lr_dat_saldo.subcuenta  ,
                            lr_dat_saldo.grupo      ,
                            HOY                      
                      INTO  lr_saldo_dia.*

        CASE lr_saldo_dia.subcuenta
            WHEN 1
                LET lr_op70.saldo_retiro97  = lr_op70.saldo_retiro97 + lr_preliq.monto_pesos
            WHEN 2                       
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 6                          
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 9                          
                LET lr_op70.saldo_cv        = lr_op70.saldo_cv + lr_preliq.monto_pesos
            WHEN 5                          
                LET lr_op70.saldo_cs        = lr_op70.saldo_cs + lr_preliq.monto_pesos
            WHEN 4                          
                LET lr_op70.saldo_viv97     = lr_op70.saldo_viv97 + lr_preliq.monto_pesos
        END CASE
       
    END FOREACH


    INSERT INTO pen_detalle_op70
    VALUES (lr_op70.*)

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
    LET ls_exit = 1

    -- --------------------------------------------------------------------

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
        LET ls_exit = 0
    END IF

    RETURN ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_sobregiro : Verifica si el pago que se debe realizar no        #
#                        sobregira la cuenta individual del nss             #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_sobregiro(pr_sobregiro)

    DEFINE pr_sobregiro RECORD     
        nss             LIKE pen_solicitud_pmg.nss              ,
        consecutivo     LIKE pen_solicitud_pmg.consecutivo      ,
        id_viv          LIKE pen_solicitud_pmg.estado_sub_viv   ,
        total_pesos     LIKE pen_ctr_pago_det.mto_pago_pesos
    END RECORD
    
    DEFINE lr_saldo RECORD
        subcuenta           SMALLINT        ,
        siefore             SMALLINT        ,
        monto_acc           DECIMAL(16,6)   ,
        monto_pesos         DECIMAL(16,6)
    END RECORD

    DEFINE
        ls_sobregiro            ,
        ls_subcta               ,
        ls_gr_saldo             ,
        ls_grupo                SMALLINT

    DEFINE 
        ld_saldo_dia_pesos      DECIMAL(22,6)

    -- --------------------------------------------------------------------
    
    LET ls_sobregiro        = 0
    LET ls_gr_saldo         = 0
    LET ld_saldo_dia_pesos  = 0
    
    SELECT grupo
    INTO   ls_grupo
    FROM   pen_solicitud_pmg
    WHERE  nss          = pr_sobregiro.nss        
    AND    consecutivo  = pr_sobregiro.consecutivo
    
    DECLARE cur_sgiro CURSOR FOR
      SELECT subcuenta
      FROM   tab_agrupa_subcta
      WHERE  grupo = ls_grupo
      ORDER BY 1

    FOREACH cur_sgiro INTO ls_subcta
        
        DECLARE cur_sct_sgiro CURSOR FOR eje_saldo_dia

        FOREACH cur_sct_sgiro USING pr_sobregiro.nss ,
                                    ls_subcta        ,
                                    ls_gr_saldo      ,
                                    HOY               
                              INTO  lr_saldo.*        
        
            IF ls_subcta <> 4 OR (ls_subcta = 4 AND pr_sobregiro.id_viv = 1) THEN
                LET ld_saldo_dia_pesos  = lr_saldo.monto_pesos + ld_saldo_dia_pesos
            END IF
        END FOREACH
        
    END FOREACH 
    
    -- Si el monto a pagar es mayor al monto en pesos de la cuenta entonces se
    -- detecta un sobregiro
    IF pr_sobregiro.total_pesos > ld_saldo_dia_pesos THEN
        LET ls_sobregiro = 1
    END IF
    
    RETURN ls_sobregiro

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_por_liquidar
        DROP TABLE tmp_preliquida_pmg
        DROP TABLE tmp_detalle_sol
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_por_liquidar
    (
     consecutivo        DECIMAL(11,0) NOT NULL  ,
     sec_contrato       SMALLINT NOT NULL       ,
     nss                CHAR(11) NOT NULL       ,
     folio_lote         INTEGER                 ,
     fecha_pago_est     DATE                    ,
     num_mensualidad    SMALLINT                ,
     id_viv             SMALLINT                ,
     total_pesos        DECIMAL(16,6)           ,     --MLM-1936 antes 2 dec
     diag_registro      CHAR(3)                 ,
     marca_ult_pago     CHAR(1)
    ) 

    #-- --------------------------------------------------------------------

    SELECT *
    FROM   pen_preliquida_pmg
    WHERE  1 = 0
    INTO TEMP tmp_preliquida_pmg

    #-- --------------------------------------------------------------------

    SELECT *
    FROM   pen_detalle_sol
    WHERE  1 = 0
    INTO TEMP tmp_detalle_sol

    #-- --------------------------------------------------------------------

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
# f_obtiene_folio: Obtiene el ultimo folio para asignarse en                #
#                  el proceso de preliquidacion                             #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_folio()

    DEFINE
        li_ult_folio        INTEGER


    SELECT NVL(MAX(folio),0) + 1
    INTO   li_ult_folio
    FROM   glo_folio

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio

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
