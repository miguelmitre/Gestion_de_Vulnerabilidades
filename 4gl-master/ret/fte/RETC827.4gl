################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                #
#Owner             => E.F.P.                                                   #
#Programa RETC827  => GENERA NOTIFICACION DE RETIRO (PAGO EFECTUADO) (16)      #
#Fecha creacion    => 27 DE FEBRERO DEL 2004                                   #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 17 DE MARZO DEL 2008                                     #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 19 DE MARZO DE 2008                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Correccion de errores en la generacion del formato       #
#                     y del nombre del layout                                  #
#Fecha actualiz.   => 20 DE MAYO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Modificaciones para la adaptacion de la circular 31-11.  #
#                  => Se adapto el programa para que solo liquide y genere la  #
#                  => Operacion 16 para los retiros por matrimonio y los       #
#                     retiros por desempleo tipo D (anterior) o los que aun no #
#                     han ingresado al nuevo esquema                           #
#Fecha actualiz.   => 28 DE MAYO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se recupera el monto de desempleo de ret_parcial         #
#Fecha actualiz.   => 16 DE JUNIO DE 2009                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se agrego opcion para liquidar recalculando al dia de la #
#                     liquidacion                                              #
#Fecha actualiz.   => 26 DE JUNIO DE 2009                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se elimina la generacion de la operacion 16 desde este   #
#                  => programa y se traslada al programa RETC857               #
#Sistema           => RET                                                      #
################################################################################

DATABASE safre_af
GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_mov RECORD
        desempleo       SMALLINT,
        matrimonio      SMALLINT
    END RECORD

    DEFINE gr_prest RECORD
        desempleo       SMALLINT,
        matrimonio      SMALLINT
    END RECORD

    DEFINE gr_edo RECORD #gr_edo
        procesado             LIKE ret_estado.estado_solicitud ,
        provisionado          LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper13          INTEGER
    END RECORD

    DEFINE gs_cod_sct        LIKE tab_afore_local.codigo_afore

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(001) ,
        usuario               CHAR(012)

    DEFINE #glo #smallint
        gs_dias_sbc_t2        ,
        gs_dias_sbc_t3        , -- Indica el numero de dias a pagar del SBC
        gs_num_siefores       , -- Indica el numero de siefores que se usan actualmente
        v_marca_ent           ,
        s_codigo_afore        SMALLINT


    DEFINE #glo #integer
        cont_des              ,
        cont_mat              ,
        cont_1                ,
        cont_reg_av           INTEGER

    DEFINE #glo #decimal
        gd_porc_pago_t2       DECIMAL(10,5) , 
        gd_porc_pago_t3       DECIMAL(10,5) , -- Indica el porcentaje a pagar de RCV
        gd_tot_monto_pesos    DECIMAL(15,2)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
        CALL STARTLOG("RETC827.log")

    CALL init() #i
    OPEN WINDOW retc827 AT 4,4 WITH FORM "RETC8271" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC827  LIQUIDA RET. PARCIALES - MATRIMONIO Y DES. ANTERIOR                " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.folio_oper13 WITHOUT DEFAULTS
        AFTER FIELD folio_oper13
            IF reg_1.folio_oper13 IS NULL OR
               reg_1.folio_oper13 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper13
            ELSE
                SELECT "OK"
                FROM   ret_parcial_tx
                WHERE  folio = reg_1.folio_oper13
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR"    FOLIO INEXISTENTE" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper13
                END IF
            END IF

        ON KEY (ESC)
            IF reg_1.folio_oper13 IS NULL  OR
               reg_1.folio_oper13 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper13
            ELSE
                SELECT "OK"
                FROM   ret_parcial_tx
                WHERE  folio = reg_1.folio_oper13
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR"    FOLIO INEXISTENTE" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper13
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio            = reg_1.folio_oper13
            AND    tipo_movimiento IN (gr_mov.desempleo,
                                       gr_mov.matrimonio)
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR"    FOLIO YA LIQUIDADO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper13
            END IF

            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    SELECT  COUNT(*)
    INTO    cont_1
    FROM    ret_parcial A   ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     A.nss               = B.nss
    AND     A.consecutivo       = B.consecutivo
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada
    AND     A.estado_solicitud  = gr_edo.procesado
    AND    ( A.tipo_prestacion = gr_prest.matrimonio
             OR ( A.tipo_prestacion = gr_prest.desempleo AND A.tipo_desempleo = "D")
           )

    DISPLAY "TOTAL REGISTROS A PROCESAR : ", cont_1 AT 10,21
    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    IF s_codigo_afore = gs_cod_sct THEN
        CALL primer_paso_sct()  #pp -----RETIRO POR DESEMPLEO CON RECALCULO (TIPOS D2 y D3)
    END IF

    CALL primer_paso()  #pp -----RETIRO POR DESEMPLEO
    CALL segundo_paso() #sp -----RETIRO POR MATRIMONIO
    CALL tercer_paso()  #tp -----INSERTA EN ret_sum_envio

    DISPLAY "FOLIO NUMERO               : ",reg_1.folio_oper13  AT 08,21
    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1              AT 10,21
    DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg_av         AT 12,21

    DISPLAY "LIQUIDADOS DESEMPLEO       : ",cont_des            AT 13,21
    DISPLAY "LIQUIDADOS MATRIMONIO      : ",cont_mat            AT 14,21

    PROMPT  " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc827

END MAIN


FUNCTION init()
#--------------

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                   = TODAY
    LET cont_1                = 0
    LET cont_reg_av           = 0
    LET cont_des              = 0
    LET cont_mat              = 0
    LET gd_tot_monto_pesos    = 0
    
    LET gd_porc_pago_t2       = 11.5 -- El porcentaje a pagar del tipo 2 es el 11.5%
    LET gd_porc_pago_t3       = 10   -- El porcentaje a pagar del tipo 3 es el 10%
    
    LET gs_dias_sbc_t2        = 90 -- El numero de dias del SBC tipo 2 a pagar son 90
    LET gs_dias_sbc_t3        = 75 -- El numero de dias del SBC tipo 3 a pagar son 75

    LET gr_prest.desempleo    = 6
    LET gr_mov.desempleo      = 875
    LET gr_prest.matrimonio   = 7
    LET gr_mov.matrimonio     = 870

    CALL f_obtiene_precios_accion(HOY)

    -- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore < 11

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
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
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT MAX(folio)
    INTO   reg_1.folio_oper13
    FROM   ret_parcial_tx

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    ----- SALDO DIA -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    ----- PORCENTAJE DE RCV -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_calcula_parcial_porcentaje (?,?,?)"
    PREPARE eje_parcial FROM lc_prepare

    ----- PAGO DE DIAS DE SALARIO BASE COT  -----
    LET lc_prepare = " "
    LET lc_prepare ="EXECUTE FUNCTION fn_calcula_pago_sbc_parciales (?,?,?)"
    PREPARE eje_parcial_sbc FROM lc_prepare

    ----- LIQUIDACION PARA PARCIAL MATRIOMONIO  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_liq_parcial_mat_5 (?,?,?,?)"
    PREPARE eje_liq_parcial FROM lc_prepare

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = " "
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_parciales_consar(?,?,?,?)"
    PREPARE eje_CONSAR_parcial FROM lc_prepare

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " "
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

END FUNCTION


FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_10 RECORD #loc #reg_10
        nss                     LIKE ret_parcial_tx.nss           ,
        consecutivo             LIKE ret_parcial.consecutivo      ,
        pago_desempleo          LIKE ret_parcial.pago_desempleo   ,
        salario_base_cot        LIKE ret_parcial.salario_base_cot ,
        num_resolucion          LIKE ret_parcial.num_resolucion
    END RECORD
    
    DEFINE reg_13 RECORD #loc #reg_13
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD    

    DEFINE lar_pago ARRAY [20] OF RECORD
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE lr_det_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_sie_acc   DECIMAL(16,6) ,
        acciones_pago   DECIMAL(16,6) ,
        pesos_pago      DECIMAL(16,6)
    END RECORD

    DEFINE
        ld_monto_acc_cv       DECIMAL(16,6) ,
        ld_porc_pesos_rcv     DECIMAL(16,6) ,
        ld_pago_sbc           DECIMAL(16,2) ,
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6)

    DEFINE #loc #smallint
        ls_resp                 ,
        ls_cod_tramite          ,
        ls_inserta              ,
        ls_siefore              , #-- contador para los ciclos for
        f_subcta                ,
        f_subcuenta             ,
        f_siefore               SMALLINT

    -- -----------------------------------------------------------------------------

    LET reg_13.estado_marca = 0
    LET reg_13.marca_causa  = 0

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    SELECT  "OK"
    FROM    ret_parcial A    ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     B.nss               = A.nss
    AND     B.consecutivo       = A.consecutivo
    AND     A.tipo_prestacion   = gr_prest.desempleo
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada
    AND     A.tipo_desempleo    = "D"
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_2 CURSOR FOR
    SELECT  B.nss               ,
            B.consecutivo       ,
            A.pago_desempleo    ,
            A.salario_base_cot  ,
            A.num_resolucion
    FROM    ret_parcial A       ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     B.nss               = A.nss
    AND     B.consecutivo       = A.consecutivo
    AND     A.tipo_prestacion   = gr_prest.desempleo
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada
    AND     A.tipo_desempleo    = "D"

    FOREACH cur_2 INTO reg_10.*
        LET cont_reg_av       = cont_reg_av + 1
        LET cont_des          = cont_des + 1
        LET ld_pago_sbc       = 0
        LET ld_porc_pesos_rcv = 0

        DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg_av         AT 12,21
        DISPLAY "LIQUIDADOS DESEMPLEO       : ",cont_des            AT 13,21

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_pago[ls_siefore].activo             = FALSE
            LET lar_pago[ls_siefore].acciones_ret97     = 0
            LET lar_pago[ls_siefore].acciones_cv        = 0
            LET lar_pago[ls_siefore].acciones_cs        = 0
            LET lar_pago[ls_siefore].acciones_est       = 0
            LET lar_pago[ls_siefore].acciones_esp       = 0
        END FOR

        -- Obtenemos los montos por subcuenta para liquidar el pago
        DECLARE cur_liq CURSOR FOR eje_parcial_sbc
        FOREACH cur_liq USING reg_10.nss            ,
                              reg_10.pago_desempleo ,
                              HOY
                        INTO  lr_det_pago.*

            IF lr_det_pago.acciones_pago > 0 THEN
                LET f_siefore                  = lr_det_pago.siefore
                LET lar_pago[f_siefore].activo = TRUE

                CALL actualiza_cuenta_ind(reg_1.folio_oper13                   ,
                                          reg_10.nss                           ,
                                          lr_det_pago.subcta                   ,
                                          lr_det_pago.siefore                  ,
                                          gr_mov.desempleo                     ,
                                          HOY                                  ,
                                          HOY                                  ,
                                          HOY                                  ,
                                          gar_precio_acc[f_siefore].precio_dia ,
                                          -lr_det_pago.acciones_pago           ,
                                          -lr_det_pago.pesos_pago              ,
                                          reg_10.consecutivo                   ,
                                          usuario
                                         )
            ELSE
                LET lr_det_pago.acciones_pago = 0
            END IF

            CASE lr_det_pago.subcta
                WHEN 1
                    LET lar_pago[f_siefore].acciones_ret97 = lr_det_pago.acciones_pago
                WHEN 2
                    LET lar_pago[f_siefore].acciones_cv    = lr_det_pago.acciones_pago
                WHEN 5
                    LET lar_pago[f_siefore].acciones_cs    = lr_det_pago.acciones_pago
                WHEN 6
                    LET lar_pago[f_siefore].acciones_est   = lr_det_pago.acciones_pago
                WHEN 9
                    LET lar_pago[f_siefore].acciones_esp   = lr_det_pago.acciones_pago
            END CASE
        END FOREACH

        FOR ls_siefore = 1 TO gs_num_siefores

            LET ld_monto_acc_cv = 0

            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_pago[ls_siefore].activo = TRUE THEN

                LET ld_monto_acc_cv  = lar_pago[ls_siefore].acciones_cv  +
                                       lar_pago[ls_siefore].acciones_est +
                                       lar_pago[ls_siefore].acciones_esp

                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss                          ,--nss
                        reg_10.consecutivo                  ,--consecutivo
                        reg_1.folio_oper13                  ,--folio
                        "I"                                 ,--tipo_retiro
                        16                                  ,--tipo_operacion
                        ls_siefore                          ,--siefore
                        lar_pago[ls_siefore].acciones_ret97 ,--acciones_ret97
                        ld_monto_acc_cv                     ,--acciones_cv
                        lar_pago[ls_siefore].acciones_cs    ,--acciones_cs
                        0                                    --acciones_ret92
                       )
            END IF
        END FOR

        SELECT ROUND(SUM(monto_en_pesos),2)
        INTO   ld_porc_pesos_rcv
        FROM   dis_cuenta
        WHERE  nss   = reg_10.nss
        AND    folio = reg_1.folio_oper13
        AND    consecutivo_lote = reg_10.consecutivo
        AND    subcuenta IN (1,2,5,6,9)

        IF ld_porc_pesos_rcv IS NULL THEN
            LET ld_porc_pesos_rcv = 0
        ELSE
            LET ld_porc_pesos_rcv = ld_porc_pesos_rcv * -1
        END IF

        LET gd_tot_monto_pesos = gd_tot_monto_pesos + ld_porc_pesos_rcv

        EXECUTE eje_CONSAR_parcial USING reg_10.nss             ,
                                         reg_10.consecutivo     ,
                                         reg_10.num_resolucion  ,
                                         reg_1.folio_oper13
                                   INTO  ls_inserta

        LET ls_resp     = 0

        EXECUTE eje_marca_pen USING reg_10.nss              ,
                                    reg_1.folio_oper13      ,
                                    reg_10.consecutivo      ,
                                    "I"                     ,
                                    ls_cod_tramite          ,
                                    HOY
                              INTO  ls_resp

        UPDATE ret_parcial_tx
        SET    fecha_valuacion = HOY ,
               fecha_pago      = HOY ,
               pago_desempleo  = ld_porc_pesos_rcv
        WHERE  nss             = reg_10.nss
        AND    consecutivo     = reg_10.consecutivo
        AND    folio           = reg_1.folio_oper13

        UPDATE ret_parcial
        SET    estado_solicitud = gr_edo.liquidado
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo

        EXECUTE eje_desmarca USING reg_10.nss           ,
                                   gr_mov.desempleo     ,
                                   reg_10.consecutivo   ,
                                   reg_13.estado_marca  ,
                                   reg_13.marca_causa   ,
                                   usuario


    END FOREACH
END FUNCTION


FUNCTION primer_paso_sct()
#pp-------------------
    DEFINE reg_10 RECORD #loc #reg_10
        nss                   LIKE ret_parcial_tx.nss           ,
        consecutivo           LIKE ret_parcial.consecutivo      ,
        salario_base_cot      LIKE ret_parcial.salario_base_cot ,
        tipo_pago             LIKE ret_parcial.tipo_pago
    END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD    

    DEFINE lar_siefore ARRAY [20] OF RECORD
        activo            SMALLINT,
        pesos_ret97       DECIMAL(16,6) ,
        pesos_cv          DECIMAL(16,6) ,
        pesos_cs          DECIMAL(16,6) ,
        pesos_est         DECIMAL(16,6) ,
        pesos_esp         DECIMAL(16,6)
    END RECORD

    DEFINE lar_pago ARRAY [20] OF RECORD
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE lr_det_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        saldo_sie_acc   DECIMAL(16,6) ,
        acciones_pago   DECIMAL(16,6) ,
        pesos_pago      DECIMAL(16,6)
    END RECORD

    DEFINE
        ld_porc_pago          DECIMAL(10,5) ,
        monto_pesos_cv        DECIMAL(16,2) ,
        monto_pesos_rcv       DECIMAL(12,2) ,
        ld_monto_acc_cv       DECIMAL(16,6) ,
        ld_porc_pesos_rcv     DECIMAL(16,6) ,
        ld_pago_desempleo     DECIMAL(16,6) ,
        ld_pago_sbc           DECIMAL(16,2) ,
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6) ,
        monto_pesos_sub1_2    DECIMAL(10,2) ,
        monto_pesos_sub5_2    DECIMAL(10,2)

    DEFINE #loc #smallint
        ls_dias_sbc           ,
        ls_siefore            , #-- contador para los ciclos for
        v_subcuenta           ,
        v_grupo               ,
        f_subcta              ,
        f_subcuenta           ,
        f_siefore             SMALLINT

    LET reg_13.estado_marca = 0
    LET reg_13.marca_causa  = 0

    SELECT  "OK"
    FROM    ret_parcial A    ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     B.nss               = A.nss
    AND     B.consecutivo       = A.consecutivo
    AND     A.tipo_prestacion   = gr_prest.desempleo
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada
    AND     A.tipo_desempleo    = "D"
    AND     A.tipo_pago         IN (2,3)
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_sct CURSOR FOR
    SELECT  B.nss               ,
            B.consecutivo       ,
            A.salario_base_cot  ,
            A.tipo_pago
    FROM    ret_parcial A    ,
            ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.tipo_prestacion = gr_prest.desempleo
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada
    AND     A.tipo_desempleo  = "D"
    AND     A.tipo_pago         IN (2,3)

    FOREACH cur_sct INTO reg_10.*
        LET cont_reg_av       = cont_reg_av + 1
        LET cont_des          = cont_des + 1
        LET ld_pago_sbc       = 0
        LET ld_porc_pesos_rcv = 0
        LET ld_pago_desempleo = 0

        DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg_av         AT 12,21
        DISPLAY "LIQUIDADOS DESEMPLEO       : ",cont_des            AT 13,21

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_siefore[ls_siefore].activo          = FALSE
            LET lar_siefore[ls_siefore].pesos_ret97     = 0
            LET lar_siefore[ls_siefore].pesos_cv        = 0
            LET lar_siefore[ls_siefore].pesos_cs        = 0
            LET lar_siefore[ls_siefore].pesos_est       = 0
            LET lar_siefore[ls_siefore].pesos_esp       = 0

            LET lar_pago[ls_siefore].activo             = FALSE
            LET lar_pago[ls_siefore].acciones_ret97     = 0
            LET lar_pago[ls_siefore].acciones_cv        = 0
            LET lar_pago[ls_siefore].acciones_cs        = 0
            LET lar_pago[ls_siefore].acciones_est       = 0
            LET lar_pago[ls_siefore].acciones_esp       = 0
        END FOR

        LET v_subcuenta = 0
        LET v_grupo     = 0

        --OBTIENE EL SALDO POR NSS DE CADA UNA DE LAS SUBCUENTAS--------------
        DECLARE c_saldo CURSOR FOR eje_saldo_dia
        FOREACH c_saldo  USING reg_10.nss   ,
                               v_subcuenta  , --Si subcta. y grupo son 0, se
                               v_grupo      , --obtiene saldo de todas las
                               HOY            --subcuentas.
                         INTO  f_subcuenta  ,
                               f_siefore    ,
                               f_monto_acc  ,
                               f_monto_pes

            IF f_subcuenta = 1 OR f_subcuenta = 2 OR f_subcuenta = 5 OR
               f_subcuenta = 6 OR f_subcuenta = 9 THEN

                IF f_monto_acc IS NULL OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                    LET f_monto_pes = 0
                ELSE
                    IF f_siefore <> 11 THEN
                        #-- Marcamos como activo el registro de la siefore actual
                        LET lar_siefore[f_siefore].activo = TRUE
                        CASE f_subcuenta
                            WHEN 1
                                LET lar_siefore[f_siefore].pesos_ret97 = f_monto_pes
                            WHEN 2
                                LET lar_siefore[f_siefore].pesos_cv    = f_monto_pes
                            WHEN 5
                                LET lar_siefore[f_siefore].pesos_cs    = f_monto_pes
                            WHEN 6
                                LET lar_siefore[f_siefore].pesos_est   = f_monto_pes
                            WHEN 9
                                LET lar_siefore[f_siefore].pesos_esp   = f_monto_pes
                        END CASE
                    END IF
                END IF -- Monto en pesos no nulo
            END IF -- Subcuentas de RCV
        END FOREACH -- Siguiente subcuenta

        FOR ls_siefore = 1 TO gs_num_siefores
            IF lar_siefore[ls_siefore].activo = TRUE THEN
                LET monto_pesos_cv  = lar_siefore[ls_siefore].pesos_cv +
                                      lar_siefore[ls_siefore].pesos_est +
                                      lar_siefore[ls_siefore].pesos_esp


                LET monto_pesos_rcv = monto_pesos_cv +
                                      lar_siefore[ls_siefore].pesos_ret97 +
                                      lar_siefore[ls_siefore].pesos_cs

            END IF
        END FOR

        IF reg_10.tipo_pago = 2 THEN
            LET ls_dias_sbc  = gs_dias_sbc_t2
            LET ld_porc_pago = gd_porc_pago_t2
        ELSE
            LET ls_dias_sbc  = gs_dias_sbc_t3
            LET ld_porc_pago = gd_porc_pago_t3
        END IF
        
        LET ld_pago_sbc       = reg_10.salario_base_cot * ls_dias_sbc
        LET ld_porc_pesos_rcv = (monto_pesos_rcv * ld_porc_pago) / 100

        -- Determinamos si lo que se paga es el porcentaje de RCV
        -- o tantos dias del SBC dependiendo del tipo de pago
        IF ld_porc_pesos_rcv < ld_pago_sbc THEN

            LET ld_pago_desempleo = ld_porc_pesos_rcv
            
            --- CALCULA EL PORCENTAJE A PAGAR PARA DESEMPLEO---------------------------------------
            DECLARE cur_4 CURSOR FOR eje_parcial
            FOREACH cur_4 USING reg_10.nss   ,
                                ld_porc_pago ,
                                HOY
                          INTO  lr_det_pago.subcta        ,
                                lr_det_pago.siefore       ,
                                lr_det_pago.saldo_sie_acc ,
                                lr_det_pago.acciones_pago

                IF lr_det_pago.acciones_pago > 0 THEN

                    LET f_siefore                  = lr_det_pago.siefore
                    LET lar_pago[f_siefore].activo = TRUE

                    LET lr_det_pago.pesos_pago = lr_det_pago.acciones_pago *
                                                 gar_precio_acc[f_siefore].precio_dia

                    CALL actualiza_cuenta_ind(reg_1.folio_oper13                    ,
                                              reg_10.nss                            ,
                                              lr_det_pago.subcta                    ,
                                              lr_det_pago.siefore                   ,
                                              gr_mov.desempleo                      ,
                                              HOY                                   ,
                                              HOY                                   ,
                                              HOY                                   ,
                                              gar_precio_acc[f_siefore].precio_dia  ,
                                              -lr_det_pago.acciones_pago            ,
                                              -lr_det_pago.pesos_pago               ,
                                              reg_10.consecutivo                    ,
                                              usuario
                                             )
                ELSE
                    LET lr_det_pago.acciones_pago = 0
                END IF

                CASE lr_det_pago.subcta
                     WHEN 1
                        LET lar_pago[f_siefore].acciones_ret97 = lr_det_pago.acciones_pago
                     WHEN 2
                        LET lar_pago[f_siefore].acciones_cv    = lr_det_pago.acciones_pago
                     WHEN 5
                        LET lar_pago[f_siefore].acciones_cs    = lr_det_pago.acciones_pago
                     WHEN 6
                        LET lar_pago[f_siefore].acciones_est   = lr_det_pago.acciones_pago
                     WHEN 9
                        LET lar_pago[f_siefore].acciones_esp   = lr_det_pago.acciones_pago
                END CASE
            END FOREACH

            FOR ls_siefore = 1 TO gs_num_siefores

                LET ld_monto_acc_cv = 0

                #-- Almacenamos solo los registros de las siefores que contengan saldo
                IF lar_pago[ls_siefore].activo = TRUE THEN

                    LET ld_monto_acc_cv  = lar_pago[ls_siefore].acciones_cv  +
                                           lar_pago[ls_siefore].acciones_est +
                                           lar_pago[ls_siefore].acciones_esp

                    INSERT INTO ret_monto_siefore
                    VALUES (reg_10.nss                          ,--nss
                            reg_10.consecutivo                  ,--consecutivo
                            reg_1.folio_oper13                  ,--folio
                            "I"                                 ,--tipo_retiro
                            16                                  ,--tipo_operacion
                            ls_siefore                          ,--siefore
                            lar_pago[ls_siefore].acciones_ret97 ,--acciones_ret97
                            ld_monto_acc_cv                     ,--acciones_cv
                            lar_pago[ls_siefore].acciones_cs    ,--acciones_cs
                            0                                    --acciones_ret92
                           )
                END IF
            END FOR
        ELSE
            ---CALCULA PAGO EN BASE A SALARIO BASE DE COTIZACION - DESEMPLEO-----------

            LET ld_pago_desempleo = ld_pago_sbc
            
            DECLARE cur_6 CURSOR FOR eje_parcial_sbc
            FOREACH cur_6 USING reg_10.nss      ,
                                ld_pago_sbc ,
                                HOY
                          INTO  lr_det_pago.*

                IF lr_det_pago.acciones_pago > 0 THEN
                    LET f_siefore                  = lr_det_pago.siefore
                    LET lar_pago[f_siefore].activo = TRUE

                    CALL actualiza_cuenta_ind(reg_1.folio_oper13                   ,
                                              reg_10.nss                           ,
                                              lr_det_pago.subcta                   ,
                                              lr_det_pago.siefore                  ,
                                              gr_mov.desempleo                     ,
                                              HOY                                  ,
                                              HOY                                  ,
                                              HOY                                  ,
                                              gar_precio_acc[f_siefore].precio_dia ,
                                              -lr_det_pago.acciones_pago           ,
                                              -lr_det_pago.pesos_pago              ,
                                              reg_10.consecutivo                   ,
                                              usuario
                                             )
                ELSE
                    LET lr_det_pago.acciones_pago = 0
                END IF

                CASE lr_det_pago.subcta
                    WHEN 1
                        LET lar_pago[f_siefore].acciones_ret97 = lr_det_pago.acciones_pago
                    WHEN 2
                        LET lar_pago[f_siefore].acciones_cv    = lr_det_pago.acciones_pago
                    WHEN 5
                        LET lar_pago[f_siefore].acciones_cs    = lr_det_pago.acciones_pago
                    WHEN 6
                        LET lar_pago[f_siefore].acciones_est   = lr_det_pago.acciones_pago
                    WHEN 9
                        LET lar_pago[f_siefore].acciones_esp   = lr_det_pago.acciones_pago
                END CASE
            END FOREACH

            FOR ls_siefore = 1 TO gs_num_siefores

                LET ld_monto_acc_cv = 0

                #-- Almacenamos solo los registros de las siefores que contengan saldo
                IF lar_pago[ls_siefore].activo = TRUE THEN

                    LET ld_monto_acc_cv  = lar_pago[ls_siefore].acciones_cv  +
                                           lar_pago[ls_siefore].acciones_est +
                                           lar_pago[ls_siefore].acciones_esp

                    INSERT INTO ret_monto_siefore
                    VALUES (reg_10.nss                          ,--nss
                            reg_10.consecutivo                  ,--consecutivo
                            reg_1.folio_oper13                  ,--folio
                            "I"                                 ,--tipo_retiro
                            16                                  ,--tipo_operacion
                            ls_siefore                          ,--siefore
                            lar_pago[ls_siefore].acciones_ret97 ,--acciones_ret97    1
                            ld_monto_acc_cv                     ,--acciones_cv       2,6,9
                            lar_pago[ls_siefore].acciones_cs    ,--acciones_cs       5
                            0                                    --acciones_ret92
                           )
                END IF
            END FOR
        END IF -- Pago de porcentaje o dias de SBC

        SELECT ROUND(SUM(monto_en_pesos),2)
        INTO   ld_porc_pesos_rcv
        FROM   dis_cuenta
        WHERE  nss   = reg_10.nss
        AND    folio = reg_1.folio_oper13
        AND    consecutivo_lote = reg_10.consecutivo
        AND    subcuenta IN (1,2,5,6,9)

        IF ld_porc_pesos_rcv IS NULL THEN
            LET ld_porc_pesos_rcv = 0
        ELSE
            LET ld_porc_pesos_rcv = ld_porc_pesos_rcv * -1
        END IF

        LET gd_tot_monto_pesos = gd_tot_monto_pesos + ld_porc_pesos_rcv

        UPDATE ret_parcial_tx
        SET    fecha_valuacion = HOY ,
               fecha_pago      = HOY ,
               pago_desempleo  = ld_porc_pesos_rcv
        WHERE  nss             = reg_10.nss
        AND    consecutivo     = reg_10.consecutivo
        AND    folio           = reg_1.folio_oper13

        UPDATE ret_parcial
        SET    estado_solicitud = gr_edo.liquidado  ,
               pago_desempleo   = ld_pago_desempleo
        WHERE  nss              = reg_10.nss
        AND    consecutivo      = reg_10.consecutivo

        EXECUTE eje_desmarca USING reg_10.nss           ,
                                   gr_mov.desempleo     ,
                                   reg_10.consecutivo   ,
                                   reg_13.estado_marca  ,
                                   reg_13.marca_causa   ,
                                   usuario
    END FOREACH
END FUNCTION



FUNCTION segundo_paso()
#sp--------------------
    DEFINE reg_11 RECORD #loc #reg_11
        nss                 LIKE ret_parcial_tx.nss             ,
        consecutivo         LIKE ret_parcial.consecutivo        ,
        impt_autorizado     LIKE ret_parcial.impt_autorizado    ,
        num_resolucion      LIKE ret_parcial.num_resolucion
    END RECORD
    
    DEFINE reg_13 RECORD #loc #reg_13
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD        

    DEFINE #loc #smallint
        ls_resp                 ,
        ls_cod_tramite          ,
        ls_inserta              ,
        estado                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET reg_13.estado_marca = 0
    LET reg_13.marca_causa  = 0

    SELECT cod_tramite
    INTO   ls_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"


    SELECT  "OK"
    FROM    ret_parcial A    ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     B.nss               = A.nss
    AND     B.consecutivo       = A.consecutivo
    AND     A.tipo_prestacion   = gr_prest.matrimonio
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_3 CURSOR FOR
    SELECT  B.nss               ,
            B.consecutivo       ,
            A.impt_autorizado   ,
            A.num_resolucion
    FROM    ret_parcial A       ,
            ret_parcial_tx B
    WHERE   B.folio             = reg_1.folio_oper13
    AND     B.nss               = A.nss
    AND     B.consecutivo       = A.consecutivo
    AND     A.tipo_prestacion   = gr_prest.matrimonio
    AND     A.estado_solicitud  = gr_edo.procesado
    AND     A.diag_cuenta_ind   = 400 --Cuenta Aceptada

    FOREACH cur_3 INTO reg_11.*

        LET cont_reg_av         = cont_reg_av + 1
        LET cont_mat            = cont_mat + 1
        LET gd_tot_monto_pesos  = gd_tot_monto_pesos + reg_11.impt_autorizado

        DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg_av         AT 12,21
        DISPLAY "LIQUIDADOS MATRIMONIO      : ",cont_mat            AT 14,21

        EXECUTE eje_liq_parcial USING reg_1.folio_oper13 ,--folio
                                      reg_11.nss         ,--nss
                                      reg_11.consecutivo ,--consecutivo
                                      HOY                 --fecha liquida
                                INTO  estado

        IF estado <> 0 THEN
            PROMPT "INCONSISTENCIA EN LA LIQUIDACION ",reg_11.nss ,estado
            FOR CHAR enter
        END IF

        EXECUTE eje_CONSAR_parcial USING reg_11.nss             ,
                                         reg_11.consecutivo     ,
                                         reg_11.num_resolucion  ,
                                         reg_1.folio_oper13
                                   INTO  ls_inserta

        LET ls_resp     = 0

        EXECUTE eje_marca_pen USING reg_11.nss              ,
                                    reg_1.folio_oper13      ,
                                    reg_11.consecutivo      ,
                                    "I"                     ,
                                    ls_cod_tramite          ,
                                    HOY
                              INTO  ls_resp


        UPDATE ret_parcial_tx
        SET    fecha_valuacion = HOY ,
               fecha_pago      = HOY ,
               pago_desempleo  = 0
        WHERE  nss             = reg_11.nss
        AND    consecutivo     = reg_11.consecutivo
        AND    folio           = reg_1.folio_oper13

        UPDATE ret_parcial
        SET    estado_solicitud = gr_edo.liquidado
        WHERE  nss              = reg_11.nss
        AND    consecutivo      = reg_11.consecutivo

        EXECUTE eje_desmarca USING reg_11.nss           ,
                                   gr_mov.matrimonio    ,
                                   reg_11.consecutivo   ,
                                   reg_13.estado_marca  ,
                                   reg_13.marca_causa   ,
                                   usuario
    END FOREACH

END FUNCTION

FUNCTION tercer_paso()
#tp-------------------
    INSERT INTO ret_ctr_envio
        VALUES (reg_1.folio_oper13        , -- folio
                "AV16"                    , -- tipo_operacion
                "CZAAV16 DETAV16 SUMAV16" , -- estructura
                gr_edo.procesado          , -- status
                0                         , -- orden_de_envio
                HOY                         -- fecha_envio
               )

    INSERT INTO ret_sum_envio
        VALUES(reg_1.folio_oper13 , -- folio
               "16"               , -- tipo_operacion
               NULL               , -- diag_cuenta_ind
               "AV"               , -- area_origen
               cont_reg_av        , -- tot_registros
               gd_tot_monto_pesos , -- impt_tot_oper
               gr_edo.procesado   , -- status
               HOY                  -- fecha_envio
              ) 
               
END FUNCTION


FUNCTION actualiza_cuenta_ind(reg_4)
#aci--------------------------------
    DEFINE reg_4 RECORD
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

    DEFINE lc_id_aportante LIKE dis_cuenta.id_aportante

    IF reg_4.tipo_movimiento = 560 THEN
        LET lc_id_aportante = "DEV_APL"
    ELSE
        LET lc_id_aportante = "RETIRO"
    END IF

    INSERT INTO safre_af:dis_cuenta
        VALUES(reg_4.tipo_movimiento   ,
               reg_4.subcuenta         ,
               reg_4.siefore           ,
               reg_4.folio             ,
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      , -- curp
               ""                      , -- folio_sua
               reg_4.fecha_pago        , -- fecha_pago
               reg_4.fecha_valor       , -- fecha_valor
               reg_4.fecha_conversion  , -- fecha_conversion
               reg_4.monto_en_pesos    , -- monto_en_pesos
               reg_4.monto_en_acciones , -- monto_en_acciones
               reg_4.precio_accion     , -- precio_accion
               0                       , -- dias_cotizados
               ""                      , -- sucursal
               lc_id_aportante         , -- id_aportante
               gr_edo.liquidado        , -- status
               HOY                     , -- fecha_proceso
               reg_4.usuario           , -- usuario
               HOY                     , -- fecha_archivo
               0                         -- etiqueta
              )
END FUNCTION

FUNCTION f_obtiene_precios_accion(fecha_precios)
#opa------------------------------------------
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
