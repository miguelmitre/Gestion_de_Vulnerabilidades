################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETC809  => GENERA NOTIFICACION DE DISPOSICION DE RECURSOS SOLICITUD #
#                     OP.05 DE RETIROS POR REINGRESO                           #
#Fecha creacion    => 11 DE MARZO DEL 2008                                     #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiza   => 8 DE JULIO DE 2008                                       #
#                  => Se modifico la forma de provisionar las subcuentas para  #
#                     que en el caso de vivienda se provisione con el saldo al #
#                     primer dia del mes. Ademas se incluye la verificacion de #
#                     sobregiro.                                               #
#Sistema           => RET                                                      #
################################################################################
#Modificacion     => Se fue modificado el sub query                            #
#05-feb-2015      => Phelippe Ricardo dos Santos       --CPL-1863              #
################################################################################
#Modificacion     => Se fue modificado el sub query                            #
#05-feb-2015      => Alejandro Chagoya Salazar       --CPL-1881                #
################################################################################
#Modificacion      => Jonathan Joddy Zavala Zavala   CPL-2106  1-Oct-2015      #
#                  => Se actualiza programa agregando caracteristicas derivadas#
#                  => de la creacion de la siefore basica 0/90                 #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #param_ret
        g_param_ret            RECORD LIKE seg_modulo.*

    #DEFINE gar_precio_acc ARRAY [20] OF RECORD #CPL-2106
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        confirmado              LIKE ret_estado.estado_solicitud ,
        modificado              LIKE ret_estado.estado_solicitud ,
        procesado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD

    DEFINE #glo #date
        HOY                    ,
        gd_fecha_viv      DATE

    DEFINE #glo #char
        v_valida_precio         CHAR(200) ,
        comando                 CHAR(110) ,
        gc_tipo_ret             CHAR(001) ,
        enter                   CHAR(001) ,
        gc_usuario              CHAR(008) ,
        G_LISTA_DET             CHAR(100) ,
        HORA                    CHAR(005) ,
        v_saldo_dia             CHAR(150) ,
        v_obten_fecha_viv       CHAR(100) ,
        v_provisiona            CHAR(150) ,
        v_ap_posteriores        CHAR(100)


    DEFINE #glo #smallint
        gs_viv                  ,       -- Indica en que posicion se encuentra almacenada
                                        -- la info de precio de vivienda
        gs_codigo_afore         ,
        gs_num_siefores         ,       --Indica el numero de siefores que se usan actualmente
        sw_1                    ,
        gs_tipo_movimiento      SMALLINT

    DEFINE #glo #integer
        gi_ult_folio           ,
        cont_reg               INTEGER


END GLOBALS

DEFINE ms_99_siefores  SMALLINT  #CPL-2106

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()
    CALL obtiene_precios_accion(HOY)

    OPEN WINDOW retc809 AT 4,4 WITH FORM "RETC8091" ATTRIBUTE(BORDER)
    DISPLAY " RETC809    GENERA NOTIFICACION POR DISPOSICION DE RECURSOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < Ctrl-C > Salir                                           TIPO RETIRO J      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    SELECT  "OK"
    FROM    ret_solicitud_tx A
    WHERE   A.tipo_retiro       = gc_tipo_ret
    AND     A.estado_solicitud  IN (reg_2.confirmado, reg_2.modificado)
    AND     A.rechazo_cod       = 0
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        CALL f_lib_error_msg("NO EXISTEN REGISTROS A PROCESAR")
    ELSE
        IF f_lib_pregunta("¿EJECUTAR LA PROVISION DEL TIPO RETIRO J? (S/N) ") = TRUE THEN

            LET gi_ult_folio = f_lib_obtiene_ult_folio()

            DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

            CALL primer_paso()  #pp --CALCULA SALDOS
            CALL segundo_paso() #sp --GENERA PLANO

            DISPLAY "TOTAL DE REGISTROS A ENVIAR     : ",cont_reg AT 11,19

            DISPLAY " FOLIO NUMERO : ",gi_ult_folio  AT 18,1
            CALL f_lib_error_msg("PROCESO FINALIZADO CORRECTAMENTE")
        END IF
    END IF

    CLOSE WINDOW retc809

END MAIN


FUNCTION init()
#--------------
    LET HOY             = TODAY
    LET HORA            = TIME
    LET gc_tipo_ret     = "J"
    LET gs_viv          = 20
    LET gc_usuario      = f_lib_obten_user()
    LET ms_99_siefores  = 99

    --Obtenemos el numero de siefores en el sistema--
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore  > 0
    AND    codigo_siefore <> 11

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_ret.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.estado_solicitud
    INTO   reg_2.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_2.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   reg_2.modificado
    FROM   ret_estado A
    WHERE  A.descripcion = "MODIFICADO"

    SELECT movimiento
    INTO   gs_tipo_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = gc_tipo_ret

    SELECT "OK"
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro      = gc_tipo_ret
    AND    A.estado_solicitud IN (reg_2.confirmado, reg_2.modificado)
    GROUP BY 1

    --- SALDOS ---
    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? )"
    PREPARE eje_saldo_dia FROM v_saldo_dia

    --- PROVISION ---
    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona


    --- FECHA VALOR VIVENDA ---
    LET v_obten_fecha_viv = " EXECUTE FUNCTION fn_obten_fecha_val( ? ) "
    PREPARE eje_fecha_val FROM v_obten_fecha_viv

    DECLARE cur_obten_fecha_viv CURSOR FOR eje_fecha_val
    OPEN    cur_obten_fecha_viv USING HOY
    FETCH   cur_obten_fecha_viv INTO gd_fecha_viv
    CLOSE   cur_obten_fecha_viv

    --- APORTACIONES POSTERIORES A LA FIP AL PRIMER DECIMO ---
    LET v_ap_posteriores = " EXECUTE FUNCTION fn_aportes_posteriores(?,?,?,?) "
    PREPARE eje_aportes_pos FROM v_ap_posteriores

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        fecha_ini_pen         LIKE ret_solicitud_tx.fecha_ini_pen   ,
        diag_registro         LIKE ret_solicitud_tx.diag_registro   ,
        grupo                 LIKE ret_solicitud_tx.grupo
    END RECORD

    #DEFINE arr_siefore ARRAY [20] OF RECORD #CPL-2106
    DEFINE arr_siefore ARRAY [99] OF RECORD
                                        activo                SMALLINT      ,
                                        acciones_ret97        DECIMAL(16,6) ,
                                        acciones_cv           DECIMAL(16,6) ,
                                        acciones_cs           DECIMAL(16,6) ,
                                        acciones_est          DECIMAL(16,6) ,
                                        acciones_ret92        DECIMAL(16,6) ,
                                        acciones_esp          DECIMAL(16,6)
                                     END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        acciones_viv97        DECIMAL(16,6) ,
        acciones_viv92        DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        f_subcuenta           ,
        f_siefore             ,
        ls_siefore            , --contador para los ciclos for
        ls_cont               ,
        s_subcta              ,
        ls_siefore_act        ,
        ls_siefore_act_trasp  SMALLINT

    DEFINE #loc #int
        v_periodo_pago        INTEGER

    DEFINE #loc #decimal
        mto_traspasado        , --coppel
        f_monto_acc           ,
        f_monto_pesos         ,
        acciones_cv           ,
        ld_monto_pesos_trans  ,
        ld_precio_origen      ,
        ld_precio_destino     DECIMAL(16,6)

    DEFINE #loc #date
        ld_max_fecha_banxico        DATE



    DECLARE cur_33 CURSOR FOR
    SELECT A.nss             ,
           A.consecutivo     ,
           A.regimen         ,
           A.tipo_seguro     ,
           A.tipo_pension    ,
           A.tipo_prestacion ,
           A.fecha_ini_pen   ,
           A.diag_registro   ,
           A.grupo
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro        = gc_tipo_ret
    AND    A.estado_solicitud   IN (reg_2.confirmado, reg_2.modificado)
    AND    A.rechazo_cod        = 0
    ORDER BY nss

    FOREACH cur_33 INTO reg_5.*
        --Inicializamos variables del arreglo--
        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo         = FALSE
            LET arr_siefore[ls_siefore].acciones_ret97 = 0
            LET arr_siefore[ls_siefore].acciones_cv    = 0
            LET arr_siefore[ls_siefore].acciones_cs    = 0
            LET arr_siefore[ls_siefore].acciones_est   = 0
            LET arr_siefore[ls_siefore].acciones_ret92 = 0
            LET arr_siefore[ls_siefore].acciones_esp   = 0
        END FOR

        LET reg_13.acciones_viv97 = 0
        LET reg_13.acciones_viv92 = 0

        --Carga en tabla temporal histórico de movimiento--
        CALL genera_tmp_cuenta(reg_5.nss) #gtc

        -- Obtiene la fecha del ultimo traspaso para el nss
        SELECT MAX(fecha_mov_banxico)
        INTO   ld_max_fecha_banxico
        FROM   taa_rcv_recepcion
        WHERE  nss             = reg_5.nss
        AND    ident_operacion = "09"

        DECLARE cur_11 CURSOR FOR
        SELECT A.subcuenta
        FROM   tab_agrupa_subcta A
        WHERE  A.grupo     = reg_5.grupo
        AND    A.subcuenta > 0

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_11 INTO s_subcta
        
            CALL  f_obten_monto_traspaso(reg_5.nss, s_subcta, ld_max_fecha_banxico)
             RETURNING mto_traspasado, ls_siefore_act_trasp
            
            SELECT codigo_siefore
              INTO   ls_siefore_act
              FROM   cta_nss_regimen
              WHERE  nss = reg_5.nss
              AND    grupo_regimen = 1
             	
              SELECT precio_del_dia
              INTO   ld_precio_origen 
              FROM    glo_valor_accion
              WHERE   fecha_valuacion = "12/16/2019"
              AND     codigo_siefore  = ls_siefore_act_trasp
              
              SELECT precio_del_dia
              INTO   ld_precio_destino
              FROM    glo_valor_accion
              WHERE   fecha_valuacion = "12/16/2019"
              AND     codigo_siefore  = ls_siefore_act
            
            LET mto_traspasado = mto_traspasado * ld_precio_origen
            
            LET mto_traspasado = mto_traspasado / ld_precio_destino

            --Elimina todos los movimientos anteriores al último retiro--
            CALL f_elimina_reingresos(reg_5.nss, s_subcta)

            --- CASO 1 - FECHA INI PEN < 1997 - PAGA SALDO AL DIA  ---
            IF (reg_5.fecha_ini_pen < "07/01/1997") OR
               (s_subcta = 1 AND reg_5.regimen = 73) THEN --if01

                --Si la subcuenta es de retiro (1) y el regimen es 73 se paga la totalidad
                --de la subcuenta

                CALL f_provisiona_saldo_dia(reg_5.nss, s_subcta)
                    RETURNING f_siefore, f_subcuenta, f_monto_acc, f_monto_pesos
                    
            ELSE
                --- CASO 2 - FECHA INI PEN > 1997 - PAGA AP POSTERIORES A LA FIP ---
                CALL f_provisiona_ap_posteriores(reg_5.nss, s_subcta, reg_5.fecha_ini_pen)
                    RETURNING f_siefore, f_subcuenta, f_monto_acc
                    
                
                SELECT codigo_siefore
                INTO   ls_siefore_act
                FROM   cta_nss_regimen
                WHERE  nss = reg_5.nss
                AND    grupo_regimen = 1
               	
                SELECT precio_del_dia
                INTO   ld_precio_origen 
                FROM    glo_valor_accion
                WHERE   fecha_valuacion = "12/16/2019"
                AND     codigo_siefore  = f_siefore
                
                SELECT precio_del_dia
                INTO   ld_precio_destino
                FROM    glo_valor_accion
                WHERE   fecha_valuacion = "12/16/2019"
                AND     codigo_siefore  = ls_siefore_act

               IF (ls_siefore_act <> f_siefore) AND (f_siefore <> 11 ) THEN --CPL-2614
                
                  LET f_monto_acc = f_monto_acc * ld_precio_origen -- se convierten acciones a pesos el dia del corte tdf
                  LET f_monto_acc = f_monto_acc /ld_precio_destino --se convierten los pesos a las acciones del corte tdf nueva siefore
                  
                  DISPLAY " "
                  
                  LET f_siefore      = ls_siefore_act
               END IF

                -- Coppel
                -- Se obtiene el monto del traspaso segun lo capturado en el programa RETC847
                SELECT "OK"
                FROM   ret_pago_reingreso
                WHERE  nss           = reg_5.nss
                AND    consecutivo   = reg_5.consecutivo
                AND    pago_traspaso = 1
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                                         
                    IF mto_traspasado > 0 THEN
                        LET f_monto_acc = f_monto_acc + mto_traspasado
                    END IF
                  
                    DISPLAY " "

                END IF

                LET f_monto_acc = f_verifica_sobregiro(reg_5.nss, f_subcuenta, f_monto_acc)
            END IF

            IF f_siefore = 11 THEN
                --El elemento gs_viv del arreglo de precios contiene la informacion de la siefore 11--
                --Si la siefore es 11 se debe redondear el valor en pesos a dos decimales
                LET f_monto_acc    = redondea_val(f_monto_acc, 2)
                LET f_monto_pesos  = redondea_val(f_monto_acc * gar_precio_acc[gs_viv].precio_dia, 2)
            ELSE
                IF f_siefore <> 0 THEN
                    LET f_monto_pesos = f_monto_acc * gar_precio_acc[f_siefore].precio_dia
                END IF
            END IF
            IF f_monto_acc > 0 THEN  --if06

                IF f_siefore <> 11 THEN --if07
                    --Marcamos como activo el registro de la siefore actual--
                    LET arr_siefore[f_siefore].activo = TRUE
                        CASE f_subcuenta
                            WHEN 1
                                LET arr_siefore[f_siefore].acciones_ret97 = f_monto_acc
                            WHEN 2
                                LET arr_siefore[f_siefore].acciones_cv    = f_monto_acc
                            WHEN 5
                                LET arr_siefore[f_siefore].acciones_cs    = f_monto_acc
                            WHEN 6
                                LET arr_siefore[f_siefore].acciones_est   = f_monto_acc
                            WHEN 7
                                LET arr_siefore[f_siefore].acciones_ret92 = f_monto_acc
                            WHEN 9
                                LET arr_siefore[f_siefore].acciones_esp   = f_monto_acc
                            OTHERWISE
                                --Para evitar que se provisionen subcuentas que no corresponden a este tipo de retiro
                                LET f_monto_acc = 0
                        END CASE
                ELSE --if07
                    CASE f_subcuenta
                        WHEN 4
                            LET reg_13.acciones_viv97 = f_monto_acc
                        WHEN 8
                            LET reg_13.acciones_viv92 = f_monto_acc
                    END CASE
                END IF --if07

                CALL f_provisiona_subcta(reg_5.nss         ,
                                         f_siefore         ,
                                         f_subcuenta       ,
                                         reg_5.consecutivo ,
                                         f_monto_acc       ,
                                         f_monto_pesos     )

            END IF
        END FOREACH -- Subcuentas

        #FOR ls_siefore = 1 TO gs_num_siefores  #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET acciones_cv = 0
            --Almacenamos solo los registros de las siefores que contengan saldo--
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                LET acciones_cv = arr_siefore[ls_siefore].acciones_cv  +
                                  arr_siefore[ls_siefore].acciones_est +
                                  arr_siefore[ls_siefore].acciones_esp

                INSERT INTO ret_monto_siefore
                VALUES(reg_5.nss                              ,--nss
                       reg_5.consecutivo                      ,--consecutivo
                       gi_ult_folio                           ,--folio
                       gc_tipo_ret                            ,--tipo_retiro
                       5                                      ,--tipo_operacion
                       ls_siefore                             ,--siefore
                       arr_siefore[ls_siefore].acciones_ret97 ,--acciones_ret97
                       acciones_cv                            ,--acciones_cv
                       arr_siefore[ls_siefore].acciones_cs    ,--acciones_cs
                       arr_siefore[ls_siefore].acciones_ret92  --acciones_ret92
                      )
            END IF
        END FOR

        INSERT INTO ret_monto_viv
        VALUES(reg_5.nss             ,--nss
               reg_5.consecutivo     ,--consecutivo
               gi_ult_folio          ,--folio
               gc_tipo_ret           ,--tipo_retiro
               gd_fecha_viv          ,--fecha_valor_viv
               reg_13.acciones_viv97 ,--acciones_viv97
               reg_13.acciones_viv92 ,--acciones_viv92
               0                     ,--pesos_viv72
               NULL                  ,--estado_sub_viv
               0                     ,--acc_viv97_bdsviv
               0                      --acc_viv92_bdsviv
              )

        --Obtenemos el periodo de pago que se ingresa a ret_solicitud_tx
        CALL f_periodo_pago(reg_5.nss, reg_5.fecha_ini_pen)
            RETURNING v_periodo_pago

        IF v_periodo_pago IS NULL OR v_periodo_pago = " " THEN
            LET v_periodo_pago = 0
        END IF

        UPDATE ret_solicitud_tx
        SET    folio            = gi_ult_folio          ,
               saldo_viv97      = reg_13.acciones_viv97 ,
               saldo_viv92      = reg_13.acciones_viv92 ,
               saldo_viv72      = 0                     ,
               fecha_envio      = HOY                   ,
               fecha_valor_viv  = gd_fecha_viv          ,
               periodo_pago     = v_periodo_pago        ,
               estado_solicitud = reg_2.procesado
        WHERE  nss              = reg_5.nss
        AND    consecutivo      = reg_5.consecutivo
        AND    estado_solicitud IN (reg_2.confirmado, reg_2.modificado)

    END FOREACH -- Siguiente NSS

END FUNCTION


FUNCTION segundo_paso()
#cp-------------------
    DEFINE #loc #reg_6
        reg_6           RECORD LIKE ret_solicitud_tx.*

    DEFINE
        cont_det_pe05   INTEGER

    DEFINE
        ls_num_montos   SMALLINT

    DEFINE
        ruta_det_nss    ,
        ruta_det_sie    ,
        ruta_det_viv    CHAR(100)

    DEFINE reg_9 RECORD #loc #reg_9
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) ,
        acciones_ret92        DECIMAL(16,6)
    END RECORD

    DEFINE lr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_param_ret.ruta_envio CLIPPED, "/", "DET-NSS-J-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_param_ret.ruta_envio CLIPPED, "/", "DET-SIE-J-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = g_param_ret.ruta_envio CLIPPED, "/", "DET-VIV-J-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET  = g_param_ret.ruta_envio CLIPPED, "/", "DET-J"
    LET G_LISTA_DET  = G_LISTA_DET CLIPPED

    LET cont_reg        = 0
    LET cont_det_pe05   = 0

    DECLARE cur_5 CURSOR FOR
    SELECT  *
    FROM    ret_solicitud_tx
    WHERE   ret_solicitud_tx.folio            = gi_ult_folio
    AND     ret_solicitud_tx.estado_solicitud = reg_2.procesado
    AND     ret_solicitud_tx.tipo_prestacion  = 10

    FOREACH cur_5 INTO reg_6.*

        #-- Iniciamos los reportes
        START REPORT det_solicitudes_03 TO ruta_det_nss
        START REPORT det_vivienda_05 TO ruta_det_viv

        LET cont_det_pe05 = cont_det_pe05 + 1
        LET cont_reg      = cont_reg + 1
        LET ls_num_montos = 0

        IF reg_6.periodo_pago = 0 THEN
            LET reg_6.periodo_pago = "000000"
        END IF

        #CPL-2430
        INITIALIZE lr_info_biometrica.* TO NULL
        LET lr_info_biometrica.identificador = 1 --IMSS
        LET lr_info_biometrica.tiposervicio  = 2 --TOTAL

        #Se obtiene informacion biometrica
        SELECT LPAD(idsolicitante,2,0) idsolicitante,
               curpsolicitante                      ,
               LPAD(NVL(sellotrabajador,0),14,0)    ,
               curpagenteservicio
        INTO   lr_info_biometrica.idsolicitante     ,
               lr_info_biometrica.curpsolicitante   ,
               lr_info_biometrica.sellotrabajador   ,
               lr_info_biometrica.curpagenteservicio
        FROM   ret_sellosbiometricos_suv
        WHERE  identificador = lr_info_biometrica.identificador
        AND    tiposervicio  = lr_info_biometrica.tiposervicio
        AND    nss           = reg_6.nss
        AND    consecutivo   = reg_6.consecutivo

        IF lr_info_biometrica.idsolicitante IS NULL THEN
           LET lr_info_biometrica.idsolicitante   = '00'
           LET lr_info_biometrica.sellotrabajador = '00000000000000'
        END IF

        OUTPUT TO REPORT det_solicitudes_03(reg_6.*, lr_info_biometrica.*)
        OUTPUT TO REPORT det_vivienda_05(reg_6.*)

        SELECT COUNT(*)
            INTO   ls_num_montos
            FROM   ret_monto_siefore
            WHERE  folio          = gi_ult_folio
            AND    nss            = reg_6.nss
            AND    consecutivo    = reg_6.consecutivo
            AND    tipo_operacion = 5

        #-- Si existen registros de saldo en siefores para el nss actual,
        #-- los barremos para generar el reporte del detalle de siefores
        IF ls_num_montos > 0 THEN
            START REPORT det_siefores_04 TO ruta_det_sie

            DECLARE cur_6 CURSOR FOR
                SELECT siefore        ,
                       acciones_ret97 ,
                       acciones_cv    ,
                       acciones_cs    ,
                       acciones_ret92
                FROM   ret_monto_siefore
                WHERE  folio          = gi_ult_folio
                AND    nss            = reg_6.nss
                AND    consecutivo    = reg_6.consecutivo
                AND    tipo_operacion = 5

            FOREACH cur_6 INTO reg_9.*
                OUTPUT TO REPORT det_siefores_04(reg_6.nss, reg_6.curp, reg_9.*)
            END FOREACH

            FINISH REPORT det_siefores_04
        END IF

        FINISH REPORT det_solicitudes_03
        FINISH REPORT det_vivienda_05

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss,
                             ruta_det_sie,
                             ruta_det_viv,
                             ls_num_montos,
                             cont_reg)

        UPDATE ret_solicitud_tx
        SET    ret_solicitud_tx.estado_solicitud = reg_2.procesado
        WHERE  ret_solicitud_tx.folio            = gi_ult_folio
        AND    ret_solicitud_tx.consecutivo      = reg_6.consecutivo

    END FOREACH

    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando


    INSERT INTO ret_ctr_envio_lote
    VALUES (HOY              ,#fecha_genera
            gc_tipo_ret              ,#tipo_retiro
            gi_ult_folio     ,#folio
            NULL             ,#fecha_envio
            NULL             ,#fceha_reverso
            HORA             ,#hora_genera
            NULL             ,#hora_envio
            gc_usuario          ,#usuario_genera
            NULL             ,#usuario_envio
            NULL             ,#usuario reverso
            reg_2.procesado  ,#estado
            cont_det_pe05     #total_registros
           )

END FUNCTION

FUNCTION concat_reportes(lc_det_nss, lc_det_sie, lc_det_viv, p_monto, p_regs)

    DEFINE
        lc_det_nss      ,
        lc_det_sie      ,
        lc_det_viv      CHAR(100)

    DEFINE p_monto SMALLINT
    DEFINE p_regs SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = g_param_ret.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_param_ret.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_nss CLIPPED, " ", lc_det_viv CLIPPED

    #-- Si se genero el reporte de saldo de siefores, se incluye en los comandos
    IF p_monto > 0 THEN

        LET com_cat = "cat ", lc_det_nss CLIPPED, " ",
                              lc_det_sie CLIPPED, " ",
                              lc_det_viv CLIPPED, " > ", ruta_tmp

        LET com_cat = com_cat CLIPPED

        LET com_rm = com_rm CLIPPED, " ", lc_det_sie CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_nss CLIPPED, " ",
                              lc_det_viv CLIPPED, " > ", ruta_tmp CLIPPED
    END IF

    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN com_cat
    RUN com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET com_cat = "cat ", G_LISTA_DET, " ", ruta_tmp, " > ", ruta_det
        RUN com_cat

        LET com_cat = " mv ", ruta_det, " ", G_LISTA_DET
        RUN com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET com_cat = " cp ", ruta_tmp, " ", G_LISTA_DET
        RUN com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET com_rm = "rm ", ruta_tmp
    RUN com_rm

END FUNCTION

FUNCTION genera_tmp_cuenta (p_nss)
#gtc------------------------------
    DEFINE #loc #char
        p_nss                 CHAR(0011) ,
        sel_his               CHAR(2000) ,
        v_nombre_tabla        CHAR(0020)

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta;
        SELECT * 
        FROM   dis_cuenta
        WHERE  1=2 INTO TEMP tmp_dis_cuenta
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
    SELECT tabname
    FROM   systables
    WHERE  tabname matches "dis_cuenta??"
    OR     tabname =  "dis_cuenta"

    FOREACH cur_his INTO v_nombre_tabla
      
      LET sel_his =  " INSERT INTO tmp_dis_cuenta            \n",
                        " SELECT   *                         \n",
                        " FROM     ",v_nombre_tabla CLIPPED ,"\n",
                        " WHERE    nss         =      ?      \n",
                        " AND tipo_movimiento NOT IN (888,999) "

      LET sel_his = sel_his CLIPPED
      PREPARE sql_totales FROM sel_his
      EXECUTE sql_totales USING p_nss

    END FOREACH

    CLOSE cur_his

    CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta
    (folio            ,
     consecutivo_lote ,
     subcuenta        ,
     siefore
    )
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION


FUNCTION obtiene_precios_accion(fecha_precios)
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
        ls_siefore            SMALLINT

    DEFINE #loc #smallint
        li_cont               SMALLINT

    LET li_cont = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*
      LET ls_siefore = lr_precio_acc.siefore
        IF lr_precio_acc.estado <> 0 THEN

            LET lc_mensaje = " FALTAN PRECIOS ACCION DIA ", fecha_precios, " < ENTER > PARA SALIR "
            LET lc_mensaje = lc_mensaje CLIPPED
            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            IF lr_precio_acc.siefore = 11 THEN
                LET gar_precio_acc[gs_viv].* = lr_precio_acc.*
            ELSE
                LET gar_precio_acc[ls_siefore].* = lr_precio_acc.*
            END IF
        END IF
        LET li_cont = li_cont + 1
    END FOREACH
END FUNCTION

FUNCTION f_periodo_pago(reg_10)
#pp-------------------------
    DEFINE reg_10 RECORD #loc #reg_10
        nss                   CHAR(11) ,
        fecha_pension         DATE
    END RECORD

    DEFINE reg_15 RECORD #loc #reg_15
        periodo_pago          CHAR(6) ,
        folio                 INTEGER ,
        consecutivo           INTEGER
    END RECORD

    DEFINE #loc #char
        periodo               CHAR(6)

    DEFINE #loc #smallint
        residuo               ,
        mes                   ,
        ano                   ,
        cont_aportaciones     SMALLINT


    --DETERMINA EL AÑO Y EL BIMESTRE DE CADA APORTACION--------------
    LET mes = MONTH(reg_10.fecha_pension)
    LET ano = YEAR (reg_10.fecha_pension)

    LET residuo = mes MOD 2

    IF residuo > 0 THEN
        LET mes = mes + 1
    END IF

    LET periodo = ano USING "&&&&",mes USING "&&"

    DECLARE cur_7 CURSOR FOR
    SELECT A.periodo_pago    ,
           A.folio           ,
           A.consec_reg_lote
    FROM   dis_det_aporte A
    WHERE  A.n_seguro     = reg_10.nss
    AND    A.periodo_pago > periodo
    ORDER BY 1

    FOREACH cur_7 INTO reg_15.*
        LET cont_aportaciones = 0

        SELECT COUNT(*)
        INTO   cont_aportaciones
        FROM   tmp_dis_cuenta
        WHERE  nss              = reg_10.nss
        AND    folio            = reg_15.folio
        AND    consecutivo_lote = reg_15.consecutivo
        AND    tipo_movimiento IN (1,2,3)

        IF cont_aportaciones > 0 THEN
            EXIT FOREACH
        ELSE
            LET reg_15.periodo_pago = 0
            CONTINUE FOREACH
        END IF
    END FOREACH

    RETURN reg_15.periodo_pago

END FUNCTION

#----------------------------------------------------------------------------------
# Obtiene el monto que se debe provisionar en la subcuenta dada cuando se deba
# pagar el saldo al dia de la subcuenta.
#----------------------------------------------------------------------------------
FUNCTION f_provisiona_saldo_dia(p_nss, p_subcta)

    DEFINE p_nss LIKE ret_solicitud_tx.nss

    DEFINE #loc #smallint
        p_subcta            ,
        ls_siefore          ,
        ls_subcta           SMALLINT

    DEFINE #loc #decimal
        ld_monto_acc         ,
        ld_monto_pes         DECIMAL(16,6)

    DEFINE #loc #date
        ld_fecha_saldo        DATE

    -- Determinamos a que fecha se va a obtener el saldo
    IF (p_subcta <> 4) AND (p_subcta <> 8) THEN
        LET ld_fecha_saldo = HOY
    ELSE
        --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes
        IF MONTH(gd_fecha_viv) = MONTH(TODAY) THEN
            LET ld_fecha_saldo = gd_fecha_viv
        ELSE
           -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
           LET ld_fecha_saldo = HOY
        END IF
    END IF

    CALL f_obten_saldo_subcta(p_nss, p_subcta, ld_fecha_saldo)
        RETURNING ls_siefore, ls_subcta, ld_monto_acc, ld_monto_pes

    IF (p_subcta = 4) OR (p_subcta = 8) THEN
        LET ld_monto_acc  = redondea_val(ld_monto_acc, 2)
        LET ld_monto_pes  = redondea_val(ld_monto_pes, 2)
    END IF

    RETURN ls_siefore, ls_subcta, ld_monto_acc, ld_monto_pes

END FUNCTION

#----------------------------------------------------------------------------------
# Obtiene el monto que se debe provisionar en la subcuenta dada cuando se deba
# pagar las aportaciones posteriores al bimestre de la fip de la subcuenta.
#----------------------------------------------------------------------------------
FUNCTION f_provisiona_ap_posteriores(p_nss, p_subcta, p_fip)

    DEFINE p_nss LIKE ret_solicitud_tx.nss

    DEFINE #loc #smallint
        p_subcta            ,
        ls_siefore          ,
        ls_subcta           SMALLINT

    DEFINE #loc #date
        ld_fecha_transf     ,
        p_fip               DATE


    DEFINE #loc #decimal
        ld_monto_acc         DECIMAL(16,6)

    LET ld_monto_acc = 0
    LET ls_siefore   = 0

    DECLARE c_aportes_pos CURSOR FOR eje_aportes_pos

    OPEN c_aportes_pos  USING p_nss      ,
                              ls_siefore , --se está mandando en cero
                              p_subcta   ,
                              p_fip

    FETCH c_aportes_pos INTO  ls_siefore    ,
                              ls_subcta     ,
                              ld_monto_acc  --Monto de pago

    CLOSE c_aportes_pos

    RETURN ls_siefore, ls_subcta, ld_monto_acc

END FUNCTION


#----------------------------------------------------------------------------------
# Obtiene el monto de la subcuenta si el nss proviene de un traspaso
#----------------------------------------------------------------------------------
FUNCTION f_obten_monto_traspaso(p_nss, p_subcta, p_fecha_traspaso)

    DEFINE p_nss LIKE ret_solicitud_tx.nss

    DEFINE #loc #smallint
        p_subcta              SMALLINT,
        ls_siefore            SMALLINT 

    DEFINE #loc #date
        p_fecha_traspaso      DATE

    DEFINE #loc #decimal
        ld_mto_traspaso       DECIMAL(16,6)


    LET ld_mto_traspaso = 0

    SELECT SUM(A.monto_en_acciones), siefore
    INTO   ld_mto_traspaso , ls_siefore
    FROM   tmp_dis_cuenta A,
           taa_rcv_recepcion B
    WHERE  B.nss       = p_nss
    AND    A.nss       = B.nss
    AND    B.fecha_mov_banxico >= p_fecha_traspaso
    AND    A.folio     = B.folio
    AND    A.subcuenta = p_subcta
    GROUP BY siefore

    RETURN ld_mto_traspaso, ls_siefore

END FUNCTION


FUNCTION f_elimina_reingresos(pc_nss, ls_subcta)

    DEFINE pc_nss LIKE ret_solicitud_tx.nss

    DEFINE li_folio LIKE ret_solicitud_tx.folio

    DEFINE
        ls_num_reingreso        SMALLINT,
        ls_subcta               SMALLINT

    DEFINE
        ldt_ult_reingreso       DATE,
        ld_fecha_subcuenta      DATE 

    -- -----------------------------------------------------------------------------

    LET ls_num_reingreso = 0

    SELECT NVL(COUNT(*), 0)
    INTO   ls_num_reingreso
    FROM   tmp_dis_cuenta
    WHERE  nss             = pc_nss
    AND    tipo_movimiento = gs_tipo_movimiento
    AND subcuenta = ls_subcta


    IF (ls_num_reingreso > 0) THEN

        SELECT MAX(fecha_conversion)                                            --CPL-1863
        INTO   ldt_ult_reingreso
        FROM   tmp_dis_cuenta
        WHERE  nss             = pc_nss
        AND    tipo_movimiento = gs_tipo_movimiento
        AND subcuenta = ls_subcta

        SELECT folio                                                            --CPL-1863
        INTO   li_folio
        FROM   tmp_dis_cuenta
        WHERE  nss             = pc_nss
        AND    tipo_movimiento = gs_tipo_movimiento
        AND    fecha_conversion = ldt_ult_reingreso
        AND subcuenta = ls_subcta
        GROUP BY 1
        -- CPL-1863
        -- Si el diagnostico de PROCESAR del reintegro anterior es 400
        -- se eliminan los registros de vivienda
        SELECT "OK"
        FROM   ret_solicitud_tx
        WHERE  nss             = pc_nss
        AND    tipo_retiro      = gc_tipo_ret
        AND    folio            = li_folio
        AND    diag_registro    = 400
        GROUP BY 1

        IF (STATUS <> NOTFOUND) THEN
            DELETE
            FROM   tmp_dis_cuenta
            WHERE  nss              = pc_nss
            AND    fecha_conversion < ldt_ult_reingreso - 1
            AND    siefore          = 11
        END IF

        DELETE
        FROM   tmp_dis_cuenta
        WHERE  nss              = pc_nss
        AND    fecha_conversion < ldt_ult_reingreso - 1
        AND    subcuenta       = ls_subcta
        AND    siefore         <> 11

    END IF

END FUNCTION

FUNCTION f_verifica_sobregiro(p_nss, p_subcuenta, p_monto)

    DEFINE
        p_nss        CHAR(11)


    DEFINE
        ls_grupo    ,
        f_siefore   ,
        f_subcuenta ,
        p_subcuenta SMALLINT

    DEFINE
        f_monto_acc ,
        f_monto_pesos,
        p_monto     DECIMAL(16,6)


    LET ls_grupo      = 0
    LET f_monto_acc   = 0
    LET f_monto_pesos = 0


    DECLARE cur_03 CURSOR FOR eje_saldo_dia


    OPEN cur_03 USING p_nss       ,
                      p_subcuenta ,
                      ls_grupo    ,
                      HOY

    FETCH cur_03 INTO f_subcuenta ,
                      f_siefore   ,
                      f_monto_acc ,
                      f_monto_pesos

    CLOSE cur_03

    IF p_monto > f_monto_acc THEN
        LET p_monto = f_monto_acc
    END IF

    RETURN p_monto

END FUNCTION

#----------------------------------------------------------------------------------
# Obtiene el saldo del nss de la subcuenta indicada a la fecha dada por p_fecha_saldo
#----------------------------------------------------------------------------------
FUNCTION f_obten_saldo_subcta(p_nss, p_subcuenta, p_fecha_saldo)

    DEFINE p_nss LIKE ret_solicitud_tx.nss

    DEFINE
        p_fecha_saldo         DATE

    DEFINE
        ls_subcta             ,
        ls_sie                ,
        ls_grupo              ,
        p_subcuenta           ,
        ls_cont               SMALLINT

    DEFINE
        ld_saldo_dia_viv      ,
        ld_monto_acc          ,
        ld_monto_pes          DECIMAL(16,6)


    LET ls_grupo  = 0
    LET ls_subcta = 0
    LET ls_sie    = 0

    DECLARE c_saldo_subcta CURSOR FOR eje_saldo_dia

    OPEN c_saldo_subcta USING p_nss        ,
                              p_subcuenta  ,
                              ls_grupo     ,
                              p_fecha_saldo

    FETCH c_saldo_subcta INTO ls_subcta    ,
                              ls_sie       ,
                              ld_monto_acc ,
                              ld_monto_pes

    CLOSE c_saldo_subcta

    IF ld_monto_acc IS NULL OR ld_monto_acc = 0 THEN
        --Si no tiene saldo en la subcuenta se manda el saldo como cero
        LET ld_monto_acc = 0
        LET ld_monto_pes = 0
    END IF

    -- Verificamos si no existe un sobregiro en vivienda
    IF ls_sie = 11 THEN

        LET ld_saldo_dia_viv = 0

        SELECT SUM(monto_en_acciones)
        INTO   ld_saldo_dia_viv
        FROM   dis_cuenta
        WHERE  nss       = p_nss
        AND    siefore   = 11
        AND    subcuenta = p_subcuenta

        IF ld_saldo_dia_viv < 0 OR ld_saldo_dia_viv IS NULL THEN
            LET ld_saldo_dia_viv = 0
        END IF

        -- Si lo que se obtiene al primer dia natural es mayor a lo que hay
        -- actualmente en la cuenta individual, tomamos el saldo al dia
        IF ld_monto_acc > ld_saldo_dia_viv THEN
            LET ld_monto_acc = ld_saldo_dia_viv
        END IF
    END IF

    RETURN ls_sie, ls_subcta, ld_monto_acc, ld_monto_pes

END FUNCTION


FUNCTION f_provisiona_subcta(p_nss, p_siefore, p_subcta, p_consecu, p_acciones, p_pesos)

    DEFINE p_nss     LIKE ret_solicitud_tx.nss
    DEFINE p_consecu LIKE ret_solicitud_tx.consecutivo

    DEFINE
        p_siefore             ,
        p_subcta              ,
        si_provisiono         SMALLINT

    DEFINE
        ld_acc              ,
        p_acciones          ,
        ld_pesos            ,
        p_pesos             DECIMAL(16,6)


    DEFINE #loc #char
        folio_sua             CHAR(06) ,
        id_aporte             CHAR(06)



    LET si_provisiono = 0
    LET folio_sua     = ""
    LET id_aporte     = "RETIRO"


    LET ld_acc     = -p_acciones
    LET ld_pesos   = -p_pesos

    DECLARE cur_prov CURSOR FOR eje_provisiona
    OPEN cur_prov USING gi_ult_folio        ,--folio
                        folio_sua           ,--folio_sua
                        p_nss               ,--nss
                        p_subcta            ,--subcuenta
                        gs_tipo_movimiento   ,--tipo_movimiento
                        p_consecu           ,--consecutivo_lote
                        p_siefore           ,--siefore
                        ld_acc              ,--monto_en_acciones
                        ld_pesos            ,--monto_en_pesos
                        id_aporte           ,--id_aportante
                        HOY                  --fecha proceso

    FETCH cur_prov INTO si_provisiono

    IF si_provisiono < 0 THEN
        PROMPT "El NSS :",p_nss CLIPPED," NO PROVISIONO LA SUBCTA ", p_subcta FOR CHAR enter
    END IF

    CLOSE cur_prov

END FUNCTION


FUNCTION redondea_val(p_monto_redondear, p_redondea)

    DEFINE
        p_monto_redondear DECIMAL(16,6)

    DEFINE
        p_redondea       SMALLINT


    DEFINE
        ls_monto_return   DECIMAL(16,2)

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING p_monto_redondear, p_redondea
    FETCH round_cur INTO ls_monto_return

    CLOSE round_cur

    RETURN ls_monto_return


END FUNCTION


REPORT det_solicitudes_03(reg_7, pr_info_biometrica)  #detreg05
#detpe16---------------------------------------------
    DEFINE #loc #reg_7
        reg_7                 RECORD LIKE ret_solicitud_tx.*

    DEFINE reg_8 RECORD  #loc #reg_8
        paterno               LIKE afi_mae_afiliado.paterno ,
        materno               LIKE afi_mae_afiliado.materno ,
        nombres               LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE #loc #char
        v_sec_pension         CHAR(02) ,
        vmax_sec_pension      CHAR(02)

    DEFINE #loc #integer
        vmax_folio            INTEGER

    DEFINE pr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            SELECT MAX(folio)
            INTO   vmax_folio
            FROM   ret_det_datamart
            WHERE  nss            = reg_7.nss
            AND    tipo_seguro    = "IV"
            AND    tipo_pension  IN ("VE","CE")
            AND    regimen        = reg_7.regimen
            AND    fecha_ini_pen  = reg_7.fecha_ini_pen
            AND    diag_datamart IN (101,106,300,301)

            SELECT MAX(sec_pension)
            INTO   vmax_sec_pension
            FROM   ret_det_datamart
            WHERE  nss            = reg_7.nss
            AND    tipo_seguro    = "IV"
            AND    tipo_pension  IN ("VE","CE")
            AND    regimen        = reg_7.regimen
            AND    fecha_ini_pen  = reg_7.fecha_ini_pen
            AND    diag_datamart IN (101,106,300,301)
            AND    folio          = vmax_folio

            SELECT sec_pension
            INTO   v_sec_pension
            FROM   ret_det_datamart
            WHERE  nss            = reg_7.nss
            AND    tipo_seguro    = "IV"
            AND    tipo_pension  IN ("VE","CE")
            AND    regimen        = reg_7.regimen
            AND    fecha_ini_pen  = reg_7.fecha_ini_pen
            AND    diag_datamart IN (101,106,300,301)
            AND    sec_pension    = vmax_sec_pension
            AND    folio          = vmax_folio

            IF STATUS = NOTFOUND THEN
                LET v_sec_pension = 2 SPACES
            END IF

            SELECT paterno       ,
                   materno       ,
                   nombres
            INTO   reg_8.paterno ,
                   reg_8.materno ,
                   reg_8.nombres
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_7.nss

    PRINT
        COLUMN 001, "03"                                     , #tipo_registro
        COLUMN 003, "04"                                     , #ident_servicio
        COLUMN 005, "05"                                     , #ident_operacion
        COLUMN 007, reg_7.nss                                ,
        COLUMN 018, reg_7.curp                               ,
        COLUMN 036, reg_8.nombres                            ,
        COLUMN 076, reg_8.paterno                            ,
        COLUMN 116, reg_8.materno                            ,
        COLUMN 156, v_sec_pension                            , #sec de pension
        COLUMN 158, reg_7.tipo_retiro                        ,
        COLUMN 159, reg_7.regimen                            ,
        COLUMN 161, reg_7.tipo_seguro                        ,
        COLUMN 163, reg_7.tipo_pension                       ,
        COLUMN 165, reg_7.tipo_prestacion  USING "&&"        ,
        COLUMN 167, reg_7.fecha_ini_pen    USING "YYYYMMDD"  ,
        COLUMN 175, reg_7.fecha_resolucion USING "YYYYMMDD"  ,
        COLUMN 183, 00000                  USING "&&&&&"     , #porcentaje va
        COLUMN 188, 0000                   USING "&&&&"      , #semanas cotiz
        COLUMN 192, reg_7.fecha_solicitud  USING "YYYYMMDD"  ,
        COLUMN 200, 1 SPACES                                 , #cve doc proba
        COLUMN 201, "00010101"                               , #f.nacimiento
        COLUMN 209, 3 SPACES                                 , #aseguradora
        COLUMN 212, 7 SPACES                                 , #actuario
        COLUMN 219, 8 SPACES                                 , #plan privado
        COLUMN 227, reg_7.periodo_pago     USING "&&&&&&"    , #periodo pag
        COLUMN 233, reg_7.consecutivo      USING "&&&&&&&&&&&",
        #CPL-2430
        COLUMN 244, pr_info_biometrica.idsolicitante          , --ID Solicitante
        COLUMN 246, pr_info_biometrica.curpsolicitante        , --CURP Solicitante
        COLUMN 264, pr_info_biometrica.sellotrabajador        , --Sello único de verificación
        COLUMN 278, pr_info_biometrica.curpagenteservicio     , --CURP Agente de Servicio
        COLUMN 296, 485 SPACES                                  --Campos 31 a 48 Filler LAYOUT(780 posiciones)

END REPORT


#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de siefore
#---------------------------------------------------------------------------
REPORT det_siefores_04(p_nss, p_curp, reg_11)
#ds04----------------------------

    DEFINE p_nss  LIKE ret_solicitud_tx.nss
    DEFINE p_curp LIKE ret_solicitud_tx.curp

    DEFINE reg_11 RECORD #loc #reg_11
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) ,
        acciones_ret92        DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        c15_acc_ret97       CHAR(15) ,
        c14_acc_ret97       CHAR(14) ,
        c15_acc_cv          CHAR(15) ,
        c14_acc_cv          CHAR(14) ,
        c15_acc_cs          CHAR(15) ,
        c14_acc_cs          CHAR(14) ,
        c15_acc_ret92       CHAR(15) ,
        c14_acc_ret92       CHAR(14)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos el valor de las Acciones de Retiro 97
            IF reg_11.acciones_ret97 IS NULL THEN
                LET reg_11.acciones_ret97 = 0
            END IF

            LET c15_acc_ret97           = reg_11.acciones_ret97 USING"&&&&&&&&.&&&&&&"
            LET c14_acc_ret97           = c15_acc_ret97[01,08],
                                          c15_acc_ret97[10,15]

            #-- Obtenemos el valor de las Acciones de CV
            IF reg_11.acciones_cv IS NULL THEN
                LET reg_11.acciones_cv = 0
            END IF

            LET c15_acc_cv              = reg_11.acciones_cv USING"&&&&&&&&.&&&&&&"
            LET c14_acc_cv              = c15_acc_cv[01,08],
                                          c15_acc_cv[10,15]

            #-- Obtenemos el valor de las Acciones de CS
            IF reg_11.acciones_cs IS NULL THEN
                LET reg_11.acciones_cs = 0
            END IF

            LET c15_acc_cs              = reg_11.acciones_cs USING"&&&&&&&&.&&&&&&"
            LET c14_acc_cs              = c15_acc_cs[01,08],
                                          c15_acc_cs[10,15]

            #-- Obtenemos el valor de las Acciones de Retiro 92
            IF reg_11.acciones_ret92 IS NULL THEN
                LET reg_11.acciones_ret92 = 0
            END IF

            LET c15_acc_ret92           = reg_11.acciones_ret92 USING"&&&&&&&&.&&&&&&"
            LET c14_acc_ret92           = c15_acc_ret92[01,08],
                                          c15_acc_ret92[10,15]

        PRINT
            COLUMN 001, "04"                                   ,# Tipo de registro
            COLUMN 003, p_nss                                  ,
            COLUMN 014, p_curp                                 ,
            COLUMN 032, reg_11.siefore USING "&&"              ,# Clave de siefore
            COLUMN 034, c14_acc_ret97                          ,# Acciones de retiro 97
            COLUMN 048, c14_acc_cv                             ,# Acciones de CV
            COLUMN 062, c14_acc_cs                             ,# Acciones de CS
            COLUMN 076, c14_acc_ret92                          ,# Acciones de retiro 92
            COLUMN 090, 691 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout

END REPORT

#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de vivienda
#---------------------------------------------------------------------------
REPORT det_vivienda_05(reg_7)
#dv05----------------------------

    DEFINE #loc #reg_7
        reg_7               RECORD LIKE ret_solicitud_tx.*

    DEFINE #loc #dec
       d2_saldo_viv97       ,
       d2_saldo_viv92       DECIMAL(14,6)

    DEFINE #loc #char
        c8_fecha_val        CHAR(08) ,
        c10_fecha_val       CHAR(10) ,
        c15_impt_viv_97     CHAR(15) ,
        c14_impt_viv_97     CHAR(14) ,
        c15_impt_viv_92     CHAR(15) ,
        c14_impt_viv_92     CHAR(14)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos la fecha Valor de Vivienda
            LET c10_fecha_val      = reg_7.fecha_valor_viv
            LET c8_fecha_val       = c10_fecha_val[7,10] ,
                                     c10_fecha_val[1,02] ,
                                     c10_fecha_val[4,05]

            #-- Obtenemos aplicacion de intereses viv. 97
            LET d2_saldo_viv97     = reg_7.saldo_viv97
            LET c15_impt_viv_97    = d2_saldo_viv97 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_97    = c15_impt_viv_97[01,08],
                                     c15_impt_viv_97[10,15]

            #-- Obtenemos aplicacion de intereses viv. 92
            LET d2_saldo_viv92     = reg_7.saldo_viv92
            LET c15_impt_viv_92    = d2_saldo_viv92 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_92    = c15_impt_viv_92[01,08],
                                     c15_impt_viv_92[10,15]

        PRINT
            COLUMN 001, "05"                                   ,# Tipo de registro
            COLUMN 003, reg_7.nss                              ,
            COLUMN 014, reg_7.curp                             ,
            COLUMN 032, c8_fecha_val                           ,# Fecha valor vivienda
            COLUMN 040, c14_impt_viv_97                        ,# Intereses Viv 97
            COLUMN 054, c14_impt_viv_92                        ,# Intereses Viv 92
            COLUMN 068, "00000000000000"                       ,# Fondo Viv 72
            #--                                                   Estatus de la subcta vivienda
            COLUMN 083, "00000000000000"                       ,# Intereses Viv 97 en BDSVIV
            COLUMN 097, "00000000000000"                       ,# Intereses Viv 92 en BDSVIV
            COLUMN 111, 670 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout

END REPORT

