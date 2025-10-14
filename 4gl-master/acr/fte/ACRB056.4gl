#######################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                     #
#Propietario       => E.F.P.                                          #
#Programa ACRB056  => INCORPORAR REGISTROS DE DEVOLUCION              #
#                     DE ANUALIDADES GARANTIZADAS                     #
#Fecha creacion    => 14 DE ENERO DE 2010                             #
#Por               => MAURO MUNIZ CABALLERO                           #
#Sistema           => ACR                                             #
#######################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        cza RECORD LIKE acr_cza_dev_ag.* ,
        det RECORD LIKE acr_det_dev_ag.* ,
        sum RECORD LIKE acr_sum_dev_ag.*

    DEFINE
        HOY    DATE

    DEFINE
        cod_maq    CHAR(03) ,
        aportante  CHAR(11) ,
        HORA       CHAR(10) ,
        aux_pausa  CHAR(01) ,
        usuario    CHAR(08)

    DEFINE
        vprecio_accion  DECIMAL(22,14),
        vfecha_archivo  CHAR(10),
        opc             CHAR(1),
        enter           CHAR(1),
        vfolio          INTEGER

END GLOBALS

MAIN

    DEFER INTERRUPT
        OPTIONS PROMPT LINE LAST

    DISPLAY "              INCORPORAR REGISTROS DEVOLUCION AG           " AT 12,10 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT " [T]ransferencia devolucion anualidades garantizadas =>  " FOR opc

        IF opc MATCHES '[tT]' THEN
           EXIT WHILE
        END IF
    END WHILE

    IF opc="t" THEN
        LET opc ="T"
    END IF

    PROMPT "FOLIO A PROVISIONAR: " FOR vfolio

    PROMPT "DESEA GENERAR EL PROCESO ? [S/N] " FOR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
        ERROR "PROCESANDO INFORMACION ... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

        CALL proceso_principal() #pp
    ELSE
        EXIT PROGRAM
    END IF

    PROMPT "PROCESO FINALIZO NORMALMENTE. PRESIONE [Enter] PARA SALIR" FOR aux_pausa

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE
        HAY_REGISTROS        ,
        num_reg_procesados   ,
        TODOS                SMALLINT

    LET HORA = TIME
    LET HOY  = DATE

    SELECT COUNT(*)
    INTO   TODOS
    FROM   acr_det_dev_ag
    WHERE  folio  = vfolio
    AND    tipo_registro = "02"
    AND    estado = 1

    DISPLAY "REGISTROS A PROCESAR: ",TODOS AT 15,10

    DECLARE cursor_1 CURSOR FOR 
        SELECT HD.*,
               USER
        FROM   acr_det_dev_ag HD
        WHERE  HD.folio         = vfolio
        AND    HD.estado        = 1
        AND    HD.tipo_registro = "02"

        LET HAY_REGISTROS      = FALSE
        LET num_reg_procesados = 1

        FOREACH cursor_1 INTO det.*,usuario
            SELECT 'X'
              FROM afi_mae_afiliado A
             WHERE A.n_seguro = det.n_seguro

            IF SQLCA.SQLCODE = 0 THEN
                DISPLAY "Registros Procesados ",num_reg_procesados AT 17,10
                DISPLAY "N.S.S. ",det.n_seguro at 18,10

                CALL dispersa(det.*)
            END IF

            LET num_reg_procesados = num_reg_procesados + 1
        END FOREACH

        UPDATE acr_cza_dev_ag
        SET    estado = 2
        WHERE  folio = vfolio
        AND    tipo_registro = "01"
        AND    estado = 1

        UPDATE acr_det_dev_ag
        SET    estado = 2
        WHERE  folio = vfolio
        AND    tipo_registro = "02"
        AND    estado = 1

        UPDATE acr_sum_dev_ag
        SET    estado = 2
        WHERE  folio= vfolio
        AND    tipo_registro = "09"
        AND    estado = 1

        UPDATE acr_devol_ag
        SET    estado = 2
        WHERE  folio= vfolio
        AND    estado = 1

END FUNCTION

FUNCTION dispersa(x_historico)
#d----------------------------

    DEFINE
        x_historico  RECORD LIKE acr_det_dev_ag.* ,
        g_sie        RECORD LIKE cta_regimen.*    ,
        reg_prov     RECORD LIKE dis_provision.*  ,
        i            SMALLINT,
        txt          CHAR(100)

    DEFINE
        valor_en_pesos  DECIMAL(18,6),
        vpesos_cta      DECIMAL(18,2)

    LET txt = " SELECT * ",
              " FROM   cta_regimen ms ",
              " WHERE  ms.nss = ","'",x_historico.n_seguro,"'",
              " AND    ms.subcuenta   = ?"

    PREPARE cla_exe FROM txt

    LET reg_prov.estado           = 5
    LET reg_prov.tipo_movimiento  = 1
    LET reg_prov.folio            = x_historico.folio
    LET reg_prov.nss              = x_historico.n_seguro
    LET reg_prov.curp             = ''
    LET reg_prov.folio_sua        = ''
    LET reg_prov.dias_cotizados   = 0
    LET reg_prov.sucursal         = ''
    LET reg_prov.fecha_proceso    = TODAY
    LET reg_prov.usuario          = usuario
    LET reg_prov.fecha_archivo    = cza.fecha_presentacion
    LET reg_prov.etiqueta         = 0
    LET reg_prov.id_aportante     = "DEV. INF."
    LET reg_prov.fecha_pago       = x_historico.fecha_mov_banxico
    LET reg_prov.fecha_valor      = x_historico.fecha_mov_banxico
    LET reg_prov.fecha_conversion = x_historico.fecha_mov_banxico

    FOR i = 1 TO 2
        CASE i
            WHEN 1
                LET reg_prov.subcuenta = 4

                DECLARE cursor_4 CURSOR FOR cla_exe

                FOREACH cursor_4 USING reg_prov.subcuenta INTO g_sie.*

                SELECT precio_del_dia
                  INTO vprecio_accion
                  FROM glo_valor_accion
                 WHERE fecha_valuacion = x_historico.fecha_mov_banxico
                   AND codigo_siefore  = g_sie.codigo_siefore

                LET reg_prov.monto_en_acciones = x_historico.aivs_v97
                LET valor_en_pesos             = reg_prov.monto_en_acciones
                                                 * vprecio_accion
                LET vpesos_cta                 = valor_en_pesos
                LET reg_prov.monto_en_pesos    = vpesos_cta
                LET reg_prov.siefore           = g_sie.codigo_siefore
                LET reg_prov.precio_accion     = vprecio_accion

                END FOREACH
            WHEN 2
                LET reg_prov.subcuenta = 8

                DECLARE cursor_8 CURSOR FOR cla_exe

                FOREACH cursor_8 USING reg_prov.subcuenta INTO g_sie.*

                SELECT precio_del_dia
                  INTO vprecio_accion
                  FROM glo_valor_accion
                 WHERE fecha_valuacion = x_historico.fecha_mov_banxico
                   AND codigo_siefore  = g_sie.codigo_siefore

                LET reg_prov.monto_en_acciones = x_historico.aivs_v92
                LET valor_en_pesos             = reg_prov.monto_en_acciones
                                                 * vprecio_accion
                LET vpesos_cta                 = valor_en_pesos
                LET reg_prov.monto_en_pesos    = vpesos_cta
                LET reg_prov.siefore           = g_sie.codigo_siefore
                LET reg_prov.precio_accion     = vprecio_accion
                END FOREACH
        END CASE

        LET reg_prov.consecutivo_lote = x_historico.cont_servicio

        IF reg_prov.monto_en_pesos IS NOT NULL AND
           reg_prov.monto_en_pesos > 0 THEN
            INSERT INTO dis_provision VALUES(reg_prov.*)

            IF STATUS < 0 THEN
                ERROR "Error al insertar provision devolucion de saldos ",STATUS
            END IF
        END IF
    END FOR

END FUNCTION

