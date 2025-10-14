#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETC822  => GENERACION DE SALDOS DEL PROCESO PENSION GARANTIZADA      #
#                     TIPO DE RETIRO C                                          #
#By                => DMR                                                       #
#Fecha Elaboracion.=> 11 DE FEBRERO DEL 2004                                    #
#Actualizacion     => 04-Agosto-2004                                            #
#Sistema           => RET                                                       #
# Actualizacion    => IJR 29-Ago/2005 Masnejo de AIV's                          #
# Actualizacion    => IJR 240406 Correccion a validaion incorrecta diag 502     #
# Actualizacion    => Javier Gonzalez Jeronimo - 9 de Enero de 2007             #
#                     Modificaciones para soporte de Multisiefores              #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 15 DE DICIEMBRE DE 2008                                   #
#                     Ajustes del programa para el proceso de mto constitutivo  #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 16 DE AGOSTO DE 2010                                      #
#                     Se agrega el campo "clave de aseguradora" en la           #
#                     generacion de la operacion 03                             #
#################################################################################

DATABASE safre_af

GLOBALS
    DEFINE
        s_modulo              RECORD LIKE seg_modulo.*

    DEFINE reg_3 RECORD #reg_3
        folio_oper_02         INTEGER
    END RECORD

    DEFINE reg_20 RECORD #loc #reg_20
        estado_marca          SMALLINT,
        codigo_rechazo        SMALLINT,
        marca_causa           SMALLINT,
        fecha_causa           DATE
    END RECORD

    DEFINE reg_01 RECORD #glo #reg_01
        nss                   LIKE ret_transf_rx.nss              ,
        nombre_datamart       LIKE ret_transf_rx.nombre_datamart  ,
        tipo_seguro           LIKE ret_transf_rx.tipo_seguro      ,
        tipo_pension          LIKE ret_transf_rx.tipo_pension     ,
        tipo_prestacion       LIKE ret_transf_rx.tipo_prestacion  ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen    ,
        porcentaje_val        DECIMAL(5,2)                        ,
        diag_registro         LIKE ret_transf_rx.diag_registro    ,
        consecutivo           LIKE ret_transf_rx.consecutivo
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        recepcionado          LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        diagnosticado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
                                        estado      SMALLINT     ,
                                        fecha       DATE         ,
                                        siefore     SMALLINT     ,
                                        precio_dia  DECIMAL(16,6)
                                      END RECORD
    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD

    DEFINE #glo #date
        HOY                   ,
        f_verifi              ,
        gd_primero_mes        DATE

    DEFINE #glo #char 
        gs_tipo_retiro        CHAR(001) ,
        comando               CHAR(110) ,
        var_nula              CHAR(001) ,
        c11_id_aport          CHAR(011) ,
        usuario               CHAR(010) ,
        G_LISTA_DET           CHAR(100) ,
        enter                 CHAR(001) ,
        v_desmarca            CHAR(150) ,
        v_saldo_dia           CHAR(150) ,
        v_provision           CHAR(150)

    DEFINE #glo #smallint
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_sieviv             ,-- Indica en que posicion se encuentra almacenada 
                               -- la info de precio de vivienda
        s_codigo_afore        ,
        v_subcuenta           ,
        v_grupo               ,
        v_siefore             ,
        cod_resp              ,
        gs_tipo_mov           SMALLINT

    DEFINE #glo #integer
        cont_2                ,
        cont_det_pe02         ,
        cont_provi            ,
        i_tot_registros       INTEGER

    DEFINE #glo #decimal
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6)

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC822.log")

    CALL init() #i
    CALL f_abre_ventana()
    CALL f_obtiene_precios_accion(HOY)
    CALL f_captura_datos()
    LET  gd_primero_mes = f_obten_primerdia_mes()
    
    DISPLAY "PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    --PROCESO PRINCIPAL

    DISPLAY " PROCESANDO PRIMER PASO               " AT 15,10 ATTRIBUTE(REVERSE)
    CALL primer_paso()         #pp  DIAGNOSTICA Y CREA RET_SOLICITUD_TX

    DISPLAY " PROCESANDO SEGUNDO PASO              " AT 15,10 ATTRIBUTE(REVERSE)
    CALL segundo_paso()        #sp  CARGA APORTES X BIMESTRE  NOT VI-VO-OR-AS

    DISPLAY " PROCESANDO TERCER PASO               " AT 15,10 ATTRIBUTE(REVERSE)
    CALL tercer_paso()         #tp ACTUALIZA SALDOS EN 0 NO PAGABLES EN VIVIENDA

    DISPLAY " GENERANDO ARCHIVO DE OPERACION 03    " AT 15,10 ATTRIBUTE(REVERSE)
    CALL genera_operacion03()  #sep GENERA ARCHIVO 03
    
    DISPLAY " DESMARCA DE CUENTAS                  " AT 15,10 ATTRIBUTE(REVERSE)
    CALL desmarca_no_liq()     #DESMARCA CUENTAS

    CLOSE WINDOW retc8221
    CALL f_abre_ventana()
    
    DISPLAY BY NAME reg_3.*

    DISPLAY "TOTAL REGISTROS DE SOLICITUDES : ",cont_det_pe02 AT 12,21

    SELECT COUNT(UNIQUE nss)
    INTO   cont_provi
    FROM   dis_provision
    WHERE  folio = reg_3.folio_oper_02
    AND    tipo_movimiento = gs_tipo_mov

    DISPLAY "TOTAL REGISTROS PROVISIONADOS  : ",cont_provi    AT 14,21

    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc8221
END MAIN


FUNCTION init()
#i-------------
    LET HOY               = TODAY
    LET cont_det_pe02     = 0
    LET gs_sieviv         = 11
    LET c11_id_aport      = "RETIRO"
    LET gs_tipo_retiro    = "C"
    
    INITIALIZE  var_nula TO NULL

    SELECT codigo_afore,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore < gs_sieviv

    SELECT *
    INTO   s_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.estado_solicitud
    INTO   reg_2.recepcionado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   reg_2.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   reg_2.diagnosticado
    FROM   ret_estado A
    WHERE  A.descripcion = "DIAGNOSTICADO"

    SELECT MAX(folio)
    INTO   reg_3.folio_oper_02
    FROM   ret_transf_rx
    WHERE  ret_transf_rx.estado_solicitud IN ( reg_2.recepcionado  ,
                                               reg_2.diagnosticado )
    AND    tipo_retiro = gs_tipo_retiro

    SELECT A.movimiento
    INTO   gs_tipo_mov
    FROM   tab_retiro A
    WHERE  A.tipo_retiro = gs_tipo_retiro

    --- SALDO AL DIA ---
    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
    PREPARE eje_saldo_dia FROM v_saldo_dia

    --- PROVISION ---
    LET v_provision = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provision FROM v_provision


END FUNCTION

#=============================================================================#
# ASIGNACION DE DIAGNOSTICOS                                                  #
#=============================================================================#
FUNCTION primer_paso()
#pp------------------

    DEFINE
        c1_estado_sub_viv         CHAR(1)

    DECLARE cur_2 CURSOR FOR
    SELECT A.nss              ,
           A.nombre_datamart  ,
           A.tipo_seguro      ,
           A.tipo_pension     ,
           A.tipo_prestacion  ,
           A.fecha_ini_pen    ,
           A.porcentaje_val   ,
           A.diag_registro    ,
           A.consecutivo      ,
           A.estado_sub_viv
    FROM   ret_transf_rx A
    WHERE  A.folio            = reg_3.folio_oper_02
    AND    A.estado_solicitud = reg_2.recepcionado
    AND    A.tipo_retiro      = gs_tipo_retiro

    FOREACH cur_2 INTO reg_01.*, c1_estado_sub_viv
        LET reg_01.diag_registro = 501  -- CTA ACEPTADA CON TRANSFERENCIA DE RECURSOS

        SELECT "OK"
        FROM   afi_mae_afiliado
        WHERE  n_seguro = reg_01.nss
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
           LET reg_01.diag_registro = 503  --CUENTA INEXISTENTE
        ELSE
           SELECT "OK"
           FROM   cta_act_marca
           WHERE  nss        =  reg_01.nss
           AND    marca_cod IN (120,130,140)  --CUENTA INHABILITADA
           GROUP BY 1

           IF STATUS = 0  THEN --Lo encontro
               LET reg_01.diag_registro = 502 -- CUENTA CON SALDO CERO
           ELSE
                SELECT "OK"
                FROM   cta_act_marca
                WHERE  nss        = reg_01.nss
                AND    marca_cod = 590  -- EN TRAMITE JUDICIAL
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    LET reg_01.diag_registro = 504 -- CUENTA EN LAUDO JUDICIAL EN LA AFORE
                ELSE
                    SELECT fecha_carga
                    INTO   f_verifi
                    FROM   ret_cza_lote
                    WHERE  folio = reg_3.folio_oper_02
                    
                    SELECT "OK"
                    FROM   cta_act_marca
                    WHERE  nss           = reg_01.nss
                    AND    marca_cod     = 815
                    AND    fecha_ini     = f_verifi
                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
                       LET reg_01.diag_registro = 506 -- CUENTA EN PROCESO OPERATIVO EN LA AFORE
                    END IF -- Marcado Transferencia
                END IF -- Laudo Judicial
           END IF -- Saldo Cero
        END IF -- Inexistente

        INSERT INTO ret_transf_tx
        VALUES ( reg_01.nss           ,
                 reg_01.consecutivo   ,
                 reg_3.folio_oper_02  ,
                 reg_01.diag_registro ,
                 c1_estado_sub_viv    ,
                 0                    ,  --monto_accion_97  
                 0                    ,  --monto_accion_cv  
                 0                    ,  --monto_accion_so  
                 0                    ,  --monto_saldo_viv97
                 NULL                    --monto constitutivo afore (solo aplica en Retiros A)
               )
    END FOREACH
END FUNCTION

#=============================================================================#
# Objetivo : Calcular saldos para el Tipo de Pension por Muerte               #
#                                                                             #
#=============================================================================#
FUNCTION segundo_paso()
#sp-----------------

    DEFINE arr_siefore ARRAY [20] OF RECORD
                                        activo            SMALLINT,
                                        monto_accion_97   DECIMAL(16,6) ,
                                        monto_accion_cv   DECIMAL(16,6) ,
                                        monto_accion_es   DECIMAL(16,6) ,
                                        monto_accion_esp  DECIMAL(16,6) ,
                                        monto_accion_so   DECIMAL(16,6)
                                    END RECORD

    DEFINE reg_11 RECORD #glo #reg_11
        monto_parti_viv       DECIMAL(16,6)
    END RECORD

    DEFINE reg_13 RECORD #glo #reg_13
        nss                   LIKE ret_transf_rx.nss            ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen  ,
        diag_registro         LIKE ret_transf_rx.diag_registro  ,
        consecutivo           LIKE ret_transf_rx.consecutivo
    END RECORD

    DEFINE #loc #smallint
        ls_siefore            SMALLINT            #-- contador para los ciclos for

    DEFINE #loc #char
        estado_sub_viv           CHAR(1)

    DEFINE #loc #decimal
        sum_monto_acc_rcv     DECIMAL(16,6) ,
        d6_monto_acc_rcv      DECIMAL(16,6) ,
        d6_monto_acc_cv       DECIMAL(16,6) ,
        monto_viv_ori_02      DECIMAL(16,6)

    LET i_tot_registros = 0

    SELECT COUNT(*)
    INTO   i_tot_registros
    FROM   ret_transf_rx A , ret_transf_tx B
    WHERE  A.folio                = reg_3.folio_oper_02
    AND    A.tipo_retiro          = gs_tipo_retiro
    AND    A.folio                = B.folio
    AND    A.nss                  = B.nss
    AND    A.consecutivo          = B.consecutivo
    AND    A.estado_solicitud     = reg_2.recepcionado
    AND    B.diag_registro        = 501

    IF i_tot_registros = 0 THEN
        RETURN
    END IF

    DECLARE cur_62 CURSOR FOR
    SELECT A.nss              ,
           A.fecha_ini_pen    ,
           B.diag_registro    ,
           A.consecutivo      ,
           A.saldo_viv97      ,
           A.estado_sub_viv
    FROM   ret_transf_rx A , ret_transf_tx B
    WHERE  A.folio                = reg_3.folio_oper_02
    AND    A.tipo_retiro          = gs_tipo_retiro
    AND    A.folio                = B.folio
    AND    A.nss                  = B.nss
    AND    A.consecutivo          = B.consecutivo
    AND    A.estado_solicitud     = reg_2.recepcionado
    AND    B.diag_registro        = 501


    LET cont_2 = 0

    --PROCESA
    FOREACH cur_62 INTO reg_13.*, monto_viv_ori_02, estado_sub_viv

        IF monto_viv_ori_02 IS NULL OR monto_viv_ori_02 < 0 THEN
           LET monto_viv_ori_02 = 0
        END IF

        LET cont_2 = cont_2 + 1

        DISPLAY " PROCESANDO EL REGISTRO ",cont_2 USING "&&&&&&" AT 16,10 ATTRIBUTE(REVERSE)
        DISPLAY " TOTAL REGISTRO         ",i_tot_registros USING "&&&&&&" AT 17,10
                                           ATTRIBUTE(REVERSE)

        --INICIALIZACION PARA ACUMULADO POR SIEFORE
        FOR ls_siefore = 1 TO gs_num_siefores
            LET arr_siefore[ls_siefore].activo           = FALSE
            LET arr_siefore[ls_siefore].monto_accion_97  = 0
            LET arr_siefore[ls_siefore].monto_accion_cv  = 0
            LET arr_siefore[ls_siefore].monto_accion_so  = 0
            LET arr_siefore[ls_siefore].monto_accion_es  = 0
            LET arr_siefore[ls_siefore].monto_accion_esp = 0
        END FOR

        LET reg_11.monto_parti_viv  = 0

        LET v_subcuenta = 0
        LET v_grupo     = 0

        --OBTIENE EL SALDO POR NSS DE CADA UNA DE LAS SUBCUENTAS--
        DECLARE c_saldo CURSOR FOR eje_saldo_dia

        FOREACH c_saldo USING reg_13.nss,
                               v_subcuenta,
                               v_grupo,
                               HOY
                          INTO v_subcuenta,
                               v_siefore,
                               f_monto_acc,
                               f_monto_pes

            IF v_subcuenta = 1  OR  v_subcuenta = 2  OR  v_subcuenta = 5  OR
               v_subcuenta = 6  OR  v_subcuenta = 9  OR  v_subcuenta = 4  THEN

                IF f_monto_acc IS NULL OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                END IF

                IF v_siefore = gs_sieviv THEN
                    #-- El elemento gs_sieviv del arreglo de precios contiene la informacion de la siefore 11
                    LET f_monto_pes = f_monto_acc * gar_precio_acc[gs_sieviv].precio_dia
                ELSE
                    LET f_monto_pes = f_monto_acc * gar_precio_acc[v_siefore].precio_dia
                END IF

                --SE INVIERTE EL SIGNO POR TRATARSE DE UN CARGO
                LET f_monto_acc = f_monto_acc * (-1)
                LET f_monto_pes = f_monto_pes * (-1)

                #-- Acumulamos montos dependiendo de la siefore
                IF f_monto_acc < 0  THEN
                    IF v_siefore <> 11 THEN

                        #-- Marcamos como activo el registro de la siefore actual
                        LET arr_siefore[v_siefore].activo = TRUE

                        CASE v_subcuenta
                            WHEN 1
                                LET arr_siefore[v_siefore].monto_accion_97  = f_monto_acc
                            WHEN 2
                                LET arr_siefore[v_siefore].monto_accion_cv  = f_monto_acc
                            WHEN 5
                                LET arr_siefore[v_siefore].monto_accion_so  = f_monto_acc
                            WHEN 6
                                LET arr_siefore[v_siefore].monto_accion_es  = f_monto_acc
                            WHEN 9
                                LET arr_siefore[v_siefore].monto_accion_esp = f_monto_acc
                        END CASE
                    ELSE
                        IF v_subcuenta = 4 THEN
                            LET f_monto_acc = redondea_val(f_monto_acc, 2)
                            LET f_monto_pes = redondea_val(f_monto_pes, 2)
                            LET reg_11.monto_parti_viv = f_monto_acc
                        END IF
                    END IF

                    IF v_subcuenta = 4 AND ( reg_11.monto_parti_viv * (-1) ) > monto_viv_ori_02 THEN
                        --SI EL MONTO CALCULADO ES MAYOR QUE EL INFORMADO (NO SE PROVISIONA)
                        LET reg_13.diag_registro   = 507
                        LET reg_11.monto_parti_viv = 0
                    ELSE
                        IF v_subcuenta = 4 AND estado_sub_viv <> 1 THEN
                            --estado_sub_viv<>1 no se paga
                            LET reg_11.monto_parti_viv = 0
                        ELSE
                            --PARA CUALQUIER SUBCUENTA <> 4 PROVISIONA
                            --O PARA SUBCUENTA 4 CON MONTO CALCULADO MENOR QUE MONTO INFORMADO
                            
                            DECLARE cur_prov CURSOR FOR eje_provision

                            OPEN cur_prov   USING reg_3.folio_oper_02,
                                                  var_nula           ,
                                                  reg_13.nss         ,
                                                  v_subcuenta        ,
                                                  gs_tipo_mov        ,
                                                  reg_13.consecutivo ,
                                                  v_siefore          ,
                                                  f_monto_acc        ,
                                                  f_monto_pes        ,
                                                  c11_id_aport       ,
                                                  HOY

                            FETCH cur_prov INTO cod_resp
                            CLOSE cur_prov

                            IF cod_resp < 0 THEN
                                ERROR "NSS ",reg_01.nss," NO pudo PROVISIONARSE"
                                SLEEP 2
                            END IF
                        END IF #-- v_subcuenta = 4 AND estado_sub_viv <> 1
                    END IF #-- v_subcuenta = 4 AND ( reg_11.monto_parti_viv * (-1) ) > monto_viv_ori_02
                END IF #-- f_monto_acc < 0
            END IF #-- Subctas
        END FOREACH

        #-- Determinamos los importes a informar por siefore
        LET sum_monto_acc_rcv = 0
        FOR ls_siefore = 1 TO gs_num_siefores
            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                LET d6_monto_acc_cv  = arr_siefore[ls_siefore].monto_accion_cv  +
                                       arr_siefore[ls_siefore].monto_accion_es  +
                                       arr_siefore[ls_siefore].monto_accion_esp

                LET d6_monto_acc_rcv = arr_siefore[ls_siefore].monto_accion_97  +
                                       arr_siefore[ls_siefore].monto_accion_cv  +
                                       arr_siefore[ls_siefore].monto_accion_es  +
                                       arr_siefore[ls_siefore].monto_accion_esp +
                                       arr_siefore[ls_siefore].monto_accion_so

                LET sum_monto_acc_rcv = sum_monto_acc_rcv + d6_monto_acc_rcv

                IF d6_monto_acc_rcv IS NOT NULL AND d6_monto_acc_rcv != 0 THEN
                    INSERT INTO ret_monto_siefore
                        VALUES (reg_13.nss                               ,  --Nss
                                reg_13.consecutivo                       ,  --Consecutivo
                                reg_3.folio_oper_02                      ,  --Folio
                                gs_tipo_retiro                           ,  --Tipo de Retiro
                                3                                        ,  --Tipo de Operacion (transferencias)
                                ls_siefore                               ,  --Siefore
                                -arr_siefore[ls_siefore].monto_accion_97 ,  --Acciones Retiro 97
                                -d6_monto_acc_cv                         ,  --Acciones CV
                                -arr_siefore[ls_siefore].monto_accion_so ,  --Acciones Cuota social
                                0                                           --Acciones Retiro92
                               )

                    IF SQLCA.SQLCODE != 0 THEN
                        PROMPT "ERROR AL INSERTAR MONTOS PARA SIEFORE ", ls_siefore, " PRESIONE ENTER ",
                               "PARA CONTINUAR" ATTRIBUTE(REVERSE) FOR CHAR enter
                    END IF
                END IF
            END IF
        END FOR

        #-- Si los importes de las siefores es cero se informa con diagnostico 502
        IF sum_monto_acc_rcv = 0 AND
           reg_11.monto_parti_viv = 0 AND reg_13.diag_registro != 507 THEN
            LET reg_13.diag_registro = 502
        END IF

        --ACTUALIZA SALDO DE VIVIENDA
        UPDATE ret_transf_tx
        SET    diag_registro     = reg_13.diag_registro    ,
               saldo_viv97       = - reg_11.monto_parti_viv,
               estado_sub_viv    = estado_sub_viv
        WHERE  folio       = reg_3.folio_oper_02
        AND    nss         = reg_13.nss
        AND    consecutivo = reg_13.consecutivo

        -- Se actualiza la fecha de vivienda y el estado de la solicitud
        UPDATE ret_transf_rx
        SET    fecha_valor_viv  = gd_primero_mes
        WHERE  folio            = reg_3.folio_oper_02
        AND    nss              = reg_13.nss
        AND    consecutivo      = reg_13.consecutivo
    END FOREACH

END FUNCTION

#============================================================================#
# Actualiza saldos en cero para registros rechazados                         #
#============================================================================#
FUNCTION tercer_paso()
#tp------------------
    DEFINE reg_08 RECORD #loc #reg_08
        nss                   LIKE ret_transf_rx.nss           ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen ,
        diag_registro         LIKE ret_transf_rx.diag_registro ,
        tipo_pension          LIKE ret_transf_rx.tipo_pension  ,
        consecutivo           LIKE ret_transf_rx.consecutivo   ,
        estado_sub_viv        LIKE ret_transf_rx.estado_sub_viv
    END RECORD

    DEFINE #loc #char
        c2_diag_registro      CHAR(2)

    DEFINE #loc #smallint
        s_nss_traspasado      SMALLINT

    DEFINE
        v_sum_acciones_cv      DECIMAL(16,6),
        v_sum_acciones_cs      DECIMAL(16,6),
        v_sum_acciones_ret97   DECIMAL(16,6)

    DECLARE cur_7 CURSOR FOR
    SELECT A.nss           ,
           A.fecha_ini_pen ,
           B.diag_registro ,
           A.tipo_pension  ,
           A.consecutivo   ,
           A.estado_sub_viv,
           B.acciones_ret97,
           B.acciones_cv   ,
           B.acciones_cuota_soc
    FROM   ret_transf_rx A , ret_transf_tx B
    WHERE  A.folio          = reg_3.folio_oper_02
    AND    A.tipo_retiro    = gs_tipo_retiro
    AND    A.estado_sub_viv IN ("2","3","5")
    AND    A.folio          = B.folio
    AND    A.nss            = B.nss
    AND    A.consecutivo    = B.consecutivo

    FOREACH cur_7 INTO reg_08.*
        IF reg_08.diag_registro = 502 OR
           reg_08.diag_registro = 506 OR
           reg_08.diag_registro = 505 THEN

            UPDATE ret_transf_tx
            SET    acciones_ret97     = 0 ,
                   acciones_cv        = 0 ,
                   acciones_cuota_soc = 0 ,
                   saldo_viv97        = 0 ,
                   estado_sub_viv     = reg_08.estado_sub_viv
            WHERE  folio       = reg_3.folio_oper_02
            AND    nss         = reg_08.nss
            AND    consecutivo = reg_08.consecutivo
        ELSE
           -- Verifica si los saldos de RCV son 0 para diagnosticar en 502
           SELECT SUM(acciones_cv),
                  SUM(acciones_cs),
                  SUM(acciones_ret97)
           INTO   v_sum_acciones_cv,
                  v_sum_acciones_cs,
                  v_sum_acciones_ret97
           FROM   ret_monto_siefore
           WHERE  nss   = reg_08.nss
           AND    folio = reg_3.folio_oper_02

           IF v_sum_acciones_cv    = 0 AND
              v_sum_acciones_cs    = 0 AND
              v_sum_acciones_ret97 = 0 THEN

              -- SI SALDOS DE RCV = 0 ENTONCES DIAGNOSTICA EN 502
              UPDATE ret_transf_tx
              SET    saldo_viv97    = 0  ,
                     diag_registro  = 502
              WHERE  folio          = reg_3.folio_oper_02
              AND    nss            = reg_08.nss
              AND    consecutivo    = reg_08.consecutivo
           ELSE
              -- SE PAGA RCV, VIVIENDA NO
              UPDATE ret_transf_tx
              SET    saldo_viv97    = 0
              WHERE  folio          = reg_3.folio_oper_02
              AND    nss            = reg_08.nss
              AND    consecutivo    = reg_08.consecutivo
           END IF
        END IF

        UPDATE ret_transf_rx
        SET    fecha_valor_viv = "01/01/0001"
        WHERE  folio           = reg_3.folio_oper_02
        AND    nss             = reg_08.nss
        AND    consecutivo     = reg_08.consecutivo
    END FOREACH

END FUNCTION

--GENERA ARCHIVO DE LA OPERACION 03
FUNCTION genera_operacion03()
#go03------------------------
    DEFINE
        r_transf_rx           RECORD LIKE ret_transf_rx.*     ,
        r_transf_tx           RECORD LIKE ret_transf_tx.*     ,
        r_monto_siefore       RECORD LIKE ret_monto_siefore.*

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_viv      CHAR(100)

    DEFINE
        HORA                  CHAR(5)

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = s_modulo.ruta_envio CLIPPED, "/", "DET-NSS-C-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = s_modulo.ruta_envio CLIPPED, "/", "DET-SIE-C-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = s_modulo.ruta_envio CLIPPED, "/", "DET-VIV-C-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET = s_modulo.ruta_envio CLIPPED, "/", "DET-C"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    LET i_tot_registros = 0

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_8 CURSOR FOR
    SELECT A.*,
           B.*
    FROM   ret_transf_rx A ,
           ret_transf_tx B
    WHERE  A.folio           = reg_3.folio_oper_02
    AND    A.folio           = B.folio
    AND    A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo
    AND    A.tipo_retiro     = gs_tipo_retiro

    FOREACH cur_8 INTO r_transf_rx.*,r_transf_tx.*

        #-- Iniciamos los reportes
        START REPORT det_transferencia TO ruta_det_nss
        START REPORT det_vivienda TO ruta_det_viv
        START REPORT det_siefores TO ruta_det_sie

        LET i_tot_registros = i_tot_registros + 1
        LET cont_det_pe02   = cont_det_pe02 + 1

        OUTPUT TO REPORT det_transferencia(r_transf_rx.*, r_transf_tx.*)
        OUTPUT TO REPORT det_vivienda(r_transf_rx.*, r_transf_tx.*)

        #-- Obtenemos los montos para las siefores que tengan saldos
        DECLARE cur_10 CURSOR FOR
            SELECT *
                FROM   ret_monto_siefore
                WHERE  nss         = r_transf_rx.nss
                AND    consecutivo = r_transf_rx.consecutivo

        FOREACH cur_10 INTO r_monto_siefore.*
            OUTPUT TO REPORT det_siefores(r_transf_tx.nss, r_transf_rx.curp, r_monto_siefore.*)
        END FOREACH

        FINISH REPORT det_transferencia
        FINISH REPORT det_vivienda
        FINISH REPORT det_siefores

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss,
                             ruta_det_sie,
                             ruta_det_viv,
                             i_tot_registros)

        DISPLAY "TOTAL REGISTROS DE SOLICITUDES : ",cont_det_pe02 AT 12,21

        --SE CAMBIA EL ESTADO A PROCESADO
        UPDATE ret_transf_rx
        SET    estado_solicitud = reg_2.procesado
        WHERE  folio            = reg_3.folio_oper_02
        AND    nss              = r_transf_tx.nss
        AND    consecutivo      = r_transf_tx.consecutivo

    END FOREACH

    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando

    INSERT INTO ret_seguimiento
    VALUES(reg_3.folio_oper_02  ,#folio
           ""                   ,#tipo_oper_recep
           ""                   ,#fecha_recepcion
           "03"                 ,#tipo_oper_envio
           HOY                  ,#fecha_envio
           reg_2.procesado      ,#status
           0
          )

    LET HORA = TIME

    INSERT INTO ret_ctr_envio_lote
    VALUES (HOY                            ,#fecha_genera
            gs_tipo_retiro                 ,#tipo_retiro
            reg_3.folio_oper_02            ,#folio
            NULL                           ,#fecha_envio
            NULL                           ,#fecha_reverso
            HORA                           ,#hora_genera
            NULL                           ,#hora_envio
            usuario                        ,#usuario_genera
            NULL                           ,#usuario_envio
            NULL                           ,#usuario_reversa
            reg_2.procesado                ,#estado
            cont_det_pe02
           )
END FUNCTION

#----------------------------------------------------------------------------------
# Esta funcion, dadas las rutas de los tres archivos de detalle temporales de los
# reportes, los va concatenando en uno solo que sera el archivo de detalle final.
#----------------------------------------------------------------------------------
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_5, p_regs)

    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_5      CHAR(100)

    DEFINE p_regs SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = s_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = s_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ",
                        lc_det_4 CLIPPED, " ",
                        lc_det_5 CLIPPED
    LET com_rm = com_rm CLIPPED

    LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                          lc_det_4 CLIPPED, " ",
                          lc_det_5 CLIPPED, " > ", ruta_tmp
    LET com_cat = com_cat CLIPPED

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
        ls_sie                SMALLINT

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " -- SIEFORE ", lc_siefore CLIPPED

            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF
    
    END FOREACH

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

FUNCTION f_abre_ventana()

    OPEN WINDOW retc8221 AT 4,4 WITH FORM "RETC8221" ATTRIBUTE(BORDER)
    DISPLAY "   < Ctrl-C > Salir                                        RETIRO 'C'       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC822             GENERA SALDOS DE PENSION GARANTIZADA                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

FUNCTION f_captura_datos()

    --CAPTURA DEL FOLIO A PROCESAR
    INPUT BY NAME reg_3.* WITHOUT DEFAULTS
        AFTER FIELD folio_oper_02
            IF reg_3.folio_oper_02 IS NULL THEN
                ERROR "    FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_cza_lote A
                WHERE  A.folio = reg_3.folio_oper_02
                AND    A.folio IN ( SELECT B.folio 
                                    FROM   ret_transf_rx B
                                    WHERE  B.folio       = A.folio
                                    AND    B.tipo_retiro = gs_tipo_retiro )

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_02
                ELSE
                    SELECT "OK"
                    FROM   ret_transf_rx A
                    WHERE  A.folio        = reg_3.folio_oper_02
                    AND    A.estado_solicitud IN ( reg_2.recepcionado ,
                                                   reg_2.diagnosticado )
                    AND    A.tipo_retiro  = gs_tipo_retiro
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR"    FOLIO YA PROCESADO CON ANTERIORIDAD"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

        ON KEY (ESC)
            IF reg_3.folio_oper_02 IS NULL THEN
                ERROR "    FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_cza_lote A
                WHERE  A.folio = reg_3.folio_oper_02
                AND    A.folio IN ( SELECT B.folio FROM ret_transf_rx B
                                    WHERE  B.folio = A.folio
                                    AND    B.tipo_retiro = gs_tipo_retiro )

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_02
                ELSE
                    SELECT "OK"
                    FROM   ret_transf_rx A
                    WHERE  A.folio             = reg_3.folio_oper_02
                    AND    A.estado_solicitud IN ( reg_2.recepcionado,
                                                   reg_2.diagnosticado )
                    AND    A.tipo_retiro  = gs_tipo_retiro
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR"    FOLIO YA PROCESADO CON ANTERIORIDAD"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT" PROCESO CANCELADO... <ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT" PROCESO CANCELADO... <ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
               PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF
        END IF
    END WHILE

END FUNCTION

-------------------------------------------------------------------------------
-- Obtiene el 1er dia natural del mes del dia de transferencia (liquidacion) --
-------------------------------------------------------------------------------
FUNCTION f_obten_primerdia_mes()

    DEFINE
        ld_fecha_primero      ,
        f_fecha_valor_trans   DATE

    DEFINE
        lc_cadena10              CHAR(010)


    --RECUPERA LA FECHA DE LA TRANSFERENCIA--
    SELECT fecha_valor_trans
    INTO   f_fecha_valor_trans --Fecha de liquidacion del archivo
    FROM   ret_cza_lote
    WHERE  folio = reg_3.folio_oper_02

    --OBTIENE EL 1ER DIA NATURAL DEL MES DEL DIA DE TRANSFERENCIA(LIQUIDACION)--
    #ff NO ES NECESARIO EL IF
    IF MONTH(f_fecha_valor_trans) <> MONTH(TODAY) THEN
        LET lc_cadena10 = MONTH(f_fecha_valor_trans) USING "&&","/01/",
                       YEAR(f_fecha_valor_trans) USING "&&&&"
        LET ld_fecha_primero = lc_cadena10
    ELSE
        LET lc_cadena10 = MONTH(TODAY) USING "&&","/01/",YEAR(TODAY) USING "&&&&"
        LET ld_fecha_primero = lc_cadena10
    END IF

    RETURN ld_fecha_primero

END FUNCTION 



FUNCTION desmarca_no_liq()
    DEFINE
        nssa   LIKE ret_transf_rx.nss,
        consec LIKE ret_transf_rx.consecutivo

    DECLARE cur_des CURSOR FOR
        SELECT A.nss, A.consecutivo
        FROM   ret_transf_rx A, ret_transf_tx B
        WHERE  A.folio = reg_3.folio_oper_02
        AND    A.folio = B.folio
        AND    A.nss   = B.nss
        AND    A.consecutivo   = B.consecutivo
        AND    A.tipo_retiro   = gs_tipo_retiro
        AND    B.diag_registro NOT IN ( 501,507 )

    FOREACH cur_des INTO nssa, consec

        LET reg_20.estado_marca = 40
        LET reg_20.marca_causa  = 0

        LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta('",
                          nssa,"',",gs_tipo_mov,",",consec,",",
                          reg_20.estado_marca,",",reg_20.marca_causa
                          ,",' ",usuario,"')"

        PREPARE eje_desmar FROM v_desmarca
        EXECUTE eje_desmar
    END FOREACH

END FUNCTION

REPORT det_transferencia(r_transf_rx, r_transf_tx)
#l----------------------------------------------------------------------
    DEFINE
        r_transf_tx           RECORD LIKE ret_transf_tx.*     ,
        r_transf_rx           RECORD LIKE ret_transf_rx.*


    DEFINE
        c6_porcentaje_val     CHAR(006) ,
        c6_porcentaje_ret97   CHAR(006) ,
        c6_porcentaje_cv      CHAR(006) ,
        c6_porcentaje_cs      CHAR(006) ,
        c6_porcentaje_viv     CHAR(006) ,

        c5_porcentaje_val     CHAR(005) ,
        c5_porcentaje_ret97   CHAR(005) ,
        c5_porcentaje_cv      CHAR(005) ,
        c5_porcentaje_cs      CHAR(005) ,
        c5_porcentaje_viv     CHAR(005) 


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            IF r_transf_rx.porcentaje_val IS NULL THEN
                LET r_transf_rx.porcentaje_val = 0
            END IF
            
            LET c6_porcentaje_val   = r_transf_rx.porcentaje_val USING "&&&.&&"
            LET c5_porcentaje_val   = c6_porcentaje_val[01,03],
                                      c6_porcentaje_val[05,06]

            IF r_transf_rx.porcentaje_ret97 IS NULL THEN
                LET r_transf_rx.porcentaje_ret97 = 0
            END IF

            LET c6_porcentaje_ret97 = r_transf_rx.porcentaje_ret97 USING "&&&.&&"
            LET c5_porcentaje_ret97 = c6_porcentaje_ret97[01,03],
                                      c6_porcentaje_ret97[05,06]

            IF r_transf_rx.porcentaje_cv IS NULL THEN
                LET r_transf_rx.porcentaje_cv = 0
            END IF

            LET c6_porcentaje_cv    = r_transf_rx.porcentaje_cv USING "&&&.&&"
            LET c5_porcentaje_cv    = c6_porcentaje_cv[01,03],
                                      c6_porcentaje_cv[05,06]

            IF r_transf_rx.porcentaje_cs IS NULL THEN
                LET r_transf_rx.porcentaje_cs = 0
            END IF

            LET c6_porcentaje_cs    = r_transf_rx.porcentaje_cs USING "&&&.&&"
            LET c5_porcentaje_cs    = c6_porcentaje_cs[01,03],
                                      c6_porcentaje_cs[05,06]

            IF r_transf_rx.porcentaje_viv IS NULL THEN
                LET r_transf_rx.porcentaje_viv = 0
            END IF

            LET c6_porcentaje_viv    = r_transf_rx.porcentaje_viv USING "&&&.&&"
            LET c5_porcentaje_viv    = c6_porcentaje_viv[01,03],
                                       c6_porcentaje_viv[05,06]

        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, "03"                                            , -- ID de operacion
            COLUMN 007, r_transf_tx.nss                                 , -- NSS
            COLUMN 018, r_transf_rx.curp                                , -- CURP
            COLUMN 036, r_transf_rx.nombre_datamart                     , -- Nombre Datamart
            COLUMN 086, r_transf_rx.nombre_afore                        , -- Nombre
            COLUMN 126, r_transf_rx.paterno_afore                       , -- Ap Paterno
            COLUMN 166, r_transf_rx.materno_afore                       , -- Ap Materno
            COLUMN 206, r_transf_rx.sec_pension                         , -- Sec de Pension
            COLUMN 208, r_transf_rx.tipo_mov_procesar  USING "###"      , -- Tipo Movimiento
            COLUMN 211, r_transf_rx.regimen                             , -- Regimen
            COLUMN 213, r_transf_rx.tipo_retiro                         , -- Tipo de Retiro
            COLUMN 214, r_transf_rx.tipo_seguro                         , -- Tipo de Seguro
            COLUMN 216, r_transf_rx.tipo_pension                        , -- Tipo de Pension
            COLUMN 218, r_transf_rx.tipo_prestacion    USING "&&"       , -- Tipo de Prestacion
            COLUMN 220, r_transf_rx.fecha_ini_pen      USING "YYYYMMDD" , -- Fecha Inicio pension
            COLUMN 228, r_transf_rx.fecha_resolucion   USING "YYYYMMDD" , -- Fecha resolucion
            COLUMN 236, c5_porcentaje_val                               , -- Porc. Valuacion
            COLUMN 241, r_transf_rx.semanas_cotizadas  USING "&&&&"     , -- Sems cotizadas
            COLUMN 245, r_transf_rx.fecha_carga_datama USING "YYYYMMDD" , -- Fecha de carga DATAMART
            COLUMN 253, r_transf_tx.diag_registro      USING "&&&"      , -- Diagnostico del registro
            COLUMN 256, "000000"                                        , -- Fecha periodo de pago reing
            COLUMN 262, c5_porcentaje_ret97                             , -- Porc. Retiro 97 a trans
            COLUMN 267, c5_porcentaje_cv                                , -- Porc. CV a trans
            COLUMN 272, c5_porcentaje_cs                                , -- Porc. CS a trans
            COLUMN 277, c5_porcentaje_viv                               , -- Porc. Viv 97 a trans
            COLUMN 282, "000000000000000"                               , -- Monto Constitutivo (Solo Retiros A)
            COLUMN 297, r_transf_rx.cve_aseguradora USING "&&&"         , -- Cve Aseguradora
            COLUMN 300, 40 SPACES                                       , -- Filler
            COLUMN 340, 2  SPACES                                       , -- Resultado de la oper
            COLUMN 342, 3  SPACES                                       , -- Mot Rechazo 1
            COLUMN 345, 3  SPACES                                       , -- Mot Rechazo 2
            COLUMN 348, 3  SPACES                                         -- Mot Rechazo 3
END REPORT

#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de siefore
#---------------------------------------------------------------------------
REPORT det_siefores(p_nss, p_curp, r_monto_siefore)
#ds04----------------------------

    DEFINE p_nss  LIKE ret_solicitud_tx.nss
    DEFINE p_curp LIKE ret_solicitud_tx.curp
    DEFINE r_monto_siefore RECORD LIKE ret_monto_siefore.*

    DEFINE
        c14_impt_ret_97    CHAR(14) ,
        c14_impt_ces_vej   CHAR(14) ,
        c14_impt_cuo_soc   CHAR(14)

    DEFINE
        c15_impt_ret_97    CHAR(15) ,
        c15_impt_ces_vej   CHAR(15) ,
        c15_impt_cuo_soc   CHAR(15)

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            #-- Obtenemos el valor de las Acciones de Retiro 97
            IF r_monto_siefore.acciones_ret97 IS NULL THEN
                LET r_monto_siefore.acciones_ret97 = 0
            END IF

            LET c15_impt_ret_97 = r_monto_siefore.acciones_ret97 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ret_97 = c15_impt_ret_97[01,08],
                                  c15_impt_ret_97[10,15]

            #-- Obtenemos el valor de las Acciones de CV
            IF r_monto_siefore.acciones_cv IS NULL THEN
                LET r_monto_siefore.acciones_cv = 0
            END IF

            LET c15_impt_ces_vej = r_monto_siefore.acciones_cv USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ces_vej = c15_impt_ces_vej[01,08],
                                   c15_impt_ces_vej[10,15]

            #-- Obtenemos el valor de las Acciones de CS
            IF r_monto_siefore.acciones_cs IS NULL THEN
                LET r_monto_siefore.acciones_cs = 0
            END IF

            LET c15_impt_cuo_soc = r_monto_siefore.acciones_cs USING "&&&&&&&&.&&&&&&"
            LET c14_impt_cuo_soc = c15_impt_cuo_soc[01,08],
                                   c15_impt_cuo_soc[10,15]

        PRINT
            COLUMN 001, "04"                                   , -- Tipo de registro
            COLUMN 003, p_nss                                  , -- NSS
            COLUMN 014, p_curp                                 , -- CURP
            COLUMN 032, r_monto_siefore.siefore USING "&&"     , -- Clave de siefore
            COLUMN 034, c14_impt_ret_97                        , -- Acciones de retiro 97
            COLUMN 048, c14_impt_ces_vej                       , -- Acciones de CV
            COLUMN 062, c14_impt_cuo_soc                       , -- Acciones de CS
            COLUMN 076, 275 SPACES                               -- Filler

END REPORT

#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de vivienda
#---------------------------------------------------------------------------
REPORT det_vivienda(r_transf_rx, r_transf_tx)
#dv05----------------------------

    DEFINE
        r_transf_tx           RECORD LIKE ret_transf_tx.*     ,
        r_transf_rx           RECORD LIKE ret_transf_rx.*

    DEFINE #loc #char
        c15_impt_viv_97     CHAR(15) ,
        c14_impt_viv_97     CHAR(14)

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos Intereses de Vivienda 97
            LET c15_impt_viv_97 = r_transf_tx.saldo_viv97 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_97 = c15_impt_viv_97[01,08],
                                  c15_impt_viv_97[10,15]

            IF r_transf_tx.diag_registro <> 501 THEN
               LET r_transf_rx.fecha_valor_viv = "01/01/0001"
            END IF

        PRINT
            COLUMN 001, "05"                                         ,-- Tipo de registro
            COLUMN 003, r_transf_tx.nss                              ,-- NSS
            COLUMN 014, r_transf_rx.curp                             ,-- CURP
            COLUMN 032, r_transf_tx.estado_sub_viv                   ,-- Estatus subcuenta vivienda
            COLUMN 033, r_transf_rx.fecha_valor_viv USING "YYYYMMDD" ,-- Fecha Valor de la vivienda
            COLUMN 041, c14_impt_viv_97                              ,-- Intereses Vivienda 97
            COLUMN 055, "00000000000000"                             ,-- Intereses Viv 97 en BDSVIV
            COLUMN 069, 282 SPACES                                    -- Filler

END REPORT





