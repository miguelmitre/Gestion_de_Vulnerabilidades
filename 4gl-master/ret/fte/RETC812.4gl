#################################################################################
# Proyecto         => SISTEMA SAFRE ( MEXICO )                                  #
# Owner            => E.F.P.                                                    #
# Programa RETC812 => PROCESO HISTORICO DE IV-RT (PROVISION Y GENERA. OPER 03)  #
#                     TIPO DE RETIRO A                                          #
# Sistema          => RET                                                       #
# Autor            => FRANCO ESTEBAN ULLOA VIDELA                               #
# Fecha            => 23 ENERO DEL 2004                                         #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 8 DE ENERO DE 2008                                        #
#                     Modificaciones para soporte de Multisiefores              #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 6 DE OCTUBRE DE 2008                                      #
#                     Modificaciones para validacion de monto constitutivo      #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 16 DE AGOSTO DE 2010                                      #
#                     Se agrega el campo "clave de aseguradora" en la           #
#                     generacion de la operacion 03                             #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE #glo #s_modulo
        s_modulo              RECORD LIKE seg_modulo.*

    DEFINE reg_3 RECORD #reg_3
        folio_oper_02         INTEGER
    END RECORD

    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD

    DEFINE reg_20 RECORD #loc #reg_20
        estado_marca          SMALLINT ,
        codigo_rechazo        SMALLINT ,
        marca_causa           SMALLINT ,
        fecha_causa           DATE
    END RECORD

    DEFINE reg_01 RECORD #glo #reg_01
        nss                   LIKE ret_transf_rx.nss             ,
        nombre_datamart       LIKE ret_transf_rx.nombre_datamart ,
        tipo_seguro           LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension          LIKE ret_transf_rx.tipo_pension    ,
        tipo_prestacion       LIKE ret_transf_rx.tipo_prestacion ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen   ,
        porcentaje_val        LIKE ret_transf_rx.porcentaje_val  ,
        diag_registro         LIKE ret_transf_rx.diag_registro   ,
        consecutivo           LIKE ret_transf_rx.consecutivo
    END RECORD

    DEFINE reg_02 RECORD #glo #reg_02
        recepcionado          LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        provisionado          LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud ,
        diagnosticado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gs_mto_pesos_total LIKE ret_transf_rx.mto_constitutivo

    DEFINE #glo #date
        f_fecha_carga         ,
        gd_primero_mes        ,
        HOY                   DATE

    DEFINE #glo #char
        gs_tipo_retiro        CHAR(001) ,
        v_saldo_dia           CHAR(100) ,
        comando               CHAR(110) ,
        G_LISTA_DET           CHAR(100) ,
        usuario               CHAR(008) ,
        enter                 CHAR(001) ,
        v_desmarca            CHAR(150) ,
        v_calc_mto_cons       CHAR(150) ,        
        v_provisiona          CHAR(150) 

    DEFINE #glo #smallint
        gs_num_siefores       ,-- Indica el numero de siefores que se usan actualmente
        gs_sieviv             ,-- Indica en que posicion se encuentra almacenada
                               -- la info de precio de vivienda
        s_codigo_afore        ,
        v_subcuenta           ,
        v_grupo               ,
        v_siefore             ,
        i                     ,
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

    CALL STARTLOG("RETC812.log")
    
    CALL init() #i
    CALL f_abre_ventana()
    CALL f_obtiene_precios_accion(HOY)
    CALL f_captura_datos()
    LET  gd_primero_mes = f_obten_primerdia_mes()
    
    DISPLAY " PROCESANDO INFORMACION               " AT 19,2 ATTRIBUTE(REVERSE)
    
    --PROCESO PRINCIPAL--

    DISPLAY " PROCESANDO PRIMER PASO               " AT 15,10 ATTRIBUTE(REVERSE)
    CALL primer_paso()             #pp -- DIAGNOSTICA Y CREA RET_SOLICITUD_TX

    DISPLAY " PROCESANDO SEGUNDO PASO               " AT 15,10 ATTRIBUTE(REVERSE)
    CALL segundo_paso()            #sp --CALCULA SALDOS Y APLICA VALIDACION DEL MONTO CONSTITUTIVO

    DISPLAY " PROCESANDO TERCER PASO               " AT 15,10 ATTRIBUTE(REVERSE)
    CALL tercer_paso()             #tp --ACTUALIZA SALDOS EN CERO PARA REGISTROS RECHAZADOS

    DISPLAY " GENERANDO ARCHIVO DE OPERACION 03    " AT 15,10 ATTRIBUTE(REVERSE)
    CALL genera_operacion03()      #go --GENERA OPER.03

    DISPLAY " DESMARCA DE CUENTAS                  " AT 15,10 ATTRIBUTE(REVERSE)
    CALL desmarca_no_liq()

    CLOSE WINDOW retc8121
    CALL f_abre_ventana()
    
    DISPLAY reg_3.folio_oper_02 AT 8,50

    DISPLAY "TOTAL REGISTROS DE SOLICITUDES : ",cont_det_pe02 AT 12,21

    SELECT COUNT(UNIQUE nss)
    INTO   cont_provi
    FROM   dis_provision
    WHERE  folio           = reg_3.folio_oper_02
    AND    tipo_movimiento = gs_tipo_mov

    DISPLAY "TOTAL REGISTROS PROVISIONADOS  : ",cont_provi    AT 14,21

    PROMPT " PROCESO FINALIZADO...< ENTER > PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc8121
END MAIN


FUNCTION init()
#i-------------
    LET HOY            = TODAY
    LET cont_det_pe02  = 0
    LET gs_sieviv      = 11
    LET gs_tipo_retiro = "A"

    --Obtenemos el numero de siefores actual en el sistema--
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore  > 0
    AND    codigo_siefore  < gs_sieviv

    SELECT *
    INTO   s_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_02.recepcionado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   reg_02.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   reg_02.diagnosticado
    FROM   ret_estado A
    WHERE  A.descripcion = "DIAGNOSTICADO"

    SELECT MAX(A.folio)
    INTO   reg_3.folio_oper_02
    FROM   ret_transf_rx A
    WHERE  A.tipo_retiro       = gs_tipo_retiro
    AND    A.estado_solicitud IN (reg_02.recepcionado ,
                                  reg_02.diagnosticado)

    SELECT A.movimiento
    INTO   gs_tipo_mov
    FROM   tab_retiro A
    WHERE  A.tipo_retiro = gs_tipo_retiro

    --- SALDO AL DIA ---
    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
    PREPARE eje_saldo_dia FROM v_saldo_dia
    
    --- PROVISION ---
    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona

    --- CALCULO MONTO CONSTITUTIVO ---
    LET v_calc_mto_cons = "EXECUTE FUNCTION fn_calc_constitutivo(?,?,?)"
    PREPARE eje_mto_cons FROM v_calc_mto_cons

END FUNCTION

#=============================================================================#
# ASIGNACION DE DIAGNOSTICOS                                                  #
#=============================================================================#
FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #char
        c1_estado_sub_viv     CHAR(1)

    DEFINE #loc #smallint
        s_nss_traspasado      ,
        v_diag_registro       SMALLINT


    --SELECCIONA TODOS LOS REGISTROS RECIBIDOS PARA EL TIPO DE RETIRO "A"
    DECLARE cur_2 CURSOR FOR
    SELECT A.nss             ,
           A.nombre_datamart ,
           A.tipo_seguro     ,
           A.tipo_pension    ,
           A.tipo_prestacion ,
           A.fecha_ini_pen   ,
           A.porcentaje_val  ,
           A.diag_registro   ,
           A.consecutivo     ,
           A.estado_sub_viv
    FROM   ret_transf_rx A
    WHERE  A.folio            = reg_3.folio_oper_02
    AND    A.estado_solicitud = reg_02.recepcionado
    AND    A.tipo_retiro      = gs_tipo_retiro

    FOREACH cur_2 INTO reg_01.*, c1_estado_sub_viv
        
        --Se guarda el diagnostico que originalmente trae la solicitud--
        LET v_diag_registro      = reg_01.diag_registro
        LET reg_01.diag_registro = 501 -- CTA ACEPTADA CON TRANSFERENCIA DE RECURSOS

        SELECT "OK"
        FROM   afi_mae_afiliado
        WHERE  n_seguro = reg_01.nss
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            LET reg_01.diag_registro = 503 --CUENTA INEXISTENTE
        ELSE
            SELECT "OK"
            FROM   cta_act_marca
            WHERE  nss        = reg_01.nss
            AND    marca_cod IN (120,130,140) -- CUENTA INHABILITADA
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET reg_01.diag_registro   = 502 -- CUENTA CON SALDO CERO
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
                    INTO   f_fecha_carga
                    FROM   ret_cza_lote
                    WHERE  folio = reg_3.folio_oper_02
                    
                    --VERIFICA QUE EL NSS ESTE MARCADO EN UN PROCESO DE TRANSFERENCIA IVRT--
                    SELECT "OK"
                    FROM   cta_act_marca
                    WHERE  nss           = reg_01.nss
                    AND    marca_cod     = 800
                    AND    fecha_ini     = f_fecha_carga
                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET reg_01.diag_registro = 506 --CUENTA EN PROCESO OPERATIVO EN LA AFORE
                    ELSE
                        --CHECA SI TIENE UN TRASPASO AFO AFO RECEPTORA--
                    
                        CALL checa_tra_afo(reg_01.nss, reg_01.fecha_ini_pen) #cta
                            RETURNING s_nss_traspasado
                    
                        IF s_nss_traspasado = 1 THEN
                           --SOLO SE VALIDAN LOS QUE NO SON POR MUERTE--
                           IF reg_01.tipo_pension = "IN" OR
                              reg_01.tipo_pension = "IP" THEN
                              --No es por Muerte
                              IF v_diag_registro != 302 THEN
                                  LET reg_01.diag_registro = 505
                              END IF
                           END IF
                        END IF -- Traspaso
                    END IF -- Marcado Transferencia
                END IF -- Laudo Judicial
            END IF -- Saldo Cero
        END IF -- Inexistente en afore

        IF (reg_01.fecha_ini_pen < "07/01/1997") AND
           (reg_01.diag_registro <> 503 AND
            reg_01.diag_registro <> 506 AND
            reg_01.diag_registro <> 505) THEN
           IF reg_01.tipo_pension = "IN" OR reg_01.tipo_pension = "IP" THEN
              LET reg_01.diag_registro = 502
           END IF
        END IF

        INSERT INTO ret_transf_tx
        VALUES (reg_01.nss           ,
                reg_01.consecutivo   ,
                reg_3.folio_oper_02  ,
                reg_01.diag_registro ,
                c1_estado_sub_viv    ,
                0                    ,--monto_accion_97
                0                    ,--monto_accion_cv
                0                    ,--monto_accion_so
                0                    ,--monto_saldo_viv97
                0                     --mto_const_afore
               )

    END FOREACH
END FUNCTION

#=============================================================================#
# Objetivo : Calcular saldos para los registros tipo retiro A                 #
#                                                                             #
#=============================================================================#
FUNCTION segundo_paso()
#tp------------------
    DEFINE
        v_diag_procesar       LIKE ret_transf_rx.diag_registro

    DEFINE reg_14 RECORD #loc #reg_14
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE reg_15 RECORD
        monto_accion_97       DECIMAL(16,6) ,
        monto_accion_cv       DECIMAL(16,6) ,
        monto_accion_viv      DECIMAL(16,2) ,
        monto_accion_so       DECIMAL(16,6) ,
        monto_accion_es       DECIMAL(16,6) ,
        monto_accion_esp      DECIMAL(16,6) ,
        monto_accion_269      DECIMAL(16,6) ,
        monto_accion_rcv      DECIMAL(16,6) ,
        mto_pesos_total       LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_porc RECORD
        p_ret97    like ret_transf_rx.porcentaje_ret97 ,
        p_cv       like ret_transf_rx.porcentaje_cv    ,
        p_cs       like ret_transf_rx.porcentaje_cs    ,
        p_viv      like ret_transf_rx.porcentaje_viv  
    END RECORD
    





    DEFINE #loc #char
        estado_sub_viv        CHAR(001)

    DEFINE #loc #smallint
        ls_grupo              ,
        v_subcta              ,
        ls_siefore            ,        --Contador para los ciclos for
        v_cont_reg            SMALLINT --Contador de registros

    DEFINE #loc #decimal
        monto_viv_ori_02      DECIMAL(16,6)


    ---------------------------------------------------------------------------
    --VERIFICA SI EXISTEN DATOS PARA EL TIPO DE PENSION Y EL TIPO DE RETIRO "A"
    ---------------------------------------------------------------------------
    SELECT COUNT(*)
    INTO   i_tot_registros
    FROM   ret_transf_rx A , ret_transf_tx B
    WHERE  A.folio         = reg_3.folio_oper_02
    AND    A.tipo_retiro   = gs_tipo_retiro
    AND    A.folio         = B.folio
    AND    A.nss           = B.nss
    AND    A.consecutivo   = B.consecutivo
    AND    B.diag_registro = 501          -- CUENTA ACEPTADA

    IF i_tot_registros = 0 THEN
        RETURN   -- SALE, NO HAY NADA QUE PROCESAR
    END IF

    LET v_cont_reg = 0

    ------------------------------------------------------------------------
    --SELECCIONA LOS DATOS PARA EL TIPO DE PENSION Y EL TIPO DE RETIRO "A"
    ------------------------------------------------------------------------
    DECLARE cur_33 CURSOR FOR
    SELECT A.nss             ,
           A.nombre_datamart ,
           A.tipo_seguro     ,
           A.tipo_pension    ,
           A.tipo_prestacion ,
           A.fecha_ini_pen   ,
           A.porcentaje_val  ,
           B.diag_registro   ,    -- Diagnosticado en el primer paso
           A.consecutivo     ,
           A.saldo_viv97     ,    -- Monto de Vivienda Operacion 02
           A.estado_sub_viv  ,
           A.diag_registro   ,     -- Diagnostico de procesar
           A.porcentaje_ret97,
           A.porcentaje_cv   ,
           A.porcentaje_cs   ,
           A.porcentaje_viv


    FROM   ret_transf_rx A , 
           ret_transf_tx B
    WHERE  A.folio         = reg_3.folio_oper_02
    AND    A.tipo_retiro   = gs_tipo_retiro
    AND    A.folio         = B.folio
    AND    A.nss           = B.nss
    AND    A.consecutivo   = B.consecutivo
    AND    B.diag_registro = 501          -- CUENTA ACEPTADA

    FOREACH cur_33 INTO reg_01.*, monto_viv_ori_02, estado_sub_viv, v_diag_procesar, lr_porc.*

        IF (lr_porc.p_ret97 = 100 AND
            lr_porc.p_cv   = 100 AND
            lr_porc.p_cs   = 100 AND
            lr_porc.p_viv  = 100 ) THEN

            LET v_diag_procesar = 300
            
        END IF

        -- Inicializamos variables
        LET v_cont_reg = v_cont_reg + 1

        LET ls_grupo                = 0
        LET v_subcta                = 0

        LET gs_mto_pesos_total      = 0
        LET reg_15.monto_accion_97  = 0
        LET reg_15.monto_accion_cv  = 0
        LET reg_15.monto_accion_viv = 0
        LET reg_15.monto_accion_so  = 0
        LET reg_15.monto_accion_es  = 0
        LET reg_15.monto_accion_esp = 0
        LET reg_15.monto_accion_269 = 0
        LET reg_15.monto_accion_rcv = 0
        LET reg_15.mto_pesos_total  = 0

        DISPLAY " PROCESANDO EL REGISTRO ",v_cont_reg USING "&&&&&&" AT 16,10 ATTRIBUTE(REVERSE)
        DISPLAY " TOTAL REGISTRO         ",i_tot_registros USING "&&&&&&" AT 17,10 ATTRIBUTE(REVERSE)

        IF monto_viv_ori_02 IS NULL OR monto_viv_ori_02 < 0 THEN-- ojo poner esta validacion en la carga
            LET monto_viv_ori_02 = 0
        END IF

        --Genera la tabla con toda la informacion de la cuenta (tmp_dis_cuenta)
        CALL genera_tmp_cuenta (reg_01.nss)

        --Crea la tabla donde se almacenan los montos a provisionar
        CALL genera_tmp_mto_no_pago() 

        IF v_diag_procesar = 302 THEN
            CALL f_gen_aportes_traspaso(reg_01.*)
        END IF

        DECLARE c_saldo_dia  CURSOR FOR eje_saldo_dia
        FOREACH c_saldo_dia  USING reg_01.nss     ,
                                   v_subcta       ,
                                   ls_grupo       ,
                                   HOY
                             INTO  reg_14.subcuenta   ,
                                   reg_14.siefore     ,
                                   reg_14.monto_acc   ,
                                   reg_14.monto_pesos

            IF reg_14.subcuenta = 1 OR reg_14.subcuenta = 2  OR  reg_14.subcuenta = 5  OR
               reg_14.subcuenta = 6 OR reg_14.subcuenta = 9  OR  reg_14.subcuenta = 4 THEN

                IF reg_01.tipo_pension <> "AS" AND 
                   reg_01.tipo_pension <> "OR" AND 
                   reg_01.tipo_pension <> "VI" AND 
                   reg_01.tipo_pension <> "VO" THEN

                    -- Si es un tipo de pension que no es por muerte, aplicamos calculo de
                    -- saldo al dia - aportaciones posteriores
                    LET reg_14.monto_acc = f_obten_ap_posterior(reg_01.nss           ,
                                                                reg_01.fecha_ini_pen ,
                                                                v_diag_procesar      ,
                                                                reg_14.*)
                    
                    -- Se aplica el porcentaje de IP
                    IF reg_01.tipo_seguro = "RT" AND reg_01.tipo_pension = "IP" AND reg_14.monto_acc > 0 THEN
                        LET reg_14.monto_acc = (reg_14.monto_acc * reg_01.porcentaje_val) / 100
                    END IF
                END IF
                
                IF reg_14.subcuenta = 4 THEN

                    LET reg_14.monto_acc = redondea_val(reg_14.monto_acc, 2)

                    IF reg_14.monto_acc > monto_viv_ori_02  THEN
                        -- Si el monto de vivienda calculado es mayor que el informado, No se provisiona --
                        LET reg_01.diag_registro    = 507
                        LET reg_15.monto_accion_viv = 0
                        LET reg_14.monto_acc        = 0
                    ELSE
                        IF estado_sub_viv <> 1 THEN
                            -- Si el estatus de vivienda es <> 1, no se provisiona --
                            LET reg_15.monto_accion_viv = 0
                            LET reg_14.monto_acc        = 0
                        ELSE
                            LET reg_15.monto_accion_viv = reg_14.monto_acc
                        END IF
                    END IF
                ELSE
                    LET reg_15.monto_accion_rcv = reg_15.monto_accion_rcv + reg_14.monto_acc
                END IF

                IF reg_14.monto_acc > 0 THEN
                    
                    IF reg_14.siefore = gs_sieviv THEN
                        --El elemento gs_sieviv del arreglo de precios contiene la informacion de la siefore 11--
                        LET reg_14.monto_pesos = reg_14.monto_acc * gar_precio_acc[gs_sieviv].precio_dia
                        LET reg_14.monto_pesos = redondea_val(reg_14.monto_pesos, 2)
                    ELSE
                        LET ls_siefore = reg_14.siefore
                        LET reg_14.monto_pesos = reg_14.monto_acc * gar_precio_acc[ls_siefore].precio_dia
                    END IF
                    
                    -- Convertimos a pesos para obtener el monto total a provisionar
                    LET gs_mto_pesos_total = gs_mto_pesos_total + reg_14.monto_pesos

                    INSERT INTO tmp_mto_no_pago VALUES(reg_14.siefore   , 
                                                       reg_14.subcuenta ,
                                                       reg_14.monto_pesos)
                END IF -- reg_14.monto_acc > 0
            END IF -- Subcuentas
        END FOREACH -- Saldo al dia

        IF (reg_15.monto_accion_rcv IS NOT NULL AND reg_15.monto_accion_rcv > 0)
           OR (reg_15.monto_accion_viv > 0) THEN

            -- Si existe monto en RCV o VIV se calcula el proporcional para el 
            -- monto constitutivo y se hace la provision
            DECLARE c_mto_const CURSOR FOR eje_mto_cons

            FOREACH c_mto_const USING reg_01.nss              ,
                                      reg_01.consecutivo      ,
                                      gs_mto_pesos_total
                                INTO  reg_14.subcuenta   ,
                                      reg_14.monto_pesos 

                LET reg_15.mto_pesos_total = reg_15.mto_pesos_total + reg_14.monto_pesos

                IF reg_14.subcuenta = 4 THEN
                    --El elemento gs_sieviv del arreglo de precios contiene la informacion de la siefore 11--
                    LET reg_14.monto_acc   = reg_14.monto_pesos / gar_precio_acc[gs_sieviv].precio_dia
                    LET reg_14.monto_acc   = redondea_val(reg_14.monto_acc, 2)
                    LET reg_14.siefore     = gs_sieviv
                ELSE
                    LET reg_14.monto_acc = reg_14.monto_pesos / gar_precio_acc[ls_siefore].precio_dia
                    LET reg_14.siefore     = ls_siefore
                END IF

                CALL f_provisiona_subcta(reg_01.nss         ,
                                         reg_14.siefore     ,
                                         reg_14.subcuenta   ,
                                         reg_01.consecutivo ,
                                         reg_14.monto_acc   ,
                                         reg_14.monto_pesos )

                -- Se actualizan los montos de acuerdo a lo calculado en el proporcional del 
                -- monto constitutivo
                CASE reg_14.subcuenta
                    WHEN 1
                        LET reg_15.monto_accion_97  = reg_14.monto_acc
                    WHEN 2
                        LET reg_15.monto_accion_cv  = reg_14.monto_acc
                        LET reg_15.monto_accion_269 = reg_15.monto_accion_269 + reg_14.monto_acc
                    WHEN 4
                        LET reg_15.monto_accion_viv = reg_14.monto_acc
                    WHEN 5
                        LET reg_15.monto_accion_so  = reg_14.monto_acc
                    WHEN 6
                        LET reg_15.monto_accion_es  = reg_14.monto_acc
                        LET reg_15.monto_accion_269 = reg_15.monto_accion_269 + reg_14.monto_acc
                    WHEN 9
                        LET reg_15.monto_accion_esp = reg_14.monto_acc
                        LET reg_15.monto_accion_269 = reg_15.monto_accion_269 + reg_14.monto_acc
                END CASE
            
            END FOREACH -- Provision
            
            IF (reg_15.monto_accion_rcv IS NOT NULL AND reg_15.monto_accion_rcv > 0) THEN
                -- Si existe monto en RCV se inserta en ret_monto_siefore
                
                INSERT INTO ret_monto_siefore
                    VALUES (reg_01.nss              ,--Nss
                            reg_01.consecutivo      ,--Consecutivo
                            reg_3.folio_oper_02     ,--Folio
                            gs_tipo_retiro          ,--Tipo de Retiro
                            3                       ,--Tipo de Operacion (transferencias)
                            ls_siefore              ,--Siefore
                            reg_15.monto_accion_97  ,--Acciones Retiro 97
                            reg_15.monto_accion_269 ,--Acciones CV
                            reg_15.monto_accion_so  ,--Acciones Cuota social
                            0                        --Acciones Retiro92
                           )
                
                IF SQLCA.SQLCODE != 0 THEN
                    PROMPT "ERROR AL INSERTAR MONTOS PARA SIEFORE ", ls_siefore, " PRESIONE ENTER ",
                           "PARA CONTINUAR" ATTRIBUTE(REVERSE) FOR CHAR enter
                END IF
            END IF -- monto rcv <> 0
        END IF -- monto rcv <> 0 or monto viv <> 0

        --Si el importe de las siefores es 0 se informa con diagnostico 502--
        IF reg_15.monto_accion_rcv <= 0 AND
           reg_15.monto_accion_viv <= 0 AND reg_01.diag_registro != 507 THEN
            LET reg_01.diag_registro = 502
        END IF

        IF reg_01.diag_registro = 501 THEN
            --SE ACTUALIZA VIVIENDA Y MONTO CONSTITUTIVO CALCULADO 
            UPDATE ret_transf_tx
            SET    saldo_viv97        = reg_15.monto_accion_viv ,
                   mto_const_afore    = reg_15.mto_pesos_total
            WHERE  folio              = reg_3.folio_oper_02
            AND    nss                = reg_01.nss
            AND    consecutivo        = reg_01.consecutivo
        ELSE
            --SE ACTUALIZA SALDO DE VIVIENDA y DIAGNOSTICO--
            UPDATE ret_transf_tx
            SET    diag_registro      = reg_01.diag_registro    ,
                   saldo_viv97        = reg_15.monto_accion_viv
            WHERE  folio              = reg_3.folio_oper_02
            AND    nss                = reg_01.nss
            AND    consecutivo        = reg_01.consecutivo
        END IF

        -- Se actualiza la fecha de vivienda y el estado de la solicitud
        UPDATE ret_transf_rx
        SET    fecha_valor_viv  = gd_primero_mes  
        WHERE  folio            = reg_3.folio_oper_02
        AND    nss              = reg_01.nss
        AND    consecutivo      = reg_01.consecutivo

    END FOREACH --SIGUIENTE NSS
END FUNCTION

#============================================================================#
# Actualiza saldos en cero para registros rechazados                         #
#============================================================================#
FUNCTION tercer_paso()
#cp-------------------
    DEFINE reg_08 RECORD #loc #reg_08
        nss                   LIKE ret_transf_rx.nss            ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen  ,
        diag_registro         LIKE ret_transf_rx.diag_registro  ,
        tipo_pension          LIKE ret_transf_rx.tipo_pension   ,
        consecutivo           LIKE ret_transf_rx.consecutivo    ,
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
    SELECT A.nss            ,
           A.fecha_ini_pen  ,
           B.diag_registro  ,
           A.tipo_pension   ,
           A.consecutivo    ,
           A.estado_sub_viv
    FROM   ret_transf_rx A , ret_transf_tx B
    WHERE  A.folio           = reg_3.folio_oper_02
    AND    A.tipo_retiro     = gs_tipo_retiro
    AND    A.estado_sub_viv IN ("2","3","5")   --NO PAGA VIVIENDA
    AND    A.folio           = B.folio
    AND    A.nss             = B.nss
    AND    A.consecutivo     = B.consecutivo

    FOREACH cur_7 INTO reg_08.*
        IF reg_08.diag_registro = 502 OR    --CUENTA CON SALDOS EN CERO
           reg_08.diag_registro = 506 OR    --CUENTA EN PROCESO OPERATIVO
           reg_08.diag_registro = 505 THEN  --CUENTA SIN MOVIMIENTOS HISTORICOS

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
            SELECT SUM(acciones_cv)    ,
                   SUM(acciones_cs)    ,
                   SUM(acciones_ret97)
            INTO   v_sum_acciones_cv   ,
                   v_sum_acciones_cs   ,
                   v_sum_acciones_ret97
            FROM   ret_monto_siefore
            WHERE  nss   = reg_08.nss
            AND    folio = reg_3.folio_oper_02

            IF v_sum_acciones_cv    = 0 AND
               v_sum_acciones_cs    = 0 AND
               v_sum_acciones_ret97 = 0 THEN

                --SI SALDOS DE RCV = 0 ENTONCES DIAGNOSTICA EN 502
                UPDATE ret_transf_tx
                SET    saldo_viv97    = 0 ,
                       diag_registro  = 502
                WHERE  folio       = reg_3.folio_oper_02
                AND    nss         = reg_08.nss
                AND    consecutivo = reg_08.consecutivo
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
        WHERE  folio       = reg_3.folio_oper_02
        AND    nss         = reg_08.nss
        AND    consecutivo = reg_08.consecutivo
    END FOREACH
END FUNCTION

--GENERA ARCHIVO DE LA OPERACION 03--
FUNCTION genera_operacion03()
#go03-----------------------

    DEFINE #loc #r_transf_rx
        r_transf_rx           RECORD LIKE ret_transf_rx.*     ,
        r_transf_tx           RECORD LIKE ret_transf_tx.*     ,
        r_monto_siefore       RECORD LIKE ret_monto_siefore.*

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_viv      CHAR(100)

    DEFINE #loc #char
        hora              CHAR(5)

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = s_modulo.ruta_envio CLIPPED, "/", "DET-NSS-A-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = s_modulo.ruta_envio CLIPPED, "/", "DET-SIE-A-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = s_modulo.ruta_envio CLIPPED, "/", "DET-VIV-A-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET = s_modulo.ruta_envio CLIPPED, "/", "DET-A"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    LET i_tot_registros = 0

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_8 CURSOR FOR
    SELECT A.*,
           B.*
    FROM   ret_transf_rx  A,
           ret_transf_tx  B
    WHERE  A.folio        = reg_3.folio_oper_02
    AND    A.folio        = B.folio
    AND    A.nss          = B.nss
    AND    A.consecutivo  = B.consecutivo
    AND    A.tipo_retiro  = gs_tipo_retiro

    FOREACH cur_8 INTO r_transf_rx.*, r_transf_tx.*

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

        --SE CAMBIA ESTADO A PROCESADO
        UPDATE ret_transf_rx
        SET    estado_solicitud = reg_02.procesado
        WHERE  folio            = reg_3.folio_oper_02
        AND    nss              = r_transf_tx.nss
        AND    consecutivo      = r_transf_tx.consecutivo

    END FOREACH

    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando

    INSERT INTO ret_seguimiento
    VALUES(reg_3.folio_oper_02 ,#folio
           ""                  ,#tipo_oper_recep
           ""                  ,#fecha_recepcion
           "03"                ,#tipo_oper_envio
           HOY                 ,#fecha_envio
           reg_02.procesado    ,#status
           0
          )

    LET hora = TIME

    INSERT INTO ret_ctr_envio_lote
    VALUES (HOY                 ,#fecha_genera
            gs_tipo_retiro      ,#tipo_retiro
            reg_3.folio_oper_02 ,#folio
            NULL                ,#fecha_envio
            NULL                ,#fecha_reverso
            hora                ,#hora_genera
            NULL                ,#hora_envio
            usuario             ,#usuario_genera
            NULL                ,#usuario_envio
            NULL                ,#usuario_reverso
            reg_02.procesado    ,#estado
            cont_det_pe02        #total_registros
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

#============================================================================#
# VERIFICA QUE TENGA TRASPASOS POSTERIORES A LA FECHA INICIO DE PENSION      #
#============================================================================#
FUNCTION checa_tra_afo(reg_4)
#cta-------------------------
    DEFINE reg_4 RECORD  #loc #reg_4
                    nss                  LIKE ret_transf_rx.nss,
                    fecha_ini_pen        LIKE ret_transf_rx.fecha_ini_pen
                 END RECORD

    DEFINE #loc #smallint
        s_nss_traspasado     SMALLINT

    --SE VERIFICA QUE EL TRASPASO SEA POSTERIOR A LA FIP (EN TABLA HISTORICA DE TRASPASOS RECEPTORA)
    SELECT "OK"
    FROM   taa_rcv_recepcion
    WHERE  nss               = reg_4.nss
    AND    fecha_mov_banxico > reg_4.fecha_ini_pen
    AND    ident_operacion   = "09"  --TRASPASO ORDINARIO
    AND    estado            = 2     --LIQUIDADO
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       LET s_nss_traspasado = 1
    ELSE
       LET s_nss_traspasado = 0
    END IF

    RETURN s_nss_traspasado
END FUNCTION

#============================================================================#
# DESMARCA CUENTAS QUE NO SE PAGAN                                           #
#============================================================================#
FUNCTION desmarca_no_liq()
#dnl----------------------
    DEFINE
        nssa                  LIKE ret_transf_rx.nss       ,
        consec                LIKE ret_transf_rx.consecutivo,
        vdiag                 LIKE ret_transf_tx.diag_registro
    DEFINE
        vcont                 SMALLINT

    DECLARE cur_des CURSOR FOR
    SELECT  A.nss, A.consecutivo, B.diag_registro
    FROM    ret_transf_rx   A,
            ret_transf_tx   B
    WHERE   A.folio         = reg_3.folio_oper_02
    AND     A.folio         = B.folio
    AND     A.nss           = B.nss
    AND     A.consecutivo   = B.consecutivo
    AND     A.tipo_retiro   = gs_tipo_retiro
    AND     B.diag_registro NOT IN (501)  --(501,507)

    FOREACH cur_des INTO nssa, consec, vdiag

        --SI TIENE DIAGNOSTICO 507 Y NO SE PROVISIONO RCV (SE DESMARCA)
        IF vdiag = 507 THEN
           LET vcont = 0
           SELECT COUNT(*)
           INTO   vcont
           FROM   dis_provision
           WHERE  nss               = nssa
           AND    consecutivo_lote  = consec
           AND    subcuenta        != 4

           IF vcont > 0 then
              CONTINUE FOREACH -- Debe desmarcarse en la liquidacion
           END IF
        END IF

        LET reg_20.estado_marca = 40 --DESMARCA POR VALIDACIÓN INTERNA--
        LET reg_20.marca_causa  = 0

        LET v_desmarca =" EXECUTE PROCEDURE desmarca_cuenta('",
                         nssa,"',",gs_tipo_mov,",",consec,",",
                         reg_20.estado_marca,",",reg_20.marca_causa
                         ,",' ",usuario,"')"

        PREPARE eje_desmar FROM v_desmarca
        EXECUTE eje_desmar
    END FOREACH
END FUNCTION


-----------------------------------------------------------------------------
--GENERA TABLA TEMPORAL CON INFORMACION DE LAS TABLAS dis_cuenta HISTORICAS---
-----------------------------------------------------------------------------

FUNCTION genera_tmp_cuenta(p_nss)
#gtc-----------------------------
     DEFINE #local #char
         p_nss                CHAR(11)  ,
         v_nombre_tabla       CHAR(20)  ,
         sel_his              CHAR(1500)

     WHENEVER ERROR CONTINUE
         DROP TABLE tmp_dis_cuenta;
     WHENEVER ERROR STOP

     DECLARE cur_his CURSOR FOR
     SELECT tabname
     FROM   systables
     WHERE  tabname matches "dis_cuenta??"

     FOREACH cur_his INTO v_nombre_tabla

         LET sel_his = sel_his CLIPPED,
                       " SELECT * ",
                       " FROM ",v_nombre_tabla          ,
                       " WHERE nss = ","'",p_nss,"'"  ,
                       " UNION ALL "
     END FOREACH

     CLOSE cur_his

     LET sel_his = sel_his CLIPPED,
                   " SELECT * ",
                   " FROM dis_cuenta ",
                   " WHERE nss = ","'",p_nss,"'"  ,
                   " INTO TEMP tmp_dis_cuenta "

     PREPARE eje_sel_his FROM sel_his

     EXECUTE eje_sel_his

     CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta (folio            ,
                                                     consecutivo_lote ,
                                                     subcuenta        ,
                                                     siefore
                                                    )
     UPDATE STATISTICS FOR TABLE tmp_dis_cuenta
END FUNCTION


FUNCTION genera_tmp_mto_no_pago()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_mto_no_pago
        CREATE TEMP TABLE tmp_mto_no_pago
        (
             siefore        SMALLINT,
             subcuenta      SMALLINT,
             monto_pesos    DECIMAL(16,6)
        )
    WHENEVER ERROR STOP

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

    OPEN WINDOW retc8121 AT 4,4 WITH FORM "RETC8121" ATTRIBUTE(BORDER)
    DISPLAY "   < Ctrl-C > Salir                                        RETIRO 'A'       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC812            CONFIRMACION DE TRANSFERENCIA IV-RT                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION


FUNCTION f_captura_datos()

    INPUT BY NAME reg_3.folio_oper_02 WITHOUT DEFAULTS
        AFTER FIELD folio_oper_02
            IF reg_3.folio_oper_02 IS NULL THEN
                ERROR"    FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
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
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE " ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_02
                ELSE
                    SELECT "OK"
                    FROM  ret_transf_rx A
                    WHERE A.folio             = reg_3.folio_oper_02
                    AND   A.estado_solicitud IN( reg_02.recepcionado,
                                                 reg_02.diagnosticado )
                    AND   A.tipo_retiro       = gs_tipo_retiro
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "    FOLIO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
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
                WHERE  A.folio  = reg_3.folio_oper_02
                AND    A.folio IN ( SELECT B.folio
                                    FROM   ret_transf_rx B
                                    WHERE  B.folio       = A.folio
                                    AND    B.tipo_retiro = gs_tipo_retiro )

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE " ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_02
                ELSE
                    SELECT "OK"
                    FROM   ret_transf_rx A
                    WHERE  A.folio             = reg_3.folio_oper_02
                    AND    A.estado_solicitud IN ( reg_02.recepcionado,
                                                   reg_02.diagnosticado )
                    AND    A.tipo_retiro       = gs_tipo_retiro
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "    FOLIO YA PROCESADO CON ANTERIORIDAD"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

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
                PROMPT " PROCESO CANCELADO...< ENTER > PARA SALIR "
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


FUNCTION f_gen_aportes_traspaso(lr_datos)

    DEFINE lr_datos RECORD #loc #lr_datos
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

    DEFINE
        v_subcuenta_tra       SMALLINT

    DEFINE
        v_monto_tra           DECIMAL(16,6)

    DEFINE
        v_stmt0               CHAR(200)

            -----------------------------------------------------------------
            -- PREPARACION DE LOS MONTOS DEL TRASPASO QUE NO SE PAGAN POR NSS
            -----------------------------------------------------------------
            WHENEVER ERROR CONTINUE
                DROP TABLE tmp_aporte_traspaso
                CREATE TEMP TABLE tmp_aporte_traspaso
                (
                     subcuenta   SMALLINT,
                     monto       DECIMAL(16,6)
                )
            WHENEVER ERROR STOP

            LET v_stmt0 = "EXECUTE FUNCTION fn_aporte_traspaso(?,?,?,?)"

            PREPARE exe1 FROM v_stmt0

            DECLARE cur1 CURSOR FOR exe1

            FOREACH cur1 USING lr_datos.nss           ,
                               lr_datos.consecutivo   ,
                               lr_datos.fecha_ini_pen ,
                               HOY
                         INTO  v_subcuenta_tra  ,
                               v_monto_tra
                INSERT INTO tmp_aporte_traspaso VALUES(v_subcuenta_tra, v_monto_tra)
            END FOREACH

END FUNCTION

#=============================================================================#
# Funcion  : f_obten_ap_posterior                                             #
# Objetivo : Obtiene las aportaciones posteriores al bimestre de la fecha de  #
#            inicio de pension para la siefore y subcuenta de un nss. Ademas  #
#            le da seguimiento a la transferencia de decimos y al calculo del #
#            porcentaje de traspaso en caso de tener alguna de las dos.       #
#=============================================================================#
FUNCTION f_obten_ap_posterior(p_nss, p_fip, p_diag_procesar, lr_montos)

    DEFINE lr_montos RECORD #loc #lr_montos
        subcuenta           SMALLINT      ,
        siefore             SMALLINT      ,
        monto_acc           DECIMAL(16,6) ,
        monto_pesos         DECIMAL(16,6)
    END RECORD

    DEFINE
        p_fip               DATE

    DEFINE
        p_nss               CHAR(11) ,
        lc_exe              CHAR(100)

    DEFINE
        p_diag_procesar     ,
        ls_sie              ,
        ls_subcta           SMALLINT

    DEFINE
	    ld_porc_no_pago	    ,
        ld_monto_acc        DECIMAL(16,6)

    LET ld_monto_acc    = 0
    LET ld_porc_no_pago = 0
    LET lc_exe          = " EXECUTE FUNCTION fn_aportes_posteriores(?,?,?,?) "

    PREPARE prp_ap_pos FROM lc_exe
    DECLARE cur_ap_pos CURSOR FOR prp_ap_pos

    FOREACH cur_ap_pos USING p_nss               ,
                             lr_montos.siefore   ,
                             lr_montos.subcuenta ,
                             p_fip
                       INTO  ls_sie     ,
                             ls_subcta  ,
                             ld_monto_acc -- Monto de no pago

        IF ld_monto_acc IS NULL OR ld_monto_acc < 0 THEN
            LET ld_monto_acc = 0
        END IF

    END FOREACH

    IF p_diag_procesar = 302 THEN

        -- Obtiene el porcentaje del monto de no pago
        SELECT NVL(monto,0)
        INTO   ld_porc_no_pago
        FROM   tmp_aporte_traspaso
        WHERE  subcuenta = ls_subcta

        LET ld_monto_acc = ld_monto_acc + ld_porc_no_pago

    END IF

    LET lr_montos.monto_acc = lr_montos.monto_acc - ld_monto_acc

    IF lr_montos.monto_acc < 0 THEN
        LET lr_montos.monto_acc = 0
    END IF

    RETURN lr_montos.monto_acc

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

    IF ld_pesos < 0 THEN
        DECLARE cur_prov CURSOR FOR eje_provisiona
        OPEN cur_prov USING reg_3.folio_oper_02 ,--folio
                            folio_sua           ,--folio_sua
                            p_nss               ,--nss
                            p_subcta            ,--subcuenta
                            gs_tipo_mov         ,--tipo_movimiento
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
    END IF

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
        c16_mto_const         CHAR(016) ,

        c5_porcentaje_val     CHAR(005) ,
        c5_porcentaje_ret97   CHAR(005) ,
        c5_porcentaje_cv      CHAR(005) ,
        c5_porcentaje_cs      CHAR(005) ,
        c5_porcentaje_viv     CHAR(005) ,
        c15_mto_const         CHAR(015) 


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

            LET c6_porcentaje_val = r_transf_rx.porcentaje_val USING "&&&.&&"
            LET c5_porcentaje_val = c6_porcentaje_val[01,03],
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


            -- Formateamos el monto constitutivo calculado
            LET c16_mto_const = r_transf_rx.mto_constitutivo USING "&&&&&&&&&&&&&.&&"
            LET c15_mto_const = c16_mto_const[01,13],
                                c16_mto_const[15,16]

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
            COLUMN 282, c15_mto_const                                   , -- Monto constitutivo calculado por la Afore
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
            COLUMN 001, "05"                                         , -- Tipo de registro
            COLUMN 003, r_transf_tx.nss                              , -- NSS
            COLUMN 014, r_transf_rx.curp                             , -- CURP
            COLUMN 032, r_transf_tx.estado_sub_viv                   , -- Estatus subcuenta vivienda
            COLUMN 033, r_transf_rx.fecha_valor_viv USING "YYYYMMDD" , -- Fecha Valor de la vivienda
            COLUMN 041, c14_impt_viv_97                              , -- Intereses Vivienda 97
            COLUMN 055, "00000000000000"                             , -- Intereses Viv 97 en BDSVIV
            COLUMN 069, 282 SPACES                                     -- Filler

END REPORT
