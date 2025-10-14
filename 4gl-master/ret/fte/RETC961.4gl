#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC961  => GENERA Y PROVISIONA SALDOS DE TRANFERENCIAS ISSSTE        #
#Fecha creacion    => 22 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 17 DE MAYO DE 2010                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Se incluye el tipo de retiro N en la lista de retiros     #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado_issste.estado_solicitud ,
        rechazado             LIKE ret_estado_issste.estado_solicitud ,
        provisionado          LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #glo #date
        gdt_fecha_viv         ,
        HOY                   DATE

    DEFINE #glo #char
        gc_tipo_ret           CHAR(001) ,
        enter                 CHAR(001) ,
        gs_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_peiss              ,
        gs_xxi                ,
        gs_diag_no_redim      ,
        gs_tipo_op            ,
        gs_procesa            ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore       SMALLINT

    DEFINE #glo #integer
        gs_ult_folio          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC961.log")

    CALL f_tablas_tmp()
    CALL init() #i
    CALL f_obtiene_precios_accion(HOY)

    CALL f_despliega_info() RETURNING gs_procesa    ,
                                      gs_ult_folio

    IF gs_procesa THEN
        CALL f_abre_ventana()
        CALL primer_paso(gs_ult_folio)  #-- Diagnostica las cuentas a provisionar
        CALL segundo_paso(gs_ult_folio) #-- Provision de solicitudes
        CALL tercer_paso(gs_ult_folio)  #-- Afecto tablas fisicas
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    DEFINE
        ls_dias_hab     SMALLINT

    LET HOY                 = TODAY
    LET gs_tipo_op          = 43
    LET gs_sieviv           = 12
    LET ls_dias_hab         = 1
    LET gs_diag_no_redim    = 502

    ----- FECHA DE VIVIENDA (HOY + 1 dia habil)  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_siguiente ( ?,? )"
    PREPARE eje_dia_sig FROM lc_prepare

    LET lc_prepare = " "

    EXECUTE eje_dia_sig USING HOY, ls_dias_hab INTO gdt_fecha_viv

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gs_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_xxi
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*XXI*"

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia_isss (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    --- CALCULO MONTO CONSTITUTIVO ---
    LET lc_prepare = "EXECUTE FUNCTION fn_calc_constitutivo_issste(?,?,?,?)"
    PREPARE eje_mto_cons FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    provision de transferencias                            #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lar_ret ARRAY[6] OF RECORD
        tipo_retiro        LIKE ret_trans_issste.tipo_retiro ,
        desc_retiro        LIKE tab_ret_issste.descripcion   ,
        num_cap            INTEGER                           ,
        num_prov           INTEGER
    END RECORD


    DEFINE lr_soli RECORD
        tipo_retiro        SMALLINT                                 ,
        edo_soli           LIKE ret_trans_issste.estado_solicitud   ,
        num_regs           INTEGER
    END RECORD

    DEFINE li_folio_trans  LIKE ret_trans_issste.folio

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    OPEN WINDOW retc9611 AT 4,4 WITH FORM "RETC9611" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                        <CTRL-P> PROVISIONAR   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC961      GENERACION DE MONTOS DE TRANSFERENCIAS ISSSTE                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    -- Obtenemos el folio de la operacion deseada
    SELECT MAX(folio)
    INTO   li_folio_trans
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.capturado

    IF STATUS = NOTFOUND THEN
        PROMPT " NO EXISTEN REGISTROS PARA PROVISIONAR ... <ENTER> PARA SALIR " FOR CHAR enter
        LET ls_flag = 0
    ELSE
        DISPLAY "FOLIO DE TRANSFERENCIA  :  ", li_folio_trans AT 16,4

        FOR ls_cont = 1 TO 6
            INITIALIZE lar_ret[ls_cont].* TO NULL
        END FOR

        LET lar_ret[1].tipo_retiro  = "H"
        LET lar_ret[2].tipo_retiro  = "I"
        LET lar_ret[3].tipo_retiro  = "J"
        LET lar_ret[4].tipo_retiro  = "K"
        LET lar_ret[5].tipo_retiro  = "L"
        LET lar_ret[6].tipo_retiro  = "N"

        FOR ls_cont = 1 TO 6
            SELECT descripcion
            INTO   lar_ret[ls_cont].desc_retiro
            FROM   tab_ret_issste
            WHERE  tipo_retiro  = lar_ret[ls_cont].tipo_retiro
            AND    cod_tramite  = 10

            LET lar_ret[ls_cont].num_cap  = 0
            LET lar_ret[ls_cont].num_prov = 0
        END FOR

        LET li_tot_prov = 0
        LET ls_flag     = 1
        LET ls_cont     = 1

        LET lc_query = " SELECT CASE tipo_retiro    ",
                          " WHEN 'H' THEN 1 ",
                          " WHEN 'I' THEN 2 ",
                          " WHEN 'J' THEN 3 ",
                          " WHEN 'K' THEN 4 ",
                          " WHEN 'L' THEN 5 ",
                          " WHEN 'N' THEN 6 ",
                          " END , ",
                       "        estado_solicitud ,  ",
                       "        COUNT(*)            ",
                       " FROM   ret_trans_issste    ",
                       " WHERE  estado_solicitud = ?",
                       " GROUP BY 1,2               ",
                       " ORDER BY 1,2               "

        PREPARE prp_datos_tot FROM lc_query
        DECLARE cur_datos_tot CURSOR FOR prp_datos_tot

        FOREACH cur_datos_tot USING gr_edo.capturado
                              INTO lr_soli.*

            LET ls_ret = lr_soli.tipo_retiro

            IF lr_soli.edo_soli = gr_edo.capturado THEN
                LET lar_ret[ls_ret].num_cap  = lr_soli.num_regs
                LET lar_ret[ls_ret].num_prov = lr_soli.num_regs
                LET li_tot_prov              = li_tot_prov + lr_soli.num_regs
            END IF

            LET ls_cont = ls_cont + 1

        END FOREACH

        IF li_tot_prov = 0 THEN
            PROMPT " NO EXISTEN REGISTROS PARA PROVISIONAR ... <ENTER> PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
        ELSE
            CALL SET_COUNT(6)
            DISPLAY ARRAY lar_ret TO scr_provi.*

                ON KEY (INTERRUPT)
                    PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                    LET ls_flag = 0
                    EXIT DISPLAY

                -- Provision de las cuentas
                ON KEY (CONTROL-P)
                    PROMPT "SE PROVISIONARAN ", li_tot_prov, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN #ff
                            LET ls_flag = 1
                        ELSE
                            PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                            LET ls_flag = 0
                        END IF
                    END IF

                    EXIT DISPLAY
            END DISPLAY
        END IF
    END IF  -- folio encontrado


    CLOSE WINDOW retc9611

    RETURN ls_flag, li_folio_trans

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Diagnostica las cuentas a provisionar del folio dado        #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE lr_transf RECORD LIKE ret_trans_issste.*

    DEFINE lr_diag  RECORD
        folio           LIKE ret_trans_issste.folio           ,
        consecutivo     LIKE ret_trans_issste.consecutivo     ,
        curp            LIKE ret_trans_issste.curp            ,
        nss             LIKE ret_trans_issste.nss             ,
        movimiento      LIKE tab_ret_issste.movimiento        ,
        diag_registro   LIKE ret_trans_issste.diag_registro   ,
        diag_procesar   LIKE ret_trans_issste.diag_procesar   ,
        mto_const_calc  LIKE ret_trans_issste.mto_const_calculado
    END RECORD

    DEFINE ldt_fip LIKE ret_trans_issste.fecha_ini_pen
    DEFINE lc_tipo_pen LIKE ret_trans_issste.tipo_pension


    DISPLAY "DIAGNOSTICANDO CUENTAS ...." AT 6,2

    DECLARE cur_diag CURSOR FOR
    SELECT A.folio           ,
           A.consecutivo     ,
           A.curp            ,
           A.nss             ,
           B.movimiento      ,
           A.diag_registro   ,
           A.diag_procesar   ,
           0                 ,
           A.fecha_ini_pen   ,
           A.tipo_pension
    FROM   ret_trans_issste A ,
           tab_ret_issste B
    WHERE  A.folio              = pi_folio
    AND    A.estado_solicitud   = gr_edo.capturado
    AND    A.tipo_retiro        = B.tipo_retiro
    AND    B.cod_tramite        = 10

    FOREACH cur_diag INTO lr_diag.*, ldt_fip, lc_tipo_pen

        -- Inicializamos el diagnostico procesar como aceptado
        LET lr_diag.diag_procesar = 501

        #-- Verificamos que exista la cuenta en el maestro de afiliados
        SELECT "OK"
        FROM   afi_mae_afiliado
        WHERE  n_unico  = lr_diag.curp
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            LET lr_diag.diag_procesar  = 503
        ELSE
            #-- Verificamos que no este inhabilitada con saldo cero
            SELECT "OK"
            FROM   cta_act_marca
            WHERE  nss        = lr_diag.nss
            AND    marca_cod IN (120,130,140)
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET lr_diag.diag_procesar = 502
            ELSE
                #-- Verificamos que este en un laudo judicial con la afore
                SELECT "OK"
                FROM   cta_act_marca
                WHERE  nss       = lr_diag.nss
                AND    marca_cod = 590
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    LET lr_diag.diag_procesar = 504
                ELSE
                    #-- Verifica que se encuentre marcada de acuerdo a su movimiento
                    SELECT "OK"
                    FROM   cta_act_marca
                    WHERE  nss           = lr_diag.nss
                    AND    marca_cod     = lr_diag.movimiento
                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
                        LET lr_diag.diag_procesar = 506
                    ELSE
                        IF gs_codigo_afore <> gs_peiss THEN 
                            IF f_checa_trasp_afore(lr_diag.nss, ldt_fip) THEN
                                -- solo se validan los que no son por muerte
                                IF (lc_tipo_pen = "IN" OR lc_tipo_pen = "IP") AND (lr_diag.diag_registro <> 302) THEN
                                    LET lr_diag.diag_procesar = 505
                                END IF
                            END IF -- Viene de un traspaso
                        ELSE
                            IF f_valida_no_redencion(lr_diag.curp) THEN
                                LET lr_diag.diag_procesar = gs_diag_no_redim
                            END IF -- Redencion de bono
                        END IF -- Cod afore
                    END IF -- Cuenta no marcada en transferencias
                END IF -- Laudo Judicial
            END IF --Cuenta inhabilitada
        END IF -- Cuenta inexistente

        IF (ldt_fip < "07/01/1997") AND
           (lr_diag.diag_procesar <> 503 AND lr_diag.diag_procesar <> 506 AND lr_diag.diag_procesar <> 505) THEN

            IF lc_tipo_pen = "IN" OR lc_tipo_pen = "IP" THEN
                LET lr_diag.diag_procesar = 502
            END IF
        END IF

        INSERT INTO safre_tmp:tmp_diagnostico
        VALUES(lr_diag.*)

    END FOREACH

    DISPLAY "(TERMINADO)" AT 6,30

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza la provision de las solicitudes del folio dado     #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE lr_datos RECORD
        curp                LIKE ret_trans_issste.curp                  ,
        nss                 LIKE ret_trans_issste.nss                   ,
        consecutivo         LIKE ret_trans_issste.consecutivo           ,
        monto_pago          LIKE ret_trans_issste.mto_solic_issste      ,
        grupo               LIKE ret_trans_issste.grupo                 ,
        edo_viv             LIKE ret_monto_viv_issste.estado_sub_viv    ,
        mto_viv_enviado     LIKE ret_monto_viv_issste.acc_viv08_bdsviv  ,
        tipo_retiro         LIKE ret_trans_issste.tipo_retiro           ,
        diag_registro       LIKE ret_trans_issste.diag_registro         ,
        diag_procesar       LIKE ret_trans_issste.diag_procesar
    END RECORD

    DEFINE lar_siefore ARRAY [20] OF RECORD
        activo            SMALLINT      ,
        acciones_ret08    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_ahsol    DECIMAL(16,6) ,
        acciones_ret92    DECIMAL(16,6) ,
        acciones_comp     DECIMAL(16,6) ,
        pesos_banxico     DECIMAL(16,6)
    END RECORD

    DEFINE lr_mto_viv RECORD
        acc_viv08       DECIMAL(16,6) ,
        acc_viv92       DECIMAL(16,6)
    END RECORD

    DEFINE lr_montos RECORD
        tot_viv             DECIMAL(16,6) ,
        tot_rcv             DECIMAL(16,6) ,
        mto_pesos_total     DECIMAL(16,6)
    END RECORD


    DEFINE lr_saldo_dia RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE #loc #date
        ld_fecha_saldo          DATE

    DEFINE #loc #smallint
        ls_insert               ,
        ls_sie_unica            ,        
        ls_tipo_mov             ,
        ls_siefore              , #-- contador para los ciclos for
        ls_subcta               SMALLINT

    DEFINE
        li_aceptados            INTEGER

    DEFINE
        ld_const_afore          ,
        ld_mto_pesos_tot        DECIMAL(16,6)

    -- Verifica si existen datos para transferir
    SELECT COUNT(*)
    INTO   li_aceptados
    FROM   ret_trans_issste A,
           safre_tmp:tmp_diagnostico B
    WHERE  A.folio            = pi_folio
    AND    A.folio            = B.folio
    AND    A.consecutivo      = B.consecutivo   
    AND    B.diag_procesar    = 501
    AND    A.estado_solicitud = gr_edo.capturado

    IF li_aceptados = 0 THEN
        RETURN   -- SALE, NO HAY NADA QUE PROCESAR
    END IF

    DISPLAY "CALCULANDO MONTOS ...." AT 8,2

    DECLARE cur_prov CURSOR FOR
    SELECT A.curp               ,
           A.nss                ,
           A.consecutivo        ,
           A.mto_solic_issste   ,
           A.grupo              ,
           C.estado_sub_viv     ,
           C.acc_viv08_bdsviv   ,
           A.tipo_retiro        ,
           B.diag_registro      ,
           B.diag_procesar
    FROM   ret_trans_issste A         ,
           safre_tmp:tmp_diagnostico B,
           ret_monto_viv_issste C
    WHERE  A.folio            = pi_folio
    AND    A.folio            = B.folio
    AND    A.folio            = C.folio
    AND    A.consecutivo      = B.consecutivo
    AND    A.consecutivo      = C.consecutivo
    AND    B.diag_procesar    = 501
    AND    A.estado_solicitud = gr_edo.capturado

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_datos.*

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_siefore[ls_siefore].activo          = FALSE
            LET lar_siefore[ls_siefore].acciones_ret08  = 0
            LET lar_siefore[ls_siefore].acciones_cv     = 0
            LET lar_siefore[ls_siefore].acciones_ahsol  = 0
            LET lar_siefore[ls_siefore].acciones_ret92  = 0
            LET lar_siefore[ls_siefore].acciones_comp   = 0
            LET lar_siefore[ls_siefore].pesos_banxico   = 0
        END FOR

        -- Se inicializan variables
        LET ls_insert             = 0
        LET ld_const_afore        = 0
        LET lr_montos.tot_rcv     = 0
        LET lr_montos.tot_viv     = 0
        LET ld_mto_pesos_tot      = 0
        LET lr_mto_viv.acc_viv08  = 0
        LET lr_mto_viv.acc_viv92  = 0

        SELECT movimiento
        INTO   ls_tipo_mov
        FROM   tab_ret_issste
        WHERE  tipo_retiro  = lr_datos.tipo_retiro
        AND    cod_tramite  = 10

        --Crea la tabla donde se almacenan los montos a provisionar
        CALL f_gen_tabla_mto_pago()

        DECLARE cur_subcta CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = lr_datos.grupo
        AND    subcuenta > 0

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_subcta INTO ls_subcta

            LET lr_saldo_dia.subcuenta = 0
            LET lr_saldo_dia.siefore   = 0
            LET lr_saldo_dia.monto_acc = 0
            LET lr_saldo_dia.monto_pes = 0

            -- Determinamos a que fecha se va a obtener el saldo
            IF (ls_subcta <> 14) AND (ls_subcta <> 35) THEN
                LET ld_fecha_saldo = HOY
            ELSE
                LET ld_fecha_saldo = gdt_fecha_viv
            END IF

            CALL f_obten_saldo_subcta(lr_datos.nss, ls_subcta, ld_fecha_saldo)
                RETURNING lr_saldo_dia.*

            IF (ls_subcta <> 14) AND (ls_subcta <> 35) AND (ls_subcta <> 19) THEN
                LET ls_sie_unica = lr_saldo_dia.siefore
            END IF

            IF lr_saldo_dia.monto_acc > 0 THEN

                -- Se aplica el porcentaje a transferir
                IF lr_datos.diag_registro = 302 THEN
                    CALL f_aplica_porcentaje(pi_folio               ,
                                             lr_datos.curp          ,
                                             lr_datos.consecutivo   ,
                                             lr_saldo_dia.subcuenta ,
                                             lr_saldo_dia.monto_pes)
                        RETURNING lr_saldo_dia.monto_pes 
                END IF 

                IF lr_saldo_dia.siefore = gs_sieviv THEN
                    
                    IF lr_saldo_dia.monto_acc > lr_datos.mto_viv_enviado THEN
                        -- Si el monto de vivienda calculado es mayor que el informado entonces
                        -- NO se provisiona vivienda
                        LET lr_datos.diag_procesar  = 507
                        LET lr_saldo_dia.monto_acc  = 0
                        LET lr_saldo_dia.monto_pes  = 0
                    ELSE
                        IF lr_datos.edo_viv = 1 THEN
                            --El elemento gs_sieviv del arreglo de precios contiene la informacion de la siefore 12 --
                            LET lr_saldo_dia.monto_pes = lr_saldo_dia.monto_acc * gar_precio_acc[gs_sieviv].precio_dia
                        ELSE
                            LET lr_saldo_dia.monto_pes = 0
                            LET lr_saldo_dia.monto_acc = 0
                        END IF
                    END IF

                    LET lr_montos.tot_viv = lr_montos.tot_viv + lr_saldo_dia.monto_acc
                ELSE
                    LET ls_siefore             = lr_saldo_dia.siefore
                    LET lr_saldo_dia.monto_pes = lr_saldo_dia.monto_acc * gar_precio_acc[ls_siefore].precio_dia
                    LET lr_montos.tot_rcv      = lr_montos.tot_rcv + lr_saldo_dia.monto_acc
                END IF

                -- Convertimos a pesos para obtener el monto total a provisionar
                LET ld_mto_pesos_tot = ld_mto_pesos_tot + lr_saldo_dia.monto_pes

                INSERT INTO tmp_mto_no_pago VALUES(lr_saldo_dia.siefore     ,
                                                   lr_saldo_dia.subcuenta   ,
                                                   lr_saldo_dia.monto_pes )

            END IF -- lr_saldo_dia.monto_acc > 0

        END FOREACH -- Subcuentas

        IF (lr_montos.tot_rcv IS NOT NULL AND lr_montos.tot_rcv > 0)
            OR (lr_montos.tot_viv > 0 ) THEN

            -- Si existe monto en RCV o VIV se calcula el proporcional para el
            -- monto constitutivo y se hace la provision
            DECLARE c_mto_const CURSOR FOR eje_mto_cons

            FOREACH c_mto_const USING lr_datos.curp           ,
                                      lr_datos.consecutivo    ,
                                      lr_datos.grupo          ,
                                      ld_mto_pesos_tot
                                INTO  lr_saldo_dia.subcuenta  ,
                                      lr_saldo_dia.monto_pes

                LET ld_const_afore = ld_const_afore + lr_saldo_dia.monto_pes

                IF (lr_saldo_dia.subcuenta = 14) OR (lr_saldo_dia.subcuenta = 35) THEN
                    --El elemento gs_sieviv del arreglo de precios contiene la informacion de la siefore 12--
                    LET lr_saldo_dia.monto_acc   = lr_saldo_dia.monto_pes / gar_precio_acc[gs_sieviv].precio_dia
                    LET lr_saldo_dia.siefore     = gs_sieviv
                ELSE
                    LET lr_saldo_dia.monto_acc   = lr_saldo_dia.monto_pes / gar_precio_acc[ls_siefore].precio_dia
                    LET lr_saldo_dia.siefore     = ls_siefore
                END IF

                IF lr_saldo_dia.monto_pes > 0 THEN

                    IF lr_saldo_dia.siefore <> gs_sieviv THEN

                        LET ls_siefore = lr_saldo_dia.siefore

                        -- Marcamos como activo el registro de la siefore actual
                        LET lar_siefore[ls_siefore].activo = TRUE
                        CASE
                            WHEN lr_saldo_dia.subcuenta = 30
                                LET lar_siefore[ls_siefore].acciones_ret08 = lr_saldo_dia.monto_acc

                            WHEN (lr_saldo_dia.subcuenta = 31) OR (lr_saldo_dia.subcuenta = 32)
                                LET lar_siefore[ls_siefore].acciones_cv = lr_saldo_dia.monto_acc 
                                            + lar_siefore[ls_siefore].acciones_cv 

                            WHEN (lr_saldo_dia.subcuenta = 33) OR (lr_saldo_dia.subcuenta = 34)
                                LET lar_siefore[ls_siefore].acciones_ahsol = lr_saldo_dia.monto_acc
                                            + lar_siefore[ls_siefore].acciones_ahsol

                            -- No se pagan en transferencias !!!
                            WHEN lr_saldo_dia.subcuenta = 13
                                LET lar_siefore[ls_siefore].acciones_ret92 = lr_saldo_dia.monto_acc

                            WHEN (lr_saldo_dia.subcuenta = 24) OR (lr_saldo_dia.subcuenta = 25)
                                LET lar_siefore[ls_siefore].acciones_comp = lr_saldo_dia.monto_acc
                            -- 
                                
                        END CASE
                    ELSE
                        CASE lr_saldo_dia.subcuenta
                            WHEN 35
                                LET lr_mto_viv.acc_viv08 = lr_saldo_dia.monto_acc
                            
                            -- No se pagan en transferencias !!!
                            WHEN 14
                                LET lr_mto_viv.acc_viv92 = lr_saldo_dia.monto_acc
                            --
                        END CASE
                    END IF

                    CALL f_provisiona_subcta(lr_datos.curp              ,
                                             lr_datos.nss               ,
                                             lr_saldo_dia.subcuenta     ,
                                             lr_datos.consecutivo       ,
                                             lr_saldo_dia.monto_acc     ,
                                             lr_saldo_dia.monto_pes     ,
                                             HOY                        ,
                                             ls_tipo_mov                ,
                                             lr_saldo_dia.siefore       )
                END IF
            END FOREACH
        END IF -- Monto tot > 0

        FOR ls_siefore = 1 TO gs_num_siefores

            LET ls_insert = 1

            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_siefore[ls_siefore].activo = TRUE THEN

                INSERT INTO safre_tmp:tmp_tr_monto_issste
                    VALUES(lr_datos.curp                            , -- curp
                           lr_datos.consecutivo                     , -- consecutivo
                           1                                        , -- folio
                           lr_datos.tipo_retiro                     , -- tipo_retiro
                           gs_tipo_op                               , -- tipo_operacion
                           ls_siefore                               , -- siefore
                           lar_siefore[ls_siefore].acciones_ret08   , -- acciones_ret08
                           lar_siefore[ls_siefore].acciones_cv      , -- acciones_cv
                           lar_siefore[ls_siefore].acciones_ahsol   , -- acciones_ahsol
                           lar_siefore[ls_siefore].acciones_ret92   , -- acciones_ret92
                           lar_siefore[ls_siefore].acciones_comp    , -- acciones_comp
                           lar_siefore[ls_siefore].pesos_banxico      -- pesos banxico
                          )

            END IF
        END FOR

        -- Si no encontro montos provisionados de las siefores entonces se inserta en ceros 
        -- el registro de tmp_tr_monto_issste
        IF NOT ls_insert THEN
            INSERT INTO safre_tmp:tmp_tr_monto_issste
                VALUES(lr_soli.curp                          , -- curp
                       lr_soli.consecutivo                   , -- consecutivo
                       1                                     , -- folio
                       lr_soli.tipo_retiro                   , -- tipo_retiro
                       gs_tipo_op                            , -- tipo_operacion
                       ls_sie_unica                          , -- siefore
                       0                                     , -- acciones_ret08
                       0                                     , -- acciones_cv
                       0                                     , -- acciones_ahsol
                       0                                     , -- acciones_ret92
                       0                                     , -- acciones_comp
                       lar_siefore[ls_siefore].pesos_banxico   -- pesos banxico
                      )
        END IF

        --Si el importe de las siefores es 0 se informa con diagnostico 502--
        IF lr_montos.tot_rcv <= 0 AND lr_montos.tot_viv <= 0 AND lr_datos.diag_procesar != 507 THEN
            LET lr_datos.diag_procesar = 502
        END IF

        UPDATE  safre_tmp:tmp_diagnostico
        SET     mto_const_calc = ld_const_afore ,
                diag_procesar  = lr_datos.diag_procesar
        WHERE   curp           = lr_datos.curp
        AND     consecutivo    = lr_datos.consecutivo
        AND     folio          = pi_folio

        INSERT INTO safre_tmp:tmp_tr_monto_viv_issste
            VALUES(lr_datos.curp         , -- curp
                   lr_datos.consecutivo  , -- consecutivo
                   1                     , -- folio
                   lr_datos.tipo_retiro  , -- tipo_retiro
                   gs_tipo_op            , -- tipo_operacion
                   gdt_fecha_viv         , -- fecha_valor_viv
                   lr_mto_viv.acc_viv08  , -- acciones_viv08
                   lr_mto_viv.acc_viv92  , -- acciones_viv92
                   NULL                  , -- estado_sub_viv
                   0                     , -- acc_viv97_bdsviv
                   0                       -- acc_viv92_bdsviv
                  )

    END FOREACH -- Siguiente NSS

    DISPLAY "(TERMINADO)" AT 8,30

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Se afectan las tablas fisicas del proceso una vez que se    #
#               termino correctamente                                       #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE la_rets ARRAY[6] OF INTEGER

    DEFINE lr_datos RECORD
        curp            LIKE dis_provision.curp             ,
        nss             LIKE dis_provision.nss              ,
        tipo_mov        LIKE dis_provision.tipo_movimiento  ,
        consec          LIKE dis_provision.consecutivo_lote ,
        diag_procesar   LIKE ret_trans_issste.diag_procesar ,
        mto_const_calc  LIKE ret_trans_issste.mto_const_calculado
    END RECORD

    DEFINE lr_mto_viv RECORD LIKE ret_monto_viv_issste.*
        
    DEFINE
        li_cont_rec     INTEGER

    DEFINE
        ls_edo_sol      SMALLINT

    ------------------------------------------------------------------

    DISPLAY "PROVISIONANDO CUENTAS ...." AT 10,2
    DISPLAY "FOLIO    : ", pi_folio AT 12,13

    LET li_cont_rec = 0
    
    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_tr_provision
    SET    folio = pi_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_tr_provision
    WHERE  folio = pi_folio

    -- Copiamos la tabla de montos temporal a la definitiva
    UPDATE safre_tmp:tmp_tr_monto_issste
    SET    folio = pi_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_issste
    SELECT *
    FROM   safre_tmp:tmp_tr_monto_issste
    WHERE  folio = pi_folio

    -- Actualizamos la tabla de montos de vivienda temporal
    UPDATE safre_tmp:tmp_tr_monto_viv_issste
    SET    folio = pi_folio
    WHERE  folio = 1

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT A.curp               ,
           A.nss                ,
           C.movimiento         ,
           A.consecutivo        ,
           B.diag_procesar      ,
           B.mto_const_calc
    FROM   ret_trans_issste A          ,
           safre_tmp:tmp_diagnostico B ,
           tab_ret_issste C
    WHERE  A.folio              = pi_folio
    AND    A.folio              = B.folio
    AND    A.curp               = B.curp
    AND    A.consecutivo        = B.consecutivo
    AND    A.tipo_retiro        = C.tipo_retiro 
    AND    C.cod_tramite        = 10   
    ORDER BY 1

    FOREACH cur_soli INTO lr_datos.*

        IF lr_datos.diag_procesar = 501 THEN
            LET ls_edo_sol = gr_edo.provisionado
        ELSE
            IF f_desmarca_no_liq(lr_datos.nss          ,
                                 lr_datos.consec       ,
                                 lr_datos.tipo_mov     ,
                                 gs_usuario            ,
                                 lr_datos.diag_procesar) THEN

                LET ls_edo_sol  = gr_edo.rechazado
                LET li_cont_rec = li_cont_rec + 1
            ELSE
                LET ls_edo_sol = gr_edo.provisionado
            END IF
        END IF

        -- Copiamos la tabla de montos de vivienda temporal a la definitiva
        SELECT *
        INTO   lr_mto_viv.*
        FROM   safre_tmp:tmp_tr_monto_viv_issste
        WHERE  folio        = pi_folio
        AND    consecutivo  = lr_datos.consec
        AND    curp         = lr_datos.curp

        UPDATE ret_monto_viv_issste
        SET    cve_operacion    = lr_mto_viv.cve_operacion    ,
               fecha_valor_viv  = lr_mto_viv.fecha_valor_viv  ,
               acciones_viv08   = lr_mto_viv.acciones_viv08   ,
               acciones_viv92   = lr_mto_viv.acciones_viv92
        WHERE  folio            = pi_folio
        AND    consecutivo      = lr_datos.consec
        AND    curp             = lr_datos.curp

        -- Actualizamos la tabla de solicitudes de transferencias
        UPDATE ret_trans_issste
        SET    diag_procesar        = lr_datos.diag_procesar  ,
               mto_const_calculado  = lr_datos.mto_const_calc ,
               estado_solicitud     = ls_edo_sol
        WHERE  folio                = pi_folio
        AND    curp                 = lr_datos.curp
        AND    consecutivo          = lr_datos.consec
        AND    estado_solicitud     = gr_edo.capturado

        CASE lr_datos.tipo_mov
            WHEN 861
                LET la_rets[1] = la_rets[1] + 1
            WHEN 862
                LET la_rets[2] = la_rets[2] + 1
            WHEN 863
                LET la_rets[3] = la_rets[3] + 1
            WHEN 864
                LET la_rets[4] = la_rets[4] + 1
            WHEN 865
                LET la_rets[5] = la_rets[5] + 1
            WHEN 866
                LET la_rets[6] = la_rets[6] + 1
        END CASE

        DISPLAY "RETIRO H : ", la_rets[1] AT 13,13
        DISPLAY "RETIRO I : ", la_rets[2] AT 14,13
        DISPLAY "RETIRO J : ", la_rets[3] AT 15,13
        DISPLAY "RETIRO K : ", la_rets[4] AT 13,46
        DISPLAY "RETIRO L : ", la_rets[5] AT 14,46
        DISPLAY "RETIRO N : ", la_rets[6] AT 15,46

        DISPLAY "RECHAZOS : ", li_cont_rec AT 16,46

    END FOREACH

    DISPLAY "(TERMINADO)" AT 10,30
    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc9612

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de transferencias                              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9612 AT 4,4 WITH FORM "RETC9612" ATTRIBUTE(BORDER)
    DISPLAY "                                                       TRANSFERENCIAS ISSSTE " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC961      GENERACION DE MONTOS DE TRANSFERENCIAS ISSSTE                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_checa_trasp_afore : Verifica que la cuenta tenga o no traspasos         #
#                       afore-afore receptora                               #
#---------------------------------------------------------------------------#
FUNCTION f_checa_trasp_afore(pr_trasp)

    DEFINE pr_trasp RECORD
        nss              LIKE ret_trans_issste.nss          ,
        fecha_ini_pen    LIKE ret_trans_issste.fecha_ini_pen
    END RECORD

    DEFINE
        ls_flag_traspaso     SMALLINT

    -- Se verifica que el traspaso sea posterior a la fip (en tabla historica de traspasos receptora)
    SELECT "OK"
    FROM   taa_rcv_recepcion
    WHERE  nss               = pr_trasp.nss
    AND    fecha_mov_banxico > pr_trasp.fecha_ini_pen
    AND    ident_operacion   = "09"  -- Traspaso ordinario
    AND    estado            = 2     -- Liquidado
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       LET ls_flag_traspaso = 1
    ELSE
       LET ls_flag_traspaso = 0
    END IF

    RETURN ls_flag_traspaso
END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_saldo_subcta : Obtiene el saldo del nss de la subcuenta indicada  #
#                        a la fecha dada por pr_datos.fec_saldo             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_subcta(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_trans_issste.nss           ,
        subcta      LIKE dis_provision.subcuenta        ,
        fec_saldo   LIKE dis_provision.fecha_conversion
    END RECORD

    DEFINE lr_sal_dia RECORD
        subcta      LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE ld_saldo_dia_viv LIKE dis_provision.monto_en_acciones

    DEFINE
        ls_grupo            SMALLINT

    LET ls_grupo            = 0
    LET lr_sal_dia.subcta   = 0
    LET lr_sal_dia.siefore  = 0

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    OPEN cur_saldo USING pr_datos.nss       ,
                         pr_datos.subcta    ,
                         ls_grupo           ,
                         pr_datos.fec_saldo

    FETCH cur_saldo INTO lr_sal_dia.*

    CLOSE cur_saldo

    IF lr_sal_dia.subcta <> 19 THEN

        IF lr_sal_dia.monto_acc IS NULL OR lr_sal_dia.monto_acc = 0 THEN
            --Si no tiene saldo en la subcuenta se manda el saldo como cero
            LET lr_sal_dia.monto_acc = 0
            LET lr_sal_dia.monto_pes = 0
        END IF

        -- Verificamos si no existe un sobregiro en vivienda
        IF lr_sal_dia.siefore = gs_sieviv THEN

            LET ld_saldo_dia_viv = 0

            SELECT SUM(monto_en_acciones)
            INTO   ld_saldo_dia_viv
            FROM   dis_cuenta
            WHERE  nss       = pr_datos.nss
            AND    siefore   = gs_sieviv
            AND    subcuenta = pr_datos.subcta

            IF ld_saldo_dia_viv < 0 OR ld_saldo_dia_viv IS NULL THEN
                LET ld_saldo_dia_viv = 0
            END IF

            -- Si lo que se obtiene al primer dia natural es mayor a lo que hay
            -- actualmente en la cuenta individual, tomamos el saldo al dia
            IF lr_sal_dia.monto_acc > ld_saldo_dia_viv THEN
                LET lr_sal_dia.monto_acc = ld_saldo_dia_viv
            END IF
        END IF
    END IF

    RETURN lr_sal_dia.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_sobregiro : Verifica que los montos de aportes posteriores     #
#                        no rebasen el saldo al dia                         #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_sobregiro(pr_ap_post)

    DEFINE pr_ap_post RECORD
        nss             LIKE ret_trans_issste.nss            ,
        subcta          SMALLINT                             ,
        monto_acc       LIKE dis_provision.monto_en_acciones ,
        monto_pes       LIKE dis_provision.monto_en_acciones ,
        fecha           DATE
    END RECORD

    DEFINE lr_saldo_dia RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        ls_grupo     SMALLINT

    DEFINE
        ld_monto_acc     ,
        ld_monto_pes     DECIMAL(16,6)

    LET ld_monto_acc            = 0
    LET ld_monto_pes            = 0
    LET ls_grupo                = 0
    LET lr_saldo_dia.monto_acc  = 0

    DECLARE cur_val_sobre CURSOR FOR eje_saldo_dia

    OPEN cur_val_sobre USING pr_ap_post.nss        ,
                             pr_ap_post.subcta     ,
                             ls_grupo              ,
                             pr_ap_post.fecha

    FETCH cur_val_sobre INTO lr_saldo_dia.*

    CLOSE cur_val_sobre

    -- Si el monto de ap posteriores es mayor al saldo al dia entonces se debe
    -- tomar este saldo

    IF pr_ap_post.monto_acc > lr_saldo_dia.monto_acc THEN
        LET ld_monto_acc = lr_saldo_dia.monto_acc
        LET ld_monto_pes = lr_saldo_dia.monto_pes
    ELSE
        LET ld_monto_acc = pr_ap_post.monto_acc
        LET ld_monto_pes = pr_ap_post.monto_pes
    END IF

    RETURN ld_monto_acc, ld_monto_pes

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_no_redencion : Verifica que el curp ingresado tenga o no el bono #
#                         issste redimido                                   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_no_redencion(pr_curp)

    DEFINE pr_curp LIKE ret_trans_issste.curp

    DEFINE 
        ls_tipo_bono        SMALLINT

    SELECT ind_tipo_bono_isss
    INTO   ls_tipo_bono
    FROM   dis_hist_apor_bono
    WHERE  n_unico = pr_curp

    RETURN ls_tipo_bono

END FUNCTION

#---------------------------------------------------------------------------#
# f_gen_tabla_mto_pago : Genera la tabla donde se almacenan el saldo de las #
#                        subcuentas que se van a provisionar por nss        #
#---------------------------------------------------------------------------#
FUNCTION f_gen_tabla_mto_pago()

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

#---------------------------------------------------------------------------#
# f_aplica_porcentaje : Aplica el porcentaje a transferir de la subcuenta   #
#                       en caso de que el nss tenga diagnostico 302         #
#---------------------------------------------------------------------------#
FUNCTION f_aplica_porcentaje(pr_prc, pr_pesos)

    DEFINE pr_prc RECORD
        folio       LIKE ret_trans_issste.folio       ,
        curp        LIKE ret_trans_issste.curp        ,
        consec      LIKE ret_trans_issste.consecutivo ,
        subcta      SMALLINT
    END RECORD

    DEFINE 
        pr_pesos        ,
        ld_pesos_prc    LIKE dis_provision.monto_en_pesos
    
    DEFINE ld_porcentaje LIKE ret_trans_issste.porcentaje_ret08
    
    DEFINE
        lc_query        CHAR(150),
        lc_sel          CHAR(040)
    
    
    CASE 
        WHEN pr_prc.subcta = 30
            LET lc_sel = " SELECT porcentaje_ret08 "
        
        WHEN (pr_prc.subcta = 31) OR (pr_prc.subcta = 32)
            LET lc_sel = " SELECT porcentaje_cv "
        
        WHEN (pr_prc.subcta = 33) OR (pr_prc.subcta = 34)
            LET lc_sel = " SELECT porcentaje_ahorro_sol "
        
        WHEN pr_prc.subcta = 35
            LET lc_sel = " SELECT porcentaje_viv08 "
    END CASE
    
    LET lc_query = lc_sel,
                   " FROM   ret_trans_issste ",
                   " WHERE  folio = ?        ",
                   " AND    curp = ?         ",
                   " AND    consecutivo = ?  "

    LET lc_query = lc_query CLIPPED

    PREPARE eje_porc FROM lc_query

    EXECUTE eje_porc USING pr_prc.folio ,
                           pr_prc.curp  ,
                           pr_prc.consec
                     INTO  ld_porcentaje

    LET ld_pesos_prc = (pr_pesos * ld_porcentaje) / 100

    RETURN ld_pesos_prc

END FUNCTION

#---------------------------------------------------------------------------#
# f_provisiona_subcta : Inserta los montos que se usaran en la provision    #
#                       en la tabla temporal de provisiones                 #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_subcta(pr_provi, ps_sie)

    DEFINE pr_provi RECORD
        curp        LIKE ret_trans_issste.curp          ,
        nss         LIKE ret_trans_issste.nss           ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_trans_issste.consecutivo   ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE                                ,
        tipo_mov    SMALLINT
    END RECORD

    DEFINE
        ps_sie              SMALLINT

    DEFINE ld_precio_acc LIKE dis_provision.precio_accion

    DEFINE
        si_provisiono       SMALLINT

    DEFINE
        ld_acc              ,
        ld_pesos            DECIMAL(16,6)

    DEFINE
        ldt_fec_proc        DATE


    DEFINE #loc #char
        folio_sua             CHAR(06) ,
        id_aporte             CHAR(06)

    LET si_provisiono = 0
    LET folio_sua     = ""
    LET id_aporte     = "RETIRO"

    LET ld_acc          = -pr_provi.acciones
    LET ld_pesos        = -pr_provi.pesos
    LET ld_precio_acc   = gar_precio_acc[ps_sie].precio_dia

    IF (pr_provi.subcta = 14) OR (pr_provi.subcta = 35) THEN
        LET ldt_fec_proc = gdt_fecha_viv

        IF gs_codigo_afore = gs_xxi THEN
            LET ld_pesos = f_redondea_val(ld_pesos, 2)
        END IF
    ELSE
        LET ldt_fec_proc = pr_provi.fecha_proc
    END IF

    INSERT INTO safre_tmp:tmp_tr_provision
        VALUES (
         pr_provi.tipo_mov      ,-- tipo_movimiento
         pr_provi.subcta        ,-- subcuenta
         ps_sie                 ,-- siefore
         1                      ,-- folio (temporal)
         pr_provi.consec        ,-- consecutivo_lote
         pr_provi.nss           ,-- nss
         pr_provi.curp          ,-- curp
         NULL                   ,-- folio_sua
         HOY                    ,-- fecha_pago
         ldt_fec_proc           ,-- fecha_valor
         HOY                    ,-- fecha_conversion
         ld_pesos               ,-- monto_en_pesos
         ld_acc                 ,-- monto_en_acciones
         ld_precio_acc          ,-- precio_accion
         0                      ,-- dias_cotizados
         ""                     ,-- sucursal
         id_aporte              ,-- id_aportante
         6                      ,-- estado
         HOY                    ,-- fecha_proceso
         gs_usuario             ,-- usuario
         HOY                    ,-- fecha_archivo
         1                        -- etiqueta
        )

END FUNCTION

#---------------------------------------------------------------------------#
# f_redondea_val : Redondea el monto dado por p_monto_redondear a tantos    #
#                  como se indique en p_redondea                            #
#---------------------------------------------------------------------------#
FUNCTION f_redondea_val(p_monto_redondear, p_redondea)

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

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
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
        ls_sie                SMALLINT

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.siefore = gs_sieviv THEN

            LET ls_sie = gs_sieviv

            SELECT 0                ,
                   fecha_valuacion  ,
                   codigo_siefore   ,
                   precio_del_dia
            INTO   gar_precio_acc[ls_sie].*
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = gdt_fecha_viv
            AND    codigo_siefore  = gs_sieviv

            IF STATUS = NOTFOUND THEN
                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", gdt_fecha_viv USING "DD/MM/yyyy",
                                 " -- SIEFORE ", gs_sieviv CLIPPED
                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            END IF
        ELSE

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF

        END IF


    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_no_liq : Ejecuta el SPL que realiza la desmarca de las cuentas #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_no_liq(pr_desmarca, ps_diag)

    DEFINE pr_desmarca RECORD
        nss         LIKE ret_trans_issste.nss           ,
        consec      LIKE ret_trans_issste.consecutivo   ,
        tipo_mov    LIKE dis_provision.tipo_movimiento  ,
        usuario     LIKE ret_trans_issste.usuario
    END RECORD

    DEFINE
        ps_diag         ,
        ls_desmarca     ,
        ls_cont         SMALLINT

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    LET ls_desmarca         = 1
    LET ls_cont             = 0
    LET lr_dat.edo_marca    = 40 --Desmarca por validación interna
    LET lr_dat.marca_causa  = 0

    -- Si tiene diagnostico 507 y no se provisiono rcv se desmarca
    IF ps_diag = 507 THEN

       SELECT COUNT(*)
       INTO   ls_cont
       FROM   safre_tmp:tmp_tr_provision
       WHERE  nss               = pr_desmarca.nss
       AND    consecutivo_lote  = pr_desmarca.consec
       AND    subcuenta        != 35

       IF ls_cont > 0 then
          LET ls_desmarca = 0
       END IF
    ELSE
        --MARCAJE--
        EXECUTE eje_desmarca USING pr_desmarca.nss          ,--nss
                                   pr_desmarca.tipo_mov     ,--marca entrante
                                   pr_desmarca.consec       ,--consecutivo
                                   lr_dat.edo_marca         ,--estado_marco
                                   lr_dat.marca_causa       ,--marca_causa
                                   pr_desmarca.usuario       --usuario
    END IF

    RETURN ls_desmarca

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_diagnostico
        DROP TABLE tmp_tr_provision
        DROP TABLE tmp_tr_monto_issste
        DROP TABLE tmp_tr_monto_viv_issste
    WHENEVER ERROR STOP

    --------------------------------

    CREATE TABLE tmp_diagnostico(
        folio           INTEGER         ,
        consecutivo     DECIMAL(11,0)   ,
        curp            CHAR(18)        ,
        nss             CHAR(11)        ,
        movimiento      SMALLINT        ,
        diag_registro   SMALLINT        ,
        diag_procesar   SMALLINT        ,
        mto_const_calc  DECIMAL(16,6)
    )

    GRANT ALL ON tmp_diagnostico TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_tr_provision (
     tipo_movimiento    SMALLINT NOT NULL       ,
     subcuenta          SMALLINT NOT NULL       ,
     siefore            SMALLINT                ,
     folio              DECIMAL(10,0) NOT NULL  ,
     consecutivo_lote   INTEGER                 ,
     nss                CHAR(11) NOT NULL       ,
     curp               CHAR(18)                ,
     folio_sua          CHAR(6)                 ,
     fecha_pago         DATE                    ,
     fecha_valor        DATE                    ,
     fecha_conversion   DATE                    ,
     monto_en_pesos     DECIMAL(16,6)           ,
     monto_en_acciones  DECIMAL(16,6)           ,
     precio_accion      DECIMAL(16,6)           ,
     dias_cotizados     INTEGER                 ,
     sucursal           CHAR(10)                ,
     id_aportante       CHAR(11)                ,
     estado             SMALLINT                ,
     fecha_proceso      DATE                    ,
     usuario            CHAR(8)                 ,
     fecha_archivo      DATE                    ,
     etiqueta           INTEGER
      )

    CREATE INDEX tmp_tr_provision_1 ON tmp_tr_provision
        (folio,subcuenta,tipo_movimiento,estado)

    CREATE INDEX tmp_tr_provision_2 ON tmp_tr_provision
        (nss)

    CREATE INDEX tmp_tr_provision_3 ON tmp_tr_provision
        (folio,subcuenta)

    GRANT ALL ON tmp_tr_provision TO PUBLIC
    
    --------------------------------

    CREATE TABLE tmp_tr_monto_issste (
        curp            CHAR(18)        ,
        consecutivo     DECIMAL(11,0)   ,
        folio           INTEGER         ,
        tipo_retiro     CHAR(1)         ,
        cve_operacion   SMALLINT        ,
        siefore         SMALLINT        ,
        acc_ret08       DECIMAL(16,6)   ,
        acc_cv          DECIMAL(16,6)   ,
        acc_ahorro_sol  DECIMAL(16,6)   ,
        acc_ret92       DECIMAL(16,6)   ,
        acc_comp_ret    DECIMAL(16,6)   ,
        pes_banxico     DECIMAL(16,6)
    )

    CREATE INDEX tmp_tr_mto_isss_01 ON tmp_tr_monto_issste
        (curp,consecutivo,cve_operacion)

    GRANT ALL ON tmp_tr_monto_issste TO PUBLIC
    
    --------------------------------

    CREATE TABLE tmp_tr_monto_viv_issste
      (
       curp                CHAR(18)        ,
       consecutivo         DECIMAL(11,0)   ,
       folio               INTEGER         ,
       tipo_retiro         CHAR(1)         ,
       cve_operacion       SMALLINT        ,
       fecha_valor_viv     DATE            ,
       acciones_viv08      DECIMAL(14,6)   ,
       acciones_viv92      DECIMAL(14,6)   ,
       estado_sub_viv      CHAR(1)         ,
       acc_viv08_bdsviv    DECIMAL(14,6)   ,
       acc_viv92_bdsviv    DECIMAL(14,6)
      )

    CREATE INDEX tmp_tr_viv_isss_01 ON tmp_tr_monto_viv_issste
        (curp,consecutivo,cve_operacion)
    
    GRANT ALL ON tmp_tr_monto_viv_issste TO PUBLIC

    --------------------------------

    DATABASE safre_af

END FUNCTION
