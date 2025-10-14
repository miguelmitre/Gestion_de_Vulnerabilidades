#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC704  => Provision de solicitud de saldos                          #
#Fecha creacion    =>                                                           #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 19-jun-2012                                               #
#Actualizacion     => Se agrega parametro de fecha                              #
#                     Alejandro Chagoya Salazar                                 #
#Sistema           => RET                                                       #
#------------------------------MODIFICACIONES-----------------------------------#
#Fecha actualiz.   => 31-Mayo-2012                                              #
#Actualizacion     => las cuentas con diagnostico 509 son  aceptadas y se cambia#
#                  => estado de rechazo a liquidados                            #
#                  => Se agrega validacion de Viv97. al diagnostico 509 y se    #
#                  => elimina marca 140 para el diagnostico 502 -> ACS 04062012 #
#################################################################################
#Requerimiento     => CPL-918   29-Ago-2012   Alejandro Chagoya Salazar         #
#Descripcion       => Se cambian las cuentas 502 como liquidadas y se desmarca  #
#                  => hasta el momento de la reinversion                        #
#################################################################################
#Requerimiento     => CPL-1199  26-Feb-2013   Alejandro Chagoya Salazar         #
#Descripcion       => Se cambian la funcion fn_obten_fecha_val por              #
#                  => fn_obten_fecha_val_des, especial para desinversion        #
#-------------------------------------------------------------------------------#
#Requerimiento     => MLM-2179    19-Nov-2013   Alejandro Chagoya Salazar       #
#Actualizacion     => Se modifica estructura de la tabla tmp_sol_saldo          #
#                  => por la nueva plataforma                                   #
#-------------------------------------------------------------------------------#
#Requerimiento     => CPL-1765    27-Oct-2014   Alejandro Chagoya Salazar       #
#Actualizacion     => Se agrega asignacion de variable de pesos y acciones de   #
#                  => subcuentas de vivienda                                    #
#################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        sol_saldo           LIKE ret_estado.estado_solicitud,
        provisionado        LIKE ret_estado.estado_solicitud,
        rechazado           LIKE ret_estado.estado_solicitud,
        liquidado           LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD
        estado          SMALLINT     ,
        fecha           DATE         ,
        siefore         SMALLINT     ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE #glo #date
        gdt_fecha_viv           ,
        HOY                     DATE

    DEFINE #glo #char
        gc_tipo_ret             CHAR(001) ,
        enter                   CHAR(001) ,
        gs_usuario              CHAR(015)

    DEFINE #glo #smallint
        gs_grupo_sub            ,
        gs_tipo_op              ,
        gs_procesa              ,
        gs_sieviv               ,
        gs_num_siefores         , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore         SMALLINT

    DEFINE #glo #integer
        gs_ult_folio            INTEGER,
        m_fecha                 DATE    #ACS 19Jun2012

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC704.log")

    CALL f_tablas_tmp()
    CALL init() #i
    CALL f_obtiene_precios_accion(HOY)

    CALL f_despliega_info() RETURNING gs_procesa

    IF gs_procesa THEN

        CALL primer_paso()      #-- Diagnostica las cuentas a provisionar
        CALL segundo_paso()     #-- Provision de solicitudes
        CALL tercer_paso()      #-- Se afectan las tablas fisicas
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------
    INITIALIZE m_fecha TO NULL

    LET HOY                 = TODAY
    LET gs_tipo_op          = 43
    LET gs_sieviv           = 11
    LET gs_grupo_sub        = 8     -- Grupo que paga las subcuentas asociadas

    ----- FECHA DE VIVIENDA -----
    #CPL-1199
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val_des(?) "
    PREPARE eje_fecha_viv FROM lc_prepare
    EXECUTE eje_fecha_viv USING HOY INTO gdt_fecha_viv
    LET lc_prepare = " "

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

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.sol_saldo
    FROM   ret_estado A
    WHERE  A.descripcion = "SOLICITUD SALDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO SALDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO SALDO"    

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO SALDO"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- CONSULTA DE SALDOS -----
    LET lc_prepare = "EXECUTE FUNCTION fn_consulta_saldos(?,?,?,?,?,?,?) "
    PREPARE eje_con_saldos FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info :                                                        #
#                                                                           #
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
        li_tot_prov         INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()
    
          INPUT m_fecha  WITHOUT DEFAULTS FROM fecha
             AFTER FIELD fecha
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF

                IF m_fecha > TODAY THEN
                   ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
                   NEXT FIELD fecha
                END IF

             ON KEY (ESC)
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF
                EXIT INPUT

             ON KEY (CONTROL-C, INTERRUPT)
                ERROR "PROCESO CANCELADO" SLEEP 2
                ERROR ""
                EXIT PROGRAM
          
          END INPUT

    WHILE TRUE
      PROMPT "¿DESEA EJECUTAR PROVISION CON ESTA FECHA? (S/N)" FOR CHAR enter
      IF enter MATCHES "[SsNn]" THEN
        IF enter MATCHES "[Ss]" THEN
           CALL f_valida_provisiona() RETURNING ls_flag, li_tot_prov
           
           IF ls_flag = 1 THEN
              IF li_tot_prov = 0 THEN
                 CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
                 LET ls_flag = 0
              ELSE
                 WHILE TRUE    
                    PROMPT "SE PROVISIONARAN ", li_tot_prov, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                       IF enter MATCHES "[sS]" THEN
                           LET ls_flag = 1
                       ELSE
                           CALL f_error_msg("PROCESO CANCELADO")
                           LET ls_flag = 0
                       END IF
                       EXIT WHILE
                    END IF
                 END WHILE 
              END IF
           END IF 
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           EXIT PROGRAM
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Diagnostica las cuentas a provisionar del folio dado        #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_diag  RECORD
        id_solicitud_saldo      LIKE ret_solicitud_saldo.id_solicitud_saldo     ,
        nss                     LIKE ret_solicitud_saldo.nss                    ,
        curp                    LIKE ret_solicitud_saldo.curp                   ,
        diag_recep_afore        LIKE ret_solicitud_saldo.diag_recep_afore       ,
        diag_envio_procesar     LIKE ret_solicitud_saldo.diag_envio_procesar    
    END RECORD

    DEFINE lr_subs_rcv RECORD
        ret_97          DECIMAL(16,2),
        viv_97          DECIMAL(16,2),  #ACS 04.06.2012
        ces_vej         DECIMAL(16,2),
        cuota_soc       DECIMAL(16,2),
        tot_sub_rcv     DECIMAL(16,2)
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "DIAGNOSTICANDO CUENTAS ...." AT 8,2

    DECLARE cur_diag CURSOR FOR
        SELECT id_solicitud_saldo     ,
               nss                    ,
               curp                   ,
               diag_recep_afore       ,
               diag_envio_procesar
        FROM   safre_tmp:tmp_sol_saldo
        WHERE  estado_solicitud = gr_edo.sol_saldo

    FOREACH cur_diag INTO lr_diag.*

        -- Inicializamos el diagnostico procesar como aceptado
        LET lr_diag.diag_envio_procesar = 501

        #-- Verificamos que no este inhabilitada con saldo cero
        # Se elimina marca 140 #ACS 04062012
        SELECT "OK"
        FROM   cta_act_marca
        WHERE  nss        = lr_diag.nss
        AND    marca_cod IN (120,130)
        GROUP BY 1
        
        IF STATUS <> NOTFOUND THEN
            LET lr_diag.diag_envio_procesar = 502
        ELSE
            IF f_verifica_saldo_dia(lr_diag.nss, HOY) = 0 THEN
               LET lr_diag.diag_envio_procesar = 502
            ELSE
               #-- Verificamos que este en un laudo judicial con la afore
               SELECT "OK"
               FROM   cta_act_marca
               WHERE  nss       = lr_diag.nss
               AND    marca_cod = 590
               GROUP BY 1
               
               IF STATUS <> NOTFOUND THEN
                   LET lr_diag.diag_envio_procesar = 504
               ELSE
                   #-- Verificamos que las subcuentas de RCV no esten con saldo cero
                   CALL f_obtiene_rcv(lr_diag.nss, HOY) RETURNING lr_subs_rcv.*
               
                   IF lr_subs_rcv.tot_sub_rcv <= 0 THEN
                       LET lr_diag.diag_envio_procesar = 509
                   END IF -- Aceptada sin recursos a transferir
               END IF -- Laudo judicial
            END IF -- Cuenta con saldo total cero
        END IF  -- Cuenta inhabilitada
             
        INSERT INTO safre_tmp:tmp_diagnostico
        VALUES(lr_diag.*)

    END FOREACH

    DISPLAY "(TERMINADO)" AT 8,30

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza la provision de las solicitudes del folio dado     #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_sol_saldo RECORD LIKE ret_solicitud_saldo.*
    
    DEFINE lr_monto_saldo RECORD LIKE ret_mto_solicitud_saldo.*

    DEFINE lr_siefore RECORD
        activo              SMALLINT        ,
        pesos_retiro        DECIMAL(14,2)   ,
        pesos_cv            DECIMAL(14,2)   ,
        pesos_cs            DECIMAL(14,2)   ,
        pesos_estatal       DECIMAL(14,2)   ,
        pesos_esp           DECIMAL(14,2)   ,
        pesos_SAR92         DECIMAL(14,2)
    END RECORD

    DEFINE lr_mto_viv RECORD
        pesos_viv97         DECIMAL(14,2) ,
        pesos_viv92         DECIMAL(14,2)
    END RECORD

    DEFINE lr_montos RECORD
        tot_viv             DECIMAL(16,6) ,
        tot_rcv             DECIMAL(16,6) ,
        mto_pesos_total     DECIMAL(16,6)
    END RECORD
    
    DEFINE lr_saldo_traspaso RECORD
        ret_97      LIKE ret_solicitud_saldo.pes_ret97_post ,      
        cs          LIKE ret_solicitud_saldo.pes_cs_post    ,
        cv          LIKE ret_solicitud_saldo.pes_cv_post    ,
        viv97       LIKE ret_solicitud_saldo.pes_viv97_post
    END RECORD

    DEFINE lr_saldo_dia RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE lc_diag_procesar LIKE ret_solicitud_saldo.diag_envio_procesar
    
    DEFINE
        ldt_fip                 ,
        ld_fecha_saldo          DATE

    DEFINE #loc #smallint
        ls_insert               ,
        ls_siefore              , #-- contador para los ciclos for
        ls_subcta               SMALLINT

    DEFINE
        li_aceptados            INTEGER,
        l_msg                   CHAR(300)

    -- -----------------------------------------------------------------------------

    -- Verifica si existen datos para transferir
    SELECT COUNT(*)
    INTO   li_aceptados
    FROM   safre_tmp:tmp_sol_saldo A,
           safre_tmp:tmp_diagnostico B
    WHERE  A.nss                    = B.nss
    AND    A.id_solicitud_saldo     = B.id_solicitud_saldo   
    AND    B.diag_envio_procesar    = 501
    AND    A.estado_solicitud       = gr_edo.sol_saldo

    IF li_aceptados = 0 THEN
        RETURN   -- SALE, NO HAY NADA QUE PROCESAR
    END IF

    DISPLAY "CALCULANDO MONTOS ...." AT 9,2

    DECLARE cur_prov CURSOR FOR
    SELECT A.*                      ,
           B.diag_envio_procesar
    FROM   safre_tmp:tmp_sol_saldo A,
           safre_tmp:tmp_diagnostico B
    WHERE  A.nss                    = B.nss
    AND    A.id_solicitud_saldo     = B.id_solicitud_saldo   
    AND    B.diag_envio_procesar    = 501
    AND    A.estado_solicitud       = gr_edo.sol_saldo

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_sol_saldo.*, lc_diag_procesar

        LET lr_siefore.activo           = FALSE
        LET lr_siefore.pesos_retiro     = 0
        LET lr_siefore.pesos_cv         = 0
        LET lr_siefore.pesos_cs         = 0
        LET lr_siefore.pesos_estatal    = 0
        LET lr_siefore.pesos_esp        = 0
        LET lr_siefore.pesos_SAR92      = 0

        -- Se inicializan variables
        LET ls_insert                   = 0
        LET lr_montos.tot_rcv           = 0
        LET lr_montos.tot_viv           = 0
        LET lr_mto_viv.pesos_viv97      = 0
        LET lr_mto_viv.pesos_viv92      = 0

        -- Si el indicador es 2 informamos el monto a la FIP
        IF lr_sol_saldo.ind_saldo_fip   = 2 THEN
            LET ldt_fip = lr_sol_saldo.fecha_ini_pen
            CALL f_genera_tmp_cuenta(lr_sol_saldo.nss) 
        ELSE
            INITIALIZE ldt_fip TO NULL
        END IF
        
        -- Si el indicador es 1 informamos con los montos del traspaso
        IF lr_sol_saldo.ind_trasp_post_fip = 1 THEN
            LET lr_saldo_traspaso.ret_97    = lr_sol_saldo.pes_ret97_post
            LET lr_saldo_traspaso.cs        = lr_sol_saldo.pes_cs_post   
            LET lr_saldo_traspaso.cv        = lr_sol_saldo.pes_cv_post   
            LET lr_saldo_traspaso.viv97     = lr_sol_saldo.pes_viv97_post
        ELSE
            LET lr_saldo_traspaso.ret_97    = 0
            LET lr_saldo_traspaso.cs        = 0
            LET lr_saldo_traspaso.cv        = 0
            LET lr_saldo_traspaso.viv97     = 0
        END IF

WHENEVER ERROR CONTINUE

        EXECUTE eje_con_saldos USING lr_sol_saldo.nss   ,
                                     HOY                ,
                                     ldt_fip            ,
                                     lr_saldo_traspaso.*
                               INTO  lr_siefore.activo              ,
                                     lr_monto_saldo.fecha_valuacion ,
                                     lr_monto_saldo.precio_accion   ,
                                     lr_siefore.pesos_retiro        ,
                                     lr_siefore.pesos_cv            ,
                                     lr_siefore.pesos_cs            ,
                                     lr_siefore.pesos_estatal       ,
                                     lr_siefore.pesos_esp           ,
                                     lr_mto_viv.pesos_viv97          

IF SQLCA.SQLCODE != 0 THEN
   LET l_msg = ""
   LET l_msg = ERR_GET(SQLCA.SQLCODE)
   ERROR "ERROR NSS: ",lr_sol_saldo.nss
   PROMPT l_msg CLIPPED ATTRIBUTE(REVERSE) FOR CHAR enter
   SLEEP 3
   EXIT PROGRAM
END IF

WHENEVER ERROR STOP

        LET lr_monto_saldo.id_solicitud_saldo   = lr_sol_saldo.id_solicitud_saldo
        LET lr_monto_saldo.siefore              = lr_siefore.activo
        LET lr_monto_saldo.folio_cargo          = 1 -- temporal
        LET lr_monto_saldo.nss                  = lr_sol_saldo.nss
        LET lr_monto_saldo.curp                 = lr_sol_saldo.curp

        DECLARE cur_subcta CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = gs_grupo_sub
        AND    subcuenta > 0
        ORDER BY 1

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_subcta INTO ls_subcta

            LET lr_saldo_dia.subcuenta = 0
            LET lr_saldo_dia.siefore   = 0
            LET lr_saldo_dia.monto_acc = 0
            LET lr_saldo_dia.monto_pes = 0

            -- Determinamos a que fecha se va a obtener el saldo
            IF (ls_subcta <> 4) AND (ls_subcta <> 8) THEN #ACS03092012 -or-->and
                LET ld_fecha_saldo = HOY
            ELSE
                LET ld_fecha_saldo = gdt_fecha_viv
            END IF

             #DISPLAY "nss ",lr_sol_saldo.nss, " subcta: ", ls_subcta, " fecha: ", ld_fecha_saldo

            CALL f_obten_saldo_subcta(lr_sol_saldo.nss, ls_subcta, ld_fecha_saldo)
                RETURNING lr_saldo_dia.*
                #DISPLAY "saldo_dia", lr_saldo_dia.*
            CASE ls_subcta
                WHEN 1
                    LET lr_monto_saldo.mto_en_pesos = lr_siefore.pesos_retiro
                    
                WHEN 2
                    LET lr_monto_saldo.mto_en_pesos = lr_siefore.pesos_cv
                    
                WHEN 4
                    LET lr_monto_saldo.mto_en_pesos = lr_mto_viv.pesos_viv97

                WHEN 5                
                    LET lr_monto_saldo.mto_en_pesos = lr_siefore.pesos_cs

                WHEN 6
                    LET lr_monto_saldo.mto_en_pesos = lr_siefore.pesos_estatal

                WHEN 7
                    LET lr_monto_saldo.mto_en_pesos = lr_saldo_dia.monto_pes

                WHEN 8
                    LET lr_monto_saldo.mto_en_pesos = lr_saldo_dia.monto_pes

                WHEN 9
                    LET lr_monto_saldo.mto_en_pesos = lr_siefore.pesos_esp
            END CASE 

            LET lr_monto_saldo.subcuenta        = ls_subcta
            LET lr_monto_saldo.mto_en_acciones  = lr_monto_saldo.mto_en_pesos/lr_monto_saldo.precio_accion

            IF lr_saldo_dia.monto_acc > 0 THEN

                CALL f_provisiona_subcta(lr_sol_saldo.curp                  , -- curp       
                                         lr_sol_saldo.nss                   , -- nss        
                                         lr_saldo_dia.subcuenta             , -- subcta     
                                         lr_sol_saldo.id_solicitud_saldo    , -- consec     
                                         lr_saldo_dia.monto_acc             , -- acciones   
                                         lr_saldo_dia.monto_pes             , -- pesos      
                                         HOY                                , -- fecha_proc 
                                         921                                , -- tipo_mov   
                                         lr_saldo_dia.siefore               ) -- siefore


            END IF -- lr_saldo_dia.monto_acc > 0

        END FOREACH -- Subcuentas

    END FOREACH -- Siguiente NSS

    DISPLAY "(TERMINADO)" AT 9,30

END FUNCTION


#---------------------------------------------------------------------------#
# tercer_paso : Se afectan las tablas fisicas del proceso una vez que se    #
#               termino correctamente                                       #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE li_folio LIKE ret_solicitud_saldo.folio_cargo

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE lr_datos RECORD
        nss             LIKE dis_provision.nss                      ,
        tipo_mov        LIKE dis_provision.tipo_movimiento          ,
        consec          LIKE dis_provision.consecutivo_lote         ,
        diag_procesar   LIKE ret_solicitud_saldo.diag_envio_procesar
    END RECORD

    DEFINE
        li_enviados         ,
        li_cont_rec         INTEGER

    DEFINE
        ls_edo_sol          SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_folio = f_ultimo_folio()

    DISPLAY "PROVISIONANDO CUENTAS ...." AT 10,2
    DISPLAY "FOLIO      : ", li_folio AT 12,13

    LET li_cont_rec = 0
    LET li_enviados = 0
    
    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_tr_provision
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_tr_provision
    WHERE  folio = li_folio

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT A.nss                    ,
           921                      ,
           A.id_solicitud_saldo     ,
           B.diag_envio_procesar
    FROM   safre_tmp:tmp_sol_saldo A,
           safre_tmp:tmp_diagnostico B
    WHERE  A.nss                    = B.nss
    AND    A.id_solicitud_saldo     = B.id_solicitud_saldo   
    AND    A.estado_solicitud       = gr_edo.sol_saldo
    ORDER BY 1

    FOREACH cur_soli INTO lr_datos.*

        IF lr_datos.diag_procesar = 501 THEN
            LET ls_edo_sol  = gr_edo.provisionado
            LET li_enviados = li_enviados + 1
        ELSE
           #Cuenta sin recursos a transferir #ACS 31052012
           #Y cuenta con diagnostico 502   ACS 03092012
           IF (lr_datos.diag_procesar = 509 OR lr_datos.diag_procesar = 502) THEN
              LET ls_edo_sol  = gr_edo.liquidado
              LET li_enviados = li_enviados + 1
           ELSE --solo desmarca 504
              CALL f_desmarca_no_liq(lr_datos.nss          ,
                                     lr_datos.consec       ,
                                     lr_datos.tipo_mov     ,
                                     gs_usuario            )
              
              LET ls_edo_sol  = gr_edo.rechazado
              LET li_cont_rec = li_cont_rec + 1
           END IF
        END IF

        -- Actualizamos la tabla de solicitudes de transferencias
        UPDATE ret_solicitud_saldo
        SET    diag_envio_procesar      = lr_datos.diag_procesar        ,
               f_recep_respuesta_safre  = HOY                           ,
               folio_cargo              = li_folio                      ,
               estado_solicitud         = ls_edo_sol
        WHERE  nss                  = lr_datos.nss
        AND    id_solicitud_saldo   = lr_datos.consec
        AND    estado_solicitud     = gr_edo.sol_saldo


        DISPLAY "ACEPTADOS  : ", li_enviados AT 13,13
        DISPLAY "RECHAZOS   : ", li_cont_rec AT 14,13

    END FOREACH
    
    -- Se inserta el registro del folio de cargo
    INSERT INTO ret_folio
    VALUES (li_folio, 0, gr_edo.provisionado)

    DISPLAY "(TERMINADO)" AT 10,30
    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW RETC7041

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_provisiona : Verifica si existen registros a provisionar. En     #
#                       caso de existir la funcion pasa las solicitudes a   #
#                       la tabla temporal y regresa el numero de solicitudes#
#---------------------------------------------------------------------------#
FUNCTION f_valida_provisiona()

    DEFINE
        li_tot_soli         INTEGER

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_tot_soli = 0

#ACS se agrega fecha a peticion de la afore 19Jun2012
    -- Pasamos todas las solicitudes en estado "Solicitud Saldo" a la tabla temporal
    --DISPLAY "fec ",m_fecha
    INSERT INTO safre_tmp:tmp_sol_saldo
    SELECT *
    FROM   ret_solicitud_saldo
    WHERE  estado_solicitud     = gr_edo.sol_saldo
    AND DATE(f_recep_procesar)= m_fecha

    SELECT COUNT(*)
    INTO   li_tot_soli
    FROM   safre_tmp:tmp_sol_saldo

    IF li_tot_soli <= 0 THEN
        CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
        LET ls_procesa  = 0
    ELSE
        LET ls_procesa  = 1
    END IF

    RETURN ls_procesa, li_tot_soli

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de transferencias                              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW RETC7041 AT 4,4 WITH FORM "RETC7041" ATTRIBUTE(BORDER)
    DISPLAY " RETC704                                                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "       [Esc]Provision                                  [Ctrl-C]Salir         " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "            EJECUCION DE PROVISION DE RECURSOS A DESINVERTIR                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 1,67 ATTRIBUTE(REVERSE)

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

    -- -----------------------------------------------------------------------------

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

#CPL-1765 INI
            LET  lr_sal_dia.subcta = pr_datos.subcta
            LET  lr_sal_dia.siefore = gs_sieviv
            LET  lr_sal_dia.monto_acc = ld_saldo_dia_viv  
            LET  lr_sal_dia.monto_pes = ld_saldo_dia_viv * gar_precio_acc[gs_sieviv].precio_dia
            LET  lr_sal_dia.monto_pes = f_redondea_val(lr_sal_dia.monto_pes, 2)    
#CPL-1765 FIN
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
# f_verifica_saldo_dia : Verifica si la cuenta individual tiene saldo en    #
#                        sus subcuentas                                     #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_saldo_dia(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_trans_issste.nss           ,
        fec_saldo   LIKE dis_provision.fecha_conversion
    END RECORD

    DEFINE lr_sal_dia RECORD
        subcta      LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE ld_saldo_dia LIKE dis_provision.monto_en_acciones

    DEFINE
        ls_subcta           ,
        ls_grupo            SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET ls_grupo            = 0
    LET ls_subcta           = 0
    LET ld_saldo_dia        = 0
    INITIALIZE lr_sal_dia.* TO NULL

    DECLARE cur_saldo_dia CURSOR FOR eje_saldo_dia

     OPEN cur_saldo_dia USING pr_datos.nss       ,
                                 ls_subcta          ,
                                 ls_grupo           ,
                                 pr_datos.fec_saldo

    FOREACH cur_saldo_dia INTO lr_sal_dia.*

        IF lr_sal_dia.monto_pes <= 0 OR lr_sal_dia.monto_pes IS NULL THEN
            LET lr_sal_dia.monto_pes  = 0
        END IF

        LET ld_saldo_dia = ld_saldo_dia + lr_sal_dia.monto_pes

    END FOREACH

    CLOSE cur_saldo_dia

    RETURN ld_saldo_dia

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

    -- -----------------------------------------------------------------------------

    LET si_provisiono = 0
    LET folio_sua     = ""
    LET id_aporte     = "RETIRO"

    LET ld_acc          = -pr_provi.acciones
    LET ld_pesos        = -pr_provi.pesos
    LET ld_precio_acc   = gar_precio_acc[ps_sie].precio_dia

    IF (pr_provi.subcta = 4) OR (pr_provi.subcta = 8) THEN
        LET ldt_fec_proc = gdt_fecha_viv
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
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(76)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR  "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

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

    -- -----------------------------------------------------------------------------
    
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

    -- -----------------------------------------------------------------------------

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
# f_ultimo_folio : Obtiene el ultimo folio que se usara para procesar los   #
#                  registros de disposiciones                               #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER


    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio


END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera la tabla temporal tmp_dis_cuenta que         #
#                       contiene la informacion historica de la cuenta      #
#                       individual del nss                                  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(p_nss)

    DEFINE
         p_nss                  CHAR(11)  ,
         lc_nom_tabla           CHAR(20)  ,
         lc_sel_hist            CHAR(1500)

     WHENEVER ERROR CONTINUE
         DROP TABLE tmp_dis_cuenta
     WHENEVER ERROR STOP

     DECLARE cur_his CURSOR FOR
     SELECT tabname
     FROM   systables
     WHERE  tabname matches "dis_cuenta??"

     FOREACH cur_his INTO lc_nom_tabla

        LET lc_sel_hist = lc_sel_hist CLIPPED,
                          " SELECT * ",
                          " FROM ",lc_nom_tabla          ,
                          " WHERE nss = ","'",p_nss,"'"  ,
                          " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET lc_sel_hist = lc_sel_hist CLIPPED,
                      " SELECT * ",
                      " FROM dis_cuenta ",
                      " WHERE nss = ","'",p_nss,"'"  ,
                      " INTO TEMP tmp_dis_cuenta "
    
    PREPARE eje_sel_his FROM lc_sel_hist
    EXECUTE eje_sel_his
    
    CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta (tipo_movimiento)
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_rcv : Obtiene los montos de RCV del nss en la fecha dada          #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_rcv(p_nss, pdt_fecha)

    DEFINE p_nss        LIKE ret_solicitud_saldo.nss

    DEFINE ls_sie       LIKE dis_provision.siefore

    DEFINE lr_rcv RECORD
        ret_97          DECIMAL(16,2),
        viv_97          DECIMAL(16,2), #ACS 04.06.2012
        ces_vej         DECIMAL(16,2),
        cuota_soc       DECIMAL(16,2),
        tot_sub_rcv     DECIMAL(16,2)
    END RECORD

    DEFINE
        pdt_fecha           DATE

    DEFINE 
        ld_monto            DECIMAL(16,2)

    -- -----------------------------------------------------------------------------

    LET ld_monto            = 0
    LET lr_rcv.ret_97       = 0
    LET lr_rcv.viv_97       = 0
    LET lr_rcv.ces_vej      = 0
    LET lr_rcv.cuota_soc    = 0
    LET lr_rcv.tot_sub_rcv  = 0

     #Obtiene saldos de vivienda --> ACS - 04062012

    LET ld_monto = 0

        SELECT SUM(monto_en_acciones) * gar_precio_acc[gs_sieviv].precio_dia
        INTO   lr_rcv.viv_97
        FROM   dis_cuenta
        WHERE  nss        = p_nss
        AND    siefore    = gs_sieviv
        AND    subcuenta  = 4
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF lr_rcv.viv_97 IS NULL THEN
           LET lr_rcv.viv_97 = 0
        END IF

    DECLARE cur_rcv_01 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        = 1
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_01 INTO ls_sie

        -- Se obtienen los montos que se insertaran en la tabla
        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        = 1
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.ret_97 = lr_rcv.ret_97 + ld_monto

    END FOREACH

    LET ld_monto = 0

    DECLARE cur_rcv_02 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        IN (2,6,9)
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_02 INTO ls_sie

        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        IN (2,6,9)
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.ces_vej  = lr_rcv.ces_vej + ld_monto

    END FOREACH

    LET ld_monto = 0

    DECLARE cur_rcv_03 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        = 5
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_03 INTO ls_sie

        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        = 5
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.cuota_soc = lr_rcv.cuota_soc + ld_monto

    END FOREACH

    LET lr_rcv.tot_sub_rcv = lr_rcv.ret_97 + lr_rcv.ces_vej + lr_rcv.cuota_soc + lr_rcv.viv_97

    RETURN lr_rcv.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_no_liq : Ejecuta el SPL que realiza la desmarca de las cuentas #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_no_liq(pr_desmarca)

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

    -- -----------------------------------------------------------------------------

    LET ls_desmarca         = 1
    LET ls_cont             = 0
    LET lr_dat.edo_marca    = 40 --Desmarca por validación interna
    LET lr_dat.marca_causa  = 0

    --MARCAJE--
    EXECUTE eje_desmarca USING pr_desmarca.nss          ,--nss
                               pr_desmarca.tipo_mov     ,--marca entrante
                               pr_desmarca.consec       ,--consecutivo
                               lr_dat.edo_marca         ,--estado_marco
                               lr_dat.marca_causa       ,--marca_causa
                               pr_desmarca.usuario       --usuario
    

--    RETURN ls_desmarca

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
        DROP TABLE tmp_sol_saldo
    WHENEVER ERROR STOP

    --------------------------------

    CREATE TABLE tmp_diagnostico(
        id_solicitud_saldo      DECIMAL(11,0)   ,
        nss                     CHAR(11)        ,
        curp                    CHAR(18)        ,
        diag_recep_afore        SMALLINT        ,
        diag_envio_procesar     SMALLINT        
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
#MLM-2179  INI
    CREATE TABLE tmp_sol_saldo (
        id_solicitud_saldo          DECIMAL(11,0),
        folio_t_procesar            CHAR(50),
        folio_cargo                 DECIMAL(11,0),
        nss                         CHAR(11),
        curp                        CHAR(18),
        entidad_origen              CHAR(3),
        tipo_tramite                CHAR(3),
        nombre                      CHAR(40),
        apellido_paterno            CHAR(40),
        apellido_materno            CHAR(40),
        num_issste                  CHAR(10),
        num_folio_iss               CHAR(14),
        fecha_sol_trabajador        DATETIME YEAR TO SECOND,  
        fecha_ini_pen               DATETIME YEAR TO SECOND,  
        ind_saldo_fip               SMALLINT,
        clave_pension               CHAR(3),
        ind_portabilidad            SMALLINT,
        ind_trasp_post_fip          SMALLINT,
        pes_ret97_post              DECIMAL(15,2),
        pes_cs_post                 DECIMAL(15,2),
        pes_cv_post                 DECIMAL(15,2),
        pes_viv97_post              DECIMAL(15,2),
        pes_saldo_viv97             DECIMAL(15,2),
        pes_ahorro_sol_post         DECIMAL(15,2),
        pes_cv_iss_post             DECIMAL(15,2),
        pes_ret_iss08_post          DECIMAL(15,2),
        pes_cs_iss_post             DECIMAL(15,2),
        pes_apor_comp_post          DECIMAL(15,2),
        pes_apor_vol_post           DECIMAL(15,2),
        pes_apor_lp_post            DECIMAL(15,2),
        pes_fov08_post              DECIMAL(15,2),
        pes_saldo_fov08             DECIMAL(15,2),
        estado_vivienda             SMALLINT,
        fecha_vencimiento           DATETIME YEAR TO SECOND,
        f_recep_procesar            DATETIME YEAR TO SECOND,
        f_envio_a_safre             DATETIME YEAR TO SECOND,
        f_recep_respuesta_safre     DATETIME YEAR TO SECOND,
        f_respuesta_a_procesar      DATETIME YEAR TO SECOND,
        diag_recep_afore            SMALLINT,
        diag_envio_procesar         SMALLINT,
        ind_recep_procesar          SMALLINT,
        usuario_carga               CHAR(18),
        estado_solicitud            SMALLINT
      )
#MLM-2179  FIN

    GRANT ALL ON tmp_sol_saldo TO PUBLIC

    DATABASE safre_af

END FUNCTION
