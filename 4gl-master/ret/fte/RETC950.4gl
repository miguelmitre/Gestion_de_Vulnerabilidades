#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC950  => GENERA Y PROVISIONA SALDOS DE DISPOSICIONES ISSSTE        #
#Fecha creacion    => 12 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos validos de      #
#                     retiros totales ISSSTE                                    #
#Sistema           => RET                                                       #
#################################################################################
#Actualizacion     => CPL-2107    30-Sep-2015   Jonathan Joddy Zavala Zavala    #
#Requerimiento     => Se actualiza programa adecuando las caracteristicas       #
#                  => incorporadas por la creación de la siefore basica 0/90    #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        capturado               LIKE ret_estado_issste.estado_solicitud ,
        confirmado              LIKE ret_estado_issste.estado_solicitud ,
        provisionado            LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_tipo RECORD
        transferencia           LIKE tab_tipo_tramite.cod_tramite       ,
        disposicion             LIKE tab_tipo_tramite.cod_tramite
    END RECORD

    #DEFINE gar_precio_acc ARRAY [20] OF RECORD  #CPL-2107
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
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

    DEFINE
        gs_retiro_B             ,
        gs_transf_I             ,
        gs_num_disp             ,
        gs_tipo_op              ,
        gs_xxi                  ,
        gs_inv                  ,
        gs_sieviv               ,
        gs_num_siefores         , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore         SMALLINT

    DEFINE #glo #integer
        gs_ult_folio          INTEGER

END GLOBALS

DEFINE ms_99_siefores    SMALLINT   #CPL-2107

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC950")


    CALL init()
    CALL f_obtiene_precios_accion(HOY)

    IF f_despliega_info() THEN
        CALL f_abre_ventana()
        CALL f_tablas_tmp()
        CALL primer_paso()   #-- Provision de solicitudes
        CALL segundo_paso()  #-- Afecto tablas fisicas
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

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gs_tipo_op          = 45
    LET gs_sieviv           = 12
    LET ls_dias_hab         = 2
    LET gs_num_siefores     = f_lib_obtiene_num_siefores()
    LET ms_99_siefores      = 99

    ----- TIPOS DE MOVIMIENTO -----
    SELECT cod_tramite
    INTO   gr_tipo.transferencia
    FROM   tab_tipo_tramite
    WHERE  descripcion    = "TRANSFERENCIA ISSSTE"

    SELECT cod_tramite
    INTO   gr_tipo.disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion    = "DISPOSICION ISSSTE"

    -- Indica el numero de retiros por disposicion
    SELECT COUNT(*)
    INTO   gs_num_disp
    FROM   tab_ret_issste                                 
    WHERE  cod_tramite = gr_tipo.disposicion

    SELECT movimiento
    INTO   gs_retiro_B
    FROM   tab_ret_issste
    WHERE  tipo_retiro  = "B"
    AND    cod_tramite  = gr_tipo.disposicion
    
    SELECT movimiento
    INTO   gs_transf_I
    FROM   tab_ret_issste
    WHERE  tipo_retiro  = "I"
    AND    cod_tramite  = gr_tipo.transferencia

    ----- FECHA DE VIVIENDA (HOY + 2 dias habiles)  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_siguiente ( ?,? )"
    PREPARE eje_dia_sig FROM lc_prepare

    LET lc_prepare = " "

    EXECUTE eje_dia_sig USING HOY, ls_dias_hab INTO gdt_fecha_viv

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gs_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_xxi
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*AFORE XXI*"

    SELECT afore_cod
    INTO   gs_inv
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*INVERCAP*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"

    ----- SALDO AL DIA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia_isss (?,?,?,?) "
    PREPARE prp_saldo_dia FROM lc_prepare
        
    LET lc_prepare = " "

    ----- APORTACIONES POSTERIORES AL BIMESTRE DE LA FIP -----
    LET lc_prepare = " EXECUTE FUNCTION fn_app_pos_fip_issste(?,?,?) "
    PREPARE eje_app_post_fip FROM lc_prepare


    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    provision de disposiciones                             #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    -- Se define de acuerdo al numero tipos de retiros por disposicion
    DEFINE lar_ret ARRAY[8] OF RECORD
        tipo_retiro        LIKE ret_sol_issste_tx.tipo_retiro,
        desc_retiro        LIKE tab_ret_issste.descripcion   ,
        num_cap            INTEGER                           ,
        num_conf           INTEGER                           ,
        num_prov           INTEGER
    END RECORD


    DEFINE lr_soli RECORD
        tipo_retiro        SMALLINT                             ,
        edo_soli           LIKE ret_sol_issste_tx.estado_solicitud   ,
        num_regs           INTEGER
    END RECORD

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    OPEN WINDOW retc9501 AT 4,4 WITH FORM "RETC9501" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                        <CTRL-P> PROVISIONAR   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC950    PROVISION DE SOLICITUDES DE RETIROS TOTALES ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    FOR ls_cont = 1 TO gs_num_disp
        INITIALIZE lar_ret[ls_cont].* TO NULL
    END FOR
    
    LET lar_ret[1].tipo_retiro  = "A"
    LET lar_ret[2].tipo_retiro  = "B"
    LET lar_ret[3].tipo_retiro  = "C"
    LET lar_ret[4].tipo_retiro  = "D"
    LET lar_ret[5].tipo_retiro  = "E"
    LET lar_ret[6].tipo_retiro  = "I"
    LET lar_ret[7].tipo_retiro  = "K"
    LET lar_ret[8].tipo_retiro  = "M"

    FOR ls_cont = 1 TO gs_num_disp
        SELECT descripcion
        INTO   lar_ret[ls_cont].desc_retiro
        FROM   tab_ret_issste
        WHERE  tipo_retiro  = lar_ret[ls_cont].tipo_retiro
        AND    cod_tramite  = gr_tipo.disposicion

        LET lar_ret[ls_cont].num_cap  = 0
        LET lar_ret[ls_cont].num_conf = 0
        LET lar_ret[ls_cont].num_prov = 0
    END FOR

    LET li_tot_prov = 0
    LET ls_flag     = 1
    LET ls_cont     = 1

    LET lc_query = " SELECT CASE tipo_retiro    ",
                      " WHEN 'A' THEN 1 ",
                      " WHEN 'B' THEN 2 ",
                      " WHEN 'C' THEN 3 ",
                      " WHEN 'D' THEN 4 ",
                      " WHEN 'E' THEN 5 ",
                      " WHEN 'I' THEN 6 ",
                      " WHEN 'K' THEN 7 ",
                      " WHEN 'M' THEN 8 ",
                      " END , ",
                   "        estado_solicitud ,  ",
                   "        COUNT(*)            ",
                   " FROM   ret_sol_issste_tx   ",
                   " WHERE  estado_solicitud IN (?,?) ",
                   " AND    (codigo_rechazo = 0 OR codigo_rechazo IS NULL)   ",
                   " AND    tipo_retiro IN ('A','B','C','D','E','I','K','M') ",
                   " GROUP BY 1,2               ",
                   " ORDER BY 1,2               "

    PREPARE prp_datos_tot FROM lc_query
    DECLARE cur_datos_tot CURSOR FOR prp_datos_tot

    FOREACH cur_datos_tot USING gr_edo.capturado,
                                gr_edo.confirmado
                          INTO lr_soli.*

        LET ls_ret = lr_soli.tipo_retiro

        IF lr_soli.edo_soli = gr_edo.capturado THEN
            LET lar_ret[ls_ret].num_cap  = lr_soli.num_regs
        ELSE
            LET lar_ret[ls_ret].num_conf = lr_soli.num_regs
            LET lar_ret[ls_ret].num_prov = lr_soli.num_regs
            LET li_tot_prov = li_tot_prov + lr_soli.num_regs
        END IF

        LET ls_cont = ls_cont + 1
    
    END FOREACH

    IF li_tot_prov = 0 THEN
        PROMPT " NO EXISTEN REGISTROS PARA PROVISIONAR ... <ENTER> PARA SALIR " FOR CHAR enter
        LET ls_flag = 0
    ELSE
        CALL SET_COUNT(gs_num_disp)
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

    CLOSE WINDOW retc9501

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Realiza la provision de disposiciones                       #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_soli RECORD LIKE ret_sol_issste_tx.*

    #DEFINE arr_siefore ARRAY [20] OF RECORD  #CPL-2107
    DEFINE arr_siefore ARRAY [99] OF RECORD
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

    DEFINE lr_saldo RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        ldt_fecha_saldo             DATE

    DEFINE
        ls_insert                   ,
        ls_sie_unica                ,
        ls_tipo_mov                 ,
        ls_siefore                  , -- contador para los ciclos for
        ls_subcta                   SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "CALCULANDO MONTOS ...." AT 6,2

    DECLARE cur_prov CURSOR FOR
        SELECT  A.*,
                B.movimiento
        FROM    ret_sol_issste_tx A ,
                tab_ret_issste B
        WHERE   A.estado_solicitud  = gr_edo.confirmado
        AND     ( A.codigo_rechazo    = 0
                   OR A.codigo_rechazo IS NULL )
        AND     A.tipo_retiro       = B.tipo_retiro
        AND     B.cod_tramite       = gr_tipo.disposicion
        ORDER BY A.tipo_retiro, A.curp

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_soli.*, ls_tipo_mov

        -- Inicializamos variables del arreglo
        #FOR ls_siefore = 1 TO gs_num_siefores   #MLM-2107
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo          = FALSE
            LET arr_siefore[ls_siefore].acciones_ret08  = 0
            LET arr_siefore[ls_siefore].acciones_cv     = 0
            LET arr_siefore[ls_siefore].acciones_ahsol  = 0
            LET arr_siefore[ls_siefore].acciones_ret92  = 0
            LET arr_siefore[ls_siefore].acciones_comp   = 0
            LET arr_siefore[ls_siefore].pesos_banxico   = 0
        END FOR

        LET ls_insert             = 0
        LET lr_mto_viv.acc_viv08  = 0
        LET lr_mto_viv.acc_viv92  = 0

        DECLARE cur_subcta CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = lr_soli.grupo
        AND    subcuenta > 0

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_subcta INTO ls_subcta

            LET lr_saldo.subcuenta = 0
            LET lr_saldo.siefore   = 0
            LET lr_saldo.monto_acc = 0
            LET lr_saldo.monto_pes = 0

            -- Determinamos a que fecha se va a obtener el saldo
            IF (ls_subcta <> 14) AND (ls_subcta <> 35) THEN
                LET ldt_fecha_saldo = HOY
            ELSE
                LET ldt_fecha_saldo = gdt_fecha_viv
            END IF

-- Comentario por Javier Gonzalez
-- Provisional, ya que aun no se define como funcionara el tipo K
-- 21/10/2012
{
            -- Si el tipo retiro es K (reingreso), provisionamos las aportaciones posteriores
            -- al bimestre de la fip
            IF lr_soli.tipo_retiro = "K" THEN

                --Carga en tabla temporal histórico de movimiento--
                CALL f_genera_tmp_cuenta (lr_soli.nss) #gtc

                --Elimina todos los movimientos anteriores al último retiro--
                CALL f_elimina_reingresos(lr_soli.nss, ls_tipo_mov) 
                
                CALL f_provisiona_ap_posteriores(lr_soli.nss, ls_subcta, lr_soli.fecha_ini_pen)
                    RETURNING lr_saldo.*
            
                CALL f_verifica_sobregiro(lr_soli.nss, 
                                          ls_subcta, 
                                          lr_saldo.monto_acc,
                                          lr_saldo.monto_pes,
                                          ldt_fecha_saldo)
                    RETURNING lr_saldo.monto_acc, lr_saldo.monto_pes
            ELSE
                CALL f_obten_saldo_subcta(lr_soli.nss, ls_subcta, ldt_fecha_saldo)
                    RETURNING lr_saldo.*
            
                IF (ls_subcta <> 14) AND (ls_subcta <> 35) AND (ls_subcta <> 19) THEN
                    LET ls_sie_unica = lr_saldo.siefore
                END IF
            
            END IF
}
-- Fin comentario
            
            CALL f_obten_saldo_subcta(lr_soli.nss, ls_subcta, ldt_fecha_saldo)
                RETURNING lr_saldo.*
            
            IF (ls_subcta <> 14) AND (ls_subcta <> 35) AND (ls_subcta <> 19) THEN
                LET ls_sie_unica = lr_saldo.siefore
            END IF            

            IF (lr_saldo.subcuenta <> 19 AND lr_saldo.monto_acc > 0) OR
               ( lr_saldo.subcuenta = 19 AND lr_saldo.monto_pes > 0 AND lr_soli.regimen <> 'DT' AND lr_soli.tipo_retiro <> 'B') THEN

                IF lr_saldo.siefore <> gs_sieviv THEN

                    IF lr_saldo.siefore <> 0 THEN 
                        LET ls_siefore = lr_saldo.siefore
                    END IF

                    -- Marcamos como activo el registro de la siefore actual
                    LET arr_siefore[ls_siefore].activo = TRUE
                    CASE
                        WHEN lr_saldo.subcuenta = 30
                            LET arr_siefore[ls_siefore].acciones_ret08 = lr_saldo.monto_acc

                        WHEN (lr_saldo.subcuenta = 31) OR (lr_saldo.subcuenta = 32)
                            LET arr_siefore[ls_siefore].acciones_cv = lr_saldo.monto_acc 
                                            + arr_siefore[ls_siefore].acciones_cv 

                        WHEN (lr_saldo.subcuenta = 33) OR (lr_saldo.subcuenta = 34)
                            LET arr_siefore[ls_siefore].acciones_ahsol = lr_saldo.monto_acc
                                            + arr_siefore[ls_siefore].acciones_ahsol 

                        WHEN lr_saldo.subcuenta = 13
                            LET arr_siefore[ls_siefore].acciones_ret92 = lr_saldo.monto_acc

                        WHEN lr_saldo.subcuenta = 19
                           LET arr_siefore[ls_siefore].pesos_banxico   = lr_saldo.monto_pes

                        WHEN (lr_saldo.subcuenta = 24) OR (lr_saldo.subcuenta = 25)
                            LET arr_siefore[ls_siefore].acciones_comp = lr_saldo.monto_acc 
                                             + arr_siefore[ls_siefore].acciones_comp
                                            
                    END CASE
                ELSE
                    CASE lr_saldo.subcuenta
                        WHEN 35
                            LET lr_mto_viv.acc_viv08 = lr_saldo.monto_acc
                        WHEN 14
                            LET lr_mto_viv.acc_viv92 = lr_saldo.monto_acc
                    END CASE
                END IF

                CALL f_provisiona_subcta(lr_soli.curp           ,
                                         lr_soli.nss            ,
                                         lr_saldo.subcuenta     ,
                                         lr_soli.consecutivo    ,
                                         lr_saldo.monto_acc     ,
                                         lr_saldo.monto_pes     ,
                                         ldt_fecha_saldo         ,
                                         ls_tipo_mov            ,
                                         lr_saldo.siefore       )
            END IF
        END FOREACH -- Subcuentas


        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2107
        FOR ls_siefore = 1 TO ms_99_siefores

            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN
                
                LET ls_insert = 1
                
                INSERT INTO safre_tmp:tmp_monto_issste
                    VALUES(lr_soli.curp                             , -- curp
                           lr_soli.consecutivo                      , -- consecutivo
                           1                                        , -- folio
                           lr_soli.tipo_retiro                      , -- tipo_retiro
                           gs_tipo_op                               , -- tipo_operacion
                           ls_siefore                               , -- siefore
                           arr_siefore[ls_siefore].acciones_ret08   , -- acciones_ret08
                           arr_siefore[ls_siefore].acciones_cv      , -- acciones_cv
                           arr_siefore[ls_siefore].acciones_ahsol   , -- acciones_ahsol
                           arr_siefore[ls_siefore].acciones_ret92   , -- acciones_ret92
                           arr_siefore[ls_siefore].acciones_comp    , -- acciones_comp
                           arr_siefore[ls_siefore].pesos_banxico      -- pesos banxico
                          )

            END IF
        END FOR
        
        -- Si no encontro montos provisionados de las siefores entonces se inserta en ceros 
        -- el registro de tmp_monto_issste
        IF NOT ls_insert THEN
            INSERT INTO safre_tmp:tmp_monto_issste
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
                       0                                       -- pesos banxico
                      )
        END IF

        INSERT INTO safre_tmp:tmp_monto_viv_issste
            VALUES(lr_soli.curp         , -- curp
                   lr_soli.consecutivo  , -- consecutivo
                   1                    , -- folio
                   lr_soli.tipo_retiro  , -- tipo_retiro
                   gs_tipo_op           , -- tipo_operacion
                   gdt_fecha_viv        , -- fecha_valor_viv
                   lr_mto_viv.acc_viv08 , -- acciones_viv08
                   lr_mto_viv.acc_viv92 , -- acciones_viv92
                   NULL                 , -- estado_sub_viv
                   0                    , -- acc_viv97_bdsviv
                   0                      -- acc_viv92_bdsviv
                  )

    END FOREACH -- Siguiente NSS

    DISPLAY "(TERMINADO)" AT 6,25

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza el paso de informacion de las tablas temporales a  #
#                las fisicas                                                #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE la_rets ARRAY[8] OF INTEGER

    DEFINE lr_datos RECORD
        curp        LIKE dis_provision.curp             ,
        tipo_mov    LIKE dis_provision.tipo_movimiento  ,
        consec      LIKE dis_provision.consecutivo_lote
    END RECORD

    ------------------------------------------------------------------

    DISPLAY "PROVISIONANDO CUENTAS ...." AT 8,2

    LET gs_ult_folio = f_lib_obtiene_ult_folio()

    DISPLAY "FOLIO    : ", gs_ult_folio AT 10,13

    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_provision
    SET    folio = gs_ult_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_provision
    WHERE  folio = gs_ult_folio

    -- Copiamos la tabla de montos temporal a la definitiva
    UPDATE safre_tmp:tmp_monto_issste
    SET    folio = gs_ult_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_issste
    SELECT *
    FROM   safre_tmp:tmp_monto_issste
    WHERE  folio = gs_ult_folio

    -- Copiamos la tabla de montos de vivienda temporal a la definitiva
    UPDATE safre_tmp:tmp_monto_viv_issste
    SET    folio = gs_ult_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_viv_issste
    SELECT *
    FROM   safre_tmp:tmp_monto_viv_issste
    WHERE  folio = gs_ult_folio

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT UNIQUE(curp)    ,
           tipo_movimiento ,
           consecutivo_lote
    FROM   dis_provision
    WHERE  folio = gs_ult_folio
    ORDER BY 1

    FOREACH cur_soli INTO lr_datos.*

        UPDATE ret_sol_issste_tx
        SET    folio            = gs_ult_folio       ,
               estado_solicitud = gr_edo.provisionado
        WHERE  curp             = lr_datos.curp
        AND    consecutivo      = lr_datos.consec
        AND    estado_solicitud = gr_edo.confirmado

        SELECT 'OK'
        FROM ret_sol_issste_tx
        WHERE  folio  = gs_ult_folio
        AND    curp   = lr_datos.curp
        AND    consecutivo = lr_datos.consec
        AND    estado_solicitud = gr_edo.provisionado
        GROUP BY 1
        
        IF SQLCA.SQLCODE = NOTFOUND THEN 
             CALL ERRORLOG("No se actualizo el estado del curp: "||lr_datos.curp||" para el folio: "||gs_ult_folio)
        END IF  

        CASE lr_datos.tipo_mov
            WHEN 851
                LET la_rets[1] = la_rets[1] + 1
            WHEN 852
                LET la_rets[2] = la_rets[2] + 1
            WHEN 853
                LET la_rets[3] = la_rets[3] + 1
            WHEN 854
                LET la_rets[4] = la_rets[4] + 1
            WHEN 855
                LET la_rets[5] = la_rets[5] + 1
            WHEN 862 -- Tipo I para Transferencias (Invercap)
                LET la_rets[6] = la_rets[6] + 1
            WHEN 864 -- Tipo I para Disposiciones (Metlife)
                LET la_rets[6] = la_rets[6] + 1
            WHEN 858
                LET la_rets[7] = la_rets[7] + 1
            WHEN 859
                LET la_rets[8] = la_rets[8] + 1

        END CASE

        DISPLAY "RETIRO A : ", la_rets[1] AT 12,13
        DISPLAY "RETIRO B : ", la_rets[2] AT 13,13
        DISPLAY "RETIRO C : ", la_rets[3] AT 14,13
        DISPLAY "RETIRO D : ", la_rets[4] AT 12,46
        DISPLAY "RETIRO E : ", la_rets[5] AT 13,46
        DISPLAY "RETIRO I : ", la_rets[6] AT 14,46
        DISPLAY "RETIRO K : ", la_rets[7] AT 15,13
        DISPLAY "RETIRO M : ", la_rets[8] AT 16,13
    END FOREACH

    -- Si la afore es INVERCAP se cambia el tipo de movimiento de los retiros I a B
    IF gs_codigo_afore = gs_inv THEN
        UPDATE dis_provision
        SET    tipo_movimiento  = gs_retiro_B
        WHERE  tipo_movimiento  = gs_transf_I
        AND    folio            = gs_ult_folio
    END IF


    DISPLAY "(TERMINADO)" AT 8,29
    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc9502

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de dispocisiones                               #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9502 AT 4,4 WITH FORM "RETC9502" ATTRIBUTE(BORDER)
    DISPLAY "                                                       DISPOSICIONES ISSSTE " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC950    PROVISION DE SOLICITUDES DE RETIROS TOTALES ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_saldo_subcta : Obtiene el saldo del nss de la subcuenta indicada  #
#                        a la fecha dada por pr_datos.fec_saldo             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_subcta(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_sol_issste_tx.nss          ,
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

    DEFINE
        lc_prepare          CHAR(300)
    
    -- -----------------------------------------------------------------------------

    LET ls_grupo            = 0
    LET lr_sal_dia.subcta   = 0
    LET lr_sal_dia.siefore  = 0

    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia_isss (?,?,?,?) "
    PREPARE prp_saldo FROM lc_prepare
    DECLARE cur_saldo CURSOR FOR prp_saldo

    FOREACH cur_saldo USING pr_datos.nss       ,
                            pr_datos.subcta    ,
                            ls_grupo           ,
                            pr_datos.fec_saldo
                      INTO lr_sal_dia.*
                      
        EXIT FOREACH 
    
    END FOREACH

    IF lr_sal_dia.subcta <> 19 THEN

        -- Si la siefore es 6 se quita el saldo ya que no debe provisionar dicha siefore
        IF lr_sal_dia.siefore = 6 THEN
            LET lr_sal_dia.monto_acc = 0
            LET lr_sal_dia.monto_pes = 0        
        END IF 
        
        -- Si no tiene saldo en la subcuenta se manda el saldo como cero
        IF lr_sal_dia.monto_acc IS NULL OR lr_sal_dia.monto_acc = 0 THEN
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
# f_provisiona_ap_posteriores : Obtiene el monto que se debe provisionar en #
#                               la subcuenta dada cuando se deba pagar las  #
#                               aportaciones posteriores al bimestre de la  #
#                               fip de la subcuenta                         #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_ap_posteriores(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_sol_issste_tx.nss  ,
        subcta      SMALLINT                    ,
        fip         DATE
    END RECORD

    DEFINE lr_ap_post RECORD
        subcuenta   LIKE dis_provision.subcuenta        ,
        siefore     LIKE dis_provision.siefore          ,
        monto_acc   LIKE dis_provision.monto_en_acciones,
        monto_pes   LIKE dis_provision.monto_en_pesos
    END RECORD

    DEFINE
        ls_sie      SMALLINT

    -- -----------------------------------------------------------------------------

    LET lr_ap_post.monto_acc = 0
    LET lr_ap_post.siefore   = 0

    DECLARE c_aportes_pos CURSOR FOR eje_app_post_fip
    OPEN c_aportes_pos  USING pr_datos.*

    FETCH c_aportes_pos INTO lr_ap_post.subcuenta ,
                             lr_ap_post.siefore   ,
                             lr_ap_post.monto_acc

    CLOSE c_aportes_pos

    IF lr_ap_post.siefore = gs_sieviv THEN
        --Si la siefore es de vivienda se debe redondear el valor en pesos a dos decimales
        LET lr_ap_post.monto_acc = f_lib_redondea_val(lr_ap_post.monto_acc, 2)
        LET lr_ap_post.monto_pes = f_lib_redondea_val(lr_ap_post.monto_acc * gar_precio_acc[gs_sieviv].precio_dia, 2)
    ELSE
        IF lr_ap_post.siefore <> 0 THEN
            LET ls_sie               = lr_ap_post.siefore
            LET lr_ap_post.monto_pes = lr_ap_post.monto_acc * gar_precio_acc[ls_sie].precio_dia
        END IF
    END IF

    RETURN lr_ap_post.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_sobregiro : Verifica que el monto de aportaciones posteriores  #
#                        no sobregire las subcuentas a liquidar             #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_sobregiro(pr_ap_post)

    DEFINE pr_ap_post RECORD
        nss             LIKE ret_sol_issste_tx.nss           ,
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

    -- -----------------------------------------------------------------------------

    LET ld_monto_acc            = 0 
    LET ld_monto_pes            = 0 
    LET ls_grupo                = 0
    LET lr_saldo_dia.monto_acc  = 0

    DECLARE cur_val_sobre CURSOR FOR prp_saldo_dia

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
# f_provisiona_subcta : Realiza la provision del monto dado en la subcuenta #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_subcta(pr_provi)

    DEFINE pr_provi RECORD
        curp        LIKE ret_sol_issste_tx.curp         ,
        nss         LIKE ret_sol_issste_tx.nss          ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_sol_issste_tx.consecutivo  ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE                                ,
        tipo_mov    SMALLINT                            ,
        siefore     SMALLINT 
    END RECORD

    DEFINE lr_provision RECORD LIKE dis_provision.*

    DEFINE ld_precio_acc LIKE dis_provision.precio_accion
    
    DEFINE
        ls_sie              SMALLINT

    -- -----------------------------------------------------------------------------

    IF (pr_provi.subcta = 14) OR (pr_provi.subcta = 35) THEN
        IF gs_codigo_afore = gs_xxi THEN
            LET pr_provi.pesos = f_lib_redondea_val(pr_provi.pesos, 2)
        END IF
    END IF

    IF pr_provi.siefore <> 0 THEN
        LET ls_sie          = pr_provi.siefore
        LET ld_precio_acc   = gar_precio_acc[ls_sie].precio_dia
    ELSE
        LET ld_precio_acc   = 0
    END IF

    LET lr_provision.tipo_movimiento    = pr_provi.tipo_mov  
    LET lr_provision.subcuenta          = pr_provi.subcta    
    LET lr_provision.siefore            = pr_provi.siefore             
    LET lr_provision.folio              = 1                  
    LET lr_provision.consecutivo_lote   = pr_provi.consec    
    LET lr_provision.nss                = pr_provi.nss       
    LET lr_provision.curp               = pr_provi.curp      
    LET lr_provision.folio_sua          = NULL               
    LET lr_provision.fecha_pago         = HOY                
    LET lr_provision.fecha_valor        = pr_provi.fecha_proc
    LET lr_provision.fecha_conversion   = HOY                
    LET lr_provision.monto_en_pesos     = pr_provi.pesos * -1           
    LET lr_provision.monto_en_acciones  = pr_provi.acciones * -1             
    LET lr_provision.precio_accion      = ld_precio_acc      
    LET lr_provision.dias_cotizados     = 0         
    LET lr_provision.sucursal           = ""        
    LET lr_provision.id_aportante       = "RETIRO"
    LET lr_provision.estado             = 6         
    LET lr_provision.fecha_proceso      = HOY       
    LET lr_provision.usuario            = gs_usuario
    LET lr_provision.fecha_archivo      = HOY       
    LET lr_provision.etiqueta           = 1         

    INSERT INTO safre_tmp:tmp_provision
    VALUES (lr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_precios)

    DEFINE
        pdt_precios             DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100)

    DEFINE
        lc_mensaje              CHAR(100) ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_precios
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
# f_genera_tmp_cuenta : Genera una tabla temporal con los movimientos       #
#                       historicos de el nss dado                           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta (p_nss)

    DEFINE #loc #char
        p_nss                 CHAR(0011) ,
        sel_his               CHAR(2000) ,
        v_nombre_tabla        CHAR(0020)

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
                    " FROM ", v_nombre_tabla       ,
                    " WHERE nss = ","'",p_nss,"'"  ,
                    " AND tipo_movimiento NOT IN (888,999) ",
                    " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET sel_his = sel_his CLIPPED,
                  " SELECT * ",
                  "  FROM dis_cuenta ",
                  "  WHERE nss = ","'",p_nss,"'"  ,
                  "  AND tipo_movimiento NOT IN (888,999) ",
                  "  INTO TEMP tmp_dis_cuenta "

    PREPARE eje_sel_his FROM sel_his
    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta
    (folio            ,
     consecutivo_lote ,
     subcuenta        ,
     siefore
    )
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_reingresos : Elimina los reingresos que pudo tener el trabajador#
#---------------------------------------------------------------------------#
FUNCTION f_elimina_reingresos(pc_nss, ps_tipo_mov)

    DEFINE pc_nss LIKE ret_sol_issste_tx.nss

    DEFINE
        ls_cont_reingreso     ,
        ps_tipo_mov           SMALLINT

    DEFINE
        ldt_ult_reingreso     DATE

    LET ls_cont_reingreso = 0
        
    SELECT count(*)
    INTO   ls_cont_reingreso
    FROM   tmp_dis_cuenta
    WHERE  nss             = pc_nss
    AND    tipo_movimiento = ps_tipo_mov

    IF ls_cont_reingreso > 0 THEN
        SELECT MAX(fecha_conversion)
        INTO   ldt_ult_reingreso
        FROM   tmp_dis_cuenta
        WHERE  nss             = pc_nss
        AND    tipo_movimiento = ps_tipo_mov
                                     
        DELETE                   
        FROM   tmp_dis_cuenta    
        WHERE  nss              = pc_nss
        AND    fecha_conversion < ldt_ult_reingreso - 1 -- porque se provisionó el día anterior
    END IF                

END FUNCTION 

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_provision
        DROP TABLE tmp_monto_issste
        DROP TABLE tmp_monto_viv_issste
    WHENEVER ERROR STOP
    
    --------------------------------

    CREATE TABLE tmp_provision (
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

    CREATE INDEX tmp_provision_1 ON tmp_provision
        (folio,subcuenta,tipo_movimiento,estado)

    CREATE INDEX tmp_provision_2 ON tmp_provision
        (nss)

    CREATE INDEX tmp_provision_3 ON tmp_provision
        (folio,subcuenta)

    GRANT ALL ON tmp_provision TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_monto_issste (
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

    CREATE INDEX tmp_mto_isss_01 ON tmp_monto_issste
        (curp,consecutivo,cve_operacion)

    GRANT ALL ON tmp_monto_issste TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_monto_viv_issste
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

    CREATE INDEX tmp_viv_isss_01 ON tmp_monto_viv_issste
        (curp,consecutivo,cve_operacion)

    GRANT ALL ON tmp_monto_viv_issste TO PUBLIC

    --------------------------------

    DATABASE safre_af

END FUNCTION

