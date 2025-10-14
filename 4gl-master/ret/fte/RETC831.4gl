#################################################################################
# Sistema           => RET                                                      #
# Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
# Owner             => E.F.P.                                                   #
# Programa RETC831  => IDENTIFICACION DE APORTACIONES EXTEMPORANEAS PARA NSS    #
#                   => QUE TUVIERON UNA TRANSFERENCIA EXITOSA AL GOB.FEDERAL    #
# Elaborado por     => FRANCO ESTEBAN ULLOA VIDELA                              #
# Fecha Elaboracion.=> 20 DE ABRIL DEL 2004                                     #
# Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
# Fecha Actualiza   => 22 DE SEPTIEMBRE DEL 2008                                #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
# Fecha Actualiza   => 5 DE MARZO DE 2009                                       #
#                   => Correccion de errores:                                   #
#                      - Dejaba pasar aportes con periodo mayor al fip          #
#                      - Error en el cursor para retiros B                      #
#                      Se adapto para poder ejecutarse desde la ultima corrida  #
#                      terminada correctamente                                  #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
# Fecha Actualiza   => 2 DE MARZO DE 2011                                       #
#                   => Se rearregla el codigo del programa                      #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
# Fecha Actualiza   => 7 DE NOVIEMBRE DE 2012                                   #
#                   => Version para generar los registros de extemporaneas      #
#                      de pagos hechos por ventanilla 2.5                       #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        gdt_fecha_cont          ,
        gdt_fecha_ini           ,
        HOY                     DATE

    DEFINE
        enter                   CHAR(01)

    DEFINE #glo #integer
        gi_cont                 ,
        gi_dias                 ,
        gi_cont_tot_ac          ,
        gi_cont_ret_a           ,
        gi_cont_ret_b           ,
        gi_cont_ext_a           ,
        gi_cont_ext_b           ,
        gi_cont_ext_c           INTEGER
END GLOBALS

MAIN
    CALL STARTLOG("RETC831.log")
    CALL init()

    LET gdt_fecha_ini   = f_obtiene_fecha_ini()

-- Fecha ini sera la fecha de entrada en vigencia de Ventanilla 2.5
    LET gdt_fecha_ini   = MDY(07, 01, 2012)

    LET gdt_fecha_cont  = gdt_fecha_ini + 1
    LET gi_dias         = HOY - gdt_fecha_ini
    
    -- Barremos todos los dias que existen entre hoy y el ultimo proceso terminado correctamente
    FOR gi_cont = 1 TO gi_dias
        IF gdt_fecha_cont > HOY THEN
            EXIT FOR
        END IF

        -- Si el proceso es nuevo o no termino anteriormente, se permite la ejecucion
        IF f_insert_ret_ctr_extemp(gdt_fecha_cont) THEN
            CALL f_genera_datos_tmp(gdt_fecha_cont)
            CALL f_identifica_trans_AC(gdt_fecha_cont)
            CALL f_identifica_trans_B(gdt_fecha_cont)
            CALL f_act_estadistica(gdt_fecha_cont, gi_cont_tot_ac)
        END IF
        
        LET gdt_fecha_cont = gdt_fecha_cont + 1
    END FOR
    
    CALL ERRORLOG("PROCESO TERMINADO")
    CALL ERRORLOG("-----------------------------------------------------------")

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    ----- VARIABLES GLOBALES -----

--    LET HOY             = MDY(07, 02, 2012)
    LET HOY             = TODAY
    LET gi_cont_tot_ac  = 0
    LET gi_cont_ret_a   = 0
    LET gi_cont_ret_b   = 0
    LET gi_cont_ext_c   = 0
           
    ----- ESTADOS DE SOLICITUD -----
    SELECT estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado
    WHERE  descripcion = "CAPTURADO"

    SELECT estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado
    WHERE  descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_identifica_trans_AC : Busca NSS que recibieron aportaciones posteriores #
#                         a una transferencia de IV-RT o Pension            #
#                         GarantizadaTransferencias realizadas al IMSS      #
#                         (Tipos A y C)                                     #
#---------------------------------------------------------------------------#
FUNCTION f_identifica_trans_AC(pdt_fecha_conv)
    
    DEFINE
        pdt_fecha_conv          DATE

    DEFINE lr_aporte_AC RECORD
        nss                     CHAR(11)      ,
        folio                   INTEGER       ,
        consecutivo_lote        INTEGER       ,
        fecha_conversion        DATE
    END RECORD

    DEFINE lr_transf_AC RECORD
        tipo_retiro             LIKE ret_trans_imss.tipo_retiro     ,
        consecutivo             LIKE ret_trans_imss.consecutivo     ,
        fecha_ini_pen           LIKE ret_trans_imss.fecha_ini_pen   ,
        sec_pension             LIKE ret_trans_imss.sec_pension
    END RECORD

    DEFINE lr_importes_AC RECORD
        rcv                     DECIMAL(16,6) ,
        viv97                   DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_periodo_aporte       CHAR(06),
        lc_periodo_fip          CHAR(06)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_aporte_AC.*       TO NULL
    INITIALIZE lr_importes_AC.*     TO NULL
    INITIALIZE lr_transf_AC.*       TO NULL

    --Identifica nss con movimientos de aporte al dia de hoy
    DECLARE cur_mov_AC CURSOR FOR
    SELECT *
    FROM   tmp_dis_cta_extemp
    ORDER BY 1,2

    FOREACH cur_mov_AC INTO lr_aporte_AC.*

        -- Incrementa contador de nss con aportes procesados
        LET gi_cont_tot_ac = gi_cont_tot_ac + 1

        -- Identifica si el nss se encuentra en un proceso de traspaso
        SELECT "OK"
        FROM   dis_cuenta
        WHERE  nss              = lr_aporte_AC.nss
        AND    subcuenta       IN (1,2,4,5,6,9)
        AND    tipo_movimiento IN (220,290,247,240,295)
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            -- La cuenta no esta en un proceso de traspaso, obtiene informacion 
            -- de solicitudes de transferencia

            DECLARE cur_transf_AC CURSOR FOR
            SELECT A.tipo_retiro    ,
                   A.consecutivo    ,
                   A.fecha_ini_pen  ,
                   A.sec_pension
            FROM   ret_trans_imss A     , 
                   ret_det_datamart B
            WHERE  A.nss                    = lr_aporte_AC.nss
            AND    A.nss                    = B.nss
            AND    A.consecutivo            = B.id_registro
            AND    A.tipo_retiro            <> "B"
--            AND    B.ind_env_recep_trans    IN (501,507)
            ORDER BY 3 DESC, 2 DESC

            FOREACH cur_transf_AC INTO lr_transf_AC.*

                -- Se recupera el periodo de pago de la FIP
                LET lc_periodo_fip = f_obten_periodo_pago(lr_transf_AC.fecha_ini_pen)
                
                INITIALIZE lc_periodo_aporte TO NULL
                
                -- Recupera el periodo de pago del aporte
                SELECT periodo_pago
                INTO   lc_periodo_aporte
                FROM   dis_det_aporte
                WHERE  folio           = lr_aporte_AC.folio
                AND    n_seguro        = lr_aporte_AC.nss
                AND    consec_reg_lote = lr_aporte_AC.consecutivo_lote

                IF (STATUS <> NOTFOUND) AND (lc_periodo_aporte IS NOT NULL) THEN
                    IF lc_periodo_aporte > lc_periodo_fip THEN

                        CALL f_obtiene_montos(lr_aporte_AC.nss              ,
                                              lr_aporte_AC.consecutivo_lote ,
                                              lr_transf_AC.tipo_retiro      ,
                                              pdt_fecha_conv
                                             )
                        RETURNING lr_importes_AC.*
                        
                        CALL f_inserta_extemp(lr_transf_AC.consecutivo  ,
                                              lr_transf_AC.sec_pension  ,
                                              lr_aporte_AC.*            ,
                                              lr_importes_AC.*          ,
                                              lc_periodo_aporte         ,
                                              lr_transf_AC.tipo_retiro
                                             )
                        RETURNING gi_cont_ext_a ,
                                  gi_cont_ext_b ,
                                  gi_cont_ext_c
                    END IF
                ELSE
                    -- Si no tiene periodo de pago y es retiro C se guarda como aportacion especial
                    -- Para los casos de Retiros A y B queda pendiente.
                    IF lr_transf_AC.tipo_retiro = "C"  THEN

                        CALL f_inserta_especial(lr_aporte_AC.nss                ,
                                                lr_aporte_AC.fecha_conversion   ,
                                                lr_aporte_AC.consecutivo_lote   ,
                                                lr_transf_AC.consecutivo        ,
                                                lr_transf_AC.sec_pension
                                               )
                    END IF -- Tipo retiro C
                END IF -- Aporte con periodo de pago
            END FOREACH
        END IF -- NSS en proceso de traspasos
    END FOREACH

    UPDATE ret_ctr_extemp
    SET    tot_ret_tipo_a   = gi_cont_ret_a  ,
           tot_extemp_a     = gi_cont_ext_a  ,
           tot_extemp_c     = gi_cont_ext_c
    WHERE  fecha_proceso    = pdt_fecha_conv

END FUNCTION

#---------------------------------------------------------------------------#
# f_identifica_trans_B : Busca NSS que recibieron aportaciones posteriores  #
#                        a una transferencia de de Devolucion al Gobierno   #
#                        Federal por Ley 73 (Tipos B)                       #
#---------------------------------------------------------------------------#
FUNCTION f_identifica_trans_B(pdt_fecha_conv)

    DEFINE
        pdt_fecha_conv         DATE

    DEFINE lr_aporte_B RECORD
        nss                     CHAR(11)      ,
        folio                   INTEGER       ,
        consecutivo_lote        INTEGER       ,
        fecha_conversion        DATE
    END RECORD

    DEFINE lr_transf_B RECORD
        tipo_retiro             LIKE ret_trans_imss.tipo_retiro     ,
        consecutivo             LIKE ret_trans_imss.consecutivo     ,
        fecha_ini_pen           LIKE ret_trans_imss.fecha_ini_pen   ,
        sec_pension             LIKE ret_trans_imss.sec_pension
    END RECORD

    DEFINE lr_importes_B RECORD
        rcv                     DECIMAL(16,6) ,
        viv97                   DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_sec_pension          CHAR(02),
        lc_periodo_aporte       CHAR(06),
        lc_periodo_fip          CHAR(06)

    -- -----------------------------------------------------------------------------

    INITIALIZE lc_sec_pension   TO NULL
    INITIALIZE lr_importes_B.*  TO NULL
    INITIALIZE lr_aporte_B.*    TO NULL
    INITIALIZE lr_transf_B      TO NULL

    -- Identifica nss con movimientos de aporte al dia de hoy
    DECLARE cur_mov_B CURSOR FOR
    SELECT *
    FROM   tmp_dis_cta_extemp
    ORDER BY 1,2

    FOREACH cur_mov_B INTO lr_aporte_B.*

        -- Incrementa contador de nss con aportes procesados
        LET gi_cont_tot_ac = gi_cont_tot_ac + 1

        -- Identifica si el nss se encuentra en un proceso de traspaso
        SELECT "OK"
        FROM   dis_cuenta
        WHERE  nss              = lr_aporte_B.nss
        AND    subcuenta       IN (1,2,4,5,6,9)
        AND    tipo_movimiento IN (220,290,247,240,295)
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            -- La cuenta no esta en un proceso de traspaso, obtiene informacion 
            -- de solicitudes de transferencia

            DECLARE cur_transf_B CURSOR FOR
            SELECT A.tipo_retiro    ,
                   A.consecutivo    ,
                   A.fecha_ini_pen  ,
                   A.sec_pension
            FROM   ret_trans_imss A     , 
                   ret_det_datamart B
            WHERE  A.nss                    = lr_aporte_B.nss
            AND    A.nss                    = B.nss
            AND    A.consecutivo            = B.id_registro
            AND    A.tipo_retiro            = "B"
--            AND    B.ind_env_recep_trans    IN (501,507)
            ORDER BY 3 DESC, 2 DESC


            FOREACH cur_transf_B INTO lr_transf_B.*
                
                -- Se recupera el periodo de pago de la fip
                LET lc_periodo_fip = f_obten_periodo_pago(lr_transf_B.fecha_ini_pen)

                -- Recupera el periodo de pago del aporte
                INITIALIZE lc_periodo_aporte TO NULL

                SELECT periodo_pago
                INTO   lc_periodo_aporte
                FROM   dis_det_aporte
                WHERE  folio           = lr_aporte_B.folio
                AND    n_seguro        = lr_aporte_B.nss
                AND    consec_reg_lote = lr_aporte_B.consecutivo_lote

                IF (STATUS <> NOTFOUND) AND (lc_periodo_aporte IS NOT NULL) THEN
                    
                    IF lc_periodo_aporte > lc_periodo_fip THEN
                        CALL f_obtiene_montos(lr_aporte_B.nss               ,
                                              lr_aporte_B.consecutivo_lote  ,
                                              lr_transf_B.tipo_retiro       ,
                                              pdt_fecha_conv
                                             )
                        RETURNING lr_importes_B.*

                        CALL f_inserta_extemp(lr_transf_B.consecutivo   ,
                                              lr_transf_B.sec_pension   ,
                                              lr_aporte_B.*             ,
                                              lr_importes_B.*           ,
                                              lc_periodo_aporte         ,
                                              lr_transf_B.tipo_retiro
                                             )
                        RETURNING gi_cont_ext_a ,
                                  gi_cont_ext_b ,
                                  gi_cont_ext_c
                    END IF -- Periodo Aporte menor al periodo fip
                END IF -- Encontro aporte
            END FOREACH -- siguiente movimiento
        END IF
    END FOREACH

    UPDATE ret_ctr_extemp
    SET    tot_ret_tipo_b   = gi_cont_ret_b  ,
           tot_extemp_b     = gi_cont_ext_b
    WHERE  fecha_proceso    = pdt_fecha_conv

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_estadistica : Actualiza las estadisticas de procesamiento           #
#---------------------------------------------------------------------------#
FUNCTION f_act_estadistica(pdt_fecha, pi_tot_procesados)

    DEFINE
        pdt_fecha               DATE

    DEFINE
        pi_tot_procesados       INTEGER

    -- -----------------------------------------------------------------------------

    -- Se marca id_fin_proceso con S para indicar proceso terminado
    UPDATE ret_ctr_extemp
    SET    tot_procesados   = pi_tot_procesados ,
           id_fin_proceso   = "S"
    WHERE  fecha_proceso    = pdt_fecha

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_fecha_ini : Obtiene la ultima fecha de ejecucion del proceso    #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_fecha_ini()

    DEFINE
        ld_fecha_ini    DATE

    -- -----------------------------------------------------------------------------

    SELECT MAX(fecha_proceso)
    INTO   ld_fecha_ini
    FROM   ret_ctr_extemp

    IF (ld_fecha_ini IS NULL) OR (ld_fecha_ini > HOY) THEN
        LET ld_fecha_ini = HOY
    END IF
    
    RETURN ld_fecha_ini

END FUNCTION

#---------------------------------------------------------------------------#
# f_insert_ret_ctr_extemp : Determina si se insertara un nuevo registro de  #
#                           proceso o se actualizara otro que no termino    #
#                           correctamente.                                  #
#---------------------------------------------------------------------------#
FUNCTION f_insert_ret_ctr_extemp(pdt_fec_proceso)

    DEFINE
        pdt_fec_proceso     DATE

    DEFINE lc_id_fin LIKE ret_ctr_extemp.id_fin_proceso
    
    DEFINE
        lc_msg              CHAR(100)

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    -- Si ls_flag = 1 el proceso se ejecuta para esa fecha
    LET ls_flag = 1                   

    LET gi_cont_tot_ac = 0
    LET gi_cont_ext_a  = 0
    LET gi_cont_ext_b  = 0
    LET gi_cont_ext_c  = 0    
        
    -- Solo se procesaran los registros de Lunes a Viernes
    IF WEEKDAY(pdt_fec_proceso) <= 5 AND WEEKDAY(pdt_fec_proceso) >= 1 THEN
        SELECT id_fin_proceso
        INTO   lc_id_fin
        FROM   ret_ctr_extemp
        WHERE  fecha_proceso = pdt_fec_proceso
        
        IF STATUS = NOTFOUND THEN
            -- Si no existe el proceso, se da de alta en ret_ctr_extemp
            INSERT INTO ret_ctr_extemp
                VALUES(pdt_fec_proceso  ,   -- fecha_proceso
                       0                ,   -- tot_procesados
                       0                ,   -- tot_ret_tipo_a
                       0                ,   -- tot_ret_tipo_b
                       0                ,   -- tot_extemp_a
                       0                ,   -- tot_extemp_b
                       0                ,   -- tot_extemp_c
                       "N"                  -- id_fin_proceso N Inidica proceso no terminado
                      )
        ELSE
            -- El proceso ya fue ejecutado y termino correctamente, por lo que no
            -- debe ejecutarse el proceso para esta fecha
            IF lc_id_fin = "S" THEN
                LET lc_msg = "El proceso con fecha ", pdt_fec_proceso, " ya habia sido previamente ejecutado" CLIPPED
                CALL ERRORLOG(lc_msg)
                LET ls_flag = 0
            ELSE
                -- El proceso fue ejecutado pero no termino correctamente, por lo que reiniciamos
                -- los contadores para esa fecha y se ejecuta el proceso
                CALL f_inicializa_contadores(pdt_fec_proceso)
                    RETURNING gi_cont_ext_a,
                              gi_cont_ext_b,
                              gi_cont_ext_c
                
                LET lc_msg = "El proceso con fecha ", pdt_fec_proceso, " no concluyo correctamente y se actualizara" CLIPPED
                CALL ERRORLOG(lc_msg)
            END IF -- lc_id_fin = "S"
        END IF -- Existe el proceso
    ELSE
        -- El dia actual es sabado o domingo
        LET ls_flag = 0
    END IF
       
    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_inicializa_contadores : Obtiene los valores de los contadores en la     #
#                           tabla ret_ctr_extemp a la fecha dada            #
#---------------------------------------------------------------------------#
FUNCTION f_inicializa_contadores(pdt_fec_eje)

    DEFINE
        pdt_fec_eje        DATE

    DEFINE lr_contador RECORD
        extemp_a        SMALLINT,
        extemp_b        SMALLINT,
        extemp_c        SMALLINT
    END RECORD

    -- -----------------------------------------------------------------------------
    
    INITIALIZE lr_contador.* TO NULL

    SELECT tot_extemp_a,
           tot_extemp_b,
           tot_extemp_c
    INTO   lr_contador.*
    FROM   ret_ctr_extemp
    WHERE  fecha_proceso = pdt_fec_eje   

    RETURN lr_contador.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_periodo_pago : Recupera el periodo de pago para la FIP dada       #
#---------------------------------------------------------------------------#
FUNCTION f_obten_periodo_pago(pdt_FIP)

    DEFINE
        pdt_FIP             DATE

    DEFINE
        lc_per_pago         CHAR(06)

    DEFINE
        ls_residuo          ,
        ls_mes              ,
        ls_ano              SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_mes      = MONTH(pdt_FIP)
    LET ls_ano      = YEAR (pdt_FIP)
    LET ls_residuo  = ls_mes MOD 2
    
    IF ls_residuo > 0 THEN
        LET ls_mes = ls_mes + 1
    END IF
    
    LET lc_per_pago = ls_ano USING "&&&&", ls_mes USING "&&"
    
    RETURN lc_per_pago

END FUNCTION


#---------------------------------------------------------------------------#
# f_genera_datos_tmp : Genera la tabla temporal con los datos a buscar en   #
#                      dis_cuenta de los registros generados                #
#---------------------------------------------------------------------------#
FUNCTION f_genera_datos_tmp(pdt_fecha_conv)
    
    DEFINE
        pdt_fecha_conv          DATE

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cta_extemp
    WHENEVER ERROR STOP

    IF YEAR(pdt_fecha_conv) = 2012 THEN
        SELECT A.nss                ,
               A.folio              ,   -- folio del aporte
               A.consecutivo_lote   ,   -- consecutivo del aporte
               A.fecha_conversion
        FROM   dis_cuenta12 A         , 
               tab_movimiento B
        WHERE  A.fecha_conversion   = pdt_fecha_conv
        AND    A.subcuenta          IN (1,2,4,5,6,9)
        AND    A.tipo_movimiento    = B.codigo
        AND    B.tipo               = 1
        AND    B.codigo             NOT IN (999)
        GROUP BY 1,2,3,4
        ORDER BY 1,2
        INTO TEMP tmp_dis_cta_extemp
    ELSE
        SELECT A.nss                ,
               A.folio              ,   -- folio del aporte
               A.consecutivo_lote   ,   -- consecutivo del aporte
               A.fecha_conversion
        FROM   dis_cuenta A         , 
               tab_movimiento B
        WHERE  A.fecha_conversion   = pdt_fecha_conv
        AND    A.subcuenta          IN (1,2,4,5,6,9)
        AND    A.tipo_movimiento    = B.codigo
        AND    B.tipo               = 1
        AND    B.codigo             NOT IN (999)
        GROUP BY 1,2,3,4
        ORDER BY 1,2
        INTO TEMP tmp_dis_cta_extemp
    END IF 
    
    CREATE INDEX tmp_dis_cta_extemp_01
    ON tmp_dis_cta_extemp(nss, folio)
    
    UPDATE STATISTICS FOR TABLE tmp_dis_cta_extemp

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_montos : Obtiene los montos correspondientes al nss y           #
#                    consecutivo dados en la cuenta individual              #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_montos(pr_transf, pdt_fecha_conv)

    DEFINE pr_transf RECORD
        nss                     LIKE dis_cuenta.nss              ,
        consecutivo_lote        LIKE dis_cuenta.consecutivo_lote ,
        tipo_retiro             LIKE ret_trans_imss.tipo_retiro
    END RECORD

    DEFINE
        pdt_fecha_conv         DATE

    DEFINE lr_imp_transf RECORD
        rcv         LIKE dis_cuenta.monto_en_acciones   ,
        viv         LIKE dis_cuenta.monto_en_acciones
    END RECORD

    -- -----------------------------------------------------------------------------

    IF pr_transf.tipo_retiro = "B" THEN
        SELECT SUM(A.monto_en_acciones)
        INTO   lr_imp_transf.rcv
        FROM   dis_cuenta A
        WHERE  A.subcuenta       IN (2,5,6,9)
        AND    A.nss              = pr_transf.nss
        AND    A.consecutivo_lote = pr_transf.consecutivo_lote
        AND    A.fecha_conversion = pdt_fecha_conv
    ELSE
        SELECT SUM(A.monto_en_acciones)
        INTO   lr_imp_transf.rcv
        FROM   dis_cuenta A
        WHERE  A.subcuenta       IN (1,2,5,6,9)
        AND    A.nss              = pr_transf.nss
        AND    A.consecutivo_lote = pr_transf.consecutivo_lote
        AND    A.fecha_conversion = pdt_fecha_conv
    END IF

    SELECT SUM(C.monto_en_pesos)
    INTO   lr_imp_transf.viv
    FROM   dis_cuenta C
    WHERE  C.subcuenta        = 4
    AND    C.nss              = pr_transf.nss
    AND    C.consecutivo_lote = pr_transf.consecutivo_lote
    AND    C.fecha_conversion = pdt_fecha_conv

    RETURN lr_imp_transf.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_extemp : Inserta el registro del detalle del aporte y de la     #
#                    aportacion extemporanea en las tablas correspondientes #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_extemp(lr_insert_extemp)

    DEFINE lr_insert_extemp RECORD
        consec_retiro               LIKE ret_trans_imss.consecutivo ,
        sec_pension                 LIKE ret_trans_imss.sec_pension ,
        nss                         CHAR(11)                        ,
        folio_aporte                INTEGER                         ,
        consec_lote_aporte          INTEGER                         ,
        fec_conversion_aporte       DATE                            ,
        importe_rcv                 DECIMAL(16,6)                   ,
        importe_viv97               DECIMAL(16,6)                   ,
        periodo_pago_aporte         CHAR(06)                        ,
        tipo_retiro                 LIKE ret_trans_imss.tipo_retiro
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT "OK"
    FROM   ret_aporte
    WHERE  nss              = lr_insert_extemp.nss
    AND    consecutivo_lote = lr_insert_extemp.consec_lote_aporte
    AND    folio            = lr_insert_extemp.folio_aporte
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        INSERT INTO ret_aporte
        VALUES(lr_insert_extemp.nss                     ,-- nss
               lr_insert_extemp.consec_lote_aporte      ,-- consecutivo_lote
               lr_insert_extemp.folio_aporte            ,-- folio
               lr_insert_extemp.periodo_pago_aporte     ,-- periodo_pago
               lr_insert_extemp.fec_conversion_aporte   ,-- fecha_conversion
               lr_insert_extemp.importe_rcv             ,-- importe_rcv
               lr_insert_extemp.importe_viv97           ,-- importe_viv97
               1                                        ,-- id_tramite
               ""                                       ,-- f_cambio_tramite
               1                                        ,-- id_aporte
               0                                        ,-- consec_tramite
               ""                                       ,-- usuario_deshabilit
               ""                                       ,-- fecha_deshabilita
               ""                                       ,-- usuario_habilita
               ""                                        -- fecha_habilita
              )
    END IF

    SELECT "OK"
    FROM   ret_extemporanea
    WHERE  nss              = lr_insert_extemp.nss
    AND    estado_registro  IN (gr_edo.capturado, 
                                gr_edo.enviado  )
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        INSERT INTO ret_extemporanea
        VALUES(lr_insert_extemp.nss                     , -- nss
               lr_insert_extemp.sec_pension             , -- sec_pension
               lr_insert_extemp.consec_lote_aporte      , -- consecutivo_lote (del aporte)
               0                                        , -- consecutivo
               lr_insert_extemp.consec_retiro           , -- consecutivo_his
               lr_insert_extemp.periodo_pago_aporte     , -- periodo_pago
               1                                        , -- id_tramite
               0                                        , -- consec_tramite
               lr_insert_extemp.fec_conversion_aporte   , -- fecha_conversion
               gr_edo.capturado                         , -- estado_registro
               NULL                                     , -- fecha_envio
               NULL                                       -- diag_envio_procesar
              )

        CASE lr_insert_extemp.tipo_retiro 
            WHEN "A"
                LET gi_cont_ext_a = gi_cont_ext_a + 1    
            WHEN "B"
                LET gi_cont_ext_b = gi_cont_ext_b + 1    
            WHEN "C"
                LET gi_cont_ext_c = gi_cont_ext_c + 1
        END CASE 
    END IF

    RETURN gi_cont_ext_a ,
           gi_cont_ext_b ,
           gi_cont_ext_c

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_especial : Inserta el registro de la aportación especial en la  #
#                      tabla correspondiente                                #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_especial(lr_insert_esp)

    DEFINE lr_insert_esp RECORD
        nss                     CHAR(11)    ,
        fecha_conversion        DATE        ,
        consecutivo_lote        INTEGER     ,
        consecutivo             INTEGER     ,
        sec_pension             CHAR(02)
    END RECORD
    

    -- -----------------------------------------------------------------------------
    
    SELECT "OK"
    FROM   ret_especial
    WHERE  nss              = lr_insert_esp.nss
    AND    estado_registro  IN (gr_edo.capturado, 
                                gr_edo.enviado  )
    GROUP BY 1
    
    IF STATUS = NOTFOUND THEN
       INSERT INTO ret_especial
           VALUES(lr_insert_esp.nss                 ,   -- nss
                  lr_insert_esp.fecha_conversion    ,   -- fecha_conversion
                  lr_insert_esp.consecutivo_lote    ,   -- consecutivo_lote (del aporte)
                  lr_insert_esp.consecutivo         ,   -- consecutivo (de la solicitud)
                  0                                 ,   -- consec_tramite
                  0                                 ,   -- folio
                  lr_insert_esp.sec_pension         ,   -- sec_pension
                  gr_edo.capturado                  ,   -- estado_registro
                  NULL                                  -- fecha_envio
                 )
    END IF

END FUNCTION
