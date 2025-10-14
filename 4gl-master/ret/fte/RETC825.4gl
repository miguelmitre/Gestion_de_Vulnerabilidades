#################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                 #
#Owner             => E.F.P.                                                    #
#Programa RETC825  => GENERA SOLICITUD DE VERIFICACION DE RESOLUCION IMSS       #
#                     - OPERACION 12 RETIROS PARCIALES IMSS                     #
#Fecha creacion    => 25 DE ENERO DEL 2004                                      #
#By                => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha actualiz.   => 19 DE NOVIEMBRE DEL 2004                                  #
#Actualizacion     => VERONICA LOPEZ SANCHEZ                                    #
#Fecha actualiz.   => 18 DE MAYO DE 2009                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Modificaciones para la adaptacion de la circular 31-11.   #
#                  => Se agrego el calculo de mensualidades para los tipos de   #
#                  => retiro A y B en la tabla ret_ctr_pago_det                 #
#Fecha actualiz.   => 16 DE JUNIO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrego el tipo desempleo C a la generacion de la op    #
#Fecha actualiz.   => 20 DE SEPTIEMBRE DE 2012                                  #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => - Optimizacion del codigo                                 #
#                     - Se cambio el proceso para almacenar primero en tablas   #
#                       temporales y luego consolidar a tablas fisicas          #
#                     - Se agregaron funciones de las librearias de retiros     #
#Sistema           => RET                                                       #
#################################################################################
#Requerimiento     => EFPS-266  17-Mzo-2014   Alejandro Chagoya Salazar         #
#Descripcion       => Se agregan los nuevos campos del contrato de la Nueva     #
#                  => Plataforma. CPL-1602, version especial para COPPEL        #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_estado RECORD #gr_estado
        capturado               LIKE ret_estado.estado_solicitud    ,
        confirmado              LIKE ret_estado.estado_solicitud    ,
        procesado               LIKE ret_estado.estado_solicitud    ,
        provisionado            LIKE ret_estado.estado_solicitud    ,
        enviado                 LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE
        HOY                     DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_usuario              CHAR(012) ,
        gc_comando              CHAR(150)
    DEFINE
        gs_num_siefores         , -- Indica el numero de siefores que se usan actualmente
        gs_cod_inv              ,
        gs_codigo_afore         SMALLINT

    DEFINE mr_envio   RECORD
           folioCliente        CHAR(50),
           nss                 CHAR(11),
           nombre,
           apellidoPaterno,
           apellidoMaterno     CHAR(40),
           tipo_Retiro         CHAR(1),
           tipo_Prestacion     CHAR(2),
           num_Resolucion      CHAR(6)
        END RECORD

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC825")

    CALL init()
    CALL f_abre_ventana()

    IF f_valida_ejecucion() THEN
        CALL f_tablas_temp()
        CALL primer_paso()      -- Almacena en ret_monto_siefore
        CALL segundo_paso()     -- Genera registros de pago de mensualidades
        CALL tercer_paso()      -- Genera operacion 12
        CALL cuarto_paso()      -- Consolida las tablas temporales en las tablas fisicas
    END IF

    CLOSE WINDOW retc825
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY         = TODAY
    LET gc_usuario  = f_lib_obten_user()

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_cod_inv
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*INVERCAP*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_estado.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore < 11

    ----- HABIL SIGUIENTE -----
    LET gc_comando = " EXECUTE FUNCTION fn_habil_siguiente ( ?,? )"
    PREPARE eje_dia_sig FROM gc_comando

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Obtiene el saldo al dia de las subcuentas a pagar e inserta #
#               lo obtenido en ret_monto_siefore                            #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_parcial RECORD LIKE ret_parcial.*

    DEFINE lar_siefore ARRAY[20] OF RECORD
        activo                  SMALLINT        ,
        acciones_ret97          DECIMAL(16,6)   ,
        acciones_cv             DECIMAL(16,6)   ,
        acciones_cs             DECIMAL(16,6)   ,
        acciones_est            DECIMAL(16,6)   ,
        acciones_esp            DECIMAL(16,6)
    END RECORD

    DEFINE lr_datos RECORD
        nss             LIKE ret_parcial.nss    ,
        subcta          SMALLINT                ,
        grupo           SMALLINT                ,
        fecha           DATE
    END RECORD

    DEFINE lr_saldo RECORD
        subcuenta       SMALLINT        ,
        siefore         SMALLINT        ,
        monto_acc       DECIMAL(16,6)   ,
        monto_pes       DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_eje_saldo                CHAR(150)

    DEFINE
        ls_siefore                  SMALLINT

    DEFINE
        li_num_regs                 INTEGER

    DEFINE
        ld_acciones_cv              DECIMAL(16,6)

    -- -----------------------------------------------------------------------------

    DISPLAY " RECUPERANDO SALDO AL DIA DE LOS REGISTROS ...              " AT 19,1 ATTRIBUTE(REVERSE)

    LET li_num_regs = 0

    DISPLAY "TOTAL REGISTROS A ENVIAR     : " AT 11,4
    DISPLAY li_num_regs AT 11, 35
    
    LET lr_datos.subcta = 0
    LET lr_datos.grupo  = 0
    LET lr_datos.fecha  = HOY

    DECLARE cur_confirm CURSOR FOR
    SELECT  A.*
    FROM    ret_parcial A
    WHERE   A.estado_solicitud = gr_estado.confirmado
    AND    ((A.rechazo_cod = 0) OR (A.rechazo_cod IS NULL))

    FOREACH cur_confirm INTO lr_parcial.*

        INITIALIZE lr_saldo.* TO NULL
        LET lr_datos.nss    = lr_parcial.nss

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_siefore[ls_siefore].activo             = FALSE
            LET lar_siefore[ls_siefore].acciones_ret97     = 0
            LET lar_siefore[ls_siefore].acciones_cv        = 0
            LET lar_siefore[ls_siefore].acciones_cs        = 0
            LET lar_siefore[ls_siefore].acciones_est       = 0
            LET lar_siefore[ls_siefore].acciones_esp       = 0
        END FOR

        -- Obtiene el saldo al dia de cada subcuenta relacionada
        LET lc_eje_saldo = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "

        PREPARE prp_saldos FROM lc_eje_saldo
        DECLARE cur_saldos CURSOR FOR prp_saldos

        FOREACH cur_saldos USING lr_datos.*
                           INTO  lr_saldo.*

            IF lr_saldo.subcuenta = 1 OR lr_saldo.subcuenta = 2 OR lr_saldo.subcuenta = 5 OR
               lr_saldo.subcuenta = 6 OR lr_saldo.subcuenta = 9 THEN

                IF lr_saldo.monto_acc IS NULL OR lr_saldo.monto_acc < 0 THEN
                    LET lr_saldo.monto_acc = 0
                END IF

                IF lr_saldo.monto_pes IS NULL OR lr_saldo.monto_pes < 0 THEN
                    LET lr_saldo.monto_pes = 0
                END IF

                IF lr_saldo.siefore <> 11 THEN

                    LET ls_siefore  = lr_saldo.siefore

                    -- Marcamos como activo el registro de la siefore actual
                    LET lar_siefore[ls_siefore].activo = TRUE
                    CASE lr_saldo.subcuenta
                        WHEN 1
                            LET lar_siefore[ls_siefore].acciones_ret97 = lr_saldo.monto_acc
                        WHEN 2
                            LET lar_siefore[ls_siefore].acciones_cv    = lr_saldo.monto_acc
                        WHEN 5
                            LET lar_siefore[ls_siefore].acciones_cs    = lr_saldo.monto_acc
                        WHEN 6
                            LET lar_siefore[ls_siefore].acciones_est   = lr_saldo.monto_acc
                        WHEN 9
                            LET lar_siefore[ls_siefore].acciones_esp   = lr_saldo.monto_acc
                    END CASE
                END IF

            END IF -- Subcuentas
        END FOREACH -- Saldo

        FOR ls_siefore = 1 TO gs_num_siefores
            LET ld_acciones_cv = 0

            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_siefore[ls_siefore].activo = TRUE THEN

                LET ld_acciones_cv = lar_siefore[ls_siefore].acciones_cv  +
                                     lar_siefore[ls_siefore].acciones_est  +
                                     lar_siefore[ls_siefore].acciones_esp

                INSERT INTO tmp_monto_siefore
                    VALUES(lr_parcial.nss                           ,   -- nss
                           lr_parcial.consecutivo                   ,   -- consecutivo
                           1                                        ,   -- folio
                           "I"                                      ,   -- tipo_retiro
                           12                                       ,   -- tipo_operacion
                           ls_siefore                               ,   -- siefore
                           lar_siefore[ls_siefore].acciones_ret97   ,   -- acciones_ret97
                           ld_acciones_cv                           ,   -- acciones_cv
                           lar_siefore[ls_siefore].acciones_cs      ,   -- acciones_cs
                           0                                            -- acciones_ret92
                          )
               LET li_num_regs = li_num_regs + 1
               DISPLAY li_num_regs AT 11, 35
            END IF
        END FOR



    END FOREACH -- Siguiente nss

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Calcula los pagos a realizarse en las cuentas con tipos    #
#                de desempleo A, B y Complementarios                        #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_pago_det  RECORD LIKE ret_ctr_pago_det.*

    DEFINE lr_pago RECORD
        nss                 LIKE ret_ctr_pago.nss           ,
        consecutivo         LIKE ret_ctr_pago.consecutivo   ,
        mto_pago            LIKE ret_ctr_pago.mto_pago      ,
        mensualidades       LIKE ret_ctr_pago.mensualidades ,
        tipo_desempleo      LIKE ret_parcial.tipo_desempleo ,
        tipo_pago           LIKE ret_parcial.tipo_pago      ,
        num_resolucion      LIKE ret_parcial.num_resolucion ,
        salario_base_cot    LIKE ret_parcial.salario_base_cot
    END RECORD

    DEFINE lr_dia_sig RECORD
        fecha       DATE,
        numero      SMALLINT
    END RECORD

    DEFINE ld_salario_base LIKE ret_parcial_resol.salario_base_a
    DEFINE ld_primer_pago  LIKE ret_parcial_resol.salario_base_a

    DEFINE
        li_cont                 SMALLINT

    DEFINE
        ldt_fecha_ini           DATE

    -- -----------------------------------------------------------------------------

    DISPLAY " CALCULANDO PAGOS PARA PARCIALES CON DESEMPLEO ...          " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1
    
    LET lr_dia_sig.numero = 0

    DECLARE cur_pago CURSOR FOR
    SELECT A.nss                ,
           A.consecutivo        ,
           A.mto_pago           ,
           A.mensualidades      ,
           B.tipo_desempleo     ,
           B.tipo_pago          ,
           B.num_resolucion     ,
           B.salario_base_cot
    FROM   ret_ctr_pago A       ,
           ret_parcial  B       ,
           tmp_monto_siefore C
    WHERE  A.nss              = B.nss
    AND    A.consecutivo      = B.consecutivo
    AND    A.nss              = C.nss
    AND    A.consecutivo      = C.consecutivo
    AND    B.tipo_prestacion  = 6
    AND    B.tipo_desempleo   IN ("A","B","C")
    AND    A.estado           = gr_estado.capturado
    AND    B.estado_solicitud = gr_estado.confirmado

    FOREACH cur_pago INTO lr_pago.*
        LET ld_primer_pago              = lr_pago.salario_base_cot * 30
        LET lr_pago_det.nss             = lr_pago.nss
        LET lr_pago_det.consecutivo     = lr_pago.consecutivo
        LET lr_pago_det.estado          = gr_estado.provisionado
        LET lr_pago_det.folio_op16      = NULL
        LET lr_dia_sig.fecha            = HOY + 1

        -- El dia del primer pago es el dia habil siguiente a la fecha en curso
        EXECUTE eje_dia_sig USING lr_dia_sig.*
                            INTO  ldt_fecha_ini

        -- Realizamos el calculo de mensualidades
        FOR li_cont = 1 TO lr_pago.mensualidades

            LET lr_pago_det.num_mensualidad = li_cont

            IF li_cont = 1 THEN
                LET lr_pago_det.fecha_liquida   = ldt_fecha_ini

                -- Si solo se paga una mensualidad, entonces el primer pago es el monto total,
                -- en otro caso es el monto calculado como limite de la primera mensualidad
                IF lr_pago.mensualidades = 1 THEN
                    LET lr_pago_det.mto_mensualidad = lr_pago.mto_pago
                ELSE
                    LET lr_pago_det.mto_mensualidad = ld_primer_pago
                END IF
            ELSE
                -- Verificamos si la fecha a pagarse de la mensualidad es habil o no.
                -- En caso de no ser habil devuelve la fecha del habil siguiente
                EXECUTE eje_dia_sig USING lr_dia_sig.*
                                    INTO  lr_pago_det.fecha_liquida

                IF lr_pago.mensualidades = 2 THEN
                    LET lr_pago_det.mto_mensualidad = lr_pago.mto_pago - ld_primer_pago
                ELSE
                    LET lr_pago_det.mto_mensualidad = (lr_pago.mto_pago - ld_primer_pago)/5
                END IF
            END IF

            INSERT INTO tmp_ctr_pago_det 
            VALUES (lr_pago_det.*)

            -- Actualizamos la fecha de pago al siguiente mes
            LET lr_dia_sig.fecha = f_lib_suma_mes(ldt_fecha_ini, li_cont)

        END FOR -- Siguiente mensualidad

    END FOREACH -- Siguiente NSS

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el archivo plano de la operacion 12                  #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()
    DEFINE lr_parcial RECORD LIKE ret_parcial.*
    DEFINE lr_etapa_1 RECORD
        nss                 CHAR(11) ,
        folio_serv          INTEGER  ,
        tipo_serv           CHAR(01) ,
        usuario             CHAR(15)
    END RECORD,
        ls_codigo_afore    CHAR(3)

    DEFINE
        li_num_regs                 INTEGER

    DISPLAY " ENVIANDO INFORMACION OPERACION 12 ...                   " AT 19,1 ATTRIBUTE(REVERSE)

    LET li_num_regs = 0

    -- Variables para el cierre de etapas de servicios INVERCAP
    LET lr_etapa_1.tipo_serv    = "S"
    LET lr_etapa_1.usuario      = gc_usuario
    LET ls_codigo_afore = gs_codigo_afore USING "&&&"

    DECLARE cur_rep CURSOR FOR
    SELECT  A.*
    FROM    ret_parcial A       ,
            tmp_monto_siefore B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo        = B.consecutivo
    AND    A.estado_solicitud   = gr_estado.confirmado
    AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)

    INITIALIZE lr_parcial.* TO NULL
    FOREACH cur_rep INTO lr_parcial.*

        LET lr_etapa_1.nss         = lr_parcial.nss
        LET lr_etapa_1.folio_serv  = lr_parcial.folio_solicitud

        -- Si es Invercap, se ejecuta el cierre automatico de la etapa 1
        IF gs_codigo_afore = gs_cod_inv THEN
            CALL f_inv_sol_etapa(lr_etapa_1.*)
        END IF

        LET mr_envio.folioCliente     = lr_parcial.consecutivo
        LET mr_envio.nss              = lr_parcial.nss
        LET mr_envio.tipo_Retiro      = lr_parcial.tipo_retiro
        LET mr_envio.tipo_Prestacion  = lr_parcial.tipo_prestacion  USING "&&"
        LET mr_envio.num_Resolucion   = lr_parcial.num_resolucion USING "&&&&&&"

        SELECT UNIQUE paterno, materno, nombres
        INTO   mr_envio.apellidoPaterno, mr_envio.apellidoMaterno, mr_envio.nombre
        FROM   afi_mae_afiliado
        WHERE  n_seguro = lr_parcial.nss

           INSERT INTO tmp_bus_diag12 VALUES (
                                              mr_envio.folioCliente,
                                              1, ---folio
                                              mr_envio.nss,
                                              mr_envio.nombre,
                                              mr_envio.apellidoPaterno,
                                              mr_envio.apellidoMaterno,
                                              mr_envio.tipo_Retiro,
                                              mr_envio.tipo_Prestacion,
                                              mr_envio.num_Resolucion,
                                              ls_codigo_afore );

           LET li_num_regs = li_num_regs + 1

    END FOREACH

    INSERT INTO tmp_ctr_envio
        VALUES (1                           ,-- folio
                "AV12"                      ,-- tipo_operacion
                "CZAAV12 DETAV12 SUMAV12"   ,-- estructura
                gr_estado.enviado         ,-- status	#cambiar estado a enviado
                0                           ,-- orden_de_envio
                HOY                          -- fecha_envio
               )

    INSERT INTO tmp_envio
    VALUES (1                       , -- folio
            12                      , -- tipo_operacion
            HOY                     , -- fecha_envio
            CURRENT HOUR TO SECOND  , -- hora_envio
            "NUEVA PLATAFORMA"      , -- nom_archivo
            li_num_regs             , -- tot_registros
            gc_usuario              , -- usuario
            gr_estado.enviado         -- estado
           )

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Consolida y actualiza las tablas fisicas a partir de las    #
#               tablas temporales del proceso                               #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE li_folio LIKE ret_parcial.folio

    -- -----------------------------------------------------------------------------

    DISPLAY " CONSOLIDANDO TABLAS FISICAS ...                            " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_folio    = f_lib_obtiene_ult_folio()
    DISPLAY "FOLIO OPERACION 12 : " AT 16, 4
    DISPLAY li_folio  AT 16, 25

    -- Copiamos ret_bus_diag12 de la tabla temporal a la definitiva
    UPDATE tmp_bus_diag12
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO ret_bus_diag12
    SELECT *
    FROM   tmp_bus_diag12
    WHERE  folio = li_folio

    -- Copiamos ret_monto_siefore de la tabla temporal a la definitiva
    UPDATE tmp_monto_siefore
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_siefore
    SELECT *
    FROM   tmp_monto_siefore
    WHERE  folio = li_folio

    -- Copiamos ret_ctr_pago_det de la tabla temporal a la definitiva
    INSERT INTO ret_ctr_pago_det
    SELECT *
    FROM   tmp_ctr_pago_det

    -- Copiamos ret_ctr_envio de la tabla temporal a la definitiva
    UPDATE tmp_ctr_envio
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO ret_ctr_envio
    SELECT *
    FROM   tmp_ctr_envio
    WHERE  folio = li_folio

    -- Copiamos ret_envio de la tabla temporal a la definitiva
    UPDATE tmp_envio
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO ret_envio
    SELECT *
    FROM   tmp_envio
    WHERE  folio = li_folio

   -- Actualizamos el estado y el folio de ret_ctr_pago
    UPDATE ret_ctr_pago
    SET    folio_op12   = li_folio              ,
           estado       = gr_estado.provisionado
    WHERE  consecutivo  IN (SELECT consecutivo
                            FROM   ret_monto_siefore
                            WHERE  folio = li_folio
                           )

   -- Actualizamos el estado y el folio de ret_parcial
    UPDATE ret_parcial
    SET    estado_solicitud = gr_estado.enviado   , #cambio estado a enviado
           folio            = li_folio              ,
           fecha_genera     = HOY                   ,
           fecha_envio      = HOY
    WHERE  consecutivo      IN (SELECT consecutivo
                                FROM   ret_monto_siefore
                                WHERE  folio = li_folio
                               )

    CALL f_lib_error_msg("PROCESO FINALIZADO CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_ejecucion : Valida si existen registros a procesar y en caso de  #
#                      haberlos indica en pantalla cuantos y de que tipo son#
#---------------------------------------------------------------------------#
FUNCTION f_valida_ejecucion()

    DEFINE lr_datos ARRAY[2] OF RECORD
        tipo                CHAR(10)    ,
        num_regs            SMALLINT
    END RECORD

    DEFINE lr_valida_precios RECORD
        codigo_err               SMALLINT   ,
        mensaje                  CHAR(100)
    END RECORD 

    DEFINE
        ls_procesa          ,
        ls_cont             SMALLINT

    -- -----------------------------------------------------------------------------

    -- Valida que existan precios de accion
    CALL f_lib_valida_precios_accion(HOY) RETURNING lr_valida_precios.*
    
    IF lr_valida_precios.codigo_err <> 0 THEN
        CALL f_lib_error_msg(lr_valida_precios.mensaje)
        LET ls_procesa  = 0            
    ELSE
        INITIALIZE lr_datos[1].* TO NULL
        INITIALIZE lr_datos[2].* TO NULL
        
        LET ls_cont     = 1
        LET ls_procesa  = 1
        #Se valida el saldo en la cuenta #ACS -29052014
        DECLARE cur_regs CURSOR FOR
            SELECT a.tipo_prestacion, COUNT(unique a.nss),SUM(b.monto_en_acciones)
            FROM   ret_parcial a, dis_cuenta b
            WHERE  a.nss = b.nss
            AND a.estado_solicitud = gr_estado.confirmado
            AND b.subcuenta IN (1,2,5,6,9)
            AND ((a.rechazo_cod = 0) OR (a.rechazo_cod IS NULL))
            GROUP BY 1
            HAVING SUM(b.monto_en_acciones) > 0
            ORDER BY 1

        FOREACH cur_regs INTO lr_datos[ls_cont].*
            LET ls_cont = ls_cont + 1
        
            IF ls_cont > 2 THEN
                EXIT FOREACH
            END IF
        
        END FOREACH
        
        IF ls_cont > 1 THEN
            DISPLAY "REGISTROS A PROCESAR" AT 6,8
            DISPLAY "DESEMPLEO       : " AT 8,17
            DISPLAY lr_datos[1].num_regs AT 8,35
        
            DISPLAY "MATRIMONIO      : " AT 9,17
            DISPLAY lr_datos[2].num_regs AT 9,35
        
            WHILE TRUE
                PROMPT "¿DESEA GENERAR LA OPERACION 12? (S/N) " FOR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa  = 1
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET ls_procesa  = 0
                    END IF
        
                    EXIT WHILE
                END IF
            END WHILE
        ELSE
            CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROCESAR")
            LET ls_procesa  = 0
        END IF
    END IF -- Valida precios de accion

    RETURN ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  liquidacion de retiros issste                            #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc825 AT 4,4 WITH FORM "RETC8251" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC825 GENERA SOLICITUD DE VERIFICACION DE RESOLUCION (Op 12)                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_temp : Genera las tablas temporales donde se almacenan los       #
#                       calculos y cambios la operacion 12                  #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_temp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_monto_siefore
        DROP TABLE tmp_ctr_pago_det
        DROP TABLE tmp_ctr_envio
        DROP TABLE tmp_envio
        DROP TABLE tmp_bus_diag12
    WHENEVER ERROR STOP

    --------------------------------

    SELECT *
    FROM   ret_monto_siefore
    WHERE  1 = 0
    INTO TEMP tmp_monto_siefore WITH NO LOG

    --------------------------------

    SELECT *
    FROM   ret_ctr_pago_det
    WHERE  1 = 0
    INTO TEMP tmp_ctr_pago_det WITH NO LOG

    --------------------------------

    SELECT *
    FROM   ret_ctr_envio
    WHERE  1 = 0
    INTO TEMP tmp_ctr_envio WITH NO LOG

    --------------------------------

    SELECT *
    FROM   ret_envio
    WHERE  1 = 0
    INTO TEMP tmp_envio WITH NO LOG

    --------------------------------
    SELECT * FROM safre_af:ret_bus_diag12
    WHERE 1 = 0
    INTO TEMP tmp_bus_diag12 WITH NO LOG
    --------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# f_inv_sol_etapa : Ejecuta el cierre automatico de la etapa 1 para el      #
#                   modulo de servicios                                     #
#---------------------------------------------------------------------------#
FUNCTION f_inv_sol_etapa( l_nss_afi         ,
                          l_fol_serv        ,
                          l_tipo_serv       ,
                          l_usuario )

#---          Definición de Variables que cacha la Funcion     ---
DEFINE  l_nss_afi               CHAR(11)
DEFINE  l_fol_serv              INTEGER
DEFINE  l_tipo_serv             CHAR(01)
DEFINE  l_usuario               CHAR(10)
#---  Fin de las Definiciones de Variables que cacha la Funcion ---

DEFINE  l_verif_txt_eta_sol            ,
        l_procede                      ,
        l_encontro                     ,
        l_consec_sol                   ,
        l_etapa_cod                    ,
        l_resol_linea           SMALLINT

DEFINE  l_leyenda_eta1          CHAR(25)

DEFINE  reg_serv     RECORD
        folio_rec    LIKE safre_af:rec_solucion.folio_rec         ,
        tipo_id      LIKE safre_af:rec_solucion.tipo_id           ,
        tipo_cod     LIKE safre_af:rec_solucion.tipo_cod          ,
        motivo_cod   LIKE safre_af:rec_solucion.motivo_cod        ,
        etapa_cod    LIKE safre_af:rec_solucion.etapa_cod         ,
        ffin_est     LIKE safre_af:rec_solucion.ffin_est          ,
        ffin_real    LIKE safre_af:rec_solucion.ffin_real         ,
        consec_sol   LIKE safre_af:rec_solucion.consec_sol        ,
        termino_cod  LIKE safre_af:rec_solicitud.termino_cod      ,
        n_seguro     LIKE safre_af:afi_mae_afiliado.n_seguro
                     END RECORD

   #Valores Fijos

   LET      l_consec_sol           =         1
   LET      l_etapa_cod            =         1
   LET      l_resol_linea          =         1
   LET      l_leyenda_eta1         =         "SOLICITUD PROCESADA"

   LET      l_encontro             =         0
   LET      l_procede              =         0

   SELECT  COUNT(*)
   INTO    l_encontro
   FROM    rec_solicitud A
   WHERE   A.folio_rec            =        l_fol_serv
   AND     A.tipo_id              =        l_tipo_serv
   AND     A.n_seguro             =        l_nss_afi
   AND     A.termino_cod          =        10          #INICIADO ó  LIBERADO

   CASE l_encontro

      WHEN 0  #Registro  -- NO -- ENCONTRADO PARA CERRARLE ETAPA

         LET l_procede  = 0 #NO PROCEDE
         RETURN

      WHEN 1  #CASO NORMAL

         LET l_procede  = 1 #SE PRENDE BANDERA

      OTHERWISE #Registro Duplicado NO  PUEDE CERRARSE LAS ETAPAS

         LET l_procede  = 0 #NO PROCEDE
         RETURN

   END CASE

   IF  ( l_procede   =   1  )  THEN #REGISTRO SI PROCEDE

      #-- INICIALIZA VARIABLES --#

          INITIALIZE  reg_serv.*  TO   NULL

      #-- FIN DE INICIALIZACION DE VARIABLES--#

      DECLARE c CURSOR FOR

         SELECT  solucion.folio_rec       ,
                 solucion.tipo_id         ,
                 solucion.tipo_cod        ,
                 solucion.motivo_cod      ,
                 solucion.etapa_cod       ,
                 solucion.ffin_est        ,
                 solucion.ffin_real       ,
                 solucion.consec_sol      ,
                 recsolic.termino_cod     ,
                 recsolic.n_seguro
         FROM    rec_solucion solucion    , rec_solicitud recsolic
         WHERE   solucion.folio_rec  > 0
         AND     recsolic.folio_rec       =       l_fol_serv
         AND     recsolic.tipo_id         =       l_tipo_serv
         AND     recsolic.folio_rec       =       solucion.folio_rec
         AND     recsolic.tipo_id         =       solucion.tipo_id
         AND     solucion.ffin_real  IS NULL
         AND     recsolic.termino_cod     =   10 #LIBERADA o INICIADA
         AND     solucion.consec_sol      =   1  #Esto es para cerrarle la 1era
                                                 #etapa y asegurarnos que tenga
                                                 #1era Etapa.


      FOREACH c INTO reg_serv.*

         UPDATE  safre_af:rec_solicitud
         SET     termino_cod       =   50 #EN PROCESO DE SOLUCIÒN
         WHERE   folio_rec         =   reg_serv.folio_rec
         AND     tipo_id           =   reg_serv.tipo_id
         AND     n_seguro          =   reg_serv.n_seguro

         UPDATE  safre_af:rec_solucion
         SET     ffin_real         =   TODAY
         WHERE   folio_rec         =   reg_serv.folio_rec
         AND     tipo_id           =   reg_serv.tipo_id
         AND     etapa_cod         =   l_etapa_cod   #1
         AND     consec_sol        =   l_consec_sol  #1

         #Verifica si existe el registro antes de INSERTARSE

         LET l_verif_txt_eta_sol   =    0

         SELECT  COUNT(*)
         INTO    l_verif_txt_eta_sol
         FROM    rec_txt_etapa_sol A
         WHERE   A.folio_rec         =   reg_serv.folio_rec
         AND     A.tipo_id           =   reg_serv.tipo_id
         AND     A.etapa_cod         =   l_etapa_cod     #1
         AND     A.consec_sol        =   l_consec_sol    #1

         IF ( l_verif_txt_eta_sol = 0 ) THEN #-- NO EXISTE REG -- SE PUEDE INSERTAR

            INSERT INTO safre_af:rec_txt_etapa_sol VALUES ( reg_serv.folio_rec  ,#folio_rec
                                                            reg_serv.tipo_id    ,#tipo_id
                                                            l_consec_sol        ,#consec_sol
                                                            l_etapa_cod         ,#etapa_cod
                                                            l_resol_linea       ,#resol_linea
                                                            l_leyenda_eta1      ,#resol_eta
                                                            l_usuario           ,#usuario
                                                            TODAY               #factualiza
                                                            )
         END IF
      END FOREACH
   END IF #FIN DE  VALIDACION DE REGISTRO PROCEDIDO ó NO

   #---INICIALIZA VARIABLES DE SALIDA  f_inv_sol_etapa ---#

   LET      l_consec_sol           =         0
   LET      l_etapa_cod            =         0
   LET      l_resol_linea          =         0
   LET      l_leyenda_eta1         =         NULL
   LET      l_encontro             =         0
   LET      l_procede              =         0
   LET      l_verif_txt_eta_sol    =         0

   #---FIN DE INICIALIZACION DE VARIABLES ---#

END FUNCTION

