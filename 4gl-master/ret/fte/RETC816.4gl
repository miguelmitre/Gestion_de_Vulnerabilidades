#################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                 #
#Owner             => E.F.P.                                                    #
#Programa RETC816  => GENERA NOTIFICACION DE DISPOSICION DE RECURSOS SOLICITUD  #
#                     OP.05 POR RETIRO "E"                                      #
#Fecha creacion    => 02 DE ENERO 2004                                          #
#By                => STEFANIE DANIELA VERA PIÑA                                #
#Fecha actualiza   => 11 DE DICIEMBRE DE 2007                                   #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiza   => 30 DE JUNIO DE 2008                                       #
#                  => Se modifico la forma de provisionar las subcuentas para   #
#                     que en el caso de vivienda se provisione con el saldo al  #
#                     primer dia del mes. Ademas se incluye la verificacion de  #
#                     sobregiro en el caso de la subcuenta de vivienda.         #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 15 DE FEBRERO DE 2012                                     #
#                     Modificaciones relacionadas a los cambios de la ley del   #
#                     INFONAVIT (Req. EFPS-187)                                 #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 7 DE MARZO DE 2012                                        #
#                     Mejoras relacionadas a los cambios de la ley del          #
#                     INFONAVIT (Req. EFPS-195)                                 #
#Sistema           => RET                                                       #
#-------------------------------------------------------------------------------#
#Modificado        => PHELIPPE RICARDO DOS SANTOS                               #
#                  => Se agrega nueva validacion con mensaje                    #
#Fecha             => 19 MAYO 2015        CPL-1977                              #
#-------------------------------------------------------------------------------#
#Modificado        => Alejandro Chagoya Salazar   CPL-2020   08-julio-2015      #
#                  => Se agrega validacion de saldo para vivienda en detalle 03 #
#################################################################################
#Modificacion      => Jonathan Joddy Zavala Zavala   CPL-2106  1-Oct-2015       #
#                  => Se actualiza programa agregando caracteristicas derivadas #
#                  => de la creacion de la siefore basica 0/90                  #
#################################################################################

DATABASE safre_af

GLOBALS

    #DEFINE gar_precio_acc ARRAY [20] OF RECORD   #CPL-2106
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
        estado          SMALLINT        ,
        fecha           DATE            ,
        siefore         SMALLINT        ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        modificado          LIKE ret_estado.estado_solicitud ,
        confirmado          LIKE ret_estado.estado_solicitud ,
        procesado           LIKE ret_estado.estado_solicitud ,
        cancelado           LIKE ret_estado.estado_solicitud ,
        transicion          LIKE ret_estado.estado_solicitud ,
        liquidado           LIKE ret_estado.estado_solicitud ,
        enviado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_afores RECORD
        metlife             SMALLINT
    END RECORD

    DEFINE gr_param_ret RECORD LIKE seg_modulo.*

    DEFINE
        HOY                     ,
        gdt_cambio_infonavit    ,
        gd_fecha_viv            DATE

    DEFINE #glo #char
        comando                 CHAR(110) ,
        gc_tipo_ret             CHAR(001) ,
        enter                   CHAR(001) ,
        gc_usuario              CHAR(015) ,
        G_LISTA_DET             CHAR(100) ,
        HORA                    CHAR(005) ,
        v_provision             CHAR(002)

    DEFINE #glo #smallint
        gs_mov_viv97            ,
        gs_codigo_afore         ,
        gs_num_siefores         ,
        sw_1                    ,
        gs_tipo_movimiento      SMALLINT

    DEFINE #glo #integer
        gi_ult_folio            ,
        cont_reg                INTEGER

    DEFINE cont_reg_rech      INTEGER

END GLOBALS

DEFINE ms_99_siefores  SMALLINT   #CPL-2106

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC816")

    CALL init()
    CALL f_abre_ventana()

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                CALL fn_identifica_benef_designados()
                CALL primer_paso()       #-- Calcula saldos
                CALL primer_paso_benef() #-- Calcula saldos benef
                CALL segundo_paso()      #-- Genera plano
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
            END IF

            EXIT WHILE
        END IF
    END WHILE

    CLOSE WINDOW retc8161

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare              CHAR(300)
   
   DEFINE ls_count1           SMALLINT
   DEFINE ls_count2           SMALLINT

    -- -----------------------------------------------------------------------------
    LET ls_count1 = 0
    LET ls_count2 = 0

    LET HOY                     = TODAY
    LET HORA                    = TIME
    LET gc_tipo_ret             = "E"
    LET gs_tipo_movimiento      = 0
    LET gdt_cambio_infonavit    = MDY(01,12,2012)
    LET gc_usuario              = f_lib_obten_user()
    LET gs_num_siefores         = f_lib_obtiene_num_siefores()
    LET ms_99_siefores          = 99   #CPL-2106

    CALL f_obtiene_precios_accion(HOY)

    SELECT *
    INTO   gr_param_ret.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gr_afores.metlife
    FROM   tab_afore
    WHERE  afore_desc MATCHES "[Mm][Ee][Tt][Ll][Ii][Ff][Ee]"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.modificado
    FROM   ret_estado A
    WHERE  A.descripcion = "MODIFICADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.cancelado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.transicion
    FROM   ret_estado A
    WHERE  A.descripcion = "TRANSICION"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT movimiento
    INTO   gs_tipo_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = gc_tipo_ret
    
    SELECT COUNT(UNIQUE nss)
    INTO   ls_count1
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro        = gc_tipo_ret
    AND    A.estado_solicitud   IN (gr_edo.confirmado, gr_edo.modificado)
    AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
    AND    (A.diag_registro = '0' OR A.diag_registro IS NULL OR A.diag_registro = "   ")
    AND    A.nss          NOT IN (SELECT UNIQUE nss
                                    FROM ret_ctr_benef)

    SELECT COUNT(UNIQUE nss)
    INTO   ls_count2
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro        = gc_tipo_ret
    AND    A.estado_solicitud   IN (gr_edo.confirmado, gr_edo.modificado,gr_edo.transicion)
    AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
    AND    (A.diag_registro = '0' OR A.diag_registro IS NULL OR A.diag_registro = "   ")
    AND    A.nss              IN (SELECT UNIQUE nss
                                    FROM ret_ctr_benef)

    IF (ls_count1 + ls_count2) = 0 THEN
        CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROCESAR")
        EXIT PROGRAM
    ELSE
        LET gi_ult_folio = f_lib_obtiene_ult_folio()
    END IF

    --- SALDOS ---
    LET lc_prepare =  " EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? )"
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    --- PROVISION ---
    LET lc_prepare = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM lc_prepare

    LET lc_prepare = " "

    --- FECHA VALOR VIVIENDA ---
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val ( ? ) "
    PREPARE eje_fecha_viv FROM  lc_prepare
    EXECUTE eje_fecha_viv USING HOY
                                INTO gd_fecha_viv

    LET lc_prepare = " "
    
    ----- DESMARCA DE CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE desmarca_cuenta( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Obtiene los registros que seran provisionados y calcula el  #
#               monto a provisionar                                         #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_solicitud RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_viv97 RECORD
        siefore             SMALLINT        ,
        subcuenta           SMALLINT        ,
        acciones            DECIMAL(16,6)   ,
        pesos               DECIMAL(16,6)
    END RECORD

    #DEFINE arr_siefore ARRAY [20] OF RECORD   #CPL-2106
   DEFINE arr_siefore ARRAY [99] OF RECORD
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_ret92    DECIMAL(16,6) ,
        acciones_viv92    DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE
        ld_fecha_saldo          ,
        ld_fecha_saldo_viv      DATE

    DEFINE
        ls_siefore            , #-- contador para los ciclos for
        f_siefore             ,
        f_subcuenta           ,
        s_subcta              SMALLINT

    DEFINE
        f_monto_pes          ,
        f_monto_acc          ,
        acciones_cv          ,
        s11_acc_viv92        ,
        s11_acc_viv97        DECIMAL(16,6)

    DEFINE lr_infonavit      RECORD LIKE ret_folio_infonavit.*

    DEFINE 
        ls_consulta          CHAR(1000)

    -- -----------------------------------------------------------------------------

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    LET ld_fecha_saldo = HOY
    
    LET ls_consulta = " SELECT A.* \n ",
                      " FROM   ret_solicitud_tx A \n ",
                      " WHERE  A.tipo_retiro      = '", gc_tipo_ret, "' \n ",
                      " AND    A.estado_solicitud IN (",gr_edo.confirmado, ",", gr_edo.modificado, ") \n ",
                      " AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL) \n ",
                      " AND    (A.diag_registro = '0' OR A.diag_registro IS NULL OR A.diag_registro = '   ') \n ",
                      " AND    A.nss||A.consecutivo NOT IN (SELECT UNIQUE nss||consecutivo_solic \n ",
                      "                                     FROM ret_ctr_benef) \n ",
                      " ORDER BY 1 "
 
    PREPARE cur_pre_1 FROM ls_consulta
    DECLARE cur_1 CURSOR FOR cur_pre_1

    -- Iniciamos ciclo para cada nss
    FOREACH cur_1 INTO lr_solicitud.*

        -- Inicializamos variables del arreglo
        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo             = FALSE
            LET arr_siefore[ls_siefore].acciones_ret97     = 0
            LET arr_siefore[ls_siefore].acciones_cv        = 0
            LET arr_siefore[ls_siefore].acciones_cs        = 0
            LET arr_siefore[ls_siefore].acciones_est       = 0
            LET arr_siefore[ls_siefore].acciones_ret92     = 0
            LET arr_siefore[ls_siefore].acciones_viv92     = 0
            LET arr_siefore[ls_siefore].acciones_esp       = 0
        END FOR

        LET s11_acc_viv92 = 0
        LET s11_acc_viv97 = 0

        --CPL-3422 Institutos Cruzados, solo provisiona subcuenta 7 y 8 grupo 2
        INITIALIZE lr_infonavit.* TO NULL
        
        SELECT *
        INTO lr_infonavit.*
        FROM ret_folio_infonavit
        WHERE nss = lr_solicitud.nss
        AND   consecutivo = lr_solicitud.consecutivo

        IF lr_infonavit.tipo_ventanilla = '0202' THEN
            LET lr_solicitud.grupo = 2
        END IF

        DECLARE cur_2 CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = lr_solicitud.grupo
        AND    subcuenta > 0

        -- Iniciamos ciclo para cada subcuenta del grupo del nss actual
        FOREACH cur_2 INTO s_subcta

            LET f_subcuenta  = 0
            LET f_siefore    = 0
            LET f_monto_acc  = 0
            LET f_monto_pes  = 0
            LET v_provision  = "SI"

            IF lr_solicitud.tipo_pension = "IP" THEN
                IF (s_subcta <> 1) OR (s_subcta = 1 AND lr_solicitud.regimen = 97) THEN
                    IF lr_solicitud.porcentaje_val < 50  THEN
                        LET v_provision = "NO"
                    END IF
                END IF
            END IF

            IF v_provision = "SI" THEN

                CALL f_obten_saldo_subcta(lr_solicitud.nss, s_subcta, ld_fecha_saldo)
                    RETURNING f_siefore, f_subcuenta, f_monto_acc, f_monto_pes

                IF f_monto_acc > 0 AND f_monto_pes > 0 THEN
                    IF f_siefore <> 11 THEN
                        #-- Marcamos como activo el registro de la siefore actual
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
                            WHEN 8
                                LET arr_siefore[f_siefore].acciones_viv92 = f_monto_acc
                            WHEN 9
                                LET arr_siefore[f_siefore].acciones_esp   = f_monto_acc
                        END CASE
                    ELSE
                       IF f_subcuenta = 8 THEN
                            LET s11_acc_viv92   = f_monto_acc
                        END IF

                    END IF
                    IF f_subcuenta <> 4 THEN  --- Se quita de este punto la subcuenta de vivienda 97 CPL-3355 

                        CALL f_provisiona_subcta(lr_solicitud.nss           ,
                                                 f_siefore                  ,
                                                 f_subcuenta                ,
                                                 lr_solicitud.consecutivo   ,
                                                 f_monto_acc                ,
                                                 f_monto_pes                )
                    END IF 

                END IF

            END IF -- si provisiono

        END FOREACH -- Subcuentas

        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET acciones_cv = 0
            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                IF arr_siefore[ls_siefore].acciones_ret97 < 0 THEN
                    LET arr_siefore[ls_siefore].acciones_ret97 = 0
                END IF

                LET acciones_cv = arr_siefore[ls_siefore].acciones_cv  +
                                  arr_siefore[ls_siefore].acciones_est +
                                  arr_siefore[ls_siefore].acciones_esp

                IF arr_siefore[ls_siefore].acciones_ret92 < 0 THEN
                    LET arr_siefore[ls_siefore].acciones_ret92 = 0
                END IF

                INSERT INTO ret_monto_siefore
                    VALUES(lr_solicitud.nss                         ,--nss
                           lr_solicitud.consecutivo                 ,--consecutivo
                           gi_ult_folio                             ,--folio
                           gc_tipo_ret                              ,--tipo_retiro
                           5                                        ,--tipo_operacion
                           ls_siefore                               ,--siefore
                           arr_siefore[ls_siefore].acciones_ret97   ,--acciones_ret97
                           acciones_cv                              ,--acciones_cv
                           arr_siefore[ls_siefore].acciones_cs      ,--acciones_cs
                           arr_siefore[ls_siefore].acciones_ret92    --acciones_ret92
                          )
            END IF
        END FOR

        INITIALIZE lr_viv97.* TO NULL
        LET s11_acc_viv97       = 0
        LET lr_viv97.acciones   = 0

        -- Si se le indico al programa no considerar el pago de vivienda (Estado = 33)
        -- No debe entrar a verificar la vivienda 97 en ningun caso
        IF lr_solicitud.estado_solicitud <> gr_edo.modificado THEN
            IF  lr_infonavit.tipo_ventanilla IS NULL OR lr_infonavit.tipo_ventanilla <> '0202' THEN
                IF ( (lr_solicitud.regimen = 73) AND (lr_solicitud.fecha_resolucion > gdt_cambio_infonavit))
                THEN
                    SELECT "OK"
                    FROM   ret_notifica_vivienda
                    WHERE  nss              = lr_solicitud.nss
                    AND    consecutivo      = lr_solicitud.consecutivo
                    AND    grupo_trabajador = "0101"
                    AND    indicador        = "A"
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        CALL f_obten_saldo_subcta(lr_solicitud.nss, 4, ld_fecha_saldo)
                            RETURNING lr_viv97.*

                        LET s11_acc_viv97   = lr_viv97.acciones

                        IF s11_acc_viv97 > 0 AND lr_viv97.pesos THEN
                            CALL f_provisiona_subcta(lr_solicitud.nss           ,
                                                     lr_viv97.siefore           ,
                                                     lr_viv97.subcuenta         ,
                                                     lr_solicitud.consecutivo   ,
                                                     lr_viv97.acciones          ,
                                                     lr_viv97.pesos
                                                    )
                        END IF

                        UPDATE dis_provision
                        SET    tipo_movimiento  = gs_mov_viv97
                        WHERE  nss              = lr_solicitud.nss
                        AND    folio            = gi_ult_folio
                        AND    consecutivo_lote = lr_solicitud.consecutivo
                        AND    subcuenta       IN (4,8)
                        AND    tipo_movimiento  = gs_tipo_movimiento
                    ELSE
                        LET s11_acc_viv97       = 0
                        LET lr_viv97.acciones   = 0
                    END IF
                ELSE
                    LET s11_acc_viv97       = 0
                    LET lr_viv97.acciones   = 0
                END IF -- Provision de la subcuenta 4
            END IF
        END IF -- Estado modificado

        INSERT INTO ret_monto_viv
        VALUES(lr_solicitud.nss         ,   -- nss
               lr_solicitud.consecutivo ,   -- consecutivo
               gi_ult_folio             ,   -- folio
               gc_tipo_ret              ,   -- tipo_retiro
               gd_fecha_viv             ,   -- fecha_valor_viv
               s11_acc_viv97            ,   -- acciones_viv97
               s11_acc_viv92            ,   -- acciones_viv92
               0                        ,   -- pesos_viv72
               NULL                     ,   -- estado_sub_viv
               0                        ,   -- acc_viv97_bdsviv
               0                            -- acc_viv92_bdsviv
              )

        LET s11_acc_viv97   = lr_viv97.acciones

        UPDATE ret_solicitud_tx
        SET    folio               = gi_ult_folio    ,
               saldo_viv97         = s11_acc_viv97   ,
               saldo_viv72         = 0               ,
               saldo_viv92         = s11_acc_viv92   ,
               fecha_envio         = HOY             ,
               fecha_valor_viv     = gd_fecha_viv    ,
               estado_solicitud    = gr_edo.procesado
        WHERE  nss                 = lr_solicitud.nss
        AND    consecutivo         = lr_solicitud.consecutivo
        AND    estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado)

    END FOREACH -- Siguiente NSS

END FUNCTION


FUNCTION fn_identifica_benef_designados()

   DEFINE arr_subcuentas ARRAY [8] OF SMALLINT

   DEFINE v_fecha_saldo       DATE
   DEFINE v_fecha_saldo_dia   DATE
   DEFINE v_fecha_saldo_viv   DATE
   
   DEFINE i                   SMALLINT
   DEFINE v_nss               CHAR(11)
   DEFINE v_folio_sol         INTEGER
   DEFINE v_consecutivo       DECIMAL(11,0)
   DEFINE v_consecutivo_padre DECIMAL(11,0)
   DEFINE v_consec_padre_nul  DECIMAL(11,0)
   
   DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pesos          LIKE dis_cuenta.monto_en_pesos
   END RECORD
   DEFINE v_precio_del_dia    DECIMAL(19,14)
   DEFINE ls_tipo_benef       SMALLINT
   DEFINE ls_grupo            SMALLINT

    DISPLAY " IDENTIFICACIÓN DE BENEFICIARIOS " AT 19,1 ATTRIBUTE(REVERSE)

   --WHENEVER ERROR CALL fn_error

   LET arr_subcuentas[1] = 1
   LET arr_subcuentas[2] = 2
   LET arr_subcuentas[3] = 4
   LET arr_subcuentas[4] = 5
   LET arr_subcuentas[5] = 6
   LET arr_subcuentas[6] = 7
   LET arr_subcuentas[7] = 8
   LET arr_subcuentas[8] = 9
   
   --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes
   IF MONTH(gd_fecha_viv) = MONTH(TODAY) THEN
       LET v_fecha_saldo_viv = gd_fecha_viv
   ELSE
      -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
      LET v_fecha_saldo_viv = HOY
   END IF
   
   LET v_fecha_saldo_dia = HOY

   LET ls_grupo = 0
   INITIALIZE lr_saldos.* TO NULL

   --BARRE LAS SOLICITUDES QUE COINCIDAN CON BENEFICIARIOS DESIGNADOS
   DECLARE cur_benef CURSOR FOR 
      SELECT a.nss,a.folio_solicitud,a.consecutivo,b.tipo_benef
      FROM ret_solicitud_tx a,
           ret_ctr_benef b
      WHERE a.nss = b.nss
      AND   a.consecutivo = b.consecutivo_solic
      AND   a.tipo_retiro = gc_tipo_ret
      AND   a.estado_solicitud IN (gr_edo.confirmado, gr_edo.modificado)
      AND   (a.rechazo_cod = 0 OR a.rechazo_cod IS NULL)
      AND   (a.diag_registro = '0' OR a.diag_registro IS NULL OR a.diag_registro = "   ")
      ORDER BY a.nss,a.folio_solicitud,a.consecutivo

   FOREACH cur_benef INTO v_nss,v_folio_sol,v_consecutivo,ls_tipo_benef
           
      IF ls_tipo_benef = 3 OR ls_tipo_benef = 1 THEN
          IF (fn_valida_benef(v_nss, v_consecutivo, ls_tipo_benef) = 1) THEN
              CONTINUE FOREACH
          END IF
      END IF

      CALL genera_tmp_cuenta(v_nss)

      --Busca el consecutivo padre
      SELECT MIN(consecutivo)
      INTO v_consecutivo_padre
      FROM ret_solicitud_tx
      WHERE nss            = v_nss
      AND folio_solicitud  = v_folio_sol
      AND tipo_retiro      = gc_tipo_ret
      AND estado_solicitud IN (gr_edo.confirmado, gr_edo.modificado)

      --Evalua si ya tiene consecutivo padre al solicitud
      SELECT UNIQUE consecutivo_padre
      INTO v_consec_padre_nul
      FROM ret_ctr_benef
      WHERE nss             = v_nss
      AND consecutivo_solic = v_consecutivo
      AND tipo_retiro = gc_tipo_ret

      IF v_consec_padre_nul IS NULL THEN  --Si no tiene consecutivo padre, le asigna el menor consecutivo de la solicitud
         UPDATE ret_ctr_benef
         SET consecutivo_padre = v_consecutivo_padre
         WHERE nss             = v_nss
         AND consecutivo_solic = v_consecutivo
         AND consecutivo_padre IS NULL
         AND tipo_retiro = gc_tipo_ret
      END IF

      IF v_consecutivo = v_consecutivo_padre THEN --Evalúa si la solicitud es el consecutivo padre. 
      
         CALL fn_verif_saldo_benef(v_nss)

         FOR i = 1 TO 8
             --Consulta saldo benef
             SELECT MAX(fecha_saldo)
             INTO v_fecha_saldo
             FROM ret_saldo_benef
             WHERE nss       = v_nss
               AND subcuenta = arr_subcuentas[i]
             
             -- Si no tiene saldos, llena la tabla con el saldo al día 
             IF v_fecha_saldo IS NULL THEN
                  
                 DECLARE cur_saldo_dia CURSOR FOR eje_saldo_dia
                 OPEN cur_saldo_dia USING v_nss,
                                          arr_subcuentas[i],
                                          ls_grupo,
                                          v_fecha_saldo_dia
                 
                 FETCH cur_saldo_dia INTO lr_saldos.*
                 CLOSE cur_saldo_dia
                 IF lr_saldos.monto_acciones > 0 AND lr_saldos.monto_pesos > 0 THEN
                    INSERT INTO ret_saldo_benef(nss,fecha_saldo,subcuenta,siefore,monto_acc,monto_pes)
                    VALUES(v_nss,HOY,lr_saldos.subcuenta,lr_saldos.siefore,lr_saldos.monto_acciones,lr_saldos.monto_pesos)
                 END IF
                
             ELSE -- Si ya tiene saldos, busca las aportaciones posteriores a la última fecha saldo
                 SELECT a.siefore,a.subcuenta,SUM(a.monto_en_acciones),SUM(a.monto_en_pesos)
                   INTO lr_saldos.siefore,lr_saldos.subcuenta,lr_saldos.monto_acciones,lr_saldos.monto_pesos
                 FROM tmp_dis_cuenta a,
                      tab_movimiento b
                 WHERE b.codigo = a.tipo_movimiento
                 AND   b.tipo = 1
                 AND   a.subcuenta = arr_subcuentas[i]
                 AND   a.nss = v_nss
                 AND   a.fecha_conversion > v_fecha_saldo
                 AND   a.tipo_movimiento IN (1, 2, 3, 1319, 1320, 1321, 1322)
                 GROUP BY a.siefore,a.subcuenta
                 HAVING SUM(monto_en_acciones) > 0
             
                 IF lr_saldos.siefore = 11 THEN 
                    SELECT precio_del_dia
                    INTO v_precio_del_dia
                    FROM glo_valor_accion
                    WHERE codigo_siefore = lr_saldos.siefore
                    AND fecha_valuacion = v_fecha_saldo_viv
                 ELSE
                    SELECT precio_del_dia
                    INTO v_precio_del_dia
                    FROM glo_valor_accion
                    WHERE codigo_siefore = lr_saldos.siefore
                    AND fecha_valuacion = HOY
                 END IF
             
                 LET lr_saldos.monto_pesos = lr_saldos.monto_acciones * v_precio_del_dia
             
                 IF lr_saldos.monto_acciones > 0 AND lr_saldos.monto_pesos > 0 THEN
                     INSERT INTO ret_saldo_benef(nss,fecha_saldo,subcuenta,siefore,monto_acc,monto_pes)
                        VALUES(v_nss,HOY,lr_saldos.subcuenta,lr_saldos.siefore,lr_saldos.monto_acciones,lr_saldos.monto_pesos)
                 END IF
                   
             END IF
             INITIALIZE lr_saldos.* TO NULL
         END FOR
      ELSE
         --Actualiza estado de solicitud hijo a transición
         UPDATE ret_solicitud_tx
         SET estado_solicitud = gr_edo.transicion --ESTADO TRANSICIÓN
         WHERE nss            = v_nss
         AND consecutivo      = v_consecutivo
         AND folio_solicitud  = v_folio_sol
         AND estado_solicitud IN (gr_edo.confirmado, gr_edo.modificado)
      END IF

   END FOREACH

END FUNCTION

FUNCTION fn_verif_saldo_benef(p_nss)

   DEFINE p_nss               CHAR(11)
   DEFINE v_fecha_saldo       DATE
   DEFINE v_subcuenta         SMALLINT
   
    DECLARE cur_verif_saldo CURSOR FOR
        SELECT UNIQUE fecha_saldo, subcuenta
          FROM ret_saldo_benef
         WHERE nss = p_nss
         ORDER BY 1 DESC, 2 ASC

    INITIALIZE v_fecha_saldo, v_subcuenta TO NULL
    -- Iniciamos ciclo para cada fecha
    FOREACH cur_verif_saldo INTO v_fecha_saldo, v_subcuenta
        
         SELECT "OK"
           FROM dis_provision dp,
                tmp_dis_cuenta dis
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = dis.nss
            AND dp.consecutivo_lote = dis.consecutivo_lote
            AND dp.subcuenta        = dis.subcuenta
         UNION 
         SELECT "OK"
           FROM dis_provision dp,
                ret_solicitud_tx rst
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = rst.nss
            AND dp.consecutivo_lote = rst.consecutivo
            AND (rst.estado_solicitud = gr_edo.enviado
                 AND (rst.diag_registro = '0' OR rst.diag_registro IS NULL OR rst.diag_registro = "   "))
            AND rst.tipo_retiro        = gc_tipo_ret
          GROUP BY 1
         
         IF STATUS = NOTFOUND THEN
             DELETE
               FROM ret_saldo_benef 
              WHERE nss = p_nss
                AND fecha_saldo = v_fecha_saldo
                AND subcuenta   = v_subcuenta
         END IF
        
        INITIALIZE v_fecha_saldo, v_subcuenta TO NULL
    END FOREACH

END FUNCTION

#=============================================================================#
# Funcion  : genera_tmp_dis_cuenta                                            #
# Objetivo : Generar la tabla temporal <tmp_dis_cuenta> con la informacion    #
#            historica de las dis_cuenta solo para el NSS recibido            #
#=============================================================================#
FUNCTION genera_tmp_cuenta(p_nss)

    DEFINE p_nss              CHAR(11)
    DEFINE v_nombre_tabla     CHAR(20)
    DEFINE sel_his            CHAR(1500)

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta;
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
    SELECT tabname
    FROM systables
    WHERE tabname matches "dis_cuenta??"

    FOREACH cur_his INTO v_nombre_tabla

        LET sel_his = sel_his CLIPPED,
                      " SELECT * ",
                      "  FROM ",v_nombre_tabla          ,
                      " WHERE nss = ","'",p_nss,"'"  ,
                      "  UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET sel_his = sel_his CLIPPED,
                  " SELECT * ",
                  "  FROM dis_cuenta ",
                  " WHERE nss = ","'",p_nss,"'"  ,
                  " INTO TEMP tmp_dis_cuenta "

    PREPARE eje_sel_his FROM sel_his

    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta ( folio,
                                                     consecutivo_lote,
                                                     subcuenta,
                                                     siefore
                                                   )
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

#---------------------------------------------------------------------------------#
# primer_paso_benef : Obtiene los registros que seran provisionados y calcula el  #
#               monto a provisionar                                         ------#
#---------------------------------------------------------------------------------#
FUNCTION primer_paso_benef()

    DEFINE lr_solicitud RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_viv97 RECORD
        siefore             SMALLINT        ,
        subcuenta           SMALLINT        ,
        acciones            DECIMAL(16,6)   ,
        pesos               DECIMAL(16,6)
    END RECORD

    #DEFINE arr_siefore ARRAY [20] OF RECORD   #CPL-2106
   DEFINE arr_siefore ARRAY [99] OF RECORD
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_ret92    DECIMAL(16,6) ,
        acciones_viv92    DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE
        ld_fecha_saldo          ,
        ld_fecha_saldo_viv      DATE

    DEFINE
        ls_siefore            , #-- contador para los ciclos for
        f_siefore             ,
        f_subcuenta           ,
        s_subcta              SMALLINT

    DEFINE
        f_monto_pes          ,
        f_monto_acc          ,
        acciones_cv          ,
        s11_acc_viv92        ,
        s11_acc_viv97        DECIMAL(16,6)
        
    DEFINE ls_tipo_benef         SMALLINT
    DEFINE aux_tipo_benef        SMALLINT
    DEFINE ld_porcentaje         DECIMAL(5,2)
    DEFINE ld_fecha_saldo_benef  DATE
    DEFINE ld_consecutivo        DECIMAL(11,0)
    DEFINE lc_curp               LIKE ret_beneficiario.curp_benef
    DEFINE ls_ind                SMALLINT
    DEFINE ls_count_reg          SMALLINT
    DEFINE ls_grupo              SMALLINT
    
    DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pesos          LIKE dis_cuenta.monto_en_pesos
   END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY " PROCESANDO INFORMACION BENEFICIARIOS " AT 19,1 ATTRIBUTE(REVERSE)    

    LET ld_fecha_saldo_viv = HOY
    LET ls_grupo = 0

    DECLARE cur_benef_1 CURSOR FOR
        SELECT A.*
        FROM   ret_solicitud_tx A
        WHERE  A.tipo_retiro      = gc_tipo_ret
        AND    A.estado_solicitud IN (gr_edo.confirmado, gr_edo.modificado,gr_edo.transicion)
        AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
        AND    (A.diag_registro = '0' OR A.diag_registro IS NULL OR A.diag_registro = "   ")
        AND    A.nss IN (SELECT UNIQUE B.nss
                           FROM ret_ctr_benef B
                          WHERE B.consecutivo_solic = A.consecutivo)
        ORDER BY 1

    -- Iniciamos ciclo para cada nss
    FOREACH cur_benef_1 INTO lr_solicitud.*
    
        CALL genera_tmp_cuenta(lr_solicitud.nss)

        DECLARE cur_ctr_benef_1 CURSOR FOR
        SELECT tipo_benef, porcentaje
          FROM ret_ctr_benef
         WHERE nss               = lr_solicitud.nss
           AND consecutivo_solic = lr_solicitud.consecutivo
           AND tipo_retiro       = gc_tipo_ret

        FOREACH cur_ctr_benef_1 INTO ls_tipo_benef,
                                     ld_porcentaje
            
            LET aux_tipo_benef = ls_tipo_benef
            
            IF ls_tipo_benef = 1 THEN
                SELECT "OK"
                  FROM afi_mae_benefici a,
                       afi_ctr_beneficiario b
                 WHERE a.n_seguro       = lr_solicitud.nss
                   AND a.n_folio        = b.n_folio
                   AND a.tipo_solicitud = b.tipo_solicitud
                   AND b.tipo_beneficiario = 2    -- CPL-3664
                 GROUP BY 1
                IF STATUS = NOTFOUND THEN
                    LET aux_tipo_benef = 4
                END IF
            END IF

            INITIALIZE lr_solicitud.grupo TO NULL
            SELECT grupo
              INTO lr_solicitud.grupo
              FROM ret_matriz_benef
             WHERE regimen         = lr_solicitud.regimen
               AND tipo_retiro     = gc_tipo_ret
               AND tipo_prestacion = lr_solicitud.tipo_prestacion
               AND tipo_benef      = aux_tipo_benef
            
            IF (lr_solicitud.grupo IS NULL OR lr_solicitud.grupo = 0) THEN
            	  SELECT "OK"
                  FROM ret_ctr_benef
                 WHERE nss               = lr_solicitud.nss
                   AND consecutivo_solic = lr_solicitud.consecutivo
                   AND tipo_retiro       = gc_tipo_ret
                   AND tipo_benef = 3
                   GROUP BY 1
                
                IF STATUS <> NOTFOUND AND ls_tipo_benef = 1 THEN
                    CONTINUE FOREACH
                END IF
                
            	  UPDATE ret_solicitud_tx
                   SET estado_solicitud    = gr_edo.cancelado,
                       cod_rechazo_ent     = 322,
                       folio               = gi_ult_folio
                 WHERE nss                 = lr_solicitud.nss
                   AND consecutivo         = lr_solicitud.consecutivo
                   AND tipo_retiro         = gc_tipo_ret
                   AND estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado, gr_edo.transicion)
            END IF
            
            DECLARE cur_subct_benef CURSOR FOR
            SELECT subcuenta
            FROM   tab_agrupa_subcta
            WHERE  grupo     = lr_solicitud.grupo
            AND    subcuenta > 0
            
            -- Iniciamos ciclo para cada subcuenta del grupo del nss actual
            FOREACH cur_subct_benef INTO s_subcta
            
                LET ls_ind       = 0
                LET f_subcuenta  = 0
                LET f_siefore    = 0
                LET f_monto_acc  = 0
                LET f_monto_pes  = 0
                LET v_provision  = "SI"
            
                IF lr_solicitud.tipo_pension = "IP" THEN
                    IF (s_subcta <> 1) OR (s_subcta = 1 AND lr_solicitud.regimen = 97) THEN
                        IF lr_solicitud.porcentaje_val < 50  THEN
                            LET v_provision = "NO"
                        END IF
                    END IF
                END IF
            
                IF v_provision = "SI" THEN
            
                    DECLARE cur_saldo_benef CURSOR FOR
                    SELECT siefore, subcuenta, monto_acc, monto_pes, fecha_saldo
                      FROM ret_saldo_benef
                     WHERE nss       = lr_solicitud.nss
                       AND subcuenta = s_subcta
                     ORDER BY fecha_saldo ASC
                       
                    FOREACH cur_saldo_benef INTO f_siefore,
                                                 f_subcuenta,
                                                 f_monto_acc,
                                                 f_monto_pes,
                                                 ld_fecha_saldo_benef
                    
                        SELECT "OK"
                          FROM dis_provision dp,
                               tmp_dis_cuenta dc,
                               ret_ctr_benef rcb
                         WHERE dp.nss                = lr_solicitud.nss
                           AND dp.subcuenta          = f_subcuenta
                           AND dp.fecha_conversion   = ld_fecha_saldo_benef
                           AND dp.nss                = dc.nss
                           AND dp.consecutivo_lote   = dc.consecutivo_lote
                           AND dp.subcuenta          = dc.subcuenta
                           AND rcb.nss               = dp.nss
                           AND rcb.consecutivo_solic = dp.consecutivo_lote
                           AND rcb.porcentaje = 100
                         GROUP BY 1
            
                        IF STATUS <> NOTFOUND THEN
                        	  LET ls_ind = 1
                        	  CONTINUE FOREACH
                        END IF
            
                        SELECT curp_benef
                          INTO lc_curp
                          FROM ret_beneficiario
                         WHERE nss         = lr_solicitud.nss
                           AND consecutivo = lr_solicitud.consecutivo
            
                        DECLARE cur_busca_cons_benef CURSOR FOR
                         SELECT UNIQUE a.consecutivo
                           FROM ret_beneficiario a,
				                        ret_solicitud_tx b,
				                        ret_ctr_benef c
                          WHERE a.nss                = lr_solicitud.nss
                            AND a.curp_benef         = lc_curp
					                  AND a.nss              = b.nss
					                  AND a.consecutivo      = b.consecutivo
					                  AND a.nss              = c.nss
					                  AND a.consecutivo      = c.consecutivo_solic
					                  AND (b.estado_solicitud IN (gr_edo.confirmado,
					                                             gr_edo.modificado,
					                                             gr_edo.transicion,
					                                             gr_edo.liquidado)
					                   OR (b.estado_solicitud = gr_edo.enviado
                                 AND (b.diag_registro = '0' OR b.diag_registro IS NULL OR b.diag_registro = "   ")))
					                  AND c.tipo_benef       = ls_tipo_benef
					                  AND c.tipo_retiro      = gc_tipo_ret
					                ORDER BY a.consecutivo ASC
                        
                        LET ld_consecutivo = 0
            
                        FOREACH cur_busca_cons_benef INTO ld_consecutivo
                        
                            IF ld_consecutivo <> lr_solicitud.consecutivo AND ls_ind = 1 THEN
                            	  CONTINUE FOREACH
                            END IF
                        
                            SELECT "OK"
                              FROM dis_provision dp,
                                   tmp_dis_cuenta dc
                             WHERE dp.nss              = lr_solicitud.nss
                               AND dp.consecutivo_lote = ld_consecutivo
                               AND dp.subcuenta        = f_subcuenta
                               AND dp.fecha_conversion = ld_fecha_saldo_benef
                               AND dp.nss              = dc.nss
                               AND dp.consecutivo_lote = dc.consecutivo_lote
                               AND dp.subcuenta        = dc.subcuenta
                             GROUP BY 1
            
                            IF STATUS = NOTFOUND THEN
                        
                                LET ls_count_reg = 0
                                SELECT COUNT(*)
                                  INTO ls_count_reg
                                  FROM ret_saldo_benef
                                 WHERE nss = lr_solicitud.nss
                                   AND subcuenta = f_subcuenta
                                   
                            	  IF ld_consecutivo <> lr_solicitud.consecutivo AND ls_count_reg = 1 THEN
                                	  CONTINUE FOREACH
                                END IF
                                IF f_monto_acc > 0 AND f_monto_pes > 0 THEN
         
                                    LET f_monto_acc = f_monto_acc * (ld_porcentaje / 100)
                        
                                    INITIALIZE lr_saldos.* TO NULL
                                    DECLARE cur_saldo_dia_2 CURSOR FOR eje_saldo_dia
                                    OPEN cur_saldo_dia_2 USING lr_solicitud.nss,
                                                               f_subcuenta,
                                                               ls_grupo,
                                                               HOY
                        
                                    FETCH cur_saldo_dia_2 INTO lr_saldos.*
                                    CLOSE cur_saldo_dia_2
                            
                                    IF lr_saldos.monto_acciones IS NULL THEN
                                    	   LET lr_saldos.monto_acciones = 0
                                    END IF
                        
                                    IF f_monto_acc > lr_saldos.monto_acciones THEN
                                    	  LET f_monto_acc = lr_saldos.monto_acciones
                                    END IF
                                    
                                    LET f_monto_pes = f_monto_acc * gar_precio_acc[f_siefore].precio_dia
            
                                    IF f_subcuenta = 8 OR f_subcuenta = 4 THEN
                                        LET f_monto_pes = f_lib_redondea_val(f_monto_pes, 2)
                                    END IF
                                
                                    SELECT "OK"
                                      FROM ret_ctr_benef_det
                                     WHERE nss               = lr_solicitud.nss
                                       AND consecutivo_solic = lr_solicitud.consecutivo
                                       AND folio_liquida     = gi_ult_folio
                                       AND subcuenta         = f_subcuenta
                                       
                                    IF STATUS = NOTFOUND THEN
            
                                        INSERT INTO ret_ctr_benef_det VALUES(lr_solicitud.nss           ,
                                                                             lr_solicitud.consecutivo   ,
                                                                             gi_ult_folio               ,
                                                                             f_subcuenta                ,
                                                                             f_monto_pes                ,
                                                                             f_monto_acc                ,
                                                                             NULL                       ,
                                                                             NULL                       ,
                                                                             f_siefore                  ,
                                                                             NULL                       ,
                                                                             NULL
                                                                             )
                                    ELSE
                                        UPDATE ret_ctr_benef_det 
                                           SET monto_total     = monto_total    + f_monto_pes,
                                               acciones_bruto  = acciones_bruto + f_monto_acc
                                         WHERE nss             = lr_solicitud.nss
                                         AND consecutivo_solic = lr_solicitud.consecutivo
                                         AND folio_liquida     = gi_ult_folio
                                         AND subcuenta         = f_subcuenta
                                    END IF
                                END IF
                            ELSE
                                LET ls_ind = 1
                                EXIT FOREACH
                            END IF
                        END FOREACH
                    END FOREACH
                END IF -- si provisiono
            END FOREACH -- Subcuentas
        END FOREACH -- Benef
    END FOREACH -- Siguiente NSS


    DECLARE cur_benef_2 CURSOR FOR
        SELECT A.*
        FROM   ret_solicitud_tx A
        WHERE  A.tipo_retiro      = gc_tipo_ret
        AND    A.estado_solicitud IN (gr_edo.confirmado, gr_edo.modificado)
        AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
        AND    (A.diag_registro = '0' OR A.diag_registro IS NULL OR A.diag_registro = "   ")
        AND    A.nss IN (SELECT UNIQUE B.nss
                           FROM ret_ctr_benef B
                          WHERE B.consecutivo_padre = A.consecutivo)
        ORDER BY 1

    -- Iniciamos ciclo para cada nss
    FOREACH cur_benef_2 INTO lr_solicitud.*

        -- Inicializamos variables del arreglo
        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo             = FALSE
            LET arr_siefore[ls_siefore].acciones_ret97     = 0
            LET arr_siefore[ls_siefore].acciones_cv        = 0
            LET arr_siefore[ls_siefore].acciones_cs        = 0
            LET arr_siefore[ls_siefore].acciones_est       = 0
            LET arr_siefore[ls_siefore].acciones_ret92     = 0
            LET arr_siefore[ls_siefore].acciones_viv92     = 0
            LET arr_siefore[ls_siefore].acciones_esp       = 0
        END FOR

        LET s11_acc_viv92 = 0
        LET s11_acc_viv97 = 0

        LET f_subcuenta  = 0
        LET f_siefore    = 0
        LET f_monto_acc  = 0
        LET f_monto_pes  = 0
    
        DECLARE cur_subct_benef_2 CURSOR FOR
        SELECT siefore, subcuenta, SUM(acciones_bruto), SUM(monto_total)
          FROM ret_ctr_benef_det
         WHERE nss               = lr_solicitud.nss
           AND consecutivo_solic = lr_solicitud.consecutivo
           AND folio_liquida     = gi_ult_folio
         GROUP BY 1,2

        FOREACH cur_subct_benef_2 INTO f_siefore,
                                       f_subcuenta,
                                       f_monto_acc,
                                       f_monto_pes
        
            IF f_monto_acc > 0 AND f_monto_pes > 0 THEN
                IF f_siefore <> 11 THEN
                    #-- Marcamos como activo el registro de la siefore actual
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
                        WHEN 8
                            LET arr_siefore[f_siefore].acciones_viv92 = f_monto_acc
                        WHEN 9
                            LET arr_siefore[f_siefore].acciones_esp   = f_monto_acc
                    END CASE
                ELSE
                    IF f_subcuenta = 8 THEN
                        LET s11_acc_viv92   = f_monto_acc
                    END IF
                END IF
                    
                IF f_subcuenta <> 4 THEN
                    CALL f_provisiona_subcta(lr_solicitud.nss           ,
                                             f_siefore                  ,
                                             f_subcuenta                ,
                                             lr_solicitud.consecutivo   ,
                                             f_monto_acc                ,
                                             f_monto_pes                )
                END IF
            END IF
        END FOREACH 

        INITIALIZE lr_viv97.* TO NULL
        LET s11_acc_viv97       = 0
        LET lr_viv97.acciones   = 0

        -- Si se le indico al programa no considerar el pago de vivienda (Estado = 33)
        -- No debe entrar a verificar la vivienda 97 en ningun caso
        IF lr_solicitud.estado_solicitud <> gr_edo.modificado THEN
            IF ( (lr_solicitud.regimen = 73) AND (lr_solicitud.fecha_resolucion > gdt_cambio_infonavit))
            THEN
                SELECT "OK"
                FROM   ret_notifica_vivienda
                WHERE  nss              = lr_solicitud.nss
                AND    consecutivo      = lr_solicitud.consecutivo
                AND    grupo_trabajador = "0101"
                AND    indicador        = "A"
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    CALL f_obten_saldo_subcta(lr_solicitud.nss, 4, ld_fecha_saldo_viv)
                        RETURNING lr_viv97.*

                    LET s11_acc_viv97   = lr_viv97.acciones

                    IF s11_acc_viv97 > 0 AND lr_viv97.pesos THEN
                        CALL f_provisiona_subcta(lr_solicitud.nss           ,
                                                 lr_viv97.siefore           ,
                                                 lr_viv97.subcuenta         ,
                                                 lr_solicitud.consecutivo   ,
                                                 lr_viv97.acciones          ,
                                                 lr_viv97.pesos
                                                )
                    END IF

                    UPDATE dis_provision
                    SET    tipo_movimiento  = gs_mov_viv97
                    WHERE  nss              = lr_solicitud.nss
                    AND    folio            = gi_ult_folio
                    AND    consecutivo_lote = lr_solicitud.consecutivo
                    AND    subcuenta       IN (4,8)
                    AND    tipo_movimiento  = gs_tipo_movimiento
                ELSE
                    LET s11_acc_viv97       = 0
                    LET lr_viv97.acciones   = 0
                END IF
            ELSE
                LET s11_acc_viv97       = 0
                LET lr_viv97.acciones   = 0
            END IF -- Provision de la subcuenta 4
        END IF -- Estado modificado

        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET acciones_cv = 0
            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                IF arr_siefore[ls_siefore].acciones_ret97 < 0 THEN
                    LET arr_siefore[ls_siefore].acciones_ret97 = 0
                END IF

                LET acciones_cv = arr_siefore[ls_siefore].acciones_cv  +
                                  arr_siefore[ls_siefore].acciones_est +
                                  arr_siefore[ls_siefore].acciones_esp

                IF arr_siefore[ls_siefore].acciones_ret92 < 0 THEN
                    LET arr_siefore[ls_siefore].acciones_ret92 = 0
                END IF

                INSERT INTO ret_monto_siefore
                    VALUES(lr_solicitud.nss                         ,--nss
                           lr_solicitud.consecutivo                 ,--consecutivo
                           gi_ult_folio                             ,--folio
                           gc_tipo_ret                              ,--tipo_retiro
                           5                                        ,--tipo_operacion
                           ls_siefore                               ,--siefore
                           arr_siefore[ls_siefore].acciones_ret97   ,--acciones_ret97
                           acciones_cv                              ,--acciones_cv
                           arr_siefore[ls_siefore].acciones_cs      ,--acciones_cs
                           arr_siefore[ls_siefore].acciones_ret92    --acciones_ret92
                          )
            END IF
        END FOR

        INSERT INTO ret_monto_viv
        VALUES(lr_solicitud.nss         ,   -- nss
               lr_solicitud.consecutivo ,   -- consecutivo
               gi_ult_folio             ,   -- folio
               gc_tipo_ret              ,   -- tipo_retiro
               gd_fecha_viv             ,   -- fecha_valor_viv
               s11_acc_viv97            ,   -- acciones_viv97
               s11_acc_viv92            ,   -- acciones_viv92
               0                        ,   -- pesos_viv72
               NULL                     ,   -- estado_sub_viv
               0                        ,   -- acc_viv97_bdsviv
               0                            -- acc_viv92_bdsviv
              )

        --LET s11_acc_viv97   = lr_viv97.acciones

        UPDATE ret_solicitud_tx
        SET    folio               = gi_ult_folio    ,
               saldo_viv97         = s11_acc_viv97   ,
               saldo_viv72         = 0               ,
               saldo_viv92         = s11_acc_viv92   ,
               fecha_envio         = HOY             ,
               fecha_valor_viv     = gd_fecha_viv    ,
               estado_solicitud    = gr_edo.procesado
        WHERE  nss                 = lr_solicitud.nss
        AND    consecutivo         = lr_solicitud.consecutivo
        AND    estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado)
        
        UPDATE ret_ctr_benef 
           SET folio_liquida     = gi_ult_folio
         WHERE nss               = lr_solicitud.nss
           AND consecutivo_solic = lr_solicitud.consecutivo
           AND tipo_retiro       = gc_tipo_ret

    END FOREACH -- Siguiente NSS

    CALL fn_desmarca_cuenta()

END FUNCTION

FUNCTION fn_valida_benef(p_nss, p_consecutivo, p_tipo_benef)

    DEFINE p_nss           LIKE ret_solicitud_tx.nss
    DEFINE p_consecutivo   LIKE ret_solicitud_tx.consecutivo
    DEFINE p_tipo_benef    SMALLINT

    DEFINE lc_curp         LIKE ret_beneficiario.curp_benef
    DEFINE ls_indicador    SMALLINT
    DEFINE ls_porcentaje   SMALLINT

    LET ls_indicador  = 0
    LET ls_porcentaje = 0

    IF p_tipo_benef = 3 THEN
        SELECT curp_benef
          INTO lc_curp
          FROM ret_beneficiario
         WHERE nss         = p_nss
           AND consecutivo = p_consecutivo

        SELECT "OK"
          FROM afi_mae_benefici a,
               afi_ctr_beneficiario b
         WHERE a.n_seguro       = p_nss
           AND a.curp           = lc_curp
           AND a.n_folio        = b.n_folio
           AND a.tipo_solicitud = b.tipo_solicitud
           AND b.tipo_beneficiario = 2    -- CPL-3664
           GROUP BY 1

        IF STATUS = NOTFOUND THEN
            UPDATE ret_solicitud_tx
               SET estado_solicitud    = gr_edo.cancelado,
                   cod_rechazo_ent     = 320,
                   folio               = gi_ult_folio
             WHERE nss                 = p_nss
               AND estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado, gr_edo.transicion)

            LET ls_indicador = 1
        END IF
    END IF

--    SELECT SUM(porcentaje)
--      INTO ls_porcentaje
--      FROM ret_ctr_benef
--     WHERE nss           = p_nss
--       AND tipo_benef    = p_tipo_benef
--       AND folio_liquida IS NULL
--       AND tipo_retiro   = gc_tipo_ret
    SELECT SUM(porcentaje)
      INTO ls_porcentaje
      FROM ret_ctr_benef a,
           ret_solicitud_tx b
     WHERE a.nss = b.nss
       AND a.consecutivo_solic = b.consecutivo
       AND a.nss           = p_nss
       AND a.tipo_benef    = p_tipo_benef
       AND a.folio_liquida IS NULL
       AND a.tipo_retiro   = gc_tipo_ret
       AND b.estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado, gr_edo.transicion)

    IF (ls_porcentaje > 100) THEN
        UPDATE ret_solicitud_tx
           SET estado_solicitud    = gr_edo.cancelado,
               cod_rechazo_ent     = 321,
               folio               = gi_ult_folio
         WHERE nss                 = p_nss
           AND estado_solicitud    IN (gr_edo.confirmado, gr_edo.modificado, gr_edo.transicion)

        LET ls_indicador = 1
    END IF

    RETURN ls_indicador

END FUNCTION

FUNCTION fn_desmarca_cuenta()

    DEFINE lr_desmarca         RECORD
           nss                 LIKE cta_his_marca.nss          ,
           movimiento          LIKE cta_his_marca.marca_cod    ,
           consecutivo         LIKE cta_his_marca.correlativo  ,
           edo_causa           LIKE cta_his_marca.estado_marca ,
           marca_causa         LIKE cta_his_marca.marca_causa
           END RECORD
    
    DECLARE cur_desmarca CURSOR FOR
        SELECT A.nss                ,
               A.marca_cod          ,
               A.correlativo        ,
               0                    ,
               0
        FROM   cta_his_marca A      ,
               ret_solicitud_tx B
        WHERE  A.correlativo        = B.consecutivo
        AND    A.nss                = B.nss
        AND    A.marca_cod          = gs_tipo_movimiento
        AND    B.tipo_retiro        = gc_tipo_ret
        AND    B.folio              = gi_ult_folio
        AND    A.fecha_fin IS NULL
        AND    B.estado_solicitud   = gr_edo.cancelado
        ORDER BY 1
       
    LET cont_reg_rech = 0

    FOREACH cur_desmarca INTO lr_desmarca.*
    
        EXECUTE eje_desmarca USING lr_desmarca.*, gc_usuario
        
        LET cont_reg_rech = cont_reg_rech + 1
        
    END FOREACH

END FUNCTION


FUNCTION segundo_paso()
#sp-------------------

    DEFINE #loc #reg_6
        reg_6                 RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_benef RECORD LIKE ret_beneficiario.*

    DEFINE
        ls_num_montos   SMALLINT

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_viv      CHAR(100)

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

    INITIALIZE lr_benef.* TO NULL

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = gr_param_ret.ruta_envio clipped, "/", "DET-NSS-E-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = gr_param_ret.ruta_envio clipped, "/", "DET-SIE-E-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = gr_param_ret.ruta_envio clipped, "/", "DET-VIV-E-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET  = gr_param_ret.ruta_envio clipped, "/", "DET-E"
    LET G_LISTA_DET  = G_LISTA_DET CLIPPED

    LET sw_1     = 0
    LET cont_reg = 0

    DECLARE cur_4 CURSOR FOR
        SELECT  A.*
        FROM    ret_solicitud_tx A
        WHERE   A.folio              = gi_ult_folio
        AND     A.estado_solicitud   = gr_edo.procesado
        AND     A.tipo_retiro        = gc_tipo_ret

    #-- Barremos los registros a generar
    FOREACH cur_4 INTO reg_6.*

        #-- Iniciamos los reportes
        START REPORT det_solicitudes_03 TO ruta_det_nss
        START REPORT det_vivienda_05 TO ruta_det_viv

        LET sw_1          = 1
        LET cont_reg      = cont_reg + 1
        LET ls_num_montos = 0

        IF reg_6.porcentaje_val IS NULL THEN
            LET reg_6.porcentaje_val = 0
        END IF

        SELECT *
        INTO   lr_benef.*
        FROM   ret_beneficiario
        WHERE  nss              = reg_6.nss
        AND    consecutivo      = reg_6.consecutivo
        AND    consec_benef     = 1

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

        OUTPUT TO REPORT det_solicitudes_03(reg_6.*, lr_benef.*, lr_info_biometrica.*)
        OUTPUT TO REPORT det_vivienda_05(reg_6.*)


        SELECT COUNT(*)
            INTO   ls_num_montos
            FROM   ret_monto_siefore
            WHERE  nss            = reg_6.nss
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
                WHERE  nss            = reg_6.nss
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
        CALL concat_reportes(ruta_det_nss  ,
                             ruta_det_sie  ,
                             ruta_det_viv  ,
                             ls_num_montos ,
                             cont_reg)
     END FOREACH

    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando

    IF sw_1 = 1 THEN
        INSERT INTO ret_ctr_envio_lote
        VALUES (HOY                 ,--fecha_genera
                gc_tipo_ret         ,--tipo_retiro
                gi_ult_folio        ,--folio
                " "                 ,--fecha_envio
                " "                 ,--fecha_reverso
                HORA                ,--hora_genera
                " "                 ,--hora_envio
                gc_usuario          ,--usuario_genera
                " "                 ,--usuario_envio
                " "                 ,--usuario_reverso
                gr_edo.procesado     ,--estado
                cont_reg             --total_registros
                )
    END IF

    DISPLAY "TOTAL DE REGISTROS A ENVIAR     : ",cont_reg         AT 11,19
    DISPLAY "TOTAL DE REGISTROS RECHAZADOS   : ",cont_reg_rech    AT 12,19

    DISPLAY " FOLIO NUMERO : ",gi_ult_folio  AT 18,1
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter

END FUNCTION #-- Segundo Paso

#----------------------------------------------------------------------------------
# Esta funcion, dadas las rutas de los tres archivos de detalle temporales de los
# reportes, los va concatenando en uno solo que sera el archivo de detalle final.
#----------------------------------------------------------------------------------
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_5, p_monto, p_regs)

    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_5      CHAR(100)

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

    LET ruta_tmp    = gr_param_ret.ruta_envio clipped, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = gr_param_ret.ruta_envio clipped, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ", lc_det_5 CLIPPED

    #-- Si se genero el reporte de saldo de siefores, se incluye en los comandos
    IF p_monto > 0 THEN

        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_4 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp

        LET com_cat = com_cat CLIPPED

        LET com_rm = com_rm CLIPPED, " ", lc_det_4 CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp CLIPPED
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
                        gs_tipo_movimiento  ,--tipo_movimiento
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

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se captura los datos para generar  #
#                  la consulta de rechazos                                  #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc8161 AT 4,4 WITH FORM "RETC8161" ATTRIBUTE(BORDER)
    DISPLAY "                                                            TIPO RETIRO E    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC816     NOTIFICA Y PROVISIONA DISPOSICION DE RECURSOS                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Valida si existen los precios de accion de la  #
#                            fecha indicada y los almacena para su posible  #
#                            uso en el programa                             #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE
        pdt_fec_precios         DATE

    DEFINE lr_precio_acc RECORD
        estado              SMALLINT     ,
        fecha               DATE         ,
        siefore             SMALLINT     ,
        precio_dia          DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100)   ,
        lc_mensaje              CHAR(100)   ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_precios_acc
    DECLARE c_precios CURSOR FOR eje_precios_accion

    FOREACH c_precios USING pdt_fec_precios
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


REPORT det_solicitudes_03(reg_7, pr_benef, pr_info_biometrica)
#ds03----------------------------
    DEFINE #loc #reg_7
        reg_7                 RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_folio_infonavit RECORD LIKE ret_folio_infonavit.*

    DEFINE pr_benef RECORD LIKE ret_beneficiario.*

    DEFINE pr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    DEFINE reg_8 RECORD  #loc #reg_8
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        nombres               LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE #loc #char
        lc_grupo_trabajador     CHAR(02)    ,
        lc_rfc                  CHAR(13)    ,
        lc_curp_benef           CHAR(18)    ,
        lc_cta_clabe            CHAR(18)    ,
        lc_tipo_vent            CHAR(04)    ,
        lc_folio_infonavit      CHAR(14)    ,
        v_sec_pension           CHAR(02)    ,
        vmax_sec_pension        CHAR(02)    ,
        c6_porc_val             CHAR(06)    ,
        c5_porc_val             CHAR(05)

    DEFINE #loc #integer
        vmax_folio          INTEGER

    DEFINE
        ls_id_benef         SMALLINT

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            INITIALIZE lr_folio_infonavit.* TO NULL
            
            INITIALIZE
                reg_8.*                 ,
                lc_folio_infonavit      ,
                lc_tipo_vent            ,
                ls_id_benef             ,
                lc_cta_clabe            ,
                lc_grupo_trabajador     TO NULL

            LET lc_folio_infonavit      = 14 SPACES
            LET ls_id_benef             =  1 SPACES
            LET lc_cta_clabe            = 18 SPACES
            LET lc_grupo_trabajador     =  2 SPACES

            SELECT paterno   ,
                   materno   ,
                   nombres
            INTO   reg_8.paterno ,
                   reg_8.materno ,
                   reg_8.nombres
            FROM   afi_mae_afiliado
            WHERE  n_seguro  = reg_7.nss

            #-- Obtenemos el porcentaje de valuacion
            LET c6_porc_val        = reg_7.porcentaje_val USING "&&&.&&"
            LET c5_porc_val        = c6_porc_val[1,3],
                                     c6_porc_val[5,6]

            SELECT folio_infonavit    ,
                   cta_clabe
            INTO   lc_folio_infonavit ,
                   lc_cta_clabe
            FROM   afop_fto_conformidad
            WHERE  nss              = reg_7.nss
            AND    folio_solicitud  = reg_7.folio_solicitud

            IF lc_folio_infonavit IS NULL THEN
                LET lc_folio_infonavit = "00000000000000"
            END IF

            SELECT grupo_trabajador ,
                   id_benef
            INTO   lc_tipo_vent,
                   ls_id_benef
            FROM   ret_notifica_vivienda
            WHERE  nss          = reg_7.nss
            AND    consecutivo  = reg_7.consecutivo

            IF lc_cta_clabe IS NULL THEN
                LET lc_cta_clabe    = 18 SPACES
            END IF

            IF lc_tipo_vent IS NULL THEN
                LET lc_tipo_vent = "0201"
            ELSE

            END IF

            LET lc_grupo_trabajador = "01"

            -- Si el tipo de parentesco es 12 = titular entonces el tipo es 2 = trabajador
            IF (pr_benef.paren_cod = 12) THEN
                LET ls_id_benef = 2
            ELSE
                LET ls_id_benef = 1
            END IF

            IF ( (pr_benef.curp_benef IS NULL) OR (pr_benef.curp_benef = "                  ") ) THEN
                LET lc_curp_benef = reg_7.curp
            ELSE
                LET lc_curp_benef = pr_benef.curp_benef
            END IF

            -- Validamos que exista el campo RFC
            IF ( (pr_benef.rfc_benef IS NULL) OR (pr_benef.rfc_benef = "             ") ) THEN
                SELECT n_rfc
                INTO   pr_benef.rfc_benef
                FROM   afi_mae_afiliado
                WHERE  n_seguro = pr_benef.nss
            END IF

            -- Si el RFC no viene a 13 posiciones, se rellenan las faltantes
            -- con ceros
            LET lc_rfc = pr_benef.rfc_benef CLIPPED

            IF ( (LENGTH(lc_rfc) = 10) OR
                 ((lc_rfc[11] = " ") AND (lc_rfc[12] = " ") AND (lc_rfc[13] = " "))
               )
            THEN
                LET lc_rfc = lc_rfc[1,10], "000"
            END IF

            -- Inician cambios CPL-3422           
            SELECT *
            INTO   lr_folio_infonavit.*
            FROM   ret_folio_infonavit
            WHERE  nss          = reg_7.nss
            AND    consecutivo  = reg_7.consecutivo

            -- Los campos correspondientes al layout de las ventanillas solo deben mostrarse
            -- para los retiros E, regimen 73 y fecha de resolucion mayor al 12/01/2012
            IF (NOT( (reg_7.tipo_retiro = "E") AND
                    (reg_7.regimen = 73) AND
                    (reg_7.fecha_resolucion > gdt_cambio_infonavit)
                  ))
                  OR ( (reg_7.tipo_retiro = "E") AND (reg_7.saldo_viv97 + reg_7.saldo_viv92) = 0
                      AND (lc_tipo_vent = "0201") )   #CPL-2020   CPL-2036
            THEN
                LET ls_id_benef                         =  1 SPACES
                LET lc_cta_clabe                        = 18 SPACES
                LET lc_curp_benef                       = 18 SPACES
                LET pr_benef.paterno                    = 40 SPACES
                LET pr_benef.materno                    = 40 SPACES
                LET pr_benef.nombres                    = 40 SPACES
                LET lc_rfc                              = 13 SPACES
                LET lc_folio_infonavit                  = 14 SPACES
                LET lc_tipo_vent                        =  4 SPACES
                LET lc_grupo_trabajador                 =  2 SPACES
            END IF

            IF reg_7.tipo_retiro = "E" AND lc_tipo_vent = "0101" THEN   #CPL-2036
                LET lc_cta_clabe                        = 18 SPACES
                LET lc_folio_infonavit                  = 14 SPACES

                IF pr_info_biometrica.idsolicitante IS NULL THEN
                   CASE ls_id_benef
                       WHEN 2
                           LET pr_info_biometrica.idsolicitante = 1
                       WHEN 1
                           LET pr_info_biometrica.idsolicitante = 2
                   END CASE
                END IF
             END IF
             

            IF (lr_folio_infonavit.tipo_ventanilla = "0202") THEN
                LET ls_id_benef                         =  1 SPACES
                LET lc_cta_clabe                        = 18 SPACES
                LET lc_curp_benef                       = 18 SPACES
                LET pr_benef.paterno                    = 40 SPACES
                LET pr_benef.materno                    = 40 SPACES
                LET pr_benef.nombres                    = 40 SPACES
                LET lc_rfc                              = 13 SPACES
                LET lc_folio_infonavit                  = 14 SPACES
                LET lc_grupo_trabajador                 =  2 SPACES                  
                LET lc_tipo_vent = lr_folio_infonavit.tipo_ventanilla
            END IF

            IF (reg_7.nss[1] = 'I')THEN
                LET reg_7.nss = '00000000000'
            END IF

        PRINT
            COLUMN 001, "03"                                    ,# Tipo de registro
            COLUMN 003, "04"                                    ,# ident_servicio
            COLUMN 005, "05"                                    ,# ident_operacion
            COLUMN 007, reg_7.nss                               ,
            COLUMN 018, reg_7.curp                              ,
            COLUMN 036, reg_8.nombres                           ,
            COLUMN 076, reg_8.paterno                           ,
            COLUMN 116, reg_8.materno                           ,
            COLUMN 156, reg_7.sec_pension USING "&&"            ,
            COLUMN 158, reg_7.tipo_retiro                       ,
            COLUMN 159, reg_7.regimen                           ,
            COLUMN 161, reg_7.tipo_seguro                       ,
            COLUMN 163, reg_7.tipo_pension                      ,
            COLUMN 165, reg_7.tipo_prestacion USING "&&"        ,
            COLUMN 167, reg_7.fecha_ini_pen USING"YYYYMMDD"     ,# Fecha inicio pension
            COLUMN 175, reg_7.fecha_resolucion USING"YYYYMMDD"  ,# Fecha resolucion
            COLUMN 183, c5_porc_val                             ,# Porcentaje_val
            COLUMN 188, reg_7.semanas_cotizadas USING"&&&&"     ,# Semanas cotizada
            COLUMN 192, reg_7.fecha_solicitud USING "YYYYMMDD"  ,
            #--                                                    Clave de docto probatorio
            COLUMN 201, "00010101"                              ,# Fecha de nacimiento
            #--                                                    Aseguradora
            #--                                                    Actuario autorizado
            #--                                                    Num de registro del PPP
            COLUMN 227, "000000"                                ,# Fecha periodo pago reingreso
            COLUMN 233, reg_7.consecutivo USING"&&&&&&&&&&&"    ,
            
            #CPL-2430
            COLUMN 244, pr_info_biometrica.idsolicitante          , --ID Solicitante
            COLUMN 246, pr_info_biometrica.curpsolicitante        , --CURP Solicitante
            COLUMN 264, pr_info_biometrica.sellotrabajador        , --Sello único de verificación
            COLUMN 278, pr_info_biometrica.curpagenteservicio     , --CURP Agente de Servicio
            --COLUMN 296, 485 SPACES                                  --Campos 31 a 48 Filler LAYOUT(780 posiciones)
            
            --COLUMN 244, pr_info_biometrica.sellovoluntad         , -- Sello de Voluntad (Info Biometrica)
            --COLUMN 258, pr_info_biometrica.curpagenteservicio    , -- Curp Agente Servicio (Info Biometrica)
            --COLUMN 276, pr_info_biometrica.curpfuncionario       , -- Curp Funcionario (Info Biometrica)
            --COLUMN 294, pr_info_biometrica.sellofuncionario      , -- Sello Funcionario (Info Biometrica)
            --COLUMN 308, pr_info_biometrica.idsolicitante USING "&", -- Id solicitante (Info Biometrica)
            ----COLUMN 309, 42 SPACES                              ,# Campos 27 al 32 del layout

            -- Campos adicionales al layout para la operacion 05
            COLUMN 351, ls_id_benef USING "&"                   ,   # id benef
            COLUMN 352, lc_cta_clabe                            ,   # clabe
            COLUMN 370, lc_curp_benef                           ,
            COLUMN 388, pr_benef.paterno                        ,   # paterno
            COLUMN 428, pr_benef.materno                        ,   # materno
            COLUMN 468, pr_benef.nombres                        ,   # nombres
            COLUMN 508, lc_rfc                                  ,   # rfc
            COLUMN 521, lc_folio_infonavit                      ,   # Folio infonavit
            COLUMN 535, lc_tipo_vent                            ,   # tipo de ventanilla
            COLUMN 539, lc_grupo_trabajador                     ,   # grupo
            COLUMN 541, 240 SPACES                                  -- Se modifica para hacer la longitud de acuerdo al cambio de layout

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

            IF (p_nss[1] = 'I')THEN
                LET p_nss = '00000000000'
            END IF

        PRINT
            COLUMN 001, "04"                                   ,-- Tipo de registro
            COLUMN 003, p_nss                                  ,
            COLUMN 014, p_curp                                 ,
            COLUMN 032, reg_11.siefore USING "&&"              ,-- Clave de siefore
            COLUMN 034, c14_acc_ret97                          ,-- Acciones de retiro 97
            COLUMN 048, c14_acc_cv                             ,-- Acciones de CV
            COLUMN 062, c14_acc_cs                             ,-- Acciones de CS
            COLUMN 076, c14_acc_ret92                          ,-- Acciones de retiro 92
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

            IF (reg_7.nss[1] = 'I')THEN
                LET reg_7.nss = '00000000000'
            END IF

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

