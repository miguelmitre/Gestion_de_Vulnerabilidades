################################################################################
# Proyecto          => SISTEMA DE AFORE ( SAFRE )                              #
# Sistema           => RET                                                     #
# Owner             => E.F.P.                                                  #
# Programa RETC840  => INFORMA CALCULO DE ISR PARA SHCP                        #
# Fecha creacion    => 10 DE ABRIL 2006                                        #
# By                => ISAI JIMENEZ ROJAS / FRANCO ESTEBAN ULLOA VIDELA        #
# Fecha actualiza   => 21 DE NOVIEMBRE DEL 2007                                #
################################################################################
DATABASE safre_af
    DEFINE reg_10 RECORD #glo #reg_10
        nss                   CHAR(11),
        consecutivo           INTEGER ,
        fecha_conversion      DATE
    END RECORD
    
    DEFINE
        lr_afi_mae_afiliado   RECORD LIKE afi_mae_afiliado.*
    
    DEFINE #glo #char
        enter                 ,
        g_mensaje             CHAR(80)
    
    DEFINE g_errores              SMALLINT
    DEFINE ultimo_folio           INTEGER
        
MAIN

    DEFINE lr_ret_solicitud_tx   RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_ret_cta_vol        RECORD 
        n_seguro        LIKE ret_cta_vol.n_seguro,
        consecutivo     LIKE ret_cta_vol.consecutivo
    END RECORD

    DEFINE fecha1, fecha2        DATE 
    DEFINE vcont_sol             INTEGER            -- Contador de solicitudes
    DEFINE vcont_vol             INTEGER            -- contador de voluntarias
    DEFINE v_tipo_movimiento     LIKE dis_cuenta.tipo_movimiento
    DEFINE v_suma_sar_isr        LIKE dis_cuenta.monto_en_pesos  --Lo seleccionado
    DEFINE v_suma_sar_neto       LIKE dis_cuenta.monto_en_pesos
    DEFINE v_bruto_sar           LIKE dis_cuenta.monto_en_pesos --(sar_isr+sar_neto)
    DEFINE v_neto_sar_calc       LIKE dis_cuenta.monto_en_pesos  --Lo calculado
    DEFINE v_isr_sar_calc        LIKE dis_cuenta.monto_en_pesos
    DEFINE v_mto_grabable_sar    LIKE dis_cuenta.monto_en_pesos
    DEFINE v_mto_grabable_rcv    LIKE dis_cuenta.monto_en_pesos
    DEFINE v_exento_sar_calc     LIKE dis_cuenta.monto_en_pesos
    DEFINE v_suma_rcv_isr        LIKE dis_cuenta.monto_en_pesos
    DEFINE v_suma_rcv_neto       LIKE dis_cuenta.monto_en_pesos
    DEFINE v_bruto_rcv           LIKE dis_cuenta.monto_en_pesos
    DEFINE v_neto_rcv_calc       LIKE dis_cuenta.monto_en_pesos  --Lo calculado
    DEFINE v_isr_rcv_calc        LIKE dis_cuenta.monto_en_pesos
    DEFINE v_exento_rcv_calc     LIKE dis_cuenta.monto_en_pesos
    DEFINE v_suma_isr            LIKE dis_cuenta.monto_en_pesos
    DEFINE v_suma_neto           LIKE dis_cuenta.monto_en_pesos
    DEFINE v_neto                DECIMAL(16,6)
    DEFINE v_isr                 DECIMAL(16,6)
    DEFINE v_exento              DECIMAL(16,6)
    DEFINE calcular_sar          CHAR(2)
    DEFINE calcular_isr_E        CHAR(2) 
    DEFINE calcular_rcv          CHAR(2)
    DEFINE v_fecha_retiro        DATE
    DEFINE vnom_archivo          CHAR(200)
    DEFINE HOY                   DATE
    DEFINE vopcion               CHAR(1)
    DEFINE vtecla                CHAR(1)
    DEFINE vruta_envio           CHAR(200)
    DEFINE vcomando              CHAR(300)
    DEFINE vtot_unload           INTEGER
    DEFINE usuario               CHAR(8)


    OPTIONS INPUT WRAP,
            PROMPT  LINE LAST -1,
            MESSAGE LINE LAST -2
    
    DEFER INTERRUPT

    CALL STARTLOG("RETC840.log")

    --INICIALIZACIONES
    LET HOY       = TODAY
    LET fecha1    = TODAY
    LET fecha2    = TODAY
    LET g_errores = 0
 
    --DESPLEGADO DE LA VENTANA PARA CAPTURA DE DATOS
    
    OPEN WINDOW wretc614 AT 2,2 WITH FORM "RETC8401" ATTRIBUTE(BORDER)
    DISPLAY " ESC = Aceptar                                                 Ctrl-C = Salir " AT 1,1
    DISPLAY " RETC840              INFORME DETALLADO DE ISR PARA SHCP                      " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,68 ATTRIBUTE(REVERSE)
   
    --CAPTURA DEL PERIODO DE EJECUCION
    
    INPUT BY NAME fecha1, fecha2 WITHOUT DEFAULTS ATTRIBUTE(REVERSE)
        AFTER FIELD fecha1
            IF fecha1  IS NULL THEN
                ERROR " LA FECHA NO PUEDE SER NULA "
                NEXT FIELD fecha1
            END IF
        AFTER FIELD fecha2
            IF fecha2  IS NULL THEN
               ERROR " LA FECHA NO PUEDE SER NULA "
               NEXT FIELD fecha2
            END IF
        
            IF fecha2 < fecha1 THEN
               ERROR " LA FECHA FINAL NO PUEDE SER MAYOR QUE LA INICIAL "
                NEXT FIELD fecha2
            END IF
    END INPUT
    
    IF INT_FLAG THEN 
        ERROR "OPERACION CANCELADA"
        EXIT PROGRAM 
    END IF 
    
    WHILE TRUE
        PROMPT "ESTA SEGURO (S/N):" FOR CHAR vopcion
        IF vopcion MATCHES "[SsNn]" THEN
            IF vopcion MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                ERROR "OPERACION CANCELADA..."
                SLEEP 3
                EXIT PROGRAM
            END IF
        END IF
    END WHILE
    
    MESSAGE "PROCESANDO..." ATTRIBUTE(REVERSE)
    
    
    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF
    	
    INSERT INTO glo_folio VALUES (ultimo_folio)
    
    INSERT INTO ret_ctr_shcp
    VALUES(ultimo_folio ,
           fecha1       ,
           fecha2       ,
           HOY          ,
           usuario
          )
           
         
    -- INVOCA A FUNCION PARA CALCULO DE INTERESES DE VOLUNTARIAS
    CALL calcula_intereses(fecha1, fecha2)

    -------------------------------------------------------
    --SELECCIONA LAS SOLICITUDES DE DISPOSICION DEL PERIODO
    -------------------------------------------------------
    
    LET vcont_sol = 0 
    
    DECLARE cur_10 CURSOR FOR
    SELECT UNIQUE nss       ,
           consecutivo_lote ,
           fecha_conversion
    FROM   dis_cuenta
    WHERE  tipo_movimiento  = 10
    AND    fecha_conversion BETWEEN fecha1 and fecha2
    AND    subcuenta    NOT IN(3,10)
    
    FOREACH cur_10 INTO reg_10.*
        DECLARE cur_sol CURSOR FOR
        SELECT *
        FROM   ret_solicitud_tx
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        
        FOREACH cur_sol INTO lr_ret_solicitud_tx.*
            LET vcont_sol = vcont_sol + 1
            
            --EVALUA GRUPO PARA DETERMINAR SALDOS A RECUPERAR
            
            LET v_suma_sar_isr     = 0 
            LET v_suma_sar_neto    = 0 
            LET v_bruto_sar        = 0
            LET v_neto_sar_calc    = 0 
            LET v_isr_sar_calc     = 0 
            LET v_exento_sar_calc  = 0 
            
            LET v_suma_rcv_isr     = 0 
            LET v_suma_rcv_neto    = 0 
            LET v_bruto_rcv        = 0 
            LET v_neto_rcv_calc    = 0 
            LET v_isr_rcv_calc     = 0 
            LET v_exento_rcv_calc  = 0 
            
            LET v_suma_isr         = 0 
            LET v_suma_neto        = 0 
            LET v_neto             = 0 
            LET v_isr              = 0 
            LET v_exento           = 0 
            
            LET v_mto_grabable_sar = 0
            LET v_mto_grabable_rcv = 0
             
            LET calcular_sar   = "NO"
            LET calcular_rcv   = "NO"       
            
            CASE lr_ret_solicitud_tx.grupo
                WHEN 2
                     LET calcular_sar = "SI"
                WHEN 5
                     LET calcular_sar = "SI"
                WHEN 7
                     LET calcular_sar = "SI"
                     LET calcular_rcv = "SI"
                WHEN 6
                     LET calcular_sar = "SI"
                     LET calcular_rcv = "SI"
                WHEN 8
                     LET calcular_sar = "SI"
                     LET calcular_rcv = "SI"
                WHEN 9
                     LET calcular_sar = "SI"
                     LET calcular_rcv = "SI"
            END CASE
            
            --RECUPERA EL SALDO DE SAR--
            IF calcular_sar = "SI" THEN       
                --OBTIENE ISR--
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   v_suma_sar_isr
                FROM   dis_cuenta
                WHERE  nss              = lr_ret_solicitud_tx.nss
                AND    consecutivo_lote = lr_ret_solicitud_tx.consecutivo
                AND    subcuenta        = 7
                AND    tipo_movimiento  = 10
                
                --OBTIENE NETO--
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   v_suma_sar_neto
                FROM   dis_cuenta
                WHERE  nss              = lr_ret_solicitud_tx.nss
                AND    consecutivo_lote = lr_ret_solicitud_tx.consecutivo
                AND    subcuenta        = 7
                AND    tipo_movimiento  != 10
                
                LET v_bruto_sar = v_suma_sar_isr + v_suma_sar_neto
                
                LET v_neto_sar_calc    = v_suma_sar_neto
                LET v_isr_sar_calc     = v_suma_sar_isr
                LET v_mto_grabable_sar = v_isr_sar_calc / 0.20
                LET v_exento_sar_calc  = v_bruto_sar - v_mto_grabable_sar
            END IF 
            
            --RECUPERA EL SALDO DE RCV--
            IF calcular_rcv = "SI" THEN
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   v_suma_rcv_isr
                FROM   dis_cuenta
                WHERE  nss              = lr_ret_solicitud_tx.nss
                AND    consecutivo_lote = lr_ret_solicitud_tx.consecutivo
                AND    subcuenta        IN (1,2,5,6,9)
                AND    tipo_movimiento  =  10
                
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   v_suma_rcv_neto
                FROM   dis_cuenta
                WHERE  nss              = lr_ret_solicitud_tx.nss
                AND    consecutivo_lote = lr_ret_solicitud_tx.consecutivo
                AND    subcuenta        IN (1,2,5,6,9)
                AND    tipo_movimiento  != 10
                
                LET v_bruto_rcv = v_suma_rcv_isr + v_suma_rcv_neto
                 
                LET  v_neto_rcv_calc    = v_suma_rcv_neto
                LET  v_isr_rcv_calc     = v_suma_rcv_isr
                LET  v_mto_grabable_rcv = v_isr_rcv_calc / 0.20
                LET  v_exento_rcv_calc  = v_bruto_rcv - v_mto_grabable_rcv
            END IF
            
            --Suma el neto de ambos calculos en el caso de que aplique--
            LET v_suma_isr  = v_suma_sar_isr  + v_suma_rcv_isr
            LET v_suma_neto = v_suma_sar_neto + v_suma_rcv_neto
            
            LET v_neto   = v_neto_rcv_calc   + v_neto_sar_calc
            LET v_isr    = v_isr_rcv_calc    + v_isr_sar_calc
            LET v_exento = v_exento_rcv_calc + v_exento_sar_calc
            
            SELECT *
            INTO   lr_afi_mae_afiliado.*
            FROM   afi_mae_afiliado
            WHERE  n_seguro = lr_ret_solicitud_tx.nss 
            
            --RECUPERA LA FECHA CONVERSION DEL RETIRO--
            
            INSERT INTO ret_paso_shcp
            VALUES (lr_ret_solicitud_tx.nss         ,
                    lr_ret_solicitud_tx.consecutivo ,
                    ultimo_folio                    ,
                    lr_ret_solicitud_tx.tipo_retiro ,
                    lr_afi_mae_afiliado.n_rfc       ,
                    lr_afi_mae_afiliado.n_unico     ,
                    lr_afi_mae_afiliado.paterno     ,
                    lr_afi_mae_afiliado.materno     ,
                    lr_afi_mae_afiliado.nombres     ,
                    reg_10.fecha_conversion         , 
                    v_suma_neto                     ,
                    v_suma_isr                      , 
                    0                               ,
                    0                               ,
                    v_exento
                   )
        END FOREACH
    END FOREACH
      
    -------------------------------------------------------
    --SELECCIONA LAS SOLICITUDES DE VOLUNTARIAS
    -------------------------------------------------------

    LET vcont_vol = 0 

    DECLARE cur_vol CURSOR FOR
    SELECT  A.n_seguro,
            A.consecutivo
    FROM    ret_cta_vol A
    WHERE   A.estado = 8
    AND     A.fecha_ult_ret BETWEEN fecha1 AND fecha2

    FOREACH cur_vol INTO lr_ret_cta_vol.*
       LET vcont_vol = vcont_vol + 1
       
       --EVALUA GRUPO PARA DETERMINAR SALDOS A RECUPERAR
        
       LET v_neto         = 0   --Resultado del calculo
       LET v_isr          = 0   --Resultado del calculo
       LET v_exento       = 0   --Resultado del calculo

       --RECUPERA ISR
       SELECT NVL(SUM(monto_en_pesos),0)
       INTO   v_isr 
       FROM   dis_cuenta
       WHERE  nss              = lr_ret_cta_vol.n_seguro
       AND    consecutivo_lote = lr_ret_cta_vol.consecutivo
       AND    subcuenta        IN (3,10,23)
       AND    tipo_movimiento  in (10,20)

       let v_isr = v_isr * -1

       -- SELECCIONA EL NETO
       SELECT NVL(SUM(monto_en_pesos),0) 
       INTO   v_neto
       FROM   dis_cuenta
       WHERE  nss              = lr_ret_cta_vol.n_seguro
       AND    consecutivo_lote = lr_ret_cta_vol.consecutivo
       AND    subcuenta        IN (3,10,23) 
       AND    tipo_movimiento  = 490

       LET v_neto = v_neto * -1
       
       SELECT * 
       INTO   lr_afi_mae_afiliado.*  
       FROM   afi_mae_afiliado
       WHERE  n_seguro = lr_ret_cta_vol.n_seguro 

       --RECUPERA LA FECHA CONVERSION DEL RETIRO
       SELECT UNIQUE fecha_conversion
       INTO   v_fecha_retiro
       FROM   dis_cuenta
       WHERE  nss              = lr_ret_cta_vol.n_seguro
       AND    consecutivo_lote = lr_ret_cta_vol.consecutivo
       AND    tipo_movimiento  = 490 --ff
      
       INSERT INTO ret_paso_shcp
       VALUES ( lr_ret_cta_vol.n_seguro,
                lr_ret_cta_vol.consecutivo,
                ultimo_folio,
                "V",
                lr_afi_mae_afiliado.n_rfc,
                lr_afi_mae_afiliado.n_unico,
                lr_afi_mae_afiliado.paterno,
                lr_afi_mae_afiliado.materno,
                lr_afi_mae_afiliado.nombres,
                v_fecha_retiro,
                v_neto,
                v_isr,
                0,
                0,
                0)
                
    END FOREACH 

    MESSAGE ""
    
    -- VALIDA EXISTENCIA DE INFORMACION PARA GENERACION DE ARCHIVO
    
    IF vcont_sol = 0 AND vcont_vol = 0 THEN
       PROMPT " NO HAY INFORMACION DEL PERIODO A INFORMAR, PRESIONE RETURN "
              FOR CHAR vtecla
       EXIT PROGRAM
    END IF

    ---------------------------
    -- GENERACION DEL ARCHIVO 
    ---------------------------
    
    SELECT ruta_envio
    INTO   vruta_envio
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
    
    IF STATUS = NOTFOUND THEN
       PROMPT " LA RUTA DE ENVIO NO EXISTE, PRESIONE RETURN PARA SALIR" 
             FOR CHAR vtecla
       EXIT PROGRAM
    END IF
      
    LET vnom_archivo = vruta_envio CLIPPED, "/", TODAY USING "YYYYMMDD.ISR"

    UNLOAD TO vnom_archivo
    SELECT nss,
           rfc,
           curp,
           fecha_retiro,
           paterno,
           materno,
           nombres,
           tipo_retiro,
           (neto_reg + isr_reg) ,
           exento_calc,
           isr_reg,
           0,
           0
    FROM   ret_paso_shcp
    WHERE  folio = ultimo_folio
    AND    tipo_retiro != "V"
    AND    fecha_retiro BETWEEN fecha1 AND fecha2
    UNION ALL
    SELECT a.nss,
           a.rfc,
           a.curp,
           a.fecha_retiro,
           a.paterno,
           a.materno,
           a.nombres,
           a.tipo_retiro,
           (a.neto_reg + a.isr_reg) ,
           a.exento_calc,
           a.isr_reg,
           b.ren_nominal_tot,
           b.ren_real_tot
    FROM   ret_paso_shcp a,
           ret_interes_shcp b
    WHERE  a.folio = ultimo_folio
    AND    a.folio = b.folio
    AND    a.tipo_retiro="V"
    AND    a.nss          = b.nss
    AND    a.consecutivo  = b.consecutivo
    AND    a.fecha_retiro BETWEEN fecha1 AND fecha2
    ORDER  BY 8
    
    LET vtot_unload = SQLCA.SQLERRD[3]
    
    LET vcomando = "chmod 777 ",vnom_archivo CLIPPED," 2> /dev/null"
    RUN vcomando


    DISPLAY " EL ARCHIVO ", vnom_archivo AT 13,13
    DISPLAY " ha sido generado con <",vtot_unload USING "<<<,<<&>"," Registros."
            AT 15,13
    
    DISPLAY "FOLIO : ",ultimo_folio,"" AT 19,2
    PROMPT " PRESIONE RETURN PARA CONTINUAR: " FOR CHAR vtecla
    
END MAIN


{==============================================================================}
{ FUNCION    : calcula_intereses()                                             }
{ DESARROLLO : Franco Ulloa                                                    }
{ OBJETIVO   :                                                                 }
{ RECIBE     : periodo del cual se requiere buscar la informacion              }
{ RETORNA    : cantidad de registros procesados                                }
{==============================================================================}

FUNCTION calcula_intereses(pfecha1, pfecha2)
#ci----------------------------------------
    DEFINE reg_1 RECORD #loc #reg_1
        nss                   CHAR(11) ,
        consecutivo           INTEGER
    END RECORD

    DEFINE reg_2 RECORD #loc #reg_2
        nss                   CHAR(11)      ,
        folio                 INTEGER       ,
        consecutivo           INTEGER       , 
        fecha_aporte          DATE          ,
        fecha_liquidacion     DATE          ,
        mto_neto              DECIMAL(16,6) ,
        mto_retencion         DECIMAL(16,6) ,
        mto_rendimiento       DECIMAL(16,6)
    END RECORD
    
    DEFINE #loc #date
        pfecha1               DATE ,           --Fecha inicial periodo consulta
        pfecha2               DATE             --Fecha final   periodo consulta
    
    DEFINE #loc
        vsiefore              SMALLINT
    
    DEFINE #loc #decimal
        importe_fin           DECIMAL(16,6)  ,
        importe_ini           DECIMAL(16,6)  ,
        total_accion          DECIMAL(16,6)  ,
        ren_real              DECIMAL(16,6)  ,
        mac                   DECIMAL(16,6)  ,
        infla_per             DECIMAL(16,6)  ,
        factor_infla          DECIMAL(16,6)  ,
        precio_acc_ini        DECIMAL(16,6)  ,
        precio_acc_fin        DECIMAL(16,6)  ,
        ren_nominal_tot       DECIMAL(16,6)  ,
        ren_real_tot          DECIMAL(16,6)  ,
        precio_udi_ini        DECIMAL(19,14) ,
        precio_udi_fin        DECIMAL(19,14)
    


    -- PROCESA LAS SOLICITUDES DE APORTACIONES VOLUNTARIAS LIQUIDADAS--

    DECLARE cur_1 CURSOR FOR
    SELECT n_seguro    ,
           consecutivo
    FROM   ret_cta_vol
    WHERE  fecha_ult_ret BETWEEN pfecha1 AND pfecha2
    AND    estado        = 8     --Liquidado

    FOREACH cur_1 INTO reg_1.*    
        LET ren_nominal_tot = 0
        LET ren_real_tot    = 0

        --RECUPERA LOS MONTOS PAGADOS EN PESOS PARA LA SOLICITUD--
        
        DECLARE cur_2 CURSOR FOR
        SELECT nss                ,
               folio              ,
               consecutivo        , 
               fecha_aporte       ,
               fecha_liquidacion  ,
               mto_neto * -1      ,
               mto_retencion * -1 ,
               mto_rendimiento
        FROM   ret_pago_vol
        WHERE  nss         = reg_1.nss
        AND    consecutivo = reg_1.consecutivo

        FOREACH cur_2 INTO reg_2.*    
            SELECT UNIQUE(siefore)
            INTO   vsiefore
            FROM   dis_cuenta
            WHERE  nss              = reg_2.nss
            AND    folio            = reg_2.folio
            AND    consecutivo_lote = reg_2.consecutivo
            AND    monto_en_pesos   = reg_2.mto_neto * -1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                LET g_mensaje = "NO SE ENCONTRARON MOVIMIENTOS NSS:",reg_2.nss
                CALL ERRORLOG(g_mensaje)
                ERROR g_mensaje
                SLEEP 3
{
                prompt "para 570" for char enter
}
                LET g_errores = 1
            END IF
               
            -----------------------------------------------------------------
            -- Obtiene el precio de accion del aporte y del retiro         --
            -- Verificar que las liquidaciones se hicieron en la siefore 2 --
            -----------------------------------------------------------------

            LET precio_acc_ini = 0

            SELECT precio_del_dia
            INTO   precio_acc_ini
            FROM   glo_valor_accion
            WHERE  codigo_siefore  = vsiefore
            AND    fecha_valuacion = reg_2.fecha_aporte

            IF SQLCA.SQLCODE = NOTFOUND THEN
               LET g_mensaje = "NO SE ENCONTRO PRECIO SIEFORE ", vsiefore USING "<<&" ,
                               " FECHA: ", reg_2.fecha_aporte,
                               " NSS:",reg_2.nss
               CALL ERRORLOG(g_mensaje)
               ERROR g_mensaje
               SLEEP 3
               LET g_errores = 1
            END IF


            LET precio_acc_fin = 0

            SELECT precio_del_dia
            INTO   precio_acc_fin
            FROM   glo_valor_accion
            WHERE  codigo_siefore  = vsiefore
            AND    fecha_valuacion = reg_2.fecha_liquidacion

            IF SQLCA.SQLCODE = NOTFOUND THEN
               LET g_mensaje = "NO SE ENCONTRO PRECIO SIEFORE ", vsiefore USING "<<&" ,
                               " FECHA: ", reg_2.fecha_liquidacion,
                               " NSS:",reg_2.nss
               CALL ERRORLOG(g_mensaje)
               ERROR g_mensaje
               SLEEP 3
               LET g_errores = 1
            END IF
            ---------------------------------------------------------------

            ---------------------------------------------------------------
            --Obtiene el precio de la udi del aporte y del retiro        --
            
            LET precio_udi_ini = 0

            SELECT valor_udi
            INTO   precio_udi_ini
            FROM   tab_udi
            WHERE  fecha_udi = reg_2.fecha_aporte

            IF SQLCA.SQLCODE = NOTFOUND THEN
               LET g_mensaje = "NO SE ENCONTRO PRECIO UDI ", 
                               " FECHA: ", reg_2.fecha_aporte,
                               " NSS:",reg_2.nss
               CALL ERRORLOG(g_mensaje)
               ERROR g_mensaje
               SLEEP 3
               LET g_errores = 1
            END IF  
        

            LET precio_udi_fin = 0

            SELECT valor_udi
            INTO   precio_udi_fin
            FROM   tab_udi
            WHERE  fecha_udi = reg_2.fecha_liquidacion

            IF SQLCA.SQLCODE = NOTFOUND THEN
               LET g_mensaje = "NO SE ENCONTRO PRECIO UDI ", 
                               " FECHA: ", reg_2.fecha_liquidacion,
                               " NSS:",reg_2.nss
               CALL ERRORLOG(g_mensaje)
               ERROR g_mensaje
               SLEEP 3
               LET g_errores = 1
            END IF

            ---------------------------------------------------------------

            LET factor_infla = 0
            LET infla_per    = 0
            LET mac          = 0
            LET ren_real     = 0
            LET total_accion = 0
            LET importe_ini  = 0
            LET importe_fin  = 0

            --------------------------------------------------------
            --verificar que los importes de la tabla ret_pago_vol --
            --cuadren con la tabla ret_shcp                       --
            --------------------------------------------------------

            IF precio_acc_fin = 0 OR precio_udi_ini = 0 THEN
               ERROR "DIVISION POR CERO"
               CALL ERRORLOG("DIVISION POR CERO CALCULA_INTERESES")
               SLEEP 3
               LET g_errores = 1
            END IF

            LET total_accion = (reg_2.mto_neto + reg_2.mto_retencion)/
                                precio_acc_fin

            LET importe_ini           = total_accion * precio_acc_ini
            LET importe_fin           = total_accion * precio_acc_fin

            LET reg_2.mto_rendimiento = importe_fin - importe_ini

            LET factor_infla          = (precio_udi_fin / precio_udi_ini) -1

            LET mac                   = (reg_2.mto_neto + reg_2.mto_retencion) -
                                         reg_2.mto_rendimiento
            LET infla_per             = factor_infla * mac

            LET ren_real              = reg_2.mto_rendimiento - infla_per

            LET ren_real_tot          = ren_real_tot + ren_real

            LET ren_nominal_tot       = ren_nominal_tot + reg_2.mto_rendimiento

            INSERT INTO ret_det_int_shcp
            VALUES(reg_1.nss             ,
                   reg_1.consecutivo     ,
                   ultimo_folio          ,
                   total_accion          ,
                   importe_ini           ,
                   importe_fin           ,
                   reg_2.mto_rendimiento ,
                   factor_infla          ,
                   mac                   ,
                   infla_per             ,
                   ren_real
                  )
        END FOREACH

        INSERT INTO ret_interes_shcp
        VALUES(reg_1.nss         ,
               reg_1.consecutivo ,
               ultimo_folio      ,
               ren_nominal_tot   ,
               ren_real_tot
              )
    
    END FOREACH   -- Siguiente Solicitud

    IF g_errores != 0 THEN
       CALL notifica_error()
    END IF
      
END FUNCTION

FUNCTION notifica_error()

   define vtecla  char(1)

   open window wmensaje at 7,12 with 10 rows, 60 columns
        attribute(border, PROMPT line last -1)

        DISPLAY "                   A V I S O                       " AT 1,5
        DISPLAY " SE HAN PRESENTADO ERRORES FAVOR DE NOTIFICAR      " AT 3,5
        DISPLAY " AL AREA DE SISTEMAS                               " AT 4,5
        PROMPT "Presione ENTER para continuar: " for char vtecla

   close window wmensaje

END FUNCTION


