################################################################################
# Proyecto          => SISTEMA DE AFORE ( SAFRE )                              #
# Sistema           => RET                                                     #
# Owner             => E.F.P.                                                  #
# Programa RETC849  => INFORMA CALCULO DE ISR PARA SHCP                        #
# Fecha creacion    => 8 DE FEBRERO DEL 2008                                   #
# By                => FRANCO ESTEBAN ULLOA VIDELA                             #
# Fecha actualiza   =>                                                         #
################################################################################
DATABASE safre_af
    DEFINE reg_10 RECORD #glo reg_10
        fecha_ini             DATE ,
        fecha_fin             DATE
    END RECORD
    
    DEFINE reg_11 RECORD #glo #reg_11
        nss                   CHAR(11)     ,
        consecutivo           INTEGER      ,
        fecha_conversion      DATE         ,
        precio_accion         DECIMAL(16,6) 
    END RECORD
    
    DEFINE reg_13 RECORD #glo #reg_13
        suma_sar_isr          DECIMAL(16,6) ,
        suma_sar_neto         DECIMAL(16,6) ,
        suma_sar_bruto        DECIMAL(16,6)
    END RECORD
    
    DEFINE reg_14 RECORD #glo #reg_14
        suma_rcv_isr          DECIMAL(16,6) , 
        suma_rcv_neto         DECIMAL(16,6) , 
        suma_rcv_bruto        DECIMAL(16,6)
    END RECORD
    
    DEFINE reg_15 RECORD #glo #reg_15
        tot_suma_isr          DECIMAL(16,6) , 
        tot_suma_neto         DECIMAL(16,6) ,
        interes_nominal       DECIMAL(16,6) ,
        p_ini_deflactado      DECIMAL(16,6) ,
        factor_inflacion      DECIMAL(16,6) ,
        interes_real          DECIMAL(16,6) 
    END RECORD
    
    DEFINE reg_20 RECORD #glo #reg_20
        siefore               SMALLINT      ,
        precio_del_dia        DECIMAL(16,6) ,
        saldo_acciones        DECIMAL(16,6) ,
        saldo_pesos           DECIMAL(16,6)
    END RECORD
      
    DEFINE
        reg_16                RECORD LIKE afi_mae_afiliado.* ,
        reg_12                RECORD LIKE ret_solicitud_tx.*
                                                                                                         
    DEFINE #glo #date
        HOY                   DATE
        
    DEFINE #glo #char
        usuario               CHAR(008) ,
        vopcion               CHAR(001) ,
        vruta_envio           CHAR(200) ,
        vcomando              CHAR(300) ,
        calcular_sar          CHAR(002) ,          
        calcular_rcv          CHAR(002) ,          
        vnom_archivo          CHAR(200) ,        
        enter                 CHAR(001)
    
    DEFINE #glo #integer
        vtot_unload           ,
        vcont_sol             ,       -- Contador de solicitudes
        ultimo_folio          INTEGER    
        
MAIN
    OPTIONS INPUT WRAP           ,
            PROMPT  LINE LAST -1 ,
            MESSAGE LINE LAST -1
    DEFER INTERRUPT

    CALL STARTLOG("RETC849.log")

    CALL init() #i
    OPEN WINDOW retc8491 AT 2,2 WITH FORM "RETC8491" ATTRIBUTE(BORDER)
    DISPLAY " ESC = Aceptar                                                 Ctrl-C = Salir " AT 1,1
    DISPLAY " RETC849              INFORME DETALLADO DE ISR PARA SHCP                      " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,68 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME reg_10.* WITHOUT DEFAULTS ATTRIBUTE(REVERSE)
        AFTER FIELD fecha_ini
            IF reg_10.fecha_ini  IS NULL THEN
                ERROR " LA FECHA NO PUEDE SER NULA "
                NEXT FIELD reg_10.fecha_ini
            END IF
            
        AFTER FIELD fecha_fin
            IF reg_10.fecha_fin  IS NULL THEN
               ERROR " LA FECHA NO PUEDE SER NULA "
               NEXT FIELD reg_10.fecha_fin
            END IF
        
            IF reg_10.fecha_fin < reg_10.fecha_ini THEN
               ERROR " LA FECHA FINAL NO PUEDE SER MENOR A LA INICIAL "
                NEXT FIELD reg_10.fecha_fin
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
    
    MESSAGE " PROCESANDO..." ATTRIBUTE(REVERSE)

    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF
    	
    INSERT INTO glo_folio VALUES (ultimo_folio)
    
    INSERT INTO ret_ctr_shcp
    VALUES(ultimo_folio     ,--folio
           reg_10.fecha_ini ,--fecha_ini
           reg_10.fecha_fin ,--fecha_fin
           HOY              ,--fecha_proceso
           usuario           --usuario
          )

    ---------------------------------------------------------
    --SELECCIONA LAS SOLICITUDES DE DISPOSICION DEL PERIODO--
    ---------------------------------------------------------
    
    LET vcont_sol = 0 
    
    --Selecciona todos los trabajadores que realizaron un retiro por--
    --Disposición que tuvieron retención de impuestos (ISR). Es     --
    --decir, no se se están considerando aquellos a los que no se   --
    --les retuvo impuesto                                           --
    
    DECLARE cur_11 CURSOR FOR
    SELECT UNIQUE nss       ,
           consecutivo_lote ,
           fecha_conversion ,
           precio_accion
    FROM   dis_cuenta
    WHERE  tipo_movimiento        = 10
    AND    fecha_conversion BETWEEN reg_10.fecha_ini and reg_10.fecha_fin
    AND    subcuenta         NOT IN(3,10)
    
    FOREACH cur_11 INTO reg_11.*
        DECLARE cur_12 CURSOR FOR
        SELECT *
        FROM   ret_solicitud_tx
        WHERE  nss         = reg_11.nss
        AND    consecutivo = reg_11.consecutivo
        
        FOREACH cur_12 INTO reg_12.*
            LET vcont_sol = vcont_sol + 1
            
            --EVALUA GRUPO PARA DETERMINAR SALDOS A RECUPERAR
            
            LET reg_13.suma_sar_isr   = 0 
            LET reg_13.suma_sar_neto  = 0 
            LET reg_13.suma_sar_bruto = 0
            
            LET reg_14.suma_rcv_isr   = 0 
            LET reg_14.suma_rcv_neto  = 0 
            LET reg_14.suma_rcv_bruto = 0  
            
            LET reg_15.tot_suma_isr   = 0 
            LET reg_15.tot_suma_neto  = 0 
             
            LET calcular_sar   = "NO"
            LET calcular_rcv   = "NO"       
            
            CASE reg_12.grupo
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
                
                OTHERWISE
                     PROMPT "NO EXISTE GRUPO",reg_12.grupo FOR CHAR enter
                     EXIT PROGRAM
            END CASE
            
            --RECUPERA EL SALDO DE SAR--
            IF calcular_sar = "SI" THEN       
                --OBTIENE ISR--
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   reg_13.suma_sar_isr
                FROM   dis_cuenta
                WHERE  nss              = reg_12.nss
                AND    consecutivo_lote = reg_12.consecutivo
                AND    subcuenta        = 7
                AND    tipo_movimiento  = 10
                
                --OBTIENE NETO--
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   reg_13.suma_sar_neto
                FROM   dis_cuenta
                WHERE  nss              = reg_12.nss
                AND    consecutivo_lote = reg_12.consecutivo
                AND    subcuenta        = 7
                AND    tipo_movimiento != 10
                
                LET reg_13.suma_sar_bruto = reg_13.suma_sar_isr + reg_13.suma_sar_neto   
            END IF 
            
            --RECUPERA EL SALDO DE RCV--
            IF calcular_rcv = "SI" THEN
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   reg_14.suma_rcv_isr
                FROM   dis_cuenta
                WHERE  nss              = reg_12.nss
                AND    consecutivo_lote = reg_12.consecutivo
                AND    subcuenta        IN (1,2,5,6,9)
                AND    tipo_movimiento  =  10
                
                SELECT NVL(SUM(monto_en_pesos*-1),0)
                INTO   reg_14.suma_rcv_neto
                FROM   dis_cuenta
                WHERE  nss              = reg_12.nss
                AND    consecutivo_lote = reg_12.consecutivo
                AND    subcuenta        IN (1,2,5,6,9)
                AND    tipo_movimiento  != 10
                
                LET reg_14.suma_rcv_bruto = reg_14.suma_rcv_isr + reg_14.suma_rcv_neto
            END IF
            
            --Suma el neto de ambos calculos en el caso de que aplique--
            LET reg_15.tot_suma_isr  = reg_13.suma_sar_isr  + reg_14.suma_rcv_isr
            LET reg_15.tot_suma_neto = reg_13.suma_sar_neto + reg_14.suma_rcv_neto
            
            
            SELECT *
            INTO   reg_16.*
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_12.nss 
           
        END FOREACH 


        CALL calcula_montos(reg_11.nss              ,
                            reg_11.fecha_conversion ,
                            reg_11.precio_accion    ,
                            reg_12.grupo
                           ) #cm
            RETURNING reg_15.interes_nominal  ,
                      reg_15.p_ini_deflactado ,
                      reg_15.factor_inflacion ,
                      reg_15.interes_real
 
 
        --Cálculo el saldo al día en pesos--
        
        DECLARE cur_14 CURSOR FOR
        SELECT siefore ,
               NVL(SUM(A.monto_en_acciones),0)
        FROM   dis_cuenta A, tab_agrupa_subcta B
        WHERE  A.nss            = reg_11.nss
        AND    B.grupo          = reg_12.grupo
        AND    A.subcuenta      = B.subcuenta
        AND    A.subcuenta NOT IN(4,8)
        GROUP BY 1
        
        LET reg_20.saldo_pesos = 0
        
        FOREACH cur_14 INTO reg_20.siefore ,
                            reg_20.saldo_acciones
                            
            SELECT precio_del_dia
            INTO   reg_20.precio_del_dia
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = HOY
            AND    codigo_siefore  = reg_20.siefore
            
            LET reg_20.saldo_pesos = reg_20.saldo_pesos + 
                                     reg_20.saldo_acciones * reg_20.precio_del_dia
        END FOREACH
        
        IF reg_20.saldo_pesos IS NULL THEN
            LET reg_20.saldo_pesos = 0
        END IF
        --Fin cálculo del saldo al día en pesos--
 
                   
        INSERT INTO ret_paso_shcp
        VALUES (reg_12.nss              ,--nss
                reg_12.consecutivo      ,--consecutivo
                ultimo_folio            ,--folio
                reg_12.tipo_retiro      ,--tipo_retiro
                reg_16.n_rfc            ,--rfc
                reg_16.n_unico          ,--curp
                reg_16.paterno          ,--paterno
                reg_16.materno          ,--materno
                reg_16.nombres          ,--nombres
                reg_11.fecha_conversion ,--fecha_retiro
                reg_15.tot_suma_neto    ,--neto_reg
                reg_15.tot_suma_isr     ,--isr_reg
                reg_15.interes_nominal  ,--interes_nominal
                reg_15.p_ini_deflactado ,--p_ini_deflactado
                reg_15.factor_inflacion ,--factor_inflacion
                reg_15.interes_real     ,--interes_real
                reg_20.saldo_pesos       --saldo_pesos
               )
    END FOREACH
      
    -- VALIDA EXISTENCIA DE INFORMACION PARA GENERACION DE ARCHIVO--
    
    IF vcont_sol = 0 THEN
       PROMPT " NO HAY INFORMACION DEL PERIODO A INFORMAR, PRESIONE RETURN "
              FOR CHAR enter
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
             FOR CHAR enter
       EXIT PROGRAM
    END IF
      
    LET vnom_archivo = vruta_envio CLIPPED, "/", HOY USING "YYYYMMDD.ISR"

    UNLOAD TO vnom_archivo
    SELECT A.nss             ,
           A.paterno         ,
           A.materno         ,
           A.nombres         ,
           A.rfc             ,
           A.curp            ,
           B.dom_calle       ,
           B.dom_numero_ext  ,
           B.dom_numero_int  ,
           B.dom_colonia     ,
           B.dom_delega      ,
           B.dom_ciudad_cod  ,
           B.dom_estado_cod  ,
           B.dom_codpos      ,
           A.interes_nominal ,
           A.interes_real    ,
           A.isr_reg         ,
           A.saldo_pesos
    FROM   ret_paso_shcp A, OUTER ret_beneficiario B
    WHERE  A.folio              = ultimo_folio
    AND    A.tipo_retiro       != "V" --Retiro voluntario
    AND    A.fecha_retiro BETWEEN reg_10.fecha_ini AND reg_10.fecha_fin
    AND    A.nss                = B.nss
    AND    A.consecutivo        = B.consecutivo
    AND    B.consec_benef       = 1
 
    
    LET vtot_unload = SQLCA.SQLERRD[3]
    
    LET vcomando = "chmod 777 ",vnom_archivo CLIPPED," 2> /dev/null"
    RUN vcomando


    DISPLAY " EL ARCHIVO ", vnom_archivo AT 13,13
    DISPLAY " ha sido generado con <",vtot_unload USING "<<<,<<&>"," Registros."
            AT 15,13
    
    DISPLAY "FOLIO : ",ultimo_folio,"" AT 19,2
    PROMPT " PRESIONE RETURN PARA CONTINUAR: " FOR CHAR enter
    
END MAIN

FUNCTION init()
#i------------
    LET HOY              = TODAY
    LET reg_10.fecha_ini = "01012007"
    LET reg_10.fecha_fin = HOY
    
    SELECT USER
    INTO   usuario
    FROM   tab_afore_local
END FUNCTION

FUNCTION calcula_montos(reg_17)
#cm----------------------------
    DEFINE reg_17 RECORD
        nss                   CHAR(11)      ,
        fecha_conversion      DATE          ,
        precio_accion         DECIMAL(16,6) ,
        grupo                 SMALLINT
    END RECORD 
    
    DEFINE reg_18 RECORD
        monto_en_acciones     DECIMAL(16,6) ,
        monto_en_pesos        DECIMAL(16,6) ,
        fecha_conversion      DATE          ,
        precio_accion         DECIMAL(16,6) ,
        interes_nominal       DECIMAL(16,6) ,
        p_ini_deflactado      DECIMAL(16,6) ,--precio inicial deflactado
        factor_inflacion      DECIMAL(16,6) ,
        interes_real          DECIMAL(16,6)
    END RECORD
    
    DEFINE reg_19 RECORD
        udi_inicial           LIKE tab_udi.valor_udi ,
        udi_final             LIKE tab_udi.valor_udi
    END RECORD
    
    DEFINE #loc #decimal
        tot_factor_inflacion  DECIMAL(16,6) ,
        d16_tot_mto_en_pesos  DECIMAL(16,6) ,
        d16_mto_pesos_his     DECIMAL(16,6) ,
        tot_p_ini_deflactado  DECIMAL(16,6)

    LET reg_18.monto_en_acciones = 0
    LET reg_18.monto_en_pesos    = 0
    LET reg_18.fecha_conversion  = NULL
    LET reg_18.precio_accion     = 0
    LET reg_18.interes_nominal   = 0
    LET reg_18.p_ini_deflactado  = 0
    LET tot_p_ini_deflactado     = 0
    LET reg_18.factor_inflacion  = 0
    LET tot_factor_inflacion     = 0
    LET tot_p_ini_deflactado     = 0
    LET reg_18.interes_real      = 0
    
    LET d16_tot_mto_en_pesos     = 0
    LET d16_mto_pesos_his        = 0
    
    SELECT valor_udi
    INTO   reg_19.udi_final
    FROM   tab_udi
    WHERE  fecha_udi = reg_17.fecha_conversion --fecha del retiro
    
    DECLARE cur_13 CURSOR FOR 
    SELECT A.monto_en_acciones ,
           A.monto_en_pesos    ,
           A.fecha_conversion  ,--fecha del aporte
           A.precio_accion      --precio accion del aporte
    FROM   dis_cuenta A, tab_agrupa_subcta B
    WHERE  A.nss               = reg_17.nss
    AND    A.fecha_conversion  < reg_17.fecha_conversion
    AND    A.monto_en_acciones > 0
    AND    B.grupo             = reg_17.grupo
    AND    B.subcuenta         = A.subcuenta

    FOREACH cur_13 INTO reg_18.monto_en_acciones ,
                        reg_18.monto_en_pesos    ,
                        reg_18.fecha_conversion  ,--fecha_del_aporte
                        reg_18.precio_accion
                        
        LET d16_tot_mto_en_pesos = d16_tot_mto_en_pesos + (reg_18.monto_en_acciones * reg_17.precio_accion)
        LET d16_mto_pesos_his    = d16_mto_pesos_his + reg_18.monto_en_pesos
        
        LET reg_19.udi_inicial = 0
        SELECT valor_udi
        INTO   reg_19.udi_inicial
        FROM   tab_udi
        WHERE  fecha_udi = reg_18.fecha_conversion
        
        IF reg_19.udi_inicial = 0 OR reg_19.udi_inicial IS NULL THEN
            PROMPT "FALTA EL PRECIO DE LA UDI PARA EL DIA ",reg_18.fecha_conversion
            FOR CHAR enter
            EXIT PROGRAM
        END IF
        
        LET reg_18.p_ini_deflactado = (reg_19.udi_final/reg_19.udi_inicial) * reg_18.precio_accion
        LET tot_p_ini_deflactado    = tot_p_ini_deflactado + reg_18.p_ini_deflactado

        LET reg_18.factor_inflacion = reg_17.precio_accion - reg_18.p_ini_deflactado
        LET tot_factor_inflacion    = tot_factor_inflacion + reg_18.factor_inflacion
        
        LET reg_18.interes_real     = reg_18.interes_real + (reg_18.factor_inflacion * reg_18.precio_accion)
    END FOREACH
    
    IF reg_18.p_ini_deflactado IS NULL THEN
        LET reg_18.p_ini_deflactado = 0
    END IF
    
    LET reg_18.interes_nominal  = d16_tot_mto_en_pesos - d16_mto_pesos_his
    
    IF reg_18.interes_nominal < 0 THEN
        LET reg_18.interes_nominal = 0
    END IF
    
    RETURN reg_18.interes_nominal ,
           tot_p_ini_deflactado   ,
           tot_factor_inflacion   ,
           reg_18.interes_real
    
END FUNCTION

FUNCTION notifica_error()
#ne----------------------
    open window wmensaje at 7,12 with 10 rows, 60 columns
         attribute(border, PROMPT line last -1)
    
         DISPLAY "                   A V I S O                       " AT 1,5
         DISPLAY " SE HAN PRESENTADO ERRORES FAVOR DE NOTIFICAR      " AT 3,5
         DISPLAY " AL AREA DE SISTEMAS                               " AT 4,5
         PROMPT "Presione ENTER para continuar: " for char enter
    
    close window wmensaje
    
END FUNCTION


