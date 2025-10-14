################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa RETC903  => PROGRAMA DE LIQUIDACION DE RETIRO TOTAL ISSSTE           #
#Fecha creacion    => 12 OCTUBRE 2006                                          #
#By                => DMR                                                      #
#Fecha Modifica.   => 18 DE NOVIEMBRE DE 2007                                  #
#Modificado por    => XAVIER TORRES RIOS                                       #
#                  => SE LE PUSO STATUS DE 131 (DIAGNOSTICADO) YA QUE ASI      #
#                     VIENE DE LA OPERACION 31.                                #
#Fecha Modifica.   => 17 de Abril de 2008                                      #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                 #
#                     Se agregaron cambios para el soporte de multisiefores    #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af 
GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2                      
        recibido_op31         LIKE ret_estado.estado_solicitud,      
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD                                           

    DEFINE #glo #char
        nss_sobregirado       CHAR(011),
        v_liquida             CHAR(100),
        v_liquida1            CHAR(100),
        v_desmarca            CHAR(100),
        v_saldo_dia           CHAR(100),
        enter                 CHAR(001),
        usuario               CHAR(008)

    DEFINE #glo #integer
        sw_1                  ,
        cont_reg              ,
        cont_1                INTEGER

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #smallint
        gs_num_siefores       ,-- Indica el numero de siefores que se usan actualmente
        gs_viv                ,-- Indica en que posicion se encuentra almacenada
                               -- la info de precio de vivienda
        v_marca_ent           ,
        folio_oper_27         ,
        s_codigo_afore        SMALLINT

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS 
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST 

    CALL STARTLOG ("RETC903.log") 

    CALL init()
    
    OPEN WINDOW retc903 AT 4,4 WITH FORM "RETC9031" ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC903       INCORPORA PAGO DE RETIROS TOTALES ISSSTE                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL valida_ejecucion() #ve

    INPUT BY NAME folio_oper_27 WITHOUT DEFAULTS
        BEFORE FIELD folio_oper_27
             DISPLAY BY NAME folio_oper_27
             
             IF folio_oper_27 IS NULL OR folio_oper_27 <= 0 THEN
                 PROMPT " NO EXISTEN REGISTROS A LIQUIDAR...<ENTER> PARA SALIR "
                 FOR CHAR enter
                 EXIT PROGRAM
             END IF

        AFTER FIELD folio_oper_27
            IF folio_oper_27 IS NULL  OR folio_oper_27 <= 0 THEN
                ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_27
            END IF
            
            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO NO EXISTE... "
                NEXT FIELD folio_oper_27
            END IF

            SELECT "OK"
            FROM   dis_cuenta A
            WHERE  A.folio = folio_oper_27
            GROUP BY 1       
            
            IF STATUS <> NOTFOUND THEN
                ERROR "    FOLIO YA LIQUIDADO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_27
            END IF

        ON KEY (ESC)
            IF folio_oper_27 IS NULL OR folio_oper_27 <= 0 THEN
                ERROR " CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_27
            END IF

            SELECT "OK" 
            FROM   ret_sol_issste_tot
            WHERE  folio           = folio_oper_27
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO NO EXISTE... "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_27
            END IF

            SELECT "OK"
            FROM   dis_cuenta A
            WHERE  A.folio = folio_oper_27
            GROUP BY 1       
            
            IF STATUS <> NOTFOUND THEN
                ERROR "    FOLIO YA LIQUIDADO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_27
            END IF
        
            EXIT INPUT
     
        ON KEY (CONTROL-C, INTERRUPT)
            PROMPT" PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    
    END INPUT

    WHILE TRUE
       PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
       IF enter MATCHES "[sSnN]" THEN
          IF enter MATCHES "[sS]" THEN
             EXIT WHILE
          ELSE
             PROMPT" PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
             EXIT PROGRAM
          END IF
       END IF
    END WHILE

    CALL valida_sobregiro(folio_oper_27) --Valida sobregiro--
    RETURNING sw_1,nss_sobregirado

    IF sw_1 <> 0 THEN
        PROMPT  " ERROR AL LIQUIDAR, SE SOBREGIRA CUENTA NSS: ",nss_sobregirado
        FOR CHAR enter
        EXIT PROGRAM
    END IF


    SELECT  COUNT(*)
    INTO    cont_1
    FROM    ret_sol_issste_tot A
    WHERE   A.folio            = folio_oper_27
    AND    (A.status_sub_ret   = "01" OR
            A.status_fondo_viv = "01")

    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1      AT 11,20
    --DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()   #pp  #Liquida  
    CALL segundo_paso()  #sp  #desmarca

    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1   AT 11,20
    DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg AT 13,20
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retc903

END MAIN


FUNCTION init()
#--------------
    LET HOY      = TODAY
    LET cont_1   = 0
    LET cont_reg = 0
    
    --Obtenemos el numero de siefores actual en el sistema--
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore  > 0
    AND    codigo_siefore <> 11

    LET gs_viv = 20

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_2.recibido_op31
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER31"

    SELECT A.estado_solicitud
    INTO   reg_2.liquidado 
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ---- OBTIENE ULTIMO FOLIO ----
    SELECT MAX(folio)
    INTO   folio_oper_27
    FROM   ret_sol_issste_tot
    WHERE  estado_solicitud = reg_2.recibido_op31

    ----- DESMARCA -----
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    LET v_marca_ent = 0

    LET v_liquida = " EXECUTE FUNCTION fn_liquida_ret_ms ( ?,?,?,? ) "
    PREPARE eje_liquida FROM v_liquida

    LET v_liquida1 = " EXECUTE FUNCTION fn_liquida ( ?,?,?,?,? ) "
    PREPARE eje_liquida1 FROM v_liquida1

END FUNCTION

FUNCTION valida_ejecucion()
#ve-------------------

    -- Valida y obtiene los precios de accion
    CALL obtiene_precios_accion(HOY)

END FUNCTION


FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_3  RECORD
        nss_imss              LIKE dis_provision.nss                  ,
        fecha_val_viv         LIKE dis_provision.fecha_valor          ,
        consecutivo           LIKE dis_provision.consecutivo_lote     ,
        status_fondo_viv      LIKE ret_sol_issste_tot.status_fondo_viv,
        status_sub_ret        LIKE ret_sol_issste_tot.status_sub_ret  ,
        subcuenta             LIKE dis_provision.subcuenta            ,
        diag_issste           LIKE ret_sol_issste_tot.diag_issste
    END RECORD

    DEFINE #loc #char
        n_nss                 CHAR(11)
        
    DEFINE #loc #decimal            
        mto_total             DECIMAL(16,6)
        
       
    DEFINE #loc #smallint
        cod_resp              ,
        ban                   SMALLINT


    INITIALIZE n_nss TO NULL
    LET ban = 0

    SELECT  "OK"            
    FROM    ret_sol_issste_tot A
    WHERE   A.folio            = folio_oper_27
    AND    (A.status_sub_ret   = "01" OR
            A.status_fondo_viv = "01")
    AND     A.estado_solicitud = reg_2.recibido_op31
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT " NO EXISTEN REGISTROS A LIQUIDAR..<ENTER> PARA FINALIZAR"
        FOR CHAR enter
        EXIT PROGRAM
    END IF

    DECLARE cur_1 CURSOR FOR
    SELECT UNIQUE A.nss_imss  ,
            A.fecha_val_viv    ,
            A.consecutivo      ,
            A.status_fondo_viv ,
            A.status_sub_ret   ,
            C.subcuenta        ,
            A.diag_issste
    FROM    ret_sol_issste_tot A, dis_provision C
    WHERE   A.folio            = folio_oper_27
    AND     A.folio            = C.folio
    AND     A.nss_imss         = C.nss
    AND     A.consecutivo      = C.consecutivo_lote
    AND    (A.status_sub_ret   = "01" OR
            A.status_fondo_viv = "01")
    AND     A.estado_solicitud = reg_2.recibido_op31

    FOREACH cur_1 INTO reg_3.*
        
        IF ban = 0 THEN
            LET cont_reg = cont_reg + 1
            LET n_nss    = reg_3.nss_imss 
            LET ban      = 1
        END IF

        IF n_nss <> reg_3.nss_imss THEN
            LET n_nss    = reg_3.nss_imss 
            LET cont_reg = cont_reg + 1
        END IF
  
        CASE reg_3.subcuenta
                
            WHEN 14
                IF reg_3.status_fondo_viv = "01" THEN
                    DECLARE cur_liq_viv97 CURSOR FOR eje_liquida1
                    OPEN cur_liq_viv97 USING folio_oper_27   ,#folio
                                             reg_3.nss_imss  ,#nss
                                             reg_3.subcuenta ,#subcuenta
                                             HOY             ,#fecha_liquida
                                             HOY              #fecha_proceso
                 
                    FETCH cur_liq_viv97 INTO cod_resp,mto_total
                    CLOSE cur_liq_viv97
                END IF
            
            WHEN 13
                IF reg_3.status_sub_ret = "01" THEN
                    DECLARE cur_liq_ret CURSOR FOR eje_liquida1
                    OPEN cur_liq_ret  USING folio_oper_27   ,#folio
                                            reg_3.nss_imss  ,#nss
                                            reg_3.subcuenta ,#subcuenta
                                            HOY             ,#fecha_liquida
                                            HOY              #fecha_proceso
                    FETCH cur_liq_ret INTO cod_resp, mto_total
                    CLOSE cur_liq_ret
                END IF
            
            WHEN 19
                IF reg_3.status_sub_ret = "01" THEN 
                    DECLARE cur_liq_1 CURSOR FOR eje_liquida1
                    OPEN cur_liq_1  USING folio_oper_27   ,#folio
                                          reg_3.nss_imss  ,#nss
                                          reg_3.subcuenta ,#subcuenta
                                          HOY             ,#fecha_liquida
                                          HOY              #fecha_proceso
                    FETCH cur_liq_1 INTO cod_resp,mto_total
                    CLOSE cur_liq_1  
                END IF
        END CASE

        IF cod_resp <> 0 THEN
            PROMPT " No Liquido correctamente a :",reg_3.nss_imss CLIPPED,
                   " ",cod_resp," ",mto_total, " ...<ENTER> continua "
            FOR enter
            EXIT PROGRAM
        ELSE
            UPDATE ret_sol_issste_tot
            SET    estado_solicitud   = reg_2.liquidado 
            WHERE  folio              = folio_oper_27
            AND    nss_imss           = reg_3.nss_imss
            AND    consecutivo        = reg_3.consecutivo
        END IF

    END FOREACH

END FUNCTION


FUNCTION segundo_paso()
#sp--------------------
    DEFINE reg_4 RECORD #reg_4
        nss_imss         LIKE ret_sol_issste_tot.nss_imss    , 
        consecutivo      LIKE ret_sol_issste_tot.consecutivo , 
        tipo_retiro      LIKE ret_sol_issste_tot.tipo_retiro  
    END RECORD

    DEFINE reg_5 RECORD #loc #reg_5
        estado_marca           SMALLINT,
        marca_causa            SMALLINT
    END RECORD
   
    DEFINE
        c_ind_trans            CHAR(70),
        resp_ind               CHAR(1),
        ind_trans_ret          SMALLINT

    LET ind_trans_ret = 9

    LET reg_5.estado_marca = 0
    LET reg_5.marca_causa  = 0


    DECLARE cur_2 CURSOR FOR
    SELECT A.nss_imss     ,
           A.consecutivo  ,
           A.tipo_retiro    
    FROM   ret_sol_issste_tot A
    WHERE  A.folio            = folio_oper_27
    AND    A.estado_solicitud = reg_2.liquidado

    FOREACH cur_2 INTO reg_4.*

        SELECT movimiento
        INTO   v_marca_ent
        FROM   tab_retiro_issste
        WHERE  tipo_retiro = reg_4.tipo_retiro 

        LET v_desmarca =" EXECUTE PROCEDURE desmarca_cuenta('",
                          reg_4.nss_imss,"',",v_marca_ent,
                          ",",reg_4.consecutivo,",",
                          reg_5.estado_marca,",",reg_5.marca_causa
                          ,",'",usuario,"')"

        PREPARE eje_desmarca FROM v_desmarca
        EXECUTE eje_desmarca

        LET c_ind_trans = "EXECUTE FUNCTION fn_ind_transferencia (?,?,?)"

        PREPARE eje_ind_trans FROM c_ind_trans

        DECLARE cur_ind_trans CURSOR FOR eje_ind_trans
        FOREACH cur_ind_trans USING reg_4.nss_imss,
                                    ind_trans_ret,
                                    HOY
                              INTO  resp_ind
        END FOREACH

        SELECT "OK"
        FROM   cta_act_marca
        WHERE  nss       = reg_4.nss_imss
        AND    marca_cod = 140
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            CALL solicita_estado_cuenta(reg_4.nss_imss)
        END IF

    END FOREACH

END FUNCTION


FUNCTION solicita_estado_cuenta(xnss)
#sec---------------------------------
    DEFINE 
        xnss   CHAR(11)

    UPDATE cta_ctr_cuenta
    SET    tipo_informe  = 5,
           fecha_informe = today
    WHERE  nss           = xnss

END FUNCTION


FUNCTION valida_sobregiro(v_folio)
#pp--------------------------

    DEFINE reg_7 RECORD #loc #reg_7
        nss_imss              CHAR(11)     ,
        subcuenta             SMALLINT     ,
        monto_en_acciones     DECIMAL(16,6),
        monto_en_pesos        DECIMAL(16,6)
    END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        subcuenta                          ,
        siefore               SMALLINT     ,
        monto_acc                          ,
        monto_pesos           DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        v_grupo               SMALLINT

    DEFINE #loc #date
        v_fecha               DATE

    DEFINE #loc #integer
        sw_1                  ,
        v_folio               INTEGER

    DEFINE #loc #decimal
        monto_accion_13       ,
        monto_accion_19       ,
        monto_pesos_14        DECIMAL(16,6)

    LET reg_7.monto_en_acciones = 0
    LET reg_7.monto_en_pesos    = 0
    LET monto_accion_13         = 0
    LET monto_accion_19         = 0
    LET monto_pesos_14          = 0

    LET v_fecha                 = HOY
    LET v_grupo                 = 0
    LET sw_1                    = 0

    DECLARE  cur_4 CURSOR FOR
    SELECT  A.nss                         ,
            A.subcuenta                   ,
            SUM(A.monto_en_acciones) * -1 ,
            SUM(A.monto_en_pesos)    * -1
    FROM    dis_provision A, ret_sol_issste_tot B
    WHERE   A.folio            = v_folio
    AND     A.folio            = B.folio 
    AND     A.nss              = B.nss_imss
    AND     A.consecutivo_lote = B.consecutivo
    AND    (B.status_sub_ret   = "01" OR  
            B.status_fondo_viv = "01")
    GROUP   BY 1,2

    FOREACH cur_4 INTO reg_7.*
        LET monto_accion_13 = 0
        LET monto_accion_19 = 0
        LET monto_pesos_14  = 0        

        LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"

        PREPARE eje_saldo_dia FROM v_saldo_dia

        DECLARE c_saldo  CURSOR FOR eje_saldo_dia
        FOREACH c_saldo  USING reg_7.nss_imss  ,
                               reg_7.subcuenta ,
                               v_grupo         ,
                               v_fecha
                         INTO  reg_13.subcuenta   ,
                               reg_13.siefore     ,
                               reg_13.monto_acc   ,
                               reg_13.monto_pesos

            CASE reg_7.subcuenta
                WHEN 13
                    LET monto_accion_13 = monto_accion_13 +
                                          reg_13.monto_acc
                WHEN 19
                    LET monto_accion_19 = monto_accion_19 +
                                          reg_13.monto_acc
                WHEN 14
                    LET monto_pesos_14  = monto_pesos_14  +
                                          reg_13.monto_pesos
            END CASE
        
        END FOREACH

        CASE reg_7.subcuenta
            WHEN 13
                IF reg_7.monto_en_acciones > monto_accion_13 THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 19
                IF reg_7.monto_en_acciones > monto_accion_19 THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
            WHEN 14
                SELECT saldo_pesos * (-1) 
                INTO   reg_7.monto_en_pesos
                FROM   ret_monto_sie_issste 
                WHERE  folio     = v_folio
                AND    nss       = reg_7.nss_imss
                AND    subcuenta = 14
                AND    siefore   = 0
               
                IF reg_7.monto_en_pesos > monto_pesos_14  THEN
                    LET sw_1 = 1
                    EXIT FOREACH
                END IF
        END CASE
    END FOREACH

    RETURN sw_1,reg_7.nss_imss

END FUNCTION


FUNCTION obtiene_precios_accion(fecha_precios)
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
        li_cont               SMALLINT

    LET li_cont = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION DIA ", fecha_precios, " < ENTER > PARA SALIR "
            PROMPT lc_mensaje CLIPPED FOR CHAR enter
            EXIT PROGRAM
        ELSE
            IF lr_precio_acc.siefore = 11 THEN
                LET gar_precio_acc[gs_viv].*  = lr_precio_acc.*
            ELSE
                LET gar_precio_acc[li_cont].* = lr_precio_acc.*
            END IF
        END IF

        LET li_cont = li_cont + 1
    END FOREACH
END FUNCTION


