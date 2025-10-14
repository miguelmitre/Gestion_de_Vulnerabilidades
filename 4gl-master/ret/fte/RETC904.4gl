################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa RETC904  => INCORPORA PAGO DE RETIROS-ISSSTE PARA PARCIALES          #
#Fecha creacion    => 23 DE OCTUBRE DE 2006                                    #
#By                => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
#Modifico          =>  XAVIER TORRES RIOS                                      #
#                  =>  25 de Noviembre 2007                                    #
#                      Se le asigno al estado solicitud de diagnosticado el    #
#                      valor de 137 ya que con ese valor viene de la operacion #
#                      37.                                                     #
#                  =>  XAVIER TORRES RIOS                                      #
#                      18 de Abril 2008                                        #
#                      Adaptacion para soporte de multisiefores                #
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
        recibido_op37     LIKE ret_estado.estado_solicitud ,
        liquidado         LIKE ret_estado.estado_solicitud
    END RECORD                                           

    DEFINE #glo #char
        nss_sobregirado   CHAR(011),
        v_desmarca        CHAR(100),
        v_liquida         CHAR(100),
        v_saldo_dia       CHAR(100),
        enter             CHAR(001),
        usuario           CHAR(008)

    DEFINE #glo #date
        HOY               DATE

    DEFINE #glo #integer
	cont_1            ,
	cont_reg          ,
        folio_oper_33     INTEGER

    DEFINE #glo #smallint
        sw_1              ,
        gs_viv            ,
        v_marca_ent       SMALLINT

    DEFINE #glo #dec
        d11_6_precio_dia  DECIMAL(11,6)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS 
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL init()
    OPEN WINDOW retc904 AT 4,4 WITH FORM "RETC9041" ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC904      INCORPORA PAGO DE RETIROS PARCIALES ISSSTE                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL valida_ejecucion() #ve

    INPUT BY NAME folio_oper_33 WITHOUT DEFAULTS
        AFTER FIELD folio_oper_33
            IF folio_oper_33 IS NULL  OR 
               folio_oper_33 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_33
            END IF
            
            SELECT "OK" 
            FROM   ret_sol_issste_par A
            WHERE  A.folio = folio_oper_33
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR "    EL FOLIO NO EXISTE...    "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_33
            END IF

	    SELECT "OK"
	    FROM   dis_cuenta  A
	    WHERE  A.folio = folio_oper_33
	    AND    A.tipo_movimiento IN (884,885)
	    GROUP BY 1       

	    IF STATUS <> NOTFOUND THEN
                ERROR "    FOLIO YA LIQUIDADO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_33
            END IF


        ON KEY (ESC)
            IF folio_oper_33 IS NULL OR
               folio_oper_33 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_33
            END IF

            SELECT "OK"
            FROM   ret_sol_issste_par A
            WHERE  A.folio = folio_oper_33
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " EL FOLIO NO EXISTE... "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_33
            END IF

	    SELECT "OK"
	    FROM   dis_cuenta  A
	    WHERE  A.folio = folio_oper_33
	    AND    A.tipo_movimiento IN (884,885)
	    GROUP BY 1       

	    IF STATUS <> NOTFOUND THEN
                ERROR "    FOLIO YA LIQUIDADO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_33
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT" PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT" PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO... < ENTER > PARA SALIR " 
                FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    CALL valida_sobregiro(folio_oper_33)
    RETURNING sw_1,nss_sobregirado

    IF sw_1 <> 0 THEN
        PROMPT  " ERROR AL LIQUIDAR, SE SOBREGIRA CUENTA NSS: ",nss_sobregirado
        FOR CHAR enter
            EXIT PROGRAM
        END IF

    SELECT  count(*)
    INTO    cont_1
    FROM    ret_sol_issste_par A  
    WHERE   A.folio          = folio_oper_33
    AND     A.estado_sub_ret = "01"

    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1      AT 11,20
    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()      #pp LIQUIDA
    CALL segundo_paso()     #sp DESMARCA

    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1   AT 11,20
    DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg AT 13,20
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter 

    CLOSE WINDOW retc904
END MAIN

FUNCTION init()
#--------------
    LET HOY      = TODAY
    LET cont_1   = 0
    LET cont_reg = 0

    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_2.recibido_op37
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER37"

    SELECT A.estado_solicitud
    INTO   reg_2.liquidado 
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT MAX(folio)
    INTO   folio_oper_33
    FROM   ret_sol_issste_par
    WHERE  estado_solicitud = reg_2.recibido_op37

    ----- DESMARCA -----
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    LET v_marca_ent = 0                        

    ----- LIQUIDA SIN CALCULO DE ISR -----
    LET v_liquida = " EXECUTE FUNCTION fn_liquida ( ?,?,?,?,? ) "
    PREPARE eje_liquida FROM v_liquida

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss_imss          LIKE ret_sol_issste_par.nss_imss        ,
        consecutivo       LIKE ret_sol_issste_par.consecutivo     ,
        subcuenta         LIKE dis_provision.subcuenta
    END RECORD

    DEFINE #loc #integer
        subcta_13         ,      
        subcta_19         INTEGER

    LET subcta_13 = 0
    LET subcta_19 = 0 

    SELECT  "OK"
    FROM    ret_sol_issste_par A  
    WHERE   A.folio          = folio_oper_33
    AND     A.estado_sub_ret = "01"
    AND     A.estado_solicitud = reg_2.recibido_op37
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT " NO EXISTEN REGISTROS A LIQUIDAR..<ENTER> PARA FINALIZAR"
        FOR CHAR enter
        EXIT PROGRAM
    END IF

    DECLARE cur_1 CURSOR FOR
        SELECT  A.nss              ,
                A.consecutivo_lote ,
                A.subcuenta        
        FROM    dis_provision A, ret_sol_issste_par B
        WHERE   A.folio            = folio_oper_33
        AND     A.nss              = B.nss_imss
        AND     A.consecutivo_lote = B.consecutivo
        AND     B.estado_sub_ret   = "01"
        AND     B.estado_solicitud = reg_2.recibido_op37
        GROUP BY 1,2,3

    FOREACH cur_1 INTO reg_3.*  
        LET cont_reg = cont_reg + 1

        CALL actualiza_cuenta_ind(folio_oper_33    ,
                                   reg_3.nss_imss  ,
                                   reg_3.subcuenta ) #aci

        UPDATE ret_sol_issste_par
        SET    ret_sol_issste_par.estado_solicitud = reg_2.liquidado
        WHERE  ret_sol_issste_par.nss_imss         = reg_3.nss_imss
        AND    ret_sol_issste_par.consecutivo      = reg_3.consecutivo

    END FOREACH

END FUNCTION

FUNCTION segundo_paso()
#sp--------------------
    DEFINE reg_4 RECORD #reg_4
        nss_imss              LIKE ret_sol_issste_par.nss_imss     ,
        consecutivo           LIKE ret_sol_issste_par.consecutivo  ,
        tipo_retiro           LIKE ret_sol_issste_par.tipo_retiro 
    END RECORD

    DEFINE reg_5 RECORD #loc #reg_5
        estado_marca         SMALLINT,
        marca_causa          SMALLINT
    END RECORD

    LET reg_5.estado_marca = 0
    LET reg_5.marca_causa  = 0

    DECLARE cur_2 CURSOR FOR
    SELECT A.nss_imss       ,
           A.consecutivo    ,
           A.tipo_retiro
    FROM   ret_sol_issste_par A
    WHERE  A.folio            = folio_oper_33
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
                          ,",' ",usuario,"')"

        PREPARE eje_desmarca FROM v_desmarca
        EXECUTE eje_desmarca

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

   DEFINE xnss   CHAR(11)

   UPDATE cta_ctr_cuenta
   SET    tipo_informe  = 5,
          fecha_informe = HOY
   WHERE  nss           = xnss

END FUNCTION


FUNCTION valida_sobregiro(v_folio)
#vs--------------------------------
    DEFINE reg_7 RECORD #loc #reg_7
        nss                 CHAR(11)      ,
        subcuenta           SMALLINT      ,
        siefore             SMALLINT      ,
        monto_en_acciones   DECIMAL(16,6)
    END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        subcuenta                    ,
        siefore             SMALLINT ,
        monto_acc                    ,
        monto_pesos         DECIMAL(16,6)
    END RECORD

    DEFINE  #loc #char
        v_retiro            CHAR(001)  

    DEFINE #loc #smallint
        v_grupo             ,
        v_subcuenta         SMALLINT

    DEFINE #loc #date
        v_fecha             DATE

    DEFINE #loc #integer
        sw_1                ,
        v_folio             INTEGER

    DEFINE #loc #decimal
        acc_sub13_sief1     ,
        acc_sub19_sief1     ,
        acc_sub13_sief2     ,
        acc_sub19_sief2     ,
        acc_sub13_sief3     ,
        acc_sub19_sief3     ,
        acc_sub13_sief4     ,
        acc_sub19_sief4     ,
        acc_sub13_sief5     ,
        acc_sub19_sief5     DECIMAL(16,6)

    LET sw_1             = 0
    LET v_fecha          = HOY
    LET v_grupo          = 0
    LET acc_sub13_sief1  = 0  
    LET acc_sub19_sief1  = 0 
    LET acc_sub13_sief2  = 0
    LET acc_sub19_sief2  = 0
    LET acc_sub13_sief3  = 0
    LET acc_sub19_sief3  = 0
    LET acc_sub13_sief4  = 0
    LET acc_sub19_sief4  = 0
    LET acc_sub13_sief5  = 0
    LET acc_sub19_sief5  = 0
    LET reg_7.monto_en_acciones = 0

    DECLARE  cur_4 CURSOR FOR
    SELECT  A.nss                    ,
            A.subcuenta              ,
            A.siefore                ,
            SUM(A.monto_en_acciones) * -1
    FROM    dis_provision A, ret_sol_issste_par B
    WHERE   A.folio            = v_folio 
    AND     A.nss              = B.nss_imss
    AND     A.consecutivo_lote = B.consecutivo
    AND     B.diag_issste      = 400
    GROUP   BY 1,2,3

    FOREACH cur_4 INTO reg_7.*
        LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"

        PREPARE eje_saldo_dia FROM v_saldo_dia

        DECLARE c_saldo  CURSOR FOR eje_saldo_dia
        FOREACH c_saldo  USING reg_7.nss       ,
                               reg_7.subcuenta ,
                               v_grupo         ,
                               v_fecha
                         INTO  reg_13.subcuenta   ,
                               reg_13.siefore     ,
                               reg_13.monto_acc   ,
                               reg_13.monto_pesos

            IF reg_13.siefore = 1 THEN
                CASE reg_13.subcuenta
                    WHEN 13
                        LET acc_sub13_sief1 = reg_13.monto_acc
                    WHEN 19
                        LET acc_sub19_sief1 = reg_13.monto_acc
                END CASE
            END IF

            IF reg_13.siefore = 2 THEN
                CASE reg_13.subcuenta
                    WHEN 13
                        LET acc_sub13_sief2 = reg_13.monto_acc
                    WHEN 19
                        LET acc_sub19_sief2 = reg_13.monto_acc
                END CASE
            END IF

            IF reg_13.siefore = 3 THEN
                CASE reg_13.subcuenta
                    WHEN 13
                        LET acc_sub13_sief3 = reg_13.monto_acc
                    WHEN 19
                        LET acc_sub19_sief3 = reg_13.monto_acc
                END CASE
            END IF

            IF reg_13.siefore = 4 THEN
                CASE reg_13.subcuenta
                    WHEN 13
                        LET acc_sub13_sief4 = reg_13.monto_acc
                    WHEN 19
                        LET acc_sub19_sief4 = reg_13.monto_acc
                END CASE
            END IF

            IF reg_13.siefore = 5 THEN
                CASE reg_13.subcuenta
                    WHEN 13
                        LET acc_sub13_sief5 = reg_13.monto_acc
                    WHEN 19
                        LET acc_sub19_sief5 = reg_13.monto_acc
                END CASE
            END IF
        END FOREACH

        IF reg_7.siefore = 1 THEN
            CASE reg_7.subcuenta
                WHEN 13
                    IF reg_7.monto_en_acciones > acc_sub13_sief1 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                WHEN 19
                    IF reg_7.monto_en_acciones > acc_sub19_sief1 THEN
                        LET sw_1 = 1 
                        EXIT FOREACH
                    END IF
            END CASE
        END IF

        IF reg_7.siefore = 2 THEN
            CASE reg_7.subcuenta
                WHEN 13
                    IF reg_7.monto_en_acciones > acc_sub13_sief2 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                WHEN 19
                    IF reg_7.monto_en_acciones > acc_sub19_sief2 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
            END CASE
        END IF

        IF reg_7.siefore = 3 THEN
            CASE reg_7.subcuenta
                WHEN 13
                    IF reg_7.monto_en_acciones > acc_sub13_sief3 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                WHEN 19
                    IF reg_7.monto_en_acciones > acc_sub19_sief3 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
            END CASE
        END IF

        IF reg_7.siefore = 4 THEN
            CASE reg_7.subcuenta
                WHEN 13
                    IF reg_7.monto_en_acciones > acc_sub13_sief4 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                WHEN 19
                    IF reg_7.monto_en_acciones > acc_sub19_sief4 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
            END CASE
        END IF

        IF reg_7.siefore = 5 THEN
            CASE reg_7.subcuenta
                WHEN 13
                    IF reg_7.monto_en_acciones > acc_sub13_sief5 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
                WHEN 19
                    IF reg_7.monto_en_acciones > acc_sub19_sief5 THEN
                        LET sw_1 = 1
                        EXIT FOREACH
                    END IF
            END CASE
        END IF
    END FOREACH

    RETURN sw_1,reg_7.nss
END FUNCTION


FUNCTION actualiza_cuenta_ind(vfolio,vnss,vsubcuenta)
#aci------------------------------------------------
    DEFINE  #loc  #integer
         vfolio               ,
         vsubcuenta           INTEGER

    DEFINE  #loc  #char
         vnss                 CHAR(11)

    DEFINE  #loc  #smallint
         cod_resp             SMALLINT

    DECLARE cur_liq2 CURSOR FOR eje_liquida
    OPEN cur_liq2  USING vfolio       ,#folio
                         vnss         ,#nss
                         vsubcuenta   ,#subcuenta
                         HOY          ,#fecha_liquida
                         HOY           #fecha_proceso
    FETCH cur_liq2 INTO cod_resp
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
                LET gar_precio_acc[li_cont].* = lr_precio_acc.*
            ELSE
                LET gar_precio_acc[li_cont].* = lr_precio_acc.*
            END IF
        END IF

        LET li_cont = li_cont + 1
    END FOREACH
END FUNCTION

FUNCTION valida_ejecucion()
#ve-------------------

    -- Valida y obtiene los precios de accion
    CALL obtiene_precios_accion(HOY)

END FUNCTION

