################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                #
#Owner             => E.F.P.                                                   #
#Programa RETC827  => GENERA NOTIFICACION DE RETIRO (PAGO EFECTUADO) (16)      #
#Fecha creacion    => 27 DE FEBRERO DEL 2004                                   #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 17 DE MARZO DEL 2008                                     #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 19 DE MARZO DE 2008                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Correccion de errores en la generacion del formato       #
#                     y del nombre del layout                                  #
#Sistema           => RET                                                      #
################################################################################

DATABASE safre_af
GLOBALS
    DEFINE #glo #g_seg_modulo
        g_seg_modulo          RECORD LIKE seg_modulo.*

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper13          INTEGER
    END RECORD

    DEFINE lr_precio_acc RECORD #glo #lr_precio_acc
        estado                SMALLINT  ,
        fecha                 DATE      ,
        siefore               SMALLINT  ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        procesado             LIKE ret_estado.estado_solicitud
    END RECORD
    
    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD
    
    DEFINE #glo #date
        HOY                   ,
        dia_habil             DATE

    DEFINE #glo #char
        v_valida_precio       CHAR(200) ,
        borra_lineas          CHAR(100) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        v_desmarca            CHAR(100) ,
        v_parcial             CHAR(300) ,
        v_parcial_75          CHAR(300) ,
        v_saldo_dia           CHAR(300) ,
        v_saldo_dia_mat       CHAR(300) ,
        v_liq_parcial         CHAR(300) ,
        c40_paterno           CHAR(040) ,
        c40_materno           CHAR(040) ,
        c40_nombres           CHAR(040) ,
        elimina               CHAR(100) ,
        c110_chmod            CHAR(110) ,
        G_LISTA               CHAR(100) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        enter                 CHAR(001) ,
        usuario               CHAR(012) ,
        v_marca               CHAR(100) ,
        v_precios_accion      CHAR(100) ,
        lc_mensaje            CHAR(100) ,
        v_habil_siguiente     CHAR(100)
        
    DEFINE #glo #smallint
        v_marca_ent           ,
        num_dia_habil         ,
        s_codigo_afore        SMALLINT


    DEFINE #glo #integer
        cont_1                ,
        cont_reg_av           INTEGER
      
    DEFINE #glo #decimal
        ---
        monto_pesos_cv        DECIMAL(16,2) ,
        monto_pesos_rcv       DECIMAL(12,2) ,
        ---
        d2tot_monto_pesos     DECIMAL(15,2) ,
        porc10_pesos_rcv      DECIMAL(16,6) ,
        d11_6_precio_dia_s1   DECIMAL(16,6) ,
        d11_6_precio_dia_s2   DECIMAL(16,6) ,
        d11_6_precio_dia_s3   DECIMAL(16,6) ,
        d11_6_precio_dia_s4   DECIMAL(16,6) ,
        d11_6_precio_dia_s5   DECIMAL(16,6) ,
        d11_6_precio_dia_s11  DECIMAL(16,6) ,
        s1monto_acc_cv_10     DECIMAL(16,6) ,
        s2monto_acc_cv_10     DECIMAL(16,6) ,
        s3monto_acc_cv_10     DECIMAL(16,6) ,
        s4monto_acc_cv_10     DECIMAL(16,6) ,
        s5monto_acc_cv_10     DECIMAL(16,6) ,
        s1monto_acc_cv_75     DECIMAL(16,6) ,
        s2monto_acc_cv_75     DECIMAL(16,6) ,
        s3monto_acc_cv_75     DECIMAL(16,6) ,
        s4monto_acc_cv_75     DECIMAL(16,6) ,
        s5monto_acc_cv_75     DECIMAL(16,6)
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
        CALL STARTLOG("RETC827.log")

    CALL init() #i
    OPEN WINDOW retc827 AT 4,4 WITH FORM "RETC8271" ATTRIBUTE(BORDER)
    DISPLAY " RETC827   GENERA NOTIFICACION DE RETIRO (PAGO EFECTUADO) (16)                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.folio_oper13 WITHOUT DEFAULTS
        AFTER FIELD folio_oper13
            IF reg_1.folio_oper13 IS NULL  OR 
               reg_1.folio_oper13 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper13
            ELSE
                SELECT "OK"
                FROM   ret_parcial_tx  
                WHERE  folio = reg_1.folio_oper13
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR"    FOLIO INEXISTENTE" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper13
                END IF
            END IF

        ON KEY (ESC)
            IF reg_1.folio_oper13 IS NULL  OR 
               reg_1.folio_oper13 <= 0 THEN
                ERROR "    CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper13
            ELSE
                SELECT "OK"
                FROM   ret_parcial_tx  
                WHERE  folio = reg_1.folio_oper13
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR"    FOLIO INEXISTENTE" ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper13
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio            = reg_1.folio_oper13
            AND    tipo_movimiento IN (870, 875) --870 Retiro por Matrimonio
            GROUP BY 1                           --875 Retiro por Desempleo
            
            IF STATUS <> NOTFOUND THEN
               ERROR"    FOLIO YA LIQUIDADO " ATTRIBUTE(NORMAL)
               NEXT FIELD folio_oper13
        END IF
        EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
            
                LET v_valida_precio = " EXECUTE FUNCTION fn_valida_existe_precio() "
                PREPARE eje_valida_precio FROM v_valida_precio
                
                DECLARE cur_sp1 CURSOR FOR eje_valida_precio
                OPEN cur_sp1
                    FETCH cur_sp1 INTO reg_12.estado    ,
                                       reg_12.fecha_rcv ,
                                       reg_12.fecha_viv 
                CLOSE cur_sp1
                          
                IF reg_12.estado = 1 THEN
                    PROMPT " FALTA PRECIO DE ACCION FECHA ",reg_12.fecha_rcv USING "DD-MM-yyyy",", VIV:",reg_12.fecha_viv USING"DD-MM-YYYY"
                    FOR CHAR ENTER
                    EXIT PROGRAM 
                END IF
               
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    SELECT  COUNT(*)
    INTO    cont_1
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     A.nss             = B.nss
    AND     A.consecutivo     = B.consecutivo
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada

    DISPLAY " TOTAL REGISTROS A PROCESAR : ",cont_1      AT 11,21
    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()  #pp -----RETIRO POR DESEMPLEO
    CALL segundo_paso() #sp -----RETIRO POR MATRIMONIO
    CALL tercer_paso()  #tp -----INSERTA EN ret_sum_envio
    CALL cuarto_paso()  #cp -----GENERA ARCHIVO PLANO

    CLOSE WINDOW retc827

    OPEN WINDOW retc827 AT 4,4 WITH FORM "RETC8271" ATTRIBUTE(BORDER)
    DISPLAY " RETC827   GENERA NOTIFICACION DE RETIRO (PAGO EFECTUADO) (16)                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    DISPLAY "FOLIO NUMERO               : ",reg_1.folio_oper13  AT 08,21
    DISPLAY "TOTAL REGISTROS A PROCESAR : ",cont_1              AT 10,21
    DISPLAY "TOTAL REGISTROS PROCESADOS : ",cont_reg_av         AT 11,21
    
    DISPLAY "EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 13,21 
    DISPLAY G_LISTA CLIPPED AT 14,21 
    DISPLAY "CON EL NOMBRE : ",c12_nombre_plano AT 16,21
     
    PROMPT  " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc827
END MAIN


FUNCTION init()
#--------------
--    LET HOY               = TODAY

    LET HOY               = "09/23/2008"

    LET cont_1            = 0
    LET cont_reg_av       = 0
    LET d2tot_monto_pesos = 0

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   reg_2.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
   
    --CARGA LOS PRECIOS DE ACCIÓN DE TODAS LAS SIEFORES--------------
    LET v_precios_accion  = "EXECUTE FUNCTION fn_verifica_precio_accion (?)"

    PREPARE eje_precios_accion FROM v_precios_accion
    DECLARE cur_10 CURSOR FOR eje_precios_accion
    
    FOREACH cur_10 USING HOY INTO lr_precio_acc.*
        CASE lr_precio_acc.siefore
            WHEN 1
                LET d11_6_precio_dia_s1 = lr_precio_acc.precio_dia
            WHEN 2
                LET d11_6_precio_dia_s2 = lr_precio_acc.precio_dia
            WHEN 3
                LET d11_6_precio_dia_s3 = lr_precio_acc.precio_dia
            WHEN 4
                LET d11_6_precio_dia_s4 = lr_precio_acc.precio_dia
            WHEN 5
                LET d11_6_precio_dia_s5 = lr_precio_acc.precio_dia
        END CASE
    END FOREACH
    -----------------------------------------------------------------

{
    --DETERMINA EL DÍA HÁBIL SIGUIENTE-------------------------------
    LET num_dia_habil     = 1
    LET v_habil_siguiente = "EXECUTE FUNCTION fn_habil_siguiente (?,?)"

    PREPARE eje_habil_sig FROM v_habil_siguiente
    DECLARE cur_20 CURSOR FOR eje_habil_sig
    OPEN cur_20 USING HOY           ,---dia habil
                      num_dia_habil  ---habil_siguiente
    
        FETCH cur_20 INTO dia_habil
    CLOSE cur_20
    -----------------------------------------------------------------
}

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DETAV16"
    LET c12_nombre_plano = HOY USING "YYYYMMDD", ".16P"

    SELECT MAX(folio)
    INTO   reg_1.folio_oper13
    FROM   ret_parcial_tx

END FUNCTION


FUNCTION primer_paso() 
#pp-------------------
    DEFINE reg_10 RECORD #loc #reg_10
        nss                   LIKE ret_parcial_tx.nss         ,
        consecutivo           LIKE ret_parcial.consecutivo    ,
        salario_base_cot      LIKE ret_parcial.salario_base_cot
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        s1_acciones_ret97     DECIMAL(16,6) ,   
        s1_acciones_cv        DECIMAL(16,6) ,
        s1_acciones_so        DECIMAL(16,6) ,
        s1_acciones_est       DECIMAL(16,6) ,
        s1_acciones_esp       DECIMAL(16,6) ,

        s1_pesos_ret97        DECIMAL(16,6) ,
        s1_pesos_cv           DECIMAL(16,6) ,
        s1_pesos_so           DECIMAL(16,6) ,
        s1_pesos_est          DECIMAL(16,6) ,
        s1_pesos_esp          DECIMAL(16,6) ,
        s1_pesos_rcv          DECIMAL(16,6) 
    END RECORD

    DEFINE reg_5 RECORD #glo #reg_5
        s2_acciones_ret97     DECIMAL(16,6) ,
        s2_acciones_cv        DECIMAL(16,6) ,
        s2_acciones_so        DECIMAL(16,6) ,
        s2_acciones_est       DECIMAL(16,6) ,
        s2_acciones_esp       DECIMAL(16,6) ,

        s2_pesos_ret97        DECIMAL(16,6) ,
        s2_pesos_cv           DECIMAL(16,6) ,
        s2_pesos_so           DECIMAL(16,6) ,
        s2_pesos_est          DECIMAL(16,6) ,
        s2_pesos_esp          DECIMAL(16,6) ,
        s2_pesos_rcv          DECIMAL(16,6) 
    END RECORD

    DEFINE reg_23 RECORD #glo #reg_23
        s3_acciones_ret97     DECIMAL(16,6) ,
        s3_acciones_cv        DECIMAL(16,6) ,
        s3_acciones_so        DECIMAL(16,6) ,
        s3_acciones_est       DECIMAL(16,6) ,
        s3_acciones_esp       DECIMAL(16,6) ,

        s3_pesos_ret97        DECIMAL(16,6) ,
        s3_pesos_cv           DECIMAL(16,6) ,
        s3_pesos_so           DECIMAL(16,6) ,
        s3_pesos_est          DECIMAL(16,6) ,
        s3_pesos_esp          DECIMAL(16,6) ,
        s3_pesos_rcv          DECIMAL(16,6) 
    END RECORD

    DEFINE reg_24 RECORD #glo #reg_24
        s4_acciones_ret97     DECIMAL(16,6) ,
        s4_acciones_cv        DECIMAL(16,6) ,
        s4_acciones_so        DECIMAL(16,6) ,
        s4_acciones_est       DECIMAL(16,6) ,
        s4_acciones_esp       DECIMAL(16,6) ,

        s4_pesos_ret97        DECIMAL(16,6) ,
        s4_pesos_cv           DECIMAL(16,6) ,
        s4_pesos_so           DECIMAL(16,6) ,
        s4_pesos_est          DECIMAL(16,6) ,
        s4_pesos_esp          DECIMAL(16,6) ,
        s4_pesos_rcv          DECIMAL(16,6) 
    END RECORD

    DEFINE reg_25 RECORD #glo #reg_25
        s5_acciones_ret97     DECIMAL(16,6) ,
        s5_acciones_cv        DECIMAL(16,6) ,
        s5_acciones_so        DECIMAL(16,6) ,
        s5_acciones_est       DECIMAL(16,6) ,
        s5_acciones_esp       DECIMAL(16,6) ,

        s5_pesos_ret97        DECIMAL(16,6) ,
        s5_pesos_cv           DECIMAL(16,6) ,
        s5_pesos_so           DECIMAL(16,6) ,
        s5_pesos_est          DECIMAL(16,6) ,
        s5_pesos_esp          DECIMAL(16,6) ,
        s5_pesos_rcv          DECIMAL(16,6) 
    END RECORD
 
    DEFINE reg_6 RECORD #glo #reg_6
        s1_acc_ret97_10       DECIMAL(16,6) ,
        s1_acc_cv_10          DECIMAL(16,6) , 
        s1_acc_so_10          DECIMAL(16,6) ,
        s1_acc_est_10         DECIMAL(16,6) ,
        s1_acc_esp_10         DECIMAL(16,6) 
    END RECORD

    DEFINE reg_7 RECORD #glo #reg_7
        s2_acc_ret97_10       DECIMAL(16,6) , 
        s2_acc_cv_10          DECIMAL(16,6) , 
        s2_acc_so_10          DECIMAL(16,6) , 
        s2_acc_est_10         DECIMAL(16,6) , 
        s2_acc_esp_10         DECIMAL(16,6) 
    END RECORD

    DEFINE reg_26 RECORD #glo #reg_26
        s3_acc_ret97_10       DECIMAL(16,6) , 
        s3_acc_cv_10          DECIMAL(16,6) , 
        s3_acc_so_10          DECIMAL(16,6) , 
        s3_acc_est_10         DECIMAL(16,6) , 
        s3_acc_esp_10         DECIMAL(16,6) 
    END RECORD

    DEFINE reg_27 RECORD #glo #reg_27
        s4_acc_ret97_10       DECIMAL(16,6) , 
        s4_acc_cv_10          DECIMAL(16,6) , 
        s4_acc_so_10          DECIMAL(16,6) , 
        s4_acc_est_10         DECIMAL(16,6) , 
        s4_acc_esp_10         DECIMAL(16,6) 
    END RECORD

    DEFINE reg_28 RECORD #glo #reg_28
        s5_acc_ret97_10       DECIMAL(16,6) , 
        s5_acc_cv_10          DECIMAL(16,6) , 
        s5_acc_so_10          DECIMAL(16,6) , 
        s5_acc_est_10         DECIMAL(16,6) , 
        s5_acc_esp_10         DECIMAL(16,6) 
    END RECORD

    DEFINE reg_8 RECORD #glo #reg_8
        s1_acc_ret97_75       DECIMAL(16,6) , 
        s1_acc_cv_75          DECIMAL(16,6) , 
        s1_acc_so_75          DECIMAL(16,6) , 
        s1_acc_est_75         DECIMAL(16,6) , 
        s1_acc_esp_75         DECIMAL(16,6)
    END RECORD
    
    DEFINE reg_9 RECORD #glo #reg_9
        s2_acc_ret97_75       DECIMAL(16,6) , 
        s2_acc_cv_75          DECIMAL(16,6) , 
        s2_acc_so_75          DECIMAL(16,6) , 
        s2_acc_est_75         DECIMAL(16,6) , 
        s2_acc_esp_75         DECIMAL(16,6)  
    END RECORD

    DEFINE reg_20 RECORD #glo #reg_20
        s3_acc_ret97_75       DECIMAL(16,6) , 
        s3_acc_cv_75          DECIMAL(16,6) , 
        s3_acc_so_75          DECIMAL(16,6) , 
        s3_acc_est_75         DECIMAL(16,6) , 
        s3_acc_esp_75         DECIMAL(16,6)  
    END RECORD

    DEFINE reg_21 RECORD #glo #reg_21
        s4_acc_ret97_75       DECIMAL(16,6) , 
        s4_acc_cv_75          DECIMAL(16,6) , 
        s4_acc_so_75          DECIMAL(16,6) , 
        s4_acc_est_75         DECIMAL(16,6) , 
        s4_acc_esp_75         DECIMAL(16,6)  
    END RECORD

    DEFINE reg_22 RECORD #glo #reg_22
        s5_acc_ret97_75       DECIMAL(16,6) , 
        s5_acc_cv_75          DECIMAL(16,6) , 
        s5_acc_so_75          DECIMAL(16,6) , 
        s5_acc_est_75         DECIMAL(16,6) , 
        s5_acc_esp_75         DECIMAL(16,6)  
    END RECORD

    DEFINE #loc #decimal
        salario_75_dias       DECIMAL(16,2) ,
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6) ,
        f_saldo_s1            DECIMAL(16,6) ,
        f_saldo_s2            DECIMAL(16,6) ,
        f_saldo_s1_75         DECIMAL(16,6) ,
        f_saldo_s2_75         DECIMAL(16,6) ,
        f_acc_pago_s1         DECIMAL(16,6) ,
        f_acc_pago_s2         DECIMAL(16,6) ,
        f_acc_pago_s1_75      DECIMAL(16,6) ,
        f_acc_pago_s2_75      DECIMAL(16,6) ,
        f_pes_pago_s1_75      DECIMAL(16,6) ,
        f_pes_pago_s2_75      DECIMAL(16,6) ,
        d16_pesos_pago_s1     DECIMAL(16,6) ,
        d16_pesos_pago_s2     DECIMAL(16,6) ,
        d16_pesos_pago_s3     DECIMAL(16,6) ,
        d16_pesos_pago_s4     DECIMAL(16,6) ,
        d16_pesos_pago_s5     DECIMAL(16,6) ,
        d2pago_desempleo      DECIMAL(10,2) ,
        monto_pesos_sub1_2    DECIMAL(10,2) ,
        monto_pesos_sub5_2    DECIMAL(10,2) 

    DEFINE #loc #smallint
        sw_1                  ,
        sw_2                  ,
        sw_3                  ,
        sw_4                  ,
        sw_5                  ,
        sw_6                  ,
        v_subcuenta           ,
        v_grupo               ,
        f_subcta              ,
        f_subcuenta           ,
        f_siefore             ,
        siefore1              ,
        siefore2              , 
        siefore1_75           ,
        siefore2_75           SMALLINT

    WHENEVER ERROR CONTINUE
        LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/DETAV16"
        RUN elimina
    WHENEVER ERROR STOP

    SELECT  "OK"
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.tipo_prestacion = 6   --Tipo de prestacion 6 es por desempleo
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_2 CURSOR FOR
    SELECT  B.nss            ,
            B.consecutivo    ,
            A.salario_base_cot
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.tipo_prestacion = 6
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada

    FOREACH cur_2 INTO reg_10.*
        ----
        --display "estoy en desempleo : ", reg_10.nss
        --sleep 8
        ----
        LET cont_reg_av             = cont_reg_av + 1

        LET salario_75_dias         = 0

        LET reg_4.s1_acciones_ret97 = 0
        LET reg_4.s1_acciones_cv    = 0
        LET reg_4.s1_acciones_so    = 0
        LET reg_4.s1_acciones_est   = 0
        LET reg_4.s1_acciones_esp   = 0
    
        LET reg_4.s1_pesos_ret97    = 0
        LET reg_4.s1_pesos_cv       = 0
        LET reg_4.s1_pesos_so       = 0
        LET reg_4.s1_pesos_est      = 0
        LET reg_4.s1_pesos_esp      = 0
        LET reg_4.s1_pesos_rcv      = 0

        LET reg_5.s2_acciones_ret97 = 0
        LET reg_5.s2_acciones_cv    = 0
        LET reg_5.s2_acciones_so    = 0
        LET reg_5.s2_acciones_est   = 0
        LET reg_5.s2_acciones_esp   = 0

        LET reg_5.s2_pesos_ret97    = 0
        LET reg_5.s2_pesos_cv       = 0
        LET reg_5.s2_pesos_so       = 0
        LET reg_5.s2_pesos_est      = 0
        LET reg_5.s2_pesos_esp      = 0
        LET reg_5.s2_pesos_rcv      = 0

        LET reg_23.s3_acciones_ret97 = 0
        LET reg_23.s3_acciones_cv    = 0
        LET reg_23.s3_acciones_so    = 0
        LET reg_23.s3_acciones_est   = 0
        LET reg_23.s3_acciones_esp   = 0

        LET reg_23.s3_pesos_ret97    = 0
        LET reg_23.s3_pesos_cv       = 0
        LET reg_23.s3_pesos_so       = 0
        LET reg_23.s3_pesos_est      = 0
        LET reg_23.s3_pesos_esp      = 0
        LET reg_23.s3_pesos_rcv      = 0

        LET reg_24.s4_acciones_ret97 = 0
        LET reg_24.s4_acciones_cv    = 0
        LET reg_24.s4_acciones_so    = 0
        LET reg_24.s4_acciones_est   = 0
        LET reg_24.s4_acciones_esp   = 0

        LET reg_24.s4_pesos_ret97    = 0
        LET reg_24.s4_pesos_cv       = 0
        LET reg_24.s4_pesos_so       = 0
        LET reg_24.s4_pesos_est      = 0
        LET reg_24.s4_pesos_esp      = 0
        LET reg_24.s4_pesos_rcv      = 0

        LET reg_25.s5_acciones_ret97 = 0
        LET reg_25.s5_acciones_cv    = 0
        LET reg_25.s5_acciones_so    = 0
        LET reg_25.s5_acciones_est   = 0
        LET reg_25.s5_acciones_esp   = 0

        LET reg_25.s5_pesos_ret97    = 0
        LET reg_25.s5_pesos_cv       = 0
        LET reg_25.s5_pesos_so       = 0
        LET reg_25.s5_pesos_est      = 0
        LET reg_25.s5_pesos_esp      = 0
        LET reg_25.s5_pesos_rcv      = 0

        LET reg_6.s1_acc_ret97_10   = 0
        LET reg_6.s1_acc_cv_10      = 0 
        LET reg_6.s1_acc_so_10      = 0
        LET reg_6.s1_acc_est_10     = 0
        LET reg_6.s1_acc_esp_10     = 0
        
        LET reg_7.s2_acc_ret97_10   = 0
        LET reg_7.s2_acc_cv_10      = 0
        LET reg_7.s2_acc_so_10      = 0
        LET reg_7.s2_acc_est_10     = 0
        LET reg_7.s2_acc_esp_10     = 0

        LET reg_26.s3_acc_ret97_10   = 0
        LET reg_26.s3_acc_cv_10      = 0
        LET reg_26.s3_acc_so_10      = 0
        LET reg_26.s3_acc_est_10     = 0
        LET reg_26.s3_acc_esp_10     = 0

        LET reg_27.s4_acc_ret97_10   = 0
        LET reg_27.s4_acc_cv_10      = 0
        LET reg_27.s4_acc_so_10      = 0
        LET reg_27.s4_acc_est_10     = 0
        LET reg_27.s4_acc_esp_10     = 0

        LET reg_28.s5_acc_ret97_10   = 0
        LET reg_28.s5_acc_cv_10      = 0
        LET reg_28.s5_acc_so_10      = 0
        LET reg_28.s5_acc_est_10     = 0
        LET reg_28.s5_acc_esp_10     = 0

        LET reg_8.s1_acc_ret97_75   = 0
        LET reg_8.s1_acc_cv_75      = 0
        LET reg_8.s1_acc_so_75      = 0
        LET reg_8.s1_acc_est_75     = 0
        LET reg_8.s1_acc_esp_75     = 0

        LET reg_9.s2_acc_ret97_75   = 0
        LET reg_9.s2_acc_cv_75      = 0
        LET reg_9.s2_acc_so_75      = 0
        LET reg_9.s2_acc_est_75     = 0
        LET reg_9.s2_acc_esp_75     = 0

        LET reg_20.s3_acc_ret97_75   = 0
        LET reg_20.s3_acc_cv_75      = 0
        LET reg_20.s3_acc_so_75      = 0
        LET reg_20.s3_acc_est_75     = 0
        LET reg_20.s3_acc_esp_75     = 0

        LET reg_21.s4_acc_ret97_75   = 0
        LET reg_21.s4_acc_cv_75      = 0
        LET reg_21.s4_acc_so_75      = 0
        LET reg_21.s4_acc_est_75     = 0
        LET reg_21.s4_acc_esp_75     = 0

        LET reg_22.s5_acc_ret97_75   = 0
        LET reg_22.s5_acc_cv_75      = 0
        LET reg_22.s5_acc_so_75      = 0
        LET reg_22.s5_acc_est_75     = 0
        LET reg_22.s5_acc_esp_75     = 0

        LET sw_1                    = 0
        LET sw_2                    = 0        
        LET v_subcuenta             = 0
        LET v_grupo                 = 0
        LET d2pago_desempleo        = 0

        --OBTIENE EL SALDO POR NSS DE CADA UNA DE LAS SUBCUENTAS--------------
        LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
 
        PREPARE eje_saldo_dia FROM v_saldo_dia
        DECLARE c_saldo CURSOR FOR eje_saldo_dia

        FOREACH c_saldo  USING reg_10.nss   ,
                               v_subcuenta  , --Si subcta. y grupo son 0, se
                               v_grupo      , --obtiene saldo de todas las
                               HOY            --subcuentas.
                         INTO
                               f_subcuenta  ,
                               f_siefore    ,
                               f_monto_acc  ,
                               f_monto_pes

            IF f_subcuenta = 1 OR f_subcuenta = 2 OR f_subcuenta = 5 OR
               f_subcuenta = 6 OR f_subcuenta = 9 THEN

                IF f_monto_acc IS NULL OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                    LET f_monto_pes = 0
                ELSE
                    CASE f_siefore
                        WHEN 1   ---Siefore  1 --- 
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_4.s1_acciones_ret97 = f_monto_acc 
                                    LET reg_4.s1_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_4.s1_acciones_cv    = f_monto_acc
                                    LET reg_4.s1_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_4.s1_acciones_so    = f_monto_acc
                                    LET reg_4.s1_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_4.s1_acciones_est   = f_monto_acc
                                    LET reg_4.s1_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_4.s1_acciones_esp   = f_monto_acc
                                    LET reg_4.s1_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 2   ---Siefore  2 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_5.s2_acciones_ret97 = f_monto_acc
                                    LET reg_5.s2_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_5.s2_acciones_cv    = f_monto_acc
                                    LET reg_5.s2_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_5.s2_acciones_so    = f_monto_acc
                                    LET reg_5.s2_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_5.s2_acciones_est   = f_monto_acc
                                    LET reg_5.s2_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_5.s2_acciones_esp   = f_monto_acc
                                    LET reg_5.s2_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 3   ---Siefore  3 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_23.s3_acciones_ret97 = f_monto_acc
                                    LET reg_23.s3_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_23.s3_acciones_cv    = f_monto_acc
                                    LET reg_23.s3_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_23.s3_acciones_so    = f_monto_acc
                                    LET reg_23.s3_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_23.s3_acciones_est   = f_monto_acc
                                    LET reg_23.s3_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_23.s3_acciones_esp   = f_monto_acc
                                    LET reg_23.s3_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 4   ---Siefore  4 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_24.s4_acciones_ret97 = f_monto_acc
                                    LET reg_24.s4_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_24.s4_acciones_cv    = f_monto_acc
                                    LET reg_24.s4_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_24.s4_acciones_so    = f_monto_acc
                                    LET reg_24.s4_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_24.s4_acciones_est   = f_monto_acc
                                    LET reg_24.s4_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_24.s4_acciones_esp   = f_monto_acc
                                    LET reg_24.s4_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 5   ---Siefore  5 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_25.s5_acciones_ret97 = f_monto_acc
                                    LET reg_25.s5_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_25.s5_acciones_cv    = f_monto_acc
                                    LET reg_25.s5_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_25.s5_acciones_so    = f_monto_acc
                                    LET reg_25.s5_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_25.s5_acciones_est   = f_monto_acc
                                    LET reg_25.s5_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_25.s5_acciones_esp   = f_monto_acc
                                    LET reg_25.s5_pesos_esp      = f_monto_pes
                            END CASE
                    END CASE
                END IF
            END IF
        END FOREACH

        LET monto_pesos_cv   = reg_4.s1_pesos_cv  +
                               reg_5.s2_pesos_cv  +
                               reg_23.s3_pesos_cv  +
                               reg_24.s4_pesos_cv  +
                               reg_25.s5_pesos_cv  +
                               reg_4.s1_pesos_est +
                               reg_5.s2_pesos_est +
                               reg_23.s3_pesos_est +
                               reg_24.s4_pesos_est +
                               reg_25.s5_pesos_est +
                               reg_4.s1_pesos_esp +
                               reg_23.s3_pesos_esp +
                               reg_24.s4_pesos_esp +
                               reg_25.s5_pesos_esp +
                               reg_5.s2_pesos_esp

        LET monto_pesos_rcv  = reg_4.s1_pesos_ret97 +
                   reg_5.s2_pesos_ret97 +
                   reg_23.s3_pesos_ret97 +
                   reg_24.s4_pesos_ret97 +
                   reg_25.s5_pesos_ret97 +
                   monto_pesos_cv       +
                               reg_4.s1_pesos_so    +
                               reg_23.s3_pesos_so    +
                               reg_24.s4_pesos_so    +
                               reg_25.s5_pesos_so    +
                   reg_5.s2_pesos_so 
                             
        LET salario_75_dias  = reg_10.salario_base_cot * 75
        LET porc10_pesos_rcv = monto_pesos_rcv * 0.1

        ---CALCULA EL 10 % PARA DESEMPLEO---------------------------------------
        IF porc10_pesos_rcv < salario_75_dias THEN
            LET v_parcial = "EXECUTE FUNCTION fn_calcula_parcial_10_ms ( ? )"
            PREPARE eje_parcial FROM v_parcial

            DECLARE cur_4 CURSOR FOR eje_parcial
            FOREACH cur_4 USING reg_10.nss
                { INTO f_subcta      ,--subcuenta 
                     f_saldo_s1    ,--saldo siefore1
                     f_saldo_s2    ,--saldo siefore2
                     siefore1      ,--codigo siefore1
                     f_acc_pago_s1 ,--total siefore1
                     siefore2      ,--codigo siefore2
                     f_acc_pago_s2  --total siefore2 }
 
                 INTO f_subcta      ,--subcuenta 
                      siefore1      ,--codigo siefore1
                      f_saldo_s1    ,--saldo siefore1
                      f_acc_pago_s1    --total siefore1

                LET sw_1 = 0
                LET sw_2 = 0
                LET sw_3 = 0
                LET sw_4 = 0
                LET sw_5 = 0

                CASE siefore1 
                     WHEN 1
                        IF f_acc_pago_s1 > 0 THEN
                           LET sw_1              = 1
                           LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                                   d11_6_precio_dia_s1
                    
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1            ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s1 ,#p_accion
                                              -f_acc_pago_s1      ,#mto_acc
                                              -d16_pesos_pago_s1  ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                           LET f_acc_pago_s1 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                                LET reg_6.s1_acc_ret97_10 = f_acc_pago_s1
                             WHEN 2
                                LET reg_6.s1_acc_cv_10    = f_acc_pago_s1
                             WHEN 5
                                LET reg_6.s1_acc_so_10    = f_acc_pago_s1
                             WHEN 6
                                LET reg_6.s1_acc_est_10   = f_acc_pago_s1
                             WHEN 9
                                LET reg_6.s1_acc_esp_10   = f_acc_pago_s1
                        END CASE
                    
                     WHEN 2
                          IF f_acc_pago_s1 > 0 THEN
                             LET sw_2              = 1
                             LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s2
                    
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1           ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s2 ,#p_accion
                                              -f_acc_pago_s1      ,#mto_acc
                                              -d16_pesos_pago_s1  ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                          ELSE
                             LET f_acc_pago_s1 = 0
                          END IF
                          CASE f_subcta
                             WHEN 1
                                LET reg_7.s2_acc_ret97_10 = f_acc_pago_s1
                             WHEN 2
                                LET reg_7.s2_acc_cv_10    = f_acc_pago_s1
                             WHEN 5
                                LET reg_7.s2_acc_so_10    = f_acc_pago_s1
                             WHEN 6
                                LET reg_7.s2_acc_est_10   = f_acc_pago_s1
                             WHEN 9
                                LET reg_7.s2_acc_esp_10   = f_acc_pago_s1
                          END CASE

                     WHEN 3
                          IF f_acc_pago_s1 > 0 THEN
                             LET sw_3              = 1
                             LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s3
                    
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1           ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s3 ,#p_accion
                                              -f_acc_pago_s1      ,#mto_acc
                                              -d16_pesos_pago_s1  ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                          ELSE
                             LET f_acc_pago_s1 = 0
                          END IF
                          CASE f_subcta
                             WHEN 1
                                LET reg_26.s3_acc_ret97_10 = f_acc_pago_s1
                             WHEN 2
                                LET reg_26.s3_acc_cv_10    = f_acc_pago_s1
                             WHEN 5
                                LET reg_26.s3_acc_so_10    = f_acc_pago_s1
                             WHEN 6
                                LET reg_26.s3_acc_est_10   = f_acc_pago_s1
                             WHEN 9
                                LET reg_26.s3_acc_esp_10   = f_acc_pago_s1
                          END CASE
                     WHEN 4
                          IF f_acc_pago_s1 > 0 THEN
                             LET sw_4              = 1
                             LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s4
                    
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1           ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s4 ,#p_accion
                                              -f_acc_pago_s1      ,#mto_acc
                                              -d16_pesos_pago_s1  ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                          ELSE
                             LET f_acc_pago_s1 = 0
                          END IF
                          CASE f_subcta
                             WHEN 1
                                LET reg_27.s4_acc_ret97_10 = f_acc_pago_s1
                             WHEN 2
                                LET reg_27.s4_acc_cv_10    = f_acc_pago_s1
                             WHEN 5
                                LET reg_27.s4_acc_so_10    = f_acc_pago_s1
                             WHEN 6
                                LET reg_27.s4_acc_est_10   = f_acc_pago_s1
                             WHEN 9
                                LET reg_27.s4_acc_esp_10   = f_acc_pago_s1
                          END CASE
                     WHEN 5
                          IF f_acc_pago_s1 > 0 THEN
                             LET sw_5              = 1
                             LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s5
                    
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1           ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s5 ,#p_accion
                                              -f_acc_pago_s1      ,#mto_acc
                                              -d16_pesos_pago_s1  ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                          ELSE
                             LET f_acc_pago_s1 = 0
                          END IF
                          CASE f_subcta
                             WHEN 1
                                LET reg_28.s5_acc_ret97_10 = f_acc_pago_s1
                             WHEN 2
                                LET reg_28.s5_acc_cv_10    = f_acc_pago_s1
                             WHEN 5
                                LET reg_28.s5_acc_so_10    = f_acc_pago_s1
                             WHEN 6
                                LET reg_28.s5_acc_est_10   = f_acc_pago_s1
                             WHEN 9
                                LET reg_28.s5_acc_esp_10   = f_acc_pago_s1
                          END CASE
                      END CASE

                { CASE f_subcta
                    WHEN 1
                        LET reg_6.s1_acc_ret97_10 = f_acc_pago_s1
                        LET reg_7.s2_acc_ret97_10 = f_acc_pago_s2
                    WHEN 2
                        LET reg_6.s1_acc_cv_10    = f_acc_pago_s1
                        LET reg_7.s2_acc_cv_10    = f_acc_pago_s2
                    WHEN 5
                        LET reg_6.s1_acc_so_10    = f_acc_pago_s1
                        LET reg_7.s2_acc_so_10    = f_acc_pago_s2
                    WHEN 6
                        LET reg_6.s1_acc_est_10   = f_acc_pago_s1
                        LET reg_7.s2_acc_est_10   = f_acc_pago_s2
                    WHEN 9
                        LET reg_6.s1_acc_esp_10   = f_acc_pago_s1
                        LET reg_7.s2_acc_esp_10   = f_acc_pago_s2
                END CASE }
            END FOREACH

            LET s1monto_acc_cv_10 = reg_6.s1_acc_cv_10  +
                                    reg_6.s1_acc_est_10 +
                                    reg_6.s1_acc_esp_10

            LET s2monto_acc_cv_10 = reg_7.s2_acc_cv_10  +
                                    reg_7.s2_acc_est_10 +
                                    reg_7.s2_acc_esp_10

            LET s3monto_acc_cv_10 = reg_26.s3_acc_cv_10  +
                                    reg_26.s3_acc_est_10 +
                                    reg_26.s3_acc_esp_10

            LET s4monto_acc_cv_10 = reg_27.s4_acc_cv_10  +
                                    reg_27.s4_acc_est_10 +
                                    reg_27.s4_acc_esp_10

            LET s5monto_acc_cv_10 = reg_28.s5_acc_cv_10  +
                                    reg_28.s5_acc_est_10 +
                                    reg_28.s5_acc_esp_10
            
            IF sw_1 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        1                     ,--siefore
                        reg_6.s1_acc_ret97_10 ,--acciones_ret97
                        s1monto_acc_cv_10     ,--acciones_cv
                        reg_6.s1_acc_so_10    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_2 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        2                     ,--siefore
                        reg_7.s2_acc_ret97_10 ,--acciones_ret97
                        s2monto_acc_cv_10     ,--acciones_cv
                        reg_7.s2_acc_so_10    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_3 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        3                     ,--siefore
                        reg_26.s3_acc_ret97_10 ,--acciones_ret97
                        s3monto_acc_cv_10     ,--acciones_cv
                        reg_26.s3_acc_so_10    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF
            IF sw_4 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        4                     ,--siefore
                        reg_27.s4_acc_ret97_10 ,--acciones_ret97
                        s4monto_acc_cv_10     ,--acciones_cv
                        reg_27.s4_acc_so_10    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF
            IF sw_5 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        5                     ,--siefore
                        reg_28.s5_acc_ret97_10 ,--acciones_ret97
                        s5monto_acc_cv_10     ,--acciones_cv
                        reg_28.s5_acc_so_10    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF
        ELSE
            --
            --display "entre a dias "
            --sleep 5
            --
            ---CALCULA 75 DIAS SALARIO BASE DE COTIZACION - DESEMPLEO-----------
            LET v_parcial_75 ="EXECUTE FUNCTION fn_calcula_parcial_75_ms ( ?,? )"
            PREPARE eje_parcial_75 FROM v_parcial_75

            DECLARE cur_6 CURSOR FOR eje_parcial_75
            FOREACH cur_6 USING reg_10.nss,salario_75_dias
                { INTO f_subcta         ,--subcuenta 
                     f_saldo_s1_75    ,--saldo acciones siefore1
                     f_saldo_s2_75    ,--saldo acciones siefore2
                     siefore1_75      ,--codigo siefore1
                     f_acc_pago_s1_75 ,--acciones a pagar siefore1
                     f_pes_pago_s1_75 ,--pesos a pagar siefore1
                     siefore2_75      ,--codigo siefore2
                     f_acc_pago_s2_75 ,--acciones a pagar siefore2
                     f_pes_pago_s2_75  --pesos a pagar siefore2 }

                INTO f_subcta         ,--subcuenta 
                     siefore1_75      ,--codigo siefore1
                     f_saldo_s1_75    ,--saldo acciones siefore1
                     f_acc_pago_s1_75 ,--acciones a pagar siefore1
                     f_pes_pago_s1_75  --pesos a pagar siefore1

                CASE siefore1_75
                     WHEN 1
                        IF f_acc_pago_s1_75 > 0 THEN
                           LET sw_1 = 1
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1_75         ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s1 ,#p_accion
                                              -f_acc_pago_s1_75   ,#mto_acc
                                              -f_pes_pago_s1_75   ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                          LET f_acc_pago_s1_75 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                               LET reg_8.s1_acc_ret97_75 = f_acc_pago_s1_75
                             WHEN 2
                               LET reg_8.s1_acc_cv_75    = f_acc_pago_s1_75
                             WHEN 5
                               LET reg_8.s1_acc_so_75    = f_acc_pago_s1_75
                             WHEN 6
                               LET reg_8.s1_acc_est_75   = f_acc_pago_s1_75
                             WHEN 9
                               LET reg_8.s1_acc_esp_75   = f_acc_pago_s1_75 
                        END CASE

                     WHEN 2
                        IF f_acc_pago_s1_75 > 0 THEN
                           LET sw_2 = 1
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1_75         ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s2 ,#p_accion
                                              -f_acc_pago_s1_75   ,#mto_acc
                                              -f_pes_pago_s1_75   ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                          LET f_acc_pago_s1_75 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                               LET reg_9.s2_acc_ret97_75 = f_acc_pago_s1_75
                             WHEN 2
                               LET reg_9.s2_acc_cv_75    = f_acc_pago_s1_75
                             WHEN 5
                               LET reg_9.s2_acc_so_75    = f_acc_pago_s1_75
                             WHEN 6
                               LET reg_9.s2_acc_est_75   = f_acc_pago_s1_75
                             WHEN 9
                               LET reg_9.s2_acc_esp_75   = f_acc_pago_s1_75 
                        END CASE
                     WHEN 3
                        IF f_acc_pago_s1_75 > 0 THEN
                           LET sw_3 = 1
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1_75         ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s3 ,#p_accion
                                              -f_acc_pago_s1_75   ,#mto_acc
                                              -f_pes_pago_s1_75   ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                          LET f_acc_pago_s1_75 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                               LET reg_20.s3_acc_ret97_75 = f_acc_pago_s1_75
                             WHEN 2
                               LET reg_20.s3_acc_cv_75    = f_acc_pago_s1_75
                             WHEN 5
                               LET reg_20.s3_acc_so_75    = f_acc_pago_s1_75
                             WHEN 6
                               LET reg_20.s3_acc_est_75   = f_acc_pago_s1_75
                             WHEN 9
                               LET reg_20.s3_acc_esp_75   = f_acc_pago_s1_75 
                        END CASE
                     WHEN 4
                        IF f_acc_pago_s1_75 > 0 THEN
                           LET sw_4 = 1
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1_75         ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s4 ,#p_accion
                                              -f_acc_pago_s1_75   ,#mto_acc
                                              -f_pes_pago_s1_75   ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                          LET f_acc_pago_s1_75 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                               LET reg_21.s4_acc_ret97_75 = f_acc_pago_s1_75
                             WHEN 2
                               LET reg_21.s4_acc_cv_75    = f_acc_pago_s1_75
                             WHEN 5
                               LET reg_21.s4_acc_so_75    = f_acc_pago_s1_75
                             WHEN 6
                               LET reg_21.s4_acc_est_75   = f_acc_pago_s1_75
                             WHEN 9
                               LET reg_21.s4_acc_esp_75   = f_acc_pago_s1_75 
                        END CASE

                     WHEN 5
                        IF f_acc_pago_s1_75 > 0 THEN
                           LET sw_5 = 1
                           CALL actualiza_cuenta_ind(reg_1.folio_oper13  ,#folio
                                              reg_10.nss          ,#nss
                                              f_subcta            ,#subcta
                                              siefore1_75         ,#siefore
                                              875                 ,#tipo_mov
                                              HOY                 ,#fech_pag
                                              HOY                 ,#fech_val
                                              HOY                 ,#fec_conv
                                              d11_6_precio_dia_s5 ,#p_accion
                                              -f_acc_pago_s1_75   ,#mto_acc
                                              -f_pes_pago_s1_75   ,#mto_pes
                                              reg_10.consecutivo  ,#consec_l
                                              usuario              #usuario
                                             ) #aci
                        ELSE
                          LET f_acc_pago_s1_75 = 0
                        END IF
                        CASE f_subcta
                             WHEN 1
                               LET reg_22.s5_acc_ret97_75 = f_acc_pago_s1_75
                             WHEN 2
                               LET reg_22.s5_acc_cv_75    = f_acc_pago_s1_75
                             WHEN 5
                               LET reg_22.s5_acc_so_75    = f_acc_pago_s1_75
                             WHEN 6
                               LET reg_22.s5_acc_est_75   = f_acc_pago_s1_75
                             WHEN 9
                               LET reg_22.s5_acc_esp_75   = f_acc_pago_s1_75 
                        END CASE
                END CASE

                { CASE f_subcta
                    WHEN 1
                        LET reg_8.s1_acc_ret97_75 = f_acc_pago_s1_75
                        LET reg_9.s2_acc_ret97_75 = f_acc_pago_s2_75
                    WHEN 2
                        LET reg_8.s1_acc_cv_75    = f_acc_pago_s1_75
                        LET reg_9.s2_acc_cv_75    = f_acc_pago_s2_75
                    WHEN 5
                        LET reg_8.s1_acc_so_75    = f_acc_pago_s1_75
                        LET reg_9.s2_acc_so_75    = f_acc_pago_s2_75
                    WHEN 6
                        LET reg_8.s1_acc_est_75   = f_acc_pago_s1_75
                        LET reg_9.s2_acc_est_75   = f_acc_pago_s2_75
                    WHEN 9
                        LET reg_8.s1_acc_esp_75   = f_acc_pago_s1_75 
                        LET reg_9.s2_acc_esp_75   = f_acc_pago_s2_75
                END CASE }
            END FOREACH

            LET s1monto_acc_cv_75 = reg_8.s1_acc_cv_75  +
                                    reg_8.s1_acc_est_75 +
                                    reg_8.s1_acc_esp_75

            LET s2monto_acc_cv_75 = reg_9.s2_acc_cv_75  +
                                    reg_9.s2_acc_est_75 +
                                    reg_9.s2_acc_esp_75

            LET s3monto_acc_cv_75 = reg_20.s3_acc_cv_75  +
                                    reg_20.s3_acc_est_75 +
                                    reg_20.s3_acc_esp_75

            LET s4monto_acc_cv_75 = reg_21.s4_acc_cv_75  +
                                    reg_21.s4_acc_est_75 +
                                    reg_21.s4_acc_esp_75

            LET s5monto_acc_cv_75 = reg_22.s5_acc_cv_75  +
                                    reg_22.s5_acc_est_75 +
                                    reg_22.s5_acc_esp_75
            IF sw_1 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        1                     ,--siefore
                        reg_8.s1_acc_ret97_75 ,--acciones_ret97
                        s1monto_acc_cv_75     ,--acciones_cv
                        reg_8.s1_acc_so_75    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_2 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        2                     ,--siefore
                        reg_9.s2_acc_ret97_75 ,--acciones_ret97
                        s2monto_acc_cv_75     ,--acciones_cv
                        reg_9.s2_acc_so_75    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_3 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        3                     ,--siefore
                        reg_20.s3_acc_ret97_75 ,--acciones_ret97
                        s3monto_acc_cv_75     ,--acciones_cv
                        reg_20.s3_acc_so_75    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_4 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        4                     ,--siefore
                        reg_21.s4_acc_ret97_75 ,--acciones_ret97
                        s4monto_acc_cv_75     ,--acciones_cv
                        reg_21.s4_acc_so_75    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF

            IF sw_5 = 1 THEN
                INSERT INTO ret_monto_siefore
                VALUES (reg_10.nss            ,--nss
                        reg_10.consecutivo    ,--consecutivo
                        reg_1.folio_oper13    ,--folio
                        "I"                   ,--tipo_retiro
                        16                    ,--tipo_operacion
                        5                     ,--siefore
                        reg_22.s5_acc_ret97_75 ,--acciones_ret97
                        s5monto_acc_cv_75     ,--acciones_cv
                        reg_22.s5_acc_so_75    ,--acciones_cs
                        0                      --acciones_ret92
                       )
            END IF
        END IF

        SELECT ROUND(sum(monto_en_pesos),2)
    INTO   monto_pesos_sub1_2
    FROM   dis_cuenta
    WHERE  nss   = reg_10.nss
    AND    folio = reg_1.folio_oper13
    AND    consecutivo_lote = reg_10.consecutivo 
        AND    subcuenta = 1

    IF monto_pesos_sub1_2 IS NULL THEN 
       LET monto_pesos_sub1_2 = 0
        END IF

        SELECT ROUND(sum(monto_en_pesos),2)
    INTO   monto_pesos_sub5_2
    FROM   dis_cuenta
    WHERE  nss   = reg_10.nss
    AND    folio = reg_1.folio_oper13
    AND    consecutivo_lote = reg_10.consecutivo 
        AND    subcuenta = 5

    IF monto_pesos_sub5_2 IS NULL THEN 
       LET monto_pesos_sub5_2 = 0
        END IF

        SELECT ROUND(sum(monto_en_pesos),2)
    INTO   monto_pesos_cv   
    FROM   dis_cuenta
    WHERE  nss   = reg_10.nss
    AND    folio = reg_1.folio_oper13
    AND    consecutivo_lote = reg_10.consecutivo 
        AND    subcuenta IN (2,6,9)

    IF monto_pesos_cv IS NULL THEN 
       LET monto_pesos_cv = 0
        END IF

    LET porc10_pesos_rcv = ( monto_pesos_cv + monto_pesos_sub1_2 +
                     monto_pesos_sub5_2 ) * (-1)

        UPDATE ret_parcial_tx
        SET    fecha_valuacion = HOY ,
               fecha_pago      = HOY ,
               pago_desempleo  = porc10_pesos_rcv
        WHERE  nss             = reg_10.nss
        AND    consecutivo     = reg_10.consecutivo
        AND    folio           = reg_1.folio_oper13

        UPDATE ret_parcial
        SET    estado_solicitud = 8
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo

    END FOREACH
END FUNCTION


FUNCTION segundo_paso()
#sp--------------------
    DEFINE reg_11 RECORD #loc #reg_11
        nss                   LIKE ret_parcial_tx.nss        ,
        consecutivo           LIKE ret_parcial.consecutivo   ,
        impt_autorizado       LIKE ret_parcial.impt_autorizado 
    END RECORD

    DEFINE reg_14 RECORD #loc  #reg_14
        s1_acc_ret97          DECIMAL(16,6) ,
        s1_acc_cv             DECIMAL(16,6) ,
        s1_acc_so             DECIMAL(16,6) ,
        s1_acc_est            DECIMAL(16,6) ,
        s1_acc_esp            DECIMAL(16,6) ,

        s1_pes_ret97          DECIMAL(16,6) , 
        s1_pes_cv             DECIMAL(16,6) , 
        s1_pes_so             DECIMAL(16,6) , 
        s1_pes_est            DECIMAL(16,6) , 
        s1_pes_esp            DECIMAL(16,6) 
    END RECORD

    DEFINE reg_15 RECORD #loc #reg_15
        s2_acc_ret97          DECIMAL(16,6) ,       
        s2_acc_cv             DECIMAL(16,6) , 
        s2_acc_so             DECIMAL(16,6) , 
        s2_acc_est            DECIMAL(16,6) , 
        s2_acc_esp            DECIMAL(16,6) , 

        s2_pes_ret97          DECIMAL(16,6) , 
        s2_pes_cv             DECIMAL(16,6) , 
        s2_pes_so             DECIMAL(16,6) , 
        s2_pes_est            DECIMAL(16,6) , 
        s2_pes_esp            DECIMAL(16,6)
    END RECORD

    DEFINE reg_29 RECORD #loc #reg_29
        s3_acc_ret97          DECIMAL(16,6) ,       
        s3_acc_cv             DECIMAL(16,6) , 
        s3_acc_so             DECIMAL(16,6) , 
        s3_acc_est            DECIMAL(16,6) , 
        s3_acc_esp            DECIMAL(16,6) , 

        s3_pes_ret97          DECIMAL(16,6) , 
        s3_pes_cv             DECIMAL(16,6) , 
        s3_pes_so             DECIMAL(16,6) , 
        s3_pes_est            DECIMAL(16,6) , 
        s3_pes_esp            DECIMAL(16,6)
    END RECORD

    DEFINE reg_30 RECORD #loc #reg_30
        s4_acc_ret97          DECIMAL(16,6) ,       
        s4_acc_cv             DECIMAL(16,6) , 
        s4_acc_so             DECIMAL(16,6) , 
        s4_acc_est            DECIMAL(16,6) , 
        s4_acc_esp            DECIMAL(16,6) , 

        s4_pes_ret97          DECIMAL(16,6) , 
        s4_pes_cv             DECIMAL(16,6) , 
        s4_pes_so             DECIMAL(16,6) , 
        s4_pes_est            DECIMAL(16,6) , 
        s4_pes_esp            DECIMAL(16,6)
    END RECORD

    DEFINE reg_31 RECORD #loc #reg_31
        s5_acc_ret97          DECIMAL(16,6) ,       
        s5_acc_cv             DECIMAL(16,6) , 
        s5_acc_so             DECIMAL(16,6) , 
        s5_acc_est            DECIMAL(16,6) , 
        s5_acc_esp            DECIMAL(16,6) , 

        s5_pes_ret97          DECIMAL(16,6) , 
        s5_pes_cv             DECIMAL(16,6) , 
        s5_pes_so             DECIMAL(16,6) , 
        s5_pes_est            DECIMAL(16,6) , 
        s5_pes_esp            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #smallint
        estado                ,
        v_subcuenta_mat       ,
        v_grupo_mat           ,
        f_subcuenta_mat       ,
        f_siefore_mat         SMALLINT

    DEFINE #loc #decimal
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6) ,
        monto_pesos_cv_mat    DECIMAL(16,6)

    SELECT  "OK"
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.tipo_prestacion = 7
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada
    GROUP BY 1 

    IF STATUS = NOTFOUND THEN
        RETURN
    END IF

    DECLARE cur_3 CURSOR FOR
    SELECT  B.nss           ,
            B.consecutivo   ,
            A.impt_autorizado
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.tipo_prestacion = 7
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada

    FOREACH cur_3 INTO reg_11.*
        ----
        --display "estoy en matrimonio: ", reg_11.nss
        --sleep 8
        ----
        LET cont_reg_av         = cont_reg_av + 1

        LET reg_14.s1_acc_ret97 = 0
        LET reg_14.s1_acc_cv    = 0
        LET reg_14.s1_acc_so    = 0
        LET reg_14.s1_acc_est   = 0
        LET reg_14.s1_acc_esp   = 0 

        LET reg_14.s1_pes_ret97 = 0
        LET reg_14.s1_pes_cv    = 0
        LET reg_14.s1_pes_so    = 0
        LET reg_14.s1_pes_est   = 0
        LET reg_14.s1_pes_esp   = 0

        LET reg_15.s2_acc_ret97 = 0
        LET reg_15.s2_acc_cv    = 0
        LET reg_15.s2_acc_so    = 0
        LET reg_15.s2_acc_est   = 0
        LET reg_15.s2_acc_esp   = 0

        LET reg_15.s2_pes_ret97 = 0
        LET reg_15.s2_pes_cv    = 0
        LET reg_15.s2_pes_so    = 0
        LET reg_15.s2_pes_est   = 0
        LET reg_15.s2_pes_esp   = 0
 
        LET reg_29.s3_acc_ret97 = 0
        LET reg_29.s3_acc_cv    = 0
        LET reg_29.s3_acc_so    = 0
        LET reg_29.s3_acc_est   = 0
        LET reg_29.s3_acc_esp   = 0

        LET reg_29.s3_pes_ret97 = 0
        LET reg_29.s3_pes_cv    = 0
        LET reg_29.s3_pes_so    = 0
        LET reg_29.s3_pes_est   = 0
        LET reg_29.s3_pes_esp   = 0

        LET reg_30.s4_acc_ret97 = 0
        LET reg_30.s4_acc_cv    = 0
        LET reg_30.s4_acc_so    = 0
        LET reg_30.s4_acc_est   = 0
        LET reg_30.s4_acc_esp   = 0

        LET reg_30.s4_pes_ret97 = 0
        LET reg_30.s4_pes_cv    = 0
        LET reg_30.s4_pes_so    = 0
        LET reg_30.s4_pes_est   = 0
        LET reg_30.s4_pes_esp   = 0

        LET reg_31.s5_acc_ret97 = 0
        LET reg_31.s5_acc_cv    = 0
        LET reg_31.s5_acc_so    = 0
        LET reg_31.s5_acc_est   = 0
        LET reg_31.s5_acc_esp   = 0

        LET reg_31.s5_pes_ret97 = 0
        LET reg_31.s5_pes_cv    = 0
        LET reg_31.s5_pes_so    = 0
        LET reg_31.s5_pes_est   = 0
        LET reg_31.s5_pes_esp   = 0

        LET v_subcuenta_mat     = 0
        LET v_grupo_mat         = 0

        ---SE OBTIENE EL SALDO POR NSS DE CADA UNA DE LAS SUBCUENTAS------------
        LET v_saldo_dia_mat = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "

        PREPARE eje_saldo_dia_mat FROM v_saldo_dia_mat
        DECLARE cur_1 CURSOR FOR eje_saldo_dia_mat

        FOREACH cur_1  USING reg_11.nss       ,
                             v_subcuenta_mat  , --Si subcta. y grupo son 0
                             v_grupo_mat      , --se obtiene saldo de
                             HOY                --todas las subcuentas.
                       INTO  
                             f_subcuenta_mat  ,
                             f_siefore_mat    ,
                             f_monto_acc      ,
                             f_monto_pes

            IF f_subcuenta_mat = 1 OR f_subcuenta_mat = 2 OR 
               f_subcuenta_mat = 5 OR f_subcuenta_mat = 6 OR 
               f_subcuenta_mat = 9 THEN

                IF f_monto_acc IS NULL OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                    LET f_monto_pes = 0
                ELSE
                    CASE f_siefore_mat
                        WHEN 1  --- SIEFORE 1 ---
                            CASE f_subcuenta_mat
                                WHEN 1
                                    LET reg_14.s1_acc_ret97 = f_monto_acc
                                    LET reg_14.s1_pes_ret97 = f_monto_pes
                                WHEN 2
                                    LET reg_14.s1_acc_cv    = f_monto_acc
                                    LET reg_14.s1_pes_cv    = f_monto_pes
                                WHEN 5
                                    LET reg_14.s1_acc_so    = f_monto_acc
                                    LET reg_14.s1_pes_so    = f_monto_pes
                                WHEN 6
                                    LET reg_14.s1_acc_est   = f_monto_acc
                                    LET reg_14.s1_pes_est   = f_monto_pes
                                WHEN 9
                                    LET reg_14.s1_acc_esp   = f_monto_acc
                                    LET reg_14.s1_pes_esp   = f_monto_pes
                            END CASE
                        WHEN 2  --- SIEFORE 2 ---
                            CASE f_subcuenta_mat
                                WHEN 1
                                    LET reg_15.s2_acc_ret97 = f_monto_acc
                                    LET reg_15.s2_pes_ret97 = f_monto_pes
                                WHEN 2
                                    LET reg_15.s2_acc_cv    = f_monto_acc
                                    LET reg_15.s2_pes_cv    = f_monto_pes
                                WHEN 5
                                    LET reg_15.s2_acc_so    = f_monto_acc
                                    LET reg_15.s2_pes_so    = f_monto_pes
                                WHEN 6
                                    LET reg_15.s2_acc_est   = f_monto_acc
                                    LET reg_15.s2_pes_est   = f_monto_pes
                                WHEN 9
                                    LET reg_15.s2_acc_esp   = f_monto_acc
                                    LET reg_15.s2_pes_esp   = f_monto_pes
                            END CASE
                        WHEN 3  --- SIEFORE 3 ---
                            CASE f_subcuenta_mat
                                WHEN 1
                                    LET reg_29.s3_acc_ret97 = f_monto_acc
                                    LET reg_29.s3_pes_ret97 = f_monto_pes
                                WHEN 2
                                    LET reg_29.s3_acc_cv    = f_monto_acc
                                    LET reg_29.s3_pes_cv    = f_monto_pes
                                WHEN 5
                                    LET reg_29.s3_acc_so    = f_monto_acc
                                    LET reg_29.s3_pes_so    = f_monto_pes
                                WHEN 6
                                    LET reg_29.s3_acc_est   = f_monto_acc
                                    LET reg_29.s3_pes_est   = f_monto_pes
                                WHEN 9
                                    LET reg_29.s3_acc_esp   = f_monto_acc
                                    LET reg_29.s3_pes_esp   = f_monto_pes
                            END CASE
                        WHEN 4  --- SIEFORE 4 ---
                            CASE f_subcuenta_mat
                                WHEN 1
                                    LET reg_30.s4_acc_ret97 = f_monto_acc
                                    LET reg_30.s4_pes_ret97 = f_monto_pes
                                WHEN 2
                                    LET reg_30.s4_acc_cv    = f_monto_acc
                                    LET reg_30.s4_pes_cv    = f_monto_pes
                                WHEN 5
                                    LET reg_30.s4_acc_so    = f_monto_acc
                                    LET reg_30.s4_pes_so    = f_monto_pes
                                WHEN 6
                                    LET reg_30.s4_acc_est   = f_monto_acc
                                    LET reg_30.s4_pes_est   = f_monto_pes
                                WHEN 9
                                    LET reg_30.s4_acc_esp   = f_monto_acc
                                    LET reg_30.s4_pes_esp   = f_monto_pes
                            END CASE
                        WHEN 5  --- SIEFORE 5 ---
                            CASE f_subcuenta_mat
                                WHEN 1
                                    LET reg_31.s5_acc_ret97 = f_monto_acc
                                    LET reg_31.s5_pes_ret97 = f_monto_pes
                                WHEN 2
                                    LET reg_31.s5_acc_cv    = f_monto_acc
                                    LET reg_31.s5_pes_cv    = f_monto_pes
                                WHEN 5
                                    LET reg_31.s5_acc_so    = f_monto_acc
                                    LET reg_31.s5_pes_so    = f_monto_pes
                                WHEN 6
                                    LET reg_31.s5_acc_est   = f_monto_acc
                                    LET reg_31.s5_pes_est   = f_monto_pes
                                WHEN 9
                                    LET reg_31.s5_acc_esp   = f_monto_acc
                                    LET reg_31.s5_pes_esp   = f_monto_pes
                            END CASE
                    END CASE
                END IF
            END IF
        END FOREACH

        --LIQUIDACION DEL NSS EN dis_cuenta-----------------------------------
        LET v_liq_parcial = "EXECUTE FUNCTION fn_liq_parcial_mat_5 (?,?,?,?)" 
        
        PREPARE eje_liq_parcial FROM v_liq_parcial
        DECLARE cur_7 CURSOR FOR eje_liq_parcial
            OPEN cur_7 USING reg_1.folio_oper13 ,--folio
                             reg_11.nss         ,--nss
                             reg_11.consecutivo ,--consecutivo
                             HOY                 --fecha liquida
                FETCH cur_7  INTO estado

                IF estado <> 0 THEN
                   PROMPT "INCONSISTENCIA EN LA LIQUIDACION ",reg_11.nss ,estado
                   FOR CHAR enter
                END IF       
        ----------------------------------------------------------------------

        UPDATE ret_parcial_tx
        SET    fecha_valuacion = HOY ,
               fecha_pago      = HOY ,
               pago_desempleo  = 0
        WHERE  nss         = reg_11.nss
        AND    consecutivo = reg_11.consecutivo
        AND    folio       = reg_1.folio_oper13

        UPDATE ret_parcial
        SET    estado_solicitud = 8
        WHERE  nss              = reg_11.nss
        AND    consecutivo      = reg_11.consecutivo

    END FOREACH
END FUNCTION


FUNCTION tercer_paso() 
#tp-------------------
    INSERT INTO ret_ctr_envio
        VALUES (reg_1.folio_oper13        ,#folio
                "AV16"                    ,#tipo_operacion
                "CZAAV16 DETAV16 SUMAV16" ,#estructura
                reg_2.procesado           ,#status
                0                         ,#orden_de_envio
                HOY                        #fecha_envio
               )

    INSERT INTO ret_sum_envio
        VALUES(reg_1.folio_oper13 ,#folio
               "16"               ,#tipo_operacion
               NULL               ,#diag_cuenta_ind
               "AV"               ,#area_origen
               cont_reg_av        ,#tot_registros
               d2tot_monto_pesos  ,#impt_tot_oper
               reg_2.procesado    ,#status
               NULL)               #fecha_envio
END FUNCTION   
               
                
FUNCTION cuarto_paso()
#cp------------ -------
    DEFINE reg_12 RECORD #loc #reg_12
        nss                   LIKE ret_parcial_tx.nss              ,
        consecutivo           LIKE ret_parcial_tx.consecutivo      ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion     ,
        pago_desempleo        LIKE ret_parcial_tx.pago_desempleo   ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud     ,
        impt_tot_sub_rcv      LIKE ret_parcial.impt_tot_sub_rcv
    END RECORD

    DEFINE reg_13 RECORD #loc #reg_13
        estado_marca         SMALLINT ,
        marca_causa          SMALLINT
    END RECORD

    DEFINE reg_18 RECORD #loc #reg_18
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE v_subcuenta        SMALLINT
    DEFINE v_grupo            SMALLINT
    DEFINE v_siefore          SMALLINT
    DEFINE v_suma_rcv         DECIMAL(10,2)
    DEFINE v_monto_acc        DECIMAL(20,6)
    DEFINE v_monto_pes        DECIMAL(20,6)
    DEFINE vsaldo_s1          DECIMAL(20,6)
    DEFINE vsaldo_s2          DECIMAL(20,6)
    DEFINE vsaldo_s3          DECIMAL(20,6)
    DEFINE vsaldo_s4          DECIMAL(20,6)
    DEFINE vsaldo_s5          DECIMAL(20,6)

    LET reg_13.estado_marca = 0
    LET reg_13.marca_causa  = 0

    DECLARE cur_5 CURSOR FOR
    SELECT  B.nss                ,
            B.consecutivo        ,
            A.tipo_prestacion    ,
            B.pago_desempleo     ,
            A.fecha_solicitud    ,
            A.impt_tot_sub_rcv
    FROM    ret_parcial A, ret_parcial_tx B
    WHERE   B.folio           = reg_1.folio_oper13
    AND     B.nss             = A.nss
    AND     B.consecutivo     = A.consecutivo
    AND     A.diag_cuenta_ind = 400 --Cuenta Aceptada

    LET G_LISTA_1       = G_LISTA CLIPPED,"/",c7_nombre_plano
    
    START REPORT listado_detalle TO G_LISTA_1
        FOREACH cur_5 INTO reg_12.*
        
        IF reg_12.tipo_prestacion = 6 THEN
                ---
                --display "prestacion desempleo"
                --sleep 8
                --
            LET v_marca_ent = 875
        ELSE
                ---
                --display "prestacion matrimonio"
                --sleep 8
                --
            LET v_marca_ent = 870
        END IF

            LET vsaldo_s1 = 0
            LET vsaldo_s2 = 0
            LET vsaldo_s3 = 0
            LET vsaldo_s4 = 0
            LET vsaldo_s5 = 0

            SELECT sum(NVL(monto_en_acciones,0)) * d11_6_precio_dia_s1
            INTO   vsaldo_s1
            FROM   dis_cuenta
            WHERE  nss        = reg_12.nss
            AND    subcuenta IN (1,2,5,6,9)
            AND    siefore    = 1
            AND    folio     NOT IN (reg_1.folio_oper13)

            IF vsaldo_s1 IS NULL THEN
                LET vsaldo_s1 = 0
            END IF

            SELECT sum(NVL(monto_en_acciones,0)) * d11_6_precio_dia_s2
            INTO   vsaldo_s2
            FROM   dis_cuenta
            WHERE  nss        = reg_12.nss
            AND    subcuenta IN (1,2,5,6,9)
            AND    siefore    = 2
            AND    folio     NOT IN (reg_1.folio_oper13)

            IF vsaldo_s2 IS NULL THEN
                LET vsaldo_s2 = 0
            END IF

            SELECT sum(NVL(monto_en_acciones,0)) * d11_6_precio_dia_s3
            INTO   vsaldo_s3
            FROM   dis_cuenta
            WHERE  nss        = reg_12.nss
            AND    subcuenta IN (1,2,5,6,9)
            AND    siefore    = 3
            AND    folio     NOT IN (reg_1.folio_oper13)

            IF vsaldo_s3 IS NULL THEN
                LET vsaldo_s3 = 0
            END IF

            SELECT sum(NVL(monto_en_acciones,0)) * d11_6_precio_dia_s4
            INTO   vsaldo_s4
            FROM   dis_cuenta
            WHERE  nss        = reg_12.nss
            AND    subcuenta IN (1,2,5,6,9)
            AND    siefore    = 4
            AND    folio     NOT IN (reg_1.folio_oper13)

            IF vsaldo_s4 IS NULL THEN
                LET vsaldo_s4 = 0
            END IF

            SELECT sum(NVL(monto_en_acciones,0)) * d11_6_precio_dia_s5
            INTO   vsaldo_s5
            FROM   dis_cuenta
            WHERE  nss        = reg_12.nss
            AND    subcuenta IN (1,2,5,6,9)
            AND    siefore    = 5
            AND    folio     NOT IN (reg_1.folio_oper13)

            IF vsaldo_s5 IS NULL THEN
                LET vsaldo_s5 = 0
            END IF

            LET v_suma_rcv = vsaldo_s1 + vsaldo_s2 + vsaldo_s3 + vsaldo_s4 + vsaldo_s5 

            LET reg_18.dom_calle       = NULL
            LET reg_18.dom_numero_ext  = NULL
            LET reg_18.dom_numero_int  = NULL
            LET reg_18.dom_colonia     = NULL
            LET reg_18.deleg_desc      = NULL
            LET reg_18.dom_codpos      = NULL
            LET reg_18.estad_desc      = NULL

            SELECT A.calle      ,
                   A.numero     ,
                   A.depto      ,
                   A.colonia    ,
                   B.deleg_desc ,
                   A.codpos     ,
                   C.estad_desc
            INTO   reg_18.* 
            FROM   afi_domicilio A,tab_delegacion B,tab_estado C
            WHERE  A.nss = reg_12.nss
            AND    A.delega = B.deleg_cod
            AND    A.estado = C.estad_cod
            AND    A.factualiza = (SELECT MAX(factualiza)
                                   FROM   afi_domicilio
                                   WHERE  nss = reg_12.nss
                                   )

            OUTPUT TO REPORT listado_detalle(reg_12.*,v_suma_rcv,reg_18.*) #ld

            LET v_desmarca =" EXECUTE PROCEDURE desmarca_cuenta('",
                              reg_12.nss,"',",v_marca_ent,
                              ",",reg_12.consecutivo,",",
                              reg_13.estado_marca,",",reg_13.marca_causa
                              ,",'",usuario,"')"

            PREPARE eje_desmarca FROM v_desmarca
            EXECUTE eje_desmarca
        END FOREACH
    FINISH REPORT listado_detalle

    LET c110_chmod = "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/DETAV16"
    RUN c110_chmod
    
    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano

    LET c110_chmod = "chmod 777 ",G_LISTA_2
    RUN c110_chmod
    
    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    RUN borra_lineas

    LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,"/DETAV16"
    RUN elimina

END FUNCTION


REPORT listado_detalle(reg_12,v_suma_rcv,reg_18)
#ld---------------------------------------------
    DEFINE reg_12 RECORD #loc #reg_12
        nss                   LIKE ret_parcial_tx.nss              ,
        consecutivo           LIKE ret_parcial_tx.consecutivo      ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion     ,
        pago_desempleo        LIKE ret_parcial_tx.pago_desempleo   ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud     ,
        impt_tot_sub_rcv      LIKE ret_parcial.impt_tot_sub_rcv
    END RECORD

    DEFINE v_suma_rcv         DECIMAL(10,2)

    DEFINE reg_16 RECORD #loc #reg_16
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) 
    END RECORD

    DEFINE reg_17 RECORD #loc #reg_17
        siefore               SMALLINT      ,
        acciones_cs           DECIMAL(16,6) 
    END RECORD

    DEFINE reg_18 RECORD #loc #reg_18
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE reg_21 RECORD #loc #reg_21
        nss                   LIKE ret_parcial.nss             ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion , 
        impt_autorizado       LIKE ret_parcial.impt_autorizado ,
        pago_desempleo        LIKE ret_parcial.pago_desempleo
    END RECORD

    DEFINE reg_19 RECORD #loc #reg_19
        nss                   LIKE ret_parcial.nss             ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion , 
        impt_autorizado       LIKE ret_parcial.impt_autorizado
    END RECORD

    DEFINE #loc #char
        c15_s1_acc_ret97      CHAR(15) ,
        c14_s1_acc_ret97      CHAR(14) ,
        c15_s1_acc_cv         CHAR(15) ,
        c14_s1_acc_cv         CHAR(14) ,
        c15_s1_acc_cs         CHAR(15) ,
        c14_s1_acc_cs         CHAR(14) ,

        c15_s2_acc_ret97      CHAR(15) ,
        c14_s2_acc_ret97      CHAR(14) ,
        c15_s2_acc_cv         CHAR(15) ,
        c14_s2_acc_cv         CHAR(14) ,
        c15_s2_acc_cs         CHAR(15) ,
        c14_s2_acc_cs         CHAR(14) ,

        c15_s3_acc_ret97      CHAR(15) ,
        c14_s3_acc_ret97      CHAR(14) ,
        c15_s3_acc_cv         CHAR(15) ,
        c14_s3_acc_cv         CHAR(14) ,
        c15_s3_acc_cs         CHAR(15) ,
        c14_s3_acc_cs         CHAR(14) ,

        c15_s4_acc_ret97      CHAR(15) ,
        c14_s4_acc_ret97      CHAR(14) ,
        c15_s4_acc_cv         CHAR(15) ,
        c14_s4_acc_cv         CHAR(14) ,
        c15_s4_acc_cs         CHAR(15) ,
        c14_s4_acc_cs         CHAR(14) ,

        c15_s5_acc_ret97      CHAR(15) ,
        c14_s5_acc_ret97      CHAR(14) ,
        c15_s5_acc_cv         CHAR(15) ,
        c14_s5_acc_cv         CHAR(14) ,
        c15_s5_acc_cs         CHAR(15) ,
        c14_s5_acc_cs         CHAR(14) ,

        c15_s11_acc_ret97     CHAR(15) ,
        c14_s11_acc_ret97     CHAR(14) ,
        c15_s11_acc_cv        CHAR(15) ,
        c14_s11_acc_cv        CHAR(14) ,
        c15_s11_acc_cs        CHAR(15) ,
        c14_s11_acc_cs        CHAR(14) ,

        c10_impt_pagado       CHAR(10) ,
        c11_impt_pagado       CHAR(11) ,
        c10_impt_cta_ind      CHAR(10) ,
        c11_impt_cta_ind      CHAR(11) ,
 
        c16_imp_tot_oper      CHAR(16) ,
        c15_imp_tot_oper      CHAR(15)

    DEFINE #loc #smallint  
        cont_1                SMALLINT,
        x_siefore1            SMALLINT,
        x_siefore2            SMALLINT,
        x_siefore3            SMALLINT,
        x_siefore4            SMALLINT,
        x_siefore5            SMALLINT,
        x_siefore11           SMALLINT

    DEFINE #loc #decimal
        c14_acc_ret97_s1      DECIMAL(16,6) ,      
        c14_acc_cv_s1         DECIMAL(16,6) , 
        c14_acc_cs_s1         DECIMAL(16,6) ,
        c14_acc_ret97_s2      DECIMAL(16,6) ,
        c14_acc_cv_s2         DECIMAL(16,6) ,
        c14_acc_cs_s2         DECIMAL(16,6) ,
        c14_acc_ret97_s3      DECIMAL(16,6) ,
        c14_acc_cv_s3         DECIMAL(16,6) ,
        c14_acc_cs_s3         DECIMAL(16,6) ,
        c14_acc_ret97_s4      DECIMAL(16,6) ,
        c14_acc_cv_s4         DECIMAL(16,6) ,
        c14_acc_cs_s4         DECIMAL(16,6) ,
        c14_acc_ret97_s5      DECIMAL(16,6) ,
        c14_acc_cv_s5         DECIMAL(16,6) ,
        c14_acc_cs_s5         DECIMAL(16,6) ,
        c14_acc_ret97_s11     DECIMAL(16,6) ,
        c14_acc_cv_s11        DECIMAL(16,6) ,
        c14_acc_cs_s11        DECIMAL(16,6) ,
        d10_2_impt_pagado     DECIMAL(10,2) ,
        impt_pagado_mat       DECIMAL(16,6) ,
        impt_pagado_des       DECIMAL(16,6) , 
        c13_imp_tot_oper      DECIMAL(13,2) ,
        imp_mat               DECIMAL(10,2) ,
        imp_des               DECIMAL(10,2)

    OUTPUT
        PAGE LENGTH   1000
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

    FORMAT
    FIRST PAGE HEADER
        PRINT
            COLUMN 001,"01"                      ,--tipo de registro
            COLUMN 003,"04"                      ,--identificador de servicio
            COLUMN 005,"01"                      ,--tipo entidad origen
            COLUMN 007,s_codigo_afore USING"&&&" ,--clave entidad origen
            COLUMN 010,"03"                      ,--tipo entidad destino
            COLUMN 012,"001"                     ,--clave entidad destino
            COLUMN 015,HOY USING "YYYYMMDD"      ,--fecha de operacion
            COLUMN 023, 578 SPACES
    
    LET c13_imp_tot_oper = 0
    LET imp_mat = 0
    LET imp_des = 0

    ON EVERY ROW
        LET x_siefore1 = 0
        LET x_siefore2 = 0
        LET x_siefore3 = 0
        LET x_siefore4 = 0
        LET x_siefore5 = 0
        LET x_siefore11 = 0

        LET cont_1 = cont_1 + 1

        SELECT paterno     ,
               materno     ,
               nombres
        INTO   c40_paterno ,
               c40_materno ,
               c40_nombres
        FROM   afi_mae_afiliado
    WHERE  n_seguro = reg_12.nss

        IF reg_12.tipo_prestacion = 7 THEN
            --
            --display "matrimonio"
            --sleep 8
            --
        SELECT impt_autorizado  
        INTO   d10_2_impt_pagado 
        FROM   ret_parcial
        WHERE  nss         = reg_12.nss
        AND    consecutivo = reg_12.consecutivo

            LET imp_mat = imp_mat + d10_2_impt_pagado
    ELSE
            --
            --display "desempleo"
            --sleep 8
            --
        SELECT A.pago_desempleo  
        INTO   d10_2_impt_pagado
        FROM   ret_parcial_tx A
        WHERE  A.nss         = reg_12.nss
        AND    A.consecutivo = reg_12.consecutivo

            LET imp_des = imp_des + d10_2_impt_pagado
    END IF
 
        LET c13_imp_tot_oper = imp_mat + imp_des

        LET c11_impt_pagado = d10_2_impt_pagado USING"&&&&&&&&.&&"
        LET c10_impt_pagado = c11_impt_pagado [01,08],
                              c11_impt_pagado [10,11]

        LET c16_imp_tot_oper = c13_imp_tot_oper USING"&&&&&&&&&&&&&.&&"
        LET c15_imp_tot_oper = c16_imp_tot_oper[01,13],
                               c16_imp_tot_oper[15,16]

        LET c11_impt_cta_ind = v_suma_rcv  USING"&&&&&&&&.&&"
        LET c10_impt_cta_ind = c11_impt_cta_ind [01,08],
                               c11_impt_cta_ind [10,11]

        IF reg_12.tipo_prestacion = 6 THEN
            --
            --display "desempleo"
            --sleep 8
            --

            LET c14_s1_acc_ret97 = "00000000000000"
            LET c14_s1_acc_cv    = "00000000000000"
            LET c14_s1_acc_cs    = "00000000000000"

            LET c14_s2_acc_ret97 = "00000000000000"
            LET c14_s2_acc_cv    = "00000000000000"
            LET c14_s2_acc_cs    = "00000000000000"

            LET c14_s3_acc_ret97 = "00000000000000"
            LET c14_s3_acc_cv    = "00000000000000"
            LET c14_s3_acc_cs    = "00000000000000"

            LET c14_s4_acc_ret97 = "00000000000000"
            LET c14_s4_acc_cv    = "00000000000000"
            LET c14_s4_acc_cs    = "00000000000000"

            LET c14_s5_acc_ret97 = "00000000000000"
            LET c14_s5_acc_cv    = "00000000000000"
            LET c14_s5_acc_cs    = "00000000000000"

            LET c14_s11_acc_ret97 = "00000000000000"
            LET c14_s11_acc_cv    = "00000000000000"
            LET c14_s11_acc_cs    = "00000000000000"

            DECLARE cur_8 CURSOR FOR
            SELECT A.siefore        ,
                   A.acciones_ret97 ,
                   A.acciones_cv    ,
                   A.acciones_cs   
            FROM   ret_monto_siefore A
            WHERE  A.nss            = reg_12.nss
            AND    A.consecutivo    = reg_12.consecutivo
            AND    A.tipo_retiro    = "I"
            AND    A.tipo_operacion = 16 

            FOREACH cur_8 INTO reg_16.*
                IF reg_16.siefore = 1 THEN
                    LET x_siefore1 = reg_16.siefore
                    LET c15_s1_acc_ret97 = reg_16.acciones_ret97 USING"&&&&&&&&.&&&&&&"
                    LET c14_s1_acc_ret97 = c15_s1_acc_ret97[01,08],
                                           c15_s1_acc_ret97[10,15]
    
                    LET c15_s1_acc_cv    = reg_16.acciones_cv     USING"&&&&&&&&.&&&&&&"
                    LET c14_s1_acc_cv    = c15_s1_acc_cv[01,08],
                                           c15_s1_acc_cv[10,15]

                    LET c15_s1_acc_cs    = reg_16.acciones_cs     USING"&&&&&&&&.&&&&&&"
                    LET c14_s1_acc_cs    = c15_s1_acc_cs[01,08],
                                           c15_s1_acc_cs[10,15]
                ELSE
                   IF reg_16.siefore = 2 THEN
                      LET x_siefore2 = reg_16.siefore
                      LET c15_s2_acc_ret97 = reg_16.acciones_ret97 USING"&&&&&&&&.&&&&&&"
                      LET c14_s2_acc_ret97 = c15_s2_acc_ret97[01,08],
                                           c15_s2_acc_ret97[10,15]
    
                      LET c15_s2_acc_cv    = reg_16.acciones_cv     USING"&&&&&&&&.&&&&&&"
                      LET c14_s2_acc_cv    = c15_s2_acc_cv[01,08],
                                           c15_s2_acc_cv[10,15]

                      LET c15_s2_acc_cs    = reg_16.acciones_cs     USING"&&&&&&&&.&&&&&&"
                      LET c14_s2_acc_cs    = c15_s2_acc_cs[01,08],
                                           c15_s2_acc_cs[10,15]
                   ELSE
                      IF reg_16.siefore = 3 THEN
                         LET x_siefore3 = reg_16.siefore
                         LET c15_s3_acc_ret97 = reg_16.acciones_ret97 USING"&&&&&&&&.&&&&&&"
                         LET c14_s3_acc_ret97 = c15_s3_acc_ret97[01,08],
                                                c15_s3_acc_ret97[10,15]
    
                         LET c15_s3_acc_cv    = reg_16.acciones_cv     USING"&&&&&&&&.&&&&&&"
                         LET c14_s3_acc_cv    = c15_s3_acc_cv[01,08],
                                                c15_s3_acc_cv[10,15]

                         LET c15_s3_acc_cs    = reg_16.acciones_cs     USING"&&&&&&&&.&&&&&&"
                         LET c14_s3_acc_cs    = c15_s3_acc_cs[01,08],
                                                c15_s3_acc_cs[10,15]
                      ELSE
                         IF reg_16.siefore = 4 THEN
                            LET x_siefore4 = reg_16.siefore
                            LET c15_s4_acc_ret97 = reg_16.acciones_ret97 USING"&&&&&&&&.&&&&&&"
                            LET c14_s4_acc_ret97 = c15_s4_acc_ret97[01,08],
                                                   c15_s4_acc_ret97[10,15]
    
                            LET c15_s4_acc_cv    = reg_16.acciones_cv     USING"&&&&&&&&.&&&&&&"
                            LET c14_s4_acc_cv    = c15_s4_acc_cv[01,08],
                                                   c15_s4_acc_cv[10,15]

                            LET c15_s4_acc_cs    = reg_16.acciones_cs     USING"&&&&&&&&.&&&&&&"
                            LET c14_s4_acc_cs    = c15_s4_acc_cs[01,08],
                                                   c15_s4_acc_cs[10,15]
                         ELSE
                            IF reg_16.siefore = 5 THEN
                               LET x_siefore5 = reg_16.siefore
                               LET c15_s5_acc_ret97 = reg_16.acciones_ret97 USING"&&&&&&&&.&&&&&&"
                               LET c14_s5_acc_ret97 = c15_s5_acc_ret97[01,08],
                                                      c15_s5_acc_ret97[10,15]
    
                               LET c15_s5_acc_cv    = reg_16.acciones_cv     USING"&&&&&&&&.&&&&&&"
                               LET c14_s5_acc_cv    = c15_s5_acc_cv[01,08],
                                                      c15_s5_acc_cv[10,15]

                               LET c15_s5_acc_cs    = reg_16.acciones_cs     USING"&&&&&&&&.&&&&&&"
                               LET c14_s5_acc_cs    = c15_s5_acc_cs[01,08],
                                                      c15_s5_acc_cs[10,15]

                            END IF
                         END IF 
                      END IF
                   END IF
                END IF
            END FOREACH
        ELSE
            ---MATRIMONIO SOLO PAGA CUOTA SOCIAL (5)

            --
            --display "matrimonio"
            --sleep 8
            --
            LET c14_s1_acc_cs    = "00000000000000"
            LET c14_s2_acc_cs    = "00000000000000"
            LET c14_s3_acc_cs    = "00000000000000"
            LET c14_s4_acc_cs    = "00000000000000"
            LET c14_s5_acc_cs    = "00000000000000"
            LET c14_s11_acc_cs    = "00000000000000"

            DECLARE cur_9 CURSOR FOR
            SELECT A.siefore        ,
                   A.acciones_cs   
            FROM   ret_monto_siefore A
            WHERE  A.nss            = reg_12.nss
            AND    A.consecutivo    = reg_12.consecutivo
            AND    A.tipo_retiro    = "I"
            AND    A.tipo_operacion = 16 

            FOREACH cur_9 INTO reg_17.*
                IF reg_17.siefore = 1 THEN
                    LET x_siefore1 = reg_17.siefore
                    LET c15_s1_acc_cs    = reg_17.acciones_cs     USING"&&&&&&&&.&&&&&&"
                    LET c14_s1_acc_cs    = c15_s1_acc_cs[01,08],
                                           c15_s1_acc_cs[10,15]
                ELSE
                  IF reg_17.siefore = 2 THEN
                    LET x_siefore2 = reg_17.siefore
                    LET c15_s2_acc_cs    = reg_17.acciones_cs     USING"&&&&&&&&.&&&&&&"
                    LET c14_s2_acc_cs    = c15_s2_acc_cs[01,08],
                                           c15_s2_acc_cs[10,15]
                  ELSE
                    IF reg_17.siefore = 3 THEN
                       LET x_siefore3 = reg_17.siefore
                       LET c15_s3_acc_cs    = reg_17.acciones_cs     USING"&&&&&&&&.&&&&&&"
                       LET c14_s3_acc_cs    = c15_s3_acc_cs[01,08],
                                              c15_s3_acc_cs[10,15]
                    ELSE
                     IF reg_17.siefore = 4 THEN
                        LET x_siefore4 = reg_17.siefore
                        LET c15_s4_acc_cs    = reg_17.acciones_cs     USING"&&&&&&&&.&&&&&&"
                        LET c14_s4_acc_cs    = c15_s4_acc_cs[01,08],
                                               c15_s4_acc_cs[10,15]
                     ELSE
                      IF reg_17.siefore = 5 THEN
                         LET x_siefore5 = reg_17.siefore
                         LET c15_s5_acc_cs    = reg_17.acciones_cs     USING"&&&&&&&&.&&&&&&"
                         LET c14_s5_acc_cs    = c15_s5_acc_cs[01,08],
                                                c15_s5_acc_cs[10,15]
                      END IF
                     END IF
                    END IF
                  END IF
                END IF
            END FOREACH

            LET c14_s1_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s1_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s2_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s2_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s3_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s3_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s4_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s4_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s5_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s5_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s11_acc_ret97 = 0 USING"&&&&&&&&&&&&&&"
            LET c14_s11_acc_cv    = 0 USING"&&&&&&&&&&&&&&"
        END IF

        PRINT
            COLUMN 001,"03"                                   ,--tipo_registro
            COLUMN 003,"04"                                   ,
            COLUMN 005,"16"                                   ,--tipo_operacion
            COLUMN 007,reg_12.nss                             ,--nss
            COLUMN 018,18 SPACES                              ,
            COLUMN 036,c40_nombres                            ,--nombres
            COLUMN 076,c40_paterno                            ,--paterno
            COLUMN 116,c40_materno                            ,--materno
            COLUMN 156,"I"                                    ,--tipo_retiro
            COLUMN 157,reg_12.tipo_prestacion USING"&&"       ,--tipo_prestacion
            COLUMN 159,reg_12.fecha_solicitud USING"YYYYMMDD" ,--fecha_solic
            COLUMN 167,reg_12.consecutivo USING"&&&&&&"       ,--consecutivo
            COLUMN 173,c10_impt_pagado                        ,--mto_pagado
            COLUMN 183,c10_impt_cta_ind                       ,--saldo cta. ind
            COLUMN 193,HOY USING"YYYYMMDD"                    ,--fecha_val
            COLUMN 201,3   SPACES                             ,
            COLUMN 204,reg_18.dom_calle                       ,--calle
            COLUMN 269,reg_18.dom_numero_ext                  ,--num_ext
            COLUMN 284,reg_18.dom_numero_int                  ,--num_int
            COLUMN 299,reg_18.dom_colonia                     ,--colonia
            COLUMN 364,reg_18.deleg_desc                      ,--delegacion
            COLUMN 429,reg_18.dom_codpos                      ,--cp
            COLUMN 434,reg_18.estad_desc                      ,
            COLUMN 499,91  SPACES                             ,
            COLUMN 590,2   SPACES                             ,
            COLUMN 592,3   SPACES                             ,
            COLUMN 595,3   SPACES                             ,
            COLUMN 598,3   SPACES
     IF x_siefore1 <> 0 THEN   
        PRINT
            COLUMN 001,"04"                                   ,--tipo_operacion
            COLUMN 003,reg_12.nss                             ,--nss
            COLUMN 014,18 SPACES                              ,
            COLUMN 032,x_siefore1           USING "&&"            ,-- siefore
            COLUMN 034,c14_s1_acc_ret97                       ,--acc_ret97 SB1
            COLUMN 048,c14_s1_acc_cv                          ,--acc_cv SB1
            COLUMN 062,c14_s1_acc_cs                          ,--acc_cs SB1
            COLUMN 076, 525 SPACES
     END IF
     IF x_siefore2 <> 0 THEN   
        PRINT
            COLUMN 001,"04"                                   ,--tipo_operacion
            COLUMN 003,reg_12.nss                             ,--nss
            COLUMN 014,18 SPACES                              ,
            COLUMN 032,x_siefore2           USING "&&"            ,-- siefore
            COLUMN 034,c14_s2_acc_ret97                       ,--acc_ret97 SB2
            COLUMN 048,c14_s2_acc_cv                          ,--acc_cv SB2
            COLUMN 062,c14_s2_acc_cs                          ,--acc_cs SB2
            COLUMN 076, 525 SPACES
     END IF
     IF x_siefore3 <> 0 THEN   
        PRINT
            COLUMN 001,"04"                                   ,--tipo_operacion
            COLUMN 003,reg_12.nss                             ,--nss
            COLUMN 014,18 SPACES                              ,
            COLUMN 032,x_siefore3           USING "&&"            ,-- siefore
            COLUMN 034,c14_s3_acc_ret97                       ,--acc_ret97 SB3
            COLUMN 048,c14_s3_acc_cv                          ,--acc_cv SB3
            COLUMN 062,c14_s3_acc_cs                          ,--acc_cs SB3
            COLUMN 076, 525 SPACES
     END IF
     IF x_siefore4 <> 0 THEN   
        PRINT
            COLUMN 001,"04"                                   ,--tipo_operacion
            COLUMN 003,reg_12.nss                             ,--nss
            COLUMN 014,18 SPACES                              ,
            COLUMN 032,x_siefore4           USING "&&"            ,-- siefore
            COLUMN 034,c14_s4_acc_ret97                       ,--acc_ret97 SB4
            COLUMN 048,c14_s4_acc_cv                          ,--acc_cv SB4
            COLUMN 062,c14_s4_acc_cs                          ,--acc_cs SB4
            COLUMN 076, 525 SPACES
     END IF
     IF x_siefore5 <> 0 THEN   
        PRINT
            COLUMN 001,"04"                                   ,--tipo_operacion
            COLUMN 003,reg_12.nss                             ,--nss
            COLUMN 014,18 SPACES                              ,
            COLUMN 032,x_siefore5           USING "&&"            ,-- siefore
            COLUMN 034,c14_s5_acc_ret97                       ,--acc_ret97 SB5
            COLUMN 048,c14_s5_acc_cv                          ,--acc_cv SB5
            COLUMN 062,c14_s5_acc_cs                          ,--acc_cs SB5
            COLUMN 076, 525 SPACES
     END IF

    ON LAST ROW
        PRINT
            COLUMN 001,"09"                      ,--tipo de registro
            COLUMN 003,"04"                      ,--identificador de servicio
            COLUMN 005,"16"                      ,--identificado de operacion
            COLUMN 007,"01"                      ,--tipo entidad origen
            COLUMN 009,s_codigo_afore USING"&&&" ,--clave entidad origen
            COLUMN 012,"03"                      ,--tipo entidad destino
            COLUMN 014,"001"                     ,--clave entidad destino
            COLUMN 017,HOY USING "YYYYMMDD"      ,--fecha de operacion
            COLUMN 025,"AV "                     ,--area origen
            COLUMN 028,cont_1 USING"&&&&&&"      ,--numero de registros
            COLUMN 034,c15_imp_tot_oper          ,--suma importes
            COLUMN 049,552 SPACES
END REPORT

FUNCTION actualiza_cuenta_ind(reg_4)
#aci--------------------------------
    DEFINE reg_4 RECORD
        folio                 INTEGER                           ,
        n_seguro              LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        siefore               LIKE dis_cuenta.siefore           ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        fecha_pago            LIKE dis_cuenta.fecha_pago        ,
        fecha_valor           LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion  ,
        precio_accion         LIKE dis_cuenta.precio_accion     ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos        LIKE dis_cuenta.monto_en_pesos    ,
        consecutivo           INTEGER                           ,
        usuario               CHAR(8)
    END RECORD

    DEFINE #loc #char
        c11_id_aportante      CHAR(11)

    IF reg_4.tipo_movimiento = 560 THEN
        LET c11_id_aportante = "DEV_APL"
    ELSE
        LET c11_id_aportante = "RETIRO"
    END IF
    ---
    --display "entre a insertar dis_cuenta"
    --sleep 5
    --
    INSERT INTO safre_af:dis_cuenta
        VALUES(reg_4.tipo_movimiento   ,
               reg_4.subcuenta         ,
               reg_4.siefore           ,
               reg_4.folio             ,     
               reg_4.consecutivo       ,
               reg_4.n_seguro          ,
               ""                      ,#curp
               ""                      ,#folio_sua
               reg_4.fecha_pago        ,#fecha_pago
               reg_4.fecha_valor       ,#fecha_valor
               reg_4.fecha_conversion  ,#fecha_conversion
               reg_4.monto_en_pesos    ,#monto_en_pesos
               reg_4.monto_en_acciones ,#monto_en_acciones
               reg_4.precio_accion     ,#precio_accion
               0                       ,#dias_cotizados
               ""                      ,#sucursal
               c11_id_aportante        ,#id_aportante
               8                       ,#status
               HOY                     ,#fecha_proceso
               reg_4.usuario           ,#usuario
               ""                      ,#fecha_archivo
               0                        #etiqueta 
              )
END FUNCTION
