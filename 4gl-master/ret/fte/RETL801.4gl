###############################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa RETL801  => REPORTE DE RETIRO PARCIAL PREVIO A LA Oper.12            #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha             => 25 DE ABRIL DEL 2000 .....                               #
#Actualizacion     => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiz.   => 29 DE NOVIEMBRE DE 2004                                  #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
        w_codigo_afore     LIKE tab_afore_local.codigo_afore 

    DEFINE #glo #w_tabafore #w_paramgrales
        w_paramgrales         RECORD LIKE seg_modulo.* ,
        w_tabafore            RECORD LIKE tab_afore_local.*

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper12         INTEGER
    END RECORD

    DEFINE lr_precio_acc RECORD #glo #lr_precio_acc
        estado                SMALLINT  ,
        fecha                 DATE      ,
        siefore               SMALLINT  ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE 
        HOY                   DATE

    DEFINE #glo #char
        nom_impresora         CHAR(020) ,
        G_LISTA               CHAR(100) ,
        v_precios_accion      CHAR(100) ,
        lc_mensaje            CHAR(100) ,
        usuario               CHAR(008) ,
        enter                 CHAR(001) ,
        sw_1                  SMALLINT  


    DEFINE #glo #char  
    monto_pesos_97_sb1        DECIMAL(10,2) ,
    monto_pesos_cv_sb1        DECIMAL(10,2) ,
    monto_pesos_so_sb1        DECIMAL(10,2) ,
    dmto_pesos_est_esp_sb1    DECIMAL(10,2) ,
    monto_pesos_rcv_sb1       DECIMAL(10,2) ,
    monto_accion_97_sb1       DECIMAL(16,6) ,
    monto_accion_cv_sb1       DECIMAL(16,6) ,
    monto_accion_so_sb1       DECIMAL(16,6) ,
    mto_accion_est_esp_sb1    DECIMAL(16,6) ,

    monto_pesos_97_sb2        DECIMAL(10,2) ,
    monto_pesos_cv_sb2        DECIMAL(10,2) ,
    monto_pesos_so_sb2        DECIMAL(10,2) ,
    dmto_pesos_est_esp_sb2    DECIMAL(10,2) ,
    monto_pesos_rcv_sb2       DECIMAL(10,2) ,
    monto_accion_97_sb2       DECIMAL(16,6) ,
    monto_accion_cv_sb2       DECIMAL(16,6) ,
    monto_accion_so_sb2       DECIMAL(16,6) ,
    mto_accion_est_esp_sb2    DECIMAL(16,6),

    monto_pesos_97_sb3        DECIMAL(10,2) ,
    monto_pesos_cv_sb3        DECIMAL(10,2) ,
    monto_pesos_so_sb3        DECIMAL(10,2) ,
    dmto_pesos_est_esp_sb3    DECIMAL(10,2) ,
    monto_pesos_rcv_sb3       DECIMAL(10,2) ,
    monto_accion_97_sb3       DECIMAL(16,6) ,
    monto_accion_cv_sb3       DECIMAL(16,6) ,
    monto_accion_so_sb3       DECIMAL(16,6) ,
    mto_accion_est_esp_sb3    DECIMAL(16,6) ,

    monto_pesos_97_sb4        DECIMAL(10,2) ,
    monto_pesos_cv_sb4        DECIMAL(10,2) ,
    monto_pesos_so_sb4        DECIMAL(10,2) ,
    dmto_pesos_est_esp_sb4    DECIMAL(10,2) ,
    monto_pesos_rcv_sb4       DECIMAL(10,2) ,
    monto_accion_97_sb4       DECIMAL(16,6) ,
    monto_accion_cv_sb4       DECIMAL(16,6) ,
    monto_accion_so_sb4       DECIMAL(16,6) ,
    mto_accion_est_esp_sb4    DECIMAL(16,6) ,

    monto_pesos_97_sb5        DECIMAL(10,2) ,
    monto_pesos_cv_sb5        DECIMAL(10,2) ,
    monto_pesos_so_sb5        DECIMAL(10,2) ,
    dmto_pesos_est_esp_sb5    DECIMAL(10,2) ,
    monto_pesos_rcv_sb5       DECIMAL(10,2) ,
    monto_accion_97_sb5       DECIMAL(16,6) ,
    monto_accion_cv_sb5       DECIMAL(16,6) ,
    monto_accion_so_sb5       DECIMAL(16,6) ,
    mto_accion_est_esp_sb5    DECIMAL(16,6)

    DEFINE #glo #decimal
    porc10_pesos_rcv_sb1      DECIMAL(10,2) ,
    porc10_pesos_rcv_sb2     DECIMAL(10,2) ,
    porc10_pesos_rcv_sb3      DECIMAL(10,2) ,
    porc10_pesos_rcv_sb4      DECIMAL(10,2) ,
    porc10_pesos_rcv_sb5      DECIMAL(10,2) ,
    d11_6_precio_dia_s1       DECIMAL(16,6) ,
    d11_6_precio_dia_s2       DECIMAL(16,6) ,
    d11_6_precio_dia_s3       DECIMAL(16,6) ,
    d11_6_precio_dia_s4       DECIMAL(16,6) ,
    d11_6_precio_dia_s5       DECIMAL(16,6)  

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    CALL STARTLOG("RETL801.log") 
    OPEN WINDOW retl8011 AT 4,4 WITH FORM "RETL8011" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL801    REPORTE, PROVISION RETIRO PARCIAL POR DESEMPLEO                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME reg_1.folio_oper12 WITHOUT DEFAULTS
        AFTER FIELD folio_oper12                                   
            IF reg_1.folio_oper12 IS NULL THEN                         
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper12                                
            END IF

        SELECT 'X'
        FROM ret_parcial 
       WHERE @folio = reg_1.folio_oper12
       GROUP BY 1

       IF STATUS = NOTFOUND THEN
           MESSAGE "NO EXISTE INFORMACION PARA ESTE FOLIO"
           ATTRIBUTE(REVERSE)
           SLEEP 2
           MESSAGE ""
           NEXT FIELD folio_oper12
       END IF

        ON KEY (ESC)
            IF reg_1.folio_oper12 IS NULL THEN                         
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper12                                
            ELSE
                 EXIT INPUT
            END IF

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso()  #pp
    CALL segundo_paso() #sp
    CLOSE WINDOW retl8011
    CALL tercer_paso()  #tp
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN

FUNCTION init()
#i-------------
    SELECT codigo_afore 
    INTO   w_codigo_afore 
    FROM   tab_afore_local
    
    LET HOY = TODAY

    SELECT *, USER
    INTO   w_tabafore.*, usuario
    FROM   tab_afore_local
                                                                               
    SELECT comando_impresion
    INTO   nom_impresora
    FROM   tab_cmd_impresora
    WHERE  codigo_afore = w_tabafore.codigo_afore

    SELECT MAX(folio)
    INTO   reg_1.folio_oper12
    FROM   ret_parcial

    SELECT *                                                             
    INTO   w_paramgrales.*                                               
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    { SELECT precio_del_dia
    INTO   d11_6_precio_dia_s1
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = HOY 
    AND    codigo_siefore  = 1

    SELECT precio_del_dia
    INTO   d11_6_precio_dia_s2
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = HOY 
    AND    codigo_siefore  = 2 }

    LET v_precios_accion  = "EXECUTE FUNCTION fn_verifica_precio_accion (?)"

    PREPARE eje_precios_accion FROM v_precios_accion
    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios  USING HOY INTO lr_precio_acc.*
        IF lr_precio_acc.estado <> 0 THEN
           LET lc_mensaje = "FALTA PRECIO DE ACCION DEL DIA SIEFORE: ", lr_precio_acc.siefore,"...<ENTER> PARA FINALIZAR"
           PROMPT lc_mensaje
           FOR CHAR enter
           EXIT PROGRAM
        END IF
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

    LET G_LISTA = w_paramgrales.ruta_listados CLIPPED,"/",HOY USING"DDMMYYYY",
                  ".801"
 
    WHENEVER ERROR CONTINUE
    DROP TABLE ret_prov_parcial
    
    CREATE TEMP TABLE ret_prov_parcial
        (nss char(11)      NOT null ,
         consecutivo       INTEGER,
         folio_op12        DECIMAL(10,0) not null ,
         siefore           SMALLINT,
         subcuenta         SMALLINT not null ,
         monto_en_acciones DECIMAL(16,6),
         monto_en_pesos    DECIMAL(16,6),
         fecha_conversion  DATE
        )
    WHENEVER ERROR STOP 

END FUNCTION

FUNCTION primer_paso() 
#pp-------------------

    DEFINE reg_10 RECORD #loc #reg_10
        nss                   LIKE ret_parcial_tx.nss         ,
        consecutivo           LIKE ret_parcial.consecutivo    ,
        salario_base_cot      LIKE ret_parcial.salario_base_cot
    END RECORD

#jj
    DEFINE reg_4 RECORD #glo #reg_4
        s1_acciones_ret97     DECIMAL(22,6) ,
        s1_acciones_cv        DECIMAL(22,6) ,
        s1_acciones_so        DECIMAL(22,6) ,
        s1_acciones_est       DECIMAL(22,6) ,
        s1_acciones_esp       DECIMAL(22,6) ,

        s1_pesos_ret97        DECIMAL(22,6) ,
        s1_pesos_cv           DECIMAL(22,6) ,
        s1_pesos_so           DECIMAL(22,6) ,
        s1_pesos_est          DECIMAL(22,6) ,
        s1_pesos_esp          DECIMAL(22,6) 
    END RECORD

    DEFINE reg_5 RECORD #glo #reg_5
        s2_acciones_ret97     DECIMAL(22,6) ,
        s2_acciones_cv        DECIMAL(22,6) ,
        s2_acciones_so        DECIMAL(22,6) ,
        s2_acciones_est       DECIMAL(22,6) ,
        s2_acciones_esp       DECIMAL(22,6) ,

        s2_pesos_ret97        DECIMAL(22,6) ,
        s2_pesos_cv           DECIMAL(22,6) ,
        s2_pesos_so           DECIMAL(22,6) ,
        s2_pesos_est          DECIMAL(22,6) ,
        s2_pesos_esp          DECIMAL(22,6) 
    END RECORD
 
    DEFINE reg_6 RECORD #glo #reg_6
        s3_acciones_ret97     DECIMAL(22,6) ,
        s3_acciones_cv        DECIMAL(22,6) ,
        s3_acciones_so        DECIMAL(22,6) ,
        s3_acciones_est       DECIMAL(22,6) ,
        s3_acciones_esp       DECIMAL(22,6) ,

        s3_pesos_ret97        DECIMAL(22,6) ,
        s3_pesos_cv           DECIMAL(22,6) ,
        s3_pesos_so           DECIMAL(22,6) ,
        s3_pesos_est          DECIMAL(22,6) ,
        s3_pesos_esp          DECIMAL(22,6) 
    END RECORD
    DEFINE reg_7 RECORD #glo #reg_7
        s4_acciones_ret97     DECIMAL(22,6) ,
        s4_acciones_cv        DECIMAL(22,6) ,
        s4_acciones_so        DECIMAL(22,6) ,
        s4_acciones_est       DECIMAL(22,6) ,
        s4_acciones_esp       DECIMAL(22,6) ,

        s4_pesos_ret97        DECIMAL(22,6) ,
        s4_pesos_cv           DECIMAL(22,6) ,
        s4_pesos_so           DECIMAL(22,6) ,
        s4_pesos_est          DECIMAL(22,6) ,
        s4_pesos_esp          DECIMAL(22,6) 
    END RECORD
    DEFINE reg_8 RECORD #glo #reg_8
        s5_acciones_ret97     DECIMAL(22,6) ,
        s5_acciones_cv        DECIMAL(22,6) ,
        s5_acciones_so        DECIMAL(22,6) ,
        s5_acciones_est       DECIMAL(22,6) ,
        s5_acciones_esp       DECIMAL(22,6) ,

        s5_pesos_ret97        DECIMAL(22,6) ,
        s5_pesos_cv           DECIMAL(22,6) ,
        s5_pesos_so           DECIMAL(22,6) ,
        s5_pesos_est          DECIMAL(22,6) ,
        s5_pesos_esp          DECIMAL(22,6) 
    END RECORD
    DEFINE #loc #decimal
        salario_75_dias       DECIMAL(22,6) ,
        f_monto_acc           DECIMAL(22,6) ,
        f_monto_pes           DECIMAL(22,6) ,
        f_saldo_s1            DECIMAL(22,6) ,
        f_saldo_s2            DECIMAL(22,6) ,
        f_saldo_s3            DECIMAL(22,6) ,
        f_saldo_s4            DECIMAL(22,6) ,
        f_saldo_s5            DECIMAL(22,6) ,
        f_saldo_s1_75         DECIMAL(22,6) ,
        f_saldo_s2_75         DECIMAL(22,6) ,
        f_saldo_s3_75         DECIMAL(22,6) ,
        f_saldo_s4_75         DECIMAL(22,6) ,
        f_saldo_s5_75         DECIMAL(22,6) ,
        f_acc_pago_s1         DECIMAL(22,6) ,
        f_acc_pago_s2         DECIMAL(22,6) ,
        f_acc_pago_s3         DECIMAL(22,6) ,
        f_acc_pago_s4         DECIMAL(22,6) ,
        f_acc_pago_s5         DECIMAL(22,6) ,
        f_acc_pago_s1_75      DECIMAL(22,6) ,
        f_acc_pago_s2_75      DECIMAL(22,6) ,
        f_acc_pago_s3_75      DECIMAL(22,6) ,
        f_acc_pago_s4_75      DECIMAL(22,6) ,
        f_acc_pago_s5_75      DECIMAL(22,6) ,
        f_pes_pago_s1_75      DECIMAL(22,6) ,
        f_pes_pago_s2_75      DECIMAL(22,6) ,
        f_pes_pago_s3_75      DECIMAL(22,6) ,
        f_pes_pago_s4_75      DECIMAL(22,6) ,
        f_pes_pago_s5_75      DECIMAL(22,6) ,
        d16_pesos_pago_s1     DECIMAL(22,6) ,
        d16_pesos_pago_s2     DECIMAL(22,6) ,
        d16_pesos_pago_s3     DECIMAL(22,6) ,
        d16_pesos_pago_s4     DECIMAL(22,6) ,
        d16_pesos_pago_s5     DECIMAL(22,6)

    DEFINE #loc #smallint
        v_subcuenta           ,
        v_grupo               ,
        f_subcta              ,
        f_subcuenta           ,
        f_siefore             ,
        siefore1              ,
        siefore2              , 
        siefore3              , 
        siefore4              , 
        siefore5              , 
        siefore1_75           ,
        siefore2_75           ,
        siefore3_75           ,
        siefore4_75           ,
        siefore5_75           ,
	bandera               SMALLINT

    DEFINE #glo #char
        borra_lineas          CHAR(100) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        v_desmarca            CHAR(100) ,
        v_parcial             CHAR(300) ,
        v_parcial_75          CHAR(300) ,
        v_saldo_dia           CHAR(300) ,
        v_saldo_dia_mat       CHAR(300) 
        
    DEFINE #glo #decimal
        ---
        monto_pesos_cv        DECIMAL(22,6) ,
        monto_pesos_rcv       DECIMAL(22,6) ,
	    ---
        porc10_pesos_rcv      DECIMAL(22,6) ,
        d11_6_precio_dia_s1   DECIMAL(22,6) ,
        d11_6_precio_dia_s2   DECIMAL(22,6) ,
        d11_6_precio_dia_s3   DECIMAL(22,6) ,
        d11_6_precio_dia_s4   DECIMAL(22,6) ,
        d11_6_precio_dia_s5   DECIMAL(22,6) 

#jj


    DEFINE lr_porc10 RECORD #loc lr_porc10
        acc_97_sb1            DECIMAL(22,6) ,
        acc_cv_sb1            DECIMAL(22,6) , 
        acc_cs_sb1            DECIMAL(22,6) , 
        acc_est_esp_sb1       DECIMAL(22,6) , 
    	
        acc_97_sb2            DECIMAL(22,6) , 
        acc_cv_sb2            DECIMAL(22,6) , 
        acc_cs_sb2            DECIMAL(22,6) , 
        acc_est_esp_sb2       DECIMAL(22,6) , 

        acc_97_sb3            DECIMAL(22,6) , 
        acc_cv_sb3            DECIMAL(22,6) , 
        acc_cs_sb3            DECIMAL(22,6) , 
        acc_est_esp_sb3       DECIMAL(22,6) , 

        acc_97_sb4            DECIMAL(22,6) , 
        acc_cv_sb4            DECIMAL(22,6) , 
        acc_cs_sb4            DECIMAL(22,6) , 
        acc_est_esp_sb4       DECIMAL(22,6) , 

        acc_97_sb5            DECIMAL(22,6) , 
        acc_cv_sb5            DECIMAL(22,6) , 
        acc_cs_sb5            DECIMAL(22,6) , 
        acc_est_esp_sb5       DECIMAL(22,6) , 

        acc_97_sb1_75         DECIMAL(22,6) , 
        acc_cv_sb1_75         DECIMAL(22,6) , 
        acc_cs_sb1_75         DECIMAL(22,6) , 
        acc_est_esp_sb1_75    DECIMAL(22,6) , 

        acc_97_sb2_75         DECIMAL(22,6) , 
        acc_cv_sb2_75         DECIMAL(22,6) , 
        acc_cs_sb2_75         DECIMAL(22,6) , 
        acc_est_esp_sb2_75    DECIMAL(22,6) , 

        acc_97_sb3_75         DECIMAL(22,6) , 
        acc_cv_sb3_75         DECIMAL(22,6) , 
        acc_cs_sb3_75         DECIMAL(22,6) , 
        acc_est_esp_sb3_75    DECIMAL(22,6) , 

        acc_97_sb4_75         DECIMAL(22,6) , 
        acc_cv_sb4_75         DECIMAL(22,6) , 
        acc_cs_sb4_75         DECIMAL(22,6) , 
        acc_est_esp_sb4_75    DECIMAL(22,6) , 

        acc_97_sb5_75         DECIMAL(22,6) , 
        acc_cv_sb5_75         DECIMAL(22,6) , 
        acc_cs_sb5_75         DECIMAL(22,6) , 
        acc_est_esp_sb5_75    DECIMAL(22,6)  
    END RECORD 
    	
    SELECT  "OK"
    FROM    ret_parcial A 
    WHERE   A.folio            = reg_1.folio_oper12
    AND     A.tipo_prestacion  = 6
    AND     A.estado_solicitud = 2
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT " NO EXISTEN REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF

    DECLARE cur_2 CURSOR FOR
    SELECT  A.nss            ,
            A.consecutivo    ,
            A.salario_base_cot
    FROM    ret_parcial A    
    WHERE   A.folio            = reg_1.folio_oper12
    AND     A.tipo_prestacion  = 6
    AND     A.estado_solicitud = 2

    START REPORT listado_1 TO G_LISTA
    FOREACH cur_2 INTO reg_10.*

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

        LET reg_6.s3_acciones_ret97 = 0
        LET reg_6.s3_acciones_cv    = 0
        LET reg_6.s3_acciones_so    = 0
        LET reg_6.s3_acciones_est   = 0
        LET reg_6.s3_acciones_esp   = 0

        LET reg_6.s3_pesos_ret97    = 0
        LET reg_6.s3_pesos_cv       = 0
        LET reg_6.s3_pesos_so       = 0
        LET reg_6.s3_pesos_est      = 0
        LET reg_6.s3_pesos_esp      = 0

        LET reg_7.s4_acciones_ret97 = 0
        LET reg_7.s4_acciones_cv    = 0
        LET reg_7.s4_acciones_so    = 0
        LET reg_7.s4_acciones_est   = 0
        LET reg_7.s4_acciones_esp   = 0

        LET reg_7.s4_pesos_ret97    = 0
        LET reg_7.s4_pesos_cv       = 0
        LET reg_7.s4_pesos_so       = 0
        LET reg_7.s4_pesos_est      = 0
        LET reg_7.s4_pesos_esp      = 0

        LET reg_8.s5_acciones_ret97 = 0
        LET reg_8.s5_acciones_cv    = 0
        LET reg_8.s5_acciones_so    = 0
        LET reg_8.s5_acciones_est   = 0
        LET reg_8.s5_acciones_esp   = 0

        LET reg_8.s5_pesos_ret97    = 0
        LET reg_8.s5_pesos_cv       = 0
        LET reg_8.s5_pesos_so       = 0
        LET reg_8.s5_pesos_est      = 0
        LET reg_8.s5_pesos_esp      = 0

        LET v_subcuenta             = 0
        LET v_grupo                 = 0

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
                                    LET reg_6.s3_acciones_ret97 = f_monto_acc
                                    LET reg_6.s3_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_6.s3_acciones_cv    = f_monto_acc
                                    LET reg_6.s3_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_6.s3_acciones_so    = f_monto_acc
                                    LET reg_6.s3_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_6.s3_acciones_est   = f_monto_acc
                                    LET reg_6.s3_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_6.s3_acciones_esp   = f_monto_acc
                                    LET reg_6.s3_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 4   ---Siefore  4 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_7.s4_acciones_ret97 = f_monto_acc
                                    LET reg_7.s4_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_7.s4_acciones_cv    = f_monto_acc
                                    LET reg_7.s4_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_7.s4_acciones_so    = f_monto_acc
                                    LET reg_7.s4_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_7.s4_acciones_est   = f_monto_acc
                                    LET reg_7.s4_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_7.s4_acciones_esp   = f_monto_acc
                                    LET reg_7.s4_pesos_esp      = f_monto_pes
                            END CASE
                        WHEN 5   ---Siefore  5 ---
                            CASE f_subcuenta
                                WHEN 1
                                    LET reg_8.s5_acciones_ret97 = f_monto_acc
                                    LET reg_8.s5_pesos_ret97    = f_monto_pes
                                WHEN 2
                                    LET reg_8.s5_acciones_cv    = f_monto_acc
                                    LET reg_8.s5_pesos_cv       = f_monto_pes
                                WHEN 5
                                    LET reg_8.s5_acciones_so    = f_monto_acc
                                    LET reg_8.s5_pesos_so       = f_monto_pes
                                WHEN 6
                                    LET reg_8.s5_acciones_est   = f_monto_acc
                                    LET reg_8.s5_pesos_est      = f_monto_pes
                                WHEN 9
                                    LET reg_8.s5_acciones_esp   = f_monto_acc
                                    LET reg_8.s5_pesos_esp      = f_monto_pes
                            END CASE
                    END CASE
                END IF
            END IF
        END FOREACH

        LET monto_pesos_cv   = reg_4.s1_pesos_cv  +
                               reg_5.s2_pesos_cv  +
                               reg_6.s3_pesos_cv  +
                               reg_7.s4_pesos_cv  +
                               reg_8.s5_pesos_cv  +
                               reg_4.s1_pesos_est +
                               reg_5.s2_pesos_est +
                               reg_6.s3_pesos_est +
                               reg_7.s4_pesos_est +
                               reg_8.s5_pesos_est +
                               reg_4.s1_pesos_esp +
                               reg_5.s2_pesos_esp +
                               reg_6.s3_pesos_esp +
                               reg_7.s4_pesos_esp +
                               reg_8.s5_pesos_esp
                             
        LET monto_pesos_rcv  = reg_4.s1_pesos_ret97 +   --Siefore1
                               reg_5.s2_pesos_ret97 +   --Siefore2
                               reg_6.s3_pesos_ret97 +   --Siefore3
                               reg_7.s4_pesos_ret97 +   --Siefore4
                               reg_8.s5_pesos_ret97 +   --Siefore5
                               monto_pesos_cv       +
                               reg_4.s1_pesos_so    +
                               reg_5.s2_pesos_so    +
                               reg_6.s3_pesos_so    +
                               reg_7.s4_pesos_so    +
                               reg_8.s5_pesos_so
                            
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
                     f_acc_pago_s1  --total siefore1
            CASE siefore1
              WHEN 1
                IF f_acc_pago_s1 > 0 THEN
                    LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s1

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 1
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo
                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                1                  ,
                                f_subcta           ,
                                f_acc_pago_s1      ,
                                d16_pesos_pago_s1  ,
                                HOY
                               )
--                    END IF            
                END IF
              WHEN 2
                IF f_acc_pago_s1 > 0 THEN
                    LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s2
{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 2
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                2                  ,
                                f_subcta           ,
                                f_acc_pago_s1      ,
                                d16_pesos_pago_s1  ,
                                HOY
                               )
--                    END IF 
                END IF
              WHEN 3
                IF f_acc_pago_s1 > 0 THEN
                    LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s3
{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 3
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                3                  ,
                                f_subcta           ,
                                f_acc_pago_s1      ,
                                d16_pesos_pago_s1  ,
                                HOY
                               )
--                    END IF 
                END IF
              WHEN 4
                IF f_acc_pago_s1 > 0 THEN
                    LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s4
{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 4
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                4                  ,
                                f_subcta           ,
                                f_acc_pago_s1      ,
                                d16_pesos_pago_s1  ,
                                HOY
                               )
--                    END IF 
                END IF
              WHEN 5
                IF f_acc_pago_s1 > 0 THEN
                    LET d16_pesos_pago_s1 = f_acc_pago_s1 * 
                                            d11_6_precio_dia_s5
{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 5
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                5                  ,
                                f_subcta           ,
                                f_acc_pago_s1      ,
                                d16_pesos_pago_s1  ,
                                HOY
                               )
--                    END IF 
                END IF
             END CASE
            END FOREACH
        ELSE
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

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 1
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo
                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                1                  ,
                                f_subcta           ,
                                f_acc_pago_s1_75   ,
                                f_pes_pago_s1_75   ,
                                HOY
                               )
--                    END IF 
                END IF
             WHEN 2 
                IF f_acc_pago_s1_75 > 0 THEN

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
--
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 2
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                2                  ,
                                f_subcta           ,
                                f_acc_pago_s1_75   ,
                                f_pes_pago_s1_75   ,
                                HOY
                               )
--                    END IF 
                END IF
             WHEN 3 
                IF f_acc_pago_s1_75 > 0 THEN

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
--
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 2
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                3                  ,
                                f_subcta           ,
                                f_acc_pago_s1_75   ,
                                f_pes_pago_s1_75   ,
                                HOY
                               )
--                    END IF 
                END IF
             WHEN 4 
                IF f_acc_pago_s1_75 > 0 THEN

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
--
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 2
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                4                  ,
                                f_subcta           ,
                                f_acc_pago_s1_75   ,
                                f_pes_pago_s1_75   ,
                                HOY
                               )
--                    END IF 
                END IF
             WHEN 5 
                IF f_acc_pago_s1_75 > 0 THEN

{                    SELECT "OK" 
                    FROM   safre_tmp:ret_rpt_desempleo
                    WHERE  nss         = reg_10.nss
--
                    AND    consecutivo = reg_10.consecutivo
--                    AND    folio_op12  = reg_1.folio_oper12
--                    AND    siefore     = 2
--                    GROUP BY 1
                    
                    IF STATUS = NOTFOUND THEN
}
--                        INSERT INTO safre_tmp:ret_rpt_desempleo

                        INSERT  INTO ret_prov_parcial
                        VALUES (reg_10.nss         ,
                                reg_10.consecutivo ,
                                reg_1.folio_oper12 ,
                                5                  ,
                                f_subcta           ,
                                f_acc_pago_s1_75   ,
                                f_pes_pago_s1_75   ,
                                HOY
                               )
--                    END IF 
                END IF
              END CASE
            END FOREACH
        END IF    	

	LET bandera = 0

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_97_sb1
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 1
        AND    subcuenta   = 1 #RETIRO 97

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cv_sb1
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 1
        AND    subcuenta   = 2 #CESANTIA EN EDAD AVANZADA Y VEJEZ
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cs_sb1
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 1
        AND    subcuenta   = 5 # CUOTA SOCIAL
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_est_esp_sb1
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 1
        AND    subcuenta   IN (6,9) # APORTACION ESTATAL / ESPECIAL
        
        IF lr_porc10.acc_97_sb1      > 0 OR
           lr_porc10.acc_cv_sb1      > 0 OR
           lr_porc10.acc_cs_sb1      > 0 OR
           lr_porc10.acc_est_esp_sb1 > 0 THEN 

	    LET bandera = 1
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12       ,
                                       reg_10.consecutivo       ,
                                       reg_10.nss               ,
                                       "SB1"                    ,
                                       salario_75_dias          ,
                                       lr_porc10.acc_97_sb1     ,
                                       lr_porc10.acc_cv_sb1     ,
                                       lr_porc10.acc_cs_sb1     ,
                                       lr_porc10.acc_est_esp_sb1)
        END IF                                        

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_97_sb2
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 2
        AND    subcuenta   = 1 #RETIRO 97

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cv_sb2
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 2
        AND    subcuenta   = 2 #CESANTIA EN EDAD AVANZADA Y VEJEZ
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cs_sb2
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 2
        AND    subcuenta   = 5 # CUOTA SOCIAL
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_est_esp_sb2
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 2
        AND    subcuenta   IN (6,9) # APORTACION ESTATAL / ESPECIAL

        IF lr_porc10.acc_97_sb2      > 0 OR
           lr_porc10.acc_cv_sb2      > 0 OR
           lr_porc10.acc_cs_sb2      > 0 OR
           lr_porc10.acc_est_esp_sb2 > 0 THEN 

	    IF bandera = 1 THEN
	       LET salario_75_dias = 0
            ELSE
               LET bandera = 1
            END IF 
        
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12       ,
                                       reg_10.consecutivo       ,
                                       reg_10.nss               ,
                                       "SB2"                    ,
                                       salario_75_dias          ,
                                       lr_porc10.acc_97_sb2     ,
                                       lr_porc10.acc_cv_sb2     ,
                                       lr_porc10.acc_cs_sb2     ,
                                       lr_porc10.acc_est_esp_sb2)
        END IF 

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_97_sb3
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 3
        AND    subcuenta   = 1 #RETIRO 97

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cv_sb3
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 3
        AND    subcuenta   = 2 #CESANTIA EN EDAD AVANZADA Y VEJEZ
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cs_sb3
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 3
        AND    subcuenta   = 5 # CUOTA SOCIAL
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_est_esp_sb3
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 3
        AND    subcuenta   IN (6,9) # APORTACION ESTATAL / ESPECIAL

        IF lr_porc10.acc_97_sb3      > 0 OR
           lr_porc10.acc_cv_sb3      > 0 OR
           lr_porc10.acc_cs_sb3      > 0 OR
           lr_porc10.acc_est_esp_sb3 > 0 THEN 

	    IF bandera = 1 THEN
	       LET salario_75_dias = 0
            ELSE
               LET bandera = 1
            END IF 
        
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12       ,
                                       reg_10.consecutivo       ,
                                       reg_10.nss               ,
                                       "SB3"                    ,
                                       salario_75_dias          ,
                                       lr_porc10.acc_97_sb3     ,
                                       lr_porc10.acc_cv_sb3     ,
                                       lr_porc10.acc_cs_sb3     ,
                                       lr_porc10.acc_est_esp_sb3)
       END IF 

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_97_sb4
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 4
        AND    subcuenta   = 1 #RETIRO 97

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cv_sb4
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 4
        AND    subcuenta   = 2 #CESANTIA EN EDAD AVANZADA Y VEJEZ
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cs_sb4
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 4
        AND    subcuenta   = 5 # CUOTA SOCIAL
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_est_esp_sb4
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 4
        AND    subcuenta   IN (6,9) # APORTACION ESTATAL / ESPECIAL

        IF lr_porc10.acc_97_sb4      > 0 OR
           lr_porc10.acc_cv_sb4      > 0 OR
           lr_porc10.acc_cs_sb4      > 0 OR
           lr_porc10.acc_est_esp_sb4 > 0 THEN 

	    IF bandera = 1 THEN
	       LET salario_75_dias = 0
            ELSE
               LET bandera = 1
            END IF 
        
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12       ,
                                       reg_10.consecutivo       ,
                                       reg_10.nss               ,
                                       "SB4"                    ,
                                       salario_75_dias          ,
                                       lr_porc10.acc_97_sb4     ,
                                       lr_porc10.acc_cv_sb4     ,
                                       lr_porc10.acc_cs_sb4     ,
                                       lr_porc10.acc_est_esp_sb4)
        END IF 

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_97_sb5
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 5
        AND    subcuenta   = 1 #RETIRO 97

        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cv_sb5
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 5
        AND    subcuenta   = 2 #CESANTIA EN EDAD AVANZADA Y VEJEZ
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_cs_sb5
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 5
        AND    subcuenta   = 5 # CUOTA SOCIAL
        
        SELECT SUM(monto_en_acciones)
        INTO   lr_porc10.acc_est_esp_sb5
--        FROM   safre_tmp:ret_rpt_desempleo
        FROM   ret_prov_parcial
        WHERE  nss         = reg_10.nss
        AND    consecutivo = reg_10.consecutivo
        AND    folio_op12  = reg_1.folio_oper12
        AND    siefore     = 5
        AND    subcuenta   IN (6,9) # APORTACION ESTATAL / ESPECIAL

        IF lr_porc10.acc_97_sb5      > 0 OR
           lr_porc10.acc_cv_sb5      > 0 OR
           lr_porc10.acc_cs_sb5      > 0 OR
           lr_porc10.acc_est_esp_sb5 > 0 THEN 

	    IF bandera = 1 THEN
	       LET salario_75_dias = 0
            ELSE
               LET bandera = 1
            END IF 
        
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12       ,
                                       reg_10.consecutivo       ,
                                       reg_10.nss               ,
                                       "SB5"                    ,
                                       salario_75_dias          ,
                                       lr_porc10.acc_97_sb5     ,
                                       lr_porc10.acc_cv_sb5     ,
                                       lr_porc10.acc_cs_sb5     ,
                                       lr_porc10.acc_est_esp_sb5)
       END IF 
    END FOREACH
    FINISH REPORT listado_1
END FUNCTION


FUNCTION segundo_paso()
#sp-------------------
    DEFINE #loc #char
        lp                    CHAR(100),
        permisos              CHAR(100)

    WHILE TRUE
        PROMPT " DESEA GENERAR IMPRESION S/N  ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                --LET lp = nom_impresora CLIPPED," ",G_LISTA
                LET lp = "lp ",G_LISTA CLIPPED
                RUN lp
                RETURN
            ELSE 
                PROMPT" PROCESO FINALIZADO... < ENTER > PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF
	    LET permisos = "chmod 777 ",G_LISTA CLIPPED
	    RUN permisos  
        END IF
    END WHILE
END FUNCTION


FUNCTION tercer_paso()
#tp-------------------
    OPEN WINDOW retl8011 AT 4,4 WITH FORM "RETL8011" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL801    REPORTE, PROVISION RETIRO PARCIAL POR DESEMPLEO                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)
    DISPLAY BY NAME reg_1.folio_oper12 
END FUNCTION

REPORT listado_1(vfolio_oper12     ,  
                 vconsecutivo      , 
                 vnss              ,
                 vnombre_siefore   ,  
                 vsalario_75_dias  , 
                 vporc10_accion_97 , 
                 vporc10_accion_cv , 
                 vporc10_accion_so , 
                 vp10_accion_est_esp)
#l1--------------------
    DEFINE
        vfolio_oper12         INTEGER                     ,
        vconsecutivo          LIKE ret_parcial.consecutivo,
        vnss                  CHAR(11)                    ,
        vnombre_siefore       CHAR(03)                    ,
        vsalario_75_dias      DECIMAL(16,6)               ,
        vporc10_accion_97     DECIMAL(16,6)               ,
        vporc10_accion_cv     DECIMAL(16,6)               ,
        vporc10_accion_so     DECIMAL(16,6)               ,
        vp10_accion_est_esp   DECIMAL(16,6)               

    DEFINE
        sb1                  CHAR(03),
        sb2                  CHAR(03),
        sb3                  CHAR(03),
        sb4                  CHAR(03),
        sb5                  CHAR(03),
        vnombres             CHAR(40),
        vpaterno             CHAR(40),
        vmaterno             CHAR(40)

    DEFINE #loc #date
        d_fecha_operacion     DATE,
        nombre_final          CHAR(50)

    DEFINE 
        G_salario_75_dias_sb1     DECIMAL(16,6) ,
        G_monto_accion_97_sb1     DECIMAL(16,6) ,
        G_monto_accion_cv_sb1     DECIMAL(16,6) ,
        G_monto_accion_so_sb1     DECIMAL(16,6) ,
        G_mto_accion_est_esp_sb1  DECIMAL(16,6)

    DEFINE 
        G_salario_75_dias_sb2     DECIMAL(16,6) ,
        G_monto_accion_97_sb2     DECIMAL(16,6) ,
        G_monto_accion_cv_sb2     DECIMAL(16,6) ,
        G_monto_accion_so_sb2     DECIMAL(16,6) ,
        G_mto_accion_est_esp_sb2  DECIMAL(16,6)

    DEFINE 
        G_salario_75_dias_sb3     DECIMAL(16,6) ,
        G_monto_accion_97_sb3     DECIMAL(16,6) ,
        G_monto_accion_cv_sb3     DECIMAL(16,6) ,
        G_monto_accion_so_sb3     DECIMAL(16,6) ,
        G_mto_accion_est_esp_sb3  DECIMAL(16,6)

    DEFINE 
        G_salario_75_dias_sb4     DECIMAL(16,6) ,
        G_monto_accion_97_sb4     DECIMAL(16,6) ,
        G_monto_accion_cv_sb4     DECIMAL(16,6) ,
        G_monto_accion_so_sb4     DECIMAL(16,6) ,
        G_mto_accion_est_esp_sb4  DECIMAL(16,6)

    DEFINE 
        G_salario_75_dias_sb5     DECIMAL(16,6) ,
        G_monto_accion_97_sb5     DECIMAL(16,6) ,
        G_monto_accion_cv_sb5     DECIMAL(16,6) ,
        G_monto_accion_so_sb5     DECIMAL(16,6) ,
        G_mto_accion_est_esp_sb5  DECIMAL(16,6)

    DEFINE #loc #char
        encabezado            CHAR(60) ,
        var2                  CHAR(10) ,
        var1                  CHAR(10) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L3                    CHAR(03) ,
        L4                    CHAR(04) ,
        L5                    CHAR(05) ,
        L6                    CHAR(06) ,
        L7                    CHAR(07) ,
        L8                    CHAR(08) ,
        L9                    CHAR(09) ,
        L10                   CHAR(10) ,
        L11                   CHAR(11) 

    DEFINE #loc #integer
        cont_nss_unicos       ,
        total_nss_sb1         ,
        total_nss_sb2         ,
        total_nss_sb3         ,
        total_nss_sb4         ,
        total_nss_sb5         INTEGER

    DEFINE #loc #
        d16_monto             ,
        vprecio_sb1           ,
        vprecio_sb2           ,
        vprecio_sb3           ,
        vprecio_sb4           ,
        vprecio_sb5           DECIMAL(16,6)
        
    DEFINE #loc #date
        vfecha_genera         DATE
          
    OUTPUT
        --PAGE LENGTH   90
        PAGE LENGTH   45
        --PAGE LENGTH    0 
        LEFT MARGIN    0
        --RIGHT MARGIN 150
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    FIRST PAGE HEADER
        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9 = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

        LET G_salario_75_dias_sb1    = 0
        LET G_monto_accion_97_sb1    = 0
        LET G_monto_accion_cv_sb1    = 0
        LET G_monto_accion_so_sb1    = 0
        LET G_mto_accion_est_esp_sb1 = 0

        LET G_salario_75_dias_sb2    = 0
        LET G_monto_accion_97_sb2    = 0
        LET G_monto_accion_cv_sb2    = 0
        LET G_monto_accion_so_sb2    = 0
        LET G_mto_accion_est_esp_sb2 = 0

        LET G_salario_75_dias_sb3    = 0
        LET G_monto_accion_97_sb3    = 0
        LET G_monto_accion_cv_sb3    = 0
        LET G_monto_accion_so_sb3    = 0
        LET G_mto_accion_est_esp_sb3 = 0

        LET G_salario_75_dias_sb4    = 0
        LET G_monto_accion_97_sb4    = 0
        LET G_monto_accion_cv_sb4    = 0
        LET G_monto_accion_so_sb4    = 0
        LET G_mto_accion_est_esp_sb4 = 0
      
        LET G_salario_75_dias_sb5    = 0
        LET G_monto_accion_97_sb5    = 0
        LET G_monto_accion_cv_sb5    = 0
        LET G_monto_accion_so_sb5    = 0
        LET G_mto_accion_est_esp_sb5 = 0

        SELECT UNIQUE fecha_genera
        INTO   vfecha_genera
        FROM   ret_parcial
        WHERE  folio       = vfolio_oper12
        
        SELECT precio_del_dia
        INTO   vprecio_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 1

        SELECT precio_del_dia
        INTO   vprecio_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 2
        
        SELECT precio_del_dia
        INTO   vprecio_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 3

        SELECT precio_del_dia
        INTO   vprecio_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 4

        SELECT precio_del_dia
        INTO   vprecio_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 5

        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote
        WHERE  folio = vfolio_oper12

        IF status = NOTFOUND THEN
           LET d_fecha_operacion = ""
        END IF

        IF w_codigo_afore = 532 THEN
            LET encabezado = "S U B D I R E C C I O N    D E    R E T I R O S"
        ELSE 
        	  LET encabezado = "      M O D U L O   D E   R E T I R O S        "     
        END IF 	  
       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
       PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

       -- PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
         --   '\033e\033(s16H'

       PRINT
            --COLUMN 045,encabezado
            COLUMN 045,encabezado
            --'\033015'

       SKIP 1 LINES

       PRINT COLUMN 45,"           RETIRO PARCIAL DESEMPLEO"
            --'\033015'
       SKIP 4 LINES

       PRINT
            COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
            COLUMN 108,"PROG.    : RETL801"
           -- '\033015'
       PRINT
       PRINT
            COLUMN 1,"FOLIO INTERNO     : ", vfolio_oper12 USING"##########",
            COLUMN 108,"PAGINA   :    ",PAGENO USING "####"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"TIPO DE OPERACION : REPORTE DE PROVISION DE RETIRO PARCIAL POR DESEMPLEO     ",
            COLUMN 108,"FECHA : ", HOY USING "DD/MM/YYYY"
          --  '\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA GENERA      : ",vfecha_genera USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
      PRINT COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
      PRINT COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
            

       PRINT '\033e\033(s218T\033(s16H\033(s7B'

       PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L5,
                       "\302", L5,
                       "\302", L5,
                       "\302",L9,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\277"
                       --'\033015'


       PRINT
       PRINT
            COLUMN 001,"|          |           |",
            COLUMN 060,"|",
            COLUMN 066,"|",
            COLUMN 072,"|",
            COLUMN 074,"75 DIAS",
            COLUMN 082,"|      10%",
            COLUMN 098,"|      10%",
            COLUMN 114,"|      10%",
            COLUMN 130,"|      10%",
            COLUMN 146,"|               |"
            --'\033015'

       PRINT
       PRINT
            COLUMN 1,"|  CONSEC. |    NSS    |       NOMBRE DEL TRABAJADOR       |",
            COLUMN 066,"|",
            COLUMN 072,"|",
            COLUMN 074,"SALARIO",
            COLUMN 082,"|"   ,
            COLUMN 086,"RETIRO 97" ,
            COLUMN 098,"|"   ,
            COLUMN 100,"CES. Y VEJEZ" ,
            COLUMN 114,"|"   ,
            COLUMN 116,"CUOTA  SOCIAL" ,
            COLUMN 130,"|"   ,
            COLUMN 131,"ESTATAL/ESPECIA" ,
            COLUMN 146,"|     TOTAL     |"
            --'\033015'

       PRINT
       PRINT
            COLUMN 01,"|          |           |",
            COLUMN 60,"|SIEF.|DIAG.",
            COLUMN 72,"| (PESOS) |  (ACCIONES)   |  (ACCIONES)   |   (ACCIONES)  ",
                      "|   (ACCIONES)  |   (ACCIONES)  |"
            --'\033015'
                                                                               
       PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L5,
                     "\301",L5,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L5,L10,
                     "\301",L10,L5,
                     "\331"
           -- '\033015'

    PAGE HEADER
        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9 = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

        SELECT UNIQUE fecha_genera
        INTO   vfecha_genera
        FROM   ret_parcial
        WHERE  folio       = vfolio_oper12
        
        SELECT precio_del_dia
        INTO   vprecio_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 1

        SELECT precio_del_dia
        INTO   vprecio_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 3
        
        SELECT precio_del_dia
        INTO   vprecio_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 4

        SELECT precio_del_dia
        INTO   vprecio_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 5

        SELECT precio_del_dia
        INTO   vprecio_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 2

        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote
        WHERE  folio = vfolio_oper12
        IF status = NOTFOUND THEN
           LET d_fecha_operacion = ""
        END IF

        IF w_codigo_afore = 532 THEN
            LET encabezado = "S U B D I R E C C I O N    D E    R E T I R O S"
        ELSE 
        	  LET encabezado = "      M O D U L O   D E   R E T I R O S        "     
        END IF 	  

       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
       PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

       -- PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
         --   '\033e\033(s16H'

       PRINT
            --COLUMN 045,encabezado
            COLUMN 045,encabezado
          --  '\033015'

       SKIP 1 LINES

       PRINT COLUMN 45,"           RETIRO PARCIAL DESEMPLEO"
            --'\033015'
       SKIP 4 LINES

       PRINT
            COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
            COLUMN 108,"PROG.    : RETL801"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"FOLIO INTERNO     : ", vfolio_oper12 USING"##########",
            COLUMN 108,"PAGINA   :    ",PAGENO USING "####"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"TIPO DE OPERACION : REPORTE DE PROVISION DE RETIRO PARCIAL POR DESEMPLEO     ",
            COLUMN 108,"FECHA : ", HOY USING "DD/MM/YYYY"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA GENERA      : ",vfecha_genera USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&"
            --'\033015'
      PRINT
      PRINT COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&" 
            --'\033015'
      PRINT
      PRINT COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&" 
            --'\033015'
            
      PRINT

       PRINT '\033e\033(s218T\033(s16H\033(s7B'

       PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L5,
                       "\302", L5,
                       "\302", L5,
                       "\302",L9,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\277"
            --'\033015'

       PRINT
            COLUMN 001,"|          |           |",
            COLUMN 060,"|",
            COLUMN 066,"|",
            COLUMN 072,"|",
            COLUMN 074,"75 DIAS",
            COLUMN 082,"|      10%",
            COLUMN 098,"|      10%",
            COLUMN 114,"|      10%",
            COLUMN 130,"|      10%",
            COLUMN 146,"|               |"
            --'\033015'

      PRINT
       PRINT
            COLUMN 1,"|  CONSEC. |    NSS    |       NOMBRE DEL TRABAJADOR       |",
            COLUMN 066,"|",
            COLUMN 072,"|",
            COLUMN 074,"SALARIO",
            COLUMN 082,"|"   ,
            COLUMN 086,"RETIRO 97" ,
            COLUMN 098,"|"   ,
            COLUMN 100,"CES. Y VEJEZ" ,
            COLUMN 114,"|"   ,
            COLUMN 116,"CUOTA  SOCIAL" ,
            COLUMN 130,"|"   ,
            COLUMN 131,"ESTATAL/ESPECIA" ,
            COLUMN 146,"|     TOTAL     |"
            --'\033015'

      PRINT
       PRINT
            COLUMN 01,"|          |           |",
            COLUMN 60,"|SIEF.|DIAG.",
            COLUMN 72,"| (PESOS) |  (ACCIONES)   |  (ACCIONES)   |   (ACCIONES)  ",
                      "|   (ACCIONES)  |   (ACCIONES)  |"
           -- '\033015'
                                                                               
       PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L5,
                     "\301",L5,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L5,L10,
                     "\301",L10,L5,
                     "\331"
            --'\033015'

    ON EVERY ROW
        LET vsalario_75_dias = vsalario_75_dias  

        INITIALIZE nombre_final, vnombres, vpaterno, vmaterno TO NULL

        SELECT @nombres, @paterno, @materno
        INTO vnombres, vpaterno, vmaterno
        FROM afi_mae_afiliado
        WHERE @n_seguro = vnss 

        LET nombre_final = vpaterno CLIPPED, " ",
                           vmaterno CLIPPED, " ", 
                           vnombres CLIPPED

        LET d16_monto = vporc10_accion_97 +
                        vporc10_accion_cv +
                        vporc10_accion_so +
                        vp10_accion_est_esp

        PRINT                                                                 
        IF vp10_accion_est_esp IS NULL THEN
           LET vp10_accion_est_esp = 0
        END IF
        IF vporc10_accion_97  IS NULL THEN
           LET vporc10_accion_97  = 0
        END IF
        IF vporc10_accion_cv  IS NULL THEN
           LET vporc10_accion_cv  = 0
        END IF
        IF vporc10_accion_so  IS NULL THEN
           LET vporc10_accion_so  = 0
        END IF

        PRINT                                                                 
            COLUMN 004,vconsecutivo   USING "######"                ,
            COLUMN 013,vnss                                         ,
            COLUMN 026,nombre_final[1,35]                           ,
            COLUMN 062,vnombre_siefore                              ,
            COLUMN 073,vsalario_75_dias     USING "#####&.&&"       ,
            COLUMN 083,vporc10_accion_97    USING "#######&.&&&&&&" ,
            COLUMN 099,vporc10_accion_cv    USING "#######&.&&&&&&" ,
            COLUMN 115,vporc10_accion_so    USING "#######&.&&&&&&" ,
            COLUMN 131,vp10_accion_est_esp  USING "#######&.&&&&&&" ,
            COLUMN 147,(vporc10_accion_97+
                        vporc10_accion_cv+
                        vporc10_accion_so+
                        vp10_accion_est_esp)  USING "#######&.&&&&&&" 
           -- '\033015'
                                                                               
            IF vnombre_siefore = "SB1" THEN
            	 LET sb1                     = "SB1" 
                 LET G_salario_75_dias_sb1   = G_salario_75_dias_sb1  + vsalario_75_dias
                 LET G_monto_accion_97_sb1   = G_monto_accion_97_sb1  + vporc10_accion_97 
                 LET G_monto_accion_cv_sb1   = G_monto_accion_cv_sb1  + vporc10_accion_cv 
                 LET G_monto_accion_so_sb1   = G_monto_accion_so_sb1  + vporc10_accion_so 
                 LET G_mto_accion_est_esp_sb1= G_mto_accion_est_esp_sb1  + vp10_accion_est_esp 
                 LET total_nss_sb1           = total_nss_sb1 + 1
            ELSE 
             IF vnombre_siefore = "SB2" THEN
            	 LET sb2                     = "SB2" 
            	 LET G_salario_75_dias_sb2   = G_salario_75_dias_sb2  + vsalario_75_dias
                 LET G_monto_accion_97_sb2   = G_monto_accion_97_sb2  + vporc10_accion_97 
                 LET G_monto_accion_cv_sb2   = G_monto_accion_cv_sb2  + vporc10_accion_cv 
                 LET G_monto_accion_so_sb2   = G_monto_accion_so_sb2  + vporc10_accion_so 
                 LET G_mto_accion_est_esp_sb2= G_mto_accion_est_esp_sb2  + vp10_accion_est_esp
                 LET total_nss_sb2           = total_nss_sb2 + 1
             ELSE
              IF vnombre_siefore = "SB3" THEN
            	 LET sb3                     = "SB3" 
            	 LET G_salario_75_dias_sb3   = G_salario_75_dias_sb3  + vsalario_75_dias
                 LET G_monto_accion_97_sb3   = G_monto_accion_97_sb3  + vporc10_accion_97 
                 LET G_monto_accion_cv_sb3   = G_monto_accion_cv_sb3  + vporc10_accion_cv 
                 LET G_monto_accion_so_sb3   = G_monto_accion_so_sb3  + vporc10_accion_so 
                 LET G_mto_accion_est_esp_sb3= G_mto_accion_est_esp_sb3  + vp10_accion_est_esp
                 LET total_nss_sb3           = total_nss_sb3 + 1
              ELSE
               IF vnombre_siefore = "SB4" THEN
            	  LET sb4                     = "SB4" 
            	  LET G_salario_75_dias_sb4   = G_salario_75_dias_sb4  + vsalario_75_dias
                  LET G_monto_accion_97_sb4   = G_monto_accion_97_sb4  + vporc10_accion_97 
                  LET G_monto_accion_cv_sb4   = G_monto_accion_cv_sb4  + vporc10_accion_cv 
                  LET G_monto_accion_so_sb4   = G_monto_accion_so_sb4  + vporc10_accion_so 
                  LET G_mto_accion_est_esp_sb4= G_mto_accion_est_esp_sb4  + vp10_accion_est_esp
                  LET total_nss_sb4           = total_nss_sb4 + 1
               ELSE
                IF vnombre_siefore = "SB5" THEN
            	   LET sb5                     = "SB5" 
            	   LET G_salario_75_dias_sb5   = G_salario_75_dias_sb5  + vsalario_75_dias
                   LET G_monto_accion_97_sb5   = G_monto_accion_97_sb5  + vporc10_accion_97 
                   LET G_monto_accion_cv_sb5   = G_monto_accion_cv_sb5  + vporc10_accion_cv 
                   LET G_monto_accion_so_sb5   = G_monto_accion_so_sb5  + vporc10_accion_so 
                   LET G_mto_accion_est_esp_sb5= G_mto_accion_est_esp_sb5  + vp10_accion_est_esp
                   LET total_nss_sb5           = total_nss_sb5 + 1
                END IF 
               END IF 
              END IF 
             END IF 
            END IF 

            {IF lineno > 46 THEN
                SKIP TO TOP OF PAGE
            END IF}

    ON LAST ROW

        SKIP 3 LINES                                                          

        -------------------------
        --IMPRIME TOTALES POR SB1
        -------------------------
        IF total_nss_sb1 > 0 THEN
        	
            LET G_salario_75_dias_sb1 = G_salario_75_dias_sb1  
            PRINT
                COLUMN 001,"\332",L10,
                           "\302",L10,L1,
                           "\302",L10,L10,L10,L5,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'
            
            PRINT                                                                 
                COLUMN 001,"|          |"                  ,
                COLUMN 015,total_nss_sb1 USING "#######","  |" ,
                COLUMN 037,"T O T A L E S   : "           ,
                COLUMN 060,"|"                             ,
                COLUMN 062,sb1                             ,
                COLUMN 066,"|"                             ,
                COLUMN 071,G_salario_75_dias_sb1  USING "########.&&"     ,
                COLUMN 082,"|"                             ,
                COLUMN 083,G_monto_accion_97_sb1  USING "#######&.&&&&&&" ,
                COLUMN 098,"|"                             ,
                COLUMN 099,G_monto_accion_cv_sb1  USING "#######&.&&&&&&" ,
                COLUMN 114,"|"                             ,
                COLUMN 115,G_monto_accion_so_sb1  USING "#######&.&&&&&&" ,
                COLUMN 130,"|"                             ,
                COLUMN 131,G_mto_accion_est_esp_sb1  USING "#######&.&&&&&&" ,
                COLUMN 146,"|",
                COLUMN 147,(G_monto_accion_97_sb1+
                            G_monto_accion_cv_sb1+
                            G_monto_accion_so_sb1+
                            G_mto_accion_est_esp_sb1) USING "#######&.&&&&&&",
                COLUMN 162,"|"
            --'\033015'
                                                                                 
            PRINT
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
           -- '\033015'
        END IF
        	
        
        -------------------------
        --IMPRIME TOTALES POR SB2
        -------------------------
        IF total_nss_sb2 > 0 THEN
        	
            LET G_salario_75_dias_sb2 = G_salario_75_dias_sb2  
            PRINT
                COLUMN 001,"\332",L10,
                           "\302",L10,L1,
                           "\302",L10,L10,L10,L5,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'
            
            PRINT                                                                 
                COLUMN 001,"|          |"                  ,
                COLUMN 015,total_nss_sb2 USING "#######","  |" ,
                COLUMN 037,"T O T A L E S   : "           ,
                COLUMN 060,"|"                             ,
                COLUMN 062,sb2                             ,
                COLUMN 066,"|"                             ,
                COLUMN 071,G_salario_75_dias_sb2  USING "########.&&"     ,
                COLUMN 082,"|"                             ,
                COLUMN 083,G_monto_accion_97_sb2  USING "#######&.&&&&&&" ,
                COLUMN 098,"|"                             ,
                COLUMN 099,G_monto_accion_cv_sb2  USING "#######&.&&&&&&" ,
                COLUMN 114,"|"                             ,
                COLUMN 115,G_monto_accion_so_sb2  USING "#######&.&&&&&&" ,
                COLUMN 130,"|"                             ,
                COLUMN 131,G_mto_accion_est_esp_sb2  USING "#######&.&&&&&&" ,
                COLUMN 146,"|",
                COLUMN 147,(G_monto_accion_97_sb2+
                            G_monto_accion_cv_sb2+
                            G_monto_accion_so_sb2+
                            G_mto_accion_est_esp_sb2) USING "#######&.&&&&&&",
                COLUMN 162,"|"
            --'\033015'
                                                                                 
            PRINT
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
            --'\033015'
        END IF 
        -------------------------
        --IMPRIME TOTALES POR SB3
        -------------------------
        IF total_nss_sb3 > 0 THEN
        	
            LET G_salario_75_dias_sb3 = G_salario_75_dias_sb3  
            PRINT
                COLUMN 001,"\332",L10,
                           "\302",L10,L1,
                           "\302",L10,L10,L10,L5,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'
            
            PRINT                                                                 
                COLUMN 001,"|          |"                  ,
                COLUMN 015,total_nss_sb3 USING "#######","  |" ,
                COLUMN 037,"T O T A L E S   : "           ,
                COLUMN 060,"|"                             ,
                COLUMN 062,sb3                             ,
                COLUMN 066,"|"                             ,
                COLUMN 071,G_salario_75_dias_sb3  USING "########.&&"     ,
                COLUMN 082,"|"                             ,
                COLUMN 083,G_monto_accion_97_sb3  USING "#######&.&&&&&&" ,
                COLUMN 098,"|"                             ,
                COLUMN 099,G_monto_accion_cv_sb3  USING "#######&.&&&&&&" ,
                COLUMN 114,"|"                             ,
                COLUMN 115,G_monto_accion_so_sb3  USING "#######&.&&&&&&" ,
                COLUMN 130,"|"                             ,
                COLUMN 131,G_mto_accion_est_esp_sb3  USING "#######&.&&&&&&" ,
                COLUMN 146,"|",
                COLUMN 147,(G_monto_accion_97_sb3+
                            G_monto_accion_cv_sb3+
                            G_monto_accion_so_sb3+
                            G_mto_accion_est_esp_sb3) USING "#######&.&&&&&&",
                COLUMN 162,"|"
           -- '\033015'
                                                                                 
            PRINT
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
            --'\033015'
        END IF 
        -------------------------
        --IMPRIME TOTALES POR SB4
        -------------------------
        IF total_nss_sb4 > 0 THEN
        	
            LET G_salario_75_dias_sb4 = G_salario_75_dias_sb4  
            PRINT
                COLUMN 001,"\332",L10,
                           "\302",L10,L1,
                           "\302",L10,L10,L10,L5,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'
            
            PRINT                                                                 
                COLUMN 001,"|          |"                  ,
                COLUMN 015,total_nss_sb4 USING "#######","  |" ,
                COLUMN 037,"T O T A L E S   : "           ,
                COLUMN 060,"|"                             ,
                COLUMN 062,sb4                             ,
                COLUMN 066,"|"                             ,
                COLUMN 071,G_salario_75_dias_sb4  USING "########.&&"     ,
                COLUMN 082,"|"                             ,
                COLUMN 083,G_monto_accion_97_sb4  USING "#######&.&&&&&&" ,
                COLUMN 098,"|"                             ,
                COLUMN 099,G_monto_accion_cv_sb4  USING "#######&.&&&&&&" ,
                COLUMN 114,"|"                             ,
                COLUMN 115,G_monto_accion_so_sb4  USING "#######&.&&&&&&" ,
                COLUMN 130,"|"                             ,
                COLUMN 131,G_mto_accion_est_esp_sb4  USING "#######&.&&&&&&" ,
                COLUMN 146,"|",
                COLUMN 147,(G_monto_accion_97_sb4+
                            G_monto_accion_cv_sb4+
                            G_monto_accion_so_sb4+
                            G_mto_accion_est_esp_sb4) USING "#######&.&&&&&&",
                COLUMN 162,"|"
            --'\033015'
                                                                                 
            PRINT
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
            --'\033015'
        END IF 
        -------------------------
        --IMPRIME TOTALES POR SB5
        -------------------------
        IF total_nss_sb5 > 0 THEN
        	
            LET G_salario_75_dias_sb5 = G_salario_75_dias_sb5  
            PRINT
                COLUMN 001,"\332",L10,
                           "\302",L10,L1,
                           "\302",L10,L10,L10,L5,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'
            
            PRINT                                                                 
                COLUMN 001,"|          |"                  ,
                COLUMN 015,total_nss_sb5 USING "#######","  |" ,
                COLUMN 037,"T O T A L E S   : "           ,
                COLUMN 060,"|"                             ,
                COLUMN 062,sb5                             ,
                COLUMN 066,"|"                             ,
                COLUMN 071,G_salario_75_dias_sb5  USING "########.&&"     ,
                COLUMN 082,"|"                             ,
                COLUMN 083,G_monto_accion_97_sb5  USING "#######&.&&&&&&" ,
                COLUMN 098,"|"                             ,
                COLUMN 099,G_monto_accion_cv_sb5  USING "#######&.&&&&&&" ,
                COLUMN 114,"|"                             ,
                COLUMN 115,G_monto_accion_so_sb5  USING "#######&.&&&&&&" ,
                COLUMN 130,"|"                             ,
                COLUMN 131,G_mto_accion_est_esp_sb5  USING "#######&.&&&&&&" ,
                COLUMN 146,"|",
                COLUMN 147,(G_monto_accion_97_sb5+
                            G_monto_accion_cv_sb5+
                            G_monto_accion_so_sb5+
                            G_mto_accion_est_esp_sb5) USING "#######&.&&&&&&",
                COLUMN 162,"|"
            --'\033015'
                                                                                 
            PRINT
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
            --'\033015'
        END IF 

        SELECT COUNT(UNIQUE nss)
        INTO   cont_nss_unicos
        FROM   ret_parcial
        WHERE  folio            = reg_1.folio_oper12
        AND    tipo_prestacion  = 6
        AND    estado_solicitud = 2
        
        IF cont_nss_unicos > 0 THEN 
            PRINT
                COLUMN 1,"\332",L10,
                         L10,L2,
                         L10,L10,L10,L6,
                         L6,
                         L10,L6,
                         L10,L6,
                         L10,L6,
                         L10,L6,
                         L10,L6,
                         L10,L6,
                         "\277"
            --'\033015'
            PRINT 
                COLUMN 001,"|",
                COLUMN 015,"TOTAL DE NSS UNICOS: ", cont_nss_unicos USING "#######",
                COLUMN 162,"|"
            --'\033015'

            PRINT 
                COLUMN 1,"\300",L10,
                         "\301",L11,
                         "\301",L10,L10,L10,L5,
                         "\301",L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\301",L10,L5,
                         "\331"
            --'\033015'
        END IF 
   PAGE TRAILER

      PRINT COLUMN 001,"REPORTE : ",HOY USING "DDMMYYYY",".801"  
          --  '\033015'
END REPORT 
