################################################################################
#Proyecto          => SISTEMA DE AFORES.( SAFRE )                              #
#Owner             => E. F. P.                                                 #
#Programa RETL803  => REPORTE DE OPERACION 16 (PARCIAL)                        #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Fecha             => 15 DE MAYO DE 2000                                       #
#Actualiza         => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiza   => 30 DE NOVIEMBRE DE 2004                                  #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #w_tabafore #w_paramgrales
        w_paramgrales            RECORD LIKE seg_modulo.*   ,
        w_tabafore               RECORD LIKE tab_afore_local.* ,
        nom_impresora            LIKE tab_cmd_impresora.comando_impresion
 
    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper_16            INTEGER ,
        fecha_pago               DATE
    END RECORD

    DEFINE 
        HOY                      DATE

    DEFINE #glo #char
        G_LISTA                  CHAR(100) ,
        usuario                  CHAR(008) ,
        enter                    CHAR(001)    
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        MESSAGE LINE LAST,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    CALL STARTLOG("RETL803.log")

    OPEN WINDOW retl8031 AT 4,4 WITH FORM "RETL8031" ATTRIBUTE (BORDER)
    DISPLAY "                               < Ctrl-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL803     REPORTE POSTERIOR A LA OPERACION 16 (Parcial)                     " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME reg_1.folio_oper_16 WITHOUT DEFAULTS

        BEFORE FIELD folio_oper_16
            DISPLAY reg_1.folio_oper_16 TO folio_oper_16 

        AFTER FIELD folio_oper_16 
            IF reg_1.folio_oper_16 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " 
                NEXT FIELD folio_oper_16
            END IF

            SELECT 'OK'
            FROM ret_parcial_tx
            WHERE @folio = reg_1.folio_oper_16
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               MESSAGE "  NO EXISTE INFORMACION PARA ESTE FOLIO  "
               ATTRIBUTE(REVERSE)
               SLEEP 2
               MESSAGE ""
               NEXT FIELD folio_oper_16
            END IF

        ON KEY (ESC)
            IF reg_1.folio_oper_16 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " 
                NEXT FIELD folio_oper_16
            ELSE
                SELECT UNIQUE fecha_valuacion
                INTO   reg_1.fecha_pago
                FROM   ret_parcial_tx
                WHERE  folio            = reg_1.folio_oper_16
                AND    fecha_valuacion <> '01/01/0001'

                IF STATUS = NOTFOUND THEN
                   ERROR " NO EXISTE FOLIO ... "
                   INITIALIZE reg_1.folio_oper_16 TO NULL
                   NEXT FIELD folio_oper_16
                ELSE 
                   EXIT INPUT
                END IF
                EXIT INPUT
            END IF

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    SELECT UNIQUE fecha_valuacion
    INTO   reg_1.fecha_pago
    FROM   ret_parcial_tx
    WHERE  folio            = reg_1.folio_oper_16
    AND    fecha_valuacion <> '01/01/0001'

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()  #pp
    CALL segundo_paso() #sp
    CLOSE WINDOW retl8031
    CALL tercer_paso()  #tp

    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT *,
           USER
    INTO   w_tabafore.*,
           usuario
    FROM   tab_afore_local
                                                                               
    SELECT comando_impresion
    INTO   nom_impresora
    FROM   tab_cmd_impresora
    WHERE  codigo_afore = w_tabafore.codigo_afore

    SELECT MAX(folio)
    INTO   reg_1.folio_oper_16
    FROM   ret_parcial_tx                                                   

    SELECT *                                                             
    INTO   w_paramgrales.*                                               
    FROM   seg_modulo
    WHERE  @modulo_cod = 'ret'

    LET G_LISTA = w_paramgrales.ruta_listados CLIPPED,"/",HOY USING"DDMMYYYY",
                  ".803"
END FUNCTION


FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_2d RECORD #loc #reg_2                                  
        consecutivo           LIKE ret_parcial_tx.consecutivo  ,
        nss                   LIKE ret_parcial_tx.nss          ,
        nom_trabajador        CHAR(120)                        ,
        acciones_pagadas      DECIMAL(22,6)                    , 
        tipo_prestacion       SMALLINT                         ,
        nombre_siefore        CHAR(003)                        ,
        impt_pagado           DECIMAL(16,6)                    ,  
        impt_ret_97           DECIMAL(16,6)                    ,
        impt_cuo_soc          DECIMAL(16,6)                    ,
        diag_cuenta_ind       LIKE ret_parcial.diag_cuenta_ind ,
        fecha_pago            DATE                             ,
        monto_pesos_97        DECIMAL(16,6)                    ,
        monto_pesos_cv        DECIMAL(16,6)                    ,
        monto_pesos_es        DECIMAL(16,6)                    ,
        monto_pesos_sp        DECIMAL(16,6)                    ,
        monto_pesos_cv_es_sp  DECIMAL(16,6)                    ,
        monto_pesos_so        DECIMAL(16,6)
    END RECORD                                                       
     
    DEFINE reg_7 RECORD #loc #reg_7
        siefore               LIKE dis_cuenta.siefore
    END RECORD

    DEFINE #loc #date
        f_fecha_pago          DATE

    DEFINE #loc #smallint
        aux_subcuenta_sb1         ,
        aux_subcuenta_sb2         ,
        aux_subcuenta_sb3         ,
        aux_subcuenta_sb4         ,
        aux_subcuenta_sb5         SMALLINT

    DEFINE #loc #decimal
        aux_monto_pesos_sb1       DECIMAL(16,6) ,
        aux_monto_acciones_sb1    DECIMAL(16,6) ,
        d6_total_acciones_sb1     DECIMAL(16,6) ,
        d6_monto_pesos_97_sb1     DECIMAL(16,6) ,
        d6_monto_pesos_cv_sb1     DECIMAL(16,6) ,
        d6_monto_pesos_es_sb1     DECIMAL(16,6) ,
        d6_monto_pesos_so_sb1     DECIMAL(16,6) ,
        monto_accion_97_sb1       DECIMAL(16,6) ,
        monto_accion_cv_sb1       DECIMAL(16,6) ,
        monto_accion_es_sb1       DECIMAL(16,6) ,
        monto_accion_sp_sb1       DECIMAL(16,6) ,
        monto_accion_so_sb1       DECIMAL(16,6) ,

        aux_monto_pesos_sb2       DECIMAL(16,6) ,
        aux_monto_acciones_sb2    DECIMAL(16,6) ,
        d6_total_acciones_sb2     DECIMAL(16,6) ,
        d6_monto_pesos_97_sb2     DECIMAL(16,6) ,
        d6_monto_pesos_cv_sb2     DECIMAL(16,6) ,
        d6_monto_pesos_es_sb2     DECIMAL(16,6) ,
        d6_monto_pesos_so_sb2     DECIMAL(16,6) ,
        monto_accion_97_sb2       DECIMAL(16,6) ,
        monto_accion_cv_sb2       DECIMAL(16,6) ,
        monto_accion_es_sb2       DECIMAL(16,6) ,
        monto_accion_sp_sb2       DECIMAL(16,6) ,
        monto_accion_so_sb2       DECIMAL(16,6) ,

        aux_monto_pesos_sb3       DECIMAL(16,6) ,
        aux_monto_acciones_sb3    DECIMAL(16,6) ,
        d6_total_acciones_sb3     DECIMAL(16,6) ,
        d6_monto_pesos_97_sb3     DECIMAL(16,6) ,
        d6_monto_pesos_cv_sb3     DECIMAL(16,6) ,
        d6_monto_pesos_es_sb3     DECIMAL(16,6) ,
        d6_monto_pesos_so_sb3     DECIMAL(16,6) ,
        monto_accion_97_sb3       DECIMAL(16,6) ,
        monto_accion_cv_sb3       DECIMAL(16,6) ,
        monto_accion_es_sb3       DECIMAL(16,6) ,
        monto_accion_sp_sb3       DECIMAL(16,6) ,
        monto_accion_so_sb3       DECIMAL(16,6) ,

        aux_monto_pesos_sb4       DECIMAL(16,6) ,
        aux_monto_acciones_sb4    DECIMAL(16,6) ,
        d6_total_acciones_sb4     DECIMAL(16,6) ,
        d6_monto_pesos_97_sb4     DECIMAL(16,6) ,
        d6_monto_pesos_cv_sb4     DECIMAL(16,6) ,
        d6_monto_pesos_es_sb4     DECIMAL(16,6) ,
        d6_monto_pesos_so_sb4     DECIMAL(16,6) ,
        monto_accion_97_sb4       DECIMAL(16,6) ,
        monto_accion_cv_sb4       DECIMAL(16,6) ,
        monto_accion_es_sb4       DECIMAL(16,6) ,
        monto_accion_sp_sb4       DECIMAL(16,6) ,
        monto_accion_so_sb4       DECIMAL(16,6) ,

        aux_monto_pesos_sb5       DECIMAL(16,6) ,
        aux_monto_acciones_sb5    DECIMAL(16,6) ,
        d6_total_acciones_sb5     DECIMAL(16,6) ,
        d6_monto_pesos_97_sb5     DECIMAL(16,6) ,
        d6_monto_pesos_cv_sb5     DECIMAL(16,6) ,
        d6_monto_pesos_es_sb5     DECIMAL(16,6) ,
        d6_monto_pesos_so_sb5     DECIMAL(16,6) ,
        monto_accion_97_sb5       DECIMAL(16,6) ,
        monto_accion_cv_sb5       DECIMAL(16,6) ,
        monto_accion_es_sb5       DECIMAL(16,6) ,
        monto_accion_sp_sb5       DECIMAL(16,6) ,
        monto_accion_so_sb5       DECIMAL(16,6)

    DECLARE cur_1 CURSOR FOR                         
    SELECT A.consecutivo     ,
           A.nss             ,
           B.tipo_prestacion ,
           0                 ,
           B.diag_cuenta_ind ,
           A.fecha_pago
    FROM   ret_parcial_tx A,
           ret_parcial B
    WHERE  A.nss              = B.nss
    AND    A.consecutivo      = B.consecutivo
    AND    A.folio            = reg_1.folio_oper_16
    AND    B.diag_cuenta_ind  = "400"
    AND    B.tipo_prestacion IN (6,7)
    ORDER BY 3,2,1

    START REPORT listado_1 TO G_LISTA
    FOREACH cur_1 INTO reg_2d.consecutivo     ,
                       reg_2d.nss             ,
                       reg_2d.tipo_prestacion ,
                       reg_2d.impt_pagado     ,
                       reg_2d.diag_cuenta_ind ,
                       reg_2d.fecha_pago

        LET d6_total_acciones_sb1       = 0
        LET monto_accion_97_sb1         = 0
        LET monto_accion_cv_sb1         = 0
        LET monto_accion_es_sb1         = 0
        LET monto_accion_sp_sb1         = 0
        LET monto_accion_so_sb1         = 0
        
        LET d6_total_acciones_sb2       = 0
        LET monto_accion_97_sb2         = 0
        LET monto_accion_cv_sb2         = 0
        LET monto_accion_es_sb2         = 0
        LET monto_accion_sp_sb2         = 0
        LET monto_accion_so_sb2         = 0
        
        LET d6_total_acciones_sb3       = 0
        LET monto_accion_97_sb3         = 0
        LET monto_accion_cv_sb3         = 0
        LET monto_accion_es_sb3         = 0
        LET monto_accion_sp_sb3         = 0
        LET monto_accion_so_sb3         = 0

        LET d6_total_acciones_sb4       = 0
        LET monto_accion_97_sb4         = 0
        LET monto_accion_cv_sb4         = 0
        LET monto_accion_es_sb4         = 0
        LET monto_accion_sp_sb4         = 0
        LET monto_accion_so_sb4         = 0

        LET d6_total_acciones_sb5       = 0
        LET monto_accion_97_sb5         = 0
        LET monto_accion_cv_sb5         = 0
        LET monto_accion_es_sb5         = 0
        LET monto_accion_sp_sb5         = 0
        LET monto_accion_so_sb5         = 0

        LET reg_2d.monto_pesos_97       = 0
        LET reg_2d.monto_pesos_cv       = 0 
        LET reg_2d.monto_pesos_es       = 0
        LET reg_2d.monto_pesos_sp       = 0
        LET reg_2d.monto_pesos_cv_es_sp = 0
        LET reg_2d.monto_pesos_so       = 0
        
        DECLARE cur_7 CURSOR FOR
        SELECT siefore 
        FROM   dis_cuenta
        WHERE  nss     = reg_2d.nss
        AND    siefore NOT IN(0,11)
        GROUP BY 1
        ORDER BY 1

        FOREACH cur_7 INTO reg_7.*
            CASE reg_7.siefore
                WHEN 1
                    LET reg_2d.nombre_siefore = "SB1"
                    DECLARE cur_2 CURSOR  FOR
                    SELECT  subcuenta             ,
                            SUM(monto_en_pesos)   ,
                            SUM(monto_en_acciones)* -1
                    FROM    dis_cuenta                       
                    WHERE   folio            = reg_1.folio_oper_16
                    AND     subcuenta       IN (1,2,5,6,9)
                    AND     tipo_movimiento IN (870,875)
                    AND     nss              = reg_2d.nss
                    AND     consecutivo_lote = reg_2d.consecutivo 
                    AND     siefore          = reg_7.siefore
                    GROUP BY 1 
                    
                    LET reg_2d.monto_pesos_cv_es_sp = 0
                    LET reg_2d.monto_pesos_97       = 0

                    FOREACH cur_2 INTO aux_subcuenta_sb1    ,
                                       aux_monto_pesos_sb1  ,
                                       aux_monto_acciones_sb1 
                    
                        IF aux_monto_pesos_sb1 IS NULL OR aux_monto_pesos_sb1 = " " THEN
                            LET aux_monto_pesos_sb1 = 0
                        END IF       
                    
                        IF aux_monto_acciones_sb1 IS NULL OR aux_monto_acciones_sb1 = " " THEN
                            LET aux_monto_acciones_sb1 = 0
                        END IF       
                    
                        CASE aux_subcuenta_sb1 
                            WHEN 1 
                    
                                LET monto_accion_97_sb1   = aux_monto_acciones_sb1 
                                LET reg_2d.monto_pesos_97 = aux_monto_pesos_sb1
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_97       
                                EXIT CASE
                            WHEN 2 
                                LET monto_accion_cv_sb1         = aux_monto_acciones_sb1 
                                LET reg_2d.monto_pesos_cv       = aux_monto_pesos_sb1  
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_cv
                                
                                LET reg_2d.impt_pagado          = reg_2d.impt_pagado  +
                                                                  reg_2d.monto_pesos_cv
                                EXIT CASE
                            WHEN 5 
                                LET monto_accion_so_sb1   = aux_monto_acciones_sb1 
                                LET reg_2d.monto_pesos_so = aux_monto_pesos_sb1 
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_so
                                EXIT CASE
                            WHEN 6 
                                LET monto_accion_es_sb1   = aux_monto_acciones_sb1 
                                LET reg_2d.monto_pesos_es = aux_monto_pesos_sb1
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_es
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_es
                                EXIT CASE
                            WHEN 9 
                                LET monto_accion_sp_sb1   = aux_monto_acciones_sb1 
                                LET reg_2d.monto_pesos_sp = aux_monto_pesos_sb1
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_sp
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_sp
                                EXIT CASE
                        END CASE
                    
                        LET aux_monto_acciones_sb1 = 0
                        LET aux_monto_pesos_sb1    = 0
                    END FOREACH

                    SELECT SUM(monto_en_acciones) 
                    INTO   reg_2d.acciones_pagadas
                    FROM   dis_cuenta
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    tipo_movimiento in (870,875)
                    AND    nss              = reg_2d.nss 
                    AND    siefore          = reg_7.siefore

                    IF reg_2d.acciones_pagadas IS NULL OR 
                       reg_2d.acciones_pagadas = " " THEN
                        LET reg_2d.acciones_pagadas = 0
                    ELSE
                        OUTPUT TO REPORT listado_1(reg_2d.*) #l1
                    END IF      

                WHEN 2
                    LET reg_2d.nombre_siefore = "SB2"

                    DECLARE cur_3 CURSOR  FOR
                    SELECT subcuenta             ,
                           SUM(monto_en_pesos)   ,
                           SUM(monto_en_acciones)* -1
                    FROM   dis_cuenta                       
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    subcuenta       IN (1,2,5,6,9)
                    AND    tipo_movimiento IN (870,875)
                    AND    nss              = reg_2d.nss
                    AND    consecutivo_lote = reg_2d.consecutivo 
                    AND    siefore          = reg_7.siefore
                    GROUP BY 1 
                    
                    LET reg_2d.monto_pesos_cv_es_sp = 0
                    LET reg_2d.monto_pesos_97       = 0
                    
                    FOREACH cur_3 INTO aux_subcuenta_sb2    ,
                                       aux_monto_pesos_sb2  ,
                                       aux_monto_acciones_sb2 
                    
                        IF aux_monto_pesos_sb2 IS NULL OR aux_monto_pesos_sb2 = " " THEN
                            LET aux_monto_pesos_sb2 = 0
                        END IF       
                    
                        IF aux_monto_acciones_sb2 IS NULL OR aux_monto_acciones_sb2 = " " THEN
                            LET aux_monto_acciones_sb2 = 0
                        END IF       
                    
                        CASE aux_subcuenta_sb2 
                            WHEN 1 
                    
                                LET monto_accion_97_sb2   = aux_monto_acciones_sb2 
                                LET reg_2d.monto_pesos_97 = aux_monto_pesos_sb2
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_97       
                                EXIT CASE
                            WHEN 2 
                                LET monto_accion_cv_sb2         = aux_monto_acciones_sb2 
                                LET reg_2d.monto_pesos_cv       = aux_monto_pesos_sb2  
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_cv
                                
                                LET reg_2d.impt_pagado          = reg_2d.impt_pagado  +
                                                                  reg_2d.monto_pesos_cv
                                EXIT CASE
                            WHEN 5 
                                LET monto_accion_so_sb2   = aux_monto_acciones_sb2 
                                LET reg_2d.monto_pesos_so = aux_monto_pesos_sb2 
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_so
                                EXIT CASE
                            WHEN 6 
                                LET monto_accion_es_sb2   = aux_monto_acciones_sb2 
                                LET reg_2d.monto_pesos_es = aux_monto_pesos_sb2
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_es
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_es
                                EXIT CASE
                            WHEN 9 
                                LET monto_accion_sp_sb2   = aux_monto_acciones_sb2 
                                LET reg_2d.monto_pesos_sp = aux_monto_pesos_sb2
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_sp
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_sp
                                EXIT CASE
                        END CASE
                    
                        LET aux_monto_acciones_sb2 = 0
                        LET aux_monto_pesos_sb2    = 0
                    
                    END FOREACH

                    SELECT SUM(monto_en_acciones) 
                    INTO   reg_2d.acciones_pagadas
                    FROM   dis_cuenta
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    tipo_movimiento in (870,875)
                    AND    nss              = reg_2d.nss 
                    AND    siefore          = reg_7.siefore

                    IF reg_2d.acciones_pagadas IS NULL OR 
                       reg_2d.acciones_pagadas = " " THEN
                        LET reg_2d.acciones_pagadas = 0
                    ELSE
                        OUTPUT TO REPORT listado_1(reg_2d.*) #l1
                    END IF      
                WHEN 3
                    LET reg_2d.nombre_siefore = "SB3"

                    DECLARE cur_4 CURSOR  FOR
                    SELECT subcuenta             ,
                           SUM(monto_en_pesos)   ,
                           SUM(monto_en_acciones)* -1
                    FROM   dis_cuenta                       
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    subcuenta       IN (1,2,5,6,9)
                    AND    tipo_movimiento IN (870,875)
                    AND    nss              = reg_2d.nss
                    AND    consecutivo_lote = reg_2d.consecutivo 
                    AND    siefore          = reg_7.siefore
                    GROUP BY 1 
                    
                    LET reg_2d.monto_pesos_cv_es_sp = 0
                    LET reg_2d.monto_pesos_97       = 0
                    
                    FOREACH cur_4 INTO aux_subcuenta_sb3    ,
                                       aux_monto_pesos_sb3  ,
                                       aux_monto_acciones_sb3 
                    
                        IF aux_monto_pesos_sb3 IS NULL OR aux_monto_pesos_sb3 = " " THEN
                            LET aux_monto_pesos_sb3 = 0
                        END IF       
                    
                        IF aux_monto_acciones_sb3 IS NULL OR aux_monto_acciones_sb3 = " " THEN
                            LET aux_monto_acciones_sb3 = 0
                        END IF       
                    
                        CASE aux_subcuenta_sb3 
                            WHEN 1 
                    
                                LET monto_accion_97_sb3   = aux_monto_acciones_sb3 
                                LET reg_2d.monto_pesos_97 = aux_monto_pesos_sb3
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_97       
                                EXIT CASE
                            WHEN 2 
                                LET monto_accion_cv_sb3         = aux_monto_acciones_sb3 
                                LET reg_2d.monto_pesos_cv       = aux_monto_pesos_sb3  
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_cv
                                
                                LET reg_2d.impt_pagado          = reg_2d.impt_pagado  +
                                                                  reg_2d.monto_pesos_cv
                                EXIT CASE
                            WHEN 5 
                                LET monto_accion_so_sb3   = aux_monto_acciones_sb3 
                                LET reg_2d.monto_pesos_so = aux_monto_pesos_sb3 
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_so
                                EXIT CASE
                            WHEN 6 
                                LET monto_accion_es_sb3   = aux_monto_acciones_sb3 
                                LET reg_2d.monto_pesos_es = aux_monto_pesos_sb3
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_es
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_es
                                EXIT CASE
                            WHEN 9 
                                LET monto_accion_sp_sb3   = aux_monto_acciones_sb3 
                                LET reg_2d.monto_pesos_sp = aux_monto_pesos_sb3
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_sp
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_sp
                                EXIT CASE
                        END CASE
                    
                        LET aux_monto_acciones_sb3 = 0
                        LET aux_monto_pesos_sb3    = 0
                    
                    END FOREACH

                    SELECT SUM(monto_en_acciones) 
                    INTO   reg_2d.acciones_pagadas
                    FROM   dis_cuenta
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    tipo_movimiento in (870,875)
                    AND    nss              = reg_2d.nss 
                    AND    siefore          = reg_7.siefore

                    IF reg_2d.acciones_pagadas IS NULL OR 
                       reg_2d.acciones_pagadas = " " THEN
                        LET reg_2d.acciones_pagadas = 0
                    ELSE
                        OUTPUT TO REPORT listado_1(reg_2d.*) #l1
                    END IF      
                WHEN 4
                    LET reg_2d.nombre_siefore = "SB4"

                    DECLARE cur_5 CURSOR  FOR
                    SELECT subcuenta             ,
                           SUM(monto_en_pesos)   ,
                           SUM(monto_en_acciones)* -1
                    FROM   dis_cuenta                       
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    subcuenta       IN (1,2,5,6,9)
                    AND    tipo_movimiento IN (870,875)
                    AND    nss              = reg_2d.nss
                    AND    consecutivo_lote = reg_2d.consecutivo 
                    AND    siefore          = reg_7.siefore
                    GROUP BY 1 
                    
                    LET reg_2d.monto_pesos_cv_es_sp = 0
                    LET reg_2d.monto_pesos_97       = 0
                    
                    FOREACH cur_5 INTO aux_subcuenta_sb4    ,
                                       aux_monto_pesos_sb4  ,
                                       aux_monto_acciones_sb4 
                    
                        IF aux_monto_pesos_sb4 IS NULL OR aux_monto_pesos_sb4 = " " THEN
                            LET aux_monto_pesos_sb4 = 0
                        END IF       
                    
                        IF aux_monto_acciones_sb4 IS NULL OR aux_monto_acciones_sb4 = " " THEN
                            LET aux_monto_acciones_sb4 = 0
                        END IF       
                    
                        CASE aux_subcuenta_sb4 
                            WHEN 1 
                    
                                LET monto_accion_97_sb4   = aux_monto_acciones_sb4 
                                LET reg_2d.monto_pesos_97 = aux_monto_pesos_sb4
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_97       
                                EXIT CASE
                            WHEN 2 
                                LET monto_accion_cv_sb4         = aux_monto_acciones_sb4 
                                LET reg_2d.monto_pesos_cv       = aux_monto_pesos_sb4  
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_cv
                                
                                LET reg_2d.impt_pagado          = reg_2d.impt_pagado  +
                                                                  reg_2d.monto_pesos_cv
                                EXIT CASE
                            WHEN 5 
                                LET monto_accion_so_sb4   = aux_monto_acciones_sb4 
                                LET reg_2d.monto_pesos_so = aux_monto_pesos_sb4 
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_so
                                EXIT CASE
                            WHEN 6 
                                LET monto_accion_es_sb4   = aux_monto_acciones_sb4 
                                LET reg_2d.monto_pesos_es = aux_monto_pesos_sb4
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_es
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_es
                                EXIT CASE
                            WHEN 9 
                                LET monto_accion_sp_sb4   = aux_monto_acciones_sb4 
                                LET reg_2d.monto_pesos_sp = aux_monto_pesos_sb4
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_sp
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_sp
                                EXIT CASE
                        END CASE
                    
                        LET aux_monto_acciones_sb4 = 0
                        LET aux_monto_pesos_sb4    = 0
                    
                    END FOREACH

                    SELECT SUM(monto_en_acciones) 
                    INTO   reg_2d.acciones_pagadas
                    FROM   dis_cuenta
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    tipo_movimiento in (870,875)
                    AND    nss              = reg_2d.nss 
                    AND    siefore          = reg_7.siefore

                    IF reg_2d.acciones_pagadas IS NULL OR 
                       reg_2d.acciones_pagadas = " " THEN
                        LET reg_2d.acciones_pagadas = 0
                    ELSE
                        OUTPUT TO REPORT listado_1(reg_2d.*) #l1
                    END IF      
                WHEN 5
                    LET reg_2d.nombre_siefore = "SB5"

                    DECLARE cur_6 CURSOR  FOR
                    SELECT subcuenta             ,
                           SUM(monto_en_pesos)   ,
                           SUM(monto_en_acciones)* -1
                    FROM   dis_cuenta                       
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    subcuenta       IN (1,2,5,6,9)
                    AND    tipo_movimiento IN (870,875)
                    AND    nss              = reg_2d.nss
                    AND    consecutivo_lote = reg_2d.consecutivo 
                    AND    siefore          = reg_7.siefore
                    GROUP BY 1 
                    
                    LET reg_2d.monto_pesos_cv_es_sp = 0
                    LET reg_2d.monto_pesos_97       = 0
                    
                    FOREACH cur_6 INTO aux_subcuenta_sb5    ,
                                       aux_monto_pesos_sb5  ,
                                       aux_monto_acciones_sb5 
                    
                        IF aux_monto_pesos_sb5 IS NULL OR aux_monto_pesos_sb5 = " " THEN
                            LET aux_monto_pesos_sb5 = 0
                        END IF       
                    
                        IF aux_monto_acciones_sb5 IS NULL OR aux_monto_acciones_sb5 = " " THEN
                            LET aux_monto_acciones_sb5 = 0
                        END IF       
                    
                        CASE aux_subcuenta_sb5 
                            WHEN 1 
                    
                                LET monto_accion_97_sb5   = aux_monto_acciones_sb5 
                                LET reg_2d.monto_pesos_97 = aux_monto_pesos_sb5
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_97       
                                EXIT CASE
                            WHEN 2 
                                LET monto_accion_cv_sb5         = aux_monto_acciones_sb5 
                                LET reg_2d.monto_pesos_cv       = aux_monto_pesos_sb5  
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_cv
                                
                                LET reg_2d.impt_pagado          = reg_2d.impt_pagado  +
                                                                  reg_2d.monto_pesos_cv
                                EXIT CASE
                            WHEN 5 
                                LET monto_accion_so_sb5   = aux_monto_acciones_sb5 
                                LET reg_2d.monto_pesos_so = aux_monto_pesos_sb5 
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_so
                                EXIT CASE
                            WHEN 6 
                                LET monto_accion_es_sb5   = aux_monto_acciones_sb5 
                                LET reg_2d.monto_pesos_es = aux_monto_pesos_sb5
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_es
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_es
                                EXIT CASE
                            WHEN 9 
                                LET monto_accion_sp_sb5   = aux_monto_acciones_sb5 
                                LET reg_2d.monto_pesos_sp = aux_monto_pesos_sb5
                                LET reg_2d.monto_pesos_cv_es_sp = reg_2d.monto_pesos_cv_es_sp +
                                                                  reg_2d.monto_pesos_sp
                                LET reg_2d.impt_pagado    = reg_2d.impt_pagado  +
                                                            reg_2d.monto_pesos_sp
                                EXIT CASE
                        END CASE
                    
                        LET aux_monto_acciones_sb5 = 0
                        LET aux_monto_pesos_sb5    = 0
                    
                    END FOREACH

                    SELECT SUM(monto_en_acciones) 
                    INTO   reg_2d.acciones_pagadas
                    FROM   dis_cuenta
                    WHERE  folio            = reg_1.folio_oper_16
                    AND    tipo_movimiento in (870,875)
                    AND    nss              = reg_2d.nss 
                    AND    siefore          = reg_7.siefore

                    IF reg_2d.acciones_pagadas IS NULL OR 
                       reg_2d.acciones_pagadas = " " THEN
                        LET reg_2d.acciones_pagadas = 0
                    ELSE
                        OUTPUT TO REPORT listado_1(reg_2d.*) #l1
                    END IF      

            END CASE
        END FOREACH      
    END FOREACH
    FINISH REPORT listado_1
END FUNCTION


FUNCTION segundo_paso()
#sp-------------------
    DEFINE #loc #char
        lp                    CHAR(100),
        permisos              CHAR(100)

    WHILE TRUE
        PROMPT " DESEA IMPRIMIR  S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                LET lp = "lp ",G_LISTA CLIPPED
            --    LET lp = "vi ",G_LISTA CLIPPED
                RUN lp
                RETURN
            ELSE 
                PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
	    LET permisos = " chmod 777 ",G_LISTA CLIPPED
	    RUN permisos 
        END IF
    END WHILE
END FUNCTION


FUNCTION tercer_paso()
#tp-------------------
    OPEN WINDOW retl8031 AT 4,4 WITH FORM "RETL8031" ATTRIBUTE (BORDER)
    DISPLAY "                               < Ctrl-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL803     REPORTE POSTERIOR A LA OPERACION 16 (Parcial)                     " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)
    DISPLAY reg_1.folio_oper_16 TO folio_oper_16 
END FUNCTION


REPORT listado_1(reg_2d)
#l1--------------------
    DEFINE reg_2d RECORD #loc #reg_2                                  
        consecutivo           LIKE ret_parcial_tx.consecutivo  ,
        nss                   LIKE ret_parcial_tx.nss          ,
        nom_trabajador        CHAR(120)                        ,
        acciones_pagadas      DECIMAL(22,6)                    , 
        tipo_prestacion       SMALLINT                         ,
        nombre_siefore        CHAR(003)                        ,
        impt_pagado           DECIMAL(16,2)                    ,  
        impt_ret_97           DECIMAL(16,6)                    ,
        impt_cuo_soc          DECIMAL(16,6)                    ,
        diag_cuenta_ind       LIKE ret_parcial.diag_cuenta_ind ,
        fecha_pago            DATE                             ,
        monto_pesos_97        DECIMAL(16,2)                    ,
        monto_pesos_cv        DECIMAL(16,6)                    ,
        monto_pesos_es        DECIMAL(16,6)                    ,
        monto_pesos_sp        DECIMAL(16,6)                    ,
        monto_pesos_cv_es_sp  DECIMAL(16,2)                    ,
        monto_pesos_so        DECIMAL(16,2)
    END RECORD                                                       

    DEFINE #loc #date
        d_fecha_operacion     DATE

    DEFINE #loc #char
        desc_tipo_prestacion  CHAR(30) ,
        vnombres	            CHAR(40) ,
        vpaterno	            CHAR(40) ,
        vmaterno	            CHAR(40) ,
        nombre_final          CHAR(50) ,
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
        cont_tot_nss_unicos   ,
        total_nss_sb1         ,
        total_nss_sb2         ,
        total_nss_sb3         ,
        total_nss_sb4         ,
        total_nss_sb5         INTEGER

    DEFINE
        acciones_pagadas_sb1          DECIMAL(22,6) , 
        impt_pagado_sb1               DECIMAL(16,2) ,  
        monto_pesos_97_sb1            DECIMAL(16,2) ,
        monto_pesos_cv_es_sp_sb1      DECIMAL(16,2) ,
        monto_pesos_so_sb1            DECIMAL(16,2) ,

        acciones_pagadas_sb2          DECIMAL(22,6) , 
        impt_pagado_sb2               DECIMAL(16,2) ,  
        monto_pesos_97_sb2            DECIMAL(16,2) ,
        monto_pesos_cv_es_sp_sb2      DECIMAL(16,2) ,
        monto_pesos_so_sb2            DECIMAL(16,2) ,

        acciones_pagadas_sb3          DECIMAL(22,6) , 
        impt_pagado_sb3               DECIMAL(16,2) ,  
        monto_pesos_97_sb3            DECIMAL(16,2) ,
        monto_pesos_cv_es_sp_sb3      DECIMAL(16,2) ,
        monto_pesos_so_sb3            DECIMAL(16,2) ,

        acciones_pagadas_sb4          DECIMAL(22,6) , 
        impt_pagado_sb4               DECIMAL(16,2) ,  
        monto_pesos_97_sb4            DECIMAL(16,2) ,
        monto_pesos_cv_es_sp_sb4      DECIMAL(16,2) ,
        monto_pesos_so_sb4            DECIMAL(16,2) ,

        acciones_pagadas_sb5          DECIMAL(22,6) , 
        impt_pagado_sb5               DECIMAL(16,2) ,  
        monto_pesos_97_sb5            DECIMAL(16,2) ,
        monto_pesos_cv_es_sp_sb5      DECIMAL(16,2) ,
        monto_pesos_so_sb5            DECIMAL(16,2) ,

        sub_acciones_pagadas_sb1      DECIMAL(22,6) ,
        sub_importe_pagado_sb1        DECIMAL(16,6) ,
        sub_monto_pesos_97_sb1        DECIMAL(16,6) ,
        sub_monto_pesos_cv_es_sp_sb1  DECIMAL(16,6) ,
        sub_monto_pesos_so_sb1        DECIMAL(16,6) ,
        sub_total_sb1                 DECIMAL(16,6) ,
        sub_cont_sb1                  INTEGER       ,
        

        sub_acciones_pagadas_sb2      DECIMAL(22,6) ,
        sub_importe_pagado_sb2        DECIMAL(16,6) ,
        sub_monto_pesos_97_sb2        DECIMAL(16,6) ,
        sub_monto_pesos_cv_es_sp_sb2  DECIMAL(16,6) ,
        sub_monto_pesos_so_sb2        DECIMAL(16,6) ,
        sub_total_sb2                 DECIMAL(16,6) ,
        sub_cont_sb2                  INTEGER,      

        sub_acciones_pagadas_sb3      DECIMAL(22,6) ,
        sub_importe_pagado_sb3        DECIMAL(16,6) ,
        sub_monto_pesos_97_sb3        DECIMAL(16,6) ,
        sub_monto_pesos_cv_es_sp_sb3  DECIMAL(16,6) ,
        sub_monto_pesos_so_sb3        DECIMAL(16,6) ,
        sub_total_sb3                 DECIMAL(16,6) ,
        sub_cont_sb3                  INTEGER,      

        sub_acciones_pagadas_sb4      DECIMAL(22,6) ,
        sub_importe_pagado_sb4        DECIMAL(16,6) ,
        sub_monto_pesos_97_sb4        DECIMAL(16,6) ,
        sub_monto_pesos_cv_es_sp_sb4  DECIMAL(16,6) ,
        sub_monto_pesos_so_sb4        DECIMAL(16,6) ,
        sub_total_sb4                 DECIMAL(16,6) ,
        sub_cont_sb4                  INTEGER,      

        sub_acciones_pagadas_sb5      DECIMAL(22,6) ,
        sub_importe_pagado_sb5        DECIMAL(16,6) ,
        sub_monto_pesos_97_sb5        DECIMAL(16,6) ,
        sub_monto_pesos_cv_es_sp_sb5  DECIMAL(16,6) ,
        sub_monto_pesos_so_sb5        DECIMAL(16,6) ,
        sub_total_sb5                 DECIMAL(16,6) ,
        sub_cont_sb5                  INTEGER       



    DEFINE #loc #
        vprecio_sb1           ,
        vprecio_sb2           ,
        vprecio_sb3           ,
        vprecio_sb4           ,
        vprecio_sb5           DECIMAL(16,6)
        
    DEFINE #loc #date
        vfecha_conversion     DATE


    OUTPUT
        --PAGE LENGTH   90
        PAGE LENGTH   45
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
        LET L9  = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote
        WHERE  folio = reg_1.folio_oper_16
        IF status = NOTFOUND THEN
           LET d_fecha_operacion = ""
        END IF

        SELECT  fecha_conversion
        INTO    vfecha_conversion                    
        FROM    dis_cuenta                                     
        WHERE   folio            = reg_1.folio_oper_16         
        AND     subcuenta       IN (1,2,5,6,9)         
        AND     tipo_movimiento IN (870,875)                   
        AND     nss              = reg_2d.nss                  
        AND     consecutivo_lote = reg_2d.consecutivo          
        GROUP BY 1               

        SELECT precio_del_dia
        INTO   vprecio_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 1
        
        SELECT precio_del_dia
        INTO   vprecio_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 2

        SELECT precio_del_dia
        INTO   vprecio_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 3

        SELECT precio_del_dia
        INTO   vprecio_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 4

        SELECT precio_del_dia
        INTO   vprecio_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 5
                                      
        LET acciones_pagadas_sb1      = 0 
        LET impt_pagado_sb1           = 0  
        LET monto_pesos_97_sb1        = 0
        LET monto_pesos_cv_es_sp_sb1  = 0
        LET monto_pesos_so_sb1        = 0

        LET acciones_pagadas_sb2      = 0 
        LET impt_pagado_sb2           = 0  
        LET monto_pesos_97_sb2        = 0
        LET monto_pesos_cv_es_sp_sb2  = 0
        LET monto_pesos_so_sb2        = 0

        LET acciones_pagadas_sb3      = 0 
        LET impt_pagado_sb3           = 0  
        LET monto_pesos_97_sb3        = 0
        LET monto_pesos_cv_es_sp_sb3  = 0
        LET monto_pesos_so_sb3        = 0

        LET acciones_pagadas_sb4      = 0 
        LET impt_pagado_sb4           = 0  
        LET monto_pesos_97_sb4        = 0
        LET monto_pesos_cv_es_sp_sb4  = 0
        LET monto_pesos_so_sb4        = 0

        LET acciones_pagadas_sb5      = 0 
        LET impt_pagado_sb5           = 0  
        LET monto_pesos_97_sb5        = 0
        LET monto_pesos_cv_es_sp_sb5  = 0
        LET monto_pesos_so_sb5        = 0


        LET sub_acciones_pagadas_sb1      = 0
        LET sub_importe_pagado_sb1        = 0
        LET sub_monto_pesos_97_sb1        = 0
        LET sub_monto_pesos_cv_es_sp_sb1  = 0
        LET sub_monto_pesos_so_sb1        = 0
        LET sub_total_sb1                 = 0
        LET sub_cont_sb1                  = 0

        LET sub_acciones_pagadas_sb2      = 0
        LET sub_importe_pagado_sb2        = 0
        LET sub_monto_pesos_97_sb2        = 0
        LET sub_monto_pesos_cv_es_sp_sb2  = 0
        LET sub_monto_pesos_so_sb2        = 0
        LET sub_total_sb2                 = 0
        LET sub_cont_sb2                  = 0

        LET sub_acciones_pagadas_sb3      = 0
        LET sub_importe_pagado_sb3        = 0
        LET sub_monto_pesos_97_sb3        = 0
        LET sub_monto_pesos_cv_es_sp_sb3  = 0
        LET sub_monto_pesos_so_sb3        = 0
        LET sub_total_sb3                 = 0
        LET sub_cont_sb3                  = 0

        LET sub_acciones_pagadas_sb4      = 0
        LET sub_importe_pagado_sb4        = 0
        LET sub_monto_pesos_97_sb4        = 0
        LET sub_monto_pesos_cv_es_sp_sb4  = 0
        LET sub_monto_pesos_so_sb4        = 0
        LET sub_total_sb4                 = 0
        LET sub_cont_sb4                  = 0

        LET sub_acciones_pagadas_sb5      = 0
        LET sub_importe_pagado_sb5        = 0
        LET sub_monto_pesos_97_sb5        = 0
        LET sub_monto_pesos_cv_es_sp_sb5  = 0
        LET sub_monto_pesos_so_sb5        = 0
        LET sub_total_sb5                 = 0
        LET sub_cont_sb5                  = 0



        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
          --  '\033e\033(s16H'

        PRINT
            COLUMN 042,"S U B D I R E C C I O N  D E  B E N E F I C I O S"
            --'\033015'

         SKIP 1 LINES

         PRINT COLUMN  57, "RETIRO PARCIAL"
           -- '\033015'

         SKIP 4 LINES                                                          

        PRINT
            COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
            COLUMN 108,"PROG.    : RETL803"
          --  '\033015'

        PRINT
        PRINT
            COLUMN 1,"FOLIO INTERNO     : ",reg_1.folio_oper_16 USING"##########",
            COLUMN 108,"PAGINA   : ",PAGENO USING "####"
           -- '\033015'
        PRINT

        PRINT
            COLUMN 1,"TIPO DE OPERACION : REPORTE DE LIQUIDACION DE RETIRO PARCIAL OPERACION (16)",
            COLUMN 108,"FECHA : ", TODAY USING "DD/MM/YYYY"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA CONVERSION  : ",vfecha_conversion USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&"
           -- '\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&"
            --'\033015'

        PRINT '\033e\033(s218T\033(s14H\033(s7B'

        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L2,
                       "\302",L5,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
           -- '\033015'

        PRINT
            COLUMN 001,"|          |           |",
            COLUMN 062,"|  ACCIONES",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 091,"|",
            COLUMN 095,"IMPORTE",
            COLUMN 105,"|",
            COLUMN 119,"|",
            COLUMN 133,"|",
            COLUMN 149,"|"
           -- '\033015'

        PRINT
            COLUMN 1,"| CONSEC.  |    NSS    |         NOMBRE DEL ",
                     "TRABAJADOR       |            |",
            COLUMN 081,"|",
            COLUMN 091,"|",
            COLUMN 095,"PAGADO",
            COLUMN 105,"|",
            COLUMN 108,"RETIRO 97",
            COLUMN 119,"|",
            COLUMN 120,"CVEJ.EST.ESP.",
            COLUMN 133,"|",
            COLUMN 136,"CUOTA SOCIAL",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 01,"|          |           |",
            COLUMN 062,"|  PAGADAS ",
            COLUMN 75,"|PRES.",
            COLUMN 80,"| SIEFORE |   (PESOS)   |   (PESOS)   |   (PESOS)   ",
                      "|    (PESOS)    |"
           -- '\033015'
                                                                               
        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L2,
                     "\301",L5,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'

    PAGE HEADER
        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9  = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote
        WHERE  folio = reg_1.folio_oper_16
        IF status = NOTFOUND THEN
           LET d_fecha_operacion = ""
        END IF

        SELECT  fecha_conversion
        INTO    vfecha_conversion                    
        FROM    dis_cuenta                                     
        WHERE   folio            = reg_1.folio_oper_16         
        AND     subcuenta       IN (1,2,5,6,9)         
        AND     tipo_movimiento IN (870,875)                   
        AND     nss              = reg_2d.nss                  
        AND     consecutivo_lote = reg_2d.consecutivo          
        GROUP BY 1               

        SELECT precio_del_dia
        INTO   vprecio_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 1
        
        SELECT precio_del_dia
        INTO   vprecio_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 2
                                      
        SELECT precio_del_dia
        INTO   vprecio_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 3

        SELECT precio_del_dia
        INTO   vprecio_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 4

        SELECT precio_del_dia
        INTO   vprecio_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion  = vfecha_conversion
        AND    codigo_siefore   = 5

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
          --  '\033e\033(s16H'

        PRINT
            COLUMN 042,"S U B D I R E C C I O N  D E  B E N E F I C I O S"
           -- '\033015'

         SKIP 1 LINES

         PRINT COLUMN  57, "RETIRO PARCIAL"
           -- '\033015'

         SKIP 4 LINES                                                          

        PRINT
            COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
            COLUMN 108,"PROG.    : RETL803"
           -- '\033015'

        PRINT
        PRINT
            COLUMN 1,"FOLIO INTERNO     : ",reg_1.folio_oper_16 USING"##########",
            COLUMN 108,"PAGINA   : ",PAGENO USING "####"
           -- '\033015'
        PRINT

        PRINT
            COLUMN 1,"TIPO DE OPERACION : REPORTE DE LIQUIDACION DE RETIRO PARCIAL OPERACION (16)",
            COLUMN 108,"FECHA : ", TODAY USING "DD/MM/YYYY"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA CONVERSION  : ",vfecha_conversion USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&"
            --'\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&"
            --'\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&"
           -- '\033015'

        PRINT '\033e\033(s218T\033(s14H\033(s7B'

        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L2,
                       "\302",L5,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
          --  '\033015'

        PRINT
            COLUMN 001,"|          |           |",
            COLUMN 062,"|  ACCIONES",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 091,"|",
            COLUMN 095,"IMPORTE",
            COLUMN 105,"|",
            COLUMN 119,"|",
            COLUMN 133,"|",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 1,"| CONSEC.  |    NSS    |         NOMBRE DEL ",
                     "TRABAJADOR       |            |",
            COLUMN 081,"|",
            COLUMN 091,"|",
            COLUMN 095,"PAGADO",
            COLUMN 105,"|",
            COLUMN 108,"RETIRO 97",
            COLUMN 119,"|",
            COLUMN 120,"CVEJ.EST.ESP.",
            COLUMN 133,"|",
            COLUMN 136,"CUOTA SOCIAL",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 01,"|          |           |",
            COLUMN 062,"|  PAGADAS ",
            COLUMN 75,"|PRES.",
            COLUMN 80,"| SIEFORE |   (PESOS)   |   (PESOS)   |   (PESOS)   ",
                      "|    (PESOS)    |"
          --  '\033015'
                                                                               
        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L2,
                     "\301",L5,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
           -- '\033015'

    ON EVERY ROW

    LET reg_2d.impt_pagado = reg_2d.monto_pesos_97 + 
                             reg_2d.monto_pesos_so +
                             reg_2d.monto_pesos_cv_es_sp  

        INITIALIZE nombre_final, vnombres, vpaterno, vmaterno TO NULL

        SELECT @nombres , 
               @paterno , 
               @materno
        INTO   vnombres , 
               vpaterno ,
               vmaterno
        FROM   afi_mae_afiliado
        WHERE  @n_seguro = reg_2d.nss

        LET nombre_final = vpaterno CLIPPED," ",
                           vmaterno CLIPPED," ",
                           vnombres CLIPPED

        PRINT                                                                 
        PRINT                                                                 
            COLUMN 004,reg_2d.consecutivo          USING "######"       ,
            COLUMN 013,reg_2d.nss                                       ,
            COLUMN 026,nombre_final[1,35] CLIPPED                       ,
            COLUMN 063,reg_2d.acciones_pagadas     USING "#####.######" ,
            COLUMN 078,reg_2d.tipo_prestacion      USING "&"            ,
            COLUMN 084,reg_2d.nombre_siefore                            ,
            COLUMN 093,reg_2d.impt_pagado          USING "######.&&"    ,
            COLUMN 107,reg_2d.monto_pesos_97       USING "######.&&"    ,
            COLUMN 121,reg_2d.monto_pesos_cv_es_sp USING "######.&&"    ,
            COLUMN 135,reg_2d.monto_pesos_so       USING "#######.&&"
            --'\033015'

            IF reg_2d.nombre_siefore = "SB1" THEN
                LET total_nss_sb1            = total_nss_sb1 + 1
                LET acciones_pagadas_sb1     = acciones_pagadas_sb1     +   
                                               reg_2d.acciones_pagadas
                LET impt_pagado_sb1          = impt_pagado_sb1          + 
                                               reg_2d.impt_pagado
                LET monto_pesos_97_sb1       = monto_pesos_97_sb1       + 
                                               reg_2d.monto_pesos_97
                LET monto_pesos_cv_es_sp_sb1 = monto_pesos_cv_es_sp_sb1 + 
                                               reg_2d.monto_pesos_cv_es_sp
                LET monto_pesos_so_sb1       = monto_pesos_so_sb1       + 
                                               reg_2d.monto_pesos_so



                LET sub_cont_sb1 = sub_cont_sb1 + 1
                LET sub_acciones_pagadas_sb1 = sub_acciones_pagadas_sb1 +
                                               reg_2d.acciones_pagadas
                
                LET sub_importe_pagado_sb1   = sub_importe_pagado_sb1      + 
                                               reg_2d.monto_pesos_97       +
                                               reg_2d.monto_pesos_cv_es_sp + 
                                               reg_2d.monto_pesos_so 
                
                LET sub_monto_pesos_97_sb1    = sub_monto_pesos_97_sb1 +
                                                reg_2d.monto_pesos_97
                
                LET sub_monto_pesos_cv_es_sp_sb1 = sub_monto_pesos_cv_es_sp_sb1 + 
                                                   reg_2d.monto_pesos_cv_es_sp
                
                LET sub_monto_pesos_so_sb1   = sub_monto_pesos_so_sb1 + 
                                               reg_2d.monto_pesos_so
                
                LET sub_total_sb1             = sub_total_sb1               + 
                                                reg_2d.monto_pesos_97       +
                                                reg_2d.monto_pesos_cv_es_sp + 
                                                reg_2d.monto_pesos_so
                

            ELSE
             IF reg_2d.nombre_siefore = "SB2" THEN
                LET total_nss_sb2            = total_nss_sb2 + 1
                LET acciones_pagadas_sb2     = acciones_pagadas_sb2     + 
                                               reg_2d.acciones_pagadas
                LET impt_pagado_sb2          = impt_pagado_sb2          + 
                                               reg_2d.impt_pagado
                LET monto_pesos_97_sb2       = monto_pesos_97_sb2       + 
                                               reg_2d.monto_pesos_97
                LET monto_pesos_cv_es_sp_sb2 = monto_pesos_cv_es_sp_sb2 + 
                                               reg_2d.monto_pesos_cv_es_sp
                LET monto_pesos_so_sb2       = monto_pesos_so_sb2       + 
                                               reg_2d.monto_pesos_so


                LET sub_cont_sb2 = sub_cont_sb2 + 1
                LET sub_acciones_pagadas_sb2 = sub_acciones_pagadas_sb2 +
                                               reg_2d.acciones_pagadas
                
                LET sub_importe_pagado_sb2   = sub_importe_pagado_sb2      + 
                                               reg_2d.monto_pesos_97       +
                                               reg_2d.monto_pesos_cv_es_sp + 
                                               reg_2d.monto_pesos_so 
                
                LET sub_monto_pesos_97_sb2    = sub_monto_pesos_97_sb2 +
                                                reg_2d.monto_pesos_97
                
                LET sub_monto_pesos_cv_es_sp_sb2 = sub_monto_pesos_cv_es_sp_sb2 + 
                                                   reg_2d.monto_pesos_cv_es_sp
                
                LET sub_monto_pesos_so_sb2   = sub_monto_pesos_so_sb2 + 
                                               reg_2d.monto_pesos_so
                
                LET sub_total_sb2             = sub_total_sb2               + 
                                                reg_2d.monto_pesos_97       +
                                                reg_2d.monto_pesos_cv_es_sp + 
                                                reg_2d.monto_pesos_so
             ELSE
              IF reg_2d.nombre_siefore = "SB3" THEN
                 LET total_nss_sb3            = total_nss_sb3 + 1
                 LET acciones_pagadas_sb3     = acciones_pagadas_sb3     + 
                                                reg_2d.acciones_pagadas
                 LET impt_pagado_sb3          = impt_pagado_sb3          + 
                                                reg_2d.impt_pagado
                 LET monto_pesos_97_sb3       = monto_pesos_97_sb3       + 
                                                reg_2d.monto_pesos_97
                 LET monto_pesos_cv_es_sp_sb3 = monto_pesos_cv_es_sp_sb3 + 
                                                reg_2d.monto_pesos_cv_es_sp
                 LET monto_pesos_so_sb3       = monto_pesos_so_sb3       + 
                                                reg_2d.monto_pesos_so


                 LET sub_cont_sb3 = sub_cont_sb3 + 1
                 LET sub_acciones_pagadas_sb3 = sub_acciones_pagadas_sb3 +
                                                reg_2d.acciones_pagadas
                
                 LET sub_importe_pagado_sb3   = sub_importe_pagado_sb3      + 
                                                reg_2d.monto_pesos_97       +
                                                reg_2d.monto_pesos_cv_es_sp + 
                                                reg_2d.monto_pesos_so 
                
                 LET sub_monto_pesos_97_sb3    = sub_monto_pesos_97_sb3 +
                                                 reg_2d.monto_pesos_97
                
                 LET sub_monto_pesos_cv_es_sp_sb3 = sub_monto_pesos_cv_es_sp_sb3 + 
                                                    reg_2d.monto_pesos_cv_es_sp
                
                 LET sub_monto_pesos_so_sb3   = sub_monto_pesos_so_sb3 + 
                                                reg_2d.monto_pesos_so
                 
                 LET sub_total_sb3             = sub_total_sb3               + 
                                                 reg_2d.monto_pesos_97       +
                                                 reg_2d.monto_pesos_cv_es_sp + 
                                                 reg_2d.monto_pesos_so
              ELSE
               IF reg_2d.nombre_siefore = "SB4" THEN
                 LET total_nss_sb4            = total_nss_sb4 + 1
                 LET acciones_pagadas_sb4     = acciones_pagadas_sb4     + 
                                                reg_2d.acciones_pagadas
                 LET impt_pagado_sb4          = impt_pagado_sb4          + 
                                                reg_2d.impt_pagado
                 LET monto_pesos_97_sb4       = monto_pesos_97_sb4       + 
                                                reg_2d.monto_pesos_97
                 LET monto_pesos_cv_es_sp_sb4 = monto_pesos_cv_es_sp_sb4 + 
                                                reg_2d.monto_pesos_cv_es_sp
                 LET monto_pesos_so_sb4       = monto_pesos_so_sb4       + 
                                                reg_2d.monto_pesos_so


                 LET sub_cont_sb4 = sub_cont_sb4 + 1
                 LET sub_acciones_pagadas_sb4 = sub_acciones_pagadas_sb4 +
                                                reg_2d.acciones_pagadas
                
                 LET sub_importe_pagado_sb4   = sub_importe_pagado_sb4      + 
                                                reg_2d.monto_pesos_97       +
                                                reg_2d.monto_pesos_cv_es_sp + 
                                                reg_2d.monto_pesos_so 
                
                 LET sub_monto_pesos_97_sb4    = sub_monto_pesos_97_sb4 +
                                                 reg_2d.monto_pesos_97
                
                 LET sub_monto_pesos_cv_es_sp_sb4 = sub_monto_pesos_cv_es_sp_sb4 + 
                                                    reg_2d.monto_pesos_cv_es_sp
                
                 LET sub_monto_pesos_so_sb4   = sub_monto_pesos_so_sb4 + 
                                                reg_2d.monto_pesos_so
                 
                 LET sub_total_sb4             = sub_total_sb4               + 
                                                 reg_2d.monto_pesos_97       +
                                                 reg_2d.monto_pesos_cv_es_sp + 
                                                 reg_2d.monto_pesos_so
               ELSE
                IF reg_2d.nombre_siefore = "SB5" THEN
                 LET total_nss_sb5            = total_nss_sb5 + 1
                 LET acciones_pagadas_sb5     = acciones_pagadas_sb5     + 
                                                reg_2d.acciones_pagadas
                 LET impt_pagado_sb5          = impt_pagado_sb5          + 
                                                reg_2d.impt_pagado
                 LET monto_pesos_97_sb5       = monto_pesos_97_sb5       + 
                                                reg_2d.monto_pesos_97
                 LET monto_pesos_cv_es_sp_sb5 = monto_pesos_cv_es_sp_sb5 + 
                                                reg_2d.monto_pesos_cv_es_sp
                 LET monto_pesos_so_sb5       = monto_pesos_so_sb5       + 
                                                reg_2d.monto_pesos_so


                 LET sub_cont_sb5 = sub_cont_sb5 + 1
                 LET sub_acciones_pagadas_sb5 = sub_acciones_pagadas_sb5 +
                                                reg_2d.acciones_pagadas
                
                 LET sub_importe_pagado_sb5   = sub_importe_pagado_sb5      + 
                                                reg_2d.monto_pesos_97       +
                                                reg_2d.monto_pesos_cv_es_sp + 
                                                reg_2d.monto_pesos_so 
                
                 LET sub_monto_pesos_97_sb5    = sub_monto_pesos_97_sb5 +
                                                 reg_2d.monto_pesos_97
                
                 LET sub_monto_pesos_cv_es_sp_sb5 = sub_monto_pesos_cv_es_sp_sb5 + 
                                                    reg_2d.monto_pesos_cv_es_sp
                
                 LET sub_monto_pesos_so_sb5   = sub_monto_pesos_so_sb5 + 
                                                reg_2d.monto_pesos_so
                 
                 LET sub_total_sb5             = sub_total_sb5               + 
                                                 reg_2d.monto_pesos_97       +
                                                 reg_2d.monto_pesos_cv_es_sp + 
                                                 reg_2d.monto_pesos_so

                END IF             	    
               END IF             	    
              END IF             	    
             END IF             	    
            END IF             	    
                                                                               
            {IF lineno > 45 THEN
                SKIP TO TOP OF PAGE
            END IF}

    AFTER GROUP OF reg_2d.tipo_prestacion 
    SKIP 2 LINES

        CASE reg_2d.tipo_prestacion
            WHEN 6
              LET desc_tipo_prestacion = '(DESEMPLEO)'
              
              SELECT COUNT(UNIQUE nss)
              INTO   cont_nss_unicos
              FROM   dis_cuenta 
              WHERE  folio           = reg_1.folio_oper_16
              AND    tipo_movimiento = 875
              
            WHEN 7
              LET desc_tipo_prestacion = '(MATRIMONIO)'

              SELECT COUNT(UNIQUE nss)
              INTO   cont_nss_unicos
              FROM   dis_cuenta 
              WHERE  folio           = reg_1.folio_oper_16
              AND    tipo_movimiento = 870

        END CASE

        PRINT COLUMN 001,"SUBTOTAL DE REGISTROS ", 
                         desc_tipo_prestacion CLIPPED,
                         ":  ", 
                         sub_cont_sb1                 USING "&&&",
              COLUMN 062,sub_acciones_pagadas_sb1     USING "######.######" ,
              COLUMN 084,"SB1"                                         ,
              COLUMN 093,sub_importe_pagado_sb1       USING "######.&&",
              COLUMN 107,sub_monto_pesos_97_sb1       USING "######.&&", 
              COLUMN 121,sub_monto_pesos_cv_es_sp_sb1 USING "######.&&", 
              COLUMN 135,sub_monto_pesos_so_sb1       USING "#######.&&"
          --  '\033015'



        PRINT COLUMN 001,"SUBTOTAL DE REGISTROS ", 
                         desc_tipo_prestacion CLIPPED,
                         ":  ", 
                         sub_cont_sb2                 USING "&&&",
              COLUMN 062,sub_acciones_pagadas_sb2     USING "######.######" ,
              COLUMN 084,"SB2" ,
              COLUMN 093,sub_importe_pagado_sb2       USING "######.&&",
              COLUMN 107,sub_monto_pesos_97_sb2       USING "######.&&", 
              COLUMN 121,sub_monto_pesos_cv_es_sp_sb2 USING "######.&&", 
              COLUMN 135,sub_monto_pesos_so_sb2       USING "#######.&&"
          --  '\033015'

        PRINT COLUMN 001,"SUBTOTAL DE REGISTROS ", 
                         desc_tipo_prestacion CLIPPED,
                         ":  ", 
                         sub_cont_sb3                 USING "&&&",
              COLUMN 062,sub_acciones_pagadas_sb3     USING "######.######" ,
              COLUMN 084,"SB3" ,
              COLUMN 093,sub_importe_pagado_sb3       USING "######.&&",
              COLUMN 107,sub_monto_pesos_97_sb3       USING "######.&&", 
              COLUMN 121,sub_monto_pesos_cv_es_sp_sb3 USING "######.&&", 
              COLUMN 135,sub_monto_pesos_so_sb3       USING "#######.&&"
            --'\033015'

        PRINT COLUMN 001,"SUBTOTAL DE REGISTROS ", 
                         desc_tipo_prestacion CLIPPED,
                         ":  ", 
                         sub_cont_sb4                 USING "&&&",
              COLUMN 062,sub_acciones_pagadas_sb4     USING "######.######" ,
              COLUMN 084,"SB4" ,
              COLUMN 093,sub_importe_pagado_sb4       USING "######.&&",
              COLUMN 107,sub_monto_pesos_97_sb4       USING "######.&&", 
              COLUMN 121,sub_monto_pesos_cv_es_sp_sb4 USING "######.&&", 
              COLUMN 135,sub_monto_pesos_so_sb4       USING "#######.&&"
            --'\033015'

        PRINT COLUMN 001,"SUBTOTAL DE REGISTROS ", 
                         desc_tipo_prestacion CLIPPED,
                         ":  ", 
                         sub_cont_sb5                 USING "&&&",
              COLUMN 062,sub_acciones_pagadas_sb5     USING "######.######" ,
              COLUMN 084,"SB5" ,
              COLUMN 093,sub_importe_pagado_sb5       USING "######.&&",
              COLUMN 107,sub_monto_pesos_97_sb5       USING "######.&&", 
              COLUMN 121,sub_monto_pesos_cv_es_sp_sb5 USING "######.&&", 
              COLUMN 135,sub_monto_pesos_so_sb5       USING "#######.&&"
        --    '\033015'


        PRINT COLUMN 001,"TOTAL DE NSS UNICOS : ", cont_nss_unicos
         --   '\033015'


        SKIP 3 LINES
 
    BEFORE GROUP OF reg_2d.tipo_prestacion 
       CASE reg_2d.tipo_prestacion
           WHEN 6
             LET desc_tipo_prestacion = 'DESEMPLEO'
           WHEN 7
             LET desc_tipo_prestacion = 'MATRIMONIO'
       END CASE

       PRINT COLUMN 001, "TIPO PRESTACION ", desc_tipo_prestacion CLIPPED
           -- '\033015'

       LET sub_cont_sb1                 = 0
       LET sub_acciones_pagadas_sb1     = 0
       LET sub_importe_pagado_sb1       = 0
       LET sub_monto_pesos_97_sb1       = 0
       LET sub_monto_pesos_cv_es_sp_sb1 = 0
       LET sub_monto_pesos_so_sb1       = 0 
       LET sub_total_sb1                = 0 


       LET sub_cont_sb2                 = 0
       LET sub_acciones_pagadas_sb2     = 0
       LET sub_importe_pagado_sb2       = 0
       LET sub_monto_pesos_97_sb2       = 0
       LET sub_monto_pesos_cv_es_sp_sb2 = 0
       LET sub_monto_pesos_so_sb2       = 0 
       LET sub_total_sb2                = 0 


       LET sub_cont_sb3                 = 0
       LET sub_acciones_pagadas_sb3     = 0
       LET sub_importe_pagado_sb3       = 0
       LET sub_monto_pesos_97_sb3       = 0
       LET sub_monto_pesos_cv_es_sp_sb3 = 0
       LET sub_monto_pesos_so_sb3       = 0 
       LET sub_total_sb3                = 0 
   
       LET sub_cont_sb4                 = 0
       LET sub_acciones_pagadas_sb4     = 0
       LET sub_importe_pagado_sb4       = 0
       LET sub_monto_pesos_97_sb4       = 0
       LET sub_monto_pesos_cv_es_sp_sb4 = 0
       LET sub_monto_pesos_so_sb4       = 0 
       LET sub_total_sb4                = 0 

       LET sub_cont_sb5                 = 0
       LET sub_acciones_pagadas_sb5     = 0
       LET sub_importe_pagado_sb5       = 0
       LET sub_monto_pesos_97_sb5       = 0
       LET sub_monto_pesos_cv_es_sp_sb5 = 0
       LET sub_monto_pesos_so_sb5       = 0 
       LET sub_total_sb5                = 0 

    ON LAST ROW
        SKIP 3 LINES                                                          

        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L3,
                       "\302",L4,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
            --'\033015'
        PRINT                                                                 
                                                                             
            COLUMN 001,"|          |",
            COLUMN 015,total_nss_sb1 USING "#######","  |",
            COLUMN 035,"T O T A L E S : ",
            COLUMN 062,"|",
            COLUMN 062,acciones_pagadas_sb1 USING "######.######",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 084,"SB1" ,
            COLUMN 091,"|",
            COLUMN 093,impt_pagado_sb1 USING "#######.&&",
            COLUMN 105,"|",
            COLUMN 107,monto_pesos_97_sb1 USING "######.&&",
            COLUMN 119,"|",
            COLUMN 120,monto_pesos_cv_es_sp_sb1 USING "#######.&&",
            COLUMN 133,"|",
            COLUMN 134,monto_pesos_so_sb1 USING "#######.&&",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L3,
                     "\301",L4,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'

        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L3,
                       "\302",L4,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
            --'\033015'
        PRINT                                                                 
            COLUMN 001,"|          |",
            COLUMN 015,total_nss_sb2            USING "#######","  |",
            COLUMN 035,"T O T A L E S : ",
            COLUMN 062,"|",
            COLUMN 062,acciones_pagadas_sb2     USING "######.######",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 084,"SB2" ,
            COLUMN 091,"|",
            COLUMN 093,impt_pagado_sb2          USING "#######.&&",
            COLUMN 105,"|",
            COLUMN 107,monto_pesos_97_sb2       USING "######.&&",
            COLUMN 119,"|",
            COLUMN 120,monto_pesos_cv_es_sp_sb2 USING "#######.&&",
            COLUMN 133,"|",
            COLUMN 134,monto_pesos_so_sb2       USING "#######.&&",
            COLUMN 149,"|"
           -- '\033015'

        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L3,
                     "\301",L4,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'
        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L3,
                       "\302",L4,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
            --'\033015'
        PRINT                                                                 
            COLUMN 001,"|          |",
            COLUMN 015,total_nss_sb3            USING "#######","  |",
            COLUMN 035,"T O T A L E S : ",
            COLUMN 062,"|",
            COLUMN 062,acciones_pagadas_sb3     USING "######.######",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 084,"SB3" ,
            COLUMN 091,"|",
            COLUMN 093,impt_pagado_sb3          USING "#######.&&",
            COLUMN 105,"|",
            COLUMN 107,monto_pesos_97_sb3       USING "######.&&",
            COLUMN 119,"|",
            COLUMN 120,monto_pesos_cv_es_sp_sb3 USING "#######.&&",
            COLUMN 133,"|",
            COLUMN 134,monto_pesos_so_sb3       USING "#######.&&",
            COLUMN 149,"|"
           -- '\033015'

        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L3,
                     "\301",L4,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'

        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L3,
                       "\302",L4,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
            --'\033015'
        PRINT                                                                 
            COLUMN 001,"|          |",
            COLUMN 015,total_nss_sb4            USING "#######","  |",
            COLUMN 035,"T O T A L E S : ",
            COLUMN 062,"|",
            COLUMN 062,acciones_pagadas_sb4     USING "######.######",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 084,"SB4" ,
            COLUMN 091,"|",
            COLUMN 093,impt_pagado_sb4          USING "#######.&&",
            COLUMN 105,"|",
            COLUMN 107,monto_pesos_97_sb4       USING "######.&&",
            COLUMN 119,"|",
            COLUMN 120,monto_pesos_cv_es_sp_sb4 USING "#######.&&",
            COLUMN 133,"|",
            COLUMN 134,monto_pesos_so_sb4       USING "#######.&&",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L3,
                     "\301",L4,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'
        PRINT
            COLUMN 001,"\332",L10,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L7,
                       "\302",L10,L3,
                       "\302",L4,
                       "\302",L9,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L3,
                       "\302",L10,L5,
                       "\277"
            --'\033015'
        PRINT                                                                 
            COLUMN 001,"|          |",
            COLUMN 015,total_nss_sb5            USING "#######","  |",
            COLUMN 035,"T O T A L E S : ",
            COLUMN 062,"|",
            COLUMN 062,acciones_pagadas_sb5     USING "######.######",
            COLUMN 075,"|",
            COLUMN 081,"|",
            COLUMN 084,"SB5" ,
            COLUMN 091,"|",
            COLUMN 093,impt_pagado_sb5          USING "#######.&&",
            COLUMN 105,"|",
            COLUMN 107,monto_pesos_97_sb5       USING "######.&&",
            COLUMN 119,"|",
            COLUMN 120,monto_pesos_cv_es_sp_sb5 USING "#######.&&",
            COLUMN 133,"|",
            COLUMN 134,monto_pesos_so_sb5       USING "#######.&&",
            COLUMN 149,"|"
            --'\033015'

        PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L7,
                     "\301",L10,L3,
                     "\301",L4,
                     "\301",L9,
                     "\301",L4,L9,
                     "\301",L10,L3,
                     "\301",L10,L3,
                     "\301",L5,L10,
                     "\331"
            --'\033015'

        SELECT COUNT(UNIQUE nss)
        INTO   cont_tot_nss_unicos
        FROM   ret_parcial
        WHERE  folio           = reg_1.folio_oper_16
        AND    tipo_prestacion IN (6,7)
        AND    diag_cuenta_ind = "400"
        
        IF cont_tot_nss_unicos > 0 then
            PRINT
            COLUMN 001,"\332",L10,
                       L10,L2,
                       L10,L10,L10,L8,
                       L10,L4,
                       L5,
                       L10,
                       L10,L4,
                       L10,L4,
                       L10,L4,
                       L10,L6,
                       "\277"
           -- '\033015'

            PRINT 
            COLUMN 001,"|",
            COLUMN 035,"TOTAL DE NSS UNICOS : ", cont_tot_nss_unicos,
            COLUMN 149,"|"            
            --'\033015'

            PRINT
            COLUMN 1,"\300",L10,
                       L10,L2,
                       L10,L10,L10,L8,
                       L10,L4,
                       L5,
                       L10,
                       L10,L4,
                       L10,L4,
                       L10,L4,
                       L10,L6,
                     "\331"
            --'\033015'
        END IF 
      PAGE TRAILER

      PRINT COLUMN 001,"REPORTE : ",HOY USING "DDMMYYYY",".803"
            --'\033015'
                  
END REPORT 
