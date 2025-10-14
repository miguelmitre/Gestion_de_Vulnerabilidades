################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETC900  => GENERA OPERACIÓN 33 DE RETIROS-ISSSTE PARA PARCIALES     #
#Fecha creacion    => 02 DE OCTUBRE 2006                                       #
#By                => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
#Modifico          => XAVIER TORRES RIOS                                       #
#Fecha             => 15 de Noviembre 2007                                     #
#                     Se movieron los importes de la siefore1 , siefore2 y     #
#                     Retiro despues del UPDATE de ret_sol_issste_par ya que   #
#                     no estaban saliendo en el archivo, salian en cero.       #
#                     Si el nss_imss en la posicion 1 tiene una "I" se le mueve#
#                     espacios a todo el campo del reporte                     #
#                  => XAVIER TORRES RIOS                                       #
#                     18 DE ABRIL 2008                                         #
#                     Adaptacion para soporte a multisiefores                  #
#                  => JAVIER GONZALEZ JERONIMO                                 #
#                     18 DE AGOSTO 2008                                        #
#                     Modificaciones para la nueva version de layout           #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        g_seg_modulo          RECORD LIKE seg_modulo.*

    DEFINE reg_3              RECORD LIKE ret_sol_issste_par.*
       
    DEFINE reg_2 RECORD #reg_2
        confirmado            LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        borra_lineas          CHAR(100) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        c11_id_aportante      CHAR(011) ,
        ch                    CHAR(110) ,
        elimina               CHAR(100) ,
        enter                 CHAR(001) ,
        folio_sua             CHAR(006) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        G_LISTA               CHAR(300) ,
        usuario               CHAR(012) ,
        v_saldo_dia           CHAR(150) ,
        v_provisiona          CHAR(150)

    DEFINE #glo #decimal
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6)

    DEFINE #glo #smallint
        gs_num_siefores       ,-- Indica el numero de siefores que se usan actualmente
        gs_viv                ,-- Indica en que posicion se encuentra almacenada
                               -- la info de precio de vivienda
        f_subcuenta           ,
        f_siefore             ,
        s_codigo_afore        ,
        s_tipo_movimiento     ,
        si_provisiono         SMALLINT

    DEFINE #glo #integer
        cont_1                ,
        cont_fin              ,
        cont_reg_av           ,
        ultimo_folio          INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
	CALL STARTLOG ("RETC900.log")

    CALL init() #i
    OPEN WINDOW retc900 AT 4,4 WITH FORM "RETC9001" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC900      GENERA ARCHIVO RETIROS PARCIALES ISSSTE (Op. 33)                 " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL valida_ejecucion() #ve

    SELECT "OK"
    FROM   ret_sol_issste_par
    WHERE  estado_solicitud = reg_2.confirmado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT" NO EXISTEN REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF
    
    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    SELECT COUNT(*)
    INTO   cont_fin
    FROM   ret_sol_issste_par A
    WHERE  A.estado_solicitud   = reg_2.confirmado

    CALL primer_paso()  #pp -----PROVISIONA CUENTAS
    CALL segundo_paso() #sp -----GENERA EL ARCHIVO OP 27

    DISPLAY "TOTAL REGISTROS A PROCESAR          : ",cont_fin         AT 08,19
    DISPLAY "TOTAL REGISTROS A ENVIAR            : ",cont_reg_av      AT 09,19

    DISPLAY "EL LOTE HA SIDO GENERADO EN LA RUTA : "  AT 12,19
    DISPLAY G_LISTA CLIPPED AT 13,19
    DISPLAY "CON EL NOMBRE : ",c12_nombre_plano AT 15,19

    DISPLAY " FOLIO NUMERO : ",ultimo_folio  AT 18,01
    PROMPT  " PROCESO FINALIZADO... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc900
END MAIN


FUNCTION init()
#i-------------

    --Obtenemos el numero de siefores actual en el sistema--
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore  > 0
    AND    codigo_siefore <> 11
    
    LET gs_viv = 20


    LET HOY               = TODAY
    LET cont_1            = 0
    LET c11_id_aportante  = "RETIRO"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT MAX(A.folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio A

    INSERT INTO glo_folio VALUES (ultimo_folio)
    
    SELECT A.estado_solicitud
    INTO   reg_2.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DETOP33"
    LET c12_nombre_plano = HOY USING"YYYYMMDD",".33P"

    SELECT  COUNT(*)
    INTO    cont_1
    FROM    ret_sol_issste_par A
    WHERE   A.estado_solicitud = reg_2.confirmado

    --- PROVISION ---
    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona

    ----- SALDO AL DIA -----
    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM v_saldo_dia

    WHENEVER ERROR CONTINUE
        LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,HOY USING"YYYYMMDD",".33P"
        RUN elimina
    WHENEVER ERROR STOP
END FUNCTION


FUNCTION primer_paso() 
#pp-------------------

    DEFINE reg_5 RECORD #loc #reg_5
        nss_imss             LIKE ret_sol_issste_par.nss_imss         ,
        consecutivo          LIKE ret_sol_issste_par.consecutivo      ,
        tipo_retiro          LIKE ret_sol_issste_par.tipo_retiro      
    END RECORD     

    DEFINE arr_siefore ARRAY [20] OF RECORD
                                        activo            SMALLINT,
                                        acciones_ret      DECIMAL(16,6)
                                    END RECORD

    DEFINE #loc #decimal
        pes_impt_ret         DECIMAL(16,2)

    DEFINE #loc #smallint
        ls_siefore          , #-- contador para los ciclos for
        v_subcuenta         ,
        v_grupo             SMALLINT

-- --------------------------------------

    SELECT  "OK"
    FROM    ret_sol_issste_par A
    WHERE   A.estado_solicitud  = reg_2.confirmado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT "NO EXISTEN REGISTROS PROCESO CANCELADO...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF

    DECLARE cur_1 CURSOR FOR
    SELECT  A.nss_imss         ,
            A.consecutivo      ,
            A.tipo_retiro      
    FROM    ret_sol_issste_par A
    WHERE   A.estado_solicitud = reg_2.confirmado

    
    FOREACH cur_1 INTO reg_5.*
        
        #-- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET arr_siefore[ls_siefore].activo        = FALSE
            LET arr_siefore[ls_siefore].acciones_ret  = 0
        END FOR

        LET pes_impt_ret = 0
        LET v_subcuenta  = 0
        LET v_grupo      = 0
        LET f_subcuenta  = 0
        LET f_siefore    = 0
        LET f_monto_acc  = 0
        LET f_monto_pes  = 0


        CASE reg_3.tipo_inversion
            WHEN 1
                LET v_subcuenta = 13
            WHEN 7
                LET v_subcuenta = 19
        END CASE
        
        DECLARE c_saldo CURSOR FOR eje_saldo_dia 
            
        FOREACH c_saldo USING reg_5.nss_imss ,
                              v_subcuenta    , -- Trae el saldo al dia segun la subcuenta
                              v_grupo        ,
                              HOY          
                        INTO  f_subcuenta ,
                              f_siefore   ,
                              f_monto_acc ,
                              f_monto_pes


            IF f_monto_pes IS NULL OR f_monto_pes = " " OR f_monto_pes < 0 THEN
                LET f_monto_pes = 0
            END IF
            
            IF f_monto_acc IS NULL OR f_monto_acc = " " OR f_monto_acc < 0 THEN
                LET f_monto_acc = 0
            END IF
            
            IF f_siefore > 0 AND f_siefore <> 11 THEN
                #-- Marcamos como activo el registro de la siefore actual
                LET arr_siefore[f_siefore].activo       = TRUE
                LET arr_siefore[f_siefore].acciones_ret = f_monto_acc
            ELSE
                LET f_monto_acc = 0
            END IF

            ------- PROVISION  ------------------------
            
            IF f_monto_acc > 0 THEN
            
                LET si_provisiono = 0
                LET folio_sua     = ""
                LET f_monto_acc   = -f_monto_acc
                LET f_monto_pes   = -f_monto_pes
            
                SELECT movimiento 
                INTO   s_tipo_movimiento
                FROM   tab_retiro_issste
                WHERE  tipo_retiro = reg_5.tipo_retiro
            
                DECLARE cur_3 CURSOR FOR eje_provisiona
                OPEN cur_3 USING  ultimo_folio        ,--folio             
                                  folio_sua           ,--folio_sua         
                                  reg_5.nss_imss      ,--nss               
                                  f_subcuenta         ,--subcuenta         
                                  s_tipo_movimiento   ,--tipo_movimiento   
                                  reg_5.consecutivo   ,--consecutivo_lote  
                                  f_siefore           ,--siefore           
                                  f_monto_acc         ,--monto_en_acciones 
                                  f_monto_pes         ,--monto_en_pesos    
                                  c11_id_aportante    ,--id_aportante      
                                  HOY                  --fecha proceso     
            
                FETCH cur_3 INTO si_provisiono
                CLOSE cur_3
                
                IF si_provisiono < 0 THEN
                    PROMPT "EL NSS :",reg_5.nss_imss CLIPPED," NO PROVISIONO LA SUBCTA ",f_subcuenta FOR enter
                ELSE
                    INSERT INTO ret_monto_sie_issste
                    VALUES (ultimo_folio             ,--folio
                            reg_5.nss_imss           ,--nss
                            reg_5.consecutivo        ,--consecutivo
                            f_siefore                ,--siefore
                            f_subcuenta              ,--subcuenta
                            -f_monto_acc             ,--saldo_acciones
                            -f_monto_pes             ,--saldo_pesos
                            0                         --interes_pesos
                            )
                END IF
            END IF
        END FOREACH

        IF f_monto_pes >= 0 THEN
            LET f_monto_pes  = -f_monto_pes   
            LET pes_impt_ret = pes_impt_ret + f_monto_pes
        END IF
        
        UPDATE ret_sol_issste_par
        SET    estado_solicitud   = reg_2.enviado   ,
               folio              = ultimo_folio    ,
               acciones_ret_sief1 = arr_siefore[1].acciones_ret ,
               acciones_ret_sief2 = arr_siefore[2].acciones_ret ,
               importe_retiro     = pes_impt_ret    ,
               fecha_envio        = HOY
        WHERE  nss_imss           = reg_5.nss_imss
        AND    consecutivo        = reg_5.consecutivo

    END FOREACH

END FUNCTION


FUNCTION segundo_paso()

    LET G_LISTA_1 = G_LISTA CLIPPED,"/",c7_nombre_plano

    START REPORT listado_det_op_33 TO G_LISTA_1
        
    DECLARE cur_03 CURSOR FOR
    SELECT  A.*
    FROM    ret_sol_issste_par A
    WHERE   A.estado_solicitud  = reg_2.enviado 
    AND     A.folio             = ultimo_folio  
    AND     A.fecha_envio       = HOY           

    LET cont_reg_av = 0    

    LET ch = "chmod 777 ", G_LISTA_1
    RUN ch
    
    FOREACH cur_03 INTO reg_3.*

        OUTPUT TO REPORT listado_det_op_33(reg_3.*) #ldav12
        LET cont_reg_av = cont_reg_av + 1
          
    END FOREACH    
    
    FINISH REPORT listado_det_op_33    

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano
    #ff
    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    run borra_lineas

    LET ch = "cp ",G_LISTA_2 CLIPPED," ",G_LISTA_1 CLIPPED
    RUN ch

    INSERT INTO ret_ctr_envio
        VALUES (ultimo_folio              ,#folio
                "OP33"                    ,#tipo_operacion
                "CZA33-I DET33-I SUM33-I" ,#estructura
                reg_2.enviado             ,#status
                0                         ,#orden_de_envio
                HOY                        #fecha_envio
               )

    LET ch = "chmod 777 ", G_LISTA_1
    run ch
    
    LET ch = "chmod 777 ", G_LISTA_2
    run ch


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


FUNCTION f_datos_afiliado(p_nss)

    DEFINE p_nss LIKE ret_sol_issste_par.nss_imss

    DEFINE lr_datos RECORD #loc #lr_datos
        calle                 CHAR(40),
        numero                CHAR(10),
        depto                 CHAR(10),
        colonia               CHAR(60),
        codpos                CHAR(05),
        delega                INTEGER,
        deleg_desc            CHAR(40),
        estado                SMALLINT,
        estad_desc            CHAR(40),
        tipo_solicitud        SMALLINT,
        tipo_trabajador       CHAR(02)
    END RECORD

    SELECT d.calle   ,
           d.numero  ,
           d.depto   ,
           d.colonia ,
           d.delega  ,
           d.codpos  ,
           d.estado  ,
           a.tipo_solicitud
    INTO   lr_datos.calle  ,
           lr_datos.numero ,
           lr_datos.depto  ,
           lr_datos.colonia,
           lr_datos.delega ,
           lr_datos.codpos ,
           lr_datos.estado ,
           lr_datos.tipo_solicitud
    FROM   afi_domicilio       d  ,
           afi_mae_afiliado    a  ,
           tab_zona_geo        tz ,
           tabsalario_minimo2  tm
    WHERE  a.n_seguro       = p_nss
    AND    a.n_seguro       = d.nss
    AND    a.n_folio        = d.n_folio
    AND    a.tipo_solicitud = d.tipo_solicitud
    AND    d.marca_envio    = "X"
    AND    d.dom_cod        = 1
    AND    tz.estad_cod     = d.estado
    AND    tz.deleg_cod     = d.delega
    AND    tm.zona_cod      = tz.zona_cod
    AND    tm.fecha_hasta_sm IS NULL

    IF lr_datos.tipo_solicitud = 8 OR lr_datos.tipo_solicitud = 12 THEN
        LET lr_datos.tipo_trabajador = "01"
    ELSE
        LET lr_datos.tipo_trabajador = "02"
    END IF 
    
    SELECT deleg_desc
    INTO   lr_datos.deleg_desc
    FROM   tab_delegacion
    WHERE  estad_cod = lr_datos.estado
    AND    deleg_cod = lr_datos.delega
    
    SELECT estad_desc
    INTO   lr_datos.estad_desc
    FROM   tab_estado
    WHERE  estad_cod = lr_datos.estado
    
    RETURN lr_datos.*

END FUNCTION



REPORT listado_det_op_33(reg_5)
#ldav33------------------------------

    DEFINE #loc #reg_5 
        reg_5                 RECORD LIKE ret_sol_issste_par.*

    DEFINE reg_6 RECORD #glo #reg_6
        calle                 CHAR(40),
        numero                CHAR(10),
        depto                 CHAR(10),
        colonia               CHAR(60),
        codpos                CHAR(05),
        delega                INTEGER,
        deleg_desc            CHAR(40),
        estado                SMALLINT,
        estad_desc            CHAR(40),
        tipo_solicitud        SMALLINT,
        tipo_trabajador       CHAR(02)
    END RECORD

    DEFINE lr_saldos_siefore RECORD #loc #lr_saldos_siefore
        siefore         SMALLINT                                ,
        saldo_acciones  LIKE ret_monto_sie_issste.saldo_acciones
    END RECORD
    
    DEFINE la_monto_sie ARRAY[20] OF DECIMAL(16,6)
    
    DEFINE c14_monto_sie ARRAY[20] OF CHAR(14)

    DEFINE #loc #smallint
        ls_cont               ,
        ls_sie                ,
        cont_2                SMALLINT


    DEFINE #loc #char
        c15_sie               CHAR(15) ,
        c19_importe_retiro    CHAR(19) ,
        c18_importe_retiro    CHAR(18)


    OUTPUT
        PAGE LENGTH    1000
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER           
            
            LET cont_2 = 0
            
            PRINT
                COLUMN 001,"01"                            ,
                COLUMN 003,"04"                            ,
                COLUMN 005,"33"                            ,
                COLUMN 007,"01"                            ,
                COLUMN 009,s_codigo_afore USING"&&&"       ,
                COLUMN 012,"03"                            ,
                COLUMN 014,"001"                           ,
                COLUMN 017,HOY USING"YYYYMMDD"             ,
                COLUMN 025,1175 SPACES

        ON EVERY ROW
            LET cont_2 = cont_2 + 1

            CALL f_datos_afiliado(reg_5.nss_imss) RETURNING reg_6.*
        
            IF reg_5.fecha_baja IS NULL THEN
                LET reg_5.fecha_baja = "01010001"
            END IF

            IF reg_5.fecha_solicitud IS NULL THEN
                LET reg_5.fecha_solicitud = "01010001"
            END IF

            IF reg_5.fecha_valoriza IS NULL THEN
                LET reg_5.fecha_valoriza = "01010001"
            END IF

            IF reg_5.fecha_entrega_rec IS NULL THEN
                LET reg_5.fecha_entrega_rec = "01010001"
            END IF

            ---- OBTENEMOS LOS VALORES PARA CADA SIEFORE ----
            FOR ls_cont = 1 TO gs_num_siefores 
                LET la_monto_sie[ls_cont] = 0
            END FOR
           
            DECLARE cur_sie CURSOR FOR
                SELECT siefore       , 
                       saldo_acciones
                FROM   ret_monto_sie_issste
                WHERE  nss         = reg_5.nss_imss
                AND    folio       = reg_5.folio
                AND    consecutivo = reg_5.consecutivo
            
            FOREACH cur_sie INTO lr_saldos_siefore.*
                LET ls_sie = lr_saldos_siefore.siefore
                LET la_monto_sie[ls_sie] = lr_saldos_siefore.saldo_acciones
            END FOREACH

            FOR ls_cont = 1 TO gs_num_siefores 
                
                LET c14_monto_sie[ls_cont] = "00000000000000"

                IF reg_5.tipo_inversion = 1 OR reg_5.tipo_inversion = 8 THEN
                    LET c15_sie                = la_monto_sie[ls_cont] USING "&&&&&&&&.&&&&&&"
                    LET c14_monto_sie[ls_cont] = c15_sie[01,08],
                                                 c15_sie[10,15]
                END IF
                
                LET c15_sie = " "
                
            END FOR

            -- Se envia el importe de retiro
            IF reg_5.tipo_inversion = 7 OR reg_5.tipo_inversion = 8 THEN
                IF reg_5.importe_retiro IS NULL THEN
                    LET reg_5.importe_retiro = 0
                END IF
            
                LET c19_importe_retiro = reg_5.importe_retiro USING "&&&&&&&&&&&&.&&&&&&"
                LET c18_importe_retiro = c19_importe_retiro[01,12],
                                         c19_importe_retiro[14,19]
            ELSE
                LET c18_importe_retiro = "000000000000000000"
            END IF 

            IF reg_5.nss_imss[1,1] = "I" THEN
                LET reg_5.nss_imss = "           "
            END IF
        
            PRINT
                COLUMN 001,"03"                                  ,#tipo_registro
                COLUMN 003,"04"                                  ,#ident_servicio
                COLUMN 005,"33"                                  ,#ident_operacion
                COLUMN 007,s_codigo_afore        USING"&&&"      ,
                COLUMN 010,reg_5.nss_imss                        ,
                COLUMN 021,reg_5.nss_issste                      ,
                COLUMN 032,reg_5.curp                            ,
                COLUMN 050,reg_5.nombres                         ,
                COLUMN 090,reg_5.paterno                         ,
                COLUMN 130,reg_5.materno                         ,
                COLUMN 170,reg_5.rfc                             ,
                COLUMN 183,reg_5.tipo_retiro     USING"&&"       ,
                COLUMN 185,reg_5.tipo_beneficio                  ,
                COLUMN 188,reg_5.fecha_baja      USING "YYYYMMDD",
                COLUMN 196,reg_5.folio_dictamen                  ,
                COLUMN 206,reg_5.fecha_solicitud USING "YYYYMMDD",
                COLUMN 214,reg_5.tipo_inversion  USING"&&"       ,
                COLUMN 216,reg_5.fecha_valoriza  USING "YYYYMMDD",
                COLUMN 224,c14_monto_sie[1]                      ,
                COLUMN 238,c14_monto_sie[2]                      ,
                COLUMN 252,c14_monto_sie[3]                      ,
                COLUMN 266,c14_monto_sie[4]                      ,
                COLUMN 280,c14_monto_sie[5]                      ,
                COLUMN 308,c18_importe_retiro                    ,
                COLUMN 347,reg_6.tipo_trabajador                 ,
                COLUMN 359,reg_6.calle                           ,
                COLUMN 424,reg_6.numero                          ,
                COLUMN 439,reg_6.depto                           ,
                COLUMN 454,reg_6.colonia                         ,
                COLUMN 519,reg_6.deleg_desc                      ,
                COLUMN 584,reg_6.codpos                          ,
                COLUMN 589,reg_6.estad_desc                      ,
                COLUMN 654,546 SPACES
               
        ON LAST ROW
            PRINT
                COLUMN 001,"09"                          ,
                COLUMN 003,"04"                          ,
                COLUMN 005,"01"                          ,
                COLUMN 007,s_codigo_afore USING"&&&"     ,
                COLUMN 010,"03"                          ,
                COLUMN 012,"001"                         ,
                COLUMN 015,HOY USING "YYYYMMDD"          ,
                COLUMN 023,cont_2 USING"&&&&&&"          ,
                COLUMN 029,1171 SPACES
END REPORT



