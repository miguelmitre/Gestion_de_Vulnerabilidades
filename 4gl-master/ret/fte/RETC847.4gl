################################################################################
#Proyecto          => SISTEMA DE AFORES.( SAFRE )                              #
#Owner             => E. F. P.                                                 #
#Programa RETC847  => CONSULTA DE SOLICITUDES CONFIRMADAS RET "J"              #
#Por               => J.GABRIEL ADOLFO ESCOBEDO ZAMORA                         #
#Fecha             => 31 DE OCTUBRE DE 2007                                    #
#Actualiza         => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiza   => 22 DE MAYO DE 2008                                       #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE #glo #date
        v_fecha_captura       ,
        v_hoy                 ,
        v_fecha_valor         DATE

    DEFINE reg_retiro RECORD #glo #reg_retiro
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        fecha_ini_pen         LIKE ret_solicitud_tx.fecha_ini_pen   ,
        periodo_pago          CHAR(6)                               ,
        estado                CHAR(12)                              ,
        grupo                 LIKE ret_solicitud_tx.grupo
    END RECORD

    DEFINE reg_retiro_des ARRAY[100] OF RECORD #glo #reg_retiro_des
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        fecha_ini_pen         LIKE ret_solicitud_tx.fecha_ini_pen   ,
        periodo_pago          CHAR(6)                               ,
        fecha_traspaso        DATE                                  ,
        estado                CHAR(12)                              ,
        grupo                 LIKE ret_solicitud_tx.grupo
    END RECORD

    DEFINE reg_saldo ARRAY[50] OF RECORD #glo #reg_saldo
        sie_saldo             SMALLINT      ,
        sub_saldo             SMALLINT      ,
        acc_saldo             DECIMAL(16,6) ,
        pes_saldo             DECIMAL(16,6)
    END RECORD

    DEFINE reg_movto ARRAY[200] OF RECORD #glo #reg_movto
        sie_detalle           SMALLINT     ,
        sub_detalle           SMALLINT     ,
        tm_detalle            SMALLINT     ,
        fc_detalle            DATE         ,
        acc_detalle           DECIMAL(16,6),
        pes_detalle           DECIMAL(16,6)
    END RECORD

    DEFINE #glo #integer
        v_solicitudes         ,
        v_saldos              ,
        v_movimientos         ,
        v_siefores            INTEGER

    DEFINE #glo #smallint
        sw_1                  ,
        v_cancela             ,
        v_datos               SMALLINT


    DEFINE #glo #char
        enter                 CHAR(001) ,
        v_condicion           CHAR(100) ,
        v_sql                 CHAR(1000)
END GLOBALS


MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
{
    DEFER INTERRUPT
    OPTIONS
        PROMPT LINE LAST
}
    CALL STARTLOG ("RETC847.log")
    CALL init()

    LET sw_1 = 1

    WHILE sw_1

        CALL fn_principal()

    END WHILE

END MAIN


FUNCTION init()
#fi--------------------
    DEFINE #loc #char
        l_obten_ftraspaso CHAR(120)

    LET v_hoy         = TODAY

    LET v_fecha_valor = MDY(MONTH(v_hoy),1,YEAR(v_hoy))

    LET l_obten_ftraspaso = " SELECT MAX(fecha_mov_banxico) ",
                            " FROM   taa_viv_recepcion      ",
                            " WHERE  nss = ? ",
                            " AND    ident_operacion = '09' "

    PREPARE eje_obten_ftraspaso FROM  l_obten_ftraspaso


END FUNCTION

FUNCTION fn_principal()
#fp--------------------

    DEFINE #loc #smallint
        l_num_solicitud       SMALLINT

    OPEN WINDOW retc847_1 AT 2,2 WITH FORM "RETC8471" ATTRIBUTE(BORDER)
    DISPLAY "                 [Esc] = Aceptar,     [Ctrl-C] = Cancelar                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC847     CONSULTA DE SOLICITUDES CONFIRMADAS TIPO RETIRO J                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY v_hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    --Selecciona las solicitudes de reingreso, según el criterio indicado------
    CALL fn_lee_datos() RETURNING l_num_solicitud
    ---------------------------------------------------------------------------

    IF l_num_solicitud > 0 THEN
        CALL fn_despliega_datos(l_num_solicitud)
    END IF

    CLOSE WINDOW retc847_1

END FUNCTION

FUNCTION fn_lee_datos()
#fld-------------------
    DEFINE #loc #smallint
        l_num_solicitud SMALLINT

    LET l_num_solicitud = 0
    LET int_flag        = FALSE

    IF sw_1 = 1 THEN
        CONSTRUCT v_condicion
            ON fecha_captura
               FROM fecha_captura

            ON KEY (CONTROL-C,INTERRUPT)
               EXIT CONSTRUCT

            ON KEY (ESC,CONTROL-M)
               EXIT CONSTRUCT
        END CONSTRUCT
    END IF

    IF int_flag THEN
        LET int_flag = FALSE
        PROMPT "CONSULTA CANCELADA...<ENTER> PARA SALIR" FOR CHAR enter
        EXIT PROGRAM
    END IF

    LET v_sql = " SELECT UNIQUE(a.nss)                  ,",
                "        a.consecutivo                  ,",
                "        a.tipo_pension                 ,",
                "        a.tipo_prestacion              ,",
                "        a.fecha_ini_pen                ,",
                "        TO_CHAR(a.fecha_ini_pen,'%Y%m'),",
                "        c.descripcion                  ,",
                "        a.grupo                         ",
                " FROM   ret_solicitud_tx a  ,",
                "        afi_mae_afiliado b  ,",
                "        taa_viv_recepcion d ,",
                "        ret_estado c         ",
                " WHERE  a.nss               = b.n_seguro ",
                " AND    a.nss               = d.nss      ",
                " AND    b.tipo_solicitud    IN (1,2,9)   ",
                " AND    a.estado_solicitud  IN (3,33)    ",
                " AND    a.tipo_retiro       = 'J'        ",
                " AND    c.estado_solicitud  = a.estado_solicitud ",
                " AND    ", v_condicion CLIPPED,
                " ORDER BY 1 "

    PREPARE exe_sql_1 FROM v_sql
    DECLARE c_principal CURSOR FOR exe_sql_1

    FOREACH c_principal INTO reg_retiro.*

        LET l_num_solicitud = l_num_solicitud + 1


        LET reg_retiro_des[l_num_solicitud].nss             = reg_retiro.nss
        LET reg_retiro_des[l_num_solicitud].consecutivo     = reg_retiro.consecutivo
        LET reg_retiro_des[l_num_solicitud].tipo_pension    = reg_retiro.tipo_pension
        LET reg_retiro_des[l_num_solicitud].tipo_prestacion = reg_retiro.tipo_prestacion
        LET reg_retiro_des[l_num_solicitud].fecha_ini_pen   = reg_retiro.fecha_ini_pen
        LET reg_retiro_des[l_num_solicitud].periodo_pago    = reg_retiro.periodo_pago
        LET reg_retiro_des[l_num_solicitud].estado          = reg_retiro.estado
        LET reg_retiro_des[l_num_solicitud].grupo           = reg_retiro.grupo

        EXECUTE eje_obten_ftraspaso USING reg_retiro.nss
            INTO reg_retiro_des[l_num_solicitud].fecha_traspaso


    END FOREACH

    IF l_num_solicitud = 0 THEN
          ERROR "NO EXISTEN DATOS CON EL CRITERIO INDICADO..."
          SLEEP 3
          ERROR ""
    END IF

    RETURN l_num_solicitud
END FUNCTION

FUNCTION fn_despliega_datos(l_num_solicitud)
#fdd----------------------------------------
    DEFINE #loc integer
        v_arr_elemento        ,
        v_scr_elemento        INTEGER

    DEFINE #loc #smallint
        l_num_solicitud       SMALLINT


    OPEN WINDOW retc847_2 AT 2,2 WITH FORM "RETC8472" ATTRIBUTE(BORDER)
    DISPLAY "              [Enter] = Seleccionar      [Ctrl-C] = Cancelar                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC847     CONSULTA DE SOLICITUDES CONFIRMADAS TIPO RETIRO J                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY v_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL SET_COUNT(l_num_solicitud )
    DISPLAY ARRAY reg_retiro_des TO scr_1.*

        ON KEY ( CONTROL-C,INTERRUPT )
           LET int_flag = FALSE
           let sw_1 = 0

           EXIT DISPLAY

        ON KEY ( ESC,CONTROL-M )
           LET v_arr_elemento = ARR_CURR()
           LET v_scr_elemento = SCR_LINE()

           CALL fn_calcula_saldo(reg_retiro_des[v_arr_elemento].nss         ,
                                 reg_retiro_des[v_arr_elemento].consecutivo ,
                                 reg_retiro_des[v_arr_elemento].grupo)

           LET sw_1 = 2
           EXIT DISPLAY
    END DISPLAY

    CLOSE WINDOW retc847_2

END FUNCTION

FUNCTION fn_calcula_saldo(r_nss, r_consecutivo,r_grupo)
#fcs--------------------------------------------------
    DEFINE #loc #char
        r_nss                 CHAR(11)

    DEFINE #loc #smallint
        r_grupo               SMALLINT

    DEFINE #loc #integer
        r_consecutivo         ,
        l_num_regs            INTEGER

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_saldo_retiro
    WHENEVER ERROR STOP

    SELECT s.siefore   ,
           s.subcuenta ,
           SUM(s.monto_en_acciones) acciones
    FROM   dis_cuenta s, tab_agrupa_subcta t
    WHERE  s.nss       = r_nss
    AND    s.subcuenta = t.subcuenta
    AND    t.grupo     = r_grupo
    GROUP BY 1,2
    ORDER BY 1,2
    INTO TEMP tmp_saldo_retiro

    DECLARE c_saldo CURSOR FOR
    SELECT s.siefore   ,
           s.subcuenta ,
           s.acciones  ,
           s.acciones * g.precio_del_dia pesos
    FROM   tmp_saldo_retiro s, glo_valor_accion g
    WHERE  g.codigo_siefore  = s.siefore
    AND    g.fecha_valuacion = v_hoy
    AND    s.siefore        <> 11
    UNION ALL
    SELECT s.siefore   ,
           s.subcuenta ,
           s.acciones  ,
           s.acciones * g.precio_del_dia pesos
    FROM   tmp_saldo_retiro s, glo_valor_accion g
    WHERE g.codigo_siefore  = s.siefore
    AND   g.fecha_valuacion = v_fecha_valor
    AND   s.siefore         = 11
    ORDER BY 1,2

    LET l_num_regs = 1

    FOREACH c_saldo INTO reg_saldo[l_num_regs].*
       LET l_num_regs = l_num_regs + 1
    END FOREACH
    
    CALL fn_despliega_saldos(r_nss,r_consecutivo,l_num_regs)
END FUNCTION


FUNCTION fn_despliega_saldos(l_nss, l_consecutivo, l_num_regs)
#fds-------------------------------------------------------

    DEFINE #loc #char
        respuesta             CHAR(01) ,
        l_nss                 CHAR(11)

    DEFINE #loc #integer
        l_consecutivo         ,
        l_num_regs            ,
        l_st                  ,
        v_arr_elemento        ,
        v_scr_elemento        INTEGER

    DEFINE lr_nombre_afiliado RECORD #loc #lr_nombre_afiliado
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombres               CHAR(40)
    END RECORD

    DEFINE
        ld_precio_sie       ,
        ld_precio_viv       DECIMAL(10,6)

    CALL fn_obtiene_nombre(l_nss) RETURNING lr_nombre_afiliado.*
    
    OPEN WINDOW retc847_3 AT 2,2 WITH FORM "RETC8473" ATTRIBUTE(BORDER)
    DISPLAY "          [Esc] = Aceptar    [Ctrl-C] = Cancelar   [Ctrl-N] = Detalle        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC847     CONSULTA DE SOLICITUDES CONFIRMADAS TIPO RETIRO J                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY v_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "                         SALDO DE LA CUENTA INDIVIDUAL                         " AT 8,1 ATTRIBUTE(REVERSE)

    DISPLAY l_nss                      TO nss
    DISPLAY lr_nombre_afiliado.paterno TO paterno
    DISPLAY lr_nombre_afiliado.materno TO materno
    DISPLAY lr_nombre_afiliado.nombres TO nombres

    DISPLAY v_hoy         TO FORMONLY.fecha_conversion
    DISPLAY v_fecha_valor TO FORMONLY.fecha_valor

    -- despleiga valor de vivienda
    SELECT precio_del_dia
    INTO  ld_precio_viv
    FROM  glo_valor_accion
    WHERE codigo_siefore = 11
    AND   fecha_valuacion = v_fecha_valor
  
    DISPLAY ld_precio_viv TO FORMONLY.precio_sb11

    -- despliega valor de siefore

    DISPLAY reg_saldo[1].sie_saldo TO FORMONLY.siefore_act

    SELECT precio_del_dia
    INTO  ld_precio_sie
    FROM  glo_valor_accion
    WHERE codigo_siefore = reg_saldo[1].sie_saldo
    AND   fecha_valuacion = v_hoy
  
    DISPLAY ld_precio_sie TO FORMONLY.precio_sb1
    
    CALL SET_COUNT(l_num_regs -1)
    DISPLAY ARRAY reg_saldo TO scr_3.*
        
        ON KEY ( CONTROL-C,INTERRUPT )
            LET int_flag = FALSE
            EXIT DISPLAY
            
        ON KEY ( ESC )
            LET v_arr_elemento = ARR_CURR()
            LET v_scr_elemento = SCR_LINE()
            LET respuesta      = ""

            OPEN WINDOW w_pregunta AT 12,10 WITH 3 ROWS, 60 COLUMNS ATTRIBUTE (BORDER)

            WHENEVER ERROR CONTINUE
            WHILE TRUE
                PROMPT "SE CONSIDERA EL MONTO TRASPASADO EN EL RETIRO (S/N) : " FOR respuesta

                SELECT "OK"
                FROM  ret_pago_reingreso
                WHERE nss         = l_nss
                AND   consecutivo = l_consecutivo
                GROUP BY 1

                LET l_st = SQLCA.SQLCODE

                IF UPSHIFT(respuesta) = "S" AND l_st = NOTFOUND THEN
                    INSERT INTO ret_pago_reingreso 
                        VALUES (1,l_nss,l_consecutivo,1)
                    
                    PROMPT "REGISTRO INGRESADO CORRECTAMENTE (ENTER PARA CONTINUAR) ..." FOR CHAR enter
                    EXIT WHILE
                ELSE
                    IF UPSHIFT(respuesta) = "S" AND l_st = 0 THEN

                        UPDATE ret_pago_reingreso
                        SET    pago_traspaso = 1
                        WHERE  nss         = l_nss
                        AND    consecutivo = l_consecutivo

                        UPDATE ret_solicitud_tx
                        SET    estado_solicitud = 33
                        WHERE  nss         = l_nss
                        AND    consecutivo = l_consecutivo

                        PROMPT "REGISTRO INGRESADO CORRECTAMENTE (ENTER PARA CONTINUAR) ..." FOR CHAR enter

                        EXIT WHILE
                    END IF
                END IF

                IF UPSHIFT(respuesta) = "N" AND l_st = NOTFOUND THEN

                    INSERT INTO ret_pago_reingreso
                        VALUES (1,l_nss,l_consecutivo,0)

                    UPDATE ret_solicitud_tx
                    SET    estado_solicitud = 3
                    WHERE  nss         = l_nss
                    AND    consecutivo = l_consecutivo

                    PROMPT "NO SE INCLUIRA EL TRASPASO (ENTER PARA CONTINUAR) ..." FOR CHAR enter                    

                    EXIT WHILE
                ELSE
                    IF UPSHIFT(respuesta) = "N" AND l_st = 0 THEN
 
                        UPDATE ret_pago_reingreso
                        SET    pago_traspaso = 0
                        WHERE  nss         = l_nss
                        AND    consecutivo = l_consecutivo

                        UPDATE ret_solicitud_tx
                        SET    estado_solicitud = 3
                        WHERE  nss         = l_nss
                        AND    consecutivo = l_consecutivo

                        PROMPT "NO SE INCLUIRA EL TRASPASO (ENTER PARA CONTINUAR) ..." FOR CHAR enter

                        EXIT WHILE
                    END IF
                END IF
            END WHILE
            WHENEVER ERROR STOP
            CLOSE WINDOW w_pregunta
            EXIT DISPLAY

        ON KEY ( CONTROL-N )
            LET v_arr_elemento = ARR_CURR()
            LET v_scr_elemento = SCR_LINE()

            CALL fn_obtiene_movtos(l_nss,
                                   reg_saldo[v_arr_elemento].sie_saldo,
                                   reg_saldo[v_arr_elemento].sub_saldo,
                                   v_arr_elemento)

    END DISPLAY
    CLOSE WINDOW retc847_3
END FUNCTION

FUNCTION fn_obtiene_nombre(l_nss)

    DEFINE
        l_nss CHAR(11)

    DEFINE lr_nombre_afiliado RECORD
        paterno        CHAR(40),
        materno        CHAR(40),
        nombres        CHAR(40)
    END RECORD

    SELECT paterno,
           materno,
           nombres
    INTO lr_nombre_afiliado.*
    FROM afi_mae_afiliado
    WHERE n_seguro = l_nss

    RETURN lr_nombre_afiliado.*

END FUNCTION


FUNCTION fn_obtiene_movtos(l_nss, l_siefore, l_subcuenta ,l_num_elemento)
#fom---------------------------------------------------------------------
    DEFINE #loc #char
        l_nss                 CHAR(11)

    DEFINE #loc #smallint
        l_siefore             ,
        l_subcuenta           SMALLINT

    DEFINE  l_num_elemento ,
            l_movimientos INTEGER

    LET v_sql = " SELECT s.siefore           , ",
                "        s.subcuenta         , ",
                "        s.tipo_movimiento   , ",
                "        s.fecha_conversion  , ",
                "        s.monto_en_acciones , ",
                "        s.monto_en_pesos      ",
                " FROM   dis_cuenta s  ",
                " WHERE  s.nss       = ",l_nss       ,
                " AND    s.subcuenta = ",l_subcuenta ,
            --  " AND    s.siefore   = ",l_siefore   ,
                " AND    s.fecha_conversion >= ",reg_retiro_des[l_num_elemento].fecha_traspaso,
                " ORDER BY 4 "

    PREPARE exe_sql_2 FROM v_sql
    DECLARE cur_movtos CURSOR FOR exe_sql_2

    LET l_movimientos = 1

    FOREACH cur_movtos INTO reg_movto[l_movimientos].*
       LET l_movimientos = l_movimientos + 1
    END FOREACH

    CALL fn_despliega_movtos(l_movimientos)

    CLOSE WINDOW retc847_4

END FUNCTION

FUNCTION fn_despliega_movtos(l_movimientos)
#fdm---------------------------------------
    DEFINE #loc #integer
        v_arr_elemento        ,
        v_scr_elemento        ,
        l_movimientos         INTEGER

    OPEN WINDOW retc847_4 AT 16,2 WITH FORM "RETC8474"
    DISPLAY "          DETALLE DE MOVIMIENTOS A PARTIR DEL TRASPASO  [Ctrl-C] Salir                    " AT 1,1 ATTRIBUTE(REVERSE)

    CALL SET_COUNT(l_movimientos-1)
    DISPLAY ARRAY reg_movto TO scr_4.*

        ON KEY ( CONTROL-C,INTERRUPT )
            LET int_flag = FALSE
            EXIT DISPLAY

        ON KEY ( ESC, CONTROL-M )
            LET v_arr_elemento = ARR_CURR()
            LET v_scr_elemento = SCR_LINE()
    END DISPLAY

END FUNCTION
