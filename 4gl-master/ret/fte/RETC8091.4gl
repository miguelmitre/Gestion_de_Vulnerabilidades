################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa RETC8091 => REVERSO NOTIFICACION DE DISPOSICION DE RECURSOS SOLICITUD#
#                     OP.05 POR RETIRO "J"                                     #
#Fecha creacion    => 06 DE NOVIEMBRE DE 2004                                  #
#Desarrado por     => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_2 RECORD #glo #reg_2
        confirmado            LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud 
    END RECORD

    DEFINE #glo #integer
        vfolio_oper05                INTEGER

    DEFINE #glo #smallint
        vestado               ,
        vtipo_movimiento      SMALLINT

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    OPEN WINDOW retc80911 AT 4,4 WITH FORM "RETC80911" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                              TIPO RETIRO J     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8091 REVERSO NOTIFICA Y PROVISIONA DISPOSICION DE RECURSOS                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio_oper05 WITHOUT DEFAULTS

        AFTER FIELD vfolio_oper05
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_solicitud_tx
            WHERE  folio       = vfolio_oper05
            AND    tipo_retiro = "J"
            GROUP  BY 1     
       
            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            ELSE
                IF vestado = reg_2.enviado THEN
                    ERROR "    ERROR...PRIMERO SE DEBE REVERSAR EL LOTE"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper05
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "    ERROR...FOLIO YA LIQUIDADO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            END IF

            DECLARE cur_1 CURSOR FOR
            SELECT tipo_movimiento
            FROM   dis_provision
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            FOREACH cur_1 INTO vtipo_movimiento

                IF vtipo_movimiento = 880
                OR vtipo_movimiento = 10 THEN
                    # EL FOLIO ES UNICO PARA RETIRO J
                ELSE
                    ERROR "    ERROR...FOLIO NO UNICO PARA TIPO RETIRO J "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper05
                END IF
            END FOREACH


        ON KEY (ESC)
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_solicitud_tx
            WHERE  folio       = vfolio_oper05
            AND    tipo_retiro = "J"
            GROUP  BY 1 
           
            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            ELSE
                IF vestado = reg_2.enviado THEN
                    ERROR "    ERROR...PRIMERO SE DEBE REVERSAR EL LOTE"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper05
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "    ERROR...FOLIO YA LIQUIDADO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            END IF

            DECLARE cur_2 CURSOR FOR
            SELECT tipo_movimiento
            FROM   dis_provision
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            FOREACH cur_2 INTO vtipo_movimiento

                IF vtipo_movimiento = 880
                OR vtipo_movimiento = 10 THEN
                    # EL FOLIO ES UNICO PARA RETIRO J
                ELSE
                    ERROR "    ERROR...FOLIO NO UNICO PARA TIPO RETIRO J "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper05
                END IF
            END FOREACH

            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    EXIT PROGRAM

    CLOSE WINDOW retc80911
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT A.estado_solicitud
    INTO   reg_2.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT MAX(folio)
    INTO   vfolio_oper05
    FROM   ret_solicitud_tx
    WHERE  tipo_retiro = "J"
END FUNCTION

FUNCTION primer_paso()
#pp-------------------

    DELETE
    FROM  dis_provision 
    WHERE folio = vfolio_oper05;

    DELETE 
    FROM  ret_ctr_envio_lote
    WHERE folio        = vfolio_oper05
    AND   tipo_retiro  = "J";

    DELETE
    FROM   ret_monto_siefore
    WHERE  folio       = vfolio_oper05
    AND    tipo_retiro = "J";

    DELETE
    FROM   ret_monto_viv
    WHERE  folio       = vfolio_oper05
    AND    tipo_retiro = "J";


    UPDATE ret_solicitud_tx
    SET    acciones_ret97     = 0,
           acciones_cuota_soc = 0,
           acciones_ret92     = 0,
           acciones_cv        = 0,
           saldo_viv97        = 0,
           saldo_viv92        = 0,
           saldo_viv72        = 0,
--           folio              = 0,
           fecha_envio        = "01010001",
           fecha_valor_viv    = "01010001",
           estado_solicitud   = reg_2.confirmado
    WHERE  folio              = vfolio_oper05
    AND    tipo_retiro        = "J"
END FUNCTION
